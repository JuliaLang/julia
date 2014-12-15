import Base: convert, show

abstract Ty

type TypeName
    name::Symbol
    super::Ty    # actually TagT
    # arity
    # representation
    # abstract, mutable
    TypeName(name, super) = new(name, super)
    TypeName(name) = new(name)
end

show(io::IO, x::TypeName) = print(io, x.name)

type TagT <: Ty
    name::TypeName
    params
    vararg::Bool
    TagT(n, p, v=false) = new(n, p, v)
end

function show(io::IO, t::TagT)
    print(io, t.name.name)
    isempty(t.params) && return
    print(io, '{')
    l = length(t.params)
    for i=1:l
        show(io, t.params[i])
        if i == l && t.vararg
            print(io, "...")
        elseif i < l
            print(io, ",")
        end
    end
    print(io, '}')
end

type BottomTy <: Ty
end

show(io::IO, ::BottomTy) = print(io, "BottomT")

type UnionT <: Ty
    a
    b
    UnionT() = BottomT
    UnionT(a) = a
    UnionT(a, b) = new(a, b)
    UnionT(a, ts...) = new(a, UnionT(ts...))
end

function show(io::IO, t::UnionT)
    print(io, "UnionT(")
    while true
        show(io, t.a)
        print(io, ",")
        if isa(t.b, UnionT)
            t = t.b
        else
            show(io, t.b)
            break
        end
    end
    print(io, ')')
end

type Var
    name::Symbol
    lb
    ub
    Var(n, lb=BottomT, ub=AnyT) = new(n, lb, ub)
end

function show_var_bounds(io::IO, v::Var)
    if v.lb !== BottomT
        show(io, v.lb)
        print(io, "<:")
    end
    print(io, v.name)
    if v.ub !== AnyT
        print(io, "<:")
        show(io, v.ub)
    end
end

show(io::IO, v::Var) = print(io, v.name)

type UnionAllT <: Ty
    var::Var
    T
    UnionAllT(v::Var, t) = new(v, t)
    UnionAllT(v::Var, t::Union(Type,Tuple)) = new(v, convert(Ty, t))
end

function show(io::IO, x::UnionAllT)
    print(io, "(@UnionAll ")
    show_var_bounds(io, x.var)
    print(io, " ")
    show(io, x.T)
    print(io, ")")
end


# Any, Bottom, and Tuple

const AnyT = TagT(TypeName(:Any), ())
AnyT.name.super = AnyT

const BottomT = BottomTy()

const TupleName = TypeName(:Tuple, AnyT)
const TupleT = TagT(TupleName, (AnyT,), true)
tupletype(xs...) = inst(TupleName, xs...)
vatype(xs...) = (t = inst(TupleName, xs...); t.vararg = true; t)


# type application

inst(typename::TypeName, params...) = TagT(typename, params)

inst(t::TagT) = t

inst(t::UnionAllT, param) = subst(t.T, Dict{Any,Any}(t.var => param))
inst(t::UnionAllT, param, rest...) = inst(inst(t,param), rest...)

super(t::TagT) = inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = t===AnyT ? t : TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = UnionT(subst(t.a,env), subst(t.b,env))
subst(t::Var,     env) = get(env, t, t)
subst(t::UnionAllT, env) = (assert(!haskey(env, t.var));
                            newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                            UnionAllT(newVar, subst(t.T, extend(env, t.var, newVar))))
subst(t, env) = t


# subtype

isequal_type(x, y) = issub(x, y) && issub(y, x)

type Bounds
    # record current lower and upper bounds of a Var
    # depth: invariant position nesting depth of a Var's UnionAll
    # right: whether this Var is on the right-hand side of A <: B
    lb
    ub
    depth::Int
    right::Bool
end

type UnionSearchState
    i::Int       # traversal-order index of current union (0 <= i < nbits(idxs))
    idxs::Int64  # bit vector representing current combination being tested
    UnionSearchState() = new(0,0)
end

type Env
    vars::Dict{Var,Bounds}
    depth::Int

    Ldepth::Int        # number of union decision points we're inside (depth)
    Lnew::Int          # # unions found at next nesting depth
    Lunions::Vector{UnionSearchState}  # stack of decisions for each depth

    Rdepth::Int        # same on right-hand side
    Rnew::Int
    Runions::Vector{UnionSearchState}

    Env() = new(Dict{Var,Bounds}(), 1,  1,0,UnionSearchState[], 1,0,UnionSearchState[])
end

issub(x, y) = forall_exists_issub(x, y, Env(), 0)

function forall_exists_issub(x, y, env, nL)
    assert(nL < 63)
    # for all combinations of elements from Unions on the left, there must
    # exist a combination of elements from Unions on the right that makes
    # issub() true. Unions in invariant position are on both the left and
    # the right in this formula.

    for forall in 1:(1<<nL)
        if !isempty(env.Lunions)
            env.Lunions[end].idxs = forall
        end

        if !exists_issub(x, y, env, 0)
            return false
        end

        if env.Lnew > 0
            push!(env.Lunions, UnionSearchState())
            sub = forall_exists_issub(x, y, env, env.Lnew)
            pop!(env.Lunions)
            if !sub
                return false
            end
        end
    end
    return true
end

function exists_issub(x, y, env, nR)
    assert(nR < 63)
    for exists in 1:(1<<nR)
        if !isempty(env.Runions)
            env.Runions[end].idxs = exists
        end
        for ru in env.Runions; ru.i = -1; end
        for lu in env.Lunions; lu.i = -1; end
        env.Ldepth = env.Rdepth = 1
        env.Lnew = env.Rnew = 0

        sub = issub(x, y, env)

        if env.Lnew > 0
            # return up to forall_exists_issub. the recursion must have this shape:
            # ∀₁         ∀₁
            #   ∃₁  =>     ∀₂
            #                ...
            #                ∃₁
            #                  ∃₂
            return true
        end
        if env.Rnew > 0
            push!(env.Runions, UnionSearchState())
            found = exists_issub(x, y, env, env.Rnew)
            pop!(env.Runions)
            if env.Lnew > 0
                # return up to forall_exists_issub
                return true
            end
        else
            found = sub
        end
        found && return true
    end
    return false
end

issub(x, y, env) = (x === y)
issub(x::Ty, y::Ty, env) = (x === y) || x === BottomT

function union_issub(a::UnionT, b::Ty, env)
    if env.Ldepth > length(env.Lunions)
        # at a new nesting depth, begin by just counting unions
        env.Lnew += 1
        return true
    end
    L = env.Lunions[env.Ldepth]; L.i += 1
    env.Ldepth += 1
    ans = issub(a.((L.idxs&(1<<L.i)!=0) + 1), b, env)
    env.Ldepth -= 1
    return ans
end

function issub_union(a::Ty, b::UnionT, env)
    a === BottomT && return true
    if env.Rdepth > length(env.Runions)
        env.Rnew += 1
        return true
    end
    R = env.Runions[env.Rdepth]; R.i += 1
    env.Rdepth += 1
    ans = issub(a, b.((R.idxs&(1<<R.i)!=0) + 1), env)
    env.Rdepth -= 1
    return ans
end

issub(a::UnionT, b::UnionT, env) = a === b || union_issub(a, b, env)
issub(a::UnionT, b::Ty, env) = union_issub(a, b, env)
issub(a::Ty, b::UnionT, env) = issub_union(a, b, env)

function issub(a::TagT, b::TagT, env)
    a === b && return true
    b === AnyT && return true
    a === AnyT && return false
    if a.name !== b.name
        a.name === TupleName && return false
        return issub(super(a), b, env)
    end
    if a.name === TupleName
        va, vb = a.vararg, b.vararg
        la, lb = length(a.params), length(b.params)
        if va && (!vb || la < lb)
            return false
        end
        ai = bi = 1
        while true
            ai > la && return bi > lb || (bi==lb && vb)
            bi > lb && return false
            !issub(a.params[ai], b.params[bi], env) && return false
            ai==la && bi==lb && va && vb && return true
            if ai < la || !va
                ai += 1
            end
            if bi < lb || !vb
                bi += 1
            end
        end
        @assert false
    else
        env.depth += 1  # crossing invariant constructor, increment depth
        for i = 1:length(a.params)
            ai, bi = a.params[i], b.params[i]
            # use issub in both directions to test equality
            if !(ai===bi || (issub(ai, bi, env) && issub(bi, ai, env)))
                env.depth -= 1
                return false
            end
        end
        env.depth -= 1
    end
    return true
end

function issub(a::Var, b::Ty, env)
    aa = env.vars[a]
    # Vars are fully checked by the "forward" direction of A<:B in
    # invariant position. So just return true when checking the "flipped"
    # direction B<:A.
    aa.right && return true
    if env.depth != aa.depth  # && env.depth > 1  ???
        # Var <: non-Var can only be true when there are no invariant
        # constructors between the UnionAll and this occurrence of Var.
        return false
    end
    return issub(aa.ub, b, env)
end

function issub(a::Var, b::Var, env)
    a === b && return true
    aa = env.vars[a]
    aa.right && return true
    bb = env.vars[b]
    if aa.depth != bb.depth  # && env.depth > 1  ???
        # Vars must occur at same depth
        return false
    end
    if env.depth > bb.depth
        # if there are invariant constructors between a UnionAll and
        # this occurrence of Var, then we have an equality constraint on Var.
        if isa(bb.ub,Var)
            # right-side Var cannot equal more than one left-side Var, e.g.
            # (L,L) <: (T,S) but not (T,S) <: (R,R)
            bb.lb = a
            return bb.ub === a
        end
        if isa(bb.lb,Var)
            bb.ub = a
            return bb.lb === a
        end
        if !(issub(bb.lb, aa.lb, env) && issub(aa.ub, bb.ub, env))
            # make sure equality constraint is within the current bounds of Var
            return false
        end
        bb.lb = bb.ub = a
    else
        !issub(aa.ub, bb.ub, env) && return false
        # track greatest lower bound from covariant position. e.g.
        # (Int, Real, Integer, Array{?}) <: (T, T, T, Array{T})
        # is only true if ? >: Real.
        if issub(bb.lb, aa.ub, env)
            bb.lb = a
        end
    end
    return true
end

function issub(a::Ty, b::Var, env)
    bb = env.vars[b]
    !bb.right && return true
    if env.depth > bb.depth
        if isa(bb.ub,Var)
            return false
        end
        if !(issub(bb.lb, a, env) && issub(a, bb.ub, env))
            return false
        end
        bb.lb = bb.ub = a
    else
        !issub(a, bb.ub, env) && return false
        if issub(bb.lb, a, env)
            bb.lb = a
        end
    end
    return true
end

function rename(t::UnionAllT)
    v = Var(t.var.name, t.var.lb, t.var.ub)
    UnionAllT(v, inst(t,v))
end

function issub_unionall(a::Ty, b::UnionAllT, env)
    a === BottomT && return true
    haskey(env.vars, b.var) && (b = rename(b))
    env.vars[b.var] = Bounds(b.var.lb, b.var.ub, env.depth, true)
    ans = issub(a, b.T, env)
    delete!(env.vars, b.var)
    return ans
end

function unionall_issub(a::UnionAllT, b::Ty, env)
    haskey(env.vars, a.var) && (a = rename(a))
    env.vars[a.var] = Bounds(a.var.lb, a.var.ub, env.depth, false)
    ans = issub(a.T, b, env)
    delete!(env.vars, a.var)
    return ans
end

issub(a::UnionAllT, b::UnionAllT, env) = a===b || issub_unionall(a, b, env)

issub(a::UnionT, b::UnionAllT, env) = issub_unionall(a, b, env)
issub(a::Ty, b::UnionAllT, env) = issub_unionall(a, b, env)

issub(a::UnionAllT, b::UnionT, env) = unionall_issub(a, b, env)
issub(a::UnionAllT, b::Ty, env) = unionall_issub(a, b, env)


# convenient syntax

macro UnionAll(var, expr)
    lb = :BottomT
    ub = :AnyT
    if isa(var,Expr) && var.head === :comparison
        if length(var.args) == 3
            v = var.args[1]
            if var.args[2] == :(<:)
                ub = esc(var.args[3])
            elseif var.args[2] == :(>:)
                lb = esc(var.args[3])
            else
                error("invalid bounds in UnionAll")
            end
        elseif length(var.args) == 5
            v = var.args[3]
            if var.args[2] == var.args[4] == :(<:)
                lb = esc(var.args[1])
                ub = esc(var.args[5])
            else
                error("invalid bounds in UnionAll")
            end
        else
            error("invalid bounds in UnionAll")
        end
    elseif !isa(var,Symbol)
        error("invalid variable in UnionAll")
    else
        v = var
    end
    quote
        let $(esc(v)) = Var($(Expr(:quote,v)), $lb, $ub)
            UnionAllT($(esc(v)), $(esc(expr)))
        end
    end
end


# translating from existing julia types

const tndict = ObjectIdDict()

xlate(t) = xlate(t, ObjectIdDict())

xlate(t, env) = t

function xlate(t::UnionType, env)
    if t === Union()
        return BottomT
    end
    UnionT(map(x->xlate(x,env), t.types)...)
end

function xlate(t::Tuple, env)
    if length(t) == 0
        return inst(TupleName)
    end
    va = Base.isvarargtype(t[end])
    ts = map(x->(Base.isvarargtype(x) ? xlate(x.parameters[1],env) : xlate(x,env)), t)
    tnew = inst(TupleName, ts...)
    tnew.vararg = va
    tnew
end

function xlate(t::TypeVar, env)
    if haskey(env, t)
        return env[t]
    end
    v = Var(t.name, xlate(t.lb,env), xlate(t.ub,env))
    env[t] = v
    v
end

function xlate(t::DataType, env)
    if t === Any
        return AnyT
    end
    if !haskey(tndict,t.name)
        para = map(x->xlate(x,env), t.name.primary.parameters)  # adds tvars to env
        sup = xlate(t.name.primary.super, env)
        for i = length(para):-1:1
            sup = UnionAllT(para[i], sup)
        end
        tn = TypeName(t.name.name, sup)
        tndict[t.name] = tn
    else
        tn = tndict[t.name]
    end
    inst(tn, map(x->xlate(x,env), t.parameters)...)
end

convert(::Type{Ty}, t::Union(Type,Tuple)) = xlate(t)
convert(::Type{Ty}, t::TypeVar) = xlate(t)

issub(a::Type, b::Type) = issub(xlate(a), xlate(b))
issub(a::Ty  , b::Type) = issub(a       , xlate(b))
issub(a::Type, b::Ty  ) = issub(xlate(a), b)


# tests

AbstractArrayT =
    let AbstractArrayName = TypeName(:AbstractArray, @UnionAll T @UnionAll N AnyT)
        @UnionAll T @UnionAll N inst(AbstractArrayName, T, N)
    end

ArrayT =
    let ArrayName = TypeName(:Array, @UnionAll T @UnionAll N inst(AbstractArrayT, T, N))
        @UnionAll T @UnionAll N inst(ArrayName, T, N)
    end

PairT = let PairName = TypeName(:Pair, @UnionAll A @UnionAll B AnyT)
    @UnionAll A @UnionAll B inst(PairName, A, B)
end

RefT = let RefName = TypeName(:Ref, @UnionAll T AnyT)
    @UnionAll T inst(RefName, T)
end

tndict[AbstractArray.name] = AbstractArrayT.T.T.name
tndict[Array.name] = ArrayT.T.T.name
tndict[Pair.name] = PairT.T.T.name

function non_terminating()
    # undecidable F_<: instance
    ¬T = @UnionAll α<:T α

    θ = @UnionAll α ¬(@UnionAll β<:α ¬β)

    a0 = Var(:a0, BottomT, θ)

    issub(a0, (@UnionAll a1<:a0 ¬a1))
end

using Base.Test

issub_strict(x,y) = issub(x,y) && !issub(y,x)

# level 1: no varags, union, UnionAll
function test_1()
    @test issub_strict(Int, Integer)
    @test issub_strict(Array{Int,1}, AbstractArray{Int,1})

    @test isequal_type(Int, Int)
    @test isequal_type(Integer, Integer)
    @test isequal_type(Array{Int,1}, Array{Int,1})
    @test isequal_type(AbstractArray{Int,1}, AbstractArray{Int,1})

    @test issub_strict((Int,Int), (Integer,Integer))
    @test issub_strict((Array{Int,1},), (AbstractArray{Int,1},))

    @test isequal_type((Integer,Integer), (Integer,Integer))

    @test !issub((Int,Int), (Int,))
    @test !issub((Int,), (Integer,Integer))
end

# level 2: varargs
function test_2()
    @test issub_strict((Int,Int), (Int...,))
    @test issub_strict((Int,Int), (Int,Int...,))
    @test issub_strict((Int,Int), (Int,Integer...,))
    @test issub_strict((Int,Int), (Int,Int,Integer...,))
    @test issub_strict((Int,Int...), (Int...,))
    @test issub_strict((Int,Int,Int...), (Int...,))
    @test issub_strict((Int,Int,Int...), (Integer,Int...,))
    @test issub_strict((Int...,), (Any...,))
    @test issub_strict((), (Any...,))

    @test isequal_type((Int...,), (Int...,))
    @test isequal_type((Integer...,), (Integer...,))

    @test !issub((), (Int, Int...))
    @test !issub((Int,), (Int, Int, Int...))

    @test !issub((Int, (Real, Integer)), (Int...))
end

# level 3: UnionAll
function test_3()
    @test issub_strict(Ty(Array{Int,1}), @UnionAll T inst(ArrayT, T, 1))
    @test issub_strict((@UnionAll T inst(PairT,T,T)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll S inst(PairT,Ty(Int),S)))

    @test !issub((@UnionAll T<:Ty(Real) T), (@UnionAll T<:Ty(Integer) T))

    @test issub((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test issub((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test isequal_type((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test isequal_type((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test !issub((@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
                 (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)))

    AUA = inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1)
    UAA = (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1))

    @test !issub(AUA, UAA)
    @test !issub(UAA, AUA)
    @test !isequal_type(AUA, UAA)

    @test issub_strict((@UnionAll T Int), (@UnionAll T<:Ty(Integer) Integer))

    @test isequal_type((@UnionAll T @UnionAll S tupletype(T, tupletype(S))),
                       (@UnionAll T tupletype(T, @UnionAll S tupletype(S))))

    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int8)))
    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int)))

    @test isequal_type((@UnionAll T tupletype(T)), tupletype(AnyT))
    @test isequal_type((@UnionAll T<:Ty(Real) tupletype(T)), tupletype(Ty(Real)))

    @test  issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Int)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Real)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test isequal_type(Ty(Array{Int,1}), inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1))
    @test isequal_type(Ty(Array{(Any,),1}), inst(ArrayT, (@UnionAll T tupletype(T)), 1))

    @test isequal_type(Ty(Array{(Integer,Integer),1}),
                       inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))
    @test !issub(Ty(Array{(Int,Integer),1}),
                 inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))


    @test !issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T inst(PairT,T,T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Int),1), Ty(Integer)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(Ty(Integer), inst(ArrayT,Ty(Int),1)),
                 (@UnionAll T<:Ty(Integer) tupletype(T, inst(ArrayT,T,1))))

    @test !issub(Ty(Array{Array{Int,1},Integer}),
                 (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test issub(Ty(Array{Array{Int,1},Int}),
                (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test  issub(Ty((Integer,Int)), @UnionAll T<:Ty(Integer) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(Int) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(String) @UnionAll S<:T tupletype(T,S))

    @test issub(Ty((Float32,Array{Float32,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Float32,Array{Float64,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test issub(Ty((Float32,Array{Real,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Number,Array{Real,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T), @UnionAll T<:Ty(Real) T)
    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                (@UnionAll T<:Ty(Real) inst(ArrayT,T,1)))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T),
                (@UnionAll Ty(Integer)<:T<:Ty(Real) T))
    @test !issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                 (@UnionAll Ty(Integer)<:T<:Ty(Real) inst(ArrayT,T,1)))

    X = (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))
    Y = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(A,B))
    @test isequal_type(X,Y)
    Z = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(Ty(Real),B))
    @test issub_strict(X,Z)
end

# level 4: Union
function test_4()
    @test isequal_type(UnionT(BottomT,BottomT), BottomT)

    @test issub_strict(Int, Union(Int,String))
    @test issub_strict(Union(Int,Int8), Integer)

    @test isequal_type(Union(Int,Int8), Union(Int,Int8))

    @test isequal_type(UnionT(Ty(Int),Ty(Integer)), Ty(Integer))

    @test isequal_type((Union(Int,Int8),Int16), Union((Int,Int16),(Int8,Int16)))

    @test issub_strict((Int,Int8,Int), (Union(Int,Int8)...,))
    @test issub_strict((Int,Int8,Int), (Union(Int,Int8,Int16)...,))

    # nested unions
    @test !issub(UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
                 UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int8),Ty(Int16)))))

    A = Ty(Int);   B = Ty(Int8)
    C = Ty(Int16); D = Ty(Int32)
    @test  issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,D))))
    @test !issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,A))))

    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,B,C,D))
    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,UnionT(B,C),D))
    @test isequal_type(UnionT(UnionT(UnionT(UnionT(A)),B,C), UnionT(D)),
                       UnionT(A,UnionT(B,C),D))

    @test issub_strict(UnionT(UnionT(A,C), UnionT(D)),  UnionT(A,B,C,D))

    @test !issub(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,C,D))
end

# level 5: union and UnionAll
function test_5()
    @test issub(Ty((String,Array{Int,1})),
                (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                                    tupletype(T,inst(ArrayT,Ty(Int),1)))))

    @test issub(Ty((Union(Vector{Int},Vector{Int8}),)),
                @UnionAll T tupletype(inst(ArrayT,T,1),))

    @test !issub(Ty((Union(Vector{Int},Vector{Int8}),Vector{Int})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty((Union(Vector{Int},Vector{Int8}),Vector{Int8})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty(Vector{Int}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Integer}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Union(Int,Int8)}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Union(Int,Int8)) inst(ArrayT,T,1)),
                (@UnionAll Ty(Int)<:T<:Ty(Union(Int,Int8)) inst(ArrayT,T,1)))

    # with varargs
    @test !issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                 @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test !issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                 @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))

    @test issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))
end

# examples that might take a long time
function test_slow()
    A = Ty(Int);   B = Ty(Int8)
    C = Ty(Int16); D = Ty(Int32)
    # obviously these unions can be simplified, but when they aren't there's trouble
    X = UnionT(UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),
               UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C))
    Y = UnionT(UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),
               UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(A,B,C))
    @test issub_strict(X,Y)
end

# tests that don't pass yet
function test_failing()
end

function test_all()
    test_1()
    test_2()
    test_3()
    test_4()
    test_5()
    test_slow()
    test_failing()
end

const menagerie =
    Any[BottomT, AnyT, Ty(Int), Ty(Int8), Ty(Integer), Ty(Real),
        Ty(Array{Int,1}), Ty(AbstractArray{Int,1}),
        Ty((Int,Integer...,)), Ty((Integer,Int...,)), Ty(()),
        Ty(Union(Int,Int8)),
        (@UnionAll T inst(ArrayT, T, 1)),
        (@UnionAll T inst(PairT,T,T)),
        (@UnionAll T @UnionAll S inst(PairT,T,S)),
        inst(PairT,Ty(Int),Ty(Int8)),
        (@UnionAll S inst(PairT,Ty(Int),S)),
        (@UnionAll T tupletype(T,T)),
        (@UnionAll T<:Ty(Integer) tupletype(T,T)),
        (@UnionAll T @UnionAll S tupletype(T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)),
        inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1),
        (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1)),
        inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1),
        (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S)),
        UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
        (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                            tupletype(T,inst(ArrayT,Ty(Int),1)))),
        ]

let new = Any[]
    # add variants of each type
    for T in menagerie
        push!(new, inst(RefT, T))
        push!(new, tupletype(T))
        push!(new, tupletype(T,T))
        push!(new, vatype(T))
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T inst(RefT,S))
    end
    append!(menagerie, new)
end

function test_properties()
    x→y = !x || y

    # transitivity
    for T in menagerie
        for S in menagerie
            if issub(T, S)
                for R in menagerie
                    @test issub(S, R) → issub(T, R)
                end
            end
        end
    end

    for T in menagerie
        for S in menagerie
            # union subsumption
            @test isequal_type(T, UnionT(T,S)) → issub(S, T)

            # invariance
            @test isequal_type(T, S) == isequal_type(inst(RefT,T), inst(RefT,S))

            # covariance
            @test issub(T, S) == issub(tupletype(T), tupletype(S))
            @test issub(T, S) == issub(vatype(T), vatype(S))
            @test issub(T, S) == issub(tupletype(T), vatype(S))
        end

        # unionall identity
        @test isequal_type(T, @UnionAll S<:T S)
    end
end
