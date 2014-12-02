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

type UnionT <: Ty
    types
    UnionT(ts...) = new(ts)
end

function show(io::IO, t::UnionT)
    print(io, "UnionT(")
    l = length(t.types)
    for i=1:l
        show(io, t.types[i])
        if i < l
            print(io, ",")
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
    UnionAllT(v::Var, t::Type) = new(v, convert(Ty, t))
end

function show(io::IO, x::UnionAllT)
    print(io, "(⋃ ")
    show_var_bounds(io, x.var)
    print(io, ". ")
    show(io, x.T)
    print(io, ")")
end


# Any, Bottom, and Tuple

AnyT = TagT(TypeName(:Any), ())
AnyT.name.super = AnyT

BottomT = UnionT()

TupleName = TypeName(:Tuple, AnyT)
TupleT = TagT(TupleName, (AnyT,), true)
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
subst(t::UnionT,  env) = t===BottomT ? t : UnionT(map(x->subst(x,env), t.types)...)
subst(t::Var,     env) = get(env, t, t)
subst(t::UnionAllT, env) = (assert(!haskey(env, t.var));
                            newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                            UnionAllT(newVar, subst(t.T, extend(env, t.var, newVar))))
subst(t, env) = t


# subtype

isequal_type(x::Ty, y::Ty) = issub(x, y) && issub(y, x)

type Bounds
    # record current lower and upper bounds of a Var
    # depth: invariant position nesting depth of a Var's UnionAll
    # right: whether this Var is on the right-hand side of A <: B
    lb
    ub
    depth::Int
    right::Bool
end

type Env
    vars::Dict{Var,Bounds}
    depth::Int

    Lunion::Int                # traversal-order index of current union
    Ldepth::Int                # number of union decision points we're inside
    Lunion_idxs                # indexes representing current combination being tested
    Lunion_sizes::Vector{Int}  # number of elements in each decision point
    Lper_depth::Vector{Int}    # cumulative # of unions for each depth

    Runion::Int                ##  same on the right-hand side
    Rdepth::Int                ##
    Runion_idxs                ##
    Runion_sizes::Vector{Int}  ##
    Rper_depth::Vector{Int}    ##

    Env() = new(Dict{Var,Bounds}(), 1,
                0, 0, nothing, Int[], Int[],
                0, 0, nothing, Int[], Int[])
end

function issub(x, y)
    env = Env()

    lastl = lastr = 0
    while true
        # the strategy is to first record all points where we'd need to pick
        # a union element. then we iterate over all possible choices. if we
        # encounter unions while doing that (i.e. nested unions) the search
        # space is extended. each decision point has a linear address,
        # discovered incrementally.
        ans = forall_exists_issub(x, y, env)

        # see if we need to extend the search depth another level
        ll, lr = length(env.Lunion_sizes), length(env.Runion_sizes)
        ll == lastl && lr == lastr && return ans

        ll > lastl && push!(env.Lper_depth, ll)
        lr > lastr && push!(env.Rper_depth, lr)

        lastl, lastr = ll, lr
    end
end

function forall_exists_issub(x, y, env)
    lspace = Base.IteratorsMD.IndexIterator(tuple(env.Lunion_sizes...))
    rspace = Base.IteratorsMD.IndexIterator(tuple(env.Runion_sizes...))

    # for all combinations of elements from Unions on the left, there must
    # exist a combination of elements from Unions on the right that makes
    # issub() true. Unions in invariant position are on both the left and
    # the right in this formula.

    for forall in lspace
        env.Lunion_idxs = forall
        found = false
        for exists in rspace
            env.Runion_idxs = exists
            env.Lunion = env.Runion = 0
            env.Ldepth = env.Rdepth = 0
            if issub(x, y, env)
                found = true; break
            end
        end
        if !found
            return false
        end
    end
    return true
end

issub(x, y, env) = (x === y)

nparts{N}(::Base.IteratorsMD.CartesianIndex{N}) = N

function discover_Lunion(a::UnionT, env)
    env.Lunion += 1
    if env.Lunion > nparts(env.Lunion_idxs)
        # on the first run through, just record the size of this union
        push!(env.Lunion_sizes, length(a.types))
        return true
    end
    return false
end

function discover_Runion(b::UnionT, env)
    env.Runion += 1
    if env.Runion > nparts(env.Runion_idxs)
        push!(env.Runion_sizes, length(b.types))
        return true
    end
    return false
end

function issub(a::UnionT, b::UnionT, env)
    a === BottomT && return true
    if discover_Lunion(a, env) | discover_Runion(b, env)
        return true
    end
    env.Ldepth += 1
    last_Lunion = env.Lunion
    env.Lunion = env.Lper_depth[env.Ldepth]
    env.Rdepth += 1
    last_Runion = env.Runion
    env.Runion = env.Rper_depth[env.Rdepth]

    ans = issub(a.types[env.Lunion_idxs.(last_Lunion)],
                b.types[env.Runion_idxs.(last_Runion)], env)

    env.Runion = last_Runion
    env.Rdepth -= 1
    env.Lunion = last_Lunion
    env.Ldepth -= 1
    return ans
end

function issub(a::UnionT, t::Ty, env)
    a === BottomT && return true
    if discover_Lunion(a, env)
        return true
    end
    env.Ldepth += 1
    last_Lunion = env.Lunion
    env.Lunion = env.Lper_depth[env.Ldepth]
    ans = issub(a.types[env.Lunion_idxs.(last_Lunion)], t, env)
    env.Lunion = last_Lunion
    env.Ldepth -= 1
    return ans
end

function issub(a::Ty, b::UnionT, env)
    b === BottomT && return false
    if discover_Runion(b, env)
        return true
    end
    env.Rdepth += 1
    last_Runion = env.Runion
    env.Runion = env.Rper_depth[env.Rdepth]
    ans = issub(a, b.types[env.Runion_idxs.(last_Runion)], env)
    env.Runion = last_Runion
    env.Rdepth -= 1
    return ans
end

function issub(a::TagT, b::TagT, env)
    a === b && return true
    b === AnyT && return true
    a === AnyT && return false
    if a.name !== b.name
        a.name === TupleName && return false
        return issub(super(a), b, env)
    end
    if a.name === TupleName
        va = a.vararg
        vb = b.vararg
        la = length(a.params)
        lb = length(b.params)
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
    d = env.depth
    if d != aa.depth  # && d > 1  ???
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
    d = env.depth
    if aa.depth != bb.depth  # && d > 1  ???
        # Vars must occur at same depth
        return false
    end
    if d > bb.depth
        # if there are invariant constructors between a UnionAll and
        # this occurrence of Var, then we have an equality constraint on Var.
        if isa(bb.ub,Var) && bb.ub !== a
            # right-side Var cannot equal more than one left-side Var, e.g.
            # (L,L) <: (T,S) but not (T,S) <: (R,R)
            return false
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
            bb.lb = aa.ub
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

function issub_unionall(a::Ty, b::UnionAllT, env)
    env.vars[b.var] = Bounds(b.var.lb, b.var.ub, env.depth, true)
    ans = issub(a, b.T, env)
    delete!(env.vars, b.var)
    return ans
end

function unionall_issub(a::UnionAllT, b::Ty, env)
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

convert(::Type{Ty}, t::Type)    = xlate(t)
convert(::Type{Ty}, t::TypeVar) = xlate(t)

issub(a::Type, b::Type)        = issub(xlate(a), xlate(b))
isequal_type(a::Type, b::Type) = isequal_type(xlate(a), xlate(b))


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
end

# level 4: Union
function test_4()
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

# tests that don't pass yet
function test_failing()
end
