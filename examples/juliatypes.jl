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

AnyT = TagT(TypeName(:Any), ())
AnyT.name.super = AnyT

BottomT = UnionT(())

inst(typename::TypeName, params...) = TagT(typename, params)

inst(t::TagT) = t

inst(t::UnionAllT, param) = subst(t.T, Dict{Any,Any}(t.var => param))
inst(t::UnionAllT, param, rest...) = inst(inst(t,param), rest...)

super(t::TagT) = inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = t===AnyT ? t : TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = t===BottomT ? t : UnionT(map(x->subst(x,env), t.types))
subst(t::Var,     env) = get(env, t, t)
subst(t::UnionAllT, env) = (assert(!haskey(env, t.var));
                            newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                            UnionAllT(newVar, subst(t.T, extend(env, t.var, newVar))))
subst(t, env) = t

TupleName = TypeName(:Tuple, AnyT)
TupleT = TagT(TupleName, (AnyT,), true)
tupletype(xs...) = inst(TupleName, xs...)


# subtype

type Bounds
    # record current lower and upper bounds of a Var
    # depth: invariant position nesting depth of a Var's UnionAll
    # right: whether this Var is on the right-hand side of A <: B
    lb
    ub
    depth::Int
    right::Bool
end

Base.copy(b::Bounds) = Bounds(b.lb, b.ub, b.depth, b.right)

# maps Var to Bounds, and `:depth` to current depth
const Env = Dict{Any,Any}

isequal_type(x::Ty, y::Ty) = issub(x, y) && issub(y, x)

function issub(x, y)
    env = Env()
    env[:depth] = 1
    ans = issub(x, y, env)
    #if ans && !isempty(env)
    #    println("subject to")
    #    Base.showdict(env, limit=false); println()
    #end
    ans
end

issub(x, y, env) = (x === y)

function union_issub(x::UnionT, t::Ty, env)
    for tt in x.types
        if !issub(tt, t, env)
            return false
        end
    end
    return true
end

issub(x::UnionT, t::UnionT, env) = union_issub(x, t, env)
issub(x::UnionT, t::Ty, env)     = union_issub(x, t, env)

function issub(x::Ty, t::UnionT, env)
    # TODO: distribute Union over tuple
    for tt in t.types
        e′ = Env([ k => copy(v) for (k,v) in env])
        if issub(x, tt, e′)
            merge!(env, e′)
            return true
        end
    end
    return false
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
        env = copy(env)
        env[:depth] = env[:depth]+1  # crossing invariant constructor, increment depth
        for i = 1:length(a.params)
            ai, bi = a.params[i], b.params[i]
            # use issub in both directions to test equality
            if !(ai===bi || (issub(ai, bi, env) && issub(bi, ai, env)))
                return false
            end
        end
    end
    return true
end

function issub(a::Var, b::Ty, env)
    aa = env[a]
    # Vars are fully checked by the "forward" direction of A<:B in
    # invariant position. So just return true when checking the "flipped"
    # direction B<:A.
    aa.right && return true
    d = env[:depth]
    if d != aa.depth  # && d > 1  ???
        # Var <: non-Var can only be true when there are no invariant
        # constructors between the UnionAll and this occurrence of Var.
        return false
    end
    return issub(aa.ub, b, env)
end

function issub(a::Var, b::Var, env)
    a === b && return true
    aa = env[a]
    aa.right && return true
    bb = env[b]
    d = env[:depth]
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
        if !(issub(bb.lb, aa.ub, env) && issub(aa.ub, bb.ub, env))
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
    bb = env[b]
    !bb.right && return true
    if env[:depth] > bb.depth
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
    env[b.var] = Bounds(b.var.lb, b.var.ub, env[:depth], true)
    ans = issub(a, b.T, env)
    delete!(env, b.var)
    return ans
end

function unionall_issub(a::UnionAllT, b::Ty, env)
    env[a.var] = Bounds(a.var.lb, a.var.ub, env[:depth], false)
    ans = issub(a.T, b, env)
    delete!(env, a.var)
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
    UnionT(map(x->xlate(x,env), t.types))
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

tndict[AbstractArray.name] = AbstractArrayT.T.T.name
tndict[Array.name] = ArrayT.T.T.name

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
    @test issub_strict((@UnionAll T inst(ArrayT,T,T)), (@UnionAll T @UnionAll S inst(ArrayT,T,S)))
    @test issub(inst(ArrayT,Ty(Int),Ty(Int8)), (@UnionAll T @UnionAll S inst(ArrayT,T,S)))
    @test issub(inst(ArrayT,Ty(Int),Ty(Int8)), (@UnionAll S inst(ArrayT,Ty(Int),S)))

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

    @test !issub((@UnionAll T inst(ArrayT,T,T)), inst(ArrayT,Ty(Int),Ty(Int8)))
    @test !issub((@UnionAll T inst(ArrayT,T,T)), inst(ArrayT,Ty(Int),Ty(Int)))

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


    @test !issub(inst(ArrayT,Ty(Int),Ty(Int8)), (@UnionAll T inst(ArrayT,T,T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Int),1), Ty(Integer)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(Ty(Integer), inst(ArrayT,Ty(Int),1)),
                 (@UnionAll T<:Ty(Integer) tupletype(T, inst(ArrayT,T,1))))

    @test !issub(Ty(Array{Array{Int,1},Integer}),
                 (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test issub(Ty(Array{Array{Int,1},Int}),
                (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))
end

# level 4: Union
function test_4()
    @test issub_strict(Int, Union(Int,String))
    @test issub_strict(Union(Int,Int8), Integer)

    @test isequal_type(Union(Int,Int8), Union(Int,Int8))

    @test isequal_type(UnionT((Ty(Int),Ty(Integer))), Ty(Integer))

    @test issub_strict((Int,Int8,Int), (Union(Int,Int8)...,))
    @test issub_strict((Int,Int8,Int), (Union(Int,Int8,Int16)...,))
end

# level 5: union and UnionAll
function test_5()
    @test issub(Ty((String,Array{Int,1})),
                (@UnionAll T UnionT((tupletype(T,inst(ArrayT,T,1)),
                                     tupletype(T,inst(ArrayT,Ty(Int),1))))))
end

# tests that don't pass yet
function test_failing()
    @test issub((Union(Int,Int8),Int16), Union((Int,Int16),(Int8,Int16)))
end
