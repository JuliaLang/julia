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

type ForAllT <: Ty
    var::Var
    T
    ForAllT(v::Var, t) = new(v, t)
    ForAllT(v::Var, t::Type) = new(v, convert(Ty, t))
end

function show(io::IO, x::ForAllT)
    if x.T === x.var && x.var.ub !== AnyT
        print(io, "¬", x.var.ub)
        return
    end
    print(io, "(∀ ")
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

inst(t::ForAllT, param) = subst(t.T, Dict{Any,Any}(t.var => param))
inst(t::ForAllT, param, rest...) = inst(inst(t,param), rest...)

super(t::TagT) = inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = t===AnyT ? t : TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = t===BottomT ? t : UnionT(map(x->subst(x,env), t.types))
subst(t::Var,     env) = get(env, t, t)
subst(t::ForAllT, env) = (assert(!haskey(env, t.var));
                          newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                          ForAllT(newVar, subst(t.T, extend(env, t.var, newVar))))

TupleName = TypeName(:Tuple, AnyT)
TupleT = TagT(TupleName, (AnyT,), true)
tupletype(xs...) = inst(TupleName, xs...)


# subtype

isequal_type(x::Ty, y::Ty) = issub(x, y, true)

issub(x, y, env, inv) = (x === y)

function issub(x::Ty, y::Ty, inv::Bool = false)
    env = Dict()
    ans = issub(x, y, env, inv)
    if ans && !isempty(env)
        println("subject to")
        Base.showdict(env, limit=false); println()
    end
    ans
end

function union_issub(x::UnionT, t::Ty, env, invariant)
    for tt in x.types
        if !issub(tt, t, env, false)
            return false
        end
    end
    if invariant
        return issub(t, x, copy(env), false)
    end
    return true
end

issub(x::UnionT, t::UnionT, env, invariant) = union_issub(x, t, env, invariant)
issub(x::UnionT, t::Ty, env, invariant)     = union_issub(x, t, env, invariant)

function issub(x::Ty, t::UnionT, env, invariant)
    # TODO: distribute Union over tuple
    for tt in t.types
        e′ = copy(env)
        if issub(x, tt, e′, false)
            merge!(env, e′)
            if invariant
                return issub(t, x, copy(env), false)
            end
            return true
        end
    end
    return false
end

function issub(a::TagT, b::TagT, env, invariant)
    a === b && return true
    if !invariant
        b === AnyT && return true
        a === AnyT && return false
    end
    if a.name !== b.name
        invariant && return false
        return issub(super(a), b, env, false)
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
            ai > la && return bi > lb || (vb && !invariant)
            bi > lb && return false
            !issub(a.params[ai], b.params[bi], env, invariant) && return false
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
        for i = 1:length(a.params)
            if !issub(a.params[i], b.params[i], env, true)
                return false
            end
        end
    end
    return true
end

type EqConstraint    # T == rhs
    rhs
end

type SubConstraint   # T <: rhs
    rhs
end

type SupConstraint   # T >: rhs
    rhs
end

function add_constraint!(env, var, c::EqConstraint)
    if c.rhs !== var
        cs = get!(()->[], env, var)
        push!(cs, c)
    end
end

function add_constraint!(env, var, c::SubConstraint)
    if c.rhs !== AnyT && c.rhs !== var
        cs = get!(()->[], env, var)
        push!(cs, c)
    end
end

function add_constraint!(env, var, c::SupConstraint)
    if c.rhs !== BottomT && c.rhs !== var
        cs = get!(()->[], env, var)
        push!(cs, c)
    end
end

ctr = 1
rename(x) = x
function rename(x::ForAllT)
    global ctr
    v = Var(symbol(string("a",ctr)), x.var.lb, rename(x.var.ub))
    ctr += 1
    ForAllT(v, rename(inst(x, v)))
end

function issub_var(a::Var, b, env, invariant)
    if b === AnyT
        return true
    end
    if haskey(env, a) && !invariant
        return issub(rename(env[a][1].rhs), b, env, invariant)
    end
    add_constraint!(env, a, invariant ? EqConstraint(b) : SubConstraint(b))
    return true
end

issub(a::Var, b::Var, env, invariant) = issub_var(a, b, env, invariant)

function issub(a::Var, b::Ty, env, invariant)
    if invariant
        return false
    end
    issub_var(a, b, env, invariant)
end

function issub(a::Ty, b::Var, env, invariant)
    add_constraint!(env, b, invariant ? EqConstraint(a) : SupConstraint(a))
    return true
end

function issub(a::ForAllT, b::ForAllT, env, invariant)
    println(a, " <: ", b); sleep(.5)

    # 1. handle bounds
    if !(issub(b.var.ub, a.var.ub, copy(env), false) &&
         issub(a.var.lb, b.var.lb, copy(env), false))
        return false
    end

    # 2. handle expression
    # for contravariance, fresh<:b.var.ub, for covariance fresh<:a.var.ub
    lb = b.var.lb
    ub = b.var.ub
    #var = Var(symbol(string("a",ctr)), lb, ub)
    #global ctr += 1
    var = b.var

    env = copy(env)
    #add_constraint!(env, var, SupConstraint(lb))
    add_constraint!(env, var, SubConstraint(ub))

    a = inst(a, var)
    b = b.T

    inner = issub(a, b, env, invariant)
    return inner
end

function issub_forall(a::Ty, b::ForAllT, env, invariant)
    fresh = Var(b.var.name, b.var.lb, b.var.ub)
    body = inst(b, fresh)
    add_constraint!(env, fresh, SupConstraint(b.var.lb))
    add_constraint!(env, fresh, SubConstraint(b.var.ub))
    return issub(a, body, env, invariant)
end

issub(a::UnionT, b::ForAllT, env, invariant) = issub_forall(a, b, env, invariant)
issub(a::Ty, b::ForAllT, env, invariant) = issub_forall(a, b, env, invariant)

function forall_issub(a::ForAllT, b::Ty, env, invariant)
    fresh = Var(a.var.name, a.var.lb, a.var.ub)
    body = inst(a, fresh)
    add_constraint!(env, fresh, SupConstraint(a.var.lb))
    add_constraint!(env, fresh, SubConstraint(a.var.ub))
    return issub(body, b, env, invariant)
end

issub(a::ForAllT, b::UnionT, env, invariant) = forall_issub(a, b, env, invariant)
issub(a::ForAllT, b::Ty, env, invariant) = forall_issub(a, b, env, invariant)


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
            ForAllT($(esc(v)), $(esc(expr)))
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
            sup = ForAllT(para[i], sup)
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

function non_terminating()
    # undecidable F_<: instance
    ¬T = @UnionAll α<:T α

    θ = @UnionAll α ¬(@UnionAll β<:α ¬β)

    a0 = Var(:a0)
    env = Dict{Any,Any}(a0 => Any[SubConstraint(θ)])

    issub(a0, (@UnionAll a1<:a0 ¬a1), env, false)
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
end
