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

type UnionAllT <: Ty
    var::Var
    T
    UnionAllT(v::Var, t) = new(v, t)
    UnionAllT(v::Var, t::Type) = new(v, convert(Ty, t))
end

function show(io::IO, x::UnionAllT)
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

const Env = Dict{Any,Any}

isequal_type(x::Ty, y::Ty) = issub(x, y) && issub(y, x)

function issub(x, y)
    env = Env()
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
        e′ = copy(env)
        if issub(x, tt, e′)
            merge!(env, e′)
            return true
        end
    end
    return false
end

function issub(a, b, env, invariant)
    if invariant && !issub(b, a, env)
        return false
    end
    return issub(a, b, env)
end

function issub(a::TagT, b::TagT, env, invariant=false)
    a === b && return true
    if !invariant
        b === AnyT && return true
        a === AnyT && return false
    end
    if a.name !== b.name
        invariant && return false
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

#rename(x) = x
#function rename(x::UnionAllT)
#    v = Var(x.var.name, rename(x.var.lb), rename(x.var.ub))
#    UnionAllT(v, rename(inst(x, v)))
#end

function issub(a::Var, b::Ty, env, invariant=false)
    invariant && return false
    if b === AnyT || a === b
        return true
    end
    return issub(a.ub, b, env)
end

function issub(a::Var, b::Var, env, invariant=false)
    if a === b
        return true
    end
    if invariant && haskey(env, b)
        return false
    end
    return issub(a.ub, b.ub, env)
end

issub(a::Ty, b::Var, env, invariant=false) = issub(a, b.ub, env)

function issub(a::UnionAllT, b::UnionAllT, env)
    if !issub(a.var.ub, b.var.ub, env)
        return false
    end
    env = copy(env)
    var = Var(a.var.name, a.var.lb, a.var.ub)
    env[var] = true
    return issub(inst(a,var), inst(b,var), env)
end

issub_unionall(a::Ty, b::UnionAllT, env) = issub(a, b.T, env)

issub(a::UnionT, b::UnionAllT, env) = issub_unionall(a, b, env)
issub(a::Ty, b::UnionAllT, env) = issub_unionall(a, b, env)

function unionall_issub(a::UnionAllT, b::Ty, env)
    var = Var(a.var.name, a.var.lb, a.var.ub)
    env = copy(env)
    env[var] = true
    return issub(inst(a,var), b, env)
end

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
end

function test_3()
    @test issub_strict(xlate(Array{Int,1}), @UnionAll T inst(ArrayT, T, 1))
    @test issub_strict((@UnionAll T inst(ArrayT,T,T)), (@UnionAll T @UnionAll S inst(ArrayT,T,S)))

    @test issub((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test issub((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test isequal_type((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test isequal_type((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))
end
