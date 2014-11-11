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
    lb::Ty
    ub::Ty
    Var(n, lb=BottomT, ub=AnyT) = Var(n, lb, ub)
end

type ForAllT <: Ty
    var::Var
    T::Ty
end

type Value
    tag::TagT
    data
end

AnyT = TagT(TypeName(:Any), ())
AnyT.name.super = AnyT

BottomT = UnionT(())

inst(typename::TypeName, params...) = TagT(typename, params)

inst(t::ForAllT) = t
inst(t::ForAllT, param) = subst(t.T, Dict(t.var => param))
inst(t::ForAllT, param, rest...) = inst(inst(t,param), rest...)

super(t::TagT) = inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = UnionT(map(x->subst(x,env), t.types))
subst(t::Var,     env) = get(env, t, t)
subst(t::ForAllT, env) = (assert(!haskey(env, t.var));
                          newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                          ForAllT(newVar, subst(t.T, extend(env, t.var, newVar))))

let T = Var(:T), N = Var(:N)
    AbstractArrayName = TypeName(:AbstractArray,AnyT)
    global AbstractArrayT = ForAllT(T, ForAllT(N, inst(AbstractArrayName, T, N)))
end

let T = Var(:T), N = Var(:N)
    ArrayName = TypeName(:Array, ForAllT(T, ForAllT(N, inst(AbstractArrayT, T, N))))
    global ArrayT = ForAllT(T, ForAllT(N, inst(ArrayName, T, N)))
end

TupleName = TypeName(:Tuple, AnyT)
TupleT = TagT(TupleName, (AnyT,), true)

# subtype

isequal_type(x::Ty, y::Ty) = issub(x, y, Dict(), true)
issub(x::Ty, y::Ty) = issub(x, y, Dict(), false)

function union_issub(x, t::UnionT, env, invariant)
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

issub(x::Ty, t::UnionT, env, invariant) = union_issub(x, t, env, invariant)
issub(x::UnionT, t::UnionT, env, invariant) = union_issub(x, t, env, invariant)

function issub(x::UnionT, t::Ty, env, invariant)
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
        if va && !vb
            return false
        end
        la = length(a.params)
        lb = length(b.params)
        ai = bi = 1
        while true
            ai > la && return bi > lb || (vb && !invariant)
            bi > lb && return false
            !issub(a.params[i], b.params[i], env, invariant) && return false
            ai==la && bi==lb && va && vb && return true
            if ai < la || (ai==la && !va)
                ai += 1
            end
            if bi < lb || (bi==lb && !vb)
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

function issub_var(a::Var, b, env, invariant)
    c = get!(()->[], env, a)
    push!(c, invariant ? EqConstraint(b) : SubConstraint(b))
    return true
end

issub(a::Var, b::Var, env, invariant) = issub_var(a, b, env, invariant)
issub(a::Var, b::Ty, env, invariant) = issub_var(a, b, env, invariant)

function issub(a::Ty, b::Var, env, invariant)
    c = get!(()->[], env, b)
    push!(c, invariant ? EqConstraint(a) : SupConstraint(a))
    return true
end

# body_for(t::Ty, env) = t
# function body_for(a::ForAllT, env)
#     fresh = Var(a.var.name, a.var.lb, a.var.ub)
#     body = inst(a, fresh)
#     c = Any[]
#     if fresh.ub !== AnyT
#         push!(c, SubConstraint(fresh.ub))
#     end
#     if fresh.lb !== BottomT
#         push!(c, SupConstraint(fresh.lb))
#     end
#     env[fresh] = c
#     body
# end

# function issub(a::Ty, b::Ty, env, invariant)
#     @assert isa(a,ForAllT) || isa(b,ForAllT)
#     a = body_for(a, env)
#     b = body_for(b, env)
#     return issub(a, b, env, invariant)
# end

# translating from existing julia types

const tndict = ObjectIdDict()

xlate(t) = xlate(t, ObjectIdDict())

xlate(t::UnionType, env) = UnionT(map(x->xlate(x,env), t.types))

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
