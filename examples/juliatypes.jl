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
                return issub(t, x, env, false)
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
        return issub(t, x, env, false)
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
        # TODO
    else
        for i = 1:length(a.params)
            if !issub(a.params[i], b.params[i], env, true)
                return false
            end
        end
    end
    return true
end
