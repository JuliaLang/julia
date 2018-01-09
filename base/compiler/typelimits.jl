# This file is a part of Julia. License is MIT: https://julialang.org/license

#########################
# limitation parameters #
#########################

const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 8
const MAX_INLINE_CONST_SIZE = 256
const TUPLE_COMPLEXITY_LIMIT_DEPTH = 3

#########################
# limitation heuristics #
#########################

function limit_type_depth(@nospecialize(t), d::Int)
    r = limit_type_depth(t, d, true, TypeVar[])
    @assert !isa(t, Type) || t <: r
    return r
end

limit_tuple_depth(params::Params, @nospecialize(t)) = limit_tuple_depth_(params,t,0)

function limit_tuple_depth_(params::Params, @nospecialize(t), d::Int)
    if isa(t,Union)
        # also limit within Union types.
        # may have to recur into other stuff in the future too.
        return Union{limit_tuple_depth_(params, t.a, d+1),
                     limit_tuple_depth_(params, t.b, d+1)}
    elseif isa(t,UnionAll)
        ub = limit_tuple_depth_(params, t.var.ub, d)
        if ub !== t.var.ub
            var = TypeVar(t.var.name, t.var.lb, ub)
            body = t{var}
        else
            var = t.var
            body = t.body
        end
        body = limit_tuple_depth_(params, body, d)
        return UnionAll(var, body)
    elseif !(isa(t,DataType) && t.name === Tuple.name)
        return t
    elseif d > params.MAX_TUPLE_DEPTH
        return Tuple
    end
    p = map(x->limit_tuple_depth_(params,x,d+1), t.parameters)
    Tuple{p...}
end

limit_tuple_type = (@nospecialize(t), params::Params) -> limit_tuple_type_n(t, params.MAX_TUPLETYPE_LEN)

function limit_tuple_type_n(@nospecialize(t), lim::Int)
    if isa(t,UnionAll)
        return UnionAll(t.var, limit_tuple_type_n(t.body, lim))
    end
    p = t.parameters
    n = length(p)
    if n > lim
        tail = reduce(typejoin, Bottom, Any[p[lim:(n-1)]..., unwrapva(p[n])])
        return Tuple{p[1:(lim-1)]..., Vararg{tail}}
    end
    return t
end

function limit_type_depth(@nospecialize(t), d::Int, cov::Bool, vars::Vector{TypeVar}=TypeVar[])
    if isa(t, Union)
        if d < 0
            if cov
                return Any
            else
                var = TypeVar(:_)
                push!(vars, var)
                return var
            end
        end
        return Union{limit_type_depth(t.a, d - 1, cov, vars),
                     limit_type_depth(t.b, d - 1, cov, vars)}
    elseif isa(t, UnionAll)
        v = t.var
        if v.ub === Any
            if v.lb === Bottom
                return UnionAll(t.var, limit_type_depth(t.body, d, cov, vars))
            end
            ub = Any
        else
            ub = limit_type_depth(v.ub, d - 1, cov, vars)
        end
        if v.lb === Bottom || type_depth(v.lb) > d
            # note: lower bounds need to be widened by making them lower
            lb = Bottom
        else
            lb = v.lb
        end
        v2 = TypeVar(v.name, lb, ub)
        return UnionAll(v2, limit_type_depth(t{v2}, d, cov, vars))
    elseif !isa(t,DataType)
        return t
    end
    P = t.parameters
    isempty(P) && return t
    if d < 0
        if isvarargtype(t)
            # never replace Vararg with non-Vararg
            # passing depth=0 avoids putting a bare typevar here, for the diagonal rule
            return Vararg{limit_type_depth(P[1], 0, cov, vars), P[2]}
        end
        widert = t.name.wrapper
        if !(t <: widert)
            # This can happen when a typevar has bounds too wide for its context, e.g.
            # `Complex{T} where T` is not a subtype of `Complex`. In that case widen even
            # faster to something safe to ensure the result is a supertype of the input.
            widert = Any
        end
        cov && return widert
        var = TypeVar(:_, widert)
        push!(vars, var)
        return var
    end
    stillcov = cov && (t.name === Tuple.name)
    newdepth = d - 1
    if isvarargtype(t)
        newdepth = max(newdepth, 0)
    end
    Q = map(x -> limit_type_depth(x, newdepth, stillcov, vars), P)
    R = t.name.wrapper{Q...}
    if cov && !stillcov
        for var in vars
            R = UnionAll(var, R)
        end
    end
    return R
end

# limit the complexity of type `t` to be simpler than the comparison type `compare`
# no new values may be introduced, so the parameter `source` encodes the set of all values already present
# the outermost tuple type is permitted to have up to `allowed_tuplelen` parameters
function limit_type_size(@nospecialize(t), @nospecialize(compare), @nospecialize(source), allowed_tuplelen::Int)
    source = svec(unwrap_unionall(compare), unwrap_unionall(source))
    source[1] === source[2] && (source = svec(source[1]))
    type_more_complex(t, compare, source, 1, TUPLE_COMPLEXITY_LIMIT_DEPTH, allowed_tuplelen) || return t
    r = _limit_type_size(t, compare, source, 1, allowed_tuplelen)
    @assert t <: r
    #@assert r === _limit_type_size(r, t, source) # this monotonicity constraint is slightly stronger than actually required,
      # since we only actually need to demonstrate that repeated application would reaches a fixed point,
      #not that it is already at the fixed point
    return r
end

# type vs. comparison or which was derived from source
function _limit_type_size(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, allowed_tuplelen::Int)
    if t === c
        return t # quick egal test
    elseif t === Union{}
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    elseif isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return t # t is already wider than the comparison in the type lattice
    elseif is_derived_type_from_any(unwrap_unionall(t), sources, depth)
        return t # t isn't something new
    end
    if isa(t, TypeVar)
        if isa(c, TypeVar)
            if t.ub === c.ub && t.lb === c.lb
                return t
            end
        end
    elseif isa(t, Union)
        if isa(c, Union)
            a = _limit_type_size(t.a, c.a, sources, depth, allowed_tuplelen)
            b = _limit_type_size(t.b, c.b, sources, depth, allowed_tuplelen)
            return Union{a, b}
        end
    elseif isa(t, UnionAll)
        if isa(c, UnionAll)
            tv = t.var
            cv = c.var
            if tv.ub === cv.ub
                if tv.lb === cv.lb
                    return UnionAll(tv, _limit_type_size(t.body, c.body, sources, depth + 1, allowed_tuplelen))
                end
                ub = tv.ub
            else
                ub = _limit_type_size(tv.ub, cv.ub, sources, depth + 1, 0)
            end
            if tv.lb === cv.lb
                lb = tv.lb
            else
                # note: lower bounds need to be widened by making them lower
                lb = Bottom
            end
            v2 = TypeVar(tv.name, lb, ub)
            return UnionAll(v2, _limit_type_size(t{v2}, c{v2}, sources, depth + 1, allowed_tuplelen))
        end
        tbody = _limit_type_size(t.body, c, sources, depth + 1, allowed_tuplelen)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)
    elseif isa(c, UnionAll)
        # peel off non-matching wrapper of comparison
        return _limit_type_size(t, c.body, sources, depth, allowed_tuplelen)
    elseif isa(t, DataType)
        if isa(c, DataType)
            tP = t.parameters
            cP = c.parameters
            if t.name === c.name && !isempty(cP)
                if isvarargtype(t)
                    VaT = _limit_type_size(tP[1], cP[1], sources, depth + 1, 0)
                    N = tP[2]
                    if isa(N, TypeVar) || N === cP[2]
                        return Vararg{VaT, N}
                    end
                    return Vararg{VaT}
                elseif t.name === Tuple.name
                    # for covariant datatypes (Tuple),
                    # apply type-size limit element-wise
                    ltP = length(tP)
                    lcP = length(cP)
                    np = min(ltP, max(lcP, allowed_tuplelen))
                    Q = Any[ tP[i] for i in 1:np ]
                    if ltP > np
                        # combine tp[np:end] into tP[np] using Vararg
                        Q[np] = tuple_tail_elem(Bottom, Any[ tP[i] for i in np:ltP ])
                    end
                    for i = 1:np
                        # now apply limit element-wise to Q
                        # padding out the comparison as needed to allowed_tuplelen elements
                        if i <= lcP
                            cPi = cP[i]
                        elseif isvarargtype(cP[lcP])
                            cPi = cP[lcP]
                        else
                            cPi = Any
                        end
                        Q[i] = _limit_type_size(Q[i], cPi, sources, depth + 1, 0)
                    end
                    return Tuple{Q...}
                end
            elseif isvarargtype(c)
                # Tuple{Vararg{T}} --> Tuple{T} is OK
                return _limit_type_size(t, cP[1], sources, depth, 0)
            end
        end
        if isType(t) # allow taking typeof as Type{...}, but ensure it doesn't start nesting
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) && return t
            end
        end
        if isvarargtype(t)
            # never replace Vararg with non-Vararg
            return Vararg
        end
        if allowed_tuplelen < 1 && t.name === Tuple.name
            return Any
        end
        widert = t.name.wrapper
        if !(t <: widert)
            # This can happen when a typevar has bounds too wide for its context, e.g.
            # `Complex{T} where T` is not a subtype of `Complex`. In that case widen even
            # faster to something safe to ensure the result is a supertype of the input.
            return Any
        end
        return widert
    end
    return Any
end

function type_more_complex(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, tupledepth::Int, allowed_tuplelen::Int)
    # detect cases where the comparison is trivial
    if t === c
        return false
    elseif t === Union{}
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif tupledepth > 0 && isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return false # t is already wider than the comparison in the type lattice
    elseif tupledepth > 0 && is_derived_type_from_any(unwrap_unionall(t), sources, depth)
        return false # t isn't something new
    end
    # peel off wrappers
    if isa(c, UnionAll)
        # allow wrapping type with fewer UnionAlls than comparison if in a covariant context
        if !isa(t, UnionAll) && tupledepth == 0
            return true
        end
        t = unwrap_unionall(t)
        c = unwrap_unionall(c)
    end
    # rules for various comparison types
    if isa(c, TypeVar)
        tupledepth = 1 # allow replacing a TypeVar with a concrete value (since we know the UnionAll must be in covariant position)
        if isa(t, TypeVar)
            return !(t.lb === Union{} || t.lb === c.lb) || # simplify lb towards Union{}
                   type_more_complex(t.ub, c.ub, sources, depth + 1, tupledepth, 0)
        end
        c.lb === Union{} || return true
        return type_more_complex(t, c.ub, sources, depth, tupledepth, 0)
    elseif isa(c, Union)
        if isa(t, Union)
            return type_more_complex(t.a, c.a, sources, depth, tupledepth, allowed_tuplelen) ||
                   type_more_complex(t.b, c.b, sources, depth, tupledepth, allowed_tuplelen)
        end
        return type_more_complex(t, c.a, sources, depth, tupledepth, allowed_tuplelen) &&
               type_more_complex(t, c.b, sources, depth, tupledepth, allowed_tuplelen)
    elseif isa(t, Int) && isa(c, Int)
        return t !== 1 # alternatively, could use !(0 <= t < c)
    end
    # base case for data types
    if isa(t, DataType)
        tP = t.parameters
        if isa(c, DataType) && t.name === c.name
            cP = c.parameters
            length(cP) < length(tP) && return true
            ntail = length(cP) - length(tP) # assume parameters were dropped from the tuple head
            # allow creating variation within a nested tuple, but only so deep
            if t.name === Tuple.name && tupledepth > 0
                tupledepth -= 1
            elseif !isvarargtype(t)
                tupledepth = 0
            end
            isgenerator = (t.name.name === :Generator && t.name.module === _topmod(t.name.module))
            for i = 1:length(tP)
                tPi = tP[i]
                cPi = cP[i + ntail]
                if isgenerator
                    let tPi = unwrap_unionall(tPi),
                        cPi = unwrap_unionall(cPi)
                        if isa(tPi, DataType) && isa(cPi, DataType) &&
                                !tPi.abstract && !cPi.abstract &&
                                sym_isless(cPi.name.name, tPi.name.name)
                            # allow collect on (anonymous) Generators to nest, provided that their functions are appropriately ordered
                            # TODO: is there a better way?
                            continue
                        end
                    end
                end
                type_more_complex(tPi, cPi, sources, depth + 1, tupledepth, 0) && return true
            end
            return false
        elseif isvarargtype(c)
            return type_more_complex(t, unwrapva(c), sources, depth, tupledepth, 0)
        end
        if isType(t) # allow taking typeof any source type anywhere as Type{...}, as long as it isn't nesting Type{Type{...}}
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) || return true
                return false
            end
        end
    end
    return true
end

function type_too_complex(@nospecialize(t), d::Int)
    if d < 0
        return true
    elseif isa(t, Union)
        return type_too_complex(t.a, d - 1) || type_too_complex(t.b, d - 1)
    elseif isa(t, TypeVar)
        return type_too_complex(t.lb, d - 1) || type_too_complex(t.ub, d - 1)
    elseif isa(t, UnionAll)
        return type_too_complex(t.var, d) || type_too_complex(t.body, d)
    elseif isa(t, DataType)
        for x in (t.parameters)::SimpleVector
            if type_too_complex(x, d - 1)
                return true
            end
        end
    end
    return false
end
