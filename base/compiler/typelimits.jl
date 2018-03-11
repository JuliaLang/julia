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

limit_tuple_type(@nospecialize(t), params::Params) = limit_tuple_type_n(t, params.MAX_TUPLETYPE_LEN)

function limit_tuple_type_n(@nospecialize(t), lim::Int)
    if isa(t, UnionAll)
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

# try to find `type` somewhere in `comparison` type
# at a minimum nesting depth of `mindepth`
function is_derived_type(@nospecialize(t), @nospecialize(c), mindepth::Int)
    if mindepth > 0
        mindepth -= 1
    end
    if t === c
        return mindepth == 0
    end
    if isa(c, TypeVar)
        # see if it is replacing a TypeVar upper bound with something simpler
        return is_derived_type(t, c.ub, mindepth)
    elseif isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a, mindepth + 1) || is_derived_type(t, c.b, mindepth + 1)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        return is_derived_type(t, c.body, mindepth)
    elseif isa(c, DataType)
        if isa(t, DataType)
            # see if it is one of the supertypes of a parameter
            super = supertype(c)
            while super !== Any
                t === super && return true
                super = supertype(super)
            end
        end
        # see if it was extracted from a type parameter
        cP = c.parameters
        for p in cP
            is_derived_type(t, p, mindepth) && return true
        end
        if isconcretetype(c) && isbits(c)
            # see if it was extracted from a fieldtype
            # however, only look through types that can be inlined
            # to ensure monotonicity of derivation
            # since we know that for immutable, concrete, bits types,
            # the field types must have been constructed prior to the type,
            # it cannot have a reference cycle in the type graph
            cF = c.types
            for f in cF
                is_derived_type(t, f, mindepth) && return true
            end
        end
    end
    return false
end

function is_derived_type_from_any(@nospecialize(t), sources::SimpleVector, mindepth::Int)
    for s in sources
        is_derived_type(t, s, mindepth) && return true
    end
    return false
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
        return t !== 1 && !(0 <= t < c) # alternatively, could use !(abs(t) <= abs(c) || abs(t) < n) for some n
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
