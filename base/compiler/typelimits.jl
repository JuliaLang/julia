# This file is a part of Julia. License is MIT: https://julialang.org/license

#########################
# limitation parameters #
#########################

const MAX_TYPEUNION_COMPLEXITY = 3
const MAX_INLINE_CONST_SIZE = 256

#########################
# limitation heuristics #
#########################

# limit the complexity of type `t` to be simpler than the comparison type `compare`
# no new values may be introduced, so the parameter `source` encodes the set of all values already present
# the outermost tuple type is permitted to have up to `allowed_tuplelen` parameters
function limit_type_size(@nospecialize(t), @nospecialize(compare), @nospecialize(source), allowed_tupledepth::Int, allowed_tuplelen::Int)
    source = svec(unwrap_unionall(compare), unwrap_unionall(source))
    source[1] === source[2] && (source = svec(source[1]))
    type_more_complex(t, compare, source, 1, allowed_tupledepth, allowed_tuplelen) || return t
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
    if t === c
        return mindepth <= 1
    end
    if isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a, mindepth) || is_derived_type(t, c.b, mindepth)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        # also handle the var here, since this construct bounds the mindepth to the smallest possible value
        return is_derived_type(t, c.var.ub, mindepth) || is_derived_type(t, c.body, mindepth)
    elseif isa(c, DataType)
        if mindepth > 0
            mindepth -= 1
        end
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
        if isconcretetype(c) && isbitstype(c)
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

# The goal of this function is to return a type of greater "size" and less "complexity" than
# both `t` or `c` over the lattice defined by `sources`, `depth`, and `allowed_tuplelen`.
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
    # peel off (and ignore) wrappers - they contribute no useful information, so we don't need to consider their size
    # first attempt to turn `c` into a type that contributes meaningful information
    # by peeling off meaningless non-matching wrappers of comparison one at a time
    # then unwrap `t`
    if isa(c, TypeVar)
        if isa(t, TypeVar) && t.ub === c.ub && (t.lb === Union{} || t.lb === c.lb)
            return t # it's ok to change the name, or widen `lb` to Union{}, so we can handle this immediately here
        end
        return _limit_type_size(t, c.ub, sources, depth, allowed_tuplelen)
    end
    if isa(c, UnionAll)
        return _limit_type_size(t, c.body, sources, depth, allowed_tuplelen)
    end
    if isa(t, UnionAll)
        tbody = _limit_type_size(t.body, c, sources, depth, allowed_tuplelen)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)
    elseif isa(t, TypeVar)
        # don't have a matching TypeVar in comparison, so we keep just the upper bound
        return _limit_type_size(t.ub, c, sources, depth, allowed_tuplelen)
    elseif isa(t, Union)
        if isa(c, Union)
            a = _limit_type_size(t.a, c.a, sources, depth, allowed_tuplelen)
            b = _limit_type_size(t.b, c.b, sources, depth, allowed_tuplelen)
            return Union{a, b}
        end
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

# pick a wider type that contains both typea and typeb,
# with some limits on how "large" it can get,
# but without losing too much precision in common cases
# and also trying to be mostly associative and commutative
function tmerge(@nospecialize(typea), @nospecialize(typeb))
    typea ⊑ typeb && return typeb
    typeb ⊑ typea && return typea
    # type-lattice for MaybeUndef wrapper
    if isa(typea, MaybeUndef) || isa(typeb, MaybeUndef)
        return MaybeUndef(tmerge(
            isa(typea, MaybeUndef) ? typea.typ : typea,
            isa(typeb, MaybeUndef) ? typeb.typ : typeb))
    end
    # type-lattice for Conditional wrapper
    if isa(typea, Conditional) && isa(typeb, Const)
        if typeb.val === true
            typeb = Conditional(typea.var, Any, Union{})
        elseif typeb.val === false
            typeb = Conditional(typea.var, Union{}, Any)
        end
    end
    if isa(typeb, Conditional) && isa(typea, Const)
        if typea.val === true
            typea = Conditional(typeb.var, Any, Union{})
        elseif typea.val === false
            typea = Conditional(typeb.var, Union{}, Any)
        end
    end
    if isa(typea, Conditional) && isa(typeb, Conditional)
        if typea.var === typeb.var
            vtype = tmerge(typea.vtype, typeb.vtype)
            elsetype = tmerge(typea.elsetype, typeb.elsetype)
            if vtype != elsetype
                return Conditional(typea.var, vtype, elsetype)
            end
        end
        val = maybe_extract_const_bool(typea)
        if val isa Bool && val === maybe_extract_const_bool(typeb)
            return Const(val)
        end
        return Bool
    end
    # no special type-inference lattice, join the types
    typea, typeb = widenconst(typea), widenconst(typeb)
    typea === typeb && return typea
    if !(isa(typea, Type) || isa(typea, TypeVar)) ||
       !(isa(typeb, Type) || isa(typeb, TypeVar))
        # XXX: this should never happen
        return Any
    end
    # it's always ok to form a Union of two concrete types
    if (isconcretetype(typea) || isType(typea)) && (isconcretetype(typeb) || isType(typeb))
        return Union{typea, typeb}
    end
    # collect the list of types from past tmerge calls returning Union
    # and then reduce over that list
    types = Any[]
    _uniontypes(typea, types)
    _uniontypes(typeb, types)
    typenames = Vector{Core.TypeName}(undef, length(types))
    for i in 1:length(types)
        # check that we will be able to analyze (and simplify) everything
        # bail if everything isn't a well-formed DataType
        ti = types[i]
        uw = unwrap_unionall(ti)
        (uw isa DataType && ti <: uw.name.wrapper) || return Any
        typenames[i] = uw.name
    end
    # see if any of the union elements have the same TypeName
    # in which case, simplify this tmerge by replacing it with
    # the widest possible version of itself (the wrapper)
    for i in 1:length(types)
        ti = types[i]
        for j in (i + 1):length(types)
            if typenames[i] === typenames[j]
                tj = types[j]
                if ti <: tj
                    types[i] = Union{}
                    break
                elseif tj <: ti
                    types[j] = Union{}
                    typenames[j] = Any.name
                else
                    if typenames[i] === Tuple.name
                        # try to widen Tuple slower: make a single non-concrete Tuple containing both
                        # converge the Tuple element-wise if they are the same length
                        # see 4ee2b41552a6bc95465c12ca66146d69b354317b, be59686f7613a2ccfd63491c7b354d0b16a95c05,
                        widen = tuplemerge(unwrap_unionall(ti)::DataType, unwrap_unionall(tj)::DataType)
                        widen = rewrap_unionall(rewrap_unionall(widen, ti), tj)
                    else
                        widen = typenames[i].wrapper
                    end
                    types[i] = Union{}
                    types[j] = widen
                    break
                end
            end
        end
    end
    u = Union{types...}
    if unioncomplexity(u) <= MAX_TYPEUNION_COMPLEXITY
        # don't let type unions get too big, if the above didn't reduce it enough
        return u
    end
    # finally, just return the widest possible type
    return Any
end

# the inverse of switchtupleunion, with limits on max element union size
function tuplemerge(a::DataType, b::DataType)
    @assert a.name === b.name === Tuple.name "assertion failure"
    ap, bp = a.parameters, b.parameters
    lar = length(ap)::Int
    lbr = length(bp)::Int
    va = lar > 0 && isvarargtype(ap[lar])
    vb = lbr > 0 && isvarargtype(bp[lbr])
    if lar == lbr && !va && !vb
        lt = lar
        vt = false
    else
        lt = 0 # or min(lar - va, lbr - vb)
        vt = true
    end
    # combine the common elements
    p = Vector{Any}(undef, lt + vt)
    for i = 1:lt
        ui = Union{ap[i], bp[i]}
        if unioncomplexity(ui) < MAX_TYPEUNION_COMPLEXITY
            p[i] = ui
        else
            p[i] = Any
        end
    end
    # merge the remaining tail into a single, simple Tuple{Vararg{T}} (#22120)
    if vt
        tail = Union{}
        for loop_b = (false, true)
            for i = (lt + 1):(loop_b ? lbr : lar)
                ti = unwrapva(loop_b ? bp[i] : ap[i])
                while ti isa TypeVar
                    ti = ti.ub
                end
                # compare (ti <-> tail), (wrapper ti <-> tail), (ti <-> wrapper tail), then (wrapper ti <-> wrapper tail)
                # until we find the first element that contains the other in the pair
                # TODO: this result would be more stable (and more associative and more commutative)
                #   if we either joined all of the element wrappers first into a wide-tail, then picked between that or an exact tail,
                #   or (equivalently?) iteratively took super-types until reaching a common wrapper
                #   e.g. consider the results of `tuplemerge(Tuple{Complex}, Tuple{Number, Int})` and of
                #   `tuplemerge(Tuple{Int}, Tuple{String}, Tuple{Int, String})`
                if !(ti <: tail)
                    if tail <: ti
                        tail = ti # widen to ti
                    else
                        uw = unwrap_unionall(tail)
                        if uw isa DataType && tail <: uw.name.wrapper
                            # widen tail to wrapper(tail)
                            tail = uw.name.wrapper
                            if !(ti <: tail)
                                #assert !(tail <: ti)
                                uw = unwrap_unionall(ti)
                                if uw isa DataType && ti <: uw.name.wrapper
                                    # widen ti to wrapper(ti)
                                    ti = uw.name.wrapper
                                    #assert !(ti <: tail)
                                    if tail <: ti
                                        tail = ti
                                    else
                                        tail = Any # couldn't find common super-type
                                    end
                                else
                                    tail = Any # couldn't analyze type
                                end
                            end
                        else
                            tail = Any # couldn't analyze type
                        end
                    end
                end
                tail === Any && return Tuple # short-circuit loop
            end
        end
        @assert !(tail === Union{})
        p[lt + 1] = Vararg{tail}
    end
    return Tuple{p...}
end
