# This file is a part of Julia. License is MIT: https://julialang.org/license

#########################
# limitation parameters #
#########################

const MAX_TYPEUNION_COMPLEXITY = 3
const MAX_TYPEUNION_LENGTH = 3
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
    #@assert t <: r # this may fail if t contains a typevar in invariant and multiple times
        # in covariant position and r looses the occurrence in invariant position (see #36407)
    if !(t <: r) # ideally, this should never happen
        # widen to minimum complexity to obtain a valid result
        r = _limit_type_size(t, Any, source, 1, allowed_tuplelen)
        t <: r || (r = Any) # final escape hatch
    end
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
    isvarargtype(t) && (t = unwrapva(t))
    isvarargtype(c) && (c = unwrapva(c))
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
    @assert isa(t, Type) && isa(c, Type) "unhandled TypeVar / Vararg"
    if t === c
        return t # quick egal test
    elseif t === Bottom
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    else
        ut = unwrap_unionall(t)
        if isa(ut, DataType) && isa(c, Type) && c !== Bottom && c <: t
            # TODO: need to check that the UnionAll bounds on t are limited enough too
            return t # t is already wider than the comparison in the type lattice
        elseif is_derived_type_from_any(ut, sources, depth)
            return t # t isn't something new
        end
    end
    # peel off (and ignore) wrappers - they contribute no useful information, so we don't need to consider their size
    # first attempt to turn `c` into a type that contributes meaningful information
    # by peeling off meaningless non-matching wrappers of comparison one at a time
    # then unwrap `t`
    # NOTE that `TypeVar` / `Vararg` are handled separately to catch the logic errors
    if isa(c, UnionAll)
        return __limit_type_size(t, c.body, sources, depth, allowed_tuplelen)::Type
    end
    if isa(t, UnionAll)
        tbody = __limit_type_size(t.body, c, sources, depth, allowed_tuplelen)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)::Type
    elseif isa(t, Union)
        if isa(c, Union)
            a = __limit_type_size(t.a, c.a, sources, depth, allowed_tuplelen)
            b = __limit_type_size(t.b, c.b, sources, depth, allowed_tuplelen)
            return Union{a, b}
        end
    elseif isa(t, DataType)
        if isType(t) # see equivalent case in type_more_complex
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, Union) || isa(tt, TypeVar) || isType(tt)
                is_derived_type_from_any(tt, sources, depth + 1) && return t
            else
                isType(c) && (c = unwrap_unionall(c.parameters[1]))
                type_more_complex(tt, c, sources, depth, 0, 0) || return t
            end
            return Type
        elseif isa(c, DataType)
            tP = t.parameters
            cP = c.parameters
            if t.name === c.name && !isempty(cP)
                if t.name === Tuple.name
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
                        Q[i] = __limit_type_size(Q[i], cPi, sources, depth + 1, 0)
                    end
                    return Tuple{Q...}
                end
            end
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

# helper function of `_limit_type_size`, which has the right to take and return `TypeVar` / `Vararg`
function __limit_type_size(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, allowed_tuplelen::Int)
    cN = 0
    if isvarargtype(c) # Tuple{Vararg{T}} --> Tuple{T} is OK
        isdefined(c, :N) && (cN = c.N)
        c = unwrapva(c)
    end
    if isa(c, TypeVar)
        if isa(t, TypeVar) && t.ub === c.ub && (t.lb === Bottom || t.lb === c.lb)
            return t # it's ok to change the name, or widen `lb` to Bottom, so we can handle this immediately here
        end
        return __limit_type_size(t, c.ub, sources, depth, allowed_tuplelen)
    elseif isa(t, TypeVar)
        # don't have a matching TypeVar in comparison, so we keep just the upper bound
        return __limit_type_size(t.ub, c, sources, depth, allowed_tuplelen)
    elseif isvarargtype(t)
        # Tuple{Vararg{T,N}} --> Tuple{Vararg{S,M}} is OK
        # Tuple{T} --> Tuple{Vararg{T}} is OK
        # but S must be more limited than T, and must not introduce a new number for M
        VaT = __limit_type_size(unwrapva(t), c, sources, depth + 1, 0)
        if isdefined(t, :N)
            tN = t.N
            if isa(tN, TypeVar) || tN === cN
                return Vararg{VaT, tN}
            end
        end
        return Vararg{VaT}
    else
        return _limit_type_size(t, c, sources, depth, allowed_tuplelen)
    end
end

function type_more_complex(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, tupledepth::Int, allowed_tuplelen::Int)
    # detect cases where the comparison is trivial
    if t === c
        return false
    elseif t === Bottom
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif tupledepth > 0 && isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Bottom && c <: t
        # TODO: need to check that the UnionAll bounds on t are limited enough too
        return false # t is already wider than the comparison in the type lattice
    elseif tupledepth > 0 && is_derived_type_from_any(unwrap_unionall(t), sources, depth)
        return false # t isn't something new
    end
    # peel off wrappers
    isvarargtype(t) && (t = unwrapva(t))
    isvarargtype(c) && (c = unwrapva(c))
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
            return !(t.lb === Bottom || t.lb === c.lb) || # simplify lb towards Bottom
                   type_more_complex(t.ub, c.ub, sources, depth + 1, tupledepth, 0)
        end
        c.lb === Bottom || return true
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
        if isType(t)
            # Treat Type{T} and T as equivalent to allow taking typeof any
            # source type (DataType) anywhere as Type{...}, as long as it isn't
            # nesting as Type{Type{...}}
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, Union) || isa(tt, TypeVar) || isType(tt)
                return !is_derived_type_from_any(tt, sources, depth + 1)
            else
                isType(c) && (c = unwrap_unionall(c.parameters[1]))
                return type_more_complex(tt, c, sources, depth, 0, 0)
            end
        elseif isa(c, DataType) && t.name === c.name
            cP = c.parameters
            length(cP) < length(tP) && return true
            length(cP) > length(tP) && !isvarargtype(tP[end]) && depth == 1 && return false
            ntail = length(cP) - length(tP) # assume parameters were dropped from the tuple head
            # allow creating variation within a nested tuple, but only so deep
            if t.name === Tuple.name && tupledepth > 0
                tupledepth -= 1
            else
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
                            !isabstracttype(tPi) && !isabstracttype(cPi) &&
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
        end
    end
    return true
end

union_count_abstract(x::Union) = union_count_abstract(x.a) + union_count_abstract(x.b)
union_count_abstract(@nospecialize(x)) = !isdispatchelem(x)

function issimpleenoughtype(@nospecialize t)
    if isa(t, LatticeElement) # TODO (lattice overhaul) can handle this sort of code better?
        if isVararg(t)
            t = vararg(t)
        else
            t = unwraptype(ignorelimited(t))
        end
    end
    return unionlen(t) + union_count_abstract(t) <= MAX_TYPEUNION_LENGTH &&
           unioncomplexity(t) <= MAX_TYPEUNION_COMPLEXITY
end

"""
    a::LatticeElement ⊔ b::LatticeElement -> x::LatticeElement

A widening operator of the type inference lattice.
Since the type inference lattice has infinite height, `x` overapproximates the join of `a`
and `b` in order to ensure the convergence of inference, i.e., it picks a wider type that
contains both `a` and `b`, with some limits on how "large" it can get, but without losing
too much precision in common cases.
`⊔` also tries to be mostly asociative and commutative.
Note that this operation is often denoted as `∇` in the literature of abstract interpretation.
"""
a::LatticeElement ⊔ b::LatticeElement = begin
    a === ⊥ && return b
    b === ⊥ && return a

    # # COMBAK (lattice overhaul) moved to typemerge
    # still should enable a similar fast pass here as well?
    # asub = a ⊑ b
    # asub && issimpleenoughtype(b) && return b
    # bsub = b ⊑ a
    # asub && bsub && return a
    # bsub && issimpleenoughtype(a) && return a

    # merge Const and PartialStruct properties
    mconstant = __NULL_CONSTANT__
    aconstant = a.constant
    if aconstant !== __NULL_CONSTANT__
        bconstant = b.constant
        if aconstant === bconstant
            mconstant = aconstant
        elseif bconstant !== __NULL_CONSTANT__
            aty = widenconst(a)
            bty = widenconst(b)
            if aty === bty
                a_nfields = nfields_tfunc(a)
                b_nfields = nfields_tfunc(b)
                if isConst(a_nfields) && isConst(b_nfields)
                    type_nfields = constant(a_nfields)::Int
                    if type_nfields == constant(b_nfields)::Int
                        if type_nfields ≠ 0
                            fields = Vector{LatticeElement}(undef, type_nfields)
                            anyconst = false
                            for i = 1:type_nfields
                                ai = getfield_tfunc(a, Const(i))
                                bi = getfield_tfunc(b, Const(i))
                                ity = ai ⊔ bi
                                if ai === ⊥ || bi === ⊥
                                    ity = NativeType(widenconst(ity))
                                end
                                fields[i] = ity
                                anyconst |= has_nontrivial_const_info(ity)
                            end
                            if anyconst
                                mconstant = fields
                            end
                        end
                    end
                end
            end
        end
    end

    # merge special properties
    # TODO implement specialmerge for `PartialTypeVar`?
    aspecial = a.special
    bspecial = b.special
    if isa(aspecial, ConditionalInfo)
        if isa(bspecial, ConditionalInfo)
            mspecial = specialmerge(a, b, aspecial, bspecial)
        else
            mspecial = specialmerge(a, b, aspecial, Special(bspecial))
        end
    elseif isa(bspecial, ConditionalInfo)
        mspecial = specialmerge(a, b, Special(aspecial), bspecial)
    elseif isa(aspecial, PartialOpaque) && isa(bspecial, PartialOpaque)
        mspecial = specialmerge(a, b, aspecial, bspecial)
    elseif aspecial === nothing || bspecial === nothing
        mspecial = nothing
    else
        mspecial = specialmerge(a, b, aspecial, bspecial) # customization
    end

    # merge LimitedAccuracy properties
    # the merge create a slightly narrower property than needed, but we can't
    # represent the precise intersection of causes and don't attempt to
    # enumerate some of these cases where we could
    acauses = a.causes
    bcauses = b.causes
    if acauses !== nothing
        acauses = acauses::IdSet{InferenceState}
        if bcauses !== nothing
            mcauses = union!(copy(acauses), bcauses::IdSet{InferenceState})
        else
            mcauses = acauses
        end
    elseif bcauses !== nothing
        mcauses = bcauses::IdSet{InferenceState}
    else
        mcauses = nothing
    end

    # merge MaybeUndef properties
    mmaybeundef = a.maybeundef | b.maybeundef

    # finally join the types
    mtyp = typemerge(widenconst(a), widenconst(b))

    return LatticeElement(mtyp, mconstant, mspecial;
        causes = mcauses, maybeundef = mmaybeundef)
end

struct Special
    val
    Special(@nospecialize val) = new(val)
end

@inline function specialmerge(_::LatticeElement, _::LatticeElement,
    acond::ConditionalInfo, bcond::ConditionalInfo)
    ainter = acond.inter
    binter = bcond.inter
    @assert ainter === binter "invalid ConditionalInfo merge"
    if is_same_conditionals(acond, bcond)
        vtype = acond.vtype ⊔ bcond.vtype
        elsetype = acond.elsetype ⊔ bcond.elsetype
        if !is_lattice_equal(vtype, elsetype)
            return ConditionalInfo(acond.slot_id, vtype, elsetype, ainter)
        end
    end
    return nothing
end

@inline function specialmerge(a::LatticeElement, b::LatticeElement,
    acond::ConditionalInfo, bspecial::Special)
    bval = bspecial.val
    bval === nothing || return nothing # conflict of special property
    bconstant = b.constant
    if bconstant === true
        return specialmerge(a, b, acond, ConditionalInfo(acond.slot_id, ⊤, ⊥, acond.inter))
    elseif bconstant === false
        return specialmerge(a, b, acond, ConditionalInfo(acond.slot_id, ⊥, ⊤, acond.inter))
    end
    return nothing
end

@inline specialmerge(a::LatticeElement, b::LatticeElement,
    aspecial::Special, bcond::ConditionalInfo) = specialmerge(b, a, bcond, aspecial)

@inline function specialmerge(_::LatticeElement, _::LatticeElement,
    a::PartialOpaque, b::PartialOpaque)
    atyp = a.typ
    btyp = b.typ
    if atyp === btyp
        if (a.source === b.source &&
            a.isva === b.isva &&
            a.parent === b.parent)
            env = a.env::LatticeElement ⊔ b.env::LatticeElement
            return PartialOpaque(atyp, env, a.isva, a.parent, a.source)
        end
    end
    return nothing
end

@noinline specialmerge(_::LatticeElement, _::LatticeElement,
    @nospecialize(::Any), @nospecialize(::Any)) = nothing

@inline function typemerge(@nospecialize(typea::Type), @nospecialize(typeb::Type))
    typea === Bottom && return typeb
    typeb === Bottom && return typea
    asub = typea <: typeb
    asub && issimpleenoughtype(typeb) && return typeb
    bsub = typeb <: typea
    asub && bsub && return typea
    bsub && issimpleenoughtype(typea) && return typea

    typea == typeb && return typea
    # it's always ok to form a Union of two concrete types
    if (isconcretetype(typea) || isType(typea)) && (isconcretetype(typeb) || isType(typeb))
        return Union{typea, typeb}
    end
    # collect the list of types from past typemerge calls returning Union
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
        uw isa DataType || return Any
        ti <: uw.name.wrapper || return Any
        typenames[i] = uw.name
    end
    u = Union{types...}
    if issimpleenoughtype(u)
        return u
    end
    # see if any of the union elements have the same TypeName
    # in which case, simplify this typemerge by replacing it with
    # the widest possible version of itself (the wrapper)
    for i in 1:length(types)
        ti = types[i]
        for j in (i + 1):length(types)
            if typenames[i] === typenames[j]
                tj = types[j]
                if ti <: tj
                    types[i] = Bottom
                    typenames[i] = Any.name
                    break
                elseif tj <: ti
                    types[j] = Bottom
                    typenames[j] = Any.name
                else
                    if typenames[i] === Tuple.name
                        # try to widen Tuple slower: make a single non-concrete Tuple containing both
                        # converge the Tuple element-wise if they are the same length
                        # see 4ee2b41552a6bc95465c12ca66146d69b354317b, be59686f7613a2ccfd63491c7b354d0b16a95c05,
                        widen = tuplemerge(unwrap_unionall(ti)::DataType, unwrap_unionall(tj)::DataType)
                        widen = rewrap_unionall(rewrap_unionall(widen, ti), tj)
                    else
                        wr = typenames[i].wrapper
                        uw = unwrap_unionall(wr)::DataType
                        ui = unwrap_unionall(ti)::DataType
                        uj = unwrap_unionall(tj)::DataType
                        merged = wr
                        for k = 1:length(uw.parameters)
                            ui_k = ui.parameters[k]
                            if ui_k === uj.parameters[k] && !has_free_typevars(ui_k)
                                merged = merged{ui_k}
                            else
                                merged = merged{uw.parameters[k]}
                            end
                        end
                        widen = rewrap_unionall(merged, wr)
                    end
                    types[i] = Bottom
                    typenames[i] = Any.name
                    types[j] = widen
                    break
                end
            end
        end
    end
    u = Union{types...}
    # don't let type unions get too big, if the above didn't reduce it enough
    if issimpleenoughtype(u)
        return u
    end
    # don't let the slow widening of Tuple cause the whole type to grow too fast
    for i in 1:length(types)
        if typenames[i] === Tuple.name
            widen = unwrap_unionall(types[i])
            if isa(widen, DataType) && !isvatuple(widen)
                widen = NTuple{length(widen.parameters), Any}
            else
                widen = Tuple
            end
            types[i] = widen
            u = Union{types...}
            if issimpleenoughtype(u)
                return u
            end
            break
        end
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
        p[i] = issimpleenoughtype(ui) ? ui : Any
    end
    # merge the remaining tail into a single, simple Tuple{Vararg{T}} (#22120)
    if vt
        tail = Bottom
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
        @assert !(tail === Bottom)
        p[lt + 1] = Vararg{tail}
    end
    return Tuple{p...}
end

# compute typeintersect over the extended inference lattice
# where v is in the extended lattice, and t is a Type
"""
    v::LatticeElement ⊓ t::Type -> x::LatticeElement

`⊓` computes `typeintersect` over the type inference lattice.
Note that this operation is not the valid "meet" operation,
since `v` is in the extended lattice while `t` needs to be a `Type`.
"""
v::LatticeElement ⊓ @nospecialize(t) = begin
    if isConditional(v)
        if !(Bool <: t)
            return ⊥
        end
        return v
    elseif isConst(v)
        if !has_free_typevars(t) && !isa(constant(v), t)
            return ⊥
        end
        return v
    elseif isPartialStruct(v)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        if widev <: t
            return v
        end
        ti = typeintersect(widev, t)
        valid_as_lattice(ti) || return ⊥
        @assert widev <: Tuple
        vfields = partialfields(v)
        nfields = length(vfields)
        new_fields = Vector{LatticeElement}(undef, nfields)
        for i = 1:nfields
            vfi = vfields[i]
            if isVararg(vfi)
                new_fields[i] = vfi
            else
                nf = vfi ⊓ widenconst(getfield_tfunc(NativeType(t), Const(i)))
                nf === ⊥ && return ⊥
                new_fields[i] = nf
            end
        end
        return tuple_tfunc(new_fields)
    end
    ti = typeintersect(widenconst(v), t)
    valid_as_lattice(ti) || return ⊥
    return NativeType(ti)
end
