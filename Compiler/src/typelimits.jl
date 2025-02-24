# This file is a part of Julia. License is MIT: https://julialang.org/license

#########################
# limitation parameters #
#########################

const MAX_TYPEUNION_COMPLEXITY = 3
const MAX_TYPEUNION_LENGTH = 3

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
    if has_free_typevars(t) || has_free_typevars(c)
        # Don't allow finding types with free typevars. These strongly depend
        # on identity and we do not make any effort to make sure this returns
        # sensible results in that case.
        return false
    end
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
    elseif t === Union{}
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    else
        ut = unwrap_unionall(t)
        if is_derived_type_from_any(ut, sources, depth)
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
        if isType(t)
            # Type is fairly important, so do not widen it as fast as other types if avoidable
            tt = t.parameters[1]
            ttu = unwrap_unionall(tt) # TODO: use argument_datatype(tt) after #50692 fixed
            # must forbid nesting through this if we detect that potentially occurring
            # we already know !is_derived_type_from_any so refuse to recurse here
            if !isa(ttu, DataType)
                return Type
            elseif isType(ttu)
                return Type{<:Type}
            end
            # try to peek into c to get a comparison object, but if we can't perhaps t is already simple enough on its own
            if isType(c)
                ct = c.parameters[1]
            else
                ct = Union{}
            end
            Qt = __limit_type_size(tt, ct, sources, depth + 1, 0)
            Qt === tt && return t
            Qt === Any && return Type
            # Can't form Type{<:Qt} just yet, without first make sure we limited the depth
            # enough, since this moves Qt outside of Type for is_derived_type_from_any
            Qt = __limit_type_size(tt, ct, sources, depth + 2, 0)
            Qt === Any && return Type
            return Type{<:Qt}
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
                        Q[np] = tuple_tail_elem(fallback_lattice, Bottom, Any[ tP[i] for i in np:ltP ])
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
            # forbid nesting Tuple{Tuple{Tuple...}} through this
            return Any
        end
        widert = t.name.wrapper
        if !(t <: widert) # XXX: we should call has_free_typevars(t) here, but usually t does not have those wrappers by the time it got here
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
        if isa(t, TypeVar) && t.ub === c.ub && (t.lb === Union{} || t.lb === c.lb)
            return t # it's ok to change the name, or widen `lb` to Union{}, so we can handle this immediately here
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
    elseif t === Union{}
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif is_derived_type_from_any(unwrap_unionall(t), sources, depth)
        return false # t isn't something new
    end
    # peel off wrappers
    isvarargtype(t) && (t = unwrapva(t))
    isvarargtype(c) && (c = unwrapva(c))
    if isa(c, UnionAll)
        # allow wrapping type with fewer UnionAlls than comparison only if in a covariant context
        if !isa(t, UnionAll) && tupledepth == 0
            return true
        end
        c = unwrap_unionall(c)
    end
    if isa(t, UnionAll)
        t = unwrap_unionall(t)
    end
    # rules for various comparison types
    if isa(c, TypeVar)
        tupledepth = 1
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
        if isType(t)
            # Type is fairly important, so do not widen it as fast as other types if avoidable
            tt = tP[1]
            ttu = unwrap_unionall(tt) # TODO: use argument_datatype(tt) after #50692 fixed
            if isType(c)
                ct = c.parameters[1]
            else
                ct = Union{}
                tupledepth == 0 && return true # cannot allow nesting
            end
            # allow creating variation within a nested Type, but not very deep
            if tupledepth > 1
                tupledepth = 1
            else
                tupledepth = 0
            end
            return type_more_complex(tt, ct, sources, depth + 1, tupledepth, 0)
        elseif isa(c, DataType) && t.name === c.name
            cP = c.parameters
            length(cP) < length(tP) && return true
            isempty(tP) && return false
            length(cP) > length(tP) && !isvarargtype(tP[end]) && depth == 1 && return false # is this line necessary?
            ntail = length(cP) - length(tP) # assume parameters were dropped from the tuple head
            # allow creating variation within a nested tuple, but only so deep
            if t.name === Tuple.name && tupledepth > 0
                tupledepth -= 1
            else
                tupledepth = 0
            end
            for i = 1:length(tP)
                tPi = tP[i]
                cPi = cP[i + ntail]
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
    ut = unwrap_unionall(t)
    ut isa DataType && ut.name.wrapper == t && return true
    return max(unionlen(t), union_count_abstract(t) + 1) <= MAX_TYPEUNION_LENGTH &&
           unioncomplexity(t) <= MAX_TYPEUNION_COMPLEXITY
end

# We may want to apply a stricter limit than issimpleenoughtype to
# tupleelements individually, to try to keep the whole tuple under the limit,
# even after complicated recursion and other operations on it elsewhere
const issimpleenoughtupleelem = issimpleenoughtype

function n_initialized(t::Const)
    nf = nfields(t.val)
    return something(findfirst(i::Int->!isdefined(t.val,i), 1:nf), nf+1)-1
end

is_field_maybe_undef(t::Const, i) = !isdefined(t.val, i)

function n_initialized(pstruct::PartialStruct)
    i = findfirst(pstruct.undef)
    nmin = datatype_min_ninitialized(pstruct.typ)
    i === nothing && return max(length(pstruct.undef), nmin)
    n = i::Int - 1
    @assert n ≥ nmin
    n
end

function is_field_maybe_undef(pstruct::PartialStruct, fi)
    fi ≥ 1 || return true
    fi ≤ length(pstruct.undef) && return pstruct.undef[fi]
    fi > datatype_min_ninitialized(pstruct.typ)
end

function partialstruct_getfield(pstruct::PartialStruct, fi::Integer)
    @assert fi > 0
    fi ≤ length(pstruct.fields) && return pstruct.fields[fi]
    fieldtype(pstruct.typ, fi)
end

function refines_definedness_information(pstruct::PartialStruct)
    nflds = length(pstruct.undef)
    something(findfirst(pstruct.undef), nflds + 1) - 1 > datatype_min_ninitialized(pstruct.typ)
end

function define_field(pstruct::PartialStruct, fi::Int)
    if !is_field_maybe_undef(pstruct, fi)
        # no new information to be gained
        return nothing
    end

    new = expand_partialstruct(pstruct, fi)
    if new === nothing
        new = PartialStruct(fallback_lattice, pstruct.typ, copy(pstruct.undef), copy(pstruct.fields))
    end
    new.undef[fi] = false
    return new
end

function expand_partialstruct(pstruct::PartialStruct, until::Int)
    n = length(pstruct.undef)
    until ≤ n && return nothing

    undef = partialstruct_init_undef(pstruct.typ, until; all_defined = false)
    for i in 1:n
        undef[i] &= pstruct.undef[i]
    end
    nf = length(pstruct.fields)
    typ = pstruct.typ
    fields = Any[i ≤ nf ? pstruct.fields[i] : fieldtype(typ, i) for i in 1:until]
    return PartialStruct(fallback_lattice, typ, undef, fields)
end

# A simplified type_more_complex query over the extended lattice
# (assumes typeb ⊑ typea)
@nospecializeinfer function issimplertype(𝕃::AbstractLattice, @nospecialize(typea), @nospecialize(typeb))
    @assert !isa(typea, LimitedAccuracy) && !isa(typeb, LimitedAccuracy) "LimitedAccuracy not supported by simplertype lattice" # n.b. the caller was supposed to handle these
    typea === typeb && return true
    if typea isa PartialStruct
        aty = widenconst(typea)
        if typeb isa Const || typeb isa PartialStruct
            @assert n_initialized(typea) ≤ n_initialized(typeb) "typeb ⊑ typea is assumed"
        elseif typeb isa PartialStruct
            @assert n_initialized(typea) ≤ n_initialized(typeb) &&
                all(b < a for (a, b) in zip(typea.undef, typeb.undef)) "typeb ⊑ typea is assumed"
        else
            return false
        end
        for i = 1:length(typea.fields)
            ai = unwrapva(typea.fields[i])
            bi = fieldtype(aty, i)
            is_lattice_equal(𝕃, ai, bi) && continue
            tni = _typename(widenconst(ai))
            if tni isa Const
                bi = (tni.val::Core.TypeName).wrapper
                is_lattice_equal(𝕃, ai, bi) && continue
            end
            bi = getfield_tfunc(𝕃, typeb, Const(i))
            is_lattice_equal(𝕃, ai, bi) && continue
            # It is not enough for ai to be simpler than bi: it must exactly equal
            # (for this, an invariant struct field, by contrast to
            # type_more_complex above which handles covariant tuples).
            return false
        end
    elseif typea isa Type
        return issimpleenoughtype(typea)
    # elseif typea isa Const # fall-through to true is good
    elseif typea isa Conditional # follow issubconditional query
        typeb isa Const && return true
        typeb isa Conditional || return false
        is_same_conditionals(typea, typeb) || return false
        issimplertype(𝕃, typea.thentype, typeb.thentype) || return false
        issimplertype(𝕃, typea.elsetype, typeb.elsetype) || return false
    elseif typea isa InterConditional # ibid
        typeb isa Const && return true
        typeb isa InterConditional || return false
        is_same_conditionals(typea, typeb) || return false
        issimplertype(𝕃, typea.thentype, typeb.thentype) || return false
        issimplertype(𝕃, typea.elsetype, typeb.elsetype) || return false
    elseif typea isa MustAlias
        typeb isa MustAlias || return false
        issubalias(typeb, typea) || return false
        issimplertype(𝕃, typea.vartyp, typeb.vartyp) || return false
        issimplertype(𝕃, typea.fldtyp, typeb.fldtyp) || return false
    elseif typea isa InterMustAlias
        typeb isa InterMustAlias || return false
        issubalias(typeb, typea) || return false
        issimplertype(𝕃, typea.vartyp, typeb.vartyp) || return false
        issimplertype(𝕃, typea.fldtyp, typeb.fldtyp) || return false
    elseif typea isa PartialOpaque
        # TODO
        typeb isa PartialOpaque || return false
        aty = widenconst(typea)
        bty = widenconst(typeb)
        if typea.source === typeb.source && typea.parent === typeb.parent && aty == bty && typea.env == typeb.env
            return false
        end
        return false
    end
    return true
end

@inline function tmerge_fast_path(lattice::AbstractLattice, @nospecialize(typea), @nospecialize(typeb))
    # Fast paths
    typea === Union{} && return typeb
    typeb === Union{} && return typea
    typea === typeb && return typea

    suba = ⊑(lattice, typea, typeb)
    suba && issimplertype(lattice, typeb, typea) && return typeb
    subb = ⊑(lattice, typeb, typea)
    suba && subb && return typea
    subb && issimplertype(lattice, typea, typeb) && return typea
    return nothing
end

function union_causes(causesa::IdSet{InferenceState}, causesb::IdSet{InferenceState})
    if causesa ⊆ causesb
        return causesb
    elseif causesb ⊆ causesa
        return causesa
    else
        return union!(copy(causesa), causesb)
    end
end

function merge_causes(causesa::IdSet{InferenceState}, causesb::IdSet{InferenceState})
    # TODO: When lattice elements are equal, we're allowed to discard one or the
    # other set, but we'll need to come up with a consistent rule. For now, we
    # just check the length, but other heuristics may be applicable.
    if length(causesa) < length(causesb)
        return causesa
    elseif length(causesb) < length(causesa)
        return causesb
    else
        return union!(copy(causesa), causesb)
    end
end

@nospecializeinfer @noinline function tmerge_limited(lattice::InferenceLattice, @nospecialize(typea), @nospecialize(typeb))
    typea === Union{} && return typeb
    typeb === Union{} && return typea

    # Like tmerge_fast_path, but tracking which causes need to be preserved at
    # the same time.
    if isa(typea, LimitedAccuracy) && isa(typeb, LimitedAccuracy)
        causesa = typea.causes
        causesb = typeb.causes
        typea = typea.typ
        typeb = typeb.typ
        suba = ⊑(lattice, typea, typeb)
        subb = ⊑(lattice, typeb, typea)

        # Approximated types are lattice equal. Merge causes.
        if suba && subb
            return LimitedAccuracy(typeb, merge_causes(causesa, causesb))
        elseif suba
            issimplertype(lattice, typeb, typea) && return LimitedAccuracy(typeb, causesb)
            causes = causesb
            # `a`'s causes may be discarded
        elseif subb
            causes = causesa
        else
            causes = union_causes(causesa, causesb)
        end
    else
        if isa(typeb, LimitedAccuracy)
            (typea, typeb) = (typeb, typea)
        end
        typea = typea::LimitedAccuracy

        causes = typea.causes
        typea = typea.typ

        suba = ⊑(lattice, typea, typeb)
        if suba
            issimplertype(lattice, typeb, typea) && return typeb
            # `typea` was narrower than `typeb`. Whatever tmerge produces,
            # we know it must be wider than `typeb`, so we may drop the
            # causes.
            causes = nothing
        end
        subb = ⊑(lattice, typeb, typea)
    end

    suba && subb && return LimitedAccuracy(typea, causes)
    subb && issimplertype(lattice, typea, typeb) && return LimitedAccuracy(typea, causes)
    return LimitedAccuracy(tmerge(widenlattice(lattice), typea, typeb), causes)
end

@nospecializeinfer function tmerge(lattice::InferenceLattice, @nospecialize(typea), @nospecialize(typeb))
    if isa(typea, LimitedAccuracy) || isa(typeb, LimitedAccuracy)
        return tmerge_limited(lattice, typea, typeb)
    end

    return tmerge(widenlattice(lattice), typea, typeb)
end

@nospecializeinfer function tmerge(lattice::ConditionalsLattice, @nospecialize(typea), @nospecialize(typeb))
    # type-lattice for Conditional wrapper (NOTE never be merged with InterConditional)
    if isa(typea, Conditional) && isa(typeb, Const)
        if typeb.val === true
            typeb = Conditional(typea.slot, Any, Union{})
        elseif typeb.val === false
            typeb = Conditional(typea.slot, Union{}, Any)
        end
    end
    if isa(typeb, Conditional) && isa(typea, Const)
        if typea.val === true
            typea = Conditional(typeb.slot, Any, Union{})
        elseif typea.val === false
            typea = Conditional(typeb.slot, Union{}, Any)
        end
    end
    if isa(typea, Conditional) && isa(typeb, Conditional)
        if is_same_conditionals(typea, typeb)
            thentype = tmerge(widenlattice(lattice), typea.thentype, typeb.thentype)
            elsetype = tmerge(widenlattice(lattice), typea.elsetype, typeb.elsetype)
            if thentype !== elsetype
                return Conditional(typea.slot, thentype, elsetype)
            end
        end
        val = maybe_extract_const_bool(typea)
        if val isa Bool && val === maybe_extract_const_bool(typeb)
            return Const(val)
        end
        return Bool
    end
    if isa(typea, Conditional)
        typeb === Union{} && return typea
        typea = widenconditional(typea)
    elseif isa(typeb, Conditional)
        typea === Union{} && return typeb
        typeb = widenconditional(typeb)
    end
    return tmerge(widenlattice(lattice), typea, typeb)
end

@nospecializeinfer function tmerge(lattice::InterConditionalsLattice, @nospecialize(typea), @nospecialize(typeb))
    # type-lattice for InterConditional wrapper (NOTE never be merged with Conditional)
    if isa(typea, InterConditional) && isa(typeb, Const)
        if typeb.val === true
            typeb = InterConditional(typea.slot, Any, Union{})
        elseif typeb.val === false
            typeb = InterConditional(typea.slot, Union{}, Any)
        end
    end
    if isa(typeb, InterConditional) && isa(typea, Const)
        if typea.val === true
            typea = InterConditional(typeb.slot, Any, Union{})
        elseif typea.val === false
            typea = InterConditional(typeb.slot, Union{}, Any)
        end
    end
    if isa(typea, InterConditional) && isa(typeb, InterConditional)
        if is_same_conditionals(typea, typeb)
            thentype = tmerge(widenlattice(lattice), typea.thentype, typeb.thentype)
            elsetype = tmerge(widenlattice(lattice), typea.elsetype, typeb.elsetype)
            if thentype !== elsetype
                return InterConditional(typea.slot, thentype, elsetype)
            end
        end
        val = maybe_extract_const_bool(typea)
        if val isa Bool && val === maybe_extract_const_bool(typeb)
            return Const(val)
        end
        return Bool
    end
    if isa(typea, InterConditional)
        typeb === Union{} && return typea
        typea = widenconditional(typea)
    elseif isa(typeb, InterConditional)
        typea === Union{} && return typeb
        typeb = widenconditional(typeb)
    end
    return tmerge(widenlattice(lattice), typea, typeb)
end

@nospecializeinfer function tmerge(𝕃::AnyMustAliasesLattice, @nospecialize(typea), @nospecialize(typeb))
    if is_valid_lattice_norec(𝕃, typea)
        typeb === Union{} && return typea
        typea = widenmustalias(typea)
    end
    if is_valid_lattice_norec(𝕃, typeb)
        typea === Union{} && return typeb
        typeb = widenmustalias(typeb)
    end
    return tmerge(widenlattice(𝕃), typea, typeb)
end

# N.B. This can also be called with both typea::Const and typeb::Const to
# to recover PartialStruct from `Const`s with overlapping fields.
@nospecializeinfer function tmerge_partial_struct(𝕃::PartialsLattice, @nospecialize(typea), @nospecialize(typeb))
    aty = widenconst(typea)
    bty = widenconst(typeb)
    if aty === bty && !isType(aty)
        if typea isa PartialStruct
            if typeb isa PartialStruct
                nflds = min(length(typea.fields), length(typeb.fields))
                nundef = nflds - (isvarargtype(typea.fields[end]) && isvarargtype(typeb.fields[end]))
            else
                nflds = min(length(typea.fields), n_initialized(typeb::Const))
                nundef = nflds
            end
        elseif typeb isa PartialStruct
            nflds = min(n_initialized(typea::Const), length(typeb.fields))
            nundef = nflds
        else
            nflds = min(n_initialized(typea::Const), n_initialized(typeb::Const))
            nundef = nflds
        end
        nflds == 0 && return nothing
        _undef = partialstruct_init_undef(aty, nundef; all_defined = false)
        fields = Vector{Any}(undef, nflds)
        fldmin = datatype_min_ninitialized(aty)
        n_initialized_merged = min(n_initialized(typea::Union{Const, PartialStruct}), n_initialized(typeb::Union{Const, PartialStruct}))
        anyrefine = n_initialized_merged > fldmin
        for i = 1:nflds
            ai = getfield_tfunc(𝕃, typea, Const(i))
            bi = getfield_tfunc(𝕃, typeb, Const(i))
            ft = fieldtype(aty, i)
            if is_lattice_equal(𝕃, ai, bi) || is_lattice_equal(𝕃, ai, ft)
                # Since ai===bi, the given type has no restrictions on complexity.
                # and can be used to refine ft
                tyi = ai
            elseif is_lattice_equal(𝕃, bi, ft)
                tyi = bi
            elseif (tyi′ = tmerge_field(𝕃, ai, bi); tyi′ !== nothing)
                # allow external lattice implementation to provide a custom field-merge strategy
                tyi = tyi′
            else
                # Otherwise use the default aggressive field-merge implementation, and
                # choose between using the fieldtype or some other simple merged type.
                # The wrapper type never has restrictions on complexity,
                # so try to use that to refine the estimated type too.
                tni = _typename(widenconst(ai))
                if tni isa Const && tni === _typename(widenconst(bi))
                    # A tmeet call may cause tyi to become complex, but since the inputs were
                    # strictly limited to being egal, this has no restrictions on complexity.
                    # (Otherwise, we would need to use <: and take the narrower one without
                    # intersection. See the similar comment in abstract_call_method.)
                    tyi = typeintersect(ft, (tni.val::Core.TypeName).wrapper)
                else
                    # Since aty===bty, the fieldtype has no restrictions on complexity.
                    tyi = ft
                end
            end
            fields[i] = tyi
            if i ≤ nundef
                _undef[i] = is_field_maybe_undef(typea, i) || is_field_maybe_undef(typeb, i)
            end
            if !anyrefine
                anyrefine = has_nontrivial_extended_info(𝕃, tyi) || # extended information
                            ⋤(𝕃, tyi, ft) || # just a type-level information, but more precise than the declared type
                            !get(_undef, i, true) && i > fldmin # possibly uninitialized field is known to be initialized
            end
        end
        anyrefine && return PartialStruct(𝕃, aty, _undef, fields)
    end
    return nothing
end

@nospecializeinfer function tmerge(lattice::PartialsLattice, @nospecialize(typea), @nospecialize(typeb))
    r = tmerge_fast_path(lattice, typea, typeb)
    r !== nothing && return r

    # type-lattice for Const and PartialStruct wrappers
    aps = isa(typea, PartialStruct)
    bps = isa(typeb, PartialStruct)
    acp = aps || isa(typea, Const)
    bcp = bps || isa(typeb, Const)
    if acp && bcp
        typea === typeb && return typea
        psrt = tmerge_partial_struct(lattice, typea, typeb)
        psrt !== nothing && return psrt
    end

    # Don't widen const here - external AbstractInterpreter might insert lattice
    # layers between us and `ConstsLattice`.
    wl = widenlattice(lattice)
    aps && (typea = widenlattice(wl, typea))
    bps && (typeb = widenlattice(wl, typeb))

    # type-lattice for PartialOpaque wrapper
    apo = isa(typea, PartialOpaque)
    bpo = isa(typeb, PartialOpaque)
    if apo && bpo
        aty = widenconst(typea)
        bty = widenconst(typeb)
        if aty == bty
            if !(typea.source === typeb.source &&
                typea.parent === typeb.parent)
                return widenconst(typea)
            end
            return PartialOpaque(typea.typ, tmerge(lattice, typea.env, typeb.env),
                typea.parent, typea.source)
        end
        typea = aty
        typeb = bty
    elseif apo
        typea = widenlattice(wl, typea)
    elseif bpo
        typeb = widenlattice(wl, typeb)
    end

    return tmerge(wl, typea, typeb)
end

@nospecializeinfer function tmerge(lattice::ConstsLattice, @nospecialize(typea), @nospecialize(typeb))
    acp = isa(typea, Const) || isa(typea, PartialTypeVar)
    bcp = isa(typeb, Const) || isa(typeb, PartialTypeVar)
    if acp && bcp
        typea === typeb && return typea
    end
    wl = widenlattice(lattice)
    acp && (typea = widenlattice(wl, typea))
    bcp && (typeb = widenlattice(wl, typeb))
    return tmerge(wl, typea, typeb)
end

@nospecializeinfer function tmerge(lattice::JLTypeLattice, @nospecialize(typea::Type), @nospecialize(typeb::Type))
    # it's always ok to form a Union of two concrete types
    act = isconcretetype(typea)
    bct = isconcretetype(typeb)
    if act && bct
        # Extra fast path for pointer-egal concrete types
        (pointer_from_objref(typea) === pointer_from_objref(typeb)) && return typea
    end
    if (act || isType(typea)) && (bct || isType(typeb))
        return Union{typea, typeb}
    end
    u = tmerge_fast_path(lattice, typea, typeb)
    u === nothing || return u
    return tmerge_types_slow(typea, typeb)
end

@nospecializeinfer @noinline function tname_intersect(aname::Core.TypeName, bname::Core.TypeName)
    aname === bname && return aname
    if !isabstracttype(aname.wrapper) && !isabstracttype(bname.wrapper)
        return nothing # fast path
    end
    Any.name === aname && return aname
    a = unwrap_unionall(aname.wrapper)
    heighta = 0
    while a !== Any
        heighta += 1
        a = a.super
    end
    b = unwrap_unionall(bname.wrapper)
    heightb = 0
    while b !== Any
        b.name === aname && return aname
        heightb += 1
        b = b.super
    end
    a = unwrap_unionall(aname.wrapper)
    while heighta > heightb
        a = a.super
        heighta -= 1
    end
    return a.name === bname ? bname : nothing
end

@nospecializeinfer @noinline function tmerge_types_slow(@nospecialize(typea::Type), @nospecialize(typeb::Type))
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
        uw isa DataType || return Any
        ti <: uw.name.wrapper || return Any
        typenames[i] = uw.name
    end
    u = Union{types...}
    if issimpleenoughtype(u)
        return u
    end
    # see if any of the union elements have the same TypeName
    # in which case, simplify this tmerge by replacing it with
    # the widest possible version of itself (the wrapper)
    simplify = falses(length(types))
    for i in 1:length(types)
        typenames[i] === Any.name && continue
        ti = types[i]
        for j in (i + 1):length(types)
            typenames[j] === Any.name && continue
            ijname = tname_intersect(typenames[i], typenames[j])
            if !(ijname === nothing)
                tj = types[j]
                if ti <: tj
                    types[i] = Union{}
                    typenames[i] = Any.name
                    simplify[i] = false
                    simplify[j] = true
                    break
                elseif tj <: ti
                    types[j] = Union{}
                    typenames[j] = Any.name
                    simplify[j] = false
                    simplify[i] = true
                else
                    if ijname === Tuple.name
                        # try to widen Tuple slower: make a single non-concrete Tuple containing both
                        # converge the Tuple element-wise if they are the same length
                        # see 4ee2b41552a6bc95465c12ca66146d69b354317b, be59686f7613a2ccfd63491c7b354d0b16a95c05,
                        widen = tuplemerge(unwrap_unionall(ti)::DataType, unwrap_unionall(tj)::DataType)
                        widen = rewrap_unionall(rewrap_unionall(widen, ti), tj)
                        simplify[j] = false
                    else
                        wr = ijname.wrapper
                        uw = unwrap_unionall(wr)::DataType
                        ui = unwrap_unionall(ti)::DataType
                        while ui.name !== ijname
                            ui = ui.super
                        end
                        uj = unwrap_unionall(tj)::DataType
                        while uj.name !== ijname
                            uj = uj.super
                        end
                        p = Vector{Any}(undef, length(uw.parameters))
                        usep = true
                        widen = wr
                        for k = 1:length(uw.parameters)
                            ui_k = ui.parameters[k]
                            if ui_k === uj.parameters[k] && !has_free_typevars(ui_k)
                                p[k] = ui_k
                                usep = true
                            else
                                p[k] = uw.parameters[k]
                            end
                        end
                        if usep
                            widen = rewrap_unionall(wr{p...}, wr)
                            widen <: wr || (widen = wr) # sometimes there are cross-constraints on wr that we may lose in this process, but that would cause future calls to this to need to return Any, which is undesirable
                        end
                        simplify[j] = !usep
                    end
                    types[i] = Union{}
                    typenames[i] = Any.name
                    simplify[i] = false
                    types[j] = widen
                    typenames[j] = ijname
                    break
                end
            end
        end
    end
    # don't let elements of the union get too big, if the above didn't reduce something enough
    # Specifically widen Tuple{..., Union{lots of stuff}...} to Tuple{..., Any, ...}
    # Don't let Val{<:Val{<:Val}} keep nesting abstract levels either
    for i in 1:length(types)
        simplify[i] || continue
        ti = types[i]
        issimpleenoughtype(ti) && continue
        if typenames[i] === Tuple.name
            # otherwise we need to do a simple version of tuplemerge for one element now
            tip = (unwrap_unionall(ti)::DataType).parameters
            lt = length(tip)
            p = Vector{Any}(undef, lt)
            for j = 1:lt
                ui = tip[j]
                p[j] = issimpleenoughtupleelem(unwrapva(ui)) ? ui : isvarargtype(ui) ? Vararg : Any
            end
            types[i] = rewrap_unionall(Tuple{p...}, ti)
        else
            # this element is not simple enough yet, make it so now
            types[i] = typenames[i].wrapper
        end
    end
    u = Union{types...}
    return u
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
        p[i] = issimpleenoughtupleelem(ui) ? ui : Any
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
                #   c.f. tname_intersect in the algorithm above
                hasfree = has_free_typevars(ti)
                if hasfree || !(ti <: tail)
                    if !hasfree && tail <: ti
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
                tail === Any && return Tuple # short-circuit loops
            end
        end
        @assert !(tail === Union{})
        if !issimpleenoughtupleelem(tail) || tail === Any
            p[lt + 1] = Vararg
            lt == 0 && return Tuple
        else
            p[lt + 1] = Vararg{tail}
        end
    end
    return Tuple{p...}
end
