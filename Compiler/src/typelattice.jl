# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# structs/constants #
#####################

# N.B.: Const/PartialStruct/InterConditional are defined in Core, to allow them to be used
# inside the global code cache.
import Core: Const, InterConditional, PartialStruct

"""
    cnd::Conditional

The type of this value might be `Bool`.
However, to enable a limited amount of back-propagation,
we also keep some information about how this `Bool` value was created.
In particular, if you branch on this value, then may assume that in the true branch,
the type of `SlotNumber(cnd.slot)` will be limited by `cnd.thentype`
and in the false branch, it will be limited by `cnd.elsetype`.
Example:
```julia
let cond = isa(x::Union{Int, Float}, Int)::Conditional(x, Int, Float)
    if cond
       # May assume x is `Int` now
    else
       # May assume x is `Float` now
    end
end
```
"""
struct Conditional
    slot::Int
    thentype
    elsetype
    # `isdefined` indicates this `Conditional` is from `@isdefined slot`, implying that
    # the `undef` information of `slot` can be improved in the then branch.
    # Since this is only beneficial for local inference, it is not translated into `InterConditional`.
    isdefined::Bool
    function Conditional(slot::Int, @nospecialize(thentype), @nospecialize(elsetype);
                         isdefined::Bool=false)
        assert_nested_slotwrapper(thentype)
        assert_nested_slotwrapper(elsetype)
        return new(slot, thentype, elsetype, isdefined)
    end
end
Conditional(var::SlotNumber, @nospecialize(thentype), @nospecialize(elsetype); isdefined::Bool=false) =
    Conditional(slot_id(var), thentype, elsetype; isdefined)

const AnyConditional = Union{Conditional,InterConditional}
Conditional(cnd::InterConditional) = Conditional(cnd.slot, cnd.thentype, cnd.elsetype)
InterConditional(cnd::Conditional) = InterConditional(cnd.slot, cnd.thentype, cnd.elsetype)

"""
    alias::MustAlias

This lattice element wraps a reference to object field while recoding the identity of the
parent object. It allows certain constraints that can be imposed on the object field type
by built-in functions like `isa` and `===` to be propagated to another reference to the
same object field.
One important note is that this lattice element assumes the invariant that the field of
wrapped slot object never changes until the slot object is re-assigned. This means, the
wrapped object field should be constant as inference currently doesn't track any memory
effects on per-object basis. Particularly `maybe_const_fldidx` takes the lift to check if
a given lattice element is eligible to be wrapped by `MustAlias`. Example:
```juila
let alias = getfield(x::Some{Union{Nothing,String}}, :value)::MustAlias(x, Some{Union{Nothing,String}}, 1, Union{Nothing,String})
    if alias === nothing
        # May assume `getfield(x, :value)` is `nothing` now
    else
        # May assume `getfield(x, :value)` is `::String` now
    end
end
```
N.B. currently this lattice element is only used in abstractinterpret, not in optimization
"""
struct MustAlias
    slot::Int
    vartyp::Any
    fldidx::Int
    fldtyp::Any
    function MustAlias(slot::Int, @nospecialize(vartyp), fldidx::Int, @nospecialize(fldtyp))
        assert_nested_slotwrapper(vartyp)
        assert_nested_slotwrapper(fldtyp)
        # @assert !isalreadyconst(vartyp) "vartyp is already const"
        # @assert !isalreadyconst(fldtyp) "fldtyp is already const"
        return new(slot, vartyp, fldidx, fldtyp)
    end
end
MustAlias(var::SlotNumber, @nospecialize(vartyp), fldidx::Int, @nospecialize(fldtyp)) =
    MustAlias(slot_id(var), vartyp, fldidx, fldtyp)

"""
    alias::InterMustAlias

This lattice element used in a very similar way as `InterConditional`, but corresponds to `MustAlias`.
"""
struct InterMustAlias
    slot::Int
    vartyp::Any
    fldidx::Int
    fldtyp::Any
    function InterMustAlias(slot::Int, @nospecialize(vartyp), fldidx::Int, @nospecialize(fldtyp))
        assert_nested_slotwrapper(vartyp)
        assert_nested_slotwrapper(fldtyp)
        # @assert !isalreadyconst(vartyp) "vartyp is already const"
        # @assert !isalreadyconst(fldtyp) "fldtyp is already const"
        return new(slot, vartyp, fldidx, fldtyp)
    end
end
InterMustAlias(var::SlotNumber, @nospecialize(vartyp), fldidx::Int, @nospecialize(fldtyp)) =
    InterMustAlias(slot_id(var), vartyp, fldidx, fldtyp)

const AnyMustAlias = Union{MustAlias,InterMustAlias}
MustAlias(alias::InterMustAlias) = MustAlias(alias.slot, alias.vartyp, alias.fldidx, alias.fldtyp)
InterMustAlias(alias::MustAlias) = InterMustAlias(alias.slot, alias.vartyp, alias.fldidx, alias.fldtyp)

struct PartialTypeVar
    tv::TypeVar
    # N.B.: Currently unused, but would allow turning something back
    # into Const, if the bounds are pulled out of this TypeVar
    lb_certain::Bool
    ub_certain::Bool
    PartialTypeVar(tv::TypeVar, lb_certain::Bool, ub_certain::Bool) = new(tv, lb_certain, ub_certain)
end

struct StateUpdate
    var::SlotNumber
    vtype::VarState
    conditional::Bool
    StateUpdate(var::SlotNumber, vtype::VarState, conditional::Bool=false) = new(var, vtype, conditional)
end

"""
    struct LimitedAccuracy

A `LimitedAccuracy` lattice element is used to indicate that the true inference
result was approximate due to heuristic termination of a recursion. For example,
consider two call stacks starting from `A` and `B` that look like:

    A -> C -> A -> D
    B -> C -> A -> D

In the first case, inference may have decided that `A->C->A` constitutes a cycle,
widening the result it obtained for `C`, even if it might otherwise have been
able to obtain a result. In this case, the result inferred for `C` will be
annotated with this lattice type to indicate that the obtained result is an
upper bound for the non-limited inference. In particular, this means that the
call stack originating at `B` will re-perform inference without being poisoned
by the potentially inaccurate result obtained during the inference of `A`.

N.B.: We do *not* take any efforts to ensure the reverse. For example, if `B`
is inferred first, then we may cache a precise result for `C` and re-use this
result while inferring `A`, even if inference of `A` would have not been able
to obtain this result due to limiting. This is undesirable, because it makes
some inference results order dependent, but there it is unclear how this situation
could be avoided.

A `LimitedAccuracy` element wraps another lattice element (let's call it `T`)
and additionally tracks the `causes` due to which limitation occurred. As a
lattice element, `LimitedAccuracy(T)` is considered ε smaller than the
corresponding lattice element `T`, but in particular, all lattice elements that
are `⊑ T` (but not equal `T`) are also `⊑ LimitedAccuracy(T)`.

The `causes` list is used to determine whether a particular cause of limitation is
inevitable and if so, widening `LimitedAccuracy(T)` back to `T`. For example,
in the call stacks above, if any call to `A` always leads back to `A`, then
it does not matter whether we start at `A` or reach it via `B`: Any inference
that reaches `A` will always hit the same limitation and the result may thus
be cached.
"""
struct LimitedAccuracy
    typ
    causes::IdSet{InferenceState}
    function LimitedAccuracy(@nospecialize(typ), causes::IdSet{InferenceState})
        @assert !isa(typ, LimitedAccuracy) "found nested LimitedAccuracy"
        return new(typ, causes)
    end
end
LimitedAccuracy(@nospecialize(T), ::Nothing) = T

"""
    struct NotFound end
    const NOT_FOUND = NotFound()

A special singleton that represents a variable has not been analyzed yet.
Particularly, all SSA value types are initialized as `NOT_FOUND` when creating a new `InferenceState`.
Note that this is only used for `smerge`, which updates abstract state `VarTable`,
and thus we don't define the lattice for this.
"""
struct NotFound end

const NOT_FOUND = NotFound()

#################
# lattice logic #
#################

# slot wrappers
# =============

@nospecializeinfer function assert_nested_slotwrapper(@nospecialize t)
    @assert !(t isa Conditional)      "found nested Conditional"
    @assert !(t isa InterConditional) "found nested InterConditional"
    @assert !(t isa MustAlias)        "found nested MustAlias"
    @assert !(t isa InterMustAlias)   "found nested InterMustAlias"
    return t
end

@nospecializeinfer function widenslotwrapper(@nospecialize typ)
    if isa(typ, AnyConditional)
        return widenconditional(typ)
    elseif isa(typ, AnyMustAlias)
        return widenmustalias(typ)
    end
    return typ
end

@nospecializeinfer function widenwrappedslotwrapper(@nospecialize typ)
    if isa(typ, LimitedAccuracy)
        return LimitedAccuracy(widenslotwrapper(typ.typ), typ.causes)
    end
    return widenslotwrapper(typ)
end

# Conditional
# ===========

@nospecializeinfer function widenconditional(@nospecialize typ)
    if isa(typ, AnyConditional)
        if typ.thentype === Union{}
            return Const(false)
        elseif typ.elsetype === Union{}
            return Const(true)
        else
            return Bool
        end
    elseif isa(typ, LimitedAccuracy)
        error("unhandled LimitedAccuracy")
    end
    return typ
end
@nospecializeinfer function widenwrappedconditional(@nospecialize typ)
    if isa(typ, LimitedAccuracy)
        return LimitedAccuracy(widenconditional(typ.typ), typ.causes)
    end
    return widenconditional(typ)
end

# `Conditional` and `InterConditional` are valid in opposite contexts
# (i.e. local inference and inter-procedural call), as such they will never be compared
@nospecializeinfer issubconditional(𝕃::AbstractLattice, a::Conditional, b::Conditional) =
    _issubconditional(𝕃, a, b, #=check_isdefined=#true)
@nospecializeinfer issubconditional(𝕃::AbstractLattice, a::InterConditional, b::InterConditional) =
    _issubconditional(𝕃, a, b, #=check_isdefined=#false)
@nospecializeinfer function _issubconditional(𝕃::AbstractLattice, a::C, b::C, check_isdefined::Bool) where C<:AnyConditional
    if is_same_conditionals(a, b)
        if ⊑(𝕃, a.thentype, b.thentype)
            if ⊑(𝕃, a.elsetype, b.elsetype)
                if !check_isdefined || a.isdefined ≥ b.isdefined
                    return true
                end
            end
        end
    end
    return false
end

is_same_conditionals(a::C, b::C) where C<:AnyConditional = a.slot == b.slot

@nospecializeinfer is_lattice_bool(lattice::AbstractLattice, @nospecialize(typ)) = typ !== Bottom && ⊑(lattice, typ, Bool)

maybe_extract_const_bool(c::Const) = (val = c.val; isa(val, Bool)) ? val : nothing
function maybe_extract_const_bool(c::AnyConditional)
    (c.thentype === Bottom && !(c.elsetype === Bottom)) && return false
    (c.elsetype === Bottom && !(c.thentype === Bottom)) && return true
    nothing
end
@nospecializeinfer maybe_extract_const_bool(@nospecialize c) = nothing

# MustAlias
# =========

@nospecializeinfer function widenmustalias(@nospecialize typ)
    if isa(typ, AnyMustAlias)
        return typ.fldtyp
    elseif isa(typ, LimitedAccuracy)
        error("unhandled LimitedAccuracy")
    end
    return typ
end

@nospecializeinfer function isalreadyconst(@nospecialize t)
    isa(t, Const) && return true
    issingletontype(t) && return true
    return isconstType(t)
end

@nospecializeinfer function maybe_const_fldidx(@nospecialize(objtyp), @nospecialize(fldval))
    t = widenconst(objtyp)
    if isa(fldval, Int)
        fldidx = fldval
    elseif isa(fldval, Symbol)
        isa(t, DataType) || isa(t, UnionAll) || return nothing
        fldidx = fieldindex(t, fldval, false)
    else
        return nothing
    end
    fldidx == 0 && return nothing
    isconst(t, fldidx) || return nothing
    fldcnt = fieldcount_noerror(t)
    (fldcnt === nothing || fldcnt == 0) && return nothing
    return fldidx
end

@nospecializeinfer function form_mustalias_conditional(alias::MustAlias, @nospecialize(thentype), @nospecialize(elsetype))
    (; slot, vartyp, fldidx) = alias
    if isa(vartyp, PartialStruct)
        fields = vartyp.fields
        thenfields = thentype === Bottom ? nothing : copy(fields)
        elsefields = elsetype === Bottom ? nothing : copy(fields)
        undefs = copy(_getundefs(vartyp))
        if 1 ≤ fldidx ≤ length(fields)
            thenfields === nothing || (thenfields[fldidx] = thentype)
            elsefields === nothing || (elsefields[fldidx] = elsetype)
            undefs[fldidx] = false
        end
        return Conditional(slot,
            thenfields === nothing ? Bottom : PartialStruct(fallback_lattice, vartyp.typ, undefs, thenfields),
            elsefields === nothing ? Bottom : PartialStruct(fallback_lattice, vartyp.typ, undefs, elsefields))
    else
        vartyp_widened = widenconst(vartyp)
        thenfields = thentype === Bottom ? nothing : Any[]
        elsefields = elsetype === Bottom ? nothing : Any[]
        for i in 1:fieldcount(vartyp_widened)
            if i == fldidx
                thenfields === nothing || push!(thenfields, thentype)
                elsefields === nothing || push!(elsefields, elsetype)
            else
                t = fieldtype(vartyp_widened, i)
                thenfields === nothing || push!(thenfields, t)
                elsefields === nothing || push!(elsefields, t)
            end
        end
        return Conditional(slot,
            thenfields === nothing ? Bottom : PartialStruct(fallback_lattice, vartyp_widened, thenfields),
            elsefields === nothing ? Bottom : PartialStruct(fallback_lattice, vartyp_widened, elsefields))
    end
end

function issubalias(a::AnyMustAlias, b::AnyMustAlias)
    return a.slot == b.slot && a.fldidx == b.fldidx &&
        a.vartyp ⊑ b.vartyp && a.fldtyp ⊑ b.fldtyp
end

# LimitedAccuracy
# ===============

ignorelimited(@nospecialize typ) = typ
ignorelimited(typ::LimitedAccuracy) = typ.typ

# lattice order
# =============

@nospecializeinfer function ⊑(lattice::InferenceLattice, @nospecialize(a), @nospecialize(b))
    ⊑(widenlattice(lattice), ignorelimited(a), ignorelimited(b)) || return false

    isa(b, LimitedAccuracy) || return true

    # We've found that ignorelimited(a) ⊑ ignorelimited(b).
    # Now perform the reverse query to check for equality.
    ab_eq = ⊑(widenlattice(lattice), b.typ, ignorelimited(a))

    if !ab_eq
        # a's unlimited type is strictly smaller than b's
        return true
    end

    # a and b's unlimited types are equal.
    isa(a, LimitedAccuracy) || return false # b is limited, so ε smaller
    return b.causes ⊆ a.causes
end

@nospecializeinfer function ⊑(lattice::AnyConditionalsLattice, @nospecialize(a), @nospecialize(b))
    # Fast paths for common cases
    b === Any && return true
    a === Any && return false
    a === Union{} && return true
    b === Union{} && return false
    ConditionalT = isa(lattice, ConditionalsLattice) ? Conditional : InterConditional
    if isa(a, ConditionalT)
        if isa(b, ConditionalT)
            return issubconditional(lattice, a, b)
        elseif isa(b, Const) && isa(b.val, Bool)
            return maybe_extract_const_bool(a) === b.val
        end
        a = Bool
    elseif isa(b, ConditionalT)
        if isa(a, Const) && isa(a.val, Bool)
           if (a.val === true && b.thentype === Any && b.elsetype === Bottom) ||
              (a.val === false && b.elsetype === Any && b.thentype === Bottom)
               # this Conditional contains distinctly no lattice information, and is simply an alternative representation of the Const Bool used for internal tracking purposes
               return true
           end
        end
        return false
    end
    return ⊑(widenlattice(lattice), a, b)
end

@nospecializeinfer function ⊑(𝕃::AnyMustAliasesLattice, @nospecialize(a), @nospecialize(b))
    MustAliasT = isa(𝕃, MustAliasesLattice) ? MustAlias : InterMustAlias
    if isa(a, MustAliasT)
        if isa(b, MustAliasT)
            return issubalias(a, b)
        end
        a = widenmustalias(a)
    elseif isa(b, MustAliasT)
        return ⊏(widenlattice(𝕃), a, widenmustalias(b))
    end
    return ⊑(widenlattice(𝕃), a, b)
end

@nospecializeinfer function ⊑(lattice::PartialsLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, PartialStruct)
        if isa(b, PartialStruct)
            a.typ <: b.typ || return false
            nflds = length(a.fields)
            nflds == length(b.fields) || return false
            for i in 1:nflds
                if !(_getundefs(b)[i] === nothing || _getundefs(a)[i] === _getundefs(b)[i])
                    return false
                end
                af = a.fields[i]
                bf = b.fields[i]
                if i == nflds
                    if isvarargtype(af)
                        # If `af` is vararg, so must bf by the <: above
                        @assert isvarargtype(bf)
                        continue
                    elseif isvarargtype(bf)
                        # If `bf` is vararg, it must match the information
                        # in the type, so there's nothing to check here.
                        continue
                    end
                end
                ⊑(lattice, af, bf) || return false
            end
            return true
        end
        return isa(b, Type) && a.typ <: b
    elseif isa(b, PartialStruct)
        if isa(a, Const)
            widea = widenconst(a)::DataType
            wideb = widenconst(b)
            wideb′ = unwrap_unionall(wideb)::DataType
            widea.name === wideb′.name || return false
            if wideb′.name === Tuple.name
                # We can skip the subtype check if b is a Tuple, since in that
                # case, the ⊑ of the elements is sufficient.
                # But for tuple comparisons, we need their lengths to be the same for now.
                # TODO improve accuracy for cases when `b` contains vararg element
                nfields(a.val) == length(b.fields) || return false
            else
                widea <: wideb || return false
                # for structs we need to check that `a` does not have less information than `b` that may be partially initialized
                n_initialized(a) ≥ n_initialized(b) || return false
            end
            nf = nfields(a.val)
            for i in 1:nf
                if !isdefined(a.val, i)
                    _getundefs(b)[i] === false && return false # conflicting defined-ness information
                    continue # since ∀ T Union{} ⊑ T
                end
                i > length(b.fields) && break # `a` has more information than `b` that is partially initialized struct
                if _getundefs(b)[i] === true
                    return false # conflicting defined-ness information
                end
                bfᵢ = b.fields[i]
                if i == nf
                    bfᵢ = unwrapva(bfᵢ)
                end
                ⊑(lattice, Const(getfield(a.val, i)), bfᵢ) || return false
            end
            return true
        end
        return false
    end
    if isa(a, PartialOpaque)
        if isa(b, PartialOpaque)
            (a.parent === b.parent && a.source === b.source) || return false
            return (widenconst(a) <: widenconst(b)) &&
                ⊑(lattice, a.env, b.env)
        end
        return ⊑(widenlattice(lattice), widenconst(a), b)
    elseif isa(b, PartialOpaque)
        return false
    end
    return ⊑(widenlattice(lattice), a, b)
end

@nospecializeinfer function ⊑(lattice::ConstsLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, Const)
        if isa(b, Const)
            return a.val === b.val
        end
        # TODO: `b` could potentially be a `PartialTypeVar` here, in which case we might be
        # able to return `true` in more cases; in the meantime, just returning this is the
        # most conservative option.
        return isa(b, Type) && isa(a.val, b)
    elseif isa(b, Const)
        if issingletontype(a)
            return a.instance === b.val
        end
        return false
    elseif isa(a, PartialTypeVar)
        return b === TypeVar || a === b
    elseif isa(b, PartialTypeVar)
        return false
    end
    return ⊑(widenlattice(lattice), a, b)
end

@nospecializeinfer function is_lattice_equal(lattice::InferenceLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, LimitedAccuracy)
        isa(b, LimitedAccuracy) || return false
        a.causes == b.causes || return false
        a = a.typ
        b = b.typ
    elseif isa(b, LimitedAccuracy)
        return false
    end
    return is_lattice_equal(widenlattice(lattice), a, b)
end

@nospecializeinfer function is_lattice_equal(lattice::AnyConditionalsLattice, @nospecialize(a), @nospecialize(b))
    ConditionalT = isa(lattice, ConditionalsLattice) ? Conditional : InterConditional
    if isa(a, ConditionalT) || isa(b, ConditionalT)
        # TODO: Unwrap these and recurse to is_lattice_equal
        return ⊑(lattice, a, b) && ⊑(lattice, b, a)
    end
    return is_lattice_equal(widenlattice(lattice), a, b)
end

@nospecializeinfer function is_lattice_equal(lattice::PartialsLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, PartialStruct)
        isa(b, PartialStruct) || return false
        length(a.fields) == length(b.fields) || return false
        _getundefs(a) == _getundefs(b) || return false
        widenconst(a) == widenconst(b) || return false
        a.fields === b.fields && return true # fast path
        for i in 1:length(a.fields)
            is_lattice_equal(lattice, a.fields[i], b.fields[i]) || return false
        end
        return true
    end
    isa(b, PartialStruct) && return false
    if isa(a, PartialOpaque)
        isa(b, PartialOpaque) || return false
        widenconst(a) == widenconst(b) || return false
        a.source === b.source || return false
        a.parent === b.parent || return false
        return is_lattice_equal(lattice, a.env, b.env)
    end
    isa(b, PartialOpaque) && return false
    return is_lattice_equal(widenlattice(lattice), a, b)
end

@nospecializeinfer function is_lattice_equal(lattice::ConstsLattice, @nospecialize(a), @nospecialize(b))
    a === b && return true
    if a isa Const
        if issingletontype(b)
            return a.val === b.instance
        end
        # N.B. Assumes a === b checked above
        return false
    end
    if b isa Const
        if issingletontype(a)
            return a.instance === b.val
        end
        # N.B. Assumes a === b checked above
        return false
    end
    if isa(a, PartialTypeVar) || isa(b, PartialTypeVar)
        return false
    end
    return is_lattice_equal(widenlattice(lattice), a, b)
end

# lattice operations
# ==================

@nospecializeinfer function tmeet(lattice::PartialsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, PartialStruct)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        ti = typeintersect(widev, t)
        if ti === widev
            return v
        end
        valid_as_lattice(ti, true) || return Bottom
        if widev <: Tuple
            new_fields = Vector{Any}(undef, length(v.fields))
            for i = 1:length(new_fields)
                vfi = v.fields[i]
                if isvarargtype(vfi)
                    new_fields[i] = vfi
                else
                    nfi = new_fields[i] = tmeet(lattice, vfi, widenconst(getfield_tfunc(lattice, t, Const(i))))
                    if nfi === Bottom
                        return Bottom
                    end
                end
            end
            return tuple_tfunc(lattice, new_fields)
        end
        v = widev
    elseif isa(v, PartialOpaque)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        if widev <: t
            return v
        end
        ti = typeintersect(widev, t)
        valid_as_lattice(ti, true) || return Bottom
        return PartialOpaque(ti, v.env, v.parent, v.source)
    end
    return tmeet(widenlattice(lattice), v, t)
end

@nospecializeinfer function tmeet(lattice::ConstsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, Const)
        if !has_free_typevars(t) && !isa(v.val, t)
            return Bottom
        end
        return v
    end
    tmeet(widenlattice(lattice), widenconst(v), t)
end

@nospecializeinfer function tmeet(lattice::ConditionalsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, Conditional)
        if !(Bool <: t)
            return Bottom
        end
        return v
    end
    tmeet(widenlattice(lattice), v, t)
end

@nospecializeinfer function tmeet(𝕃::MustAliasesLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, MustAlias)
        v = widenmustalias(v)
    end
    return tmeet(widenlattice(𝕃), v, t)
end

@nospecializeinfer function tmeet(lattice::InferenceLattice, @nospecialize(v), @nospecialize(t::Type))
    # TODO: This can probably happen and should be handled
    @assert !isa(v, LimitedAccuracy)
    tmeet(widenlattice(lattice), v, t)
end

@nospecializeinfer function tmeet(lattice::InterConditionalsLattice, @nospecialize(v), @nospecialize(t::Type))
    # TODO: This can probably happen and should be handled
    @assert !isa(v, AnyConditional)
    tmeet(widenlattice(lattice), v, t)
end

@nospecializeinfer function tmeet(𝕃::InterMustAliasesLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, InterMustAlias)
        v = widenmustalias(v)
    end
    return tmeet(widenlattice(𝕃), v, t)
end

"""
    widenconst(x) -> t::Type

Widens extended lattice element `x` to native `Type` representation.
"""
widenconst(::AnyConditional) = Bool
widenconst(a::AnyMustAlias) = widenconst(widenmustalias(a))
widenconst(c::Const) = (v = c.val; isa(v, Type) ? Type{v} : typeof(v))
widenconst(::PartialTypeVar) = TypeVar
widenconst(t::Core.PartialStruct) = t.typ
widenconst(t::PartialOpaque) = t.typ
@nospecializeinfer widenconst(@nospecialize t::Type) = t
widenconst(::TypeVar) = error("unhandled TypeVar")
widenconst(::TypeofVararg) = error("unhandled Vararg")
widenconst(::LimitedAccuracy) = error("unhandled LimitedAccuracy")

####################
# state management #
####################

function smerge(lattice::AbstractLattice, sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    sa === sb && return sa
    sa === NOT_FOUND && return sb
    sb === NOT_FOUND && return sa
    return VarState(tmerge(lattice, sa.typ, sb.typ), sa.undef | sb.undef)
end

@nospecializeinfer @inline schanged(lattice::AbstractLattice, @nospecialize(n), @nospecialize(o)) =
    (n !== o) && (o === NOT_FOUND || (n !== NOT_FOUND && !(n.undef <= o.undef && ⊑(lattice, n.typ, o.typ))))

# remove any lattice elements that wrap the reassigned slot object from the vartable
function invalidate_slotwrapper(vt::VarState, changeid::Int, ignore_conditional::Bool)
    newtyp = ignorelimited(vt.typ)
    if (!ignore_conditional && isa(newtyp, Conditional) && newtyp.slot == changeid) ||
       (isa(newtyp, MustAlias) && newtyp.slot == changeid)
        newtyp = @noinline widenwrappedslotwrapper(vt.typ)
        return VarState(newtyp, vt.undef)
    end
    return nothing
end

function stupdate!(lattice::AbstractLattice, state::VarTable, changes::VarTable)
    changed = false
    for i = 1:length(state)
        newtype = changes[i]
        oldtype = state[i]
        if schanged(lattice, newtype, oldtype)
            state[i] = smerge(lattice, oldtype, newtype)
            changed = true
        end
    end
    return changed
end

function stoverwrite!(state::VarTable, newstate::VarTable)
    for i = 1:length(state)
        state[i] = newstate[i]
    end
    return state
end

function stoverwrite1!(state::VarTable, change::StateUpdate)
    changeid = slot_id(change.var)
    for i = 1:length(state)
        invalidated = invalidate_slotwrapper(state[i], changeid, change.conditional)
        if invalidated !== nothing
            state[i] = invalidated
        end
    end
    # and update the type of it
    newtype = change.vtype
    state[changeid] = newtype
    return state
end

# The ::AbstractLattice argument is unused and simply serves to disambiguate
# different instances of the compiler that may share the `Core.PartialStruct`
# type.

# Legacy constructor
function Core.PartialStruct(𝕃::AbstractLattice, @nospecialize(typ), fields::Vector{Any})
    undefs = partialstruct_init_undefs(typ, fields)
    undefs === nothing && error("This object never exists at runtime")
    return PartialStruct(𝕃, typ, undefs, fields)
end

function Core.PartialStruct(::AbstractLattice, @nospecialize(typ), undefs::Vector{Union{Nothing,Bool}}, fields::Vector{Any})
    for i = 1:length(fields)
        assert_nested_slotwrapper(fields[i])
    end
    return PartialStruct(typ, undefs, fields)
end

# a special getter for `PartialStruct` to achieve better type stability:
# `(x::PartialStruct).undefs` will be lowered to `getfield(x, :undefs)::Any` otherwise
_getundefs(p::PartialStruct) = Base.getproperty(p, :undefs)
