# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# structs/constants #
#####################

# N.B.: Const/PartialStruct/InterConditional are defined in Core, to allow them to be used
# inside the global code cache.
#
# # The type of a value might be constant
# struct Const
#     val
# end
#
# struct PartialStruct
#     typ
#     fields::Vector{Any} # elements are other type lattice members
# end
import Core: Const, PartialStruct
function PartialStruct(@nospecialize(typ), fields::Vector{Any})
    for i = 1:length(fields)
        assert_nested_slotwrapper(fields[i])
    end
    return Core._PartialStruct(typ, fields)
end

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
    function Conditional(slot::Int, @nospecialize(thentype), @nospecialize(elsetype))
        assert_nested_slotwrapper(thentype)
        assert_nested_slotwrapper(elsetype)
        return new(slot, thentype, elsetype)
    end
end
Conditional(var::SlotNumber, @nospecialize(thentype), @nospecialize(elsetype)) =
    Conditional(slot_id(var), thentype, elsetype)

"""
    cnd::InterConditional

Similar to `Conditional`, but conveys inter-procedural constraints imposed on call arguments.
This is separate from `Conditional` to catch logic errors: the lattice element name is `InterConditional`
while processing a call, then `Conditional` everywhere else. Thus `InterConditional` does not appear in
`CompilerTypes`—these type's usages are disjoint—though we define the lattice for `InterConditional`.
"""
:(InterConditional)
import Core: InterConditional
# struct InterConditional
#     slot::Int
#     thentype
#     elsetype
#     InterConditional(slot::Int, @nospecialize(thentype), @nospecialize(elsetype)) =
#         new(slot, thentype, elsetype)
# end
InterConditional(var::SlotNumber, @nospecialize(thentype), @nospecialize(elsetype)) =
    InterConditional(slot_id(var), thentype, elsetype)

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

_uniontypes(x::MustAlias, ts) = _uniontypes(widenconst(x), ts)

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

# Wraps a type and represents that the value may also be undef at this point.
# (only used in optimize, not abstractinterpret)
# N.B. in the lattice, this is epsilon bigger than `typ` (even Any)
struct MaybeUndef
    typ
    MaybeUndef(@nospecialize(typ)) = new(typ)
end

struct StateUpdate
    var::SlotNumber
    vtype::VarState
    state::VarTable
    conditional::Bool
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

const CompilerTypes = Union{MaybeUndef, Const, Conditional, MustAlias, NotFound, PartialStruct}
==(x::CompilerTypes, y::CompilerTypes) = x === y
==(x::Type, y::CompilerTypes) = false
==(x::CompilerTypes, y::Type) = false

#################
# lattice logic #
#################

# slot wrappers
# =============

function assert_nested_slotwrapper(@nospecialize t)
    @assert !(t isa Conditional)      "found nested Conditional"
    @assert !(t isa InterConditional) "found nested InterConditional"
    @assert !(t isa MustAlias)        "found nested MustAlias"
    @assert !(t isa InterMustAlias)   "found nested InterMustAlias"
    return t
end

function widenslotwrapper(@nospecialize typ)
    if isa(typ, AnyConditional)
        return widenconditional(typ)
    elseif isa(typ, AnyMustAlias)
        return widenmustalias(typ)
    end
    return typ
end

function widenwrappedslotwrapper(@nospecialize typ)
    if isa(typ, LimitedAccuracy)
        return LimitedAccuracy(widenslotwrapper(typ.typ), typ.causes)
    end
    return widenslotwrapper(typ)
end

# Conditional
# ===========

function widenconditional(@nospecialize typ)
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
function widenwrappedconditional(@nospecialize typ)
    if isa(typ, LimitedAccuracy)
        return LimitedAccuracy(widenconditional(typ.typ), typ.causes)
    end
    return widenconditional(typ)
end

# `Conditional` and `InterConditional` are valid in opposite contexts
# (i.e. local inference and inter-procedural call), as such they will never be compared
function issubconditional(lattice::AbstractLattice, a::C, b::C) where {C<:AnyConditional}
    if is_same_conditionals(a, b)
        if ⊑(lattice, a.thentype, b.thentype)
            if ⊑(lattice, a.elsetype, b.elsetype)
                return true
            end
        end
    end
    return false
end

is_same_conditionals(a::C, b::C) where C<:AnyConditional = a.slot == b.slot

is_lattice_bool(lattice::AbstractLattice, @nospecialize(typ)) = typ !== Bottom && ⊑(lattice, typ, Bool)

maybe_extract_const_bool(c::Const) = (val = c.val; isa(val, Bool)) ? val : nothing
function maybe_extract_const_bool(c::AnyConditional)
    (c.thentype === Bottom && !(c.elsetype === Bottom)) && return false
    (c.elsetype === Bottom && !(c.thentype === Bottom)) && return true
    nothing
end
maybe_extract_const_bool(@nospecialize c) = nothing

# MustAlias
# =========

function widenmustalias(@nospecialize typ)
    if isa(typ, AnyMustAlias)
        return typ.fldtyp
    elseif isa(typ, LimitedAccuracy)
        error("unhandled LimitedAccuracy")
    end
    return typ
end

function isalreadyconst(@nospecialize t)
    isa(t, Const) && return true
    isa(t, DataType) && isdefined(t, :instance) && return true
    return isconstType(t)
end

function maybe_const_fldidx(@nospecialize(objtyp), @nospecialize(fldval))
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

function form_mustalias_conditional(alias::MustAlias, @nospecialize(thentype), @nospecialize(elsetype))
    (; slot, vartyp, fldidx) = alias
    if isa(vartyp, PartialStruct)
        fields = vartyp.fields
        thenfields = thentype === Bottom ? nothing : copy(fields)
        elsefields = elsetype === Bottom ? nothing : copy(fields)
        for i in 1:length(fields)
            if i == fldidx
                thenfields === nothing || (thenfields[i] = thentype)
                elsefields === nothing || (elsefields[i] = elsetype)
            end
        end
        return Conditional(slot,
            thenfields === nothing ? Bottom : PartialStruct(vartyp.typ, thenfields),
            elsefields === nothing ? Bottom : PartialStruct(vartyp.typ, elsefields))
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
            thenfields === nothing ? Bottom : PartialStruct(vartyp_widened, thenfields),
            elsefields === nothing ? Bottom : PartialStruct(vartyp_widened, elsefields))
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

function ⊑(lattice::InferenceLattice, @nospecialize(a), @nospecialize(b))
    r = ⊑(widenlattice(lattice), ignorelimited(a), ignorelimited(b))
    r || return false
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

function ⊑(lattice::OptimizerLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, MaybeUndef)
        isa(b, MaybeUndef) || return false
        a, b = a.typ, b.typ
    elseif isa(b, MaybeUndef)
        b = b.typ
    end
    return ⊑(widenlattice(lattice), a, b)
end

function ⊑(lattice::AnyConditionalsLattice, @nospecialize(a), @nospecialize(b))
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
        return false
    end
    return ⊑(widenlattice(lattice), a, b)
end

function ⊑(𝕃::AnyMustAliasesLattice, @nospecialize(a), @nospecialize(b))
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

function ⊑(lattice::PartialsLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, PartialStruct)
        if isa(b, PartialStruct)
            if !(length(a.fields) == length(b.fields) && a.typ <: b.typ)
                return false
            end
            for i in 1:length(b.fields)
                af = a.fields[i]
                bf = b.fields[i]
                if i == length(b.fields)
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
            nf = nfields(a.val)
            nf == length(b.fields) || return false
            widea = widenconst(a)::DataType
            wideb = widenconst(b)
            wideb′ = unwrap_unionall(wideb)::DataType
            widea.name === wideb′.name || return false
            # We can skip the subtype check if b is a Tuple, since in that
            # case, the ⊑ of the elements is sufficient.
            if wideb′.name !== Tuple.name && !(widea <: wideb)
                return false
            end
            for i in 1:nf
                isdefined(a.val, i) || continue # since ∀ T Union{} ⊑ T
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

function ⊑(lattice::ConstsLattice, @nospecialize(a), @nospecialize(b))
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

function is_lattice_equal(lattice::InferenceLattice, @nospecialize(a), @nospecialize(b))
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

function is_lattice_equal(lattice::OptimizerLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, MaybeUndef) || isa(b, MaybeUndef)
        # TODO: Unwrap these and recurse to is_lattice_equal
        return ⊑(lattice, a, b) && ⊑(lattice, b, a)
    end
    return is_lattice_equal(widenlattice(lattice), a, b)
end

function is_lattice_equal(lattice::AnyConditionalsLattice, @nospecialize(a), @nospecialize(b))
    ConditionalT = isa(lattice, ConditionalsLattice) ? Conditional : InterConditional
    if isa(a, ConditionalT) || isa(b, ConditionalT)
        # TODO: Unwrap these and recurse to is_lattice_equal
        return ⊑(lattice, a, b) && ⊑(lattice, b, a)
    end
    return is_lattice_equal(widenlattice(lattice), a, b)
end

function is_lattice_equal(lattice::PartialsLattice, @nospecialize(a), @nospecialize(b))
    if isa(a, PartialStruct)
        isa(b, PartialStruct) || return false
        length(a.fields) == length(b.fields) || return false
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

function is_lattice_equal(lattice::ConstsLattice, @nospecialize(a), @nospecialize(b))
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

function tmeet(lattice::PartialsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, PartialStruct)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        ti = typeintersect(widev, t)
        if ti === widev
            return v
        end
        valid_as_lattice(ti) || return Bottom
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
        valid_as_lattice(ti) || return Bottom
        return PartialOpaque(ti, v.env, v.parent, v.source)
    end
    return tmeet(widenlattice(lattice), v, t)
end

function tmeet(lattice::ConstsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, Const)
        if !has_free_typevars(t) && !isa(v.val, t)
            return Bottom
        end
        return v
    end
    tmeet(widenlattice(lattice), widenconst(v), t)
end

function tmeet(lattice::ConditionalsLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, Conditional)
        if !(Bool <: t)
            return Bottom
        end
        return v
    end
    tmeet(widenlattice(lattice), v, t)
end

function tmeet(𝕃::MustAliasesLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, MustAlias)
        v = widenmustalias(v)
    end
    return tmeet(widenlattice(𝕃), v, t)
end

function tmeet(lattice::InferenceLattice, @nospecialize(v), @nospecialize(t::Type))
    # TODO: This can probably happen and should be handled
    @assert !isa(v, LimitedAccuracy)
    tmeet(widenlattice(lattice), v, t)
end

function tmeet(lattice::InterConditionalsLattice, @nospecialize(v), @nospecialize(t::Type))
    # TODO: This can probably happen and should be handled
    @assert !isa(v, AnyConditional)
    tmeet(widenlattice(lattice), v, t)
end

function tmeet(𝕃::InterMustAliasesLattice, @nospecialize(v), @nospecialize(t::Type))
    if isa(v, InterMustAlias)
        v = widenmustalias(v)
    end
    return tmeet(widenlattice(𝕃), v, t)
end

function tmeet(lattice::OptimizerLattice, @nospecialize(v), @nospecialize(t::Type))
    # TODO: This can probably happen and should be handled
    @assert !isa(v, MaybeUndef)
    tmeet(widenlattice(lattice), v, t)
end

"""
    widenconst(x) -> t::Type

Widens extended lattice element `x` to native `Type` representation.
"""
widenconst(::AnyConditional) = Bool
widenconst(a::AnyMustAlias) = widenconst(widenmustalias(a))
widenconst(c::Const) = (v = c.val; isa(v, Type) ? Type{v} : typeof(v))
widenconst(m::MaybeUndef) = widenconst(m.typ)
widenconst(::PartialTypeVar) = TypeVar
widenconst(t::PartialStruct) = t.typ
widenconst(t::PartialOpaque) = t.typ
widenconst(t::Type) = t
widenconst(::TypeVar) = error("unhandled TypeVar")
widenconst(::TypeofVararg) = error("unhandled Vararg")
widenconst(::LimitedAccuracy) = error("unhandled LimitedAccuracy")

####################
# state management #
####################

issubstate(lattice::AbstractLattice, a::VarState, b::VarState) =
    ⊑(lattice, a.typ, b.typ) && a.undef <= b.undef

function smerge(lattice::AbstractLattice, sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    sa === sb && return sa
    sa === NOT_FOUND && return sb
    sb === NOT_FOUND && return sa
    issubstate(lattice, sa, sb) && return sb
    issubstate(lattice, sb, sa) && return sa
    return VarState(tmerge(lattice, sa.typ, sb.typ), sa.undef | sb.undef)
end

@inline tchanged(lattice::AbstractLattice, @nospecialize(n), @nospecialize(o)) =
    o === NOT_FOUND || (n !== NOT_FOUND && !⊑(lattice, n, o))
@inline schanged(lattice::AbstractLattice, @nospecialize(n), @nospecialize(o)) =
    (n !== o) && (o === NOT_FOUND || (n !== NOT_FOUND && !issubstate(lattice, n::VarState, o::VarState)))

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

function stupdate!(lattice::AbstractLattice, state::VarTable, changes::StateUpdate)
    changed = false
    changeid = slot_id(changes.var)
    for i = 1:length(state)
        if i == changeid
            newtype = changes.vtype
        else
            newtype = changes.state[i]
        end
        invalidated = invalidate_slotwrapper(newtype, changeid, changes.conditional)
        if invalidated !== nothing
            newtype = invalidated
        end
        oldtype = state[i]
        if schanged(lattice, newtype, oldtype)
            state[i] = smerge(lattice, oldtype, newtype)
            changed = true
        end
    end
    return changed
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

function stupdate1!(lattice::AbstractLattice, state::VarTable, change::StateUpdate)
    changeid = slot_id(change.var)
    for i = 1:length(state)
        invalidated = invalidate_slotwrapper(state[i], changeid, change.conditional)
        if invalidated !== nothing
            state[i] = invalidated
        end
    end
    # and update the type of it
    newtype = change.vtype
    oldtype = state[changeid]
    if schanged(lattice, newtype, oldtype)
        state[changeid] = smerge(lattice, oldtype, newtype)
        return true
    end
    return false
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
