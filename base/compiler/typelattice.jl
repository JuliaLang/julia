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
function PartialStruct(typ::DataType, fields::Vector{Any})
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
Conditional(cnd::InterConditional) = Conditinal(cnd.slot, cnd.thentype, cnd.elsetype)
InterConditional(cnd::Conditional) = InterConditional(cnd.slot, cnd.thentype, cnd.elsetype)

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

# Represent that the type estimate has been approximated, due to "causes"
# (only used in abstract interpretion, doesn't appear in optimization)
# N.B. in the lattice, this is epsilon smaller than `typ` (except Union{})
struct LimitedAccuracy
    typ
    causes::IdSet{InferenceState}
    function LimitedAccuracy(@nospecialize(typ), causes::IdSet{InferenceState})
        @assert !isa(typ, LimitedAccuracy) "found nested LimitedAccuracy"
        return new(typ, causes)
    end
end

"""
    struct NotFound end
    const NOT_FOUND = NotFound()

A special sigleton that represents a variable has not been analyzed yet.
Particularly, all SSA value types are initialized as `NOT_FOUND` when creating a new `InferenceState`.
Note that this is only used for `smerge`, which updates abstract state `VarTable`,
and thus we don't define the lattice for this.
"""
struct NotFound end

const NOT_FOUND = NotFound()

const CompilerTypes = Union{MaybeUndef, Const, Conditional, NotFound, PartialStruct}
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
    return t
end

widenslotwrapper(@nospecialize typ) = typ
widenslotwrapper(typ::AnyConditional) = widenconditional(typ)
widenwrappedslotwrapper(@nospecialize typ) = widenslotwrapper(typ)
widenwrappedslotwrapper(typ::LimitedAccuracy) = LimitedAccuracy(widenslotwrapper(typ.typ), typ.causes)

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
    end
    return typ
end
widenconditional(::LimitedAccuracy) = error("unhandled LimitedAccuracy")
widenwrappedconditional(@nospecialize typ) = widenconditional(typ)
widenwrappedconditional(typ::LimitedAccuracy) = LimitedAccuracy(widenconditional(typ.typ), typ.causes)

# `Conditional` and `InterConditional` are valid in opposite contexts
# (i.e. local inference and inter-procedural call), as such they will never be compared
function issubconditional(a::C, b::C) where {C<:AnyConditional}
    if is_same_conditionals(a, b)
        if a.thentype ⊑ b.thentype
            if a.elsetype ⊑ b.elsetype
                return true
            end
        end
    end
    return false
end

is_same_conditionals(a::C, b::C) where C<:AnyConditional = a.slot == b.slot

is_lattice_bool(@nospecialize(typ)) = typ !== Bottom && typ ⊑ Bool

maybe_extract_const_bool(c::Const) = (val = c.val; isa(val, Bool)) ? val : nothing
function maybe_extract_const_bool(c::AnyConditional)
    (c.thentype === Bottom && !(c.elsetype === Bottom)) && return false
    (c.elsetype === Bottom && !(c.thentype === Bottom)) && return true
    nothing
end
maybe_extract_const_bool(@nospecialize c) = nothing

# LimitedAccuracy
# ===============

ignorelimited(@nospecialize typ) = typ
ignorelimited(typ::LimitedAccuracy) = typ.typ

# lattice order
# =============

"""
    a ⊑ b -> Bool

The non-strict partial order over the type inference lattice.
"""
@nospecialize(a) ⊑ @nospecialize(b) = begin
    if isa(b, LimitedAccuracy)
        if !isa(a, LimitedAccuracy)
            return false
        end
        if b.causes ⊈ a.causes
            return false
        end
        b = b.typ
    end
    isa(a, LimitedAccuracy) && (a = a.typ)
    if isa(a, MaybeUndef) && !isa(b, MaybeUndef)
        return false
    end
    isa(a, MaybeUndef) && (a = a.typ)
    isa(b, MaybeUndef) && (b = b.typ)
    b === Any && return true
    a === Any && return false
    a === Union{} && return true
    b === Union{} && return false
    @assert !isa(a, TypeVar) "invalid lattice item"
    @assert !isa(b, TypeVar) "invalid lattice item"
    if isa(a, AnyConditional)
        if isa(b, AnyConditional)
            return issubconditional(a, b)
        elseif isa(b, Const) && isa(b.val, Bool)
            return maybe_extract_const_bool(a) === b.val
        end
        a = Bool
    elseif isa(b, AnyConditional)
        return false
    end
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
                ⊑(af, bf) || return false
            end
            return true
        end
        return isa(b, Type) && a.typ <: b
    elseif isa(b, PartialStruct)
        if isa(a, Const)
            nf = nfields(a.val)
            nf == length(b.fields) || return false
            widenconst(b).name === widenconst(a).name || return false
            # We can skip the subtype check if b is a Tuple, since in that
            # case, the ⊑ of the elements is sufficient.
            if b.typ.name !== Tuple.name && !(widenconst(a) <: widenconst(b))
                return false
            end
            for i in 1:nf
                isdefined(a.val, i) || continue # since ∀ T Union{} ⊑ T
                bf = b.fields[i]
                if i == nf
                    bf = unwrapva(bf)
                end
                ⊑(Const(getfield(a.val, i)), bf) || return false
            end
            return true
        end
        return false
    end
    if isa(a, PartialOpaque)
        if isa(b, PartialOpaque)
            (a.parent === b.parent && a.source === b.source) || return false
            return (widenconst(a) <: widenconst(b)) &&
                ⊑(a.env, b.env)
        end
        return widenconst(a) ⊑ b
    elseif isa(b, PartialOpaque)
        return false
    end
    if isa(a, Const)
        if isa(b, Const)
            return a.val === b.val
        end
        # TODO: `b` could potentially be a `PartialTypeVar` here, in which case we might be
        # able to return `true` in more cases; in the meantime, just returning this is the
        # most conservative option.
        return isa(b, Type) && isa(a.val, b)
    elseif isa(b, Const)
        if isa(a, DataType) && isdefined(a, :instance)
            return a.instance === b.val
        end
        return false
    elseif isa(a, PartialTypeVar)
        return b === TypeVar || a === b
    elseif isa(b, PartialTypeVar)
        return a === b
    else
        return a <: b
    end
end

"""
    a ⊏ b -> Bool

The strict partial order over the type inference lattice.
This is defined as the irreflexive kernel of `⊑`.
"""
@nospecialize(a) ⊏ @nospecialize(b) = a ⊑ b && !⊑(b, a)

"""
    a ⋤ b -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `a ⊑ b` holds.
"""
@nospecialize(a) ⋤ @nospecialize(b) = !⊑(b, a)

"""
    is_lattice_equal(a, b) -> Bool

Check if two lattice elements are partial order equivalent.
This is basically `a ⊑ b && b ⊑ a` but with extra performance optimizations.
"""
function is_lattice_equal(@nospecialize(a), @nospecialize(b))
    a === b && return true
    if isa(a, PartialStruct)
        isa(b, PartialStruct) || return false
        length(a.fields) == length(b.fields) || return false
        widenconst(a) == widenconst(b) || return false
        for i in 1:length(a.fields)
            is_lattice_equal(a.fields[i], b.fields[i]) || return false
        end
        return true
    end
    isa(b, PartialStruct) && return false
    if a isa Const
        if issingletontype(b)
            return a.val === b.instance
        end
        return false
    end
    if b isa Const
        if issingletontype(a)
            return a.instance === b.val
        end
        return false
    end
    if isa(a, PartialOpaque)
        isa(b, PartialOpaque) || return false
        widenconst(a) == widenconst(b) || return false
        a.source === b.source || return false
        a.parent === b.parent || return false
        return is_lattice_equal(a.env, b.env)
    end
    return a ⊑ b && b ⊑ a
end

# lattice operations
# ==================

"""
    tmeet(v, t::Type) -> x

Computes typeintersect over the extended inference lattice, as precisely as we can,
where `v` is in the extended lattice, and `t` is a `Type`.
"""
function tmeet(@nospecialize(v), @nospecialize(t::Type))
    if isa(v, Const)
        if !has_free_typevars(t) && !isa(v.val, t)
            return Bottom
        end
        return v
    elseif isa(v, PartialStruct)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        if widev <: t
            return v
        end
        ti = typeintersect(widev, t)
        valid_as_lattice(ti) || return Bottom
        @assert widev <: Tuple
        new_fields = Vector{Any}(undef, length(v.fields))
        for i = 1:length(new_fields)
            vfi = v.fields[i]
            if isvarargtype(vfi)
                new_fields[i] = vfi
            else
                new_fields[i] = tmeet(vfi, widenconst(getfield_tfunc(t, Const(i))))
                if new_fields[i] === Bottom
                    return Bottom
                end
            end
        end
        return tuple_tfunc(new_fields)
    elseif isa(v, PartialOpaque)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        if widev <: t
            return v
        end
        ti = typeintersect(widev, t)
        valid_as_lattice(ti) || return Bottom
        return PartialOpaque(ti, v.env, v.parent, v.source)
    elseif isa(v, Conditional)
        if !(Bool <: t)
            return Bottom
        end
        return v
    end
    ti = typeintersect(widenconst(v), t)
    valid_as_lattice(ti) || return Bottom
    return ti
end

"""
    widenconst(x) -> t::Type

Widens extended lattice element `x` to native `Type` representation.
"""
widenconst(::AnyConditional) = Bool
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

issubstate(a::VarState, b::VarState) = (a.typ ⊑ b.typ && a.undef <= b.undef)

function smerge(sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    sa === sb && return sa
    sa === NOT_FOUND && return sb
    sb === NOT_FOUND && return sa
    issubstate(sa, sb) && return sb
    issubstate(sb, sa) && return sa
    return VarState(tmerge(sa.typ, sb.typ), sa.undef | sb.undef)
end

@inline tchanged(@nospecialize(n), @nospecialize(o)) = o === NOT_FOUND || (n !== NOT_FOUND && !(n ⊑ o))
@inline schanged(@nospecialize(n), @nospecialize(o)) = (n !== o) && (o === NOT_FOUND || (n !== NOT_FOUND && !issubstate(n::VarState, o::VarState)))

# remove any lattice elements that wrap the reassigned slot object from the vartable
function invalidate_slotwrapper(vt::VarState, changeid::Int, ignore_conditional::Bool)
    newtyp = ignorelimited(vt.typ)
    if (!ignore_conditional && isa(newtyp, Conditional) && newtyp.slot == changeid)
        newtyp = widenwrappedslotwrapper(vt.typ)
        return VarState(newtyp, vt.undef)
    end
    return nothing
end

function stupdate!(state::VarTable, changes::StateUpdate)
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
        if schanged(newtype, oldtype)
            state[i] = smerge(oldtype, newtype)
            changed = true
        end
    end
    return changed
end

function stupdate!(state::VarTable, changes::VarTable)
    changed = false
    for i = 1:length(state)
        newtype = changes[i]
        oldtype = state[i]
        if schanged(newtype, oldtype)
            state[i] = smerge(oldtype, newtype)
            changed = true
        end
    end
    return changed
end

function stupdate1!(state::VarTable, change::StateUpdate)
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
    if schanged(newtype, oldtype)
        state[changeid] = smerge(oldtype, newtype)
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
