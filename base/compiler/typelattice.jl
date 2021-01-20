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

# The type of this value might be Bool.
# However, to enable a limited amount of back-propagagation,
# we also keep some information about how this Bool value was created.
# In particular, if you branch on this value, then may assume that in
# the true branch, the type of `var` will be limited by `vtype` and in
# the false branch, it will be limited by `elsetype`. Example:
# ```
# cond = isa(x::Union{Int, Float}, Int)::Conditional(x, Int, Float)
# if cond
#    # May assume x is `Int` now
# else
#    # May assume x is `Float` now
# end
# ```
struct Conditional
    var::Slot
    vtype
    elsetype
    function Conditional(
                var,
                @nospecialize(vtype),
                @nospecialize(nottype))
        return new(var, vtype, nottype)
    end
end

# # Similar to `Conditional`, but conveys inter-procedural constraints imposed on call arguments.
# # This is separate from `Conditional` to catch logic errors: the lattice element name is InterConditional
# # while processing a call, then Conditional everywhere else. Thus InterConditional does not appear in
# # CompilerTypes—these type's usages are disjoint—though we define the lattice for InterConditional.
# struct InterConditional
#     slot::Int
#     vtype
#     elsetype
# end
import Core: InterConditional
const AnyConditional = Union{Conditional,InterConditional}

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

# The type of a variable load is either a value or an UndefVarError
# (only used in abstractinterpret, doesn't appear in optimize)
struct VarState
    typ
    undef::Bool
    VarState(@nospecialize(typ), undef::Bool) = new(typ, undef)
end

const VarTable = Array{Any,1}

struct StateUpdate
    var::Union{Slot,SSAValue}
    vtype::VarState
    state::VarTable
end

# Represent that the type estimate has been approximated, due to "causes"
# (only used in abstractinterpret, doesn't appear in optimize)
# N.B. in the lattice, this is epsilon smaller than `typ` (except Union{})
struct LimitedAccuracy
    typ
    causes::IdSet{InferenceState}
    LimitedAccuracy(@nospecialize(typ), causes::IdSet{InferenceState}) =
        new(typ, causes)
end

struct NotFound end

const NOT_FOUND = NotFound()

const CompilerTypes = Union{MaybeUndef, Const, Conditional, NotFound, PartialStruct}
==(x::CompilerTypes, y::CompilerTypes) = x === y
==(x::Type, y::CompilerTypes) = false
==(x::CompilerTypes, y::Type) = false

#################
# lattice logic #
#################

# `Conditional` and `InterConditional` are valid in opposite contexts
# (i.e. local inference and inter-procedural call), as such they will never be compared
function issubconditional(a::C, b::C) where {C<:AnyConditional}
    if is_same_conditionals(a, b)
        if a.vtype ⊑ b.vtype
            if a.elsetype ⊑ b.elsetype
                return true
            end
        end
    end
    return false
end

is_same_conditionals(a::Conditional,      b::Conditional)      = slot_id(a.var) === slot_id(b.var)
is_same_conditionals(a::InterConditional, b::InterConditional) = a.slot === b.slot

is_lattice_bool(@nospecialize(typ)) = typ !== Bottom && typ ⊑ Bool

maybe_extract_const_bool(c::Const) = (val = c.val; isa(val, Bool)) ? val : nothing
function maybe_extract_const_bool(c::AnyConditional)
    (c.vtype === Bottom && !(c.elsetype === Bottom)) && return false
    (c.elsetype === Bottom && !(c.vtype === Bottom)) && return true
    nothing
end
maybe_extract_const_bool(@nospecialize c) = nothing

function ⊑(@nospecialize(a), @nospecialize(b))
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
                # XXX: let's handle varargs later
                ⊑(a.fields[i], b.fields[i]) || return false
            end
            return true
        end
        return isa(b, Type) && a.typ <: b
    elseif isa(b, PartialStruct)
        if isa(a, Const)
            nfields(a.val) == length(b.fields) || return false
            widenconst(b).name === widenconst(a).name || return false
            # We can skip the subtype check if b is a Tuple, since in that
            # case, the ⊑ of the elements is sufficient.
            if b.typ.name !== Tuple.name && !(widenconst(a) <: widenconst(b))
                return false
            end
            for i in 1:nfields(a.val)
                # XXX: let's handle varargs later
                isdefined(a.val, i) || return false
                ⊑(Const(getfield(a.val, i)), b.fields[i]) || return false
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
    elseif isa(a, PartialTypeVar) && b === TypeVar
        return true
    elseif isa(a, Type) && isa(b, Type)
        return a <: b
    else # handle this conservatively in the remaining cases
        return a === b
    end
end

# Check if two lattice elements are partial order equivalent. This is basically
# `a ⊑ b && b ⊑ a` but with extra performance optimizations.
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

widenconst(c::AnyConditional) = Bool
function widenconst(c::Const)
    if isa(c.val, Type)
        if isvarargtype(c.val)
            return Type
        end
        return Type{c.val}
    else
        return typeof(c.val)
    end
end
widenconst(m::MaybeUndef) = widenconst(m.typ)
widenconst(c::PartialTypeVar) = TypeVar
widenconst(t::PartialStruct) = t.typ
widenconst(t::PartialOpaque) = t.typ
widenconst(t::Type) = t
widenconst(t::TypeVar) = t
widenconst(t::Core.TypeofVararg) = t
widenconst(t::LimitedAccuracy) = error("unhandled LimitedAccuracy")

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
@inline schanged(@nospecialize(n), @nospecialize(o)) = (n !== o) && (o === NOT_FOUND || (n !== NOT_FOUND && !issubstate(n, o)))

widenconditional(@nospecialize typ) = typ
function widenconditional(typ::AnyConditional)
    if typ.vtype === Union{}
        return Const(false)
    elseif typ.elsetype === Union{}
        return Const(true)
    else
        return Bool
    end
end
widenconditional(t::LimitedAccuracy) = error("unhandled LimitedAccuracy")

ignorelimited(@nospecialize typ) = typ
ignorelimited(typ::LimitedAccuracy) = typ.typ

function stupdate!(state::Nothing, changes::StateUpdate)
    newst = copy(changes.state)
    if isa(changes.var, Slot)
        changeid = slot_id(changes.var::Slot)
        newst[changeid] = changes.vtype
        # remove any Conditional for this Slot from the vtable
        for i = 1:length(newst)
            newtype = newst[i]
            if isa(newtype, VarState)
                newtypetyp = ignorelimited(newtype.typ)
                if isa(newtypetyp, Conditional) && slot_id(newtypetyp.var) == changeid
                    newtypetyp = widenconditional(newtypetyp)
                    if newtype.typ isa LimitedAccuracy
                        newtypetyp = LimitedAccuracy(newtypetyp, newtype.typ.causes)
                    end
                    newst[i] = VarState(newtypetyp, newtype.undef)
                end
            end
        end
    end
    return newst
end

function stupdate!(state::VarTable, changes::StateUpdate)
    if !isa(changes.var, Slot)
        return stupdate!(state, changes.state)
    end
    newstate = nothing
    changeid = slot_id(changes.var::Slot)
    for i = 1:length(state)
        if i == changeid
            newtype = changes.vtype
        else
            newtype = changes.state[i]
        end
        oldtype = state[i]
        # remove any Conditional for this Slot from the vtable
        if isa(newtype, VarState)
            newtypetyp = ignorelimited(newtype.typ)
            if isa(newtypetyp, Conditional) && slot_id(newtypetyp.var) == changeid
                newtypetyp = widenconditional(newtypetyp)
                if newtype.typ isa LimitedAccuracy
                    newtypetyp = LimitedAccuracy(newtypetyp, newtype.typ.causes)
                end
                newtype = VarState(newtypetyp, newtype.undef)
            end
        end
        if schanged(newtype, oldtype)
            newstate = state
            state[i] = smerge(oldtype, newtype)
        end
    end
    return newstate
end

function stupdate!(state::VarTable, changes::VarTable)
    newstate = nothing
    for i = 1:length(state)
        newtype = changes[i]
        oldtype = state[i]
        if schanged(newtype, oldtype)
            newstate = state
            state[i] = smerge(oldtype, newtype)
        end
    end
    return newstate
end

stupdate!(state::Nothing, changes::VarTable) = copy(changes)

stupdate!(state::Nothing, changes::Nothing) = nothing

function stupdate1!(state::VarTable, change::StateUpdate)
    if !isa(change.var, Slot)
        return false
    end
    changeid = slot_id(change.var::Slot)
    # remove any Conditional for this Slot from the catch block vtable
    for i = 1:length(state)
        oldtype = state[i]
        if isa(oldtype, VarState)
            oldtypetyp = ignorelimited(oldtype.typ)
            if isa(oldtypetyp, Conditional) && slot_id(oldtypetyp.var) == changeid
                oldtypetyp = widenconditional(oldtypetyp)
                if oldtype.typ isa LimitedAccuracy
                    oldtypetyp = LimitedAccuracy(oldtypetyp, oldtype.typ.causes)
                end
                state[i] = VarState(oldtypetyp, oldtype.undef)
            end
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
