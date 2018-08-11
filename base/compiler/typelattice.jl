# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# structs/constants #
#####################


# The type of a value might be constant
struct Const
    val
    actual::Bool  # if true, we obtained `val` by actually calling a @pure function
    Const(@nospecialize(v)) = new(v, false)
    Const(@nospecialize(v), a::Bool) = new(v, a)
end

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

struct NotFound end

const NOT_FOUND = NotFound()

#################
# lattice logic #
#################

function issubconditional(a::Conditional, b::Conditional)
    avar = a.var
    bvar = b.var
    if (isa(avar, Slot) && isa(bvar, Slot) && slot_id(avar) === slot_id(bvar)) ||
       (isa(avar, SSAValue) && isa(bvar, SSAValue) && avar === bvar)
        if a.vtype ⊑ b.vtype
            if a.elsetype ⊑ b.elsetype
                return true
            end
        end
    end
    return false
end

maybe_extract_const_bool(c::Const) = isa(c.val, Bool) ? c.val : nothing
function maybe_extract_const_bool(c::Conditional)
    (c.vtype === Bottom && !(c.elsetype === Bottom)) && return false
    (c.elsetype === Bottom && !(c.vtype === Bottom)) && return true
    nothing
end
maybe_extract_const_bool(@nospecialize c) = nothing

function ⊑(@nospecialize(a), @nospecialize(b))
    if isa(a, MaybeUndef) && !isa(b, MaybeUndef)
        return false
    end
    isa(a, MaybeUndef) && (a = a.typ)
    isa(b, MaybeUndef) && (b = b.typ)
    (a === NOT_FOUND || b === Any) && return true
    (a === Any || b === NOT_FOUND) && return false
    a === Union{} && return true
    b === Union{} && return false
    if isa(a, Conditional)
        if isa(b, Conditional)
            return issubconditional(a, b)
        elseif isa(b, Const) && isa(b.val, Bool)
            return maybe_extract_const_bool(a) === b.val
        end
        a = Bool
    elseif isa(b, Conditional)
        return a === Bottom
    end
    if isa(a, Const)
        if isa(b, Const)
            return a.val === b.val
        end
        return isa(a.val, widenconst(b))
    elseif isa(b, Const)
        if isa(a, DataType) && isdefined(a, :instance)
            return a.instance === b.val
        end
        return a === Bottom
    elseif !(isa(a, Type) || isa(a, TypeVar)) ||
           !(isa(b, Type) || isa(b, TypeVar))
        return a === b
    else
        return a <: b
    end
end

widenconst(c::Conditional) = Bool
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
widenconst(@nospecialize(t)) = t

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

function widenconditional(typ::Conditional)
    if typ.vtype == Union{}
        return Const(false)
    elseif typ.elsetype == Union{}
        return Const(true)
    else
        return Bool
    end
end

function stupdate!(state::Tuple{}, changes::StateUpdate)
    newst = copy(changes.state)
    if isa(changes.var, Slot)
        changeid = slot_id(changes.var::Slot)
        newst[changeid] = changes.vtype
        # remove any Conditional for this Slot from the vtable
        for i = 1:length(newst)
            newtype = newst[i]
            if isa(newtype, VarState)
                newtypetyp = newtype.typ
                if isa(newtypetyp, Conditional) && slot_id(newtypetyp.var) == changeid
                    newst[i] = VarState(widenconditional(newtypetyp), newtype.undef)
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
    newstate = false
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
            newtypetyp = newtype.typ
            if isa(newtypetyp, Conditional) && slot_id(newtypetyp.var) == changeid
                newtype = VarState(widenconditional(newtypetyp), newtype.undef)
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
    newstate = false
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

stupdate!(state::Tuple{}, changes::VarTable) = copy(changes)

stupdate!(state::Tuple{}, changes::Tuple{}) = false

function stupdate1!(state::VarTable, change::StateUpdate)
    if !isa(change.var, Slot)
        return false
    end
    changeid = slot_id(change.var::Slot)
    # remove any Conditional for this Slot from the catch block vtable
    for i = 1:length(state)
        oldtype = state[i]
        if isa(oldtype, VarState)
            oldtypetyp = oldtype.typ
            if isa(oldtypetyp, Conditional) && slot_id(oldtypetyp.var) == changeid
                state[i] = VarState(widenconditional(oldtypetyp), oldtype.undef)
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
