# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# structs/constants #
#####################

const __NULL_CONSTANT__ = :__NULL_CONSTANT__

"""
    x::LatticeElement

The lattice for Julia's native type inference implementation.
`LatticeElement` has following lattice properties and these attributes are combined to create
a partial lattice whose height is infinite.

---
- `x.constant ::Any (constant value) or ::PartialFields (partial constant information)` \\
  When `isConst(x)`, it means `x` is constant-folded and `x.constant` holds actual constant value.
  Note that it is valid if `x` has other lattice properties even when it is constant-folded.
  For example, `x` may have "interesting" `x.conditional` property when `isConst(x)`.

  See also:
  - constructor: `Const(val)`
  - property query: `isConst(x)`
  - value retrieval: `constant(x)`

  If `isPartialStruct(x)`, `x.constant` keeps field information about a partially constant-folded `struct`.
  When fields of a `struct` are fully known we just constant-fold it, but even when some of
  the fields can not be folded, inference will try to keep partial constant information of
  folded fields in `x.constant::PartialFields`.
  Note that this property assumes the following invariants:
  - `immutabletype(x.typ)`: since inference does not reason about memory-effects of object fields
  - `x.typ` is concrete or `Tuple` type: the lattice assumes `Const ⊑ PartialStruct ⊑ concrete type ⊑ abstract type`

  See also:
  - constructor: `PartialStruct(typ, fields::PartialFields)`
  - property query: `isPartialStruct(x)`
  - value retrieval: `partialfields(x)`

---
- `x.conditional :: ConditionalInfo` \\
  The lattice property that comes along with `Bool`.
  It keeps some information about how this `Bool` value was created in order to enable a
  limited amount of type constraint back-propagation.
  In particular, if we branch on an object that has this property `cnd::ConditionalInfo`,
  then we may assume that in the "then" branch, the type of `SlotNumber(cnd.slot_id)` will be
  limited by `cnd.vtype` and in the "else" branch, it will be limited by `cnd.elsetype`.
  By default, this property is initialized as `__NULL_CONDITIONAL__`, which does not convey
  any useful information (and thus should never be used).
  Example:
  ```
  cond = isa(x::Union{Int, String}, Int) # ::Conditional(:(x), Int, String)
  if cond
      ... # x::Int
  else
      ... # x::String
  end
  ```

  The boolean flag `x.conditional.inter` indicates it conveys inter-procedural constraints
  imposed on call arguments.
  This flag is raised to catch logic errors: `inter` should be `true` while processing a call,
  then should be `false` elsewhere else.
  Thus `ConditionalInfo`s with different `inter` flags should not appear in the same context
  -- their usages are disjoint -- though we define the lattice for both cases (but separately).

  See also:
  - constructor: `Conditional(slot_id::Int, vtype, elsetype, inter::Bool)`
  - property query: `isConditional(x)` / `isInterConditional(x)` / `isAnyConditional(x)`
  - property retrieval: `conditional(x)` / `interconditional(x)`
  - property widening: `widenconditional(x)`

---
- `x.special :: Union{PartialTypeVarInfo, PartialOpaque}` \\
  `x.special::PartialTypeVarInfo` tracks an identity of `TypeVar` so that `x` can produce
  better inference for `UnionAll` construction.
  `x.special::PartialOpaque` holds opaque closure information.
  By default `x.special` is initialized with `nothing` (no information).

  See also:
  - constructor: `PartialTypeVar(::TypeVar, lb_certain::Bool, ub_certain::Bool)` / `mkPartialOpaque`
  - property query: `isPartialTypeVar(x)` / `isPartialOpaque`
  - property retrieval: `partialtypevar(x)` / `partialopaque(x)`

---
- `x.causes :: IdSet{InferenceState}` \\
  If not empty, it indicates the `x` has been approximated due to the "causes".
  This attribute is only used in abstract interpretation, and not in optimization.
  N.B. in the lattice, `x` is epsilon smaller than `ignorelimited(x)` (except `⊥`)

  See also:
  - constructor: `LimitedAccuracy(::LatticeElement, ::IdSet{InferenceState})`
  - property query: `isLimitedAccuracy(x)`
  - property widening: `ignorelimited(x)`
  - property retrieval: `causes(x)`

---
- `x.maybeundef :: Bool` \\
  Indicates that this variable may be undefined at this point.
  This attribute is only used in optimization, and not in abstract interpretation.
  N.B. in the lattice, `x` is epsilon bigger than `ignoremaybeundef(x)`.

  See also:
  - constructor: `MaybeUndef(::LatticeElement)`
  - property query: `isMaybeUndef(x)`
  - property widening: `ignoremaybeundef(x)`

---
"""
struct LatticeElement
    typ # ::Type

    constant # ::Any (constant value) or ::PartialFields

    # some special information for this lattice element
    special # ::Union{ConditionalInfo, PartialTypeVarInfo, PartialOpaque, Core.TypeofVararg} or nothing

    # abstract interpretation specific attributes
    causes # ::IdSet{InferenceState} or nothing

    # optimization specific specific attributes
    maybeundef::Bool

    function LatticeElement(typ,
        constant = __NULL_CONSTANT__, special = nothing;
        causes#=::IdSet{InferenceState}=# = nothing, maybeundef::Bool = false)
        @nospecialize typ constant special
        if isvarargtype(typ)
            @assert special === nothing
            special = typ
            typ = Any # COMBAK (lattice overhaul) what `typ` should this have ?
        end
        @assert isa(typ, Type)
        return new(typ,
                   constant,
                   special,
                   causes,
                   maybeundef,
                   )
    end
    function LatticeElement(x::LatticeElement,
        constant = x.constant, special = x.special;
        causes#=::IdSet{InferenceState}=# = x.causes::Union{Nothing,IdSet{InferenceState}},
        maybeundef::Bool = x.maybeundef)
        @nospecialize constant special
        return new(x.typ,
                   constant,
                   special,
                   causes,
                   maybeundef,
                   )
    end
end

NativeType(@nospecialize typ) = typ === Any ? ⊤ : typ === Bottom ? ⊥ : LatticeElement(typ)
isNativeType(@nospecialize typ) = true
isNativeType(typ::LatticeElement) = (
    typ.constant === __NULL_CONSTANT__ &&
    typ.special === nothing &&
    typ.causes === nothing &&
    typ.maybeundef === false &&
    true)

# NOTE once we pack all extended lattice types into `LatticeElement`, we don't need this `unwraptype`:
# - `unwraptype`: unwrap `NativeType` to native Julia type
# - `widenconst`: unwrap any extended type lattice to native Julia type
unwraptype(@nospecialize typ) = typ
unwraptype(typ::LatticeElement) = isNativeType(typ) ? typ.typ::Type : typ

function Const(@nospecialize constant)
    typ = isa(constant, Type) ? Type{constant} : typeof(constant)
    return LatticeElement(typ, constant)
end
isConst(@nospecialize typ) = false
isConst(typ::LatticeElement) = typ.constant !== __NULL_CONSTANT__ && !_isPartialStruct(typ)
constant(x::LatticeElement) = x.constant

const PartialFields = Vector{LatticeElement}
function PartialStruct(@nospecialize(typ), fields::PartialFields)
    @assert (isconcretetype(typ) || istupletype(typ)) && !ismutabletype(typ) "invalid PartialStruct typ"
    @assert !isempty(fields) "invalid PartialStruct fields"
    for i = 1:length(fields)
        @assert !isConditional(fields[i]) "invalid PartialStruct field"
    end
    return LatticeElement(typ, fields)
end
istupletype(@nospecialize typ) = isa(typ, DataType) && typ.name.name === :Tuple
isPartialStruct(@nospecialize typ) = false
isPartialStruct(typ::LatticeElement) = typ.constant !== __NULL_CONSTANT__ && _isPartialStruct(typ)
_isPartialStruct(typ::LatticeElement) = isa(typ.constant, PartialFields) && !(typ.typ <: PartialFields)
partialfields(x::LatticeElement) = x.constant::PartialFields

struct ConditionalInfo
    slot_id::Int
    vtype::LatticeElement
    elsetype::LatticeElement
    inter::Bool
    function ConditionalInfo(
        slot_id::Int, vtype::LatticeElement, elsetype::LatticeElement, inter::Bool)
        @assert !isAnyConditional(vtype) "nested Conditional vtype"
        @assert !isAnyConditional(elsetype) "nested Conditional elsetype"
        return new(slot_id, vtype, elsetype, inter)
    end
end
function _Conditional(slot_id::Int, vtype::LatticeElement, elsetype::LatticeElement, inter::Bool)
    if vtype === ⊥
        constant = false
    elseif elsetype === ⊥
        constant = true
    else
        constant = __NULL_CONSTANT__
    end
    conditional = ConditionalInfo(slot_id, vtype, elsetype, inter)
    return LatticeElement(Bool, constant, conditional)
end
Conditional(slot_id::Int, vtype::LatticeElement, elsetype::LatticeElement) =
    _Conditional(slot_id, vtype, elsetype, false)
InterConditional(slot_id::Int, vtype::LatticeElement, elsetype::LatticeElement) =
    _Conditional(slot_id, vtype, elsetype, true)
isConditional(@nospecialize typ) = false
isConditional(typ::LatticeElement) = (special = typ.special; isa(special, ConditionalInfo) && !special.inter)
isInterConditional(@nospecialize typ) = false
isInterConditional(typ::LatticeElement) = (special = typ.special; isa(special, ConditionalInfo) && special.inter)
isAnyConditional(@nospecialize typ) = false
isAnyConditional(typ::LatticeElement) = (special = typ.special; isa(special, ConditionalInfo))
@inline function conditional(typ::LatticeElement)
    conditional = typ.special::ConditionalInfo
    @assert !conditional.inter
    return conditional
end
@inline function interconditional(typ::LatticeElement)
    conditional = typ.special::ConditionalInfo
    @assert conditional.inter
    return conditional
end
widenconditional(typ::LatticeElement) = isAnyConditional(typ) ? LatticeElement(typ, typ.constant, nothing) : typ

struct PartialTypeVarInfo
    tv::TypeVar
    PartialTypeVarInfo(tv::TypeVar) = new(tv)
end
function PartialTypeVar(
    tv::TypeVar,
    # N.B.: Currently unused, but could be used to form something like `Constant`
    # if the bounds are pulled out of this `TypeVar`
    lb_certain::Bool, ub_certain::Bool)
    return LatticeElement(TypeVar, __NULL_CONSTANT__, PartialTypeVarInfo(tv))
end
isPartialTypeVar(@nospecialize typ) = false
isPartialTypeVar(typ::LatticeElement) = isa(typ.special, PartialTypeVarInfo)
@inline partialtypevar(typ::LatticeElement) = typ.special::PartialTypeVarInfo

function mkPartialOpaque(@nospecialize(typ), env::LatticeElement, isva::Bool, parent::MethodInstance, source::Method)
    return LatticeElement(typ, __NULL_CONSTANT__, PartialOpaque(typ, env, isva, parent, source))
end
isPartialOpaque(@nospecialize typ) = false
isPartialOpaque(typ::LatticeElement) = isa(typ.special, PartialOpaque)
@inline partialopaque(typ::LatticeElement) = typ.special::PartialOpaque

isVararg(@nospecialize typ) = false
isVararg(typ::LatticeElement) = isa(typ.special, TypeofVararg)
@inline vararg(typ::LatticeElement) = typ.special::TypeofVararg
unwrapva_𝑳(x::LatticeElement) = isVararg(x) ? NativeType(unwrapva(vararg(x))) : x

function LimitedAccuracy(x::LatticeElement, causes#=::IdSet{InferenceState}=#)
    causes = causes::IdSet{InferenceState}
    @assert !isLimitedAccuracy(x) "nested LimitedAccuracy"
    @assert !isempty(causes) "malformed LimitedAccuracy"
    return LatticeElement(x; causes)
end
isLimitedAccuracy(@nospecialize typ) = false
isLimitedAccuracy(typ::LatticeElement) = typ.causes !== nothing
ignorelimited(typ::LatticeElement) = isLimitedAccuracy(typ) ? _ignorelimited(typ) : typ
_ignorelimited(typ::LatticeElement) = LatticeElement(typ; causes = nothing)
@inline causes(typ::LatticeElement) = typ.causes::IdSet{InferenceState}

isMaybeUndef(@nospecialize typ) = false
isMaybeUndef(typ::LatticeElement) = typ.maybeundef
MaybeUndef(x::LatticeElement) = isMaybeUndef(x) ? x : LatticeElement(x; maybeundef = true)
ignoremaybeundef(typ::LatticeElement) = isMaybeUndef(typ) ? LatticeElement(typ; maybeundef = false) : typ

const ⊤, ⊥ = LatticeElement(Any), LatticeElement(Bottom)
const LBool, LInt, LNothing = NativeType(Bool), NativeType(Int), NativeType(Nothing)

# The type of a variable load is either a value or an UndefVarError
# (only used in abstractinterpret, doesn't appear in optimize)
struct VarState
    typ::LatticeElement
    undef::Bool
    VarState(typ::LatticeElement, undef::Bool) = new(typ, undef)
end

"""
    const VarTable = Vector{VarState}

The extended lattice that maps local variables to inferred type represented as `LatticeElement`.
Each index corresponds to the `id` of `SlotNumber` which identifies each local variable.
Note that `InferenceState` will maintain multiple `VarTable`s at each SSA statement
to enable flow-sensitive analysis.
"""
const VarTable = Vector{VarState}

struct StateUpdate
    var::SlotNumber
    vtype::VarState
    state::VarTable
    conditional::Bool
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

# the types of `(src::CodeInfo).ssavaluetypes` after `InferenceState` construction and until `ir_to_codeinf!(src)` is called
const SSAValueTypes = Vector{Any}
const SSAValueType  = Union{NotFound,LatticeElement} # element

# allow comparison with unwrapped types
# TODO (lattice overhaul) remove me, this is just for prototyping
x::Type == y::LatticeElement = x === unwraptype(y)
x::LatticeElement == y::Type = unwraptype(x) === y

#################
# lattice logic #
#################

# `Conditional` and `InterConditional` are valid in opposite contexts
# (i.e. local inference and inter-procedural call), as such they will never be compared
function issubconditional(a::LatticeElement, b::LatticeElement)
    a, b = a.special, b.special
    @assert a.inter === b.inter "invalid conditional lattice element comparison"
    if is_same_conditionals(a, b)
        if a.vtype ⊑ b.vtype
            if a.elsetype ⊑ b.elsetype
                return true
            end
        end
    end
    return false
end

is_same_conditionals(a::ConditionalInfo, b::ConditionalInfo) = a.slot_id == b.slot_id

function is_lattice_bool(typ::LatticeElement)
    ty = widenconst(typ)
    return ty !== Bottom && ty <: Bool
end

function maybe_extract_const_bool(x::LatticeElement)
    if isConst(x)
        val = constant(x)
        return isa(val, Bool) ? val : nothing
    end
    if isAnyConditional(x)
        cnd = x.special::ConditionalInfo
        (cnd.vtype === ⊥ && !(cnd.elsetype === ⊥)) && return false
        (cnd.elsetype === ⊥ && !(cnd.vtype === ⊥)) && return true
    end
    return nothing
end
maybe_extract_const_bool(@nospecialize c) = nothing

"""
    a ⊑ b -> Bool

The non-strict partial order over the type inference lattice.
"""
@nospecialize(a) ⊑ @nospecialize(b) = begin
    if isLimitedAccuracy(b)
        if !isLimitedAccuracy(a)
            return false
        end
        if causes(b) ⊈ causes(a)
            return false
        end
        b = _ignorelimited(b)
    end
    if isLimitedAccuracy(a)
        a = _ignorelimited(a)
    end
    if isMaybeUndef(a) && !isMaybeUndef(b)
        return false
    end
    a = unwraptype(a)
    b = unwraptype(b)
    b === Any && return true
    a === Any && return false
    a === Union{} && return true
    b === Union{} && return false
    @assert !isa(a, TypeVar) "invalid lattice item"
    @assert !isa(b, TypeVar) "invalid lattice item"
    if isAnyConditional(a)
        if isAnyConditional(b)
            return issubconditional(a, b)
        elseif isConst(b) && isa(constant(b), Bool)
            return maybe_extract_const_bool(a) === constant(b)
        end
        a = Bool
    elseif isAnyConditional(b)
        return false
    end
    if isPartialStruct(a)
        if isPartialStruct(b)
            afields, bfields = partialfields(a), partialfields(b)
            if !(length(afields) == length(bfields) && a.typ <: b.typ)
                return false
            end
            for i in 1:length(bfields)
                # XXX: let's handle varargs later
                ⊑(afields[i], bfields[i]) || return false
            end
            return true
        end
        return isa(b, Type) && a.typ <: b
    elseif isPartialStruct(b)
        if isConst(a)
            aval = constant(a)
            bfields = partialfields(b)
            nfields(aval) == length(bfields) || return false
            widenconst(b).name === widenconst(a).name || return false
            # We can skip the subtype check if b is a Tuple, since in that
            # case, the ⊑ of the elements is sufficient.
            if b.typ.name !== Tuple.name && !(widenconst(a) <: widenconst(b))
                return false
            end
            for i in 1:nfields(aval)
                # XXX: let's handle varargs later
                isdefined(aval, i) || return false
                ⊑(Const(getfield(aval, i)), bfields[i]) || return false
            end
            return true
        end
        return false
    end
    if isPartialOpaque(a)
        if isPartialOpaque(b)
            a, b = partialopaque(a), partialopaque(b)
            (a.parent === b.parent && a.source === b.source) || return false
            return (a.typ <: b.typ) && ⊑(a.env, b.env)
        end
        return widenconst(a) ⊑ b
    end
    if isConst(a)
        aval = constant(a)
        if isConst(b)
            return aval === constant(b)
        end
        # TODO: `b` could potentially be a `PartialTypeVar` here, in which case we might be
        # able to return `true` in more cases; in the meantime, just returning this is the
        # most conservative option.
        return isa(b, Type) && isa(aval, b)
    elseif isConst(b)
        if isa(a, DataType) && isdefined(a, :instance)
            return a.instance === constant(b)
        end
        return false
    elseif isPartialTypeVar(a) && b === TypeVar
        return true
    elseif isa(a, Type) && isa(b, Type)
        return a <: b
    else # handle this conservatively in the remaining cases
        return a === b
    end
end

"""
    a::LatticeElement ⊏ b::LatticeElement -> Bool

The strict partial order over the type inference lattice.
This is defined as the irreflexive kernel of `⊑`.
"""
a::LatticeElement ⊏ b::LatticeElement = a ⊑ b && !⊑(b, a)

"""
    a::LatticeElement ⋤ b::LatticeElement -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `a ⊑ b` holds.
"""
a::LatticeElement ⋤ b::LatticeElement = !⊑(b, a)

a::LatticeElement ⊑ₜ @nospecialize(b#=::Type=#) = widenconst(a) <: b
a::LatticeElement ⊏ₜ @nospecialize(b#=::Type=#) = widenconst(a) <: b && !(b <: widenconst(a))
a::LatticeElement ⋤ₜ @nospecialize(b#=::Type=#) = !(b <: widenconst(a))

# Check if two lattice elements are partial order equivalent. This is basically
# `a ⊑ b && b ⊑ a` but with extra performance optimizations.
function is_lattice_equal(a::LatticeElement, b::LatticeElement)
    a === b && return true
    if isPartialStruct(a)
        isPartialStruct(b) || return false
        widenconst(a) == widenconst(b) || return false
        afields, bfields = partialfields(a), partialfields(b)
        length(afields) == length(bfields) || return false
        for i in 1:length(afields)
            is_lattice_equal(afields[i], bfields[i]) || return false
        end
        return true
    end
    isPartialStruct(b) && return false
    if isConst(a)
        isConst(b) && return constant(a) === constant(b)
        b = widenconst(b)
        if issingletontype(b)
            return constant(a) === b.instance
        end
        return false
    end
    if isConst(b)
        a = widenconst(a)
        if issingletontype(a)
            return a.instance === constant(b)
        end
        return false
    end
    if isPartialOpaque(a)
        isPartialOpaque(b) || return false
        a, b = partialopaque(a), partialopaque(b)
        a.typ === b.typ || return false
        a.source === b.source || return false
        a.parent === b.parent || return false
        return is_lattice_equal(a.env, b.env)
    end
    return a ⊑ b && b ⊑ a
end

widenconst(x::LatticeElement) = (@assert !isVararg(x) "unhandled Vararg"; x.typ::Type)
widenconst(t::Type) = t

issubstate(a::VarState, b::VarState) = (a.typ ⊑ b.typ && a.undef <= b.undef)

function smerge(sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    sa === sb && return sa
    sa === NOT_FOUND && return sb
    sb === NOT_FOUND && return sa
    issubstate(sa, sb) && return sb
    issubstate(sb, sa) && return sa
    return VarState(sa.typ ⊔ sb.typ, sa.undef | sb.undef)
end

@inline schanged(@nospecialize(n), @nospecialize(o)) = (n !== o) && (o === NOT_FOUND || (n !== NOT_FOUND && !issubstate(n::VarState, o::VarState)))

function stupdate!(state::Nothing, changes::StateUpdate)
    newst = copy(changes.state)
    changeid = slot_id(changes.var)
    newst[changeid] = changes.vtype
    # remove any Conditional for this slot from the vtable
    # (unless this change is came from the conditional)
    if !changes.conditional
        for i = 1:length(newst)
            newtype = newst[i]
            if isa(newtype, VarState)
                newtypetyp = newtype.typ
                if isConditional(newtypetyp) && conditional(newtypetyp).slot_id == changeid
                    newst[i] = VarState(widenconditional(newtypetyp), newtype.undef)
                end
            end
        end
    end
    return newst
end

function stupdate!(state::VarTable, changes::StateUpdate)
    newstate = nothing
    changeid = slot_id(changes.var)
    for i = 1:length(state)
        if i == changeid
            newtype = changes.vtype
        else
            newtype = changes.state[i]
        end
        oldtype = state[i]
        # remove any Conditional for this slot from the vtable
        # (unless this change is came from the conditional)
        if !changes.conditional && isa(newtype, VarState)
            newtypetyp = newtype.typ
            if isConditional(newtypetyp) && conditional(newtypetyp).slot_id == changeid
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
    changeid = slot_id(change.var)
    # remove any Conditional for this slot from the catch block vtable
    # (unless this change is came from the conditional)
    if !change.conditional
        for i = 1:length(state)
            oldtype = state[i]
            if isa(oldtype, VarState)
                oldtypetyp = oldtype.typ
                if isConditional(oldtypetyp) && conditional(oldtypetyp).slot_id == changeid
                    state[i] = VarState(widenconditional(oldtypetyp), oldtype.undef)
                end
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
