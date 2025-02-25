# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.PhiNode() = Core.PhiNode(Int32[], Any[])

"""
    struct Const
        val
    end

The type representing a constant value.
"""
Core.Const

"""
    struct PartialStruct
        typ
        undef::BitVector # represents whether a given field may be undefined
        fields::Vector{Any} # i-th element describes the lattice element for the i-th defined field
    end

This extended lattice element is introduced when we have information about an object's
fields beyond what can be obtained from the object type. E.g. it represents a tuple where
some elements are known to be constants or a struct whose `Any`-typed field is initialized
with `Int` values.

- `typ` indicates the type of the object
- `undef` records which fields are possibly undefined
- `fields` holds the lattice elements corresponding to each defined field of the object

If `typ` is a struct, `undef` represents whether the corresponding field of the struct is guaranteed to be
initialized. For any defined field, there is a corresponding `fields` element which provides information
about the type of the defined field.

If `typ` is a tuple, the last element of `fields` may be `Vararg`. In this case, it is
guaranteed that the number of elements in the tuple is at least `length(fields)-1`, but the
exact number of elements is unknown (`undef` then has a length of `length(fields)-1`).
"""
Core.PartialStruct

function Core.PartialStruct(typ::Type, undef::BitVector, fields::Vector{Any})
    @assert length(undef) == length(fields) - isvarargtype(fields[end])
    for i = 1:length(fields)
        @assert fields[i] !== Union{} # TODO remove me once we start to exploit strict undef-ness of fields
    end
    return Core._PartialStruct(typ, undef, fields)
end

function Core.PartialStruct(@nospecialize(typ), fields::Vector{Any})
    return Core.PartialStruct(typ, partialstruct_init_undef(typ, fields), fields)
end

partialstruct_undef_length(fields) = length(fields) - isvarargtype(fields[end])

function partialstruct_init_undef(@nospecialize(typ), fields; all_defined = true)
    n = partialstruct_undef_length(fields)
    return partialstruct_init_undef(typ, n; all_defined)
end

function partialstruct_init_undef(@nospecialize(typ), n::Integer; all_defined = true)
    all_defined && return falses(n)
    undef = trues(n)
    for i in 1:min(datatype_min_ninitialized(typ), n)
        undef[i] = false
    end
    return undef
end

(==)(a::PartialStruct, b::PartialStruct) = a.typ === b.typ && a.undef == b.undef && a.fields == b.fields

function Base.getproperty(pstruct::Core.PartialStruct, name::Symbol)
    name === :undef && return getfield(pstruct, :undef)::BitVector
    return getfield(pstruct, name)
end

"""
    struct InterConditional
        slot::Int
        thentype
        elsetype
    end

Similar to `Conditional`, but conveys inter-procedural constraints imposed on call arguments.
This is separate from `Conditional` to catch logic errors: the lattice element name is `InterConditional`
while processing a call, then `Conditional` everywhere else.
"""
Core.InterConditional

Core.InterConditional(var::SlotNumber, @nospecialize(thentype), @nospecialize(elsetype)) =
    InterConditional(slot_id(var), thentype, elsetype)
