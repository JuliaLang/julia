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
        defined::BitVector # sorted list of fields that are known to be defined
        fields::Vector{Any} # i-th element describes the lattice element for the i-th defined field
    end

This extended lattice element is introduced when we have information about an object's
fields beyond what can be obtained from the object type. E.g. it represents a tuple where
some elements are known to be constants or a struct whose `Any`-typed field is initialized
with `Int` values.

- `typ` indicates the type of the object
- `defined` records which fields are defined
- `fields` holds the lattice elements corresponding to each defined field of the object

If `typ` is a struct, `defined` represents whether the corresponding field of the struct is guaranteed to be
initialized. For any defined field (`defined[i] === true`), there is a corresponding `fields` element
which provides information about the type of the defined field.

If `typ` is a tuple, the last element of `fields` may be `Vararg`. In this case, it is
guaranteed that the number of elements in the tuple is at least `length(fields)-1`, but the
exact number of elements is unknown (`defined` then has a length of `length(fields)-1`).
"""
Core.PartialStruct

function Core.PartialStruct(@nospecialize(typ), fields::Vector{Any})
    ndefined = lastindex(fields)
    fields[end] === Vararg && (ndefined -= 1)
    t = typ
    (isa(t, UnionAll) || isa(t, Union)) && (t = argument_datatype(t))
    nfields = isa(t, DataType) ? datatype_fieldcount(t) : nothing
    if nfields === nothing || nfields == ndefined
        defined = trues(ndefined)
    else
        @assert nfields ≥ ndefined
        defined = falses(nfields)
        for i in 1:ndefined
            defined[i] = true
        end
    end
    Core._PartialStruct(typ, defined, fields)
end

(==)(a::PartialStruct, b::PartialStruct) = a.typ === b.typ && a.defined == b.defined && a.fields == b.fields

function Base.getproperty(pstruct::Core.PartialStruct, name::Symbol)
    name === :defined && return getfield(pstruct, :defined)::BitVector
    getfield(pstruct, name)
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

InterConditional(var::SlotNumber, @nospecialize(thentype), @nospecialize(elsetype)) =
    InterConditional(slot_id(var), thentype, elsetype)
