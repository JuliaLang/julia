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
        undefs::Vector{Union{Nothing,Bool}} # represents whether a given field may be undefined
        fields::Vector{Any} # i-th element describes the lattice element for the i-th defined field
    end

This extended lattice element is introduced when we have information about an object's
fields beyond what can be obtained from the object type. E.g. it represents a tuple where
some elements are known to be constants or a struct whose `Any`-typed field is initialized
with value whose type is concrete.

- `typ` indicates the type of the object
- `undefs` records defined-ness of each field
- `fields` holds the lattice elements corresponding to each field of the object

`fields` corresponds to the fields that `typ` can have.
If `typ` is a struct that can have `n` fields, then `length(fields) == n`.
A special case: if `typ` is a variable-length `Tuple`,
then `length(fields) == datatype_min_ninitialized(typ) + 1`.
The last element represents the `Vararg` element.

`undefs` is a `Vector{Union{Nothing,Bool}}` with the same length as `fields`, encoding
the following information about field defined-ness:
- `undefs[i] === nothing` indicates the corresponding element in `fields` may be undefined
- `undefs[i] === false` indicates the corresponding element in `fields` is guaranteed to be defined
- `undefs[i] === true` indicates the corresponding element in `fields` is guaranteed to be undefined
If `field[i]` is of type `Union{}`, it means the `i`-th field is never be initialized and
thus never be defined. In this case, `undefs[i]` should always be `true`.

The same applies if `typ` is a `Tuple`, and because of how `Tuple` elements are initialized,
`undefs[i] === false` holds except that `undefs[end]` may be `nothing` when the last element
is `Vararg`.
"""
Core.PartialStruct

function Core.PartialStruct(typ::Type, undefs::Vector{Union{Nothing,Bool}}, fields::Vector{Any})
    fldcnt = fieldcount_noerror(typ)
    if fldcnt !== nothing
        @assert fldcnt == length(fields)
    else
        @assert typ <: Tuple && isvarargtype(fields[end])
        @assert datatype_min_ninitialized(typ) == length(fields) - 1
        @assert undefs[end] === nothing
    end
    @assert length(fields) == length(undefs)
    for i = 1:length(fields)
        if fields[i] === Union{}
            @assert undefs[i] === true "`Union{}` typed field should be strictly undefined"
        end
    end
    return Core._PartialStruct(typ, undefs, fields)
end

# Legacy constructor
function Core.PartialStruct(@nospecialize(typ), fields::Vector{Any})
    undefs = partialstruct_init_undefs(typ, fields)
    undefs === nothing && error("This object never exists at runtime")
    return Core.PartialStruct(typ, undefs, fields)
end

function partialstruct_init_undefs(@nospecialize(typ), fields::Vector{Any})
    nf = length(fields)
    minf = datatype_min_ninitialized(typ)
    for i = 1:minf
        if fields[i] === Union{}
            return nothing # disallow runtime-invalid `PartialStruct`
        end
    end
    undefs = Union{Nothing,Bool}[nothing for _ in 1:nf]
    for i in 1:minf
        undefs[i] = false
    end
    for i = minf+1:nf
        if fields[i] === Union{}
            undefs[i] = true
        end
    end
    return undefs
end

a::PartialStruct == b::PartialStruct = a.typ === b.typ && a.undefs == b.undefs && a.fields == b.fields

function Base.getproperty(pstruct::Core.PartialStruct, name::Symbol)
    name === :undefs && return getfield(pstruct, :undefs)::Vector{Union{Nothing,Bool}}
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
