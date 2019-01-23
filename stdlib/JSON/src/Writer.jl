module Writer

using Dates
using ..Common
using ..Serializations: Serialization, StandardSerialization,
                        CommonSerialization

using Unicode


"""
Internal JSON.jl implementation detail; do not depend on this type.

A JSON primitive that wraps around any composite type to enable `Dict`-like
serialization.
"""
struct CompositeTypeWrapper{T}
    wrapped::T
    fns::Vector{Symbol}
end

CompositeTypeWrapper(x, syms) = CompositeTypeWrapper(x, collect(syms))
CompositeTypeWrapper(x) = CompositeTypeWrapper(x, fieldnames(typeof(x)))

"""
    lower(x)

Return a value of a JSON-encodable primitive type that `x` should be lowered
into before encoding as JSON. Supported types are: `AbstractDict` to JSON
objects, `Tuple` and `AbstractVector` to JSON arrays, `AbstractArray` to nested
JSON arrays, `AbstractString`, `Symbol`, `Enum`, or `Char` to JSON string,
`Integer` and `AbstractFloat` to JSON number, `Bool` to JSON boolean, and
`Nothing` to JSON null, or any other types with a `show_json` method defined.

Extensions of this method should preserve the property that the return value is
one of the aforementioned types. If first lowering to some intermediate type is
required, then extensions should call `lower` before returning a value.

Note that the return value need not be *recursively* lowered—this function may
for instance return an `AbstractArray{Any, 1}` whose elements are not JSON
primitives.
"""
function lower(a)
    if nfields(a) > 0
        CompositeTypeWrapper(a)
    else
        error("Cannot serialize type $(typeof(a))")
    end
end

# To avoid allocating an intermediate string, we directly define `show_json`
# for this type instead of lowering it to a string first (which would
# allocate). However, the `show_json` method does call `lower` so as to allow
# users to change the lowering of their `Enum` or even `AbstractString`
# subtypes if necessary.
const IsPrintedAsString = Union{
    Dates.TimeType, Char, Type, AbstractString, Enum, Symbol}
lower(x::IsPrintedAsString) = x

lower(m::Module) = throw(ArgumentError("cannot serialize Module $m as JSON"))
lower(x::Real) = convert(Float64, x)
lower(x::Base.AbstractSet) = collect(x)

"""
Abstract supertype of all JSON and JSON-like structural writer contexts.
"""
abstract type StructuralContext <: IO end

"""
Internal implementation detail.

A JSON structural context around an `IO` object. Structural writer contexts
define the behaviour of serializing JSON structural objects, such as objects,
arrays, and strings to JSON. The translation of Julia types to JSON structural
objects is not handled by a `JSONContext`, but by a `Serialization` wrapper
around it. Abstract supertype of `PrettyContext` and `CompactContext`. Data can
be written to a JSON context in the usual way, but often higher-level operations
such as `begin_array` or `begin_object` are preferred to directly writing bytes
to the stream.
"""
abstract type JSONContext <: StructuralContext end

"""
Internal implementation detail.

Keeps track of the current location in the array or object, which winds and
unwinds during serialization.
"""
mutable struct PrettyContext{T<:IO} <: JSONContext
    io::T
    step::Int     # number of spaces to step
    state::Int    # number of steps at present
    first::Bool   # whether an object/array was just started
end
PrettyContext(io::IO, step) = PrettyContext(io, step, 0, false)

"""
Internal implementation detail.

For compact printing, which in JSON is fully recursive.
"""
mutable struct CompactContext{T<:IO} <: JSONContext
    io::T
    first::Bool
end
CompactContext(io::IO) = CompactContext(io, false)

"""
Internal implementation detail.

Implements an IO context safe for printing into JSON strings.
"""
struct StringContext{T<:IO} <: IO
    io::T
end

# These aliases make defining additional methods on `show_json` easier.
const CS = CommonSerialization
const SC = StructuralContext

# Low-level direct access
Base.write(io::JSONContext, byte::UInt8) = write(io.io, byte)
Base.write(io::StringContext, byte::UInt8) =
    write(io.io, ESCAPED_ARRAY[byte + 0x01])
#= turn on if there's a performance benefit
write(io::StringContext, char::Char) =
    char <= '\x7f' ? write(io, ESCAPED_ARRAY[UInt8(c) + 0x01]) :
                     Base.print(io, c)
=#

"""
    indent(io::StructuralContext)

If appropriate, write a newline to the given context, then indent it by the
appropriate number of spaces. Otherwise, do nothing.
"""
@inline function indent(io::PrettyContext)
    write(io, NEWLINE)
    for _ in 1:io.state
        write(io, SPACE)
    end
end
@inline indent(io::CompactContext) = nothing

"""
    separate(io::StructuralContext)

Write a colon, followed by a space if appropriate, to the given context.
"""
@inline separate(io::PrettyContext) = write(io, SEPARATOR, SPACE)
@inline separate(io::CompactContext) = write(io, SEPARATOR)

"""
    delimit(io::StructuralContext)

If this is not the first item written in a collection, write a comma in the
structural context.  Otherwise, do not write a comma, but set a flag that the
first element has been written already.
"""
@inline function delimit(io::JSONContext)
    if !io.first
        write(io, DELIMITER)
    end
    io.first = false
end

for kind in ("object", "array")
    beginfn = Symbol("begin_", kind)
    beginsym = Symbol(uppercase(kind), "_BEGIN")
    endfn = Symbol("end_", kind)
    endsym = Symbol(uppercase(kind), "_END")
    # Begin and end objects
    @eval function $beginfn(io::PrettyContext)
        write(io, $beginsym)
        io.state += io.step
        io.first = true
    end
    @eval $beginfn(io::CompactContext) = (write(io, $beginsym); io.first = true)
    @eval function $endfn(io::PrettyContext)
        io.state -= io.step
        if !io.first
            indent(io)
        end
        write(io, $endsym)
        io.first = false
    end
    @eval $endfn(io::CompactContext) = (write(io, $endsym); io.first = false)
end

"""
    show_string(io::IO, str)

Print `str` as a JSON string (that is, properly escaped and wrapped by double
quotes) to the given IO object `io`.
"""
function show_string(io::IO, x)
    write(io, STRING_DELIM)
    Base.print(StringContext(io), x)
    write(io, STRING_DELIM)
end

"""
    show_null(io::IO)

Print the string `null` to the given IO object `io`.
"""
show_null(io::IO) = Base.print(io, "null")

"""
    show_element(io::StructuralContext, s, x)

Print object `x` as an element of a JSON array to context `io` using rules
defined by serialization `s`.
"""
function show_element(io::JSONContext, s, x)
    delimit(io)
    indent(io)
    show_json(io, s, x)
end

"""
    show_key(io::StructuralContext, k)

Print string `k` as the key of a JSON key-value pair to context `io`.
"""
function show_key(io::JSONContext, k)
    delimit(io)
    indent(io)
    show_string(io, k)
    separate(io)
end

"""
    show_pair(io::StructuralContext, s, k, v)

Print the key-value pair defined by `k => v` as JSON to context `io`, using
rules defined by serialization `s`.
"""
function show_pair(io::JSONContext, s, k, v)
    show_key(io, k)
    show_json(io, s, v)
end
show_pair(io::JSONContext, s, kv) = show_pair(io, s, first(kv), last(kv))

# Default serialization rules for CommonSerialization (CS)
function show_json(io::SC, s::CS, x::IsPrintedAsString)
    # We need this check to allow `lower(x::Enum)` overrides to work if needed;
    # it should be optimized out if `lower` is a no-op
    lx = lower(x)
    if x === lx
        show_string(io, x)
    else
        show_json(io, s, lx)
    end
end

function show_json(io::SC, s::CS, x::Union{Integer, AbstractFloat})
    if isfinite(x)
        Base.print(io, x)
    else
        show_null(io)
    end
end

show_json(io::SC, ::CS, ::Nothing) = show_null(io)
show_json(io::SC, ::CS, ::Missing) = show_null(io)

function show_json(io::SC, s::CS, a::AbstractDict)
    begin_object(io)
    for kv in a
        show_pair(io, s, kv)
    end
    end_object(io)
end

function show_json(io::SC, s::CS, kv::Pair)
    begin_object(io)
    show_pair(io, s, kv)
    end_object(io)
end

function show_json(io::SC, s::CS, x::CompositeTypeWrapper)
    begin_object(io)
    for fn in x.fns
        show_pair(io, s, fn, getfield(x.wrapped, fn))
    end
    end_object(io)
end

function show_json(io::SC, s::CS, x::Union{AbstractVector, Tuple})
    begin_array(io)
    for elt in x
        show_element(io, s, elt)
    end
    end_array(io)
end

"""
Serialize a multidimensional array to JSON in column-major format. That is,
`json([1 2 3; 4 5 6]) == "[[1,4],[2,5],[3,6]]"`.
"""
function show_json(io::SC, s::CS, A::AbstractArray{<:Any,n}) where n
    begin_array(io)
    newdims = ntuple(_ -> :, n - 1)
    for j in axes(A, n)
        show_element(io, s, view(A, newdims..., j))
    end
    end_array(io)
end

# special case for 0-dimensional arrays
show_json(io::SC, s::CS, A::AbstractArray{<:Any,0}) = show_json(io, s, A[])

show_json(io::SC, s::CS, a) = show_json(io, s, lower(a))

# Fallback show_json for non-SC types
"""
Serialize Julia object `obj` to IO `io` using the behaviour described by `s`. If
`indent` is provided, then the JSON will be pretty-printed; otherwise it will be
printed on one line. If pretty-printing is enabled, then a trailing newline will
be printed; otherwise there will be no trailing newline.
"""
function show_json(io::IO, s::Serialization, obj; indent=nothing)
    ctx = indent === nothing ? CompactContext(io) : PrettyContext(io, indent)
    show_json(ctx, s, obj)
    if indent !== nothing
        println(io)
    end
end

"""
    JSONText(s::AbstractString)

`JSONText` is a wrapper around a Julia string representing JSON-formatted
text, which is inserted *as-is* in the JSON output of `JSON.print` and `JSON.json`
for compact output, and is otherwise re-parsed for pretty-printed output.

`s` *must* contain valid JSON text.  Otherwise compact output will contain
the malformed `s` and other serialization output will throw a parsing exception.
"""
struct JSONText
    s::String
end
show_json(io::CompactContext, s::CS, json::JSONText) = write(io, json.s)
# other contexts for JSONText are handled by lower(json) = parse(json.s)

print(io::IO, obj, indent) =
    show_json(io, StandardSerialization(), obj; indent=indent)
print(io::IO, obj) = show_json(io, StandardSerialization(), obj)

print(a, indent) = print(stdout, a, indent)
print(a) = print(stdout, a)

json(a) = sprint(print, a)
json(a, indent) = sprint(print, a, indent)

end
