# Test that Serialization.serialize supports trimming

using Serialization

mutable struct TrimSerMut
    s::String
    m::Memory{Int}
    u::Union{Int, Nothing}
    t::Tuple{Int, String}
    # A `Memory` whose element type is itself a `Union`. Serializing this
    # field forces the serializer to write `Memory{Union{Int, String}}` as
    # a `DataType` value, which in turn serializes the `Union{Int, String}`
    # type parameter. That path must not fall through to the reflective
    # `serialize_fields(::Type{<:Union>})` method, which is not trim-safe.
    mu::Memory{Union{Int, String}}
end

struct TrimSerImm
    a::Int
    b::TrimSerMut
end

function @main(args::Vector{String})::Cint
    dir = dirname(PROGRAM_FILE)

    # Serialize stdlib types (test runner will deserialize and verify)
    serialize(joinpath(dir, "_trim_stdlib.jls"),
             (42, "hello", :sym, (1, 2.0), Int[10, 20, 30]))

    # Serialize custom struct types (exercises struct/Memory/Union/Tuple paths)
    mu = Memory{Union{Int, String}}(undef, 2)
    mu[1] = 123
    mu[2] = "abc"
    x = TrimSerImm(42, TrimSerMut("hello", [4, 2].ref.mem, 7, (3, "world"), mu))
    serialize(joinpath(dir, "_trim_custom.jls"), x)

    println(Core.stdout, "serialization ok")
    return 0
end
