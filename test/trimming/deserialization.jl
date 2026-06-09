# Test that Serialization.deserialize with type annotation supports trimming

using Serialization

struct TrimDeserPoint
    x::Int
    y::Float64
    s::String
end

# Nested struct — regression test for typed deserialization of a struct whose
# fields are themselves user-defined struct types. The generated body must not
# leave behind a cross-type `deserialize_typed` call that the trim verifier
# cannot resolve.
struct TrimDeserInner
    v::Vector{Int}
    t::TrimDeserPoint
end
struct TrimDeserOuter
    a::Int
    b::TrimDeserInner
    c::Vector{TrimDeserPoint}
    # `Memory{UserStruct}` + `Vector{UserStructWithNestedArray}` are separate
    # trim-regression cases: they go through `deserialize_memory_typed` /
    # `deserialize_array_typed` → `deserialize_fillarray_typed!`, which then
    # invokes `deserialize_typed(..., <user struct>)`. That inner invoke must
    # be resolvable by the trim verifier even when the element type itself
    # contains further user-struct / array fields.
    d::Memory{TrimDeserPoint}
    e::Vector{TrimDeserInner}
    # NamedTuple field — regression test that typed deserialization of a
    # struct field of `NamedTuple` type does not fall back to the untyped
    # `handle_deserialize`, which would be unresolvable for trimming.
    f::@NamedTuple{lon::Float64, lat::Float64}
end

function @main(args::Vector{String})::Cint
    dir = dirname(PROGRAM_FILE)

    # Deserialize stdlib types with type annotations (data prepared by test runner)
    val1 = deserialize(joinpath(dir, "_trim_deser_int.jls"), Int)
    println(Core.stdout, val1)

    val2 = deserialize(joinpath(dir, "_trim_deser_str.jls"), String)
    println(Core.stdout, val2)

    val3 = deserialize(joinpath(dir, "_trim_deser_vec.jls"), Vector{Int})
    println(Core.stdout, val3[1], " ", val3[2], " ", val3[3])

    val4 = deserialize(joinpath(dir, "_trim_deser_dict.jls"), Dict{String, Int})
    println(Core.stdout, val4["a"], " ", val4["b"])

    # Deserialize a user-defined struct with concrete fields. This exercises
    # the typed `deserialize_typed(::Type{T})` path for user types, forcing
    # field-by-field dispatch to concrete `Int`/`Float64`/`String` readers.
    val5 = deserialize(joinpath(dir, "_trim_deser_pt.jls"), TrimDeserPoint)
    println(Core.stdout, val5.x, " ", val5.y, " ", val5.s)

    # Deserialize a struct whose fields include other user-defined struct types
    # and a `Vector` of such structs.
    val6 = deserialize(joinpath(dir, "_trim_deser_nest.jls"), TrimDeserOuter)
    println(Core.stdout, val6.a)
    println(Core.stdout, val6.b.v[1])
    println(Core.stdout, val6.b.t.x)
    println(Core.stdout, val6.c[1].s)
    println(Core.stdout, val6.c[2].y)
    println(Core.stdout, val6.d[1].x)
    println(Core.stdout, val6.d[2].s)
    println(Core.stdout, val6.e[1].v[2])
    println(Core.stdout, val6.e[1].t.s)
    println(Core.stdout, val6.f.lon)
    println(Core.stdout, val6.f.lat)

    println(Core.stdout, "deserialization ok")
    return 0
end
