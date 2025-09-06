# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, UUIDs, Random
using UUIDs: _build_uuid1, _build_uuid7

# results similar to Python builtin uuid
# To reproduce the sequence
#=
import uuid
uuids = [uuid.UUID("22b4a8a1-e548-4eeb-9270-60426d66a48e")]
for _ in range(5):
    uuids.append(uuid.uuid5(uuids[-1], "julia"))
=#

const following_uuids = [
    UUID("22b4a8a1-e548-4eeb-9270-60426d66a48e"),
    UUID("30ea6cfd-c270-569f-b4cb-795dead63686"),
    UUID("31099374-e3a0-5fde-9482-791c639bf29b"),
    UUID("6b34b357-a348-53aa-8c71-fb9b06c3a51e"),
    UUID("fdbd7d4d-c462-59cc-ae6a-0c3b010240e2"),
    UUID("d8cc6298-75d5-57e0-996c-279259ab365c"),
]

# Python-generated UUID following each of the standard namespaces
const standard_namespace_uuids = [
    (UUIDs.namespace_dns,  UUID("00ca23ad-40ef-500c-a910-157de3950d07")),
    (UUIDs.namespace_oid,  UUID("b7bf72b0-fb4e-538b-952a-3be296f07f6d")),
    (UUIDs.namespace_url,  UUID("997cd5be-4705-5439-9fe6-d77b18d612e5")),
    (UUIDs.namespace_x500, UUID("993c6684-82e7-5cdb-bd46-9bff0362e6a9")),
]

@testset "UUIDs" begin
u1 = uuid1()
u4 = uuid4()
u5 = uuid5(u1, "julia")
u7 = uuid7()

@testset "Extraction of version numbers" begin
    @test uuid_version(u1) == 1
    @test uuid_version(u4) == 4
    @test uuid_version(u5) == 5
    @test uuid_version(u7) == 7
end

@testset "Extraction of variant bits" begin
    # RFC 4122, section 4.1.1
    uuid_variant(u::UUID) = Int((u.value >> 62) & 0x3)
    @test uuid_variant(u1) == 2
    @test uuid_variant(u4) == 2
    @test uuid_variant(u5) == 2
    @test uuid_variant(u7) == 2
end

@testset "Parsing from string" begin
    @test u1 == UUID(string(u1)) == UUID(GenericString(string(u1)))
    @test u4 == UUID(string(u4)) == UUID(GenericString(string(u4)))
    @test u5 == UUID(string(u5)) == UUID(GenericString(string(u5)))
    @test u7 == UUID(string(u7)) == UUID(GenericString(string(u7)))
end

@testset "UInt128 conversion" begin
    @test u1 == UUID(UInt128(u1))
    @test u4 == UUID(UInt128(u4))
    @test u5 == UUID(UInt128(u5))
    @test u7 == UUID(UInt128(u7))
end

@testset "Passing an RNG" begin
    rng = Xoshiro(0)
    @test uuid1(rng) isa UUID
    @test uuid4(rng) isa UUID
    @test uuid7(rng) isa UUID
end

@testset "uuid1, uuid4 & uuid7 RNG stability" begin
    @test uuid4(Xoshiro(0)) == uuid4(Xoshiro(0))

    time_uuid1 = rand(UInt64)
    time_uuid7 = rand(UInt128)

    # we need to go through the internal function to test RNG stability
    @test _build_uuid1(Xoshiro(0), time_uuid1) == _build_uuid1(Xoshiro(0), time_uuid1)
    @test _build_uuid7(Xoshiro(0), time_uuid7) == _build_uuid7(Xoshiro(0), time_uuid7)
end

@testset "Rejection of invalid UUID strings" begin
    @test_throws ArgumentError UUID("550e8400e29b-41d4-a716-446655440000")
    @test_throws ArgumentError UUID("550e8400e29b-41d4-a716-44665544000098")
    @test_throws ArgumentError UUID("z50e8400-e29b-41d4-a716-446655440000")
    @test_throws ArgumentError UUID("22b4a8a1ae548-4eeb-9270-60426d66a48e")
    @test_throws ArgumentError UUID("22b4a8a1-e548a4eeb-9270-60426d66a48e")
    @test_throws ArgumentError UUID("22b4a8a1-e548-4eeba9270-60426d66a48e")
    @test_throws ArgumentError UUID("22b4a8a1-e548-4eeb-9270a60426d66a48e")
end

@testset "UUID sequence" begin
    for (idx, init_uuid) in enumerate(following_uuids[1:end-1])
        next_id = uuid5(init_uuid, "julia")
        @test next_id == following_uuids[idx+1]
    end
end

@testset "Standard namespace UUIDs" begin
    for (init_uuid, next_uuid) in standard_namespace_uuids
        result = uuid5(init_uuid, "julia")
        @test next_uuid == result
    end
end

@testset "Use of Random.RandomDevice (#35860)" begin
    Random.seed!(Random.default_rng(), 10)
    u1 = uuid1()
    u4 = uuid4()
    u7 = uuid7()
    Random.seed!(Random.default_rng(), 10)
    @test u1 != uuid1()
    @test u4 != uuid4()
    @test u7 != uuid7()
end

@testset "case invariance" begin
    str = "22b4a8a1-e548-4eeb-9270-60426d66a48e"
    @test UUID(uppercase(str)) == UUID(str)
end

@testset "Equality of string parsing & direct UInt128 passing" begin
    for r in rand(UInt128, 10^3)
        @test UUID(r) == UUID(string(UUID(r)))
    end
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(UUIDs))
end
end
