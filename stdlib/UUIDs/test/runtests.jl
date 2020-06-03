# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, UUIDs, Random

u1 = uuid1()
u4 = uuid4()
u5 = uuid5(u1, "julia")
@test uuid_version(u1) == 1
@test uuid_version(u4) == 4
@test uuid_version(u5) == 5
@test u1 == UUID(string(u1)) == UUID(GenericString(string(u1)))
@test u4 == UUID(string(u4)) == UUID(GenericString(string(u4)))
@test u5 == UUID(string(u5)) == UUID(GenericString(string(u5)))
@test u1 == UUID(UInt128(u1))
@test u4 == UUID(UInt128(u4))
@test u5 == UUID(UInt128(u5))
@test uuid4(MersenneTwister(0)) == uuid4(MersenneTwister(0))
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-446655440000")
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-44665544000098")
@test_throws ArgumentError UUID("z50e8400-e29b-41d4-a716-446655440000")

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

for (idx, init_uuid) in enumerate(following_uuids[1:end-1])
    next_id = uuid5(init_uuid, "julia")
    @test next_id == following_uuids[idx+1]
end

# Python-generated UUID following each of the standard namespaces
const standard_namespace_uuids = [
    (UUIDs.namespace_dns,  UUID("00ca23ad-40ef-500c-a910-157de3950d07")),
    (UUIDs.namespace_oid,  UUID("b7bf72b0-fb4e-538b-952a-3be296f07f6d")),
    (UUIDs.namespace_url,  UUID("997cd5be-4705-5439-9fe6-d77b18d612e5")),
    (UUIDs.namespace_x500, UUID("993c6684-82e7-5cdb-bd46-9bff0362e6a9")),
]

for (init_uuid, next_uuid) in standard_namespace_uuids
    result = uuid5(init_uuid, "julia")
    @test next_uuid == result
end

# Issue 35860
Random.seed!(Random.GLOBAL_RNG, 10)
u1 = uuid1()
u4 = uuid4()
Random.seed!(Random.GLOBAL_RNG, 10)
@test u1 != uuid1()
@test u4 != uuid4()
