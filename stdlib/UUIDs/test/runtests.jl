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
