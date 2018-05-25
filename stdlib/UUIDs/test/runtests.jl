# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, UUIDs, Random

u1 = uuid1()
u4 = uuid4()
@test uuid_version(u1) == 1
@test uuid_version(u4) == 4
@test u1 == UUID(string(u1)) == UUID(GenericString(string(u1)))
@test u4 == UUID(string(u4)) == UUID(GenericString(string(u4)))
@test u1 == UUID(UInt128(u1))
@test u4 == UUID(UInt128(u4))
@test uuid4(MersenneTwister(0)) == uuid4(MersenneTwister(0))
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-446655440000")
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-44665544000098")
@test_throws ArgumentError UUID("z50e8400-e29b-41d4-a716-446655440000")
