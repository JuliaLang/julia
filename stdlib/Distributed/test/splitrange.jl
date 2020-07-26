# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Distributed
using Distributed: splitrange

@test splitrange(1, 11, 1) == Array{UnitRange{Int64},1}([1:11])
@test splitrange(0, 10, 1) == Array{UnitRange{Int64},1}([0:10])
@test splitrange(-1, 9, 1) == Array{UnitRange{Int64},1}([-1:9])

@test splitrange(1, 11, 2) == Array{UnitRange{Int64},1}([1:6,7:11])
@test splitrange(0, 10, 2) == Array{UnitRange{Int64},1}([0:5,6:10])
@test splitrange(-1, 9, 2) == Array{UnitRange{Int64},1}([-1:4,5:9])

@test splitrange(1, 11, 3) == Array{UnitRange{Int64},1}([1:4,5:8,9:11])
@test splitrange(0, 10, 3) == Array{UnitRange{Int64},1}([0:3,4:7,8:10])
@test splitrange(-1, 9, 3) == Array{UnitRange{Int64},1}([-1:2,3:6,7:9])

@test splitrange(1, 3, 3) == Array{UnitRange{Int64},1}([1:1,2:2,3:3])
@test splitrange(1, 3, 4) == Array{UnitRange{Int64},1}([1:1,2:2,3:3])
@test splitrange(0, 2, 3) == Array{UnitRange{Int64},1}([0:0,1:1,2:2])
@test splitrange(0, 2, 4) == Array{UnitRange{Int64},1}([0:0,1:1,2:2])
@test splitrange(-1, 1, 3) == Array{UnitRange{Int64},1}([-1:-1,0:0,1:1])
@test splitrange(-1, 1, 4) == Array{UnitRange{Int64},1}([-1:-1,0:0,1:1])

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

oa = OffsetArray([123, -345], (-2,))
@sync @distributed for i in eachindex(oa)
    @test i ∈ (-1, 0)
end
