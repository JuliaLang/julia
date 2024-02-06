# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Future

@testset "Testing copy! for sets" begin
    dst = Set([])  # Create an empty set
    src = Set([1, 24, 5, 6, 51, 62])
    copy!(dst, src)
    @test dst == src
end

@testset "Testing copy! for Dictionaries" begin
    dst = Dict("a" => 1, "b" => 2)
    src = Dict("c" => 3, "d" => 4)
    copy!(dst, src)
    @test dst == src
end

@testset "Testing copy! for Arrays" begin
    dst = [1, 2, 3]
    src = [4, 5, 6]
    copy!(dst, src)
    @test dst == src
end
