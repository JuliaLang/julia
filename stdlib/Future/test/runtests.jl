# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Future
using SparseArrays

@testset "Future.copy! for AbstractSet" begin
    for S = (Set, BitSet)
        s = S([1, 2])
        for a = ([1], UInt[1], [3, 4, 5], UInt[3, 4, 5])
            @test s === Future.copy!(s, Set(a)) == S(a)
            @test s === Future.copy!(s, BitSet(a)) == S(a)
        end
    end
end


@testset "Future.copy! for AbstractDict" begin
    s = Dict(1=>2, 2=>3)
    for a = ([3=>4], [0x3=>0x4], [3=>4, 5=>6, 7=>8], Pair{UInt,UInt}[3=>4, 5=>6, 7=>8])
        @test s === Future.copy!(s, Dict(a)) == Dict(a)
        if length(a) == 1 # current limitation of Base.ImmutableDict
            @test s === Future.copy!(s, Base.ImmutableDict(a[])) == Dict(a[])
        end
    end
end

@testset "Future.copy! for AbstractVector" begin
        s = Vector([1, 2])
        for a = ([1], UInt[1], [3, 4, 5], UInt[3, 4, 5])
            @test s === Future.copy!(s, Vector(a)) == Vector(a)
            @test s === Future.copy!(s, SparseVector(a)) == Vector(a)
        end
end

@testset "Future.copy! for AbstractArray" begin
    @test_throws ArgumentError Future.copy!(zeros(2, 3), zeros(3, 2))
    s = zeros(2, 2)
    @test s === Future.copy!(s, fill(1, 2, 2)) == fill(1, 2, 2)
    @test s === Future.copy!(s, fill(1.0, 2, 2)) == fill(1.0, 2, 2)
end
