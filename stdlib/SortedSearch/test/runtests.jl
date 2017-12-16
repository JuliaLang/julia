# This file is a part of Julia. License is MIT: https://julialang.org/license

using SortedSearch
using Base.Order: Forward

@testset "searchsorted" begin
    numTypes = [ Int8,  Int16,  Int32,  Int64,  Int128,
                UInt8, UInt16, UInt32, UInt64, UInt128,
                Float16, Float32, Float64, BigInt, BigFloat]

    @test searchsorted([1:10;], 1, by=(x -> x >= 5)) == 1:4
    @test searchsorted([1:10;], 10, by=(x -> x >= 5)) == 5:10
    @test searchsorted([1:5; 1:5; 1:5], 1, 6, 10, Forward) == 6:6
    @test searchsorted(ones(15), 1, 6, 10, Forward) == 6:10

    for R in numTypes, T in numTypes
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(0)) == 1:0
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(1)) == 1:2
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(2)) == 3:4
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(4)) == 7:6
        @test searchsorted(R[1, 1, 2, 2, 3, 3], 2.5) == 5:4

        @test searchsorted(1:3, T(0)) == 1:0
        @test searchsorted(1:3, T(1)) == 1:1
        @test searchsorted(1:3, T(2)) == 2:2
        @test searchsorted(1:3, T(4)) == 4:3

        @test searchsorted(R[1:10;], T(1), by=(x -> x >= 5)) == 1:4
        @test searchsorted(R[1:10;], T(10), by=(x -> x >= 5)) == 5:10
        @test searchsorted(R[1:5; 1:5; 1:5], T(1), 6, 10, Forward) == 6:6
        @test searchsorted(ones(R, 15), T(1), 6, 10, Forward) == 6:10
    end

    for (rg,I) in [(49:57,47:59), (1:2:17,-1:19), (-3:0.5:2,-5:.5:4)]
        rg_r = reverse(rg)
        rgv, rgv_r = [rg;], [rg_r;]
        for i = I
            @test searchsorted(rg,i) == searchsorted(rgv,i)
            @test searchsorted(rg_r,i,rev=true) == searchsorted(rgv_r,i,rev=true)
        end
    end

    rg = 0.0:0.01:1.0
    for i = 2:101
        @test searchsorted(rg, rg[i]) == i:i
        @test searchsorted(rg, prevfloat(rg[i])) == i:i-1
        @test searchsorted(rg, nextfloat(rg[i])) == i+1:i
    end

    rg_r = reverse(rg)
    for i = 1:100
        @test searchsorted(rg_r, rg_r[i], rev=true) == i:i
        @test searchsorted(rg_r, prevfloat(rg_r[i]), rev=true) == i+1:i
        @test searchsorted(rg_r, nextfloat(rg_r[i]), rev=true) == i:i-1
    end

    @test searchsorted(1:10, 1, by=(x -> x >= 5)) == searchsorted([1:10;], 1, by=(x -> x >= 5))
    @test searchsorted(1:10, 10, by=(x -> x >= 5)) == searchsorted([1:10;], 10, by=(x -> x >= 5))

    @test searchsorted([], 0) == 1:0
    @test searchsorted([1,2,3], 0) == 1:0
    @test searchsorted([1,2,3], 4) == 4:3

    @testset "issue 8866" begin
        @test searchsortedfirst(500:1.0:600, -1.0e20) == 1
        @test searchsortedfirst(500:1.0:600, 1.0e20) == 102
        @test searchsortedlast(500:1.0:600, -1.0e20) == 0
        @test searchsortedlast(500:1.0:600, 1.0e20) == 101
    end
end

# exercise the codepath in searchsorted* methods for ranges that check for zero step range
struct ConstantRange{T} <: AbstractRange{T}
    val::T
    len::Int
 end

Base.length(r::ConstantRange) = r.len
Base.getindex(r::ConstantRange, i::Int) = (1 <= i <= r.len || throw(BoundsError(r,i)); r.val)
Base.step(r::ConstantRange) = 0

@testset "searchsorted method with ranges which check for zero step range" begin
    r = ConstantRange(1, 5)

    @test searchsortedfirst(r, 1.0, Forward) == 1
    @test searchsortedfirst(r, 1, Forward) == 1
    @test searchsortedfirst(r, UInt(1), Forward) == 1

    @test searchsortedlast(r, 1.0, Forward) == 5
    @test searchsortedlast(r, 1, Forward) == 5
    @test searchsortedlast(r, UInt(1), Forward) == 5
end

@testset "issue #19005" begin
    @test searchsortedfirst(0:256, 0x80) == 129
    @test searchsortedlast(0:256, 0x80) == 129
end