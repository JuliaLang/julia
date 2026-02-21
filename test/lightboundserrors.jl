# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.LightBoundsErrors
using Test

function error_message_pattern(indices::String, collection)
    t = typeof(collection)
    a = axes(collection)
    ["LightBoundsError: ", "out-of-bounds index", "`collection[$indices]`", "`typeof(collection) == $t`", "`axes(collection) == $a`"]
end

@testset "LightBoundsErrors.jl" begin
    @testset "`LightBoundsError`" begin
        @test LightBoundsError <: Exception
    end
    @testset "`throw_lightboundserror`" begin
        @test_throws LightBoundsError throw_lightboundserror([1])
        @test_throws LightBoundsError throw_lightboundserror([1], 3, 7)
    end
    @testset "`checkbounds_lightboundserror`" begin
        elt = Float32
        @testset "0D" begin
            a = Array{elt, 0}(undef)
            for n ∈ 0:4
                @test let is = ntuple(Returns(1), n)
                    nothing === @inferred checkbounds_lightboundserror(a, is...)
                end
            end
            for i ∈ [-1, 0, 2]
                @test_throws LightBoundsError checkbounds_lightboundserror(a, i)
                @test_throws error_message_pattern("$i", a) checkbounds_lightboundserror(a, i)
                for j ∈ -1:2
                    @test_throws LightBoundsError checkbounds_lightboundserror(a, i, j)
                    @test_throws error_message_pattern("$i, $j", a) checkbounds_lightboundserror(a, i, j)
                end
            end
        end
        @testset "1D" begin
            a = Vector{elt}(undef, 2)
            for n ∈ 1:4
                @test let is = ntuple(Returns(1), n)
                    nothing === @inferred checkbounds_lightboundserror(a, is...)
                end
            end
            for n ∈ 0:3
                @test let is = ntuple(Returns(1), n)
                    nothing === @inferred checkbounds_lightboundserror(a, 2, is...)
                end
            end
            @test_throws LightBoundsError checkbounds_lightboundserror(a)
            @test_throws error_message_pattern("", a) checkbounds_lightboundserror(a)
            @test_throws LightBoundsError checkbounds_lightboundserror(a, 0, 0, 0)
            @test_throws error_message_pattern("0, 0, 0", a) checkbounds_lightboundserror(a, 0, 0, 0)
        end
    end
end
