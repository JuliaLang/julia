# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random

# test the basic floating point functions

@testset "flipsign" begin
    for elty in (Float32,Float64)
        x = convert(elty,-2.0)
        x = flipsign(x,-1.0)
        @test flipsign(x,big(-1.0)) == convert(elty,-2.0)
    end
end

@testset "maxintfloat" begin
    @test maxintfloat(Float16) === Float16(2048f0)
    for elty in (Float16,Float32,Float64)
        @test maxintfloat(rand(elty)) === maxintfloat(elty)
    end
    @test maxintfloat() === maxintfloat(Float64)
    @test maxintfloat(Float64, Int32) === 2147483647.0
    @test maxintfloat(Float32, Int32) === maxintfloat(Float32)
    @test maxintfloat(Float64, Int16) === 32767.0
    @test maxintfloat(Float64, Int64) === maxintfloat(Float64)
end

@testset "isinteger" begin
    for elty in (Float16, Float32, Float64)
        @test !isinteger(elty(1.2))
        @test isinteger(elty(12))
        @test isinteger(zero(elty))
        @test isinteger(-zero(elty))
        @test !isinteger(nextfloat(zero(elty)))
        @test !isinteger(prevfloat(zero(elty)))
        @test isinteger(maxintfloat(elty))
        @test isinteger(-maxintfloat(elty))
        @test !isinteger(elty(Inf))
        @test !isinteger(-elty(Inf))
        @test !isinteger(elty(NaN))
    end
end

@testset "round" begin
    for elty in (Float32, Float64)
        x = rand(elty)
        A = fill(x,(10,10))
        @test round.(A,RoundToZero) == fill(trunc(x),(10,10))
        @test round.(A,RoundUp) == fill(ceil(x),(10,10))
        @test round.(A,RoundDown) == fill(floor(x),(10,10))
        A = fill(x,(10,10,10))
        @test round.(A,RoundToZero) == fill(trunc(x),(10,10,10))
        @test round.(A,RoundUp) == fill(ceil(x),(10,10,10))
        @test round.(A,RoundDown) == fill(floor(x),(10,10,10))
        for elty2 in (Int32,Int64)
            A = fill(x,(10,))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,))
            A = fill(x,(10,10))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10))
            A = fill(x,(10,10,10))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10,10))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10,10))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10,10))
            @test round.(elty2,A) == fill(round(elty2,x),(10,10,10))
        end
    end
end

@testset "Types" begin
    for x in (Int16(0), 1, 2f0, pi, 3//4, big(5//6), 7.8, big(9), big(ℯ))
        @test float(typeof(x)) == typeof(float(x))
        @test float(typeof(complex(x, x))) == typeof(float(complex(x, x)))
    end
end

@testset "significant digits" begin
    # (would be nice to have a smart vectorized
    # version of signif)
    @test signif(123.456, 1) ≈ 100.
    @test signif(123.456, 3) ≈ 123.
    @test signif(123.456, 5) ≈ 123.46
    @test signif(123.456, 8, base = 2) ≈ 123.5
    @test signif(123.456, 2, base = 4) ≈ 128.0
    @test signif(0.0, 1) === 0.0
    @test signif(-0.0, 1) === -0.0
    @test signif(1.2, 2) === 1.2
    @test signif(1.0, 6) === 1.0
    @test signif(0.6, 1) === 0.6
    @test signif(7.262839104539736, 2) === 7.3
    @test isinf(signif(Inf, 3))
    @test isnan(signif(NaN, 3))
    @test signif(1.12312, 1000) === 1.12312
    @test signif(Float32(7.262839104539736), 3) === Float32(7.26)
    @test signif(Float32(7.262839104539736), 4) === Float32(7.263)
    @test signif(Float32(1.2), 3) === Float32(1.2)
    @test signif(Float32(1.2), 5) === Float32(1.2)
    @test signif(Float16(0.6), 2) === Float16(0.6)
    @test signif(Float16(1.1), 70) === Float16(1.1)
end
