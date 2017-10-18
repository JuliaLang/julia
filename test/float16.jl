# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

f = Float16(2.)
g = Float16(1.)
@testset "comparisons" begin
    @test f >= g
    @test f > g
    @test g < f
    @test g <= g
    @test all([g g] .< [f f])
    @test all([g g] .<= [f f])
    @test all([f f] .> [g g])
    @test all([f f] .>= [g g])
    @test isless(g, f)
    @test !isless(f, g)

    @test Float16(2.5) == Float16(2.5)
    @test Float16(2.5) != Float16(2.6)
    @test isequal(Float16(0.0), Float16(0.0))
    @test !isequal(Float16(-0.0), Float16(0.0))
    @test !isequal(Float16(0.0), Float16(-0.0))
end

@testset "convert" begin
    @test convert(Bool,Float16(0.0)) == false
    @test convert(Bool,Float16(1.0)) == true
    @test_throws InexactError convert(Bool,Float16(0.1))

    @test convert(Int128,Float16(-2.0)) == Int128(-2)
    @test convert(UInt128,Float16(2.0)) == UInt128(2)

    # convert(::Type{Int128},  x::Float16)
    @test convert(Int128, Float16(1.0)) === Int128(1.0)
    @test convert(Int128, Float16(-1.0)) === Int128(-1.0)
    @test_throws InexactError convert(Int128, Float16(3.5))

    # convert(::Type{UInt128}, x::Float16)
    @test convert(UInt128, Float16(1.0)) === UInt128(1.0)
    @test_throws InexactError convert(UInt128, Float16(3.5))
    @test_throws InexactError convert(UInt128, Float16(-1))

    @test convert(Int128,Float16(-1.0)) == Int128(-1)
    @test convert(UInt128,Float16(5.0)) == UInt128(5)
end

@testset "round, trunc, float, ceil" begin
    @test round(Int,Float16(0.5f0)) == round(Int,0.5f0)
    @test trunc(Int,Float16(0.9f0)) === trunc(Int,0.9f0) === 0
    @test floor(Int,Float16(0.9f0)) === floor(Int,0.9f0) === 0
    @test trunc(Int,Float16(1)) === 1
    @test floor(Int,Float16(1)) === 1
    @test ceil(Int,Float16(0.1f0)) === ceil(Int,0.1f0) === 1
    @test ceil(Int,Float16(0)) === ceil(Int,0) === 0
    @test round(Float16(0.1f0)) == round(0.1f0) == 0
    @test round(Float16(0.9f0)) == round(0.9f0) == 1
    @test trunc(Float16(0.9f0)) == trunc(0.9f0) == 0
    @test floor(Float16(0.9f0)) == floor(0.9f0) == 0
    @test trunc(Float16(1)) === Float16(1)
    @test floor(Float16(1)) === Float16(1)
    @test ceil(Float16(0.1)) == ceil(0.1)
    @test ceil(Float16(0.9)) == ceil(0.9)
    @test unsafe_trunc(UInt8, Float16(3)) === 0x03
    @test unsafe_trunc(Int16, Float16(3)) === Int16(3)
    @test unsafe_trunc(UInt128, Float16(3)) === UInt128(3)
    @test unsafe_trunc(Int128, Float16(3)) === Int128(3)
    @test unsafe_trunc(Int16, NaN16) === Int16(0)  #18771
end
@testset "fma and muladd" begin
    @test fma(Float16(0.1),Float16(0.9),Float16(0.5)) ≈ fma(0.1,0.9,0.5)
    @test muladd(Float16(0.1),Float16(0.9),Float16(0.5)) ≈ muladd(0.1,0.9,0.5)
end
@testset "unary ops" begin
    @test -f === Float16(-2.)
    @test Float16(0.5f0)^2 ≈ Float16(0.5f0^2)
    @test sin(f) ≈ sin(2f0)
    @test log10(Float16(100)) == Float16(2.0)

    # no domain error is thrown for negative values
    @test cbrt(Float16(-1.0)) == -1.0
end
@testset "binary ops" begin
    @test f+g === Float16(3f0)
    @test f-g === Float16(1f0)
    @test f*g === Float16(2f0)
    @test f/g === Float16(2f0)
    @test f^g === Float16(2f0)
    @test f^1 === Float16(2f0)
    @test f^-g === Float16(0.5f0)

    @test f + 2 === Float16(4f0)
    @test f - 2 === Float16(0f0)
    @test f*2 === Float16(4f0)
    @test f/2 === Float16(1f0)
    @test f + 2. === 4.
    @test f - 2. === 0.
    @test f*2. === 4.
    @test f/2. === 1.
end

@testset "NaN16 and Inf16" begin
    @test isnan(NaN16)
    @test isnan(-NaN16)
    @test !isnan(Inf16)
    @test !isnan(-Inf16)
    @test !isnan(Float16(2.6))
    @test NaN16 != NaN16
    @test isequal(NaN16, NaN16)
    @test repr(NaN16) == "NaN16"
    @test sprint(showcompact, NaN16) == "NaN"

    @test isinf(Inf16)
    @test isinf(-Inf16)
    @test !isinf(NaN16)
    @test !isinf(-NaN16)
    @test !isinf(Float16(2.6))
    @test Inf16 == Inf16
    @test Inf16 != -Inf16
    @test -Inf16 < Inf16
    @test isequal(Inf16, Inf16)
    @test repr(Inf16) == "Inf16"
    @test sprint(showcompact, Inf16) == "Inf"

    @test isnan(reinterpret(Float16,0x7c01))
    @test !isinf(reinterpret(Float16,0x7c01))

    @test nextfloat(Inf16) === Inf16
    @test prevfloat(-Inf16) === -Inf16
end

@test repr(Float16(44099)) == "Float16(4.41e4)"

@testset "signed zeros" begin
    for z1 in (Float16(0.0), Float16(-0.0)), z2 in (Float16(0.0), Float16(-0.0))
        @test z1 == z2
        @test isequal(z1, z1)
        @test z1 === z1
        for elty in (Float32, Float64)
            z3 = convert(elty, z2)
            @test z1==z3
        end
    end
end

@testset "rounding in conversions" begin
    for f32 in [.3325f0, -.3325f0]
        f16 = Float16(f32)
        # need to round away from 0. make sure we picked closest number.
        @test abs(f32 - f16) < abs(f32 - nextfloat(f16))
        @test abs(f32 - f16) < abs(f32 - prevfloat(f16))
    end
    # halfway between and last bit is 1
    f = reinterpret(Float32,                           0b00111110101010100011000000000000)
    @test Float32(Float16(f)) === reinterpret(Float32, 0b00111110101010100100000000000000)
    # halfway between and last bit is 0
    f = reinterpret(Float32,                           0b00111110101010100001000000000000)
    @test Float32(Float16(f)) === reinterpret(Float32, 0b00111110101010100000000000000000)
end

# issue #5948
@test string(reinterpret(Float16, 0x7bff)) == "6.55e4"

#  #9939 (and #9897)
@test rationalize(Float16(0.1)) == 1//10

# issue #17148
@test rem(Float16(1.2), Float16(one(1.2))) == 0.20019531f0
