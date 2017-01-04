# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

f = Float16(2.)
g = Float16(1.)

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

@test Float16(0.5f0)^2 ≈ Float16(0.5f0^2)
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
@test fma(Float16(0.1),Float16(0.9),Float16(0.5)) ≈ fma(0.1,0.9,0.5)
@test muladd(Float16(0.1),Float16(0.9),Float16(0.5)) ≈ muladd(0.1,0.9,0.5)
@test convert(Int128,Float16(-1.0)) == Int128(-1)
@test convert(UInt128,Float16(5.0)) == UInt128(5)

@test -f === Float16(-2.)

@test f+g === Float16(3f0)
@test f-g === Float16(1f0)
@test f*g === Float16(2f0)
@test f/g === Float16(2f0)
@test f^g === Float16(2f0)
@test f^-g === Float16(0.5f0)

@test f + 2 === Float16(4f0)
@test f - 2 === Float16(0f0)
@test f*2 === Float16(4f0)
@test f/2 === Float16(1f0)
@test f + 2. === 4.
@test f - 2. === 0.
@test f*2. === 4.
@test f/2. === 1.

@test sin(f) ≈ sin(2f0)

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

@test repr(Float16(44099)) == "Float16(4.41e4)"

for z1 in (Float16(0.0), Float16(-0.0)), z2 in (Float16(0.0), Float16(-0.0))
    @test z1 == z2
    @test isequal(z1, z1)
    @test z1 === z1
    for elty in (Float32, Float64)
        z3 = convert(elty, z2)
        @test z1==z3
    end
end

@test Float16(2.5) == Float16(2.5)
@test Float16(2.5) != Float16(2.6)
@test isequal(Float16(0.0), Float16(0.0))
@test !isequal(Float16(-0.0), Float16(0.0))
@test !isequal(Float16(0.0), Float16(-0.0))

@test isnan(reinterpret(Float16,0x7c01))
@test !isinf(reinterpret(Float16,0x7c01))

@test nextfloat(Inf16) === Inf16
@test prevfloat(-Inf16) === -Inf16

# rounding in conversions
let
    for f in [.3325f0, -.3325f0]
        f16 = Float16(f)
        # need to round away from 0. make sure we picked closest number.
        @test abs(f-f16) < abs(f-nextfloat(f16))
        @test abs(f-f16) < abs(f-prevfloat(f16))
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

@test log10(Float16(100)) == Float16(2.0)

#  #9939 (and #9897)
@test rationalize(Float16(0.1)) == 1//10

# issue #17148
@test rem(Float16(1.2), Float16(one(1.2))) == 0.20019531f0

# no domain error is thrown for negative values
@test cbrt(Float16(-1.0)) == -1.0
