# This file is a part of Julia. License is MIT: https://julialang.org/license

# Small sanity tests to ensure changing the rounding of float functions work
using Base.MathConstants

using Test

@testset "Float64 checks" begin
    # a + b returns a number exactly between prevfloat(1.) and 1., so its
    # final result depends strongly on the utilized rounding direction.
    a = prevfloat(0.5)
    b = 0.5
    c = 0x1p-54
    d = prevfloat(1.)

    @testset "Default rounding direction, RoundNearest" begin
        @test a + b === 1.
        @test - a - b === -1.
        @test a - b === -c
        @test b - a === c
    end
    @testset "RoundToZero" begin
        setrounding(Float64,RoundToZero) do
            @test a + b === d
            @test - a - b === -d
            @test a - b === -c
            @test b - a === c
        end
        # Sanity check to see if we have returned to RoundNearest
        @test a + b === 1.
        @test - a - b === -1.
        @test a - b == -c
        @test b - a == c
    end

    @testset "RoundUp" begin
        setrounding(Float64,RoundUp) do
            @test a + b === 1.
            @test - a - b === -d
            @test a - b === -c
            @test b - a === c
        end
    end
    @testset "RoundDown" begin
        setrounding(Float64,RoundDown) do
            @test a + b === d
            @test - a - b === -1.
            @test a - b === -c
            @test b - a === c
        end
    end
end

@testset "Float32 checks" begin
    a32 = prevfloat(0.5f0)
    b32 = 0.5f0
    c32 = (1.f0 - prevfloat(1.f0))/2
    d32 = prevfloat(1.0f0)

    @testset "Default rounding direction, RoundNearest" begin
        @test a32 + b32 === 1.0f0
        @test - a32 - b32 === -1.0f0
        @test a32 - b32 === -c32
        @test b32 - a32 === c32
    end
    @testset "RoundToZero" begin
        setrounding(Float32,RoundToZero) do
            @test a32 + b32 === d32
            @test - a32 - b32 === -d32
            @test a32 - b32 === -c32
            @test b32 - a32 === c32
        end

        # Sanity check to see if we have returned to RoundNearest
        @test a32 + b32 === 1.0f0
        @test - a32 - b32 === -1.0f0
        @test a32 - b32 == -c32
        @test b32 - a32 == c32
    end
    @testset "RoundUp" begin
        setrounding(Float32,RoundUp) do
            @test a32 + b32 === 1.0f0
            @test - a32 - b32 === -d32
            @test a32 - b32 === -c32
            @test b32 - a32 === c32
        end
    end
    @testset "RoundDown" begin
        setrounding(Float32,RoundDown) do
            @test a32 + b32 === d32
            @test - a32 - b32 === -1.0f0
            @test a32 - b32 === -c32
            @test b32 - a32 === c32
        end
    end
end

@testset "convert with rounding" begin
    for v = [sqrt(2),-1/3,nextfloat(1.0),prevfloat(1.0),nextfloat(-1.0),
             prevfloat(-1.0),nextfloat(0.0),prevfloat(0.0)]
        pn = Float32(v,RoundNearest)
        @test pn == convert(Float32,v)
        pz = Float32(v,RoundToZero)
        @test pz == setrounding(()->convert(Float32,v), Float64, RoundToZero)
        pd = Float32(v,RoundDown)
        @test pd == setrounding(()->convert(Float32,v), Float64, RoundDown)
        pu = Float32(v,RoundUp)
        @test pu == setrounding(()->convert(Float32,v), Float64, RoundUp)

        @test pn == pd || pn == pu
        @test v > 0 ? pz == pd : pz == pu
        @test pu - pd == eps(pz)
    end

    for T in [Float32,Float64]
        for v in [sqrt(big(2.0)),-big(1.0)/big(3.0),nextfloat(big(1.0)),
                  prevfloat(big(1.0)),nextfloat(big(0.0)),prevfloat(big(0.0)),
                  pi,ℯ,eulergamma,catalan,golden,
                  typemax(Int64),typemax(UInt64),typemax(Int128),typemax(UInt128),0xa2f30f6001bb2ec6]
            pn = T(v,RoundNearest)
            @test pn == convert(T,BigFloat(v))
            pz = T(v,RoundToZero)
            @test pz == setrounding(()->convert(T,BigFloat(v)), BigFloat, RoundToZero)
            pd = T(v,RoundDown)
            @test pd == setrounding(()->convert(T,BigFloat(v)), BigFloat, RoundDown)
            pu = T(v,RoundUp)
            @test pu == setrounding(()->convert(T,BigFloat(v)), BigFloat, RoundUp)

            @test pn == pd || pn == pu
            @test v > 0 ? pz == pd : pz == pu
            @test isinf(pu) || pu - pd == eps(pz)
        end
    end
end
@testset "fenv" begin
    @test Base.Rounding.from_fenv(Base.Rounding.to_fenv(RoundNearest)) == RoundNearest
    @test Base.Rounding.from_fenv(Base.Rounding.to_fenv(RoundToZero)) == RoundToZero
    @test Base.Rounding.from_fenv(Base.Rounding.to_fenv(RoundUp)) == RoundUp
    @test Base.Rounding.from_fenv(Base.Rounding.to_fenv(RoundDown)) == RoundDown
    @test_throws ArgumentError Base.Rounding.from_fenv(-99)
end

@testset "round error throwing" begin
    badness = 1//0
    @test_throws DivideError round(Int64,badness,RoundNearestTiesAway)
    @test_throws DivideError round(Int64,badness,RoundNearestTiesUp)
end

@testset "rounding difficult values" begin
    for x = 2^53-10:2^53+10
        y = Float64(x)
        i = trunc(Int64,y)
        @test Int64(trunc(y)) == i
        @test Int64(round(y)) == i
        @test Int64(floor(y)) == i
        @test Int64(ceil(y))  == i

        @test round(Int64,y)       == i
        @test floor(Int64,y)       == i
        @test ceil(Int64,y)        == i
    end

    for x = 2^24-10:2^24+10
        y = Float32(x)
        i = trunc(Int,y)
        @test Int(trunc(y)) == i
        @test Int(round(y)) == i
        @test Int(floor(y)) == i
        @test Int(ceil(y))  == i
        @test round(Int,y)     == i
        @test floor(Int,y)     == i
        @test ceil(Int,y)      == i
    end

    # rounding vectors
    let ≈(x,y) = x==y && typeof(x)==typeof(y)
        for t in [Float32,Float64]
            # try different vector lengths
            for n in [0,3,255,256]
                r = (1:n) .- div(n,2)
                y = t[x/4 for x in r]
                @test trunc.(y) ≈ t[div(i,4) for i in r]
                @test floor.(y) ≈ t[i>>2 for i in r]
                @test ceil.(y)  ≈ t[(i+3)>>2 for i in r]
                @test round.(y) ≈ t[(i+1+isodd(i>>2))>>2 for i in r]
                @test broadcast(x -> round(x, RoundNearestTiesAway), y) ≈ t[(i+1+(i>=0))>>2 for i in r]
                @test broadcast(x -> round(x, RoundNearestTiesUp), y) ≈ t[(i+2)>>2 for i in r]
            end
        end
    end

    @test_throws InexactError round(Int,Inf)
    @test_throws InexactError round(Int,NaN)
    @test round(Int,2.5) == 2
    @test round(Int,1.5) == 2
    @test round(Int,-2.5) == -2
    @test round(Int,-1.5) == -2
    @test round(Int,2.5,RoundNearestTiesAway) == 3
    @test round(Int,1.5,RoundNearestTiesAway) == 2
    @test round(Int,2.5,RoundNearestTiesUp) == 3
    @test round(Int,1.5,RoundNearestTiesUp) == 2
    @test round(Int,-2.5,RoundNearestTiesAway) == -3
    @test round(Int,-1.5,RoundNearestTiesAway) == -2
    @test round(Int,-2.5,RoundNearestTiesUp) == -2
    @test round(Int,-1.5,RoundNearestTiesUp) == -1
    @test round(Int,-1.9) == -2
    @test_throws InexactError round(Int64, 9.223372036854776e18)
    @test       round(Int64, 9.223372036854775e18) == 9223372036854774784
    @test_throws InexactError round(Int64, -9.223372036854778e18)
    @test       round(Int64, -9.223372036854776e18) == typemin(Int64)
    @test_throws InexactError round(UInt64, 1.8446744073709552e19)
    @test       round(UInt64, 1.844674407370955e19) == 0xfffffffffffff800
    @test_throws InexactError round(Int32, 2.1474836f9)
    @test       round(Int32, 2.1474835f9) == 2147483520
    @test_throws InexactError round(Int32, -2.147484f9)
    @test       round(Int32, -2.1474836f9) == typemin(Int32)
    @test_throws InexactError round(UInt32, 4.2949673f9)
    @test       round(UInt32, 4.294967f9) == 0xffffff00


    for Ti in [Int,UInt]
        for Tf in [Float16,Float32,Float64]

            @test round(Ti,Tf(-0.0)) == 0
            @test round(Ti,Tf(-0.0),RoundNearestTiesAway) == 0
            @test round(Ti,Tf(-0.0),RoundNearestTiesUp) == 0

            @test round(Ti, Tf(0.5)) == 0
            @test round(Ti, Tf(0.5), RoundNearestTiesAway) == 1
            @test round(Ti, Tf(0.5), RoundNearestTiesUp) == 1

            @test round(Ti, prevfloat(Tf(0.5))) == 0
            @test round(Ti, prevfloat(Tf(0.5)), RoundNearestTiesAway) == 0
            @test round(Ti, prevfloat(Tf(0.5)), RoundNearestTiesUp) == 0

            @test round(Ti, nextfloat(Tf(0.5))) == 1
            @test round(Ti, nextfloat(Tf(0.5)), RoundNearestTiesAway) == 1
            @test round(Ti, nextfloat(Tf(0.5)), RoundNearestTiesUp) == 1

            @test round(Ti, Tf(-0.5)) == 0
            @test round(Ti, Tf(-0.5), RoundNearestTiesUp) == 0

            @test round(Ti, nextfloat(Tf(-0.5))) == 0
            @test round(Ti, nextfloat(Tf(-0.5)), RoundNearestTiesAway) == 0
            @test round(Ti, nextfloat(Tf(-0.5)), RoundNearestTiesUp) == 0

            if Ti <: Signed
                @test round(Ti, Tf(-0.5), RoundNearestTiesAway) == -1
                @test round(Ti, prevfloat(Tf(-0.5))) == -1
                @test round(Ti, prevfloat(Tf(-0.5)), RoundNearestTiesAway) == -1
                @test round(Ti, prevfloat(Tf(-0.5)), RoundNearestTiesUp) == -1
            else
                @test_throws InexactError round(Ti, Tf(-0.5), RoundNearestTiesAway)
                @test_throws InexactError round(Ti, prevfloat(Tf(-0.5)))
                @test_throws InexactError round(Ti, prevfloat(Tf(-0.5)), RoundNearestTiesAway)
                @test_throws InexactError round(Ti, prevfloat(Tf(-0.5)), RoundNearestTiesUp)
            end
        end
    end

    # numbers that can't be rounded by trunc(x+0.5)
    @test round(Int64, 2.0^52 + 1) == 4503599627370497
    @test round(Int32, 2.0f0^23 + 1) == 8388609
end

# custom rounding and significant-digit ops
@testset "rounding to digits relative to the decimal point" begin
    @test round(pi,0) ≈ 3.
    @test round(pi,1) ≈ 3.1
    @test round(10*pi,-1) ≈ 30.
    @test round(.1,0) == 0.
    @test round(-.1,0) == -0.
    @test isnan(round(NaN, 2))
    @test isinf(round(Inf,2))
    @test isinf(round(-Inf,2))
end
@testset "round vs trunc vs floor vs ceil" begin
    @test round(123.456,1) ≈ 123.5
    @test round(-123.456,1) ≈ -123.5
    @test trunc(123.456,1) ≈ 123.4
    @test trunc(-123.456,1) ≈ -123.4
    @test ceil(123.456,1) ≈ 123.5
    @test ceil(-123.456,1) ≈ -123.4
    @test floor(123.456,1) ≈ 123.4
    @test floor(-123.456,1) ≈ -123.5
end
@testset "rounding with too much (or too few) precision" begin
    for x in (12345.6789, 0, -12345.6789)
        y = float(x)
        @test y == trunc(x, 1000)
        @test y == round(x, 1000)
        @test y == floor(x, 1000)
        @test y == ceil(x, 1000)
    end
    let x = 12345.6789
        @test 0.0 == trunc(x, -1000)
        @test 0.0 == round(x, -1000)
        @test 0.0 == floor(x, -1000)
        @test Inf == ceil(x, -1000)
    end
    let x = -12345.6789
        @test -0.0 == trunc(x, -1000)
        @test -0.0 == round(x, -1000)
        @test -Inf == floor(x, -1000)
        @test -0.0 == ceil(x, -1000)
    end
    let x = 0.0
        @test 0.0 == trunc(x, -1000)
        @test 0.0 == round(x, -1000)
        @test 0.0 == floor(x, -1000)
        @test 0.0 == ceil(x, -1000)
    end
end
@testset "rounding in other bases" begin
    @test round(pi, 2, base = 2) ≈ 3.25
    @test round(pi, 3, base = 2) ≈ 3.125
    @test round(pi, 3, base = 5) ≈ 3.144
end
@testset "vectorized trunc/round/floor/ceil with digits/base argument" begin
    a = rand(2, 2, 2)
    for f in (round, trunc, floor, ceil)
        @test f.(a[:, 1, 1], 2) == map(x->f(x, 2), a[:, 1, 1])
        @test f.(a[:, :, 1], 2) == map(x->f(x, 2), a[:, :, 1])
        @test f.(a, 9, base = 2) == map(x->f(x, 9, base = 2), a)
        @test f.(a[:, 1, 1], 9, base = 2) == map(x->f(x, 9, base = 2), a[:, 1, 1])
        @test f.(a[:, :, 1], 9, base = 2) == map(x->f(x, 9, base = 2), a[:, :, 1])
        @test f.(a, 9, base = 2) == map(x->f(x, 9, base = 2), a)
    end
end
