# This file is a part of Julia. License is MIT: https://julialang.org/license

# Small sanity tests to ensure changing the rounding of float functions work
using Base.MathConstants

using Test
using Random

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
end

@testset "convert with rounding" begin
    for v = [sqrt(2),-1/3,nextfloat(1.0),prevfloat(1.0),nextfloat(-1.0),
             prevfloat(-1.0),nextfloat(0.0),prevfloat(0.0)]

        pn = Float32(v,RoundNearest)
        @test pn == convert(Float32,v)

        pz = Float32(v,RoundToZero)
        @test abs(pz) <= abs(v) < nextfloat(abs(pz))
        @test signbit(pz) == signbit(v)

        pd = Float32(v,RoundDown)
        @test pd <= v < nextfloat(pd)

        pu = Float32(v,RoundUp)
        @test prevfloat(pu) < v <= pu

        @test pn == pd || pn == pu
        @test v > 0 ? pz == pd : pz == pu
        @test pu - pd == eps(pz)
    end

    for T in [Float16,Float32,Float64]
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

@testset "rounding properties" for Tf in [Float16,Float32,Float64]
    # these should hold for all u, but we just test the smallest and largest
    # of each binade

    for i in exponent(floatmin(Tf)):exponent(floatmax(Tf))
        for u in [ldexp(Tf(1.0), i), -ldexp(Tf(1.0), i),
                  ldexp(prevfloat(Tf(2.0)), i), -ldexp(prevfloat(Tf(2.0)), i)]

            r = round(u, RoundNearest)
            if isfinite(u)
                @test isfinite(r)
                @test isinteger(r)
                @test abs(r-u) < 0.5 || abs(r-u) == 0.5 && isinteger(r/2)
                @test signbit(u) == signbit(r)
            else
                @test u === r
            end

            r = round(u, RoundNearestTiesAway)
            if isfinite(u)
                @test isfinite(r)
                @test isinteger(r)
                @test abs(r-u) < 0.5 || (r-u) == copysign(0.5,u)
                @test signbit(u) == signbit(r)
            else
                @test u === r
            end

            r = round(u, RoundNearestTiesUp)
            if isfinite(u)
                @test isfinite(r)
                @test isinteger(r)
                @test -0.5 < r-u <= 0.5
                @test signbit(u) == signbit(r)
            else
                @test u === r
            end

            r = round(u, RoundFromZero)
            if isfinite(u)
                @test isfinite(r)
                @test isinteger(r)
                @test signbit(u) ? (r == floor(u)) : (r == ceil(u))
                @test signbit(u) == signbit(r)
            else
                @test u === r
            end
        end
    end
end

@testset "rounding difficult values" begin
    for x = Int64(2)^53-10:Int64(2)^53+10
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
                @test broadcast(x -> round(x, RoundFromZero), y) ≈ t[(i+3*(i>=0))>>2 for i in r]
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
    @test round(Int,nextfloat(1.0),RoundFromZero) == 2
    @test round(Int,-nextfloat(1.0),RoundFromZero) == -2
    @test round(Int,prevfloat(1.0),RoundFromZero) == 1
    @test round(Int,-prevfloat(1.0),RoundFromZero) == -1
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
    @test round(pi) ≈ 3.
    @test round(pi, base=10) ≈ 3.
    @test round(pi, digits=0) ≈ 3.
    @test round(pi, digits=1) ≈ 3.1
    @test round(pi, digits=3, base=2) ≈ 3.125
    @test round(pi, sigdigits=1) ≈ 3.
    @test round(pi, sigdigits=3) ≈ 3.14
    @test round(pi, sigdigits=4, base=2) ≈ 3.25
    @test round(big(pi)) ≈ big"3."
    @test round(big(pi), digits=0) ≈ big"3."
    @test round(big(pi), digits=1) ≈ big"3.1"
    @test round(big(pi), digits=3, base=2) ≈ big"3.125"
    @test round(big(pi), sigdigits=1) ≈ big"3."
    @test round(big(pi), sigdigits=3) ≈ big"3.14"
    @test round(big(pi), sigdigits=4, base=2) ≈ big"3.25"
    @test round(10*pi, digits=-1) ≈ 30.
    @test round(.1, digits=0) == 0.
    @test round(-.1, digits=0) == -0.
    @test isnan(round(NaN, digits=2))
    @test isinf(round(Inf, digits=2))
    @test isinf(round(-Inf, digits=2))
end
@testset "round vs trunc vs floor vs ceil" begin
    @test round(123.456, digits=1) ≈ 123.5
    @test round(-123.456, digits=1) ≈ -123.5
    @test trunc(123.456, digits=1) ≈ 123.4
    @test trunc(-123.456, digits=1) ≈ -123.4
    @test ceil(123.456, digits=1) ≈ 123.5
    @test ceil(-123.456, digits=1) ≈ -123.4
    @test floor(123.456, digits=1) ≈ 123.4
    @test floor(-123.456, digits=1) ≈ -123.5
end
@testset "rounding to digits is exact w.r.t. the value of x" begin
    modes = (RoundDown, RoundUp, RoundToZero, RoundFromZero,
             RoundNearest, RoundNearestTiesAway, RoundNearestTiesUp)
    # `round(x * base^d, r) / base^d` on the exact value of `x`, rounded once at the end
    function refround(x::T, r::RoundingMode, d::Integer, base::Integer=10) where {T<:AbstractFloat}
        setprecision(BigFloat, 4 * max(precision(x), 53) + 4 * abs(d) + 64) do
            s = big(base)^abs(d)
            y = d >= 0 ? round(big(x) * s, r) / s : round(big(x) / s, r) * s
            return T <: BigFloat ? BigFloat(y; precision=precision(x)) : T(y)
        end
    end

    # the rounded product used to land exactly on a rounding boundary
    let x = float(2^53 - 4)
        for f in (floor, ceil, trunc, round)
            @test f(x; digits=1) === x
        end
    end
    @test floor(2.52379309758e8; digits=3) === 2.52379309757e8
    @test ceil(4.312747009431438e17; digits=-2) === nextfloat(4.312747009431438e17)
    @test floor(0.29; digits=2) === 0.28
    @test round(1.15; digits=1) === 1.1
    @test round(2.675; digits=2) === 2.67
    @test round(0.5; digits=0) === 0.0
    @test round(1.5; digits=0) === 2.0
    @test round(Float32(1.15); digits=1) === 1.1f0            # also below 1.15
    @test round(1.25; digits=1) === 1.2                        # exact ties are still ties
    @test round(1.25, RoundNearestTiesAway; digits=1) === 1.3
    @test round(-1.25, RoundNearestTiesUp; digits=1) === -1.2
    # signed zeros
    @test round(-0.0; digits=2) === -0.0
    @test round(-0.1; digits=0) === -0.0
    @test ceil(-0.4; digits=0) === -0.0
    @test trunc(-1e-320; digits=-5) === -0.0
    @test floor(-1e-320; digits=-5) === -1e5

    # the primitives: `hi + lo == round(x * y, r)` exactly, even when not representable
    hilo(t::Base.TwicePrecision) = (t.hi, t.lo)
    let x = 2.0^53 - 4, y = 10.0
        for r in modes
            n, m = hilo(Base._mul_round(x, y, r))
            @test isinteger(n) && isinteger(m)
            @test big(n) + big(m) == round(big(x) * big(y), r)
        end
        @test hilo(Base._mul_round(1.15, 10.0, RoundNearest)) == (11.0, 0.0)
        for r in modes, y in (100.0, -100.0)
            n, m = hilo(Base._div_round(4.312747009431438e17, y, r))
            @test big(n) + big(m) == round(big(4.312747009431438e17) / y, r)
            n, m = hilo(Base._div_round(2.0^60 + 2.0^8, y / 10, r))
            @test big(n) + big(m) == round((big(2)^60 + 2^8) / (y / 10), r)
        end
        # `_mul_round` is exact at any magnitude; `_div_round` up to |x/y| < maxintfloat^2/4,
        # and within its documented bound beyond that
        rng = Random.MersenneTwister(0xf00d)
        for _ in 1:20, r in modes
            x = ldexp(rand(rng) + 1, rand(rng, 100:900)) * rand(rng, (-1.0, 1.0)); y = (rand(rng) + 1) * rand(rng, (-1.0, 1.0))
            n, m = hilo(Base._mul_round(x, y, r))
            @test big(n) + big(m) == round(big(x) * big(y), r)
            x = ldexp(rand(rng) + 1, rand(rng, 60:80)) * rand(rng, (-1.0, 1.0)); y = ldexp(rand(rng) + 1, -20) * rand(rng, (-1.0, 1.0))
            @test abs(x / y) < 2.0^104
            n, m = hilo(Base._div_round(x, y, r))
            @test big(n) + big(m) == round(big(x) / big(y), r)
            x = ldexp(rand(rng) + 1, rand(rng, 120:200)) * rand(rng, (-1.0, 1.0))
            n, m = hilo(Base._div_round(x, y, r))
            @test abs(big(n) + big(m) - round(big(x) / big(y), r)) <= big(eps(m)) / 2 + 1
        end
        for r in modes
            @test Base._mul_round(1e200, 1e200, r) === Base.TwicePrecision(Inf, 0.0)
            @test isnan(Base._mul_round(NaN, 1.0, r).hi)
            @test Base._div_round(1.0, 0.0, r) === Base.TwicePrecision(Inf, 0.0)
            @test Base._div_round(-1.0, Inf, r) === Base.TwicePrecision(-0.0, 0.0)
        end
    end

    # sweep against the exact reference, including magnitudes where the scaled value is
    # not representable, and where the result is x itself
    rng = Random.MersenneTwister(0xc0ffee)
    for (T, exps, ds) in ((Float64, -20:70, -4:4), (Float32, -20:40, -3:3), (Float16, -24:14, -6:9),
                          (Float64, 1000:1023, 0:3), (Float64, -1074:-1040, -2:2),
                          (Float64, -50:20, 15:22), (Float64, 40:120, -22:-15)),
        _ in 1:150
        x = T(ldexp(rand(rng) + 1, rand(rng, exps))) * rand(rng, (-one(T), one(T)))
        for d in ds
            @test floor(x; digits=d) <= x <= ceil(x; digits=d)
            @test abs(trunc(x; digits=d)) <= abs(x)
            for r in modes
                @test round(x, r; digits=d) === refround(x, r, d)
            end
        end
    end
    for base in (2, 3, 16), _ in 1:50
        x = ldexp(rand(rng) + 1, rand(rng, -10:60)) * rand(rng, (-1.0, 1.0))
        for d in -3:6, r in modes
            @test round(x, r; digits=d, base=base) === refround(x, r, d, base)
        end
    end

    # base^digits overflows: Float16 from digits == 5, which ordinary values reach
    @test ceil(Float16(0.0001); digits=5) === Float16(0.00011)
    @test floor(Float16(0.0001); digits=5) === Float16(0.0001)
    @test floor(floatmax(Float16); digits=-1) === floatmax(Float16)
    @test trunc(-floatmax(Float16); digits=-1) === -floatmax(Float16)
    @test ceil(4.761846424831262e-296; digits=317) >= 4.761846424831262e-296
    @test floor(1.7110486385708392e-305; digits=328) <= 1.7110486385708392e-305
    @test round(-1e-320; digits=309) === -0.0
    @test round(5e-324, RoundUp; digits=323) === 1e-323
    # bases that overflow the float type, and extreme digits
    @test ceil(nextfloat(0f0); digits=1, base=typemax(UInt128)) === Float32(1 / big(typemax(UInt128)))
    @test floor(-5e-324; digits=1, base=big(2)^1030) === -2.0^-1030
    @test floor(1.5; digits=typemin(Int)) === 0.0
    @test ceil(1.5; digits=typemin(Int)) === Inf
    # 10^digits overflows the float type here, and the results are subnormal at times
    for (T, exps, ds) in ((Float32, -150:-100, 38:46), (Float64, -1075:-950, 308:325)), _ in 1:40
        x = T(ldexp(rand(rng) + 1, rand(rng, exps))) * rand(rng, (-one(T), one(T)))
        for d in ds, r in modes
            @test round(x, r; digits=d) === refround(x, r, d)
        end
    end

    # base^|digits| is finite but not exact: the grid is still that of the exact power
    @test floor(floatmax(); digits=-299) === 1.797693134e308
    @test round(prevfloat(floatmax(), 4); sigdigits=15) === 1.79769313486231e308
    @test round(floatmax(); digits=-377, base=6) === 1.7976931348623147e308
    @test trunc(floatmax(Float32); sigdigits=8, base=11) === 3.4028233f38
    @test round(-prevfloat(floatmax(Float32), 3), RoundDown; digits=-31, base=11) === -3.4028229f38
    @test round(1.2e23; digits=-23) === 1e23           # 10^23 is halfway between floats
    @test ceil(1.0e23; digits=-23) === 1e23
    @test round(1.5e23; digits=-23) === 2e23
    @test round(1e30; sigdigits=3) === 1e30
    @test round(1.234567e-30; sigdigits=3) === 1.23e-30
    @test floor(1.2345e-25; digits=27) === 1.23e-25
    for (T, exps, ds) in ((Float64, 60:200, -60:-23), (Float64, -200:-60, 23:60),
                          (Float32, 25:120, -36:-11), (Float32, -120:-25, 11:36),
                          (Float64, 1000:1023, -308:-290), (Float32, 120:127, -38:-30)),
        _ in 1:60
        x = T(ldexp(rand(rng) + 1, rand(rng, exps))) * rand(rng, (-one(T), one(T)))
        d = rand(rng, ds)
        for r in modes, base in (10, 3, 6)
            @test round(x, r; digits=d, base=base) === refround(x, r, d, base)
        end
        for s in (1, 3, 9), r in modes
            @test round(x, r; sigdigits=s) === refround(x, r, s - Base.hidigit(x, 10))
        end
    end
    # digits whose absolute value overflows its type
    @test round(1e130; digits=Int8(-128)) === 1e130
    @test round(1f30, RoundUp; digits=Int8(-128)) === Inf32
    @test round(1e300; digits=Int8(-128), base=7) === 1e300
    @test round(1//3; digits=Int8(-128)) === 0.0
    # decimal literals sit close to the grid of their own power of ten
    for e in 23:300, m in (1, 15, 999)
        x = parse(Float64, "$(m)e$e")
        for r in (RoundDown, RoundUp, RoundNearest)
            @test round(x, r; sigdigits=3) === refround(x, r, 3 - Base.hidigit(x, 10))
        end
    end
    @test_throws ArgumentError round(1e25, RoundingMode{:Foo}(); digits=-23)
    # the band where base^digits is just beyond floatmax
    for (T, base) in ((Float64, 3), (Float64, 7), (Float32, 3), (Float32, 11)), i in 0:20
        k = ceil(Int, log(base, big(floatmax(T))))
        x = prevfloat(floatmax(T), 1000i)
        for r in modes
            @test round(x, r; digits=-k, base=base) === refround(x, r, -k, base)
        end
    end

    # BigFloat
    setprecision(BigFloat, 256) do
        x = big(2)^300 - big(2)^248
        for f in (floor, ceil, trunc, round)
            @test f(x; digits=1) == x
        end
        for _ in 1:30
            x = ldexp(1 + big(rand(rng)) + big(rand(rng)) * big(2)^-53 + big(rand(rng)) * big(2)^-106 + big(rand(rng)) * big(2)^-159,
                      rand(rng, -20:300)) * rand(rng, (-1, 1))
            for d in (-3, -1, 0, 1, 3, 20, 60), r in modes
                y = round(x, r; digits=d)
                @test y == refround(x, r, d)
                @test signbit(y) == signbit(refround(x, r, d))
            end
        end
        # exact ties, and other bases
        for k in -40:40, d in -2:2, r in modes, base in (10, 3)
            x = big(k) / 8
            e = d >= 0 ? round(x * big(base)^d, r) / big(base)^d : round(x / big(base)^-d, r) * big(base)^-d
            @test round(x, r; digits=d, base=base) == e
        end
        @test signbit(round(big"-0.01"; digits=1)) && signbit(ceil(big"-0.01"; digits=1))
        @test floor(big"-0.01"; digits=1) == -big"0.1"
        @test_throws ArgumentError round(big(1.5), RoundingMode{:Foo}(); digits=1)
        # a BigInt base is not modified
        b = big(10)
        @test round(big"1.23456"; digits=2, base=b) == big"1.23"
        @test b == 10
        # extreme digits and bases
        @test round(big"1.5"; digits=-10^9) == 0
        @test round(big"1.5", RoundUp; digits=-10^9) == big(10.0)^(10^9)
        @test round(big"1.5", RoundUp; digits=typemin(Int)) == Inf
        @test round(big"-1.5", RoundDown; digits=-3) == -1000
        @test round(big"1.5"; digits=1, base=-10) == 1.5
        y = big(2)^-2000 * (1 + big(2)^-100)
        @test ceil(y; digits=1, base=big(2)^1100) == big(2)^-1100
        @test floor(y; digits=1, base=big(2)^1100) == 0
    end
    # the result has the current precision, whatever the precision of x
    for (px, p) in ((300, 64), (64, 300), (200, 64))
        for x in (BigFloat(π; precision=px), BigFloat(12; precision=px), BigFloat(2.0^255; precision=px))
            setprecision(BigFloat, p) do
                for d in (-1, 0, 2, 10), r in modes
                    y = round(x, r; digits=d)
                    @test precision(y) == p
                    @test y == BigFloat(refround(BigFloat(x; precision=4000), r, d); precision=p)
                end
            end
        end
    end
    setprecision(BigFloat, 2) do
        @test round(BigFloat(7674; precision=256); digits=0) == 8192
    end
    # x has more precision, is halfway between floats of the current one, and is not a
    # multiple of the step, so the side of x that the exact result is on decides
    let x = setprecision(() -> 1 + BigFloat(2)^-100, 200)
        setprecision(BigFloat, 100) do
            @test ceil(x; digits=70) == 1 + big(2.0)^-99
            @test round(x, RoundFromZero; digits=70) == 1 + big(2.0)^-99
            @test floor(x; digits=70) == 1
        end
    end
    # the result is rounded to nearest, whatever the rounding mode of BigFloat
    for x in (big"0.0027", big(1) / 3, big(π), big(2)^300 / 3, big"12345.678", BigFloat(π; precision=300)), s in (1, -1),
            (d, base) in ((3, 3), (100, 10), (-2, 10), (2, 10), (0, 2), (5, 7)), r in modes
        sx = s > 0 ? x : -x   # exact, at the precision of x
        want = round(sx, r; digits=d, base=base)
        for rm in (RoundUp, RoundDown, RoundToZero, RoundFromZero)
            @test setrounding(() -> round(sx, r; digits=d, base=base), BigFloat, rm) == want
        end
    end
end
@testset "rounding Rationals to digits is exact" begin
    modes = (RoundDown, RoundUp, RoundToZero, RoundFromZero,
             RoundNearest, RoundNearestTiesAway, RoundNearestTiesUp)
    # round the exact rational, then convert once
    function refround(x::Rational, r::RoundingMode, d::Integer, base::Integer=10)
        T = typeof(float(x))
        b = big(base)^abs(d)
        v = d >= 0 ? round(Rational{BigInt}(x) * b, r) // b : round(Rational{BigInt}(x) // b, r) * b
        iszero(v) && return x < 0 ? -zero(T) : zero(T)
        return T <: BigFloat ? BigFloat(v) : T(setprecision(() -> BigFloat(v), BigFloat, 5000))
    end
    @test round(23//20; digits=1) === 1.2               # an exact tie, to even
    @test floor(big(23)//20; digits=2) == big"1.15"     # float(23//20) is below 1.15
    @test round(-1//100; digits=1) === -0.0
    @test round(Int8(1)//Int8(3); digits=2) === 0.33
    rng = Random.MersenneTwister(0xbeef)
    for _ in 1:200
        num, den = rand(rng, -10^6:10^6), rand(rng, 1:10^4)
        for x in (num//den, big(num)//den, Int16(num % 3000)//Int16(max(den % 300, 1)))
            for d in (-3, -1, 0, 1, 2, 5), base in (10, 3), r in modes
                y = round(x, r; digits=d, base=base)
                @test y == refround(x, r, d, base)
                @test signbit(y) == signbit(refround(x, r, d, base))
                @test typeof(y) == typeof(float(x))
            end
        end
    end    # to nearest, whatever the rounding mode of BigFloat
    for x in (big(1)//3, -big(2)//3, big(10)^40//7), d in (-1, 2, 30), r in modes
        @test setrounding(() -> round(x, r; digits=d), BigFloat, RoundUp) == round(x, r; digits=d)
    end
end
@testset "rounding to digits past floatmax" begin
    # the rounded value is beyond floatmax: Inf, not 0 (#64142)
    @test round(floatmax(); sigdigits=1) === Inf
    @test round(-floatmax(); sigdigits=2) === -Inf
    @test round(floatmax(); digits=-308) === Inf
    @test round(floatmax(), RoundFromZero; digits=-308) === Inf
    @test round(floatmax(Float32), RoundFromZero; digits=-38) === Inf32
    @test round(floatmax(Float16), RoundFromZero; sigdigits=1) === Inf16
    for s in 1:5
        @test round(floatmax(); sigdigits=s) === Inf
    end
    @test round(floatmax(); sigdigits=6) === 1.79769e308
    @test round(floatmax(), RoundDown; digits=-308) === 1.0e308
    @test round(-floatmax(), RoundUp; digits=-308) === -1.0e308
    # the step itself overflows: 0 or the infinite step
    for r in (RoundNearest, RoundToZero, RoundNearestTiesAway, RoundNearestTiesUp)
        @test round(1.25, r; digits=typemin(Int)) === 0.0
        @test round(-1.25, r; digits=-400) === -0.0
    end
    @test round(1.25, RoundFromZero; digits=typemin(Int)) === Inf
    @test round(-1.25, RoundFromZero; digits=-400) === -Inf
    @test round(-1.25, RoundUp; digits=-400) === -0.0
    @test round(-1.25, RoundDown; digits=-400) === -Inf
    for x in (Inf, -Inf, NaN, 0.0, -0.0), r in (RoundNearest, RoundUp, RoundDown, RoundFromZero)
        @test round(x, r; digits=-400) === x
        @test round(x, r; digits=-308) === x
    end
end
@testset "sigdigits of Rationals counts digits exactly" begin
    # float(x) rounds up to a power of the base
    @test round((10^17 - 1)//10^17, RoundDown; sigdigits=2) === 0.99
    @test round(-(10^17 - 1)//10^17, RoundUp; sigdigits=2) === -0.99
    @test round((big(10)^40 - 1)//big(10)^40, RoundDown; sigdigits=3) == big"0.999"
    @test round(1//1000; sigdigits=1) === 0.001
    @test round(1000//1; sigdigits=1) === 1000.0
    @test round((3^30 - 1)//3^30, RoundDown; sigdigits=1, base=3) === 2/3
    @test round(0//1; sigdigits=3) === 0.0
    rng = Random.MersenneTwister(0x5d)
    for _ in 1:300
        n, d = rand(rng, 1:10^15), rand(rng, 1:10^15)
        x = rand(rng, Bool) ? n//d : -n//d
        for base in (10, 3, 2)
            h = Base.hidigit(x, base)
            @test (big(base)//1)^(h - 1) <= abs(big(x)) < (big(base)//1)^h
        end
    end
    # within one unit in the 18th digit of a power of ten
    for k in -3:3, δ in (-1, 0, 1), s in (1, -1)
        x = s * (10^17 + δ)//10^(17 - k)
        h = Base.hidigit(x, 10)
        @test (big(10)//1)^(h - 1) <= abs(big(x)) < (big(10)//1)^h
    end
end
@testset "rounding with too much (or too few) precision" begin
    for x in (12345.6789, 0, -12345.6789)
        y = float(x)
        @test y == trunc(x, digits=1000)
        @test y == round(x, digits=1000)
        @test y == floor(x, digits=1000)
        @test y == ceil(x, digits=1000)
    end
    let x = 12345.6789
        @test 0.0 == trunc(x, digits=-1000)
        @test 0.0 == round(x, digits=-1000)
        @test 0.0 == floor(x, digits=-1000)
        @test Inf == ceil(x, digits=-1000)
    end
    let x = -12345.6789
        @test -0.0 == trunc(x, digits=-1000)
        @test -0.0 == round(x, digits=-1000)
        @test -Inf == floor(x, digits=-1000)
        @test -0.0 == ceil(x, digits=-1000)
    end
    let x = 0.0
        @test 0.0 == trunc(x, digits=-1000)
        @test 0.0 == round(x, digits=-1000)
        @test 0.0 == floor(x, digits=-1000)
        @test 0.0 == ceil(x, digits=-1000)
    end
end
@testset "rounding in other bases" begin
    @test round(pi, digits = 2, base = 2) ≈ 3.25
    @test round(pi, digits = 3, base = 2) ≈ 3.125
    @test round(pi, digits = 3, base = 5) ≈ 3.144
end
# a float type with only basic arithmetic: no `fma`, `eps`, `floatmin` or `maxintfloat`
struct NoFMAFloat <: AbstractFloat
    v::Float64
end
for f in (:+, :-, :*, :/)
    @eval Base.$f(a::NoFMAFloat, b::NoFMAFloat) = NoFMAFloat($f(a.v, b.v))
end
Base.:-(a::NoFMAFloat) = NoFMAFloat(-a.v)
for f in (:<, :<=, :(==))
    @eval Base.$f(a::NoFMAFloat, b::NoFMAFloat) = $f(a.v, b.v)
end
Base.round(a::NoFMAFloat, r::RoundingMode) = NoFMAFloat(round(a.v, r))
Base.isfinite(a::NoFMAFloat) = isfinite(a.v)
Base.signbit(a::NoFMAFloat) = signbit(a.v)
Base.abs(a::NoFMAFloat) = NoFMAFloat(abs(a.v))
Base.log10(a::NoFMAFloat) = NoFMAFloat(log10(a.v))
Base.floor(::Type{Int}, a::NoFMAFloat) = floor(Int, a.v)
Base.zero(::Type{NoFMAFloat}) = NoFMAFloat(0.0)
Base.one(::Type{NoFMAFloat}) = NoFMAFloat(1.0)
NoFMAFloat(x::NoFMAFloat) = x
Base.Float64(a::NoFMAFloat) = a.v
Base.convert(::Type{NoFMAFloat}, x::Real) = NoFMAFloat(Float64(x))
Base.promote_rule(::Type{NoFMAFloat}, ::Type{<:Integer}) = NoFMAFloat
@testset "rounding to digits for float types without fma" begin
    # these round the float product x * base^digits, as before
    x = NoFMAFloat(1.15)
    @test round(x; digits=1) === NoFMAFloat(1.2)
    @test floor(x; digits=1) === NoFMAFloat(1.1)
    @test ceil(x; digits=1) === NoFMAFloat(1.2)
    @test trunc(-x; digits=1) === NoFMAFloat(-1.1)
    @test round(x; sigdigits=2) === NoFMAFloat(1.2)
    @test round(NoFMAFloat(12345.6789); digits=-2) === NoFMAFloat(12300.0)
    @test ceil(x; digits=-1000) === NoFMAFloat(Inf)
    @test floor(-x; digits=-1000) === NoFMAFloat(-Inf)
end
@testset "vectorized trunc/round/floor/ceil with digits/base argument" begin
    a = rand(2, 2, 2)
    for f in (round, trunc, floor, ceil)
        @test f.(a[:, 1, 1],  digits=2) == map(x->f(x, digits=2), a[:, 1, 1])
        @test f.(a[:, :, 1],  digits=2) == map(x->f(x, digits=2), a[:, :, 1])
        @test f.(a,  digits=9, base = 2) == map(x->f(x, digits=9, base = 2), a)
        @test f.(a[:, 1, 1], digits=9, base = 2) == map(x->f(x, digits=9, base = 2), a[:, 1, 1])
        @test f.(a[:, :, 1], digits=9, base = 2) == map(x->f(x, digits=9, base = 2), a[:, :, 1])
        @test f.(a, digits=9, base = 2) == map(x->f(x, digits=9, base = 2), a)
    end
end

@testset "rounding for F32/F64" begin
    for T in [Float32, Float64]
        old = rounding(T)
        Base.Rounding.setrounding_raw(T, Base.Rounding.JL_FE_TOWARDZERO)
        @test rounding(T) == RoundToZero
        @test round(T(2.7)) == T(2.0)
        Base.Rounding.setrounding_raw(T, Base.Rounding.to_fenv(old))
    end
end

@testset "rounding floats with specified return type #50778" begin
    @test round(Float64, 1.2) === 1.0
    @test round(Float32, 1e60) === Inf32
    x = floatmax(Float32)-1.0
    @test round(Float32, x) == x
end

@testset "rounding complex numbers (#42060, #47128)" begin
    # 42060
    @test ceil(Complex(4.6, 2.2)) === Complex(5.0, 3.0)
    @test floor(Complex(4.6, 2.2)) === Complex(4.0, 2.0)
    @test trunc(Complex(4.6, 2.2)) === Complex(4.0, 2.0)
    @test round(Complex(4.6, 2.2)) === Complex(5.0, 2.0)
    @test ceil(Complex(-4.6, -2.2)) === Complex(-4.0, -2.0)
    @test floor(Complex(-4.6, -2.2)) === Complex(-5.0, -3.0)
    @test trunc(Complex(-4.6, -2.2)) === Complex(-4.0, -2.0)
    @test round(Complex(-4.6, -2.2)) === Complex(-5.0, -2.0)

    # 47128
    @test round(Complex{Int}, Complex(4.6, 2.2)) === Complex(5, 2)
    @test ceil(Complex{Int}, Complex(4.6, 2.2)) === Complex(5, 3)
end

@testset "rounding to custom integers" begin
    struct Int50812 <: Integer
        x::Int
    end
    @test round(Int50812, 1.2) === Int50812(1)
    @test round(Int50812, π) === Int50812(3)
    @test ceil(Int50812, π) === Int50812(4)
end

const MPFRRM = Base.MPFR.MPFRRoundingMode

function mpfr_to_ieee(::Type{Float32}, x::BigFloat, r::MPFRRM)
    ccall((:mpfr_get_flt, Base.MPFR.libmpfr), Float32, (Ref{BigFloat}, MPFRRM), x, r)
end
function mpfr_to_ieee(::Type{Float64}, x::BigFloat, r::MPFRRM)
    ccall((:mpfr_get_d, Base.MPFR.libmpfr), Float64, (Ref{BigFloat}, MPFRRM), x, r)
end

function mpfr_to_ieee(::Type{G}, x::BigFloat, r::RoundingMode) where {G}
    mpfr_to_ieee(G, x, convert(MPFRRM, r))
end

const mpfr_rounding_modes = map(
    Base.Fix1(convert, MPFRRM),
    (RoundNearest, RoundToZero, RoundFromZero, RoundDown, RoundUp)
)

sample_float(::Type{T}, e::Integer) where {T<:AbstractFloat} = ldexp(rand(T) + true, e)::T

function float_samples(::Type{T}, exponents, n::Int) where {T<:AbstractFloat}
    ret = T[]
    for e ∈ exponents, i ∈ 1:n
        push!(ret, sample_float(T, e), -sample_float(T, e))
    end
    ret
end

# a reasonable range of values for testing behavior between 1:200
const fib200 = [1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 200]

@testset "IEEEFloat(::BigFloat) against MPFR" begin
    for pr ∈ fib200
        setprecision(BigFloat, pr) do
            exp = exponent(floatmax(Float64)) + 10
            bf_samples = float_samples(BigFloat, (-exp):exp, 20) # about 82680 random values
            for mpfr_rm ∈ mpfr_rounding_modes, bf ∈ bf_samples, F ∈ (Float32, Float64)
                @test (
                    mpfr_to_ieee(F, bf, mpfr_rm) ===
                    F(bf, mpfr_rm) === F(bf, convert(RoundingMode, mpfr_rm))
                )
            end
        end
    end
end

const native_rounding_modes = (
    RoundNearest, RoundNearestTiesAway, RoundNearestTiesUp,
    RoundToZero, RoundFromZero, RoundUp, RoundDown
)

# Checks that each rounding mode is faithful.
@testset "IEEEFloat(::BigFloat) faithful rounding" begin
    for pr ∈ fib200
        setprecision(BigFloat, pr) do
            exp = 500
            bf_samples = float_samples(BigFloat, (-exp):exp, 20) # about 40040 random values
            for rm ∈ (mpfr_rounding_modes..., Base.MPFR.MPFRRoundFaithful,
                      native_rounding_modes...),
                bf ∈ bf_samples,
                F ∈ (Float16, Float32, Float64)
                f = F(bf, rm)
                @test (f === F(bf, RoundDown)) | (f === F(bf, RoundUp))
            end
        end
    end
end

@testset "round(Int, -Inf16) should throw (#51113)" begin
    @test_throws InexactError round(Int32, -Inf16)
    @test_throws InexactError round(Int64, -Inf16)
    @test_throws InexactError round(Int128, -Inf16)
    # More comprehensive testing is present in test/floatfuncs.jl
end

@testset "rounding to Union{} throws MethodError" begin
    # `Type{Union{}}` dispatches to its own method, which throws, instead of matching `Type{T}` methods
    for f in (trunc, floor, ceil, round, unsafe_trunc), x in (1.5, 1, big(1.5))
        @test_throws MethodError(f, (Union{}, x)) f(Union{}, x)
        @test which(f, (Type{Union{}}, typeof(x))).sig == Tuple{typeof(f), Type{Union{}}, Vararg}
    end
    for f in (trunc, floor, ceil, round)
        @test_throws MethodError(f, (Union{}, 1.5)) f(Union{}, 1.5; digits = 1)
    end
    @test_throws MethodError(round, (Union{}, 1.5, RoundDown)) round(Union{}, 1.5, RoundDown)
end

@testset "rounding mode of Union{} throws MethodError" begin
    # `Type{Union{}}` dispatches to its own method, which throws, instead of matching `Type{T}` methods
    f = () -> 1
    @test_throws MethodError(rounding, (Union{},)) rounding(Union{})
    @test which(rounding, (Type{Union{}},)).sig == Tuple{typeof(rounding), Type{Union{}}, Vararg}
    @test_throws MethodError(setrounding, (f, Union{}, RoundDown)) setrounding(f, Union{}, RoundDown)
    @test which(setrounding, (typeof(f), Type{Union{}}, typeof(RoundDown))).sig ==
          Tuple{typeof(setrounding), Any, Type{Union{}}, Vararg}
end

@testset "floor(<:AbstractFloat, large_number) (#52355)" begin
    @test floor(Float32, 0xffff_ffff) == prevfloat(2f0^32) <= 0xffff_ffff
    @test trunc(Float16, typemax(UInt128)) == floatmax(Float16)
    @test round(Float16, typemax(UInt128)) == Inf16
    for i in [-BigInt(floatmax(Float64)), -BigInt(floatmax(Float64))*100, BigInt(floatmax(Float64)), BigInt(floatmax(Float64))*100]
        f = ceil(Float64, i)
        @test f >= i
        @test isinteger(f) || isinf(f)
        @test prevfloat(f) < i
    end
end

@testset "π to `BigFloat` with `setrounding`" begin
    function irrational_to_big_float(c::AbstractIrrational)
        BigFloat(c)
    end

    function irrational_to_big_float_with_rounding_mode(c::AbstractIrrational, rm::RoundingMode)
        f = () -> irrational_to_big_float(c)
        setrounding(f, BigFloat, rm)
    end

    function irrational_to_big_float_with_rounding_mode_and_precision(c::AbstractIrrational, rm::RoundingMode, prec::Int)
        f = () -> irrational_to_big_float_with_rounding_mode(c, rm)
        setprecision(f, BigFloat, prec)
    end

    for c ∈ (π, MathConstants.γ, MathConstants.catalan)
        for p ∈ 1:40
            @test (
                irrational_to_big_float_with_rounding_mode_and_precision(c, RoundDown, p) < c <
                irrational_to_big_float_with_rounding_mode_and_precision(c, RoundUp, p)
            )
        end
    end
end

@testset "Rounding to floating point types with RoundFromZero #55820" begin
    @testset "Testing float types: $f" for f ∈ (Float16, Float32, Float64, BigFloat)
        @testset "Testing value types: $t" for t ∈ (Bool, Rational{Int8})
            @test iszero(f(zero(t), RoundFromZero))
        end
    end
    @test Float16(100000, RoundToZero) === floatmax(Float16)
    @test Float16(100000, RoundFromZero) === Inf16
    @test Float16(-100000, RoundToZero) === -floatmax(Float16)
    @test Float16(-100000, RoundFromZero) === -Inf16
    @test Float32(nextfloat(0.0), RoundFromZero) === nextfloat(0.0f0)
end
