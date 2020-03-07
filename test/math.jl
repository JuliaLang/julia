# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random
using LinearAlgebra

function isnan_type(::Type{T}, x) where T
    isa(x, T) && isnan(x)
end

@testset "clamp" begin
    @test clamp(0, 1, 3) == 1
    @test clamp(1, 1, 3) == 1
    @test clamp(2, 1, 3) == 2
    @test clamp(3, 1, 3) == 3
    @test clamp(4, 1, 3) == 3

    @test clamp(0.0, 1, 3) == 1.0
    @test clamp(1.0, 1, 3) == 1.0
    @test clamp(2.0, 1, 3) == 2.0
    @test clamp(3.0, 1, 3) == 3.0
    @test clamp(4.0, 1, 3) == 3.0

    @test clamp.([0, 1, 2, 3, 4], 1.0, 3.0) == [1.0, 1.0, 2.0, 3.0, 3.0]
    @test clamp.([0 1; 2 3], 1.0, 3.0) == [1.0 1.0; 2.0 3.0]

    @test clamp(-200, Int8) === typemin(Int8)
    @test clamp(100, Int8) === Int8(100)
    @test clamp(200, Int8) === typemax(Int8)

    begin
        x = [0.0, 1.0, 2.0, 3.0, 4.0]
        clamp!(x, 1, 3)
        @test x == [1.0, 1.0, 2.0, 3.0, 3.0]
    end
end

@testset "constants" begin
    @test pi != ℯ
    @test ℯ != 1//2
    @test 1//2 <= ℯ
    @test ℯ <= 15//3
    @test big(1//2) < ℯ
    @test ℯ < big(20//6)
    @test ℯ^pi == exp(pi)
    @test ℯ^2 == exp(2)
    @test ℯ^2.4 == exp(2.4)
    @test ℯ^(2//3) == exp(2//3)

    @test Float16(3.0) < pi
    @test pi < Float16(4.0)
    @test widen(pi) === pi

    @test occursin("3.14159", sprint(show, MIME"text/plain"(), π))
    @test repr(Any[pi ℯ; ℯ pi]) == "Any[π ℯ; ℯ π]"
    @test string(pi) == "π"
end

@testset "frexp,ldexp,significand,exponent" begin
    @testset "$T" for T in (Float16,Float32,Float64)
        for z in (zero(T),-zero(T))
            frexp(z) === (z,0)
            significand(z) === z
            @test_throws DomainError exponent(z)
        end

        for (a,b) in [(T(12.8),T(0.8)),
                      (prevfloat(floatmin(T)), prevfloat(one(T), 2)),
                      (prevfloat(floatmin(T)), prevfloat(one(T), 2)),
                      (prevfloat(floatmin(T)), nextfloat(one(T), -2)),
                      (nextfloat(zero(T), 3), T(0.75)),
                      (prevfloat(zero(T), -3), T(0.75)),
                      (nextfloat(zero(T)), T(0.5))]

            n = Int(log2(a/b))
            @test frexp(a) == (b,n)
            @test ldexp(b,n) == a
            @test ldexp(a,-n) == b
            @test significand(a) == 2b
            @test exponent(a) == n-1

            @test frexp(-a) == (-b,n)
            @test ldexp(-b,n) == -a
            @test ldexp(-a,-n) == -b
            @test significand(-a) == -2b
            @test exponent(-a) == n-1
        end
        @test_throws DomainError exponent(convert(T,NaN))
        @test isnan_type(T, significand(convert(T,NaN)))
        x,y = frexp(convert(T,NaN))
        @test isnan_type(T, x)
        @test y == 0

        @testset "ldexp function" begin
            @test ldexp(T(0.0), 0) === T(0.0)
            @test ldexp(T(-0.0), 0) === T(-0.0)
            @test ldexp(T(Inf), 1) === T(Inf)
            @test ldexp(T(Inf), 10000) === T(Inf)
            @test ldexp(T(-Inf), 1) === T(-Inf)
            @test isnan_type(T, ldexp(T(NaN), 10))
            @test ldexp(T(1.0), 0) === T(1.0)
            @test ldexp(T(0.8), 4) === T(12.8)
            @test ldexp(T(-0.854375), 5) === T(-27.34)
            @test ldexp(T(1.0), typemax(Int)) === T(Inf)
            @test ldexp(T(1.0), typemin(Int)) === T(0.0)
            @test ldexp(prevfloat(floatmin(T)), typemax(Int)) === T(Inf)
            @test ldexp(prevfloat(floatmin(T)), typemin(Int)) === T(0.0)

            @test ldexp(T(0.0), Int128(0)) === T(0.0)
            @test ldexp(T(-0.0), Int128(0)) === T(-0.0)
            @test ldexp(T(1.0), Int128(0)) === T(1.0)
            @test ldexp(T(0.8), Int128(4)) === T(12.8)
            @test ldexp(T(-0.854375), Int128(5)) === T(-27.34)
            @test ldexp(T(1.0), typemax(Int128)) === T(Inf)
            @test ldexp(T(1.0), typemin(Int128)) === T(0.0)
            @test ldexp(prevfloat(floatmin(T)), typemax(Int128)) === T(Inf)
            @test ldexp(prevfloat(floatmin(T)), typemin(Int128)) === T(0.0)

            @test ldexp(T(0.0), BigInt(0)) === T(0.0)
            @test ldexp(T(-0.0), BigInt(0)) === T(-0.0)
            @test ldexp(T(1.0), BigInt(0)) === T(1.0)
            @test ldexp(T(0.8), BigInt(4)) === T(12.8)
            @test ldexp(T(-0.854375), BigInt(5)) === T(-27.34)
            @test ldexp(T(1.0), BigInt(typemax(Int128))) === T(Inf)
            @test ldexp(T(1.0), BigInt(typemin(Int128))) === T(0.0)
            @test ldexp(prevfloat(floatmin(T)), BigInt(typemax(Int128))) === T(Inf)
            @test ldexp(prevfloat(floatmin(T)), BigInt(typemin(Int128))) === T(0.0)

            # Test also against BigFloat reference. Needs to be exactly rounded.
            @test ldexp(floatmin(T), -1) == T(ldexp(big(floatmin(T)), -1))
            @test ldexp(floatmin(T), -2) == T(ldexp(big(floatmin(T)), -2))
            @test ldexp(floatmin(T)/2, 0) == T(ldexp(big(floatmin(T)/2), 0))
            @test ldexp(floatmin(T)/3, 0) == T(ldexp(big(floatmin(T)/3), 0))
            @test ldexp(floatmin(T)/3, -1) == T(ldexp(big(floatmin(T)/3), -1))
            @test ldexp(floatmin(T)/3, 11) == T(ldexp(big(floatmin(T)/3), 11))
            @test ldexp(floatmin(T)/11, -10) == T(ldexp(big(floatmin(T)/11), -10))
            @test ldexp(-floatmin(T)/11, -10) == T(ldexp(big(-floatmin(T)/11), -10))
        end
    end
end

# We compare to BigFloat instead of hard-coding
# values, assuming that BigFloat has an independently tested implementation.
@testset "basic math functions" begin
    @testset "$T" for T in (Float32, Float64)
        x = T(1//3)
        y = T(1//2)
        yi = 4
        @testset "Random values" begin
            @test x^y ≈ big(x)^big(y)
            @test x^1 === x
            @test x^yi ≈ big(x)^yi
            @test acos(x) ≈ acos(big(x))
            @test acosh(1+x) ≈ acosh(big(1+x))
            @test asin(x) ≈ asin(big(x))
            @test asinh(x) ≈ asinh(big(x))
            @test atan(x) ≈ atan(big(x))
            @test atan(x,y) ≈ atan(big(x),big(y))
            @test atanh(x) ≈ atanh(big(x))
            @test cbrt(x) ≈ cbrt(big(x))
            @test cos(x) ≈ cos(big(x))
            @test cosh(x) ≈ cosh(big(x))
            @test exp(x) ≈ exp(big(x))
            @test exp10(x) ≈ exp10(big(x))
            @test exp2(x) ≈ exp2(big(x))
            @test expm1(x) ≈ expm1(big(x))
            @test hypot(x,y) ≈ hypot(big(x),big(y))
            @test hypot(x,x,y) ≈ hypot(hypot(big(x),big(x)),big(y))
            @test hypot(x,x,y,y) ≈ hypot(hypot(big(x),big(x)),hypot(big(y),big(y)))
            @test log(x) ≈ log(big(x))
            @test log10(x) ≈ log10(big(x))
            @test log1p(x) ≈ log1p(big(x))
            @test log2(x) ≈ log2(big(x))
            @test sin(x) ≈ sin(big(x))
            @test sinh(x) ≈ sinh(big(x))
            @test sqrt(x) ≈ sqrt(big(x))
            @test tan(x) ≈ tan(big(x))
            @test tanh(x) ≈ tanh(big(x))
            @test sec(x) ≈ sec(big(x))
            @test csc(x) ≈ csc(big(x))
            @test secd(x) ≈ secd(big(x))
            @test cscd(x) ≈ cscd(big(x))
            @test sech(x) ≈ sech(big(x))
            @test csch(x) ≈ csch(big(x))
        end
        @testset "Special values" begin
            @test isequal(T(1//4)^T(1//2), T(1//2))
            @test isequal(T(1//4)^2, T(1//16))
            @test isequal(acos(T(1)), T(0))
            @test isequal(acosh(T(1)), T(0))
            @test asin(T(1)) ≈ T(pi)/2 atol=eps(T)
            @test atan(T(1)) ≈ T(pi)/4 atol=eps(T)
            @test atan(T(1),T(1)) ≈ T(pi)/4 atol=eps(T)
            @test isequal(cbrt(T(0)), T(0))
            @test isequal(cbrt(T(1)), T(1))
            @test isequal(cbrt(T(1000000000)), T(1000))
            @test isequal(cos(T(0)), T(1))
            @test cos(T(pi)/2) ≈ T(0) atol=eps(T)
            @test isequal(cos(T(pi)), T(-1))
            @test exp(T(1)) ≈ T(ℯ) atol=10*eps(T)
            @test isequal(exp10(T(1)), T(10))
            @test isequal(exp2(T(1)), T(2))
            @test isequal(expm1(T(0)), T(0))
            @test expm1(T(1)) ≈ T(ℯ)-1 atol=10*eps(T)
            @test isequal(hypot(T(3),T(4)), T(5))
            @test isequal(hypot(floatmax(T),T(1)),floatmax(T))
            @test isequal(hypot(floatmin(T)*sqrt(eps(T)),T(0)),floatmin(T)*sqrt(eps(T)))
            @test isequal(floatmin(T)*hypot(1.368423059742933,1.3510496552495361),hypot(floatmin(T)*1.368423059742933,floatmin(T)*1.3510496552495361))
            @test isequal(log(T(1)), T(0))
            @test isequal(log(ℯ,T(1)), T(0))
            @test log(T(ℯ)) ≈ T(1) atol=eps(T)
            @test isequal(log10(T(1)), T(0))
            @test isequal(log10(T(10)), T(1))
            @test isequal(log1p(T(0)), T(0))
            @test log1p(T(ℯ)-1) ≈ T(1) atol=eps(T)
            @test isequal(log2(T(1)), T(0))
            @test isequal(log2(T(2)), T(1))
            @test isequal(sin(T(0)), T(0))
            @test isequal(sin(T(pi)/2), T(1))
            @test sin(T(pi)) ≈ T(0) atol=eps(T)
            @test isequal(sqrt(T(0)), T(0))
            @test isequal(sqrt(T(1)), T(1))
            @test isequal(sqrt(T(100000000)), T(10000))
            @test isequal(tan(T(0)), T(0))
            @test tan(T(pi)/4) ≈ T(1) atol=eps(T)
            @test isequal(sec(T(pi)), -one(T))
            @test isequal(csc(T(pi)/2), one(T))
            @test isequal(secd(T(180)), -one(T))
            @test isequal(cscd(T(90)), one(T))
            @test isequal(sech(log(one(T))), one(T))
            @test isequal(csch(zero(T)), T(Inf))
        end
        @testset "Inverses" begin
            @test acos(cos(x)) ≈ x
            @test acosh(cosh(x)) ≈ x
            @test asin(sin(x)) ≈ x
            @test cbrt(x)^3 ≈ x
            @test cbrt(x^3) ≈ x
            @test asinh(sinh(x)) ≈ x
            @test atan(tan(x)) ≈ x
            @test atan(x,y) ≈ atan(x/y)
            @test atanh(tanh(x)) ≈ x
            @test cos(acos(x)) ≈ x
            @test cosh(acosh(1+x)) ≈ 1+x
            @test exp(log(x)) ≈ x
            @test exp10(log10(x)) ≈ x
            @test exp2(log2(x)) ≈ x
            @test expm1(log1p(x)) ≈ x
            @test log(exp(x)) ≈ x
            @test log10(exp10(x)) ≈ x
            @test log1p(expm1(x)) ≈ x
            @test log2(exp2(x)) ≈ x
            @test sin(asin(x)) ≈ x
            @test sinh(asinh(x)) ≈ x
            @test sqrt(x)^2 ≈ x
            @test sqrt(x^2) ≈ x
            @test tan(atan(x)) ≈ x
            @test tanh(atanh(x)) ≈ x
        end
        @testset "Relations between functions" begin
            @test cosh(x) ≈ (exp(x)+exp(-x))/2
            @test cosh(x)^2-sinh(x)^2 ≈ 1
            @test hypot(x,y) ≈ sqrt(x^2+y^2)
            @test sin(x)^2+cos(x)^2 ≈ 1
            @test sinh(x) ≈ (exp(x)-exp(-x))/2
            @test tan(x) ≈ sin(x)/cos(x)
            @test tanh(x) ≈ sinh(x)/cosh(x)
            @test sec(x) ≈ inv(cos(x))
            @test csc(x) ≈ inv(sin(x))
            @test secd(x) ≈ inv(cosd(x))
            @test cscd(x) ≈ inv(sind(x))
            @test sech(x) ≈ inv(cosh(x))
            @test csch(x) ≈ inv(sinh(x))
        end
        @testset "Edge cases" begin
            @test isinf(log(zero(T)))
            @test isnan_type(T, log(convert(T,NaN)))
            @test_throws DomainError log(-one(T))
            @test isinf(log1p(-one(T)))
            @test isnan_type(T, log1p(convert(T,NaN)))
            @test_throws DomainError log1p(convert(T,-2.0))
            @test hypot(T(0), T(0)) === T(0)
            @test hypot(T(Inf), T(Inf)) === T(Inf)
            @test hypot(T(Inf), T(x)) === T(Inf)
            @test hypot(T(Inf), T(NaN)) === T(Inf)
            @test isnan_type(T, hypot(T(x), T(NaN)))
        end
    end
end

@testset "exp function" for T in (Float64, Float32)
    @testset "$T accuracy" begin
        X = map(T, vcat(-10:0.0002:10, -80:0.001:80, 2.0^-27, 2.0^-28, 2.0^-14, 2.0^-13))
        for x in X
            y, yb = exp(x), exp(big(x))
            @test abs(y-yb) <= 1.0*eps(T(yb))
        end
    end
    @testset "$T edge cases" begin
        @test isnan_type(T, exp(T(NaN)))
        @test exp(T(-Inf)) === T(0.0)
        @test exp(T(Inf)) === T(Inf)
        @test exp(T(0.0)) === T(1.0) # exact
        @test exp(T(5000.0)) === T(Inf)
        @test exp(T(-5000.0)) === T(0.0)
    end
end

@testset "exp10 function" begin
    @testset "accuracy" begin
        X = map(Float64, vcat(-10:0.00021:10, -35:0.0023:100, -300:0.001:300))
        for x in X
            y, yb = exp10(x), exp10(big(x))
            @test abs(y-yb) <= 1.2*eps(Float64(yb))
        end
        X = map(Float32, vcat(-10:0.00021:10, -35:0.0023:35, -35:0.001:35))
        for x in X
            y, yb = exp10(x), exp10(big(x))
            @test abs(y-yb) <= 1.2*eps(Float32(yb))
        end
    end
    @testset "$T edge cases" for T in (Float64, Float32)
        @test isnan_type(T, exp10(T(NaN)))
        @test exp10(T(-Inf)) === T(0.0)
        @test exp10(T(Inf)) === T(Inf)
        @test exp10(T(0.0)) === T(1.0) # exact
        @test exp10(T(1.0)) === T(10.0)
        @test exp10(T(3.0)) === T(1000.0)
        @test exp10(T(5000.0)) === T(Inf)
        @test exp10(T(-5000.0)) === T(0.0)
    end
end

@testset "test abstractarray trig functions" begin
    TAA = rand(2,2)
    TAA = (TAA + TAA')/2.
    STAA = Symmetric(TAA)
    @test Array(atanh.(STAA)) == atanh.(TAA)
    @test Array(asinh.(STAA)) == asinh.(TAA)
    TAA .+= 1
    @test Array(acosh.(STAA)) == acosh.(TAA)
    @test Array(acsch.(STAA)) == acsch.(TAA)
    @test Array(acoth.(STAA)) == acoth.(TAA)
end

@testset "check exp2(::Integer) matches exp2(::Float)" begin
    for ii in -2048:2048
        expected = exp2(float(ii))
        @test exp2(Int16(ii)) == expected
        @test exp2(Int32(ii)) == expected
        @test exp2(Int64(ii)) == expected
        @test exp2(Int128(ii)) == expected
        if ii >= 0
            @test exp2(UInt16(ii)) == expected
            @test exp2(UInt32(ii)) == expected
            @test exp2(UInt64(ii)) == expected
            @test exp2(UInt128(ii)) == expected
        end
    end
end

@testset "deg2rad/rad2deg" begin
    @testset "$T" for T in (Int, Float64, BigFloat)
        @test deg2rad(T(180)) ≈ 1pi
        @test deg2rad.(T[45, 60]) ≈ [pi/T(4), pi/T(3)]
        @test rad2deg.([pi/T(4), pi/T(3)]) ≈ [45, 60]
        @test rad2deg(T(1)*pi) ≈ 180
        @test rad2deg(T(1)) ≈ rad2deg(true)
        @test deg2rad(T(1)) ≈ deg2rad(true)
    end
    @test deg2rad(180 + 60im) ≈ pi + (pi/3)*im
    @test rad2deg(pi + (pi/3)*im) ≈ 180 + 60im
end

@testset "degree-based trig functions" begin
    @testset "$T" for T = (Float32,Float64,Rational{Int})
        fT = typeof(float(one(T)))
        fTsc = typeof( (float(one(T)), float(one(T))) )
        for x = -400:40:400
            @test sind(convert(T,x))::fT ≈ convert(fT,sin(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))
            @test cosd(convert(T,x))::fT ≈ convert(fT,cos(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))

            s,c = sincosd(convert(T,x))
            @test s::fT ≈ convert(fT,sin(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))
            @test c::fT ≈ convert(fT,cos(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))
        end
        @testset "sind" begin
            @test sind(convert(T,0.0))::fT === zero(fT)
            @test sind(convert(T,180.0))::fT === zero(fT)
            @test sind(convert(T,360.0))::fT === zero(fT)
            T != Rational{Int} && @test sind(convert(T,-0.0))::fT === -zero(fT)
            @test sind(convert(T,-180.0))::fT === -zero(fT)
            @test sind(convert(T,-360.0))::fT === -zero(fT)
        end
        @testset "cosd" begin
            @test cosd(convert(T,90))::fT === zero(fT)
            @test cosd(convert(T,270))::fT === zero(fT)
            @test cosd(convert(T,-90))::fT === zero(fT)
            @test cosd(convert(T,-270))::fT === zero(fT)
        end
        @testset "sincosd" begin
            @test sincosd(convert(T,-360))::fTsc === ( -zero(fT),  one(fT) )
            @test sincosd(convert(T,-270))::fTsc === (   one(fT), zero(fT) )
            @test sincosd(convert(T,-180))::fTsc === ( -zero(fT), -one(fT) )
            @test sincosd(convert(T, -90))::fTsc === (  -one(fT), zero(fT) )
            @test sincosd(convert(T,   0))::fTsc === (  zero(fT),  one(fT) )
            @test sincosd(convert(T,  90))::fTsc === (   one(fT), zero(fT) )
            @test sincosd(convert(T, 180))::fTsc === (  zero(fT), -one(fT) )
            @test sincosd(convert(T, 270))::fTsc === (  -one(fT), zero(fT) )
        end

        @testset "sinpi and cospi" begin
            for x = -3:0.3:3
                @test sinpi(convert(T,x))::fT ≈ convert(fT,sin(pi*x)) atol=eps(pi*convert(fT,x))
                @test cospi(convert(T,x))::fT ≈ convert(fT,cos(pi*x)) atol=eps(pi*convert(fT,x))
            end

            @test sinpi(convert(T,0.0))::fT === zero(fT)
            @test sinpi(convert(T,1.0))::fT === zero(fT)
            @test sinpi(convert(T,2.0))::fT === zero(fT)
            T != Rational{Int} && @test sinpi(convert(T,-0.0))::fT === -zero(fT)
            @test sinpi(convert(T,-1.0))::fT === -zero(fT)
            @test sinpi(convert(T,-2.0))::fT === -zero(fT)
            @test_throws DomainError sinpi(convert(T,Inf))

            @test cospi(convert(T,0.5))::fT === zero(fT)
            @test cospi(convert(T,1.5))::fT === zero(fT)
            @test cospi(convert(T,-0.5))::fT === zero(fT)
            @test cospi(convert(T,-1.5))::fT === zero(fT)
            @test_throws DomainError cospi(convert(T,Inf))
        end
        @testset "Check exact values" begin
            @test sind(convert(T,30)) == 0.5
            @test cosd(convert(T,60)) == 0.5
            @test sind(convert(T,150)) == 0.5
            @test sinpi(one(T)/convert(T,6)) == 0.5
            @test_throws DomainError sind(convert(T,Inf))
            @test_throws DomainError cosd(convert(T,Inf))
            T != Float32 && @test cospi(one(T)/convert(T,3)) == 0.5
            T == Rational{Int} && @test sinpi(5//6) == 0.5
        end
    end
    scdm = sincosd(missing)
    @test ismissing(scdm[1])
    @test ismissing(scdm[2])
end

@testset "Integer args to sinpi/cospi/sinc/cosc" begin
    @test sinpi(1) == 0
    @test sinpi(-1) == -0
    @test cospi(1) == -1
    @test cospi(2) == 1

    @test sinc(1) == 0
    @test sinc(complex(1,0)) == 0
    @test sinc(0) == 1
    @test sinc(Inf) == 0
    @test cosc(1) == -1
    @test cosc(0) == 0
    @test cosc(complex(1,0)) == -1
    @test cosc(Inf) == 0
end

@testset "Irrational args to sinpi/cospi/sinc/cosc" begin
    for x in (pi, ℯ, Base.MathConstants.golden)
        @test sinpi(x) ≈ Float64(sinpi(big(x)))
        @test cospi(x) ≈ Float64(cospi(big(x)))
        @test sinc(x)  ≈ Float64(sinc(big(x)))
        @test cosc(x)  ≈ Float64(cosc(big(x)))
        @test sinpi(complex(x, x)) ≈ Complex{Float64}(sinpi(complex(big(x), big(x))))
        @test cospi(complex(x, x)) ≈ Complex{Float64}(cospi(complex(big(x), big(x))))
        @test sinc(complex(x, x))  ≈ Complex{Float64}(sinc(complex(big(x),  big(x))))
        @test cosc(complex(x, x))  ≈ Complex{Float64}(cosc(complex(big(x),  big(x))))
    end
end

@testset "trig function type stability" begin
    @testset "$T $f" for T = (Float32,Float64,BigFloat), f = (sind,cosd,sinpi,cospi)
        @test Base.return_types(f,Tuple{T}) == [T]
    end
end

# useful test functions for relative error, which differ from isapprox (≈)
# in that relerrc separately looks at the real and imaginary parts
relerr(z, x) = z == x ? 0.0 : abs(z - x) / abs(x)
relerrc(z, x) = max(relerr(real(z),real(x)), relerr(imag(z),imag(x)))
≅(a,b) = relerrc(a,b) ≤ 1e-13

@testset "subnormal flags" begin
    # Ensure subnormal flags functions don't segfault
    @test any(set_zero_subnormals(true) .== [false,true])
    @test any(get_zero_subnormals() .== [false,true])
    @test set_zero_subnormals(false)
    @test !get_zero_subnormals()
end

@testset "evalpoly" begin
    @test @evalpoly(2,3,4,5,6) == 3+2*(4+2*(5+2*6)) == @evalpoly(2+0im,3,4,5,6)
    a0 = 1
    a1 = 2
    c = 3
    @test @evalpoly(c, a0, a1) == 7
    @test @evalpoly(1, 2) == 2
end

@testset "evalpoly real" begin
    for x in -1.0:2.0, p1 in -3.0:3.0, p2 in -3.0:3.0, p3 in -3.0:3.0
        evpm = @evalpoly(x, p1, p2, p3)
        @test evalpoly(x, (p1, p2, p3)) == evpm
        @test evalpoly(x, [p1, p2, p3]) == evpm
    end
end

@testset "evalpoly complex" begin
    for x in -1.0:2.0, y in -1.0:2.0, p1 in -3.0:3.0, p2 in -3.0:3.0, p3 in -3.0:3.0
        z = x + im * y
        evpm = @evalpoly(z, p1, p2, p3)
        @test evalpoly(z, (p1, p2, p3)) == evpm
        @test evalpoly(z, [p1, p2, p3]) == evpm
    end
    @test evalpoly(1+im, (2,)) == 2
    @test evalpoly(1+im, [2,]) == 2
end

@testset "cis" begin
    for z in (1.234, 1.234 + 5.678im)
        @test cis(z) ≈ exp(im*z)
    end
    let z = [1.234, 5.678]
        @test cis.(z) ≈ exp.(im*z)
    end
end

@testset "modf" begin
    @testset "$elty" for elty in (Float16, Float32, Float64)
        @test modf( convert(elty,1.2) )[1] ≈ convert(elty,0.2)
        @test modf( convert(elty,1.2) )[2] ≈ convert(elty,1.0)
        @test modf( convert(elty,1.0) )[1] ≈ convert(elty,0.0)
        @test modf( convert(elty,1.0) )[2] ≈ convert(elty,1.0)
    end
end

@testset "frexp" begin
    @testset "$elty" for elty in (Float16, Float32, Float64)
        @test frexp( convert(elty,0.5) ) == (0.5, 0)
        @test frexp( convert(elty,4.0) ) == (0.5, 3)
        @test frexp( convert(elty,10.5) ) == (0.65625, 4)
    end
end

@testset "log/log1p" begin
    # using Tang's algorithm, should be accurate to within 0.56 ulps
    X = rand(100)
    for x in X
        for n = -5:5
            xn = ldexp(x,n)

            for T in (Float32,Float64)
                xt = T(x)

                y = log(xt)
                yb = log(big(xt))
                @test abs(y-yb) <= 0.56*eps(T(yb))

                y = log1p(xt)
                yb = log1p(big(xt))
                @test abs(y-yb) <= 0.56*eps(T(yb))

                if n <= 0
                    y = log1p(-xt)
                    yb = log1p(big(-xt))
                    @test abs(y-yb) <= 0.56*eps(T(yb))
                end
            end
        end
    end

    for n = 0:28
        @test log(2,2^n) == n
    end
    setprecision(10_000) do
        @test log(2,big(2)^100) == 100
        @test log(2,big(2)^200) == 200
        @test log(2,big(2)^300) == 300
        @test log(2,big(2)^400) == 400
    end

    for T in (Float32,Float64)
        @test log(zero(T)) == -Inf
        @test isnan_type(T, log(T(NaN)))
        @test_throws DomainError log(-one(T))
        @test log1p(-one(T)) == -Inf
        @test isnan_type(T, log1p(T(NaN)))
        @test_throws DomainError log1p(-2*one(T))
    end
end

@testset "vectorization of 2-arg functions" begin
    binary_math_functions = [
        copysign, flipsign, log, atan, hypot, max, min,
    ]
    @testset "$f" for f in binary_math_functions
        x = y = 2
        v = [f(x,y)]
        @test f.([x],y) == v
        @test f.(x,[y]) == v
        @test f.([x],[y]) == v
    end
end

@testset "issues #3024, #12822, #24240" begin
    p2 = -2
    p3 = -3
    @test_throws DomainError 2 ^ p2
    @test 2 ^ -2 == 0.25 == (2^-1)^2
    @test_throws DomainError (-2)^(2.2)
    @test_throws DomainError (-2.0)^(2.2)
    @test_throws DomainError false ^ p2
    @test false ^ -2 == Inf
    @test 1 ^ -2 === (-1) ^ -2 == 1 ^ p2 === (-1) ^ p2 === 1
    @test (-1) ^ -1 === (-1) ^ -3 == (-1) ^ p3 === -1
    @test true ^ -2 == true ^ p2 === true
end

@testset "issue #13748" begin
    let A = [1 2; 3 4]; B = [5 6; 7 8]; C = [9 10; 11 12]
        @test muladd(A,B,C) == A*B + C
    end
end

@testset "issue #19872" begin
    f19872a(x) = x ^ 5
    f19872b(x) = x ^ (-1024)
    @test 0 < f19872b(2.0) < 1e-300
    @test issubnormal(2.0 ^ (-1024))
    @test issubnormal(f19872b(2.0))
    @test !issubnormal(f19872b(0.0))
    @test f19872a(2.0) === 32.0
    @test !issubnormal(f19872a(2.0))
    @test !issubnormal(0.0)
end

# no domain error is thrown for negative values
@test invoke(cbrt, Tuple{AbstractFloat}, -1.0) == -1.0

@testset "promote Float16 irrational #15359" begin
    @test typeof(Float16(.5) * pi) == Float16
end

@testset "sincos" begin
    @test sincos(1.0) === (sin(1.0), cos(1.0))
    @test sincos(1f0) === (sin(1f0), cos(1f0))
    @test sincos(Float16(1)) === (sin(Float16(1)), cos(Float16(1)))
    @test sincos(1) === (sin(1), cos(1))
    @test sincos(big(1)) == (sin(big(1)), cos(big(1)))
    @test sincos(big(1.0)) == (sin(big(1.0)), cos(big(1.0)))
    @test sincos(NaN) === (NaN, NaN)
    @test sincos(NaN32) === (NaN32, NaN32)
end

@testset "test fallback definitions" begin
    @test exp10(5) ≈ exp10(5.0)
    @test exp10(50//10) ≈ exp10(5.0)
    @test log10(exp10(ℯ)) ≈ ℯ
    @test log(ℯ) === 1
    @test exp2(Float16(2.0)) ≈ exp2(2.0)
    @test exp2(Float16(1.0)) === Float16(exp2(1.0))
    @test exp10(Float16(1.0)) === Float16(exp10(1.0))
end

# #22742: updated isapprox semantics
@test !isapprox(1.0, 1.0+1e-12, atol=1e-14)
@test isapprox(1.0, 1.0+0.5*sqrt(eps(1.0)))
@test !isapprox(1.0, 1.0+1.5*sqrt(eps(1.0)), atol=sqrt(eps(1.0)))

# test AbstractFloat fallback pr22716
struct Float22716{T<:AbstractFloat} <: AbstractFloat
    x::T
end
Base.:^(x::Number, y::Float22716) = x^(y.x)
let x = 2.0
    @test exp2(Float22716(x)) === 2^x
    @test exp10(Float22716(x)) === 10^x
end

@testset "asin #23088" begin
    for T in (Float32, Float64)
        @test asin(zero(T)) === zero(T)
        @test asin(-zero(T)) === -zero(T)
        @test asin(nextfloat(zero(T))) === nextfloat(zero(T))
        @test asin(prevfloat(zero(T))) === prevfloat(zero(T))
        @test asin(one(T)) === T(pi)/2
        @test asin(-one(T)) === -T(pi)/2
        for x in (0.45, 0.6, 0.98)
            by = asin(big(T(x)))
            @test T(abs(asin(T(x)) - by))/eps(T(abs(by))) <= 1
            bym = asin(big(T(-x)))
            @test T(abs(asin(T(-x)) - bym))/eps(T(abs(bym))) <= 1
        end
        @test_throws DomainError asin(-T(Inf))
        @test_throws DomainError asin(T(Inf))
        @test isnan_type(T, asin(T(NaN)))
    end
end

@testset "sin, cos, sincos, tan #23088" begin
    for T in (Float32, Float64)
        @test sin(zero(T)) === zero(T)
        @test sin(-zero(T)) === -zero(T)
        @test cos(zero(T)) === T(1.0)
        @test cos(-zero(T)) === T(1.0)
        @test sin(nextfloat(zero(T))) === nextfloat(zero(T))
        @test sin(prevfloat(zero(T))) === prevfloat(zero(T))
        @test cos(nextfloat(zero(T))) === T(1.0)
        @test cos(prevfloat(zero(T))) === T(1.0)
        for x in (0.1, 0.45, 0.6, 0.75, 0.79, 0.98)
            for op in (sin, cos, tan)
                by = T(op(big(x)))
                @test abs(op(T(x)) - by)/eps(by) <= one(T)
                bym = T(op(big(-x)))
                @test abs(op(T(-x)) - bym)/eps(bym) <= one(T)
            end
        end
        @test_throws DomainError sin(-T(Inf))
        @test_throws DomainError sin(T(Inf))
        @test_throws DomainError cos(-T(Inf))
        @test_throws DomainError cos(T(Inf))
        @test_throws DomainError tan(-T(Inf))
        @test_throws DomainError tan(T(Inf))
        @test sin(T(NaN)) === T(NaN)
        @test cos(T(NaN)) === T(NaN)
        @test tan(T(NaN)) === T(NaN)
    end
end

@testset "rem_pio2 #23088" begin
    vals = (2.356194490192345f0, 3.9269908169872414f0, 7.0685834705770345f0,
              5.497787143782138f0, 4.216574282663131f8, 4.216574282663131f12)
    for (i, x) in enumerate(vals)
        for op in (prevfloat, nextfloat)
            Ty = Float32(Base.Math.rem_pio2_kernel(op(vals[i]))[2].hi)
            By = Float32(rem(big(op(x)), pi/2))
            @test Ty ≈ By || Ty ≈ By-Float32(pi)/2
        end
    end
end

@testset "atan #23383" begin
    for T in (Float32, Float64)
        @test atan(T(NaN)) === T(NaN)
        @test atan(-T(Inf)) === -T(pi)/2
        @test atan(T(Inf)) === T(pi)/2
        # no reduction needed |x| < 7/16
        @test atan(zero(T)) === zero(T)
        @test atan(prevfloat(zero(T))) === prevfloat(zero(T))
        @test atan(nextfloat(zero(T))) === nextfloat(zero(T))
        for x in (T(7/16), (T(7/16)+T(11/16))/2, T(11/16),
                  (T(11/16)+T(19/16))/2, T(19/16),
                  (T(19/16)+T(39/16))/2, T(39/16),
                  (T(39/16)+T(2)^23)/2, T(2)^23)
            x = T(7/16)
            by = T(atan(big(x)))
            @test abs(atan(x) - by)/eps(by) <= one(T)
            x = prevfloat(T(7/16))
            by = T(atan(big(x)))
            @test abs(atan(x) - by)/eps(by) <= one(T)
            x = nextfloat(T(7/16))
            by = T(atan(big(x)))
            @test abs(atan(x) - by)/eps(by) <= one(T)
        end
        # This case was used to find a bug, but it isn't special in itself
        @test atan(1.7581305072934137) ≈ 1.053644580517088
    end
end
@testset "atan" begin
    for T in (Float32, Float64)
        @test isnan_type(T, atan(T(NaN), T(NaN)))
        @test isnan_type(T, atan(T(NaN), T(0.1)))
        @test isnan_type(T, atan(T(0.1), T(NaN)))
        r = T(randn())
        absr = abs(r)
        # y zero
        @test atan(T(r), one(T)) === atan(T(r))
        @test atan(zero(T), absr) === zero(T)
        @test atan(-zero(T), absr) === -zero(T)
        @test atan(zero(T), -absr) === T(pi)
        @test atan(-zero(T), -absr) === -T(pi)
        # x zero and y not zero
        @test atan(one(T), zero(T)) === T(pi)/2
        @test atan(-one(T), zero(T)) === -T(pi)/2
        # isinf(x) == true && isinf(y) == true
        @test atan(T(Inf), T(Inf)) === T(pi)/4 # m == 0 (see atan code)
        @test atan(-T(Inf), T(Inf)) === -T(pi)/4 # m == 1
        @test atan(T(Inf), -T(Inf)) === 3*T(pi)/4 # m == 2
        @test atan(-T(Inf), -T(Inf)) === -3*T(pi)/4 # m == 3
        # isinf(x) == true && isinf(y) == false
        @test atan(absr, T(Inf)) === zero(T) # m == 0
        @test atan(-absr, T(Inf)) === -zero(T) # m == 1
        @test atan(absr, -T(Inf)) === T(pi) # m == 2
        @test atan(-absr, -T(Inf)) === -T(pi) # m == 3
        # isinf(y) == true && isinf(x) == false
        @test atan(T(Inf), absr) === T(pi)/2
        @test atan(-T(Inf), absr) === -T(pi)/2
        @test atan(T(Inf), -absr) === T(pi)/2
        @test atan(-T(Inf), -absr) === -T(pi)/2
        # |y/x| above high threshold
        atanpi = T(1.5707963267948966)
        @test atan(T(2.0^61), T(1.0)) === atanpi # m==0
        @test atan(-T(2.0^61), T(1.0)) === -atanpi # m==1
        @test atan(T(2.0^61), -T(1.0)) === atanpi # m==2
        @test atan(-T(2.0^61), -T(1.0)) === -atanpi # m==3
        @test atan(-T(Inf), -absr) === -T(pi)/2
        # |y|/x between 0 and low threshold
        @test atan(T(2.0^-61), -T(1.0)) === T(pi) # m==2
        @test atan(-T(2.0^-61), -T(1.0)) === -T(pi) # m==3
        # y/x is "safe" ("arbitrary values", just need to hit the branch)
        _ATAN_PI_LO(::Type{Float32}) = -8.7422776573f-08
        _ATAN_PI_LO(::Type{Float64}) = 1.2246467991473531772E-16
        @test atan(T(5.0), T(2.5)) === atan(abs(T(5.0)/T(2.5)))
        @test atan(-T(5.0), T(2.5)) === -atan(abs(-T(5.0)/T(2.5)))
        @test atan(T(5.0), -T(2.5)) === T(pi)-(atan(abs(T(5.0)/-T(2.5)))-_ATAN_PI_LO(T))
        @test atan(-T(5.0), -T(2.5)) === -(T(pi)-atan(abs(-T(5.0)/-T(2.5)))-_ATAN_PI_LO(T))
        @test atan(T(1235.2341234), T(2.5)) === atan(abs(T(1235.2341234)/T(2.5)))
        @test atan(-T(1235.2341234), T(2.5)) === -atan(abs(-T(1235.2341234)/T(2.5)))
        @test atan(T(1235.2341234), -T(2.5)) === T(pi)-(atan(abs(T(1235.2341234)/-T(2.5)))-_ATAN_PI_LO(T))
        @test atan(-T(1235.2341234), -T(2.5)) === -(T(pi)-(atan(abs(-T(1235.2341234)/T(2.5)))-_ATAN_PI_LO(T)))
    end
end

@testset "atand" begin
    for T in (Float32, Float64)
        r = T(randn())
        absr = abs(r)

        # Tests related to the 1-argument version of `atan`.
        # ==================================================

        @test atand(T(Inf))  === T(90.0)
        @test atand(-T(Inf)) === -T(90.0)
        @test atand(zero(T)) === T(0.0)
        @test atand(one(T))  === T(45.0)
        @test atand(-one(T)) === -T(45.0)

        # Tests related to the 2-argument version of `atan`.
        # ==================================================

        # If `x` is one, then `atand(y,x)` must be equal to `atand(y)`.
        @test atand(T(r), one(T))    === atand(T(r))

        # `y` zero.
        @test atand(zero(T), absr)   === zero(T)
        @test atand(-zero(T), absr)  === -zero(T)
        @test atand(zero(T), -absr)  === T(180.0)
        @test atand(-zero(T), -absr) === -T(180.0)

        # `x` zero and `y` not zero.
        @test atand(one(T), zero(T))  === T(90.0)
        @test atand(-one(T), zero(T)) === -T(90.0)

        # `x` and `y` equal for each quadrant.
        @test atand(+absr, +absr) === T(45.0)
        @test atand(-absr, +absr) === -T(45.0)
        @test atand(+absr, -absr) === T(135.0)
        @test atand(-absr, -absr) === -T(135.0)
    end
end

@testset "acos #23283" begin
    for T in (Float32, Float64)
        @test acos(zero(T)) === T(pi)/2
        @test acos(-zero(T)) === T(pi)/2
        @test acos(nextfloat(zero(T))) === T(pi)/2
        @test acos(prevfloat(zero(T))) === T(pi)/2
        @test acos(one(T)) === T(0.0)
        @test acos(-one(T)) === T(pi)
        for x in (0.45, 0.6, 0.98)
            by = acos(big(T(x)))
            @test T((acos(T(x)) - by))/eps(abs(T(by))) <= 1
            bym = acos(big(T(-x)))
            @test T(abs(acos(T(-x)) - bym))/eps(abs(T(bym))) <= 1
        end
        @test_throws DomainError acos(-T(Inf))
        @test_throws DomainError acos(T(Inf))
        @test isnan_type(T, acos(T(NaN)))
    end
end

#prev, current, next float
pcnfloat(x) = prevfloat(x), x, nextfloat(x)
import Base.Math: COSH_SMALL_X, H_SMALL_X, H_MEDIUM_X, H_LARGE_X

@testset "sinh" begin
    for T in (Float32, Float64)
        @test sinh(zero(T)) === zero(T)
        @test sinh(-zero(T)) === -zero(T)
        @test sinh(nextfloat(zero(T))) === nextfloat(zero(T))
        @test sinh(prevfloat(zero(T))) === prevfloat(zero(T))
        @test sinh(T(1000)) === T(Inf)
        @test sinh(-T(1000)) === -T(Inf)
        @test isnan_type(T, sinh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([H_SMALL_X(T), H_MEDIUM_X(T), H_LARGE_X(T)]))
            @test sinh(x) ≈ sinh(big(x)) rtol=eps(T)
            @test sinh(-x) ≈ sinh(big(-x)) rtol=eps(T)
        end
    end
end

@testset "cosh" begin
    for T in (Float32, Float64)
        @test cosh(zero(T)) === one(T)
        @test cosh(-zero(T)) === one(T)
        @test cosh(nextfloat(zero(T))) === one(T)
        @test cosh(prevfloat(zero(T))) === one(T)
        @test cosh(T(1000)) === T(Inf)
        @test cosh(-T(1000)) === T(Inf)
        @test isnan_type(T, cosh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([COSH_SMALL_X(T), H_MEDIUM_X(T), H_LARGE_X(T)]))
            @test cosh(x) ≈ cosh(big(x)) rtol=eps(T)
            @test cosh(-x) ≈ cosh(big(-x)) rtol=eps(T)
        end
    end
end

@testset "tanh" begin
    for T in (Float32, Float64)
        @test tanh(zero(T)) === zero(T)
        @test tanh(-zero(T)) === -zero(T)
        @test tanh(nextfloat(zero(T))) === nextfloat(zero(T))
        @test tanh(prevfloat(zero(T))) === prevfloat(zero(T))
        @test tanh(T(1000)) === one(T)
        @test tanh(-T(1000)) === -one(T)
        @test isnan_type(T, tanh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([H_SMALL_X(T), T(1.0), H_MEDIUM_X(T)]))
            @test tanh(x) ≈ tanh(big(x)) rtol=eps(T)
            @test tanh(-x) ≈ tanh(big(-x)) rtol=eps(T)
        end
    end
end

@testset "asinh" begin
    for T in (Float32, Float64)
        @test asinh(zero(T)) === zero(T)
        @test asinh(-zero(T)) === -zero(T)
        @test asinh(nextfloat(zero(T))) === nextfloat(zero(T))
        @test asinh(prevfloat(zero(T))) === prevfloat(zero(T))
        @test isnan_type(T, asinh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([T(2)^-28,T(2),T(2)^28]))
            @test asinh(x) ≈ asinh(big(x)) rtol=eps(T)
            @test asinh(-x) ≈ asinh(big(-x)) rtol=eps(T)
        end
    end
end

@testset "acosh" begin
    for T in (Float32, Float64)
        @test_throws DomainError acosh(T(0.1))
        @test acosh(one(T)) === zero(T)
        @test isnan_type(T, acosh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([nextfloat(T(1.0)), T(2), T(2)^28]))
            @test acosh(x) ≈ acosh(big(x)) rtol=eps(T)
        end
    end
end

@testset "atanh" begin
    for T in (Float32, Float64)
        @test_throws DomainError atanh(T(1.1))
        @test atanh(zero(T)) === zero(T)
        @test atanh(-zero(T)) === -zero(T)
        @test atanh(one(T)) === T(Inf)
        @test atanh(-one(T)) === -T(Inf)
        @test atanh(nextfloat(zero(T))) === nextfloat(zero(T))
        @test atanh(prevfloat(zero(T))) === prevfloat(zero(T))
        @test isnan_type(T, atanh(T(NaN)))
        for x in Iterators.flatten(pcnfloat.([T(2.0)^-28, T(0.5)]))
            @test atanh(x) ≈ atanh(big(x)) rtol=eps(T)
            @test atanh(-x) ≈ atanh(big(-x)) rtol=eps(T)
        end
    end
end

# Define simple wrapper of a Float type:
struct FloatWrapper <: Real
    x::Float64
end

import Base: +, -, *, /, ^, sin, cos, exp, sinh, cosh, convert, isfinite, float, promote_rule

for op in (:+, :-, :*, :/, :^)
    @eval $op(x::FloatWrapper, y::FloatWrapper) = FloatWrapper($op(x.x, y.x))
end

for op in (:sin, :cos, :exp, :sinh, :cosh, :-)
    @eval $op(x::FloatWrapper) = FloatWrapper($op(x.x))
end

for op in (:isfinite,)
    @eval $op(x::FloatWrapper) = $op(x.x)
end

convert(::Type{FloatWrapper}, x::Int) = FloatWrapper(float(x))
promote_rule(::Type{FloatWrapper}, ::Type{Int}) = FloatWrapper

float(x::FloatWrapper) = x

@testset "exp(Complex(a, b)) for a and b of non-standard real type #25292" begin

    x = FloatWrapper(3.1)
    y = FloatWrapper(4.1)

    @test sincos(x) == (sin(x), cos(x))

    z = Complex(x, y)

    @test isa(exp(z), Complex)
    @test isa(sin(z), Complex)
    @test isa(cos(z), Complex)
end

@testset "cbrt" begin
    for T in (Float32, Float64)
        @test cbrt(zero(T)) === zero(T)
        @test cbrt(-zero(T)) === -zero(T)
        @test cbrt(one(T)) === one(T)
        @test cbrt(-one(T)) === -one(T)
        @test cbrt(T(Inf)) === T(Inf)
        @test cbrt(-T(Inf)) === -T(Inf)
        @test isnan_type(T, cbrt(T(NaN)))
        for x in (pcnfloat(nextfloat(nextfloat(zero(T))))...,
                  pcnfloat(prevfloat(prevfloat(zero(T))))...,
                  0.45, 0.6, 0.98,
                  map(x->x^3, 1.0:1.0:1024.0)...,
                  nextfloat(-T(Inf)), prevfloat(T(Inf)))
            by = cbrt(big(T(x)))
            @test cbrt(T(x)) ≈ by rtol=eps(T)
            bym = cbrt(big(T(-x)))
            @test cbrt(T(-x)) ≈ bym rtol=eps(T)
        end
    end
end

@testset "hypot" begin
    @test hypot(0, 0) == 0.0
    @test hypot(3, 4) == 5.0
    @test hypot(NaN, Inf) == Inf
    @test hypot(Inf, NaN) == Inf
    @test hypot(Inf, Inf) == Inf

    isdefined(Main, :Furlongs) || @eval Main include("testhelpers/Furlongs.jl")
    using .Main.Furlongs
    @test hypot(Furlong(0), Furlong(0)) == Furlong(0.0)
    @test hypot(Furlong(3), Furlong(4)) == Furlong(5.0)
    @test hypot(Complex(3), Complex(4)) == 5.0
end
