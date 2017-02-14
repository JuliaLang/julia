# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    begin
        x = [0.0, 1.0, 2.0, 3.0, 4.0]
        clamp!(x, 1, 3)
        @test x == [1.0, 1.0, 2.0, 3.0, 3.0]
    end
end

@testset "constants" begin
    @test pi != e
    @test e != 1//2
    @test 1//2 <= e
    @test e <= 15//3
    @test big(1//2) < e
    @test e < big(20//6)
    @test e^pi == exp(pi)
    @test e^2 == exp(2)
    @test e^2.4 == exp(2.4)
    @test e^(2//3) == exp(2//3)

    @test Float16(3.0) < pi
    @test pi < Float16(4.0)
    @test contains(sprint(show,π),"3.14159")
end

@testset "frexp,ldexp,significand,exponent" begin
    @testset "$T" for T in (Float16,Float32,Float64)
        for z in (zero(T),-zero(T))
            frexp(z) === (z,0)
            significand(z) === z
            @test_throws DomainError exponent(z)
        end

        for (a,b) in [(T(12.8),T(0.8)),
                      (prevfloat(realmin(T)), nextfloat(one(T),-2)),
                      (nextfloat(zero(T),3), T(0.75)),
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
        @test isnan(significand(convert(T,NaN)))
        x,y = frexp(convert(T,NaN))
        @test isnan(x)
        @test y == 0

        @testset "ldexp function" begin
            @test ldexp(T(0.0), 0) === T(0.0)
            @test ldexp(T(-0.0), 0) === T(-0.0)
            @test ldexp(T(Inf), 1) === T(Inf)
            @test ldexp(T(Inf), 10000) === T(Inf)
            @test ldexp(T(-Inf), 1) === T(-Inf)
            @test ldexp(T(NaN), 10) === T(NaN)
            @test ldexp(T(1.0), 0) === T(1.0)
            @test ldexp(T(0.8), 4) === T(12.8)
            @test ldexp(T(-0.854375), 5) === T(-27.34)
            @test ldexp(T(1.0), typemax(Int)) === T(Inf)
            @test ldexp(T(1.0), typemin(Int)) === T(0.0)
            @test ldexp(prevfloat(realmin(T)), typemax(Int)) === T(Inf)
            @test ldexp(prevfloat(realmin(T)), typemin(Int)) === T(0.0)

            @test ldexp(T(0.0), Int128(0)) === T(0.0)
            @test ldexp(T(-0.0), Int128(0)) === T(-0.0)
            @test ldexp(T(1.0), Int128(0)) === T(1.0)
            @test ldexp(T(0.8), Int128(4)) === T(12.8)
            @test ldexp(T(-0.854375), Int128(5)) === T(-27.34)
            @test ldexp(T(1.0), typemax(Int128)) === T(Inf)
            @test ldexp(T(1.0), typemin(Int128)) === T(0.0)
            @test ldexp(prevfloat(realmin(T)), typemax(Int128)) === T(Inf)
            @test ldexp(prevfloat(realmin(T)), typemin(Int128)) === T(0.0)

            @test ldexp(T(0.0), BigInt(0)) === T(0.0)
            @test ldexp(T(-0.0), BigInt(0)) === T(-0.0)
            @test ldexp(T(1.0), BigInt(0)) === T(1.0)
            @test ldexp(T(0.8), BigInt(4)) === T(12.8)
            @test ldexp(T(-0.854375), BigInt(5)) === T(-27.34)
            @test ldexp(T(1.0), BigInt(typemax(Int128))) === T(Inf)
            @test ldexp(T(1.0), BigInt(typemin(Int128))) === T(0.0)
            @test ldexp(prevfloat(realmin(T)), BigInt(typemax(Int128))) === T(Inf)
            @test ldexp(prevfloat(realmin(T)), BigInt(typemin(Int128))) === T(0.0)

            # Test also against BigFloat reference. Needs to be exactly rounded.
            @test ldexp(realmin(T), -1) == T(ldexp(big(realmin(T)), -1))
            @test ldexp(realmin(T), -2) == T(ldexp(big(realmin(T)), -2))
            @test ldexp(realmin(T)/2, 0) == T(ldexp(big(realmin(T)/2), 0))
            @test ldexp(realmin(T)/3, 0) == T(ldexp(big(realmin(T)/3), 0))
            @test ldexp(realmin(T)/3, -1) == T(ldexp(big(realmin(T)/3), -1))
            @test ldexp(realmin(T)/3, 11) == T(ldexp(big(realmin(T)/3), 11))
            @test ldexp(realmin(T)/11, -10) == T(ldexp(big(realmin(T)/11), -10))
            @test ldexp(-realmin(T)/11, -10) == T(ldexp(big(-realmin(T)/11), -10))
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
            @test x^yi ≈ big(x)^yi
            @test acos(x) ≈ acos(big(x))
            @test acosh(1+x) ≈ acosh(big(1+x))
            @test asin(x) ≈ asin(big(x))
            @test asinh(x) ≈ asinh(big(x))
            @test atan(x) ≈ atan(big(x))
            @test atan2(x,y) ≈ atan2(big(x),big(y))
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
        end
        @testset "Special values" begin
            @test isequal(T(1//4)^T(1//2), T(1//2))
            @test isequal(T(1//4)^2, T(1//16))
            @test isequal(acos(T(1)), T(0))
            @test isequal(acosh(T(1)), T(0))
            @test asin(T(1)) ≈ T(pi)/2 atol=eps(T)
            @test atan(T(1)) ≈ T(pi)/4 atol=eps(T)
            @test atan2(T(1),T(1)) ≈ T(pi)/4 atol=eps(T)
            @test isequal(cbrt(T(0)), T(0))
            @test isequal(cbrt(T(1)), T(1))
            @test isequal(cbrt(T(1000000000)), T(1000))
            @test isequal(cos(T(0)), T(1))
            @test cos(T(pi)/2) ≈ T(0) atol=eps(T)
            @test isequal(cos(T(pi)), T(-1))
            @test exp(T(1)) ≈ T(e) atol=10*eps(T)
            @test isequal(exp10(T(1)), T(10))
            @test isequal(exp2(T(1)), T(2))
            @test isequal(expm1(T(0)), T(0))
            @test expm1(T(1)) ≈ T(e)-1 atol=10*eps(T)
            @test isequal(hypot(T(3),T(4)), T(5))
            @test isequal(log(T(1)), T(0))
            @test isequal(log(e,T(1)), T(0))
            @test log(T(e)) ≈ T(1) atol=eps(T)
            @test isequal(log10(T(1)), T(0))
            @test isequal(log10(T(10)), T(1))
            @test isequal(log1p(T(0)), T(0))
            @test log1p(T(e)-1) ≈ T(1) atol=eps(T)
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
        end
        @testset "Inverses" begin
            @test acos(cos(x)) ≈ x
            @test acosh(cosh(x)) ≈ x
            @test asin(sin(x)) ≈ x
            @test cbrt(x)^3 ≈ x
            @test cbrt(x^3) ≈ x
            @test asinh(sinh(x)) ≈ x
            @test atan(tan(x)) ≈ x
            @test atan2(x,y) ≈ atan(x/y)
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
        end
        @testset "Edge cases" begin
            @test isinf(log(zero(T)))
            @test isnan(log(convert(T,NaN)))
            @test_throws DomainError log(-one(T))
            @test isinf(log1p(-one(T)))
            @test isnan(log1p(convert(T,NaN)))
            @test_throws DomainError log1p(convert(T,-2.0))
            @test hypot(T(0), T(0)) === T(0)
            @test hypot(T(Inf), T(Inf)) === T(Inf)
            @test hypot(T(Inf), T(x)) === T(Inf)
            @test hypot(T(Inf), T(NaN)) === T(Inf)
            @test isnan(hypot(T(x), T(NaN)))
        end
    end
end
@test exp10(5) ≈ exp10(5.0)
@test exp2(Float16(2.)) ≈ exp2(2.)
@test log(e) == 1

@testset "exp function" for T in (Float64, Float32)
    @testset "$T accuracy" begin
        X = map(T, vcat(-10:0.0002:10, -80:0.001:80, 2.0^-27, 2.0^-28, 2.0^-14, 2.0^-13))
        for x in X
            y, yb = exp(x), exp(big(x))
            @test abs(y-yb) <= 1.0*eps(T(yb))
        end
    end
    @testset "$T edge cases" begin
        @test isnan(exp(T(NaN)))
        @test exp(T(-Inf)) === T(0.0)
        @test exp(T(Inf)) === T(Inf)
        @test exp(T(0.0)) === T(1.0) # exact
        @test exp(T(5000.0)) === T(Inf)
        @test exp(T(-5000.0)) === T(0.0)
    end
end

@testset "test abstractarray trig fxns" begin
    TAA = rand(2,2)
    TAA = (TAA + TAA.')/2.
    STAA = Symmetric(TAA)
    @test full(atanh.(STAA)) == atanh.(TAA)
    @test full(asinh.(STAA)) == asinh.(TAA)
    @test full(acosh.(STAA+Symmetric(ones(TAA)))) == acosh.(TAA+ones(TAA))
    @test full(acsch.(STAA+Symmetric(ones(TAA)))) == acsch.(TAA+ones(TAA))
    @test full(acoth.(STAA+Symmetric(ones(TAA)))) == acoth.(TAA+ones(TAA))
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
end

@testset "degree-based trig functions" begin
    @testset "$T" for T = (Float32,Float64,Rational{Int})
        fT = typeof(float(one(T)))
        for x = -400:40:400
            @test sind(convert(T,x))::fT ≈ convert(fT,sin(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))
            @test cosd(convert(T,x))::fT ≈ convert(fT,cos(pi/180*x)) atol=eps(deg2rad(convert(fT,x)))
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

@testset "trig function type stability" begin
    @testset "$T $f" for T = (Float32,Float64,BigFloat), f = (sind,cosd,sinpi,cospi)
        @test Base.return_types(f,Tuple{T}) == [T]
    end
end

@testset "beta, lbeta" begin
    @test beta(3/2,7/2) ≈ 5π/128
    @test beta(3,5) ≈ 1/105
    @test lbeta(5,4) ≈ log(beta(5,4))
    @test beta(5,4) ≈ beta(4,5)
    @test beta(-1/2, 3) ≈ beta(-1/2 + 0im, 3 + 0im) ≈ -16/3
    @test lbeta(-1/2, 3) ≈ log(16/3)
    @test beta(Float32(5),Float32(4)) == beta(Float32(4),Float32(5))
    @test beta(3,5) ≈ beta(3+0im,5+0im)
    @test(beta(3.2+0.1im,5.3+0.3im) ≈ exp(lbeta(3.2+0.1im,5.3+0.3im)) ≈
          0.00634645247782269506319336871208405439180447035257028310080 -
          0.00169495384841964531409376316336552555952269360134349446910im)
end

# useful test functions for relative error, which differ from isapprox (≈)
# in that relerrc separately looks at the real and imaginary parts
relerr(z, x) = z == x ? 0.0 : abs(z - x) / abs(x)
relerrc(z, x) = max(relerr(real(z),real(x)), relerr(imag(z),imag(x)))
≅(a,b) = relerrc(a,b) ≤ 1e-13

@testset "gamma and friends" begin
    @testset "gamma, lgamma (complex argument)" begin
        if Base.Math.libm == "libopenlibm"
            @test gamma.(Float64[1:25;]) == gamma.(1:25)
        else
            @test gamma.(Float64[1:25;]) ≈ gamma.(1:25)
        end
        for elty in (Float32, Float64)
            @test gamma(convert(elty,1/2)) ≈ convert(elty,sqrt(π))
            @test gamma(convert(elty,-1/2)) ≈ convert(elty,-2sqrt(π))
            @test lgamma(convert(elty,-1/2)) ≈ convert(elty,log(abs(gamma(-1/2))))
        end
        @test lgamma(1.4+3.7im) ≈ -3.7094025330996841898 + 2.4568090502768651184im
        @test lgamma(1.4+3.7im) ≈ log(gamma(1.4+3.7im))
        @test lgamma(-4.2+0im) ≈ lgamma(-4.2)-5pi*im
        @test factorial(3.0) == gamma(4.0) == factorial(3)
        for x in (3.2, 2+1im, 3//2, 3.2+0.1im)
            @test factorial(x) == gamma(1+x)
        end
        @test lfact(1) == 0
        @test lfact(2) == lgamma(3)
    end

    # lgamma test cases (from Wolfram Alpha)
    @test lgamma(-300im) ≅ -473.17185074259241355733179182866544204963885920016823743 - 1410.3490664555822107569308046418321236643870840962522425im
    @test lgamma(3.099) ≅ lgamma(3.099+0im) ≅ 0.786413746900558058720665860178923603134125854451168869796
    @test lgamma(1.15) ≅ lgamma(1.15+0im) ≅ -0.06930620867104688224241731415650307100375642207340564554
    @test lgamma(0.89) ≅ lgamma(0.89+0im) ≅ 0.074022173958081423702265889979810658434235008344573396963
    @test lgamma(0.91) ≅ lgamma(0.91+0im) ≅ 0.058922567623832379298241751183907077883592982094770449167
    @test lgamma(0.01) ≅ lgamma(0.01+0im) ≅ 4.599479878042021722513945411008748087261001413385289652419
    @test lgamma(-3.4-0.1im) ≅ -1.1733353322064779481049088558918957440847715003659143454 + 12.331465501247826842875586104415980094316268974671819281im
    @test lgamma(-13.4-0.1im) ≅ -22.457344044212827625152500315875095825738672314550695161 + 43.620560075982291551250251193743725687019009911713182478im
    @test lgamma(-13.4+0.0im) ≅ conj(lgamma(-13.4-0.0im)) ≅ -22.404285036964892794140985332811433245813398559439824988 - 43.982297150257105338477007365913040378760371591251481493im
    @test lgamma(-13.4+8im) ≅ -44.705388949497032519400131077242200763386790107166126534 - 22.208139404160647265446701539526205774669649081807864194im
    @test lgamma(1+exp2(-20)) ≅ lgamma(1+exp2(-20)+0im) ≅ -5.504750066148866790922434423491111098144565651836914e-7
    @test lgamma(1+exp2(-20)+exp2(-19)*im) ≅ -5.5047799872835333673947171235997541985495018556426e-7 - 1.1009485171695646421931605642091915847546979851020e-6im
    @test lgamma(-300+2im) ≅ -1419.3444991797240659656205813341478289311980525970715668 - 932.63768120761873747896802932133229201676713644684614785im
    @test lgamma(300+2im) ≅ 1409.19538972991765122115558155209493891138852121159064304 + 11.4042446282102624499071633666567192538600478241492492652im
    @test lgamma(1-6im) ≅ -7.6099596929506794519956058191621517065972094186427056304 - 5.5220531255147242228831899544009162055434670861483084103im
    @test lgamma(1-8im) ≅ -10.607711310314582247944321662794330955531402815576140186 - 9.4105083803116077524365029286332222345505790217656796587im
    @test lgamma(1+6.5im) ≅ conj(lgamma(1-6.5im)) ≅ -8.3553365025113595689887497963634069303427790125048113307 + 6.4392816159759833948112929018407660263228036491479825744im
    @test lgamma(1+1im) ≅ conj(lgamma(1-1im)) ≅ -0.6509231993018563388852168315039476650655087571397225919 - 0.3016403204675331978875316577968965406598997739437652369im
    @test lgamma(-pi*1e7 + 6im) ≅ -5.10911758892505772903279926621085326635236850347591e8 - 9.86959420047365966439199219724905597399295814979993e7im
    @test lgamma(-pi*1e7 + 8im) ≅ -5.10911765175690634449032797392631749405282045412624e8 - 9.86959074790854911974415722927761900209557190058925e7im
    @test lgamma(-pi*1e14 + 6im) ≅ -1.0172766411995621854526383224252727000270225301426e16 - 9.8696044010873714715264929863618267642124589569347e14im
    @test lgamma(-pi*1e14 + 8im) ≅ -1.0172766411995628137711690403794640541491261237341e16 - 9.8696044010867038531027376655349878694397362250037e14im
    @test lgamma(2.05 + 0.03im) ≅ conj(lgamma(2.05 - 0.03im)) ≅ 0.02165570938532611215664861849215838847758074239924127515 + 0.01363779084533034509857648574107935425251657080676603919im
    @test lgamma(2+exp2(-20)+exp2(-19)*im) ≅ 4.03197681916768997727833554471414212058404726357753e-7 + 8.06398296652953575754782349984315518297283664869951e-7im

    @testset "lgamma for non-finite arguments" begin
        @test lgamma(Inf + 0im) === Inf + 0im
        @test lgamma(Inf - 0.0im) === Inf - 0.0im
        @test lgamma(Inf + 1im) === Inf + Inf*im
        @test lgamma(Inf - 1im) === Inf - Inf*im
        @test lgamma(-Inf + 0.0im) === -Inf - Inf*im
        @test lgamma(-Inf - 0.0im) === -Inf + Inf*im
        @test lgamma(Inf*im) === -Inf + Inf*im
        @test lgamma(-Inf*im) === -Inf - Inf*im
        @test lgamma(Inf + Inf*im) === lgamma(NaN + 0im) === lgamma(NaN*im) === NaN + NaN*im
    end
end

@testset "subnormal flags" begin
    # Ensure subnormal flags functions don't segfault
    @test any(set_zero_subnormals(true) .== [false,true])
    @test any(get_zero_subnormals() .== [false,true])
    @test set_zero_subnormals(false)
    @test !get_zero_subnormals()
end

@testset "evalpoly" begin
    @test @evalpoly(2,3,4,5,6) == 3+2*(4+2*(5+2*6)) == @evalpoly(2+0im,3,4,5,6)
    @test let evalcounts=0
              @evalpoly(begin
                            evalcounts += 1
                            4
                        end, 1,2,3,4,5)
              evalcounts
          end == 1
    a0 = 1
    a1 = 2
    c = 3
    @test @evalpoly(c, a0, a1) == 7
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
    # if using Tang's algorithm, should be accurate to within 0.56 ulps
    X = rand(100)
    for x in X
        for n = -5:5
            xn = ldexp(x,n)

            for T in (Float32,Float64)
                xt = T(x)

                y = Base.Math.JuliaLibm.log(xt)
                yb = log(big(xt))
                @test abs(y-yb) <= 0.56*eps(T(yb))

                y = Base.Math.JuliaLibm.log1p(xt)
                yb = log1p(big(xt))
                @test abs(y-yb) <= 0.56*eps(T(yb))

                if n <= 0
                    y = Base.Math.JuliaLibm.log1p(-xt)
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
        @test isnan(log(NaN))
        @test_throws DomainError log(-one(T))
        @test log1p(-one(T)) == -Inf
        @test isnan(log1p(NaN))
        @test_throws DomainError log1p(-2*one(T))
    end
end

@testset "vectorization of 2-arg functions" begin
    binary_math_functions = [
        copysign, flipsign, log, atan2, hypot, max, min,
        beta, lbeta,
    ]
    @testset "$f" for f in binary_math_functions
        x = y = 2
        v = [f(x,y)]
        @test f.([x],y) == v
        @test f.(x,[y]) == v
        @test f.([x],[y]) == v
    end
end

@testset "issues #3024, #12822" begin
    @test_throws DomainError 2 ^ -2
    @test_throws DomainError (-2)^(2.2)
    @test_throws DomainError (-2.0)^(2.2)
    @test_throws DomainError false ^ -2
    @test 1 ^ -2 === (-1) ^ -2 === 1
    @test (-1) ^ -3 === -1
    @test true ^ -2 === true
end

@testset "issue #13748" begin
    let A = [1 2; 3 4]; B = [5 6; 7 8]; C = [9 10; 11 12]
        @test muladd(A,B,C) == A*B + C
    end
end

# no domain error is thrown for negative values
@test invoke(cbrt, Tuple{AbstractFloat}, -1.0) == -1.0

@testset "promote Float16 irrational #15359" begin
    @test typeof(Float16(.5) * pi) == Float16
end
