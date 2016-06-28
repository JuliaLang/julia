# This file is a part of Julia. License is MIT: http://julialang.org/license
@testset "math" begin
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

    @test clamp([0, 1, 2, 3, 4], 1.0, 3.0) == [1.0, 1.0, 2.0, 3.0, 3.0]
    @test clamp([0 1; 2 3], 1.0, 3.0) == [1.0 1.0; 2.0 3.0]
    begin
        x = [0.0, 1.0, 2.0, 3.0, 4.0]
        clamp!(x, 1, 3)
        @test x == [1.0, 1.0, 2.0, 3.0, 3.0]
    end
end

@testset "constants" begin
    @test !(pi == e)
    @test !(e == 1//2)
    @test 1//2 <= e
    @test big(1//2) < e
    @test e < big(20//6)
    @test e^pi == exp(pi)
    @test e^2 == exp(2)
    @test e^2.4 == exp(2.4)
    @test e^(2//3) == exp(2//3)

    @test Float16(3.) < pi
    @test pi < Float16(4.)
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
    end
end

# We compare to BigFloat instead of hard-coding
# values, assuming that BigFloat has an independent and independently
# tested implementation.
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
            @test_approx_eq_eps asin(T(1)) T(pi)/2 eps(T)
            @test_approx_eq_eps atan(T(1)) T(pi)/4 eps(T)
            @test_approx_eq_eps atan2(T(1),T(1)) T(pi)/4 eps(T)
            @test isequal(cbrt(T(0)), T(0))
            @test isequal(cbrt(T(1)), T(1))
            @test isequal(cbrt(T(1000000000)), T(1000))
            @test isequal(cos(T(0)), T(1))
            @test_approx_eq_eps cos(T(pi)/2) T(0) eps(T)
            @test isequal(cos(T(pi)), T(-1))
            @test_approx_eq_eps exp(T(1)) T(e) 10*eps(T)
            @test isequal(exp10(T(1)), T(10))
            @test isequal(exp2(T(1)), T(2))
            @test isequal(expm1(T(0)), T(0))
            @test_approx_eq_eps expm1(T(1)) T(e)-1 10*eps(T)
            @test isequal(hypot(T(3),T(4)), T(5))
            @test isequal(log(T(1)), T(0))
            @test isequal(log(e,T(1)), T(0))
            @test_approx_eq_eps log(T(e)) T(1) eps(T)
            @test isequal(log10(T(1)), T(0))
            @test isequal(log10(T(10)), T(1))
            @test isequal(log1p(T(0)), T(0))
            @test_approx_eq_eps log1p(T(e)-1) T(1) eps(T)
            @test isequal(log2(T(1)), T(0))
            @test isequal(log2(T(2)), T(1))
            @test isequal(sin(T(0)), T(0))
            @test isequal(sin(T(pi)/2), T(1))
            @test_approx_eq_eps sin(T(pi)) T(0) eps(T)
            @test isequal(sqrt(T(0)), T(0))
            @test isequal(sqrt(T(1)), T(1))
            @test isequal(sqrt(T(100000000)), T(10000))
            @test isequal(tan(T(0)), T(0))
            @test_approx_eq_eps tan(T(pi)/4) T(1) eps(T)
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

@testset "deg2rad/rad2deg" begin
    @testset "$T" for T in (Int, Float64, BigFloat)
        @test deg2rad(T(180)) ≈ 1pi
        @test deg2rad(T[45, 60]) ≈ [pi/T(4), pi/T(3)]
        @test rad2deg([pi/T(4), pi/T(3)]) ≈ [45, 60]
        @test rad2deg(T(1)*pi) ≈ 180
        @test rad2deg(T(1)) ≈ rad2deg(true)
        @test deg2rad(T(1)) ≈ deg2rad(true)
    end
end

@testset "degree-based trig functions" begin
    @testset "$T" for T = (Float32,Float64,Rational{Int})
        fT = typeof(float(one(T)))
        for x = -400:40:400
            @test_approx_eq_eps sind(convert(T,x))::fT convert(fT,sin(pi/180*x)) eps(deg2rad(convert(fT,x)))
            @test_approx_eq_eps cosd(convert(T,x))::fT convert(fT,cos(pi/180*x)) eps(deg2rad(convert(fT,x)))
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
                @test_approx_eq_eps sinpi(convert(T,x))::fT convert(fT,sin(pi*x)) eps(pi*convert(fT,x))
                @test_approx_eq_eps cospi(convert(T,x))::fT convert(fT,cos(pi*x)) eps(pi*convert(fT,x))
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

@testset "error functions" begin
    @test erf(Float16(1)) ≈ 0.84270079294971486934
    @test erf(1) ≈ 0.84270079294971486934
    @test erfc(1) ≈ 0.15729920705028513066
    @test erfc(Float16(1)) ≈ 0.15729920705028513066
    @test erfcx(1) ≈ 0.42758357615580700442
    @test erfcx(Float32(1)) ≈ 0.42758357615580700442
    @test erfcx(Complex64(1)) ≈ 0.42758357615580700442
    @test erfi(1) ≈ 1.6504257587975428760
    @test erfinv(0.84270079294971486934) ≈ 1
    @test erfcinv(0.15729920705028513066) ≈ 1
    @test dawson(1) ≈ 0.53807950691276841914

    @test erf(1+2im) ≈ -0.53664356577856503399-5.0491437034470346695im
    @test erfc(1+2im) ≈ 1.5366435657785650340+5.0491437034470346695im
    @test erfcx(1+2im) ≈ 0.14023958136627794370-0.22221344017989910261im
    @test erfi(1+2im) ≈ -0.011259006028815025076+1.0036063427256517509im
    @test dawson(1+2im) ≈ -13.388927316482919244-11.828715103889593303im

    for elty in [Float32,Float64]
        for x in logspace(-200, -0.01)
            @test_approx_eq_eps erf(erfinv(x)) x 1e-12*x
            @test_approx_eq_eps erf(erfinv(-x)) -x 1e-12*x
            @test_approx_eq_eps erfc(erfcinv(2*x)) 2*x 1e-12*x
            if x > 1e-20
                xf = Float32(x)
                @test_approx_eq_eps erf(erfinv(xf)) xf 1e-5*xf
                @test_approx_eq_eps erf(erfinv(-xf)) -xf 1e-5*xf
                @test_approx_eq_eps erfc(erfcinv(2xf)) 2xf 1e-5*xf
            end
        end
        @test erfinv(one(elty)) == Inf
        @test erfinv(-one(elty)) == -Inf
        @test_throws DomainError erfinv(convert(elty,2.0))

        @test erfcinv(zero(elty)) == Inf
        @test_throws DomainError erfcinv(-one(elty))
    end

    @test erfinv(one(Int)) == erfinv(1.0)
    @test erfcinv(one(Int)) == erfcinv(1.0)
end

@testset "airy" begin
    @test airy(1.8) ≈ airyai(1.8)
    @test airyprime(1.8) ≈ -0.0685247801186109345638
    @test airyaiprime(1.8) ≈ airyprime(1.8)
    @test airybi(1.8) ≈ 2.595869356743906290060
    @test airybiprime(1.8) ≈ 2.98554005084659907283
    @test_throws Base.Math.AmosException airy(200im)
    @test_throws Base.Math.AmosException airybi(200)
    @test_throws ArgumentError airy(5,one(Complex128))
    z = 1.8 + 1.0im
    for elty in [Complex64,Complex128]
        @test airy(convert(elty,1.8)) ≈ 0.0470362168668458052247
        z = convert(elty,z)
        @test airyx(z) ≈ airyx(0,z)
        @test airyx(0, z) ≈ airy(0, z) * exp(2/3 * z * sqrt(z))
        @test airyx(1, z) ≈ airy(1, z) * exp(2/3 * z * sqrt(z))
        @test airyx(2, z) ≈ airy(2, z) * exp(-abs(real(2/3 * z * sqrt(z))))
        @test airyx(3, z) ≈ airy(3, z) * exp(-abs(real(2/3 * z * sqrt(z))))
        @test_throws ArgumentError airyx(5,z)
    end
    @test_throws MethodError airy(complex(big(1.0)))
end

@testset "bessel functions" begin
    bessel_funcs = [(bessely0, bessely1, bessely), (besselj0, besselj1, besselj)]
    @testset "$z, $o" for (z, o, f) in bessel_funcs
        @test z(Float32(2.0)) ≈ z(Float64(2.0))
        @test o(Float32(2.0)) ≈ o(Float64(2.0))
        @test z(2) ≈ z(2.0)
        @test o(2) ≈ o(2.0)
        @test z(2.0 + im) ≈ f(0, 2.0 + im)
        @test o(2.0 + im) ≈ f(1, 2.0 + im)
    end
    @testset "besselj error throwing" begin
        @test_throws MethodError besselj(1.2,big(1.0))
        @test_throws MethodError besselj(1,complex(big(1.0)))
        @test_throws MethodError besseljx(1,big(1.0))
        @test_throws MethodError besseljx(1,complex(big(1.0)))
    end
    @testset "besselh" begin
        true_h133 = 0.30906272225525164362 - 0.53854161610503161800im
        @test besselh(3,1,3) ≈ true_h133
        @test besselh(-3,1,3) ≈ -true_h133
        @test besselh(3,2,3) ≈ conj(true_h133)
        @test besselh(-3,2,3) ≈ -conj(true_h133)
        @testset "Error throwing" begin
            @test_throws Base.Math.AmosException besselh(1,0)
            @test_throws MethodError besselh(1,big(1.0))
            @test_throws MethodError besselh(1,complex(big(1.0)))
            @test_throws MethodError besselhx(1,big(1.0))
            @test_throws MethodError besselhx(1,complex(big(1.0)))
        end
    end
    @testset "besseli" begin
        true_i33 = 0.95975362949600785698
        @test besseli(3,3) ≈ true_i33
        @test besseli(-3,3) ≈ true_i33
        @test besseli(3,-3) ≈ -true_i33
        @test besseli(-3,-3) ≈ -true_i33
        @test besseli(Float32(-3),Complex64(-3,0)) ≈ -true_i33
        @testset "Error throwing" begin
            @test_throws Base.Math.AmosException besseli(1,1000)
            @test_throws DomainError besseli(0.4,-1.0)
            @test_throws MethodError besseli(1,big(1.0))
            @test_throws MethodError besseli(1,complex(big(1.0)))
            @test_throws MethodError besselix(1,big(1.0))
            @test_throws MethodError besselix(1,complex(big(1.0)))
        end
    end
    @testset "besselj" begin
        @test besselj(0,0) == 1
        for i = 1:5
            @test besselj(i,0) == 0
            @test besselj(-i,0) == 0
            @test besselj(-i,Float32(0)) == 0
            @test besselj(-i,Float32(0)) == 0
        end

        j33 = besselj(3,3.)
        @test besselj(3,3) == j33
        @test besselj(-3,-3) == j33
        @test besselj(-3,3) == -j33
        @test besselj(3,-3) == -j33
        @test besselj(3,3f0) ≈ j33
        @test besselj(3,complex(3.)) ≈ j33
        @test besselj(3,complex(3f0)) ≈ j33
        @test besselj(3,complex(3)) ≈ j33

        j43 = besselj(4,3.)
        @test besselj(4,3) == j43
        @test besselj(-4,-3) == j43
        @test besselj(-4,3) == j43
        @test besselj(4,-3) == j43
        @test besselj(4,3f0) ≈ j43
        @test besselj(4,complex(3.)) ≈ j43
        @test besselj(4,complex(3f0)) ≈ j43
        @test besselj(4,complex(3)) ≈ j43

        @test j33 ≈ 0.30906272225525164362
        @test j43 ≈ 0.13203418392461221033
        @test besselj(0.1, complex(-0.4)) ≈ 0.820421842809028916 + 0.266571215948350899im
        @test besselj(3.2, 1.3+0.6im) ≈ 0.01135309305831220201 + 0.03927719044393515275im
        @test besselj(1, 3im) ≈ 3.953370217402609396im
        @test besselj(1.0,3im) ≈ besselj(1,3im)
        @testset "Error throwing" begin
            @test_throws DomainError    besselj(0.1, -0.4)
            @test_throws Base.Math.AmosException besselj(20,1000im)
            @test_throws MethodError besselj(big(1.0),3im)
        end
    end

    @testset "besselk" begin
        true_k33 = 0.12217037575718356792
        @test besselk(3,3) ≈ true_k33
        @test besselk(-3,3) ≈ true_k33
        true_k3m3 = -0.1221703757571835679 - 3.0151549516807985776im
        @test besselk(3,complex(-3)) ≈ true_k3m3
        @test besselk(-3,complex(-3)) ≈ true_k3m3
        # issue #6564
        @test besselk(1.0,0.0) == Inf
        @testset "Error throwing" begin
            @test_throws Base.Math.AmosException besselk(200,0.01)
            @test_throws DomainError besselk(3,-3)
            @test_throws MethodError besselk(1,big(1.0))
            @test_throws MethodError besselk(1,complex(big(1.0)))
            @test_throws MethodError besselkx(1,big(1.0))
            @test_throws MethodError besselkx(1,complex(big(1.0)))
        end
    end

    @testset "bessely" begin
        y33 = bessely(3,3.)
        @test bessely(3,3) == y33
        @test bessely(3.,3.) == y33
        @test bessely(3,Float32(3.)) ≈ y33
        @test bessely(-3,3) ≈ -y33
        @test y33 ≈ -0.53854161610503161800
        @test bessely(3,complex(-3)) ≈ 0.53854161610503161800 - 0.61812544451050328724im
        @testset "Error throwing" begin
            @test_throws Base.Math.AmosException bessely(200.5,0.1)
            @test_throws DomainError bessely(3,-3)
            @test_throws DomainError bessely(0.4,-1.0)
            @test_throws DomainError bessely(0.4,Float32(-1.0))
            @test_throws DomainError bessely(1,Float32(-1.0))
            @test_throws DomainError bessely(Cint(3),Float32(-3.))
            @test_throws DomainError bessely(Cint(3),Float64(-3.))

            @test_throws MethodError bessely(1.2,big(1.0))
            @test_throws MethodError bessely(1,complex(big(1.0)))
            @test_throws MethodError besselyx(1,big(1.0))
            @test_throws MethodError besselyx(1,complex(big(1.0)))
        end
    end

    @testset "besselhx" begin
        for elty in [Complex64,Complex128]
            z = convert(elty, 1.0 + 1.9im)
            @test besselhx(1.0, 1, z) ≈ convert(elty,-0.5949634147786144 - 0.18451272807835967im)
            @test besselhx(Float32(1.0), 1, z) ≈ convert(elty,-0.5949634147786144 - 0.18451272807835967im)
        end
        @testset "Error throwing" begin
            @test_throws MethodError besselh(1,1,big(1.0))
            @test_throws MethodError besselh(1,1,complex(big(1.0)))
            @test_throws MethodError besselhx(1,1,big(1.0))
            @test_throws MethodError besselhx(1,1,complex(big(1.0)))
        end
    end
    @testset "scaled bessel[ijky] and hankelh[12]" begin
        for x in (1.0, 0.0, -1.0), y in (1.0, 0.0, -1.0), nu in (1.0, 0.0, -1.0)
            z = Complex128(x + y * im)
            z == zero(z) || @test hankelh1x(nu, z) ≈ hankelh1(nu, z) * exp(-z * im)
            z == zero(z) || @test hankelh2x(nu, z) ≈ hankelh2(nu, z) * exp(z * im)
            (nu < 0 && z == zero(z)) || @test besselix(nu, z) ≈ besseli(nu, z) * exp(-abs(real(z)))
            (nu < 0 && z == zero(z)) || @test besseljx(nu, z) ≈ besselj(nu, z) * exp(-abs(imag(z)))
            z == zero(z) || @test besselkx(nu, z) ≈ besselk(nu, z) * exp(z)
            z == zero(z) || @test besselyx(nu, z) ≈ bessely(nu, z) * exp(-abs(imag(z)))
        end
        @test besselkx(1, 0) == Inf
        @testset "Error throwing" begin
            @test_throws Base.Math.AmosException hankelh1x(1, 0)
            @test_throws Base.Math.AmosException hankelh2x(1, 0)
            @test_throws Base.Math.AmosException besselix(-1, 0)
            @test_throws Base.Math.AmosException besseljx(-1, 0)
            @test_throws Base.Math.AmosException besselyx(1, 0)
            @test_throws DomainError besselix(0.4,-1.0)
            @test_throws DomainError besseljx(0.4, -1.0)
            @test_throws DomainError besselkx(0.4,-1.0)
            @test_throws DomainError besselyx(0.4,-1.0)
        end
    end
    @testset "issue #6653" begin
        @testset "$f" for f in (besselj,bessely,besseli,besselk,hankelh1,hankelh2)
            @test f(0,1) ≈ f(0,Complex128(1))
            @test f(0,1) ≈ f(0,Complex64(1))
        end
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

@testset "gamma and friends" begin
    @testset "gamma, lgamma (complex argument)" begin
        if Base.Math.libm == "libopenlibm"
            @test gamma(Float64[1:25;]) == gamma(1:25)
        else
            @test gamma(Float64[1:25;]) ≈ gamma(1:25)
        end
        for elty in (Float32, Float64)
            @test gamma(convert(elty,1/2)) ≈ convert(elty,sqrt(π))
            @test gamma(convert(elty,-1/2)) ≈ convert(elty,-2sqrt(π))
            @test lgamma(convert(elty,-1/2)) ≈ convert(elty,log(abs(gamma(-1/2))))
        end
        @test lgamma(1.4+3.7im) ≈ -3.7094025330996841898 + 2.4568090502768651184im
        @test lgamma(1.4+3.7im) ≈ log(gamma(1.4+3.7im))
        @test lgamma(-4.2+0im) ≈ lgamma(-4.2)-pi*im
        @test factorial(3.0) == gamma(4.0) == factorial(3)
        for x in (3.2, 2+1im, 3//2, 3.2+0.1im)
            @test factorial(x) == gamma(1+x)
        end
        @test lfact(1) == 0
        @test lfact(2) == lgamma(3)
    end

    @testset "digamma" begin
        @testset "$elty" for elty in (Float32, Float64)
            @test digamma(convert(elty, 9)) ≈ convert(elty, 2.140641477955609996536345)
            @test digamma(convert(elty, 2.5)) ≈ convert(elty, 0.7031566406452431872257)
            @test digamma(convert(elty, 0.1)) ≈ convert(elty, -10.42375494041107679516822)
            @test digamma(convert(elty, 7e-4)) ≈ convert(elty, -1429.147493371120205005198)
            @test digamma(convert(elty, 7e-5)) ≈ convert(elty, -14286.29138623969227538398)
            @test digamma(convert(elty, 7e-6)) ≈ convert(elty, -142857.7200612932791081972)
            @test digamma(convert(elty, 2e-6)) ≈ convert(elty, -500000.5772123750382073831)
            @test digamma(convert(elty, 1e-6)) ≈ convert(elty, -1000000.577214019968668068)
            @test digamma(convert(elty, 7e-7)) ≈ convert(elty, -1428572.005785942019703646)
            @test digamma(convert(elty, -0.5)) ≈ convert(elty, .03648997397857652055902367)
            @test digamma(convert(elty, -1.1)) ≈ convert(elty,  10.15416395914385769902271)

            @test digamma(convert(elty, 0.1)) ≈ convert(elty, -10.42375494041108)
            @test digamma(convert(elty, 1/2)) ≈ convert(elty, -γ - log(4))
            @test digamma(convert(elty, 1)) ≈ convert(elty, -γ)
            @test digamma(convert(elty, 2)) ≈ convert(elty, 1 - γ)
            @test digamma(convert(elty, 3)) ≈ convert(elty, 3/2 - γ)
            @test digamma(convert(elty, 4)) ≈ convert(elty, 11/6 - γ)
            @test digamma(convert(elty, 5)) ≈ convert(elty, 25/12 - γ)
            @test digamma(convert(elty, 10)) ≈ convert(elty, 7129/2520 - γ)
        end
    end

    @testset "trigamma" begin
        @testset "$elty" for elty in (Float32, Float64)
            @test trigamma(convert(elty, 0.1)) ≈ convert(elty, 101.433299150792758817)
            @test trigamma(convert(elty, 0.1)) ≈ convert(elty, 101.433299150792758817)
            @test trigamma(convert(elty, 1/2)) ≈ convert(elty, π^2/2)
            @test trigamma(convert(elty, 1)) ≈ convert(elty, π^2/6)
            @test trigamma(convert(elty, 2)) ≈ convert(elty, π^2/6 - 1)
            @test trigamma(convert(elty, 3)) ≈ convert(elty, π^2/6 - 5/4)
            @test trigamma(convert(elty, 4)) ≈ convert(elty, π^2/6 - 49/36)
            @test trigamma(convert(elty, 5)) ≈ convert(elty, π^2/6 - 205/144)
            @test trigamma(convert(elty, 10)) ≈ convert(elty, π^2/6 - 9778141/6350400)
        end
    end

    @testset "invdigamma" begin
        @testset "$elty" for elty in (Float32, Float64)
            for val in [0.001, 0.01, 0.1, 1.0, 10.0]
                @test abs(invdigamma(digamma(convert(elty, val))) - convert(elty, val)) < 1e-8
            end
        end
        @test abs(invdigamma(2)) == abs(invdigamma(2.))
    end

    @testset "polygamma" begin
        @test polygamma(20, 7.) ≈ -4.644616027240543262561198814998587152547
        @test polygamma(20, Float16(7.)) ≈ -4.644616027240543262561198814998587152547
    end

    @testset "eta" begin
        @test eta(1) ≈ log(2)
        @test eta(2) ≈ pi^2/12
        @test eta(Float32(2)) ≈ eta(2)
        @test eta(Complex64(2)) ≈ eta(2)
    end
end

@testset "zeta" begin
    @test zeta(0) ≈ -0.5
    @test zeta(2) ≈ pi^2/6
    @test zeta(Complex64(2)) ≈ zeta(2)
    @test zeta(4) ≈ pi^4/90
    @test zeta(1,Float16(2.)) ≈ zeta(1,2.)
    @test zeta(1.,Float16(2.)) ≈ zeta(1,2.)
    @test zeta(Float16(1.),Float16(2.)) ≈ zeta(1,2.)
    @test isnan(zeta(NaN))
    @test isnan(zeta(1.0e0))
    @test isnan(zeta(1.0f0))
    @test isnan(zeta(complex(0,Inf)))
    @test isnan(zeta(complex(-Inf,0)))
end

@testset "quadgk" begin
    @test quadgk(cos, 0,0.7,1)[1] ≈ sin(1)
    @test quadgk(x -> exp(im*x), 0,0.7,1)[1] ≈ (exp(1im)-1)/im
    @test quadgk(x -> exp(im*x), 0,1im)[1] ≈ -1im*expm1(-1)
    @test_approx_eq_eps quadgk(cos, 0,BigFloat(1),order=40)[1] sin(BigFloat(1)) 1000*eps(BigFloat)
    @test quadgk(x -> exp(-x), 0,0.7,Inf)[1] ≈ 1.0
    @test quadgk(x -> exp(x), -Inf,0)[1] ≈ 1.0
    @test quadgk(x -> exp(-x^2), -Inf,Inf)[1] ≈ sqrt(pi)
    @test quadgk(x -> [exp(-x), exp(-2x)], 0, Inf)[1] ≈ [1,0.5]
    @test quadgk(cos, 0,0.7,1, norm=abs)[1] ≈ sin(1)
end

@testset "subnormal flags" begin
    # Ensure subnormal flags functions don't segfault
    @test any(set_zero_subnormals(true) .== [false,true])
    @test any(get_zero_subnormals() .== [false,true])
    @test set_zero_subnormals(false)
    @test !get_zero_subnormals()
end

# useful test functions for relative error, which differ from isapprox
# in that errc separately looks at the real and imaginary parts
err(z, x) = z == x ? 0.0 : abs(z - x) / abs(x)
errc(z, x) = max(err(real(z),real(x)), err(imag(z),imag(x)))
≅(a,b) = errc(a,b) ≤ 1e-13

for x in -10.2:0.3456:50
    @test 1e-12 > err(digamma(x+0im), digamma(x))
end

#(compared to Wolfram Alpha)
@testset "digamma, trigamma, polygamma & zeta" begin
    @test digamma(7+0im) ≅ 1.872784335098467139393487909917597568957840664060076401194232
    @test digamma(7im) ≅ 1.94761433458434866917623737015561385331974500663251349960124 + 1.642224898223468048051567761191050945700191089100087841536im
    @test digamma(-3.2+0.1im) ≅ 4.65022505497781398615943030397508454861261537905047116427511+2.32676364843128349629415011622322040021960602904363963042380im
    @test trigamma(8+0im) ≅ 0.133137014694031425134546685920401606452509991909746283540546
    @test trigamma(8im) ≅ -0.0078125000000000000029194973110119898029284994355721719150 - 0.12467345030312762782439017882063360876391046513966063947im
    @test trigamma(-3.2+0.1im) ≅ 15.2073506449733631753218003030676132587307964766963426965699+15.7081038855113567966903832015076316497656334265029416039199im
    @test polygamma(2, 8.1+0im) ≅ -0.01723882695611191078960494454602091934457319791968308929600
    @test polygamma(30, 8.1+2im) ≅ -2722.8895150799704384107961215752996280795801958784600407589+6935.8508929338093162407666304759101854270641674671634631058im
    @test polygamma(3, 2.1+1im) ≅ 0.00083328137020421819513475400319288216246978855356531898998-0.27776110819632285785222411186352713789967528250214937861im
    @test 1e-11 > err(polygamma(3, -4.2 + 2im),-0.0037752884324358856340054736472407163991189965406070325067-0.018937868838708874282432870292420046797798431078848805822im)
    @test polygamma(13, 5.2 - 2im) ≅ 0.08087519202975913804697004241042171828113370070289754772448-0.2300264043021038366901951197725318713469156789541415899307im
    @test 1e-11 > err(polygamma(123, -47.2 + 0im), 5.7111648667225422758966364116222590509254011308116701029e291)
    @test zeta(4.1+0.3im, -3.2+0.1im) ≅ -281.34474134962502296077659347175501181994490498591796647 + 286.55601240093672668066037366170168712249413003222992205im
    @test zeta(4.1+0.3im, 3.2+0.1im) ≅ 0.0121197525131633219465301571139288562254218365173899270675-0.00687228692565614267981577154948499247518236888933925740902im
    @test zeta(4.1, 3.2+0.1im) ≅ 0.0137637451187986846516125754047084829556100290057521276517-0.00152194599531628234517456529686769063828217532350810111482im
    @test 1e-12 > errc(zeta(1.0001, -4.5e2+3.2im), 10003.765660925877260544923069342257387254716966134385170 - 0.31956240712464746491659767831985629577542514145649468090im)
    @test zeta(3.1,-4.2) ≅ zeta(3.1,-4.2+0im) ≅ 149.7591329008219102939352965761913107060718455168339040295
    @test 1e-15 > errc(zeta(3.1+0im,-4.2), zeta(3.1,-4.2+0im))
    @test zeta(3.1,4.2) ≅ 0.029938344862645948405021260567725078588893266227472565010234
    @test zeta(27, 3.1) ≅ 5.413318813037879056337862215066960774064332961282599376e-14
    @test zeta(27, 2) ≅ 7.4507117898354294919810041706041194547190318825658299932e-9
    @test 1e-12 > err(zeta(27, -105.3), 1.3113726525492708826840989036205762823329453315093955e14)
    @test polygamma(4, -3.1+Inf*im) == polygamma(4, 3.1+Inf*im) == 0
    @test polygamma(4, -0.0) == Inf == -polygamma(4, +0.0)
    @test zeta(4, +0.0) == zeta(4, -0.0) ≅ pi^4 / 90
    @test zeta(5, +0.0) == zeta(5, -0.0) ≅ 1.036927755143369926331365486457034168057080919501912811974
    @test zeta(Inf, 1.) == 1
    @test zeta(Inf, 2.) == 0
    @test isnan(zeta(NaN, 1.))
    @test isa([digamma(x) for x in [1.0]], Vector{Float64})
    @test isa([trigamma(x) for x in [1.0]], Vector{Float64})
    @test isa([polygamma(3,x) for x in [1.0]], Vector{Float64})
    @test zeta(2 + 1im, -1.1) ≅ zeta(2 + 1im, -1.1+0im) ≅ -64.580137707692178058665068045847533319237536295165484548 + 73.992688148809018073371913557697318846844796582012921247im
    @test polygamma(3,5) ≈ polygamma(3,5.)

    @test zeta(-3.0, 7.0) ≅ -52919/120
    @test zeta(-3.0, -7.0) ≅ 94081/120
    @test zeta(-3.1, 7.2) ≅ -587.457736596403704429103489502662574345388906240906317350719
    @test zeta(-3.1, -7.2) ≅ 1042.167459863862249173444363794330893294733001752715542569576
    @test zeta(-3.1, 7.0) ≅ -518.431785723446831868686653718848680989961581500352503093748
    @test zeta(-3.1, -7.0) ≅ 935.1284612957581823462429983411337864448020149908884596048161
    @test zeta(-3.1-0.1im, 7.2) ≅ -579.29752287650299181119859268736614017824853865655709516268 - 96.551907752211554484321948972741033127192063648337407683877im
    @test zeta(-3.1-0.1im, -7.2) ≅ 1025.17607931184231774568797674684390615895201417983173984531 + 185.732454778663400767583204948796029540252923367115805842138im
    @test zeta(-3.1-0.1im, 7.2 + 0.1im) ≅ -571.66133526455569807299410569274606007165253039948889085762 - 131.86744836357808604785415199791875369679879576524477540653im
    @test zeta(-3.1-0.1im, -7.2 + 0.1im) ≅ 1035.35760409421020754141207226034191979220047873089445768189 + 130.905870774271320475424492384335798304480814695778053731045im
    @test zeta(-3.1-0.1im, -7.0 + 0.1im) ≅ 929.546530292101383210555114424269079830017210969572819344670 + 113.646687807533854478778193456684618838875194573742062527301im
    @test zeta(-3.1, 7.2 + 0.1im) ≅ -586.61801005507638387063781112254388285799318636946559637115 - 36.148831292706044180986261734913443701649622026758378669700im
    @test zeta(-3.1, -7.2 + 0.1im) ≅ 1041.04241628770682295952302478199641560576378326778432301623 - 55.7154858634145071137760301929537184886497572057171143541058im
    @test zeta(-13.4, 4.1) ≅ -3.860040842156185186414774125656116135638705768861917e6
    @test zeta(3.2, -4) ≅ 2.317164896026427640718298719837102378116771112128525719078
    @test zeta(3.2, 0) ≅ 1.166773370984467020452550350896512026772734054324169010977
    @test zeta(-3.2+0.1im, 0.0) ≅ zeta(-3.2+0.1im, 0.0+0im) ≅ 0.0070547946138977701155565365569392198424378109226519905493 + 0.00076891821792430587745535285452496914239014050562476729610im
    @test zeta(-3.2, 0.0) ≅ zeta(-3.2, 0.0+0im) ≅ 0.007011972077091051091698102914884052994997144629191121056378

    @test 1e-14 > err(eta(1+1e-9), 0.693147180719814213126976796937244130533478392539154928250926)
    @test 1e-14 > err(eta(1+5e-3), 0.693945708117842473436705502427198307157819636785324430166786)
    @test 1e-13 > err(eta(1+7.1e-3), 0.694280602623782381522315484518617968911346216413679911124758)
    @test 1e-13 > err(eta(1+8.1e-3), 0.694439974969407464789106040237272613286958025383030083792151)
    @test 1e-13 > err(eta(1 - 2.1e-3 + 2e-3 * im), 0.69281144248566007063525513903467244218447562492555491581+0.00032001240133205689782368277733081683574922990400416791019im)
    @test 1e-13 > err(eta(1 + 5e-3 + 5e-3 * im), 0.69394652468453741050544512825906295778565788963009705146+0.00079771059614865948716292388790427833787298296229354721960im)
    @test 1e-12 > errc(zeta(1e-3+1e-3im), -0.5009189365276307665899456585255302329444338284981610162-0.0009209468912269622649423786878087494828441941303691216750im)
    @test 1e-13 > errc(zeta(1e-4 + 2e-4im), -0.5000918637469642920007659467492165281457662206388959645-0.0001838278317660822408234942825686513084009527096442173056im)

    # Issue #7169: (TODO: better accuracy should be possible?)
    @test 1e-9 > errc(zeta(0 + 99.69im), 4.67192766128949471267133846066040655597942700322077493021802+3.89448062985266025394674304029984849370377607524207984092848im)
    @test 1e-12 > errc(zeta(3 + 99.69im), 1.09996958148566565003471336713642736202442134876588828500-0.00948220959478852115901654819402390826992494044787958181148im)
    @test 1e-9 > errc(zeta(-3 + 99.69im), 10332.6267578711852982128675093428012860119184786399673520976+13212.8641740351391796168658602382583730208014957452167440726im)
    @test 1e-13 > errc(zeta(2 + 99.69im, 1.3), 0.41617652544777996034143623540420694985469543821307918291931-0.74199610821536326325073784018327392143031681111201859489991im)
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
    for z in (1.234, 1.234 + 5.678im, [1.234, 5.678])
        @test cis(z) ≈ exp(im*z)
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
    @testset "$elty" for elty in (Float32, Float64)
        @test frexp( convert(elty,0.5) ) == (convert(elty,0.5),0)
        @test frexp( convert(elty,4.0) ) == (convert(elty,0.5),3)
        @test frexp( convert(elty,10.5) )[1] ≈ convert(elty,0.65625)
        @test frexp( convert(elty,10.5) )[2] == 4
        @test frexp( [ convert(elty,4.0) convert(elty,10.5) ] )[1][1] ≈ convert(elty,0.5)
        @test frexp( [ convert(elty,4.0) convert(elty,10.5) ] )[1][2] ≈ convert(elty,0.65625)
        @test frexp( [ convert(elty,4.0) convert(elty,10.5) ] )[2] == [ 3 4 ]
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
        airy, airyx, besselh, hankelh1, hankelh2, hankelh1x, hankelh2x,
        besseli, besselix, besselj, besseljx, besselk, besselkx, bessely, besselyx,
        polygamma, zeta, beta, lbeta,
    ]
    @testset "$f" for f in binary_math_functions
        x = y = 2
        v = [f(x,y)]
        @test f([x],y) == v
        @test f(x,[y]) == v
        @test f([x],[y]) == v
    end
end

@testset "issues #3024, #12822" begin
    @test_throws DomainError 2 ^ -2
    @test_throws DomainError (-2)^(2.2)
    @test_throws DomainError (-2.0)^(2.2)
end

@testset "issue #13748" begin
    let A = [1 2; 3 4]; B = [5 6; 7 8]; C = [9 10; 11 12]
        @test muladd(A,B,C) == A*B + C
    end
end

@test Base.Math.f32(complex(1.0,1.0)) == complex(Float32(1.),Float32(1.))
@test Base.Math.f16(complex(1.0,1.0)) == complex(Float16(1.),Float16(1.))
end
