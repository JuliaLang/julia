# frexp,ldexp,significand,exponent
@test frexp(12.8) == (0.8,4)
@test ldexp(0.8,4) == 12.8
@test significand(12.8) == 1.6
@test exponent(12.8) == 3

# degree-based trig functions
for T = (Float32,Float64)
    for x = -400:40:400
        @test_approx_eq_eps sind(convert(T,x))::T convert(T,sin(pi/180*x)) eps(deg2rad(convert(T,x)))
        @test_approx_eq_eps cosd(convert(T,x))::T convert(T,cos(pi/180*x)) eps(deg2rad(convert(T,x)))
    end
    for x = 0.0:180:720
        @test sind(convert(T,x)) === zero(T)
        @test sind(-convert(T,x)) === -zero(T)
    end

    for x = -3:0.3:3
        @test_approx_eq_eps sinpi(convert(T,x))::T convert(T,sin(pi*x)) eps(pi*convert(T,x))
        @test_approx_eq_eps cospi(convert(T,x))::T convert(T,cos(pi*x)) eps(pi*convert(T,x))
    end
    for x = 0.0:1.0:4.0
        @test sinpi(convert(T,x)) === zero(T)
        @test sinpi(-convert(T,x)) === -zero(T)
    end
end

# check type stability
for T = (Float32,Float64,BigFloat)
    for f = (sind,cosd,sinpi,cospi)
        @test Base.return_types(f,(T,)) == [T]
    end
end


# error functions
@test_approx_eq erf(1) 0.84270079294971486934
@test_approx_eq erfc(1) 0.15729920705028513066
@test_approx_eq erfcx(1) 0.42758357615580700442
@test_approx_eq erfi(1) 1.6504257587975428760
@test_approx_eq erfinv(0.84270079294971486934) 1
@test_approx_eq erfcinv(0.15729920705028513066) 1
@test_approx_eq dawson(1) 0.53807950691276841914
# TODO: complex versions only supported on 64-bit for now
@unix_only if WORD_SIZE==64
    @test_approx_eq erf(1+2im) -0.53664356577856503399-5.0491437034470346695im
    @test_approx_eq erfc(1+2im) 1.5366435657785650340+5.0491437034470346695im
    @test_approx_eq erfcx(1+2im) 0.14023958136627794370-0.22221344017989910261im
    @test_approx_eq erfi(1+2im) -0.011259006028815025076+1.0036063427256517509im
    @test_approx_eq dawson(1+2im) -13.388927316482919244-11.828715103889593303im
end
for x in logspace(-200, -0.01)
    @test_approx_eq_eps erf(erfinv(x)) x 1e-12*x
    @test_approx_eq_eps erf(erfinv(-x)) -x 1e-12*x
    @test_approx_eq_eps erfc(erfcinv(2*x)) 2*x 1e-12*x
    if x > 1e-20
        xf = float32(x)
        @test_approx_eq_eps erf(erfinv(xf)) xf 1e-5*xf
        @test_approx_eq_eps erf(erfinv(-xf)) -xf 1e-5*xf
        @test_approx_eq_eps erfc(erfcinv(2xf)) 2xf 1e-5*xf
    end
end

# airy
@test_approx_eq airy(1.8) 0.0470362168668458052247
@test_approx_eq airyprime(1.8) -0.0685247801186109345638
@test_approx_eq airybi(1.8) 2.595869356743906290060
@test_approx_eq airybiprime(1.8) 2.98554005084659907283
@test_throws Base.Math.AmosException airy(200im)
@test_throws Base.Math.AmosException airybi(200)

# besselh
true_h133 = 0.30906272225525164362 - 0.53854161610503161800im
@test_approx_eq besselh(3,1,3) true_h133
@test_approx_eq besselh(-3,1,3) -true_h133
@test_approx_eq besselh(3,2,3) conj(true_h133)
@test_approx_eq besselh(-3,2,3) -conj(true_h133)
@test_throws Base.Math.AmosException besselh(1,0)


# besseli
true_i33 = 0.95975362949600785698
@test_approx_eq besseli(3,3) true_i33
@test_approx_eq besseli(-3,3) true_i33
@test_approx_eq besseli(3,-3) -true_i33
@test_approx_eq besseli(-3,-3) -true_i33
@test_throws Base.Math.AmosException besseli(1,1000)

# besselj
@test besselj(0,0) == 1
for i = 1:5
    @test besselj(i,0) == 0
    @test besselj(-i,0) == 0
end

j33 = besselj(3,3.)
@test besselj(3,3) == j33
@test besselj(-3,-3) == j33
@test besselj(-3,3) == -j33
@test besselj(3,-3) == -j33

j43 = besselj(4,3.)
@test besselj(4,3) == j43
@test besselj(-4,-3) == j43
@test besselj(-4,3) == j43
@test besselj(4,-3) == j43

@test_approx_eq j33 0.30906272225525164362
@test_approx_eq j43 0.13203418392461221033
@test_throws DomainError    besselj(0.1, -0.4)
@test_approx_eq besselj(0.1, complex(-0.4)) 0.820421842809028916 + 0.266571215948350899im
@test_approx_eq besselj(3.2, 1.3+0.6im) 0.01135309305831220201 + 0.03927719044393515275im
@test_approx_eq besselj(1, 3im) 3.953370217402609396im
@test_throws Base.Math.AmosException besselj(20,1000im)

# besselk
true_k33 = 0.12217037575718356792
@test_approx_eq besselk(3,3) true_k33
@test_approx_eq besselk(-3,3) true_k33
true_k3m3 = -0.1221703757571835679 - 3.0151549516807985776im
@test_throws DomainError besselk(3,-3)
@test_approx_eq besselk(3,complex(-3)) true_k3m3
@test_approx_eq besselk(-3,complex(-3)) true_k3m3
@test_throws Base.Math.AmosException besselk(200,0.01)
# issue #6564
@test besselk(1.0,0.0) == Inf

# bessely
y33 = bessely(3,3.)
@test bessely(3,3) == y33
@test_approx_eq bessely(-3,3) -y33
@test_approx_eq y33 -0.53854161610503161800
@test_throws DomainError bessely(3,-3)
@test_approx_eq bessely(3,complex(-3)) 0.53854161610503161800 - 0.61812544451050328724im
@test_throws Base.Math.AmosException bessely(200.5,0.1)

# issue #6653
for f in (besselj,bessely,besseli,besselk,hankelh1,hankelh2)
    @test_approx_eq f(0,1) f(0,complex128(1))
    @test_approx_eq f(0,1) f(0,complex64(1))
end

# beta, lbeta
@test_approx_eq beta(3/2,7/2) 5π/128
@test_approx_eq beta(3,5) 1/105
@test_approx_eq lbeta(5,4) log(beta(5,4))
@test_approx_eq beta(5,4) beta(4,5)
@test_approx_eq beta(-1/2, 3) -16/3
@test_approx_eq lbeta(-1/2, 3) log(16/3)

# gamma, lgamma (complex argument)
@test_approx_eq gamma(1/2) sqrt(π)
@test_approx_eq gamma(-1/2) -2sqrt(π)
@test_approx_eq lgamma(-1/2) log(abs(gamma(-1/2)))
@test_approx_eq lgamma(1.4+3.7im) -3.7094025330996841898 + 2.4568090502768651184im
@test_approx_eq lgamma(1.4+3.7im) log(gamma(1.4+3.7im))

# digamma
for elty in (Float32, Float64)

    @test_approx_eq digamma(convert(elty, 9)) convert(elty, 2.140641477955609996536345)
    @test_approx_eq digamma(convert(elty, 2.5)) convert(elty, 0.7031566406452431872257)
    @test_approx_eq digamma(convert(elty, 0.1)) convert(elty, -10.42375494041107679516822)
    @test_approx_eq digamma(convert(elty, 7e-4)) convert(elty, -1429.147493371120205005198)
    @test_approx_eq digamma(convert(elty, 7e-5)) convert(elty, -14286.29138623969227538398)
    @test_approx_eq digamma(convert(elty, 7e-6)) convert(elty, -142857.7200612932791081972)
    @test_approx_eq digamma(convert(elty, 2e-6)) convert(elty, -500000.5772123750382073831)
    @test_approx_eq digamma(convert(elty, 1e-6)) convert(elty, -1000000.577214019968668068)
    @test_approx_eq digamma(convert(elty, 7e-7)) convert(elty, -1428572.005785942019703646)
    @test_approx_eq digamma(convert(elty, -0.5)) convert(elty, .03648997397857652055902367)
    @test_approx_eq digamma(convert(elty, -1.1)) convert(elty,  10.15416395914385769902271)

    @test_approx_eq digamma(convert(elty, 0.1)) convert(elty, -10.42375494041108)
    @test_approx_eq digamma(convert(elty, 1/2)) convert(elty, -γ - log(4))
    @test_approx_eq digamma(convert(elty, 1)) convert(elty, -γ)
    @test_approx_eq digamma(convert(elty, 2)) convert(elty, 1 - γ)
    @test_approx_eq digamma(convert(elty, 3)) convert(elty, 3/2 - γ)
    @test_approx_eq digamma(convert(elty, 4)) convert(elty, 11/6 - γ)
    @test_approx_eq digamma(convert(elty, 5)) convert(elty, 25/12 - γ)
    @test_approx_eq digamma(convert(elty, 10)) convert(elty, 7129/2520 - γ)
end

# trigamma
for elty in (Float32, Float64)
    @test_approx_eq trigamma(convert(elty, 0.1)) convert(elty, 101.433299150792758817)
    @test_approx_eq trigamma(convert(elty, 1/2)) convert(elty, π^2/2)
    @test_approx_eq trigamma(convert(elty, 1)) convert(elty, π^2/6)
    @test_approx_eq trigamma(convert(elty, 2)) convert(elty, π^2/6 - 1)
    @test_approx_eq trigamma(convert(elty, 3)) convert(elty, π^2/6 - 5/4)
    @test_approx_eq trigamma(convert(elty, 4)) convert(elty, π^2/6 - 49/36)
    @test_approx_eq trigamma(convert(elty, 5)) convert(elty, π^2/6 - 205/144)
    @test_approx_eq trigamma(convert(elty, 10)) convert(elty, π^2/6 - 9778141/6350400)
end

# invdigamma
for elty in (Float32, Float64)
    for val in [0.001, 0.01, 0.1, 1.0, 10.0]
        @test abs(invdigamma(digamma(convert(elty, val))) - convert(elty, val)) < 1e-8
    end
end

# eta, zeta
@test_approx_eq eta(1) log(2)
@test_approx_eq eta(2) pi^2/12
@test_approx_eq zeta(0) -0.5
@test_approx_eq zeta(2) pi^2/6
@test_approx_eq zeta(4) pi^4/90

# quadgk
@test_approx_eq quadgk(cos, 0,0.7,1)[1] sin(1)
@test_approx_eq quadgk(x -> exp(im*x), 0,0.7,1)[1] (exp(1im)-1)/im
@test_approx_eq quadgk(x -> exp(im*x), 0,1im)[1] -1im*expm1(-1)
@test_approx_eq_eps quadgk(cos, 0,BigFloat(1),order=40)[1] sin(BigFloat(1)) 1000*eps(BigFloat)
@test_approx_eq quadgk(x -> exp(-x), 0,0.7,Inf)[1] 1.0
@test_approx_eq quadgk(x -> exp(x), -Inf,0)[1] 1.0
@test_approx_eq quadgk(x -> exp(-x^2), -Inf,Inf)[1] sqrt(pi)
@test_approx_eq quadgk(x -> [exp(-x), exp(-2x)], 0, Inf)[1] [1,0.5]
@test_approx_eq quadgk(cos, 0,0.7,1, norm=abs)[1] sin(1)

# Ensure subnormal flags functions don't segfault
@test any(ccall("jl_zero_subnormals", Uint8, (Uint8,), 1) .== [0x00 0x01])
@test any(ccall("jl_zero_subnormals", Uint8, (Uint8,), 0) .== [0x00 0x01])

# isqrt (issue #4884)
@test isqrt(9223372030926249000) == 3037000498
@test isqrt(typemax(Int128)) == int128("13043817825332782212")
@test isqrt(int128(typemax(Int64))^2-1) == 9223372036854775806
@test isqrt(0) == 0
for i = 1:1000
    n = rand(Uint128)
    s = isqrt(n)
    @test s*s <= n
    @test (s+1)*(s+1) > n
    n = rand(Uint64)
    s = isqrt(n)
    @test s*s <= n
    @test (s+1)*(s+1) > n
end
