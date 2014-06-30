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
z = 1.8 + 1.0im
@test_approx_eq airyx(0, z) airy(0, z) * exp(2/3 * z * sqrt(z))
@test_approx_eq airyx(1, z) airy(1, z) * exp(2/3 * z * sqrt(z))
@test_approx_eq airyx(2, z) airy(2, z) * exp(-abs(real(2/3 * z * sqrt(z))))
@test_approx_eq airyx(3, z) airy(3, z) * exp(-abs(real(2/3 * z * sqrt(z))))

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

# scaled bessel[ijky] and hankelh[12]
for x in (1.0, 0.0, -1.0), y in (1.0, 0.0, -1.0), nu in (1.0, 0.0, -1.0)
    z = complex128(x + y * im)
    z == zero(z) || @test_approx_eq hankelh1x(nu, z) hankelh1(nu, z) * exp(-z * im)
    z == zero(z) || @test_approx_eq hankelh2x(nu, z) hankelh2(nu, z) * exp(z * im)
    (nu < 0 && z == zero(z)) || @test_approx_eq besselix(nu, z) besseli(nu, z) * exp(-abs(real(z)))
    (nu < 0 && z == zero(z)) || @test_approx_eq besseljx(nu, z) besselj(nu, z) * exp(-abs(imag(z)))
    z == zero(z) || @test_approx_eq besselkx(nu, z) besselk(nu, z) * exp(z)
    z == zero(z) || @test_approx_eq besselyx(nu, z) bessely(nu, z) * exp(-abs(imag(z)))
end
@test_throws Base.Math.AmosException hankelh1x(1, 0)
@test_throws Base.Math.AmosException hankelh2x(1, 0)
@test_throws Base.Math.AmosException besselix(-1, 0)
@test_throws Base.Math.AmosException besseljx(-1, 0)
@test besselkx(1, 0) == Inf
@test_throws Base.Math.AmosException besselyx(1, 0)

# beta, lbeta
@test_approx_eq beta(3/2,7/2) 5π/128
@test_approx_eq beta(3,5) 1/105
@test_approx_eq lbeta(5,4) log(beta(5,4))
@test_approx_eq beta(5,4) beta(4,5)
@test_approx_eq beta(-1/2, 3) -16/3
@test_approx_eq lbeta(-1/2, 3) log(16/3)

# gamma, lgamma (complex argument)
@test gamma(1:25) == gamma(Float64[1:25])
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

@test_approx_eq polygamma(20, 7.) -4.644616027240543262561198814998587152547

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

# useful test functions for relative error
err(z, x) = abs(z - x) / abs(x)
errc(z, x) = max(err(real(z),real(x)), err(imag(z),imag(x)))

for x in -10.2:0.3456:50
    @test 1e-12 > err(digamma(x+0im), digamma(x))
end

# digamma, trigamma, polygamma & zeta test cases (compared to Wolfram Alpha)
@test 1e-13 > err(digamma(7+0im), 1.872784335098467139393487909917597568957840664060076401194232)
@test 1e-13 > errc(digamma(7im), 1.94761433458434866917623737015561385331974500663251349960124 + 1.642224898223468048051567761191050945700191089100087841536im)
@test 1e-13 > errc(digamma(-3.2+0.1im), 4.65022505497781398615943030397508454861261537905047116427511+2.32676364843128349629415011622322040021960602904363963042380im)
@test 1e-13 > err(trigamma(8+0im), 0.133137014694031425134546685920401606452509991909746283540546)
@test 1e-13 > errc(trigamma(8im), -0.0078125000000000000029194973110119898029284994355721719150 - 0.12467345030312762782439017882063360876391046513966063947im)
@test 1e-13 > errc(trigamma(-3.2+0.1im), 15.2073506449733631753218003030676132587307964766963426965699+15.7081038855113567966903832015076316497656334265029416039199im)
@test 1e-13 > err(polygamma(2, 8.1+0im), -0.01723882695611191078960494454602091934457319791968308929600)
@test 1e-13 > errc(polygamma(30, 8.1+2im), -2722.8895150799704384107961215752996280795801958784600407589+6935.8508929338093162407666304759101854270641674671634631058im)
@test 1e-13 > errc(polygamma(3, 2.1+1im), 0.00083328137020421819513475400319288216246978855356531898998-0.27776110819632285785222411186352713789967528250214937861im)
@test 1e-11 > err(polygamma(3, -4.2 + 2im),-0.0037752884324358856340054736472407163991189965406070325067-0.018937868838708874282432870292420046797798431078848805822im)
@test 1e-13 > err(polygamma(13, 5.2 - 2im), 0.08087519202975913804697004241042171828113370070289754772448-0.2300264043021038366901951197725318713469156789541415899307im)
@test 1e-11 > err(polygamma(123, -47.2 + 0im), 5.7111648667225422758966364116222590509254011308116701029e291)
@test 1e-13 > errc(zeta(4.1+0.3im, -3.2+0.1im), -461.95403678374488506025596495576748255121001107881278765917+926.02552636148651929560277856510991293536052745360005500774im)
@test 1e-13 > errc(zeta(4.1+0.3im, 3.2+0.1im), 0.0121197525131633219465301571139288562254218365173899270675-0.00687228692565614267981577154948499247518236888933925740902im)
@test 1e-13 > errc(zeta(4.1, 3.2+0.1im),0.0137637451187986846516125754047084829556100290057521276517-0.00152194599531628234517456529686769063828217532350810111482im)
@test 1e-12 > errc(zeta(1.0001, -4.5e2+3.2im), 9993.89099199843392251301993718413132850540848778561412270571-3.13257480938495907945892330398176989805350557816701044268548im)
@test_throws DomainError zeta(3.1,-4.2)
@test 1e-13 > errc(zeta(3.1,-4.2+0im), -138.06320182025311080661516120845508778572835942189570145952+45.586579397698817209431034568162819207622092308850063038062im)
@test 1e-15 > errc(zeta(3.1+0im,-4.2), zeta(3.1,-4.2+0im))
@test 1e-13 > errc(zeta(3.1,4.2), 0.029938344862645948405021260567725078588893266227472565010234)
@test 1e-13 > err(zeta(27, 3.1), 5.413318813037879056337862215066960774064332961282599376e-14)
@test 1e-13 > err(zeta(27, 2), 7.4507117898354294919810041706041194547190318825658299932e-9)
@test 1e-12 > err(zeta(27, -105.3), -1.311372652244914148556295810515903234635727465138859603e14)
@test polygamma(4, -3.1+Inf*im) == polygamma(4, 3.1+Inf*im) == 0
@test polygamma(4, -0.0) == Inf == -polygamma(4, +0.0)
@test zeta(4, +0.0) == Inf == zeta(4, -0.0)
@test zeta(5, +0.0) == Inf == -zeta(5, -0.0)
@test isa([digamma(x) for x in [1.0]], Vector{Float64})
@test isa([trigamma(x) for x in [1.0]], Vector{Float64})
@test isa([polygamma(3,x) for x in [1.0]], Vector{Float64})
@test 1e-13 > errc(zeta(2 + 1im, -1.1), zeta(2 + 1im, -1.1+0im))
@test 1e-13 > errc(zeta(2 + 1im, -1.1), -1525.8095173321060982383023516086563741006869909580583246557 + 1719.4753293650912305811325486980742946107143330321249869576im)

@test @evalpoly(2,3,4,5,6) == 3+2*(4+2*(5+2*6)) == @evalpoly(2+0im,3,4,5,6)

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
