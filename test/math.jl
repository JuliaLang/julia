# error functions
@test_approx_eq erf(1) 0.84270079294971486934
@test_approx_eq erfc(1) 0.15729920705028513066
@test_approx_eq erfcx(1) 0.42758357615580700442
@test_approx_eq erfi(1) 1.6504257587975428760
@test_approx_eq dawson(1) 0.53807950691276841914
# TODO: complex versions only supported on 64-bit for now
if WORD_SIZE==64
    @test_approx_eq erf(1+2im) -0.53664356577856503399-5.0491437034470346695im
    @test_approx_eq erfc(1+2im) 1.5366435657785650340+5.0491437034470346695im
    @test_approx_eq erfcx(1+2im) 0.14023958136627794370-0.22221344017989910261im
    @test_approx_eq erfi(1+2im) -0.011259006028815025076+1.0036063427256517509im
    @test_approx_eq dawson(1+2im) -13.388927316482919244-11.828715103889593303im
end

# airy
@test_approx_eq airy(1.8) 0.0470362168668458052247
@test_approx_eq airyprime(1.8) -0.0685247801186109345638
@test_approx_eq airybi(1.8) 2.595869356743906290060
@test_approx_eq airybiprime(1.8) 2.98554005084659907283

# besselh
true_h133 = 0.30906272225525164362 - 0.53854161610503161800im
@test_approx_eq besselh(3,1,3) true_h133
@test_approx_eq besselh(-3,1,3) -true_h133
@test_approx_eq besselh(3,2,3) conj(true_h133)
@test_approx_eq besselh(-3,2,3) -conj(true_h133)

# besseli
true_i33 = 0.95975362949600785698
@test_approx_eq besseli(3,3) true_i33
@test_approx_eq besseli(-3,3) true_i33
@test_approx_eq besseli(3,-3) -true_i33
@test_approx_eq besseli(-3,-3) -true_i33

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
@test_approx_eq besselj(0.1, -0.4) 0.820421842809028916 + 0.266571215948350899im
@test_approx_eq besselj(3.2, 1.3+0.6im) 0.01135309305831220201 + 0.03927719044393515275im
@test_approx_eq besselj(1, 3im) 3.953370217402609396im

# besselk
true_k33 = 0.12217037575718356792
@test_approx_eq besselk(3,3) true_k33
@test_approx_eq besselk(-3,3) true_k33
true_k3m3 = -0.1221703757571835679 - 3.0151549516807985776im
@test_approx_eq besselk(3,-3) true_k3m3
@test_approx_eq besselk(-3,-3) true_k3m3

# bessely
y33 = bessely(3,3.)
@test bessely(3,3) == y33
@test_approx_eq bessely(-3,3) -y33
@test_approx_eq y33 -0.53854161610503161800
@test_approx_eq bessely(3,-3) 0.53854161610503161800 - 0.61812544451050328724im

# beta, lbeta
@test_approx_eq beta(3/2,7/2) 5pi/128
@test_approx_eq beta(3,5) 1/105
@test_approx_eq lbeta(5,4) log(beta(5,4))
@test_approx_eq beta(5,4) beta(4,5)

# gamma, lgamma (complex argument)
@test_approx_eq gamma(0.5) sqrt(pi)
@test_approx_eq lgamma(1.4+3.7im) -3.7094025330996841898 + 2.4568090502768651184im
@test_approx_eq lgamma(1.4+3.7im) log(gamma(1.4+3.7im))

# digamma
euler_mascheroni = 0.5772156649015329
for elty in (Float32, Float64)
    @test_approx_eq digamma(convert(elty, 0.1)) convert(elty, -10.42375494041108)
    @test_approx_eq -digamma(convert(elty, 1.0)) convert(elty, euler_mascheroni)
    @test_approx_eq digamma(convert(elty, 2.0)) convert(elty, 0.4227843350984675)
    @test_approx_eq digamma(convert(elty, 3.0)) convert(elty, 0.9227843350984675)
    @test_approx_eq digamma(convert(elty, 4.0)) convert(elty, 1.256117668431801)
    @test_approx_eq digamma(convert(elty, 5.0)) convert(elty, 1.506117668431801)
    @test_approx_eq digamma(convert(elty, 10.0)) convert(elty, 2.251752589066721)
end

# eta, zeta
@test_approx_eq eta(1) log(2)
@test_approx_eq eta(2) pi^2/12
@test_approx_eq zeta(0) -0.5
@test_approx_eq zeta(2) pi^2/6
@test_approx_eq zeta(4) pi^4/90
