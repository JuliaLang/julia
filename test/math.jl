# airy
@assert_approx_eq airy(1.8) 0.0470362168668458052247
@assert_approx_eq airyprime(1.8) -0.0685247801186109345638
@assert_approx_eq airybi(1.8) 2.595869356743906290060
@assert_approx_eq airybiprime(1.8) 2.98554005084659907283

# besselh
true_h133 = 0.30906272225525164362 - 0.53854161610503161800im
@assert_approx_eq besselh(3,1,3) true_h133
@assert_approx_eq besselh(-3,1,3) -true_h133
@assert_approx_eq besselh(3,2,3) conj(true_h133)
@assert_approx_eq besselh(-3,2,3) -conj(true_h133)

# besseli
true_i33 = 0.95975362949600785698
@assert_approx_eq besseli(3,3) true_i33
@assert_approx_eq besseli(-3,3) true_i33
@assert_approx_eq besseli(3,-3) -true_i33
@assert_approx_eq besseli(-3,-3) -true_i33

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

@assert_approx_eq j33 0.30906272225525164362
@assert_approx_eq j43 0.13203418392461221033
@assert_approx_eq besselj(0.1, -0.4) 0.820421842809028916 + 0.266571215948350899im
@assert_approx_eq besselj(3.2, 1.3+0.6im) 0.01135309305831220201 + 0.03927719044393515275im
@assert_approx_eq besselj(1, 3im) 3.953370217402609396im

# besselk
true_k33 = 0.12217037575718356792
@assert_approx_eq besselk(3,3) true_k33
@assert_approx_eq besselk(-3,3) true_k33
true_k3m3 = -0.1221703757571835679 - 3.0151549516807985776im
@assert_approx_eq besselk(3,-3) true_k3m3
@assert_approx_eq besselk(-3,-3) true_k3m3

# bessely
y33 = bessely(3,3.)
@test bessely(3,3) == y33
@assert_approx_eq bessely(-3,3) -y33
@assert_approx_eq y33 -0.53854161610503161800
@assert_approx_eq bessely(3,-3) 0.53854161610503161800 - 0.61812544451050328724im

# beta, lbeta
@assert_approx_eq beta(3/2,7/2) 5pi/128
@assert_approx_eq beta(3,5) 1/105
@assert_approx_eq lbeta(5,4) log(beta(5,4))
@assert_approx_eq beta(5,4) beta(4,5)

# gamma, lgamma (complex argument)
@assert_approx_eq gamma(0.5) sqrt(pi)
@assert_approx_eq lgamma(1.4+3.7im) -3.7094025330996841898 + 2.4568090502768651184im
@assert_approx_eq lgamma(1.4+3.7im) log(gamma(1.4+3.7im))

# digamma
euler_mascheroni = 0.5772156649015329
for elty in (Float32, Float64)
    @assert_approx_eq digamma(convert(elty, 0.1)) convert(elty, -10.42375494041108)
    @assert_approx_eq -digamma(convert(elty, 1.0)) convert(elty, euler_mascheroni)
    @assert_approx_eq digamma(convert(elty, 2.0)) convert(elty, 0.4227843350984675)
    @assert_approx_eq digamma(convert(elty, 3.0)) convert(elty, 0.9227843350984675)
    @assert_approx_eq digamma(convert(elty, 4.0)) convert(elty, 1.256117668431801)
    @assert_approx_eq digamma(convert(elty, 5.0)) convert(elty, 1.506117668431801)
    @assert_approx_eq digamma(convert(elty, 10.0)) convert(elty, 2.251752589066721)
end

# eta, zeta
@assert_approx_eq eta(1) log(2)
@assert_approx_eq eta(2) pi^2/12
@assert_approx_eq zeta(0) -0.5
@assert_approx_eq zeta(2) pi^2/6
@assert_approx_eq zeta(4) pi^4/90
