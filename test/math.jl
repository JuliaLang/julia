# airy
@assert_approx_eq airy(1.8) 0.0470362
@assert_approx_eq airyprime(1.8) -0.0685248
@assert_approx_eq airybi(1.8) 2.59587
@assert_approx_eq airybiprime(1.8) 2.98554

# besselh
true_h133 = 0.309063 - 0.538542im
@assert_approx_eq besselh(3,1,3) true_h133
@assert_approx_eq besselh(-3,1,3) -true_h133
@assert_approx_eq besselh(3,2,3) conj(true_h133)
@assert_approx_eq besselh(-3,2,3) -conj(true_h133)

# besseli
true_i33 = 0.959754
@assert_approx_eq besseli(3,3) true_i33
@assert_approx_eq besseli(-3,3) true_i33
@assert_approx_eq besseli(3,-3) -true_i33
@assert_approx_eq besseli(-3,-3) -true_i33

# besselj
@assert besselj(0,0) == 1
for i = 1:5
    @assert besselj(i,0) == 0
    @assert besselj(-i,0) == 0
end

j33 = besselj(3,3.)
@assert besselj(3,3) == j33
@assert besselj(-3,-3) == j33
@assert besselj(-3,3) == -j33
@assert besselj(3,-3) == -j33

j43 = besselj(4,3.)
@assert besselj(4,3) == j43
@assert besselj(-4,-3) == j43
@assert besselj(-4,3) == j43
@assert besselj(4,-3) == j43

@assert_approx_eq j33 0.309063
@assert_approx_eq j43 0.132034
@assert_approx_eq besselj(0.1, -0.4) 0.820422 + 0.266571im
@assert_approx_eq besselj(3.2, 1.3+0.6im) 0.0113531 + 0.0392772im
@assert_approx_eq besselj(1, 3im) 3.95337im

# besselk
true_k33 = 0.12217
@assert_approx_eq besselk(3,3) true_k33
@assert_approx_eq besselk(-3,3) true_k33
true_k3m3 = -0.122170 - 3.015155im
@assert_approx_eq besselk(3,-3) true_k3m3
@assert_approx_eq besselk(-3,-3) true_k3m3

# bessely
y33 = bessely(3,3.)
@assert bessely(3,3) == y33
@assert_approx_eq bessely(-3,3) -y33
@assert_approx_eq y33 -0.538542
@assert_approx_eq bessely(3,-3) 0.538542 - 0.618125im

# beta, lbeta
@assert_approx_eq beta(3/2,7/2) 5pi/128
@assert_approx_eq beta(3,5) 1/105
@assert_approx_eq lbeta(5,4) log(beta(5,4))
@assert_approx_eq beta(5,4) beta(4,5)

# gamma, lgamma (complex argument)
@assert_approx_eq gamma(0.5) sqrt(pi)
@assert_approx_eq lgamma(1.4+3.7im) -3.709402533100+2.456809050277im
@assert_approx_eq lgamma(1.4+3.7im) log(gamma(1.4+3.7im))

# digamma
euler_mascheroni = 0.5772156649015329
@assert_approx_eq digamma(0.1) -10.42375494041108
@assert_approx_eq -digamma(1.0) euler_mascheroni
@assert_approx_eq digamma(2.0) 0.4227843350984675
@assert_approx_eq digamma(3.0) 0.9227843350984675
@assert_approx_eq digamma(4.0) 1.256117668431801
@assert_approx_eq digamma(5.0) 1.506117668431801
@assert_approx_eq digamma(10.0) 2.251752589066721

# eta, zeta
@assert_approx_eq eta(1) log(2)
@assert_approx_eq eta(2) pi^2/12
@assert_approx_eq zeta(0) -0.5
@assert_approx_eq zeta(2) pi^2/6
@assert_approx_eq zeta(4) pi^4/90
