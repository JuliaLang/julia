# R_pow
@assert abs(R_pow(2.0, 3) - 8.0) < 10e-8
@assert abs(R_pow(2.0, 1/2) - sqrt(2.0)) < 10e-8

# dbeta
@assert abs(dbeta(-1, 1, 1) - 0.0) < 10e-8
@assert abs(dbeta(0, 1, 1) - 1.0) < 10e-8
@assert abs(dbeta(1, 1, 1) - 1.0) < 10e-8

# dbinom
@assert abs(dbinom(0, 2, 0.5) - 0.25) < 10e-8
@assert abs(dbinom(1, 2, 0.5) - 0.5) < 10e-8
@assert abs(dbinom(2, 2, 0.5) - 0.25) < 10e-8

# dcauchy
@assert abs(dcauchy(0, 0, 1) - (1 / pi) * (1 / ((0 - 0)^2 + 1^2))) < 10e-8
@assert abs(dcauchy(0, 1, 2) - (1 / pi) * (2 / ((0 - 1)^2 + 2^2))) < 10e-8

# dchisq
@assert abs(dchisq(1, 1) - let x = 1; k = 1; (x^((k / 2) - 1) * exp(-(x / 2))) / (2^(k / 2) * gamma(k / 2)) end) < 10e-8
@assert abs(dchisq(2, 3) - let x = 2; k = 3; (x^((k / 2) - 1) * exp(-(x / 2))) / (2^(k / 2) * gamma(k / 2)) end) < 10e-8

# dexp
@assert abs(dexp(1, 2) - (1 / 2) * exp(-(1 / 2) * 1)) < 10e-8
@assert abs(dexp(1, 3) - (1 / 3) * exp(-(1 / 3) * 1)) < 10e-8
@assert abs(dexp(2, 3) - (1 / 3) * exp(-(1 / 3) * 2)) < 10e-8
