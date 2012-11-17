require("../extras/Rmath")

srand(124)

function allEq(target::Vector{Float64}, current::Vector{Float64}, tolerance::Float64)
    @assert numel(target) == numel(current)
    if all(target == current)
        return true
    end
    xy = mean(abs(target - current))
    xn = mean(abs(target))
    if (isfinite(xn) && xn > tolerance)
        xy /= xn
    end
    @assert xy < tolerance
    return true
end

allEq(target::Vector{Float64}, current::Vector{Float64}) =
    allEq(target, current, sqrt(eps()))

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

n = 26

Rbeta	  = rbeta    (n, .8, 2)
Rbinom	  = rbinom   (n, 55, pi/16)
Rcauchy   = rcauchy  (n, 12, 2)
Rchisq	  = rchisq   (n, 3)
Rexp	  = rexp     (n, 2)
Rf	  = rf	     (n, 12, 6)
Rgamma	  = rgamma   (n, 2, 5)
Rgeom	  = rgeom    (n, pi/16)
Rhyper	  = rhyper   (n, 40, 30, 20)
Rlnorm	  = rlnorm   (n, -1, 3)
Rlogis	  = rlogis   (n, 12, 2)
Rnbinom   = rnbinom  (n, 7, .01)
Rnorm	  = rnorm    (n, -1, 3)
Rpois	  = rpois    (n, 12)
Rsignrank = rsignrank(n, 47)
Rt	  = rt	     (n, 11)
## Rt2 below (to preserve the following random numbers!)
Runif	  = runif    (n, .2, 2)
Rweibull  = rweibull (n, 3, 2)
Rwilcox   = rwilcox  (n, 13, 17)
Rt2	  = rt	     (n, 1.01)

Pbeta	  = pbeta    (Rbeta, .8, 2)
Pbinom	  = pbinom   (Rbinom, 55, pi/16)
Pcauchy   = pcauchy  (Rcauchy, 12, 2)
Pchisq	  = pchisq   (Rchisq, 3)
Pexp	  = pexp     (Rexp, 2)
Pf	  = pf	     (Rf, 12, 6)
Pgamma	  = pgamma   (Rgamma, 2, 5)
Pgeom	  = pgeom    (Rgeom, pi/16)
Phyper	  = phyper   (Rhyper, 40, 30, 20)
Plnorm	  = plnorm   (Rlnorm, -1, 3)
Plogis	  = plogis   (Rlogis, 12, 2)
Pnbinom   = pnbinom  (Rnbinom, 7, .01)
Pnorm	  = pnorm    (Rnorm, -1, 3)
Ppois	  = ppois    (Rpois, 12)
#Psignrank = psignrank(Rsignrank, 47)
Pt	  = pt	     (Rt, 11)
Pt2	  = pt	     (Rt2, 1.01)
Punif	  = punif    (Runif, .2, 2)
Pweibull  = pweibull (Rweibull, 3, 2)
#Pwilcox   = pwilcox  (Rwilcox, 13, 17)

dbeta	 (Rbeta, .8, 2)
dbinom	 (Rbinom, 55, pi/16)
dcauchy	 (Rcauchy, 12, 2)
dchisq	 (Rchisq, 3)
dexp	 (Rexp, 2)
df	 (Rf, 12, 6)
dgamma	 (Rgamma, 2, 5)
dgeom	 (Rgeom, pi/16)
dhyper	 (Rhyper, 40, 30, 20)
dlnorm	 (Rlnorm, -1, 3)
dlogis	 (Rlogis, 12, 2)
dnbinom	 (Rnbinom, 7, .01)
dnorm	 (Rnorm, -1, 3)
dpois	 (Rpois, 12)
#dsignrank(Rsignrank, 47)
dt	 (Rt, 11)
dunif	 (Runif, .2, 2)
dweibull (Rweibull, 3, 2)
#dwilcox	 (Rwilcox, 13, 17)

## Check q*(p*(.)) = identity
allEq(Rbeta,	  qbeta	   (Pbeta, .8, 2))
allEq(Rbinom,	  qbinom   (Pbinom, 55, pi/16))
allEq(Rcauchy,	  qcauchy  (Pcauchy, 12, 2))
allEq(Rchisq,	  qchisq   (Pchisq, 3))
allEq(Rexp,	  qexp	   (Pexp, 2))
allEq(Rf,	  qf	   (Pf, 12, 6))
allEq(Rgamma,	  qgamma   (Pgamma, 2, 5))
allEq(Rgeom,	  qgeom	   (Pgeom, pi/16))
allEq(Rhyper,	  qhyper   (Phyper, 40, 30, 20))
allEq(Rlnorm,	  qlnorm   (Plnorm, -1, 3))
allEq(Rlogis,	  qlogis   (Plogis, 12, 2))
allEq(Rnbinom,	  qnbinom  (Pnbinom, 7, .01))
allEq(Rnorm,	  qnorm	   (Pnorm, -1, 3))
allEq(Rpois,	  qpois	   (Ppois, 12))
#allEq(Rsignrank,  qsignrank(Psignrank, 47))
allEq(Rt,	  qt	   (Pt,	11))
allEq(Rt2,	  qt	   (Pt2, 1.01), 1e-2)
allEq(Runif,	  qunif	   (Punif, .2, 2))
allEq(Rweibull,   qweibull (Pweibull, 3, 2))
#allEq(Rwilcox,	  qwilcox  (Pwilcox, 13, 17))

## Same with "upper tail":
allEq(Rbeta,	  qbeta	   (1- Pbeta, .8, 2, false))
allEq(Rbinom,	  qbinom   (1- Pbinom, 55, pi/16, false))
allEq(Rcauchy,	  qcauchy  (1- Pcauchy, 12, 2, false))
allEq(Rchisq,	  qchisq   (1- Pchisq, 3, false))
allEq(Rexp,	  qexp	   (1- Pexp, 2, false))
allEq(Rf,	  qf	   (1- Pf, 12, 6, false))
allEq(Rgamma,	  qgamma   (1- Pgamma, 2, 5, false))
allEq(Rgeom,	  qgeom	   (1- Pgeom, pi/16, false))
allEq(Rhyper,	  qhyper   (1- Phyper, 40, 30, 20, false))
allEq(Rlnorm,	  qlnorm   (1- Plnorm, -1, 3, false))
allEq(Rlogis,	  qlogis   (1- Plogis, 12, 2, false))
allEq(Rnbinom,	  qnbinom  (1- Pnbinom, 7, .01, false))
allEq(Rnorm,	  qnorm	   (1- Pnorm, -1, 3,false))
allEq(Rpois,	  qpois	   (1- Ppois, 12, false))
#allEq(Rsignrank,  qsignrank(1- Psignrank, 47, false))
allEq(Rt,	  qt	   (1- Pt,  11,   false))
allEq(Rt2,	  qt	   (1- Pt2, 1.01, false), 1e-2)
allEq(Runif,	  qunif	   (1- Punif, .2, 2, false))
allEq(Rweibull,   qweibull (1- Pweibull, 3, 2, false))
#allEq(Rwilcox,	  qwilcox  (1- Pwilcox, 13, 17, false))

## Check q*(p* ( log ), log) = identity
allEq(Rbeta,	  qbeta	   (log(Pbeta), .8, 2, true, true))
allEq(Rbinom,	  qbinom   (log(Pbinom), 55, pi/16, true, true))
allEq(Rcauchy,	  qcauchy  (log(Pcauchy), 12, 2, true, true))
allEq(Rchisq,     qchisq   (log(Pchisq), 3, true, true), 1e-14)
allEq(Rexp,	  qexp	   (log(Pexp), 2, true, true))
allEq(Rf,	  qf	   (log(Pf), 12, 6, true, true))
allEq(Rgamma,	  qgamma   (log(Pgamma), 2, 5, true, true))
allEq(Rgeom,	  qgeom	   (log(Pgeom), pi/16, true, true))
allEq(Rhyper,	  qhyper   (log(Phyper), 40, 30, 20, true, true))
allEq(Rlnorm,	  qlnorm   (log(Plnorm), -1, 3, true, true))
allEq(Rlogis,	  qlogis   (log(Plogis), 12, 2, true, true))
allEq(Rnbinom,	  qnbinom  (log(Pnbinom), 7, .01, true, true))
allEq(Rnorm,	  qnorm	   (log(Pnorm), -1, 3, true, true))
allEq(Rpois,	  qpois	   (log(Ppois), 12, true, true))
#allEq(Rsignrank,  qsignrank(log(Psignrank), 47, true, true))
allEq(Rt,	  qt	   (log(Pt), 11, true, true))
allEq(Rt2,	  qt	   (log(Pt2), 1.01, true, true), 1e-2)
allEq(Runif,	  qunif	   (log(Punif), .2, 2, true, true))
allEq(Rweibull,  qweibull (log(Pweibull), 3, 2, true, true))
#allEq(Rwilcox,	  qwilcox  (log(Pwilcox), m = 13, n = 17, true, true))

## same q*(p* (log) log) with upper tail:

allEq(Rbeta,	  qbeta	   (log(1- Pbeta), .8, 2, false, true))
allEq(Rbinom,	  qbinom   (log(1- Pbinom), 55, pi/16, false, true))
allEq(Rcauchy,	  qcauchy  (log(1- Pcauchy), 12, 2, false, true))
allEq(Rchisq,	  qchisq   (log(1- Pchisq), 3, false, true))
allEq(Rexp,	  qexp	   (log(1- Pexp), 2, false, true))
allEq(Rf,	  qf	   (log(1- Pf), 12, 6, false, true))
allEq(Rgamma,	  qgamma   (log(1- Pgamma), 2, 5, false, true))
allEq(Rgeom,	  qgeom	   (log(1- Pgeom), pi/16, false, true))
allEq(Rhyper,	  qhyper   (log(1- Phyper), 40, 30, 20, false, true))
allEq(Rlnorm,	  qlnorm   (log(1- Plnorm), -1, 3, false, true))
allEq(Rlogis,	  qlogis   (log(1- Plogis), 12, 2, false, true))
allEq(Rnbinom,	  qnbinom  (log(1- Pnbinom), 7, .01, false, true))
allEq(Rnorm,	  qnorm	   (log(1- Pnorm), -1, 3, false, true))
allEq(Rpois,	  qpois	   (log(1- Ppois), 12, false, true))
#allEq(Rsignrank, qsignrank(log(1- Psignrank), 47, false, true))
allEq(Rt,	  qt	   (log(1- Pt ), 11,   false, true))
allEq(Rt2,	  qt	   (log(1- Pt2), 1.01, false, true), 1e-2)
allEq(Runif,	  qunif	   (log(1- Punif), .2, 2, false, true))
allEq(Rweibull,   qweibull (log(1- Pweibull), 3, 2, false, true))
#allEq(Rwilcox,	  qwilcox  (log(1- Pwilcox), 13, 17, false, true))
