module Distributions
using Base

export                                  # types
    Distribution,
    DiscreteDistribution,
    ContinuousDistribution,
    Bernoulli,
    Beta,
    Binomial,
    Categorical,
    Cauchy,
    Chisq,
    Dirichlet,
#    DiscreteUniform,            # need to add this
    Exponential,
    FDist,
    Gamma,
    Geometric,
    HyperGeometric,
    Logistic,
    logNormal,
    Multinomial,
    NegativeBinomial,
    NoncentralBeta,
    NoncentralChisq,
    NoncentralF,
    NoncentralT,
    Normal,
    Poisson,
    TDist,
    Uniform,
    Weibull,
                                        # methods
    ccdf,       # complementary cdf, i.e. 1 - cdf
    cdf,        # cumulative distribution function
    cquantile,  # complementary quantile (i.e. using prob in right hand tail)
    deviance,   # deviance of fitted and observed responses
    devresid,   # vector of squared deviance residuals
    insupport,  # predicate, is x in the support of the distribution?
    invlogccdf, # complementary quantile based on log probability
    invlogcdf,  # quantile based on log probability
    kurtosis,   # kurtosis of the distribution
    logccdf,    # ccdf returning log-probability
    logcdf,     # cdf returning log-probability
    logpdf,     # log probability density
    logpmf,     # log probability mass
    mean,       # mean of distribution
    median,     # median of distribution
    mustart,    # starting values of mean vector in GLMs
    pdf,        # probability density function (ContinuousDistribution)
    pmf,        # probability mass function (DiscreteDistribution)
    quantile,   # inverse of cdf (defined for p in (0,1))
    rand,       # random sampler
    sample,     # another random sampler - not sure why this is here
    skewness,   # skewness of the distribution
    std,        # standard deviation of distribution
    var         # variance of distribution

import Base.mean, Base.median, Base.quantile, Base.rand, Base.std, Base.var

abstract Distribution
abstract DiscreteDistribution   <: Distribution
abstract ContinuousDistribution <: Distribution

_jl_libRmath = dlopen("libRmath")

## Fallback methods, usually overridden for specific distributions
ccdf(d::Distribution, q::Real)                = 1 - cdf(d,q)
cquantile(d::Distribution, p::Real)           = quantile(d, 1-p)
function deviance{M<:Real,Y<:Real,W<:Real}(d::DiscreteDistribution,
                                           mu::AbstractArray{M},
                                           y::AbstractArray{Y},
                                           wt::AbstractArray{W})
    promote_shape(size(mu), promote_shape(size(y), size(wt)))
    ans = 0.
    for i in 1:numel(y)
        ans += wt[i] * logpmf(d, mu[i], y[i])
    end
    -2ans
end
function deviance{M<:Real,Y<:Real,W<:Real}(d::ContinuousDistribution,
                                           mu::AbstractArray{M},
                                           y::AbstractArray{Y},
                                           wt::AbstractArray{W})
    promote_shape(size(mu), promote_shape(size(y), size(wt))) 
    ans = 0.
    for i in 1:numel(y)
        ans += wt[i] * logpdf(d, mu[i], y[i])
    end
    -2ans
end
devresid(d::DiscreteDistribution, y::Real, mu::Real, wt::Real) =
    -2wt*logpmf(d, mu, y)
devresid(d::ContinuousDistribution, y::Real, mu::Real, wt::Real) =
    -2wt*logpdf(d, mu, y)
function devresid{Y<:Real,M<:Real,W<:Real}(d::Distribution,
                                           y::AbstractArray{Y},
                                           mu::AbstractArray{M},
                                           wt::AbstractArray{W})
    R = Array(Float64, promote_shape(size(y), promote_shape(size(mu), size(wt))))
    for i in 1:numel(mu)
        R[i] = devResid(d, y[i], mu[i], wt[i])
    end
    R
end
invlogccdf(d::Distribution, lp::Real)         = quantile(d, exp(-lp))
invlogcdf(d::Distribution, lp::Real)          = quantile(d, exp(lp))
logccdf(d::Distribution, q::Real)             = log(ccdf(d,q))
logcdf(d::Distribution, q::Real)              = log(cdf(d,q))
logpdf(d::ContinuousDistribution, x::Real)    = log(pdf(d,x))
logpmf(d::DiscreteDistribution, x::Real)      = log(pmf(d,x))
function mustart{Y<:Real,W<:Real}(d::Distribution,
                                  y::AbstractArray{Y},
                                  wt::AbstractArray{W})
    M = Array(Float64, promote_shape(size(y), size(wt)))
    for i in 1:numel(M)
        M[i] = mustart(d, y[i], wt[i])
    end
    M
end
std(d::Distribution)                          = sqrt(var(d))
function rand!(d::ContinuousDistribution, A::Array{Float64})
    for i in 1:numel(A) A[i] = rand(d) end
    A
end
rand(d::ContinuousDistribution, dims::Dims)   = rand!(d, Array(Float64,dims))
rand(d::ContinuousDistribution, dims::Int...) = rand(d, dims)
function rand!(d::DiscreteDistribution, A::Array{Int})
    for i in 1:numel(A) A[i] = int(rand(d)) end
    A
end
rand(d::DiscreteDistribution, dims::Dims)     = rand!(d, Array(Int,dims))
rand(d::DiscreteDistribution, dims::Int...)   = rand(d, dims)
function var{M<:Real}(d::Distribution, mu::AbstractArray{M})
    V = similar(mu, Float64)
    for i in 1:numel(mu)
        V[i] = var(d, mu[i])
    end
    V
end

function insupport{T<:Real}(d::Distribution, x::AbstractArray{T})
    for e in x
        if !insupport(d, e)
            return false
        end
    end
    true
end

## FIXME: Replace the three _jl_dist_*p macros with one by defining
## the argument tuples for the ccall dynamically from pn
macro _jl_dist_1p(T, b)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    Ty = eval(T)
    dc = Ty <: DiscreteDistribution
    pf = dc ? :pmf : :pdf
    lf = dc ? :logpmf : :logpdf
    pn = Ty.names                       # parameter names
    p  = expr(:quote,pn[1])
    quote
        global $pf,$lf,cdf,logcdf,ccdf,logccdf,quantile,cquantile,invlogcdf,invlogccdf,rand
        function ($pf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Int32),
                  x, d.($p), 0)
        end
        function ($lf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Int32),
                  x, d.($p), 1)
        end
        function cdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Int32, Int32),
                  q, d.($p), 1, 0)
        end
        function logcdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath,  $pp),
                  Float64, (Float64, Float64, Int32, Int32),
                  q, d.($p), 1, 1)
        end
        function ccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Int32, Int32),
                  q, d.($p), 0, 0)
        end
        function logccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Int32, Int32),
                  q, d.($p), 0, 1)
        end
        function quantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Int32, Int32),
                  p, d.($p), 1, 0)
        end
        function cquantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Int32, Int32),
                  p, d.($p), 0, 0)
        end
        function invlogcdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Int32, Int32),
                  lp, d.($p), 1, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath,  $qq),
                  Float64, (Float64, Float64, Int32, Int32),
                  lp, d.($p), 0, 1)
        end
        if $dc
            function rand(d::($T))
                int(ccall(dlsym(_jl_libRmath,  $rr), Float64, (Float64,), d.($p)))
            end
        else
            function rand(d::($T))
                ccall(dlsym(_jl_libRmath,  $rr), Float64, (Float64,), d.($p))
            end
        end
    end
end

macro _jl_dist_2p(T, b)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    Ty = eval(T)
    dc = Ty <: DiscreteDistribution
    pf = dc ? :pmf : :pdf
    lf = dc ? :logpmf : :logpdf
    pn = Ty.names                       # parameter names
    p1 = expr(:quote,pn[1])
    p2 = expr(:quote,pn[2])    
    if string(b) == "norm"              # normal dist has unusual names
        dd = expr(:quote, :dnorm4)
        pp = expr(:quote, :pnorm5)
        qq = expr(:quote, :qnorm5)
    end
    quote
        global $pf,$lf,cdf,logcdf,ccdf,logccdf,quantile,cquantile,invlogcdf,invlogccdf,rand
        function ($pf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Float64, Int32),
                  x, d.($p1), d.($p2), 0)
        end
        function ($lf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Float64, Int32),
                  x, d.($p1), d.($p2), 1)
        end
        function cdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), 1, 0)
        end
        function logcdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath,  $pp),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), 1, 1)
        end
        function ccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), 0, 0)
        end
        function logccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), 0, 1)
        end
        function quantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  p, d.($p1), d.($p2), 1, 0)
        end
        function cquantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  p, d.($p1), d.($p2), 0, 0)
        end
        function invlogcdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), 1, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), 0, 1)
        end
        if $dc
            function rand(d::($T))
                int(ccall(dlsym(_jl_libRmath,  $rr), Float64,
                          (Float64,Float64), d.($p1), d.($p2)))
            end
        else
            function rand(d::($T))
                ccall(dlsym(_jl_libRmath,  $rr), Float64,
                      (Float64,Float64), d.($p1), d.($p2))
            end
        end
    end
end

macro _jl_dist_3p(T, b)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    Ty = eval(T)
    dc = Ty <: DiscreteDistribution
    pf = dc ? :pmf : :pdf
    lf = dc ? :logpmf : :logpdf
    pn = Ty.names                       # parameter names
    p1 = expr(:quote,pn[1])
    p2 = expr(:quote,pn[2])    
    p3 = expr(:quote,pn[3])
    quote
        global $pf,$lf,cdf,logcdf,ccdf,logccdf,quantile,cquantile,invlogcdf,invlogccdf,rand
        function ($pf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Float64, Float64, Int32),
                  x, d.($p1), d.($p2), d.($p3), 0)
        end
        function ($lf)(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Float64, Float64, Int32),
                  x, d.($p1), d.($p2), d.($p3), 1)
        end
        function cdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), d.($p3), 1, 0)
        end
        function logcdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath,  $pp),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), d.($p3), 1, 1)
        end
        function ccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), d.($p3), 0, 0)
        end
        function logccdf(d::($T), q::Real)
            ccall(dlsym(_jl_libRmath, $pp),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  q, d.($p1), d.($p2), d.($p3), 0, 1)
        end
        function quantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  p, d.($p1), d.($p2), d.($p3), 1, 0)
        end
        function cquantile(d::($T), p::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  p, d.($p1), d.($p2), d.($p3), 0, 0)
        end
        function invlogcdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath, $qq),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), d.($p3), 1, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath,  $qq),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), d.($p3), 0, 1)
        end
        if $dc
            function rand(d::($T))
                int(ccall(dlsym(_jl_libRmath,  $rr), Float64,
                          (Float64,Float64,Float64), d.($p1), d.($p2), d.($p3)))
            end
        else
            function rand(d::($T))
                ccall(dlsym(_jl_libRmath,  $rr), Float64,
                      (Float64,Float64,Float64), d.($p1), d.($p2), d.($p3))
            end
        end
    end
end

type Alpha <: ContinuousDistribution
    location::Float64
    scale::Float64
    Alpha(l, s) = s < 0 ? error("scale must be non-negative") : new(float64(l), float64(s))
end
const Levy = Alpha

type Arcsine <: ContinuousDistribution
end

type Bernoulli <: DiscreteDistribution
    prob::Float64
    Bernoulli(p) = 0. <= p <= 1. ? new(float64(p)) : error("prob must be in [0,1]")
end
Bernoulli() = Bernoulli(0.5)

cdf(d::Bernoulli, q::Real) = q < 0. ? 0. : (q >= 1. ? 1. : 1. - d.prob)
insupport(d::Bernoulli, x::Number) = (x == 0) || (x == 1)
kurtosis(d::Bernoulli) = 1/var(d) - 6
logpmf( d::Bernoulli, mu::Real, y::Real) = y==0? log(1. - mu): (y==1? log(mu): -Inf)
mean(d::Bernoulli) = d.prob
mustart(d::Bernoulli,  y::Real, wt::Real) = (wt * y + 0.5)/(wt + 1)
pmf(d::Bernoulli, x::Real) = x == 0 ? (1 - d.prob) : (x == 1 ? d.prob : 0)
quantile(d::Bernoulli, p::Real) = 0 < p < 1 ? (p <= (1. - d.prob) ? 0 : 1) : NaN
rand(d::Bernoulli) = rand() > d.prob ? 0 : 1
skewness(d::Bernoulli) = (1-2d.prob)/std(d)
var(d::Bernoulli, mu::Real) = max(eps(), mu*(1. - mu))
var(d::Bernoulli) = d.prob * (1. - d.prob)

type Beta <: ContinuousDistribution
    alpha::Float64
    beta::Float64
    Beta(a, b) = a > 0 && b > 0 ? new(float64(a), float64(b)) : error("Both alpha and beta must be positive")
end
Beta(a) = Beta(a, a)                    # symmetric in [0,1]
Beta()  = Beta(1)                       # uniform
@_jl_dist_2p Beta beta
mean(d::Beta) = d.alpha / (d.alpha + d.beta)
var(d::Beta) = (ab = d.alpha + d.beta; d.alpha * d.beta /(ab * ab * (ab + 1.)))
skewness(d::Beta) = 2(d.beta - d.alpha)*sqrt(d.alpha + d.beta + 1)/((d.alpha + d.beta + 2)*sqrt(d.alpha*d.beta))
rand(d::Beta) = randbeta(d.alpha, d.beta)
rand!(d::Beta, A::Array{Float64}) = randbeta!(alpha, beta, A)
insupport(d::Beta, x::Number) = real_valued(x) && 0 < x < 1

type BetaPrime <: ContinuousDistribution
    alpha::Float64
    beta::Float64
end

type Binomial <: DiscreteDistribution
    size::Int
    prob::Float64
    Binomial(n, p) = n <= 0 ?  error("size must be positive") : (0. <= p <= 1. ? new(int(n), float64(p)) : error("prob must be in [0,1]"))
end
Binomial(size) = Binomial(size, 0.5)
Binomial()     = Binomial(1, 0.5)
@_jl_dist_2p Binomial binom
mean(d::Binomial)     = d.size * d.prob
var(d::Binomial)      = d.size * d.prob * (1. - d.prob)
skewness(d::Binomial) = (1-2d.prob)/std(d)
kurtosis(d::Binomial) = (1-2d.prob*(1-d.prob))/var(d)
insupport(d::Binomial, x::Number) = integer_valued(x) && 0 <= x <= d.size

type Cauchy <: ContinuousDistribution
    location::Real
    scale::Real
    Cauchy(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end
Cauchy(l) = Cauchy(l, 1)
Cauchy()  = Cauchy(0, 1)
@_jl_dist_2p Cauchy cauchy
mean(d::Cauchy)     = NaN
var(d::Cauchy)      = NaN
skewness(d::Cauchy) = NaN
kurtosis(d::Cauchy) = NaN
insupport(d::Cauchy, x::Number) = real_valued(x) && isfinite(x)

type Chi <: ContinuousDistribution
    df::Float64
end

type Chisq <: ContinuousDistribution
    df::Float64      # non-integer degrees of freedom are meaningful
    Chisq(d) = d > 0 ? new(float64(d)) : error("df must be positive")
end
@_jl_dist_1p Chisq chisq
mean(d::Chisq)     = d.df
var(d::Chisq)      = 2d.df
skewness(d::Chisq) = sqrt(8/d.df)
kurtosis(d::Chisq) = 12/d.df
rand(d::Chisq)     = randchi2(d.df)
rand!(d::Chisq, A::Array{Float64}) = randchi2!(d.df, A)
insupport(d::Chisq, x::Number) =  real_valued(x) && isfinite(x) && 0 <= x

type Erlang <: ContinuousDistribution
    shape::Float64
    rate::Float64
end

type Exponential <: ContinuousDistribution
    scale::Float64                      # note: scale not rate
    Exponential(sc) = sc > 0 ? new(float64(sc)) : error("scale must be positive")
end
Exponential() = Exponential(1.)
mean(d::Exponential)     = d.scale
median(d::Exponential)   = d.scale * log(2.)
var(d::Exponential)      = d.scale * d.scale
skewness(d::Exponential) = 2.
kurtosis(d::Exponential) = 6.
function cdf(d::Exponential, q::Real)
    q <= 0. ? 0. : -expm1(-q/d.scale)
end
function logcdf(d::Exponential, q::Real)
    q <= 0. ? -Inf : (qs = -q/d.scale; qs > log(0.5) ? log(-expm1(qs)) : log1p(-exp(qs)))
end
function ccdf(d::Exponential, q::Real)
    q <= 0. ? 1. : exp(-q/d.scale)
end
function logccdf(d::Exponential, q::Real)
    q <= 0. ? 0. : -q/d.scale
end
function pdf(d::Exponential, x::Real)
    x <= 0. ? 0. : exp(-x/d.scale) / d.scale
end
function logpdf(d::Exponential, x::Real)
    x <= 0. ? -Inf : (-x/d.scale) - log(d.scale)
end
function quantile(d::Exponential, p::Real)
    0. <= p <= 1. ? -d.scale * log1p(-p) : NaN
end
function invlogcdf(d::Exponential, lp::Real)
    lp <= 0. ? -d.scale * (lp > log(0.5) ? log(-expm1(lp)) : log1p(-exp(lp))) : NaN
end
function cquantile(d::Exponential, p::Real)
    0. <= p <= 1. ? -d.scale * log(p) : NaN
end
function invlogccdf(d::Exponential, lp::Real)
    lp <= 0. ? -d.scale * lp : NaN
end
rand(d::Exponential)                     = d.scale * randexp()
rand!(d::Exponential, A::Array{Float64}) = d.scale * randexp!(A)
insupport(d::Exponential, x::Number) = real_valued(x) && isfinite(x) && 0 <= x

type FDist <: ContinuousDistribution
    ndf::Float64
    ddf::Float64
    FDist(d1,d2) = d1 > 0 && d2 > 0 ? new(float64(d1), float64(d2)) : error("Both numerator and denominator degrees of freedom must be positive")
end
@_jl_dist_2p FDist f
mean(d::FDist) = 2 < d.ddf ? d.ddf/(d.ddf - 2) : NaN
var(d::FDist)  = 4 < d.ddf ? 2d.ddf^2*(d.ndf+d.ddf-2)/(d.ndf*(d.ddf-2)^2*(d.ddf-4)) : NaN
insupport(d::FDist, x::Number) = real_valued(x) && isfinite(x) && 0 <= x

type Gamma <: ContinuousDistribution
    shape::Float64
    scale::Float64
    Gamma(sh,sc) = sh > 0 && sc > 0 ? new(float64(sh), float64(sc)) : error("Both schape and scale must be positive")
end
Gamma(sh) = Gamma(sh, 1.)
Gamma()   = Gamma(1., 1.)               # Standard exponential distribution
@_jl_dist_2p Gamma gamma
mean(d::Gamma)     = d.shape * d.scale
var(d::Gamma)      = d.shape * d.scale * d.scale
skewness(d::Gamma) = 2/sqrt(d.shape)
rand(d::Gamma)     = d.scale * randg(d.shape)
rand!(d::Gamma, A::Array{Float64}) = d.scale * randg!(d.shape, A)
insupport(d::Gamma, x::Number) = real_valued(x) && isfinite(x) && 0 <= x

type Geometric <: DiscreteDistribution
    # In the form of # of failures before the first success
    prob::Float64
    Geometric(p) = 0 < p < 1 ? new(float64(p)) : error("prob must be in (0,1)")
end
Geometric() = Geometric(0.5)            # Flips of a fair coin
@_jl_dist_1p Geometric geom
mean(d::Geometric)     = (1-d.prob)/d.prob
var(d::Geometric)      = (1-d.prob)/d.prob^2
skewness(d::Geometric) = (2-d.prob)/sqrt(1-d.prob)
kurtosis(d::Geometric) = 6+d.prob^2/(1-d.prob)
function cdf(d::Geometric, q::Real)
    q < 0. ? 0. : -expm1(log1p(-d.prob) * (floor(q) + 1.))
end
function ccdf(d::Geometric, q::Real)
    q < 0. ? 1. : exp(log1p(-d.prob) * (floor(q + 1e-7) + 1.))
end
insupport(d::Geometric, x::Number) = integer_valued(x) && 0 <= x

type HyperGeometric <: DiscreteDistribution
    ns::Float64                         # number of successes in population
    nf::Float64                         # number of failures in population
    n::Float64                          # sample size
    function HyperGeometric(s,f,n)
        s = 0 <= s && int(s) == s ? int(s) : error("ns must be a non-negative integer")
        f = 0 <= f && int(f) == f ? int(f) : error("nf must be a non-negative integer")        
        n = 0 < n <= (s+f) && int(n) == n ? new(float64(s), float64(f), float64(n)) : error("n must be a positive integer <= (ns + nf)")
    end
end
@_jl_dist_3p HyperGeometric hyper
mean(d::HyperGeometric) = d.n*d.ns/(d.ns+d.nf)
var(d::HyperGeometric)  = (N=d.ns+d.nf; p=d.ns/N; d.n*p*(1-p)*(N-d.n)/(N-1))
insupport(d::HyperGeometric, x::Number) = integer_valued(x) && 0 <= x <= d.n && (d.n - d.nf) <= x <= d.ns

type Logistic <: ContinuousDistribution
    location::Real
    scale::Real
    Logistic(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end
Logistic(l) = Logistic(l, 1)
Logistic()  = Logistic(0, 1)
@_jl_dist_2p Logistic logis
mean(d::Logistic)     = d.location
median(d::Logistic)   = d.location
var(d::Logistic)      = (pi*d.scale)^2/3.
std(d::Logistic)      = pi*d.scale/sqrt(3.)
skewness(d::Logistic) = 0.
kurtosis(d::Logistic) = 1.2
isupport(d::Logistic, x::Number) = real_valued(x) && isfinite(x)

type logNormal <: ContinuousDistribution
    meanlog::Float64
    sdlog::Float64
    logNormal(ml,sdl) = sdl > 0 ? new(float64(ml), float64(sdl)) : error("sdlog must be positive")
end
logNormal(ml) = logNormal(ml, 1)
logNormal()   = logNormal(0, 1)
@_jl_dist_2p logNormal lnorm
mean(d::logNormal) = exp(d.meanlog + d.sdlog^2/2)
var(d::logNormal)  = (sigsq=d.sdlog^2; (exp(sigsq) - 1)*exp(2d.meanlog+sigsq))
insupport(d::logNormal, x::Number) = real_valued(x) && isfinite(x) && 0 < x


## NegativeBinomial is the distribution of the number of failures
## before the size'th success in a sequence of Bernoulli trials.
## We do not enforce integer size, as the distribution is well defined
## for non-integers, and this can be useful for e.g. overdispersed
## discrete survival times.
type NegativeBinomial <: DiscreteDistribution
    size::Float64
    prob::Float64
    NegativeBinomial(s,p) = 0 < p <= 1 ? (s >= 0 ? new(float64(s),float64(p)) : error("size must be non-negative")) : error("prob must be in (0,1]")
end
@_jl_dist_2p NegativeBinomial nbinom
insupport(d::NegativeBinomial, x::Number) = integer_valued(x) && 0 <= x

type NoncentralBeta <: ContinuousDistribution
    alpha::Float64
    beta::Float64
    ncp::Float64
    NonCentralBeta(a,b,nc) = a > 0 && b > 0 && nc >= 0 ? new(float64(a),float64(b),float64(nc)) : error("alpha and beta must be > 0 and ncp >= 0")
end
@_jl_dist_3p NoncentralBeta nbeta

type NoncentralChisq <: ContinuousDistribution
    df::Float64
    ncp::Float64
    NonCentralChisq(d,nc) = d >= 0 && nc >= 0 ? new(float64(d),float64(nc)) : error("df and ncp must be non-negative")
end
@_jl_dist_2p NoncentralChisq nchisq
insupport(d::NoncentralChisq, x::Number) = real_valued(x) && isfinite(x) && 0 < x

type NoncentralF <: ContinuousDistribution
    ndf::Float64
    ddf::Float64
    ncp::Float64
    NonCentralF(n,d,nc) = n > 0 && d > 0 && nc >= 0 ? new(float64(n),float64(d),float64(nc)) : error("ndf and ddf must be > 0 and ncp >= 0")
end
@_jl_dist_3p NoncentralF nf
insupport(d::logNormal, x::Number) = real_valued(x) && isfinite(x) && 0 <= x

type NoncentralT <: ContinuousDistribution
    df::Float64
    ncp::Float64
    NonCentralT(d,nc) = d >= 0 && nc >= 0 ? new(float64(d),float64(nc)) : error("df and ncp must be non-negative")
end
@_jl_dist_2p NoncentralT nt
insupport(d::NoncentralT, x::Number) = real_valued(x) && isfinite(x)

type Normal <: ContinuousDistribution
    mean::Float64
    std::Float64
    Normal(mu, sd) = sd > 0 ? new(float64(mu), float64(sd)) : error("std must be positive")
end
Normal(mu) = Normal(mu, 1)
Normal() = Normal(0,1)
const Gaussian = Normal
@_jl_dist_2p Normal norm
mean(d::Normal) = d.mean
median(d::Normal) = d.mean
var(d::Normal) = d.std^2
skewness(d::Normal) = 0.
kurtosis(d::Normal) = 0.
## redefine common methods
cdf(d::Normal, x::Real) = (1+erf((x-d.mean)/(d.std*sqrt(2))))/2
pdf(d::Normal, x::Real) = exp(-(x-d.mean)^2/(2d.std^2))/(d.std*sqrt(2pi))
rand(d::Normal) = d.mean + d.std * randn()
insupport(d::Normal, x::Number) = real_valued(x) && isfinite(x)

type Poisson <: DiscreteDistribution
    lambda::Float64
    Poisson(l) = l > 0 ? new(float64(l)) : error("lambda must be positive")
end
Poisson() = Poisson(1)
@_jl_dist_1p Poisson pois
devresid(d::Poisson,  y::Real, mu::Real, wt::Real) = 2wt*((y==0? 0.: log(y/mu)) - (y-mu))
insupport(d::Poisson, x::Number) = integer_valued(x) && 0 <= x
logpmf(  d::Poisson, mu::Real, y::Real) = ccall(dlsym(_jl_libRmath,:dpois),Float64,(Float64,Float64,Int32),y,mu,1)
mean(d::Poisson) = d.lambda
mustart( d::Poisson,  y::Real, wt::Real) = y + 0.1
var(     d::Poisson, mu::Real) = mu
var(d::Poisson) = d.lambda

type TDist <: ContinuousDistribution
    df::Float64                         # non-integer degrees of freedom allowed
    TDist(d) = d > 0 ? new(float64(d)) : error("df must be positive")
end
@_jl_dist_1p TDist t
mean(d::TDist) = d.df > 1 ? 0. : NaN
median(d::TDist) = 0.
var(d::TDist) = d.df > 2 ? d.df/(d.df-2) : d.df > 1 ? Inf : NaN
insupport(d::TDist, x::Number) = real_valued(x) && isfinite(x)

type Uniform <: ContinuousDistribution
    a::Float64
    b::Float64
    Uniform(a, b) = a < b ? new(float64(a), float64(b)) : error("a < b required for range [a, b]")
end
Uniform() = Uniform(0, 1)
@_jl_dist_2p Uniform unif
mean(d::Uniform) = (d.a + d.b) / 2.
median(d::Uniform) = (d.a + d.b)/2.
rand(d::Uniform) = d.a + (d.b - d.a) * rand()
var(d::Uniform) = (w = d.b - d.a; w * w / 12.)
insupport(d::Uniform, x::Number) = real_valued(x) && d.a <= x <= d.b

type Weibull <: ContinuousDistribution
    shape::Float64
    scale::Float64
    Weibull(sh,sc) = 0 < sh && 0 < sc ? new(float64(sh), float64(sc)) : error("Both shape and scale must be positive")
end
Weibull(sh) = Weibull(sh, 1)
@_jl_dist_2p Weibull weibull
mean(d::Weibull) = d.scale * gamma(1 + 1/d.shape)
var(d::Weibull) = d.scale^2*gamma(1 + 2/d.shape) - mean(d)^2
cdf(d::Weibull, x::Real) = 0 < x ? 1. - exp(-((x/d.scale)^d.shape)) : 0.
insupport(d::Weibull, x::Number) = real_valued(x) && isfinite(x) && 0 <= x

for f in (:cdf, :logcdf, :ccdf, :logccdf, :quantile, :cquantile, :invlogcdf, :invlogccdf)
    @eval begin
        function ($f){T<:Real}(d::Distribution, x::AbstractArray{T})
            reshape([($f)(d, e) for e in x], size(x))
        end
    end
end
for f in (:pmf, :logpmf)
    @eval begin
        function ($f){T<:Real}(d::DiscreteDistribution, x::AbstractArray{T})
            reshape([($f)(d, e) for e in x], size(x))
        end
    end
end
for f in (:pdf, :logpdf)
    @eval begin
        function ($f){T<:Real}(d::ContinuousDistribution, x::AbstractArray{T})
            reshape([($f)(d, e) for e in x], size(x))
        end
    end
end

## Distributions contributed by John Myles White.

type Multinomial <: DiscreteDistribution
    n::Int
    prob::Vector{Float64}
    function Multinomial(n::Integer, p::Vector{Float64})
        if n <= 0 error("Multinomial: n must be positive") end
        sump = 0.
        for i in 1:numel(p)
            if p[i] < 0. error("Multinomial: probabilities must be non-negative") end
            sump += p[i]
        end
        ## if abs(sump - 1.) > sqrt(eps())   # allow a bit of slack
        ##     error("Multinomial: probabilities must add to 1")
        ## end
        new(int(n), p ./ sump)
    end      
end

function Multinomial(n::Integer, d::Integer)
    if d <= 1  error("d must be greater than 1") end
    Multinomial(n, ones(Float64, d)./float64(d))
end

Multinomial(d::Integer) = Multinomial(1, d)

function Multinomial(n::Integer, p::Matrix{Float64})
    if !(size(p, 1) == 1 || size(p, 2) == 1)
        error("Probability matrix must be a single row or single column")
    end
    Multinomial(int(n), reshape(p, (numel(p),)))
end

mean(d::Multinomial) = d.n .* d.prob
var(d::Multinomial)  = d.n .* d.prob .* (1 - d.prob)
                                        # convenience methods for integer_valued
integer_valued{T<:Integer}(x::AbstractArray{T}) = true
function integer_valued{T<:Number}(x::AbstractArray{T})
    for el in x if !integer_valued(el) return false end end
    true
end

insupport{T <: Real}(d::Multinomial, x::Vector{T}) = integer_valued(x) && all(x .>= 0) && sum(x) == d.n && numel(d.prob) == numel(x)

# log_factorial(n::Int64) = sum(log(1:n)) # lgamma(n + 1) is often much faster

function logpmf{T <: Real}(d::Multinomial, x::Array{T, 1})
  !insupport(d, x) ? -Inf : lgamma(d.n + 1) - sum(lgamma(x + 1)) + sum(x .* log(d.prob))
end

pmf{T <: Real}(d::Multinomial, x::Vector{T}) = exp(logpmf(d, x))

function rand(d::Multinomial)
  n = d.n
  l = numel(d.prob)
  s = zeros(Int, l)
  psum = 1.0
  for j = 1:(l - 1)
    s[j] = int(ccall(dlsym(_jl_libRmath, "rbinom"), Float64, (Float64, Float64), n, d.prob[j] / psum))
    n -= s[j]
    if n == 0
      break
    end
    psum -= d.prob[j]
  end
  s[end] = n
  s
end

rand(d::Multinomial, count::Int) = rand(d, (numel(d.prob), count))

function rand!(d::Multinomial, A::Matrix{Int})
  n = size(A, 2)
  for i = 1:n
    A[:, i] = rand(d)
  end
  A
end

type Dirichlet <: ContinuousDistribution
    alpha::Vector{Float64}
    function Dirichlet{T<:Real}(alpha::Vector{T})
        for el in alpha
            if el < 0 error("Dirichlet: elements of alpha must be non-negative") end
        end
        new(float64(alpha))
    end
end

Dirichlet(dim::Int) = Dirichlet(ones(dim))

function Dirichlet(alpha::Matrix{Float64})
    if !(size(alpha, 1) == 1 || size(alpha, 2) == 1)
        error("2D concentration parameters must come in a 1xN or Nx1 array")
    end
    Dirichlet(reshape(alpha, (numel(alpha),)))
end

mean(d::Dirichlet) = d.alpha ./ sum(d.alpha)
function var(d::Dirichlet)
    alpha0 = sum(d.alpha)
    d.alpha .* (alpha0 - d.alpha) / (alpha0^2 * (alpha0 + 1))
end

                                        # perhaps allow a bit of fuzz on sum(x) == 1?
insupport{T<:Real}(d::Dirichlet, x::Vector{T}) =
    numel(d.alpha) == numel(x) && all(x .>= 0.) && sum(x) == 1.
    # removed the redundant real_valued check

function pdf{T <: Real}(d::Dirichlet, x::Array{T,1})
  if !insupport(d, x)
    error("x not in the support of Dirichlet distribution")
  end
  b = prod(gamma(d.alpha)) / gamma(sum(d.alpha))
  (1 / b) * prod(x.^(d.alpha - 1))
end

# Idea adapted from R's MCMCpack Dirichlet sampler.
function rand(d::Dirichlet)
    x = [randg(el) for el in d.alpha]
    x ./ sum(x)
end

function rand!(d::Dirichlet, A::Array{Float64,2})
  for i in 1:size(A, 1)
    A[i, :] = rand(d)'
  end
end

# Categorical distribution
type Categorical <: DiscreteDistribution
    prob::Vector{Float64}
    function Categorical(p::Vector{Float64})
        if length(p) <= 1 error("Categorical: there must be at least two categories") end
        sump = 0.
        for i in 1:numel(p)
            if p[i] < 0. error("Categorical: probabilities must be non-negative") end
            sump += p[i]
        end
#        if abs(sump - 1.) > sqrt(eps())   # allow a bit of slack
#            error("Categorical: probabilities must add to 1")
#        end
        new(p ./ sump)
    end
end

function Categorical(d::Integer)
    if d <= 1 error("d must be greater than 1") end
    Categorical(ones(Float64, d) ./ float64(d))
end

function Categorical(p::Matrix{Float64})
    if !(size(p, 1) == 1 || size(p, 2) == 1)
        error("Probability matrix must be a single row or single column")
    end
    Categorical(reshape(p, (numel(p),)))
end

insupport(d::Categorical, x::Int) = 1 <= x <= length(d.prob) && d.prob[x] != 0.0

function logpmf(d::Categorical, x::Int)
  !insupport(d, x) ? -Inf : log(d.prob[x])
end

pmf(d::Categorical, x::Int) = exp(logpmf(d, x))

function rand(d::Categorical)
  l = numel(d.prob)
  r = rand()
  for j = 1:l
    r -= d.prob[j]
    if r <= 0.0
      return j
    end
  end
  return l
end

## Why is this needed?  There is already such a method for DiscreteDistribution
## function rand!{T<:Integer}(d::Categorical, A::Vector{T})
##   for i = 1:length(A)
##     A[i] = rand(d)
##   end
## end

function sample{T<:Real}(a::AbstractVector, probs::Vector{T})
  i = rand(Categorical(probs))
  a[i]
end

function sample(a::Array)
  n = numel(a)
  probs = ones(n) ./ n
  sample(a, probs)
end

end  #module
