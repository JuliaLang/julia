abstract Distribution
abstract DiscreteDistribution   <: Distribution
abstract ContinuousDistribution <: Distribution

_jl_libRmath = dlopen("libRmath")

macro _jl_dist_1p(T, b, p)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    pf = eval(T) <: DiscreteDistribution ? :pmf : :pdf
    lf = eval(T) <: DiscreteDistribution ? :logpmf : :logpdf
    p =  expr(:quote,p)
    quote
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
                  lp, d.($p), 0, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath,  $qq),
                  Float64, (Float64, Float64, Int32, Int32),
                  lp, d.($p), 1, 1)
        end
        function rand(d::($T))
            ccall(dlsym(_jl_libRmath,  $rr),
                  Float64, (Float64,), d.($p))
        end
    end
end

macro _jl_dist_2p(T, b, p1, p2)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    pf = eval(T) <: DiscreteDistribution ? :pmf : :pdf
    lf = eval(T) <: DiscreteDistribution ? :logpmf : :logpdf
    p1 = expr(:quote,p1)
    p2 = expr(:quote,p2)
    if string(b) == "norm"              # normal dist has unusual names
        dd = expr(:quote, :dnorm4)
        pp = expr(:quote, :pnorm5)
        qq = expr(:quote, :qnorm5)
    end
    quote
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
                  lp, d.($p1), d.($p2), 0, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath,  $qq),
                  Float64, (Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), 1, 1)
        end
        function rand(d::($T))
            ccall(dlsym(_jl_libRmath,  $rr),
                  Float64, (Float64, Float64), d.($p1), d.($p2))
        end
    end
end

macro _jl_dist_3p(T, b, p1, p2, p3)
    dd = expr(:quote,strcat("d",b))     # C name for pdf or pmf
    pp = expr(:quote,strcat("p",b))     # C name for cdf
    qq = expr(:quote,strcat("q",b))     # C name for quantile
    rr = expr(:quote,strcat("r",b))     # C name for random sampler
    pf = eval(T) <: DiscreteDistribution ? :pmf : :pdf
    lf = eval(T) <: DiscreteDistribution ? :logpmf : :logpdf
    p1 = expr(:quote,p1)
    p2 = expr(:quote,p2)
    p3 = expr(:quote,p3)
    quote
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
                  lp, d.($p1), d.($p2), d.($p3), 0, 1)
        end
        function invlogccdf(d::($T), lp::Real)
            ccall(dlsym(_jl_libRmath,  $qq),
                  Float64, (Float64, Float64, Float64, Float64, Int32, Int32),
                  lp, d.($p1), d.($p2), d.($p3), 1, 1)
        end
        function rand(d::($T))
            ccall(dlsym(_jl_libRmath,  $rr),
                  Float64, (Float64, Float64, Float64), d.($p1), d.($p2), d.($p3))
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
mean(d::Bernoulli)     = d.prob
variance(d::Bernoulli) = d.prob * (1. - d.prob)

## default methods are those for the Binomial with size = 1
@_jl_dist_2p Bernoulli binom 1 prob

## Redefine common methods
pmf(d::Bernoulli, x::Real) = x == 0 ? (1 - d.prob) : (x == 1 ? d.prob : 0)
cdf(d::Bernoulli, q::Real) = q < 0. ? 0. : (q >= 1. ? 1. : 1. - d.prob)
rand(d::Bernoulli) = rand() > d.prob ? 0 : 1
quantile(d::Bernoulli, p::Real) = 0 < p < 1 ? (p <= (1. - d.prob) ? 0 : 1) : NaN

type Beta <: ContinuousDistribution
    alpha::Float64
    beta::Float64
    Beta(a, b) = a > 0 && b > 0 ? new(float64(a), float64(b)) : error("Both alpha and beta must be positive")
end
Beta(a) = Beta(a, a)                    # symmetric in [0,1]
Beta()  = Beta(1)                       # uniform
mean(d::Beta) = d.alpha / (d.alpha + d.beta)
variance(d::Beta) = (ab = d.alpha + d.beta; d.alpha * d.beta /(ab * ab * (ab + 1.)))
@_jl_dist_2p Beta beta alpha beta

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
mean(d::Binomial)     = d.size * d.prob
variance(d::Binomial) = d.size * d.prob * (1. - d.prob)
@_jl_dist_2p Binomial binom size prob

type Cauchy <: ContinuousDistribution
    location::Real
    scale::Real
    Cauchy(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end
Cauchy(l) = Cauchy(l, 1)
Cauchy()  = Cauchy(0, 1)
mean(d::Cauchy) = NaN
variance(d::Cauchy) = NaN
@_jl_dist_2p Cauchy cauchy location scale

type Chi <: ContinuousDistribution
    df::Float64
end

type Chisq <: ContinuousDistribution
    df::Float64      # non-integer degrees of freedom are meaningful
    Chisq(d) = d > 0 ? new(float64(d)) : error("df must be positive")
end
@_jl_dist_1p Chisq chisqr df

type Erlang <: ContinuousDistribution
    shape::Float64
    rate::Float64
end

type Exponential <: ContinuousDistribution
    scale::Float64                      # note: scale not rate
    Exponential(sc) = sc > 0 ? new(float64(sc)) : error("scale must be positive")
end
@_jl_dist_1p Exponential exp scale

type FDist <: ContinuousDistribution
    ndf::Float64
    ddf::Float64
    FDist(d1,d2) = d1 > 0 && d2 > 0 ? new(float64(d1), float64(d2)) : error("Both numerator and denominator degrees of freedom must be positive")
end

@_jl_dist_2p FDist f ndf ddf

type Gamma <: ContinuousDistribution
    shape::Float64
    scale::Float64
    Gamma(sh,sc) = sh > 0 && sc > 0 ? new(float64(sh), float64(sc)) : error("Both schape and scale must be positive")
end
Gamma(sh) = Gamma(sh, 1.)
Gamma()   = Gamma(1., 1.)               # Exponential distribution
mean(d::Gamma) = d.shape * d.scale
variance(d::Gamma) = d.shape * d.scale * d.scale

@_jl_dist_2p Gamma gamma shape scale
## redefine the rand method
rand(d::Gamma) = d.scale * randg(d.shape)

type Geometric <: DiscreteDistribution
    prob::Float64
    Geometric(p) = 0 < p < 1 ? new(float64(p)) : error("prob must be in (0,1)")
end
Geometric() = Geometric(0.5)            # Flips of a fair coin
mean(d::Geometric) = 1 / d.prob         # assuming total number of trials - check this
variance(d::Geometric) = (1 - d.prob)/d.prob^2
@_jl_dist_1p Geometric geom prob

type HyperGeometric <: DiscreteDistribution
    ns::Float64                         # number of successes in population
    nf::Float64                         # number of failures in population
    n::Float64                          # sample size
    function HyperGeometric(s,f,n)
        n=int(n)
        s=int(s)
        f=int(f)
        n > 0 && s >= 0 && f >= 0 && n >= (s+f) ? new(float64(s), float64(f), float64(n)) : error("ns, nf and n must be non-negative integers satisfying n >= (ns + nf)")
    end
end
@_jl_dist_3p HyperGeometric hyper ns nf n

type Logistic <: ContinuousDistribution
    location::Real
    scale::Real
    Logistic(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end

@_jl_dist_2p Logistic logis location scale

type logNormal <: ContinuousDistribution
    meanlog::Float64
    sdlog::Float64
    logNormal(ml,sdl) = sdl > 0 ? new(float64(ml), float64(sdl)) : error("sdlog must be positive")
end
logNormal(ml) = logNormal(ml, 1)
logNormal()   = logNormal(0, 1)
@_jl_dist_2p logNormal lnorm meanlog sdlog

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
@_jl_dist_2p NegativeBinomial nbinom size prob

type NoncentralBeta <: ContinuousDistribution
    alpha::Float64
    beta::Float64
    ncp::Float64
    NonCentralBeta(a,b,nc) = a > 0 && b > 0 && nc >= 0 ? new(float64(a),float64(b),float64(nc)) : error("alpha and beta must be > 0 and ncp >= 0")
end
@_jl_dist_3p NoncentralBeta nbeta alpha beta ncp

type NoncentralChisq <: ContinuousDistribution
    df::Float64
    ncp::Float64
    NonCentralChisq(d,nc) = d >= 0 && nc >= 0 ? new(float64(d),float64(nc)) : error("df and ncp must be non-negative")
end
@_jl_dist_2p NoncentralChisq nchisq df ncp

type NoncentralF <: ContinuousDistribution
    ndf::Float64
    ddf::Float64
    ncp::Float64
    NonCentralF(n,d,nc) = n > 0 && d > 0 && nc >= 0 ? new(float64(n),float64(d),float64(nc)) : error("ndf and ddf must be > 0 and ncp >= 0")
end
@_jl_dist_3p NoncentralF nf ndf ddf ncp

type NoncentralT <: ContinuousDistribution
    df::Float64
    ncp::Float64
    NonCentralT(d,nc) = d >= 0 && nc >= 0 ? new(float64(d),float64(nc)) : error("df and ncp must be non-negative")
end
@_jl_dist_2p NoncentralT nt df ncp

type Normal <: ContinuousDistribution
    mean::Float64
    std::Float64
    Normal(mu, sd) = sd > 0 ? new(float64(mu), float64(sd)) : error("std must be positive")
end
Normal(mu) = Normal(mu, 1)
Normal() = Normal(0,1)
const Gaussian = Normal
mean(d::Normal) = d.mean
median(d::Normal) = d.mean
variance(d::Normal) = d.std^2

@_jl_dist_2p Normal norm mean std
## redefine common methods
cdf(d::Normal, x::Real) = (1+erf(x-d.mean)/(d.std*sqrt(2)))/2
pdf(d::Normal, x::Real) = exp(-(x-d.mean)^2/(2d.std^2))/(d.std*sqrt(2pi))
rand(d::Normal) = d.mean + d.std * randn()

type Poisson <: DiscreteDistribution
    lambda::Float64
    Poisson(l) = l > 0 ? new(float64(l)) : error("lambda must be positive")
end
mean(d::Poisson) = d.lambda
variance(d::Poisson) = d.lambda
@_jl_dist_1p Poisson pois lambda

type TDist <: ContinuousDistribution
    df::Float64                         # non-integer degrees of freedom allowed
    TDist(d) = d > 0 ? new(float(d)) : error("df must be positive")
end
@_jl_dist_1p TDist t df
mean(d::TDist) = d.df > 1 ? 0. : NaN
median(d::TDist) = 0.
variance(d::TDist) = d.df > 2 ? d.df/(d.df-2) : d.df > 1 ? Inf : NaN

type Uniform <: ContinuousDistribution
    a::Float64
    b::Float64
    Uniform(aa, bb) = aa > bb ? new(float64(aa), float(bb)) : error("a < b required for range [a, b]")
end
Uniform() = Uniform(0, 1)
@_jl_dist_2p Uniform unif a b
## Specific methods
mean(d::Uniform) = (d.a + d.b) / 2.
median(d::Uniform) = (d.a + d.b)/2.
rand(d::Uniform) = d.a + d.b * rand()   # override the Rmath method
variance(d::Uniform) = (w = d.b - d.a; w * w / 12.)

type Weibull <: ContinuousDistribution
    shape::Float64
    scale::Float64
    Weibull(sh,sc) = 0 < sh && 0 < sc ? new(float64(sh), float(sc)) : error("Both shape and scale must be positive")
end
Weibull(sh) = Weibull(sh, 1)
@_jl_dist_2p Weibull weibull shape scale
## Specific methods
mean(d::Weibull) = d.scale * gamma(1 + 1/d.shape)
variance(d::Weibull) = d.scale^2*gamma(1 + 2/d.shape) - mean(d)^2

for f in (:cdf, :logcdf, :ccdf, :logccdf, :quantile, :cquantile, :invlogcdf, :invlogccdf)
    @eval begin
        function ($f){T<:Real}(d::Distribution, p::AbstractVector{T})
            reshape([($f)(d, e) for e in p], size(x))
        end
    end
end
for f in (:pmf, :logpmf)
    @eval begin
        function ($f){T<:Real}(d::DiscreteDistribution, p::AbstractVector{T})
            reshape([($f)(d, e) for e in p], size(x))
        end
    end
end
for f in (:pdf, :logpdf)
    @eval begin
        function ($f){T<:Real}(d::ContinuousDistribution, p::AbstractVector{T})
            reshape([($f)(d, e) for e in p], size(x))
        end
    end
end
rand(d::Distribution, n::Int) = [rand(d) for i=1:n]
std(d::Distribution) = sqrt(variance(d))
