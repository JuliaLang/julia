abstract Distribution
abstract DiscreteDistribution   <: Distribution
abstract ContinuousDistribution <: Distribution

_jl_libRmath=dlopen("libRmath")

macro _jl_dist_2p_pdf(T, b, p1, p2)
    dd = expr(:quote,strcat("d",b))     # C function names - density or pmf
    pp = expr(:quote,strcat("p",b))     # cdf
    qq = expr(:quote,strcat("q",b))     # quantile
    rr = expr(:quote,strcat("r",b))     # random sampler
    p1 = expr(:quote,p1)
    p2 = expr(:quote,p2)
    if string(b) == "norm"              # normal dist has unusual names
        dd = :dnorm4
        pp = :pnorm5
        qq = :qnorm5
    end
    quote
        function pdf(d::($T), x::Real)
            ccall(dlsym(_jl_libRmath, $dd),
                  Float64, (Float64, Float64, Float64, Int32),
                  x, d.($p1), d.($p2), 0)
        end
        function logpdf(d::($T), x::Real)
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

type Beta <: ContinuousDistribution
    alpha::Float64
    beta::Float64
    Beta(a, b) = a > 0 && b > 0 ? new(float64(a), float64(b)) : error("Both alpha and beta must be positive")
end
Beta(a) = Beta(a, a)                    # symmetric in [0,1]
Beta()  = Beta(1)                       # uniform
mean(d::Beta) = d.alpha / (d.alpha + d.beta)
variance(d::Beta) = (ab = d.alpha + d.beta; d.alpha * d.beta /(ab * ab * (ab + 1.)))
@_jl_dist_2p_pdf Beta beta alpha beta

type Cauchy <: ContinuousDistribution
    location::Real
    scale::Real
    Cauchy(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end
Cauchy(l) = Cauchy(l, 1)
Cauchy()  = Cauchy(0, 1)
mean(d::Cauchy) = NaN
variance(d::Cauchy) = NaN
@_jl_dist_2p_pdf Beta beta alpha beta

type FDist <: ContinuousDistribution
    ndf::Float64
    ddf::Float64
    FDist(d1,d2) = d1 > 0 && d2 > 0 ? new(float64(d1), float64(d2)) : error("Both numerator and denominator degrees of freedom must be positive")
end

@_jl_dist_2p_pdf FDist f ndf ddf

type Gamma <: ContinuousDistribution
    shape::Real
    scale::Real
    Gamma(sh,sc) = sh > 0 && sc > 0 ? new(float64(sh), float64(sc)) : error("Both schape and scale must be positive")
end
Gamma(sh) = Gamma(sh, 1.)
Gamma()   = Gamma(1., 1.)               # Exponential distribution
mean(d::Gamma) = d.shape * d.scale
variance(d::Gamma) = d.shape * d.scale * d.scale

@_jl_dist_2p_pdf Gamma gamma shape scale
## override the rand method
rand(d::Gamma) = d.scale * randg(d.shape)

type Logistic <: ContinuousDistribution
    location::Real
    scale::Real
    Logistic(l, s) = s > 0 ? new(float64(l), float64(s)) : error("scale must be positive")
end

@_jl_dist_2p_pdf Logistic logis location scale

type Normal <: ContinuousDistribution
    mean::Float64
    stddev::Float64
    Normal(mu, sd) = sd < 0 ? error("stddev must be non-negative") : new(float64(mu), float64(sd))
end
Normal(mu) = Normal(mu, 1)
Normal() = Normal(0,1)
const Gaussian = Normal
mean(d::Normal) = d.mean
variance(d::Normal) = d.stddev^2

@_jl_dist_2p_pdf Normal norm mean stddev
## override the rand method
rand(d::Normal) = d.mean + d.stddev * randn()

type Uniform <: ContinuousDistribution
    a::Float64
    b::Float64
    Uniform(aa, bb) = aa > bb ? new(float64(aa), float(bb)) : error("a < b required for range [a, b]")
end
Uniform() = Uniform(0, 1)
mean(d::Uniform) = (d.a + d.b) / 2.
variance(d::Uniform) = (w = d.b - d.a; w * w / 12.)

@_jl_dist_2p_pdf Uniform unif a b
## override the rand method
rand(d::Uniform) = d.a + d.b * rand()

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

variance(d::Bernoulli) = d.prob * (1. - d.prob)
mean(d::Bernoulli)     = d.prob
pmf(d::Bernoulli, x::Real) = x == 0 ? (1. - d.prob) : (x == 1 ? d.prob : 0.)
function logpmf(d::Bernoulli, x::Real)
    ccall(dlsym(_jl_libRmath, :dbinom), Float64, (Float64,Float64,Float64,Int32),
          x, 1., d.prob, 1)
end
cdf(d::Bernoulli, q::Real) = q < 0. ? 0. : (q >= 1. ? 1. : 1. - d.prob)
rand(d::Bernoulli) = rand() > d.prob ? 0 : 1
function quantile(d::Bernoulli, p::Real)
    if !(0 < p < 1) error("argument p must be in [0,1]") end
    p <= (1. - d.prob) ? 0 : 1
end

type BetaPrime <: ContinuousDistribution
    alpha::Float64
    beta::Float64
end

type Binomial <: DiscreteDistribution
    size::Int
    prob::Float64

    Binomial(n, p) = n <= 0 ?  error("size must be positive") : (0. <= p <= 1. ? new(int(n), float64(p)) : error("prob must be in [0,1]"))
end

mean(d::Binomial)     = d.size * d.prob
variance(d::Binomial) = d.size * d.prob * (1. - d.prob)
function rand(d::Binomial)
    int(ccall(dlsym(_jl_libRmath, :rbinom), Float64, (Float64, Float64), d.size, d.prob))
end
function pmf(d::Binomial, x::Real)
    ccall(dlsym(_jl_libRmath, :dbinom), Float64, (Float64,Float64,Float64,Int32),
          x, d.size, d.prob, 0)
end
function logpmf(d::Binomial, x::Real)
    ccall(dlsym(_jl_libRmath, :dbinom), Float64, (Float64,Float64,Float64,Int32),
          x, d.size, d.prob, 1)
end
function cdf(d::Binomial, q::Real)
    ccall(dlsym(_jl_libRmath, :pbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          q, d.size, d.prob, 1, 0)
end
function logcdf(d::Binomial, q::Real)
    ccall(dlsym(_jl_libRmath, :pbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          q, d.size, d.prob, 1, 1)
end
function ccdf(d::Binomial, q::Real)
    ccall(dlsym(_jl_libRmath, :pbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          q, d.size, d.prob, 0, 0)
end
function logccdf(d::Binomial, q::Real)
    ccall(dlsym(_jl_libRmath, :pbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          q, d.size, d.prob, 0, 1)
end
function quantile(d::Binomial, p::Real)
    ccall(dlsym(_jl_libRmath, :qbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          p, d.size, d.prob, 1, 0)
end
function invlogcdf(d::Binomial, lp::Real)
    ccall(dlsym(_jl_libRmath, :qbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          pp, d.size, d.prob, 1, 1)
end
function cquantile(d::Binomial, lp::Real)
    ccall(dlsym(_jl_libRmath, :qbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          pp, d.size, d.prob, 0, 0)
end
function invlogccdf(d::Binomial, lp::Real)
    ccall(dlsym(_jl_libRmath, :qbinom), Float64, (Float64,Float64,Float64,Int32,Int32),
          pp, d.size, d.prob, 0, 1)
end

type Chi <: ContinuousDistribution
    df::Int
end

type Chisq <: ContinuousDistribution
    df::Float64      # non-integer degrees of freedom are meaningful
    Chisq(d) = d <= 0 ? error("df must be positive") : new(float64(d))
end

function pdf(d::Chisq, x::Real)
    ccall(dlsym(_jl_libRmath, :dcauchy), Float64, (Float64, Float64, Float64, Int32),
          x, d.df, 0)
end
function logpdf(d::Chisq, x::Real)
    ccall(dlsym(_jl_libRmath, :dcauchy), Float64, (Float64, Float64, Float64, Int32),
          x, d.df, 1)
end
function cdf(d::Chisq, q::Real)
    ccall(dlsym(_jl_libRmath, :pcauchy), Float64, (Float64, Float64, Int32, Int32),
          q, d.df, 1, 0)
end
function logcdf(d::Chisq, q::Real)
    ccall(dlsym(_jl_libRmath, :pcauchy), Float64, (Float64, Float64, Int32, Int32),
          q, d.df, 1, 1)
end
function ccdf(d::Chisq, q::Real)
    ccall(dlsym(_jl_libRmath, :pcauchy), Float64, (Float64, Float64, Int32, Int32),
          q, d.df, 0, 0)
end
function logccdf(d::Chisq, q::Real)
    ccall(dlsym(_jl_libRmath, :pcauchy), Float64, (Float64, Float64, Int32, Int32),
          q, d.df, 0, 1)
end
function quantile(d::Chisq, p::Real)
    ccall(dlsym(_jl_libRmath, :qcauchy), Float64, (Float64, Float64, Int32, Int32),
          p, d.df, 1, 0)
end
function cquantile(d::Chisq, p::Real)
    ccall(dlsym(_jl_libRmath, :qcauchy), Float64, (Float64, Float64, Int32, Int32),
          p, d.df, 0, 0)
end
function invlogcdf(d::Chisq, lp::Real)
    ccall(dlsym(_jl_libRmath, :qcauchy), Float64, (Float64, Float64, Int32, Int32),
          lp, d.df, 0, 1)
end
function invlogccdf(d::Chisq, lp::Real)
    ccall(dlsym(_jl_libRmath, :qcauchy), Float64, (Float64, Float64, Int32, Int32),
          lp, d.df, 1, 1)
end
function rand(d::Chisq)
    ccall(dlsym(_jl_libRmath, :rcauchy), Float64, (Float64,), d.df)
end

type Erlang <: ContinuousDistribution
    shape::Integer
    rate::Real
end

type Exponential <: ContinuousDistribution
    rate::Real
end

type Laplace <: ContinuousDistribution
    mean::Real
    scale::Real
end

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
stddev(d::Distribution) = sqrt(variance(d))
