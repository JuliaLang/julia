## Interface to the Rmath library ##
Rmath = dlopen("libRmath")

## An version of pow for R (probably not necessary)
## returns x^y 
function R_pow(x::Number, y::Number)
    if isa(y, Integer)
        return ccall(dlsym(Rmath, :R_pow_di), Float64, (Float64, Int32), x, y)
    end
    ccall(dlsym(Rmath, :R_pow), Float64, (Float64, Float64), x, y)
end

## Density of normal (Gaussian) distribution
function dnorm(x::Number, mu::Number, sigma::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, mu, sigma, give_log)
end

function dnorm(x::Number, mu::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, mu, 1., give_log)
end

function dnorm(x::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, 0., 1., give_log)
end

function dnorm(x::Number, mu::Number, sigma::Number)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, mu, sigma, false)
end

function dnorm(x::Number, mu::Number)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, mu, 1., false)
end

function dnorm(x::Number)
    ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x, 0., 1., false)
end

function dnorm(x::Vector{Float64})
    [ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x[i], 0., 1., false) | i=1:size(x,1)]
end

function dnorm(x::Range{Float64})
    [ccall(dlsym(Rmath, :dnorm4), Float64, (Float64, Float64, Float64, Int32), x[i], 0., 1., false) | i=1:size(x,1)]
end

## Cumulative distribution function (cdf) of the normal (Gaussian) distribution
## @argument q - quantile
function pnorm(q::Number, mean::Number, sd::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pnorm5), Float64, (Float64, Float64, Float64, Int32, Int32), q, mean, sd, lower_tail, log_p)
end

## Quantile function of the normal (Gaussian) distribution
## @argument p - probability , must be in (0,1)
function qnorm(p::Number, mean::Number, sd::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qnorm5), Float64, (Float64, Float64, Float64, Int32, Int32), p, mean, sd, lower_tail, log_p)
end

## Density of uniform distribution
## a and b are the upper and lower end-points of the distribution
## R defaults are a=0, b=1, give_log=false
function dunif(x::Number, a::Number, b::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dunif), Float64, (Float64, Float64, Float64, Int32), x, a, b, give_log)
end

## Cumulative distribution function (cdf) of the uniform distribution
## @argument q - quantile
## a and b are the upper and lower end-points of the distribution
## R defaults are a=0, b=1, lower_tail=true, log_p=false
function punif(q::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :punif), Float64, (Float64, Float64, Float64, Int32, Int32), q, a, b, lower_tail, log_p)
end

## Quantile function of the uniform distribution
## @argument p - probability , must be in (0,1)
## a and b are the upper and lower end-points of the distribution
## R defaults are a=0, b=1, lower_tail=true, log_p=false
function qunif(p::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qunif), Float64, (Float64, Float64, Float64, Int32, Int32), p, a, b, lower_tail, log_p)
end

## Density of gamma distribution
## shape and scale are the parameters of the distribution
## R defaults are scale=1, give_log=false
function dgamma(x::Number, shape::Number, scale::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dgamma), Float64, (Float64, Float64, Float64, Int32), x, shape, scale, give_log)
end

## Cumulative distribution function (cdf) of the gamma distribution
## @argument q - quantile
## shape and scale are the parameters of the distribution
## R defaults are scale=1, lower_tail=true, log_p=false
function pgamma(q::Number, shape::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pgamma), Float64, (Float64, Float64, Float64, Int32, Int32), q, shape, scale, lower_tail, log_p)
end

## Quantile function of the gamma distribution
## @argument p - probability , must be in (0,1)
## shape and scale are the parameters of the distribution
## R defaults are scale=1, lower_tail=true, log_p=false
function qgamma(p::Number, shape::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qgamma), Float64, (Float64, Float64, Float64, Int32, Int32), p, shape, scale, lower_tail, log_p)
end

## Density of beta distribution
## shape1 and shape2 are the upper and lower end-points of the distribution
## R defaults are give_log=false
function dbeta(x::Number, shape1::Number, shape2::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dbeta), Float64, (Float64, Float64, Float64, Int32), x, shape1, shape2, give_log)
end

## Cumulative distribution function (cdf) of the beta distribution
## @argument q - quantile
## shape1 and shape2 are the upper and lower end-points of the distribution
## R defaults are lower_tail=true, log_p=false
function pbeta(q::Number, shape1::Number, shape2::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pbeta), Float64, (Float64, Float64, Float64, Int32, Int32), q, shape1, shape2, lower_tail, log_p)
end

## Quantile function of the beta distribution
## @argument p - probability , must be in (0,1)
## shape1 and shape2 are the upper and lower end-points of the distribution
## R defaults are lower_tail=true, log_p=false
function qbeta(p::Number, shape1::Number, shape2::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qbeta), Float64, (Float64, Float64, Float64, Int32, Int32), p, shape1, shape2, lower_tail, log_p)
end

## Density of log-normal distribution
## R defaults are meanlog=0, sdlog=1, give_log=false
function dlnorm(x::Number, meanlog::Number, sdlog::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dlnorm), Float64, (Float64, Float64, Float64, Int32), x, meanlog, sdlog, give_log)
end

## Cumulative distribution function (cdf) of the log-normal distribution
## @argument q - quantile
## R defaults are meanlog=0, sdlog=1, lower_tail=true, log_p=false
function plnorm(q::Number, meanlog::Number, sdlog::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :plnorm), Float64, (Float64, Float64, Float64, Int32, Int32), q, meanlog, sdlog, lower_tail, log_p)
end

## Quantile function of the log-normal distribution
## @argument p - probability , must be in (0,1)
## R defaults are meanlog=0, sdlog=1, lower_tail=true, log_p=false
function qlnorm(p::Number, meanlog::Number, sdlog::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qlnorm), Float64, (Float64, Float64, Float64, Int32, Int32), p, meanlog, sdlog, lower_tail, log_p)
end

## Density of Chi-squared distribution
## R defaults are give_log=false
function dchisq(x::Number, df::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dchisq), Float64, (Float64, Float64, Int32), x, df, give_log)
end

## Cumulative distribution function (cdf) of the Chi-squared distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pchisq(q::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pchisq), Float64, (Float64, Float64, Int32, Int32), q, df, lower_tail, log_p)
end

## Quantile function of the Chi-squared distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qchisq(p::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qchisq), Float64, (Float64, Float64, Int32, Int32), p, df, lower_tail, log_p)
end

## Density of Chi-squared distribution
## R defaults are give_log=false
function dchisq(x::Number, df::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dchisq), Float64, (Float64, Float64, Int32), x, df, give_log)
end

## Cumulative distribution function (cdf) of the Chi-squared distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pchisq(q::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pchisq), Float64, (Float64, Float64, Int32, Int32), q, df, lower_tail, log_p)
end

## Quantile function of the Chi-squared distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qchisq(p::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qchisq), Float64, (Float64, Float64, Int32, Int32), p, df, lower_tail, log_p)
end

## Density of noncentral Chi-squared distribution
## R defaults are give_log=false
function dnchisq(x::Number, df::Number, ncp::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dnchisq), Float64, (Float64, Float64, Float64, Int32), x, df, ncp, give_log)
end

## Cumulative distribution function (cdf) of the noncentral Chi-squared distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pnchisq(q::Number, df::Number, ncp::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pnchisq), Float64, (Float64, Float64, Float64, Int32, Int32), q, df, ncp, lower_tail, log_p)
end

## Quantile function of the noncentral Chi-squared distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qnchisq(p::Number, df::Number, ncp::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qnchisq), Float64, (Float64, Float64, Float64, Int32, Int32), p, df, ncp, lower_tail, log_p)
end

## Density of F distribution
## R defaults are give_log=false
function df(x::Number, df1::Number, df2::Number, give_log::Bool)
    ccall(dlsym(Rmath, :df), Float64, (Float64, Float64, Float64, Int32), x, df1, df2, give_log)
end

## Cumulative distribution function (cdf) of the F distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pf(q::Number, df1::Number, df2::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pf), Float64, (Float64, Float64, Float64, Int32, Int32), q, df1, df2, lower_tail, log_p)
end

## Quantile function of the F distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qf(p::Number, df1::Number, df2::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qf), Float64, (Float64, Float64, Float64, Int32, Int32), p, df1, df2, lower_tail, log_p)
end

## Density of Student's t distribution
## R defaults are give_log=false
function dt(x::Number, df::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dt), Float64, (Float64, Float64, Int32), x, df, give_log)
end

## Cumulative distribution function (cdf) of the Student's t distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pt(q::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pt), Float64, (Float64, Float64, Int32, Int32), q, df, lower_tail, log_p)
end

## Quantile function of the Student's t distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qt(p::Number, df::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qt), Float64, (Float64, Float64, Int32, Int32), p, df, lower_tail, log_p)
end

## Probability mass function of binomial distribution
## R defaults are give_log=false
function dbinom(x::Number, size::Number, prob::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dbinom), Float64, (Float64, Float64, Float64, Int32), x, size, prob, give_log)
end

## Cumulative distribution function (cdf) of the binomial distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pbinom(q::Number, size::Number, prob::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pbinom), Float64, (Float64, Float64, Float64, Int32, Int32), q, size, prob, lower_tail, log_p)
end

## Quantile function of the binomial distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qbinom(p::Number, size::Number, prob::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qbinom), Float64, (Float64, Float64, Float64, Int32, Int32), p, size, prob, lower_tail, log_p)
end

## Density of Cauchy distribution
## R defaults are location=0, scale=1, give_log=false
function dcauchy(x::Number, location::Number, scale::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dcauchy), Float64, (Float64, Float64, Float64, Int32), x, location, scale, give_log)
end

## Cumulative distribution function (cdf) of the Cauchy distribution
## @argument q - quantile
## R defaults are location=0, scale=1, lower_tail=true, log_p=false
function pcauchy(q::Number, location::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pcauchy), Float64, (Float64, Float64, Float64, Int32, Int32), q, location, scale, lower_tail, log_p)
end

## Quantile function of the Cauchy distribution
## @argument p - probability , must be in (0,1)
## R defaults are location=0, scale=1, lower_tail=true, log_p=false
function qcauchy(p::Number, location::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qcauchy), Float64, (Float64, Float64, Float64, Int32, Int32), p, location, scale, lower_tail, log_p)
end

## Density of Weibull distribution
## R defaults are scale=1, give_log=false
function dweibull(x::Number, shape::Number, scale::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dweibull), Float64, (Float64, Float64, Float64, Int32), x, shape, scale, give_log)
end

## Cumulative distribution function (cdf) of the Weibull distribution
## @argument q - quantile
## R defaults are scale=1, lower_tail=true, log_p=false
function pweibull(q::Number, shape::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pweibull), Float64, (Float64, Float64, Float64, Int32, Int32), q, shape, scale, lower_tail, log_p)
end

## Quantile function of the Weibull distribution
## @argument p - probability , must be in (0,1)
## R defaults are scale=1, lower_tail=true, log_p=false
function qweibull(p::Number, shape::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qweibull), Float64, (Float64, Float64, Float64, Int32, Int32), p, shape, scale, lower_tail, log_p)
end

## Density of logistic distribution
## R defaults are location=0, scale=1, give_log=false
function dlogis(x::Number, location::Number, scale::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dlogis), Float64, (Float64, Float64, Float64, Int32), x, location, scale, give_log)
end

## Cumulative distribution function (cdf) of the logistic distribution
## @argument q - quantile
## R defaults are location=0, scale=1, lower_tail=true, log_p=false
function plogis(q::Number, location::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :plogis), Float64, (Float64, Float64, Float64, Int32, Int32), q, location, scale, lower_tail, log_p)
end

## Quantile function of the logistic distribution
## @argument p - probability , must be in (0,1)
## R defaults are location=0, scale=1, lower_tail=true, log_p=false
function qcauchy(p::Number, location::Number, scale::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qcauchy), Float64, (Float64, Float64, Float64, Int32, Int32), p, location, scale, lower_tail, log_p)
end

## Density of exponential distribution
## R defaults are give_log=false
function dexp(x::Number, rate::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dexp), Float64, (Float64, Float64, Int32), x, rate, give_log)
end

## Cumulative distribution function (cdf) of the exponential distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function pexp(q::Number, rate::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :pexp), Float64, (Float64, Float64, Int32, Int32), q, rate, lower_tail, log_p)
end

## Quantile function of the exponential distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qexp(p::Number, rate::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qexp), Float64, (Float64, Float64, Int32, Int32), p, rate, lower_tail, log_p)
end

## Probability mass function of Poisson distribution
## R defaults are give_log=false
function dpois(x::Number, lambda::Number, give_log::Bool)
    ccall(dlsym(Rmath, :dpois), Float64, (Float64, Float64, Int32), x, lambda, give_log)
end

## Cumulative distribution function (cdf) of the Poisson distribution
## @argument q - quantile
## R defaults are lower_tail=true, log_p=false
function ppois(q::Number, lambda::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :ppois), Float64, (Float64, Float64, Int32, Int32), q, lambda, lower_tail, log_p)
end

## Quantile function of the Poisson distribution
## @argument p - probability , must be in (0,1)
## R defaults are lower_tail=true, log_p=false
function qpois(p::Number, lambda::Number, lower_tail::Bool, log_p::Bool)
    ccall(dlsym(Rmath, :qpois), Float64, (Float64, Float64, Int32, Int32), p, lambda, lower_tail, log_p)
end
