## Interface to the Rmath library ##
_jl_libRmath = dlopen("libRmath")

macro _jl_libRmath_vectorize_3arg(f)
    quote
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::T3)
            reshape([ ($f)(x[i], y, z) | i=1:numel(x) ], size(x))
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::AbstractArray{T2}, z::T3)
            reshape([ ($f)(x, y[i], z) | i=1:numel(y) ], size(y))
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::T2, z::AbstractArray{T3})
            reshape([ ($f)(x, y, z[i]) | i=1:numel(z) ], size(z))
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::AbstractArray{T2}, z::T3)
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i], z) | i=1:numel(x) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::AbstractArray{T2}, z::AbstractArray{T3})
            shp = promote_shape(size(y),size(z))
            reshape([ ($f)(x, y[i], z[i]) | i=1:numel(y) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::AbstractArray{T3})
            shp = promote_shape(size(x),size(z))
            reshape([ ($f)(x[i], y, z[i]) | i=1:numel(x) ], shp)
        end
    end
end

## Density of normal (Gaussian) distribution - special case because the dlsym is dnorm4, not dnorm
dnorm(x::Number, mu::Number, sigma::Number, give_log::Bool) =
    ccall(dlsym(_jl_libRmath,:dnorm4),Float64,(Float64,Float64,Float64,Int32), x, mu, sigma, give_log)
dnorm(x::Number, mu::Number, give_log::Bool)                = dnorm(x, mu, 1., give_log)
dnorm(x::Number, give_log::Bool)                            = dnorm(x, 0., 1., give_log)
dnorm(x::Number, mu::Number, sigma::Number)                 = dnorm(x, mu, sigma, false)
dnorm(x::Number, mu::Number)                                = dnorm(x, mu, 1., false)
dnorm(x::Number)                                            = dnorm(x, 0., 1., false)

@vectorize_1arg Number dnorm
@vectorize_2arg Number dnorm
@_jl_libRmath_vectorize_3arg dnorm

## Cumulative distribution function (cdf) of the normal (Gaussian) distribution - special case because the dlsym is pnorm5
## @argument q - quantile
pnorm(q::Number, mean::Number, sd::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, sd, lower_tail, log_p)
pnorm(q::Number, mu::Number, lower_tail::Bool, log_p::Bool) = 
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, 1., lower_tail, log_p)
pnorm(q::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, 0., 1., lower_tail, log_p)
pnorm(q::Number, mean::Number, sd::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, sd, lower_tail, false)
pnorm(q::Number, mu::Number, lower_tail::Bool) = 
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, 1., lower_tail, false)
pnorm(q::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, 0., 1., lower_tail, false)
pnorm(q::Number, mu::Number, sigma::Number) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, sd, true, false)
pnorm(q::Number, mu::Number) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, mean, 1., true, false)
pnorm(q::Number) =
    ccall(dlsym(_jl_libRmath,:pnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), q, 0., 1., true, false)

@vectorize_1arg Number pnorm
@vectorize_2arg Number pnorm
@_jl_libRmath_vectorize_3arg pnorm

## Quantile function of the normal (Gaussian) distribution - special case because the dlsym is qnorm5
## @argument p - probability , must be in (0,1)
qnorm(p::Number, mean::Number, sd::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:qnorm5),Float64,(Float64,Float64,Float64,Int32,Int32), p, mean, sd, lower_tail, log_p)
qnorm(p::Number, mu::Number, lower_tail::Bool, log_p::Bool) = qnorm(p, mu, 1., lower_tail, log_p)
qnorm(p::Number, lower_tail::Bool, log_p::Bool)             = qnorm(p, 0., 1., lower_tail, log_p)
qnorm(p::Number, mu::Number, sigma::Number)                 = qnorm(p, mu, sigma, true, false)
qnorm(p::Number, mu::Number)                                = qnorm(p, mu, 1., true, false)
qnorm(p::Number)                                            = qnorm(p, 0., 1., true, false)

@vectorize_1arg Number qnorm
@vectorize_2arg Number qnorm
@_jl_libRmath_vectorize_3arg qnorm

## Density of uniform distribution - special case because there are no 1-parameter defaults
## a and b are the upper and lower end-points of the non-zero density
## R defaults are a=0, b=1, give_log=false
dunif(x::Number, a::Number, b::Number, give_log::Bool) =
    ccall(dlsym(_jl_libRmath,:dunif),Float64,(Float64,Float64,Float64,Int32), x, a, b, give_log)
dunif(x::Number, give_log::Bool)                            = dunif(x, 0., 1., give_log)
dunif(x::Number, a::Number, b::Number)                      = dunif(x, a, b, false)
dunif(x::Number)                                            = dunif(x, 0., 1., false)

@vectorize_1arg Number dunif
@_jl_libRmath_vectorize_3arg dunif

## Cumulative distribution function (cdf) of the uniform distribution - special case because there are no 1-parameter default cases
## @argument q - quantile
## a and b are the upper and lower end-points of the nonzero density
## R defaults are a=0, b=1, lower_tail=true, log_p=false
punif(q::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:punif),Float64,(Float64,Float64,Float64,Int32,Int32), q, a, b, lower_tail, log_p)
punif(q::Number, lower_tail::Bool, log_p::Bool)             = punif(q, 0., 1., lower_tail, log_p)
punif(q::Number, a::Number, b::Number)                      = punif(q, a, b, true, false)
punif(q::Number)                                            = punif(q, 0., 1., true, false)

@vectorize_1arg Number punif
@_jl_libRmath_vectorize_3arg punif

## Quantile function of the uniform distribution  - special case because there are no 1-parameter default cases
## @argument p - probability , must be in (0,1)
## a and b are the upper and lower end-points of the distribution
## R defaults are a=0, b=1, lower_tail=true, log_p=false
qunif(p::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:qunif),Float64,(Float64,Float64,Float64,Int32,Int32), p, a, b, lower_tail, log_p)
qunif(p::Number, lower_tail::Bool, log_p::Bool)             = qunif(p, 0., 1., lower_tail, log_p)
qunif(p::Number, a::Number, b::Number)                      = qunif(p, a, b, true, false)
qunif(p::Number)                                            = qunif(p, 0., 1., true, false)

@vectorize_1arg Number qunif
@_jl_libRmath_vectorize_3arg qunif

set_seed(a1::Integer, a2::Integer) = ccall(dlsym(_jl_libRmath,:set_seed),Void,(Int32,Int32), a1, a2)

## Density (or probability mass) function for distributions with 1 parameter and no default
macro _jl_libRmathfunc_d_1par_0d(f)
    quote
        ($f)(x::Number, a1::Number, give_log::Bool) = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, a1, give_log)
        ($f)(x::Number, a1::Number) = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, a1, false)
        @vectorize_2arg Number $f
    end
end

## Cumulative distribution function or quantile function for distributions with 1 parameter and no default
macro _jl_libRmathfunc_pq_1par_0d(f)
    quote
        ($f)(x::Number, a1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, lower_tail, log_p)
        ($f)(x::Number, a1::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, lower_tail, false)
        ($f)(x::Number, a1::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, true, false)
        @vectorize_2arg Number $f
    end
end

## Random samples from distributions with 1 parameter and no default
macro _jl_libRmathfunc_r_1par_0d(f)
    quote
        ($f)(nn::Integer, a1::Number) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,),a1)|i=1:nn]
        ($f){T<:Number}(a1::AbstractArray{T,1}) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,),a1[i])|i=1:length(x)]
    end
end

## Density (or probability mass) function for distributions with 1 parameter and a default
macro _jl_libRmathfunc_d_1par(f, d)
    quote
        ($f)(x::Number, a1::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, a1, give_log)
        ($f)(x::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, $d, give_log)
        ($f)(x::Number, a1::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, a1, false)
        ($f)(x::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32), x, $d, false)
        @vectorize_1arg Number $f
        @vectorize_2arg Number $f
    end
end

## Cumulative distribution function or quantile function for distributions with 1 parameter and a default
macro _jl_libRmathfunc_pq_1par(f, d)
    quote
        ($f)(x::Number, a1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, lower_tail, log_p)
        ($f)(x::Number, a1::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, lower_tail, false)
        ($f)(x::Number, a1::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, a1, true, false)
        ($f)(x::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, $d, lower_tail, log_p)
        ($f)(x::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, $d, lower_tail, false)
        ($f)(x::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Int32,Int32), x, $d, true, false)
        @vectorize_2arg Number $f
        @vectorize_1arg Number $f
    end
end

## Density (or probability mass) function for distributions with 2 parameters and no defaults
macro _jl_libRmathfunc_d_2par_0d(f)
    quote
        ($f)(x::Number, p1::Number, p2::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($f)(x::Number, p1::Number, p2::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Cumulative distribution function or quantile function for distributions with 2 parameters and no defaults
macro _jl_libRmathfunc_pq_2par_0d(f)
    quote
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, log_p)
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, false)
        ($f)(x::Number, p1::Number, p2::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, true, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Density (or probability mass) function for distributions with 2 parameters and 1 default
macro _jl_libRmathfunc_d_2par_1d(f, d)
    quote
        ($f)(x::Number, p1::Number, p2::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($f)(x::Number, p1::Number, p2::Number)                 =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, false)
        ($f)(x::Number, p1::Number, give_log::Bool)             =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, $d, give_log)
        ($f)(x::Number, p1::Number)                             =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, $d, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Cumulative distribution function or quantile function for distributions with 2 parameters and 1 default
macro _jl_libRmathfunc_pq_2par_1d(f, d)
    quote
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, log_p)
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool)              =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, false)
        ($f)(x::Number, p1::Number, p2::Number)                                = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, true, false)
        ($f)(x::Number, p1::Number, lower_tail::Bool, log_p::Bool)             = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d, lower_tail, log_p)
        ($f)(x::Number, p1::Number, lower_tail::Bool)                          =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d, lower_tail, false)
        ($f)(x::Number, p1::Number)                                            = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d, true, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Density (or probability mass) function for distributions with 2 parameters and 2 defaults
macro _jl_libRmathfunc_d_2par_2d(f, d1, d2)
    quote
        ($f)(x::Number, p1::Number, p2::Number, give_log::Bool) = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($f)(x::Number, p1::Number, p2::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, false)
        ($f)(x::Number, p1::Number, give_log::Bool) = 
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, $d2, give_log)
        ($f)(x::Number, p1::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, p1, $d2, false)
        ($f)(x::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, $d1, $d2, give_log)
        ($f)(x::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32), x, $d1, $d2, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Cumulative distribution function or quantile function for distributions with 2 parameters and 2 defaults
macro _jl_libRmathfunc_pq_2par_2d(f, d1, d2)
    quote
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, log_p)
        ($f)(x::Number, p1::Number, p2::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, lower_tail, false)
        ($f)(x::Number, p1::Number, p2::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, p2, true, false)
        ($f)(x::Number, p1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d2, lower_tail, log_p)
        ($f)(x::Number, p1::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d2, lower_tail, false)
        ($f)(x::Number, p1::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, p1, $d2, true, false)
        ($f)(x::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, $d1, $d2, lower_tail, log_p)
        ($f)(x::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, $d1, $d2, lower_tail, false)
        ($f)(x::Number) =
            ccall(dlsym(_jl_libRmath,$string(f)), Float64, (Float64,Float64,Float64,Int32,Int32), x, $d1, $d2, true, false)
        @vectorize_2arg Number $f
        @_jl_libRmath_vectorize_3arg $f
    end
end

## Central Chi-squared distribution (df)
@_jl_libRmathfunc_d_1par_0d  dchisq
@_jl_libRmathfunc_pq_1par_0d pchisq
@_jl_libRmathfunc_pq_1par_0d qchisq
@_jl_libRmathfunc_r_1par_0d  rchisq

## Poisson distribution (lambda)
@_jl_libRmathfunc_d_1par_0d  dpois
@_jl_libRmathfunc_pq_1par_0d ppois
@_jl_libRmathfunc_pq_1par_0d qpois
@_jl_libRmathfunc_r_1par_0d  rpois

## Signrank - Wilcoxon Signed Rank statistic
@_jl_libRmathfunc_d_1par_0d  dsignrank
@_jl_libRmathfunc_pq_1par_0d psignrank
@_jl_libRmathfunc_pq_1par_0d qsignrank
@_jl_libRmathfunc_r_1par_0d  rsignrank

## Student's t distribution (df)
@_jl_libRmathfunc_d_1par_0d  dt
@_jl_libRmathfunc_pq_1par_0d pt
@_jl_libRmathfunc_pq_1par_0d qt
@_jl_libRmathfunc_r_1par_0d  rt

## Geometric distribution (prob)
@_jl_libRmathfunc_d_1par_0d  dgeom
@_jl_libRmathfunc_pq_1par_0d pgeom
@_jl_libRmathfunc_pq_1par_0d qgeom
@_jl_libRmathfunc_r_1par_0d  rgeom

## Exponential distribution (rate)
@_jl_libRmathfunc_d_1par     dexp 1
@_jl_libRmathfunc_pq_1par    pexp 1
@_jl_libRmathfunc_pq_1par    qexp 1

## Central F distribution (df1, df2)
@_jl_libRmathfunc_d_2par_0d  df
@_jl_libRmathfunc_pq_2par_0d pf
@_jl_libRmathfunc_pq_2par_0d qf

## Binomial distribution (size, prob)
@_jl_libRmathfunc_d_2par_0d  dbinom
@_jl_libRmathfunc_pq_2par_0d pbinom
@_jl_libRmathfunc_pq_2par_0d qbinom

## Negative binomial distribution (size, prob) - alternative of mu needs to be written in Julia
@_jl_libRmathfunc_d_2par_0d  dnbinom
@_jl_libRmathfunc_pq_2par_0d pnbinom
@_jl_libRmathfunc_pq_2par_0d qnbinom

## Beta distribution (shape1, shape2)
@_jl_libRmathfunc_d_2par_0d  dbeta
@_jl_libRmathfunc_pq_2par_0d pbeta
@_jl_libRmathfunc_pq_2par_0d qbeta

## Noncentral Chi-squared distribution (df, ncp)
@_jl_libRmathfunc_d_2par_0d  dnchisq
@_jl_libRmathfunc_pq_2par_0d pnchisq
@_jl_libRmathfunc_pq_2par_0d qnchisq

## Wilcox - Wilcox's Rank Sum statistic (m, n) - probably only makes sense for positive integers
@_jl_libRmathfunc_d_2par_0d  dwilcox
@_jl_libRmathfunc_pq_2par_0d pwilcox
@_jl_libRmathfunc_pq_2par_0d qwilcox

## Gamma distribution  (shape, scale)
@_jl_libRmathfunc_d_2par_1d  dgamma 1
@_jl_libRmathfunc_pq_2par_1d pgamma 1
@_jl_libRmathfunc_pq_2par_1d qgamma 1

## Weibull distribution (shape, scale)
@_jl_libRmathfunc_d_2par_1d  dweibull 1
@_jl_libRmathfunc_pq_2par_1d pweibull 1
@_jl_libRmathfunc_pq_2par_1d qweibull 1

## Log-normal distribution (meanlog, sdlog)
@_jl_libRmathfunc_d_2par_2d  dlnorm 0 1
@_jl_libRmathfunc_pq_2par_2d plnorm 0 1
@_jl_libRmathfunc_pq_2par_2d qlnorm 0 1

## Logistic distribution (location, scale)
@_jl_libRmathfunc_d_2par_2d  dlogis 0 1
@_jl_libRmathfunc_pq_2par_2d plogis 0 1
@_jl_libRmathfunc_pq_2par_2d qlogis 0 1

## Cauchy distribution (location, scale)
@_jl_libRmathfunc_d_2par_2d  dcauchy 0 1
@_jl_libRmathfunc_pq_2par_2d pcauchy 0 1
@_jl_libRmathfunc_pq_2par_2d qcauchy 0 1

## Not done yet 
## hyper (Hypergeometric, 3pars, no defaults; I'm not sure I even want to think of vectorize_4args)
## nbeta (Noncentral beta, 3pars, no defaults)
## nf (Noncentral f, 3pars, no defaults)
## tukey (Studentized Range Distribution - p and q only - 3pars)

## An version of pow from R (probably not necessary)
## returns x^y 
function R_pow(x::Number, y::Number)
    if isa(y, Integer)
        return ccall(dlsym(_jl_libRmath, :R_pow_di), Float64, (Float64, Int32), x, y)
    end
    ccall(dlsym(_jl_libRmath, :R_pow), Float64, (Float64, Float64), x, y)
end

