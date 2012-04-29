## Interface to the Rmath library ##
_jl_libRmath = dlopen("libRmath")

macro _jl_libRmath_vectorize_3arg(f)
    quote
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::T3) = reshape([ ($f)(x[i], y, z) | i=1:numel(x) ], size(x))
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::AbstractArray{T2}, z::T3) = reshape([ ($f)(x, y[i], z) | i=1:numel(y) ], size(y))
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::T2, z::AbstractArray{T3}) = reshape([ ($f)(x, y, z[i]) | i=1:numel(z) ], size(z))
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
qnorm(p::Number, mu::Number, lower_tail::Bool, log_p::Bool) =
    qnorm(p, mu, 1., lower_tail, log_p)
qnorm(p::Number, lower_tail::Bool, log_p::Bool) =
    qnorm(p, 0., 1., lower_tail, log_p)
qnorm(p::Number, mu::Number, sigma::Number) =
    qnorm(p, mu, sigma, true, false)
qnorm(p::Number, mu::Number) = qnorm(p, mu, 1., true, false)
qnorm(p::Number) = qnorm(p, 0., 1., true, false)

@vectorize_1arg Number qnorm
@vectorize_2arg Number qnorm
@_jl_libRmath_vectorize_3arg qnorm

rnorm(nn::Integer, mu::Number, sigma::Number) =
    [ccall(dlsym(_jl_libRmath,:rnorm), Float64, (Float64,Float64),mu,sigma)|i=1:nn]
rnorm(nn::Integer, mu::Number) =
    [ccall(dlsym(_jl_libRmath,:rnorm), Float64, (Float64,Float64),mu,1.)|i=1:nn]
rnorm(nn::Integer) =
    [ccall(dlsym(_jl_libRmath,:rnorm), Float64, (Float64,Float64),0.,1.)|i=1:nn]

## Density of uniform distribution - special case because there are no 1-parameter defaults
## a and b are the upper and lower end-points of the non-zero density
## R defaults are a=0, b=1, give_log=false
dunif(x::Number, a::Number, b::Number, give_log::Bool) =
    ccall(dlsym(_jl_libRmath,:dunif),Float64,(Float64,Float64,Float64,Int32), x, a, b, give_log)
dunif(x::Number, give_log::Bool) = dunif(x, 0., 1., give_log)
dunif(x::Number, a::Number, b::Number) = dunif(x, a, b, false)
dunif(x::Number) = dunif(x, 0., 1., false)

@vectorize_1arg Number dunif
@_jl_libRmath_vectorize_3arg dunif

## Cumulative distribution function (cdf) of the uniform distribution - special case because there are no 1-parameter default cases
## @argument q - quantile
## a and b are the upper and lower end-points of the nonzero density
## R defaults are a=0, b=1, lower_tail=true, log_p=false
punif(q::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:punif),Float64,(Float64,Float64,Float64,Int32,Int32), q, a, b, lower_tail, log_p)
punif(q::Number, lower_tail::Bool, log_p::Bool) = punif(q, 0., 1., lower_tail, log_p)
punif(q::Number, a::Number, b::Number) = punif(q, a, b, true, false)
punif(q::Number) = punif(q, 0., 1., true, false)

@vectorize_1arg Number punif
@_jl_libRmath_vectorize_3arg punif

## Quantile function of the uniform distribution  - special case because there are no 1-parameter default cases
## @argument p - probability , must be in (0,1)
## a and b are the upper and lower end-points of the distribution
## R defaults are a=0, b=1, lower_tail=true, log_p=false
qunif(p::Number, a::Number, b::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath,:qunif),Float64,(Float64,Float64,Float64,Int32,Int32), p, a, b, lower_tail, log_p)
qunif(p::Number, lower_tail::Bool, log_p::Bool) = qunif(p, 0., 1., lower_tail, log_p)
qunif(p::Number, a::Number, b::Number) = qunif(p, a, b, true, false)
qunif(p::Number) = qunif(p, 0., 1., true, false)

@vectorize_1arg Number qunif
@_jl_libRmath_vectorize_3arg qunif

runif(nn::Integer, a::Number, b::Number) =
    [ccall(dlsym(_jl_libRmath,:runif), Float64, (Float64,Float64),a,b)|i=1:nn]

set_seed(a1::Integer, a2::Integer) =
    ccall(dlsym(_jl_libRmath,:set_seed),Void,(Int32,Int32), a1, a2)

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

## Random samples from distributions with 1 parameter and a default
macro _jl_libRmathfunc_r_1par(f, d)
    quote
        ($f)(nn::Integer, a1::Number) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,),a1)|i=1:nn]
        ($f)(nn::Integer) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,),$d)|i=1:nn]
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

## Random samples from a distribution with 2 parameters and no default
macro _jl_libRmathfunc_r_2par_0d(f)
    quote
        ($f)(nn::Integer, p1::Number, p2::Number) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),p1,p2)|i=1:nn]
#        ($f){T<:Number}(p1::AbstractArray{T,1},) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,),a1[i])|i=1:length(x)]
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

## Random samples from a distribution with 2 parameters and 1 default
macro _jl_libRmathfunc_r_2par_1d(f, d)
    quote
        ($f)(nn::Integer, p1::Number, p2::Number) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),p1,p2)|i=1:nn]
        ($f)(nn::Integer, p1::Number)             = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),p1,$d)|i=1:nn]
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

## Random samples from a distribution with 2 parameters and 2 defaults
macro _jl_libRmathfunc_r_2par_1d(f, d1, d2)
    quote
        ($f)(nn::Integer, p1::Number, p2::Number) = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),p1,p2)|i=1:nn]
        ($f)(nn::Integer, p1::Number)             = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),p1,$d2)|i=1:nn]
        ($f)(nn::Integer)                         = [ccall(dlsym(_jl_libRmath,$string(f)),Float64,(Float64,Float64),$d1,$d2)|i=1:nn]
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

## The d-p-q functions in Rmath allocate storage that must be freed
## Signrank - Wilcoxon Signed Rank statistic
#@_jl_libRmathfunc_d_1par_0d  dsignrank
#@_jl_libRmathfunc_pq_1par_0d psignrank
#@_jl_libRmathfunc_pq_1par_0d qsignrank
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

## May need to handle this as a special case.  The Rmath library uses 1/rate, not rate
## Exponential distribution (rate)
@_jl_libRmathfunc_d_1par     dexp 1
@_jl_libRmathfunc_pq_1par    pexp 1
@_jl_libRmathfunc_pq_1par    qexp 1
@_jl_libRmathfunc_r_1par     rexp 1

## Central F distribution (df1, df2)
@_jl_libRmathfunc_d_2par_0d  df
@_jl_libRmathfunc_pq_2par_0d pf
@_jl_libRmathfunc_pq_2par_0d qf
@_jl_libRmathfunc_r_2par_0d  rf

## Binomial distribution (size, prob)
@_jl_libRmathfunc_d_2par_0d  dbinom
@_jl_libRmathfunc_pq_2par_0d pbinom
@_jl_libRmathfunc_pq_2par_0d qbinom
@_jl_libRmathfunc_r_2par_0d  rbinom

## Negative binomial distribution (size, prob) - alternative of mu needs to be written in Julia
@_jl_libRmathfunc_d_2par_0d  dnbinom
@_jl_libRmathfunc_pq_2par_0d pnbinom
@_jl_libRmathfunc_pq_2par_0d qnbinom
@_jl_libRmathfunc_r_2par_0d  rbinom

## Beta distribution (shape1, shape2)
@_jl_libRmathfunc_d_2par_0d  dbeta
@_jl_libRmathfunc_pq_2par_0d pbeta
@_jl_libRmathfunc_pq_2par_0d qbeta
@_jl_libRmathfunc_r_2par_0d  rbeta

## Noncentral Chi-squared distribution (df, ncp)
@_jl_libRmathfunc_d_2par_0d  dnchisq
@_jl_libRmathfunc_pq_2par_0d pnchisq
@_jl_libRmathfunc_pq_2par_0d qnchisq
@_jl_libRmathfunc_r_2par_0d  rnchisq

## Need to handle the d-p-q separately because the Rmath functions allocate storage that must be freed.
## Wilcox - Wilcox's Rank Sum statistic (m, n) - probably only makes sense for positive integers
#@_jl_libRmathfunc_d_2par_0d  dwilcox
#@_jl_libRmathfunc_pq_2par_0d pwilcox
#@_jl_libRmathfunc_pq_2par_0d qwilcox
@_jl_libRmathfunc_r_2par_0d  rwilcox

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

## tukey (Studentized Range Distribution - p and q only - 3pars)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,log_p)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,false)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,true,false)
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,log_p)|i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,false)|i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([ptukey(q[i],nmeans,df,nranges,true,false)|i=1:numel(q)], size(q))

## tukey (Studentized Range Distribution - p and q only - 3pars)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,log_p)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,false)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,true,false)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,1.,df,true,false)
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,log_p)|i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,false)|i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([ptukey(q[i],nmeans,df,nranges,true,false)|i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number) =
    reshape([ptukey(q[i],nmeans,df,1.,true,false)|i=1:numel(q)], size(q))

qtukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,lower_tail,log_p)
qtukey(p::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,lower_tail,false)
qtukey(p::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,true,false)
qtukey(p::Number, nmeans::Number, df::Number) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,1.,df,true,false)
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    reshape([qtukey(p[i],nmeans,df,nranges,lower_tail,log_p)|i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([qtukey(p[i],nmeans,df,nranges,lower_tail,false)|i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([qtukey(p[i],nmeans,df,nranges,true,false)|i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number) =
    reshape([qtukey(p[i],nmeans,df,1.,true,false)|i=1:numel(p)], size(p))

## Vectorize over four numeric arguments
macro _jl_libRmath_vectorize_4arg(f)
    quote
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::T3, a4::T4) =
            reshape([ ($f)(a1[i], a2, a3, a4) | i=1:numel(a1) ], size(a1))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::T3, a4::T4) =
            reshape([ ($f)(a1, a2[i], a3, a4) | i=1:numel(a2) ], size(a2))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::AbstractArray{T3}, a4::T4) =
            reshape([ ($f)(a1, a2, a3[i], a4) | i=1:numel(a3) ], size(a3))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::T3, a4::AbstractArray{T4}) =
            reshape([ ($f)(a1, a2, a3, a4[i]) | i=1:numel(a4) ], size(a4))
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::T3, a4::T4)
            shp = promote_shape(size(a1), size(a2))
            reshape([ ($f)(a1[i], a2[i], a3, a4) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(size(a1), size(a3))
            reshape([ ($f)(a1[i], a2, a3[i], a4) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(size(a1), size(a4))
            reshape([ ($f)(a1[i], a2, a3, a4[i]) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(size(a2), size(a3))
            reshape([ ($f)(a1, a2[i], a3[i], a4) | i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(size(a2), size(a4))
            reshape([ ($f)(a1, a2[i], a3, a4[i]) | i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(size(a3), size(a4))
            reshape([ ($f)(a1, a2, a3[i], a4[i]) | i=1:numel(a3) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(promote_shape(size(a1), size(a2)), size(a3))
            reshape([ ($f)(a1[i], a2[i], a3[i], a4) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a1), size(a2)), size(a4))
            reshape([ ($f)(a1[i], a2[i], a3, a4[i]) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a1), size(a3)), size(a4))
            reshape([ ($f)(a1[i], a2, a3[i], a4[i]) | i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a2), size(a3)), size(a4))
            reshape([ ($f)(a1, a2[i], a3[i], a4[i]) | i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(promote_shape(size(a1), size(a2)), size(a3)), size(a4))
            reshape([ ($f)(a1[i], a2[i], a3[i], a4[i]) | i=1:numel(a2) ], shp)
        end
    end
end

## Distributions with 3 parameters and no defaults
macro _jl_libRmath_3par_0d(dd, pp, qq, rr)
    quote
        ($dd)(x::Number, p1::Number, p2::Number, p3::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$string(dd)), Float64, (Float64,Float64,Float64,Float64,Int32), x, p1, p2, p3, give_log)
        ($dd)(x::Number, p1::Number, p2::Number, p3::Number) =
            ccall(dlsym(_jl_libRmath,$string(dd)), Float64, (Float64,Float64,Float64,Float64,Int32), x, p1, p2, p3, false)
        @_jl_libRmath_vectorize_4arg $dd
        ($pp)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(pp)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), q, p1, p2, p3, lower_tail, log_p)
        ($pp)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(pp)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), q, p1, p2, p3, lower_tail, false)
        ($pp)(q::Number, p1::Number, p2::Number, p3::Number) =
            ccall(dlsym(_jl_libRmath,$string(pp)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), q, p1, p2, p3, true, false)
        @_jl_libRmath_vectorize_4arg $qq
        ($qq)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$string(qq)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), p, p1, p2, p3, lower_tail, log_p)
        ($qq)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool) =
            ccall(dlsym(_jl_libRmath,$string(qq)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), p, p1, p2, p3, lower_tail, false)
        ($qq)(q::Number, p1::Number, p2::Number, p3::Number) =
            ccall(dlsym(_jl_libRmath,$string(qq)), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), p, p1, p2, p3, true, false)
        @_jl_libRmath_vectorize_4arg $qq
        ($rr)(nn::Integer, p1::Number, p2::Number, p3::Number) =
            [ ccall(dlsym(_jl_libRmath,$string(rr)), Float64, (Float64,Float64,Float64), p1, p2, p3) | i=1:nn ]
    end
end

@_jl_libRmath_3par_0d dhyper phyper qhyper rhyper
@_jl_libRmath_3par_0d dnbeta pnbeta qnbeta rnbeta
@_jl_libRmath_3par_0d dnf pnf qnf rnf

