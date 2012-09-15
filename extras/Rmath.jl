## Interface to the Rmath library ##
_jl_libRmath = dlopen("libRmath")

macro _jl_libRmath_vectorize_3arg(f)
    quote
        global $f
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::T3) =
            reshape([ ($f)(x[i], y, z) for i=1:numel(x) ], size(x))
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::AbstractArray{T2}, z::T3) =
            reshape([ ($f)(x, y[i], z) for i=1:numel(y) ], size(y))
        ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::T2, z::AbstractArray{T3}) =
            reshape([ ($f)(x, y, z[i]) for i=1:numel(z) ], size(z))
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::AbstractArray{T2}, z::T3)
            shp = promote_shape(size(x), size(y))
            reshape([ ($f)(x[i], y[i], z) for i=1:numel(x) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::T1, y::AbstractArray{T2}, z::AbstractArray{T3})
            shp = promote_shape(size(y), size(z))
            reshape([ ($f)(x, y[i], z[i]) for i=1:numel(y) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::AbstractArray{T3})
            shp = promote_shape(size(x), size(z))
            reshape([ ($f)(x[i], y, z[i]) for i=1:numel(x) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::T2, z::AbstractArray{T3})
            shp = promote_shape(size(x), size(z))
            reshape([ ($f)(x[i], y, z[i]) for i=1:numel(x) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number}(x::AbstractArray{T1}, y::AbstractArray{T2}, z::AbstractArray{T3})
            shp = promote_shape(promote_shape(size(x), size(y)), size(z))
            reshape([ ($f)(x[i], y, z[i]) for i=1:numel(x) ], shp)
        end
    end
end

## Is this version still needed?
set_seed(a1::Integer, a2::Integer) =
    ccall(dlsym(_jl_libRmath,:set_seed), Void, (Int32,Int32), a1, a2)

## The d-p-q functions in Rmath for signrank allocate storage that must be freed
## Signrank - Wilcoxon Signed Rank statistic
rsignrank(nn::Integer, p1::Number) =
    [ ccall(dlsym(_jl_libRmath, "rsignrank"), Float64, (Float64,), p1) for i=1:nn ]

## Need to handle the d-p-q for Wilcox separately because the Rmath functions allocate storage that must be freed.
## Wilcox - Wilcox's Rank Sum statistic (m, n) - probably only makes sense for positive integers
rwilcox(nn::Integer, p1::Number, p2::Number) =
    [ ccall(dlsym(_jl_libRmath, "rwilcox"), Float64, (Float64,Float64), p1, p2) for i=1:nn ]

## Vectorize over four numeric arguments
macro _jl_libRmath_vectorize_4arg(f)
    quote
        global $f
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::T3, a4::T4) =
            reshape([ ($f)(a1[i], a2, a3, a4) for i=1:numel(a1) ], size(a1))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::T3, a4::T4) =
            reshape([ ($f)(a1, a2[i], a3, a4) for i=1:numel(a2) ], size(a2))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::AbstractArray{T3}, a4::T4) =
            reshape([ ($f)(a1, a2, a3[i], a4) for i=1:numel(a3) ], size(a3))
        ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::T3, a4::AbstractArray{T4}) =
            reshape([ ($f)(a1, a2, a3, a4[i]) for i=1:numel(a4) ], size(a4))
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::T3, a4::T4)
            shp = promote_shape(size(a1), size(a2))
            reshape([ ($f)(a1[i], a2[i], a3, a4) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(size(a1), size(a3))
            reshape([ ($f)(a1[i], a2, a3[i], a4) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(size(a1), size(a4))
            reshape([ ($f)(a1[i], a2, a3, a4[i]) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(size(a2), size(a3))
            reshape([ ($f)(a1, a2[i], a3[i], a4) for i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(size(a2), size(a4))
            reshape([ ($f)(a1, a2[i], a3, a4[i]) for i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::T2, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(size(a3), size(a4))
            reshape([ ($f)(a1, a2, a3[i], a4[i]) for i=1:numel(a3) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::T4)
            shp = promote_shape(promote_shape(size(a1), size(a2)), size(a3))
            reshape([ ($f)(a1[i], a2[i], a3[i], a4) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::T3, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a1), size(a2)), size(a4))
            reshape([ ($f)(a1[i], a2[i], a3, a4[i]) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::T2, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a1), size(a3)), size(a4))
            reshape([ ($f)(a1[i], a2, a3[i], a4[i]) for i=1:numel(a1) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::T1, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(size(a2), size(a3)), size(a4))
            reshape([ ($f)(a1, a2[i], a3[i], a4[i]) for i=1:numel(a2) ], shp)
        end
        function ($f){T1<:Number, T2<:Number, T3<:Number, T4<:Number}(a1::AbstractArray{T1}, a2::AbstractArray{T2}, a3::AbstractArray{T3}, a4::AbstractArray{T4})
            shp = promote_shape(promote_shape(promote_shape(size(a1), size(a2)), size(a3)), size(a4))
            reshape([ ($f)(a1[i], a2[i], a3[i], a4[i]) for i=1:numel(a2) ], shp)
        end
    end
end

## Distributions with 1 parameter and no default
macro _jl_libRmath_1par_0d(base)
    dd = symbol(strcat("d", string(base)))
    pp = symbol(strcat("p", string(base)))
    qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))   
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, give_log::Bool) = 
            ccall(dlsym(_jl_libRmath,$(string(dd))), Float64, (Float64,Float64,Int32), x, p1, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, p1::Number) = ($dd)(x, p1, false)
        @vectorize_2arg Number $dd

        ($pp)(q::Number, p1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(pp))), Float64, (Float64,Float64,Int32,Int32), q, p1, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, lower_tail::Bool) = ($pp)(q, p1, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($pp)(q, p1, lower_tail, false)
        ($pp)(q::Number, p1::Number) = ($pp)(q, p1, true, false)
        @vectorize_2arg Number $pp

        ($qq)(p::Number, p1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qq))), Float64, (Float64,Float64,Int32,Int32), p, p1, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, lower_tail::Bool) = ($qq)(p, p1, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($qq)(p, p1, lower_tail, false)
        ($qq)(p::Number, p1::Number) = ($qq)(p, p1, true, false)
        @vectorize_2arg Number $qq

        ($rr)(nn::Integer, p1::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,), p1) for i=1:nn ]
    end
end

@_jl_libRmath_1par_0d t           # Student's t distribution (df)
@_jl_libRmath_1par_0d chisq       # Central Chi-squared distribution (df)
@_jl_libRmath_1par_0d pois        # Poisson distribution (lambda)
@_jl_libRmath_1par_0d geom        # Geometric distribution (prob)

## Distributions with 1 parameter and a default
macro _jl_libRmath_1par_1d(base, d1)
    dd = symbol(strcat("d", string(base)))
    pp = symbol(strcat("p", string(base)))
    qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))   
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, give_log::Bool) = 
            ccall(dlsym(_jl_libRmath,$(string(dd))), Float64, (Float64,Float64,Int32), x, p1, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, give_log::Bool) = ($dd)(x, $d1, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, give_log::Bool) = ($dd)(x, $d1, give_log)
        ($dd)(x::Number, p1::Number) = ($dd)(x, p1, false)
        @vectorize_2arg Number $dd
        ($dd)(x::Number) = ($dd)(x, $d1, false)
        ($dd){T<:Number}(x::AbstractArray{T}) = ($dd)(x, $d1, false)

        ($pp)(q::Number, p1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(pp))), Float64, (Float64,Float64,Int32,Int32), q, p1, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, $d1, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, lower_tail::Bool, log_p::Bool) = ($pp)(q, $d1, lower_tail, log_p)
        ($pp)(q::Number, p1::Number, lower_tail::Bool) = ($pp)(q, p1, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($pp)(q, p1, lower_tail, false)
        ($pp)(q::Number, lower_tail::Bool) = ($pp)(q, $d1, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, lower_tail::Bool) = ($pp)(q, $d1, lower_tail, false)
        ($pp)(q::Number, p1::Number) = ($pp)(q, p1, true, false)
        @vectorize_2arg Number $pp
        ($pp)(q::Number) = ($pp)(q, $d1, true, false)
        ($pp){T<:Number}(q::AbstractArray{T}) = ($pp)(q, $d1, true, false)

        ($qq)(p::Number, p1::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qq))), Float64, (Float64,Float64,Int32,Int32), p, p1, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, $d1, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, lower_tail::Bool, log_p::Bool) = ($qq)(p, $d1, lower_tail, log_p)
        ($qq)(p::Number, p1::Number, lower_tail::Bool) = ($qq)(p, p1, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($qq)(p, p1, lower_tail, false)
        ($qq)(p::Number, lower_tail::Bool) = ($qq)(p, $d1, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, lower_tail::Bool) = ($qq)(p, $d1, lower_tail, false)
        ($qq)(p::Number, p1::Number) = ($qq)(p, p1, true, false)
        @vectorize_2arg Number $qq
        ($qq)(p::Number) = ($qq)(p, $d1, true, false)
        ($qq){T<:Number}(p::AbstractArray{T}) = ($qq)(p, $d1, true, false)

        ($rr)(nn::Integer, p1::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,), p1) for i=1:nn ]
        ($rr)(nn::Integer) = ($rr)(nn, $d1)
    end
end

## May need to handle this as a special case.  The Rmath library uses 1/rate, not rate
@_jl_libRmath_1par_1d exp 1      # Exponential distribution (rate)

## Distributions with 2 parameters and no defaults
macro _jl_libRmath_2par_0d(base)
    dd = symbol(strcat("d", string(base)))
    pp = symbol(strcat("p", string(base)))
    qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))    
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, p2::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(dd))), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, p2::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, p2, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, p1::Number, p2::Number) = ($dd)(x, p1, p2, false)
        @_jl_libRmath_vectorize_3arg $dd

        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(pp))), Float64, (Float64,Float64,Float64,Int32,Int32), q, p1, p2, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, p2, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool) = ($pp)(q, p1, p2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) =
            reshape([ ($pp)(q[i], p1, p2, lower_tail, false) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, p2::Number) = ($pp)(q, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $pp

        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qq))), Float64, (Float64,Float64,Float64,Int32,Int32), p, p1, p2, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, p2, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool) = ($qq)(p, p1, p2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) =
            reshape([ ($qq)(p[i], p1, p2, lower_tail, false) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, p2::Number) = ($qq)(p, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $qq

        ($rr)(nn::Integer, p1::Number, p2::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,Float64), p1, p2) for i=1:nn ]
    end
end

@_jl_libRmath_2par_0d f           # Central F distribution (df1, df2)
@_jl_libRmath_2par_0d binom       # Binomial distribution (size, prob)
@_jl_libRmath_2par_0d nbinom      # Negative binomial distribution (size, prob)
@_jl_libRmath_2par_0d nbinom_mu   # Negative binomial distribution (size, mu)
@_jl_libRmath_2par_0d beta        # Beta distribution (shape1, shape2)
@_jl_libRmath_2par_0d nchisq      # Noncentral Chi-squared distribution (df, ncp)

## Distributions with 2 parameters and 1 default
macro _jl_libRmath_2par_1d(base, d2)
    dd = symbol(strcat("d", string(base)))
    pp = symbol(strcat("p", string(base)))
    qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))    
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, p2::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(dd))), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, p2::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, p2, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, p1::Number, give_log::Bool) = ($dd)(x, p1, $d2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, give_log::Bool) = ($dd)(x, p1, $d2, give_log)
        ($dd)(x::Number, p1::Number, p2::Number) = ($dd)(x, p1, p2, false)
        @_jl_libRmath_vectorize_3arg $dd
        ($dd)(x::Number, p1::Number) = ($dd)(x, p1, $d2, false)        
        @vectorize_2arg Number $dd
        
        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(pp))), Float64, (Float64,Float64,Float64,Int32,Int32), q, p1, p2, lower_tail, log_p)
        ($pp)(q::Number, p1::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, p1, $d2, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, p2, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, p1, $d2, lower_tail, log_p)
        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool) = ($pp)(q, p1, p2, lower_tail, false)
        ($pp)(q::Number, p1::Number, lower_tail::Bool) = ($pp)(q, p1, $d2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) = ($pp)(q, p1, p2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($pp)(q, p1, $d2, lower_tail, false)
        ($pp)(q::Number, p1::Number, p2::Number) = ($pp)(q, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $pp
        ($pp)(q::Number, p1::Number) = ($pp)(q, p1, $d2, true, false)
        @vectorize_2arg Number $pp
        
        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qq))), Float64, (Float64,Float64,Float64,Int32,Int32), p, p1, p2, lower_tail, log_p)
        ($qq)(p::Number, p1::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, p1, $d2, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, p2, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, p1, $d2, lower_tail, log_p)
        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool) = ($qq)(p, p1, p2, lower_tail, false)
        ($qq)(p::Number, p1::Number, lower_tail::Bool) = ($qq)(p, p1, $d2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) = ($qq)(p, p1, p2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($qq)(p, p1, $d2, lower_tail, false)
        ($qq)(p::Number, p1::Number, p2::Number) = ($qq)(p, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $qq
        ($qq)(p::Number, p1::Number) = ($qq)(p, p1, $d2, true, false)
        @vectorize_2arg Number $qq

        ($rr)(nn::Integer, p1::Number, p2::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,Float64), p1, p2) for i=1:nn ]
        ($rr)(nn::Integer, p1::Number) = ($rr)(nn, p1, $d2)
    end
end

@_jl_libRmath_2par_1d gamma 1     # Gamma distribution  (shape, scale)
@_jl_libRmath_2par_1d weibull 1   # Weibull distribution (shape, scale)

## Distributions with 2 parameters and 2 defaults
macro _jl_libRmath_2par_2d(base, d1, d2)
    ddsym = dd = symbol(strcat("d", string(base)))
    ppsym = pp = symbol(strcat("p", string(base)))
    qqsym = qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))    
    if (string(base) == "norm")
        ddsym = :dnorm4
        ppsym = :pnorm5
        qqsym = :qnorm5
    end
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, p2::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(ddsym))), Float64, (Float64,Float64,Float64,Int32), x, p1, p2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, p2::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, p2, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, p1::Number, give_log::Bool) = ($dd)(x, p1, $d2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, give_log::Bool) = ($dd)(x, p1, $d2, give_log)
        ($dd)(x::Number, give_log::Bool) = ($dd)(x, $d1, $d2, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, give_log::Bool) = ($dd)(x, $d1, $d2, give_log)
        ($dd)(x::Number, p1::Number, p2::Number) = ($dd)(x, p1, p2, false)
        @_jl_libRmath_vectorize_3arg $dd
        ($dd)(x::Number, p1::Number) = ($dd)(x, p1, $d2, false)
        @vectorize_2arg Number $dd
        ($dd)(x::Number) = ($dd)(x, $d1, $d2, false)
        ($dd){T<:Number}(x::AbstractArray{T}) = ($dd)(x, $d1, $d2, give_log)

        
        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(ppsym))), Float64, (Float64,Float64,Float64,Int32,Int32), q, p1, p2, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, p2, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, p1, $d2, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, p1, $d2, lower_tail, log_p)
        ($pp)(q::Number, lower_tail::Bool, log_p::Bool) = ($pp)(q, $d1, $d2, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, lower_tail::Bool, log_p::Bool) = ($pp)(q, $d1, $d2, lower_tail, log_p)
        ($pp)(q::Number, p1::Number, p2::Number, lower_tail::Bool) = ($pp)(q, p1, p2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) = ($pp)(q, p1, p2, lower_tail, false)
        ($pp)(q::Number, p1::Number, lower_tail::Bool) = ($pp)(q, p1, $d2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($pp)(q, p1, $d2, lower_tail, false)
        ($pp)(q::Number, lower_tail::Bool) = ($pp)(q, $d1, $d2, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, lower_tail::Bool) = ($pp)(q, $d1, $d2, lower_tail, false)
        ($pp)(q::Number, p1::Number, p2::Number) = ($pp)(q, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $pp
        ($pp)(q::Number, p1::Number) = ($pp)(q, p1, $d2, true, false)
        @vectorize_2arg Number $pp
        ($pp)(q::Number) = ($pp)(q, $d1, $d2, true, false)
        ($pp){T<:Number}(q::AbstractArray{T}) = ($pp)(q, $d1, $d2, true, false)
        
        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qqsym))), Float64, (Float64,Float64,Float64,Int32,Int32), p, p1, p2, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, p2, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, p1, $d2, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, p1, $d2, lower_tail, log_p)
        ($qq)(p::Number, lower_tail::Bool, log_p::Bool) = ($qq)(p, $d1, $d2, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, lower_tail::Bool, log_p::Bool) = ($qq)(p, $d1, $d2, lower_tail, log_p)
        ($qq)(p::Number, p1::Number, p2::Number, lower_tail::Bool) = ($qq)(p, p1, p2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, lower_tail::Bool) = ($qq)(p, p1, p2, lower_tail, false)
        ($qq)(p::Number, p1::Number, lower_tail::Bool) = ($qq)(p, p1, $d2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, lower_tail::Bool) = ($qq)(p, p1, $d2, lower_tail, false)
        ($qq)(p::Number, lower_tail::Bool) = ($qq)(p, $d1, $d2, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, lower_tail::Bool) = ($qq)(p, $d1, $d2, lower_tail, false)
        ($qq)(p::Number, p1::Number, p2::Number) = ($qq)(p, p1, p2, true, false)
        @_jl_libRmath_vectorize_3arg $qq
        ($qq)(p::Number, p1::Number) = ($qq)(p, p1, $d2, true, false)
        @vectorize_2arg Number $qq
        ($qq)(p::Number) = ($qq)(p, $d1, $d2, true, false)
        ($qq){T<:Number}(p::AbstractArray{T}) = ($qq)(p, $d1, $d2, true, false)

        ($rr)(nn::Integer, p1::Number, p2::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,Float64), p1, p2) for i=1:nn ]
        ($rr)(nn::Integer, p1::Number) = ($rr)(nn, p1, $d2)
        ($rr)(nn::Integer) = ($rr)(nn, $d1, $d2)
    end
end

@_jl_libRmath_2par_2d cauchy 0 1  # Cauchy distribution (location, scale)
@_jl_libRmath_2par_2d lnorm  0 1  # Log-normal distribution (meanlog, sdlog)
@_jl_libRmath_2par_2d logis  0 1  # Logistic distribution (location, scale)
@_jl_libRmath_2par_2d norm   0 1  # Normal (Gaussian) distribution (mu, sd)
@_jl_libRmath_2par_2d unif   0 1  # Uniform distribution (min, max)

## Distributions with 3 parameters and no defaults
macro _jl_libRmath_3par_0d(base)
    dd = symbol(strcat("d", string(base)))
    pp = symbol(strcat("p", string(base)))
    qq = symbol(strcat("q", string(base)))
    rr = symbol(strcat("r", string(base)))    
    quote
        global $dd, $pp, $qq, $rr
        ($dd)(x::Number, p1::Number, p2::Number, p3::Number, give_log::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(dd))), Float64, (Float64,Float64,Float64,Float64,Int32), x, p1, p2, p3, give_log)
        ($dd){T<:Number}(x::AbstractArray{T}, p1::Number, p2::Number, p3::Number, give_log::Bool) =
            reshape([ ($dd)(x[i], p1, p2, p3, give_log) for i=1:numel(x) ], size(x))
        ($dd)(x::Number, p1::Number, p2::Number, p3::Number) = ($dd)(x, p1, p2, p3, false)
        @_jl_libRmath_vectorize_4arg $dd

        ($pp)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(pp))), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), q, p1, p2, p3, lower_tail, log_p)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($pp)(q[i], p1, p2, p3, lower_tail, log_p) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool) = ($pp)(q, p1, p2, p3, lower_tail, false)
        ($pp){T<:Number}(q::AbstractArray{T}, p1::Number, p2::Number, p3::Number, lower_tail::Bool) =
            reshape([ ($pp)(q[i], p1, p2, p3, lower_tail, false) for i=1:numel(q) ], size(q))
        ($pp)(q::Number, p1::Number, p2::Number, p3::Number) = ($pp)(q, p1, p2, p3, true, false)
        @_jl_libRmath_vectorize_4arg $pp

        ($qq)(p::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            ccall(dlsym(_jl_libRmath,$(string(qq))), Float64, (Float64,Float64,Float64,Float64,Int32,Int32), p, p1, p2, p3, lower_tail, log_p)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, p3::Number, lower_tail::Bool, log_p::Bool) =
            reshape([ ($qq)(p[i], p1, p2, p3, lower_tail, log_p) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, p2::Number, p3::Number, lower_tail::Bool) = ($qq)(p, p1, p2, p3, lower_tail, false)
        ($qq){T<:Number}(p::AbstractArray{T}, p1::Number, p2::Number, p3::Number, lower_tail::Bool) =
            reshape([ ($qq)(p[i], p1, p2, p3, lower_tail, false) for i=1:numel(p) ], size(p))
        ($qq)(p::Number, p1::Number, p2::Number, p3::Number) = ($qq)(p, p1, p2, p3, true, false)
        @_jl_libRmath_vectorize_4arg $qq

        ($rr)(nn::Integer, p1::Number, p2::Number, p3::Number) =
            [ ccall(dlsym(_jl_libRmath,$(string(rr))), Float64, (Float64,Float64,Float64), p1, p2, p3) for i=1:nn ]
    end
end

@_jl_libRmath_3par_0d hyper       # Hypergeometric (m, n, k)
@_jl_libRmath_3par_0d nbeta       # Non-central beta (shape1, shape2, ncp)
@_jl_libRmath_3par_0d nf          # Non-central F (df1, df2, ncp)

## tukey (Studentized Range Distribution - p and q only - 3pars)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,log_p)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,lower_tail,false)
ptukey(q::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :ptukey), Float64, (Float64,Float64,Float64,Int32,Int32),q,nranges,nmeans,df,true,false)
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,log_p) for i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,false) for i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([ptukey(q[i],nmeans,df,nranges,true,false) for i=1:numel(q)], size(q))

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
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,log_p) for i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([ptukey(q[i],nmeans,df,nranges,lower_tail,false) for i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([ptukey(q[i],nmeans,df,nranges,true,false) for i=1:numel(q)], size(q))
ptukey{T<:Number}(q::AbstractArray{T}, nmeans::Number, df::Number) =
    reshape([ptukey(q[i],nmeans,df,1.,true,false) for i=1:numel(q)], size(q))

qtukey(q::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,lower_tail,log_p)
qtukey(p::Number, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,lower_tail,false)
qtukey(p::Number, nmeans::Number, df::Number, nranges::Number) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,nmeans,df,true,false)
qtukey(p::Number, nmeans::Number, df::Number) =
    ccall(dlsym(_jl_libRmath, :qtukey), Float64, (Float64,Float64,Float64,Int32,Int32),p,nranges,1.,df,true,false)
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool, log_p::Bool) =
    reshape([qtukey(p[i],nmeans,df,nranges,lower_tail,log_p) for i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number, lower_tail::Bool) =
    reshape([qtukey(p[i],nmeans,df,nranges,lower_tail,false) for i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number, nranges::Number) =
    reshape([qtukey(p[i],nmeans,df,nranges,true,false) for i=1:numel(p)], size(p))
qtukey{T<:Number}(p::AbstractArray{T}, nmeans::Number, df::Number) =
    reshape([qtukey(p[i],nmeans,df,1.,true,false) for i=1:numel(p)], size(p))
