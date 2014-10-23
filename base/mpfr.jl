module MPFR

export
    BigFloat,
    get_bigfloat_precision,
    set_bigfloat_precision,
    with_bigfloat_precision

import
    Base: (*), +, -, /, <, <=, ==, >, >=, ^, besselj, besselj0, besselj1, bessely,
        bessely0, bessely1, ceil, cmp, convert, copysign, deg2rad,
        exp, exp2, exponent, factorial, floor, hypot, isinteger, iround,
        isfinite, isinf, isnan, ldexp, log, log2, log10, max, min, mod, modf,
        nextfloat, prevfloat, promote_rule, rad2deg, rem, round, show,
        showcompact, sum, sqrt, string, print, trunc, precision, exp10, expm1,
        gamma, lgamma, digamma, erf, erfc, zeta, eta, log1p, airyai, iceil, ifloor,
        itrunc, eps, signbit, sin, cos, tan, sec, csc, cot, acos, asin, atan,
        cosh, sinh, tanh, sech, csch, coth, acosh, asinh, atanh, atan2,
        serialize, deserialize, cbrt, typemax, typemin,
        realmin, realmax, get_rounding, set_rounding, maxintfloat, widen,
        significand, frexp

import Base.GMP: ClongMax, CulongMax, CdoubleMax

import Base.Math.lgamma_r

const ROUNDING_MODE = [0]
const DEFAULT_PRECISION = [256]

# Basic type and initialization definitions

type BigFloat <: FloatingPoint
    prec::Clong
    sign::Cint
    exp::Clong
    d::Ptr{Culong}
    function BigFloat()
        N = get_bigfloat_precision()
        z = new(zero(Clong), zero(Cint), zero(Clong), C_NULL)
        ccall((:mpfr_init2,:libmpfr), Void, (Ptr{BigFloat}, Clong), &z, N)
        finalizer(z, Base.GMP._mpfr_clear_func)
        return z
    end
    # Not recommended for general use
    function BigFloat(prec::Clong, sign::Cint, exp::Clong, d::Ptr{Void})
        new(prec, sign, exp, d)
    end
end

widen(::Type{Float64}) = BigFloat
widen(::Type{BigFloat}) = BigFloat

BigFloat(x::BigFloat) = x

for (fJ, fC) in ((:si,:Clong), (:ui,:Culong), (:d,:Float64))
    @eval begin
        function BigFloat(x::($fC))
            z = BigFloat()
            ccall(($(string(:mpfr_set_,fJ)), :libmpfr), Int32, (Ptr{BigFloat}, ($fC), Int32), &z, x, ROUNDING_MODE[end])
            return z
        end
    end
end

function BigFloat(x::BigInt)
    z = BigFloat()
    ccall((:mpfr_set_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, ROUNDING_MODE[end])   
    return z
end

function BigFloat(x::String, base::Int)
    z = BigFloat()
    err = ccall((:mpfr_set_str, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{Uint8}, Int32, Int32), &z, x, base, ROUNDING_MODE[end])
    if err != 0; error("incorrectly formatted number"); end
    return z
end
BigFloat(x::String) = BigFloat(x, 10)

BigFloat(x::Integer) = BigFloat(BigInt(x))

BigFloat(x::Union(Bool,Int8,Int16,Int32)) = BigFloat(convert(Clong,x))
BigFloat(x::Union(Uint8,Uint16,Uint32)) = BigFloat(convert(Culong,x))

BigFloat(x::Union(Float16,Float32)) = BigFloat(float64(x))
BigFloat(x::Rational) = BigFloat(num(x)) / BigFloat(den(x))

convert(::Type{Rational}, x::BigFloat) = convert(Rational{BigInt}, x)
convert{S}(::Type{BigFloat}, x::Rational{S}) = BigFloat(x) # to resolve ambiguity
convert(::Type{BigFloat}, x::Real) = BigFloat(x)
convert(::Type{FloatingPoint}, x::BigInt) = BigFloat(x)

for to in (Int8, Int16, Int32, Int64)
    @eval begin
        function convert(::Type{$to}, x::BigFloat)
            (isinteger(x) && (typemin($to) <= x <= typemax($to))) || throw(InexactError())
            convert($to, ccall((:__gmpfr_mpfr_get_sj,:libmpfr),
                               Cintmax_t, (Ptr{BigFloat}, Int32), &x, 0))
        end
    end
end

for to in (Uint8, Uint16, Uint32, Uint64)
    @eval begin
        function convert(::Type{$to}, x::BigFloat)
            (isinteger(x) && (typemin($to) <= x <= typemax($to))) || throw(InexactError())
            convert($to, ccall((:__gmpfr_mpfr_get_uj,:libmpfr),
                               Cuintmax_t, (Ptr{BigFloat}, Int32), &x, 0))
        end
    end
end

function Base.BigInt(x::BigFloat)
    !isinteger(x) && throw(InexactError())
    return itrunc(x)
end
convert(::Type{Float64}, x::BigFloat) =
    ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{BigFloat},Int32), &x, ROUNDING_MODE[end])
convert(::Type{Float32}, x::BigFloat) =
    ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{BigFloat},Int32), &x, ROUNDING_MODE[end])

convert(::Type{Integer}, x::BigFloat) = convert(BigInt, x)

promote_rule{T<:Real}(::Type{BigFloat}, ::Type{T}) = BigFloat
promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = BigFloat
promote_rule{T<:FloatingPoint}(::Type{BigFloat},::Type{T}) = BigFloat

function convert(::Type{Rational{BigInt}}, x::FloatingPoint)
    if isnan(x); return zero(BigInt)//zero(BigInt); end
    if isinf(x); return copysign(one(BigInt),x)//zero(BigInt); end
    if x == 0;   return zero(BigInt) // one(BigInt); end
    s = max(precision(x) - exponent(x), 0)
    BigInt(ldexp(x,s)) // (BigInt(1) << s)
end

# serialization

function serialize(s, n::BigFloat)
    Base.serialize_type(s, BigFloat)
    serialize(s, string(n))
end

deserialize(s, ::Type{BigFloat}) = BigFloat(deserialize(s))

# Basic arithmetic without promotion
for (fJ, fC) in ((:+,:add), (:*,:mul))
    @eval begin 
        # BigFloat
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
            return z
        end

        # Unsigned Integer
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        ($fJ)(c::CulongMax, x::BigFloat) = ($fJ)(x,c)

        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        ($fJ)(c::ClongMax, x::BigFloat) = ($fJ)(x,c)

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cdouble, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        ($fJ)(c::CdoubleMax, x::BigFloat) = ($fJ)(x,c)

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
            return z
        end
        ($fJ)(c::BigInt, x::BigFloat) = ($fJ)(x,c)
    end
end

for (fJ, fC) in ((:-,:sub), (:/,:div))
    @eval begin 
        # BigFloat
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
            return z
        end

        # Unsigned Int
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        function ($fJ)(c::CulongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:ui_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Culong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
            return z
        end
        
        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        function ($fJ)(c::ClongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:si_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
            return z
        end

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cdouble, Int32), &z, &x, c, ROUNDING_MODE[end])
            return z
        end
        function ($fJ)(c::CdoubleMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:d_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Cdouble, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
            return z
        end

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
            return z
        end
        # no :mpfr_z_div function
    end
end

function -(c::BigInt, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_z_sub, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &c, &x, ROUNDING_MODE[end])
    return z
end



# More efficient commutative operations
for (fJ, fC, fI) in ((:+, :add, 0), (:*, :mul, 1))
    @eval begin
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[end])
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &d, ROUNDING_MODE[end])
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat, e::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &d, ROUNDING_MODE[end])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &e, ROUNDING_MODE[end])
            return z
        end
    end
end

function -(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_neg, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function sqrt(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_sqrt, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    if isnan(z)
        throw(DomainError())
    end
    return z
end

sqrt(x::BigInt) = sqrt(BigFloat(x))

rad2deg(z::BigFloat) = 180/big(pi)*z
deg2rad(z::BigFloat) = big(pi)/180*z

function ^(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_pow, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function ^(x::BigFloat, y::CulongMax)
    z = BigFloat()
    ccall((:mpfr_pow_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::BigFloat, y::ClongMax)
    z = BigFloat()
    ccall((:mpfr_pow_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::BigFloat, y::BigInt)
    z = BigFloat()
    ccall((:mpfr_pow_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

for f in (:exp, :exp2, :exp10, :expm1, :digamma, :erf, :erfc, :zeta,
          :cosh,:sinh,:tanh,:sech,:csch,:coth, :cbrt)
    @eval function $f(x::BigFloat)
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
        return z
    end
end

# return log(2)
function big_ln2()
    c = BigFloat()
    ccall((:mpfr_const_log2, :libmpfr), Cint, (Ptr{BigFloat}, Int32),
          &c, MPFR.ROUNDING_MODE[end])
    return c
end

function eta(x::BigFloat)
    x == 1 && return big_ln2()
    return -zeta(x) * expm1(big_ln2()*(1-x))
end

function airyai(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ai, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function ldexp(x::BigFloat, n::Clong)
    z = BigFloat()
    ccall((:mpfr_mul_2si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, n, ROUNDING_MODE[end])
    return z
end
function ldexp(x::BigFloat, n::Culong)
    z = BigFloat()
    ccall((:mpfr_mul_2ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, n, ROUNDING_MODE[end])
    return z
end
ldexp(x::BigFloat, n::ClongMax) = ldexp(x, convert(Clong, n))
ldexp(x::BigFloat, n::CulongMax) = ldexp(x, convert(Culong, n))
ldexp(x::BigFloat, n::Integer) = x*exp2(BigFloat(n))

function besselj0(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_j0, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function besselj1(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_j1, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function besselj(n::Integer, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_jn, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, n, &x, ROUNDING_MODE[end])
    return z
end

function bessely0(x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_y0, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely1(x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_y1, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely(n::Integer, x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_yn, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, n, &x, ROUNDING_MODE[end])
    return z
end

function factorial(x::BigFloat)
    if x < 0 || !isinteger(x)
        throw(DomainError())
    end
    ui = convert(Culong, x)
    z = BigFloat()
    ccall((:mpfr_fac_ui, :libmpfr), Int32, (Ptr{BigFloat}, Culong, Int32), &z, ui, ROUNDING_MODE[end])
    return z
end

function hypot(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_hypot, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

for f in (:log, :log2, :log10)
    @eval function $f(x::BigFloat)
        if x < 0
            throw(DomainError())
        end
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
        return z
    end
end

function log1p(x::BigFloat)
    if x < -1
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_log1p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function max(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_max, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function min(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_min, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function modf(x::BigFloat)
    if isinf(x)
        return (BigFloat(NaN), x)
    end
    zint = BigFloat()
    zfloat = BigFloat()
    ccall((:mpfr_modf, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &zint, &zfloat, &x, ROUNDING_MODE[end])
    return (zfloat, zint)
end

function rem(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_fmod, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function sum(arr::AbstractArray{BigFloat})
    z = BigFloat(0)
    for i in arr
        ccall((:mpfr_add, :libmpfr), Int32,
            (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Cint),
            &z, &z, &i, 0)
    end
    return z
end

# Functions for which NaN results are converted to DomainError, following Base
for f in (:sin,:cos,:tan,:sec,:csc,
          :acos,:asin,:atan,:acosh,:asinh,:atanh, :gamma)
    @eval begin
        function ($f)(x::BigFloat)
            if isnan(x)
                return x
            end
            z = BigFloat()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
            if isnan(z)
                throw(DomainError())
            end
            return z
        end
    end
end

# log of absolute value of gamma function
const lgamma_signp = Array(Cint, 1)
function lgamma(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_lgamma,:libmpfr), Cint, (Ptr{BigFloat}, Ptr{Cint}, Ptr{BigFloat}, Int32), &z, lgamma_signp, &x, ROUNDING_MODE[end])
    return z
end

lgamma_r(x::BigFloat) = (lgamma(x), lgamma_signp[1])

function atan2(y::BigFloat, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_atan2, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &y, &x, ROUNDING_MODE[end])
    return z
end

# Utility functions
==(x::BigFloat, y::BigFloat) = ccall((:mpfr_equal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
<=(x::BigFloat, y::BigFloat) = ccall((:mpfr_lessequal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
>=(x::BigFloat, y::BigFloat) = ccall((:mpfr_greaterequal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
<(x::BigFloat, y::BigFloat) = ccall((:mpfr_less_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
>(x::BigFloat, y::BigFloat) = ccall((:mpfr_greater_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0

function cmp(x::BigFloat, y::BigInt)
    isnan(x) && throw(DomainError())
    ccall((:mpfr_cmp_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}), &x, &y)
end
function cmp(x::BigFloat, y::ClongMax)
    isnan(x) && throw(DomainError())
    ccall((:mpfr_cmp_si, :libmpfr), Int32, (Ptr{BigFloat}, Clong), &x, y)
end
function cmp(x::BigFloat, y::CulongMax)
    isnan(x) && throw(DomainError())
    ccall((:mpfr_cmp_ui, :libmpfr), Int32, (Ptr{BigFloat}, Culong), &x, y)
end
cmp(x::BigFloat, y::Integer) = cmp(x,big(y))
cmp(x::Integer, y::BigFloat) = -cmp(y,x)

function cmp(x::BigFloat, y::CdoubleMax)
    (isnan(x) || isnan(y)) && throw(DomainError())
    ccall((:mpfr_cmp_d, :libmpfr), Int32, (Ptr{BigFloat}, Cdouble), &x, y)
end
cmp(x::CdoubleMax, y::BigFloat) = -cmp(y,x)

==(x::BigFloat, y::Integer)   = !isnan(x) && cmp(x,y) == 0
==(x::Integer, y::BigFloat)   = y == x
==(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) == 0
==(x::CdoubleMax, y::BigFloat) = y == x

<(x::BigFloat, y::Integer)   = !isnan(x) && cmp(x,y) < 0
<(x::Integer, y::BigFloat)   = !isnan(y) && cmp(y,x) > 0
<(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) < 0
<(x::CdoubleMax, y::BigFloat) = !isnan(x) && !isnan(y) && cmp(y,x) > 0

<=(x::BigFloat, y::Integer)   = !isnan(x) && cmp(x,y) <= 0
<=(x::Integer, y::BigFloat)   = !isnan(y) && cmp(y,x) >= 0
<=(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) <= 0
<=(x::CdoubleMax, y::BigFloat) = !isnan(x) && !isnan(y) && cmp(y,x) >= 0

signbit(x::BigFloat) = ccall((:mpfr_signbit, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0

function precision(x::BigFloat)
    return ccall((:mpfr_get_prec, :libmpfr), Clong, (Ptr{BigFloat},), &x)
end

get_bigfloat_precision() = DEFAULT_PRECISION[end]
function set_bigfloat_precision(x::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[end] = x
end

maxintfloat(x::BigFloat) = BigFloat(2)^precision(x)
maxintfloat(::Type{BigFloat}) = BigFloat(2)^get_bigfloat_precision()

function to_mpfr(r::RoundingMode)
    c = r.code
    if !(0 <= c <= 4)
        error("invalid BigFloat rounding mode")
    end
    c
end

function from_mpfr(c::Integer)
    if !(0 <= c <= 4)
        error("invalid MPFR rounding mode code")
    end
    RoundingMode(c)
end

get_rounding(::Type{BigFloat}) = from_mpfr(ROUNDING_MODE[end])
set_rounding(::Type{BigFloat},r::RoundingMode) = ROUNDING_MODE[end] = to_mpfr(r)

function copysign(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_copysign, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function exponent(x::BigFloat)
    if x == 0 || !isfinite(x)
        throw(DomainError())
    end
    # The '- 1' is to make it work as Base.exponent
    return ccall((:mpfr_get_exp, :libmpfr), Clong, (Ptr{BigFloat},), &x) - 1
end

function frexp(x::BigFloat)
    z = BigFloat()
    c = Clong[0]
    ccall((:mpfr_frexp, :libmpfr), Int32, (Ptr{Clong}, Ptr{BigFloat}, Ptr{BigFloat}, Cint), c, &z, &x, ROUNDING_MODE[end])
    return (z, c[1])
end

function significand(x::BigFloat)
    z = BigFloat()
    c = Clong[0]
    ccall((:mpfr_frexp, :libmpfr), Int32, (Ptr{Clong}, Ptr{BigFloat}, Ptr{BigFloat}, Cint), c, &z, &x, ROUNDING_MODE[end])
    # Double the significand to make it work as Base.significand
    ccall((:mpfr_mul_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &z, 2, ROUNDING_MODE[end])
    return z
end

function isinteger(x::BigFloat)
    return ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

for f in (:ceil, :floor, :trunc, :round)
    @eval begin
        function ($f)(x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &x)
            return z
        end
    end
end

function itrunc(x::BigFloat)
    z = BigInt()
    ccall((:mpfr_get_z, :libmpfr), Int32, (Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &x, 0)
    return z
end

iround(x::BigFloat) = itrunc(round(x))

function isinf(x::BigFloat)
    return ccall((:mpfr_inf_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

function isnan(x::BigFloat)
    return ccall((:mpfr_nan_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

isfinite(x::BigFloat) = !isinf(x) && !isnan(x)

@eval typemax(::Type{BigFloat}) = $(BigFloat( Inf))
@eval typemin(::Type{BigFloat}) = $(BigFloat(-Inf))

function nextfloat(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, &x, ROUNDING_MODE[end])
    ccall((:mpfr_nextabove, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
    return z
end

function prevfloat(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, &x, ROUNDING_MODE[end])
    ccall((:mpfr_nextbelow, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
    return z
end

eps(::Type{BigFloat}) = nextfloat(BigFloat(1)) - BigFloat(1)

realmin(::Type{BigFloat}) = nextfloat(zero(BigFloat))
realmax(::Type{BigFloat}) = prevfloat(BigFloat(Inf))

function with_bigfloat_precision(f::Function, precision::Integer)
    old_precision = get_bigfloat_precision()
    set_bigfloat_precision(precision)
    try
        return f()
    finally
        set_bigfloat_precision(old_precision)
    end
end

function string(x::BigFloat)
    lng = 128
    for i = 1:2
        z = Array(Uint8, lng + 1)
        lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{Uint8}, Culong, Ptr{Uint8}, Ptr{BigFloat}...), z, lng + 1, "%.Re", &x)
        if lng < 128 || i == 2
            return bytestring(z[1:lng])
        end
    end
end

print(io::IO, b::BigFloat) = print(io, string(b))
show(io::IO, b::BigFloat) = print(io, string(b), " with $(precision(b)) bits of precision")
showcompact(io::IO, b::BigFloat) = print(io, string(b))

end #module
