module MPFR

export
    # Types
    BigFloat,
    # Functions
    bigfloat_pi,
    bigfloat_eulergamma,
    bigfloat_catalan,
    get_bigfloat_precision,
    set_bigfloat_precision,
    with_bigfloat_precision,
    set_bigfloat_rounding,
    get_bigfloat_rounding,
    with_bigfloat_rounding

import
    Base: (*), +, -, /, <, <=, ==, >, >=, ^, besselj, besselj0, besselj1,
        bessely, bessely0, bessely1, ceil, cmp, convert, copysign, exp, exp2,
        exponent, factorial, floor, hypot, integer_valued, iround, isfinite,
        isinf, isnan, ldexp, log, log2, log10, max, min, mod, modf, nextfloat,
        prevfloat, promote_rule, rem, round, show, showcompact, sum, sqrt,
        string, trunc, get_precision, exp10, expm1, gamma, lgamma, digamma,
        erf, erfc, zeta, log1p, airyai, iceil, ifloor, itrunc,
    # import trigonometric functions
        sin, cos, tan, sec, csc, cot, acos, asin, atan, cosh, sinh, tanh,
        sech, csch, coth, acosh, asinh, atanh, atan2

const ROUNDING_MODE = [0]
const DEFAULT_PRECISION = [256]

# Rounding modes
const RoundToNearest = 0
const RoundToZero = 1
const RoundUp = 2
const RoundDown = 3
const RoundAwayZero = 4

# Basic type and initialization definitions

type BigFloat <: FloatingPoint
    prec::Clong
    sign::Cint
    exp::Clong
    d::Ptr{Void}
    function BigFloat()
        N = get_bigfloat_precision()
        z = new(zero(Clong), zero(Cint), zero(Clong), C_NULL)
        ccall((:mpfr_init2,:libmpfr), Void, (Ptr{BigFloat}, Clong), &z, N)
        finalizer(z, MPFR_clear)
        return z
    end
    # Not recommended for general use
    function BigFloat(prec::Clong, sign::Cint, exp::Clong, d::Ptr{Void})
        new(prec, sign, exp, d)
    end
end
MPFR_clear(mpfr::BigFloat) = ccall((:mpfr_clear, :libmpfr), Void, (Ptr{BigFloat},), &mpfr)

BigFloat(x::BigFloat) = x

for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
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
    if err != 0; error("Invalid input"); end
    return z
end
BigFloat(x::String) = BigFloat(x, 10)


BigFloat(x::Bool) = BigFloat(uint(x))
BigFloat(x::Signed) = BigFloat(int(x))
BigFloat(x::Unsigned) = BigFloat(uint(x))
if WORD_SIZE == 32
    BigFloat(x::Int64) = BigFloat(string(x))
    BigFloat(x::Uint64) = BigFloat(BigInt(x))
end
BigFloat(x::Float32) = BigFloat(float64(x))
BigFloat(x::Rational) = BigFloat(num(x)) / BigFloat(den(x))

convert(::Type{BigFloat}, x::Rational) = BigFloat(x) # to resolve ambiguity
convert(::Type{BigFloat}, x::Real) = BigFloat(x)
convert(::Type{FloatingPoint}, x::BigInt) = BigFloat(x)


convert(::Type{Int64}, x::BigFloat) = int64(convert(Clong, x))
convert(::Type{Int32}, x::BigFloat) = int32(convert(Clong, x))
function convert(::Type{Clong}, x::BigFloat)
    fits = ccall((:mpfr_fits_slong_p, :libmpfr), Int32, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end]) != 0
    if integer_valued(x) && fits
        ccall((:mpfr_get_si,:libmpfr), Clong, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end])
    else
        throw(InexactError())
    end
end
convert(::Type{Uint64}, x::BigFloat) = uint64(convert(Culong, x))
convert(::Type{Uint32}, x::BigFloat) = uint32(convert(Culong, x))
function convert(::Type{Culong}, x::BigFloat)
    fits = ccall((:mpfr_fits_ulong_p, :libmpfr), Int32, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end]) != 0
    if integer_valued(x) && fits
        ccall((:mpfr_get_ui,:libmpfr), Culong, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end])
    else
        throw(InexactError())
    end
end
function convert(::Type{BigInt}, x::BigFloat)
    if integer_valued(x)
        z = BigInt()
        ccall((:mpfr_get_z,:libmpfr), Int32, (Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
        return z
    else
        throw(InexactError())
    end
end
convert(::Type{Float64}, x::BigFloat) = ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{BigFloat},), &x)
convert(::Type{Float32}, x::BigFloat) = ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{BigFloat},), &x)

promote_rule{T<:Real}(::Type{BigFloat}, ::Type{T}) = BigFloat

promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = BigFloat
promote_rule{T<:FloatingPoint}(::Type{BigFloat},::Type{T}) = BigFloat

# Basic arithmetic without promotion
# Unsigned addition
function +(x::BigFloat, c::Culong)
    z = BigFloat()
    ccall((:mpfr_add_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Culong, x::BigFloat) = x + c
+(c::Unsigned, x::BigFloat) = x + convert(Culong, c)
+(x::BigFloat, c::Unsigned) = x + convert(Culong, c)

# Signed addition
function +(x::BigFloat, c::Clong)
    z = BigFloat()
    ccall((:mpfr_add_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Clong, x::BigFloat) = x + c
+(x::BigFloat, c::Signed) = x + convert(Clong, c)
+(c::Signed, x::BigFloat) = x + convert(Clong, c)

# Float64 addition
function +(x::BigFloat, c::Float64)
    z = BigFloat()
    ccall((:mpfr_add_d, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Float64, x::BigFloat) = x + c
+(c::Float32, x::BigFloat) = x + convert(Float64, c)
+(x::BigFloat, c::Float32) = x + convert(Float64, c)

# BigInt addition
function +(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_add_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
+(c::BigInt, x::BigFloat) = x + c

# Unsigned subtraction
function -(x::BigFloat, c::Culong)
    z = BigFloat()
    ccall((:mpfr_sub_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Culong, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ui_sub, :libmpfr), Int32, (Ptr{BigFloat}, Culong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::BigFloat, c::Unsigned) = -(x, convert(Culong, c))
-(c::Unsigned, x::BigFloat) = -(convert(Culong, c), x)

# Signed subtraction
function -(x::BigFloat, c::Clong)
    z = BigFloat()
    ccall((:mpfr_sub_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Clong, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_si_sub, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::BigFloat, c::Signed) = -(x, convert(Clong, c))
-(c::Signed, x::BigFloat) = -(convert(Clong, c), x)

# Float64 subtraction
function -(x::BigFloat, c::Float64)
    z = BigFloat()
    ccall((:mpfr_sub_d, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Float64, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_d_sub, :libmpfr), Int32, (Ptr{BigFloat}, Float64, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::BigFloat, c::Float32) = -(x, convert(Float64, c))
-(c::Float32, x::BigFloat) = -(convert(Float64, c), x)

# BigInt subtraction
function -(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_sub_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
function -(c::BigInt, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_z_sub, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &c, &x, ROUNDING_MODE[end])
    return z
end

# Unsigned multiplication
function *(x::BigFloat, c::Culong)
    z = BigFloat()
    ccall((:mpfr_mul_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Culong, x::BigFloat) = x * c
*(c::Unsigned, x::BigFloat) = x * convert(Culong, c)
*(x::BigFloat, c::Unsigned) = x * convert(Culong, c)

# Signed multiplication
function *(x::BigFloat, c::Clong)
    z = BigFloat()
    ccall((:mpfr_mul_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Clong, x::BigFloat) = x * c
*(x::BigFloat, c::Signed) = x * convert(Clong, c)

# Float64 multiplication
function *(x::BigFloat, c::Float64)
    z = BigFloat()
    ccall((:mpfr_mul_d, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Float64, x::BigFloat) = x * c
*(c::Float32, x::BigFloat) = x * convert(Float64, c)
*(x::BigFloat, c::Float32) = x * convert(Float64, c)

# BigInt multiplication
*(c::Signed, x::BigFloat) = x * convert(Clong, c)
function *(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_mul_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
*(c::BigInt, x::BigFloat) = x * c

# Unsigned division
function /(x::BigFloat, c::Culong)
    z = BigFloat()
    ccall((:mpfr_div_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Culong, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ui_div, :libmpfr), Int32, (Ptr{BigFloat}, Culong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::BigFloat, c::Unsigned) = /(x, convert(Culong, c))
/(c::Unsigned, x::BigFloat) = /(convert(Culong, c), x)

# Signed division
function /(x::BigFloat, c::Clong)
    z = BigFloat()
    ccall((:mpfr_div_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Clong, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_si_div, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::BigFloat, c::Signed) = /(x, convert(Clong, c))
/(c::Signed, x::BigFloat) = /(convert(Clong, c), x)

# Float64 division
function /(x::BigFloat, c::Float64)
    z = BigFloat()
    ccall((:mpfr_div_d, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Float64, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_d_div, :libmpfr), Int32, (Ptr{BigFloat}, Float64, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::BigFloat, c::Float32) = /(x, convert(Float64, c))
/(c::Float32, x::BigFloat) = /(convert(Float64, c), x)

# BigInt division
function /(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_div_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end

# Basic operations
for (fJ, fC) in ((:+,:add), (:-,:sub), (:*,:mul), (:/,:div), (:^, :pow))
    @eval begin 
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
            return z
        end
    end
end

function -(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_neg, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function cmp(x::BigFloat, y::BigFloat)
    ccall((:mpfr_cmp, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y)
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

function ^(x::BigFloat, y::Unsigned)
    z = BigFloat()
    ccall((:mpfr_pow_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::BigFloat, y::Signed)
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
          :cosh,:sinh,:tanh,:sech,:csch,:coth,)
    @eval function $f(x::BigFloat)
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
        return z
    end
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
ldexp(x::BigFloat, n::Signed) = ldexp(x, convert(Clong, n))
ldexp(x::BigFloat, n::Unsigned) = ldexp(x, convert(Culong, n))

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
    z = BigFloat()
    ccall((:mpfr_y0, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely1(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_y1, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely(n::Integer, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_yn, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, n, &x, ROUNDING_MODE[end])
    return z
end

function factorial(x::BigFloat)
    if x < 0 || !integer_valued(x)
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
    ccall((:mpfr_remainder, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

# function sum{T<:BigFloat}(arr::AbstractArray{T})
#     z = BigFloat()
#     n = length(arr)
#     ptrarr = [pointer(&x) for x in arr]
#     ccall((:mpfr_sum, :libmpfr), Int32,
#         (Ptr{BigFloat}, Ptr{Void}, Culong, Int32), 
#         &z, ptrarr, n, ROUNDING_MODE[1])
#     return z
# end

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

function atan2(y::BigFloat, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_atan2, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &y, &x, ROUNDING_MODE[end])
    return z
end

# Mathematical constants:
for (c, cmpfr) in ((:pi,:pi), (:eulergamma, :euler), (:catalan, :catalan))
    @eval function $(symbol(string("bigfloat_", c)))()
        c = BigFloat()
        ccall(($(string("mpfr_const_", cmpfr)), :libmpfr), 
              Cint, (Ptr{BigFloat}, Int32),
              &c, ROUNDING_MODE[end])
        return c
    end
end

# Utility functions
==(x::BigFloat, y::BigFloat) = ccall((:mpfr_equal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
<=(x::BigFloat, y::BigFloat) = ccall((:mpfr_lessequal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
>=(x::BigFloat, y::BigFloat) = ccall((:mpfr_greaterequal_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
<(x::BigFloat, y::BigFloat) = ccall((:mpfr_less_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0
>(x::BigFloat, y::BigFloat) = ccall((:mpfr_greater_p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &x, &y) != 0

function get_precision(x::BigFloat)
    return ccall((:mpfr_get_prec, :libmpfr), Clong, (Ptr{BigFloat},), &x)
end

get_bigfloat_precision() = DEFAULT_PRECISION[end]
function set_bigfloat_precision(x::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[end] = x
end

get_bigfloat_rounding() = ROUNDING_MODE[end]
function set_bigfloat_rounding(x::Int)
    if x < 0 || x > 4
        throw(DomainError())
    end
    ROUNDING_MODE[end] = x
end

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

function integer_valued(x::BigFloat)
    return ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

for f in (:ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &x)
            return z
        end
    end
end

for (f, g) in ((:iceil, :ceil), (:ifloor, :floor), (:itrunc, :trunc))
    @eval ($f)(x::BigFloat) = convert(BigInt, ($g)(x))
end

for (c,t) in ((:uint8,:Uint8),   (:int8,:Int8),
              (:uint16,:Uint16), (:int16,:Int16),
              (:uint32,:Uint32), (:int32,:Int32),
              (:uint64,:Uint64), (:int64,:Int64),
              (:uint,:Uint),     (:int, :Int))
    for (f, g) in ((:iceil, :ceil), (:ifloor, :floor), (:itrunc, :trunc))
        @eval begin
            @eval ($f)(::Type{$t}, x::BigFloat) = ($c)(($f)(x))
        end
    end
end

function iround(x::BigFloat)
    fits = ccall((:mpfr_fits_slong_p, :libmpfr), Int32, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end])
    if fits != 0
        return ccall((:mpfr_get_si, :libmpfr), Clong, (Ptr{BigFloat}, Int32), &x, ROUNDING_MODE[end])
    end
    z = BigInt()
    ccall((:mpfr_get_z, :libmpfr), Int32, (Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

for (f,t) in ((:uint8,:Uint8),   (:int8,:Int8),
              (:uint16,:Uint16), (:int16,:Int16),
              (:uint32,:Uint32), (:int32,:Int32),
              (:uint64,:Uint64), (:int64,:Int64),
              (:uint,:Uint),     (:int, :Int))
    @eval iround(::Type{$t}, x::BigFloat) = ($f)(iround(x))
end

isfinite(x::BigFloat) = !isinf(x)
function isinf(x::BigFloat)
    return ccall((:mpfr_inf_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

function isnan(x::BigFloat)
    return ccall((:mpfr_nan_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

function nextfloat(x::BigFloat)
   z = copy(x)
   ccall((:mpfr_nextabove, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
   return z
end

function prevfloat(x::BigFloat)
    z = copy(x)
   ccall((:mpfr_nextbelow, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
   return z
end

function copy(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function with_bigfloat_precision(f::Function, precision::Integer)
    old_precision = get_bigfloat_precision()
    set_bigfloat_precision(precision)
    try
        return f()
    finally
        set_bigfloat_precision(old_precision)
    end
end

function with_bigfloat_rounding(f::Function, rounding::Integer)
    old_rounding = get_bigfloat_rounding()
    set_bigfloat_rounding(rounding)
    try
        return f()
    finally
        set_bigfloat_rounding(old_rounding)
    end
end

function round(x::BigFloat, prec::Integer)
    if prec < 1
        throw(DomainError())
    end
    prec = int(ceil(log2(10^prec)))
    z = BigFloat(x)
    ccall((:mpfr_prec_round, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Int32), &z, prec, ROUNDING_MODE[end])
    return z
end

function string(x::BigFloat)
    lng = 128
    for i = 1:2
        z = Array(Uint8, lng)
        lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{Uint8}, Culong, Ptr{Uint8}, Ptr{BigFloat}...), z, lng, "%.Re", &x)
        if lng < 128 || i == 2
            return bytestring(convert(Ptr{Uint8}, z[1:lng]))
        end
    end
end

show(io::IO, b::BigFloat) = print(io, string(b) * " with $(get_precision(b)) bits of precision")
showcompact(io::IO, b::BigFloat) = print(io, string(b))

end #module
