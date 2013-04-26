module MPFR

export
    # Types
    MPFRFloat,
    # Functions
    exp10,
    get_bigfloat_precision,
    set_bigfloat_precision,
    with_bigfloat_precision
    
import
    Base: (*), +, -, /, <, <=, ==, >, >=, ^, besselj, besselj0, besselj1,
        bessely, bessely0, bessely1, ceil, cmp, convert, copysign, exp, exp2,
        exponent, factorial, floor, hypot, integer_valued, iround, isfinite,
        isinf, isnan, ldexp, log, log2, log10, max, min, mod, modf, nextfloat,
        prevfloat, promote_rule, rem, round, show, showcompact, sum, sqrt,
        string, trunc, get_precision,
    # import trigonometric functions
        sin, cos, tan, sec, csc, cot, acos, asin, atan, cosh, sinh, tanh,
        sech, csch, coth, acosh, asinh, atanh, atan2

const ROUNDING_MODE = [0]
const DEFAULT_PRECISION = [256]

# Basic type and initialization definitions

type MPFRFloat <: FloatingPoint
    prec::Clong
    sign::Cint
    exp::Clong
    d::Ptr{Void}
    function MPFRFloat()
        N = get_bigfloat_precision()
        z = new(zero(Clong), zero(Cint), zero(Clong), C_NULL)
        ccall((:mpfr_init2,:libmpfr), Void, (Ptr{MPFRFloat}, Clong), &z, N)
        finalizer(z, MPFR_clear)
        return z
    end
    # Not recommended for general use
    function MPFRFloat(prec::Clong, sign::Cint, exp::Clong, d::Ptr{Void})
        new(prec, sign, exp, d)
    end
end
MPFR_clear(mpfr::MPFRFloat) = ccall((:mpfr_clear, :libmpfr), Void, (Ptr{MPFRFloat},), &mpfr)

MPFRFloat(x::MPFRFloat) = x

for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPFRFloat(x::($fC))
            z = MPFRFloat()
            ccall(($(string(:mpfr_set_,fJ)), :libmpfr), Int32, (Ptr{MPFRFloat}, ($fC), Int32), &z, x, ROUNDING_MODE[end])
            return z
        end
    end
end

function MPFRFloat(x::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_set_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, ROUNDING_MODE[end])   
    return z
end

function MPFRFloat(x::BigFloat)
    z = MPFRFloat()
    ccall((:mpfr_set_f, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{Void}, Int32), &z, x.mpf, ROUNDING_MODE[end])   
    return z
end

function MPFRFloat(x::String, base::Int)
    z = MPFRFloat()
    err = ccall((:mpfr_set_str, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{Uint8}, Int32, Int32), &z, x, base, ROUNDING_MODE[end])
    if err != 0; error("Invalid input"); end
    return z
end
MPFRFloat(x::String) = MPFRFloat(x, 10)


MPFRFloat(x::Bool) = MPFRFloat(uint(x))
MPFRFloat(x::Signed) = MPFRFloat(int(x))
MPFRFloat(x::Unsigned) = MPFRFloat(uint(x))
if WORD_SIZE == 32
    MPFRFloat(x::Int64) = MPFRFloat(string(x))
    MPFRFloat(x::Uint64) = MPFRFloat(BigInt(x))
end
MPFRFloat(x::Float32) = MPFRFloat(float64(x))
MPFRFloat(x::Rational) = MPFRFloat(num(x)) / MPFRFloat(den(x))

convert(::Type{MPFRFloat}, x::Rational) = MPFRFloat(x) # to resolve ambiguity
convert(::Type{MPFRFloat}, x::Real) = MPFRFloat(x)


convert(::Type{Int64}, x::MPFRFloat) = int64(convert(Clong, x))
convert(::Type{Int32}, x::MPFRFloat) = int32(convert(Clong, x))
convert(::Type{Clong}, x::MPFRFloat) = integer_valued(x) ?
    ccall((:mpfr_get_si,:libmpfr), Clong, (Ptr{MPFRFloat}, Int32), &x, ROUNDING_MODE[end]) :
    throw(InexactError())
convert(::Type{Uint32}, x::MPFRFloat) = uint64(convert(Culong, x))
convert(::Type{Uint32}, x::MPFRFloat) = uint32(convert(Culong, x))
convert(::Type{Culong}, x::MPFRFloat) = integer_valued(x) ?
    ccall((:mpfr_get_ui,:libmpfr), Culong, (Ptr{MPFRFloat}, Int32), &x, ROUNDING_MODE[end]) :
    throw(InexactError())
function convert(::Type{BigInt}, x::MPFRFloat) 
    if integer_valued(x)
        z = BigInt()
        ccall((:mpfr_get_z,:libmpfr), Int32, (Ptr{BigInt}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
        return z
    else
        throw(InexactError())
    end
end
convert(::Type{Float64}, x::MPFRFloat) = ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{MPFRFloat},), &x)
convert(::Type{Float32}, x::MPFRFloat) = ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{MPFRFloat},), &x)

promote_rule{T<:Real}(::Type{MPFRFloat}, ::Type{T}) = MPFRFloat


# TODO: Decide if overwriting the default BigFloat rule is good
promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = MPFRFloat
promote_rule{T<:FloatingPoint}(::Type{BigFloat},::Type{T}) = MPFRFloat

# Basic arithmetic without promotion
# Unsigned addition
function +(x::MPFRFloat, c::Culong)
    z = MPFRFloat()
    ccall((:mpfr_add_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Culong, x::MPFRFloat) = x + c
+(c::Unsigned, x::MPFRFloat) = x + convert(Culong, c)
+(x::MPFRFloat, c::Unsigned) = x + convert(Culong, c)

# Signed addition
function +(x::MPFRFloat, c::Clong)
    z = MPFRFloat()
    ccall((:mpfr_add_si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Clong, x::MPFRFloat) = x + c
+(x::MPFRFloat, c::Signed) = x + convert(Clong, c)
+(c::Signed, x::MPFRFloat) = x + convert(Clong, c)

# Float64 addition
function +(x::MPFRFloat, c::Float64)
    z = MPFRFloat()
    ccall((:mpfr_add_d, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
+(c::Float64, x::MPFRFloat) = x + c
+(c::Float32, x::MPFRFloat) = x + convert(Float64, c)
+(x::MPFRFloat, c::Float32) = x + convert(Float64, c)

# BigInt addition
function +(x::MPFRFloat, c::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_add_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
+(c::BigInt, x::MPFRFloat) = x + c

# Unsigned subtraction
function -(x::MPFRFloat, c::Culong)
    z = MPFRFloat()
    ccall((:mpfr_sub_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Culong, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_ui_sub, :libmpfr), Int32, (Ptr{MPFRFloat}, Culong, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::MPFRFloat, c::Unsigned) = -(x, convert(Culong, c))
-(c::Unsigned, x::MPFRFloat) = -(convert(Culong, c), x)

# Signed subtraction
function -(x::MPFRFloat, c::Clong)
    z = MPFRFloat()
    ccall((:mpfr_sub_si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Clong, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_si_sub, :libmpfr), Int32, (Ptr{MPFRFloat}, Clong, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::MPFRFloat, c::Signed) = -(x, convert(Clong, c))
-(c::Signed, x::MPFRFloat) = -(convert(Clong, c), x)

# Float64 subtraction
function -(x::MPFRFloat, c::Float64)
    z = MPFRFloat()
    ccall((:mpfr_sub_d, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function -(c::Float64, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_d_sub, :libmpfr), Int32, (Ptr{MPFRFloat}, Float64, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
-(x::MPFRFloat, c::Float32) = -(x, convert(Float64, c))
-(c::Float32, x::MPFRFloat) = -(convert(Float64, c), x)

# BigInt subtraction
function -(x::MPFRFloat, c::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_sub_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
function -(c::BigInt, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_z_sub, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{BigInt}, Ptr{MPFRFloat}, Int32), &z, &c, &x, ROUNDING_MODE[end])
    return z
end

# Unsigned multiplication
function *(x::MPFRFloat, c::Culong)
    z = MPFRFloat()
    ccall((:mpfr_mul_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Culong, x::MPFRFloat) = x * c
*(c::Unsigned, x::MPFRFloat) = x * convert(Culong, c)
*(x::MPFRFloat, c::Unsigned) = x * convert(Culong, c)

# Signed multiplication
function *(x::MPFRFloat, c::Clong)
    z = MPFRFloat()
    ccall((:mpfr_mul_si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Clong, x::MPFRFloat) = x * c
*(x::MPFRFloat, c::Signed) = x * convert(Clong, c)

# Float64 multiplication
function *(x::MPFRFloat, c::Float64)
    z = MPFRFloat()
    ccall((:mpfr_mul_d, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
*(c::Float64, x::MPFRFloat) = x * c
*(c::Float32, x::MPFRFloat) = x * convert(Float64, c)
*(x::MPFRFloat, c::Float32) = x * convert(Float64, c)

# BigInt multiplication
*(c::Signed, x::MPFRFloat) = x * convert(Clong, c)
function *(x::MPFRFloat, c::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_mul_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end
*(c::BigInt, x::MPFRFloat) = x * c

# Unsigned division
function /(x::MPFRFloat, c::Culong)
    z = MPFRFloat()
    ccall((:mpfr_div_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Culong, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_ui_div, :libmpfr), Int32, (Ptr{MPFRFloat}, Culong, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::MPFRFloat, c::Unsigned) = /(x, convert(Culong, c))
/(c::Unsigned, x::MPFRFloat) = /(convert(Culong, c), x)

# Signed division
function /(x::MPFRFloat, c::Clong)
    z = MPFRFloat()
    ccall((:mpfr_div_si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Clong, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_si_div, :libmpfr), Int32, (Ptr{MPFRFloat}, Clong, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::MPFRFloat, c::Signed) = /(x, convert(Clong, c))
/(c::Signed, x::MPFRFloat) = /(convert(Clong, c), x)

# Float64 division
function /(x::MPFRFloat, c::Float64)
    z = MPFRFloat()
    ccall((:mpfr_div_d, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Float64, Int32), &z, &x, c, ROUNDING_MODE[end])
    return z
end
function /(c::Float64, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_d_div, :libmpfr), Int32, (Ptr{MPFRFloat}, Float64, Ptr{MPFRFloat}, Int32), &z, c, &x, ROUNDING_MODE[end])
    return z
end
/(x::MPFRFloat, c::Float32) = /(x, convert(Float64, c))
/(c::Float32, x::MPFRFloat) = /(convert(Float64, c), x)

# BigInt division
function /(x::MPFRFloat, c::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_div_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[end])
    return z
end

# Basic operations
for (fJ, fC) in ((:+,:add), (:-,:sub), (:*,:mul), (:/,:div), (:^, :pow))
    @eval begin 
        function ($fJ)(x::MPFRFloat, y::MPFRFloat)
            z = MPFRFloat()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
            return z
        end
    end
end

function -(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_neg, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function cmp(x::MPFRFloat, y::MPFRFloat)
    ccall((:mpfr_cmp, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y)
end

function sqrt(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_sqrt, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    if isnan(z)
        throw(DomainError())
    end
    return z
end

for f in (:ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::MPFRFloat)
            z = MPFRFloat()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &z, &x)
            return z
        end
    end
end

function ^(x::MPFRFloat, y::Unsigned)
    z = MPFRFloat()
    ccall((:mpfr_pow_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPFRFloat, y::Signed)
    z = MPFRFloat()
    ccall((:mpfr_pow_si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPFRFloat, y::BigInt)
    z = MPFRFloat()
    ccall((:mpfr_pow_z, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{BigInt}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function exp(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_exp, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function exp2(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_exp2, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function exp10(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_exp10, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function ldexp(x::MPFRFloat, n::Clong)
    z = MPFRFloat()
    ccall((:mpfr_mul_2si, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Clong, Int32), &z, &x, n, ROUNDING_MODE[end])
    return z
end
function ldexp(x::MPFRFloat, n::Culong)
    z = MPFRFloat()
    ccall((:mpfr_mul_2ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Culong, Int32), &z, &x, n, ROUNDING_MODE[end])
    return z
end
ldexp(x::MPFRFloat, n::Signed) = ldexp(x, convert(Clong, n))
ldexp(x::MPFRFloat, n::Unsigned) = ldexp(x, convert(Culong, n))

function besselj0(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_j0, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function besselj1(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_j1, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function besselj(n::Integer, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_jn, :libmpfr), Int32, (Ptr{MPFRFloat}, Clong, Ptr{MPFRFloat}, Int32), &z, n, &x, ROUNDING_MODE[end])
    return z
end

function bessely0(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_y0, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely1(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_y1, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function bessely(n::Integer, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_yn, :libmpfr), Int32, (Ptr{MPFRFloat}, Clong, Ptr{MPFRFloat}, Int32), &z, n, &x, ROUNDING_MODE[end])
    return z
end

function factorial(x::MPFRFloat)
    if x < 0 || !integer_valued(x)
        throw(DomainError())
    end
    ui = convert(Culong, x)
    z = MPFRFloat()
    ccall((:mpfr_fac_ui, :libmpfr), Int32, (Ptr{MPFRFloat}, Culong, Int32), &z, ui, ROUNDING_MODE[end])
    return z
end

function hypot(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_hypot, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function log(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat()
    ccall((:mpfr_log, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function log2(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat()
    ccall((:mpfr_log2, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function log10(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat()
    ccall((:mpfr_log10, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function max(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_max, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function min(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_min, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function modf(x::MPFRFloat)
    if isinf(x)
        return (MPFRFloat(NaN), x)
    end
    zint = MPFRFloat()
    zfloat = MPFRFloat()
    ccall((:mpfr_modf, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &zint, &zfloat, &x, ROUNDING_MODE[end])
    return (zfloat, zint)
end

function rem(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_remainder, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

# function sum{T<:MPFRFloat}(arr::AbstractArray{T})
#     z = MPFRFloat()
#     n = length(arr)
#     ptrarr = [pointer(&x) for x in arr]
#     ccall((:mpfr_sum, :libmpfr), Int32,
#         (Ptr{MPFRFloat}, Ptr{Void}, Culong, Int32), 
#         &z, ptrarr, n, ROUNDING_MODE[1])
#     return z
# end

# Trigonometric functions
# Every NaN is thrown as an error, and it follows somewhat closely
# the Base functions behavior
for f in (:sin,:cos,:tan,:sec,:csc,:cot,:acos,:asin,:atan,
        :cosh,:sinh,:tanh,:sech,:csch,:coth,:acosh,:asinh,:atanh)
    @eval begin
        function ($f)(x::MPFRFloat)
            z = MPFRFloat()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
            if isnan(z)
                throw(DomainError())
            end
            return z
        end
    end
end

function atan2(y::MPFRFloat, x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_atan2, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &y, &x, ROUNDING_MODE[end])
    return z
end

# Utility functions
==(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_equal_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y) != 0
<=(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_lessequal_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y) != 0
>=(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_greaterequal_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y) != 0
<(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_less_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y) != 0
>(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_greater_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}), &x, &y) != 0

function get_precision(x::MPFRFloat)
    return ccall((:mpfr_get_prec, :libmpfr), Clong, (Ptr{MPFRFloat},), &x)
end

get_bigfloat_precision() = DEFAULT_PRECISION[end]
function set_bigfloat_precision(x::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[end] = x
end

function copysign(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_copysign, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function exponent(x::MPFRFloat)
    if x == 0 || !isfinite(x)
        throw(DomainError())
    end
    # The '- 1' is to make it work as Base.exponent
    return ccall((:mpfr_get_exp, :libmpfr), Clong, (Ptr{MPFRFloat},), &x) - 1
end

function integer_valued(x::MPFRFloat)
    return ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{MPFRFloat},), &x) != 0
end

function iround(x::MPFRFloat)
    fits = ccall((:mpfr_fits_slong_p, :libmpfr), Int32, (Ptr{MPFRFloat}, Int32), &x, ROUNDING_MODE[end])
    if fits != 0
        return ccall((:mpfr_get_si, :libmpfr), Clong, (Ptr{MPFRFloat}, Int32), &x, ROUNDING_MODE[end])
    end
    z = BigInt()
    ccall((:mpfr_get_z, :libmpfr), Int32, (Ptr{BigInt}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

for (f,t) in ((:uint8,:Uint8), (:uint16,:Uint16), (:uint32,:Uint32),
              (:int64,:Int64), (:uint64,:Uint64),
              # requires int128/uint128(::Type{BigInt}) support
              # (:int128,:Int128), (:uint128,:Uint128),
              (:unsigned,:Uint), (:uint,:Uint))
    @eval iround(::Type{$t}, x::MPFRFloat) = ($f)(iround(x))
end

isfinite(x::MPFRFloat) = !isinf(x)
function isinf(x::MPFRFloat)
    return ccall((:mpfr_inf_p, :libmpfr), Int32, (Ptr{MPFRFloat},), &x) != 0
end

function isnan(x::MPFRFloat)
    return ccall((:mpfr_nan_p, :libmpfr), Int32, (Ptr{MPFRFloat},), &x) != 0
end

function nextfloat(x::MPFRFloat)
   z = copy(x)
   ccall((:mpfr_nextabove, :libmpfr), Int32, (Ptr{MPFRFloat},), &z) != 0
   return z
end

function prevfloat(x::MPFRFloat)
    z = copy(x)
   ccall((:mpfr_nextbelow, :libmpfr), Int32, (Ptr{MPFRFloat},), &z) != 0
   return z
end

function copy(x::MPFRFloat)
    z = MPFRFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function with_bigfloat_precision(f::Function, precision::Integer)
    old_precision = get_bigfloat_precision()
    set_bigfloat_precision(precision)
    ret = f()
    set_bigfloat_precision(old_precision)
    return ret
end

function round(x::MPFRFloat, prec::Integer)
    if prec < 1
        throw(DomainError())
    end
    prec = int(ceil(log2(10^prec)))
    z = MPFRFloat(x)
    ccall((:mpfr_prec_round, :libmpfr), Int32, (Ptr{MPFRFloat}, Clong, Int32), &z, prec, ROUNDING_MODE[end])
    return z
end

function string(x::MPFRFloat)
    lng = 128
    for i = 1:2
        z = Array(Uint8, lng)
        lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{Uint8}, Culong, Ptr{Uint8}, Ptr{MPFRFloat}...), z, lng, "%.Re", &x)
        if lng < 128 || i == 2
            return bytestring(convert(Ptr{Uint8}, z[1:lng]))
        end
    end
end

show(io::IO, b::MPFRFloat) = print(io, string(b) * " with $(get_precision(b)) bits of precision")
showcompact(io::IO, b::MPFRFloat) = print(io, string(b))

end #module
