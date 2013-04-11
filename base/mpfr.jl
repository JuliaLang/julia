module MPFR

export
    # Types
    MPFRFloat,
    # Functions
    exp10,
    prec,
    get_precision,
    set_precision,
    with_precision
    
import
    Base: (*), +, -, /, <, <=, ==, >, >=, ^, besselj, besselj0, besselj1,
        bessely, bessely0, bessely1, ceil, cmp, convert, copysign, exp, exp2, 
        exponent, factorial, floor, integer_valued, iround, isfinite, isinf, 
        isnan, log, log2, log10, max, min, mod, modf, nextfloat, prevfloat, 
        promote_rule, rem, round, show, showcompact, sum, sqrt, string, trunc,
    # import trigonometric functions
        sin, cos, tan, sec, csc, cot, acos, asin, atan, cosh, sinh, tanh,
        sech, csch, coth, acosh, asinh, atanh

const ROUNDING_MODE = [0]
const DEFAULT_PRECISION = [53, 53]

# Basic type and initialization definitions

immutable MPFRFloat{N} <: FloatingPoint
    mpfr::Vector{Int32}
    function MPFRFloat()
        if N < 2
            error("Invalid precision")
        end
        z = Array(Int32, 5)
        ccall((:mpfr_init2,:libmpfr), Void, (Ptr{Void}, Int), z, N)
        b = new(z)
        finalizer(b.mpfr, MPFR_clear)
        return b
    end
end

MPFR_clear(mpfr::Vector{Int32}) = ccall((:mpfr_clear, :libmpfr), Void, (Ptr{Void},), mpfr)

function MPFRFloat(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPFRFloat(x::($fC))
            z = MPFRFloat{DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpfr_set_,fJ)), :libmpfr), Int32, (Ptr{Void}, ($fC), Int32), z.mpfr, x, ROUNDING_MODE[end])   
            return z
        end
    end
end

function MPFRFloat(x::BigInt)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_set_z, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, &(x.mpz), ROUNDING_MODE[end])   
    return z
end

function MPFRFloat(x::BigFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_set_f, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpf, ROUNDING_MODE[end])   
    return z
end

function MPFRFloat(x::String, base::Int)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    err = ccall((:mpfr_set_str, :libmpfr), Int32, (Ptr{Void}, Ptr{Uint8}, Int32, Int32), z.mpfr, x, base, ROUNDING_MODE[end])
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

# TODO: fix the precision support here
convert{N}(::Type{MPFRFloat{N}}, x::Rational) = MPFRFloat(x) # to resolve ambiguity
convert{N}(::Type{MPFRFloat{N}}, x::Real) = MPFRFloat(x)
convert(::Type{MPFRFloat}, x::Real) = MPFRFloat(x)
convert{N,T}(::Type{MPFRFloat{N}}, x::MPFRFloat{T}) = MPFRFloat(x)


convert(::Type{Int64}, x::MPFRFloat) = integer_valued(x) ?
    ccall((:mpfr_get_si,:libmpfr), Int64, (Ptr{Void}, Int32), x.mpfr, ROUNDING_MODE[end]) :
    throw(InexactError())
convert(::Type{Int32}, x::MPFRFloat) = int32(convert(Int64, x))
convert(::Type{Uint64}, x::MPFRFloat) = integer_valued(x) ?
    ccall((:mpfr_get_ui,:libmpfr), Uint64, (Ptr{Void}, Int32), x.mpfr, ROUNDING_MODE[end]) :
    throw(InexactError())
function convert(::Type{BigInt}, x::MPFRFloat) 
    if integer_valued(x)
        z = BigInt()
        ccall((:mpfr_get_z,:libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), &(z.mpz), x.mpfr, ROUNDING_MODE[end])
        return z
    else
        throw(InexactError())
    end
end
convert(::Type{Uint32}, x::MPFRFloat) = uint32(convert(Int64, x))
convert(::Type{Float64}, x::MPFRFloat) = ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{Void},), x.mpfr)
convert(::Type{Float32}, x::MPFRFloat) = ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{Void},), x.mpfr)

# If two different precisions given, promote to the default
promote_rule{T,S}(::Type{MPFRFloat{T}}, ::Type{MPFRFloat{S}}) =
    MPFRFloat{DEFAULT_PRECISION[end]}

promote_rule{T<:Real,N}(::Type{MPFRFloat{N}}, ::Type{T}) = MPFRFloat{N}
promote_rule{T<:Real}(::Type{MPFRFloat}, ::Type{T}) = MPFRFloat


# TODO: Decide if overwriting the default BigFloat rule is good
#promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = MPFRFloat{DEFAULT_PRECISION[end]}
#promote_rule{T<:FloatingPoint}(::Type{BigFloat},::Type{T}) = MPFRFloat{DEFAULT_PRECISION[end]}

# Basic operations

for (fJ, fC) in ((:+,:add), (:-,:sub), (:*,:mul), (:/,:div), (:^, :pow))
    @eval begin 
        function ($fJ)(x::MPFRFloat, y::MPFRFloat)
            z = MPFRFloat{DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, y.mpfr, ROUNDING_MODE[end])
            return z
        end
    end
end

function -(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_neg, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function cmp(x::MPFRFloat, y::MPFRFloat)
    ccall((:mpfr_cmp, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr)
end

function sqrt(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_sqrt, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    if isnan(z)
        throw(DomainError())
    end
    return z
end

for f in (:ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::MPFRFloat)
            z = MPFRFloat{DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), z.mpfr, x.mpfr)
            return z
        end
    end
end

function ^(x::MPFRFloat, y::Uint)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_pow_ui, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Uint, Int32), z.mpfr, x.mpfr, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPFRFloat, y::Int)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_pow_si, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int, Int32), z.mpfr, x.mpfr, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPFRFloat, y::BigInt)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_pow_z, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, &(y.mpz), ROUNDING_MODE[end])
    return z
end

function exp(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_exp, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function exp2(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_exp2, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function exp10(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_exp10, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function besselj0(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_j0, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function besselj1(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_j1, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function besselj(nu::Integer, x::MPFRFloat)
    n = int64(nu)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_jn, :libmpfr), Int32, (Ptr{Void}, Int64, Ptr{Void}, Int32), z.mpfr, n, x.mpfr, ROUNDING_MODE[end])
    return z
end

function bessely0(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_y0, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function bessely1(x::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_y1, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function bessely(nu::Integer, x::MPFRFloat)
    n = int64(nu)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_yn, :libmpfr), Int32, (Ptr{Void}, Int64, Ptr{Void}, Int32), z.mpfr, n, x.mpfr, ROUNDING_MODE[end])
    return z
end

function factorial(x::MPFRFloat)
    if x < 0 || !integer_valued(x)
        throw(DomainError())
    end
    ui = uint64(x)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_fac_ui, :libmpfr), Int32, (Ptr{Void}, Uint64, Int32), z.mpfr, ui, ROUNDING_MODE[end])
    return z
end

function log(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_log, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function log2(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_log2, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function log10(x::MPFRFloat)
    if x < 0
        throw(DomainError())
    end
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_log10, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
    return z
end

function max(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_max, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, y.mpfr, ROUNDING_MODE[end])
    return z
end

function min(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_min, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, y.mpfr, ROUNDING_MODE[end])
    return z
end

function modf(x::MPFRFloat)
    if isinf(x)
        return (MPFRFloat(NaN), x)
    end
    zint = MPFRFloat{DEFAULT_PRECISION[end]}()
    zfloat = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_modf, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), zint.mpfr, zfloat.mpfr, x.mpfr, ROUNDING_MODE[end])
    return (zfloat, zint)
end

function rem(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_remainder, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, y.mpfr, ROUNDING_MODE[end])
    return z
end

function sum{T<:MPFRFloat}(arr::AbstractArray{T})
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    n = length(arr)
    ptrarr = [pointer(x.mpfr) for x in arr]
    ccall((:mpfr_sum, :libmpfr), Int32,
        (Ptr{Void}, Ptr{Void}, Uint, Int32), 
        z.mpfr, ptrarr, n, ROUNDING_MODE[1])
    return z
end

# Trigonometric functions
# Every NaN is thrown as an error, and it follows somewhat closely
# the Base functions behavior
for f in (:sin,:cos,:tan,:sec,:csc,:cot,:acos,:asin,:atan,
        :cosh,:sinh,:tanh,:sech,:csch,:coth,:acosh,:asinh,:atanh)
    @eval begin
        function ($f)(x::MPFRFloat)
            z = MPFRFloat{DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, ROUNDING_MODE[end])
            if isnan(z)
                throw(DomainError())
            end
            return z
        end
    end
end

# Utility functions
==(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_equal_p, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr) != 0
<=(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_lessequal_p, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr) != 0
>=(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_greaterequal_p, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr) != 0
<(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_less_p, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr) != 0
>(x::MPFRFloat, y::MPFRFloat) = ccall((:mpfr_greater_p, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}), x.mpfr, y.mpfr) != 0

function prec(x::MPFRFloat)
    return ccall((:mpfr_get_prec, :libmpfr), Int, (Ptr{Void},), x.mpfr)
end

get_precision() = DEFAULT_PRECISION[end]
function set_precision(x::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[end] = x
end

function copysign(x::MPFRFloat, y::MPFRFloat)
    z = MPFRFloat{DEFAULT_PRECISION[end]}()
    ccall((:mpfr_copysign, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpfr, y.mpfr, ROUNDING_MODE[end])
    return z
end

function exponent(x::MPFRFloat)
    if x == 0 || !isfinite(x)
        throw(DomainError())
    end
    # The '- 1' is to make it work as Base.exponent
    return ccall((:mpfr_get_exp, :libmpfr), Int, (Ptr{Void},), x.mpfr) - 1
end

function integer_valued(x::MPFRFloat)
    return ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{Void},), x.mpfr) != 0
end

function iround(x::MPFRFloat)
    fits = ccall((:mpfr_fits_slong_p, :libmpfr), Int32, (Ptr{Void}, Int32), x.mpfr, ROUNDING_MODE[end])
    if fits != 0
        return ccall((:mpfr_get_si, :libmpfr), Int64, (Ptr{Void}, Int32), x.mpfr, ROUNDING_MODE[end])
    end
    z = BigInt()
    ccall((:mpfr_get_z, :libmpfr), Int32, (Ptr{Void}, Ptr{Void}, Int32), &(z.mpz), x.mpfr, ROUNDING_MODE[end])
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
    return ccall((:mpfr_inf_p, :libmpfr), Int32, (Ptr{Void},), x.mpfr) != 0
end

function isnan(x::MPFRFloat)
    return ccall((:mpfr_nan_p, :libmpfr), Int32, (Ptr{Void},), x.mpfr) != 0
end

function nextfloat(x::MPFRFloat)
    z = MPFRFloat(x)
   ccall((:mpfr_nextabove, :libmpfr), Int32, (Ptr{Void},), z.mpfr) != 0
   return z
end

function prevfloat(x::MPFRFloat)
    z = MPFRFloat(x)
   ccall((:mpfr_nextbelow, :libmpfr), Int32, (Ptr{Void},), z.mpfr) != 0
   return z
end

function with_precision(f::Function, precision::Integer)
    old_precision = get_precision()
    set_precision(precision)
    ret = f()
    set_precision(old_precision)
    return ret
end

function round(x::MPFRFloat, prec::Int)
    if prec < 1
        throw(DomainError())
    end
    prec = int(ceil(log2(10^prec)))
    z = MPFRFloat(x)
    ccall((:mpfr_prec_round, :libmpfr), Int32, (Ptr{Void}, Int, Int32), z.mpfr, prec, ROUNDING_MODE[end])
    return z
end

function string(x::MPFRFloat)
    lng = 128
    for i = 1:2
        z = Array(Uint8, lng)
        lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{Uint8}, Uint, Ptr{Uint8}, Ptr{Void}...), z, lng, "%.Re", x.mpfr)
        if lng < 128 || i == 2
            return bytestring(convert(Ptr{Uint8}, z[1:lng]))
        end
    end
end

show(io::IO, b::MPFRFloat) = print(io, string(b) * " with $(prec(b)) bits of precision")
showcompact(io::IO, b::MPFRFloat) = print(io, string(b))

end #module
