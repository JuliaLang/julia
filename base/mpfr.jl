# This file is a part of Julia. License is MIT: http://julialang.org/license

module MPFR

export
    BigFloat,
    setprecision,
    big_str

import
    Base: (*), +, -, /, <, <=, ==, >, >=, ^, besselj, besselj0, besselj1, bessely,
        bessely0, bessely1, ceil, cmp, convert, copysign, div,
        exp, exp2, exponent, factorial, floor, fma, hypot, isinteger,
        isfinite, isinf, isnan, ldexp, log, log2, log10, max, min, mod, modf,
        nextfloat, prevfloat, promote_rule, rem, rem2pi, round, show,
        sum, sqrt, string, print, trunc, precision, exp10, expm1,
        gamma, lgamma, digamma, erf, erfc, zeta, eta, log1p, airyai,
        eps, signbit, sin, cos, tan, sec, csc, cot, acos, asin, atan,
        cosh, sinh, tanh, sech, csch, coth, acosh, asinh, atanh, atan2,
        cbrt, typemax, typemin, unsafe_trunc, realmin, realmax, rounding,
        setrounding, maxintfloat, widen, significand, frexp, tryparse, iszero

import Base.Rounding: rounding_raw, setrounding_raw

import Base.GMP: ClongMax, CulongMax, CdoubleMax, Limb

import Base.Math.lgamma_r

function __init__()
    try
        # set exponent to full range by default
        set_emin!(get_emin_min())
        set_emax!(get_emax_max())
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module MPFR")
    end
end

const ROUNDING_MODE = Ref{Cint}(0)
const DEFAULT_PRECISION = [256]

# Basic type and initialization definitions

"""
    BigFloat(x)

Create an arbitrary precision floating point number. `x` may be an `Integer`, a `Float64` or
a `BigInt`. The usual mathematical operators are defined for this type, and results are
promoted to a `BigFloat`.

Note that because decimal literals are converted to floating point numbers when parsed,
`BigFloat(2.1)` may not yield what you expect. You may instead prefer to initialize
constants from strings via [`parse`](@ref), or using the `big` string literal.

```jldoctest
julia> BigFloat(2.1)
2.100000000000000088817841970012523233890533447265625000000000000000000000000000

julia> big"2.1"
2.099999999999999999999999999999999999999999999999999999999999999999999999999986
```
"""
type BigFloat <: AbstractFloat
    prec::Clong
    sign::Cint
    exp::Clong
    d::Ptr{Limb}

    function BigFloat()
        prec = precision(BigFloat)
        z = new(zero(Clong), zero(Cint), zero(Clong), C_NULL)
        ccall((:mpfr_init2,:libmpfr), Void, (Ptr{BigFloat}, Clong), &z, prec)
        finalizer(z, cglobal((:mpfr_clear, :libmpfr)))
        return z
    end

    # Not recommended for general use:
    function BigFloat(prec::Clong, sign::Cint, exp::Clong, d::Ptr{Void})
        new(prec, sign, exp, d)
    end
end

widen(::Type{Float64}) = BigFloat
widen(::Type{BigFloat}) = BigFloat

convert(::Type{BigFloat}, x::BigFloat) = x

# convert to BigFloat
for (fJ, fC) in ((:si,:Clong), (:ui,:Culong), (:d,:Float64))
    @eval begin
        function convert(::Type{BigFloat}, x::($fC))
            z = BigFloat()
            ccall(($(string(:mpfr_set_,fJ)), :libmpfr), Int32, (Ptr{BigFloat}, ($fC), Int32), &z, x, ROUNDING_MODE[])
            return z
        end
    end
end

function convert(::Type{BigFloat}, x::BigInt)
    z = BigFloat()
    ccall((:mpfr_set_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

convert(::Type{BigFloat}, x::Integer) = BigFloat(BigInt(x))

convert(::Type{BigFloat}, x::Union{Bool,Int8,Int16,Int32}) = BigFloat(convert(Clong,x))
convert(::Type{BigFloat}, x::Union{UInt8,UInt16,UInt32}) = BigFloat(convert(Culong,x))

convert(::Type{BigFloat}, x::Union{Float16,Float32}) = BigFloat(Float64(x))
convert(::Type{BigFloat}, x::Rational) = BigFloat(numerator(x)) / BigFloat(denominator(x))

function tryparse(::Type{BigFloat}, s::AbstractString, base::Int=0)
    z = BigFloat()
    err = ccall((:mpfr_set_str, :libmpfr), Int32, (Ptr{BigFloat}, Cstring, Int32, Int32), &z, s, base, ROUNDING_MODE[])
    err == 0 ? Nullable(z) : Nullable{BigFloat}()
end

convert(::Type{Rational}, x::BigFloat) = convert(Rational{BigInt}, x)
convert(::Type{AbstractFloat}, x::BigInt) = BigFloat(x)

# generic constructor with arbitrary precision:
"""
    BigFloat(x, prec::Int)

Create a representation of `x` as a `BigFloat` with precision `prec`.
"""
function BigFloat(x, prec::Int)
    setprecision(BigFloat, prec) do
        BigFloat(x)
    end
end

"""
    BigFloat(x, prec::Int, rounding::RoundingMode)

Create a representation of `x` as a `BigFloat` with precision `prec` and rounding mode `rounding`.
"""
function BigFloat(x, prec::Int, rounding::RoundingMode)
    setrounding(BigFloat, rounding) do
        BigFloat(x, prec)
    end
end

"""
    BigFloat(x, rounding::RoundingMode)

Create a representation of `x` as a `BigFloat` with the current global precision and rounding mode `rounding`.
"""
function BigFloat(x::Union{Integer, AbstractFloat, String}, rounding::RoundingMode)
    BigFloat(x, precision(BigFloat), rounding)
end

"""
    BigFloat(x::String)

Create a representation of the string `x` as a `BigFloat`.
"""
BigFloat(x::String) = parse(BigFloat, x)


## BigFloat -> Integer
function unsafe_cast(::Type{Int64}, x::BigFloat, ri::Cint)
    ccall((:__gmpfr_mpfr_get_sj,:libmpfr), Cintmax_t,
          (Ptr{BigFloat}, Cint), &x, ri)
end
function unsafe_cast(::Type{UInt64}, x::BigFloat, ri::Cint)
    ccall((:__gmpfr_mpfr_get_uj,:libmpfr), Cuintmax_t,
          (Ptr{BigFloat}, Cint), &x, ri)
end

function unsafe_cast{T<:Signed}(::Type{T}, x::BigFloat, ri::Cint)
    unsafe_cast(Int64, x, ri) % T
end
function unsafe_cast{T<:Unsigned}(::Type{T}, x::BigFloat, ri::Cint)
    unsafe_cast(UInt64, x, ri) % T
end

function unsafe_cast(::Type{BigInt}, x::BigFloat, ri::Cint)
    # actually safe, just keep naming consistent
    z = BigInt()
    ccall((:mpfr_get_z, :libmpfr), Int32, (Ptr{BigInt}, Ptr{BigFloat}, Int32),
          &z, &x, ri)
    z
end
unsafe_cast(::Type{Int128}, x::BigFloat, ri::Cint) = Int128(unsafe_cast(BigInt,x,ri))
unsafe_cast(::Type{UInt128}, x::BigFloat, ri::Cint) = UInt128(unsafe_cast(BigInt,x,ri))
unsafe_cast{T<:Integer}(::Type{T}, x::BigFloat, r::RoundingMode) = unsafe_cast(T,x,to_mpfr(r))

unsafe_trunc{T<:Integer}(::Type{T}, x::BigFloat) = unsafe_cast(T,x,RoundToZero)

function trunc{T<:Union{Signed,Unsigned}}(::Type{T}, x::BigFloat)
    (typemin(T) <= x <= typemax(T)) || throw(InexactError())
    unsafe_cast(T,x,RoundToZero)
end
function floor{T<:Union{Signed,Unsigned}}(::Type{T}, x::BigFloat)
    (typemin(T) <= x <= typemax(T)) || throw(InexactError())
    unsafe_cast(T,x,RoundDown)
end
function ceil{T<:Union{Signed,Unsigned}}(::Type{T}, x::BigFloat)
    (typemin(T) <= x <= typemax(T)) || throw(InexactError())
    unsafe_cast(T,x,RoundUp)
end

function round{T<:Union{Signed,Unsigned}}(::Type{T}, x::BigFloat)
    (typemin(T) <= x <= typemax(T)) || throw(InexactError())
    unsafe_cast(T,x,ROUNDING_MODE[])
end

trunc(::Type{BigInt}, x::BigFloat) = unsafe_cast(BigInt, x, RoundToZero)
floor(::Type{BigInt}, x::BigFloat) = unsafe_cast(BigInt, x, RoundDown)
ceil(::Type{BigInt}, x::BigFloat) = unsafe_cast(BigInt, x, RoundUp)
round(::Type{BigInt}, x::BigFloat) = unsafe_cast(BigInt, x, ROUNDING_MODE[])

# convert/round/trunc/floor/ceil(Integer, x) should return a BigInt
trunc(::Type{Integer}, x::BigFloat) = trunc(BigInt, x)
floor(::Type{Integer}, x::BigFloat) = floor(BigInt, x)
ceil(::Type{Integer}, x::BigFloat) = ceil(BigInt, x)
round(::Type{Integer}, x::BigFloat) = round(BigInt, x)

convert(::Type{Bool}, x::BigFloat) = x==0 ? false : x==1 ? true : throw(InexactError())
function convert(::Type{BigInt},x::BigFloat)
    isinteger(x) || throw(InexactError())
    trunc(BigInt,x)
end

function convert(::Type{Integer},x::BigFloat)
    isinteger(x) || throw(InexactError())
    trunc(Integer,x)
end
function convert{T<:Integer}(::Type{T},x::BigFloat)
    isinteger(x) || throw(InexactError())
    trunc(T,x)
end

## BigFloat -> AbstractFloat
convert(::Type{Float64}, x::BigFloat) =
    ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{BigFloat},Int32), &x, ROUNDING_MODE[])
convert(::Type{Float32}, x::BigFloat) =
    ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{BigFloat},Int32), &x, ROUNDING_MODE[])
# TODO: avoid double rounding
convert(::Type{Float16}, x::BigFloat) = convert(Float16, convert(Float32, x))

(::Type{Float64})(x::BigFloat, r::RoundingMode) =
    ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{BigFloat},Int32), &x, to_mpfr(r))
(::Type{Float32})(x::BigFloat, r::RoundingMode) =
    ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{BigFloat},Int32), &x, to_mpfr(r))
# TODO: avoid double rounding
(::Type{Float16})(x::BigFloat, r::RoundingMode) =
    convert(Float16, Float32(x, r))

promote_rule{T<:Real}(::Type{BigFloat}, ::Type{T}) = BigFloat
promote_rule{T<:AbstractFloat}(::Type{BigInt},::Type{T}) = BigFloat
promote_rule{T<:AbstractFloat}(::Type{BigFloat},::Type{T}) = BigFloat

function convert(::Type{Rational{BigInt}}, x::AbstractFloat)
    if isnan(x); return zero(BigInt)//zero(BigInt); end
    if isinf(x); return copysign(one(BigInt),x)//zero(BigInt); end
    if x == 0;   return zero(BigInt) // one(BigInt); end
    s = max(precision(x) - exponent(x), 0)
    BigInt(ldexp(x,s)) // (BigInt(1) << s)
end

# Basic arithmetic without promotion
for (fJ, fC) in ((:+,:add), (:*,:mul))
    @eval begin
        # BigFloat
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
            return z
        end

        # Unsigned Integer
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        ($fJ)(c::CulongMax, x::BigFloat) = ($fJ)(x,c)

        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        ($fJ)(c::ClongMax, x::BigFloat) = ($fJ)(x,c)

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cdouble, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        ($fJ)(c::CdoubleMax, x::BigFloat) = ($fJ)(x,c)

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[])
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
            ccall(($(string(:mpfr_,fC)),:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
            return z
        end

        # Unsigned Int
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        function ($fJ)(c::CulongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:ui_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Culong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[])
            return z
        end

        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        function ($fJ)(c::ClongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:si_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[])
            return z
        end

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cdouble, Int32), &z, &x, c, ROUNDING_MODE[])
            return z
        end
        function ($fJ)(c::CdoubleMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:d_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Cdouble, Ptr{BigFloat}, Int32), &z, c, &x, ROUNDING_MODE[])
            return z
        end

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, ROUNDING_MODE[])
            return z
        end
        # no :mpfr_z_div function
    end
end

function -(c::BigInt, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_z_sub, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigInt}, Ptr{BigFloat}, Int32), &z, &c, &x, ROUNDING_MODE[])
    return z
end

function fma(x::BigFloat, y::BigFloat, z::BigFloat)
    r = BigFloat()
    ccall(("mpfr_fma",:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &r, &x, &y, &z, ROUNDING_MODE[])
    return r
end

# div
# BigFloat
function div(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_div,:libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end

# Unsigned Int
function div(x::BigFloat, c::CulongMax)
    z = BigFloat()
    ccall((:mpfr_div_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, c, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end
function div(c::CulongMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ui_div, :libmpfr), Int32, (Ptr{BigFloat}, Culong, Ptr{BigFloat}, Int32), &z, c, &x, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end

# Signed Integer
function div(x::BigFloat, c::ClongMax)
    z = BigFloat()
    ccall((:mpfr_div_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, c, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end
function div(c::ClongMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_si_div, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, c, &x, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end

# Float32/Float64
function div(x::BigFloat, c::CdoubleMax)
    z = BigFloat()
    ccall((:mpfr_div_d, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cdouble, Int32), &z, &x, c, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end
function div(c::CdoubleMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_d_div, :libmpfr), Int32, (Ptr{BigFloat}, Cdouble, Ptr{BigFloat}, Int32), &z, c, &x, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end

# BigInt
function div(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_div_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &c, to_mpfr(RoundToZero))
    ccall((:mpfr_trunc, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &z)
    return z
end


# More efficient commutative operations
for (fJ, fC, fI) in ((:+, :add, 0), (:*, :mul, 1))
    @eval begin
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[])
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &d, ROUNDING_MODE[])
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat, e::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &a, &b, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &c, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &d, ROUNDING_MODE[])
            ccall(($(string(:mpfr_,fC)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &z, &e, ROUNDING_MODE[])
            return z
        end
    end
end

function -(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_neg, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function sqrt(x::BigFloat)
    isnan(x) && return x
    z = BigFloat()
    ccall((:mpfr_sqrt, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    if isnan(z)
        throw(DomainError())
    end
    return z
end

sqrt(x::BigInt) = sqrt(BigFloat(x))

function ^(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_pow, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

function ^(x::BigFloat, y::CulongMax)
    z = BigFloat()
    ccall((:mpfr_pow_ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, y, ROUNDING_MODE[])
    return z
end

function ^(x::BigFloat, y::ClongMax)
    z = BigFloat()
    ccall((:mpfr_pow_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, y, ROUNDING_MODE[])
    return z
end

function ^(x::BigFloat, y::BigInt)
    z = BigFloat()
    ccall((:mpfr_pow_z, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigInt}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

^(x::BigFloat, y::Integer)  = typemin(Clong)  <= y <= typemax(Clong)  ? x^Clong(y)  : x^BigInt(y)
^(x::BigFloat, y::Unsigned) = typemin(Culong) <= y <= typemax(Culong) ? x^Culong(y) : x^BigInt(y)

for f in (:exp, :exp2, :exp10, :expm1, :digamma, :erf, :erfc, :zeta,
          :cosh,:sinh,:tanh,:sech,:csch,:coth, :cbrt)
    @eval function $f(x::BigFloat)
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
        return z
    end
end

# return log(2)
function big_ln2()
    c = BigFloat()
    ccall((:mpfr_const_log2, :libmpfr), Cint, (Ptr{BigFloat}, Int32),
          &c, MPFR.ROUNDING_MODE[])
    return c
end

function eta(x::BigFloat)
    x == 1 && return big_ln2()
    return -zeta(x) * expm1(big_ln2()*(1-x))
end

function airyai(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ai, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function ldexp(x::BigFloat, n::Clong)
    z = BigFloat()
    ccall((:mpfr_mul_2si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &x, n, ROUNDING_MODE[])
    return z
end
function ldexp(x::BigFloat, n::Culong)
    z = BigFloat()
    ccall((:mpfr_mul_2ui, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Culong, Int32), &z, &x, n, ROUNDING_MODE[])
    return z
end
ldexp(x::BigFloat, n::ClongMax) = ldexp(x, convert(Clong, n))
ldexp(x::BigFloat, n::CulongMax) = ldexp(x, convert(Culong, n))
ldexp(x::BigFloat, n::Integer) = x*exp2(BigFloat(n))

function besselj0(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_j0, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function besselj1(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_j1, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function besselj(n::Integer, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_jn, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, n, &x, ROUNDING_MODE[])
    return z
end

function bessely0(x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_y0, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function bessely1(x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_y1, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function bessely(n::Integer, x::BigFloat)
    if x < 0
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_yn, :libmpfr), Int32, (Ptr{BigFloat}, Clong, Ptr{BigFloat}, Int32), &z, n, &x, ROUNDING_MODE[])
    return z
end

function factorial(x::BigFloat)
    if x < 0 || !isinteger(x)
        throw(DomainError())
    end
    ui = convert(Culong, x)
    z = BigFloat()
    ccall((:mpfr_fac_ui, :libmpfr), Int32, (Ptr{BigFloat}, Culong, Int32), &z, ui, ROUNDING_MODE[])
    return z
end

function hypot(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_hypot, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

for f in (:log, :log2, :log10)
    @eval function $f(x::BigFloat)
        if x < 0
            throw(DomainError())
        end
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
        return z
    end
end

function log1p(x::BigFloat)
    if x < -1
        throw(DomainError())
    end
    z = BigFloat()
    ccall((:mpfr_log1p, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
    return z
end

function max(x::BigFloat, y::BigFloat)
    isnan(x) && return x
    isnan(y) && return y
    z = BigFloat()
    ccall((:mpfr_max, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

function min(x::BigFloat, y::BigFloat)
    isnan(x) && return x
    isnan(y) && return y
    z = BigFloat()
    ccall((:mpfr_min, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

function modf(x::BigFloat)
    if isinf(x)
        return (BigFloat(NaN), x)
    end
    zint = BigFloat()
    zfloat = BigFloat()
    ccall((:mpfr_modf, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &zint, &zfloat, &x, ROUNDING_MODE[])
    return (zfloat, zint)
end

function rem(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_fmod, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

function rem(x::BigFloat, y::BigFloat, ::RoundingMode{:Nearest})
    z = BigFloat()
    ccall((:mpfr_remainder, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
    return z
end

# TODO: use a higher-precision BigFloat(pi) here?
rem2pi(x::BigFloat, r::RoundingMode) = rem(x, 2*BigFloat(pi), r)

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
            ccall(($(string(:mpfr_,f)), :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, ROUNDING_MODE[])
            if isnan(z)
                throw(DomainError())
            end
            return z
        end
    end
end

# log of absolute value of gamma function
const lgamma_signp = Array{Cint}(1)
function lgamma(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_lgamma,:libmpfr), Cint, (Ptr{BigFloat}, Ptr{Cint}, Ptr{BigFloat}, Int32), &z, lgamma_signp, &x, ROUNDING_MODE[])
    return z
end

lgamma_r(x::BigFloat) = (lgamma(x), lgamma_signp[1])

function atan2(y::BigFloat, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_atan2, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &y, &x, ROUNDING_MODE[])
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

function precision(x::BigFloat)  # precision of an object of type BigFloat
    return ccall((:mpfr_get_prec, :libmpfr), Clong, (Ptr{BigFloat},), &x)
end

"""
    precision(BigFloat)

Get the precision (in bits) currently used for `BigFloat` arithmetic.
"""
precision(::Type{BigFloat}) = DEFAULT_PRECISION[end]  # precision of the type BigFloat itself

"""
    setprecision([T=BigFloat,] precision::Int)

Set the precision (in bits) to be used for `T` arithmetic.
"""
function setprecision(::Type{BigFloat}, precision::Int)
    if precision < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[end] = precision
end

setprecision(precision::Int) = setprecision(BigFloat, precision)

maxintfloat(x::BigFloat) = BigFloat(2)^precision(x)
maxintfloat(::Type{BigFloat}) = BigFloat(2)^precision(BigFloat)

to_mpfr(::RoundingMode{:Nearest}) = Cint(0)
to_mpfr(::RoundingMode{:ToZero}) = Cint(1)
to_mpfr(::RoundingMode{:Up}) = Cint(2)
to_mpfr(::RoundingMode{:Down}) = Cint(3)
to_mpfr(::RoundingMode{:FromZero}) = Cint(4)

function from_mpfr(c::Integer)
    if c == 0
        return RoundNearest
    elseif c == 1
        return RoundToZero
    elseif c == 2
        return RoundUp
    elseif c == 3
        return RoundDown
    elseif c == 4
        return RoundFromZero
    else
        throw(ArgumentError("invalid MPFR rounding mode code: $c"))
    end
    RoundingMode(c)
end

rounding_raw(::Type{BigFloat}) = ROUNDING_MODE[]
setrounding_raw(::Type{BigFloat},i::Integer) = ROUNDING_MODE[] = i

rounding(::Type{BigFloat}) = from_mpfr(rounding_raw(BigFloat))
setrounding(::Type{BigFloat},r::RoundingMode) = setrounding_raw(BigFloat,to_mpfr(r))

function copysign(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_copysign, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Ptr{BigFloat}, Int32), &z, &x, &y, ROUNDING_MODE[])
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
    c = Ref{Clong}()
    ccall((:mpfr_frexp, :libmpfr), Int32, (Ptr{Clong}, Ptr{BigFloat}, Ptr{BigFloat}, Cint), c, &z, &x, ROUNDING_MODE[])
    return (z, c[])
end

function significand(x::BigFloat)
    z = BigFloat()
    c = Ref{Clong}()
    ccall((:mpfr_frexp, :libmpfr), Int32, (Ptr{Clong}, Ptr{BigFloat}, Ptr{BigFloat}, Cint), c, &z, &x, ROUNDING_MODE[])
    # Double the significand to make it work as Base.significand
    ccall((:mpfr_mul_si, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Clong, Int32), &z, &z, 2, ROUNDING_MODE[])
    return z
end

function isinteger(x::BigFloat)
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

function round(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_rint, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Cint), &z, &x, ROUNDING_MODE[])
    return z
end
function round(x::BigFloat,::RoundingMode{:NearestTiesAway})
    z = BigFloat()
    ccall((:mpfr_round, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}), &z, &x)
    return z
end

function isinf(x::BigFloat)
    return ccall((:mpfr_inf_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

function isnan(x::BigFloat)
    return ccall((:mpfr_nan_p, :libmpfr), Int32, (Ptr{BigFloat},), &x) != 0
end

isfinite(x::BigFloat) = !isinf(x) && !isnan(x)

iszero(x::BigFloat) = x == Clong(0)

@eval typemax(::Type{BigFloat}) = $(BigFloat( Inf))
@eval typemin(::Type{BigFloat}) = $(BigFloat(-Inf))

function nextfloat(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, &x, ROUNDING_MODE[])
    ccall((:mpfr_nextabove, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
    return z
end

function prevfloat(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, &x, ROUNDING_MODE[])
    ccall((:mpfr_nextbelow, :libmpfr), Int32, (Ptr{BigFloat},), &z) != 0
    return z
end

eps(::Type{BigFloat}) = nextfloat(BigFloat(1)) - BigFloat(1)

realmin(::Type{BigFloat}) = nextfloat(zero(BigFloat))
realmax(::Type{BigFloat}) = prevfloat(BigFloat(Inf))

"""
    setprecision(f::Function, [T=BigFloat,] precision::Integer)

Change the `T` arithmetic precision (in bits) for the duration of `f`.
It is logically equivalent to:

    old = precision(BigFloat)
    setprecision(BigFloat, precision)
    f()
    setprecision(BigFloat, old)

Often used as `setprecision(T, precision) do ... end`
"""
function setprecision{T}(f::Function, ::Type{T}, prec::Integer)
    old_prec = precision(T)
    setprecision(T, prec)
    try
        return f()
    finally
        setprecision(T, old_prec)
    end
end

setprecision(f::Function, precision::Integer) = setprecision(f, BigFloat, precision)

function string(x::BigFloat)
    # In general, the number of decimal places needed to read back the number exactly
    # is, excluding the most significant, ceil(log(10, 2^precision(x)))
    k = ceil(Int32, precision(x) * 0.3010299956639812)
    lng = k + Int32(8) # Add space for the sign, the most significand digit, the dot and the exponent
    buf = Base.StringVector(lng + 1)
    # format strings are guaranteed to contain no NUL, so we don't use Cstring
    lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{UInt8}, Culong, Ptr{UInt8}, Ptr{BigFloat}...), buf, lng + 1, "%.Re", &x)
    if lng < k + 5 # print at least k decimal places
        lng = ccall((:mpfr_sprintf,:libmpfr), Int32, (Ptr{UInt8}, Ptr{UInt8}, Ptr{BigFloat}...), buf, "%.$(k)Re", &x)
    elseif lng > k + 8
        buf = Base.StringVector(lng + 1)
        lng = ccall((:mpfr_snprintf,:libmpfr), Int32, (Ptr{UInt8}, Culong, Ptr{UInt8}, Ptr{BigFloat}...), buf, lng + 1, "%.Re", &x)
    end
    n = (1 <= x < 10 || -10 < x <= -1 || x == 0) ? lng - 4 : lng
    return String(resize!(buf,n))
end

print(io::IO, b::BigFloat) = print(io, string(b))
show(io::IO, b::BigFloat) = print(io, string(b))

# get/set exponent min/max
get_emax() = ccall((:mpfr_get_emax, :libmpfr), Clong, ())
get_emax_min() = ccall((:mpfr_get_emax_min, :libmpfr), Clong, ())
get_emax_max() = ccall((:mpfr_get_emax_max, :libmpfr), Clong, ())

get_emin() = ccall((:mpfr_get_emin, :libmpfr), Clong, ())
get_emin_min() = ccall((:mpfr_get_emin_min, :libmpfr), Clong, ())
get_emin_max() = ccall((:mpfr_get_emin_max, :libmpfr), Clong, ())

set_emax!(x) = ccall((:mpfr_set_emax, :libmpfr), Void, (Clong,), x)
set_emin!(x) = ccall((:mpfr_set_emin, :libmpfr), Void, (Clong,), x)

function Base.deepcopy_internal(x::BigFloat, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    N = precision(x)
    y = BigFloat(zero(Clong), zero(Cint), zero(Clong), C_NULL)
    ccall((:mpfr_init2,:libmpfr), Void, (Ptr{BigFloat}, Clong), &y, N)
    finalizer(y, cglobal((:mpfr_clear, :libmpfr)))
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &y, &x, ROUNDING_MODE[])
    stackdict[x] = y
    return y
end

function Base.lerpi(j::Integer, d::Integer, a::BigFloat, b::BigFloat)
    t = BigFloat(j)/d
    fma(t, b, fma(-t, a, a))
end

end #module
