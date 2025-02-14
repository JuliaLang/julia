# This file is a part of Julia. License is MIT: https://julialang.org/license

module MPFR

export
    BigFloat,
    setprecision

import
    .Base: *, +, -, /, <, <=, ==, >, >=, ^, ceil, cmp, convert, copysign, div,
        inv, exp, exp2, exponent, factorial, floor, fma, muladd, hypot, isinteger,
        isfinite, isinf, isnan, ldexp, log, log2, log10, max, min, mod, modf,
        nextfloat, prevfloat, promote_rule, rem, rem2pi, round, show, float,
        sum, sqrt, string, print, trunc, precision, _precision, exp10, expm1, log1p,
        eps, signbit, sign, sin, cos, sincos, tan, sec, csc, cot, acos, asin, atan,
        cosh, sinh, tanh, sech, csch, coth, acosh, asinh, atanh, lerpi,
        cbrt, typemax, typemin, unsafe_trunc, floatmin, floatmax, rounding,
        setrounding, maxintfloat, widen, significand, frexp, tryparse, iszero,
        isone, big, _string_n, decompose, minmax, _precision_with_base_2,
        sinpi, cospi, sincospi, tanpi, sind, cosd, tand, asind, acosd, atand,
        uinttype, exponent_max, exponent_min, ieee754_representation, significand_mask

import .Core: AbstractFloat
import .Base: Rational, Float16, Float32, Float64, Bool

using .Base.Libc
import ..Rounding: Rounding,
    rounding_raw, setrounding_raw, rounds_to_nearest, rounds_away_from_zero,
    tie_breaker_is_to_even, correct_rounding_requires_increment

import ..GMP: ClongMax, CulongMax, CdoubleMax, Limb, libgmp, BigInt

import ..FastMath.sincos_fast

if Sys.iswindows()
    const libmpfr = "libmpfr-6.dll"
elseif Sys.isapple()
    const libmpfr = "@rpath/libmpfr.6.dylib"
else
    const libmpfr = "libmpfr.so.6"
end

version() = VersionNumber(unsafe_string(ccall((:mpfr_get_version,libmpfr), Ptr{Cchar}, ())))
patches() = split(unsafe_string(ccall((:mpfr_get_patches,libmpfr), Ptr{Cchar}, ())),' ')

function __init__()
    try
        # set exponent to full range by default
        set_emin!(get_emin_min())
        set_emax!(get_emax_max())
    catch ex
        Base.showerror_nostdio(ex, "WARNING: Error during initialization of module MPFR")
    end
    nothing
end

"""
    MPFR.MPFRRoundingMode

Matches the `mpfr_rnd_t` enum provided by MPFR, see
https://www.mpfr.org/mpfr-current/mpfr.html#Rounding-Modes

This is for internal use, and ensures that `ROUNDING_MODE[]` is type-stable.
"""
@enum MPFRRoundingMode begin
    MPFRRoundNearest
    MPFRRoundToZero
    MPFRRoundUp
    MPFRRoundDown
    MPFRRoundFromZero
    MPFRRoundFaithful
end

convert(::Type{MPFRRoundingMode}, ::RoundingMode{:Nearest})  = MPFRRoundNearest
convert(::Type{MPFRRoundingMode}, ::RoundingMode{:ToZero})   = MPFRRoundToZero
convert(::Type{MPFRRoundingMode}, ::RoundingMode{:Up})       = MPFRRoundUp
convert(::Type{MPFRRoundingMode}, ::RoundingMode{:Down})     = MPFRRoundDown
convert(::Type{MPFRRoundingMode}, ::RoundingMode{:FromZero}) = MPFRRoundFromZero

function convert(::Type{RoundingMode}, r::MPFRRoundingMode)
    if r == MPFRRoundNearest
        return RoundNearest
    elseif r == MPFRRoundToZero
        return RoundToZero
    elseif r == MPFRRoundUp
        return RoundUp
    elseif r == MPFRRoundDown
        return RoundDown
    elseif r == MPFRRoundFromZero
        return RoundFromZero
    else
        throw(ArgumentError("invalid MPFR rounding mode code: $r"))
    end
end

rounds_to_nearest(m::MPFRRoundingMode) = m == MPFRRoundNearest
function rounds_away_from_zero(m::MPFRRoundingMode, sign_bit::Bool)
    if m == MPFRRoundToZero
        false
    elseif m == MPFRRoundUp
        !sign_bit
    elseif m == MPFRRoundDown
        sign_bit
    else
        # Assuming `m == MPFRRoundFromZero`
        true
    end
end
tie_breaker_is_to_even(::MPFRRoundingMode) = true

const ROUNDING_MODE = Ref{MPFRRoundingMode}(MPFRRoundNearest)
const CURRENT_ROUNDING_MODE = Base.ScopedValues.ScopedValue{MPFRRoundingMode}()
const DEFAULT_PRECISION = Ref{Clong}(256)
const CURRENT_PRECISION = Base.ScopedValues.ScopedValue{Clong}()
# Basic type and initialization definitions

# Warning: the constants are MPFR implementation details from
# `src/mpfr-impl.h`, search for `MPFR_EXP_ZERO`.
const mpfr_special_exponent_zero = typemin(Clong) + true
const mpfr_special_exponent_nan = mpfr_special_exponent_zero + true
const mpfr_special_exponent_inf = mpfr_special_exponent_nan + true

struct BigFloatLayout
    prec::Clong
    sign::Cint
    exp::Clong
    d::Ptr{Limb}
    # possible padding
    p::Limb # Tuple{Vararg{Limb}}
end
const offset_prec = fieldoffset(BigFloatLayout, 1) % Int
const offset_sign = fieldoffset(BigFloatLayout, 2) % Int
const offset_exp = fieldoffset(BigFloatLayout, 3) % Int
const offset_d = fieldoffset(BigFloatLayout, 4) % Int
const offset_p_limbs = ((fieldoffset(BigFloatLayout, 5) % Int + sizeof(Limb) - 1) ÷ sizeof(Limb))
const offset_p = offset_p_limbs * sizeof(Limb)

"""
    BigFloat <: AbstractFloat

Arbitrary precision floating point number type.
"""
struct BigFloat <: AbstractFloat
    d::Memory{Limb}

    # Not recommended for general use:
    # used internally by, e.g. deepcopy
    global function _BigFloat(d::Memory{Limb})
        Base.unsafe_convert(Ref{BigFloat}, BigFloatData(d)) # force early initialization of pointer field of z.d
        return new(d)
    end

    function BigFloat(; precision::Integer=_precision_with_base_2(BigFloat))
        precision < 1 && throw(DomainError(precision, "`precision` cannot be less than 1."))
        nb = ccall((:mpfr_custom_get_size,libmpfr), Csize_t, (Clong,), precision)
        nl = (nb + offset_p + sizeof(Limb) - 1) ÷ Core.sizeof(Limb) # align to number of Limb allocations required for this
        d = Memory{Limb}(undef, nl % Int)
        # ccall-based version, inlined below
        #ccall((:mpfr_custom_init,libmpfr), Cvoid, (Ptr{Limb}, Clong), BigFloatData(d), prec) # currently seems to be a no-op in mpfr
        #NAN_KIND = Cint(0)
        #ccall((:mpfr_custom_init_set,libmpfr), Cvoid, (Ref{BigFloat}, Cint, Clong, Ptr{Limb}), z, NAN_KIND, prec, BigFloatData(d))
        p = Base.unsafe_convert(Ptr{Limb}, d)
        GC.@preserve d begin # initialize to +NAN
            unsafe_store!(Ptr{Clong}(p) + offset_prec, Clong(precision))
            unsafe_store!(Ptr{Cint}(p) + offset_sign, one(Cint))
            unsafe_store!(Ptr{Clong}(p) + offset_exp, mpfr_special_exponent_nan)
            unsafe_store!(Ptr{Ptr{Limb}}(p) + offset_d, p + offset_p)
        end
        return new(d)
    end
end

"""
Segment of raw words of bits interpreted as a big integer. Less
significant words come first. Each word is in machine-native bit-order.
"""
struct BigFloatData{Limb}
    d::Memory{Limb}
end

# BigFloat interface
@inline function Base.getproperty(x::BigFloat, s::Symbol)
    d = getfield(x, :d)
    p = Base.unsafe_convert(Ptr{Limb}, d)
    if s === :prec
        return GC.@preserve d unsafe_load(Ptr{Clong}(p) + offset_prec)
    elseif s === :sign
        return GC.@preserve d unsafe_load(Ptr{Cint}(p) + offset_sign)
    elseif s === :exp
        return GC.@preserve d unsafe_load(Ptr{Clong}(p) + offset_exp)
    elseif s === :d
        return BigFloatData(d)
    else
        return throw(FieldError(typeof(x), s))
    end
end

# While BigFloat (like all Numbers) is considered immutable, for practical reasons
# of writing the algorithms on it we allow mutating sign, exp, and the contents of d
@inline function Base.setproperty!(x::BigFloat, s::Symbol, v)
    d = getfield(x, :d)
    p = Base.unsafe_convert(Ptr{Limb}, d)
    if s === :sign
        return GC.@preserve d unsafe_store!(Ptr{Cint}(p) + offset_sign, v)
    elseif s === :exp
        return GC.@preserve d unsafe_store!(Ptr{Clong}(p) + offset_exp, v)
    #elseif s === :d || s === :prec # not mutable
    else
        return throw(FieldError(x, s))
    end
end

# Ref interface: make sure the conversion to C is done properly
Base.unsafe_convert(::Type{Ref{BigFloat}}, x::Ptr{BigFloat}) = error("not compatible with mpfr")
Base.unsafe_convert(::Type{Ref{BigFloat}}, x::Ref{BigFloat}) = error("not compatible with mpfr")
Base.cconvert(::Type{Ref{BigFloat}}, x::BigFloat) = x.d # BigFloatData is the Ref type for BigFloat
Base.cconvert(::Type{Ref{BigFloat}}, x::Number) = convert(BigFloat, x).d # avoid default conversion to Ref(BigFloat(x))
Base.cconvert(::Type{Ref{BigFloat}}, x::Ref{BigFloat}) = x[].d
function Base.unsafe_convert(::Type{Ref{BigFloat}}, x::BigFloatData)
    d = getfield(x, :d)
    p = Base.unsafe_convert(Ptr{Limb}, d)
    dptrptr = Ptr{Ptr{Limb}}(p) + offset_d
    dptr = p + offset_p
    GC.@preserve d if unsafe_load(dptrptr, :monotonic) != dptr # make sure this pointer value was recomputed after any deserialization or copying
        unsafe_store!(dptrptr, dptr, :monotonic) # :monotonic ensure that TSAN knows that this isn't a data race
    end
    return Ptr{BigFloat}(p)
end
Base.unsafe_convert(::Type{Ptr{Limb}}, fd::BigFloatData) = Base.unsafe_convert(Ptr{Limb}, getfield(fd, :d)) + offset_p
function Base.setindex!(fd::BigFloatData, v, i)
    d = getfield(fd, :d)
    @boundscheck 1 <= i <= length(d) - offset_p_limbs || throw(BoundsError(fd, i))
    @inbounds d[i + offset_p_limbs] = v
    return fd
end
function Base.getindex(fd::BigFloatData, i)
    d = getfield(fd, :d)
    @boundscheck 1 <= i <= length(d) - offset_p_limbs || throw(BoundsError(fd, i))
    @inbounds d[i + offset_p_limbs]
end
Base.length(fd::BigFloatData) = length(getfield(fd, :d)) - offset_p_limbs
Base.copyto!(fd::BigFloatData, limbs) = copyto!(getfield(fd, :d), offset_p_limbs + 1, limbs) # for Random

include("rawbigfloats.jl")

rounding_raw(::Type{BigFloat}) = something(Base.ScopedValues.get(CURRENT_ROUNDING_MODE), ROUNDING_MODE[])
setrounding_raw(::Type{BigFloat}, r::MPFRRoundingMode) = ROUNDING_MODE[]=r
function setrounding_raw(f::Function, ::Type{BigFloat}, r::MPFRRoundingMode)
    Base.ScopedValues.@with(CURRENT_ROUNDING_MODE => r, f())
end

rounding(::Type{BigFloat}) = convert(RoundingMode, rounding_raw(BigFloat))
setrounding(::Type{BigFloat}, r::RoundingMode) = setrounding_raw(BigFloat, convert(MPFRRoundingMode, r))
setrounding(f::Function, ::Type{BigFloat}, r::RoundingMode) =
    setrounding_raw(f, BigFloat, convert(MPFRRoundingMode, r))


"""
    BigFloat(x::Union{Real, AbstractString} [, rounding::RoundingMode=rounding(BigFloat)]; [precision::Integer=precision(BigFloat)])

Create an arbitrary precision floating point number from `x`, with precision
`precision`. The `rounding` argument specifies the direction in which the result should be
rounded if the conversion cannot be done exactly. If not provided, these are set by the current global values.

`BigFloat(x::Real)` is the same as `convert(BigFloat,x)`, except if `x` itself is already
`BigFloat`, in which case it will return a value with the precision set to the current
global precision; `convert` will always return `x`.

`BigFloat(x::AbstractString)` is identical to [`parse`](@ref). This is provided for
convenience since decimal literals are converted to `Float64` when parsed, so
`BigFloat(2.1)` may not yield what you expect.

See also:
- [`@big_str`](@ref)
- [`rounding`](@ref) and [`setrounding`](@ref)
- [`precision`](@ref) and [`setprecision`](@ref)

!!! compat "Julia 1.1"
    `precision` as a keyword argument requires at least Julia 1.1.
    In Julia 1.0 `precision` is the second positional argument (`BigFloat(x, precision)`).

# Examples
```jldoctest
julia> BigFloat(2.1) # 2.1 here is a Float64
2.100000000000000088817841970012523233890533447265625

julia> BigFloat("2.1") # the closest BigFloat to 2.1
2.099999999999999999999999999999999999999999999999999999999999999999999999999986

julia> BigFloat("2.1", RoundUp)
2.100000000000000000000000000000000000000000000000000000000000000000000000000021

julia> BigFloat("2.1", RoundUp, precision=128)
2.100000000000000000000000000000000000007
```
"""
BigFloat(x, r::RoundingMode)

widen(::Type{Float64}) = BigFloat
widen(::Type{BigFloat}) = BigFloat

function BigFloat(x::BigFloat, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat))
    if precision == _precision_with_base_2(x)
        return x
    else
        z = BigFloat(;precision=precision)
        ccall((:mpfr_set, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode),
              z, x, r)
        return z
    end
end

function _duplicate(x::BigFloat)
    z = BigFloat(;precision=_precision_with_base_2(x))
    ccall((:mpfr_set, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Int32), z, x, 0)
    return z
end

# convert to BigFloat
for (fJ, fC) in ((:si,:Clong), (:ui,:Culong))
    @eval begin
        function BigFloat(x::($fC), r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat))
            z = BigFloat(;precision=precision)
            ccall(($(string(:mpfr_set_,fJ)), libmpfr), Int32, (Ref{BigFloat}, $fC, MPFRRoundingMode), z, x, r)
            return z
        end
    end
end

function BigFloat(x::Float64, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat))
    z = BigFloat(;precision)
    # punt on the hard case where we might have to deal with rounding
    # we could use this path in all cases, but mpfr_set_d has a lot of overhead.
    if precision <= Base.significand_bits(Float64)
        ccall((:mpfr_set_d, libmpfr), Int32, (Ref{BigFloat}, Float64, MPFRRoundingMode), z, x, r)
        if isnan(x) && signbit(x) != signbit(z)
            z.sign = -z.sign
        end
        return z
    end
    z.sign = 1-2*signbit(x)
    if iszero(x) || !isfinite(x)
        if isinf(x)
            z.exp = mpfr_special_exponent_inf
        elseif isnan(x)
            z.exp = mpfr_special_exponent_nan
        else
            z.exp = mpfr_special_exponent_zero
        end
        return z
    end
    z.exp = 1 + exponent(x)
    # BigFloat doesn't have an implicit bit
    val = reinterpret(UInt64, significand(x))<<11 | typemin(Int64)
    nlimbs = (precision + 8*Core.sizeof(Limb) - 1) ÷ (8*Core.sizeof(Limb))

    # Limb is a CLong which is a UInt32 on windows (thank M$) which makes this more complicated and slower.
    zd = z.d
    if Limb === UInt64
        for i in 1:nlimbs-1
            @inbounds setindex!(zd, 0x0, i)
        end
        @inbounds setindex!(zd, val, nlimbs)
    else
        for i in 1:nlimbs-2
            @inbounds setindex!(zd, 0x0, i)
        end
        @inbounds setindex!(zd, val % UInt32, nlimbs-1)
        @inbounds setindex!(zd, (val >> 32) % UInt32, nlimbs)
    end
    z
end

function BigFloat(x::BigInt, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat))
    z = BigFloat(;precision=precision)
    ccall((:mpfr_set_z, libmpfr), Int32, (Ref{BigFloat}, Ref{BigInt}, MPFRRoundingMode), z, x, r)
    return z
end

BigFloat(x::Integer; precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(BigInt(x)::BigInt, rounding_raw(BigFloat); precision=precision)
BigFloat(x::Integer, r::MPFRRoundingMode; precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(BigInt(x)::BigInt, r; precision=precision)

BigFloat(x::Union{Bool,Int8,Int16,Int32}, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(convert(Clong, x), r; precision=precision)
BigFloat(x::Union{UInt8,UInt16,UInt32}, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(convert(Culong, x), r; precision=precision)

BigFloat(x::Union{Float16,Float32}, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(Float64(x), r; precision=precision)

function BigFloat(x::Rational, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat))
    setprecision(BigFloat, precision) do
        setrounding_raw(BigFloat, r) do
            BigFloat(numerator(x))::BigFloat / BigFloat(denominator(x))::BigFloat
        end
    end
end

function tryparse(::Type{BigFloat}, s::AbstractString; base::Integer=0, precision::Integer=_precision_with_base_2(BigFloat), rounding::MPFRRoundingMode=rounding_raw(BigFloat))
    !isempty(s) && isspace(s[end]) && return tryparse(BigFloat, rstrip(s), base = base)
    z = BigFloat(precision=precision)
    err = ccall((:mpfr_set_str, libmpfr), Int32, (Ref{BigFloat}, Cstring, Int32, MPFRRoundingMode), z, s, base, rounding)
    err == 0 ? z : nothing
end

BigFloat(x::AbstractString, r::MPFRRoundingMode=rounding_raw(BigFloat); precision::Integer=_precision_with_base_2(BigFloat)) =
    parse(BigFloat, x; precision=precision, rounding=r)

Rational(x::BigFloat) = convert(Rational{BigInt}, x)
AbstractFloat(x::BigInt) = BigFloat(x)

float(::Type{BigInt}) = BigFloat

BigFloat(x::Real, r::RoundingMode; precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(x, convert(MPFRRoundingMode, r); precision=precision)::BigFloat
BigFloat(x::AbstractString, r::RoundingMode; precision::Integer=_precision_with_base_2(BigFloat)) =
    BigFloat(x, convert(MPFRRoundingMode, r); precision=precision)

## BigFloat -> Integer
_unchecked_cast(T, x::BigFloat, r::RoundingMode) = _unchecked_cast(T, x, convert(MPFRRoundingMode, r))

function _unchecked_cast(::Type{Int64}, x::BigFloat, r::MPFRRoundingMode)
    ccall((:__gmpfr_mpfr_get_sj,libmpfr), Cintmax_t, (Ref{BigFloat}, MPFRRoundingMode), x, r)
end

function _unchecked_cast(::Type{UInt64}, x::BigFloat, r::MPFRRoundingMode)
    ccall((:__gmpfr_mpfr_get_uj,libmpfr), Cuintmax_t, (Ref{BigFloat}, MPFRRoundingMode), x, r)
end

function _unchecked_cast(::Type{BigInt}, x::BigFloat, r::MPFRRoundingMode)
    z = BigInt()
    ccall((:mpfr_get_z, libmpfr), Int32, (Ref{BigInt}, Ref{BigFloat}, MPFRRoundingMode), z, x, r)
    return z
end

function _unchecked_cast(::Type{T}, x::BigFloat, r::MPFRRoundingMode) where T<:Union{Signed, Unsigned}
    CT = T <: Signed ? Int64 : UInt64
    typemax(T) < typemax(CT) ? _unchecked_cast(CT, x, r) : _unchecked_cast(BigInt, x, r)
end

function round(::Type{T}, x::BigFloat, r::Union{RoundingMode, MPFRRoundingMode}) where T<:Union{Signed, Unsigned}
    clear_flags()
    res = _unchecked_cast(T, x, r)
    if had_range_exception() || !(typemin(T) <= res <= typemax(T))
        throw(InexactError(:round, T, x))
    end
    return unsafe_trunc(T, res)
end

function round(::Type{BigInt}, x::BigFloat, r::Union{RoundingMode, MPFRRoundingMode})
    clear_flags()
    res = _unchecked_cast(BigInt, x, r)
    had_range_exception() && throw(InexactError(:round, BigInt, x))
    return res
end

round(::Type{T}, x::BigFloat, r::RoundingMode) where T<:Union{Signed, Unsigned} =
    invoke(round, Tuple{Type{<:Union{Signed, Unsigned}}, BigFloat, Union{RoundingMode, MPFRRoundingMode}}, T, x, r)
round(::Type{BigInt}, x::BigFloat, r::RoundingMode) =
    invoke(round, Tuple{Type{BigInt}, BigFloat, Union{RoundingMode, MPFRRoundingMode}}, BigInt, x, r)


unsafe_trunc(::Type{T}, x::BigFloat) where {T<:Integer} = unsafe_trunc(T, _unchecked_cast(T, x, RoundToZero))
unsafe_trunc(::Type{BigInt}, x::BigFloat) = _unchecked_cast(BigInt, x, RoundToZero)

round(::Type{T}, x::BigFloat) where T<:Integer = round(T, x, rounding_raw(BigFloat))
# these two methods are split to increase their precedence in disambiguation:
round(::Type{Integer}, x::BigFloat, r::RoundingMode) = round(BigInt, x, r)
round(::Type{Integer}, x::BigFloat, r::MPFRRoundingMode) = round(BigInt, x, r)

function Bool(x::BigFloat)
    iszero(x) && return false
    isone(x) && return true
    throw(InexactError(:Bool, Bool, x))
end
function BigInt(x::BigFloat)
    isinteger(x) || throw(InexactError(:BigInt, BigInt, x))
    trunc(BigInt, x)
end

function (::Type{T})(x::BigFloat) where T<:Integer
    isinteger(x) || throw(InexactError(nameof(T), T, x))
    trunc(T,x)
end

function to_ieee754(::Type{T}, x::BigFloat, rm) where {T<:AbstractFloat}
    sb = signbit(x)
    is_zero = iszero(x)
    is_inf = isinf(x)
    is_nan = isnan(x)
    is_regular = !is_zero & !is_inf & !is_nan
    ieee_exp = Int(x.exp) - 1
    ieee_precision = precision(T)
    ieee_exp_max = exponent_max(T)
    ieee_exp_min = exponent_min(T)
    exp_diff = ieee_exp - ieee_exp_min
    is_normal = 0 ≤ exp_diff
    (rm_is_to_zero, rm_is_from_zero) = if rounds_to_nearest(rm)
        (false, false)
    else
        let from = rounds_away_from_zero(rm, sb)
            (!from, from)
        end
    end::NTuple{2,Bool}
    exp_is_huge_p = ieee_exp_max < ieee_exp
    exp_is_huge_n = signbit(exp_diff + ieee_precision)
    rounds_to_inf = is_regular & exp_is_huge_p & !rm_is_to_zero
    rounds_to_zero = is_regular & exp_is_huge_n & !rm_is_from_zero
    U = uinttype(T)

    ret_u = if is_regular & !rounds_to_inf & !rounds_to_zero
        if !exp_is_huge_p
            # significand
            v = x.d::BigFloatData
            len = max(ieee_precision + min(exp_diff, 0), 0)::Int
            signif = truncated(U, v, len) & significand_mask(T)

            # round up if necessary
            rh = BigFloatDataRoundingIncrementHelper(v, len)
            incr = correct_rounding_requires_increment(rh, rm, sb)

            # exponent
            exp_field = max(exp_diff, 0) + is_normal

            ieee754_representation(T, sb, exp_field, signif) + incr
        else
            ieee754_representation(T, sb, Val(:omega))
        end
    else
        if is_zero | rounds_to_zero
            ieee754_representation(T, sb, Val(:zero))
        elseif is_inf | rounds_to_inf
            ieee754_representation(T, sb, Val(:inf))
        else
            ieee754_representation(T, sb, Val(:nan))
        end
    end::U

    reinterpret(T, ret_u)
end

Float16(x::BigFloat, r::MPFRRoundingMode=rounding_raw(BigFloat)) = to_ieee754(Float16, x, r)
Float32(x::BigFloat, r::MPFRRoundingMode=rounding_raw(BigFloat)) = to_ieee754(Float32, x, r)
Float64(x::BigFloat, r::MPFRRoundingMode=rounding_raw(BigFloat)) = to_ieee754(Float64, x, r)
Float16(x::BigFloat, r::RoundingMode) = to_ieee754(Float16, x, r)
Float32(x::BigFloat, r::RoundingMode) = to_ieee754(Float32, x, r)
Float64(x::BigFloat, r::RoundingMode) = to_ieee754(Float64, x, r)

promote_rule(::Type{BigFloat}, ::Type{<:Real}) = BigFloat
promote_rule(::Type{BigInt}, ::Type{<:AbstractFloat}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{<:AbstractFloat}) = BigFloat

big(::Type{<:AbstractFloat}) = BigFloat

big(x::AbstractFloat) = convert(BigFloat, x)

function Rational{BigInt}(x::AbstractFloat)
    isnan(x) && return zero(BigInt) // zero(BigInt)
    isinf(x) && return copysign(one(BigInt),x) // zero(BigInt)
    iszero(x) && return zero(BigInt) // one(BigInt)
    s = max(precision(x) - exponent(x), 0)
    BigInt(ldexp(x,s)) // (BigInt(1) << s)
end

# Basic arithmetic without promotion
for (fJ, fC) in ((:+,:add), (:*,:mul))
    @eval begin
        # BigFloat
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)),libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
            return z
        end

        # Unsigned Integer
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        ($fJ)(c::CulongMax, x::BigFloat) = ($fJ)(x,c)

        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        ($fJ)(c::ClongMax, x::BigFloat) = ($fJ)(x,c)

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Cdouble, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        ($fJ)(c::CdoubleMax, x::BigFloat) = ($fJ)(x,c)

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigInt}, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
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
            ccall(($(string(:mpfr_,fC)),libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
            return z
        end

        # Unsigned Int
        function ($fJ)(x::BigFloat, c::CulongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_ui)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        function ($fJ)(c::CulongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:ui_,fC)), libmpfr), Int32, (Ref{BigFloat}, Culong, Ref{BigFloat}, MPFRRoundingMode), z, c, x, rounding_raw(BigFloat))
            return z
        end

        # Signed Integer
        function ($fJ)(x::BigFloat, c::ClongMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_si)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        function ($fJ)(c::ClongMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:si_,fC)), libmpfr), Int32, (Ref{BigFloat}, Clong, Ref{BigFloat}, MPFRRoundingMode), z, c, x, rounding_raw(BigFloat))
            return z
        end

        # Float32/Float64
        function ($fJ)(x::BigFloat, c::CdoubleMax)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_d)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Cdouble, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        function ($fJ)(c::CdoubleMax, x::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,:d_,fC)), libmpfr), Int32, (Ref{BigFloat}, Cdouble, Ref{BigFloat}, MPFRRoundingMode), z, c, x, rounding_raw(BigFloat))
            return z
        end

        # BigInt
        function ($fJ)(x::BigFloat, c::BigInt)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC,:_z)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigInt}, MPFRRoundingMode), z, x, c, rounding_raw(BigFloat))
            return z
        end
        # no :mpfr_z_div function
    end
end

function -(c::BigInt, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_z_sub, libmpfr), Int32, (Ref{BigFloat}, Ref{BigInt}, Ref{BigFloat}, MPFRRoundingMode), z, c, x, rounding_raw(BigFloat))
    return z
end

inv(x::BigFloat) = one(Clong) / x # faster than fallback one(x)/x

function fma(x::BigFloat, y::BigFloat, z::BigFloat)
    r = BigFloat()
    ccall(("mpfr_fma",libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), r, x, y, z, rounding_raw(BigFloat))
    return r
end

muladd(x::BigFloat, y::BigFloat, z::BigFloat) = fma(x, y, z)

# div
# BigFloat
function div(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_div,libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end

# Unsigned Int
function div(x::BigFloat, c::CulongMax)
    z = BigFloat()
    ccall((:mpfr_div_ui, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, c, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end
function div(c::CulongMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_ui_div, libmpfr), Int32, (Ref{BigFloat}, Culong, Ref{BigFloat}, MPFRRoundingMode), z, c, x, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end

# Signed Integer
function div(x::BigFloat, c::ClongMax)
    z = BigFloat()
    ccall((:mpfr_div_si, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, x, c, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end
function div(c::ClongMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_si_div, libmpfr), Int32, (Ref{BigFloat}, Clong, Ref{BigFloat}, MPFRRoundingMode), z, c, x, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end

# Float32/Float64
function div(x::BigFloat, c::CdoubleMax)
    z = BigFloat()
    ccall((:mpfr_div_d, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Cdouble, MPFRRoundingMode), z, x, c, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end
function div(c::CdoubleMax, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_d_div, libmpfr), Int32, (Ref{BigFloat}, Cdouble, Ref{BigFloat}, MPFRRoundingMode), z, c, x, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end

# BigInt
function div(x::BigFloat, c::BigInt)
    z = BigFloat()
    ccall((:mpfr_div_z, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigInt}, MPFRRoundingMode), z, x, c, RoundToZero)
    ccall((:mpfr_trunc, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, z)
    return z
end


# More efficient commutative operations
for (fJ, fC, fI) in ((:+, :add, 0), (:*, :mul, 1))
    @eval begin
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, a, b, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, c, rounding_raw(BigFloat))
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, a, b, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, c, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, d, rounding_raw(BigFloat))
            return z
        end
        function ($fJ)(a::BigFloat, b::BigFloat, c::BigFloat, d::BigFloat, e::BigFloat)
            z = BigFloat()
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, a, b, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, c, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, d, rounding_raw(BigFloat))
            ccall(($(string(:mpfr_,fC)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, e, rounding_raw(BigFloat))
            return z
        end
    end
end

function -(x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_neg, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
    return z
end

function sqrt(x::BigFloat)
    isnan(x) && return x
    z = BigFloat()
    ccall((:mpfr_sqrt, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
    isnan(z) && throw(DomainError(x, "NaN result for non-NaN input."))
    return z
end

sqrt(x::BigInt) = sqrt(BigFloat(x))

function ^(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_pow, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

function ^(x::BigFloat, y::CulongMax)
    z = BigFloat()
    ccall((:mpfr_pow_ui, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

function ^(x::BigFloat, y::ClongMax)
    z = BigFloat()
    ccall((:mpfr_pow_si, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

function ^(x::BigFloat, y::BigInt)
    z = BigFloat()
    ccall((:mpfr_pow_z, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigInt}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

^(x::BigFloat, y::Integer)  = typemin(Clong)  <= y <= typemax(Clong)  ? x^Clong(y)  : x^BigInt(y)
^(x::BigFloat, y::Unsigned) = typemin(Culong) <= y <= typemax(Culong) ? x^Culong(y) : x^BigInt(y)

for f in (:exp, :exp2, :exp10, :expm1, :cosh, :sinh, :tanh, :sech, :csch, :coth, :cbrt)
    @eval function $f(x::BigFloat)
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
        return z
    end
end

function sincos_fast(v::BigFloat)
    s = BigFloat()
    c = BigFloat()
    ccall((:mpfr_sin_cos, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), s, c, v, rounding_raw(BigFloat))
    return (s, c)
end
sincos(v::BigFloat) = sincos_fast(v)

# return log(2)
function big_ln2()
    c = BigFloat()
    ccall((:mpfr_const_log2, libmpfr), Cint, (Ref{BigFloat}, MPFRRoundingMode), c, MPFR.rounding_raw(BigFloat))
    return c
end

function ldexp(x::BigFloat, n::Clong)
    z = BigFloat()
    ccall((:mpfr_mul_2si, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, x, n, rounding_raw(BigFloat))
    return z
end
function ldexp(x::BigFloat, n::Culong)
    z = BigFloat()
    ccall((:mpfr_mul_2ui, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, n, rounding_raw(BigFloat))
    return z
end
ldexp(x::BigFloat, n::ClongMax) = ldexp(x, convert(Clong, n))
ldexp(x::BigFloat, n::CulongMax) = ldexp(x, convert(Culong, n))
ldexp(x::BigFloat, n::Integer) = x * exp2(BigFloat(n))

function factorial(x::BigFloat)
    if x < 0 || !isinteger(x)
        throw(DomainError(x, "Must be a non-negative integer."))
    end
    ui = convert(Culong, x)
    z = BigFloat()
    ccall((:mpfr_fac_ui, libmpfr), Int32, (Ref{BigFloat}, Culong, MPFRRoundingMode), z, ui, rounding_raw(BigFloat))
    return z
end

function hypot(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_hypot, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

for f in (:log, :log2, :log10)
    @eval function $f(x::BigFloat)
        if x < 0
            throw(DomainError(x, string($f, " was called with a negative real argument but ",
                              "will only return a complex result if called ",
                              "with a complex argument. Try ", $f, "(complex(x)).")))
        end
        z = BigFloat()
        ccall(($(string(:mpfr_,f)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
        return z
    end
end

function log1p(x::BigFloat)
    if x < -1
        throw(DomainError(x, string("log1p was called with a real argument < -1 but ",
                          "will only return a complex result if called ",
                          "with a complex argument. Try log1p(complex(x)).")))
    end
    z = BigFloat()
    ccall((:mpfr_log1p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
    return z
end

# For `min`/`max`, general fallback for `AbstractFloat` is good enough.
# Only implement `minmax` and `_extrema_rf` to avoid repeated calls.
function minmax(x::BigFloat, y::BigFloat)
    isnan(x) && return x, x
    isnan(y) && return y, y
    Base.Math._isless(x, y) ? (x, y) : (y, x)
end

function Base._extrema_rf(x::NTuple{2,BigFloat}, y::NTuple{2,BigFloat})
    (x1, x2), (y1, y2) = x, y
    isnan(x1) && return x
    isnan(y1) && return y
    z1 = Base.Math._isless(x1, y1) ? x1 : y1
    z2 = Base.Math._isless(x2, y2) ? y2 : x2
    z1, z2
end

function modf(x::BigFloat)
    zint = BigFloat()
    zfloat = BigFloat()
    ccall((:mpfr_modf, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), zint, zfloat, x, rounding_raw(BigFloat))
    return (zfloat, zint)
end

function rem(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_fmod, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

function rem(x::BigFloat, y::BigFloat, ::RoundingMode{:Nearest})
    z = BigFloat()
    ccall((:mpfr_remainder, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

# TODO: use a higher-precision BigFloat(pi) here?
rem2pi(x::BigFloat, r::RoundingMode) = rem(x, 2*BigFloat(pi), r)

function sum(arr::AbstractArray{BigFloat})
    z = BigFloat(0)
    for i in arr
        ccall((:mpfr_add, libmpfr), Int32,
            (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, z, i, rounding_raw(BigFloat))
    end
    return z
end

# Functions for which NaN results are converted to DomainError, following Base
for f in (:sin, :cos, :tan, :sec, :csc, :acos, :asin, :atan, :acosh, :asinh, :atanh, :sinpi, :cospi, :tanpi)
    @eval begin
        function ($f)(x::BigFloat)
            isnan(x) && return x
            z = BigFloat()
            ccall(($(string(:mpfr_,f)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, rounding_raw(BigFloat))
            isnan(z) && throw(DomainError(x, "NaN result for non-NaN input."))
            return z
        end
    end
end
sincospi(x::BigFloat) = (sinpi(x), cospi(x))

function atan(y::BigFloat, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_atan2, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, y, x, rounding_raw(BigFloat))
    return z
end

# degree functions
for f in (:sin, :cos, :tan)
    @eval begin
        function ($(Symbol(f,:d)))(x::BigFloat)
            isnan(x) && return x
            z = BigFloat()
            ccall(($(string(:mpfr_,f,:u)), :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, 360, rounding_raw(BigFloat))
            isnan(z) && throw(DomainError(x, "NaN result for non-NaN input."))
            return z
        end
        function ($(Symbol(:a,f,:d)))(x::BigFloat)
            isnan(x) && return x
            z = BigFloat()
            ccall(($(string(:mpfr_a,f,:u)), :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, x, 360, rounding_raw(BigFloat))
            isnan(z) && throw(DomainError(x, "NaN result for non-NaN input."))
            return z
        end
    end
end
function atand(y::BigFloat, x::BigFloat)
    z = BigFloat()
    ccall((:mpfr_atan2u, :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, Culong, MPFRRoundingMode), z, y, x, 360, rounding_raw(BigFloat))
    return z
end


# Utility functions
==(x::BigFloat, y::BigFloat) = ccall((:mpfr_equal_p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), x, y) != 0
<=(x::BigFloat, y::BigFloat) = ccall((:mpfr_lessequal_p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), x, y) != 0
>=(x::BigFloat, y::BigFloat) = ccall((:mpfr_greaterequal_p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), x, y) != 0
<(x::BigFloat, y::BigFloat) = ccall((:mpfr_less_p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), x, y) != 0
>(x::BigFloat, y::BigFloat) = ccall((:mpfr_greater_p, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), x, y) != 0

function cmp(x::BigFloat, y::BigInt)
    isnan(x) && return 1
    ccall((:mpfr_cmp_z, libmpfr), Int32, (Ref{BigFloat}, Ref{BigInt}), x, y)
end
function cmp(x::BigFloat, y::ClongMax)
    isnan(x) && return 1
    ccall((:mpfr_cmp_si, libmpfr), Int32, (Ref{BigFloat}, Clong), x, y)
end
function cmp(x::BigFloat, y::CulongMax)
    isnan(x) && return 1
    ccall((:mpfr_cmp_ui, libmpfr), Int32, (Ref{BigFloat}, Culong), x, y)
end
cmp(x::BigFloat, y::Integer) = cmp(x,big(y))
cmp(x::Integer, y::BigFloat) = -cmp(y,x)

function cmp(x::BigFloat, y::CdoubleMax)
    isnan(x) && return isnan(y) ? 0 : 1
    isnan(y) && return -1
    ccall((:mpfr_cmp_d, libmpfr), Int32, (Ref{BigFloat}, Cdouble), x, y)
end
cmp(x::CdoubleMax, y::BigFloat) = -cmp(y,x)

==(x::BigFloat, y::Integer)    = !isnan(x) && cmp(x,y) == 0
==(x::Integer, y::BigFloat)    = y == x
==(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) == 0
==(x::CdoubleMax, y::BigFloat) = y == x

<(x::BigFloat, y::Integer)    = !isnan(x) && cmp(x,y) < 0
<(x::Integer, y::BigFloat)    = !isnan(y) && cmp(y,x) > 0
<(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) < 0
<(x::CdoubleMax, y::BigFloat) = !isnan(x) && !isnan(y) && cmp(y,x) > 0

<=(x::BigFloat, y::Integer)    = !isnan(x) && cmp(x,y) <= 0
<=(x::Integer, y::BigFloat)    = !isnan(y) && cmp(y,x) >= 0
<=(x::BigFloat, y::CdoubleMax) = !isnan(x) && !isnan(y) && cmp(x,y) <= 0
<=(x::CdoubleMax, y::BigFloat) = !isnan(x) && !isnan(y) && cmp(y,x) >= 0

# Note: this inlines the implementation of `mpfr_signbit` to avoid a
# `ccall`.
signbit(x::BigFloat) = signbit(x.sign)

function sign(x::BigFloat)
    c = cmp(x, 0)
    (c == 0 || isnan(x)) && return x
    return c < 0 ? -one(x) : one(x)
end

function _precision_with_base_2(x::BigFloat)  # precision of an object of type BigFloat
    return ccall((:mpfr_get_prec, libmpfr), Clong, (Ref{BigFloat},), x)
end
precision(x::BigFloat; base::Integer=2) = _precision(x, base)


_convert_precision_from_base(precision::Integer, base::Integer) =
    base == 2 ? precision : ceil(Int, precision * log2(base))

_precision_with_base_2(::Type{BigFloat}) =
    Int(something(Base.ScopedValues.get(CURRENT_PRECISION), DEFAULT_PRECISION[])) # default precision of the type BigFloat itself

"""
    setprecision([T=BigFloat,] precision::Int; base=2)

Set the precision (in bits, by default) to be used for `T` arithmetic.
If `base` is specified, then the precision is the minimum required to give
at least `precision` digits in the given `base`.

!!! warning

    This function is not thread-safe. It will affect code running on all threads, but
    its behavior is undefined if called concurrently with computations that use the
    setting.

!!! compat "Julia 1.8"
    The `base` keyword requires at least Julia 1.8.
"""
function setprecision(::Type{BigFloat}, precision::Integer; base::Integer=2)
    base > 1 || throw(DomainError(base, "`base` cannot be less than 2."))
    precision > 0 || throw(DomainError(precision, "`precision` cannot be less than 1."))
    DEFAULT_PRECISION[] = _convert_precision_from_base(precision, base)
    return precision
end

setprecision(precision::Integer; base::Integer=2) = setprecision(BigFloat, precision; base)

maxintfloat(x::BigFloat) = BigFloat(2)^precision(x)
maxintfloat(::Type{BigFloat}) = BigFloat(2)^precision(BigFloat)

function copysign(x::BigFloat, y::BigFloat)
    z = BigFloat()
    ccall((:mpfr_copysign, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), z, x, y, rounding_raw(BigFloat))
    return z
end

function exponent(x::BigFloat)
    if iszero(x) || !isfinite(x)
        throw(DomainError(x, "`x` must be non-zero and finite."))
    end
    # The '- 1' is to make it work as Base.exponent
    return ccall((:mpfr_get_exp, libmpfr), Clong, (Ref{BigFloat},), x) - 1
end

function frexp(x::BigFloat)
    z = BigFloat()
    c = Ref{Clong}()
    ccall((:mpfr_frexp, libmpfr), Int32, (Ptr{Clong}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), c, z, x, rounding_raw(BigFloat))
    return (z, c[])
end

function significand(x::BigFloat)
    z = BigFloat()
    c = Ref{Clong}()
    ccall((:mpfr_frexp, libmpfr), Int32, (Ptr{Clong}, Ref{BigFloat}, Ref{BigFloat}, MPFRRoundingMode), c, z, x, rounding_raw(BigFloat))
    # Double the significand to make it work as Base.significand
    ccall((:mpfr_mul_si, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Clong, MPFRRoundingMode), z, z, 2, rounding_raw(BigFloat))
    return z
end

function isinteger(x::BigFloat)
    return ccall((:mpfr_integer_p, libmpfr), Int32, (Ref{BigFloat},), x) != 0
end

for (f,R) in ((:roundeven, :Nearest),
              (:ceil, :Up),
              (:floor, :Down),
              (:trunc, :ToZero),
              (:round, :NearestTiesAway))
    @eval begin
        function round(x::BigFloat, ::RoundingMode{$(QuoteNode(R))})
            z = BigFloat()
            ccall(($(string(:mpfr_,f)), libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}), z, x)
            return z
        end
    end
end

function isinf(x::BigFloat)
    return x.exp == mpfr_special_exponent_inf
end

function isnan(x::BigFloat)
    return x.exp == mpfr_special_exponent_nan
end

isfinite(x::BigFloat) = !isinf(x) && !isnan(x)

iszero(x::BigFloat) = x.exp == mpfr_special_exponent_zero
isone(x::BigFloat) = x == Clong(1)

@eval typemax(::Type{BigFloat}) = $(BigFloat(Inf))
@eval typemin(::Type{BigFloat}) = $(BigFloat(-Inf))

function nextfloat!(x::BigFloat, n::Integer=1)
    signbit(n) && return prevfloat!(x, abs(n))
    for i = 1:n
        ccall((:mpfr_nextabove, libmpfr), Int32, (Ref{BigFloat},), x)
    end
    return x
end

function prevfloat!(x::BigFloat, n::Integer=1)
    signbit(n) && return nextfloat!(x, abs(n))
    for i = 1:n
        ccall((:mpfr_nextbelow, libmpfr), Int32, (Ref{BigFloat},), x)
    end
    return x
end

nextfloat(x::BigFloat, n::Integer=1) = n == 0 ? x : nextfloat!(_duplicate(x), n)
prevfloat(x::BigFloat, n::Integer=1) = n == 0 ? x : prevfloat!(_duplicate(x), n)

eps(::Type{BigFloat}) = nextfloat(BigFloat(1)) - BigFloat(1)

floatmin(::Type{BigFloat}) = nextfloat(zero(BigFloat))
floatmax(::Type{BigFloat}) = prevfloat(BigFloat(Inf))

"""
    setprecision(f::Function, [T=BigFloat,] precision::Integer; base=2)

Change the `T` arithmetic precision (in the given `base`) for the duration of `f`.
It is logically equivalent to:

    old = precision(BigFloat)
    setprecision(BigFloat, precision)
    f()
    setprecision(BigFloat, old)

Often used as `setprecision(T, precision) do ... end`

Note: `nextfloat()`, `prevfloat()` do not use the precision mentioned by
`setprecision`.

!!! compat "Julia 1.8"
    The `base` keyword requires at least Julia 1.8.
"""
function setprecision(f::Function, ::Type{BigFloat}, prec::Integer; base::Integer=2)
    Base.ScopedValues.@with(CURRENT_PRECISION => _convert_precision_from_base(prec, base), f())
end

setprecision(f::Function, prec::Integer; base::Integer=2) = setprecision(f, BigFloat, prec; base)

function string_mpfr(x::BigFloat, fmt::String)
    pc = Ref{Ptr{UInt8}}()
    n = ccall((:mpfr_asprintf,libmpfr), Cint,
              (Ptr{Ptr{UInt8}}, Ptr{UInt8}, Ref{BigFloat}...),
              pc, fmt, x)
    p = pc[]
    # convert comma decimal separator to dot
    for i = 1:n
        if unsafe_load(p, i) == UInt8(',')
            unsafe_store!(p, '.', i)
            break
        end
    end
    str = unsafe_string(p)
    ccall((:mpfr_free_str, libmpfr), Cvoid, (Ptr{UInt8},), p)
    return str
end

function _prettify_bigfloat(s::String)::String
    mantissa, exponent = eachsplit(s, 'e')
    if !occursin('.', mantissa)
        mantissa = string(mantissa, '.')
    end
    mantissa = rstrip(mantissa, '0')
    if endswith(mantissa, '.')
        mantissa = string(mantissa, '0')
    end
    expo = parse(Int, exponent)
    if -5 < expo < 6
        expo == 0 && return mantissa
        int, frac = eachsplit(mantissa, '.')
        if expo > 0
            expo < length(frac) ?
                string(int, frac[1:expo], '.', frac[expo+1:end]) :
                string(int, frac, '0'^(expo-length(frac)), '.', '0')
        else
            neg = startswith(int, '-')
            neg == true && (int = lstrip(int, '-'))
            @assert length(int) == 1
            string(neg ? '-' : "", '0', '.', '0'^(-expo-1), int, frac == "0" ? "" : frac)
        end
    else
        string(mantissa, 'e', exponent)
    end
end

function _string(x::BigFloat, fmt::String)::String
    isfinite(x) || return string(Float64(x))
    _prettify_bigfloat(string_mpfr(x, fmt))
end
_string(x::BigFloat) = _string(x, "%Re")
_string(x::BigFloat, k::Integer) = _string(x, "%.$(k)Re")

string(b::BigFloat) = _string(b)

print(io::IO, b::BigFloat) = print(io, string(b))
function show(io::IO, b::BigFloat)
    if get(io, :compact, false)::Bool
        print(io, _string(b, 5))
    else
        print(io, _string(b))
    end
end

# get/set exponent min/max
get_emax() = ccall((:mpfr_get_emax, libmpfr), Clong, ())
get_emax_min() = ccall((:mpfr_get_emax_min, libmpfr), Clong, ())
get_emax_max() = ccall((:mpfr_get_emax_max, libmpfr), Clong, ())

get_emin() = ccall((:mpfr_get_emin, libmpfr), Clong, ())
get_emin_min() = ccall((:mpfr_get_emin_min, libmpfr), Clong, ())
get_emin_max() = ccall((:mpfr_get_emin_max, libmpfr), Clong, ())

check_exponent_err(ret) = ret == 0 || throw(ArgumentError("Invalid MPFR exponent range"))
set_emax!(x) = check_exponent_err(ccall((:mpfr_set_emax, libmpfr), Cint, (Clong,), x))
set_emin!(x) = check_exponent_err(ccall((:mpfr_set_emin, libmpfr), Cint, (Clong,), x))

function Base.deepcopy_internal(x::BigFloat, stackdict::IdDict)
    get!(stackdict, x) do
        d′ = copy(getfield(x, :d))
        y = _BigFloat(d′)
        #ccall((:mpfr_custom_move,libmpfr), Cvoid, (Ref{BigFloat}, Ptr{Limb}), y, d) # unnecessary
        return y
    end::BigFloat
end

function decompose(x::BigFloat)::Tuple{BigInt, Int, Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return x.sign, 0, 0
    x == 0 && return 0, 0, x.sign
    s = BigInt()
    s.size = cld(x.prec, 8*sizeof(Limb)) # limbs
    b = s.size * sizeof(Limb)            # bytes
    ccall((:__gmpz_realloc2, libgmp), Cvoid, (Ref{BigInt}, Culong), s, 8b) # bits
    xd = x.d
    GC.@preserve xd memcpy(s.d, Base.unsafe_convert(Ptr{Limb}, xd), b)
    s, x.exp - 8b, x.sign
end

function lerpi(j::Integer, d::Integer, a::BigFloat, b::BigFloat)
    t = BigFloat(j)/d
    fma(t, b, fma(-t, a, a))
end

# flags
clear_flags() = ccall((:mpfr_clear_flags, libmpfr), Cvoid, ())
had_underflow() = ccall((:mpfr_underflow_p, libmpfr), Cint, ()) != 0
had_overflow() = ccall((:mpfr_overflow_p, libmpfr), Cint, ()) != 0
had_divbyzero() = ccall((:mpfr_divby0_p, libmpfr), Cint, ()) != 0
had_nan() = ccall((:mpfr_nanflag_p, libmpfr), Cint, ()) != 0
had_inexact_exception() = ccall((:mpfr_inexflag_p, libmpfr), Cint, ()) != 0
had_range_exception() = ccall((:mpfr_erangeflag_p, libmpfr), Cint, ()) != 0

end #module
