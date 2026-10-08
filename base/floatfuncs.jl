# This file is a part of Julia. License is MIT: https://julialang.org/license

## floating-point functions ##

copysign(x::Float64, y::Float64) = copysign_float(x, y)
copysign(x::Float32, y::Float32) = copysign_float(x, y)
copysign(x::Float32, y::Real) = copysign(x, Float32(y))
copysign(x::Float64, y::Real) = copysign(x, Float64(y))

flipsign(x::Float64, y::Float64) = bitcast(Float64, xor_int(bitcast(UInt64, x), and_int(bitcast(UInt64, y), 0x8000000000000000)))
flipsign(x::Float32, y::Float32) = bitcast(Float32, xor_int(bitcast(UInt32, x), and_int(bitcast(UInt32, y), 0x80000000)))
flipsign(x::Float32, y::Real) = flipsign(x, Float32(y))
flipsign(x::Float64, y::Real) = flipsign(x, Float64(y))

signbit(x::Float64) = signbit(bitcast(Int64, x))
signbit(x::Float32) = signbit(bitcast(Int32, x))
signbit(x::Float16) = signbit(bitcast(Int16, x))

"""
    maxintfloat(T=Float64)
    maxintfloat(::T)

The largest consecutive integer-valued floating-point number that is exactly represented in
the given floating-point type `T` (which defaults to `Float64`). The argument can alternatively
be an instance of `T`.

That is, `maxintfloat` returns the smallest positive integer-valued floating-point number
`n` such that `n+1` is *not* exactly representable in the type `T`.

When an `Integer`-type value is needed, use `Integer(maxintfloat(T))`.

See also [`typemax`](@ref), [`floatmax`](@ref).
"""
maxintfloat(::Type{Float64}) = 9007199254740992.
maxintfloat(::Type{Float32}) = Float32(16777216.)
maxintfloat(::Type{Float16}) = Float16(2048f0)
maxintfloat(x::T) where {T<:AbstractFloat} = maxintfloat(T)

"""
    maxintfloat(T, S)

The largest consecutive integer representable in the given floating-point type `T` that
also does not exceed the maximum integer representable by the integer type `S`.  Equivalently,
it is the minimum of `maxintfloat(T)` and [`typemax(S)`](@ref).
"""
maxintfloat(::Type{S}, ::Type{T}) where {S<:AbstractFloat, T<:Integer} = min(maxintfloat(S), S(typemax(T)))
maxintfloat() = maxintfloat(Float64)

isinteger(x::AbstractFloat) = iszero(x - trunc(x)) # note: x == trunc(x) would be incorrect for x=Inf

# See rounding.jl for docstring.

# NOTE: this relies on the current keyword dispatch behaviour (#9498).
function round(x::Real, r::RoundingMode=RoundNearest;
               digits::Union{Nothing,Integer}=nothing, sigdigits::Union{Nothing,Integer}=nothing, base::Union{Nothing,Integer}=nothing)
    if digits === nothing
        if sigdigits === nothing
            if base === nothing
                # avoid recursive calls
                throw(MethodError(round, (x,r)))
            else
                round(x,r)
                # or throw(ArgumentError("`round` cannot use `base` argument without `digits` or `sigdigits` arguments."))
            end
        else
            isfinite(x) || return float(x)
            _round_sigdigits(x, r, sigdigits, base === nothing ? 10 : base)
        end
    else
        if sigdigits === nothing
            isfinite(x) || return float(x)
            _round_digits(x, r, digits, base === nothing ? 10 : base)
        else
            throw(ArgumentError("`round` cannot use both `digits` and `sigdigits` arguments."))
        end
    end
end

# round x to multiples of 1/invstep
function _round_invstep(x, invstep, r::RoundingMode)
    y = round(x * invstep, r) / invstep
    if !isfinite(y)
        return x
    end
    return y
end

# round x to multiples of 1/(invstepsqrt^2)
# Using square root of step prevents overflowing
function _round_invstepsqrt(x, invstepsqrt, r::RoundingMode)
    y = round((x * invstepsqrt) * invstepsqrt, r) / invstepsqrt / invstepsqrt
    if !isfinite(y)
        return x
    end
    return y
end

# round x to multiples of step
function _round_step(x, step, r::RoundingMode)
    # TODO: use div with rounding mode
    y = round(x / step, r) * step
    if !isfinite(y)
        if x > 0
            return (r == RoundUp ? oftype(x, Inf) : zero(x))
        elseif x < 0
            return (r == RoundDown ? -oftype(x, Inf) : -zero(x))
        else
            return x
        end
    end
    return y
end

function _round_digits(x, r::RoundingMode, digits::Integer, base)
    fx = float(x)
    if digits >= 0
        invstep = oftype(fx, base)^digits
        if isfinite(invstep)
            return _round_invstep(fx, invstep, r)
        else
            invstepsqrt = oftype(fx, base)^oftype(fx, digits/2)
            return _round_invstepsqrt(fx, invstepsqrt, r)
        end
    else
        step = oftype(fx, base)^-digits
        return _round_step(fx, step, r)
    end
end

hidigit(x::Integer, base) = ndigits0z(x, base)
function hidigit(x::AbstractFloat, base)
    iszero(x) && return 0
    if base == 10
        return 1 + floor(Int, log10(abs(x)))
    elseif base == 2
        return 1 + exponent(x)
    else
        return 1 + floor(Int, log(base, abs(x)))
    end
end
hidigit(x::Real, base) = hidigit(float(x), base)

function _round_sigdigits(x, r::RoundingMode, sigdigits::Integer, base)
    h = hidigit(x, base)
    _round_digits(x, r, sigdigits-h, base)
end

function round(x::AbstractFloat, r::RoundingMode{:NearestTiesAway};
               digits::Union{Nothing,Integer}=nothing, sigdigits::Union{Nothing,Integer}=nothing, base::Union{Nothing,Integer}=nothing)
    if digits === nothing && sigdigits === nothing && base === nothing
        y = trunc(x)
        return ifelse(x==y, y, trunc(2*x-y))
    end
    return Base._round_kwargs(x, r, digits, sigdigits, base)
end
# Java-style round
function round(x::T, ::RoundingMode{:NearestTiesUp}) where {T <: AbstractFloat}
    copysign(floor((x + (T(0.25) - eps(T(0.5)))) + (T(0.25) + eps(T(0.5)))), x)
end

function Base.round(x::AbstractFloat, ::typeof(RoundFromZero))
    signbit(x) ? round(x, RoundDown) : round(x, RoundUp)
end

# isapprox: approximate equality of numbers
"""
    isapprox(x, y; atol::Number=0, rtol::Real=atol>0 ? 0 : √eps, nans::Bool=false[, norm::Function])

Inexact equality comparison. Two numbers compare equal if their relative distance *or* their
absolute distance is within tolerance bounds: `isapprox` returns `true` if
`norm(x-y) <= max(atol, rtol*max(norm(x), norm(y)))`. The default `atol` (absolute tolerance) is zero and the
default `rtol` (relative tolerance) depends on the types of `x` and `y`. The keyword argument `nans` determines
whether or not NaN values are considered equal (defaults to false).

For real or complex floating-point values, if an `atol > 0` is not specified, `rtol` defaults to
the square root of [`eps`](@ref) of the type of `x` or `y`, whichever is bigger (least precise).
This corresponds to requiring equality of about half of the significant digits. Otherwise,
e.g. for integer arguments or if an `atol > 0` is supplied, `rtol` defaults to zero.

The absolute tolerance `atol` has the same units as `x`; for dimensionful number types
its default is the zero of those units, `zero(real(x))`. The relative tolerance `rtol` is a
dimensionless number.

The `norm` keyword defaults to `abs` for numeric `(x,y)` and to `LinearAlgebra.norm` for
arrays (where an alternative `norm` choice is sometimes useful).
When `x` and `y` are arrays, if `norm(x-y)` is not finite (i.e. `±Inf`
or `NaN`), the comparison falls back to checking whether all elements of `x` and `y` are
approximately equal component-wise.

The binary operator `≈` is equivalent to `isapprox` with the default arguments, and `x ≉ y`
is equivalent to `!isapprox(x,y)`.

Note that `x ≈ 0` (i.e., comparing to zero with the default tolerances) is
equivalent to `x == 0` since the default `atol` is `0`.  In such cases, you should either
supply an appropriate `atol` (or use `norm(x) ≤ atol`) or rearrange your code (e.g.
use `x ≈ y` rather than `x - y ≈ 0`).   It is not possible to pick a nonzero `atol`
automatically because it depends on the overall scaling (the "units") of your problem:
for example, in `x - y ≈ 0`, `atol=1e-9` is an absurdly small tolerance if `x` is the
[radius of the Earth](https://en.wikipedia.org/wiki/Earth_radius) in meters,
but an absurdly large tolerance if `x` is the
[radius of a Hydrogen atom](https://en.wikipedia.org/wiki/Bohr_radius) in meters.

!!! compat "Julia 1.6"
    Passing the `norm` keyword argument when comparing numeric (non-array) arguments
    requires Julia 1.6 or later.

# Examples
```jldoctest
julia> isapprox(0.1, 0.15; atol=0.05)
true

julia> isapprox(0.1, 0.15; rtol=0.34)
true

julia> isapprox(0.1, 0.15; rtol=0.33)
false

julia> 0.1 + 1e-10 ≈ 0.1
true

julia> 1e-10 ≈ 0
false

julia> isapprox(1e-10, 0, atol=1e-8)
true

julia> isapprox([10.0^9, 1.0], [10.0^9, 2.0]) # using `norm`
true
```
"""
function isapprox(x::Number, y::Number;
                  atol::Number=zero(real(x)), rtol::Real=rtoldefault(x,y,atol),
                  nans::Bool=false, norm::Function=abs)
    x′, y′ = promote(x, y) # to avoid integer overflow
    x == y ||
        (isfinite(x) && isfinite(y) && norm(x-y) <= max(atol, rtol*max(norm(x′), norm(y′)))) ||
         (nans && isnan(x) && isnan(y))
end

"""
    _uabsdiff(x::Integer, y::Integer)

Compute the exact absolute difference, widening the result type when necessary.
"""
_uabsdiff(x::Integer, y::Integer) = x < y ? y - x : x - y

_uabsdiff(x::Bool, y::BitInteger) = _uabsdiff(oftype(y, x), y)
_uabsdiff(x::BitInteger, y::Bool) = _uabsdiff(x, oftype(x, y))

function _uabsdiff(x::BitUnsigned, y::BitUnsigned)
    lo, hi = minmax(x, y)
    return hi - lo
end
function _uabsdiff(x::BitSigned, y::BitSigned)
    lo, hi = minmax(x, y)
    # Unsigned subtraction gives the exact distance even across zero.
    return unsigned(hi) - unsigned(lo)
end

# Return (m, u, d, carried), where d is the wrapped absolute difference.
# On carry, m = |x| and the exact difference is m + u.
function _mixed_uabsdiff(x::BitSigned, y::BitUnsigned)
    U = promote_type(unsigned(typeof(x)), typeof(y))
    v, u = x % U, y % U
    d = ifelse(x < y, u - v, v - u)
    # For x < 0, the distance is |x| + y, which carries exactly when d < u.
    return -v, u, d, (x < 0) & (d < u)
end

function _uabsdiff(x::BitSigned, y::BitUnsigned)
    m, u, d, carried = _mixed_uabsdiff(x, y)
    return carried ? widen(m) + widen(u) : d
end
_uabsdiff(x::BitUnsigned, y::BitSigned) = _uabsdiff(y, x)

_uabsdiff_le(x::Integer, y::Integer, b::Real) = _uabsdiff(x, y) <= b
# Keep widening out of the caller to limit code size.
@noinline _widesum_le(m::T, u::T, b::Real) where {T<:BitUnsigned} = widen(m) + widen(u) <= b
function _uabsdiff_le(x::BitSigned, y::BitUnsigned, b::Real)
    m, u, d, carried = _mixed_uabsdiff(x, y)
    # Widen only if both the distance and the bound exceed typemax(d).
    carried & (b > typemax(d)) && return _widesum_le(m, u, b)
    return (d <= b) & !carried
end
_uabsdiff_le(x::BitUnsigned, y::BitSigned, b::Real) = _uabsdiff_le(y, x, b)

_scaled_rtol(rtol::Real, scale::Integer) = (rtol * scale, false)
_scaled_rtol(rtol::BitInteger, scale::BitInteger) = mul_with_overflow(promote(rtol, scale)...)

function isapprox(x::Integer, y::Integer;
                  atol::Real=0, rtol::Real=rtoldefault(x,y,atol),
                  nans::Bool=false, norm::Function=abs)
    if norm === abs
        atol < 1 && rtol == 0 && return x == y
        # Check equality before forming the bound, since Inf * 0 is NaN.
        x == y && return true
        # uabs handles typemin and avoids signed/unsigned promotion.
        b, overflowed = _scaled_rtol(rtol, max(uabs(x), uabs(y)))
        # Overflow implies rtol >= 2, hence rtol * max(|x|, |y|) >= |x - y|.
        return overflowed || _uabsdiff_le(x, y, max(atol, b))
    end
    return x == y ||
        norm(_uabsdiff(x, y)) <= max(atol, rtol*max(norm(uabs(x)), norm(uabs(y))))
end

"""
    isapprox(x; kwargs...) / ≈(x; kwargs...)

Create a function that compares its argument to `x` using `≈`, i.e. a function equivalent to `y -> y ≈ x`.

The keyword arguments supported here are the same as those in the 2-argument `isapprox`.

!!! compat "Julia 1.5"
    This method requires Julia 1.5 or later.
"""
isapprox(y; kwargs...) = x -> isapprox(x, y; kwargs...)

const ≈ = isapprox
"""
    x ≉ y

This is equivalent to `!isapprox(x,y)` (see [`isapprox`](@ref)).
"""
≉(args...; kws...) = !≈(args...; kws...)

# default tolerance arguments
rtoldefault(::Type{T}) where {T<:AbstractFloat} = sqrt(eps(T))
rtoldefault(::Type{<:Real}) = 0
# other number types, e.g. dimensionful quantities: the relative tolerance is dimensionless,
# so it is determined by the type of the multiplicative identity
function rtoldefault(::Type{T}) where {T<:Number}
    S = typeof(one(T))
    S === T && throw(MethodError(rtoldefault, (T,)))
    return rtoldefault(S)
end
function rtoldefault(x::Union{T,Type{T}}, y::Union{S,Type{S}}, atol::Number) where {T<:Number,S<:Number}
    rtol = max(rtoldefault(real(T)),rtoldefault(real(S)))
    return atol > zero(atol) ? zero(rtol) : rtol
end

# fused multiply-add

"""
    fma(x, y, z)

Compute `x*y+z` without rounding the intermediate result `x*y`. On some systems this is
significantly more expensive than `x*y+z`. `fma` is used to improve accuracy in certain
algorithms. See [`muladd`](@ref).
"""
function fma end
function fma_emulated(a::T, b::T, c::T) where {T<:Union{Float16, Float32}}
    W = widen(T)
    ab = W(a) * b # exact
    res = ab + c
    bb = res - ab
    err = (ab - (res - bb)) + (c - bb) # exact error of ab + c (TwoSum)
    # Round res to odd, so that rounding it to T (which has at least 2 fewer bits) is correct
    u = reinterpret(Unsigned, res)
    adjust = (abs(err) > 0) & iseven(u) # false if err is zero or NaN (from Inf/NaN inputs)
    u += ifelse(adjust, ifelse(signbit(err) == signbit(res), one(u), -one(u)), zero(u))
    return T(reinterpret(W, u))
end

""" Splits a Float64 into a hi bit and a low bit where the high bit has 27 trailing 0s and the low bit has 26 trailing 0s"""
@inline function splitbits(x::Float64)
    hi = truncbits(x, 27)
    return hi, x-hi
end

# two-product: returns (hi, lo) with hi + lo == x*y exactly. Uses a hardware
# fma when available, otherwise a split-based error-free transformation.
function two_mul(x::T, y::T) where {T<:Number}
    xy = x*y
    xy, fma(x, y, -xy)
end

@assume_effects :consistent @inline function two_mul(x::Float64, y::Float64)
    if Core.Intrinsics.have_fma(Float64)
        xy = x*y
        return xy, fma_float(x, y, -xy)
    end
    # fma-free fallback; `fma_emulated` relies on this branch never calling `fma`
    xhi, xlo = splitbits(x)
    yhi, ylo = splitbits(y)
    xy = x*y
    ylohi, ylolo = splitbits(ylo)
    xylo = xlo*ylohi - (((xy - xhi*yhi) - xlo*yhi) - xhi*ylo) + ylolo*xlo
    return xy, xylo
end

@assume_effects :consistent @inline function two_mul(x::T, y::T) where T<:Union{Float16, Float32}
    if Core.Intrinsics.have_fma(T)
        xy = x*y
        return xy, fma(x, y, -xy)
    end
    xy = widen(x)*y
    Txy = T(xy)
    return Txy, T(xy-Txy)
end

# Error of `r = abhi + c` plus `ablo`, rounded to odd. Rounding to odd (rather than
# nearest) makes `r + fma_correction(...)` round correctly to nearest, since rounding
# `s` to nearest could land on a tie of `r + s` whose direction depends on the lost bits.
@inline function fma_correction(abhi::Float64, ablo::Float64, c::Float64, r::Float64)
    e = (abs(abhi) > abs(c)) ? (abhi-r+c) : (c-r+abhi) # exact
    s = e + ablo
    serr = (abs(e) > abs(ablo)) ? (e-s+ablo) : (ablo-s+e) # exact
    if !iszero(serr) && iseven(reinterpret(UInt64, s))
        s = nextfloat(s, serr > 0 ? 1 : -1)
    end
    return s
end

function fma_emulated(a::Float64, b::Float64,c::Float64)
    abhi, ablo = @inline two_mul(a, b)
    # two_mul is only exact if the low parts of a and b (and their product) don't underflow
    if !isfinite(abhi+c) || isless(abs(abhi), nextfloat(0x1p-969)) || isless(abs(a), 0x1p-969) || isless(abs(b), 0x1p-969)
        aandbfinite = isfinite(a) && isfinite(b)
        if !(isfinite(c) && aandbfinite)
            return aandbfinite ? c : abhi+c
        end
        (iszero(a) || iszero(b)) && return abhi+c
        # The checks above satisfy exponent's nothrow precondition
        bias = Math._exponent_finite_nonzero(a) + Math._exponent_finite_nonzero(b)
        c_denorm = ldexp(c, -bias)
        if isfinite(c_denorm)
            # rescale a and b to [1,2), equivalent to ldexp(a, -exponent(a))
            issubnormal(a) && (a *= 0x1p52)
            issubnormal(b) && (b *= 0x1p52)
            a = reinterpret(Float64, (reinterpret(UInt64, a) & ~Base.exponent_mask(Float64)) | Base.exponent_one(Float64))
            b = reinterpret(Float64, (reinterpret(UInt64, b) & ~Base.exponent_mask(Float64)) | Base.exponent_one(Float64))
            # If c underflowed when rescaled, it is far below every rounding boundary
            # of a*b, so only its sign matters (to break ties). Keep it nonzero.
            c = ldexp(c_denorm, bias) == c ? c_denorm : copysign(floatmin(Float64), c)
            abhi, ablo = two_mul(a, b)
            # abhi <= 4 -> isfinite(r)      (α)
            r = abhi+c
            # s ≈ 0                         (β)
            s = fma_correction(abhi, ablo, c, r)
            # α ⩓ β -> isfinite(sumhi)      (γ)
            sumhi = r+s
            # If result is subnormal, ldexp will cause double rounding because subnormals have fewer mantisa bits.
            # As such, we need to check whether round to even would lead to double rounding and manually round sumhi to avoid it.
            # This must be decided before rounding, which can carry the result to 0 or floatmin.
            # finite: See γ
            if !iszero(sumhi) && (bits_lost = -bias-Math._exponent_finite_nonzero(sumhi)-1022) > 0
                sumlo = r-sumhi+s
                sumhiInt = reinterpret(UInt64, sumhi)
                if (bits_lost != 1) ⊻ (sumhiInt&1 == 1)
                    sumhi = nextfloat(sumhi, cmp(sumlo, 0))
                end
            end
            return ldexp(sumhi, bias)
        end
        isinf(abhi) && signbit(c) == signbit(a*b) && return abhi
        # fall through
    end
    r = abhi+c
    s = fma_correction(abhi, ablo, c, r)
    return r+s
end

# Disable LLVM's fma if it is incorrect, e.g. because LLVM falls back
# onto a broken system libm; if so, use a software emulated fma
@assume_effects :consistent function fma(x::T, y::T, z::T) where {T<:IEEEFloat}
    Core.Intrinsics.have_fma(T) ? fma_float(x,y,z) : fma_emulated(x,y,z)
end

# This is necessary at least on 32-bit Intel Linux, since fma_float may
# have called glibc, and some broken glibc fma implementations don't
# properly restore the rounding mode
Rounding.setrounding_raw(Float32, Rounding.JL_FE_TONEAREST)
Rounding.setrounding_raw(Float64, Rounding.JL_FE_TONEAREST)
