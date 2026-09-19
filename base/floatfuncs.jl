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

The largest consecutive integer-valued floating-point number that is exactly represented in
the given floating-point type `T` (which defaults to `Float64`).

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

# Rounding to digits scales x, rounds to an integer, and scales back. The scaling
# step itself rounds, and if it lands exactly on a rounding boundary (an integer, or
# a half-integer for the nearest modes) the mode can't tell which side the exact
# value was on. floor(x) could end up above x. So `_mul_round` and `_div_round`
# recover the scaling error with an fma and round the exact product or quotient.
# The rounded integer may not be representable, hence the `TwicePrecision`.

# round(s + δ, r) for an infinitesimal δ with the sign of σ (δ == 0 iff σ == 0).
# Branch-free: the sign of σ is unpredictable. Needs eps(s) <= 1 so the step of
# one is representable, and σ to have the sign of s when s is a signed zero;
# callers pass |s| < maxintfloat/2 or |s| <= 4, and σ is the error of s itself.
function _round_sticky(s, σ, ::RoundingMode{:Down})
    f = floor(s)
    return ifelse((f == s) & (σ < 0), f - one(f), f)
end
function _round_sticky(s, σ, ::RoundingMode{:Up})
    c = ceil(s)
    return ifelse((c == s) & (σ > 0), copysign(c + one(c), c), c)
end
function _round_sticky(s, σ, ::RoundingMode{:ToZero})
    t = trunc(s)
    return ifelse((t == s) & !iszero(σ) & (signbit(σ) != signbit(s)), copysign(t + copysign(one(t), σ), s), t)
end
function _round_sticky(s, σ, ::RoundingMode{:FromZero})
    t = round(s, RoundFromZero)
    return ifelse((t == s) & !iszero(σ) & (signbit(σ) == signbit(s)), t + copysign(one(t), σ), t)
end
function _round_sticky(s, σ, r::Union{RoundingMode{:Nearest}, RoundingMode{:NearestTiesAway}, RoundingMode{:NearestTiesUp}})
    half = oftype(s, 0.5)
    tie = (abs(s - trunc(s)) == half) & !iszero(σ)
    return ifelse(tie, copysign(s + copysign(half, σ), s), round(s, r))
end

# parity of an integer-valued float, without converting to an Integer
_iseven_integral(s) = s == 2 * trunc(s / 2)

# round(s + err + δ, r) - s for an integer-valued s, |err| < |s|, and an infinitesimal
# δ with the sign of σ. The result is an integer, but s plus it may not be representable.
function _round_offset(s, err, σ, r::RoundingMode)
    # Only the fractional part of err and the sign and parity of s + trunc(err) matter,
    # so swap the big integer for a small one with the same sign and parity.
    ei = trunc(err)
    ef = err - ei                              # exact
    b = copysign(oftype(s, _iseven_integral(s) == _iseven_integral(ei) ? 2 : 3), s)
    t = b + ef
    e = (b - t) + ef                           # exact: b + ef == t + e
    return ei + (_round_sticky(t, iszero(e) ? σ : e, r) - b)
end

# round(x * y, r) applied to the exact product. Exact unless x * y underflows,
# which loses the sign of err; here y is an integer, so it never does.
@inline function _mul_round(x, y, r::RoundingMode)
    xy, err = two_mul(x, y)          # x * y == xy + err
    isfinite(xy) || return TwicePrecision(xy, zero(xy))
    if abs(xy) < maxintfloat(xy) / 2
        return TwicePrecision(_round_sticky(xy, err, r), zero(xy))
    end
    return TwicePrecision(xy, _round_offset(xy, err, zero(err), r))
end

# round(x / y, r) applied to the exact quotient. Exact for |x / y| < maxintfloat^2 / 4;
# past that the rounded quotient generally doesn't fit in a TwicePrecision, and the
# result is the closest one to x / y instead (within eps(lo) / 2 + 1 of the rounded quotient).
@inline function _div_round(x, y, r::RoundingMode)
    q = x / y
    (isfinite(q) && isfinite(y)) || return TwicePrecision(round(q, r), zero(q))
    rem = fma(-q, y, x)              # x == q * y + rem
    if abs(q) < maxintfloat(q) / 2
        return TwicePrecision(_round_sticky(q, flipsign(rem, y), r), zero(q))
    end
    # q is an integer and |rem / y| <= eps(q) / 2
    δ = rem / y
    if abs(δ) >= maxintfloat(q) / 2
        return TwicePrecision(q, δ)  # δ is already an integer
    end
    ρ = fma(-δ, y, rem)              # rem == δ * y + ρ
    return TwicePrecision(q, _round_offset(q, δ, flipsign(ρ, y), r))
end

# (v.hi + v.lo) / y as a float, for v from `_mul_round`. When v.lo != 0 this rounds
# twice. With an integer numerator and y = 10^d the exact quotient is at least
# ulp/(2*5^d) from any midpoint, which is beyond the error in c for d <= 21; for
# d == 22 the margin is gone on paper but no counterexample has been found.
@inline function _tp_div(v::TwicePrecision, y)
    hi = v.hi / y
    iszero(v.lo) && return hi
    rq = fma(-hi, y, v.hi)           # exact: v.hi == hi * y + rq
    c = (rq + v.lo) / y
    return hi + c
end

# (v.hi + v.lo) * y as a float, for v from `_div_round`. Correctly rounded when
# pe + v.lo * y is exact: both are multiples of 2^d for y = 10^d, and with |v.lo| <= 2
# (the fast path in `_round_step` keeps |v.hi| < 2^55) the sum fits for d <= 21.
@inline function _tp_mul(v::TwicePrecision, y)
    iszero(v.lo) && return v.hi * y
    p, pe = two_mul(v.hi, y)         # exact: v.hi * y == p + pe
    return p + (pe + v.lo * y)
end

# round x to multiples of 1/invstep
function _round_invstep(x, invstep, r::RoundingMode)
    # If 1/invstep is less than a quarter of the spacing of floats around x, the
    # rounded value is within a quarter ulp of x and converts back to x, so skip the
    # work. This also covers x * invstep overflowing.
    eps(x) * abs(invstep) >= 4 && return x
    return _tp_div(_mul_round(x, invstep, r), invstep)
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
    if isfinite(step)
        # step is less than a quarter of the spacing of floats around x: the result is x
        eps(x) >= 4 * abs(step) && return x
        y = _tp_mul(_div_round(x, step, r), step)
    else
        y = round(x / step, r) * step
    end
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

# C-style round
function round(x::AbstractFloat, ::RoundingMode{:NearestTiesAway})
    y = trunc(x)
    ifelse(x==y,y,trunc(2*x-y))
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
    isapprox(x, y; atol::Real=0, rtol::Real=atol>0 ? 0 : √eps, nans::Bool=false[, norm::Function])

Inexact equality comparison. Two numbers compare equal if their relative distance *or* their
absolute distance is within tolerance bounds: `isapprox` returns `true` if
`norm(x-y) <= max(atol, rtol*max(norm(x), norm(y)))`. The default `atol` (absolute tolerance) is zero and the
default `rtol` (relative tolerance) depends on the types of `x` and `y`. The keyword argument `nans` determines
whether or not NaN values are considered equal (defaults to false).

For real or complex floating-point values, if an `atol > 0` is not specified, `rtol` defaults to
the square root of [`eps`](@ref) of the type of `x` or `y`, whichever is bigger (least precise).
This corresponds to requiring equality of about half of the significant digits. Otherwise,
e.g. for integer arguments or if an `atol > 0` is supplied, `rtol` defaults to zero.

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
                  atol::Real=0, rtol::Real=rtoldefault(x,y,atol),
                  nans::Bool=false, norm::Function=abs)
    x′, y′ = promote(x, y) # to avoid integer overflow
    x == y ||
        (isfinite(x) && isfinite(y) && norm(x-y) <= max(atol, rtol*max(norm(x′), norm(y′)))) ||
         (nans && isnan(x) && isnan(y))
end

function isapprox(x::Integer, y::Integer;
                  atol::Real=0, rtol::Real=rtoldefault(x,y,atol),
                  nans::Bool=false, norm::Function=abs)
    if norm === abs && atol < 1 && rtol == 0
        return x == y
    else
        # We need to take the difference `max` - `min` when comparing unsigned integers.
        _x, _y = x < y ? (x, y) : (y, x)
        return norm(_y - _x) <= max(atol, rtol*max(norm(_x), norm(_y)))
    end
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
function rtoldefault(x::Union{T,Type{T}}, y::Union{S,Type{S}}, atol::Real) where {T<:Number,S<:Number}
    rtol = max(rtoldefault(real(T)),rtoldefault(real(S)))
    return atol > 0 ? zero(rtol) : rtol
end

# fused multiply-add

"""
    fma(x, y, z)

Compute `x*y+z` without rounding the intermediate result `x*y`. On some systems this is
significantly more expensive than `x*y+z`. `fma` is used to improve accuracy in certain
algorithms. See [`muladd`](@ref).
"""
function fma end
function fma_emulated(a::Float16, b::Float16, c::Float16)
    Float16(muladd(Float32(a), Float32(b), Float32(c))) #don't use fma if the hardware doesn't have it.
end
function fma_emulated(a::Float32, b::Float32, c::Float32)::Float32
    ab = Float64(a) * b
    res = ab+c
    reinterpret(UInt64, res)&0x1fff_ffff!=0x1000_0000 && return res
    # yes error compensation is necessary. It sucks
    reslo = abs(c)>abs(ab) ? ab-(res - c) : c-(res - ab)
    res = iszero(reslo) ? res : (signbit(reslo) ? prevfloat(res) : nextfloat(res))
    return res
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

function fma_emulated(a::Float64, b::Float64,c::Float64)
    abhi, ablo = @inline two_mul(a, b)
    if !isfinite(abhi+c) || isless(abs(abhi), nextfloat(0x1p-969)) || issubnormal(a) || issubnormal(b)
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
            c = c_denorm
            abhi, ablo = two_mul(a, b)
            # abhi <= 4 -> isfinite(r)      (α)
            r = abhi+c
            # s ≈ 0                         (β)
            s = (abs(abhi) > abs(c)) ? (abhi-r+c+ablo) : (c-r+abhi+ablo)
            # α ⩓ β -> isfinite(sumhi)      (γ)
            sumhi = r+s
            # If result is subnormal, ldexp will cause double rounding because subnormals have fewer mantisa bits.
            # As such, we need to check whether round to even would lead to double rounding and manually round sumhi to avoid it.
            if issubnormal(ldexp(sumhi, bias))
                sumlo = r-sumhi+s
                # finite: See γ
                # non-zero: If sumhi == ±0., then ldexp(sumhi, bias) == ±0,
                # so we don't take this branch.
                bits_lost = -bias-Math._exponent_finite_nonzero(sumhi)-1022
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
    s = (abs(abhi) > abs(c)) ? (abhi-r+c+ablo) : (c-r+abhi+ablo)
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
