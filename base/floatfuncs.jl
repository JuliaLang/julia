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
maxintfloat(::Type{Union{}}, slurp...) = throw(MethodError(maxintfloat, (Union{}, slurp...)))
maxintfloat(x, ::Type{Union{}}, slurp...) = throw(MethodError(maxintfloat, (x, Union{}, slurp...)))

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

# round x to multiples of 1/base^digits when base^digits overflows, for float types other
# than Float32 and Float64, which `_round_digits_dd` rounds exactly. Only tiny x have
# anything to round, so move a power of two from base^digits onto x, exactly in both
# cases: base^digits == M * 2^(k*digits) with M = (base/2^k)^digits, and x * base^digits
# == ldexp(x, s) * invstep. The odd part of base^digits is far beyond the precision of x
# here, so invstep is inexact and the grid is approximate, but the rounding mode is
# applied exactly to it: floor(x) <= x <= ceil(x) still holds.
function _round_invstep_overflow(x, base, digits::Integer, r::RoundingMode)
    base >= 2 || return x                  # not a meaningful base
    # 2^k <= base < 2^(k+1), taken from the integer since base itself may overflow
    k = top_set_bit(base) - 1
    ee = exponent(eps(x))
    # eps(x) * base^digits >= 2^(ee + k*digits) >= 8: the result is x (see `_round_invstep`)
    digits >= cld(3 - ee, k) && return x
    m = ldexp(oftype(x, base >> max(k - 63, 0)), -min(k, 63))   # base / 2^k, in [1, 2]
    M = m^Int(digits)                      # in [1, 2^digits], no overflow
    eM = exponent(M)
    s = k * Int(digits) + eM - (precision(x) + 2)
    invstep = ldexp(M, precision(x) + 2 - eM)
    # s > 0 is large enough that ldexp(x, s) is normal, and |ldexp(x, s)| < 2^eM
    return ldexp(_tp_div(_mul_round(ldexp(x, s), invstep, r), invstep), -s)
end

# round x to multiples of step
function _round_step(x, step, r::RoundingMode)
    if isfinite(step)
        # step is less than a quarter of the spacing of floats around x: the result is x
        eps(x) >= 4 * abs(step) && return x
        v = _div_round(x, step, r)
        y = _tp_mul(v, step)
        isfinite(y) || return _round_step_nonfinite(x, step, r, v)
    else
        y = round(x / step, r) * step
        isfinite(y) || return _round_step_nonfinite(x, step, r, nothing)
    end
    return y
end

# the result of `_round_step` when the rounded value is not finite as a float
@noinline function _round_step_nonfinite(x, step, r::RoundingMode, v)
    (isfinite(x) && !iszero(x)) || return x
    if v !== nothing
        # the product in `_tp_mul` can overflow although the rounded value is finite, as
        # the multiple of step that `_div_round` chose may be near floatmax; scaling the
        # step by a power of two changes nothing else (step >= 2, so step / 4 is normal)
        y = ldexp(_tp_mul(v, ldexp(step, -2)), 2)
        isfinite(y) && return y
    end
    # With a finite step, the rounded value is a nonzero multiple of step beyond floatmax,
    # so the result is ±Inf. With an infinite step, the rounded value is 0 or ±step: ±Inf
    # when rounding away from zero, and 0 otherwise. For the nearest modes that assumes
    # |x| < step / 2, which can fail only for a base whose first power beyond floatmax is
    # below 2 * floatmax (Float32 and Float64 take `_round_digits_dd` then).
    if isfinite(step) || r == RoundFromZero || r == (x > 0 ? RoundUp : RoundDown)
        return copysign(oftype(x, Inf), x)
    end
    return copysign(zero(x), x)
end

# When base^k is not exact in the float type, the float step is off its exact value and
# rounding to it rounds to the wrong grid: `floor(floatmax(); digits=-299)` would be a
# multiple of 1.0e299, not of 10^299. With base == bo * 2^t for odd bo, base^k == o * 2^j
# for o == bo^k and j == t * k. Then o is carried as a double-double, exact for o < 2^106
# and within a relative 2^-100 beyond, x is scaled by 2^∓j exactly, and the rounded
# quotient or product N, and the result from it, are computed with a margin for the
# error. When a value is within the margin of a rounding boundary or of a midpoint
# between floats, exact integer arithmetic decides (`_round_digits_exact`). The odd part
# o has more bits than the float, so x / base^k is never on a boundary, but N * base^k
# can be a midpoint: 10^23 is.

# whether `_round_digits_dd` handles x and base; other float types and bases are left
# to the float code
_has_dd_path(x, base) = x isa Union{Float32, Float64} && base isa Integer && base >= 2

# whether the float power pow == base^k is exact and finite for certain, with one
# comparison: it is faithful, so below maxintfloat exactly when base^k is, and exact then.
# Other powers take `_round_digits_inexact`. Other float types only need it finite.
@inline _pow_is_small(x::Union{Float32, Float64}, pow) = abs(pow) < maxintfloat(x)
@inline _pow_is_small(x, pow) = isfinite(pow)

# whether bo^k < 2^p for the odd part bo of base, so that base^k is exact in precision p
@noinline function _odd_pow_is_exact(base::Integer, k::Integer, p::Int)
    bo = base >> trailing_zeros(base)
    bo == 1 && return true
    k * (top_set_bit(bo) - 1) >= p && return false  # bo^k >= 2^p
    # bo < 2^p and k < p here; bo^k by squaring, where any overflow means bo^k >= 2^64
    o, b, k = UInt64(1), UInt64(bo), Int(k)
    while true
        if isodd(k)
            o, f = Checked.mul_with_overflow(o, b)
            f && return false
        end
        k >>= 1
        k == 0 && return o < UInt64(1) << p
        b, f = Checked.mul_with_overflow(b, b)
        f && return false
    end
end

# function of mpfr.jl: round(x, r; digits, base) with integer arithmetic
function _round_digits_exact end

# the rounding modes that `_round_digits_dd` supports
const _DigitsRoundingModes = Union{RoundingMode{:Down}, RoundingMode{:Up}, RoundingMode{:ToZero},
    RoundingMode{:FromZero}, RoundingMode{:Nearest}, RoundingMode{:NearestTiesAway},
    RoundingMode{:NearestTiesUp}}

function _round_digits_dd(x::T, r::RoundingMode, digits::Integer,
                          base::Integer) where {T<:Union{Float32, Float64}}
    r isa _DigitsRoundingModes || throw(ArgumentError("unsupported rounding mode $r"))
    # base^|digits| >= 2^|digits|, against eps(x) >= 2^-1074 and |x| < 2^1024
    if digits > 1100
        return x                                    # the step is far below eps(x)
    elseif digits < -1100
        n = _round_dd_small(Float64(x), r)          # |x| is far below half the step
        return iszero(n) ? copysign(zero(T), x) : copysign(T(Inf), x)
    end
    tz = trailing_zeros(base)
    bo = base >> tz
    bo < Int64(2)^53 || return _round_digits_exact(x, r, digits, base)
    k = Int(abs(widen(digits)))                     # abs(typemin(Int8)) < 0
    j = tz * k
    oh, ol = _odd_pow_dd(bo, k)
    xf = Float64(x)
    ex = Float64(eps(x))
    if !isfinite(oh)
        # o >= 2^1023: settle the common cases from that bound before the integers
        if digits >= 0
            ldexp(ex, 1023 + j) >= 4 && return x
        elseif abs(xf) < ldexp(1.0, 1021 + j)       # |x| < base^k / 4
            n = _round_dd_small(xf, r)
            return iszero(n) ? copysign(zero(T), x) : copysign(T(Inf), x)
        end
        return _round_digits_exact(x, r, digits, base)
    end
    if digits < 0
        # N = round(x / base^k), the result N * base^k
        # the result is x when the step is below a quarter of the spacing of floats around x
        _ldexp(ex, -j) >= 4 * oh && return x
        s = _ldexp(xf, -j)                          # |x / 2^j| >= o / 4 is normal
        if abs(s) < oh / 4
            n = _round_dd_small(xf, r)
        else
            q = s / oh
            n = _round_dd_int(q, (fma(-q, oh, s) - q * ol) / oh, r, xf)
        end
        n === nothing && return _round_digits_exact(x, r, digits, base)
        iszero(n) && return copysign(zero(T), x)
        nh = Float64(n)
        nl = Float64(n - Int64(nh))
        h, e = two_mul(nh, oh)
        u = _dd_to_float(T, h, e + (nh * ol + nl * oh))
        if u === nothing
            # N * o can be halfway between floats, but only when o < 2^54; the product is
            # exact in UInt128 then, and its conversion rounds correctly
            oh < 0x1p71 || return _round_digits_exact(x, r, digits, base)
            u = copysign(T(UInt128(abs(n)) * UInt128(bo)^k), x)
        end
        return _ldexp(u, j)                         # exact, or overflows to ±Inf
    else
        # N = round(x * base^k), the result N / base^k
        _ldexp(ex, j) * oh >= 4 && return x
        s = _ldexp(xf, j)                           # |x * 2^j| < 2^56
        h, e = two_mul(s, oh)
        n = abs(h) < 0.25 ? _round_dd_small(xf, r) : _round_dd_int(h, e + s * ol, r, xf)
        n === nothing && return _round_digits_exact(x, r, digits, base)
        iszero(n) && return copysign(zero(T), x)
        nh = Float64(n)
        nl = Float64(n - Int64(nh))
        q = nh / oh
        u = _dd_to_float(T, q, (fma(-q, oh, nh) + nl - q * ol) / oh)
        # ldexp is exact unless the result is subnormal
        (u === nothing || exponent(u) - j < exponent(floatmin(T))) &&
            return _round_digits_exact(x, r, digits, base)
        return _ldexp(u, -j)
    end
end

# ldexp(x, j), as a multiplication when 2^j is a normal Float64
@inline _ldexp(x::Float64, j::Int) =
    -1022 <= j <= 1023 ? x * reinterpret(Float64, UInt64(j + 1023) << 52) : ldexp(x, j)
@inline _ldexp(x::Float32, j::Int) =
    -126 <= j <= 127 ? x * reinterpret(Float32, UInt32(j + 127) << 23) : ldexp(x, j)

# b^k as a double-double, for an odd integer b < 2^53, with a relative error below 2^-100
# for k < 2^11 (exact while b^k < 2^106)
function _dd_pow(b::Float64, k::Int)
    rh, rl = 1.0, 0.0
    ph, pl = b, 0.0
    while true
        isodd(k) && ((rh, rl) = _dd_mul(rh, rl, ph, pl))
        k >>= 1
        k == 0 && return rh, rl
        ph, pl = _dd_mul(ph, pl, ph, pl)
    end
end
@inline function _dd_mul(ah, al, bh, bl)
    p, e = two_mul(ah, bh)
    e += ah * bl + al * bh
    h = p + e
    return h, e - (h - p)
end
# 5^k for base 10, the common case (the table follows `two_mul` below)
@inline _odd_pow_dd(bo, k::Int) = bo == 5 && k < 64 ? @inbounds(_POW5_DD[k + 1]) : _dd_pow(Float64(bo), k)

# round(±δ, r) for |δ| < 1/4 with the sign of x
_round_dd_small(x::Float64, r::RoundingMode) = Int64(_round_sticky(copysign(0.0, x), x, r))

# round(q + q2, r) as an Int64, for the double-double q + q2 with |q| < 2^56 that
# approximates a value with the sign of x and never on a rounding boundary, or nothing
# when it is too close to one to tell. The distance to the boundary is measured from the
# nearest integer n0 to q, so values close to an integer, as from decimal literals, are
# told apart up to the error of q + q2 itself rather than that of its fraction.
function _round_dd_int(q::Float64, q2::Float64, r::RoundingMode, x::Float64)
    n0 = round(q)
    d = (q - n0) + q2                               # q - n0 is exact
    rd = round(d)
    n = Int64(n0) + Int64(rd)
    d2 = d - rd                                     # exact, in [-1/2, 1/2]
    m = abs(q) * 0x1p-90 + (abs(d) + abs(q2)) * 0x1p-50
    if r isa Union{RoundingMode{:Nearest}, RoundingMode{:NearestTiesAway}, RoundingMode{:NearestTiesUp}}
        0.5 - abs(d2) < m && return nothing         # n is the nearest integer
        return n
    end
    abs(d2) < m && return nothing
    fl = d2 < 0 ? n - 1 : n                         # floor
    up = r === RoundUp || (r === RoundFromZero && x > 0) || (r === RoundToZero && x < 0)
    return up ? fl + 1 : fl
end

# h + l rounded to nearest in T, for a double-double with a relative error below 2^-95,
# or nothing when it is too close to a midpoint between floats to tell
function _dd_to_float(::Type{T}, h::Float64, l::Float64) where {T<:Union{Float32, Float64}}
    u = T(h + l)
    if !isfinite(u)
        # beyond floatmax(T), well past where it rounds to Inf
        return abs(h) >= 2 * Float64(floatmax(T)) ? copysign(T(Inf), h) : nothing
    end
    d = (h - Float64(u)) + l                        # h - u is exact
    up = Float64(nextfloat(u)) - Float64(u)
    dn = Float64(u) - Float64(prevfloat(u))
    isfinite(up) || (up = dn)
    isfinite(dn) || (dn = up)
    m = abs(h) * 0x1p-94
    (-dn / 2 + m < d < up / 2 - m) || return nothing
    return u
end

function _round_digits(x, r::RoundingMode, digits::Integer, base)
    fx = float(x)
    fx isa Union{IEEEFloat, BigFloat} || return _round_digits_approx(fx, r, digits, base)
    if digits >= 0
        pow = oftype(fx, base)^digits                # 1 / step
        _pow_is_small(fx, pow) && return _round_invstep(fx, pow, r)
    else
        # -digits overflows for typemin; base^-widen(digits) is then Inf or 1 anyway
        pow = oftype(fx, base)^(digits isa BitSigned && digits == typemin(digits) ? -widen(digits) : -digits)
        _pow_is_small(fx, pow) && return _round_step(fx, pow, r)
    end
    return _round_digits_inexact(fx, r, digits, base, pow)
end

# base^|digits| is large: it may overflow or not be exact
@noinline function _round_digits_inexact(x, r::RoundingMode, digits::Integer, base, pow)
    if digits >= 0
        invstep = pow
        if _has_dd_path(x, base) && !(isfinite(invstep) && _odd_pow_is_exact(base, digits, precision(x)))
            return _round_digits_dd(x, r, digits, base)
        end
        return isfinite(invstep) ? _round_invstep(x, invstep, r) : _round_invstep_overflow(x, base, digits, r)
    else
        step = pow
        if _has_dd_path(x, base) && !(isfinite(step) && _odd_pow_is_exact(base, -digits, precision(x)))
            return _round_digits_dd(x, r, digits, base)
        end
        return _round_step(x, step, r)
    end
end
# Float16 rounds natively while base^|digits| is below maxintfloat(Float16) == 2048, so
# it is exact (for base 10, |digits| <= 3; for any |base| >= 2, |digits| <= 10). Past
# that it is inexact, and 10^digits overflows from digits == 5, which ordinary values
# reach. Those round in Float64 instead, which holds base^digits exactly for every
# digits that matters to a Float16. The exactly rounded value is either a Float16
# midpoint or at least ulp/(2*5^digits) away from one, so rounding the Float64 result
# to Float16 gives the Float16 closest to it.
function _round_digits(x::Float16, r::RoundingMode, digits::Integer, base)
    if -11 < digits < 11
        pow = Float16(base)^Int(abs(digits))
        if abs(pow) < maxintfloat(Float16)
            return digits >= 0 ? _round_invstep(x, pow, r) : _round_step(x, pow, r)
        end
    end
    return Float16(_round_digits(Float64(x), r, digits, base))
end

# Rounding exactly needs a correctly rounded `fma` (a `muladd` stand-in would not be
# exact), along with `eps`, `maxintfloat` and friends, which other AbstractFloat types
# need not define. Those keep the previous formulas, which round the float product
# x * base^digits rather than the exact value.
function _round_digits_approx(x, r::RoundingMode, digits::Integer, base)
    if digits >= 0
        invstep = oftype(x, base)^digits
        if isfinite(invstep)
            y = round(x * invstep, r) / invstep
        else
            # square root of invstep, to avoid overflowing
            invstepsqrt = oftype(x, base)^oftype(x, digits/2)
            y = round((x * invstepsqrt) * invstepsqrt, r) / invstepsqrt / invstepsqrt
        end
        return isfinite(y) ? y : x
    else
        step = oftype(x, base)^-digits
        y = round(x / step, r) * step
        isfinite(y) && return y
        x > 0 && return r == RoundUp ? oftype(x, Inf) : zero(x)
        x < 0 && return r == RoundDown ? -oftype(x, Inf) : -zero(x)
        return x
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

# 5^k as double-doubles for `_odd_pow_dd`
const _POW5_DD = ntuple(i -> _dd_pow(5.0, i - 1), 64)

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
