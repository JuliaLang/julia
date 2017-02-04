# This file is a part of Julia. License is MIT: http://julialang.org/license

module Math

export sin, cos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot,
       sech, csch, coth, asech, acsch, acoth,
       sinpi, cospi, sinc, cosc,
       cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       rad2deg, deg2rad,
       log, log2, log10, log1p, exponent, exp, exp2, exp10, expm1,
       cbrt, sqrt, erf, erfc, erfcx, erfi, dawson,
       significand,
       lgamma, hypot, gamma, lfact, max, min, minmax, ldexp, frexp,
       clamp, clamp!, modf, ^, mod2pi, rem2pi,
       airyai, airyaiprime, airybi, airybiprime,
       airyaix, airyaiprimex, airybix, airybiprimex,
       besselj0, besselj1, besselj, besseljx,
       bessely0, bessely1, bessely, besselyx,
       hankelh1, hankelh2, hankelh1x, hankelh2x,
       besseli, besselix, besselk, besselkx, besselh, besselhx,
       beta, lbeta, eta, zeta, polygamma, invdigamma, digamma, trigamma,
       erfinv, erfcinv, @evalpoly

import Base: log, exp, sin, cos, tan, sinh, cosh, tanh, asin,
             acos, atan, asinh, acosh, atanh, sqrt, log2, log10,
             max, min, minmax, ^, exp2, muladd, rem,
             exp10, expm1, log1p

using Base: sign_mask, exponent_mask, exponent_one, exponent_bias,
            exponent_half, exponent_max, exponent_raw_max, fpinttype,
            significand_mask, significand_bits, exponent_bits

using Core.Intrinsics: sqrt_llvm, powi_llvm

typealias IEEEFloat Union{Float16,Float32,Float64}
# non-type specific math functions

"""
    clamp(x, lo, hi)

Return `x` if `lo <= x <= hi`. If `x < lo`, return `lo`. If `x > hi`, return `hi`. Arguments
are promoted to a common type.

```jldoctest
julia> clamp.([pi, 1.0, big(10.)], 2., 9.)
3-element Array{BigFloat,1}:
 3.141592653589793238462643383279502884197169399375105820974944592307816406286198
 2.000000000000000000000000000000000000000000000000000000000000000000000000000000
 9.000000000000000000000000000000000000000000000000000000000000000000000000000000
```
"""
clamp{X,L,H}(x::X, lo::L, hi::H) =
    ifelse(x > hi, convert(promote_type(X,L,H), hi),
           ifelse(x < lo,
                  convert(promote_type(X,L,H), lo),
                  convert(promote_type(X,L,H), x)))

"""
    clamp!(array::AbstractArray, lo, hi)

Restrict values in `array` to the specified range, in-place.
See also [`clamp`](@ref).
"""
function clamp!{T}(x::AbstractArray{T}, lo, hi)
    @inbounds for i in eachindex(x)
        x[i] = clamp(x[i], lo, hi)
    end
    x
end

# evaluate p[1] + x * (p[2] + x * (....)), i.e. a polynomial via Horner's rule
macro horner(x, p...)
    ex = esc(p[end])
    for i = length(p)-1:-1:1
        ex = :(muladd(t, $ex, $(esc(p[i]))))
    end
    Expr(:block, :(t = $(esc(x))), ex)
end

# Evaluate p[1] + z*p[2] + z^2*p[3] + ... + z^(n-1)*p[n].  This uses
# Horner's method if z is real, but for complex z it uses a more
# efficient algorithm described in Knuth, TAOCP vol. 2, section 4.6.4,
# equation (3).

"""
    @evalpoly(z, c...)

Evaluate the polynomial ``\\sum_k c[k] z^{k-1}`` for the coefficients `c[1]`, `c[2]`, ...;
that is, the coefficients are given in ascending order by power of `z`.  This macro expands
to efficient inline code that uses either Horner's method or, for complex `z`, a more
efficient Goertzel-like algorithm.

```jldoctest
julia> @evalpoly(3, 1, 0, 1)
10

julia> @evalpoly(2, 1, 0, 1)
5

julia> @evalpoly(2, 1, 1, 1)
7
```
"""
macro evalpoly(z, p...)
    a = :($(esc(p[end])))
    b = :($(esc(p[end-1])))
    as = []
    for i = length(p)-2:-1:1
        ai = Symbol("a", i)
        push!(as, :($ai = $a))
        a = :(muladd(r, $ai, $b))
        b = :($(esc(p[i])) - s * $ai) # see issue #15985 on fused mul-subtract
    end
    ai = :a0
    push!(as, :($ai = $a))
    C = Expr(:block,
             :(x = real(tt)),
             :(y = imag(tt)),
             :(r = x + x),
             :(s = muladd(x, x, y*y)),
             as...,
             :(muladd($ai, tt, $b)))
    R = Expr(:macrocall, Symbol("@horner"), :tt, map(esc, p)...)
    :(let tt = $(esc(z))
          isa(tt, Complex) ? $C : $R
      end)
end

"""
    rad2deg(x)

Convert `x` from radians to degrees.

```jldoctest
julia> rad2deg(pi)
180.0
```
"""
rad2deg(z::AbstractFloat) = z * (180 / oftype(z, pi))

"""
    deg2rad(x)

Convert `x` from degrees to radians.

```jldoctest
julia> deg2rad(90)
1.5707963267948966
```
"""
deg2rad(z::AbstractFloat) = z * (oftype(z, pi) / 180)
rad2deg(z::Real) = rad2deg(float(z))
deg2rad(z::Real) = deg2rad(float(z))

log{T<:Number}(b::T, x::T) = log(x)/log(b)

"""
    log(b,x)

Compute the base `b` logarithm of `x`. Throws [`DomainError`](@ref) for negative `Real` arguments.

```jldoctest
julia> log(4,8)
1.5

julia> log(4,2)
0.5
```

!!! note
    If `b` is a power of 2 or 10, [`log2`](@ref) or [`log10`](@ref) should be used, as these will
    typically be faster and more accurate. For example,

    ```jldoctest
    julia> log(100,1000000)
    2.9999999999999996

    julia> log10(1000000)/2
    3.0
    ```
"""
log(b::Number, x::Number) = log(promote(b,x)...)

# type specific math functions

const libm = Base.libm_name
const openspecfun = "libopenspecfun"

# functions with no domain error
"""
    sinh(x)

Compute hyperbolic sine of `x`.
"""
sinh(x)

"""
    cosh(x)

Compute hyperbolic cosine of `x`.
"""
cosh(x)

"""
    tanh(x)

Compute hyperbolic tangent of `x`.
"""
tanh(x)

"""
    atan(x)

Compute the inverse tangent of `x`, where the output is in radians.
"""
atan(x)

"""
    asinh(x)

Compute the inverse hyperbolic sine of `x`.
"""
asinh(x)

"""
    erf(x)

Compute the error function of `x`, defined by ``\\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt``
for arbitrary complex `x`.
"""
erf(x)

"""
    erfc(x)

Compute the complementary error function of `x`, defined by ``1 - \\operatorname{erf}(x)``.
"""
erfc(x)

"""
    expm1(x)

Accurately compute ``e^x-1``.
"""
expm1(x)
for f in (:cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :erf, :erfc, :exp2, :expm1)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end
# pure julia exp function
include("special/exp.jl")
exp(x::Real) = exp(float(x))

# fallback definitions to prevent infinite loop from $f(x::Real) def above

"""
    cbrt(x::Real)

Return the cube root of `x`, i.e. ``x^{1/3}``. Negative values are accepted
(returning the negative real root when ``x < 0``).

The prefix operator `∛` is equivalent to `cbrt`.

```jldoctest
julia> cbrt(big(27))
3.000000000000000000000000000000000000000000000000000000000000000000000000000000
```
"""
cbrt(x::AbstractFloat) = x < 0 ? -(-x)^(1//3) : x^(1//3)

"""
    exp2(x)

Compute ``2^x``.

```jldoctest
julia> exp2(5)
32.0
```
"""
exp2(x::AbstractFloat) = 2^x
for f in (:sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :expm1)
    @eval ($f)(x::AbstractFloat) = error("not implemented for ", typeof(x))
end

# functions with special cases for integer arguments
@inline function exp2(x::Base.BitInteger)
    if x > 1023
        Inf64
    elseif x <= -1023
        # if -1073 < x <= -1023 then Result will be a subnormal number
        # Hex literal with padding must be used to work on 32bit machine
        reinterpret(Float64, 0x0000_0000_0000_0001 << ((x + 1074)) % UInt)
    else
        # We will cast everything to Int64 to avoid errors in case of Int128
        # If x is a Int128, and is outside the range of Int64, then it is not -1023<x<=1023
        reinterpret(Float64, (exponent_bias(Float64) + (x % Int64)) << (significand_bits(Float64)) % UInt)
    end
end

# TODO: GNU libc has exp10 as an extension; should openlibm?
exp10(x::Float64) = 10.0^x
exp10(x::Float32) = 10.0f0^x
exp10(x::Integer) = exp10(float(x))

# utility for converting NaN return to DomainError
@inline nan_dom_err(f, x) = isnan(f) & !isnan(x) ? throw(DomainError()) : f

# functions that return NaN on non-NaN argument for domain error
"""
    sin(x)

Compute sine of `x`, where `x` is in radians.
"""
sin(x)

"""
    cos(x)

Compute cosine of `x`, where `x` is in radians.
"""
cos(x)

"""
    tan(x)

Compute tangent of `x`, where `x` is in radians.
"""
tan(x)

"""
    asin(x)

Compute the inverse sine of `x`, where the output is in radians.
"""
asin(x)

"""
    acos(x)

Compute the inverse cosine of `x`, where the output is in radians
"""
acos(x)

"""
    acosh(x)

Compute the inverse hyperbolic cosine of `x`.
"""
acosh(x)

"""
    atanh(x)

Compute the inverse hyperbolic tangent of `x`.
"""
atanh(x)

"""
    log(x)

Compute the natural logarithm of `x`. Throws [`DomainError`](@ref) for negative `Real` arguments.
Use complex negative arguments to obtain complex results.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log(x)

"""
    log2(x)

Compute the logarithm of `x` to base 2. Throws [`DomainError`](@ref) for negative `Real` arguments.

```jldoctest
julia> log2(4)
2.0

julia> log2(10)
3.321928094887362
```
"""
log2(x)

"""
    log10(x)

Compute the logarithm of `x` to base 10.
Throws [`DomainError`](@ref) for negative `Real` arguments.

```jldoctest
julia> log10(100)
2.0

julia> log10(2)
0.3010299956639812
```
"""
log10(x)

"""
    log1p(x)

Accurate natural logarithm of `1+x`. Throws [`DomainError`](@ref) for `Real` arguments less than -1.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log1p(x)
for f in (:sin, :cos, :tan, :asin, :acos, :acosh, :atanh, :log, :log2, :log10,
          :lgamma, :log1p)
    @eval begin
        ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)),libm), Float64, (Float64,), x), x)
        ($f)(x::Float32) = nan_dom_err(ccall(($(string(f,"f")),libm), Float32, (Float32,), x), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end

sqrt(x::Float64) = sqrt_llvm(x)
sqrt(x::Float32) = sqrt_llvm(x)

"""
    sqrt(x)

Return ``\\sqrt{x}``. Throws [`DomainError`](@ref) for negative `Real` arguments. Use complex
negative arguments instead.  The prefix operator `√` is equivalent to `sqrt`.
"""
sqrt(x::Real) = sqrt(float(x))

"""
    hypot(x, y)

Compute the hypotenuse ``\\sqrt{x^2+y^2}`` avoiding overflow and underflow.
"""
hypot(x::Number, y::Number) = hypot(promote(x, y)...)
function hypot{T<:Number}(x::T, y::T)
    ax = abs(x)
    ay = abs(y)
    if ax < ay
        ax, ay = ay, ax
    end
    if ax == 0
        r = ay / one(ax)
    else
        r = ay / ax
    end

    rr = ax * sqrt(1 + r * r)

    # Use type of rr to make sure that return type is the same for
    # all branches
    if isnan(r)
        isinf(ax) && return oftype(rr, Inf)
        isinf(ay) && return oftype(rr, Inf)
        return oftype(rr, r)
    else
        return rr
    end
end

"""
    hypot(x...)

Compute the hypotenuse ``\\sqrt{\\sum x_i^2}`` avoiding overflow and underflow.
"""
hypot(x::Number...) = vecnorm(x)

"""
    atan2(y, x)

Compute the inverse tangent of `y/x`, using the signs of both `x` and `y` to determine the
quadrant of the return value.
"""
atan2(y::Real, x::Real) = atan2(promote(float(y),float(x))...)
atan2{T<:AbstractFloat}(y::T, x::T) = Base.no_op_err("atan2", T)

atan2(y::Float64, x::Float64) = ccall((:atan2,libm), Float64, (Float64, Float64,), y, x)
atan2(y::Float32, x::Float32) = ccall((:atan2f,libm), Float32, (Float32, Float32), y, x)

max{T<:AbstractFloat}(x::T, y::T) = ifelse((y > x) | (signbit(y) < signbit(x)),
                                    ifelse(isnan(x), x, y), ifelse(isnan(y), y, x))


min{T<:AbstractFloat}(x::T, y::T) = ifelse((y < x) | (signbit(y) > signbit(x)),
                                    ifelse(isnan(x), x, y), ifelse(isnan(y), y, x))

minmax{T<:AbstractFloat}(x::T, y::T) =
    ifelse(isnan(x) | isnan(y), ifelse(isnan(x), (x,x), (y,y)),
           ifelse((y > x) | (signbit(x) > signbit(y)), (x,y), (y,x)))


"""
    ldexp(x, n)

Compute ``x \\times 2^n``.

```jldoctest
julia> ldexp(5., 2)
20.0
```
"""
function ldexp{T<:IEEEFloat}(x::T, e::Integer)
    xu = reinterpret(Unsigned, x)
    xs = xu & ~sign_mask(T)
    xs >= exponent_mask(T) && return x # NaN or Inf
    k = Int(xs >> significand_bits(T))
    if k == 0 # x is subnormal
        xs == 0 && return x # +-0
        m = leading_zeros(xs) - exponent_bits(T)
        ys = xs << unsigned(m)
        xu = ys | (xu & sign_mask(T))
        k = 1 - m
        # underflow, otherwise may have integer underflow in the following n + k
        e < -50000 && return flipsign(T(0.0), x)
    end
    # For cases where e of an Integer larger than Int make sure we properly
    # overflow/underflow; this is optimized away otherwise.
    if e > typemax(Int)
        return flipsign(T(Inf), x)
    elseif e < typemin(Int)
        return flipsign(T(0.0), x)
    end
    n = e % Int
    k += n
    # overflow, if k is larger than maximum posible exponent
    if k >= exponent_raw_max(T)
        return flipsign(T(Inf), x)
    end
    if k > 0 # normal case
        xu = (xu & ~exponent_mask(T)) | (rem(k, fpinttype(T)) << significand_bits(T))
        return reinterpret(T, xu)
    else # subnormal case
        if k <= -significand_bits(T) # underflow
            # overflow, for the case of integer overflow in n + k
            e > 50000 && return flipsign(T(Inf), x)
            return flipsign(T(0.0), x)
        end
        k += significand_bits(T)
        z = T(2.0)^-significand_bits(T)
        xu = (xu & ~exponent_mask(T)) | (rem(k, fpinttype(T)) << significand_bits(T))
        return z*reinterpret(T, xu)
    end
end
ldexp(x::Float16, q::Integer) = Float16(ldexp(Float32(x), q))

"""
    exponent(x) -> Int

Get the exponent of a normalized floating-point number.
"""
function exponent{T<:IEEEFloat}(x::T)
    xs = reinterpret(Unsigned, x) & ~sign_mask(T)
    xs >= exponent_mask(T) && return throw(DomainError()) # NaN or Inf
    k = Int(xs >> significand_bits(T))
    if k == 0 # x is subnormal
        xs == 0 && throw(DomainError())
        m = leading_zeros(xs) - exponent_bits(T)
        k = 1 - m
    end
    return k - exponent_bias(T)
end

"""
    significand(x)

Extract the `significand(s)` (a.k.a. mantissa), in binary representation, of a
floating-point number. If `x` is a non-zero finite number, then the result will be
a number of the same type on the interval ``[1,2)``. Otherwise `x` is returned.

```jldoctest
julia> significand(15.2)/15.2
0.125

julia> significand(15.2)*8
15.2
```
"""
function significand{T<:IEEEFloat}(x::T)
    xu = reinterpret(Unsigned, x)
    xs = xu & ~sign_mask(T)
    xs >= exponent_mask(T) && return x # NaN or Inf
    if xs <= (~exponent_mask(T) & ~sign_mask(T)) # x is subnormal
        xs == 0 && return x # +-0
        m = unsigned(leading_zeros(xs) - exponent_bits(T))
        xs <<= m
        xu = xs | (xu & sign_mask(T))
    end
    xu = (xu & ~exponent_mask(T)) | exponent_one(T)
    return reinterpret(T, xu)
end

"""
    frexp(val)

Return `(x,exp)` such that `x` has a magnitude in the interval ``[1/2, 1)`` or 0,
and `val` is equal to ``x \\times 2^{exp}``.
"""
function frexp{T<:IEEEFloat}(x::T)
    xu = reinterpret(Unsigned, x)
    xs = xu & ~sign_mask(T)
    xs >= exponent_mask(T) && return x, 0 # NaN or Inf
    k = Int(xs >> significand_bits(T))
    if k == 0 # x is subnormal
        xs == 0 && return x, 0 # +-0
        m = leading_zeros(xs) - exponent_bits(T)
        xs <<= unsigned(m)
        xu = xs | (xu & sign_mask(T))
        k = 1 - m
    end
    k -= (exponent_bias(T) - 1)
    xu = (xu & ~exponent_mask(T)) | exponent_half(T)
    return reinterpret(T, xu), k
end

"""
    rem(x, y, r::RoundingMode)

Compute the remainder of `x` after integer division by `y`, with the quotient rounded
according to the rounding mode `r`. In other words, the quantity

    x - y*round(x/y,r)

without any intermediate rounding.

- if `r == RoundNearest`, then the result is exact, and in the interval
  ``[-|y|/2, |y|/2]``.

- if `r == RoundToZero` (default), then the result is exact, and in the interval
  ``[0, |y|)`` if `x` is positive, or ``(-|y|, 0]`` otherwise.

- if `r == RoundDown`, then the result is in the interval ``[0, y)`` if `y` is positive, or
  ``(y, 0]`` otherwise. The result may not be exact if `x` and `y` have different signs, and
  `abs(x) < abs(y)`.

- if `r == RoundUp`, then the result is in the interval `(-y,0]` if `y` is positive, or
  `[0,-y)` otherwise. The result may not be exact if `x` and `y` have the same sign, and
  `abs(x) < abs(y)`.

"""
rem(x, y, ::RoundingMode{:ToZero}) = rem(x,y)
rem(x, y, ::RoundingMode{:Down}) = mod(x,y)
rem(x, y, ::RoundingMode{:Up}) = mod(x,-y)

rem(x::Float64, y::Float64, ::RoundingMode{:Nearest}) =
    ccall((:remainder, libm),Float64,(Float64,Float64),x,y)
rem(x::Float32, y::Float32, ::RoundingMode{:Nearest}) =
    ccall((:remainderf, libm),Float32,(Float32,Float32),x,y)
rem(x::Float16, y::Float16, r::RoundingMode{:Nearest}) = Float16(rem(Float32(x), Float32(y), r))


"""
    modf(x)

Return a tuple (fpart,ipart) of the fractional and integral parts of a number. Both parts
have the same sign as the argument.

```jldoctest
julia> modf(3.5)
(0.5, 3.0)
```
"""
modf(x) = rem(x,one(x)), trunc(x)

const _modff_temp = Ref{Float32}()
function modf(x::Float32)
    f = ccall((:modff,libm), Float32, (Float32,Ptr{Float32}), x, _modff_temp)
    f, _modff_temp[]
end

const _modf_temp = Ref{Float64}()
function modf(x::Float64)
    f = ccall((:modf,libm), Float64, (Float64,Ptr{Float64}), x, _modf_temp)
    f, _modf_temp[]
end

^(x::Float64, y::Float64) = nan_dom_err(ccall((:pow,libm),  Float64, (Float64,Float64), x, y), x+y)
^(x::Float32, y::Float32) = nan_dom_err(ccall((:powf,libm), Float32, (Float32,Float32), x, y), x+y)

^(x::Float64, y::Integer) = x^Int32(y)
^(x::Float64, y::Int32) = powi_llvm(x, y)
^(x::Float32, y::Integer) = x^Int32(y)
^(x::Float32, y::Int32) = powi_llvm(x, y)
^(x::Float16, y::Integer) = Float16(Float32(x)^y)

function angle_restrict_symm(theta)
    const P1 = 4 * 7.8539812564849853515625e-01
    const P2 = 4 * 3.7748947079307981766760e-08
    const P3 = 4 * 2.6951514290790594840552e-15

    y = 2*floor(theta/(2*pi))
    r = ((theta - y*P1) - y*P2) - y*P3
    if (r > pi)
        r -= (2*pi)
    end
    return r
end

## rem2pi-related calculations ##

function add22condh(xh::Float64, xl::Float64, yh::Float64, yl::Float64)
    # as above, but only compute and return high double
    r = xh+yh
    s = (abs(xh) > abs(yh)) ? (xh-r+yh+yl+xl) : (yh-r+xh+xl+yl)
    zh = r+s
    return zh
end

function ieee754_rem_pio2(x::Float64)
    # rem_pio2 essentially computes x mod pi/2 (ie within a quarter circle)
    # and returns the result as
    # y between + and - pi/4 (for maximal accuracy (as the sign bit is exploited)), and
    # n, where n specifies the integer part of the division, or, at any rate,
    # in which quadrant we are.
    # The invariant fulfilled by the returned values seems to be
    #  x = y + n*pi/2 (where y = y1+y2 is a double-double and y2 is the "tail" of y).
    # Note: for very large x (thus n), the invariant might hold only modulo 2pi
    # (in other words, n might be off by a multiple of 4, or a multiple of 100)

    # this is just wrapping up
    # https://github.com/JuliaLang/openspecfun/blob/master/rem_pio2/e_rem_pio2.c

    y = [0.0,0.0]
    n = ccall((:__ieee754_rem_pio2, openspecfun), Cint, (Float64,Ptr{Float64}), x, y)
    return (n,y)
end

# multiples of pi/2, as double-double (ie with "tail")
const pi1o2_h  = 1.5707963267948966     # convert(Float64, pi * BigFloat(1/2))
const pi1o2_l  = 6.123233995736766e-17  # convert(Float64, pi * BigFloat(1/2) - pi1o2_h)

const pi2o2_h  = 3.141592653589793      # convert(Float64, pi * BigFloat(1))
const pi2o2_l  = 1.2246467991473532e-16 # convert(Float64, pi * BigFloat(1) - pi2o2_h)

const pi3o2_h  = 4.71238898038469       # convert(Float64, pi * BigFloat(3/2))
const pi3o2_l  = 1.8369701987210297e-16 # convert(Float64, pi * BigFloat(3/2) - pi3o2_h)

const pi4o2_h  = 6.283185307179586      # convert(Float64, pi * BigFloat(2))
const pi4o2_l  = 2.4492935982947064e-16 # convert(Float64, pi * BigFloat(2) - pi4o2_h)

"""
    rem2pi(x, r::RoundingMode)

Compute the remainder of `x` after integer division by `2π`, with the quotient rounded
according to the rounding mode `r`. In other words, the quantity

    x - 2π*round(x/(2π),r)

without any intermediate rounding. This internally uses a high precision approximation of
2π, and so will give a more accurate result than `rem(x,2π,r)`

- if `r == RoundNearest`, then the result is in the interval ``[-π, π]``. This will generally
  be the most accurate result.

- if `r == RoundToZero`, then the result is in the interval ``[0, 2π]`` if `x` is positive,.
  or ``[-2π, 0]`` otherwise.

- if `r == RoundDown`, then the result is in the interval ``[0, 2π]``.

- if `r == RoundUp`, then the result is in the interval ``[-2π, 0]``.

```jldoctest
julia> rem2pi(7pi/4, RoundNearest)
-0.7853981633974485

julia> rem2pi(7pi/4, RoundDown)
5.497787143782138
```
"""
function rem2pi end
function rem2pi(x::Float64, ::RoundingMode{:Nearest})
    abs(x) < pi && return x

    (n,y) = ieee754_rem_pio2(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add/subtract pi
            if y[1] <= 0
                return add22condh(y[1],y[2],pi2o2_h,pi2o2_l)
            else
                return add22condh(y[1],y[2],-pi2o2_h,-pi2o2_l)
            end
        else          # n % 4 == 0: add 0
            return y[1]
        end
    else
        if n & 2 == 2 # n % 4 == 3: subtract pi/2
            return add22condh(y[1],y[2],-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y[1],y[2],pi1o2_h,pi1o2_l)
        end
    end
end
function rem2pi(x::Float64, ::RoundingMode{:ToZero})
    ax = abs(x)
    ax <= 2*Float64(pi,RoundDown) && return x

    (n,y) = ieee754_rem_pio2(ax)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            z = add22condh(y[1],y[2],pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y[1] > 0
                z = y[1]
            else      # negative: add 2pi
                z = add22condh(y[1],y[2],pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            z = add22condh(y[1],y[2],pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            z = add22condh(y[1],y[2],pi1o2_h,pi1o2_l)
        end
    end
    copysign(z,x)
end
function rem2pi(x::Float64, ::RoundingMode{:Down})
    if x < pi4o2_h
        if x >= 0
            return x
        elseif x > -pi4o2_h
            return add22condh(x,0.0,pi4o2_h,pi4o2_l)
        end
    end

    (n,y) = ieee754_rem_pio2(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            return add22condh(y[1],y[2],pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y[1] > 0
                return y[1]
            else      # negative: add 2pi
                return add22condh(y[1],y[2],pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            return add22condh(y[1],y[2],pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y[1],y[2],pi1o2_h,pi1o2_l)
        end
    end
end
function rem2pi(x::Float64, ::RoundingMode{:Up})
    if x > -pi4o2_h
        if x <= 0
            return x
        elseif x < pi4o2_h
            return add22condh(x,0.0,-pi4o2_h,-pi4o2_l)
        end
    end

    (n,y) = ieee754_rem_pio2(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: sub pi
            return add22condh(y[1],y[2],-pi2o2_h,-pi2o2_l)
        else          # n % 4 == 0: sub 0 or 2pi
            if y[1] < 0
                return y[1]
            else      # positive: sub 2pi
                return add22condh(y[1],y[2],-pi4o2_h,-pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: sub pi/2
            return add22condh(y[1],y[2],-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: sub 3pi/2
            return add22condh(y[1],y[2],-pi3o2_h,-pi3o2_l)
        end
    end
end

rem2pi(x::Float32, r::RoundingMode) = Float32(rem2pi(Float64(x), r))
rem2pi(x::Float16, r::RoundingMode) = Float16(rem2pi(Float64(x), r))
rem2pi(x::Int32, r::RoundingMode) = rem2pi(Float64(x), r)
function rem2pi(x::Int64, r::RoundingMode)
    fx = Float64(x)
    fx == x || throw(ArgumentError("Int64 argument to rem2pi is too large: $x"))
    rem2pi(fx, r)
end

"""
    mod2pi(x)

Modulus after division by `2π`, returning in the range ``[0,2π)``.

This function computes a floating point representation of the modulus after division by
numerically exact `2π`, and is therefore not exactly the same as `mod(x,2π)`, which would
compute the modulus of `x` relative to division by the floating-point number `2π`.

```jldoctest
julia> mod2pi(9*pi/4)
0.7853981633974481
```
"""
mod2pi(x) = rem2pi(x,RoundDown)

# generic fallback; for number types, promotion.jl does promotion

"""
    muladd(x, y, z)

Combined multiply-add, computes `x*y+z` in an efficient manner. This may on some systems be
equivalent to `x*y+z`, or to `fma(x,y,z)`. `muladd` is used to improve performance.
See [`fma`](@ref).
"""
muladd(x,y,z) = x*y+z

# Float16 definitions

for func in (:sin,:cos,:tan,:asin,:acos,:atan,:sinh,:cosh,:tanh,:asinh,:acosh,
             :atanh,:exp,:log,:log2,:log10,:sqrt,:lgamma,:log1p,:erf,:erfc)
    @eval begin
        $func(a::Float16) = Float16($func(Float32(a)))
        $func(a::Complex32) = Complex32($func(Complex64(a)))
    end
end

for func in (:atan2,:hypot)
    @eval begin
        $func(a::Float16,b::Float16) = Float16($func(Float32(a),Float32(b)))
    end
end

cbrt(a::Float16) = Float16(cbrt(Float32(a)))

# More special functions
include("special/trig.jl")
include("special/bessel.jl")
include("special/erf.jl")
include("special/gamma.jl")

module JuliaLibm
include("special/log.jl")
end

end # module
