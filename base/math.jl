# This file is a part of Julia. License is MIT: https://julialang.org/license

module Math

export sin, cos, sincos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot,
       sech, csch, coth, asech, acsch, acoth,
       sinpi, cospi, sinc, cosc,
       cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       rad2deg, deg2rad,
       log, log2, log10, log1p, exponent, exp, exp2, exp10, expm1,
       cbrt, sqrt, significand,
       lgamma, hypot, gamma, lfact, max, min, minmax, ldexp, frexp,
       clamp, clamp!, modf, ^, mod2pi, rem2pi,
       beta, lbeta, @evalpoly

import Base: log, exp, sin, cos, tan, sinh, cosh, tanh, asin,
             acos, atan, asinh, acosh, atanh, sqrt, log2, log10,
             max, min, minmax, ^, exp2, muladd, rem,
             exp10, expm1, log1p

using Base: sign_mask, exponent_mask, exponent_one,
            exponent_half, fpinttype, significand_mask

using Core.Intrinsics: sqrt_llvm

const IEEEFloat = Union{Float16, Float32, Float64}

for T in (Float16, Float32, Float64)
    @eval significand_bits(::Type{$T}) = $(trailing_ones(significand_mask(T)))
    @eval exponent_bits(::Type{$T}) = $(sizeof(T)*8 - significand_bits(T) - 1)
    @eval exponent_bias(::Type{$T}) = $(Int(exponent_one(T) >> significand_bits(T)))
    # maximum float exponent
    @eval exponent_max(::Type{$T}) = $(Int(exponent_mask(T) >> significand_bits(T)) - exponent_bias(T))
    # maximum float exponent without bias
    @eval exponent_raw_max(::Type{$T}) = $(Int(exponent_mask(T) >> significand_bits(T)))
end

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
clamp(x::X, lo::L, hi::H) where {X,L,H} =
    ifelse(x > hi, convert(promote_type(X,L,H), hi),
           ifelse(x < lo,
                  convert(promote_type(X,L,H), lo),
                  convert(promote_type(X,L,H), x)))

"""
    clamp!(array::AbstractArray, lo, hi)

Restrict values in `array` to the specified range, in-place.
See also [`clamp`](@ref).
"""
function clamp!(x::AbstractArray, lo, hi)
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
    R = Expr(:macrocall, Symbol("@horner"), (), :tt, map(esc, p)...)
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

log(b::T, x::T) where {T<:Number} = log(x)/log(b)

"""
    log(b,x)

Compute the base `b` logarithm of `x`. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments.

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
    expm1(x)

Accurately compute ``e^x-1``.
"""
expm1(x)
for f in (:cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :exp2, :expm1)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end
exp(x::Real) = exp(float(x))
exp10(x::Real) = exp10(float(x))

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
for f in (:sinh, :cosh, :tanh, :atan, :asinh, :exp, :expm1)
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

# utility for converting NaN return to DomainError
# the branch in nan_dom_err prevents its callers from inlining, so be sure to force it
# until the heuristics can be improved
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

Compute the natural logarithm of `x`. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments. Use complex negative arguments to obtain complex results.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log(x)

"""
    log2(x)

Compute the logarithm of `x` to base 2. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments.

# Example
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
Throws [`DomainError`](@ref) for negative [`Real`](@ref) arguments.

# Example
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

Accurate natural logarithm of `1+x`. Throws [`DomainError`](@ref) for [`Real`](@ref)
arguments less than -1.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.

# Examples
```jldoctest
julia> log1p(-0.5)
-0.6931471805599453

julia> log1p(0)
0.0
```
"""
log1p(x)
for f in (:sin, :cos, :tan, :asin, :acos, :acosh, :atanh, :log, :log2, :log10,
          :lgamma, :log1p)
    @eval begin
        @inline ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)), libm), Float64, (Float64,), x), x)
        @inline ($f)(x::Float32) = nan_dom_err(ccall(($(string(f, "f")), libm), Float32, (Float32,), x), x)
        @inline ($f)(x::Real) = ($f)(float(x))
    end
end

"""
    sincos(x)

Compute sine and cosine of `x`, where `x` is in radians.
"""
@inline function sincos(x)
    res = Base.FastMath.sincos_fast(x)
    if (isnan(res[1]) | isnan(res[2])) & !isnan(x)
        throw(DomainError())
    end
    return res
end

sqrt(x::Float64) = sqrt_llvm(x)
sqrt(x::Float32) = sqrt_llvm(x)

"""
    sqrt(x)

Return ``\\sqrt{x}``. Throws [`DomainError`](@ref) for negative [`Real`](@ref) arguments.
Use complex negative arguments instead. The prefix operator `√` is equivalent to `sqrt`.
"""
sqrt(x::Real) = sqrt(float(x))

"""
    hypot(x, y)

Compute the hypotenuse ``\\sqrt{x^2+y^2}`` avoiding overflow and underflow.

# Examples
```jldoctest
julia> a = 10^10;

julia> hypot(a, a)
1.4142135623730951e10

julia> √(a^2 + a^2) # a^2 overflows
ERROR: DomainError:
sqrt will only return a complex result if called with a complex argument. Try sqrt(complex(x)).
Stacktrace:
 [1] sqrt(::Int64) at ./math.jl:447
```
"""
hypot(x::Number, y::Number) = hypot(promote(x, y)...)
function hypot(x::T, y::T) where T<:Number
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
atan2(y::T, x::T) where {T<:AbstractFloat} = Base.no_op_err("atan2", T)

atan2(y::Float64, x::Float64) = ccall((:atan2,libm), Float64, (Float64, Float64,), y, x)
atan2(y::Float32, x::Float32) = ccall((:atan2f,libm), Float32, (Float32, Float32), y, x)

max(x::T, y::T) where {T<:AbstractFloat} = ifelse((y > x) | (signbit(y) < signbit(x)),
                                    ifelse(isnan(x), x, y), ifelse(isnan(y), y, x))


min(x::T, y::T) where {T<:AbstractFloat} = ifelse((y < x) | (signbit(y) > signbit(x)),
                                    ifelse(isnan(x), x, y), ifelse(isnan(y), y, x))

minmax(x::T, y::T) where {T<:AbstractFloat} =
    ifelse(isnan(x) | isnan(y), ifelse(isnan(x), (x,x), (y,y)),
           ifelse((y > x) | (signbit(x) > signbit(y)), (x,y), (y,x)))


"""
    ldexp(x, n)

Compute ``x \\times 2^n``.

# Example
```jldoctest
julia> ldexp(5., 2)
20.0
```
"""
function ldexp(x::T, e::Integer) where T<:IEEEFloat
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
function exponent(x::T) where T<:IEEEFloat
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

# Examples
```jldoctest
julia> significand(15.2)/15.2
0.125

julia> significand(15.2)*8
15.2
```
"""
function significand(x::T) where T<:IEEEFloat
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
function frexp(x::T) where T<:IEEEFloat
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

# Example
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

@inline ^(x::Float64, y::Float64) = nan_dom_err(ccall("llvm.pow.f64", llvmcall, Float64, (Float64, Float64), x, y), x + y)
@inline ^(x::Float32, y::Float32) = nan_dom_err(ccall("llvm.pow.f32", llvmcall, Float32, (Float32, Float32), x, y), x + y)
@inline ^(x::Float64, y::Integer) = x ^ Float64(y)
@inline ^(x::Float32, y::Integer) = x ^ Float32(y)
@inline ^(x::Float16, y::Integer) = Float16(Float32(x) ^ Float32(y))
@inline literal_pow(::typeof(^), x::Float16, ::Type{Val{p}}) where {p} = Float16(literal_pow(^,Float32(x),Val{p}))

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
    # This algorithm, due to Dekker, computes the sum of
    # two double-double numbers and return high double.  References:
    # [1] http://www.digizeitschriften.de/en/dms/img/?PID=GDZPPN001170007
    # [2] https://dx.doi.org/10.1007/BF01397083
    r = xh+yh
    s = (abs(xh) > abs(yh)) ? (xh-r+yh+yl+xl) : (yh-r+xh+xl+yl)
    zh = r+s
    return zh
end

const invpio2 =  6.36619772367581382433e-01
const pio2_1  =  1.57079632673412561417e+00
const pio2_1t =  6.07710050650619224932e-11
const pio2_2  =  6.07710050630396597660e-11
const pio2_2t =  2.02226624879595063154e-21
const pio2_3  =  2.02226624871116645580e-21
const pio2_3t =  8.47842766036889956997e-32

function cody_waite_2c_pio2(x, signed_k, n)
    z = x - signed_k*pio2_1;
    y1 = z - signed_k*pio2_1t;
    y2 = (z - y1) - signed_k*pio2_1t;
    n, y1, y2
end

function cody_waite_ext_pio2(x, xʰ⁺)
    fn = x*invpio2+0x1.8p52
    fn = fn-0x1.8p52
    n  = Int32(fn)
    r  = x-fn*pio2_1
    w  = fn*pio2_1t # 1st round good to 85 bit
    j  = xʰ⁺>>20
    y1 = r-w
    high = highword(y1)
    i = j-((high>>20)&0x7ff)
    if i>16  # 2nd iteration needed, good to 118
        t  = r
        w  = fn*pio2_2
        r  = t-w
        w  = fn*pio2_2t-((t-r)-w)
        y1 = r-w
        high = highword(y1)
        i = j-((high>>20)&0x7ff)
        if i>49 # 3rd iteration need, 151 bits acc
            t  = r # will cover all possible cases
            w  = fn*pio2_3
            r  = t-w
            w  = fn*pio2_3t-((t-r)-w)
            y1 = r-w
        end
    end
    y2 = (r-y1)-w
    return Int(n), y1, y2
end

# constants and functions for large (>2.0^20) inputs to rem_pio2_kernel
const INV2PI = UInt64[
    0x28be_60db_9391_054a,
    0x7f09_d5f4_7d4d_3770,
    0x36d8_a566_4f10_e410,
    0x7f94_58ea_f7ae_f158,
    0x6dc9_1b8e_9093_74b8,
    0x0192_4bba_8274_6487,
    0x3f87_7ac7_2c4a_69cf,
    0xba20_8d7d_4bae_d121,
    0x3a67_1c09_ad17_df90,
    0x4e64_758e_60d4_ce7d,
    0x2721_17e2_ef7e_4a0e,
    0xc7fe_25ff_f781_6603,
    0xfbcb_c462_d682_9b47,
    0xdb4d_9fb3_c9f2_c26d,
    0xd3d1_8fd9_a797_fa8b,
    0x5d49_eeb1_faf9_7c5e,
    0xcf41_ce7d_e294_a4ba,
    0x9afe_d7ec_47e3_5742,
    0x1580_cc11_bf1e_daea]


"""
    fromfraction(f::Int128)

Computes a tuple of values `(y1,y2)` such that
    y1 + y2 == f / 2^128
and the significand of `y1` has 27 trailing zeros.
"""
function fromfraction(f::Int128)
    if f == 0
        return (0.0,0.0)
    end

    # 1. get leading term truncated to 26 bits
    s = ((f < 0) % UInt64) << 63     # sign bit
    x = abs(f) % UInt128             # magnitude
    n1 = 128-leading_zeros(x)         # ndigits0z(x,2)
    m1 = ((x >> (n1-26)) % UInt64) << 27
    d1 = ((n1-128+1021) % UInt64) << 52
    z1 = reinterpret(Float64, s | (d1 + m1))

    # 2. compute remaining term
    x2 = (x - (UInt128(m1) << (n1-53)))
    if x2 == 0
        return (z1, 0.0)
    end
    n2 = 128-leading_zeros(x2)
    m2 = (x2 >> (n2-53)) % UInt64
    d2 = ((n2-128+1021) % UInt64) << 52
    z2 = reinterpret(Float64,  s | (d2 + m2))
    return (z1,z2)
end

function paynehanek(x::Float64)
    # 1. Convert to form
    #
    #    x = X * 2^k,
    #
    # where 2^(n-1) <= X < 2^n  is an n-bit integer (n = 53, k = exponent(x)-52 )

    u = reinterpret(UInt64, x)
    X = (u & significand_mask(Float64)) | (one(UInt64) << significand_bits(Float64))
    k = Int((u & exponent_mask(Float64)) >> significand_bits(Float64)) - exponent_bias(Float64) - significand_bits(Float64)

    # 2. Let α = 1/2π, then:
    #
    #    α*x mod 1 ≡ [(α*2^k mod 1)*X] mod 1
    #
    # so we can ignore the first k bits of α. Extract the next 3 64-bit parts of α.
    #
    # i.e. equivalent to
    #     setprecision(BigFloat,4096)
    #     α  = 1/(2*big(pi))
    #     A  = mod(ldexp(α,k), 1)
    #     z1 = ldexp(A,64)
    #     a1 = trunc(UInt64, z1)
    #     z2 = ldexp(z1-a1, 64)
    #     a2 = trunc(UInt64, z2)
    #     z3 = ldexp(z2-a2, 64)
    #     a3 = trunc(UInt64, z3)

    # idx, shift = divrem(k, 64), but divrem is slower
    idx = k >> 6
    shift = k - (idx << 6)
    if shift == 0
        a1 = INV2PI[idx+1]
        a2 = INV2PI[idx+2]
        a3 = INV2PI[idx+3]
    else
        # use shifts to extract the relevant 64 bit window
        a1 = (idx < 0 ? zero(UInt64) : INV2PI[idx+1] << shift) | (INV2PI[idx+2] >> (64 - shift))
        a2 = (INV2PI[idx+2] << shift) | (INV2PI[idx+3] >> (64 - shift))
        a3 = (INV2PI[idx+3] << shift) | (INV2PI[idx+4] >> (64 - shift))
    end

    # 3. Perform the multiplication:
    #
    #      X.  0  0  0
    #   ×  0. a1 a2 a3
    #   ==============
    #      _.  w  w  _
    #
    # (i.e. ignoring integer and lowest bit parts of result)

    w1 = UInt128(X*a1) << 64 # overflow becomes integer
    w2 = widemul(X,a2)
    w3 = widemul(X,a3) >> 64
    w = w1 + w2 + w3         # quotient fraction after division by 2π

    # adjust for sign of x
    w = flipsign(w,x)

    # 4. convert to quadrant, quotient fraction after division by π/2:
    q = (((w>>125)%Int +1)>>1) # nearest quadrant
    f = (w<<2) % Int128 # fraction part of quotient after division by π/2, taking values on [-0.5,0.5)

    # 5. convert quotient fraction to split precision Float64
    z_hi,z_lo = fromfraction(f)

    # 6. multiply by π/2
    pio2_hi = 1.5707963407039642
    pio2_lo = -1.3909067614167116e-8

    y_hi = (z_hi+z_lo)*(pio2_hi+pio2_lo)
    y_lo = (((z_hi*pio2_hi - y_hi) + z_hi*pio2_lo) + z_lo*pio2_hi) + z_lo*pio2_lo
    return q, y_hi, y_lo
end


"""
    highword(x)

returns the high word of x as a UInt32.
"""
@inline highword(x::UInt64) = unsafe_trunc(UInt32,x >> 32)
@inline highword(x::Float64) = unsafe_trunc(UInt32,highword(reinterpret(UInt64, x)))

"""
    rem_pio2(x::Float64)

returns the remainder of x modulo π/2 as a TwicePrecision number, along with a k
such that k mod 3 == K mod 3 where K*π/2 = x -rem.
"""

function rem_pio2(x::Float64)
    xh = highword(x)
    xhp = xh & 0x7fffffff # positive part of highword

    if xhp <= 0x3fe921fb #|x| ~<= pi/4, no need for reduction, just return input
        return Int(0), x, 0.0
    end
    rem_pio2_kernel(x, xh, xhp)
end

"""
    rem_pio2_kernel(x, xh, xhp)

returns the remainder of x modulo π/2 as a TwicePrecision number, along with a k
such that k mod 3 == K mod 3 where K*π/2 = x -rem.
"""
function rem_pio2_kernel(x::Float64)
    # rem_pio2_kernel essentially computes x mod pi/2 (ie within a quarter circle)
    # and returns the result as
    # y between + and - pi/4 (for maximal accuracy (as the sign bit is exploited)), and
    # n, where n specifies the integer part of the division, or, at any rate,
    # in which quadrant we are.
    # The invariant fulfilled by the returned values seems to be
    #  x = y + n*pi/2 (where y = y1+y2 is a double-double and y2 is the "tail" of y).
    # Note: for very large x (thus n), the invariant might hold only modulo 2pi
    # (in other words, n might be off by a multiple of 4, or a multiple of 100)

    xh = highword(x)
    xhp = xh & 0x7fffffff # positive part of highword
    rem_pio2_kernel(x, xh, xhp)
end

function rem_pio2_kernel(x::Float64, xh, xhp)
    if xhp <= 0x400f6a7a #|x| ~<= 5pi/4, use Cody Waite with two constants
        if (xhp & 0xfffff) == 0x921fb # |x| ~= pi/2 or 2pi/2, use precise Cody Waite scheme
            return cody_waite_ext_pio2(x, xhp)
        end
        if xhp <= 0x4002d97c # |x| ~<= 3pi/4
            if x > 0
                return cody_waite_2c_pio2(x, 1.0, 1)
            else
                return cody_waite_2c_pio2(x, -1.0, -1)
            end
        else
            if x > 0
                return cody_waite_2c_pio2(x, 2.0, 2)
            else
                return cody_waite_2c_pio2(x, -2.0, -2)
            end
        end
    end

    if xhp <= 0x401c463b # |x| ~<= 9pi/4, use Cody Waite with two constants
        if (xhp <= 0x4015fdbc) # |x| ~<= 7pi/4
            if (xhp == 0x4012d97c) # |x| ~= 3pi/2, use precise Cody Waite scheme
                return cody_waite_ext_pio2(x, xhp)
            end
            if x > 0
                return cody_waite_2c_pio2(x, 3.0, 3)
            else
                return cody_waite_2c_pio2(x, -3.0, -3)
            end
        else
            if xhp == 0x401921fb # |x| ~= 4pi/2, use precise Cody Waite scheme
                return cody_waite_ext_pio2(x, xhp)
            end
            if x > 0
                return cody_waite_2c_pio2(x, 4.0, 4)
            else
                return cody_waite_2c_pio2(x, -4.0, -4)
            end
        end
    end
    if xhp<0x413921fb # |x| ~< 2^20*pi/2, use precise Cody Waite scheme
        return cody_waite_ext_pio2(x, xhp)
    end

    # if |x| >= 2^20*pi/2 switch to Payne Hanek
    return paynehanek(x)
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

# Example
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

    n,y1,y2 = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add/subtract pi
            if y1 <= 0
                return add22condh(y1,y2,pi2o2_h,pi2o2_l)
            else
                return add22condh(y1,y2,-pi2o2_h,-pi2o2_l)
            end
        else          # n % 4 == 0: add 0
            return y1
        end
    else
        if n & 2 == 2 # n % 4 == 3: subtract pi/2
            return add22condh(y1,y2,-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y1,y2,pi1o2_h,pi1o2_l)
        end
    end
end
function rem2pi(x::Float64, ::RoundingMode{:ToZero})
    ax = abs(x)
    ax <= 2*Float64(pi,RoundDown) && return x

    n,y1,y2 = rem_pio2_kernel(ax)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            z = add22condh(y1,y2,pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y1 > 0
                z = y1
            else      # negative: add 2pi
                z = add22condh(y1,y2,pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            z = add22condh(y1,y2,pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            z = add22condh(y1,y2,pi1o2_h,pi1o2_l)
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

    n,y1,y2 = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            return add22condh(y1,y2,pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y1 > 0
                return y1
            else      # negative: add 2pi
                return add22condh(y1,y2,pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            return add22condh(y1,y2,pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y1,y2,pi1o2_h,pi1o2_l)
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

    n,y1,y2 = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: sub pi
            return add22condh(y1,y2,-pi2o2_h,-pi2o2_l)
        else          # n % 4 == 0: sub 0 or 2pi
            if y1 < 0
                return y1
            else      # positive: sub 2pi
                return add22condh(y1,y2,-pi4o2_h,-pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: sub pi/2
            return add22condh(y1,y2,-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: sub 3pi/2
            return add22condh(y1,y2,-pi3o2_h,-pi3o2_l)
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

# Example
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

# Example
```jldoctest
julia> muladd(3, 2, 1)
7

julia> 3 * 2 + 1
7
```
"""
muladd(x,y,z) = x*y+z

# Float16 definitions

for func in (:sin,:cos,:tan,:asin,:acos,:atan,:sinh,:cosh,:tanh,:asinh,:acosh,
             :atanh,:exp,:log,:log2,:log10,:sqrt,:lgamma,:log1p)
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
include(joinpath("special", "exp.jl"))
include(joinpath("special", "exp10.jl"))
include(joinpath("special", "trig.jl"))
include(joinpath("special", "gamma.jl"))

module JuliaLibm
include(joinpath("special", "log.jl"))
end

end # module
