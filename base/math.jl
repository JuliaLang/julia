# This file is a part of Julia. License is MIT: https://julialang.org/license

module Math

export sin, cos, sincos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot,
       sech, csch, coth, asech, acsch, acoth,
       sinpi, cospi, sinc, cosc,
       cosd, cotd, cscd, secd, sind, tand, sincosd,
       acosd, acotd, acscd, asecd, asind, atand,
       rad2deg, deg2rad,
       log, log2, log10, log1p, exponent, exp, exp2, exp10, expm1,
       cbrt, sqrt, significand,
       hypot, max, min, minmax, ldexp, frexp,
       clamp, clamp!, modf, ^, mod2pi, rem2pi,
       @evalpoly, evalpoly

import .Base: log, exp, sin, cos, tan, sinh, cosh, tanh, asin,
             acos, atan, asinh, acosh, atanh, sqrt, log2, log10,
             max, min, minmax, ^, exp2, muladd, rem,
             exp10, expm1, log1p

using .Base: sign_mask, exponent_mask, exponent_one,
            exponent_half, uinttype, significand_mask,
            significand_bits, exponent_bits, exponent_bias,
            exponent_max, exponent_raw_max

using Core.Intrinsics: sqrt_llvm

using .Base: IEEEFloat

@noinline function throw_complex_domainerror(f::Symbol, x)
    throw(DomainError(x, string("$f will only return a complex result if called with a ",
                                "complex argument. Try $f(Complex(x)).")))
end
@noinline function throw_exp_domainerror(x)
    throw(DomainError(x, string("Exponentiation yielding a complex result requires a ",
                                "complex argument.\nReplace x^y with (x+0im)^y, ",
                                "Complex(x)^y, or similar.")))
end

# non-type specific math functions

"""
    clamp(x, lo, hi)

Return `x` if `lo <= x <= hi`. If `x > hi`, return `hi`. If `x < lo`, return `lo`. Arguments
are promoted to a common type.

# Examples
```jldoctest
julia> clamp.([pi, 1.0, big(10.)], 2., 9.)
3-element Array{BigFloat,1}:
 3.141592653589793238462643383279502884197169399375105820974944592307816406286198
 2.0
 9.0

julia> clamp.([11,8,5],10,6) # an example where lo > hi
3-element Array{Int64,1}:
  6
  6
 10
```
"""
clamp(x::X, lo::L, hi::H) where {X,L,H} =
    ifelse(x > hi, convert(promote_type(X,L,H), hi),
           ifelse(x < lo,
                  convert(promote_type(X,L,H), lo),
                  convert(promote_type(X,L,H), x)))

"""
    clamp(x, T)::T

Clamp `x` between `typemin(T)` and `typemax(T)` and convert the result to type `T`.

# Examples
```jldoctest
julia> clamp(200, Int8)
127
julia> clamp(-200, Int8)
-128
```
"""
clamp(x, ::Type{T}) where {T<:Integer} = clamp(x, typemin(T), typemax(T)) % T


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


"""
    evalpoly(x, p)

Evaluate the polynomial ``\\sum_k p[k] x^{k-1}`` for the coefficients `p[1]`, `p[2]`, ...;
that is, the coefficients are given in ascending order by power of `x`.
Loops are unrolled at compile time if the number of coefficients is statically known, i.e.
when `p` is a `Tuple`.
This function generates efficient code using Horner's method if `x` is real, or using
a Goertzel-like [^DK62] algorithm if `x` is complex.

[^DK62]: Donald Knuth, Art of Computer Programming, Volume 2: Seminumerical Algorithms, Sec. 4.6.4.

!!! compat "Julia 1.4"
    This function requires Julia 1.4 or later.

# Example
```jldoctest
julia> evalpoly(2, (1, 2, 3))
17
```
"""
function evalpoly(x, p::Tuple)
    if @generated
        N = length(p.parameters)
        ex = :(p[end])
        for i in N-1:-1:1
            ex = :(muladd(x, $ex, p[$i]))
        end
        ex
    else
        _evalpoly(x, p)
    end
end

evalpoly(x, p::AbstractVector) = _evalpoly(x, p)

function _evalpoly(x, p)
    N = length(p)
    ex = p[end]
    for i in N-1:-1:1
        ex = muladd(x, ex, p[i])
    end
    ex
end

function evalpoly(z::Complex, p::Tuple)
    if @generated
        N = length(p.parameters)
        a = :(p[end])
        b = :(p[end-1])
        as = []
        for i in N-2:-1:1
            ai = Symbol("a", i)
            push!(as, :($ai = $a))
            a = :(muladd(r, $ai, $b))
            b = :(muladd(-s, $ai, p[$i]))
        end
        ai = :a0
        push!(as, :($ai = $a))
        C = Expr(:block,
                 :(x = real(z)),
                 :(y = imag(z)),
                 :(r = x + x),
                 :(s = muladd(x, x, y*y)),
                 as...,
                 :(muladd($ai, z, $b)))
    else
        _evalpoly(z, p)
    end
end
evalpoly(z::Complex, p::Tuple{<:Any}) = p[1]


evalpoly(z::Complex, p::AbstractVector) = _evalpoly(z, p)

function _evalpoly(z::Complex, p)
    length(p) == 1 && return p[1]
    N = length(p)
    a = p[end]
    b = p[end-1]

    x = real(z)
    y = imag(z)
    r = 2x
    s = muladd(x, x, y*y)
    for i in N-2:-1:1
        ai = a
        a = muladd(r, ai, b)
        b = muladd(-s, ai, p[i])
    end
    ai = a
    muladd(ai, z, b)
end

"""
    @horner(x, p...)

Evaluate `p[1] + x * (p[2] + x * (....))`, i.e. a polynomial via Horner's rule.
"""
macro horner(x, p...)
     xesc, pesc = esc(x), esc.(p)
    :(invoke(evalpoly, Tuple{Any, Tuple}, $xesc, ($(pesc...),)))
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

# Examples
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
    zesc, pesc = esc(z), esc.(p)
    :(evalpoly($zesc, ($(pesc...),)))
end

"""
    rad2deg(x)

Convert `x` from radians to degrees.

# Examples
```jldoctest
julia> rad2deg(pi)
180.0
```
"""
rad2deg(z::AbstractFloat) = z * (180 / oftype(z, pi))

"""
    deg2rad(x)

Convert `x` from degrees to radians.

# Examples
```jldoctest
julia> deg2rad(90)
1.5707963267948966
```
"""
deg2rad(z::AbstractFloat) = z * (oftype(z, pi) / 180)
rad2deg(z::Real) = rad2deg(float(z))
deg2rad(z::Real) = deg2rad(float(z))
rad2deg(z::Number) = (z/pi)*180
deg2rad(z::Number) = (z*pi)/180

log(b::T, x::T) where {T<:Number} = log(x)/log(b)

"""
    log(b,x)

Compute the base `b` logarithm of `x`. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> log(4,8)
1.5

julia> log(4,2)
0.5

julia> log(-2, 3)
ERROR: DomainError with -2.0:
log will only return a complex result if called with a complex argument. Try log(Complex(x)).
Stacktrace:
 [1] throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:31
[...]

julia> log(2, -3)
ERROR: DomainError with -3.0:
log will only return a complex result if called with a complex argument. Try log(Complex(x)).
Stacktrace:
 [1] throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:31
[...]
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

# functions with no domain error
"""
    sinh(x)

Compute hyperbolic sine of `x`.
"""
sinh(x::Number)

"""
    cosh(x)

Compute hyperbolic cosine of `x`.
"""
cosh(x::Number)

"""
    tanh(x)

Compute hyperbolic tangent of `x`.
"""
tanh(x::Number)

"""
    atan(y)
    atan(y, x)

Compute the inverse tangent of `y` or `y/x`, respectively.

For one argument, this is the angle in radians between the positive *x*-axis and the point
(1, *y*), returning a value in the interval ``[-\\pi/2, \\pi/2]``.

For two arguments, this is the angle in radians between the positive *x*-axis and the
point (*x*, *y*), returning a value in the interval ``[-\\pi, \\pi]``. This corresponds to a
standard [`atan2`](https://en.wikipedia.org/wiki/Atan2) function.
"""
atan(x::Number)

"""
    asinh(x)

Compute the inverse hyperbolic sine of `x`.
"""
asinh(x::Number)

"""
    expm1(x)

Accurately compute ``e^x-1``.
"""
expm1(x)
for f in (:exp2, :expm1)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end

"""
    exp2(x)

Compute the base 2 exponential of `x`, in other words ``2^x``.

# Examples
```jldoctest
julia> exp2(5)
32.0
```
"""
exp2(x::AbstractFloat) = 2^x

"""
    exp10(x)

Compute the base 10 exponential of `x`, in other words ``10^x``.

# Examples
```jldoctest
julia> exp10(2)
100.0
```
"""
exp10(x::AbstractFloat) = 10^x

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
        reinterpret(Float64, 0x0000_0000_0000_0001 << ((x + 1074) % UInt))
    else
        # We will cast everything to Int64 to avoid errors in case of Int128
        # If x is a Int128, and is outside the range of Int64, then it is not -1023<x<=1023
        reinterpret(Float64, (exponent_bias(Float64) + (x % Int64)) << (significand_bits(Float64) % UInt))
    end
end

# utility for converting NaN return to DomainError
# the branch in nan_dom_err prevents its callers from inlining, so be sure to force it
# until the heuristics can be improved
@inline nan_dom_err(out, x) = isnan(out) & !isnan(x) ? throw(DomainError(x, "NaN result for non-NaN input.")) : out

# functions that return NaN on non-NaN argument for domain error
"""
    sin(x)

Compute sine of `x`, where `x` is in radians.
"""
sin(x::Number)

"""
    cos(x)

Compute cosine of `x`, where `x` is in radians.
"""
cos(x::Number)

"""
    tan(x)

Compute tangent of `x`, where `x` is in radians.
"""
tan(x::Number)

"""
    asin(x)

Compute the inverse sine of `x`, where the output is in radians.
"""
asin(x::Number)

"""
    acos(x)

Compute the inverse cosine of `x`, where the output is in radians
"""
acos(x::Number)

"""
    acosh(x)

Compute the inverse hyperbolic cosine of `x`.
"""
acosh(x::Number)

"""
    atanh(x)

Compute the inverse hyperbolic tangent of `x`.
"""
atanh(x::Number)

"""
    log(x)

Compute the natural logarithm of `x`. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments. Use complex negative arguments to obtain complex results.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> log(2)
0.6931471805599453

julia> log(-3)
ERROR: DomainError with -3.0:
log will only return a complex result if called with a complex argument. Try log(Complex(x)).
Stacktrace:
 [1] throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:31
[...]
```
"""
log(x::Number)

"""
    log2(x)

Compute the logarithm of `x` to base 2. Throws [`DomainError`](@ref) for negative
[`Real`](@ref) arguments.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> log2(4)
2.0

julia> log2(10)
3.321928094887362

julia> log2(-2)
ERROR: DomainError with -2.0:
NaN result for non-NaN input.
Stacktrace:
 [1] nan_dom_err at ./math.jl:325 [inlined]
[...]
```
"""
log2(x)

"""
    log10(x)

Compute the logarithm of `x` to base 10.
Throws [`DomainError`](@ref) for negative [`Real`](@ref) arguments.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> log10(100)
2.0

julia> log10(2)
0.3010299956639812

julia> log10(-2)
ERROR: DomainError with -2.0:
NaN result for non-NaN input.
Stacktrace:
 [1] nan_dom_err at ./math.jl:325 [inlined]
[...]
```
"""
log10(x)

"""
    log1p(x)

Accurate natural logarithm of `1+x`. Throws [`DomainError`](@ref) for [`Real`](@ref)
arguments less than -1.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> log1p(-0.5)
-0.6931471805599453

julia> log1p(0)
0.0

julia> log1p(-2)
ERROR: DomainError with -2.0:
log1p will only return a complex result if called with a complex argument. Try log1p(Complex(x)).
Stacktrace:
 [1] throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:31
[...]
```
"""
log1p(x)
for f in (:log2, :log10)
    @eval begin
        @inline ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)), libm), Float64, (Float64,), x), x)
        @inline ($f)(x::Float32) = nan_dom_err(ccall(($(string(f, "f")), libm), Float32, (Float32,), x), x)
        @inline ($f)(x::Real) = ($f)(float(x))
    end
end

@inline function sqrt(x::Union{Float32,Float64})
    x < zero(x) && throw_complex_domainerror(:sqrt, x)
    sqrt_llvm(x)
end

"""
    sqrt(x)

Return ``\\sqrt{x}``. Throws [`DomainError`](@ref) for negative [`Real`](@ref) arguments.
Use complex negative arguments instead. The prefix operator `√` is equivalent to `sqrt`.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> sqrt(big(81))
9.0

julia> sqrt(big(-81))
ERROR: DomainError with -81.0:
NaN result for non-NaN input.
Stacktrace:
 [1] sqrt(::BigFloat) at ./mpfr.jl:501
[...]

julia> sqrt(big(complex(-81)))
0.0 + 9.0im
```
"""
sqrt(x::Real) = sqrt(float(x))

"""
    hypot(x, y)

Compute the hypotenuse ``\\sqrt{|x|^2+|y|^2}`` avoiding overflow and underflow.

This code is an implementation of the algorithm described in:
An Improved Algorithm for `hypot(a,b)`
by Carlos F. Borges
The article is available online at ArXiv at the link
  https://arxiv.org/abs/1904.09481

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> a = Int64(10)^10;

julia> hypot(a, a)
1.4142135623730951e10

julia> √(a^2 + a^2) # a^2 overflows
ERROR: DomainError with -2.914184810805068e18:
sqrt will only return a complex result if called with a complex argument. Try sqrt(Complex(x)).
Stacktrace:
[...]

julia> hypot(3, 4im)
5.0
```
"""
hypot(x::Number, y::Number) = hypot(promote(x, y)...)
hypot(x::Complex, y::Complex) = hypot(abs(x), abs(y))
hypot(x::T, y::T) where {T<:Real} = hypot(float(x), float(y))
function hypot(x::T, y::T) where {T<:Number}
    if !iszero(x)
        z = y/x
        z2 = z*z

        abs(x) * sqrt(oneunit(z2) + z2)
    else
        abs(y)
    end
end

function hypot(x::T, y::T) where T<:AbstractFloat
    # Return Inf if either or both inputs is Inf (Compliance with IEEE754)
    if isinf(x) || isinf(y)
        return T(Inf)
    end

    # Order the operands
    ax,ay = abs(x), abs(y)
    if ay > ax
        ax,ay = ay,ax
    end

    # Widely varying operands
    if ay <= ax*sqrt(eps(T)/2)  #Note: This also gets ay == 0
        return ax
    end

    # Operands do not vary widely
    scale = eps(sqrt(floatmin(T)))  #Rescaling constant
    if ax > sqrt(floatmax(T)/2)
        ax = ax*scale
        ay = ay*scale
        scale = inv(scale)
    elseif ay < sqrt(floatmin(T))
        ax = ax/scale
        ay = ay/scale
    else
        scale = one(scale)
    end
    h = sqrt(muladd(ax,ax,ay*ay))
    # This branch is correctly rounded but requires a native hardware fma.
    if Base.Math.FMA_NATIVE
        hsquared = h*h
        axsquared = ax*ax
        h -= (fma(-ay,ay,hsquared-axsquared) + fma(h,h,-hsquared) - fma(ax,ax,-axsquared))/(2*h)
    # This branch is within one ulp of correctly rounded.
    else
        if h <= 2*ay
            delta = h-ay
            h -= muladd(delta,delta-2*(ax-ay),ax*(2*delta - ax))/(2*h)
        else
            delta = h-ax
            h -= muladd(delta,delta,muladd(ay,(4*delta-ay),2*delta*(ax-2*ay)))/(2*h)
        end
    end
    return h*scale
end

"""
    hypot(x...)

Compute the hypotenuse ``\\sqrt{\\sum |x_i|^2}`` avoiding overflow and underflow.

# Examples
```jldoctest
julia> hypot(-5.7)
5.7

julia> hypot(3, 4im, 12.0)
13.0
```
"""
hypot(x::Number...) = sqrt(sum(abs2(y) for y in x))

atan(y::Real, x::Real) = atan(promote(float(y),float(x))...)
atan(y::T, x::T) where {T<:AbstractFloat} = Base.no_op_err("atan", T)

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

# Examples
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
    # overflow, if k is larger than maximum possible exponent
    if k >= exponent_raw_max(T)
        return flipsign(T(Inf), x)
    end
    if k > 0 # normal case
        xu = (xu & ~exponent_mask(T)) | (rem(k, uinttype(T)) << significand_bits(T))
        return reinterpret(T, xu)
    else # subnormal case
        if k <= -significand_bits(T) # underflow
            # overflow, for the case of integer overflow in n + k
            e > 50000 && return flipsign(T(Inf), x)
            return flipsign(T(0.0), x)
        end
        k += significand_bits(T)
        z = T(2.0)^-significand_bits(T)
        xu = (xu & ~exponent_mask(T)) | (rem(k, uinttype(T)) << significand_bits(T))
        return z*reinterpret(T, xu)
    end
end
ldexp(x::Float16, q::Integer) = Float16(ldexp(Float32(x), q))

"""
    exponent(x) -> Int

Get the exponent of a normalized floating-point number.
"""
function exponent(x::T) where T<:IEEEFloat
    @noinline throw1(x) = throw(DomainError(x, "Cannot be NaN or Inf."))
    @noinline throw2(x) = throw(DomainError(x, "Cannot be subnormal converted to 0."))
    xs = reinterpret(Unsigned, x) & ~sign_mask(T)
    xs >= exponent_mask(T) && throw1(x)
    k = Int(xs >> significand_bits(T))
    if k == 0 # x is subnormal
        xs == 0 && throw2(x)
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

rem(x::Float64, y::Float64, ::RoundingMode{:Nearest}) =
    ccall((:remainder, libm),Float64,(Float64,Float64),x,y)
rem(x::Float32, y::Float32, ::RoundingMode{:Nearest}) =
    ccall((:remainderf, libm),Float32,(Float32,Float32),x,y)
rem(x::Float16, y::Float16, r::RoundingMode{:Nearest}) = Float16(rem(Float32(x), Float32(y), r))


"""
    modf(x)

Return a tuple `(fpart, ipart)` of the fractional and integral parts of a number. Both parts
have the same sign as the argument.

# Examples
```jldoctest
julia> modf(3.5)
(0.5, 3.0)

julia> modf(-3.5)
(-0.5, -3.0)
```
"""
modf(x) = rem(x,one(x)), trunc(x)

function modf(x::Float32)
    temp = Ref{Float32}()
    f = ccall((:modff, libm), Float32, (Float32, Ptr{Float32}), x, temp)
    f, temp[]
end

function modf(x::Float64)
    temp = Ref{Float64}()
    f = ccall((:modf, libm), Float64, (Float64, Ptr{Float64}), x, temp)
    f, temp[]
end

@inline function ^(x::Float64, y::Float64)
    z = ccall("llvm.pow.f64", llvmcall, Float64, (Float64, Float64), x, y)
    if isnan(z) & !isnan(x+y)
        throw_exp_domainerror(x)
    end
    z
end
@inline function ^(x::Float32, y::Float32)
    z = ccall("llvm.pow.f32", llvmcall, Float32, (Float32, Float32), x, y)
    if isnan(z) & !isnan(x+y)
        throw_exp_domainerror(x)
    end
    z
end
@inline ^(x::Float64, y::Integer) = ccall("llvm.pow.f64", llvmcall, Float64, (Float64, Float64), x, Float64(y))
@inline ^(x::Float32, y::Integer) = ccall("llvm.pow.f32", llvmcall, Float32, (Float32, Float32), x, Float32(y))
@inline ^(x::Float16, y::Integer) = Float16(Float32(x) ^ y)
@inline literal_pow(::typeof(^), x::Float16, ::Val{p}) where {p} = Float16(literal_pow(^,Float32(x),Val(p)))

## rem2pi-related calculations ##

function add22condh(xh::Float64, xl::Float64, yh::Float64, yl::Float64)
    # This algorithm, due to Dekker, computes the sum of two
    # double-double numbers and returns the high double. References:
    # [1] http://www.digizeitschriften.de/en/dms/img/?PID=GDZPPN001170007
    # [2] https://doi.org/10.1007/BF01397083
    r = xh+yh
    s = (abs(xh) > abs(yh)) ? (xh-r+yh+yl+xl) : (yh-r+xh+xl+yl)
    zh = r+s
    return zh
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
  be the most accurate result. See also [`RoundNearest`](@ref).

- if `r == RoundToZero`, then the result is in the interval ``[0, 2π]`` if `x` is positive,.
  or ``[-2π, 0]`` otherwise. See also [`RoundToZero`](@ref).

- if `r == RoundDown`, then the result is in the interval ``[0, 2π]``.
  See also [`RoundDown`](@ref).
- if `r == RoundUp`, then the result is in the interval ``[-2π, 0]``.
  See also [`RoundUp`](@ref).

# Examples
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

    n,y = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add/subtract pi
            if y.hi <= 0
                return add22condh(y.hi,y.lo,pi2o2_h,pi2o2_l)
            else
                return add22condh(y.hi,y.lo,-pi2o2_h,-pi2o2_l)
            end
        else          # n % 4 == 0: add 0
            return y.hi+y.lo
        end
    else
        if n & 2 == 2 # n % 4 == 3: subtract pi/2
            return add22condh(y.hi,y.lo,-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y.hi,y.lo,pi1o2_h,pi1o2_l)
        end
    end
end
function rem2pi(x::Float64, ::RoundingMode{:ToZero})
    ax = abs(x)
    ax <= 2*Float64(pi,RoundDown) && return x

    n,y = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            z = add22condh(y.hi,y.lo,pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y.hi > 0
                z = y.hi+y.lo
            else      # negative: add 2pi
                z = add22condh(y.hi,y.lo,pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            z = add22condh(y.hi,y.lo,pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            z = add22condh(y.hi,y.lo,pi1o2_h,pi1o2_l)
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

    n,y = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: add pi
            return add22condh(y.hi,y.lo,pi2o2_h,pi2o2_l)
        else          # n % 4 == 0: add 0 or 2pi
            if y.hi > 0
                return y.hi+y.lo
            else      # negative: add 2pi
                return add22condh(y.hi,y.lo,pi4o2_h,pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: add 3pi/2
            return add22condh(y.hi,y.lo,pi3o2_h,pi3o2_l)
        else          # n % 4 == 1: add pi/2
            return add22condh(y.hi,y.lo,pi1o2_h,pi1o2_l)
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

    n,y = rem_pio2_kernel(x)

    if iseven(n)
        if n & 2 == 2 # n % 4 == 2: sub pi
            return add22condh(y.hi,y.lo,-pi2o2_h,-pi2o2_l)
        else          # n % 4 == 0: sub 0 or 2pi
            if y.hi < 0
                return y.hi+y.lo
            else      # positive: sub 2pi
                return add22condh(y.hi,y.lo,-pi4o2_h,-pi4o2_l)
            end
        end
    else
        if n & 2 == 2 # n % 4 == 3: sub pi/2
            return add22condh(y.hi,y.lo,-pi1o2_h,-pi1o2_l)
        else          # n % 4 == 1: sub 3pi/2
            return add22condh(y.hi,y.lo,-pi3o2_h,-pi3o2_l)
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

!!! note
    Depending on the format of the input value, the closest representable value to 2π may
    be less than 2π. For example, the expression `mod2pi(2π)` will not return `0`, because
    the intermediate value of `2*π` is a `Float64` and `2*Float64(π) < 2*big(π)`. See
    [`rem2pi`](@ref) for more refined control of this behavior.

# Examples
```jldoctest
julia> mod2pi(9*pi/4)
0.7853981633974481
```
"""
mod2pi(x) = rem2pi(x,RoundDown)

# generic fallback; for number types, promotion.jl does promotion

"""
    muladd(x, y, z)

Combined multiply-add: computes `x*y+z`, but allowing the add and multiply to be merged
with each other or with surrounding operations for performance.
For example, this may be implemented as an [`fma`](@ref) if the hardware supports it
efficiently.
The result can be different on different machines and can also be different on the same machine
due to constant propagation or other optimizations.
See [`fma`](@ref).

# Examples
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
             :atanh,:exp,:exp2,:exp10,:log,:log2,:log10,:sqrt,:lgamma,:log1p)
    @eval begin
        $func(a::Float16) = Float16($func(Float32(a)))
        $func(a::ComplexF16) = ComplexF16($func(ComplexF32(a)))
    end
end

for func in (:atan,:hypot)
    @eval begin
        $func(a::Float16,b::Float16) = Float16($func(Float32(a),Float32(b)))
    end
end

cbrt(a::Float16) = Float16(cbrt(Float32(a)))
sincos(a::Float16) = Float16.(sincos(Float32(a)))

# helper functions for Libm functionality

"""
    highword(x)

Return the high word of `x` as a `UInt32`.
"""
@inline highword(x::Float64) = highword(reinterpret(UInt64, x))
@inline highword(x::UInt64)  = (x >>> 32) % UInt32
@inline highword(x::Float32) = reinterpret(UInt32, x)

@inline fromhighword(::Type{Float64}, u::UInt32) = reinterpret(Float64, UInt64(u) << 32)
@inline fromhighword(::Type{Float32}, u::UInt32) = reinterpret(Float32, u)


"""
    poshighword(x)

Return positive part of the high word of `x` as a `UInt32`.
"""
@inline poshighword(x::Float64) = poshighword(reinterpret(UInt64, x))
@inline poshighword(x::UInt64)  = highword(x) & 0x7fffffff
@inline poshighword(x::Float32) = highword(x) & 0x7fffffff

# More special functions
include("special/cbrt.jl")
include("special/exp.jl")
include("special/exp10.jl")
include("special/ldexp_exp.jl")
include("special/hyperbolic.jl")
include("special/trig.jl")
include("special/rem_pio2.jl")
include("special/log.jl")

# `missing` definitions for functions in this module
for f in (:(acos), :(acosh), :(asin), :(asinh), :(atan), :(atanh),
          :(sin), :(sinh), :(cos), :(cosh), :(tan), :(tanh),
          :(exp), :(exp2), :(expm1), :(log), :(log10), :(log1p),
          :(log2), :(exponent), :(sqrt))
    @eval $(f)(::Missing) = missing
end
clamp(::Missing, lo, hi) = missing

end # module
