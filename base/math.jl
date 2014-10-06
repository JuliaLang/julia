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
       ceil, floor, trunc, round, significand, 
       lgamma, hypot, gamma, lfact, max, min, minmax, ldexp, frexp,
       clamp, modf, ^, mod2pi,
       airy, airyai, airyprime, airyaiprime, airybi, airybiprime, airyx,
       besselj0, besselj1, besselj, besseljx,
       bessely0, bessely1, bessely, besselyx,
       hankelh1, hankelh2, hankelh1x, hankelh2x,
       besseli, besselix, besselk, besselkx, besselh,
       beta, lbeta, eta, zeta, polygamma, invdigamma, digamma, trigamma,
       erfinv, erfcinv, @evalpoly

import Base: log, exp, sin, cos, tan, sinh, cosh, tanh, asin,
             acos, atan, asinh, acosh, atanh, sqrt, log2, log10,
             max, min, minmax, ceil, floor, trunc, round, ^, exp2,
             exp10, expm1, log1p

import Core.Intrinsics: nan_dom_err, sqrt_llvm, box, unbox, powi_llvm

# non-type specific math functions

clamp{X,L,H}(x::X, lo::L, hi::H) =
    ifelse(x > hi, convert(promote_type(X,L,H), hi),
           ifelse(x < lo,
                  convert(promote_type(X,L,H), lo),
                  convert(promote_type(X,L,H), x)))

clamp{T}(x::AbstractArray{T,1}, lo, hi) = [clamp(xx, lo, hi) for xx in x]
clamp{T}(x::AbstractArray{T,2}, lo, hi) =
    [clamp(x[i,j], lo, hi) for i in 1:size(x,1), j in 1:size(x,2)]
clamp{T}(x::AbstractArray{T}, lo, hi) =
    reshape([clamp(xx, lo, hi) for xx in x], size(x))

# evaluate p[1] + x * (p[2] + x * (....)), i.e. a polynomial via Horner's rule
macro horner(x, p...)
    ex = esc(p[end])
    for i = length(p)-1:-1:1
        ex = :($(esc(p[i])) + t * $ex)
    end
    Expr(:block, :(t = $(esc(x))), ex)
end

# Evaluate p[1] + z*p[2] + z^2*p[3] + ... + z^(n-1)*p[n].  This uses
# Horner's method if z is real, but for complex z it uses a more
# efficient algorithm described in Knuth, TAOCP vol. 2, section 4.6.4,
# equation (3).
macro evalpoly(z, p...)
    a = :($(esc(p[end])))
    b = :($(esc(p[end-1])))
    as = []
    for i = length(p)-2:-1:1
        ai = symbol(string("a", i))
        push!(as, :($ai = $a))
        a = :($b + r*$ai)
        b = :($(esc(p[i])) - s * $ai)
    end
    ai = :a0
    push!(as, :($ai = $a))
    C = Expr(:block,
             :(x = real(tt)),
             :(y = imag(tt)),
             :(r = x + x),
             :(s = x*x + y*y),
             as...,
             :($ai * tt + $b))
    R = Expr(:macrocall, symbol("@horner"), :tt, p...)
    :(let tt = $(esc(z))
          isa(tt, Complex) ? $C : $R
      end)
end

rad2deg(z::Real) = oftype(z, 57.29577951308232*z)
deg2rad(z::Real) = oftype(z, 0.017453292519943295*z)
rad2deg(z::Integer) = rad2deg(float(z))
deg2rad(z::Integer) = deg2rad(float(z))
@vectorize_1arg Real rad2deg
@vectorize_1arg Real deg2rad

log(b,x) = log(x)./log(b)

# type specific math functions

const libm = Base.libm_name
const openspecfun = "libopenspecfun"

# functions with no domain error
for f in (:cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :exp2, :expm1)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

# fallback definitions to prevent infinite loop from $f(x::Real) def above
cbrt(x::FloatingPoint) = x^(1//3)
exp2(x::FloatingPoint) = 2^x
for f in (:sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :expm1)
    @eval ($f)(x::FloatingPoint) = error("not implemented for ", typeof(x))
end

# TODO: GNU libc has exp10 as an extension; should openlibm?
exp10(x::Float64) = 10.0^x
exp10(x::Float32) = 10.0f0^x
exp10(x::Integer) = exp10(float(x))
@vectorize_1arg Number exp10

# functions that return NaN on non-NaN argument for domain error
for f in (:sin, :cos, :tan, :asin, :acos, :acosh, :atanh, :log, :log2, :log10,
          :lgamma, :log1p)
    @eval begin
        ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)),libm), Float64, (Float64,), x), x)
        ($f)(x::Float32) = nan_dom_err(ccall(($(string(f,"f")),libm), Float32, (Float32,), x), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

sqrt(x::Float64) = box(Float64,sqrt_llvm(unbox(Float64,x)))
sqrt(x::Float32) = box(Float32,sqrt_llvm(unbox(Float32,x)))
sqrt(x::Real) = sqrt(float(x))
@vectorize_1arg Number sqrt

for f in (:ceil, :trunc, :significand) # :rint, :nearbyint
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        @vectorize_1arg Real $f
    end
end

round(x::Float32) = ccall((:roundf, libm), Float32, (Float32,), x)
@vectorize_1arg Real round

floor(x::Float32) = ccall((:floorf, libm), Float32, (Float32,), x)
@vectorize_1arg Real floor

hypot(x::Real, y::Real) = hypot(promote(float(x), float(y))...)
function hypot{T<:FloatingPoint}(x::T, y::T) 
    x = abs(x)
    y = abs(y)
    if x < y
        x, y = y, x
    end
    if x == 0
        r = y/one(x)
    else
        r = y/x
        if isnan(r)
            isinf(x) && return x
            isinf(y) && return y
            return r
        end
    end
    x * sqrt(one(r)+r*r)
end

atan2(x::Real, y::Real) = atan2(promote(float(x),float(y))...)
atan2{T<:FloatingPoint}(x::T, y::T) = Base.no_op_err("atan2", T)

for f in (:atan2, :hypot)
    @eval begin
        ($f)(x::Float64, y::Float64) = ccall(($(string(f)),libm), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32, Float32), x, y)
        @vectorize_2arg Number $f
    end
end

max{T<:FloatingPoint}(x::T, y::T) = ifelse((y > x) | (x != x), y, x)
@vectorize_2arg Real max

min{T<:FloatingPoint}(x::T, y::T) = ifelse((y < x) | (x != x), y, x)
@vectorize_2arg Real min

minmax{T<:FloatingPoint}(x::T, y::T) =  x <= y ? (x, y) :
                                        x >  y ? (y, x) :
                                        x == x ? (x, x) : (y, y)

function exponent(x::Float64)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogb,libm), Int32, (Float64,), x))
end
function exponent(x::Float32)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogbf,libm), Int32, (Float32,), x))
end
@vectorize_1arg Real exponent

ldexp(x::Float64,e::Int) = ccall((:scalbn,libm),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall((:scalbnf,libm), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize ldexp

function frexp(x::Float64)
    xu = reinterpret(Uint64,x)
    k = int(xu >> 52) & 0x07ff
    if k == 0 # x is subnormal
        x == zero(x) && return x,0
        x *= 1.8014398509481984e16 # 0x1p54, normalise significand
        xu = reinterpret(Uint64,x)
        k = int(xu >> 52) & 0x07ff - 54
    elseif k == 0x07ff # NaN or Inf
        return x,0
    end
    k -= 1022
    xu = (xu & 0x800f_ffff_ffff_ffff) | 0x3fe0_0000_0000_0000
    reinterpret(Float64,xu), k
end
function frexp(x::Float32)
    xu = reinterpret(Uint32,x)
    k = int(xu >> 23) & 0x00ff
    if k == 0 # x is subnormal
        x == zero(x) && return x,0
        x *= 3.3554432f7 # 0x1p25: no Float32 hex literal
        xu = reinterpret(Uint32,x)
        k = int(xu >> 23) & 0x00ff - 25
    elseif k == 0x00ff # NaN or Inf
        return x,0
    end
    k -= 126
    xu = (xu & 0x807f_ffff) | 0x3f00_0000
    reinterpret(Float32,xu), k
end

function frexp{T<:FloatingPoint}(A::Array{T})
    f = similar(A)
    e = Array(Int, size(A))
    for i = 1:length(A)
        f[i], e[i] = frexp(A[i])
    end
    return (f, e)
end

modf(x) = rem(x,one(x)), trunc(x)

const _modff_temp = Float32[0]
function modf(x::Float32)
    f = ccall((:modff,libm), Float32, (Float32,Ptr{Float32}), x, _modff_temp)
    f, _modff_temp[1]
end

const _modf_temp = Float64[0]
function modf(x::Float64)
    f = ccall((:modf,libm), Float64, (Float64,Ptr{Float64}), x, _modf_temp)
    f, _modf_temp[1]
end

^(x::Float64, y::Float64) = nan_dom_err(ccall((:pow,libm),  Float64, (Float64,Float64), x, y), x+y)
^(x::Float32, y::Float32) = nan_dom_err(ccall((:powf,libm), Float32, (Float32,Float32), x, y), x+y)

^(x::Float64, y::Integer) =
    box(Float64, powi_llvm(unbox(Float64,x), unbox(Int32,int32(y))))
^(x::Float32, y::Integer) =
    box(Float32, powi_llvm(unbox(Float32,x), unbox(Int32,int32(y))))

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

## mod2pi-related calculations ##

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

function mod2pi(x::Float64) # or modtau(x)
# with r = mod2pi(x)
# a) 0 <= r < 2π  (note: boundary open or closed - a bit fuzzy, due to rem_pio2 implementation)
# b) r-x = k*2π with k integer

# note: mod(n,4) is 0,1,2,3; while mod(n-1,4)+1 is 1,2,3,4.
# We use the latter to push negative y in quadrant 0 into the positive (one revolution, + 4*pi/2)

    if x < pi4o2_h
        if 0.0 <= x return x end
        if x > -pi4o2_h 
            return add22condh(x,0.0,pi4o2_h,pi4o2_l)
        end
    end

    (n,y) = ieee754_rem_pio2(x)

    if iseven(n)
        if n & 2 == 2 # add pi
            return add22condh(y[1],y[2],pi2o2_h,pi2o2_l)
        else # add 0 or 2pi
            if y[1] > 0.0
                return y[1]
            else # else add 2pi
                return add22condh(y[1],y[2],pi4o2_h,pi4o2_l)
            end
        end
    else # add pi/2 or 3pi/2
        if n & 2 == 2 # add 3pi/2
            return add22condh(y[1],y[2],pi3o2_h,pi3o2_l) 
        else # add pi/2
            return add22condh(y[1],y[2],pi1o2_h,pi1o2_l) 
        end
    end
end

mod2pi(x::Float32) = float32(mod2pi(float64(x)))
mod2pi(x::Int32) = mod2pi(float64(x))
function mod2pi(x::Int64)
  fx = float64(x)
  fx == x || error("Integer argument to mod2pi is too large: $x")
  mod2pi(fx)
end

# More special functions

include("special/trig.jl")
include("special/bessel.jl")
include("special/erf.jl")
include("special/gamma.jl")

end # module
