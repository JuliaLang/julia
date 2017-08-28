# This file is a part of Julia. Except for the *_kernel functions (see below),
# license is MIT: https://julialang.org/license

struct DoubleFloat64
    hi::Float64
    lo::Float64
end
struct DoubleFloat32
    hi::Float64
end

# sin_kernel and cos_kernel functions are only valid for |x| < pi/4 = 0.7854
# translated from openlibm code: k_sin.c, k_cos.c, k_sinf.c, k_cosf.c.
# acos functions are based on openlibm code: e_acos.c, e_acosf.c.
# asin functions are based on openlibm code: e_asin.c, e_asinf.c. The above
# functions are made available under the following licence:

## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
##
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this
## software is freely granted, provided that this notice
## is preserved.

function sin_kernel(x::DoubleFloat64)
    S1 = -1.66666666666666324348e-01
    S2 =  8.33333333332248946124e-03
    S3 = -1.98412698298579493134e-04
    S4 =  2.75573137070700676789e-06
    S5 = -2.50507602534068634195e-08
    S6 =  1.58969099521155010221e-10

    z = x.hi*x.hi
    w = z*z
    r = S2+z*(S3+z*S4) + z*w*(S5+z*S6)
    v = z*x.hi
    x.hi-((z*(0.5*x.lo-v*r)-x.lo)-v*S1)
end

function cos_kernel(x::DoubleFloat64)
    C1 =  4.16666666666666019037e-02
    C2 = -1.38888888888741095749e-03
    C3 =  2.48015872894767294178e-05
    C4 = -2.75573143513906633035e-07
    C5 =  2.08757232129817482790e-09
    C6 = -1.13596475577881948265e-11

    z = x.hi*x.hi
    w = z*z
    r = z*(C1+z*(C2+z*C3)) + w*w*(C4+z*(C5+z*C6))
    hz = 0.5*z
    w = 1.0-hz
    w + (((1.0-w)-hz) + (z*r-x.hi*x.lo))
end

function sin_kernel(x::DoubleFloat32)
    S1 = -0.16666666641626524
    S2 = 0.008333329385889463
    S3 = -0.00019839334836096632
    S4 = 2.718311493989822e-6

    z = x.hi*x.hi
    w = z*z
    r = S3+z*S4
    s = z*x.hi
    Float32((x.hi + s*(S1+z*S2)) + s*w*r)
end

function cos_kernel(x::DoubleFloat32)
    C0 = -0.499999997251031
    C1 = 0.04166662332373906
    C2 = -0.001388676377460993
    C3 = 2.439044879627741e-5

    z = x.hi*x.hi
    w = z*z
    r = C2+z*C3
    Float32(((1.0+z*C0) + w*C1) + (w*z)*r)
end

# fallback methods
sin_kernel(x::Real) = sin(x)
cos_kernel(x::Real) = cos(x)

# Inverse trigonometric functions
# asin methods
ASIN_X_MIN_THRESHOLD(::Type{Float32}) = 2.0f0^-12
ASIN_X_MIN_THRESHOLD(::Type{Float64}) = sqrt(eps(Float64))

arc_p(t::Float64) =
    t*@horner(t,
    1.66666666666666657415e-01,
    -3.25565818622400915405e-01,
    2.01212532134862925881e-01,
    -4.00555345006794114027e-02,
    7.91534994289814532176e-04,
    3.47933107596021167570e-05)

arc_q(z::Float64) =
    @horner(z,
    1.0,
    -2.40339491173441421878e+00,
    2.02094576023350569471e+00,
    -6.88283971605453293030e-01,
    7.70381505559019352791e-02)

arc_p(t::Float32) =
    t*@horner(t,
    1.6666586697f-01,
    -4.2743422091f-02,
    -8.6563630030f-03)

arc_q(t::Float32) = @horner(t, 1.0f0, -7.0662963390f-01)

@inline arc_tRt(t) = arc_p(t)/arc_q(t)


@inline function asin_kernel(t::Float64, x::Float64)
    # we use that for 1/2 <= x < 1 we have
    #     asin(x) = pi/2-2*asin(sqrt((1-x)/2))
    # Let y = (1-x), z = y/2, s := sqrt(z), and pio2_hi+pio2_lo=pi/2;
    # then for x>0.98
    #     asin(x) = pi/2 - 2*(s+s*z*R(z))
    #         = pio2_hi - (2*(s+s*z*R(z)) - pio2_lo)
    # For x<=0.98, let pio4_hi = pio2_hi/2, then
    #     f = hi part of s;
    #     c = sqrt(z) - f = (z-f*f)/(s+f)     ...f+c=sqrt(z)
    #  and
    #     asin(x) = pi/2 - 2*(s+s*z*R(z))
    #         = pio4_hi+(pio4-2s)-(2s*z*R(z)-pio2_lo)
    #         = pio4_hi+(pio4-2f)-(2s*z*R(z)-(pio2_lo+2c))
    pio2_lo = 6.12323399573676603587e-17
    s = sqrt_llvm(t)
    tRt = arc_tRt(t)
    if abs(x) >= 0.975 # |x| > 0.975
        return flipsign(pi/2 - (2.0*(s + s*tRt) - pio2_lo), x)
    else
        s0 = reinterpret(Float64, (reinterpret(UInt64, s) >> 32) << 32)
        c = (t - s0*s0)/(s + s0)
        p = 2.0*s*tRt - (pio2_lo - 2.0*c)
        q = pi/4 - 2.0*s0
        return flipsign(pi/4 - (p-q), x)
    end
end
@inline function asin_kernel(t::Float32, x::Float32)
    s = sqrt_llvm(Float64(t))
    tRt = arc_tRt(t) # rational approximation
    flipsign(Float32(pi/2 - 2*(s + s*tRt)), x)
end

@noinline asin_domain_error(x) = throw(DomainError(x, "asin(x) is not defined for |x|>1."))
function asin(x::T) where T<:Union{Float32, Float64}
    # Since  asin(x) = x + x^3/6 + x^5*3/40 + x^7*15/336 + ...
    # we approximate asin(x) on [0,0.5] by
    #     asin(x) = x + x*x^2*R(x^2)
    # where
    #     R(x^2) is a rational approximation of (asin(x)-x)/x^3
    # and its remez error is bounded by
    #     |(asin(x)-x)/x^3 - R(x^2)| < 2^(-58.75)
    absx = abs(x)
    if absx >= T(1.0) # |x|>= 1
        if absx == T(1.0)
            return flipsign(T(pi)/2, x)
        end
        asin_domain_error(x)
    elseif absx < T(1.0)/2
        # if |x| sufficiently small, |x| is a good approximation
        if absx < ASIN_X_MIN_THRESHOLD(T)
            return x
        end
        return muladd(x, arc_tRt(x*x), x)
    end
    # else 1/2 <= |x| < 1
    t = (T(1.0) - absx)/2
    return asin_kernel(t, x)
end

# acos methods
ACOS_X_MIN_THRESHOLD(::Type{Float32}) = 2.0f0^-26
ACOS_X_MIN_THRESHOLD(::Type{Float64}) = 2.0^-57
PIO2_HI(::Type{Float32}) = 1.5707962513f+00
PIO2_LO(::Type{Float32}) = 7.5497894159f-08
PIO2_HI(::Type{Float64}) = 1.57079632679489655800e+00
PIO2_LO(::Type{Float64}) = 6.12323399573676603587e-17
ACOS_PI(::Type{Float32}) = 3.1415925026f+00
ACOS_PI(::Type{Float64}) = 3.14159265358979311600e+00
@inline ACOS_CORRECT_LOWWORD(::Type{Float32}, x) = reinterpret(Float32, (reinterpret(UInt32, x) & 0xfffff000))
@inline ACOS_CORRECT_LOWWORD(::Type{Float64}, x) = reinterpret(Float64, (reinterpret(UInt64, x) >> 32) << 32)

@noinline acos_domain_error(x) = throw(DomainError(x, "acos(x) not defined for |x| > 1"))
function acos(x::T) where T <: Union{Float32, Float64}
    # Method :
    #    acos(x)  = pi/2 - asin(x)
    #    acos(-x) = pi/2 + asin(x)
    # As a result, we use the same rational approximation (arc_tRt) as in asin.
    # See the comments in asin for more information about this approximation.
    # 1) For |x| <= 0.5
    #    acos(x) = pi/2 - (x + x*x^2*R(x^2))
    # 2) For x < -0.5
    #    acos(x) = pi - 2asin(sqrt((1 - |x|)/2))
    #        = pi - 0.5*(s+s*z*R(z))
    # where z=(1-|x|)/2, s=sqrt(z)
    # 3) For x > 0.5
    #     acos(x) = pi/2 - (pi/2 - 2asin(sqrt((1 - x)/2)))
    #        = 2asin(sqrt((1 - x)/2))
    #        = 2s + 2s*z*R(z)     ...z=(1 - x)/2, s=sqrt(z)
    #        = 2f + (2c + 2s*z*R(z))
    #    where f=hi part of s, and c = (z - f*f)/(s + f) is the correction term
    #    for f so that f + c ~ sqrt(z).

    # Special cases:
    #    4) if x is NaN, return x itself;
    #    5) if |x|>1 throw warning.

    absx = abs(x)
    if absx >= T(1.0)
        # acos(-1) = π, acos(1) = 0
        absx == T(1.0) && return x > T(0.0) ? T(0.0) : T(pi)
        # acos(x) is not defined for |x| > 1
        acos_domain_error(x) # see 5) above
    elseif absx < T(1.0)/2 # see 1) above
        # if |x| sufficiently small, acos(x) ≈ pi/2
        absx < ACOS_X_MIN_THRESHOLD(T) && return T(pi)/2
        # if |x| < 0.5 we have acos(x) = pi/2 - (x + x*x^2*R(x^2))
        return PIO2_HI(T) - (x - (PIO2_LO(T) - x*arc_tRt(x*x)))
    end
    z = (T(1.0) - absx)*T(0.5)
    zRz = arc_tRt(z)
    s = sqrt_llvm(z)
    if x < T(0.0) # see 2) above
        return ACOS_PI(T) - T(2.0)*(s + (zRz*s - PIO2_LO(T)))
    else # see 3) above
        # if x > 0.5 we have
        # acos(x) = pi/2 - (pi/2 - 2asin(sqrt((1-x)/2)))
        #         = 2asin(sqrt((1-x)/2))
        #         = 2s + 2s*z*R(z)    ...z=(1-x)/2, s=sqrt(z)
        #         = 2f + (2c + 2s*z*R(z))
        # where f=hi part of s, and c = (z-f*f)/(s+f) is the correction term
        # for f so that f+c ~ sqrt(z).
        df = ACOS_CORRECT_LOWWORD(T, s)
        c  = (z - df*df)/(s + df)
        return T(2.0)*(df + (zRz*s + c))
    end
end

# multiply in extended precision
function mulpi_ext(x::Float64)
    m = 3.141592653589793
    m_hi = 3.1415926218032837
    m_lo = 3.178650954705639e-8

    x_hi = reinterpret(Float64, reinterpret(UInt64,x) & 0xffff_ffff_f800_0000)
    x_lo = x-x_hi

    y_hi = m*x
    y_lo = x_hi * m_lo + (x_lo* m_hi + ((x_hi*m_hi-y_hi) + x_lo*m_lo))

    DoubleFloat64(y_hi,y_lo)
end
mulpi_ext(x::Float32) = DoubleFloat32(pi*Float64(x))
mulpi_ext(x::Rational) = mulpi_ext(float(x))
mulpi_ext(x::Real) = pi*x # Fallback

"""
    sinpi(x)

Compute ``\\sin(\\pi x)`` more accurately than `sin(pi*x)`, especially for large `x`.
"""
function sinpi(x::T) where T<:AbstractFloat
    if !isfinite(x)
        isnan(x) && return x
        throw(DomainError(x, "`x` cannot be infinite."))
    end

    ax = abs(x)
    s = maxintfloat(T)/2
    ax >= s && return copysign(zero(T),x) # integer-valued

    # reduce to interval [-1,1]
    # assumes RoundNearest rounding mode
    t = 3*s
    rx = x-((x+t)-t) # zeros may be incorrectly signed
    arx = abs(rx)

    if (arx == 0) | (arx == 1)
        copysign(zero(T),x)
    elseif arx < 0.25
        sin_kernel(mulpi_ext(rx))
    elseif arx < 0.75
        y = mulpi_ext(T(0.5) - arx)
        copysign(cos_kernel(y),rx)
    else
        y = mulpi_ext(copysign(one(T),rx) - rx)
        sin_kernel(y)
    end
end

# Integers and Rationals
function sinpi(x::T) where T<:Union{Integer,Rational}
    Tf = float(T)
    if !isfinite(x)
        throw(DomainError(x, "`x` must be finite."))
    end

    # until we get an IEEE remainder function (#9283)
    rx = rem(x,2)
    if rx > 1
        rx -= 2
    elseif rx < -1
        rx += 2
    end
    arx = abs(rx)

    if (arx == 0) | (arx == 1)
        copysign(zero(Tf),x)
    elseif arx < 0.25
        sin_kernel(mulpi_ext(rx))
    elseif arx < 0.75
        y = mulpi_ext(T(0.5) - arx)
        copysign(cos_kernel(y),rx)
    else
        y = mulpi_ext(copysign(one(T),rx) - rx)
        sin_kernel(y)
    end
end

"""
    cospi(x)

Compute ``\\cos(\\pi x)`` more accurately than `cos(pi*x)`, especially for large `x`.
"""
function cospi(x::T) where T<:AbstractFloat
    if !isfinite(x)
        isnan(x) && return x
        throw(DomainError(x, "`x` cannot be infinite."))
    end

    ax = abs(x)
    s = maxintfloat(T)
    ax >= s && return one(T) # even integer-valued

    # reduce to interval [-1,1], then [0,1]
    # assumes RoundNearest rounding mode
    rx = abs(ax-((ax+s)-s))

    if rx <= 0.25
        cos_kernel(mulpi_ext(rx))
    elseif rx < 0.75
        y = mulpi_ext(T(0.5) - rx)
        sin_kernel(y)
    else
        y = mulpi_ext(one(T) - rx)
        -cos_kernel(y)
    end
end

# Integers and Rationals
function cospi(x::T) where T<:Union{Integer,Rational}
    if !isfinite(x)
        throw(DomainError(x, "`x` must be finite."))
    end

    ax = abs(x)
    # until we get an IEEE remainder function (#9283)
    rx = rem(ax,2)
    if rx > 1
        rx = 2-rx
    end

    if rx <= 0.25
        cos_kernel(mulpi_ext(rx))
    elseif rx < 0.75
        y = mulpi_ext(T(0.5) - rx)
        sin_kernel(y)
    else
        y = mulpi_ext(one(T) - rx)
        -cos_kernel(y)
    end
end

sinpi(x::Integer) = x >= 0 ? zero(float(x)) : -zero(float(x))
cospi(x::Integer) = isodd(x) ? -one(float(x)) : one(float(x))
sinpi(x::Real) = sinpi(float(x))
cospi(x::Real) = cospi(float(x))

function sinpi(z::Complex{T}) where T
    F = float(T)
    zr, zi = reim(z)
    if isinteger(zr)
        # zr = ...,-2,-1,0,1,2,...
        # sin(pi*zr) == ±0
        # cos(pi*zr) == ±1
        # cosh(pi*zi) > 0
        s = copysign(zero(F),zr)
        c_pos = isa(zr,Integer) ? iseven(zr) : isinteger(zr/2)
        sh = sinh(pi*zi)
        Complex(s, c_pos ? sh : -sh)
    elseif isinteger(2*zr)
        # zr = ...,-1.5,-0.5,0.5,1.5,2.5,...
        # sin(pi*zr) == ±1
        # cos(pi*zr) == +0
        # sign(sinh(pi*zi)) == sign(zi)
        s_pos = isinteger((2*zr-1)/4)
        ch = cosh(pi*zi)
        Complex(s_pos ? ch : -ch, isnan(zi) ? zero(F) : copysign(zero(F),zi))
    elseif !isfinite(zr)
        if zi == 0 || isinf(zi)
            Complex(F(NaN), F(zi))
        else
            Complex(F(NaN), F(NaN))
        end
    else
        pizi = pi*zi
        Complex(sinpi(zr)*cosh(pizi), cospi(zr)*sinh(pizi))
    end
end

function cospi(z::Complex{T}) where T
    F = float(T)
    zr, zi = reim(z)
    if isinteger(zr)
        # zr = ...,-2,-1,0,1,2,...
        # sin(pi*zr) == ±0
        # cos(pi*zr) == ±1
        # sign(sinh(pi*zi)) == sign(zi)
        # cosh(pi*zi) > 0
        s = copysign(zero(F),zr)
        c_pos = isa(zr,Integer) ? iseven(zr) : isinteger(zr/2)
        ch = cosh(pi*zi)
        Complex(c_pos ? ch : -ch, isnan(zi) ? s : -flipsign(s,zi))
    elseif isinteger(2*zr)
        # zr = ...,-1.5,-0.5,0.5,1.5,2.5,...
        # sin(pi*zr) == ±1
        # cos(pi*zr) == +0
        # sign(sinh(pi*zi)) == sign(zi)
        s_pos = isinteger((2*zr-1)/4)
        sh = sinh(pi*zi)
        Complex(zero(F), s_pos ? -sh : sh)
    elseif !isfinite(zr)
        if zi == 0
            Complex(F(NaN), isnan(zr) ? zero(F) : -flipsign(F(zi),zr))
        elseif isinf(zi)
            Complex(F(Inf), F(NaN))
        else
            Complex(F(NaN), F(NaN))
        end
    else
        pizi = pi*zi
        Complex(cospi(zr)*cosh(pizi), -sinpi(zr)*sinh(pizi))
    end
end

"""
    sinc(x)

Compute ``\\sin(\\pi x) / (\\pi x)`` if ``x \\neq 0``, and ``1`` if ``x = 0``.
"""
sinc(x::Number) = x==0 ? one(x)  : oftype(x,sinpi(x)/(pi*x))
sinc(x::Integer) = x==0 ? one(x) : zero(x)
sinc(x::Complex{<:AbstractFloat}) = x==0 ? one(x) : oftype(x, sinpi(x)/(pi*x))
sinc(x::Complex) = sinc(float(x))
sinc(x::Real) = x==0 ? one(x) : isinf(x) ? zero(x) : sinpi(x)/(pi*x)

"""
    cosc(x)

Compute ``\\cos(\\pi x) / x - \\sin(\\pi x) / (\\pi x^2)`` if ``x \\neq 0``, and ``0`` if
``x = 0``. This is the derivative of `sinc(x)`.
"""
cosc(x::Number) = x==0 ? zero(x) : oftype(x,(cospi(x)-sinpi(x)/(pi*x))/x)
cosc(x::Integer) = cosc(float(x))
cosc(x::Complex{<:AbstractFloat}) = x==0 ? zero(x) : oftype(x,(cospi(x)-sinpi(x)/(pi*x))/x)
cosc(x::Complex) = cosc(float(x))
cosc(x::Real) = x==0 || isinf(x) ? zero(x) : (cospi(x)-sinpi(x)/(pi*x))/x

for (finv, f, finvh, fh, finvd, fd, fn) in ((:sec, :cos, :sech, :cosh, :secd, :cosd, "secant"),
                                            (:csc, :sin, :csch, :sinh, :cscd, :sind, "cosecant"),
                                            (:cot, :tan, :coth, :tanh, :cotd, :tand, "cotangent"))
    name = string(finv)
    hname = string(finvh)
    dname = string(finvd)
    @eval begin
        @doc """
            $($name)(x)

        Compute the $($fn) of `x`, where `x` is in radians.
        """ ($finv)(z::T) where {T<:Number} = one(T) / (($f)(z))
        @doc """
            $($hname)(x)

        Compute the hyperbolic $($fn) of `x`.
        """ ($finvh)(z::T) where {T<:Number} = one(T) / (($fh)(z))
        @doc """
            $($dname)(x)

        Compute the $($fn) of `x`, where `x` is in degrees.
        """ ($finvd)(z::T) where {T<:Number} = one(T) / (($fd)(z))
    end
end

for (tfa, tfainv, hfa, hfainv, fn) in ((:asec, :acos, :asech, :acosh, "secant"),
                                       (:acsc, :asin, :acsch, :asinh, "cosecant"),
                                       (:acot, :atan, :acoth, :atanh, "cotangent"))
    tname = string(tfa)
    hname = string(hfa)
    @eval begin
        @doc """
            $($tname)(x)
        Compute the inverse $($fn) of `x`, where the output is in radians. """ ($tfa)(y::T) where {T<:Number} = ($tfainv)(one(T) / y)
        @doc """
            $($hname)(x)
        Compute the inverse hyperbolic $($fn) of `x`. """ ($hfa)(y::T) where {T<:Number} = ($hfainv)(one(T) / y)
    end
end


# multiply in extended precision
function deg2rad_ext(x::Float64)
    m = 0.017453292519943295
    m_hi = 0.01745329238474369
    m_lo = 1.3519960527851425e-10

    u = 134217729.0*x # 0x1p27 + 1
    x_hi = u-(u-x)
    x_lo = x-x_hi

    y_hi = m*x
    y_lo = x_hi * m_lo + (x_lo* m_hi + ((x_hi*m_hi-y_hi) + x_lo*m_lo))

    DoubleFloat64(y_hi,y_lo)
end
deg2rad_ext(x::Float32) = DoubleFloat32(deg2rad(Float64(x)))
deg2rad_ext(x::Real) = deg2rad(x) # Fallback

function sind(x::Real)
    if isinf(x)
        return throw(DomainError(x, "`x` cannot be infinite."))
    elseif isnan(x)
        return oftype(x,NaN)
    end

    rx = copysign(float(rem(x,360)),x)
    arx = abs(rx)

    if rx == zero(rx)
        return rx
    elseif arx < oftype(rx,45)
        return sin_kernel(deg2rad_ext(rx))
    elseif arx <= oftype(rx,135)
        y = deg2rad_ext(oftype(rx,90) - arx)
        return copysign(cos_kernel(y),rx)
    elseif arx == oftype(rx,180)
        return copysign(zero(rx),rx)
    elseif arx < oftype(rx,225)
        y = deg2rad_ext((oftype(rx,180) - arx)*sign(rx))
        return sin_kernel(y)
    elseif arx <= oftype(rx,315)
        y = deg2rad_ext(oftype(rx,270) - arx)
        return -copysign(cos_kernel(y),rx)
    else
        y = deg2rad_ext(rx - copysign(oftype(rx,360),rx))
        return sin_kernel(y)
    end
end

function cosd(x::Real)
    if isinf(x)
        return throw(DomainError(x, "`x` cannot be infinite."))
    elseif isnan(x)
        return oftype(x,NaN)
    end

    rx = abs(float(rem(x,360)))

    if rx <= oftype(rx,45)
        return cos_kernel(deg2rad_ext(rx))
    elseif rx < oftype(rx,135)
        y = deg2rad_ext(oftype(rx,90) - rx)
        return sin_kernel(y)
    elseif rx <= oftype(rx,225)
        y = deg2rad_ext(oftype(rx,180) - rx)
        return -cos_kernel(y)
    elseif rx < oftype(rx,315)
        y = deg2rad_ext(rx - oftype(rx,270))
        return sin_kernel(y)
    else
        y = deg2rad_ext(oftype(rx,360) - rx)
        return cos_kernel(y)
    end
end

tand(x::Real) = sind(x) / cosd(x)

for (fd, f, fn) in ((:sind, :sin, "sine"), (:cosd, :cos, "cosine"), (:tand, :tan, "tangent"))
    name = string(fd)
    @eval begin
        @doc """
            $($name)(x)
        Compute $($fn) of `x`, where `x` is in degrees. """ ($fd)(z) = ($f)(deg2rad(z))
    end
end

for (fd, f, fn) in ((:asind, :asin, "sine"), (:acosd, :acos, "cosine"), (:atand, :atan, "tangent"),
                    (:asecd, :asec, "secant"), (:acscd, :acsc, "cosecant"), (:acotd, :acot, "cotangent"))
    name = string(fd)
    @eval begin
        @doc """
            $($name)(x)

        Compute the inverse $($fn) of `x`, where the output is in degrees. """ ($fd)(y) = rad2deg(($f)(y))
    end
end
