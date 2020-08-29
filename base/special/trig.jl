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
# atan functions are based on openlibm code: s_atan.c, s_atanf.c.
# acos functions are based on openlibm code: e_acos.c, e_acosf.c.
# asin functions are based on openlibm code: e_asin.c, e_asinf.c. The above
# functions are made available under the following licence:

## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
##
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this
## software is freely granted, provided that this notice
## is preserved.

# Trigonometric functions
# sin methods
@noinline sin_domain_error(x) = throw(DomainError(x, "sin(x) is only defined for finite x."))
function sin(x::T) where T<:Union{Float32, Float64}
    absx = abs(x)
    if absx < T(pi)/4 #|x| ~<= pi/4, no need for reduction
        if absx < sqrt(eps(T))
            return x
        end
        return sin_kernel(x)
    elseif isnan(x)
        return T(NaN)
    elseif isinf(x)
        sin_domain_error(x)
    end
    n, y = rem_pio2_kernel(x)
    n = n&3
    if n == 0
        return sin_kernel(y)
    elseif n == 1
        return cos_kernel(y)
    elseif n == 2
        return -sin_kernel(y)
    else
        return -cos_kernel(y)
    end
end
sin(x::Real) = sin(float(x))

# Coefficients in 13th order polynomial approximation on [0; π/4]
#     sin(x) ≈ x + S1*x³ + S2*x⁵ + S3*x⁷ + S4*x⁹ + S5*x¹¹ + S6*x¹³
# D for double, S for sin, number is the order of x-1
const DS1 = -1.66666666666666324348e-01
const DS2 = 8.33333333332248946124e-03
const DS3 = -1.98412698298579493134e-04
const DS4 = 2.75573137070700676789e-06
const DS5 = -2.50507602534068634195e-08
const DS6 = 1.58969099521155010221e-10

"""
    sin_kernel(yhi, ylo)

Computes the sine on the interval [-π/4; π/4].
"""
@inline function sin_kernel(y::DoubleFloat64)
    y² = y.hi*y.hi
    y⁴ = y²*y²
    r  = @horner(y², DS2, DS3, DS4) + y²*y⁴*@horner(y², DS5, DS6)
    y³ = y²*y.hi
    y.hi-((y²*(0.5*y.lo-y³*r)-y.lo)-y³*DS1)
end
@inline function sin_kernel(y::Float64)
    y² =  y*y
    y⁴ =  y²*y²
    r  =  @horner(y², DS2, DS3, DS4) + y²*y⁴*@horner(y², DS5, DS6)
    y³ =  y²*y
    y+y³*(DS1+y²*r)
end

# sin_kernels accepting values from rem_pio2 in the Float32 case
@inline sin_kernel(x::Float32) = sin_kernel(DoubleFloat32(x))
@inline function sin_kernel(y::DoubleFloat32)
    S1 = -0.16666666641626524
    S2 = 0.008333329385889463
    z = y.hi*y.hi
    w = z*z
    r = @horner(z, -0.00019839334836096632, 2.718311493989822e-6)
    s = z*y.hi
    Float32((y.hi + s*@horner(z, S1, S2)) + s*w*r)
end

# cos methods
@noinline cos_domain_error(x) = throw(DomainError(x, "cos(x) is only defined for finite x."))
function cos(x::T) where T<:Union{Float32, Float64}
    absx = abs(x)
    if absx < T(pi)/4
        if absx < sqrt(eps(T)/T(2.0))
            return T(1.0)
        end
        return cos_kernel(x)
    elseif isnan(x)
        return T(NaN)
    elseif isinf(x)
        cos_domain_error(x)
    else
        n, y = rem_pio2_kernel(x)
        n = n&3
        if n == 0
            return cos_kernel(y)
        elseif n == 1
            return -sin_kernel(y)
        elseif n == 2
            return -cos_kernel(y)
        else
            return sin_kernel(y)
        end
    end
end
cos(x::Real) = cos(float(x))

const DC1 = 4.16666666666666019037e-02
const DC2 = -1.38888888888741095749e-03
const DC3 = 2.48015872894767294178e-05
const DC4 = -2.75573143513906633035e-07
const DC5 = 2.08757232129817482790e-09
const DC6 = -1.13596475577881948265e-11
"""
    cos_kernel(y)

Compute the cosine on the interval y∈[-π/4; π/4].
"""
@inline function cos_kernel(y::DoubleFloat64)
    y² = y.hi*y.hi
    y⁴ = y²*y²
    r  = y²*@horner(y², DC1, DC2, DC3) + y⁴*y⁴*@horner(y², DC4, DC5, DC6)
    half_y² = 0.5*y²
    w  = 1.0-half_y²
    w + (((1.0-w)-half_y²) + (y²*r-y.hi*y.lo))
end
@inline function cos_kernel(y::Float64)
    y² = y*y
    y⁴ = y²*y²
    r  = y²*@horner(y², DC1, DC2, DC3) + y⁴*y⁴*@horner(y², DC4, DC5, DC6)
    half_y² = 0.5*y²
    w  = 1.0-half_y²
    w + (((1.0-w)-half_y²) + (y²*r))
end

# cos_kernels accepting values from rem_pio2 in the Float32 case
cos_kernel(x::Float32) = cos_kernel(DoubleFloat32(x))
@inline function cos_kernel(y::DoubleFloat32)
    C0 = -0.499999997251031
    C1 = 0.04166662332373906
    y² = y.hi*y.hi
    y⁴ = y²*y²
    r = @horner(y², -0.001388676377460993, 2.439044879627741e-5)
    Float32(((1.0+y²*C0) + y⁴*C1) + (y⁴*y²)*r)
end

### sincos methods
@noinline sincos_domain_error(x) = throw(DomainError(x, "sincos(x) is only defined for finite x."))

"""
    sincos(x)

Simultaneously compute the sine and cosine of `x`, where the `x` is in radians.
"""
function sincos(x::T) where T<:Union{Float32, Float64}
    if abs(x) < T(pi)/4
        if x == zero(T)
            return x, one(T)
        end
        return sincos_kernel(x)
    elseif isnan(x)
        return T(NaN), T(NaN)
    elseif isinf(x)
        sincos_domain_error(x)
    end
    n, y = rem_pio2_kernel(x)
    n = n&3
    # calculate both kernels at the reduced y...
    si, co = sincos_kernel(y)
    # ... and use the same selection scheme as above: (sin, cos, -sin, -cos) for
    # for sin and (cos, -sin, -cos, sin) for cos
    if n == 0
        return si, co
    elseif n == 1
        return co, -si
    elseif n == 2
        return -si, -co
    else
        return -co, si
    end
end

_sincos(x::AbstractFloat) = sincos(x)
_sincos(x) = (sin(x), cos(x))

sincos(x) = _sincos(float(x))



# There's no need to write specialized kernels, as inlining takes care of remo-
# ving superfluous calculations.
@inline sincos_kernel(y::Union{Float32, Float64, DoubleFloat32, DoubleFloat64}) = (sin_kernel(y), cos_kernel(y))

# tangent methods
@noinline tan_domain_error(x) = throw(DomainError(x, "tan(x) is only defined for finite x."))
function tan(x::T) where T<:Union{Float32, Float64}
    absx = abs(x)
    if absx < T(pi)/4
        if absx < sqrt(eps(T))/2 # first order dominates, but also allows tan(-0)=-0
            return x
        end
        return tan_kernel(x)
    elseif isnan(x)
        return T(NaN)
    elseif isinf(x)
        tan_domain_error(x)
    end
    n, y = rem_pio2_kernel(x)
    if iseven(n)
        return tan_kernel(y,1)
    else
        return tan_kernel(y,-1)
    end
end
tan(x::Real) = tan(float(x))

@inline tan_kernel(y::Float64) = tan_kernel(DoubleFloat64(y, 0.0), 1)
@inline function tan_kernel(y::DoubleFloat64, k)
    # kernel tan function on ~[-pi/4, pi/4] (except on -0)
    # Input y is assumed to be bounded by ~pi/4 in magnitude.
    # Input k indicates whether tan (if k = 1) or -1/tan (if k = -1) is returned.

    # Algorithm
    #    1. Since tan(-y) = -tan(y), we need only to consider positive y.
    #    2. Callers must return tan(-0) = -0 without calling here since our
    #       odd polynomial is not evaluated in a way that preserves -0.
    #       Callers may do the optimization tan(y) ~ y for tiny y.
    #    3. tan(y) is approximated by a odd polynomial of degree 27 on
    #       [0,0.67434]
    #                            3             27
    #           tan(y) ~ y + T1*y + ... + T13*y ≡ P(y)
    #       where
    #
    #                          |tan(y)         2     4            26   |     -59.2
    #        (tan(y)-P(y))/y = |----- - (1+T1*y +T2*y +.... +T13*y    )| <= 2
    #                          |  y                                    |
    #
    #       Note: tan(y+z) = tan(y) + tan'(y)*z
    #                  ~ tan(y) + (1+y*y)*z
    #       Therefore, for better accuracz in computing tan(y+z), let
    #             3      2      2       2       2
    #        r = y *(T2+y *(T3+y *(...+y *(T12+y *T13))))
    #       then
    #                     3    2
    #        tan(y+z) = y + (T1*y + (y *(r+z)+z))
    #
    #   4. For y in [0.67434,pi/4],  let z = pi/4 - y, then
    #        tan(y) = tan(pi/4-z) = (1-tan(z))/(1+tan(z))
    #               = 1 - 2*(tan(z) - (tan(z)^2)/(1+tan(z)))

    yhi = y.hi
    ylo = y.lo

    if abs(yhi) >= 0.6744
        if yhi < 0.0
            yhi = -yhi
            ylo = -ylo
        end
        # Then, accurately reduce y as "pio4hi"-yhi+"pio4lo"-ylo
        yhi = (pi/4 - yhi) + (3.06161699786838301793e-17 - ylo)
        # yhi is guaranteed to be exact, so ylo is identically zero
        ylo = 0.0
    end
    y² = yhi * yhi
    y⁴ = y² * y²

    # Break P(y)-T1*y³ = y^5*(T[2]+y^2*T[3]+...) into y⁵*r + y⁵*v where
    # r = T[2]+y^4*T[4]+...+y^20*T[12])
    # v = (y^2*(T[3]+y^4*T[5]+...+y^22*[T13]))
    r = @horner(y⁴,
        1.33333333333201242699e-01, # T2
        2.18694882948595424599e-02, # T4
        3.59207910759131235356e-03, # T6
        5.88041240820264096874e-04, # T8
        7.81794442939557092300e-05, # T10
        -1.85586374855275456654e-05) # T12
    v = y² * @horner(y⁴,
        5.39682539762260521377e-02, # T3
        8.86323982359930005737e-03, # T5
        1.45620945432529025516e-03, # T7
        2.46463134818469906812e-04, # T9
        7.14072491382608190305e-05, # T11
        2.59073051863633712884e-05) # T13
    # Precompute y³
    y³ = y² * yhi
    # Calculate  P(y)-y-T1*y³ =  y⁵*r + y⁵*v  = y²(y³*(r+v))
    r = ylo + y² * (y³ * (r + v) + ylo)
    # Calculate P(y)-y = r+T1*y³
    r += 3.33333333333334091986e-01*y³
    # Calculate w = r+y = P(y)
    Px = yhi + r
    if abs(y.hi) >= 0.6744
        # If the original y was above the threshold, then we calculate
        #     tan(y) = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
        #            ≈ 1 - 2*(P(z) - (P(z)^2)/(1+P(z)))
        # where z = y-π/4.
        return (signbit(y.hi) ? -1.0 : 1.0)*(k - 2*(yhi-(Px^2/(k+Px)-r)))
    end
    if k == 1
        # Else, we simply return w = P(y) if k == 1 (integer multiple from argument
        # reduction was even)...
        return Px
    else
        # ...or tan(y) ≈ -1.0/(y+r) if !(k == 1) (integer multiple from argument
        # reduction was odd). If 2ulp error is allowed, simply return the frac-
        # tion directly. Instead, we calculate it accurately.

        # Px0 is w with zeroed out low word
        Px0 = reinterpret(Float64, (reinterpret(UInt64, Px) >> 32) << 32)
        v = r - (Px0 - yhi) # Px0+v = r+y
        t = a = -1.0 / Px
        # zero out low word of t
        t = reinterpret(Float64, (reinterpret(UInt64, t) >> 32) << 32)
        s = 1.0 + t * Px0
        return t + a * (s + t * v)
    end
end

@inline tan_kernel(y::Float32) = tan_kernel(DoubleFloat32(y), 1)
@inline function tan_kernel(y::DoubleFloat32, k)
    # |tan(y)/y - t(y)| < 2**-25.5 (~[-2e-08, 2e-08]). */
    y² = y.hi*y.hi
    r  = @horner(y², 0.00297435743359967304927, 0.00946564784943673166728)
    t  = @horner(y², 0.0533812378445670393523, 0.0245283181166547278873)
    y⁴ = y²*y²
    y³ = y²*y.hi
    u  = @horner(y², 0.333331395030791399758, 0.133392002712976742718)
    Py  = (y.hi+y³*u)+(y³*y⁴)*(t+y⁴*r)
    if k == 1
        return Float32(Py)
    end

    return Float32(-1.0/Py)
end

# fallback methods
sin_kernel(x::Real) = sin(x)
cos_kernel(x::Real) = cos(x)
tan_kernel(x::Real) = tan(x)
sincos_kernel(x::Real) = sincos(x)

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
asin(x::Real) = asin(float(x))

# atan methods
ATAN_1_O_2_HI(::Type{Float64}) = 4.63647609000806093515e-01 # atan(0.5).hi
ATAN_2_O_2_HI(::Type{Float64}) = 7.85398163397448278999e-01 # atan(1.0).hi
ATAN_3_O_2_HI(::Type{Float64}) = 9.82793723247329054082e-01 # atan(1.5).hi
ATAN_INF_HI(::Type{Float64}) = 1.57079632679489655800e+00 # atan(Inf).hi

ATAN_1_O_2_HI(::Type{Float32}) = 4.6364760399f-01 # atan(0.5).hi
ATAN_2_O_2_HI(::Type{Float32}) = 7.8539812565f-01 # atan(1.0).hi
ATAN_3_O_2_HI(::Type{Float32}) = 9.8279368877f-01 # atan(1.5).hi
ATAN_INF_HI(::Type{Float32}) = 1.5707962513f+00 # atan(Inf).hi

ATAN_1_O_2_LO(::Type{Float64}) = 2.26987774529616870924e-17 # atan(0.5).lo
ATAN_2_O_2_LO(::Type{Float64}) = 3.06161699786838301793e-17 # atan(1.0).lo
ATAN_3_O_2_LO(::Type{Float64}) = 1.39033110312309984516e-17 # atan(1.5).lo
ATAN_INF_LO(::Type{Float64}) = 6.12323399573676603587e-17 # atan(Inf).lo

ATAN_1_O_2_LO(::Type{Float32}) = 5.0121582440f-09  # atan(0.5).lo
ATAN_2_O_2_LO(::Type{Float32}) = 3.7748947079f-08  # atan(1.0).lo
ATAN_3_O_2_LO(::Type{Float32}) = 3.4473217170f-08  # atan(1.5).lo
ATAN_INF_LO(::Type{Float32}) = 7.5497894159f-08  # atan(Inf).lo

ATAN_LARGE_X(::Type{Float64}) = 2.0^66 # seems too large? 2.0^60 gives the same
ATAN_SMALL_X(::Type{Float64}) = 2.0^-27
ATAN_LARGE_X(::Type{Float32}) = 2.0f0^26
ATAN_SMALL_X(::Type{Float32}) = 2.0f0^-12

atan_p(z::Float64, w::Float64) = z*@horner(w,
     3.33333333333329318027e-01,
     1.42857142725034663711e-01,
     9.09088713343650656196e-02,
     6.66107313738753120669e-02,
     4.97687799461593236017e-02,
     1.62858201153657823623e-02)
atan_q(w::Float64) = w*@horner(w,
     -1.99999999998764832476e-01,
     -1.11111104054623557880e-01,
     -7.69187620504482999495e-02,
     -5.83357013379057348645e-02,
     -3.65315727442169155270e-02)
atan_p(z::Float32, w::Float32) = z*@horner(w, 3.3333328366f-01,  1.4253635705f-01, 6.1687607318f-02)
atan_q(w::Float32) = w*@horner(w, -1.9999158382f-01, -1.0648017377f-01)
@inline function atan_pq(x)
    x² = x*x
    x⁴ = x²*x²
    # break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly
    atan_p(x², x⁴), atan_q(x⁴)
end

atan(x::Real) = atan(float(x))
function atan(x::T) where T<:Union{Float32, Float64}
    # Method
    #   1. Reduce x to positive by atan(x) = -atan(-x).
    #   2. According to the integer k=4t+0.25 chopped, t=x, the argument
    #      is further reduced to one of the following intervals and the
    #      arctangent of t is evaluated by the corresponding formula:
    #
    #      [0,7/16]      atan(x) = t-t^3*(a1+t^2*(a2+...(a10+t^2*a11)...)
    #      [7/16,11/16]  atan(x) = atan(1/2) + atan( (t-0.5)/(1+t/2) )
    #      [11/16.19/16] atan(x) = atan( 1 ) + atan( (t-1)/(1+t) )
    #      [19/16,39/16] atan(x) = atan(3/2) + atan( (t-1.5)/(1+1.5t) )
    #      [39/16,INF]   atan(x) = atan(INF) + atan( -1/t )
    #
    #  If isnan(x) is true, then the nan value will eventually be passed to
    #  atan_pq(x) and return the appropriate nan value.

    absx = abs(x)
    if absx >= ATAN_LARGE_X(T)
        return copysign(T(1.5707963267948966), x)
    end
    if absx < T(7/16)
        # no reduction needed
        if absx < ATAN_SMALL_X(T)
            return x
        end
        p, q = atan_pq(x)
        return x - x*(p + q)
    end
    xsign = sign(x)
    if absx < T(19/16) # 7/16 <= |x| < 19/16
        if absx < T(11/16) # 7/16 <= |x| <11/16
            hi = ATAN_1_O_2_HI(T)
            lo = ATAN_1_O_2_LO(T)
            x = (T(2.0)*absx - T(1.0))/(T(2.0) + absx)
        else # 11/16 <= |x| < 19/16
            hi = ATAN_2_O_2_HI(T)
            lo = ATAN_2_O_2_LO(T)
            x  = (absx - T(1.0))/(absx + T(1.0))
        end
    else
        if absx < T(39/16)  # 19/16 <= |x| < 39/16
            hi = ATAN_3_O_2_HI(T)
            lo = ATAN_3_O_2_LO(T)
            x = (absx - T(1.5))/(T(1.0) + T(1.5)*absx)
        else # 39/16 <= |x| < upper threshold (2.0^66 or 2.0f0^26)
            hi = ATAN_INF_HI(T)
            lo = ATAN_INF_LO(T)
            x  = -T(1.0)/absx
        end
    end
    # end of argument reduction
    p, q = atan_pq(x)
    z = hi - ((x*(p + q) - lo) - x)
    copysign(z, xsign)
end
# atan2 methods
ATAN2_PI_LO(::Type{Float32}) = -8.7422776573f-08
ATAN2_RATIO_BIT_SHIFT(::Type{Float32}) = 23
ATAN2_RATIO_THRESHOLD(::Type{Float32}) = 26

ATAN2_PI_LO(::Type{Float64}) = 1.2246467991473531772E-16
ATAN2_RATIO_BIT_SHIFT(::Type{Float64}) = 20
ATAN2_RATIO_THRESHOLD(::Type{Float64}) = 60

function atan(y::T, x::T) where T<:Union{Float32, Float64}
    # Method :
    #    M1) Reduce y to positive by atan2(y,x)=-atan2(-y,x).
    #    M2) Reduce x to positive by (if x and y are unexceptional):
    #        ARG (x+iy) = arctan(y/x)          ... if x > 0,
    #        ARG (x+iy) = pi - arctan[y/(-x)]   ... if x < 0,
    #
    # Special cases:
    #
    #    S1) ATAN2((anything), NaN ) is NaN;
    #    S2) ATAN2(NAN , (anything) ) is NaN;
    #    S3) ATAN2(+-0, +(anything but NaN)) is +-0  ;
    #    S4) ATAN2(+-0, -(anything but NaN)) is +-pi ;
    #    S5) ATAN2(+-(anything but 0 and NaN), 0) is +-pi/2;
    #    S6) ATAN2(+-(anything but INF and NaN), +INF) is +-0 ;
    #    S7) ATAN2(+-(anything but INF and NaN), -INF) is +-pi;
    #    S8) ATAN2(+-INF,+INF ) is +-pi/4 ;
    #    S9) ATAN2(+-INF,-INF ) is +-3pi/4;
    #    S10) ATAN2(+-INF, (anything but,0,NaN, and INF)) is +-pi/2;
    if isnan(x) || isnan(y) # S1 or S2
        return T(NaN)
    end

    if x == T(1.0) # then y/x = y and x > 0, see M2
        return atan(y)
    end
    # generate an m ∈ {0, 1, 2, 3} to branch off of
    m = 2*signbit(x) + 1*signbit(y)

    if iszero(y)
        if m == 0 || m == 1
            return y # atan(+-0, +anything) = +-0
        elseif m == 2
            return T(pi) # atan(+0, -anything) = pi
        elseif m == 3
            return -T(pi) # atan(-0, -anything) =-pi
        end
    elseif iszero(x)
        return flipsign(T(pi)/2, y)
    end

    if isinf(x)
        if isinf(y)
            if m == 0
                return T(pi)/4  # atan(+Inf), +Inf))
            elseif m == 1
                return -T(pi)/4 # atan(-Inf), +Inf))
            elseif m == 2
                return 3*T(pi)/4 # atan(+Inf, -Inf)
            elseif m == 3
                return -3*T(pi)/4 # atan(-Inf,-Inf)
            end
        else
            if m == 0
                return zero(T)  # atan(+...,+Inf) */
            elseif m == 1
                return -zero(T) # atan(-...,+Inf) */
            elseif m == 2
                return T(pi)    # atan(+...,-Inf) */
            elseif m == 3
                return -T(pi)   # atan(-...,-Inf) */
            end
        end
    end

    # x wasn't Inf, but y is
    isinf(y) && return copysign(T(pi)/2, y)

    ypw = poshighword(y)
    xpw = poshighword(x)
    # compute y/x for Float32
    k = reinterpret(Int32, ypw-xpw)>>ATAN2_RATIO_BIT_SHIFT(T)

    if k > ATAN2_RATIO_THRESHOLD(T) # |y/x| >  threshold
        z=T(pi)/2+T(0.5)*ATAN2_PI_LO(T)
        m&=1;
    elseif x<0 && k < -ATAN2_RATIO_THRESHOLD(T) # 0 > |y|/x > threshold
        z = zero(T)
    else #safe to do y/x
        z = atan(abs(y/x))
    end

    if m == 0
        return z # atan(+,+)
    elseif m == 1
        return -z # atan(-,+)
    elseif m == 2
        return T(pi)-(z-ATAN2_PI_LO(T)) # atan(+,-)
    else # default case m == 3
        return (z-ATAN2_PI_LO(T))-T(pi) # atan(-,-)
    end
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
acos(x::Real) = acos(float(x))

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

# Rationals
function sinpi(x::T) where T<:Rational
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

# Rationals
function cospi(x::T) where T<:Rational
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

"""
    sincospi(x)

Simultaneously compute `sinpi(x)` and `cospi(x)`, where the `x` is in radians.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
function sincospi(x::T) where T<:AbstractFloat
    if !isfinite(x)
        isnan(x) && return x, x
        throw(DomainError(x, "`x` cannot be infinite."))
    end

    ax = abs(x)
    s = maxintfloat(T)
    ax >= s && return (copysign(zero(T), x), one(T)) # even integer-valued

    # reduce to interval [-1,1]
    # assumes RoundNearest rounding mode
    t = 3*(s/2)
    rx = x-((x+t)-t) # zeros may be incorrectly signed
    arx = abs(rx)

    # same selection scheme as sinpi and cospi
    if (arx == 0) | (arx == 1)
        return copysign(zero(T), x), ifelse(ax % 2 == 0, one(T), -one(T))
    elseif arx < 0.25
        return sincos_kernel(mulpi_ext(rx))
    elseif arx < 0.75
        y = mulpi_ext(T(0.5) - arx)
        return copysign(cos_kernel(y), rx), sin_kernel(y)
    else
        y_si = mulpi_ext(copysign(one(T), rx) - rx)
        y_co = mulpi_ext(one(T) - arx)
        return sin_kernel(y_si), -cos_kernel(y_co)
    end
end

# Rationals
function sincospi(x::T) where T<:Rational
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

    # same selection scheme as sinpi and cospi
    if (arx == 0) | (arx == 1)
        return copysign(zero(Tf),x), ifelse(iseven(numerator(x)), one(Tf), -one(Tf))
    elseif arx < 0.25
        return sincos_kernel(mulpi_ext(rx))
    elseif arx < 0.75
        y = mulpi_ext(T(0.5) - arx)
        return copysign(cos_kernel(y), rx), sin_kernel(y)
    else
        y_si = mulpi_ext(copysign(one(T), rx) - rx)
        y_co = mulpi_ext(one(T) - arx)
        return sin_kernel(y_si), -cos_kernel(y_co)
    end
end

sinpi(x::Integer) = x >= 0 ? zero(float(x)) : -zero(float(x))
cospi(x::Integer) = isodd(x) ? -one(float(x)) : one(float(x))
sincospi(x::Integer) = (sinpi(x), cospi(x))
sinpi(x::Real) = sinpi(float(x))
cospi(x::Real) = cospi(float(x))
sincospi(x::Real) = sincospi(float(x))

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
        sipi, copi = sincospi(zr)
        Complex(sipi*cosh(pizi), copi*sinh(pizi))
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
        sipi, copi = sincospi(zr)
        Complex(copi*cosh(pizi), -sipi*sinh(pizi))
    end
end

function sincospi(z::Complex{T}) where T
    F = float(T)
    zr, zi = reim(z)
    if isinteger(zr)
        # zr = ...,-2,-1,0,1,2,...
        # sin(pi*zr) == ±0
        # cos(pi*zr) == ±1
        # cosh(pi*zi) > 0
        s = copysign(zero(F),zr)
        c_pos = isa(zr,Integer) ? iseven(zr) : isinteger(zr/2)
        pizi = pi*zi
        sh, ch = sinh(pizi), cosh(pizi)
        (
            Complex(s, c_pos ? sh : -sh),
            Complex(c_pos ? ch : -ch, isnan(zi) ? s : -flipsign(s,zi)),
        )
    elseif isinteger(2*zr)
        # zr = ...,-1.5,-0.5,0.5,1.5,2.5,...
        # sin(pi*zr) == ±1
        # cos(pi*zr) == +0
        # sign(sinh(pi*zi)) == sign(zi)
        s_pos = isinteger((2*zr-1)/4)
        pizi = pi*zi
        sh, ch = sinh(pizi), cosh(pizi)
        (
            Complex(s_pos ? ch : -ch, isnan(zi) ? zero(F) : copysign(zero(F),zi)),
            Complex(zero(F), s_pos ? -sh : sh),
        )
    elseif !isfinite(zr)
        if zi == 0
            Complex(F(NaN), F(zi)), Complex(F(NaN), isnan(zr) ? zero(F) : -flipsign(F(zi),zr))
        elseif isinf(zi)
            Complex(F(NaN), F(zi)), Complex(F(Inf), F(NaN))
        else
            Complex(F(NaN), F(NaN)), Complex(F(NaN), F(NaN))
        end
    else
        pizi = pi*zi
        sipi, copi = sincospi(zr)
        sihpi, cohpi = sinh(pizi), cosh(pizi)
        (
            Complex(sipi*cohpi, copi*sihpi),
            Complex(copi*cohpi, -sipi*sihpi),
        )
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
        """ ($finv)(z::Number) = inv(($f)(z))
        @doc """
            $($hname)(x)

        Compute the hyperbolic $($fn) of `x`.
        """ ($finvh)(z::Number) = inv(($fh)(z))
        @doc """
            $($dname)(x)

        Compute the $($fn) of `x`, where `x` is in degrees.
        """ ($finvd)(z::Number) = inv(($fd)(z))
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
        Compute the inverse $($fn) of `x`, where the output is in radians. """ ($tfa)(y::Number) = ($tfainv)(inv(y))
        @doc """
            $($hname)(x)
        Compute the inverse hyperbolic $($fn) of `x`. """ ($hfa)(y::Number) = ($hfainv)(inv(y))
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

"""
    sincosd(x)

Simultaneously compute the sine and cosine of `x`, where `x` is in degrees.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function sincosd(x::Real)
    if isinf(x)
        return throw(DomainError(x, "sincosd(x) is only defined for finite `x`."))
    elseif isnan(x)
        return (oftype(x,NaN), oftype(x,NaN))
    end

    # It turns out that calling those functions separately yielded better
    # performance than considering each case and calling `sincos_kernel`.
    return (sind(x), cosd(x))
end

sincosd(::Missing) = (missing, missing)

for (fd, f, fn) in ((:sind, :sin, "sine"), (:cosd, :cos, "cosine"), (:tand, :tan, "tangent"))
    name = string(fd)
    @eval begin
        @doc """
            $($name)(x)
        Compute $($fn) of `x`, where `x` is in degrees. """ ($fd)(z) = ($f)(deg2rad(z))
    end
end

for (fd, f, fn) in ((:asind, :asin, "sine"), (:acosd, :acos, "cosine"),
                    (:asecd, :asec, "secant"), (:acscd, :acsc, "cosecant"), (:acotd, :acot, "cotangent"))
    name = string(fd)
    @eval begin
        @doc """
            $($name)(x)

        Compute the inverse $($fn) of `x`, where the output is in degrees. """ ($fd)(y) = rad2deg(($f)(y))
    end
end

"""
    atand(y)
    atand(y,x)

Compute the inverse tangent of `y` or `y/x`, respectively, where the output is in degrees.
"""
atand(y)    = rad2deg(atan(y))
atand(y, x) = rad2deg(atan(y,x))
