# This file is a part of Julia. Except for the asin, acos and atan functions (see below),
# license is MIT: https://julialang.org/license

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
### sincos methods

_sincos(x::AbstractFloat) = sincos(x)
_sincos(x) = (sin(x), cos(x))

"""
    sincos(x::T) where T -> Tuple{float(T),float(T)}

Simultaneously compute the sine and cosine of `x`, where `x` is in radians, returning
a tuple `(sine, cosine)`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `(T(NaN), T(NaN))` if `isnan(x)`.

See also [`cis`](@ref), [`sincospi`](@ref), [`sincosd`](@ref).
"""
sincos(x) = _sincos(float(x))

# Float16, Float32 and Float64 sin/cos/sincos/tan, the corresponding functions of π*x (sinpi, ...)
# and of x in degrees (sind, ...), based on CORE-MATH's sinf/cosf (https://core-math.gitlabpages.inria.fr/,
# MIT licensed). The angle is reduced to π/16*(n + z) with |z| <= 1/2, and the angle-addition
# formulas are applied with sin(π n/16) and cos(π n/16) from a 32-entry table, so there is no
# branch on the quadrant. Float16 is computed in Float32 and Float32 in Float64, so each result is
# rounded only once at the end; Float64 uses hi + lo table entries and one exact product instead.
# Only the reduction differs between radians, π*x and degrees.

# sin(π j/16) for j = 0:31; cos(π j/16) is entry j + 8
const SINPI16_TABLE = (0x0p+0, 0x1.8f8b83c69a60bp-3, 0x1.87de2a6aea963p-2, 0x1.1c73b39ae68c8p-1,
    0x1.6a09e667f3bcdp-1, 0x1.a9b66290ea1a3p-1, 0x1.d906bcf328d46p-1, 0x1.f6297cff75cbp-1,
    0x1p+0, 0x1.f6297cff75cbp-1, 0x1.d906bcf328d46p-1, 0x1.a9b66290ea1a3p-1,
    0x1.6a09e667f3bcdp-1, 0x1.1c73b39ae68c8p-1, 0x1.87de2a6aea963p-2, 0x1.8f8b83c69a60bp-3,
    0x0p+0, -0x1.8f8b83c69a60bp-3, -0x1.87de2a6aea963p-2, -0x1.1c73b39ae68c8p-1,
    -0x1.6a09e667f3bcdp-1, -0x1.a9b66290ea1a3p-1, -0x1.d906bcf328d46p-1, -0x1.f6297cff75cbp-1,
    -0x1p+0, -0x1.f6297cff75cbp-1, -0x1.d906bcf328d46p-1, -0x1.a9b66290ea1a3p-1,
    -0x1.6a09e667f3bcdp-1, -0x1.1c73b39ae68c8p-1, -0x1.87de2a6aea963p-2, -0x1.8f8b83c69a60bp-3)
const SINPI16_TABLE_F32 = map(Float32, SINPI16_TABLE)
# :nothrow needed since the compiler can't prove the index is inbounds.
@assume_effects :nothrow sinpi16_table(::Type{Float64}, n::Int) = getfield(SINPI16_TABLE, n & 31 + 1)
@assume_effects :nothrow sinpi16_table(::Type{Float32}, n::Int) = getfield(SINPI16_TABLE_F32, n & 31 + 1)

# Float64: the low parts of SINPI16_TABLE, so that sin(π j/16) ≈ hi + lo
const SINPI16_TABLE_LO = (0.0, -0x1.26d19b9ff8d82p-57, -0x1.72cedd3d5a61p-57, 0x1.b25dd267f66p-55,
    -0x1.bdd3413b26456p-55, 0x1.9f630e8b6dac8p-60, 0x1.457e610231ac2p-56, 0x1.562172a361fd3p-56,
    0.0, 0x1.562172a361fd3p-56, 0x1.457e610231ac2p-56, 0x1.9f630e8b6dac8p-60,
    -0x1.bdd3413b26456p-55, 0x1.b25dd267f66p-55, -0x1.72cedd3d5a61p-57, -0x1.26d19b9ff8d82p-57,
    0.0, 0x1.26d19b9ff8d82p-57, 0x1.72cedd3d5a61p-57, -0x1.b25dd267f66p-55,
    0x1.bdd3413b26456p-55, -0x1.9f630e8b6dac8p-60, -0x1.457e610231ac2p-56, -0x1.562172a361fd3p-56,
    0.0, -0x1.562172a361fd3p-56, -0x1.457e610231ac2p-56, -0x1.9f630e8b6dac8p-60,
    0x1.bdd3413b26456p-55, -0x1.b25dd267f66p-55, 0x1.72cedd3d5a61p-57, 0x1.26d19b9ff8d82p-57)
@assume_effects :nothrow sinpi16_table_hilo(n::Int) =
    getfield(SINPI16_TABLE, n & 31 + 1), getfield(SINPI16_TABLE_LO, n & 31 + 1)

# Returns (sin(π z/16), 1 - cos(π z/16)): minimax polynomials with errors of about 2^-37 (relative)
# and 2^-34 (absolute) in Float64 (for Float32 results), and 2^-23 and 2^-20 in Float32 (for Float16
# results). They are fitted for |z| <= 0.53, which covers the rounding of n in the reductions.
@inline function sincos16_poly(z::Float64)
    z2 = z*z
    z4 = z2*z2
    sz = z*muladd(z4, 2.4310853410670896e-6, muladd(z2, -0.0012616485288428397, 0.1963495408478149))
    omc = z2*muladd(z2, -6.189991248606663e-5, 0.01927656839146767)
    return sz, omc
end
@inline function sincos16_poly(z::Float32)
    z2 = z*z
    return z*muladd(z2, -0.0012609607f0, 0.19634952f0), z2*0.019262165f0
end

# For Float16 and Float32 (computed in Float32 and Float64) and |z| <= 1/2, returns
# (sin(π(n+z)/16), cos(π(n+z)/16)) via the angle-addition formulas.
@inline function sincos16_kernel(z::T, n::Int) where T<:Union{Float32, Float64}
    sz, omc = sincos16_poly(z)
    s0 = sinpi16_table(T, n)
    c0 = sinpi16_table(T, n + 8)
    return muladd(sz, c0, muladd(-omc, s0, s0)), muladd(-sz, s0, muladd(-omc, c0, c0))
end

# For Float64: returns sin(π n/16 + r) and cos(π n/16 + r) for r = rh + rl with |r| <= π/32,
# each as an unevaluated sum hi + lo. The leading products cos(π n/16)*rh and sin(π n/16)*rh are
# formed exactly and added to the table values with Fast2Sum (|sin(π n/16)| >= |cos(π n/16)*rh|
# unless sin(π n/16) == 0, and similarly for cos); the remaining terms only need Float64 accuracy.
@inline function sincos16_kernel_hilo(rh::Float64, rl::Float64, n::Int)
    s0h, s0l = sinpi16_table_hilo(n)
    c0h, c0l = sinpi16_table_hilo(n + 8)
    r2 = rh*rh
    r4 = r2*r2
    # sin(r) - rh and cos(r) - 1 (minimax polynomials for |r| <= 0.53*π/16, evaluated by Estrin's
    # scheme)
    sr = muladd(rh*r2, muladd(r4, muladd(r2, 2.7549955351646673e-6, -0.0001984126909501825),
                                  muladd(r2, 0.008333333333303527, -0.16666666666666663)), rl)
    omc = r2*muladd(r4, muladd(r2, 2.4794226240351404e-5, -0.0013888888211012165),
                        muladd(r2, 0.04166666666642064, -0.4999999999999997))
    ph = c0h*rh
    sh = s0h + ph
    sl = (((s0h - sh) + ph) + (fma(c0h, rh, -ph) + muladd(c0l, rh, s0l))) + muladd(c0h, sr, s0h*omc)
    qh = -s0h*rh
    ch = c0h + qh
    cl = (((c0h - ch) + qh) + (fma(-s0h, rh, -qh) + muladd(-s0l, rh, c0l))) + muladd(-s0h, sr, c0h*omc)
    return sh, sl, ch, cl
end
@inline function sincos16_kernel(rh::Float64, rl::Float64, n::Int)
    sh, sl, ch, cl = sincos16_kernel_hilo(rh, rl, n)
    return sh + sl, ch + cl
end

# tan as sin/cos. For Float64, the quotient of the unevaluated sums is compensated with one
# reciprocal; when cos is exactly zero (only at table points), the quotient is already ±Inf.
@inline function tan16(rh::Float64, rl::Float64, n::Int)
    sh, sl, ch, cl = sincos16_kernel_hilo(rh, rl, n)
    s = sh + sl
    se = (sh - s) + sl
    c = ch + cl
    ce = (ch - c) + cl
    ic = inv(c)
    q = s*ic
    return ifelse(iszero(c), q, muladd(fma(-q, c, s) + muladd(-q, ce, se), ic, q))
end
@inline function tan16(z::Union{Float32, Float64}, n::Int)
    si, co = sincos16_kernel(z, n)
    return si/co
end

# The reductions for π*x and degrees, returning the same form as rem_pio16 (radians, in
# pi_remainders.jl): they take a = |x| for finite x and find n and z with |z| <= 1/2 and
# a = (n + z)/16 (for π*x) or a = 11.25*(n + z) (for degrees). Float16 and Float32 return (z, n);
# Float64 returns (rh, rl, n), with the reduced angle π/16*z = rh + rl in radians.

# π*x: 16a is exact. All Float32 with a >= 2^24 are even integers, which behave like a = 0.
@inline function rem_pi16(a::Float32)
    ad = Float64(ifelse(a < 0x1p24, a, 0f0))
    id, n = roundmul(ad, 16.0)
    return 16*ad - id, n
end
@inline function rem_pi16(a::Float16)
    af = Float32(a)
    id, n = roundmul(af, 16f0)
    return 16*af - id, n
end

# π*x, Float64: 16a and z = 16a - n are exact, and r = π/16*z is formed as hi + lo. All Float64
# with a >= 2^53 are even integers, which behave like a = 0. (16a can exceed 2^51, so n is computed
# with round rather than roundmul.)
@inline function rem_pi16(a::Float64)
    t = 16*ifelse(a < 0x1p53, a, 0.0)
    n = round(t)
    z = t - n
    rh = PIO16_HI*z
    return rh, muladd(PIO16_LO, z, fma(PIO16_HI, z, -rh)), unsafe_trunc(Int, n)
end

# Degrees: 11.25 is exact, and for a < 2^48 (all finite Float16) both n*11.25 and a - n*11.25
# are exact, so multiples of 11.25 (including all zeros of sind and cosd) give z == 0. Larger
# Float32 are first reduced exactly by 360 = 32*11.25, which doesn't change n & 31.
@inline function rem_deg16(a::Float32)
    ad = a < 0x1p48 ? Float64(a) : Float64(@noinline rem(a, 360f0))
    fn, n = roundmul(ad, 4/45)
    return (ad - 11.25*fn)*(4/45), n
end
@inline function rem_deg16(a::Float16)
    af = Float32(a)
    fn, n = roundmul(af, 4f0/45)
    return (af - 11.25f0*fn)*(4f0/45), n
end

# Degrees, Float64: as for Float32, a - 11.25*n is exact (here for a < 2^50), and
# r = π/180*(a - 11.25*n) is formed as hi + lo.
@inline function rem_deg16(a::Float64)
    a = a < 0x1p50 ? a : @noinline rem(a, 360.0)
    fn, k = roundmul(a, 4/45)
    d = a - 11.25*fn
    rh = 0x1.1df46a2529d39p-6*d
    return rh, muladd(0x1.5c1d8becdd291p-62, d, fma(0x1.1df46a2529d39p-6, d, -rh)), k
end

@noinline trig16_nonfinite(f::Symbol, x) = isnan(x) ? x : throw_finite_domainerror(f, x)

for T in (Float16, Float32, Float64), (rem16, fsin, fcos, fsincos, ftan) in (
        (:rem_pio16, :sin, :cos, :sincos, :tan),
        (:rem_pi16, :sinpi, :cospi, :sincospi, :tanpi),
        (:rem_deg16, :sind, :cosd, :sincosd, :tand))
    @eval begin
        # The odd functions are computed at |x| and flipsign keeps the sign of zero results.
        function $fsin(x::$T)
            isfinite(x) || return trig16_nonfinite($(QuoteNode(fsin)), x)
            si, _ = sincos16_kernel($rem16(abs(x))...)
            return flipsign($T(si), x)
        end
        function $fcos(x::$T)
            isfinite(x) || return trig16_nonfinite($(QuoteNode(fcos)), x)
            _, co = sincos16_kernel($rem16(abs(x))...)
            return $T(co)
        end
        function $fsincos(x::$T)
            if !isfinite(x)
                y = trig16_nonfinite($(QuoteNode(fsincos)), x)
                return y, y
            end
            si, co = sincos16_kernel($rem16(abs(x))...)
            return flipsign($T(si), x), $T(co)
        end
        function $ftan(x::$T)
            isfinite(x) || return trig16_nonfinite($(QuoteNode(ftan)), x)
            return flipsign($T(tan16($rem16(abs(x))...)), x)
        end
    end
end

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

@noinline asin_domain_error(x) = throw(DomainError(x, "asin(x) is not defined for |x| > 1."))
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
    if isnan(x) | isnan(y) # S1 or S2
        return isnan(x) ? x : y
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
    k = reinterpret(Int32, ypw -% xpw)>>ATAN2_RATIO_BIT_SHIFT(T)

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

"""
    sinpi(x::T) where T -> float(T)

Compute ``\\sin(\\pi x)`` more accurately than `sin(pi*x)`, especially for large `x`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.

See also [`sind`](@ref), [`cospi`](@ref), [`sincospi`](@ref).
"""
sinpi(x::Number)

"""
    cospi(x::T) where T -> float(T)

Compute ``\\cos(\\pi x)`` more accurately than `cos(pi*x)`, especially for large `x`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.

See also [`cispi`](@ref), [`sincosd`](@ref), [`sinpi`](@ref).
"""
cospi(x::Number)

"""
    sincospi(x::T) where T -> Tuple{float(T),float(T)}

Simultaneously compute [`sinpi(x)`](@ref) and [`cospi(x)`](@ref) (the sine and cosine of `π*x`,
where `x` is in radians), returning a tuple `(sine, cosine)`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `(T(NaN), T(NaN))` tuple if `isnan(x)`.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.

See also [`cispi`](@ref), [`sincosd`](@ref), [`sinpi`](@ref).
"""
sincospi(x::Number)

"""
    tanpi(x::T) where T -> float(T)

Compute ``\\tan(\\pi x)`` more accurately than `tan(pi*x)`, especially for large `x`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.

!!! compat "Julia 1.10"
    This function requires at least Julia 1.10.

See also [`tand`](@ref), [`sinpi`](@ref), [`cospi`](@ref), [`sincospi`](@ref).
"""
tanpi(x::Number)

sinpi(x::Integer) = x >= 0 ? zero(float(x)) : -zero(float(x))
cospi(x::Integer) = isodd(x) ? -one(float(x)) : one(float(x))
tanpi(x::Integer) = x >= 0 ? (isodd(x) ? -zero(float(x)) : zero(float(x))) :
                             (isodd(x) ? zero(float(x)) : -zero(float(x)))
sincospi(x::Integer) = (sinpi(x), cospi(x))
sinpi(x::AbstractFloat) = sin(pi*x)
cospi(x::AbstractFloat) = cos(pi*x)
sincospi(x::AbstractFloat) = sincos(pi*x)
tanpi(x::AbstractFloat) = tan(pi*x)

function tanpi(z::Complex)
    zr, zi = reim(z)
    iszero(zi) && return Complex(tanpi(zr))
    sr, cr = sincospi(zr)
    ti = tanh(zi * pi)
    cz = Complex(cr, ti * sr)
    Complex(sr, ti * cr) * cz / abs2(cz)
end

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
    fastabs(x::Number)

Faster `abs`-like function for rough magnitude comparisons.
`fastabs` is equivalent to `abs(x)` for most `x`,
but for complex `x` it computes `abs(real(x))+abs(imag(x))` rather
than requiring `hypot`.
"""
fastabs(x::Number) = abs(x)
fastabs(z::Complex) = abs(real(z)) + abs(imag(z))

# sinc and cosc are zero if the real part is Inf and imag is finite
isinf_real(x::Real) = isinf(x)
isinf_real(x::Complex) = isinf(real(x)) && isfinite(imag(x))
isinf_real(x::Number) = false

"""
    sinc(x::T) where {T <: Number} -> float(T)

Compute normalized sinc function ``\\operatorname{sinc}(x) = \\sin(\\pi x) / (\\pi x)`` if ``x \\neq 0``, and ``1`` if ``x = 0``.

Return a `T(NaN)` if `isnan(x)`.

See also [`cosc`](@ref), its derivative.
"""
sinc(x::Number) = _sinc(float(x))
sinc(x::Integer) = iszero(x) ? one(x) : zero(x)
_sinc(x::Number) = iszero(x) ? one(x) : isinf_real(x) ? zero(x) : sinpi(x)/(pi*x)
_sinc_threshold(::Type{Float64}) = 0.001
_sinc_threshold(::Type{Float32}) = 0.05f0
@inline _sinc(x::Union{T,Complex{T}}) where {T<:Union{Float32,Float64}} =
    fastabs(x) < _sinc_threshold(T) ? evalpoly(x^2, (T(1), -T(pi)^2/6, T(pi)^4/120)) : isinf_real(x) ? zero(x) : sinpi(x)/(pi*x)
_sinc(x::Float16) = Float16(_sinc(Float32(x)))
_sinc(x::ComplexF16) = ComplexF16(_sinc(ComplexF32(x)))

"""
    cosc(x::T) where {T <: Number} -> float(T)

Compute ``\\cos(\\pi x) / x - \\sin(\\pi x) / (\\pi x^2)`` if ``x \\neq 0``, and ``0`` if
``x = 0``. This is the derivative of `sinc(x)`.

Return a `T(NaN)` if `isnan(x)`.

See also [`sinc`](@ref).
"""
cosc(x::Number) = _cosc(float(x))
function _cosc_generic(x)
    pi_x = pi * x
    (pi_x*cospi(x)-sinpi(x))/(pi_x*x)
end
function _cosc(x::Number)
    # naive cosc formula is susceptible to catastrophic
    # cancellation error near x=0, so we use the Taylor series
    # for small enough |x|.
    if fastabs(x) < 0.5
        # generic Taylor series: π ∑ (-1)^n (πx)^{2n-1}/a(n) where
        # a(n) = (1+2n)*(2n-1)! (= OEIS A174549)
        s = (term = -(π*x))/3
        iszero(s) && return s  # preserve floating-point signed zero
        π²x² = term^2
        ε = eps(fastabs(term)) # error threshold to stop sum
        n = 1
        while true
            n += 1
            term *= π²x²/((1-2n)*(2n-2))
            s += (δs = term/(1+2n))
            fastabs(δs) ≤ ε && break
        end
        return π*s
    else
        return isinf_real(x) ? zero(x) : _cosc_generic(x)
    end
end

#=

## `cosc(x)` for `x` around the first zero, at `x = 0`

`Float32`:

```sollya
prec = 500!;
accurate = ((pi * x) * cos(pi * x) - sin(pi * x)) / (pi * x * x);
b1 = 0.27001953125;
b2 = 0.449951171875;
domain_0 = [-b1/2, b1];
domain_1 = [b1, b2];
machinePrecision = 24;
freeMonomials = [|1, 3, 5, 7|];
freeMonomialPrecisions = [|machinePrecision, machinePrecision, machinePrecision, machinePrecision|];
polynomial_0 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_0);
polynomial_1 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_1);
polynomial_0;
polynomial_1;
```

`Float64`:

```sollya
prec = 500!;
accurate = ((pi * x) * cos(pi * x) - sin(pi * x)) / (pi * x * x);
b1 = 0.1700439453125;
b2 = 0.27001953125;
b3 = 0.340087890625;
b4 = 0.39990234375;
domain_0 = [-b1/2, b1];
domain_1 = [b1, b2];
domain_2 = [b2, b3];
domain_3 = [b3, b4];
machinePrecision = 53;
freeMonomials = [|1, 3, 5, 7, 9, 11|];
freeMonomialPrecisions = [|machinePrecision, machinePrecision, machinePrecision, machinePrecision, machinePrecision, machinePrecision|];
polynomial_0 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_0);
polynomial_1 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_1);
polynomial_2 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_2);
polynomial_3 = fpminimax(accurate, freeMonomials, freeMonomialPrecisions, domain_3);
polynomial_0;
polynomial_1;
polynomial_2;
polynomial_3;
```

=#

function _cos_cardinal_eval(x::AbstractFloat, polynomials_close_to_origin::NTuple)
    function choose_poly(a::AbstractFloat, polynomials_close_to_origin::NTuple{2})
        ((b1, p0), (_, p1)) = polynomials_close_to_origin
        if a ≤ b1
            p0
        else
            p1
        end
    end
    function choose_poly(a::AbstractFloat, polynomials_close_to_origin::NTuple{4})
        ((b1, p0), (b2, p1), (b3, p2), (_, p3)) = polynomials_close_to_origin
        if a ≤ b2  # hardcoded binary search
            if a ≤ b1
                p0
            else
                p1
            end
        else
            if a ≤ b3
                p2
            else
                p3
            end
        end
    end
    a = abs(x)
    if (polynomials_close_to_origin !== ()) && (a ≤ polynomials_close_to_origin[end][1])
        x * evalpoly(x * x, choose_poly(a, polynomials_close_to_origin))
    elseif isinf(x)
        typeof(x)(0)
    else
        _cosc_generic(x)
    end
end

const _cosc_f32 = let b = Float32 ∘ Float16
    (
        (b(0.27), (-3.289868f0, 3.246966f0, -1.1443111f0, 0.20542027f0)),
        (b(0.45), (-3.2898617f0, 3.2467577f0, -1.1420113f0, 0.1965574f0)),
    )
end

const _cosc_f64 = let b = Float64 ∘ Float16
    (
        (b(0.17), (-3.289868133696453, 3.2469697011333203, -1.1445109446992934, 0.20918277797812262, -0.023460519561502552, 0.001772485141534688)),
        (b(0.27), (-3.289868133695205, 3.246969700970421, -1.1445109360543062, 0.20918254132488637, -0.023457115021035743, 0.0017515112964895303)),
        (b(0.34), (-3.289868133634355, 3.246969697075094, -1.1445108347839286, 0.209181201609773, -0.023448079433318045, 0.001726628430505518)),
        (b(0.4),  (-3.289868133074254, 3.2469696736659346, -1.1445104406286049, 0.20917785794416457, -0.02343378376047161, 0.0017019796223768677)),
    )
end

function _cosc(x::Union{Float32, Float64})
    if x isa Float32
        pols = _cosc_f32
    else
        pols = _cosc_f64
    end
    _cos_cardinal_eval(x, pols)
end

# hard-code Float64/Float32 Taylor series, with coefficients
#  Float64.([(-1)^n*big(pi)^(2n)/((2n+1)*factorial(2n-1)) for n = 1:6])
_cosc(x::ComplexF64) =
    fastabs(x) < 0.14 ? x*evalpoly(x^2, (-3.289868133696453, 3.2469697011334144, -1.1445109447325053, 0.2091827825412384, -0.023460810354558236, 0.001781145516372852)) :
    isinf_real(x) ? zero(x) : _cosc_generic(x)
_cosc(x::ComplexF32) =
    fastabs(x) < 0.26f0 ? x*evalpoly(x^2, (-3.289868f0, 3.2469697f0, -1.144511f0, 0.20918278f0)) :
    isinf_real(x) ? zero(x) : _cosc_generic(x)
_cosc(x::Float16) = Float16(_cosc(Float32(x)))
_cosc(x::ComplexF16) = ComplexF16(_cosc(ComplexF32(x)))

for (finv, f, finvh, fh, finvd, fd, fn) in ((:sec, :cos, :sech, :cosh, :secd, :cosd, "secant"),
                                            (:csc, :sin, :csch, :sinh, :cscd, :sind, "cosecant"),
                                            (:cot, :tan, :coth, :tanh, :cotd, :tand, "cotangent"))
    name = string(finv)
    hname = string(finvh)
    dname = string(finvd)
    @eval begin
        @doc """
            $($name)(x::T) where {T <: Number} -> float(T)

        Compute the $($fn) of `x`, where `x` is in radians.

        Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.
        """ ($finv)(z::Number) = inv(($f)(z))
        @doc """
            $($hname)(x::T) where {T <: Number} -> float(T)

        Compute the hyperbolic $($fn) of `x`.

        Return a `T(NaN)` if `isnan(x)`.
        """ ($finvh)(z::Number) = inv(($fh)(z))
        @doc """
            $($dname)(x::T) where {T <: Number} -> float(T)

        Compute the $($fn) of `x`, where `x` is in degrees.

        Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.
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
            $($tname)(x::T) where {T <: Number} -> float(T)

        Compute the inverse $($fn) of `x`, where the output is in radians.
        """ ($tfa)(y::Number) = ($tfainv)(inv(y))
        @doc """
            $($hname)(x::T) where {T <: Number} -> float(T)

        Compute the inverse hyperbolic $($fn) of `x`.
        """ ($hfa)(y::Number) = ($hfainv)(inv(y))
    end
end


function sind(x::Real)
    if isinf(x)
        return throw_finite_domainerror(:sind, x)
    elseif isnan(x)
        return x
    end

    rx = copysign(float(rem(x,360)),x)
    rx isa IEEEFloat && return sind(rx)
    arx = abs(rx)

    if rx == zero(rx)
        return rx
    elseif arx < oftype(rx,45)
        return sin(deg2rad(rx))
    elseif arx <= oftype(rx,135)
        y = deg2rad(oftype(rx,90) - arx)
        return copysign(cos(y),rx)
    elseif arx == oftype(rx,180)
        return copysign(zero(rx),rx)
    elseif arx < oftype(rx,225)
        y = deg2rad((oftype(rx,180) - arx)*sign(rx))
        return sin(y)
    elseif arx <= oftype(rx,315)
        y = deg2rad(oftype(rx,270) - arx)
        return -copysign(cos(y),rx)
    else
        y = deg2rad(rx - copysign(oftype(rx,360),rx))
        return sin(y)
    end
end

function cosd(x::Real)
    if isinf(x)
        return throw_finite_domainerror(:cosd, x)
    elseif isnan(x)
        return x
    end

    rx = abs(float(rem(x,360)))
    rx isa IEEEFloat && return cosd(rx)

    if rx <= oftype(rx,45)
        return cos(deg2rad(rx))
    elseif rx < oftype(rx,135)
        y = deg2rad(oftype(rx,90) - rx)
        return sin(y)
    elseif rx <= oftype(rx,225)
        y = deg2rad(oftype(rx,180) - rx)
        return -cos(y)
    elseif rx < oftype(rx,315)
        y = deg2rad(rx - oftype(rx,270))
        return sin(y)
    else
        y = deg2rad(oftype(rx,360) - rx)
        return cos(y)
    end
end

tand(x::Real) = sind(x) / cosd(x)

"""
    sincosd(x::T) where T -> Tuple{float(T),float(T)}

Simultaneously compute the sine and cosine of `x`, where `x` is in degrees, returning
a tuple `(sine, cosine)`.

Throw a [`DomainError`](@ref) if `isinf(x)`, return a `(T(NaN), T(NaN))` tuple if `isnan(x)`.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
sincosd(x) = (sind(x), cosd(x))

sincosd(::Missing) = (missing, missing)

for (fd, f, fn) in ((:sind, :sin, "sine"), (:cosd, :cos, "cosine"), (:tand, :tan, "tangent"))
    for (fu, un) in ((:deg2rad, "degrees"),)
        name = string(fd)
        @eval begin
            @doc """
                $($name)(x::T) where T -> float(T)

            Compute $($fn) of `x`, where `x` is in $($un).
            If `x` is a matrix, `x` needs to be a square matrix.

            Throw a [`DomainError`](@ref) if `isinf(x)`, return a `T(NaN)` if `isnan(x)`.

            !!! compat "Julia 1.7"
                Matrix arguments require Julia 1.7 or later.
            """ ($fd)(x) = ($f)(($fu).(x))
        end
    end
end

for (fd, f, fn) in ((:asind, :asin, "sine"), (:acosd, :acos, "cosine"),
                    (:asecd, :asec, "secant"), (:acscd, :acsc, "cosecant"), (:acotd, :acot, "cotangent"))

    for (fu, un) in ((:rad2deg, "degrees"),)
        name = string(fd)
        @eval begin
            @doc """
                $($name)(x)

            Compute the inverse $($fn) of `x`, where the output is in $($un).
            If `x` is a matrix, `x` needs to be a square matrix.

            !!! compat "Julia 1.7"
                Matrix arguments require Julia 1.7 or later.
            """ ($fd)(x) = ($fu).(($f)(x))
        end
    end
end

"""
    atand(y::T) where T -> float(T)
    atand(y::T, x::S) where {T,S} -> promote_type(T,S)
    atand(y::AbstractMatrix{T}) where T -> AbstractMatrix{Complex{float(T)}}

Compute the inverse tangent of `y` or `y/x`, respectively, where the output is in degrees.

Return a `NaN` if `isnan(y)` or `isnan(x)`. The returned `NaN` is either a `T` in the single
argument version, or a `promote_type(T,S)` in the two argument version.

!!! compat "Julia 1.7"
    The one-argument method supports square matrix arguments as of Julia 1.7.
"""
atand(y)    = rad2deg.(atan(y))
atand(y, x) = rad2deg.(atan(y,x))
