# e_j0f.c -- float version of e_j0.c.
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.

# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunPro, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

module J0F

import Base.Math: libm, @evalpoly

sin_unchecked(x::Float32) = ccall((:sinf, libm), Float32, (Float32,), x)
cos_unchecked(x::Float32) = ccall((:cosf, libm), Float32, (Float32,), x)
log_unchecked(x::Float32) = ccall((:logf, libm), Float32, (Float32,), x)

# The asymptotic expansions of pzero is
#     1 - 9 / 128 s^2 + 11025 / 98304 s^4 - ..., where s = 1 / x.
# For x >= 2, We approximate pzero by
#     pzero(x) = 1 + R / S
# where  R = pR0 + pR1 * s^2 + pR2 * s^4 + ... + pR5 * s^10
#     S = 1 + pS0 * s^2 + ... + pS4 * s^10
# and
#     |pzero(x) - 1 - R / S| <= 2^-60.26
const pR8 = (0.0000000000f00, # 0x00000000
             -7.0312500000f-02, # 0xbd900000
             -8.0816707611f+00, # 0xc1014e86
             -2.5706311035f+02, # 0xc3808814
             -2.4852163086f+03, # 0xc51b5376
             -5.2530439453f+03, # 0xc5a4285a
             )::NTuple{6,Float32}
const pS8 = (1.1653436279f+02, # 0x42e91198
             3.8337448730f+03, # 0x456f9beb
             4.0597855469f+04, # 0x471e95db
             1.1675296875f+05, # 0x47e4087c
             4.7627726562f+04, # 0x473a0bba
             )::NTuple{5,Float32}
# for x in [8, 4.5454] = 1 / [0.125, 0.22001]
const pR5 = (-1.1412546255f-11, # 0xad48c58a
             -7.0312492549f-02, # 0xbd8fffff
             -4.1596107483f+00, # 0xc0851b88
             -6.7674766541f+01, # 0xc287597b
             -3.3123129272f+02, # 0xc3a59d9b
             -3.4643338013f+02, # 0xc3ad3779
             )::NTuple{6,Float32}
const pS5 = (6.0753936768f+01, # 0x42730408
             1.0512523193f+03, # 0x44836813
             5.9789707031f+03, # 0x45bad7c4
             9.6254453125f+03, # 0x461665c8
             2.4060581055f+03, # 0x451660ee
             )::NTuple{5,Float32}
# for x in [4.547, 2.8571] = 1 / [0.2199, 0.35001]
const pR3 = (-2.5470459075f-09, # 0xb12f081b
             -7.0311963558f-02, # 0xbd8fffb8
             -2.4090321064f+00, # 0xc01a2d95
             -2.1965976715f+01, # 0xc1afba52
             -5.8079170227f+01, # 0xc2685112
             -3.1447946548f+01, # 0xc1fb9565
             )::NTuple{6,Float32}
const pS3 = (3.5856033325f+01, # 0x420f6c94
             3.6151397705f+02, # 0x43b4c1ca
             1.1936077881f+03, # 0x44953373
             1.1279968262f+03, # 0x448cffe6
             1.7358093262f+02, # 0x432d94b8
             )::NTuple{5,Float32}
# for x in [2.8570, 2] = 1 / [0.3499, 0.5]
const pR2 = (-8.8753431271f-08, # 0xb3be98b7
             -7.0303097367f-02, # 0xbd8ffb12
             -1.4507384300f+00, # 0xbfb9b1cc
             -7.6356959343f+00, # 0xc0f4579f
             -1.1193166733f+01, # 0xc1331736
             -3.2336456776f+00, # 0xc04ef40d
             )::NTuple{6,Float32}
const pS2 = (2.2220300674f+01, # 0x41b1c32d
             1.3620678711f+02, # 0x430834f0
             2.7047027588f+02, # 0x43873c32
             1.5387539673f+02, # 0x4319e01a
             1.4657617569f+01, # 0x416a859a
             )::NTuple{5,Float32}

# Note: This function is only called for ix>=0x40000000 (see below)
function pzerof(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    # @assert ix >= 0x40000000 && ix <= 0x48000000
    z = 1f0 / (x * x)
    if ix >= 0x41000000
        p = pR8
        q = pS8
    elseif ix >= 0x409173eb
        p = pR5
        q = pS5
    elseif ix >= 0x4036d917
        p = pR3
        q = pS3
    else
        p = pR2
        q = pS2
    end
    r = @evalpoly(z, p[1], p[2], p[3], p[4], p[5], p[6])
    s = @evalpoly(z, 1f0, q[1], q[2], q[3], q[4], q[5])
    1f0 + r / s
end

# For x >= 8, the asymptotic expansions of qzero is
#    -1 / 8 s + 75 / 1024 s^3 - ..., where s = 1 / x.
# We approximate pzero by
#    qzero(x) = s * (-1.25 + R / S)
# where  R = qR0 + qR1 * s^2 + qR2 * s^4 + ... + qR5 * s^10
#    S = 1 + qS0 * s^2 + ... + qS5 * s^12
# and
#    |qzero(x) / s + 1.25 - R / S| <= 2^(-61.22)
# for x in [inf, 8] = 1 / [0, 0.125]
const qR8 = (0.0000000000f+00, # 0x00000000
             7.3242187500f-02, # 0x3d960000
             1.1768206596f+01, # 0x413c4a93
             5.5767340088f+02, # 0x440b6b19
             8.8591972656f+03, # 0x460a6cca
             3.7014625000f+04, # 0x471096a0
             )::NTuple{6,Float32}
const qS8 = (1.6377603149f+02, # 0x4323c6aa
             8.0983447266f+03, # 0x45fd12c2
             1.4253829688f+05, # 0x480b3293
             8.0330925000f+05, # 0x49441ed4
             8.4050156250f+05, # 0x494d3359
             -3.4389928125f+05, # 0xc8a7eb69
             )::NTuple{6,Float32}
# for x in [8, 4.5454] = 1 / [0.125, 0.22001]
const qR5 = (1.8408595828f-11, # 0x2da1ec79
             7.3242180049f-02, # 0x3d95ffff
             5.8356351852f+00, # 0x40babd86
             1.3511157227f+02, # 0x43071c90
             1.0272437744f+03, # 0x448067cd
             1.9899779053f+03, # 0x44f8bf4b
             )::NTuple{6,Float32}
const qS5 = (8.2776611328f+01, # 0x42a58da0
             2.0778142090f+03, # 0x4501dd07
             1.8847289062f+04, # 0x46933e94
             5.6751113281f+04, # 0x475daf1d
             3.5976753906f+04, # 0x470c88c1
             -5.3543427734f+03, # 0xc5a752be
             )::NTuple{6,Float32}
# for x in [4.547, 2.8571] = 1 / [0.2199, 0.35001]
const qR3 = (4.3774099900f-09, # 0x3196681b
             7.3241114616f-02, # 0x3d95ff70
             3.3442313671f+00, # 0x405607e3
             4.2621845245f+01, # 0x422a7cc5
             1.7080809021f+02, # 0x432acedf
             1.6673394775f+02, # 0x4326bbe4
             )::NTuple{6,Float32}
const qS3 = (4.8758872986f+01, # 0x42430916
             7.0968920898f+02, # 0x44316c1c
             3.7041481934f+03, # 0x4567825f
             6.4604252930f+03, # 0x45c9e367
             2.5163337402f+03, # 0x451d4557
             -1.4924745178f+02, # 0xc3153f59
             )::NTuple{6,Float32}
# for x in [2.8570, 2] = 1 / [0.3499, 0.5]
const qR2 = (1.5044444979f-07, # 0x342189db
             7.3223426938f-02, # 0x3d95f62a
             1.9981917143f+00, # 0x3fffc4bf
             1.4495602608f+01, # 0x4167edfd
             3.1666231155f+01, # 0x41fd5471
             1.6252708435f+01, # 0x4182058c
             )::NTuple{6,Float32}
const qS2 = (3.0365585327f+01, # 0x41f2ecb8
             2.6934811401f+02, # 0x4386ac8f
             8.4478375244f+02, # 0x44533229
             8.8293585205f+02, # 0x445cbbe5
             2.1266638184f+02, # 0x4354aa98
             -5.3109550476f+00, # 0xc0a9f358
             )::NTuple{6,Float32}

# Note: This function is only called for ix >= 0x40000000 (see below)
function qzerof(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    z = 1f0 / (x * x)
    # @assert ix >= 0x40000000 && ix <= 0x48000000
    if ix >= 0x41000000
        p = qR8
        q = qS8
    elseif ix >= 0x409173eb
        p = qR5
        q = qS5
    elseif ix >= 0x4036d917
        p = qR3
        q = qS3
    else
        p = qR2
        q = qS2
    end
    r = @evalpoly(z, p[1], p[2], p[3], p[4], p[5], p[6])
    s = @evalpoly(z, 1f0, q[1], q[2], q[3], q[4], q[5], q[6])
    (-0.125f0 + r / s) / x
end

const huge = 1f30
const invsqrtpi = 5.6418961287f-01 # 0x3f106ebb
const tpi = 6.3661974669f01 # 0x3f22f983
# R0 / S0 on [0, 2.00]
const R02 =  1.5625000000f-02 # 0x3c800000
const R03 = -1.8997929874f-04 # 0xb947352e
const R04 =  1.8295404516f-06 # 0x35f58e88
const R05 = -4.6183270541f-09 # 0xb19eaf3c
const S01 =  1.5619102865f-02 # 0x3c7fe744
const S02 =  1.1692678527f-04 # 0x38f53697
const S03 =  5.1354652442f-07 # 0x3509daa6
const S04 =  1.1661400734f-09 # 0x30a045e8

function ieee754_j0f(x::Float32)
    x = abs(x)
    ix::UInt32 = reinterpret(UInt32, x)
    ix >= 0x7f800000 && return 1f0 / (x * x)
    if ix >= 0x40000000 # |x| >= 2.0
        s = sin_unchecked(x)
        c = cos_unchecked(x)
        ss = s - c
        cc = s + c
        if ix < 0x7f000000 # make sure x + x not overflow
            z = -cos_unchecked(x + x)
            if s * c < 0f0
                cc = z / ss
            else
                ss = z / cc
            end
        end
        # j0(x) = 1 / √π * (P(0, x) * cc - Q(0, x) * ss) / sqrt(x)
        # y0(x) = 1 / √π * (P(0, x) * ss + Q(0, x) * cc) / sqrt(x)
        if ix > 0x58000000 # |x| > 2^49
            z = invsqrtpi * cc / sqrt(x)
        else
            u = pzerof(x)
            v = qzerof(x)
            z = invsqrtpi * (u * cc - v * ss) / sqrt(x)
        end
        return z
    end
    z = x * x
    if ix < 0x3b000000 # |x| < 2^-9
        if huge + x > 1f0 # raise inexact if x != 0
            if ix < 0x39800000 # |x| < 2^-12
                return 1f0
            else
                return muladd(z, -0.25f0, 1f0)
            end
        end
    end
    r = z * @evalpoly(z, R02, R03, R04, R05)
    s = @evalpoly(z, 1f0, S01, S02, S03, S04)
    r /= s
    if ix < 0x3F800000 # |x| < 1.00
        muladd(z, -0.25f0 + r, 1f0)
    else
        u = 0.5f0 * x
        muladd(1f0 + u, 1f0 - u, z * r)
    end
end

const u00 = -7.3804296553f-02 # 0xbd9726b5
const u01 =  1.7666645348f-01 # 0x3e34e80d
const u02 = -1.3818567619f-02 # 0xbc626746
const u03 =  3.4745343146f-04 # 0x39b62a69
const u04 = -3.8140706238f-06 # 0xb67ff53c
const u05 =  1.9559013964f-08 # 0x32a802ba
const u06 = -3.9820518410f-11 # 0xae2f21eb
const v01 =  1.2730483897f-02 # 0x3c509385
const v02 =  7.6006865129f-05 # 0x389f65e0
const v03 =  2.5915085189f-07 # 0x348b216c
const v04 =  4.4111031494f-10 # 0x2ff280c2

function ieee754_y0f(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    # y0(NaN) is NaN, y0(-Inf) is NaN, y0(Inf) is 0
    ix >= 0x7f800000 && return 1f0 / (x + x * x)
    ix == 0 && return -Inf32
    # use (x < 0) so that llvm can infer that x >= 0 afterwards
    # somehow llvm cannot remove the bounds check on sqrt with `x <= 0f0`....
    x < 0 && return NaN32
    if ix >= 0x40000000 # |x| >= 2.0
        # y0(x) = sqrt(2 / (π * x)) * (p0(x) * sin(x0) + q0(x) * cos(x0))
        # where x0 = x - π / 4
        #      Better formula:
        #              cos(x0) = cos(x)cos(π / 4) + sin(x)sin(π / 4)
        #                      =  1/√2 * (sin(x) + cos(x))
        #              sin(x0) = sin(x)cos(3π / 4) - cos(x)sin(3π / 4)
        #                      =  1/√2 * (sin(x) - cos(x))
        # To avoid cancellation, use
        #              sin(x) ± cos(x) = -cos(2x) / (sin(x) ∓ cos(x))
        # to compute the worse one.
        s = sin_unchecked(x)
        c = cos_unchecked(x)
        ss = s - c
        cc = s + c
        # j0(x) = 1 / √π * (P(0, x) * cc - Q(0, x) * ss) / sqrt(x)
        # y0(x) = 1 / √π * (P(0, x) * ss + Q(0, x) * cc) / sqrt(x)
        if ix < 0x7f000000 # make sure x + x not overflow
            z = -cos_unchecked(x + x)
            if s * c < 0f0
                cc = z / ss
            else
                ss = z / cc
            end
        end
        if ix > 0x58000000 # |x| > 2^49
            z = (invsqrtpi * ss) / sqrt(x)
        else
            u = pzerof(x)
            v = qzerof(x)
            z = invsqrtpi * (u * ss + v * cc) / sqrt(x)
        end
        return z
    end
    if ix <= 0x39000000 # x < 2^-13
        return muladd(tpi, log_unchecked(x), u00)
    end
    z = x * x
    u = @evalpoly(z, u00, u01, u02, u03, u04, u05, u06)
    v = @evalpoly(z, 1f0, v01, v02, v03, v04)
    muladd(tpi, ieee754_j0f(x) * log_unchecked(x), u / v)
end
end
