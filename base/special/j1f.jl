# e_j1f.c -- float version of e_j1.c.
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.

# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunPro, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

module J1F

import Base.Math: libm, @evalpoly

sin_unchecked(x::Float32) = ccall((:sinf, libm), Float32, (Float32,), x)
cos_unchecked(x::Float32) = ccall((:cosf, libm), Float32, (Float32,), x)
log_unchecked(x::Float32) = ccall((:logf, libm), Float32, (Float32,), x)

# For x >= 8, the asymptotic expansions of pone is
#     1 + 15 / 128 s^2 - 4725 / 2^15 s^4 - ..., where s = 1/x.
# We approximate pone by
#     pone(x) = 1 + R / S
# where  R = pr0 + pr1 * s^2 + pr2 * s^4 + ... + pr5 * s^10
#     S = 1 + ps0 * s^2 + ... + ps4 * s^10
# and
#     |pone(x) - 1 - R / S| <= 2^-60.06

# for x in [inf, 8] = 1 / [0, 0.125]
const pr8 = (0.0000000000f+00, # 0x00000000
             1.1718750000f-01, # 0x3df00000
             1.3239480972f+01, # 0x4153d4ea
             4.1205184937f+02, # 0x43ce06a3
             3.8747453613f+03, # 0x45722bed
             7.9144794922f+03, # 0x45f753d6
             )::NTuple{6,Float32}
const ps8 = (1.1420736694f+02, # 0x42e46a2c
             3.6509309082f+03, # 0x45642ee5
             3.6956207031f+04, # 0x47105c35
             9.7602796875f+04, # 0x47bea166
             3.0804271484f+04, # 0x46f0a88b
             )::NTuple{5,Float32}
# for x in [8, 4.5454] = 1 / [0.125, 0.22001]
const pr5 = (1.3199052094f-11, # 0x2d68333f
             1.1718749255f-01, # 0x3defffff
             6.8027510643f+00, # 0x40d9b023
             1.0830818176f+02, # 0x42d89dca
             5.1763616943f+02, # 0x440168b7
             5.2871520996f+02, # 0x44042dc6
             )::NTuple{6,Float32}
const ps5 = (5.9280597687f+01, # 0x426d1f55
             9.9140142822f+02, # 0x4477d9b1
             5.3532670898f+03, # 0x45a74a23
             7.8446904297f+03, # 0x45f52586
             1.5040468750f+03, # 0x44bc0180
             )::NTuple{5,Float32}
const pr3 = (3.0250391081f-09, # 0x314fe10d
             1.1718686670f-01, # 0x3defffab
             3.9329774380f+00, # 0x407bb5e7
             3.5119403839f+01, # 0x420c7a45
             9.1055007935f+01, # 0x42b61c2a
             4.8559066772f+01, # 0x42423c7c
             )::NTuple{6,Float32}
const ps3 = (3.4791309357f+01, # 0x420b2a4d
             3.3676245117f+02, # 0x43a86198
             1.0468714600f+03, # 0x4482dbe3
             8.9081134033f+02, # 0x445eb3ed
             1.0378793335f+02, # 0x42cf936c
             )::NTuple{5,Float32}
# for x in [2.8570, 2] = 1 / [0.3499, 0.5]
const pr2 = (1.0771083225f-07, # 0x33e74ea8
             1.1717621982f-01, # 0x3deffa16
             2.3685150146f+00, # 0x401795c0
             1.2242610931f+01, # 0x4143e1bc
             1.7693971634f+01, # 0x418d8d41
             5.0735230446f+00, # 0x40a25a4d
             )::NTuple{6,Float32}
const ps2 = (2.1436485291f+01, # 0x41ab7dec
             1.2529022980f+02, # 0x42fa9499
             2.3227647400f+02, # 0x436846c7
             1.1767937469f+02, # 0x42eb5bd7
             8.3646392822f+00, # 0x4105d590
             )::NTuple{5,Float32}

function ponef(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    z = 1f0 / (x * x)
    if ix >= 0x41000000
        p = pr8
        q = ps8
    elseif ix >= 0x409173eb
        p = pr5
        q = ps5
    elseif ix >= 0x4036d917
        p = pr3
        q = ps3
    else
        p = pr2
        q = ps2
    end
    r = @evalpoly(z, p[1], p[2], p[3], p[4], p[5], p[6])
    s = @evalpoly(z, 1f0, q[1], q[2], q[3], q[4], q[5])
    1f0 + r / s
end

# For x >= 8, the asymptotic expansions of qone is
#     3 / 8 s - 105 / 1024 s^3 - ..., where s = 1 / x.
# We approximate pone by
#     qone(x) = s * (0.375 + R / S)
# where R = qr1 * s^2 + qr2 * s^4 + ... + qr5 * s^10
#     S = 1 + qs1 * s^2 + ... + qs6 * s^12
# and
#     |qone(x) / s - 0.375 - R / S| <= 2^-61.13

# for x in [inf, 8] = 1 / [0, 0.125]
const qr8 = (0.0000000000f+00, # 0x00000000
             -1.0253906250f-01, # 0xbdd20000
             -1.6271753311f+01, # 0xc1822c8d
             -7.5960174561f+02, # 0xc43de683
             -1.1849806641f+04, # 0xc639273a
             -4.8438511719f+04, # 0xc73d3683
             )::NTuple{6,Float32}
const qs8 = (1.6139537048f+02, # 0x43216537
             7.8253862305f+03, # 0x45f48b17
             1.3387534375f+05, # 0x4802bcd6
             7.1965775000f+05, # 0x492fb29c
             6.6660125000f+05, # 0x4922be94
             -2.9449025000f+05, # 0xc88fcb48
             )::NTuple{6,Float32}
# for x in [8, 4.5454] = 1 / [0.125, 0.22001]
const qr5 = (-2.0897993405f-11, # 0xadb7d219
             -1.0253904760f-01, # 0xbdd1fffe
             -8.0564479828f+00, # 0xc100e736
             -1.8366960144f+02, # 0xc337ab6b
             -1.3731937256f+03, # 0xc4aba633
             -2.6124443359f+03, # 0xc523471c
             )::NTuple{6,Float32}
const qs5 = (8.1276550293f+01, # 0x42a28d98
             1.9917987061f+03, # 0x44f8f98f
             1.7468484375f+04, # 0x468878f8
             4.9851425781f+04, # 0x4742bb6d
             2.7948074219f+04, # 0x46da5826
             -4.7191835938f+03, # 0xc5937978
             )::NTuple{6,Float32}
const qr3 = (-5.0783124372f-09, # 0xb1ae7d4f
             -1.0253783315f-01, # 0xbdd1ff5b
             -4.6101160049f+00, # 0xc0938612
             -5.7847221375f+01, # 0xc267638e
             -2.2824453735f+02, # 0xc3643e9a
             -2.1921012878f+02, # 0xc35b35cb
             )::NTuple{6,Float32}
const qs3 = (4.7665153503f+01, # 0x423ea91e
             6.7386511230f+02, # 0x4428775e
             3.3801528320f+03, # 0x45534272
             5.5477290039f+03, # 0x45ad5dd5
             1.9031191406f+03, # 0x44ede3d0
             -1.3520118713f+02, # 0xc3073381
             )::NTuple{6,Float32}
# for x in [2.8570, 2] = 1 / [0.3499, 0.5]
const qr2 = (-1.7838172539f-07, # 0xb43f8932
             -1.0251704603f-01, # 0xbdd1f475
             -2.7522056103f+00, # 0xc0302423
             -1.9663616180f+01, # 0xc19d4f16
             -4.2325313568f+01, # 0xc2294d1f
             -2.1371921539f+01, # 0xc1aaf9b2
             )::NTuple{6,Float32}
const qs2 = (2.9533363342f+01, # 0x41ec4454
             2.5298155212f+02, # 0x437cfb47
             7.5750280762f+02, # 0x443d602e
             7.3939318848f+02, # 0x4438d92a
             1.5594900513f+02, # 0x431bf2f2
             -4.9594988823f+00, # 0xc09eb437
             )::NTuple{6,Float32}

function qonef(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    z = 1f0 / (x * x)
    if ix >= 0x41000000
        p = qr8
        q = qs8
    elseif ix >= 0x409173eb
        p = qr5
        q = qs5
    elseif ix >= 0x4036d917
        p = qr3
        q = qs3
    else
        p = qr2
        q = qs2
    end
    r = @evalpoly(z, p[1], p[2], p[3], p[4], p[5], p[6])
    s = @evalpoly(z, 1f0, q[1], q[2], q[3], q[4], q[5], q[6])
    (0.375f0 + r / s) / x
end

const huge = 1f30
const invsqrtpi = 5.6418961287f-01 # 0x3f106ebb
const tpi = 6.3661974669f-01 # 0x3f22f983
# R0 / S0 on [0, 2]
const r00 = -6.2500000000f-02 # 0xbd800000
const r01 =  1.4070566976f-03 # 0x3ab86cfd
const r02 = -1.5995563444f-05 # 0xb7862e36
const r03 =  4.9672799207f-08 # 0x335557d2
const s01 =  1.9153760746f-02 # 0x3c9ce859
const s02 =  1.8594678841f-04 # 0x3942fab6
const s03 =  1.1771846857f-06 # 0x359dffc2
const s04 =  5.0463624390f-09 # 0x31ad6446
const s05 =  1.2354227016f-11 # 0x2d59567e

function ieee754_j1f(x::Float32)
    hx = reinterpret(UInt32, x)
    ix::UInt32 = hx & 0x7fffffff
    ix >= 0x7f800000 && return 1f0 / x
    y = abs(x)
    if ix >= 0x40000000 # |x| >= 2.0
        s = sin_unchecked(y)
        c = cos_unchecked(y)
        ss = -s - c
        cc = s - c
        if ix < 0x7f000000 # make sure y + y not overflow
            z = cos_unchecked(y + y)
            if s * c > 0f0
                cc = z / ss
            else
                ss = z / cc
            end
        end
        # j1(x) = 1 / √π * (P(1, x) * cc - Q(1, x) * ss) / sqrt(x)
        # y1(x) = 1 / √π * (P(1, x) * ss + Q(1, x) * cc) / sqrt(x)
        if ix > 0x58000000
            z = (invsqrtpi * cc) / sqrt(y) # |x| > 2^49
        else
            u = ponef(y)
            v = qonef(y)
            z = invsqrtpi * (u * cc - v * ss) / sqrt(y)
        end
        return hx & 0x8000000 != 0 ? -z : z
    end
    if ix < 0x39000000 # |x| < 2^-13
        huge + x > 1f0 && return 0.5f0 * x # inexact if x != 0 necessary
    end
    z = x * x
    r = z * @evalpoly(z, r00, r01, r02, r03)
    s = @evalpoly(z, 1f0, s01, s02, s03, s04, s05)
    r *= x
    x * 0.5f0 + r / s
end

const U0 = (-1.9605709612f-01, # 0xbe48c331
            5.0443872809f-02, # 0x3d4e9e3c
            -1.9125689287f-03, # 0xbafaaf2a
            2.3525259166f-05, # 0x37c5581c
            -9.1909917899f-08, # 0xb3c56003
            )::NTuple{5,Float32}
const V0 = (1.9916731864f-02, # 0x3ca3286a
            2.0255257550f-04, # 0x3954644b
            1.3560879779f-06, # 0x35b602d4
            6.2274145840f-09, # 0x31d5f8eb
            1.6655924903f-11, # 0x2d9281cf
            )::NTuple{5,Float32}

function ieee754_y1f(x::Float32)
    ix = reinterpret(UInt32, x) & 0x7fffffff
    ix >= 0x7f800000 && return 1f0 / (x + x * x)
    ix == 0 && return -Inf32
    # use (x < 0) so that llvm can infer that x >= 0 afterwards
    # somehow llvm cannot remove the bounds check on sqrt with `x <= 0f0`....
    x < 0f0 && return NaN32
    if ix >= 0x40000000 # |x| >= 2.0
        s = sin_unchecked(x)
        c = cos_unchecked(x)
        ss = -s - c
        cc = s - c
        if ix < 0x7f000000 # make sure x + x not overflow
            z = cos_unchecked(x + x)
            if s * c > 0f0
                cc = z / ss
            else
                ss = z / cc
            end
        end
        # y1(x) = sqrt(2 / (π*x)) * (p1(x) * sin(x0) + q1(x) * cos(x0))
        # where x0 = x - 3π / 4
        #      Better formula:
        #              cos(x0) = cos(x)cos(3π / 4) + sin(x)sin(3π / 4)
        #                      = 1 / √2 * (sin(x) - cos(x))
        #              sin(x0) = sin(x)cos(3π / 4) - cos(x)sin(3π / 4)
        #                      = -1/√2 * (cos(x) + sin(x))
        # To avoid cancellation, use
        #              sin(x) ± cos(x) = -cos(2x) / (sin(x) ∓ cos(x))
        # to compute the worse one.
        if ix > 0x58000000
            z = (invsqrtpi * ss) / sqrt(x) # |x| > 2^49
        else
            u = ponef(x)
            v = qonef(x)
            z = invsqrtpi * (u * ss + v * cc) / sqrt(x)
        end
        return z;
    end
    ix <= 0x33000000 && return -tpi / x # x < 2^-25
    z = x * x
    u = @evalpoly(z, U0[1], U0[2], U0[3], U0[4], U0[5])
    v = @evalpoly(z, 1f0, V0[1], V0[2], V0[3], V0[4], V0[5])
    muladd(tpi, muladd(ieee754_j1f(x), log_unchecked(x), -1f0 / x), x * (u / v))
end
end
