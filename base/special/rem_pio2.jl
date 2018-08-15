# This file is a part of Julia. Except for the rem_pio2_kernel, and
# cody_waite_* methods (see below) license is MIT: https://julialang.org/license

# rem_pio2_kernel and cody_waite_* methods are heavily based on FDLIBM code:
# __ieee754_rem_pio2, that is made available under the following licence:

## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
##
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this
## software is freely granted, provided that this notice
## is preserved.

# Bits of 1/2π
#   1/2π == sum(x / 0x1p64^i for i,x = enumerate(INV_2PI))
# Can be obtained by:
#
#    setprecision(BigFloat, 4096)
#    I = 0.5/big(pi)
#    for i = 1:19
#        I *= 0x1p64
#        k = trunc(UInt64, I)
#        @printf "0x%016x,\n" k
#        I -= k
#    end
const INV_2PI = UInt64[
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

@inline function cody_waite_2c_pio2(x::Float64, fn, n)
    pio2_1 = 1.57079632673412561417e+00
    pio2_1t = 6.07710050650619224932e-11

    z = muladd(-fn, pio2_1, x) # x - fn*pio2_1
    y1 = muladd(-fn, pio2_1t, z) # z - fn*pio2_1t
    y2 = muladd(-fn, pio2_1t, (z - y1)) # (z - y1) - fn*pio2_1t
    n, DoubleFloat64(y1, y2)
end

@inline function cody_waite_ext_pio2(x::Float64, xhp)
    pio2_1 = 1.57079632673412561417e+00
    pio2_1t = 6.07710050650619224932e-11
    pio2_2 = 6.07710050630396597660e-11
    pio2_2t = 2.02226624879595063154e-21
    pio2_3 = 2.02226624871116645580e-21
    pio2_3t = 8.47842766036889956997e-32

    fn = round(x*(2/pi)) # round to integer
    # on older systems, the above could be faster with
    # rf = 1.5/eps(Float64)
    # fn = (x*(2/pi)+rf)-rf

    r  = muladd(-fn, pio2_1, x) # x - fn*pio2_1
    w  = fn*pio2_1t # 1st round good to 85 bit
    j  = xhp>>20
    y1 = r-w
    high = highword(y1)
    i = j-((high>>20)&0x7ff)
    if i>16  # 2nd iteration needed, good to 118
        t  = r
        w  = fn*pio2_2
        r  = t-w
        w  = muladd(fn, pio2_2t,-((t-r)-w))
        y1 = r-w
        high = highword(y1)
        i = j-((high>>20)&0x7ff)
        if i>49 # 3rd iteration need, 151 bits acc
            t  = r # will cover all possible cases
            w  = fn*pio2_3
            r  = t-w
            w  = muladd(fn, pio2_3t, -((t-r)-w))
            y1 = r-w
        end
    end
    y2 = (r-y1)-w
    return unsafe_trunc(Int, fn), DoubleFloat64(y1, y2)
end

"""
    fromfraction(f::Int128)

Compute a tuple of values `(z1,z2)` such that
    ``z1 + z2 == f / 2^128``
and the significand of `z1` has 27 trailing zeros.
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

    # Computations are integer based, so reinterpret x as UInt64
    u = reinterpret(UInt64, x)
    # Strip x of exponent bits and replace with ^1
    X = (u & significand_mask(Float64)) | (one(UInt64) << significand_bits(Float64))
    # Get k from formula above
    # k = exponent(x)-52
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

    # This is equivalent to
    #     idx, shift = divrem(k, 64)
    # but divrem is slower.
    idx = k >> 6

    shift = k - (idx << 6)
    if shift == 0
        @inbounds a1 = INV_2PI[idx+1]
        @inbounds a2 = INV_2PI[idx+2]
        @inbounds a3 = INV_2PI[idx+3]
    else
        # use shifts to extract the relevant 64 bit window
        @inbounds a1 = (idx < 0 ? zero(UInt64) : INV_2PI[idx+1] << shift) | (INV_2PI[idx+2] >> (64 - shift))
        @inbounds a2 = (INV_2PI[idx+2] << shift) | (INV_2PI[idx+3] >> (64 - shift))
        @inbounds a3 = (INV_2PI[idx+3] << shift) | (INV_2PI[idx+4] >> (64 - shift))
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
    pio2 = 1.5707963267948966
    pio2_hi = 1.5707963407039642
    pio2_lo = -1.3909067614167116e-8
    y_hi = (z_hi+z_lo)*pio2
    y_lo = (((z_hi*pio2_hi - y_hi) + z_hi*pio2_lo) + z_lo*pio2_hi) + z_lo*pio2_lo
    return q, DoubleFloat64(y_hi, y_lo)
end

"""
    rem_pio2_kernel(x)

Return the remainder of `x` modulo π/2 as a double-double pair, along with a `k`
such that ``k \\mod 3 == K \\mod 3`` where ``K*π/2 = x - rem``. Note, that it is
only meant for use when ``|x|>=π/4``, and that ``π/2`` is always subtracted or
added for ``π/4<|x|<=π/2`` instead of simply returning `x`.
"""
@inline function rem_pio2_kernel(x::Float64)
    xhp = poshighword(x)
    #  xhp <= highword(5pi/4) implies |x| ~<= 5pi/4,
    if xhp <= 0x400f6a7a
        #  last five bits of xhp == last five bits of highword(pi/2) or
        #  highword(2pi/2) implies |x| ~= pi/2 or 2pi/2,
        if (xhp & 0xfffff) == 0x921fb # use precise Cody Waite scheme
            return cody_waite_ext_pio2(x, xhp)
        end
        # use Cody Waite with two constants
        #  xhp <= highword(3pi/4) implies |x| ~<= 3pi/4
        if xhp <= 0x4002d97c
            if x > 0.0
                return cody_waite_2c_pio2(x, 1.0, 1)
            else
                return cody_waite_2c_pio2(x, -1.0, -1)
            end
        # 3pi/4 < |x| <= 5pi/4
        else
            if x > 0.0
                return cody_waite_2c_pio2(x, 2.0, 2)
            else
                return cody_waite_2c_pio2(x, -2.0, -2)
            end
        end
    end
    #  xhp <= highword(9pi/4) implies |x| ~<= 9pi/4
    if xhp <= 0x401c463b
        #  xhp <= highword(7pi/4) implies |x| ~<= 7pi/4
        if xhp <= 0x4015fdbc
            #  xhp == highword(3pi/2) implies |x| ~= 3pi/2
            if xhp == 0x4012d97c # use precise Cody Waite scheme
                return cody_waite_ext_pio2(x, xhp)
            end
            # use Cody Waite with two constants
            if x > 0.0
                return cody_waite_2c_pio2(x, 3.0, 3)
            else
                return cody_waite_2c_pio2(x, -3.0, -3)
            end
        # 7pi/4 < |x| =< 9pi/4
        else
            #  xhp == highword(4pi/2) implies |x| ~= 4pi/2
            if xhp == 0x401921fb # use precise Cody Waite scheme
                return cody_waite_ext_pio2(x, xhp)
            end
            # use Cody Waite with two constants
            if x > 0.0
                return cody_waite_2c_pio2(x, 4.0, 4)
            else
                return cody_waite_2c_pio2(x, -4.0, -4)
            end
        end
    end
    #  xhp < highword(2.0^20*pi/2) implies |x| ~< 2^20*pi/2
    if xhp < 0x413921fb # use precise Cody Waite scheme
        return cody_waite_ext_pio2(x, xhp)
    end
    # if |x| >= 2^20*pi/2 switch to Payne Hanek
    return paynehanek(x)
end

## Float32
@inline function rem_pio2_kernel(x::Float32)
    pio2_1 = 1.57079631090164184570e+00
    pio2_1t = 1.58932547735281966916e-08
    inv_pio2 = 6.36619772367581382433e-01
    xd = convert(Float64, x)
    absxd = abs(xd)
    # it is assumed that NaN and Infs have been checked
    if absxd <= pi*5/4
        if absxd <= pi*3/4
            if x > 0
                return 1, DoubleFloat32(xd - pi/2)
            else
                return -1, DoubleFloat32(xd + pi/2)
            end
        end
        if x > 0
            return 2, DoubleFloat32(xd - pi)
        else
            return -2, DoubleFloat32(xd + pi)
        end
    elseif absxd <= pi*9/4
        if absxd <= pi*7/4
            if x > 0
                return 3, DoubleFloat32(xd - pi*3/2)
            else
                return -3, DoubleFloat32(xd + pi*3/2)
            end
        end
        if x > 0
            return 4, DoubleFloat32(xd - pi*4/2)
        else
            return -4, DoubleFloat32(xd + pi*4/2)
        end
    end
    #/* 33+53 bit pi is good enough for medium size */
    if absxd < Float32(pi)/2*2.0f0^28 # medium size */
        # use Cody Waite reduction with two coefficients
        fn = round(xd*inv_pio2)
        r  = xd-fn*pio2_1
        w  = fn*pio2_1t
        y = r-w;
        return unsafe_trunc(Int, fn), DoubleFloat32(y)
    end
    n, y = rem_pio2_kernel(xd)
    return n, DoubleFloat32(y.hi)
end
