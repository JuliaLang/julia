# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunPro, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================
#
# Optimized by Bruce D. Evans.

# s_cbrT.c -- float version of s_cbrt.c.
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
# Debugged and optimized by Bruce D. Evans.

cbrt(x::Real) = cbrt(float(x))

const cbrt_B1_32 = 0x2a5119f2 # UInt32(709958130) # B1 = (127-127.0/3-0.03306235651)*2**23
const cbrt_B1_64 = 0x2a9f7893 # UInt32(715094163) # B1 = (1023-1023/3-0.03306235651)*2**20

const cbrt_B2_32 = 0x265119f2 # UInt32(642849266) # B2 = (127-127.0/3-24/3-0.03306235651)*2**23
const cbrt_B2_64 = 0x297f7893 # UInt32(696219795) # B2 = (1023-1023/3-54/3-0.03306235651)*2**20

cbrt_t(::Type{Float64}) = 1.8014398509481984e16# 2.0^54
cbrt_t(::Type{Float32}) = 1.6777216f7 ## 2f0^24

cbrt_t_from_words(::Type{Float32}, high)   = reinterpret(Float32, (div(high & 0x7fffffff, 0x00000003))+cbrt_B2_32)
cbrt_t_from_words_alt(::Type{Float32}, hx) = reinterpret(Float32, (div(hx, 0x00000003) + cbrt_B1_32))

cbrt_t_from_words(::Type{Float64}, high)   = reinterpret(Float64, UInt64((div(high & 0x7fffffff, 0x00000003)+cbrt_B2_64))<<32)
cbrt_t_from_words_alt(::Type{Float64}, hx) = reinterpret(Float64, UInt64((div(hx, 0x00000003) + cbrt_B1_64))<<32)

function cbrt(x::Tf) where Tf <: Union{Float32, Float64}
    # mathematically cbrt(x) is defined as the real number y such that y^3 == x

    if isnan(x) || isinf(x)
        return x
    end

    # rough cbrt to 5 bits

    #    cbrt(2**e*(1+m) ~= 2**(e/3)*(1+(e%3+m)/3)
    # where e is integral and >= 0, m is real and in [0, 1), and "/" and
    # "%" are integer division and modulus with rounding towards minus
    # infinity.  The RHS is always >= the LHS and has a maximum relative
    # error of about 1 in 16.  Adding a bias of -0.03306235651 to the
    # (e%3+m)/3 term reduces the error to about 1 in 32. With the IEEE
    # floating point representation, for finite positive normal values,
    # ordinary integer divison of the value in bits magically gives
    # almost exactly the RHS of the above provided we first subtract the
    # exponent bias and later add it back.  We do the
    # subtraction virtually to keep e >= 0 so that ordinary integer
    # division rounds towards minus infinity; this is also efficient.

    if x == zero(Tf)
        return x # cbrt(+-0) is itself
    elseif issubnormal(x) # zero or subnormal?
        t = x*cbrt_t(Tf)
        high = highword(t)
        t = copysign(cbrt_t_from_words(Tf, high), x)
    else
        hw = poshighword(x)
        t = copysign(cbrt_t_from_words_alt(Tf, hw), x)
    end

    if Tf == Float32
        # First step Newton iteration (solving t*t-x/t == 0) to 16 bits.  In
        # double precision so that its terms can be arranged for efficiency
        # without causing overflow or underflow.

        T = Float64(t)
        r = T*T*T
        T = T*(Float64(x) + x + r)/(x + r + r)

        # Second step Newton iteration to 47 bits.  In double precision for
        # efficiency and accuracy.
        r = T*T*T
        T = T*(Float64(x) + x + r)/(x + r + r)

        return Float32(T)
    elseif Tf == Float64
        # New cbrt to 23 bits:
        #    cbrt(x) = t*cbrt(x/t**3) ~= t*P(t**3/x)
        # where P(r) is a polynomial of degree 4 that approximates 1/cbrt(r)
        # to within 2**-23.5 when |r - 1| < 1/10.  The rough approximation
        # has produced t such than |t/cbrt(x) - 1| ~< 1/32, and cubing this
        # gives us bounds for r = t**3/x.

        r = (t*t)*(t/x)
        t = t*(@horner(r, 1.87595182427177009643, -1.88497979543377169875, 1.621429720105354466140) +
            ((r*r)*r)*(@horner(r, -0.758397934778766047437, 0.145996192886612446982)))

        # Round t away from zero to 23 bits (sloppily except for ensuring that
        # the result is larger in magnitude than cbrt(x) but not much more than
        # 2 23-bit ulps larger).  With rounding towards zero, the error bound
        # would be ~5/6 instead of ~4/6.  With a maximum error of 2 23-bit ulps
        # in the rounded t, the infinite-precision error in the Newton
        # approximation barely affects third digit in the final error
        # 0.667; the error in the rounded t can be up to about 3 23-bit ulps
        # before the final error is larger than 0.667 ulps.

        u = reinterpret(UInt64, t)
        u = (u + 0x80000000) & UInt64(0xffffffffc0000000)
        t = reinterpret(Float64, u)

        # one step Newton iteration to 53 bits with error < 0.667 ulps
        s = t*t             # t*t is exact
        r = x/s             # error <= 0.5 ulps; |r| < |t|
        w = t + t           # t+t is exact
        r = (r - t)/(w + r) # r-t is exact; w+r ~= 3*t
        t = muladd(t, r, t) # error <= 0.5 + 0.5/3 + epsilon
        return t
    end
end
