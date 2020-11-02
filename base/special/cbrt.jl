# This file is a part of Julia. License is MIT: https://julialang.org/license

# Float32/Float64 based on C implementations from FDLIBM (http://www.netlib.org/fdlibm/)
# and FreeBSD:
#
## ====================================================
## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
##
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this
## software is freely granted, provided that this notice
## is preserved.
## ====================================================
## Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
## Debugged and optimized by Bruce D. Evans.

"""
    cbrt(x::Real)

Return the cube root of `x`, i.e. ``x^{1/3}``. Negative values are accepted
(returning the negative real root when ``x < 0``).

The prefix operator `∛` is equivalent to `cbrt`.

# Examples
```jldoctest
julia> cbrt(big(27))
3.0

julia> cbrt(big(-27))
-3.0
```
"""
cbrt(x::Real) = cbrt(float(x))
cbrt(x::AbstractFloat) = x < 0 ? -(-x)^(1//3) : x^(1//3)

"""
    _approx_cbrt(x)

Approximate `cbrt` to 5 bits precision

    cbrt(2^e * (1+m)) ≈ 2^(e÷3) * (1 + (e%3+m)÷3)

where:
 - `e` is integral and >= 0
 - `m` is real and in [0, 1),
 - `÷` is integer division
 - `%` is integer remainder

The RHS is always >= the LHS and has a maximum relative error of about 1 in 16.
Adding a bias of -0.03306235651 to the `(e%3+m)÷3` term reduces the error to about 1 in
32.

With the IEEE floating point representation, for finite positive normal values, ordinary
integer division of the value in bits magically gives almost exactly the RHS of the above
provided we first subtract the exponent bias and later add it back.  We do the
subtraction virtually to keep e >= 0 so that ordinary integer division rounds towards
minus infinity; this is also efficient. All operations can be done in 32-bit.

These implementations assume that NaNs, infinities and zeros have already been filtered.
"""
@inline function _approx_cbrt(x::T) where {T<:Union{Float32,Float64}}
    # floor(UInt32, adj * exp2(k)) should be evaluated to 2 constants.
    adj = exponent_bias(T)*2/3 - 0.03306235651
    k = significand_bits(T) - (8*sizeof(T) - 32)

    u = highword(x) & 0x7fff_ffff
    if u >= Base.Math.highword(floatmin(T))
        v = div(u, UInt32(3)) + floor(UInt32, adj * exp2(k))
    else
        # subnormal
        x *= maxintfloat(T)
        adj -= exponent(maxintfloat(T))/3
        u = highword(x) & 0x7fff_ffff
        v = div(u, UInt32(3)) + floor(UInt32, adj * exp2(k))
    end
    return copysign(fromhighword(T, v), x)
end

@inline function _improve_cbrt(x::Float32, t::Float32)
    # Newton iterations solving
    #   t^2 - x/t == 0
    # with update
    #   t <- t*(t^3 + 2*x)/(2*t^3 + x)

    # Use double precision so that its terms can be arranged for efficiency
    # without causing overflow or underflow.
    xx = Float64(x)
    tt = Float64(t)

    # 1st step: 16 bits accuracy
    tt3 = tt^3
    tt *= (2*xx + tt3)/(x + 2*tt3)

    # 2nd step: 47 bits accuracy
    tt3 = tt^3
    tt *= (2*xx + tt3)/(x + 2*tt3)

    return Float32(tt)
end

@inline function _improve_cbrt(x::Float64, t::Float64)
    # cbrt to 23 bits:
    #
    #    cbrt(x) = t * cbrt(x / t^3) ~= t * P(t^3 / x)
    #
    # where P(r) is a polynomial of degree 4 that approximates 1/cbrt(r)
    # to within 2^-23.5 when |r - 1| < 1/10.  The rough approximation
    # has produced t such than |t/cbrt(x) - 1| ~< 1/32, and cubing this
    # gives us bounds for r = t^3/x.

    r = (t*t)*(t/x)
    t *= (@horner(r, 1.87595182427177009643, -1.88497979543377169875, 1.621429720105354466140) +
          r^3 * @horner(r, -0.758397934778766047437, 0.145996192886612446982))

    # Round t away from zero to 23 bits (sloppily except for ensuring that
    # the result is larger in magnitude than cbrt(x) but not much more than
    # 2 23-bit ulps larger).  With rounding towards zero, the error bound
    # would be ~5/6 instead of ~4/6.  With a maximum error of 2 23-bit ulps
    # in the rounded t, the infinite-precision error in the Newton
    # approximation barely affects third digit in the final error
    # 0.667; the error in the rounded t can be up to about 3 23-bit ulps
    # before the final error is larger than 0.667 ulps.

    u = reinterpret(UInt64, t)
    u = (u + 0x8000_0000) & UInt64(0xffff_ffff_c000_0000)
    t = reinterpret(Float64, u)

    # one step Newton iteration solving
    #   t^3 - x == 0
    # with update
    #   t <- t + t * (x/t^2 - t) / (3*t)

    # to 53 bits with error < 0.667 ulps
    s = t*t             # t*t is exact
    r = x/s             # error <= 0.5 ulps; |r| < |t|
    w = t+t             # t+t is exact
    r = (r - t)/(w + r) # r-t is exact; w+r ~= 3*t
    t = muladd(t, r, t) # error <= 0.5 + 0.5/3 + epsilon
    return t
end

function cbrt(x::Union{Float32,Float64})
    if !isfinite(x) || iszero(x)
        return x
    end
    t = _approx_cbrt(x)
    return _improve_cbrt(x, t)
end
