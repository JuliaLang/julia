#
#                     The LLVM Compiler Infrastructure
#
# This file is dual licensed under the MIT and the University of Illinois Open
# Source Licenses. See LICENSE.TXT for details.
#
#
# This file implements a fairly generic conversion from a wider to a narrower
# IEEE-754 floating-point type in the default (round to nearest, ties to even)
# rounding mode.  The constants and types defined following the includes below
# parameterize the conversion.
#
# This routine can be trivially adapted to support conversions to
# half-precision or from quad-precision. It does not support types that don't
# use the usual IEEE-754 interchange formats; specifically, some work would be
# needed to adapt it to (for example) the Intel 80-bit format or PowerPC
# double-double format.
#
# Note please, however, that this implementation is only intended to support
# *narrowing* operations; if you need to convert to a *wider* floating-point
# type (e.g. float -> double), then this routine will not do what you want it
# to.
#
# It also requires that integer types at least as large as both formats
# are available on the target platform; this may pose a problem when trying
# to add support for quad on some 32-bit systems, for example.
#
# Finally, the following assumptions are made:
#
# 1. floating-point types and integer types have the same endianness on the
#    target platform
#
# 2. quiet NaNs, if supported, are indicated by the leading bit of the
#    significand field being set

@inline function truncXfYf2{dst_t<:RTLIB_FLOAT, src_t<:RTLIB_FLOAT}(::Type{dst_t}, a::src_t)
    # Various constants whose values follow from the type parameters.
    # Any reasonable optimizer will fold and propagate all of these.
    const src_rep_t = fptoui(src_t)
    const dst_rep_t = fptoui(dst_t)
    const srcSigBits = significand_bits(src_t)
    const dstSigBits = significand_bits(dst_t)

    const srcBits = nbits(src_t)
    const srcExpBits = exponent_bits(src_t)
    const srcInfExp = exponent_inf(src_t)
    const srcExpBias = exponent_bias(src_t)

    const srcMinNormal = one(src_rep_t) << srcSigBits
    const srcSignificandMask = srcMinNormal - one(src_rep_t)
    const srcInfinity = srcInfExp << srcSigBits
    const srcSignMask = one(src_rep_t) << (srcSigBits + srcExpBits)
    const srcAbsMask = srcSignMask - one(src_rep_t)
    const roundMask = (one(src_rep_t) << (srcSigBits - dstSigBits)) - one(src_rep_t)
    const halfway = one(src_rep_t) << (srcSigBits - dstSigBits - one(src_rep_t))
    const srcQNaN = one(src_rep_t) << (srcSigBits - one(src_rep_t))
    const srcNaNCode = srcQNaN - one(src_rep_t)

    const dstBits = nbits(dst_t)

    const dstExpBits = exponent_bits(dst_t)
    const dstInfExp = exponent_inf(dst_t)
    const dstExpBias = exponent_bias(dst_t)

    const underflowExponent = srcExpBias - dstExpBias + 1
    const overflowExponent = srcExpBias + dstInfExp - dstExpBias
    const underflow::src_rep_t = underflowExponent << srcSigBits
    const overflow::src_rep_t = overflowExponent << srcSigBits

    const dstQNaN = one(dst_rep_t) << (dstSigBits - 1)
    const dstNaNCode = dstQNaN - one(dst_rep_t)

    # Break a into a sign and representation of the absolute value
    const aRep = reinterpret(src_rep_t, a)
    const aAbs = aRep & srcAbsMask
    const sign = aRep & srcSignMask

    local absResult :: dst_rep_t

    if (aAbs - underflow) < (aAbs - overflow)
        # The exponent of a is within the range of normal numbers in the
        # destination format.  We can convert by simply right-shifting with
        # rounding and adjusting the exponent.
        absResult = (aAbs >> (srcSigBits - dstSigBits)) % dst_rep_t
        absResult -= (srcExpBias - dstExpBias) % dst_rep_t << dstSigBits

        const roundBits = aAbs & roundMask
        # Round to nearest
        if roundBits > halfway
            absResult += one(dst_rep_t)
        # Ties to even
        elseif roundBits == halfway
            absResult += absResult & one(dst_rep_t)
        end
    elseif aAbs > srcInfinity
        # a is NaN.
        # Conjure the result by beginning with infinity, setting the qNaN
        # bit and inserting the (truncated) trailing NaN field.
        absResult = dstInfExp << dstSigBits
        absResult |= dstQNaN
        absResult |= ((aAbs & srcNaNCode) >> (srcSigBits - dstSigBits)) & dstNaNCode
    elseif aAbs >= overflow
        # a overflows to infinity.
        absResult = dstInfExp << dstSigBits
    else
        # a underflows on conversion to the destination type or is an exact
        # zero.  The result may be a denormal or zero.  Extract the exponent
        # to get the shift amount for the denormalization.
        const aExp = aAbs >> srcSigBits
        const shift = srcExpBias - dstExpBias - aExp + 1

        const significand = (aRep & srcSignificandMask) | srcMinNormal

        # Right shift by the denormalization amount with sticky.
        if shift > srcSigBits
            absResult = zero(dst_rep_t)
        else
            const sticky = significand << (srcBits - shift)
            denormalizedSignificand = significand >> shift | sticky
            absResult = (denormalizedSignificand >> (srcSigBits - dstSigBits)) % dst_rep_t
            const roundBits = denormalizedSignificand & roundMask
            # Round to nearest
            if roundBits > halfway
                absResult += one(dst_rep_t)
            # Ties to even
            elseif roundBits == halfway
                absResult += absResult & one(dst_rep_t)
            end
        end
    end

    # Apply the signbit to (dst_t)abs(a).
    const result = absResult | (sign >> (srcBits - dstBits)) % dst_rep_t
    return reinterpret(dst_t, result)
end