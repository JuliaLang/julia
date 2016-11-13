#
#                     The LLVM Compiler Infrastructure
#
# This file is dual licensed under the MIT and the University of Illinois Open
# Source Licenses. See LICENSE.TXT for details.
#
#
# This file implements a fairly generic conversion from a narrower to a wider
# IEEE-754 floating-point type.  The constants and types defined following the
# includes below parameterize the conversion.
#
# It does not support types that don't use the usual IEEE-754 interchange
# formats; specifically, some work would be needed to adapt it to
# (for example) the Intel 80-bit format or PowerPC double-double format.
#
# Note please, however, that this implementation is only intended to support
# *widening* operations; if you need to convert to a *narrower* floating-point
# type (e.g. double -> float), then this routine will not do what you want it
# to.
#
# It also requires that integer types at least as large as both formats
# are available on the target platform; this may pose a problem when trying
# to add support for quad on some 32-bit systems, for example.  You also may
# run into trouble finding an appropriate CLZ function for wide source types;
# you will likely need to roll your own on some platforms.
#
# Finally, the following assumptions are made:
#
# 1. floating-point types and integer types have the same endianness on the
#    target platform
#
# 2. quiet NaNs, if supported, are indicated by the leading bit of the
#    significand field being set

@inline function extendXfYf2{dst_t<:RTLIB_FLOAT, src_t<:RTLIB_FLOAT}(::Type{dst_t}, a::src_t)
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
    const srcInfinity = srcInfExp << srcSigBits
    const srcSignMask = one(src_rep_t) << (srcSigBits + srcExpBits)
    const srcAbsMask = srcSignMask - one(src_rep_t)
    const srcQNaN = one(src_rep_t) << (srcSigBits - 1)
    const srcNaNCode = srcQNaN - one(src_rep_t)

    const dstBits = nbits(dst_t)
    const dstExpBits = exponent_bits(dst_t)
    const dstInfExp = exponent_inf(dst_t)
    const dstExpBias = exponent_bias(dst_t)

    const dstMinNormal = one(dst_rep_t) << dstSigBits

    # Break a into a sign and representation of the absolute value
    const aRep = reinterpret(src_rep_t, a)
    const aAbs = aRep & srcAbsMask
    const sign = aRep & srcSignMask

    local absResult :: dst_rep_t

    if (aAbs - srcMinNormal) < (srcInfinity - srcMinNormal)
        # a is a normal number.
        # Extend to the destination type by shifting the significand and
        # exponent into the proper position and rebiasing the exponent.
        absResult = (aAbs % dst_rep_t) << (dstSigBits - srcSigBits)
        absResult += (dstExpBias - srcExpBias) << dstSigBits
    elseif aAbs >= srcInfinity
        # a is NaN or infinity.
        # Conjure the result by beginning with infinity, then setting the qNaN
        # bit (if needed) and right-aligning the rest of the trailing NaN
        # payload field.
        absResult = dstInfExp << dstSigBits
        absResult |= (aAbs & srcQNaN) % dst_rep_t << (dstSigBits - srcSigBits)
        absResult |= (aAbs & srcNaNCode) % dst_rep_t << (dstSigBits - srcSigBits)
    elseif aAbs != zero(src_rep_t) # in c if (aAbs)
        # a is denormal.
        # renormalize the significand and clear the leading bit, then insert
        # the correct adjusted exponent in the destination type.
        const scale = leading_zeros(aAbs) - leading_zeros(srcMinNormal)
        absResult = aAbs % dst_rep_t << (dstSigBits - srcSigBits + scale)
        absResult $= dstMinNormal
        const resultExponent = (dstExpBias - srcExpBias - scale + 1) % dst_rep_t
        absResult |= resultExponent << dstSigBits
    else
        # a is zero.
        absResult = zero(dst_rep_t)
    end

    # Apply the signbit to (dst_t)abs(a).
    const result = absResult | sign % dst_rep_t << (dstBits - srcBits)
    return reinterpret(dst_t, result)
end
