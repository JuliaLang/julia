#
#                     The LLVM Compiler Infrastructure
#
# This file is dual licensed under the MIT and the University of Illinois Open
# Source Licenses. See LICENSE.TXT for details.
#
#
# This file implements float to integer conversion for the
# compiler-rt library.
#

@inline function fixint{fixint_t, fp_t<:RTLIB_FLOAT}(::Type{fixint_t}, a::fp_t)
    const rep_t = fptoui(fp_t)

    # Get masks
    const signBit = one(src_rep_t) << (significand_bits(fp_t) + exponent_bits(fp_t))
    const absMask = signBit - one(src_rep_t)
    # Break a into sign, exponent, significand
    const aRep = reinterpret(rep_t, a)
    const aAbs = aRep & absMask
    const sign = ifelse(aRep & signBit != 0, -one(fixint_t), one(fixint_t))
    const exponent :: rep_t = (aAbs >> significand_bits(fp_t)) - exponent_bias(fp_t)
    const significand = (aAbs & significandMask) | implicitBit

    # If exponent is negative, the result is zero.
    if exponent < 0
        return zero(fixint_t)
    end

    # If the value is too large for the integer type, saturate.
    if exponent >= nbits(fixint_t)
        return ifelse(sign == 1, typemax(fixint_t), typemin(fixint_t))
    end

    # If 0 <= exponent < significandBits, right shift to get the result.
    # Otherwise, shift left.
    if exponent < significand_bits(fp_t)
        return sign * (significand >> (significand_bits(fp_t) - exponent))
    else
        return sign * ((significand % fixint_t) << (exponent - significandBits))
    end
end
