# This file is a part of Julia. License is MIT: https://julialang.org/license

# This code is a Julia translation of the C code from Openlibm (http://www.openlibm.org/)
# with the following license:

# Copyright (c) 2011 David Schultz <das@FreeBSD.ORG>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

modify_highword(x::Float32, hw) = reinterpret(Float32, hw)
modify_highword(x::Float64, hw) = reinterpret(Float64, (UInt64(hw)<<32)|(reinterpret(UInt64, x)<<32)>>32)

exponent_rshift(T::Type{Float32}, hw) = hw >> 23 # this comes from 32 (bits in UInt32) minus 9 bits for the sign and exponent
exponent_rshift(T::Type{Float64}, hw) = hw >> 20 # this comes from 32 (bits in UInt32) minus 12 bits for the sign and exponent
exponent_lshift(T::Type{Float32}, hw) = hw << 23 # this comes from 32 (bits in UInt32) minus 9 bits for the sign and exponent
exponent_lshift(T::Type{Float64}, hw) = hw << 20 # this comes from 32 (bits in UInt32) minus 12 bits for the sign and exponent

function modify_exponent(x::T, expnt_x) where T <: Union{Float32, Float64}
    # mask away exponent; "100...0111..111" with 9 or 12 leading 0's
    high_mask = T == Float32 ? 0x807fffff : 0x800fffff # don't mask away the sign
    # use mask to replace with first 9 or 12 bits with expnt_x << appropriately
    modify_highword(x, (highword(x) & high_mask) | exponent_lshift(T, expnt_x))
end

"""
    _ldexp_exp(x, l2)
Returns exp(x) * 2^l2. The function is intended for large arguments, x, where
x >= ln(prevfloat(typemax(x)) and care is needed to avoid overflow.

The present implementation is narrowly tailored for our hyperbolic and
exponential functions.  We assume l2 is small (0 or -1), and the caller
has filtered out very large x, for which overflow would be inevitable.
"""
function _ldexp_exp(x::T, l2) where T <: Union{Float32, Float64}
    # This function is intended for use in our hyperbolic and exponential functions.

    # Calculate exp(x) = (exp(x-kr*log(2))*2^ks*)2^k2 = exp_x*2^k2
    exp_x, k2 = _frexp_exp(x)

    # Add the two exponents together to form (2^l2)*(2^k2) = 2^(l2+k2) = 2^L
    l2 += k2
    L_as_hw = exponent_lshift(T, UInt32(exponent_bias(T) + l2))
    # Form 2^L
    scale = fromhighword(T, L_as_hw)
    # Return exp(x)*2^l2
    return exp_x * scale
end

"""
    exp_x, k2 = _frexp_exp(x)

Calculate exp(x) as exp_x*2^k2 and return exp_x = exp(x-kr*log(w))*2^ks where kr
is a type dependant range reduction constant, ks scales exp_x towards the largest
finite number, and k2 is used to absorb the remaning scale to allow for exp(x)
to be outside the normal floating point range.

This function is intended for use in our hyperbolic and exponential functions.
"""
function _frexp_exp(x::T) where T<:Union{Float32, Float64}
    # and should only be used for values in the range (let T = typeof(x)):
    #
    #     log(prevfloat(typemax(x))) <= x < log(2 * prevfloat(typemax(x) / nextfloat(T(0)))
    #
    # where the upper bound is around 192.7f0 and ~= 1454.91. The function outputs
    # exp_x in the ranges
    #     [2f0^127, 2f0^128) and
    #     [2.0^1023, 2.0^1024)
    # respectively.

    # We use exp(x) = exp(x - kln2) * 2**k, carefully chosen to
    # minimize |exp(kln2) - 2**k|.
    kr = T == Float32 ? UInt32(235) : UInt32(1799)

    # We also scale the exponent of exp_x to exponent_bias + the largest finite
    # exponent (exponent of T(Inf)-1, so that the result can be multiplied by
    # a tiny number without losing accuracy due to denormalization.
    exp_x = exp(x - kr*log(T(2))) # exp_x*2^k = exp(x)

    # Calculate the ks in exp_x*2^ks
    ks = exponent_rshift(T, highword(exp_x)) - (exponent_bias(T) + (exponent_max(T) - 1)) + kr

    # Rescale exp_x to have exponent k2 = exponent_max(T) - 1
    exp_x = modify_exponent(exp_x, UInt32(exponent_bias(T) + (exponent_max(T) - 1)))
    return exp_x, ks
end
