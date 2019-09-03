# This file is a part of Julia, but is derived from
# https://github.com/google/double-conversion which has the following license
#
# Copyright 2006-2014, the V8 project authors. All rights reserved.
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#     * Neither the name of Google Inc. nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

const kMinExp = -60
const kMaxExp = -32

function roundweed(buffer,len,rest,tk,unit,kappa,too_high::UInt64,unsafe_interval::UInt64)
    small = too_high - unit
    big = too_high + unit
    while rest < small &&
            unsafe_interval - rest >= tk &&
            (rest + tk < small ||
            small - rest >= rest + tk - small)
        buffer[len-1] -= 1
        rest += tk
    end
    if rest < big &&
        unsafe_interval - rest >= tk &&
        (rest + tk < big ||
        big - rest > rest + tk - big)
        return false, kappa
    end
    return (2 * unit <= rest) && (rest <= unsafe_interval - 4 * unit), kappa
end

const SmallPowersOfTen = [
        0, 1, 10, 100, 1000, 10000, 100000,
        1000000, 10000000, 100000000, 1000000000]

function bigpowten(n,n_bits)
    guess = ((n_bits + 1) * 1233) >> 12
    guess += 1
    i = SmallPowersOfTen[guess+1]
    return n < i ? (SmallPowersOfTen[guess], guess-1) : (i,guess)
end

function digitgen(low,w,high,buffer)
    unit::UInt64 = 1
    one = Float(unit << -w.e, w.e)
    too_high = Float(high.s+unit,high.e)
    unsafe_interval = too_high - Float(low.s-unit,low.e)
    integrals = too_high.s >> -one.e
    fractionals = too_high.s & (one.s-1)
    divisor, kappa = bigpowten(integrals, 64 + one.e)
    len = 1
    rest = UInt64(0)
    while kappa > 0
        digit = div(integrals,divisor)
        buffer[len] = 0x30 + digit
        len += 1
        integrals %= divisor
        kappa -= 1
        rest = (UInt64(integrals) << -one.e) + fractionals
        if rest < unsafe_interval.s
            r, kappa = roundweed(buffer, len, rest, UInt64(divisor) << -one.e,
                        unit,kappa,(too_high - w).s,unsafe_interval.s)
            return r, kappa, len
        end
        divisor = div(divisor,10)
    end
    while true
        fractionals *= 10
        unit *= 10
        unsafe_interval = Float(unsafe_interval.s*10,unsafe_interval.e)
        digit = fractionals >> -one.e
        buffer[len] = 0x30 + digit
        len += 1
        fractionals &= one.s - 1
        kappa -= 1
        if fractionals < unsafe_interval.s
            r, kappa = roundweed(buffer,len,fractionals,one.s,
                        unit,kappa,(too_high - w).s*unit,unsafe_interval.s)
            return r, kappa, len
        end
    end
end

function fastshortest(v, buffer = Vector{UInt8}(undef, 17))
    f = normalize(Float64(v))
    bound_minus, bound_plus = normalizedbound(v)
    ten_mk_min_exp = kMinExp - (f.e + FloatSignificandSize)
    ten_mk_max_exp = kMaxExp - (f.e + FloatSignificandSize)
    cp = binexp_cache(ten_mk_min_exp,ten_mk_max_exp)
    scaled_w = f * cp
    scaled_bound_minus = bound_minus * cp
    scaled_bound_plus = bound_plus * cp
    r, kappa, len = digitgen(scaled_bound_minus,scaled_w,
                             scaled_bound_plus,buffer)
    decimal_exponent = -cp.de + kappa
    return r, len, decimal_exponent+len-1
end
