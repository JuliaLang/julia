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

function roundweed(buffer,len,rest,tk,unit,kappa)
    unit >= tk && return false, kappa
    tk - unit <= unit && return false, kappa
    tk - rest > rest && (tk - 2 * rest >= 2 * unit) && return true, kappa
    if rest > unit && (tk - (rest - unit) <= (rest - unit))
        buffer[len-1] += 1
        for i = (len-1):-1:2
            buffer[i] != 0x30 + 10 && break
            buffer[i] = 0x30
            buffer[i-1] += 1
        end
        if buffer[1] == 0x30 + 10
            buffer[1] = 0x31
            kappa += 1
        end
        return true, kappa
    end
    return false, kappa
end

function digitgen(w,buffer,requested_digits=1000)
    unit::UInt64 = 1
    one = Float(unit << -w.e, w.e)
    integrals = w.s >> -one.e
    fractionals = w.s & (one.s-1)
    divisor, kappa = bigpowten(integrals, 64 + one.e)
    len = 1
    rest = 0
    while kappa > 0
        digit = div(integrals,divisor)
        buffer[len] = 0x30 + digit
        len += 1
        requested_digits -= 1
        integrals %= divisor
        kappa -= 1
        if requested_digits == 0
            rest = (UInt64(integrals) << -one.e) + fractionals
            r, kappa = roundweed(buffer, len, rest, UInt64(divisor) << -one.e,
                    unit,kappa)
            return r, kappa, len
        end
        divisor = div(divisor,10)
    end
    while requested_digits > 0 && fractionals > unit
        fractionals *= 10
        unit *= 10
        digit = fractionals >> -one.e
        buffer[len] = 0x30 + digit
        len += 1
        requested_digits -= 1
        fractionals &= one.s - 1
        kappa -= 1
    end
    requested_digits != 0 && return false, kappa, len
    r, kappa = roundweed(buffer,len,fractionals,one.s,
                         unit,kappa)
    return r, kappa, len
end

function fastprecision(v, requested_digits, buffer = Vector{UInt8}(undef, 100))
    f = normalize(Float64(v))
    ten_mk_min_exp = kMinExp - (f.e + FloatSignificandSize)
    ten_mk_max_exp = kMaxExp - (f.e + FloatSignificandSize)
    cp = binexp_cache(ten_mk_min_exp,ten_mk_max_exp)
    scaled_w = f * cp
    r, kappa, len = digitgen(scaled_w,buffer,requested_digits)
    decimal_exponent = -cp.de + kappa
    return r, len, decimal_exponent+len-1
end
