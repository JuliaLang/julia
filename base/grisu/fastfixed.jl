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

const kDoubleSignificandSize = 53

function filldigits32fixedlength(n1,requested_len,buffer,len)
    for i = (requested_len-1):-1:0
        buffer[len+i] = 0x30 + n1 % 10
        n1 = div(n1,10)
    end
    return len + requested_len
end

function filldigits32(n,buffer,len)
    n_len = 0
    while n != 0
        digit = n % 10
        n = div(n,10)
        buffer[len+n_len] = 0x30 + digit
        n_len += 1
    end
    i,j = len, len + n_len - 1
    while i < j
        buffer[i], buffer[j] = buffer[j], buffer[i]
        i += 1
        j -= 1
    end
    return len + n_len
end

function filldigits64fixedlength(n2,buffer,len)
    kTen7 = 10000000
    part2 = n2 % kTen7
    n2 = div(n2,kTen7)
    part0, part1 = divrem(n2,kTen7)
    len = filldigits32fixedlength(part0, 3, buffer, len)
    len = filldigits32fixedlength(part1, 7, buffer, len)
    len = filldigits32fixedlength(part2, 7, buffer, len)
    return len
end

function filldigits64(n3,buffer,len)
    kTen7 = 10000000
    part2 = n3 % kTen7
    n3 = div(n3,kTen7)
    part0, part1 = divrem(n3,kTen7)
    if part0 != 0
        len = filldigits32(part0, buffer, len)
        len = filldigits32fixedlength(part1, 7, buffer, len)
        len = filldigits32fixedlength(part2, 7, buffer, len)
    elseif part1 != 0
        len = filldigits32(part1, buffer, len)
        len = filldigits32fixedlength(part2, 7, buffer, len)
    else
        len = filldigits32(part2, buffer, len)
    end
    return len
end

function roundup(buffer, len, decimal_point)
    if len == 1
        buffer[1] = 0x31
        decimal_point = 1
        len = 2
        return len, decimal_point
    end
    buffer[len - 1] += 1
    for i = (len-1):-1:2
        buffer[i] != 0x30 + 10 && return len, decimal_point
        buffer[i] = 0x30
        buffer[i - 1] += 1
    end
    if buffer[1] == 0x30 + 10
        buffer[1] = 0x31
        decimal_point += 1
    end
    return len, decimal_point
end

function fillfractionals(fractionals, exponent,
                         fractional_count, buffer,
                         len, decimal_point)
    if -exponent <= 64
        point = -exponent
        for i = 1:fractional_count
            fractionals == 0 && break
            fractionals *= 5
            point -= 1
            digit = fractionals >> point
            buffer[len] = 0x30 + digit
            len += 1
            fractionals -= UInt64(digit) << point
        end
        if ((fractionals >> (point - 1)) & 1) == 1
            len, decimal_point = roundup(buffer, len, decimal_point)
        end
    else
        fract128 = UInt128(fractionals) << 64
        fract128 = shift(fract128,-exponent - 64)
        point = 128
        for i = 1:fractional_count
            fract128 == 0 && break
            fract128 *= 5
            point -= 1
            digit, fract128 = divrem2(fract128,point)
            buffer[len] = 0x30 + digit
            len += 1
        end
        if bitat(fract128,point - 1) == 1
            len, decimal_point = roundup(buffer, len, decimal_point)
        end
    end
    return len, decimal_point
end

low(x) = UInt64(x&0xffffffffffffffff)
high(x) = UInt64(x >>> 64)
bitat(x::UInt128,y) = y >= 64 ? (Int32(high(x) >> (y-64)) & 1) : (Int32(low(x) >> y) & 1)
function divrem2(x,power)
    h = high(x)
    l = low(x)
    if power >= 64
        result = Int32(h >> (power - 64))
        h -= UInt64(result) << (power - 64)
        return result, (UInt128(h) << 64) + l
    else
        part_low::UInt64 = l >> power
        part_high::UInt64 = h << (64 - power)
        result = Int32(part_low + part_high)
        return result, UInt128(l - (part_low << power))
    end
end
function shift(x::UInt128,amt)
    if amt == 0
      return x
    elseif amt == -64
        return x << 64
    elseif amt == 64
        return x >> 64
    elseif amt <= 0
        h = high(x); l = low(x)
        h <<= -amt
        h += l >> (64 + amt)
        l <<= -amt
        return (UInt128(h) << 64) + l
    else
        h = high(x); l = low(x)
        l >>= amt
        l += h << (64 - amt)
        h >>= amt
        return (UInt128(h) << 64) + l
    end
end

function trimzeros(buffer, len, decimal_point)
    while len > 1 && buffer[len - 1] == 0x30
        len -= 1
    end
    first_non_zero::Int32 = 1
    while first_non_zero < len && buffer[first_non_zero] == 0x30
        first_non_zero += 1
    end
    if first_non_zero != 1
        for i = first_non_zero:(len-1)
            buffer[i - first_non_zero + 1] = buffer[i]
        end
        len -= first_non_zero-1
        decimal_point -= first_non_zero-1
    end
    return len, decimal_point
end

function fastfixedtoa(v,mode,fractional_count,buffer)
    v = Float64(v)
    significand::UInt64 = _significand(v)
    exponent = _exponent(v)
    exponent > 20 && return false, 0, 0
    fractional_count > 20 && return false, 0, 0
    len = 1
    if exponent + kDoubleSignificandSize > 64
        kFive17 = divisor = Int64(5)^17
        divisor_power = 17
        dividend = significand
        if exponent > divisor_power
            dividend <<= exponent - divisor_power
            quotient = div(dividend,divisor)
            remainder = (dividend % divisor) << divisor_power
        else
            divisor <<= divisor_power - exponent
            quotient = div(dividend,divisor)
            remainder = (dividend % divisor) << exponent
        end
        len = filldigits32(quotient, buffer, len)
        len = filldigits64fixedlength(remainder, buffer, len)
        decimal_point = len-1
    elseif exponent >= 0
        significand <<= exponent
        len = filldigits64(significand, buffer, len)
        decimal_point = len-1
    elseif exponent > -kDoubleSignificandSize
        integrals = significand >> -exponent
        fractionals = significand - (integrals << -exponent)
        if integrals > 0xFFFFFFFF
            len = filldigits64(integrals,buffer,len)
        else
            len = filldigits32(integrals%UInt32,buffer,len)
        end
        decimal_point = len-1
        len, decimal_point = fillfractionals(fractionals,exponent,fractional_count,
                                             buffer,len, decimal_point)
    elseif exponent < -128
        len = 1
        decimal_point = -fractional_count
    else
        decimal_point = 0
        len, decimal_point = fillfractionals(significand,exponent,fractional_count,
                                             buffer,len, decimal_point)
    end
    len, decimal_point = trimzeros(buffer,len,decimal_point)
    buffer[len] = 0
    if (len-1) == 0
        decimal_point = -fractional_count
    end
    return true, len, decimal_point
end
