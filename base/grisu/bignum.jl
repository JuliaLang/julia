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

function normalizedexponent(significand, exponent::Int32)
    significand = UInt64(significand)
    while (significand & HiddenBit(Float64)) == 0
        significand <<= UInt64(1)
        exponent -= Int32(1)
    end
    return exponent
end

function bignumdtoa(v,mode,requested_digits::Int,buffer,bignums)
    significand = _significand(v)
    exponent = _exponent(v)
    lower_boundary_is_closer = lowerboundaryiscloser(v)
    need_boundary_deltas = mode == SHORTEST

    is_even = (significand & 1) == 0
    normalized_exponent = normalizedexponent(significand, exponent)
    estimated_power = estimatepower(Int(normalized_exponent))

    if mode == FIXED && -estimated_power - 1 > requested_digits
        buffer[1] = 0
        len = 1
        decimal_point = -requested_digits
        return true, len, decimal_point
    end
    num, den, minus, plus = bignums[1], bignums[2], bignums[3], bignums[4]
    initialscaledstartvalues!(significand,exponent,lower_boundary_is_closer,
                              estimated_power,need_boundary_deltas,
                              num,den,minus,plus)
    decimal_point = fixupmultiply10!(estimated_power,is_even,num,den,minus,plus)
    if mode == SHORTEST
        len = generateshortestdigits!(num,den,minus,plus,is_even,buffer)
    elseif mode == FIXED
        len, decimal_point = bignumtofixed!(requested_digits,num,den,buffer,decimal_point)
    elseif mode == PRECISION
        len, decimal_point = generatecounteddigits!(requested_digits,num,den,buffer,decimal_point)
    end
    buffer[len] = 0
    return true, len, decimal_point
end

function generateshortestdigits!(num,den,minus,plus,is_even,buffer)
    minus == plus && (plus = minus)
    len = 1
    while true
        digit = Bignums.dividemodulointbignum!(num,den)
        buffer[len] = 0x30 + (digit % UInt8)
        len += 1
        in_delta_room_minus = is_even ?
            Bignums.lessequal(num,minus) : Bignums.less(num,minus)
        in_delta_room_plus = is_even ?
            Bignums.pluscompare(num,plus,den) >= 0 : Bignums.pluscompare(num,plus,den) > 0
        if !in_delta_room_minus && !in_delta_room_plus
            Bignums.times10!(num)
            Bignums.times10!(minus)
            minus != plus && Bignums.times10!(plus)
        elseif in_delta_room_minus && in_delta_room_plus
            compare = Bignums.pluscompare(num,num,den)
            if compare < 0
            elseif compare > 0
                buffer[len - 1] += 1
            else
                if (buffer[len - 1] - 0x30) % 2 == 0
                else
                    buffer[len - 1] += 1
                end
            end
            return len
        elseif in_delta_room_minus
            return len
        else
            buffer[len - 1] += 1
            return len
        end
    end
end

function generatecounteddigits!(count,num,den,buffer,decimal_point)
    for i = 1:(count-1)
        digit = Bignums.dividemodulointbignum!(num,den)
        buffer[i] = 0x30 + (digit % UInt8)
        Bignums.times10!(num)
    end
    digit = Bignums.dividemodulointbignum!(num,den)
    if Bignums.pluscompare(num,num,den) >= 0
        digit += 1
    end
    buffer[count] = 0x30 + (digit % UInt8)
    for i = count:-1:2
        buffer[i] != 0x30 + 10 && break
        buffer[i] = 0x30
        buffer[i - 1] += 1
    end
    if buffer[1] == 0x30 + 10
        buffer[1] = 0x31
        decimal_point += 1
    end
    len = count+1
    return len, decimal_point
end

function bignumtofixed!(requested_digits,num,den,buffer,decimal_point)
    if -decimal_point > requested_digits
        decimal_point = -requested_digits
        len = 1
        return len, decimal_point
    elseif -decimal_point == requested_digits
        Bignums.times10!(den)
        if Bignums.pluscompare(num,num,den) >= 0
            buffer[1] = 0x31
            len = 2
            decimal_point += 1
        else
            len = 1
        end
        return len, decimal_point
    else
        needed_digits = decimal_point + requested_digits
        len, decimal_point = generatecounteddigits!(
              needed_digits,num,den,buffer,decimal_point)
    end
    return len, decimal_point
end


const k1Log10 = 0.30102999566398114
const kSignificandSize = SignificandSize(Float64)
estimatepower(exponent::Int) = ceil(Int,(exponent + kSignificandSize - 1) * k1Log10 - 1e-10)

function init3!(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    Bignums.assignuint64!(num,UInt64(significand))
    Bignums.shiftleft!(num,exponent)
    Bignums.assignpoweruint16!(den,UInt16(10),estimated_power)
    if need_boundary_deltas
        Bignums.shiftleft!(den,1)
        Bignums.shiftleft!(num,1)
        Bignums.assignuint16!(plus,UInt16(1))
        Bignums.shiftleft!(plus,exponent)
        Bignums.assignuint16!(minus,UInt16(1))
        Bignums.shiftleft!(minus,exponent)
    else
        Bignums.zero!(plus)
        Bignums.zero!(minus)
    end
    return
end


function init1!(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    Bignums.assignuint64!(num,UInt64(significand))
    Bignums.assignpoweruint16!(den,UInt16(10),estimated_power)
    Bignums.shiftleft!(den,-exponent)
    if need_boundary_deltas
        Bignums.shiftleft!(den,1)
        Bignums.shiftleft!(num,1)
        Bignums.assignuint16!(plus,UInt16(1))
        Bignums.assignuint16!(minus,UInt16(1))
    else
        Bignums.zero!(plus)
        Bignums.zero!(minus)
    end
    return
end

function init2!(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    power_ten = num
    Bignums.assignpoweruint16!(power_ten,UInt16(10),-estimated_power)
    if need_boundary_deltas
        Bignums.assignbignum!(plus,power_ten)
        Bignums.assignbignum!(minus,power_ten)
    else
        Bignums.zero!(plus)
        Bignums.zero!(minus)
    end
    Bignums.multiplybyuint64!(num,UInt64(significand))
    Bignums.assignuint16!(den,UInt16(1))
    Bignums.shiftleft!(den,-exponent)
    if need_boundary_deltas
        Bignums.shiftleft!(num,1)
        Bignums.shiftleft!(den,1)
    end
    return
end

function initialscaledstartvalues!(significand,
            exponent,lower_boundary_is_closer,estimated_power,
            need_boundary_deltas,num,den,minus,plus)
    if exponent >= 0
        init3!(significand, exponent, estimated_power, need_boundary_deltas,num,den,minus,plus)
    elseif estimated_power >= 0
        init1!(significand, exponent, estimated_power, need_boundary_deltas,num,den,minus,plus)
    else
        init2!(significand, exponent, estimated_power, need_boundary_deltas,num,den,minus,plus)
    end
    if need_boundary_deltas && lower_boundary_is_closer
        Bignums.shiftleft!(den,1)
        Bignums.shiftleft!(num,1)
        Bignums.shiftleft!(plus,1)
    end
    return
end

function fixupmultiply10!(estimated_power,is_even,num,den,minus,plus)
    in_range = is_even ? Bignums.pluscompare(num,plus,den) >= 0 :
                         Bignums.pluscompare(num,plus,den) > 0
    if in_range
        decimal_point = estimated_power + 1
    else
        decimal_point = estimated_power
        Bignums.times10!(num)
        if minus == plus
            Bignums.times10!(minus)
            Bignums.assignbignum!(plus,minus)
        else
            Bignums.times10!(minus)
            Bignums.times10!(plus)
        end
    end
    return decimal_point
end
