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

module Bignums

import Base: ==, <

export Bignum

const kMaxSignificantBits = 3584

const Chunk = UInt32
const DoubleChunk = UInt64

const kChunkSize = sizeof(Chunk) * 8
const kDoubleChunkSize = sizeof(DoubleChunk) * 8
# With bigit size of 28 we loose some bits, but a double still fits easily
# into two chunks, and more importantly we can use the Comba multiplication.
const kBigitSize = 28
const kBigitMask = Chunk((1 << kBigitSize) - 1)
# Every instance allocates kBigitLength chunks on the stack. Bignums cannot
# grow. There are no checks if the stack-allocated space is sufficient.
const kBigitCapacity = div(kMaxSignificantBits, kBigitSize)

mutable struct Bignum
    bigits::Vector{UInt32}
    used_digits::Int32
    exponent::Int32
    function Bignum()
        bigits = Vector{UInt32}(undef, kBigitCapacity)
        @inbounds for i = 1:kBigitCapacity
            bigits[i] = 0
        end
        new(bigits,0,0)
    end
end

==(a::Bignum,b::Bignum) = compare(a,b) == 0
<(a::Bignum,b::Bignum) = compare(a,b) < 0

times10!(x::Bignum) = multiplybyuint32!(x,UInt32(10))

plusequal(a,b,c) = pluscompare(a,b,c) == 0
pluslessequal(a,b,c) = pluscompare(a,b,c) <= 0
plusless(a,b,c) = pluscompare(a,b,c) < 0
lessequal(a::Bignum,b::Bignum) = compare(a,b) <= 0
less(a::Bignum,b::Bignum) = compare(a,b) < 0

bigitlength(x::Bignum) = x.used_digits + x.exponent

bitsize(value) = 8 * sizeof(value)

function zero!(x::Bignum)
    for i = 1:x.used_digits
        @inbounds x.bigits[i] = 0
    end
    x.used_digits = 0
    x.exponent = 0
    return
end

function clamp!(x::Bignum)
    @inbounds while (x.used_digits > 0 && x.bigits[x.used_digits] == 0)
        x.used_digits -= 1
    end
    x.used_digits == 0 && (x.exponent = 0)
    return
end

isclamped(x::Bignum) = x.used_digits == 0 || x.bigits[x.used_digits] != 0

function align!(x::Bignum,other::Bignum)
    @inbounds if x.exponent > other.exponent
        zero_digits = x.exponent - other.exponent
        for i = x.used_digits:-1:1
            x.bigits[i + zero_digits] = x.bigits[i]
        end
        for i = 1:zero_digits
            x.bigits[i] = 0
        end
        x.used_digits += zero_digits
        x.exponent -= zero_digits
    end
    return
end

function bigitshiftleft!(x::Bignum,shift_amount)
    carry::UInt32 = 0
    @inbounds begin
    for i = 1:x.used_digits
        new_carry::Chunk = x.bigits[i] >> (kBigitSize - shift_amount)
        x.bigits[i] = ((x.bigits[i] << shift_amount) + carry) & kBigitMask
        carry = new_carry
    end
    if carry != 0
        x.bigits[x.used_digits+1] = carry
        x.used_digits += 1
    end
    end
    return
end

function subtracttimes!(x::Bignum,other::Bignum,factor)
    if factor < 3
        for i = 1:factor
            subtractbignum!(x,other)
        end
        return
    end
    borrow::Chunk = 0
    exponent_diff = other.exponent - x.exponent
    @inbounds begin
    for i = 1:other.used_digits
        product::DoubleChunk = DoubleChunk(factor) * other.bigits[i]
        remove::DoubleChunk = borrow + product
        difference::Chunk = (x.bigits[i+exponent_diff] - (remove & kBigitMask)) % Chunk
        x.bigits[i+exponent_diff] = difference & kBigitMask
        borrow = ((difference >> (kChunkSize - 1)) + (remove >> kBigitSize)) % Chunk
    end
    for i = (other.used_digits + exponent_diff + 1):x.used_digits
        borrow == 0 && return
        difference::Chunk = x.bigits[i] - borrow
        x.bigits[i] = difference & kBigitMask
        borrow = difference >> (kChunkSize - 1)
    end
    end
    clamp!(x)
end

function assignuint16!(x::Bignum,value::UInt16)
    zero!(x)
    value == 0 && return
    x.bigits[1] = value
    x.used_digits = 1
    return
end

const kUInt64Size = 64
function assignuint64!(x::Bignum,value::UInt64)
    zero!(x)
    value == 0 && return
    needed_bigits = div(kUInt64Size,kBigitSize) + 1
    @inbounds for i = 1:needed_bigits
        x.bigits[i] = value & kBigitMask
        value >>= kBigitSize
    end
    x.used_digits = needed_bigits
    clamp!(x)
end

function assignbignum!(x::Bignum,other::Bignum)
    x.exponent = other.exponent
    @inbounds begin
    for i = 1:other.used_digits
        x.bigits[i] = other.bigits[i]
    end
    for i = (other.used_digits+1):x.used_digits
        x.bigits[i] = 0
    end
    end
    x.used_digits = other.used_digits
    return
end

function adduint64!(x::Bignum,operand::UInt64)
    operand == 0 && return
    other = Bignum()
    assignuint64!(other,operand)
    addbignum!(x,other)
end

function addbignum!(x::Bignum,other::Bignum)
    align!(x,other)
    carry::Chunk = 0
    bigit_pos = other.exponent - x.exponent
    @inbounds for i = 1:other.used_digits
        sum::Chunk = x.bigits[bigit_pos+1] + other.bigits[i] + carry
        x.bigits[bigit_pos+1] = sum & kBigitMask
        carry = sum >> kBigitSize
        bigit_pos += 1
    end
    @inbounds while carry != 0
        sum = x.bigits[bigit_pos+1] + carry
        x.bigits[bigit_pos+1] = sum & kBigitMask
        carry = sum >> kBigitSize
        bigit_pos += 1
    end
    x.used_digits = max(bigit_pos,x.used_digits)
    return
end

function subtractbignum!(x::Bignum,other::Bignum)
    align!(x,other)
    offset = other.exponent - x.exponent
    borrow = Chunk(0)
    @inbounds begin
    for i = 1:other.used_digits
        difference = x.bigits[i+offset] - other.bigits[i] - borrow
        x.bigits[i+offset] = difference & kBigitMask
        borrow = difference >> (kChunkSize - 1)
    end
    i = other.used_digits+1
    while borrow != 0
        difference = x.bigits[i+offset] - borrow
        x.bigits[i+offset] = difference & kBigitMask
        borrow = difference >> (kChunkSize - 1)
        i += 1
    end
    end
    clamp!(x)
end

function shiftleft!(x::Bignum,shift_amount)
    x.used_digits == 0 && return
    x.exponent += div(shift_amount,kBigitSize)
    local_shift = shift_amount % kBigitSize
    bigitshiftleft!(x,local_shift)
end

function multiplybyuint32!(x::Bignum,factor::UInt32)
    factor == 1 && return
    if factor == 0
        zero!(x)
        return
    end
    x.used_digits == 0 && return
    carry::DoubleChunk = 0
    @inbounds begin
    for i = 1:x.used_digits
        product::DoubleChunk = (factor % DoubleChunk) * x.bigits[i] + carry
        x.bigits[i] = (product & kBigitMask) % Chunk
        carry = product >> kBigitSize
    end
    while carry != 0
        x.bigits[x.used_digits+1] = carry & kBigitMask
        x.used_digits += 1
        carry >>= kBigitSize
    end
    end
    return
end

function multiplybyuint64!(x::Bignum,factor::UInt64)
    factor == 1 && return
    if factor == 0
        zero!(x)
        return
    end
    carry::UInt64 = 0
    low::UInt64 = factor & 0xFFFFFFFF
    high::UInt64 = factor >> 32
    @inbounds begin
    for i = 1:x.used_digits
        product_low::UInt64 = low * x.bigits[i]
        product_high::UInt64 = high * x.bigits[i]
        tmp::UInt64 = (carry & kBigitMask) + product_low
        x.bigits[i] = tmp & kBigitMask
        carry = (carry >> kBigitSize) + (tmp >> kBigitSize) +
                (product_high << (32 - kBigitSize))
    end
    while carry != 0
        x.bigits[x.used_digits+1] = carry & kBigitMask
        x.used_digits += 1
        carry >>= kBigitSize
    end
    end
    return
end

const kFive27 = UInt64(0x6765c793fa10079d)
const kFive1 = UInt16(5)
const kFive2 = UInt16(kFive1 * 5)
const kFive3 = UInt16(kFive2 * 5)
const kFive4 = UInt16(kFive3 * 5)
const kFive5 = UInt16(kFive4 * 5)
const kFive6 = UInt16(kFive5 * 5)
const kFive7 = UInt32(kFive6 * 5)
const kFive8 = UInt32(kFive7 * 5)
const kFive9 = UInt32(kFive8 * 5)
const kFive10 = UInt32(kFive9 * 5)
const kFive11 = UInt32(kFive10 * 5)
const kFive12 = UInt32(kFive11 * 5)
const kFive13 = UInt32(kFive12 * 5)
const kFive1_to_12 = UInt32[kFive1, kFive2, kFive3, kFive4, kFive5, kFive6,
        kFive7, kFive8, kFive9, kFive10, kFive11, kFive12]
function multiplybypoweroften!(x::Bignum,exponent)
    exponent == 0 && return
    x.used_digits == 0 && return
    remaining_exponent = exponent
    while remaining_exponent >= 27
        multiplybyuint64!(x,kFive27)
        remaining_exponent -= 27
    end
    while remaining_exponent >= 13
        multiplybyuint32!(x,kFive13)
        remaining_exponent -= 13
    end
    remaining_exponent > 0 && multiplybyuint32!(x,
                            kFive1_to_12[remaining_exponent])
    shiftleft!(x,exponent)
end

function square!(x::Bignum)
    product_length = 2 * x.used_digits
    (1 << (2 * (kChunkSize - kBigitSize))) <= x.used_digits && error("unimplemented")
    accumulator::DoubleChunk = 0
    copy_offset = x.used_digits
    @inbounds begin
    for i = 1:x.used_digits
        x.bigits[copy_offset + i] = x.bigits[i]
    end
    for i = 1:x.used_digits
        bigit_index1 = i-1
        bigit_index2 = 0
        while bigit_index1 >= 0
            chunk1::Chunk = x.bigits[copy_offset + bigit_index1 + 1]
            chunk2::Chunk = x.bigits[copy_offset + bigit_index2 + 1]
            accumulator += (chunk1 % DoubleChunk) * chunk2
            bigit_index1 -= 1
            bigit_index2 += 1
        end
        x.bigits[i] = (accumulator % Chunk) & kBigitMask
        accumulator >>= kBigitSize
    end
    for i = x.used_digits+1:product_length
        bigit_index1 = x.used_digits - 1
        bigit_index2 = i - bigit_index1 - 1
        while bigit_index2 < x.used_digits
            chunk1::Chunk = x.bigits[copy_offset + bigit_index1 + 1]
            chunk2::Chunk = x.bigits[copy_offset + bigit_index2 + 1]
            accumulator += (chunk1 % DoubleChunk) * chunk2
            bigit_index1 -= 1
            bigit_index2 += 1
        end
        x.bigits[i] = (accumulator % Chunk) & kBigitMask
        accumulator >>= kBigitSize
    end
    end
    x.used_digits = product_length
    x.exponent *= 2
    clamp!(x)
end

function assignpoweruint16!(x::Bignum,base::UInt16,power_exponent::Int)
    if power_exponent == 0
        assignuint16!(x,UInt16(1))
        return
    end
    zero!(x)
    shifts::Int = 0
    while base & UInt16(1) == UInt16(0)
        base >>= UInt16(1)
        shifts += 1
    end
    bit_size::Int = 0
    tmp_base::Int= base
    while tmp_base != 0
        tmp_base >>= 1
        bit_size += 1
    end
    final_size = bit_size * power_exponent
    mask::Int = 1
    while power_exponent >= mask
        mask <<= 1
    end
    mask >>= 2
    this_value::UInt64 = base
    delayed_multiplication = false
    max_32bits::UInt64 = 0xFFFFFFFF
    while mask != 0 && this_value <= max_32bits
        this_value *= this_value
        if (power_exponent & mask) != 0
            base_bits_mask::UInt64 = ~(UInt64(1) << (64 - bit_size) - 1)
            high_bits_zero = (this_value & base_bits_mask) == 0
            if high_bits_zero
                this_value *= base
            else
                delayed_multiplication = true
            end
        end
        mask >>= 1
    end
    assignuint64!(x,this_value)
    delayed_multiplication && multiplybyuint32!(x,UInt32(base))
    while mask != 0
        square!(x)
        (power_exponent & mask) != 0 && multiplybyuint32!(x,UInt32(base))
        mask >>= 1
    end
    shiftleft!(x,shifts * power_exponent)
end

function dividemodulointbignum!(x::Bignum,other::Bignum)
    bigitlength(x) < bigitlength(other) && return UInt16(0)
    align!(x,other)
    result::UInt16 = 0
    @inbounds begin
    while bigitlength(x) > bigitlength(other)
        result += x.bigits[x.used_digits] % UInt16
        subtracttimes!(x,other,x.bigits[x.used_digits])
    end
    this_bigit::Chunk = x.bigits[x.used_digits]
    other_bigit::Chunk = other.bigits[other.used_digits]
    if other.used_digits == 1
        quotient = reinterpret(Int32,div(this_bigit,other_bigit))
        x.bigits[x.used_digits] = this_bigit - other_bigit * reinterpret(UInt32,quotient)
        result += quotient % UInt16
        clamp!(x)
        return result
    end
    end
    division_estimate = reinterpret(Int32,div(this_bigit,other_bigit+Chunk(1)))
    result += division_estimate % UInt16
    subtracttimes!(x,other,division_estimate)
    other_bigit * (division_estimate+1) > this_bigit && return result
    while lessequal(other, x)
        subtractbignum!(x,other)
        result += UInt16(1)
    end
    return result
end

function pluscompare(a::Bignum,b::Bignum,c::Bignum)
    bigitlength(a) < bigitlength(b) && return pluscompare(b,a,c)
    bigitlength(a) + 1 < bigitlength(c) && return -1
    bigitlength(a) > bigitlength(c) && return 1
    a.exponent >= bigitlength(b) && bigitlength(a) < bigitlength(c) && return -1
    borrow::Chunk = 0
    min_exponent = min(a.exponent,b.exponent,c.exponent)
    for i = (bigitlength(c)-1):-1:min_exponent
        chunk_a::Chunk = bigitat(a,i)
        chunk_b::Chunk = bigitat(b,i)
        chunk_c::Chunk = bigitat(c,i)
        sum::Chunk = chunk_a + chunk_b
        if sum > chunk_c + borrow
            return 1
        else
            borrow = chunk_c + borrow - sum
            borrow > 1 && return -1
            borrow <<= kBigitSize
        end
    end
    borrow == 0 && return 0
    return -1
end

function compare(a::Bignum,b::Bignum)
    bigit_length_a = bigitlength(a)
    bigit_length_b = bigitlength(b)
    bigit_length_a < bigit_length_b && return -1
    bigit_length_a > bigit_length_b && return 1
    for i = (bigit_length_a-1):-1:min(a.exponent,b.exponent)
        bigit_a::Chunk = bigitat(a,i)
        bigit_b::Chunk = bigitat(b,i)
        bigit_a < bigit_b && return -1
        bigit_a > bigit_b && return 1
    end
    return 0
end

function bigitat(x::Bignum,index)
    index >= bigitlength(x) && return Chunk(0)
    index < x.exponent && return Chunk(0)
    @inbounds ret = x.bigits[index - x.exponent+1]::Chunk
    return ret
end

end # module
