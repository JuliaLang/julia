const MANTISSA_MASK = 0x000fffffffffffff
const EXP_MASK = 0x00000000000007ff

memcpy(d, doff, s, soff, n) = ccall(:memcpy, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Int), d + doff - 1, s + soff - 1, n)
memmove(d, doff, s, soff, n) = ccall(:memmove, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Int), d + doff - 1, s + soff - 1, n)

uint(x::Float16) = Core.bitcast(UInt16, x)
uint(x::Float32) = Core.bitcast(UInt32, x)
uint(x::Float64) = Core.bitcast(UInt64, x)

mantissabits(::Type{Float16}) = 10
mantissabits(::Type{Float32}) = 23
mantissabits(::Type{Float64}) = 52

exponentbits(::Type{Float16}) = 5
exponentbits(::Type{Float32}) = 8
exponentbits(::Type{Float64}) = 11

bias(::Type{Float16}) = 15
bias(::Type{Float32}) = 127
bias(::Type{Float64}) = 1023

pow5_bitcount(::Type{Float16}) = 30
pow5_bitcount(::Type{Float32}) = 61
pow5_bitcount(::Type{Float64}) = 121

pow5_inv_bitcount(::Type{Float16}) = 30
pow5_inv_bitcount(::Type{Float32}) = 59
pow5_inv_bitcount(::Type{Float64}) = 122

qinvbound(::Type{Float16}) = 4
qinvbound(::Type{Float32}) = 9
qinvbound(::Type{Float64}) = 21

qbound(::Type{Float16}) = 15
qbound(::Type{Float32}) = 31
qbound(::Type{Float64}) = 63

log10pow2(e) = (e * 78913) >> 18
log10pow5(e) = (e * 732923) >> 20
pow5bits(e) = ((e * 1217359) >> 19) + 1
@inline mulshift(m::UInt64, mula, mulb, j) = ((((UInt128(m) * mula) >> 64) + UInt128(m) * mulb) >> (j - 64)) % UInt64
@inline mulshift(m::UInt32, mul, j) = ((((UInt64(m) * (mul % UInt32)) >> 32) + (UInt64(m) * (mul >> 32))) >> (j - 32)) % UInt32
@inline mulshift(m::UInt16, mul, j) = ((((UInt32(m) * (mul % UInt16)) >> 16) + (UInt32(m) * (mul >> 16))) >> (j - 16))
indexforexp(e) = div(e + 15, 16)
pow10bitsforindex(idx) = 16 * idx + 120
lengthforindex(idx) = div(((Int64(16 * idx) * 1292913986) >> 32) + 1 + 16 + 8, 9)

@inline function pow5(x, p)
    count = 0
    while true
        q = div(x, 5)
        r = x - 5 * q
        r != 0 && return count >= p
        x = q
        count += 1
    end
end

pow2(x, p) = (x & ((Int64(1) << p) - 1)) == 0

@inline function decimallength(v)
    v >= 10000000000000000 && return 17
    v >= 1000000000000000 && return 16
    v >= 100000000000000 && return 15
    v >= 10000000000000 && return 14
    v >= 1000000000000 && return 13
    v >= 100000000000 && return 12
    v >= 10000000000 && return 11
    v >= 1000000000 && return 10
    v >= 100000000 && return 9
    v >= 10000000 && return 8
    v >= 1000000 && return 7
    v >= 100000 && return 6
    v >= 10000 && return 5
    v >= 1000 && return 4
    v >= 100 && return 3
    v >= 10 && return 2
    return 1
end

@inline function decimallength(v::UInt32)
    v >= 100000000 && return 9
    v >= 10000000 && return 8
    v >= 1000000 && return 7
    v >= 100000 && return 6
    v >= 10000 && return 5
    v >= 1000 && return 4
    v >= 100 && return 3
    v >= 10 && return 2
    return 1
end

@inline function decimallength(v::UInt16)
    v >= 10000 && return 5
    v >= 1000 && return 4
    v >= 100 && return 3
    v >= 10 && return 2
    return 1
end

@inline function mulshiftinvsplit(::Type{Float64}, mv, mp, mm, i, j)
    @inbounds mula, mulb = DOUBLE_POW5_INV_SPLIT[i + 1]
    vr = mulshift(mv, mula, mulb, j)
    vp = mulshift(mp, mula, mulb, j)
    vm = mulshift(mm, mula, mulb, j)
    return vr, vp, vm
end

@inline function mulshiftinvsplit(::Type{Float32}, mv, mp, mm, i, j)
    @inbounds mul = FLOAT_POW5_INV_SPLIT[i + 1]
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

@inline function mulshiftinvsplit(::Type{Float16}, mv, mp, mm, i, j)
    @inbounds mul = HALF_POW5_INV_SPLIT[i + 1]
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

@inline function mulshiftsplit(::Type{Float64}, mv, mp, mm, i, j)
    @inbounds mula, mulb = DOUBLE_POW5_SPLIT[i + 1]
    vr = mulshift(mv, mula, mulb, j)
    vp = mulshift(mp, mula, mulb, j)
    vm = mulshift(mm, mula, mulb, j)
    return vr, vp, vm
end

@inline function mulshiftsplit(::Type{Float32}, mv, mp, mm, i, j)
    @inbounds mul = FLOAT_POW5_SPLIT[i + 1]
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

@inline function mulshiftsplit(::Type{Float16}, mv, mp, mm, i, j)
    @inbounds mul = HALF_POW5_SPLIT[i + 1]
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

@inline function umul256(a, bHi, bLo)
    aLo = a % UInt64
    aHi = (a >> 64) % UInt64

    b00 = UInt128(aLo) * bLo
    b01 = UInt128(aLo) * bHi
    b10 = UInt128(aHi) * bLo
    b11 = UInt128(aHi) * bHi

    b00Lo = b00 % UInt64
    b00Hi = (b00 >> 64) % UInt64

    mid1 = b10 + b00Hi
    mid1Lo = mid1 % UInt64
    mid1Hi = (mid1 >> 64) % UInt64

    mid2 = b01 + mid1Lo
    mid2Lo = mid2 % UInt64
    mid2Hi = (mid2 >> 64) % UInt64

    pHi = b11 + mid1Hi + mid2Hi
    pLo = (UInt128(mid2Lo) << 64) | b00Lo
    return pLo, pHi
end

@inline umul256_hi(a, bHi, bLo) = umul256(a, bHi, bLo)[2]

@inline function mulshiftmod1e9(m, mula, mulb, mulc, j)
    b0 = UInt128(m) * mula
    b1 = UInt128(m) * mulb
    b2 = UInt128(m) * mulc
    mid = b1 + ((b0 >> 64) % UInt64)
    s1 = b2 + ((mid >> 64) % UInt64)
    v = s1 >> (j - 128)
    multiplied = umul256_hi(v, 0x89705F4136B4A597, 0x31680A88F8953031)
    shifted = (multiplied >> 29) % UInt32
    return (v % UInt32) - UInt32(1000000000) * shifted
end

@inline function append_n_digits(olength, digits, buf, pos)
    i = 0
    while digits >= 10000
        c = digits % 10000
        digits = div(digits, 10000)
        c0 = (c % 100) << 1
        c1 = div(c, 100) << 1
        unsafe_copyto!(buf, pos + olength - i - 2, DIGIT_TABLE, c0 + 1, 2)
        unsafe_copyto!(buf, pos + olength - i - 4, DIGIT_TABLE, c1 + 1, 2)
        i += 4
    end
    if digits >= 100
        c = (digits % 100) << 1
        digits = div(digits, 100)
        unsafe_copyto!(buf, pos + olength - i - 2, DIGIT_TABLE, c + 1, 2)
        i += 2
    end
    if digits >= 10
        c = digits << 1
        unsafe_copyto!(buf, pos + olength - i - 2, DIGIT_TABLE, c + 1, 2)
        i += 2
    else
        buf[pos] = UInt8('0') + digits
        i += 1
    end
    return pos + i
end

@inline function append_d_digits(olength, digits, buf, pos, decchar)
    i = 0
    while digits >= 10000
        c = digits % 10000
        digits = div(digits, 10000)
        c0 = (c % 100) << 1
        c1 = div(c, 100) << 1
        unsafe_copyto!(buf, pos + olength + 1 - i - 2, DIGIT_TABLE, c0 + 1, 2)
        unsafe_copyto!(buf, pos + olength + 1 - i - 4, DIGIT_TABLE, c1 + 1, 2)
        i += 4
    end
    if digits >= 100
        c = (digits % 100) << 1
        digits = div(digits, 100)
        unsafe_copyto!(buf, pos + olength + 1 - i - 2, DIGIT_TABLE, c + 1, 2)
        i += 2
    end
    if digits >= 10
        c = digits << 1
        buf[pos] = DIGIT_TABLE[c + 1]
        buf[pos + 1] = decchar
        buf[pos + 2] = DIGIT_TABLE[c + 2]
        i += 3
    else
        buf[pos] = UInt8('0') + digits
        buf[pos + 1] = decchar
        i += 2
    end
    return pos + i
end

@inline function append_c_digits(count, digits, buf, pos)
    i = 0
    while i < count - 1
        c = (digits % 100) << 1
        digits = div(digits, 100)
        unsafe_copyto!(buf, pos + count - i - 2, DIGIT_TABLE, c + 1, 2)
        i += 2
    end
    if i < count
        buf[pos + count - i - 1] = UInt8('0') + (digits % 10)
        i += 1
    end
    return pos + i
end

@inline function append_nine_digits(digits, buf, pos)
    if digits == 0
        for _ = 1:9
            buf[pos] = UInt8('0')
            pos += 1
        end
        return pos
    end
    i = 0
    while i < 5
        c = digits % 10000
        digits = div(digits, 10000)
        c0 = (c % 100) << 1
        c1 = div(c, 100) << 1
        unsafe_copyto!(buf, pos + 7 - i, DIGIT_TABLE, c0 + 1, 2)
        unsafe_copyto!(buf, pos + 5 - i, DIGIT_TABLE, c1 + 1, 2)
        i += 4
    end
    buf[pos] = UInt8('0') + digits
    i += 1
    return pos + i
end

const BIG_MASK = (big(1) << 64) - 1

const POW10_SPLIT = collect(Iterators.flatten(map(0:63) do idx
    pow10bits = pow10bitsforindex(idx)
    map(0:lengthforindex(idx)-1) do i
        v = (div(big(1) << pow10bits, big(10)^(9 * i)) + 1) % ((big(10)^9) << 136)
        return (UInt64(v & BIG_MASK), UInt64((v >> 64) & BIG_MASK), UInt64((v >> 128) & BIG_MASK))
    end
end))

function generateinversetables()
    POW10_OFFSET_2 = Vector{UInt16}(undef, 68 + 1)
    MIN_BLOCK_2 = fill(0xff, 68 + 1)
    POW10_SPLIT_2 = Tuple{UInt64, UInt64, UInt64}[]
    lowerCutoff = big(1) << (54 + 8)
    for idx = 0:67
        POW10_OFFSET_2[idx + 1] = length(POW10_SPLIT_2)
        i = 0
        while true
            v = ((big(10)^(9 * (i + 1)) >> (-(120 - 16 * idx))) % (big(10)^9) << (120 + 16))
            if MIN_BLOCK_2[idx + 1] == 0xff && ((v * lowerCutoff) >> 128) == 0
                i += 1
                continue
            end
            if MIN_BLOCK_2[idx + 1] == 0xff
                MIN_BLOCK_2[idx + 1] = i
            end
            v == 0 && break
            push!(POW10_SPLIT_2, ((v & BIG_MASK) % UInt64, ((v >> 64) & BIG_MASK) % UInt64, ((v >> 128) & BIG_MASK) % UInt64))
            i += 1
        end
    end
    POW10_OFFSET_2[end] = length(POW10_SPLIT_2)
    MIN_BLOCK_2[end] = 0x00

    return POW10_OFFSET_2, MIN_BLOCK_2, POW10_SPLIT_2
end

const POW10_OFFSET_2, MIN_BLOCK_2, POW10_SPLIT_2 = generateinversetables()

bitlength(this) = Base.GMP.MPZ.sizeinbase(this, 2)

@inline function pow5invsplit(::Type{Float64}, i)
    pow = big(5)^i
    inv = div(big(1) << (bitlength(pow) - 1 + pow5_inv_bitcount(Float64)), pow) + 1
    return (UInt64(inv & ((big(1) << 64) - 1)), UInt64(inv >> 64))
end

@inline function pow5invsplit(::Type{Float32}, i)
    pow = big(5)^i
    inv = div(big(1) << (bitlength(pow) - 1 + pow5_inv_bitcount(Float32)), pow) + 1
    return UInt64(inv)
end

@inline function pow5invsplit(::Type{Float16}, i)
    pow = big(5)^i
    inv = div(big(1) << (bitlength(pow) - 1 + pow5_inv_bitcount(Float16)), pow) + 1
    return UInt32(inv)
end

@inline function pow5split(::Type{Float64}, i)
    pow = big(5)^i
    j = bitlength(pow) - pow5_bitcount(Float64)
    return (UInt64((pow >> j) & ((big(1) << 64) - 1)), UInt64(pow >> (j + 64)))
end

@inline function pow5split(::Type{Float32}, i)
    pow = big(5)^i
    return UInt64(pow >> (bitlength(pow) - pow5_bitcount(Float32)))
end

@inline function pow5split(::Type{Float16}, i)
    pow = big(5)^i
    return UInt32(pow >> (bitlength(pow) - pow5_bitcount(Float16)))
end

const DOUBLE_POW5_INV_SPLIT = map(i->pow5invsplit(Float64, i), 0:291)
const FLOAT_POW5_INV_SPLIT = map(i->pow5invsplit(Float32, i), 0:30)
const HALF_POW5_INV_SPLIT = map(i->pow5invsplit(Float16, i), 0:17)

const DOUBLE_POW5_SPLIT = map(i->pow5split(Float64, i), 0:325)
const FLOAT_POW5_SPLIT = map(i->pow5split(Float32, i), 0:46)
const HALF_POW5_SPLIT = map(i->pow5split(Float16, i), 0:23)

const DIGIT_TABLE = UInt8[
  '0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
  '1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
  '2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
  '3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
  '4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
  '5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
  '6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
  '7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
  '8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
  '9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
]

const POW10_OFFSET = UInt16[
  0, 2, 5, 8, 12, 16, 21, 26, 32, 39,
  46, 54, 62, 71, 80, 90, 100, 111, 122, 134,
  146, 159, 173, 187, 202, 217, 233, 249, 266, 283,
  301, 319, 338, 357, 377, 397, 418, 440, 462, 485,
  508, 532, 556, 581, 606, 632, 658, 685, 712, 740,
  769, 798, 828, 858, 889, 920, 952, 984, 1017, 1050,
  1084, 1118, 1153, 1188
]
