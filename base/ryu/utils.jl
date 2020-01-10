const MANTISSA_MASK = Base.significand_mask(Float64)
const EXP_MASK = Base.exponent_mask(Float64) >> Base.significand_bits(Float64)

memcpy(d, doff, s, soff, n) = (ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), d + doff - 1, s + soff - 1, n); nothing)
memmove(d, doff, s, soff, n) = (ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), d + doff - 1, s + soff - 1, n); nothing)

# Note: these are smaller than the values given in Figure 4 from the paper
# see https://github.com/ulfjack/ryu/issues/119
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

"""
    Ryu.log10pow2(e::Integer)

Computes `floor(log10(2^e))`. This is valid for all `e < 1651`.
"""
log10pow2(e) = (e * 78913) >> 18


"""
    Ryu.log10pow5(e::Integer)

Computes `floor(log10(5^e))`. This is valid for all `e < 2621`.
"""
log10pow5(e) = (e * 732923) >> 20

"""
    Ryu.pow5bits(e)

Computes `e == 0 ? 1 : ceil(log2(5^e))`. This is valid for `e < 3529` (if performend in `Int32` arithmetic).
"""
pow5bits(e) = ((e * 1217359) >> 19) + 1

""""
     Ryu.mulshift(m::U, mula, j) where {U<:Unsigned}

Compute `(m * mul) >> j`, where `j >= 8*sizeof(U)`. The type of the results is the larger of `U` or `UInt32`.
"""
@inline function mulshift(m::U, mul, j) where {U<:Unsigned}
    W = widen(U)
    nbits = 8*sizeof(U)
    return ((((W(m) * (mul % U)) >> nbits) + W(m) * (mul >> nbits)) >> (j - nbits)) % promote_type(U,UInt32)
end

indexforexp(e) = div(e + 15, 16)
pow10bitsforindex(idx) = 16 * idx + 120
lengthforindex(idx) = div(((Int64(16 * idx) * 1292913986) >> 32) + 1 + 16 + 8, 9)

"""
    Ryu.pow5(x, p)

Return `true` if `5^p` is a divisor of `x`.
"""
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

"""
    Ryu.pow2(x, p)

Return `true` if `2^p` is a divisor of `x`. In other words, if the trailing `p` bits of `x` are zero.
"""
pow2(x, p) = (x & ((Int64(1) << p) - 1)) == 0

"""
    Ryu.decimallength(v)

The number of decimal digits of the integer `v`.
"""
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

@inline function mulshiftinvsplit(::Type{T}, mv, mp, mm, i, j) where {T}
    mul = pow5invsplit_lookup(T, i)
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

@inline function mulshiftsplit(::Type{T}, mv, mp, mm, i, j) where {T}
    mul = pow5split_lookup(T, i)
    vr = mulshift(mv, mul, j)
    vp = mulshift(mp, mul, j)
    vm = mulshift(mm, mul, j)
    return vr, vp, vm
end

"""
    Ryu.umul256(a::UInt128, bHi::UInt64, bLo::UInt64)::Tuple{UInt128, UInt128}

Compute `p = a*b` where `b = bLo + bHi<<64`, returning the result as `pLo, pHi` where `p = pLo + pHi<<128`.
"""
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

"""
    Ryu.umul256_hi(a::UInt128, bHi::UInt64, bLo::UInt64)::UInt128

Compute `pHi = (a*b)>>128` where `b = bLo + bHi<<64`.
"""
@inline umul256_hi(a, bHi, bLo) = umul256(a, bHi, bLo)[2]

"""
    Ryu.mulshiftmod1e9(m, mula, mulb, mulc, j)::UInt32

Compute `(m * mul) >> j % 10^9` where `mul = mula + mulb<<64 + mulc<<128`, and `j >= 128`.
"""
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

"""
    Ryu.pow5invsplit(T, i)

Compute `floor(2^k/5^i)+1`, where `k = pow5bits(i) - 1 + pow5_inv_bitcount(T)`. The result
is an unsigned integer twice as wide as `T` (i.e. a `UInt128` if `T == Float64`), with
`pow5_inv_bitcount(T)` significant bits.
"""
function pow5invsplit(::Type{T}, i) where {T<:AbstractFloat}
    W = widen(uinttype(T))
    pow = big(5)^i
    inv = div(big(1) << (ndigits(pow, base=2) - 1 + pow5_inv_bitcount(T)), pow) + 1
    return W(inv)
end

"""
    Ryu.pow5invsplit_lookup(T, i)

[`pow5invsplit`](@ref) computed via lookup table.
"""
function pow5invsplit_lookup end
for T in (Float64, Float32, Float16)
    e2_max = exponent_max(T) - precision(T) - 2
    i_max = log10pow2(e2_max)
    table = [pow5invsplit(T, i) for i = 0:i_max]
    @eval pow5invsplit_lookup(::Type{$T}, i) = @inbounds($table[i+1])
end


"""
    Ryu.pow5split(T, i)

Compute `floor(5^i/2^k)`, where `k = pow5bits(i) - pow5_bitcount(T)`. The result is an
unsigned integer twice as wide as `T` (i.e. a `UInt128` if `T == Float64`), with
`pow5_bitcount(T)` significant bits.
"""
function pow5split(::Type{T}, i) where {T<:AbstractFloat}
    W = widen(uinttype(T))
    pow = big(5)^i
    return W(pow >> (ndigits(pow, base=2) - pow5_bitcount(T)))
end

"""
    Ryu.pow5split_lookup(T, i)

[`pow5split`](@ref) computed via lookup table.
"""
function pow5split_lookup end
for T in (Float64, Float32, Float16)
    e2_min = 1 - exponent_bias(T) - significand_bits(T) - 2
    i_max = 1 - e2_min - log10pow5(-e2_min)
    table = [pow5split(T, i) for i = 0:i_max]
    @eval pow5split_lookup(::Type{$T}, i) = @inbounds($table[i+1])
end

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
