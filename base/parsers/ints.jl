# This file is a part of Julia. License is MIT: https://julialang.org/license

# Decimal digits are parsed eight at a time: `load8` reads eight bytes as one word,
# `isdigits8` checks that they are all ASCII digits, and `digits8` converts them to
# their value with three multiplies instead of eight multiply-adds.

# Bytes `i:i+7` of `buf` as a little-endian word, so the first byte is the lowest.
# Storage with a stable pointer is read with one unaligned load.
@inline load8(buf::Union{Vector{UInt8}, Memory{UInt8},
                         FastContiguousSubArray{UInt8, 1, <:Union{Vector{UInt8}, Memory{UInt8}}}},
              i::Int) =
    GC.@preserve buf ltoh(unsafe_load(Ptr{UInt64}(pointer(buf, i))))
@inline load8(buf::CodeUnits{UInt8, <:DenseUTF8String}, i::Int) =
    GC.@preserve buf ltoh(unsafe_load(Ptr{UInt64}(pointer(buf.s, i))))
@inline function load8(buf::AbstractVector{UInt8}, i::Int)
    w = zero(UInt64)
    for k in 7:-1:0
        w = w << 8 | @inbounds buf[i + k]
    end
    return w
end

# Whether every byte of `w` is in '0':'9' (0x30:0x39): its high nibble must be 3,
# and still be 3 after adding 6, which carries 0x3a:0x3f into 0x40:0x45.
@inline isdigits8(w::UInt64) =
    ((w & 0xf0f0f0f0_f0f0f0f0) | ((w + 0x06060606_06060606) & 0xf0f0f0f0_f0f0f0f0) >> 4) ==
        0x33333333_33333333

# The value of the eight ASCII digits in `w`, first digit in the lowest byte. After
# subtracting '0', adjacent digits are combined into two-digit values in bytes 0, 2, 4
# and 6, and two multiplies sum those times 10^6, 10^4, 10^2 and 1 into the upper half.
@inline function digits8(w::UInt64)
    w -= 0x30303030_30303030
    w = w * 10 + (w >> 8)
    return (((w & 0x000000ff_000000ff) * 0x000f4240_00000064) +
            (((w >> 16) & 0x000000ff_000000ff) * 0x00002710_00000001)) >> 32
end

# The value of the code unit `b` as a digit in `base` (at most 62), or `typemax(b)` if it
# is not one: '0':'9' are 0:9, 'A':'Z' are 10:35, and 'a':'z' are 10:35 up to base 36
# and 36:61 above it.
@inline function digitvalue(b::Unsigned, base::Int)
    UInt8('0') <= b <= UInt8('9') && return b - UInt8('0')
    UInt8('A') <= b <= UInt8('Z') && return b - UInt8('A') + 0x0a
    UInt8('a') <= b <= UInt8('z') && return b - UInt8('a') + (base <= 36 ? 0x0a : 0x24)
    return typemax(b)
end

# The failure when the digits overflow before index `i`: `OVERFLOW`, unless a later byte is
# not a digit.
function overflowfailure(buf::AbstractVector{UInt8}, i::Int, j::Int, base::Int)
    for k in i:j
        digitvalue(@inbounds(buf[k]), base) < base || return INVALID
    end
    return OVERFLOW
end

# Parse the digits `buf[i:j]`, without a sign, in `base` (2 to 62) as a `T` that is negated
# if `neg` (signed `T` only), or return a `ParseFailure`. `i:j` must be in bounds.
@inline function parseint(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int,
                          base::Int, neg::Bool) where {T<:BitInteger}
    i <= j || return INVALID
    U = sizeof(T) > 8 ? UInt128 : UInt64
    n::U = 0
    if base == 10
        # the first 19 (38) decimal digits cannot overflow a UInt64 (UInt128)
        k = min(j, i + (U === UInt64 ? 18 : 37))
        while k - i >= 7
            w = load8(buf, i)
            isdigits8(w) || return INVALID
            n = n * U(100_000_000) + digits8(w)
            i += 8
        end
        while i <= k
            d = (@inbounds buf[i]) - UInt8('0')
            d <= 0x09 || return INVALID
            n = n * U(10) + d
            i += 1
        end
    end
    # any remaining digits, checking for overflow
    while i <= j
        d = digitvalue(@inbounds(buf[i]), base)
        d < base || return INVALID
        n, ov_mul = mul_with_overflow(n, base % U)
        n, ov_add = add_with_overflow(n, d % U)
        ov_mul | ov_add && return overflowfailure(buf, i + 1, j, base)
        i += 1
    end
    # a negative value can reach one past `typemax(T)`
    n <= (typemax(T) % U) + neg || return OVERFLOW
    return (neg ? -n : n) % T
end

# Other integer types accumulate in `T` itself, negatively for a negative value so that
# `typemin(T)` is reachable.
function parseint(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int,
                  base::Int, neg::Bool) where {T<:Integer}
    i <= j || return INVALID
    n = zero(T)
    for k in i:j
        d = digitvalue(@inbounds(buf[k]), base)
        d < base || return INVALID
        n, ov_mul = mul_with_overflow(n, T(base))
        n, ov_add = add_with_overflow(n, neg ? -T(d) : T(d))
        ov_mul | ov_add && return overflowfailure(buf, k + 1, j, base)
    end
    return n
end

"""
    Base.Parsers.parsevalue(T, bytes; kw...) -> Union{T, ParseFailure}

Parse all of `bytes`, a one-based `AbstractVector{UInt8}`, as a `T`, or return a
[`ParseFailure`](@ref Base.Parsers.ParseFailure) instead of throwing. Pass a view, such as
`view(buf, i:j)`, to parse part of a buffer.

For integer types, the bytes are an optional `-` or `+` (signed types only) followed by
digits in `base` (keyword, 2 to 62, default 10), with letters for digits above 9 as in
[`parse`](@ref).

# Examples
```jldoctest
julia> Base.Parsers.parsevalue(Int, codeunits("-42"))
-42

julia> Base.Parsers.parsevalue(UInt8, view(codeunits("x=256;"), 3:5))
Base.Parsers.OVERFLOW

julia> Base.Parsers.parsevalue(Int, UInt8[0x37, 0x66]; base = 16)
127
```
"""
function parsevalue end

@inline function parsevalue(::Type{T}, bytes::AbstractVector{UInt8};
                            base::Integer = 10) where {T<:Integer}
    require_one_based_indexing(bytes)
    2 <= base <= 62 ||
        throw(ArgumentError(LazyString("invalid base: base must be 2 ≤ base ≤ 62, got ", base)))
    i, j = 1, length(bytes)
    neg = false
    if T <: Signed && i <= j
        b = @inbounds bytes[i]
        if b == UInt8('-') || b == UInt8('+')
            neg = b == UInt8('-')
            i += 1
        end
    end
    return parseint(T, bytes, i, j, Int(base), neg)
end
