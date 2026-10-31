# This file is a part of Julia. License is MIT: https://julialang.org/license

# Decimal numbers are converted with Clinger's exact fast path when the significand and
# the power of ten are both exact in `T`, and otherwise with the Eisel–Lemire algorithm,
# as implemented in fast_float: one or two 64×64-bit products with a 128-bit power of five
# give the correctly rounded result for any significand of at most 19 digits (Mushtak &
# Lemire, "Fast Number Parsing Without Fallback"). A longer significand is rounded from its
# first 19 digits, unless the value is so close to a midpoint between two floats that all of
# its digits must be compared with the midpoint. A hexadecimal float is a binary fraction,
# so its first 16 hexadecimal digits, and whether any later one is nonzero, decide it.

# Arithmetic on unsigned integers of any size, stored as little-endian 64-bit limbs

# x = x * m + a
function mulsmall!(x::Vector{UInt64}, m::UInt64, a::UInt64 = zero(UInt64))
    for k in eachindex(x)
        p = widemul(x[k], m) + a
        x[k] = p % UInt64
        a = (p >> 64) % UInt64
    end
    iszero(a) || push!(x, a)
    return x
end
# x = x ÷ 5
function div5!(x::Vector{UInt64})
    r = zero(UInt128)
    for k in reverse(eachindex(x))
        n = r << 64 | x[k]
        x[k] = (n ÷ 5) % UInt64
        r = n % 5
    end
    return x
end
# The 128 most significant bits of x
function top128(x::Vector{UInt64})
    k = findlast(!iszero, x)
    s = leading_zeros(x[k])
    limb(i) = i >= 1 ? x[i] : zero(UInt64)
    hi = s == 0 ? limb(k) : limb(k) << s | limb(k - 1) >> (64 - s)
    lo = s == 0 ? limb(k - 1) : limb(k - 1) << s | limb(k - 2) >> (64 - s)
    return UInt128(hi) << 64 | lo
end

# fast_float's 128-bit significands of 5^q, for every q a `Float64` needs: the top 128 bits
# of 5^q, or of floor(2^1024 / 5^-q) for q < 0, plus one for -27 <= q < 0. The proof that
# no fallback is needed holds for exactly these values.
const POW5_MIN, POW5_MAX = -342, 308
function pow5table()
    table = Vector{UInt128}(undef, POW5_MAX - POW5_MIN + 1)
    x = UInt64[1]
    for q in 0:POW5_MAX
        table[q - POW5_MIN + 1] = top128(x)
        mulsmall!(x, UInt64(5))
    end
    x = zeros(UInt64, 17)
    x[17] = 1
    for q in -1:-1:POW5_MIN
        table[q - POW5_MIN + 1] = top128(div5!(x)) + (q >= -27)
    end
    return table
end
const POW5 = pow5table()

# The powers of ten outside of which every nonzero 64-bit significand rounds to zero or
# overflows, and those for which a product can fall exactly halfway between two floats.
pow10range(::Type{Float64}) = (-342, 308)
pow10range(::Type{Float32}) = (-64, 38)
pow10range(::Type{Float16}) = (-26, 4)
halfwayrange(::Type{Float64}) = (-4, 23)
halfwayrange(::Type{Float32}) = (-17, 10)
halfwayrange(::Type{Float16}) = (-26, 5)

@inline function mul64(a::UInt64, b::UInt64)
    p = widemul(a, b)
    return (p >> 64) % UInt64, p % UInt64
end

# The bits of a `T` from its significand m, of `significand_bits(T) + 1` bits or one more
# if rounding carried, and its biased exponent e: those of infinity if it overflows. For
# e <= 0, m is all of a subnormal's bits, which are floatmin's if rounding carried.
@inline function packbits(::Type{T}, m::UInt64, e::Int) where {T}
    mbits = significand_bits(T)
    e <= 0 && return m
    if m >= UInt64(2) << mbits
        m >>= 1
        e += 1
    end
    e >= exponent_raw_max(T) && return UInt64(exponent_raw_max(T)) << mbits
    return UInt64(e) << mbits | (m & (UInt64(1) << mbits - 1))
end

# The bits of the `T` nearest to w * 10^q, for w > 0: zero if it rounds to zero, and those
# of infinity if it overflows.
@inline function eisellemire(::Type{T}, w::UInt64, q::Int) where {T}
    mbits = significand_bits(T)
    qmin, qmax = pow10range(T)
    q < qmin && return zero(UInt64)
    q > qmax && return UInt64(exponent_raw_max(T)) << mbits
    lz = leading_zeros(w)
    w <<= lz
    # w * 5^q, refined by the next 64 bits of 5^q if the bits below those kept are all ones
    @inbounds p5 = POW5[q - POW5_MIN + 1]
    hi, lo = mul64(w, (p5 >> 64) % UInt64)
    mask = typemax(UInt64) >> (mbits + 3)
    if hi & mask == mask
        hi2, _ = mul64(w, p5 % UInt64)
        lo += hi2
        hi += lo < hi2
    end
    upper = (hi >> 63) % Int
    shift = upper + 64 - mbits - 3
    m = hi >> shift                              # the significand and a rounding bit
    e = ((217706 * q) >> 16) + 63 + upper - lz + exponent_bias(T)
    if e <= 0                                    # a subnormal has fewer significand bits
        shift += 1 - e
        shift >= 64 && return zero(UInt64)
        m = hi >> shift
    end
    hlo, hhi = halfwayrange(T)
    if lo <= 1 && hlo <= q <= hhi && m & 3 == 1 && m << shift == hi
        m &= ~UInt64(1)                          # exactly halfway: round to even
    end
    return packbits(T, (m + (m & 1)) >> 1, e)
end

# The powers of ten that are exact in `T`, for Clinger's fast path, in the type it computes
# in. `Float16` computes in `Float32`, which gives the same rounding for every w <= 2^11.
const EXACT_POW10_64 = ntuple(k -> Float64(UInt128(10)^(k - 1)), 23)
const EXACT_POW10_32 = ntuple(k -> Float32(UInt64(10)^(k - 1)), 11)
const EXACT_POW10_16 = EXACT_POW10_32[1:5]
exactpow10s(::Type{Float64}) = EXACT_POW10_64
exactpow10s(::Type{Float32}) = EXACT_POW10_32
exactpow10s(::Type{Float16}) = EXACT_POW10_16

@inline function frombits(::Type{T}, bits::UInt64, neg::Bool) where {T}
    iszero(bits) && return UNDERFLOW
    bits == UInt64(exponent_raw_max(T)) << significand_bits(T) && return OVERFLOW
    x = reinterpret(T, bits % uinttype(T))
    return neg ? -x : x
end

# w * 10^q as a `T`, negated if `neg`, for a significand w of at most 19 digits
@inline function fromdecimal(::Type{T}, w::UInt64, q::Int, neg::Bool) where {T}
    iszero(w) && return neg ? -zero(T) : zero(T)
    p10 = exactpow10s(T)
    if abs(q) < length(p10) && w <= UInt64(1) << (significand_bits(T) + 1)
        # both factors are exact, so one rounding gives the nearest float
        S = eltype(p10)
        x = T(q < 0 ? S(w) / (@inbounds p10[1 - q]) : S(w) * (@inbounds p10[q + 1]))
        T === Float16 && isinf(x) && return OVERFLOW
        return neg ? -x : x
    end
    return frombits(T, eisellemire(T, w, q), neg)
end

# x = x * b^n, for b = 2 or 5
function mulpow!(x::Vector{UInt64}, b::Int, n::Int)
    step = b == 2 ? 63 : 27                      # the largest powers that fit in a limb
    while n > 0
        k = min(n, step)
        mulsmall!(x, UInt64(b)^k)
        n -= k
    end
    return x
end

# How the number with the digits in `buf[k:l]`, skipping a '.', times 10^p compares with the
# midpoint between the `T`s with bits `bits` and `bits + 1`: -1, 0 or 1. A midpoint has at
# most 768 significant digits, so the first 800 decide unless they are equal, when any later
# nonzero digit makes the number larger.
@noinline function cmpmidpoint(::Type{T}, buf::AbstractVector{UInt8}, k::Int, l::Int,
                               p::Int, bits::UInt64) where {T}
    x = UInt64[0]
    w, nw, n = zero(UInt64), 0, 0                # w has the nw digits not yet in x
    sticky = false
    for i in k:l
        d = (@inbounds buf[i]) - UInt8('0')
        d <= 0x09 || continue
        if n < 800
            w = 10w + d
            nw += 1
            n += 1
            nw == 19 && (mulsmall!(x, UInt64(10)^19, w); w = zero(UInt64); nw = 0)
        else
            sticky |= !iszero(d)
            p += 1
        end
    end
    mulsmall!(x, UInt64(10)^nw, w)
    # the midpoint is y * 2^e
    mbits = significand_bits(T)
    be = Int(bits >> mbits)                      # the biased exponent
    m = bits & (UInt64(1) << mbits - 1)
    be > 0 && (m |= UInt64(1) << mbits)
    e = max(be, 1) - exponent_bias(T) - mbits - 1
    y = UInt64[2m + 1]
    # x * 5^p * 2^p against y * 2^e, as integers
    p >= 0 ? mulpow!(x, 5, p) : mulpow!(y, 5, -p)
    e >= p ? mulpow!(y, 2, e - p) : mulpow!(x, 2, p - e)
    for i in max(length(x), length(y)):-1:1
        a, b = get(x, i, zero(UInt64)), get(y, i, zero(UInt64))
        a == b || return a < b ? -1 : 1
    end
    return Int(sticky)
end

# The decimal digits at the start of `buf[i:j]` accumulated into w, modulo 2^64, and the
# index after them
@inline function scandigits(buf::AbstractVector{UInt8}, i::Int, j::Int, w::UInt64)
    while j - i >= 7
        x = load8(buf, i)
        isdigits8(x) || break
        w = w * 100_000_000 + digits8(x)
        i += 8
    end
    while i <= j
        d = (@inbounds buf[i]) - UInt8('0')
        d <= 0x09 || break
        w = 10w + d
        i += 1
    end
    return w, i
end

# The exponent at the start of `buf[i:j]`, decimal digits after an optional sign: its value,
# which stops growing far past every float's range, the index after it, and whether it has
# any digits
@inline function scanexponent(buf::AbstractVector{UInt8}, i::Int, j::Int)
    i <= j || return 0, i, false
    b = @inbounds buf[i]
    neg = b == UInt8('-')
    (neg || b == UInt8('+')) && (i += 1)
    e = 0
    k = i
    while i <= j
        d = (@inbounds buf[i]) - UInt8('0')
        d <= 0x09 || break
        e < 100_000_000 && (e = 10e + d)
        i += 1
    end
    return neg ? -e : e, i, i > k
end

# The bits of the `T` nearest to (m + f) * 2^e, for m > 0 and some 0 <= f < 1 that is
# nonzero if `sticky`: zero if it rounds to zero, and those of infinity if it overflows.
@inline function hexbits(::Type{T}, m::UInt64, e::Int, sticky::Bool) where {T}
    mbits = significand_bits(T)
    lz = leading_zeros(m)
    m <<= lz
    e += 63 - lz + exponent_bias(T)              # the biased exponent of m's leading bit
    # the bits below the result's last one, more for a subnormal
    shift = 63 - mbits + max(1 - e, 0)
    shift > 64 && return zero(UInt64)            # below half of the smallest subnormal
    rest = m & (UInt64(1) << shift - 1)
    half = UInt64(1) << (shift - 1)
    m >>= shift
    m += rest > half || (rest == half && (sticky || isodd(m)))
    return packbits(T, m, e)
end

# The hexadecimal digits at the start of `buf[i:j]`, shifted into m while it has room for
# them: the new m, the index after the last digit it took, whether a digit it had no room
# for is nonzero, and the index after the digits
@inline function scanhex(buf::AbstractVector{UInt8}, i::Int, j::Int, m::UInt64,
                         sticky::Bool)
    while j - i >= 7 && m >> 32 == 0             # eight at a time while m has room
        w = load8(buf, i)
        ishexdigits8(w) || break
        m = m << 32 | hexdigits8(w)
        i += 8
    end
    k = i
    while i <= j
        b = @inbounds buf[i]
        d, h = b - UInt8('0'), (b | 0x20) - UInt8('a')
        (d <= 0x09) | (h <= 0x05) || break
        if m >> 60 == 0
            m = m << 4 | ifelse(d <= 0x09, d, h + 0x0a)
            k += 1
        else
            sticky |= d != 0x00
        end
        i += 1
    end
    return m, k, sticky, i
end

# A hexadecimal float, from the digits after its "0x"
@noinline function parsehex(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int,
                            neg::Bool, decimal::UInt8) where {T}
    m, k, sticky, l = scanhex(buf, i, j, zero(UInt64), false)
    e = 4 * (l - k)                              # integer digits m had no room for
    nd = l - i
    if l <= j && (@inbounds buf[l]) == decimal
        m, k, sticky, i = scanhex(buf, l + 1, j, m, sticky)
        e -= 4 * (k - l - 1)                     # fraction digits in m
        nd += i - l - 1
        l = i
    end
    nd > 0 || return INVALID
    if l <= j && (@inbounds buf[l]) | 0x20 == UInt8('p')
        x, l, ok = scanexponent(buf, l + 1, j)
        ok || return INVALID
        e += x
    end
    l > j || return INVALID
    iszero(m) && return neg ? -zero(T) : zero(T)
    return frombits(T, hexbits(T, m, e, sticky), neg)
end

# Whether the bytes from `buf[i]` on spell the lowercase ASCII `word`, in any case
@inline function matchword(buf::AbstractVector{UInt8}, i::Int, word::String)
    for k in 1:ncodeunits(word)
        (@inbounds buf[i + k - 1]) | 0x20 == codeunit(word, k) || return false
    end
    return true
end

# Whether `b` may be inside the parentheses of `nan(...)`: an ASCII letter or digit, or '_'
isnanchar(b::UInt8) =
    b - UInt8('0') <= 0x09 || (b | 0x20) - UInt8('a') <= 0x19 || b == UInt8('_')

# The NaN for `buf[i:j]`, what follows "nan(" in `nan(chars)`. As in glibc, chars that C's
# `strtoull` reads in full as an unsigned integer (decimal, `0x` hexadecimal or `0` octal)
# set the NaN's payload, its significand bits below the quiet bit.
function nanpayload(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int) where {T}
    (@inbounds buf[j]) == UInt8(')') || return INVALID
    j -= 1
    for k in i:j
        isnanchar(@inbounds buf[k]) || return INVALID
    end
    base = 10
    if i <= j && (@inbounds buf[i]) == UInt8('0')
        base = 8
        if i < j && (@inbounds buf[i + 1]) | 0x20 == UInt8('x')
            i, base = i + 2, 16
        end
    end
    p = parseint(UInt64, buf, i, j, base, false)
    payload = p isa UInt64 ? p : p === OVERFLOW ? typemax(UInt64) : zero(UInt64)
    U = uinttype(T)
    mask = one(U) << (significand_bits(T) - 1) - one(U)
    return reinterpret(T, reinterpret(U, T(NaN)) | (payload % U & mask))
end

# `inf`, `infinity`, `nan` or `nan(chars)`, in any case
@noinline function parsespecial(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int,
                                neg::Bool) where {T}
    n = j - i + 1
    if (n == 3 && matchword(buf, i, "inf")) || (n == 8 && matchword(buf, i, "infinity"))
        x = T(Inf)
    elseif n == 3 && matchword(buf, i, "nan")
        x = T(NaN)
    elseif n > 4 && matchword(buf, i, "nan") && (@inbounds buf[i + 3]) == UInt8('(')
        x = nanpayload(T, buf, i + 4, j)
        x isa ParseFailure && return x
    else
        return INVALID
    end
    return neg ? -x : x
end

# Parse the bytes `buf[i:j]` as a `T` with the decimal point `decimal`, or return a
# `ParseFailure`. `i:j` must be in bounds.
@inline function parsefloat(::Type{T}, buf::AbstractVector{UInt8}, i::Int, j::Int,
                            decimal::UInt8) where {T<:IEEEFloat}
    i <= j || return INVALID
    b = @inbounds buf[i]
    neg = b == UInt8('-')
    if neg || b == UInt8('+')
        i += 1
        i <= j || return INVALID
        b = @inbounds buf[i]
    end
    b - UInt8('0') <= 0x09 || b == decimal || return parsespecial(T, buf, i, j, neg)
    b == UInt8('0') && i < j && (@inbounds buf[i + 1]) | 0x20 == UInt8('x') &&
        return parsehex(T, buf, i + 2, j, neg, decimal)
    # significand: w, with nd digits, of which -q follow the decimal point
    first = i
    w, i = scandigits(buf, i, j, zero(UInt64))
    dot = i                                      # the end of the integer digits
    nd = dot - first
    q = 0
    if i <= j && (@inbounds buf[i]) == decimal
        w, i = scandigits(buf, dot + 1, j, w)
        nd += i - dot - 1
        q = dot + 1 - i
    end
    nd > 0 || return INVALID
    last = i - 1                                 # the end of the significand
    if i <= j && (@inbounds buf[i]) | 0x20 == UInt8('e')
        e, i, ok = scanexponent(buf, i + 1, j)
        ok || return INVALID
        q += e
    end
    i > j || return INVALID
    if nd > 19
        # leading zeros are not significant
        k = first
        while k <= last && (@inbounds buf[k]) in (UInt8('0'), decimal)
            (@inbounds buf[k]) == UInt8('0') && (nd -= 1)
            k += 1
        end
        if nd > 19
            # The value lies between w and w + 1 times 10^(q + nd - 19), for the first 19
            # significant digits w. When both round to the same float, so does the value;
            # otherwise it rounds to the one on its side of the midpoint between them.
            n = clamp(dot - k, 0, 19)              # the digits of w before the point
            w, _ = scandigits(buf, k, k + n - 1, zero(UInt64))
            f = max(k, dot + 1)
            w, _ = scandigits(buf, f, f + 18 - n, w)
            bits = eisellemire(T, w, q + nd - 19)
            if bits != eisellemire(T, w + 1, q + nd - 19)
                c = cmpmidpoint(T, buf, k, last, q, bits)
                bits += c > 0 || (c == 0 && isodd(bits))
            end
            return frombits(T, bits, neg)
        end
    end
    return fromdecimal(T, w, q, neg)
end

@inline function parsevalue(::Type{T}, bytes::AbstractVector{UInt8};
                            decimal::AbstractChar = '.') where {T<:IEEEFloat}
    require_one_based_indexing(bytes)
    # a character outside ASCII is rejected along with the letters, digits and signs, which
    # could be read as part of the number
    d = isascii(decimal) ? UInt8(decimal) : UInt8('0')
    if d - UInt8('0') <= 0x09 || (d | 0x20) - UInt8('a') <= 0x19 || d == UInt8('+') ||
            d == UInt8('-')
        throw(ArgumentError(LazyString("invalid decimal: decimal must be an ASCII ",
            "character other than a letter, digit, `+` or `-`, got ", repr(decimal))))
    end
    return parsefloat(T, bytes, 1, length(bytes), d)
end
