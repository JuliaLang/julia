## efficient value-based hashing of integers ##

function hash_integer(n::Integer, h::Uint)
    h = hash_uint(uint(n & typemax(Uint)) $ h) $ h
    n = ifelse(n < 0, oftype(n,-n), n)
    n >>>= sizeof(Uint) << 3
    while n != 0
        h = hash_uint(uint(n & typemax(Uint)) $ h) $ h
        n >>>= sizeof(Uint) << 3
    end
    return h
end

function hash_integer(n::BigInt, h::Uint)
    s = n.size
    s == 0 && return hash_integer(0, h)
    p = convert(Ptr{Uint}, n.d)
    b = unsafe_load(p)
    h = hash_uint(ifelse(s < 0, -b, b) $ h) $ h
    for k = 2:abs(s)
        h = hash_uint(unsafe_load(p, k) $ h) $ h
    end
    return h
end

## generic hashing for rational values ##

function hash(x::Real, h::Uint)
    # decompose x as num*2^pow/den
    num, pow, den = decompose(x)

    # handle special values
    num == 0 && den == 0 && return hash(NaN, h)
    num == 0 && return hash(ifelse(den > 0, 0.0, -0.0), h)
    den == 0 && return hash(ifelse(num > 0, Inf, -Inf), h)

    # normalize decomposition
    if den < 0
        num = -num
        den = -den
    end
    z = trailing_zeros(num)
    if z != 0
        num >>= z
        pow += z
    end
    z = trailing_zeros(den)
    if z != 0
        den >>= z
        pow -= z
    end

    # handle values representable as Int64, Uint64, Float64
    if den == 1
        left = ndigits0z(num,2) + pow
        right = trailing_zeros(num) + pow
        if -1074 <= right
            if 0 <= right && left <= 64
                left <= 63                     && return hash(int64(num) << int(pow), h)
                signbit(num) == signbit(den)   && return hash(uint64(num) << int(pow), h)
            end # typemin(Int64) handled by Float64 case
            left <= 1024 && left - right <= 53 && return hash(float64(num) * 2.0^pow, h)
        end
    end

    # handle generic rational values
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

#=
`decompose(x)`: non-canonical decomposition of rational values as `num*2^pow/den`.

The decompose function is the point where rational-valued numeric types that support
hashing hook into the hashing protocol. `decompose(x)` should return three integer
values `num, pow, den`, such that the value of `x` is mathematically equal to

    num*2^pow/den

The decomposition need not be canonical in the sense that it just needs to be *some*
way to express `x` in this form, not any particular way – with the restriction that
`num` and `den` may not share any odd common factors. They may, however, have powers
of two in common – the generic hashing code will normalize those as necessary.

Special values:

 - `x` is zero: `num` should be zero and `den` should have the same sign as `x`
 - `x` is infinite: `den` should be zero and `num` should have the same sign as `x`
 - `x` is not a number: `num` and `den` should both be zero
=#

decompose(x::Integer) = x, 0, 1
decompose(x::Rational) = num(x), 0, den(x)

function decompose(x::Float32)
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(Int32, x)
    s = int32(n & 0x007fffff)
    e = int32(n & 0x7f800000 >> 23)
    s |= int32(e != 0) << 23
    d = ifelse(signbit(n) == 1, -1, 1)
    int(s), int(e - 150 + (e == 0)), d
end

function decompose(x::Float64)
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(Int64, x)
    s = int64(n & 0x000fffffffffffff)
    e = int64(n & 0x7ff0000000000000 >> 52)
    s |= int64(e != 0) << 52
    d = ifelse(signbit(n) == 1, -1, 1)
    int(s), int(e - 1075 + (e == 0)), d
end

function decompose(x::BigFloat)
    isnan(x) && return big(0), 0, 0
    isinf(x) && return big(x.sign), 0, 0
    x == 0 && return big(0), 0, int(x.sign)
    s = BigInt()
    ccall((:__gmpz_realloc2, :libgmp), Void, (Ptr{BigInt}, Culong), &s, x.prec)
    s.size = -fld(-x.prec,(sizeof(Culong)<<3))
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t), s.d, x.d, s.size*sizeof(Culong))
    s, int(x.exp - x.prec), int(x.sign)
end

## streamlined hashing for smallish rational types ##

function hash{T<:Integer64}(x::Rational{T}, h::Uint)
    num, den = Base.num(x), Base.den(x)
    den == 1 && return hash(num, h)
    den == 0 && return hash(ifelse(num > 0, Inf, -Inf), h)
    if isodd(den)
        pow = trailing_zeros(num)
        num >>= pow
    else
        pow = trailing_zeros(den)
        den >>= pow
        pow = -pow
        if den == 1 && abs(num) < 9007199254740992
            return hash(float64(num) * 2.0^pow)
        end
    end
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

## hashing Float16s ##

hash(x::Float16, h::Uint) = hash(float64(x), h)

## hashing strings ##

const memhash = Uint == Uint64 ? :memhash_seed : :memhash32_seed

function hash{T<:ByteString}(s::Union(T,SubString{T}), h::Uint)
    h += uint(0x71e729fd56419c81)
    ccall(memhash, Uint, (Ptr{Uint8}, Csize_t, Uint32), pointer(s), sizeof(s), h) + h
end
hash(s::String, h::Uint) = hash(bytestring(s), h)

## hashing collections ##

function hash(v::Union(Tuple,AbstractArray,Associative), h::Uint)
    h += object_id(eltype(v))
    for x = v
        h = hash(x, h)
    end
    return h
end

hash(s::Set, h::Uint) = hash(sort(s.dict.keys[s.dict.slots .!= 0]), h)

hash(r::Range{Bool}, h::Uint) = invoke(hash, (Range, Uint), r, h)
hash(B::BitArray, h::Uint) = hash((size(B),B.chunks), h)
hash(a::AbstractArray{Bool}, h::Uint) = hash(bitpack(a), h)

# hashing ranges by component at worst leads to collisions for very similar ranges
function hash{T<:Range}(r::T, h::Uint)
    h += uint(0x80707b6821b70087)
    h = hash(first(r), h)
    h = hash(step(r), h)
    h = hash(last(r), h)
end

## hashing general objects ##

hash(x::ANY,  h::Uint) = hash(object_id(x), h)
