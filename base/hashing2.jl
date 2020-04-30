# This file is a part of Julia. License is MIT: https://julialang.org/license

## efficient value-based hashing of integers ##

function hash_integer(n::Integer, h::UInt)
    h ⊻= hash_uint((n % UInt) ⊻ h)
    n = abs(n)
    n >>>= sizeof(UInt) << 3
    while n != 0
        h ⊻= hash_uint((n % UInt) ⊻ h)
        n >>>= sizeof(UInt) << 3
    end
    return h
end

# this condition is true most (all?) of the time, and in this case we can define
# an optimized version of the above hash_integer(::Integer, ::UInt) method for BigInt
if GMP.Limb === UInt
    # used e.g. for Rational{BigInt}
    function hash_integer(n::BigInt, h::UInt)
        GC.@preserve n begin
            s = n.size
            s == 0 && return hash_integer(0, h)
            p = convert(Ptr{UInt}, n.d)
            b = unsafe_load(p)
            h ⊻= hash_uint(ifelse(s < 0, -b, b) ⊻ h)
            for k = 2:abs(s)
                h ⊻= hash_uint(unsafe_load(p, k) ⊻ h)
            end
            return h
        end
    end
end

## generic hashing for rational values ##

function hash(x::Real, h::UInt)
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

    # handle values representable as Int64, UInt64, Float64
    if den == 1
        left = ndigits0z(num,2) + pow
        right = trailing_zeros(num) + pow
        if -1074 <= right
            if 0 <= right && left <= 64
                left <= 63                     && return hash(Int64(num) << Int(pow), h)
                signbit(num) == signbit(den)   && return hash(UInt64(num) << Int(pow), h)
            end # typemin(Int64) handled by Float64 case
            left <= 1024 && left - right <= 53 && return hash(ldexp(Float64(num),pow), h)
        end
    end

    # handle generic rational values
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

## streamlined hashing for BigInt, by avoiding allocation from shifts ##

if GMP.Limb === UInt
    _divLimb(n) = UInt === UInt64 ? n >>> 6 : n >>> 5
    _modLimb(n) = UInt === UInt64 ? n & 63 : n & 31

    function hash(x::BigInt, h::UInt)
        GC.@preserve x begin
            sz = x.size
            sz == 0 && return hash(0, h)
            ptr = Ptr{UInt}(x.d)
            if sz == 1
                return hash(unsafe_load(ptr), h)
            elseif sz == -1
                limb = unsafe_load(ptr)
                limb <= typemin(Int) % UInt && return hash(-(limb % Int), h)
            end
            pow = trailing_zeros(x)
            nd = ndigits0z(x, 2)
            idx = _divLimb(pow) + 1
            shift = _modLimb(pow) % UInt
            upshift = GMP.BITS_PER_LIMB - shift
            asz = abs(sz)
            if shift == 0
                limb = unsafe_load(ptr, idx)
            else
                limb1 = unsafe_load(ptr, idx)
                limb2 = idx < asz ? unsafe_load(ptr, idx+1) : UInt(0)
                limb = limb2 << upshift | limb1 >> shift
            end
            if nd <= 1024 && nd - pow <= 53
                return hash(ldexp(flipsign(Float64(limb), sz), pow), h)
            end
            h = hash_integer(1, h)
            h = hash_integer(pow, h)
            h ⊻= hash_uint(flipsign(limb, sz) ⊻ h)
            for idx = idx+1:asz
                if shift == 0
                    limb = unsafe_load(ptr, idx)
                else
                    limb1 = limb2
                    if idx == asz
                        limb = limb1 >> shift
                        limb == 0 && break # don't hash leading zeros
                    else
                        limb2 = unsafe_load(ptr, idx+1)
                        limb = limb2 << upshift | limb1 >> shift
                    end
                end
                h ⊻= hash_uint(limb ⊻ h)
            end
            return h
        end
    end
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
of two in common – the generic hashing code will normalize those as necessary.

Special values:

 - `x` is zero: `num` should be zero and `den` should have the same sign as `x`
 - `x` is infinite: `den` should be zero and `num` should have the same sign as `x`
 - `x` is not a number: `num` and `den` should both be zero
=#

decompose(x::Integer) = x, 0, 1
decompose(x::Rational) = numerator(x), 0, denominator(x)

function decompose(x::Float16)::NTuple{3,Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt16, x)
    s = (n & 0x03ff) % Int16
    e = ((n & 0x7c00) >> 10) % Int
    s |= Int16(e != 0) << 10
    d = ifelse(signbit(x), -1, 1)
    s, e - 25 + (e == 0), d
end

function decompose(x::Float32)::NTuple{3,Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt32, x)
    s = (n & 0x007fffff) % Int32
    e = ((n & 0x7f800000) >> 23) % Int
    s |= Int32(e != 0) << 23
    d = ifelse(signbit(x), -1, 1)
    s, e - 150 + (e == 0), d
end

function decompose(x::Float64)::Tuple{Int64, Int, Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt64, x)
    s = (n & 0x000fffffffffffff) % Int64
    e = ((n & 0x7ff0000000000000) >> 52) % Int
    s |= Int64(e != 0) << 52
    d = ifelse(signbit(x), -1, 1)
    s, e - 1075 + (e == 0), d
end

function decompose(x::BigFloat)::Tuple{BigInt, Int, Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return x.sign, 0, 0
    x == 0 && return 0, 0, x.sign
    s = BigInt()
    s.size = cld(x.prec, 8*sizeof(GMP.Limb)) # limbs
    b = s.size * sizeof(GMP.Limb)            # bytes
    ccall((:__gmpz_realloc2, :libgmp), Cvoid, (Ref{BigInt}, Culong), s, 8b) # bits
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s.d, x.d, b) # bytes
    s, x.exp - 8b, x.sign
end

## streamlined hashing for smallish rational types ##

function hash(x::Rational{<:BitInteger64}, h::UInt)
    num, den = Base.numerator(x), Base.denominator(x)
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
            return hash(ldexp(Float64(num),pow),h)
        end
    end
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

## hashing Float16s ##

hash(x::Float16, h::UInt) = hash(Float64(x), h)

## hashing strings ##

const memhash = UInt === UInt64 ? :memhash_seed : :memhash32_seed
const memhash_seed = UInt === UInt64 ? 0x71e729fd56419c81 : 0x56419c81

function hash(s::Union{String,SubString{String}}, h::UInt)
    h += memhash_seed
    ccall(memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), s, sizeof(s), h % UInt32) + h
end
hash(s::AbstractString, h::UInt) = hash(String(s), h)
