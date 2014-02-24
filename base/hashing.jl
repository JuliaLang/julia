## core data hashing functions ##

function hash_uint(n::Uint64)
    local a::Uint64 = n
    a = ~a + a << 21
    a =  a $ a >> 24
    a =  a + a << 3 + a << 8
    a =  a $ a >> 14
    a =  a + a << 2 + a << 4
    a =  a $ a >> 28
    a =  a + a << 31
    return a
end

function hash_uint(n::Uint32)
    local a::Uint32 = n
    a = a + 0x7ed55d16 + a << 12
    a = a $ 0xc761c23c $ a >> 19
    a = a + 0x165667b1 + a << 5
    a = a + 0xd3a2646c $ a << 9
    a = a + 0xfd7046c5 + a << 3
    a = a $ 0xb55a4f09 $ a >> 16
    return a
end

## efficient value-based hashing of integers ##

function hash_integer(n::Integer, h::Uint=zero(Uint))
    h = hash_uint(uint(n & typemax(Uint)) $ h) $ h
    n = ifelse(n < 0, oftype(n,-n), n)
    n >>>= sizeof(Uint) << 3
    while n != 0
        h = hash_uint(uint(n & typemax(Uint)) $ h) $ h
        n >>>= sizeof(Uint) << 3
    end
    return h
end

## hashing rational values ##

#=
`decompose(x)`: non-canonical decomposition of rational values as `den*2^pow/num`.

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

# hashing methods for rational-valued types

hx(a::Uint64, b::Float64, h::Uint) = hash_uint((3a + reinterpret(Uint64,b)) - h)

hash(x::Uint64,  h::Uint=zero(Uint)) = hx(x, float64(x), h)
hash(x::Int64,   h::Uint=zero(Uint)) = hx(reinterpret(Uint64,x), float64(x), h)
hash(x::Float64, h::Uint=zero(Uint)) = hx(box(Uint64,fptosi(unbox(Float64,x))), ifelse(x==x,x,NaN), h)

hash(x::Union(Int8,Uint8,Int16,Uint16,Int32,Uint32)) = hash(int64(x))
hash(x::Union(Float16,Float32)) = hash(float64(x))

const hash_NaN = hash(NaN)
const hash_pos_Inf = hash(+Inf)
const hash_neg_Inf = hash(-Inf)
const hash_pos_zero = hash(+0.)
const hash_neg_zero = hash(-0.)

function hash(x::Real, h::Uint=zero(Uint))
    # decompose x as num*2^pow/den
    num, pow, den = decompose(x)::(Integer,Integer,Integer)

    # handle special values
    num == 0 && den == 0 && return hash(NaN, h)
    if num == 0
        den > 0 && return hash(+0.0, h)
        den < 0 && return hash(-0.0, h)
    end
    if den == 0
        num > 0 && return hash(+Inf, h)
        num < 0 && return hash(-Inf, h)
    end

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
            end
            left <= 1024 && left - right <= 53 && return hash(float64(num) * 2.0^pow, h)
        end
    end

    # handle "generic" real values
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

## hashing complex values ##

const h_imag = 0x32a7a07f3e7cd1f9
const hash_0_imag = hash(0, h_imag)

function hash(z::Complex, h::Uint=zero(Uint))
    # TODO: with default argument specialization, this would be better:
    # hash(real(z), h $ hash(imag(z), h $ h_imag) $ hash(0, h $ h_imag))
    hash(real(z), h $ hash(imag(z), h_imag) $ hash_0_imag)
end

## special hashing for booleans and characters ##

hash(x::Bool, h::Uint=zero(Uint)) = hash(int(x), h + 0x4cd135a1755139a5)
hash(x::Char, h::Uint=zero(Uint)) = hash(int(x), h + 0x10f989ff0f886f11)

## expression hashing ##

hash(x::Symbol, h::Uint=zero(Uint)) = hash(object_id(x), h)
hash(x::Expr, h::Uint=zero(Uint)) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
