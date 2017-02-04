# This file is a part of Julia. License is MIT: http://julialang.org/license

## integer arithmetic ##

# The tuples and types that do not include 128 bit sizes are necessary to handle
# certain issues on 32-bit machines, and also to simplify promotion rules, as
# they are also used elsewhere where Int128/UInt128 support is separated out,
# such as in hashing2.jl

const BitSigned64_types   = (Int8, Int16, Int32, Int64)
const BitUnsigned64_types = (UInt8, UInt16, UInt32, UInt64)
const BitInteger64_types  = (BitSigned64_types..., BitUnsigned64_types...)
const BitSigned_types     = (BitSigned64_types..., Int128)
const BitUnsigned_types   = (BitUnsigned64_types..., UInt128)
const BitInteger_types    = (BitSigned_types..., BitUnsigned_types...)

typealias BitSigned64   Union{BitSigned64_types...}
typealias BitUnsigned64 Union{BitUnsigned64_types...}
typealias BitInteger64  Union{BitInteger64_types...}
typealias BitSigned     Union{BitSigned_types...}
typealias BitUnsigned   Union{BitUnsigned_types...}
typealias BitInteger    Union{BitInteger_types...}
typealias BitSigned64T  Union{Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}}
typealias BitUnsigned64T Union{Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}}

## integer comparisons ##

<{T<:BitSigned}(x::T, y::T)  = slt_int(x, y)

-{T<:BitInteger}(x::T)       = neg_int(x)
-{T<:BitInteger}(x::T, y::T) = sub_int(x, y)
+{T<:BitInteger}(x::T, y::T) = add_int(x, y)
*{T<:BitInteger}(x::T, y::T) = mul_int(x, y)

inv(x::Integer) = float(one(x)) / float(x)
/{T<:Integer}(x::T, y::T) = float(x) / float(y)
# skip promotion for system integer types
/(x::BitInteger, y::BitInteger) = float(x) / float(y)

"""
    isodd(x::Integer) -> Bool

Returns `true` if `x` is odd (that is, not divisible by 2), and `false` otherwise.

```jldoctest
julia> isodd(9)
true

julia> isodd(10)
false
```
"""
isodd(n::Integer) = rem(n, 2) != 0

"""
    iseven(x::Integer) -> Bool

Returns `true` is `x` is even (that is, divisible by 2), and `false` otherwise.

```jldoctest
julia> iseven(9)
false

julia> iseven(10)
true
```
"""
iseven(n::Integer) = !isodd(n)

signbit(x::Integer) = x < 0
signbit(x::Unsigned) = false

flipsign{T<:BitSigned}(x::T, y::T) = flipsign_int(x, y)

flipsign(x::Signed, y::Signed)  = convert(typeof(x), flipsign(promote_noncircular(x, y)...))
flipsign(x::Signed, y::Float16) = flipsign(x, bitcast(Int16, y))
flipsign(x::Signed, y::Float32) = flipsign(x, bitcast(Int32, y))
flipsign(x::Signed, y::Float64) = flipsign(x, bitcast(Int64, y))
flipsign(x::Signed, y::Real)    = flipsign(x, -oftype(x, signbit(y)))

copysign(x::Signed, y::Signed)  = flipsign(x, x ⊻ y)
copysign(x::Signed, y::Float16) = copysign(x, bitcast(Int16, y))
copysign(x::Signed, y::Float32) = copysign(x, bitcast(Int32, y))
copysign(x::Signed, y::Float64) = copysign(x, bitcast(Int64, y))
copysign(x::Signed, y::Real)    = copysign(x, -oftype(x, signbit(y)))

"""
    abs(x)

The absolute value of `x`.

When `abs` is applied to signed integers, overflow may occur,
resulting in the return of a negative value. This overflow occurs only
when `abs` is applied to the minimum representable value of a signed
integer. That is, when `x == typemin(typeof(x))`, `abs(x) == x < 0`,
not `-x` as might be expected.
"""
function abs end

abs(x::Unsigned) = x
abs(x::Signed) = flipsign(x,x)

~(n::Integer) = -n-1

unsigned(x::Signed) = reinterpret(typeof(convert(Unsigned, zero(x))), x)
unsigned(x::Bool) = convert(Unsigned, x)
unsigned(x) = convert(Unsigned, x)
signed(x::Unsigned) = reinterpret(typeof(convert(Signed, zero(x))), x)
signed(x) = convert(Signed, x)

div(x::Signed, y::Unsigned) = flipsign(signed(div(unsigned(abs(x)), y)), x)
div(x::Unsigned, y::Signed) = unsigned(flipsign(signed(div(x, unsigned(abs(y)))), y))

rem(x::Signed, y::Unsigned) = flipsign(signed(rem(unsigned(abs(x)), y)), x)
rem(x::Unsigned, y::Signed) = rem(x, unsigned(abs(y)))

fld(x::Signed, y::Unsigned) = div(x, y) - (signbit(x) & (rem(x, y) != 0))
fld(x::Unsigned, y::Signed) = div(x, y) - (signbit(y) & (rem(x, y) != 0))


"""
    mod(x, y)

Modulus after flooring division, returning in the range ``[0,y)``, if `y` is positive, or
``(y,0]`` if `y` is negative.

```julia
x == fld(x,y)*y + mod(x,y)
```
"""
function mod{T<:Integer}(x::T, y::T)
    y == -1 && return T(0)   # avoid potential overflow in fld
    return x - fld(x, y) * y
end
mod(x::Signed, y::Unsigned) = rem(y + unsigned(rem(x, y)), y)
mod(x::Unsigned, y::Signed) = rem(y + signed(rem(x, y)), y)
mod{T<:Unsigned}(x::T, y::T) = rem(x, y)

cld(x::Signed, y::Unsigned) = div(x, y) + (!signbit(x) & (rem(x, y) != 0))
cld(x::Unsigned, y::Signed) = div(x, y) + (!signbit(y) & (rem(x, y) != 0))

# Don't promote integers for div/rem/mod since there is no danger of overflow,
# while there is a substantial performance penalty to 64-bit promotion.
div{T<:BitSigned64}(x::T, y::T) = checked_sdiv_int(x, y)
rem{T<:BitSigned64}(x::T, y::T) = checked_srem_int(x, y)
div{T<:BitUnsigned64}(x::T, y::T) = checked_udiv_int(x, y)
rem{T<:BitUnsigned64}(x::T, y::T) = checked_urem_int(x, y)


# fld(x,y) == div(x,y) - ((x>=0) != (y>=0) && rem(x,y) != 0 ? 1 : 0)
fld{T<:Unsigned}(x::T, y::T) = div(x,y)
function fld{T<:Integer}(x::T, y::T)
    d = div(x, y)
    return d - (signbit(x ⊻ y) & (d * y != x))
end

# cld(x,y) = div(x,y) + ((x>0) == (y>0) && rem(x,y) != 0 ? 1 : 0)
function cld{T<:Unsigned}(x::T, y::T)
    d = div(x, y)
    return d + (d * y != x)
end
function cld{T<:Integer}(x::T, y::T)
    d = div(x, y)
    return d + (((x > 0) == (y > 0)) & (d * y != x))
end

## integer bitwise operations ##

(~){T<:BitInteger}(x::T)       = not_int(x)
(&){T<:BitInteger}(x::T, y::T) = and_int(x, y)
(|){T<:BitInteger}(x::T, y::T) = or_int(x, y)
xor{T<:BitInteger}(x::T, y::T) = xor_int(x, y)

bswap{T<:Union{Int8, UInt8}}(x::T) = x
bswap{T<:Union{Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128}}(x::T) =
    bswap_int(x)

"""
    count_ones(x::Integer) -> Integer

Number of ones in the binary representation of `x`.

```jldoctest
julia> count_ones(7)
3
```
"""
count_ones{T<:BitInteger}(x::T) = Int(ctpop_int(x))

"""
    leading_zeros(x::Integer) -> Integer

Number of zeros leading the binary representation of `x`.

```jldoctest
julia> leading_zeros(Int32(1))
31
```
"""
leading_zeros{T<:BitInteger}(x::T) = Int(ctlz_int(x))

"""
    trailing_zeros(x::Integer) -> Integer

Number of zeros trailing the binary representation of `x`.

```jldoctest
julia> trailing_zeros(2)
1
```
"""
trailing_zeros{T<:BitInteger}(x::T) = Int(cttz_int(x))

"""
    count_zeros(x::Integer) -> Integer

Number of zeros in the binary representation of `x`.

```jldoctest
julia> count_zeros(Int32(2 ^ 16 - 1))
16
```
"""
count_zeros(  x::Integer) = count_ones(~x)

"""
    leading_ones(x::Integer) -> Integer

Number of ones leading the binary representation of `x`.

```jldoctest
julia> leading_ones(UInt32(2 ^ 32 - 2))
31
```
"""
leading_ones( x::Integer) = leading_zeros(~x)

"""
    trailing_ones(x::Integer) -> Integer

Number of ones trailing the binary representation of `x`.

```jldoctest
julia> trailing_ones(3)
2
```
"""
trailing_ones(x::Integer) = trailing_zeros(~x)

## integer comparisons ##

<{T<:BitUnsigned}(x::T, y::T)  = ult_int(x, y)
<={T<:BitSigned}(x::T, y::T)   = sle_int(x, y)
<={T<:BitUnsigned}(x::T, y::T) = ule_int(x, y)

==(x::Signed,   y::Unsigned) = (x >= 0) & (unsigned(x) == y)
==(x::Unsigned, y::Signed  ) = (y >= 0) & (x == unsigned(y))
<( x::Signed,   y::Unsigned) = (x <  0) | (unsigned(x) <  y)
<( x::Unsigned, y::Signed  ) = (y >= 0) & (x <  unsigned(y))
<=(x::Signed,   y::Unsigned) = (x <  0) | (unsigned(x) <= y)
<=(x::Unsigned, y::Signed  ) = (y >= 0) & (x <= unsigned(y))

## integer shifts ##

# unsigned shift counts always shift in the same direction
>>{T<:BitSigned,S<:BitUnsigned}(x::T, y::S) = ashr_int(x, y)
>>{T<:BitUnsigned,S<:BitUnsigned}(x::T, y::S) = lshr_int(x, y)
<<{T<:BitInteger,S<:BitUnsigned}(x::T,  y::S) = shl_int(x, y)
>>>{T<:BitInteger,S<:BitUnsigned}(x::T, y::S) = lshr_int(x, y)
# signed shift counts can shift in either direction
# note: this early during bootstrap, `>=` is not yet available
# note: we only define Int shift counts here; the generic case is handled later
>>(x::BitInteger, y::Int) =
    select_value(0 <= y, x >> unsigned(y), x << unsigned(-y))
<<(x::BitInteger, y::Int) =
    select_value(0 <= y, x << unsigned(y), x >> unsigned(-y))
>>>(x::BitInteger, y::Int) =
    select_value(0 <= y, x >>> unsigned(y), x << unsigned(-y))

## integer conversions ##

for to in BitInteger_types, from in (BitInteger_types..., Bool)
    if !(to === from)
        if to.size < from.size
            if issubtype(to, Signed)
                if issubtype(from, Unsigned)
                    @eval convert(::Type{$to}, x::($from)) =
                        checked_trunc_sint($to, check_top_bit(x))
                else
                    @eval convert(::Type{$to}, x::($from)) =
                        checked_trunc_sint($to, x)
                end
            else
                @eval convert(::Type{$to}, x::($from)) =
                    checked_trunc_uint($to, x)
            end
            @eval rem(x::($from), ::Type{$to}) = trunc_int($to, x)
        elseif from.size < to.size || from === Bool
            if issubtype(from, Signed)
                if issubtype(to, Unsigned)
                    @eval convert(::Type{$to}, x::($from)) =
                        sext_int($to, check_top_bit(x))
                else
                    @eval convert(::Type{$to}, x::($from)) =
                        sext_int($to, x)
                end
                @eval rem(x::($from), ::Type{$to}) = sext_int($to, x)
            else
                @eval convert(::Type{$to}, x::($from)) = zext_int($to, x)
                @eval rem(x::($from), ::Type{$to}) = convert($to, x)
            end
        else
            if !(issubtype(from, Signed) === issubtype(to, Signed))
                # raise InexactError if x's top bit is set
                @eval convert(::Type{$to}, x::($from)) = bitcast($to, check_top_bit(x))
            else
                @eval convert(::Type{$to}, x::($from)) = bitcast($to, x)
            end
            @eval rem(x::($from), ::Type{$to}) = bitcast($to, x)
        end
    end
end

rem{T<:Integer}(x::T, ::Type{T}) = x
rem(x::Integer, ::Type{Bool}) = ((x & 1) != 0)
mod{T<:Integer}(x::Integer, ::Type{T}) = rem(x, T)

unsafe_trunc{T<:Integer}(::Type{T}, x::Integer) = rem(x, T)
for (Ts, Tu) in ((Int8, UInt8), (Int16, UInt16), (Int32, UInt32), (Int64, UInt64), (Int128, UInt128))
    @eval convert(::Type{Signed}, x::$Tu) = convert($Ts, x)
    @eval convert(::Type{Unsigned}, x::$Ts) = convert($Tu, x)
end

convert{T<:Union{Float32, Float64, Bool}}(::Type{Signed}, x::T) = convert(Int, x)
convert{T<:Union{Float32, Float64, Bool}}(::Type{Unsigned}, x::T) = convert(UInt, x)

convert(::Type{Integer}, x::Integer) = x
convert(::Type{Integer}, x::Real) = convert(Signed, x)

round(x::Integer) = x
trunc(x::Integer) = x
floor(x::Integer) = x
 ceil(x::Integer) = x

round{T<:Integer}(::Type{T}, x::Integer) = convert(T, x)
trunc{T<:Integer}(::Type{T}, x::Integer) = convert(T, x)
floor{T<:Integer}(::Type{T}, x::Integer) = convert(T, x)
 ceil{T<:Integer}(::Type{T}, x::Integer) = convert(T, x)

## integer construction ##

macro int128_str(s)
    return parse(Int128, s)
end

macro uint128_str(s)
    return parse(UInt128, s)
end

macro big_str(s)
    n = tryparse(BigInt, s)
    !isnull(n) && return get(n)
    n = tryparse(BigFloat, s)
    !isnull(n) && return get(n)
    message = "invalid number format $s for BigInt or BigFloat"
    return :(throw(ArgumentError($message)))
end

## integer promotions ##

promote_rule(::Type{Int8}, ::Type{Int16})   = Int16
promote_rule(::Type{UInt8}, ::Type{UInt16}) = UInt16
promote_rule{T<:Union{Int8,Int16}}(::Type{Int32}, ::Type{T})    = Int32
promote_rule{T<:Union{UInt8,UInt16}}(::Type{UInt32}, ::Type{T}) = UInt32
promote_rule{T<:Union{Int8,Int16,Int32}}(::Type{Int64}, ::Type{T})     = Int64
promote_rule{T<:Union{UInt8,UInt16,UInt32}}(::Type{UInt64}, ::Type{T}) = UInt64
promote_rule{T<:BitSigned64}(::Type{Int128}, ::Type{T})    = Int128
promote_rule{T<:BitUnsigned64}(::Type{UInt128}, ::Type{T}) = UInt128
for T in BitSigned_types
    @eval promote_rule{S<:Union{UInt8,UInt16}}(::Type{S}, ::Type{$T}) =
        $(sizeof(T) < sizeof(Int) ? Int : T)
end
@eval promote_rule{T<:Union{Int8,Int16,Int32}}(::Type{UInt32}, ::Type{T}) =
    $(Core.sizeof(Int) == 8 ? Int : UInt)
promote_rule(::Type{UInt32}, ::Type{Int64}) = Int64
promote_rule{T<:BitSigned64}(::Type{UInt64}, ::Type{T}) = UInt64
promote_rule{T<:Union{UInt32, UInt64}}(::Type{T}, ::Type{Int128}) = Int128
promote_rule{T<:BitSigned}(::Type{UInt128}, ::Type{T}) = UInt128

_default_type(T::Type{Unsigned}) = UInt
_default_type(T::Union{Type{Integer},Type{Signed}}) = Int

## traits ##

typemin(::Type{Int8  }) = Int8(-128)
typemax(::Type{Int8  }) = Int8(127)
typemin(::Type{UInt8 }) = UInt8(0)
typemax(::Type{UInt8 }) = UInt8(255)
typemin(::Type{Int16 }) = Int16(-32768)
typemax(::Type{Int16 }) = Int16(32767)
typemin(::Type{UInt16}) = UInt16(0)
typemax(::Type{UInt16}) = UInt16(65535)
typemin(::Type{Int32 }) = Int32(-2147483648)
typemax(::Type{Int32 }) = Int32(2147483647)
typemin(::Type{UInt32}) = UInt32(0)
typemax(::Type{UInt32}) = UInt32(4294967295)
typemin(::Type{Int64 }) = -9223372036854775808
typemax(::Type{Int64 }) = 9223372036854775807
typemin(::Type{UInt64}) = UInt64(0)
typemax(::Type{UInt64}) = 0xffffffffffffffff
@eval typemin(::Type{UInt128}) = $(convert(UInt128, 0))
@eval typemax(::Type{UInt128}) = $(bitcast(UInt128, convert(Int128, -1)))
@eval typemin(::Type{Int128} ) = $(convert(Int128, 1) << 127)
@eval typemax(::Type{Int128} ) = $(bitcast(Int128, typemax(UInt128) >> 1))

widen{T<:Union{Int8, Int16}}(::Type{T}) = Int32
widen(::Type{Int32}) = Int64
widen(::Type{Int64}) = Int128
widen{T<:Union{UInt8, UInt16}}(::Type{T}) = UInt32
widen(::Type{UInt32}) = UInt64
widen(::Type{UInt64}) = UInt128

# a few special cases,
# Int64*UInt64 => Int128
# |x|<=2^(k-1), |y|<=2^k-1   =>   |x*y|<=2^(2k-1)-1
widemul(x::Signed,y::Unsigned) = widen(x) * signed(widen(y))
widemul(x::Unsigned,y::Signed) = signed(widen(x)) * widen(y)
# multplication by Bool doesn't require widening
widemul(x::Bool,y::Bool) = x * y
widemul(x::Bool,y::Number) = x * y
widemul(x::Number,y::Bool) = x * y


## wide multiplication, Int128 multiply and divide ##

if Core.sizeof(Int) == 4
    function widemul(u::Int64, v::Int64)
        local u0::UInt64, v0::UInt64, w0::UInt64
        local u1::Int64, v1::Int64, w1::UInt64, w2::Int64, t::UInt64

        u0 = u & 0xffffffff; u1 = u >> 32
        v0 = v & 0xffffffff; v1 = v >> 32
        w0 = u0 * v0
        t = reinterpret(UInt64, u1) * v0 + (w0 >>> 32)
        w2 = reinterpret(Int64, t) >> 32
        w1 = u0 * reinterpret(UInt64, v1) + (t & 0xffffffff)
        hi = u1 * v1 + w2 + (reinterpret(Int64, w1) >> 32)
        lo = w0 & 0xffffffff + (w1 << 32)
        return Int128(hi) << 64 + Int128(lo)
    end

    function widemul(u::UInt64, v::UInt64)
        local u0::UInt64, v0::UInt64, w0::UInt64
        local u1::UInt64, v1::UInt64, w1::UInt64, w2::UInt64, t::UInt64

        u0 = u & 0xffffffff; u1 = u >>> 32
        v0 = v & 0xffffffff; v1 = v >>> 32
        w0 = u0 * v0
        t = u1 * v0 + (w0 >>> 32)
        w2 = t >>> 32
        w1 = u0 * v1 + (t & 0xffffffff)
        hi = u1 * v1 + w2 + (w1 >>> 32)
        lo = w0 & 0xffffffff + (w1 << 32)
        return UInt128(hi) << 64 + UInt128(lo)
    end

    function *(u::Int128, v::Int128)
        u0 = u % UInt64; u1 = Int64(u >> 64)
        v0 = v % UInt64; v1 = Int64(v >> 64)
        lolo = widemul(u0, v0)
        lohi = widemul(reinterpret(Int64, u0), v1)
        hilo = widemul(u1, reinterpret(Int64, v0))
        t = reinterpret(UInt128, hilo) + (lolo >>> 64)
        w1 = reinterpret(UInt128, lohi) + (t & 0xffffffffffffffff)
        return Int128(lolo & 0xffffffffffffffff) + reinterpret(Int128, w1) << 64
    end

    function *(u::UInt128, v::UInt128)
        u0 = u % UInt64; u1 = UInt64(u>>>64)
        v0 = v % UInt64; v1 = UInt64(v>>>64)
        lolo = widemul(u0, v0)
        lohi = widemul(u0, v1)
        hilo = widemul(u1, v0)
        t = hilo + (lolo >>> 64)
        w1 = lohi + (t & 0xffffffffffffffff)
        return (lolo & 0xffffffffffffffff) + UInt128(w1) << 64
    end

    function div(x::Int128, y::Int128)
        (x == typemin(Int128)) & (y == -1) && throw(DivideError())
        return Int128(div(BigInt(x), BigInt(y)))
    end
    function div(x::UInt128, y::UInt128)
        return UInt128(div(BigInt(x), BigInt(y)))
    end

    function rem(x::Int128, y::Int128)
        return Int128(rem(BigInt(x), BigInt(y)))
    end
    function rem(x::UInt128, y::UInt128)
        return UInt128(rem(BigInt(x), BigInt(y)))
    end

    function mod(x::Int128, y::Int128)
        return Int128(mod(BigInt(x), BigInt(y)))
    end
else
    *{T<:Union{Int128,UInt128}}(x::T, y::T)  = mul_int(x, y)

    div(x::Int128,  y::Int128)  = checked_sdiv_int(x, y)
    div(x::UInt128, y::UInt128) = checked_udiv_int(x, y)

    rem(x::Int128,  y::Int128)  = checked_srem_int(x, y)
    rem(x::UInt128, y::UInt128) = checked_urem_int(x, y)
end
