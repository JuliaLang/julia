# This file is a part of Julia. License is MIT: https://julialang.org/license

## integer arithmetic ##

# The tuples and types that do not include 128 bit sizes are necessary to handle
# certain issues on 32-bit machines, and also to simplify promotion rules, as
# they are also used elsewhere where Int128/UInt128 support is separated out,
# such as in hashing2.jl

const BitSigned32_types      = (Int8, Int16, Int32)
const BitUnsigned32_types    = (UInt8, UInt16, UInt32)
const BitInteger32_types     = (BitSigned32_types..., BitUnsigned32_types...)

const BitSigned64_types      = (BitSigned32_types..., Int64)
const BitUnsigned64_types    = (BitUnsigned32_types..., UInt64)
const BitInteger64_types     = (BitSigned64_types..., BitUnsigned64_types...)

const BitSigned_types        = (BitSigned64_types..., Int128)
const BitUnsigned_types      = (BitUnsigned64_types..., UInt128)
const BitInteger_types       = (BitSigned_types..., BitUnsigned_types...)

const BitSignedSmall_types   = Int === Int64 ? ( Int8,  Int16,  Int32) : ( Int8,  Int16)
const BitUnsignedSmall_types = Int === Int64 ? (UInt8, UInt16, UInt32) : (UInt8, UInt16)
const BitIntegerSmall_types  = (BitSignedSmall_types..., BitUnsignedSmall_types...)

const BitSigned32      = Union{BitSigned32_types...}
const BitUnsigned32    = Union{BitUnsigned32_types...}
const BitInteger32     = Union{BitInteger32_types...}

const BitSigned64      = Union{BitSigned64_types...}
const BitUnsigned64    = Union{BitUnsigned64_types...}
const BitInteger64     = Union{BitInteger64_types...}

const BitSigned        = Union{BitSigned_types...}
const BitUnsigned      = Union{BitUnsigned_types...}
const BitInteger       = Union{BitInteger_types...}

const BitSignedSmall   = Union{BitSignedSmall_types...}
const BitUnsignedSmall = Union{BitUnsignedSmall_types...}
const BitIntegerSmall  = Union{BitIntegerSmall_types...}

const BitSigned64T     = Union{Type{Int8}, Type{Int16}, Type{Int32}, Type{Int64}}
const BitUnsigned64T   = Union{Type{UInt8}, Type{UInt16}, Type{UInt32}, Type{UInt64}}

const BitIntegerType = Union{map(T->Type{T}, BitInteger_types)...}

# the partner functions `unsigned(::Type{IntNN})` exist
"""
    signed(T::Integer)

Convert an integer bitstype to the signed type of the same size.
# Examples
```jldoctest
julia> signed(UInt16)
Int16
julia> signed(UInt64)
Int64
```
"""
signed(::Type{UInt8}) = Int8
signed(::Type{UInt16}) = Int16
signed(::Type{UInt32}) = Int32
signed(::Type{UInt64}) = Int64
signed(::Type{UInt128}) = Int128
signed(::Type{T}) where {T<:Signed} = T

## integer comparisons ##

(<)(x::T, y::T) where {T<:BitSigned}  = slt_int(x, y)

(-)(x::BitInteger)                    = neg_int(x)
(-)(x::T, y::T) where {T<:BitInteger} = sub_int(x, y)
(+)(x::T, y::T) where {T<:BitInteger} = add_int(x, y)
(*)(x::T, y::T) where {T<:BitInteger} = mul_int(x, y)

inv(x::Integer) = float(one(x)) / float(x)
(/)(x::T, y::T) where {T<:Integer} = float(x) / float(y)
# skip promotion for system integer types
(/)(x::BitInteger, y::BitInteger) = float(x) / float(y)

"""
    isodd(x::Integer) -> Bool

Return `true` if `x` is odd (that is, not divisible by 2), and `false` otherwise.

# Examples
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

Return `true` is `x` is even (that is, divisible by 2), and `false` otherwise.

# Examples
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

flipsign(x::T, y::T) where {T<:BitSigned} = flipsign_int(x, y)
flipsign(x::BitSigned, y::BitSigned) = flipsign_int(promote(x, y)...) % typeof(x)

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

# Examples
```jldoctest
julia> abs(-3)
3

julia> abs(1 + im)
1.4142135623730951

julia> abs(typemin(Int64))
-9223372036854775808
```
"""
function abs end

abs(x::Unsigned) = x
abs(x::Signed) = flipsign(x,x)

~(n::Integer) = -n-1

"""
    unsigned(x)
Convert a number to an unsigned integer. If the argument is signed, it is reinterpreted as
unsigned without checking for negative values.
# Examples
```jldoctest
julia> unsigned(-2)
0xfffffffffffffffe
julia> unsigned(2)
0x0000000000000002
julia> signed(unsigned(-2))
-2
```
"""
unsigned(x) = x % typeof(convert(Unsigned, zero(x)))
unsigned(x::BitSigned) = reinterpret(typeof(convert(Unsigned, zero(x))), x)

"""
    unsigned(T::Integer)
Convert an integer bitstype to the unsigned type of the same size.
# Examples
```jldoctest
julia> unsigned(Int16)
UInt16
julia> unsigned(UInt64)
UInt64
```
""" unsigned

"""
    signed(x)
Convert a number to a signed integer. If the argument is unsigned, it is reinterpreted as
signed without checking for overflow.
"""
signed(x) = x % typeof(convert(Signed, zero(x)))
signed(x::BitUnsigned) = reinterpret(typeof(convert(Signed, zero(x))), x)

div(x::BitSigned, y::Unsigned) = flipsign(signed(div(unsigned(abs(x)), y)), x)
div(x::Unsigned, y::BitSigned) = unsigned(flipsign(signed(div(x, unsigned(abs(y)))), y))

rem(x::BitSigned, y::Unsigned) = flipsign(signed(rem(unsigned(abs(x)), y)), x)
rem(x::Unsigned, y::BitSigned) = rem(x, unsigned(abs(y)))

function divrem(x::BitSigned, y::Unsigned)
    q, r = divrem(unsigned(abs(x)), y)
    flipsign(signed(q), x), flipsign(signed(r), x)
end

function divrem(x::Unsigned, y::BitSigned)
    q, r = divrem(x, unsigned(abs(y)))
    unsigned(flipsign(signed(q), y)), r
end


"""
    mod(x, y)
    rem(x, y, RoundDown)

The reduction of `x` modulo `y`, or equivalently, the remainder of `x` after floored
division by `y`, i.e. `x - y*fld(x,y)` if computed without intermediate rounding.

The result will have the same sign as `y`, and magnitude less than `abs(y)` (with some
exceptions, see note below).

!!! note

    When used with floating point values, the exact result may not be representable by the
    type, and so rounding error may occur. In particular, if the exact result is very
    close to `y`, then it may be rounded to `y`.

```jldoctest
julia> mod(8, 3)
2

julia> mod(9, 3)
0

julia> mod(8.9, 3)
2.9000000000000004

julia> mod(eps(), 3)
2.220446049250313e-16

julia> mod(-eps(), 3)
3.0
```
"""
function mod(x::T, y::T) where T<:Integer
    y == -1 && return T(0)   # avoid potential overflow in fld
    return x - fld(x, y) * y
end
mod(x::BitSigned, y::Unsigned) = rem(y + unsigned(rem(x, y)), y)
mod(x::Unsigned, y::Signed) = rem(y + signed(rem(x, y)), y)
mod(x::T, y::T) where {T<:Unsigned} = rem(x, y)

# Don't promote integers for div/rem/mod since there is no danger of overflow,
# while there is a substantial performance penalty to 64-bit promotion.
div(x::T, y::T) where {T<:BitSigned64} = checked_sdiv_int(x, y)
rem(x::T, y::T) where {T<:BitSigned64} = checked_srem_int(x, y)
div(x::T, y::T) where {T<:BitUnsigned64} = checked_udiv_int(x, y)
rem(x::T, y::T) where {T<:BitUnsigned64} = checked_urem_int(x, y)

## integer bitwise operations ##

"""
    ~(x)

Bitwise not.

# Examples
```jldoctest
julia> ~4
-5

julia> ~10
-11

julia> ~true
false
```
"""
(~)(x::BitInteger)             = not_int(x)

"""
    x & y

Bitwise and. Implements [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic),
returning [`missing`](@ref) if one operand is `missing` and the other is `true`.

# Examples
```jldoctest
julia> 4 & 10
0

julia> 4 & 12
4

julia> true & missing
missing

julia> false & missing
false
```
"""
(&)(x::T, y::T) where {T<:BitInteger} = and_int(x, y)

"""
    x | y

Bitwise or. Implements [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic),
returning [`missing`](@ref) if one operand is `missing` and the other is `false`.

# Examples
```jldoctest
julia> 4 | 10
14

julia> 4 | 1
5

julia> true | missing
true

julia> false | missing
missing
```
"""
(|)(x::T, y::T) where {T<:BitInteger} = or_int(x, y)
xor(x::T, y::T) where {T<:BitInteger} = xor_int(x, y)

"""
    bswap(n)

Reverse the byte order of `n`.

(See also [`ntoh`](@ref) and [`hton`](@ref) to convert between the current native byte order and big-endian order.)

# Examples
```jldoctest
julia> a = bswap(0x10203040)
0x40302010

julia> bswap(a)
0x10203040

julia> string(1, base = 2)
"1"

julia> string(bswap(1), base = 2)
"100000000000000000000000000000000000000000000000000000000"
```
"""
bswap(x::Union{Int8, UInt8}) = x
bswap(x::Union{Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128}) =
    bswap_int(x)

"""
    count_ones(x::Integer) -> Integer

Number of ones in the binary representation of `x`.

# Examples
```jldoctest
julia> count_ones(7)
3
```
"""
count_ones(x::BitInteger) = ctpop_int(x) % Int

"""
    leading_zeros(x::Integer) -> Integer

Number of zeros leading the binary representation of `x`.

# Examples
```jldoctest
julia> leading_zeros(Int32(1))
31
```
"""
leading_zeros(x::BitInteger) = ctlz_int(x) % Int

"""
    trailing_zeros(x::Integer) -> Integer

Number of zeros trailing the binary representation of `x`.

# Examples
```jldoctest
julia> trailing_zeros(2)
1
```
"""
trailing_zeros(x::BitInteger) = cttz_int(x) % Int

"""
    count_zeros(x::Integer) -> Integer

Number of zeros in the binary representation of `x`.

# Examples
```jldoctest
julia> count_zeros(Int32(2 ^ 16 - 1))
16
```
"""
count_zeros(x::Integer) = count_ones(~x)

"""
    leading_ones(x::Integer) -> Integer

Number of ones leading the binary representation of `x`.

# Examples
```jldoctest
julia> leading_ones(UInt32(2 ^ 32 - 2))
31
```
"""
leading_ones(x::Integer) = leading_zeros(~x)

"""
    trailing_ones(x::Integer) -> Integer

Number of ones trailing the binary representation of `x`.

# Examples
```jldoctest
julia> trailing_ones(3)
2
```
"""
trailing_ones(x::Integer) = trailing_zeros(~x)

## integer comparisons ##

(< )(x::T, y::T) where {T<:BitUnsigned} = ult_int(x, y)
(<=)(x::T, y::T) where {T<:BitSigned}   = sle_int(x, y)
(<=)(x::T, y::T) where {T<:BitUnsigned} = ule_int(x, y)

==(x::BitSigned,   y::BitUnsigned) = (x >= 0) & (unsigned(x) == y)
==(x::BitUnsigned, y::BitSigned  ) = (y >= 0) & (x == unsigned(y))
<( x::BitSigned,   y::BitUnsigned) = (x <  0) | (unsigned(x) <  y)
<( x::BitUnsigned, y::BitSigned  ) = (y >= 0) & (x <  unsigned(y))
<=(x::BitSigned,   y::BitUnsigned) = (x <  0) | (unsigned(x) <= y)
<=(x::BitUnsigned, y::BitSigned  ) = (y >= 0) & (x <= unsigned(y))

## integer shifts ##

# unsigned shift counts always shift in the same direction
>>(x::BitSigned,   y::BitUnsigned) = ashr_int(x, y)
>>(x::BitUnsigned, y::BitUnsigned) = lshr_int(x, y)
<<(x::BitInteger,  y::BitUnsigned) = shl_int(x, y)
>>>(x::BitInteger, y::BitUnsigned) = lshr_int(x, y)
# signed shift counts can shift in either direction
# note: this early during bootstrap, `>=` is not yet available
# note: we only define Int shift counts here; the generic case is handled later
>>(x::BitInteger, y::Int) =
    ifelse(0 <= y, x >> unsigned(y), x << unsigned(-y))
<<(x::BitInteger, y::Int) =
    ifelse(0 <= y, x << unsigned(y), x >> unsigned(-y))
>>>(x::BitInteger, y::Int) =
    ifelse(0 <= y, x >>> unsigned(y), x << unsigned(-y))

for to in BitInteger_types, from in (BitInteger_types..., Bool)
    if !(to === from)
        if to.size < from.size
            @eval rem(x::($from), ::Type{$to}) = trunc_int($to, x)
        elseif from === Bool
            @eval rem(x::($from), ::Type{$to}) = convert($to, x)
        elseif from.size < to.size
            if from <: Signed
                @eval rem(x::($from), ::Type{$to}) = sext_int($to, x)
            else
                @eval rem(x::($from), ::Type{$to}) = convert($to, x)
            end
        else
            @eval rem(x::($from), ::Type{$to}) = bitcast($to, x)
        end
    end
end

## integer bitwise rotations ##

"""
    bitrotate(x::Base.BitInteger, k::Integer)

`bitrotate(x, k)` implements bitwise rotation.
It returns the value of `x` with its bits rotated left `k` times.
A negative value of `k` will rotate to the right instead.

!!! compat "Julia 1.5"
    This function requires Julia 1.5 or later.

```jldoctest
julia> bitrotate(UInt8(114), 2)
0xc9

julia> bitstring(bitrotate(0b01110010, 2))
"11001001"

julia> bitstring(bitrotate(0b01110010, -2))
"10011100"

julia> bitstring(bitrotate(0b01110010, 8))
"01110010"
```
"""
bitrotate(x::T, k::Integer) where {T <: BitInteger} =
    (x << ((sizeof(T) << 3 - 1) & k)) | (x >>> ((sizeof(T) << 3 - 1) & -k))

# @doc isn't available when running in Core at this point.
# Tuple syntax for documentation two function signatures at the same time
# doesn't work either at this point.
if nameof(@__MODULE__) === :Base
    for fname in (:mod, :rem)
        @eval @doc """
            rem(x::Integer, T::Type{<:Integer}) -> T
            mod(x::Integer, T::Type{<:Integer}) -> T
            %(x::Integer, T::Type{<:Integer}) -> T

        Find `y::T` such that `x` ≡ `y` (mod n), where n is the number of integers representable
        in `T`, and `y` is an integer in `[typemin(T),typemax(T)]`.
        If `T` can represent any integer (e.g. `T == BigInt`), then this operation corresponds to
        a conversion to `T`.

        # Examples
        ```jldoctest
        julia> 129 % Int8
        -127
        ```
        """ $fname(x::Integer, T::Type{<:Integer})
    end
end

rem(x::T, ::Type{T}) where {T<:Integer} = x
rem(x::Signed, ::Type{Unsigned}) = x % unsigned(typeof(x))
rem(x::Unsigned, ::Type{Signed}) = x % signed(typeof(x))
rem(x::Integer, T::Type{<:Integer}) = convert(T, x)  # `x % T` falls back to `convert`
rem(x::Integer, ::Type{Bool}) = ((x & 1) != 0)
mod(x::Integer, ::Type{T}) where {T<:Integer} = rem(x, T)

unsafe_trunc(::Type{T}, x::Integer) where {T<:Integer} = rem(x, T)

"""
    trunc([T,] x)
    trunc(x; digits::Integer= [, base = 10])
    trunc(x; sigdigits::Integer= [, base = 10])

`trunc(x)` returns the nearest integral value of the same type as `x` whose absolute value
is less than or equal to `x`.

`trunc(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits`, `sigdigits` and `base` work as for [`round`](@ref).
"""
function trunc end

"""
    floor([T,] x)
    floor(x; digits::Integer= [, base = 10])
    floor(x; sigdigits::Integer= [, base = 10])

`floor(x)` returns the nearest integral value of the same type as `x` that is less than or
equal to `x`.

`floor(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits`, `sigdigits` and `base` work as for [`round`](@ref).
"""
function floor end

"""
    ceil([T,] x)
    ceil(x; digits::Integer= [, base = 10])
    ceil(x; sigdigits::Integer= [, base = 10])

`ceil(x)` returns the nearest integral value of the same type as `x` that is greater than or
equal to `x`.

`ceil(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is not
representable.

`digits`, `sigdigits` and `base` work as for [`round`](@ref).
"""
function ceil end

round(::Type{T}, x::Integer) where {T<:Integer} = convert(T, x)
trunc(::Type{T}, x::Integer) where {T<:Integer} = convert(T, x)
floor(::Type{T}, x::Integer) where {T<:Integer} = convert(T, x)
 ceil(::Type{T}, x::Integer) where {T<:Integer} = convert(T, x)

## integer construction ##

"""
    @int128_str str
    @int128_str(str)

`@int128_str` parses a string into a Int128
Throws an `ArgumentError` if the string is not a valid integer
"""
macro int128_str(s)
    return parse(Int128, s)
end

"""
    @uint128_str str
    @uint128_str(str)

`@uint128_str` parses a string into a UInt128
Throws an `ArgumentError` if the string is not a valid integer
"""
macro uint128_str(s)
    return parse(UInt128, s)
end

"""
    @big_str str
    @big_str(str)

Parse a string into a [`BigInt`](@ref) or [`BigFloat`](@ref),
and throw an `ArgumentError` if the string is not a valid number.
For integers `_` is allowed in the string as a separator.

# Examples
```jldoctest
julia> big"123_456"
123456

julia> big"7891.5"
7891.5
```
"""
macro big_str(s)
    if '_' in s
        # remove _ in s[2:end-1]
        bf = IOBuffer(maxsize=lastindex(s))
        print(bf, s[1])
        for c in SubString(s, 2, lastindex(s)-1)
            c != '_' && print(bf, c)
        end
        print(bf, s[end])
        seekstart(bf)
        n = tryparse(BigInt, String(take!(bf)))
        n === nothing || return n
    else
        n = tryparse(BigInt, s)
        n === nothing || return n
        n = tryparse(BigFloat, s)
        n === nothing || return n
    end
    message = "invalid number format $s for BigInt or BigFloat"
    return :(throw(ArgumentError($message)))
end

## integer promotions ##

# with different sizes, promote to larger type
promote_rule(::Type{Int16}, ::Union{Type{Int8}, Type{UInt8}}) = Int16
promote_rule(::Type{Int32}, ::Union{Type{Int16}, Type{Int8}, Type{UInt16}, Type{UInt8}}) = Int32
promote_rule(::Type{Int64}, ::Union{Type{Int16}, Type{Int32}, Type{Int8}, Type{UInt16}, Type{UInt32}, Type{UInt8}}) = Int64
promote_rule(::Type{Int128}, ::Union{Type{Int16}, Type{Int32}, Type{Int64}, Type{Int8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt8}}) = Int128
promote_rule(::Type{UInt16}, ::Union{Type{Int8}, Type{UInt8}}) = UInt16
promote_rule(::Type{UInt32}, ::Union{Type{Int16}, Type{Int8}, Type{UInt16}, Type{UInt8}}) = UInt32
promote_rule(::Type{UInt64}, ::Union{Type{Int16}, Type{Int32}, Type{Int8}, Type{UInt16}, Type{UInt32}, Type{UInt8}}) = UInt64
promote_rule(::Type{UInt128}, ::Union{Type{Int16}, Type{Int32}, Type{Int64}, Type{Int8}, Type{UInt16}, Type{UInt32}, Type{UInt64}, Type{UInt8}}) = UInt128
# with mixed signedness and same size, Unsigned wins
promote_rule(::Type{UInt8},   ::Type{Int8}  ) = UInt8
promote_rule(::Type{UInt16},  ::Type{Int16} ) = UInt16
promote_rule(::Type{UInt32},  ::Type{Int32} ) = UInt32
promote_rule(::Type{UInt64},  ::Type{Int64} ) = UInt64
promote_rule(::Type{UInt128}, ::Type{Int128}) = UInt128

## traits ##

"""
    typemin(T)

The lowest value representable by the given (real) numeric DataType `T`.

# Examples
```jldoctest
julia> typemin(Float16)
-Inf16

julia> typemin(Float32)
-Inf32
```
"""
function typemin end

"""
    typemax(T)

The highest value representable by the given (real) numeric `DataType`.

# Examples
```jldoctest
julia> typemax(Int8)
127

julia> typemax(UInt32)
0xffffffff
```
"""
function typemax end

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


widen(::Type{Int8}) = Int16
widen(::Type{Int16}) = Int32
widen(::Type{Int32}) = Int64
widen(::Type{Int64}) = Int128
widen(::Type{UInt8}) = UInt16
widen(::Type{UInt16}) = UInt32
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

    function _setbit(x::UInt128, i)
        # faster version of `return x | (UInt128(1) << i)`
        j = i >> 5
        y = UInt128(one(UInt32) << (i & 0x1f))
        if j == 0
            return x | y
        elseif j == 1
            return x | (y << 32)
        elseif j == 2
            return x | (y << 64)
        elseif j == 3
            return x | (y << 96)
        end
        return x
    end

    function divrem(x::UInt128, y::UInt128)
        iszero(y) && throw(DivideError())
        if (x >> 64) % UInt64 == 0
            if (y >> 64) % UInt64 == 0
                # fast path: upper 64 bits are zero, so we can fallback to UInt64 division
                q64, x64 = divrem(x % UInt64, y % UInt64)
                return UInt128(q64), UInt128(x64)
            else
                # this implies y>x, so
                return zero(UInt128), x
            end
        end
        n = leading_zeros(y) - leading_zeros(x)
        q = zero(UInt128)
        ys = y << n
        while n >= 0
            # ys == y * 2^n
            if ys <= x
                x -= ys
                q = _setbit(q, n)
                if (x >> 64) % UInt64 == 0
                    # exit early, similar to above fast path
                    if (y >> 64) % UInt64 == 0
                        q64, x64 = divrem(x % UInt64, y % UInt64)
                        q |= q64
                        x = UInt128(x64)
                    end
                    return q, x
                end
            end
            ys >>>= 1
            n -= 1
        end
        return q, x
    end

    function div(x::Int128, y::Int128)
        (x == typemin(Int128)) & (y == -1) && throw(DivideError())
        return Int128(div(BigInt(x), BigInt(y)))::Int128
    end
    div(x::UInt128, y::UInt128) = divrem(x, y)[1]

    function rem(x::Int128, y::Int128)
        return Int128(rem(BigInt(x), BigInt(y)))::Int128
    end

    function rem(x::UInt128, y::UInt128)
        iszero(y) && throw(DivideError())
        if (x >> 64) % UInt64 == 0
            if (y >> 64) % UInt64 == 0
                # fast path: upper 64 bits are zero, so we can fallback to UInt64 division
                return UInt128(rem(x % UInt64, y % UInt64))
            else
                # this implies y>x, so
                return x
            end
        end
        n = leading_zeros(y) - leading_zeros(x)
        ys = y << n
        while n >= 0
            # ys == y * 2^n
            if ys <= x
                x -= ys
                if (x >> 64) % UInt64 == 0
                    # exit early, similar to above fast path
                    if (y >> 64) % UInt64 == 0
                        x = UInt128(rem(x % UInt64, y % UInt64))
                    end
                    return x
                end
            end
            ys >>>= 1
            n -= 1
        end
        return x
    end

    function mod(x::Int128, y::Int128)
        return Int128(mod(BigInt(x), BigInt(y)))::Int128
    end
else
    *(x::T, y::T) where {T<:Union{Int128,UInt128}}  = mul_int(x, y)

    div(x::Int128,  y::Int128)  = checked_sdiv_int(x, y)
    div(x::UInt128, y::UInt128) = checked_udiv_int(x, y)

    rem(x::Int128,  y::Int128)  = checked_srem_int(x, y)
    rem(x::UInt128, y::UInt128) = checked_urem_int(x, y)
end

# issue #15489: since integer ops are unchecked, they shouldn't check promotion
for op in (:+, :-, :*, :&, :|, :xor)
    @eval function $op(a::Integer, b::Integer)
        T = promote_typeof(a, b)
        aT, bT = a % T, b % T
        not_sametype((a, b), (aT, bT))
        return $op(aT, bT)
    end
end

const _mask1_uint128 = (UInt128(0x5555555555555555) << 64) | UInt128(0x5555555555555555)
const _mask2_uint128 = (UInt128(0x3333333333333333) << 64) | UInt128(0x3333333333333333)
const _mask4_uint128 = (UInt128(0x0f0f0f0f0f0f0f0f) << 64) | UInt128(0x0f0f0f0f0f0f0f0f)

"""
    bitreverse(x)

Reverse the order of bits in integer `x`. `x` must have a fixed bit width,
e.g. be an `Int16` or `Int32`.

!!! compat "Julia 1.5"
    This function requires Julia 1.5 or later.

# Examples
```jldoctest
julia> bitreverse(0x8080808080808080)
0x0101010101010101

julia> reverse(bitstring(0xa06e)) == bitstring(bitreverse(0xa06e))
true
```
"""
function bitreverse(x::BitInteger)
    # TODO: consider using llvm.bitreverse intrinsic
    z = unsigned(x)
    mask1 = _mask1_uint128 % typeof(z)
    mask2 = _mask2_uint128 % typeof(z)
    mask4 = _mask4_uint128 % typeof(z)
    z = ((z & mask1) << 1) | ((z >> 1) & mask1)
    z = ((z & mask2) << 2) | ((z >> 2) & mask2)
    z = ((z & mask4) << 4) | ((z >> 4) & mask4)
    return bswap(z) % typeof(x)
end
