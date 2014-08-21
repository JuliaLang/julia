## type aliases ##

typealias SignedUpto64 Union(Int8,Int16,Int32,Int64)
typealias UnsignedUpto64 Union(Uint8,Uint16,Uint32,Uint64)
typealias IntegerUpto64 Union(SignedUpto64,UnsignedUpto64)

typealias SignedUpto128 Union(SignedUpto64,Int128)
typealias UnsignedUpto128 Union(UnsignedUpto64,Uint128)
typealias IntegerUpto128 Union(SignedUpto128,UnsignedUpto128)

## integer arithmetic ##

-{T<:IntegerUpto128}(x::T) = box(T,neg_int(unbox(T,x)))
+{T<:IntegerUpto128}(x::T, y::T) = box(T,add_int(unbox(T,x),unbox(T,y)))
-{T<:IntegerUpto128}(x::T, y::T) = box(T,sub_int(unbox(T,x),unbox(T,y)))
*{T<:IntegerUpto64}(x::T, y::T) = box(T,mul_int(unbox(T,x),unbox(T,y)))

/(x::Integer, y::Integer) = float(x)/float(y)
inv(x::Integer) = float(one(x))/float(x)

isodd(n::Integer) = bool(rem(n,2))
iseven(n::Integer) = !isodd(n)

signbit(x::Integer) = x < 0
signbit(x::Unsigned) = false

flipsign{T<:SignedUpto128}(x::T, y::T) = box(T,flipsign_int(unbox(T,x),unbox(T,y)))

flipsign(x::Signed, y::Signed)  = flipsign(promote(x,y)...)
flipsign(x::Signed, y::Float32) = flipsign(x, reinterpret(Int32,y))
flipsign(x::Signed, y::Float64) = flipsign(x, reinterpret(Int64,y))
flipsign(x::Signed, y::Real)    = flipsign(x, -oftype(x,signbit(y)))

copysign(x::Signed, y::Signed)  = flipsign(x, x$y)
copysign(x::Signed, y::Float32) = copysign(x, reinterpret(Int32,y))
copysign(x::Signed, y::Float64) = copysign(x, reinterpret(Int64,y))
copysign(x::Signed, y::Real)    = copysign(x, -oftype(x,signbit(y)))

abs(x::Unsigned) = x
abs(x::Signed) = flipsign(x,x)

~(n::Integer) = -n-1

div(x::Signed, y::Unsigned) = flipsign(signed(div(unsigned(abs(x)),y)),x)
div(x::Unsigned, y::Signed) = unsigned(flipsign(signed(div(x,unsigned(abs(y)))),y))

rem(x::Signed, y::Unsigned) = flipsign(signed(rem(unsigned(abs(x)),y)),x)
rem(x::Unsigned, y::Signed) = rem(x,unsigned(abs(y)))

fld(x::Signed, y::Unsigned) = div(x,y)-(signbit(x)&(rem(x,y)!=0))
fld(x::Unsigned, y::Signed) = div(x,y)-(signbit(y)&(rem(x,y)!=0))

mod(x::Signed, y::Unsigned) = rem(y+unsigned(rem(x,y)),y)
mod(x::Unsigned, y::Signed) = rem(y+signed(rem(x,y)),y)

# Don't promote integers for div/rem/mod since there no danger of overflow,
# while there is a substantial performance penalty to 64-bit promotion.

div{T<:SignedUpto64}  (x::T, y::T) = box(T,sdiv_int(unbox(T,x),unbox(T,y)))
div{T<:UnsignedUpto64}(x::T, y::T) = box(T,udiv_int(unbox(T,x),unbox(T,y)))
rem{T<:SignedUpto64}  (x::T, y::T) = box(T,srem_int(unbox(T,x),unbox(T,y)))
rem{T<:UnsignedUpto64}(x::T, y::T) = box(T,urem_int(unbox(T,x),unbox(T,y)))
mod{T<:SignedUpto64}  (x::T, y::T) = box(T,smod_int(unbox(T,x),unbox(T,y)))

mod{T<:Unsigned}(x::T, y::T) = rem(x,y)

fld{T<:Unsigned}(x::T, y::T) = div(x,y)
fld{T<:Integer }(x::T, y::T) = div(x,y)-(signbit(x$y)&(rem(x,y)!=0))

## integer bitwise operations ##

~{T<:IntegerUpto128}(x::T) = box(T,not_int(unbox(T,x)))
(&){T<:IntegerUpto128}(x::T, y::T) = box(T,and_int(unbox(T,x),unbox(T,y)))
(|){T<:IntegerUpto128}(x::T, y::T) = box(T,or_int(unbox(T,x),unbox(T,y)))
($){T<:IntegerUpto128}(x::T, y::T) = box(T,xor_int(unbox(T,x),unbox(T,y)))

<<{T<:IntegerUpto128}(x::T, y::Int32) = box(T,shl_int(unbox(T,x),unbox(Int32,y)))
>>{T<:SignedUpto128}(x::T, y::Int32) = box(T,ashr_int(unbox(T,x),unbox(Int32,y)))
>>{T<:UnsignedUpto128}(x::T, y::Int32) = box(T,lshr_int(unbox(T,x),unbox(Int32,y)))
>>>{T<:IntegerUpto128} (x::T, y::Int32) = box(T,lshr_int(unbox(T,x),unbox(Int32,y)))

bswap(x::Int8)    = x
bswap(x::Uint8)   = x
typealias ByteSwapIntegers Union(Int16, Uint16, Int32, Uint32, Int64, Uint64, Int128, Uint128)
bswap{T<:ByteSwapIntegers}(x::T) = box(T,bswap_int(unbox(T,x)))

count_ones{T<:IntegerUpto128}(x::T) = int(box(T,ctpop_int(unbox(T,x))))
leading_zeros{T<:IntegerUpto128}(x::T) = int(box(T,ctlz_int(unbox(T,x))))
trailing_zeros{T<:IntegerUpto128}(x::T) = int(box(T,cttz_int(unbox(T,x))))

count_zeros  (x::Integer) = count_ones(~x)
leading_ones (x::Integer) = leading_zeros(~x)
trailing_ones(x::Integer) = trailing_zeros(~x)

## integer comparisons ##

<{T<:SignedUpto128}(x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))
<{T<:UnsignedUpto128}(x::T, y::T) = ult_int(unbox(T,x),unbox(T,y))
<={T<:SignedUpto128}(x::T, y::T) = sle_int(unbox(T,x),unbox(T,y))
<={T<:UnsignedUpto128}(x::T, y::T) = ule_int(unbox(T,x),unbox(T,y))

==(x::Signed,   y::Unsigned) = (x >= 0) & (unsigned(x) == y)
==(x::Unsigned, y::Signed  ) = (y >= 0) & (x == unsigned(y))
< (x::Signed,   y::Unsigned) = (x <  0) | (unsigned(x) <  y)
< (x::Unsigned, y::Signed  ) = (y >  0) & (x <  unsigned(y))
<=(x::Signed,   y::Unsigned) = (x <= 0) | (unsigned(x) <= y)
<=(x::Unsigned, y::Signed  ) = (y >= 0) & (x <= unsigned(y))

## integer conversions ##

const _inttypes = (Bool, Int8, Uint8, Int16, Uint16, Int32, Uint32, Char,
                   Int64, Uint64, Int128, Uint128)

for to in _inttypes, from in _inttypes
    if !(to===from) && !(to===Bool)
        if to.size < from.size
            @eval convert(::Type{$to}, x::($from)) = box($to,trunc_int($to,unbox($from,x)))
        elseif from.size < to.size || from===Bool
            if issubtype(from, Signed)
                @eval convert(::Type{$to}, x::($from)) = box($to,sext_int($to,unbox($from,x)))
            else
                @eval convert(::Type{$to}, x::($from)) = box($to,zext_int($to,unbox($from,x)))
            end
        else
            @eval convert(::Type{$to}, x::($from)) = box($to,unbox($from,x))
        end
    end
end

convert{T<:SignedUpto64}(::Type{T}, x::Float32) = box(T,checked_fptosi(T,unbox(Float32,x)))
convert{T<:SignedUpto64}(::Type{T}, x::Float64) = box(T,checked_fptosi(T,unbox(Float64,x)))
convert{T<:UnsignedUpto64}(::Type{T}, x::Float32) = box(T,checked_fptoui(T,unbox(Float32,x)))
convert{T<:UnsignedUpto64}(::Type{T}, x::Float64) = box(T,checked_fptoui(T,unbox(Float64,x)))

function convert(::Type{Int128}, x::FloatingPoint)
    ax = abs(x)
    top = trunc(ldexp(ax,-64))
    bot = ax - ldexp(top,64)
    n = int128(convert(Uint64,top))<<64 + int128(convert(Uint64,bot))
    return x<0 ? -n : n
end
convert(::Type{Int128}, x::Float32) = convert(Int128, float64(x))

function convert(::Type{Uint128}, x::FloatingPoint)
    ax = abs(x)
    top = trunc(ldexp(ax,-64))
    bot = ax - ldexp(top,64)
    n = uint128(convert(Uint64,top))<<64 + uint128(convert(Uint64,bot))
    return x<0 ? -n : n
end
convert(::Type{Uint128}, x::Float32) = convert(Uint128, float64(x))

convert(::Type{Char}, x::Float32) = char(convert(Int, x))
convert(::Type{Char}, x::Float64) = char(convert(Int, x))

convert(::Type{Signed}, x::Uint8  ) = convert(Int8,x)
convert(::Type{Signed}, x::Uint16 ) = convert(Int16,x)
convert(::Type{Signed}, x::Uint32 ) = convert(Int32,x)
convert(::Type{Signed}, x::Uint64 ) = convert(Int64,x)
convert(::Type{Signed}, x::Uint128) = convert(Int128,x)
convert(::Type{Signed}, x::Float32) = convert(Int,x)
convert(::Type{Signed}, x::Float64) = convert(Int,x)
convert(::Type{Signed}, x::Char)    = convert(Int,x)

convert(::Type{Unsigned}, x::Int8   ) = convert(Uint8,x)
convert(::Type{Unsigned}, x::Int16  ) = convert(Uint16,x)
convert(::Type{Unsigned}, x::Int32  ) = convert(Uint32,x)
convert(::Type{Unsigned}, x::Int64  ) = convert(Uint64,x)
convert(::Type{Unsigned}, x::Int128 ) = convert(Uint128,x)
convert(::Type{Unsigned}, x::Float32) = convert(Uint,x)
convert(::Type{Unsigned}, x::Float64) = convert(Uint,x)
convert(::Type{Unsigned}, x::Char)    = convert(Uint,x)

convert(::Type{Integer}, x::Float32) = convert(Int,x)
convert(::Type{Integer}, x::Float64) = convert(Int,x)

int8(x) = convert(Int8,x)
int16(x) = convert(Int16,x)
int32(x) = convert(Int32,x)
int64(x) = convert(Int64,x)
int128(x) = convert(Int128,x)

uint8(x) = convert(Uint8,x)
uint16(x) = convert(Uint16,x)
uint32(x) = convert(Uint32,x)
uint64(x) = convert(Uint64,x)
uint128(x) = convert(Uint128,x)

signed(x) = convert(Signed,x)
unsigned(x) = convert(Unsigned,x)
integer(x) = convert(Integer,x)

round(x::Integer) = x
trunc(x::Integer) = x
floor(x::Integer) = x
 ceil(x::Integer) = x

iround(x::Integer) = x
iround{T<:Integer}(::Type{T}, x::Integer) = convert(T, x)
itrunc(x::Integer) = x
ifloor(x::Integer) = x
 iceil(x::Integer) = x

## integer construction ##

macro int128_str(str)
    int128(str)
end

macro uint128_str(str)
    uint128(str)
end

macro bigint_str(str)
    BigInt(str)
end

## system word size ##

const WORD_SIZE = int(Int.size)*8

## integer promotions ##

promote_rule(::Type{Int16},  ::Type{Int8} ) = Int16
promote_rule(::Type{Int32},  ::Type{Int8} ) = Int32
promote_rule(::Type{Int32},  ::Type{Int16}) = Int32
promote_rule(::Type{Int64},  ::Type{Int8} ) = Int64
promote_rule(::Type{Int64},  ::Type{Int16}) = Int64
promote_rule(::Type{Int64},  ::Type{Int32}) = Int64
promote_rule(::Type{Int128}, ::Type{Int8} ) = Int128
promote_rule(::Type{Int128}, ::Type{Int16}) = Int128
promote_rule(::Type{Int128}, ::Type{Int32}) = Int128
promote_rule(::Type{Int128}, ::Type{Int64}) = Int128

promote_rule(::Type{Uint16},  ::Type{Uint8} ) = Uint16
promote_rule(::Type{Uint32},  ::Type{Uint8} ) = Uint32
promote_rule(::Type{Uint32},  ::Type{Uint16}) = Uint32
promote_rule(::Type{Uint64},  ::Type{Uint8} ) = Uint64
promote_rule(::Type{Uint64},  ::Type{Uint16}) = Uint64
promote_rule(::Type{Uint64},  ::Type{Uint32}) = Uint64
promote_rule(::Type{Uint128}, ::Type{Uint8} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint16}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint32}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint64}) = Uint128

promote_rule(::Type{Uint8}, ::Type{Int8}  ) = Int8
promote_rule(::Type{Uint8}, ::Type{Int16} ) = Int16
promote_rule(::Type{Uint8}, ::Type{Int32} ) = Int32
promote_rule(::Type{Uint8}, ::Type{Int64} ) = Int64
promote_rule(::Type{Uint8}, ::Type{Int128}) = Int128

promote_rule(::Type{Uint16}, ::Type{Int8}  ) = Int16
promote_rule(::Type{Uint16}, ::Type{Int16} ) = Int16
promote_rule(::Type{Uint16}, ::Type{Int32} ) = Int32
promote_rule(::Type{Uint16}, ::Type{Int64} ) = Int64
promote_rule(::Type{Uint16}, ::Type{Int128}) = Int128

if WORD_SIZE == 64
    promote_rule(::Type{Uint32}, ::Type{Int8} ) = Int
    promote_rule(::Type{Uint32}, ::Type{Int16}) = Int
    promote_rule(::Type{Uint32}, ::Type{Int32}) = Int
else
    promote_rule(::Type{Uint32}, ::Type{Int8} ) = Uint
    promote_rule(::Type{Uint32}, ::Type{Int16}) = Uint
    promote_rule(::Type{Uint32}, ::Type{Int32}) = Uint
end
promote_rule(::Type{Uint32}, ::Type{Int64} ) = Int64
promote_rule(::Type{Uint32}, ::Type{Int128}) = Int128

promote_rule(::Type{Uint64}, ::Type{Int8}  ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Int16} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Int32} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Int64} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Int128}) = Int128

promote_rule(::Type{Uint128}, ::Type{Int8}  ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Int16} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Int32} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Int64} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Int128}) = Uint128

## traits ##

typemin(::Type{Int8  }) = int8(-128)
typemax(::Type{Int8  }) = int8(127)
typemin(::Type{Uint8 }) = uint8(0)
typemax(::Type{Uint8 }) = uint8(255)
typemin(::Type{Int16 }) = int16(-32768)
typemax(::Type{Int16 }) = int16(32767)
typemin(::Type{Uint16}) = uint16(0)
typemax(::Type{Uint16}) = uint16(65535)
typemin(::Type{Int32 }) = int32(-2147483648)
typemax(::Type{Int32 }) = int32(2147483647)
typemin(::Type{Uint32}) = uint32(0)
typemax(::Type{Uint32}) = uint32(4294967295)
typemin(::Type{Int64 }) = -9223372036854775808
typemax(::Type{Int64 }) = 9223372036854775807
typemin(::Type{Uint64}) = uint64(0)
typemax(::Type{Uint64}) = 0xffffffffffffffff
@eval begin
    typemin(::Type{Uint128}) = uint128(0)
    typemax(::Type{Uint128}) = $(uint128(-1))
    typemin(::Type{Int128} ) = $(int128((uint128(-1))>>int32(1))+int128(1))
    typemax(::Type{Int128} ) = $(int128((uint128(-1))>>int32(1)))
end

sizeof(::Type{Int8})    = 1
sizeof(::Type{Uint8})   = 1
sizeof(::Type{Int16})   = 2
sizeof(::Type{Uint16})  = 2
sizeof(::Type{Int32})   = 4
sizeof(::Type{Uint32})  = 4
sizeof(::Type{Int64})   = 8
sizeof(::Type{Uint64})  = 8
sizeof(::Type{Int128})  = 16
sizeof(::Type{Uint128}) = 16

widen(::Type{Int8}) = Int
widen(::Type{Int16}) = Int
widen(::Type{Int32}) = Int64
widen(::Type{Int64}) = Int128
widen(::Type{Uint8}) = Uint
widen(::Type{Uint16}) = Uint
widen(::Type{Uint32}) = Uint64
widen(::Type{Uint64}) = Uint128

## float to integer coercion ##

# requires int arithmetic defined, for the loops to work

for (f,t) in ((:uint8,:Uint8), (:uint16,:Uint16), (:uint32,:Uint32), (:uint64,:Uint64),
              (:int8,:Int8),   (:int16,:Int16),   (:int32,:Int32),   (:int64,:Int64),
              (:int128,:Int128), (:uint128,:Uint128),
              (:signed,:Int), (:unsigned,:Uint), (:integer,:Int),
              (:int,:Int), (:uint,:Uint))
    @eval ($f)(x::FloatingPoint) = iround($t,x)
end

## wide multiplication, Int128 multiply and divide ##

if WORD_SIZE==32
    function widemul(u::Int64, v::Int64)
        local u0::Uint64, v0::Uint64, w0::Uint64
        local u1::Int64, v1::Int64, w1::Int64, w2::Int64, t::Int64

        u0 = u&0xffffffff; u1 = u>>32
        v0 = v&0xffffffff; v1 = v>>32
        w0 = u0*v0
        t = u1*v0 + (w0>>>32)
        w2 = t>>32
        w1 = u0*v1 + (t&0xffffffff)
        hi = u1*v1 + w2 + (w1 >> 32)
        lo = w0&0xffffffff + (w1 << 32)
        int128(hi)<<64 + int128(uint128(lo))
    end

    function widemul(u::Uint64, v::Uint64)
        local u0::Uint64, v0::Uint64, w0::Uint64
        local u1::Uint64, v1::Uint64, w1::Uint64, w2::Uint64, t::Uint64

        u0 = u&0xffffffff; u1 = u>>>32
        v0 = v&0xffffffff; v1 = v>>>32
        w0 = u0*v0
        t = u1*v0 + (w0>>>32)
        w2 = t>>>32
        w1 = u0*v1 + (t&0xffffffff)
        hi = u1*v1 + w2 + (w1 >>> 32)
        lo = w0&0xffffffff + (w1 << 32)
        uint128(hi)<<64 + uint128(lo)
    end

    function *(u::Int128, v::Int128)
        u0 = uint64(u); u1 = int64(u>>64)
        v0 = uint64(v); v1 = int64(v>>64)
        lolo = widemul(u0, v0)
        lohi = widemul(int64(u0), v1)
        hilo = widemul(u1, int64(v0))
        t = hilo + (lolo>>>64)
        w2 = t>>64
        w1 = lohi + (t&0xffffffffffffffff)
        int128(lolo&0xffffffffffffffff) + int128(w1)<<64
    end

    function *(u::Uint128, v::Uint128)
        u0 = uint64(u); u1 = uint64(u>>>64)
        v0 = uint64(v); v1 = uint64(v>>>64)
        lolo = widemul(u0, v0)
        lohi = widemul(u0, v1)
        hilo = widemul(u1, v0)
        t = hilo + (lolo>>>64)
        w2 = t>>>64
        w1 = lohi + (t&0xffffffffffffffff)
        (lolo&0xffffffffffffffff) + uint128(w1)<<64
    end

    div(x::Int128, y::Int128) = int128(div(BigInt(x),BigInt(y)))
    div(x::Uint128, y::Uint128) = uint128(div(BigInt(x),BigInt(y)))

    rem(x::Int128, y::Int128) = int128(rem(BigInt(x),BigInt(y)))
    rem(x::Uint128, y::Uint128) = uint128(rem(BigInt(x),BigInt(y)))

    mod(x::Int128, y::Int128) = int128(mod(BigInt(x),BigInt(y)))

    << (x::Int128,  y::Int32) = y == 0 ? x : box(Int128,shl_int(unbox(Int128,x),unbox(Int32,y)))
    << (x::Uint128, y::Int32) = y == 0 ? x : box(Uint128,shl_int(unbox(Uint128,x),unbox(Int32,y)))
    >> (x::Int128,  y::Int32) = y == 0 ? x : box(Int128,ashr_int(unbox(Int128,x),unbox(Int32,y)))
    >> (x::Uint128, y::Int32) = y == 0 ? x : box(Uint128,lshr_int(unbox(Uint128,x),unbox(Int32,y)))
    >>>(x::Int128,  y::Int32) = y == 0 ? x : box(Int128,lshr_int(unbox(Int128,x),unbox(Int32,y)))
    >>>(x::Uint128, y::Int32) = y == 0 ? x : box(Uint128,lshr_int(unbox(Uint128,x),unbox(Int32,y)))
else
    *(x::Int128,  y::Int128)  = box(Int128,mul_int(unbox(Int128,x),unbox(Int128,y)))
    *(x::Uint128, y::Uint128) = box(Uint128,mul_int(unbox(Uint128,x),unbox(Uint128,y)))

    div(x::Int128,  y::Int128)  = box(Int128,sdiv_int(unbox(Int128,x),unbox(Int128,y)))
    div(x::Uint128, y::Uint128) = box(Uint128,udiv_int(unbox(Uint128,x),unbox(Uint128,y)))

    rem(x::Int128,  y::Int128)  = box(Int128,srem_int(unbox(Int128,x),unbox(Int128,y)))
    rem(x::Uint128, y::Uint128) = box(Uint128,urem_int(unbox(Uint128,x),unbox(Uint128,y)))

    mod(x::Int128, y::Int128) = box(Int128,smod_int(unbox(Int128,x),unbox(Int128,y)))
end

## checked +, - and *

for T in (Int8,Int16,Int32,Int64)#,Int128) ## FIXME: #4905
    @eval begin
        checked_add(x::$T, y::$T) = box($T,checked_sadd(unbox($T,x),unbox($T,y)))
        checked_sub(x::$T, y::$T) = box($T,checked_ssub(unbox($T,x),unbox($T,y)))
    end
end
for T in (Int16,Int32)
    @eval begin
        checked_mul(x::$T, y::$T) = box($T,checked_smul(unbox($T,x),unbox($T,y)))
    end
end
for T in (Uint8,Uint16,Uint32,Uint64)#,Uint128) ## FIXME: #4905
    @eval begin
        checked_add(x::$T, y::$T) = box($T,checked_uadd(unbox($T,x),unbox($T,y)))
        checked_sub(x::$T, y::$T) = box($T,checked_usub(unbox($T,x),unbox($T,y)))
    end
end
for T in (Uint16,Uint32)
    @eval begin
        checked_mul(x::$T, y::$T) = box($T,checked_umul(unbox($T,x),unbox($T,y)))
    end
end

# checked mul is broken for 8-bit types (LLVM bug?) ## FIXME: #4905

for T in (Int8,Uint8)
    @eval function checked_mul(x::$T, y::$T)
        xy = x*y
        xy8 = convert($T,xy)
        xy == xy8 || throw(OverflowError())
        return xy8
    end
end

if WORD_SIZE == 32
for T in (Int64,Uint64)
    @eval function checked_mul(x::$T, y::$T)
        xy = int128(x)*int128(y)
        xy64 = convert($T,xy)
        xy == xy64 || throw(OverflowError())
        return xy64
    end
end
else
    checked_mul(x::Int64, y::Int64)   = box(Int64,checked_smul(unbox(Int64,x),unbox(Int64,y)))
    checked_mul(x::Uint64, y::Uint64) = box(Uint64,checked_umul(unbox(Uint64,x),unbox(Uint64,y)))
end

# checked ops are broken for 128-bit types (LLVM bug) ## FIXME: #4905

checked_add(x::Int128, y::Int128) = x + y
checked_sub(x::Int128, y::Int128) = x - y
checked_mul(x::Int128, y::Int128) = x * y

checked_add(x::Uint128, y::Uint128) = x + y
checked_sub(x::Uint128, y::Uint128) = x - y
checked_mul(x::Uint128, y::Uint128) = x * y
