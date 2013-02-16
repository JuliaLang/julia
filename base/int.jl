## integer arithmetic ##

-(x::Signed) = -int(x)
-(x::Unsigned) = -uint(x)

+{T<:Signed}(x::T, y::T) = int(x) + int(y)
-{T<:Signed}(x::T, y::T) = int(x) - int(y)
*{T<:Signed}(x::T, y::T) = int(x) * int(y)

+{T<:Unsigned}(x::T, y::T) = uint(x) + uint(y)
-{T<:Unsigned}(x::T, y::T) = uint(x) - uint(y)
*{T<:Unsigned}(x::T, y::T) = uint(x) * uint(y)

-(x::Int)     = box(Int,neg_int(unbox(Int,x)))
-(x::Uint)    = box(Uint,neg_int(unbox(Uint,x)))
-(x::Int64)   = box(Int64,neg_int(unbox(Int64,x)))
-(x::Uint64)  = box(Uint64,neg_int(unbox(Uint64,x)))
-(x::Int128)  = box(Int128,neg_int(unbox(Int128,x)))
-(x::Uint128) = box(Uint128,neg_int(unbox(Uint128,x)))

+(x::Int,     y::Int)     = box(Int,add_int(unbox(Int,x),unbox(Int,y)))
+(x::Uint,    y::Uint)    = box(Uint,add_int(unbox(Uint,x),unbox(Uint,y)))
+(x::Int64,   y::Int64)   = box(Int64,add_int(unbox(Int64,x),unbox(Int64,y)))
+(x::Uint64,  y::Uint64)  = box(Uint64,add_int(unbox(Uint64,x),unbox(Uint64,y)))
+(x::Int128,  y::Int128)  = box(Int128,add_int(unbox(Int128,x),unbox(Int128,y)))
+(x::Uint128, y::Uint128) = box(Uint128,add_int(unbox(Uint128,x),unbox(Uint128,y)))

-(x::Int,     y::Int)     = box(Int,sub_int(unbox(Int,x),unbox(Int,y)))
-(x::Uint,    y::Uint)    = box(Uint,sub_int(unbox(Uint,x),unbox(Uint,y)))
-(x::Int64,   y::Int64)   = box(Int64,sub_int(unbox(Int64,x),unbox(Int64,y)))
-(x::Uint64,  y::Uint64)  = box(Uint64,sub_int(unbox(Uint64,x),unbox(Uint64,y)))
-(x::Int128,  y::Int128)  = box(Int128,sub_int(unbox(Int128,x),unbox(Int128,y)))
-(x::Uint128, y::Uint128) = box(Uint128,sub_int(unbox(Uint128,x),unbox(Uint128,y)))

*(x::Int,     y::Int)     = box(Int,mul_int(unbox(Int,x),unbox(Int,y)))
*(x::Uint,    y::Uint)    = box(Uint,mul_int(unbox(Uint,x),unbox(Uint,y)))
*(x::Int64,   y::Int64)   = box(Int64,mul_int(unbox(Int64,x),unbox(Int64,y)))
*(x::Uint64,  y::Uint64)  = box(Uint64,mul_int(unbox(Uint64,x),unbox(Uint64,y)))

/(x::Integer, y::Integer) = float64(x)/float64(y)
inv(x::Integer) = 1.0/float64(x)

div{T<:Signed}(x::T, y::T) = div(int(x),int(y))
rem{T<:Signed}(x::T, y::T) = rem(int(x),int(y))
mod{T<:Signed}(x::T, y::T) = mod(int(x),int(y))

div{T<:Unsigned}(x::T, y::T) = div(uint(x),uint(y))
rem{T<:Unsigned}(x::T, y::T) = rem(uint(x),uint(y))
mod{T<:Unsigned}(x::T, y::T) = rem(x,y)

div(x::Signed, y::Unsigned) = flipsign(signed(div(unsigned(abs(x)),y)),x)
div(x::Unsigned, y::Signed) = unsigned(flipsign(signed(div(x,unsigned(abs(y)))),y))

rem(x::Signed, y::Unsigned) = flipsign(signed(rem(unsigned(abs(x)),y)),x)
rem(x::Unsigned, y::Signed) = rem(x,unsigned(abs(y)))

fld(x::Signed, y::Unsigned) = div(x,y)-(signbit(x)&(rem(x,y)!=0))
fld(x::Unsigned, y::Signed) = div(x,y)-(signbit(y)&(rem(x,y)!=0))

mod(x::Signed, y::Unsigned) = rem(y+unsigned(rem(x,y)),y)
mod(x::Unsigned, y::Signed) = rem(y+signed(rem(x,y)),y)

div(x::Int,     y::Int)     = box(Int,sdiv_int(unbox(Int,x),unbox(Int,y)))
div(x::Uint,    y::Uint)    = box(Uint,udiv_int(unbox(Uint,x),unbox(Uint,y)))
div(x::Int64,   y::Int64)   = box(Int64,sdiv_int(unbox(Int64,x),unbox(Int64,y)))
div(x::Uint64,  y::Uint64)  = box(Uint64,udiv_int(unbox(Uint64,x),unbox(Uint64,y)))

rem(x::Int,     y::Int)     = box(Int,srem_int(unbox(Int,x),unbox(Int,y)))
rem(x::Uint,    y::Uint)    = box(Uint,urem_int(unbox(Uint,x),unbox(Uint,y)))
rem(x::Int64,   y::Int64)   = box(Int64,srem_int(unbox(Int64,x),unbox(Int64,y)))
rem(x::Uint64,  y::Uint64)  = box(Uint64,urem_int(unbox(Uint64,x),unbox(Uint64,y)))

fld{T<:Unsigned}(x::T, y::T) = div(x,y)
fld{T<:Integer }(x::T, y::T) = div(x,y)-(signbit(x$y)&(rem(x,y)!=0))

mod(x::Int,    y::Int)    = box(Int,smod_int(unbox(Int,x),unbox(Int,y)))
mod(x::Int64,  y::Int64)  = box(Int64,smod_int(unbox(Int64,x),unbox(Int64,y)))

## integer bitwise operations ##

~(x::Int8 )  = box(Int8,not_int(unbox(Int8,x)))
~(x::Int16)  = box(Int16,not_int(unbox(Int16,x)))
~(x::Int32)  = box(Int32,not_int(unbox(Int32,x)))
~(x::Int64)  = box(Int64,not_int(unbox(Int64,x)))
~(x::Int128) = box(Int128,not_int(unbox(Int128,x)))

~(x::Uint8 )  = box(Uint8,not_int(unbox(Uint8,x)))
~(x::Uint16)  = box(Uint16,not_int(unbox(Uint16,x)))
~(x::Uint32)  = box(Uint32,not_int(unbox(Uint32,x)))
~(x::Uint64)  = box(Uint64,not_int(unbox(Uint64,x)))
~(x::Uint128) = box(Uint128,not_int(unbox(Uint128,x)))

(&)(x::Int8,   y::Int8 )  = box(Int8,and_int(unbox(Int8,x),unbox(Int8,y)))
(&)(x::Int16,  y::Int16)  = box(Int16,and_int(unbox(Int16,x),unbox(Int16,y)))
(&)(x::Int32,  y::Int32)  = box(Int32,and_int(unbox(Int32,x),unbox(Int32,y)))
(&)(x::Int64,  y::Int64)  = box(Int64,and_int(unbox(Int64,x),unbox(Int64,y)))
(&)(x::Int128, y::Int128) = box(Int128,and_int(unbox(Int128,x),unbox(Int128,y)))

(&)(x::Uint8,   y::Uint8 )  = box(Uint8,and_int(unbox(Uint8,x),unbox(Uint8,y)))
(&)(x::Uint16,  y::Uint16)  = box(Uint16,and_int(unbox(Uint16,x),unbox(Uint16,y)))
(&)(x::Uint32,  y::Uint32)  = box(Uint32,and_int(unbox(Uint32,x),unbox(Uint32,y)))
(&)(x::Uint64,  y::Uint64)  = box(Uint64,and_int(unbox(Uint64,x),unbox(Uint64,y)))
(&)(x::Uint128, y::Uint128) = box(Uint128,and_int(unbox(Uint128,x),unbox(Uint128,y)))

|(x::Int8,   y::Int8)   = box(Int8,or_int(unbox(Int8,x),unbox(Int8,y)))
|(x::Int16,  y::Int16)  = box(Int16,or_int(unbox(Int16,x),unbox(Int16,y)))
|(x::Int32,  y::Int32)  = box(Int32,or_int(unbox(Int32,x),unbox(Int32,y)))
|(x::Int64,  y::Int64)  = box(Int64,or_int(unbox(Int64,x),unbox(Int64,y)))
|(x::Int128, y::Int128) = box(Int128,or_int(unbox(Int128,x),unbox(Int128,y)))

|(x::Uint8,   y::Uint8)   = box(Uint8,or_int(unbox(Uint8,x),unbox(Uint8,y)))
|(x::Uint16,  y::Uint16)  = box(Uint16,or_int(unbox(Uint16,x),unbox(Uint16,y)))
|(x::Uint32,  y::Uint32)  = box(Uint32,or_int(unbox(Uint32,x),unbox(Uint32,y)))
|(x::Uint64,  y::Uint64)  = box(Uint64,or_int(unbox(Uint64,x),unbox(Uint64,y)))
|(x::Uint128, y::Uint128) = box(Uint128,or_int(unbox(Uint128,x),unbox(Uint128,y)))

($)(x::Int8,   y::Int8)   = box(Int8,xor_int(unbox(Int8,x),unbox(Int8,y)))
($)(x::Int16,  y::Int16)  = box(Int16,xor_int(unbox(Int16,x),unbox(Int16,y)))
($)(x::Int32,  y::Int32)  = box(Int32,xor_int(unbox(Int32,x),unbox(Int32,y)))
($)(x::Int64,  y::Int64)  = box(Int64,xor_int(unbox(Int64,x),unbox(Int64,y)))
($)(x::Int128, y::Int128) = box(Int128,xor_int(unbox(Int128,x),unbox(Int128,y)))

($)(x::Uint8,   y::Uint8)   = box(Uint8,xor_int(unbox(Uint8,x),unbox(Uint8,y)))
($)(x::Uint16,  y::Uint16)  = box(Uint16,xor_int(unbox(Uint16,x),unbox(Uint16,y)))
($)(x::Uint32,  y::Uint32)  = box(Uint32,xor_int(unbox(Uint32,x),unbox(Uint32,y)))
($)(x::Uint64,  y::Uint64)  = box(Uint64,xor_int(unbox(Uint64,x),unbox(Uint64,y)))
($)(x::Uint128, y::Uint128) = box(Uint128,xor_int(unbox(Uint128,x),unbox(Uint128,y)))

<<(x::Int8,   y::Int32) = box(Int8,shl_int(unbox(Int8,x),unbox(Int32,y)))
<<(x::Int16,  y::Int32) = box(Int16,shl_int(unbox(Int16,x),unbox(Int32,y)))
<<(x::Int32,  y::Int32) = box(Int32,shl_int(unbox(Int32,x),unbox(Int32,y)))
<<(x::Int64,  y::Int32) = box(Int64,shl_int(unbox(Int64,x),unbox(Int32,y)))
<<(x::Int128, y::Int32) = box(Int128,shl_int(unbox(Int128,x),unbox(Int32,y)))

<<(x::Uint8,   y::Int32) = box(Uint8,shl_int(unbox(Uint8,x),unbox(Int32,y)))
<<(x::Uint16,  y::Int32) = box(Uint16,shl_int(unbox(Uint16,x),unbox(Int32,y)))
<<(x::Uint32,  y::Int32) = box(Uint32,shl_int(unbox(Int32,x),unbox(Uint32,y)))
<<(x::Uint64,  y::Int32) = box(Uint64,shl_int(unbox(Uint64,x),unbox(Int32,y)))
<<(x::Uint128, y::Int32) = box(Uint128,shl_int(unbox(Uint128,x),unbox(Int32,y)))

>>(x::Int8,   y::Int32) = box(Int8,ashr_int(unbox(Int8,x),unbox(Int32,y)))
>>(x::Int16,  y::Int32) = box(Int16,ashr_int(unbox(Int16,x),unbox(Int32,y)))
>>(x::Int32,  y::Int32) = box(Int32,ashr_int(unbox(Int32,x),unbox(Int32,y)))
>>(x::Int64,  y::Int32) = box(Int64,ashr_int(unbox(Int64,x),unbox(Int32,y)))
>>(x::Int128, y::Int32) = box(Int128,ashr_int(unbox(Int128,x),unbox(Int32,y)))

>>(x::Uint8,   y::Int32) = box(Uint8,lshr_int(unbox(Uint8,x),unbox(Int32,y)))
>>(x::Uint16,  y::Int32) = box(Uint16,lshr_int(unbox(Uint16,x),unbox(Int32,y)))
>>(x::Uint32,  y::Int32) = box(Uint32,lshr_int(unbox(Int32,x),unbox(Uint32,y)))
>>(x::Uint64,  y::Int32) = box(Uint64,lshr_int(unbox(Uint64,x),unbox(Int32,y)))
>>(x::Uint128, y::Int32) = box(Uint128,lshr_int(unbox(Uint128,x),unbox(Int32,y)))

>>>(x::Int8,   y::Int32) = box(Int8,lshr_int(unbox(Int8,x),unbox(Int32,y)))
>>>(x::Int16,  y::Int32) = box(Int16,lshr_int(unbox(Int16,x),unbox(Int32,y)))
>>>(x::Int32,  y::Int32) = box(Int32,lshr_int(unbox(Int32,x),unbox(Int32,y)))
>>>(x::Int64,  y::Int32) = box(Int64,lshr_int(unbox(Int64,x),unbox(Int32,y)))
>>>(x::Int128, y::Int32) = box(Int128,lshr_int(unbox(Int128,x),unbox(Int32,y)))

>>>(x::Uint8,   y::Int32) = box(Uint8,lshr_int(unbox(Uint8,x),unbox(Int32,y)))
>>>(x::Uint16,  y::Int32) = box(Uint16,lshr_int(unbox(Uint16,x),unbox(Int32,y)))
>>>(x::Uint32,  y::Int32) = box(Uint32,lshr_int(unbox(Int32,x),unbox(Uint32,y)))
>>>(x::Uint64,  y::Int32) = box(Uint64,lshr_int(unbox(Uint64,x),unbox(Int32,y)))
>>>(x::Uint128, y::Int32) = box(Uint128,lshr_int(unbox(Uint128,x),unbox(Int32,y)))

bswap(x::Int8)    = x
bswap(x::Uint8)   = x
bswap(x::Int16)   = box(Int16,bswap_int(unbox(Int16,x)))
bswap(x::Uint16)  = box(Uint16,bswap_int(unbox(Uint16,x)))
bswap(x::Int32)   = box(Int32,bswap_int(unbox(Int32,x)))
bswap(x::Uint32)  = box(Uint32,bswap_int(unbox(Uint32,x)))
bswap(x::Int64)   = box(Int64,bswap_int(unbox(Int64,x)))
bswap(x::Uint64)  = box(Uint64,bswap_int(unbox(Uint64,x)))
bswap(x::Int128)  = box(Int128,bswap_int(unbox(Int128,x)))
bswap(x::Uint128) = box(Uint128,bswap_int(unbox(Uint128,x)))

count_ones(x::Int8)    = int(box(Int8,ctpop_int(unbox(Int8,x))))
count_ones(x::Uint8)   = int(box(Uint8,ctpop_int(unbox(Uint8,x))))
count_ones(x::Int16)   = int(box(Int16,ctpop_int(unbox(Int16,x))))
count_ones(x::Uint16)  = int(box(Uint16,ctpop_int(unbox(Uint16,x))))
count_ones(x::Int32)   = int(box(Int32,ctpop_int(unbox(Int32,x))))
count_ones(x::Uint32)  = int(box(Uint32,ctpop_int(unbox(Uint32,x))))
count_ones(x::Int64)   = int(box(Int64,ctpop_int(unbox(Int64,x))))
count_ones(x::Uint64)  = int(box(Uint64,ctpop_int(unbox(Uint64,x))))
count_ones(x::Int128)  = int(box(Int128,ctpop_int(unbox(Int128,x))))
count_ones(x::Uint128) = int(box(Uint128,ctpop_int(unbox(Uint128,x))))

leading_zeros(x::Int8)    = int(box(Int8,ctlz_int(unbox(Int8,x))))
leading_zeros(x::Uint8)   = int(box(Uint8,ctlz_int(unbox(Uint8,x))))
leading_zeros(x::Int16)   = int(box(Int16,ctlz_int(unbox(Int16,x))))
leading_zeros(x::Uint16)  = int(box(Uint16,ctlz_int(unbox(Uint16,x))))
leading_zeros(x::Int32)   = int(box(Int32,ctlz_int(unbox(Int32,x))))
leading_zeros(x::Uint32)  = int(box(Uint32,ctlz_int(unbox(Uint32,x))))
leading_zeros(x::Int64)   = int(box(Int64,ctlz_int(unbox(Int64,x))))
leading_zeros(x::Uint64)  = int(box(Uint64,ctlz_int(unbox(Uint64,x))))
leading_zeros(x::Int128)  = int(box(Int128,ctlz_int(unbox(Int128,x))))
leading_zeros(x::Uint128) = int(box(Uint128,ctlz_int(unbox(Uint128,x))))

trailing_zeros(x::Int8)    = int(box(Int8,cttz_int(unbox(Int8,x))))
trailing_zeros(x::Uint8)   = int(box(Uint8,cttz_int(unbox(Uint8,x))))
trailing_zeros(x::Int16)   = int(box(Int16,cttz_int(unbox(Int16,x))))
trailing_zeros(x::Uint16)  = int(box(Uint16,cttz_int(unbox(Uint16,x))))
trailing_zeros(x::Int32)   = int(box(Int32,cttz_int(unbox(Int32,x))))
trailing_zeros(x::Uint32)  = int(box(Uint32,cttz_int(unbox(Uint32,x))))
trailing_zeros(x::Int64)   = int(box(Int64,cttz_int(unbox(Int64,x))))
trailing_zeros(x::Uint64)  = int(box(Uint64,cttz_int(unbox(Uint64,x))))
trailing_zeros(x::Int128)  = int(box(Int128,cttz_int(unbox(Int128,x))))
trailing_zeros(x::Uint128) = int(box(Uint128,cttz_int(unbox(Uint128,x))))

count_zeros  (x::Integer) = count_ones(~x)
leading_ones (x::Integer) = leading_zeros(~x)
trailing_ones(x::Integer) = trailing_zeros(~x)

## integer comparisons ##

==(x::Int8,   y::Int8 )  = eq_int(unbox(Int8,x),unbox(Int8,y))
==(x::Int16,  y::Int16)  = eq_int(unbox(Int16,x),unbox(Int16,y))
==(x::Int32,  y::Int32)  = eq_int(unbox(Int32,x),unbox(Int32,y))
==(x::Int64,  y::Int64)  = eq_int(unbox(Int64,x),unbox(Int64,y))
==(x::Int128, y::Int128) = eq_int(unbox(Int128,x),unbox(Int128,y))

==(x::Uint8,   y::Uint8 )  = eq_int(unbox(Uint8,x),unbox(Uint8,y))
==(x::Uint16,  y::Uint16)  = eq_int(unbox(Uint16,x),unbox(Uint16,y))
==(x::Uint32,  y::Uint32)  = eq_int(unbox(Uint32,x),unbox(Uint32,y))
==(x::Uint64,  y::Uint64)  = eq_int(unbox(Uint64,x),unbox(Uint64,y))
==(x::Uint128, y::Uint128) = eq_int(unbox(Uint128,x),unbox(Uint128,y))

!=(x::Int8,   y::Int8)   = ne_int(unbox(Int8,x),unbox(Int8,y))
!=(x::Int16,  y::Int16)  = ne_int(unbox(Int16,x),unbox(Int16,y))
!=(x::Int32,  y::Int32)  = ne_int(unbox(Int32,x),unbox(Int32,y))
!=(x::Int64,  y::Int64)  = ne_int(unbox(Int64,x),unbox(Int64,y))
!=(x::Int128, y::Int128) = ne_int(unbox(Int128,x),unbox(Int128,y))

!=(x::Uint8,   y::Uint8)   = ne_int(unbox(Uint8,x),unbox(Uint8,y))
!=(x::Uint16,  y::Uint16)  = ne_int(unbox(Uint16,x),unbox(Uint16,y))
!=(x::Uint32,  y::Uint32)  = ne_int(unbox(Uint32,x),unbox(Uint32,y))
!=(x::Uint64,  y::Uint64)  = ne_int(unbox(Uint64,x),unbox(Uint64,y))
!=(x::Uint128, y::Uint128) = ne_int(unbox(Uint128,x),unbox(Uint128,y))

<(x::Int8,   y::Int8)   = slt_int(unbox(Int8,x),unbox(Int8,y))
<(x::Int16,  y::Int16)  = slt_int(unbox(Int16,x),unbox(Int16,y))
<(x::Int32,  y::Int32)  = slt_int(unbox(Int32,x),unbox(Int32,y))
<(x::Int64,  y::Int64)  = slt_int(unbox(Int64,x),unbox(Int64,y))
<(x::Int128, y::Int128) = slt_int(unbox(Int128,x),unbox(Int128,y))

<(x::Uint8,   y::Uint8)   = ult_int(unbox(Uint8,x),unbox(Uint8,y))
<(x::Uint16,  y::Uint16)  = ult_int(unbox(Uint16,x),unbox(Uint16,y))
<(x::Uint32,  y::Uint32)  = ult_int(unbox(Uint32,x),unbox(Uint32,y))
<(x::Uint64,  y::Uint64)  = ult_int(unbox(Uint64,x),unbox(Uint64,y))
<(x::Uint128, y::Uint128) = ult_int(unbox(Uint128,x),unbox(Uint128,y))

<=(x::Int8,   y::Int8)   = sle_int(unbox(Int8,x),unbox(Int8,y))
<=(x::Int16,  y::Int16)  = sle_int(unbox(Int16,x),unbox(Int16,y))
<=(x::Int32,  y::Int32)  = sle_int(unbox(Int32,x),unbox(Int32,y))
<=(x::Int64,  y::Int64)  = sle_int(unbox(Int64,x),unbox(Int64,y))
<=(x::Int128, y::Int128) = sle_int(unbox(Int128,x),unbox(Int128,y))

<=(x::Uint8,   y::Uint8)   = ule_int(unbox(Uint8,x),unbox(Uint8,y))
<=(x::Uint16,  y::Uint16)  = ule_int(unbox(Uint16,x),unbox(Uint16,y))
<=(x::Uint32,  y::Uint32)  = ule_int(unbox(Uint32,x),unbox(Uint32,y))
<=(x::Uint64,  y::Uint64)  = ule_int(unbox(Uint64,x),unbox(Uint64,y))
<=(x::Uint128, y::Uint128) = ule_int(unbox(Uint128,x),unbox(Uint128,y))

==(x::Signed,   y::Unsigned) = (x >= 0) & (unsigned(x) == y)
==(x::Unsigned, y::Signed  ) = (y >= 0) & (x == unsigned(y))
!=(x::Signed,   y::Unsigned) = (x <  0) | (unsigned(x) != y)
!=(x::Unsigned, y::Signed  ) = (y <  0) | (x != unsigned(y))
< (x::Signed,   y::Unsigned) = (x <  0) | (unsigned(x) <  y)
< (x::Unsigned, y::Signed  ) = (y >  0) & (x <  unsigned(y))
<=(x::Signed,   y::Unsigned) = (x <= 0) | (unsigned(x) <= y)
<=(x::Unsigned, y::Signed  ) = (y >= 0) & (x <= unsigned(y))

## integer conversions ##

const _inttypes = (Bool, Int8, Uint8, Int16, Uint16, Int32, Uint32, Char,
                   Int64, Uint64, Int128, Uint128)

for to in _inttypes, from in _inttypes
    if !(to===from) && !(to===Bool)
        if to.nbits < from.nbits
            @eval convert(::Type{$to}, x::($from)) = box($to,trunc_int($to,unbox($from,x)))
        elseif from.nbits < to.nbits || from===Bool
            if subtype(from, Signed)
                @eval convert(::Type{$to}, x::($from)) = box($to,sext_int($to,unbox($from,x)))
            else
                @eval convert(::Type{$to}, x::($from)) = box($to,zext_int($to,unbox($from,x)))
            end
        else
            @eval convert(::Type{$to}, x::($from)) = box($to,unbox($from,x))
        end
    end
end

for to in (Int8, Uint8, Int16, Uint16)
    @eval begin
        convert(::Type{$to}, x::Float32) = box($to,trunc_int($to,checked_fptosi32(unbox(Float32,x))))
        convert(::Type{$to}, x::Float64) = box($to,trunc_int($to,checked_fptosi64(unbox(Float64,x))))
    end
end

convert(::Type{Int32}, x::Float32) = box(Int32,checked_fptosi32(unbox(Float32,x)))
convert(::Type{Int32}, x::Float64) = box(Int32,trunc_int(Int32,checked_fptosi64(unbox(Float64,x))))

convert(::Type{Uint32}, x::Float32) = box(Uint32,checked_fptoui32(unbox(Float32,x)))
convert(::Type{Uint32}, x::Float64) = box(Uint32,trunc_int(Uint32,checked_fptoui64(unbox(Float64,x))))

convert(::Type{Int64}, x::Float32) = box(Int64,checked_fptosi64(fpext64(unbox(Float32,x))))
convert(::Type{Int64}, x::Float64) = box(Int64,checked_fptosi64(unbox(Float64,x)))

convert(::Type{Uint64}, x::Float32) = box(Uint64,checked_fptoui64(fpext64(unbox(Float32,x))))
convert(::Type{Uint64}, x::Float64) = box(Uint64,checked_fptoui64(unbox(Float64,x)))

function convert(::Type{Int128}, x::Float64)
    ax = abs(x)
    top = trunc(ldexp(ax,-64))
    bot = ax - ldexp(top,64)
    n = int128(convert(Uint64,top))<<64 + int128(convert(Uint64,bot))
    return x<0 ? -n : n
end
convert(::Type{Int128}, x::Float32) = convert(Int128, float64(x))

function convert(::Type{Uint128}, x::Float64)
    ax = abs(x)
    top = trunc(ldexp(ax,-64))
    bot = ax - ldexp(top,64)
    n = uint128(convert(Uint64,top))<<64 + uint128(convert(Uint64,bot))
    return x<0 ? -n : n
end
convert(::Type{Uint128}, x::Float32) = convert(Uint128, float64(x))

convert(::Type{Char}, x::Float32) = char(convert(Int, x))
convert(::Type{Char}, x::Float64) = char(convert(Int, x))

convert(::Type{Signed}, x::Uint8  ) = convert(Int,x)
convert(::Type{Signed}, x::Uint16 ) = convert(Int,x)
convert(::Type{Signed}, x::Uint32 ) = convert(Int,x)
convert(::Type{Signed}, x::Uint64 ) = convert(Int64,x)
convert(::Type{Signed}, x::Uint128) = convert(Int128,x)
convert(::Type{Signed}, x::Float32) = convert(Int,x)
convert(::Type{Signed}, x::Float64) = convert(Int,x)

convert(::Type{Unsigned}, x::Int8   ) = convert(Uint,x)
convert(::Type{Unsigned}, x::Int16  ) = convert(Uint,x)
convert(::Type{Unsigned}, x::Int32  ) = convert(Uint,x)
convert(::Type{Unsigned}, x::Int64  ) = convert(Uint64,x)
convert(::Type{Unsigned}, x::Int128 ) = convert(Uint128,x)
convert(::Type{Unsigned}, x::Float32) = convert(Uint,x)
convert(::Type{Unsigned}, x::Float64) = convert(Uint,x)

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

## system word size ##

const WORD_SIZE = int(Int.nbits)

## integer promotions ##

promote_rule(::Type{Int16},  ::Type{Int8} ) = Int
promote_rule(::Type{Int32},  ::Type{Int8} ) = Int
promote_rule(::Type{Int32},  ::Type{Int16}) = Int
promote_rule(::Type{Int64},  ::Type{Int8} ) = Int64
promote_rule(::Type{Int64},  ::Type{Int16}) = Int64
promote_rule(::Type{Int64},  ::Type{Int32}) = Int64
promote_rule(::Type{Int128}, ::Type{Int8} ) = Int128
promote_rule(::Type{Int128}, ::Type{Int16}) = Int128
promote_rule(::Type{Int128}, ::Type{Int32}) = Int128
promote_rule(::Type{Int128}, ::Type{Int64}) = Int128

promote_rule(::Type{Uint16},  ::Type{Uint8} ) = Uint
promote_rule(::Type{Uint32},  ::Type{Uint8} ) = Uint
promote_rule(::Type{Uint32},  ::Type{Uint16}) = Uint
promote_rule(::Type{Uint64},  ::Type{Uint8} ) = Uint64
promote_rule(::Type{Uint64},  ::Type{Uint16}) = Uint64
promote_rule(::Type{Uint64},  ::Type{Uint32}) = Uint64
promote_rule(::Type{Uint128}, ::Type{Uint8} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint16}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint32}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint64}) = Uint128

promote_rule(::Type{Uint8}, ::Type{Int8}  ) = Int
promote_rule(::Type{Uint8}, ::Type{Int16} ) = Int
promote_rule(::Type{Uint8}, ::Type{Int32} ) = Int
promote_rule(::Type{Uint8}, ::Type{Int64} ) = Int64
promote_rule(::Type{Uint8}, ::Type{Int128}) = Int128

promote_rule(::Type{Uint16}, ::Type{Int8}  ) = Int
promote_rule(::Type{Uint16}, ::Type{Int16} ) = Int
promote_rule(::Type{Uint16}, ::Type{Int32} ) = Int
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

morebits(::Type{Int8}) = Int16
morebits(::Type{Int16}) = Int32
morebits(::Type{Int32}) = Int64
morebits(::Type{Int64}) = Int128
morebits(::Type{Uint8}) = Uint16
morebits(::Type{Uint16}) = Uint32
morebits(::Type{Uint32}) = Uint64
morebits(::Type{Uint64}) = Uint128

## float to integer coercion ##

# requires int arithmetic defined, for the loops to work

for f in (:int, :int8, :int16, :int32, :signed, :integer)
    @eval ($f)(x::FloatingPoint) = ($f)(iround(x))
end

for (f,t) in ((:uint8,:Uint8), (:uint16,:Uint16), (:uint32,:Uint32),
              (:int64,:Int64), (:uint64,:Uint64),
              (:int128,:Int128), (:uint128,:Uint128),
              (:unsigned,:Uint), (:uint,:Uint))
    @eval ($f)(x::FloatingPoint) = iround($t,x)
end

## wide multiplication, Int128 multiply and divide ##

widemul(x::Int32, y::Int32) = int64(x)*int64(y)
widemul(x::Uint32, y::Uint32) = uint64(x)*uint64(y)

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
        high = u1*v1 + w2 + (w1 >> 32)
        lo = w0&0xffffffff + (w1 << 32)
        int128(high)<<64 + int128(uint128(lo))
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
        high = u1*v1 + w2 + (w1 >>> 32)
        lo = w0&0xffffffff + (w1 << 32)
        int128(high)<<64 + int128(uint128(lo))
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
        (lolo&0xffffffffffffffff) + int128(w1)<<64
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
else
    widemul(u::Int64, v::Int64) = int128(u)*int128(v)
    widemul(u::Uint64, v::Uint64) = uint128(u)*uint128(v)

    *(x::Int128,  y::Int128)  = box(Int128,mul_int(unbox(Int128,x),unbox(Int128,y)))
    *(x::Uint128, y::Uint128) = box(Uint128,mul_int(unbox(Uint128,x),unbox(Uint128,y)))

    div(x::Int128,  y::Int128)  = box(Int128,sdiv_int(unbox(Int128,x),unbox(Int128,y)))
    div(x::Uint128, y::Uint128) = box(Uint128,udiv_int(unbox(Uint128,x),unbox(Uint128,y)))

    rem(x::Int128,  y::Int128)  = box(Int128,srem_int(unbox(Int128,x),unbox(Int128,y)))
    rem(x::Uint128, y::Uint128) = box(Uint128,urem_int(unbox(Uint128,x),unbox(Uint128,y)))

    mod(x::Int128, y::Int128) = box(Int128,smod_int(unbox(Int128,x),unbox(Int128,y)))
end
