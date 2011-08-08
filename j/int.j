## integer conversions ##

convert(::Type{Int8}, x::Bool   ) = boxsi8(unbox8(x))
convert(::Type{Int8}, x::Uint8  ) = boxsi8(unbox8(x))
convert(::Type{Int8}, x::Int16  ) = boxsi8(trunc8(unbox16(x)))
convert(::Type{Int8}, x::Uint16 ) = boxsi8(trunc8(unbox16(x)))
convert(::Type{Int8}, x::Char   ) = boxsi8(trunc8(unbox32(x)))
convert(::Type{Int8}, x::Int32  ) = boxsi8(trunc8(unbox32(x)))
convert(::Type{Int8}, x::Uint32 ) = boxsi8(trunc8(unbox32(x)))
convert(::Type{Int8}, x::Int64  ) = boxsi8(trunc8(unbox64(x)))
convert(::Type{Int8}, x::Uint64 ) = boxsi8(trunc8(unbox64(x)))
convert(::Type{Int8}, x::Float32) = boxsi8(fptosi8(unbox32(x)))
convert(::Type{Int8}, x::Float64) = boxsi8(fptosi8(unbox64(x)))

convert(::Type{Uint8}, x::Bool   ) = boxui8(unbox8(x))
convert(::Type{Uint8}, x::Int8   ) = boxui8(unbox8(x))
convert(::Type{Uint8}, x::Int16  ) = boxui8(trunc8(unbox16(x)))
convert(::Type{Uint8}, x::Uint16 ) = boxui8(trunc8(unbox16(x)))
convert(::Type{Uint8}, x::Char   ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Int32  ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Uint32 ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Int64  ) = boxui8(trunc8(unbox64(x)))
convert(::Type{Uint8}, x::Uint64 ) = boxui8(trunc8(unbox64(x)))
convert(::Type{Uint8}, x::Float32) = boxui8(fptoui8(unbox32(x)))
convert(::Type{Uint8}, x::Float64) = boxui8(fptoui8(unbox64(x)))

convert(::Type{Int16}, x::Bool   ) = boxsi16(sext16(unbox8(x)))
convert(::Type{Int16}, x::Int8   ) = boxsi16(sext16(unbox8(x)))
convert(::Type{Int16}, x::Uint8  ) = boxsi16(zext16(unbox8(x)))
convert(::Type{Int16}, x::Uint16 ) = boxsi16(unbox16(x))
convert(::Type{Int16}, x::Char   ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Int32  ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Uint32 ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Int64  ) = boxsi16(trunc16(unbox64(x)))
convert(::Type{Int16}, x::Uint64 ) = boxsi16(trunc16(unbox64(x)))
convert(::Type{Int16}, x::Float32) = boxsi16(fptosi16(unbox32(x)))
convert(::Type{Int16}, x::Float64) = boxsi16(fptosi16(unbox64(x)))

convert(::Type{Uint16}, x::Bool   ) = boxui16(sext16(unbox8(x)))
convert(::Type{Uint16}, x::Int8   ) = boxui16(sext16(unbox8(x)))
convert(::Type{Uint16}, x::Uint8  ) = boxui16(zext16(unbox8(x)))
convert(::Type{Uint16}, x::Int16  ) = boxui16(unbox16(x))
convert(::Type{Uint16}, x::Char   ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Int32  ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Uint32 ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Int64  ) = boxui16(trunc16(unbox64(x)))
convert(::Type{Uint16}, x::Uint64 ) = boxui16(trunc16(unbox64(x)))
convert(::Type{Uint16}, x::Float32) = boxui16(fptoui16(unbox32(x)))
convert(::Type{Uint16}, x::Float64) = boxui16(fptoui16(unbox64(x)))

convert(::Type{Int32}, x::Bool   ) = boxsi32(sext32(unbox8(x)))
convert(::Type{Int32}, x::Int8   ) = boxsi32(sext32(unbox8(x)))
convert(::Type{Int32}, x::Uint8  ) = boxsi32(zext32(unbox8(x)))
convert(::Type{Int32}, x::Int16  ) = boxsi32(sext32(unbox16(x)))
convert(::Type{Int32}, x::Uint16 ) = boxsi32(zext32(unbox16(x)))
convert(::Type{Int32}, x::Char   ) = boxsi32(unbox32(x))
convert(::Type{Int32}, x::Uint32 ) = boxsi32(unbox32(x))
convert(::Type{Int32}, x::Int64  ) = boxsi32(trunc32(unbox64(x)))
convert(::Type{Int32}, x::Uint64 ) = boxsi32(trunc32(unbox64(x)))
convert(::Type{Int32}, x::Float32) = boxsi32(fptosi32(unbox32(x)))
convert(::Type{Int32}, x::Float64) = boxsi32(fptosi32(unbox64(x)))

convert(::Type{Uint32}, x::Bool   ) = boxui32(sext32(unbox8(x)))
convert(::Type{Uint32}, x::Int8   ) = boxui32(sext32(unbox8(x)))
convert(::Type{Uint32}, x::Uint8  ) = boxui32(zext32(unbox8(x)))
convert(::Type{Uint32}, x::Int16  ) = boxui32(sext32(unbox16(x)))
convert(::Type{Uint32}, x::Uint16 ) = boxui32(zext32(unbox16(x)))
convert(::Type{Uint32}, x::Char   ) = boxui32(unbox32(x))
convert(::Type{Uint32}, x::Int32  ) = boxui32(unbox32(x))
convert(::Type{Uint32}, x::Int64  ) = boxui32(trunc32(unbox64(x)))
convert(::Type{Uint32}, x::Uint64 ) = boxui32(trunc32(unbox64(x)))
convert(::Type{Uint32}, x::Float32) = boxui32(fptoui32(unbox32(x)))
convert(::Type{Uint32}, x::Float64) = boxui32(fptoui32(unbox64(x)))

convert(::Type{Int64}, x::Bool   ) = boxsi64(sext64(unbox8(x)))
convert(::Type{Int64}, x::Int8   ) = boxsi64(sext64(unbox8(x)))
convert(::Type{Int64}, x::Uint8  ) = boxsi64(zext64(unbox8(x)))
convert(::Type{Int64}, x::Int16  ) = boxsi64(sext64(unbox16(x)))
convert(::Type{Int64}, x::Uint16 ) = boxsi64(zext64(unbox16(x)))
convert(::Type{Int64}, x::Char   ) = boxsi64(zext64(unbox32(x)))
convert(::Type{Int64}, x::Int32  ) = boxsi64(sext64(unbox32(x)))
convert(::Type{Int64}, x::Uint32 ) = boxsi64(zext64(unbox32(x)))
convert(::Type{Int64}, x::Uint64 ) = boxsi64(unbox64(x))
convert(::Type{Int64}, x::Float32) = boxsi64(fptosi64(unbox32(x)))
convert(::Type{Int64}, x::Float64) = boxsi64(fptosi64(unbox64(x)))

convert(::Type{Uint64}, x::Bool   ) = boxui64(sext64(unbox8(x)))
convert(::Type{Uint64}, x::Int8   ) = boxui64(sext64(unbox8(x)))
convert(::Type{Uint64}, x::Uint8  ) = boxui64(zext64(unbox8(x)))
convert(::Type{Uint64}, x::Int16  ) = boxui64(sext64(unbox16(x)))
convert(::Type{Uint64}, x::Uint16 ) = boxui64(zext64(unbox16(x)))
convert(::Type{Uint64}, x::Char   ) = boxui64(zext64(unbox32(x)))
convert(::Type{Uint64}, x::Int32  ) = boxui64(sext64(unbox32(x)))
convert(::Type{Uint64}, x::Uint32 ) = boxui64(zext64(unbox32(x)))
convert(::Type{Uint64}, x::Int64  ) = boxui64(unbox64(x))
convert(::Type{Uint64}, x::Float32) = boxui64(fptoui64(unbox32(x)))
convert(::Type{Uint64}, x::Float64) = boxui64(fptoui64(unbox64(x)))

convert(::Type{Int}, x::Bool   ) = convert(Int8,  x)
convert(::Type{Int}, x::Float32) = convert(Int32, x)
convert(::Type{Int}, x::Float64) = convert(Int64, x)

convert(::Type{Uint}, x::Bool   ) = convert(Uint8,  x)
convert(::Type{Uint}, x::Int8   ) = convert(Uint16, x)
convert(::Type{Uint}, x::Int16  ) = convert(Uint32, x)
convert(::Type{Uint}, x::Int32  ) = convert(Uint64, x)
convert(::Type{Uint}, x::Int64  ) = convert(Uint64, x) # LOSSY
convert(::Type{Uint}, x::Float32) = convert(Uint32, x)
convert(::Type{Uint}, x::Float64) = convert(Uint64, x)

int8  (x) = convert(Int8,   x)
uint8 (x) = convert(Uint8,  x)
int16 (x) = convert(Int16,  x)
uint16(x) = convert(Uint16, x)
int32 (x) = convert(Int32,  x)
uint32(x) = convert(Uint32, x)
int64 (x) = convert(Int64,  x)
uint64(x) = convert(Uint64, x)

int (x) = convert(Int,  x)
uint(x) = convert(Uint, x)

signed(x::Int) = x
signed(x::Uint8 ) = convert(Int8 , x)
signed(x::Uint16) = convert(Int16, x)
signed(x::Uint32) = convert(Int32, x)
signed(x::Uint64) = convert(Int64, x)

round(x::Int)  = x
iround(x::Int) = x
trunc(x::Int)  = x
floor(x::Int)  = x
ceil(x::Int)   = x

## integer promotions ##

promote_rule(::Type{Int16}, ::Type{Int8} ) = Int16
promote_rule(::Type{Int32}, ::Type{Int8} ) = Int32
promote_rule(::Type{Int32}, ::Type{Int16}) = Int32
promote_rule(::Type{Int64}, ::Type{Int8} ) = Int64
promote_rule(::Type{Int64}, ::Type{Int16}) = Int64
promote_rule(::Type{Int64}, ::Type{Int32}) = Int64

promote_rule(::Type{Uint16}, ::Type{Uint8} ) = Uint16
promote_rule(::Type{Uint32}, ::Type{Uint8} ) = Uint32
promote_rule(::Type{Uint32}, ::Type{Uint16}) = Uint32
promote_rule(::Type{Uint64}, ::Type{Uint8} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint16}) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint32}) = Uint64

promote_rule(::Type{Uint8} , ::Type{Int8} ) = Int16
promote_rule(::Type{Uint8} , ::Type{Int16}) = Int16
promote_rule(::Type{Uint8} , ::Type{Int32}) = Int32
promote_rule(::Type{Uint8} , ::Type{Int64}) = Int64

promote_rule(::Type{Uint16}, ::Type{Int8} ) = Int32
promote_rule(::Type{Uint16}, ::Type{Int16}) = Int32
promote_rule(::Type{Uint16}, ::Type{Int32}) = Int32
promote_rule(::Type{Uint16}, ::Type{Int64}) = Int64

promote_rule(::Type{Uint32}, ::Type{Int8} ) = Int64
promote_rule(::Type{Uint32}, ::Type{Int16}) = Int64
promote_rule(::Type{Uint32}, ::Type{Int32}) = Int64
promote_rule(::Type{Uint32}, ::Type{Int64}) = Int64

promote_rule(::Type{Uint64}, ::Type{Int8} ) = Int64 # LOSSY
promote_rule(::Type{Uint64}, ::Type{Int16}) = Int64 # LOSSY
promote_rule(::Type{Uint64}, ::Type{Int32}) = Int64 # LOSSY
promote_rule(::Type{Uint64}, ::Type{Int64}) = Int64 # LOSSY

## integer arithmetic ##

-(x::Int8 ) = boxsi8 (neg_int(unbox8 (x)))
-(x::Int16) = boxsi16(neg_int(unbox16(x)))
-(x::Int32) = boxsi32(neg_int(unbox32(x)))
-(x::Int64) = boxsi64(neg_int(unbox64(x)))

-(x::Uint8 ) = boxsi16(neg_int(zext16(unbox8 (x))))
-(x::Uint16) = boxsi32(neg_int(zext32(unbox16(x))))
-(x::Uint32) = boxsi64(neg_int(zext64(unbox32(x))))
-(x::Uint64) = boxsi64(neg_int(zext64(unbox64(x)))) # LOSSY

+(x::Int8 , y::Int8 ) = boxsi8 (add_int(unbox8 (x), unbox8 (y)))
+(x::Int16, y::Int16) = boxsi16(add_int(unbox16(x), unbox16(y)))
+(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
+(x::Int64, y::Int64) = boxsi64(add_int(unbox64(x), unbox64(y)))

+(x::Uint8 , y::Uint8 ) = boxui8 (add_int(unbox8 (x), unbox8 (y)))
+(x::Uint16, y::Uint16) = boxui16(add_int(unbox16(x), unbox16(y)))
+(x::Uint32, y::Uint32) = boxui32(add_int(unbox32(x), unbox32(y)))
+(x::Uint64, y::Uint64) = boxui64(add_int(unbox64(x), unbox64(y)))

-(x::Int8 , y::Int8 ) = boxsi8 (sub_int(unbox8 (x), unbox8 (y)))
-(x::Int16, y::Int16) = boxsi16(sub_int(unbox16(x), unbox16(y)))
-(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
-(x::Int64, y::Int64) = boxsi64(sub_int(unbox64(x), unbox64(y)))

-(x::Uint8 , y::Uint8 ) = boxui8 (sub_int(unbox8 (x), unbox8 (y)))
-(x::Uint16, y::Uint16) = boxui16(sub_int(unbox16(x), unbox16(y)))
-(x::Uint32, y::Uint32) = boxui32(sub_int(unbox32(x), unbox32(y)))
-(x::Uint64, y::Uint64) = boxui64(sub_int(unbox64(x), unbox64(y)))

*(x::Int8 , y::Int8 ) = boxsi8 (mul_int(unbox8 (x), unbox8 (y)))
*(x::Int16, y::Int16) = boxsi16(mul_int(unbox16(x), unbox16(y)))
*(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
*(x::Int64, y::Int64) = boxsi64(mul_int(unbox64(x), unbox64(y)))

*(x::Uint8 , y::Uint8 ) = boxui8 (mul_int(unbox8 (x), unbox8 (y)))
*(x::Uint16, y::Uint16) = boxui16(mul_int(unbox16(x), unbox16(y)))
*(x::Uint32, y::Uint32) = boxui32(mul_int(unbox32(x), unbox32(y)))
*(x::Uint64, y::Uint64) = boxui64(mul_int(unbox64(x), unbox64(y)))

/(x::Int, y::Int) = float64(x)/float64(y)

div(x::Int8 , y::Int8 ) = boxsi8 (sdiv_int(unbox8 (x), unbox8 (y)))
div(x::Int16, y::Int16) = boxsi16(sdiv_int(unbox16(x), unbox16(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
div(x::Int64, y::Int64) = boxsi64(sdiv_int(unbox64(x), unbox64(y)))

div(x::Uint8 , y::Uint8 ) = boxui8 (udiv_int(unbox8 (x), unbox8 (y)))
div(x::Uint16, y::Uint16) = boxui16(udiv_int(unbox16(x), unbox16(y)))
div(x::Uint32, y::Uint32) = boxui32(udiv_int(unbox32(x), unbox32(y)))
div(x::Uint64, y::Uint64) = boxui64(udiv_int(unbox64(x), unbox64(y)))

fld{T<:Uint}(x::T, y::T) = div(x,y)
# TODO: faster signed int fld?

rem(x::Int8 , y::Int8 ) = boxsi8 (srem_int(unbox8 (x), unbox8 (y)))
rem(x::Int16, y::Int16) = boxsi16(srem_int(unbox16(x), unbox16(y)))
rem(x::Int32, y::Int32) = boxsi32(srem_int(unbox32(x), unbox32(y)))
rem(x::Int64, y::Int64) = boxsi64(srem_int(unbox64(x), unbox64(y)))

rem(x::Uint8 , y::Uint8 ) = boxui8 (urem_int(unbox8 (x), unbox8 (y)))
rem(x::Uint16, y::Uint16) = boxui16(urem_int(unbox16(x), unbox16(y)))
rem(x::Uint32, y::Uint32) = boxui32(urem_int(unbox32(x), unbox32(y)))
rem(x::Uint64, y::Uint64) = boxui64(urem_int(unbox64(x), unbox64(y)))

mod{T<:Uint}(x::T, y::T) = rem(x,y)
mod{T<:Int }(x::T, y::T) = rem(y+rem(x,y),y)

## integer bitwise operations ##

~(x::Int8 ) = boxsi8 (not_int(unbox8 (x)))
~(x::Int16) = boxsi16(not_int(unbox16(x)))
~(x::Int32) = boxsi32(not_int(unbox32(x)))
~(x::Int64) = boxsi64(not_int(unbox64(x)))

~(x::Uint8 ) = boxui8 (not_int(unbox8 (x)))
~(x::Uint16) = boxui16(not_int(unbox16(x)))
~(x::Uint32) = boxui32(not_int(unbox32(x)))
~(x::Uint64) = boxui64(not_int(unbox64(x)))

&(x::Int8 , y::Int8 ) = boxsi8 (and_int(unbox8 (x), unbox8 (y)))
&(x::Int16, y::Int16) = boxsi16(and_int(unbox16(x), unbox16(y)))
&(x::Int32, y::Int32) = boxsi32(and_int(unbox32(x), unbox32(y)))
&(x::Int64, y::Int64) = boxsi64(and_int(unbox64(x), unbox64(y)))

&(x::Uint8 , y::Uint8 ) = boxui8 (and_int(unbox8 (x), unbox8 (y)))
&(x::Uint16, y::Uint16) = boxui16(and_int(unbox16(x), unbox16(y)))
&(x::Uint32, y::Uint32) = boxui32(and_int(unbox32(x), unbox32(y)))
&(x::Uint64, y::Uint64) = boxui64(and_int(unbox64(x), unbox64(y)))

|(x::Int8 , y::Int8 ) = boxsi8 (or_int(unbox8 (x), unbox8 (y)))
|(x::Int16, y::Int16) = boxsi16(or_int(unbox16(x), unbox16(y)))
|(x::Int32, y::Int32) = boxsi32(or_int(unbox32(x), unbox32(y)))
|(x::Int64, y::Int64) = boxsi64(or_int(unbox64(x), unbox64(y)))

|(x::Uint8 , y::Uint8 ) = boxui8 (or_int(unbox8 (x), unbox8 (y)))
|(x::Uint16, y::Uint16) = boxui16(or_int(unbox16(x), unbox16(y)))
|(x::Uint32, y::Uint32) = boxui32(or_int(unbox32(x), unbox32(y)))
|(x::Uint64, y::Uint64) = boxui64(or_int(unbox64(x), unbox64(y)))

$(x::Int8 , y::Int8 ) = boxsi8 (xor_int(unbox8 (x), unbox8 (y)))
$(x::Int16, y::Int16) = boxsi16(xor_int(unbox16(x), unbox16(y)))
$(x::Int32, y::Int32) = boxsi32(xor_int(unbox32(x), unbox32(y)))
$(x::Int64, y::Int64) = boxsi64(xor_int(unbox64(x), unbox64(y)))

$(x::Uint8 , y::Uint8 ) = boxui8 (xor_int(unbox8 (x), unbox8 (y)))
$(x::Uint16, y::Uint16) = boxui16(xor_int(unbox16(x), unbox16(y)))
$(x::Uint32, y::Uint32) = boxui32(xor_int(unbox32(x), unbox32(y)))
$(x::Uint64, y::Uint64) = boxui64(xor_int(unbox64(x), unbox64(y)))

<<(x::Int8 , y::Int32) = boxsi8 (shl_int(unbox8 (x), unbox32(y)))
<<(x::Int16, y::Int32) = boxsi16(shl_int(unbox16(x), unbox32(y)))
<<(x::Int32, y::Int32) = boxsi32(shl_int(unbox32(x), unbox32(y)))
<<(x::Int64, y::Int32) = boxsi64(shl_int(unbox64(x), unbox32(y)))

<<(x::Uint8 , y::Int32) = boxui8 (shl_int(unbox8 (x), unbox32(y)))
<<(x::Uint16, y::Int32) = boxui16(shl_int(unbox16(x), unbox32(y)))
<<(x::Uint32, y::Int32) = boxui32(shl_int(unbox32(x), unbox32(y)))
<<(x::Uint64, y::Int32) = boxui64(shl_int(unbox64(x), unbox32(y)))

>>(x::Int8 , y::Int32) = boxsi8 (ashr_int(unbox8 (x), unbox32(y)))
>>(x::Int16, y::Int32) = boxsi16(ashr_int(unbox16(x), unbox32(y)))
>>(x::Int32, y::Int32) = boxsi32(ashr_int(unbox32(x), unbox32(y)))
>>(x::Int64, y::Int32) = boxsi64(ashr_int(unbox64(x), unbox32(y)))

>>(x::Uint8 , y::Int32) = boxui8 (lshr_int(unbox8 (x), unbox32(y)))
>>(x::Uint16, y::Int32) = boxui16(lshr_int(unbox16(x), unbox32(y)))
>>(x::Uint32, y::Int32) = boxui32(lshr_int(unbox32(x), unbox32(y)))
>>(x::Uint64, y::Int32) = boxui64(lshr_int(unbox64(x), unbox32(y)))

>>>(x::Int8 , y::Int32) = boxsi8 (lshr_int(unbox8 (x), unbox32(y)))
>>>(x::Int16, y::Int32) = boxsi16(lshr_int(unbox16(x), unbox32(y)))
>>>(x::Int32, y::Int32) = boxsi32(lshr_int(unbox32(x), unbox32(y)))
>>>(x::Int64, y::Int32) = boxsi64(lshr_int(unbox64(x), unbox32(y)))

>>>(x::Uint8 , y::Int32) = boxui8 (lshr_int(unbox8 (x), unbox32(y)))
>>>(x::Uint16, y::Int32) = boxui16(lshr_int(unbox16(x), unbox32(y)))
>>>(x::Uint32, y::Int32) = boxui32(lshr_int(unbox32(x), unbox32(y)))
>>>(x::Uint64, y::Int32) = boxui64(lshr_int(unbox64(x), unbox32(y)))

bswap(x::Int8)   = x
bswap(x::Uint8)  = x
bswap(x::Int16)  = boxsi16(bswap_int(unbox16(x)))
bswap(x::Uint16) = boxui16(bswap_int(unbox16(x)))
bswap(x::Int32)  = boxsi32(bswap_int(unbox32(x)))
bswap(x::Uint32) = boxui32(bswap_int(unbox32(x)))
bswap(x::Int64)  = boxsi64(bswap_int(unbox64(x)))
bswap(x::Uint64) = boxui64(bswap_int(unbox64(x)))

## integer comparisons ##

==(x::Int8 , y::Int8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Int16, y::Int16) = eq_int(unbox16(x),unbox16(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
==(x::Int64, y::Int64) = eq_int(unbox64(x),unbox64(y))

==(x::Uint8 , y::Uint8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Uint16, y::Uint16) = eq_int(unbox16(x),unbox16(y))
==(x::Uint32, y::Uint32) = eq_int(unbox32(x),unbox32(y))
==(x::Uint64, y::Uint64) = eq_int(unbox64(x),unbox64(y))

!=(x::Int8 , y::Int8 ) = ne_int(unbox8 (x),unbox8 (y))
!=(x::Int16, y::Int16) = ne_int(unbox16(x),unbox16(y))
!=(x::Int32, y::Int32) = ne_int(unbox32(x),unbox32(y))
!=(x::Int64, y::Int64) = ne_int(unbox64(x),unbox64(y))

!=(x::Uint8 , y::Uint8 ) = ne_int(unbox8 (x),unbox8 (y))
!=(x::Uint16, y::Uint16) = ne_int(unbox16(x),unbox16(y))
!=(x::Uint32, y::Uint32) = ne_int(unbox32(x),unbox32(y))
!=(x::Uint64, y::Uint64) = ne_int(unbox64(x),unbox64(y))

<(x::Int8 , y::Int8 ) = slt_int(unbox8 (x),unbox8 (y))
<(x::Int16, y::Int16) = slt_int(unbox16(x),unbox16(y))
<(x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
<(x::Int64, y::Int64) = slt_int(unbox64(x),unbox64(y))

<(x::Uint8 , y::Uint8 ) = ult_int(unbox8 (x),unbox8 (y))
<(x::Uint16, y::Uint16) = ult_int(unbox16(x),unbox16(y))
<(x::Uint32, y::Uint32) = ult_int(unbox32(x),unbox32(y))
<(x::Uint64, y::Uint64) = ult_int(unbox64(x),unbox64(y))

<=(x::Int8 , y::Int8 ) = sle_int(unbox8 (x),unbox8 (y))
<=(x::Int16, y::Int16) = sle_int(unbox16(x),unbox16(y))
<=(x::Int32, y::Int32) = sle_int(unbox32(x),unbox32(y))
<=(x::Int64, y::Int64) = sle_int(unbox64(x),unbox64(y))

<=(x::Uint8 , y::Uint8 ) = ule_int(unbox8 (x),unbox8 (y))
<=(x::Uint16, y::Uint16) = ule_int(unbox16(x),unbox16(y))
<=(x::Uint32, y::Uint32) = ule_int(unbox32(x),unbox32(y))
<=(x::Uint64, y::Uint64) = ule_int(unbox64(x),unbox64(y))

## traits ##

typemin(::Type{Int8  }) = int8(-128)
typemax(::Type{Int8  }) = int8(127)
typemin(::Type{Uint8 }) = uint8(0)
typemax(::Type{Uint8 }) = uint8(255)
typemin(::Type{Int16 }) = int16(-32768)
typemax(::Type{Int16 }) = int16(32767)
typemin(::Type{Uint16}) = uint16(0)
typemax(::Type{Uint16}) = uint16(65535)
typemin(::Type{Int32 }) = (-2147483647-1)
typemax(::Type{Int32 }) = 2147483647
typemin(::Type{Uint32}) = uint32(0)
typemax(::Type{Uint32}) = uint32(4294967295)
typemin(::Type{Int64 }) = (-9223372036854775807-1)
typemax(::Type{Int64 }) = 9223372036854775807
typemin(::Type{Uint64}) = uint64(0)
typemax(::Type{Uint64}) = 18446744073709551615

sizeof(::Type{Int8})   = 1
sizeof(::Type{Uint8})  = 1
sizeof(::Type{Int16})  = 2
sizeof(::Type{Uint16}) = 2
sizeof(::Type{Int32})  = 4
sizeof(::Type{Uint32}) = 4
sizeof(::Type{Int64})  = 8
sizeof(::Type{Uint64}) = 8
