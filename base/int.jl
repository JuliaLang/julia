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
convert(::Type{Int8}, x::Int128 ) = boxsi8(trunc8(unbox(Int128,x)))
convert(::Type{Int8}, x::Uint128) = boxsi8(trunc8(unbox(Uint128,x)))
convert(::Type{Int8}, x::Float32) = boxsi8(trunc8(fpsiround32(unbox32(x))))
convert(::Type{Int8}, x::Float64) = boxsi8(trunc8(fpsiround64(unbox64(x))))

convert(::Type{Uint8}, x::Bool   ) = boxui8(unbox8(x))
convert(::Type{Uint8}, x::Int8   ) = boxui8(unbox8(x))
convert(::Type{Uint8}, x::Int16  ) = boxui8(trunc8(unbox16(x)))
convert(::Type{Uint8}, x::Uint16 ) = boxui8(trunc8(unbox16(x)))
convert(::Type{Uint8}, x::Char   ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Int32  ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Uint32 ) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Int64  ) = boxui8(trunc8(unbox64(x)))
convert(::Type{Uint8}, x::Uint64 ) = boxui8(trunc8(unbox64(x)))
convert(::Type{Uint8}, x::Int128 ) = boxui8(trunc8(unbox(Int128,x)))
convert(::Type{Uint8}, x::Uint128) = boxui8(trunc8(unbox(Uint128,x)))
convert(::Type{Uint8}, x::Float32) = boxui8(trunc8(fpuiround32(unbox32(x))))
convert(::Type{Uint8}, x::Float64) = boxui8(trunc8(fpuiround64(unbox64(x))))

convert(::Type{Int16}, x::Bool   ) = boxsi16(zext16(unbox8(x)))
convert(::Type{Int16}, x::Int8   ) = boxsi16(sext16(unbox8(x)))
convert(::Type{Int16}, x::Uint8  ) = boxsi16(zext16(unbox8(x)))
convert(::Type{Int16}, x::Uint16 ) = boxsi16(unbox16(x))
convert(::Type{Int16}, x::Char   ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Int32  ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Uint32 ) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Int64  ) = boxsi16(trunc16(unbox64(x)))
convert(::Type{Int16}, x::Uint64 ) = boxsi16(trunc16(unbox64(x)))
convert(::Type{Int16}, x::Int128 ) = boxsi16(trunc16(unbox(Int128,x)))
convert(::Type{Int16}, x::Uint128) = boxsi16(trunc16(unbox(Uint128,x)))
convert(::Type{Int16}, x::Float32) = boxsi16(trunc16(fpsiround32(unbox32(x))))
convert(::Type{Int16}, x::Float64) = boxsi16(trunc16(fpsiround64(unbox64(x))))

convert(::Type{Uint16}, x::Bool   ) = boxui16(zext16(unbox8(x)))
convert(::Type{Uint16}, x::Int8   ) = boxui16(sext16(unbox8(x)))
convert(::Type{Uint16}, x::Uint8  ) = boxui16(zext16(unbox8(x)))
convert(::Type{Uint16}, x::Int16  ) = boxui16(unbox16(x))
convert(::Type{Uint16}, x::Char   ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Int32  ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Uint32 ) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Int64  ) = boxui16(trunc16(unbox64(x)))
convert(::Type{Uint16}, x::Uint64 ) = boxui16(trunc16(unbox64(x)))
convert(::Type{Uint16}, x::Int128 ) = boxui16(trunc16(unbox(Int128,x)))
convert(::Type{Uint16}, x::Uint128) = boxui16(trunc16(unbox(Uint128,x)))
convert(::Type{Uint16}, x::Float32) = boxui16(trunc16(fpuiround32(unbox32(x))))
convert(::Type{Uint16}, x::Float64) = boxui16(trunc16(fpuiround64(unbox64(x))))

convert(::Type{Int32}, x::Bool   ) = boxsi32(zext32(unbox8(x)))
convert(::Type{Int32}, x::Int8   ) = boxsi32(sext32(unbox8(x)))
convert(::Type{Int32}, x::Uint8  ) = boxsi32(zext32(unbox8(x)))
convert(::Type{Int32}, x::Int16  ) = boxsi32(sext32(unbox16(x)))
convert(::Type{Int32}, x::Uint16 ) = boxsi32(zext32(unbox16(x)))
convert(::Type{Int32}, x::Char   ) = boxsi32(unbox32(x))
convert(::Type{Int32}, x::Uint32 ) = boxsi32(unbox32(x))
convert(::Type{Int32}, x::Int64  ) = boxsi32(trunc32(unbox64(x)))
convert(::Type{Int32}, x::Uint64 ) = boxsi32(trunc32(unbox64(x)))
convert(::Type{Int32}, x::Int128 ) = boxsi32(trunc32(unbox(Int128,x)))
convert(::Type{Int32}, x::Uint128) = boxsi32(trunc32(unbox(Uint128,x)))
convert(::Type{Int32}, x::Float32) = boxsi32(fpsiround32(unbox32(x)))
convert(::Type{Int32}, x::Float64) = boxsi32(trunc32(fpsiround64(unbox64(x))))

convert(::Type{Uint32}, x::Bool   ) = boxui32(zext32(unbox8(x)))
convert(::Type{Uint32}, x::Int8   ) = boxui32(sext32(unbox8(x)))
convert(::Type{Uint32}, x::Uint8  ) = boxui32(zext32(unbox8(x)))
convert(::Type{Uint32}, x::Int16  ) = boxui32(sext32(unbox16(x)))
convert(::Type{Uint32}, x::Uint16 ) = boxui32(zext32(unbox16(x)))
convert(::Type{Uint32}, x::Char   ) = boxui32(unbox32(x))
convert(::Type{Uint32}, x::Int32  ) = boxui32(unbox32(x))
convert(::Type{Uint32}, x::Int64  ) = boxui32(trunc32(unbox64(x)))
convert(::Type{Uint32}, x::Uint64 ) = boxui32(trunc32(unbox64(x)))
convert(::Type{Uint32}, x::Int128 ) = boxui32(trunc32(unbox(Int128,x)))
convert(::Type{Uint32}, x::Uint128) = boxui32(trunc32(unbox(Uint128,x)))
convert(::Type{Uint32}, x::Float32) = boxui32(fpuiround32(unbox32(x)))
convert(::Type{Uint32}, x::Float64) = boxui32(trunc32(fpuiround64(unbox64(x))))

convert(::Type{Int64}, x::Bool   ) = boxsi64(zext64(unbox8(x)))
convert(::Type{Int64}, x::Int8   ) = boxsi64(sext64(unbox8(x)))
convert(::Type{Int64}, x::Uint8  ) = boxsi64(zext64(unbox8(x)))
convert(::Type{Int64}, x::Int16  ) = boxsi64(sext64(unbox16(x)))
convert(::Type{Int64}, x::Uint16 ) = boxsi64(zext64(unbox16(x)))
convert(::Type{Int64}, x::Char   ) = boxsi64(zext64(unbox32(x)))
convert(::Type{Int64}, x::Int32  ) = boxsi64(sext64(unbox32(x)))
convert(::Type{Int64}, x::Uint32 ) = boxsi64(zext64(unbox32(x)))
convert(::Type{Int64}, x::Uint64 ) = boxsi64(unbox64(x))
convert(::Type{Int64}, x::Int128 ) = boxsi64(trunc64(unbox(Int128,x)))
convert(::Type{Int64}, x::Uint128) = boxsi64(trunc64(unbox(Uint128,x)))
convert(::Type{Int64}, x::Float32) = boxsi64(fpsiround64(fpext64(unbox32(x))))
convert(::Type{Int64}, x::Float64) = boxsi64(fpsiround64(unbox64(x)))

convert(::Type{Uint64}, x::Bool   ) = boxui64(zext64(unbox8(x)))
convert(::Type{Uint64}, x::Int8   ) = boxui64(sext64(unbox8(x)))
convert(::Type{Uint64}, x::Uint8  ) = boxui64(zext64(unbox8(x)))
convert(::Type{Uint64}, x::Int16  ) = boxui64(sext64(unbox16(x)))
convert(::Type{Uint64}, x::Uint16 ) = boxui64(zext64(unbox16(x)))
convert(::Type{Uint64}, x::Char   ) = boxui64(zext64(unbox32(x)))
convert(::Type{Uint64}, x::Int32  ) = boxui64(sext64(unbox32(x)))
convert(::Type{Uint64}, x::Uint32 ) = boxui64(zext64(unbox32(x)))
convert(::Type{Uint64}, x::Int64  ) = boxui64(unbox64(x))
convert(::Type{Uint64}, x::Int128 ) = boxui64(trunc64(unbox(Int128,x)))
convert(::Type{Uint64}, x::Uint128) = boxui64(trunc64(unbox(Uint128,x)))
convert(::Type{Uint64}, x::Float32) = boxui64(fpuiround64(fpext64(unbox32(x))))
convert(::Type{Uint64}, x::Float64) = boxui64(fpuiround64(unbox64(x)))

convert(::Type{Int128}, x::Bool   ) = box(Int128,zext_int(Int128,unbox8(x)))
convert(::Type{Int128}, x::Int8   ) = box(Int128,sext_int(Int128,unbox8(x)))
convert(::Type{Int128}, x::Uint8  ) = box(Int128,zext_int(Uint128,unbox8(x)))
convert(::Type{Int128}, x::Int16  ) = box(Int128,sext_int(Int128,unbox16(x)))
convert(::Type{Int128}, x::Uint16 ) = box(Int128,zext_int(Uint128,unbox16(x)))
convert(::Type{Int128}, x::Char   ) = box(Int128,zext_int(Uint128,unbox32(x)))
convert(::Type{Int128}, x::Int32  ) = box(Int128,sext_int(Int128,unbox32(x)))
convert(::Type{Int128}, x::Uint32 ) = box(Int128,zext_int(Uint128,unbox32(x)))
convert(::Type{Int128}, x::Int64  ) = box(Int128,sext_int(Int128,unbox64(x)))
convert(::Type{Int128}, x::Uint64 ) = box(Int128,zext_int(Uint128,unbox64(x)))
convert(::Type{Int128}, x::Uint128) = box(Int128,unbox(Uint128,x))
# TODO: convert(::Type{Int128}, x::Float32)
# TODO: convert(::Type{Int128}, x::Float64)

convert(::Type{Uint128}, x::Bool   ) = box(Uint128,zext_int(Int128,unbox8(x)))
convert(::Type{Uint128}, x::Int8   ) = box(Uint128,sext_int(Int128,unbox8(x)))
convert(::Type{Uint128}, x::Uint8  ) = box(Uint128,zext_int(Uint128,unbox8(x)))
convert(::Type{Uint128}, x::Int16  ) = box(Uint128,sext_int(Int128,unbox16(x)))
convert(::Type{Uint128}, x::Uint16 ) = box(Uint128,zext_int(Uint128,unbox16(x)))
convert(::Type{Uint128}, x::Char   ) = box(Uint128,zext_int(Uint128,unbox32(x)))
convert(::Type{Uint128}, x::Int32  ) = box(Uint128,sext_int(Int128,unbox32(x)))
convert(::Type{Uint128}, x::Uint32 ) = box(Uint128,zext_int(Uint128,unbox32(x)))
convert(::Type{Uint128}, x::Int64  ) = box(Uint128,sext_int(Int128,unbox64(x)))
convert(::Type{Uint128}, x::Uint64 ) = box(Uint128,zext_int(Uint128,unbox64(x)))
convert(::Type{Uint128}, x::Int128 ) = box(Uint128,unbox(Uint128,x))
# TODO: convert(::Type{Uint128}, x::Float32)
# TODO: convert(::Type{Uint128}, x::Float64)

convert(::Type{Integer}, x::Float32) = convert(Int, x)
convert(::Type{Integer}, x::Float64) = convert(Int64, x)

convert(::Type{Signed}, x::Bool   ) = convert(Int, x)
convert(::Type{Signed}, x::Char   ) = convert(Int, x)
convert(::Type{Signed}, x::Uint8  ) = convert(Int, x)
convert(::Type{Signed}, x::Uint16 ) = convert(Int, x)
convert(::Type{Signed}, x::Uint32 ) = convert(Int, x)
convert(::Type{Signed}, x::Float32) = convert(Int, x)
convert(::Type{Signed}, x::Uint64 ) = convert(Int64, x)
convert(::Type{Signed}, x::Float64) = convert(Int64, x)
convert(::Type{Signed}, x::Uint128) = convert(Int128, x)

convert(::Type{Unsigned}, x::Bool   ) = convert(Uint, x)
convert(::Type{Unsigned}, x::Char   ) = convert(Uint, x)
convert(::Type{Unsigned}, x::Int8   ) = convert(Uint, x)
convert(::Type{Unsigned}, x::Int16  ) = convert(Uint, x)
convert(::Type{Unsigned}, x::Int32  ) = convert(Uint, x)
convert(::Type{Unsigned}, x::Float32) = convert(Uint, x)
convert(::Type{Unsigned}, x::Int64  ) = convert(Uint64, x)
convert(::Type{Unsigned}, x::Float64) = convert(Uint64, x)
convert(::Type{Unsigned}, x::Int128 ) = convert(Uint128, x)

int8  (x) = convert(Int8,  x)
int16 (x) = convert(Int16, x)
int32 (x) = convert(Int32, x)
int64 (x) = convert(Int64, x)
int128(x) = convert(Int128, x)

uint8  (x) = convert(Uint8,   x)
uint16 (x) = convert(Uint16,  x)
uint32 (x) = convert(Uint32,  x)
uint64 (x) = convert(Uint64,  x)
uint128(x) = convert(Uint128, x)

integer (x) = convert(Integer,  x)
signed  (x) = convert(Signed,   x)
unsigned(x) = convert(Unsigned, x)

round(x::Integer) = x
trunc(x::Integer) = x
floor(x::Integer) = x
ceil (x::Integer) = x

iround(x::Integer) = x
itrunc(x::Integer) = x
ifloor(x::Integer) = x
iceil (x::Integer) = x

## integer arithmetic ##

-(x::Signed) = -int(x)
-(x::Unsigned) = -uint(x)

+{T<:Signed}(x::T, y::T) = int(x) + int(y)
-{T<:Signed}(x::T, y::T) = int(x) - int(y)
*{T<:Signed}(x::T, y::T) = int(x) * int(y)

+{T<:Unsigned}(x::T, y::T) = uint(x) + uint(y)
-{T<:Unsigned}(x::T, y::T) = uint(x) - uint(y)
*{T<:Unsigned}(x::T, y::T) = uint(x) * uint(y)

-(x::Int)     = boxsint(neg_int(unboxwd(x)))
-(x::Uint)    = boxuint(neg_int(unboxwd(x)))
-(x::Int64)   = boxsi64(neg_int(unbox64(x)))
-(x::Uint64)  = boxui64(neg_int(unbox64(x)))
-(x::Int128)  = box(Int128,neg_int(unbox(Int128,x)))
-(x::Uint128) = box(Uint128,neg_int(unbox(Uint128,x)))

+(x::Int,     y::Int)     = boxsint(add_int(unboxwd(x), unboxwd(y)))
+(x::Uint,    y::Uint)    = boxuint(add_int(unboxwd(x), unboxwd(y)))
+(x::Int64,   y::Int64)   = boxsi64(add_int(unbox64(x), unbox64(y)))
+(x::Uint64,  y::Uint64)  = boxui64(add_int(unbox64(x), unbox64(y)))
+(x::Int128,  y::Int128)  = box(Int128,add_int(unbox(Int128,x), unbox(Int128,y)))
+(x::Uint128, y::Uint128) = box(Uint128,add_int(unbox(Uint128,x), unbox(Uint128,y)))

-(x::Int,     y::Int)     = boxsint(sub_int(unboxwd(x), unboxwd(y)))
-(x::Uint,    y::Uint)    = boxuint(sub_int(unboxwd(x), unboxwd(y)))
-(x::Int64,   y::Int64)   = boxsi64(sub_int(unbox64(x), unbox64(y)))
-(x::Uint64,  y::Uint64)  = boxui64(sub_int(unbox64(x), unbox64(y)))
-(x::Int128,  y::Int128)  = box(Int128,sub_int(unbox(Int128,x), unbox(Int128,y)))
-(x::Uint128, y::Uint128) = box(Uint128,sub_int(unbox(Uint128,x), unbox(Uint128,y)))

*(x::Int,     y::Int)     = boxsint(mul_int(unboxwd(x), unboxwd(y)))
*(x::Uint,    y::Uint)    = boxuint(mul_int(unboxwd(x), unboxwd(y)))
*(x::Int64,   y::Int64)   = boxsi64(mul_int(unbox64(x), unbox64(y)))
*(x::Uint64,  y::Uint64)  = boxui64(mul_int(unbox64(x), unbox64(y)))
*(x::Int128,  y::Int128)  = box(Int128,mul_int(unbox(Int128,x), unbox(Int128,y)))
*(x::Uint128, y::Uint128) = box(Uint128,mul_int(unbox(Uint128,x), unbox(Uint128,y)))

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

div(x::Int,     y::Int)     = boxsint(sdiv_int(unboxwd(x), unboxwd(y)))
div(x::Uint,    y::Uint)    = boxuint(udiv_int(unboxwd(x), unboxwd(y)))
div(x::Int64,   y::Int64)   = boxsi64(sdiv_int(unbox64(x), unbox64(y)))
div(x::Uint64,  y::Uint64)  = boxui64(udiv_int(unbox64(x), unbox64(y)))
div(x::Int128,  y::Int128)  = box(Int128,sdiv_int(unbox(Int128,x), unbox(Int128,y)))
div(x::Uint128, y::Uint128) = box(Uint128,udiv_int(unbox(Uint128,x), unbox(Uint128,y)))

rem(x::Int,    y::Int)    = boxsint(srem_int(unboxwd(x), unboxwd(y)))
rem(x::Uint,   y::Uint)   = boxuint(urem_int(unboxwd(x), unboxwd(y)))
rem(x::Int64,  y::Int64)  = boxsi64(srem_int(unbox64(x), unbox64(y)))
rem(x::Uint64, y::Uint64) = boxui64(urem_int(unbox64(x), unbox64(y)))
rem(x::Int128,  y::Int128)  = box(Int128,srem_int(unbox(Int128,x), unbox(Int128,y)))
rem(x::Uint128, y::Uint128) = box(Uint128,urem_int(unbox(Uint128,x), unbox(Uint128,y)))

fld{T<:Unsigned}(x::T, y::T) = div(x,y)
fld{T<:Integer }(x::T, y::T) = div(x,y)-(signbit(x$y)&(rem(x,y)!=0))

mod(x::Int,    y::Int)    = boxsint(smod_int(unboxwd(x), unboxwd(y)))
mod(x::Int64,  y::Int64)  = boxsi64(smod_int(unbox64(x), unbox64(y)))
mod(x::Int128, y::Int128) = box(Int128,smod_int(unbox(Int128,x), unbox(Int128,y)))

## integer bitwise operations ##

~(x::Int8 ) = boxsi8 (not_int(unbox8 (x)))
~(x::Int16) = boxsi16(not_int(unbox16(x)))
~(x::Int32) = boxsi32(not_int(unbox32(x)))
~(x::Int64) = boxsi64(not_int(unbox64(x)))
~(x::Int128) = box(Int128,not_int(unbox(Int128,x)))

~(x::Uint8 ) = boxui8 (not_int(unbox8 (x)))
~(x::Uint16) = boxui16(not_int(unbox16(x)))
~(x::Uint32) = boxui32(not_int(unbox32(x)))
~(x::Uint64) = boxui64(not_int(unbox64(x)))
~(x::Uint128) = box(Uint128,not_int(unbox(Uint128,x)))

(&)(x::Int8 , y::Int8 ) = boxsi8 (and_int(unbox8 (x), unbox8 (y)))
(&)(x::Int16, y::Int16) = boxsi16(and_int(unbox16(x), unbox16(y)))
(&)(x::Int32, y::Int32) = boxsi32(and_int(unbox32(x), unbox32(y)))
(&)(x::Int64, y::Int64) = boxsi64(and_int(unbox64(x), unbox64(y)))
(&)(x::Int128, y::Int128) = box(Int128,and_int(unbox(Int128,x), unbox(Int128,y)))

(&)(x::Uint8 , y::Uint8 ) = boxui8 (and_int(unbox8 (x), unbox8 (y)))
(&)(x::Uint16, y::Uint16) = boxui16(and_int(unbox16(x), unbox16(y)))
(&)(x::Uint32, y::Uint32) = boxui32(and_int(unbox32(x), unbox32(y)))
(&)(x::Uint64, y::Uint64) = boxui64(and_int(unbox64(x), unbox64(y)))
(&)(x::Uint128, y::Uint128) = box(Uint128,and_int(unbox(Uint128,x), unbox(Uint128,y)))

|(x::Int8 , y::Int8 ) = boxsi8 (or_int(unbox8 (x), unbox8 (y)))
|(x::Int16, y::Int16) = boxsi16(or_int(unbox16(x), unbox16(y)))
|(x::Int32, y::Int32) = boxsi32(or_int(unbox32(x), unbox32(y)))
|(x::Int64, y::Int64) = boxsi64(or_int(unbox64(x), unbox64(y)))
|(x::Int128, y::Int128) = box(Int128,or_int(unbox(Int128,x), unbox(Int128,y)))

|(x::Uint8 , y::Uint8 ) = boxui8 (or_int(unbox8 (x), unbox8 (y)))
|(x::Uint16, y::Uint16) = boxui16(or_int(unbox16(x), unbox16(y)))
|(x::Uint32, y::Uint32) = boxui32(or_int(unbox32(x), unbox32(y)))
|(x::Uint64, y::Uint64) = boxui64(or_int(unbox64(x), unbox64(y)))
|(x::Uint128, y::Uint128) = box(Uint128,or_int(unbox(Uint128,x), unbox(Uint128,y)))

($)(x::Int8 , y::Int8 ) = boxsi8 (xor_int(unbox8 (x), unbox8 (y)))
($)(x::Int16, y::Int16) = boxsi16(xor_int(unbox16(x), unbox16(y)))
($)(x::Int32, y::Int32) = boxsi32(xor_int(unbox32(x), unbox32(y)))
($)(x::Int64, y::Int64) = boxsi64(xor_int(unbox64(x), unbox64(y)))
($)(x::Int128, y::Int128) = box(Int128,xor_int(unbox(Int128,x), unbox(Int128,y)))

($)(x::Uint8 , y::Uint8 ) = boxui8 (xor_int(unbox8 (x), unbox8 (y)))
($)(x::Uint16, y::Uint16) = boxui16(xor_int(unbox16(x), unbox16(y)))
($)(x::Uint32, y::Uint32) = boxui32(xor_int(unbox32(x), unbox32(y)))
($)(x::Uint64, y::Uint64) = boxui64(xor_int(unbox64(x), unbox64(y)))
($)(x::Uint128, y::Uint128) = box(Uint128,xor_int(unbox(Uint128,x), unbox(Uint128,y)))

<<(x::Int8 , y::Int32) = boxsi8 (shl_int(unbox8 (x), unbox32(y)))
<<(x::Int16, y::Int32) = boxsi16(shl_int(unbox16(x), unbox32(y)))
<<(x::Int32, y::Int32) = boxsi32(shl_int(unbox32(x), unbox32(y)))
<<(x::Int64, y::Int32) = boxsi64(shl_int(unbox64(x), unbox32(y)))
<<(x::Int128, y::Int32) = box(Int128,shl_int(unbox(Int128,x), unbox32(y)))

<<(x::Uint8 , y::Int32) = boxui8 (shl_int(unbox8 (x), unbox32(y)))
<<(x::Uint16, y::Int32) = boxui16(shl_int(unbox16(x), unbox32(y)))
<<(x::Uint32, y::Int32) = boxui32(shl_int(unbox32(x), unbox32(y)))
<<(x::Uint64, y::Int32) = boxui64(shl_int(unbox64(x), unbox32(y)))
<<(x::Uint128, y::Int32) = box(Uint128,shl_int(unbox(Uint128,x), unbox32(y)))

>>(x::Int8 , y::Int32) = boxsi8 (ashr_int(unbox8 (x), unbox32(y)))
>>(x::Int16, y::Int32) = boxsi16(ashr_int(unbox16(x), unbox32(y)))
>>(x::Int32, y::Int32) = boxsi32(ashr_int(unbox32(x), unbox32(y)))
>>(x::Int64, y::Int32) = boxsi64(ashr_int(unbox64(x), unbox32(y)))
>>(x::Int128, y::Int32) = box(Int128,ashr_int(unbox(Int128,x), unbox32(y)))

>>(x::Uint8 , y::Int32) = boxui8 (lshr_int(unbox8 (x), unbox32(y)))
>>(x::Uint16, y::Int32) = boxui16(lshr_int(unbox16(x), unbox32(y)))
>>(x::Uint32, y::Int32) = boxui32(lshr_int(unbox32(x), unbox32(y)))
>>(x::Uint64, y::Int32) = boxui64(lshr_int(unbox64(x), unbox32(y)))
>>(x::Uint128, y::Int32) = box(Uint128,lshr_int(unbox(Uint128,x), unbox32(y)))

>>>(x::Int8 , y::Int32) = boxsi8 (lshr_int(unbox8 (x), unbox32(y)))
>>>(x::Int16, y::Int32) = boxsi16(lshr_int(unbox16(x), unbox32(y)))
>>>(x::Int32, y::Int32) = boxsi32(lshr_int(unbox32(x), unbox32(y)))
>>>(x::Int64, y::Int32) = boxsi64(lshr_int(unbox64(x), unbox32(y)))
>>>(x::Int128, y::Int32) = box(Int128,lshr_int(unbox(Int128,x), unbox32(y)))

>>>(x::Uint8 , y::Int32) = boxui8 (lshr_int(unbox8 (x), unbox32(y)))
>>>(x::Uint16, y::Int32) = boxui16(lshr_int(unbox16(x), unbox32(y)))
>>>(x::Uint32, y::Int32) = boxui32(lshr_int(unbox32(x), unbox32(y)))
>>>(x::Uint64, y::Int32) = boxui64(lshr_int(unbox64(x), unbox32(y)))
>>>(x::Uint128, y::Int32) = box(Uint128,lshr_int(unbox(Uint128,x), unbox32(y)))

bswap(x::Int8)    = x
bswap(x::Uint8)   = x
bswap(x::Int16)   = boxsi16(bswap_int(unbox16(x)))
bswap(x::Uint16)  = boxui16(bswap_int(unbox16(x)))
bswap(x::Int32)   = boxsi32(bswap_int(unbox32(x)))
bswap(x::Uint32)  = boxui32(bswap_int(unbox32(x)))
bswap(x::Int64)   = boxsi64(bswap_int(unbox64(x)))
bswap(x::Uint64)  = boxui64(bswap_int(unbox64(x)))
bswap(x::Int128)  = box(Int128,bswap_int(unbox(Int128,x)))
bswap(x::Uint128) = box(Uint128,bswap_int(unbox(Uint128,x)))

count_ones(x::Int8)    = int(boxsi8 (ctpop_int(unbox8 (x))))
count_ones(x::Uint8)   = int(boxui8 (ctpop_int(unbox8 (x))))
count_ones(x::Int16)   = int(boxsi16(ctpop_int(unbox16(x))))
count_ones(x::Uint16)  = int(boxui16(ctpop_int(unbox16(x))))
count_ones(x::Int32)   = int(boxsi32(ctpop_int(unbox32(x))))
count_ones(x::Uint32)  = int(boxui32(ctpop_int(unbox32(x))))
count_ones(x::Int64)   = int(boxsi64(ctpop_int(unbox64(x))))
count_ones(x::Uint64)  = int(boxui64(ctpop_int(unbox64(x))))
count_ones(x::Int128)  = int(box(Int128,ctpop_int(unbox(Int128,x))))
count_ones(x::Uint128) = int(box(Uint128,ctpop_int(unbox(Uint128,x))))

leading_zeros(x::Int8)    = int(boxsi8 (ctlz_int(unbox8 (x))))
leading_zeros(x::Uint8)   = int(boxui8 (ctlz_int(unbox8 (x))))
leading_zeros(x::Int16)   = int(boxsi16(ctlz_int(unbox16(x))))
leading_zeros(x::Uint16)  = int(boxui16(ctlz_int(unbox16(x))))
leading_zeros(x::Int32)   = int(boxsi32(ctlz_int(unbox32(x))))
leading_zeros(x::Uint32)  = int(boxui32(ctlz_int(unbox32(x))))
leading_zeros(x::Int64)   = int(boxsi64(ctlz_int(unbox64(x))))
leading_zeros(x::Uint64)  = int(boxui64(ctlz_int(unbox64(x))))
leading_zeros(x::Int128)  = int(box(Int128,ctlz_int(unbox(Int128,x))))
leading_zeros(x::Uint128) = int(box(Uint128,ctlz_int(unbox(Uint128,x))))

trailing_zeros(x::Int8)    = int(boxsi8 (cttz_int(unbox8 (x))))
trailing_zeros(x::Uint8)   = int(boxui8 (cttz_int(unbox8 (x))))
trailing_zeros(x::Int16)   = int(boxsi16(cttz_int(unbox16(x))))
trailing_zeros(x::Uint16)  = int(boxui16(cttz_int(unbox16(x))))
trailing_zeros(x::Int32)   = int(boxsi32(cttz_int(unbox32(x))))
trailing_zeros(x::Uint32)  = int(boxui32(cttz_int(unbox32(x))))
trailing_zeros(x::Int64)   = int(boxsi64(cttz_int(unbox64(x))))
trailing_zeros(x::Uint64)  = int(boxui64(cttz_int(unbox64(x))))
trailing_zeros(x::Int128)  = int(box(Int128,cttz_int(unbox(Int128,x))))
trailing_zeros(x::Uint128) = int(box(Uint128,cttz_int(unbox(Uint128,x))))

count_zeros  (x::Integer) = count_ones(~x)
leading_ones (x::Integer) = leading_zeros(~x)
trailing_ones(x::Integer) = trailing_zeros(~x)

## integer comparisons ##

==(x::Int8 , y::Int8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Int16, y::Int16) = eq_int(unbox16(x),unbox16(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
==(x::Int64, y::Int64) = eq_int(unbox64(x),unbox64(y))
==(x::Int128, y::Int128) = eq_int(unbox(Int128,x),unbox(Int128,y))

==(x::Uint8 , y::Uint8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Uint16, y::Uint16) = eq_int(unbox16(x),unbox16(y))
==(x::Uint32, y::Uint32) = eq_int(unbox32(x),unbox32(y))
==(x::Uint64, y::Uint64) = eq_int(unbox64(x),unbox64(y))
==(x::Uint128, y::Uint128) = eq_int(unbox(Uint128,x),unbox(Uint128,y))

!=(x::Int8 , y::Int8 ) = ne_int(unbox8 (x),unbox8 (y))
!=(x::Int16, y::Int16) = ne_int(unbox16(x),unbox16(y))
!=(x::Int32, y::Int32) = ne_int(unbox32(x),unbox32(y))
!=(x::Int64, y::Int64) = ne_int(unbox64(x),unbox64(y))
!=(x::Int128, y::Int128) = ne_int(unbox(Int128,x),unbox(Int128,y))

!=(x::Uint8 , y::Uint8 ) = ne_int(unbox8 (x),unbox8 (y))
!=(x::Uint16, y::Uint16) = ne_int(unbox16(x),unbox16(y))
!=(x::Uint32, y::Uint32) = ne_int(unbox32(x),unbox32(y))
!=(x::Uint64, y::Uint64) = ne_int(unbox64(x),unbox64(y))
!=(x::Uint128, y::Uint128) = ne_int(unbox(Uint128,x),unbox(Uint128,y))

<(x::Int8 , y::Int8 ) = slt_int(unbox8 (x),unbox8 (y))
<(x::Int16, y::Int16) = slt_int(unbox16(x),unbox16(y))
<(x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
<(x::Int64, y::Int64) = slt_int(unbox64(x),unbox64(y))
<(x::Int128, y::Int128) = slt_int(unbox(Int128,x),unbox(Int128,y))

<(x::Uint8 , y::Uint8 ) = ult_int(unbox8 (x),unbox8 (y))
<(x::Uint16, y::Uint16) = ult_int(unbox16(x),unbox16(y))
<(x::Uint32, y::Uint32) = ult_int(unbox32(x),unbox32(y))
<(x::Uint64, y::Uint64) = ult_int(unbox64(x),unbox64(y))
<(x::Uint128, y::Uint128) = ult_int(unbox(Uint128,x),unbox(Uint128,y))

<=(x::Int8 , y::Int8 ) = sle_int(unbox8 (x),unbox8 (y))
<=(x::Int16, y::Int16) = sle_int(unbox16(x),unbox16(y))
<=(x::Int32, y::Int32) = sle_int(unbox32(x),unbox32(y))
<=(x::Int64, y::Int64) = sle_int(unbox64(x),unbox64(y))
<=(x::Int128, y::Int128) = sle_int(unbox(Int128,x),unbox(Int128,y))

<=(x::Uint8 , y::Uint8 ) = ule_int(unbox8 (x),unbox8 (y))
<=(x::Uint16, y::Uint16) = ule_int(unbox16(x),unbox16(y))
<=(x::Uint32, y::Uint32) = ule_int(unbox32(x),unbox32(y))
<=(x::Uint64, y::Uint64) = ule_int(unbox64(x),unbox64(y))
<=(x::Uint128, y::Uint128) = ule_int(unbox(Uint128,x),unbox(Uint128,y))

==(x::Signed  , y::Unsigned) = (x >= 0) & (unsigned(x) == y)
==(x::Unsigned, y::Signed  ) = (y >= 0) & (x == unsigned(y))
!=(x::Signed  , y::Unsigned) = (x <  0) | (unsigned(x) != y)
!=(x::Unsigned, y::Signed  ) = (y <  0) | (x != unsigned(y))
< (x::Signed  , y::Unsigned) = (x <  0) | (unsigned(x) <  y)
< (x::Unsigned, y::Signed  ) = (y >  0) & (x <  unsigned(y))
<=(x::Signed  , y::Unsigned) = (x <= 0) | (unsigned(x) <= y)
<=(x::Unsigned, y::Signed  ) = (y >= 0) & (x <= unsigned(y))

## system word size ##

const WORD_SIZE = int(Int.nbits)

## integer promotions ##

promote_rule(::Type{Int16}, ::Type{Int8}  ) = Int
promote_rule(::Type{Int32}, ::Type{Int8}  ) = Int
promote_rule(::Type{Int32}, ::Type{Int16} ) = Int
promote_rule(::Type{Int64}, ::Type{Int8}  ) = Int64
promote_rule(::Type{Int64}, ::Type{Int16} ) = Int64
promote_rule(::Type{Int64}, ::Type{Int32} ) = Int64
promote_rule(::Type{Int128}, ::Type{Int8} ) = Int128
promote_rule(::Type{Int128}, ::Type{Int16}) = Int128
promote_rule(::Type{Int128}, ::Type{Int32}) = Int128
promote_rule(::Type{Int128}, ::Type{Int64}) = Int128

promote_rule(::Type{Uint16}, ::Type{Uint8}  ) = Uint
promote_rule(::Type{Uint32}, ::Type{Uint8}  ) = Uint
promote_rule(::Type{Uint32}, ::Type{Uint16} ) = Uint
promote_rule(::Type{Uint64}, ::Type{Uint8}  ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint16} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint32} ) = Uint64
promote_rule(::Type{Uint128}, ::Type{Uint8} ) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint16}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint32}) = Uint128
promote_rule(::Type{Uint128}, ::Type{Uint64}) = Uint128

promote_rule(::Type{Uint8} , ::Type{Int8}  ) = Int
promote_rule(::Type{Uint8} , ::Type{Int16} ) = Int
promote_rule(::Type{Uint8} , ::Type{Int32} ) = Int
promote_rule(::Type{Uint8} , ::Type{Int64} ) = Int64
promote_rule(::Type{Uint8} , ::Type{Int128}) = Int128

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
@eval typemin(::Type{Int64 }) = $(-9223372036854775807-int64(1))
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
