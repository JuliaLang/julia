## char conversions ##

convert(::Type{Char}, x::Bool   ) = box(Char,sext32(unbox8(x)))
convert(::Type{Char}, x::Int8   ) = box(Char,sext32(unbox8(x)))
convert(::Type{Char}, x::Uint8  ) = box(Char,zext32(unbox8(x)))
convert(::Type{Char}, x::Int16  ) = box(Char,sext32(unbox16(x)))
convert(::Type{Char}, x::Uint16 ) = box(Char,zext32(unbox16(x)))
convert(::Type{Char}, x::Int32  ) = box(Char,unbox32(x))
convert(::Type{Char}, x::Uint32 ) = box(Char,unbox32(x))
convert(::Type{Char}, x::Int64  ) = box(Char,trunc32(unbox64(x)))
convert(::Type{Char}, x::Uint64 ) = box(Char,trunc32(unbox64(x)))
convert(::Type{Char}, x::Float32) = box(Char,fptoui32(unbox32(x)))
convert(::Type{Char}, x::Float64) = box(Char,fptoui32(unbox64(x)))

char(x::Scalar) = convert(Char, x)

## char promotions ##

promote_rule(::Type{Char}, ::Type{Int8})   = Int32
promote_rule(::Type{Char}, ::Type{Uint8})  = Int32
promote_rule(::Type{Char}, ::Type{Int16})  = Int32
promote_rule(::Type{Char}, ::Type{Uint16}) = Int32
promote_rule(::Type{Char}, ::Type{Int32})  = Int32
promote_rule(::Type{Char}, ::Type{Uint32}) = Uint32
promote_rule(::Type{Char}, ::Type{Int64})  = Int64
promote_rule(::Type{Char}, ::Type{Uint32}) = Uint64

## character operations & comparisons ##

-(x::Char) = -int32(x)
~(x::Char) = ~int32(x)
(+)(x::Char, y::Char) = int32(x) + int32(y)
(-)(x::Char, y::Char) = int32(x) - int32(y)
(*)(x::Char, y::Char) = int32(x) * int32(y)
div(x::Char, y::Char) = div(int32(x), int32(y))
(%)(x::Char, y::Char) = int32(x) % int32(y)
(&)(x::Char, y::Char) = int32(x) & int32(y)
(|)(x::Char, y::Char) = int32(x) | int32(y)
($)(x::Char, y::Char) = int32(x) $ int32(y)
(<<)(x::Char, y::Int32) = int32(x) << y
(>>)(x::Char, y::Int32) = int32(x) >> y
(>>>)(x::Char, y::Int32) = int32(x) >>> y
==(x::Char, y::Char) = int32(x) == int32(y)
< (x::Char, y::Char) = int32(x) <  int32(y)

## traits ##

sizeof(::Type{Char}) = 4
