## char conversions ##

check_char(x::Char) =
    ('\ud800' <= x <= '\udfff' || '\U10ffff' < x) ?
        error("invalid Unicode code point: U+", hex(x)) : x

convert(::Type{Char}, x::Bool   ) = check_char(box(Char,sext32(unbox8(x))))
convert(::Type{Char}, x::Int8   ) = check_char(box(Char,sext32(unbox8(x))))
convert(::Type{Char}, x::Uint8  ) = check_char(box(Char,zext32(unbox8(x))))
convert(::Type{Char}, x::Int16  ) = check_char(box(Char,sext32(unbox16(x))))
convert(::Type{Char}, x::Uint16 ) = check_char(box(Char,zext32(unbox16(x))))
convert(::Type{Char}, x::Int32  ) = check_char(box(Char,unbox32(x)))
convert(::Type{Char}, x::Uint32 ) = check_char(box(Char,unbox32(x)))
convert(::Type{Char}, x::Int64  ) = check_char(box(Char,trunc32(unbox64(x))))
convert(::Type{Char}, x::Uint64 ) = check_char(box(Char,trunc32(unbox64(x))))
convert(::Type{Char}, x::Float32) = check_char(box(Char,fptoui32(unbox32(x))))
convert(::Type{Char}, x::Float64) = check_char(box(Char,fptoui32(unbox64(x))))

char(x) = convert(Char, x)

int(x::Char) = int32(x)
uint(x::Char) = uint32(x)
float(x::Char) = float32(x)

## char promotions ##

promote_rule(::Type{Char}, ::Type{Int8})   = Int32
promote_rule(::Type{Char}, ::Type{Uint8})  = Int32
promote_rule(::Type{Char}, ::Type{Int16})  = Int32
promote_rule(::Type{Char}, ::Type{Uint16}) = Int32
promote_rule(::Type{Char}, ::Type{Int32})  = Int32
promote_rule(::Type{Char}, ::Type{Uint32}) = Uint32
promote_rule(::Type{Char}, ::Type{Int64})  = Int64
promote_rule(::Type{Char}, ::Type{Uint64}) = Uint64

## character operations & comparisons ##

-(x::Char) = -int32(x)
~(x::Char) = ~int32(x)
(+)(x::Char, y::Char) = int32(x) + int32(y)
(-)(x::Char, y::Char) = int32(x) - int32(y)
(*)(x::Char, y::Char) = int32(x) * int32(y)
div(x::Char, y::Char) = div(int32(x), int32(y))
fld(x::Char, y::Char) = div(int32(x), int32(y))
rem(x::Char, y::Char) = rem(int32(x), int32(y))
mod(x::Char, y::Char) = rem(int32(x), int32(y))
(&)(x::Char, y::Char) = int32(x) & int32(y)
(|)(x::Char, y::Char) = int32(x) | int32(y)
($)(x::Char, y::Char) = int32(x) $ int32(y)
(<<)(x::Char, y::Int32) = int32(x) << y
(>>)(x::Char, y::Int32) = int32(x) >>> y
(>>>)(x::Char, y::Int32) = int32(x) >>> y
==(x::Char, y::Char) = uint32(x) == uint32(y)
< (x::Char, y::Char) = uint32(x) <  uint32(y)

## traits ##

sizeof(::Type{Char}) = 4

## libc character class testing functions ##

for f = (:iswalnum, :iswalpha, :iswascii, :iswblank, :iswcntrl, :iswdigit,
         :iswgraph, :iswhexnumber, :iswideogram, :iswlower, :iswnumber,
         :iswphonogram, :iswprint, :iswpunct, :iswrune, :iswspace,
         :iswspecial, :iswupper, :iswxdigit)
    @eval ($f)(c::Char) = bool(ccall(dlsym(libc,$expr(:quote,f)),
                                     Int32, (Char,), c))
end
