## char conversions ##

convert(::Type{Char}, x::Bool   ) = box(Char,sext32(unbox(Bool,x)))
convert(::Type{Char}, x::Int8   ) = box(Char,sext32(unbox(Int8,x)))
convert(::Type{Char}, x::Uint8  ) = box(Char,zext32(unbox(Uint8,x)))
convert(::Type{Char}, x::Int16  ) = box(Char,sext32(unbox(Int16,x)))
convert(::Type{Char}, x::Uint16 ) = box(Char,zext32(unbox(Uint16,x)))
convert(::Type{Char}, x::Int32  ) = box(Char,unbox(Int32,x))
convert(::Type{Char}, x::Uint32 ) = box(Char,unbox(Uint32,x))
convert(::Type{Char}, x::Int64  ) = box(Char,trunc32(unbox(Int64,x)))
convert(::Type{Char}, x::Uint64 ) = box(Char,trunc32(unbox(Uint64,x)))
convert(::Type{Char}, x::Int128 ) = box(Char,trunc32(unbox(Int128,x)))
convert(::Type{Char}, x::Uint128) = box(Char,trunc32(unbox(Uint128,x)))
convert(::Type{Char}, x::Float32) = box(Char,fptoui32(unbox(Float32,x)))
convert(::Type{Char}, x::Float64) = box(Char,fptoui32(unbox(Float64,x)))

char(x) = convert(Char, x)

function safe_char(x)
    c = char(x)
    if '\ud800' <= c <= '\udfff' || '\U10ffff' < c
        error("invalid Unicode code point: U+", hex(c))
    end
    return c
end

integer(x::Char) = int32(x)
unsigned(x::Char) = uint32(x)

## char promotions ##

promote_rule(::Type{Char}, ::Type{Int8})    = Int32
promote_rule(::Type{Char}, ::Type{Uint8})   = Int32
promote_rule(::Type{Char}, ::Type{Int16})   = Int32
promote_rule(::Type{Char}, ::Type{Uint16})  = Int32
promote_rule(::Type{Char}, ::Type{Int32})   = Int32
promote_rule(::Type{Char}, ::Type{Uint32})  = Uint32
promote_rule(::Type{Char}, ::Type{Int64})   = Int64
promote_rule(::Type{Char}, ::Type{Uint64})  = Uint64
promote_rule(::Type{Char}, ::Type{Int128})  = Int128
promote_rule(::Type{Char}, ::Type{Uint128}) = Uint128

## character operations & comparisons ##

-(x::Char) = -int(x)
+(x::Char, y::Char) = int(x) + int(y)
-(x::Char, y::Char) = int(x) - int(y)
*(x::Char, y::Char) = int(x) * int(y)

div(x::Char, y::Char) = div(int(x), int(y))
fld(x::Char, y::Char) = div(int(x), int(y))
rem(x::Char, y::Char) = rem(int(x), int(y))
mod(x::Char, y::Char) = rem(int(x), int(y))

~(x::Char)            = ~uint32(x)
(&)(x::Char, y::Char) = uint32(x) & uint32(y)
|(x::Char, y::Char)   = uint32(x) | uint32(y)
($)(x::Char, y::Char) = uint32(x) $ uint32(y)

<<(x::Char, y::Int32)  = uint32(x) << y
>>(x::Char, y::Int32)  = uint32(x) >>> y
>>>(x::Char, y::Int32) = uint32(x) >>> y

==(x::Char, y::Char) = uint32(x) == uint32(y)
< (x::Char, y::Char) = uint32(x) <  uint32(y)
<=(x::Char, y::Char) = uint32(x) <= uint32(y)

## traits ##

sizeof(::Type{Char}) = 4

## printing & showing characters ##

print(io::IO, c::Char) = (write(io,c); nothing)
show(io, c::Char) = (print(io,'\''); print_escaped(io,string(c),"'"); print(io,'\''))

## libc character class testing functions ##

iswascii(c::Char) = c < 0x80

for f = (:iswalnum, :iswalpha, :iswblank, :iswcntrl, :iswdigit,
         :iswgraph, :iswlower, :iswprint, :iswpunct, :iswspace,
         :iswupper, :iswxdigit,
         # these are BSD-only
         #:iswhexnumber, :iswideogram, :iswnumber, :iswphonogram, :iswrune, :iswspecial, 
         )
    @eval ($f)(c::Char) = bool(ccall($(expr(:quote,f)), Int32, (Char,), c))
end
