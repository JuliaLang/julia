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
convert(::Type{Char}, x::Float32) = char(convert(Int, x))
convert(::Type{Char}, x::Float64) = char(convert(Int, x))

char(x) = convert(Char, x)
char(x::FloatingPoint) = char(iround(x))

function safe_char(x)
    c = char(x)
    if '\ud800' <= c <= '\udfff' || '\U10ffff' < c
        error("invalid Unicode code point: U+", hex(c))
    end
    return c
end

integer(x::Char) = int(x)
unsigned(x::Char) = uint(x)

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

-(x::Char, y::Char) = int(x)-int(y)
+(x::Char   , y::Char   ) = char(int(x)+int(y)) # TODO: delete me
+(x::Char   , y::Integer) = char(int(x)+int(y))
+(x::Integer, y::Char   ) = y+x
-(x::Char   , y::Integer) = char(int(x)-int(y))

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
show(io::IO, c::Char) = (print(io,'\''); print_escaped(io,CharString(c),"'"); print(io,'\''))
