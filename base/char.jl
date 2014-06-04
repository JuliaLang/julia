char(x) = convert(Char, x)
char(x::FloatingPoint) = char(iround(x))

integer(x::Char) = int(x)

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

# numeric operations
# TODO: this should be removed, but needs to be here as long as Char <: Integer
+(x::Char   , y::Char   ) = int(x)+int(y)

# ordinal operations
+(x::Char   , y::Integer) = char(int(x)+int(y))
+(x::Integer, y::Char   ) = y+x
-(x::Char   , y::Char   ) = int(x)-int(y)
-(x::Char   , y::Integer) = char(int(x)-int(y))

# bitwise operations
(~)(x::Char) = char(~uint32(x))
(&)(x::Char, y::Char) = char(uint32(x) & uint32(y))
(|)(x::Char, y::Char) = char(uint32(x) | uint32(y))
($)(x::Char, y::Char) = char(uint32(x) $ uint32(y))

bswap(x::Char) = char(bswap(uint32(x)))

<<(x::Char, y::Int32)  = uint32(x) << y
>>(x::Char, y::Int32)  = uint32(x) >>> y
>>>(x::Char, y::Int32) = uint32(x) >>> y

< (x::Char, y::Char) = uint32(x) <  uint32(y)
<=(x::Char, y::Char) = uint32(x) <= uint32(y)

## traits ##

sizeof(::Type{Char}) = 4

## printing & showing characters ##

print(io::IO, c::Char) = (write(io,c); nothing)
show(io::IO, c::Char) = (print(io,'\''); print_escaped(io,utf32(c),"'"); print(io,'\''))
