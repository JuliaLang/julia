char(x) = convert(Char, x)
char(x::FloatingPoint) = char(round(UInt32,x))

integer(x::Char) = int(x)

convert(::Type{Char}, x::Float16) = char(convert(UInt32, x))
convert(::Type{Char}, x::Float32) = char(convert(UInt32, x))
convert(::Type{Char}, x::Float64) = char(convert(UInt32, x))

typemax(::Type{Char}) = char(typemax(UInt32))
typemin(::Type{Char}) = char(typemin(UInt32))

## character operations & comparisons ##
size(c::Char) = ()
size(c::Char,d) = convert(Int, d) < 1 ? throw(BoundsError()) : 1
ndims(c::Char) = 0
ndims(::Type{Char}) = 0
length(c::Char) = 1
endof(c::Char) = 1
getindex(c::Char) = c
getindex(c::Char, i::Integer) = i == 1 ? c : throw(BoundsError())
getindex(c::Char, I::Integer...) = all(EqX(1), I) ? c : throw(BoundsError())
getindex(c::Char, I::Real...) = getindex(c, to_index(I)...)
first(c::Char) = c
last(c::Char) = c
eltype(c::Char) = Char

start(c::Char) = false
next(c::Char, state) = (c, true)
done(c::Char, state) = state
isempty(c::Char) = false
in(x::Char, y::Char) = x == y


==(x::Char, y::Char) = uint32(x) == uint32(y)
==(x::Char, y::Integer) = uint32(x) == y
==(x::Integer, y::Char) = x == uint32(y)

isless(x::Char, y::Char)    = isless(uint32(x), uint32(y))
isless(x::Char, y::Integer) = isless(uint32(x), y)
isless(x::Integer, y::Char) = isless(x, uint32(y))

# numeric operations

# ordinal operations
+(x::Char   , y::Integer) = reinterpret(Char, int32(x) + int32(y))
+(x::Integer, y::Char) = y + x
-(x::Char   , y::Char) = int(x) - int(y)
-(x::Char   , y::Integer) = reinterpret(Char, int32(x) - int32(y))

# bitwise operations
(~)(x::Char) = char(~uint32(x))
(&)(x::Char, y::Char) = char(uint32(x) & uint32(y))
(|)(x::Char, y::Char) = char(uint32(x) | uint32(y))
($)(x::Char, y::Char) = char(uint32(x) $ uint32(y))

bswap(x::Char) = char(bswap(uint32(x)))

## printing & showing characters ##
print(io::IO, c::Char) = (write(io, c); nothing)
show(io::IO,  c::Char) = begin
    print(io, '\'')
    print_escaped(io, utf32(c), "'")
    print(io, '\'')
end
