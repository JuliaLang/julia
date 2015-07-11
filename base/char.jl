# This file is a part of Julia. License is MIT: http://julialang.org/license

convert(::Type{Char}, x::Float16) = Char(convert(UInt32, x))
convert(::Type{Char}, x::Float32) = Char(convert(UInt32, x))
convert(::Type{Char}, x::Float64) = Char(convert(UInt32, x))

typemax(::Type{Char}) = Char(typemax(UInt32))
typemin(::Type{Char}) = Char(typemin(UInt32))

size(c::Char) = ()
size(c::Char,d) = convert(Int, d) < 1 ? throw(BoundsError()) : 1
ndims(c::Char) = 0
ndims(::Type{Char}) = 0
length(c::Char) = 1
endof(c::Char) = 1
getindex(c::Char) = c
getindex(c::Char, i::Integer) = i == 1 ? c : throw(BoundsError())
getindex(c::Char, I::Integer...) = all(EqX(1), I) ? c : throw(BoundsError())
getindex(c::Char, I::Real...) = getindex(c, to_indexes(I...)...)
first(c::Char) = c
last(c::Char) = c
eltype(c::Char) = Char

start(c::Char) = false
next(c::Char, state) = (c, true)
done(c::Char, state) = state
isempty(c::Char) = false
in(x::Char, y::Char) = x == y

==(x::Char, y::Char) = UInt32(x) == UInt32(y)
==(x::Char, y::Integer) = UInt32(x) == y
==(x::Integer, y::Char) = x == UInt32(y)

isless(x::Char, y::Char)    = isless(UInt32(x), UInt32(y))
isless(x::Char, y::Integer) = isless(UInt32(x), y)
isless(x::Integer, y::Char) = isless(x, UInt32(y))

-(x::Char, y::Char) = Int(x) - Int(y)
-(x::Char, y::Integer) = reinterpret(Char, Int32(x) - Int32(y))
+(x::Char, y::Integer) = reinterpret(Char, Int32(x) + Int32(y))
+(x::Integer, y::Char) = y + x

bswap(x::Char) = Char(bswap(UInt32(x)))

print(io::IO, c::Char) = (write(io, c); nothing)
function show(io::IO, c::Char)
    print(io, '\'')
    print_escaped(io, utf32(c), "'")
    print(io, '\'')
end
