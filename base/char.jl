# This file is a part of Julia. License is MIT: https://julialang.org/license

convert(::Type{Char}, x::Number) = Char(x)
convert(::Type{T}, x::Char) where {T<:Number} = T(x)

rem(x::Char, ::Type{T}) where {T<:Number} = rem(UInt32(x), T)

typemax(::Type{Char}) = reinterpret(Char, typemax(UInt32))
typemin(::Type{Char}) = reinterpret(Char, typemin(UInt32))

size(c::Char) = ()
size(c::Char,d) = convert(Int, d) < 1 ? throw(BoundsError()) : 1
ndims(c::Char) = 0
ndims(::Type{Char}) = 0
length(c::Char) = 1
endof(c::Char) = 1
getindex(c::Char) = c
getindex(c::Char, i::Integer) = i == 1 ? c : throw(BoundsError())
getindex(c::Char, I::Integer...) = all(x -> x == 1, I) ? c : throw(BoundsError())
first(c::Char) = c
last(c::Char) = c
eltype(::Type{Char}) = Char

start(c::Char) = false
next(c::Char, state) = (c, true)
done(c::Char, state) = state
isempty(c::Char) = false
in(x::Char, y::Char) = x == y

==(x::Char, y::Char) = UInt32(x) == UInt32(y)
isless(x::Char, y::Char) = UInt32(x) < UInt32(y)

const hashchar_seed = 0xd4d64234
hash(x::Char, h::UInt) = hash_uint64(((UInt64(x)+hashchar_seed)<<32) ⊻ UInt64(h))

-(x::Char, y::Char) = Int(x) - Int(y)
-(x::Char, y::Integer) = Char(Int32(x) - Int32(y))
+(x::Char, y::Integer) = Char(Int32(x) + Int32(y))
+(x::Integer, y::Char) = y + x

bswap(x::Char) = Char(bswap(UInt32(x)))

print(io::IO, c::Char) = (write(io, c); nothing)

const hex_chars = UInt8['0':'9';'a':'z']

function show(io::IO, c::Char)
    if c <= '\\'
        b = c == '\0' ? 0x30 :
            c == '\a' ? 0x61 :
            c == '\b' ? 0x62 :
            c == '\t' ? 0x74 :
            c == '\n' ? 0x6e :
            c == '\v' ? 0x76 :
            c == '\f' ? 0x66 :
            c == '\r' ? 0x72 :
            c == '\e' ? 0x65 :
            c == '\'' ? 0x27 :
            c == '\\' ? 0x5c : 0xff
        if b != 0xff
            write(io, 0x27, 0x5c, b, 0x27)
            return
        end
    end
    if isprint(c)
        write(io, 0x27, c, 0x27)
    else
        u = UInt32(c)
        write(io, 0x27, 0x5c, c <= '\x7f' ? 0x78 : c <= '\uffff' ? 0x75 : 0x55)
        d = max(2, 8 - (leading_zeros(u) >> 2))
        while 0 < d
            write(io, hex_chars[((u >> ((d -= 1) << 2)) & 0xf) + 1])
        end
        write(io, 0x27)
    end
    return
end

function show(io::IO, ::MIME"text/plain", c::Char)
    show(io, c)
    u = UInt32(c)
    print(io, ": ", isascii(c) ? "ASCII/" : "", "Unicode U+", hex(u, u > 0xffff ? 6 : 4))
    print(io, " (category ", UTF8proc.category_abbrev(c), ": ", UTF8proc.category_string(c), ")")
end
