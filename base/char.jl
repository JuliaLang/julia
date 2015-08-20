# This file is a part of Julia. License is MIT: http://julialang.org/license

function convert(::Type{UInt32}, c::Char)
    u = reinterpret(UInt32, c)
    u $= ifelse(
        u <= 0x0000ffff,
        ifelse(u <= 0x000000ff, 0x00000000, 0x0000c080),
        ifelse(u <= 0x00ffffff, 0x00e08080, 0xf0808080),
    )
    ((u & 0x000000ff) >> 0) $ ((u & 0x0000ff00) >> 2) $
    ((u & 0x00ff0000) >> 4) $ ((u & 0xff000000) >> 6)
end

function convert(::Type{Char}, u::UInt32)
    u < 0x00200000 || throw(InexactError())
    c = (u & 0x3f) | ((u << 2) & 0x3f00) | ((u << 4) & 0x3f0000) | ((u << 6) & 0x3f000000)
    reinterpret(Char, ifelse(u <= 0x7f, u,
        c | ifelse(u <= 0x000007ff, 0x0000c080,
            ifelse(u <= 0x0000ffff, 0x00e08080, 0xf0808080))))
end

convert(::Type{Char}, x::Number) = Char(UInt32(x))
convert{T<:Number}(::Type{T}, x::Char) = convert(T, UInt32(x))

rem{T<:Number}(x::Char, ::Type{T}) = rem(UInt32(x), T)

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
getindex(c::Char, I::Integer...) = all(EqX(1), I) ? c : throw(BoundsError())
first(c::Char) = c
last(c::Char) = c
eltype(::Type{Char}) = Char

start(c::Char) = false
next(c::Char, state) = (c, true)
done(c::Char, state) = state
isempty(c::Char) = false
in(x::Char, y::Char) = x == y

==(x::Char, y::Char) = eq_int(unbox(Char,x), unbox(Char,y))
==(x::Char, y::Integer) = UInt32(x) == y
==(x::Integer, y::Char) = x == UInt32(y)

isless(x::Char, y::Char)    = ult_int(unbox(Char,x), unbox(Char,y))
isless(x::Char, y::Integer) = isless(UInt32(x), y)
isless(x::Integer, y::Char) = isless(x, UInt32(y))

-(x::Char, y::Char) = Int(x) - Int(y)
-(x::Char, y::Integer) = Char(Int32(x) - Int32(y))
+(x::Char, y::Integer) = Char(Int32(x) + Int32(y))
+(x::Integer, y::Char) = y + x

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
