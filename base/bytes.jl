using Core.Intrinsics
importall Core
importall Base
import Base: show_comma_array
export Bytes, Str

Bytes(a::Vector{Uint8}) = ccall(:jl_bytes, Bytes, (Ptr{Uint8}, Csize_t), a, length(a))
Bytes(s::AbstractString) = Bytes(bytestring(s).data)

size(b::Bytes) = (length(b),)
length(b::Bytes) = ifelse(b.neglen < 0, -b.neglen, b.neglen >>> ((sizeof(Int)-1) << 3))
getindex(b::Bytes, i::Real) = Core.bytesref(b, convert(Int,i))

start(b::Bytes) = 1
next(b::Bytes, i::Int) = (b[i], i+1)
done(b::Bytes, i::Int) = length(b) < i

function ==(x::Bytes, y::Bytes)
    x.neglen < 0 || return x === y
    x.neglen == y.neglen || return false
    x.pointer == y.pointer || 0 ==
    ccall(:memcmp, Cint, (Ptr{Uint8}, Ptr{Uint8}, Csize_t), x.pointer, y.pointer, -x.neglen)
end

function cmp(x::Bytes, y::Bytes)
    lx, ly = length(x), length(y); l = min(lx, ly)
    if x.neglen < 0 && y.neglen < 0
        c = ccall(:memcmp, Cint, (Ptr{Uint8}, Ptr{Uint8}, Csize_t), x.pointer, y.pointer, l)
    elseif x.neglen < 0
        c = ccall(:memcmp, Cint, (Ptr{Uint8}, Ptr{Bytes}, Csize_t), x.pointer, &y, l)
    elseif y.neglen < 0
        c = ccall(:memcmp, Cint, (Ptr{Bytes}, Ptr{Uint8}, Csize_t), &x, y.pointer, l)
    else
        d = cmp(bswap(uint(x.pointer)), bswap(uint(y.pointer)))
        return d != 0 ? d : cmp(bswap(x.neglen), bswap(y.neglen))
    end
    c == 0 ? cmp(lx, ly) : c < 0 ? -1 : 1
end

isless(x::Bytes, y::Bytes) = cmp(x, y) < 0

## Bytes-based string type ##

immutable Str <: AbstractString
    data::Bytes
end
Str(s::AbstractString) = Str(Bytes(s))

function endof(s::Str)
    d = s.data
    i = length(d)
    i == 0 && return i
    while !is_utf8_start(d[i])
        i -= 1
    end
    i
end

function length(s::Str)
    d = s.data
    n = 0
    for i = 1:length(s.data)
        n += is_utf8_start(d[i])
    end
    return n
end

function next(s::Str, i::Int)
    d = s.data
    a::Uint32 = d[i]
    a < 0x80 && return Char(a), i+1
    # is_utf8_start(a) || error("invalid UTF-8 character index")
    b::Uint32 = a << 6 + d[i+1]
    a < 0xe0 && return Char(b - 0x00003080), i+2
    c::Uint32 = b << 6 + d[i+2]
    a < 0xf0 && return Char(c - 0x000e2080), i+3
    return Char(c << 6 + d[i+3] - 0x03c82080), i+4
end

## overload methods for efficiency ##

sizeof(s::Str) = length(s.data)

isless(x::Str, y::Str) = isless(x.data, y.data)
   cmp(x::Str, y::Str) =    cmp(x.data, y.data)

