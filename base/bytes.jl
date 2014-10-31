using Core.Intrinsics
importall Core
importall Base
import Base: show_comma_array

Bytes(a::Vector{Uint8}) = ccall(:jl_bytes, Bytes, (Ptr{Uint8}, Csize_t), a, length(a))
Bytes(s::AbstractString) = Bytes(bytestring(s).data)

size(b::Bytes) = (length(b),)
length(b::Bytes) = ifelse(b.neglen < 0, -b.neglen, b.neglen >>> ((sizeof(Int)-1) << 3))
getindex(b::Bytes, i::Real) = ccall(:jl_bytesref, Uint8, (Bytes, Csize_t), b, i-1)

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
