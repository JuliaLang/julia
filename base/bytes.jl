import Core.ByteVec
export ByteVec, Str

ByteVec(a::Vector{UInt8}) = ccall(:jl_bytevec, ByteVec, (Ptr{UInt8}, Csize_t), a, length(a))
ByteVec(s::AbstractString) = ByteVec(bytestring(s).data)

size(b::ByteVec) = (length(b),)
length(b::ByteVec) = box(Int, bytevec_len(unbox(typeof(b.x), b.x)))
getindex(b::ByteVec, i::Real) =
    box(Uint8, bytevec_ref(unbox(typeof(b.x), b.x), unbox(Int, Int(i))))
getu32(b::ByteVec, i::Int) =
    box(Uint32, bytevec_ref32(unbox(typeof(b.x), b.x), unbox(Int, i)))

# ==(x::ByteVec, y::ByteVec) = bytevec_eq(x, y)
# cmp(x::ByteVec, y::ByteVec) = bytevec_cmp(x, y)
# isless(x::ByteVec, y::ByteVec) = cmp(x, y) < 0

start(b::ByteVec) = 1
next(b::ByteVec, i::Int) = (b[i], i+1)
done(b::ByteVec, i::Int) = length(b) < i

## ByteVec-based string type ##

immutable Str <: AbstractString
    data::ByteVec
end
Str(s::AbstractString) = Str(ByteVec(s))

function endof(s::Str)
    d = s.data
    i = length(d)
    i == 0 && return i
    @inbounds while !is_utf8_start(d[i])
        i -= 1
    end
    i
end

function length(s::Str)
    n = 0
    @inbounds for b in s.data
        n += is_utf8_start(b)
    end
    return n
end

@inline function next(s::Str, i::Int)
    u = getu32(s.data, i)
    a::UInt32 = u & 0xff
    a < 0x80 && return Char(a), i+1
    is_utf8_start(a) || error("invalid UTF-8 character index")
    b::UInt32 = a << 6 + (u >> 8) & 0xff
    a < 0xe0 && return Char(b - 0x00003080), i+2
    c::UInt32 = b << 6 + (u >> 16) & 0xff
    a < 0xf0 && return Char(c - 0x000e2080), i+3
    d::Uint32 = c << 6 + (u >> 24) & 0xff
                return Char(d - 0x03c82080), i+4
end

## overload methods for efficiency ##

sizeof(s::Str) = length(s.data)

isless(x::Str, y::Str) = isless(x.data, y.data)
   cmp(x::Str, y::Str) =    cmp(x.data, y.data)

