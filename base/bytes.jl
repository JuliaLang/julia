import Core.ByteVec
export ByteVec, Str

ByteVec(a::Vector{UInt8}) = ccall(:jl_bytevec, ByteVec, (Ptr{UInt8}, Csize_t), a, length(a))
ByteVec(s::AbstractString) = ByteVec(bytestring(s).data)

length(b::ByteVec) = box(Int, bytevec_len(unbox(typeof(b.x), b.x)))
size(b::ByteVec) = (length(b),)

function getindex(b::ByteVec, i::Int)
    1 <= i <= length(b) || throw(BoundsError())
    box(Uint8, bytevec_ref(unbox(typeof(b.x), b.x), unbox(Int, i)))
end
getindex(b::ByteVec, i::Real) = b[Int(i)]

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
    a::UInt32 = d[i]
    a < 0x80 && return Char(a), i+1
    # is_utf8_start(a) || error("invalid UTF-8 character index")
    b::UInt32 = a << 6 + d[i+1]
    a < 0xe0 && return Char(b - 0x00003080), i+2
    c::UInt32 = b << 6 + d[i+2]
    a < 0xf0 && return Char(c - 0x000e2080), i+3
    return Char(c << 6 + d[i+3] - 0x03c82080), i+4
end

## overload methods for efficiency ##

sizeof(s::Str) = length(s.data)

isless(x::Str, y::Str) = isless(x.data, y.data)
   cmp(x::Str, y::Str) =    cmp(x.data, y.data)

