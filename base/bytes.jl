import Core.ByteVec
export ByteVec, Str

ByteVec(a::Vector{UInt8}) = ccall(:jl_bytevec, ByteVec, (Ptr{UInt8}, Csize_t), a, length(a))
ByteVec(s::AbstractString) = ByteVec(bytestring(s).data)

size(b::ByteVec) = (length(b),)

function length(b::ByteVec)
    here = ((b.x >>> 8*(sizeof(b.x)-1)) % Int) & 255
    there = -(b.x >> 8*sizeof(Int)) % Int
    ifelse(b.x < 0, there, here)
end

getindex(b::ByteVec, i::Real) =
    box(UInt8, bytevec_ref(unbox(typeof(b.x), b.x), unbox(Int, Int(i))))
getu32(b::ByteVec, i::Int) =
    box(UInt32, bytevec_ref32(unbox(typeof(b.x), b.x), unbox(Int, i)))

function ==(a::ByteVec, b::ByteVec)
    a_hi = (a.x >> 8*sizeof(Int)) % Int
    b_hi = (b.x >> 8*sizeof(Int)) % Int
    (a_hi != b_hi) | (a_hi >= 0) | (b_hi >= 0) && return a.x == b.x
    pa = reinterpret(Ptr{Uint8}, a.x % UInt)
    pb = reinterpret(Ptr{Uint8}, b.x % UInt)
    ccall(:memcmp, Cint, (Ptr{Uint8}, Ptr{Uint8}, Csize_t), pa, pb, -a_hi % Uint) == 0
end

function cmp(a::ByteVec, b::ByteVec)
    a_x, b_x = a.x, b.x
    a_here, b_here = a_x >= 0, b_x >= 0
    if !(a_here & b_here)
        if b_here
            a_x = unsafe_load(reinterpret(Ptr{typeof(a_x)}, a_x % UInt))
        elseif a_here
            b_x = unsafe_load(reinterpret(Ptr{typeof(b_x)}, b_x % UInt))
        else
            pa = reinterpret(Ptr{Uint8}, a_x % UInt)
            pb = reinterpret(Ptr{Uint8}, b_x % UInt)
            la = -(a_x >>> 8*sizeof(Int)) % UInt
            lb = -(b_x >>> 8*sizeof(Int)) % UInt
            c = Int(ccall(:memcmp, Cint, (Ptr{Uint8}, Ptr{Uint8}, Csize_t), pa, pb, min(la,lb)))
            return ifelse(c == 0, cmp(la,lb), sign(c))
        end
    end
    cmp(bswap(a_x), bswap(b_x))
end
isless(x::ByteVec, y::ByteVec) = cmp(x, y) < 0

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

    ==(s::Str, t::Str) =     ==(s.data, t.data)
isless(s::Str, t::Str) = isless(s.data, t.data)
   cmp(s::Str, t::Str) =    cmp(s.data, t.data)

