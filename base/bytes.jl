import Core.ByteVec
export ByteVec, Str

ByteVec(a::Vector{UInt8}) = ccall(:jl_bytevec, ByteVec, (Ptr{UInt8}, Csize_t), a, length(a))
ByteVec(s::AbstractString) = ByteVec(bytestring(s).data)

size(b::ByteVec) = (length(b),)

function length(b::ByteVec)
    here = (b.x >>> 8*(sizeof(b.x)-1)) % Int
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

@inline function endof(s::Str)
    n = length(s.data)
    @inbounds u = getu32(s.data, n-3)
    x = (u & 0xc0c0c0c0) $ 0x80808080
    n - leading_zeros(x) >>> 3
end

function next(s::Str, k::Int)
    a = bswap(getu32(s.data, k))
    0 <= reinterpret(Int32, a) && return Char(a >> 24), k + 1
    l = leading_ones(a)
    b = (a << l >> l) $ 0x808080
    r = 32 - 8l
    c = b >> r
    t = (l != 1) & (l <= 4) & ((b & 0xc0c0c0) >> r == 0)
    d = ( (c >> 24)         << 18) |
        (((c >> 16) & 0xff) << 12) |
        (((c >>  8) & 0xff) <<  6) | (c & 0xff)
    ifelse(t, Char(d), '\ufffd'), k + ifelse(t, l, 1)
end

const mask = div(typemax(UInt),typemax(UInt8))

function length(s::Str)
    x = s.data.x
    if 0 <= x
        lo, hi = x % UInt, (x >>> 8*sizeof(Uint)) % Uint
        n = (hi >>> 8*(sizeof(Uint)-1)) % Int
        n -= count_ones((mask & (lo >>> 7)) & ~(mask & (lo >>> 6)))
        n -= count_ones((mask & (hi >>> 7)) & ~(mask & (hi >>> 6)))
        return n
    else
        p = reinterpret(Ptr{Uint8}, x % UInt)
        n = -(x >> 8*sizeof(Int)) % Int
        for i = 1:n
            b = unsafe_load(p, i)
            n -= (b & 0xc0) == 0x80
        end
        return n
    end
end

## overload methods for efficiency ##

sizeof(s::Str) = length(s.data)

    ==(s::Str, t::Str) =     ==(s.data, t.data)
isless(s::Str, t::Str) = isless(s.data, t.data)
   cmp(s::Str, t::Str) =    cmp(s.data, t.data)

