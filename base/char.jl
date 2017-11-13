# This file is a part of Julia. License is MIT: https://julialang.org/license

struct MalformedCharError <: Exception
    char::Char
end
struct CodePointError <: Exception
    code::Integer
end
@noinline malformed_char(c::Char) = throw(MalformedCharError(c))
@noinline code_point_err(u::UInt32) = throw(CodePointError(u))

function ismalformed(c::Char)
    u = reinterpret(UInt32, c)
    l1 = leading_ones(u)
    t0 = trailing_zeros(u) >> 3
    (l1 == 1) | (l1 + t0 > 4) |
    (((u & 0x00c0c0c0) ⊻ 0x00808080) >> (t0 << 3) != 0)
end

function convert(::Type{UInt32}, c::Char)
    # TODO: use optimized inline LLVM
    u = reinterpret(UInt32, c)
    u < 0x80000000 && return reinterpret(UInt32, u >> 24)
    l1 = leading_ones(u)
    t0 = trailing_zeros(u) >> 3
    (l1 == 1) | (l1 + t0 > 4) |
    (((u & 0x00c0c0c0) ⊻ 0x00808080) >> (t0 << 3) != 0) &&
        malformed_char(c)::Union{}
    u &= 0xffffffff >> l1
    u >>= 8t0
    (u & 0x0000007f >> 0) | (u & 0x00007f00 >> 2) |
    (u & 0x007f0000 >> 4) | (u & 0x7f000000 >> 6)
end

function convert(::Type{Char}, u::UInt32)
    u < 0x80 && return reinterpret(Char, u << 24)
    u < 0x00200000 || code_point_err(u)::Union{}
    c = ((u << 0) & 0x0000003f) | ((u << 2) & 0x00003f00) |
        ((u << 4) & 0x003f0000) | ((u << 6) & 0x3f000000)
    c = u < 0x00000800 ? (c << 16) | 0xc0800000 :
        u < 0x00010000 ? (c << 08) | 0xe0808000 :
                         (c << 00) | 0xf0808080
    reinterpret(Char, c)
end

function convert(::Type{T}, c::Char) where T <: Union{Int8,UInt8}
    i = reinterpret(Int32, c)
    i ≥ 0 ? ((i >>> 24) % T) : T(UInt32(c))
end

function convert(::Type{Char}, b::Union{Int8,UInt8})
    0 ≤ b ≤ 0x7f ? reinterpret(Char, (b % UInt32) << 24) : Char(UInt32(b))
end

convert(::Type{Char}, x::Number) = Char(UInt32(x))
convert(::Type{T}, x::Char) where {T<:Number} = convert(T, UInt32(x))

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

==(x::Char, y::Char) = reinterpret(UInt32, x) == reinterpret(UInt32, y)
isless(x::Char, y::Char) = reinterpret(UInt32, x) < reinterpret(UInt32, y)
hash(x::Char, h::UInt) = hash(reinterpret(UInt32, x), hash(Char, h))

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
    elseif !ismalformed(c)
        u = UInt32(c)
        write(io, 0x27, 0x5c, c <= '\x7f' ? 0x78 : c <= '\uffff' ? 0x75 : 0x55)
        d = max(2, 8 - (leading_zeros(u) >> 2))
        while 0 < d
            write(io, hex_chars[((u >> ((d -= 1) << 2)) & 0xf) + 1])
        end
        write(io, 0x27)
    else # malformed
        write(io, 0x27)
        u = reinterpret(UInt32, c)
        while true
            a = hex_chars[((u >> 28) & 0xf) + 1]
            b = hex_chars[((u >> 24) & 0xf) + 1]
            write(io, 0x5c, 'x', a, b)
            (u <<= 8) == 0 && break
        end
        write(io, 0x27)
    end
    return
end

function show(io::IO, ::MIME"text/plain", c::Char)
    show(io, c)
    if !ismalformed(c)
        u = UInt32(c)
        print(io, ": ", isascii(c) ? "ASCII/" : "", "Unicode U+", hex(u, u > 0xffff ? 6 : 4))
    else
        print(io, ": Malformed UTF-8")
    end
    abr = UTF8proc.category_abbrev(c)
    str = UTF8proc.category_string(c)
    print(io, " (category ", abr, ": ", str, ")")
end
