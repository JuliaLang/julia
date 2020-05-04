# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
The `AbstractChar` type is the supertype of all character implementations
in Julia. A character represents a Unicode code point, and can be converted
to an integer via the [`codepoint`](@ref) function in order to obtain the
numerical value of the code point, or constructed from the same integer.
These numerical values determine how characters are compared with `<` and `==`,
for example.  New `T <: AbstractChar` types should define a `codepoint(::T)`
method and a `T(::UInt32)` constructor, at minimum.

A given `AbstractChar` subtype may be capable of representing only a subset
of Unicode, in which case conversion from an unsupported `UInt32` value
may throw an error. Conversely, the built-in [`Char`](@ref) type represents
a *superset* of Unicode (in order to losslessly encode invalid byte streams),
in which case conversion of a non-Unicode value *to* `UInt32` throws an error.
The [`isvalid`](@ref) function can be used to check which codepoints are
representable in a given `AbstractChar` type.

Internally, an `AbstractChar` type may use a variety of encodings.  Conversion
via `codepoint(char)` will not reveal this encoding because it always returns the
Unicode value of the character. `print(io, c)` of any `c::AbstractChar`
produces an encoding determined by `io` (UTF-8 for all built-in `IO`
types), via conversion to `Char` if necessary.

`write(io, c)`, in contrast, may emit an encoding depending on
`typeof(c)`, and `read(io, typeof(c))` should read the same encoding as `write`.
New `AbstractChar` types must provide their own implementations of
`write` and `read`.
"""
AbstractChar

"""
    Char(c::Union{Number,AbstractChar})

`Char` is a 32-bit [`AbstractChar`](@ref) type that is the default representation
of characters in Julia. `Char` is the type used for character literals like `'x'`
and it is also the element type of [`String`](@ref).

In order to losslessly represent arbitrary byte streams stored in a `String`,
a `Char` value may store information that cannot be converted to a Unicode
codepoint — converting such a `Char` to `UInt32` will throw an error.
The [`isvalid(c::Char)`](@ref) function can be used to query whether `c`
represents a valid Unicode character.
"""
Char

(::Type{T})(x::Number) where {T<:AbstractChar} = T(UInt32(x))
(::Type{AbstractChar})(x::Number) = Char(x)
(::Type{T})(x::AbstractChar) where {T<:Union{Number,AbstractChar}} = T(codepoint(x))
(::Type{T})(x::T) where {T<:AbstractChar} = x
AbstractChar(x::AbstractChar) = x

"""
    ncodeunits(c::Char) -> Int

Return the number of code units required to encode a character as UTF-8.
This is the number of bytes which will be printed if the character is written
to an output stream, or `ncodeunits(string(c))` but computed efficiently.

!!! compat "Julia 1.1"
    This method requires at least Julia 1.1. In Julia 1.0 consider
    using `ncodeunits(string(c))`.
"""
ncodeunits(c::Char) = write(devnull, c) # this is surprisingly efficient

"""
    codepoint(c::AbstractChar) -> Integer

Return the Unicode codepoint (an unsigned integer) corresponding
to the character `c` (or throw an exception if `c` does not represent
a valid character). For `Char`, this is a `UInt32` value, but
`AbstractChar` types that represent only a subset of Unicode may
return a different-sized integer (e.g. `UInt8`).
"""
function codepoint end

codepoint(c::Char) = UInt32(c)

struct InvalidCharError{T<:AbstractChar} <: Exception
    char::T
end
struct CodePointError{T<:Integer} <: Exception
    code::T
end
@noinline invalid_char(c::AbstractChar) = throw(InvalidCharError(c))
@noinline code_point_err(u::Integer) = throw(CodePointError(u))

function ismalformed(c::Char)
    u = reinterpret(UInt32, c)
    l1 = leading_ones(u) << 3
    t0 = trailing_zeros(u) & 56
    (l1 == 8) | (l1 + t0 > 32) |
    (((u & 0x00c0c0c0) ⊻ 0x00808080) >> t0 != 0)
end

@inline is_overlong_enc(u::UInt32) = (u >> 24 == 0xc0) | (u >> 24 == 0xc1) | (u >> 21 == 0x0704) | (u >> 20 == 0x0f08)

function isoverlong(c::Char)
    u = reinterpret(UInt32, c)
    is_overlong_enc(u)
end

# fallback: other AbstractChar types, by default, are assumed
#           not to support malformed or overlong encodings.

"""
    ismalformed(c::AbstractChar) -> Bool

Return `true` if `c` represents malformed (non-Unicode) data according to the
encoding used by `c`.  Defaults to `false` for non-`Char` types.  See also
[`show_invalid`](@ref).
"""
ismalformed(c::AbstractChar) = false

"""
    isoverlong(c::AbstractChar) -> Bool

Return `true` if `c` represents an overlong UTF-8 sequence. Defaults
to `false` for non-`Char` types.  See also [`decode_overlong`](@ref)
and [`show_invalid`](@ref).
"""
isoverlong(c::AbstractChar) = false

function UInt32(c::Char)
    # TODO: use optimized inline LLVM
    u = reinterpret(UInt32, c)
    u < 0x80000000 && return u >> 24
    l1 = leading_ones(u)
    t0 = trailing_zeros(u) & 56
    (l1 == 1) | (8l1 + t0 > 32) |
    ((((u & 0x00c0c0c0) ⊻ 0x00808080) >> t0 != 0) | is_overlong_enc(u)) &&
        invalid_char(c)::Union{}
    u &= 0xffffffff >> l1
    u >>= t0
    ((u & 0x0000007f) >> 0) | ((u & 0x00007f00) >> 2) |
    ((u & 0x007f0000) >> 4) | ((u & 0x7f000000) >> 6)
end

"""
    decode_overlong(c::AbstractChar) -> Integer

When [`isoverlong(c)`](@ref) is `true`, `decode_overlong(c)` returns
the Unicode codepoint value of `c`. `AbstractChar` implementations
that support overlong encodings should implement `Base.decode_overlong`.
"""
function decode_overlong end

function decode_overlong(c::Char)
    u = reinterpret(UInt32, c)
    l1 = leading_ones(u)
    t0 = trailing_zeros(u) & 56
    u &= 0xffffffff >> l1
    u >>= t0
    ((u & 0x0000007f) >> 0) | ((u & 0x00007f00) >> 2) |
    ((u & 0x007f0000) >> 4) | ((u & 0x7f000000) >> 6)
end

function Char(u::UInt32)
    u < 0x80 && return reinterpret(Char, u << 24)
    u < 0x00200000 || code_point_err(u)::Union{}
    c = ((u << 0) & 0x0000003f) | ((u << 2) & 0x00003f00) |
        ((u << 4) & 0x003f0000) | ((u << 6) & 0x3f000000)
    c = u < 0x00000800 ? (c << 16) | 0xc0800000 :
        u < 0x00010000 ? (c << 08) | 0xe0808000 :
                         (c << 00) | 0xf0808080
    reinterpret(Char, c)
end

function (T::Union{Type{Int8},Type{UInt8}})(c::Char)
    i = reinterpret(Int32, c)
    i ≥ 0 ? ((i >>> 24) % T) : T(UInt32(c))
end

function Char(b::Union{Int8,UInt8})
    0 ≤ b ≤ 0x7f ? reinterpret(Char, (b % UInt32) << 24) : Char(UInt32(b))
end

convert(::Type{AbstractChar}, x::Number) = Char(x) # default to Char
convert(::Type{T}, x::Number) where {T<:AbstractChar} = T(x)
convert(::Type{T}, x::AbstractChar) where {T<:Number} = T(x)
convert(::Type{T}, c::AbstractChar) where {T<:AbstractChar} = T(c)
convert(::Type{AbstractChar}, c::AbstractChar) = c
convert(::Type{T}, c::T) where {T<:AbstractChar} = c

rem(x::AbstractChar, ::Type{T}) where {T<:Number} = rem(codepoint(x), T)

typemax(::Type{Char}) = reinterpret(Char, typemax(UInt32))
typemin(::Type{Char}) = reinterpret(Char, typemin(UInt32))

size(c::AbstractChar) = ()
size(c::AbstractChar, d::Integer) = d < 1 ? throw(BoundsError()) : 1
ndims(c::AbstractChar) = 0
ndims(::Type{<:AbstractChar}) = 0
length(c::AbstractChar) = 1
IteratorSize(::Type{Char}) = HasShape{0}()
firstindex(c::AbstractChar) = 1
lastindex(c::AbstractChar) = 1
getindex(c::AbstractChar) = c
getindex(c::AbstractChar, i::Integer) = i == 1 ? c : throw(BoundsError())
getindex(c::AbstractChar, I::Integer...) = all(x -> x == 1, I) ? c : throw(BoundsError())
first(c::AbstractChar) = c
last(c::AbstractChar) = c
eltype(::Type{T}) where {T<:AbstractChar} = T

iterate(c::AbstractChar, done=false) = done ? nothing : (c, true)
isempty(c::AbstractChar) = false
in(x::AbstractChar, y::AbstractChar) = x == y

==(x::Char, y::Char) = reinterpret(UInt32, x) == reinterpret(UInt32, y)
isless(x::Char, y::Char) = reinterpret(UInt32, x) < reinterpret(UInt32, y)
hash(x::Char, h::UInt) =
    hash_uint64(((reinterpret(UInt32, x) + UInt64(0xd4d64234)) << 32) ⊻ UInt64(h))

first_utf8_byte(c::Char) = (reinterpret(UInt32, c) >> 24) % UInt8

# fallbacks:
isless(x::AbstractChar, y::AbstractChar) = isless(Char(x), Char(y))
==(x::AbstractChar, y::AbstractChar) = Char(x) == Char(y)
hash(x::AbstractChar, h::UInt) = hash(Char(x), h)
widen(::Type{T}) where {T<:AbstractChar} = T

@inline -(x::AbstractChar, y::AbstractChar) = Int(x) - Int(y)
@inline -(x::T, y::Integer) where {T<:AbstractChar} = T(Int32(x) - Int32(y))
@inline +(x::T, y::Integer) where {T<:AbstractChar} = T(Int32(x) + Int32(y))
@inline +(x::Integer, y::AbstractChar) = y + x

# `print` should output UTF-8 by default for all AbstractChar types.
# (Packages may implement other IO subtypes to specify different encodings.)
# In contrast, `write(io, c)` outputs a `c` in an encoding determined by typeof(c).
print(io::IO, c::Char) = (write(io, c); nothing)
print(io::IO, c::AbstractChar) = print(io, Char(c)) # fallback: convert to output UTF-8

const hex_chars = UInt8['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
                        'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                        's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

function show_invalid(io::IO, c::Char)
    write(io, 0x27)
    u = reinterpret(UInt32, c)
    while true
        a = hex_chars[((u >> 28) & 0xf) + 1]
        b = hex_chars[((u >> 24) & 0xf) + 1]
        write(io, 0x5c, UInt8('x'), a, b)
        (u <<= 8) == 0 && break
    end
    write(io, 0x27)
end

"""
    show_invalid(io::IO, c::AbstractChar)

Called by `show(io, c)` when [`isoverlong(c)`](@ref) or
[`ismalformed(c)`](@ref) return `true`.   Subclasses
of `AbstractChar` should define `Base.show_invalid` methods
if they support storing invalid character data.
"""
show_invalid

# show c to io, assuming UTF-8 encoded output
function show(io::IO, c::AbstractChar)
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
    if isoverlong(c) || ismalformed(c)
        show_invalid(io, c)
    elseif isprint(c)
        write(io, 0x27)
        print(io, c) # use print, not write, to use UTF-8 for any AbstractChar
        write(io, 0x27)
    else # unprintable, well-formed, non-overlong Unicode
        u = codepoint(c)
        write(io, 0x27, 0x5c, u <= 0x7f ? 0x78 : u <= 0xffff ? 0x75 : 0x55)
        d = max(2, 8 - (leading_zeros(u) >> 2))
        while 0 < d
            write(io, hex_chars[((u >> ((d -= 1) << 2)) & 0xf) + 1])
        end
        write(io, 0x27)
    end
    return
end

function show(io::IO, ::MIME"text/plain", c::T) where {T<:AbstractChar}
    show(io, c)
    get(io, :compact, false) && return
    if !ismalformed(c)
        print(io, ": ")
        if isoverlong(c)
            print(io, "[overlong] ")
            u = decode_overlong(c)
            c = T(u)
        else
            u = codepoint(c)
        end
        h = uppercase(string(u, base = 16, pad = 4))
        print(io, (isascii(c) ? "ASCII/" : ""), "Unicode U+", h)
    else
        print(io, ": Malformed UTF-8")
    end
    abr = Unicode.category_abbrev(c)
    str = Unicode.category_string(c)
    print(io, " (category ", abr, ": ", str, ")")
end
