# This file is a part of Julia. License is MIT: https://julialang.org/license

const ByteArray = Union{Vector{UInt8},Vector{Int8}}

## constructors and conversions ##

# String constructor docstring from boot.jl, workaround for #16730
# and the unavailability of @doc in boot.jl context.
"""
    String(v::Vector{UInt8})

Create a new `String` from a vector `v` of bytes containing
UTF-8 encoded characters.   This function takes "ownership" of
the array, which means that you should not subsequently modify
`v` (since strings are supposed to be immutable in Julia) for
as long as the string exists.

If you need to subsequently modify `v`, use `String(copy(v))` instead.
"""
function String(v::Array{UInt8,1})
    ccall(:jl_array_to_string, Ref{String}, (Any,), v)
end

"""
    unsafe_string(p::Ptr{UInt8}, [length::Integer])

Copy a string from the address of a C-style (NUL-terminated) string encoded as UTF-8.
(The pointer can be safely freed afterwards.) If `length` is specified
(the length of the data in bytes), the string does not have to be NUL-terminated.

This function is labelled "unsafe" because it will crash if `p` is not
a valid memory address to data of the requested length.
"""
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_pchar_to_string, Ref{String}, (Ptr{UInt8}, Int), p, len)
end
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}})
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_cstr_to_string, Ref{String}, (Ptr{UInt8},), p)
end

_string_n(n::Integer) = ccall(:jl_alloc_string, Ref{String}, (Csize_t,), n)

"""
    String(s::AbstractString)

Convert a string to a contiguous byte array representation encoded as UTF-8 bytes.
This representation is often appropriate for passing strings to C.
"""
String(s::AbstractString) = print_to_string(s)

String(s::Symbol) = unsafe_string(Cstring(s))

(::Type{Vector{UInt8}})(s::String) = ccall(:jl_string_to_array, Ref{Vector{UInt8}}, (Any,), s)

## low-level functions ##

pointer(s::String) = unsafe_convert(Ptr{UInt8}, s)
pointer(s::String, i::Integer) = pointer(s)+(i-1)

sizeof(s::String) = Core.sizeof(s)

"""
    codeunit(s::AbstractString, i::Integer)

Get the `i`th code unit of an encoded string. For example,
returns the `i`th byte of the representation of a UTF-8 string.

# Examples
```jldoctest
julia> s = "δ=γ"; [codeunit(s, i) for i in 1:sizeof(s)]
5-element Array{UInt8,1}:
 0xce
 0xb4
 0x3d
 0xce
 0xb3
```
"""
codeunit(s::AbstractString, i::Integer)

@inline function codeunit(s::String, i::Integer)
    @boundscheck if (i < 1) | (i > sizeof(s))
        throw(BoundsError(s,i))
    end
    @gc_preserve s unsafe_load(pointer(s, i))
end

"""
    ncodeunits(s::AbstractString)

The number of code units in a string. For example, for UTF-8-like data such as
the default `String` type, the number of code units is the number of bytes in
the string, a.k.a. `sizeof(s)`. For a UTF-16 encoded string type, however, the
code unit is `UInt16` so the number of code units is the number of `UInt16`
words in the representation of the string. The expression `codeunit(s, i)` is
valid and safe for precisely the range of `i` values `1:ncodeunits(s)`.

See also: [`codeunit`](@ref).
"""
ncodeunits(s::String) = sizeof(s)

write(io::IO, s::String) =
    @gc_preserve s unsafe_write(io, pointer(s), reinterpret(UInt, sizeof(s)))

## comparison ##

function cmp(a::String, b::String)
    al, bl = sizeof(a), sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              a, b, min(al,bl))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(al,bl)
end

function ==(a::String, b::String)
    al = sizeof(a)
    al == sizeof(b) && 0 == ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, al)
end

## thisind, prevind and nextind ##

function thisind(s::String, i::Integer)
    j = Int(i)
    j < 1 && return 0
    n = ncodeunits(s)
    j > n && return n + 1
    @inbounds while j > 0 && is_valid_continuation(codeunit(s,j))
        j -= 1
    end
    j
end

function prevind(s::String, i::Integer)
    j = Int(i)
    e = sizeof(s)
    if j > e
        return endof(s)
    end
    j -= 1
    @inbounds while j > 0 && is_valid_continuation(codeunit(s,j))
        j -= 1
    end
    j
end

function prevind(s::String, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    j = Int(i)
    e = sizeof(s)
    while nchar > 0
        if j > e
            j = endof(s)
        else
            j -= 1
            @inbounds while j > 0 && is_valid_continuation(codeunit(s,j))
                j -= 1
            end
        end
        nchar -= 1
        j <= 0 && return j - nchar
    end
    j
end

function nextind(s::String, i::Integer)
    j = Int(i)
    if j < 1
        return 1
    end
    e = sizeof(s)
    j += 1
    @inbounds while j <= e && is_valid_continuation(codeunit(s,j))
        j += 1
    end
    j
end

function nextind(s::String, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    j = Int(i)
    e = sizeof(s)
    while nchar > 0
        if j < 1
            j = 1
        else
            j += 1
            @inbounds while j <= e && is_valid_continuation(codeunit(s,j))
                j += 1
            end
        end
        nchar -= 1
        j > e && return j + nchar
    end
    j
end

## checking UTF-8 & ACSII validity ##

byte_string_classify(data::Vector{UInt8}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), data, length(data))
byte_string_classify(s::String) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), s, sizeof(s))
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

isvalid(::Type{String}, s::Union{Vector{UInt8},String}) = byte_string_classify(s) != 0
isvalid(s::String) = isvalid(String, s)

## basic UTF-8 decoding & iteration ##

is_surrogate_lead(c::Unsigned) = ((c & ~0x003ff) == 0xd800)
is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
is_surrogate_codeunit(c::Unsigned) = ((c & ~0x007ff) == 0xd800)
is_valid_continuation(c) = ((c & 0xc0) == 0x80)

const utf8_offset = [
    0x00000000, 0x00003080,
    0x000e2080, 0x03c82080,
    0xfa082080, 0x82082080,
]

const utf8_trailing = [
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,
]

## required core functionality ##

function endof(s::String)
    i = sizeof(s)
    @inbounds while i > 0 && is_valid_continuation(codeunit(s, i))
        i -= 1
    end
    i
end

function length(s::String)
    cnum = 0
    @inbounds for i = 1:sizeof(s)
        cnum += !is_valid_continuation(codeunit(s, i))
    end
    cnum
end

@noinline function slow_utf8_next(s::String, b::UInt8, i::Int, l::Int)
    @inbounds if is_valid_continuation(b)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, codeunit(s, i)))
    end
    trailing = utf8_trailing[b + 1]
    if l < i + trailing
        return '\ufffd', i+1
    end
    c::UInt32 = 0
    @inbounds for j = 1:(trailing + 1)
        c <<= 6
        c += codeunit(s, i)
        i += 1
    end
    c -= utf8_offset[trailing + 1]
    return Char(c), i
end

# This implementation relies on `next` returning a value past the end of the
# String's underlying data, which is true for valid Strings
done(s::String, state) = state > sizeof(s)

@inline function next(s::String, i::Int)
    # function is split into this critical fast-path
    # for pure ascii data, such as parsing numbers,
    # and a longer function that can handle any utf8 data
    @boundscheck if (i < 1) | (i > sizeof(s))
        throw(BoundsError(s,i))
    end
    @inbounds b = codeunit(s, i)
    if b < 0x80
        return Char(b), i + 1
    end
    return slow_utf8_next(s, b, i, sizeof(s))
end

function first_utf8_byte(ch::Char)
    c = UInt32(ch)
    b = c < 0x80    ? c%UInt8 :
        c < 0x800   ? ((c>>6)  | 0xc0)%UInt8 :
        c < 0x10000 ? ((c>>12) | 0xe0)%UInt8 :
                      ((c>>18) | 0xf0)%UInt8
    return b
end

## overload methods for efficiency ##

isvalid(s::String, i::Integer) =
    (1 <= i <= sizeof(s)) && ((@inbounds b = codeunit(s, i)); !is_valid_continuation(b))

function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    l = sizeof(s)
    i = first(r)
    if i < 1 || i > l
        throw(BoundsError(s, i))
    end
    @inbounds si = codeunit(s, i)
    if is_valid_continuation(si)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, si))
    end
    j = last(r)
    if j > l
        throw(BoundsError(s, j))
    end
    @inbounds sj = codeunit(s, j)
    if is_valid_continuation(sj)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, j, sj))
    end
    j = nextind(s,j)
    unsafe_string(pointer(s,i), j-i)
end

function search(s::String, c::Char, i::Integer = 1)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return 0
        throw(BoundsError(s, i))
    end
    @inbounds if is_valid_continuation(codeunit(s,i))
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, codeunit(s,i)))
    end
    c < Char(0x80) && return search(s, c%UInt8, i)
    while true
        i = search(s, first_utf8_byte(c), i)
        (i==0 || s[i] == c) && return i
        i = next(s,i)[2]
    end
end

function search(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = 1)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : Int(q-p+1)
end

function search(a::ByteArray, b::Char, i::Integer = 1)
    if isascii(b)
        search(a,UInt8(b),i)
    else
        search(a,Vector{UInt8}(string(b)),i).start
    end
end

function rsearch(s::String, c::Char, i::Integer = sizeof(s))
    c < Char(0x80) && return rsearch(s, c%UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(s, b, i)
        (i==0 || s[i] == c) && return i
        i = prevind(s,i)
    end
end

function rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = sizeof(s))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
    q == C_NULL ? 0 : Int(q-p+1)
end

function rsearch(a::ByteArray, b::Char, i::Integer = length(a))
    if isascii(b)
        rsearch(a,UInt8(b),i)
    else
        rsearch(a,Vector{UInt8}(string(b)),i).start
    end
end

## optimized concatenation, reverse, repeat ##

function string(a::String...)
    if length(a) == 1
        return a[1]::String
    end
    n = 0
    for str in a
        n += sizeof(str)
    end
    out = _string_n(n)
    offs = 1
    for str in a
        unsafe_copy!(pointer(out,offs), pointer(str), sizeof(str))
        offs += sizeof(str)
    end
    return out
end

# UTF-8 encoding length of a character
function codelen(d::Char)
    c = UInt32(d)
    if c < 0x80
        return 1
    elseif c < 0x800
        return 2
    elseif c < 0x10000
        return 3
    elseif c < 0x110000
        return 4
    end
    return 3  # '\ufffd'
end

function string(a::Union{String,Char}...)
    n = 0
    for d in a
        if isa(d,Char)
            n += codelen(d::Char)
        else
            n += sizeof(d::String)
        end
    end
    out = _string_n(n)
    offs = 1
    p = pointer(out)
    for d in a
        if isa(d,Char)
            c = UInt32(d::Char)
            if c < 0x80
                unsafe_store!(p, c%UInt8, offs); offs += 1
            elseif c < 0x800
                unsafe_store!(p, (( c >> 6          ) | 0xC0)%UInt8, offs); offs += 1
                unsafe_store!(p, (( c        & 0x3F ) | 0x80)%UInt8, offs); offs += 1
            elseif c < 0x10000
                unsafe_store!(p, (( c >> 12         ) | 0xE0)%UInt8, offs); offs += 1
                unsafe_store!(p, (((c >> 6)  & 0x3F ) | 0x80)%UInt8, offs); offs += 1
                unsafe_store!(p, (( c        & 0x3F ) | 0x80)%UInt8, offs); offs += 1
            elseif c < 0x110000
                unsafe_store!(p, (( c >> 18         ) | 0xF0)%UInt8, offs); offs += 1
                unsafe_store!(p, (((c >> 12) & 0x3F ) | 0x80)%UInt8, offs); offs += 1
                unsafe_store!(p, (((c >> 6)  & 0x3F ) | 0x80)%UInt8, offs); offs += 1
                unsafe_store!(p, (( c        & 0x3F ) | 0x80)%UInt8, offs); offs += 1
            else
                # '\ufffd'
                unsafe_store!(p, 0xef, offs); offs += 1
                unsafe_store!(p, 0xbf, offs); offs += 1
                unsafe_store!(p, 0xbd, offs); offs += 1
            end
        else
            l = sizeof(d::String)
            unsafe_copy!(pointer(out,offs), pointer(d::String), l)
            offs += l
        end
    end
    return out
end

function repeat(s::String, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    n = sizeof(s)
    out = _string_n(n*r)
    if n == 1 # common case: repeating a single ASCII char
        @inbounds ccall(:memset, Ptr{Void}, (Ptr{UInt8}, Cint, Csize_t), out, codeunit(s, 1), r)
    else
        for i=1:r
            unsafe_copy!(pointer(out, 1+(i-1)*n), pointer(s), n)
        end
    end
    return out
end

"""
    repeat(c::Char, r::Integer) -> String

Repeat a character `r` times. This can equivalently be accomplished by calling [`c^r`](@ref ^).

# Examples
```jldoctest
julia> repeat('A', 3)
"AAA"
```
"""
function repeat(c::Char, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a character $r times"))
    r == 0 && return ""
    ch = UInt(c)
    if ch < 0x80
        out = _string_n(r)
        ccall(:memset, Ptr{Void}, (Ptr{UInt8}, Cint, Csize_t), out, c, r)
    elseif ch < 0x800
        out = _string_n(2r)
        p16 = reinterpret(Ptr{UInt16}, pointer(out))
        u16 = ((ch >> 0x6) | (ch & 0x3f) << 0x8) % UInt16 | 0x80c0
        @inbounds for i = 1:r
            unsafe_store!(p16, u16, i)
        end
    elseif ch < 0x10000
        (0xd800 ≥ ch ≤ 0xdfff) || throw(ArgumentError("invalid character 0x$(hex(ch))"))
        out = _string_n(3r)
        p = pointer(out)
        b1 = (ch >> 0xc) % UInt8 | 0xe0
        b2 = ((ch >> 0x6) & 0x3f) % UInt8 | 0x80
        b3 = (ch & 0x3f) % UInt8 | 0x80
        @inbounds for i = 1:r
            unsafe_store!(p, b1)
            unsafe_store!(p, b2, 2)
            unsafe_store!(p, b3, 3)
            p += 3
        end
    elseif ch < 0x110000
        out = _string_n(4r)
        p32 = reinterpret(Ptr{UInt32}, pointer(out))
        u32 = ((ch >> 0x12) | ((ch >> 0x4) & 0x03f00) |
            ((ch << 0xa) & 0x3f0000) | ((ch & 0x3f) << 0x18)) % UInt32 | 0x808080f0
        @inbounds for i = 1:r
            unsafe_store!(p32, u32)
            p32 += 4
        end
    else
        throw(ArgumentError("invalid character 0x$(hex(ch))"))
    end
    return out
end
