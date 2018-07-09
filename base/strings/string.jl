# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    StringIndexError(str, i)

An error occurred when trying to access `str` at index `i` that is not valid.
"""
struct StringIndexError <: Exception
    string::AbstractString
    index::Integer
end
@noinline string_index_err(s::AbstractString, i::Integer) =
    throw(StringIndexError(s, Int(i)))

const ByteArray = Union{Vector{UInt8},Vector{Int8}}

@inline between(b::T, lo::T, hi::T) where {T<:Integer} = (lo ≤ b) & (b ≤ hi)

## constructors and conversions ##

# String constructor docstring from boot.jl, workaround for #16730
# and the unavailability of @doc in boot.jl context.
"""
    String(v::AbstractVector{UInt8})

Create a new `String` object from a byte vector `v` containing UTF-8 encoded
characters. If `v` is `Vector{UInt8}` it will be truncated to zero length and
future modification of `v` cannot affect the contents of the resulting string.
To avoid truncation use `String(copy(v))`.

When possible, the memory of `v` will be used without copying when the `String`
object is created. This is guaranteed to be the case for byte vectors returned
by [`take!`](@ref) on a writable [`IOBuffer`](@ref) and by calls to
[`read(io, nb)`](@ref). This allows zero-copy conversion of I/O data to strings.
In other cases, `Vector{UInt8}` data may be copied, but `v` is truncated anyway
to guarantee consistent behavior.
"""
String(v::AbstractVector{UInt8}) = String(copyto!(StringVector(length(v)), v))
String(v::Vector{UInt8}) = ccall(:jl_array_to_string, Ref{String}, (Any,), v)

"""
    unsafe_string(p::Ptr{UInt8}, [length::Integer])

Copy a string from the address of a C-style (NUL-terminated) string encoded as UTF-8.
(The pointer can be safely freed afterwards.) If `length` is specified
(the length of the data in bytes), the string does not have to be NUL-terminated.

This function is labeled "unsafe" because it will crash if `p` is not
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
String(s::Symbol) = unsafe_string(unsafe_convert(Ptr{UInt8}, s))

unsafe_wrap(::Type{Vector{UInt8}}, s::String) = ccall(:jl_string_to_array, Ref{Vector{UInt8}}, (Any,), s)

(::Type{Vector{UInt8}})(s::CodeUnits{UInt8,String}) = copyto!(Vector{UInt8}(undef, length(s)), s)

String(s::CodeUnits{UInt8,String}) = s.s

## low-level functions ##

pointer(s::String) = unsafe_convert(Ptr{UInt8}, s)
pointer(s::String, i::Integer) = pointer(s)+(i-1)

ncodeunits(s::String) = Core.sizeof(s)
sizeof(s::String) = Core.sizeof(s)
codeunit(s::String) = UInt8

@inline function codeunit(s::String, i::Integer)
    @boundscheck checkbounds(s, i)
    GC.@preserve s unsafe_load(pointer(s, i))
end

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

typemin(::Type{String}) = ""
typemin(::String) = typemin(String)

## thisind, nextind ##

thisind(s::String, i::Int) = _thisind_str(s, i)

# s should be String or SubString{String}
function _thisind_str(s, i::Int)
    i == 0 && return 0
    n = ncodeunits(s)
    i == n + 1 && return i
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    @inbounds b = codeunit(s, i)
    (b & 0xc0 == 0x80) & (i-1 > 0) || return i
    @inbounds b = codeunit(s, i-1)
    between(b, 0b11000000, 0b11110111) && return i-1
    (b & 0xc0 == 0x80) & (i-2 > 0) || return i
    @inbounds b = codeunit(s, i-2)
    between(b, 0b11100000, 0b11110111) && return i-2
    (b & 0xc0 == 0x80) & (i-3 > 0) || return i
    @inbounds b = codeunit(s, i-3)
    between(b, 0b11110000, 0b11110111) && return i-3
    return i
end

nextind(s::String, i::Int) = _nextind_str(s, i)

# s should be String or SubString{String}
function _nextind_str(s, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    @inbounds l = codeunit(s, i)
    (l < 0x80) | (0xf8 ≤ l) && return i+1
    if l < 0xc0
        i′ = thisind(s, i)
        return i′ < i ? nextind(s, i′) : i+1
    end
    # first continuation byte
    (i += 1) > n && return i
    @inbounds b = codeunit(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xe0) && return i
    # second continuation byte
    @inbounds b = codeunit(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xf0) && return i
    # third continuation byte
    @inbounds b = codeunit(s, i)
    ifelse(b & 0xc0 ≠ 0x80, i, i+1)
end

## checking UTF-8 & ACSII validity ##

byte_string_classify(data::Vector{UInt8}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), data, length(data))
byte_string_classify(s::String) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), s, sizeof(s))
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

isvalid(::Type{String}, s::Union{Vector{UInt8},String}) = byte_string_classify(s) ≠ 0
isvalid(s::String) = isvalid(String, s)

is_valid_continuation(c) = c & 0xc0 == 0x80

## required core functionality ##

@propagate_inbounds function iterate(s::String, i::Int=firstindex(s))
    i > ncodeunits(s) && return nothing
    b = codeunit(s, i)
    u = UInt32(b) << 24
    between(b, 0x80, 0xf7) || return reinterpret(Char, u), i+1
    return next_continued(s, i, u)
end

function next_continued(s::String, i::Int, u::UInt32)
    u < 0xc0000000 && (i += 1; @goto ret)
    n = ncodeunits(s)
    # first continuation byte
    (i += 1) > n && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16
    # second continuation byte
    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8
    # third continuation byte
    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b); i += 1
@label ret
    return reinterpret(Char, u), i
end

@propagate_inbounds function getindex(s::String, i::Int)
    b = codeunit(s, i)
    u = UInt32(b) << 24
    between(b, 0x80, 0xf7) || return reinterpret(Char, u)
    return getindex_continued(s, i, u)
end

function getindex_continued(s::String, i::Int, u::UInt32)
    if u < 0xc0000000
        # called from `getindex` which checks bounds
        @inbounds isvalid(s, i) && @goto ret
        string_index_err(s, i)
    end
    n = ncodeunits(s)

    (i += 1) > n && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 1
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16

    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 2
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8

    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 3
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b)
@label ret
    return reinterpret(Char, u)
end

getindex(s::String, r::UnitRange{<:Integer}) = s[Int(first(r)):Int(last(r))]

function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(s, r)
        @inbounds isvalid(s, i) || string_index_err(s, i)
        @inbounds isvalid(s, j) || string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    n = j - i + 1
    ss = _string_n(n)
    p = pointer(ss)
    for k = 1:n
        unsafe_store!(p, codeunit(s, i + k - 1), k)
    end
    return ss
end

function length(s::String, i::Int, j::Int)
    @boundscheck begin
        0 < i ≤ ncodeunits(s)+1 || throw(BoundsError(s, i))
        0 ≤ j < ncodeunits(s)+1 || throw(BoundsError(s, j))
    end
    j < i && return 0
    @inbounds i, k = thisind(s, i), i
    c = j - i + (i == k)
    _length(s, i, j, c)
end

length(s::String) = _length(s, 1, ncodeunits(s), ncodeunits(s))

@inline function _length(s::String, i::Int, n::Int, c::Int)
    i < n || return c
    @inbounds b = codeunit(s, i)
    @inbounds while true
        while true
            (i += 1) ≤ n || return c
            0xc0 ≤ b ≤ 0xf7 && break
            b = codeunit(s, i)
        end
        l = b
        b = codeunit(s, i) # cont byte 1
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xe0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 2
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xf0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 3
        c -= (b & 0xc0 == 0x80)
    end
end

# TODO: delete or move to char.jl
first_utf8_byte(c::Char) = (reinterpret(UInt32, c) >> 24) % UInt8

## overload methods for efficiency ##

isvalid(s::String, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

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
        unsafe_copyto!(pointer(out,offs), pointer(str), sizeof(str))
        offs += sizeof(str)
    end
    return out
end

# UTF-8 encoding length of a character
# TODO: delete or move to char.jl
codelen(c::Char) = 4 - (trailing_zeros(0xff000000 | reinterpret(UInt32, c)) >> 3)

function string(a::Union{String,AbstractChar}...)
    io = IOBuffer()
    for x in a
        print(io, x)
    end
    return String(resize!(io.data, io.size))
end

function repeat(s::String, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    n = sizeof(s)
    out = _string_n(n*r)
    if n == 1 # common case: repeating a single-byte string
        @inbounds b = codeunit(s, 1)
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), out, b, r)
    else
        for i = 0:r-1
            unsafe_copyto!(pointer(out, i*n+1), pointer(s), n)
        end
    end
    return out
end

"""
    repeat(c::AbstractChar, r::Integer) -> String

Repeat a character `r` times. This can equivalently be accomplished by calling [`c^r`](@ref ^).

# Examples
```jldoctest
julia> repeat('A', 3)
"AAA"
```
"""
repeat(c::AbstractChar, r::Integer) = repeat(Char(c), r) # fallback
function repeat(c::Char, r::Integer)
    r == 0 && return ""
    r < 0 && throw(ArgumentError("can't repeat a character $r times"))
    u = bswap(reinterpret(UInt32, c))
    n = 4 - (leading_zeros(u | 0xff) >> 3)
    s = _string_n(n*r)
    p = pointer(s)
    if n == 1
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), p, u % UInt8, r)
    elseif n == 2
        p16 = reinterpret(Ptr{UInt16}, p)
        for i = 1:r
            unsafe_store!(p16, u % UInt16, i)
        end
    elseif n == 3
        b1 = (u >> 0) % UInt8
        b2 = (u >> 8) % UInt8
        b3 = (u >> 16) % UInt8
        for i = 0:r-1
            unsafe_store!(p, b1, 3i + 1)
            unsafe_store!(p, b2, 3i + 2)
            unsafe_store!(p, b3, 3i + 3)
        end
    elseif n == 4
        p32 = reinterpret(Ptr{UInt32}, pointer(s))
        for i = 1:r
            unsafe_store!(p32, u, i)
        end
    end
    return s
end
