# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias ByteArray Union{Vector{UInt8},Vector{Int8}}

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

convert(::Type{Vector{UInt8}}, s::String) = ccall(:jl_string_to_array, Ref{Vector{UInt8}}, (Any,), s)
convert(::Type{String}, s::String) = s
convert(::Type{String}, v::Vector{UInt8}) = String(v)

## low-level functions ##

pointer(s::String) = unsafe_convert(Ptr{UInt8}, s)
pointer(s::String, i::Integer) = pointer(s)+(i-1)

sizeof(s::String) = s.len

"""
    codeunit(s::AbstractString, i::Integer)

Get the `i`th code unit of an encoded string. For example,
returns the `i`th byte of the representation of a UTF-8 string.
"""
codeunit(s::AbstractString, i::Integer)

@inline function codeunit(s::String, i::Integer)
    @boundscheck if (i < 1) | (i > s.len)
        throw(BoundsError(s,i))
    end
    unsafe_load(pointer(s),i)
end

write(io::IO, s::String) = unsafe_write(io, pointer(s), UInt(s.len))

## comparison ##

function cmp(a::String, b::String)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              a, b, min(a.len,b.len))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(a.len,b.len)
end

function ==(a::String, b::String)
    a.len == b.len && 0 == ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, a.len)
end

## prevind and nextind ##

function prevind(s::String, i::Integer)
    j = Int(i)
    e = s.len
    if j > e
        return endof(s)
    end
    j -= 1
    @inbounds while j > 0 && is_valid_continuation(codeunit(s,j))
        j -= 1
    end
    j
end

function nextind(s::String, i::Integer)
    j = Int(i)
    if j < 1
        return 1
    end
    e = s.len
    j += 1
    @inbounds while j <= e && is_valid_continuation(codeunit(s,j))
        j += 1
    end
    j
end

## checking UTF-8 & ACSII validity ##

byte_string_classify(data::Vector{UInt8}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), data, length(data))
byte_string_classify(s::String) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), s, s.len)
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
    p = pointer(s)
    i = s.len
    while i > 0 && is_valid_continuation(unsafe_load(p,i))
        i -= 1
    end
    i
end

function length(s::String)
    p = pointer(s)
    cnum = 0
    for i = 1:s.len
        cnum += !is_valid_continuation(unsafe_load(p,i))
    end
    cnum
end

@noinline function slow_utf8_next(p::Ptr{UInt8}, b::UInt8, i::Int, l::Int)
    if is_valid_continuation(b)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, unsafe_load(p,i)))
    end
    trailing = utf8_trailing[b + 1]
    if l < i + trailing
        return '\ufffd', i+1
    end
    c::UInt32 = 0
    for j = 1:(trailing + 1)
        c <<= 6
        c += unsafe_load(p,i)
        i += 1
    end
    c -= utf8_offset[trailing + 1]
    return Char(c), i
end

# This implementation relies on `next` returning a value past the end of the
# String's underlying data, which is true for valid Strings
done(s::String, state) = state > s.len

@inline function next(s::String, i::Int)
    # function is split into this critical fast-path
    # for pure ascii data, such as parsing numbers,
    # and a longer function that can handle any utf8 data
    @boundscheck if (i < 1) | (i > s.len)
        throw(BoundsError(s,i))
    end
    p = pointer(s)
    b = unsafe_load(p, i)
    if b < 0x80
        return Char(b), i + 1
    end
    return slow_utf8_next(p, b, i, s.len)
end

function first_utf8_byte(ch::Char)
    c = UInt32(ch)
    b = c < 0x80    ? c%UInt8 :
        c < 0x800   ? ((c>>6)  | 0xc0)%UInt8 :
        c < 0x10000 ? ((c>>12) | 0xe0)%UInt8 :
                      ((c>>18) | 0xf0)%UInt8
    return b
end

function reverseind(s::String, i::Integer)
    j = s.len + 1 - i
    p = pointer(s)
    while is_valid_continuation(unsafe_load(p,j))
        j -= 1
    end
    return j
end

## overload methods for efficiency ##

isvalid(s::String, i::Integer) =
    (1 <= i <= s.len) && !is_valid_continuation(unsafe_load(pointer(s),i))

function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    i, j = first(r), last(r)
    l = s.len
    if i < 1 || i > l
        throw(BoundsError(s, i))
    end
    @inbounds si = codeunit(s, i)
    if is_valid_continuation(si)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, si))
    end
    if j > l
        throw(BoundsError())
    end
    j = nextind(s,j)-1
    unsafe_string(pointer(s,i), j-i+1)
end

function search(s::String, c::Char, i::Integer = 1)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return 0
        throw(BoundsError(s, i))
    end
    if is_valid_continuation(codeunit(s,i))
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

function rsearch(s::String, c::Char, i::Integer = s.len)
    c < Char(0x80) && return rsearch(s, c%UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(s, b, i)
        (i==0 || s[i] == c) && return i
        i = prevind(s,i)
    end
end

function rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = s.len)
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
        n += str.len
    end
    out = _string_n(n)
    offs = 1
    for str in a
        unsafe_copy!(pointer(out,offs), pointer(str), str.len)
        offs += str.len
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
            n += (d::String).len
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
            l = (d::String).len
            unsafe_copy!(pointer(out,offs), pointer(d::String), l)
            offs += l
        end
    end
    return out
end

function reverse(s::String)
    dat = convert(Vector{UInt8},s)
    n = length(dat)
    n <= 1 && return s
    buf = StringVector(n)
    out = n
    pos = 1
    @inbounds while out > 0
        ch = dat[pos]
        if ch > 0xdf
            if ch < 0xf0
                (out -= 3) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out + 1], buf[out + 2], buf[out + 3] = ch, dat[pos + 1], dat[pos + 2]
                pos += 3
            else
                (out -= 4) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out+1], buf[out+2], buf[out+3], buf[out+4] = ch, dat[pos+1], dat[pos+2], dat[pos+3]
                pos += 4
            end
        elseif ch > 0x7f
            (out -= 2) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
            buf[out + 1], buf[out + 2] = ch, dat[pos + 1]
            pos += 2
        else
            buf[out] = ch
            out -= 1
            pos += 1
        end
    end
    String(buf)
end

function repeat(s::String, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    n = s.len
    out = _string_n(n*r)
    for i=1:r
        unsafe_copy!(pointer(out, 1+(i-1)*n), pointer(s), n)
    end
    return out
end
