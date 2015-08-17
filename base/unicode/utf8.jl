# This file is a part of Julia. License is MIT: http://julialang.org/license

## from base/boot.jl:
#
# immutable UTF8String <: AbstractString
#     data::Vector{UInt8}
# end
#

## basic UTF-8 decoding & iteration ##

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

# Retained because although undocumented and unexported, used in a package (MutableStrings)
# should be deprecated
is_utf8_start(byte::UInt8) = ((byte&0xc0)!=0x80)

## required core functionality ##

function endof(s::UTF8String)
    d = s.data
    i = length(d)
    i == 0 && return i
    while is_valid_continuation(d[i])
        i -= 1
    end
    i
end

function length(s::UTF8String)
    d = s.data
    cnum = 0
    for i = 1:length(d)
        @inbounds cnum += !is_valid_continuation(d[i])
    end
    cnum
end

function next(s::UTF8String, i::Int)
    # potentially faster version
    # d = s.data
    # a::UInt32 = d[i]
    # if a < 0x80; return Char(a); end
    # #if a&0xc0==0x80; return '\ufffd'; end
    # b::UInt32 = a<<6 + d[i+1]
    # if a < 0xe0; return Char(b - 0x00003080); end
    # c::UInt32 = b<<6 + d[i+2]
    # if a < 0xf0; return Char(c - 0x000e2080); end
    # return Char(c<<6 + d[i+3] - 0x03c82080)

    d = s.data
    b = d[i]
    if is_valid_continuation(b)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    trailing = utf8_trailing[b+1]
    if length(d) < i + trailing
        return '\ufffd', i+1
    end
    c::UInt32 = 0
    for j = 1:trailing+1
        c <<= 6
        c += d[i]
        i += 1
    end
    c -= utf8_offset[trailing+1]
    Char(c), i
end

function first_utf8_byte(ch::Char)
    c = reinterpret(UInt32, ch)
    c < 0x80    ? c%UInt8 :
    c < 0x800   ? ((c>>6)  | 0xc0)%UInt8 :
    c < 0x10000 ? ((c>>12) | 0xe0)%UInt8 :
                  ((c>>18) | 0xf0)%UInt8
end

function reverseind(s::UTF8String, i::Integer)
    j = lastidx(s) + 1 - i
    d = s.data
    while is_valid_continuation(d[j])
        j -= 1
    end
    return j
end

## overload methods for efficiency ##

sizeof(s::UTF8String) = sizeof(s.data)

lastidx(s::UTF8String) = length(s.data)

isvalid(s::UTF8String, i::Integer) =
    (1 <= i <= endof(s.data)) && !is_valid_continuation(s.data[i])

const empty_utf8 = UTF8String(UInt8[])

function getindex(s::UTF8String, r::UnitRange{Int})
    isempty(r) && return empty_utf8
    i, j = first(r), last(r)
    d = s.data
    if i < 1 || i > length(s.data)
        throw(BoundsError(s, i))
    end
    if !is_utf8_start(d[i])
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    if j > length(d)
        throw(BoundsError())
    end
    j = nextind(s,j)-1
    UTF8String(d[i:j])
end

function search(s::UTF8String, c::Char, i::Integer)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return 0
        throw(BoundsError(s, i))
    end
    d = s.data
    if is_valid_continuation(d[i])
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    c < Char(0x80) && return search(d, c%UInt8, i)
    while true
        i = search(d, first_utf8_byte(c), i)
        (i==0 || s[i] == c) && return i
        i = next(s,i)[2]
    end
end

function rsearch(s::UTF8String, c::Char, i::Integer)
    c < Char(0x80) && return rsearch(s.data, c%UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(s.data, b, i)
        (i==0 || s[i] == c) && return i
        i = prevind(s,i)
    end
end

function string(a::ByteString...)
    if length(a) == 1
        return a[1]::UTF8String
    end
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called
    data = Array(UInt8,0)
    for d in a
        append!(data,d.data)
    end
    UTF8String(data)
end

function string(a::Union{ByteString,Char}...)
    s = Array(UInt8,0)
    for d in a
        if isa(d,Char)
            c = reinterpret(UInt32, d::Char)
            if c < 0x80
                push!(s, c%UInt8)
            elseif c < 0x800
                push!(s, (( c >> 6          ) | 0xC0)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            elseif c < 0x10000
                push!(s, (( c >> 12         ) | 0xE0)%UInt8)
                push!(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            elseif c < 0x110000
                push!(s, (( c >> 18         ) | 0xF0)%UInt8)
                push!(s, (((c >> 12) & 0x3F ) | 0x80)%UInt8)
                push!(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            else
                # '\ufffd'
                push!(s, 0xef); push!(s, 0xbf); push!(s, 0xbd)
            end
        else
            append!(s,d.data)
        end
    end
    UTF8String(s)
end

function reverse(s::UTF8String)
    out = similar(s.data)
    if ccall(:u8_reverse, Cint, (Ptr{UInt8}, Ptr{UInt8}, Csize_t),
             out, s.data, length(out)) == 1
        throw(UnicodeError(UTF_ERR_INVALID_8,0,0))
    end
    UTF8String(out)
end

## outputing UTF-8 strings ##

write(io::IO, s::UTF8String) = write(io, s.data)

## transcoding to UTF-8 ##

utf8(x) = convert(UTF8String, x)
convert(::Type{UTF8String}, s::UTF8String) = s
convert(::Type{UTF8String}, s::ASCIIString) = UTF8String(s.data)
convert(::Type{SubString{UTF8String}}, s::SubString{ASCIIString}) =
    SubString(utf8(s.string), s.offset+1, s.endof+s.offset)

"""
Converts a UTF-8 encoded vector of `UInt8` to a `UTF8String`

### Returns:
*   `UTF8String`

### Throws:
*   `UnicodeError`
"""
function convert(::Type{UTF8String}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(dat)
    if (flags & (UTF_LONG | UTF_SURROGATE)) == 0
        len = sizeof(dat)
        @inbounds return UTF8String(copy!(Vector{UInt8}(len), 1, dat, 1, len))
    end
    # Copy, but eliminate over-long encodings and surrogate pairs
    len += num2byte + num3byte*2 + num4byte*3
    buf = Vector{UInt8}(len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch::UInt32 = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle overlong < 0x100
        elseif ch < 0xc2
            buf[out += 1] = ((ch & 3) << 6) | (dat[pos += 1] & 0x3f)
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            buf[out += 1] = ch
            buf[out += 1] = dat[pos += 1]
        elseif ch != 0xed
            buf[out += 1] = ch
            buf[out += 1] = dat[pos += 1]
            buf[out += 1] = dat[pos += 1]
            # Copy 4-byte encoded value
            ch >= 0xf0 && (buf[out += 1] = dat[pos += 1])
        # Handle surrogate pairs
        else
            ch = dat[pos += 1]
            if ch < 0xa0 # not surrogate pairs
                buf[out += 1] = 0xed
                buf[out += 1] = ch
                buf[out += 1] = dat[pos += 1]
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch = ((((((ch & 0x3f) << 6) | (dat[pos + 1] & 0x3f)) << 10)
                       + (((dat[pos + 3] & 0x3f)%UInt32 << 6) | (dat[pos + 4] & 0x3f)))
                      - 0x01f0c00)
                pos += 4
                output_utf8_4byte!(buf, out, ch)
                out += 4
            end
        end
    end
    UTF8String(buf)
end

function convert(::Type{UTF8String}, a::Vector{UInt8}, invalids_as::AbstractString)
    l = length(a)
    idx = 1
    iscopy = false
    while idx <= l
        if !is_valid_continuation(a[idx])
            nextidx = idx+1+utf8_trailing[a[idx]+1]
            (nextidx <= (l+1)) && (idx = nextidx; continue)
        end
        !iscopy && (a = copy(a); iscopy = true)
        endn = idx
        while endn <= l
            !is_valid_continuation(a[endn]) && break
            endn += 1
        end
        (endn > idx) && (endn -= 1)
        splice!(a, idx:endn, invalids_as.data)
        l = length(a)
    end
    UTF8String(a)
end
convert(::Type{UTF8String}, s::AbstractString) = utf8(bytestring(s))

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8String`

### Input Arguments:
* `dat` Vector of code units (`UInt16` or `UInt32`), explicit `\0` is not converted
* `len` length of output in bytes

### Returns:
* `UTF8String`
"""
function encode_to_utf8{T<:Union{UInt16, UInt32}}(::Type{T}, dat, len)
    buf = Vector{UInt8}(len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch::UInt32 = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle 0x80-0x7ff
        elseif ch < 0x800
            buf[out += 1] = 0xc0 | (ch >>> 6)
            buf[out += 1] = 0x80 | (ch & 0x3f)
        # Handle 0x10000-0x10ffff (if input is UInt32)
        elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
            output_utf8_4byte!(buf, out, ch)
            out += 4
        # Handle surrogate pairs
        elseif is_surrogate_codeunit(ch)
            output_utf8_4byte!(buf, out, get_supplementary(ch, dat[pos += 1]))
            out += 4
        # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
        else
            buf[out += 1] = 0xe0 | ((ch >>> 12) & 0x3f)
            buf[out += 1] = 0x80 | ((ch >>> 6) & 0x3f)
            buf[out += 1] = 0x80 | (ch & 0x3f)
        end
    end
    UTF8String(buf)
end

utf8(p::Ptr{UInt8}) = UTF8String(bytestring(p))
utf8(p::Ptr{UInt8}, len::Integer) = utf8(pointer_to_array(p, len))
