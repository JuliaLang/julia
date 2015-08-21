# This file is a part of Julia. License is MIT: http://julialang.org/license

# UTF-32 basic functions
next(s::UTF32String, i::Int) = (Char(s.data[i]), i+1)
endof(s::UTF32String) = length(s.data) - 1
length(s::UTF32String) = length(s.data) - 1

reverse(s::UTF32String) = UTF32String(reverse!(copy(s.data), 1, length(s)))

sizeof(s::UTF32String) = sizeof(s.data) - sizeof(UInt32)

const empty_utf32 = UTF32String(UInt32[0])

convert(::Type{UTF32String}, c::Char) = UTF32String(UInt32[c, 0])
convert(::Type{UTF32String}, s::UTF32String) = s

function convert(::Type{UTF32String}, str::AbstractString)
    len, flags = unsafe_checkstring(str)
    buf = Vector{UInt32}(len+1)
    out = 0
    @inbounds for ch in str ; buf[out += 1] = ch ; end
    @inbounds buf[out + 1] = 0 # NULL termination
    UTF32String(buf)
end

function convert(::Type{UTF8String},  str::UTF32String)
    dat = str.data
    len = sizeof(dat) >>> 2
    # handle zero length string quickly
    len <= 1 && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(dat, 1, len-1)
    flags == 0 && @inbounds return UTF8String(copy!(Vector{UInt8}(len), 1, dat, 1, len))
    return encode_to_utf8(UInt32, dat, len + num2byte + num3byte*2 + num4byte*3)
end

function convert(::Type{UTF32String}, str::UTF8String)
    dat = str.data
    # handle zero length string quickly
    sizeof(dat) == 0 && return empty_utf32
    # Validate UTF-8 encoding, and get number of words to create
    len, flags = unsafe_checkstring(dat)
    # Optimize case where no characters > 0x7f
    flags == 0 && @inbounds return fast_utf_copy(UTF32String, UInt32, len, dat, true)
    # has multi-byte UTF-8 sequences
    buf = Vector{UInt32}(len+1)
    @inbounds buf[len+1] = 0 # NULL termination
    local ch::UInt32, surr::UInt32
    out = 0
    pos = 0
    @inbounds while out < len
        ch = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            buf[out += 1] = ((ch & 0x1f) << 6) | (dat[pos += 1] & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            pos += 2
            ch = get_utf8_3byte(dat, pos, ch)
            # Handle surrogate pairs (should have been encoded in 4 bytes)
            if is_surrogate_lead(ch)
                # Build up 32-bit character from ch and trailing surrogate in next 3 bytes
                pos += 3
                surr = ((UInt32(dat[pos-2] & 0xf) << 12)
                        | (UInt32(dat[pos-1] & 0x3f) << 6)
                        | (dat[pos] & 0x3f))
                ch = get_supplementary(ch, surr)
            end
            buf[out += 1] = ch
        # Handle range 0x10000-0x10ffff
        else
            pos += 3
            buf[out += 1] = get_utf8_4byte(dat, pos, ch)
        end
    end
    UTF32String(buf)
end

function convert(::Type{UTF32String}, str::UTF16String)
    dat = str.data
    len = sizeof(dat)
    # handle zero length string quickly (account for trailing \0)
    len <= 2 && return empty_utf32
    # get number of words to create
    len, flags, num4byte = unsafe_checkstring(dat, 1, len>>>1)
    # No surrogate pairs, do optimized copy
    (flags & UTF_UNICODE4) == 0 && @inbounds return UTF32String(copy!(Vector{Char}(len), dat))
    local ch::UInt32
    buf = Vector{UInt32}(len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch = dat[pos += 1]
        # check for surrogate pair
        if is_surrogate_lead(ch) ; ch = get_supplementary(ch, dat[pos += 1]) ; end
        buf[out += 1] = ch
    end
    UTF32String(buf)
end

function convert(::Type{UTF16String}, str::UTF32String)
    dat = str.data
    len = sizeof(dat)
    # handle zero length string quickly
    len <= 4 && return empty_utf16
    # get number of words to allocate
    len, flags, num4byte = unsafe_checkstring(dat, 1, len>>>2)
    # optimized path, no surrogates
    num4byte == 0 && @inbounds return UTF16String(copy!(Vector{UInt16}(len), dat))
    return encode_to_utf16(dat, len + num4byte)
end

function convert(::Type{UTF32String}, dat::AbstractVector{UInt32})
    @inbounds return fast_utf_copy(UTF32String, UInt32, length(dat), dat, true)
end

convert(::Type{UTF32String}, data::AbstractVector{Int32}) =
    convert(UTF32String, reinterpret(UInt32, convert(Vector{T}, data)))

convert(::Type{UTF32String}, data::AbstractVector{Char}) =
    convert(UTF32String, map(UInt32, data))

convert{T<:AbstractString, S<:Union{UInt32,Char,Int32}}(::Type{T}, v::AbstractVector{S}) =
    convert(T, utf32(v))

# specialize for performance reasons:
function convert{T<:ByteString, S<:Union{UInt32,Char,Int32}}(::Type{T}, data::AbstractVector{S})
    s = IOBuffer(Array(UInt8,length(data)), true, true)
    truncate(s,0)
    for x in data
        print(s, Char(x))
    end
    convert(T, takebuf_string(s))
end

convert(::Type{Vector{UInt32}}, str::UTF32String) = str.data
convert(::Type{Array{UInt32}},  str::UTF32String) = str.data

unsafe_convert{T<:Union{UInt32,Int32,Char}}(::Type{Ptr{T}}, s::UTF32String) =
    convert(Ptr{T}, pointer(s))

function convert(T::Type{UTF32String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return empty_utf32
    length(bytes) & 3 != 0 && throw(UnicodeError(UTF_ERR_ODD_BYTES_32,0,0))
    data = reinterpret(UInt32, bytes)
    # check for byte-order mark (BOM):
    if data[1] == 0x0000feff # native byte order
        d = Array(UInt32, length(data))
        copy!(d,1, data, 2, length(data)-1)
    elseif data[1] == 0xfffe0000 # byte-swapped
        d = Array(UInt32, length(data))
        for i = 2:length(data)
            @inbounds d[i-1] = bswap(data[i])
        end
    else
        d = Array(UInt32, length(data) + 1)
        copy!(d, 1, data, 1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    UTF32String(d)
end

function isvalid(::Type{UTF32String}, str::Union{Vector{UInt32}, Vector{Char}})
    for i=1:length(str)
        @inbounds if !isvalid(Char, UInt32(str[i])) ; return false ; end
    end
    return true
end
isvalid(str::Vector{Char}) = isvalid(UTF32String, str)

utf32(x) = convert(UTF32String, x)

utf32(p::Ptr{UInt32}, len::Integer) = utf32(pointer_to_array(p, len))
utf32(p::Union{Ptr{Char}, Ptr{Int32}}, len::Integer) = utf32(convert(Ptr{UInt32}, p), len)
function utf32(p::Union{Ptr{UInt32}, Ptr{Char}, Ptr{Int32}})
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf32(p, len)
end

function map(f, s::UTF32String)
    d = s.data
    out = similar(d)
    out[end] = 0

    @inbounds for i = 1:(length(d)-1)
        c2 = f(Char(d[i]))
        if !isa(c2, Char)
            throw(UnicodeError(UTF_ERR_MAP_CHAR, 0, 0))
        end
        out[i] = (c2::Char)
    end
    UTF32String(out)
end

# Definitions for C compatible strings, that don't allow embedded
# '\0', and which are terminated by a '\0'

containsnul(s::AbstractString) = '\0' in s
containsnul(s::ByteString) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))
containsnul(s::Union{UTF16String,UTF32String}) = findfirst(s.data, 0) != length(s.data)

if sizeof(Cwchar_t) == 2
    const WString = UTF16String
    const wstring = utf16
elseif sizeof(Cwchar_t) == 4
    const WString = UTF32String
    const wstring = utf32
end
wstring(s::Cwstring) = wstring(convert(Ptr{Cwchar_t}, s))

# Cwstring is defined in c.jl, but conversion needs to be defined here
# to have WString
function unsafe_convert(::Type{Cwstring}, s::WString)
    if containsnul(s)
        throw(ArgumentError("embedded NUL chars are not allowed in C strings: $(repr(s))"))
    end
    return Cwstring(unsafe_convert(Ptr{Cwchar_t}, s))
end

# pointer conversions of ASCII/UTF8/UTF16/UTF32 strings:
pointer(x::Union{ByteString,UTF16String,UTF32String}) = pointer(x.data)
pointer(x::ByteString, i::Integer) = pointer(x.data)+(i-1)
pointer(x::Union{UTF16String,UTF32String}, i::Integer) = pointer(x)+(i-1)*sizeof(eltype(x.data))

# pointer conversions of SubString of ASCII/UTF8/UTF16/UTF32:
pointer{T<:ByteString}(x::SubString{T}) = pointer(x.string.data) + x.offset
pointer{T<:ByteString}(x::SubString{T}, i::Integer) = pointer(x.string.data) + x.offset + (i-1)
pointer{T<:Union{UTF16String,UTF32String}}(x::SubString{T}) = pointer(x.string.data) + x.offset*sizeof(eltype(x.string.data))
pointer{T<:Union{UTF16String,UTF32String}}(x::SubString{T}, i::Integer) = pointer(x.string.data) + (x.offset + (i-1))*sizeof(eltype(x.string.data))
