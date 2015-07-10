# This file is a part of Julia. License is MIT: http://julialang.org/license

# Functions to convert to different UTF encodings

# Quickly copy and set trailing \0
@inline function fast_utf_copy(T::Type{UInt16}, len, dat)
    @inbounds return UTF16String(setindex!(copy!(Vector{T}(len), dat), 0, len))
end
@inline function fast_utf_copy(T::Type{Char}, len, dat)
    @inbounds return UTF32String(setindex!(copy!(Vector{T}(len), dat), 0, len))
end

# Get rest of character ch from 3-byte UTF-8 sequence in dat
@inline function get_utf8_3(dat, pos, ch)
    @inbounds return ((ch & 0xf) << 12) | (UInt32(dat[pos-1] & 0x3f) << 6) | (dat[pos] & 0x3f)
end

# Get rest of character ch from 4-byte UTF-8 sequence in dat
@inline function get_utf8_4(dat, pos, ch)
    @inbounds return (((ch & 0x7) << 18)
                        | (UInt32(dat[pos-2] & 0x3f) << 12)
                        | (UInt32(dat[pos-1] & 0x3f) << 6)
                        | (dat[pos] & 0x3f))
end

# Output a character as a 4-byte UTF-8 sequence
@inline function output_utf8_4(buf, out, ch)
    @inbounds begin
        buf[out + 1] = 0xf0 | (ch >>> 18)
        buf[out + 2] = 0x80 | ((ch >>> 12) & 0x3f)
        buf[out + 3] = 0x80 | ((ch >>> 6) & 0x3f)
        buf[out + 4] = 0x80 | (ch & 0x3f)
    end
end

#=
"""
@brief      Converts an AbstractString to a UTF16String

@param[in]  ::Type{UTF16String}
@param[in]  str::AbstractString

@return     ::UTF16String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF16String}, str::AbstractString)
    len, flags, num4byte = check_string_abs(str)
    buf = Vector{UInt16}(len+num4byte+1)
    out = 0
    @inbounds for ch in str
        c = UInt32(ch)
        if c < 0x10000
            buf[out += 1] = UInt16(c)
        else
            # output surrogate pair
            buf[out += 1] = UInt16(0xd7c0 + (ch >>> 10))
            buf[out += 1] = UInt16(0xdc00 + (ch & 0x3ff))
        end
    end
    @inbounds buf[out + 1] = 0 # NULL termination
    UTF16String(buf)
end

#=
"""
@brief      Converts an AbstractString to a UTF32String

@param[in]  ::Type{UTF32String}
@param[in]  str::AbstractString

@return     ::UTF32String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF32String}, str::AbstractString)
    len, flags = check_string_abs(str)
    buf = Vector{Char}(len+1)
    out = 0
    @inbounds for ch in str ; buf[out += 1] = ch ; end
    @inbounds buf[out + 1] = 0 # NULL termination
    UTF32String(buf)
end

#=
@doc """
@brief      Converts a UTF8String to a UTF16String

@param[in]  ::Type{UTF16String}
@param[in]  str::UTF8String

@return     ::UTF16String
@throws     ArgumentError
""" ->
=#
function convert(::Type{UTF16String}, str::UTF8String)
    dat = str.data
    # handle zero length string quickly
    sizeof(dat) == 0 && return empty_utf16
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = check_string_utf8(dat)
    len += num4byte
    buf = Vector{UInt16}(len+1)
    @inbounds buf[len+1] = 0
    # Optimize case where no characters > 0x7f
    flags == 0 && @inbounds return UTF16String(copy!(buf, dat))
    out = 0
    pos = 0
    @inbounds while out < len
        ch::UInt32 = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            buf[out += 1] = ((ch & 0x1f) << 6) | (dat[pos += 1] & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            pos += 2
            buf[out += 1] = get_utf8_3(dat, pos, ch)
        # Handle range 0x10000-0x10ffff
        else
            pos += 3
            ch = get_utf8_4(dat, pos, ch)
            # output surrogate pair
            buf[out += 1] = UInt16(0xd7c0 + (ch >>> 10))
            buf[out += 1] = UInt16(0xdc00 + (ch & 0x3ff))
        end
    end
    UTF16String(buf)
end

#=
@doc """
@brief      Converts a UTF-16 encoded vector of UInt16 to a UTF8String

@param[in]  ::Type{UTF8String}
@param[in]  dat::Vector{UInt16}

@return     ::UTF8String
@throws     ArgumentError
""" ->
=#
function convert(::Type{UTF8String}, dat::Vector{UInt16})
    len = sizeof(dat)
    # handle zero length string quickly
    len == 0 && return UTF8String("")
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = check_string_utf16(dat, len>>>1)
    flags == 0 && @inbounds return UTF8String(copy!(Vector{UInt8}(len), dat))
    return encode_to_utf8(UInt16, dat, len + num2byte + num3byte*2 + num4byte*3)
end

#=
@doc """
@brief      Converts a UTF16String to a UTF8String

@param[in]  ::Type{UTF8String}
@param[in]  str::UTF16String

@return     ::UTF8String
@throws     ArgumentError
""" ->
=#
function convert(::Type{UTF8String}, str::UTF16String)
    dat = str.data
    len = sizeof(dat) >>> 1
    # handle zero length string quickly
    len <= 1 && return UTF8String("")
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = check_string_utf16(dat, len-1)
    flags == 0 && @inbounds return UTF8String(copy!(Vector{UInt8}(len), 1, dat, 1, len))
    return encode_to_utf8(UInt16, dat, len + num2byte + num3byte*2 + num4byte*3)
end

#=
@doc """
@brief      Encodes a UTF-32 encoded vector of UInt32 to a UTF8String

@param[in]  ::Type{UTF8String}
@param[in]  dat::Vector{UInt32}

@return     ::UTF8String
@throws     ArgumentError
""" ->
=#
function convert(::Type{UTF8String}, dat::Vector{UInt32})
    len = sizeof(dat)
    # handle zero length string quickly
    len == 0 && return UTF8String("")
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = check_string_utf32(dat, len>>>2)
    flags == 0 && @inbounds return UTF8String(copy!(Vector{UInt8}(len), 1, dat, 1, len))
    return encode_to_utf8(UInt32, dat, len + num2byte + num3byte*2 + num4byte*3)
end

#=
@doc """
@brief      Converts a UTF32String to a UTF8String

@param[in]  ::Type{UTF8String}
@param[in]  str::UTF32String

@return     ::UTF8String
@throws     ArgumentError
""" ->
=#
function convert(::Type{UTF8String},  str::UTF32String)
    dat = reinterpret(UInt32, str.data)
    len = sizeof(dat) >>> 2
    # handle zero length string quickly
    len <= 1 && return UTF8String("")
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = check_string_utf32(dat, len-1)
    flags == 0 && @inbounds return UTF8String(copy!(Vector{UInt8}(len), 1, dat, 1, len))
    return encode_to_utf8(UInt32, dat, len + num2byte + num3byte*2 + num4byte*3)
end

#=
@doc """
@brief      Converts an already validated vector of UInt16 or UInt32 to a UTF8String

@param[in]  T           type (UInt16 or UInt32)
@param[in]  dat         Vector{T}
@param[in]  len         length of output in bytes

@return     ::UTF8String
""" ->
=#
function encode_to_utf8{T<:Union(UInt16, UInt32)}(::Type{T}, dat, len)
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
        elseif T == UInt32 && ch > 0xffff
            output_utf8_4(buf, out, ch)
            out += 4
        # Handle surrogate pairs
        elseif is_surrogate_codeunit(ch)
            output_utf8_4(buf, out, get_supplementary(ch, dat[pos += 1]))
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

#=
"""
@brief      Converts a UTF8String to a UTF32String

@param[in]  ::Type{UTF32String}
@param[in]  str::UTF8String

@return     ::UTF32String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF32String}, str::UTF8String)
    dat = str.data
    # handle zero length string quickly
    sizeof(dat) == 0 && return empty_utf32
    # Validate UTF-8 encoding, and get number of words to create
    len, flags = check_string_utf8(dat)
    # Optimize case where no characters > 0x7f
    totlen = len+1
    flags == 0 && return fast_utf_copy(Char, totlen, dat)
    # has multi-byte UTF-8 sequences
    buf = Vector{Char}(totlen)
    @inbounds buf[totlen] = 0 # NULL termination
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
            ch = get_utf8_3(dat, pos, ch)
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
            buf[out += 1] = get_utf8_4(dat, pos, ch)
        end
    end
    UTF32String(buf)
end

#=
"""
@brief      Converts a UTF16String to UTF32String

@param[in]  ::Type{UTF32String}
@param[in]  str::UTF16String

@return     ::UTF32String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF32String}, str::UTF16String)
    dat = str.data
    len = sizeof(dat)
    # handle zero length string quickly (account for trailing \0)
    len <= 2 && return empty_utf32
    # get number of words to create
    len, flags, num4byte = check_string_utf16(dat, len>>>1)
    # No surrogate pairs, do optimized copy
    (flags & UTF_UNICODE4) == 0 && @inbounds return UTF32String(copy!(Vector{Char}(len), dat))
    local ch::UInt32
    buf = Vector{Char}(len)
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

#=
"""
@brief      Converts a UTF-32 encoded vector of UInt32 to a UTF16String

@param[in]  ::Type{UTF16String}
@param[in]  dat::Vector{UInt32}

@return     ::UTF16String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF16String}, dat::Vector{UInt32})
    len = sizeof(dat)
    # handle zero length string quickly
    len <= 4 && return empty_utf16
    # get number of words to allocate
    len, flags, num4byte = check_string_utf32(dat, len>>>2)
    len += num4byte + 1
    # optimized path, no surrogates
    num4byte == 0 && return fast_utf_copy(UInt16, len, dat)
    return encode_to_utf16(dat, len)
end

#=
"""
@brief      Converts a UTF32String to UTF16String

@param[in]  ::Type{UTF16String}
@param[in]  str::UTF32String

@return     ::UTF16String
@throws     ArgumentError
"""
=#
function convert(::Type{UTF16String}, str::UTF32String)
    dat = reinterpret(UInt32, str.data)
    len = sizeof(dat)
    # handle zero length string quickly
    len <= 4 && return empty_utf16
    # get number of words to allocate
    len, flags, num4byte = check_string_utf32(dat, len>>>2)
    # optimized path, no surrogates
    num4byte == 0 && @inbounds return UTF16String(copy!(Vector{UInt16}(len), dat))
    return encode_to_utf16(dat, len + num4byte)
end

#=
@doc """
@brief      Converts an already validated UTF-32 encoded vector of UInt32 to a UTF16String

@param[in]  dat::Vector{UInt32} UTF-32 encoded data
@param[in]  len                 length of output in 16-bit words

@return     ::UTF16String
""" ->
=#
function encode_to_utf16(dat, len)
    buf = Vector{UInt16}(len)
    @inbounds buf[len] = 0 # NULL termination
    out = 0
    pos = 0
    @inbounds while out < len
        ch = UInt32(dat[pos += 1])
        if ch > 0xffff
            # Output surrogate pair for 0x10000-0x10ffff
            buf[out += 1] = 0xd7c0 + (ch >>> 10)
            ch = 0xdc00 + (ch & 0x3ff)
        end
        buf[out += 1] = ch
    end
    UTF16String(buf)
end

convert(::Type{UTF8String},  dat::Vector{Char})   = convert(UTF8String, reinterpret(UInt32, dat))

function convert(::Type{UTF16String}, str::ASCIIString)
    dat = str.data
    fast_utf_copy(UInt16, length(dat)+1, dat)
end

function convert(::Type{UTF32String}, str::ASCIIString)
    dat = str.data
    fast_utf_copy(Char, length(dat)+1, dat)
end

convert(::Type{UTF16String}, str::UTF16String)    = str
convert(::Type{UTF16String}, dat::Vector{Char})   = convert(UTF16String, reinterpret(UInt32, dat))

convert(::Type{Vector{UInt16}}, str::UTF16String) = str.data
convert(::Type{Array{UInt16}},  str::UTF16String) = str.data

convert(::Type{UTF32String}, str::UTF32String)    = str

convert(::Type{UTF32String}, c::Char)             = UTF32String(Char[c, Char(0)])
