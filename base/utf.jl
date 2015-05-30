# This file is a part of Julia. License is MIT: http://julialang.org/license

#=
@doc """
@brief      Error messages for Unicode / UTF support
""" ->
=#

const UTF_ERR_SHORT = 1
const UTF_ERR_CONT  = 2
const UTF_ERR_LONG  = 3
const UTF_ERR_NOT_LEAD = 4
const UTF_ERR_NOT_TRAIL = 5
const UTF_ERR_NOT_SURROGATE = 6
const UTF_ERR_MISSING_SURROGATE = 7
const UTF_ERR_INVALID = 8
const UTF_ERR_SURROGATE = 9
const UTF_ERR_NULL_16_TERMINATE = 10
const UTF_ERR_NULL_32_TERMINATE = 11
const UTF_ERR_MAX = 11

const errMsgs = [
    "invalid UTF-8 sequence starting at index <<1>> (0x<<2>>) missing one or more continuation bytes)",
    "invalid UTF-8 sequence starting at index <<1>> (0x<<2>> is not a continuation byte)",
    "invalid UTF-8 sequence, overlong encoding starting at index <<1>> (0x<<2>>)",
    "not a leading Unicode surrogate codepoint at index <<1>> (0x<<2>>)",
    "not a trailing Unicode surrogate codepoint at index <<1>> (0x<<2>>)",
    "not a valid Unicode surrogate codepoint at index <<1>> (0x<<2>>",
    "missing trailing Unicode surrogate codepoint after index <<1>> (0x<<2>>)",
    "invalid Unicode character starting at index <<1>> (0x<<2>> > 0x10ffff)",
    "surrogate encoding not allowed in UTF-8 or UTF-32, at index <<1>> (0x<<2>>)",
    "UTF16String data must be NULL-terminated",
    "UTF32String data must be NULL-terminated"
]
#=
@doc """
@brief      Throws ArgumentError with information about the specific error, location, and character

@param[in]  errcode Error code for Unicode error (one of UTF_ERR_*)
@param[in]  charpos Index of invalid byte or character
@param[in]  invchar Invalid byte or character

@throws never returns, always throws ArgumentError
""" ->
=#
function utf_errfunc(errcode::Integer, charpos, invchar)
    if errcode < 1 || errcode > UTF_ERR_MAX
        throw(ArgumentError("Invalid error code for Unicode error: $errcode, Pos = $charpos, Char = $invchar"))
    end
    throw(ArgumentError(replace(replace(errMsgs[errcode],"<<1>>",string(charpos)),"<<2>>",hex(invchar))))
end

#=
@doc """
@brief      Base UTF16String type, has 16-bit NULL termination word after data, native byte order
""" ->
=#
immutable UTF16String <: AbstractString
    data::Vector{UInt16} # includes 16-bit NULL termination after string chars
    function UTF16String(data::Vector{UInt16})
        if length(data) < 1 || data[end] != 0
            utf_errfunc(UTF_ERR_NULL_16_TERMINATE, 0, 0)
        end
        new(data)
    end
end

#=
@doc """
@brief      Base UTF32String type, has 32-bit NULL termination word after data, native byte order
""" ->
=#
immutable UTF32String <: DirectIndexString
    data::Vector{Char} # includes 32-bit NULL termination after string chars

    function UTF32String(data::Vector{Char})
        if length(data) < 1 || data[end] != Char(0)
            utf_errfunc(UTF_ERR_NULL_32_TERMINATE, 0, 0)
        end
        new(data)
    end
end
UTF32String(data::Vector{UInt32}) = UTF32String(reinterpret(Char, data))

const empty_utf16 = UTF16String(UInt16[0])
const empty_utf32 = UTF32String(UInt32[0])

is_surrogate_lead(c::Unsigned) = ((c & ~0x003ff) == 0xd800)
is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
is_surrogate_codepoint(c::Unsigned) = ((c & ~0x007ff) == 0xd800)
is_valid_continuation(c) = ((c & 0xc0) == 0x80)

function length(s::UTF16String)
    d = s.data
    len = length(d) - 1
    len == 0 && return 0
    cnum = 0
    for i = 1:len
        @inbounds cnum += !is_surrogate_trail(d[i])
    end
    cnum
end

function endof(s::UTF16String)
    d = s.data
    i = length(d) - 1
    i == 0 && return i
    return is_surrogate_codepoint(d[i]) ? i-1 : i
end

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function next(s::UTF16String, i::Int)
    ch = s.data[i]
    !is_surrogate_codepoint(ch) && return (Char(ch), i+1)
    # check length, account for terminating \0
    i >= (length(s.data)-1) && utf_errfunc(UTF_ERR_MISSING_SURROGATE, i, UInt32(ch))
    !is_surrogate_lead(ch) && utf_errfunc(UTF_ERR_NOT_LEAD, i, ch)
    ct = s.data[i+1]
    !is_surrogate_trail(ct) && utf_errfunc(UTF_ERR_NOT_TRAIL, i, ch)
    Char(get_supplementary(ch, ct)), i+2
end

function reverseind(s::UTF16String, i::Integer)
    j = length(s.data) - i
    return is_surrogate_trail(s.data[j]) ? j-1 : j
end

lastidx(s::UTF16String) = length(s.data) - 1 # s.data includes NULL terminator

function reverse(s::UTF16String)
    d = s.data
    out = similar(d)
    out[end] = 0 # NULL termination
    n = length(d)
    @inbounds for i = 1:n-1
        ch = d[n-i]
        if is_surrogate_lead(ch)
            out[i],out[i-1] = out[i-1],ch
        else
            out[i] = ch
        end
    end
    UTF16String(out)
end

next(s::UTF32String, i::Int) = (s.data[i], i+1)
endof(s::UTF32String) = length(s.data) - 1
length(s::UTF32String) = length(s.data) - 1

const UTF_NO_LONG_NULL = 1      # don't accept 0xc0 0x80 for '\0'
const UTF_NO_SURROGATES = 2     # don't accept surrogate pairs in UTF-8/UTF-32
const UTF_ACCEPT_LONG = 4       # accept long encodings (other than long null in UTF-8)

const UTF_LONG = 1              # Long encodings are present
const UTF_LATIN1 = 2            # characters in range 0x80-0xFF present
const UTF_UNICODE2 = 4          # characters in range 0x100-0x7ff present
const UTF_UNICODE3 = 8          # characters in range 0x800-0xd7ff, 0xe000-0xffff
const UTF_UNICODE4 = 16         # non-BMP characters present
const UTF_SURROGATE = 32        # surrogate pairs present

# Get a UTF-8 continuation byte, give error if invalid, and update position and character value
@inline function get_continuation(ch::UInt32, byt::UInt8, pos)
    !is_valid_continuation(byt) && utf_errfunc(UTF_ERR_CONT, pos, byt)
    (ch << 6) | (byt & 0x3f)
end

#=
@doc """
@brief      Validates and calculates number of characters in a UTF-8 encoded vector of UInt8

@param[in]  str     Vector of UInt8
@param[in]  options flags to determine error handling (default 0)

@return     (total characters, flags, 4-byte, 3-byte, 2-byte)
@throws     ArgumentError
""" ->
=#
function check_string_utf8(dat::Vector{UInt8}, options::Integer=0)
    local byt::UInt8, ch::UInt32, surr::UInt32
    flags::UInt = 0
    totalchar = num2byte = num3byte = num4byte = 0
    pos = 0
    len = sizeof(dat)
    @inbounds while pos < len
        ch = dat[pos += 1]
        totalchar += 1
        if ch > 0x7f
            # Check UTF-8 encoding
            if ch < 0xe0
                # 2-byte UTF-8 sequence (i.e. characters 0x80-0x7ff)
                (pos == len) && utf_errfunc(UTF_ERR_SHORT, pos, ch)
                ch = get_continuation(ch & 0x3f, dat[pos += 1], pos)
                if ch > 0x7f
                    num2byte += 1
                    flags |= (ch > 0xff) ? UTF_UNICODE2 : UTF_LATIN1
                elseif (options & UTF_ACCEPT_LONG) != 0
                    flags |= UTF_LONG
                elseif (ch == 0) && ((options & UTF_NO_LONG_NULL) == 0)
                    flags |= UTF_LONG
                else
                    utf_errfunc(UTF_ERR_LONG, pos, ch)
                end
             elseif ch < 0xf0
                # 3-byte UTF-8 sequence (i.e. characters 0x800-0xffff)
                (pos + 2 > len) && utf_errfunc(UTF_ERR_SHORT, pos, ch)
                ch = get_continuation(ch & 0x0f, dat[pos += 1], pos)
                ch = get_continuation(ch, dat[pos += 1], pos)
                # check for surrogate pairs, make sure correct
                if is_surrogate_codepoint(ch)
                    !is_surrogate_lead(ch) && utf_errfunc(UTF_ERR_NOT_LEAD, pos-2, ch)
                    # next character *must* be a trailing surrogate character
                    (pos + 3 > len) && utf_errfunc(UTF_ERR_MISSING_SURROGATE, pos-2, ch)
                    byt = dat[pos += 1]
                    (byt != 0xed) && utf_errfunc(UTF_ERR_NOT_TRAIL, pos, byt)
                    surr = get_continuation(0x0000d, dat[pos += 1], pos)
                    surr = get_continuation(surr, dat[pos += 1], pos)
                    !is_surrogate_trail(surr) && utf_errfunc(UTF_ERR_NOT_TRAIL, pos-2, surr)
                    (options & UTF_NO_SURROGATES) != 0 && utf_errfunc(UTF_ERR_SURROGATE, pos-2, surr)
                    flags |= UTF_SURROGATE
                    num4byte += 1
                elseif ch > 0x07ff
                    num3byte += 1
                elseif (options & UTF_ACCEPT_LONG) != 0
                    flags |= UTF_LONG
                    num2byte += 1
                else
                    utf_errfunc(UTF_ERR_LONG, pos-2, ch)
                end
            elseif ch < 0xf5
                # 4-byte UTF-8 sequence (i.e. characters > 0xffff)
                (pos + 3 > len) && utf_errfunc(UTF_ERR_SHORT, pos, ch)
                ch = get_continuation(ch & 0x07, dat[pos += 1], pos)
                ch = get_continuation(ch, dat[pos += 1], pos)
                ch = get_continuation(ch, dat[pos += 1], pos)
                if ch > 0x10ffff
                    utf_errfunc(UTF_ERR_INVALID, pos-3, ch)
                elseif ch > 0xffff
                    num4byte += 1
                elseif is_surrogate_codepoint(ch)
                    utf_errfunc(UTF_ERR_SURROGATE, pos-3, ch)
                elseif (options & UTF_ACCEPT_LONG) != 0
                    # This is an overly long encode character
                    flags |= UTF_LONG
                    if ch > 0x7ff
                        num3byte += 1
                    elseif ch > 0x7f
                        num2byte += 1
                    end
                else
                    utf_errfunc(UTF_ERR_LONG, pos-2, ch)
                end
            else
                utf_errfunc(UTF_ERR_INVALID, pos, ch)
            end
        end
    end
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    return totalchar, flags, num4byte, num3byte, num2byte
end

#=
@doc """
@brief      Validates and calculates number of characters in a UTF-16 encoded vector of UInt16

@param[in]  dat     Vector{UInt16}
@param[in]  options flags to determine error handling (default 0)

@return     (total characters, flags, 4-byte, 3-byte, 2-byte)
@throws     ArgumentError
""" ->
=#
function check_string_utf16(dat::Vector{UInt16}, len::Int)
    local ch::UInt32
    flags::UInt = 0
    totalchar = num2byte = num3byte = num4byte = 0
    pos = 0
    @inbounds while pos < len
        ch = dat[pos += 1]
        totalchar += 1
        if ch > 0x7f
            if ch < 0x100
                num2byte += 1
                flags |= UTF_LATIN1
            elseif ch < 0x800
                num2byte += 1
                flags |= UTF_UNICODE2
            elseif !is_surrogate_codepoint(ch)
                num3byte += 1
            elseif is_surrogate_lead(ch)
                pos == len && utf_errfunc(UTF_ERR_MISSING_SURROGATE, pos, ch)
                # next character *must* be a trailing surrogate character
                ch = dat[pos += 1]
                !is_surrogate_trail(ch) && utf_errfunc(UTF_ERR_NOT_TRAIL, pos, ch)
                num4byte += 1
            else
                utf_errfunc(UTF_ERR_NOT_LEAD, pos, ch)
            end
        end
    end
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    return totalchar, flags, num4byte, num3byte, num2byte
end

#=
@doc """
@brief      Validates and calculates number of characters in a UTF-32 encoded vector of UInt32

@param[in]  dat     Vector{UInt32}
@param[in]  options flags to determine error handling (default 0)

@return     (total characters, flags, 4-byte, 3-byte, 2-byte)
@throws     ArgumentError
""" ->
=#
function check_string_utf32(dat::Vector{UInt32}, len::Int, options::Integer=0)
    local ch::UInt32
    flags::UInt = 0
    totalchar = num2byte = num3byte = num4byte = 0
    pos = 0
    @inbounds while pos < len
        ch = dat[pos += 1]
        totalchar += 1
        if ch > 0x7f
            if ch < 0x100
                num2byte += 1
                flags |= UTF_LATIN1
            elseif ch < 0x800
                num2byte += 1
                flags |= UTF_UNICODE2
            elseif ch > 0xffff
                (ch > 0x10ffff) && utf_errfunc(UTF_ERR_INVALID, pos, ch)
                num4byte += 1
            elseif !is_surrogate_codepoint(ch)
                num3byte += 1
            elseif is_surrogate_lead(ch)
                pos == len && utf_errfunc(UTF_ERR_MISSING_SURROGATE, pos, ch)
                # next character *must* be a trailing surrogate character
                ch = dat[pos += 1]
                !is_surrogate_trail(ch) && utf_errfunc(UTF_ERR_NOT_TRAIL, pos, ch)
                num4byte += 1
                (options & UTF_NO_SURROGATES) != 0 && utf_errfunc(UTF_ERR_SURROGATE, pos, ch)
                flags |= UTF_SURROGATE
            else
                utf_errfunc(UTF_ERR_NOT_LEAD, pos, ch)
            end
        end
    end
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    return totalchar, flags, num4byte, num3byte, num2byte
end

function check_string_abs(str::AbstractString, options::Integer=0)
    local ch::UInt32
    flags::UInt = 0
    totalchar = num2byte = num3byte = num4byte = 0
    pos = start(str)
    len = endof(str)
    @inbounds while pos < len
        ch, pos = next(str, pos)
        totalchar += 1
        if ch > 0x7f
            if ch < 0x100
                num2byte += 1
                flags |= UTF_LATIN1
            elseif ch < 0x800
                num2byte += 1
                flags |= UTF_UNICODE2
            elseif ch > 0xffff
                (ch > 0x10ffff) && utf_errfunc(UTF_ERR_INVALID, pos, ch)
                num4byte += 1
            elseif !is_surrogate_codepoint(ch)
                num3byte += 1
            elseif is_surrogate_lead(ch)
                pos == len && utf_errfunc(UTF_ERR_MISSING_SURROGATE, pos, ch)
                # next character *must* be a trailing surrogate character
                ch, pos = next(str, pos)
                !is_surrogate_trail(ch) && utf_errfunc(UTF_ERR_NOT_TRAIL, pos, ch)
                num4byte += 1
                (options & UTF_NO_SURROGATES) != 0 && utf_errfunc(UTF_ERR_SURROGATE, pos, ch)
                flags |= UTF_SURROGATE
            else
                utf_errfunc(UTF_ERR_NOT_LEAD, pos, ch)
            end
        end
    end
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    return totalchar, flags, num4byte, num3byte, num2byte
end

# Quickly copy and set trailing \0
@inline function fast_utf_copy(T::Type{UInt16}, len, dat)
    @inbounds return UTF16String(setindex!(copy!(Vector{T}(len), dat), 0, len))
end
@inline function fast_utf_copy(T::Type{Char}, len, dat)
    @inbounds return UTF32String(setindex!(copy!(Vector{T}(len), dat), 0, len))
end

# Get rest of character ch from 3-byte UTF-8 sequence in str
@inline function get_utf8_3(dat, pos, ch)
    @inbounds return ((ch & 0xf) << 12) | (UInt32(dat[pos-1] & 0x3f) << 6) | (dat[pos] & 0x3f)
end

# Get rest of character ch from 4-byte UTF-8 sequence in dat, update pos and return character
@inline function get_utf8_4(dat, pos, ch)
    @inbounds return (((ch & 0x7) << 18)
                        | (UInt32(dat[pos-2] & 0x3f) << 12)
                        | (UInt32(dat[pos-1] & 0x3f) << 6)
                        | (dat[pos] & 0x3f))
end

# Output a character as a 4-byte UTF-8 sequence, update the position
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
        elseif is_surrogate_codepoint(ch)
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

utf16(x) = convert(UTF16String, x)

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

utf32(x) = convert(UTF32String, x)

convert(::Type{UTF32String}, str::UTF32String)    = str

convert(::Type{UTF32String}, c::Char)             = UTF32String(Char[c, Char(0)])

sizeof(s::UTF16String) = sizeof(s.data) - sizeof(UInt16)
unsafe_convert{T<:Union(Int16,UInt16)}(::Type{Ptr{T}}, s::UTF16String) =
    convert(Ptr{T}, pointer(s))

function isvalid(::Type{UTF16String}, data::AbstractArray{UInt16})
    i = 1
    n = length(data) # this may include NULL termination; that's okay
    @inbounds while i < n # check for unpaired surrogates
        if is_surrogate_lead(data[i]) && is_surrogate_trail(data[i+1])
            i += 2
        elseif is_surrogate_codepoint(data[i])
            return false
        else
            i += 1
        end
    end
    return i > n || !is_surrogate_codepoint(data[i])
end

function convert(::Type{UTF16String}, data::AbstractVector{UInt16})
    !isvalid(UTF16String, data) && throw(ArgumentError("invalid UTF16 data"))
    len = length(data)
    @inbounds return UTF16String(setindex!(copy!(Vector{UInt16}(len+1),1,data,1,len),0,len+1))
end

convert(T::Type{UTF16String}, data::AbstractArray{UInt16}) =
    convert(T, reshape(data, length(data)))

convert(T::Type{UTF16String}, data::AbstractArray{Int16}) =
    convert(T, reinterpret(UInt16, data))

function convert(T::Type{UTF16String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return UTF16String(UInt16[0])
    isodd(length(bytes)) && throw(ArgumentError("odd number of bytes"))
    data = reinterpret(UInt16, bytes)
    # check for byte-order mark (BOM):
    if data[1] == 0xfeff        # native byte order
        d = Array(UInt16, length(data))
        copy!(d,1, data,2, length(data)-1)
    elseif data[1] == 0xfffe    # byte-swapped
        d = Array(UInt16, length(data))
        for i = 2:length(data)
            d[i-1] = bswap(data[i])
        end
    else
        d = Array(UInt16, length(data) + 1)
        copy!(d,1, data,1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    !isvalid(UTF16String, d) && throw(ArgumentError("invalid UTF16 data"))
    UTF16String(d)
end

utf16(p::Ptr{UInt16}, len::Integer) = utf16(pointer_to_array(p, len))
utf16(p::Ptr{Int16}, len::Integer) = utf16(convert(Ptr{UInt16}, p), len)
function utf16(p::Union(Ptr{UInt16}, Ptr{Int16}))
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf16(p, len)
end

function convert(::Type{UTF32String}, data::AbstractVector{Char})
    len = length(data)
    @inbounds return UTF32String(setindex!(copy!(Vector{Char}(len+1),1,data,1,len),0,len+1))
end

convert{T<:Union(Int32,UInt32)}(::Type{UTF32String}, data::AbstractVector{T}) =
    convert(UTF32String, reinterpret(Char, data))

convert{T<:AbstractString}(::Type{T}, v::AbstractVector{Char}) = convert(T, utf32(v))

# specialize for performance reasons:
function convert{T<:ByteString}(::Type{T}, data::AbstractVector{Char})
    s = IOBuffer(Array(UInt8,length(data)), true, true)
    truncate(s,0)
    for x in data
        print(s, x)
    end
    convert(T, takebuf_string(s))
end

convert(::Type{Vector{Char}}, str::UTF32String) = str.data
convert(::Type{Array{Char}},  str::UTF32String) = str.data

reverse(s::UTF32String) = UTF32String(reverse!(copy(s.data), 1, length(s)))

sizeof(s::UTF32String) = sizeof(s.data) - sizeof(Char)
unsafe_convert{T<:Union(Int32,UInt32,Char)}(::Type{Ptr{T}}, s::UTF32String) =
    convert(Ptr{T}, pointer(s))

function convert(T::Type{UTF32String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return UTF32String(Char[0])
    length(bytes) & 3 != 0 && throw(ArgumentError("need multiple of 4 bytes"))
    data = reinterpret(Char, bytes)
    # check for byte-order mark (BOM):
    if data[1] == Char(0x0000feff) # native byte order
        d = Array(Char, length(data))
        copy!(d,1, data, 2, length(data)-1)
    elseif data[1] == Char(0xfffe0000) # byte-swapped
        d = Array(Char, length(data))
        @inbounds for i = 2:length(data) ; d[i-1] = bswap(data[i]) ; end
    else
        d = Array(Char, length(data) + 1)
        copy!(d, 1, data, 1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    UTF32String(d)
end

function isvalid(::Type{UTF32String}, str::Union(Vector{Char}, Vector{UInt32}))
    for i=1:length(str)
        @inbounds if !isvalid(Char, UInt32(str[i])) ; return false ; end
    end
    return true
end
isvalid(str::Vector{Char}) = isvalid(UTF32String, str)
isvalid{T<:Union(ASCIIString,UTF8String,UTF16String,UTF32String)}(str::T) = isvalid(T, str.data)
isvalid{T<:Union(ASCIIString,UTF8String,UTF16String,UTF32String)}(::Type{T}, str::T) = isvalid(T, str.data)

utf32(p::Ptr{Char}, len::Integer) = utf32(pointer_to_array(p, len))
utf32(p::Union(Ptr{UInt32}, Ptr{Int32}), len::Integer) = utf32(convert(Ptr{Char}, p), len)
function utf32(p::Union(Ptr{Char}, Ptr{UInt32}, Ptr{Int32}))
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf32(p, len)
end

function map(f, s::UTF32String)
    d = s.data
    out = similar(d)
    out[end] = 0

    @inbounds for i = 1:(length(d)-1)
        c2 = f(d[i])
        if !isa(c2, Char)
            throw(ArgumentError("map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"))
        end
        out[i] = (c2::Char)
    end
    UTF32String(out)
end
