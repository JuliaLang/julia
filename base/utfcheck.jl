# This file is a part of Julia. License is MIT: http://julialang.org/license

# Functions to check validity of UTF-8, UTF-16, and UTF-32 encoded strings,
# and also to return information necessary to convert to other encodings

is_surrogate_lead(c::Unsigned) = ((c & ~0x003ff) == 0xd800)
is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
is_surrogate_codepoint(c::Unsigned) = ((c & ~0x007ff) == 0xd800)
is_valid_continuation(c) = ((c & 0xc0) == 0x80)

# Options for check_string_* functions

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
