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
