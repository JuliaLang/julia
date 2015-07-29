# This file is a part of Julia. License is MIT: http://julialang.org/license

##    Error messages for Unicode / UTF support

const UTF_ERR_SHORT             = "invalid UTF-8 sequence starting at index <<1>> (0x<<2>>) missing one or more continuation bytes)"
const UTF_ERR_CONT              = "invalid UTF-8 sequence starting at index <<1>> (0x<<2>> is not a continuation byte)"
const UTF_ERR_LONG              = "invalid UTF-8 sequence, overlong encoding starting at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_LEAD          = "not a leading Unicode surrogate code unit at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_TRAIL         = "not a trailing Unicode surrogate code unit at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_SURROGATE     = "not a valid Unicode surrogate code unit at index <<1>> (0x<<2>>"
const UTF_ERR_MISSING_SURROGATE = "missing trailing Unicode surrogate code unit after index <<1>> (0x<<2>>)"
const UTF_ERR_INVALID           = "invalid Unicode character starting at index <<1>> (0x<<2>> > 0x10ffff)"
const UTF_ERR_SURROGATE         = "surrogate encoding not allowed in UTF-8 or UTF-32, at index <<1>> (0x<<2>>)"
const UTF_ERR_NULL_16_TERMINATE = "UTF16String data must be NULL-terminated"
const UTF_ERR_NULL_32_TERMINATE = "UTF32String data must be NULL-terminated"
const UTF_ERR_ODD_BYTES_16      = "UTF16String can't have odd number of bytes <<1>>"
const UTF_ERR_ODD_BYTES_32      = "UTF32String must have multiple of 4 bytes <<1>>"
const UTF_ERR_INVALID_CHAR      = "invalid Unicode character (0x<<2>> > 0x10ffff)"
const UTF_ERR_INVALID_8         = "invalid UTF-8 data"
const UTF_ERR_INVALID_16        = "invalid UTF-16 data"
const UTF_ERR_INVALID_INDEX     = "invalid character index"
const UTF_ERR_MAP_CHAR          = "map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"

type UnicodeError <: Exception
    errmsg::AbstractString      ##< A UTF_ERR_ message
    errpos::Int32               ##< Position of invalid character
    errchr::UInt32              ##< Invalid character
end

show(io::IO, exc::UnicodeError) = print(io, replace(replace(string("UnicodeError: ",exc.errmsg),
    "<<1>>",string(exc.errpos)),"<<2>>",hex(exc.errchr)))
