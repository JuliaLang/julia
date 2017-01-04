# This file is a part of Julia. License is MIT: http://julialang.org/license

##    Error messages for Unicode / UTF support

const UTF_ERR_SHORT             = "invalid UTF-8 sequence starting at index <<1>> (0x<<2>> missing one or more continuation bytes)"
const UTF_ERR_INVALID_INDEX     = "invalid character index"

type UnicodeError <: Exception
    errmsg::AbstractString      ##< A UTF_ERR_ message
    errpos::Int32               ##< Position of invalid character
    errchr::UInt32              ##< Invalid character
end

show(io::IO, exc::UnicodeError) = print(io, replace(replace(string("UnicodeError: ",exc.errmsg),
    "<<1>>",string(exc.errpos)),"<<2>>",hex(exc.errchr)))
