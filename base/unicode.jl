# This file is a part of Julia. License is MIT: http://julialang.org/license

module Unicode
import Base: string, convert, write, length, endof, next, reverseind, lastidx, reverse, isvalid,
	     sizeof, unsafe_convert, map, getindex, search, rsearch, pointer, containsnul,
	     lowercase, uppercase, eltype, isless, promote_rule, ==

export UnicodeError, UTF16String, UTF32String, unsafe_checkstring, checkstring,
       utf8, utf16, utf32, containsnul, WString, wstring, charprop, Cat, CharType, CharCode,
       is_assigned_char, islower, isupper, isdigit, isalpha, isnumber, isalnum, iscntrl,
       ispunct, isspace, isprint, isgraph,
       isgraphemebreak, GraphemeIterator, normalize_string, graphemes, charwidth

include("unicode/UnicodeError.jl")
include("unicode/types.jl")
include("unicode/checkstring.jl")
include("unicode/utf8.jl")
include("unicode/utf16.jl")
include("unicode/utf32.jl")
include("unicode/properties.jl")
include("unicode/utf8proc.jl")
end
importall .Unicode
