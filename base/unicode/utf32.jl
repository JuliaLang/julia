# This file is a part of Julia. License is MIT: http://julialang.org/license

# specialize for performance reasons:
function convert{T<:Union{UInt32,Char,Int32}}(::Type{String}, data::AbstractVector{T})
    s = IOBuffer(Array(UInt8,length(data)), true, true)
    truncate(s,0)
    for x in data
        print(s, Char(x))
    end
    convert(String, takebuf_string(s))
end

# Definitions for C compatible strings, that don't allow embedded
# '\0', and which are terminated by a '\0'

containsnul(s::AbstractString) = '\0' in s
containsnul(s::String) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))

# pointer conversions of ASCII/UTF8/UTF16/UTF32 strings:
pointer(x::String) = pointer(x.data)
pointer(x::String, i::Integer) = pointer(x.data)+(i-1)

# pointer conversions of SubString of ASCII/UTF8/UTF16/UTF32:
pointer(x::SubString{String}) = pointer(x.string.data) + x.offset
pointer(x::SubString{String}, i::Integer) = pointer(x.string.data) + x.offset + (i-1)
