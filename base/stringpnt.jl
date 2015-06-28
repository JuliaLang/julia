# This file is a part of Julia. License is MIT: http://julialang.org/license

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
wstring(s::Cwstring) = wstring(box(Ptr{Cwchar_t}, unbox(Cwstring,s)))

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
pointer{T<:ByteString}(x::SubString{T}) = pointer(x.string.data) + x.offset
pointer(x::ByteString, i::Integer) = pointer(x.data)+(i-1)
pointer{T<:ByteString}(x::SubString{T}, i::Integer) = pointer(x.string.data) + x.offset + (i-1)
pointer(x::Union{UTF16String,UTF32String}, i::Integer) = pointer(x)+(i-1)*sizeof(eltype(x.data))
pointer{T<:Union{UTF16String,UTF32String}}(x::SubString{T}) = pointer(x.string.data) + x.offset*sizeof(eltype(x.data))
pointer{T<:Union{UTF16String,UTF32String}}(x::SubString{T}, i::Integer) = pointer(x.string.data) + (x.offset + (i-1))*sizeof(eltype(x.data))
