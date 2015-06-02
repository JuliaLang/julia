# This file is a part of Julia. License is MIT: http://julialang.org/license

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

isvalid{T<:Union(ASCIIString,UTF8String,UTF16String,UTF32String)}(str::T) = isvalid(T, str.data)
isvalid{T<:Union(ASCIIString,UTF8String,UTF16String,UTF32String)}(::Type{T}, str::T) = isvalid(T, str.data)
