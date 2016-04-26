# This file is a part of Julia. License is MIT: http://julialang.org/license

##\brief      Base UTF16String type, has 16-bit NULL termination word after data, native byte order
#
# \throws     UnicodeError

immutable UTF16String <: AbstractString
    data::Vector{UInt16} # includes 16-bit NULL termination after string chars
    function UTF16String(data::Vector{UInt16})
        if length(data) < 1 || data[end] != 0
            throw(UnicodeError(UTF_ERR_NULL_16_TERMINATE, 0, 0))
        end
        new(data)
    end
end

##\brief      Base UTF32String type, has 32-bit NULL termination word after data, native byte order
#
# \throws     UnicodeError

immutable UTF32String <: DirectIndexString
    data::Vector{UInt32} # includes 32-bit NULL termination after string chars

    function UTF32String(data::Vector{UInt32})
        if length(data) < 1 || data[end] != 0
            throw(UnicodeError(UTF_ERR_NULL_32_TERMINATE, 0, 0))
        end
        new(data)
    end
end
UTF32String(data::Vector{Char}) = UTF32String(reinterpret(UInt32, data))

isvalid{T<:Union{String,UTF16String,UTF32String}}(str::T) = isvalid(T, str.data)
isvalid{T<:Union{String,UTF16String,UTF32String}}(::Type{T}, str::T) = isvalid(T, str.data)
