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

isvalid{T<:Union{String,UTF16String}}(str::T) = isvalid(T, str.data)
isvalid{T<:Union{String,UTF16String}}(::Type{T}, str::T) = isvalid(T, str.data)
