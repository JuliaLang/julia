# This file is a part of Julia. License is MIT: http://julialang.org/license

function StrArrayStruct{T<:AbstractString}(strs::T...)
    count = length(strs)
    strings = convert(Ptr{Ptr{UInt8}}, Libc.malloc(sizeof(Ptr{UInt8}) * count))
    for i=1:count
        len = length(strs[i])
        #in_ptr  = convert(Ptr{Uint8}, bytestring(strs[i]))
        in_ptr  = unsafe_convert(Cstring, strs[i])
        out_ptr = convert(Ptr{UInt8}, Libc.malloc(sizeof(UInt8) * (len + 1)))
        unsafe_copy!(out_ptr, in_ptr, len)
        unsafe_store!(out_ptr, zero(UInt8), len + 1) # NULL byte
        unsafe_store!(strings, out_ptr, i)
    end
    return StrArrayStruct(strings, count)
end
StrArrayStruct{T<:AbstractString}(strs::Vector{T}) = StrArrayStruct(strs...)

function StrArrayStruct2{T<:AbstractString}(strs::T...)
    count = length(strs)
    strings = convert(Ptr{Cstring}, Libc.malloc(sizeof(Ptr{UInt8}) * count))
    for i=1:count
        len = length(strs[i])
        in_ptr  = unsafe_convert(Cstring, strs[i])
        out_ptr = convert(Cstring, Libc.malloc(sizeof(UInt8) * (len + 1)))
        unsafe_copy!(out_ptr, in_ptr, len)
        unsafe_store!(out_ptr, zero(UInt8), len + 1) # NULL byte
        unsafe_store!(strings, out_ptr, i)
    end
    return StrArrayStruct(strings, count)
end

function Base.convert(::Type{Vector{AbstractString}}, sa::StrArrayStruct)
    arr = Array(AbstractString, sa.count)
    for i=1:sa.count
        arr[i] = bytestring(unsafe_load(sa.strings, i))
    end
    return arr
end
