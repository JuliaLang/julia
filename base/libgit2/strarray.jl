# This file is a part of Julia. License is MIT: http://julialang.org/license

function StrArrayStruct{T<:AbstractString}(strs::T...)
    strcount = length(strs)
    for s in strs
        if Base.containsnul(s)
            throw("embedded NULs are not allowed in C strings: $(repr(s))")
        end
    end
    sa_strings = convert(Ptr{Cstring}, Libc.malloc(sizeof(Cstring) * strcount))
    for i=1:strcount
        len = length(strs[i])
        in_ptr  = pointer(String(strs[i]))
        out_ptr = convert(Ptr{UInt8}, Libc.malloc(sizeof(UInt8) * (len + 1)))
        unsafe_copy!(out_ptr, in_ptr, len)
        unsafe_store!(out_ptr, zero(UInt8), len + 1) # NULL byte
        unsafe_store!(sa_strings, convert(Cstring, out_ptr), i)
    end
    return StrArrayStruct(sa_strings, strcount)
end
StrArrayStruct{T<:AbstractString}(strs::Vector{T}) = StrArrayStruct(strs...)

function Base.convert(::Type{Vector{AbstractString}}, sa::StrArrayStruct)
    arr = Array{AbstractString}(sa.count)
    for i=1:sa.count
        arr[i] = unsafe_string(unsafe_load(sa.strings, i))
    end
    return arr
end

function Base.copy(src::StrArrayStruct)
    dst_ptr = Ref(StrArrayStruct())
    @check ccall((:git_strarray_copy, :libgit2), Cint,
                  (Ptr{StrArrayStruct}, Ptr{StrArrayStruct}),
                   dst_ptr, Ref(src))
    return dst_ptr[]
end
