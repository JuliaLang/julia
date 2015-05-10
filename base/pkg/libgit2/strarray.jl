function StrArrayStruct{T<:AbstractString}(strs::T...)
    count = length(strs)
    strings = convert(Ptr{Ptr{Uint8}}, Libc.malloc(sizeof(Ptr{Uint8}) * count))
    for i=1:count
        len = length(strs[i])
        #in_ptr  = convert(Ptr{Uint8}, bytestring(strs[i]))
        in_ptr  = pointer(bytestring(strs[i]))
        out_ptr = convert(Ptr{Uint8}, Libc.malloc(sizeof(Uint8) * (len + 1)))
        unsafe_copy!(out_ptr, in_ptr, len)
        unsafe_store!(out_ptr, zero(Uint8), len + 1) # NULL byte
        unsafe_store!(strings, out_ptr, i)
    end
    return StrArrayStruct(strings, count)
end
StrArrayStruct{T<:AbstractString}(strs::Vector{T}) = StrArrayStruct(strs...)

function Base.convert(::Type{Vector{AbstractString}}, sa::StrArrayStruct)
    arr = Array(AbstractString, sa.count)
    for i=1:sa.count
        arr[i] = bytestring(unsafe_load(sa.strings, i))
    end
    return arr
end

function finalize(sa::StrArrayStruct)
    sa_ptr = Ref(sa)
    ccall((:git_strarray_free, :libgit2), Void, (Ptr{StrArrayStruct},), sa_ptr)
    return sa_ptr[]
end