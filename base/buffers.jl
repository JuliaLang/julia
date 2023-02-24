
const BufferType{T} = Union{ImmutableBuffer{T}, MutableBuffer{T}}

# MutableBuffer
if nameof(@__MODULE__) === :Base  # avoid method overwrite
function MutableBuffer{T}(::UndefInitializer, n::Int) where {T}
    ccall(:jl_new_buffer, Any, (Any, UInt), MutableBuffer{T}, n)
end
MutableBuffer(a::AbstractArray{T}) where {T} = MutableBuffer{T}(a)
function MutableBuffer{T}(a::AbstractArray) where {T}
    n = length(a)
    b = MutableBuffer{T}(undef, n)
    i = 1
    for a_i in a
        @inbounds b[i] = a_i
        i += 1
    end
    return b
end

MutableBuffer(b::ImmutableBuffer) = Core.bufthaw(b)

ImmutableBuffer(b::MutableBuffer) = Core.buffreeze(b)

end

@eval getindex(b::MutableBuffer{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), b, i)
@eval getindex(b::ImmutableBuffer{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), b, i)
@eval setindex!(b::MutableBuffer{T}, x, i::Int) where {T} = Core.bufset($(Expr(:boundscheck)), b, convert(T, x)::T, i)
eltype(::Type{<:BufferType{T}}) where {T} = T
elsize(@nospecialize T::Type{<:BufferType}) = aligned_sizeof(eltype(T))
length(b::BufferType) = Core.buflen(b)
firstindex(b::BufferType) = 1
lastindex(b::BufferType) = length(b)
keys(b::BufferType) = OneTo(length(b))
axes(b::BufferType) = (OneTo(length(b)),)
axes(b::BufferType, d::Integer) = d <= 1 ? OneTo(length(b)) : OneTo(1)
iterate(v::BufferType, i=1) = (length(v) < i ? nothing : (v[i], i + 1))
isempty(b::BufferType) = (length(b) == 0)
function ==(v1::BufferType, v2::BufferType)
    length(v1)==length(v2) || return false
    for i in 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

unsafe_freeze!(b::MutableBuffer) = Core.mutating_buffreeze(b)
