
const BufferType{T, AA} = Union{ImmutableBuffer{T, AA}, MutableBuffer{T, AA}}

# MutableBuffer
if nameof(@__MODULE__) === :Base  # avoid method overwrite
function MutableBuffer{T}(::UndefInitializer, n::Int) where {T}
    ccall(:jl_new_buffer, Any, (Any, UInt), T, n)
end
MutableBuffer(a::AbstractArray{T}) where {T} = MutableBuffer{T}(a)
function MutableBuffer{T}(a::AbstractArray) where {T}
    n = length(a)
    sb = MutableBuffer{T}(undef, n)
    i = 1
    for a_i in a
        @inbounds sb[i] = a_i
        i += 1
    end
    return sb
end
end

@eval getindex(sb::MutableBuffer{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), sb, i)
@eval setindex!(sb::MutableBuffer{T}, x, i::Int) where {T} = Core.bufset($(Expr(:boundscheck)), sb, convert(T, x)::T, i)
eltype(::Type{<:BufferType{T}}) where {T} = T
elsize(@nospecialize T::Type{<:BufferType}) = aligned_sizeof(eltype(T))
length(sb::BufferType) = Core.buflen(sb)
firstindex(sb::BufferType) = 1
lastindex(sb::BufferType) = length(sb)
keys(sb::BufferType) = OneTo(length(sb))
axes(sb::BufferType) = (OneTo(length(sb)),)
axes(sb::BufferType, d::Integer) = d <= 1 ? OneTo(length(sb)) : OneTo(1)
iterate(v::BufferType, i=1) = (length(v) < i ? nothing : (v[i], i + 1))
isempty(sb::BufferType) = (length(sb) == 0)
function ==(v1::BufferType, v2::BufferType)
    length(v1)==length(v2) || return false
    for i in 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end
