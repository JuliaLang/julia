
# SimpleBuffer
if nameof(@__MODULE__) === :Base  # avoid method overwrite
function SimpleBuffer{T}(::UndefInitializer, n::Int) where {T}
    ccall(:jl_new_sbuf, Any, (Any, UInt), T, n)
end
SimpleBuffer(a::AbstractArray{T}) where {T} = SimpleBuffer{T}(a)
function SimpleBuffer{T}(a::AbstractArray) where {T}
    n = length(a)
    sb = SimpleBuffer{T}(undef, n)
    i = 1
    for a_i in a
        @inbounds sb[i] = a_i
        i += 1
    end
    return sb
end
end

@eval getindex(sb::SimpleBuffer{T}, i::Int) where {T} = Core.sbufref($(Expr(:boundscheck)), sb, i)
@eval setindex!(sb::SimpleBuffer{T}, x, i::Int) where {T} = Core.sbufset($(Expr(:boundscheck)), sb, convert(T, x)::T, i)
eltype(::Type{<:SimpleBuffer{T}}) where {T} = T
elsize(@nospecialize T::Type{<:SimpleBuffer}) = aligned_sizeof(eltype(T))
length(sb::SimpleBuffer) = Core.sbuflen(sb)
firstindex(sb::SimpleBuffer) = 1
lastindex(sb::SimpleBuffer) = length(sb)
keys(sb::SimpleBuffer) = OneTo(length(sb))
axes(sb::SimpleBuffer) = (OneTo(length(sb)),)
axes(sb::SimpleBuffer, d::Integer) = d <= 1 ? OneTo(length(sb)) : OneTo(1)
iterate(v::SimpleBuffer, i=1) = (length(v) < i ? nothing : (v[i], i + 1))
isempty(sb::SimpleBuffer) = (length(sb) == 0)
function ==(v1::SimpleBuffer, v2::SimpleBuffer)
    length(v1)==length(v2) || return false
    for i = 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end
