"""
    module OffsetDenseArrays

A minimal implementation of an offset array which is also <: DenseArray.
"""
module OffsetDenseArrays

struct OffsetDenseArray{A <: DenseVector, T} <: DenseVector{T}
    x::A
    offset::Int
end
OffsetDenseArray(x::AbstractVector{T}, i::Integer) where {T} = OffsetDenseArray{typeof(x), T}(x, Int(i))

Base.size(x::OffsetDenseArray) = size(x.x)
Base.pointer(x::OffsetDenseArray) = pointer(x.x)

function Base.getindex(x::OffsetDenseArray, i::Integer)
    @boundscheck checkbounds(x.x, i - x.offset)
    x.x[i - x.offset]
end

function Base.setindex(x::OffsetDenseArray, v, i::Integer)
    @boundscheck checkbounds(x.x, i - x.offset)
    x.x[i - x.offset] = v
end

Base.IndexStyle(::Type{<:OffsetDenseArray}) = Base.IndexLinear()
Base.axes(x::OffsetDenseArray) = (x.offset + 1 : x.offset + length(x.x),)
Base.keys(x::OffsetDenseArray) = only(axes(x))

end # module
