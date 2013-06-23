## subviews of arrays ##

abstract AbstractStridedView{T,N} <: AbstractArray{T,N}
abstract AbstractContiguousView{T,N} <: AbstractStridedView{T,N}

typealias ContiguousArray{T,N} Union(Array{T,N}, AbstractContiguousView{T,N})

#####################################################################
#
#   In general, a subview type should be a subtype
#   of AbstractArray, and with following methods specialized:
#
#   - isempty
#   - length
#   - size
#   - getindex
#   - setindex!
#   - slice
#   
#   For subtypes of AbstractStridedView, we additionally have   
#
#   - stride
#   - strides
#   - pointer
#
#   AbstractContiguousView is a subtype of AbstractStridedView,
#   for which no additional methods are required. We just have
#   to exploit the fact that the memory is contiguous.
#
#   Notes:
#   - getindex & setindex! do not do bound checking
#   - We will check the validity of indexes upon the construction
#     of a view using the ``sub`` function.
#
#####################################################################

## helpers

offset_pointer{T}(a::Ptr{T}, offset::Int) = a + sizeof(T) * offset


### Contiguous views ###

immutable VectorView{T,A<:AbstractArray} <: AbstractContiguousView{T,1}
    parent::A
    ptr::Ptr{T}
    len::Int
end

isempty(a::VectorView) = (a.len == 0)
length(a::VectorView) = a.len
size(a::VectorView) = (a.len, 1)
size(a::VectorView, d::Int) = d == 1 ? a.len : 1
stride(a::VectorView, d::Int) = d == 1 ? 1 : throw(BoundsError())
strides(a::VectorView) = (1,)

getindex(a::VectorView, i::Int) = unsafe_load(a.ptr, i)
setindex!(a::VectorView, v, i::Int) = unsafe_store!(a.ptr, v, i)

pointer(a::VectorView) = a.ptr
pointer(a::VectorView, i::Int) = offset_pointer(a.ptr, i-1)


### Strided views ###

immutable StridedVectorView{T,A<:AbstractArray} <: AbstractStridedView{T,1}
    parent::A
    ptr::Ptr{T}
    len::Int
    stride1::Int
end

isempty(a::StridedVectorView) = (a.len == 0)
length(a::StridedVectorView) = a.len
size(a::StridedVectorView) = (a.len, 1)
size(a::StridedVectorView, d::Int) = d == 1 ? a.len : 1
stride(a::StridedVectorView, d::Int) = d == 1 ? a.stride1 : throw(BoundsError())
strides(a::StridedVectorView) = (a.stride1,)

getindex(a::StridedVectorView, i::Int) = unsafe_load(a.ptr, (i-1) * a.stride1 + 1)
setindex!(a::StridedVectorView, v, i::Int) = unsafe_store!(a.ptr, v, (i-1) * a.stride1 + 1)

pointer(a::StridedVectorView) = a.ptr
pointer(a::StridedVectorView, i::Int) = offset_pointer(a.ptr, (i-1) * a.stride1)


