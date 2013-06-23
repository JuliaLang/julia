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

compute_strides(d1::Int) = (1,)
compute_strides(d1::Int, d2::Int) = (1, d1)
compute_strides(d1::Int, d2::Int, d3::Int) = (1, d1, d1 * d2)
compute_strides(d1::Int, d2::Int, d3::Int, d4::Int) = (s3 = d1 * d2; s4 = s3 * d3; (1, d1, s3, s4))

function compute_strides(dims::Int...)
	n = length(dims)
	strides = Array(Int, n)
	strides[1] = 1
	strides[2] = dims[1]
	for i = 3 : n
		strides[i] = strides[i-1] * dims[i-1]
	end
	tuple(strides...)
end


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
stride(a::VectorView, d::Int) = d <= 1 ? 1 : a.len
strides(a::VectorView) = (1,)

getindex(a::VectorView, i::Int) = unsafe_load(a.ptr, i)
getindex(a::VectorView, i::Int, r::Int...) = unsafe_load(a.ptr, i)
setindex!(a::VectorView, v, i::Int) = unsafe_store!(a.ptr, v, i)
setindex!(a::VectorView, v, i::Int, r::Int...) = unsafe_store!(a.ptr, v, i)

pointer(a::VectorView) = a.ptr
pointer(a::VectorView, i::Int) = offset_pointer(a.ptr, i-1)


immutable MatrixView{T,A<:AbstractArray} <: AbstractContiguousView{T,2}
    parent::A
    ptr::Ptr{T}
    dim1::Int
    dim2::Int
    len::Int

    MatrixView(pa::A, p::Ptr{T}, d1::Int, d2::Int) = new(pa, p, d1, d2, d1 * d2)
end

isempty(a::MatrixView) = (a.len == 0)
length(a::MatrixView) = a.len
size(a::MatrixView) = (a.dim1, a.dim2)
size(a::MatrixView, d::Int) = d == 1 ? a.dim1 : d == 2 ? a.dim2 : 1
stride(a::MatrixView, d::Int) = d <= 1 ? 1 : d == 2 ? a.dim1 : a.len
strides(a::MatrixView) = (1, a.dim1)

getindex(a::MatrixView, i::Int) = unsafe_load(a.ptr, i)
getindex(a::MatrixView, i::Int, j::Int) = unsafe_load(a.ptr, i + (j - 1) * a.dim1)
getindex(a::MatrixView, i::Int, j::Int, r::Int...) = unsafe_load(a.ptr, i + (j - 1) * a.dim1)

setindex!(a::MatrixView, v, i::Int) = unsafe_store!(a.ptr, v, i)
setindex!(a::MatrixView, v, i::Int, j::Int) = unsafe_store!(a.ptr, v, i + (j - 1) * a.dim1)
setindex!(a::MatrixView, v, i::Int, j::Int, r::Int...) = unsafe_store!(a.ptr, v, i + (j - 1) * a.dim1)

pointer(a::MatrixView) = a.ptr
pointer(a::MatrixView, i::Int) = offset_pointer(a.ptr, i-1)


immutable MultidimView{T,N,A<:AbstractArray} <: AbstractContiguousView{T,N}  # Only for N >= 3
    parent::A
    ptr::Ptr{T}
    dims::NTuple{N,Int}
    strides::NTuple{N,Int}
    len::Int

    function MultidimView(pa::A, p::Ptr{T}, dims::NTuple{N,Int}) 
    	strides = compute_strides(dims)
    	len = strides[N] * dims[N]
    	new(pa, p, dims, strides, len)
    end
end

isempty(a::MultidimView) = (a.len == 0)
length(a::MultidimView) = a.len
size(a::MultidimView) = a.dims
size(a::MultidimView, d::Int) = d == 1 ? a.dim1 : d == 2 ? a.dim2 : 1
stride(a::MultidimView, d::Int) = d <= 1 ? 1 : d <= N ? a.strides[d] : a.len
strides(a::MultidimView) = a.strides

getindex(a::MultidimView, i::Int) = unsafe_load(a.ptr, i)
getindex(a::MultidimView, i::Int, j::Int) = unsafe_load(a.ptr, i + (j - 1) * a.strides[2])
getindex(a::MultidimView, i::Int, j::Int, k::Int) = unsafe_load(a.ptr, i + (j - 1) * a.strides[2] + (k - 1) * a.strides[3])
getindex(a::MultidimView, inds::Int...) = unsafe_load(a.ptr, sub2ind(a.dims, inds...))

setindex!(a::MultidimView, v, i::Int) = unsafe_store!(a.ptr, v, i)
setindex!(a::MultidimView, v, i::Int, j::Int) = unsafe_store!(a.ptr, v, i + (j - 1) * a.strides[2])
setindex!(a::MultidimView, v, i::Int, j::Int, k::Int) = unsafe_store!(a.ptr, v, i + (j - 1) * a.strides[2] + (k - 1) * a.strides[3])
setindex!(a::MultidimView, v, inds::Int...) = unsafe_store!(a.ptr, v, sub2ind(a.dims, inds...))

pointer(a::MultidimView) = a.ptr
pointer(a::MultidimView, i::Int) = offset_pointer(a.ptr, i-1)


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
stride(a::StridedVectorView, d::Int) = d == 1 ? a.stride1 : d > 1 ? a.stride1 * a.len : 1
strides(a::StridedVectorView) = (a.stride1,)

getindex(a::StridedVectorView, i::Int) = unsafe_load(a.ptr, (i-1) * a.stride1 + 1)
setindex!(a::StridedVectorView, v, i::Int) = unsafe_store!(a.ptr, v, (i-1) * a.stride1 + 1)

pointer(a::StridedVectorView) = a.ptr
pointer(a::StridedVectorView, i::Int) = offset_pointer(a.ptr, (i-1) * a.stride1)


