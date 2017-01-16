# This file is a part of Julia. License is MIT: http://julialang.org/license

using  Base.MultiplicativeInverses: SignedMultiplicativeInverse

immutable ReshapedArray{T,N,P<:AbstractArray,MI<:Tuple{Vararg{SignedMultiplicativeInverse{Int}}}} <: AbstractArray{T,N}
    parent::P
    dims::NTuple{N,Int}
    mi::MI
end
ReshapedArray{T,N}(parent::AbstractArray{T}, dims::NTuple{N,Int}, mi) = ReshapedArray{T,N,typeof(parent),typeof(mi)}(parent, dims, mi)

# LinearFast ReshapedArray
typealias ReshapedArrayLF{T,N,P<:AbstractArray} ReshapedArray{T,N,P,Tuple{}}

# Fast iteration on ReshapedArrays: use the parent iterator
immutable ReshapedArrayIterator{I,M}
    iter::I
    mi::NTuple{M,SignedMultiplicativeInverse{Int}}
end
ReshapedArrayIterator(A::ReshapedArray) = _rs_iterator(parent(A), A.mi)
function _rs_iterator{M}(P, mi::NTuple{M})
    iter = eachindex(P)
    ReshapedArrayIterator{typeof(iter),M}(iter, mi)
end

immutable ReshapedIndex{T}
    parentindex::T
end

# eachindex(A::ReshapedArray) = ReshapedArrayIterator(A)  # TODO: uncomment this line
start(R::ReshapedArrayIterator) = start(R.iter)
@inline done(R::ReshapedArrayIterator, i) = done(R.iter, i)
@inline function next(R::ReshapedArrayIterator, i)
    item, inext = next(R.iter, i)
    ReshapedIndex(item), inext
end
length(R::ReshapedArrayIterator) = length(R.iter)

reshape(parent::AbstractArray, dims::IntOrInd...) = reshape(parent, dims)
reshape(parent::AbstractArray, shp::NeedsShaping) = reshape(parent, to_shape(shp))
reshape(parent::AbstractArray, dims::Dims)        = _reshape(parent, dims)

reshape{T,N}(parent::AbstractArray{T,N}, ndims::Type{Val{N}}) = parent
function reshape{T,AN,N}(parent::AbstractArray{T,AN}, ndims::Type{Val{N}})
    reshape(parent, rdims((), indices(parent), Val{N}))
end
# Move elements from inds to out until out reaches the desired
# dimensionality N, either filling with OneTo(1) or collapsing the
# product of trailing dims into the last element
@pure rdims{N}(out::NTuple{N,Any}, inds::Tuple{}, ::Type{Val{N}}) = out
@pure function rdims{N}(out::NTuple{N,Any}, inds::Tuple{Any, Vararg{Any}}, ::Type{Val{N}})
    l = length(last(out)) * prod(map(length, inds))
    (front(out)..., OneTo(l))
end
@pure rdims{N}(out::Tuple, inds::Tuple{}, ::Type{Val{N}}) = rdims((out..., OneTo(1)), (), Val{N})
@pure rdims{N}(out::Tuple, inds::Tuple{Any, Vararg{Any}}, ::Type{Val{N}}) = rdims((out..., first(inds)), tail(inds), Val{N})

# _reshape on Array returns an Array
_reshape(parent::Vector, dims::Dims{1}) = parent
_reshape(parent::Array, dims::Dims{1}) = reshape(parent, dims)
_reshape(parent::Array, dims::Dims) = reshape(parent, dims)

# When reshaping Vector->Vector, don't wrap with a ReshapedArray
function _reshape(v::AbstractVector, dims::Dims{1})
    len = dims[1]
    len == length(v) || throw(DimensionMismatch("parent has $(length(v)) elements, which is incompatible with length $len"))
    v
end
# General reshape
function _reshape(parent::AbstractArray, dims::Dims)
    n = _length(parent)
    prod(dims) == n || throw(DimensionMismatch("parent has $n elements, which is incompatible with size $dims"))
    __reshape((parent, linearindexing(parent)), dims)
end

# Reshaping a ReshapedArray
_reshape{T}(v::ReshapedArray{T,1}, dims::Dims{1}) = _reshape(v.parent, dims)
_reshape(R::ReshapedArray, dims::Dims) = _reshape(R.parent, dims)

function __reshape(p::Tuple{AbstractArray,LinearSlow}, dims::Dims)
    parent = p[1]
    strds = front(size_strides(parent))
    strds1 = map(s->max(1,s), strds)  # for resizing empty arrays
    mi = map(SignedMultiplicativeInverse, strds1)
    ReshapedArray(parent, dims, reverse(mi))
end

function __reshape(p::Tuple{AbstractArray,LinearFast}, dims::Dims)
    parent = p[1]
    ReshapedArray(parent, dims, ())
end

@inline size_strides(A::AbstractArray) = tail(size_strides((1,), size(A)...))
size_strides(out::Tuple) = out
@inline size_strides(out, s, sz...) = size_strides((out..., out[end]*s), sz...)

size(A::ReshapedArray) = A.dims
similar(A::ReshapedArray, eltype::Type, dims::Dims) = similar(parent(A), eltype, dims)
linearindexing{R<:ReshapedArrayLF}(::Type{R}) = LinearFast()
parent(A::ReshapedArray) = A.parent
parentindexes(A::ReshapedArray) = map(s->1:s, size(parent(A)))
reinterpret{T}(::Type{T}, A::ReshapedArray, dims::Dims) = reinterpret(T, parent(A), dims)

@inline ind2sub_rs(::Tuple{}, i::Int) = i
@inline ind2sub_rs(strds, i) = ind2sub_rs((), strds, i-1)
@inline ind2sub_rs(out, ::Tuple{}, ind) = (ind+1, out...)
@inline function ind2sub_rs(out, strds, ind)
    d, r = divrem(ind, strds[1])
    ind2sub_rs((d+1, out...), tail(strds), r)
end

@inline function getindex(A::ReshapedArrayLF, index::Int)
    @boundscheck checkbounds(A, index)
    @inbounds ret = parent(A)[index]
    ret
end
@inline function getindex(A::ReshapedArray, indexes::Int...)
    @boundscheck checkbounds(A, indexes...)
    _unsafe_getindex(A, indexes...)
end
@inline function getindex(A::ReshapedArray, index::ReshapedIndex)
    @boundscheck checkbounds(parent(A), index.parentindex)
    @inbounds ret = parent(A)[index.parentindex]
    ret
end

@inline function _unsafe_getindex(A::ReshapedArray, indexes::Int...)
    @inbounds ret = parent(A)[ind2sub_rs(A.mi, sub2ind(size(A), indexes...))...]
    ret
end
@inline function _unsafe_getindex(A::ReshapedArrayLF, indexes::Int...)
    @inbounds ret = parent(A)[sub2ind(size(A), indexes...)]
    ret
end

@inline function setindex!(A::ReshapedArrayLF, val, index::Int)
    @boundscheck checkbounds(A, index)
    @inbounds parent(A)[index] = val
    val
end
@inline function setindex!(A::ReshapedArray, val, indexes::Int...)
    @boundscheck checkbounds(A, indexes...)
    _unsafe_setindex!(A, val, indexes...)
end
@inline function setindex!(A::ReshapedArray, val, index::ReshapedIndex)
    @boundscheck checkbounds(parent(A), index.parentindex)
    @inbounds parent(A)[index.parentindex] = val
    val
end

@inline function _unsafe_setindex!(A::ReshapedArray, val, indexes::Int...)
    @inbounds parent(A)[ind2sub_rs(A.mi, sub2ind(size(A), indexes...))...] = val
    val
end
@inline function _unsafe_setindex!(A::ReshapedArrayLF, val, indexes::Int...)
    @inbounds parent(A)[sub2ind(size(A), indexes...)] = val
    val
end

# helpful error message for a common failure case
typealias ReshapedRange{T,N,A<:Range} ReshapedArray{T,N,A,Tuple{}}
setindex!(A::ReshapedRange, val, index::Int) = _rs_setindex!_err()
setindex!(A::ReshapedRange, val, indexes::Int...) = _rs_setindex!_err()
setindex!(A::ReshapedRange, val, index::ReshapedIndex) = _rs_setindex!_err()

_rs_setindex!_err() = error("indexed assignment fails for a reshaped range; consider calling collect")

unsafe_convert{T}(::Type{Ptr{T}}, a::ReshapedArray{T}) = unsafe_convert(Ptr{T}, parent(a))
