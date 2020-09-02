# This file is a part of Julia. License is MIT: https://julialang.org/license

using  Base.MultiplicativeInverses: SignedMultiplicativeInverse

struct ReshapedArray{T,N,P<:AbstractArray,MI<:Tuple{Vararg{SignedMultiplicativeInverse{Int}}}} <: AbstractArray{T,N}
    parent::P
    dims::NTuple{N,Int}
    mi::MI
end
ReshapedArray(parent::AbstractArray{T}, dims::NTuple{N,Int}, mi) where {T,N} = ReshapedArray{T,N,typeof(parent),typeof(mi)}(parent, dims, mi)

# IndexLinear ReshapedArray
const ReshapedArrayLF{T,N,P<:AbstractArray} = ReshapedArray{T,N,P,Tuple{}}

# Fast iteration on ReshapedArrays: use the parent iterator
struct ReshapedArrayIterator{I,M}
    iter::I
    mi::NTuple{M,SignedMultiplicativeInverse{Int}}
end
ReshapedArrayIterator(A::ReshapedArray) = _rs_iterator(parent(A), A.mi)
function _rs_iterator(P, mi::NTuple{M}) where M
    iter = eachindex(P)
    ReshapedArrayIterator{typeof(iter),M}(iter, mi)
end

struct ReshapedIndex{T}
    parentindex::T
end

# eachindex(A::ReshapedArray) = ReshapedArrayIterator(A)  # TODO: uncomment this line
@inline function iterate(R::ReshapedArrayIterator, i...)
    item, inext = iterate(R.iter, i...)
    ReshapedIndex(item), inext
end
length(R::ReshapedArrayIterator) = length(R.iter)
eltype(::Type{<:ReshapedArrayIterator{I}}) where {I} = @isdefined(I) ? ReshapedIndex{eltype(I)} : Any

## reshape(::Array, ::Dims) returns an Array, except for isbitsunion eltypes (issue #28611)
# reshaping to same # of dimensions
function reshape(a::Array{T,M}, dims::NTuple{N,Int}) where {T,N,M}
    throw_dmrsa(dims, len) =
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $len"))

    if prod(dims) != length(a)
        throw_dmrsa(dims, length(a))
    end
    isbitsunion(T) && return ReshapedArray(a, dims, ())
    if N == M && dims == size(a)
        return a
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

"""
    reshape(A, dims...) -> AbstractArray
    reshape(A, dims) -> AbstractArray

Return an array with the same data as `A`, but with different
dimension sizes or number of dimensions. The two arrays share the same
underlying data, so that the result is mutable if and only if `A` is
mutable, and setting elements of one alters the values of the other.

The new dimensions may be specified either as a list of arguments or
as a shape tuple. At most one dimension may be specified with a `:`,
in which case its length is computed such that its product with all
the specified dimensions is equal to the length of the original array
`A`. The total number of elements must not change.

# Examples
```jldoctest
julia> A = Vector(1:16)
16-element Vector{Int64}:
  1
  2
  3
  4
  5
  6
  7
  8
  9
 10
 11
 12
 13
 14
 15
 16

julia> reshape(A, (4, 4))
4×4 Matrix{Int64}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> reshape(A, 2, :)
2×8 Matrix{Int64}:
 1  3  5  7   9  11  13  15
 2  4  6  8  10  12  14  16

julia> reshape(1:6, 2, 3)
2×3 reshape(::UnitRange{Int64}, 2, 3) with eltype Int64:
 1  3  5
 2  4  6
```
"""
reshape

reshape(parent::AbstractArray, dims::IntOrInd...) = reshape(parent, dims)
reshape(parent::AbstractArray, shp::Tuple{Union{Integer,OneTo}, Vararg{Union{Integer,OneTo}}}) = reshape(parent, to_shape(shp))
reshape(parent::AbstractArray, dims::Dims)        = _reshape(parent, dims)

# Allow missing dimensions with Colon():
reshape(parent::AbstractVector, ::Colon) = parent
reshape(parent::AbstractArray, dims::Int...) = reshape(parent, dims)
reshape(parent::AbstractArray, dims::Union{Int,Colon}...) = reshape(parent, dims)
reshape(parent::AbstractArray, dims::Tuple{Vararg{Union{Int,Colon}}}) = _reshape(parent, _reshape_uncolon(parent, dims))
@inline function _reshape_uncolon(A, dims)
    @noinline throw1(dims) = throw(DimensionMismatch(string("new dimensions $(dims) ",
        "may have at most one omitted dimension specified by `Colon()`")))
    @noinline throw2(A, dims) = throw(DimensionMismatch(string("array size $(length(A)) ",
        "must be divisible by the product of the new dimensions $dims")))
    pre = _before_colon(dims...)
    post = _after_colon(dims...)
    _any_colon(post...) && throw1(dims)
    sz, remainder = divrem(length(A), prod(pre)*prod(post))
    remainder == 0 || throw2(A, dims)
    (pre..., Int(sz), post...)
end
@inline _any_colon() = false
@inline _any_colon(dim::Colon, tail...) = true
@inline _any_colon(dim::Any, tail...) = _any_colon(tail...)
@inline _before_colon(dim::Any, tail...) = (dim, _before_colon(tail...)...)
@inline _before_colon(dim::Colon, tail...) = ()
@inline _after_colon(dim::Any, tail...) =  _after_colon(tail...)
@inline _after_colon(dim::Colon, tail...) = tail

reshape(parent::AbstractArray{T,N}, ndims::Val{N}) where {T,N} = parent
function reshape(parent::AbstractArray, ndims::Val{N}) where N
    reshape(parent, rdims(Val(N), axes(parent)))
end

# Move elements from inds to out until out reaches the desired
# dimensionality N, either filling with OneTo(1) or collapsing the
# product of trailing dims into the last element
rdims_trailing(l, inds...) = length(l) * rdims_trailing(inds...)
rdims_trailing(l) = length(l)
rdims(out::Val{N}, inds::Tuple) where {N} = rdims(ntuple(i -> OneTo(1), Val(N)), inds)
rdims(out::Tuple{}, inds::Tuple{}) = () # N == 0, M == 0
rdims(out::Tuple{}, inds::Tuple{Any}) = ()
rdims(out::Tuple{}, inds::NTuple{M,Any}) where {M} = ()
rdims(out::Tuple{Any}, inds::Tuple{}) = out # N == 1, M == 0
rdims(out::NTuple{N,Any}, inds::Tuple{}) where {N} = out # N > 1, M == 0
rdims(out::Tuple{Any}, inds::Tuple{Any}) = inds # N == 1, M == 1
rdims(out::Tuple{Any}, inds::NTuple{M,Any}) where {M} = (OneTo(rdims_trailing(inds...)),) # N == 1, M > 1
rdims(out::NTuple{N,Any}, inds::NTuple{N,Any}) where {N} = inds # N > 1, M == N
rdims(out::NTuple{N,Any}, inds::NTuple{M,Any}) where {N,M} = (first(inds), rdims(tail(out), tail(inds))...) # N > 1, M > 1, M != N


# _reshape on Array returns an Array
_reshape(parent::Vector, dims::Dims{1}) = parent
_reshape(parent::Array, dims::Dims{1}) = reshape(parent, dims)
_reshape(parent::Array, dims::Dims) = reshape(parent, dims)

# When reshaping Vector->Vector, don't wrap with a ReshapedArray
function _reshape(v::AbstractVector, dims::Dims{1})
    require_one_based_indexing(v)
    len = dims[1]
    len == length(v) || _throw_dmrs(length(v), "length", len)
    v
end
# General reshape
function _reshape(parent::AbstractArray, dims::Dims)
    n = length(parent)
    prod(dims) == n || _throw_dmrs(n, "size", dims)
    __reshape((parent, IndexStyle(parent)), dims)
end

@noinline function _throw_dmrs(n, str, dims)
    throw(DimensionMismatch("parent has $n elements, which is incompatible with $str $dims"))
end

# Reshaping a ReshapedArray
_reshape(v::ReshapedArray{<:Any,1}, dims::Dims{1}) = _reshape(v.parent, dims)
_reshape(R::ReshapedArray, dims::Dims) = _reshape(R.parent, dims)

function __reshape(p::Tuple{AbstractArray,IndexCartesian}, dims::Dims)
    parent = p[1]
    strds = front(size_to_strides(map(length, axes(parent))..., 1))
    strds1 = map(s->max(1,Int(s)), strds)  # for resizing empty arrays
    mi = map(SignedMultiplicativeInverse, strds1)
    ReshapedArray(parent, dims, reverse(mi))
end

function __reshape(p::Tuple{AbstractArray{<:Any,0},IndexCartesian}, dims::Dims)
    parent = p[1]
    ReshapedArray(parent, dims, ())
end

function __reshape(p::Tuple{AbstractArray,IndexLinear}, dims::Dims)
    parent = p[1]
    ReshapedArray(parent, dims, ())
end

size(A::ReshapedArray) = A.dims
similar(A::ReshapedArray, eltype::Type, dims::Dims) = similar(parent(A), eltype, dims)
IndexStyle(::Type{<:ReshapedArrayLF}) = IndexLinear()
parent(A::ReshapedArray) = A.parent
parentindices(A::ReshapedArray) = map(OneTo, size(parent(A)))
reinterpret(::Type{T}, A::ReshapedArray, dims::Dims) where {T} = reinterpret(T, parent(A), dims)
elsize(::Type{<:ReshapedArray{<:Any,<:Any,P}}) where {P} = elsize(P)

unaliascopy(A::ReshapedArray) = typeof(A)(unaliascopy(A.parent), A.dims, A.mi)
dataids(A::ReshapedArray) = dataids(A.parent)

@inline ind2sub_rs(ax, ::Tuple{}, i::Int) = (i,)
@inline ind2sub_rs(ax, strds, i) = _ind2sub_rs(ax, strds, i - 1)
@inline _ind2sub_rs(ax, ::Tuple{}, ind) = (ind + first(ax[end]),)
@inline function _ind2sub_rs(ax, strds, ind)
    d, r = divrem(ind, strds[1])
    (_ind2sub_rs(front(ax), tail(strds), r)..., d + first(ax[end]))
end
offset_if_vec(i::Integer, axs::Tuple{<:AbstractUnitRange}) = i + first(axs[1]) - 1
offset_if_vec(i::Integer, axs::Tuple) = i

@inline function getindex(A::ReshapedArrayLF, index::Int)
    @boundscheck checkbounds(A, index)
    @inbounds ret = parent(A)[index]
    ret
end
@inline function getindex(A::ReshapedArray{T,N}, indices::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, indices...)
    _unsafe_getindex(A, indices...)
end
@inline function getindex(A::ReshapedArray, index::ReshapedIndex)
    @boundscheck checkbounds(parent(A), index.parentindex)
    @inbounds ret = parent(A)[index.parentindex]
    ret
end

@inline function _unsafe_getindex(A::ReshapedArray{T,N}, indices::Vararg{Int,N}) where {T,N}
    axp = axes(A.parent)
    i = offset_if_vec(Base._sub2ind(size(A), indices...), axp)
    I = ind2sub_rs(axp, A.mi, i)
    _unsafe_getindex_rs(parent(A), I)
end
@inline _unsafe_getindex_rs(A, i::Integer) = (@inbounds ret = A[i]; ret)
@inline _unsafe_getindex_rs(A, I) = (@inbounds ret = A[I...]; ret)

@inline function setindex!(A::ReshapedArrayLF, val, index::Int)
    @boundscheck checkbounds(A, index)
    @inbounds parent(A)[index] = val
    val
end
@inline function setindex!(A::ReshapedArray{T,N}, val, indices::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, indices...)
    _unsafe_setindex!(A, val, indices...)
end
@inline function setindex!(A::ReshapedArray, val, index::ReshapedIndex)
    @boundscheck checkbounds(parent(A), index.parentindex)
    @inbounds parent(A)[index.parentindex] = val
    val
end

@inline function _unsafe_setindex!(A::ReshapedArray{T,N}, val, indices::Vararg{Int,N}) where {T,N}
    axp = axes(A.parent)
    i = offset_if_vec(Base._sub2ind(size(A), indices...), axp)
    @inbounds parent(A)[ind2sub_rs(axes(A.parent), A.mi, i)...] = val
    val
end

# helpful error message for a common failure case
const ReshapedRange{T,N,A<:AbstractRange} = ReshapedArray{T,N,A,Tuple{}}
setindex!(A::ReshapedRange, val, index::Int) = _rs_setindex!_err()
setindex!(A::ReshapedRange{T,N}, val, indices::Vararg{Int,N}) where {T,N} = _rs_setindex!_err()
setindex!(A::ReshapedRange, val, index::ReshapedIndex) = _rs_setindex!_err()

@noinline _rs_setindex!_err() = error("indexed assignment fails for a reshaped range; consider calling collect")

unsafe_convert(::Type{Ptr{T}}, a::ReshapedArray{T}) where {T} = unsafe_convert(Ptr{T}, parent(a))

# Add a few handy specializations to further speed up views of reshaped ranges
const ReshapedUnitRange{T,N,A<:AbstractUnitRange} = ReshapedArray{T,N,A,Tuple{}}
viewindexing(I::Tuple{Slice, ReshapedUnitRange, Vararg{ScalarIndex}}) = IndexLinear()
viewindexing(I::Tuple{ReshapedRange, Vararg{ScalarIndex}}) = IndexLinear()
compute_stride1(s, inds, I::Tuple{ReshapedRange, Vararg{Any}}) = s*step(I[1].parent)
compute_offset1(parent::AbstractVector, stride1::Integer, I::Tuple{ReshapedRange}) =
    (@_inline_meta; first(I[1]) - first(axes1(I[1]))*stride1)
substrides(strds::NTuple{N,Int}, I::Tuple{ReshapedUnitRange, Vararg{Any}}) where N =
    (size_to_strides(strds[1], size(I[1])...)..., substrides(tail(strds), tail(I))...)
unsafe_convert(::Type{Ptr{T}}, V::SubArray{T,N,P,<:Tuple{Vararg{Union{RangeIndex,ReshapedUnitRange}}}}) where {T,N,P} =
    unsafe_convert(Ptr{T}, V.parent) + (first_index(V)-1)*sizeof(T)
