# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias NonSliceIndex Union{Colon, AbstractVector}
typealias ViewIndex Union{Real, NonSliceIndex}

# L is true if the view itself supports fast linear indexing
immutable SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
    first_index::Int   # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
end
# Compute the linear indexability of the indices, and combine it with the linear indexing of the parent
function SubArray(parent::AbstractArray, indexes::Tuple, dims::Tuple)
    SubArray(linearindexing(viewindexing(indexes), linearindexing(parent)), parent, indexes, convert(Dims, dims))
end
function SubArray{P, I, N}(::LinearSlow, parent::P, indexes::I, dims::NTuple{N, Int})
    SubArray{eltype(P), N, P, I, false}(parent, indexes, dims, 0, 0)
end
function SubArray{P, I, N}(::LinearFast, parent::P, indexes::I, dims::NTuple{N, Int})
    # Compute the first index and stride
    SubArray{eltype(P), N, P, I, true}(parent, indexes, dims, compute_first_index(parent, indexes), compute_stride1(parent, indexes))
end

# The NoSlice one-element vector type keeps dimensions without losing performance
immutable NoSlice <: AbstractVector{Int}
    i::Int
end
size(::NoSlice) = (1,)
length(::NoSlice) = 1
linearindexing(::Type{NoSlice}) = LinearFast()
function getindex(N::NoSlice, i::Int)
    @_inline_meta
    @boundscheck i == 1 || throw_boundserror(N, i)
    N.i
end
function getindex(N::NoSlice, i::NoSlice)
    @_inline_meta
    @boundscheck i.i == 1 || throw_boundserror(N, i)
    N
end
getindex(N::NoSlice, ::Colon) = N
function getindex(N::NoSlice, r::Range{Int})
    @_inline_meta
    @boundscheck checkbounds(N, r)
    N
end

abstract AbstractCartesianIndex{N} # This is a hacky forward declaration for CartesianIndex
typealias StridedArray{T,N,A<:DenseArray,I<:Tuple{Vararg{Union{RangeIndex, NoSlice, AbstractCartesianIndex}}}} Union{DenseArray{T,N}, SubArray{T,N,A,I}}
typealias StridedVector{T,A<:DenseArray,I<:Tuple{Vararg{Union{RangeIndex, NoSlice, AbstractCartesianIndex}}}}  Union{DenseArray{T,1}, SubArray{T,1,A,I}}
typealias StridedMatrix{T,A<:DenseArray,I<:Tuple{Vararg{Union{RangeIndex, NoSlice, AbstractCartesianIndex}}}}  Union{DenseArray{T,2}, SubArray{T,2,A,I}}
typealias StridedVecOrMat{T} Union{StridedVector{T}, StridedMatrix{T}}

# This computes the linear indexing compatability for a given tuple of indices
viewindexing() = LinearFast()
# Leading scalar indexes simply increase the stride
viewindexing(I::Tuple{Union{Real, NoSlice}, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# Colons may begin a section which may be followed by any number of Colons
viewindexing(I::Tuple{Colon, Colon, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# A UnitRange can follow Colons, but only if all other indices are scalar
viewindexing(I::Tuple{Colon, UnitRange, Vararg{Union{Real, NoSlice}}}) = LinearFast()
# In general, ranges are only fast if all other indices are scalar
viewindexing(I::Tuple{Union{Range, Colon}, Vararg{Union{Real, NoSlice}}}) = LinearFast()
# All other index combinations are slow
viewindexing(I::Tuple{Vararg{Any}}) = LinearSlow()
# Of course, all other array types are slow
viewindexing(I::Tuple{AbstractArray, Vararg{Any}}) = LinearSlow()

# Simple utilities
size(V::SubArray) = V.dims
length(V::SubArray) = prod(V.dims)

similar(V::SubArray, T, dims::Dims) = similar(V.parent, T, dims)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(i->1:size(a,i), ndims(a))

## SubArray creation
# Drops singleton dimensions (those indexed with a scalar)
function slice(A::AbstractArray, I::ViewIndex...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    J = to_indexes(I...)
    SubArray(A, J, index_shape(A, J...))
end

keep_leading_scalars(T::Tuple{}) = T
keep_leading_scalars(T::Tuple{Real, Vararg{Real}}) = T
keep_leading_scalars(T::Tuple{Real, Vararg{Any}}) = (@_inline_meta; (NoSlice(T[1]), keep_leading_scalars(tail(T))...))
keep_leading_scalars(T::Tuple{Any, Vararg{Any}}) = (@_inline_meta; (T[1], keep_leading_scalars(tail(T))...))

function sub(A::AbstractArray, I::ViewIndex...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    J = keep_leading_scalars(to_indexes(I...))
    SubArray(A, J, index_shape(A, J...))
end

# Re-indexing is the heart of a view, transforming A[i, j][x, y] to A[i[x], j[y]]
#
# Recursively look through the heads of the parent- and sub-indexes, considering
# the following cases:
# * Parent index is empty  -> ignore trailing scalars, but preserve added dimensions
# * Parent index is Any    -> re-index that with the sub-index
# * Parent index is Scalar -> that dimension was dropped, so skip the sub-index and use the index as is
#
# Furthermore, we must specially consider the case with one final sub-index,
# as it may be a linear index that spans multiple parent indexes.

typealias DroppedScalar Union{Real, AbstractCartesianIndex}
# When indexing beyond the parent indices, drop all trailing scalars (they must be 1 to be inbounds)
reindex(V, idxs::Tuple{}, subidxs::Tuple{Vararg{DroppedScalar}}) = ()
# Drop any intervening scalars that are beyond the parent indices but before a nonscalar
reindex(V, idxs::Tuple{}, subidxs::Tuple{DroppedScalar, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (reindex(V, idxs, tail(subidxs))...))
# And keep the nonscalar index to add the dimension
reindex(V, idxs::Tuple{}, subidxs::Tuple{Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (subidxs[1], reindex(V, idxs, tail(subidxs))...))

reindex(V, idxs::Tuple{Any}, subidxs::Tuple{Any}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]],))
reindex(V, idxs::Tuple{Any}, subidxs::Tuple{Any, Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]],))
reindex(V, idxs::Tuple{Any, Any, Vararg{Any}}, subidxs::Tuple{Any}) =
    (@_propagate_inbounds_meta; (merge_indexes(V, idxs, subidxs[1]),))
# As an optimization, we don't need to merge indices if all trailing indices are dropped scalars
reindex(V, idxs::Tuple{Any, DroppedScalar, Vararg{DroppedScalar}}, subidxs::Tuple{Any}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]], tail(idxs)...))
reindex(V, idxs::Tuple{Any, Any, Vararg{Any}}, subidxs::Tuple{Any, Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]], reindex(V, tail(idxs), tail(subidxs))...))

reindex(V, idxs::Tuple{DroppedScalar}, subidxs::Tuple{Any}) = idxs
reindex(V, idxs::Tuple{DroppedScalar}, subidxs::Tuple{Any, Any, Vararg{Any}}) = idxs
reindex(V, idxs::Tuple{DroppedScalar, Any, Vararg{Any}}, subidxs::Tuple{Any}) =
    (@_propagate_inbounds_meta; (idxs[1], reindex(V, tail(idxs), subidxs)...))
reindex(V, idxs::Tuple{DroppedScalar, Any, Vararg{Any}}, subidxs::Tuple{Any, Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1], reindex(V, tail(idxs), subidxs)...))

# In general, we simply re-index the parent indices by the provided ones
getindex(V::SubArray) = (@_propagate_inbounds_meta; getindex(V, 1))
function getindex(V::SubArray, I::Real...)
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = V.parent[reindex(V, V.indexes, to_indexes(I...))...]
    r
end

typealias FastSubArray{T,N,P,I} SubArray{T,N,P,I,true}
getindex(V::FastSubArray) = (@_propagate_inbounds_meta; getindex(V, 1))
function getindex(V::FastSubArray, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.first_index + V.stride1*(to_index(i)-1)]
    r
end
# We can avoid a multiplication if the first parent index is a Colon or UnitRange
typealias FastContiguousSubArray{T,N,P,I<:Tuple{Union{Colon, UnitRange}, Vararg{Any}}} SubArray{T,N,P,I,true}
function getindex(V::FastContiguousSubArray, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.first_index + to_index(i)-1]
    r
end
# We need this because the ::ViewIndex... method would otherwise obscure the Base fallback
function getindex(V::FastSubArray, I::Real...)
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = getindex(V, sub2ind(size(V), to_indexes(I...)...))
    r
end
getindex{T,N}(V::SubArray{T,N}, I::ViewIndex...) = (@_propagate_inbounds_meta; copy(slice(V, I...)))

setindex!(V::SubArray, x) = (@_propagate_inbounds_meta; setindex!(V, x, 1))
function setindex!{T,N}(V::SubArray{T,N}, x, I::Real...)
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds V.parent[reindex(V, V.indexes, to_indexes(I...))...] = x
    V
end
# Nonscalar setindex! falls back to the defaults

function slice{T,N}(V::SubArray{T,N}, I::ViewIndex...)
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    idxs = reindex(V, V.indexes, to_indexes(I...))
    SubArray(V.parent, idxs, index_shape(V.parent, idxs...))
end

function sub{T,N}(V::SubArray{T,N}, I::ViewIndex...)
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    idxs = reindex(V, V.indexes, keep_leading_scalars(to_indexes(I...)))
    SubArray(V.parent, idxs, index_shape(V.parent, idxs...))
end

linearindexing(A::FastSubArray) = LinearFast()
linearindexing(A::SubArray) = LinearSlow()

getindex(::Colon, i) = to_index(i)
unsafe_getindex(::Colon, i) = to_index(i)

step(::Colon) = 1
first(::Colon) = 1
isempty(::Colon) = false
in(::Integer, ::Colon) = true

# Strides are the distance between adjacent elements in a given dimension,
# so they are well-defined even for non-linear memory layouts
strides{T,N,P,I}(V::SubArray{T,N,P,I}) = substrides(V.parent, V.indexes)

substrides(parent, I::Tuple) = substrides(1, parent, 1, I)
substrides(s, parent, dim, ::Tuple{}) = ()
substrides(s, parent, dim, I::Tuple{Real, Vararg{Any}}) = (substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{AbstractCartesianIndex, Vararg{Any}}) = substrides(s, parent, dim, (I[1].I..., tail(I)...))
substrides(s, parent, dim, I::Tuple{NoSlice, Vararg{Any}}) = (s, substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Colon, Vararg{Any}}) = (s, substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Range, Vararg{Any}}) = (s*step(I[1]), substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("strides is invalid for SubArrays with indices of type $(typeof(I[1]))"))

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

compute_stride1(parent, I::Tuple) = compute_stride1(1, parent, 1, I)
compute_stride1(s, parent, dim, I::Tuple{}) = s
compute_stride1(s, parent, dim, I::Tuple{Union{DroppedScalar, NoSlice}, Vararg{Any}}) =
    (@_inline_meta; compute_stride1(s*size(parent, dim), parent, dim+1, tail(I)))
compute_stride1(s, parent, dim, I::Tuple{Range, Vararg{Any}}) = s*step(I[1])
compute_stride1(s, parent, dim, I::Tuple{Colon, Vararg{Any}}) = s
compute_stride1(s, parent, dim, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("invalid strided index type $(typeof(I[1]))"))

iscontiguous(A::SubArray) = iscontiguous(typeof(A))
iscontiguous{S<:SubArray}(::Type{S}) = false
iscontiguous{F<:FastContiguousSubArray}(::Type{F}) = true

# Fast linear SubArrays have their first index cached
first_index(V::FastSubArray) = V.first_index
first_index(V::SubArray) = first_index(V.parent, V.indexes)
function first_index(P::AbstractArray, indexes::Tuple)
    f = 1
    s = 1
    for i = 1:length(indexes)
        f += (first(indexes[i])-1)*s
        s *= size(P, i)
    end
    f
end

# Computing the first index simply steps through the indices, accumulating the
# sum of index each multiplied by the parent's stride.
# The running sum is `f`; the cumulative stride product is `s`.
compute_first_index(parent, I::Tuple) = compute_first_index(1, 1, parent, 1, I)
compute_first_index(f, s, parent, dim, I::Tuple{Real, Vararg{Any}}) =
    (@_inline_meta; compute_first_index(f + (I[1]-1)*s, s*size(parent, dim), parent, dim+1, tail(I)))
compute_first_index(f, s, parent, dim, I::Tuple{NoSlice, Vararg{Any}}) =
    (@_inline_meta; compute_first_index(f + (I[1].i-1)*s, s*size(parent, dim), parent, dim+1, tail(I)))
# Just splat out the cartesian indices and continue
compute_first_index(f, s, parent, dim, I::Tuple{AbstractCartesianIndex, Vararg{Any}}) =
    (@_inline_meta; compute_first_index(f, s, parent, dim, (I[1].I..., tail(I)...)))
compute_first_index(f, s, parent, dim, I::Tuple{Colon, Vararg{Any}}) =
    (@_inline_meta; compute_first_index(f, s*size(parent, dim), parent, dim+1, tail(I)))
compute_first_index(f, s, parent, dim, I::Tuple{Any, Vararg{Any}}) =
    (@_inline_meta; compute_first_index(f + (first(I[1])-1)*s, s*size(parent, dim), parent, dim+1, tail(I)))
compute_first_index(f, s, parent, dim, I::Tuple{}) = f


unsafe_convert{T,N,P<:Array,I<:Tuple{Vararg{Union{RangeIndex, NoSlice}}}}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    pointer(V.parent) + (first_index(V)-1)*sizeof(T)

unsafe_convert{T,N,P<:Array,I<:Tuple{Vararg{Union{RangeIndex, NoSlice}}}}(::Type{Ptr{Void}}, V::SubArray{T,N,P,I}) =
    convert(Ptr{Void}, unsafe_convert(Ptr{T}, V))

pointer(V::FastSubArray, i::Int) = pointer(V.parent, V.first_index + V.stride1*(i-1))
pointer(V::FastContiguousSubArray, i::Int) = pointer(V.parent, V.first_index + i-1)
pointer(V::SubArray, i::Int) = pointer(V, ind2sub(size(V), i))

function pointer{T,N,P<:Array,I<:Tuple{Vararg{Union{RangeIndex, NoSlice}}}}(V::SubArray{T,N,P,I}, is::Tuple{Vararg{Int}})
    index = first_index(V)
    strds = strides(V)
    for d = 1:length(is)
        index += (is[d]-1)*strds[d]
    end
    return pointer(V.parent, index)
end

## Convert
convert{T,S,N}(::Type{Array{T,N}}, V::SubArray{S,N}) = copy!(Array(T, size(V)), V)


## Compatability
# deprecate?
function parentdims(s::SubArray)
    nd = ndims(s)
    dimindex = Array(Int, nd)
    sp = strides(s.parent)
    sv = strides(s)
    j = 1
    for i = 1:ndims(s.parent)
        r = s.indexes[i]
        if j <= nd && (isa(r,Union{Colon,Range}) ? sp[i]*step(r) : sp[i]) == sv[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end
