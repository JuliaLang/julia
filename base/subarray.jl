# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias NonSliceIndex Union{Colon, AbstractArray}
typealias ViewIndex Union{Real, NonSliceIndex}
abstract AbstractCartesianIndex{N} # This is a hacky forward declaration for CartesianIndex

# L is true if the view itself supports fast linear indexing
immutable SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
    first_index::Int   # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
    function SubArray(parent, indexes, dims, first_index, stride1)
        check_parent_index_match(parent, indexes)
        new(parent, indexes, dims, first_index, stride1)
    end
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

check_parent_index_match{T,N}(parent::AbstractArray{T,N}, indexes::NTuple{N}) = nothing
check_parent_index_match(parent, indexes) = throw(ArgumentError("number of indices ($(length(indexes))) must match the parent dimensionality ($(ndims(parent)))"))

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

similar(V::SubArray, T::Type, dims::Dims) = similar(V.parent, T, dims)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(i->1:size(a,i), ndims(a))

## SubArray creation
# Drops singleton dimensions (those indexed with a scalar)
function slice{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_slice(A, I...)
end
function slice(A::AbstractArray, i::ViewIndex)
    @_inline_meta
    @boundscheck checkbounds(A, i)
    unsafe_slice(reshape(A, Val{1}), i)
end
function slice{N}(A::AbstractArray, I::Vararg{ViewIndex,N}) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_slice(reshape(A, Val{N}), I...)
end
function unsafe_slice{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    J = to_indexes(I...)
    SubArray(A, J, index_shape(A, J...))
end

keep_leading_scalars(T::Tuple{}) = T
keep_leading_scalars(T::Tuple{Real, Vararg{Real}}) = T
keep_leading_scalars(T::Tuple{Real, Vararg{Any}}) = (@_inline_meta; (NoSlice(T[1]), keep_leading_scalars(tail(T))...))
keep_leading_scalars(T::Tuple{Any, Vararg{Any}}) = (@_inline_meta; (T[1], keep_leading_scalars(tail(T))...))

function sub{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_sub(A, I...)
end
function sub(A::AbstractArray, i::ViewIndex)
    @_inline_meta
    @boundscheck checkbounds(A, i)
    unsafe_sub(reshape(A, Val{1}), i)
end
function sub{N}(A::AbstractArray, I::Vararg{ViewIndex,N}) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_sub(reshape(A, Val{N}), I...)
end
function unsafe_sub{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    J = keep_leading_scalars(to_indexes(I...))
    SubArray(A, J, index_shape(A, J...))
end

# Re-indexing is the heart of a view, transforming A[i, j][x, y] to A[i[x], j[y]]
#
# Recursively look through the heads of the parent- and sub-indexes, considering
# the following cases:
# * Parent index is array  -> re-index that with one or more sub-indexes (one per dimension)
# * Parent index is Colon  -> just use the sub-index as provided
# * Parent index is scalar -> that dimension was dropped, so skip the sub-index and use the index as is

typealias AbstractZeroDimArray{T} AbstractArray{T, 0}
typealias DroppedScalar Union{Real, AbstractCartesianIndex}

reindex(V, ::Tuple{}, ::Tuple{}) = ()

# Skip dropped scalars, so simply peel them off the parent indices and continue
reindex(V, idxs::Tuple{DroppedScalar, Vararg{Any}}, subidxs::Tuple{Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1], reindex(V, tail(idxs), subidxs)...))

# Colons simply pass their subindexes straight through
reindex(V, idxs::Tuple{Colon, Vararg{Any}}, subidxs::Tuple{Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (subidxs[1], reindex(V, tail(idxs), tail(subidxs))...))

# Re-index into parent vectors with one subindex
reindex(V, idxs::Tuple{AbstractVector, Vararg{Any}}, subidxs::Tuple{Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]], reindex(V, tail(idxs), tail(subidxs))...))

# Parent matrices are re-indexed with two sub-indices
reindex(V, idxs::Tuple{AbstractMatrix, Vararg{Any}}, subidxs::Tuple{Any, Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1], subidxs[2]], reindex(V, tail(idxs), tail(tail(subidxs)))...))

# In general, we index N-dimensional parent arrays with N indices
@generated function reindex{T,N}(V, idxs::Tuple{AbstractArray{T,N}, Vararg{Any}}, subidxs::Tuple{Vararg{Any}})
    if length(subidxs.parameters) >= N
        subs = [:(subidxs[$d]) for d in 1:N]
        tail = [:(subidxs[$d]) for d in N+1:length(subidxs.parameters)]
        :(@_propagate_inbounds_meta; (idxs[1][$(subs...)], reindex(V, tail(idxs), ($(tail...),))...))
    else
        :(throw(ArgumentError("cannot re-index $(ndims(V)) dimensional SubArray with fewer than $(ndims(V)) indices\nThis should not occur; please submit a bug report.")))
    end
end

# In general, we simply re-index the parent indices by the provided ones
typealias SlowSubArray{T,N,P,I} SubArray{T,N,P,I,false}
function getindex{T,N}(V::SlowSubArray{T,N}, I::Vararg{Real,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = V.parent[reindex(V, V.indexes, to_indexes(I...))...]
    r
end
# Explicitly define scalar linear indexing -- this is needed so the nonscalar
# indexing methods don't take precedence here
function getindex(V::SlowSubArray, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[reindex(V, V.indexes, ind2sub(size(V), to_index(i)))...]
    r
end

typealias FastSubArray{T,N,P,I} SubArray{T,N,P,I,true}
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
# Just like the slow case, explicitly define scalar indexing at N dims, too
function getindex{T,N}(V::FastSubArray{T,N}, I::Vararg{Real,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = getindex(V, sub2ind(size(V), to_indexes(I...)...))
    r
end

# Nonscalar indexing just copies a view
getindex{T,N}(V::SubArray{T,N}, i::ViewIndex) = (@_propagate_inbounds_meta; copy(slice(V, i)))
getindex{T,N}(V::SubArray{T,N}, I::Vararg{ViewIndex,N}) = (@_propagate_inbounds_meta; copy(slice(V, I...)))

# Setindex is similar, but since we don't specially define non-scalar methods
# we only need to define the canonical methods. We don't need to worry about
# e.g., linear indexing for SlowSubArray since the fallbacks can do their thing
function setindex!{T,N}(V::SlowSubArray{T,N}, x, I::Vararg{Real,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds V.parent[reindex(V, V.indexes, to_indexes(I...))...] = x
    V
end
function setindex!(V::FastSubArray, x, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.first_index + V.stride1*(to_index(i)-1)] = x
    V
end
function setindex!(V::FastContiguousSubArray, x, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.first_index + to_index(i)-1] = x
    V
end
# Nonscalar setindex! falls back to the defaults

function unsafe_slice{T,N}(V::SubArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    idxs = reindex(V, V.indexes, to_indexes(I...))
    SubArray(V.parent, idxs, index_shape(V.parent, idxs...))
end

function unsafe_sub{T,N}(V::SubArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    idxs = reindex(V, V.indexes, keep_leading_scalars(to_indexes(I...)))
    SubArray(V.parent, idxs, index_shape(V.parent, idxs...))
end

linearindexing{T<:FastSubArray}(::Type{T}) = LinearFast()
linearindexing{T<:SubArray}(::Type{T}) = LinearSlow()

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


unsafe_convert{T,N,P,I<:Tuple{Vararg{Union{RangeIndex, NoSlice}}}}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    unsafe_convert(Ptr{T}, V.parent) + (first_index(V)-1)*sizeof(T)

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
