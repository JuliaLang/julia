# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias NonSliceIndex Union{Colon, AbstractArray}
typealias ViewIndex Union{Real, NonSliceIndex}
abstract AbstractCartesianIndex{N} # This is a hacky forward declaration for CartesianIndex

# L is true if the view itself supports fast linear indexing
immutable SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
    offset1::Int       # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
    function SubArray(parent, indexes, dims, offset1, stride1)
        check_parent_index_match(parent, indexes)
        new(parent, indexes, dims, offset1, stride1)
    end
end
# Compute the linear indexability of the indices, and combine it with the linear indexing of the parent
function SubArray(parent::AbstractArray, indexes::Tuple, dims::Tuple)
    SubArray(linearindexing(viewindexing(indexes), linearindexing(parent)), parent, indexes, map(dimlength, dims))
end
function SubArray{P, I, N}(::LinearSlow, parent::P, indexes::I, dims::NTuple{N, Int})
    SubArray{eltype(P), N, P, I, false}(parent, indexes, dims, 0, 0)
end
function SubArray{P, I, N}(::LinearFast, parent::P, indexes::I, dims::NTuple{N, Int})
    # Compute the stride and offset
    stride1 = compute_stride1(parent, indexes)
    SubArray{eltype(P), N, P, I, true}(parent, indexes, dims, compute_offset1(parent, stride1, indexes), stride1)
end

check_parent_index_match{T,N}(parent::AbstractArray{T,N}, indexes::NTuple{N}) = nothing
check_parent_index_match(parent, indexes) = throw(ArgumentError("number of indices ($(length(indexes))) must match the parent dimensionality ($(ndims(parent)))"))

# This computes the linear indexing compatability for a given tuple of indices
viewindexing() = LinearFast()
# Leading scalar indexes simply increase the stride
viewindexing(I::Tuple{Real, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# Colons may begin a section which may be followed by any number of Colons
viewindexing(I::Tuple{Colon, Colon, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# A UnitRange can follow Colons, but only if all other indices are scalar
viewindexing(I::Tuple{Colon, UnitRange, Vararg{Real}}) = LinearFast()
# In general, ranges are only fast if all other indices are scalar
viewindexing(I::Tuple{Union{Range, Colon}, Vararg{Real}}) = LinearFast()
# All other index combinations are slow
viewindexing(I::Tuple{Vararg{Any}}) = LinearSlow()
# Of course, all other array types are slow
viewindexing(I::Tuple{AbstractArray, Vararg{Any}}) = LinearSlow()

dimlength(r::Range) = length(r)
dimlength(i::Integer) = Int(i)

# Simple utilities
size(V::SubArray) = V.dims
length(V::SubArray) = prod(V.dims)

similar(V::SubArray, T::Type, dims::Dims) = similar(V.parent, T, dims)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(i->OneTo(size(a,i)), ndims(a))

## SubArray creation
# Drops singleton dimensions (those indexed with a scalar)
function view{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_view(A, I...)
end
function view(A::AbstractArray, i::ViewIndex)
    @_inline_meta
    @boundscheck checkbounds(A, i)
    unsafe_view(reshape(A, Val{1}), i)
end
function view{N}(A::AbstractArray, I::Vararg{ViewIndex,N}) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    unsafe_view(reshape(A, Val{N}), I...)
end

function unsafe_view{T,N}(A::AbstractArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    J = to_indexes(I...)
    SubArray(A, J, map(dimlength, index_shape(A, J...)))
end
function unsafe_view{T,N}(V::SubArray{T,N}, I::Vararg{ViewIndex,N})
    @_inline_meta
    idxs = reindex(V, V.indexes, to_indexes(I...))
    SubArray(V.parent, idxs, map(dimlength, (index_shape(V.parent, idxs...))))
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

typealias FastSubArray{T,N,P,I} SubArray{T,N,P,I,true}
function getindex(V::FastSubArray, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + V.stride1*to_index(i)]
    r
end
# We can avoid a multiplication if the first parent index is a Colon or UnitRange
typealias FastContiguousSubArray{T,N,P,I<:Tuple{Union{Colon, UnitRange}, Vararg{Any}}} SubArray{T,N,P,I,true}
function getindex(V::FastContiguousSubArray, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + to_index(i)]
    r
end

function setindex!{T,N}(V::SlowSubArray{T,N}, x, I::Vararg{Real,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds V.parent[reindex(V, V.indexes, to_indexes(I...))...] = x
    V
end
function setindex!(V::FastSubArray, x, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + V.stride1*to_index(i)] = x
    V
end
function setindex!(V::FastContiguousSubArray, x, i::Real)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + to_index(i)] = x
    V
end

linearindexing{T<:FastSubArray}(::Type{T}) = LinearFast()
linearindexing{T<:SubArray}(::Type{T}) = LinearSlow()

getindex(::Colon, i) = to_index(i)
unsafe_getindex(::Colon, i) = to_index(i)

step(::Colon) = 1
isempty(::Colon) = false
in(::Integer, ::Colon) = true

# Strides are the distance between adjacent elements in a given dimension,
# so they are well-defined even for non-linear memory layouts
strides{T,N,P,I}(V::SubArray{T,N,P,I}) = substrides(V.parent, V.indexes)

substrides(parent, I::Tuple) = substrides(1, parent, 1, I)
substrides(s, parent, dim, ::Tuple{}) = ()
substrides(s, parent, dim, I::Tuple{Real, Vararg{Any}}) = (substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{AbstractCartesianIndex, Vararg{Any}}) = substrides(s, parent, dim, (I[1].I..., tail(I)...))
substrides(s, parent, dim, I::Tuple{Colon, Vararg{Any}}) = (s, substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Range, Vararg{Any}}) = (s*step(I[1]), substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("strides is invalid for SubArrays with indices of type $(typeof(I[1]))"))

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

compute_stride1(parent, I::Tuple) = compute_stride1(1, parent, 1, I)
compute_stride1(s, parent, dim, I::Tuple{}) = s
compute_stride1(s, parent, dim, I::Tuple{DroppedScalar, Vararg{Any}}) =
    (@_inline_meta; compute_stride1(s*size(parent, dim), parent, dim+1, tail(I)))
compute_stride1(s, parent, dim, I::Tuple{Range, Vararg{Any}}) = s*step(I[1])
compute_stride1(s, parent, dim, I::Tuple{Colon, Vararg{Any}}) = s
compute_stride1(s, parent, dim, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("invalid strided index type $(typeof(I[1]))"))

iscontiguous(A::SubArray) = iscontiguous(typeof(A))
iscontiguous{S<:SubArray}(::Type{S}) = false
iscontiguous{F<:FastContiguousSubArray}(::Type{F}) = true

# Fast linear SubArrays have their first index cached
first_index(V::FastSubArray) = V.offset1 + V.stride1
first_index(V::SubArray) = first_index(V.parent, V.indexes)
function first_index(P::AbstractArray, indexes::Tuple)
    f = first(linearindices(P))
    s = 1
    for i = 1:length(indexes)
        f += (_first(indexes[i], P, i)-first(indices(P, i)))*s
        s *= size(P, i)
    end
    f
end
_first(::Colon, P, ::Colon) = first(linearindices(P))
_first(i, P, ::Colon) = first(i)
_first(::Colon, P, d) = first(indices(P, d))
_first(i, P, d) = first(i)

# Computing the first index simply steps through the indices, accumulating the
# sum of index each multiplied by the parent's stride.
# The running sum is `f`; the cumulative stride product is `s`.
# If the result is one-dimensional and it's a Colon, then linear
# indexing uses the indices along the given dimension. Otherwise
# linear indexing always starts with 1.
compute_offset1(parent, stride1::Integer, I::Tuple) = (@_inline_meta; compute_offset1(parent, stride1, find_extended_dims(I)..., I))
compute_offset1(parent, stride1::Integer, dims::Tuple{Int}, inds::Tuple{Colon}, I::Tuple) = compute_linindex(parent, I) - stride1*first(indices(parent, dims[1]))  # index-preserving case
compute_offset1(parent, stride1::Integer, dims, inds, I::Tuple) = compute_linindex(parent, I) - stride1  # linear indexing starts with 1

compute_linindex(parent, I) = compute_linindex(1, 1, parent, 1, I)
compute_linindex(f, s, parent, dim, I::Tuple{Real, Vararg{Any}}) =
    (@_inline_meta; compute_linindex(f + (I[1]-first(indices(parent,dim)))*s, s*size(parent, dim), parent, dim+1, tail(I)))
# Just splat out the cartesian indices and continue
compute_linindex(f, s, parent, dim, I::Tuple{AbstractCartesianIndex, Vararg{Any}}) =
    (@_inline_meta; compute_linindex(f, s, parent, dim, (I[1].I..., tail(I)...)))
compute_linindex(f, s, parent, dim, I::Tuple{Colon, Vararg{Any}}) =
    (@_inline_meta; compute_linindex(f, s*size(parent, dim), parent, dim+1, tail(I)))
compute_linindex(f, s, parent, dim, I::Tuple{Any, Vararg{Any}}) =
    (@_inline_meta; compute_linindex(f + (first(I[1])-first(indices(parent,dim)))*s, s*size(parent, dim), parent, dim+1, tail(I)))
compute_linindex(f, s, parent, dim, I::Tuple{}) = f

find_extended_dims(I) = (@_inline_meta; _find_extended_dims((), (), 1, I...))
_find_extended_dims(dims, inds, dim) = dims, inds
_find_extended_dims(dims, inds, dim, ::Real, I...) = _find_extended_dims(dims, inds, dim+1, I...)
_find_extended_dims(dims, inds, dim, i1::AbstractCartesianIndex, I...) = _find_extended_dims(dims, inds, dim, i1.I..., I...)
_find_extended_dims(dims, inds, dim, i1, I...) = _find_extended_dims((dims..., dim), (inds..., i1), dim+1, I...)

unsafe_convert{T,N,P,I<:Tuple{Vararg{RangeIndex}}}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    unsafe_convert(Ptr{T}, V.parent) + (first_index(V)-1)*sizeof(T)

pointer(V::FastSubArray, i::Int) = pointer(V.parent, V.offset1 + V.stride1*i)
pointer(V::FastContiguousSubArray, i::Int) = pointer(V.parent, V.offset1 + i)
pointer(V::SubArray, i::Int) = _pointer(V, i)
_pointer{T}(V::SubArray{T,1}, i::Int) = pointer(V, (i,))
_pointer(V::SubArray, i::Int) = pointer(V, ind2sub(indices(V), i))

function pointer{T,N,P<:Array,I<:Tuple{Vararg{RangeIndex}}}(V::SubArray{T,N,P,I}, is::Tuple{Vararg{Int}})
    index = first_index(V)
    strds = strides(V)
    for d = 1:length(is)
        index += (is[d]-1)*strds[d]
    end
    return pointer(V.parent, index)
end

# indices of the parent are preserved for ::Colon indices, otherwise
# they are taken from the range/vector
# Since bounds-checking is performance-critical and uses
# indices, it's worth optimizing these implementations thoroughly
indices(S::SubArray, d::Integer) = 1 <= d <= ndims(S) ? indices(S)[d] : (d > ndims(S) ? OneTo(1) : error("dimension $d out of range"))
indices(S::SubArray) = (@_inline_meta; _indices_sub(S, 1, S.indexes...))
_indices_sub(S::SubArray, dim::Int) = ()
_indices_sub(S::SubArray, dim::Int, ::Real, I...) = (@_inline_meta; _indices_sub(S, dim+1, I...))
_indices_sub(S::SubArray, dim::Int, ::Colon, I...) = (@_inline_meta; (indices(parent(S), dim), _indices_sub(S, dim+1, I...)...))
_indices_sub(S::SubArray, dim::Int, i1::AbstractArray, I...) = (@_inline_meta; (indices(i1)..., _indices_sub(S, dim+1, I...)...))
indices1{T}(S::SubArray{T,0}) = OneTo(1)
indices1(S::SubArray) = (@_inline_meta; _indices1(S, 1, S.indexes...))
_indices1(S::SubArray, dim, i1::Real, I...) = (@_inline_meta; _indices1(S, dim+1, I...))
_indices1(S::SubArray, dim, i1::Colon, I...) = (@_inline_meta; indices(parent(S), dim))
_indices1(S::SubArray, dim, i1::AbstractArray, I...) = (@_inline_meta; indices1(i1))

# Moreover, incides(S) is fast but indices(S, d) is slower
indicesperformance{T<:SubArray}(::Type{T}) = IndicesSlow1D()

## Compatability
# deprecate?
function parentdims(s::SubArray)
    nd = ndims(s)
    dimindex = Array{Int}(nd)
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

"""
    replace_ref_end!(ex)

Recursively replace occurences of the symbol :end in a "ref" expression (i.e. A[...]) `ex`
with the appropriate function calls (`endof`, `size` or `trailingsize`). Replacement uses
the closest enclosing ref, so

    A[B[end]]

should transform to

    A[B[endof(B)]]

"""
function replace_ref_end!(ex,withex=nothing)
    if isa(ex,Symbol) && ex == :end
        withex == nothing && error("Invalid use of end")
        return withex
    elseif isa(ex,Expr)
        if ex.head == :ref
            S = ex.args[1] = replace_ref_end!(ex.args[1],withex)
            # new :ref, so redefine withex
            nargs = length(ex.args)-1
            if nargs == 0
                return ex
            elseif nargs == 1
                # replace with endof(S)
                ex.args[2] = replace_ref_end!(ex.args[2],:(Base.endof($S)))
            else
                n = 1
                J = endof(ex.args)
                for j = 2:J-1
                    exj = ex.args[j] = replace_ref_end!(ex.args[j],:(Base.size($S,$n)))
                    if isa(exj,Expr) && exj.head == :...
                        # splatted object
                        exjs = exj.args[1]
                        n = :($n + length($exjs))
                    elseif isa(n, Expr)
                        # previous expression splatted
                        n = :($n + 1)
                    else
                        # an integer
                        n += 1
                    end
                end
                ex.args[J] = replace_ref_end!(ex.args[J],:(Base.trailingsize($S,$n)))
            end
        else
            # recursive search
            for i = eachindex(ex.args)
                ex.args[i] = replace_ref_end!(ex.args[i],withex)
            end
        end
    end
    ex
end

"""
    @view A[inds...]

Creates a `SubArray` from an indexing expression. This can only be applied directly to a
reference expression (e.g. `@view A[1,2:end]`), and should *not* be used as the target of
an assignment (e.g. `@view(A[1,2:end]) = ...`).
"""
macro view(ex)
    if isa(ex, Expr) && ex.head == :ref
        ex = replace_ref_end!(ex)
        Expr(:&&, true, esc(Expr(:call,:(Base.view),ex.args...)))
    else
        throw(ArgumentError("Invalid use of @view macro: argument must be a reference expression A[...]."))
    end
end
