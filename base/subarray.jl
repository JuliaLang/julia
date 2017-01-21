# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractCartesianIndex{N} # This is a hacky forward declaration for CartesianIndex
typealias ViewIndex Union{Real, AbstractArray}
typealias ScalarIndex Real

# L is true if the view itself supports fast linear indexing
immutable SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indexes::I
    offset1::Int       # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
    function SubArray(parent, indexes, offset1, stride1)
        @_inline_meta
        check_parent_index_match(parent, indexes)
        new(parent, indexes, offset1, stride1)
    end
end
# Compute the linear indexability of the indices, and combine it with the linear indexing of the parent
function SubArray(parent::AbstractArray, indexes::Tuple)
    @_inline_meta
    SubArray(linearindexing(viewindexing(indexes), linearindexing(parent)), parent, ensure_indexable(indexes), index_dimsum(indexes...))
end
function SubArray{P, I, N}(::LinearSlow, parent::P, indexes::I, ::NTuple{N,Any})
    @_inline_meta
    SubArray{eltype(P), N, P, I, false}(parent, indexes, 0, 0)
end
function SubArray{P, I, N}(::LinearFast, parent::P, indexes::I, ::NTuple{N,Any})
    @_inline_meta
    # Compute the stride and offset
    stride1 = compute_stride1(parent, indexes)
    SubArray{eltype(P), N, P, I, true}(parent, indexes, compute_offset1(parent, stride1, indexes), stride1)
end

check_parent_index_match(parent, indexes) = check_parent_index_match(parent, index_ndims(indexes...))
check_parent_index_match{T,N}(parent::AbstractArray{T,N}, ::NTuple{N, Bool}) = nothing
check_parent_index_match{N}(parent, ::NTuple{N, Bool}) =
    throw(ArgumentError("number of indices ($N) must match the parent dimensionality ($(ndims(parent)))"))

# This computes the linear indexing compatability for a given tuple of indices
viewindexing() = LinearFast()
# Leading scalar indexes simply increase the stride
viewindexing(I::Tuple{ScalarIndex, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# Slices may begin a section which may be followed by any number of Slices
viewindexing(I::Tuple{Slice, Slice, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# A UnitRange can follow Slices, but only if all other indices are scalar
viewindexing(I::Tuple{Slice, UnitRange, Vararg{ScalarIndex}}) = LinearFast()
# In general, ranges are only fast if all other indices are scalar
viewindexing(I::Tuple{Union{Range, Slice}, Vararg{ScalarIndex}}) = LinearFast()
# All other index combinations are slow
viewindexing(I::Tuple{Vararg{Any}}) = LinearSlow()
# Of course, all other array types are slow
viewindexing(I::Tuple{AbstractArray, Vararg{Any}}) = LinearSlow()

# Simple utilities
size(V::SubArray) = (@_inline_meta; map(n->Int(unsafe_length(n)), indices(V)))

similar(V::SubArray, T::Type, dims::Dims) = similar(V.parent, T, dims)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
"""
    parentindexes(A)

From an array view `A`, returns the corresponding indexes in the parent.
"""
parentindexes(a::AbstractArray) = ntuple(i->OneTo(size(a,i)), ndims(a))

## SubArray creation
# We always assume that the dimensionality of the parent matches the number of
# indices that end up getting passed to it, so we store the parent as a
# ReshapedArray view if necessary. The trouble is that arrays of `CartesianIndex`
# can make the number of effective indices not equal to length(I).
_maybe_reshape_parent(A::AbstractArray, ::NTuple{1, Bool}) = reshape(A, Val{1})
_maybe_reshape_parent{_}(A::AbstractArray{_,1}, ::NTuple{1, Bool}) = reshape(A, Val{1})
_maybe_reshape_parent{_,N}(A::AbstractArray{_,N}, ::NTuple{N, Bool}) = A
_maybe_reshape_parent{N}(A::AbstractArray, ::NTuple{N, Bool}) = reshape(A, Val{N}) # TODO: DEPRECATE FOR #14770
"""
    view(A, inds...)

Like [`getindex`](@ref), but returns a view into the parent array `A` with the
given indices instead of making a copy.  Calling [`getindex`](@ref) or
[`setindex!`](@ref) on the returned `SubArray` computes the
indices to the parent array on the fly without checking bounds.
"""
function view(A::AbstractArray, I...)
    @_inline_meta
    J = to_indices(A, I)
    @boundscheck checkbounds(A, J...)
    unsafe_view(_maybe_reshape_parent(A, index_ndims(J...)), J...)
end

function unsafe_view(A::AbstractArray, I::ViewIndex...)
    @_inline_meta
    SubArray(A, I)
end
# When we take the view of a view, it's often possible to "reindex" the parent
# view's indices such that we can "pop" the parent view and keep just one layer
# of indirection. But we can't always do this because arrays of `CartesianIndex`
# might span multiple parent indices, making the reindex calculation very hard.
# So we use _maybe_reindex to figure out if there are any arrays of
# `CartesianIndex`, and if so, we punt and keep two layers of indirection.
unsafe_view(V::SubArray, I::ViewIndex...) = (@_inline_meta; _maybe_reindex(V, I))
_maybe_reindex(V, I) = (@_inline_meta; _maybe_reindex(V, I, I))
_maybe_reindex{C<:AbstractCartesianIndex}(V, I, ::Tuple{AbstractArray{C}, Vararg{Any}}) =
    (@_inline_meta; SubArray(V, I))
# But allow arrays of CartesianIndex{1}; they behave just like arrays of Ints
_maybe_reindex{C<:AbstractCartesianIndex{1}}(V, I, A::Tuple{AbstractArray{C}, Vararg{Any}}) =
    (@_inline_meta; _maybe_reindex(V, I, tail(A)))
_maybe_reindex(V, I, A::Tuple{Any, Vararg{Any}}) = (@_inline_meta; _maybe_reindex(V, I, tail(A)))
function _maybe_reindex(V, I, ::Tuple{})
    @_inline_meta
    idxs = to_indices(V.parent, reindex(V, V.indexes, I))
    SubArray(V.parent, idxs)
end

## Re-indexing is the heart of a view, transforming A[i, j][x, y] to A[i[x], j[y]]
#
# Recursively look through the heads of the parent- and sub-indexes, considering
# the following cases:
# * Parent index is array  -> re-index that with one or more sub-indexes (one per dimension)
# * Parent index is Colon  -> just use the sub-index as provided
# * Parent index is scalar -> that dimension was dropped, so skip the sub-index and use the index as is

typealias AbstractZeroDimArray{T} AbstractArray{T, 0}

reindex(V, ::Tuple{}, ::Tuple{}) = ()

# Skip dropped scalars, so simply peel them off the parent indices and continue
reindex(V, idxs::Tuple{ScalarIndex, Vararg{Any}}, subidxs::Tuple{Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1], reindex(V, tail(idxs), subidxs)...))

# Slices simply pass their subindexes straight through
reindex(V, idxs::Tuple{Slice, Vararg{Any}}, subidxs::Tuple{Any, Vararg{Any}}) =
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
function getindex{T,N}(V::SlowSubArray{T,N}, I::Vararg{Int,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = V.parent[reindex(V, V.indexes, I)...]
    r
end

typealias FastSubArray{T,N,P,I} SubArray{T,N,P,I,true}
function getindex(V::FastSubArray, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + V.stride1*i]
    r
end
# We can avoid a multiplication if the first parent index is a Colon or UnitRange
typealias FastContiguousSubArray{T,N,P,I<:Tuple{Union{Slice, UnitRange}, Vararg{Any}}} SubArray{T,N,P,I,true}
function getindex(V::FastContiguousSubArray, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + i]
    r
end

function setindex!{T,N}(V::SlowSubArray{T,N}, x, I::Vararg{Int,N})
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds V.parent[reindex(V, V.indexes, I)...] = x
    V
end
function setindex!(V::FastSubArray, x, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + V.stride1*i] = x
    V
end
function setindex!(V::FastContiguousSubArray, x, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + i] = x
    V
end

linearindexing{T<:FastSubArray}(::Type{T}) = LinearFast()
linearindexing{T<:SubArray}(::Type{T}) = LinearSlow()

# Strides are the distance between adjacent elements in a given dimension,
# so they are well-defined even for non-linear memory layouts
strides{T,N,P,I}(V::SubArray{T,N,P,I}) = substrides(V.parent, V.indexes)

substrides(parent, I::Tuple) = substrides(1, parent, 1, I)
substrides(s, parent, dim, ::Tuple{}) = ()
substrides(s, parent, dim, I::Tuple{ScalarIndex, Vararg{Any}}) = (substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Slice, Vararg{Any}}) = (s, substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Range, Vararg{Any}}) = (s*step(I[1]), substrides(s*size(parent, dim), parent, dim+1, tail(I))...)
substrides(s, parent, dim, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("strides is invalid for SubArrays with indices of type $(typeof(I[1]))"))

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

compute_stride1{N}(parent::AbstractArray, I::NTuple{N,Any}) =
    (@_inline_meta; compute_stride1(1, fill_to_length(indices(parent), OneTo(1), Val{N}), I))
compute_stride1(s, inds, I::Tuple{}) = s
compute_stride1(s, inds, I::Tuple{ScalarIndex, Vararg{Any}}) =
    (@_inline_meta; compute_stride1(s*unsafe_length(inds[1]), tail(inds), tail(I)))
compute_stride1(s, inds, I::Tuple{Range, Vararg{Any}}) = s*step(I[1])
compute_stride1(s, inds, I::Tuple{Slice, Vararg{Any}}) = s
compute_stride1(s, inds, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("invalid strided index type $(typeof(I[1]))"))

iscontiguous(A::SubArray) = iscontiguous(typeof(A))
iscontiguous{S<:SubArray}(::Type{S}) = false
iscontiguous{F<:FastContiguousSubArray}(::Type{F}) = true

first_index(V::FastSubArray) = V.offset1 + V.stride1 # cached for fast linear SubArrays
function first_index(V::SubArray)
    P, I = parent(V), V.indexes
    s1 = compute_stride1(P, I)
    s1 + compute_offset1(P, s1, I)
end

# Computing the first index simply steps through the indices, accumulating the
# sum of index each multiplied by the parent's stride.
# The running sum is `f`; the cumulative stride product is `s`.
# If the result is one-dimensional and it's a Colon, then linear
# indexing uses the indices along the given dimension. Otherwise
# linear indexing always starts with 1.
compute_offset1(parent, stride1::Integer, I::Tuple) =
    (@_inline_meta; compute_offset1(parent, stride1, find_extended_dims(I)..., I))
compute_offset1(parent, stride1::Integer, dims::Tuple{Int}, inds::Tuple{Slice}, I::Tuple) =
    (@_inline_meta; compute_linindex(parent, I) - stride1*first(indices(parent, dims[1])))  # index-preserving case
compute_offset1(parent, stride1::Integer, dims, inds, I::Tuple) =
    (@_inline_meta; compute_linindex(parent, I) - stride1)  # linear indexing starts with 1

function compute_linindex{N}(parent, I::NTuple{N,Any})
    @_inline_meta
    IP = fill_to_length(indices(parent), OneTo(1), Val{N})
    compute_linindex(1, 1, IP, I)
end
function compute_linindex(f, s, IP::Tuple, I::Tuple{ScalarIndex, Vararg{Any}})
    @_inline_meta
    Δi = I[1]-first(IP[1])
    compute_linindex(f + Δi*s, s*unsafe_length(IP[1]), tail(IP), tail(I))
end
function compute_linindex(f, s, IP::Tuple, I::Tuple{Any, Vararg{Any}})
    @_inline_meta
    Δi = first(I[1])-first(IP[1])
    compute_linindex(f + Δi*s, s*unsafe_length(IP[1]), tail(IP), tail(I))
end
compute_linindex(f, s, IP::Tuple, I::Tuple{}) = f

find_extended_dims(I) = (@_inline_meta; _find_extended_dims((), (), 1, I...))
_find_extended_dims(dims, inds, dim) = dims, inds
_find_extended_dims(dims, inds, dim, ::ScalarIndex, I...) =
    (@_inline_meta; _find_extended_dims(dims, inds, dim+1, I...))
_find_extended_dims(dims, inds, dim, i1, I...) =
    (@_inline_meta; _find_extended_dims((dims..., dim), (inds..., i1), dim+1, I...))

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

# indices are taken from the range/vector
# Since bounds-checking is performance-critical and uses
# indices, it's worth optimizing these implementations thoroughly
indices(S::SubArray) = (@_inline_meta; _indices_sub(S, S.indexes...))
_indices_sub(S::SubArray) = ()
_indices_sub(S::SubArray, ::Real, I...) = (@_inline_meta; _indices_sub(S, I...))
function _indices_sub(S::SubArray, i1::AbstractArray, I...)
    @_inline_meta
    (unsafe_indices(i1)..., _indices_sub(S, I...)...)
end

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
        if j <= nd && (isa(r,Union{Slice,Range}) ? sp[i]*step(r) : sp[i]) == sv[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end

"""
    replace_ref_end!(ex)

Recursively replace occurrences of the symbol :end in a "ref" expression (i.e. A[...]) `ex`
with the appropriate function calls (`endof`, `size` or `trailingsize`). Replacement uses
the closest enclosing ref, so

    A[B[end]]

should transform to

    A[B[endof(B)]]

"""
function replace_ref_end!(ex,withex=nothing)
    if isa(ex,Symbol) && ex == :end
        withex === nothing && error("Invalid use of end")
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
an assignment (e.g. `@view(A[1,2:end]) = ...`).  See also [`@views`](@ref)
to switch an entire block of code to use views for slicing.
"""
macro view(ex)
    if isa(ex, Expr) && ex.head == :ref
        ex = replace_ref_end!(ex)
        Expr(:&&, true, esc(Expr(:call,:(Base.view),ex.args...)))
    else
        throw(ArgumentError("Invalid use of @view macro: argument must be a reference expression A[...]."))
    end
end

############################################################################
# @views macro code:

# maybeview is like getindex, but returns a view for slicing operations
# (while remaining equivalent to getindex for scalar indices and non-array types)
@propagate_inbounds maybeview(A, args...) = getindex(A, args...)
@propagate_inbounds maybeview(A::AbstractArray, args...) = view(A, args...)
@propagate_inbounds maybeview(A::AbstractArray, args::Number...) = getindex(A, args...)
@propagate_inbounds maybeview(A) = getindex(A)
@propagate_inbounds maybeview(A::AbstractArray) = getindex(A)

_views(x) = x
_views(x::Symbol) = esc(x)
function _views(ex::Expr)
    if ex.head in (:(=), :(.=))
        # don't use view on the lhs of an assignment
        Expr(ex.head, esc(ex.args[1]), _views(ex.args[2]))
    elseif ex.head == :ref
        ex = replace_ref_end!(ex)
        Expr(:call, :maybeview, _views.(ex.args)...)
    else
        h = string(ex.head)
        if last(h) == '='
            # don't use view on the lhs of an op-assignment
            Expr(first(h) == '.' ? :(.=) : :(=), esc(ex.args[1]),
                 Expr(:call, esc(Symbol(h[1:end-1])), _views.(ex.args)...))
        else
            Expr(ex.head, _views.(ex.args)...)
        end
    end
end

"""
    @views expression

Convert every array-slicing operation in the given expression
(which may be a `begin`/`end` block, loop, function, etc.)
to return a view.   Scalar indices, non-array types, and
explicit `getindex` calls (as opposed to `array[...]`) are
unaffected.

Note that the `@views` macro only affects `array[...]` expressions
that appear explicitly in the given `expression`, not array slicing that
occurs in functions called by that code.
"""
macro views(x)
    _views(x)
end
