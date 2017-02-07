# This file is a part of Julia. License is MIT: http://julialang.org/license

### Multidimensional iterators
module IteratorsMD
    import Base: eltype, length, size, start, done, next, last, in, getindex,
                 setindex!, linearindexing, min, max, zero, one, isless, eachindex,
                 ndims, iteratorsize, convert

    importall ..Base.Operators
    import Base: simd_outer_range, simd_inner_length, simd_index
    using Base: LinearFast, LinearSlow, AbstractCartesianIndex, fill_to_length, tail

    export CartesianIndex, CartesianRange

    # CartesianIndex
    immutable CartesianIndex{N} <: AbstractCartesianIndex{N}
        I::NTuple{N,Int}
        CartesianIndex{N}(index::NTuple{N,Integer}) where N = new(index)
    end

    CartesianIndex(index::NTuple{N,Integer}) where N = CartesianIndex{N}(index)
    CartesianIndex(index::Integer...) = CartesianIndex(index)
    CartesianIndex{N}(index::Vararg{Integer,N}) where N = CartesianIndex{N}(index)
    # Allow passing tuples smaller than N
    CartesianIndex{N}(index::Tuple) where N = CartesianIndex{N}(fill_to_length(index, 1, Val{N}))
    CartesianIndex{N}(index::Integer...) where N = CartesianIndex{N}(index)
    CartesianIndex{N}() where N = CartesianIndex{N}(())
    # Un-nest passed CartesianIndexes
    CartesianIndex(index::Union{Integer, CartesianIndex}...) = CartesianIndex(flatten(index))
    flatten(I::Tuple{}) = I
    flatten(I::Tuple{Any}) = I
    flatten{N}(I::Tuple{CartesianIndex{N}}) = I[1].I
    @inline flatten(I) = _flatten(I...)
    @inline _flatten() = ()
    @inline _flatten(i, I...)                 = (i, _flatten(I...)...)
    @inline _flatten(i::CartesianIndex, I...) = (i.I..., _flatten(I...)...)
    CartesianIndex(index::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = CartesianIndex(index...)

    # length
    length{N}(::CartesianIndex{N})=N
    length{N}(::Type{CartesianIndex{N}})=N

    # indexing
    getindex(index::CartesianIndex, i::Integer) = index.I[i]

    # zeros and ones
    zero{N}(::CartesianIndex{N}) = zero(CartesianIndex{N})
    zero{N}(::Type{CartesianIndex{N}}) = CartesianIndex(ntuple(x -> 0, Val{N}))
    one{N}(::CartesianIndex{N}) = one(CartesianIndex{N})
    one{N}(::Type{CartesianIndex{N}}) = CartesianIndex(ntuple(x -> 1, Val{N}))

    # arithmetic, min/max
    (-){N}(index::CartesianIndex{N}) = CartesianIndex{N}(map(-, index.I))
    (+){N}(index1::CartesianIndex{N}, index2::CartesianIndex{N}) =
        CartesianIndex{N}(map(+, index1.I, index2.I))
    (-){N}(index1::CartesianIndex{N}, index2::CartesianIndex{N}) =
        CartesianIndex{N}(map(-, index1.I, index2.I))
    min{N}(index1::CartesianIndex{N}, index2::CartesianIndex{N}) =
        CartesianIndex{N}(map(min, index1.I, index2.I))
    max{N}(index1::CartesianIndex{N}, index2::CartesianIndex{N}) =
        CartesianIndex{N}(map(max, index1.I, index2.I))

    (+){N}(index::CartesianIndex{N}, i::Integer) = CartesianIndex{N}(map(x->x+i, index.I))
    (+){N}(i::Integer, index::CartesianIndex{N}) = index+i
    (-){N}(index::CartesianIndex{N}, i::Integer) = CartesianIndex{N}(map(x->x-i, index.I))
    (-){N}(i::Integer, index::CartesianIndex{N}) = CartesianIndex{N}(map(x->i-x, index.I))
    (*){N}(a::Integer, index::CartesianIndex{N}) = CartesianIndex{N}(map(x->a*x, index.I))
    (*)(index::CartesianIndex,a::Integer)=*(a,index)

    # comparison
    @inline isless{N}(I1::CartesianIndex{N}, I2::CartesianIndex{N}) = _isless(0, I1.I, I2.I)
    @inline function _isless{N}(ret, I1::NTuple{N,Int}, I2::NTuple{N,Int})
        newret = ifelse(ret==0, icmp(I1[N], I2[N]), ret)
        _isless(newret, Base.front(I1), Base.front(I2))
    end
    _isless(ret, ::Tuple{}, ::Tuple{}) = ifelse(ret==1, true, false)
    icmp(a, b) = ifelse(isless(a,b), 1, ifelse(a==b, 0, -1))

    # Iteration
    immutable CartesianRange{I<:CartesianIndex}
        start::I
        stop::I
    end

    CartesianRange{N}(index::CartesianIndex{N}) = CartesianRange(one(index), index)
    CartesianRange(::Tuple{}) = CartesianRange{CartesianIndex{0}}(CartesianIndex{0}(()),CartesianIndex{0}(()))
    CartesianRange{N}(sz::NTuple{N,Int}) = CartesianRange(CartesianIndex(sz))
    CartesianRange{N}(rngs::NTuple{N,Union{Integer,AbstractUnitRange}}) =
        CartesianRange(CartesianIndex(map(first, rngs)), CartesianIndex(map(last, rngs)))

    convert{N}(::Type{NTuple{N,UnitRange{Int}}}, R::CartesianRange{CartesianIndex{N}}) =
        map((f,l)->f:l, first(R).I, last(R).I)
    convert{N}(::Type{NTuple{N,UnitRange}}, R::CartesianRange) =
        convert(NTuple{N,UnitRange{Int}}, R)
    convert{N}(::Type{Tuple{Vararg{UnitRange{Int}}}}, R::CartesianRange{CartesianIndex{N}}) =
        convert(NTuple{N,UnitRange{Int}}, R)
    convert(::Type{Tuple{Vararg{UnitRange}}}, R::CartesianRange) =
        convert(Tuple{Vararg{UnitRange{Int}}}, R)

    ndims(R::CartesianRange) = length(R.start)
    ndims{I<:CartesianIndex}(::Type{CartesianRange{I}}) = length(I)

    eachindex(::LinearSlow, A::AbstractArray) = CartesianRange(indices(A))

    @inline eachindex(::LinearSlow, A::AbstractArray, B::AbstractArray...) =
        CartesianRange(maxsize((), A, B...))
    maxsize(sz) = sz
    @inline maxsize(sz, A, B...) = maxsize(maxt(sz, size(A)), B...)
    @inline maxt(a::Tuple{}, b::Tuple{}) = ()
    @inline maxt(a::Tuple{}, b::Tuple)   = b
    @inline maxt(a::Tuple,   b::Tuple{}) = a
    @inline maxt(a::Tuple,   b::Tuple)   = (max(a[1], b[1]), maxt(tail(a), tail(b))...)

    eltype{I}(::Type{CartesianRange{I}}) = I
    iteratorsize{I}(::Type{CartesianRange{I}}) = Base.HasShape()

    @inline function start{I<:CartesianIndex}(iter::CartesianRange{I})
        if any(map(>, iter.start.I, iter.stop.I))
            return iter.stop+1
        end
        iter.start
    end
    @inline function next{I<:CartesianIndex}(iter::CartesianRange{I}, state)
        state, I(inc(state.I, iter.start.I, iter.stop.I))
    end
    # increment & carry
    @inline inc(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
    @inline inc(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int}) = (state[1]+1,)
    @inline function inc(state, start, stop)
        if state[1] < stop[1]
            return (state[1]+1,tail(state)...)
        end
        newtail = inc(tail(state), tail(start), tail(stop))
        (start[1], newtail...)
    end
    @inline done{I<:CartesianIndex}(iter::CartesianRange{I}, state) = state.I[end] > iter.stop.I[end]

    # 0-d cartesian ranges are special-cased to iterate once and only once
    start{I<:CartesianIndex{0}}(iter::CartesianRange{I}) = false
    next{I<:CartesianIndex{0}}(iter::CartesianRange{I}, state) = iter.start, true
    done{I<:CartesianIndex{0}}(iter::CartesianRange{I}, state) = state

    size{I<:CartesianIndex}(iter::CartesianRange{I}) = map(dimlength, iter.start.I, iter.stop.I)
    dimlength(start, stop) = stop-start+1

    length(iter::CartesianRange) = prod(size(iter))

    last(iter::CartesianRange) = iter.stop

    @inline function in{I<:CartesianIndex}(i::I, r::CartesianRange{I})
        _in(true, i.I, r.start.I, r.stop.I)
    end
    _in(b, ::Tuple{}, ::Tuple{}, ::Tuple{}) = b
    @inline _in(b, i, start, stop) = _in(b & (start[1] <= i[1] <= stop[1]), tail(i), tail(start), tail(stop))

    simd_outer_range(iter::CartesianRange{CartesianIndex{0}}) = iter
    function simd_outer_range{I}(iter::CartesianRange{I})
        start = CartesianIndex(tail(iter.start.I))
        stop  = CartesianIndex(tail(iter.stop.I))
        CartesianRange(start, stop)
    end

    simd_inner_length{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex) = 1
    simd_inner_length(iter::CartesianRange, I::CartesianIndex) = iter.stop[1]-iter.start[1]+1

    simd_index{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex, I1::Int) = iter.start
    @inline function simd_index{N}(iter::CartesianRange, Ilast::CartesianIndex{N}, I1::Int)
        CartesianIndex((I1+iter.start[1], Ilast.I...))
    end

    # Split out the first N elements of a tuple
    @inline split{N}(t, V::Type{Val{N}}) = _split((), t, V)
    @inline _split(tN, trest, V) = _split((tN..., trest[1]), tail(trest), V)
    # exit either when we've exhausted the input tuple or when tN has length N
    @inline _split{N}(tN::NTuple{N,Any}, ::Tuple{}, ::Type{Val{N}}) = tN, ()  # ambig.
    @inline _split{N}(tN,                ::Tuple{}, ::Type{Val{N}}) = tN, ()
    @inline _split{N}(tN::NTuple{N,Any},  trest,    ::Type{Val{N}}) = tN, trest
end  # IteratorsMD


using .IteratorsMD

## Bounds-checking with CartesianIndex
# Disallow linear indexing with CartesianIndex
function checkbounds(::Type{Bool}, A::AbstractArray, i::Union{CartesianIndex, AbstractArray{C} where C <: CartesianIndex})
    @_inline_meta
    checkbounds_indices(Bool, indices(A), (i,))
end

@inline checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, (), (I[1].I..., tail(I)...))
@inline checkbounds_indices(::Type{Bool}, IA::Tuple{Any}, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, IA, (I[1].I..., tail(I)...))
@inline checkbounds_indices(::Type{Bool}, IA::Tuple, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, IA, (I[1].I..., tail(I)...))

# Indexing into Array with mixtures of Integers and CartesianIndices is
# extremely performance-sensitive. While the abstract fallbacks support this,
# codegen has extra support for SIMDification that sub2ind doesn't (yet) support
@propagate_inbounds getindex(A::Array, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    A[to_indices(A, (i1, I...))...]
@propagate_inbounds setindex!(A::Array, v, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    (A[to_indices(A, (i1, I...))...] = v; A)

# Support indexing with an array of CartesianIndex{N}s
# Here we try to consume N of the indices (if there are that many available)
# The first two simply handle ambiguities
@inline function checkbounds_indices{N}(::Type{Bool}, ::Tuple{},
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}})
    checkindex(Bool, (), I[1]) & checkbounds_indices(Bool, (), tail(I))
end
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple{Any},
        I::Tuple{AbstractArray{CartesianIndex{0}},Vararg{Any}})
    checkbounds_indices(Bool, IA, tail(I))
end
@inline function checkbounds_indices{N}(::Type{Bool}, IA::Tuple{Any},
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}})
    checkindex(Bool, IA, I[1]) & checkbounds_indices(Bool, (), tail(I))
end
@inline function checkbounds_indices{N}(::Type{Bool}, IA::Tuple,
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}})
    IA1, IArest = IteratorsMD.split(IA, Val{N})
    checkindex(Bool, IA1, I[1]) & checkbounds_indices(Bool, IArest, tail(I))
end

function checkindex{N}(::Type{Bool}, inds::Tuple, I::AbstractArray{CartesianIndex{N}})
    b = true
    for i in I
        b &= checkbounds_indices(Bool, inds, (i,))
    end
    b
end

# combined count of all indices, including CartesianIndex and
# AbstractArray{CartesianIndex}
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferrable
@inline index_ndims(i1, I...) = (true, index_ndims(I...)...)
@inline function index_ndims{N}(i1::CartesianIndex{N}, I...)
    (map(x->true, i1.I)..., index_ndims(I...)...)
end
@inline function index_ndims{N}(i1::AbstractArray{CartesianIndex{N}}, I...)
    (ntuple(x->true, Val{N})..., index_ndims(I...)...)
end
index_ndims() = ()

# combined dimensionality of all indices
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferrable
@inline index_dimsum(i1, I...) = (index_dimsum(I...)...)
@inline index_dimsum(::Colon, I...) = (true, index_dimsum(I...)...)
@inline index_dimsum(::AbstractArray{Bool}, I...) = (true, index_dimsum(I...)...)
@inline function index_dimsum{_,N}(::AbstractArray{_,N}, I...)
    (ntuple(x->true, Val{N})..., index_dimsum(I...)...)
end
index_dimsum() = ()

# Recursively compute the lengths of a list of indices, without dropping scalars
index_lengths() = ()
@inline index_lengths(::Real, rest...) = (1, index_lengths(rest...)...)
@inline index_lengths(A::AbstractArray, rest...) = (length(A), index_lengths(rest...)...)
@inline index_lengths(A::Slice, rest...) = (length(indices1(A)), index_lengths(rest...)...)

# shape of array to create for getindex() with indexes I, dropping scalars
# returns a Tuple{Vararg{AbstractUnitRange}} of indices
index_shape() = ()
@inline index_shape(::Real, rest...) = index_shape(rest...)
@inline index_shape(A::AbstractArray, rest...) = (indices(A)..., index_shape(rest...)...)

"""
    LogicalIndex(mask)

The `LogicalIndex` type is a special vector that simply contains all indices I
where `mask[I]` is true. This specialized type does not support indexing
directly as doing so would require O(n) lookup time. `AbstractArray{Bool}` are
wrapped with `LogicalIndex` upon calling `to_indices`.
"""
immutable LogicalIndex{T, A<:AbstractArray{Bool}} <: AbstractVector{T}
    mask::A
    sum::Int
    LogicalIndex{T,A}(mask::A) where {T,A<:AbstractArray{Bool}} = new(mask, countnz(mask))
end
LogicalIndex(mask::AbstractVector{Bool}) = LogicalIndex{Int, typeof(mask)}(mask)
LogicalIndex(mask::AbstractArray{Bool, N}) where N = LogicalIndex{CartesianIndex{N}, typeof(mask)}(mask)
(::Type{LogicalIndex{Int}})(mask::AbstractArray) = LogicalIndex{Int, typeof(mask)}(mask)
size(L::LogicalIndex) = (L.sum,)
length(L::LogicalIndex) = L.sum
collect(L::LogicalIndex) = [i for i in L]
show(io::IO, r::LogicalIndex) = print(io, "Base.LogicalIndex(", r.mask, ")")
# Iteration over LogicalIndex is very performance-critical, but it also must
# support arbitrary AbstractArray{Bool}s with both Int and CartesianIndex.
# Thus the iteration state contains an index iterator and its state. We also
# keep track of the count of elements since we already know how many there
# should be -- this way we don't need to look at future indices to check done.
@inline function start(L::LogicalIndex{Int})
    r = linearindices(L.mask)
    return (r, start(r), 1)
end
@inline function start{C<:CartesianIndex}(L::LogicalIndex{C})
    r = CartesianRange(indices(L.mask))
    return (r, start(r), 1)
end
@inline function next(L::LogicalIndex, s)
    # We're looking for the n-th true element, using iterator r at state i
    r, i, n = s
    while true
        done(r, i) # Call done(r, i) for the iteration protocol, but trust done(L, s) was called
        idx, i = next(r, i)
        L.mask[idx] && return (idx, (r, i, n+1))
    end
end
done(L::LogicalIndex, s) = s[3] > length(L)
# When wrapping a BitArray, lean heavily upon its internals -- this is a common
# case. Just use the Int index and count as its state.
@inline start{B<:BitArray}(L::LogicalIndex{Int, B}) = (0, 1)
@inline function next{B<:BitArray}(L::LogicalIndex{Int, B}, s)
    i, n = s
    Bc = L.mask.chunks
    while true
        if Bc[_div64(i)+1] & (UInt64(1)<<_mod64(i)) != 0
            i += 1
            return (i, (i, n+1))
        end
        i += 1
    end
end
@inline done{B<:BitArray}(L::LogicalIndex{Int, B}, s) = s[2] > length(L)

# Checking bounds with LogicalIndex{Int} is tricky since we allow linear indexing over trailing dimensions
@inline checkbounds_indices{N}(::Type{Bool},IA::Tuple{},I::Tuple{LogicalIndex{Int,AbstractArray{Bool,N}}}) =
    checkindex(Bool, IA, I[1])
@inline checkbounds_indices{N}(::Type{Bool},IA::Tuple{Any},I::Tuple{LogicalIndex{Int,AbstractArray{Bool,N}}}) =
    checkindex(Bool, IA[1], I[1])
@inline function checkbounds_indices{N}(::Type{Bool}, IA::Tuple, I::Tuple{LogicalIndex{Int,AbstractArray{Bool,N}}})
    IA1, IArest = IteratorsMD.split(IA, Val{N})
    checkindex(Bool, IA1, I[1])
end
@inline checkbounds{T,B<:AbstractArray{Bool,1}}(::Type{Bool}, A::AbstractArray, I::LogicalIndex{T,B}) =
    linearindices(A) == linearindices(I.mask)
@inline checkbounds(::Type{Bool}, A::AbstractArray, I::LogicalIndex) = indices(A) == indices(I.mask)
@inline checkindex(::Type{Bool}, indx::AbstractUnitRange, I::LogicalIndex) = (indx,) == indices(I.mask)
checkindex(::Type{Bool}, inds::Tuple, I::LogicalIndex) = false

ensure_indexable(I::Tuple{}) = ()
@inline ensure_indexable(I::Tuple{Any, Vararg{Any}}) = (I[1], ensure_indexable(tail(I))...)
@inline ensure_indexable(I::Tuple{LogicalIndex, Vararg{Any}}) = (collect(I[1]), ensure_indexable(tail(I))...)

# In simple cases, we know that we don't need to use indices(A). Optimize those
# until Julia gets smart enough to elide the call on its own:
to_indices(A, I::Tuple{}) = ()
@inline to_indices(A, I::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = to_indices(A, (), I)
# But some index types require more context spanning multiple indices
# CartesianIndexes are simple; they just splat out
@inline to_indices(A, inds, I::Tuple{CartesianIndex, Vararg{Any}}) =
    to_indices(A, inds, (I[1].I..., tail(I)...))
# But for arrays of CartesianIndex, we just skip the appropriate number of inds
@inline function to_indices{N}(A, inds, I::Tuple{AbstractArray{CartesianIndex{N}}, Vararg{Any}})
    _, indstail = IteratorsMD.split(inds, Val{N})
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# And boolean arrays behave similarly; they also skip their number of dimensions
@inline function to_indices{N}(A, inds, I::Tuple{AbstractArray{Bool, N}, Vararg{Any}})
    _, indstail = IteratorsMD.split(inds, Val{N})
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# As an optimization, we allow trailing Array{Bool} and BitArray to be linear over trailing dimensions
@inline to_indices{N}(A, inds, I::Tuple{Union{Array{Bool,N}, BitArray{N}}}) =
    (_maybe_linear_logical_index(linearindexing(A), A, I[1]),)
_maybe_linear_logical_index(::LinearIndexing, A, i) = to_index(A, i)
_maybe_linear_logical_index(::LinearFast, A, i) = LogicalIndex{Int}(i)

# Colons get converted to slices by `uncolon`
@inline to_indices(A, inds, I::Tuple{Colon, Vararg{Any}}) =
    (uncolon(inds, I), to_indices(A, _maybetail(inds), tail(I))...)

typealias CI0 Union{CartesianIndex{0}, AbstractArray{CartesianIndex{0}}}
uncolon(inds::Tuple{},    I::Tuple{Colon})              = Slice(OneTo(1))
uncolon(inds::Tuple{},    I::Tuple{Colon, Vararg{Any}}) = Slice(OneTo(1))
uncolon(inds::Tuple{},    I::Tuple{Colon, Vararg{CI0}}) = Slice(OneTo(1))
uncolon(inds::Tuple{Any}, I::Tuple{Colon})              = Slice(inds[1])
uncolon(inds::Tuple{Any}, I::Tuple{Colon, Vararg{Any}}) = Slice(inds[1])
uncolon(inds::Tuple{Any}, I::Tuple{Colon, Vararg{CI0}}) = Slice(inds[1])
uncolon(inds::Tuple,      I::Tuple{Colon, Vararg{Any}}) = Slice(inds[1])
uncolon(inds::Tuple,      I::Tuple{Colon})              = Slice(OneTo(trailingsize(inds)))
uncolon(inds::Tuple,      I::Tuple{Colon, Vararg{CI0}}) = Slice(OneTo(trailingsize(inds)))

### From abstractarray.jl: Internal multidimensional indexing definitions ###
getindex(x::Number, i::CartesianIndex{0}) = x
getindex(t::Tuple,  i::CartesianIndex{1}) = getindex(t, i.I[1])

# These are not defined on directly on getindex to avoid
# ambiguities for AbstractArray subtypes. See the note in abstractarray.jl

@generated function _getindex(l::LinearIndexing, A::AbstractArray, I::Union{Real, AbstractArray}...)
    N = length(I)
    quote
        @_inline_meta
        @boundscheck checkbounds(A, I...)
        _unsafe_getindex(l, _maybe_reshape(l, A, I...), I...)
    end
end
# But we can speed up LinearSlow arrays by reshaping them to the appropriate dimensionality:
_maybe_reshape(::LinearFast, A::AbstractArray, I...) = A
_maybe_reshape(::LinearSlow, A::AbstractVector, I...) = A
@inline _maybe_reshape(::LinearSlow, A::AbstractArray, I...) = __maybe_reshape(A, index_ndims(I...))
@inline __maybe_reshape{T,N}(A::AbstractArray{T,N}, ::NTuple{N,Any}) = A
@inline __maybe_reshape{N}(A::AbstractArray, ::NTuple{N,Any}) = reshape(A, Val{N})

@generated function _unsafe_getindex(::LinearIndexing, A::AbstractArray, I::Union{Real, AbstractArray}...)
    N = length(I)
    quote
        # This is specifically not inlined to prevent exessive allocations in type unstable code
        @nexprs $N d->(I_d = I[d])
        shape = @ncall $N index_shape I
        dest = similar(A, shape)
        map(unsafe_length, indices(dest)) == map(unsafe_length, shape) || throw_checksize_error(dest, shape)
        @ncall $N _unsafe_getindex! dest A I
    end
end

# Always index with the exactly indices provided.
@generated function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Union{Real, AbstractArray}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        @nexprs $N d->(J_d = I[d])
        D = eachindex(dest)
        Ds = start(D)
        @inbounds @nloops $N j d->J_d begin
            d, Ds = next(D, Ds)
            dest[d] = @ncall $N getindex src j
        end
        dest
    end
end

@noinline throw_checksize_error(A, sz) = throw(DimensionMismatch("output array is the wrong size; expected $sz, got $(size(A))"))

## setindex! ##
@generated function _setindex!(l::LinearIndexing, A::AbstractArray, x, I::Union{Real, AbstractArray}...)
    N = length(I)
    quote
        @_inline_meta
        @boundscheck checkbounds(A, I...)
        _unsafe_setindex!(l, _maybe_reshape(l, A, I...), x, I...)
        A
    end
end

_iterable(v::AbstractArray) = v
_iterable(v) = Iterators.repeated(v)
@generated function _unsafe_setindex!(::LinearIndexing, A::AbstractArray, x, I::Union{Real,AbstractArray}...)
    N = length(I)
    quote
        X = _iterable(x)
        @nexprs $N d->(I_d = I[d])
        idxlens = @ncall $N index_lengths I
        @ncall $N setindex_shape_check X (d->idxlens[d])
        Xs = start(X)
        @inbounds @nloops $N i d->I_d begin
            v, Xs = next(X, Xs)
            @ncall $N setindex! A v i
        end
        A
    end
end

##

@generated function findn{T,N}(A::AbstractArray{T,N})
    quote
        nnzA = countnz(A)
        @nexprs $N d->(I_d = Array{Int}(nnzA))
        k = 1
        @nloops $N i A begin
            @inbounds if (@nref $N A i) != zero(T)
                @nexprs $N d->(I_d[k] = i_d)
                k += 1
            end
        end
        @ntuple $N I
    end
end


# see discussion in #18364 ... we try not to widen type of the resulting array
# from cumsum or cumprod, but in some cases (+, Bool) we may not have a choice.
rcum_promote_type{T,S<:Number}(op, ::Type{T}, ::Type{S}) = promote_op(op, T, S)
rcum_promote_type{T<:Number}(op, ::Type{T}) = rcum_promote_type(op, T,T)
rcum_promote_type{T}(op, ::Type{T}) = T

# handle sums of Vector{Bool} and similar.   it would be nice to handle
# any AbstractArray here, but it's not clear how that would be possible
rcum_promote_type{T,N}(op, ::Type{Array{T,N}}) = Array{rcum_promote_type(op,T), N}

# accumulate_pairwise slightly slower then accumulate, but more numerically
# stable in certain situations (e.g. sums).
# it does double the number of operations compared to accumulate,
# though for cheap operations like + this does not have much impact (20%)
function _accumulate_pairwise!{T, Op}(op::Op, c::AbstractVector{T}, v::AbstractVector, s, i1, n)::T
    @inbounds if n < 128
        s_ = v[i1]
        c[i1] = op(s, s_)
        for i = i1+1:i1+n-1
            s_ = op(s_, v[i])
            c[i] = op(s, s_)
        end
    else
        n2 = n >> 1
        s_ = _accumulate_pairwise!(op, c, v, s, i1, n2)
        s_ = op(s_, _accumulate_pairwise!(op, c, v, op(s, s_), i1+n2, n-n2))
    end
    return s_
end

function accumulate_pairwise!{Op}(op::Op, result::AbstractVector, v::AbstractVector)
    li = linearindices(v)
    li != linearindices(result) && throw(DimensionMismatch("input and output array sizes and indices must match"))
    n = length(li)
    n == 0 && return result
    i1 = first(li)
    @inbounds result[i1] = v1 = v[i1]
    n == 1 && return result
    _accumulate_pairwise!(op, result, v, v1, i1+1, n-1)
    return result
end

function accumulate_pairwise{T}(op, v::AbstractVector{T})
    out = similar(v, rcum_promote_type(op, T))
    return accumulate_pairwise!(op, out, v)
end

function cumsum!(out, v::AbstractVector, axis::Integer=1)
    # for types prone to numerical stability issues, we want
    # accumulate_pairwise.
    axis == 1 ? accumulate_pairwise!(+, out, v) : copy!(out,v)
end

function cumsum!{T <: Integer}(out, v::AbstractVector{T}, axis::Integer=1)
    axis == 1 ? accumulate!(+, out, v) : copy!(out,v)
end

"""
    cumsum(A, dim=1)

Cumulative sum along a dimension `dim` (defaults to 1). See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumsum(a,1)
2×3 Array{Int64,2}:
 1  2  3
 5  7  9

julia> cumsum(a,2)
2×3 Array{Int64,2}:
 1  3   6
 4  9  15
```
"""
function cumsum{T}(A::AbstractArray{T}, axis::Integer=1)
    out = similar(A, rcum_promote_type(+, T))
    cumsum!(out, A, axis)
end

"""
    cumsum!(B, A, dim::Integer=1)

Cumulative sum of `A` along a dimension, storing the result in `B`. The dimension defaults
to 1. See also [`cumsum`](@ref).
"""
cumsum!(B, A, axis::Integer=1) = accumulate!(+, B, A, axis)

"""
    cumprod(A, dim=1)

Cumulative product along a dimension `dim` (defaults to 1). See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumprod(a,1)
2×3 Array{Int64,2}:
 1   2   3
 4  10  18

julia> cumprod(a,2)
2×3 Array{Int64,2}:
 1   2    6
 4  20  120
```
"""
cumprod(A::AbstractArray, axis::Integer=1) = accumulate(*, A, axis)

"""
    cumprod!(B, A, dim::Integer=1)

Cumulative product of `A` along a dimension, storing the result in `B`. The dimension defaults to 1.
See also [`cumprod`](@ref).
"""
cumprod!(B, A, axis::Integer=1) = accumulate!(*, B, A, axis)

"""
    accumulate(op, A, dim=1)

Cumulative operation `op` along a dimension `dim` (defaults to 1). See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

```jldoctest
julia> accumulate(+, [1,2,3])
3-element Array{Int64,1}:
 1
 3
 6

julia> accumulate(*, [1,2,3])
3-element Array{Int64,1}:
 1
 2
 6
```
"""
function accumulate(op, A, axis::Integer=1)
    out = similar(A, rcum_promote_type(op, eltype(A)))
    accumulate!(op, out, A, axis)
end


"""
    accumulate(op, v0, A)

Like `accumulate`, but using a starting element `v0`. The first entry of the result will be
`op(v0, first(A))`. For example:

```jldoctest
julia> accumulate(+, 100, [1,2,3])
3-element Array{Int64,1}:
 101
 103
 106

julia> accumulate(min, 0, [1,2,-1])
3-element Array{Int64,1}:
  0
  0
 -1
```
"""
function accumulate(op, v0, A, axis::Integer=1)
    T = rcum_promote_type(op, typeof(v0), eltype(A))
    out = similar(A, T)
    accumulate!(op, out, v0, A, 1)
end

function accumulate!{Op}(op::Op, B, A::AbstractVector, axis::Integer=1)
    isempty(A) && return B
    v1 = first(A)
    _accumulate1!(op, B, v1, A, axis)
end

function accumulate!(op, B, v0, A::AbstractVector, axis::Integer=1)
    isempty(A) && return B
    v1 = op(v0, first(A))
    _accumulate1!(op, B, v1, A, axis)
end


function _accumulate1!(op, B, v1, A::AbstractVector, axis::Integer=1)
    axis > 0 || throw(ArgumentError("axis must be a positive integer"))
    inds = linearindices(A)
    inds == linearindices(B) || throw(DimensionMismatch("linearindices of A and B don't match"))
    axis > 1 && return copy!(B, A)
    i1 = inds[1]
    cur_val = v1
    B[i1] = cur_val
    @inbounds for i in inds[2:end]
        cur_val = op(cur_val, A[i])
        B[i] = cur_val
    end
    return B
end

"""
    accumulate!(op, B, A, dim=1)

Cumulative operation `op` on `A` along a dimension, storing the result in `B`.
The dimension defaults to 1. See also [`accumulate`](@ref).
"""
function accumulate!(op, B, A, axis::Integer=1)
    axis > 0 || throw(ArgumentError("axis must be a positive integer"))
    inds_t = indices(A)
    indices(B) == inds_t || throw(DimensionMismatch("shape of B must match A"))
    axis > ndims(A) && return copy!(B, A)
    isempty(inds_t[axis]) && return B
    if axis == 1
        # We can accumulate to a temporary variable, which allows
        # register usage and will be slightly faster
        ind1 = inds_t[1]
        @inbounds for I in CartesianRange(tail(inds_t))
            tmp = convert(eltype(B), A[first(ind1), I])
            B[first(ind1), I] = tmp
            for i_1 = first(ind1)+1:last(ind1)
                tmp = op(tmp, A[i_1, I])
                B[i_1, I] = tmp
            end
        end
    else
        R1 = CartesianRange(indices(A)[1:axis-1])   # not type-stable
        R2 = CartesianRange(indices(A)[axis+1:end])
        _accumulate!(op, B, A, R1, inds_t[axis], R2) # use function barrier
    end
    return B
end

@noinline function _accumulate!(op, B, A, R1, ind, R2)
    # Copy the initial element in each 1d vector along dimension `axis`
    i = first(ind)
    @inbounds for J in R2, I in R1
        B[I, i, J] = A[I, i, J]
    end
    # Accumulate
    @inbounds for J in R2, i in first(ind)+1:last(ind), I in R1
        B[I, i, J] = op(B[I, i-1, J], A[I, i, J])
    end
    B
end

### from abstractarray.jl

function fill!{T}(A::AbstractArray{T}, x)
    xT = convert(T, x)
    for I in eachindex(A)
        @inbounds A[I] = xT
    end
    A
end

function copy!{T,N}(dest::AbstractArray{T,N}, src::AbstractArray{T,N})
    @boundscheck checkbounds(dest, indices(src)...)
    for I in eachindex(linearindexing(src,dest), src)
        @inbounds dest[I] = src[I]
    end
    dest
end

function copy!(dest::AbstractArray, Rdest::CartesianRange, src::AbstractArray, Rsrc::CartesianRange)
    isempty(Rdest) && return dest
    if size(Rdest) != size(Rsrc)
        throw(ArgumentError("source and destination must have same size (got $(size(Rsrc)) and $(size(Rdest)))"))
    end
    @boundscheck checkbounds(dest, Rdest.start)
    @boundscheck checkbounds(dest, Rdest.stop)
    @boundscheck checkbounds(src, Rsrc.start)
    @boundscheck checkbounds(src, Rsrc.stop)
    deltaI = Rdest.start - Rsrc.start
    for I in Rsrc
        @inbounds dest[I+deltaI] = src[I]
    end
    dest
end

# circshift!
circshift!(dest::AbstractArray, src, ::Tuple{}) = copy!(dest, src)
"""
    circshift!(dest, src, shifts)

Circularly shift the data in `src`, storing the result in
`dest`. `shifts` specifies the amount to shift in each dimension.

The `dest` array must be distinct from the `src` array (they cannot
alias each other).

See also `circshift`.
"""
@noinline function circshift!{T,N}(dest::AbstractArray{T,N}, src, shiftamt::DimsInteger)
    dest === src && throw(ArgumentError("dest and src must be separate arrays"))
    inds = indices(src)
    indices(dest) == inds || throw(ArgumentError("indices of src and dest must match (got $inds and $(indices(dest)))"))
    _circshift!(dest, (), src, (), inds, fill_to_length(shiftamt, 0, Val{N}))
end
circshift!(dest::AbstractArray, src, shiftamt) = circshift!(dest, src, (shiftamt...,))

# For each dimension, we copy the first half of src to the second half
# of dest, and the second half of src to the first half of dest. This
# uses a recursive bifurcation strategy so that these splits can be
# encoded by ranges, which means that we need only one call to `mod`
# per dimension rather than one call per index.
# `rdest` and `rsrc` are tuples-of-ranges that grow one dimension at a
# time; when all the dimensions have been filled in, you call `copy!`
# for that block. In other words, in two dimensions schematically we
# have the following call sequence (--> means a call):
#   circshift!(dest, src, shiftamt) -->
#     _circshift!(dest, src, ("first half of dim1",)) -->
#       _circshift!(dest, src, ("first half of dim1", "first half of dim2")) --> copy!
#       _circshift!(dest, src, ("first half of dim1", "second half of dim2")) --> copy!
#     _circshift!(dest, src, ("second half of dim1",)) -->
#       _circshift!(dest, src, ("second half of dim1", "first half of dim2")) --> copy!
#       _circshift!(dest, src, ("second half of dim1", "second half of dim2")) --> copy!
@inline function _circshift!(dest, rdest, src, rsrc,
                             inds::Tuple{AbstractUnitRange,Vararg{Any}},
                             shiftamt::Tuple{Integer,Vararg{Any}})
    ind1, d = inds[1], shiftamt[1]
    s = mod(d, length(ind1))
    sf, sl = first(ind1)+s, last(ind1)-s
    r1, r2 = first(ind1):sf-1, sf:last(ind1)
    r3, r4 = first(ind1):sl, sl+1:last(ind1)
    tinds, tshiftamt = tail(inds), tail(shiftamt)
    _circshift!(dest, (rdest..., r1), src, (rsrc..., r4), tinds, tshiftamt)
    _circshift!(dest, (rdest..., r2), src, (rsrc..., r3), tinds, tshiftamt)
end
# At least one of inds, shiftamt is empty
function _circshift!(dest, rdest, src, rsrc, inds, shiftamt)
    copy!(dest, CartesianRange(rdest), src, CartesianRange(rsrc))
end

# circcopy!
"""
    circcopy!(dest, src)

Copy `src` to `dest`, indexing each dimension modulo its length.
`src` and `dest` must have the same size, but can be offset in
their indices; any offset results in a (circular) wraparound. If the
arrays have overlapping indices, then on the domain of the overlap
`dest` agrees with `src`.

```julia
julia> src = reshape(collect(1:16), (4,4))
4×4 Array{Int64,2}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> dest = OffsetArray{Int}((0:3,2:5))

julia> circcopy!(dest, src)
OffsetArrays.OffsetArray{Int64,2,Array{Int64,2}} with indices 0:3×2:5:
 8  12  16  4
 5   9  13  1
 6  10  14  2
 7  11  15  3

julia> dest[1:3,2:4] == src[1:3,2:4]
true
```
"""
function circcopy!(dest, src)
    dest === src && throw(ArgumentError("dest and src must be separate arrays"))
    indssrc, indsdest = indices(src), indices(dest)
    if (szsrc = map(length, indssrc)) != (szdest = map(length, indsdest))
        throw(DimensionMismatch("src and dest must have the same sizes (got $szsrc and $szdest)"))
    end
    shift = map((isrc, idest)->first(isrc)-first(idest), indssrc, indsdest)
    all(x->x==0, shift) && return copy!(dest, src)
    _circcopy!(dest, (), indsdest, src, (), indssrc)
end

# This uses the same strategy described above for _circshift!
@inline function _circcopy!(dest, rdest, indsdest::Tuple{AbstractUnitRange,Vararg{Any}},
                            src,  rsrc,  indssrc::Tuple{AbstractUnitRange,Vararg{Any}})
    indd1, inds1 = indsdest[1], indssrc[1]
    l = length(indd1)
    s = mod(first(inds1)-first(indd1), l)
    sdf = first(indd1)+s
    rd1, rd2 = first(indd1):sdf-1, sdf:last(indd1)
    ssf = last(inds1)-s
    rs1, rs2 = first(inds1):ssf, ssf+1:last(inds1)
    tindsd, tindss = tail(indsdest), tail(indssrc)
    _circcopy!(dest, (rdest..., rd1), tindsd, src, (rsrc..., rs2), tindss)
    _circcopy!(dest, (rdest..., rd2), tindsd, src, (rsrc..., rs1), tindss)
end

# At least one of indsdest, indssrc are empty (and both should be, since we've checked)
function _circcopy!(dest, rdest, indsdest, src, rsrc, indssrc)
    copy!(dest, CartesianRange(rdest), src, CartesianRange(rsrc))
end

### BitArrays

## getindex

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!
@inline function _unsafe_getindex!(X::BitArray, B::BitArray, I0::Union{UnitRange{Int},Slice})
    copy_chunks!(X.chunks, 1, B.chunks, indexoffset(I0)+1, length(I0))
    return X
end

# Optimization where the inner dimension is contiguous improves perf dramatically
@generated function _unsafe_getindex!(X::BitArray, B::BitArray,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        @nexprs $N d->(I_d = I[d])

        idxlens = @ncall $N index_lengths I0 I

        f0 = indexoffset(I0)+1
        l0 = idxlens[1]

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I_d)
            gap_lst_{d+1} *= stride
        end

        storeind = 1
        Xc, Bc = X.chunks, B.chunks
        @nloops($N, i, d->(1:idxlens[d+1]),
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_chunks!(Xc, storeind, Bc, ind, l0)
                    storeind += l0
                end)
        return X
    end
end

# in the general multidimensional non-scalar case, can we do about 10% better
# in most cases by manually hoisting the bitarray chunks access out of the loop
# (This should really be handled by the compiler or with an immutable BitArray)
@generated function _unsafe_getindex!(X::BitArray, B::BitArray, I::Union{Int,AbstractArray{Int}}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(B, d))
        $(Symbol(:offset_, N)) = 1
        ind = 0
        Xc, Bc = X.chunks, B.chunks
        @nloops $N i d->I[d] d->(@inbounds offset_{d-1} = offset_d + (i_d-1)*stride_d) begin
            ind += 1
            unsafe_bitsetindex!(Xc, unsafe_bitgetindex(Bc, offset_0), ind)
        end
        return X
    end
end

## setindex!

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

@inline function setindex!(B::BitArray, X::Union{BitArray,Array}, J0::Union{Colon,UnitRange{Int}})
    I0 = to_indices(B, (J0,))[1]
    @boundscheck checkbounds(B, I0)
    l0 = length(I0)
    setindex_shape_check(X, l0)
    l0 == 0 && return B
    f0 = indexoffset(I0)+1
    copy_to_bitarray_chunks!(B.chunks, f0, X, 1, l0)
    return B
end

@inline function setindex!(B::BitArray, X::Union{BitArray,Array},
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, X, J...)
end
@generated function _unsafe_setindex!(B::BitArray, X::Union{BitArray,Array},
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        idxlens = @ncall $N index_lengths I0 d->I[d]
        @ncall $N setindex_shape_check X idxlens[1] d->idxlens[d+1]
        isempty(X) && return B
        f0 = indexoffset(I0)+1
        l0 = idxlens[1]

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I[d])
            gap_lst_{d+1} *= stride
        end

        refind = 1
        Bc = B.chunks
        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_to_bitarray_chunks!(Bc, ind, X, refind, l0)
                    refind += l0
                end)

        return B
    end
end

@inline function setindex!(B::BitArray, x,
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, x, J...)
end
@generated function _unsafe_setindex!(B::BitArray, x,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        y = Bool(x)
        idxlens = @ncall $N index_lengths I0 d->I[d]

        f0 = indexoffset(I0)+1
        l0 = idxlens[1]
        l0 == 0 && return B
        @nexprs $N d->(isempty(I[d]) && return B)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I[d])
            gap_lst_{d+1} *= stride
        end

        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                fill_chunks!(B.chunks, y, ind, l0) # BODY
                )

        return B
    end
end

## findn

@generated function findn{N}(B::BitArray{N})
    quote
        nnzB = countnz(B)
        I = ntuple(x->Array{Int}(nnzB), $N)
        if nnzB > 0
            count = 1
            @nloops $N i B begin
                if (@nref $N B i) # TODO: should avoid bounds checking
                    @nexprs $N d->(I[d][count] = i_d)
                    count += 1
                end
            end
        end
        return I
    end
end

## isassigned

@generated function isassigned(B::BitArray, I_0::Int, I::Int...)
    N = length(I)
    quote
        @nexprs $N d->(I_d = I[d])
        stride = 1
        index = I_0
        @nexprs $N d->begin
            l = size(B,d)
            stride *= l
            1 <= I_{d-1} <= l || return false
            index += (I_d - 1) * stride
        end
        return isassigned(B, index)
    end
end

## permutedims

## Permute array dims ##

function permutedims(B::StridedArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    (ndimsB == length(perm) && isperm(perm)) || throw(ArgumentError("no valid permutation of dimensions"))
    dimsP = ntuple(i->dimsB[perm[i]], ndimsB)::typeof(dimsB)
    P = similar(B, dimsP)
    permutedims!(P, B, perm)
end

function checkdims_perm{TP,TB,N}(P::AbstractArray{TP,N}, B::AbstractArray{TB,N}, perm)
    indsB = indices(B)
    length(perm) == N || throw(ArgumentError("expected permutation of size $N, but length(perm)=$(length(perm))"))
    isperm(perm) || throw(ArgumentError("input is not a permutation"))
    indsP = indices(P)
    for i = 1:length(perm)
        indsP[i] == indsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
    end
    nothing
end

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @generated function permutedims!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        quote
            checkdims_perm(P, B, perm)

            #calculates all the strides
            strides_1 = 0
            @nexprs $N d->(strides_{d+1} = stride(B, perm[d]))

            #Creates offset, because indexing starts at 1
            offset = 1 - sum(@ntuple $N d->strides_{d+1})

            if isa(B, SubArray)
                offset += first_index(B::SubArray) - 1
                B = B.parent
            end

            ind = 1
            @nexprs 1 d->(counts_{$N+1} = strides_{$N+1}) # a trick to set counts_($N+1)
            @nloops($N, i, P,
                    d->(counts_d = strides_d), # PRE
                    d->(counts_{d+1} += strides_{d+1}), # POST
                    begin # BODY
                        sumc = sum(@ntuple $N d->counts_{d+1})
                        @inbounds P[ind] = B[sumc+offset]
                        ind += 1
                    end)

            return P
        end
    end
end

## unique across dim

# TODO: this doesn't fit into the new hashing scheme in any obvious way

immutable Prehashed
    hash::UInt
end
hash(x::Prehashed) = x.hash

"""
    unique(itr[, dim])

Returns an array containing only the unique elements of the iterable `itr`, in
the order that the first of each set of equivalent elements originally appears.
If `dim` is specified, returns unique regions of the array `itr` along `dim`.

```jldoctest
julia> A = map(isodd, reshape(collect(1:8), (2,2,2)))
2×2×2 Array{Bool,3}:
[:, :, 1] =
  true   true
 false  false

[:, :, 2] =
  true   true
 false  false

julia> unique(A)
2-element Array{Bool,1}:
  true
 false

julia> unique(A, 2)
2×1×2 Array{Bool,3}:
[:, :, 1] =
  true
 false

[:, :, 2] =
  true
 false

julia> unique(A, 3)
2×2×1 Array{Bool,3}:
[:, :, 1] =
  true   true
 false  false
```
"""
@generated function unique{T,N}(A::AbstractArray{T,N}, dim::Int)
    inds = inds -> zeros(UInt, inds)
    quote
        1 <= dim <= $N || return copy(A)
        hashes = similar($inds, indices(A, dim))

        # Compute hash for each row
        k = 0
        @nloops $N i A d->(if d == dim; k = i_d; end) begin
            @inbounds hashes[k] = hash(hashes[k], hash((@nref $N A i)))
        end

        # Collect index of first row for each hash
        uniquerow = similar(Array{Int}, indices(A, dim))
        firstrow = Dict{Prehashed,Int}()
        for k = indices(A, dim)
            uniquerow[k] = get!(firstrow, Prehashed(hashes[k]), k)
        end
        uniquerows = collect(values(firstrow))

        # Check for collisions
        collided = similar(falses, indices(A, dim))
        @inbounds begin
            @nloops $N i A d->(if d == dim
                k = i_d
                j_d = uniquerow[k]
            else
                j_d = i_d
            end) begin
                if (@nref $N A j) != (@nref $N A i)
                    collided[k] = true
                end
            end
        end

        if any(collided)
            nowcollided = similar(BitArray, indices(A, dim))
            while any(collided)
                # Collect index of first row for each collided hash
                empty!(firstrow)
                for j = indices(A, dim)
                    collided[j] || continue
                    uniquerow[j] = get!(firstrow, Prehashed(hashes[j]), j)
                end
                for v in values(firstrow)
                    push!(uniquerows, v)
                end

                # Check for collisions
                fill!(nowcollided, false)
                @nloops $N i A d->begin
                    if d == dim
                        k = i_d
                        j_d = uniquerow[k]
                        (!collided[k] || j_d == k) && continue
                    else
                        j_d = i_d
                    end
                end begin
                    if (@nref $N A j) != (@nref $N A i)
                        nowcollided[k] = true
                    end
                end
                (collided, nowcollided) = (nowcollided, collided)
            end
        end

        @nref $N A d->d == dim ? sort!(uniquerows) : (indices(A, d))
    end
end

"""
    extrema(A, dims) -> Array{Tuple}

Compute the minimum and maximum elements of an array over the given dimensions.

# Example
```jldoctest
julia> A = reshape(collect(1:2:16), (2,2,2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  5
 3  7

[:, :, 2] =
  9  13
 11  15

julia> extrema(A, (1,2))
1×1×2 Array{Tuple{Int64,Int64},3}:
[:, :, 1] =
 (1, 7)

[:, :, 2] =
 (9, 15)
```
"""
function extrema(A::AbstractArray, dims)
    sz = [size(A)...]
    sz[[dims...]] = 1
    B = Array{Tuple{eltype(A),eltype(A)}}(sz...)
    return extrema!(B, A)
end

@generated function extrema!{T,N}(B, A::AbstractArray{T,N})
    return quote
        sA = size(A)
        sB = size(B)
        @nloops $N i B begin
            AI = @nref $N A i
            (@nref $N B i) = (AI, AI)
        end
        Bmax = sB
        Istart = Int[sB[i] == 1 != sA[i] ? 2 : 1 for i = 1:ndims(A)]
        @inbounds @nloops $N i d->(Istart[d]:size(A,d)) begin
            AI = @nref $N A i
            @nexprs $N d->(j_d = min(Bmax[d], i_{d}))
            BJ = @nref $N B j
            if AI < BJ[1]
                (@nref $N B j) = (AI, BJ[2])
            elseif AI > BJ[2]
                (@nref $N B j) = (BJ[1], AI)
            end
        end
        return B
    end
end
