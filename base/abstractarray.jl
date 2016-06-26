# This file is a part of Julia. License is MIT: http://julialang.org/license

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}
typealias AbstractVecOrMat{T} Union{AbstractVector{T}, AbstractMatrix{T}}
typealias RangeIndex Union{Int, Range{Int}, AbstractUnitRange{Int}, Colon}
typealias Indices{N} NTuple{N,AbstractUnitRange}
typealias IndicesOne{N} NTuple{N,OneTo}
typealias DimOrInd Union{Integer, AbstractUnitRange}
typealias DimsOrInds{N} NTuple{N,DimOrInd}

## Basic functions ##

vect() = Array{Any}(0)
vect{T}(X::T...) = T[ X[i] for i=1:length(X) ]

function vect(X...)
    T = promote_typeof(X...)
    #T[ X[i] for i=1:length(X) ]
    # TODO: this is currently much faster. should figure out why. not clear.
    copy!(Array{T}(length(X)), X)
end

size{T,N}(t::AbstractArray{T,N}, d) = d <= N ? size(t)[d] : 1
size{N}(x, d1::Integer, d2::Integer, dx::Vararg{Integer, N}) = (size(x, d1), size(x, d2), ntuple(k->size(x, dx[k]), Val{N})...)
"""
    indices(A, d)

Returns the valid range of indices for array `A` along dimension `d`.
"""
function indices(A::AbstractArray, d)
    @_inline_meta
    OneTo(size(A,d))
end
"""
    indices(A)

Returns the tuple of valid indices for array `A`.
"""
indices{T,N}(A::AbstractArray{T,N}) = _indices((), A) # faster than ntuple(d->indices(A,d), Val{N})
_indices{T,N}(out::NTuple{N}, A::AbstractArray{T,N}) = out
function _indices(out, A::AbstractArray)
    @_inline_meta
    _indices((out..., indices(A, length(out)+1)), A)
end
# Note: a simpler implementation suffers from #16327
# function indices{T,N}(A::AbstractArray{T,N})
#     @_inline_meta
#     ntuple(d->indices(A, d), Val{N})
# end
indices1(A) = (@_inline_meta; indices(A, 1))
"""
    linearindices(A)

Returns a `UnitRange` specifying the valid range of indices for `A[i]`
where `i` is an `Int`. For arrays with conventional indexing (indices
start at 1), or any multidimensional array, this is `1:length(A)`;
however, for one-dimensional arrays with unconventional indices, this
is `indices(A, 1)`.

Calling this function is the "safe" way to write algorithms that
exploit linear indexing.
"""
linearindices(A) = 1:length(A)
linearindices(A::AbstractVector) = indices1(A)
eltype{T}(::Type{AbstractArray{T}}) = T
eltype{T,N}(::Type{AbstractArray{T,N}}) = T
elsize{T}(::AbstractArray{T}) = sizeof(T)
ndims{T,N}(::AbstractArray{T,N}) = N
ndims{T,N}(::Type{AbstractArray{T,N}}) = N
ndims{T<:AbstractArray}(::Type{T}) = ndims(supertype(T))
length(t::AbstractArray) = prod(size(t))::Int
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[first(eachindex(a))]

function first(itr)
    state = start(itr)
    done(itr, state) && throw(ArgumentError("collection must be non-empty"))
    next(itr, state)[1]
end
last(a) = a[end]

"""
    stride(A, k)

Returns the distance in memory (in number of elements) between adjacent elements in dimension `k`.
"""
function stride(a::AbstractArray, i::Integer)
    if i > ndims(a)
        return length(a)
    end
    s = 1
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end

strides{T}(A::AbstractArray{T,0}) = ()
"""
    strides(A)

Returns a tuple of the memory strides in each dimension.
"""
strides(A::AbstractArray) = _strides((1,), A)
_strides{T,N}(out::NTuple{N}, A::AbstractArray{T,N}) = out
function _strides{M,T,N}(out::NTuple{M}, A::AbstractArray{T,N})
    @_inline_meta
    _strides((out..., out[M]*size(A, M)), A)
end

function isassigned(a::AbstractArray, i::Int...)
    # TODO
    try
        a[i...]
        true
    catch
        false
    end
end

# used to compute "end" for last index
function trailingsize(A, n)
    s = 1
    for i=n:ndims(A)
        s *= size(A,i)
    end
    return s
end

## Traits for array types ##

abstract LinearIndexing
immutable LinearFast <: LinearIndexing end
immutable LinearSlow <: LinearIndexing end

linearindexing(A::AbstractArray) = linearindexing(typeof(A))
linearindexing{T<:AbstractArray}(::Type{T}) = LinearSlow()
linearindexing{T<:Array}(::Type{T}) = LinearFast()
linearindexing{T<:Range}(::Type{T}) = LinearFast()

linearindexing(A::AbstractArray, B::AbstractArray) = linearindexing(linearindexing(A), linearindexing(B))
linearindexing(A::AbstractArray, B::AbstractArray...) = linearindexing(linearindexing(A), linearindexing(B...))
linearindexing(::LinearFast, ::LinearFast) = LinearFast()
linearindexing(::LinearIndexing, ::LinearIndexing) = LinearSlow()

abstract IndicesBehavior
immutable IndicesStartAt1  <: IndicesBehavior end   # indices 1:size(A,d)
immutable IndicesUnitRange <: IndicesBehavior end   # arb UnitRange indices
immutable IndicesList      <: IndicesBehavior end   # indices like (:cat, :dog, :mouse)

indicesbehavior(A::AbstractArray) = indicesbehavior(typeof(A))
indicesbehavior{T<:AbstractArray}(::Type{T}) = IndicesStartAt1()
indicesbehavior(::Number) = IndicesStartAt1()

abstract IndicesPerformance
immutable IndicesFast1D <: IndicesPerformance end  # indices(A, d) is fast
immutable IndicesSlow1D <: IndicesPerformance end  # indices(A) is better than indices(A,d)

indicesperformance(A::AbstractArray) = indicesperformance(typeof(A))
indicesperformance{T<:AbstractArray}(::Type{T}) = IndicesFast1D()

"""
    shape(A)

Returns a tuple specifying the "shape" of array `A`. For arrays with
conventional indexing (indices start at 1), this is equivalent to
`size(A)`; otherwise it is equivalent to `indices(A)`.
"""
shape(a) = shape(indicesbehavior(a), a)
"""
    shape(A, d)

Specifies the "shape" of the array `A` along dimension `d`. For arrays
with conventional indexing (starting at 1), this is equivalent to
`size(A, d)`; for arrays with unconventional indexing (indexing may
start at something different from 1), it is equivalent to `indices(A,
d)`.
"""
shape(a, d) = shape(indicesbehavior(a), a, d)
shape(::IndicesStartAt1, a) = size(a)
shape(::IndicesStartAt1, a, d) = size(a, d)
shape(::IndicesBehavior, a) = indices(a)
shape(::IndicesBehavior, a, d) = indices(a, d)

## Bounds checking ##
@generated function trailingsize{T,N,n}(A::AbstractArray{T,N}, ::Type{Val{n}})
    (isa(n, Int) && isa(N, Int)) || error("Must have concrete type")
    n > N && return 1
    ex = :(size(A, $n))
    for m = n+1:N
        ex = :($ex * size(A, $m))
    end
    Expr(:block, Expr(:meta, :inline), ex)
end

# check along a single dimension
"""
    checkindex(Bool, inds::UnitRange, index)

Return `true` if the given `index` is within the bounds of
`inds`. Custom types that would like to behave as indices for all
arrays can extend this method in order to provide a specialized bounds
checking implementation.
"""
checkindex(::Type{Bool}, inds::AbstractUnitRange, i) = throw(ArgumentError("unable to check bounds for indices of type $(typeof(i))"))
checkindex(::Type{Bool}, inds::AbstractUnitRange, i::Real) = (first(inds) <= i) & (i <= last(inds))
checkindex(::Type{Bool}, inds::AbstractUnitRange, ::Colon) = true
function checkindex(::Type{Bool}, inds::AbstractUnitRange, r::Range)
    @_propagate_inbounds_meta
    isempty(r) | (checkindex(Bool, inds, first(r)) & checkindex(Bool, inds, last(r)))
end
checkindex{N}(::Type{Bool}, indx::AbstractUnitRange, I::AbstractArray{Bool,N}) = N == 1 && indx == indices1(I)
function checkindex(::Type{Bool}, inds::AbstractUnitRange, I::AbstractArray)
    @_inline_meta
    b = true
    for i in I
        b &= checkindex(Bool, inds, i)
    end
    b
end

# check all indices/dimensions
# To make extension easier, avoid specializing checkbounds on index types
"""
    checkbounds(Bool, array, indexes...)

Return `true` if the specified `indexes` are in bounds for the given `array`. Subtypes of
`AbstractArray` should specialize this method if they need to provide custom bounds checking
behaviors.
"""
function checkbounds(::Type{Bool}, A::AbstractArray, I...)
    @_inline_meta
    _checkbounds(Bool, indicesperformance(A), A, I...)
end
function _checkbounds(::Type{Bool}, ::IndicesSlow1D, A::AbstractArray, I...)
    @_inline_meta
    checkbounds_indices(indices(A), I)
end
_checkbounds(::Type{Bool}, ::IndicesSlow1D, A::AbstractArray, I::AbstractArray{Bool}) = _chkbnds(A, (true,), I)

# Bounds-checking for arrays for which indices(A) is faster than indices(A, d)
checkbounds_indices(::Tuple{},  ::Tuple{})    = true
checkbounds_indices(::Tuple{}, I::Tuple{Any}) = (@_inline_meta; checkindex(Bool, 1:1, I[1]))
checkbounds_indices(::Tuple{}, I::Tuple)      = (@_inline_meta; checkindex(Bool, 1:1, I[1]) & checkbounds_indices((), tail(I)))
checkbounds_indices(inds::Tuple{Any}, I::Tuple{Any}) = (@_inline_meta; checkindex(Bool, inds[1], I[1]))
checkbounds_indices(inds::Tuple, I::Tuple{Any}) = (@_inline_meta; checkindex(Bool, 1:prod(map(dimlength, inds)), I[1]))
checkbounds_indices(inds::Tuple, I::Tuple) = (@_inline_meta; checkindex(Bool, inds[1], I[1]) & checkbounds_indices(tail(inds), tail(I)))

# Bounds-checking for arrays for which indices(A, d) is fast
function _checkbounds(::Type{Bool}, ::IndicesFast1D, A::AbstractArray, I...)
    @_inline_meta
    # checked::NTuple{M} means we have checked dimensions 1:M-1, now
    # need to check dimension M. checked[M] indicates whether all the
    # previous ones are in-bounds.
    # By growing checked, it allows us to test whether we've processed
    # the same number of dimensions as the array, even while
    # supporting CartesianIndex
    _chkbnds(A, (true,), I...)
end
checkbounds(::Type{Bool}, A::AbstractArray) = checkbounds(Bool, A, 1) # 0-d case
# Single logical array indexing:
_chkbnds(A::AbstractArray, ::NTuple{1,Bool}, I::AbstractArray{Bool}) = indices(A) == indices(I)
_chkbnds(A::AbstractArray, ::NTuple{1,Bool}, I::AbstractVector{Bool}) = length(A) == length(I)
_chkbnds(A::AbstractVector, ::NTuple{1,Bool}, I::AbstractArray{Bool}) = length(A) == length(I)
_chkbnds(A::AbstractVector, ::NTuple{1,Bool}, I::AbstractVector{Bool}) = indices(A) == indices(I)
# Linear indexing:
function _chkbnds(A::AbstractVector, ::NTuple{1,Bool}, I)
    @_inline_meta
    checkindex(Bool, indices1(A), I)
end
function _chkbnds(A::AbstractArray, ::NTuple{1,Bool}, I)
    @_inline_meta
    checkindex(Bool, 1:length(A), I)
end
# When all indices have been checked:
_chkbnds{M}(A, checked::NTuple{M,Bool}) = checked[M]
# When the number of indices matches the array dimensionality:
function _chkbnds{T,N}(A::AbstractArray{T,N}, checked::NTuple{N,Bool}, I1)
    @_inline_meta
    checked[N] & checkindex(Bool, indices(A, N), I1)
end
# When the last checked dimension is not equal to the array dimensionality:
# TODO: for #14770 (deprecating partial linear indexing), change to 1:trailingsize(...) to 1:1
function _chkbnds{T,N,M}(A::AbstractArray{T,N}, checked::NTuple{M,Bool}, I1)
    @_inline_meta
    checked[M] & checkindex(Bool, 1:trailingsize(A, Val{M}), I1)
end
# Checking an interior dimension:
function _chkbnds{T,N,M}(A::AbstractArray{T,N}, checked::NTuple{M,Bool}, I1, I...)
    @_inline_meta
    # grow checked by one
    newchecked = (checked..., checked[M] & checkindex(Bool, indices(A, M), I1))
    _chkbnds(A, newchecked, I...)
end

throw_boundserror(A, I) = (@_noinline_meta; throw(BoundsError(A, I)))

"""
    checkbounds(array, indexes...)

Throw an error if the specified `indexes` are not in bounds for the given `array`.
"""
function checkbounds(A::AbstractArray, I...)
    @_inline_meta
    checkbounds(Bool, A, I...) || throw_boundserror(A, I)
end
checkbounds(A::AbstractArray) = checkbounds(A, 1) # 0-d case

# See also specializations in multidimensional

## Constructors ##

# default arguments to similar()
"""
    similar(array, [element_type=eltype(array)], [dims=size(array)])

Create an uninitialized mutable array with the given element type and size, based upon the
given source array. The second and third arguments are both optional, defaulting to the
given array's `eltype` and `size`. The dimensions may be specified either as a single tuple
argument or as a series of integer arguments.

Custom AbstractArray subtypes may choose which specific array type is best-suited to return
for the given element type and dimensionality. If they do not specialize this method, the
default is an `Array{element_type}(dims...)`.

For example, `similar(1:10, 1, 4)` returns an uninitialized `Array{Int,2}` since ranges are
neither mutable nor support 2 dimensions:

    julia> similar(1:10, 1, 4)
    1×4 Array{Int64,2}:
     4419743872  4374413872  4419743888  0

Conversely, `similar(trues(10,10), 2)` returns an uninitialized `BitVector` with two
elements since `BitArray`s are both mutable and can support 1-dimensional arrays:

    julia> similar(trues(10,10), 2)
    2-element BitArray{1}:
     false
     false

Since `BitArray`s can only store elements of type `Bool`, however, if you request a
different element type it will create a regular `Array` instead:

    julia> similar(falses(10), Float64, 2, 4)
    2×4 Array{Float64,2}:
     2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
     2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314

See also `allocate_for`.
"""
similar{T}(a::AbstractArray{T})                          = similar(a, T)
similar(   a::AbstractArray, T::Type)                    = similar(a, T, indices(a))
similar{T}(a::AbstractArray{T}, dims::Tuple)             = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::DimOrInd...)       = similar(a, T, dims)
similar(   a::AbstractArray, T::Type, dims::DimOrInd...) = similar(a, T, dims)
similar(   a::AbstractArray, T::Type, dims::DimsInteger) = similar(a, T, Dims(dims))
# similar creates an Array by default
similar(   a::AbstractArray, T::Type, inds::IndicesOne)  = similar(a, T, map(last, inds))
similar(   a::AbstractArray, T::Type, dims::Dims)        = Array(T, dims)

"""
    allocate_for(storagetype, referencearray, [shape])

Create an uninitialized mutable array analogous to that specified by
`storagetype`, but with type and shape specified by the final two
arguments. The main purpose of this function is to support allocation
of arrays that may have unconventional indexing (starting at other
than 1), as determined by `referencearray` and the optional `shape`
information.

    **Examples**:

    allocate_for(Array{Int}, A)

creates an array that "acts like" an `Array{Int}` (and might indeed be
backed by one), but which is indexed identically to `A`. If `A` has
conventional indexing, this will likely just call
`Array{Int}(size(A))`, but if `A` has unconventional indexing then the
indices of the result will match `A`.

    allocate_for(BitArray, A, (shape(A, 2),))

would create a 1-dimensional logical array whose indices match those
of the columns of `A`.

The main purpose of the `referencearray` argument is to select a
particular array type supporting unconventional indexing (as it is
possible that several different ones will be simultaneously in use).

See also `similar`.
"""
allocate_for(f, a, shape::Union{SimIdx,Tuple{Vararg{SimIdx}}}) = f(shape)
allocate_for(f, a) = allocate_for(f, a, shape(a))
# allocate_for when passed multiple arrays. Necessary for broadcast, etc.
function allocate_for(f, as::Tuple, shape::Union{SimIdx,Tuple{Vararg{SimIdx}}})
    @_inline_meta
    a = promote_indices(as...)
    allocate_for(f, a, shape)
end

promote_indices(a) = a
function promote_indices(a, b, c...)
    @_inline_meta
    promote_indices(promote_indices(a, b), c...)
end
# overload this to return true for your type, e.g.,
#   promote_indices(a::OffsetArray, b::OffsetArray) = a
promote_indices(a::AbstractArray, b::AbstractArray) = _promote_indices(indicesbehavior(a), indicesbehavior(b), a, b)
_promote_indices(::IndicesStartAt1, ::IndicesStartAt1, a, b) = a
_promote_indices(::IndicesBehavior, ::IndicesBehavior, a, b) = throw(ArgumentError("types $(typeof(a)) and $(typeof(b)) do not have promote_indices defined"))
promote_indices(a::Number, b::AbstractArray) = b
promote_indices(a::AbstractArray, b::Number) = a

# Strip off the index-changing container---this assumes that `parent`
# performs such an operation. TODO: since few things in Base need this, it
# would be great to find a way to eliminate this function.
normalize_indices(A) = normalize_indices(indicesbehavior(A), A)
normalize_indices(::IndicesStartAt1, A) = A
normalize_indices(::IndicesBehavior, A) = parent(A)

## from general iterable to any array

function copy!(dest::AbstractArray, src)
    destiter = eachindex(dest)
    state = start(destiter)
    for x in src
        i, state = next(destiter, state)
        dest[i] = x
    end
    return dest
end

function copy!(dest::AbstractArray, dstart::Integer, src)
    i = Int(dstart)
    for x in src
        dest[i] = x
        i += 1
    end
    return dest
end

# copy from an some iterable object into an AbstractArray
function copy!(dest::AbstractArray, dstart::Integer, src, sstart::Integer)
    if (sstart < 1)
        throw(ArgumentError(string("source start offset (",sstart,") is < 1")))
    end
    st = start(src)
    for j = 1:(sstart-1)
        if done(src, st)
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",sstart,", got ",j-1)))
        end
        _, st = next(src, st)
    end
    dn = done(src, st)
    if dn
        throw(ArgumentError(string("source has fewer elements than required, ",
                                      "expected at least ",sstart,", got ",sstart-1)))
    end
    i = Int(dstart)
    while !dn
        val, st = next(src, st)
        dest[i] = val
        i += 1
        dn = done(src, st)
    end
    return dest
end

# this method must be separate from the above since src might not have a length
function copy!(dest::AbstractArray, dstart::Integer, src, sstart::Integer, n::Integer)
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    n == 0 && return dest
    dmax = dstart + n - 1
    inds = linearindices(dest)
    if (dstart ∉ inds || dmax ∉ inds) | (sstart < 1)
        sstart < 1 && throw(ArgumentError(string("source start offset (",sstart,") is < 1")))
        throw(BoundsError(dest, dstart:dmax))
    end
    st = start(src)
    for j = 1:(sstart-1)
        if done(src, st)
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",sstart,", got ",j-1)))
        end
        _, st = next(src, st)
    end
    i = Int(dstart)
    while i <= dmax && !done(src, st)
        val, st = next(src, st)
        @inbounds dest[i] = val
        i += 1
    end
    i <= dmax && throw(BoundsError(dest, i))
    return dest
end

## copy between abstract arrays - generally more efficient
## since a single index variable can be used.

copy!(dest::AbstractArray, src::AbstractArray) =
    copy!(linearindexing(dest), dest, linearindexing(src), src)

function copy!(::LinearIndexing, dest::AbstractArray, ::LinearIndexing, src::AbstractArray)
    destinds, srcinds = linearindices(dest), linearindices(src)
    isempty(srcinds) || (first(srcinds) ∈ destinds && last(srcinds) ∈ destinds) || throw(BoundsError(dest, srcinds))
    @inbounds for i in srcinds
        dest[i] = src[i]
    end
    return dest
end

function copy!(::LinearIndexing, dest::AbstractArray, ::LinearSlow, src::AbstractArray)
    destinds, srcinds = linearindices(dest), linearindices(src)
    isempty(srcinds) || (first(srcinds) ∈ destinds && last(srcinds) ∈ destinds) || throw(BoundsError(dest, srcinds))
    i = 0
    @inbounds for a in src
        dest[i+=1] = a
    end
    return dest
end

function copy!(dest::AbstractArray, dstart::Integer, src::AbstractArray)
    copy!(dest, dstart, src, first(linearindices(src)), length(src))
end

function copy!(dest::AbstractArray, dstart::Integer, src::AbstractArray, sstart::Integer)
    srcinds = linearindices(src)
    sstart ∈ srcinds || throw(BoundsError(src, sstart))
    copy!(dest, dstart, src, sstart, last(srcinds)-sstart+1)
end

function copy!(dest::AbstractArray, dstart::Integer,
               src::AbstractArray, sstart::Integer,
               n::Integer)
    n == 0 && return dest
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    destinds, srcinds = linearindices(dest), linearindices(src)
    (dstart ∈ destinds && dstart+n-1 ∈ destinds) || throw(BoundsError(dest, dstart:dstart+n-1))
    (sstart ∈ srcinds  && sstart+n-1 ∈ srcinds)  || throw(BoundsError(src,  sstart:sstart+n-1))
    @inbounds for i = 0:(n-1)
        dest[dstart+i] = src[sstart+i]
    end
    return dest
end

function copy(a::AbstractArray)
    @_propagate_inbounds_meta
    copymutable(a)
end

function copy!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int},
                    A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    jdest = first(jr_dest)
    for jsrc in jr_src
        idest = first(ir_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            idest += step(ir_dest)
        end
        jdest += step(jr_dest)
    end
    return B
end

function copy_transpose!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int},
                              A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    idest = first(ir_dest)
    for jsrc in jr_src
        jdest = first(jr_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            jdest += step(jr_dest)
        end
        idest += step(ir_dest)
    end
    return B
end

function copymutable(a::AbstractArray)
    @_propagate_inbounds_meta
    copy!(similar(a), a)
end
copymutable(itr) = collect(itr)
"""
    copymutable(a)

Make a mutable copy of an array or iterable `a`.  For `a::Array`,
this is equivalent to `copy(a)`, but for other array types it may
differ depending on the type of `similar(a)`.  For generic iterables
this is equivalent to `collect(a)`.
"""
copymutable

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## iteration support for arrays by iterating over `eachindex` in the array ##
# Allows fast iteration by default for both LinearFast and LinearSlow arrays

# While the definitions for LinearFast are all simple enough to inline on their
# own, LinearSlow's CartesianRange is more complicated and requires explicit
# inlining.
start(A::AbstractArray) = (@_inline_meta; itr = eachindex(A); (itr, start(itr)))
next(A::AbstractArray,i) = (@_propagate_inbounds_meta; (idx, s) = next(i[1], i[2]); (A[idx], (i[1], s)))
done(A::AbstractArray,i) = (@_propagate_inbounds_meta; done(i[1], i[2]))

# eachindex iterates over all indices. LinearSlow definitions are later.
eachindex(A::AbstractArray) = (@_inline_meta(); eachindex(linearindexing(A), A))

function eachindex(A::AbstractArray, B::AbstractArray)
    @_inline_meta
    eachindex(linearindexing(A,B), A, B)
end
function eachindex(A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    eachindex(linearindexing(A,B...), A, B...)
end
eachindex(::LinearFast, A::AbstractArray) = linearindices(A)
function eachindex(::LinearFast, A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    1:_maxlength(A, B...)
end
_maxlength(A) = length(A)
function _maxlength(A, B, C...)
    @_inline_meta
    max(length(A), _maxlength(B, C...))
end

isempty(a::AbstractArray) = (length(a) == 0)

## Conversions ##

convert{T,N  }(::Type{AbstractArray{T,N}}, A::AbstractArray{T,N}) = A
convert{T,S,N}(::Type{AbstractArray{T,N}}, A::AbstractArray{S,N}) = copy!(similar(A,T), A)
convert{T,S,N}(::Type{AbstractArray{T  }}, A::AbstractArray{S,N}) = convert(AbstractArray{T,N}, A)

convert{T,N}(::Type{Array}, A::AbstractArray{T,N}) = convert(Array{T,N}, A)

full(x::AbstractArray) = x

map(::Type{Integer},  a::Array) = map!(Integer, similar(a,typeof(Integer(one(eltype(a))))), a)
map(::Type{Signed},   a::Array) = map!(Signed, similar(a,typeof(Signed(one(eltype(a))))), a)
map(::Type{Unsigned}, a::Array) = map!(Unsigned, similar(a,typeof(Unsigned(one(eltype(a))))), a)

## range conversions ##

map{T<:Real}(::Type{T}, r::StepRange) = T(r.start):T(r.step):T(last(r))
map{T<:Real}(::Type{T}, r::UnitRange) = T(r.start):T(last(r))
map{T<:AbstractFloat}(::Type{T}, r::FloatRange) = FloatRange(T(r.start), T(r.step), r.len, T(r.divisor))
function map{T<:AbstractFloat}(::Type{T}, r::LinSpace)
    new_len = T(r.len)
    new_len == r.len || error("$r: too long for $T")
    LinSpace(T(r.start), T(r.stop), new_len, T(r.divisor))
end

## unsafe/pointer conversions ##

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = unsafe_convert(Ptr{T}, x)
pointer{T}(x::AbstractArray{T}, i::Integer) = (@_inline_meta; unsafe_convert(Ptr{T},x) + (i-first(linearindices(x)))*elsize(x))


## Approach:
# We only define one fallback method on getindex for all argument types.
# That dispatches to an (inlined) internal _getindex function, where the goal is
# to transform the indices such that we can call the only getindex method that
# we require the type A{T,N} <: AbstractArray{T,N} to define; either:
#       getindex(::A, ::Int) # if linearindexing(A) == LinearFast() OR
#       getindex{T,N}(::A{T,N}, ::Vararg{Int, N}) # if LinearSlow()
# If the subtype hasn't defined the required method, it falls back to the
# _getindex function again where an error is thrown to prevent stack overflows.

function getindex(A::AbstractArray, I...)
    @_propagate_inbounds_meta
    _getindex(linearindexing(A), A, I...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    @inbounds r = getindex(A, I...)
    r
end
## Internal definitions
_getindex(::LinearIndexing, A::AbstractArray, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing: canonical method is one Int
_getindex(::LinearFast, A::AbstractVector, ::Int) = error("indexing not defined for ", typeof(A))
_getindex(::LinearFast, A::AbstractArray,  ::Int) = error("indexing not defined for ", typeof(A))
_getindex{T}(::LinearFast, A::AbstractArray{T,0}) = A[1]
_getindex(::LinearFast, A::AbstractArray, i::Real) = (@_propagate_inbounds_meta; getindex(A, to_index(i)))
function _getindex{T,N}(::LinearFast, A::AbstractArray{T,N}, I::Vararg{Real,N})
    # We must check bounds for sub2ind; so we can then use @inbounds
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = getindex(A, sub2ind(A, J...))
    r
end
function _getindex(::LinearFast, A::AbstractVector, I1::Real, I::Real...)
    @_inline_meta
    J = to_indexes(I1, I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = getindex(A, J[1])
    r
end
function _getindex(::LinearFast, A::AbstractArray, I::Real...) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = getindex(A, sub2ind(A, J...))
    r
end


## LinearSlow Scalar indexing: Canonical method is full dimensionality of Ints
_getindex{T,N}(::LinearSlow, A::AbstractArray{T,N}, ::Vararg{Int, N}) = error("indexing not defined for ", typeof(A))
_getindex{T,N}(::LinearSlow, A::AbstractArray{T,N}, I::Vararg{Real, N}) = (@_propagate_inbounds_meta; getindex(A, to_indexes(I...)...))
function _getindex(::LinearSlow, A::AbstractArray, i::Real)
    # ind2sub requires all dimensions to be > 0; may as well just check bounds
    @_inline_meta
    @boundscheck checkbounds(A, i)
    @inbounds r = getindex(A, ind2sub(A, to_index(i))...)
    r
end
@generated function _getindex{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, I::Real...) # TODO: DEPRECATE FOR #14770
    N = length(I)
    if N > AN
        # Drop trailing ones
        Isplat = Expr[:(I[$d]) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            @_propagate_inbounds_meta
            @boundscheck (&)($(Osplat...)) || throw_boundserror(A, I)
            getindex(A, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(I[$d]) for d = 1:N-1]
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=max(N,1):AN]
        szcheck = Expr[:(size(A, $d) > 0) for d=max(N,1):AN]
        last_idx = N > 0 ? :(to_index(I[$N])) : 1
        quote
            # ind2sub requires all dimensions to be > 0:
            @_propagate_inbounds_meta
            @boundscheck (&)($(szcheck...)) || throw_boundserror(A, I)
            getindex(A, $(Isplat...), ind2sub($sz, $last_idx)...)
        end
    end
end

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage
function setindex!(A::AbstractArray, v, I...)
    @_propagate_inbounds_meta
    _setindex!(linearindexing(A), A, v, I...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    @inbounds r = setindex!(A, v, I...)
    r
end
## Internal defitions
_setindex!(::LinearIndexing, A::AbstractArray, v, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing
_setindex!(::LinearFast, A::AbstractVector, v, ::Int) = error("indexed assignment not defined for ", typeof(A))
_setindex!(::LinearFast, A::AbstractArray, v, ::Int) = error("indexed assignment not defined for ", typeof(A))
_setindex!{T}(::LinearFast, A::AbstractArray{T,0}, v) = (@_propagate_inbounds_meta; setindex!(A, v, 1))
_setindex!(::LinearFast, A::AbstractArray, v, i::Real) = (@_propagate_inbounds_meta; setindex!(A, v, to_index(i)))
function _setindex!{T,N}(::LinearFast, A::AbstractArray{T,N}, v, I::Vararg{Real,N})
    # We must check bounds for sub2ind; so we can then use @inbounds
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = setindex!(A, v, sub2ind(A, J...))
    r
end
function _setindex!(::LinearFast, A::AbstractVector, v, I1::Real, I::Real...)
    @_inline_meta
    J = to_indexes(I1, I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = setindex!(A, v, J[1])
    r
end
function _setindex!(::LinearFast, A::AbstractArray, v, I::Real...) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = setindex!(A, v, sub2ind(A, J...))
    r
end

# LinearSlow Scalar indexing
_setindex!{T,N}(::LinearSlow, A::AbstractArray{T,N}, v, ::Vararg{Int, N}) = error("indexed assignment not defined for ", typeof(A))
_setindex!{T,N}(::LinearSlow, A::AbstractArray{T,N}, v, I::Vararg{Real, N}) = (@_propagate_inbounds_meta; setindex!(A, v, to_indexes(I...)...))
function _setindex!(::LinearSlow, A::AbstractArray, v, i::Real)
    # ind2sub requires all dimensions to be > 0; may as well just check bounds
    @_inline_meta
    @boundscheck checkbounds(A, i)
    @inbounds r = setindex!(A, v, ind2sub(A, to_index(i))...)
    r
end
@generated function _setindex!{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, v, I::Real...) # TODO: DEPRECATE FOR #14770
    N = length(I)
    if N > AN
        # Drop trailing ones
        Isplat = Expr[:(I[$d]) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            # We only check the trailing ones, so just propagate @inbounds state
            @_propagate_inbounds_meta
            @boundscheck (&)($(Osplat...)) || throw_boundserror(A, I)
            setindex!(A, v, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(I[$d]) for d = 1:N-1]
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=max(N,1):AN]
        szcheck = Expr[:(size(A, $d) > 0) for d=max(N,1):AN]
        last_idx = N > 0 ? :(to_index(I[$N])) : 1
        quote
            # ind2sub requires all dimensions to be > 0:
            @_propagate_inbounds_meta
            @boundscheck (&)($(szcheck...)) || throw_boundserror(A, I)
            setindex!(A, v, $(Isplat...), ind2sub($sz, $last_idx)...)
        end
    end
end

## get (getindex with a default value) ##

typealias RangeVecIntList{A<:AbstractVector{Int}} Union{Tuple{Vararg{Union{Range, AbstractVector{Int}}}}, AbstractVector{UnitRange{Int}}, AbstractVector{Range{Int}}, AbstractVector{A}}

get(A::AbstractArray, i::Integer, default) = checkbounds(Bool, A, i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = checkbounds(Bool, A, I...) ? A[I...] : default

function get!{T}(X::AbstractVector{T}, A::AbstractVector, I::Union{Range, AbstractVector{Int}}, default::T)
    # 1d is not linear indexing
    ind = findin(I, indices1(A))
    X[ind] = A[I[ind]]
    Xind = indices1(X)
    X[first(Xind):first(ind)-1] = default
    X[last(ind)+1:last(Xind)] = default
    X
end
function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::Union{Range, AbstractVector{Int}}, default::T)
    # Linear indexing
    ind = findin(I, 1:length(A))
    X[ind] = A[I[ind]]
    X[1:first(ind)-1] = default
    X[last(ind)+1:length(X)] = default
    X
end

get(A::AbstractArray, I::Range, default) = get!(similar(A, typeof(default), index_shape(A, I)), A, I, default)

# TODO: DEPRECATE FOR #14770 (just the partial linear indexing part)
function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::RangeVecIntList, default::T)
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end

get(A::AbstractArray, I::RangeVecIntList, default) = get!(similar(A, typeof(default), index_shape(A, I...)), A, I, default)

## structured matrix methods ##
replace_in_print_matrix(A::AbstractMatrix,i::Integer,j::Integer,s::AbstractString) = s
replace_in_print_matrix(A::AbstractVector,i::Integer,j::Integer,s::AbstractString) = s

## Concatenation ##

promote_eltype() = Bottom
promote_eltype(v1, vs...) = promote_type(eltype(v1), promote_eltype(vs...))

#TODO: ERROR CHECK
cat(catdim::Integer) = Array{Any}(0)

vcat() = Array{Any}(0)
hcat() = Array{Any}(0)
typed_vcat{T}(::Type{T}) = Array{T}(0)
typed_hcat{T}(::Type{T}) = Array{T}(0)

## cat: special cases
vcat{T}(X::T...)         = T[ X[i] for i=1:length(X) ]
vcat{T<:Number}(X::T...) = T[ X[i] for i=1:length(X) ]
hcat{T}(X::T...)         = T[ X[j] for i=1, j=1:length(X) ]
hcat{T<:Number}(X::T...) = T[ X[j] for i=1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(length(X)), X)
hcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(1,length(X)), X)
typed_vcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T}(length(X)), X)
typed_hcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T}(1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat{T}(V::AbstractVector{T}...) = typed_vcat(T, V...)

function typed_vcat{T}(::Type{T}, V::AbstractVector...)
    n::Int = 0
    for Vk in V
        n += length(Vk)
    end
    a = similar(full(V[1]), T, n)
    pos = 1
    for k=1:length(V)
        Vk = V[k]
        p1 = pos+length(Vk)-1
        a[pos:p1] = Vk
        pos = p1+1
    end
    a
end

hcat(A::AbstractVecOrMat...) = typed_hcat(promote_eltype(A...), A...)
hcat{T}(A::AbstractVecOrMat{T}...) = typed_hcat(T, A...)

function typed_hcat{T}(::Type{T}, A::AbstractVecOrMat...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        if size(Aj, 1) != nrows
            throw(ArgumentError("number of rows of each array must match (got $(map(x->size(x,1), A)))"))
        end
        dense &= isa(Aj,Array)
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
    end
    B = similar(full(A[1]), T, nrows, ncols)
    pos = 1
    if dense
        for k=1:nargs
            Ak = A[k]
            n = length(Ak)
            copy!(B, pos, Ak, 1, n)
            pos += n
        end
    else
        for k=1:nargs
            Ak = A[k]
            p1 = pos+(isa(Ak,AbstractMatrix) ? size(Ak, 2) : 1)-1
            B[:, pos:p1] = Ak
            pos = p1+1
        end
    end
    return B
end

vcat(A::AbstractMatrix...) = typed_vcat(promote_eltype(A...), A...)
vcat{T}(A::AbstractMatrix{T}...) = typed_vcat(T, A...)

function typed_vcat{T}(::Type{T}, A::AbstractMatrix...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols
            throw(ArgumentError("number of columns of each array must match (got $(map(x->size(x,2), A)))"))
        end
    end
    B = similar(full(A[1]), T, nrows, ncols)
    pos = 1
    for k=1:nargs
        Ak = A[k]
        p1 = pos+size(Ak,1)-1
        B[pos:p1, :] = Ak
        pos = p1+1
    end
    return B
end

## cat: general case

function cat(catdims, X...)
    T = promote_type(map(x->isa(x,AbstractArray) ? eltype(x) : typeof(x), X)...)
    cat_t(catdims, T, X...)
end

function cat_t(catdims, typeC::Type, X...)
    catdims = collect(catdims)
    nargs = length(X)
    ndimsX = Int[isa(a,AbstractArray) ? ndims(a) : 0 for a in X]
    ndimsC = max(maximum(ndimsX), maximum(catdims))
    catsizes = zeros(Int,(nargs,length(catdims)))
    dims2cat = zeros(Int,ndimsC)
    for k = 1:length(catdims)
        dims2cat[catdims[k]]=k
    end

    dimsC = Int[d <= ndimsX[1] ? size(X[1],d) : 1 for d=1:ndimsC]
    for k = 1:length(catdims)
        catsizes[1,k] = dimsC[catdims[k]]
    end
    for i = 2:nargs
        for d = 1:ndimsC
            currentdim = (d <= ndimsX[i] ? size(X[i],d) : 1)
            if dims2cat[d] != 0
                dimsC[d] += currentdim
                catsizes[i,dims2cat[d]] = currentdim
            elseif dimsC[d] != currentdim
                throw(DimensionMismatch(string("mismatch in dimension ",d,
                                               " (expected ",dimsC[d],
                                               " got ",currentdim,")")))
            end
        end
    end

    C = similar(isa(X[1],AbstractArray) ? full(X[1]) : [X[1]], typeC, tuple(dimsC...))
    if length(catdims)>1
        fill!(C,0)
    end

    offsets = zeros(Int,length(catdims))
    for i=1:nargs
        cat_one = [ dims2cat[d] == 0 ? (1:dimsC[d]) : (offsets[dims2cat[d]]+(1:catsizes[i,dims2cat[d]]))
                   for d=1:ndimsC ]
        C[cat_one...] = X[i]
        for k = 1:length(catdims)
            offsets[k] += catsizes[i,k]
        end
    end
    return C
end

vcat(X...) = cat(1, X...)
hcat(X...) = cat(2, X...)

typed_vcat(T::Type, X...) = cat_t(1, T, X...)
typed_hcat(T::Type, X...) = cat_t(2, T, X...)

cat{T}(catdims, A::AbstractArray{T}...) = cat_t(catdims, T, A...)

cat(catdims, A::AbstractArray...) = cat_t(catdims, promote_eltype(A...), A...)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(1, A)
vcat(A::AbstractArray, B::AbstractArray) = cat(1, A, B)
vcat(A::AbstractArray...) = cat(1, A...)
hcat(A::AbstractArray) = cat(2, A)
hcat(A::AbstractArray, B::AbstractArray) = cat(2, A, B)
hcat(A::AbstractArray...) = cat(2, A...)

typed_vcat(T::Type, A::AbstractArray) = cat_t(1, T, A)
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(1, T, A, B)
typed_vcat(T::Type, A::AbstractArray...) = cat_t(1, T, A...)
typed_hcat(T::Type, A::AbstractArray) = cat_t(2, T, A)
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(2, T, A, B)
typed_hcat(T::Type, A::AbstractArray...) = cat_t(2, T, A...)

# 2d horizontal and vertical concatenation

function hvcat(nbc::Integer, as...)
    # nbc = # of block columns
    n = length(as)
    mod(n,nbc) != 0 &&
        throw(ArgumentError("number of arrays $n is not a multiple of the requested number of block columns $nbc"))
    nbr = div(n,nbc)
    hvcat(ntuple(i->nbc, nbr), as...)
end

hvcat(rows::Tuple{Vararg{Int}}, xs::AbstractMatrix...) = typed_hvcat(promote_eltype(xs...), rows, xs...)
hvcat{T}(rows::Tuple{Vararg{Int}}, xs::AbstractMatrix{T}...) = typed_hvcat(T, rows, xs...)

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as::AbstractMatrix...)
    nbr = length(rows)  # number of block rows

    nc = 0
    for i=1:rows[1]
        nc += size(as[i],2)
    end

    nr = 0
    a = 1
    for i = 1:nbr
        nr += size(as[a],1)
        a += rows[i]
    end

    out = similar(full(as[1]), T, nr, nc)

    a = 1
    r = 1
    for i = 1:nbr
        c = 1
        szi = size(as[a],1)
        for j = 1:rows[i]
            Aj = as[a+j-1]
            szj = size(Aj,2)
            if size(Aj,1) != szi
                throw(ArgumentError("mismatched height in block row $(i) (expected $szi, got $(size(Aj,1)))"))
            end
            if c-1+szj > nc
                throw(ArgumentError("block row $(i) has mismatched number of columns (expected $nc, got $(c-1+szj))"))
            end
            out[r:r-1+szi, c:c-1+szj] = Aj
            c += szj
        end
        if c != nc+1
            throw(ArgumentError("block row $(i) has mismatched number of columns (expected $nc, got $(c-1))"))
        end
        r += szi
        a += rows[i]
    end
    out
end

hvcat(rows::Tuple{Vararg{Int}}) = []
typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}) = Array{T}(0)

function hvcat{T<:Number}(rows::Tuple{Vararg{Int}}, xs::T...)
    nr = length(rows)
    nc = rows[1]

    a = Array{T}(nr, nc)
    if length(a) != length(xs)
        throw(ArgumentError("argument count does not match specified shape (expected $(length(a)), got $(length(xs)))"))
    end
    k = 1
    @inbounds for i=1:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns (expected $nc, got $(rows[i]))"))
        end
        for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

function hvcat_fill(a::Array, xs::Tuple)
    k = 1
    nr, nc = size(a,1), size(a,2)
    for i=1:nr
        @inbounds for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

hvcat(rows::Tuple{Vararg{Int}}, xs::Number...) = typed_hvcat(promote_typeof(xs...), rows, xs...)

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, xs::Number...)
    nr = length(rows)
    nc = rows[1]
    for i = 2:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns (expected $nc, got $(rows[i]))"))
        end
    end
    len = length(xs)
    if nr*nc != len
        throw(ArgumentError("argument count $(len) does not match specified shape $((nr,nc))"))
    end
    hvcat_fill(Array{T}(nr, nc), xs)
end

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    T[rs...;]
end

## Reductions and scans ##

function isequal(A::AbstractArray, B::AbstractArray)
    if A === B return true end
    if indices(A) != indices(B)
        return false
    end
    if isa(A,Range) != isa(B,Range)
        return false
    end
    for (a, b) in zip(A, B)
        if !isequal(a, b)
            return false
        end
    end
    return true
end

function lexcmp(A::AbstractArray, B::AbstractArray)
    for (a, b) in zip(A, B)
        res = lexcmp(a, b)
        res == 0 || return res
    end
    return cmp(length(A), length(B))
end

function (==)(A::AbstractArray, B::AbstractArray)
    if indices(A) != indices(B)
        return false
    end
    if isa(A,Range) != isa(B,Range)
        return false
    end
    for (a, b) in zip(A, B)
        if !(a == b)
            return false
        end
    end
    return true
end

# sub2ind and ind2sub
# fallbacks
function sub2ind(A::AbstractArray, I...)
    @_inline_meta
    sub2ind(indicesbehavior(A), A, I...)
end
function sub2ind(::IndicesStartAt1, A::AbstractArray, I...)
    @_inline_meta
    sub2ind(size(A), I...)
end
function sub2ind(::IndicesBehavior, A::AbstractArray, I...)
    @_inline_meta
    sub2ind(indices(A), I...)
end
function ind2sub(A::AbstractArray, ind)
    @_inline_meta
    ind2sub(indicesbehavior(A), A, ind)
end
function ind2sub(::IndicesStartAt1, A::AbstractArray, ind)
    @_inline_meta
    ind2sub(size(A), ind)
end
function ind2sub(::IndicesBehavior, A::AbstractArray, ind)
    @_inline_meta
    ind2sub(indices(A), ind)
end

sub2ind(::Tuple{}) = 1
sub2ind(::DimsInteger) = 1
sub2ind(::Indices) = 1
sub2ind(::Tuple{}, I::Integer...) = (@_inline_meta; _sub2ind((), 1, 1, I...))
sub2ind(dims::DimsInteger, I::Integer...) = (@_inline_meta; _sub2ind(dims, 1, 1, I...))
sub2ind(inds::Indices, I::Integer...) = (@_inline_meta; _sub2ind(inds, 1, 1, I...))
# In 1d, there's a question of whether we're doing cartesian indexing or linear indexing. Support only the former.
sub2ind(inds::Indices{1}, I::Integer...) = throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))

_sub2ind(::Any, L, ind) = ind
function _sub2ind(::Tuple{}, L, ind, i::Integer, I::Integer...)
    @_inline_meta
    _sub2ind((), L, ind+(i-1)*L, I...)
end
function _sub2ind(inds, L, ind, i::Integer, I::Integer...)
    @_inline_meta
    r1 = inds[1]
    _sub2ind(tail(inds), nextL(L, r1), ind+offsetin(i, r1)*L, I...)
end

nextL(L, l::Integer) = L*l
nextL(L, r::AbstractUnitRange) = L*unsafe_length(r)
offsetin(i, l::Integer) = i-1
offsetin(i, r::AbstractUnitRange) = i-first(r)
unsafe_length(r::UnitRange) = r.stop-r.start+1
unsafe_length(r::OneTo) = length(r)

ind2sub(::Tuple{}, ind::Integer) = (@_inline_meta; ind == 1 ? () : throw(BoundsError()))
ind2sub(dims::DimsInteger, ind::Integer) = (@_inline_meta; _ind2sub((), dims, ind-1))
ind2sub(inds::Indices, ind::Integer) = (@_inline_meta; _ind2sub((), inds, ind-1))
ind2sub(inds::Indices{1}, ind::Integer) = throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))

_ind2sub(::Tuple{}, ::Tuple{}, ind) = (ind+1,)
function _ind2sub(out, indslast::NTuple{1}, ind)
    @_inline_meta
    (out..., _lookup(ind, indslast[1]))
end
function _ind2sub(out, inds, ind)
    @_inline_meta
    r1 = inds[1]
    indnext, f, l = _div(ind, r1)
    _ind2sub((out..., ind-l*indnext+f), tail(inds), indnext)
end

_lookup(ind, d::Integer) = ind+1
_lookup(ind, r::AbstractUnitRange) = ind+first(r)
_div(ind, d::Integer) = div(ind, d), 1, d
_div(ind, r::AbstractUnitRange) = (d = unsafe_length(r); (div(ind, d), first(r), d))

smart_ind2sub(shape::NTuple{1}, ind) = (ind,)
smart_ind2sub(shape, ind) = ind2sub(shape, ind)
smart_sub2ind(shape::NTuple{1}, i) = (i,)
smart_sub2ind(shape, I...) = (@_inline_meta; sub2ind(shape, I...))

# Vectorized forms
function sub2ind{N,T<:Integer}(inds::Union{Dims{N},Indices{N}}, I::AbstractVector{T}...)
    I1 = I[1]
    Iinds = indices1(I1)
    for j = 2:length(I)
        indices1(I[j]) == Iinds || throw(DimensionMismatch("indices of I[1] ($(Iinds)) does not match indices of I[$j] ($(indices1(I[j])))"))
    end
    Iout = similar(I1)
    _sub2ind!(Iout, inds, Iinds, I)
    Iout
end

function _sub2ind!(Iout, inds, Iinds, I)
    @_noinline_meta
    for i in Iinds
        # Iout[i] = sub2ind(inds, map(Ij->Ij[i], I)...)
        Iout[i] = sub2ind_vec(inds, i, I)
    end
    Iout
end

sub2ind_vec(inds, i, I) = (@_inline_meta; _sub2ind_vec(inds, (), i, I...))
_sub2ind_vec(inds, out, i, I1, I...) = (@_inline_meta; _sub2ind_vec(inds, (out..., I1[i]), i, I...))
_sub2ind_vec(inds, out, i) = (@_inline_meta; sub2ind(inds, out...))

function ind2sub{N,T<:Integer}(inds::Union{Dims{N},Indices{N}}, ind::AbstractVector{T})
    M = length(ind)
    t = ntuple(n->similar(ind),Val{N})
    for (i,idx) in enumerate(ind)  # FIXME: change to eachindexvalue
        sub = ind2sub(inds, idx)
        for j = 1:N
            t[j][i] = sub[j]
        end
    end
    t
end

function ind2sub!{T<:Integer}(sub::Array{T}, dims::Tuple{Vararg{T}}, ind::T)
    ndims = length(dims)
    for i=1:ndims-1
        ind2 = div(ind-1,dims[i])+1
        sub[i] = ind - dims[i]*(ind2-1)
        ind = ind2
    end
    sub[ndims] = ind
    return sub
end

## iteration utilities ##

"""
    foreach(f, c...) -> Void

Call function `f` on each element of iterable `c`.
For multiple iterable arguments, `f` is called elementwise.
`foreach` should be used instead of `map` when the results of `f` are not
needed, for example in `foreach(println, array)`.
"""
foreach(f) = (f(); nothing)
foreach(f, itr) = (for x in itr; f(x); end; nothing)
foreach(f, itrs...) = (for z in zip(itrs...); f(z...); end; nothing)

## map over arrays ##

## transform any set of dimensions
## dims specifies which dimensions will be transformed. for example
## dims==1:2 will call f on all slices A[:,:,...]
mapslices(f, A::AbstractArray, dims) = mapslices(f, A, [dims...])
function mapslices(f, A::AbstractArray, dims::AbstractVector)
    if isempty(dims)
        return map(f,A)
    end

    dimsA = [shape(A)...]
    ndimsA = ndims(A)
    alldims = [1:ndimsA;]

    otherdims = setdiff(alldims, dims)

    idx = Any[first(ind) for ind in indices(A)]
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = Colon()
    end

    Aslice = A[idx...]
    r1 = f(Aslice)

    # determine result size and allocate
    Rsize = copy(dimsA)
    # TODO: maybe support removing dimensions
    if !isa(r1, AbstractArray) || ndims(r1) == 0
        r1 = [r1]
    end
    nextra = max(0,length(dims)-ndims(r1))
    if eltype(Rsize) == Int
        Rsize[dims] = [size(r1)..., ntuple(d->1, nextra)...]
    else
        Rsize[dims] = [indices(r1)..., ntuple(d->1:1, nextra)...]
    end
    R = similar(r1, tuple(Rsize...,))

    ridx = Any[map(first, indices(R))...]
    for d in dims
        ridx[d] = indices(R,d)
    end

    R[ridx...] = r1

    isfirst = true
    nidx = length(otherdims)
    for I in CartesianRange(itershape)
        if isfirst
            isfirst = false
        else
            for i in 1:nidx
                idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
            end
            _unsafe_getindex!(Aslice, A, idx...)
            R[ridx...] = f(Aslice)
        end
    end

    return R
end

# These are needed because map(eltype, As) is not inferrable
promote_eltype_op(::Any) = (@_pure_meta; Bottom)
promote_eltype_op{T}(op, ::AbstractArray{T}) = (@_pure_meta; promote_op(op, T))
promote_eltype_op{T}(op, ::T               ) = (@_pure_meta; promote_op(op, T))
promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::AbstractArray{S}) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::S) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op{R,S}(op, ::R, ::AbstractArray{S}) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op(op, A, B, C, D...) = (@_pure_meta; promote_op(op, eltype(A), promote_eltype_op(op, B, C, D...)))

## 1 argument
map!{F}(f::F, A::AbstractArray) = map!(f, A, A)
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray)
    for (i,j) in zip(eachindex(dest),eachindex(A))
        dest[i] = f(A[j])
    end
    return dest
end

# map on collections
map(f, A::Union{AbstractArray,AbstractSet,Associative}) = collect_similar(A, Generator(f,A))

# default to returning an Array for `map` on general iterators
map(f, A) = collect(Generator(f,A))

## 2 argument
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    for (i, j, k) in zip(eachindex(dest), eachindex(A), eachindex(B))
        dest[i] = f(A[j], B[k])
    end
    return dest
end

## N argument

ith_all(i, ::Tuple{}) = ()
ith_all(i, as) = (as[1][i], ith_all(i, tail(as))...)

function map_n!{F}(f::F, dest::AbstractArray, As)
    for i = linearindices(As[1])
        dest[i] = f(ith_all(i, As)...)
    end
    return dest
end

map!{F}(f::F, dest::AbstractArray, As::AbstractArray...) = map_n!(f, dest, As)

map(f, iters...) = collect(Generator(f, iters...))

# multi-item push!, unshift! (built on top of type-specific 1-item version)
# (note: must not cause a dispatch loop when 1-item case is not defined)
push!(A, a, b) = push!(push!(A, a), b)
push!(A, a, b, c...) = push!(push!(A, a, b), c...)
unshift!(A, a, b) = unshift!(unshift!(A, b), a)
unshift!(A, a, b, c...) = unshift!(unshift!(A, c...), a, b)

## hashing collections ##

const hashaa_seed = UInt === UInt64 ? 0x7f53e68ceb575e76 : 0xeb575e76
const hashrle_seed = UInt == UInt64 ? 0x2aab8909bfea414c : 0xbfea414c
function hash(a::AbstractArray, h::UInt)
    h += hashaa_seed
    h += hash(size(a))

    state = start(a)
    done(a, state) && return h
    x2, state = next(a, state)
    done(a, state) && return hash(x2, h)

    x1 = x2
    while !done(a, state)
        x1 = x2
        x2, state = next(a, state)
        if isequal(x2, x1)
            # For repeated elements, use run length encoding
            # This allows efficient hashing of sparse arrays
            runlength = 2
            while !done(a, state)
                x2, state = next(a, state)
                isequal(x1, x2) || break
                runlength += 1
            end
            h += hashrle_seed
            h = hash(runlength, h)
        end
        h = hash(x1, h)
    end
    !isequal(x2, x1) && (h = hash(x2, h))
    return h
end
