# This file is a part of Julia. License is MIT: http://julialang.org/license

## Basic functions ##

"""
    size(A::AbstractArray, [dim...])

Returns a tuple containing the dimensions of `A`. Optionally you can specify the
dimension(s) you want the length of, and get the length of that dimension, or a tuple of the
lengths of dimensions you asked for.

```jldoctest
julia> A = ones(2,3,4);

julia> size(A, 2)
3

julia> size(A,3,2)
(4, 3)
```
"""
size{T,N}(t::AbstractArray{T,N}, d) = d <= N ? size(t)[d] : 1
size{N}(x, d1::Integer, d2::Integer, dx::Vararg{Integer, N}) = (size(x, d1), size(x, d2), ntuple(k->size(x, dx[k]), Val{N})...)

"""
    indices(A, d)

Returns the valid range of indices for array `A` along dimension `d`.

```jldoctest
julia> A = ones(5,6,7);

julia> indices(A,2)
Base.OneTo(6)
```
"""
function indices{T,N}(A::AbstractArray{T,N}, d)
    @_inline_meta
    d <= N ? indices(A)[d] : OneTo(1)
end

"""
    indices(A)

Returns the tuple of valid indices for array `A`.

```jldoctest
julia> A = ones(5,6,7);

julia> indices(A)
(Base.OneTo(5), Base.OneTo(6), Base.OneTo(7))
```
"""
function indices(A)
    @_inline_meta
    map(OneTo, size(A))
end

# Performance optimization: get rid of a branch on `d` in `indices(A,
# d)` for d=1. 1d arrays are heavily used, and the first dimension
# comes up in other applications.
indices1{T}(A::AbstractArray{T,0}) = OneTo(1)
indices1{T}(A::AbstractArray{T})   = (@_inline_meta; indices(A)[1])
indices1(iter) = OneTo(length(iter))

unsafe_indices(A) = indices(A)
unsafe_indices(r::Range) = (OneTo(unsafe_length(r)),) # Ranges use checked_sub for size

"""
    linearindices(A)

Returns a `UnitRange` specifying the valid range of indices for `A[i]`
where `i` is an `Int`. For arrays with conventional indexing (indices
start at 1), or any multidimensional array, this is `1:length(A)`;
however, for one-dimensional arrays with unconventional indices, this
is `indices(A, 1)`.

Calling this function is the "safe" way to write algorithms that
exploit linear indexing.

```jldoctest
julia> A = ones(5,6,7);

julia> b = linearindices(A);

julia> extrema(b)
(1, 210)
```
"""
linearindices(A)                 = (@_inline_meta; OneTo(_length(A)))
linearindices(A::AbstractVector) = (@_inline_meta; indices1(A))
eltype(::Type{A}) where A<:AbstractArray{E} where E  = E
elsize{T}(::AbstractArray{T}) = sizeof(T)

"""
    ndims(A::AbstractArray) -> Integer

Returns the number of dimensions of `A`.

```jldoctest
julia> A = ones(3,4,5);

julia> ndims(A)
3
```
"""
ndims{T,N}(::AbstractArray{T,N}) = N
ndims{T,N}(::Type{AbstractArray{T,N}}) = N
ndims{T<:AbstractArray}(::Type{T}) = ndims(supertype(T))

"""
    length(A::AbstractArray) -> Integer

Returns the number of elements in `A`.

```jldoctest
julia> A = ones(3,4,5);

julia> length(A)
60
```
"""
length(t::AbstractArray) = (@_inline_meta; prod(size(t)))
_length(A::AbstractArray) = (@_inline_meta; prod(map(unsafe_length, indices(A)))) # circumvent missing size
_length(A) = (@_inline_meta; length(A))
endof(a::AbstractArray) = (@_inline_meta; length(a))
first(a::AbstractArray) = a[first(eachindex(a))]

"""
    first(coll)

Get the first element of an iterable collection. Returns the start point of a
`Range` even if it is empty.

```jldoctest
julia> first(2:2:10)
2

julia> first([1; 2; 3; 4])
1
```
"""
function first(itr)
    state = start(itr)
    done(itr, state) && throw(ArgumentError("collection must be non-empty"))
    next(itr, state)[1]
end

"""
    last(coll)

Get the last element of an ordered collection, if it can be computed in O(1) time. This is
accomplished by calling [`endof`](@ref) to get the last index. Returns the end
point of a `Range` even if it is empty.

```jldoctest
julia> last(1:2:10)
9

julia> last([1; 2; 3; 4])
4
```
"""
last(a) = a[end]

"""
    stride(A, k::Integer)

Returns the distance in memory (in number of elements) between adjacent elements in dimension `k`.

```jldoctest
julia> A = ones(3,4,5);

julia> stride(A,2)
3

julia> stride(A,3)
12
```
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

```jldoctest
julia> A = ones(3,4,5);

julia> strides(A)
(1, 3, 12)
```
"""
strides(A::AbstractArray) = _strides((1,), A)
_strides{T,N}(out::NTuple{N,Any}, A::AbstractArray{T,N}) = out
function _strides{M,T,N}(out::NTuple{M}, A::AbstractArray{T,N})
    @_inline_meta
    _strides((out..., out[M]*size(A, M)), A)
end

function isassigned(a::AbstractArray, i::Int...)
    try
        a[i...]
        true
    catch e
        if isa(e, BoundsError) || isa(e, UndefRefError)
            return false
        else
            rethrow(e)
        end
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
function trailingsize(inds::Indices, n)
    s = 1
    for i=n:length(inds)
        s *= unsafe_length(inds[i])
    end
    return s
end
# This version is type-stable even if inds is heterogeneous
function trailingsize(inds::Indices)
    @_inline_meta
    prod(map(unsafe_length, inds))
end

## Traits for array types ##

abstract LinearIndexing
immutable LinearFast <: LinearIndexing end
immutable LinearSlow <: LinearIndexing end

"""
    Base.linearindexing(A)

`linearindexing` defines how an AbstractArray most efficiently accesses its elements. If
`Base.linearindexing(A)` returns `Base.LinearFast()`, this means that linear indexing with
only one index is an efficient operation. If it instead returns `Base.LinearSlow()` (by
default), this means that the array intrinsically accesses its elements with indices
specified for every dimension. Since converting a linear index to multiple indexing
subscripts is typically very expensive, this provides a traits-based mechanism to enable
efficient generic code for all array types.

An abstract array subtype `MyArray` that wishes to opt into fast linear indexing behaviors
should define `linearindexing` in the type-domain:

    Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()
"""
linearindexing(A::AbstractArray) = linearindexing(typeof(A))
linearindexing(::Type{Union{}}) = LinearFast()
linearindexing{T<:AbstractArray}(::Type{T}) = LinearSlow()
linearindexing{T<:Array}(::Type{T}) = LinearFast()
linearindexing{T<:Range}(::Type{T}) = LinearFast()

linearindexing(A::AbstractArray, B::AbstractArray) = linearindexing(linearindexing(A), linearindexing(B))
linearindexing(A::AbstractArray, B::AbstractArray...) = linearindexing(linearindexing(A), linearindexing(B...))
linearindexing(::LinearFast, ::LinearFast) = LinearFast()
linearindexing(::LinearIndexing, ::LinearIndexing) = LinearSlow()

## Bounds checking ##

# The overall hierarchy is
#     `checkbounds(A, I...)` ->
#         `checkbounds(Bool, A, I...)` ->
#             `checkbounds_indices(Bool, IA, I)`, which recursively calls
#                 `checkindex` for each dimension
#
# See the "boundscheck" devdocs for more information.
#
# Note this hierarchy has been designed to reduce the likelihood of
# method ambiguities.  We try to make `checkbounds` the place to
# specialize on array type, and try to avoid specializations on index
# types; conversely, `checkindex` is intended to be specialized only
# on index type (especially, its last argument).

"""
    checkbounds(Bool, A, I...)

Return `true` if the specified indices `I` are in bounds for the given
array `A`. Subtypes of `AbstractArray` should specialize this method
if they need to provide custom bounds checking behaviors; however, in
many cases one can rely on `A`'s indices and [`checkindex`](@ref).

See also [`checkindex`](@ref).
"""
function checkbounds(::Type{Bool}, A::AbstractArray, I...)
    @_inline_meta
    checkbounds_indices(Bool, indices(A), I)
end
# Linear indexing is explicitly allowed when there is only one (non-cartesian) index
function checkbounds(::Type{Bool}, A::AbstractArray, i)
    @_inline_meta
    checkindex(Bool, linearindices(A), i)
end
# As a special extension, allow using logical arrays that match the source array exactly
function checkbounds{_,N}(::Type{Bool}, A::AbstractArray{_,N}, I::AbstractArray{Bool,N})
    @_inline_meta
    indices(A) == indices(I)
end

"""
    checkbounds(A, I...)

Throw an error if the specified indices `I` are not in bounds for the given array `A`.
"""
function checkbounds(A::AbstractArray, I...)
    @_inline_meta
    checkbounds(Bool, A, I...) || throw_boundserror(A, I)
    nothing
end
checkbounds(A::AbstractArray) = checkbounds(A, 1) # 0-d case

"""
    checkbounds_indices(Bool, IA, I)

Return `true` if the "requested" indices in the tuple `I` fall within
the bounds of the "permitted" indices specified by the tuple
`IA`. This function recursively consumes elements of these tuples,
usually in a 1-for-1 fashion,

    checkbounds_indices(Bool, (IA1, IA...), (I1, I...)) = checkindex(Bool, IA1, I1) &
                                                          checkbounds_indices(Bool, IA, I)

Note that [`checkindex`](@ref) is being used to perform the actual
bounds-check for a single dimension of the array.

There are two important exceptions to the 1-1 rule: linear indexing and
CartesianIndex{N}, both of which may "consume" more than one element
of `IA`.
"""
function checkbounds_indices(::Type{Bool}, IA::Tuple, I::Tuple)
    @_inline_meta
    checkindex(Bool, IA[1], I[1]) & checkbounds_indices(Bool, tail(IA), tail(I))
end
checkbounds_indices(::Type{Bool}, ::Tuple{},  ::Tuple{})    = true
checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple{Any}) = (@_inline_meta; checkindex(Bool, 1:1, I[1]))
function checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple)
    @_inline_meta
    checkindex(Bool, 1:1, I[1]) & checkbounds_indices(Bool, (), tail(I))
end
function checkbounds_indices(::Type{Bool}, IA::Tuple{Any}, I::Tuple{Any})
    @_inline_meta
    checkindex(Bool, IA[1], I[1])
end
function checkbounds_indices(::Type{Bool}, IA::Tuple, I::Tuple{Any})
    @_inline_meta
    checkbounds_linear_indices(Bool, IA, I[1])
end
function checkbounds_linear_indices(::Type{Bool}, IA::Tuple, i)
    @_inline_meta
    if checkindex(Bool, IA[1], i)
        return true
    elseif checkindex(Bool, OneTo(trailingsize(IA)), i)  # partial linear indexing
        partial_linear_indexing_warning_lookup(length(IA))
        return true # TODO: Return false after the above function is removed in deprecated.jl
    end
    return false
end
function checkbounds_linear_indices(::Type{Bool}, IA::Tuple, i::Union{Slice,Colon})
    partial_linear_indexing_warning_lookup(length(IA))
    true
end
checkbounds_indices(::Type{Bool}, ::Tuple, ::Tuple{}) = true

throw_boundserror(A, I) = (@_noinline_meta; throw(BoundsError(A, I)))

# check along a single dimension
"""
    checkindex(Bool, inds::AbstractUnitRange, index)

Return `true` if the given `index` is within the bounds of
`inds`. Custom types that would like to behave as indices for all
arrays can extend this method in order to provide a specialized bounds
checking implementation.

```jldoctest
julia> checkindex(Bool,1:20,8)
true

julia> checkindex(Bool,1:20,21)
false
```
"""
checkindex(::Type{Bool}, inds::AbstractUnitRange, i) = throw(ArgumentError("unable to check bounds for indices of type $(typeof(i))"))
checkindex(::Type{Bool}, inds::AbstractUnitRange, i::Real) = (first(inds) <= i) & (i <= last(inds))
checkindex(::Type{Bool}, inds::AbstractUnitRange, ::Colon) = true
checkindex(::Type{Bool}, inds::AbstractUnitRange, ::Slice) = true
function checkindex(::Type{Bool}, inds::AbstractUnitRange, r::Range)
    @_propagate_inbounds_meta
    isempty(r) | (checkindex(Bool, inds, first(r)) & checkindex(Bool, inds, last(r)))
end
checkindex(::Type{Bool}, indx::AbstractUnitRange, I::AbstractVector{Bool}) = indx == indices1(I)
checkindex(::Type{Bool}, indx::AbstractUnitRange, I::AbstractArray{Bool}) = false
function checkindex(::Type{Bool}, inds::AbstractUnitRange, I::AbstractArray)
    @_inline_meta
    b = true
    for i in I
        b &= checkindex(Bool, inds, i)
    end
    b
end

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

"""
similar{T}(a::AbstractArray{T})                             = similar(a, T)
similar{T}(a::AbstractArray, ::Type{T})                     = similar(a, T, to_shape(indices(a)))
similar{T}(a::AbstractArray{T}, dims::Tuple)                = similar(a, T, to_shape(dims))
similar{T}(a::AbstractArray{T}, dims::DimOrInd...)          = similar(a, T, to_shape(dims))
similar{T}(a::AbstractArray, ::Type{T}, dims::DimOrInd...)  = similar(a, T, to_shape(dims))
similar{T}(a::AbstractArray, ::Type{T}, dims::NeedsShaping) = similar(a, T, to_shape(dims))
# similar creates an Array by default
similar{T,N}(a::AbstractArray, ::Type{T}, dims::Dims{N})    = Array{T,N}(dims)

to_shape(::Tuple{}) = ()
to_shape(dims::Dims) = dims
to_shape(dims::DimsOrInds) = map(to_shape, dims)
# each dimension
to_shape(i::Int) = i
to_shape(i::Integer) = Int(i)
to_shape(r::OneTo) = Int(last(r))
to_shape(r::AbstractUnitRange) = r

"""
    similar(storagetype, indices)

Create an uninitialized mutable array analogous to that specified by
`storagetype`, but with `indices` specified by the last
argument. `storagetype` might be a type or a function.

**Examples**:

    similar(Array{Int}, indices(A))

creates an array that "acts like" an `Array{Int}` (and might indeed be
backed by one), but which is indexed identically to `A`. If `A` has
conventional indexing, this will be identical to
`Array{Int}(size(A))`, but if `A` has unconventional indexing then the
indices of the result will match `A`.

    similar(BitArray, (indices(A, 2),))

would create a 1-dimensional logical array whose indices match those
of the columns of `A`.

    similar(dims->zeros(Int, dims), indices(A))

would create an array of `Int`, initialized to zero, matching the
indices of `A`.
"""
similar(f, shape::Tuple) = f(to_shape(shape))
similar(f, dims::DimOrInd...) = similar(f, dims)

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
    copy!(dest, dstart, src, first(linearindices(src)), _length(src))
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


"""
    copymutable(a)

Make a mutable copy of an array or iterable `a`.  For `a::Array`,
this is equivalent to `copy(a)`, but for other array types it may
differ depending on the type of `similar(a)`.  For generic iterables
this is equivalent to `collect(a)`.
"""
function copymutable(a::AbstractArray)
    @_propagate_inbounds_meta
    copy!(similar(a), a)
end
copymutable(itr) = collect(itr)

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
eachindex(A::AbstractVector) = (@_inline_meta(); indices1(A))

"""
    eachindex(A...)

Creates an iterable object for visiting each index of an AbstractArray `A` in an efficient
manner. For array types that have opted into fast linear indexing (like `Array`), this is
simply the range `1:length(A)`. For other array types, this returns a specialized Cartesian
range to efficiently index into the array with indices specified for every dimension. For
other iterables, including strings and dictionaries, this returns an iterator object
supporting arbitrary index types (e.g. unevenly spaced or non-integer indices).

Example for a sparse 2-d array:

```jldoctest
julia> A = sparse([1, 1, 2], [1, 3, 1], [1, 2, -5])
2×3 sparse matrix with 3 Int64 stored entries:
  [1, 1]  =  1
  [2, 1]  =  -5
  [1, 3]  =  2

julia> for iter in eachindex(A)
           @show iter.I[1], iter.I[2]
           @show A[iter]
       end
(iter.I[1], iter.I[2]) = (1, 1)
A[iter] = 1
(iter.I[1], iter.I[2]) = (2, 1)
A[iter] = -5
(iter.I[1], iter.I[2]) = (1, 2)
A[iter] = 0
(iter.I[1], iter.I[2]) = (2, 2)
A[iter] = 0
(iter.I[1], iter.I[2]) = (1, 3)
A[iter] = 2
(iter.I[1], iter.I[2]) = (2, 3)
A[iter] = 0
```

If you supply more than one `AbstractArray` argument, `eachindex` will create an
iterable object that is fast for all arguments (a `UnitRange`
if all inputs have fast linear indexing, a `CartesianRange`
otherwise).
If the arrays have different sizes and/or dimensionalities, `eachindex` returns an
iterable that spans the largest range along each dimension.
"""
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

isempty(a::AbstractArray) = (_length(a) == 0)

## Conversions ##

convert{T,N  }(::Type{AbstractArray{T,N}}, A::AbstractArray{T,N}) = A
convert{T,S,N}(::Type{AbstractArray{T,N}}, A::AbstractArray{S,N}) = copy!(similar(A,T), A)
convert{T,S,N}(::Type{AbstractArray{T  }}, A::AbstractArray{S,N}) = convert(AbstractArray{T,N}, A)

convert{T,N}(::Type{Array}, A::AbstractArray{T,N}) = convert(Array{T,N}, A)

"""
   of_indices(x, y)

Represents the array `y` as an array having the same indices type as `x`.
"""
of_indices(x, y) = similar(dims->y, oftype(indices(x), indices(y)))

full(x::AbstractArray) = x

## range conversions ##

map{T<:Real}(::Type{T}, r::StepRange) = T(r.start):T(r.step):T(last(r))
map{T<:Real}(::Type{T}, r::UnitRange) = T(r.start):T(last(r))
map{T<:AbstractFloat}(::Type{T}, r::StepRangeLen) = convert(StepRangeLen{T}, r)
function map{T<:AbstractFloat}(::Type{T}, r::LinSpace)
    LinSpace(T(r.start), T(r.stop), length(r))
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
    error_if_canonical_indexing(linearindexing(A), A, I...)
    _getindex(linearindexing(A), A, to_indices(A, I)...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    @inbounds r = getindex(A, I...)
    r
end

error_if_canonical_indexing(::LinearFast, A::AbstractArray, ::Int) = error("indexing not defined for ", typeof(A))
error_if_canonical_indexing{T,N}(::LinearSlow, A::AbstractArray{T,N}, ::Vararg{Int, N}) = error("indexing not defined for ", typeof(A))
error_if_canonical_indexing(::LinearIndexing, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_getindex(::LinearIndexing, A::AbstractArray, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing: canonical method is one Int
_getindex(::LinearFast, A::AbstractArray, i::Int) = (@_propagate_inbounds_meta; getindex(A, i))
_getindex(::LinearFast, A::AbstractArray) = (@_propagate_inbounds_meta; getindex(A, _to_linear_index(A)))
function _getindex(::LinearFast, A::AbstractArray, I::Int...)
    @_inline_meta
    @boundscheck checkbounds(A, I...) # generally _to_linear_index requires bounds checking
    @inbounds r = getindex(A, _to_linear_index(A, I...))
    r
end
_to_linear_index(A::AbstractArray, i::Int) = i
_to_linear_index(A::AbstractVector, i::Int, I::Int...) = i # TODO: DEPRECATE FOR #14770
_to_linear_index{T,N}(A::AbstractArray{T,N}, I::Vararg{Int,N}) = (@_inline_meta; sub2ind(A, I...))
_to_linear_index(A::AbstractArray) = 1 # TODO: DEPRECATE FOR #14770
_to_linear_index(A::AbstractArray, I::Int...) = (@_inline_meta; sub2ind(A, I...)) # TODO: DEPRECATE FOR #14770

## LinearSlow Scalar indexing: Canonical method is full dimensionality of Ints
_getindex(::LinearSlow, A::AbstractArray) = (@_propagate_inbounds_meta; getindex(A, _to_subscript_indices(A)...))
function _getindex(::LinearSlow, A::AbstractArray, I::Int...)
    @_inline_meta
    @boundscheck checkbounds(A, I...) # generally _to_subscript_indices requires bounds checking
    @inbounds r = getindex(A, _to_subscript_indices(A, I...)...)
    r
end
_getindex{T,N}(::LinearSlow, A::AbstractArray{T,N}, I::Vararg{Int, N}) = (@_propagate_inbounds_meta; getindex(A, I...))
_to_subscript_indices(A::AbstractArray, i::Int) = (@_inline_meta; _unsafe_ind2sub(A, i))
_to_subscript_indices{T,N}(A::AbstractArray{T,N}) = (@_inline_meta; fill_to_length((), 1, Val{N})) # TODO: DEPRECATE FOR #14770
_to_subscript_indices{T}(A::AbstractArray{T,0}) = () # TODO: REMOVE FOR #14770
_to_subscript_indices{T}(A::AbstractArray{T,0}, i::Int) = () # TODO: REMOVE FOR #14770
_to_subscript_indices{T}(A::AbstractArray{T,0}, I::Int...) = () # TODO: DEPRECATE FOR #14770
function _to_subscript_indices{T,N}(A::AbstractArray{T,N}, I::Int...) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J, _ = IteratorsMD.split(I, Val{N})    # (maybe) drop any trailing indices
    sz = _remaining_size(J, size(A))       # compute trailing size (overlapping the final index)
    (front(J)..., _unsafe_ind2sub(sz, last(J))...) # (maybe) extend the last index
end
_to_subscript_indices{T,N}(A::AbstractArray{T,N}, I::Vararg{Int,N}) = I
_remaining_size(::Tuple{Any}, t::Tuple) = t
_remaining_size(h::Tuple, t::Tuple) = (@_inline_meta; _remaining_size(tail(h), tail(t)))
_unsafe_ind2sub(::Tuple{}, i) = () # ind2sub may throw(BoundsError()) in this case
_unsafe_ind2sub(sz, i) = (@_inline_meta; ind2sub(sz, i))

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage
function setindex!(A::AbstractArray, v, I...)
    @_propagate_inbounds_meta
    error_if_canonical_indexing(linearindexing(A), A, I...)
    _setindex!(linearindexing(A), A, v, to_indices(A, I)...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    @inbounds r = setindex!(A, v, I...)
    r
end
## Internal defitions
_setindex!(::LinearIndexing, A::AbstractArray, v, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing
_setindex!(::LinearFast, A::AbstractArray, v, i::Int) = (@_propagate_inbounds_meta; setindex!(A, v, i))
_setindex!(::LinearFast, A::AbstractArray, v) = (@_propagate_inbounds_meta; setindex!(A, v, _to_linear_index(A)))
function _setindex!(::LinearFast, A::AbstractArray, v, I::Int...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_linear_index(A, I...))
    r
end

# LinearSlow Scalar indexing
_setindex!{T,N}(::LinearSlow, A::AbstractArray{T,N}, v, I::Vararg{Int, N}) = (@_propagate_inbounds_meta; setindex!(A, v, I...))
_setindex!(::LinearSlow, A::AbstractArray, v) = (@_propagate_inbounds_meta; setindex!(A, v, _to_subscript_indices(A)...))
function _setindex!(::LinearSlow, A::AbstractArray, v, I::Int...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_subscript_indices(A, I...)...)
    r
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

get(A::AbstractArray, I::Range, default) = get!(similar(A, typeof(default), index_shape(I)), A, I, default)

# TODO: DEPRECATE FOR #14770 (just the partial linear indexing part)
function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::RangeVecIntList, default::T)
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end

get(A::AbstractArray, I::RangeVecIntList, default) = get!(similar(A, typeof(default), index_shape(I...)), A, I, default)

## structured matrix methods ##
replace_in_print_matrix(A::AbstractMatrix,i::Integer,j::Integer,s::AbstractString) = s
replace_in_print_matrix(A::AbstractVector,i::Integer,j::Integer,s::AbstractString) = s

## Concatenation ##
eltypeof(x) = typeof(x)
eltypeof(x::AbstractArray) = eltype(x)

promote_eltypeof() = Bottom
promote_eltypeof(v1, vs...) = promote_type(eltypeof(v1), promote_eltypeof(vs...))

promote_eltype() = Bottom
promote_eltype(v1, vs...) = promote_type(eltype(v1), promote_eltype(vs...))

#TODO: ERROR CHECK
cat(catdim::Integer) = Array{Any,1}(0)

typed_vcat{T}(::Type{T}) = Array{T,1}(0)
typed_hcat{T}(::Type{T}) = Array{T,1}(0)

## cat: special cases
vcat{T}(X::T...)         = T[ X[i] for i=1:length(X) ]
vcat{T<:Number}(X::T...) = T[ X[i] for i=1:length(X) ]
hcat{T}(X::T...)         = T[ X[j] for i=1:1, j=1:length(X) ]
hcat{T<:Number}(X::T...) = T[ X[j] for i=1:1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(length(X)), X)
hcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(1,length(X)), X)
typed_vcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T,1}(length(X)), X)
typed_hcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T,2}(1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat{T}(V::AbstractVector{T}...) = typed_vcat(T, V...)

function typed_vcat{T}(::Type{T}, V::AbstractVector...)
    n::Int = 0
    for Vk in V
        n += length(Vk)
    end
    a = similar(V[1], T, n)
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
    B = similar(A[1], T, nrows, ncols)
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
    B = similar(A[1], T, nrows, ncols)
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

# helper functions
cat_size(A) = (1,)
cat_size(A::AbstractArray) = size(A)
cat_size(A, d) = 1
cat_size(A::AbstractArray, d) = size(A, d)

cat_indices(A, d) = OneTo(1)
cat_indices(A::AbstractArray, d) = indices(A, d)

cat_similar(A, T, shape) = Array{T}(shape)
cat_similar(A::AbstractArray, T, shape) = similar(A, T, shape)

cat_shape(dims, shape::Tuple) = shape
@inline cat_shape(dims, shape::Tuple, nshape::Tuple, shapes::Tuple...) =
    cat_shape(dims, _cshp(dims, (), shape, nshape), shapes...)

_cshp(::Tuple{}, out, ::Tuple{}, ::Tuple{}) = out
_cshp(::Tuple{}, out, ::Tuple{}, nshape) = (out..., nshape...)
_cshp(dims, out, ::Tuple{}, ::Tuple{}) = (out..., map(b -> 1, dims)...)
@inline _cshp(dims, out, shape, ::Tuple{}) =
    _cshp(tail(dims), (out..., shape[1] + dims[1]), tail(shape), ())
@inline _cshp(dims, out, ::Tuple{}, nshape) =
    _cshp(tail(dims), (out..., nshape[1]), (), tail(nshape))
@inline function _cshp(::Tuple{}, out, shape, ::Tuple{})
    _cs(length(out) + 1, false, shape[1], 1)
    _cshp((), (out..., 1), tail(shape), ())
end
@inline function _cshp(::Tuple{}, out, shape, nshape)
    next = _cs(length(out) + 1, false, shape[1], nshape[1])
    _cshp((), (out..., next), tail(shape), tail(nshape))
end
@inline function _cshp(dims, out, shape, nshape)
    next = _cs(length(out) + 1, dims[1], shape[1], nshape[1])
    _cshp(tail(dims), (out..., next), tail(shape), tail(nshape))
end

_cs(d, concat, a, b) = concat ? (a + b) : (a == b ? a : throw(DimensionMismatch(string("mismatch in dimension ", d, " (expected ", a, " got ", b, ")"))))

dims2cat{n}(::Type{Val{n}}) = ntuple(i -> (i == n), Val{n})
dims2cat(dims) = ntuple(i -> (i in dims), maximum(dims))

cat(dims, X...) = cat_t(dims, promote_eltypeof(X...), X...)

function cat_t(dims, T::Type, X...)
    catdims = dims2cat(dims)
    shape = cat_shape(catdims, (), map(cat_size, X)...)
    A = cat_similar(X[1], T, shape)
    if T <: Number && countnz(catdims) > 1
        fill!(A, zero(T))
    end
    return _cat(A, shape, catdims, X...)
end

function _cat{N}(A, shape::NTuple{N}, catdims, X...)
    offsets = zeros(Int, N)
    inds = Vector{UnitRange{Int}}(N)
    concat = copy!(zeros(Bool, N), catdims)
    for x in X
        for i = 1:N
            if concat[i]
                inds[i] = offsets[i] + cat_indices(x, i)
                offsets[i] += cat_size(x, i)
            else
                inds[i] = 1:shape[i]
            end
        end
        I::NTuple{N, UnitRange{Int}} = (inds...,)
        A[I...] = x
    end
    return A
end

"""
    vcat(A...)

Concatenate along dimension 1.

```jldoctest
julia> a = [1 2 3 4 5]
1×5 Array{Int64,2}:
 1  2  3  4  5

julia> b = [6 7 8 9 10; 11 12 13 14 15]
2×5 Array{Int64,2}:
  6   7   8   9  10
 11  12  13  14  15

julia> vcat(a,b)
3×5 Array{Int64,2}:
  1   2   3   4   5
  6   7   8   9  10
 11  12  13  14  15

julia> c = ([1 2 3], [4 5 6])
([1 2 3], [4 5 6])

julia> vcat(c...)
2×3 Array{Int64,2}:
 1  2  3
 4  5  6
```
"""
vcat(X...) = cat(Val{1}, X...)
"""
    hcat(A...)

Concatenate along dimension 2.

```jldoctest
julia> a = [1; 2; 3; 4; 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> b = [6 7; 8 9; 10 11; 12 13; 14 15]
5×2 Array{Int64,2}:
  6   7
  8   9
 10  11
 12  13
 14  15

julia> hcat(a,b)
5×3 Array{Int64,2}:
 1   6   7
 2   8   9
 3  10  11
 4  12  13
 5  14  15

julia> c = ([1; 2; 3], [4; 5; 6])
([1, 2, 3], [4, 5, 6])

julia> hcat(c...)
3×2 Array{Int64,2}:
 1  4
 2  5
 3  6
```
"""
hcat(X...) = cat(Val{2}, X...)

typed_vcat(T::Type, X...) = cat_t(Val{1}, T, X...)
typed_hcat(T::Type, X...) = cat_t(Val{2}, T, X...)

cat{T}(catdims, A::AbstractArray{T}...) = cat_t(catdims, T, A...)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(Val{1}, A)
vcat(A::AbstractArray, B::AbstractArray) = cat(Val{1}, A, B)
vcat(A::AbstractArray...) = cat(Val{1}, A...)
hcat(A::AbstractArray) = cat(Val{2}, A)
hcat(A::AbstractArray, B::AbstractArray) = cat(Val{2}, A, B)
hcat(A::AbstractArray...) = cat(Val{2}, A...)

typed_vcat(T::Type, A::AbstractArray) = cat_t(Val{1}, T, A)
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(Val{1}, T, A, B)
typed_vcat(T::Type, A::AbstractArray...) = cat_t(Val{1}, T, A...)
typed_hcat(T::Type, A::AbstractArray) = cat_t(Val{2}, T, A)
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(Val{2}, T, A, B)
typed_hcat(T::Type, A::AbstractArray...) = cat_t(Val{2}, T, A...)

# 2d horizontal and vertical concatenation

function hvcat(nbc::Integer, as...)
    # nbc = # of block columns
    n = length(as)
    mod(n,nbc) != 0 &&
        throw(ArgumentError("number of arrays $n is not a multiple of the requested number of block columns $nbc"))
    nbr = div(n,nbc)
    hvcat(ntuple(i->nbc, nbr), as...)
end

"""
    hvcat(rows::Tuple{Vararg{Int}}, values...)

Horizontal and vertical concatenation in one call. This function is called for block matrix
syntax. The first argument specifies the number of arguments to concatenate in each block
row.

```jldoctest
julia> a, b, c, d, e, f = 1, 2, 3, 4, 5, 6
(1, 2, 3, 4, 5, 6)

julia> [a b c; d e f]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> hvcat((3,3), a,b,c,d,e,f)
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> [a b;c d; e f]
3×2 Array{Int64,2}:
 1  2
 3  4
 5  6

julia> hvcat((2,2,2), a,b,c,d,e,f)
3×2 Array{Int64,2}:
 1  2
 3  4
 5  6
```

If the first argument is a single integer `n`, then all block rows are assumed to have `n`
block columns.
"""
hvcat(rows::Tuple{Vararg{Int}}, xs::AbstractVecOrMat...) = typed_hvcat(promote_eltype(xs...), rows, xs...)
hvcat{T}(rows::Tuple{Vararg{Int}}, xs::AbstractVecOrMat{T}...) = typed_hvcat(T, rows, xs...)

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as::AbstractVecOrMat...)
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

    out = similar(as[1], T, nr, nc)

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
typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}) = Array{T,1}(0)

function hvcat{T<:Number}(rows::Tuple{Vararg{Int}}, xs::T...)
    nr = length(rows)
    nc = rows[1]

    a = Array{T,2}(nr, nc)
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
    hvcat_fill(Array{T,2}(nr, nc), xs)
end

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any,1}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any,1}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = typed_hcat(T, as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    T[rs...;]
end

## Reductions and accumulates ##

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
    sub2ind(indices(A), I...)
end

"""
    ind2sub(a, index) -> subscripts

Returns a tuple of subscripts into array `a` corresponding to the linear index `index`.

```jldoctest
julia> A = ones(5,6,7);

julia> ind2sub(A,35)
(5, 1, 2)

julia> ind2sub(A,70)
(5, 2, 3)
```
"""
function ind2sub(A::AbstractArray, ind)
    @_inline_meta
    ind2sub(indices(A), ind)
end

# 0-dimensional arrays and indexing with []
sub2ind(::Tuple{}) = 1
sub2ind(::DimsInteger) = 1
sub2ind(::Indices) = 1
sub2ind(::Tuple{}, I::Integer...) = (@_inline_meta; _sub2ind((), 1, 1, I...))
# Generic cases

"""
    sub2ind(dims, i, j, k...) -> index

The inverse of [`ind2sub`](@ref), returns the linear index corresponding to the provided subscripts.

```jldoctest
julia> sub2ind((5,6,7),1,2,3)
66

julia> sub2ind((5,6,7),1,6,3)
86
```
"""
sub2ind(dims::DimsInteger, I::Integer...) = (@_inline_meta; _sub2ind(dims, 1, 1, I...))
sub2ind(inds::Indices, I::Integer...) = (@_inline_meta; _sub2ind(inds, 1, 1, I...))
# In 1d, there's a question of whether we're doing cartesian indexing
# or linear indexing. Support only the former.
sub2ind(inds::Indices{1}, I::Integer...) = throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
sub2ind(inds::Tuple{OneTo}, I::Integer...) = (@_inline_meta; _sub2ind(inds, 1, 1, I...)) # only OneTo is safe
sub2ind(inds::Tuple{OneTo}, i::Integer)    = i

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

ind2sub(::Tuple{}, ind::Integer) = (@_inline_meta; ind == 1 ? () : throw(BoundsError()))

"""
    ind2sub(dims, index) -> subscripts

Returns a tuple of subscripts into an array with dimensions `dims`,
corresponding to the linear index `index`.

**Example**:

```
i, j, ... = ind2sub(size(A), indmax(A))
```

provides the indices of the maximum element.

```jldoctest
julia> ind2sub((3,4),2)
(2, 1)

julia> ind2sub((3,4),3)
(3, 1)

julia> ind2sub((3,4),4)
(1, 2)
```
"""
ind2sub(dims::DimsInteger, ind::Integer) = (@_inline_meta; _ind2sub(dims, ind-1))
ind2sub(inds::Indices, ind::Integer)     = (@_inline_meta; _ind2sub(inds, ind-1))
ind2sub(inds::Indices{1}, ind::Integer) = throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
ind2sub(inds::Tuple{OneTo}, ind::Integer) = (ind,)

_ind2sub(::Tuple{}, ind) = (ind+1,)
function _ind2sub(indslast::NTuple{1}, ind)
    @_inline_meta
    (_lookup(ind, indslast[1]),)
end
function _ind2sub(inds, ind)
    @_inline_meta
    r1 = inds[1]
    indnext, f, l = _div(ind, r1)
    (ind-l*indnext+f, _ind2sub(tail(inds), indnext)...)
end

_lookup(ind, d::Integer) = ind+1
_lookup(ind, r::AbstractUnitRange) = ind+first(r)
_div(ind, d::Integer) = div(ind, d), 1, d
_div(ind, r::AbstractUnitRange) = (d = unsafe_length(r); (div(ind, d), first(r), d))

# Vectorized forms
function sub2ind{T<:Integer}(inds::Indices{1}, I1::AbstractVector{T}, I::AbstractVector{T}...)
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
end
sub2ind{T<:Integer}(inds::Tuple{OneTo}, I1::AbstractVector{T}, I::AbstractVector{T}...) = _sub2ind_vecs(inds, I1, I...)
sub2ind{T<:Integer}(inds::Union{DimsInteger,Indices}, I1::AbstractVector{T}, I::AbstractVector{T}...) = _sub2ind_vecs(inds, I1, I...)
function _sub2ind_vecs(inds, I::AbstractVector...)
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

function ind2sub{N,T<:Integer}(inds::Union{DimsInteger{N},Indices{N}}, ind::AbstractVector{T})
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

```jldoctest
julia> a = 1:3:7;

julia> foreach(x->println(x^2),a)
1
16
49
```
"""
foreach(f) = (f(); nothing)
foreach(f, itr) = (for x in itr; f(x); end; nothing)
foreach(f, itrs...) = (for z in zip(itrs...); f(z...); end; nothing)

## map over arrays ##

## transform any set of dimensions
## dims specifies which dimensions will be transformed. for example
## dims==1:2 will call f on all slices A[:,:,...]
"""
    mapslices(f, A, dims)

Transform the given dimensions of array `A` using function `f`. `f` is called on each slice
of `A` of the form `A[...,:,...,:,...]`. `dims` is an integer vector specifying where the
colons go in this expression. The results are concatenated along the remaining dimensions.
For example, if `dims` is `[1,2]` and `A` is 4-dimensional, `f` is called on `A[:,:,i,j]`
for all `i` and `j`.

```jldoctest
julia> a = reshape(collect(1:16),(2,2,2,2))
2×2×2×2 Array{Int64,4}:
[:, :, 1, 1] =
 1  3
 2  4

[:, :, 2, 1] =
 5  7
 6  8

[:, :, 1, 2] =
  9  11
 10  12

[:, :, 2, 2] =
 13  15
 14  16

julia> mapslices(sum, a, [1,2])
1×1×2×2 Array{Int64,4}:
[:, :, 1, 1] =
 10

[:, :, 2, 1] =
 26

[:, :, 1, 2] =
 42

[:, :, 2, 2] =
 58
```
"""
mapslices(f, A::AbstractArray, dims) = mapslices(f, A, [dims...])
function mapslices(f, A::AbstractArray, dims::AbstractVector)
    if isempty(dims)
        return map(f,A)
    end

    dimsA = [indices(A)...]
    ndimsA = ndims(A)
    alldims = [1:ndimsA;]

    otherdims = setdiff(alldims, dims)

    idx = Any[first(ind) for ind in indices(A)]
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = Slice(indices(A, d))
    end

    Aslice = A[idx...]
    r1 = f(Aslice)
    safe_for_reuse = isa(r1, Number) || (isa(r1, AbstractArray) && eltype(r1) <: Number)

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
        Rsize[dims] = [indices(r1)..., ntuple(d->OneTo(1), nextra)...]
    end
    R = similar(r1, tuple(Rsize...,))

    ridx = Any[map(first, indices(R))...]
    for d in dims
        ridx[d] = indices(R,d)
    end

    R[ridx...] = r1

    isfirst = true
    nidx = length(otherdims)
    if safe_for_reuse
        # when f returns an array, R[ridx...] = f(Aslice) line copies elements,
        # so we can reuse Aslice
        for I in CartesianRange(itershape)
            if isfirst
                isfirst = false  # skip the first element, we already handled it
            else
                for i in 1:nidx
                    idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
                end
                _unsafe_getindex!(Aslice, A, idx...)
                R[ridx...] = f(Aslice)
            end
        end
    else
        # we can't guarantee safety (#18524), so allocate new storage for each slice
        for I in CartesianRange(itershape)
            if isfirst
                isfirst = false
            else
                for i in 1:nidx
                    idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
                end
                R[ridx...] = f(A[idx...])
            end
        end
    end

    return R
end

## 1 argument

function map!{F}(f::F, dest::AbstractArray, A::AbstractArray)
    for (i,j) in zip(eachindex(dest),eachindex(A))
        dest[i] = f(A[j])
    end
    return dest
end

# map on collections
map(f, A::Union{AbstractArray,AbstractSet,Associative}) = collect_similar(A, Generator(f,A))

# default to returning an Array for `map` on general iterators
"""
    map(f, c...) -> collection

Transform collection `c` by applying `f` to each element. For multiple collection arguments,
apply `f` elementwise.

```jldoctest
julia> map((x) -> x * 2, [1, 2, 3])
3-element Array{Int64,1}:
 2
 4
 6

julia> map(+, [1, 2, 3], [10, 20, 30])
3-element Array{Int64,1}:
 11
 22
 33
```
"""
map(f, A) = collect(Generator(f,A))

## 2 argument
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    for (i, j, k) in zip(eachindex(dest), eachindex(A), eachindex(B))
        dest[i] = f(A[j], B[k])
    end
    return dest
end

## N argument

@inline ith_all(i, ::Tuple{}) = ()
@inline ith_all(i, as) = (as[1][i], ith_all(i, tail(as))...)

function map_n!{F}(f::F, dest::AbstractArray, As)
    for i = linearindices(As[1])
        dest[i] = f(ith_all(i, As)...)
    end
    return dest
end

"""
    map!(function, destination, collection...)

Like [`map`](@ref), but stores the result in `destination` rather than a new
collection. `destination` must be at least as large as the first collection.
"""
map!{F}(f::F, dest::AbstractArray, As::AbstractArray...) = map_n!(f, dest, As)

map(f) = f()
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
