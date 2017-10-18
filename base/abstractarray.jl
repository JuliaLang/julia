# This file is a part of Julia. License is MIT: https://julialang.org/license

## Basic functions ##

"""
    AbstractArray{T,N}

Supertype for `N`-dimensional arrays (or array-like types) with elements of type `T`.
[`Array`](@ref) and other types are subtypes of this. See the manual section on the
[`AbstractArray` interface](@ref man-interface-array).
"""
AbstractArray

"""
    size(A::AbstractArray, [dim...])

Returns a tuple containing the dimensions of `A`. Optionally you can specify the
dimension(s) you want the length of, and get the length of that dimension, or a tuple of the
lengths of dimensions you asked for.

# Examples
```jldoctest
julia> A = ones(2,3,4);

julia> size(A, 2)
3

julia> size(A,3,2)
(4, 3)
```
"""
size(t::AbstractArray{T,N}, d) where {T,N} = d <= N ? size(t)[d] : 1
size(x, d1::Integer, d2::Integer, dx::Vararg{Integer, N}) where {N} =
    (size(x, d1), size(x, d2), ntuple(k->size(x, dx[k]), Val(N))...)

"""
    indices(A, d)

Returns the valid range of indices for array `A` along dimension `d`.

# Examples
```jldoctest
julia> A = ones(5,6,7);

julia> indices(A,2)
Base.OneTo(6)
```
"""
function indices(A::AbstractArray{T,N}, d) where {T,N}
    @_inline_meta
    d <= N ? indices(A)[d] : OneTo(1)
end

"""
    indices(A)

Returns the tuple of valid indices for array `A`.

# Examples
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

# Performance optimization: get rid of a branch on `d` in `indices(A, d)`
# for d=1. 1d arrays are heavily used, and the first dimension comes up
# in other applications.
indices1(A::AbstractArray{<:Any,0}) = OneTo(1)
indices1(A::AbstractArray) = (@_inline_meta; indices(A)[1])
indices1(iter) = OneTo(length(iter))

unsafe_indices(A) = indices(A)
unsafe_indices(r::AbstractRange) = (OneTo(unsafe_length(r)),) # Ranges use checked_sub for size

"""
    linearindices(A)

Returns a `UnitRange` specifying the valid range of indices for `A[i]`
where `i` is an `Int`. For arrays with conventional indexing (indices
start at 1), or any multidimensional array, this is `1:length(A)`;
however, for one-dimensional arrays with unconventional indices, this
is `indices(A, 1)`.

Calling this function is the "safe" way to write algorithms that
exploit linear indexing.

# Examples
```jldoctest
julia> A = ones(5,6,7);

julia> b = linearindices(A);

julia> extrema(b)
(1, 210)
```
"""
linearindices(A::AbstractArray) = (@_inline_meta; OneTo(_length(A)))
linearindices(A::AbstractVector) = (@_inline_meta; indices1(A))

keys(a::AbstractArray) = CartesianRange(indices(a))
keys(a::AbstractVector) = linearindices(a)

prevind(::AbstractArray, i::Integer) = Int(i)-1
nextind(::AbstractArray, i::Integer) = Int(i)+1

eltype(::Type{<:AbstractArray{E}}) where {E} = E
elsize(::AbstractArray{T}) where {T} = sizeof(T)

"""
    ndims(A::AbstractArray) -> Integer

Returns the number of dimensions of `A`.

# Examples
```jldoctest
julia> A = ones(3,4,5);

julia> ndims(A)
3
```
"""
ndims(::AbstractArray{T,N}) where {T,N} = N
ndims(::Type{AbstractArray{T,N}}) where {T,N} = N
ndims(::Type{T}) where {T<:AbstractArray} = ndims(supertype(T))

"""
    length(collection) -> Integer

Return the number of elements in the collection.

Use [`endof`](@ref) to get the last valid index of an indexable collection.

# Examples
```jldoctest
julia> length(1:5)
5

julia> length([1, 2, 3, 4])
4

julia> length([1 2; 3 4])
4
```
"""
length(t::AbstractArray) = (@_inline_meta; prod(size(t)))
_length(A::AbstractArray) = (@_inline_meta; prod(map(unsafe_length, indices(A)))) # circumvent missing size
_length(A) = (@_inline_meta; length(A))

"""
    endof(collection) -> Integer

Returns the last index of the collection.

# Examples
```jldoctest
julia> endof([1,2,4])
3
```
"""
endof(a::AbstractArray) = (@_inline_meta; last(linearindices(a)))

first(a::AbstractArray) = a[first(eachindex(a))]

"""
    first(coll)

Get the first element of an iterable collection. Returns the start point of an
`AbstractRange` even if it is empty.

# Examples
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
point of an `AbstractRange` even if it is empty.

# Examples
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

# Examples
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
    for n = 1:(i-1)
        s *= size(a, n)
    end
    return s
end

"""
    strides(A)

Returns a tuple of the memory strides in each dimension.

# Examples
```jldoctest
julia> A = ones(3,4,5);

julia> strides(A)
(1, 3, 12)
```
"""
strides(A::AbstractArray) = size_to_strides(1, size(A)...)
@inline size_to_strides(s, d, sz...) = (s, size_to_strides(s * d, sz...)...)
size_to_strides(s, d) = (s,)
size_to_strides(s) = ()


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

abstract type IndexStyle end
struct IndexLinear <: IndexStyle end
struct IndexCartesian <: IndexStyle end

"""
    IndexStyle(A)
    IndexStyle(typeof(A))

`IndexStyle` specifies the "native indexing style" for array `A`. When
you define a new `AbstractArray` type, you can choose to implement
either linear indexing or cartesian indexing.  If you decide to
implement linear indexing, then you must set this trait for your array
type:

    Base.IndexStyle(::Type{<:MyArray}) = IndexLinear()

The default is `IndexCartesian()`.

Julia's internal indexing machinery will automatically (and invisibly)
convert all indexing operations into the preferred style using
[`sub2ind`](@ref) or [`ind2sub`](@ref). This allows users to access
elements of your array using any indexing style, even when explicit
methods have not been provided.

If you define both styles of indexing for your `AbstractArray`, this
trait can be used to select the most performant indexing style. Some
methods check this trait on their inputs, and dispatch to different
algorithms depending on the most efficient access pattern. In
particular, [`eachindex`](@ref) creates an iterator whose type depends
on the setting of this trait.
"""
IndexStyle(A::AbstractArray) = IndexStyle(typeof(A))
IndexStyle(::Type{Union{}}) = IndexLinear()
IndexStyle(::Type{<:AbstractArray}) = IndexCartesian()
IndexStyle(::Type{<:Array}) = IndexLinear()
IndexStyle(::Type{<:AbstractRange}) = IndexLinear()

IndexStyle(A::AbstractArray, B::AbstractArray) = IndexStyle(IndexStyle(A), IndexStyle(B))
IndexStyle(A::AbstractArray, B::AbstractArray...) = IndexStyle(IndexStyle(A), IndexStyle(B...))
IndexStyle(::IndexLinear, ::IndexLinear) = IndexLinear()
IndexStyle(::IndexStyle, ::IndexStyle) = IndexCartesian()

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

# Examples
```jldoctest
julia> A = rand(3, 3);

julia> checkbounds(Bool, A, 2)
true

julia> checkbounds(Bool, A, 3, 4)
false

julia> checkbounds(Bool, A, 1:3)
true

julia> checkbounds(Bool, A, 1:3, 2:4)
false
```
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
function checkbounds(::Type{Bool}, A::AbstractArray{<:Any,N}, I::AbstractArray{Bool,N}) where N
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

See also [`checkbounds`](@ref).
"""
function checkbounds_indices(::Type{Bool}, IA::Tuple, I::Tuple)
    @_inline_meta
    checkindex(Bool, IA[1], I[1]) & checkbounds_indices(Bool, tail(IA), tail(I))
end
function checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple)
    @_inline_meta
    checkindex(Bool, OneTo(1), I[1]) & checkbounds_indices(Bool, (), tail(I))
end
checkbounds_indices(::Type{Bool}, IA::Tuple, ::Tuple{}) = (@_inline_meta; all(x->unsafe_length(x)==1, IA))
checkbounds_indices(::Type{Bool}, ::Tuple{}, ::Tuple{}) = true

throw_boundserror(A, I) = (@_noinline_meta; throw(BoundsError(A, I)))

# check along a single dimension
"""
    checkindex(Bool, inds::AbstractUnitRange, index)

Return `true` if the given `index` is within the bounds of
`inds`. Custom types that would like to behave as indices for all
arrays can extend this method in order to provide a specialized bounds
checking implementation.

# Examples
```jldoctest
julia> checkindex(Bool, 1:20, 8)
true

julia> checkindex(Bool, 1:20, 21)
false
```
"""
checkindex(::Type{Bool}, inds::AbstractUnitRange, i) =
    throw(ArgumentError("unable to check bounds for indices of type $(typeof(i))"))
checkindex(::Type{Bool}, inds::AbstractUnitRange, i::Real) = (first(inds) <= i) & (i <= last(inds))
checkindex(::Type{Bool}, inds::AbstractUnitRange, ::Colon) = true
checkindex(::Type{Bool}, inds::AbstractUnitRange, ::Slice) = true
function checkindex(::Type{Bool}, inds::AbstractUnitRange, r::AbstractRange)
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

```julia-repl
julia> similar(1:10, 1, 4)
1×4 Array{Int64,2}:
 4419743872  4374413872  4419743888  0
```

Conversely, `similar(trues(10,10), 2)` returns an uninitialized `BitVector` with two
elements since `BitArray`s are both mutable and can support 1-dimensional arrays:

```julia-repl
julia> similar(trues(10,10), 2)
2-element BitArray{1}:
 false
 false
```

Since `BitArray`s can only store elements of type [`Bool`](@ref), however, if you request a
different element type it will create a regular `Array` instead:

```julia-repl
julia> similar(falses(10), Float64, 2, 4)
2×4 Array{Float64,2}:
 2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
 2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
```

"""
similar(a::AbstractArray{T}) where {T}                             = similar(a, T)
similar(a::AbstractArray, ::Type{T}) where {T}                     = similar(a, T, to_shape(indices(a)))
similar(a::AbstractArray{T}, dims::Tuple) where {T}                = similar(a, T, to_shape(dims))
similar(a::AbstractArray{T}, dims::DimOrInd...) where {T}          = similar(a, T, to_shape(dims))
similar(a::AbstractArray, ::Type{T}, dims::DimOrInd...) where {T}  = similar(a, T, to_shape(dims))
similar(a::AbstractArray, ::Type{T}, dims::NeedsShaping) where {T} = similar(a, T, to_shape(dims))
# similar creates an Array by default
similar(a::AbstractArray, ::Type{T}, dims::Dims{N}) where {T,N}    = Array{T,N}(dims)

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
    copy!(IndexStyle(dest), dest, IndexStyle(src), src)

function copy!(::IndexStyle, dest::AbstractArray, ::IndexStyle, src::AbstractArray)
    destinds, srcinds = linearindices(dest), linearindices(src)
    isempty(srcinds) || (first(srcinds) ∈ destinds && last(srcinds) ∈ destinds) ||
        throw(BoundsError(dest, srcinds))
    @inbounds for i in srcinds
        dest[i] = src[i]
    end
    return dest
end

function copy!(::IndexStyle, dest::AbstractArray, ::IndexCartesian, src::AbstractArray)
    destinds, srcinds = linearindices(dest), linearindices(src)
    isempty(srcinds) || (first(srcinds) ∈ destinds && last(srcinds) ∈ destinds) ||
        throw(BoundsError(dest, srcinds))
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

function copy!(B::AbstractVecOrMat{R}, ir_dest::AbstractRange{Int}, jr_dest::AbstractRange{Int},
               A::AbstractVecOrMat{S}, ir_src::AbstractRange{Int}, jr_src::AbstractRange{Int}) where {R,S}
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

# Examples
```jldoctest
julia> tup = (1, 2, 3)
(1, 2, 3)

julia> Base.copymutable(tup)
3-element Array{Int64,1}:
 1
 2
 3
```
"""
function copymutable(a::AbstractArray)
    @_propagate_inbounds_meta
    copy!(similar(a), a)
end
copymutable(itr) = collect(itr)

zero(x::AbstractArray{T}) where {T} = fill!(similar(x), zero(T))

## iteration support for arrays by iterating over `eachindex` in the array ##
# Allows fast iteration by default for both IndexLinear and IndexCartesian arrays

# While the definitions for IndexLinear are all simple enough to inline on their
# own, IndexCartesian's CartesianRange is more complicated and requires explicit
# inlining.
start(A::AbstractArray) = (@_inline_meta; itr = eachindex(A); (itr, start(itr)))
next(A::AbstractArray, i) = (@_propagate_inbounds_meta; (idx, s) = next(i[1], i[2]); (A[idx], (i[1], s)))
done(A::AbstractArray, i) = (@_propagate_inbounds_meta; done(i[1], i[2]))

# `eachindex` is mostly an optimization of `keys`
eachindex(itrs...) = keys(itrs...)

# eachindex iterates over all indices. IndexCartesian definitions are later.
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
2×3 SparseMatrixCSC{Int64,Int64} with 3 stored entries:
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
if all inputs have fast linear indexing, a [`CartesianRange`](@ref)
otherwise).
If the arrays have different sizes and/or dimensionalities, `eachindex` returns an
iterable that spans the largest range along each dimension.
"""
eachindex(A::AbstractArray) = (@_inline_meta(); eachindex(IndexStyle(A), A))

function eachindex(A::AbstractArray, B::AbstractArray)
    @_inline_meta
    eachindex(IndexStyle(A,B), A, B)
end
function eachindex(A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    eachindex(IndexStyle(A,B...), A, B...)
end
eachindex(::IndexLinear, A::AbstractArray) = linearindices(A)
function eachindex(::IndexLinear, A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    1:_maxlength(A, B...)
end
_maxlength(A) = length(A)
function _maxlength(A, B, C...)
    @_inline_meta
    max(length(A), _maxlength(B, C...))
end

isempty(a::AbstractArray) = (_length(a) == 0)

# keys with an IndexStyle
keys(s::IndexStyle, A::AbstractArray, B::AbstractArray...) = eachindex(s, A, B...)

## Conversions ##

convert(::Type{AbstractArray{T,N}}, A::AbstractArray{T,N}) where {T,N  } = A
convert(::Type{AbstractArray{T,N}}, A::AbstractArray{S,N}) where {T,S,N} = copy!(similar(A,T), A)
convert(::Type{AbstractArray{T}},   A::AbstractArray{S,N}) where {T,S,N} = convert(AbstractArray{T,N}, A)

convert(::Type{Array}, A::AbstractArray{T,N}) where {T,N} = convert(Array{T,N}, A)

"""
   of_indices(x, y)

Represents the array `y` as an array having the same indices type as `x`.
"""
of_indices(x, y) = similar(dims->y, oftype(indices(x), indices(y)))


"""
    full(F)

Reconstruct the matrix `A` from the factorization `F=factorize(A)`.
"""
full(x::AbstractArray) = x

## range conversions ##

map(::Type{T}, r::StepRange) where {T<:Real} = T(r.start):T(r.step):T(last(r))
map(::Type{T}, r::UnitRange) where {T<:Real} = T(r.start):T(last(r))
map(::Type{T}, r::StepRangeLen) where {T<:AbstractFloat} = convert(StepRangeLen{T}, r)
function map(::Type{T}, r::LinSpace) where T<:AbstractFloat
    LinSpace(T(r.start), T(r.stop), length(r))
end

## unsafe/pointer conversions ##

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer(x::AbstractArray{T}) where {T} = unsafe_convert(Ptr{T}, x)
function pointer(x::AbstractArray{T}, i::Integer) where T
    @_inline_meta
    unsafe_convert(Ptr{T}, x) + (i - first(linearindices(x)))*elsize(x)
end

## Approach:
# We only define one fallback method on getindex for all argument types.
# That dispatches to an (inlined) internal _getindex function, where the goal is
# to transform the indices such that we can call the only getindex method that
# we require the type A{T,N} <: AbstractArray{T,N} to define; either:
#       getindex(::A, ::Int) # if IndexStyle(A) == IndexLinear() OR
#       getindex(::A{T,N}, ::Vararg{Int, N}) where {T,N} # if IndexCartesian()
# If the subtype hasn't defined the required method, it falls back to the
# _getindex function again where an error is thrown to prevent stack overflows.
"""
    getindex(A, inds...)

Return a subset of array `A` as specified by `inds`, where each `ind` may be an
`Int`, an `AbstractRange`, or a [`Vector`](@ref). See the manual section on
[array indexing](@ref man-array-indexing) for details.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> getindex(A, 1)
1

julia> getindex(A, [2, 1])
2-element Array{Int64,1}:
 3
 1

julia> getindex(A, 2:4)
3-element Array{Int64,1}:
 3
 2
 4
```
"""
function getindex(A::AbstractArray, I...)
    @_propagate_inbounds_meta
    error_if_canonical_indexing(IndexStyle(A), A, I...)
    _getindex(IndexStyle(A), A, to_indices(A, I)...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    @inbounds r = getindex(A, I...)
    r
end

error_if_canonical_indexing(::IndexLinear, A::AbstractArray, ::Int) =
    error("indexing not defined for ", typeof(A))
error_if_canonical_indexing(::IndexCartesian, A::AbstractArray{T,N}, ::Vararg{Int,N}) where {T,N} =
    error("indexing not defined for ", typeof(A))
error_if_canonical_indexing(::IndexStyle, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_getindex(::IndexStyle, A::AbstractArray, I...) =
    error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing: canonical method is one Int
_getindex(::IndexLinear, A::AbstractArray, i::Int) = (@_propagate_inbounds_meta; getindex(A, i))
_getindex(::IndexLinear, A::AbstractArray) = (@_propagate_inbounds_meta; getindex(A, _to_linear_index(A)))
function _getindex(::IndexLinear, A::AbstractArray, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...) # generally _to_linear_index requires bounds checking
    @inbounds r = getindex(A, _to_linear_index(A, I...))
    r
end
_to_linear_index(A::AbstractArray, i::Int) = i
_to_linear_index(A::AbstractVector, i::Int, I::Int...) = i # TODO: DEPRECATE FOR #14770
_to_linear_index(A::AbstractArray{T,N}, I::Vararg{Int,N}) where {T,N} = (@_inline_meta; sub2ind(A, I...))
_to_linear_index(A::AbstractArray) = 1 # TODO: DEPRECATE FOR #14770
_to_linear_index(A::AbstractArray, I::Int...) = (@_inline_meta; sub2ind(A, I...)) # TODO: DEPRECATE FOR #14770

## IndexCartesian Scalar indexing: Canonical method is full dimensionality of Ints
function _getindex(::IndexCartesian, A::AbstractArray)
    @_propagate_inbounds_meta
    getindex(A, _to_subscript_indices(A)...)
end
function _getindex(::IndexCartesian, A::AbstractArray, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...) # generally _to_subscript_indices requires bounds checking
    @inbounds r = getindex(A, _to_subscript_indices(A, I...)...)
    r
end
function _getindex(::IndexCartesian, A::AbstractArray{T,N}, I::Vararg{Int, N}) where {T,N}
    @_propagate_inbounds_meta
    getindex(A, I...)
end
_to_subscript_indices(A::AbstractArray, i::Int) = (@_inline_meta; _unsafe_ind2sub(A, i))
_to_subscript_indices(A::AbstractArray{T,N}) where {T,N} = (@_inline_meta; fill_to_length((), 1, Val(N))) # TODO: DEPRECATE FOR #14770
_to_subscript_indices(A::AbstractArray{T,0}) where {T} = () # TODO: REMOVE FOR #14770
_to_subscript_indices(A::AbstractArray{T,0}, i::Int) where {T} = () # TODO: REMOVE FOR #14770
_to_subscript_indices(A::AbstractArray{T,0}, I::Int...) where {T} = () # TODO: DEPRECATE FOR #14770
function _to_subscript_indices(A::AbstractArray{T,N}, I::Int...) where {T,N} # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J, Jrem = IteratorsMD.split(I, Val(N))
    _to_subscript_indices(A, J, Jrem)
end
_to_subscript_indices(A::AbstractArray, J::Tuple, Jrem::Tuple{}) =
    __to_subscript_indices(A, indices(A), J, Jrem)
function __to_subscript_indices(A::AbstractArray,
        ::Tuple{AbstractUnitRange,Vararg{AbstractUnitRange}}, J::Tuple, Jrem::Tuple{})
    @_inline_meta
    (J..., map(first, tail(_remaining_size(J, indices(A))))...)
end
_to_subscript_indices(A, J::Tuple, Jrem::Tuple) = J # already bounds-checked, safe to drop
_to_subscript_indices(A::AbstractArray{T,N}, I::Vararg{Int,N}) where {T,N} = I
_remaining_size(::Tuple{Any}, t::Tuple) = t
_remaining_size(h::Tuple, t::Tuple) = (@_inline_meta; _remaining_size(tail(h), tail(t)))
_unsafe_ind2sub(::Tuple{}, i) = () # ind2sub may throw(BoundsError()) in this case
_unsafe_ind2sub(sz, i) = (@_inline_meta; ind2sub(sz, i))

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage

"""
    setindex!(A, X, inds...)

Store values from array `X` within some subset of `A` as specified by `inds`.
"""
function setindex!(A::AbstractArray, v, I...)
    @_propagate_inbounds_meta
    error_if_canonical_indexing(IndexStyle(A), A, I...)
    _setindex!(IndexStyle(A), A, v, to_indices(A, I)...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    @inbounds r = setindex!(A, v, I...)
    r
end
## Internal defitions
_setindex!(::IndexStyle, A::AbstractArray, v, I...) =
    error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing
_setindex!(::IndexLinear, A::AbstractArray, v, i::Int) = (@_propagate_inbounds_meta; setindex!(A, v, i))
_setindex!(::IndexLinear, A::AbstractArray, v) = (@_propagate_inbounds_meta; setindex!(A, v, _to_linear_index(A)))
function _setindex!(::IndexLinear, A::AbstractArray, v, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_linear_index(A, I...))
    r
end

# IndexCartesian Scalar indexing
function _setindex!(::IndexCartesian, A::AbstractArray{T,N}, v, I::Vararg{Int, N}) where {T,N}
    @_propagate_inbounds_meta
    setindex!(A, v, I...)
end
function _setindex!(::IndexCartesian, A::AbstractArray, v)
    @_propagate_inbounds_meta
    setindex!(A, v, _to_subscript_indices(A)...)
end
function _setindex!(::IndexCartesian, A::AbstractArray, v, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_subscript_indices(A, I...)...)
    r
end

## get (getindex with a default value) ##

RangeVecIntList{A<:AbstractVector{Int}} = Union{Tuple{Vararg{Union{AbstractRange, AbstractVector{Int}}}},
    AbstractVector{UnitRange{Int}}, AbstractVector{AbstractRange{Int}}, AbstractVector{A}}

get(A::AbstractArray, i::Integer, default) = checkbounds(Bool, A, i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = checkbounds(Bool, A, I...) ? A[I...] : default

function get!(X::AbstractVector{T}, A::AbstractVector, I::Union{AbstractRange,AbstractVector{Int}}, default::T) where T
    # 1d is not linear indexing
    ind = findin(I, indices1(A))
    X[ind] = A[I[ind]]
    Xind = indices1(X)
    X[first(Xind):first(ind)-1] = default
    X[last(ind)+1:last(Xind)] = default
    X
end
function get!(X::AbstractArray{T}, A::AbstractArray, I::Union{AbstractRange,AbstractVector{Int}}, default::T) where T
    # Linear indexing
    ind = findin(I, 1:length(A))
    X[ind] = A[I[ind]]
    X[1:first(ind)-1] = default
    X[last(ind)+1:length(X)] = default
    X
end

get(A::AbstractArray, I::AbstractRange, default) = get!(similar(A, typeof(default), index_shape(I)), A, I, default)

# TODO: DEPRECATE FOR #14770 (just the partial linear indexing part)
function get!(X::AbstractArray{T}, A::AbstractArray, I::RangeVecIntList, default::T) where T
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end

get(A::AbstractArray, I::RangeVecIntList, default) =
    get!(similar(A, typeof(default), index_shape(I...)), A, I, default)

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

typed_vcat(::Type{T}) where {T} = Array{T,1}(0)
typed_hcat(::Type{T}) where {T} = Array{T,1}(0)

## cat: special cases
vcat(X::T...) where {T}         = T[ X[i] for i=1:length(X) ]
vcat(X::T...) where {T<:Number} = T[ X[i] for i=1:length(X) ]
hcat(X::T...) where {T}         = T[ X[j] for i=1:1, j=1:length(X) ]
hcat(X::T...) where {T<:Number} = T[ X[j] for i=1:1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill(Vector{promote_typeof(X...)}(length(X)), X)
hcat(X::Number...) = hvcat_fill(Matrix{promote_typeof(X...)}(1,length(X)), X)
typed_vcat(::Type{T}, X::Number...) where {T} = hvcat_fill(Array{T,1}(length(X)), X)
typed_hcat(::Type{T}, X::Number...) where {T} = hvcat_fill(Array{T,2}(1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat(V::AbstractVector{T}...) where {T} = typed_vcat(T, V...)

function typed_vcat(::Type{T}, V::AbstractVector...) where T
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
hcat(A::AbstractVecOrMat{T}...) where {T} = typed_hcat(T, A...)

function typed_hcat(::Type{T}, A::AbstractVecOrMat...) where T
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
vcat(A::AbstractMatrix{T}...) where {T} = typed_vcat(T, A...)

function typed_vcat(::Type{T}, A::AbstractMatrix...) where T
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
    cat_shape(dims, _cshp(1, dims, shape, nshape), shapes...)

_cshp(ndim::Int, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
_cshp(ndim::Int, ::Tuple{}, ::Tuple{}, nshape) = nshape
_cshp(ndim::Int, dims, ::Tuple{}, ::Tuple{}) = ntuple(b -> 1, Val(length(dims)))
@inline _cshp(ndim::Int, dims, shape, ::Tuple{}) =
    (shape[1] + dims[1], _cshp(ndim + 1, tail(dims), tail(shape), ())...)
@inline _cshp(ndim::Int, dims, ::Tuple{}, nshape) =
    (nshape[1], _cshp(ndim + 1, tail(dims), (), tail(nshape))...)
@inline function _cshp(ndim::Int, ::Tuple{}, shape, ::Tuple{})
    _cs(ndim, shape[1], 1)
    (1, _cshp(ndim + 1, (), tail(shape), ())...)
end
@inline function _cshp(ndim::Int, ::Tuple{}, shape, nshape)
    next = _cs(ndim, shape[1], nshape[1])
    (next, _cshp(ndim + 1, (), tail(shape), tail(nshape))...)
end
@inline function _cshp(ndim::Int, dims, shape, nshape)
    a = shape[1]
    b = nshape[1]
    next = dims[1] ? a + b : _cs(ndim, a, b)
    (next, _cshp(ndim + 1, tail(dims), tail(shape), tail(nshape))...)
end

_cs(d, a, b) = (a == b ? a : throw(DimensionMismatch(
    "mismatch in dimension $d (expected $a got $b)")))

dims2cat(::Val{n}) where {n} = ntuple(i -> (i == n), Val(n))
dims2cat(dims) = ntuple(i -> (i in dims), maximum(dims))

cat(dims, X...) = cat_t(dims, promote_eltypeof(X...), X...)

function cat_t(dims, T::Type, X...)
    catdims = dims2cat(dims)
    shape = cat_shape(catdims, (), map(cat_size, X)...)
    A = cat_similar(X[1], T, shape)
    if T <: Number && count(!iszero, catdims) > 1
        fill!(A, zero(T))
    end
    return _cat(A, shape, catdims, X...)
end

function _cat(A, shape::NTuple{N}, catdims, X...) where N
    offsets = zeros(Int, N)
    inds = Vector{UnitRange{Int}}(N)
    concat = copy!(zeros(Bool, N), catdims)
    for x in X
        for i = 1:N
            if concat[i]
                inds[i] = offsets[i] .+ cat_indices(x, i)
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

# Examples
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
vcat(X...) = cat(Val(1), X...)
"""
    hcat(A...)

Concatenate along dimension 2.

# Examples
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
hcat(X...) = cat(Val(2), X...)

typed_vcat(T::Type, X...) = cat_t(Val(1), T, X...)
typed_hcat(T::Type, X...) = cat_t(Val(2), T, X...)

"""
    cat(dims, A...)

Concatenate the input arrays along the specified dimensions in the iterable `dims`. For
dimensions not in `dims`, all input arrays should have the same size, which will also be the
size of the output array along that dimension. For dimensions in `dims`, the size of the
output array is the sum of the sizes of the input arrays along that dimension. If `dims` is
a single number, the different arrays are tightly stacked along that dimension. If `dims` is
an iterable containing several dimensions, this allows one to construct block diagonal
matrices and their higher-dimensional analogues by simultaneously increasing several
dimensions for every new input array and putting zero blocks elsewhere. For example,
`cat([1,2], matrices...)` builds a block diagonal matrix, i.e. a block matrix with
`matrices[1]`, `matrices[2]`, ... as diagonal blocks and matching zero blocks away from the
diagonal.
"""
cat(catdims, A::AbstractArray{T}...) where {T} = cat_t(catdims, T, A...)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(Val(1), A)
vcat(A::AbstractArray, B::AbstractArray) = cat(Val(1), A, B)
vcat(A::AbstractArray...) = cat(Val(1), A...)
hcat(A::AbstractArray) = cat(Val(2), A)
hcat(A::AbstractArray, B::AbstractArray) = cat(Val(2), A, B)
hcat(A::AbstractArray...) = cat(Val(2), A...)

typed_vcat(T::Type, A::AbstractArray) = cat_t(Val(1), T, A)
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(Val(1), T, A, B)
typed_vcat(T::Type, A::AbstractArray...) = cat_t(Val(1), T, A...)
typed_hcat(T::Type, A::AbstractArray) = cat_t(Val(2), T, A)
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(Val(2), T, A, B)
typed_hcat(T::Type, A::AbstractArray...) = cat_t(Val(2), T, A...)

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

# Examples
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
hvcat(rows::Tuple{Vararg{Int}}, xs::AbstractVecOrMat{T}...) where {T} = typed_hvcat(T, rows, xs...)

function typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}, as::AbstractVecOrMat...) where T
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
typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}) where {T} = Array{T,1}(0)

function hvcat(rows::Tuple{Vararg{Int}}, xs::T...) where T<:Number
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

function typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}, xs::Number...) where T
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

function typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}, as...) where T
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
    if isa(A,AbstractRange) != isa(B,AbstractRange)
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
    if isa(A,AbstractRange) != isa(B,AbstractRange)
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

# Examples
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

# Examples
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
sub2ind(inds::Indices{1}, I::Integer...) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
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

# Examples
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
ind2sub(inds::Indices{1}, ind::Integer) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
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
function sub2ind(inds::Indices{1}, I1::AbstractVector{T}, I::AbstractVector{T}...) where T<:Integer
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
end
sub2ind(inds::Tuple{OneTo}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} =
    _sub2ind_vecs(inds, I1, I...)
sub2ind(inds::Union{DimsInteger,Indices}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} =
    _sub2ind_vecs(inds, I1, I...)
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
        # Iout[i] = sub2ind(inds, map(Ij -> Ij[i], I)...)
        Iout[i] = sub2ind_vec(inds, i, I)
    end
    Iout
end

sub2ind_vec(inds, i, I) = (@_inline_meta; sub2ind(inds, _sub2ind_vec(i, I...)...))
_sub2ind_vec(i, I1, I...) = (@_inline_meta; (I1[i], _sub2ind_vec(i, I...)...))
_sub2ind_vec(i) = ()

function ind2sub(inds::Union{DimsInteger{N},Indices{N}}, ind::AbstractVector{<:Integer}) where N
    M = length(ind)
    t = ntuple(n->similar(ind),Val(N))
    for (i,idx) in pairs(IndexLinear(), ind)
        sub = ind2sub(inds, idx)
        for j = 1:N
            t[j][i] = sub[j]
        end
    end
    t
end

function ind2sub!(sub::Array{T}, dims::Tuple{Vararg{T}}, ind::T) where T<:Integer
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

# Examples
```jldoctest
julia> a = 1:3:7;

julia> foreach(x -> println(x^2), a)
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

# Examples
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

    # Apply the function to the first slice in order to determine the next steps
    Aslice = A[idx...]
    r1 = f(Aslice)
    # In some cases, we can re-use the first slice for a dramatic performance
    # increase. The slice itself must be mutable and the result cannot contain
    # any mutable containers. The following errs on the side of being overly
    # strict (#18570 & #21123).
    safe_for_reuse = isa(Aslice, StridedArray) &&
                     (isa(r1, Number) || (isa(r1, AbstractArray) && eltype(r1) <: Number))

    # determine result size and allocate
    Rsize = copy(dimsA)
    # TODO: maybe support removing dimensions
    if !isa(r1, AbstractArray) || ndims(r1) == 0
        r1 = [r1]
    end
    nextra = max(0, length(dims)-ndims(r1))
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

    nidx = length(otherdims)
    indexes = Iterators.drop(CartesianRange(itershape), 1)
    inner_mapslices!(safe_for_reuse, indexes, nidx, idx, otherdims, ridx, Aslice, A, f, R)
end

@noinline function inner_mapslices!(safe_for_reuse, indexes, nidx, idx, otherdims, ridx, Aslice, A, f, R)
    if safe_for_reuse
        # when f returns an array, R[ridx...] = f(Aslice) line copies elements,
        # so we can reuse Aslice
        for I in indexes # skip the first element, we already handled it
            replace_tuples!(nidx, idx, ridx, otherdims, I)
            _unsafe_getindex!(Aslice, A, idx...)
            R[ridx...] = f(Aslice)
        end
    else
        # we can't guarantee safety (#18524), so allocate new storage for each slice
        for I in indexes
            replace_tuples!(nidx, idx, ridx, otherdims, I)
            R[ridx...] = f(A[idx...])
        end
    end

    return R
end

function replace_tuples!(nidx, idx, ridx, otherdims, I)
    for i in 1:nidx
        idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
    end
end


## 1 argument

function map!(f::F, dest::AbstractArray, A::AbstractArray) where F
    for (i,j) in zip(eachindex(dest),eachindex(A))
        dest[i] = f(A[j])
    end
    return dest
end

# map on collections
map(f, A::Union{AbstractArray,AbstractSet}) = collect_similar(A, Generator(f,A))

# default to returning an Array for `map` on general iterators
"""
    map(f, c...) -> collection

Transform collection `c` by applying `f` to each element. For multiple collection arguments,
apply `f` elementwise.

See also: [`mapslices`](@ref)

# Examples
```jldoctest
julia> map(x -> x * 2, [1, 2, 3])
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
function map!(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray) where F
    for (i, j, k) in zip(eachindex(dest), eachindex(A), eachindex(B))
        dest[i] = f(A[j], B[k])
    end
    return dest
end

## N argument

@inline ith_all(i, ::Tuple{}) = ()
@inline ith_all(i, as) = (as[1][i], ith_all(i, tail(as))...)

function map_n!(f::F, dest::AbstractArray, As) where F
    for i = linearindices(As[1])
        dest[i] = f(ith_all(i, As)...)
    end
    return dest
end

"""
    map!(function, destination, collection...)

Like [`map`](@ref), but stores the result in `destination` rather than a new
collection. `destination` must be at least as large as the first collection.

# Examples
```jldoctest
julia> x = zeros(3);

julia> map!(x -> x * 2, x, [1, 2, 3]);

julia> x
3-element Array{Float64,1}:
 2.0
 4.0
 6.0
```
"""
map!(f::F, dest::AbstractArray, As::AbstractArray...) where {F} = map_n!(f, dest, As)

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
const hashrle_seed = UInt === UInt64 ? 0x2aab8909bfea414c : 0xbfea414c
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
