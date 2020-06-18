# This file is a part of Julia. License is MIT: https://julialang.org/license

## Basic functions ##

"""
    AbstractArray{T,N}

Supertype for `N`-dimensional arrays (or array-like types) with elements of type `T`.
[`Array`](@ref) and other types are subtypes of this. See the manual section on the
[`AbstractArray` interface](@ref man-interface-array).
"""
AbstractArray

convert(::Type{T}, a::T) where {T<:AbstractArray} = a
convert(::Type{AbstractArray{T}}, a::AbstractArray) where {T} = AbstractArray{T}(a)
convert(::Type{AbstractArray{T,N}}, a::AbstractArray{<:Any,N}) where {T,N} = AbstractArray{T,N}(a)

"""
    size(A::AbstractArray, [dim])

Return a tuple containing the dimensions of `A`. Optionally you can specify a
dimension to just get the length of that dimension.

Note that `size` may not be defined for arrays with non-standard indices, in which case [`axes`](@ref)
may be useful. See the manual chapter on [arrays with custom indices](@ref man-custom-indices).

# Examples
```jldoctest
julia> A = fill(1, (2,3,4));

julia> size(A)
(2, 3, 4)

julia> size(A, 2)
3
```
"""
size(t::AbstractArray{T,N}, d) where {T,N} = d::Integer <= N ? size(t)[d] : 1

"""
    axes(A, d)

Return the valid range of indices for array `A` along dimension `d`.

See also [`size`](@ref), and the manual chapter on [arrays with custom indices](@ref man-custom-indices).

# Examples
```jldoctest
julia> A = fill(1, (5,6,7));

julia> axes(A, 2)
Base.OneTo(6)
```
"""
function axes(A::AbstractArray{T,N}, d) where {T,N}
    @_inline_meta
    d::Integer <= N ? axes(A)[d] : OneTo(1)
end

"""
    axes(A)

Return the tuple of valid indices for array `A`.

# Examples
```jldoctest
julia> A = fill(1, (5,6,7));

julia> axes(A)
(Base.OneTo(5), Base.OneTo(6), Base.OneTo(7))
```
"""
function axes(A)
    @_inline_meta
    map(OneTo, size(A))
end

"""
    has_offset_axes(A)
    has_offset_axes(A, B, ...)

Return `true` if the indices of `A` start with something other than 1 along any axis.
If multiple arguments are passed, equivalent to `has_offset_axes(A) | has_offset_axes(B) | ...`.
"""
has_offset_axes(A)    = _tuple_any(x->first(x)!=1, axes(A))
has_offset_axes(A...) = _tuple_any(has_offset_axes, A)
has_offset_axes(::Colon) = false

require_one_based_indexing(A...) = !has_offset_axes(A...) || throw(ArgumentError("offset arrays are not supported but got an array with index other than 1"))

# Performance optimization: get rid of a branch on `d` in `axes(A, d)`
# for d=1. 1d arrays are heavily used, and the first dimension comes up
# in other applications.
axes1(A::AbstractArray{<:Any,0}) = OneTo(1)
axes1(A::AbstractArray) = (@_inline_meta; axes(A)[1])
axes1(iter) = OneTo(length(iter))

unsafe_indices(A) = axes(A)
unsafe_indices(r::AbstractRange) = (OneTo(unsafe_length(r)),) # Ranges use checked_sub for size

keys(a::AbstractArray) = CartesianIndices(axes(a))
keys(a::AbstractVector) = LinearIndices(a)

"""
    keytype(T::Type{<:AbstractArray})
    keytype(A::AbstractArray)

Return the key type of an array. This is equal to the
`eltype` of the result of `keys(...)`, and is provided
mainly for compatibility with the dictionary interface.

# Examples
```jldoctest
julia> keytype([1, 2, 3]) == Int
true

julia> keytype([1 2; 3 4])
CartesianIndex{2}
```

!!! compat "Julia 1.2"
     For arrays, this function requires at least Julia 1.2.
"""
keytype(a::AbstractArray) = keytype(typeof(a))

keytype(A::Type{<:AbstractArray}) = CartesianIndex{ndims(A)}
keytype(A::Type{<:AbstractVector}) = Int

valtype(a::AbstractArray) = valtype(typeof(a))

"""
    valtype(T::Type{<:AbstractArray})
    valtype(A::AbstractArray)

Return the value type of an array. This is identical to `eltype` and is
provided mainly for compatibility with the dictionary interface.

# Examples
```jldoctest
julia> valtype(["one", "two", "three"])
String
```

!!! compat "Julia 1.2"
     For arrays, this function requires at least Julia 1.2.
"""
valtype(A::Type{<:AbstractArray}) = eltype(A)

prevind(::AbstractArray, i::Integer) = Int(i)-1
nextind(::AbstractArray, i::Integer) = Int(i)+1

eltype(::Type{<:AbstractArray{E}}) where {E} = @isdefined(E) ? E : Any
elsize(A::AbstractArray) = elsize(typeof(A))

"""
    ndims(A::AbstractArray) -> Integer

Return the number of dimensions of `A`.

# Examples
```jldoctest
julia> A = fill(1, (3,4,5));

julia> ndims(A)
3
```
"""
ndims(::AbstractArray{T,N}) where {T,N} = N
ndims(::Type{<:AbstractArray{T,N}}) where {T,N} = N

"""
    length(collection) -> Integer

Return the number of elements in the collection.

Use [`lastindex`](@ref) to get the last valid index of an indexable collection.

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
length

"""
    length(A::AbstractArray)

Return the number of elements in the array, defaults to `prod(size(A))`.

# Examples
```jldoctest
julia> length([1, 2, 3, 4])
4

julia> length([1 2; 3 4])
4
```
"""
length(t::AbstractArray) = (@_inline_meta; prod(size(t)))

# `eachindex` is mostly an optimization of `keys`
eachindex(itrs...) = keys(itrs...)

# eachindex iterates over all indices. IndexCartesian definitions are later.
eachindex(A::AbstractVector) = (@_inline_meta(); axes1(A))

@noinline function throw_eachindex_mismatch(::IndexLinear, A...)
    throw(DimensionMismatch("all inputs to eachindex must have the same indices, got $(join(eachindex.(A), ", ", " and "))"))
end
@noinline function throw_eachindex_mismatch(::IndexCartesian, A...)
    throw(DimensionMismatch("all inputs to eachindex must have the same axes, got $(join(axes.(A), ", ", " and "))"))
end

"""
    eachindex(A...)

Create an iterable object for visiting each index of an `AbstractArray` `A` in an efficient
manner. For array types that have opted into fast linear indexing (like `Array`), this is
simply the range `1:length(A)`. For other array types, return a specialized Cartesian
range to efficiently index into the array with indices specified for every dimension. For
other iterables, including strings and dictionaries, return an iterator object
supporting arbitrary index types (e.g. unevenly spaced or non-integer indices).

If you supply more than one `AbstractArray` argument, `eachindex` will create an
iterable object that is fast for all arguments (a [`UnitRange`](@ref)
if all inputs have fast linear indexing, a [`CartesianIndices`](@ref)
otherwise).
If the arrays have different sizes and/or dimensionalities, a DimensionMismatch exception
will be thrown.
# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> for i in eachindex(A) # linear indexing
           println(i)
       end
1
2
3
4

julia> for i in eachindex(view(A, 1:2, 1:1)) # Cartesian indexing
           println(i)
       end
CartesianIndex(1, 1)
CartesianIndex(2, 1)
```
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
eachindex(::IndexLinear, A::AbstractArray) = (@_inline_meta; OneTo(length(A)))
eachindex(::IndexLinear, A::AbstractVector) = (@_inline_meta; axes1(A))
function eachindex(::IndexLinear, A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    indsA = eachindex(IndexLinear(), A)
    _all_match_first(X->eachindex(IndexLinear(), X), indsA, B...) ||
        throw_eachindex_mismatch(IndexLinear(), A, B...)
    indsA
end
function _all_match_first(f::F, inds, A, B...) where F<:Function
    @_inline_meta
    (inds == f(A)) & _all_match_first(f, inds, B...)
end
_all_match_first(f::F, inds) where F<:Function = true

# keys with an IndexStyle
keys(s::IndexStyle, A::AbstractArray, B::AbstractArray...) = eachindex(s, A, B...)

"""
    lastindex(collection) -> Integer
    lastindex(collection, d) -> Integer

Return the last index of `collection`. If `d` is given, return the last index of `collection` along dimension `d`.

The syntaxes `A[end]` and `A[end, end]` lower to `A[lastindex(A)]` and
`A[lastindex(A, 1), lastindex(A, 2)]`, respectively.

# Examples
```jldoctest
julia> lastindex([1,2,4])
3

julia> lastindex(rand(3,4,5), 2)
4
```
"""
lastindex(a::AbstractArray) = (@_inline_meta; last(eachindex(IndexLinear(), a)))
lastindex(a::AbstractArray, d) = (@_inline_meta; last(axes(a, d)))

"""
    firstindex(collection) -> Integer
    firstindex(collection, d) -> Integer

Return the first index of `collection`. If `d` is given, return the first index of `collection` along dimension `d`.

# Examples
```jldoctest
julia> firstindex([1,2,4])
1

julia> firstindex(rand(3,4,5), 2)
1
```
"""
firstindex(a::AbstractArray) = (@_inline_meta; first(eachindex(IndexLinear(), a)))
firstindex(a::AbstractArray, d) = (@_inline_meta; first(axes(a, d)))

first(a::AbstractArray) = a[first(eachindex(a))]

"""
    first(coll)

Get the first element of an iterable collection. Return the start point of an
[`AbstractRange`](@ref) even if it is empty.

# Examples
```jldoctest
julia> first(2:2:10)
2

julia> first([1; 2; 3; 4])
1
```
"""
function first(itr)
    x = iterate(itr)
    x === nothing && throw(ArgumentError("collection must be non-empty"))
    x[1]
end

"""
    last(coll)

Get the last element of an ordered collection, if it can be computed in O(1) time. This is
accomplished by calling [`lastindex`](@ref) to get the last index. Return the end
point of an [`AbstractRange`](@ref) even if it is empty.

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
    strides(A)

Return a tuple of the memory strides in each dimension.

# Examples
```jldoctest
julia> A = fill(1, (3,4,5));

julia> strides(A)
(1, 3, 12)
```
"""
function strides end

"""
    stride(A, k::Integer)

Return the distance in memory (in number of elements) between adjacent elements in dimension `k`.

# Examples
```jldoctest
julia> A = fill(1, (3,4,5));

julia> stride(A,2)
3

julia> stride(A,3)
12
```
"""
function stride(A::AbstractArray, k::Integer)
    st = strides(A)
    k ≤ ndims(A) && return st[k]
    length(A)
end

@inline size_to_strides(s, d, sz...) = (s, size_to_strides(s * d, sz...)...)
size_to_strides(s, d) = (s,)
size_to_strides(s) = ()


function isassigned(a::AbstractArray, i::Integer...)
    try
        a[i...]
        true
    catch e
        if isa(e, BoundsError) || isa(e, UndefRefError)
            return false
        else
            rethrow()
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
    checkbounds_indices(Bool, axes(A), I)
end

# Linear indexing is explicitly allowed when there is only one (non-cartesian) index
function checkbounds(::Type{Bool}, A::AbstractArray, i)
    @_inline_meta
    checkindex(Bool, eachindex(IndexLinear(), A), i)
end
# As a special extension, allow using logical arrays that match the source array exactly
function checkbounds(::Type{Bool}, A::AbstractArray{<:Any,N}, I::AbstractArray{Bool,N}) where N
    @_inline_meta
    axes(A) == axes(I)
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
checkindex(::Type{Bool}, indx::AbstractUnitRange, I::AbstractVector{Bool}) = indx == axes1(I)
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
default is an `Array{element_type}(undef, dims...)`.

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
 0
 0
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
similar(a::AbstractArray, ::Type{T}) where {T}                     = similar(a, T, to_shape(axes(a)))
similar(a::AbstractArray{T}, dims::Tuple) where {T}                = similar(a, T, to_shape(dims))
similar(a::AbstractArray{T}, dims::DimOrInd...) where {T}          = similar(a, T, to_shape(dims))
similar(a::AbstractArray, ::Type{T}, dims::DimOrInd...) where {T}  = similar(a, T, to_shape(dims))
# Similar supports specifying dims as either Integers or AbstractUnitRanges or any mixed combination
# thereof. Ideally, we'd just convert Integers to OneTos and then call a canonical method with the axes,
# but we don't want to require all AbstractArray subtypes to dispatch on Base.OneTo. So instead we
# define this method to convert supported axes to Ints, with the expectation that an offset array
# package will define a method with dims::Tuple{Union{Integer, UnitRange}, Vararg{Union{Integer, UnitRange}}}
similar(a::AbstractArray, ::Type{T}, dims::Tuple{Union{Integer, OneTo}, Vararg{Union{Integer, OneTo}}}) where {T} = similar(a, T, to_shape(dims))
# similar creates an Array by default
similar(a::AbstractArray, ::Type{T}, dims::Dims{N}) where {T,N}    = Array{T,N}(undef, dims)

to_shape(::Tuple{}) = ()
to_shape(dims::Dims) = dims
to_shape(dims::DimsOrInds) = map(to_shape, dims)::DimsOrInds
# each dimension
to_shape(i::Int) = i
to_shape(i::Integer) = Int(i)
to_shape(r::OneTo) = Int(last(r))
to_shape(r::AbstractUnitRange) = r

"""
    similar(storagetype, axes)

Create an uninitialized mutable array analogous to that specified by
`storagetype`, but with `axes` specified by the last
argument. `storagetype` might be a type or a function.

**Examples**:

    similar(Array{Int}, axes(A))

creates an array that "acts like" an `Array{Int}` (and might indeed be
backed by one), but which is indexed identically to `A`. If `A` has
conventional indexing, this will be identical to
`Array{Int}(undef, size(A))`, but if `A` has unconventional indexing then the
indices of the result will match `A`.

    similar(BitArray, (axes(A, 2),))

would create a 1-dimensional logical array whose indices match those
of the columns of `A`.
"""
similar(::Type{T}, dims::DimOrInd...) where {T<:AbstractArray} = similar(T, dims)
similar(::Type{T}, shape::Tuple{Union{Integer, OneTo}, Vararg{Union{Integer, OneTo}}}) where {T<:AbstractArray} = similar(T, to_shape(shape))
similar(::Type{T}, dims::Dims) where {T<:AbstractArray} = T(undef, dims)

"""
    empty(v::AbstractVector, [eltype])

Create an empty vector similar to `v`, optionally changing the `eltype`.

# Examples

```jldoctest
julia> empty([1.0, 2.0, 3.0])
Float64[]

julia> empty([1.0, 2.0, 3.0], String)
String[]
```
"""
empty(a::AbstractVector{T}, ::Type{U}=T) where {T,U} = Vector{U}()

# like empty, but should return a mutable collection, a Vector by default
emptymutable(a::AbstractVector{T}, ::Type{U}=T) where {T,U} = Vector{U}()
emptymutable(itr, ::Type{U}) where {U} = Vector{U}()

"""
    copy!(dst, src) -> dst

In-place [`copy`](@ref) of `src` into `dst`, discarding any pre-existing
elements in `dst`.
If `dst` and `src` are of the same type, `dst == src` should hold after
the call. If `dst` and `src` are multidimensional arrays, they must have
equal [`axes`](@ref).
See also [`copyto!`](@ref).

!!! compat "Julia 1.1"
    This method requires at least Julia 1.1. In Julia 1.0 this method
    is available from the `Future` standard library as `Future.copy!`.
"""
function copy!(dst::AbstractVector, src::AbstractVector)
    if length(dst) != length(src)
        resize!(dst, length(src))
    end
    for i in eachindex(dst, src)
        @inbounds dst[i] = src[i]
    end
    dst
end

function copy!(dst::AbstractArray, src::AbstractArray)
    axes(dst) == axes(src) || throw(ArgumentError(
        "arrays must have the same axes for copy! (consider using `copyto!`)"))
    copyto!(dst, src)
end

## from general iterable to any array

function copyto!(dest::AbstractArray, src)
    destiter = eachindex(dest)
    y = iterate(destiter)
    for x in src
        y === nothing &&
            throw(ArgumentError("destination has fewer elements than required"))
        dest[y[1]] = x
        y = iterate(destiter, y[2])
    end
    return dest
end

function copyto!(dest::AbstractArray, dstart::Integer, src)
    i = Int(dstart)
    for x in src
        dest[i] = x
        i += 1
    end
    return dest
end

# copy from an some iterable object into an AbstractArray
function copyto!(dest::AbstractArray, dstart::Integer, src, sstart::Integer)
    if (sstart < 1)
        throw(ArgumentError(string("source start offset (",sstart,") is < 1")))
    end
    y = iterate(src)
    for j = 1:(sstart-1)
        if y === nothing
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",sstart,", got ",j-1)))
        end
        y = iterate(src, y[2])
    end
    if y === nothing
        throw(ArgumentError(string("source has fewer elements than required, ",
                                   "expected at least ",sstart,", got ",sstart-1)))
    end
    i = Int(dstart)
    while y !== nothing
        val, st = y
        dest[i] = val
        i += 1
        y = iterate(src, st)
    end
    return dest
end

# this method must be separate from the above since src might not have a length
function copyto!(dest::AbstractArray, dstart::Integer, src, sstart::Integer, n::Integer)
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    n == 0 && return dest
    dmax = dstart + n - 1
    inds = LinearIndices(dest)
    if (dstart ∉ inds || dmax ∉ inds) | (sstart < 1)
        sstart < 1 && throw(ArgumentError(string("source start offset (",sstart,") is < 1")))
        throw(BoundsError(dest, dstart:dmax))
    end
    y = iterate(src)
    for j = 1:(sstart-1)
        if y === nothing
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",sstart,", got ",j-1)))
        end
        y = iterate(src, y[2])
    end
    i = Int(dstart)
    while i <= dmax && y !== nothing
        val, st = y
        @inbounds dest[i] = val
        y = iterate(src, st)
        i += 1
    end
    i <= dmax && throw(BoundsError(dest, i))
    return dest
end

## copy between abstract arrays - generally more efficient
## since a single index variable can be used.

"""
    copyto!(dest::AbstractArray, src) -> dest


Copy all elements from collection `src` to array `dest`, whose length must be greater than
or equal to the length `n` of `src`. The first `n` elements of `dest` are overwritten,
the other elements are left untouched.

# Examples
```jldoctest
julia> x = [1., 0., 3., 0., 5.];

julia> y = zeros(7);

julia> copyto!(y, x);

julia> y
7-element Array{Float64,1}:
 1.0
 0.0
 3.0
 0.0
 5.0
 0.0
 0.0
```
"""
function copyto!(dest::AbstractArray, src::AbstractArray)
    isempty(src) && return dest
    src′ = unalias(dest, src)
    copyto_unaliased!(IndexStyle(dest), dest, IndexStyle(src′), src′)
end

function copyto!(deststyle::IndexStyle, dest::AbstractArray, srcstyle::IndexStyle, src::AbstractArray)
    isempty(src) && return dest
    src′ = unalias(dest, src)
    copyto_unaliased!(deststyle, dest, srcstyle, src′)
end

function copyto_unaliased!(deststyle::IndexStyle, dest::AbstractArray, srcstyle::IndexStyle, src::AbstractArray)
    isempty(src) && return dest
    destinds, srcinds = LinearIndices(dest), LinearIndices(src)
    idf, isf = first(destinds), first(srcinds)
    Δi = idf - isf
    (checkbounds(Bool, destinds, isf+Δi) & checkbounds(Bool, destinds, last(srcinds)+Δi)) ||
        throw(BoundsError(dest, srcinds))
    if deststyle isa IndexLinear
        if srcstyle isa IndexLinear
            # Single-index implementation
            @inbounds for i in srcinds
                dest[i + Δi] = src[i]
            end
        else
            # Dual-index implementation
            i = idf - 1
            @inbounds for a in src
                dest[i+=1] = a
            end
        end
    else
        iterdest, itersrc = eachindex(dest), eachindex(src)
        if iterdest == itersrc
            # Shared-iterator implementation
            for I in iterdest
                @inbounds dest[I] = src[I]
            end
        else
            # Dual-iterator implementation
            ret = iterate(iterdest)
            @inbounds for a in src
                idx, state = ret
                dest[idx] = a
                ret = iterate(iterdest, state)
            end
        end
    end
    return dest
end

function copyto!(dest::AbstractArray, dstart::Integer, src::AbstractArray)
    copyto!(dest, dstart, src, first(LinearIndices(src)), length(src))
end

function copyto!(dest::AbstractArray, dstart::Integer, src::AbstractArray, sstart::Integer)
    srcinds = LinearIndices(src)
    checkbounds(Bool, srcinds, sstart) || throw(BoundsError(src, sstart))
    copyto!(dest, dstart, src, sstart, last(srcinds)-sstart+1)
end

function copyto!(dest::AbstractArray, dstart::Integer,
               src::AbstractArray, sstart::Integer,
               n::Integer)
    n == 0 && return dest
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    destinds, srcinds = LinearIndices(dest), LinearIndices(src)
    (checkbounds(Bool, destinds, dstart) && checkbounds(Bool, destinds, dstart+n-1)) || throw(BoundsError(dest, dstart:dstart+n-1))
    (checkbounds(Bool, srcinds, sstart)  && checkbounds(Bool, srcinds, sstart+n-1))  || throw(BoundsError(src,  sstart:sstart+n-1))
    @inbounds for i = 0:(n-1)
        dest[dstart+i] = src[sstart+i]
    end
    return dest
end

function copy(a::AbstractArray)
    @_propagate_inbounds_meta
    copymutable(a)
end

function copyto!(B::AbstractVecOrMat{R}, ir_dest::AbstractRange{Int}, jr_dest::AbstractRange{Int},
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
            @inbounds B[idest,jdest] = A[isrc,jsrc]
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
    copyto!(similar(a), a)
end
copymutable(itr) = collect(itr)

zero(x::AbstractArray{T}) where {T} = fill!(similar(x), zero(T))

## iteration support for arrays by iterating over `eachindex` in the array ##
# Allows fast iteration by default for both IndexLinear and IndexCartesian arrays

# While the definitions for IndexLinear are all simple enough to inline on their
# own, IndexCartesian's CartesianIndices is more complicated and requires explicit
# inlining.
function iterate(A::AbstractArray, state=(eachindex(A),))
    y = iterate(state...)
    y === nothing && return nothing
    A[y[1]], (state[1], tail(y)...)
end

isempty(a::AbstractArray) = (length(a) == 0)


## range conversions ##

map(::Type{T}, r::StepRange) where {T<:Real} = T(r.start):T(r.step):T(last(r))
map(::Type{T}, r::UnitRange) where {T<:Real} = T(r.start):T(last(r))
map(::Type{T}, r::StepRangeLen) where {T<:AbstractFloat} = convert(StepRangeLen{T}, r)
function map(::Type{T}, r::LinRange) where T<:AbstractFloat
    LinRange(T(r.start), T(r.stop), length(r))
end

## unsafe/pointer conversions ##

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer(x::AbstractArray{T}) where {T} = unsafe_convert(Ptr{T}, x)
function pointer(x::AbstractArray{T}, i::Integer) where T
    @_inline_meta
    unsafe_convert(Ptr{T}, x) + (i - first(LinearIndices(x)))*elsize(x)
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
`Int`, an [`AbstractRange`](@ref), or a [`Vector`](@ref). See the manual section on
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
    error_if_canonical_getindex(IndexStyle(A), A, I...)
    _getindex(IndexStyle(A), A, to_indices(A, I)...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    @inbounds r = getindex(A, I...)
    r
end

error_if_canonical_getindex(::IndexLinear, A::AbstractArray, ::Int) =
    error("getindex not defined for ", typeof(A))
error_if_canonical_getindex(::IndexCartesian, A::AbstractArray{T,N}, ::Vararg{Int,N}) where {T,N} =
    error("getindex not defined for ", typeof(A))
error_if_canonical_getindex(::IndexStyle, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_getindex(::IndexStyle, A::AbstractArray, I...) =
    error("getindex for $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing: canonical method is one Int
_getindex(::IndexLinear, A::AbstractArray, i::Int) = (@_propagate_inbounds_meta; getindex(A, i))
function _getindex(::IndexLinear, A::AbstractArray, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...) # generally _to_linear_index requires bounds checking
    @inbounds r = getindex(A, _to_linear_index(A, I...))
    r
end
_to_linear_index(A::AbstractArray, i::Int) = i
_to_linear_index(A::AbstractVector, i::Int, I::Int...) = i
_to_linear_index(A::AbstractArray) = 1
_to_linear_index(A::AbstractArray, I::Int...) = (@_inline_meta; _sub2ind(A, I...))

## IndexCartesian Scalar indexing: Canonical method is full dimensionality of Ints
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
_to_subscript_indices(A::AbstractArray{T,N}) where {T,N} = (@_inline_meta; fill_to_length((), 1, Val(N)))
_to_subscript_indices(A::AbstractArray{T,0}) where {T} = ()
_to_subscript_indices(A::AbstractArray{T,0}, i::Int) where {T} = ()
_to_subscript_indices(A::AbstractArray{T,0}, I::Int...) where {T} = ()
function _to_subscript_indices(A::AbstractArray{T,N}, I::Int...) where {T,N}
    @_inline_meta
    J, Jrem = IteratorsMD.split(I, Val(N))
    _to_subscript_indices(A, J, Jrem)
end
_to_subscript_indices(A::AbstractArray, J::Tuple, Jrem::Tuple{}) =
    __to_subscript_indices(A, axes(A), J, Jrem)
function __to_subscript_indices(A::AbstractArray,
        ::Tuple{AbstractUnitRange,Vararg{AbstractUnitRange}}, J::Tuple, Jrem::Tuple{})
    @_inline_meta
    (J..., map(first, tail(_remaining_size(J, axes(A))))...)
end
_to_subscript_indices(A, J::Tuple, Jrem::Tuple) = J # already bounds-checked, safe to drop
_to_subscript_indices(A::AbstractArray{T,N}, I::Vararg{Int,N}) where {T,N} = I
_remaining_size(::Tuple{Any}, t::Tuple) = t
_remaining_size(h::Tuple, t::Tuple) = (@_inline_meta; _remaining_size(tail(h), tail(t)))
_unsafe_ind2sub(::Tuple{}, i) = () # _ind2sub may throw(BoundsError()) in this case
_unsafe_ind2sub(sz, i) = (@_inline_meta; _ind2sub(sz, i))

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage

"""
    setindex!(A, X, inds...)
    A[inds...] = X

Store values from array `X` within some subset of `A` as specified by `inds`.
The syntax `A[inds...] = X` is equivalent to `setindex!(A, X, inds...)`.

# Examples
```jldoctest
julia> A = zeros(2,2);

julia> setindex!(A, [10, 20], [1, 2]);

julia> A[[3, 4]] = [30, 40];

julia> A
2×2 Array{Float64,2}:
 10.0  30.0
 20.0  40.0
```
"""
function setindex!(A::AbstractArray, v, I...)
    @_propagate_inbounds_meta
    error_if_canonical_setindex(IndexStyle(A), A, I...)
    _setindex!(IndexStyle(A), A, v, to_indices(A, I)...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    @inbounds r = setindex!(A, v, I...)
    r
end

error_if_canonical_setindex(::IndexLinear, A::AbstractArray, ::Int) =
    error("setindex! not defined for ", typeof(A))
error_if_canonical_setindex(::IndexCartesian, A::AbstractArray{T,N}, ::Vararg{Int,N}) where {T,N} =
    error("setindex! not defined for ", typeof(A))
error_if_canonical_setindex(::IndexStyle, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_setindex!(::IndexStyle, A::AbstractArray, v, I...) =
    error("setindex! for $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing
_setindex!(::IndexLinear, A::AbstractArray, v, i::Int) = (@_propagate_inbounds_meta; setindex!(A, v, i))
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
function _setindex!(::IndexCartesian, A::AbstractArray, v, I::Vararg{Int,M}) where M
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_subscript_indices(A, I...)...)
    r
end

"""
    parent(A)

Return the underlying "parent array”. This parent array of objects of types `SubArray`, `ReshapedArray`
or `LinearAlgebra.Transpose` is what was passed as an argument to `view`, `reshape`, `transpose`, etc.
during object creation. If the input is not a wrapped object, return the input itself.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> V = view(A, 1:2, :)
2×2 view(::Array{Int64,2}, 1:2, :) with eltype Int64:
 1  2
 3  4

julia> parent(V)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
parent(a::AbstractArray) = a

## rudimentary aliasing detection ##
"""
    Base.unalias(dest, A)

Return either `A` or a copy of `A` in a rough effort to prevent modifications to `dest` from
affecting the returned object. No guarantees are provided.

Custom arrays that wrap or use fields containing arrays that might alias against other
external objects should provide a [`Base.dataids`](@ref) implementation.

This function must return an object of exactly the same type as `A` for performance and type
stability. Mutable custom arrays for which [`copy(A)`](@ref) is not `typeof(A)` should
provide a [`Base.unaliascopy`](@ref) implementation.

See also [`Base.mightalias`](@ref).
"""
unalias(dest, A::AbstractArray) = mightalias(dest, A) ? unaliascopy(A) : A
unalias(dest, A::AbstractRange) = A
unalias(dest, A) = A

"""
    Base.unaliascopy(A)

Make a preventative copy of `A` in an operation where `A` [`Base.mightalias`](@ref) against
another array in order to preserve consistent semantics as that other array is mutated.

This must return an object of the same type as `A` to preserve optimal performance in the
much more common case where aliasing does not occur. By default,
`unaliascopy(A::AbstractArray)` will attempt to use [`copy(A)`](@ref), but in cases where
`copy(A)` is not a `typeof(A)`, then the array should provide a custom implementation of
`Base.unaliascopy(A)`.
"""
unaliascopy(A::Array) = copy(A)
unaliascopy(A::AbstractArray)::typeof(A) = (@_noinline_meta; _unaliascopy(A, copy(A)))
_unaliascopy(A::T, C::T) where {T} = C
_unaliascopy(A, C) = throw(ArgumentError("""
    an array of type `$(typeof(A).name)` shares memory with another argument and must
    make a preventative copy of itself in order to maintain consistent semantics,
    but `copy(A)` returns a new array of type `$(typeof(C))`. To fix, implement:
        `Base.unaliascopy(A::$(typeof(A).name))::typeof(A)`"""))
unaliascopy(A) = A

"""
    Base.mightalias(A::AbstractArray, B::AbstractArray)

Perform a conservative test to check if arrays `A` and `B` might share the same memory.

By default, this simply checks if either of the arrays reference the same memory
regions, as identified by their [`Base.dataids`](@ref).
"""
mightalias(A::AbstractArray, B::AbstractArray) = !isbits(A) && !isbits(B) && !_isdisjoint(dataids(A), dataids(B))
mightalias(x, y) = false

_isdisjoint(as::Tuple{}, bs::Tuple{}) = true
_isdisjoint(as::Tuple{}, bs::Tuple{UInt}) = true
_isdisjoint(as::Tuple{}, bs::Tuple) = true
_isdisjoint(as::Tuple{UInt}, bs::Tuple{}) = true
_isdisjoint(as::Tuple{UInt}, bs::Tuple{UInt}) = as[1] != bs[1]
_isdisjoint(as::Tuple{UInt}, bs::Tuple) = !(as[1] in bs)
_isdisjoint(as::Tuple, bs::Tuple{}) = true
_isdisjoint(as::Tuple, bs::Tuple{UInt}) = !(bs[1] in as)
_isdisjoint(as::Tuple, bs::Tuple) = !(as[1] in bs) && _isdisjoint(tail(as), bs)

"""
    Base.dataids(A::AbstractArray)

Return a tuple of `UInt`s that represent the mutable data segments of an array.

Custom arrays that would like to opt-in to aliasing detection of their component
parts can specialize this method to return the concatenation of the `dataids` of
their component parts.  A typical definition for an array that wraps a parent is
`Base.dataids(C::CustomArray) = dataids(C.parent)`.
"""
dataids(A::AbstractArray) = (UInt(objectid(A)),)
dataids(A::Array) = (UInt(pointer(A)),)
dataids(::AbstractRange) = ()
dataids(x) = ()

## get (getindex with a default value) ##

RangeVecIntList{A<:AbstractVector{Int}} = Union{Tuple{Vararg{Union{AbstractRange, AbstractVector{Int}}}},
    AbstractVector{UnitRange{Int}}, AbstractVector{AbstractRange{Int}}, AbstractVector{A}}

get(A::AbstractArray, i::Integer, default) = checkbounds(Bool, A, i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = checkbounds(Bool, A) ? A[] : default
get(A::AbstractArray, I::Dims, default) = checkbounds(Bool, A, I...) ? A[I...] : default

function get!(X::AbstractVector{T}, A::AbstractVector, I::Union{AbstractRange,AbstractVector{Int}}, default::T) where T
    # 1d is not linear indexing
    ind = findall(in(axes1(A)), I)
    X[ind] = A[I[ind]]
    Xind = axes1(X)
    X[first(Xind):first(ind)-1] = default
    X[last(ind)+1:last(Xind)] = default
    X
end
function get!(X::AbstractArray{T}, A::AbstractArray, I::Union{AbstractRange,AbstractVector{Int}}, default::T) where T
    # Linear indexing
    ind = findall(in(1:length(A)), I)
    X[ind] = A[I[ind]]
    fill!(view(X, 1:first(ind)-1), default)
    fill!(view(X, last(ind)+1:length(X)), default)
    X
end

get(A::AbstractArray, I::AbstractRange, default) = get!(similar(A, typeof(default), index_shape(I)), A, I, default)

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
_cat(catdim::Integer) = Vector{Any}()

typed_vcat(::Type{T}) where {T} = Vector{T}()
typed_hcat(::Type{T}) where {T} = Vector{T}()

## cat: special cases
vcat(X::T...) where {T}         = T[ X[i] for i=1:length(X) ]
vcat(X::T...) where {T<:Number} = T[ X[i] for i=1:length(X) ]
hcat(X::T...) where {T}         = T[ X[j] for i=1:1, j=1:length(X) ]
hcat(X::T...) where {T<:Number} = T[ X[j] for i=1:1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill(Vector{promote_typeof(X...)}(undef, length(X)), X)
hcat(X::Number...) = hvcat_fill(Matrix{promote_typeof(X...)}(undef, 1,length(X)), X)
typed_vcat(::Type{T}, X::Number...) where {T} = hvcat_fill(Vector{T}(undef, length(X)), X)
typed_hcat(::Type{T}, X::Number...) where {T} = hvcat_fill(Matrix{T}(undef, 1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat(V::AbstractVector{T}...) where {T} = typed_vcat(T, V...)

# FIXME: this alias would better be Union{AbstractVector{T}, Tuple{Vararg{T}}}
# and method signatures should do AbstractVecOrTuple{<:T} when they want covariance,
# but that solution currently fails (see #27188 and #27224)
AbstractVecOrTuple{T} = Union{AbstractVector{<:T}, Tuple{Vararg{T}}}

function _typed_vcat(::Type{T}, V::AbstractVecOrTuple{AbstractVector}) where T
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

typed_hcat(::Type{T}, A::AbstractVecOrMat...) where {T} = _typed_hcat(T, A)

hcat(A::AbstractVecOrMat...) = typed_hcat(promote_eltype(A...), A...)
hcat(A::AbstractVecOrMat{T}...) where {T} = typed_hcat(T, A...)

function _typed_hcat(::Type{T}, A::AbstractVecOrTuple{AbstractVecOrMat}) where T
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
            copyto!(B, pos, Ak, 1, n)
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

vcat(A::AbstractVecOrMat...) = typed_vcat(promote_eltype(A...), A...)
vcat(A::AbstractVecOrMat{T}...) where {T} = typed_vcat(T, A...)

function _typed_vcat(::Type{T}, A::AbstractVecOrTuple{AbstractVecOrMat}) where T
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

typed_vcat(::Type{T}, A::AbstractVecOrMat...) where {T} = _typed_vcat(T, A)

reduce(::typeof(vcat), A::AbstractVector{<:AbstractVecOrMat}) =
    _typed_vcat(mapreduce(eltype, promote_type, A), A)

reduce(::typeof(hcat), A::AbstractVector{<:AbstractVecOrMat}) =
    _typed_hcat(mapreduce(eltype, promote_type, A), A)

## cat: general case

# helper functions
cat_size(A) = (1,)
cat_size(A::AbstractArray) = size(A)
cat_size(A, d) = 1
cat_size(A::AbstractArray, d) = size(A, d)

cat_indices(A, d) = OneTo(1)
cat_indices(A::AbstractArray, d) = axes(A, d)

cat_similar(A, T, shape) = Array{T}(undef, shape)
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

function dims2cat(::Val{n}) where {n}
    n <= 0 && throw(ArgumentError("cat dimension must be a positive integer, but got $n"))
    ntuple(i -> (i == n), Val(n))
end

function dims2cat(dims)
    if any(dims .<= 0)
        throw(ArgumentError("All cat dimensions must be positive integers, but got $dims"))
    end
    ntuple(in(dims), maximum(dims))
end

_cat(dims, X...) = cat_t(promote_eltypeof(X...), X...; dims=dims)

@inline cat_t(::Type{T}, X...; dims) where {T} = _cat_t(dims, T, X...)
@inline function _cat_t(dims, T::Type, X...)
    catdims = dims2cat(dims)
    shape = cat_shape(catdims, (), map(cat_size, X)...)
    A = cat_similar(X[1], T, shape)
    if T <: Number && count(!iszero, catdims) > 1
        fill!(A, zero(T))
    end
    return __cat(A, shape, catdims, X...)
end

function __cat(A, shape::NTuple{N}, catdims, X...) where N
    offsets = zeros(Int, N)
    inds = Vector{UnitRange{Int}}(undef, N)
    concat = copyto!(zeros(Bool, N), catdims)
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
        if x isa AbstractArray
            A[I...] = x
        else
            fill!(view(A, I...), x)
        end
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
vcat(X...) = cat(X...; dims=Val(1))
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

julia> x = Matrix(undef, 3, 0)  # x = [] would have created an Array{Any, 1}, but need an Array{Any, 2}
3×0 Array{Any,2}

julia> hcat(x, [1; 2; 3])
3×1 Array{Any,2}:
 1
 2
 3
```
"""
hcat(X...) = cat(X...; dims=Val(2))

typed_vcat(T::Type, X...) = cat_t(T, X...; dims=Val(1))
typed_hcat(T::Type, X...) = cat_t(T, X...; dims=Val(2))

"""
    cat(A...; dims=dims)

Concatenate the input arrays along the specified dimensions in the iterable `dims`. For
dimensions not in `dims`, all input arrays should have the same size, which will also be the
size of the output array along that dimension. For dimensions in `dims`, the size of the
output array is the sum of the sizes of the input arrays along that dimension. If `dims` is
a single number, the different arrays are tightly stacked along that dimension. If `dims` is
an iterable containing several dimensions, this allows one to construct block diagonal
matrices and their higher-dimensional analogues by simultaneously increasing several
dimensions for every new input array and putting zero blocks elsewhere. For example,
`cat(matrices...; dims=(1,2))` builds a block diagonal matrix, i.e. a block matrix with
`matrices[1]`, `matrices[2]`, ... as diagonal blocks and matching zero blocks away from the
diagonal.
"""
@inline cat(A...; dims) = _cat(dims, A...)
_cat(catdims, A::AbstractArray{T}...) where {T} = cat_t(T, A...; dims=catdims)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(A; dims=Val(1))
vcat(A::AbstractArray, B::AbstractArray) = cat(A, B; dims=Val(1))
vcat(A::AbstractArray...) = cat(A...; dims=Val(1))
hcat(A::AbstractArray) = cat(A; dims=Val(2))
hcat(A::AbstractArray, B::AbstractArray) = cat(A, B; dims=Val(2))
hcat(A::AbstractArray...) = cat(A...; dims=Val(2))

typed_vcat(T::Type, A::AbstractArray) = cat_t(T, A; dims=Val(1))
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(T, A, B; dims=Val(1))
typed_vcat(T::Type, A::AbstractArray...) = cat_t(T, A...; dims=Val(1))
typed_hcat(T::Type, A::AbstractArray) = cat_t(T, A; dims=Val(2))
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(T, A, B; dims=Val(2))
typed_hcat(T::Type, A::AbstractArray...) = cat_t(T, A...; dims=Val(2))

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
typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}) where {T} = Vector{T}()

function hvcat(rows::Tuple{Vararg{Int}}, xs::T...) where T<:Number
    nr = length(rows)
    nc = rows[1]

    a = Matrix{T}(undef, nr, nc)
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
hvcat(rows::Tuple{Vararg{Int}}, xs...) = typed_hvcat(promote_eltypeof(xs...), rows, xs...)

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
    hvcat_fill(Matrix{T}(undef, nr, nc), xs)
end

function typed_hvcat(::Type{T}, rows::Tuple{Vararg{Int}}, as...) where T
    nbr = length(rows)  # number of block rows
    rs = Vector{Any}(undef, nbr)
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
    if axes(A) != axes(B)
        return false
    end
    for (a, b) in zip(A, B)
        if !isequal(a, b)
            return false
        end
    end
    return true
end

function cmp(A::AbstractVector, B::AbstractVector)
    for (a, b) in zip(A, B)
        if !isequal(a, b)
            return isless(a, b) ? -1 : 1
        end
    end
    return cmp(length(A), length(B))
end

isless(A::AbstractVector, B::AbstractVector) = cmp(A, B) < 0

function (==)(A::AbstractArray, B::AbstractArray)
    if axes(A) != axes(B)
        return false
    end
    anymissing = false
    for (a, b) in zip(A, B)
        eq = (a == b)
        if ismissing(eq)
            anymissing = true
        elseif !eq
            return false
        end
    end
    return anymissing ? missing : true
end

# _sub2ind and _ind2sub
# fallbacks
function _sub2ind(A::AbstractArray, I...)
    @_inline_meta
    _sub2ind(axes(A), I...)
end

function _ind2sub(A::AbstractArray, ind)
    @_inline_meta
    _ind2sub(axes(A), ind)
end

# 0-dimensional arrays and indexing with []
_sub2ind(::Tuple{}) = 1
_sub2ind(::DimsInteger) = 1
_sub2ind(::Indices) = 1
_sub2ind(::Tuple{}, I::Integer...) = (@_inline_meta; _sub2ind_recurse((), 1, 1, I...))

# Generic cases
_sub2ind(dims::DimsInteger, I::Integer...) = (@_inline_meta; _sub2ind_recurse(dims, 1, 1, I...))
_sub2ind(inds::Indices, I::Integer...) = (@_inline_meta; _sub2ind_recurse(inds, 1, 1, I...))
# In 1d, there's a question of whether we're doing cartesian indexing
# or linear indexing. Support only the former.
_sub2ind(inds::Indices{1}, I::Integer...) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
_sub2ind(inds::Tuple{OneTo}, I::Integer...) = (@_inline_meta; _sub2ind_recurse(inds, 1, 1, I...)) # only OneTo is safe
_sub2ind(inds::Tuple{OneTo}, i::Integer)    = i

_sub2ind_recurse(::Any, L, ind) = ind
function _sub2ind_recurse(::Tuple{}, L, ind, i::Integer, I::Integer...)
    @_inline_meta
    _sub2ind_recurse((), L, ind+(i-1)*L, I...)
end
function _sub2ind_recurse(inds, L, ind, i::Integer, I::Integer...)
    @_inline_meta
    r1 = inds[1]
    _sub2ind_recurse(tail(inds), nextL(L, r1), ind+offsetin(i, r1)*L, I...)
end

nextL(L, l::Integer) = L*l
nextL(L, r::AbstractUnitRange) = L*unsafe_length(r)
nextL(L, r::Slice) = L*unsafe_length(r.indices)
offsetin(i, l::Integer) = i-1
offsetin(i, r::AbstractUnitRange) = i-first(r)

_ind2sub(::Tuple{}, ind::Integer) = (@_inline_meta; ind == 1 ? () : throw(BoundsError()))
_ind2sub(dims::DimsInteger, ind::Integer) = (@_inline_meta; _ind2sub_recurse(dims, ind-1))
_ind2sub(inds::Indices, ind::Integer)     = (@_inline_meta; _ind2sub_recurse(inds, ind-1))
_ind2sub(inds::Indices{1}, ind::Integer) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
_ind2sub(inds::Tuple{OneTo}, ind::Integer) = (ind,)

_ind2sub_recurse(::Tuple{}, ind) = (ind+1,)
function _ind2sub_recurse(indslast::NTuple{1}, ind)
    @_inline_meta
    (_lookup(ind, indslast[1]),)
end
function _ind2sub_recurse(inds, ind)
    @_inline_meta
    r1 = inds[1]
    indnext, f, l = _div(ind, r1)
    (ind-l*indnext+f, _ind2sub_recurse(tail(inds), indnext)...)
end

_lookup(ind, d::Integer) = ind+1
_lookup(ind, r::AbstractUnitRange) = ind+first(r)
_div(ind, d::Integer) = div(ind, d), 1, d
_div(ind, r::AbstractUnitRange) = (d = unsafe_length(r); (div(ind, d), first(r), d))

# Vectorized forms
function _sub2ind(inds::Indices{1}, I1::AbstractVector{T}, I::AbstractVector{T}...) where T<:Integer
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
end
_sub2ind(inds::Tuple{OneTo}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} =
    _sub2ind_vecs(inds, I1, I...)
_sub2ind(inds::Union{DimsInteger,Indices}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} =
    _sub2ind_vecs(inds, I1, I...)
function _sub2ind_vecs(inds, I::AbstractVector...)
    I1 = I[1]
    Iinds = axes1(I1)
    for j = 2:length(I)
        axes1(I[j]) == Iinds || throw(DimensionMismatch("indices of I[1] ($(Iinds)) does not match indices of I[$j] ($(axes1(I[j])))"))
    end
    Iout = similar(I1)
    _sub2ind!(Iout, inds, Iinds, I)
    Iout
end

function _sub2ind!(Iout, inds, Iinds, I)
    @_noinline_meta
    for i in Iinds
        # Iout[i] = _sub2ind(inds, map(Ij -> Ij[i], I)...)
        Iout[i] = sub2ind_vec(inds, i, I)
    end
    Iout
end

sub2ind_vec(inds, i, I) = (@_inline_meta; _sub2ind(inds, _sub2ind_vec(i, I...)...))
_sub2ind_vec(i, I1, I...) = (@_inline_meta; (I1[i], _sub2ind_vec(i, I...)...))
_sub2ind_vec(i) = ()

function _ind2sub(inds::Union{DimsInteger{N},Indices{N}}, ind::AbstractVector{<:Integer}) where N
    M = length(ind)
    t = ntuple(n->similar(ind),Val(N))
    for (i,idx) in pairs(IndexLinear(), ind)
        sub = _ind2sub(inds, idx)
        for j = 1:N
            t[j][i] = sub[j]
        end
    end
    t
end

## iteration utilities ##

"""
    foreach(f, c...) -> Nothing

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
    mapslices(f, A; dims)

Transform the given dimensions of array `A` using function `f`. `f` is called on each slice
of `A` of the form `A[...,:,...,:,...]`. `dims` is an integer vector specifying where the
colons go in this expression. The results are concatenated along the remaining dimensions.
For example, if `dims` is `[1,2]` and `A` is 4-dimensional, `f` is called on `A[:,:,i,j]`
for all `i` and `j`.

# Examples
```jldoctest
julia> a = reshape(Vector(1:16),(2,2,2,2))
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

julia> mapslices(sum, a, dims = [1,2])
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
function mapslices(f, A::AbstractArray; dims)
    if isempty(dims)
        return map(f,A)
    end
    if !isa(dims, AbstractVector)
        dims = [dims...]
    end

    dimsA = [axes(A)...]
    ndimsA = ndims(A)
    alldims = [1:ndimsA;]

    otherdims = setdiff(alldims, dims)

    idx = Any[first(ind) for ind in axes(A)]
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = Slice(axes(A, d))
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
        # If the result of f on a single slice is a scalar then we add singleton
        # dimensions. When adding the dimensions, we have to respect the
        # index type of the input array (e.g. in the case of OffsetArrays)
        tmp = similar(Aslice, typeof(r1), reduced_indices(Aslice, 1:ndims(Aslice)))
        tmp[firstindex(tmp)] = r1
        r1 = tmp
    end
    nextra = max(0, length(dims)-ndims(r1))
    if eltype(Rsize) == Int
        Rsize[dims] = [size(r1)..., ntuple(d->1, nextra)...]
    else
        Rsize[dims] = [axes(r1)..., ntuple(d->OneTo(1), nextra)...]
    end
    R = similar(r1, tuple(Rsize...,))

    ridx = Any[map(first, axes(R))...]
    for d in dims
        ridx[d] = axes(R,d)
    end

    concatenate_setindex!(R, r1, ridx...)

    nidx = length(otherdims)
    indices = Iterators.drop(CartesianIndices(itershape), 1) # skip the first element, we already handled it
    inner_mapslices!(safe_for_reuse, indices, nidx, idx, otherdims, ridx, Aslice, A, f, R)
end

@noinline function inner_mapslices!(safe_for_reuse, indices, nidx, idx, otherdims, ridx, Aslice, A, f, R)
    if safe_for_reuse
        # when f returns an array, R[ridx...] = f(Aslice) line copies elements,
        # so we can reuse Aslice
        for I in indices
            replace_tuples!(nidx, idx, ridx, otherdims, I)
            _unsafe_getindex!(Aslice, A, idx...)
            concatenate_setindex!(R, f(Aslice), ridx...)
        end
    else
        # we can't guarantee safety (#18524), so allocate new storage for each slice
        for I in indices
            replace_tuples!(nidx, idx, ridx, otherdims, I)
            concatenate_setindex!(R, f(A[idx...]), ridx...)
        end
    end

    return R
end

function replace_tuples!(nidx, idx, ridx, otherdims, I)
    for i in 1:nidx
        idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
    end
end

concatenate_setindex!(R, v, I...) = (R[I...] .= (v,); R)
concatenate_setindex!(R, X::AbstractArray, I...) = (R[I...] = X)

## 1 argument

function map!(f::F, dest::AbstractArray, A::AbstractArray) where F
    for (i,j) in zip(eachindex(dest),eachindex(A))
        val = f(@inbounds A[j])
        @inbounds dest[i] = val
    end
    return dest
end

# map on collections
map(f, A::AbstractArray) = collect_similar(A, Generator(f,A))

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

map(f, ::AbstractDict) = error("map is not defined on dictionaries")
map(f, ::AbstractSet) = error("map is not defined on sets")

## 2 argument
function map!(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray) where F
    for (i, j, k) in zip(eachindex(dest), eachindex(A), eachindex(B))
        @inbounds a, b = A[j], B[k]
        val = f(a, b)
        @inbounds dest[i] = val
    end
    return dest
end

## N argument

@inline ith_all(i, ::Tuple{}) = ()
function ith_all(i, as)
    @_propagate_inbounds_meta
    return (as[1][i], ith_all(i, tail(as))...)
end

function map_n!(f::F, dest::AbstractArray, As) where F
    idxs1 = LinearIndices(As[1])
    @boundscheck LinearIndices(dest) == idxs1 && all(x -> LinearIndices(x) == idxs1, As)
    for i = idxs1
        @inbounds I = ith_all(i, As)
        val = f(I...)
        @inbounds dest[i] = val
    end
    return dest
end

"""
    map!(function, destination, collection...)

Like [`map`](@ref), but stores the result in `destination` rather than a new
collection. `destination` must be at least as large as the first collection.

# Examples
```jldoctest
julia> a = zeros(3);

julia> map!(x -> x * 2, a, [1, 2, 3]);

julia> a
3-element Array{Float64,1}:
 2.0
 4.0
 6.0
```
"""
function map!(f::F, dest::AbstractArray, As::AbstractArray...) where {F}
    isempty(As) && throw(ArgumentError(
        """map! requires at least one "source" argument"""))
    map_n!(f, dest, As)
end

map(f) = f()
map(f, iters...) = collect(Generator(f, iters...))

# multi-item push!, pushfirst! (built on top of type-specific 1-item version)
# (note: must not cause a dispatch loop when 1-item case is not defined)
push!(A, a, b) = push!(push!(A, a), b)
push!(A, a, b, c...) = push!(push!(A, a, b), c...)
pushfirst!(A, a, b) = pushfirst!(pushfirst!(A, b), a)
pushfirst!(A, a, b, c...) = pushfirst!(pushfirst!(A, c...), a, b)

## hashing AbstractArray ##

function hash(A::AbstractArray, h::UInt)
    h = hash(AbstractArray, h)
    # Axes are themselves AbstractArrays, so hashing them directly would stack overflow
    # Instead hash the tuple of firsts and lasts along each dimension
    h = hash(map(first, axes(A)), h)
    h = hash(map(last, axes(A)), h)
    isempty(A) && return h

    # Goal: Hash approximately log(N) entries with a higher density of hashed elements
    # weighted towards the end and special consideration for repeated values. Colliding
    # hashes will often subsequently be compared by equality -- and equality between arrays
    # works elementwise forwards and is short-circuiting. This means that a collision
    # between arrays that differ by elements at the beginning is cheaper than one where the
    # difference is towards the end. Furthermore, blindly choosing log(N) entries from a
    # sparse array will likely only choose the same element repeatedly (zero in this case).

    # To achieve this, we work backwards, starting by hashing the last element of the
    # array. After hashing each element, we skip `fibskip` elements, where `fibskip`
    # is pulled from the Fibonacci sequence -- Fibonacci was chosen as a simple
    # ~O(log(N)) algorithm that ensures we don't hit a common divisor of a dimension
    # and only end up hashing one slice of the array (as might happen with powers of
    # two). Finally, we find the next distinct value from the one we just hashed.

    # This is a little tricky since skipping an integer number of values inherently works
    # with linear indices, but `findprev` uses `keys`. Hoist out the conversion "maps":
    ks = keys(A)
    key_to_linear = LinearIndices(ks) # Index into this map to compute the linear index
    linear_to_key = vec(ks)           # And vice-versa

    # Start at the last index
    keyidx = last(ks)
    linidx = key_to_linear[keyidx]
    fibskip = prevfibskip = oneunit(linidx)
    first_linear = first(LinearIndices(linear_to_key))
    n = 0
    while true
        n += 1
        # Hash the current key-index and its element
        elt = A[keyidx]
        h = hash(keyidx=>elt, h)

        # Skip backwards a Fibonacci number of indices -- this is a linear index operation
        linidx = key_to_linear[keyidx]
        linidx < fibskip + first_linear && break
        linidx -= fibskip
        keyidx = linear_to_key[linidx]

        # Only increase the Fibonacci skip once every N iterations. This was chosen
        # to be big enough that all elements of small arrays get hashed while
        # obscenely large arrays are still tractable. With a choice of N=4096, an
        # entirely-distinct 8000-element array will have ~75% of its elements hashed,
        # with every other element hashed in the first half of the array. At the same
        # time, hashing a `typemax(Int64)`-length Float64 range takes about a second.
        if rem(n, 4096) == 0
            fibskip, prevfibskip = fibskip + prevfibskip, fibskip
        end

        # Find a key index with a value distinct from `elt` -- might be `keyidx` itself
        keyidx = findprev(!isequal(elt), A, keyidx)
        keyidx === nothing && break
    end

    return h
end
