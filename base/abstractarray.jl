# This file is a part of Julia. License is MIT: https://julialang.org/license

## Basic functions ##

"""
    AbstractArray{T,N}

Supertype for `N`-dimensional arrays (or array-like types) with elements of type `T`.
[`Array`](@ref) and other types are subtypes of this. See the manual section on the
[`AbstractArray` interface](@ref man-interface-array).

See also: [`AbstractVector`](@ref), [`AbstractMatrix`](@ref), [`eltype`](@ref), [`ndims`](@ref).
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

See also: [`length`](@ref), [`ndims`](@ref), [`eachindex`](@ref), [`sizeof`](@ref).

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

# Usage note

Each of the indices has to be an `AbstractUnitRange{<:Integer}`, but at the same time can be
a type that uses custom indices. So, for example, if you need a subset, use generalized
indexing constructs like `begin`/`end` or [`firstindex`](@ref)/[`lastindex`](@ref):

```julia
ix = axes(v, 1)
ix[2:end]          # will work for eg Vector, but may fail in general
ix[(begin+1):end]  # works for generalized indexes
```
"""
function axes(A::AbstractArray{T,N}, d) where {T,N}
    @inline
    d::Integer <= N ? axes(A)[d] : OneTo(1)
end

"""
    axes(A)

Return the tuple of valid indices for array `A`.

See also: [`size`](@ref), [`keys`](@ref), [`eachindex`](@ref).

# Examples

```jldoctest
julia> A = fill(1, (5,6,7));

julia> axes(A)
(Base.OneTo(5), Base.OneTo(6), Base.OneTo(7))
```
"""
function axes(A)
    @inline
    map(oneto, size(A))
end

"""
    has_offset_axes(A)
    has_offset_axes(A, B, ...)

Return `true` if the indices of `A` start with something other than 1 along any axis.
If multiple arguments are passed, equivalent to `has_offset_axes(A) | has_offset_axes(B) | ...`.

See also [`require_one_based_indexing`](@ref).
"""
has_offset_axes(A) = _tuple_any(x->Int(first(x))::Int != 1, axes(A))
has_offset_axes(A::AbstractVector) = Int(firstindex(A))::Int != 1 # improve performance of a common case (ranges)
has_offset_axes(A...) = _tuple_any(has_offset_axes, A)
has_offset_axes(::Colon) = false

"""
    require_one_based_indexing(A::AbstractArray)
    require_one_based_indexing(A,B...)

Throw an `ArgumentError` if the indices of any argument start with something other than `1` along any axis.
See also [`has_offset_axes`](@ref).

!!! compat "Julia 1.2"
     This function requires at least Julia 1.2.
"""
require_one_based_indexing(A...) = !has_offset_axes(A...) || throw(ArgumentError("offset arrays are not supported but got an array with index other than 1"))

# Performance optimization: get rid of a branch on `d` in `axes(A, d)`
# for d=1. 1d arrays are heavily used, and the first dimension comes up
# in other applications.
axes1(A::AbstractArray{<:Any,0}) = OneTo(1)
axes1(A::AbstractArray) = (@inline; axes(A)[1])
axes1(iter) = oneto(length(iter))

"""
    keys(a::AbstractArray)

Return an efficient array describing all valid indices for `a` arranged in the shape of `a` itself.

They keys of 1-dimensional arrays (vectors) are integers, whereas all other N-dimensional
arrays use [`CartesianIndex`](@ref) to describe their locations.  Often the special array
types [`LinearIndices`](@ref) and [`CartesianIndices`](@ref) are used to efficiently
represent these arrays of integers and `CartesianIndex`es, respectively.

Note that the `keys` of an array might not be the most efficient index type; for maximum
performance use  [`eachindex`](@ref) instead.
"""
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


"""
    eltype(type)

Determine the type of the elements generated by iterating a collection of the given `type`.
For dictionary types, this will be a `Pair{KeyType,ValType}`. The definition
`eltype(x) = eltype(typeof(x))` is provided for convenience so that instances can be passed
instead of types. However the form that accepts a type argument should be defined for new
types.

See also: [`keytype`](@ref), [`typeof`](@ref).

# Examples
```jldoctest
julia> eltype(fill(1f0, (2,2)))
Float32

julia> eltype(fill(0x1, (2,2)))
UInt8
```
"""
eltype(::Type) = Any
eltype(::Type{Bottom}) = throw(ArgumentError("Union{} does not have elements"))
eltype(x) = eltype(typeof(x))
eltype(::Type{<:AbstractArray{E}}) where {E} = @isdefined(E) ? E : Any

"""
    elsize(type)

Compute the memory stride in bytes between consecutive elements of `eltype`
stored inside the given `type`, if the array elements are stored densely with a
uniform linear stride.

# Examples
```jldoctest
julia> Base.elsize(rand(Float32, 10))
4
```
"""
elsize(A::AbstractArray) = elsize(typeof(A))

"""
    ndims(A::AbstractArray) -> Integer

Return the number of dimensions of `A`.

See also: [`size`](@ref), [`axes`](@ref).

# Examples
```jldoctest
julia> A = fill(1, (3,4,5));

julia> ndims(A)
3
```
"""
ndims(::AbstractArray{T,N}) where {T,N} = N
ndims(::Type{<:AbstractArray{<:Any,N}}) where {N} = N

"""
    length(collection) -> Integer

Return the number of elements in the collection.

Use [`lastindex`](@ref) to get the last valid index of an indexable collection.

See also: [`size`](@ref), [`ndims`](@ref), [`eachindex`](@ref).

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
length(t::AbstractArray) = (@inline; prod(size(t)))

# `eachindex` is mostly an optimization of `keys`
eachindex(itrs...) = keys(itrs...)

# eachindex iterates over all indices. IndexCartesian definitions are later.
eachindex(A::AbstractVector) = (@inline(); axes1(A))


@noinline function throw_eachindex_mismatch_indices(::IndexLinear, inds...)
    throw(DimensionMismatch("all inputs to eachindex must have the same indices, got $(join(inds, ", ", " and "))"))
end
@noinline function throw_eachindex_mismatch_indices(::IndexCartesian, inds...)
    throw(DimensionMismatch("all inputs to eachindex must have the same axes, got $(join(inds, ", ", " and "))"))
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
If the arrays have different sizes and/or dimensionalities, a `DimensionMismatch` exception
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
eachindex(A::AbstractArray) = (@inline(); eachindex(IndexStyle(A), A))

function eachindex(A::AbstractArray, B::AbstractArray)
    @inline
    eachindex(IndexStyle(A,B), A, B)
end
function eachindex(A::AbstractArray, B::AbstractArray...)
    @inline
    eachindex(IndexStyle(A,B...), A, B...)
end
eachindex(::IndexLinear, A::AbstractArray) = (@inline; oneto(length(A)))
eachindex(::IndexLinear, A::AbstractVector) = (@inline; axes1(A))
function eachindex(::IndexLinear, A::AbstractArray, B::AbstractArray...)
    @inline
    indsA = eachindex(IndexLinear(), A)
    _all_match_first(X->eachindex(IndexLinear(), X), indsA, B...) ||
        throw_eachindex_mismatch_indices(IndexLinear(), eachindex(A), eachindex.(B)...)
    indsA
end
function _all_match_first(f::F, inds, A, B...) where F<:Function
    @inline
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

See also: [`axes`](@ref), [`firstindex`](@ref), [`eachindex`](@ref), [`prevind`](@ref).

# Examples
```jldoctest
julia> lastindex([1,2,4])
3

julia> lastindex(rand(3,4,5), 2)
4
```
"""
lastindex(a::AbstractArray) = (@inline; last(eachindex(IndexLinear(), a)))
lastindex(a, d) = (@inline; last(axes(a, d)))

"""
    firstindex(collection) -> Integer
    firstindex(collection, d) -> Integer

Return the first index of `collection`. If `d` is given, return the first index of `collection` along dimension `d`.

The syntaxes `A[begin]` and `A[1, begin]` lower to `A[firstindex(A)]` and
`A[1, firstindex(A, 2)]`, respectively.

See also: [`first`](@ref), [`axes`](@ref), [`lastindex`](@ref), [`nextind`](@ref).

# Examples
```jldoctest
julia> firstindex([1,2,4])
1

julia> firstindex(rand(3,4,5), 2)
1
```
"""
firstindex(a::AbstractArray) = (@inline; first(eachindex(IndexLinear(), a)))
firstindex(a, d) = (@inline; first(axes(a, d)))

first(a::AbstractArray) = a[first(eachindex(a))]

"""
    first(coll)

Get the first element of an iterable collection. Return the start point of an
[`AbstractRange`](@ref) even if it is empty.

See also: [`only`](@ref), [`firstindex`](@ref), [`last`](@ref).

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
    first(itr, n::Integer)

Get the first `n` elements of the iterable collection `itr`, or fewer elements if `itr` is not
long enough.

See also: [`startswith`](@ref), [`Iterators.take`](@ref).

!!! compat "Julia 1.6"
    This method requires at least Julia 1.6.

# Examples
```jldoctest
julia> first(["foo", "bar", "qux"], 2)
2-element Vector{String}:
 "foo"
 "bar"

julia> first(1:6, 10)
1:6

julia> first(Bool[], 1)
Bool[]
```
"""
first(itr, n::Integer) = collect(Iterators.take(itr, n))
# Faster method for vectors
function first(v::AbstractVector, n::Integer)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    @inbounds v[begin:min(begin + n - 1, end)]
end

"""
    last(coll)

Get the last element of an ordered collection, if it can be computed in O(1) time. This is
accomplished by calling [`lastindex`](@ref) to get the last index. Return the end
point of an [`AbstractRange`](@ref) even if it is empty.

See also [`first`](@ref), [`endswith`](@ref).

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
    last(itr, n::Integer)

Get the last `n` elements of the iterable collection `itr`, or fewer elements if `itr` is not
long enough.

!!! compat "Julia 1.6"
    This method requires at least Julia 1.6.

# Examples
```jldoctest
julia> last(["foo", "bar", "qux"], 2)
2-element Vector{String}:
 "bar"
 "qux"

julia> last(1:6, 10)
1:6

julia> last(Float64[], 1)
Float64[]
```
"""
last(itr, n::Integer) = reverse!(collect(Iterators.take(Iterators.reverse(itr), n)))
# Faster method for arrays
function last(v::AbstractVector, n::Integer)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    @inbounds v[max(begin, end - n + 1):end]
end

"""
    strides(A)

Return a tuple of the memory strides in each dimension.

See also: [`stride`](@ref).

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

See also: [`strides`](@ref).

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
    ndims(A) == 0 && return 1
    sz = size(A)
    s = st[1] * sz[1]
    for i in 2:ndims(A)
        s += st[i] * sz[i]
    end
    return s
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

function isstored(A::AbstractArray{<:Any,N}, I::Vararg{Integer,N}) where {N}
    @boundscheck checkbounds(A, I...)
    return true
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
        s *= length(inds[i])
    end
    return s
end
# This version is type-stable even if inds is heterogeneous
function trailingsize(inds::Indices)
    @inline
    prod(map(length, inds))
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
    @inline
    checkbounds_indices(Bool, axes(A), I)
end

# Linear indexing is explicitly allowed when there is only one (non-cartesian) index
function checkbounds(::Type{Bool}, A::AbstractArray, i)
    @inline
    checkindex(Bool, eachindex(IndexLinear(), A), i)
end
# As a special extension, allow using logical arrays that match the source array exactly
function checkbounds(::Type{Bool}, A::AbstractArray{<:Any,N}, I::AbstractArray{Bool,N}) where N
    @inline
    axes(A) == axes(I)
end

"""
    checkbounds(A, I...)

Throw an error if the specified indices `I` are not in bounds for the given array `A`.
"""
function checkbounds(A::AbstractArray, I...)
    @inline
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
    @inline
    checkindex(Bool, IA[1], I[1])::Bool & checkbounds_indices(Bool, tail(IA), tail(I))
end
function checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple)
    @inline
    checkindex(Bool, OneTo(1), I[1])::Bool & checkbounds_indices(Bool, (), tail(I))
end
checkbounds_indices(::Type{Bool}, IA::Tuple, ::Tuple{}) = (@inline; all(x->length(x)==1, IA))
checkbounds_indices(::Type{Bool}, ::Tuple{}, ::Tuple{}) = true

throw_boundserror(A, I) = (@noinline; throw(BoundsError(A, I)))

# check along a single dimension
"""
    checkindex(Bool, inds::AbstractUnitRange, index)

Return `true` if the given `index` is within the bounds of
`inds`. Custom types that would like to behave as indices for all
arrays can extend this method in order to provide a specialized bounds
checking implementation.

See also [`checkbounds`](@ref).

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
    @inline
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
1×4 Matrix{Int64}:
 4419743872  4374413872  4419743888  0
```

Conversely, `similar(trues(10,10), 2)` returns an uninitialized `BitVector` with two
elements since `BitArray`s are both mutable and can support 1-dimensional arrays:

```julia-repl
julia> similar(trues(10,10), 2)
2-element BitVector:
 0
 0
```

Since `BitArray`s can only store elements of type [`Bool`](@ref), however, if you request a
different element type it will create a regular `Array` instead:

```julia-repl
julia> similar(falses(10), Float64, 2, 4)
2×4 Matrix{Float64}:
 2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
 2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
```

See also: [`undef`](@ref), [`isassigned`](@ref).
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
similar(a::AbstractArray, ::Type{T}, dims::Tuple{Integer, Vararg{Integer}}) where {T} = similar(a, T, to_shape(dims))
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
argument.

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

See also: [`empty!`](@ref), [`isempty`](@ref), [`isassigned`](@ref).

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

# This is `@Experimental.max_methods 1 function copyto! end`, which is not
# defined at this point in bootstrap.
typeof(function copyto! end).name.max_methods = UInt8(1)

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
    if haslength(src) && length(dest) > 0
        @boundscheck checkbounds(dest, i:(i + length(src) - 1))
        for x in src
            @inbounds dest[i] = x
            i += 1
        end
    else
        for x in src
            dest[i] = x
            i += 1
        end
    end
    return dest
end

# copy from an some iterable object into an AbstractArray
function copyto!(dest::AbstractArray, dstart::Integer, src, sstart::Integer)
    if (sstart < 1)
        throw(ArgumentError(LazyString("source start offset (",sstart,") is < 1")))
    end
    y = iterate(src)
    for j = 1:(sstart-1)
        if y === nothing
            throw(ArgumentError(LazyString(
                "source has fewer elements than required, ",
                "expected at least ", sstart,", got ", j-1)))
        end
        y = iterate(src, y[2])
    end
    if y === nothing
        throw(ArgumentError(LazyString(
            "source has fewer elements than required, ",
            "expected at least ",sstart," got ", sstart-1)))
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
    n < 0 && throw(ArgumentError(LazyString("tried to copy n=",n,
        ", elements, but n should be nonnegative")))
    n == 0 && return dest
    dmax = dstart + n - 1
    inds = LinearIndices(dest)
    if (dstart ∉ inds || dmax ∉ inds) | (sstart < 1)
        sstart < 1 && throw(ArgumentError(LazyString("source start offset (",
            sstart,") is < 1")))
        throw(BoundsError(dest, dstart:dmax))
    end
    y = iterate(src)
    for j = 1:(sstart-1)
        if y === nothing
            throw(ArgumentError(LazyString(
                "source has fewer elements than required, ",
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

See also [`copy!`](@ref Base.copy!), [`copy`](@ref).

# Examples
```jldoctest
julia> x = [1., 0., 3., 0., 5.];

julia> y = zeros(7);

julia> copyto!(y, x);

julia> y
7-element Vector{Float64}:
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
    n < 0 && throw(ArgumentError(LazyString("tried to copy n=",
        n," elements, but n should be nonnegative")))
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
        throw(ArgumentError(LazyString("source and destination must have same size (got ",
            length(ir_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(jr_src)
        throw(ArgumentError(LazyString("source and destination must have same size (got ",
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

function copyto_axcheck!(dest, src)
    @noinline checkaxs(axd, axs) = axd == axs || throw(DimensionMismatch("axes must agree, got $axd and $axs"))

    checkaxs(axes(dest), axes(src))
    copyto!(dest, src)
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
3-element Vector{Int64}:
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

zero(x::AbstractArray{T}) where {T} = fill!(similar(x, typeof(zero(T))), zero(T))

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
    @inline
    unsafe_convert(Ptr{T}, x) + Int(_memory_offset(x, i))::Int
end

# The distance from pointer(x) to the element at x[I...] in bytes
_memory_offset(x::DenseArray, I::Vararg{Any,N}) where {N} = (_to_linear_index(x, I...) - first(LinearIndices(x)))*elsize(x)
function _memory_offset(x::AbstractArray, I::Vararg{Any,N}) where {N}
    J = _to_subscript_indices(x, I...)
    return sum(map((i, s, o)->s*(i-o), J, strides(x), Tuple(first(CartesianIndices(x)))))*elsize(x)
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

Return a subset of array `A` as specified by `inds`, where each `ind` may be,
for example, an `Int`, an [`AbstractRange`](@ref), or a [`Vector`](@ref).
See the manual section on [array indexing](@ref man-array-indexing) for details.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> getindex(A, 1)
1

julia> getindex(A, [2, 1])
2-element Vector{Int64}:
 3
 1

julia> getindex(A, 2:4)
3-element Vector{Int64}:
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
# To avoid invalidations from multidimensional.jl: getindex(A::Array, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...)
@propagate_inbounds getindex(A::Array, i1::Integer, I::Integer...) = A[to_indices(A, (i1, I...))...]

function unsafe_getindex(A::AbstractArray, I...)
    @inline
    @inbounds r = getindex(A, I...)
    r
end

struct CanonicalIndexError
    func::String
    type::Any
    CanonicalIndexError(func::String, @nospecialize(type)) = new(func, type)
end

error_if_canonical_getindex(::IndexLinear, A::AbstractArray, ::Int) =
    throw(CanonicalIndexError("getindex", typeof(A)))
error_if_canonical_getindex(::IndexCartesian, A::AbstractArray{T,N}, ::Vararg{Int,N}) where {T,N} =
    throw(CanonicalIndexError("getindex", typeof(A)))
error_if_canonical_getindex(::IndexStyle, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_getindex(::IndexStyle, A::AbstractArray, I...) =
    error("getindex for $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing: canonical method is one Int
_getindex(::IndexLinear, A::AbstractVector, i::Int) = (@_propagate_inbounds_meta; getindex(A, i))  # ambiguity resolution in case packages specialize this (to be avoided if at all possible, but see Interpolations.jl)
_getindex(::IndexLinear, A::AbstractArray, i::Int) = (@_propagate_inbounds_meta; getindex(A, i))
function _getindex(::IndexLinear, A::AbstractArray, I::Vararg{Int,M}) where M
    @inline
    @boundscheck checkbounds(A, I...) # generally _to_linear_index requires bounds checking
    @inbounds r = getindex(A, _to_linear_index(A, I...))
    r
end
_to_linear_index(A::AbstractArray, i::Integer) = i
_to_linear_index(A::AbstractVector, i::Integer, I::Integer...) = i
_to_linear_index(A::AbstractArray) = first(LinearIndices(A))
_to_linear_index(A::AbstractArray, I::Integer...) = (@inline; _sub2ind(A, I...))

## IndexCartesian Scalar indexing: Canonical method is full dimensionality of Ints
function _getindex(::IndexCartesian, A::AbstractArray, I::Vararg{Int,M}) where M
    @inline
    @boundscheck checkbounds(A, I...) # generally _to_subscript_indices requires bounds checking
    @inbounds r = getindex(A, _to_subscript_indices(A, I...)...)
    r
end
function _getindex(::IndexCartesian, A::AbstractArray{T,N}, I::Vararg{Int, N}) where {T,N}
    @_propagate_inbounds_meta
    getindex(A, I...)
end
_to_subscript_indices(A::AbstractArray, i::Integer) = (@inline; _unsafe_ind2sub(A, i))
_to_subscript_indices(A::AbstractArray{T,N}) where {T,N} = (@inline; fill_to_length((), 1, Val(N)))
_to_subscript_indices(A::AbstractArray{T,0}) where {T} = ()
_to_subscript_indices(A::AbstractArray{T,0}, i::Integer) where {T} = ()
_to_subscript_indices(A::AbstractArray{T,0}, I::Integer...) where {T} = ()
function _to_subscript_indices(A::AbstractArray{T,N}, I::Integer...) where {T,N}
    @inline
    J, Jrem = IteratorsMD.split(I, Val(N))
    _to_subscript_indices(A, J, Jrem)
end
_to_subscript_indices(A::AbstractArray, J::Tuple, Jrem::Tuple{}) =
    __to_subscript_indices(A, axes(A), J, Jrem)
function __to_subscript_indices(A::AbstractArray,
        ::Tuple{AbstractUnitRange,Vararg{AbstractUnitRange}}, J::Tuple, Jrem::Tuple{})
    @inline
    (J..., map(first, tail(_remaining_size(J, axes(A))))...)
end
_to_subscript_indices(A, J::Tuple, Jrem::Tuple) = J # already bounds-checked, safe to drop
_to_subscript_indices(A::AbstractArray{T,N}, I::Vararg{Int,N}) where {T,N} = I
_remaining_size(::Tuple{Any}, t::Tuple) = t
_remaining_size(h::Tuple, t::Tuple) = (@inline; _remaining_size(tail(h), tail(t)))
_unsafe_ind2sub(::Tuple{}, i) = () # _ind2sub may throw(BoundsError()) in this case
_unsafe_ind2sub(sz, i) = (@inline; _ind2sub(sz, i))

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage

"""
    setindex!(A, X, inds...)
    A[inds...] = X

Store values from array `X` within some subset of `A` as specified by `inds`.
The syntax `A[inds...] = X` is equivalent to `(setindex!(A, X, inds...); X)`.

# Examples
```jldoctest
julia> A = zeros(2,2);

julia> setindex!(A, [10, 20], [1, 2]);

julia> A[[3, 4]] = [30, 40];

julia> A
2×2 Matrix{Float64}:
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
    @inline
    @inbounds r = setindex!(A, v, I...)
    r
end

error_if_canonical_setindex(::IndexLinear, A::AbstractArray, ::Int) =
    throw(CanonicalIndexError("setindex!", typeof(A)))
error_if_canonical_setindex(::IndexCartesian, A::AbstractArray{T,N}, ::Vararg{Int,N}) where {T,N} =
    throw(CanonicalIndexError("setindex!", typeof(A)))
error_if_canonical_setindex(::IndexStyle, ::AbstractArray, ::Any...) = nothing

## Internal definitions
_setindex!(::IndexStyle, A::AbstractArray, v, I...) =
    error("setindex! for $(typeof(A)) with types $(typeof(I)) is not supported")

## IndexLinear Scalar indexing
_setindex!(::IndexLinear, A::AbstractArray, v, i::Int) = (@_propagate_inbounds_meta; setindex!(A, v, i))
function _setindex!(::IndexLinear, A::AbstractArray, v, I::Vararg{Int,M}) where M
    @inline
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
    @inline
    @boundscheck checkbounds(A, I...)
    @inbounds r = setindex!(A, v, _to_subscript_indices(A, I...)...)
    r
end

"""
    parent(A)

Return the underlying "parent array”. This parent array of objects of types `SubArray`, `ReshapedArray`
or `LinearAlgebra.Transpose` is what was passed as an argument to `view`, `reshape`, `transpose`, etc.
during object creation. If the input is not a wrapped object, return the input itself. If the input is
wrapped multiple times, only the outermost wrapper will be removed.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> V = view(A, 1:2, :)
2×2 view(::Matrix{Int64}, 1:2, :) with eltype Int64:
 1  2
 3  4

julia> parent(V)
2×2 Matrix{Int64}:
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
unaliascopy(A::AbstractArray)::typeof(A) = (@noinline; _unaliascopy(A, copy(A)))
_unaliascopy(A::T, C::T) where {T} = C
_unaliascopy(A, C) = throw(ArgumentError("""
    an array of type `$(typename(typeof(A)).wrapper)` shares memory with another argument
    and must make a preventative copy of itself in order to maintain consistent semantics,
    but `copy(::$(typeof(A)))` returns a new array of type `$(typeof(C))`.
    To fix, implement:
        `Base.unaliascopy(A::$(typename(typeof(A)).wrapper))::typeof(A)`"""))
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
get(f::Callable, A::AbstractArray, i::Integer) = checkbounds(Bool, A, i) ? A[i] : f()
get(f::Callable, A::AbstractArray, I::Tuple{}) = checkbounds(Bool, A) ? A[] : f()
get(f::Callable, A::AbstractArray, I::Dims) = checkbounds(Bool, A, I...) ? A[I...] : f()

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
_cat(catdim::Int) = Vector{Any}()

typed_vcat(::Type{T}) where {T} = Vector{T}()
typed_hcat(::Type{T}) where {T} = Vector{T}()

## cat: special cases
vcat(X::T...) where {T}         = T[ X[i] for i=1:length(X) ]
vcat(X::T...) where {T<:Number} = T[ X[i] for i=1:length(X) ]
hcat(X::T...) where {T}         = T[ X[j] for i=1:1, j=1:length(X) ]
hcat(X::T...) where {T<:Number} = T[ X[j] for i=1:1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill!(Vector{promote_typeof(X...)}(undef, length(X)), X)
hcat(X::Number...) = hvcat_fill!(Matrix{promote_typeof(X...)}(undef, 1,length(X)), X)
typed_vcat(::Type{T}, X::Number...) where {T} = hvcat_fill!(Vector{T}(undef, length(X)), X)
typed_hcat(::Type{T}, X::Number...) where {T} = hvcat_fill!(Matrix{T}(undef, 1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat(V::AbstractVector{T}...) where {T} = typed_vcat(T, V...)

# FIXME: this alias would better be Union{AbstractVector{T}, Tuple{Vararg{T}}}
# and method signatures should do AbstractVecOrTuple{<:T} when they want covariance,
# but that solution currently fails (see #27188 and #27224)
AbstractVecOrTuple{T} = Union{AbstractVector{<:T}, Tuple{Vararg{T}}}

_typed_vcat_similar(V, ::Type{T}, n) where T = similar(V[1], T, n)
_typed_vcat(::Type{T}, V::AbstractVecOrTuple{AbstractVector}) where T =
    _typed_vcat!(_typed_vcat_similar(V, T, sum(map(length, V))), V)

function _typed_vcat!(a::AbstractVector{T}, V::AbstractVecOrTuple{AbstractVector}) where T
    pos = 1
    for k=1:Int(length(V))::Int
        Vk = V[k]
        p1 = pos + Int(length(Vk))::Int - 1
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
        p1 = pos+size(Ak,1)::Int-1
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

cat_length(::Any) = 1
cat_length(a::AbstractArray) = length(a)

cat_ndims(a) = 0
cat_ndims(a::AbstractArray) = ndims(a)

cat_indices(A, d) = OneTo(1)
cat_indices(A::AbstractArray, d) = axes(A, d)

cat_similar(A, ::Type{T}, shape::Tuple) where T = Array{T}(undef, shape)
cat_similar(A, ::Type{T}, shape::Vector) where T = Array{T}(undef, shape...)
cat_similar(A::Array, ::Type{T}, shape::Tuple) where T = Array{T}(undef, shape)
cat_similar(A::Array, ::Type{T}, shape::Vector) where T = Array{T}(undef, shape...)
cat_similar(A::AbstractArray, T::Type, shape::Tuple) = similar(A, T, shape)
cat_similar(A::AbstractArray, T::Type, shape::Vector) = similar(A, T, shape...)

# These are for backwards compatibility (even though internal)
cat_shape(dims, shape::Tuple{Vararg{Int}}) = shape
function cat_shape(dims, shapes::Tuple)
    out_shape = ()
    for s in shapes
        out_shape = _cshp(1, dims, out_shape, s)
    end
    return out_shape
end
# The new way to compute the shape (more inferrable than combining cat_size & cat_shape, due to Varargs + issue#36454)
cat_size_shape(dims) = ntuple(zero, Val(length(dims)))
@inline cat_size_shape(dims, X, tail...) = _cat_size_shape(dims, _cshp(1, dims, (), cat_size(X)), tail...)
_cat_size_shape(dims, shape) = shape
@inline _cat_size_shape(dims, shape, X, tail...) = _cat_size_shape(dims, _cshp(1, dims, shape, cat_size(X)), tail...)

_cshp(ndim::Int, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
_cshp(ndim::Int, ::Tuple{}, ::Tuple{}, nshape) = nshape
_cshp(ndim::Int, dims, ::Tuple{}, ::Tuple{}) = ntuple(Returns(1), Val(length(dims)))
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

dims2cat(::Val{dims}) where dims = dims2cat(dims)
function dims2cat(dims)
    if any(≤(0), dims)
        throw(ArgumentError("All cat dimensions must be positive integers, but got $dims"))
    end
    ntuple(in(dims), maximum(dims))
end

_cat(dims, X...) = _cat_t(dims, promote_eltypeof(X...), X...)

@inline function _cat_t(dims, ::Type{T}, X...) where {T}
    catdims = dims2cat(dims)
    shape = cat_size_shape(catdims, X...)
    A = cat_similar(X[1], T, shape)
    if count(!iszero, catdims)::Int > 1
        fill!(A, zero(T))
    end
    return __cat(A, shape, catdims, X...)
end
# this version of `cat_t` is not very kind for inference and so its usage should be avoided,
# nevertheless it is here just for compat after https://github.com/JuliaLang/julia/pull/45028
@inline cat_t(::Type{T}, X...; dims) where {T} = _cat_t(dims, T, X...)

# Why isn't this called `__cat!`?
__cat(A, shape, catdims, X...) = __cat_offset!(A, shape, catdims, ntuple(zero, length(shape)), X...)

function __cat_offset!(A, shape, catdims, offsets, x, X...)
    # splitting the "work" on x from X... may reduce latency (fewer costly specializations)
    newoffsets = __cat_offset1!(A, shape, catdims, offsets, x)
    return __cat_offset!(A, shape, catdims, newoffsets, X...)
end
__cat_offset!(A, shape, catdims, offsets) = A

function __cat_offset1!(A, shape, catdims, offsets, x)
    inds = ntuple(length(offsets)) do i
        (i <= length(catdims) && catdims[i]) ? offsets[i] .+ cat_indices(x, i) : 1:shape[i]
    end
    if x isa AbstractArray
        A[inds...] = x
    else
        fill!(view(A, inds...), x)
    end
    newoffsets = ntuple(length(offsets)) do i
        (i <= length(catdims) && catdims[i]) ? offsets[i] + cat_size(x, i) : offsets[i]
    end
    return newoffsets
end

"""
    vcat(A...)

Concatenate along dimension 1. To efficiently concatenate a large vector of arrays,
use `reduce(vcat, x)`.

# Examples
```jldoctest
julia> a = [1 2 3 4 5]
1×5 Matrix{Int64}:
 1  2  3  4  5

julia> b = [6 7 8 9 10; 11 12 13 14 15]
2×5 Matrix{Int64}:
  6   7   8   9  10
 11  12  13  14  15

julia> vcat(a,b)
3×5 Matrix{Int64}:
  1   2   3   4   5
  6   7   8   9  10
 11  12  13  14  15

julia> c = ([1 2 3], [4 5 6])
([1 2 3], [4 5 6])

julia> vcat(c...)
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> vs = [[1, 2], [3, 4], [5, 6]]
3-element Vector{Vector{Int64}}:
 [1, 2]
 [3, 4]
 [5, 6]

julia> reduce(vcat, vs)
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6
```
"""
vcat(X...) = cat(X...; dims=Val(1))
"""
    hcat(A...)

Concatenate along dimension 2. To efficiently concatenate a large vector of arrays,
use `reduce(hcat, x)`.

# Examples
```jldoctest
julia> a = [1; 2; 3; 4; 5]
5-element Vector{Int64}:
 1
 2
 3
 4
 5

julia> b = [6 7; 8 9; 10 11; 12 13; 14 15]
5×2 Matrix{Int64}:
  6   7
  8   9
 10  11
 12  13
 14  15

julia> hcat(a,b)
5×3 Matrix{Int64}:
 1   6   7
 2   8   9
 3  10  11
 4  12  13
 5  14  15

julia> c = ([1; 2; 3], [4; 5; 6])
([1, 2, 3], [4, 5, 6])

julia> hcat(c...)
3×2 Matrix{Int64}:
 1  4
 2  5
 3  6

julia> x = Matrix(undef, 3, 0)  # x = [] would have created an Array{Any, 1}, but need an Array{Any, 2}
3×0 Matrix{Any}

julia> hcat(x, [1; 2; 3])
3×1 Matrix{Any}:
 1
 2
 3

julia> vs = [[1, 2], [3, 4], [5, 6]]
3-element Vector{Vector{Int64}}:
 [1, 2]
 [3, 4]
 [5, 6]

julia> reduce(hcat, vs)
2×3 Matrix{Int64}:
 1  3  5
 2  4  6
```
"""
hcat(X...) = cat(X...; dims=Val(2))

typed_vcat(::Type{T}, X...) where T = _cat_t(Val(1), T, X...)
typed_hcat(::Type{T}, X...) where T = _cat_t(Val(2), T, X...)

"""
    cat(A...; dims)

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

See also [`hcat`](@ref), [`vcat`](@ref), [`hvcat`](@ref), [`repeat`](@ref).

# Examples
```jldoctest
julia> cat([1 2; 3 4], [pi, pi], fill(10, 2,3,1); dims=2)
2×6×1 Array{Float64, 3}:
[:, :, 1] =
 1.0  2.0  3.14159  10.0  10.0  10.0
 3.0  4.0  3.14159  10.0  10.0  10.0

julia> cat(true, trues(2,2), trues(4)', dims=(1,2))
4×7 Matrix{Bool}:
 1  0  0  0  0  0  0
 0  1  1  0  0  0  0
 0  1  1  0  0  0  0
 0  0  0  1  1  1  1
```
"""
@inline cat(A...; dims) = _cat(dims, A...)
# `@constprop :aggressive` allows `catdims` to be propagated as constant improving return type inference
@constprop :aggressive _cat(catdims, A::AbstractArray{T}...) where {T} = _cat_t(catdims, T, A...)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(A; dims=Val(1))
vcat(A::AbstractArray, B::AbstractArray) = cat(A, B; dims=Val(1))
vcat(A::AbstractArray...) = cat(A...; dims=Val(1))
hcat(A::AbstractArray) = cat(A; dims=Val(2))
hcat(A::AbstractArray, B::AbstractArray) = cat(A, B; dims=Val(2))
hcat(A::AbstractArray...) = cat(A...; dims=Val(2))

typed_vcat(T::Type, A::AbstractArray) = _cat_t(Val(1), T, A)
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = _cat_t(Val(1), T, A, B)
typed_vcat(T::Type, A::AbstractArray...) = _cat_t(Val(1), T, A...)
typed_hcat(T::Type, A::AbstractArray) = _cat_t(Val(2), T, A)
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = _cat_t(Val(2), T, A, B)
typed_hcat(T::Type, A::AbstractArray...) = _cat_t(Val(2), T, A...)

# 2d horizontal and vertical concatenation

# these are produced in lowering if splatting occurs inside hvcat
hvcat_rows(rows::Tuple...) = hvcat(map(length, rows), (rows...)...)
typed_hvcat_rows(T::Type, rows::Tuple...) = typed_hvcat(T, map(length, rows), (rows...)...)

function hvcat(nbc::Integer, as...)
    # nbc = # of block columns
    n = length(as)
    mod(n,nbc) != 0 &&
        throw(ArgumentError("number of arrays $n is not a multiple of the requested number of block columns $nbc"))
    nbr = div(n,nbc)
    hvcat(ntuple(Returns(nbc), nbr), as...)
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
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> hvcat((3,3), a,b,c,d,e,f)
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> [a b; c d; e f]
3×2 Matrix{Int64}:
 1  2
 3  4
 5  6

julia> hvcat((2,2,2), a,b,c,d,e,f)
3×2 Matrix{Int64}:
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

function hvcat_fill!(a::Array, xs::Tuple)
    nr, nc = size(a,1), size(a,2)
    len = length(xs)
    if nr*nc != len
        throw(ArgumentError("argument count $(len) does not match specified shape $((nr,nc))"))
    end
    k = 1
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
    hvcat_fill!(Matrix{T}(undef, nr, nc), xs)
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

## N-dimensional concatenation ##

"""
    hvncat(dim::Int, row_first, values...)
    hvncat(dims::Tuple{Vararg{Int}}, row_first, values...)
    hvncat(shape::Tuple{Vararg{Tuple}}, row_first, values...)

Horizontal, vertical, and n-dimensional concatenation of many `values` in one call.

This function is called for block matrix syntax. The first argument either specifies the
shape of the concatenation, similar to `hvcat`, as a tuple of tuples, or the dimensions that
specify the key number of elements along each axis, and is used to determine the output
dimensions. The `dims` form is more performant, and is used by default when the concatenation
operation has the same number of elements along each axis (e.g., [a b; c d;;; e f ; g h]).
The `shape` form is used when the number of elements along each axis is unbalanced
(e.g., [a b ; c]). Unbalanced syntax needs additional validation overhead. The `dim` form
is an optimization for concatenation along just one dimension. `row_first` indicates how
`values` are ordered. The meaning of the first and second elements of `shape` are also
swapped based on `row_first`.

# Examples
```jldoctest
julia> a, b, c, d, e, f = 1, 2, 3, 4, 5, 6
(1, 2, 3, 4, 5, 6)

julia> [a b c;;; d e f]
1×3×2 Array{Int64, 3}:
[:, :, 1] =
 1  2  3

[:, :, 2] =
 4  5  6

julia> hvncat((2,1,3), false, a,b,c,d,e,f)
2×1×3 Array{Int64, 3}:
[:, :, 1] =
 1
 2

[:, :, 2] =
 3
 4

[:, :, 3] =
 5
 6

julia> [a b;;; c d;;; e f]
1×2×3 Array{Int64, 3}:
[:, :, 1] =
 1  2

[:, :, 2] =
 3  4

[:, :, 3] =
 5  6

julia> hvncat(((3, 3), (3, 3), (6,)), true, a, b, c, d, e, f)
1×3×2 Array{Int64, 3}:
[:, :, 1] =
 1  2  3

[:, :, 2] =
 4  5  6
```


# Examples for construction of the arguments:
```julia
[a b c ; d e f ;;;
 g h i ; j k l ;;;
 m n o ; p q r ;;;
 s t u ; v w x]
=> dims = (2, 3, 4)

[a b ; c ;;; d ;;;;]
 ___   _     _
 2     1     1 = elements in each row (2, 1, 1)
 _______     _
 3           1 = elements in each column (3, 1)
 _____________
 4             = elements in each 3d slice (4,)
 _____________
 4             = elements in each 4d slice (4,)
 => shape = ((2, 1, 1), (3, 1), (4,), (4,)) with `rowfirst` = true
```
"""
hvncat(dimsshape::Tuple, row_first::Bool, xs...) = _hvncat(dimsshape, row_first, xs...)
hvncat(dim::Int, xs...) = _hvncat(dim, true, xs...)

_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool) = _typed_hvncat(Any, dimsshape, row_first)
_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool, xs...) = _typed_hvncat(promote_eltypeof(xs...), dimsshape, row_first, xs...)
_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool, xs::T...) where T<:Number = _typed_hvncat(T, dimsshape, row_first, xs...)
_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool, xs::Number...) = _typed_hvncat(promote_typeof(xs...), dimsshape, row_first, xs...)
_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool, xs::AbstractArray...) = _typed_hvncat(promote_eltype(xs...), dimsshape, row_first, xs...)
_hvncat(dimsshape::Union{Tuple, Int}, row_first::Bool, xs::AbstractArray{T}...) where T = _typed_hvncat(T, dimsshape, row_first, xs...)


typed_hvncat(T::Type, dimsshape::Tuple, row_first::Bool, xs...) = _typed_hvncat(T, dimsshape, row_first, xs...)
typed_hvncat(T::Type, dim::Int, xs...) = _typed_hvncat(T, Val(dim), xs...)

# 1-dimensional hvncat methods

_typed_hvncat(::Type, ::Val{0}) = _typed_hvncat_0d_only_one()
_typed_hvncat(T::Type, ::Val{0}, x) = fill(convert(T, x))
_typed_hvncat(T::Type, ::Val{0}, x::Number) = fill(convert(T, x))
_typed_hvncat(T::Type, ::Val{0}, x::AbstractArray) = convert.(T, x)
_typed_hvncat(::Type, ::Val{0}, ::Any...) = _typed_hvncat_0d_only_one()
_typed_hvncat(::Type, ::Val{0}, ::Number...) = _typed_hvncat_0d_only_one()
_typed_hvncat(::Type, ::Val{0}, ::AbstractArray...) = _typed_hvncat_0d_only_one()

_typed_hvncat_0d_only_one() =
    throw(ArgumentError("a 0-dimensional array may only contain exactly one element"))

_typed_hvncat(T::Type, dim::Int, ::Bool, xs...) = _typed_hvncat(T, Val(dim), xs...) # catches from _hvncat type promoters

function _typed_hvncat(::Type{T}, ::Val{N}) where {T, N}
    N < 0 &&
        throw(ArgumentError("concatenation dimension must be nonnegative"))
    return Array{T, N}(undef, ntuple(x -> 0, Val(N)))
end

function _typed_hvncat(T::Type, ::Val{N}, xs::Number...) where N
    N < 0 &&
        throw(ArgumentError("concatenation dimension must be nonnegative"))
    A = cat_similar(xs[1], T, (ntuple(x -> 1, Val(N - 1))..., length(xs)))
    hvncat_fill!(A, false, xs)
    return A
end

function _typed_hvncat(::Type{T}, ::Val{N}, as::AbstractArray...) where {T, N}
    # optimization for arrays that can be concatenated by copying them linearly into the destination
    # conditions: the elements must all have 1-length dimensions above N
    length(as) > 0 ||
        throw(ArgumentError("must have at least one element"))
    N < 0 &&
        throw(ArgumentError("concatenation dimension must be nonnegative"))
    for a ∈ as
        ndims(a) <= N || all(x -> size(a, x) == 1, (N + 1):ndims(a)) ||
            return _typed_hvncat(T, (ntuple(x -> 1, Val(N - 1))..., length(as), 1), false, as...)
            # the extra 1 is to avoid an infinite cycle
    end

    nd = N

    Ndim = 0
    for i ∈ eachindex(as)
        Ndim += cat_size(as[i], N)
        nd = max(nd, cat_ndims(as[i]))
        for d ∈ 1:N - 1
            cat_size(as[1], d) == cat_size(as[i], d) || throw(ArgumentError("mismatched size along axis $d in element $i"))
        end
    end

    A = cat_similar(as[1], T, (ntuple(d -> size(as[1], d), N - 1)..., Ndim, ntuple(x -> 1, nd - N)...))
    k = 1
    for a ∈ as
        for i ∈ eachindex(a)
            A[k] = a[i]
            k += 1
        end
    end
    return A
end

function _typed_hvncat(::Type{T}, ::Val{N}, as...) where {T, N}
    length(as) > 0 ||
        throw(ArgumentError("must have at least one element"))
    N < 0 &&
        throw(ArgumentError("concatenation dimension must be nonnegative"))
    nd = N
    Ndim = 0
    for i ∈ eachindex(as)
        Ndim += cat_size(as[i], N)
        nd = max(nd, cat_ndims(as[i]))
        for d ∈ 1:N-1
            cat_size(as[i], d) == 1 ||
                throw(ArgumentError("all dimensions of element $i other than $N must be of length 1"))
        end
    end

    A = Array{T, nd}(undef, ntuple(x -> 1, Val(N - 1))..., Ndim, ntuple(x -> 1, nd - N)...)

    k = 1
    for a ∈ as
        if a isa AbstractArray
            lena = length(a)
            copyto!(A, k, a, 1, lena)
            k += lena
        else
            A[k] = a
            k += 1
        end
    end
    return A
end

# 0-dimensional cases for balanced and unbalanced hvncat method

_typed_hvncat(T::Type, ::Tuple{}, ::Bool, x...) = _typed_hvncat(T, Val(0), x...)
_typed_hvncat(T::Type, ::Tuple{}, ::Bool, x::Number...) = _typed_hvncat(T, Val(0), x...)


# balanced dimensions hvncat methods

_typed_hvncat(T::Type, dims::Tuple{Int}, ::Bool, as...) = _typed_hvncat_1d(T, dims[1], Val(false), as...)
_typed_hvncat(T::Type, dims::Tuple{Int}, ::Bool, as::Number...) = _typed_hvncat_1d(T, dims[1], Val(false), as...)

function _typed_hvncat_1d(::Type{T}, ds::Int, ::Val{row_first}, as...) where {T, row_first}
    lengthas = length(as)
    ds > 0 ||
        throw(ArgumentError("`dimsshape` argument must consist of positive integers"))
    lengthas == ds ||
        throw(ArgumentError("number of elements does not match `dimshape` argument; expected $ds, got $lengthas"))
    if row_first
        return _typed_hvncat(T, Val(2), as...)
    else
        return _typed_hvncat(T, Val(1), as...)
    end
end

function _typed_hvncat(::Type{T}, dims::NTuple{N, Int}, row_first::Bool, xs::Number...) where {T, N}
    all(>(0), dims) ||
        throw(ArgumentError("`dims` argument must contain positive integers"))
    A = Array{T, N}(undef, dims...)
    lengtha = length(A)  # Necessary to store result because throw blocks are being deoptimized right now, which leads to excessive allocations
    lengthx = length(xs) # Cuts from 3 allocations to 1.
    if lengtha != lengthx
       throw(ArgumentError("argument count does not match specified shape (expected $lengtha, got $lengthx)"))
    end
    hvncat_fill!(A, row_first, xs)
    return A
end

function hvncat_fill!(A::Array, row_first::Bool, xs::Tuple)
    # putting these in separate functions leads to unnecessary allocations
    if row_first
        nr, nc = size(A, 1), size(A, 2)
        nrc = nr * nc
        na = prod(size(A)[3:end])
        k = 1
        for d ∈ 1:na
            dd = nrc * (d - 1)
            for i ∈ 1:nr
                Ai = dd + i
                for j ∈ 1:nc
                    A[Ai] = xs[k]
                    k += 1
                    Ai += nr
                end
            end
        end
    else
        for k ∈ eachindex(xs)
            A[k] = xs[k]
        end
    end
end

function _typed_hvncat(T::Type, dims::NTuple{N, Int}, row_first::Bool, as...) where {N}
    # function barrier after calculating the max is necessary for high performance
    nd = max(maximum(cat_ndims(a) for a ∈ as), N)
    return _typed_hvncat_dims(T, (dims..., ntuple(x -> 1, nd - N)...), row_first, as)
end

function _typed_hvncat_dims(::Type{T}, dims::NTuple{N, Int}, row_first::Bool, as::Tuple) where {T, N}
    length(as) > 0 ||
        throw(ArgumentError("must have at least one element"))
    all(>(0), dims) ||
        throw(ArgumentError("`dims` argument must contain positive integers"))

    d1 = row_first ? 2 : 1
    d2 = row_first ? 1 : 2

    outdims = zeros(Int, N)

    # validate shapes for lowest level of concatenation
    d = findfirst(>(1), dims)
    if d !== nothing # all dims are 1
        if row_first && d < 3
            d = d == 1 ? 2 : 1
        end
        nblocks = length(as) ÷ dims[d]
        for b ∈ 1:nblocks
            offset = ((b - 1) * dims[d])
            startelementi = offset + 1
            for i ∈ offset .+ (2:dims[d])
                for dd ∈ 1:N
                    dd == d && continue
                    if cat_size(as[startelementi], dd) != cat_size(as[i], dd)
                        throw(ArgumentError("incompatible shape in element $i"))
                    end
                end
            end
        end
    end

    # discover number of rows or columns
    for i ∈ 1:dims[d1]
        outdims[d1] += cat_size(as[i], d1)
    end

    currentdims = zeros(Int, N)
    blockcount = 0
    elementcount = 0
    for i ∈ eachindex(as)
        elementcount += cat_length(as[i])
        currentdims[d1] += cat_size(as[i], d1)
        if currentdims[d1] == outdims[d1]
            currentdims[d1] = 0
            for d ∈ (d2, 3:N...)
                currentdims[d] += cat_size(as[i], d)
                if outdims[d] == 0 # unfixed dimension
                    blockcount += 1
                    if blockcount == dims[d]
                        outdims[d] = currentdims[d]
                        currentdims[d] = 0
                        blockcount = 0
                    else
                        break
                    end
                else # fixed dimension
                    if currentdims[d] == outdims[d] # end of dimension
                        currentdims[d] = 0
                    elseif currentdims[d] < outdims[d] # dimension in progress
                        break
                    else # exceeded dimension
                        throw(ArgumentError("argument $i has too many elements along axis $d"))
                    end
                end
            end
        elseif currentdims[d1] > outdims[d1] # exceeded dimension
            throw(ArgumentError("argument $i has too many elements along axis $d1"))
        end
    end

    outlen = prod(outdims)
    elementcount == outlen ||
        throw(ArgumentError("mismatched number of elements; expected $(outlen), got $(elementcount)"))

    # copy into final array
    A = cat_similar(as[1], T, outdims)
    # @assert all(==(0), currentdims)
    outdims .= 0
    hvncat_fill!(A, currentdims, outdims, d1, d2, as)
    return A
end


# unbalanced dimensions hvncat methods

function _typed_hvncat(T::Type, shape::Tuple{Tuple}, row_first::Bool, xs...)
    length(shape[1]) > 0 ||
        throw(ArgumentError("each level of `shape` argument must have at least one value"))
    return _typed_hvncat_1d(T, shape[1][1], Val(row_first), xs...)
end

function _typed_hvncat(T::Type, shape::NTuple{N, Tuple}, row_first::Bool, as...) where {N}
    # function barrier after calculating the max is necessary for high performance
    nd = max(maximum(cat_ndims(a) for a ∈ as), N)
    return _typed_hvncat_shape(T, (shape..., ntuple(x -> shape[end], nd - N)...), row_first, as)
end

function _typed_hvncat_shape(::Type{T}, shape::NTuple{N, Tuple}, row_first, as::Tuple) where {T, N}
    length(as) > 0 ||
        throw(ArgumentError("must have at least one element"))
    all(>(0), tuple((shape...)...)) ||
        throw(ArgumentError("`shape` argument must consist of positive integers"))

    d1 = row_first ? 2 : 1
    d2 = row_first ? 1 : 2

    shapev = collect(shape) # saves allocations later
    all(!isempty, shapev) ||
        throw(ArgumentError("each level of `shape` argument must have at least one value"))
    length(shapev[end]) == 1 ||
        throw(ArgumentError("last level of shape must contain only one integer"))
    shapelength = shapev[end][1]
    lengthas = length(as)
    shapelength == lengthas || throw(ArgumentError("number of elements does not match shape; expected $(shapelength), got $lengthas)"))
    # discover dimensions
    nd = max(N, cat_ndims(as[1]))
    outdims = fill(-1, nd)
    currentdims = zeros(Int, nd)
    blockcounts = zeros(Int, nd)
    shapepos = ones(Int, nd)

    elementcount = 0
    for i ∈ eachindex(as)
        elementcount += cat_length(as[i])
        wasstartblock = false
        for d ∈ 1:N
            ad = (d < 3 && row_first) ? (d == 1 ? 2 : 1) : d
            dsize = cat_size(as[i], ad)
            blockcounts[d] += 1

            if d == 1 || i == 1 || wasstartblock
                currentdims[d] += dsize
            elseif dsize != cat_size(as[i - 1], ad)
                throw(ArgumentError("argument $i has a mismatched number of elements along axis $ad; \
                                    expected $(cat_size(as[i - 1], ad)), got $dsize"))
            end

            wasstartblock = blockcounts[d] == 1 # remember for next dimension

            isendblock = blockcounts[d] == shapev[d][shapepos[d]]
            if isendblock
                if outdims[d] == -1
                    outdims[d] = currentdims[d]
                elseif outdims[d] != currentdims[d]
                    throw(ArgumentError("argument $i has a mismatched number of elements along axis $ad; \
                                        expected $(abs(outdims[d] - (currentdims[d] - dsize))), got $dsize"))
                end
                currentdims[d] = 0
                blockcounts[d] = 0
                shapepos[d] += 1
                d > 1 && (blockcounts[d - 1] == 0 ||
                    throw(ArgumentError("shape in level $d is inconsistent; level counts must nest \
                                        evenly into each other")))
            end
        end
    end

    outlen = prod(outdims)
    elementcount == outlen ||
        throw(ArgumentError("mismatched number of elements; expected $(outlen), got $(elementcount)"))

    if row_first
        outdims[1], outdims[2] = outdims[2], outdims[1]
    end

    # @assert all(==(0), currentdims)
    # @assert all(==(0), blockcounts)

    # copy into final array
    A = cat_similar(as[1], T, outdims)
    hvncat_fill!(A, currentdims, blockcounts, d1, d2, as)
    return A
end

function hvncat_fill!(A::AbstractArray{T, N}, scratch1::Vector{Int}, scratch2::Vector{Int}, d1::Int, d2::Int, as::Tuple{Vararg}) where {T, N}
    outdims = size(A)
    offsets = scratch1
    inneroffsets = scratch2
    for a ∈ as
        if isa(a, AbstractArray)
            for ai ∈ a
                Ai = hvncat_calcindex(offsets, inneroffsets, outdims, N)
                A[Ai] = ai

                for j ∈ 1:N
                    inneroffsets[j] += 1
                    inneroffsets[j] < cat_size(a, j) && break
                    inneroffsets[j] = 0
                end
            end
        else
            Ai = hvncat_calcindex(offsets, inneroffsets, outdims, N)
            A[Ai] = a
        end

        for j ∈ (d1, d2, 3:N...)
            offsets[j] += cat_size(a, j)
            offsets[j] < outdims[j] && break
            offsets[j] = 0
        end
    end
end

@propagate_inbounds function hvncat_calcindex(offsets::Vector{Int}, inneroffsets::Vector{Int},
                                              outdims::Tuple{Vararg{Int}}, nd::Int)
    Ai = inneroffsets[1] + offsets[1] + 1
    for j ∈ 2:nd
        increment = inneroffsets[j] + offsets[j]
        for k ∈ 1:j-1
            increment *= outdims[k]
        end
        Ai += increment
    end
    Ai
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

"""
    isless(A::AbstractVector, B::AbstractVector)

Returns true when `A` is less than `B` in lexicographic order.
"""
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
    @inline
    _sub2ind(axes(A), I...)
end

function _ind2sub(A::AbstractArray, ind)
    @inline
    _ind2sub(axes(A), ind)
end

# 0-dimensional arrays and indexing with []
_sub2ind(::Tuple{}) = 1
_sub2ind(::DimsInteger) = 1
_sub2ind(::Indices) = 1
_sub2ind(::Tuple{}, I::Integer...) = (@inline; _sub2ind_recurse((), 1, 1, I...))

# Generic cases
_sub2ind(dims::DimsInteger, I::Integer...) = (@inline; _sub2ind_recurse(dims, 1, 1, I...))
_sub2ind(inds::Indices, I::Integer...) = (@inline; _sub2ind_recurse(inds, 1, 1, I...))
# In 1d, there's a question of whether we're doing cartesian indexing
# or linear indexing. Support only the former.
_sub2ind(inds::Indices{1}, I::Integer...) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
_sub2ind(inds::Tuple{OneTo}, I::Integer...) = (@inline; _sub2ind_recurse(inds, 1, 1, I...)) # only OneTo is safe
_sub2ind(inds::Tuple{OneTo}, i::Integer)    = i

_sub2ind_recurse(::Any, L, ind) = ind
function _sub2ind_recurse(::Tuple{}, L, ind, i::Integer, I::Integer...)
    @inline
    _sub2ind_recurse((), L, ind+(i-1)*L, I...)
end
function _sub2ind_recurse(inds, L, ind, i::Integer, I::Integer...)
    @inline
    r1 = inds[1]
    _sub2ind_recurse(tail(inds), nextL(L, r1), ind+offsetin(i, r1)*L, I...)
end

nextL(L, l::Integer) = L*l
nextL(L, r::AbstractUnitRange) = L*length(r)
nextL(L, r::Slice) = L*length(r.indices)
offsetin(i, l::Integer) = i-1
offsetin(i, r::AbstractUnitRange) = i-first(r)

_ind2sub(::Tuple{}, ind::Integer) = (@inline; ind == 1 ? () : throw(BoundsError()))
_ind2sub(dims::DimsInteger, ind::Integer) = (@inline; _ind2sub_recurse(dims, ind-1))
_ind2sub(inds::Indices, ind::Integer)     = (@inline; _ind2sub_recurse(inds, ind-1))
_ind2sub(inds::Indices{1}, ind::Integer) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
_ind2sub(inds::Tuple{OneTo}, ind::Integer) = (ind,)

_ind2sub_recurse(::Tuple{}, ind) = (ind+1,)
function _ind2sub_recurse(indslast::NTuple{1}, ind)
    @inline
    (_lookup(ind, indslast[1]),)
end
function _ind2sub_recurse(inds, ind)
    @inline
    r1 = inds[1]
    indnext, f, l = _div(ind, r1)
    (ind-l*indnext+f, _ind2sub_recurse(tail(inds), indnext)...)
end

_lookup(ind, d::Integer) = ind+1
_lookup(ind, r::AbstractUnitRange) = ind+first(r)
_div(ind, d::Integer) = div(ind, d), 1, d
_div(ind, r::AbstractUnitRange) = (d = length(r); (div(ind, d), first(r), d))

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
    @noinline
    for i in Iinds
        # Iout[i] = _sub2ind(inds, map(Ij -> Ij[i], I)...)
        Iout[i] = sub2ind_vec(inds, i, I)
    end
    Iout
end

sub2ind_vec(inds, i, I) = (@inline; _sub2ind(inds, _sub2ind_vec(i, I...)...))
_sub2ind_vec(i, I1, I...) = (@inline; (I1[i], _sub2ind_vec(i, I...)...))
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
For multiple iterable arguments, `f` is called elementwise, and iteration stops when
any iterator is finished.

`foreach` should be used instead of [`map`](@ref) when the results of `f` are not
needed, for example in `foreach(println, array)`.

# Examples
```jldoctest
julia> tri = 1:3:7; res = Int[];

julia> foreach(x -> push!(res, x^2), tri)

julia> res
3-element Vector{$(Int)}:
  1
 16
 49

julia> foreach((x, y) -> println(x, " with ", y), tri, 'a':'z')
1 with a
4 with b
7 with c
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

Transform the given dimensions of array `A` by applying a function `f` on each slice
of the form `A[..., :, ..., :, ...]`, with a colon at each `d` in `dims`. The results are
concatenated along the remaining dimensions.

For example, if `dims = [1,2]` and `A` is 4-dimensional, then `f` is called on `x = A[:,:,i,j]`
for all `i` and `j`, and `f(x)` becomes `R[:,:,i,j]` in the result `R`.

See also [`eachcol`](@ref), [`eachslice`](@ref), [`mapreduce`](@ref).

# Examples
```jldoctest
julia> A = reshape(1:30,(2,5,3))
2×5×3 reshape(::UnitRange{$Int}, 2, 5, 3) with eltype $Int:
[:, :, 1] =
 1  3  5  7   9
 2  4  6  8  10

[:, :, 2] =
 11  13  15  17  19
 12  14  16  18  20

[:, :, 3] =
 21  23  25  27  29
 22  24  26  28  30

julia> f(x::Matrix) = fill(x[1,1], 1,4);  # returns a 1×4 matrix

julia> mapslices(f, A, dims=(1,2))
1×4×3 Array{$Int, 3}:
[:, :, 1] =
 1  1  1  1

[:, :, 2] =
 11  11  11  11

[:, :, 3] =
 21  21  21  21

julia> g(x) = x[begin] // x[end-1];  # returns a number

julia> mapslices(g, A, dims=[1,3])
1×5×1 Array{Rational{$Int}, 3}:
[:, :, 1] =
 1//21  3//23  1//5  7//27  9//29

julia> map(g, eachslice(A, dims=2))
5-element Vector{Rational{$Int}}:
 1//21
 3//23
 1//5
 7//27
 9//29

julia> mapslices(sum, A; dims=(1,3)) == sum(A; dims=(1,3))
true
```

Notice that in `eachslice(A; dims=2)`, the specified dimension is the
one *without* a colon in the slice. This is `view(A,:,i,:)`, whereas
`mapslices(f, A; dims=(1,3))` uses `A[:,i,:]`. The function `f` may mutate
values in the slice without affecting `A`.
"""
function mapslices(f, A::AbstractArray; dims)
    isempty(dims) && return map(f, A)

    for d in dims
        d isa Integer || throw(ArgumentError("mapslices: dimension must be an integer, got $d"))
        d >= 1 || throw(ArgumentError("mapslices: dimension must be ≥ 1, got $d"))
        # Indexing a matrix M[:,1,:] produces a 1-column matrix, but dims=(1,3) here
        # would otherwise ignore 3, and slice M[:,i]. Previously this gave error:
        # BoundsError: attempt to access 2-element Vector{Any} at index [3]
        d > ndims(A) && throw(ArgumentError("mapslices does not accept dimensions > ndims(A) = $(ndims(A)), got $d"))
    end
    dim_mask = ntuple(d -> d in dims, ndims(A))

    # Apply the function to the first slice in order to determine the next steps
    idx1 = ntuple(d -> d in dims ? (:) : firstindex(A,d), ndims(A))
    Aslice = A[idx1...]
    r1 = f(Aslice)

    res1 = if r1 isa AbstractArray && ndims(r1) > 0
        n = sum(dim_mask)
        if ndims(r1) > n && any(ntuple(d -> size(r1,d+n)>1, ndims(r1)-n))
            s = size(r1)[1:n]
            throw(DimensionMismatch("mapslices cannot assign slice f(x) of size $(size(r1)) into output of size $s"))
        end
        r1
    else
        # If the result of f on a single slice is a scalar then we add singleton
        # dimensions. When adding the dimensions, we have to respect the
        # index type of the input array (e.g. in the case of OffsetArrays)
        _res1 = similar(Aslice, typeof(r1), reduced_indices(Aslice, 1:ndims(Aslice)))
        _res1[begin] = r1
        _res1
    end

    # Determine result size and allocate. We always pad ndims(res1) out to length(dims):
    din = Ref(0)
    Rsize = ntuple(ndims(A)) do d
        if d in dims
            axes(res1, din[] += 1)
        else
            axes(A,d)
        end
    end
    R = similar(res1, Rsize)

    # Determine iteration space. It will be convenient in the loop to mask N-dimensional
    # CartesianIndices, with some trivial dimensions:
    itershape = ntuple(d -> d in dims ? Base.OneTo(1) : axes(A,d), ndims(A))
    indices = Iterators.drop(CartesianIndices(itershape), 1)

    # That skips the first element, which we already have:
    ridx = ntuple(d -> d in dims ? Slice(axes(R,d)) : firstindex(A,d), ndims(A))
    concatenate_setindex!(R, res1, ridx...)

    # In some cases, we can re-use the first slice for a dramatic performance
    # increase. The slice itself must be mutable and the result cannot contain
    # any mutable containers. The following errs on the side of being overly
    # strict (#18570 & #21123).
    safe_for_reuse = isa(Aslice, StridedArray) &&
                     (isa(r1, Number) || (isa(r1, AbstractArray) && eltype(r1) <: Number))

    _inner_mapslices!(R, indices, f, A, dim_mask, Aslice, safe_for_reuse)
    return R
end

@noinline function _inner_mapslices!(R, indices, f, A, dim_mask, Aslice, safe_for_reuse)
    must_extend = any(dim_mask .& size(R) .> 1)
    if safe_for_reuse
        # when f returns an array, R[ridx...] = f(Aslice) line copies elements,
        # so we can reuse Aslice
        for I in indices
            idx = ifelse.(dim_mask, Slice.(axes(A)), Tuple(I))
            _unsafe_getindex!(Aslice, A, idx...)
            r = f(Aslice)
            if r isa AbstractArray || must_extend
                ridx = ifelse.(dim_mask, Slice.(axes(R)), Tuple(I))
                R[ridx...] = r
            else
                ridx = ifelse.(dim_mask, first.(axes(R)), Tuple(I))
                R[ridx...] = r
            end
        end
    else
        # we can't guarantee safety (#18524), so allocate new storage for each slice
        for I in indices
            idx = ifelse.(dim_mask, Slice.(axes(A)), Tuple(I))
            ridx = ifelse.(dim_mask, Slice.(axes(R)), Tuple(I))
            concatenate_setindex!(R, f(A[idx...]), ridx...)
        end
    end
end

concatenate_setindex!(R, v, I...) = (R[I...] .= (v,); R)
concatenate_setindex!(R, X::AbstractArray, I...) = (R[I...] = X)

## 0 arguments

map(f) = f()

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

mapany(f, A::AbstractArray) = map!(f, Vector{Any}(undef, length(A)), A)
mapany(f, itr) = Any[f(x) for x in itr]

"""
    map(f, c...) -> collection

Transform collection `c` by applying `f` to each element. For multiple collection arguments,
apply `f` elementwise, and stop when when any of them is exhausted.

See also [`map!`](@ref), [`foreach`](@ref), [`mapreduce`](@ref), [`mapslices`](@ref), [`zip`](@ref), [`Iterators.map`](@ref).

# Examples
```jldoctest
julia> map(x -> x * 2, [1, 2, 3])
3-element Vector{Int64}:
 2
 4
 6

julia> map(+, [1, 2, 3], [10, 20, 30, 400, 5000])
3-element Vector{Int64}:
 11
 22
 33
```
"""
map(f, A) = collect(Generator(f,A)) # default to returning an Array for `map` on general iterators

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
collection. `destination` must be at least as large as the smallest collection.

See also: [`map`](@ref), [`foreach`](@ref), [`zip`](@ref), [`copyto!`](@ref).

# Examples
```jldoctest
julia> a = zeros(3);

julia> map!(x -> x * 2, a, [1, 2, 3]);

julia> a
3-element Vector{Float64}:
 2.0
 4.0
 6.0

julia> map!(+, zeros(Int, 5), 100:999, 1:3)
5-element Vector{$(Int)}:
 101
 103
 105
   0
   0
```
"""
function map!(f::F, dest::AbstractArray, As::AbstractArray...) where {F}
    isempty(As) && throw(ArgumentError(
        """map! requires at least one "source" argument"""))
    map_n!(f, dest, As)
end

"""
    map(f, A::AbstractArray...) -> N-array

When acting on multi-dimensional arrays of the same [`ndims`](@ref),
they must all have the same [`axes`](@ref), and the answer will too.

See also [`broadcast`](@ref), which allows mismatched sizes.

# Examples
```
julia> map(//, [1 2; 3 4], [4 3; 2 1])
2×2 Matrix{Rational{$Int}}:
 1//4  2//3
 3//2  4//1

julia> map(+, [1 2; 3 4], zeros(2,1))
ERROR: DimensionMismatch

julia> map(+, [1 2; 3 4], [1,10,100,1000], zeros(3,1))  # iterates until 3rd is exhausted
3-element Vector{Float64}:
   2.0
  13.0
 102.0
```
"""
map(f, iters...) = collect(Generator(f, iters...))

# multi-item push!, pushfirst! (built on top of type-specific 1-item version)
# (note: must not cause a dispatch loop when 1-item case is not defined)
push!(A, a, b) = push!(push!(A, a), b)
push!(A, a, b, c...) = push!(push!(A, a, b), c...)
pushfirst!(A, a, b) = pushfirst!(pushfirst!(A, b), a)
pushfirst!(A, a, b, c...) = pushfirst!(pushfirst!(A, c...), a, b)

## hashing AbstractArray ##

const hash_abstractarray_seed = UInt === UInt64 ? 0x7e2d6fb6448beb77 : 0xd4514ce5
function hash(A::AbstractArray, h::UInt)
    h += hash_abstractarray_seed
    # Axes are themselves AbstractArrays, so hashing them directly would stack overflow
    # Instead hash the tuple of firsts and lasts along each dimension
    h = hash(map(first, axes(A)), h)
    h = hash(map(last, axes(A)), h)

    # For short arrays, it's not worth doing anything complicated
    if length(A) < 8192
        for x in A
            h = hash(x, h)
        end
        return h
    end

    # Goal: Hash approximately log(N) entries with a higher density of hashed elements
    # weighted towards the end and special consideration for repeated values. Colliding
    # hashes will often subsequently be compared by equality -- and equality between arrays
    # works elementwise forwards and is short-circuiting. This means that a collision
    # between arrays that differ by elements at the beginning is cheaper than one where the
    # difference is towards the end. Furthermore, choosing `log(N)` arbitrary entries from a
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
        # Hash the element
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

# The semantics of `collect` are weird. Better to write our own
function rest(a::AbstractArray{T}, state...) where {T}
    v = Vector{T}(undef, 0)
    # assume only very few items are taken from the front
    sizehint!(v, length(a))
    return foldl(push!, Iterators.rest(a, state...), init=v)
end


## keepat! ##

# NOTE: since these use `@inbounds`, they are actually only intended for Vector and BitVector

function _keepat!(a::AbstractVector, inds)
    local prev
    i = firstindex(a)
    for k in inds
        if @isdefined(prev)
            prev < k || throw(ArgumentError("indices must be unique and sorted"))
        end
        ak = a[k] # must happen even when i==k for bounds checking
        if i != k
            @inbounds a[i] = ak # k > i, so a[i] is inbounds
        end
        prev = k
        i = nextind(a, i)
    end
    deleteat!(a, i:lastindex(a))
    return a
end

function _keepat!(a::AbstractVector, m::AbstractVector{Bool})
    length(m) == length(a) || throw(BoundsError(a, m))
    j = firstindex(a)
    for i in eachindex(a, m)
        @inbounds begin
            if m[i]
                i == j || (a[j] = a[i])
                j = nextind(a, j)
            end
        end
    end
    deleteat!(a, j:lastindex(a))
end

## 1-d circshift ##
function circshift!(a::AbstractVector, shift::Integer)
    n = length(a)
    n == 0 && return
    shift = mod(shift, n)
    shift == 0 && return
    reverse!(a, 1, shift)
    reverse!(a, shift+1, length(a))
    reverse!(a)
    return a
end
