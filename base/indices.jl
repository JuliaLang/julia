# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Dims{N}

An `NTuple` of `N` `Int`s used to represent the dimensions
of an [`AbstractArray`](@ref).
"""
Dims{N} = NTuple{N,Int}
DimsInteger{N} = NTuple{N,Integer}
Indices{N} = NTuple{N,AbstractUnitRange}

## Traits for array types ##

abstract type IndexStyle end
"""
    IndexLinear()

Subtype of [`IndexStyle`](@ref) used to describe arrays which
are optimally indexed by one linear index.

A linear indexing style uses one integer to describe the position in the array
(even if it's a multidimensional array) and column-major
ordering is used to access the elements. For example,
if `A` were a `(2, 3)` custom matrix type with linear indexing,
and we referenced `A[5]` (using linear style), this would
be equivalent to referencing `A[1, 3]` (since `2*1 + 3 = 5`).
See also [`IndexCartesian`](@ref).
"""
struct IndexLinear <: IndexStyle end
"""
    IndexCartesian()

Subtype of [`IndexStyle`](@ref) used to describe arrays which
are optimally indexed by a Cartesian index.

A cartesian indexing style uses multiple integers/indices to describe the position in the array.
For example, if `A` were a `(2, 3, 4)` custom matrix type with cartesian indexing,
we could reference `A[2, 1, 3]` and Julia would automatically convert this into the
correct location in the underlying memory. See also [`IndexLinear`](@ref).
"""
struct IndexCartesian <: IndexStyle end

"""
    IndexStyle(A)
    IndexStyle(typeof(A))

`IndexStyle` specifies the "native indexing style" for array `A`. When
you define a new [`AbstractArray`](@ref) type, you can choose to implement
either linear indexing (with [`IndexLinear`](@ref)) or cartesian indexing.
If you decide to implement linear indexing, then you must set this trait for your array
type:

    Base.IndexStyle(::Type{<:MyArray}) = IndexLinear()

The default is [`IndexCartesian()`](@ref).

Julia's internal indexing machinery will automatically (and invisibly)
convert all indexing operations into the preferred style. This allows users
to access elements of your array using any indexing style, even when explicit
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

# array shape rules

promote_shape(::Tuple{}, ::Tuple{}) = ()

function promote_shape(a::Tuple{Int,}, b::Tuple{Int,})
    if a[1] != b[1]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

function promote_shape(a::Tuple{Int,Int}, b::Tuple{Int,})
    if a[1] != b[1] || a[2] != 1
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

promote_shape(a::Tuple{Int,}, b::Tuple{Int,Int}) = promote_shape(b, a)

function promote_shape(a::Tuple{Int, Int}, b::Tuple{Int, Int})
    if a[1] != b[1] || a[2] != b[2]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

"""
    promote_shape(s1, s2)

Check two array shapes for compatibility, allowing trailing singleton dimensions, and return
whichever shape has more dimensions.

# Examples
```jldoctest
julia> a = fill(1, (3,4,1,1,1));

julia> b = fill(1, (3,4));

julia> promote_shape(a,b)
(Base.OneTo(3), Base.OneTo(4), Base.OneTo(1), Base.OneTo(1), Base.OneTo(1))

julia> promote_shape((2,3,1,4), (2, 3, 1, 4, 1))
(2, 3, 1, 4, 1)
```
"""
function promote_shape(a::Dims, b::Dims)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    return a
end

function promote_shape(a::AbstractArray, b::AbstractArray)
    promote_shape(axes(a), axes(b))
end

function promote_shape(a::Indices, b::Indices)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1:1
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    return a
end

function throw_setindex_mismatch(X, I)
    if length(I) == 1
        throw(DimensionMismatch("tried to assign $(length(X)) elements to $(I[1]) destinations"))
    else
        throw(DimensionMismatch("tried to assign $(dims2string(size(X))) array to $(dims2string(I)) destination"))
    end
end

# check for valid sizes in A[I...] = X where X <: AbstractArray
# we want to allow dimensions that are equal up to permutation, but only
# for permutations that leave array elements in the same linear order.
# those are the permutations that preserve the order of the non-singleton
# dimensions.
function setindex_shape_check(X::AbstractArray, I::Integer...)
    li = ndims(X)
    lj = length(I)
    i = j = 1
    while true
        ii = length(axes(X,i))
        jj = I[j]
        if i == li || j == lj
            while i < li
                i += 1
                ii *= length(axes(X,i))
            end
            while j < lj
                j += 1
                jj *= I[j]
            end
            if ii != jj
                throw_setindex_mismatch(X, I)
            end
            return
        end
        if ii == jj
            i += 1
            j += 1
        elseif ii == 1
            i += 1
        elseif jj == 1
            j += 1
        else
            throw_setindex_mismatch(X, I)
        end
    end
end

setindex_shape_check(X::AbstractArray) =
    (length(X)==1 || throw_setindex_mismatch(X,()))

setindex_shape_check(X::AbstractArray, i::Integer) =
    (length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check(X::AbstractArray{<:Any,1}, i::Integer) =
    (length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check(X::AbstractArray{<:Any,1}, i::Integer, j::Integer) =
    (length(X)==i*j || throw_setindex_mismatch(X, (i,j)))

function setindex_shape_check(X::AbstractArray{<:Any,2}, i::Integer, j::Integer)
    if length(X) != i*j
        throw_setindex_mismatch(X, (i,j))
    end
    sx1 = length(axes(X,1))
    if !(i == 1 || i == sx1 || sx1 == 1)
        throw_setindex_mismatch(X, (i,j))
    end
end

# convert to a supported index type (array or Int)
"""
    to_index(A, i)

Convert index `i` to an `Int` or array of indices to be used as an index into array `A`.

Custom array types may specialize `to_index(::CustomArray, i)` to provide
special indexing behaviors. Note that some index types (like `Colon`) require
more context in order to transform them into an array of indices; those get
converted in the more complicated `to_indices` function. By default, this
simply calls the generic `to_index(i)`. This must return either an `Int` or an
`AbstractArray` of scalar indices that are supported by `A`.
"""
to_index(A, i) = to_index(i)

"""
    to_index(i)

Convert index `i` to an `Int` or array of `Int`s to be used as an index for all arrays.

Custom index types may specialize `to_index(::CustomIndex)` to provide special
indexing behaviors. This must return either an `Int` or an `AbstractArray` of
`Int`s.
"""
to_index(i::Integer) = convert(Int,i)::Int
to_index(i::Bool) = throw(ArgumentError("invalid index: $i of type $(typeof(i))"))
to_index(I::AbstractArray{Bool}) = LogicalIndex(I)
to_index(I::AbstractArray) = I
to_index(I::AbstractArray{<:Union{AbstractArray, Colon}}) =
    throw(ArgumentError("invalid index: $I of type $(typeof(I))"))
to_index(::Colon) = throw(ArgumentError("colons must be converted by to_indices(...)"))
to_index(i) = throw(ArgumentError("invalid index: $i of type $(typeof(i))"))

# The general to_indices is mostly defined in multidimensional.jl, but this
# definition is required for bootstrap:
"""
    to_indices(A, I::Tuple)

Convert the tuple `I` to a tuple of indices for use in indexing into array `A`.

The returned tuple must only contain either `Int`s or `AbstractArray`s of
scalar indices that are supported by array `A`. It will error upon encountering
a novel index type that it does not know how to process.

For simple index types, it defers to the unexported `Base.to_index(A, i)` to
process each index `i`. While this internal function is not intended to be
called directly, `Base.to_index` may be extended by custom array or index types
to provide custom indexing behaviors.

More complicated index types may require more context about the dimension into
which they index. To support those cases, `to_indices(A, I)` calls
`to_indices(A, axes(A), I)`, which then recursively walks through both the
given tuple of indices and the dimensional indices of `A` in tandem. As such,
not all index types are guaranteed to propagate to `Base.to_index`.
"""
to_indices(A, I::Tuple) = (@_inline_meta; to_indices(A, axes(A), I))
to_indices(A, I::Tuple{Any}) = (@_inline_meta; to_indices(A, (eachindex(IndexLinear(), A),), I))
to_indices(A, inds, ::Tuple{}) = ()
to_indices(A, inds, I::Tuple{Any, Vararg{Any}}) =
    (@_inline_meta; (to_index(A, I[1]), to_indices(A, _maybetail(inds), tail(I))...))

_maybetail(::Tuple{}) = ()
_maybetail(t::Tuple) = tail(t)

"""
   Slice(indices)

Represent an AbstractUnitRange of indices as a vector of the indices themselves.

Upon calling `to_indices`, Colons are converted to Slice objects to represent
the indices over which the Colon spans. Slice objects are themselves unit
ranges with the same indices as those they wrap. This means that indexing into
Slice objects with an integer always returns that exact integer, and they
iterate over all the wrapped indices, even supporting offset indices.
"""
struct Slice{T<:AbstractUnitRange} <: AbstractUnitRange{Int}
    indices::T
end
Slice(S::Slice) = S
axes(S::Slice) = (S,)
unsafe_indices(S::Slice) = (S,)
axes1(S::Slice) = S
axes(S::Slice{<:OneTo}) = (S.indices,)
unsafe_indices(S::Slice{<:OneTo}) = (S.indices,)
axes1(S::Slice{<:OneTo}) = S.indices

first(S::Slice) = first(S.indices)
last(S::Slice) = last(S.indices)
size(S::Slice) = (length(S.indices),)
length(S::Slice) = length(S.indices)
unsafe_length(S::Slice) = unsafe_length(S.indices)
getindex(S::Slice, i::Int) = (@_inline_meta; @boundscheck checkbounds(S, i); i)
getindex(S::Slice, i::AbstractUnitRange{<:Integer}) = (@_inline_meta; @boundscheck checkbounds(S, i); i)
getindex(S::Slice, i::StepRange{<:Integer}) = (@_inline_meta; @boundscheck checkbounds(S, i); i)
show(io::IO, r::Slice) = print(io, "Base.Slice(", r.indices, ")")
iterate(S::Slice, s...) = iterate(S.indices, s...)

"""
    LinearIndices(A::AbstractArray)

Return a `LinearIndices` array with the same shape and [`axes`](@ref) as `A`,
holding the linear index of each entry in `A`. Indexing this array with
cartesian indices allows mapping them to linear indices.

For arrays with conventional indexing (indices start at 1), or any multidimensional
array, linear indices range from 1 to `length(A)`. However, for `AbstractVector`s
linear indices are `axes(A, 1)`, and therefore do not start at 1 for vectors with
unconventional indexing.

Calling this function is the "safe" way to write algorithms that
exploit linear indexing.

# Examples
```jldoctest
julia> A = fill(1, (5,6,7));

julia> b = LinearIndices(A);

julia> extrema(b)
(1, 210)
```

    LinearIndices(inds::CartesianIndices) -> R
    LinearIndices(sz::Dims) -> R
    LinearIndices((istart:istop, jstart:jstop, ...)) -> R

Return a `LinearIndices` array with the specified shape or [`axes`](@ref).

# Example

The main purpose of this constructor is intuitive conversion
from cartesian to linear indexing:

```jldoctest
julia> linear = LinearIndices((1:3, 1:2))
3×2 LinearIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
 1  4
 2  5
 3  6

julia> linear[1,2]
4
```
"""
struct LinearIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{Int,N}
    indices::R
end

LinearIndices(::Tuple{}) = LinearIndices{0,typeof(())}(())
LinearIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} =
    LinearIndices(map(r->convert(AbstractUnitRange{Int}, r), inds))
LinearIndices(sz::NTuple{N,<:Integer}) where {N} = LinearIndices(map(Base.OneTo, sz))
LinearIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} =
    LinearIndices(map(i->first(i):last(i), inds))
LinearIndices(A::Union{AbstractArray,SimpleVector}) = LinearIndices(axes(A))

promote_rule(::Type{LinearIndices{N,R1}}, ::Type{LinearIndices{N,R2}}) where {N,R1,R2} =
    LinearIndices{N,indices_promote_type(R1,R2)}

function indices_promote_type(::Type{Tuple{R1,Vararg{R1,N}}}, ::Type{Tuple{R2,Vararg{R2,N}}}) where {R1,R2,N}
    R = promote_type(R1, R2)
    Tuple{R,Vararg{R,N}}
end

convert(::Type{LinearIndices{N,R}}, inds::LinearIndices{N}) where {N,R} =
    LinearIndices(convert(R, inds.indices))

# AbstractArray implementation
IndexStyle(::Type{<:LinearIndices}) = IndexLinear()
axes(iter::LinearIndices) = map(axes1, iter.indices)
size(iter::LinearIndices) = map(unsafe_length, iter.indices)
function getindex(iter::LinearIndices, i::Int)
    @_inline_meta
    @boundscheck checkbounds(iter, i)
    i
end
function getindex(iter::LinearIndices, i::AbstractRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(iter, i)
    @inbounds isa(iter, LinearIndices{1}) ? iter.indices[1][i] : (first(iter):last(iter))[i]
end
# More efficient iteration — predominantly for non-vector LinearIndices
# but one-dimensional LinearIndices must be special-cased to support OffsetArrays
iterate(iter::LinearIndices{1}, s...) = iterate(axes1(iter.indices[1]), s...)
iterate(iter::LinearIndices, i=1) = i > length(iter) ? nothing : (i, i+1)

# Needed since firstindex and lastindex are defined in terms of LinearIndices
first(iter::LinearIndices) = 1
first(iter::LinearIndices{1}) = (@_inline_meta; first(axes1(iter.indices[1])))
last(iter::LinearIndices) = (@_inline_meta; length(iter))
last(iter::LinearIndices{1}) = (@_inline_meta; last(axes1(iter.indices[1])))
