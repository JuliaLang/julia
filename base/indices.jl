# This file is a part of Julia. License is MIT: http://julialang.org/license

Dims{N} = NTuple{N,Int}
DimsInteger{N} = NTuple{N,Integer}
Indices{N} = NTuple{N,AbstractUnitRange}

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

```jldoctest
julia> a = ones(3,4,1,1,1);

julia> b = ones(3,4);

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
    promote_shape(indices(a), indices(b))
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
        ii = length(indices(X,i))
        jj = I[j]
        if i == li || j == lj
            while i < li
                i += 1
                ii *= length(indices(X,i))
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
    (_length(X)==1 || throw_setindex_mismatch(X,()))

setindex_shape_check(X::AbstractArray, i::Integer) =
    (_length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check(X::AbstractArray{<:Any,1}, i::Integer) =
    (_length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check(X::AbstractArray{<:Any,1}, i::Integer, j::Integer) =
    (_length(X)==i*j || throw_setindex_mismatch(X, (i,j)))

function setindex_shape_check(X::AbstractArray{<:Any,2}, i::Integer, j::Integer)
    if length(X) != i*j
        throw_setindex_mismatch(X, (i,j))
    end
    sx1 = length(indices(X,1))
    if !(i == 1 || i == sx1 || sx1 == 1)
        throw_setindex_mismatch(X, (i,j))
    end
end
setindex_shape_check(X, I...) = nothing # Non-arrays broadcast to all idxs

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
to_index(I::AbstractArray{Bool}) = LogicalIndex(I)
to_index(I::AbstractArray) = I
to_index(I::AbstractArray{<:Union{AbstractArray, Colon}}) = throw(ArgumentError("invalid index: $I"))
to_index(::Colon) = throw(ArgumentError("colons must be converted by to_indices(...)"))
to_index(i) = throw(ArgumentError("invalid index: $i"))

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
`to_indices(A, indices(A), I)`, which then recursively walks through both the
given tuple of indices and the dimensional indices of `A` in tandem. As such,
not all index types are guaranteed to propagate to `Base.to_index`.
"""
to_indices(A, I::Tuple) = (@_inline_meta; to_indices(A, indices(A), I))
to_indices(A, inds, ::Tuple{}) = ()
to_indices(A, inds, I::Tuple{Any, Vararg{Any}}) =
    (@_inline_meta; (to_index(A, I[1]), to_indices(A, _maybetail(inds), tail(I))...))

_maybetail(::Tuple{}) = ()
_maybetail(t::Tuple) = tail(t)

# TODO: reparametrize in a manner consistent with other AbstractUnitRanges,
# rename (IdempotentRange? IdentityRange?), and move to ranges.jl
"""
    Slice(r::AbstractUnitRange) -> s

Construct an `AbstractUnitRange` where `s[i] == i` for any valid `i`;
equivalently, `indices(s, 1) == r` and `s[s] === s`.

These are particularly useful for creating `view`s of arrays that
preserve the supplied indices:
```jldoctest
julia> a = rand(8);

julia> v1 = view(a, 3:5);

julia> indices(v1, 1)
Base.OneTo(3)

julia> s = Base.Slice(3:5)
Base.Slice(3:5)

julia> v2 = view(a, s);

julia> indices(v2, 1)
3:5
```

Upon calling `to_indices()`, Colons are converted to Slice objects to represent
the indices over which the Colon spans.
"""
struct Slice{T<:AbstractUnitRange} <: AbstractUnitRange{Int}
    indices::T
end
Slice(S::Slice) = S  # idempotent
indices(S::Slice) = (S.indices,)
unsafe_indices(S::Slice) = (S.indices,)
indices1(S::Slice) = S.indices
first(S::Slice) = first(S.indices)
last(S::Slice) = last(S.indices)
errmsg(A) = error("size not supported for arrays with indices $(indices(A)); see http://docs.julialang.org/en/latest/devdocs/offset-arrays/")
size(S::Slice) = first(S.indices) == 1 ? (length(S.indices),) : errmsg(S)
length(S::Slice) = first(S.indices) == 1 ? length(S.indices) : errmsg(S)
unsafe_length(S::Slice) = first(S.indices) == 1 ? unsafe_length(S.indices) : errmsg(S)
getindex(S::Slice, i::Int) = (@_inline_meta; @boundscheck checkbounds(S, i); i)
function getindex(r::Slice, s::AbstractUnitRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    s
end
show(io::IO, r::Slice) = print(io, "Base.Slice(", r.indices, ")")
start(S::Slice) = start(S.indices)
next(S::Slice, s) = next(S.indices, s)
done(S::Slice, s) = done(S.indices, s)
intersect{I<:AbstractUnitRange{Int}}(r::Slice{I}, s::Slice{I}) =
    Slice(convert(I, max(first(r), first(s)):min(last(r), last(s))))
intersect(r::Slice, s::Slice) =
    Slice(max(first(r), first(s)):min(last(r), last(s)))
reverse(r::Slice) = error("reverse is not supported for Base.Slice")
sortperm(r::Slice) = r
==(r::Slice, s::Slice) = (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::Slice, s::OrdinalRange) = (first(r) == first(s) == 1) & (step(r) == step(s)) & (last(r) == last(s))
==(s::OrdinalRange, r::Slice) = r == s
promote_rule{R1,R2}(::Type{Slice{R1}},::Type{Slice{R2}}) =
    Slice{promote_type(R1,R2)}
convert{R<:AbstractUnitRange{Int}}(::Type{Slice{R}}, r::Slice{R}) = r
convert{R<:AbstractUnitRange{Int}}(::Type{Slice{R}}, r::Slice) =
    Slice(convert(R, r.indices))
convert{R<:AbstractUnitRange{Int}}(::Type{Slice}, r::Slice{R}) =
    convert(Slice{R}, r)
