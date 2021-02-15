"""
    SliceArray{N,L,P,CI,S} <: AbstractArray{S,N}

An `AbstractArray` of slices into a parent array.

- `N` is the dimension of the array of slices.
- `L` is a tuple of length `ndims(parent)`, denoting how each dimension should be handled:
  - an integer `i`: this is the `i`th dimension of the outer `SliceArray`.
  - `nothing`: an "inner" dimension
- `P` is the type of the parent array
- `CI` is the type of the Cartesian iterator
- `S` is the element type of the `SliceArray` (the return type of `view`).

These should typically be constructed by [`eachslice`](@ref), [`eachcol`](@ref) or
[`eachrow`](@ref).

[`parent(S::SliceArray)`](@ref) will return the parent array.
"""
struct SliceArray{N,L,P,CI,S} <: AbstractArray{S,N}
    """
    Parent array
    """
    parent::P
    """
    `CartesianIndices` iterator used to index each slice
    """
    cartiter::CI
end

unitaxis(::AbstractArray) = Base.OneTo(1)

function SliceArray{N,L}(A::P, iter::CI) where {N,L,P,CI}
    S = Base._return_type(view, Tuple{P, map((a,l) -> l === nothing ? Colon : eltype(a), axes(A), L)...})
    SliceArray{N,L,P,CI,S}(A, iter)
end


_slice_check_dims(N) = nothing
function _slice_check_dims(N, dim, dims...)
    1 <= dim <= N || throw(DimensionMismatch("Invalid dimension $dim"))
    dim in dims && throw(DimensionMismatch("Dimensions $dims are not unique"))
    _slice_check_dims(N,dims...)
end

@inline function _eachslice(A::AbstractArray{T,N}, dims::NTuple{M,Integer}, drop::Bool) where {T,N,M}
    _slice_check_dims(N,dims...)
    if drop
        # if N = 4, dims = (3,1) then
        # iter = CartesianIndices(axes(A,3), axes(A,1))
        # L = (2, nothing, 1, nothing)
        iter = CartesianIndices(map(dim -> axes(A,dim), dims))
        L = ntuple(dim -> findfirst(isequal(dim), dims), N)
        return SliceArray{M,L}(A, iter)
    else
        # if N = 4, dims = (3,1) then
        # iter = CartesianIndices(axes(A,1), OneTo(1), axes(A,3), OneTo(1))
        # L = (1, nothing, 3, nothing)
        iter = CartesianIndices(ntuple(dim -> dim in dims ? axes(A,dim) : unitaxis(A), N))
        L = ntuple(dim -> dim in dims ? dim : nothing, N)
        return SliceArray{N,L}(A, iter)
    end
end
@inline function _eachslice(A::AbstractArray{T,N}, dim::Integer, drop::Bool) where {T,N}
    _eachslice(A, (dim,), drop)
end

"""
    eachslice(A::AbstractArray; dims, drop=true)

Create a [`SliceArray`](@ref) that is indexed over dimensions `dims` of `A`, returning
views that select all the data from the other dimensions in `A`. `dims` can either by an
integer or a tuple of integers.

If `drop = true` (the default), the outer `SliceArray` will drop the inner dimensions, and
the ordering of the dimensions will match those in `dims`. If `drop = false`, then the
`SliceArray` will have the same dimensionality as the underlying array, with inner
dimensions having size 1.

See also [`eachrow`](@ref), [`eachcol`](@ref), and [`selectdim`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.7"
     Prior to Julia 1.7, this returned an iterator, and only a single dimension `dims` was supported.

# Example

```jldoctest
julia> M = [1 2 3; 4 5 6; 7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> S = eachslice(M,dims=1)
3-element Rows{Matrix{Int64}, CartesianIndices{1, Tuple{Base.OneTo{Int64}}}, SubArray{Int64, 1, Matrix{Int64}, Tuple{Int64, Base.Slice{Base.OneTo{Int64}}}, true}}:
 [1, 2, 3]
 [4, 5, 6]
 [7, 8, 9]

julia> S[1]
3-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2
 3

julia> T = eachslice(M,dims=1,drop=false)
3×1 SliceArray{2, (1, nothing), Matrix{Int64}, CartesianIndices{2, Tuple{Base.OneTo{Int64}, Base.OneTo{Int64}}}, SubArray{Int64, 1, Matrix{Int64}, Tuple{Int64, Base.Slice{Base.OneTo{Int64}}}, true}}:
 [1, 2, 3]
 [4, 5, 6]
 [7, 8, 9]
```
"""
@inline function eachslice(A; dims, drop=true)
    _eachslice(A, dims, drop)
end

"""
    eachrow(A::AbstractVecOrMat)

Create a [`Rows`](@ref) that indexes over the rows of a vector or matrix `A`.

See also [`eachcol`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.7"
     Prior to Julia 1.7, this returned an iterator.

# Example

```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> S = eachrow(a)
2-element Rows{Matrix{Int64}, CartesianIndices{1, Tuple{Base.OneTo{Int64}}}, SubArray{Int64, 1, Matrix{Int64}, Tuple{Int64, Base.Slice{Base.OneTo{Int64}}}, true}}:
 [1, 2]
 [3, 4]

julia> S[1]
2-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2
```
"""
eachrow(A::AbstractMatrix) = _eachslice(A, (1,), true)
eachrow(A::AbstractVector) = eachrow(reshape(A, size(A,1), 1))

"""
    eachcol(A::AbstractVecOrMat)

Create a [`Columns`](@ref) that iterates over the second dimension of matrix `A`, returning the
columns as `AbstractVector` views.

See also [`eachrow`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.7"
     Prior to Julia 1.7, this returned an iterator.

# Example

```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> S = eachcol(a)
2-element Columns{Matrix{Int64}, CartesianIndices{1, Tuple{Base.OneTo{Int64}}}, SubArray{Int64, 1, Matrix{Int64}, Tuple{Base.Slice{Base.OneTo{Int64}}, Int64}, true}}:
 [1, 3]
 [2, 4]

julia> S[1]
2-element view(::Matrix{Int64}, :, 1) with eltype Int64:
 1
 3
```
"""
eachcol(A::AbstractMatrix) = _eachslice(A, (2,), true)
eachcol(A::AbstractVector) = eachcol(reshape(A, size(A,1), 1))

"""
    Rows{M,CI,S}

A special case of [`SliceArray`](@ref) that is a vector of row slices of a matrix, as
constructed by [`eachrow`](@ref).

[`parent(S)`](@ref) can be used to get the underlying matrix.
"""
const Rows{P<:AbstractMatrix,CI,S<:AbstractVector} = SliceArray{1,(1,nothing),P,CI,S}

"""
    Columns{M,CI,S}

A special case of [`SliceArray`](@ref) that is a vector of column slices of a matrix, as
constructed by [`eachcol`](@ref).

[`parent(S)`](@ref) can be used to get the underlying matrix.
"""
const Columns{P<:AbstractMatrix,CI,S<:AbstractVector} = SliceArray{1,(nothing,1),P,CI,S}


IteratorSize(::Type{SliceArray{N,L,P,CI,S}}) where {N,L,P,CI,S} = IteratorSize(CI)
size(s::SliceArray) = size(s.cartiter)
size(s::SliceArray, dim) = size(s.cartiter, dim)
length(s::SliceArray) = length(s.cartiter)

@inline function _slice_index(s::SliceArray{N,L,P,CI,S}, I...) where {N,L,P,CI,S}
    c = s.cartiter[I...]
    return map(l -> l === nothing ? (:) : c[l], L)
end

getindex(s::SliceArray, I...) = view(s.parent, _slice_index(s, I...)...)
setindex!(s::SliceArray, val, I...) = s.parent[_slice_index(s, I...)...] = val

parent(s::SliceArray) = s.arr
