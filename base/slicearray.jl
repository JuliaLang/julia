"""
    AbstractSlices{S,N} <: AbstractArray{S,N}

Supertype for arrays of slices into a parent array over some dimension(s),
returning views that select all the data from the other dimensions.

`parent` will return the parent array.
"""
abstract type AbstractSlices{T,N} <: AbstractArray{T,N} end

"""
    Slices{P,SM,AX,S,N} <: AbstractSlices{S,N}

An `AbstractArray` of slices into a parent array over specified dimension(s),
returning views that select all the data from the other dimension(s).

These should typically be constructed by [`eachslice`](@ref), [`eachcol`](@ref) or
[`eachrow`](@ref). [`ColumnSlices`](@ref) and [`RowSlices`](@ref) are special cases.

[`parent(s::Slices)`](@ref) will return the parent array.
"""
struct Slices{P,SM,AX,S,N} <: AbstractSlices{S,N}
    """
    Parent array
    """
    parent::P
    """
    A tuple of length `ndims(parent)`, denoting how each dimension should be handled:
      - an integer `i`: this is the `i`th dimension of the outer `Slices` object.
      - `:`: an "inner" dimension
    """
    slicemap::SM
    """
    A tuple of length `N` containing the [`axes`](@ref) of the `Slices` object.
    """
    axes::AX
end

unitaxis(::AbstractArray) = Base.OneTo(1)

function Slices(A::P, slicemap::SM, ax::AX) where {P,SM,AX}
    N = length(ax)
    argT = map((a,l) -> l === (:) ? Colon : eltype(a), axes(A), slicemap)
    S = Base.promote_op(view, P, argT...)
    Slices{P,SM,AX,S,N}(A, slicemap, ax)
end

_slice_check_dims(N) = nothing
function _slice_check_dims(N, dim, dims...)
    1 <= dim <= N || throw(DimensionMismatch("Invalid dimension $dim"))
    dim in dims && throw(DimensionMismatch("Dimensions $dims are not unique"))
    _slice_check_dims(N,dims...)
end

@constprop :aggressive function _eachslice(A::AbstractArray{T,N}, dims::NTuple{M,Integer}, drop::Bool) where {T,N,M}
    _slice_check_dims(N,dims...)
    if drop
        # if N = 4, dims = (3,1) then
        # axes = (axes(A,3), axes(A,1))
        # slicemap = (2, :, 1, :)
        ax = map(dim -> axes(A,dim), dims)
        slicemap = ntuple(dim -> something(findfirst(isequal(dim), dims), (:)), N)
        return Slices(A, slicemap, ax)
    else
        # if N = 4, dims = (3,1) then
        # axes = (axes(A,1), OneTo(1), axes(A,3), OneTo(1))
        # slicemap = (1, :, 3, :)
        ax = ntuple(dim -> dim in dims ? axes(A,dim) : unitaxis(A), N)
        slicemap = ntuple(dim -> dim in dims ? dim : (:), N)
        return Slices(A, slicemap, ax)
    end
end
@inline function _eachslice(A::AbstractArray, dim::Integer, drop::Bool)
    _eachslice(A, (dim,), drop)
end

"""
    eachslice(A::AbstractArray; dims, drop=true)

Create a [`Slices`](@ref) object that is an array of slices over dimensions `dims` of `A`, returning
views that select all the data from the other dimensions in `A`. `dims` can either be an
integer or a tuple of integers.

If `drop = true` (the default), the outer `Slices` will drop the inner dimensions, and
the ordering of the dimensions will match those in `dims`. If `drop = false`, then the
`Slices` will have the same dimensionality as the underlying array, with inner
dimensions having size 1.

For matrices, the case `dims = 1` is [`eachrow`](@ref), and `dims = 2` is [`eachcol`](@ref).

[`stack`](@ref)`(slices; dims)` is the inverse of `eachslice(A; dims::Integer, drop=true)`.
See also [`mapslices`](@ref) and [`selectdim`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.9"
     Prior to Julia 1.9, this returned an iterator, and only a single dimension `dims` was supported.

# Examples

```jldoctest
julia> m = [1 2 3; 4 5 6; 7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> s = eachslice(m, dims=1)
3-element eachrow(::Matrix{Int64}) of 3-element slices with eltype Int64:
 [1, 2, 3]
 [4, 5, 6]
 [7, 8, 9]

julia> s[1]
3-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2
 3

julia> eachslice(m, dims=1, drop=false)  # keyword changes size of container
3×1 eachslice(::Matrix{Int64}, dims = 1, drop = false) of 3-element slices with eltype Int64:
 [1, 2, 3]
 [4, 5, 6]
 [7, 8, 9]

julia> x3 = rand(Int8, 3,4,5);

julia> eachslice(x3, dims=(3,2)) |> summary  # order of dims matters here
"5×4 eachslice(::Array{Int8, 3}, dims = (3, 2)) of 3-element slices with eltype Int8"

julia> eachslice(x3, dims=(3,2), drop=false) |> summary
"1×4×5 eachslice(::Array{Int8, 3}, dims = (2, 3), drop = false) of 3-element slices with eltype Int8"

julia> eachslice(Any[pi, 2pi], dims=1)  # eachrow would produce vector slices
2-element eachslice(::Vector{Any}, dims = 1) of 0-dimensional slices with eltype Any:
 fill(π)
 fill(6.283185307179586)
```
"""
@inline function eachslice(A; dims, drop=true)
    _eachslice(A, dims, drop)
end

"""
    eachrow(A::AbstractVecOrMat) <: AbstractVector

Create a [`RowSlices`](@ref) object that is a vector of rows of matrix or vector `A`.
Row slices are returned as `AbstractVector` views of `A`.

For the inverse, see [`stack`](@ref)`(rows; dims=1)`.

See also [`eachcol`](@ref), [`eachslice`](@ref) and [`mapslices`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.9"
     Prior to Julia 1.9, this returned an iterator.

# Examples

```jldoctest
julia> a = [1 2 3; 40 50 60]
2×3 Matrix{Int64}:
  1   2   3
 40  50  60

julia> s = eachrow(a)
2-element eachrow(::Matrix{Int64}) of 3-element slices with eltype Int64:
 [1, 2, 3]
 [40, 50, 60]

julia> s[1]
3-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2
 3

julia> s isa RowSlices, s[1] isa SubArray
(true, true)

julia> eachrow(Any[pi, 2pi])  # reshapes vector to matrix before slicing
2-element eachrow(::Matrix{Any}) of 1-element slices with eltype Any:
 [π]
 [6.283185307179586]
```
"""
eachrow(A::AbstractMatrix) = _eachslice(A, (1,), true)
eachrow(A::AbstractVector) = eachrow(reshape(A, size(A,1), 1))

"""
    eachcol(A::AbstractVecOrMat) <: AbstractVector

Create a [`ColumnSlices`](@ref) object that is a vector of columns of matrix or vector `A`.
Column slices are returned as `AbstractVector` views of `A`.

For the inverse, see [`stack`](@ref)`(cols)` or `reduce(`[`hcat`](@ref)`, cols)`.

See also [`eachrow`](@ref), [`eachslice`](@ref) and [`mapslices`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

!!! compat "Julia 1.9"
     Prior to Julia 1.9, this returned an iterator.

# Examples

```jldoctest
julia> a = [1 2 3; 40 50 60]
2×3 Matrix{Int64}:
  1   2   3
 40  50  60

julia> s = eachcol(a)
3-element eachcol(::Matrix{Int64}) of 2-element slices with eltype Int64:
 [1, 40]
 [2, 50]
 [3, 60]

julia> s[3]
2-element view(::Matrix{Int64}, :, 3) with eltype Int64:
  3
 60

julia> s isa ColumnSlices
true

julia> parent(s) === a
true

julia> eachcol('a':'c')  # accepts AbstractVector too, reshapes to 1-colum matrix:
1-element eachcol(reshape(::StepRange{Char, Int64}, 3, 1)) of 3-element slices with eltype Char:
 ['a', 'b', 'c']
```
"""
eachcol(A::AbstractMatrix) = _eachslice(A, (2,), true)
eachcol(A::AbstractVector) = eachcol(reshape(A, size(A, 1), 1))

"""
    RowSlices{M,AX,S}

A special case of [`Slices`](@ref) that is a vector of row slices of a matrix, as
constructed by [`eachrow`](@ref).

[`parent`](@ref) can be used to get the underlying matrix.
"""
const RowSlices{P<:AbstractMatrix,AX,S<:AbstractVector} = Slices{P,Tuple{Int,Colon},AX,S,1}

"""
    ColumnSlices{M,AX,S}

A special case of [`Slices`](@ref) that is a vector of column slices of a matrix, as
constructed by [`eachcol`](@ref).

[`parent`](@ref) can be used to get the underlying matrix.
"""
const ColumnSlices{P<:AbstractMatrix,AX,S<:AbstractVector} = Slices{P,Tuple{Colon,Int},AX,S,1}


IteratorSize(::Type{Slices{P,SM,AX,S,N}}) where {P,SM,AX,S,N} = HasShape{N}()
axes(s::Slices) = s.axes
size(s::Slices) = map(length, s.axes)

@inline function _slice_index(s::Slices, c...)
    return map(l -> l === (:) ? (:) : c[l], s.slicemap)
end

@inline function getindex(s::Slices{P,SM,AX,S,N}, I::Vararg{Int,N}) where {P,SM,AX,S,N}
    @boundscheck checkbounds(s, I...)
    @inbounds view(s.parent, _slice_index(s, I...)...)
end
@inline function setindex!(s::Slices{P,SM,AX,S,N}, val, I::Vararg{Int,N}) where {P,SM,AX,S,N}
    @boundscheck checkbounds(s, I...)
    @inbounds s.parent[_slice_index(s, I...)...] = val
end

parent(s::Slices) = s.parent

function _element_size(s::Slices)
    long = map((n,l) -> l === (:) ? n : nothing, size(s.parent), s.slicemap)
    filter(!isnothing, long)
end

# These control summary printing, like `3-element eachcol(adjoint(::Matrix{Int64})) of ...`
function showarg(io::IO, s::ColumnSlices, toplevel)
    print(io, "eachcol(")
    showarg(io, parent(s), false)
    print(io, ')')
    toplevel && print(io, " of ", dims2string(_element_size(s)), " slices with eltype ", eltype(eltype(s)))
    return nothing
end
function showarg(io::IO, s::RowSlices, toplevel)
    print(io, "eachrow(")
    showarg(io, parent(s), false)
    print(io, ')')
    toplevel && print(io, " of ", dims2string(_element_size(s)), " slices with eltype ", eltype(eltype(s)))
    return nothing
end
function showarg(io::IO, s::Slices, toplevel)
    drop = ndims(s) + ndims(eltype(s)) > ndims(parent(s))
    dims_vec = filter(c -> c isa Integer, [findfirst(==(d), s.slicemap) for d in 1:ndims(parent(s))])
    dims = length(dims_vec) == 1 ? only(dims_vec) : Tuple(dims_vec)
    print(io, "eachslice(")
    showarg(io, parent(s), false)
    print(io, ", dims = ", dims, drop ? ", drop = false)" : ")")
    toplevel && print(io, " of ", dims2string(_element_size(s)), " slices with eltype ", eltype(eltype(s)))
    return nothing
end
