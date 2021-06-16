    AbstractSparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}

Supertype for matrix with compressed sparse column (CSC).

!!! compat "Julia 1.4"
    `AbstractSparseMatrixCSC` requires at least Julia 1.4.

## `AbstractSparseMatrixCSC` interface

In addition to the [Abstract array interface](@ref man-interface-array), every
`AbstractSparseMatrixCSC` subtype `TS` must provide the following methods:

* [`size(::TS)`](@ref size)
* [`coloffsets(::TS) :: AbstractVector{<:Ti}`](@ref coloffsets)
* [`rowvals(::TS) :: AbstractVector{<:Ti}`](@ref rowvals)
* [`nonzeros(::TS) :: AbstractVector{<:Tv}`](@ref nonzeros)

Other sparse matrix methods such as [`nzrange`](@ref) and [`nnz`](@ref) are automatically
defined in terms of above functions.

## Assumed invariance

To use algorithms defined in SparseArrays, a matrix `A` of type `AbstractSparseMatrixCSC`
must satisfy the following constraints.

### Matrix `A` and all vectors constituting `A` have one-based indexing

```julia
@assert !has_offset_axes(A)
@assert !has_offset_axes(coloffsets(A))
@assert !has_offset_axes(rowvals(A))
@assert !has_offset_axes(nonzeros(A))
```

### Row indices in `rowval(A)` for each column are sorted

```julia
for col in axes(A, 2)
    @assert issorted(rowval(A)[nzrange(A, col)])
end
```

### Column pointers in `coloffsets(A)` are increasing, started at 1

```julia
@assert coloffsets(A)[1] === 1
@assert all(diff(coloffsets(A)) .>= 0)
```

### Row index vector `rowvals(A)` and non-zero value vector `nonzeros(A)` are long enough

```julia
@assert nnz(A) <= length(rowvals(A))
@assert nnz(A) <= length(nonzeros(A))
```

### Indices for rows, `rowvals(A)` and `nonzeros(A)` are representable by `Ti`

```julia
@assert size(A, 1) <= typemax(Ti)
@assert nnz(A) <= typemax(Ti)
```
