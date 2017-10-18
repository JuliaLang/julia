# [Arrays](@id lib-arrays)

## Constructors and Types

```@docs
Core.AbstractArray
Base.AbstractVector
Base.AbstractMatrix
Core.Array
Core.Array(::Any)
Base.Vector
Base.Vector(::Any)
Base.Matrix
Base.Matrix(::Any, ::Any)
Base.getindex(::Type, ::Any...)
Base.zeros
Base.ones
Base.BitArray
Base.BitArray(::Integer...)
Base.BitArray(::Any)
Base.trues
Base.falses
Base.fill
Base.fill!
Base.similar(::AbstractArray)
Base.similar(::Any, ::Tuple)
Base.eye
Base.linspace
Base.logspace
Base.Random.randsubseq
Base.Random.randsubseq!
```

## Basic functions

```@docs
Base.ndims
Base.size
Base.indices(::Any)
Base.indices(::AbstractArray, ::Any)
Base.length(::AbstractArray)
Base.eachindex
Base.linearindices
Base.IndexStyle
Base.conj!
Base.stride
Base.strides
Base.ind2sub
Base.sub2ind
Base.LinAlg.checksquare
```

## Broadcast and vectorization

See also the [dot syntax for vectorizing functions](@ref man-vectorized);
for example, `f.(args...)` implicitly calls `broadcast(f, args...)`.
Rather than relying on "vectorized" methods of functions like `sin`
to operate on arrays, you should use `sin.(a)` to vectorize via `broadcast`.

```@docs
Base.broadcast
Base.Broadcast.broadcast!
Base.@__dot__
Base.Broadcast.broadcast_getindex
Base.Broadcast.broadcast_setindex!
```

## Indexing and assignment

```@docs
Base.getindex(::AbstractArray, ::Any...)
Base.setindex!(::AbstractArray, ::Any, ::Any...)
Base.copy!(::AbstractArray, ::CartesianRange, ::AbstractArray, ::CartesianRange)
Base.isassigned
Base.Colon
Base.CartesianIndex
Base.CartesianRange
Base.to_indices
Base.checkbounds
Base.checkindex
```

## Views (SubArrays and other view types)

```@docs
Base.view
Base.@view
Base.@views
Base.parent
Base.parentindexes
Base.slicedim
Base.reinterpret
Base.reshape
Base.squeeze
Base.vec
```

## Concatenation and permutation

```@docs
Base.cat
Base.vcat
Base.hcat
Base.hvcat
Base.flipdim
Base.circshift
Base.circshift!
Base.circcopy!
Base.find(::Any)
Base.find(::Function, ::Any)
Base.findn
Base.findnz
Base.findfirst(::Any)
Base.findfirst(::Function, ::Any)
Base.findlast(::Any)
Base.findlast(::Function, ::Any)
Base.findnext(::Any, ::Integer)
Base.findnext(::Function, ::Any, ::Integer)
Base.findprev(::Any, ::Integer)
Base.findprev(::Function, ::Any, ::Integer)
Base.permutedims
Base.permutedims!
Base.PermutedDimsArray
Base.promote_shape
```

## Array functions

```@docs
Base.accumulate(::Any, ::Any, ::Integer)
Base.accumulate!
Base.cumprod
Base.cumprod!
Base.cumsum
Base.cumsum!
Base.cumsum_kbn
Base.crc32c
Base.LinAlg.diff
Base.repeat(::AbstractArray)
Base.rot180
Base.rotl90
Base.rotr90
Base.reducedim
Base.mapreducedim
Base.mapslices
Base.sum_kbn
```

## Combinatorics

```@docs
Base.Random.randperm
Base.Random.randperm!
Base.invperm
Base.isperm
Base.permute!(::Any, ::AbstractVector)
Base.ipermute!
Base.Random.randcycle
Base.Random.randcycle!
Base.Random.shuffle
Base.Random.shuffle!
Base.reverse
Base.reverseind
Base.reverse!
```

## BitArrays

[`BitArray`](@ref)s are space-efficient "packed" boolean arrays, which store one bit per boolean value.
They can be used similarly to `Array{Bool}` arrays (which store one byte per boolean value),
and can be converted to/from the latter via `Array(bitarray)` and `BitArray(array)`, respectively.

```@docs
Base.flipbits!
```

## [Sparse Vectors and Matrices](@id stdlib-sparse-arrays)

Sparse vectors and matrices largely support the same set of operations as their dense counterparts.
The following functions are specific to sparse arrays.

```@docs
Base.SparseArrays.SparseVector
Base.SparseArrays.SparseMatrixCSC
Base.SparseArrays.sparse
Base.SparseArrays.sparsevec
Base.SparseArrays.issparse
Base.full
Base.SparseArrays.nnz
Base.SparseArrays.spzeros
Base.SparseArrays.spones
Base.SparseArrays.speye(::Type, ::Integer, ::Integer)
Base.SparseArrays.speye(::SparseMatrixCSC)
Base.SparseArrays.spdiagm
Base.SparseArrays.sprand
Base.SparseArrays.sprandn
Base.SparseArrays.nonzeros
Base.SparseArrays.rowvals
Base.SparseArrays.nzrange
Base.SparseArrays.dropzeros!(::SparseMatrixCSC, ::Bool)
Base.SparseArrays.dropzeros(::SparseMatrixCSC, ::Bool)
Base.SparseArrays.dropzeros!(::SparseVector, ::Bool)
Base.SparseArrays.dropzeros(::SparseVector, ::Bool)
Base.SparseArrays.permute
Base.permute!{Tv, Ti, Tp <: Integer, Tq <: Integer}(::SparseMatrixCSC{Tv,Ti}, ::SparseMatrixCSC{Tv,Ti}, ::AbstractArray{Tp,1}, ::AbstractArray{Tq,1})
```
