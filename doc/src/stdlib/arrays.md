# [Arrays](@id lib-arrays)

## Basic functions

```@docs
Base.ndims
Base.size
Base.indices(::Any)
Base.indices(::AbstractArray, ::Any)
Base.length(::AbstractArray)
Base.eachindex
Base.linearindices
Base.linearindexing
Base.countnz
Base.conj!
Base.stride
Base.strides
Base.ind2sub
Base.sub2ind
Base.LinAlg.checksquare
```

## Constructors

```@docs
Core.Array
Base.getindex(::Type, ::Any...)
Base.zeros
Base.ones
Base.BitArray
Base.trues
Base.falses
Base.fill
Base.fill!
Base.reshape
Base.similar(::AbstractArray)
Base.similar(::Any, ::Tuple)
Base.reinterpret
Base.eye
Base.linspace
Base.logspace
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

## Indexing, Assignment, and Concatenation

```@docs
Base.getindex(::AbstractArray, ::Any...)
Base.view
Base.@view
Base.@views
Base.to_indices
Base.Colon
Base.parent
Base.parentindexes
Base.slicedim
Base.setindex!(::AbstractArray, ::Any, ::Any...)
Base.isassigned
Base.cat
Base.vcat
Base.hcat
Base.hvcat
Base.flipdim
Base.circshift
Base.circshift!
Base.circcopy!
Base.contains(::Function, ::Any, ::Any)
Base.find(::Any)
Base.find(::Function, ::Any)
Base.findn
Base.findnz
Base.findfirst(::Any)
Base.findfirst(::Any, ::Any)
Base.findfirst(::Function, ::Any)
Base.findlast(::Any)
Base.findlast(::Any, ::Any)
Base.findlast(::Function, ::Any)
Base.findnext(::Any, ::Integer)
Base.findnext(::Function, ::Any, ::Integer)
Base.findnext(::Any, ::Any, ::Integer)
Base.findprev(::Any, ::Integer)
Base.findprev(::Function, ::Any, ::Integer)
Base.findprev(::Any, ::Any, ::Integer)
Base.permutedims
Base.permutedims!
Base.PermutedDimsArray
Base.squeeze
Base.vec
Base.promote_shape
Base.checkbounds
Base.checkindex
Base.Random.randsubseq
Base.Random.randsubseq!
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
Base.LinAlg.diff
Base.LinAlg.gradient
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
Base.invperm
Base.isperm
Base.permute!{T}(::Any, ::AbstractArray{T, 1})
Base.ipermute!
Base.Random.randcycle
Base.Random.shuffle
Base.Random.shuffle!
Base.reverse
Base.reverseind
Base.reverse!
```

## BitArrays

`BitArray`s are space-efficient "packed" boolean arrays, which store one bit per boolean value.
 They can be used similarly to `Array{Bool}` arrays (which store one byte per boolean value),
and can be converted to/from the latter via `Array(bitarray)` and `BitArray(array)`, respectively.

```@docs
Base.flipbits!
Base.rol!
Base.rol
Base.ror!
Base.ror
```

## Sparse Vectors and Matrices

Sparse vectors and matrices largely support the same set of operations as their dense counterparts.
The following functions are specific to sparse arrays.

```@docs
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
