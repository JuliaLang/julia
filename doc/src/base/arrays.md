# [Arrays](@id lib-arrays)

## Constructors and Types

```@docs
Core.AbstractArray
Base.AbstractVector
Base.AbstractMatrix
Base.AbstractVecOrMat
Core.Array
Core.Array(::UndefInitializer, ::Any)
Core.Array(::Nothing, ::Any)
Core.Array(::Missing, ::Any)
Core.UndefInitializer
Core.undef
Base.Vector
Base.Vector(::UndefInitializer, ::Any)
Base.Vector(::Nothing, ::Any)
Base.Vector(::Missing, ::Any)
Base.Matrix
Base.Matrix(::UndefInitializer, ::Any, ::Any)
Base.Matrix(::Nothing, ::Any, ::Any)
Base.Matrix(::Missing, ::Any, ::Any)
Base.VecOrMat
Core.DenseArray
Base.DenseVector
Base.DenseMatrix
Base.DenseVecOrMat
Base.StridedArray
Base.StridedVector
Base.StridedMatrix
Base.StridedVecOrMat
Base.getindex(::Type, ::Any...)
Base.zeros
Base.ones
Base.BitArray
Base.BitArray(::UndefInitializer, ::Integer...)
Base.BitArray(::Any)
Base.trues
Base.falses
Base.fill
Base.fill!
Base.similar
```

## Basic functions

```@docs
Base.ndims
Base.size
Base.axes(::Any)
Base.axes(::AbstractArray, ::Any)
Base.length(::AbstractArray)
Base.eachindex
Base.IndexStyle
Base.IndexLinear
Base.IndexCartesian
Base.conj!
Base.stride
Base.strides
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
```

For specializing broadcast on custom types, see
```@docs
Base.BroadcastStyle
Base.broadcast_axes
Base.Broadcast.AbstractArrayStyle
Base.Broadcast.ArrayStyle
Base.Broadcast.DefaultArrayStyle
Base.Broadcast.broadcastable
```

## Indexing and assignment

```@docs
Base.getindex(::AbstractArray, ::Any...)
Base.setindex!(::AbstractArray, ::Any, ::Any...)
Base.copyto!(::AbstractArray, ::CartesianIndices, ::AbstractArray, ::CartesianIndices)
Base.isassigned
Base.Colon
Base.CartesianIndex
Base.CartesianIndices
Base.Dims
Base.LinearIndices
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
Base.parentindices
Base.selectdim
Base.reinterpret
Base.reshape
Base.dropdims
Base.vec
```

## Concatenation and permutation

```@docs
Base.cat
Base.vcat
Base.hcat
Base.hvcat
Base.vect
Base.circshift
Base.circshift!
Base.circcopy!
Base.findall(::Any)
Base.findall(::Function, ::Any)
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
Base.accumulate
Base.accumulate!
Base.cumprod
Base.cumprod!
Base.cumsum
Base.cumsum!
Base.diff
Base.repeat
Base.rot180
Base.rotl90
Base.rotr90
Base.mapslices
```

## Combinatorics

```@docs
Base.invperm
Base.isperm
Base.permute!(::Any, ::AbstractVector)
Base.invpermute!
Base.reverse(::AbstractVector; kwargs...)
Base.reverseind
Base.reverse!
```
