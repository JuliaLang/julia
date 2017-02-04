# Interfaces

A lot of the power and extensibility in Julia comes from a collection of informal interfaces.
 By extending a few specific methods to work for a custom type, objects of that type not only
receive those functionalities, but they are also able to be used in other methods that are written
to generically build upon those behaviors.

## [Iteration](@id man-interface-iteration)

| Required methods               |                        | Brief description                                                                     |
|:------------------------------ |:---------------------- |:------------------------------------------------------------------------------------- |
| `start(iter)`                  |                        | Returns the initial iteration state                                                   |
| `next(iter, state)`            |                        | Returns the current item and the next state                                           |
| `done(iter, state)`            |                        | Tests if there are any items remaining                                                |
| **Important optional methods** | **Default definition** | **Brief description**                                                                 |
| `iteratorsize(IterType)`       | `HasLength()`          | One of `HasLength()`, `HasShape()`, `IsInfinite()`, or `SizeUnknown()` as appropriate |
| `iteratoreltype(IterType)`     | `HasEltype()`          | Either `EltypeUnknown()` or `HasEltype()` as appropriate                              |
| `eltype(IterType)`             | `Any`                  | The type the items returned by `next()`                                               |
| `length(iter)`                 | (*undefined*)          | The number of items, if known                                                         |
| `size(iter, [dim...])`         | (*undefined*)          | The number of items in each dimension, if known                                       |

| Value returned by `iteratorsize(IterType)` | Required Methods                           |
|:------------------------------------------ |:------------------------------------------ |
| `HasLength()`                              | `length(iter)`                             |
| `HasShape()`                               | `length(iter)`  and `size(iter, [dim...])` |
| `IsInfinite()`                             | (*none*)                                   |
| `SizeUnknown()`                            | (*none*)                                   |

| Value returned by `iteratoreltype(IterType)` | Required Methods   |
|:-------------------------------------------- |:------------------ |
| `HasEltype()`                                | `eltype(IterType)` |
| `EltypeUnknown()`                            | (*none*)           |

Sequential iteration is implemented by the methods [`start()`](@ref), [`done()`](@ref), and [`next()`](@ref). Instead
of mutating objects as they are iterated over, Julia provides these three methods to keep track
of the iteration state externally from the object. The `start(iter)` method returns the initial
state for the iterable object `iter`. That state gets passed along to `done(iter, state)`, which
tests if there are any elements remaining, and `next(iter, state)`, which returns a tuple containing
the current element and an updated `state`. The `state` object can be anything, and is generally
considered to be an implementation detail private to the iterable object.

Any object defines these three methods is iterable and can be used in the [many functions that rely upon iteration](@ref lib-collections-iteration).
It can also be used directly in a `for` loop since the syntax:

```julia
for i in iter   # or  "for i = iter"
    # body
end
```

is translated into:

```julia
state = start(iter)
while !done(iter, state)
    (i, state) = next(iter, state)
    # body
end
```

A simple example is an iterable sequence of square numbers with a defined length:

```jldoctest squaretype
julia> immutable Squares
           count::Int
       end

julia> Base.start(::Squares) = 1

julia> Base.next(S::Squares, state) = (state*state, state+1)

julia> Base.done(S::Squares, state) = state > S.count

julia> Base.eltype(::Type{Squares}) = Int # Note that this is defined for the type

julia> Base.length(S::Squares) = S.count
```

With only [`start`](@ref), [`next`](@ref), and [`done`](@ref) definitions, the `Squares` type is already pretty powerful.
We can iterate over all the elements:

```jldoctest squaretype
julia> for i in Squares(7)
           println(i)
       end
1
4
9
16
25
36
49
```

We can use many of the builtin methods that work with iterables, like [`in()`](@ref), [`mean()`](@ref) and [`std()`](@ref):

```jldoctest squaretype
julia> 25 in Squares(10)
true

julia> mean(Squares(100))
3383.5

julia> std(Squares(100))
3024.355854282583
```

There are a few more methods we can extend to give Julia more information about this iterable
collection.  We know that the elements in a `Squares` sequence will always be `Int`. By extending
the [`eltype()`](@ref) method, we can give that information to Julia and help it make more specialized
code in the more complicated methods. We also know the number of elements in our sequence, so
we can extend [`length()`](@ref), too.

Now, when we ask Julia to [`collect()`](@ref) all the elements into an array it can preallocate a `Vector{Int}`
of the right size instead of blindly [`push!`](@ref)ing each element into a `Vector{Any}`:

```jldoctest squaretype
julia> collect(Squares(10))' # transposed to save space
1×10 RowVector{Int64,Array{Int64,1}}:
 1  4  9  16  25  36  49  64  81  100
```

While we can rely upon generic implementations, we can also extend specific methods where we know
there is a simpler algorithm. For example, there's a formula to compute the sum of squares, so
we can override the generic iterative version with a more performant solution:

```jldoctest squaretype
julia> Base.sum(S::Squares) = (n = S.count; return n*(n+1)*(2n+1)÷6)

julia> sum(Squares(1803))
1955361914
```

This is a very common pattern throughout the Julia standard library: a small set of required methods
define an informal interface that enable many fancier behaviors. In some cases, types will want
to additionally specialize those extra behaviors when they know a more efficient algorithm can
be used in their specific case.

## Indexing

| Methods to implement | Brief description                |
|:-------------------- |:-------------------------------- |
| `getindex(X, i)`     | `X[i]`, indexed element access   |
| `setindex!(X, v, i)` | `X[i] = v`, indexed assignment   |
| `endof(X)`           | The last index, used in `X[end]` |

For the `Squares` iterable above, we can easily compute the `i`th element of the sequence by squaring
it.  We can expose this as an indexing expression `S[i]`. To opt into this behavior, `Squares`
simply needs to define [`getindex()`](@ref):

```jldoctest squaretype
julia> function Base.getindex(S::Squares, i::Int)
           1 <= i <= S.count || throw(BoundsError(S, i))
           return i*i
       end

julia> Squares(100)[23]
529
```

Additionally, to support the syntax `S[end]`, we must define [`endof()`](@ref) to specify the last valid
index:

```jldoctest squaretype
julia> Base.endof(S::Squares) = length(S)

julia> Squares(23)[end]
529
```

Note, though, that the above *only* defines [`getindex()`](@ref) with one integer index. Indexing with
anything other than an `Int` will throw a [`MethodError`](@ref) saying that there was no matching method.
In order to support indexing with ranges or vectors of `Int`s, separate methods must be written:

```jldoctest squaretype
julia> Base.getindex(S::Squares, i::Number) = S[convert(Int, i)]

julia> Base.getindex(S::Squares, I) = [S[i] for i in I]

julia> Squares(10)[[3,4.,5]]
3-element Array{Int64,1}:
  9
 16
 25
```

While this is starting to support more of the [indexing operations supported by some of the builtin types](@ref man-array-indexing),
there's still quite a number of behaviors missing. This `Squares` sequence is starting to look
more and more like a vector as we've added behaviors to it. Instead of defining all these behaviors
ourselves, we can officially define it as a subtype of an `AbstractArray`.

## [Abstract Arrays](@id man-interface-array)

| Methods to implement                            |                                          | Brief description                                                                     |
|:----------------------------------------------- |:---------------------------------------- |:------------------------------------------------------------------------------------- |
| `size(A)`                                       |                                          | Returns a tuple containing the dimensions of `A`                                      |
| `getindex(A, i::Int)`                           |                                          | (if `LinearFast`) Linear scalar indexing                                              |
| `getindex(A, I::Vararg{Int, N})`                |                                          | (if `LinearSlow`, where `N = ndims(A)`) N-dimensional scalar indexing                 |
| `setindex!(A, v, i::Int)`                       |                                          | (if `LinearFast`) Scalar indexed assignment                                           |
| `setindex!(A, v, I::Vararg{Int, N})`            |                                          | (if `LinearSlow`, where `N = ndims(A)`) N-dimensional scalar indexed assignment       |
| **Optional methods**                            | **Default definition**                   | **Brief description**                                                                 |
| `Base.linearindexing(::Type)`                   | `Base.LinearSlow()`                      | Returns either `Base.LinearFast()` or `Base.LinearSlow()`. See the description below. |
| `getindex(A, I...)`                             | defined in terms of scalar `getindex()`  | [Multidimensional and nonscalar indexing](@ref man-array-indexing)                    |
| `setindex!(A, I...)`                            | defined in terms of scalar `setindex!()` | [Multidimensional and nonscalar indexed assignment](@ref man-array-indexing)          |
| `start()`/`next()`/`done()`                     | defined in terms of scalar `getindex()`  | Iteration                                                                             |
| `length(A)`                                     | `prod(size(A))`                          | Number of elements                                                                    |
| `similar(A)`                                    | `similar(A, eltype(A), size(A))`         | Return a mutable array with the same shape and element type                           |
| `similar(A, ::Type{S})`                         | `similar(A, S, size(A))`                 | Return a mutable array with the same shape and the specified element type             |
| `similar(A, dims::NTuple{Int})`                 | `similar(A, eltype(A), dims)`            | Return a mutable array with the same element type and size *dims*                     |
| `similar(A, ::Type{S}, dims::NTuple{Int})`      | `Array{S}(dims)`                         | Return a mutable array with the specified element type and size                       |
| **Non-traditional indices**                     | **Default definition**                   | **Brief description**                                                                 |
| `indices(A)`                                    | `map(OneTo, size(A))`                    | Return the `AbstractUnitRange` of valid indices                                       |
| `Base.similar(A, ::Type{S}, inds::NTuple{Ind})` | `similar(A, S, Base.to_shape(inds))`     | Return a mutable array with the specified indices `inds` (see below)                  |
| `Base.similar(T::Union{Type,Function}, inds)`   | `T(Base.to_shape(inds))`                 | Return an array similar to `T` with the specified indices `inds` (see below)          |

If a type is defined as a subtype of `AbstractArray`, it inherits a very large set of rich behaviors
including iteration and multidimensional indexing built on top of single-element access.  See
the [arrays manual page](@ref man-multi-dim-arrays) and [standard library section](@ref lib-arrays) for more supported methods.

A key part in defining an `AbstractArray` subtype is [`Base.linearindexing()`](@ref). Since indexing is
such an important part of an array and often occurs in hot loops, it's important to make both
indexing and indexed assignment as efficient as possible.  Array data structures are typically
defined in one of two ways: either it most efficiently accesses its elements using just one index
(linear indexing) or it intrinsically accesses the elements with indices specified for every dimension.
 These two modalities are identified by Julia as `Base.LinearFast()` and `Base.LinearSlow()`.
 Converting a linear index to multiple indexing subscripts is typically very expensive, so this
provides a traits-based mechanism to enable efficient generic code for all array types.

This distinction determines which scalar indexing methods the type must define. `LinearFast()`
arrays are simple: just define `getindex(A::ArrayType, i::Int)`.  When the array is subsequently
indexed with a multidimensional set of indices, the fallback `getindex(A::AbstractArray, I...)()`
efficiently converts the indices into one linear index and then calls the above method. `LinearSlow()`
arrays, on the other hand, require methods to be defined for each supported dimensionality with
`ndims(A)``Int` indices.  For example, the builtin `SparseMatrixCSC` type only supports two dimensions,
so it just defines `getindex(A::SparseMatrixCSC, i::Int, j::Int)()`.  The same holds for `setindex!()`.

Returning to the sequence of squares from above, we could instead define it as a subtype of an
`AbstractArray{Int, 1}`:

```jldoctest squarevectype
julia> immutable SquaresVector <: AbstractArray{Int, 1}
           count::Int
       end

julia> Base.size(S::SquaresVector) = (S.count,)

julia> Base.linearindexing{T<:SquaresVector}(::Type{T}) = Base.LinearFast()

julia> Base.getindex(S::SquaresVector, i::Int) = i*i
```

Note that it's very important to specify the two parameters of the `AbstractArray`; the first
defines the [`eltype()`](@ref), and the second defines the [`ndims()`](@ref). That supertype and those three
methods are all it takes for `SquaresVector` to be an iterable, indexable, and completely functional
array:

```jldoctest squarevectype
julia> s = SquaresVector(7)
7-element SquaresVector:
  1
  4
  9
 16
 25
 36
 49

julia> s[s .> 20]
3-element Array{Int64,1}:
 25
 36
 49

julia> s \ [1 2; 3 4; 5 6; 7 8; 9 10; 11 12; 13 14]
1×2 Array{Float64,2}:
 0.305389  0.335329

julia> s ⋅ s # dot(s, s)
4676
```

As a more complicated example, let's define our own toy N-dimensional sparse-like array type built
on top of [`Dict`](@ref):

```jldoctest squarevectype
julia> immutable SparseArray{T,N} <: AbstractArray{T,N}
           data::Dict{NTuple{N,Int}, T}
           dims::NTuple{N,Int}
       end

julia> SparseArray{T}(::Type{T}, dims::Int...) = SparseArray(T, dims);

julia> SparseArray{T,N}(::Type{T}, dims::NTuple{N,Int}) = SparseArray{T,N}(Dict{NTuple{N,Int}, T}(), dims);

julia> Base.size(A::SparseArray) = A.dims

julia> Base.similar{T}(A::SparseArray, ::Type{T}, dims::Dims) = SparseArray(T, dims)

julia> Base.getindex{T,N}(A::SparseArray{T,N}, I::Vararg{Int,N}) = get(A.data, I, zero(T))

julia> Base.setindex!{T,N}(A::SparseArray{T,N}, v, I::Vararg{Int,N}) = (A.data[I] = v)
```

Notice that this is a `LinearSlow` array, so we must manually define [`getindex()`](@ref) and [`setindex!()`](@ref)
at the dimensionality of the array. Unlike the `SquaresVector`, we are able to define [`setindex!()`](@ref),
and so we can mutate the array:

```jldoctest squarevectype
julia> A = SparseArray(Float64, 3, 3)
3×3 SparseArray{Float64,2}:
 0.0  0.0  0.0
 0.0  0.0  0.0
 0.0  0.0  0.0

julia> fill!(A, 2)
3×3 SparseArray{Float64,2}:
 2.0  2.0  2.0
 2.0  2.0  2.0
 2.0  2.0  2.0

julia> A[:] = 1:length(A); A
3×3 SparseArray{Float64,2}:
 1.0  4.0  7.0
 2.0  5.0  8.0
 3.0  6.0  9.0
```

The result of indexing an `AbstractArray` can itself be an array (for instance when indexing by
a `Range`). The `AbstractArray` fallback methods use [`similar()`](@ref) to allocate an `Array` of the
appropriate size and element type, which is filled in using the basic indexing method described
above. However, when implementing an array wrapper you often want the result to be wrapped as
well:

```jldoctest squarevectype
julia> A[1:2,:]
2×3 SparseArray{Float64,2}:
 1.0  4.0  7.0
 2.0  5.0  8.0
```

In this example it is accomplished by defining `Base.similar{T}(A::SparseArray, ::Type{T}, dims::Dims)`
to create the appropriate wrapped array. (Note that while `similar` supports 1- and 2-argument
forms, in most case you only need to specialize the 3-argument form.) For this to work it's important
that `SparseArray` is mutable (supports `setindex!`). Defining `similar()`, `getindex()` and
`setindex!()` for `SparseArray` also makes it possible to [`copy()`](@ref) the array:

```jldoctest squarevectype
julia> copy(A)
3×3 SparseArray{Float64,2}:
 1.0  4.0  7.0
 2.0  5.0  8.0
 3.0  6.0  9.0
```

In addition to all the iterable and indexable methods from above, these types can also interact
with each other and use most of the methods defined in the standard library for `AbstractArrays`:

```jldoctest squarevectype
julia> A[SquaresVector(3)]
3-element SparseArray{Float64,1}:
 1.0
 4.0
 9.0

julia> dot(A[:,1],A[:,2])
32.0
```

If you are defining an array type that allows non-traditional indexing (indices that start at
something other than 1), you should specialize `indices`. You should also specialize [`similar`](@ref)
so that the `dims` argument (ordinarily a `Dims` size-tuple) can accept `AbstractUnitRange` objects,
perhaps range-types `Ind` of your own design. For more information, see [Arrays with custom indices](@ref).

