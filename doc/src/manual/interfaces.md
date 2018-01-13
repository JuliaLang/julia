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
| `IteratorSize(IterType)`       | `HasLength()`          | One of `HasLength()`, `HasShape()`, `IsInfinite()`, or `SizeUnknown()` as appropriate |
| `IteratorEltype(IterType)`     | `HasEltype()`          | Either `EltypeUnknown()` or `HasEltype()` as appropriate                              |
| `eltype(IterType)`             | `Any`                  | The type of the items returned by `next()`                                            |
| `length(iter)`                 | (*undefined*)          | The number of items, if known                                                         |
| `size(iter, [dim...])`         | (*undefined*)          | The number of items in each dimension, if known                                       |

| Value returned by `IteratorSize(IterType)` | Required Methods                           |
|:------------------------------------------ |:------------------------------------------ |
| `HasLength()`                              | `length(iter)`                             |
| `HasShape()`                               | `length(iter)`  and `size(iter, [dim...])` |
| `IsInfinite()`                             | (*none*)                                   |
| `SizeUnknown()`                            | (*none*)                                   |

| Value returned by `IteratorEltype(IterType)` | Required Methods   |
|:-------------------------------------------- |:------------------ |
| `HasEltype()`                                | `eltype(IterType)` |
| `EltypeUnknown()`                            | (*none*)           |

Sequential iteration is implemented by the methods [`start`](@ref), [`done`](@ref), and [`next`](@ref). Instead
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
julia> struct Squares
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

We can use many of the builtin methods that work with iterables, like [`in`](@ref), [`mean`](@ref) and [`std`](@ref):

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
the [`eltype`](@ref) method, we can give that information to Julia and help it make more specialized
code in the more complicated methods. We also know the number of elements in our sequence, so
we can extend [`length`](@ref), too.

Now, when we ask Julia to [`collect`](@ref) all the elements into an array it can preallocate a `Vector{Int}`
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

This is a very common pattern throughout Julia Base: a small set of required methods
define an informal interface that enable many fancier behaviors. In some cases, types will want
to additionally specialize those extra behaviors when they know a more efficient algorithm can
be used in their specific case.

It is also often useful to allow iteration over a collection in *reverse order*
by iterating over [`Iterators.reverse(iterator)`](@ref).  To actually support
reverse-order iteration, however, an iterator
type `T` needs to implement `start`, `next`, and `done` methods for `Iterators.Reverse{T}`.
(Given `r::Iterators.Reverse{T}`, the underling iterator of type `T` is `r.itr`.)
In our `Squares` example, we would implement `Iterators.Reverse{Squares}` methods:

```jldoctest squaretype
julia> Base.start(rS::Iterators.Reverse{Squares}) = rS.itr.count

julia> Base.next(::Iterators.Reverse{Squares}, state) = (state*state, state-1)

julia> Base.done(::Iterators.Reverse{Squares}, state) = state < 1

julia> collect(Iterators.reverse(Squares(10)))' # transposed to save space
1×10 RowVector{Int64,Array{Int64,1}}:
 100  81  64  49  36  25  16  9  4  1
```

## Indexing

| Methods to implement | Brief description                |
|:-------------------- |:-------------------------------- |
| `getindex(X, i)`     | `X[i]`, indexed element access   |
| `setindex!(X, v, i)` | `X[i] = v`, indexed assignment   |
| `endof(X)`           | The last index, used in `X[end]` |

For the `Squares` iterable above, we can easily compute the `i`th element of the sequence by squaring
it.  We can expose this as an indexing expression `S[i]`. To opt into this behavior, `Squares`
simply needs to define [`getindex`](@ref):

```jldoctest squaretype
julia> function Base.getindex(S::Squares, i::Int)
           1 <= i <= S.count || throw(BoundsError(S, i))
           return i*i
       end

julia> Squares(100)[23]
529
```

Additionally, to support the syntax `S[end]`, we must define [`endof`](@ref) to specify the last valid
index:

```jldoctest squaretype
julia> Base.endof(S::Squares) = length(S)

julia> Squares(23)[end]
529
```

Note, though, that the above *only* defines [`getindex`](@ref) with one integer index. Indexing with
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
ourselves, we can officially define it as a subtype of an [`AbstractArray`](@ref).

## [Abstract Arrays](@id man-interface-array)

| Methods to implement                            |                                        | Brief description                                                                     |
|:----------------------------------------------- |:-------------------------------------- |:------------------------------------------------------------------------------------- |
| `size(A)`                                       |                                        | Returns a tuple containing the dimensions of `A`                                      |
| `getindex(A, i::Int)`                           |                                        | (if `IndexLinear`) Linear scalar indexing                                             |
| `getindex(A, I::Vararg{Int, N})`                |                                        | (if `IndexCartesian`, where `N = ndims(A)`) N-dimensional scalar indexing             |
| `setindex!(A, v, i::Int)`                       |                                        | (if `IndexLinear`) Scalar indexed assignment                                          |
| `setindex!(A, v, I::Vararg{Int, N})`            |                                        | (if `IndexCartesian`, where `N = ndims(A)`) N-dimensional scalar indexed assignment   |
| **Optional methods**                            | **Default definition**                 | **Brief description**                                                                 |
| `IndexStyle(::Type)`                            | `IndexCartesian()`                     | Returns either `IndexLinear()` or `IndexCartesian()`. See the description below.      |
| `getindex(A, I...)`                             | defined in terms of scalar `getindex`  | [Multidimensional and nonscalar indexing](@ref man-array-indexing)                    |
| `setindex!(A, I...)`                            | defined in terms of scalar `setindex!` | [Multidimensional and nonscalar indexed assignment](@ref man-array-indexing)          |
| `start`/`next`/`done`                           | defined in terms of scalar `getindex`  | Iteration                                                                             |
| `length(A)`                                     | `prod(size(A))`                        | Number of elements                                                                    |
| `similar(A)`                                    | `similar(A, eltype(A), size(A))`       | Return a mutable array with the same shape and element type                           |
| `similar(A, ::Type{S})`                         | `similar(A, S, size(A))`               | Return a mutable array with the same shape and the specified element type             |
| `similar(A, dims::NTuple{Int})`                 | `similar(A, eltype(A), dims)`          | Return a mutable array with the same element type and size *dims*                     |
| `similar(A, ::Type{S}, dims::NTuple{Int})`      | `Array{S}(uninitialized, dims)`        | Return a mutable array with the specified element type and size                       |
| **Non-traditional indices**                     | **Default definition**                 | **Brief description**                                                                 |
| `axes(A)`                                    | `map(OneTo, size(A))`                  | Return the `AbstractUnitRange` of valid indices                                       |
| `Base.similar(A, ::Type{S}, inds::NTuple{Ind})` | `similar(A, S, Base.to_shape(inds))`   | Return a mutable array with the specified indices `inds` (see below)                  |
| `Base.similar(T::Union{Type,Function}, inds)`   | `T(Base.to_shape(inds))`               | Return an array similar to `T` with the specified indices `inds` (see below)          |

If a type is defined as a subtype of `AbstractArray`, it inherits a very large set of rich behaviors
including iteration and multidimensional indexing built on top of single-element access.  See
the [arrays manual page](@ref man-multi-dim-arrays) and the [Julia Base section](@ref lib-arrays) for more supported methods.

A key part in defining an `AbstractArray` subtype is [`IndexStyle`](@ref). Since indexing is
such an important part of an array and often occurs in hot loops, it's important to make both
indexing and indexed assignment as efficient as possible.  Array data structures are typically
defined in one of two ways: either it most efficiently accesses its elements using just one index
(linear indexing) or it intrinsically accesses the elements with indices specified for every dimension.
 These two modalities are identified by Julia as `IndexLinear()` and `IndexCartesian()`.
 Converting a linear index to multiple indexing subscripts is typically very expensive, so this
provides a traits-based mechanism to enable efficient generic code for all array types.

This distinction determines which scalar indexing methods the type must define. `IndexLinear()`
arrays are simple: just define `getindex(A::ArrayType, i::Int)`.  When the array is subsequently
indexed with a multidimensional set of indices, the fallback `getindex(A::AbstractArray, I...)()`
efficiently converts the indices into one linear index and then calls the above method. `IndexCartesian()`
arrays, on the other hand, require methods to be defined for each supported dimensionality with
`ndims(A)` `Int` indices. For example, the built-in [`SparseMatrixCSC`](@ref) type only
supports two dimensions, so it just defines
`getindex(A::SparseMatrixCSC, i::Int, j::Int)`. The same holds for `setindex!`.

Returning to the sequence of squares from above, we could instead define it as a subtype of an
`AbstractArray{Int, 1}`:

```jldoctest squarevectype
julia> struct SquaresVector <: AbstractArray{Int, 1}
           count::Int
       end

julia> Base.size(S::SquaresVector) = (S.count,)

julia> Base.IndexStyle(::Type{<:SquaresVector}) = IndexLinear()

julia> Base.getindex(S::SquaresVector, i::Int) = i*i
```

Note that it's very important to specify the two parameters of the `AbstractArray`; the first
defines the [`eltype`](@ref), and the second defines the [`ndims`](@ref). That supertype and those three
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
1×2 RowVector{Float64,Array{Float64,1}}:
 0.305389  0.335329

julia> s ⋅ s # dot(s, s)
4676
```

As a more complicated example, let's define our own toy N-dimensional sparse-like array type built
on top of [`Dict`](@ref):

```jldoctest squarevectype
julia> struct SparseArray{T,N} <: AbstractArray{T,N}
           data::Dict{NTuple{N,Int}, T}
           dims::NTuple{N,Int}
       end

julia> SparseArray(::Type{T}, dims::Int...) where {T} = SparseArray(T, dims);

julia> SparseArray(::Type{T}, dims::NTuple{N,Int}) where {T,N} = SparseArray{T,N}(Dict{NTuple{N,Int}, T}(), dims);

julia> Base.size(A::SparseArray) = A.dims

julia> Base.similar(A::SparseArray, ::Type{T}, dims::Dims) where {T} = SparseArray(T, dims)

julia> Base.getindex(A::SparseArray{T,N}, I::Vararg{Int,N}) where {T,N} = get(A.data, I, zero(T))

julia> Base.setindex!(A::SparseArray{T,N}, v, I::Vararg{Int,N}) where {T,N} = (A.data[I] = v)
```

Notice that this is an `IndexCartesian` array, so we must manually define [`getindex`](@ref) and [`setindex!`](@ref)
at the dimensionality of the array. Unlike the `SquaresVector`, we are able to define [`setindex!`](@ref),
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
an `AbstractRange`). The `AbstractArray` fallback methods use [`similar`](@ref) to allocate an `Array`
of the appropriate size and element type, which is filled in using the basic indexing method described
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
that `SparseArray` is mutable (supports `setindex!`). Defining `similar`, `getindex` and
`setindex!` for `SparseArray` also makes it possible to [`copy`](@ref) the array:

```jldoctest squarevectype
julia> copy(A)
3×3 SparseArray{Float64,2}:
 1.0  4.0  7.0
 2.0  5.0  8.0
 3.0  6.0  9.0
```

In addition to all the iterable and indexable methods from above, these types can also interact
with each other and use most of the methods defined in Julia Base for `AbstractArrays`:

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

## [Strided Arrays]

| Methods to implement                            |                                        | Brief description                                                                     |
|:----------------------------------------------- |:-------------------------------------- |:------------------------------------------------------------------------------------- |
| `strides(A)`                             |                                        | Return the distance in memory (in number of elements) between adjacent elements in each dimension as a tuple. If `A` is an `AbstractArray{T,0}`, this should return an empty tuple.    |
| `Base.unsafe_convert(::Type{Ptr{T}}, A)`        |                                        | Return the native address of an array.                                            |
| **Optional methods**                            | **Default definition**                 | **Brief description**                                                                 |
| `stride(A, i::Int)`                             |     `strides(A)[i]`                                   | Return the distance in memory (in number of elements) between adjacent elements in dimension k.    |

A strided array is a subtype of `AbstractArray` whose entries are stored in memory with fixed strides.
Provided the element type of the array is compatible with BLAS, a strided array can utilize BLAS and LAPACK routines
for more efficient linear algebra routines.  A typical example of a user-defined strided array is one
that wraps a standard `Array` with additional structure.

Warning: do not implement these methods if the underlying storage is not actually strided, as it
may lead to incorrect results or segmentation faults.

Here are some examples to demonstrate which type of arrays are strided and which are not:
```julia
1:5   # not strided (there is no storage associated with this array.)
Vector(1:5)  # is strided with strides (1,)
A = [1 5; 2 6; 3 7; 4 8]  # is strided with strides (1,4)
V = view(A, 1:2, :)   # is strided with strides (1,4)
V = view(A, 1:2:3, 1:2)   # is strided with strides (2,4)
V = view(A, [1,2,4], :)   # is not strided, as the spacing between rows is not fixed.
```





## [Customizing broadcasting](@id man-interfaces-broadcasting)

| Methods to implement | Brief description |
|:-------------------- |:----------------- |
| `Base.BroadcastStyle(::Type{SrcType}) = SrcStyle()` | Broadcasting behavior of `SrcType` |
| `Base.broadcast_similar(::DestStyle, ::Type{ElType}, inds, bc)` | Allocation of output container |
| **Optional methods** | | |
| `Base.BroadcastStyle(::Style1, ::Style2) = Style12()` | Precedence rules for mixing styles |
| `Base.broadcast_indices(::StyleA, A)` | Declaration of the indices of `A` for broadcasting purposes (for AbstractArrays, defaults to `axes(A)`) |
| **Bypassing default machinery** | |
| `Base.copy(bc::Broadcasted{DestStyle})` | Custom implementation of `broadcast` |
| `Base.copyto!(dest, bc::Broadcasted{DestStyle})` | Custom implementation of `broadcast!`, specializing on `DestStyle` |
| `Base.copyto!(dest::DestType, bc::Broadcasted{Nothing})` | Custom implementation of `broadcast!`, specializing on `DestType` |
| `Base.is_broadcast_incremental(bc::Broadcasted{DestStyle})` | Indicate that nested broadcasting should be implemented eagerly |
| `Base.broadcast_skip_axes_instantiation(::Broadcasted{DestStyle})` | Define to return `true` if `DestStyle` doesn't benefit from computing the axes of the output |

[Broadcasting](@ref) is represented by explicit calls to `broadcast` or `broadcast!`, or implicit
"dot" operations like `A .+ b`. By default, all `AbstractArray`s support broadcasting operations
through built-in generic implementations, but there are a number of ways in which custom arrays can
customize and specialize the behavior of broadcasting in order to improve and optimize the
operation.

In general, a broadcast operation is represented by a lazy `Broadcasted` container that holds onto
the function to be applied alongside its arguments. Those arguments may themselves be more nested
`Broadcasted` containers, forming a large expression tree to be evaluated. A nested tree of
`Broadcasted` containers is directly constructed by the implicit dot syntax; `5 .+ 2.*x` is
transiently represented by `Broadcasted(+, 5, Broadcasted(*, 2, x))`, for example. This is
invisible to users as it is immediately realized through a call to `copy`, but it is this container
that provides the basis for broadcast's extensibility for authors of custom types. The built-in
broadcast machinery will then determine the result type and size based upon the arguments, allocate
it, and then finally copy the realization of the `Broadcasted` object into it with a default
`copyto!(::AbstractArray, ::Broadcasted)` method. The built-in fallback `broadcast` and
`broadcast!` methods similarly construct a transient `Broadcasted` representation of the operation
so they can follow the same codepath. This allows custom array implementations to
[provide their own `copyto!` specialization](@ref extending-in-place-broadcast) to customize and
optimize broadcasting. In order to get to that point, though, custom arrays must first signal the
fact that they should return a custom array from the broadcast operation.

### Customizing the broadcast result type

All `AbstractArray`s support broadcasting in arbitrary combinations with one another, but the
default result (output) type is `Array`. The `Broadcasted` container has a dedicated type parameter
— `Broadcasted{DestStyle}` — specifically to allow for dispatch and specialization. It computes
this "broadcast style" by recursively asking every argument for its `Base.BroadcastStyle` and
[combining them together with a promotion-like computation](@ref writing-binary-broadcasting-rules).

`Base.BroadcastStyle` is an abstract type from which all styles are derived. When used as a
function it has two possible forms, unary (single-argument) and binary. The unary variant states
that you intend to implement specific broadcasting behavior and/or output type, and do not wish to
rely on the default fallback ([`Broadcast.Scalar`](@ref) or [`Broadcast.DefaultArrayStyle`](@ref)).
To achieve this, you can define a custom `BroadcastStyle` for your object:

```julia
struct MyStyle <: Broadcast.BroadcastStyle end
Base.BroadcastStyle(::Type{<:MyType}) = MyStyle()
```

In some cases it might be convenient not to have to define `MyStyle`, in which case you can
leverage one of the general broadcast wrappers:

  - `Base.BroadcastStyle(::Type{<:MyType}) = Broadcast.Style{MyType}()` can be
    used for arbitrary types.
  - `Base.BroadcastStyle(::Type{<:MyType}) = Broadcast.ArrayStyle{MyType}()` is preferred
    if `MyType` is an `AbstractArray`.
  - For `AbstractArrays` that only support a certain dimensionality, create a subtype of `Broadcast.AbstractArrayStyle{N}` (see below).

When your broadcast operation involves several arguments, individual argument styles get
combined to determine a single `DestStyle` that controls the type of the output container.
For more detail, see [below](@ref writing-binary-broadcasting-rules).

The actual allocation of the result array is handled by `Base.broadcast_similar`:

```julia
Base.broadcast_similar(::DestStyle, ::Type{ElType}, inds, bc)
```

`DestStyle` signals the final result from combining the input styles.
The fallback definition is

```julia
broadcast_similar(::DefaultArrayStyle{N}, ::Type{ElType}, inds::Indices{N}, bc) where {N,ElType} =
    similar(Array{ElType}, inds)
```

However, if needed you can specialize on any or all of these arguments.
`bc` is the overall `Broadcasted` wrapper, available in case allocation of the output requires
access to some of the inputs. For these purposes, the important field of `Broadcasted` is called
`args`, which stores the inputs as a linked list (a `TupleLL`). `ll.head` extracts the first
element, while `ll.rest` retrieves the remaining list. The list is terminated by a `TupleLLEnd()`.

For a complete example, let's say you have created a type, `ArrayAndChar`, that stores an
array and a single character:

```jldoctest
struct ArrayAndChar{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
    char::Char
end
Base.size(A::ArrayAndChar) = size(A.data)
Base.getindex(A::ArrayAndChar{T,N}, inds::Vararg{Int,N}) where {T,N} = A.data[inds...]
Base.setindex!(A::ArrayAndChar{T,N}, val, inds::Vararg{Int,N}) where {T,N} = A.data[inds...] = val
Base.showarg(io::IO, A::ArrayAndChar, toplevel) = print(io, typeof(A), " with char '", A.char, "'")
```

You might want broadcasting to preserve the `char` "metadata." First we define

```jldoctest
Base.BroadcastStyle(::Type{<:ArrayAndChar}) = Broadcast.ArrayStyle{ArrayAndChar}()
```

This means we must also define a corresponding `broadcast_similar` method:
```jldoctest
function Base.broadcast_similar(::Broadcast.ArrayStyle{ArrayAndChar}, ::Type{ElType}, inds, bc) where ElType
    # Scan the inputs for the ArrayAndChar:
    A = find_aac(bc)
    # Use the char field of A to create the output
    ArrayAndChar(similar(Array{ElType}, inds), A.char)
end

"`A = find_aac(As)` returns the first ArrayAndChar among the arguments."
find_aac(bc::Base.Broadcast.Broadcasted) = find_aac(bc.args)
find_aac(ll::Base.TupleLL) = find_aac(find_aac(ll.head), ll.rest)
find_aac(x) = x
find_aac(a::ArrayAndChar, rest) = a
find_aac(::Any, rest) = find_aac(rest)
```

From these definitions, one obtains the following behavior:
```jldoctest
julia> a = ArrayAndChar([1 2; 3 4], 'x')
2×2 ArrayAndChar{Int64,2} with char 'x':
 1  2
 3  4

julia> a .+ 1
2×2 ArrayAndChar{Int64,2} with char 'x':
 2  3
 4  5

julia> a .+ [5,10]
2×2 ArrayAndChar{Int64,2} with char 'x':
  6   7
 13  14
```

## Eager evaluation of nested broadcasting

For some types, the machinery to "fuse" operations across nested levels of broadcasting
is not available. In such cases, you may need to evaluate `x .* (x .+ 1)` as if it had been
written `broadcast(*, x, broadcast(+, x, 1))`, where the inner operation is evaluated before
tackling the outer operation. You can force eager evaluation by defining

```julia
is_broadcast_incremental(bc::Broadcasted{DestStyle}) = true
```
In such cases you need to supply specific methods
```julia
broadcast(f, arg1::ArgType1, ...)
```
for all operations that might be triggered, otherwise the result will be circular and a
`StackOverflowError` will result.

Your definition of `is_broadcast_incremental` can be more sophisticated, if necessary;
in particular, you can examine the types of `bc.args` if you need to make a more nuanced decision.
As an example, here is the implementation that allows Julia to return `AbstractRange` objects
from broadcasting:

```julia
is_broadcast_incremental(bc::Broadcasted{DefaultArrayStyle{1}}) = maybe_range_safe(bc)

# Support incremental evaluation only for 1- or 2-argument broadcasting
# Broadcast.broadcast_all(f_filter, arg_filter, bc) is a function that checks all
# inputs to a nested broadcasting operation, ensuring that the function `f` and
# arguments return `true` for their respective filter functions.
const Args1{T} = TupleLL{T,TupleLLEnd}
const Args2{S,T} = TupleLL{S,TupleLL{T,TupleLLEnd}}
@inline maybe_range_safe(bc::Broadcasted{Style}) where {Style<:AbstractArrayStyle{1}} =
    Broadcast.broadcast_all(maybe_range_safe_f, maybe_range_safe_arg, bc) && bc.args isa Union{Args1,Args2}

# Support incremental evaluation only for operations that might return an AbstractRange
maybe_range_safe_f(::typeof(+)) = true
maybe_range_safe_f(::typeof(-)) = true
maybe_range_safe_f(::typeof(*)) = true
maybe_range_safe_f(::typeof(/)) = true
maybe_range_safe_f(::typeof(\)) = true
maybe_range_safe_f(f)           = false

maybe_range_safe_arg(::AbstractRange) = true
maybe_range_safe_arg(::Number)        = true
maybe_range_safe_arg(x)               = false
```

It's then necessary to write `broadcast` methods for all 1- and 2-argument versions of operations
involving at least one `AbstractRange` and the supported operations `+`, `-`, `*`, `/`, and `\`.
For example,

```julia
broadcast(::typeof(-), r::OrdinalRange) = range(-first(r), -step(r), length(r))
```
to define negation of a range.

### [Writing binary broadcasting rules](@id writing-binary-broadcasting-rules)

The precedence rules are defined by binary `BroadcastStyle` calls:

```julia
Base.BroadcastStyle(::Style1, ::Style2) = Style12()
```

where `Style12` is the `BroadcastStyle` you want to choose for outputs involving
arguments of `Style1` and `Style2`. For example,

```julia
Base.BroadcastStyle(::Broadcast.Style{Tuple}, ::Broadcast.Scalar) = Broadcast.Style{Tuple}()
```

indicates that `Tuple` "wins" over scalars (the output container will be a tuple).
It is worth noting that you do not need to (and should not) define both argument orders
of this call; defining one is sufficient no matter what order the user supplies the arguments in.

For `AbstractArray` types, defining a `BroadcastStyle` supersedes the fallback choice,
[`Broadcast.DefaultArrayStyle`](@ref). `DefaultArrayStyle` and the abstract supertype, `AbstractArrayStyle`, store the dimensionality as a type parameter to support specialized
array types that have fixed dimensionality requirements.

`DefaultArrayStyle` "loses" to any other
`AbstractArrayStyle` that has been defined because of the following methods:

```julia
BroadcastStyle(a::AbstractArrayStyle{Any}, ::DefaultArrayStyle) = a
BroadcastStyle(a::AbstractArrayStyle{N}, ::DefaultArrayStyle{N}) where N = a
BroadcastStyle(a::AbstractArrayStyle{M}, ::DefaultArrayStyle{N}) where {M,N} =
    typeof(a)(_max(Val(M),Val(N)))
```

You do not need to write binary `BroadcastStyle`
rules unless you want to establish precedence for
two or more non-`DefaultArrayStyle` types.

If your array type does have fixed dimensionality requirements, then you should
subtype `AbstractArrayStyle`. For example, the sparse array code has the following definitions:

```julia
struct SparseVecStyle <: Broadcast.AbstractArrayStyle{1} end
struct SparseMatStyle <: Broadcast.AbstractArrayStyle{2} end
Base.BroadcastStyle(::Type{<:SparseVector}) = SparseVecStyle()
Base.BroadcastStyle(::Type{<:SparseMatrixCSC}) = SparseMatStyle()
```

Whenever you subtype `AbstractArrayStyle`, you also need to define rules for combining
dimensionalities, by creating a constructor for your style that takes a `Val(N)` argument.
For example:

```julia
SparseVecStyle(::Val{0}) = SparseVecStyle()
SparseVecStyle(::Val{1}) = SparseVecStyle()
SparseVecStyle(::Val{2}) = SparseMatStyle()
SparseVecStyle(::Val{N}) where N = Broadcast.DefaultArrayStyle{N}()
```

These rules indicate that the combination of a `SparseVecStyle` with 0- or 1-dimensional arrays
yields another `SparseVecStyle`, that its combination with a 2-dimensional array
yields a `SparseMatStyle`, and anything of higher dimensionality falls back to the dense arbitrary-dimensional framework.
These rules allow broadcasting to keep the sparse representation for operations that result
in one or two dimensional outputs, but produce an `Array` for any other dimensionality.

### [Extending in-place broadcasting](@id extending-in-place-broadcast)

In-place broadcasting can be supported by defining the appropriate `copyto!(dest, bc::Broadcasted)`
method. Because you might want to specialize either on `dest` or the specific subtype of `bc`,
to avoid ambiguities between packages we recommend the following convention.

If you wish to specialize on a particular style `DestStyle`, define a method for
```julia
copyto!(dest, bc::Broadcasted{DestStyle})
```
Optionally, with this form you can also specialize on the type of `dest`.

If instead you want to specialize on the destination type `DestType` without specializing
on `DestStyle`, then you should define a method with the following signature:

```julia
copyto!(dest::DestType, bc::Broadcasted{Nothing})
```

This leverages a fallback implementation of `copyto!` that converts the wrapper into a
`Broadcasted{Nothing}`. Consequently, specializing on `DestType` has lower precedence than
methods that specialize on `DestStyle`.
