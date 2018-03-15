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
| `IteratorSize(IterType)`       | `HasLength()`          | One of `HasLength()`, `HasShape{N}()`, `IsInfinite()`, or `SizeUnknown()` as appropriate |
| `IteratorEltype(IterType)`     | `HasEltype()`          | Either `EltypeUnknown()` or `HasEltype()` as appropriate                              |
| `eltype(IterType)`             | `Any`                  | The type of the items returned by `next()`                                            |
| `length(iter)`                 | (*undefined*)          | The number of items, if known                                                         |
| `size(iter, [dim...])`         | (*undefined*)          | The number of items in each dimension, if known                                       |

| Value returned by `IteratorSize(IterType)` | Required Methods                           |
|:------------------------------------------ |:------------------------------------------ |
| `HasLength()`                              | `length(iter)`                             |
| `HasShape{N}()`                            | `length(iter)`  and `size(iter, [dim...])` |
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

Any object that defines these three methods is iterable and can be used in the [many functions that rely upon iteration](@ref lib-collections-iteration).
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
julia> collect(Squares(4))
4-element Array{Int64,1}:
  1
  4
  9
 16
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

julia> collect(Iterators.reverse(Squares(4)))
4-element Array{Int64,1}:
 16
  9
  4
  1
```

## Indexing

| Methods to implement | Brief description                |
|:-------------------- |:-------------------------------- |
| `getindex(X, i)`     | `X[i]`, indexed element access   |
| `setindex!(X, v, i)` | `X[i] = v`, indexed assignment   |
| `firstindex(X)`      | The first index                  |
| `lastindex(X)`        | The last index, used in `X[end]` |

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

Additionally, to support the syntax `S[end]`, we must define [`lastindex`](@ref) to specify the last
valid index. It is recommended to also define [`firstindex`](@ref) to specify the first valid index:

```jldoctest squaretype
julia> Base.firstindex(S::Squares) = 1

julia> Base.lastindex(S::Squares) = length(S)

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
| `similar(A, ::Type{S}, dims::NTuple{Int})`      | `Array{S}(undef, dims)`               | Return a mutable array with the specified element type and size                       |
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
`ndims(A)` `Int` indices. For example, [`SparseMatrixCSC`](@ref) from the `SparseArrays` standard
library module, only supports two dimensions, so it just defines
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
julia> s = SquaresVector(4)
4-element SquaresVector:
  1
  4
  9
 16

julia> s[s .> 8]
2-element Array{Int64,1}:
  9
 16

julia> s + s
4-element Array{Int64,1}:
  2
  8
 18
 32

julia> sin.(s)
4-element Array{Float64,1}:
  0.8414709848078965
 -0.7568024953079282
  0.4121184852417566
 -0.2879033166650653
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

julia> mean(A)
5.0
```

If you are defining an array type that allows non-traditional indexing (indices that start at
something other than 1), you should specialize `indices`. You should also specialize [`similar`](@ref)
so that the `dims` argument (ordinarily a `Dims` size-tuple) can accept `AbstractUnitRange` objects,
perhaps range-types `Ind` of your own design. For more information, see
[Arrays with custom indices](@ref man-custom-indices).

## [Strided Arrays](@id man-interface-strided-arrays)

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





## [Broadcasting](@id man-interfaces-broadcasting)

| Methods to implement | Brief description |
|:-------------------- |:----------------- |
| `Base.BroadcastStyle(::Type{SrcType}) = SrcStyle()` | Broadcasting behavior of `SrcType` |
| `Base.broadcast_similar(f, ::DestStyle, ::Type{ElType}, inds, As...)` | Allocation of output container |
| **Optional methods** | | |
| `Base.BroadcastStyle(::Style1, ::Style2) = Style12()` | Precedence rules for mixing styles |
| `Base.broadcast_indices(::StyleA, A)` | Declaration of the indices of `A` for broadcasting purposes (for AbstractArrays, defaults to `axes(A)`) |
| **Bypassing default machinery** | |
| `broadcast(f, As...)` | Complete bypass of broadcasting machinery |
| `broadcast(f, ::DestStyle, ::Nothing, ::Nothing, As...)` | Bypass after container type is computed |
| `broadcast(f, ::DestStyle, ::Type{ElType}, inds::Tuple, As...)` | Bypass after container type, eltype, and indices are computed |
| `broadcast!(f, dest::DestType, ::Nothing, As...)` | Bypass in-place broadcast, specialization on destination type |
| `broadcast!(f, dest, ::BroadcastStyle, As...)` | Bypass in-place broadcast, specialization on `BroadcastStyle` |

[Broadcasting](@ref) is triggered by an explicit call to `broadcast` or `broadcast!`, or implicitly by
"dot" operations like `A .+ b`. Any `AbstractArray` type supports broadcasting,
but the default result (output) type is `Array`. To specialize the result for specific input type(s),
the main task is the allocation of an appropriate result object.
(This is not an issue for `broadcast!`, where
the result object is passed as an argument.) This process is split into two stages: computation
of the behavior and type from the arguments ([`Base.BroadcastStyle`](@ref)), and allocation of the object
given the resulting type with [`Base.broadcast_similar`](@ref).

`Base.BroadcastStyle` is an abstract type from which all styles are
derived. When used as a function it has two possible forms,
unary (single-argument) and binary.
The unary variant states that you intend to
implement specific broadcasting behavior and/or output type,
and do not wish to rely on the default fallback ([`Broadcast.Scalar`](@ref) or [`Broadcast.DefaultArrayStyle`](@ref)).
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
Base.broadcast_similar(f, ::DestStyle, ::Type{ElType}, inds, As...)
```

`f` is the operation being performed and `DestStyle` signals the final result from
combining the input styles.
`As...` is the list of input objects. You may not need to use `f` or `As...`
unless they help you build the appropriate object; the fallback definition is

```julia
broadcast_similar(f, ::DefaultArrayStyle{N}, ::Type{ElType}, inds::Indices{N}, As...) where {N,ElType} =
    similar(Array{ElType}, inds)
```

However, if needed you can specialize on any or all of these arguments.

For a complete example, let's say you have created a type, `ArrayAndChar`, that stores an
array and a single character:

```jldoctest ArrayAndChar
struct ArrayAndChar{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
    char::Char
end
Base.size(A::ArrayAndChar) = size(A.data)
Base.getindex(A::ArrayAndChar{T,N}, inds::Vararg{Int,N}) where {T,N} = A.data[inds...]
Base.setindex!(A::ArrayAndChar{T,N}, val, inds::Vararg{Int,N}) where {T,N} = A.data[inds...] = val
Base.showarg(io::IO, A::ArrayAndChar, toplevel) = print(io, typeof(A), " with char '", A.char, "'")
# output

```

You might want broadcasting to preserve the `char` "metadata." First we define

```jldoctest ArrayAndChar
Base.BroadcastStyle(::Type{<:ArrayAndChar}) = Broadcast.ArrayStyle{ArrayAndChar}()
# output

```

This forces us to also define a `broadcast_similar` method:
```jldoctest ArrayAndChar; filter = r"(^find_aac \(generic function with 2 methods\)$|^$)"
function Base.broadcast_similar(f, ::Broadcast.ArrayStyle{ArrayAndChar}, ::Type{ElType}, inds, As...) where ElType
    # Scan the inputs for the ArrayAndChar:
    A = find_aac(As...)
    # Use the char field of A to create the output
    ArrayAndChar(similar(Array{ElType}, inds), A.char)
end

"`A = find_aac(As...)` returns the first ArrayAndChar among the arguments."
find_aac(A::ArrayAndChar, B...) = A
find_aac(A, B...) = find_aac(B...);
# output

```

From these definitions, one obtains the following behavior:
```jldoctest ArrayAndChar
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

Finally, it's worth noting that sometimes it's easier simply to bypass the machinery for
computing result types and container sizes, and just do everything manually. For example,
you can convert a `UnitRange{Int}` `r` to a `UnitRange{BigInt}` with `big.(r)`; the definition
of this method is approximately

```julia
Broadcast.broadcast(::typeof(big), r::UnitRange) = big(first(r)):big(last(r))
```

This exploits Julia's ability to dispatch on a particular function type. (This kind of
explicit definition can indeed be necessary if the output container does not support `setindex!`.)
You can optionally choose to implement the actual broadcasting yourself, but allow
the internal machinery to compute the container type, element type, and indices by specializing

```julia
Broadcast.broadcast(::typeof(somefunction), ::MyStyle, ::Type{ElType}, inds, As...)
```

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

### [Extending `broadcast!`](@id extending-in-place-broadcast)

Extending `broadcast!` (in-place broadcast) should be done with care, as it is easy to introduce
ambiguities between packages. To avoid these ambiguities, we adhere to the following conventions.

First, if you want to specialize on the destination type, say `DestType`, then you should
define a method with the following signature:

```julia
broadcast!(f, dest::DestType, ::Nothing, As...)
```

Note that no bounds should be placed on the types of `f` and `As...`.

Second, if specialized `broadcast!` behavior is desired depending on the input types,
you should write [binary broadcasting rules](@ref writing-binary-broadcasting-rules) to
determine a custom `BroadcastStyle` given the input types, say `MyBroadcastStyle`, and you should define a method with the following
signature:

```julia
broadcast!(f, dest, ::MyBroadcastStyle, As...)
```

Note the lack of bounds on `f`, `dest`, and `As...`.

Third, simultaneously specializing on both the type of `dest` and the `BroadcastStyle` is fine. In this case,
it is also allowed to specialize on the types of the source arguments (`As...`). For example, these method signatures are OK:

```julia
broadcast!(f, dest::DestType, ::MyBroadcastStyle, As...)
broadcast!(f, dest::DestType, ::MyBroadcastStyle, As::AbstractArray...)
broadcast!(f, dest::DestType, ::Broadcast.Scalar, As::Number...)
```
