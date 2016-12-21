# [Multi-dimensional Arrays](@id man-multi-dim-arrays)

Julia, like most technical computing languages, provides a first-class array implementation. Most
technical computing languages pay a lot of attention to their array implementation at the expense
of other containers. Julia does not treat arrays in any special way. The array library is implemented
almost completely in Julia itself, and derives its performance from the compiler, just like any
other code written in Julia. As such, it's also possible to define custom array types by inheriting
from `AbstractArray.` See the [manual section on the AbstractArray interface](@ref man-interface-array) for more details
on implementing a custom array type.

An array is a collection of objects stored in a multi-dimensional grid.  In the most general case,
an array may contain objects of type `Any`.  For most computational purposes, arrays should contain
objects of a more specific type, such as `Float64` or `Int32`.

In general, unlike many other technical computing languages, Julia does not expect programs to
be written in a vectorized style for performance. Julia's compiler uses type inference and generates
optimized code for scalar array indexing, allowing programs to be written in a style that is convenient
and readable, without sacrificing performance, and using less memory at times.

In Julia, all arguments to functions are passed by reference. Some technical computing languages
pass arrays by value, and this is convenient in many cases. In Julia, modifications made to input
arrays within a function will be visible in the parent function. The entire Julia array library
ensures that inputs are not modified by library functions. User code, if it needs to exhibit similar
behavior, should take care to create a copy of inputs that it may modify.

## Arrays

### Basic Functions

| Function               | Description                                                                      |
|:---------------------- |:-------------------------------------------------------------------------------- |
| [`eltype(A)`](@ref)    | the type of the elements contained in `A`                                        |
| [`length(A)`](@ref)    | the number of elements in `A`                                                    |
| [`ndims(A)`](@ref)     | the number of dimensions of `A`                                                  |
| [`size(A)`](@ref)      | a tuple containing the dimensions of `A`                                         |
| [`size(A,n)`](@ref)    | the size of `A` along a particular dimension                                     |
| [`indices(A)`](@ref)   | a tuple containing the valid indices of `A`                                      |
| [`indices(A,n)`](@ref) | a range expressing the valid indices along dimension `n`                         |
| [`eachindex(A)`](@ref) | an efficient iterator for visiting each position in `A`                          |
| [`stride(A,k)`](@ref)  | the stride (linear index distance between adjacent elements) along dimension `k` |
| [`strides(A)`](@ref)   | a tuple of the strides in each dimension                                         |

### Construction and Initialization

Many functions for constructing and initializing arrays are provided. In the following list of
such functions, calls with a `dims...` argument can either take a single tuple of dimension sizes
or a series of dimension sizes passed as a variable number of arguments.

| Function                                    | Description                                                                                                                                                                                                                                              |
|:------------------------------------------- |:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`Array{T}(dims...)`](@ref)                 | an uninitialized dense array                                                                                                                                                                                                                             |
| [`zeros(T, dims...)`](@ref)                 | an array of all zeros of specified type, defaults to `Float64` if `type` not specified                                                                                                                                                                   |
| [`zeros(A)`](@ref)                          | an array of all zeros of same element type and shape of `A`                                                                                                                                                                                              |
| [`ones(T, dims...)`](@ref)                  | an array of all ones of specified type, defaults to `Float64` if `type` not specified                                                                                                                                                                    |
| [`ones(A)`](@ref)                           | an array of all ones of same element type and shape of `A`                                                                                                                                                                                               |
| [`trues(dims...)`](@ref)                    | a `Bool` array with all values `true`                                                                                                                                                                                                                    |
| [`trues(A)`](@ref)                          | a `Bool` array with all values `true` and the shape of `A`                                                                                                                                                                                               |
| [`falses(dims...)`](@ref)                   | a `Bool` array with all values `false`                                                                                                                                                                                                                   |
| [`falses(A)`](@ref)                         | a `Bool` array with all values `false` and the shape of `A`                                                                                                                                                                                              |
| [`reshape(A, dims...)`](@ref)               | an array with the same data as the given array, but with different dimensions.                                                                                                                                                                           |
| [`copy(A)`](@ref)                           | copy `A`                                                                                                                                                                                                                                                 |
| [`deepcopy(A)`](@ref)                       | copy `A`, recursively copying its elements                                                                                                                                                                                                               |
| [`similar(A, element_type, dims...)`](@ref) | an uninitialized array of the same type as the given array (dense, sparse, etc.), but with the specified element type and dimensions. The second and third arguments are both optional, defaulting to the element type and dimensions of `A` if omitted. |
| [`reinterpret(T, A)`](@ref)                 | an array with the same binary data as the given array, but with the specified element type                                                                                                                                                               |
| [`rand(dims)`](@ref)                        | [`Array`](@ref) of `Float64`s with random, iid [^1] and uniformly distributed values in the half-open interval ``[0, 1)``                                                                                                                                |
| [`randn(dims)`](@ref)                       | [`Array`](@ref) of `Float64`s with random, iid and standard normally distributed random values                                                                                                                                                           |
| [`eye(n)`](@ref)                            | `n`-by-`n` identity matrix                                                                                                                                                                                                                               |
| [`eye(m, n)`](@ref)                         | `m`-by-`n` identity matrix                                                                                                                                                                                                                               |
| [`linspace(start, stop, n)`](@ref)          | range of `n` linearly spaced elements from `start` to `stop`                                                                                                                                                                                             |
| [`fill!(A, x)`](@ref)                       | fill the array `A` with the value `x`                                                                                                                                                                                                                    |
| [`fill(x, dims)`](@ref)                     | create an array filled with the value `x`                                                                                                                                                                                                                |

[^1]: *iid*, independently and identically distributed.

The syntax `[A, B, C, ...]` constructs a 1-d array (vector) of its arguments.

### Concatenation

Arrays can be constructed and also concatenated using the following functions:

| Function               | Description                                          |
|:---------------------- |:---------------------------------------------------- |
| [`cat(k, A...)`](@ref) | concatenate input n-d arrays along the dimension `k` |
| [`vcat(A...)`](@ref)   | shorthand for `cat(1, A...)`                         |
| [`hcat(A...)`](@ref)   | shorthand for `cat(2, A...)`                         |

Scalar values passed to these functions are treated as 1-element arrays.

The concatenation functions are used so often that they have special syntax:

| Expression        | Calls             |
|:----------------- |:----------------- |
| `[A; B; C; ...]`  | [`vcat()`](@ref)  |
| `[A B C ...]`     | [`hcat()`](@ref)  |
| `[A B; C D; ...]` | [`hvcat()`](@ref) |

[`hvcat()`](@ref) concatenates in both dimension 1 (with semicolons) and dimension 2 (with spaces).

### Typed array initializers

An array with a specific element type can be constructed using the syntax `T[A, B, C, ...]`. This
will construct a 1-d array with element type `T`, initialized to contain elements `A`, `B`, `C`,
etc. For example `Any[x, y, z]` constructs a heterogeneous array that can contain any values.

Concatenation syntax can similarly be prefixed with a type to specify the element type of the
result.

```julia
julia> [[1 2] [3 4]]
1×4 Array{Int64,2}:
 1  2  3  4

julia> Int8[[1 2] [3 4]]
1×4 Array{Int8,2}:
 1  2  3  4
```

### Comprehensions

Comprehensions provide a general and powerful way to construct arrays. Comprehension syntax is
similar to set construction notation in mathematics:

```
A = [ F(x,y,...) for x=rx, y=ry, ... ]
```

The meaning of this form is that `F(x,y,...)` is evaluated with the variables `x`, `y`, etc. taking
on each value in their given list of values. Values can be specified as any iterable object, but
will commonly be ranges like `1:n` or `2:(n-1)`, or explicit arrays of values like `[1.2, 3.4, 5.7]`.
The result is an N-d dense array with dimensions that are the concatenation of the dimensions
of the variable ranges `rx`, `ry`, etc. and each `F(x,y,...)` evaluation returns a scalar.

The following example computes a weighted average of the current element and its left and right
neighbor along a 1-d grid. :

```julia
julia> x = rand(8)
8-element Array{Float64,1}:
 0.843025
 0.869052
 0.365105
 0.699456
 0.977653
 0.994953
 0.41084
 0.809411

julia> [ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]
6-element Array{Float64,1}:
 0.736559
 0.57468
 0.685417
 0.912429
 0.8446
 0.656511
```

The resulting array type depends on the types of the computed elements. In order to control the
type explicitly, a type can be prepended to the comprehension. For example, we could have requested
the result in single precision by writing:

```julia
Float32[ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]
```

### Generator Expressions

Comprehensions can also be written without the enclosing square brackets, producing an object
known as a generator. This object can be iterated to produce values on demand, instead of allocating
an array and storing them in advance (see [Iteration](@ref)). For example, the following expression
sums a series without allocating memory:

```julia
julia> sum(1/n^2 for n=1:1000)
1.6439345666815615
```

When writing a generator expression with multiple dimensions inside an argument list, parentheses
are needed to separate the generator from subsequent arguments:

```julia
julia> map(tuple, 1/(i+j) for i=1:2, j=1:2, [1:4;])
ERROR: syntax: invalid iteration specification
```

All comma-separated expressions after `for` are interpreted as ranges. Adding parentheses lets
us add a third argument to `map`:

```julia
julia> map(tuple, (1/(i+j) for i=1:2, j=1:2), [1 3; 2 4])
2×2 Array{Tuple{Float64,Int64},2}:
 (0.5,1)       (0.333333,3)
 (0.333333,2)  (0.25,4)
```

Ranges in generators and comprehensions can depend on previous ranges by writing multiple `for`
keywords:

```julia
julia> [(i,j) for i=1:3 for j=1:i]
6-element Array{Tuple{Int64,Int64},1}:
 (1,1)
 (2,1)
 (2,2)
 (3,1)
 (3,2)
 (3,3)
```

In such cases, the result is always 1-d.

Generated values can be filtered using the `if` keyword:

```julia
julia> [(i,j) for i=1:3 for j=1:i if i+j == 4]
2-element Array{Tuple{Int64,Int64},1}:
 (2,2)
 (3,1)
```

### [Indexing](@id man-array-indexing)

The general syntax for indexing into an n-dimensional array A is:

```
X = A[I_1, I_2, ..., I_n]
```

where each `I_k` may be:

1. A scalar integer
2. A `Range` of the form `a:b`, or `a:b:c`
3. A `:` or `Colon()` to select entire dimensions
4. An arbitrary integer array, including the empty array `[]`
5. A boolean array to select a vector of elements at its `true` indices

If all the indices are scalars, then the result `X` is a single element from the array `A`. Otherwise,
`X` is an array with the same number of dimensions as the sum of the dimensionalities of all the
indices.

If all indices are vectors, for example, then the shape of `X` would be `(length(I_1), length(I_2), ..., length(I_n))`,
with location `(i_1, i_2, ..., i_n)` of `X` containing the value `A[I_1[i_1], I_2[i_2], ..., I_n[i_n]]`.
If `I_1` is changed to a two-dimensional matrix, then `X` becomes an `n+1`-dimensional array of
shape `(size(I_1, 1), size(I_1, 2), length(I_2), ..., length(I_n))`. The matrix adds a dimension.
The location `(i_1, i_2, i_3, ..., i_{n+1})` contains the value at `A[I_1[i_1, i_2], I_2[i_3], ..., I_n[i_{n+1}]]`.
All dimensions indexed with scalars are dropped. For example, the result of `A[2, I, 3]` is an
array with size `size(I)`. Its `i`th element is populated by `A[2, I[i], 3]`.

Indexing by a boolean array `B` is effectively the same as indexing by the vector that is returned
by [`find(B)`](@ref). Often referred to as logical indexing, this selects elements at the indices
where the values are `true`, akin to a mask. A logical index must be a vector of the same length
as the dimension it indexes into, or it must be the only index provided and match the size and
dimensionality of the array it indexes into. It is generally more efficient to use boolean arrays
as indices directly instead of first calling [`find()`](@ref).

Additionally, single elements of a multidimensional array can be indexed as `x = A[I]`, where
`I` is a `CartesianIndex`. It effectively behaves like an `n`-tuple of integers spanning multiple
dimensions of `A`. See [Iteration](@ref) below.

As a special part of this syntax, the `end` keyword may be used to represent the last index of
each dimension within the indexing brackets, as determined by the size of the innermost array
being indexed. Indexing syntax without the `end` keyword is equivalent to a call to `getindex`:

```
X = getindex(A, I_1, I_2, ..., I_n)
```

Example:

```julia
julia> x = reshape(1:16, 4, 4)
4×4 Base.ReshapedArray{Int64,2,UnitRange{Int64},Tuple{}}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> x[2:3, 2:end-1]
2×2 Array{Int64,2}:
 6  10
 7  11

julia> x[map(ispow2, x)]
5-element Array{Int64,1}:
  1
  2
  4
  8
 16

julia> x[1, [2 3; 4 1]]
2×2 Array{Int64,2}:
  5  9
 13  1
```

Empty ranges of the form `n:n-1` are sometimes used to indicate the inter-index location between
`n-1` and `n`.  For example, the [`searchsorted()`](@ref) function uses this convention to indicate
the insertion point of a value not found in a sorted array:

```julia
julia> a = [1,2,5,6,7];

julia> searchsorted(a, 3)
3:2
```

### Assignment

The general syntax for assigning values in an n-dimensional array A is:

```
A[I_1, I_2, ..., I_n] = X
```

where each `I_k` may be:

1. A scalar integer
2. A `Range` of the form `a:b`, or `a:b:c`
3. A `:` or `Colon()` to select entire dimensions
4. An arbitrary integer array, including the empty array `[]`
5. A boolean array to select elements at its `true` indices

If `X` is an array, it must have the same number of elements as the product of the lengths of
the indices: `prod(length(I_1), length(I_2), ..., length(I_n))`. The value in location `I_1[i_1], I_2[i_2], ..., I_n[i_n]`
of `A` is overwritten with the value `X[i_1, i_2, ..., i_n]`. If `X` is not an array, its value
is written to all referenced locations of `A`.

A boolean array used as an index behaves as in [`getindex()`](@ref), behaving as though it is
first transformed with [`find()`](@ref).

Index assignment syntax is equivalent to a call to [`setindex!()`](@ref):

```
setindex!(A, X, I_1, I_2, ..., I_n)
```

Example:

```julia
julia> x = collect(reshape(1:9, 3, 3))
3×3 Array{Int64,2}:
 1  4  7
 2  5  8
 3  6  9

julia> x[1:2, 2:3] = -1
-1

julia> x
3×3 Array{Int64,2}:
 1  -1  -1
 2  -1  -1
 3   6   9
```

### Iteration

The recommended ways to iterate over a whole array are

```julia
for a in A
    # Do something with the element a
end

for i in eachindex(A)
    # Do something with i and/or A[i]
end
```

The first construct is used when you need the value, but not index, of each element.  In the second
construct, `i` will be an `Int` if `A` is an array type with fast linear indexing; otherwise,
it will be a `CartesianIndex`:

```julia
A = rand(4,3)
B = view(A, 1:3, 2:3)
julia> for i in eachindex(B)
           @show i
       end
       i = Base.IteratorsMD.CartesianIndex_2(1,1)
       i = Base.IteratorsMD.CartesianIndex_2(2,1)
       i = Base.IteratorsMD.CartesianIndex_2(3,1)
       i = Base.IteratorsMD.CartesianIndex_2(1,2)
       i = Base.IteratorsMD.CartesianIndex_2(2,2)
       i = Base.IteratorsMD.CartesianIndex_2(3,2)
```

In contrast with `for i = 1:length(A)`, iterating with `eachindex` provides an efficient way to
iterate over any array type.

### Array traits

If you write a custom `AbstractArray` type, you can specify that it has fast linear indexing using

```julia
Base.linearindexing{T<:MyArray}(::Type{T}) = LinearFast()
```

This setting will cause `eachindex` iteration over a `MyArray` to use integers.  If you don't
specify this trait, the default value `LinearSlow()` is used.

### Vectorized Operators and Functions

The following operators are supported for arrays.  Also, *every* binary
operator supports a [dot version](@ref man-dot-operators) that can be
applied to arrays (and combinations of arrays and scalars) as a
[fused broadcasting operation](@ref man-vectorized).  (For comparison
operations like `<`, *only* the `.<` version is applicable to arrays.)

1. Unary arithmetic -- `-`, `+`, `!`
2. Binary arithmetic -- `+`, `-`, `*`, `/`, `\`, `^`, `.^`, `div`, `mod`
3. Comparison -- `==`, `!=`, `≈` ([`isapprox`](@ref)), `≉`
4. Unary Boolean or bitwise -- `~`
5. Binary Boolean or bitwise -- `&`, `|`, `⊻` ([`xor`](@ref))

Some operators without dots operate elementwise anyway when one argument is a scalar:
`*`, `+`, `-`, and the bitwise operators. The operators `/` and `\` operate elementwise when
the denominator is a scalar.

Note that comparisons such as `==` operate on whole arrays, giving a single boolean answer. Use
dot operators like `.==` for elementwise comparisons.

To enable convenient vectorization of mathematical and other operations, Julia [provides the compact
syntax](@ref man-vectorized) `f.(args...)`, e.g. `sin.(x)` or `min.(x,y)`, for elementwise operations over arrays or mixtures of arrays and scalars (a [Broadcasting](@ref) operation); these
have the additional advantage of "fusing" into a single loop when combined with
dot operators and other dot calls.

Note that there is a difference between `max.(a,b)`, which `broadcast`s [`max()`](@ref) elementwise
over `a` and `b`, and `maximum(a)`, which finds the largest value within `a`. The same relationship
holds for `min.(a,b)` and `minimum(a)`.

### Broadcasting

It is sometimes useful to perform element-by-element binary operations on arrays of different
sizes, such as adding a vector to each column of a matrix.  An inefficient way to do this would
be to replicate the vector to the size of the matrix:

```julia
julia> a = rand(2,1); A = rand(2,3);

julia> repmat(a,1,3)+A
2×3 Array{Float64,2}:
 1.20813  1.82068  1.25387
 1.56851  1.86401  1.67846
```

This is wasteful when dimensions get large, so Julia offers [`broadcast()`](@ref), which expands
singleton dimensions in array arguments to match the corresponding dimension in the other array
without using extra memory, and applies the given function elementwise:

```julia
julia> broadcast(+, a, A)
2×3 Array{Float64,2}:
 1.20813  1.82068  1.25387
 1.56851  1.86401  1.67846

julia> b = rand(1,2)
1×2 Array{Float64,2}:
 0.867535  0.00457906

julia> broadcast(+, a, b)
2×2 Array{Float64,2}:
 1.71056  0.847604
 1.73659  0.873631
```

[Dotted operators](@ref man-dot-operators) such as `.+` and `.*` are equivalent
to `broadcast` calls (except that they fuse, as described below). There is also a
[`broadcast!()`](@ref) function to specify an explicit destination (which can also
be accessed in a fusing fashion by `.=` assignment), and functions [`broadcast_getindex()`](@ref)
and [`broadcast_setindex!()`](@ref) that broadcast the indices before indexing.   Moreover, `f.(args...)`
is equivalent to `broadcast(f, args...)`, providing a convenient syntax to broadcast any function
([dot syntax](@ref man-vectorized)).  Nested "dot calls" `f.(...)` (including calls to `.+` etcetera)
[automatically fuse](@ref man-dot-operators) into a single `broadcast` call.

Additionally, [`broadcast()`](@ref) is not limited to arrays (see the function documentation),
it also handles tuples and treats any argument that is not an array, tuple or `Ref` (except for `Ptr`) as a "scalar".

```julia
julia> convert.(Float32, [1, 2])
2-element Array{Float32,1}:
 1.0
 2.0

julia> ceil.((UInt8,), [1.2 3.4; 5.6 6.7])
2×2 Array{UInt8,2}:
 0x02  0x04
 0x06  0x07

julia> string.(1:3, ". ", ["First", "Second", "Third"])
3-element Array{String,1}:
 "1. First"
 "2. Second"
 "3. Third"
```

### Implementation

The base array type in Julia is the abstract type `AbstractArray{T,N}`. It is parametrized by
the number of dimensions `N` and the element type `T`. `AbstractVector` and `AbstractMatrix` are
aliases for the 1-d and 2-d cases. Operations on `AbstractArray` objects are defined using higher
level operators and functions, in a way that is independent of the underlying storage. These operations
generally work correctly as a fallback for any specific array implementation.

The `AbstractArray` type includes anything vaguely array-like, and implementations of it might
be quite different from conventional arrays. For example, elements might be computed on request
rather than stored.  However, any concrete `AbstractArray{T,N}` type should generally implement
at least [`size(A)`](@ref) (returning an `Int` tuple), [`getindex(A,i)`](@ref) and [`getindex(A,i1,...,iN)`](@ref getindex);
mutable arrays should also implement [`setindex!()`](@ref).  It is recommended that these operations
have nearly constant time complexity, or technically Õ(1) complexity, as otherwise some array
functions may be unexpectedly slow.   Concrete types should also typically provide a [`similar(A,T=eltype(A),dims=size(A))`](@ref)
method, which is used to allocate a similar array for [`copy()`](@ref) and other out-of-place
operations. No matter how an `AbstractArray{T,N}` is represented internally, `T` is the type of
object returned by *integer* indexing (`A[1, ..., 1]`, when `A` is not empty) and `N` should be
the length of the tuple returned by [`size()`](@ref).

`DenseArray` is an abstract subtype of `AbstractArray` intended to include all arrays that are
laid out at regular offsets in memory, and which can therefore be passed to external C and Fortran
functions expecting this memory layout.  Subtypes should provide a method [`stride(A,k)`](@ref)
that returns the "stride" of dimension `k`: increasing the index of dimension `k` by `1` should
increase the index `i` of [`getindex(A,i)`](@ref) by [`stride(A,k)`](@ref).  If a pointer conversion
method [`Base.unsafe_convert(Ptr{T}, A)`](@ref) is provided, the memory layout should correspond
in the same way to these strides.

The [`Array`](@ref) type is a specific instance of `DenseArray` where elements are stored in column-major
order (see additional notes in [Performance Tips](@ref man-performance-tips)). `Vector` and `Matrix` are aliases for
the 1-d and 2-d cases. Specific operations such as scalar indexing, assignment, and a few other
basic storage-specific operations are all that have to be implemented for [`Array`](@ref), so
that the rest of the array library can be implemented in a generic manner.

`SubArray` is a specialization of `AbstractArray` that performs indexing by reference rather than
by copying. A `SubArray` is created with the [`view()`](@ref) function, which is called the same
way as [`getindex()`](@ref) (with an array and a series of index arguments). The result of [`view()`](@ref)
looks the same as the result of [`getindex()`](@ref), except the data is left in place. [`view()`](@ref)
stores the input index vectors in a `SubArray` object, which can later be used to index the original
array indirectly.

`StridedVector` and `StridedMatrix` are convenient aliases defined to make it possible for Julia
to call a wider range of BLAS and LAPACK functions by passing them either [`Array`](@ref) or
`SubArray` objects, and thus saving inefficiencies from memory allocation and copying.

The following example computes the QR decomposition of a small section of a larger array, without
creating any temporaries, and by calling the appropriate LAPACK function with the right leading
dimension size and stride parameters.

```julia
julia> a = rand(10,10)
10×10 Array{Float64,2}:
 0.561255   0.226678   0.203391  0.308912   …  0.750307  0.235023   0.217964
 0.718915   0.537192   0.556946  0.996234      0.666232  0.509423   0.660788
 0.493501   0.0565622  0.118392  0.493498      0.262048  0.940693   0.252965
 0.0470779  0.736979   0.264822  0.228787      0.161441  0.897023   0.567641
 0.343935   0.32327    0.795673  0.452242      0.468819  0.628507   0.511528
 0.935597   0.991511   0.571297  0.74485    …  0.84589   0.178834   0.284413
 0.160706   0.672252   0.133158  0.65554       0.371826  0.770628   0.0531208
 0.306617   0.836126   0.301198  0.0224702     0.39344   0.0370205  0.536062
 0.890947   0.168877   0.32002   0.486136      0.096078  0.172048   0.77672
 0.507762   0.573567   0.220124  0.165816      0.211049  0.433277   0.539476

julia> b = view(a, 2:2:8,2:2:4)
4×2 SubArray{Float64,2,Array{Float64,2},Tuple{StepRange{Int64,Int64},StepRange{Int64,Int64}},false}:
 0.537192  0.996234
 0.736979  0.228787
 0.991511  0.74485
 0.836126  0.0224702

julia> (q,r) = qr(b);

julia> q
4×2 Array{Float64,2}:
 -0.338809   0.78934
 -0.464815  -0.230274
 -0.625349   0.194538
 -0.527347  -0.534856

julia> r
2×2 Array{Float64,2}:
 -1.58553  -0.921517
  0.0       0.866567
```

## Sparse Matrices

[Sparse matrices](https://en.wikipedia.org/wiki/Sparse_matrix) are matrices that contain enough
zeros that storing them in a special data structure leads to savings in space and execution time.
Sparse matrices may be used when operations on the sparse representation of a matrix lead to considerable
gains in either time or space when compared to performing the same operations on a dense matrix.

### Compressed Sparse Column (CSC) Storage

In Julia, sparse matrices are stored in the [Compressed Sparse Column (CSC) format](https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29).
Julia sparse matrices have the type `SparseMatrixCSC{Tv,Ti}`, where `Tv` is the type of the nonzero
values, and `Ti` is the integer type for storing column pointers and row indices.:

```julia
type SparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
    m::Int                  # Number of rows
    n::Int                  # Number of columns
    colptr::Vector{Ti}      # Column i is in colptr[i]:(colptr[i+1]-1)
    rowval::Vector{Ti}      # Row values of nonzeros
    nzval::Vector{Tv}       # Nonzero values
end
```

The compressed sparse column storage makes it easy and quick to access the elements in the column
of a sparse matrix, whereas accessing the sparse matrix by rows is considerably slower. Operations
such as insertion of nonzero values one at a time in the CSC structure tend to be slow. This is
because all elements of the sparse matrix that are beyond the point of insertion have to be moved
one place over.

All operations on sparse matrices are carefully implemented to exploit the CSC data structure
for performance, and to avoid expensive operations.

If you have data in CSC format from a different application or library, and wish to import it
in Julia, make sure that you use 1-based indexing. The row indices in every column need to be
sorted. If your `SparseMatrixCSC` object contains unsorted row indices, one quick way to sort
them is by doing a double transpose.

In some applications, it is convenient to store explicit zero values in a `SparseMatrixCSC`. These
*are* accepted by functions in `Base` (but there is no guarantee that they will be preserved in
mutating operations).  Such explicitly stored zeros are treated as structural nonzeros by many
routines.  The [`nnz()`](@ref) function returns the number of elements explicitly stored in the
sparse data structure, including structural nonzeros. In order to count the exact number of actual
values that are nonzero, use [`countnz()`](@ref), which inspects every stored element of a sparse
matrix.

### Sparse matrix constructors

The simplest way to create sparse matrices is to use functions equivalent to the [`zeros()`](@ref)
and [`eye()`](@ref) functions that Julia provides for working with dense matrices. To produce
sparse matrices instead, you can use the same names with an `sp` prefix:

```julia
julia> spzeros(3,5)
3×5 sparse matrix with 0 Float64 nonzero entries

julia> speye(3,5)
3×5 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0
```

The [`sparse()`](@ref) function is often a handy way to construct sparse matrices. It takes as
its input a vector `I` of row indices, a vector `J` of column indices, and a vector `V` of nonzero
values. `sparse(I,J,V)` constructs a sparse matrix such that `S[I[k], J[k]] = V[k]`.

```julia
julia> I = [1, 4, 3, 5]; J = [4, 7, 18, 9]; V = [1, 2, -5, 3];

julia> S = sparse(I,J,V)
5×18 sparse matrix with 4 Int64 nonzero entries:
        [1 ,  4]  =  1
        [4 ,  7]  =  2
        [5 ,  9]  =  3
        [3 , 18]  =  -5
```

The inverse of the [`sparse()`](@ref) function is [`findn()`](@ref), which retrieves the inputs
used to create the sparse matrix.

```julia
julia> findn(S)
([1,4,5,3],[4,7,9,18])

julia> findnz(S)
([1,4,5,3],[4,7,9,18],[1,2,3,-5])
```

Another way to create sparse matrices is to convert a dense matrix into a sparse matrix using
the [`sparse()`](@ref) function:

```julia
julia> sparse(eye(5))
5×5 sparse matrix with 5 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0
        [4, 4]  =  1.0
        [5, 5]  =  1.0
```

You can go in the other direction using the [`full()`](@ref) function. The [`issparse()`](@ref)
function can be used to query if a matrix is sparse.

```julia
julia> issparse(speye(5))
true
```

### Sparse matrix operations

Arithmetic operations on sparse matrices also work as they do on dense matrices. Indexing of,
assignment into, and concatenation of sparse matrices work in the same way as dense matrices.
Indexing operations, especially assignment, are expensive, when carried out one element at a time.
In many cases it may be better to convert the sparse matrix into `(I,J,V)` format using [`findnz()`](@ref),
manipulate the non-zeroes or the structure in the dense vectors `(I,J,V)`, and then reconstruct
the sparse matrix.

### Correspondence of dense and sparse methods

The following table gives a correspondence between built-in methods on sparse matrices and their
corresponding methods on dense matrix types. In general, methods that generate sparse matrices
differ from their dense counterparts in that the resulting matrix follows the same sparsity pattern
as a given sparse matrix `S`, or that the resulting sparse matrix has density `d`, i.e. each matrix
element has a probability `d` of being non-zero.

Details can be found in the [Sparse Vectors and Matrices](@ref) section of the standard library
reference.

| Sparse                     | Dense                  | Description                                                                                                                                                           |
|:-------------------------- |:---------------------- |:--------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`spzeros(m,n)`](@ref)     | [`zeros(m,n)`](@ref)   | Creates a *m*-by-*n* matrix of zeros. ([`spzeros(m,n)`](@ref) is empty.)                                                                                              |
| [`spones(S)`](@ref)        | [`ones(m,n)`](@ref)    | Creates a matrix filled with ones. Unlike the dense version, [`spones()`](@ref) has the same sparsity pattern as *S*.                                                 |
| [`speye(n)`](@ref)         | [`eye(n)`](@ref)       | Creates a *n*-by-*n* identity matrix.                                                                                                                                 |
| [`full(S)`](@ref)          | [`sparse(A)`](@ref)    | Interconverts between dense and sparse formats.                                                                                                                       |
| [`sprand(m,n,d)`](@ref)    | [`rand(m,n)`](@ref)    | Creates a *m*-by-*n* random matrix (of density *d*) with iid non-zero elements distributed uniformly on the half-open interval ``[0, 1)``.                            |
| [`sprandn(m,n,d)`](@ref)   | [`randn(m,n)`](@ref)   | Creates a *m*-by-*n* random matrix (of density *d*) with iid non-zero elements distributed according to the standard normal (Gaussian) distribution.                  |
| [`sprandn(m,n,d,X)`](@ref) | [`randn(m,n,X)`](@ref) | Creates a *m*-by-*n* random matrix (of density *d*) with iid non-zero elements distributed according to the *X* distribution. (Requires the `Distributions` package.) |
