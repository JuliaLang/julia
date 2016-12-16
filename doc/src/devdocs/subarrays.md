# SubArrays

Julia's `SubArray` type is a container encoding a "view" of a parent `AbstractArray`.  This page
documents some of the design principles and implementation of `SubArray`s.

## Indexing: cartesian vs. linear indexing

Broadly speaking, there are two main ways to access data in an array. The first, often called
cartesian indexing, uses `N` indexes for an `N` -dimensional `AbstractArray`.  For example, a
matrix `A` (2-dimensional) can be indexed in cartesian style as `A[i,j]`.  The second indexing
method, referred to as linear indexing, uses a single index even for higher-dimensional objects.
 For example, if `A = reshape(1:12, 3, 4)`, then the expression `A[5]` returns the value 5.  Julia
allows you to combine these styles of indexing: for example, a 3d array `A3` can be indexed as
`A3[i,j]`, in which case `i` is interpreted as a cartesian index for the first dimension, and
`j` is a linear index over dimensions 2 and 3.

For `Array`s, linear indexing appeals to the underlying storage format: an array is laid out as
a contiguous block of memory, and hence the linear index is just the offset (+1) of the corresponding
entry relative to the beginning of the array.  However, this is not true for many other `AbstractArray`
types: examples include `SparseMatrixCSC`, arrays that require some kind of computation (such
as interpolation), and the type under discussion here, `SubArray`.  For these types, the underlying
information is more naturally described in terms of cartesian indexes.

You can manually convert from a cartesian index to a linear index with `sub2ind`, and vice versa
using `ind2sub`.  `getindex` and `setindex!` functions for `AbstractArray` types may include similar
operations.

While converting from a cartesian index to a linear index is fast (it's just multiplication and
addition), converting from a linear index to a cartesian index is very slow: it relies on the
`div` operation, which is one of the slowest low-level operations you can perform with a CPU.
 For this reason, any code that deals with `AbstractArray` types is best designed in terms of
cartesian, rather than linear, indexing.

## Index replacement

Consider making 2d slices of a 3d array:

```julia
S1 = view(A, :, 5, 2:6)
S2 = view(A, 5, :, 2:6)
```

`view` drops "singleton" dimensions (ones that are specified by an `Int`), so both `S1` and `S2`
are two-dimensional `SubArray`s. Consequently, the natural way to index these is with `S1[i,j]`.
 To extract the value from the parent array `A`, the natural approach is to replace `S1[i,j]`
with `A[i,5,(2:6)[j]]` and `S2[i,j]` with `A[5,i,(2:6)[j]]`.

The key feature of the design of SubArrays is that this index replacement can be performed without
any runtime overhead.

## SubArray design

### Type parameters and fields

The strategy adopted is first and foremost expressed in the definition of the type:

```
immutable SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indexes::I
    offset1::Int       # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
    ...
end
```

`SubArray` has 5 type parameters.  The first two are the standard element type and dimensionality.
 The next is the type of the parent `AbstractArray`.  The most heavily-used is the fourth parameter,
a `Tuple` of the types of the indices for each dimension. The final one, `L`, is only provided
as a convenience for dispatch; it's a boolean that represents whether the index types support
fast linear indexing. More on that later.

If in our example above `A` is a `Array{Float64, 3}`, our `S1` case above would be a `SubArray{Int64,2,Array{Int64,3},Tuple{Colon,Int64,UnitRange{Int64}},false}`.
Note in particular the tuple parameter, which stores the types of the indices used to create
`S1`.  Likewise,

```julia
julia> S1.indexes
(Colon(),5,2:6)
```

Storing these values allows index replacement, and having the types encoded as parameters allows
one to dispatch to efficient algorithms.

### Index translation

Performing index translation requires that you do different things for different concrete `SubArray`
types.  For example, for `S1`, one needs to apply the `i,j` indices to the first and third dimensions
of the parent array, whereas for `S2` one needs to apply them to the second and third.  The simplest
approach to indexing would be to do the type-analysis at runtime:

```
parentindexes = Array{Any}(0)
for thisindex in S.indexes
    ...
    if isa(thisindex, Int)
        # Don't consume one of the input indexes
        push!(parentindexes, thisindex)
    elseif isa(thisindex, AbstractVector)
        # Consume an input index
        push!(parentindexes, thisindex[inputindex[j]])
        j += 1
    elseif isa(thisindex, AbstractMatrix)
        # Consume two input indices
        push!(parentindexes, thisindex[inputindex[j], inputindex[j+1]])
        j += 2
    elseif ...
end
S.parent[parentindexes...]
```

Unfortunately, this would be disastrous in terms of performance: each element access would allocate
memory, and involves the running of a lot of poorly-typed code.

The better approach is to dispatch to specific methods to handle each type of stored index. That's
what `reindex` does: it dispatches on the type of the first stored index and consumes the appropriate
number of input indices, and then it recurses on the remaining indices. In the case of `S1`, this
expands to

```julia
Base.reindex(S1, S1.indexes, (i, j)) == (i, S1.indexes[2], S1.indexes[3][j])
```

for any pair of indices `(i,j)` (except `CartesianIndex`s and arrays thereof, see below).

This is the core of a `SubArray`; indexing methods depend upon `reindex` to do this index translation.
Sometimes, though, we can avoid the indirection and make it even faster.

### Linear indexing

Linear indexing can be implemented efficiently when the entire array has a single stride that
separates successive elements, starting from some offset. This means that we can pre-compute these
values and represent linear indexing simply as an addition and multiplication, avoiding the indirection
of `reindex` and (more importantly) the slow computation of the cartesian coordinates entirely.

For `SubArray` types, the availability of efficient linear indexing is based purely on the types
of the indices, and does not depend on values like the size of the parent array. You can ask whether
a given set of indices supports fast linear indexing with the internal `Base.viewindexing` function:

```julia
julia> Base.viewindexing(S1.indexes)
Base.LinearSlow()

julia> Base.viewindexing(S2.indexes)
Base.LinearFast()
```

This is computed during construction of the `SubArray` and stored in the `L` type parameter as
a boolean that encodes fast linear indexing support. While not strictly necessary, it means that
we can define dispatch directly on `SubArray{T,N,A,I,true}` without any intermediaries.

Since this computation doesn't depend on runtime values, it can miss some cases in which the stride
happens to be uniform:

```julia
julia> A = reshape(1:4*2, 4, 2)
4×2 Base.ReshapedArray{Int64,2,UnitRange{Int64},Tuple{}}:
 1  5
 2  6
 3  7
 4  8

julia> diff(A[2:2:4,:][:])
3-element Array{Int64,1}:
 2
 2
 2
```

A view constructed as `view(A, 2:2:4, :)` happens to have uniform stride, and therefore linear
indexing indeed could be performed efficiently.  However, success in this case depends on the
size of the array: if the first dimension instead were odd,

```julia
julia> A = reshape(1:5*2, 5, 2)
5×2 Base.ReshapedArray{Int64,2,UnitRange{Int64},Tuple{}}:
 1   6
 2   7
 3   8
 4   9
 5  10

julia> diff(A[2:2:4,:][:])
3-element Array{Int64,1}:
 2
 3
 2
```

then `A[2:2:4,:]` does not have uniform stride, so we cannot guarantee efficient linear indexing.
 Since we have to base this decision based purely on types encoded in the parameters of the `SubArray`,
`S = view(A, 2:2:4, :)` cannot implement efficient linear indexing.

### A few details

  * Note that the `Base.reindex` function is agnostic to the types of the input indices; it simply
    determines how and where the stored indices should be reindexed. It not only supports integer
    indices, but it supports non-scalar indexing, too. This means that views of views don't need two
    levels of indirection; they can simply re-compute the indices into the original parent array!
  * Hopefully by now it's fairly clear that supporting slices means that the dimensionality, given
    by the parameter `N`, is not necessarily equal to the dimensionality of the parent array or the
    length of the `indexes` tuple.  Neither do user-supplied indices necessarily line up with entries
    in the `indexes` tuple (e.g., the second user-supplied index might correspond to the third dimension
    of the parent array, and the third element in the `indexes` tuple).

    What might be less obvious is that the dimensionality of the stored parent array must be equal
    to the number of effective indices in the `indexes` tuple. Some examples:

    ```julia
    A = reshape(1:35, 5, 7) # A 2d parent Array
    S = view(A, 2:7)         # A 1d view created by linear indexing
    S = view(A, :, :, 1:1)   # Appending extra indices is supported
    ```

    Naively, you'd think you could just set `S.parent = A` and `S.indexes = (:,:,1:1)`, but supporting
    this dramatically complicates the reindexing process, especially for views of views. Not only
    do you need to dispatch on the types of the stored indices, but you need to examine whether a
    given index is the final one and "merge" any remaining stored indices together. This is not an
    easy task, and even worse: it's slow since it implicitly depends upon linear indexing.

    Fortunately, this is precisely the computation that `ReshapedArray` performs, and it does so linearly
    if possible. Consequently, `view` ensures that the parent array is the appropriate dimensionality
    for the given indices by reshaping it if needed. The inner `SubArray` constructor ensures that
    this invariant is satisfied.
  * `CartesianIndex` and arrays thereof throw a nasty wrench into the `reindex` scheme. Recall that
    `reindex` simply dispatches on the type of the stored indices in order to determine how many passed
    indices should be used and where they should go. But with `CartesianIndex`, there's no longer
    a one-to-one correspondence between the number of passed arguments and the number of dimensions
    that they index into. If we return to the above example of `Base.reindex(S1, S1.indexes, (i, j))`,
    you can see that the expansion is incorrect for `i, j = CartesianIndex(), CartesianIndex(2,1)`.
    It should *skip* the `CartesianIndex()` entirely and return:

    ```julia
    (CartesianIndex(2,1)[1], S1.indexes[2], S1.indexes[3][CartesianIndex(2,1)[2]])
    ```

    Instead, though, we get:

    ```julia
    (CartesianIndex(), S1.indexes[2], S1.indexes[3][CartesianIndex(2,1)])
    ```

    Doing this correctly would require *combined* dispatch on both the stored and passed indices across
    all combinations of dimensionalities in an intractable manner. As such, `reindex` must never be
    called with `CartesianIndex` indices. Fortunately, the scalar case is easily handled by first
    flattening the `CartesianIndex` arguments to plain integers. Arrays of `CartesianIndex`, however,
    cannot be split apart into orthogonal pieces so easily. Before attempting to use `reindex`, `view`
    must ensure that there are no arrays of `CartesianIndex` in the argument list. If there are, it
    can simply "punt" by avoiding the `reindex` calculation entirely, constructing a nested `SubArray`
    with two levels of indirection instead.
