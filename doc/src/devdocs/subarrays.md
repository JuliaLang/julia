# SubArrays

Julia's `SubArray` type is a container encoding a "view" of a parent [`AbstractArray`](@ref).  This page
documents some of the design principles and implementation of `SubArray`s.

One of the major design goals is to ensure high performance for views of both [`IndexLinear`](@ref) and
[`IndexCartesian`](@ref) arrays. Furthermore, views of `IndexLinear` arrays should themselves be
`IndexLinear` to the extent that it is possible.

## Index replacement

Consider making 2d slices of a 3d array:

```@meta
DocTestSetup = :(import Random; Random.seed!(1234))
```
```jldoctest subarray
julia> A = rand(2,3,4);

julia> S1 = view(A, :, 1, 2:3)
2×2 view(::Array{Float64,3}, :, 1, 2:3) with eltype Float64:
 0.200586  0.066423
 0.298614  0.956753

julia> S2 = view(A, 1, :, 2:3)
3×2 view(::Array{Float64,3}, 1, :, 2:3) with eltype Float64:
 0.200586  0.066423
 0.246837  0.646691
 0.648882  0.276021
```
```@meta
DocTestSetup = nothing
```

`view` drops "singleton" dimensions (ones that are specified by an `Int`), so both `S1` and `S2`
are two-dimensional `SubArray`s. Consequently, the natural way to index these is with `S1[i,j]`.
To extract the value from the parent array `A`, the natural approach is to replace `S1[i,j]`
with `A[i,1,(2:3)[j]]` and `S2[i,j]` with `A[1,i,(2:3)[j]]`.

The key feature of the design of SubArrays is that this index replacement can be performed without
any runtime overhead.

## SubArray design

### Type parameters and fields

The strategy adopted is first and foremost expressed in the definition of the type:

```julia
struct SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indices::I
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

If in our example above `A` is a `Array{Float64, 3}`, our `S1` case above would be a
`SubArray{Float64,2,Array{Float64,3},Tuple{Base.Slice{Base.OneTo{Int64}},Int64,UnitRange{Int64}},false}`.
Note in particular the tuple parameter, which stores the types of the indices used to create
`S1`. Likewise,

```jldoctest subarray
julia> S1.indices
(Base.Slice(Base.OneTo(2)), 1, 2:3)
```

Storing these values allows index replacement, and having the types encoded as parameters allows
one to dispatch to efficient algorithms.

### Index translation

Performing index translation requires that you do different things for different concrete `SubArray`
types.  For example, for `S1`, one needs to apply the `i,j` indices to the first and third dimensions
of the parent array, whereas for `S2` one needs to apply them to the second and third.  The simplest
approach to indexing would be to do the type-analysis at runtime:

```julia
parentindices = Vector{Any}()
for thisindex in S.indices
    ...
    if isa(thisindex, Int)
        # Don't consume one of the input indices
        push!(parentindices, thisindex)
    elseif isa(thisindex, AbstractVector)
        # Consume an input index
        push!(parentindices, thisindex[inputindex[j]])
        j += 1
    elseif isa(thisindex, AbstractMatrix)
        # Consume two input indices
        push!(parentindices, thisindex[inputindex[j], inputindex[j+1]])
        j += 2
    elseif ...
end
S.parent[parentindices...]
```

Unfortunately, this would be disastrous in terms of performance: each element access would allocate
memory, and involves the running of a lot of poorly-typed code.

The better approach is to dispatch to specific methods to handle each type of stored index. That's
what `reindex` does: it dispatches on the type of the first stored index and consumes the appropriate
number of input indices, and then it recurses on the remaining indices. In the case of `S1`, this
expands to

```julia
Base.reindex(S1, S1.indices, (i, j)) == (i, S1.indices[2], S1.indices[3][j])
```

for any pair of indices `(i,j)` (except [`CartesianIndex`](@ref)s and arrays thereof, see below).

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

```jldoctest subarray
julia> Base.viewindexing(S1.indices)
IndexCartesian()

julia> Base.viewindexing(S2.indices)
IndexLinear()
```

This is computed during construction of the `SubArray` and stored in the `L` type parameter as
a boolean that encodes fast linear indexing support. While not strictly necessary, it means that
we can define dispatch directly on `SubArray{T,N,A,I,true}` without any intermediaries.

Since this computation doesn't depend on runtime values, it can miss some cases in which the stride
happens to be uniform:

```jldoctest
julia> A = reshape(1:4*2, 4, 2)
4×2 reshape(::UnitRange{Int64}, 4, 2) with eltype Int64:
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

```jldoctest
julia> A = reshape(1:5*2, 5, 2)
5×2 reshape(::UnitRange{Int64}, 5, 2) with eltype Int64:
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
    length of the `indices` tuple.  Neither do user-supplied indices necessarily line up with entries
    in the `indices` tuple (e.g., the second user-supplied index might correspond to the third dimension
    of the parent array, and the third element in the `indices` tuple).

    What might be less obvious is that the dimensionality of the stored parent array must be equal
    to the number of effective indices in the `indices` tuple. Some examples:

    ```julia
    A = reshape(1:35, 5, 7) # A 2d parent Array
    S = view(A, 2:7)         # A 1d view created by linear indexing
    S = view(A, :, :, 1:1)   # Appending extra indices is supported
    ```

    Naively, you'd think you could just set `S.parent = A` and `S.indices = (:,:,1:1)`, but supporting
    this dramatically complicates the reindexing process, especially for views of views. Not only
    do you need to dispatch on the types of the stored indices, but you need to examine whether a
    given index is the final one and "merge" any remaining stored indices together. This is not an
    easy task, and even worse: it's slow since it implicitly depends upon linear indexing.

    Fortunately, this is precisely the computation that `ReshapedArray` performs, and it does so linearly
    if possible. Consequently, `view` ensures that the parent array is the appropriate dimensionality
    for the given indices by reshaping it if needed. The inner `SubArray` constructor ensures that
    this invariant is satisfied.
  * [`CartesianIndex`](@ref) and arrays thereof throw a nasty wrench into the `reindex` scheme. Recall that
    `reindex` simply dispatches on the type of the stored indices in order to determine how many passed
    indices should be used and where they should go. But with `CartesianIndex`, there's no longer
    a one-to-one correspondence between the number of passed arguments and the number of dimensions
    that they index into. If we return to the above example of `Base.reindex(S1, S1.indices, (i, j))`,
    you can see that the expansion is incorrect for `i, j = CartesianIndex(), CartesianIndex(2,1)`.
    It should *skip* the `CartesianIndex()` entirely and return:

    ```julia
    (CartesianIndex(2,1)[1], S1.indices[2], S1.indices[3][CartesianIndex(2,1)[2]])
    ```

    Instead, though, we get:

    ```julia
    (CartesianIndex(), S1.indices[2], S1.indices[3][CartesianIndex(2,1)])
    ```

    Doing this correctly would require *combined* dispatch on both the stored and passed indices across
    all combinations of dimensionalities in an intractable manner. As such, `reindex` must never be
    called with `CartesianIndex` indices. Fortunately, the scalar case is easily handled by first
    flattening the `CartesianIndex` arguments to plain integers. Arrays of `CartesianIndex`, however,
    cannot be split apart into orthogonal pieces so easily. Before attempting to use `reindex`, `view`
    must ensure that there are no arrays of `CartesianIndex` in the argument list. If there are, it
    can simply "punt" by avoiding the `reindex` calculation entirely, constructing a nested `SubArray`
    with two levels of indirection instead.
