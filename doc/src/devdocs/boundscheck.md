# Bounds checking

Like many modern programming languages, Julia uses bounds checking to ensure program safety when
accessing arrays. In tight inner loops or other performance critical situations, you may wish
to skip these bounds checks to improve runtime performance. For instance, in order to emit vectorized
(SIMD) instructions, your loop body cannot contain branches, and thus cannot contain bounds checks.
Consequently, Julia includes an `@inbounds(...)` macro to tell the compiler to skip such bounds
checks within the given block. For the built-in `Array` type, the magic happens inside the `arrayref`
and `arrayset` intrinsics. User-defined array types instead use the `@boundscheck(...)` macro
to achieve context-sensitive code selection.

## Eliding bounds checks

The `@boundscheck(...)` macro marks blocks of code that perform bounds checking. When such blocks
appear inside of an `@inbounds(...)` block, the compiler removes these blocks. When the `@boundscheck(...)`
is nested inside of a calling function containing an `@inbounds(...)`, the compiler will remove
the `@boundscheck` block *only if it is inlined* into the calling function. For example, you might
write the method `sum` as:

```julia
function sum(A::AbstractArray)
    r = zero(eltype(A))
    for i = 1:length(A)
        @inbounds r += A[i]
    end
    return r
end
```

With a custom array-like type `MyArray` having:

```julia
@inline getindex(A::MyArray, i::Real) = (@boundscheck checkbounds(A,i); A.data[to_index(i)])
```

Then when `getindex` is inlined into `sum`, the call to `checkbounds(A,i)` will be elided. If
your function contains multiple layers of inlining, only `@boundscheck` blocks at most one level
of inlining deeper are eliminated. The rule prevents unintended changes in program behavior from
code further up the stack.

## Propagating inbounds

There may be certain scenarios where for code-organization reasons you want more than one layer
between the `@inbounds` and `@boundscheck` declarations. For instance, the default `getindex`
methods have the chain `getindex(A::AbstractArray, i::Real)` calls `getindex(linearindexing(A), A, i)`
calls `_getindex(::LinearFast, A, i)`.

To override the "one layer of inlining" rule, a function may be marked with `@propagate_inbounds`
to propagate an inbounds context (or out of bounds context) through one additional layer of inlining.

## The bounds checking call hierarchy

The overall hierarchy is:

  * `checkbounds(A, I...)` which calls

      * `checkbounds(Bool, A, I...)` which calls

          * `checkbounds_indices(Bool, indices(A), I)` which recursively calls

              * `checkindex` for each dimension

Here `A` is the array, and `I` contains the "requested" indices. `indices(A)` returns a tuple
of "permitted" indices of `A`.

`checkbounds(A, I...)` throws an error if the indices are invalid, whereas `checkbounds(Bool, A, I...)`
returns `false` in that circumstance.  `checkbounds_indices` discards any information about the
array other than its `indices` tuple, and performs a pure indices-vs-indices comparison: this
allows relatively few compiled methods to serve a huge variety of array types. Indices are specified
as tuples, and are usually compared in a 1-1 fashion with individual dimensions handled by calling
another important function, `checkindex`: typically,

```julia
checkbounds_indices(Bool, (IA1, IA...), (I1, I...)) = checkindex(Bool, IA1, I1) &
                                                      checkbounds_indices(Bool, IA, I)
```

so `checkindex` checks a single dimension.  All of these functions, including the unexported
`checkbounds_indices` have docstrings accessible with `?` .

If you have to customize bounds checking for a specific array type, you should specialize `checkbounds(Bool, A, I...)`.
However, in most cases you should be able to rely on `checkbounds_indices` as long as you supply
useful `indices` for your array type.

If you have novel index types, first consider specializing `checkindex`, which handles a single
index for a particular dimension of an array.  If you have a custom multidimensional index type
(similar to `CartesianIndex`), then you may have to consider specializing `checkbounds_indices`.

Note this hierarchy has been designed to reduce the likelihood of method ambiguities.  We try
to make `checkbounds` the place to specialize on array type, and try to avoid specializations
on index types; conversely, `checkindex` is intended to be specialized only on index type (especially,
the last argument).
