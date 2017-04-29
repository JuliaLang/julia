# Arrays with custom indices

Julia 0.5 adds experimental support for arrays with arbitrary indices. Conventionally, Julia's
arrays are indexed starting at 1, whereas some other languages start numbering at 0, and yet others
(e.g., Fortran) allow you to specify arbitrary starting indices.  While there is much merit in
picking a standard (i.e., 1 for Julia), there are some algorithms which simplify considerably
if you can index outside the range `1:size(A,d)` (and not just `0:size(A,d)-1`, either). Such
array types are expected to be supplied through packages.

The purpose of this page is to address the question, "what do I have to do to support such arrays
in my own code?"  First, let's address the simplest case: if you know that your code will never
need to handle arrays with unconventional indexing, hopefully the answer is "nothing." Old code,
on conventional arrays, should function essentially without alteration as long as it was using
the exported interfaces of Julia.

## Generalizing existing code

As an overview, the steps are:

  * replace many uses of `size` with `indices`
  * replace `1:length(A)` with `linearindices(A)`, and `length(A)` with `length(linearindices(A))`
  * replace explicit allocations like `Array{Int}(size(B))` with `similar(Array{Int}, indices(B))`

These are described in more detail below.

### Background

Because unconventional indexing breaks deeply-held assumptions throughout the Julia ecosystem,
early adopters running code that has not been updated are likely to experience errors.  The most
frustrating bugs would be incorrect results or segfaults (total crashes of Julia).  For example,
consider the following function:

```julia
function mycopy!(dest::AbstractVector, src::AbstractVector)
    length(dest) == length(src) || throw(DimensionMismatch("vectors must match"))
    # OK, now we're safe to use @inbounds, right? (not anymore!)
    for i = 1:length(src)
        @inbounds dest[i] = src[i]
    end
    dest
end
```

This code implicitly assumes that vectors are indexed from 1. Previously that was a safe assumption,
so this code was fine, but (depending on what types the user passes to this function) it may no
longer be safe.  If this code continued to work when passed a vector with non-1 indices, it would
either produce an incorrect answer or it would segfault.  (If you do get segfaults, to help locate
the cause try running julia with the option `--check-bounds=yes`.)

To ensure that such errors are caught, in Julia 0.5 both `length` and `size`**should** throw an
error when passed an array with non-1 indexing.  This is designed to force users of such arrays
to check the code, and inspect it for whether it needs to be generalized.

### Using `indices` for bounds checks and loop iteration

`indices(A)` (reminiscent of `size(A)`) returns a tuple of `AbstractUnitRange` objects, specifying
the range of valid indices along each dimension of `A`.  When `A` has unconventional indexing,
the ranges may not start at 1.  If you just want the range for a particular dimension `d`, there
is `indices(A, d)`.

Base implements a custom range type, `OneTo`, where `OneTo(n)` means the same thing as `1:n` but
in a form that guarantees (via the type system) that the lower index is 1.  For any new `AbstractArray`
type, this is the default returned by `indices`, and it indicates that this array type uses "conventional"
1-based indexing.  Note that if you don't want to be bothered supporting arrays with non-1 indexing,
you can add the following line:

```julia
@assert all(x->isa(x, Base.OneTo), indices(A))
```

at the top of any function.

For bounds checking, note that there are dedicated functions `checkbounds` and `checkindex` which
can sometimes simplify such tests.

### Linear indexing (`linearindices`)

Some algorithms are most conveniently (or efficiently) written in terms of a single linear index,
`A[i]` even if `A` is multi-dimensional.  In "true" linear indexing, the indices always range
from `1:length(A)`. However, this raises an ambiguity for one-dimensional arrays (a.k.a., `AbstractVector`):
does `v[i]` mean linear indexing, or Cartesian indexing with the array's native indices?

For this reason, if you want to use linear indexing in an algorithm, your best option is to get
the index range by calling `linearindices(A)`.  This will return `indices(A, 1)` if `A` is an
`AbstractVector`, and the equivalent of `1:length(A)` otherwise.

In a sense, one can say that 1-dimensional arrays always use Cartesian indexing. To help enforce
this, it's worth noting that `sub2ind(shape, i...)` and `ind2sub(shape, ind)` will throw an error
if `shape` indicates a 1-dimensional array with unconventional indexing (i.e., is a `Tuple{UnitRange}`
rather than a tuple of `OneTo`).  For arrays with conventional indexing, these functions continue
to work the same as always.

Using `indices` and `linearindices`, here is one way you could rewrite `mycopy!`:

```julia
function mycopy!(dest::AbstractVector, src::AbstractVector)
    indices(dest) == indices(src) || throw(DimensionMismatch("vectors must match"))
    for i in linearindices(src)
        @inbounds dest[i] = src[i]
    end
    dest
end
```

### Allocating storage using generalizations of `similar`

Storage is often allocated with `Array{Int}(dims)` or `similar(A, args...)`. When the result needs
to match the indices of some other array, this may not always suffice. The generic replacement
for such patterns is to use `similar(storagetype, shape)`.  `storagetype` indicates the kind of
underlying "conventional" behavior you'd like, e.g., `Array{Int}` or `BitArray` or even `dims->zeros(Float32, dims)`
(which would allocate an all-zeros array). `shape` is a tuple of `Integer` or `AbstractUnitRange`
values, specifying the indices that you want the result to use.

Let's walk through a couple of explicit examples. First, if `A` has conventional indices, then
`similar(Array{Int}, indices(A))` would end up calling `Array{Int}(size(A))`, and thus return
an array.  If `A` is an `AbstractArray` type with unconventional indexing, then `similar(Array{Int}, indices(A))`
should return something that "behaves like" an `Array{Int}` but with a shape (including indices)
that matches `A`.  (The most obvious implementation is to allocate an `Array{Int}(size(A))` and
then "wrap" it in a type that shifts the indices.)

Note also that `similar(Array{Int}, (indices(A, 2),))` would allocate an `AbstractVector{Int}`
(i.e., 1-dimensional array) that matches the indices of the columns of `A`.

### Deprecations

In generalizing Julia's code base, at least one deprecation was unavoidable: earlier versions
of Julia defined `first(::Colon) = 1`, meaning that the first index along a dimension indexed
by `:` is 1. This definition can no longer be justified, so it was deprecated. There is no provided
replacement, because the proper replacement depends on what you are doing and might need to know
more about the array. However, it appears that many uses of `first(::Colon)` are really about
computing an index offset; when that is the case, a candidate replacement is:

```julia
indexoffset(r::AbstractVector) = first(r) - 1
indexoffset(::Colon) = 0
```

In other words, while `first(:)` does not itself make sense, in general you can say that the offset
associated with a colon-index is zero.

## Writing custom array types with non-1 indexing

Most of the methods you'll need to define are standard for any `AbstractArray` type, see [Abstract Arrays](@ref man-interface-array).
This page focuses on the steps needed to define unconventional indexing.

### Do **not** implement `size` or `length`

Perhaps the majority of pre-existing code that uses `size` will not work properly for arrays with
non-1 indices.  For that reason, it is much better to avoid implementing these methods, and use
the resulting `MethodError` to identify code that needs to be audited and perhaps generalized.

### Do **not** annotate bounds checks

Julia 0.5 includes `@boundscheck` to annotate code that can be removed for callers that exploit
`@inbounds`. Initially, it seems far preferable to run with bounds checking always enabled (i.e.,
omit the `@boundscheck` annotation so the check always runs).

### Custom `AbstractUnitRange` types

If you're writing a non-1 indexed array type, you will want to specialize `indices` so it returns
a `UnitRange`, or (perhaps better) a custom `AbstractUnitRange`.  The advantage of a custom type
is that it "signals" the allocation type for functions like `similar`. If we're writing an array
type for which indexing will start at 0, we likely want to begin by creating a new `AbstractUnitRange`,
`ZeroRange`, where `ZeroRange(n)` is equivalent to `0:n-1`.

In general, you should probably *not* export `ZeroRange` from your package: there may be other
packages that implement their own `ZeroRange`, and having multiple distinct `ZeroRange` types
is (perhaps counterintuitively) an advantage: `ModuleA.ZeroRange` indicates that `similar` should
create a `ModuleA.ZeroArray`, whereas `ModuleB.ZeroRange` indicates a `ModuleB.ZeroArray` type.
 This design allows peaceful coexistence among many different custom array types.

Note that the Julia package `CustomUnitRanges.jl` can sometimes be used to avoid the need to write
your own `ZeroRange` type.

### Specializing `indices`

Once you have your `AbstractUnitRange` type, then specialize `indices`:

```julia
Base.indices(A::ZeroArray) = map(n->ZeroRange(n), A.size)
```

where here we imagine that `ZeroArray` has a field called `size` (there would be other ways to
implement this).

In some cases, the fallback definition for `indices(A, d)`:

```julia
indices{T,N}(A::AbstractArray{T,N}, d) = d <= N ? indices(A)[d] : OneTo(1)
```

may not be what you want: you may need to specialize it to return something other than `OneTo(1)`
when `d > ndims(A)`.  Likewise, in `Base` there is a dedicated function `indices1` which is equivalent
to `indices(A, 1)` but which avoids checking (at runtime) whether `ndims(A) > 0`. (This is purely
a performance optimization.)  It is defined as:

```julia
indices1{T}(A::AbstractArray{T,0}) = OneTo(1)
indices1{T}(A::AbstractArray{T})   = indices(A)[1]
```

If the first of these (the zero-dimensional case) is problematic for your custom array type, be
sure to specialize it appropriately.

### Specializing `similar`

Given your custom `ZeroRange` type, then you should also add the following two specializations
for `similar`:

```julia
function Base.similar(A::AbstractArray, T::Type, shape::Tuple{ZeroRange,Vararg{ZeroRange}})
    # body
end

function Base.similar(f::Union{Function,DataType}, shape::Tuple{ZeroRange,Vararg{ZeroRange}})
    # body
end
```

Both of these should allocate your custom array type.

### Specializing `reshape`

Optionally, define a method

```
Base.reshape(A::AbstractArray, shape::Tuple{ZeroRange,Vararg{ZeroRange}}) = ...
```

and you can `reshape` an array so that the result has custom indices.

## Summary

Writing code that doesn't make assumptions about indexing requires a few extra abstractions, but
hopefully the necessary changes are relatively straightforward.

As a reminder, this support is still experimental. While much of Julia's base code has been updated
to support unconventional indexing, without a doubt there are many omissions that will be discovered
only through usage.  Moreover, at the time of this writing, most packages do not support unconventional
indexing.  As a consequence, early adopters should be prepared to identify and/or fix bugs.  On
the other hand, only through practical usage will it become clear whether this experimental feature
should be retained in future versions of Julia; consequently, interested parties are encouraged
to accept some ownership for putting it through its paces.
