# Sorting and Related Functions

Julia has an extensive, flexible API for sorting and interacting with already-sorted arrays of
values. By default, Julia picks reasonable algorithms and sorts in standard ascending order:

```jldoctest
julia> sort([2,3,1])
3-element Array{Int64,1}:
 1
 2
 3
```

You can easily sort in reverse order as well:

```jldoctest
julia> sort([2,3,1], rev=true)
3-element Array{Int64,1}:
 3
 2
 1
```

To sort an array in-place, use the "bang" version of the sort function:

```jldoctest
julia> a = [2,3,1];

julia> sort!(a);

julia> a
3-element Array{Int64,1}:
 1
 2
 3
```

Instead of directly sorting an array, you can compute a permutation of the array's indices that
puts the array into sorted order:

```julia-repl
julia> v = randn(5)
5-element Array{Float64,1}:
  0.297288
  0.382396
 -0.597634
 -0.0104452
 -0.839027

julia> p = sortperm(v)
5-element Array{Int64,1}:
 5
 3
 4
 1
 2

julia> v[p]
5-element Array{Float64,1}:
 -0.839027
 -0.597634
 -0.0104452
  0.297288
  0.382396
```

Arrays can easily be sorted according to an arbitrary transformation of their values:

```julia-repl
julia> sort(v, by=abs)
5-element Array{Float64,1}:
 -0.0104452
  0.297288
  0.382396
 -0.597634
 -0.839027
```

Or in reverse order by a transformation:

```julia-repl
julia> sort(v, by=abs, rev=true)
5-element Array{Float64,1}:
 -0.839027
 -0.597634
  0.382396
  0.297288
 -0.0104452
```

If needed, the sorting algorithm can be chosen:

```julia-repl
julia> sort(v, alg=InsertionSort)
5-element Array{Float64,1}:
 -0.839027
 -0.597634
 -0.0104452
  0.297288
  0.382396
```

All the sorting and order related functions rely on a "less than" relation defining a total order
on the values to be manipulated. The `isless` function is invoked by default, but the relation
can be specified via the `lt` keyword.

## Sorting Functions

```@docs
Base.sort!
Base.sort
Base.sortperm
Base.Sort.sortperm!
Base.Sort.sortrows
Base.Sort.sortcols
```

## Order-Related Functions

```@docs
Base.issorted
Base.Sort.searchsorted
Base.Sort.searchsortedfirst
Base.Sort.searchsortedlast
Base.Sort.partialsort!
Base.Sort.partialsort
Base.Sort.partialsortperm
Base.Sort.partialsortperm!
```

## Sorting Algorithms

There are currently four sorting algorithms available in base Julia:

  * `InsertionSort`
  * `QuickSort`
  * `PartialQuickSort(k)`
  * `MergeSort`

`InsertionSort` is an O(n^2) stable sorting algorithm. It is efficient for very small `n`, and
is used internally by `QuickSort`.

`QuickSort` is an O(n log n) sorting algorithm which is in-place, very fast, but not stable –
i.e. elements which are considered equal will not remain in the same order in which they originally
appeared in the array to be sorted. `QuickSort` is the default algorithm for numeric values, including
integers and floats.

`PartialQuickSort(k)` is similar to `QuickSort`, but the output array is only sorted up to index
`k` if `k` is an integer, or in the range of `k` if `k` is an `OrdinalRange`. For example:

```julia
x = rand(1:500, 100)
k = 50
k2 = 50:100
s = sort(x; alg=QuickSort)
ps = sort(x; alg=PartialQuickSort(k))
qs = sort(x; alg=PartialQuickSort(k2))
map(issorted, (s, ps, qs))             # => (true, false, false)
map(x->issorted(x[1:k]), (s, ps, qs))  # => (true, true, false)
map(x->issorted(x[k2]), (s, ps, qs))   # => (true, false, true)
s[1:k] == ps[1:k]                      # => true
s[k2] == qs[k2]                        # => true
```

`MergeSort` is an O(n log n) stable sorting algorithm but is not in-place – it requires a temporary
array of half the size of the input array – and is typically not quite as fast as `QuickSort`.
It is the default algorithm for non-numeric data.

The default sorting algorithms are chosen on the basis that they are fast and stable, or *appear*
to be so. For numeric types indeed, `QuickSort` is selected as it is faster and indistinguishable
in this case from a stable sort (unless the array records its mutations in some way). The stability
property comes at a non-negligible cost, so if you don't need it, you may want to explicitly specify
your preferred algorithm, e.g. `sort!(v, alg=QuickSort)`.

The mechanism by which Julia picks default sorting algorithms is implemented via the `Base.Sort.defalg`
function. It allows a particular algorithm to be registered as the default in all sorting functions
for specific arrays. For example, here are the two default methods from [`sort.jl`](https://github.com/JuliaLang/julia/blob/master/base/sort.jl):

```julia
defalg(v::AbstractArray) = MergeSort
defalg(v::AbstractArray{<:Number}) = QuickSort
```

As for numeric arrays, choosing a non-stable default algorithm for array types for which the notion
of a stable sort is meaningless (i.e. when two values comparing equal can not be distinguished)
may make sense.
