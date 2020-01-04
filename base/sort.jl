# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sort

import ..@__MODULE__, ..parentmodule
const Base = parentmodule(@__MODULE__)
using .Base.Order
using .Base: copymutable, LinearIndices, length, (:),
    eachindex, axes, first, last, similar, zip, OrdinalRange,
    AbstractVector, @inbounds, AbstractRange, @eval, @inline, Vector, @noinline,
    AbstractMatrix, AbstractUnitRange, isless, identity, eltype, >, <, <=, >=, |, +, -, *, !,
    extrema, sub_with_overflow, add_with_overflow, oneunit, div, getindex, setindex!,
    length, resize!, fill, Missing, require_one_based_indexing

using .Base: >>>, !==

import .Base:
    sort,
    sort!,
    issorted,
    sortperm,
    to_indices

export # also exported by Base
    # order-only:
    issorted,
    searchsorted,
    searchsortedfirst,
    searchsortedlast,
    # order & algorithm:
    sort,
    sort!,
    sortperm,
    sortperm!,
    partialsort,
    partialsort!,
    partialsortperm,
    partialsortperm!,
    # algorithms:
    InsertionSort,
    QuickSort,
    MergeSort,
    PartialQuickSort

export # not exported by Base
    Algorithm,
    DEFAULT_UNSTABLE,
    DEFAULT_STABLE,
    SMALL_ALGORITHM,
    SMALL_THRESHOLD


## functions requiring only ordering ##

function issorted(itr, order::Ordering)
    y = iterate(itr)
    y === nothing && return true
    prev, state = y
    y = iterate(itr, state)
    while y !== nothing
        this, state = y
        lt(order, this, prev) && return false
        prev = this
        y = iterate(itr, state)
    end
    return true
end

"""
    issorted(v, lt=isless, by=identity, rev:Bool=false, order::Ordering=Forward)

Test whether a vector is in sorted order. The `lt`, `by` and `rev` keywords modify what
order is considered to be sorted just as they do for [`sort`](@ref).

# Examples
```jldoctest
julia> issorted([1, 2, 3])
true

julia> issorted([(1, "b"), (2, "a")], by = x -> x[1])
true

julia> issorted([(1, "b"), (2, "a")], by = x -> x[2])
false

julia> issorted([(1, "b"), (2, "a")], by = x -> x[2], rev=true)
true
```
"""
issorted(itr;
    lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,rev,order))

function partialsort!(v::AbstractVector, k::Union{Integer,OrdinalRange}, o::Ordering)
    inds = axes(v, 1)
    sort!(v, first(inds), last(inds), PartialQuickSort(k), o)
    maybeview(v, k)
end

maybeview(v, k) = view(v, k)
maybeview(v, k::Integer) = v[k]

"""
    partialsort!(v, k; by=<transform>, lt=<comparison>, rev=false)

Partially sort the vector `v` in place, according to the order specified by `by`, `lt` and
`rev` so that the value at index `k` (or range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted via a non-stable
algorithm. If `k` is a single index, that value is returned; if `k` is a range, an array of
values at those indices is returned. Note that `partialsort!` does not fully sort the input
array.

# Examples
```jldoctest
julia> a = [1, 2, 4, 3, 4]
5-element Array{Int64,1}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4)
4

julia> a
5-element Array{Int64,1}:
 1
 2
 3
 4
 4

julia> a = [1, 2, 4, 3, 4]
5-element Array{Int64,1}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4, rev=true)
2

julia> a
5-element Array{Int64,1}:
 4
 4
 3
 2
 1
```
"""
partialsort!(v::AbstractVector, k::Union{Integer,OrdinalRange};
             lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
    partialsort!(v, k, ord(lt,by,rev,order))

"""
    partialsort(v, k, by=<transform>, lt=<comparison>, rev=false)

Variant of [`partialsort!`](@ref) which copies `v` before partially sorting it, thereby returning the
same thing as `partialsort!` but leaving `v` unmodified.
"""
partialsort(v::AbstractVector, k::Union{Integer,OrdinalRange}; kws...) =
    partialsort!(copymutable(v), k; kws...)

# This implementation of `midpoint` is performance-optimized but safe
# only if `lo <= hi`.
midpoint(lo::T, hi::T) where T<:Integer = lo + ((hi - lo) >>> 0x01)
midpoint(lo::Integer, hi::Integer) = midpoint(promote(lo, hi)...)

# reference on sorted binary search:
#   http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equal to x;
# returns length(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::T, hi::T, o::Ordering) where T<:Integer
    u = T(1)
    lo = lo - u
    hi = hi + u
    @inbounds while lo < hi - u
        m = midpoint(lo, hi)
        if lt(o, v[m], x)
            lo = m
        else
            hi = m
        end
    end
    return hi
end

# index of the last value of vector a that is less than or equal to x;
# returns 0 if x is less than all values of v.
function searchsortedlast(v::AbstractVector, x, lo::T, hi::T, o::Ordering) where T<:Integer
    u = T(1)
    lo = lo - u
    hi = hi + u
    @inbounds while lo < hi - u
        m = midpoint(lo, hi)
        if lt(o, x, v[m])
            hi = m
        else
            lo = m
        end
    end
    return lo
end

# returns the range of indices of v equal to x
# if v does not contain x, returns a 0-length range
# indicating the insertion point of x
function searchsorted(v::AbstractVector, x, ilo::T, ihi::T, o::Ordering) where T<:Integer
    u = T(1)
    lo = ilo - u
    hi = ihi + u
    @inbounds while lo < hi - u
        m = midpoint(lo, hi)
        if lt(o, v[m], x)
            lo = m
        elseif lt(o, x, v[m])
            hi = m
        else
            a = searchsortedfirst(v, x, max(lo,ilo), m, o)
            b = searchsortedlast(v, x, m, min(hi,ihi), o)
            return a : b
        end
    end
    return (lo + 1) : (hi - 1)
end

function searchsortedlast(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)
    require_one_based_indexing(a)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        n = round(Integer, clamp((x - first(a)) / step(a) + 1, 1, length(a)))
        lt(o, x, a[n]) ? n - 1 : n
    end
end

function searchsortedfirst(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)
    require_one_based_indexing(a)
    if step(a) == 0
        lt(o, first(a), x) ? length(a) + 1 : 1
    else
        n = round(Integer, clamp((x - first(a)) / step(a) + 1, 1, length(a)))
        lt(o, a[n] ,x) ? n + 1 : n
    end
end

function searchsortedlast(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)
    require_one_based_indexing(a)
    h = step(a)
    if h == 0
        lt(o, x, first(a)) ? 0 : length(a)
    elseif h > 0 && x < first(a)
        firstindex(a) - 1
    elseif h > 0 && x >= last(a)
        lastindex(a)
    elseif h < 0 && x > first(a)
        firstindex(a) - 1
    elseif h < 0 && x <= last(a)
        lastindex(a)
    else
        fld(floor(Integer, x) - first(a), h) + 1
    end
end

function searchsortedfirst(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)
    require_one_based_indexing(a)
    h = step(a)
    if h == 0
        lt(o, first(a), x) ? length(a)+1 : 1
    elseif h > 0 && x <= first(a)
        firstindex(a)
    elseif h > 0 && x > last(a)
        lastindex(a) + 1
    elseif h < 0 && x >= first(a)
        firstindex(a)
    elseif h < 0 && x < last(a)
        lastindex(a) + 1
    else
        -fld(floor(Integer, -x) + first(a), h) + 1
    end
end

function searchsortedfirst(a::AbstractRange{<:Integer}, x::Unsigned, o::DirectOrdering)
    require_one_based_indexing(a)
    if lt(o, first(a), x)
        if step(a) == 0
            length(a) + 1
        else
            min(cld(x - first(a), step(a)), length(a)) + 1
        end
    else
        1
    end
end

function searchsortedlast(a::AbstractRange{<:Integer}, x::Unsigned, o::DirectOrdering)
    require_one_based_indexing(a)
    if lt(o, x, first(a))
        0
    elseif step(a) == 0
        length(a)
    else
        min(fld(x - first(a), step(a)) + 1, length(a))
    end
end

searchsorted(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering) =
    searchsortedfirst(a, x, o) : searchsortedlast(a, x, o)

for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = (inds = axes(v, 1); $s(v,x,first(inds),last(inds),o))
        $s(v::AbstractVector, x;
           lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
            $s(v,x,ord(lt,by,rev,order))
    end
end

"""
    searchsorted(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the range of indices of `a` which compare as equal to `x` (using binary search)
according to the order specified by the `by`, `lt` and `rev` keywords, assuming that `a`
is already sorted in that order. Return an empty range located at the insertion point
if `a` does not contain values equal to `x`.

# Examples
```jldoctest
julia> searchsorted([1, 2, 4, 5, 5, 7], 4) # single match
3:3

julia> searchsorted([1, 2, 4, 5, 5, 7], 5) # multiple matches
4:5

julia> searchsorted([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
3:2

julia> searchsorted([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
7:6

julia> searchsorted([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
1:0
```
""" searchsorted

"""
    searchsortedfirst(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the index of the first value in `a` greater than or equal to `x`, according to the
specified order. Return `length(a) + 1` if `x` is greater than all values in `a`.
`a` is assumed to be sorted.

# Examples
```jldoctest
julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 4) # single match
3

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 5) # multiple matches
4

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
3

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
7

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
1
```
""" searchsortedfirst

"""
    searchsortedlast(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the index of the last value in `a` less than or equal to `x`, according to the
specified order. Return `0` if `x` is less than all values in `a`. `a` is assumed to
be sorted.

# Examples
```jldoctest
julia> searchsortedlast([1, 2, 4, 5, 5, 7], 4) # single match
3

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 5) # multiple matches
5

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
2

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
6

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
0
```
""" searchsortedlast


## sorting algorithms ##

abstract type Algorithm end

struct InsertionSortAlg <: Algorithm end
struct QuickSortAlg     <: Algorithm end
struct MergeSortAlg     <: Algorithm end

"""
    PartialQuickSort{T <: Union{Integer,OrdinalRange}}

Indicate that a sorting function should use the partial quick sort
algorithm. Partial quick sort returns the smallest `k` elements sorted from smallest
to largest, finding them and sorting them using [`QuickSort`](@ref).

Characteristics:
  * *not stable*: does not preserve the ordering of elements which
    compare equal (e.g. "a" and "A" in a sort of letters which
    ignores case).
  * *in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).
"""
struct PartialQuickSort{T <: Union{Integer,OrdinalRange}} <: Algorithm
    k::T
end


"""
    InsertionSort

Indicate that a sorting function should use the insertion sort
algorithm. Insertion sort traverses the collection one element
at a time, inserting each element into its correct, sorted position in
the output list.

Characteristics:
  * *stable*: preserves the ordering of elements which
    compare equal (e.g. "a" and "A" in a sort of letters
    which ignores case).
  * *in-place* in memory.
  * *quadratic performance* in the number of elements to be sorted:
    it is well-suited to small collections but should not be used for large ones.
"""
const InsertionSort = InsertionSortAlg()
"""
    QuickSort

Indicate that a sorting function should use the quick sort
algorithm, which is *not* stable.

Characteristics:
  * *not stable*: does not preserve the ordering of elements which
    compare equal (e.g. "a" and "A" in a sort of letters which
    ignores case).
  * *in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).
  * *good performance* for large collections.
"""
const QuickSort     = QuickSortAlg()
"""
    MergeSort

Indicate that a sorting function should use the merge sort
algorithm. Merge sort divides the collection into
subcollections and repeatedly merges them, sorting each
subcollection at each step, until the entire
collection has been recombined in sorted form.

Characteristics:
  * *stable*: preserves the ordering of elements which compare
    equal (e.g. "a" and "A" in a sort of letters which ignores
    case).
  * *not in-place* in memory.
  * *divide-and-conquer* sort strategy.
"""
const MergeSort     = MergeSortAlg()

const DEFAULT_UNSTABLE = QuickSort
const DEFAULT_STABLE   = MergeSort
const SMALL_ALGORITHM  = InsertionSort
const SMALL_THRESHOLD  = 20

function sort!(v::AbstractVector, lo::Integer, hi::Integer, ::InsertionSortAlg, o::Ordering)
    @inbounds for i = lo+1:hi
        j = i
        x = v[i]
        while j > lo
            if lt(o, x, v[j-1])
                v[j] = v[j-1]
                j -= 1
                continue
            end
            break
        end
        v[j] = x
    end
    return v
end

# selectpivot!
#
# Given 3 locations in an array (lo, mi, and hi), sort v[lo], v[mi], v[hi]) and
# choose the middle value as a pivot
#
# Upon return, the pivot is in v[lo], and v[hi] is guaranteed to be
# greater than the pivot

@inline function selectpivot!(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    @inbounds begin
        mi = midpoint(lo, hi)

        # sort v[mi] <= v[lo] <= v[hi] such that the pivot is immediately in place
        if lt(o, v[lo], v[mi])
            v[mi], v[lo] = v[lo], v[mi]
        end

        if lt(o, v[hi], v[lo])
            if lt(o, v[hi], v[mi])
                v[hi], v[lo], v[mi] = v[lo], v[mi], v[hi]
            else
                v[hi], v[lo] = v[lo], v[hi]
            end
        end

        # return the pivot
        return v[lo]
    end
end

# partition!
#
# select a pivot, and partition v according to the pivot

function partition!(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    pivot = selectpivot!(v, lo, hi, o)
    # pivot == v[lo], v[hi] > pivot
    i, j = lo, hi
    @inbounds while true
        i += 1; j -= 1
        while lt(o, v[i], pivot); i += 1; end;
        while lt(o, pivot, v[j]); j -= 1; end;
        i >= j && break
        v[i], v[j] = v[j], v[i]
    end
    v[j], v[lo] = pivot, v[j]

    # v[j] == pivot
    # v[k] >= pivot for k > j
    # v[i] <= pivot for i < j
    return j
end

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::QuickSortAlg, o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)
        if j-lo < hi-j
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
            lo < (j-1) && sort!(v, lo, j-1, a, o)
            lo = j+1
        else
            j+1 < hi && sort!(v, j+1, hi, a, o)
            hi = j-1
        end
    end
    return v
end

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::MergeSortAlg, o::Ordering, t=similar(v,0))
    @inbounds if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

        m = midpoint(lo, hi)
        (length(t) < m-lo+1) && resize!(t, m-lo+1)

        sort!(v, lo,  m,  a, o, t)
        sort!(v, m+1, hi, a, o, t)

        i, j = 1, lo
        while j <= m
            t[i] = v[j]
            i += 1
            j += 1
        end

        i, k = 1, lo
        while k < j <= hi
            if lt(o, v[j], t[i])
                v[k] = v[j]
                j += 1
            else
                v[k] = t[i]
                i += 1
            end
            k += 1
        end
        while k < j
            v[k] = t[i]
            k += 1
            i += 1
        end
    end

    return v
end

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::PartialQuickSort{<:Integer},
               o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)
        if j >= a.k
            # we don't need to sort anything bigger than j
            hi = j-1
        elseif j-lo < hi-j
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
            lo < (j-1) && sort!(v, lo, j-1, a, o)
            lo = j+1
        else
            (j+1) < hi && sort!(v, j+1, hi, a, o)
            hi = j-1
        end
    end
    return v
end


function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::PartialQuickSort{T},
               o::Ordering) where T<:OrdinalRange
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)

        if j <= first(a.k)
            lo = j+1
        elseif j >= last(a.k)
            hi = j-1
        else
            if j-lo < hi-j
                lo < (j-1) && sort!(v, lo, j-1, a, o)
                lo = j+1
            else
                hi > (j+1) && sort!(v, j+1, hi, a, o)
                hi = j-1
            end
        end
    end
    return v
end


## generic sorting methods ##

defalg(v::AbstractArray) = DEFAULT_STABLE
defalg(v::AbstractArray{<:Union{Number, Missing}}) = DEFAULT_UNSTABLE

function sort!(v::AbstractVector, alg::Algorithm, order::Ordering)
    inds = axes(v,1)
    sort!(v,first(inds),last(inds),alg,order)
end

"""
    sort!(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the vector `v` in place. [`QuickSort`](@ref) is used by default for numeric arrays while
[`MergeSort`](@ref) is used for other arrays. You can specify an algorithm to use via the `alg`
keyword (see [Sorting Algorithms](@ref) for available algorithms). The `by` keyword lets you provide
a function that will be applied to each element before comparison; the `lt` keyword allows
providing a custom "less than" function; use `rev=true` to reverse the sorting order. These
options are independent and can be used together in all possible combinations: if both `by`
and `lt` are specified, the `lt` function is applied to the result of the `by` function;
`rev=true` reverses whatever ordering specified via the `by` and `lt` keywords.

# Examples
```jldoctest
julia> v = [3, 1, 2]; sort!(v); v
3-element Array{Int64,1}:
 1
 2
 3

julia> v = [3, 1, 2]; sort!(v, rev = true); v
3-element Array{Int64,1}:
 3
 2
 1

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[1]); v
3-element Array{Tuple{Int64,String},1}:
 (1, "c")
 (2, "b")
 (3, "a")

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[2]); v
3-element Array{Tuple{Int64,String},1}:
 (3, "a")
 (2, "b")
 (1, "c")
```
"""
function sort!(v::AbstractVector;
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward)
    ordr = ord(lt,by,rev,order)
    if ordr === Forward && isa(v,Vector) && eltype(v)<:Integer
        n = length(v)
        if n > 1
            min, max = extrema(v)
            (diff, o1) = sub_with_overflow(max, min)
            (rangelen, o2) = add_with_overflow(diff, oneunit(diff))
            if !o1 && !o2 && rangelen < div(n,2)
                return sort_int_range!(v, rangelen, min)
            end
        end
    end
    sort!(v, alg, ordr)
end

# sort! for vectors of few unique integers
function sort_int_range!(x::Vector{<:Integer}, rangelen, minval)
    offs = 1 - minval
    n = length(x)

    where = fill(0, rangelen)
    @inbounds for i = 1:n
        where[x[i] + offs] += 1
    end

    idx = 1
    @inbounds for i = 1:rangelen
        lastidx = idx + where[i] - 1
        val = i-offs
        for j = idx:lastidx
            x[j] = val
        end
        idx = lastidx + 1
    end

    return x
end

"""
    sort(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Variant of [`sort!`](@ref) that returns a sorted copy of `v` leaving `v` itself unmodified.

# Examples
```jldoctest
julia> v = [3, 1, 2];

julia> sort(v)
3-element Array{Int64,1}:
 1
 2
 3

julia> v
3-element Array{Int64,1}:
 3
 1
 2
```
"""
sort(v::AbstractVector; kws...) = sort!(copymutable(v); kws...)

## partialsortperm: the permutation to sort the first k elements of an array ##

"""
    partialsortperm(v, k; by=<transform>, lt=<comparison>, rev=false)

Return a partial permutation `I` of the vector `v`, so that `v[I]` returns values of a fully
sorted version of `v` at index `k`. If `k` is a range, a vector of indices is returned; if
`k` is an integer, a single index is returned. The order is specified using the same
keywords as `sort!`. The permutation is stable, meaning that indices of equal elements
appear in ascending order.

Note that this function is equivalent to, but more efficient than, calling `sortperm(...)[k]`.

# Examples
```jldoctest
julia> v = [3, 1, 2, 1];

julia> v[partialsortperm(v, 1)]
1

julia> p = partialsortperm(v, 1:3)
3-element view(::Array{Int64,1}, 1:3) with eltype Int64:
 2
 4
 3

julia> v[p]
3-element Array{Int64,1}:
 1
 1
 2
```
"""
partialsortperm(v::AbstractVector, k::Union{Integer,OrdinalRange}; kwargs...) =
    partialsortperm!(similar(Vector{eltype(k)}, axes(v,1)), v, k; kwargs..., initialized=false)

"""
    partialsortperm!(ix, v, k; by=<transform>, lt=<comparison>, rev=false, initialized=false)

Like [`partialsortperm`](@ref), but accepts a preallocated index vector `ix` the same size as
`v`, which is used to store (a permutation of) the indices of `v`.

If the index vector `ix` is initialized with the indices of `v` (or a permutation thereof), `initialized` should be set to
`true`.

If `initialized` is `false` (the default), then `ix` is initialized to contain the indices of `v`.

If `initialized` is `true`, but `ix` does not contain (a permutation of) the indices of `v`, the behavior of
`partialsortperm!` is undefined.

(Typically, the indices of `v` will be `1:length(v)`, although if `v` has an alternative array type
with non-one-based indices, such as an `OffsetArray`, `ix` must also be an `OffsetArray` with the same
indices, and must contain as values (a permutation of) these same indices.)

Upon return, `ix` is guaranteed to have the indices `k` in their sorted positions, such that

```julia
partialsortperm!(ix, v, k);
v[ix[k]] == partialsort(v, k)
```

The return value is the `k`th element of `ix` if `k` is an integer, or view into `ix` if `k` is
a range.

# Examples
```jldoctest
julia> v = [3, 1, 2, 1];

julia> ix = Vector{Int}(undef, 4);

julia> partialsortperm!(ix, v, 1)
2

julia> ix = [1:4;];

julia> partialsortperm!(ix, v, 2:3, initialized=true)
2-element view(::Array{Int64,1}, 2:3) with eltype Int64:
 4
 3
```
 """
function partialsortperm!(ix::AbstractVector{<:Integer}, v::AbstractVector,
                          k::Union{Integer, OrdinalRange};
                          lt::Function=isless,
                          by::Function=identity,
                          rev::Union{Bool,Nothing}=nothing,
                          order::Ordering=Forward,
                          initialized::Bool=false)
    if axes(ix,1) != axes(v,1)
        throw(ArgumentError("The index vector is used as a workspace and must have the " *
                            "same length/indices as the source vector, $(axes(ix,1)) != $(axes(v,1))"))
    end
    if !initialized
        @inbounds for i = axes(ix,1)
            ix[i] = i
        end
    end

    # do partial quicksort
    sort!(ix, PartialQuickSort(k), Perm(ord(lt, by, rev, order), v))

    maybeview(ix, k)
end

## sortperm: the permutation to sort an array ##

"""
    sortperm(v; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Return a permutation vector `I` that puts `v[I]` in sorted order. The order is specified
using the same keywords as [`sort!`](@ref). The permutation is guaranteed to be stable even
if the sorting algorithm is unstable, meaning that indices of equal elements appear in
ascending order.

See also [`sortperm!`](@ref).

# Examples
```jldoctest
julia> v = [3, 1, 2];

julia> p = sortperm(v)
3-element Array{Int64,1}:
 2
 3
 1

julia> v[p]
3-element Array{Int64,1}:
 1
 2
 3
```
"""
function sortperm(v::AbstractVector;
                  alg::Algorithm=DEFAULT_UNSTABLE,
                  lt=isless,
                  by=identity,
                  rev::Union{Bool,Nothing}=nothing,
                  order::Ordering=Forward)
    ordr = ord(lt,by,rev,order)
    if ordr === Forward && isa(v,Vector) && eltype(v)<:Integer
        n = length(v)
        if n > 1
            min, max = extrema(v)
            (diff, o1) = sub_with_overflow(max, min)
            (rangelen, o2) = add_with_overflow(diff, oneunit(diff))
            if !o1 && !o2 && rangelen < div(n,2)
                return sortperm_int_range(v, rangelen, min)
            end
        end
    end
    ax = axes(v, 1)
    p = similar(Vector{eltype(ax)}, ax)
    for (i,ind) in zip(eachindex(p), ax)
        p[i] = ind
    end
    sort!(p, alg, Perm(ordr,v))
end


"""
    sortperm!(ix, v; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, initialized::Bool=false)

Like [`sortperm`](@ref), but accepts a preallocated index vector `ix`.  If `initialized` is `false`
(the default), `ix` is initialized to contain the values `1:length(v)`.

# Examples
```jldoctest
julia> v = [3, 1, 2]; p = zeros(Int, 3);

julia> sortperm!(p, v); p
3-element Array{Int64,1}:
 2
 3
 1

julia> v[p]
3-element Array{Int64,1}:
 1
 2
 3
```
"""
function sortperm!(x::AbstractVector{<:Integer}, v::AbstractVector;
                   alg::Algorithm=DEFAULT_UNSTABLE,
                   lt=isless,
                   by=identity,
                   rev::Union{Bool,Nothing}=nothing,
                   order::Ordering=Forward,
                   initialized::Bool=false)
    if axes(x,1) != axes(v,1)
        throw(ArgumentError("index vector must have the same length/indices as the source vector, $(axes(x,1)) != $(axes(v,1))"))
    end
    if !initialized
        @inbounds for i = axes(v,1)
            x[i] = i
        end
    end
    sort!(x, alg, Perm(ord(lt,by,rev,order),v))
end

# sortperm for vectors of few unique integers
function sortperm_int_range(x::Vector{<:Integer}, rangelen, minval)
    offs = 1 - minval
    n = length(x)

    where = fill(0, rangelen+1)
    where[1] = 1
    @inbounds for i = 1:n
        where[x[i] + offs + 1] += 1
    end

    #cumsum!(where, where)
    @inbounds for i = 2:length(where)
        where[i] += where[i-1]
    end

    P = Vector{Int}(undef, n)
    @inbounds for i = 1:n
        label = x[i] + offs
        P[where[label]] = i
        where[label] += 1
    end

    return P
end

## sorting multi-dimensional arrays ##

"""
    sort(A; dims::Integer, alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort a multidimensional array `A` along the given dimension.
See [`sort!`](@ref) for a description of possible
keyword arguments.

To sort slices of an array, refer to [`sortslices`](@ref).

# Examples
```jldoctest
julia> A = [4 3; 1 2]
2×2 Array{Int64,2}:
 4  3
 1  2

julia> sort(A, dims = 1)
2×2 Array{Int64,2}:
 1  2
 4  3

julia> sort(A, dims = 2)
2×2 Array{Int64,2}:
 3  4
 1  2
```
"""
function sort(A::AbstractArray;
              dims::Integer,
              alg::Algorithm=DEFAULT_UNSTABLE,
              lt=isless,
              by=identity,
              rev::Union{Bool,Nothing}=nothing,
              order::Ordering=Forward)
    dim = dims
    order = ord(lt,by,rev,order)
    n = length(axes(A, dim))
    if dim != 1
        pdims = (dim, setdiff(1:ndims(A), dim)...)  # put the selected dimension first
        Ap = permutedims(A, pdims)
        Av = vec(Ap)
        sort_chunks!(Av, n, alg, order)
        permutedims(Ap, invperm(pdims))
    else
        Av = A[:]
        sort_chunks!(Av, n, alg, order)
        reshape(Av, axes(A))
    end
end

@noinline function sort_chunks!(Av, n, alg, order)
    inds = LinearIndices(Av)
    for s = first(inds):n:last(inds)
        sort!(Av, s, s+n-1, alg, order)
    end
    Av
end

"""
    sort!(A; dims::Integer, alg::Algorithm=defalg(A), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the multidimensional array `A` along dimension `dims`.
See [`sort!`](@ref) for a description of possible keyword arguments.

To sort slices of an array, refer to [`sortslices`](@ref).

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> A = [4 3; 1 2]
2×2 Array{Int64,2}:
 4  3
 1  2

julia> sort!(A, dims = 1); A
2×2 Array{Int64,2}:
 1  2
 4  3

julia> sort!(A, dims = 2); A
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
function sort!(A::AbstractArray;
               dims::Integer,
               alg::Algorithm=defalg(A),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward)
    ordr = ord(lt, by, rev, order)
    nd = ndims(A)
    k = dims

    1 <= k <= nd || throw(ArgumentError("dimension out of range"))

    remdims = ntuple(i -> i == k ? 1 : size(A, i), nd)
    for idx in CartesianIndices(remdims)
        Av = view(A, ntuple(i -> i == k ? Colon() : idx[i], nd)...)
        sort!(Av, alg, ordr)
    end
    A
end

## fast clever sorting for floats ##

module Float
using ..Sort
using ...Order
using ..Base: @inbounds, AbstractVector, Vector, last, axes

import Core.Intrinsics: slt_int
import ..Sort: sort!
import ...Order: lt, DirectOrdering

const Floats = Union{Float32,Float64}

struct Left <: Ordering end
struct Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order), o.data)
right(o::Perm) = Perm(right(o.order), o.data)

lt(::Left, x::T, y::T) where {T<:Floats} = slt_int(y, x)
lt(::Right, x::T, y::T) where {T<:Floats} = slt_int(x, y)

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::Perm, i::Integer) = isnan(o.order,o.data[i])

function nans2left!(v::AbstractVector, o::Ordering, lo::Integer=first(axes(v,1)), hi::Integer=last(axes(v,1)))
    i = lo
    @inbounds while i <= hi && isnan(o,v[i])
        i += 1
    end
    j = i + 1
    @inbounds while j <= hi
        if isnan(o,v[j])
            v[i], v[j] = v[j], v[i]
            i += 1
        end
        j += 1
    end
    return i, hi
end
function nans2right!(v::AbstractVector, o::Ordering, lo::Integer=first(axes(v,1)), hi::Integer=last(axes(v,1)))
    i = hi
    @inbounds while lo <= i && isnan(o,v[i])
        i -= 1
    end
    j = i - 1
    @inbounds while lo <= j
        if isnan(o,v[j])
            v[i], v[j] = v[j], v[i]
            i -= 1
        end
        j -= 1
    end
    return lo, i
end

nans2end!(v::AbstractVector, o::ForwardOrdering) = nans2right!(v,o)
nans2end!(v::AbstractVector, o::ReverseOrdering) = nans2left!(v,o)
nans2end!(v::AbstractVector{<:Integer}, o::Perm{<:ForwardOrdering}) = nans2right!(v,o)
nans2end!(v::AbstractVector{<:Integer}, o::Perm{<:ReverseOrdering}) = nans2left!(v,o)

issignleft(o::ForwardOrdering, x::Floats) = lt(o, x, zero(x))
issignleft(o::ReverseOrdering, x::Floats) = lt(o, x, -zero(x))
issignleft(o::Perm, i::Integer) = issignleft(o.order, o.data[i])

function fpsort!(v::AbstractVector, a::Algorithm, o::Ordering)
    i, j = lo, hi = nans2end!(v,o)
    @inbounds while true
        while i <= j &&  issignleft(o,v[i]); i += 1; end
        while i <= j && !issignleft(o,v[j]); j -= 1; end
        i <= j || break
        v[i], v[j] = v[j], v[i]
        i += 1; j -= 1
    end
    sort!(v, lo, j,  a, left(o))
    sort!(v, i,  hi, a, right(o))
    return v
end


fpsort!(v::AbstractVector, a::Sort.PartialQuickSort, o::Ordering) =
    sort!(v, first(axes(v,1)), last(axes(v,1)), a, o)

sort!(v::AbstractVector{<:Floats}, a::Algorithm, o::DirectOrdering) = fpsort!(v,a,o)
sort!(v::Vector{Int}, a::Algorithm, o::Perm{<:DirectOrdering,<:Vector{<:Floats}}) = fpsort!(v,a,o)

end # module Sort.Float

end # module Sort
