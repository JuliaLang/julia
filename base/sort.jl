# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sort

import ..@__MODULE__, ..parentmodule
const Base = parentmodule(@__MODULE__)
using .Base.Order
using .Base: copymutable, LinearIndices, length, (:), iterate, OneTo,
    eachindex, axes, first, last, similar, zip, OrdinalRange, firstindex, lastindex,
    AbstractVector, @inbounds, AbstractRange, @eval, @inline, Vector, @noinline,
    AbstractMatrix, AbstractUnitRange, isless, identity, eltype, >, <, <=, >=, |, +, -, *, !,
    extrema, sub_with_overflow, add_with_overflow, oneunit, div, getindex, setindex!,
    length, resize!, fill, Missing, require_one_based_indexing, keytype, UnitRange,
    min, max, reinterpret, signed, unsigned, Signed, Unsigned, typemin, xor, Type, BitSigned

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
    insorted,
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
    issorted(v, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

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
    sort!(v, firstindex(v), lastindex(v), PartialQuickSort(k), o)
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
5-element Vector{Int64}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4)
4

julia> a
5-element Vector{Int64}:
 1
 2
 3
 4
 4

julia> a = [1, 2, 4, 3, 4]
5-element Vector{Int64}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4, rev=true)
2

julia> a
5-element Vector{Int64}:
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
# returns lastindex(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
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
# returns firstindex(v)-1 if x is less than all values of v.
function searchsortedlast(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
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
function searchsorted(v::AbstractVector, x, ilo::T, ihi::T, o::Ordering)::UnitRange{keytype(v)} where T<:Integer
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

function searchsortedlast(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if lt(o, x, f)
        0
    elseif h == 0 || !lt(o, x, l)
        length(a)
    else
        n = round(Integer, (x - f) / h + 1)
        lt(o, x, a[n]) ? n - 1 : n
    end
end

function searchsortedfirst(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if !lt(o, f, x)
        1
    elseif h == 0 || lt(o, l, x)
        length(a) + 1
    else
        n = round(Integer, (x - f) / h + 1)
        lt(o, a[n], x) ? n + 1 : n
    end
end

function searchsortedlast(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if lt(o, x, f)
        0
    elseif h == 0 || !lt(o, x, l)
        length(a)
    else
        if o isa ForwardOrdering
            fld(floor(Integer, x) - f, h) + 1
        else
            fld(ceil(Integer, x) - f, h) + 1
        end
    end
end

function searchsortedfirst(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if !lt(o, f, x)
        1
    elseif h == 0 || lt(o, l, x)
        length(a) + 1
    else
        if o isa ForwardOrdering
            cld(ceil(Integer, x) - f, h) + 1
        else
            cld(floor(Integer, x) - f, h) + 1
        end
    end
end

searchsorted(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering) =
    searchsortedfirst(a, x, o) : searchsortedlast(a, x, o)

for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = $s(v,x,firstindex(v),lastindex(v),o)
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

See also: [`insorted`](@ref), [`searchsortedfirst`](@ref), [`sort`](@ref), [`findall`](@ref).

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
specified order. Return `lastindex(a) + 1` if `x` is greater than all values in `a`.
`a` is assumed to be sorted.

See also: [`searchsortedlast`](@ref), [`searchsorted`](@ref), [`findfirst`](@ref).

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
specified order. Return `firstindex(a) - 1` if `x` is less than all values in `a`. `a` is
assumed to be sorted.

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

"""
    insorted(x, a; by=<transform>, lt=<comparison>, rev=false) -> Bool

Determine whether an item `x` is in the sorted collection `a`, in the sense that
it is [`==`](@ref) to one of the values of the collection according to the order
specified by the `by`, `lt` and `rev` keywords, assuming that `a` is already
sorted in that order, see [`sort`](@ref) for the keywords.

See also [`in`](@ref).

# Examples
```jldoctest
julia> insorted(4, [1, 2, 4, 5, 5, 7]) # single match
true

julia> insorted(5, [1, 2, 4, 5, 5, 7]) # multiple matches
true

julia> insorted(3, [1, 2, 4, 5, 5, 7]) # no match
false

julia> insorted(9, [1, 2, 4, 5, 5, 7]) # no match
false

julia> insorted(0, [1, 2, 4, 5, 5, 7]) # no match
false
```

!!! compat "Julia 1.6"
     `insorted` was added in Julia 1.6.
"""
function insorted end
insorted(x, v::AbstractVector; kw...) = !isempty(searchsorted(v, x; kw...))
insorted(x, r::AbstractRange) = in(x, r)

## sorting algorithms ##

abstract type Algorithm end

struct InsertionSortAlg <: Algorithm end
struct QuickSortAlg     <: Algorithm end
struct MergeSortAlg     <: Algorithm end

"""
    AdaptiveSort(fallback)

Indicate that a sorting function should use the fastest available algorithm.

Adaptive sort will use the algorithm specified by `fallback` for types and orders that are
not [`UIntMappable`](@ref). Otherwise, it will typically use:
  * Insertion sort for short vectors
  * Radix sort for long vectors
  * Counting sort for vectors of integers spanning a short range

Adaptive sort is guaranteed to be stable if the fallback algorithm is stable.
"""
struct AdaptiveSort{Fallback <: Algorithm} <: Algorithm
    fallback::Fallback
end
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
the output vector.

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

const DEFAULT_UNSTABLE = AdaptiveSort(QuickSort)
const DEFAULT_STABLE   = AdaptiveSort(MergeSort)
const SMALL_ALGORITHM  = InsertionSort
const SMALL_THRESHOLD  = 20

function sort!(v::AbstractVector, lo::Integer, hi::Integer, ::InsertionSortAlg, o::Ordering)
    @inbounds for i = lo+1:hi
        j = i
        x = v[i]
        while j > lo && lt(o, x, v[j-1])
            v[j] = v[j-1]
            j -= 1
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

function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::MergeSortAlg, o::Ordering,
        t0::Union{AbstractVector{T}, Nothing}=nothing) where T
    @inbounds if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

        m = midpoint(lo, hi)
        
        t = t0 === nothing ? similar(v, m-lo+1) : t0
        length(t) < m-lo+1 && resize!(t, m-lo+1)
        Base.require_one_based_indexing(t)

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

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::PartialQuickSort,
               o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)

        if j <= first(a.k)
            lo = j+1
        elseif j >= last(a.k)
            hi = j-1
        else
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
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

# This is a stable least significant bit first radix sort.
#
# That is, it first sorts the entire vector by the last chunk_size bits, then by the second
# to last chunk_size bits, and so on. Stability means that it will not reorder two elements
# that compare equal. This is essential so that the order introduced by earlier,
# less significant passes is preserved by later passes.
#
# Each pass divides the input into 2^chunk_size == mask+1 buckets. To do this, it
#  * counts the number of entries that fall into each bucket
#  * uses those counts to compute the indices to move elements of those buckets into
#  * moves elements into the computed indices in the swap array
#  * switches the swap and working array
#
# In the case of an odd number of passes, the returned vector will === the input vector t,
# not v. This is one of the many reasons radix_sort! is not exported.
function radix_sort!(v::AbstractVector{U}, lo::Integer, hi::Integer, bits::Unsigned,
                     t::AbstractVector{U}, chunk_size=radix_chunk_size_heuristic(lo, hi, bits)) where U <: Unsigned
    # bits is unsigned for performance reasons.
    mask = UInt(1) << chunk_size - 1
    counts = Vector{Int}(undef, mask+2)

    @inbounds for shift in 0:chunk_size:bits-1

        # counts[2:mask+2] will store the number of elements that fall into each bucket.
        # if chunk_size = 8, counts[2] is bucket 0x00 and counts[257] is bucket 0xff.
        counts .= 0
        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 2 # compute its bucket's index for this pass
            counts[i] += 1            # increment that bucket's count
        end

        counts[1] = lo                # set target index for the first bucket
        cumsum!(counts, counts)       # set target indices for subsequent buckets
        # counts[1:mask+1] now stores indices where the first member of each bucket
        # belongs, not the number of elements in each bucket. We will put the first element
        # of bucket 0x00 in t[counts[1]], the next element of bucket 0x00 in t[counts[1]+1],
        # and the last element of bucket 0x00 in t[counts[2]-1].

        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 1 # compute its bucket's index for this pass
            j = counts[i]             # lookup the target index
            t[j] = x                  # put the element where it belongs
            counts[i] = j + 1         # increment the target index for the next
        end                           #  ↳ element in this bucket

        v, t = t, v # swap the now sorted destination vector t back into primary vector v

    end

    v
end
function radix_chunk_size_heuristic(lo::Integer, hi::Integer, bits::Unsigned)
    # chunk_size is the number of bits to radix over at once.
    # We need to allocate an array of size 2^chunk size, and on the other hand the higher
    # the chunk size the fewer passes we need. Theoretically, chunk size should be based on
    # the Lambert W function applied to length. Empirically, we use this heuristic:
    guess = min(10, log(maybe_unsigned(hi-lo))*3/4+3)
    # TODO the maximum chunk size should be based on architecture cache size.

    # We need iterations * chunk size ≥ bits, and these cld's
    # make an effort to get iterations * chunk size ≈ bits
    UInt8(cld(bits, cld(bits, guess)))
end

# For AbstractVector{Bool}, counting sort is always best.
# This is an implementation of counting sort specialized for Bools.
# Accepts unused workspace to avoid method ambiguity.
function sort!(v::AbstractVector{B}, lo::Integer, hi::Integer, a::AdaptiveSort, o::Ordering,
        t::Union{AbstractVector{B}, Nothing}=nothing) where {B <: Bool}
    first = lt(o, false, true) ? false : lt(o, true, false) ? true : return v
    count = 0
    @inbounds for i in lo:hi
        if v[i] == first
            count += 1
        end
    end
    @inbounds v[lo:lo+count-1] .= first
    @inbounds v[lo+count:hi] .= !first
    v
end

maybe_unsigned(x::Integer) = x # this is necessary to avoid calling unsigned on BigInt
maybe_unsigned(x::BitSigned) = unsigned(x)
function _extrema(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    mn = mx = v[lo]
    @inbounds for i in (lo+1):hi
        vi = v[i]
        lt(o, vi, mn) && (mn = vi)
        lt(o, mx, vi) && (mx = vi)
    end
    mn, mx
end
function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::AdaptiveSort, o::Ordering,
            t::Union{AbstractVector{T}, Nothing}=nothing) where T
    # if the sorting task is not UIntMappable, then we can't radix sort or sort_int_range!
    # so we skip straight to the fallback algorithm which is comparison based.
    U = UIntMappable(T, o)
    U === nothing && return sort!(v, lo, hi, a.fallback, o)

    # to avoid introducing excessive detection costs for the trivial sorting problem
    # and to avoid overflow, we check for small inputs before any other runtime checks
    hi <= lo && return v
    lenm1 = maybe_unsigned(hi-lo) # adding 1 would risk overflow
    # only count sort on a short range can compete with insertion sort when lenm1 < 40
    # and the optimization is not worth the detection cost, so we use insertion sort.
    lenm1 < 40 && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

    # For most arrays, a presorted check is cheap (overhead < 5%) and for most large
    # arrays it is essentially free (<1%). Insertion sort runs in a fast O(n) on presorted
    # input and this guarantees presorted input will always be efficiently handled
    issorted(view(v, lo:hi), o) && return v

    # For large arrays, a reverse-sorted check is essentially free (overhead < 1%)
    if lenm1 >= 500 && issorted(view(v, lo:hi), ReverseOrdering(o))
        reverse!(view(v, lo:hi))
        return v
    end

    # UInt128 does not support fast bit shifting so we never
    # dispatch to radix sort but we may still perform count sort
    if sizeof(U) > 8
        if T <: Integer && o isa DirectOrdering
            v_min, v_max = _extrema(v, lo, hi, Forward)
            v_range = maybe_unsigned(v_max-v_min)
            v_range == 0 && return v # all same

            # we know lenm1 ≥ 40, so this will never underflow.
            # if lenm1 > 3.7e18 (59 exabytes), then this may incorrectly dispatch to fallback
            if v_range < 5lenm1-100 # count sort will outperform comparison sort if v's range is small
                return sort_int_range!(v, Int(v_range+1), v_min, o === Forward ? identity : reverse, lo, hi)
            end
        end
        return sort!(v, lo, hi, a.fallback, o)
    end

    v_min, v_max = _extrema(v, lo, hi, o)
    lt(o, v_min, v_max) || return v # all same
    if T <: Integer && o isa DirectOrdering
        R = o === Reverse
        v_range = maybe_unsigned(R ? v_min-v_max : v_max-v_min)
        if v_range < div(lenm1, 2) # count sort will be superior if v's range is very small
            return sort_int_range!(v, Int(v_range+1), R ? v_max : v_min, R ? reverse : identity, lo, hi)
        end
    end

    u_min, u_max = uint_map(v_min, o), uint_map(v_max, o)
    u_range = maybe_unsigned(u_max-u_min)
    if u_range < div(lenm1, 2) # count sort will be superior if u's range is very small
        u = uint_map!(v, lo, hi, o)
        sort_int_range!(u, Int(u_range+1), u_min, identity, lo, hi)
        return uint_unmap!(v, u, lo, hi, o)
    end

    # if u's range is small, then once we subtract out v_min, we'll get a vector like
    # UInt16[0x001a, 0x0015, 0x0006, 0x001b, 0x0008, 0x000c, 0x0001, 0x000e, 0x001c, 0x0009]
    # where we only need to radix over the last few bits (5, in the example).
    bits = unsigned(8sizeof(u_range) - leading_zeros(u_range))

    # radix sort runs in O(bits * lenm1), insertion sort runs in O(lenm1^2). Radix sort
    # has a constant factor that is three times higher, so radix runtime is 3bits * lenm1
    # and insertion runtime is lenm1^2. Empirically, insertion is faster than radix iff
    # lenm1 < 3bits.
    # Insertion < Radix
    #   lenm1^2 < 3 * bits * lenm1
    #     lenm1 < 3bits
    if lenm1 < 3bits
        # at lenm1 = 64*3-1, QuickSort is about 20% faster than InsertionSort.
        alg = a.fallback === QuickSort && lenm1 > 120 ? QuickSort : SMALL_ALGORITHM
        return sort!(v, lo, hi, alg, o)
    end

    # At this point, we are committed to radix sort.
    u = uint_map!(v, lo, hi, o)

    # we subtract u_min to avoid radixing over unnecessary bits. For example,
    # Int32[3, -1, 2] uint_maps to UInt32[0x80000003, 0x7fffffff, 0x80000002]
    # which uses all 32 bits, but once we subtract u_min = 0x7fffffff, we are left with
    # UInt32[0x00000004, 0x00000000, 0x00000003] which uses only 3 bits, and
    # Float32[2.012, 400.0, 12.345] uint_maps to UInt32[0x3fff3b63, 0x3c37ffff, 0x414570a4]
    # which is reduced to UInt32[0x03c73b64, 0x00000000, 0x050d70a5] using only 26 bits.
    # the overhead for this subtraction is small enough that it is worthwhile in many cases.

    # this is faster than u[lo:hi] .-= u_min as of v1.9.0-DEV.100
    @inbounds for i in lo:hi
        u[i] -= u_min
    end

    if t !== nothing && checkbounds(Bool, t, lo:hi) # Fully preallocated and aligned workspace
        u2 = radix_sort!(u, lo, hi, bits, reinterpret(U, t))
        uint_unmap!(v, u2, lo, hi, o, u_min)
    elseif t !== nothing && (applicable(resize!, t) || length(t) >= hi-lo+1) # Viable workspace
        length(t) >= hi-lo+1 || resize!(t, hi-lo+1)
        t1 = axes(t, 1) isa OneTo ? t : view(t, firstindex(t):lastindex(t))
        u2 = radix_sort!(view(u, lo:hi), 1, hi-lo+1, bits, reinterpret(U, t1))
        uint_unmap!(view(v, lo:hi), u2, 1, hi-lo+1, o, u_min)
    else # No viable workspace
        u2 = radix_sort!(u, lo, hi, bits, similar(u))
        uint_unmap!(v, u2, lo, hi, o, u_min)
    end
end

## generic sorting methods ##

defalg(v::AbstractArray) = DEFAULT_STABLE
defalg(v::AbstractArray{<:Union{Number, Missing}}) = DEFAULT_UNSTABLE
defalg(v::AbstractArray{Missing}) = DEFAULT_UNSTABLE # for method disambiguation
defalg(v::AbstractArray{Union{}}) = DEFAULT_UNSTABLE # for method disambiguation

function sort!(v::AbstractVector{T}, alg::Algorithm,
        order::Ordering, t::Union{AbstractVector{T}, Nothing}=nothing) where T
    sort!(v, firstindex(v), lastindex(v), alg, order, t)
end

function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, alg::Algorithm,
        order::Ordering, t::Union{AbstractVector{T}, Nothing}=nothing) where T
    sort!(v, lo, hi, alg, order)
end

"""
    sort!(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the vector `v` in place. [`QuickSort`](@ref) is used by default for numeric arrays while
[`MergeSort`](@ref) is used for other arrays. You can specify an algorithm to use via the `alg`
keyword (see [Sorting Algorithms](@ref) for available algorithms). The `by` keyword lets you provide
a function that will be applied to each element before comparison; the `lt` keyword allows
providing a custom "less than" function (note that for every `x` and `y`, only one of `lt(x,y)`
and `lt(y,x)` can return `true`); use `rev=true` to reverse the sorting order. These
options are independent and can be used together in all possible combinations: if both `by`
and `lt` are specified, the `lt` function is applied to the result of the `by` function;
`rev=true` reverses whatever ordering specified via the `by` and `lt` keywords.

# Examples
```jldoctest
julia> v = [3, 1, 2]; sort!(v); v
3-element Vector{Int64}:
 1
 2
 3

julia> v = [3, 1, 2]; sort!(v, rev = true); v
3-element Vector{Int64}:
 3
 2
 1

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[1]); v
3-element Vector{Tuple{Int64, String}}:
 (1, "c")
 (2, "b")
 (3, "a")

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[2]); v
3-element Vector{Tuple{Int64, String}}:
 (3, "a")
 (2, "b")
 (1, "c")
```
"""
function sort!(v::AbstractVector{T};
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward,
               workspace::Union{AbstractVector{T}, Nothing}=nothing) where T
    sort!(v, alg, ord(lt,by,rev,order), workspace)
end

# sort! for vectors of few unique integers
function sort_int_range!(x::AbstractVector{<:Integer}, rangelen, minval, maybereverse,
                         lo=firstindex(x), hi=lastindex(x))
    offs = 1 - minval

    counts = fill(0, rangelen)
    @inbounds for i = lo:hi
        counts[x[i] + offs] += 1
    end

    idx = lo
    @inbounds for i = maybereverse(1:rangelen)
        lastidx = idx + counts[i] - 1
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
3-element Vector{Int64}:
 1
 2
 3

julia> v
3-element Vector{Int64}:
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
3-element view(::Vector{Int64}, 1:3) with eltype Int64:
 2
 4
 3

julia> v[p]
3-element Vector{Int64}:
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
2-element view(::Vector{Int64}, 2:3) with eltype Int64:
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
        @inbounds for i in eachindex(ix)
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

See also [`sortperm!`](@ref), [`partialsortperm`](@ref), [`invperm`](@ref), [`indexin`](@ref).

# Examples
```jldoctest
julia> v = [3, 1, 2];

julia> p = sortperm(v)
3-element Vector{Int64}:
 2
 3
 1

julia> v[p]
3-element Vector{Int64}:
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
                  order::Ordering=Forward,
                  workspace::Union{AbstractVector{<:Integer}, Nothing}=nothing)
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
    p = copymutable(eachindex(v))
    sort!(p, alg, Perm(ordr,v), workspace)
end


"""
    sortperm!(ix, v; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, initialized::Bool=false)

Like [`sortperm`](@ref), but accepts a preallocated index vector `ix`.  If `initialized` is `false`
(the default), `ix` is initialized to contain the values `1:length(v)`.

# Examples
```jldoctest
julia> v = [3, 1, 2]; p = zeros(Int, 3);

julia> sortperm!(p, v); p
3-element Vector{Int64}:
 2
 3
 1

julia> v[p]
3-element Vector{Int64}:
 1
 2
 3
```
"""
function sortperm!(x::AbstractVector{T}, v::AbstractVector;
                   alg::Algorithm=DEFAULT_UNSTABLE,
                   lt=isless,
                   by=identity,
                   rev::Union{Bool,Nothing}=nothing,
                   order::Ordering=Forward,
                   initialized::Bool=false,
                   workspace::Union{AbstractVector{T}, Nothing}=nothing) where T <: Integer
    if axes(x,1) != axes(v,1)
        throw(ArgumentError("index vector must have the same length/indices as the source vector, $(axes(x,1)) != $(axes(v,1))"))
    end
    if !initialized
        @inbounds for i in eachindex(v)
            x[i] = i
        end
    end
    sort!(x, alg, Perm(ord(lt,by,rev,order),v), workspace)
end

# sortperm for vectors of few unique integers
function sortperm_int_range(x::Vector{<:Integer}, rangelen, minval)
    offs = 1 - minval
    n = length(x)

    counts = fill(0, rangelen+1)
    counts[1] = 1
    @inbounds for i = 1:n
        counts[x[i] + offs + 1] += 1
    end

    #cumsum!(counts, counts)
    @inbounds for i = 2:length(counts)
        counts[i] += counts[i-1]
    end

    P = Vector{Int}(undef, n)
    @inbounds for i = 1:n
        label = x[i] + offs
        P[counts[label]] = i
        counts[label] += 1
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
2×2 Matrix{Int64}:
 4  3
 1  2

julia> sort(A, dims = 1)
2×2 Matrix{Int64}:
 1  2
 4  3

julia> sort(A, dims = 2)
2×2 Matrix{Int64}:
 3  4
 1  2
```
"""
function sort(A::AbstractArray{T};
              dims::Integer,
              alg::Algorithm=DEFAULT_UNSTABLE,
              lt=isless,
              by=identity,
              rev::Union{Bool,Nothing}=nothing,
              order::Ordering=Forward,
              workspace::Union{AbstractVector{T}, Nothing}=similar(A, size(A, dims))) where T
    dim = dims
    order = ord(lt,by,rev,order)
    n = length(axes(A, dim))
    if dim != 1
        pdims = (dim, setdiff(1:ndims(A), dim)...)  # put the selected dimension first
        Ap = permutedims(A, pdims)
        Av = vec(Ap)
        sort_chunks!(Av, n, alg, order, workspace)
        permutedims(Ap, invperm(pdims))
    else
        Av = A[:]
        sort_chunks!(Av, n, alg, order, workspace)
        reshape(Av, axes(A))
    end
end

@noinline function sort_chunks!(Av, n, alg, order, t)
    inds = LinearIndices(Av)
    for s = first(inds):n:last(inds)
        sort!(Av, s, s+n-1, alg, order, t)
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
2×2 Matrix{Int64}:
 4  3
 1  2

julia> sort!(A, dims = 1); A
2×2 Matrix{Int64}:
 1  2
 4  3

julia> sort!(A, dims = 2); A
2×2 Matrix{Int64}:
 1  2
 3  4
```
"""
function sort!(A::AbstractArray{T};
               dims::Integer,
               alg::Algorithm=defalg(A),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward,
               workspace::Union{AbstractVector{T}, Nothing}=similar(A, size(A, dims))) where T
    ordr = ord(lt, by, rev, order)
    nd = ndims(A)
    k = dims

    1 <= k <= nd || throw(ArgumentError("dimension out of range"))

    remdims = ntuple(i -> i == k ? 1 : axes(A, i), nd)
    for idx in CartesianIndices(remdims)
        Av = view(A, ntuple(i -> i == k ? Colon() : idx[i], nd)...)
        sort!(Av, alg, ordr, workspace)
    end
    A
end


## uint mapping to allow radix sorting primitives other than UInts ##

"""
    UIntMappable(T::Type, order::Ordering)

Return `typeof(uint_map(x::T, order))` if [`uint_map`](@ref) and
[`uint_unmap`](@ref) are implemented.

If either is not implemented, return `nothing`.
"""
UIntMappable(T::Type, order::Ordering) = nothing

"""
    uint_map(x, order::Ordering)::Unsigned

Map `x` to an un unsigned integer, maintaining sort order.

The map should be reversible with [`uint_unmap`](@ref), so `isless(order, a, b)` must be
a linear ordering for `a, b <: typeof(x)`. Satisfies
`isless(order, a, b) === (uint_map(a, order) < uint_map(b, order))`
and `x === uint_unmap(typeof(x), uint_map(x, order), order)`

See also: [`UIntMappable`](@ref) [`uint_unmap`](@ref)
"""
function uint_map end

"""
    uint_unmap(T::Type, u::Unsigned, order::Ordering)

Reconstruct the unique value `x::T` that uint_maps to `u`. Satisfies
`x === uint_unmap(T, uint_map(x::T, order), order)` for all `x <: T`.

See also: [`uint_map`](@ref) [`UIntMappable`](@ref)
"""
function uint_unmap end


### Primitive Types

# Integers
uint_map(x::Unsigned, ::ForwardOrdering) = x
uint_unmap(::Type{T}, u::T, ::ForwardOrdering) where T <: Unsigned = u

uint_map(x::Signed, ::ForwardOrdering) =
    unsigned(xor(x, typemin(x)))
uint_unmap(::Type{T}, u::Unsigned, ::ForwardOrdering) where T <: Signed =
    xor(signed(u), typemin(T))

# unsigned(Int) is not available during bootstrapping.
for (U, S) in [(UInt8, Int8), (UInt16, Int16), (UInt32, Int32), (UInt64, Int64), (UInt128, Int128)]
    @eval UIntMappable(::Type{<:Union{$U, $S}}, ::ForwardOrdering) = $U
end

# Floats are not UIntMappable under regular orderings because they fail on NaN edge cases.
# uint mappings for floats are defined in Float, where the Left and Right orderings
# guarantee that there are no NaN values

# Chars
uint_map(x::Char, ::ForwardOrdering) = reinterpret(UInt32, x)
uint_unmap(::Type{Char}, u::UInt32, ::ForwardOrdering) = reinterpret(Char, u)
UIntMappable(::Type{Char}, ::ForwardOrdering) = UInt32

### Reverse orderings
uint_map(x, rev::ReverseOrdering) = ~uint_map(x, rev.fwd)
uint_unmap(T::Type, u::Unsigned, rev::ReverseOrdering) = uint_unmap(T, ~u, rev.fwd)
UIntMappable(T::Type, order::ReverseOrdering) = UIntMappable(T, order.fwd)


### Vectors

# Convert v to unsigned integers in place, maintaining sort order.
function uint_map!(v::AbstractVector, lo::Integer, hi::Integer, order::Ordering)
    u = reinterpret(UIntMappable(eltype(v), order), v)
    @inbounds for i in lo:hi
        u[i] = uint_map(v[i], order)
    end
    u
end

function uint_unmap!(v::AbstractVector, u::AbstractVector{U}, lo::Integer, hi::Integer,
                     order::Ordering, offset::U=zero(U)) where U <: Unsigned
    @inbounds for i in lo:hi
        v[i] = uint_unmap(eltype(v), u[i]+offset, order)
    end
    v
end


## fast clever sorting for floats ##

module Float
using ..Sort
using ...Order
using ..Base: @inbounds, AbstractVector, Vector, last, firstindex, lastindex, Missing, Type, reinterpret

import Core.Intrinsics: slt_int
import ..Sort: sort!, UIntMappable, uint_map, uint_unmap
import ...Order: lt, DirectOrdering

const Floats = Union{Float32,Float64}
const FPSortable = Union{ # Mixed Float32 and Float64 are not allowed.
    AbstractVector{Union{Float32, Missing}},
    AbstractVector{Union{Float64, Missing}},
    AbstractVector{Float32},
    AbstractVector{Float64},
    AbstractVector{Missing}}

struct Left <: Ordering end
struct Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order), o.data)
right(o::Perm) = Perm(right(o.order), o.data)

lt(::Left, x::T, y::T) where {T<:Floats} = slt_int(y, x)
lt(::Right, x::T, y::T) where {T<:Floats} = slt_int(x, y)

uint_map(x::Float32, ::Left) = ~reinterpret(UInt32, x)
uint_unmap(::Type{Float32}, u::UInt32, ::Left) = reinterpret(Float32, ~u)
uint_map(x::Float32, ::Right) = reinterpret(UInt32, x)
uint_unmap(::Type{Float32}, u::UInt32, ::Right) = reinterpret(Float32, u)
UIntMappable(::Type{Float32}, ::Union{Left, Right}) = UInt32

uint_map(x::Float64, ::Left) = ~reinterpret(UInt64, x)
uint_unmap(::Type{Float64}, u::UInt64, ::Left) = reinterpret(Float64, ~u)
uint_map(x::Float64, ::Right) = reinterpret(UInt64, x)
uint_unmap(::Type{Float64}, u::UInt64, ::Right) = reinterpret(Float64, u)
UIntMappable(::Type{Float64}, ::Union{Left, Right}) = UInt64

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::DirectOrdering, x::Missing) = false
isnan(o::Perm, i::Integer) = isnan(o.order,o.data[i])

ismissing(o::DirectOrdering, x::Floats) = false
ismissing(o::DirectOrdering, x::Missing) = true
ismissing(o::Perm, i::Integer) = ismissing(o.order,o.data[i])

allowsmissing(::AbstractVector{T}, ::DirectOrdering) where {T} = T >: Missing
allowsmissing(::AbstractVector{<:Integer},
              ::Perm{<:DirectOrdering,<:AbstractVector{T}}) where {T} =
    T >: Missing

function specials2left!(testf::Function, v::AbstractVector, o::Ordering,
                        lo::Integer=firstindex(v), hi::Integer=lastindex(v))
    i = lo
    @inbounds while i <= hi && testf(o,v[i])
        i += 1
    end
    j = i + 1
    @inbounds while j <= hi
        if testf(o,v[j])
            v[i], v[j] = v[j], v[i]
            i += 1
        end
        j += 1
    end
    return i, hi
end
function specials2right!(testf::Function, v::AbstractVector, o::Ordering,
                         lo::Integer=firstindex(v), hi::Integer=lastindex(v))
    i = hi
    @inbounds while lo <= i && testf(o,v[i])
        i -= 1
    end
    j = i - 1
    @inbounds while lo <= j
        if testf(o,v[j])
            v[i], v[j] = v[j], v[i]
            i -= 1
        end
        j -= 1
    end
    return lo, i
end

function specials2left!(v::AbstractVector, a::Algorithm, o::Ordering)
    lo, hi = firstindex(v), lastindex(v)
    if allowsmissing(v, o)
        i, _ = specials2left!((v, o) -> ismissing(v, o) || isnan(v, o), v, o, lo, hi)
        sort!(v, lo, i-1, a, o)
        return i, hi
    else
        return specials2left!(isnan, v, o, lo, hi)
    end
end
function specials2right!(v::AbstractVector, a::Algorithm, o::Ordering)
    lo, hi = firstindex(v), lastindex(v)
    if allowsmissing(v, o)
        _, i = specials2right!((v, o) -> ismissing(v, o) || isnan(v, o), v, o, lo, hi)
        sort!(v, i+1, hi, a, o)
        return lo, i
    else
        return specials2right!(isnan, v, o, lo, hi)
    end
end

specials2end!(v::AbstractVector, a::Algorithm, o::ForwardOrdering) =
    specials2right!(v, a, o)
specials2end!(v::AbstractVector, a::Algorithm, o::ReverseOrdering) =
    specials2left!(v, a, o)
specials2end!(v::AbstractVector{<:Integer}, a::Algorithm, o::Perm{<:ForwardOrdering}) =
    specials2right!(v, a, o)
specials2end!(v::AbstractVector{<:Integer}, a::Algorithm, o::Perm{<:ReverseOrdering}) =
    specials2left!(v, a, o)

issignleft(o::ForwardOrdering, x::Floats) = lt(o, x, zero(x))
issignleft(o::ReverseOrdering, x::Floats) = lt(o, x, -zero(x))
issignleft(o::Perm, i::Integer) = issignleft(o.order, o.data[i])

function fpsort!(v::AbstractVector{T}, a::Algorithm, o::Ordering,
        t::Union{AbstractVector{T}, Nothing}=nothing) where T
    # fpsort!'s optimizations speed up comparisons, of which there are O(nlogn).
    # The overhead is O(n). For n < 10, it's not worth it.
    length(v) < 10 && return sort!(v, firstindex(v), lastindex(v), SMALL_ALGORITHM, o, t)

    i, j = lo, hi = specials2end!(v,a,o)
    @inbounds while true
        while i <= j &&  issignleft(o,v[i]); i += 1; end
        while i <= j && !issignleft(o,v[j]); j -= 1; end
        i <= j || break
        v[i], v[j] = v[j], v[i]
        i += 1; j -= 1
    end
    sort!(v, lo, j,  a, left(o), t)
    sort!(v, i,  hi, a, right(o), t)
    return v
end


fpsort!(v::AbstractVector, a::Sort.PartialQuickSort, o::Ordering) =
    sort!(v, firstindex(v), lastindex(v), a, o)

function sort!(v::FPSortable, a::Algorithm, o::DirectOrdering,
        t::Union{FPSortable, Nothing}=nothing)
    fpsort!(v, a, o, t)
end
function sort!(v::AbstractVector{T}, a::Algorithm, o::Perm{<:DirectOrdering,<:FPSortable}, 
        t::Union{AbstractVector{T}, Nothing}=nothing) where T <: Union{Signed, Unsigned}
    fpsort!(v, a, o, t)
end

end # module Sort.Float

end # module Sort
