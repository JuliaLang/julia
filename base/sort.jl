# This file is a part of Julia. License is MIT: http://julialang.org/license

module Sort

using Base: Order, Checked, copymutable, linearindices, linearindexing, viewindexing, LinearFast, _length

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm,
    Base.Slice,
    Base.to_indices

export # also exported by Base
    # order-only:
    issorted,
    select,
    select!,
    searchsorted,
    searchsortedfirst,
    searchsortedlast,
    # order & algorithm:
    sort,
    sort!,
    selectperm,
    selectperm!,
    sortperm,
    sortperm!,
    sortrows,
    sortcols,
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
    state = start(itr)
    done(itr,state) && return true
    prev, state = next(itr, state)
    while !done(itr, state)
        this, state = next(itr, state)
        lt(order, this, prev) && return false
        prev = this
    end
    return true
end

"""
    issorted(v, by=identity, rev:Bool=false, order::Ordering=Forward)

Test whether a vector is in sorted order. The `by`, `lt` and `rev` keywords modify what
order is considered to be sorted just as they do for [`sort`](@ref).
"""
issorted(itr;
    lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,rev,order))

function select!(v::AbstractVector, k::Union{Int,OrdinalRange}, o::Ordering)
    inds = indices(v, 1)
    sort!(v, first(inds), last(inds), PartialQuickSort(k), o)
    v[k]
end
select!(v::AbstractVector, k::Union{Int,OrdinalRange};
    lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
    select!(v, k, ord(lt,by,rev,order))

select(v::AbstractVector, k::Union{Int,OrdinalRange}; kws...) = select!(copymutable(v), k; kws...)


# reference on sorted binary search:
#   http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equal to x;
# returns length(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::Int, hi::Int, o::Ordering)
    lo = lo-1
    hi = hi+1
    @inbounds while lo < hi-1
        m = (lo+hi)>>>1
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
function searchsortedlast(v::AbstractVector, x, lo::Int, hi::Int, o::Ordering)
    lo = lo-1
    hi = hi+1
    @inbounds while lo < hi-1
        m = (lo+hi)>>>1
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
function searchsorted(v::AbstractVector, x, ilo::Int, ihi::Int, o::Ordering)
    lo = ilo-1
    hi = ihi+1
    @inbounds while lo < hi-1
        m = (lo+hi)>>>1
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

function searchsortedlast{T<:Real}(a::Range{T}, x::Real, o::DirectOrdering)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        n = round(Integer, clamp((x - first(a)) / step(a) + 1, 1, length(a)))
        lt(o, x, a[n]) ? n - 1 : n
    end
end

function searchsortedfirst{T<:Real}(a::Range{T}, x::Real, o::DirectOrdering)
    if step(a) == 0
        lt(o, first(a), x) ? length(a) + 1 : 1
    else
        n = round(Integer, clamp((x - first(a)) / step(a) + 1, 1, length(a)))
        lt(o, a[n] ,x) ? n + 1 : n
    end
end

function searchsortedlast{T<:Integer}(a::Range{T}, x::Real, o::DirectOrdering)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        clamp( fld(floor(Integer, x) - first(a), step(a)) + 1, 0, length(a))
    end
end

function searchsortedfirst{T<:Integer}(a::Range{T}, x::Real, o::DirectOrdering)
    if step(a) == 0
        lt(o, first(a), x) ? length(a)+1 : 1
    else
        clamp(-fld(floor(Integer, -x) + first(a), step(a)) + 1, 1, length(a) + 1)
    end
end

function searchsortedfirst{T<:Integer}(a::Range{T}, x::Unsigned, o::DirectOrdering)
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

function searchsortedlast{T<:Integer}(a::Range{T}, x::Unsigned, o::DirectOrdering)
    if lt(o, x, first(a))
        0
    elseif step(a) == 0
        length(a)
    else
        min(fld(x - first(a), step(a)) + 1, length(a))
    end
end

searchsorted{T<:Real}(a::Range{T}, x::Real, o::DirectOrdering) =
    searchsortedfirst(a, x, o) : searchsortedlast(a, x, o)

for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = (inds = indices(v, 1); $s(v,x,first(inds),last(inds),o))
        $s(v::AbstractVector, x;
           lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
            $s(v,x,ord(lt,by,rev,order))
        $s(v::AbstractVector, x) = $s(v, x, Forward)
    end
end

## sorting algorithms ##

abstract Algorithm

immutable InsertionSortAlg <: Algorithm end
immutable QuickSortAlg     <: Algorithm end
immutable MergeSortAlg     <: Algorithm end

immutable PartialQuickSort{T <: Union{Int,OrdinalRange}} <: Algorithm
    k::T
end

Base.first(a::PartialQuickSort{Int}) = 1
Base.last(a::PartialQuickSort{Int}) = a.k
Base.first(a::PartialQuickSort) = first(a.k)
Base.last(a::PartialQuickSort) = last(a.k)

const InsertionSort = InsertionSortAlg()
const QuickSort     = QuickSortAlg()
const MergeSort     = MergeSortAlg()

const DEFAULT_UNSTABLE = QuickSort
const DEFAULT_STABLE   = MergeSort
const SMALL_ALGORITHM  = InsertionSort
const SMALL_THRESHOLD  = 20

function sort!(v::AbstractVector, lo::Int, hi::Int, ::InsertionSortAlg, o::Ordering)
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

@inline function selectpivot!(v::AbstractVector, lo::Int, hi::Int, o::Ordering)
    @inbounds begin
        mi = (lo+hi)>>>1

        # sort the values in v[lo], v[mi], v[hi]

        if lt(o, v[mi], v[lo])
            v[mi], v[lo] = v[lo], v[mi]
        end
        if lt(o, v[hi], v[mi])
            if lt(o, v[hi], v[lo])
                v[lo], v[mi], v[hi] = v[hi], v[lo], v[mi]
            else
                v[hi], v[mi] = v[mi], v[hi]
            end
        end

        # move v[mi] to v[lo] and use it as the pivot
        v[lo], v[mi] = v[mi], v[lo]
        pivot = v[lo]
    end

    # return the pivot
    return pivot
end

# partition!
#
# select a pivot, and partition v according to the pivot

function partition!(v::AbstractVector, lo::Int, hi::Int, o::Ordering)
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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::QuickSortAlg, o::Ordering)
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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSortAlg, o::Ordering, t=similar(v,0))
    @inbounds if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

        m = (lo+hi)>>>1
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

## TODO: When PartialQuickSort is parameterized by an Int, this version of sort
##       has one less comparison per loop than the version below, but enabling
##       it causes return type inference to fail for sort/sort! (#12833)
##
# function sort!(v::AbstractVector, lo::Int, hi::Int, a::PartialQuickSort{Int},
#                o::Ordering)
#     @inbounds while lo < hi
#         hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
#         j = partition!(v, lo, hi, o)
#         if j >= a.k
#             # we don't need to sort anything bigger than j
#             hi = j-1
#         elseif j-lo < hi-j
#             # recurse on the smaller chunk
#             # this is necessary to preserve O(log(n))
#             # stack space in the worst case (rather than O(n))
#             lo < (j-1) && sort!(v, lo, j-1, a, o)
#             lo = j+1
#         else
#             (j+1) < hi && sort!(v, j+1, hi, a, o)
#             hi = j-1
#         end
#     end
#     return v
# end


function sort!(v::AbstractVector, lo::Int, hi::Int, a::PartialQuickSort,
               o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)

        if j <= first(a)
            lo = j+1
        elseif j >= last(a)
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
defalg{T<:Number}(v::AbstractArray{T}) = DEFAULT_UNSTABLE

function sort!(v::AbstractVector, alg::Algorithm, order::Ordering)
    inds = indices(v,1)
    sort!(v,first(inds),last(inds),alg,order)
end

"""
    sort!(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the vector `v` in place. `QuickSort` is used by default for numeric arrays while
`MergeSort` is used for other arrays. You can specify an algorithm to use via the `alg`
keyword (see Sorting Algorithms for available algorithms). The `by` keyword lets you provide
a function that will be applied to each element before comparison; the `lt` keyword allows
providing a custom "less than" function; use `rev=true` to reverse the sorting order. These
options are independent and can be used together in all possible combinations: if both `by`
and `lt` are specified, the `lt` function is applied to the result of the `by` function;
`rev=true` reverses whatever ordering specified via the `by` and `lt` keywords.
"""
function sort!(v::AbstractVector;
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Bool=false,
               order::Ordering=Forward)
    ordr = ord(lt,by,rev,order)
    if ordr === Forward && isa(v,Vector) && eltype(v)<:Integer
        n = _length(v)
        if n > 1
            min, max = extrema(v)
            (diff, o1) = sub_with_overflow(max, min)
            (rangelen, o2) = add_with_overflow(diff, one(diff))
            if !o1 && !o2 && rangelen < div(n,2)
                return sort_int_range!(v, rangelen, min)
            end
        end
    end
    sort!(v, alg, ordr)
end

# sort! for vectors of few unique integers
function sort_int_range!{T<:Integer}(x::Vector{T}, rangelen, minval)
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
"""
sort(v::AbstractVector; kws...) = sort!(copymutable(v); kws...)

## selectperm: the permutation to sort the first k elements of an array ##

selectperm(v::AbstractVector, k::Union{Integer,OrdinalRange}; kwargs...) =
    selectperm!(similar(Vector{eltype(k)}, indices(v,1)), v, k; kwargs..., initialized=false)

function selectperm!{I<:Integer}(ix::AbstractVector{I}, v::AbstractVector,
                                 k::Union{Int, OrdinalRange};
                                 lt::Function=isless,
                                 by::Function=identity,
                                 rev::Bool=false,
                                 order::Ordering=Forward,
                                 initialized::Bool=false)
    if !initialized
        @inbounds for i = indices(ix,1)
            ix[i] = i
        end
    end

    # do partial quicksort
    sort!(ix, PartialQuickSort(k), Perm(ord(lt, by, rev, order), v))
    return ix[k]
end

## sortperm: the permutation to sort an array ##

"""
    sortperm(v; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Return a permutation vector of indices of `v` that puts it in sorted order. Specify `alg` to
choose a particular sorting algorithm (see Sorting Algorithms). `MergeSort` is used by
default, and since it is stable, the resulting permutation will be the lexicographically
first one that puts the input array into sorted order – i.e. indices of equal elements
appear in ascending order. If you choose a non-stable sorting algorithm such as `QuickSort`,
a different permutation that puts the array into order may be returned. The order is
specified using the same keywords as `sort!`.

See also [`sortperm!`](@ref).
"""
function sortperm(v::AbstractVector;
                  alg::Algorithm=DEFAULT_UNSTABLE,
                  lt=isless,
                  by=identity,
                  rev::Bool=false,
                  order::Ordering=Forward)
    ordr = ord(lt,by,rev,order)
    if ordr === Forward && isa(v,Vector) && eltype(v)<:Integer
        n = _length(v)
        if n > 1
            min, max = extrema(v)
            (diff, o1) = sub_with_overflow(max, min)
            (rangelen, o2) = add_with_overflow(diff, one(diff))
            if !o1 && !o2 && rangelen < div(n,2)
                return sortperm_int_range(v, rangelen, min)
            end
        end
    end
    p = similar(Vector{Int}, indices(v, 1))
    for (i,ind) in zip(eachindex(p), indices(v, 1))
        p[i] = ind
    end
    sort!(p, alg, Perm(ordr,v))
end


"""
    sortperm!(ix, v; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, initialized::Bool=false)

Like [`sortperm`](@ref), but accepts a preallocated index vector `ix`.  If `initialized` is `false`
(the default), ix is initialized to contain the values `1:length(v)`.
"""
function sortperm!{I<:Integer}(x::AbstractVector{I}, v::AbstractVector;
                               alg::Algorithm=DEFAULT_UNSTABLE,
                               lt=isless,
                               by=identity,
                               rev::Bool=false,
                               order::Ordering=Forward,
                               initialized::Bool=false)
    if indices(x,1) != indices(v,1)
        throw(ArgumentError("index vector must have the same indices as the source vector, $(indices(x,1)) != $(indices(v,1))"))
    end
    if !initialized
        @inbounds for i = indices(v,1)
            x[i] = i
        end
    end
    sort!(x, alg, Perm(ord(lt,by,rev,order),v))
end

# sortperm for vectors of few unique integers
function sortperm_int_range{T<:Integer}(x::Vector{T}, rangelen, minval)
    offs = 1 - minval
    n = length(x)

    where = fill(0, rangelen+1)
    where[1] = 1
    @inbounds for i = 1:n
        where[x[i] + offs + 1] += 1
    end
    cumsum!(where, where)

    P = Vector{Int}(n)
    @inbounds for i = 1:n
        label = x[i] + offs
        P[where[label]] = i
        where[label] += 1
    end

    return P
end

## sorting multi-dimensional arrays ##

"""
    sort(A, dim::Integer; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, initialized::Bool=false)

Sort a multidimensional array `A` along the given dimension.
See [`sort!`](@ref) for a description of possible
keyword arguments.
"""
function sort(A::AbstractArray, dim::Integer;
              alg::Algorithm=DEFAULT_UNSTABLE,
              lt=isless,
              by=identity,
              rev::Bool=false,
              order::Ordering=Forward,
              initialized::Bool=false)
    order = ord(lt,by,rev,order)
    n = length(indices(A, dim))
    if dim != 1
        pdims = (dim, setdiff(1:ndims(A), dim)...)  # put the selected dimension first
        Ap = permutedims(A, pdims)
        Av = vec(Ap)
        sort_chunks!(Av, n, alg, order)
        permutedims(Ap, invperm(pdims))
    else
        Av = A[:]
        sort_chunks!(Av, n, alg, order)
        reshape(Av, indices(A))
    end
end

@noinline function sort_chunks!(Av, n, alg, order)
    inds = linearindices(Av)
    for s = first(inds):n:last(inds)
        sort!(Av, s, s+n-1, alg, order)
    end
    Av
end


"""
    sortrows(A; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the rows of matrix `A` lexicographically.
See [`sort!`](@ref) for a description of possible
keyword arguments.
"""
function sortrows(A::AbstractMatrix; kws...)
    inds = indices(A,1)
    T = slicetypeof(A, inds, :)
    rows = similar(Vector{T}, indices(A, 1))
    for i in inds
        rows[i] = view(A, i, :)
    end
    p = sortperm(rows; kws..., order=Lexicographic)
    A[p,:]
end

"""
    sortcols(A; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the columns of matrix `A` lexicographically.
See [`sort!`](@ref) for a description of possible
keyword arguments.
"""
function sortcols(A::AbstractMatrix; kws...)
    inds = indices(A,2)
    T = slicetypeof(A, :, inds)
    cols = similar(Vector{T}, indices(A, 2))
    for i in inds
        cols[i] = view(A, :, i)
    end
    p = sortperm(cols; kws..., order=Lexicographic)
    A[:,p]
end

function slicetypeof{T,N}(A::AbstractArray{T,N}, i1, i2)
    I = map(slice_dummy, to_indices(A, (i1, i2)))
    fast = isa(linearindexing(viewindexing(I), linearindexing(A)), LinearFast)
    SubArray{T,1,typeof(A),typeof(I),fast}
end
slice_dummy(S::Slice) = S
slice_dummy{T}(::AbstractUnitRange{T}) = one(T)

## fast clever sorting for floats ##

module Float
using ..Sort
using ...Order

import Core.Intrinsics: slt_int
import ..Sort: sort!
import ...Order: lt, DirectOrdering

typealias Floats Union{Float32,Float64}

immutable Left <: Ordering end
immutable Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order), o.data)
right(o::Perm) = Perm(right(o.order), o.data)

lt{T<:Floats}(::Left, x::T, y::T) = slt_int(y, x)
lt{T<:Floats}(::Right, x::T, y::T) = slt_int(x, y)

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::Perm, i::Int) = isnan(o.order,o.data[i])

function nans2left!(v::AbstractVector, o::Ordering, lo::Int=first(indices(v,1)), hi::Int=last(indices(v,1)))
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
function nans2right!(v::AbstractVector, o::Ordering, lo::Int=first(indices(v,1)), hi::Int=last(indices(v,1)))
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
nans2end!{O<:ForwardOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2right!(v,o)
nans2end!{O<:ReverseOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2left!(v,o)

issignleft(o::ForwardOrdering, x::Floats) = lt(o, x, zero(x))
issignleft(o::ReverseOrdering, x::Floats) = lt(o, x, -zero(x))
issignleft(o::Perm, i::Int) = issignleft(o.order, o.data[i])

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
    sort!(v, first(indices(v,1)), last(indices(v,1)), a, o)

sort!{T<:Floats}(v::AbstractVector{T}, a::Algorithm, o::DirectOrdering) = fpsort!(v,a,o)
sort!{O<:DirectOrdering,T<:Floats}(v::Vector{Int}, a::Algorithm, o::Perm{O,Vector{T}}) = fpsort!(v,a,o)

end # module Sort.Float

end # module Sort
