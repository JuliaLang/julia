# This file is a part of Julia. License is MIT: http://julialang.org/license

module Sort

using Base.Order

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm

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
issorted(itr;
    lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,rev,order))

function select!(v::AbstractVector, k::Union{Int,OrdinalRange}, o::Ordering)
    sort!(v, 1, length(v), PartialQuickSort(k), o)
    v[k]
end
select!(v::AbstractVector, k::Union{Int,OrdinalRange};
    lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
    select!(v, k, ord(lt,by,rev,order))

select(v::AbstractVector, k::Union{Int,OrdinalRange}; kws...) = select!(copy(v), k; kws...)


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
    if step(a) == 0
        lt(o, first(a), x) ? length(a) + 1 : 1
    else
        clamp(-fld(first(a) - signed(x), step(a)) + 1, 1, length(a) + 1)
    end
end

function searchsortedlast{T<:Integer}(a::Range{T}, x::Unsigned, o::DirectOrdering)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        clamp( fld(signed(x) - first(a), step(a)) + 1, 0, length(a))
    end
end

searchsorted{T<:Real}(a::Range{T}, x::Real, o::DirectOrdering) =
    searchsortedfirst(a, x, o) : searchsortedlast(a, x, o)

for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = $s(v,x,1,length(v),o)
        $s(v::AbstractVector, x;
           lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward) =
            $s(v,x,ord(lt,by,rev,order))
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
        isempty(t) && resize!(t, m-lo+1)

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

sort!(v::AbstractVector, alg::Algorithm, order::Ordering) = sort!(v,1,length(v),alg,order)

function sort!(v::AbstractVector;
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Bool=false,
               order::Ordering=Forward)
    sort!(v, alg, ord(lt,by,rev,order))
end

sort(v::AbstractVector; kws...) = sort!(copy(v); kws...)


## selectperm: the permutation to sort the first k elements of an array ##

selectperm(v::AbstractVector, k::Union{Integer,OrdinalRange}; kwargs...) =
    selectperm!(Vector{eltype(k)}(length(v)), v, k; kwargs..., initialized=false)

function selectperm!{I<:Integer}(ix::AbstractVector{I}, v::AbstractVector,
                                 k::Union{Int, OrdinalRange};
                                 lt::Function=isless,
                                 by::Function=identity,
                                 rev::Bool=false,
                                 order::Ordering=Forward,
                                 initialized::Bool=false)
    if !initialized
        @inbounds for i = 1:length(ix)
            ix[i] = i
        end
    end

    # do partial quicksort
    sort!(ix, PartialQuickSort(k), Perm(ord(lt, by, rev, order), v))
    return ix[k]
end

## sortperm: the permutation to sort an array ##

function sortperm(v::AbstractVector;
                  alg::Algorithm=DEFAULT_UNSTABLE,
                  lt=isless,
                  by=identity,
                  rev::Bool=false,
                  order::Ordering=Forward)
    sort!(collect(1:length(v)), alg, Perm(ord(lt,by,rev,order),v))
end

function sortperm!{I<:Integer}(x::AbstractVector{I}, v::AbstractVector;
                               alg::Algorithm=DEFAULT_UNSTABLE,
                               lt=isless,
                               by=identity,
                               rev::Bool=false,
                               order::Ordering=Forward,
                               initialized::Bool=false)
    lx = length(x)
    lv = length(v)
    if lx != lv
        throw(ArgumentError("index vector must be the same length as the source vector, $lx != $lv"))
    end
    if !initialized
        @inbounds for i = 1:lv
            x[i] = i
        end
    end
    sort!(x, alg, Perm(ord(lt,by,rev,order),v))
end

## sorting multi-dimensional arrays ##

sort(A::AbstractArray, dim::Integer; kws...) = mapslices(a->sort(a; kws...), A, [dim])

function sortrows(A::AbstractMatrix; kws...)
    c = 1:size(A,2)
    rows = [ sub(A,i,c) for i=1:size(A,1) ]
    p = sortperm(rows; kws..., order=Lexicographic)
    A[p,:]
end

function sortcols(A::AbstractMatrix; kws...)
    r = 1:size(A,1)
    cols = [ sub(A,r,i) for i=1:size(A,2) ]
    p = sortperm(cols; kws..., order=Lexicographic)
    A[:,p]
end

## fast clever sorting for floats ##

module Float
using ..Sort
using ...Order

import Core.Intrinsics: unbox, slt_int
import ..Sort: sort!
import ...Order: lt, DirectOrdering

typealias Floats Union{Float32,Float64}

immutable Left <: Ordering end
immutable Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order), o.data)
right(o::Perm) = Perm(right(o.order), o.data)

lt{T<:Floats}(::Left, x::T, y::T) = slt_int(unbox(T,y),unbox(T,x))
lt{T<:Floats}(::Right, x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::Perm, i::Int) = isnan(o.order,o.data[i])

function nans2left!(v::AbstractVector, o::Ordering, lo::Int=1, hi::Int=length(v))
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
function nans2right!(v::AbstractVector, o::Ordering, lo::Int=1, hi::Int=length(v))
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
    sort!(v, 1, length(v), a, o)

sort!{T<:Floats}(v::AbstractVector{T}, a::Algorithm, o::DirectOrdering) = fpsort!(v,a,o)
sort!{O<:DirectOrdering,T<:Floats}(v::Vector{Int}, a::Algorithm, o::Perm{O,Vector{T}}) = fpsort!(v,a,o)

end # module Sort.Float

end # module Sort
