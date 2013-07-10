module Sort

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm

export # also exported by Base
    issorted,
    sort,
    sort!,
    sortperm,
    sortrows,
    sortcols,
    select,
    select!,
    searchsorted,
    searchsortedfirst,
    searchsortedlast,
    InsertionSort,
    QuickSort,
    MergeSort,
    TimSort

export # not exported by Base
    Ordering, Algorithm,
    Forward, By, Lt, lt,
    ReverseOrdering, ForwardOrdering,
    # Reverse, # TODO: clashes with Reverse iterator
    DEFAULT_UNSTABLE,
    DEFAULT_STABLE,
    SMALL_ALGORITHM,
    SMALL_THRESHOLD

# not exported
    # selectby
    # selectby!
    # sortpermby

## notions of element ordering ##

abstract Ordering

type ForwardOrdering <: Ordering end
type ReverseOrdering <: Ordering end
immutable By <: Ordering by::Function end
immutable Lt <: Ordering lt::Function end

const Forward = ForwardOrdering()
const Reverse = ReverseOrdering()

lt(o::ForwardOrdering, a, b) = isless(a,b)
lt(o::ReverseOrdering, a, b) = isless(b,a)
lt(o::By,              a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,              a, b) = o.lt(a,b)

ord(lt::Function, by::Function, order::Ordering) =
    (lt === isless) & (by === identity) ? order :
    (lt === isless) ? By(by) : Lt(lt)

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
issorted(itr; lt::Function=isless, by::Function=identity, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,order))

function select!(v::AbstractVector, k::Int, lo::Int, hi::Int, o::Ordering)
    lo <= k <= hi || error("select index $k is out of range $lo:$hi")
    while lo < hi
        if hi-lo == 1
            if lt(o, v[hi], v[lo])
                v[lo], v[hi] = v[hi], v[lo]
            end
            return v[k]
        end
        pivot = v[(lo+hi)>>>1]
        i, j = lo, hi
        while true
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            i <= j || break
            v[i], v[j] = v[j], v[i]
            i += 1; j -= 1
        end
        if k <= j
            hi = j
        elseif i <= k
            lo = i
        else
            return pivot
        end
    end
    return v[lo]
end

function select!(v::AbstractVector, r::Range1, lo::Int, hi::Int, o::Ordering)
    a, b = first(r), last(r)
    lo <= a <= b <= hi || error("select index $k is out of range $lo:$hi")
    while true
        if lo == a && hi == b
            sort!(v, lo, hi, DEFAULT_UNSTABLE, o)
            return v[r]
        end
        pivot = v[(lo+hi)>>>1]
        i, j = lo, hi
        while true
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            i <= j || break
            v[i], v[j] = v[j], v[i]
            i += 1; j -= 1
        end
        if b <= j
            hi = j
        elseif i <= a
            lo = i
        else
            a <= j && select!(v, a, lo,  j, o)
            b >= i && select!(v, b,  i, hi, o)
            sort!(v, a, b, DEFAULT_UNSTABLE, o)
            return v[r]
        end
    end
end

select!(v::AbstractVector, k, o::Ordering) = select!(v,k,1,length(v),o)
select!(v::AbstractVector, k;
    lt::Function=isless, by::Function=identity, order::Ordering=Forward) =
    select!(v, k, ord(lt,by,order))

select(v::AbstractVector, k; kws...) = select!(copy(v), k; kws...)

# reference on sorted binary search:
#   http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equal to x;
# returns length(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::Int, hi::Int, o::Ordering)
    lo = lo-1
    hi = hi+1
    while lo < hi-1
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
    while lo < hi-1
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
function searchsorted(v::AbstractVector, x, lo::Int, hi::Int, o::Ordering)
    lo = lo-1
    hi = hi+1
    while lo < hi-1
        m = (lo+hi)>>>1
        if lt(o, v[m], x)
            lo = m
        elseif lt(o, x, v[m])
            hi = m
        else
            a = searchsortedfirst(v, x, max(lo,1), m, o)
            b = searchsortedlast(v, x, m, min(hi,length(v)), o)
            return a:b
        end
    end
    return lo+1:hi-1
end

for s in {:searchsortedfirst, :searchsortedlast, :searchsorted}
    @eval $s(v::AbstractVector, x; order::Ordering=Forward) = $s(v,x,1,length(v),order)
end

function searchsortedlast{T<:Real}(a::Ranges{T}, x::Real; order::Ordering=Forward)
    if step(a) == 0
        lt(order, x, first(a)) ? 0 : length(a)
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(order, x, a[n]) ? n-1 : n
    end
end

function searchsortedfirst{T<:Real}(a::Ranges{T}, x::Real; order::Ordering=Forward)
    if step(a) == 0
        lt(order, first(a), x) ? length(a)+1 : 1
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(order, a[n] ,x) ? n+1 : n
    end
end

function searchsortedlast{T<:Integer}(a::Ranges{T}, x::Real; order::Ordering=Forward)
    if step(a) == 0
        lt(order, x, first(a)) ? 0 : length(a)
    else
        max(min(fld(ifloor(x)-first(a),step(a))+1,length(a)),0)
    end
end

function searchsortedfirst{T<:Integer}(a::Ranges{T}, x::Real; order::Ordering=Forward)
    if step(a) == 0
        lt(order, first(a), x) ? length(a)+1 : 1
    else
        max(min(-fld(ifloor(-x)+first(a),step(a))+1,length(a)+1),1)
    end
end

searchsorted{T<:Real}(a::Ranges{T}, x::Real; order::Ordering=Forward) =
    searchsortedfirst(a,x,order=order):searchsortedlast(a,x,order=order)

## sorting algorithms ##

abstract Algorithm

type InsertionSortAlg <: Algorithm end
type QuickSortAlg     <: Algorithm end
type MergeSortAlg     <: Algorithm end
type TimSortAlg       <: Algorithm end

const InsertionSort = InsertionSortAlg()
const QuickSort     = QuickSortAlg()
const MergeSort     = MergeSortAlg()
const TimSort       = TimSortAlg()

const DEFAULT_UNSTABLE = QuickSort
const DEFAULT_STABLE   = MergeSort
const SMALL_ALGORITHM  = InsertionSort
const SMALL_THRESHOLD  = 20

function sort!(v::AbstractVector, lo::Int, hi::Int, ::InsertionSortAlg, o::Ordering)
    for i = lo+1:hi
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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::QuickSortAlg, o::Ordering)
    while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        pivot = v[(lo+hi)>>>1]
        i, j = lo, hi
        while true
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            i <= j || break
            v[i], v[j] = v[j], v[i]
            i += 1; j -= 1
        end
        lo < j && sort!(v, lo, j, a, o)
        lo = i
    end
    return v
end

function sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSortAlg, o::Ordering, t=similar(v))
    if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

        m = (lo+hi)>>>1
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

include("timsort.jl")

## sortperm: the permutation to sort an array ##

immutable Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    order::O
    data::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(p::Perm, a, b) = lt(p.order, p.data[a], p.data[b])

## generic sorting methods ##

defalg(v::AbstractArray) = DEFAULT_STABLE
defalg{T<:Number}(v::AbstractArray{T}) = DEFAULT_UNSTABLE

sort!(v::AbstractVector, alg::Algorithm, order::Ordering) = sort!(v,1,length(v),alg,order)
sort!(v::AbstractVector; alg::Algorithm=defalg(v),
    lt::Function=isless, by::Function=identity, order::Ordering=Forward) =
    sort!(v, alg, ord(lt,by,order))

sortperm(v::AbstractVector; alg::Algorithm=defalg(v),
    lt::Function=isless, by::Function=identity, order::Ordering=Forward) =
    sort!([1:length(v)], alg, Perm(ord(lt,by,order),v))

sort(v::AbstractVector; kws...) = sort!(copy(v); kws...)

## sorting multi-dimensional arrays ##

sort(A::AbstractArray, dim::Integer; kws...) = mapslices(a->sort(a; kws...), A, [dim])

function sortrows(A::AbstractMatrix; kws...)
    c = 1:size(A,2)
    rows = [ sub(A,i,c) for i=1:size(A,1) ]
    p = sortperm(rows; kws...)
    A[p,:]
end

function sortcols(A::AbstractMatrix; kws...)
    r = 1:size(A,1)
    cols = [ sub(A,r,i) for i=1:size(A,2) ]
    p = sortperm(cols; kws...)
    A[:,p]
end

## fast clever sorting for floats ##

module Float
using ..Sort

import Core.Intrinsics: unbox, slt_int
import ..Sort: sort!, Perm, lt, Reverse

typealias Floats Union(Float32,Float64)
typealias Direct Union(ForwardOrdering,ReverseOrdering)

type Left <: Ordering end
type Right <: Ordering end

left(::Direct) = Left()
right(::Direct) = Right()

left{O<:Direct}(o::Perm{O}) = Perm(left(O()),o.data)
right{O<:Direct}(o::Perm{O}) = Perm(right(O()),o.data)

lt{T<:Floats}(::Left, x::T, y::T) = slt_int(unbox(T,y),unbox(T,x))
lt{T<:Floats}(::Right, x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))

isnan(o::Direct, x::Floats) = (x!=x)
isnan{O<:Direct}(o::Perm{O}, i::Int) = isnan(O(),o.data[i])

function nans2left!(v::AbstractVector, o::Ordering, lo::Int=1, hi::Int=length(v))
    hi < lo && return lo, hi
    i = lo
    while (i < hi) & isnan(o, v[i])
        i += 1
    end
    r = 0
    while true
        if isnan(o, v[i])
            i += 1
        else
            r += 1
        end
        j = i + r
        j > hi && break
        if r > 0
            v[i], v[j] = v[j], v[i]
        end
    end
    return i, hi
end
function nans2right!(v::AbstractVector, o::Ordering, lo::Int=1, hi::Int=length(v))
    hi < lo && return lo, hi
    i = hi
    while (i > lo) & isnan(o, v[i])
        i -= 1
    end
    r = 0
    while true
        if isnan(o, v[i])
            i -= 1
        else
            r += 1
        end
        j = i - r
        j < lo && break
        if r > 0
            v[i], v[j] = v[j], v[i]
        end
    end
    return lo, i
end

nans2end!(v::AbstractVector, o::ForwardOrdering) = nans2right!(v,o)
nans2end!(v::AbstractVector, o::ReverseOrdering) = nans2left!(v,o)
nans2end!{O<:ForwardOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2right!(v,o)
nans2end!{O<:ReverseOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2left!(v,o)

issignleft(o::Direct, x::Floats) = lt(o, x, zero(x))
issignleft{O<:Direct}(o::Perm{O}, i::Int) = issignleft(O(), o.data[i])

function fpsort!(v::AbstractVector, a::Algorithm, o::Ordering)
    i, j = lo, hi = nans2end!(v,o)
    while true
        while i <= j &&  issignleft(o,v[i]); i += 1; end
        while i <= j && !issignleft(o,v[j]); j -= 1; end
        if i <= j
            v[i], v[j] = v[j], v[i]
            i += 1
            j -= 1
        else
            break
        end
    end
    sort!(v, lo, j,  a, left(o))
    sort!(v, i,  hi, a, right(o))
    return v
end

sort!{T<:Floats}(v::AbstractVector{T}, a::Algorithm, o::Direct) = fpsort!(v,a,o)
sort!{O<:Direct,T<:Floats}(v::Vector{Int}, a::Algorithm, o::Perm{O,Vector{T}}) = fpsort!(v,a,o)

end # module Sort.Float

end # module Sort
