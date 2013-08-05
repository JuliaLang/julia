module Sort

using Base.Order

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm,
    Base.Collections.heapify!,
    Base.Collections.percolate_down!

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
    sortperm,
    sortrows,
    sortcols,
    # algorithms:
    InsertionSort,
    QuickSort,
    MergeSort,
    TimSort,
    HeapSort,
    RadixSort

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
    lt::Function=isless, by::Function=identity, rev::Bool=false, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,rev,order))

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

select!(v::AbstractVector, k::Union(Int,Range1), o::Ordering) = select!(v,k,1,length(v),o)
select!(v::AbstractVector, k::Union(Int,Range1);
    lt::Function=isless, by::Function=identity, rev::Bool=false, order::Ordering=Forward) =
    select!(v, k, ord(lt,by,rev,order))

select(v::AbstractVector, k::Union(Int,Range1); kws...) = select!(copy(v), k; kws...)

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

function searchsortedlast{T<:Real}(a::Ranges{T}, x::Real, o::Ordering=Forward)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(o, x, a[n]) ? n-1 : n
    end
end

function searchsortedfirst{T<:Real}(a::Ranges{T}, x::Real, o::Ordering=Forward)
    if step(a) == 0
        lt(o, first(a), x) ? length(a)+1 : 1
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(o, a[n] ,x) ? n+1 : n
    end
end

function searchsortedlast{T<:Integer}(a::Ranges{T}, x::Real, o::Ordering=Forward)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        max(min(fld(ifloor(x)-first(a),step(a))+1,length(a)),0)
    end
end

function searchsortedfirst{T<:Integer}(a::Ranges{T}, x::Real, o::Ordering=Forward)
    if step(a) == 0
        lt(o, first(a), x) ? length(a)+1 : 1
    else
        max(min(-fld(ifloor(-x)+first(a),step(a))+1,length(a)+1),1)
    end
end

searchsorted{T<:Real}(a::Ranges{T}, x::Real; kws...) =
    searchsortedfirst(a,x; kws...):searchsortedlast(a,x; kws...)

for s in {:searchsortedfirst, :searchsortedlast, :searchsorted}
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = $s(v,x,1,length(v),o)
        $s(v::AbstractVector, x;
           lt::Function=isless, by::Function=identity, rev::Bool=false, order::Ordering=Forward) =
            $s(v,x,ord(lt,by,rev,order))
    end
end

## sorting algorithms ##

abstract Algorithm

immutable InsertionSortAlg <: Algorithm end
immutable QuickSortAlg     <: Algorithm end
immutable HeapSortAlg      <: Algorithm end
immutable MergeSortAlg     <: Algorithm end
immutable TimSortAlg       <: Algorithm end
immutable RadixSortAlg     <: Algorithm end

const InsertionSort = InsertionSortAlg()
const QuickSort     = QuickSortAlg()
const HeapSort      = HeapSortAlg()
const MergeSort     = MergeSortAlg()
const TimSort       = TimSortAlg()
const RadixSort     = RadixSortAlg()

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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::HeapSortAlg, o::Ordering)
    if lo > 1 || hi < length(v)
        return sort!(sub(v, lo:hi), 1, length(v), a, o)
    end
    r = ReverseOrdering(o)
    heapify!(v, r)
    for i = length(v):-1:2
        # Swap the root with i, the last unsorted position
        x = v[i]
        v[i] = v[1]
        # The heap portion now ends at position i-1, but needs fixing up
        # starting with the root
        percolate_down!(v,1,x,r,i-1)
    end
    v
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

const RADIX_SIZE = 11
const RADIX_MASK = 0x7FF

function sort!(vs::AbstractVector, lo::Int, hi::Int, ::RadixSortAlg, o::Ordering, ts=similar(vs))
    # Input checking
    if lo >= hi;  return vs;  end

    # Make sure we're sorting a bits type
    T = ordtype(o, vs)
    if !isbits(T)
        error("Radix sort only sorts bits types (got $T)")
    end

    # Init
    iters = iceil(sizeof(T)*8/RADIX_SIZE)
    bin = zeros(Uint32, 2^RADIX_SIZE, iters)
    if lo > 1;  bin[1,:] = lo-1;  end

    # Histogram for each element, radix
    for i = lo:hi
        v = uint_mapping(o, vs[i])
        for j = 1:iters
            idx = int((v >> (j-1)*RADIX_SIZE) & RADIX_MASK)+1
            @inbounds bin[idx,j] += 1
        end
    end

    # Sort!
    swaps = 0
    len = hi-lo+1
    for j = 1:iters
        # Unroll first data iteration, check for degenerate case
        v = uint_mapping(o, vs[hi])
        idx = int((v >> (j-1)*RADIX_SIZE) & RADIX_MASK)+1

        # are all values the same at this radix?
        if bin[idx,j] == len;  continue;  end

        cbin = cumsum(bin[:,j])
        ci = cbin[idx]
        ts[ci] = vs[hi]
        cbin[idx] -= 1

        # Finish the loop...
        @inbounds for i in hi-1:-1:lo
            v = uint_mapping(o, vs[i])
            idx = int((v >> (j-1)*RADIX_SIZE) & RADIX_MASK)+1
            ci = cbin[idx]
            ts[ci] = vs[i]
            cbin[idx] -= 1
        end
        vs,ts = ts,vs
        swaps += 1
    end

    if isodd(swaps)
        vs,ts = ts,vs
        copy!(vs,ts)
    end
    vs
end

include("timsort.jl")

## generic sorting methods ##

defalg(v::AbstractArray) = DEFAULT_STABLE
defalg{T<:Number}(v::AbstractArray{T}) = DEFAULT_UNSTABLE

sort!(v::AbstractVector, alg::Algorithm, order::Ordering) = sort!(v,1,length(v),alg,order)
sort!(v::AbstractVector; alg::Algorithm=defalg(v),
    lt::Function=isless, by::Function=identity, rev::Bool=false, order::Ordering=Forward) =
    sort!(v, alg, ord(lt,by,rev,order))

sort(v::AbstractVector; kws...) = sort!(copy(v); kws...)

## sortperm: the permutation to sort an array ##

sortperm(v::AbstractVector; alg::Algorithm=defalg(v),
    lt::Function=isless, by::Function=identity, rev::Bool=false, order::Ordering=Forward) =
    sort!([1:length(v)], alg, Perm(ord(lt,by,rev,order),v))

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
using ...Order

import Core.Intrinsics: unbox, slt_int
import ..Sort: sort!
import ...Order: lt, DirectOrdering, uint_mapping

typealias Floats Union(Float32,Float64)

immutable Left <: Ordering end
immutable Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order),o.data)
right(o::Perm) = Perm(right(o.order),o.data)

lt{T<:Floats}(::Left, x::T, y::T) = slt_int(unbox(T,y),unbox(T,x))
lt{T<:Floats}(::Right, x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::Perm, i::Int) = isnan(o.order,o.data[i])

uint_mapping{F<:Floats}(::Right, x::F) = uint_mapping(Forward, x)
#uint_mapping{F<:Floats}(::Left,  x::F) = uint_mapping(Base.Ordering.Reverse, x)  ## Manually inlined
uint_mapping(::Left, x::Float32) = (y = reinterpret(Int32, x); uint32(y < 0 ? y : ~(y $ typemin(Int32))))
uint_mapping(::Left, x::Float64) = (y = reinterpret(Int64, x); uint64(y < 0 ? y : ~(y $ typemin(Int64))))

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

issignleft(o::DirectOrdering, x::Floats) = lt(o, x, zero(x))
issignleft(o::Perm, i::Int) = issignleft(o.order, o.data[i])

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

sort!{T<:Floats}(v::AbstractVector{T}, a::Algorithm, o::DirectOrdering) = fpsort!(v,a,o)
sort!{O<:DirectOrdering,T<:Floats}(v::Vector{Int}, a::Algorithm, o::Perm{O,Vector{T}}) = fpsort!(v,a,o)

end # module Sort.Float

end # module Sort
