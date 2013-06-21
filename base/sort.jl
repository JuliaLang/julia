module Sort

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm

export # also exported by Base
    sort,
    sort!,
    sortby,
    sortby!,
    sortperm,
    select,
    select!,
    issorted,
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

## functions requiring only ordering ##

function issorted(itr, o::Ordering = Forward)
    state = start(itr)
    done(itr,state) && return true
    prev, state = next(itr, state)
    while !done(itr, state)
        this, state = next(itr, state)
        lt(o, this, prev) && return false
        prev = this
    end
    return true
end

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

select!(v::AbstractVector, k, o::Ordering=Forward) = select!(v, k, 1, length(v), o)
select (v::AbstractVector, k, o::Ordering=Forward) = select!(copy(v), k, o)

for s in {:select!, :select}
    @eval begin
        $s(v::AbstractVector, k::Int, lt::Function)  = $s(v, k, Sort.Lt(lt))
        $s(lt::Function, v::AbstractVector, k::Int)  = $s(v, k, lt)
    end
end

for s in {:selectby!, :selectby}
    @eval begin
        $s(v::AbstractVector, k::Int, by::Function)  = $s(v, k, Sort.By(by))
        $s(by::Function, v::AbstractVector, k::Int)  = $s(v, k, by)
    end
end

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
            return searchsortedfirst(v, x, max(lo,1), m, o):searchsortedlast(v, x, m, min(hi,length(v)), o)
        end
    end
    return lo+1:hi-1
end

for s in {:searchsortedfirst, :searchsortedlast, :searchsorted}
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = $s(v, x, 1, length(v), o)
        $s(v::AbstractVector, x)              = $s(v, x, Forward)
    end
end

function searchsortedlast{T<:Real}(a::Ranges{T},x::Real,o::Ordering)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(o,x,a[n]) ? n-1 : n
    end
end

function searchsortedfirst{T<:Real}(a::Ranges{T},x::Real,o::Ordering)
    if step(a) == 0
        lt(o, first(a), x) ? length(a) + 1 : 1
    else
        n = max(min(iround((x-first(a))/step(a))+1,length(a)),1)
        lt(o,a[n],x) ? n+1 : n
    end
end

function searchsortedlast{T<:Integer}(a::Ranges{T},x::Real,o::Ordering)
    if step(a) == 0
        lt(o, x, first(a)) ? 0 : length(a)
    else
        max(min(fld(ifloor(x)-first(a),step(a))+1,length(a)),0)
    end
end

function searchsortedfirst{T<:Integer}(a::Ranges{T},x::Real,o::Ordering)
    if step(a) == 0
        lt(o, first(a), x) ? length(a) + 1 : 1
    else
        max(min(-fld(ifloor(-x)+first(a),step(a))+1,length(a)+1),1)
    end
end

searchsorted{T <: Real}(a::Ranges{T}, x::Real) = searchsortedfirst(a,x):searchsortedlast(a,x)

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

sort!(v::AbstractVector, a::Algorithm, o::Ordering) = sort!(v, 1, length(v), a, o)
sort (v::AbstractVector, a::Algorithm, o::Ordering) = sort!(copy(v), a, o)

sort!{T<:Number}(v::AbstractVector{T}, o::Ordering) = sort!(v, DEFAULT_UNSTABLE, o)
sort {T<:Number}(v::AbstractVector{T}, o::Ordering) = sort (v, DEFAULT_UNSTABLE, o)

sort!(v::AbstractVector, o::Ordering) = sort!(v, DEFAULT_STABLE, o)
sort (v::AbstractVector, o::Ordering) = sort (v, DEFAULT_STABLE, o)

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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSortAlg, o::Ordering, t::AbstractVector)
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
sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSortAlg, o::Ordering) = sort!(v, lo, hi, a, o, similar(v))

include("timsort.jl")

## sortperm: the permutation to sort an array ##

immutable Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    ord::O
    vec::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(p::Perm, a, b) = lt(p.ord, p.vec[a], p.vec[b])

sortperm(v::AbstractVector, a::Algorithm, o::Ordering) = sort!([1:length(v)], a, Perm(o,v))
sortperm(v::AbstractVector, o::Ordering) = sortperm(v, DEFAULT_STABLE, o)

##############

# generic sorting methods

for s in {:sort!, :sort, :sortperm}
    @eval begin
        # default to forward sort ordering
        $s(v::AbstractVector, a::Algorithm) = $s(v, a, Forward)
        $s(v::AbstractVector              ) = $s(v,    Forward)

        # also allow ordering before algorithm
        $s(v::AbstractVector, o::Ordering, a::Algorithm) = $s(v, a, o)
    end
end

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s(v::AbstractVector, a::Algorithm, lt::Function) = $s(v, a, Sort.Lt(lt))
        $s(v::AbstractVector, lt::Function, a::Algorithm) = $s(v, a, lt)
        $s(v::AbstractVector, lt::Function)               = $s(v, Sort.Lt(lt))
        $s(lt::Function, v::AbstractVector, args...)      = $s(v, lt, args...)
    end
end

for (sb,s) in {(:sortby!, :sort!), (:sortby, :sort), (:sortpermby, :sortperm)}
    @eval begin
        $sb(v::AbstractVector, a::Algorithm, by::Function) = $s(v, a, Sort.By(by))
        $sb(v::AbstractVector, by::Function, a::Algorithm) = $s(v, a, Sort.By(by))
        $sb(v::AbstractVector, by::Function)               = $s(v, Sort.By(by))
        $sb(by::Function, v::AbstractVector, args...)      = $s(v, Sort.By(by), args...)
    end
end

## fast clever sorting for floats ##

module Float
using ..Sort

import ..Sort: sort!, Perm, lt, Reverse
import Core.Intrinsics.slt_int, Core.Intrinsics.unbox

typealias Floats Union(Float32,Float64)
typealias Direct Union(ForwardOrdering,ReverseOrdering)

type Left <: Ordering end
type Right <: Ordering end

left(::Direct) = Left()
right(::Direct) = Right()

left{O<:Direct}(o::Perm{O}) = Perm(left(O()),o.vec)
right{O<:Direct}(o::Perm{O}) = Perm(right(O()),o.vec)

lt{T<:Floats}(::Left, x::T, y::T) = slt_int(unbox(T,y),unbox(T,x))
lt{T<:Floats}(::Right, x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))

isnan(o::Direct, x::Floats) = (x!=x)
isnan{O<:Direct}(o::Perm{O}, i::Int) = isnan(O(),o.vec[i])

function nans2left!(v::AbstractVector, lo::Int, hi::Int, o::Ordering)
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
function nans2right!(v::AbstractVector, lo::Int, hi::Int, o::Ordering)
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
nans2left!(v::AbstractVector, o::Ordering) = nans2left!(v, 1, length(v), o)
nans2right!(v::AbstractVector, o::Ordering) = nans2right!(v, 1, length(v), o)

nans2end!(v::AbstractVector, o::ForwardOrdering) = nans2right!(v, o)
nans2end!(v::AbstractVector, o::ReverseOrdering) = nans2left!(v, o)
nans2end!{O<:ForwardOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2right!(v, o)
nans2end!{O<:ReverseOrdering}(v::AbstractVector{Int}, o::Perm{O}) = nans2left!(v, o)

issignleft(o::Direct, x::Floats) = lt(o, x, zero(x))
issignleft{O<:Direct}(o::Perm{O}, i::Int) = issignleft(O(), o.vec[i])

function fpsort!(v::AbstractVector, a::Algorithm, o::Ordering)
    i, j = lo, hi = nans2end!(v,o)
    while true
        while i <= j &&  issignleft(o, v[i]); i += 1; end
        while i <= j && !issignleft(o, v[j]); j -= 1; end
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
sort!{T<:Floats}(v::AbstractVector{T}, a::Algorithm, o::Direct) = fpsort!(v, a, o)
sort!{O<:Direct,T<:Floats}(v::Vector{Int}, a::Algorithm, o::Perm{O,Vector{T}}) = fpsort!(v, a, o)

end # module Sort.Float

end # module Sort

# sorting multi-dimensional arrays
sort(A::AbstractArray, dim::Integer, o::Base.Sort.Ordering = Base.Sort.Forward,
     alg::Base.Sort.Algorithm = DEFAULT_STABLE) =
    mapslices(sort, A, [dim])

sort(A::AbstractArray, dim::Integer, alg::Base.Sort.Algorithm) =
    sort(A, dim, Base.Sort.Forward, alg)

sort(A::AbstractArray, dim::Integer, alg::Base.Sort.Algorithm, o::Base.Sort.Ordering) =
    sort(A, dim, o, alg)

function sortrows(A::AbstractMatrix, o::Base.Sort.Ordering = Base.Sort.Forward,
                  alg::Base.Sort.Algorithm = DEFAULT_STABLE)
    c = 1:size(A,2)
    rows = [ sub(A,i,c) for i=1:size(A,1) ]
    p = sortperm(rows, o, alg)
    A[p,:]
end

sortrows(A::AbstractMatrix, alg::Base.Sort.Algorithm) =
    sortrows(A, Base.Sort.Forward, alg)

sortrows(A::AbstractMatrix, alg::Base.Sort.Algorithm, o::Base.Sort.Ordering) =
    sortrows(A, o, alg)

function sortcols(A::AbstractMatrix, o::Base.Sort.Ordering = Base.Sort.Forward,
                  alg::Base.Sort.Algorithm = DEFAULT_STABLE)
    r = 1:size(A,1)
    cols = [ sub(A,r,i) for i=1:size(A,2) ]
    p = sortperm(cols, o, alg)
    A[:,p]
end

sortcols(A::AbstractMatrix, alg::Base.Sort.Algorithm) =
    sortcols(A, Base.Sort.Forward, alg)

sortcols(A::AbstractMatrix, alg::Base.Sort.Algorithm, o::Base.Sort.Ordering) =
    sortcols(A, o, alg)
