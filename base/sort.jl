module Sort

import
    Base.sort,
    Base.sort!,
    Base.issorted,
    Base.sortperm

export # also exported by Base
    sort,
    sort!,
    sortperm,
    select,
    select!,
    issorted,
    searchsortedfirst,
    searchsortedlast

export # not exported by Base
    Ordering,
        Forward,
        # Reverse, # TODO: clashes with Reverse iterator
        By,
        Lt,
        lt,
    Algorithm,
        InsertionSort,
        QuickSort,
        MergeSort,
        TimSort,
    DEFAULT_UNSTABLE,
    DEFAULT_STABLE,
    SMALL_ALGORITHM,
    SMALL_THRESHOLD

## notions of element ordering ##

abstract Ordering

type Forward <: Ordering end
type Reverse <: Ordering end
type By <: Ordering by::Function end
type Lt <: Ordering lt::Function end

lt(o::Forward, a, b) = isless(a,b)
lt(o::Reverse, a, b) = isless(b,a)
lt(o::By,      a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,      a, b) = o.lt(a,b)

## functions requiring only ordering ##

function issorted(o::Ordering, itr)
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
issorted{T<:Ordering}(::Type{T}, itr) = issorted(T(), itr)
issorted(itr) = issorted(Forward(), itr)

function select!(o::Ordering, v::AbstractVector, k::Int, lo::Int, hi::Int)
    lo <= k <= hi || error("select index $k is out of range $lo:$hi")
    while lo < hi
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
select!(o::Ordering, v::AbstractVector, k::Int) = select!(o, v, k, 1, length(v))
select!{T<:Ordering}(::Type{T}, v::AbstractVector, k::Int) = select!(T(), v, k)
select!(v::AbstractVector, k::Int) = select!(Forward, v, k)

select(o::Ordering, v::AbstractVector, k::Int) = select!(o, copy(v), k)
select{T<:Ordering}(::Type{T}, v::AbstractVector, k::Int) = select(T(), v, k)
select(v::AbstractVector, k::Int) = select!(copy(v), k)

# reference on sorted binary search:
#   http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equal to x;
# returns length(v)+1 if x is greater than all values in v.
function searchsortedfirst(o::Ordering, v::AbstractVector, x, lo::Int, hi::Int)
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
function searchsortedlast(o::Ordering, v::AbstractVector, x, lo::Int, hi::Int)
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

for s in {:searchsortedfirst, :searchsortedlast}
    @eval begin
        $s(o::Ordering, v::AbstractVector, x) = $s(o, v, x, 1, length(v))
        $s{O<:Ordering}(::Type{O}, v::AbstractVector, x) = $s(O(), v, x)
        $s(v::AbstractVector, x) = $s(Forward(), v, x)
    end
end

## sorting algorithms ##

abstract Algorithm

type InsertionSort <: Algorithm end
type QuickSort     <: Algorithm end
type MergeSort     <: Algorithm end
type TimSort       <: Algorithm end

const DEFAULT_UNSTABLE = QuickSort()
const DEFAULT_STABLE   = MergeSort()
const SMALL_ALGORITHM  = InsertionSort()
const SMALL_THRESHOLD  = 20

sort!(a::Algorithm, o::Ordering, v::AbstractVector) = sort!(a, o, v, 1, length(v))
sort (a::Algorithm, o::Ordering, v::AbstractVector) = sort!(a, o, copy(v))

sort!{T<:Number}(o::Ordering, v::AbstractVector{T}) = sort!(DEFAULT_UNSTABLE, o, v)
sort {T<:Number}(o::Ordering, v::AbstractVector{T}) = sort (DEFAULT_UNSTABLE, o, v)

sort!(o::Ordering, v::AbstractVector) = sort!(DEFAULT_STABLE, o, v)
sort (o::Ordering, v::AbstractVector) = sort (DEFAULT_STABLE, o, v)

function sort!(::InsertionSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int)
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

function sort!(a::QuickSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int)
    while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(SMALL_ALGORITHM, o, v, lo, hi)
        pivot = v[(lo+hi)>>>1]
        i, j = lo, hi
        while true
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            i <= j || break
            v[i], v[j] = v[j], v[i]
            i += 1; j -= 1
        end
        lo < j && sort!(a, o, v, lo, j)
        lo = i
    end
    return v
end

function sort!(a::MergeSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int, t::AbstractVector)
    if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(SMALL_ALGORITHM, o, v, lo, hi)

        m = (lo+hi)>>>1
        sort!(a, o, v, lo,  m,  t)
        sort!(a, o, v, m+1, hi, t)

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
sort!(a::MergeSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int) = sort!(a,o,v,lo,hi,similar(v))

include("timsort.jl")

## sortperm: the permutation to sort an array ##

type Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    ord::O
    vec::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(p::Perm, a, b) = lt(p.ord, p.vec[a], p.vec[b])

sortperm(a::Algorithm, o::Ordering, v::AbstractVector) = sort(a, Perm(o,v), [1:length(v)])
sortperm(o::Ordering, v::AbstractVector) = sortperm(DEFAULT_STABLE, o, v)

# generic sorting methods

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s{A<:Algorithm,O<:Ordering}(::Type{A},    ::Type{O},   v::AbstractVector) = $s(A(), O(), v)
        $s{A<:Algorithm            }(::Type{A},    o::Ordering, v::AbstractVector) = $s(A(), o,   v)
        $s{             O<:Ordering}(a::Algorithm, ::Type{O},   v::AbstractVector) = $s(a,   O(), v)
        $s{A<:Algorithm            }(::Type{A},                 v::AbstractVector) = $s(A(),      v)
        $s{             O<:Ordering}(              ::Type{O},   v::AbstractVector) = $s(     O(), v)

        $s(a::Algorithm, v::AbstractVector) = $s(a, Forward(), v)
        $s(              v::AbstractVector) = $s(   Forward(), v)
    end
end

## fast clever sorting for floats ##

module Float
using Sort

import Sort.sort!, Sort.Perm, Sort.lt, Sort.Reverse
import Intrinsics.slt_int, Intrinsics.unbox

typealias Floats Union(Float32,Float64)
typealias Direct Union(Forward,Reverse)

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

function nans2left!(o::Ordering, v::AbstractVector, lo::Int, hi::Int)
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
function nans2right!(o::Ordering, v::AbstractVector, lo::Int, hi::Int)
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
nans2left!(o::Ordering, v::AbstractVector) = nans2left!(o,v,1,length(v))
nans2right!(o::Ordering, v::AbstractVector) = nans2right!(o,v,1,length(v))

nans2end!(o::Forward, v::AbstractVector) = nans2right!(o,v)
nans2end!(o::Reverse, v::AbstractVector) = nans2left!(o,v)
nans2end!{O<:Forward}(o::Perm{O}, v::AbstractVector{Int}) = nans2right!(o,v)
nans2end!{O<:Reverse}(o::Perm{O}, v::AbstractVector{Int}) = nans2left!(o,v)

issignleft(o::Direct, x::Floats) = lt(o, x, zero(x))
issignleft{O<:Direct}(o::Perm{O}, i::Int) = issignleft(O(),o.vec[i])

function fpsort!(a::Algorithm, o::Ordering, v::AbstractVector)
    i, j = lo, hi = nans2end!(o, v)
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
    sort!(a, left(o),  v, lo, j )
    sort!(a, right(o), v, i,  hi)
    return v
end

sort!{T<:Floats}(a::Algorithm, o::Direct, v::AbstractVector{T}) = fpsort!(a,o,v)
sort!{O<:Direct,T<:Floats}(a::Algorithm, o::Perm{O,Vector{T}}, v::Vector{Int}) = fpsort!(a,o,v)

end # module Sort.Float

end # module Sort
