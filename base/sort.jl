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
    searchsortedfirst,
    searchsortedlast,
    InsertionSort,
    QuickSort,
    MergeSort,
    TimSort

export # not exported by Base
    Ordering, Algorithm,
    Forward, By, Lt, lt,
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

type Forward <: Ordering end
type Reverse <: Ordering end
type By <: Ordering by::Function end
type Lt <: Ordering lt::Function end

lt(o::Forward, a, b) = isless(a,b)
lt(o::Reverse, a, b) = isless(b,a)
lt(o::By,      a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,      a, b) = o.lt(a,b)

## functions requiring only ordering ##

function issorted(itr, o::Ordering)
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
issorted{T<:Ordering}(itr, ::Type{T}) = issorted(itr, T())
issorted             (itr)            = issorted(itr, Forward())

function select!(v::AbstractVector, k::Int, lo::Int, hi::Int, o::Ordering)
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
select!             (v::AbstractVector, k::Int, o::Ordering) = select!(v, k, 1, length(v), o)
select!{T<:Ordering}(v::AbstractVector, k::Int, ::Type{T})   = select!(v, k, T())
select!             (v::AbstractVector, k::Int)              = select!(v, k, Forward)

select             (v::AbstractVector, k::Int, o::Ordering) = select!(copy(v), k, o)
select{T<:Ordering}(v::AbstractVector, k::Int, ::Type{T})   = select (v,       k, T())
select             (v::AbstractVector, k::Int)              = select!(copy(v), k)

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

for s in {:searchsortedfirst, :searchsortedlast}
    @eval begin
        $s             (v::AbstractVector, x, o::Ordering) = $s(v, x, 1, length(v), o)
        $s{O<:Ordering}(v::AbstractVector, x, ::Type{O})   = $s(v, x, O())
        $s             (v::AbstractVector, x)              = $s(v, x, Forward())
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

sort!(v::AbstractVector, a::Algorithm, o::Ordering) = sort!(v, 1, length(v), a, o)
sort (v::AbstractVector, a::Algorithm, o::Ordering) = sort!(copy(v), a, o)

sort!{T<:Number}(v::AbstractVector{T}, o::Ordering) = sort!(v, DEFAULT_UNSTABLE, o)
sort {T<:Number}(v::AbstractVector{T}, o::Ordering) = sort (v, DEFAULT_UNSTABLE, o)

sort!(v::AbstractVector, o::Ordering) = sort!(v, DEFAULT_STABLE, o)
sort (v::AbstractVector, o::Ordering) = sort (v, DEFAULT_STABLE, o)

function sort!(v::AbstractVector, lo::Int, hi::Int, ::InsertionSort, o::Ordering)
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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::QuickSort, o::Ordering)
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

function sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSort, o::Ordering, t::AbstractVector)
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
sort!(v::AbstractVector, lo::Int, hi::Int, a::MergeSort, o::Ordering) = sort!(v, lo, hi, a, o, similar(v))

include("timsort.jl")

## sortperm: the permutation to sort an array ##

type Perm{O<:Ordering,V<:AbstractVector} <: Ordering
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
        $s(v::AbstractVector, a::Algorithm) = $s(v, a, Forward())
        $s(v::AbstractVector              ) = $s(v,    Forward())

        # auto-instntiate algorithms and orderings from types
        $s{A<:Algorithm,O<:Ordering}(v::AbstractVector, ::Type{A},    ::Type{O})   = $s(v, A(), O())
        $s{A<:Algorithm            }(v::AbstractVector, ::Type{A},    o::Ordering) = $s(v, A(), o)
        $s{             O<:Ordering}(v::AbstractVector, a::Algorithm, ::Type{O})   = $s(v, a,   O())
        $s{A<:Algorithm            }(v::AbstractVector, ::Type{A})                 = $s(v, A())
        $s{             O<:Ordering}(v::AbstractVector,               ::Type{O})   = $s(v,      O())

        # also allow ordering before algorithm
        $s                          (v::AbstractVector, o::Ordering, a::Algorithm) = $s(v, a, o)
        $s{A<:Algorithm,O<:Ordering}(v::AbstractVector, ::Type{O},   ::Type{A})    = $s(v, A(), O())
        $s{A<:Algorithm            }(v::AbstractVector, o::Ordering, ::Type{A})    = $s(v, A(), o)
        $s{             O<:Ordering}(v::AbstractVector, ::Type{O},   a::Algorithm) = $s(v, a,   O())
    end
end

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s{A<:Algorithm}(v::AbstractVector, a::Union(A,Type{A}), lt::Function) = $s(v, a, Sort.Lt(lt))
        $s{A<:Algorithm}(v::AbstractVector, lt::Function, a::Union(A,Type{A})) = $s(v, a, lt)
        $s              (v::AbstractVector, lt::Function)                      = $s(v, Sort.Lt(lt))
        $s              (lt::Function, v::AbstractVector, args...)             = $s(v, lt, args...)
    end
end

for (sb,s) in {(:sortby!, :sort!), (:sortby, :sort), (:sortpermby, :sortperm)}
    @eval begin
        $sb{A<:Algorithm}(v::AbstractVector, a::Union(A,Type{A}), by::Function) = $s(v, a, Sort.By(by))
        $sb{A<:Algorithm}(v::AbstractVector, by::Function, a::Union(A,Type{A})) = $s(v, a, by)
        $sb              (v::AbstractVector, by::Function)                      = $s(v, Sort.By(by))
        $sb              (by::Function, v::AbstractVector, args...)             = $s(v, Sort.By(by), args...)
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

nans2end!(v::AbstractVector, o::Forward) = nans2right!(v, o)
nans2end!(v::AbstractVector, o::Reverse) = nans2left!(v, o)
nans2end!{O<:Forward}(v::AbstractVector{Int}, o::Perm{O}) = nans2right!(v, o)
nans2end!{O<:Reverse}(v::AbstractVector{Int}, o::Perm{O}) = nans2left!(v, o)

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
