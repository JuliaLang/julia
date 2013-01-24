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
        MergeSort

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
    while true
        if lo == hi
            return v[lo]
        end
        i, j = lo, hi
        pivot = v[(lo+hi)>>>1]
        while i < j
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            if !lt(o, v[i], v[j]) && !lt(o, v[j], v[i])
                i += 1
            elseif i < j
                v[i], v[j] = v[j], v[i]
            end
        end
        pivot_ind = j
        len = pivot_ind - lo + 1
        if k == len
            return v[pivot_ind]
        elseif k <  len
            hi = pivot_ind - 1
        else
            lo = pivot_ind + 1
            k = k - len
        end
    end
end
function select!(o::Ordering, v::AbstractVector, k::Int)
    1 <= k <= length(v) || error("select index $k is out of bounds")
    select!(o, v, k, 1, length(v))
end
select!{T<:Ordering}(::Type{T}, v::AbstractVector, k::Int) = select!(T(), v, k)
select!(v::AbstractVector, k::Int) = select!(Forward, v, k)

select(o::Ordering, v::AbstractVector, k::Int) = select!(o, copy(v), k)
select{T<:Ordering}(::Type{T}, v::AbstractVector, k::Int) = select!(T, copy(v), k)
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

sort!(a::Algorithm, o::Ordering, v::AbstractVector) = sort!(a, o, v, 1, length(v))
sort (a::Algorithm, o::Ordering, v::AbstractVector) = sort!(a, o, copy(v))

sort!{T<:Number}(o::Ordering, v::AbstractVector{T}) = sort!(QuickSort(), o, v)
sort {T<:Number}(o::Ordering, v::AbstractVector{T}) = sort (QuickSort(), o, v)

sort!(o::Ordering, v::AbstractVector) = sort!(MergeSort(), o, v)
sort (o::Ordering, v::AbstractVector) = sort (MergeSort(), o, v)

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

function sort!(::QuickSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int)
    while hi > lo
        if hi-lo <= 20
            return sort!(InsertionSort(), o, v, lo, hi)
        end
        i, j = lo, hi
        pivot = v[(lo+hi)>>>1]
        while i <= j
            while lt(o, v[i], pivot); i += 1; end
            while lt(o, pivot, v[j]); j -= 1; end
            if i <= j
                v[i], v[j] = v[j], v[i]
                i += 1
                j -= 1
            end
        end
        if lo < j
            sort!(QuickSort(), o, v, lo, j)
        end
        lo = i
    end
    return v
end

function sort!(::MergeSort, o::Ordering, v::AbstractVector, lo::Int, hi::Int, t::AbstractVector)
    if lo < hi
        if hi-lo <= 20
            return sort!(InsertionSort(), o, v, lo, hi)
        end

        m = (lo+hi)>>>1
        sort!(MergeSort(), o, v, lo,  m,  t)
        sort!(MergeSort(), o, v, m+1, hi, t)

        i = 1
        j = lo
        while j <= m
            t[i] = v[j]
            i += 1
            j += 1
        end

        i = 1
        k = lo
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

## sortperm: the permutation to sort an array ##

type Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    ord::O
    vec::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(p::Perm, a, b) = lt(p.ord, p.vec[a], p.vec[b])

sortperm(a::Algorithm, o::Ordering, v::AbstractVector) = sort(a, Perm(o,v), [1:length(v)])
sortperm(o::Ordering, v::AbstractVector) = sortperm(MergeSort(), o, v)

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

type Fwd <: Ordering end
type Rev <: Ordering end

left(::Forward) = Rev()
left(::Reverse) = Fwd()
right(::Forward) = Fwd()
right(::Reverse) = Rev()

left{O<:Direct}(o::Perm{O}) = Perm(left(O()),o.vec)
right{O<:Direct}(o::Perm{O}) = Perm(right(O()),o.vec)

lt{T<:Floats}(::Fwd, x::T, y::T) = slt_int(unbox(T,x),unbox(T,y))
lt{T<:Floats}(::Rev, x::T, y::T) = slt_int(unbox(T,y),unbox(T,x))

isnan(o::Direct, x::Floats) = (x!=x)
isnan{O<:Direct}(o::Perm{O}, i::Int) = isnan(O(),o.vec[i])

function nans2left!(o::Ordering, v::AbstractVector, lo::Int, hi::Int)
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
