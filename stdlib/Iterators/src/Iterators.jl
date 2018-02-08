module Iterators

export enumerate, zip, rest, countfrom, take, drop, repeated, product, flatten, partition, cycle

import Base.Iter: enumerate, zip, rest, countfrom, take, drop, repeated, product, flatten, peel

# not exported, to avoid conflicts with Base:
# filter, reverse

import Base:
    start, done, next, first, last, isempty, length, size,
    eltype, IteratorSize, IteratorEltype, peek

using Base: SizeUnknown, HasLength, HasShape, IsInfinite, EltypeUnknown, HasEltype

import Base.Iter: reverse, Reverse, Pairs, Filter, Flatten, Stateful

"""
    Iterators.reverse(itr)

Given an iterator `itr`, then `reverse(itr)` is an iterator over the
same collection but in the reverse order.

This iterator is "lazy" in that it does not make a copy of the collection in
order to reverse it; see [`Base.reverse`](@ref) for an eager implementation.

Not all iterator types `T` support reverse-order iteration.  If `T`
doesn't, then iterating over `Iterators.reverse(itr::T)` will throw a [`MethodError`](@ref)
because of the missing [`start`](@ref), [`next`](@ref), and [`done`](@ref)
methods for `Iterators.Reverse{T}`.  (To implement these methods, the original iterator
`itr::T` can be obtained from `r = Iterators.reverse(itr)` by `r.itr`.)

# Examples
```jldoctest
julia> foreach(println, Iterators.reverse(1:5))
5
4
3
2
1
```
"""
reverse

# Cycle an iterator forever

struct Cycle{I}
    xs::I
end

"""
    cycle(iter)

An iterator that cycles through `iter` forever.

# Examples
```jldoctest
julia> for (i, v) in enumerate(Iterators.cycle("hello"))
           print(v)
           i > 10 && break
       end
hellohelloh
```
"""
cycle(xs) = Cycle(xs)

eltype(::Type{Cycle{I}}) where {I} = eltype(I)
IteratorEltype(::Type{Cycle{I}}) where {I} = IteratorEltype(I)
IteratorSize(::Type{Cycle{I}}) where {I} = IsInfinite()

function start(it::Cycle)
    s = start(it.xs)
    return s, done(it.xs, s)
end

function next(it::Cycle, state)
    s, d = state
    if done(it.xs, s)
        s = start(it.xs)
    end
    v, s = next(it.xs, s)
    return v, (s, false)
end

done(it::Cycle, state) = state[2]

reverse(it::Cycle) = Cycle(reverse(it.xs))

## lazy filter

"""
    Iterators.filter(flt, itr)

Given a predicate function `flt` and an iterable object `itr`, return an
iterable object which upon iteration yields the elements `x` of `itr` that
satisfy `flt(x)`. The order of the original iterator is preserved.

This function is *lazy*; that is, it is guaranteed to return in ``Θ(1)`` time
and use ``Θ(1)`` additional space, and `flt` will not be called by an
invocation of `filter`. Calls to `flt` will be made when iterating over the
returned iterable object. These calls are not cached and repeated calls will be
made when reiterating.

See [`Base.filter`](@ref) for an eager implementation of filtering for arrays.

# Examples
```jldoctest
julia> f = Iterators.filter(isodd, [1, 2, 3, 4, 5])
Base.Iter.Filter{Base.#isodd,Array{Int64,1}}(isodd, [1, 2, 3, 4, 5])

julia> foreach(println, f)
1
3
5
```
"""
filter(flt, itr) = Base.Iter.Filter(flt, itr)

## partition

"""
    partition(collection, n)

Iterate over a collection `n` elements at a time.

# Examples
```jldoctest
julia> collect(Iterators.partition([1,2,3,4,5], 2))
3-element Array{Array{Int64,1},1}:
 [1, 2]
 [3, 4]
 [5]
```
"""
partition(c::T, n::Integer) where {T} = PartitionIterator{T}(c, Int(n))

mutable struct PartitionIterator{T}
    c::T
    n::Int
end

eltype(::Type{PartitionIterator{T}}) where {T} = Vector{eltype(T)}
partition_iteratorsize(::HasShape) = HasLength()
partition_iteratorsize(isz) = isz
function IteratorSize(::Type{PartitionIterator{T}}) where {T}
    partition_iteratorsize(IteratorSize(T))
end

function length(itr::PartitionIterator)
    l = length(itr.c)
    return div(l, itr.n) + ((mod(l, itr.n) > 0) ? 1 : 0)
end

start(itr::PartitionIterator) = start(itr.c)

done(itr::PartitionIterator, state) = done(itr.c, state)

function next(itr::PartitionIterator{<:Vector}, state)
    l = state
    r = min(state + itr.n-1, length(itr.c))
    return view(itr.c, l:r), r + 1
end

function next(itr::PartitionIterator, state)
    v = Vector{eltype(itr.c)}(uninitialized, itr.n)
    i = 0
    while !done(itr.c, state) && i < itr.n
        i += 1
        v[i], state = next(itr.c, state)
    end
    return resize!(v, i), state
end

end
