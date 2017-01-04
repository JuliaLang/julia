# This file is a part of Julia. License is MIT: http://julialang.org/license

module Collections

import Base: setindex!, done, get, hash, haskey, isempty, length, next, getindex, start, copymutable
import ..Order: Forward, Ordering, lt

export
    PriorityQueue,
    dequeue!,
    enqueue!,
    heapify!,
    heapify,
    heappop!,
    heappush!,
    isheap,
    peek


# Some algorithms that can be defined only after infrastructure is in place
Base.append!(a::Vector, iter) = _append!(a, Base.iteratorsize(iter), iter)

function _append!(a, ::Base.HasLength, iter)
    n = length(a)
    resize!(a, n+length(iter))
    @inbounds for (i,item) in zip(n+1:length(a), iter)
        a[i] = item
    end
    a
end

function _append!(a, ::Base.IteratorSize, iter)
    for item in iter
        push!(a, item)
    end
    a
end

# Heap operations on flat arrays
# ------------------------------


# Binary heap indexing
heapleft(i::Integer) = 2i
heapright(i::Integer) = 2i + 1
heapparent(i::Integer) = div(i, 2)


# Binary min-heap percolate down.
function percolate_down!(xs::AbstractArray, i::Integer, x=xs[i], o::Ordering=Forward, len::Integer=length(xs))
    @inbounds while (l = heapleft(i)) <= len
        r = heapright(i)
        j = r > len || lt(o, xs[l], xs[r]) ? l : r
        if lt(o, xs[j], x)
            xs[i] = xs[j]
            i = j
        else
            break
        end
    end
    xs[i] = x
end

percolate_down!(xs::AbstractArray, i::Integer, o::Ordering, len::Integer=length(xs)) = percolate_down!(xs, i, xs[i], o, len)


# Binary min-heap percolate up.
function percolate_up!(xs::AbstractArray, i::Integer, x=xs[i], o::Ordering=Forward)
    @inbounds while (j = heapparent(i)) >= 1
        if lt(o, x, xs[j])
            xs[i] = xs[j]
            i = j
        else
            break
        end
    end
    xs[i] = x
end

percolate_up!{T}(xs::AbstractArray{T}, i::Integer, o::Ordering) = percolate_up!(xs, i, xs[i], o)

"""
    heappop!(v, [ord])

Given a binary heap-ordered array, remove and return the lowest ordered element.
For efficiency, this function does not check that the array is indeed heap-ordered.
"""
function heappop!(xs::AbstractArray, o::Ordering=Forward)
    x = xs[1]
    y = pop!(xs)
    if !isempty(xs)
        percolate_down!(xs, 1, y, o)
    end
    x
end

"""
    heappush!(v, x, [ord])

Given a binary heap-ordered array, push a new element `x`, preserving the heap property.
For efficiency, this function does not check that the array is indeed heap-ordered.
"""
function heappush!(xs::AbstractArray, x, o::Ordering=Forward)
    push!(xs, x)
    percolate_up!(xs, length(xs), x, o)
    xs
end


# Turn an arbitrary array into a binary min-heap in linear time.
"""
    heapify!(v, ord::Ordering=Forward)

In-place [`heapify`](@ref).
"""
function heapify!(xs::AbstractArray, o::Ordering=Forward)
    for i in heapparent(length(xs)):-1:1
        percolate_down!(xs, i, o)
    end
    xs
end

"""
    heapify(v, ord::Ordering=Forward)

Returns a new vector in binary heap order, optionally using the given ordering.
```jldoctest
julia> a = [1,3,4,5,2];

julia> Base.Collections.heapify(a)
5-element Array{Int64,1}:
 1
 2
 4
 5
 3

julia> Base.Collections.heapify(a, Base.Order.Reverse)
5-element Array{Int64,1}:
 5
 3
 4
 1
 2
```
"""
heapify(xs::AbstractArray, o::Ordering=Forward) = heapify!(copymutable(xs), o)

"""
    isheap(v, ord::Ordering=Forward)

Return `true` if an array is heap-ordered according to the given order.

```jldoctest
julia> a = [1,2,3]
3-element Array{Int64,1}:
 1
 2
 3

julia> Base.Collections.isheap(a,Base.Order.Forward)
true

julia> Base.Collections.isheap(a,Base.Order.Reverse)
false
```
"""
function isheap(xs::AbstractArray, o::Ordering=Forward)
    for i in 1:div(length(xs), 2)
        if lt(o, xs[heapleft(i)], xs[i]) ||
           (heapright(i) <= length(xs) && lt(o, xs[heapright(i)], xs[i]))
            return false
        end
    end
    true
end


# PriorityQueue
# -------------

"""
    PriorityQueue(K, V, [ord])

Construct a new [`PriorityQueue`](@ref), with keys of type
`K` and values/priorites of type `V`.
If an order is not given, the priority queue is min-ordered using
the default comparison for `V`.

A `PriorityQueue` acts like a `Dict`, mapping values to their
priorities, with the addition of a `dequeue!` function to remove the
lowest priority element.

```jldoctest
julia> a = Base.Collections.PriorityQueue(["a","b","c"],[2,3,1],Base.Order.Forward)
Base.Collections.PriorityQueue{String,Int64,Base.Order.ForwardOrdering} with 3 entries:
  "c" => 1
  "b" => 3
  "a" => 2
```
"""
type PriorityQueue{K,V,O<:Ordering} <: Associative{K,V}
    # Binary heap of (element, priority) pairs.
    xs::Array{Pair{K,V}, 1}
    o::O

    # Map elements to their index in xs
    index::Dict{K, Int}

    function PriorityQueue(o::O)
        new(Array{Pair{K,V}}(0), o, Dict{K, Int}())
    end

    PriorityQueue() = PriorityQueue{K,V,O}(Forward)

    function PriorityQueue(ks::AbstractArray{K}, vs::AbstractArray{V},
                           o::O)
        # TODO: maybe deprecate
        if length(ks) != length(vs)
            throw(ArgumentError("key and value arrays must have equal lengths"))
        end
        PriorityQueue{K,V,O}(zip(ks, vs), o)
    end

    function PriorityQueue(itr, o::O)
        xs = Array{Pair{K,V}}(length(itr))
        index = Dict{K, Int}()
        for (i, (k, v)) in enumerate(itr)
            xs[i] = Pair{K,V}(k, v)
            if haskey(index, k)
                throw(ArgumentError("PriorityQueue keys must be unique"))
            end
            index[k] = i
        end
        pq = new(xs, o, index)

        # heapify
        for i in heapparent(length(pq.xs)):-1:1
            percolate_down!(pq, i)
        end

        pq
    end
end

PriorityQueue(o::Ordering=Forward) = PriorityQueue{Any,Any,typeof(o)}(o)
PriorityQueue{K,V}(::Type{K}, ::Type{V}, o::Ordering=Forward) = PriorityQueue{K,V,typeof(o)}(o)

# TODO: maybe deprecate
PriorityQueue{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V},
                   o::Ordering=Forward) = PriorityQueue{K,V,typeof(o)}(ks, vs, o)

PriorityQueue{K,V}(kvs::Associative{K,V}, o::Ordering=Forward) = PriorityQueue{K,V,typeof(o)}(kvs, o)

PriorityQueue{K,V}(a::AbstractArray{Tuple{K,V}}, o::Ordering=Forward) = PriorityQueue{K,V,typeof(o)}(a, o)

length(pq::PriorityQueue) = length(pq.xs)
isempty(pq::PriorityQueue) = isempty(pq.xs)
haskey(pq::PriorityQueue, key) = haskey(pq.index, key)

"""
    peek(pq)

Return the lowest priority key from a priority queue without removing that
key from the queue.
"""
peek(pq::PriorityQueue) = pq.xs[1]

function percolate_down!(pq::PriorityQueue, i::Integer)
    x = pq.xs[i]
    @inbounds while (l = heapleft(i)) <= length(pq)
        r = heapright(i)
        j = r > length(pq) || lt(pq.o, pq.xs[l].second, pq.xs[r].second) ? l : r
        if lt(pq.o, pq.xs[j].second, x.second)
            pq.index[pq.xs[j].first] = i
            pq.xs[i] = pq.xs[j]
            i = j
        else
            break
        end
    end
    pq.index[x.first] = i
    pq.xs[i] = x
end


function percolate_up!(pq::PriorityQueue, i::Integer)
    x = pq.xs[i]
    @inbounds while i > 1
        j = heapparent(i)
        if lt(pq.o, x.second, pq.xs[j].second)
            pq.index[pq.xs[j].first] = i
            pq.xs[i] = pq.xs[j]
            i = j
        else
            break
        end
    end
    pq.index[x.first] = i
    pq.xs[i] = x
end

# Equivalent to percolate_up! with an element having lower priority than any other
function force_up!(pq::PriorityQueue, i::Integer)
    x = pq.xs[i]
    @inbounds while i > 1
        j = heapparent(i)
        pq.index[pq.xs[j].first] = i
        pq.xs[i] = pq.xs[j]
        i = j
    end
    pq.index[x.first] = i
    pq.xs[i] = x
end

function getindex{K,V}(pq::PriorityQueue{K,V}, key)
    pq.xs[pq.index[key]].second
end


function get{K,V}(pq::PriorityQueue{K,V}, key, deflt)
    i = get(pq.index, key, 0)
    i == 0 ? deflt : pq.xs[i].second
end


# Change the priority of an existing element, or equeue it if it isn't present.
function setindex!{K,V}(pq::PriorityQueue{K, V}, value, key)
    if haskey(pq, key)
        i = pq.index[key]
        oldvalue = pq.xs[i].second
        pq.xs[i] = Pair{K,V}(key, value)
        if lt(pq.o, oldvalue, value)
            percolate_down!(pq, i)
        else
            percolate_up!(pq, i)
        end
    else
        enqueue!(pq, key, value)
    end
    value
end

"""
    enqueue!(pq, k, v)

Insert the a key `k` into a priority queue `pq` with priority `v`.

```jldoctest
julia> a = Base.Collections.PriorityQueue(["a","b","c"],[2,3,1],Base.Order.Forward)
Base.Collections.PriorityQueue{String,Int64,Base.Order.ForwardOrdering} with 3 entries:
  "c" => 1
  "b" => 3
  "a" => 2

julia> Base.Collections.enqueue!(a, "d", 4)
Base.Collections.PriorityQueue{String,Int64,Base.Order.ForwardOrdering} with 4 entries:
  "c" => 1
  "b" => 3
  "a" => 2
  "d" => 4
```
"""
function enqueue!{K,V}(pq::PriorityQueue{K,V}, key, value)
    if haskey(pq, key)
        throw(ArgumentError("PriorityQueue keys must be unique"))
    end
    push!(pq.xs, Pair{K,V}(key, value))
    pq.index[key] = length(pq)
    percolate_up!(pq, length(pq))
    pq
end

"""
    dequeue!(pq)

Remove and return the lowest priority key from a priority queue.

```jldoctest
julia> a = Base.Collections.PriorityQueue(["a","b","c"],[2,3,1],Base.Order.Forward)
Base.Collections.PriorityQueue{String,Int64,Base.Order.ForwardOrdering} with 3 entries:
  "c" => 1
  "b" => 3
  "a" => 2

julia> Base.Collections.dequeue!(a)
"c"

julia> a
Base.Collections.PriorityQueue{String,Int64,Base.Order.ForwardOrdering} with 2 entries:
  "b" => 3
  "a" => 2
```
"""
function dequeue!(pq::PriorityQueue)
    x = pq.xs[1]
    y = pop!(pq.xs)
    if !isempty(pq)
        pq.xs[1] = y
        pq.index[y.first] = 1
        percolate_down!(pq, 1)
    end
    delete!(pq.index, x.first)
    x.first
end

function dequeue!(pq::PriorityQueue, key)
    idx = pq.index[key]
    force_up!(pq, idx)
    dequeue!(pq)
    key
end

# Unordered iteration through key value pairs in a PriorityQueue
start(pq::PriorityQueue) = start(pq.index)

done(pq::PriorityQueue, i) = done(pq.index, i)

function next{K,V}(pq::PriorityQueue{K,V}, i)
    (k, idx), i = next(pq.index, i)
    return (pq.xs[idx], i)
end

end # module Collections
