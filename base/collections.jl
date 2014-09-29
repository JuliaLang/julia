module Collections

import Base: setindex!, done, get, hash, haskey, isempty, length, next, getindex, start
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


# Binary min-heap pop.
function heappop!(xs::AbstractArray, o::Ordering=Forward)
    x = xs[1]
    y = pop!(xs)
    if !isempty(xs)
        percolate_down!(xs, 1, y, o)
    end
    x
end


# Binary min-heap push.
function heappush!(xs::AbstractArray, x, o::Ordering=Forward)
    push!(xs, x)
    percolate_up!(xs, length(xs), x, o)
    xs
end


# Turn an arbitrary array into a binary min-heap in linear time.
function heapify!(xs::AbstractArray, o::Ordering=Forward)
    for i in heapparent(length(xs)):-1:1
        percolate_down!(xs, i, o)
    end
    xs
end

heapify(xs::AbstractArray, o::Ordering=Forward) = heapify!(copy(xs), o)


# Is an arbitrary array heap ordered?
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

# A PriorityQueue that acts like a Dict, mapping values to their priorities,
# with the addition of a dequeue! function to remove the lowest priority
# element.
type PriorityQueue{K,V,O<:Ordering} <: Associative{K,V}
    # Binary heap of (element, priority) pairs.
    xs::Array{Pair{K,V}, 1}
    o::O

    # Map elements to their index in xs
    index::Dict{K, Int}

    function PriorityQueue(o::O)
        new(Array(Pair{K,V}, 0), o, Dict{K, Int}())
    end

    PriorityQueue() = PriorityQueue{K,V,O}(Forward)

    function PriorityQueue(ks::AbstractArray{K}, vs::AbstractArray{V},
                           o::O)
        # TODO: maybe deprecate
        if length(ks) != length(vs)
            error("key and value arrays must have equal lengths")
        end
        PriorityQueue{K,V,O}(zip(ks, vs), o)
    end

    function PriorityQueue(itr, o::O)
        xs = Array(Pair{K,V}, length(itr))
        index = Dict{K, Int}()
        for (i, (k, v)) in enumerate(itr)
            xs[i] = Pair{K,V}(k, v)
            if haskey(index, k)
                error("PriorityQueue keys must be unique")
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

PriorityQueue{K,V}(a::AbstractArray{(K,V)}, o::Ordering=Forward) = PriorityQueue{K,V,typeof(o)}(a, o)

length(pq::PriorityQueue) = length(pq.xs)
isempty(pq::PriorityQueue) = isempty(pq.xs)
haskey(pq::PriorityQueue, key) = haskey(pq.index, key)
peek(pq::PriorityQueue) = (kv = pq.xs[1]; (kv.first, kv.second))


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


function enqueue!{K,V}(pq::PriorityQueue{K,V}, key, value)
    if haskey(pq, key)
        error("PriorityQueue keys must be unique")
    end

    push!(pq.xs, Pair{K,V}(key, value))
    pq.index[key] = length(pq)
    percolate_up!(pq, length(pq))
    pq
end


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

function next(pq::PriorityQueue, i)
    (k, idx), i = next(pq.index, i)
    return ((k, pq.xs[idx].second), i)
end


end # module Collections

