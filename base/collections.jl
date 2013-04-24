
module Collections

import Base: setindex!, done, get, has, isempty, length, next, getindex, start
import ..Sort: Forward, Ordering, It, lt

export
    PriorityQueue,
    dequeue!,
    enqueue!,
    heapify!,
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
function percolate_down!(xs::AbstractArray, i::Integer, o::Ordering)
    while (l = heapleft(i)) <= length(xs)
        r = heapright(i)
        j = r > length(xs) || lt(o, xs[l], xs[r]) ? l : r
        if lt(o, xs[j], xs[i])
            xs[i], xs[j] = xs[j], xs[i]
            i = j
        else
            break
        end
    end
end

percolate_down!(xs::AbstractArray, i::Integer) = percolate_down!(xs, i, Forward())



# Binary min-heap percolate up.
function percolate_up!(xs::AbstractArray, i::Integer, o::Ordering)
    while i > 1
        j = heapparent(i)
        if lt(o, xs[i], xs[j])
            xs[i], xs[j] = xs[j], xs[i]
            i = j
        else
            break
        end
    end
end

percolate_up!(xs::AbstractArray, i::Integer) = percolate_up!(xs, i, Forward())


# Binary min-heap pop.
function heappop!(xs::AbstractArray, o::Ordering)
    x = xs[1]
    y = pop!(xs)
    if !isempty(xs)
        xs[1] = y
        percolate_down!(xs, 1, o)
    end
    x
end

heappop!(xs::AbstractArray) = heappop!(xs, Forward())


# Binary min-heap push.
function heappush!(xs::AbstractArray, x, o::Ordering)
    push!(xs, x)
    percolate_up!(xs, length(xs), o)
    xs
end

heappush!(xs::AbstractArray, x) = heappush!(xs, x, Forward())


# Turn an arbitrary array into a binary min-heap in linear time.
function heapify!(xs::AbstractArray, o::Ordering)
    for i in heapparent(length(xs)):-1:1
        percolate_down!(xs, i, o)
    end
    xs
end

heapify!(xs::AbstractArray) = heapify!(xs, Forward())
heapify(xs::AbstractArray, o::Ordering) = heapify!(copy(xs), o)
heapify(xs::AbstractArray) = heapify(xs, Forward())


# Is an arbitrary array heap ordered?
function isheap(xs::AbstractArray, o::Ordering)
    for i in 1:div(length(xs), 2)
        if lt(o, xs[heapleft(i)], xs[i]) ||
           (heapright(i) <= length(xs) && lt(o, xs[heapright(i)], xs[i]))
            return false
        end
    end
    true
end

isheap(xs::AbstractArray) = isheap(xs, Forward())


# PriorityQueue
# -------------

# A PriorityQueue that acts like a Dict, mapping values to their priorities,
# with the addition of a dequeue! function to remove the lowest priority
# element.
type PriorityQueue{K,V} <: Associative{K,V}
    # Binary heap of (element, priority) pairs.
    xs::Array{(K, V), 1}
    o::Ordering

    # Map elements to their index is xs
    index::Dict

    function PriorityQueue(o::Ordering)
        new(Array((K, V), 0), o, Dict{K, Int}())
    end

    PriorityQueue() = PriorityQueue{K,V}(Forward())

    function PriorityQueue(ks::AbstractArray{K}, vs::AbstractArray{V},
                           o::Ordering)
        if length(ks) != length(vs)
            error("Key and value arrays have unequal lengths.")
        end

        xs = Array((K, V), length(ks))
        index = Dict{K, Int}()
        for (i, (k, v)) in enumerate(zip(ks, vs))
            xs[i] = (k, v)
            if has(index, k)
                error("PriorityQueue keys must be unique.")
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

PriorityQueue(o::Ordering) = PriorityQueue{Any,Any}(o)
PriorityQueue() = PriorityQueue{Any,Any}(Forward())

function PriorityQueue{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V},
                            o::Ordering)
    PriorityQueue{K,V}(ks, vs, o)
end

function PriorityQueue{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V})
    PriorityQueue{K,V}(ks, vs, Forward())
end

function PriorityQueue{K,V}(kvs::Dict{K,V}, o::Ordering)
    PriorityQueue{K,V}([k for k in keys(kvs)], [v for v in values(kvs)], o)
end

function PriorityQueue{K,V}(kvs::Dict{K,V})
    PriorityQueue(kvs, Forward())
end


length(pq::PriorityQueue) = length(pq.xs)
isempty(pq::PriorityQueue) = isempty(pq.xs)
has(pq::PriorityQueue, key) = has(pq.index, key)
peek(pq::PriorityQueue) = pq.xs[1]


# Swap two nodes in a PriorityQueue
function swap!(pq::PriorityQueue, i::Integer, j::Integer)
    pq.index[pq.xs[i][1]] = j
    pq.index[pq.xs[j][1]] = i
    pq.xs[i], pq.xs[j] = pq.xs[j], pq.xs[i]
end


function percolate_down!(pq::PriorityQueue, i::Integer)
    while (l = heapleft(i)) <= length(pq)
        r = heapright(i)
        j = r > length(pq) || lt(pq.o, pq.xs[l][2], pq.xs[r][2]) ? l : r
        if lt(pq.o, pq.xs[j][2], pq.xs[i][2])
            swap!(pq, i, j)
            i = j
        else
            break
        end
    end
end


function percolate_up!(pq::PriorityQueue, i::Integer)
    while i > 1
        j = heapparent(i)
        if lt(pq.o, pq.xs[i][2], pq.xs[j][2])
            swap!(pq, i, j)
            i = j
        else
            break
        end
    end
end


function getindex{K,V}(pq::PriorityQueue{K,V}, key)
    pq.xs[pq.index[key]][2]
end


function get{K,V}(pq::PriorityQueue{K,V}, key, deflt)
    i = get(pq.index, key, 0)
    i == 0 ? deflt : pq.xs[i][2]
end


# Change the priority of an existing element, or equeue it if it isn't present.
function setindex!{K,V}(pq::PriorityQueue{K, V}, value, key)
    if has(pq, key)
        i = pq.index[key]
        _, oldvalue = pq.xs[i]
        pq.xs[i] = (key, value)
        if lt(pq.o, oldvalue, value)
            percolate_down!(pq, i)
        else
            percolate_up!(pq, i)
        end
    else
        enqueue!(pq, key, value)
    end
end


function enqueue!{K,V}(pq::PriorityQueue{K,V}, key, value)
    if has(pq, key)
        error("PriorityQueue keys must be unique.")
    end

    push!(pq.xs, (key, value))
    pq.index[key] = length(pq)
    percolate_up!(pq, length(pq))
    pq
end


function dequeue!(pq::PriorityQueue)
    x = pq.xs[1]
    y = pop!(pq.xs)
    if !isempty(pq)
        pq.xs[1] = y
        pq.index[pq.xs[1][1]] = 1
        percolate_down!(pq, 1)
    end
    delete!(pq.index, x[1])
    x[1]
end


# Unordered iteration through key value pairs in a PriorityQueue
start(pq::PriorityQueue) = start(pq.index)

done(pq::PriorityQueue, i) = done(pq.index, i)

function next(pq::PriorityQueue, i)
    (k, idx), i = next(pq.index, i)
    return ((k, pq.xs[idx][2]), i)
end


end # module DataStructures

