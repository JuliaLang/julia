module LRUExample
# An LRU (Least Recently Used) cache is an associative data structure which
# maintains its contents in an order such that the most recently used item
# is at the beginning of the structure, and the least recently used at the end.
#
# This file specifies two types of LRU caches, both with and without a size
# limit. BoundedLRU has a limit and evicts the LRU item if a new item is added
# after that bound is reached. UnboundedLRU does not have a maximum size, but
# can be used as a basis for more complex LRUs.
#
# LRUs should follow the interfaces for general collections, indexable
# collections, and associative collections.

# The standard implementation of an LRU backs a hash table with a doubly-linked
# list for O(1) operations when reordering on access and eviction. The Julia
# implementation instead backs the table with a Vector. For moderately-sized
# collections, the difference in performance is small, and this implmentation
# is simpler and easier to understand.

import Base.isempty, Base.length, Base.sizeof
import Base.start, Base.next, Base.done
import Base.haskey, Base.get
import Base.setindex!, Base.getindex, Base.delete!, Base.empty!
import Base.show

abstract LRU{K,V} <: Associative{K,V}

# Default cache size
const __MAXCACHE = 1024

type CacheItem{K,V}
    k::K
    v::V
end

type UnboundedLRU{K,V} <: LRU{K,V}
    ht::Dict
    q::Vector{CacheItem}

    UnboundedLRU() = new(Dict(), similar(Array(CacheItem,1), 0))
end
UnboundedLRU() = UnboundedLRU{Any, Any}()

type BoundedLRU{K,V} <: LRU{K,V}
    ht::Dict
    q::Vector{CacheItem}
    maxsize::Int

    BoundedLRU(m) = new(Dict(), similar(Array(CacheItem,1), 0), m)
    BoundedLRU() = BoundedLRU(__MAXCACHE)
end
BoundedLRU(m) = BoundedLRU{Any, Any}(m)
BoundedLRU() = BoundedLRU{Any, Any}()

## collections ##

isempty(lru::LRU) = isempty(lru.q)
length(lru::LRU) = length(lru.q)
haskey(lru::LRU, key) = haskey(lru.ht, key)

## associative ##

# Should this check count as an access?
haskey(lru::LRU, key) = haskey(lru.ht, key)

get(lru::LRU, key, default) = haskey(lru, key) ? lru[key] : default

function empty!(lru::LRU)
    empty!(lru.ht)
    empty!(lru.q)
end


show(io::IO, lru::UnboundedLRU) = print(io,"UnboundedLRU()")
show(io::IO, lru::BoundedLRU) = print(io,"BoundedLRU($(lru.maxsize))")

## indexable ##

# Method to do the second, slow lookup in the list with early return.
function locate(q, x)
    for i = 1:length(q)
        if q[i] == x
            return i
        end
    end
    error("Item not found.")
end

function getindex(lru::LRU, key)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    splice!(lru.q, idx)
    unshift!(lru.q, item)
    item.v
end

function setindex!(lru::LRU, v, key)
    if haskey(lru, key)
        item = lru.ht[key]
        idx = locate(lru.q, item)
        item.v = v
        splice!(lru.q, idx)
    else
        item = CacheItem(key, v)
        lru.ht[key] = item
    end
    unshift!(lru.q, item)
end

# Eviction
function setindex!{V,K}(lru::BoundedLRU, v::V, key::K)
    invoke(setindex!, (LRU, V, K), lru, v, key)
    nrm = length(lru) - lru.maxsize
    for i in 1:nrm
        rm = pop!(lru.q)
        delete!(lru.ht, rm.k)
    end
end

## associative ##

function delete!(lru::LRU, key)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    delete!(lru.ht, key)
    delete!(lru.q, idx)
end

end # module
