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

import Base.isempty, Base.numel, Base.length
import Base.has, Base.get
import Base.assign, Base.ref, Base.del, Base.del_all
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
numel(lru::LRU) = numel(lru.q)
length(lru::LRU) = length(lru.q)
has(lru::LRU, key) = has(lru.ht, key)

## associative ##

# Should this check count as an access?
has(lru::LRU, key) = has(lru.ht, key)

get(lru::LRU, key, default) = has(lru, key) ? lru[key] : default

function del_all(lru::LRU)
    del_all(lru.ht)
    del_all(lru.q)
end


show(io, lru::UnboundedLRU) = print("UnboundedLRU()")
show(io, lru::BoundedLRU) = print("BoundedLRU($(lru.maxsize))")

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

function ref(lru::LRU, key)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    del(lru.q, idx)
    enqueue(lru.q, item)
    item.v
end

function assign(lru::LRU, v, key)
    if has(lru, key)
        item = lru.ht[key]
        idx = locate(lru.q, item)
        item.v = v
        del(lru.q, idx)
    else
        item = CacheItem(key, v)
        lru.ht[key] = item
    end
    enqueue(lru.q, item)
end

# Eviction
function assign{V,K}(lru::BoundedLRU, v::V, key::K)
    invoke(assign, (LRU, V, K), lru, v, key)
    nrm = length(lru) - lru.maxsize
    for i in 1:nrm
        rm = pop(lru.q)
        del(lru.ht, rm.k)
    end
end

## associative ##

function del(lru::LRU, key)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    del(lru.ht, key)
    del(lru.q, idx)
end
