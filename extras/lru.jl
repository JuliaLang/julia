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
# be simpler and easier to understand.

abstract LRU{K,V} <: Associative

# Default cache size
const __MAXCACHE = 1024

type CacheItem{K,V}
    k::K
    v::V
end

type UnboundedLRU{K,V} <: LRU{K,V}
    ht::HashTable{K,CacheItem{K,V}}
    q::Vector{CacheItem{K,V}}

    UnboundedLRU() = new(HashTable{K,CacheItem{K,V}}(), empty(Array(CacheItem{K,V},1)))
end
UnboundedLRU() = UnboundedLRU{Any, Any}()

type BoundedLRU{K,V} <: LRU{K,V}
    ht::HashTable
    q::Vector{CacheItem}
    maxsize::Int

    BoundedLRU(m) = new(HashTable(), empty(Array(CacheItem,1)), m)
    BoundedLRU() = BoundedLRU(__MAXCACHE)
end
BoundedLRU(m) = BoundedLRU{Any, Any}(m)
BoundedLRU() = BoundedLRU{Any, Any}()

## collections ##

isempty(lru::LRU) = isempty(lru.q)
numel(lru::LRU) = numel(lru.q)
length(lru::LRU) = length(lru.q)
has{K}(lru::LRU{K}, key::K) = has(lru.ht, key)

## associative ##

# Should this check count as an access?
has{K}(lru::LRU{K}, key::K) = has(lru.ht, key)

get{K}(lru::LRU{K}, key::K, default) = has(lru, key) ? lru[key] : default

function del_all(lru::LRU)
    del_all(lru.ht)
    del_all(lru.q)
end


show(lru::BoundedLRU) = print("BoundedLRU($(lru.maxsize))")

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

function ref{K}(lru::LRU{K}, key::K)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    del(lru.q, idx)
    enqueue(lru.q, item)
    item.v
end

function assign{K,V}(lru::LRU{K,V}, v::V, key::K)
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
function assign{K,V}(lru::BoundedLRU{K,V}, v::V, key::K)
    invoke(assign, (LRU{K,V}, V, K), lru, v, key)
    nrm = length(lru) - lru.maxsize
    for i in 1:nrm
        rm = pop(lru.q)
        del(lru.ht, rm.k)
    end
end

## associative ##

function del{K}(lru::LRU{K}, key::K)
    item = lru.ht[key]
    idx = locate(lru.q, item)
    del(lru.ht, key)
    del(lru.q, idx)
end
