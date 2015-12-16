# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.Collections

"""
```rst
..  PriorityQueue(K, V, [ord])

Construct a new :obj:`PriorityQueue`, with keys of type ``K`` and values/priorites of
type ``V``. If an order is not given, the priority queue is min-ordered using
the default comparison for ``V``.
```
"""
Collections.PriorityQueue

"""
    enqueue!(pq, k, v)

Insert the a key `k` into a priority queue `pq` with priority `v`.
"""
Collections.enqueue!

"""
    dequeue!(pq)

Remove and return the lowest priority key from a priority queue.
"""
Collections.dequeue!

"""
    peek(pq)

Return the lowest priority key from a priority queue without removing that key from the queue.
"""
Collections.peek

"""
```rst
..  heapify!(v, [ord])

In-place :func:`heapify`.
```
"""
Collections.heapify!

"""
    heappush!(v, x, [ord])

Given a binary heap-ordered array, push a new element `x`, preserving the heap property. For
efficiency, this function does not check that the array is indeed heap-ordered.
"""
Collections.heappush!

"""
    heappop!(v, [ord])

Given a binary heap-ordered array, remove and return the lowest ordered element. For
efficiency, this function does not check that the array is indeed heap-ordered.
"""
Collections.heappop!

"""
    heapify(v, [ord])

Return a new vector in binary heap order, optionally using the given ordering.
"""
Collections.heapify

"""
    isheap(v, [ord])

Return `true` iff an array is heap-ordered according to the given order.
"""
Collections.isheap
