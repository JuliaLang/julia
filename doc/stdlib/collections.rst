.. module:: Base.Collections

Collections and Data Structures
===============================

The ``Collections`` module contains implementations of some common data
structures.


PriorityQueue
-------------

The ``PriorityQueue`` type is a basic priority queue implementation allowing for
arbitrary key and priority types. Multiple identical keys are not permitted, but
the priority of existing keys can be changed efficiently.

.. function:: PriorityQueue(K, V, [ord])

   Construct a new PriorityQueue, with keys of type ``K`` and values/priorites of
   type ``V``. If an order is not given, the priority queue is min-ordered using
   the default comparison for ``V``.

.. function:: enqueue!(pq, k, v)

   Insert the a key ``k`` into a priority queue ``pq`` with priority ``v``.

.. function:: dequeue!(pq)

   Remove and return the lowest priority key from a priority queue.

.. function:: peek(pq)

   Return the lowest priority key from a priority queue without removing that key from the queue.

``PriorityQueue`` also behaves similarly to a ``Dict`` so that keys can be
inserted and priorities accessed or changed using indexing notation::

  # Julia code
  pq = Collections.PriorityQueue()

  # Insert keys with associated priorities
  pq["a"] = 10
  pq["b"] = 5
  pq["c"] = 15

  # Change the priority of an existing key
  pq["a"] = 0


Heap Functions
--------------

Along with the ``PriorityQueue`` type are lower level functions for performing
binary heap operations on arrays. Each function takes an optional ordering
argument. If not given, default ordering is used, so that elements popped from
the heap are given in ascending order.

.. function:: heapify(v, [ord])

   Return a new vector in binary heap order, optionally using the given
   ordering.

.. function:: heapify!(v, [ord])

   In-place heapify.

.. function:: isheap(v, [ord])

   Return true iff an array is heap-ordered according to the given order.

.. function:: heappush!(v, x, [ord])

   Given a binary heap-ordered array, push a new element ``x``, preserving the heap
   property. For efficiency, this function does not check that the array is
   indeed heap-ordered.

.. function:: heappop!(v, [ord])

   Given a binary heap-ordered array, remove and return the lowest ordered
   element. For efficiency, this function does not check that the array is
   indeed heap-ordered.


