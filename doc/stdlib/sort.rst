:mod:`Base.Sort` --- Routines related to sorting
=================================================================

.. module:: Base.Sort
   :synopsis: Sort and related routines

The `Sort` module contains algorithms and other functions related to
sorting.  Default sort functions and standard versions of the various
sort algorithm are available by default. 
Specific sort algorithms can be used by importing
`Sort` or using the fully qualified algorithm name, e.g.,::

  # Julia code
  sort(v, Sort.TimSort)

will sort ``v`` using ``TimSort``.


Overview
--------

Many users will simply want to use the default sort algorithms, which
allow sorting in ascending or descending order,::

  # Julia code
  julia> sort([2,3,1]) == [1,2,3]
  true

  julia> sort([2,3,1], Sort.Reverse) == [3,2,1]
  true

return a permutation,::

  julia> v = [20,30,10]
  3-element Int64 Array:
   20
   30
   10

  julia> p = sortperm(v)
  [3, 1, 2]

  julia> v[p]
  3-element Int64 Array:
   10
   20
   30

and use a custom extractor function to order inputs::

  julia> canonicalize(s) = filter(c -> ('A'<=c<='Z' || 'a'<=c<='z'), s) | uppercase

  julia> sortby(["New York", "New Jersey", "Nevada", "Nebraska", "Newark"], canonicalize)
  5-element ASCIIString Array:
   "Nebraska"  
   "Nevada"    
   "Newark"    
   "New Jersey"
   "New York"  

Note that none of the variants above modify the original arrays.  To
sort in-place (which is often more efficient), :func:`sort` and 
:func:`sortby` have mutating versions which end with an exclamation 
point (:func:`sort!` and :func:`sortby!`).

These sort functions use reasonable default algorithms, but if you
want more control or want to see if a different sort algorithm will
work better on your data, read on... 


Sort Algorithms
---------------

There are currently four main sorting algorithms available in Julia::

  InsertionSort
  QuickSort
  MergeSort
  TimSort

Insertion sort is an O(n^2) stable sorting algorithm.  It is
efficient for very small ``n``, and is used internally by
``QuickSort`` and ``TimSort``. 

Quicksort is an O(n log n) sorting algorithm.  For efficiency, it
is not stable.  It is among the fastest sorting algorithms.

Mergesort is an O(n log n) stable sorting algorithm.

Timsort is an O(n log n) stable adaptive sorting algorithm.  It
takes advantage of sorted runs which exist in many real world
datasets.  

The sort functions select a reasonable default algorithm, depending on
the type of the target array.  To force a specific algorithm to be
used, append ``Sort.<algorithm>`` to the argument list (e.g., use 
``sort!(v, Sort.TimSort)`` to force the use of the Timsort algorithm).


Functions
---------

--------------
Sort Functions
--------------
.. function:: sort(v[, alg[, ord]])

   Sort a vector in ascending order.  Specify ``alg`` to choose a
   particular sorting algorithm (``Sort.InsertionSort``,
   ``Sort.QuickSort``, ``Sort.MergeSort``, or ``Sort.TimSort``), and
   ``ord`` to sort with a custom ordering (e.g., Sort.Reverse or a
   comparison function).

.. function:: sort!(...)

   In-place sort.

.. function:: sortby(v, by[, alg])

   Sort a vector according to ``by(v)``.  Specify ``alg`` to choose a
   particular sorting algorithm (``Sort.InsertionSort``,
   ``Sort.QuickSort``, ``Sort.MergeSort``, or ``Sort.TimSort``).

.. function:: sortby!(...)

   In-place ``sortby``.

.. function:: sortperm(v, [alg[, ord]])

   Return a permutation vector, which when applied to the input vector
   ``v`` will sort it.  Specify ``alg`` to choose a particular sorting
   algorithm (``Sort.InsertionSort``, ``Sort.QuickSort``,
   ``Sort.MergeSort``, or ``Sort.TimSort``), and ``ord`` to sort with
   a custom ordering (e.g., Sort.Reverse or a comparison function).

-------------------------
Sorting-related Functions
-------------------------

.. function:: issorted(v[, ord])

   Test whether a vector is in ascending sorted order.  If specified,
   ``ord`` gives the ordering to test.

.. function:: searchsorted(a, x[, ord])

   Returns the index of the first value of ``a`` equal to or
   succeeding ``x``, according to ordering ``ord`` (default:
   ``Sort.Forward``).

   Alias for ``searchsortedfirst()``

.. function:: searchsortedfirst(a, x[, ord])

   Returns the index of the first value of ``a`` equal to or
   succeeding ``x``, according to ordering ``ord`` (default:
   ``Sort.Forward``).

.. function:: searchsortedlast(a, x[, ord])

   Returns the index of the last value of ``a`` preceding or equal to
   ``x``, according to ordering ``ord`` (default: ``Sort.Forward``).

.. function:: select(v, k[, ord])

   Find the element in position ``k`` in the sorted vector ``v``
   without sorting, according to ordering ``ord`` (default:
   ``Sort.Forward``).

.. function:: select!(v, k[, ord])

   Version of ``select`` which permutes the input vector in place.
