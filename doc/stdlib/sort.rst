:mod:`Base.Sort` --- Routines related to sorting
=================================================================

.. module:: Base.Sort
   :synopsis: Sort and related routines

The `Sort` module contains algorithms and other functions related to
sorting.  Default sort functions and standard versions of the various
sort algorithm are available by default. 
Specific sort algorithms can be used by importing
`Sort`, or for finer grain control, importing the fully qualified
algorithm name, e.g.,::

  # Julia code
  import Sort.TimSort

will allow use of timsort with the various sort functions.  All of the
sorting algorithms can be made available directly with::

  # Julia code
  using Sort


Overview
--------

Many users will simply want to use the default sort algorithms, which
allow sorting in ascending or descending order,::

  # Julia code
  julia> sort([2,3,1]) == [1,2,3]
  true

  julia> sortr([2,3,1]) == [3,2,1]
  true

return a permutation,::

  julia> v = [20,30,10]
  3-element Int64 Array:
   20
   30
   10

  julia> (v2,p) = sortperm(v)
  ([10, 20, 30],[3, 1, 2])

  julia> v[p]
  3-element Int64 Array:
   10
   20
   30

and use a custom extractor function to order inputs::

  julia> canonicalize(s) = filter(c -> ('A'<=c<='Z' || 'a'<=c<='z'), s) | uppercase

  julia> sortby(canonicalize, ["New York", "New Jersey", "Nevada", "Nebraska", "Newark"])
  5-element ASCIIString Array:
   "Nebraska"  
   "Nevada"    
   "Newark"    
   "New Jersey"
   "New York"  

Note that none of the variants above modify the original arrays.  To sort in-place (which is often more efficient), each sort function has a mutating version which ends with an exclamation point (``sort!``, ``sortr!``, and ``sortby!``).

There are also versions of these functions which, in addition to returning a sorted array, will return the permutation of original indices which create the sorted array.  These are ``sortperm``, ``sortpermr``, and ``sortpermby``, along with mutating versions ``sortperm!``, ``sortpermr!``, and ``sortpermby!``.

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
the type of the target array.

Mutating and non-mutating versions of the sort functions using each
of the algorithms above are exported and available for use by
default.


Functions
---------

----------------------
General Sort Functions
----------------------
.. function:: sort(v[, dim])

   Sort a vector in ascending order.  If ``dim`` is provided, sort
   along the given dimension. 

.. function:: sort(lessthan, v[, dim])

   Sort with a custom comparison function.

.. function:: sort(alg, ...)

   Sort using a specific sorting algorithm (InsertionSort, QuickSort,
   MergeSort, or TimSort). 

.. function:: sort!(...)

   In-place sort.

.. function:: sortr(v[, dim])

   Sort a vector in descending order. If ``dim`` is provided, sort
   along the given dimension. 

.. function:: sortr(alg, ...)

   Sort in descending order with a specific sorting algorithm
   (InsertionSort, QuickSort, MergeSort, or TimSort).

.. function:: sortr!(...)

   In-place ``sortr``.

.. function:: sortby(by, v[, dim])

   Sort a vector according to ``by(v)``.   If ``dim`` is provided,
   sort along the given dimension. 

.. function:: sortby(alg, ...)

   ``sortby`` using a specific sorting algorithm (``InsertionSort``,
   ``QuickSort``, ``MergeSort``, or ``TimSort``). 

.. function:: sortby!(...)

   In-place ``sortby``.

.. function:: sortperm(v) -> s,p

   Sort a vector in ascending order, also constructing the permutation
   that sorts the vector.

.. function:: sortperm(lessthan, v) -> s,p

   Sort a vector with a custom comparison function, also constructing
   the permutation that sorts the vector.

.. function:: sortperm(alg, ...) -> s,p

   ``sortperm`` using a specific sorting algorithm (``InsertionSort``,
   ``QuickSort``, ``MergeSort``, or ``TimSort``).

.. function:: sortperm!(...) -> s,p

   In-place ``sortperm``.

.. function:: sortpermr(v) -> s,p

   Sort a vector in descending order, also constructing the
   permutation that sorts the vector!

.. function:: sortpermr(alg, ...) -> s,p

   ``sortpermr`` using a specific sorting algorithm
   (``InsertionSort``, ``QuickSort``, ``MergeSort``, or ``TimSort``).

.. function:: sortpermr!(v) -> s,p

   In-place ``sortpermr``.

.. function:: sortpermby(by,v) -> s,p

   Sort a vector according to the result of function ``by`` applied to
   all values, also constructing the permutation that sorts the vector.

.. function:: sortpermby(alg, ...) -> s,p

   ``sortpermby`` using a specific sorting algorithm
   (``InsertionSort``, ``QuickSort``, ``MergeSort``, or ``TimSort``).

.. function:: sortpermby!(...) -> s,p

   In-place ``sortpermby``.

-------------------------
Sorting-related Functions
-------------------------

.. function:: issorted(v)

   Test whether a vector is in ascending sorted order

.. function:: issortedr(v)

   Test whether a vector is in descending sorted order

.. function:: issortedby(by,v)

   Test whether a vector is sorted according to ``by(v)``.

.. function:: searchsorted(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the first value ``>=x``.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedfirst()``

.. function:: searchsorted(lt, a, x[, lo, hi])

   For ``a`` sorted using ``lt(x,y)``, returns the index of the first value ``>=x`` according to the induced order

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedfirst()``

.. function:: searchsortedr(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the first value ``<=x``.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedfirstr()``

.. function:: searchsortedby(by, a, x[, lo, hi])

   For ``a`` sorted according to ``by(a)``, returns the index of the first value ``>=x`` according to the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedfirstby()``

.. function:: searchsortedfirst(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the first value ``>=x``.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: searchsortedfirst(lt, a, x[, lo, hi])

   For ``a`` sorted using ordering function ``lt(x,y)``, returns the index of the first value ``>=x`` according to the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedfirst()``

.. function:: searchsortedfirstr(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the first value ``<=x``.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: searchsortedfirstby(by, a, x[, lo, hi])

   For ``a`` sorted according to ``by(a)``, returns the index of the first value ``>=x`` according to the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: searchsortedlast(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the last value ``<=x``.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: searchsortedlast(lt, a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the last value ``<=x`` according to the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``searchsortedlast()``

.. function:: searchsortedlastr(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the last value ``>=x``.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: searchsortedlastby(by, a, x[, lo, hi])

   For ``a`` sorted according to ``by(a)``, returns the index of the last value ``<=x`` according to the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: select(v, k)

   Find the element in position ``k`` in the sorted vector ``v`` without sorting

.. function:: select!(v, k)

   Version of ``select`` which permutes the input vector in place.

.. function:: select(lt, v, k)

   Find the element in position ``k`` in the vector ``v`` ordered by ``lt``, without sorting.

.. function:: select!(lt, v, k)

   Version of ``select`` which permutes the input vector in place.

.. function:: selectr(v, k)

   Find the element in position ``k`` in the reverse sorted vector ``v``, without sorting.

.. function:: selectr!(v, k)

   Version of ``selectr`` which permutes the input vector in place.

.. function:: selectby(by, v, k)

   Find the element in position ``k`` in the vector ``v`` as if sorted by sortby, without sorting.

.. function:: selectby!(by, v, k)

   Version of ``selectby`` which permutes the input vector in place.

