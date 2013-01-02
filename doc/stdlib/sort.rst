:mod:`Base.Sort` --- Routines related to sorting
=================================================================

.. module:: Base.Sort
   :synopsis: Sort and related routines

The `Sort` module contains algorithms and other functions related to
sorting.  Default sort functions and standard versions of the various
sort algorithm are available by default. 
Specific versions of unexported routines can be used by importing
`Sort`, or for finer grain control, importing the fully qualified
function name, e.g.,::

  # Julia code
  import Sort.timsort_perm!

will allow use of the in-place version of timsort which provides a
permutation, which is not exported by default.  All of the sorting
routines can be made available directly with::

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

  julia> sort_by(canonicalize, ["New York", "New Jersey", "Nevada", "Nebraska", "Newark"])
  5-element ASCIIString Array:
   "Nebraska"  
   "Nevada"    
   "Newark"    
   "New Jersey"
   "New York"  

Note that none of the variants above modify the original arrays.  To sort in-place (which is often more efficient), each sort function has a mutating version which ends with an exclamation point (``sort!``, ``sortr!``, and ``sort_by!``).

There are also versions of these functions which, in addition to returning a sorted array, will return the permutation of original indices which create the sorted array.  These are ``sortperm``, ``sortperm_r``, and ``sortperm_by``, along with mutating versions ``sortperm!``, ``sortperm_r!``, and ``sortperm_by!``.

These sort functions use reasonable default algorithms, but if you
want more control or want to see if a different sort algorithm will
work better on your data, read on... 


Sort Algorithms
---------------

There are currently four main sorting algorithms available in Julia::

  insertionsort
  quicksort
  mergesort
  timsort

Insertion sort is an O(n^2) stable sorting algorithm.  It is
efficient for very small ``n``, and is used internally by
``quicksort!`` and ``timsort!``. 

Quicksort is an O(n log n) sorting algorithm.  For efficiency, it
is not stable.  It is among the fastest sorting algorithms.

Mergesort is an O(n log n) stable sorting algorithm.

Timsort is an O(n log n) stable adaptive sorting algorithm.  It
takes advantage of sorted runs which exist in many real world
datasets.  

The ``sort``, ``sortr``, ``sort_by``, and ``sortperm`` functions select a reasonable
default algorithm, depending on the type of the target array.

Mutating and non-mutating versions of the sort functions and of each
of the algorithm functions are exported and available for use by
default.

+-------------------+--------------------+---------+-------------------+
| Non-mutating      | Mutating           | Stable  |  Time Complexity  |
+===================+====================+=========+===================+
| ``sort``          | ``sort!``          |  (\*)   |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``sortr``         | ``sortr!``         |  (\*)   |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``sort_by``       | ``sort_by!``       |  (\*)   |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``sortperm``      | ``sortperm!``      |  (\*)   |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``insertionsort`` | ``insertionsort!`` |  yes    |      O(n^2)       |
+-------------------+--------------------+---------+-------------------+
| ``quicksort``     | ``quicksort!``     |   no    |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``mergesort``     | ``mergesort!``     |  yes    |    O(n log n)     |
+-------------------+--------------------+---------+-------------------+
| ``timsort``       | ``timsort!``       |  yes    |   <= O(n log n)   |
+-------------------+--------------------+---------+-------------------+

(\*) Stability depends on the algorithm for the target array data type.

In addition to the exported functions shown in the table, each of the
algorithms also has an additional set of unexported functions for
reverse sorting, sorting by a function of the data, and for the stable
sorts, function varieties which return a permutation in addition to
the sorted array.  These are shown in the table below.

+----------------------+---------------------------+----------------------------+--------------------------+
| Sort                 |  Non-mutating Variation   | Mutating Variation         | Function                 |
+======================+===========================+============================+==========================+
| ``insertionsort``    | ``insertionsort_r``       | ``insertionsort_r!``       | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``insertionsort_by``      | ``insertionsort_by!``      | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``insertionsort_perm``    | ``insertionsort_perm!``    | Permutation sort         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``insertionsort_perm_r``  | ``insertionsort_perm_r!``  | Reverse permutation sort |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``insertionsort_perm_by`` | ``insertionsort_perm_by!`` | Permutation sort by func |
+----------------------+---------------------------+----------------------------+--------------------------+
| ``mergesort``        | ``mergesort_r``           | ``mergesort_r!``           | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_by``          | ``mergesort_by!``          | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm``        | ``mergesort_perm!``        | Permutation sort         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm_r``      | ``mergesort_perm_r!``      | Reverse permutation sort |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm_by``     | ``mergesort_perm_by!``     | Permutation sort by func |
+----------------------+---------------------------+----------------------------+--------------------------+
| ``timsort``          | ``timsort_r``             | ``timsort_r!``             | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_by``            | ``timsort_by!``            | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm``          | ``timsort_perm!``          | Permutation sort         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm_r``        | ``timsort_perm_r!``        | Reverse permutation sort |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm_by``       | ``timsort_perm_by!``       | Permutation sort by func |
+----------------------+---------------------------+----------------------------+--------------------------+
| ``quicksort``        | ``quicksort_r``           | ``quicksort_r!``           | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``quicksort_by``          | ``quicksort_by!``          | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+

Functions
---------

----------------------
General Sort Functions
----------------------
.. function:: sort(v)

   Sort a vector in ascending order, according to ``isless``.

.. function:: sort!(v)

   In-place sort.

.. function:: sortr(v)

   Sort a vector in descending order.

.. function:: sortr!(v)

   In-place sort in descending-order.

.. function:: sort_by(by, v)

   Sort a vector by the result of applying function ``by``
   to every element.

.. function:: sort_by!(by, v)

   Sort a vector in place by the result of applying function ``by``
   to every element.

.. function:: sort(a, dim)

   Sort an array along the given dimension.

.. function:: sort(lessthan, a, [dim])

   Sort with a custom comparison function.

.. function:: sortperm(v) -> s,p

   Sort a vector in ascending order, also constructing the permutation that sorts the vector

.. function:: sortperm!(v) -> s,p

   Sort a vector in ascending order in-place, also constructing the permutation that sorts the vector

.. function:: sortperm_r(v) -> s,p

   Sort a vector in descending order, also constructing the permutation that sorts the vector

.. function:: sortperm_r!(v) -> s,p

   Sort a vector in descending order in-place, also constructing the permutation that sorts the vector

.. function:: sortperm_by(by,v) -> s,p

   Sort a vector according to the result of function ``by`` applied to
   all values, also constructing the permutation that sorts the vector.

.. function:: sortperm_by!(by,v) -> s,p

   Sort a vector in-place according to the result of function ``by``
   applied to all values of ``v``, also constructing the permutation
   that sorts the vector


---------------------------
Specific Sort Functions
---------------------------

.. function:: insertionsort(v[,dim])

   Sort a vector in ascending order with insertion sort, according to ``isless``.

.. function:: insertionsort(lessthan,v[,dim])

   Sort a vector in ascending order with insertion sort, using a
   custom comparison function.

.. function:: insertionsort!(v[,dim])
.. function:: insertionsort!(v[,lo,hi])

   In-place insertion sort, accoring to ``isless``.

.. function:: insertionsort!(lessthan,v[,dim])
.. function:: insertionsort!(lessthan,v[,lo,hi])

   In-place insertion sort with a custom comparison function.

.. function:: insertionsort_r(v[,dim])
.. function:: insertionsort_r(v[,lo,hi])

   Sort a vector in descending order using insertion sort.

.. function:: insertionsort_r!(v[,dim])
.. function:: insertionsort_r!(v[,lo,hi])

   In-place insertion sort in descending order.

.. function:: insertionsort_by(by,v[,dim])
.. function:: insertionsort_by(by,v[,lo,hi])

   Sort a vector with insertion sort according to the result of
   function ``by`` applied to all values.

.. function:: insertionsort_by!(by,v[,dim]) 
.. function:: insertionsort_by!(by,v[,lo,hi]) 

   Sort a vector with insertion sort in place according to the result
   of function ``by`` applied to all values.

.. function:: insertionsort_perm(v[,p[,lo,hi]]) -> s,p

   Sort a vector in ascending order, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm(lessthan,v[,p[,lo,hi]]) -> s,p

   Sort a vector, using a custom comparison function, also
   constructing the permutation that sorts the vector .

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm!(v[,p[,lo,hi]])

   Sort a vector in ascending order in-place, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm!(lessthan,v[,p[,lo,hi]])

   Sort a vector in place, using a custom comparison function, also 
   constructing the permutation that sorts the vector .

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm_r(v[,p,[,lo,hi]])

   Sort a vector in descending order, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm_r!(v[,p,[,lo,hi]])

   Sort a vector in descending order in place, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm_by(by,v[,p[,lo,hi]])

   Sort a vector with insertion sort according to the result
   of function ``by`` applied to all values.

   If provided, ``p`` is an initial permutation.

.. function:: insertionsort_perm_by!(by,v[,p[,lo,hi]])

   Sort a vector with insertion sort in place according to the result 
   of function ``by`` applied to all values.

   If provided, ``p`` is an initial permutation.


.. function:: mergesort(v[,dim])

   Sort a vector in ascending order with mergesort, according to ``isless``.

.. function:: mergesort(lessthan,v[,dim])

   Sort a vector in ascending order with mergesort, using a
   custom comparison function.

.. function:: mergesort!(v[,dim])
.. function:: mergesort!(v[,lo,hi])

   In-place mergesort, accoring to ``isless``.

.. function:: mergesort!(lessthan,v[,dim])
.. function:: mergesort!(lessthan,v[,lo,hi])

   In-place mergesort with a custom comparison function.

.. function:: mergesort_r(v[,dim])
.. function:: mergesort_r(v[,lo,hi])

   Sort a vector in descending order using mergesort.

.. function:: mergesort_r!(v[,dim])
.. function:: mergesort_r!(v[,lo,hi])

   In-place mergesort in descending order.

.. function:: mergesort_by(by,v[,dim])
.. function:: mergesort_by(by,v[,lo,hi])

   Sort a vector with mergesort according to the result of
   function ``by`` applied to all values.

.. function:: mergesort_by!(by,v[,dim])
.. function:: mergesort_by!(by,v[,lo,hi]) 

   Sort a vector with mergesort in place according to the result
   of function ``by`` applied to all values.

.. function:: mergesort_perm(v[,p[,lo,hi]]) -> s,p

   Sort a vector in ascending order, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm(lessthan,v[,p[,lo,hi]]) -> s,p

   Sort a vector, using a custom comparison function, also
   constructing the permutation that sorts the vector .

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm!(v[,p[,lo,hi]])

   Sort a vector in ascending order in-place, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm!(lessthan,v[,p[,lo,hi]])

   Sort a vector in place, using a custom comparison function, also 
   constructing the permutation that sorts the vector .

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm_r(v[,p,[,lo,hi]])

   Sort a vector in descending order, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm_r!(v[,p,[,lo,hi]])

   Sort a vector in descending order in place, also constructing the
   permutation that sorts the vector 

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm_by(by,v[,p[,lo,hi]])

   Sort a vector with mergesort according to the result
   of function ``by`` applied to all values.

   If provided, ``p`` is an initial permutation.

.. function:: mergesort_perm_by!(by,v[,p[,lo,hi]])

   Sort a vector with mergesort in place according to the result 
   of function ``by`` applied to all values.

   If provided, ``p`` is an initial permutation.


.. function:: quicksort(v[,dim])

   Sort a vector in ascending order with quicksort, according to ``isless``.

.. function:: quicksort(lessthan,v[,dim])

   Sort a vector in ascending order with quicksort, using a
   custom comparison function.

.. function:: quicksort!(v[,dim])
.. function:: quicksort!(v[,lo,hi])

   In-place quicksort, accoring to ``isless``.

.. function:: quicksort!(lessthan,v[,dim])
.. function:: quicksort!(lessthan,v[,lo,hi])

   In-place quicksort with a custom comparison function.

.. function:: quicksort_r(v[,dim])
.. function:: quicksort_r(v[,lo,hi])

   Sort a vector in descending order using quicksort.

.. function:: quicksort_r!(v[,dim])
.. function:: quicksort_r!(v[,lo,hi])

   In-place quicksort in descending order.

.. function:: quicksort_by(by,v[,dim])
.. function:: quicksort_by(by,v[,lo,hi])

   Sort a vector with quicksort according to the result of
   function ``by`` applied to all values.

.. function:: quicksort_by!(by,v[,dim]) 
.. function:: quicksort_by!(by,v[,lo,hi]) 

   Sort a vector with quicksort in place according to the result
   of function ``by`` applied to all values.

-------------------------
Sorting-related Functions
-------------------------

.. function:: issorted(v)

   Test whether a vector is in ascending sorted order

.. function:: issorted_r(v)

   Test whether a vector is in descending sorted order

.. function:: issorted_by(by,v)

   Test whether a vector is sorted by the result of function ``by``
   applied to all values of ``v``

.. function:: search_sorted(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the first value ``>=x``.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_first()``

.. function:: search_sorted(lt, a, x[, lo, hi])

   For ``a`` sorted using ordering function ``lt(x,y)``, returns the index of the first value equal to ``x`` or following ``x`` in the induced order

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_first()``

.. function:: search_sorted_r(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the first value ``<=x``.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_first_r()``

.. function:: search_sorted_by(by, a, x[, lo, hi])

   For ``a`` sorted according to the natural order of ``by(x)`` for ``x`` in ``a``, returns the index of the first value equal to or following ``x`` in the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_first_by()``

.. function:: search_sorted_first(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the first occurance of ``x``, or if ``x`` is not in ``a``, the index of the first value following ``x`` in natural order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: search_sorted_first(lt, a, x[, lo, hi])

   For ``a`` sorted using ordering function ``lt(x,y)``, returns the index of the first occurance of ``x``, or if ``x`` is not in ``a``, the index of the first value following ``x`` in the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_first()``

.. function:: search_sorted_first_r(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the first occurance of ``x``, or if ``x`` is not in ``a``, the index of the first value following ``x`` in reverse natural order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: search_sorted_first_by(by, a, x[, lo, hi])

   For ``a`` sorted according to the natural order of ``by(x)`` for ``x`` in ``a``, returns the index of the first occurance of ``x``, or if ``x`` is not in ``a``, the index of the first value following ``x`` in the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: search_sorted_last(a, x[, lo, hi])

   For ``a`` sorted low to high, returns the index of the last occurance of ``x``, or if ``x`` is not in ``a``, the index of the last value preceding ``x`` in natural order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: search_sorted_last(lt, a, x[, lo, hi])

   For ``a`` sorted using ordering function ``lt(x,y)``, returns the index of the last occurance of``x``, or if ``x`` is not in ``a``, the index of the last value preceding ``x`` in the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

   Alias for ``search_sorted_last()``

.. function:: search_sorted_last_r(a, x[, lo, hi])

   For ``a`` sorted high to low, returns the index of the last occurance of ``x``, or if ``x`` is not in ``a``, the index of the last value preceding ``x`` in reverse natural order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: search_sorted_last_by(by, a, x[, lo, hi])

   For ``a`` sorted according to the natural order of ``by(x)`` for ``x`` in ``a``, returns the index of the last occurance of ``x``, or if ``x`` is not in ``a``, the index of the last value preceding ``x`` in the induced order.

   ``lo`` and ``hi`` optionally limit the search range.

.. function:: select(v, k)

   Find the element in position ``k`` in the sorted vector ``v`` without sorting

.. function:: select!(v, k)

   Version of ``select`` which permutes the input vector in place.

.. function:: select(lt, v, k)

   Find the element in position ``k`` in the vector ``v`` ordered by ``lt``, without sorting.

.. function:: select!(lt, v, k)

   Version of ``select`` which permutes the input vector in place.

.. function:: select_r(v, k)

   Find the element in position ``k`` in the reverse sorted vector ``v``, without sorting.

.. function:: select_r!(v, k)

   Version of ``select_r`` which permutes the input vector in place.

.. function:: select_by(by, v, k)

   Find the element in position ``k`` in the vector ``v`` as if sorted by sort_by, without sorting.

.. function:: select_by!(by, v, k)

   Version of ``select_by`` which permutes the input vector in place.

