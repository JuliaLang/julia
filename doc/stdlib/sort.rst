:mod:`Base.Sort` --- Routines related to sorting
=================================================================

.. module:: Base.Sort
   :synopsis: Sort and related routines

This module contains algorithms and other functions related to
sorting.  Standard versions of all functions are exported in base.
Specific versions of unexported routines can be used by importing
`Base.Sort`, or for finer grain control, importing the fully qualified
function name, e.g.,::

  # Julia code
  import Base.Sort.timsort_perm!

will allow use of the in-place version of timsort which provides a
permutation, which is not exported by default.  All of the sorting
routines can be made available directly with::

  # Julia code
  using Base.Sort


Overview
--------

There are currently four main sorting algorithms available in Julia::

  insertionsort
  quicksort
  mergesort
  timsort

Insertion sort is an ``O(n^2)`` stable sorting algorithm.  It is
efficient only for very small ``n``.  It is used internally by
``quicksort!`` and ``timsort!``. 

Quicksort is an ``O(n log n)`` sorting algorithm.  For efficiency, it
is not stable.  It is among the fastest sorting algorithms.

Mergesort is an ``O(n log n)`` stable sorting algorithm.

Timsort is an ``O(n log n)`` stable adaptive sorting algorithm.  It
takes advantage of sorted runs which exist in many real world
datasets.  

The `sort`, `sortr`, `sort_by`, and `sortperm` functions select a reasonable
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


-----------------
Sorting Functions
-----------------

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

