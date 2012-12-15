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

The `sort`, `sortr`, `sort_by`, and `sortperm` functions select a reasonable
default algorithm, depending on the type of the target array.

Mutating and non-mutating versions of the sort functions and of each
of the algorithm functions are exported and available for use by
default::

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
+======================+===========================+============================+==========================+
| ``mergesort``        | ``mergesort_r``           | ``mergesort_r!``           | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_by``          | ``mergesort_by!``          | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm``        | ``mergesort_perm!``        | Permutation sort         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm_r``      | ``mergesort_perm_r!``      | Reverse permutation sort |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``mergesort_perm_by``     | ``mergesort_perm_by!``     | Permutation sort by func |
+======================+===========================+============================+==========================+
| ``timsort``          | ``timsort_r``             | ``timsort_r!``             | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_by``            | ``timsort_by!``            | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm``          | ``timsort_perm!``          | Permutation sort         |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm_r``        | ``timsort_perm_r!``        | Reverse permutation sort |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``timsort_perm_by``       | ``timsort_perm_by!``       | Permutation sort by func |
+======================+===========================+============================+==========================+
| ``quicksort``        | ``quicksort_r``           | ``quicksort_r!``           | Reverse sort             |
+----------------------+---------------------------+----------------------------+--------------------------+
|                      | ``quicksort_by``          | ``quicksort_by!``          | Sort by function         |
+----------------------+---------------------------+----------------------------+--------------------------+


-----------------
Sorting Functions
-----------------

.. function:: insertionsort(v)

   Sorts ``v`` with an insertion sort, and returns the sorted array.
   The original array is not changed.

.. function:: insertionsort(fn_lt,v)

   Sorts ``v`` with an insertion sort using ``fn_lt`` as a comparison
   function, and returns the sorted array.  The original array is not
   changed.

   ``fn_lt(a,b)`` should return true if ``a`` comes strictly before
   ``b``, and false otherwise.

   Insertion sort is an ``O(n^2)`` stable sorting algorithm.  It is
   efficient only for very small ``n``.  It is used internally by
   ``quicksort!`` and ``timsort!``. 

.. function:: insertionsort!(v[,lo,hi])

   Sorts ``v`` in place with an insertion sort using ``fn_lt`` as a
   comparison function, and returns the sorted array.

   If ``lo`` and ``hi`` are provided, ``v`` is sorted in the range
   ``lo:hi``. 

   Insertion sort is an ``O(n^2)`` stable sorting algorithm.  It is
   efficient only for very small ``n``.  It is used internally by
   ``quicksort!`` and ``timsort!``. 

.. function:: insertionsort!(fn_lt,v[,lo,hi])

   Sorts ``v`` in place with an insertion sort using ``fn_lt`` as a
   comparison function, and returns the sorted array.

   If ``lo`` and ``hi`` are provided, ``v`` is sorted in the range
   ``lo:hi``. 

   This is an ``O(n^2)`` algorithm, but is efficient for very small
   ``n``.  It is used internally by ``quicksort!`` and ``timsort!``
   algorithms.


.. function:: insertionsort_r(v[,lo,hi])

   Like ``insertionsort``, but in descending order.

.. function:: insertionsort_r!(v[,lo,hi])

   Like ``insertionsort!``, but in descending order.

.. function:: insertionsort_by(by,v[,lo,hi])

   Like ``insertionsort``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: insertionsort_by!(by,v[,lo,hi]) 

   Like ``insertionsort!``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: insertionsort_perm(v)
.. function:: insertionsort_perm(fn_lt,v)

   Like ``insertionsort``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: insertionsort_perm!(v[,lo,hi])
.. function:: insertionsort_perm!(fn_lt,v[,lo,hi])

   Like ``insertionsort!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: insertionsort_perm_r(v[,lo,hi])

   Like ``insertionsort_r``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: insertionsort_perm_r!(v[,lo,hi])

   Like ``insertionsort_r!``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: insertionsort_perm_by(by,v[,lo,hi])

   Like ``insertionsort_by``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: insertionsort_perm_by!(by,v[,lo,hi]) 

   Like ``insertionsort_by!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.


.. function:: mergesort(v)

   Sorts ``v`` with a merge sort, and returns the sorted array.
   The original array is not changed.

.. function:: mergesort(fn_lt,v)

   Sorts ``v`` with a merge sort using ``fn_lt`` as a comparison
   function, and returns the sorted array.  The original array is not
   changed.

   ``fn_lt(a,b)`` should return true if ``a`` comes strictly before
   ``b``, and false otherwise.

   Mergesort is an ``O(n log n)`` stable sorting algorithm.

.. function:: mergesort!(v[,lo,hi])

   Sorts ``v`` in place with a merge sort, and returns the sorted
   array. 

.. function:: mergesort!(fn_lt,v[,lo,hi])

   Sorts ``v`` in place with a merge sort using ``fn_lt`` as a
   comparison function, and returns the sorted array.

   If ``lo`` and ``hi`` are provided, ``v`` is sorted in the range
   ``lo:hi``. 

   Mergesort is an ``O(n log n)`` stable sorting algorithm.

.. function:: mergesort_r(v[,lo,hi])

   Like ``mergesort``, but in descending order.

.. function:: mergesort_r!(v[,lo,hi])

   Like ``mergesort!``, but in descending order.

.. function:: mergesort_by(by,v[,lo,hi])

   Like ``mergesort``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: mergesort_by!(by,v[,lo,hi]) 

   Like ``mergesort!``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: mergesort_perm(v)
.. function:: mergesort_perm(fn_lt,v)

   Like ``mergesort``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: mergesort_perm!(v[,lo,hi])
.. function:: mergesort_perm!(fn_lt,v[,lo,hi])

   Like ``mergesort!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: mergesort_perm_r(v[,lo,hi])

   Like ``mergesort_r``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: mergesort_perm_r!(v[,lo,hi])

   Like ``mergesort_r!``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: mergesort_perm_by(by,v[,lo,hi])

   Like ``mergesort_by``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: mergesort_perm_by!(by,v[,lo,hi]) 

   Like ``mergesort_by!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: timsort(v)

   Sorts ``v`` with timsort, and returns the sorted array.
   The original array is not changed.

.. function:: timsort(fn_lt,v)

   Sorts ``v`` with timsort using ``fn_lt`` as a comparison
   function, and returns the sorted array.  The original array is not
   changed.

   ``fn_lt(a,b)`` should return true if ``a`` comes strictly before
   ``b``, and false otherwise.

   Timsort is an ``O(n log n)`` stable sorting algorithm, and is very
   efficient when data is already partially sorted.

.. function:: timsort!(v[,lo,hi])

   Sorts ``v`` in place with timsort, and returns the sorted array.

.. function:: timsort!(fn_lt,v[,lo,hi])

   Sorts ``v`` in place with timsort using ``fn_lt`` as a
   comparison function, and returns the sorted array.

   If ``lo`` and ``hi`` are provided, ``v`` is sorted in the range
   ``lo:hi``. 

   Timsort is an ``O(n log n)`` stable sorting algorithm, and is very
   efficient when data is already partially sorted.

.. function:: timsort_r(v[,lo,hi])

   Like ``timsort``, but in descending order.

.. function:: timsort_r!(v[,lo,hi])

   Like ``timsort!``, but in descending order.

.. function:: timsort_by(by,v[,lo,hi])

   Like ``timsort``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: timsort_by!(by,v[,lo,hi]) 

   Like ``timsort!``, but sort ``v`` according to the value of
   ``by(x)`` for ``x`` in ``v``. 

.. function:: timsort_perm(v)
.. function:: timsort_perm(fn_lt,v)

   Like ``timsort``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: timsort_perm!(v[,lo,hi])
.. function:: timsort_perm!(fn_lt,v[,lo,hi])

   Like ``timsort!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the original
   indices which produces ``b``.

.. function:: timsort_perm_r(v[,lo,hi])

   Like ``timsort_r``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: timsort_perm_r!(v[,lo,hi])

   Like ``timsort_r!``, but returns ``(b, ix)``, where ``b``
   is the reverse sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: timsort_perm_by(by,v[,lo,hi])

   Like ``timsort_by``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

.. function:: timsort_perm_by!(by,v[,lo,hi]) 

   Like ``timsort_by!``, but returns ``(b, ix)``, where ``b``
   is the sorted array and ``ix`` is the permutation of the
   original indices which produces ``b``.

