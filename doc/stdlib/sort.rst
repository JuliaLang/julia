Sorting and Related Functions
=============================

.. module:: Base.Sort

Julia has an extensive, flexible API for sorting and interacting with
already-sorted arrays of values. For many users, sorting in standard
ascending order, letting Julia pick reasonable default algorithms
will be sufficient::

  julia> sort([2,3,1])
  3-element Int64 Array:
   1
   2
   3

You can easily sort in reverse order as well::

  julia> sort([2,3,1], rev=true)
  3-element Int64 Array:
   3
   2
   1

To sort an array in-place, use the "bang" version of the sort function::

  julia> a = [2,3,1];

  julia> sort!(a);

  julia> a
  3-element Int64 Array:
   1
   2
   3

Instead of directly sorting an array, you can compute a permutation of the array's indices that puts the array into sorted order::

  julia> v = randn(5)
  5-element Float64 Array:
    0.587746
   -0.870797
   -0.111843
    1.08793
   -1.25061

  julia> p = sortperm(v)
  5-element Int64 Array:
   5
   2
   3
   1
   4

  julia> v[p]
  5-element Float64 Array:
   -1.25061
   -0.870797
   -0.111843
    0.587746
    1.08793

Arrays can easily be sorted acording to an arbitrary transformation of their values::

  julia> sort(v, by=abs)
  5-element Float64 Array:
   -0.111843
    0.587746
   -0.870797
    1.08793
   -1.25061

Or in reverse order by a transformation::

  julia> sort(v, by=abs, rev=true)
  5-element Float64 Array:
   -1.25061
    1.08793
   -0.870797
    0.587746
   -0.111843

Reasonable sorting algorithms are used by default, but you can choose
other algorithms as well::

  julia> sort(v, alg=TimSort)
  5-element Float64 Array:
   -1.25061
   -0.870797
   -0.111843
    0.587746
    1.08793


Sorting Functions
-----------------

.. function:: sort!(v, [dim,] [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the vector ``v`` in place. ``QuickSort`` is used by default for numeric arrays
   while ``MergeSort`` is used for other arrays. You can specify an algorithm to use via
   the ``alg`` keyword (see `Sorting Algorithms`_ for available algorithms). The ``by``
   keyword lets you provide a function that will be applied to each element before
   comparison; the ``lt`` keyword allows providing a custom "less than" function; use
   ``rev=true`` to reverse the sorting order. These options are independent and can be
   used together in all possible combinations: if both ``by`` and ``lt`` are specified,
   the ``lt`` function is applied to the result of the ``by`` function; ``rev=true``
   reverses whatever ordering specified via the ``by`` and ``lt`` keywords.

.. function:: sort(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of ``sort!`` that returns a sorted copy of ``v`` leaving ``v`` itself unmodified.

.. function:: sort(A, dim, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort a multidimensional array ``A`` along the given dimension.

.. function:: sortperm(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Return a permutation vector of indices of ``v`` that puts it in sorted order.
   Specify ``alg`` to choose a particular sorting algorithm (see `Sorting Algorithms`_).
   ``MergeSort`` is used by default, and since it is stable, the resulting permutation
   will be the lexicographically first one that puts the input array into sorted order –
   i.e. indices of equal elements appear in ascending order. If you choose a non-stable
   sorting algorithm such as ``QuickSort``, a different permutation that puts the array
   into order may be returned. The order is specified using the same keywords as ``sort!``.

.. function:: sortrows(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the rows of matrix ``A`` lexicographically.

.. function:: sortcols(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the columns of matrix ``A`` lexicographically.


Order-Related Functions
-----------------------

.. function:: issorted(v, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Test whether a vector is in sorted order. The ``by``, ``lt`` and ``rev``
   keywords modify what order is considered to be sorted just as they do for ``sort``.

.. function:: searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the range of indices of ``a`` which compare as equal to ``x`` according to the
   order specified by the ``by``, ``lt`` and ``rev`` keywords, assuming that ``a`` is
   already sorted in that order. Returns an empty range located at the insertion point if
   ``a`` does not contain values equal to ``x``.

.. function:: select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Partially sort the vector ``v`` in place, according to the order specified by ``by``,
   ``lt`` and ``rev`` so that the value at index ``k`` (or range of adjacent values if
   ``k`` is a range) occurs at the position where it would appear if the array were
   fully sorted. If ``k`` is a single index, that values is returned; if ``k`` is a
   range, an array of values at those indices is returned. Note that ``select!`` does
   not fully sort the input array, but does leave the returned elements where they
   would be if the array were fully sorted.

.. function:: select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of ``select!`` which copies ``v`` before partially sorting it, thereby
   returning the same thing as ``select!`` but leaving ``v`` unmodified.


Sorting Algorithms
------------------

There are currently four sorting algorithms available in base Julia:

- ``InsertionSort``
- ``QuickSort``
- ``MergeSort``
- ``TimSort``

``InsertionSort`` is an O(n^2) stable sorting algorithm. It is efficient
for very small ``n``, and is used internally by ``QuickSort`` and ``TimSort``.

``QuickSort`` is an O(n log n) sorting algorithm which is in-place,
very fast, but not stable – i.e. elements which are considered
equal will not remain in the same order in which they originally
appeared in the array to be sorted. ``QuickSort`` is the default
algorithm for numeric values, including integers and floats.

``MergeSort`` is an O(n log n) stable sorting algorithm but is not
in-place – it requires a temporary array of equal size to the
input array – and is typically not quite as fast as ``QuickSort``.
It is the default algorithm for non-numeric data.

``TimSort`` is an O(n log n) stable adaptive sorting algorithm which is used as
the default sorting algorithm in Python and Java. It takes advantage of sorted
runs which exist in many real world datasets.

The sort functions select a reasonable default algorithm, depending on
the type of the array to be sorted. To force a specific algorithm to be
used for ``sort`` or other soring functions, supply ``alg=<algorithm>``
as a keyword argument after the array to be sorted.
