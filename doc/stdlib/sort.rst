
.. currentmodule:: Base

Sorting and Related Functions
=============================

Julia has an extensive, flexible API for sorting and interacting with
already-sorted arrays of values. By default, Julia picks reasonable
algorithms and sorts in standard ascending order:

.. doctest::

   julia> sort([2,3,1])
   3-element Array{Int64,1}:
    1
    2
    3

You can easily sort in reverse order as well:

.. doctest::

   julia> sort([2,3,1], rev=true)
   3-element Array{Int64,1}:
    3
    2
    1

To sort an array in-place, use the "bang" version of the sort function:

.. doctest::

   julia> a = [2,3,1];

   julia> sort!(a);

   julia> a
   3-element Array{Int64,1}:
    1
    2
    3

Instead of directly sorting an array, you can compute a permutation of the array's indices that puts the array into sorted order:

.. testsetup::

   srand(1)

.. doctest::

   julia> v = randn(5)
   5-element Array{Float64,1}:
     0.297288
     0.382396
    -0.597634
    -0.0104452
    -0.839027

   julia> p = sortperm(v)
   5-element Array{Int64,1}:
    5
    3
    4
    1
    2

   julia> v[p]
   5-element Array{Float64,1}:
    -0.839027
    -0.597634
    -0.0104452
     0.297288
     0.382396

Arrays can easily be sorted according to an arbitrary transformation of their values:

.. doctest::

   julia> sort(v, by=abs)
   5-element Array{Float64,1}:
    -0.0104452
     0.297288
     0.382396
    -0.597634
    -0.839027

Or in reverse order by a transformation:

.. doctest::

   julia> sort(v, by=abs, rev=true)
   5-element Array{Float64,1}:
    -0.839027
    -0.597634
     0.382396
     0.297288
    -0.0104452

If needed, the sorting algorithm can be chosen:

.. doctest::

   julia> sort(v, alg=InsertionSort)
   5-element Array{Float64,1}:
    -0.839027
    -0.597634
    -0.0104452
     0.297288
     0.382396

All the sorting and order related functions rely on a "less than"
relation defining a total order on the values to be manipulated. The
``isless`` function is invoked by default, but the relation can be
specified via the ``lt`` keyword.

Sorting Functions
-----------------

.. function:: sort!(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

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

   See also :func:`sortperm!`

.. function:: sortperm!(ix, v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

   Like ``sortperm``, but accepts a preallocated index vector ``ix``.  If ``initialized`` is ``false``
   (the default), ix is initialized to contain the values ``1:length(v)``.

   See also :func:`sortperm`

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

.. function:: searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the first value in ``a`` greater than or equal to ``x``,
   according to the specified order. Returns ``length(a)+1`` if ``x`` is greater
   than all values in ``a``.

.. function:: searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the last value in ``a`` less than or equal to ``x``,
   according to the specified order. Returns ``0`` if ``x`` is less than all
   values in ``a``.

.. function:: select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Partially sort the vector ``v`` in place, according to the order specified by ``by``,
   ``lt`` and ``rev`` so that the value at index ``k`` (or range of adjacent values if
   ``k`` is a range) occurs at the position where it would appear if the array were
   fully sorted via a non-stable algorithm. If ``k`` is a single index, that value
   is returned; if ``k`` is a range, an array of values at those indices is returned.
   Note that ``select!`` does not fully sort the input array.

.. function:: select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of ``select!`` which copies ``v`` before partially sorting it, thereby
   returning the same thing as ``select!`` but leaving ``v`` unmodified.


Sorting Algorithms
------------------

There are currently three sorting algorithms available in base Julia:

- ``InsertionSort``
- ``QuickSort``
- ``MergeSort``

``InsertionSort`` is an O(n^2) stable sorting algorithm. It is efficient
for very small ``n``, and is used internally by ``QuickSort``.

``QuickSort`` is an O(n log n) sorting algorithm which is in-place,
very fast, but not stable – i.e. elements which are considered
equal will not remain in the same order in which they originally
appeared in the array to be sorted. ``QuickSort`` is the default
algorithm for numeric values, including integers and floats.

``MergeSort`` is an O(n log n) stable sorting algorithm but is not
in-place – it requires a temporary array of half the size of the
input array – and is typically not quite as fast as ``QuickSort``.
It is the default algorithm for non-numeric data.

The default sorting algorithms are chosen on the basis that they are
fast and stable, or *appear* to be so. For numeric types indeed,
``QuickSort`` is selected as it is faster and indistinguishable in
this case from a stable sort (unless the array records its mutations
in some way). The stability property comes at a non-negligible cost,
so if you don't need it, you may want to explicitly specify your
preferred algorithm, e.g. ``sort!(v, alg=QuickSort)``.

The mechanism by which Julia picks default sorting algorithms is
implemented via the ``Base.Sort.defalg`` function. It allows a
particular algorithm to be registered as the default in all sorting
functions for specific arrays. For example, here are the two default
methods from `sort.jl
<https://github.com/JuliaLang/julia/blob/master/base/sort.jl>`_::

    defalg(v::AbstractArray) = MergeSort
    defalg{T<:Number}(v::AbstractArray{T}) = QuickSort

As for numeric arrays, choosing a non-stable default algorithm for
array types for which the notion of a stable sort is meaningless (i.e.
when two values comparing equal can not be distinguished) may make
sense.
