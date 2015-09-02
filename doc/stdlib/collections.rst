.. currentmodule:: Base

*********************************
 Collections and Data Structures
*********************************

.. _stdlib-collections-iteration:

Iteration
---------

Sequential iteration is implemented by the methods :func:`start`, :func:`done`, and
:func:`next`. The general ``for`` loop::

    for i = I   # or  "for i in I"
        # body
    end

is translated into::

    state = start(I)
    while !done(I, state)
        (i, state) = next(I, state)
        # body
    end

The ``state`` object may be anything, and should be chosen appropriately for
each iterable type. See the :ref:`manual section on the iteration interface
<man-interfaces-iteration>` for more details about defining a custom iterable
type.

.. function:: start(iter) -> state

   .. Docstring generated from Julia source
   .. code-block:: julia

       start(iter) -> state

   Get initial iteration state for an iterable object

.. function:: done(iter, state) -> Bool

   .. Docstring generated from Julia source
   .. code-block:: julia

       done(iter, state) -> Bool

   Test whether we are done iterating

.. function:: next(iter, state) -> item, state

   .. Docstring generated from Julia source
   .. code-block:: julia

       next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current item and the next iteration state

.. function:: zip(iters...)

   .. Docstring generated from Julia source
   ::

              zip(iters...)

   For a set of iterable objects, returns an iterable of tuples, where the ``i``\ th tuple contains the ``i``\ th component of each input iterable.

   Note that :func:`zip` is its own inverse: ``collect(zip(zip(a...)...)) == collect(a)``.

.. function:: enumerate(iter)

   .. Docstring generated from Julia source
   ::

              enumerate(iter)

   An iterator that yields ``(i, x)`` where ``i`` is an index starting at 1, and ``x`` is the ``i``\ th value from the given iterator. It's useful when you need not only the values ``x`` over which you are iterating, but also the index ``i`` of the iterations.

   .. doctest::

   	julia> a = ["a", "b", "c"];

   	julia> for (index, value) in enumerate(a)
                   println("$index $value")
               end
        1 a
        2 b
        3 c

.. function:: rest(iter, state)

   .. Docstring generated from Julia source
   .. code-block:: julia

       rest(iter, state)

   An iterator that yields the same elements as ``iter``\ , but starting at the given ``state``\ .

.. function:: countfrom(start=1, step=1)

   .. Docstring generated from Julia source
   .. code-block:: julia

       countfrom(start=1, step=1)

   An iterator that counts forever, starting at ``start`` and incrementing by ``step``\ .

.. function:: take(iter, n)

   .. Docstring generated from Julia source
   .. code-block:: julia

       take(iter, n)

   An iterator that generates at most the first ``n`` elements of ``iter``\ .

.. function:: drop(iter, n)

   .. Docstring generated from Julia source
   .. code-block:: julia

       drop(iter, n)

   An iterator that generates all but the first ``n`` elements of ``iter``\ .

.. function:: cycle(iter)

   .. Docstring generated from Julia source
   .. code-block:: julia

       cycle(iter)

   An iterator that cycles through ``iter`` forever.

.. function:: repeated(x[, n::Int])

   .. Docstring generated from Julia source
   .. code-block:: julia

       repeated(x[, n::Int])

   An iterator that generates the value ``x`` forever. If ``n`` is specified, generates ``x`` that many times (equivalent to ``take(repeated(x), n)``\ ).

Fully implemented by:

- :obj:`Range`
- :obj:`UnitRange`
- :obj:`NDRange`
- :obj:`Tuple`
- :obj:`Number`
- :obj:`AbstractArray`
- :obj:`IntSet`
- :obj:`ObjectIdDict`
- :obj:`Dict`
- :obj:`WeakKeyDict`
- :obj:`EachLine`
- :obj:`AbstractString`
- :obj:`Set`
- :obj:`Task`

General Collections
-------------------

.. function:: isempty(collection) -> Bool

   .. Docstring generated from Julia source
   ::

              isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

   .. doctest::

   	julia> isempty([])
   	true

   	julia> isempty([1 2 3])
   	false

.. function:: empty!(collection) -> collection

   .. Docstring generated from Julia source
   .. code-block:: julia

       empty!(collection) -> collection

   Remove all elements from a ``collection``\ .

.. function:: length(collection) -> Integer

   .. Docstring generated from Julia source
   .. code-block:: julia

       length(A) -> Integer

   Returns the number of elements in A

   .. code-block:: julia

       length(collection) -> Integer

   For ordered, indexable collections, the maximum index ``i`` for which ``getindex(collection, i)`` is valid. For unordered collections, the number of elements.

   .. code-block:: julia

       length(s)

   The number of characters in string ``s``\ .

.. function:: endof(collection) -> Integer

   .. Docstring generated from Julia source
   ::

              endof(collection) -> Integer

   Returns the last index of the collection.

   .. doctest::

   	julia> endof([1,2,4])
   	3

Fully implemented by:

- :obj:`Range`
- :obj:`UnitRange`
- :obj:`Tuple`
- :obj:`Number`
- :obj:`AbstractArray`
- :obj:`IntSet`
- :obj:`Dict`
- :obj:`WeakKeyDict`
- :obj:`AbstractString`
- :obj:`Set`

Iterable Collections
--------------------

.. function:: in(item, collection) -> Bool

   .. Docstring generated from Julia source
   ::

              in(item, collection) -> Bool
              ∈(item,collection) -> Bool
              ∋(collection,item) -> Bool
              ∉(item,collection) -> Bool
              ∌(collection,item) -> Bool

   Determine whether an item is in the given collection, in the sense that it is
   ``==`` to one of the values generated by iterating over the collection.
   Some collections need a slightly different definition; for example :obj:`Set`\ s
   check whether the item :func:`isequal` to one of the elements. :obj:`Dict`\ s look for
   ``(key,value)`` pairs, and the key is compared using :func:`isequal`. To test
   for the presence of a key in a dictionary, use :func:`haskey` or
   ``k in keys(dict)``.

.. function:: eltype(type)

   .. Docstring generated from Julia source
   .. code-block:: julia

       eltype(type)

   Determine the type of the elements generated by iterating a collection of the given ``type``\ . For associative collection types, this will be a ``(key,value)`` tuple type. The definition ``eltype(x) = eltype(typeof(x))`` is provided for convenience so that instances can be passed instead of types. However the form that accepts a type argument should be defined for new types.

.. function:: indexin(a, b)

   .. Docstring generated from Julia source
   .. code-block:: julia

       indexin(a, b)

   Returns a vector containing the highest index in ``b`` for each value in ``a`` that is a member of ``b`` . The output vector contains 0 wherever ``a`` is not a member of ``b``\ .

.. function:: findin(a, b)

   .. Docstring generated from Julia source
   .. code-block:: julia

       findin(a, b)

   Returns the indices of elements in collection ``a`` that appear in collection ``b``

.. function:: unique(itr[, dim])

   .. Docstring generated from Julia source
   .. code-block:: julia

       unique(itr[, dim])

   Returns an array containing only the unique elements of the iterable ``itr``\ , in the order that the first of each set of equivalent elements originally appears. If ``dim`` is specified, returns unique regions of the array ``itr`` along ``dim``\ .

.. function:: reduce(op, v0, itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       reduce(op, v0, itr)

   Reduce the given collection ``ìtr`` with the given binary operator ``op``\ . ``v0`` must be a neutral element for ``op`` that will be returned for empty collections. It is unspecified whether ``v0`` is used for non-empty collections.

   Reductions for certain commonly-used operators have special implementations which should be used instead: ``maximum(itr)``\ , ``minimum(itr)``\ , ``sum(itr)``\ , ``prod(itr)``\ , ``any(itr)``\ , ``all(itr)``\ .

   The associativity of the reduction is implementation dependent. This means that you can't use non-associative operations like ``-`` because it is undefined whether ``reduce(-,[1,2,3])`` should be evaluated as ``(1-2)-3`` or ``1-(2-3)``\ . Use ``foldl`` or ``foldr`` instead for guaranteed left or right associativity.

   Some operations accumulate error, and parallelism will also be easier if the reduction can be executed in groups. Future versions of Julia might change the algorithm. Note that the elements are not reordered if you use an ordered collection.

   .. code-block:: julia

       reduce(op, itr)

   Like ``reduce(op, v0, itr)``\ . This cannot be used with empty collections, except for some special cases (e.g. when ``op`` is one of ``+``\ , ``*``\ , ``max``\ , ``min``\ , ``&``\ , ``|``\ ) when Julia can determine the neutral element of ``op``\ .

.. function:: reduce(op, itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       reduce(op, v0, itr)

   Reduce the given collection ``ìtr`` with the given binary operator ``op``\ . ``v0`` must be a neutral element for ``op`` that will be returned for empty collections. It is unspecified whether ``v0`` is used for non-empty collections.

   Reductions for certain commonly-used operators have special implementations which should be used instead: ``maximum(itr)``\ , ``minimum(itr)``\ , ``sum(itr)``\ , ``prod(itr)``\ , ``any(itr)``\ , ``all(itr)``\ .

   The associativity of the reduction is implementation dependent. This means that you can't use non-associative operations like ``-`` because it is undefined whether ``reduce(-,[1,2,3])`` should be evaluated as ``(1-2)-3`` or ``1-(2-3)``\ . Use ``foldl`` or ``foldr`` instead for guaranteed left or right associativity.

   Some operations accumulate error, and parallelism will also be easier if the reduction can be executed in groups. Future versions of Julia might change the algorithm. Note that the elements are not reordered if you use an ordered collection.

   .. code-block:: julia

       reduce(op, itr)

   Like ``reduce(op, v0, itr)``\ . This cannot be used with empty collections, except for some special cases (e.g. when ``op`` is one of ``+``\ , ``*``\ , ``max``\ , ``min``\ , ``&``\ , ``|``\ ) when Julia can determine the neutral element of ``op``\ .

.. function:: foldl(op, v0, itr)

   .. Docstring generated from Julia source
   ::

              foldl(op, v0, itr)

   Like :func:`reduce`, but with guaranteed left associativity. ``v0``
   will be used exactly once.

   ::

              foldl(op, itr)

   Like ``foldl(op, v0, itr)``, but using the first element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: foldl(op, itr)

   .. Docstring generated from Julia source
   ::

              foldl(op, v0, itr)

   Like :func:`reduce`, but with guaranteed left associativity. ``v0``
   will be used exactly once.

   ::

              foldl(op, itr)

   Like ``foldl(op, v0, itr)``, but using the first element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: foldr(op, v0, itr)

   .. Docstring generated from Julia source
   ::

              foldr(op, v0, itr)

   Like :func:`reduce`, but with guaranteed right associativity. ``v0``
   will be used exactly once.

   ::

              foldr(op, itr)

   Like ``foldr(op, v0, itr)``, but using the last element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: foldr(op, itr)

   .. Docstring generated from Julia source
   ::

              foldr(op, v0, itr)

   Like :func:`reduce`, but with guaranteed right associativity. ``v0``
   will be used exactly once.

   ::

              foldr(op, itr)

   Like ``foldr(op, v0, itr)``, but using the last element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: maximum(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maximum(itr)

   Returns the largest element in a collection.

   .. code-block:: julia

       maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

.. function:: maximum(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maximum(itr)

   Returns the largest element in a collection.

   .. code-block:: julia

       maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

.. function:: maximum!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maximum!(r, A)

   Compute the maximum value of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: minimum(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minimum(itr)

   Returns the smallest element in a collection.

   .. code-block:: julia

       minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

.. function:: minimum(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minimum(itr)

   Returns the smallest element in a collection.

   .. code-block:: julia

       minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

.. function:: minimum!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minimum!(r, A)

   Compute the minimum value of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: extrema(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       extrema(itr)

   Compute both the minimum and maximum element in a single pass, and return them as a 2-tuple.

.. function:: indmax(itr) -> Integer

   .. Docstring generated from Julia source
   .. code-block:: julia

       indmax(itr) -> Integer

   Returns the index of the maximum element in a collection.

.. function:: indmin(itr) -> Integer

   .. Docstring generated from Julia source
   .. code-block:: julia

       indmin(itr) -> Integer

   Returns the index of the minimum element in a collection.

.. function:: findmax(itr) -> (x, index)

   .. Docstring generated from Julia source
   .. code-block:: julia

       findmax(itr) -> (x, index)

   Returns the maximum element and its index.

   .. code-block:: julia

       findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over the given dimensions.

.. function:: findmax(A, dims) -> (maxval, index)

   .. Docstring generated from Julia source
   .. code-block:: julia

       findmax(itr) -> (x, index)

   Returns the maximum element and its index.

   .. code-block:: julia

       findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over the given dimensions.

.. function:: findmin(itr) -> (x, index)

   .. Docstring generated from Julia source
   .. code-block:: julia

       findmin(itr) -> (x, index)

   Returns the minimum element and its index.

   .. code-block:: julia

       findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over the given dimensions.

.. function:: findmin(A, dims) -> (minval, index)

   .. Docstring generated from Julia source
   .. code-block:: julia

       findmin(itr) -> (x, index)

   Returns the minimum element and its index.

   .. code-block:: julia

       findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over the given dimensions.

.. function:: maxabs(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maxabs(itr)

   Compute the maximum absolute value of a collection of values.

   .. code-block:: julia

       maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

.. function:: maxabs(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maxabs(itr)

   Compute the maximum absolute value of a collection of values.

   .. code-block:: julia

       maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

.. function:: maxabs!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       maxabs!(r, A)

   Compute the maximum absolute values over the singleton dimensions of ``r``\ , and write values to ``r``\ .

.. function:: minabs(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minabs(itr)

   Compute the minimum absolute value of a collection of values.

   .. code-block:: julia

       minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

.. function:: minabs(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minabs(itr)

   Compute the minimum absolute value of a collection of values.

   .. code-block:: julia

       minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

.. function:: minabs!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       minabs!(r, A)

   Compute the minimum absolute values over the singleton dimensions of ``r``\ , and write values to ``r``\ .

.. function:: sum(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sum(itr)

   Returns the sum of all elements in a collection.

   .. code-block:: julia

       sum(A, dims)

   Sum elements of an array over the given dimensions.

   .. code-block:: julia

       sum(f, itr)

   Sum the results of calling function ``f`` on each element of ``itr``\ .

.. function:: sum(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sum(itr)

   Returns the sum of all elements in a collection.

   .. code-block:: julia

       sum(A, dims)

   Sum elements of an array over the given dimensions.

   .. code-block:: julia

       sum(f, itr)

   Sum the results of calling function ``f`` on each element of ``itr``\ .

.. function:: sum!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sum!(r, A)

   Sum elements of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: sum(f, itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sum(itr)

   Returns the sum of all elements in a collection.

   .. code-block:: julia

       sum(A, dims)

   Sum elements of an array over the given dimensions.

   .. code-block:: julia

       sum(f, itr)

   Sum the results of calling function ``f`` on each element of ``itr``\ .

.. function:: sumabs(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs(itr)

   Sum absolute values of all elements in a collection. This is equivalent to ``sum(abs(itr))`` but faster.

   .. code-block:: julia

       sumabs(A, dims)

   Sum absolute values of elements of an array over the given dimensions.

.. function:: sumabs(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs(itr)

   Sum absolute values of all elements in a collection. This is equivalent to ``sum(abs(itr))`` but faster.

   .. code-block:: julia

       sumabs(A, dims)

   Sum absolute values of elements of an array over the given dimensions.

.. function:: sumabs!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs!(r, A)

   Sum absolute values of elements of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: sumabs2(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs2(itr)

   Sum squared absolute values of all elements in a collection. This is equivalent to ``sum(abs2(itr))`` but faster.

   .. code-block:: julia

       sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given dimensions.

.. function:: sumabs2(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs2(itr)

   Sum squared absolute values of all elements in a collection. This is equivalent to ``sum(abs2(itr))`` but faster.

   .. code-block:: julia

       sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given dimensions.

.. function:: sumabs2!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sumabs2!(r, A)

   Sum squared absolute values of elements of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: prod(itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       prod(itr)

   Returns the product of all elements of a collection.

   .. code-block:: julia

       prod(A, dims)

   Multiply elements of an array over the given dimensions.

.. function:: prod(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       prod(itr)

   Returns the product of all elements of a collection.

   .. code-block:: julia

       prod(A, dims)

   Multiply elements of an array over the given dimensions.

.. function:: prod!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       prod!(r, A)

   Multiply elements of ``A`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: any(itr) -> Bool

   .. Docstring generated from Julia source
   .. code-block:: julia

       any(itr) -> Bool

   Test whether any elements of a boolean collection are true.

   .. code-block:: julia

       any(A, dims)

   Test whether any values along the given dimensions of an array are true.

   .. code-block:: julia

       any(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for any elements of ``itr``\ .

.. function:: any(A, dims)

   .. Docstring generated from Julia source
   .. code-block:: julia

       any(itr) -> Bool

   Test whether any elements of a boolean collection are true.

   .. code-block:: julia

       any(A, dims)

   Test whether any values along the given dimensions of an array are true.

   .. code-block:: julia

       any(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for any elements of ``itr``\ .

.. function:: any!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       any!(r, A)

   Test whether any values in ``A`` along the singleton dimensions of ``r`` are true, and write results to ``r``\ .

.. function:: all(itr) -> Bool

   .. Docstring generated from Julia source
   ::

              all(itr) -> Bool

   Test whether all elements of a boolean collection are true.

   ::

              all(A, dims)

   Test whether all values along the given dimensions of an array are true.

   ::

              all(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for all elements of ``itr``.

   .. doctest::

   	julia> all(i->(4<=i<=6), [4,5,6])
   	true

.. function:: all(A, dims)

   .. Docstring generated from Julia source
   ::

              all(itr) -> Bool

   Test whether all elements of a boolean collection are true.

   ::

              all(A, dims)

   Test whether all values along the given dimensions of an array are true.

   ::

              all(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for all elements of ``itr``.

   .. doctest::

   	julia> all(i->(4<=i<=6), [4,5,6])
   	true

.. function:: all!(r, A)

   .. Docstring generated from Julia source
   .. code-block:: julia

       all!(r, A)

   Test whether all values in ``A`` along the singleton dimensions of ``r`` are true, and write results to ``r``\ .

.. function:: count(p, itr) -> Integer

   .. Docstring generated from Julia source
   .. code-block:: julia

       count(p, itr) -> Integer

   Count the number of elements in ``itr`` for which predicate ``p`` returns true.

.. function:: any(p, itr) -> Bool

   .. Docstring generated from Julia source
   .. code-block:: julia

       any(itr) -> Bool

   Test whether any elements of a boolean collection are true.

   .. code-block:: julia

       any(A, dims)

   Test whether any values along the given dimensions of an array are true.

   .. code-block:: julia

       any(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for any elements of ``itr``\ .

.. function:: all(p, itr) -> Bool

   .. Docstring generated from Julia source
   ::

              all(itr) -> Bool

   Test whether all elements of a boolean collection are true.

   ::

              all(A, dims)

   Test whether all values along the given dimensions of an array are true.

   ::

              all(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for all elements of ``itr``.

   .. doctest::

   	julia> all(i->(4<=i<=6), [4,5,6])
   	true

.. function:: map(f, c...) -> collection

   .. Docstring generated from Julia source
   ::

              map(f, c...) -> collection

   Transform collection ``c`` by applying ``f`` to each element.
   For multiple collection arguments, apply ``f`` elementwise.

   .. doctest::

      julia> map((x) -> x * 2, [1, 2, 3])
      3-element Array{Int64,1}:
       2
       4
       6

      julia> map(+, [1, 2, 3], [10, 20, 30])
      3-element Array{Int64,1}:
       11
       22
       33

.. function:: map!(function, collection)

   .. Docstring generated from Julia source
   ::

              map!(function, collection)

   In-place version of :func:`map`.

   ::

              map!(function, destination, collection...)

   Like :func:`map`, but stores the result in ``destination`` rather than a
   new collection. ``destination`` must be at least as large as the first
   collection.

.. function:: map!(function, destination, collection...)

   .. Docstring generated from Julia source
   ::

              map!(function, collection)

   In-place version of :func:`map`.

   ::

              map!(function, destination, collection...)

   Like :func:`map`, but stores the result in ``destination`` rather than a
   new collection. ``destination`` must be at least as large as the first
   collection.

.. function:: mapreduce(f, op, v0, itr)

   .. Docstring generated from Julia source
   ::

              mapreduce(f, op, v0, itr)

   Apply function ``f`` to each element in ``itr``, and then reduce
   the result using the binary function ``op``. ``v0`` must be a
   neutral element for ``op`` that will be returned for empty
   collections. It is unspecified whether ``v0`` is used for non-empty
   collections.

   :func:`mapreduce` is functionally equivalent to calling ``reduce(op,
   v0, map(f, itr))``, but will in general execute faster since no
   intermediate collection needs to be created. See documentation for
   :func:`reduce` and :func:`map`.

   .. doctest::

      julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
      14

   The associativity of the reduction is implementation-dependent.
   Additionally, some implementations may reuse the return value of
   ``f`` for elements that appear multiple times in ``itr``.
   Use :func:`mapfoldl` or :func:`mapfoldr` instead for guaranteed
   left or right associativity and invocation of ``f`` for every value.

   ::

              mapreduce(f, op, itr)

   Like ``mapreduce(f, op, v0, itr)``. In general, this cannot be used
   with empty collections (see ``reduce(op, itr)``).

.. function:: mapreduce(f, op, itr)

   .. Docstring generated from Julia source
   ::

              mapreduce(f, op, v0, itr)

   Apply function ``f`` to each element in ``itr``, and then reduce
   the result using the binary function ``op``. ``v0`` must be a
   neutral element for ``op`` that will be returned for empty
   collections. It is unspecified whether ``v0`` is used for non-empty
   collections.

   :func:`mapreduce` is functionally equivalent to calling ``reduce(op,
   v0, map(f, itr))``, but will in general execute faster since no
   intermediate collection needs to be created. See documentation for
   :func:`reduce` and :func:`map`.

   .. doctest::

      julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
      14

   The associativity of the reduction is implementation-dependent.
   Additionally, some implementations may reuse the return value of
   ``f`` for elements that appear multiple times in ``itr``.
   Use :func:`mapfoldl` or :func:`mapfoldr` instead for guaranteed
   left or right associativity and invocation of ``f`` for every value.

   ::

              mapreduce(f, op, itr)

   Like ``mapreduce(f, op, v0, itr)``. In general, this cannot be used
   with empty collections (see ``reduce(op, itr)``).

.. function:: mapfoldl(f, op, v0, itr)

   .. Docstring generated from Julia source
   ::

              mapfoldl(f, op, v0, itr)

   Like :func:`mapreduce`, but with guaranteed left associativity. ``v0``
   will be used exactly once.

   ::

              mapfoldl(f, op, itr)

   Like ``mapfoldl(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: mapfoldl(f, op, itr)

   .. Docstring generated from Julia source
   ::

              mapfoldl(f, op, v0, itr)

   Like :func:`mapreduce`, but with guaranteed left associativity. ``v0``
   will be used exactly once.

   ::

              mapfoldl(f, op, itr)

   Like ``mapfoldl(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: mapfoldr(f, op, v0, itr)

   .. Docstring generated from Julia source
   ::

              mapfoldr(f, op, v0, itr)

   Like :func:`mapreduce`, but with guaranteed right associativity. ``v0``
   will be used exactly once.

   ::

              mapfoldr(f, op, itr)

   Like ``mapfoldr(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: mapfoldr(f, op, itr)

   .. Docstring generated from Julia source
   ::

              mapfoldr(f, op, v0, itr)

   Like :func:`mapreduce`, but with guaranteed right associativity. ``v0``
   will be used exactly once.

   ::

              mapfoldr(f, op, itr)

   Like ``mapfoldr(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: first(coll)

   .. Docstring generated from Julia source
   ::

              first(coll)

   Get the first element of an iterable collection. Returns the start point of a :obj:`Range`
   even if it is empty.

.. function:: last(coll)

   .. Docstring generated from Julia source
   ::

              last(coll)

   Get the last element of an ordered collection, if it can be computed in O(1) time.
   This is accomplished by calling :func:`endof` to get the last index.
   Returns the end point of a :obj:`Range` even if it is empty.

.. function:: step(r)

   .. Docstring generated from Julia source
   ::

              step(r)

   Get the step size of a :obj:`Range` object.

.. function:: collect(collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       collect(collection)

   Return an array of all items in a collection. For associative collections, returns (key, value) tuples.

   .. code-block:: julia

       collect(element_type, collection)

   Return an array of type ``Array{element_type,1}`` of all items in a collection.

.. function:: collect(element_type, collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       collect(collection)

   Return an array of all items in a collection. For associative collections, returns (key, value) tuples.

   .. code-block:: julia

       collect(element_type, collection)

   Return an array of type ``Array{element_type,1}`` of all items in a collection.

.. function:: issubset(a, b)

   .. Docstring generated from Julia source
   ::

              issubset(a, b)
              ⊆(A,S) -> Bool
              ⊈(A,S) -> Bool
              ⊊(A,S) -> Bool

   Determine whether every element of ``a`` is also in ``b``, using :func:`in`.

   ::

              issubset(A, S) -> Bool
              ⊆(A,S) -> Bool

   True if A is a subset of or equal to S.

.. function:: filter(function, collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       filter(function, collection)

   Return a copy of ``collection``\ , removing elements for which ``function`` is false. For associative collections, the function is passed two arguments (key and value).

.. function:: filter!(function, collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       filter!(function, collection)

   Update ``collection``\ , removing elements for which ``function`` is false. For associative collections, the function is passed two arguments (key and value).

Indexable Collections
---------------------

.. function:: getindex(collection, key...)

   .. Docstring generated from Julia source
   ::

              getindex(type[, elements...])

   Construct a 1-d array of the specified type. This is usually called with the syntax ``Type[]``. Element values can be specified using ``Type[a,b,c,...]``.

   ::

              getindex(A, inds...)

   Returns a subset of array ``A`` as specified by ``inds``, where each ``ind`` may be an ``Int``, a ``Range``, or a ``Vector``. See the manual section on :ref:`array indexing <man-array-indexing>` for details.

   ::

              getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a collection.
   The syntax ``a[i,j,...]`` is converted by the compiler to
   ``getindex(a, i, j, ...)``.

.. function:: setindex!(collection, value, key...)

   .. Docstring generated from Julia source
   .. code-block:: julia

       setindex!(A, X, inds...)

   Store values from array ``X`` within some subset of ``A`` as specified by ``inds``\ .

   .. code-block:: julia

       setindex!(collection, value, key...)

   Store the given value at the given key or index within a collection. The syntax ``a[i,j,...] = x`` is converted by the compiler to ``(setindex!(a, x, i, j, ...); x)``\ .

Fully implemented by:

- :obj:`Array`
- :obj:`BitArray`
- :obj:`AbstractArray`
- :obj:`SubArray`
- :obj:`ObjectIdDict`
- :obj:`Dict`
- :obj:`WeakKeyDict`
- :obj:`AbstractString`

Partially implemented by:

- :obj:`Range`
- :obj:`UnitRange`
- :obj:`Tuple`

Associative Collections
-----------------------

:obj:`Dict` is the standard associative collection. Its implementation uses :func:`hash` as the hashing function for the key, and :func:`isequal` to determine equality. Define these two functions for custom types to override how they are stored in a hash table.

:obj:`ObjectIdDict` is a special hash table where the keys are always object identities.

:obj:`WeakKeyDict` is a hash table implementation where the keys are weak references to objects, and thus may be garbage collected even when referenced in a hash table.

:obj:`Dict`\ s can be created by passing pair objects constructed with :func:`=>` to a :obj:`Dict` constructor: ``Dict("A"=>1, "B"=>2)``. This call will attempt to infer type information from the keys and values (i.e. this example creates a ``Dict{ASCIIString, Int64}``).
To explicitly specify types use the syntax ``Dict{KeyType,ValueType}(...)``.
For example, ``Dict{ASCIIString,Int32}("A"=>1, "B"=>2)``.

As with :obj:`Array`\ s, :obj:`Dict`\ s may be created with comprehensions. For example,
``[i => f(i) for i = 1:10]``.

Given a dictionary ``D``, the syntax ``D[x]`` returns the value of key ``x`` (if it exists) or throws an error, and ``D[x] = y`` stores the key-value pair ``x => y`` in ``D`` (replacing any existing value for the key ``x``).  Multiple arguments to ``D[...]`` are converted to tuples; for example, the syntax ``D[x,y]``  is equivalent to ``D[(x,y)]``, i.e. it refers to the value keyed by the tuple ``(x,y)``.

.. function:: Dict([itr])

   .. Docstring generated from Julia source
   ::

              Dict([itr])

   ``Dict{K,V}()`` constructs a hash table with keys of type ``K`` and values of type ``V``.

   Given a single iterable argument, constructs a :obj:`Dict` whose key-value pairs
   are taken from 2-tuples ``(key,value)`` generated by the argument.

   .. doctest::

     julia> Dict([("A", 1), ("B", 2)])
     Dict{ASCIIString,Int64} with 2 entries:
       "B" => 2
       "A" => 1

   Alternatively, a sequence of pair arguments may be passed.

   .. doctest::

     julia> Dict("A"=>1, "B"=>2)
     Dict{ASCIIString,Int64} with 2 entries:
       "B" => 2
       "A" => 1

.. function:: haskey(collection, key) -> Bool

   .. Docstring generated from Julia source
   .. code-block:: julia

       haskey(collection, key) -> Bool

   Determine whether a collection has a mapping for a given key.

.. function:: get(collection, key, default)

   .. Docstring generated from Julia source
   ::

              get(x)

   Attempt to access the value of the ``Nullable`` object, ``x``. Returns the
   value if it is present; otherwise, throws a ``NullException``.

   ::

              get(x, y)

   Attempt to access the value of the ``Nullable{T}`` object, ``x``. Returns
   the value if it is present; otherwise, returns ``convert(T, y)``.

   ::

              get(collection, key, default)

   Return the value stored for the given key, or the given default value if no mapping for the key is present.

   ::

              get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, return ``f()``.  Use :func:`get!` to also store the default value in the dictionary.

   This is intended to be called using ``do`` block syntax::

     get(dict, key) do
         # default value calculated here

.. function:: get(f::Function, collection, key)

   .. Docstring generated from Julia source
   ::

              get(x)

   Attempt to access the value of the ``Nullable`` object, ``x``. Returns the
   value if it is present; otherwise, throws a ``NullException``.

   ::

              get(x, y)

   Attempt to access the value of the ``Nullable{T}`` object, ``x``. Returns
   the value if it is present; otherwise, returns ``convert(T, y)``.

   ::

              get(collection, key, default)

   Return the value stored for the given key, or the given default value if no mapping for the key is present.

   ::

              get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, return ``f()``.  Use :func:`get!` to also store the default value in the dictionary.

   This is intended to be called using ``do`` block syntax::

     get(dict, key) do
         # default value calculated here

	      time()
     end

.. function:: get!(collection, key, default)

   .. Docstring generated from Julia source
   .. code-block:: julia

       get!(collection, key, default)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => default``\ , and return ``default``\ .

   .. code-block:: julia

       get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => f()``\ , and return ``f()``\ .

   This is intended to be called using ``do`` block syntax:

   .. code-block:: julia

       get!(dict, key) do
           # default value calculated here

.. function:: get!(f::Function, collection, key)

   .. Docstring generated from Julia source
   .. code-block:: julia

       get!(collection, key, default)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => default``\ , and return ``default``\ .

   .. code-block:: julia

       get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => f()``\ , and return ``f()``\ .

   This is intended to be called using ``do`` block syntax:

   .. code-block:: julia

       get!(dict, key) do
           # default value calculated here

	      time()
     end

.. function:: getkey(collection, key, default)

   .. Docstring generated from Julia source
   .. code-block:: julia

       getkey(collection, key, default)

   Return the key matching argument ``key`` if one exists in ``collection``\ , otherwise return ``default``\ .

.. function:: delete!(collection, key)

   .. Docstring generated from Julia source
   .. code-block:: julia

       delete!(collection, key)

   Delete the mapping for the given key in a collection, and return the collection.

.. function:: pop!(collection, key[, default])

   .. Docstring generated from Julia source
   ::

              pop!(collection, key[, default])

   Delete and return the mapping for ``key`` if it exists in ``collection``, otherwise return ``default``, or throw an error if default is not specified.

   ::

              pop!(collection) -> item

   Remove the last item in ``collection`` and return it.

   .. doctest::

     julia> A=[1, 2, 3, 4, 5, 6]
     6-element Array{Int64,1}:
      1
      2
      3
      4
      5
      6

     julia> pop!(A)
     6

     julia> A
     5-element Array{Int64,1}:
      1
      2
      3
      4
      5

.. function:: keys(collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       keys(collection)

   Return an iterator over all keys in a collection. ``collect(keys(d))`` returns an array of keys.

.. function:: values(collection)

   .. Docstring generated from Julia source
   .. code-block:: julia

       values(collection)

   Return an iterator over all values in a collection. ``collect(values(d))`` returns an array of values.

.. function:: merge(collection, others...)

   .. Docstring generated from Julia source
   ::

              merge(collection, others...)

   Construct a merged collection from the given collections. If necessary, the types of the resulting collection will be promoted to accommodate the types of the merged collections. If the same key is present in another collection, the value for that key will be the value it has in the last collection listed.

   .. doctest::

     julia> a = Dict("foo" => 0.0, "bar" => 42.0)
     Dict{ASCIIString,Float64} with 2 entries:
       "bar" => 42.0
       "foo" => 0.0

     julia> b = Dict(utf8("baz") => 17, utf8("bar") => 4711)
     Dict{UTF8String,Int64} with 2 entries:
       "bar" => 4711
       "baz" => 17

     julia> merge(a, b)
     Dict{UTF8String,Float64} with 3 entries:
       "bar" => 4711.0
       "baz" => 17.0
       "foo" => 0.0

     julia> merge(b, a)
     Dict{UTF8String,Float64} with 3 entries:
       "bar" => 42.0
       "baz" => 17.0
       "foo" => 0.0

.. function:: merge!(collection, others...)

   .. Docstring generated from Julia source
   .. code-block:: julia

       merge!(collection, others...)

   Update collection with pairs from the other collections

.. function:: sizehint!(s, n)

   .. Docstring generated from Julia source
   .. code-block:: julia

       sizehint!(s, n)

   Suggest that collection ``s`` reserve capacity for at least ``n`` elements. This can improve performance.

Fully implemented by:

- :obj:`ObjectIdDict`
- :obj:`Dict`
- :obj:`WeakKeyDict`

Partially implemented by:

- :obj:`IntSet`
- :obj:`Set`
- :obj:`EnvHash`
- :obj:`Array`
- :obj:`BitArray`

Set-Like Collections
--------------------

.. function:: Set([itr])

   .. Docstring generated from Julia source
   ::

              Set([itr])

   Construct a :obj:`Set` of the values generated by the given iterable object, or an empty set.
   Should be used instead of :obj:`IntSet` for sparse integer sets, or for sets of arbitrary objects.

.. function:: IntSet([itr])

   .. Docstring generated from Julia source
   ::

              IntSet([itr])

   Construct a sorted set of the integers generated by the given iterable object, or an empty set. Implemented as a bit string, and therefore designed for dense integer sets. Only non-negative integers can be stored. If the set will be sparse (for example holding a single very large integer), use :obj:`Set` instead.

.. function:: union(s1,s2...)

   .. Docstring generated from Julia source
   .. code-block:: julia

       union(s1,s2...)
       ∪(s1,s2)

   Construct the union of two or more sets. Maintains order with arrays.

.. function:: union!(s, iterable)

   .. Docstring generated from Julia source
   .. code-block:: julia

       union!(s, iterable)

   Union each element of ``iterable`` into set ``s`` in-place.

.. function:: intersect(s1,s2...)

   .. Docstring generated from Julia source
   .. code-block:: julia

       intersect(s1,s2...)
       ∩(s1,s2)

   Construct the intersection of two or more sets. Maintains order and multiplicity of the first argument for arrays and ranges.

.. function:: setdiff(s1,s2)

   .. Docstring generated from Julia source
   .. code-block:: julia

       setdiff(s1,s2)

   Construct the set of elements in ``s1`` but not ``s2``\ . Maintains order with arrays. Note that both arguments must be collections, and both will be iterated over. In particular, ``setdiff(set,element)`` where ``element`` is a potential member of ``set``\ , will not work in general.

.. function:: setdiff!(s, iterable)

   .. Docstring generated from Julia source
   .. code-block:: julia

       setdiff!(s, iterable)

   Remove each element of ``iterable`` from set ``s`` in-place.

.. function:: symdiff(s1,s2...)

   .. Docstring generated from Julia source
   .. code-block:: julia

       symdiff(s1,s2...)

   Construct the symmetric difference of elements in the passed in sets or arrays. Maintains order with arrays.

.. function:: symdiff!(s, n)

   .. Docstring generated from Julia source
   .. code-block:: julia

       symdiff!(s, n)

   The set ``s`` is destructively modified to toggle the inclusion of integer ``n``\ .

   .. code-block:: julia

       symdiff!(s, itr)

   For each element in ``itr``\ , destructively toggle its inclusion in set ``s``\ .

   .. code-block:: julia

       symdiff!(s1, s2)

   Construct the symmetric difference of sets ``s1`` and ``s2``\ , storing the result in ``s1``\ .

.. function:: symdiff!(s, itr)

   .. Docstring generated from Julia source
   .. code-block:: julia

       symdiff!(s, n)

   The set ``s`` is destructively modified to toggle the inclusion of integer ``n``\ .

   .. code-block:: julia

       symdiff!(s, itr)

   For each element in ``itr``\ , destructively toggle its inclusion in set ``s``\ .

   .. code-block:: julia

       symdiff!(s1, s2)

   Construct the symmetric difference of sets ``s1`` and ``s2``\ , storing the result in ``s1``\ .

.. function:: symdiff!(s1, s2)

   .. Docstring generated from Julia source
   .. code-block:: julia

       symdiff!(s, n)

   The set ``s`` is destructively modified to toggle the inclusion of integer ``n``\ .

   .. code-block:: julia

       symdiff!(s, itr)

   For each element in ``itr``\ , destructively toggle its inclusion in set ``s``\ .

   .. code-block:: julia

       symdiff!(s1, s2)

   Construct the symmetric difference of sets ``s1`` and ``s2``\ , storing the result in ``s1``\ .

.. function:: complement(s)

   .. Docstring generated from Julia source
   ::

              complement(s)

   Returns the set-complement of :obj:`IntSet` ``s``.

.. function:: complement!(s)

   .. Docstring generated from Julia source
   ::

              complement!(s)

   Mutates :obj:`IntSet` ``s`` into its set-complement.

.. function:: intersect!(s1, s2)

   .. Docstring generated from Julia source
   .. code-block:: julia

       intersect!(s1, s2)

   Intersects sets ``s1`` and ``s2`` and overwrites the set ``s1`` with the result. If needed, ``s1`` will be expanded to the size of ``s2``\ .

.. function:: issubset(A, S) -> Bool

   .. Docstring generated from Julia source
   ::

              issubset(a, b)
              ⊆(A,S) -> Bool
              ⊈(A,S) -> Bool
              ⊊(A,S) -> Bool

   Determine whether every element of ``a`` is also in ``b``, using :func:`in`.

   ::

              issubset(A, S) -> Bool
              ⊆(A,S) -> Bool

   True if A is a subset of or equal to S.

Fully implemented by:

- :obj:`IntSet`
- :obj:`Set`

Partially implemented by:

- :obj:`Array`

Dequeues
--------

.. function:: push!(collection, items...) -> collection

   .. Docstring generated from Julia source
   ::

              push!(collection, items...) -> collection

   Insert one or more ``items`` at the end of ``collection``.

   .. doctest::

     julia> push!([1, 2, 3], 4, 5, 6)
     6-element Array{Int64,1}:
      1
      2
      3
      4
      5
      6

   Use :func:`append!` to add all the elements of another collection to
   ``collection``.
   The result of the preceding example is equivalent to
   ``append!([1, 2, 3], [4, 5, 6])``.

.. function:: pop!(collection) -> item

   .. Docstring generated from Julia source
   ::

              pop!(collection, key[, default])

   Delete and return the mapping for ``key`` if it exists in ``collection``, otherwise return ``default``, or throw an error if default is not specified.

   ::

              pop!(collection) -> item

   Remove the last item in ``collection`` and return it.

   .. doctest::

     julia> A=[1, 2, 3, 4, 5, 6]
     6-element Array{Int64,1}:
      1
      2
      3
      4
      5
      6

     julia> pop!(A)
     6

     julia> A
     5-element Array{Int64,1}:
      1
      2
      3
      4
      5

.. function:: unshift!(collection, items...) -> collection

   .. Docstring generated from Julia source
   ::

              unshift!(collection, items...) -> collection

   Insert one or more ``items`` at the beginning of ``collection``.

   .. doctest::

     julia> unshift!([1, 2, 3, 4], 5, 6)
     6-element Array{Int64,1}:
      5
      6
      1
      2
      3
      4

.. function:: shift!(collection) -> item

   .. Docstring generated from Julia source
   ::

              shift!(collection) -> item

   Remove the first ``item`` from ``collection``.

   .. doctest::

     julia> A = [1, 2, 3, 4, 5, 6]
     6-element Array{Int64,1}:
      1
      2
      3
      4
      5
      6

     julia> shift!(A)
     1

     julia> A
     5-element Array{Int64,1}:
      2
      3
      4
      5
      6

.. function:: insert!(collection, index, item)

   .. Docstring generated from Julia source
   ::

              insert!(collection, index, item)

   Insert an ``item`` into ``collection`` at the given ``index``.
   ``index`` is the index of ``item`` in the resulting ``collection``.

   .. doctest::

      julia> insert!([6, 5, 4, 2, 1], 4, 3)
      6-element Array{Int64,1}:
       6
       5
       4
       3
       2
       1

.. function:: deleteat!(collection, index)

   .. Docstring generated from Julia source
   ::

              deleteat!(collection, index)

   Remove the item at the given ``index`` and return the modified ``collection``.
   Subsequent items are shifted to fill the resulting gap.

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], 2)
     5-element Array{Int64,1}:
      6
      4
      3
      2
      1

   ::

              deleteat!(collection, itr)

   Remove the items at the indices given by ``itr``, and return the modified ``collection``.
   Subsequent items are shifted to fill the resulting gap. ``itr`` must be sorted and unique.

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], 1:2:5)
     3-element Array{Int64,1}:
      5
      3
      1

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], (2, 2))
     ERROR: ArgumentError: indices must be unique and sorted
      in deleteat! at array.jl:533

.. function:: deleteat!(collection, itr)

   .. Docstring generated from Julia source
   ::

              deleteat!(collection, index)

   Remove the item at the given ``index`` and return the modified ``collection``.
   Subsequent items are shifted to fill the resulting gap.

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], 2)
     5-element Array{Int64,1}:
      6
      4
      3
      2
      1

   ::

              deleteat!(collection, itr)

   Remove the items at the indices given by ``itr``, and return the modified ``collection``.
   Subsequent items are shifted to fill the resulting gap. ``itr`` must be sorted and unique.

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], 1:2:5)
     3-element Array{Int64,1}:
      5
      3
      1

   .. doctest::

     julia> deleteat!([6, 5, 4, 3, 2, 1], (2, 2))
     ERROR: ArgumentError: indices must be unique and sorted
      in deleteat! at array.jl:533

.. function:: splice!(collection, index, [replacement]) -> item

   .. Docstring generated from Julia source
   ::

              splice!(collection, index, [replacement]) -> item

   Remove the item at the given index, and return the removed item. Subsequent items
   are shifted down to fill the resulting gap. If specified, replacement values from
   an ordered collection will be spliced in place of the removed item.

   .. doctest::

     julia> A = [6, 5, 4, 3, 2, 1]; splice!(A, 5)
     2

     julia> A
     5-element Array{Int64,1}:
      6
      5
      4
      3
      1

     julia> splice!(A, 5, -1)
     1

     julia> A
     5-element Array{Int64,1}:
       6
       5
       4
       3
      -1

     julia> splice!(A, 1, [-1, -2, -3])
     6

     julia> A
     7-element Array{Int64,1}:
      -1
      -2
      -3
       5
       4
       3
      -1

   To insert ``replacement`` before an index ``n`` without removing any items, use
   ``splice!(collection, n:n-1, replacement)``.

   ::

              splice!(collection, range, [replacement]) -> items

   Remove items in the specified index range, and return a collection containing the
   removed items. Subsequent items are shifted down to fill the resulting gap.
   If specified, replacement values from an ordered collection will be spliced in place
   of the removed items.

   To insert ``replacement`` before an index ``n`` without removing any items, use
   ``splice!(collection, n:n-1, replacement)``.

   .. doctest::

     julia> splice!(A, 4:3, 2)
     0-element Array{Int64,1}

     julia> A
     8-element Array{Int64,1}:
      -1
      -2
      -3
       2
       5
       4
       3
      -1

.. function:: splice!(collection, range, [replacement]) -> items

   .. Docstring generated from Julia source
   ::

              splice!(collection, index, [replacement]) -> item

   Remove the item at the given index, and return the removed item. Subsequent items
   are shifted down to fill the resulting gap. If specified, replacement values from
   an ordered collection will be spliced in place of the removed item.

   .. doctest::

     julia> A = [6, 5, 4, 3, 2, 1]; splice!(A, 5)
     2

     julia> A
     5-element Array{Int64,1}:
      6
      5
      4
      3
      1

     julia> splice!(A, 5, -1)
     1

     julia> A
     5-element Array{Int64,1}:
       6
       5
       4
       3
      -1

     julia> splice!(A, 1, [-1, -2, -3])
     6

     julia> A
     7-element Array{Int64,1}:
      -1
      -2
      -3
       5
       4
       3
      -1

   To insert ``replacement`` before an index ``n`` without removing any items, use
   ``splice!(collection, n:n-1, replacement)``.

   ::

              splice!(collection, range, [replacement]) -> items

   Remove items in the specified index range, and return a collection containing the
   removed items. Subsequent items are shifted down to fill the resulting gap.
   If specified, replacement values from an ordered collection will be spliced in place
   of the removed items.

   To insert ``replacement`` before an index ``n`` without removing any items, use
   ``splice!(collection, n:n-1, replacement)``.

   .. doctest::

     julia> splice!(A, 4:3, 2)
     0-element Array{Int64,1}

     julia> A
     8-element Array{Int64,1}:
      -1
      -2
      -3
       2
       5
       4
       3
      -1

.. function:: resize!(collection, n) -> collection

   .. Docstring generated from Julia source
   ::

              resize!(collection, n) -> collection

   Resize ``collection`` to contain ``n`` elements.
   If ``n`` is smaller than the current collection length, the first ``n``
   elements will be retained. If ``n`` is larger, the new elements are not
   guaranteed to be initialized.

   .. doctest::

     julia> resize!([6, 5, 4, 3, 2, 1], 3)
     3-element Array{Int64,1}:
      6
      5
      4

   .. code-block:: julia

     julia> resize!([6, 5, 4, 3, 2, 1], 8)
     8-element Array{Int64,1}:
      6
      5
      4
      3
      2
      1
      0
      0

.. function:: append!(collection, collection2) -> collection.

   .. Docstring generated from Julia source
   ::

              append!(collection, collection2) -> collection.

   Add the elements of ``collection2`` to the end of ``collection``.

   .. doctest::

      julia> append!([1],[2,3])
      3-element Array{Int64,1}:
       1
       2
       3

   .. doctest::

      julia> append!([1, 2, 3], [4, 5, 6])
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

   Use :func:`push!` to add individual items to ``collection`` which are not
   already themselves in another collection.
   The result is of the preceding example is equivalent to
   ``push!([1, 2, 3], 4, 5, 6)``.

.. function:: prepend!(collection, items) -> collection

   .. Docstring generated from Julia source
   ::

              prepend!(collection, items) -> collection

   Insert the elements of ``items`` to the beginning of ``collection``.

   .. doctest::

      julia> prepend!([3],[1,2])
      3-element Array{Int64,1}:
       1
       2
       3

Fully implemented by:

- :obj:`Vector` (a.k.a. 1-dimensional :obj:`Array`)
- :obj:`BitVector` (a.k.a. 1-dimensional :obj:`BitArray`)

.. module:: Base.Collections

PriorityQueue
-------------

The :obj:`PriorityQueue` type is available from the :mod:`Collections` module. It provides
a basic priority queue implementation allowing for arbitrary key and priority types.
Multiple identical keys are not permitted, but the priority of existing keys can be
changed efficiently.

.. function:: PriorityQueue(K, V, [ord])

   .. Docstring generated from Julia source
   ::

              PriorityQueue(K, V, [ord])

   Construct a new :obj:`PriorityQueue`, with keys of type ``K`` and values/priorites of
   type ``V``. If an order is not given, the priority queue is min-ordered using
   the default comparison for ``V``.

.. function:: enqueue!(pq, k, v)

   .. Docstring generated from Julia source
   .. code-block:: julia

       enqueue!(pq, k, v)

   Insert the a key ``k`` into a priority queue ``pq`` with priority ``v``\ .

.. function:: dequeue!(pq)

   .. Docstring generated from Julia source
   .. code-block:: julia

       dequeue!(pq)

   Remove and return the lowest priority key from a priority queue.

.. function:: peek(pq)

   .. Docstring generated from Julia source
   .. code-block:: julia

       peek(pq)

   Return the lowest priority key from a priority queue without removing that key from the queue.

:obj:`PriorityQueue` also behaves similarly to a :obj:`Dict` in that keys can be
inserted and priorities accessed or changed using indexing notation.

  .. doctest::

    julia> # Julia code
           pq = Collections.PriorityQueue();

    julia> # Insert keys with associated priorities
           pq["a"] = 10; pq["b"] = 5; pq["c"] = 15; pq
    Base.Collections.PriorityQueue{Any,Any,Base.Order.ForwardOrdering} with 3 entries:
      "c" => 15
      "b" => 5
      "a" => 10

    julia> # Change the priority of an existing key
           pq["a"] = 0; pq
    Base.Collections.PriorityQueue{Any,Any,Base.Order.ForwardOrdering} with 3 entries:
      "c" => 15
      "b" => 5
      "a" => 0

Heap Functions
--------------

Along with the :obj:`PriorityQueue` type, the :mod:`Collections` module provides
lower level functions for performing binary heap operations on arrays. Each
function takes an optional ordering argument. If not given, default ordering
is used, so that elements popped from the heap are given in ascending order.

.. function:: heapify(v, [ord])

   .. Docstring generated from Julia source
   .. code-block:: julia

       heapify(v, [ord])

   Return a new vector in binary heap order, optionally using the given ordering.

.. function:: heapify!(v, [ord])

   .. Docstring generated from Julia source
   ::

              heapify!(v, [ord])

   In-place :func:`heapify`.

.. function:: isheap(v, [ord])

   .. Docstring generated from Julia source
   .. code-block:: julia

       isheap(v, [ord])

   Return true iff an array is heap-ordered according to the given order.

.. function:: heappush!(v, x, [ord])

   .. Docstring generated from Julia source
   .. code-block:: julia

       heappush!(v, x, [ord])

   Given a binary heap-ordered array, push a new element ``x``\ , preserving the heap property. For efficiency, this function does not check that the array is indeed heap-ordered.

.. function:: heappop!(v, [ord])

   .. Docstring generated from Julia source
   .. code-block:: julia

       heappop!(v, [ord])

   Given a binary heap-ordered array, remove and return the lowest ordered element. For efficiency, this function does not check that the array is indeed heap-ordered.

