.. module:: Base.Iterators

*********************
 Iteration utilities
*********************

.. function:: zip(iters...)

   .. Docstring generated from Julia source

   For a set of iterable objects, returns an iterable of tuples, where the ``i``\ th tuple contains the ``i``\ th component of each input iterable.

   Note that :func:`zip` is its own inverse: ``collect(zip(zip(a...)...)) == collect(a)``\ .

   .. doctest::

       julia> a = 1:5
       1:5

       julia> b = ["e","d","b","c","a"]
       5-element Array{String,1}:
        "e"
        "d"
        "b"
        "c"
        "a"

       julia> c = zip(a,b)
       Base.Iterators.Zip2{UnitRange{Int64},Array{String,1}}(1:5,String["e","d","b","c","a"])

       julia> length(c)
       5

       julia> first(c)
       (1,"e")

.. function:: enumerate(iter)

   .. Docstring generated from Julia source

   An iterator that yields ``(i, x)`` where ``i`` is a counter starting at 1, and ``x`` is the ``i``\ th value from the given iterator. It's useful when you need not only the values ``x`` over which you are iterating, but also the number of iterations so far. Note that ``i`` may not be valid for indexing ``iter``\ ; it's also possible that ``x != iter[i]``\ , if ``iter`` has indices that do not start at 1.

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

   An iterator that yields the same elements as ``iter``\ , but starting at the given ``state``\ .

.. function:: countfrom(start=1, step=1)

   .. Docstring generated from Julia source

   An iterator that counts forever, starting at ``start`` and incrementing by ``step``\ .

.. function:: take(iter, n)

   .. Docstring generated from Julia source

   An iterator that generates at most the first ``n`` elements of ``iter``\ .

   .. doctest::

       julia> a = 1:2:11
       1:2:11

       julia> collect(a)
       6-element Array{Int64,1}:
         1
         3
         5
         7
         9
        11

       julia> collect(Iterators.take(a,3))
       3-element Array{Int64,1}:
        1
        3
        5

.. function:: drop(iter, n)

   .. Docstring generated from Julia source

   An iterator that generates all but the first ``n`` elements of ``iter``\ .

   .. doctest::

       julia> a = 1:2:11
       1:2:11

       julia> collect(a)
       6-element Array{Int64,1}:
         1
         3
         5
         7
         9
        11

       julia> collect(Iterators.drop(a,4))
       2-element Array{Int64,1}:
         9
        11

.. function:: cycle(iter)

   .. Docstring generated from Julia source

   An iterator that cycles through ``iter`` forever.

.. function:: repeated(x[, n::Int])

   .. Docstring generated from Julia source

   An iterator that generates the value ``x`` forever. If ``n`` is specified, generates ``x`` that many times (equivalent to ``take(repeated(x), n)``\ ).

   .. doctest::

       julia> a = Iterators.repeated([1 2], 4);

       julia> collect(a)
       4-element Array{Array{Int64,2},1}:
        [1 2]
        [1 2]
        [1 2]
        [1 2]

.. function:: product(iters...)

   .. Docstring generated from Julia source

   Returns an iterator over the product of several iterators. Each generated element is a tuple whose ``i``\ th element comes from the ``i``\ th argument iterator. The first iterator changes the fastest. Example:

   .. doctest::

       julia> collect(Iterators.product(1:2,3:5))
       6-element Array{Tuple{Int64,Int64},1}:
        (1,3)
        (2,3)
        (1,4)
        (2,4)
        (1,5)
        (2,5)

.. function:: flatten(iter)

   .. Docstring generated from Julia source

   Given an iterator that yields iterators, return an iterator that yields the elements of those iterators. Put differently, the elements of the argument iterator are concatenated. Example:

   .. doctest::

       julia> collect(Iterators.flatten((1:2, 8:9)))
       4-element Array{Int64,1}:
        1
        2
        8
        9

.. function:: partition(collection, n)

   .. Docstring generated from Julia source

   Iterate over a collection ``n`` elements at a time.

   .. doctest::

       julia> collect(Iterators.partition([1,2,3,4,5], 2))
       3-element Array{Array{Int64,1},1}:
        [1,2]
        [3,4]
        [5]

