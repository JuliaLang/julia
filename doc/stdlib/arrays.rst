.. currentmodule:: Base

.. _stdlib-arrays:

********
 Arrays
********

Basic functions
---------------

.. function:: ndims(A) -> Integer

   .. Docstring generated from Julia source

   Returns the number of dimensions of ``A``

.. function:: size(A, [dim...])

   .. Docstring generated from Julia source

   Returns a tuple containing the dimensions of ``A``\ . Optionally you can specify the dimension(s) you want the length of, and get the length of that dimension, or a tuple of the lengths of dimensions you asked for.:

   .. code-block:: julia

       julia> A = rand(2,3,4);

       julia> size(A, 2)
       3

       julia> size(A,3,2)
       (4,3)

.. function:: iseltype(A,T)

   .. Docstring generated from Julia source

   Tests whether ``A`` or its elements are of type ``T``\ .

.. function:: length(A) -> Integer

   .. Docstring generated from Julia source

   Returns the number of elements in ``A``\ .

.. function:: eachindex(A...)

   .. Docstring generated from Julia source

   Creates an iterable object for visiting each index of an AbstractArray ``A`` in an efficient manner. For array types that have opted into fast linear indexing (like ``Array``\ ), this is simply the range ``1:length(A)``\ . For other array types, this returns a specialized Cartesian range to efficiently index into the array with indices specified for every dimension. For other iterables, including strings and dictionaries, this returns an iterator object supporting arbitrary index types (e.g. unevenly spaced or non-integer indices).

   Example for a sparse 2-d array:

   .. code-block:: julia

       julia> A = sprand(2, 3, 0.5)
       2x3 sparse matrix with 4 Float64 entries:
           [1, 1]  =  0.598888
           [1, 2]  =  0.0230247
           [1, 3]  =  0.486499
           [2, 3]  =  0.809041

       julia> for iter in eachindex(A)
                  @show iter.I_1, iter.I_2
                  @show A[iter]
              end
       (iter.I_1,iter.I_2) = (1,1)
       A[iter] = 0.5988881393454597
       (iter.I_1,iter.I_2) = (2,1)
       A[iter] = 0.0
       (iter.I_1,iter.I_2) = (1,2)
       A[iter] = 0.02302469881746183
       (iter.I_1,iter.I_2) = (2,2)
       A[iter] = 0.0
       (iter.I_1,iter.I_2) = (1,3)
       A[iter] = 0.4864987874354343
       (iter.I_1,iter.I_2) = (2,3)
       A[iter] = 0.8090413606455655

   If you supply more than one ``AbstractArray`` argument, ``eachindex`` will create an iterable object that is fast for all arguments (a ``UnitRange`` if all inputs have fast linear indexing, a CartesianRange otherwise).  If the arrays have different sizes and/or dimensionalities, ``eachindex`` returns an iterable that spans the largest range along each dimension.

.. function:: Base.linearindexing(A)

   .. Docstring generated from Julia source

   ``linearindexing`` defines how an AbstractArray most efficiently accesses its elements. If ``Base.linearindexing(A)`` returns ``Base.LinearFast()``\ , this means that linear indexing with only one index is an efficient operation. If it instead returns ``Base.LinearSlow()`` (by default), this means that the array intrinsically accesses its elements with indices specified for every dimension. Since converting a linear index to multiple indexing subscripts is typically very expensive, this provides a traits-based mechanism to enable efficient generic code for all array types.

   An abstract array subtype ``MyArray`` that wishes to opt into fast linear indexing behaviors should define ``linearindexing`` in the type-domain:

   .. code-block:: julia

       Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()

.. function:: countnz(A)

   .. Docstring generated from Julia source

   Counts the number of nonzero values in array ``A`` (dense or sparse). Note that this is not a constant-time operation. For sparse matrices, one should usually use ``nnz``\ , which returns the number of stored values.

.. function:: conj!(A)

   .. Docstring generated from Julia source

   Convert an array to its complex conjugate in-place

.. function:: stride(A, k)

   .. Docstring generated from Julia source

   Returns the distance in memory (in number of elements) between adjacent elements in dimension ``k``\ .

.. function:: strides(A)

   .. Docstring generated from Julia source

   Returns a tuple of the memory strides in each dimension

.. function:: ind2sub(dims, index) -> subscripts

   .. Docstring generated from Julia source

   Returns a tuple of subscripts into an array with dimensions ``dims``\ , corresponding to the linear index ``index``\ .

   **Example**: ``i, j, ... = ind2sub(size(A), indmax(A))`` provides the indices of the maximum element

.. function:: ind2sub(a, index) -> subscripts

   .. Docstring generated from Julia source

   Returns a tuple of subscripts into array ``a`` corresponding to the linear index ``index``

.. function:: sub2ind(dims, i, j, k...) -> index

   .. Docstring generated from Julia source

   The inverse of ``ind2sub``\ , returns the linear index corresponding to the provided subscripts

Constructors
------------

.. function:: Array(dims)

   .. Docstring generated from Julia source

   ``Array{T}(dims)`` constructs an uninitialized dense array with element type ``T``\ . ``dims`` may be a tuple or a series of integer arguments. The syntax ``Array(T, dims)`` is also available, but deprecated.

.. function:: getindex(type[, elements...])

   .. Docstring generated from Julia source

   Construct a 1-d array of the specified type. This is usually called with the syntax ``Type[]``. Element values can be specified using ``Type[a,b,c,...]``.

.. function:: cell(dims)

   .. Docstring generated from Julia source

   Construct an uninitialized cell array (heterogeneous array). ``dims`` can be either a tuple or a series of integer arguments.

.. function:: zeros(type, dims)

   .. Docstring generated from Julia source

   Create an array of all zeros of specified type. The type defaults to Float64 if not specified.

.. function:: zeros(A)

   .. Docstring generated from Julia source

   Create an array of all zeros with the same element type and shape as ``A``\ .

.. function:: ones(type, dims)

   .. Docstring generated from Julia source

   Create an array of all ones of specified type. The type defaults to ``Float64`` if not specified.

.. function:: ones(A)

   .. Docstring generated from Julia source

   Create an array of all ones with the same element type and shape as ``A``\ .

.. function:: trues(dims)

   .. Docstring generated from Julia source

   Create a ``BitArray`` with all values set to ``true``

.. function:: falses(dims)

   .. Docstring generated from Julia source

   Create a ``BitArray`` with all values set to ``false``

.. function:: fill(x, dims)

   .. Docstring generated from Julia source

   Create an array filled with the value ``x``\ . For example, ``fill(1.0, (10,10))`` returns a 10x10 array of floats, with each element initialized to ``1.0``\ .

   If ``x`` is an object reference, all elements will refer to the same object. ``fill(Foo(), dims)`` will return an array filled with the result of evaluating ``Foo()`` once.

.. function:: fill!(A, x)

   .. Docstring generated from Julia source

   Fill array ``A`` with the value ``x``\ . If ``x`` is an object reference, all elements will refer to the same object. ``fill!(A, Foo())`` will return ``A`` filled with the result of evaluating ``Foo()`` once.

.. function:: reshape(A, dims)

   .. Docstring generated from Julia source

   Create an array with the same data as the given array, but with different dimensions. An implementation for a particular type of array may choose whether the data is copied or shared.

.. function:: similar(array, [element_type=eltype(array)], [dims=size(array)])

   .. Docstring generated from Julia source

   Create an uninitialized mutable array with the given element type and size, based upon the given source array. The second and third arguments are both optional, defaulting to the given array's ``eltype`` and ``size``\ . The dimensions may be specified either as a single tuple argument or as a series of integer arguments.

   Custom AbstractArray subtypes may choose which specific array type is best-suited to return for the given element type and dimensionality. If they do not specialize this method, the default is an ``Array(element_type, dims...)``\ .

   For example, ``similar(1:10, 1, 4)`` returns an uninitialized ``Array{Int,2}`` since ranges are neither mutable nor support 2 dimensions:

   .. code-block:: julia

       julia> similar(1:10, 1, 4)
       1x4 Array{Int64,2}:
        4419743872  4374413872  4419743888  0

   Conversely, ``similar(trues(10,10), 2)`` returns an uninitialized ``BitVector`` with two elements since ``BitArray``\ s are both mutable and can support 1-dimensional arrays:

   .. code-block:: julia

       julia> similar(trues(10,10), 2)
       2-element BitArray{1}:
        false
        false

   Since ``BitArray``\ s can only store elements of type ``Bool``\ , however, if you request a different element type it will create a regular ``Array`` instead:

   .. code-block:: julia

       julia> similar(falses(10), Float64, 2, 4)
       2x4 Array{Float64,2}:
        2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
        2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314

.. function:: reinterpret(type, A)

   .. Docstring generated from Julia source

   Change the type-interpretation of a block of memory. For example, ``reinterpret(Float32, UInt32(7))`` interprets the 4 bytes corresponding to ``UInt32(7)`` as a ``Float32``\ . For arrays, this constructs an array with the same binary data as the given array, but with the specified element type.

.. function:: eye(n)

   .. Docstring generated from Julia source

   ``n``\ -by-``n`` identity matrix

.. function:: eye(m, n)

   .. Docstring generated from Julia source

   ``m``\ -by-``n`` identity matrix

.. function:: eye(A)

   .. Docstring generated from Julia source

   Constructs an identity matrix of the same dimensions and type as ``A``\ .

.. function:: linspace(start, stop, n=100)

   .. Docstring generated from Julia source

   Construct a range of ``n`` linearly spaced elements from ``start`` to ``stop``\ .

.. function:: logspace(start, stop, n=50)

   .. Docstring generated from Julia source

   Construct a vector of ``n`` logarithmically spaced numbers from ``10^start`` to ``10^stop``\ .

Mathematical operators and functions
------------------------------------

All mathematical operations and functions are supported for arrays

.. function:: broadcast(f, As...)

   .. Docstring generated from Julia source

   Broadcasts the arrays ``As`` to a common size by expanding singleton dimensions, and returns an array of the results ``f(as...)`` for each position.

.. function:: broadcast!(f, dest, As...)

   .. Docstring generated from Julia source

   Like ``broadcast``\ , but store the result of ``broadcast(f, As...)`` in the ``dest`` array. Note that ``dest`` is only used to store the result, and does not supply arguments to ``f`` unless it is also listed in the ``As``\ , as in ``broadcast!(f, A, A, B)`` to perform ``A[:] = broadcast(f, A, B)``\ .

.. function:: bitbroadcast(f, As...)

   .. Docstring generated from Julia source

   Like ``broadcast``\ , but allocates a ``BitArray`` to store the result, rather then an ``Array``\ .

.. function:: broadcast_function(f)

   .. Docstring generated from Julia source

   Returns a function ``broadcast_f`` such that ``broadcast_function(f)(As...) === broadcast(f, As...)``\ . Most useful in the form ``const broadcast_f = broadcast_function(f)``\ .

.. function:: broadcast!_function(f)

   .. Docstring generated from Julia source

   Like ``broadcast_function``\ , but for ``broadcast!``\ .

Indexing, Assignment, and Concatenation
---------------------------------------

.. function:: getindex(A, inds...)

   .. Docstring generated from Julia source

   Returns a subset of array ``A`` as specified by ``inds``, where each ``ind`` may be an ``Int``, a ``Range``, or a ``Vector``. See the manual section on :ref:`array indexing <man-array-indexing>` for details.

.. function:: sub(A, inds...)

   .. Docstring generated from Julia source

   Like :func:`getindex`, but returns a view into the parent array ``A`` with the given indices instead of making a copy.  Calling :func:`getindex` or :func:`setindex!` on the returned :obj:`SubArray` computes the indices to the parent array on the fly without checking bounds.

.. function:: parent(A)

   .. Docstring generated from Julia source

   Returns the "parent array" of an array view type (e.g., ``SubArray``\ ), or the array itself if it is not a view

.. function:: parentindexes(A)

   .. Docstring generated from Julia source

   From an array view ``A``\ , returns the corresponding indexes in the parent

.. function:: slicedim(A, d, i)

   .. Docstring generated from Julia source

   Return all the data of ``A`` where the index for dimension ``d`` equals ``i``\ . Equivalent to ``A[:,:,...,i,:,:,...]`` where ``i`` is in position ``d``\ .

.. function:: slice(A, inds...)

   .. Docstring generated from Julia source

   Returns a view of array ``A`` with the given indices like :func:`sub`, but drops all dimensions indexed with scalars.

.. function:: setindex!(A, X, inds...)

   .. Docstring generated from Julia source

   Store values from array ``X`` within some subset of ``A`` as specified by ``inds``\ .

.. function:: broadcast_getindex(A, inds...)

   .. Docstring generated from Julia source

   Broadcasts the ``inds`` arrays to a common size like ``broadcast``\ , and returns an array of the results ``A[ks...]``\ , where ``ks`` goes over the positions in the broadcast.

.. function:: broadcast_setindex!(A, X, inds...)

   .. Docstring generated from Julia source

   Broadcasts the ``X`` and ``inds`` arrays to a common size and stores the value from each position in ``X`` at the indices given by the same positions in ``inds``\ .

.. function:: cat(dims, A...)

   .. Docstring generated from Julia source

   Concatenate the input arrays along the specified dimensions in the iterable ``dims``\ . For dimensions not in ``dims``\ , all input arrays should have the same size, which will also be the size of the output array along that dimension. For dimensions in ``dims``\ , the size of the output array is the sum of the sizes of the input arrays along that dimension. If ``dims`` is a single number, the different arrays are tightly stacked along that dimension. If ``dims`` is an iterable containing several dimensions, this allows to construct block diagonal matrices and their higher-dimensional analogues by simultaneously increasing several dimensions for every new input array and putting zero blocks elsewhere. For example, ``cat([1,2], matrices...)`` builds a block diagonal matrix, i.e. a block matrix with ``matrices[1]``\ , ``matrices[2]``\ , ... as diagonal blocks and matching zero blocks away from the diagonal.

.. function:: vcat(A...)

   .. Docstring generated from Julia source

   Concatenate along dimension 1

.. function:: hcat(A...)

   .. Docstring generated from Julia source

   Concatenate along dimension 2

.. function:: hvcat(rows::Tuple{Vararg{Int}}, values...)

   .. Docstring generated from Julia source

   Horizontal and vertical concatenation in one call. This function is called for block matrix syntax. The first argument specifies the number of arguments to concatenate in each block row.

   .. doctest::

       julia> a, b, c, d, e, f = 1, 2, 3, 4, 5, 6
       (1,2,3,4,5,6)

       julia> [a b c; d e f]
       2x3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> hvcat((3,3), a,b,c,d,e,f)
       2x3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> [a b;c d; e f]
       3x2 Array{Int64,2}:
        1  2
        3  4
        5  6

       julia> hvcat((2,2,2), a,b,c,d,e,f)
       3x2 Array{Int64,2}:
        1  2
        3  4
        5  6

   If the first argument is a single integer ``n``\ , then all block rows are assumed to have ``n`` block columns.

.. function:: flipdim(A, d)

   .. Docstring generated from Julia source

   Reverse ``A`` in dimension ``d``\ .

.. function:: circshift(A,shifts)

   .. Docstring generated from Julia source

   Circularly shift the data in an array. The second argument is a vector giving the amount to shift in each dimension.

.. function:: find(A)

   .. Docstring generated from Julia source

   Return a vector of the linear indexes of the non-zeros in ``A`` (determined by ``A[i]!=0``\ ). A common use of this is to convert a boolean array to an array of indexes of the ``true`` elements.

.. function:: find(f,A)

   .. Docstring generated from Julia source

   Return a vector of the linear indexes of ``A`` where ``f`` returns ``true``\ .

.. function:: findn(A)

   .. Docstring generated from Julia source

   Return a vector of indexes for each dimension giving the locations of the non-zeros in ``A`` (determined by ``A[i]!=0``\ ).

.. function:: findnz(A)

   .. Docstring generated from Julia source

   Return a tuple ``(I, J, V)`` where ``I`` and ``J`` are the row and column indexes of the non-zero values in matrix ``A``\ , and ``V`` is a vector of the non-zero values.

.. function:: findfirst(A)

   .. Docstring generated from Julia source

   Return the index of the first non-zero value in ``A`` (determined by ``A[i]!=0``\ ).

.. function:: findfirst(A,v)

   .. Docstring generated from Julia source

   Return the index of the first element equal to ``v`` in ``A``\ .

.. function:: findfirst(predicate, A)

   .. Docstring generated from Julia source

   Return the index of the first element of ``A`` for which ``predicate`` returns ``true``\ .

.. function:: findlast(A)

   .. Docstring generated from Julia source

   Return the index of the last non-zero value in ``A`` (determined by ``A[i]!=0``\ ).

.. function:: findlast(A, v)

   .. Docstring generated from Julia source

   Return the index of the last element equal to ``v`` in ``A``\ .

.. function:: findlast(predicate, A)

   .. Docstring generated from Julia source

   Return the index of the last element of ``A`` for which ``predicate`` returns ``true``\ .

.. function:: findnext(A, i)

   .. Docstring generated from Julia source

   Find the next index >= ``i`` of a non-zero element of ``A``\ , or ``0`` if not found.

.. function:: findnext(predicate, A, i)

   .. Docstring generated from Julia source

   Find the next index >= ``i`` of an element of ``A`` for which ``predicate`` returns ``true``\ , or ``0`` if not found.

.. function:: findnext(A, v, i)

   .. Docstring generated from Julia source

   Find the next index >= ``i`` of an element of ``A`` equal to ``v`` (using ``==``\ ), or ``0`` if not found.

.. function:: findprev(A, i)

   .. Docstring generated from Julia source

   Find the previous index <= ``i`` of a non-zero element of ``A``\ , or ``0`` if not found.

.. function:: findprev(predicate, A, i)

   .. Docstring generated from Julia source

   Find the previous index <= ``i`` of an element of ``A`` for which ``predicate`` returns ``true``\ , or ``0`` if not found.

.. function:: findprev(A, v, i)

   .. Docstring generated from Julia source

   Find the previous index <= ``i`` of an element of ``A`` equal to ``v`` (using ``==``\ ), or ``0`` if not found.

.. function:: permutedims(A, perm)

   .. Docstring generated from Julia source

   Permute the dimensions of array ``A``\ . ``perm`` is a vector specifying a permutation of length ``ndims(A)``\ . This is a generalization of transpose for multi-dimensional arrays. Transpose is equivalent to ``permutedims(A, [2,1])``\ .

.. function:: ipermutedims(A, perm)

   .. Docstring generated from Julia source

   Like :func:`permutedims`, except the inverse of the given permutation is applied.

.. function:: permutedims!(dest, src, perm)

   .. Docstring generated from Julia source

   Permute the dimensions of array ``src`` and store the result in the array ``dest``\ . ``perm`` is a vector specifying a permutation of length ``ndims(src)``\ . The preallocated array ``dest`` should have ``size(dest) == size(src)[perm]`` and is completely overwritten. No in-place permutation is supported and unexpected results will happen if ``src`` and ``dest`` have overlapping memory regions.

.. function:: squeeze(A, dims)

   .. Docstring generated from Julia source

   Remove the dimensions specified by ``dims`` from array ``A``\ . Elements of ``dims`` must be unique and within the range ``1:ndims(A)``\ .

.. function:: vec(Array) -> Vector

   .. Docstring generated from Julia source

   Vectorize an array using column-major convention.

.. function:: promote_shape(s1, s2)

   .. Docstring generated from Julia source

   Check two array shapes for compatibility, allowing trailing singleton dimensions, and return whichever shape has more dimensions.

.. function:: checkbounds(array, indexes...)

   .. Docstring generated from Julia source

   Throw an error if the specified indexes are not in bounds for the given array. Subtypes of ``AbstractArray`` should specialize this method if they need to provide custom bounds checking behaviors.

.. function:: checkbounds(::Type{Bool}, dimlength::Integer, index)

   .. Docstring generated from Julia source

   Return a ``Bool`` describing if the given index is within the bounds of the given dimension length. Custom types that would like to behave as indices for all arrays can extend this method in order to provide a specialized bounds checking implementation.

.. function:: randsubseq(A, p) -> Vector

   .. Docstring generated from Julia source

   Return a vector consisting of a random subsequence of the given array ``A``\ , where each element of ``A`` is included (in order) with independent probability ``p``\ . (Complexity is linear in ``p*length(A)``\ , so this function is efficient even if ``p`` is small and ``A`` is large.) Technically, this process is known as "Bernoulli sampling" of ``A``\ .

.. function:: randsubseq!(S, A, p)

   .. Docstring generated from Julia source

   Like ``randsubseq``\ , but the results are stored in ``S`` (which is resized as needed).

Array functions
---------------

.. function:: cumprod(A, [dim])

   .. Docstring generated from Julia source

   Cumulative product along a dimension ``dim`` (defaults to 1).
   See also :func:`cumprod!` to use a preallocated output array,
   both for performance and to control the precision of the
   output (e.g. to avoid overflow).

.. function:: cumprod!(B, A, [dim])

   .. Docstring generated from Julia source

   Cumulative product of ``A`` along a dimension, storing the result in ``B``\ . The dimension defaults to 1.

.. function:: cumsum(A, [dim])

   .. Docstring generated from Julia source

   Cumulative sum along a dimension ``dim`` (defaults to 1).
   See also :func:`cumsum!` to use a preallocated output array,
   both for performance and to control the precision of the
   output (e.g. to avoid overflow).

.. function:: cumsum!(B, A, [dim])

   .. Docstring generated from Julia source

   Cumulative sum of ``A`` along a dimension, storing the result in ``B``\ . The dimension defaults to 1.

.. function:: cumsum_kbn(A, [dim])

   .. Docstring generated from Julia source

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy. The dimension defaults to 1.

.. function:: cummin(A, [dim])

   .. Docstring generated from Julia source

   Cumulative minimum along a dimension. The dimension defaults to 1.

.. function:: cummax(A, [dim])

   .. Docstring generated from Julia source

   Cumulative maximum along a dimension. The dimension defaults to 1.

.. function:: diff(A, [dim])

   .. Docstring generated from Julia source

   Finite difference operator of matrix or vector.

.. function:: gradient(F, [h])

   .. Docstring generated from Julia source

   Compute differences along vector ``F``\ , using ``h`` as the spacing between points. The default spacing is one.

.. function:: rot180(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` 180 degrees.

.. function:: rot180(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` 180 degrees an integer ``k`` number of times. If ``k`` is even, this is equivalent to a ``copy``\ .

.. function:: rotl90(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` left 90 degrees.

.. function:: rotl90(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` left 90 degrees an integer ``k`` number of times. If ``k`` is zero or a multiple of four, this is equivalent to a ``copy``\ .

.. function:: rotr90(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` right 90 degrees.

.. function:: rotr90(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` right 90 degrees an integer ``k`` number of times. If ``k`` is zero or a multiple of four, this is equivalent to a ``copy``\ .

.. function:: reducedim(f, A, dims[, initial])

   .. Docstring generated from Julia source

   Reduce 2-argument function ``f`` along dimensions of ``A``\ . ``dims`` is a vector specifying the dimensions to reduce, and ``initial`` is the initial value to use in the reductions. For ``+``\ , ``*``\ , ``max`` and ``min`` the ``initial`` argument is optional.

   The associativity of the reduction is implementation-dependent; if you need a particular associativity, e.g. left-to-right, you should write your own loop. See documentation for ``reduce``\ .

.. function:: mapreducedim(f, op, A, dims[, initial])

   .. Docstring generated from Julia source

   Evaluates to the same as ``reducedim(op, map(f, A), dims, f(initial))``\ , but is generally faster because the intermediate array is avoided.

.. function:: mapslices(f, A, dims)

   .. Docstring generated from Julia source

   Transform the given dimensions of array ``A`` using function ``f``\ . ``f`` is called on each slice of ``A`` of the form ``A[...,:,...,:,...]``\ . ``dims`` is an integer vector specifying where the colons go in this expression. The results are concatenated along the remaining dimensions. For example, if ``dims`` is ``[1,2]`` and ``A`` is 4-dimensional, ``f`` is called on ``A[:,:,i,j]`` for all ``i`` and ``j``\ .

.. function:: sum_kbn(A)

   .. Docstring generated from Julia source

   Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy.

Combinatorics
-------------

.. function:: nthperm(v, k)

   .. Docstring generated from Julia source

   Compute the kth lexicographic permutation of a vector.

.. function:: nthperm(p)

   .. Docstring generated from Julia source

   Return the ``k`` that generated permutation ``p``\ . Note that ``nthperm(nthperm([1:n], k)) == k`` for ``1 <= k <= factorial(n)``\ .

.. function:: nthperm!(v, k)

   .. Docstring generated from Julia source

   In-place version of :func:`nthperm`.

.. function:: randperm([rng,] n)

   .. Docstring generated from Julia source

   Construct a random permutation of length ``n``. The optional ``rng`` argument
   specifies a random number generator, see :ref:`Random Numbers <random-numbers>`.

.. function:: invperm(v)

   .. Docstring generated from Julia source

   Return the inverse permutation of v.

.. function:: isperm(v) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if ``v`` is a valid permutation.

.. function:: permute!(v, p)

   .. Docstring generated from Julia source

   Permute vector ``v`` in-place, according to permutation ``p``\ . No checking is done to verify that ``p`` is a permutation.

   To return a new permutation, use ``v[p]``\ . Note that this is generally faster than ``permute!(v,p)`` for large vectors.

.. function:: ipermute!(v, p)

   .. Docstring generated from Julia source

   Like permute!, but the inverse of the given permutation is applied.

.. function:: randcycle([rng,] n)

   .. Docstring generated from Julia source

   Construct a random cyclic permutation of length ``n``. The optional ``rng``
   argument specifies a random number generator, see :ref:`Random Numbers
   <random-numbers>`.

.. function:: shuffle([rng,] v)

   .. Docstring generated from Julia source

   Return a randomly permuted copy of ``v``. The optional ``rng`` argument
   specifies a random number generator, see :ref:`Random Numbers
   <random-numbers>`.

.. function:: shuffle!([rng,] v)

   .. Docstring generated from Julia source

   In-place version of :func:`shuffle`.

.. function:: reverse(v [, start=1 [, stop=length(v) ]] )

   .. Docstring generated from Julia source

   Return a copy of ``v`` reversed from start to stop.

.. function:: reverseind(v, i)

   .. Docstring generated from Julia source

   Given an index ``i`` in ``reverse(v)``\ , return the corresponding index in ``v`` so that ``v[reverseind(v,i)] == reverse(v)[i]``\ . (This can be nontrivial in the case where ``v`` is a Unicode string.)

.. function:: reverse!(v [, start=1 [, stop=length(v) ]]) -> v

   .. Docstring generated from Julia source

   In-place version of :func:`reverse`.

.. function:: combinations(array, n)

   .. Docstring generated from Julia source

   Generate all combinations of ``n`` elements from an indexable object. Because the number of combinations can be very large, this function returns an iterator object. Use ``collect(combinations(array,n))`` to get an array of all combinations.

.. function:: permutations(array)

   .. Docstring generated from Julia source

   Generate all permutations of an indexable object. Because the number of permutations can be very large, this function returns an iterator object. Use ``collect(permutations(array))`` to get an array of all permutations.

.. function:: partitions(n)

   .. Docstring generated from Julia source

   Generate all integer arrays that sum to ``n``\ . Because the number of partitions can be very large, this function returns an iterator object. Use ``collect(partitions(n))`` to get an array of all partitions. The number of partitions to generate can be efficiently computed using ``length(partitions(n))``\ .

.. function:: partitions(n, m)

   .. Docstring generated from Julia source

   Generate all arrays of ``m`` integers that sum to ``n``\ . Because the number of partitions can be very large, this function returns an iterator object. Use ``collect(partitions(n,m))`` to get an array of all partitions. The number of partitions to generate can be efficiently computed using ``length(partitions(n,m))``\ .

.. function:: partitions(array)

   .. Docstring generated from Julia source

   Generate all set partitions of the elements of an array, represented as arrays of arrays. Because the number of partitions can be very large, this function returns an iterator object. Use ``collect(partitions(array))`` to get an array of all partitions. The number of partitions to generate can be efficiently computed using ``length(partitions(array))``\ .

.. function:: partitions(array, m)

   .. Docstring generated from Julia source

   Generate all set partitions of the elements of an array into exactly m subsets, represented as arrays of arrays. Because the number of partitions can be very large, this function returns an iterator object. Use ``collect(partitions(array,m))`` to get an array of all partitions. The number of partitions into m subsets is equal to the Stirling number of the second kind and can be efficiently computed using ``length(partitions(array,m))``\ .

BitArrays
---------

.. function:: bitpack(A::AbstractArray{T,N}) -> BitArray

   .. Docstring generated from Julia source

   Converts a numeric array to a packed boolean array

.. function:: bitunpack(B::BitArray{N}) -> Array{Bool,N}

   .. Docstring generated from Julia source

   Converts a packed boolean array to an array of booleans

.. function:: flipbits!(B::BitArray{N}) -> BitArray{N}

   .. Docstring generated from Julia source

   Performs a bitwise not operation on ``B``. See :ref:`~ operator <~>`.

.. function:: rol!(dest::BitArray{1}, src::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a left rotation operation on ``src`` and put the result into ``dest``\ .

.. function:: rol!(B::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a left rotation operation on ``B``\ .

.. function:: rol(B::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a left rotation operation.

.. function:: ror!(dest::BitArray{1}, src::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a right rotation operation on ``src`` and put the result into ``dest``\ .

.. function:: ror!(B::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a right rotation operation on ``B``\ .

.. function:: ror(B::BitArray{1}, i::Integer) -> BitArray{1}

   .. Docstring generated from Julia source

   Performs a right rotation operation.

.. _stdlib-sparse:

Sparse Matrices
---------------

Sparse matrices support much of the same set of operations as dense matrices. The following functions are specific to sparse matrices.

.. function:: sparse(I,J,V,[m,n,combine])

   .. Docstring generated from Julia source

   Create a sparse matrix ``S`` of dimensions ``m x n`` such that ``S[I[k], J[k]] = V[k]``\ . The ``combine`` function is used to combine duplicates. If ``m`` and ``n`` are not specified, they are set to ``maximum(I)`` and ``maximum(J)`` respectively. If the ``combine`` function is not supplied, duplicates are added by default. All elements of ``I`` must satisfy ``1 <= I[k] <= m``\ , and all elements of ``J`` must satisfy ``1 <= J[k] <= n``\ .

.. function:: sparsevec(I, V, [m, combine])

   .. Docstring generated from Julia source

   Create a sparse matrix ``S`` of size ``m x 1`` such that ``S[I[k]] = V[k]``\ . Duplicates are combined using the ``combine`` function, which defaults to ``+`` if it is not provided. In julia, sparse vectors are really just sparse matrices with one column. Given Julia's Compressed Sparse Columns (CSC) storage format, a sparse column matrix with one column is sparse, whereas a sparse row matrix with one row ends up being dense.

.. function:: sparsevec(D::Dict, [m])

   .. Docstring generated from Julia source

   Create a sparse matrix of size ``m x 1`` where the row values are keys from the dictionary, and the nonzero values are the values from the dictionary.

.. function:: issparse(S)

   .. Docstring generated from Julia source

   Returns ``true`` if ``S`` is sparse, and ``false`` otherwise.

.. function:: sparse(A)

   .. Docstring generated from Julia source

   Convert an AbstractMatrix ``A`` into a sparse matrix.

.. function:: sparsevec(A)

   .. Docstring generated from Julia source

   Convert a dense vector ``A`` into a sparse matrix of size ``m x 1``\ . In julia, sparse vectors are really just sparse matrices with one column.

.. function:: full(S)

   .. Docstring generated from Julia source

   Convert a sparse matrix ``S`` into a dense matrix.

.. function:: nnz(A)

   .. Docstring generated from Julia source

   Returns the number of stored (filled) elements in a sparse matrix.

.. function:: spzeros(m,n)

   .. Docstring generated from Julia source

   Create a sparse matrix of size ``m x n``\ . This sparse matrix will not contain any nonzero values. No storage will be allocated for nonzero values during construction.

.. function:: spones(S)

   .. Docstring generated from Julia source

   Create a sparse matrix with the same structure as that of ``S``\ , but with every nonzero element having the value ``1.0``\ .

.. function:: speye(type,m[,n])

   .. Docstring generated from Julia source

   Create a sparse identity matrix of specified type of size ``m x m``\ . In case ``n`` is supplied, create a sparse identity matrix of size ``m x n``\ .

.. function:: spdiagm(B, d[, m, n])

   .. Docstring generated from Julia source

   Construct a sparse diagonal matrix. ``B`` is a tuple of vectors containing the diagonals and ``d`` is a tuple containing the positions of the diagonals. In the case the input contains only one diagonal, ``B`` can be a vector (instead of a tuple) and ``d`` can be the diagonal position (instead of a tuple), defaulting to 0 (diagonal). Optionally, ``m`` and ``n`` specify the size of the resulting sparse matrix.

.. function:: sprand([rng,] m,n,p [,rfn])

   .. Docstring generated from Julia source

   Create a random ``m`` by ``n`` sparse matrix, in which the probability of any
   element being nonzero is independently given by ``p`` (and hence the mean
   density of nonzeros is also exactly ``p``). Nonzero values are sampled from
   the distribution specified by ``rfn``. The uniform distribution is used in
   case ``rfn`` is not specified. The optional ``rng`` argument specifies a
   random number generator, see :ref:`Random Numbers <random-numbers>`.

.. function:: sprandn(m,n,p)

   .. Docstring generated from Julia source

   Create a random ``m`` by ``n`` sparse matrix with the specified (independent) probability ``p`` of any entry being nonzero, where nonzero values are sampled from the normal distribution.

.. function:: sprandbool(m,n,p)

   .. Docstring generated from Julia source

   Create a random ``m`` by ``n`` sparse boolean matrix with the specified (independent) probability ``p`` of any entry being ``true``\ .

.. function:: etree(A[, post])

   .. Docstring generated from Julia source

   Compute the elimination tree of a symmetric sparse matrix ``A`` from ``triu(A)`` and, optionally, its post-ordering permutation.

.. function:: symperm(A, p)

   .. Docstring generated from Julia source

   Return the symmetric permutation of ``A``\ , which is ``A[p,p]``\ . ``A`` should be symmetric and sparse, where only the upper triangular part of the matrix is stored. This algorithm ignores the lower triangular part of the matrix. Only the upper triangular part of the result is returned as well.

.. function:: nonzeros(A)

   .. Docstring generated from Julia source

   Return a vector of the structural nonzero values in sparse matrix ``A``\ . This includes zeros that are explicitly stored in the sparse matrix. The returned vector points directly to the internal nonzero storage of ``A``\ , and any modifications to the returned vector will mutate ``A`` as well. See ``rowvals(A)`` and ``nzrange(A, col)``\ .

.. function:: rowvals(A)

   .. Docstring generated from Julia source

   Return a vector of the row indices of ``A``\ , and any modifications to the returned vector will mutate ``A`` as well. Given the internal storage format of sparse matrices, providing access to how the row indices are stored internally can be useful in conjuction with iterating over structural nonzero values. See ``nonzeros(A)`` and ``nzrange(A, col)``\ .

.. function:: nzrange(A, col)

   .. Docstring generated from Julia source

   Return the range of indices to the structural nonzero values of a sparse matrix column. In conjunction with ``nonzeros(A)`` and ``rowvals(A)``\ , this allows for convenient iterating over a sparse matrix :

   .. code-block:: julia

       A = sparse(I,J,V)
       rows = rowvals(A)
       vals = nonzeros(A)
       m, n = size(A)
       for i = 1:n
          for j in nzrange(A, i)
             row = rows[j]
             val = vals[j]
             # perform sparse wizardry...
          end
       end

