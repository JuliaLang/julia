.. currentmodule:: Base

.. _stdlib-arrays:

********
 Arrays
********

Basic functions
---------------

.. function:: ndims(A::AbstractArray) -> Integer

   .. Docstring generated from Julia source

   Returns the number of dimensions of ``A``\ .

   .. doctest::

       julia> A = ones(3,4,5);

       julia> ndims(A)
       3

.. function:: size(A::AbstractArray, [dim...])

   .. Docstring generated from Julia source

   Returns a tuple containing the dimensions of ``A``\ . Optionally you can specify the dimension(s) you want the length of, and get the length of that dimension, or a tuple of the lengths of dimensions you asked for.

   .. doctest::

       julia> A = ones(2,3,4);

       julia> size(A, 2)
       3

       julia> size(A,3,2)
       (4,3)

.. function:: indices(A)

   .. Docstring generated from Julia source

   Returns the tuple of valid indices for array ``A``\ .

   .. doctest::

       julia> A = ones(5,6,7);

       julia> indices(A)
       (Base.OneTo(5),Base.OneTo(6),Base.OneTo(7))

.. function:: indices(A, d)

   .. Docstring generated from Julia source

   Returns the valid range of indices for array ``A`` along dimension ``d``\ .

   .. doctest::

       julia> A = ones(5,6,7);

       julia> indices(A,2)
       Base.OneTo(6)

.. function:: length(A::AbstractArray) -> Integer

   .. Docstring generated from Julia source

   Returns the number of elements in ``A``\ .

   .. doctest::

       julia> A = ones(3,4,5);

       julia> length(A)
       60

.. function:: eachindex(A...)

   .. Docstring generated from Julia source

   Creates an iterable object for visiting each index of an AbstractArray ``A`` in an efficient manner. For array types that have opted into fast linear indexing (like ``Array``\ ), this is simply the range ``1:length(A)``\ . For other array types, this returns a specialized Cartesian range to efficiently index into the array with indices specified for every dimension. For other iterables, including strings and dictionaries, this returns an iterator object supporting arbitrary index types (e.g. unevenly spaced or non-integer indices).

   Example for a sparse 2-d array:

   .. doctest::

       julia> A = sparse([1, 1, 2], [1, 3, 1], [1, 2, -5])
       2×3 sparse matrix with 3 Int64 nonzero entries:
               [1, 1]  =  1
               [2, 1]  =  -5
               [1, 3]  =  2

       julia> for iter in eachindex(A)
                  @show iter.I[1], iter.I[2]
                  @show A[iter]
              end
       (iter.I[1],iter.I[2]) = (1,1)
       A[iter] = 1
       (iter.I[1],iter.I[2]) = (2,1)
       A[iter] = -5
       (iter.I[1],iter.I[2]) = (1,2)
       A[iter] = 0
       (iter.I[1],iter.I[2]) = (2,2)
       A[iter] = 0
       (iter.I[1],iter.I[2]) = (1,3)
       A[iter] = 2
       (iter.I[1],iter.I[2]) = (2,3)
       A[iter] = 0

   If you supply more than one ``AbstractArray`` argument, ``eachindex`` will create an iterable object that is fast for all arguments (a ``UnitRange`` if all inputs have fast linear indexing, a ```CartesianRange`` <:obj`CartesianRange`>`_ otherwise). If the arrays have different sizes and/or dimensionalities, ``eachindex`` returns an iterable that spans the largest range along each dimension.

.. function:: linearindices(A)

   .. Docstring generated from Julia source

   Returns a ``UnitRange`` specifying the valid range of indices for ``A[i]`` where ``i`` is an ``Int``\ . For arrays with conventional indexing (indices start at 1), or any multidimensional array, this is ``1:length(A)``\ ; however, for one-dimensional arrays with unconventional indices, this is ``indices(A, 1)``\ .

   Calling this function is the "safe" way to write algorithms that exploit linear indexing.

   .. doctest::

       julia> A = ones(5,6,7);

       julia> b = linearindices(A);

       julia> extrema(b)
       (1,210)

.. function:: Base.linearindexing(A)

   .. Docstring generated from Julia source

   ``linearindexing`` defines how an AbstractArray most efficiently accesses its elements. If ``Base.linearindexing(A)`` returns ``Base.LinearFast()``\ , this means that linear indexing with only one index is an efficient operation. If it instead returns ``Base.LinearSlow()`` (by default), this means that the array intrinsically accesses its elements with indices specified for every dimension. Since converting a linear index to multiple indexing subscripts is typically very expensive, this provides a traits-based mechanism to enable efficient generic code for all array types.

   An abstract array subtype ``MyArray`` that wishes to opt into fast linear indexing behaviors should define ``linearindexing`` in the type-domain:

   .. code-block:: julia

       Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()

.. function:: countnz(A)

   .. Docstring generated from Julia source

   Counts the number of nonzero values in array ``A`` (dense or sparse). Note that this is not a constant-time operation. For sparse matrices, one should usually use :func:`nnz`\ , which returns the number of stored values.

.. function:: conj!(A)

   .. Docstring generated from Julia source

   Transform an array to its complex conjugate in-place.

   See also :func:`conj`\ .

.. function:: stride(A, k::Integer)

   .. Docstring generated from Julia source

   Returns the distance in memory (in number of elements) between adjacent elements in dimension ``k``\ .

   .. doctest::

       julia> A = ones(3,4,5);

       julia> stride(A,2)
       3

       julia> stride(A,3)
       12

.. function:: strides(A)

   .. Docstring generated from Julia source

   Returns a tuple of the memory strides in each dimension.

   .. doctest::

       julia> A = ones(3,4,5);

       julia> strides(A)
       (1,3,12)

.. function:: ind2sub(dims, index) -> subscripts

   .. Docstring generated from Julia source

   Returns a tuple of subscripts into an array with dimensions ``dims``\ , corresponding to the linear index ``index``\ .

   **Example**:

   .. code-block:: julia

       i, j, ... = ind2sub(size(A), indmax(A))

   provides the indices of the maximum element.

   .. doctest::

       julia> ind2sub((3,4),2)
       (2,1)

       julia> ind2sub((3,4),3)
       (3,1)

       julia> ind2sub((3,4),4)
       (1,2)

.. function:: ind2sub(a, index) -> subscripts

   .. Docstring generated from Julia source

   Returns a tuple of subscripts into array ``a`` corresponding to the linear index ``index``\ .

   .. doctest::

       julia> A = ones(5,6,7);

       julia> ind2sub(A,35)
       (5,1,2)

       julia> ind2sub(A,70)
       (5,2,3)

.. function:: sub2ind(dims, i, j, k...) -> index

   .. Docstring generated from Julia source

   The inverse of :func:`ind2sub`\ , returns the linear index corresponding to the provided subscripts.

   .. doctest::

       julia> sub2ind((5,6,7),1,2,3)
       66

       julia> sub2ind((5,6,7),1,6,3)
       86

.. function:: LinAlg.checksquare(A)

   .. Docstring generated from Julia source

   Check that a matrix is square, then return its common dimension. For multiple arguments, return a vector.

Constructors
------------

.. function:: Array(dims)

   .. Docstring generated from Julia source

   ``Array{T}(dims)`` constructs an uninitialized dense array with element type ``T``\ . ``dims`` may be a tuple or a series of integer arguments. The syntax ``Array(T, dims)`` is also available, but deprecated.

.. function:: getindex(type[, elements...])

   .. Docstring generated from Julia source

   Construct a 1-d array of the specified type. This is usually called with the syntax ``Type[]``\ . Element values can be specified using ``Type[a,b,c,...]``\ .

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

   Create a ``BitArray`` with all values set to ``true``\ .

.. function:: trues(A)

   .. Docstring generated from Julia source

   Create a ``BitArray`` with all values set to ``true`` of the same shape as ``A``\ .

.. function:: falses(dims)

   .. Docstring generated from Julia source

   Create a ``BitArray`` with all values set to ``false``\ .

.. function:: falses(A)

   .. Docstring generated from Julia source

   Create a ``BitArray`` with all values set to ``false`` of the same shape as ``A``\ .

.. function:: fill(x, dims)

   .. Docstring generated from Julia source

   Create an array filled with the value ``x``\ . For example, ``fill(1.0, (5,5))`` returns a 10×10 array of floats, with each element initialized to ``1.0``\ .

   .. doctest::

       julia> fill(1.0, (5,5))
       5×5 Array{Float64,2}:
        1.0  1.0  1.0  1.0  1.0
        1.0  1.0  1.0  1.0  1.0
        1.0  1.0  1.0  1.0  1.0
        1.0  1.0  1.0  1.0  1.0
        1.0  1.0  1.0  1.0  1.0

   If ``x`` is an object reference, all elements will refer to the same object. ``fill(Foo(), dims)`` will return an array filled with the result of evaluating ``Foo()`` once.

.. function:: fill!(A, x)

   .. Docstring generated from Julia source

   Fill array ``A`` with the value ``x``\ . If ``x`` is an object reference, all elements will refer to the same object. ``fill!(A, Foo())`` will return ``A`` filled with the result of evaluating ``Foo()`` once.

.. function:: reshape(A, dims)

   .. Docstring generated from Julia source

   Create an array with the same data as the given array, but with different dimensions.

.. function:: similar(array, [element_type=eltype(array)], [dims=size(array)])

   .. Docstring generated from Julia source

   Create an uninitialized mutable array with the given element type and size, based upon the given source array. The second and third arguments are both optional, defaulting to the given array's ``eltype`` and ``size``\ . The dimensions may be specified either as a single tuple argument or as a series of integer arguments.

   Custom AbstractArray subtypes may choose which specific array type is best-suited to return for the given element type and dimensionality. If they do not specialize this method, the default is an ``Array{element_type}(dims...)``\ .

   For example, ``similar(1:10, 1, 4)`` returns an uninitialized ``Array{Int,2}`` since ranges are neither mutable nor support 2 dimensions:

   .. code-block:: julia

       julia> similar(1:10, 1, 4)
       1×4 Array{Int64,2}:
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
       2×4 Array{Float64,2}:
        2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314
        2.18425e-314  2.18425e-314  2.18425e-314  2.18425e-314

.. function:: similar(storagetype, indices)

   .. Docstring generated from Julia source

   Create an uninitialized mutable array analogous to that specified by ``storagetype``\ , but with ``indices`` specified by the last argument. ``storagetype`` might be a type or a function.

   **Examples**:

   .. code-block:: julia

       similar(Array{Int}, indices(A))

   creates an array that "acts like" an ``Array{Int}`` (and might indeed be backed by one), but which is indexed identically to ``A``\ . If ``A`` has conventional indexing, this will be identical to ``Array{Int}(size(A))``\ , but if ``A`` has unconventional indexing then the indices of the result will match ``A``\ .

   .. code-block:: julia

       similar(BitArray, (indices(A, 2),))

   would create a 1-dimensional logical array whose indices match those of the columns of ``A``\ .

   .. code-block:: julia

       similar(dims->zeros(Int, dims), indices(A))

   would create an array of ``Int``\ , initialized to zero, matching the indices of ``A``\ .

.. function:: reinterpret(type, A)

   .. Docstring generated from Julia source

   Change the type-interpretation of a block of memory. For example, ``reinterpret(Float32, UInt32(7))`` interprets the 4 bytes corresponding to ``UInt32(7)`` as a ``Float32``\ . For arrays, this constructs an array with the same binary data as the given array, but with the specified element type.

.. function:: eye([T::Type=Float64,] n::Integer)

   .. Docstring generated from Julia source

   ``n``\ -by-``n`` identity matrix. The default element type is ``Float64``\ .

.. function:: eye([T::Type=Float64,] m::Integer, n::Integer)

   .. Docstring generated from Julia source

   ``m``\ -by-``n`` identity matrix. The default element type is ``Float64``\ .

.. function:: eye(A)

   .. Docstring generated from Julia source

   Constructs an identity matrix of the same dimensions and type as ``A``\ .

   .. doctest::

       julia> A = [1 2 3; 4 5 6; 7 8 9]
       3×3 Array{Int64,2}:
        1  2  3
        4  5  6
        7  8  9

       julia> eye(A)
       3×3 Array{Int64,2}:
        1  0  0
        0  1  0
        0  0  1

   Note the difference from :func:`ones`\ .

.. function:: linspace(start::Real, stop::Real, n::Real=50)

   .. Docstring generated from Julia source

   Construct a range of ``n`` linearly spaced elements from ``start`` to ``stop``\ .

   .. doctest::

       julia> linspace(1.3,2.9,9)
       9-element LinSpace{Float64}:
        1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7,2.9

.. function:: logspace(start::Real, stop::Real, n::Integer=50)

   .. Docstring generated from Julia source

   Construct a vector of ``n`` logarithmically spaced numbers from ``10^start`` to ``10^stop``\ .

   .. doctest::

       julia> logspace(1.,10.,5)
       5-element Array{Float64,1}:
          10.0
        1778.28
           3.16228e5
           5.62341e7
           1.0e10

Mathematical operators and functions
------------------------------------

All mathematical operations and functions are supported for arrays

.. function:: broadcast(f, As...)

   .. Docstring generated from Julia source

   Broadcasts the arrays ``As`` to a common size by expanding singleton dimensions, and returns an array of the results ``f(as...)`` for each position.

   .. doctest::

       julia> A = [1, 2, 3, 4, 5]
       5-element Array{Int64,1}:
        1
        2
        3
        4
        5

       julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
       5×2 Array{Int64,2}:
        1   2
        3   4
        5   6
        7   8
        9  10

       julia> broadcast(+, A, B)
       5×2 Array{Int64,2}:
         2   3
         5   6
         8   9
        11  12
        14  15

.. function:: broadcast!(f, dest, As...)

   .. Docstring generated from Julia source

   Like :func:`broadcast`\ , but store the result of ``broadcast(f, As...)`` in the ``dest`` array. Note that ``dest`` is only used to store the result, and does not supply arguments to ``f`` unless it is also listed in the ``As``\ , as in ``broadcast!(f, A, A, B)`` to perform ``A[:] = broadcast(f, A, B)``\ .

.. function:: bitbroadcast(f, As...)

   .. Docstring generated from Julia source

   Like :func:`broadcast`\ , but allocates a ``BitArray`` to store the result, rather then an ``Array``\ .

   .. doctest::

       julia> bitbroadcast(isodd,[1,2,3,4,5])
       5-element BitArray{1}:
         true
        false
         true
        false
         true

Indexing, Assignment, and Concatenation
---------------------------------------

.. function:: getindex(A, inds...)

   .. Docstring generated from Julia source

   Returns a subset of array ``A`` as specified by ``inds``\ , where each ``ind`` may be an ``Int``\ , a ``Range``\ , or a ``Vector``\ . See the manual section on :ref:`array indexing <man-array-indexing>` for details.

.. function:: view(A, inds...)

   .. Docstring generated from Julia source

   Like :func:`getindex`\ , but returns a view into the parent array ``A`` with the given indices instead of making a copy.  Calling :func:`getindex` or :func:`setindex!` on the returned :obj:`SubArray` computes the indices to the parent array on the fly without checking bounds.

.. function:: @view A[inds...]

   .. Docstring generated from Julia source

   Creates a ``SubArray`` from an indexing expression. This can only be applied directly to a reference expression (e.g. ``@view A[1,2:end]``\ ), and should *not* be used as the target of an assignment (e.g. ``@view(A[1,2:end]) = ...``\ ).

.. function:: parent(A)

   .. Docstring generated from Julia source

   Returns the "parent array" of an array view type (e.g., ``SubArray``\ ), or the array itself if it is not a view.

.. function:: parentindexes(A)

   .. Docstring generated from Julia source

   From an array view ``A``\ , returns the corresponding indexes in the parent.

.. function:: slicedim(A, d::Integer, i)

   .. Docstring generated from Julia source

   Return all the data of ``A`` where the index for dimension ``d`` equals ``i``\ . Equivalent to ``A[:,:,...,i,:,:,...]`` where ``i`` is in position ``d``\ .

   .. doctest::

       julia> A = [1 2 3 4; 5 6 7 8]
       2×4 Array{Int64,2}:
        1  2  3  4
        5  6  7  8

       julia> slicedim(A,2,3)
       2-element Array{Int64,1}:
        3
        7

.. function:: setindex!(A, X, inds...)

   .. Docstring generated from Julia source

   Store values from array ``X`` within some subset of ``A`` as specified by ``inds``\ .

.. function:: broadcast_getindex(A, inds...)

   .. Docstring generated from Julia source

   Broadcasts the ``inds`` arrays to a common size like :func:`broadcast` and returns an array of the results ``A[ks...]``\ , where ``ks`` goes over the positions in the broadcast result ``A``\ .

   .. doctest::

       julia> A = [1, 2, 3, 4, 5]
       5-element Array{Int64,1}:
        1
        2
        3
        4
        5

       julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
       5×2 Array{Int64,2}:
        1   2
        3   4
        5   6
        7   8
        9  10

       julia> C = broadcast(+,A,B)
       5×2 Array{Int64,2}:
         2   3
         5   6
         8   9
        11  12
        14  15

       julia> broadcast_getindex(C,[1,2,10])
       3-element Array{Int64,1}:
         2
         5
        15

.. function:: broadcast_setindex!(A, X, inds...)

   .. Docstring generated from Julia source

   Broadcasts the ``X`` and ``inds`` arrays to a common size and stores the value from each position in ``X`` at the indices in ``A`` given by the same positions in ``inds``\ .

.. function:: isassigned(array, i) -> Bool

   .. Docstring generated from Julia source

   Tests whether the given array has a value associated with index ``i``\ . Returns ``false`` if the index is out of bounds, or has an undefined reference.

.. function:: cat(dims, A...)

   .. Docstring generated from Julia source

   Concatenate the input arrays along the specified dimensions in the iterable ``dims``\ . For dimensions not in ``dims``\ , all input arrays should have the same size, which will also be the size of the output array along that dimension. For dimensions in ``dims``\ , the size of the output array is the sum of the sizes of the input arrays along that dimension. If ``dims`` is a single number, the different arrays are tightly stacked along that dimension. If ``dims`` is an iterable containing several dimensions, this allows one to construct block diagonal matrices and their higher-dimensional analogues by simultaneously increasing several dimensions for every new input array and putting zero blocks elsewhere. For example, ``cat([1,2], matrices...)`` builds a block diagonal matrix, i.e. a block matrix with ``matrices[1]``\ , ``matrices[2]``\ , ... as diagonal blocks and matching zero blocks away from the diagonal.

.. function:: vcat(A...)

   .. Docstring generated from Julia source

   Concatenate along dimension 1.

   .. doctest::

       julia> a = [1 2 3 4 5]
       1×5 Array{Int64,2}:
        1  2  3  4  5

       julia> b = [6 7 8 9 10; 11 12 13 14 15]
       2×5 Array{Int64,2}:
         6   7   8   9  10
        11  12  13  14  15

       julia> vcat(a,b)
       3×5 Array{Int64,2}:
         1   2   3   4   5
         6   7   8   9  10
        11  12  13  14  15

       julia> c = ([1 2 3], [4 5 6])
       (
       [1 2 3],
       <BLANKLINE>
       [4 5 6])

       julia> vcat(c...)
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

.. function:: hcat(A...)

   .. Docstring generated from Julia source

   Concatenate along dimension 2.

   .. doctest::

       julia> a = [1; 2; 3; 4; 5]
       5-element Array{Int64,1}:
        1
        2
        3
        4
        5

       julia> b = [6 7; 8 9; 10 11; 12 13; 14 15]
       5×2 Array{Int64,2}:
         6   7
         8   9
        10  11
        12  13
        14  15

       julia> hcat(a,b)
       5×3 Array{Int64,2}:
        1   6   7
        2   8   9
        3  10  11
        4  12  13
        5  14  15

       julia> c = ([1; 2; 3], [4; 5; 6])
       ([1,2,3],[4,5,6])

       julia> hcat(c...)
       3×2 Array{Int64,2}:
        1  4
        2  5
        3  6

.. function:: hvcat(rows::Tuple{Vararg{Int}}, values...)

   .. Docstring generated from Julia source

   Horizontal and vertical concatenation in one call. This function is called for block matrix syntax. The first argument specifies the number of arguments to concatenate in each block row.

   .. doctest::

       julia> a, b, c, d, e, f = 1, 2, 3, 4, 5, 6
       (1,2,3,4,5,6)

       julia> [a b c; d e f]
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> hvcat((3,3), a,b,c,d,e,f)
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> [a b;c d; e f]
       3×2 Array{Int64,2}:
        1  2
        3  4
        5  6

       julia> hvcat((2,2,2), a,b,c,d,e,f)
       3×2 Array{Int64,2}:
        1  2
        3  4
        5  6

   If the first argument is a single integer ``n``\ , then all block rows are assumed to have ``n`` block columns.

.. function:: flipdim(A, d::Integer)

   .. Docstring generated from Julia source

   Reverse ``A`` in dimension ``d``\ .

   .. doctest::

       julia> b = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> flipdim(b,2)
       2×2 Array{Int64,2}:
        2  1
        4  3

.. function:: circshift(A, shifts)

   .. Docstring generated from Julia source

   Circularly shift the data in an array. The second argument is a vector giving the amount to shift in each dimension.

   .. doctest::

       julia> b = reshape(collect(1:16), (4,4))
       4×4 Array{Int64,2}:
        1  5   9  13
        2  6  10  14
        3  7  11  15
        4  8  12  16

       julia> circshift(b, (0,2))
       4×4 Array{Int64,2}:
         9  13  1  5
        10  14  2  6
        11  15  3  7
        12  16  4  8

       julia> circshift(b, (-1,0))
       4×4 Array{Int64,2}:
        2  6  10  14
        3  7  11  15
        4  8  12  16
        1  5   9  13

   See also :func:`circshift!`\ .

.. function:: circshift!(dest, src, shifts)

   .. Docstring generated from Julia source

   Circularly shift the data in ``src``\ , storing the result in ``dest``\ . ``shifts`` specifies the amount to shift in each dimension.

   The ``dest`` array must be distinct from the ``src`` array (they cannot alias each other).

   See also ``circshift``\ .

.. function:: circcopy!(dest, src)

   .. Docstring generated from Julia source

   Copy ``src`` to ``dest``\ , indexing each dimension modulo its length. ``src`` and ``dest`` must have the same size, but can be offset in their indices; any offset results in a (circular) wraparound. If the arrays have overlapping indices, then on the domain of the overlap ``dest`` agrees with ``src``\ .

   .. code-block:: julia

       julia> src = reshape(collect(1:16), (4,4))
       4×4 Array{Int64,2}:
        1  5   9  13
        2  6  10  14
        3  7  11  15
        4  8  12  16

       julia> dest = OffsetArray{Int}((0:3,2:5))

       julia> circcopy!(dest, src)
       OffsetArrays.OffsetArray{Int64,2,Array{Int64,2}} with indices 0:3×2:5:
        8  12  16  4
        5   9  13  1
        6  10  14  2
        7  11  15  3

       julia> dest[1:3,2:4] == src[1:3,2:4]
       true

.. function:: find(A)

   .. Docstring generated from Julia source

   Return a vector of the linear indexes of the non-zeros in ``A`` (determined by ``A[i]!=0``\ ). A common use of this is to convert a boolean array to an array of indexes of the ``true`` elements. If there are no non-zero elements of ``A``\ , ``find`` returns an empty array.

   .. doctest::

       julia> A = [true false; false true]
       2×2 Array{Bool,2}:
         true  false
        false   true

       julia> find(A)
       2-element Array{Int64,1}:
        1
        4

.. function:: find(f::Function, A)

   .. Docstring generated from Julia source

   Return a vector ``I`` of the linear indexes of ``A`` where ``f(A[I])`` returns ``true``\ . If there are no such elements of ``A``\ , find returns an empty array.

   .. doctest::

       julia> A = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> find(isodd,A)
       2-element Array{Int64,1}:
        1
        2

.. function:: findn(A)

   .. Docstring generated from Julia source

   Return a vector of indexes for each dimension giving the locations of the non-zeros in ``A`` (determined by ``A[i]!=0``\ ). If there are no non-zero elements of ``A``\ , ``findn`` returns a 2-tuple of empty arrays.

   .. doctest::

       julia> A = [1 2 0; 0 0 3; 0 4 0]
       3×3 Array{Int64,2}:
        1  2  0
        0  0  3
        0  4  0

       julia> findn(A)
       ([1,1,3,2],[1,2,2,3])

       julia> A = zeros(2,2)
       2×2 Array{Float64,2}:
        0.0  0.0
        0.0  0.0

       julia> findn(A)
       (Int64[],Int64[])

.. function:: findnz(A)

   .. Docstring generated from Julia source

   Return a tuple ``(I, J, V)`` where ``I`` and ``J`` are the row and column indexes of the non-zero values in matrix ``A``\ , and ``V`` is a vector of the non-zero values.

   .. doctest::

       julia> A = [1 2 0; 0 0 3; 0 4 0]
       3×3 Array{Int64,2}:
        1  2  0
        0  0  3
        0  4  0

       julia> findnz(A)
       ([1,1,3,2],[1,2,2,3],[1,2,4,3])

.. function:: findfirst(A)

   .. Docstring generated from Julia source

   Return the linear index of the first non-zero value in ``A`` (determined by ``A[i]!=0``\ ). Returns ``0`` if no such value is found.

   .. doctest::

       julia> A = [0 0; 1 0]
       2×2 Array{Int64,2}:
        0  0
        1  0

       julia> findfirst(A)
       2

.. function:: findfirst(A, v)

   .. Docstring generated from Julia source

   Return the linear index of the first element equal to ``v`` in ``A``\ . Returns ``0`` if ``v`` is not found.

   .. doctest::

       julia> A = [4 6; 2 2]
       2×2 Array{Int64,2}:
        4  6
        2  2

       julia> findfirst(A,2)
       2

       julia> findfirst(A,3)
       0

.. function:: findfirst(predicate::Function, A)

   .. Docstring generated from Julia source

   Return the linear index of the first element of ``A`` for which ``predicate`` returns ``true``\ . Returns ``0`` if there is no such element.

   .. doctest::

       julia> A = [1 4; 2 2]
       2×2 Array{Int64,2}:
        1  4
        2  2

       julia> findfirst(iseven, A)
       2

       julia> findfirst(x -> x>10, A)
       0

.. function:: findlast(A)

   .. Docstring generated from Julia source

   Return the linear index of the last non-zero value in ``A`` (determined by ``A[i]!=0``\ ). Returns ``0`` if there is no non-zero value in ``A``\ .

   .. doctest::

       julia> A = [1 0; 1 0]
       2×2 Array{Int64,2}:
        1  0
        1  0

       julia> findlast(A)
       2

       julia> A = zeros(2,2)
       2×2 Array{Float64,2}:
        0.0  0.0
        0.0  0.0

       julia> findlast(A)
       0

.. function:: findlast(A, v)

   .. Docstring generated from Julia source

   Return the linear index of the last element equal to ``v`` in ``A``\ . Returns ``0`` if there is no element of ``A`` equal to ``v``\ .

   .. doctest::

       julia> A = [1 2; 2 1]
       2×2 Array{Int64,2}:
        1  2
        2  1

       julia> findlast(A,1)
       4

       julia> findlast(A,2)
       3

       julia> findlast(A,3)
       0

.. function:: findlast(predicate::Function, A)

   .. Docstring generated from Julia source

   Return the linear index of the last element of ``A`` for which ``predicate`` returns ``true``\ . Returns ``0`` if there is no such element.

   .. doctest::

       julia> A = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> findlast(isodd, A)
       2

       julia> findlast(x -> x > 5, A)
       0

.. function:: findnext(A, i::Integer)

   .. Docstring generated from Julia source

   Find the next linear index >= ``i`` of a non-zero element of ``A``\ , or ``0`` if not found.

   .. doctest::

       julia> A = [0 0; 1 0]
       2×2 Array{Int64,2}:
        0  0
        1  0

       julia> findnext(A,1)
       2

       julia> findnext(A,3)
       0

.. function:: findnext(predicate::Function, A, i::Integer)

   .. Docstring generated from Julia source

   Find the next linear index >= ``i`` of an element of ``A`` for which ``predicate`` returns ``true``\ , or ``0`` if not found.

   .. doctest::

       julia> A = [1 4; 2 2]
       2×2 Array{Int64,2}:
        1  4
        2  2

       julia> findnext(isodd, A, 1)
       1

       julia> findnext(isodd, A, 2)
       0

.. function:: findnext(A, v, i::Integer)

   .. Docstring generated from Julia source

   Find the next linear index >= ``i`` of an element of ``A`` equal to ``v`` (using ``==``\ ), or ``0`` if not found.

   .. doctest::

       julia> A = [1 4; 2 2]
       2×2 Array{Int64,2}:
        1  4
        2  2

       julia> findnext(A,4,4)
       0

       julia> findnext(A,4,3)
       3

.. function:: findprev(A, i::Integer)

   .. Docstring generated from Julia source

   Find the previous linear index <= ``i`` of a non-zero element of ``A``\ , or ``0`` if not found.

   .. doctest::

       julia> A = [0 0; 1 2]
       2×2 Array{Int64,2}:
        0  0
        1  2

       julia> findprev(A,2)
       2

       julia> findprev(A,1)
       0

.. function:: findprev(predicate::Function, A, i::Integer)

   .. Docstring generated from Julia source

   Find the previous linear index <= ``i`` of an element of ``A`` for which ``predicate`` returns ``true``\ , or ``0`` if not found.

   .. doctest::

       julia> A = [4 6; 1 2]
       2×2 Array{Int64,2}:
        4  6
        1  2

       julia> findprev(isodd, A, 1)
       0

       julia> findprev(isodd, A, 3)
       2

.. function:: findprev(A, v, i::Integer)

   .. Docstring generated from Julia source

   Find the previous linear index <= ``i`` of an element of ``A`` equal to ``v`` (using ``==``\ ), or ``0`` if not found.

   .. doctest::

       julia> A = [0 0; 1 2]
       2×2 Array{Int64,2}:
        0  0
        1  2

       julia> findprev(A, 1, 4)
       2

       julia> findprev(A, 1, 1)
       0

.. function:: permutedims(A, perm)

   .. Docstring generated from Julia source

   Permute the dimensions of array ``A``\ . ``perm`` is a vector specifying a permutation of length ``ndims(A)``\ . This is a generalization of transpose for multi-dimensional arrays. Transpose is equivalent to ``permutedims(A, [2,1])``\ .

.. function:: ipermutedims(A, perm)

   .. Docstring generated from Julia source

   Like :func:`permutedims`\ , except the inverse of the given permutation is applied.

.. function:: permutedims!(dest, src, perm)

   .. Docstring generated from Julia source

   Permute the dimensions of array ``src`` and store the result in the array ``dest``\ . ``perm`` is a vector specifying a permutation of length ``ndims(src)``\ . The preallocated array ``dest`` should have ``size(dest) == size(src)[perm]`` and is completely overwritten. No in-place permutation is supported and unexpected results will happen if ``src`` and ``dest`` have overlapping memory regions.

.. function:: squeeze(A, dims)

   .. Docstring generated from Julia source

   Remove the dimensions specified by ``dims`` from array ``A``\ . Elements of ``dims`` must be unique and within the range ``1:ndims(A)``\ . ``size(A,i)`` must equal 1 for all ``i`` in ``dims``\ .

   .. doctest::

       julia> a = reshape(collect(1:4),(2,2,1,1))
       2×2×1×1 Array{Int64,4}:
       [:, :, 1, 1] =
        1  3
        2  4

       julia> squeeze(a,3)
       2×2×1 Array{Int64,3}:
       [:, :, 1] =
        1  3
        2  4

.. function:: vec(a::AbstractArray) -> Vector

   .. Docstring generated from Julia source

   Reshape array ``a`` as a one-dimensional column vector.

   .. doctest::

       julia> a = [1 2 3; 4 5 6]
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> vec(a)
       6-element Array{Int64,1}:
        1
        4
        2
        5
        3
        6

.. function:: promote_shape(s1, s2)

   .. Docstring generated from Julia source

   Check two array shapes for compatibility, allowing trailing singleton dimensions, and return whichever shape has more dimensions.

   .. doctest::

       julia> a = ones(3,4,1,1,1);

       julia> b = ones(3,4);

       julia> promote_shape(a,b)
       (Base.OneTo(3),Base.OneTo(4),Base.OneTo(1),Base.OneTo(1),Base.OneTo(1))

       julia> promote_shape((2,3,1,4), (2,3,1,4,1))
       (2,3,1,4,1)

.. function:: checkbounds(A, I...)

   .. Docstring generated from Julia source

   Throw an error if the specified indices ``I`` are not in bounds for the given array ``A``\ .

.. function:: checkbounds(Bool, A, I...)

   .. Docstring generated from Julia source

   Return ``true`` if the specified indices ``I`` are in bounds for the given array ``A``\ . Subtypes of ``AbstractArray`` should specialize this method if they need to provide custom bounds checking behaviors; however, in many cases one can rely on ``A``\ 's indices and :func:`checkindex`\ .

   See also :func:`checkindex`\ .

.. function:: checkindex(Bool, inds::AbstractUnitRange, index)

   .. Docstring generated from Julia source

   Return ``true`` if the given ``index`` is within the bounds of ``inds``\ . Custom types that would like to behave as indices for all arrays can extend this method in order to provide a specialized bounds checking implementation.

   .. doctest::

       julia> checkindex(Bool,1:20,8)
       true

       julia> checkindex(Bool,1:20,21)
       false

.. function:: randsubseq(A, p) -> Vector

   .. Docstring generated from Julia source

   Return a vector consisting of a random subsequence of the given array ``A``\ , where each element of ``A`` is included (in order) with independent probability ``p``\ . (Complexity is linear in ``p*length(A)``\ , so this function is efficient even if ``p`` is small and ``A`` is large.) Technically, this process is known as "Bernoulli sampling" of ``A``\ .

.. function:: randsubseq!(S, A, p)

   .. Docstring generated from Julia source

   Like :func:`randsubseq`\ , but the results are stored in ``S`` (which is resized as needed).

Array functions
---------------

.. function:: cumprod(A, dim=1)

   .. Docstring generated from Julia source

   Cumulative product along a dimension ``dim`` (defaults to 1). See also :func:`cumprod!` to use a preallocated output array, both for performance and to control the precision of the output (e.g. to avoid overflow).

   .. doctest::

       julia> a = [1 2 3; 4 5 6]
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> cumprod(a,1)
       2×3 Array{Int64,2}:
        1   2   3
        4  10  18

       julia> cumprod(a,2)
       2×3 Array{Int64,2}:
        1   2    6
        4  20  120

.. function:: cumprod!(B, A, [dim])

   .. Docstring generated from Julia source

   Cumulative product of ``A`` along a dimension, storing the result in ``B``\ . The dimension defaults to 1.

.. function:: cumsum(A, dim=1)

   .. Docstring generated from Julia source

   Cumulative sum along a dimension ``dim`` (defaults to 1). See also :func:`cumsum!` to use a preallocated output array, both for performance and to control the precision of the output (e.g. to avoid overflow).

   .. doctest::

       julia> a = [1 2 3; 4 5 6]
       2×3 Array{Int64,2}:
        1  2  3
        4  5  6

       julia> cumsum(a,1)
       2×3 Array{Int64,2}:
        1  2  3
        5  7  9

       julia> cumsum(a,2)
       2×3 Array{Int64,2}:
        1  3   6
        4  9  15

.. function:: cumsum!(B, A, [dim])

   .. Docstring generated from Julia source

   Cumulative sum of ``A`` along a dimension, storing the result in ``B``\ . The dimension defaults to 1.

.. function:: cumsum_kbn(A, [dim::Integer=1])

   .. Docstring generated from Julia source

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy. The dimension defaults to 1.

.. function:: cummin(A, [dim])

   .. Docstring generated from Julia source

   Cumulative minimum along a dimension. The dimension defaults to 1.

.. function:: cummax(A, [dim])

   .. Docstring generated from Julia source

   Cumulative maximum along a dimension. The dimension defaults to 1.

.. function:: diff(A, [dim::Integer=1])

   .. Docstring generated from Julia source

   Finite difference operator of matrix or vector ``A``\ . If ``A`` is a matrix, compute the finite difference over a dimension ``dim`` (default ``1``\ ).

   .. doctest::

       julia> a = [2 4; 6 16]
       2×2 Array{Int64,2}:
        2   4
        6  16

       julia> diff(a,2)
       2×1 Array{Int64,2}:
         2
        10

.. function:: gradient(F::AbstractVector, [h::Real])

   .. Docstring generated from Julia source

   Compute differences along vector ``F``\ , using ``h`` as the spacing between points. The default spacing is one.

   .. doctest::

       julia> a = [2,4,6,8];

       julia> gradient(a)
       4-element Array{Float64,1}:
        2.0
        2.0
        2.0
        2.0

.. function:: rot180(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` 180 degrees.

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rot180(a)
       2×2 Array{Int64,2}:
        4  3
        2  1

.. function:: rot180(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` 180 degrees an integer ``k`` number of times. If ``k`` is even, this is equivalent to a ``copy``\ .

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rot180(a,1)
       2×2 Array{Int64,2}:
        4  3
        2  1

       julia> rot180(a,2)
       2×2 Array{Int64,2}:
        1  2
        3  4

.. function:: rotl90(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` left 90 degrees.

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rotl90(a)
       2×2 Array{Int64,2}:
        2  4
        1  3

.. function:: rotl90(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` left 90 degrees an integer ``k`` number of times. If ``k`` is zero or a multiple of four, this is equivalent to a ``copy``\ .

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rotl90(a,1)
       2×2 Array{Int64,2}:
        2  4
        1  3

       julia> rotl90(a,2)
       2×2 Array{Int64,2}:
        4  3
        2  1

       julia> rotl90(a,3)
       2×2 Array{Int64,2}:
        3  1
        4  2

       julia> rotl90(a,4)
       2×2 Array{Int64,2}:
        1  2
        3  4

.. function:: rotr90(A)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` right 90 degrees.

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rotr90(a)
       2×2 Array{Int64,2}:
        3  1
        4  2

.. function:: rotr90(A, k)

   .. Docstring generated from Julia source

   Rotate matrix ``A`` right 90 degrees an integer ``k`` number of times. If ``k`` is zero or a multiple of four, this is equivalent to a ``copy``\ .

   .. doctest::

       julia> a = [1 2; 3 4]
       2×2 Array{Int64,2}:
        1  2
        3  4

       julia> rotr90(a,1)
       2×2 Array{Int64,2}:
        3  1
        4  2

       julia> rotr90(a,2)
       2×2 Array{Int64,2}:
        4  3
        2  1

       julia> rotr90(a,3)
       2×2 Array{Int64,2}:
        2  4
        1  3

       julia> rotr90(a,4)
       2×2 Array{Int64,2}:
        1  2
        3  4

.. function:: reducedim(f, A, region[, v0])

   .. Docstring generated from Julia source

   Reduce 2-argument function ``f`` along dimensions of ``A``\ . ``region`` is a vector specifying the dimensions to reduce, and ``v0`` is the initial value to use in the reductions. For ``+``\ , ``*``\ , ``max`` and ``min`` the ``v0`` argument is optional.

   The associativity of the reduction is implementation-dependent; if you need a particular associativity, e.g. left-to-right, you should write your own loop. See documentation for :func:`reduce`\ .

   .. doctest::

       julia> a = reshape(collect(1:16), (4,4))
       4×4 Array{Int64,2}:
        1  5   9  13
        2  6  10  14
        3  7  11  15
        4  8  12  16

       julia> reducedim(max, a, 2)
       4×1 Array{Int64,2}:
        13
        14
        15
        16

       julia> reducedim(max, a, 1)
       1×4 Array{Int64,2}:
        4  8  12  16

.. function:: mapreducedim(f, op, A, region[, v0])

   .. Docstring generated from Julia source

   Evaluates to the same as ``reducedim(op, map(f, A), region, f(v0))``\ , but is generally faster because the intermediate array is avoided.

   .. doctest::

       julia> a = reshape(collect(1:16), (4,4))
       4×4 Array{Int64,2}:
        1  5   9  13
        2  6  10  14
        3  7  11  15
        4  8  12  16

       julia> mapreducedim(isodd, *, a, 1)
       1×4 Array{Bool,2}:
        false  false  false  false

       julia> mapreducedim(isodd, |, a, 1, true)
       1×4 Array{Bool,2}:
        true  true  true  true

.. function:: mapslices(f, A, dims)

   .. Docstring generated from Julia source

   Transform the given dimensions of array ``A`` using function ``f``\ . ``f`` is called on each slice of ``A`` of the form ``A[...,:,...,:,...]``\ . ``dims`` is an integer vector specifying where the colons go in this expression. The results are concatenated along the remaining dimensions. For example, if ``dims`` is ``[1,2]`` and ``A`` is 4-dimensional, ``f`` is called on ``A[:,:,i,j]`` for all ``i`` and ``j``\ .

   .. doctest::

       julia> a = reshape(collect(1:16),(2,2,2,2))
       2×2×2×2 Array{Int64,4}:
       [:, :, 1, 1] =
        1  3
        2  4
       <BLANKLINE>
       [:, :, 2, 1] =
        5  7
        6  8
       <BLANKLINE>
       [:, :, 1, 2] =
         9  11
        10  12
       <BLANKLINE>
       [:, :, 2, 2] =
        13  15
        14  16

       julia> mapslices(sum, a, [1,2])
       1×1×2×2 Array{Int64,4}:
       [:, :, 1, 1] =
        10
       <BLANKLINE>
       [:, :, 2, 1] =
        26
       <BLANKLINE>
       [:, :, 1, 2] =
        42
       <BLANKLINE>
       [:, :, 2, 2] =
        58

.. function:: sum_kbn(A)

   .. Docstring generated from Julia source

   Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy.

Combinatorics
-------------

.. function:: randperm([rng=GLOBAL_RNG,] n::Integer)

   .. Docstring generated from Julia source

   Construct a random permutation of length ``n``\ . The optional ``rng`` argument specifies a random number generator (see :ref:`Random Numbers <random-numbers>`\ ). To randomly permute a arbitrary vector, see :func:`shuffle` or :func:`shuffle!`\ .

.. function:: invperm(v)

   .. Docstring generated from Julia source

   Return the inverse permutation of ``v``

.. function:: isperm(v) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if ``v`` is a valid permutation.

.. function:: permute!(v, p)

   .. Docstring generated from Julia source

   Permute vector ``v`` in-place, according to permutation ``p``\ . No checking is done to verify that ``p`` is a permutation.

   To return a new permutation, use ``v[p]``\ . Note that this is generally faster than ``permute!(v,p)`` for large vectors.

.. function:: ipermute!(v, p)

   .. Docstring generated from Julia source

   Like ``permute!``\ , but the inverse of the given permutation is applied.

.. function:: randcycle([rng=GLOBAL_RNG,] n::Integer)

   .. Docstring generated from Julia source

   Construct a random cyclic permutation of length ``n``\ . The optional ``rng`` argument specifies a random number generator, see :ref:`Random Numbers <random-numbers>`\ .

.. function:: shuffle([rng=GLOBAL_RNG,] v)

   .. Docstring generated from Julia source

   Return a randomly permuted copy of ``v``\ . The optional ``rng`` argument specifies a random number generator (see :ref:`Random Numbers <random-numbers>`\ ). To permute ``v`` in-place, see :func:`shuffle!`\ .  To obtain randomly permuted indices, see :func:`randperm`\ .

.. function:: shuffle!([rng=GLOBAL_RNG,] v)

   .. Docstring generated from Julia source

   In-place version of :func:`shuffle`\ : randomly permute the array ``v`` in-place, optionally supplying the random-number generator ``rng``\ .

.. function:: reverse(v [, start=1 [, stop=length(v) ]] )

   .. Docstring generated from Julia source

   Return a copy of ``v`` reversed from start to stop.

.. function:: reverseind(v, i)

   .. Docstring generated from Julia source

   Given an index ``i`` in ``reverse(v)``\ , return the corresponding index in ``v`` so that ``v[reverseind(v,i)] == reverse(v)[i]``\ . (This can be nontrivial in the case where ``v`` is a Unicode string.)

.. function:: reverse!(v [, start=1 [, stop=length(v) ]]) -> v

   .. Docstring generated from Julia source

   In-place version of :func:`reverse`\ .

BitArrays
---------

BitArrays are space-efficient "packed" boolean arrays, which store
one bit per boolean value.  They can be used similarly to ``Array{Bool}``
arrays (which store one byte per boolean value), and can be converted
to/from the latter via ``Array(bitarray)`` and ``BitArray(array)``, respectively.

.. function:: flipbits!(B::BitArray{N}) -> BitArray{N}

   .. Docstring generated from Julia source

   Performs a bitwise not operation on ``B``\ . See :ref:`~ operator <~>`\ .

   .. doctest::

       julia> A = trues(2,2)
       2×2 BitArray{2}:
        true  true
        true  true

       julia> flipbits!(A)
       2×2 BitArray{2}:
        false  false
        false  false

.. function:: rol!(dest::BitVector, src::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a left rotation operation on ``src`` and puts the result into ``dest``\ . ``i`` controls how far to rotate the bits.

.. function:: rol!(B::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a left rotation operation in-place on ``B``\ . ``i`` controls how far to rotate the bits.

.. function:: rol(B::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a left rotation operation, returning a new ``BitVector``\ . ``i`` controls how far to rotate the bits. See also :func:`rol!`\ .

   .. doctest::

       julia> A = BitArray([true, true, false, false, true])
       5-element BitArray{1}:
         true
         true
        false
        false
         true

       julia> rol(A,1)
       5-element BitArray{1}:
         true
        false
        false
         true
         true

       julia> rol(A,2)
       5-element BitArray{1}:
        false
        false
         true
         true
         true

       julia> rol(A,5)
       5-element BitArray{1}:
         true
         true
        false
        false
         true

.. function:: ror!(dest::BitVector, src::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a right rotation operation on ``src`` and puts the result into ``dest``\ . ``i`` controls how far to rotate the bits.

.. function:: ror!(B::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a right rotation operation in-place on ``B``\ . ``i`` controls how far to rotate the bits.

.. function:: ror(B::BitVector, i::Integer) -> BitVector

   .. Docstring generated from Julia source

   Performs a right rotation operation on ``B``\ , returning a new ``BitVector``\ . ``i`` controls how far to rotate the bits. See also :func:`ror!`\ .

   .. doctest::

       julia> A = BitArray([true, true, false, false, true])
       5-element BitArray{1}:
         true
         true
        false
        false
         true

       julia> ror(A,1)
       5-element BitArray{1}:
         true
         true
         true
        false
        false

       julia> ror(A,2)
       5-element BitArray{1}:
        false
         true
         true
         true
        false

       julia> ror(A,5)
       5-element BitArray{1}:
         true
         true
        false
        false
         true

.. _stdlib-sparse:

Sparse Vectors and Matrices
---------------------------

Sparse vectors and matrices largely support the same set of operations as their
dense counterparts. The following functions are specific to sparse arrays.

.. function:: sparse(I, J, V,[ m, n, combine])

   .. Docstring generated from Julia source

   Create a sparse matrix ``S`` of dimensions ``m x n`` such that ``S[I[k], J[k]] = V[k]``\ . The ``combine`` function is used to combine duplicates. If ``m`` and ``n`` are not specified, they are set to ``maximum(I)`` and ``maximum(J)`` respectively. If the ``combine`` function is not supplied, ``combine`` defaults to ``+`` unless the elements of ``V`` are Booleans in which case ``combine`` defaults to ``|``\ . All elements of ``I`` must satisfy ``1 <= I[k] <= m``\ , and all elements of ``J`` must satisfy ``1 <= J[k] <= n``\ . Numerical zeros in (``I``\ , ``J``\ , ``V``\ ) are retained as structural nonzeros; to drop numerical zeros, use :func:`dropzeros!`\ .

   For additional documentation and an expert driver, see ``Base.SparseArrays.sparse!``\ .

.. function:: sparsevec(I, V, [m, combine])

   .. Docstring generated from Julia source

   Create a sparse vector ``S`` of length ``m`` such that ``S[I[k]] = V[k]``\ . Duplicates are combined using the ``combine`` function, which defaults to ``+`` if no ``combine`` argument is provided, unless the elements of ``V`` are Booleans in which case ``combine`` defaults to ``|``\ .

.. function:: sparsevec(D::Dict, [m])

   .. Docstring generated from Julia source

   Create a sparse vector of length ``m`` where the nonzero indices are keys from the dictionary, and the nonzero values are the values from the dictionary.

.. function:: issparse(S)

   .. Docstring generated from Julia source

   Returns ``true`` if ``S`` is sparse, and ``false`` otherwise.

.. function:: sparse(A)

   .. Docstring generated from Julia source

   Convert an AbstractMatrix ``A`` into a sparse matrix.

.. function:: sparsevec(A)

   .. Docstring generated from Julia source

   Convert a vector ``A`` into a sparse vector of length ``m``\ .

.. function:: full(S)

   .. Docstring generated from Julia source

   Convert a sparse matrix or vector ``S`` into a dense matrix or vector.

.. function:: nnz(A)

   .. Docstring generated from Julia source

   Returns the number of stored (filled) elements in a sparse array.

.. function:: spzeros([type,]m[,n])

   .. Docstring generated from Julia source

   Create a sparse vector of length ``m`` or sparse matrix of size ``m x n``\ . This sparse array will not contain any nonzero values. No storage will be allocated for nonzero values during construction. The type defaults to ``Float64`` if not specified.

.. function:: spones(S)

   .. Docstring generated from Julia source

   Create a sparse array with the same structure as that of ``S``\ , but with every nonzero element having the value ``1.0``\ .

   .. doctest::

       julia> A = sparse([1,2,3,4],[2,4,3,1],[5.,4.,3.,2.])
       4×4 sparse matrix with 4 Float64 nonzero entries:
               [4, 1]  =  2.0
               [1, 2]  =  5.0
               [3, 3]  =  3.0
               [2, 4]  =  4.0

       julia> spones(A)
       4×4 sparse matrix with 4 Float64 nonzero entries:
               [4, 1]  =  1.0
               [1, 2]  =  1.0
               [3, 3]  =  1.0
               [2, 4]  =  1.0

   Note the difference from :func:`speye`\ .

.. function:: speye([type,]m[,n])

   .. Docstring generated from Julia source

   Create a sparse identity matrix of size ``m x m``\ . When ``n`` is supplied, create a sparse identity matrix of size ``m x n``\ . The type defaults to ``Float64`` if not specified.

.. function:: speye(S)

   .. Docstring generated from Julia source

   Create a sparse identity matrix with the same size as ``S``\ .

   .. doctest::

       julia> A = sparse([1,2,3,4],[2,4,3,1],[5.,4.,3.,2.])
       4×4 sparse matrix with 4 Float64 nonzero entries:
               [4, 1]  =  2.0
               [1, 2]  =  5.0
               [3, 3]  =  3.0
               [2, 4]  =  4.0

       julia> speye(A)
       4×4 sparse matrix with 4 Float64 nonzero entries:
               [1, 1]  =  1.0
               [2, 2]  =  1.0
               [3, 3]  =  1.0
               [4, 4]  =  1.0

   Note the difference from :func:`spones`\ .

.. function:: spdiagm(B, d[, m, n])

   .. Docstring generated from Julia source

   Construct a sparse diagonal matrix. ``B`` is a tuple of vectors containing the diagonals and ``d`` is a tuple containing the positions of the diagonals. In the case the input contains only one diagonal, ``B`` can be a vector (instead of a tuple) and ``d`` can be the diagonal position (instead of a tuple), defaulting to 0 (diagonal). Optionally, ``m`` and ``n`` specify the size of the resulting sparse matrix.

   .. doctest::

       julia> spdiagm(([1,2,3,4],[4,3,2,1]),(-1,1))
       5×5 sparse matrix with 8 Int64 nonzero entries:
               [2, 1]  =  1
               [1, 2]  =  4
               [3, 2]  =  2
               [2, 3]  =  3
               [4, 3]  =  3
               [3, 4]  =  2
               [5, 4]  =  4
               [4, 5]  =  1

.. function:: sprand([rng],[type],m,[n],p::AbstractFloat,[rfn])

   .. Docstring generated from Julia source

   Create a random length ``m`` sparse vector or ``m`` by ``n`` sparse matrix, in which the probability of any element being nonzero is independently given by ``p`` (and hence the mean density of nonzeros is also exactly ``p``\ ). Nonzero values are sampled from the distribution specified by ``rfn`` and have the type ``type``\ . The uniform distribution is used in case ``rfn`` is not specified. The optional ``rng`` argument specifies a random number generator, see :ref:`Random Numbers <random-numbers>`\ .

.. function:: sprandn([rng], m[,n],p::AbstractFloat)

   .. Docstring generated from Julia source

   Create a random sparse vector of length ``m`` or sparse matrix of size ``m`` by ``n`` with the specified (independent) probability ``p`` of any entry being nonzero, where nonzero values are sampled from the normal distribution. The optional ``rng`` argument specifies a random number generator, see :ref:`Random Numbers <random-numbers>`\ .

.. function:: nonzeros(A)

   .. Docstring generated from Julia source

   Return a vector of the structural nonzero values in sparse array ``A``\ . This includes zeros that are explicitly stored in the sparse array. The returned vector points directly to the internal nonzero storage of ``A``\ , and any modifications to the returned vector will mutate ``A`` as well. See :func:`rowvals` and :func:`nzrange`\ .

.. function:: rowvals(A::SparseMatrixCSC)

   .. Docstring generated from Julia source

   Return a vector of the row indices of ``A``\ . Any modifications to the returned vector will mutate ``A`` as well. Providing access to how the row indices are stored internally can be useful in conjunction with iterating over structural nonzero values. See also :func:`nonzeros` and :func:`nzrange`\ .

.. function:: nzrange(A::SparseMatrixCSC, col)

   .. Docstring generated from Julia source

   Return the range of indices to the structural nonzero values of a sparse matrix column. In conjunction with :func:`nonzeros` and :func:`rowvals`\ , this allows for convenient iterating over a sparse matrix :

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

.. function:: dropzeros!(A::SparseMatrixCSC, trim::Bool = true)

   .. Docstring generated from Julia source

   Removes stored numerical zeros from ``A``\ , optionally trimming resulting excess space from ``A.rowval`` and ``A.nzval`` when ``trim`` is ``true``\ .

   For an out-of-place version, see :func:`dropzeros`\ . For algorithmic information, see :func:`Base.SparseArrays.fkeep!`\ .

.. function:: dropzeros(A::SparseMatrixCSC, trim::Bool = true)

   .. Docstring generated from Julia source

   Generates a copy of ``A`` and removes stored numerical zeros from that copy, optionally trimming excess space from the result's ``rowval`` and ``nzval`` arrays when ``trim`` is ``true``\ .

   For an in-place version and algorithmic information, see :func:`dropzeros!`\ .

.. function:: dropzeros!(x::SparseVector, trim::Bool = true)

   .. Docstring generated from Julia source

   Removes stored numerical zeros from ``x``\ , optionally trimming resulting excess space from ``x.nzind`` and ``x.nzval`` when ``trim`` is ``true``\ .

   For an out-of-place version, see :func:`Base.SparseArrays.dropzeros`\ . For algorithmic information, see :func:`Base.SparseArrays.fkeep!`\ .

.. function:: dropzeros(x::SparseVector, trim::Bool = true)

   .. Docstring generated from Julia source

   Generates a copy of ``x`` and removes numerical zeros from that copy, optionally trimming excess space from the result's ``nzind`` and ``nzval`` arrays when ``trim`` is ``true``\ .

   For an in-place version and algorithmic information, see :func:`Base.SparseArrays.dropzeros!`\ .

.. function:: permute{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp},
                  q::AbstractVector{Tq})

   .. Docstring generated from Julia source

   Bilaterally permute ``A``\ , returning ``PAQ`` (``A[p,q]``\ ). Column-permutation ``q``\ 's length must match ``A``\ 's column count (``length(q) == A.n``\ ). Row-permutation ``p``\ 's length must match ``A``\ 's row count (``length(p) == A.m``\ ).

   For expert drivers and additional information, see :func:`Base.SparseArrays.permute!`\ .

.. function:: permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti},
                  p::AbstractVector{Tp}, q::AbstractVector{Tq}[, C::SparseMatrixCSC{Tv,Ti}])

   .. Docstring generated from Julia source

   Bilaterally permute ``A``\ , storing result ``PAQ`` (``A[p,q]``\ ) in ``X``\ . Stores intermediate result ``(AQ)^T`` (``transpose(A[:,q])``\ ) in optional argument ``C`` if present. Requires that none of ``X``\ , ``A``\ , and, if present, ``C`` alias each other; to store result ``PAQ`` back into ``A``\ , use the following method lacking ``X``\ :

   .. code-block:: julia

       permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp},
           q::AbstractVector{Tq}[, C::SparseMatrixCSC{Tv,Ti}[, workcolptr::Vector{Ti}]])

   ``X``\ 's dimensions must match those of ``A`` (``X.m == A.m`` and ``X.n == A.n``\ ), and ``X`` must have enough storage to accommodate all allocated entries in ``A`` (``length(X.rowval) >= nnz(A)`` and ``length(X.nzval) >= nnz(A)``\ ). Column-permutation ``q``\ 's length must match ``A``\ 's column count (``length(q) == A.n``\ ). Row-permutation ``p``\ 's length must match ``A``\ 's row count (``length(p) == A.m``\ ).

   ``C``\ 's dimensions must match those of ``transpose(A)`` (``C.m == A.n`` and ``C.n == A.m``\ ), and ``C`` must have enough storage to accommodate all allocated entries in ``A`` (``length(C.rowval)`` >= nnz(A)``and``\ length(C.nzval) >= nnz(A)`).

   For additional (algorithmic) information, and for versions of these methods that forgo argument checking, see (unexported) parent methods :func:`Base.SparseArrays.unchecked_noalias_permute!` and :func:`Base.SparseArrays.unchecked_aliasing_permute!`\ .

   See also: :func:`Base.SparseArrays.permute`

