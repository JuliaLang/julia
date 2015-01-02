.. _man-arrays:

.. currentmodule:: Base

**************************
 Multi-dimensional Arrays
**************************

Julia, like most technical computing languages, provides a first-class
array implementation. Most technical computing languages pay a lot of
attention to their array implementation at the expense of other
containers. Julia does not treat arrays in any special way. The array
library is implemented almost completely in Julia itself, and derives
its performance from the compiler, just like any other code written in
Julia.

An array is a collection of objects stored in a multi-dimensional
grid.  In the most general case, an array may contain objects of type
``Any``.  For most computational purposes, arrays should contain
objects of a more specific type, such as ``Float64`` or ``Int32``.

In general, unlike many other technical computing languages, Julia does
not expect programs to be written in a vectorized style for performance.
Julia's compiler uses type inference and generates optimized code for
scalar array indexing, allowing programs to be written in a style that
is convenient and readable, without sacrificing performance, and using
less memory at times.

In Julia, all arguments to functions are passed by reference. Some
technical computing languages pass arrays by value, and this is
convenient in many cases. In Julia, modifications made to input arrays
within a function will be visible in the parent function. The entire
Julia array library ensures that inputs are not modified by library
functions. User code, if it needs to exhibit similar behavior, should
take care to create a copy of inputs that it may modify.

Arrays
======

Basic Functions
---------------

============================ ==============================================================================
Function                     Description
============================ ==============================================================================
:func:`eltype(A) <eltype>`   the type of the elements contained in A
:func:`length(A) <length>`   the number of elements in A
:func:`ndims(A) <ndims>`     the number of dimensions of A
:func:`size(A) <size>`       a tuple containing the dimensions of A
:func:`size(A,n) <size>`     the size of A in a particular dimension
:func:`stride(A,k) <stride>` the stride (linear index distance between adjacent elements) along dimension k
:func:`strides(A) <strides>` a tuple of the strides in each dimension
============================ ==============================================================================

Construction and Initialization
-------------------------------

Many functions for constructing and initializing arrays are provided. In
the following list of such functions, calls with a ``dims...`` argument
can either take a single tuple of dimension sizes or a series of
dimension sizes passed as a variable number of arguments.

=================================================== =====================================================================
Function                                            Description
=================================================== =====================================================================
:func:`Array(type, dims...) <Array>`                an uninitialized dense array
:func:`cell(dims...) <cell>`                        an uninitialized cell array (heterogeneous array)
:func:`zeros(type, dims...) <zeros>`                an array of all zeros of specified type, defaults to ``Float64`` if 
                                                    ``type`` not specified
:func:`zeros(A) <zeros>`                            an array of all zeros of same element type and shape of ``A``
:func:`ones(type, dims...) <ones>`                  an array of all ones of specified type, defaults to ``Float64`` if
                                                    ``type`` not specified
:func:`ones(A) <ones>`                              an array of all ones of same element type and shape of ``A``
:func:`trues(dims...) <trues>`                      a ``Bool`` array with all values ``true``
:func:`falses(dims...) <falses>`                    a ``Bool`` array with all values ``false``
:func:`reshape(A, dims...) <reshape>`               an array with the same data as the given array, but with
                                                    different dimensions.
:func:`copy(A) <copy>`                              copy ``A``
:func:`deepcopy(A) <deepcopy>`                      copy ``A``, recursively copying its elements
:func:`similar(A, element_type, dims...) <similar>` an uninitialized array of the same type as the given array
                                                    (dense, sparse, etc.), but with the specified element type and
                                                    dimensions. The second and third arguments are both optional,
                                                    defaulting to the element type and dimensions of ``A`` if omitted.
:func:`reinterpret(type, A) <reinterpret>`          an array with the same binary data as the given array, but with the
                                                    specified element type
:func:`rand(dims) <rand>`                           ``Array`` of ``Float64``\ s with random, iid[#]_ and uniformly
                                                    distributed values in the half-open interval [0, 1)
:func:`randn(dims) <randn>`                         ``Array`` of ``Float64``\ s with random, iid and standard normally
                                                    distributed random values
:func:`eye(n) <eye>`                                ``n``-by-``n`` identity matrix
:func:`eye(m, n) <eye>`                             ``m``-by-``n`` identity matrix
:func:`linspace(start, stop, n) <linspace>`         vector of ``n`` linearly-spaced elements from ``start`` to ``stop``
:func:`fill!(A, x) <fill!>`                         fill the array ``A`` with the value ``x``
:func:`fill(x, dims) <fill>`                        create an array filled with the value ``x``
=================================================== =====================================================================

.. [#] *iid*, independently and identically distributed.

Concatenation
-------------

Arrays can be constructed and also concatenated using the following
functions:

=========================== ======================================================
Function                    Description
=========================== ======================================================
:func:`cat(k, A...) <cat>`  concatenate input n-d arrays along the dimension ``k``
:func:`vcat(A...) <vcat>`   shorthand for ``cat(1, A...)``
:func:`hcat(A...) <hcat>`   shorthand for ``cat(2, A...)``
=========================== ======================================================

Scalar values passed to these functions are treated as 1-element arrays.

The concatenation functions are used so often that they have special syntax:

=================== =============
Expression          Calls
=================== =============
``[A B C ...]``     :func:`hcat`
``[A, B, C, ...]``  :func:`vcat`
``[A B; C D; ...]`` :func:`hvcat`
=================== =============

:func:`hvcat` concatenates in both dimension 1 (with semicolons) and dimension 2
(with spaces).

Typed array initializers
------------------------

An array with a specific element type can be constructed using the syntax
``T[A, B, C, ...]``. This will construct a 1-d array with element type
``T``, initialized to contain elements ``A``, ``B``, ``C``, etc.

An array constructed with an explicit type annotation does not automatically concatenate its arguments.

.. doctest::

    julia> [[1 2] [3 4]]
    1x4 Array{Int64,2}:
     1  2  3  4

    julia> Int64[[1 2] [3 4]]
    ERROR: `convert` has no method matching convert(::Type{Int64}, ::Array{Int64,2})
    <BLANKLINE>
    You might have used a 2d row vector where a 1d column vector was required.
    Note the difference between 1d column vector [1,2,3] and 2d row vector [1 2 3].
    You can convert to a column vector with the vec() function.
     in setindex! at array.jl:307

    julia> Array[[1 2] [3 4]]
    1x2 Array{Array{T,N},2}:
     1x2 Array{Int64,2}:
     1  2  1x2 Array{Int64,2}:
     3  4

.. _comprehensions:

Comprehensions
--------------

Comprehensions provide a general and powerful way to construct arrays.
Comprehension syntax is similar to set construction notation in
mathematics::

    A = [ F(x,y,...) for x=rx, y=ry, ... ]

The meaning of this form is that ``F(x,y,...)`` is evaluated with the
variables ``x``, ``y``, etc. taking on each value in their given list of
values. Values can be specified as any iterable object, but will
commonly be ranges like ``1:n`` or ``2:(n-1)``, or explicit arrays of
values like ``[1.2, 3.4, 5.7]``. The result is an N-d dense array with
dimensions that are the concatenation of the dimensions of the variable
ranges ``rx``, ``ry``, etc. and each ``F(x,y,...)`` evaluation returns a
scalar.

The following example computes a weighted average of the current element
and its left and right neighbor along a 1-d grid. :

.. testsetup:: *

    srand(314)

.. doctest:: array-rand

    julia> const x = rand(8)
    8-element Array{Float64,1}:
     0.843025
     0.869052
     0.365105
     0.699456
     0.977653
     0.994953
     0.41084 
     0.809411

    julia> [ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]
    6-element Array{Float64,1}:
     0.736559
     0.57468
     0.685417
     0.912429
     0.8446  
     0.656511

.. note:: In the above example, ``x`` is declared as constant because type
  inference in Julia does not work as well on non-constant global
  variables.

The resulting array type is inferred from the expression; in order to control
the type explicitly, the type can be prepended to the comprehension. For example,
in the above example we could have avoided declaring ``x`` as constant, and ensured
that the result is of type ``Float64`` by writing::

    Float64[ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]

.. _man-array-indexing:

Indexing
--------

The general syntax for indexing into an n-dimensional array A is::

    X = A[I_1, I_2, ..., I_n]

where each I\_k may be:

1. A scalar value
2. A ``Range`` of the form ``:``, ``a:b``, or ``a:b:c``
3. An arbitrary integer vector, including the empty vector ``[]``
4. A boolean vector

The result X generally has dimensions
``(length(I_1), length(I_2), ..., length(I_n))``, with location
``(i_1, i_2, ..., i_n)`` of X containing the value
``A[I_1[i_1], I_2[i_2], ..., I_n[i_n]]``. Trailing dimensions indexed with
scalars are dropped. For example, the dimensions of ``A[I, 1]`` will be
``(length(I),)``. Boolean vectors are first transformed with ``find``; the size of
a dimension indexed by a boolean vector will be the number of true values in the vector.

Indexing syntax is equivalent to a call to ``getindex``::

    X = getindex(A, I_1, I_2, ..., I_n)

Example:

.. doctest::

    julia> x = reshape(1:16, 4, 4)
    4x4 Array{Int64,2}:
     1  5   9  13
     2  6  10  14
     3  7  11  15
     4  8  12  16

    julia> x[2:3, 2:end-1]
    2x2 Array{Int64,2}:
     6  10
     7  11

Empty ranges of the form ``n:n-1`` are sometimes used to indicate the inter-index
location between ``n-1`` and ``n``.  For example, the :func:`searchsorted` function uses 
this convention to indicate the insertion point of a value not found in a sorted
array:

.. doctest::

    julia> a = [1,2,5,6,7];

    julia> searchsorted(a, 3)
    3:2

Assignment
----------

The general syntax for assigning values in an n-dimensional array A is::

    A[I_1, I_2, ..., I_n] = X

where each I\_k may be:

1. A scalar value
2. A ``Range`` of the form ``:``, ``a:b``, or ``a:b:c``
3. An arbitrary integer vector, including the empty vector ``[]``
4. A boolean vector

If ``X`` is an array, its size must be ``(length(I_1), length(I_2), ..., length(I_n))``,
and the value in location ``i_1, i_2, ..., i_n`` of ``A`` is overwritten with
the value ``X[I_1[i_1], I_2[i_2], ..., I_n[i_n]]``. If ``X`` is not an array, its
value is written to all referenced locations of ``A``.

A boolean vector used as an index behaves as in :func:`getindex` (it is first transformed
with :func:`find`).

Index assignment syntax is equivalent to a call to :func:`setindex!`::

      setindex!(A, X, I_1, I_2, ..., I_n)

Example:

.. doctest::

    julia> x = reshape(1:9, 3, 3)
    3x3 Array{Int64,2}:
     1  4  7
     2  5  8
     3  6  9

    julia> x[1:2, 2:3] = -1
    -1

    julia> x
    3x3 Array{Int64,2}:
     1  -1  -1
     2  -1  -1
     3   6   9

Vectorized Operators and Functions
----------------------------------

The following operators are supported for arrays. The dot version of a binary
operator should be used for elementwise operations.

1.  Unary arithmetic — ``-``, ``+``, ``!``
2.  Binary arithmetic — ``+``, ``-``, ``*``, ``.*``, ``/``, ``./``,
    ``\``, ``.\``, ``^``, ``.^``, ``div``, ``mod``
3.  Comparison — ``.==``, ``.!=``, ``.<``, ``.<=``, ``.>``, ``.>=``
4.  Unary Boolean or bitwise — ``~``
5.  Binary Boolean or bitwise — ``&``, ``|``, ``$``

Some operators without dots operate elementwise anyway when one argument is a
scalar. These operators are ``*``, ``+``, ``-``, and the bitwise operators. The
operators ``/`` and ``\`` operate elementwise when the denominator is a scalar.

Note that comparisons such as ``==`` operate on whole arrays, giving a single
boolean answer. Use dot operators for elementwise comparisons.

The following built-in functions are also vectorized, whereby the functions act
elementwise::

    abs abs2 angle cbrt
    airy airyai airyaiprime airybi airybiprime airyprime
    acos acosh asin asinh atan atan2 atanh
    acsc acsch asec asech acot acoth
    cos  cospi cosh  sin  sinpi sinh  tan  tanh  sinc  cosc
    csc  csch  sec  sech  cot  coth
    acosd asind atand asecd acscd acotd
    cosd  sind  tand  secd  cscd  cotd
    besselh besseli besselj besselj0 besselj1 besselk bessely bessely0 bessely1
    exp  erf  erfc  erfinv erfcinv exp2  expm1
    beta dawson digamma erfcx erfi
    exponent eta zeta gamma
    hankelh1 hankelh2
     ceil  floor  round  trunc
    isfinite isinf isnan
    lbeta lfact lgamma
    log log10 log1p log2
    copysign max min significand
    sqrt hypot

Note that there is a difference between :func:`min` and :func:`max`, which operate
elementwise over multiple array arguments, and :func:`minimum` and :func:`maximum`, which
find the smallest and largest values within an array.

Julia provides the :func:`@vectorize_1arg` and :func:`@vectorize_2arg`
macros to automatically vectorize any function of one or two arguments
respectively.  Each of these takes two arguments, namely the ``Type`` of
argument (which is usually chosen to be the most general possible) and
the name of the function to vectorize. Here is a simple example:

.. doctest::

    julia> square(x) = x^2
    square (generic function with 1 method)

    julia> @vectorize_1arg Number square
    square (generic function with 4 methods)

    julia> methods(square)
    # 4 methods for generic function "square":
    square{T<:Number}(::AbstractArray{T<:Number,1}) at operators.jl:359
    square{T<:Number}(::AbstractArray{T<:Number,2}) at operators.jl:360
    square{T<:Number}(::AbstractArray{T<:Number,N}) at operators.jl:362
    square(x) at none:1

    julia> square([1 2 4; 5 6 7])
    2x3 Array{Int64,2}:
      1   4  16
     25  36  49

Broadcasting
------------

It is sometimes useful to perform element-by-element binary operations
on arrays of different sizes, such as adding a vector to each column
of a matrix.  An inefficient way to do this would be to replicate the
vector to the size of the matrix:

.. doctest::

    julia> a = rand(2,1); A = rand(2,3);

    julia> repmat(a,1,3)+A
    2x3 Array{Float64,2}:
     1.20813  1.82068  1.25387
     1.56851  1.86401  1.67846

This is wasteful when dimensions get large, so Julia offers
:func:`broadcast`, which expands singleton dimensions in
array arguments to match the corresponding dimension in the other
array without using extra memory, and applies the given
function elementwise:

.. doctest::

    julia> broadcast(+, a, A)
    2x3 Array{Float64,2}:
     1.20813  1.82068  1.25387
     1.56851  1.86401  1.67846

    julia> b = rand(1,2)
    1x2 Array{Float64,2}:
     0.867535  0.00457906

    julia> broadcast(+, a, b)
    2x2 Array{Float64,2}:
     1.71056  0.847604
     1.73659  0.873631

Elementwise operators such as ``.+`` and ``.*`` perform broadcasting if necessary. There is also a :func:`broadcast!` function to specify an explicit destination, and :func:`broadcast_getindex` and :func:`broadcast_setindex!` that broadcast the indices before indexing.

Implementation
--------------

The base array type in Julia is the abstract type
``AbstractArray{T,N}``. It is parametrized by the number of dimensions
``N`` and the element type ``T``. ``AbstractVector`` and
``AbstractMatrix`` are aliases for the 1-d and 2-d cases. Operations on
``AbstractArray`` objects are defined using higher level operators and
functions, in a way that is independent of the underlying storage.
These operations generally work correctly as a fallback for any
specific array implementation.

The ``AbstractArray`` type includes anything vaguely array-like, and
implementations of it might be quite different from conventional
arrays. For example, elements might be computed on request rather than
stored.  However, any concrete ``AbstractArray{T,N}`` type should
generally implement at least :func:`size(A) <size>` (returing an ``Int`` tuple),
:func:`getindex(A,i) <getindex>` and :func:`getindex(A,i1,...,iN) <getindex>` (returning an element
of type ``T``); mutable arrays should also implement :func:`setindex!`.  It
is recommended that these operations have nearly constant time complexity,
or technically Õ(1) complexity, as otherwise some array functions may
be unexpectedly slow.   Concrete types should also typically provide
a :func:`similar(A,T=eltype(A),dims=size(A)) <similar>` method, which is used to allocate
a similar array for :func:`copy` and other out-of-place operations.

``DenseArray`` is an abstract subtype of ``AbstractArray`` intended
to include all arrays that are laid out at regular offsets in memory,
and which can therefore be passed to external C and Fortran functions
expecting this memory layout.  Subtypes should provide a method
:func:`stride(A,k) <stride>` that returns the "stride" of dimension ``k``:
increasing the index of dimension ``k`` by ``1`` should increase the
index ``i`` of :func:`getindex(A,i) <getindex>` by :func:`stride(A,k) <stride>`.  If a
pointer conversion method :func:`convert(Ptr{T}, A) <convert>` is provided, the
memory layout should correspond in the same way to these strides.

The ``Array{T,N}`` type is a specific instance of ``DenseArray``
where elements are stored in column-major order (see additional notes in
:ref:`man-performance-tips`). ``Vector`` and ``Matrix`` are aliases for
the 1-d and 2-d cases. Specific operations such as scalar indexing,
assignment, and a few other basic storage-specific operations are all
that have to be implemented for ``Array``, so that the rest of the array
library can be implemented in a generic manner.

``SubArray`` is a specialization of ``AbstractArray`` that performs
indexing by reference rather than by copying. A ``SubArray`` is created
with the :func:`sub` function, which is called the same way as :func:`getindex` (with
an array and a series of index arguments). The result of :func:`sub` looks
the same as the result of :func:`getindex`, except the data is left in place.
:func:`sub` stores the input index vectors in a ``SubArray`` object, which
can later be used to index the original array indirectly.

``StridedVector`` and ``StridedMatrix`` are convenient aliases defined
to make it possible for Julia to call a wider range of BLAS and LAPACK
functions by passing them either ``Array`` or ``SubArray`` objects, and
thus saving inefficiencies from memory allocation and copying.

The following example computes the QR decomposition of a small section
of a larger array, without creating any temporaries, and by calling the
appropriate LAPACK function with the right leading dimension size and
stride parameters.

.. doctest::

    julia> a = rand(10,10)
    10x10 Array{Float64,2}:
     0.561255   0.226678   0.203391  0.308912   …  0.750307  0.235023   0.217964
     0.718915   0.537192   0.556946  0.996234      0.666232  0.509423   0.660788
     0.493501   0.0565622  0.118392  0.493498      0.262048  0.940693   0.252965
     0.0470779  0.736979   0.264822  0.228787      0.161441  0.897023   0.567641
     0.343935   0.32327    0.795673  0.452242      0.468819  0.628507   0.511528
     0.935597   0.991511   0.571297  0.74485    …  0.84589   0.178834   0.284413
     0.160706   0.672252   0.133158  0.65554       0.371826  0.770628   0.0531208
     0.306617   0.836126   0.301198  0.0224702     0.39344   0.0370205  0.536062
     0.890947   0.168877   0.32002   0.486136      0.096078  0.172048   0.77672
     0.507762   0.573567   0.220124  0.165816      0.211049  0.433277   0.539476

    julia> b = sub(a, 2:2:8,2:2:4)
    4x2 SubArray{Float64,2,Array{Float64,2},(StepRange{Int64,Int64},StepRange{Int64,Int64})}:
     0.537192  0.996234
     0.736979  0.228787
     0.991511  0.74485
     0.836126  0.0224702

    julia> (q,r) = qr(b);

    julia> q
    4x2 Array{Float64,2}:
     -0.338809   0.78934
     -0.464815  -0.230274
     -0.625349   0.194538
     -0.527347  -0.534856

    julia> r
    2x2 Array{Float64,2}:
     -1.58553  -0.921517
      0.0       0.866567

Sparse Matrices
===============

`Sparse matrices <http://en.wikipedia.org/wiki/Sparse_matrix>`_ are
matrices that contain enough zeros that storing them in a special data
structure leads to savings in space and execution time. Sparse
matrices may be used when operations on the sparse representation of a
matrix lead to considerable gains in either time or space when
compared to performing the same operations on a dense matrix.

Compressed Sparse Column (CSC) Storage
--------------------------------------

In Julia, sparse matrices are stored in the `Compressed Sparse Column
(CSC) format
<http://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29>`_.
Julia sparse matrices have the type ``SparseMatrixCSC{Tv,Ti}``, where ``Tv``
is the type of the nonzero values, and ``Ti`` is the integer type for
storing column pointers and row indices.::

    type SparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
        m::Int                  # Number of rows
        n::Int                  # Number of columns
        colptr::Vector{Ti}      # Column i is in colptr[i]:(colptr[i+1]-1)
        rowval::Vector{Ti}      # Row values of nonzeros
        nzval::Vector{Tv}       # Nonzero values
    end

The compressed sparse column storage makes it easy and quick to access
the elements in the column of a sparse matrix, whereas accessing the
sparse matrix by rows is considerably slower. Operations such as
insertion of nonzero values one at a time in the CSC structure tend to
be slow. This is because all elements of the sparse matrix that are
beyond the point of insertion have to be moved one place over.

All operations on sparse matrices are carefully implemented to exploit
the CSC data structure for performance, and to avoid expensive operations.

If you have data in CSC format from a different application or library, 
and wish to import it in Julia, make sure that you use 1-based indexing.
The row indices in every column need to be sorted. If your `SparseMatrixCSC` 
object contains unsorted row indices, one quick way to sort them is by
doing a double transpose.

In some applications, it is convenient to store explicit zero values
in a `SparseMatrixCSC`. These *are* accepted by functions in ``Base``
(but there is no guarantee that they will be preserved in mutating
operations).  Such explicitly stored zeros are treated as structural
nonzeros by many routines.  The :func:`nnz` function returns the number of
elements explicitly stored in the sparse data structure,
including structural nonzeros. In order to count the exact number of actual
values that are nonzero, use :func:`countnz`, which inspects every stored
element of a sparse matrix.

Sparse matrix constructors
--------------------------

The simplest way to create sparse matrices is to use functions
equivalent to the :func:`zeros` and :func:`eye` functions that Julia provides
for working with dense matrices. To produce sparse matrices instead,
you can use the same names with an ``sp`` prefix:

.. doctest::

    julia> spzeros(3,5)
    3x5 sparse matrix with 0 Float64 entries:

    julia> speye(3,5)
    3x5 sparse matrix with 3 Float64 entries:
            [1, 1]  =  1.0
            [2, 2]  =  1.0
            [3, 3]  =  1.0

The :func:`sparse` function is often a handy way to construct sparse
matrices. It takes as its input a vector ``I`` of row indices, a
vector ``J`` of column indices, and a vector ``V`` of nonzero
values. ``sparse(I,J,V)`` constructs a sparse matrix such that
``S[I[k], J[k]] = V[k]``.

.. doctest::

    julia> I = [1, 4, 3, 5]; J = [4, 7, 18, 9]; V = [1, 2, -5, 3];

    julia> S = sparse(I,J,V)
    5x18 sparse matrix with 4 Int64 entries:
            [1 ,  4]  =  1
            [4 ,  7]  =  2
            [5 ,  9]  =  3
            [3 , 18]  =  -5

The inverse of the :func:`sparse` function is :func:`findn`, which
retrieves the inputs used to create the sparse matrix.

.. doctest::

    julia> findn(S)
    ([1,4,5,3],[4,7,9,18])

    julia> findnz(S)
    ([1,4,5,3],[4,7,9,18],[1,2,3,-5])

Another way to create sparse matrices is to convert a dense matrix
into a sparse matrix using the :func:`sparse` function:

.. doctest::

    julia> sparse(eye(5))
    5x5 sparse matrix with 5 Float64 entries:
            [1, 1]  =  1.0
            [2, 2]  =  1.0
            [3, 3]  =  1.0
            [4, 4]  =  1.0
            [5, 5]  =  1.0

You can go in the other direction using the :func:`full` function. The 
:func:`issparse` function can be used to query if a matrix is sparse.

.. doctest::

    julia> issparse(speye(5))
    true

Sparse matrix operations
------------------------

Arithmetic operations on sparse matrices also work as they do on dense
matrices. Indexing of, assignment into, and concatenation of sparse
matrices work in the same way as dense matrices. Indexing operations,
especially assignment, are expensive, when carried out one element at
a time. In many cases it may be better to convert the sparse matrix
into ``(I,J,V)`` format using :func:`findnz`, manipulate the non-zeroes or
the structure in the dense vectors ``(I,J,V)``, and then reconstruct
the sparse matrix.

Correspondence of dense and sparse methods
------------------------------------------
The following table gives a correspondence between built-in methods on sparse
matrices and their corresponding methods on dense matrix types. In general,
methods that generate sparse matrices differ from their dense counterparts in
that the resulting matrix follows the same sparsity pattern as a given sparse
matrix ``S``, or that the resulting sparse matrix has density ``d``, i.e. each
matrix element has a probability ``d`` of being non-zero.

Details can be found in the :ref:`stdlib-sparse` section of the standard library
reference.

.. tabularcolumns:: |l|l|L|

+----------------------------------------+----------------------------------+--------------------------------------------+
| Sparse                                 | Dense                            | Description                                |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`spzeros(m,n) <spzeros>`         | :func:`zeros(m,n) <zeros>`       | Creates a *m*-by-*n* matrix of zeros.      |
|                                        |                                  | (:func:`spzeros(m,n) <spzeros>` is empty.) |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`spones(S) <spones>`             | :func:`ones(m,n) <ones>`         | Creates a matrix filled with ones.         |
|                                        |                                  | Unlike the dense version, :func:`spones`   |
|                                        |                                  | has the same sparsity pattern as *S*.      |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`speye(n) <speye>`               | :func:`eye(n) <eye>`             | Creates a *n*-by-*n* identity matrix.      |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`full(S) <full>`                 | :func:`sparse(A) <sparse>`       | Interconverts between dense                |
|                                        |                                  | and sparse formats.                        |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`sprand(m,n,d) <sprand>`         | :func:`rand(m,n) <rand>`         | Creates a *m*-by-*n* random matrix (of     |
|                                        |                                  | density *d*) with iid non-zero elements    |
|                                        |                                  | distributed uniformly on the               |
|                                        |                                  | half-open interval [0, 1).                 |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`sprandn(m,n,d) <sprandn>`       | :func:`randn(m,n) <randn>`       | Creates a *m*-by-*n* random matrix (of     |
|                                        |                                  | density *d*) with iid non-zero elements    |
|                                        |                                  | distributed according to the standard      |
|                                        |                                  | normal (Gaussian) distribution.            |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`sprandn(m,n,d,X) <sprandn>`     | :func:`randn(m,n,X) <randn>`     | Creates a *m*-by-*n* random matrix (of     |
|                                        |                                  | density *d*) with iid non-zero elements    |
|                                        |                                  | distributed according to the *X*           |
|                                        |                                  | distribution. (Requires the                |
|                                        |                                  | ``Distributions`` package.)                |
+----------------------------------------+----------------------------------+--------------------------------------------+
| :func:`sprandbool(m,n,d) <sprandbool>` | :func:`randbool(m,n) <randbool>` | Creates a *m*-by-*n* random matrix (of     |
|                                        |                                  | density *d*) with non-zero  ``Bool``       |
|                                        |                                  | elements with probability *d* (*d* =0.5    |
|                                        |                                  | for :func:`randbool`.)                     |
+----------------------------------------+----------------------------------+--------------------------------------------+


