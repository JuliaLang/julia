.. _man-arrays:

*********
 Arrays   
*********

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
functions. User code, if it needs to exhibit similar behaviour, should
take care to create a copy of inputs that it may modify.

Basic Functions
---------------

1. ``ndims(A)`` — the number of dimensions of A
2. ``size(A,n)`` — the size of A in a particular dimension
3. ``size(A)`` — a tuple containing the dimensions of A
4. ``eltype(A)`` — the type of the elements contained in A
5. ``length(A)`` — the number of elements in A
6. ``nnz(A)`` — the number of nonzero values in A
7. ``stride(A,k)`` — the size of the stride along dimension k
8. ``strides(A)`` — a tuple of the linear index distances between
   adjacent elements in each dimension

Construction and Initialization
-------------------------------

Many functions for constructing and initializing arrays are provided. In
the following list of such functions, calls with a ``dims...`` argument
can either take a single tuple of dimension sizes or a series of
dimension sizes passed as a variable number of arguments.

1.  ``Array(type, dims...)`` — an uninitialized dense array
2.  ``cell(dims...)`` — an uninitialized cell array (heterogeneous
    array)
3.  ``zeros(type, dims...)`` — an array of all zeros of specified type
4.  ``ones(type, dims...)`` — an array of all ones of specified type
5.  ``trues(dims...)`` — a ``Bool`` array with all values ``true``
6.  ``falses(dims...)`` — a ``Bool`` array with all values ``false``
7.  ``reshape(A, dims...)`` — an array with the same data as the given
    array, but with different dimensions.
8.  ``copy(A)``  — copy ``A``
9.  ``deepcopy(A)`` — copy ``A``, recursively copying its elements
10. ``similar(A, element_type, dims...)`` — an uninitialized array of
    the same type as the given array (dense, sparse, etc.), but with the
    specified element type and dimensions. The second and third
    arguments are both optional, defaulting to the element type and
    dimensions of ``A`` if omitted.
11. ``reinterpret(type, A)`` — an array with the same binary data as the
    given array, but with the specified element type.
12. ``rand(dims)`` — random array with ``Float64`` uniformly distributed
    values in [0,1)
13. ``randf(dims)`` — random array with ``Float32`` uniformly
    distributed values in [0,1)
14. ``randn(dims)`` — random array with ``Float64`` normally distributed
    random values with a mean of 0 and standard deviation of 1
15. ``eye(n)`` — n-by-n identity matrix
16. ``eye(m, n)`` — m-by-n identity matrix
17. ``linspace(start, stop, n)`` — a vector of ``n`` linearly-spaced
    elements from ``start`` to ``stop``.
18. ``fill!(A, x)`` — fill the array ``A`` with value ``x``

The last function, ``fill!``, is different in that it modifies an
existing array instead of constructing a new one. As a convention,
functions with this property have names ending with an exclamation
point. These functions are sometimes called "mutating" functions, or
"in-place" functions.

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
and its left and right neighbour along a 1-d grid.

::

    julia> const x = rand(8)
    8-element Float64 Array:
     0.276455
     0.614847
     0.0601373
     0.896024
     0.646236
     0.143959
     0.0462343
     0.730987

    julia> [ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]
    6-element Float64 Array:
     0.391572
     0.407786
     0.624605
     0.583114
     0.245097
     0.241854

NOTE: In the above example, ``x`` is declared as constant because type
inference in Julia does not work as well on non-constant global
variables.

The resulting array type is inferred from the expression; in order to control
the type explicitly, the type can be prepended to the comprehension. For example,
in the above example we could have avoided declaring ``x`` as constant, and ensured
that the result is of type ``Float64`` by writing::

    Float64[ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]

Using curly brackets instead of square brackets is a shortand notation for an
array of type ``Any``::

    julia> { i/2 for i = 1:3 }
    3-element Any Array:
     0.5
     1.0
     1.5

.. _man-array-indexing:

Indexing
--------

The general syntax for indexing into an n-dimensional array A is::

    X = A[I_1, I_2, ..., I_n]

where each I\_k may be:

1. A scalar value
2. A ``Range`` of the form ``:``, ``a:b``, or ``a:b:c``
3. An arbitrary integer vector, including the empty vector ``[]``

The result X has the dimensions
``(size(I_1), size(I_2), ..., size(I_n))``, with location
``(i_1, i_2, ..., i_n)`` of X containing the value
``A[I_1[i_1], I_2[i_2], ..., I_n[i_n]]``.

Indexing syntax is equivalent to a call to ``ref``::

    X = ref(A, I_1, I_2, ..., I_n)

Example::

    julia> x = reshape(1:16, 4, 4)
    4x4 Int64 Array
    1 5 9 13
    2 6 10 14
    3 7 11 15
    4 8 12 16

    julia> x[2:3, 2:end-1]
    2x2 Int64 Array
    6 10
    7 11

Assignment
----------

The general syntax for assigning values in an n-dimensional array A is::

    A[I_1, I_2, ..., I_n] = X

where each I\_k may be:

1. A scalar value
2. A ``Range`` of the form ``:``, ``a:b``, or ``a:b:c``
3. An arbitrary integer vector, including the empty vector ``[]``

The size of X should be ``(size(I_1), size(I_2), ..., size(I_n))``, and
the value in location ``(i_1, i_2, ..., i_n)`` of A is overwritten with
the value ``X[I_1[i_1], I_2[i_2], ..., I_n[i_n]]``.

Index assignment syntax is equivalent to a call to ``assign``::

      A = assign(A, X, I_1, I_2, ..., I_n)

Example::

    julia> x = reshape(1:9, 3, 3)
    3x3 Int64 Array
    1 4 7
    2 5 8
    3 6 9

    julia> x[1:2, 2:3] = -1
    3x3 Int64 Array
    1 -1 -1
    2 -1 -1
    3 6 9

Concatenation
-------------

Arrays can be concatenated along any dimension using the following
syntax:

1. ``cat(dim, A...)`` — concatenate input n-d arrays along the dimension
   ``dim``
2. ``vcat(A...)`` — Shorthand for ``cat(1, A...)``
3. ``hcat(A...)`` — Shorthand for ``cat(2, A...)``
4. ``hvcat(A...)``

Concatenation operators may also be used for concatenating arrays:

1. ``[A B C ...]`` — calls ``hcat``
2. ``[A, B, C, ...]`` — calls ``vcat``
3. ``[A B; C D; ...]`` — calls ``hvcat``

Vectorized Operators and Functions
----------------------------------

The following operators are supported for arrays. In case of binary
operators, the dot version of the operator should be used when both
inputs are non-scalar, and any version of the operator may be used if
one of the inputs is a scalar.

1.  Unary Arithmetic — ``-``
2.  Binary Arithmetic — ``+``, ``-``, ``*``, ``.*``, ``/``, ``./``,
    ``\``, ``.\``, ``^``, ``.^``, ``div``, ``mod``
3.  Comparison — ``==``, ``!=``, ``<``, ``<=``, ``>``, ``>=``
4.  Unary Boolean or Bitwise — ``~``
5.  Binary Boolean or Bitwise — ``&``, ``|``, ``$``
6.  Trigonometrical functions — ``sin``, ``cos``, ``tan``, ``sinh``,
    ``cosh``, ``tanh``, ``asin``, ``acos``, ``atan``, ``atan2``,
    ``sec``, ``csc``, ``cot``, ``asec``, ``acsc``, ``acot``, ``sech``,
    ``csch``, ``coth``, ``asech``, ``acsch``, ``acoth``, ``sinc``,
    ``cosc``, ``hypot``
7.  Logarithmic functions — ``log``, ``log2``, ``log10``, ``log1p``,
    ``logb``, ``ilogb``
8.  Exponential functions — ``exp``, ``expm1``, ``exp2``, ``ldexp``
9.  Rounding functions — ``ceil``, ``floor``, ``trunc``, ``round``,
    ``ipart``, ``fpart``
10. Other mathematical functions — ``min``, ``max,`` ``abs``, ``pow``,
    ``sqrt``, ``cbrt``, ``erf``, ``erfc``, ``gamma``, ``lgamma``,
    ``real``, ``conj``, ``clamp``

Broadcasting
------------

It is sometimes useful to perform element-by-element binary operations
on arrays of different sizes, such as adding a vector to each column
of a matrix.  An inefficient way to do this would be to replicate the
vector to the size of the matrix::

    julia> a = rand(2,1); A = rand(2,3);

    julia> repmat(a,1,3)+A
    2x3 Float64 Array:
     0.848333  1.66714  1.3262 
     1.26743   1.77988  1.13859

This is wasteful when dimensions get large, so Julia offers the
Matlab-inspired ``bsxfun``, which expands singleton dimensions in
array arguments to match the corresponding dimension in the other
array without using extra memory, and applies the given binary
function::

    julia> bsxfun(+, a, A)
    2x3 Float64 Array:
     0.848333  1.66714  1.3262 
     1.26743   1.77988  1.13859

    julia> b = rand(1,2)
    1x2 Float64 Array:
     0.629799  0.754948

    julia> bsxfun(+, a, b)
    2x2 Float64 Array:
     1.31849  1.44364
     1.56107  1.68622

Implementation
--------------

The base array type in Julia is the abstract type
``AbstractArray{T,n}``. It is parametrized by the number of dimensions
``n`` and the element type ``T``. ``AbstractVector`` and
``AbstractMatrix`` are aliases for the 1-d and 2-d cases. Operations on
``AbstractArray`` objects are defined using higher level operators and
functions, in a way that is independent of the underlying storage class.
These operations are guaranteed to work correctly as a fallback for any
specific array implementation.

The ``Array{T,n}`` type is a specific instance of ``AbstractArray``
where elements are stored in column-major order. ``Vector`` and
``Matrix`` are aliases for the 1-d and 2-d cases. Specific operations
such as scalar indexing, assignment, and a few other basic
storage-specific operations are all that have to be implemented for
``Array``, so that the rest of the array library can be implemented in a
generic manner for ``AbstractArray``.

``SubArray`` is a specialization of ``AbstractArray`` that performs
indexing by reference rather than by copying. A ``SubArray`` is created
with the ``sub`` function, which is called the same way as ``ref`` (with
an array and a series of index arguments). The result of ``sub`` looks
the same as the result of ``ref``, except the data is left in place.
``sub`` stores the input index vectors in a ``SubArray`` object, which
can later be used to index the original array indirectly.

``StridedVector`` and ``StridedMatrix`` are convenient aliases defined
to make it possible for Julia to call a wider range of BLAS and LAPACK
functions by passing them either ``Array`` or ``SubArray`` objects, and
thus saving inefficiencies from indexing and memory allocation.

The following example computes the QR decomposition of a small section
of a larger array, without creating any temporaries, and by calling the
appropriate LAPACK function with the right leading dimension size and
stride parameters.

::

    julia> a = rand(10,10)
    10x10 Float64 Array:
     0.362525  0.811086    0.418914  0.463756  …  0.0378657  0.369678  0.734343 
     0.827145  0.856471    0.620181  0.300975     0.105997   0.207935  0.111068 
     0.232279  0.0015412   0.906766  0.202353     0.803316   0.843412  0.584969 
     0.775187  0.922005    0.773319  0.756691     0.0328393  0.407879  0.181678 
     0.749682  0.888188    0.404268  0.401692     0.50875    0.589153  0.448658 
     0.16149   0.00695726  0.311006  0.124033  …  0.953573   0.822758  0.0124734
     0.898647  0.421168    0.5257    0.136804     0.548698   0.414837  0.55708  
     0.906457  0.823346    0.684319  0.981971     0.014441   0.182403  0.0725621
     0.769292  0.7239      0.730801  0.840325     0.905064   0.097663  0.489379 
     0.636039  0.682543    0.378586  0.858242     0.508523   0.75396   0.505509 
    
    julia> (q,r) = qr(b);
    
    julia> q
    4x2 Float64 Array:
     -0.234671   -0.902037
     -0.0949767  -0.238058
     -0.486365   -0.10859 
     -0.836277    0.343315
    
    julia> r
    2x2 Float64 Array:
     -1.14325  -1.16149 
      0.0      -0.556627
