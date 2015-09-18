.. _man-interfaces:

************
 Interfaces
************

A lot of the power and extensibility in Julia comes from a collection of informal interfaces.  By extending a few specific methods to work for a custom type, objects of that type not only receive those functionalities, but they are also able to be used in other methods that are written to generically build upon those behaviors.

.. _man-interfaces-iteration:

Iteration
---------

================================= ======================== ===========================================
Required methods                                           Brief description
================================= ======================== ===========================================
:func:`start(iter) <start>`                                Returns the initial iteration state
:func:`next(iter, state) <next>`                           Returns the current item and the next state
:func:`done(iter, state) <done>`                           Tests if there are any items remaining
**Important optional methods**    **Default definition**   **Brief description**
:func:`eltype(IterType) <eltype>` ``Any``                  The type the items returned by :func:`next`
:func:`length(iter) <length>`     (*undefined*)            The number of items, if known
================================= ======================== ===========================================

Sequential iteration is implemented by the methods :func:`start`, :func:`done`, and :func:`next`. Instead of mutating objects as they are iterated over, Julia provides these three methods to keep track of the iteration state externally from the object. The :func:`start(iter) <start>` method returns the initial state for the iterable object ``iter``. That state gets passed along to :func:`done(iter, state) <done>`, which tests if there are any elements remaining, and :func:`next(iter, state) <next>`, which returns a tuple containing the current element and an updated ``state``. The ``state`` object can be anything, and is generally considered to be an implementation detail private to the iterable object.

Any object defines these three methods is iterable and can be used in the :ref:`many functions that rely upon iteration <stdlib-collections-iteration>`. It can also be used directly in a ``for`` loop since the syntax::

    for i in iter   # or  "for i = iter"
        # body
    end

is translated into::

    state = start(iter)
    while !done(iter, state)
        (i, state) = next(iter, state)
        # body
    end

A simple example is an iterable sequence of square numbers with a defined length:

.. doctest::

    julia> immutable Squares
               count::Int
           end
           Base.start(::Squares) = 1
           Base.next(S::Squares, state) = (state*state, state+1)
           Base.done(S::Squares, s) = s > S.count;
           Base.eltype(::Type{Squares}) = Int # Note that this is defined for the type
           Base.length(S::Squares) = S.count;

With only ``start``, ``next``, and ``done`` definitions, the ``Squares`` type is already pretty powerful. We can iterate over all the elements:

.. doctest::

    julia> for i in Squares(7)
               println(i)
           end
    1
    4
    9
    16
    25
    36
    49

We can use many of the builtin methods that work with iterables, like :func:`in`, :func:`mean` and :func:`std`:

.. doctest::

    julia> 25 in Squares(10)
    true

    julia> mean(Squares(100)), std(Squares(100))
    (3383.5,3024.355854282583)

There are a few more methods we can extend to give Julia more information about this iterable collection.  We know that the elements in a ``Squares`` sequence will always be ``Int``. By extending the :func:`eltype` method, we can give that information to Julia and help it make more specialized code in the more complicated methods. We also know the number of elements in our sequence, so we can extend :func:`length`, too.

Now, when we ask Julia to :func:`collect` all the elements into an array it can preallocate a ``Vector{Int}`` of the right size instead of blindly ``push!``\ ing each element into a ``Vector{Any}``:

.. doctest::

    julia> collect(Squares(100))' # transposed to save space
    1x100 Array{Int64,2}:
     1  4  9  16  25  36  49  64  81  100  …  9025  9216  9409  9604  9801  10000

While we can rely upon generic implementations, we can also extend specific methods where we know there is a simpler algorithm.  For example, there's a formula to compute the sum of squares, so we can override the generic iterative version with a more performant solution:

.. doctest::

    julia> Base.sum(S::Squares) = (n = S.count; return n*(n+1)*(2n+1)÷6)
           sum(Squares(1803))
    1955361914

This is a very common pattern throughout the Julia standard library: a small set of required methods define an informal interface that enable many fancier behaviors.  In some cases, types will want to additionally specialize those extra behaviors when they know a more efficient algorithm can be used in their specific case.

.. _man-interfaces-indexing:

Indexing
--------

====================================== ==================================
Methods to implement                   Brief description
====================================== ==================================
:func:`getindex(X, i) <getindex>`      ``X[i]``, indexed element access
:func:`setindex!(X, v, i) <setindex!>` ``X[i] = v``, indexed assignment
:func:`endof(X) <endof>`               The last index, used in ``X[end]``
====================================== ==================================

For the ``Squares`` iterable above, we can easily compute the ``i``\ th element of the sequence by squaring it.  We can expose this as an indexing expression ``S[i]``.  To opt into this behavior, ``Squares`` simply needs to define :func:`getindex`:

.. doctest::

    julia> function Base.getindex(S::Squares, i::Int)
               1 <= i <= S.count || throw(BoundsError(S, i))
               return i*i
           end
           Squares(100)[23]
    529

Additionally, to support the syntax ``S[end]``, we must define :func:`endof` to specify the last valid index:

.. doctest::

    julia> Base.endof(S::Squares) = length(S)
           Squares(23)[end]
    529

Note, though, that the above *only* defines :func:`getindex` with one integer index. Indexing with anything other than an ``Int`` will throw a ``MethodError`` saying that there was no matching method.  In order to support indexing with ranges or vectors of Ints, separate methods must be written:

.. doctest::

    julia> Base.getindex(S::Squares, i::Number) = S[convert(Int, i)]
           Base.getindex(S::Squares, I) = [S[i] for i in I]
           Squares(10)[[3,4.,5]]
    3-element Array{Int64,1}:
      9
     16
     25

While this is starting to support more of the :ref:`indexing operations supported by some of the builtin types <man-array-indexing>`, there's still quite a number of behaviors missing. This ``Squares`` sequence is starting to look more and more like a vector as we've added behaviors to it. Instead of defining all these behaviors ourselves, we can officially define it as a subtype of an ``AbstractArray``.

.. _man-interfaces-abstractarray:

Abstract Arrays
---------------

========================================================== ============================================ =======================================================================================
Methods to implement                                                                                    Brief description
========================================================== ============================================ =======================================================================================
:func:`size(A) <size>`                                                                                  Returns a tuple containing the dimensions of A
:func:`Base.linearindexing(Type) <Base.linearindexing>`                                                 Returns either ``Base.LinearFast()`` or ``Base.LinearSlow()``. See the description below.
:func:`getindex(A, i::Int) <getindex>`                                                                  (if ``LinearFast``) Linear scalar indexing
:func:`getindex(A, i1::Int, ..., iN::Int) <getindex>`                                                   (if ``LinearSlow``, where ``N = ndims(A)``) N-dimensional scalar indexing
:func:`setindex!(A, v, i::Int) <getindex>`                                                              (if ``LinearFast``) Scalar indexed assignment
:func:`setindex!(A, v, i1::Int, ..., iN::Int) <getindex>`                                               (if ``LinearSlow``, where ``N = ndims(A)``) N-dimensional scalar indexed assignment
**Optional methods**                                       **Default definition**                       **Brief description**
:func:`getindex(A, I...) <getindex>`                       defined in terms of scalar :func:`getindex`  :ref:`Multidimensional and nonscalar indexing <man-array-indexing>`
:func:`setindex!(A, I...) <setindex!>`                     defined in terms of scalar :func:`setindex!` :ref:`Multidimensional and nonscalar indexed assignment <man-array-indexing>`
:func:`start`/:func:`next`/:func:`done`                    defined in terms of scalar :func:`getindex`  Iteration
:func:`length(A) <length>`                                 ``prod(size(A))``                            Number of elements
:func:`similar(A) <similar>`                               ``similar(A, eltype(A), size(A))``           Return a mutable array with the same shape and element type
:func:`similar(A, ::Type{S}) <similar>`                    ``similar(A, S, size(A))``                   Return a mutable array with the same shape and the specified element type
:func:`similar(A, dims::NTuple{Int}) <similar>`            ``similar(A, eltype(A), dims)``              Return a mutable array with the same element type and the specified dimensions
:func:`similar(A, ::Type{S}, dims::NTuple{Int}) <similar>` ``Array(S, dims)``                           Return a mutable array with the specified element type and dimensions
========================================================== ============================================ =======================================================================================

If a type is defined as a subtype of ``AbstractArray``, it inherits a very large set of rich behaviors including iteration and multidimensional indexing built on top of single-element access.  See the :ref:`arrays manual page <man-arrays>` and :ref:`standard library section <stdlib-arrays>` for more supported methods.

A key part in defining an ``AbstractArray`` subtype is :func:`Base.linearindexing`. Since indexing is such an important part of an array and often occurs in hot loops, it's important to make both indexing and indexed assignment as efficient as possible.  Array data structures are typically defined in one of two ways: either it most efficiently accesses its elements using just one index (linear indexing) or it intrinsically accesses the elements with indices specified for every dimension.  These two modalities are identified by Julia as ``Base.LinearFast()`` and ``Base.LinearSlow()``.  Converting a linear index to multiple indexing subscripts is typically very expensive, so this provides a traits-based mechanism to enable efficient generic code for all array types.

This distinction determines which scalar indexing methods the type must define. ``LinearFast()`` arrays are simple: just define :func:`getindex(A::ArrayType, i::Int) <getindex>`.  When the array is subsequently indexed with a multidimensional set of indices, the fallback :func:`getindex(A::AbstractArray, I...)` efficiently converts the indices into one linear index and then calls the above method. ``LinearSlow()`` arrays, on the other hand, require methods to be defined for each supported dimensionality with ``ndims(A)`` ``Int`` indices.  For example, the builtin ``SparseMatrix`` type only supports two dimensions, so it just defines :func:`getindex(A::SparseMatrix, i::Int, j::Int)`.  The same holds for :func:`setindex!`.

Returning to the sequence of squares from above, we could instead define it as a subtype of an ``AbstractArray{Int, 1}``:

.. doctest::

    julia> immutable SquaresVector <: AbstractArray{Int, 1}
               count::Int
           end
           Base.size(S::SquaresVector) = (S.count,)
           Base.linearindexing(::Type{SquaresVector}) = Base.LinearFast()
           Base.getindex(S::SquaresVector, i::Int) = i*i;

Note that it's very important to specify the two parameters of the ``AbstractArray``; the first defines the :func:`eltype`, and the second defines the :func:`ndims`.  That supertype and those three methods are all it takes for ``SquaresVector`` to be an iterable, indexable, and completely functional array:

.. testsetup::

    srand(1);

.. doctest::

    julia> s = SquaresVector(7)
    7-element SquaresVector:
      1
      4
      9
     16
     25
     36
     49

    julia> s[s .> 20]
    3-element Array{Int64,1}:
     25
     36
     49

    julia> s \ rand(7,2)
    1x2 Array{Float64,2}:
     0.0151876  0.0179393

As a more complicated example, let's define our own toy N-dimensional sparse-like array type built on top of ``Dict``:

.. doctest::

    julia> immutable SparseArray{T,N} <: AbstractArray{T,N}
               data::Dict{NTuple{N,Int}, T}
               dims::NTuple{N,Int}
           end
           SparseArray{T}(::Type{T}, dims::Int...) = SparseArray(T, dims)
           SparseArray{T,N}(::Type{T}, dims::NTuple{N,Int}) = SparseArray{T,N}(Dict{NTuple{N,Int}, T}(), dims)
    SparseArray{T,N}

    julia> Base.size(A::SparseArray) = A.dims
           Base.similar{T}(A::SparseArray, ::Type{T}, dims::Dims) = SparseArray(T, dims)
           # Define scalar indexing and indexed assignment for up to 3 dimensions
           Base.getindex{T}(A::SparseArray{T,1}, i1::Int)                   = get(A.data, (i1,), zero(T))
           Base.getindex{T}(A::SparseArray{T,2}, i1::Int, i2::Int)          = get(A.data, (i1,i2), zero(T))
           Base.getindex{T}(A::SparseArray{T,3}, i1::Int, i2::Int, i3::Int) = get(A.data, (i1,i2,i3), zero(T))
           Base.setindex!{T}(A::SparseArray{T,1}, v, i1::Int)                   = (A.data[(i1,)] = v)
           Base.setindex!{T}(A::SparseArray{T,2}, v, i1::Int, i2::Int)          = (A.data[(i1,i2)] = v)
           Base.setindex!{T}(A::SparseArray{T,3}, v, i1::Int, i2::Int, i3::Int) = (A.data[(i1,i2,i3)] = v);

Notice that this is a ``LinearSlow`` array, so we must manually define :func:`getindex` and :func:`setindex!` for each dimensionality we'd like to support.  Unlike the ``SquaresVector``, we are able to define :func:`setindex!`, and so we can mutate the array:

.. doctest::

    julia> A = SparseArray(Float64,3,3)
    3x3 SparseArray{Float64,2}:
     0.0  0.0  0.0
     0.0  0.0  0.0
     0.0  0.0  0.0

    julia> rand!(A)
    3x3 SparseArray{Float64,2}:
     0.28119   0.0203749  0.0769509
     0.209472  0.287702   0.640396
     0.251379  0.859512   0.873544

    julia> A[:] = 1:length(A); A
    3x3 SparseArray{Float64,2}:
     1.0  4.0  7.0
     2.0  5.0  8.0
     3.0  6.0  9.0

The result of indexing an AbstractArray can itself be an array (for instance when indexing by a ``Range``). The AbstractArray fallback methods use :func:`similar` to allocate an ``Array`` of the appropriate size and element type, which is filled in using the basic indexing method described above. However, when implementing an array wrapper you often want the result to be wrapped as well:

.. doctest::

    julia> A[1:2,:]
    2x3 SparseArray{Float64,2}:
     1.0  4.0  7.0
     2.0  5.0  8.0

In this example it is accomplished by defining ``Base.similar{T}(A::SparseArray, ::Type{T}, dims::Dims)`` to create the appropriate wrapped array. For this to work it's important that ``SparseArray`` is mutable (supports ``setindex!``). :func:`similar` is also used to allocate result arrays for arithmetic on `AbstractArray`s, for instance:

.. doctest::

    julia> A + 4
    3x3 SparseArray{Float64,2}:
     5.0   8.0  11.0
     6.0   9.0  12.0
     7.0  10.0  13.0

In addition to all the iterable and indexable methods from above, these types can also interact with each other and use all of the methods defined in the standard library for ``AbstractArrays``:

.. doctest::

    julia> A[SquaresVector(3)]
    3-element SparseArray{Float64,1}:
     1.0
     4.0
     9.0

    julia> dot(A[:,1],A[:,2])
    32.0
