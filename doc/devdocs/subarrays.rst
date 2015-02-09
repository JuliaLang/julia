.. _devdocs-subarrays:

.. currentmodule:: Base

**************************
SubArrays
**************************

Julia's ``SubArray`` type is a container encoding a "view" of a parent
``AbstractArray``.  This page documents some of the design principles
and implementation of ``SubArray``\s.

Indexing: cartesian vs. linear indexing
---------------------------------------

Broadly speaking, there are two main ways to access data in an array.
The first, often called cartesian indexing, uses ``N`` indexes for an
``N`` -dimensional ``AbstractArray``.  For example, a matrix ``A``
(2-dimensional) can be indexed in cartesian style as ``A[i,j]``.  The
second indexing method, refered to as linear indexing, uses a single
index even for higher-dimensional objects.  For example, if ``A =
reshape(1:12, 3, 4)``, then the expression ``A[5]`` returns the
value 5.  Julia allows you to combine these styles of indexing: for
example, a 3d array ``A3`` can be indexed as ``A3[i,j]``, in which
case ``i`` is interpreted as a cartesian index for the first
dimension, and ``j`` is a linear index over dimensions 2 and 3.

For ``Array``\s, linear indexing appeals to the underlying storage
format: an array is laid out as a contiguous block of memory, and
hence the linear index is just the offset (+1) of the corresponding
entry relative to the beginning of the array.  However, this is not
true for many other ``AbstractArray`` types: examples include
``SparseMatrixCSC``, arrays that require some kind of computation
(such as interpolation), and the type under discussion here,
``SubArray``.  For these types, the underlying information is more
naturally described in terms of cartesian indexes.

You can manually convert from a cartesian index to a linear index with
``sub2ind``, and vice versa using ``ind2sub``.  ``getindex`` and
``setindex!`` functions for ``AbstractArray`` types may include
similar operations.

While converting from a cartesian index to a linear index is fast
(it's just multiplication and addition), converting from a linear
index to a cartesian index is very slow: it relies on the ``div``
operation, which is one of the slowest low-level operations you can
perform with a CPU.  For this reason, any code that deals with
``AbstractArray`` types is best designed in terms of cartesian, rather than
linear, indexing.

Index replacement
-----------------

Consider making 2d slices of a 3d array::

  S1 = slice(A, :, 5, 2:6)
  S2 = slice(A, 5, :, 2:6)

``slice`` drops "singleton" dimensions (ones that are specified by an
``Int``), so both ``S1`` and ``S2`` are two-dimensional ``SubArray``\s.
Consequently, the natural way to index these is with ``S1[i,j]``.  To
extract the value from the parent array ``A``, the natural approach is
to replace ``S1[i,j]`` with ``A[i,5,(2:6)[j]]`` and ``S2[i,j]`` with
``A[5,i,(2:6)[j]]``.

The key feature of the design of SubArrays is that this index
replacement can be performed without any runtime overhead.

SubArray design
---------------

Type parameters and fields
~~~~~~~~~~~~~~~~~~~~~~~~~~

The strategy adopted is first and foremost expressed in the definition
of the type::

    type SubArray{T,N,P<:AbstractArray,I<:(ViewIndex...),LD} <: AbstractArray{T,N}
        parent::P
        indexes::I
        dims::NTuple{N,Int}
        first_index::Int   # for linear indexing and pointer
        stride1::Int       # used only for linear indexing
    end

``SubArray`` has 5 type parameters.  The first two are the
standard element type and dimensionality.  The next is the type of the
parent ``AbstractArray``.  The most heavily-used is the fourth
parameter, a ``tuple`` of the types of the indexes for each dimension.
The final one, ``LD``, is used only in special circumstances, to
implement efficient linear indexing for those types that can support
it.

If in our example above ``A`` is a ``Array{Float64, 3}``, our ``S1``
case above would be a
``SubArray{Float64,2,Array{Float64,3},(Colon,Int64,UnitRange{Int64}),2}``.
Note in particular the tuple parameter, which stores the types of
the indexes used to create ``S1``.  Likewise,
::

    julia> S1.indexes
    (Colon(),5,2:6)

Storing these values allows index replacement, and having the types
encoded as parameters allows one to dispatch to efficient algorithms.

An ``Int`` index is used to represent a parent dimension that should
be dropped.  The distinction between the ``sub`` and ``slice``
commands is that ``sub`` converts *interior* ``Int`` indices into
ranges at the time of construction.  For example::

    S3 = sub(A, :, 5, 2:6)

    julia> S3.indexes
    (Colon(),5:5,2:6)

Because of this conversion, ``S3`` is three-dimensional.

``getindex`` and ``setindex!`` (index translation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Performing index translation requires that you do different things for
different concrete ``SubArray`` types.  For example, for ``S1``, one needs
to apply the ``i,j`` indexes to the first and third dimensions of the
parent array, whereas for ``S2`` one needs to apply them to the
second and third.  The simplest approach to indexing would be to do
the type-analysis at runtime::

    parentindexes = Array(Any, 0)
    for i = 1:ndims(S.parent)
        ...
        if isa(thisindex, Int)
            # Don't consume one of the input indexes
	    push!(parentindexes, thisindex)
        else
	    # Consume an input index
	    push!(parentindexes, thisindex[inputindex[j]])
	    j += 1
        end
    end
    S.parent[parentindexes...]

Unfortunately, this would be disastrous in terms of performance: each
element access would allocate memory, and involves the running of a
lot of poorly-typed code.

The better approach is to dispatch to specific methods to handle each
type of input.  Note, however, that the number of distinct methods
needed grows exponentially in the number of dimensions, and since
Julia supports arrays of any dimension the number of methods required
is in fact infinite.  Fortunately, ``stagedfunction``\s allow one to
generate the necessary methods quite straightforwardly.  The resulting
code looks quite a lot like the runtime approach above, but all of the
type analysis is performed at the time of method instantiation.  For a
``SubArray`` of the type of ``S1``, the method executed at runtime is
literally ::

    getindex(S::<type of S1>, i, j) = S.parent[i, S.indexes[2], S.indexes[3][j]]

Linear indexing
~~~~~~~~~~~~~~~

Linear indexing can be implemented efficiently when the entire array
has a single stride that separates successive elements.  For
``SubArray`` types, the availability of efficient linear indexing is based
purely on the types of the indexes, and does not depend on values like
the size of the array.  It therefore can miss some cases in which the
stride happens to be uniform::

 julia> A = reshape(1:4*2, 4, 2)
 4x2 Array{Int64,2}:
  1  5
  2  6
  3  7
  4  8

 julia> diff(A[2:2:4,:][:])
 3-element Array{Int64,1}:
  2
  2
  2

A view constructed as ``sub(A, 2:2:4, :)`` happens to have uniform
stride, and therefore linear indexing indeed could be performed
efficiently.  However, success in this case depends on the size of the
array: if the first dimension instead were odd, ::

 julia> A = reshape(1:5*2, 5, 2)
 5x2 Array{Int64,2}:
  1   6
  2   7
  3   8
  4   9
  5  10

 julia> diff(A[2:2:4,:][:])
 3-element Array{Int64,1}:
  2
  3
  2

then ``A[2:2:4,:]`` does not have uniform stride, so we cannot
guarantee efficient linear indexing.  Since we have to base this
decision based purely on types encoded in the parameters of the
``SubArray``, ``S = sub(A, 2:2:4, :)`` cannot implement efficient
linear indexing.

The last parameter of ``SubArray``, ``LD``, encodes the highest
dimension up to which elements are guaranteed to have uniform stride.
When ``LD == length(I)``, the length of the ``indexes`` tuple,
efficient linear indexing becomes possible.

An example might help clarify what this means:

- For ``S1`` above, the ``Colon`` along the first dimension is
  uniformly spaced (all elements are displaced by 1 from the previous
  value), so this dimension does not "break" linear indexing.
  Consequently ``LD`` has a value of at least 1.

- The second dimension of the parent, sliced out as ``5``, does not
  not by itself break linear indexing:  if all of the remaining
  indexes were ``Int``, the entire ``SubArray`` would have efficient
  linear indexing.  Consequently, ``LD`` is at least 2.

- The last dimension is a ``Range``.  This would by itself break
  linear indexing (even though it is a ``UnitRange``, the fact that it
  might not start at 1 means that there might be gaps).  Additionally,
  given the preceeding indexes any choice other than ``Int`` would
  also have truncated ``LD`` at 2.

Consequently, as a whole ``S1`` does not have efficient linear
indexing.

However, if we were to later say ``S1a = slice(S1, 2:2:7, 3)``,
``S1a`` would have an ``LD`` of 3 (its indexes tuple has type
``(Colon, Int, Int)``) and would have efficient linear indexing.  This
ability to re-slice is the main motivation to use an integer ``LD``
rather than a boolean flag to encode the applicability of linear
indexing.

The main reason ``LD`` cannot always be inferred from the ``indexes`` tuple
is because ``sub`` converts internal ``Int`` indexes into
``UnitRange``\s.  Consequently it is important to encode "safe"
dimensions of size 1 prior to conversion.  Up to the ``LDth`` entry,
we can be sure that any ``UnitRange`` was, in fact, an ``Integer``
prior to conversion.


A few details
~~~~~~~~~~~~~

- Hopefully by now it's fairly clear that supporting slices means that
  the dimensionality, given by the parameter ``N``, is not necessarily
  equal to the dimensionality of the parent array or the length of the
  ``indexes`` tuple.  Neither do user-supplied indexes necessarily
  line up with entries in the ``indexes`` tuple (e.g., the second
  user-supplied index might correspond to the third dimension of the
  parent array, and the third element in the ``indexes`` tuple).

  What might be less obvious is that the dimensionality of the parent
  array may not be equal to the length of the ``indexes`` tuple.  Some
  examples::

    A = reshape(1:35, 5, 7) # A 2d parent Array
    S = sub(A, 2:7)         # A 1d view created by linear indexing
    S = sub(A, :, :, 1)     # Appending extra indexes is supported
    S = sub(A, :, :, 1:1)

  Consequently, internal ``SubArray`` code needs to be fairly careful
  about which of these three notions of dimensionality is relevant in
  each circumstance.

- Because the processing needed to implement all of the stagedfunction
  expressions isn't readily available at the time ``subarray.jl``
  appears in the bootstrap process, ``SubArray`` functionality is
  split into two files, the second being ``subarray2.jl``.

- Bounds-checking has currently not been tackled. There are two
  relevant notions of bounds-checking, one at construction time and
  one during element access.  This is an important outstanding issue.
