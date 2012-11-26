sparse.jl --- Working with Sparse Matrices
==========================================

.. .. module:: sparse.jl
   :synopsis: Functions for working with sparse matrices

This module contains functions that allow one to use sparse matrices in Julia.

Sparse Matrices vs. Dense Matrices
----------------------------------

Often we wish to work with matrices that (1) are too large to fit into memory using the array representation employed by Julia's standard matrices and that (2) take on the value zero almost everywhere. Matrices in which a minority of entries are non-zero are called sparse. Julia provides a variety of mechanisms for working with sparse matrices.

Creating Sparse Matrices
------------------------

Probably the simplest ways to create sparse matrices are using functions equivalent to the ``zeros`` and ``eye`` functions that Julia provides for working with dense matrices. To produce sparse matrices instead, you can use the same names with an ``sp`` prefix:

- ``spzeros``
- ``speye``

For example, you might do the following:

::

    N = 1_000

    b = spzeros(N, N)
    @assert all(b - b .== b)

    c = speye(N)
    @assert all(c - b .== c)

Converting between Sparse and Dense Matrices
--------------------------------------------

Another way to create sparse matrices is to convert a dense matrix into a sparse matrix using the ``sparse`` function:

::

    x = eye(10, 10)
    sx = sparse(x)

You can go in the other direction using the ``full`` function:

::

    fx = full(x)

You can determine whether a matrix is sparse or dense using the ``issparse`` function:

::

    @assert issparse(speye(10, 10)) == true
    @assert issparse(eye(10, 10)) == false

Indexing into and Assigning to Sparse Matrix Entries
----------------------------------------------------

::

    a = speye(10, 10)
    a[1, 1] = 10
    a[5, 7] = 11
    1 + a[9, 3]
    a[end, end]

Arithmetic Operations on Sparse Matrices
----------------------------------------

- ``'`` (aka ``transpose``)
- ``+``
- ``-``
- ``.*``
- ``.^``
- ``./``

::

    a = speye(10, 10)
    @assert a' == a
    @assert a + a == 2 * a
    @assert 2 * a - a == a
    @assert a .* a == a
    @assert (2 * a).^3 == 8 * a
    @assert (8 * a) ./ 8 == a
