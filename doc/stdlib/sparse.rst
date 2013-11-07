.. _stdlib-sparse:

.. currentmodule:: Base

Sparse Matrices
---------------

Sparse matrices support much of the same set of operations as dense matrices. The following functions are specific to sparse matrices.

.. function:: sparse(I,J,V,[m,n,combine])

   Create a sparse matrix ``S`` of dimensions ``m x n`` such that ``S[I[k], J[k]] = V[k]``. The ``combine`` function is used to combine duplicates. If ``m`` and ``n`` are not specified, they are set to ``max(I)`` and ``max(J)`` respectively. If the ``combine`` function is not supplied, duplicates are added by default.

.. function:: sparsevec(I, V, [m, combine])

   Create a sparse matrix ``S`` of size ``m x 1`` such that ``S[I[k]] = V[k]``. Duplicates are combined using the ``combine`` function, which defaults to ``+`` if it is not provided. In julia, sparse vectors are really just sparse matrices with one column. Given Julia's Compressed Sparse Columns (CSC) storage format, a sparse column matrix with one column is sparse, whereas a sparse row matrix with one row ends up being dense.

.. function:: sparsevec(D::Dict, [m])

   Create a sparse matrix of size ``m x 1`` where the row values are keys from the dictionary, and the nonzero values are the values from the dictionary.

.. function:: issparse(S)

   Returns ``true`` if ``S`` is sparse, and ``false`` otherwise.

.. function:: sparse(A)

   Convert a dense matrix ``A`` into a sparse matrix.

.. function:: sparsevec(A)

   Convert a dense vector ``A`` into a sparse matrix of size ``m x 1``. In julia, sparse vectors are really just sparse matrices with one column.

.. function:: dense(S)

   Convert a sparse matrix ``S`` into a dense matrix.

.. function:: full(S)

   Convert a sparse matrix ``S`` into a dense matrix.

.. function:: spzeros(m,n)

   Create an empty sparse matrix of size ``m x n``.

.. function:: spones(S)

   Create a sparse matrix with the same structure as that of ``S``, but with every nonzero element having the value ``1.0``.

.. function:: speye(type,m[,n])

   Create a sparse identity matrix of specified type of size ``m x m``. In case ``n`` is supplied, create a sparse identity matrix of size ``m x n``.

.. function:: spdiagm(B, d[, m, n])

   Construct a sparse diagonal matrix. ``B`` is a tuple of vectors containing the diagonals and ``d`` is a tuple containing the positions of the diagonals. In the case the input contains only one diagonaly, ``B`` can be a vector (instead of a tuple) and ``d`` can be the diagonal position (instead of a tuple), defaulting to 0 (diagonal). Optionally, ``m`` and ``n`` specify the size of the resulting sparse matrix.

.. function:: sprand(m,n,density[,rng])

   Create a random sparse matrix with the specified density. Nonzeros are sampled from the distribution specified by ``rng``. The uniform distribution is used in case ``rng`` is not specified.

.. function:: sprandn(m,n,density)

   Create a random sparse matrix of specified density with nonzeros sampled from the normal distribution.

.. function:: sprandbool(m,n,density)

   Create a random sparse boolean matrix with the specified density.

.. function:: etree(A[, post])

   Compute the elimination tree of a symmetric sparse matrix ``A`` from ``triu(A)`` and, optionally, its post-ordering permutation.

.. function:: symperm(A, p)

   Return the symmetric permutation of A, which is ``A[p,p]``. A should be symmetric and sparse, where only the upper triangular part of the matrix is stored. This algorithm ignores the lower triangular part of the matrix. Only the upper triangular part of the result is returned as well.


