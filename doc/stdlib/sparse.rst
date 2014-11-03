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

.. function:: full(S)

   Convert a sparse matrix ``S`` into a dense matrix.

.. function:: nnz(A)

   Returns the number of stored (filled) elements in a sparse matrix.

.. function:: spzeros(m,n)

   Create an empty sparse matrix of size ``m x n``.

.. function:: spones(S)

   Create a sparse matrix with the same structure as that of ``S``, but with every nonzero element having the value ``1.0``.

.. function:: speye(type,m[,n])

   Create a sparse identity matrix of specified type of size ``m x m``. In case ``n`` is supplied, create a sparse identity matrix of size ``m x n``.

.. function:: spdiagm(B, d[, m, n])

   Construct a sparse diagonal matrix. ``B`` is a tuple of vectors containing the diagonals and ``d`` is a tuple containing the positions of the diagonals. In the case the input contains only one diagonaly, ``B`` can be a vector (instead of a tuple) and ``d`` can be the diagonal position (instead of a tuple), defaulting to 0 (diagonal). Optionally, ``m`` and ``n`` specify the size of the resulting sparse matrix.

.. function:: sprand(m,n,p[,rng])

   Create a random ``m`` by ``n`` sparse matrix, in which the probability of any element being nonzero is independently given by ``p`` (and hence the mean density of nonzeros is also exactly ``p``). Nonzero values are sampled from the distribution specified by ``rng``. The uniform distribution is used in case ``rng`` is not specified.

.. function:: sprandn(m,n,p)

   Create a random ``m`` by ``n`` sparse matrix with the specified (independent) probability ``p`` of any entry being nonzero, where nonzero values are sampled from the normal distribution.

.. function:: sprandbool(m,n,p)

   Create a random ``m`` by ``n`` sparse boolean matrix with the specified (independent) probability ``p`` of any entry being ``true``.

.. function:: etree(A[, post])

   Compute the elimination tree of a symmetric sparse matrix ``A`` from ``triu(A)`` and, optionally, its post-ordering permutation.

.. function:: symperm(A, p)

   Return the symmetric permutation of A, which is ``A[p,p]``. A should be symmetric and sparse, where only the upper triangular part of the matrix is stored. This algorithm ignores the lower triangular part of the matrix. Only the upper triangular part of the result is returned as well.

.. function:: nonzeros(A)

   Return a vector of the structural nonzero values in sparse matrix ``A``. This includes zeros that are explicitly stored in the sparse matrix. The returned vector points directly to the internal nonzero storage of ``A``, and any modifications to the returned vector will mutate ``A`` as well. See ``rowvals(A)`` and ``nzrange(A, col)``.

.. function:: rowvals(A)

   Return a vector of the row indices of ``A``, and any modifications to the returned vector will mutate ``A`` as well. Given the internal storage format of sparse matrices, providing access to how the row indices are stored internally can be useful in conjuction with iterating over structural nonzero values. See ``nonzeros(A)`` and ``nzrange(A, col)``.

.. function:: nzrange(A, col)

   Return the range of indices to the structural nonzero values of a sparse matrix column. In conjunction with ``nonzeros(A)`` and ``rowvals(A)``, this allows for convenient iterating over a sparse matrix ::

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
