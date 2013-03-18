Linear Algebra
--------------

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.

.. function:: *(A, B)

   Matrix multiplication

.. function:: \\(A, B)

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B``. For rectangular ``A``, QR factorization is used. For triangular ``A``, a triangular solve is performed. For square ``A``, Cholesky factorization is tried if the input is symmetric with a heavy diagonal. LU factorization is used in case Cholesky factorization fails or for general square inputs. If ``size(A,1) > size(A,2)``, the result is a least squares solution of ``A*X+eps=B`` using the singular value decomposition. ``A`` does not need to have full rank.

.. function:: dot(x, y)

   Compute the dot product

.. function:: cross(x, y)

   Compute the cross product of two 3-vectors

.. function:: norm(a)

   Compute the norm of a ``Vector`` or a ``Matrix``

.. function:: lu(A) -> L, U, P

   Compute the LU factorization of ``A``, such that ``A[P,:] = L*U``.

.. function:: lufact(A) -> LUDense

   Compute the LU factorization of ``A`` and return a ``LUDense`` object. The individual components of the factorization ``F`` can be accesed by indexing: ``F[:L]``, ``F[:U]``, and ``F[:P]`` (permutation matrix) or ``F[:p]`` (permutation vector). The following functions are available for ``LUDense`` objects: ``size``, ``\``, ``inv``, ``det``.

.. function:: lufact!(A) -> LUDense

   ``lufact!`` is the same as ``lufact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: chol(A, [LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``L`` (Lower), ``A = L*L'``. If ``LU`` is ``U`` (Upper), ``A = R'*R``.

.. function:: cholfact(A, [LU]) -> CholeskyDense

   Compute the Cholesky factorization of a symmetric positive-definite matrix ``A`` and return a ``CholeskyDense`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. The triangular matrix can be obtained from the forization ``F`` with: ``F[:L]`` and ``F[:U]``. The following functions are available for ``CholeskyDense`` objects: ``size``, ``\``, ``inv``, ``det``. A ``LAPACK.PosDefException`` error is thrown in case the matrix is not positive definite.

.. function: cholfact!(A, [LU]) -> CholeskyDense

   ``cholfact!`` is the same as ``cholfact`` but saves space by overwriting the input A, instead of creating a copy.

..  function:: cholpfact(A, [LU]) -> CholeskyPivotedDense

   Compute the pivoted Cholesky factorization of a symmetric positive semi-definite matrix ``A`` and return a ``CholeskyDensePivoted`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. The triangular factors containted in the factorization ``F`` can be obtained with ``F[:L]`` and ``F[:U]``, whereas the permutation can be obtained with ``F[:P]`` or ``F[:p]``. The following functions are available for ``CholeskyDensePivoted`` objects: ``size``, ``\``, ``inv``, ``det``. A ``LAPACK.RankDeficientException`` error is thrown in case the matrix is rank deficient.

.. function:: cholpfact!(A, [LU]) -> CholeskyPivotedDense

   ``cholpfact!`` is the same as ``cholpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qr(A) -> Q, R

   Compute the QR factorization of ``A`` such that ``A = Q*R``. Also see ``qrd``.

.. function:: qrfact(A)

   Compute the QR factorization of ``A`` and return a ``QRDense`` object. The coomponents of the factorization ``F`` can be accessed as follows: the orthogonal matrix ``Q`` can be extracted with ``F[:Q]`` and the triangular matrix ``R`` with ``F[:R]``. The following functions are available for ``QRDense`` objects: ``size``, ``\``. When ``Q`` is extracted, the resulting type is the ``QRDenseQ`` object, and has the ``*`` operator overloaded to support efficient multiplication by ``Q`` and ``Q'``.

.. function:: qrfact!(A)

   ``qrfact!`` is the same as ``qrfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qrp(A) -> Q, R, P

   Compute the QR factorization of ``A`` with pivoting, such that ``A*I[:,P] = Q*R``, where ``I`` is the identity matrix. Also see ``qrpfact``.

.. function:: qrpfact(A) -> QRPivotedDense

   Compute the QR factorization of ``A`` with pivoting and return a ``QRDensePivoted`` object. The coomponents of the factorization ``F`` can be accessed as follows: the orthogonal matrix ``Q`` can be extracted with ``F[:Q]``, the triangular matrix ``R`` with ``F[:R]``, and the permutation with ``F[:P]`` or ``F[:p]``. The following functions are available for ``QRDensePivoted`` objects: ``size``, ``\``. When ``Q`` is extracted, the resulting type is the ``QRDenseQ`` object, and has the ``*`` operator overloaded to support efficient multiplication by ``Q`` and ``Q'``.

.. function:: qrpfact!(A) -> QRPivotedDense

   ``qrpfact!`` is the same as ``qrpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then ``B*B == A`` within roundoff error.

.. function:: eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

.. function:: eigvals(A)

   Returns the eigenvalues of ``A``.

.. function:: svdfact(A, [thin]) -> SVDDense

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVDDense`` object. ``U``, ``S``, and ``Vt`` can be obtained from the factorization ``F`` with ``F[:U]``, ``F[:S]``, and ``F[:V]``, such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdfact!(A, [thin]) -> SVDDense

   ``svdfact!`` is the same as ``svdfact`` but saves space by overwriting the input A, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svd(A, [thin]) -> U, S, V

   Compute the SVD of A, returning ``U``, vector ``S``, and ``V`` such that ``A == U*diagm(S)*V'``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdt(A, [thin]) -> U, S, Vt

   Compute the SVD of A, returning ``U``, vector ``S``, and ``Vt`` such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdvals(A)

   Returns the singular values of ``A``.

.. function:: svdvals!(A)

   Returns the singular values of ``A``, while saving space by overwriting the input.

.. function:: svdfact(A, B) -> GSVDDense

   Compute the generalized SVD of ``A`` and ``B``, returning a ``GSVDDense`` Factorization object, such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.
   
.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   Compute the generalized SVD of ``A`` and ``B``, returning ``U``, ``V``, ``Q``, ``D1``, ``D2``, and ``R0`` such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.
 
.. function:: svdvals(A, B)

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``.

.. function:: triu(M)

   Upper triangle of a matrix

.. function:: tril(M)

   Lower triangle of a matrix

.. function:: diag(M, [k])

   The ``k``-th diagonal of a matrix, as a vector

.. function:: diagm(v, [k])

   Construct a diagonal matrix and place ``v`` on the ``k``-th diagonal

.. function:: diagmm(matrix, vector)

   Multiply matrices, interpreting the vector argument as a diagonal matrix.
   The arguments may occur in the other order to multiply with the diagonal
   matrix on the left.

.. function:: Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal

.. function:: Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury matrix identity

.. function:: rank(M)

   Compute the rank of a matrix

.. function:: norm(A, [p])

   Compute the ``p``-norm of a vector or a matrix. ``p`` is ``2`` by default, if not provided. If ``A`` is a vector, ``norm(A, p)`` computes the ``p``-norm. ``norm(A, Inf)`` returns the largest value in ``abs(A)``, whereas ``norm(A, -Inf)`` returns the smallest. If ``A`` is a matrix, valid values for ``p`` are ``1``, ``2``, or ``Inf``. In order to compute the Frobenius norm, use ``normfro``.

.. function:: normfro(A)

   Compute the Frobenius norm of a matrix ``A``.

.. function:: cond(M, [p])

   Matrix condition number, computed using the p-norm. ``p`` is 2 by default, if not provided. Valid values for ``p`` are ``1``, ``2``, or ``Inf``.

.. function:: trace(M)

   Matrix trace

.. function:: det(M)

   Matrix determinant

.. function:: inv(M)

   Matrix inverse

.. function:: pinv(M)

   Moore-Penrose inverse

.. function:: null(M)

   Basis for null space of M.

.. function:: repmat(A, n, m)

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.

.. function:: kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

.. function:: linreg(x, y)

   Determine parameters ``[a, b]`` that minimize the squared error between ``y`` and ``a+b*x``.

.. function:: linreg(x, y, w)

   Weighted least-squares linear regression.

.. function:: expm(A)

   Matrix exponential.

.. function:: issym(A)

   Test whether a matrix is symmetric.

.. function:: isposdef(A)

   Test whether a matrix is positive-definite.

.. function:: istril(A)

   Test whether a matrix is lower-triangular.

.. function:: istriu(A)

   Test whether a matrix is upper-triangular.

.. function:: ishermitian(A)

   Test whether a matrix is hermitian.

.. function:: transpose(A)

   The transpose operator (.').

.. function:: ctranspose(A)

   The conjugate transpose operator (').


BLAS Functions
--------------

This module provides wrappers for some of the BLAS functions for
linear algebra.  Those BLAS functions that overwrite one of the input
arrays have names ending in ``'!'``.

Usually a function has 4 methods defined, one each for ``Float64``,
``Float32``, ``Complex128`` and ``Complex64`` arrays.

.. function:: copy!(n, X, incx, Y, incy)

   Copy ``n`` elements of array ``X`` with stride ``incx`` to array
   ``Y`` with stride ``incy``.  Returns ``Y``.

.. function:: dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of ``n`` elements of array
   ``X`` with stride ``incx`` and ``n`` elements of array ``Y`` with
   stride ``incy``.  There are no ``dot`` methods for ``Complex``
   arrays.

.. function:: nrm2(n, X, incx)

   2-norm of a vector consisting of ``n`` elements of array ``X`` with
   stride ``incx``.

.. function:: axpy!(n, a, X, incx, Y, incy)

   Overwrite ``Y`` with ``a*X + Y``.  Returns ``Y``.

.. function:: syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix ``C`` as ``alpha*A*A.' +
   beta*C`` or ``alpha*A.'*A + beta*C`` according to whether ``trans``
   is 'N' or 'T'.  When ``uplo`` is 'U' the upper triangle of ``C`` is
   updated ('L' for lower triangle).  Returns ``C``.

.. function:: syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to ``uplo`` ('U' or 'L'), of ``alpha*A*A.'`` or ``alpha*A.'*A``,
   according to ``trans`` ('N' or 'T').

.. function:: herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix ``C`` as ``alpha*A*A' + beta*C`` or ``alpha*A'*A + beta*C``
   according to whether ``trans`` is 'N' or 'T'.  When ``uplo`` is 'U'
   the upper triangle of ``C`` is updated ('L' for lower triangle).
   Returns ``C``.

.. function:: herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to ``uplo`` ('U' or 'L'), of
   ``alpha*A*A'`` or ``alpha*A'*A``, according to ``trans`` ('N' or 'T').

.. function:: gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector ``y`` as ``alpha*A*x + beta*y`` or ``alpha*A'*x +
   beta*y`` according to ``trans`` ('N' or 'T').  The matrix ``A`` is
   a general band matrix of dimension ``m`` by ``size(A,2)`` with
   ``kl`` sub-diagonals and ``ku`` super-diagonals. Returns the
   updated ``y``.

.. function:: gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns ``alpha*A*x`` or ``alpha*A'*x`` according to ``trans`` ('N'
   or 'T'). The matrix ``A`` is a general band matrix of dimension
   ``m`` by ``size(A,2)`` with ``kl`` sub-diagonals and
   ``ku`` super-diagonals.

.. function:: sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector ``y`` as ``alpha*A*x + beta*y`` where ``A`` is a 
   a symmetric band matrix of order ``size(A,2)`` with
   ``k`` super-diagonals stored in the argument ``A``.  The storage
   layout for ``A`` is described the reference BLAS module, level-2
   BLAS at `<http://www.netlib.org/lapack/explore-html/>`.

   Returns the updated ``y``.

.. function:: sbmv(uplo, k, alpha, A, x)

   Returns ``alpha*A*x`` where ``A`` is a symmetric band matrix of
   order ``size(A,2)`` with ``k`` super-diagonals stored in the
   argument ``A``.

.. function:: gemm!(tA, tB, alpha, A, B, beta, C)

   Update ``C`` as ``alpha*A*B + beta*C`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.  Returns the
   updated ``C``.

.. function:: gemm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.

