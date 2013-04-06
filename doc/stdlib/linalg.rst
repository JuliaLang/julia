Linear Algebra
--------------

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.  Sparse factorizations call functions from `SuiteSparse <http:://www.suitesparse.com/>`_.

.. function:: *(A, B)

   Matrix multiplication

.. function:: \\(A, B)

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B`` when ``A`` is square.  The solver that is used depends upon the structure of ``A``.  A direct solver is used for upper- or lower triangular ``A``.  For Hermitian ``A`` (equivalent to symmetric ``A`` for non-complex ``A``) the BunchKaufman factorization is used.  Otherwise an LU factorization is used. For rectangular ``A`` the result is the minimum-norm least squares solution computed by reducing ``A`` to bidiagonal form and solving the bidiagonal least squares problem.  For sparse, square ``A`` the LU factorization (from UMFPACK) is used.

.. function:: dot(x, y)

   Compute the dot product

.. function:: cross(x, y)

   Compute the cross product of two 3-vectors

.. function:: norm(a)

   Compute the norm of a ``Vector`` or a ``Matrix``

.. function:: lu(A) -> L, U, P

   Compute the LU factorization of ``A``, such that ``P*A = L*U``.

.. function:: lufact(A) -> LU

   Compute the LU factorization of ``A``, returning an ``LU`` object for dense ``A`` or an ``UmfpackLU`` object for sparse ``A``. The individual components of the factorization ``F`` can be accesed by indexing: ``F[:L]``, ``F[:U]``, and ``F[:P]`` (permutation matrix) or ``F[:p]`` (permutation vector). An ``UmfpackLU`` object has additional components ``F[:q]`` (the left permutation vector) and ``Rs`` the vector of scaling factors. The following functions are available for both ``LU`` and ``UmfpackLU`` objects: ``size``, ``\`` and ``det``.  For ``LU`` there is also an ``inv`` method.  The sparse LU factorization is such that ``L*U`` is equal to``diagmm(Rs,A)[p,q]``.

.. function:: lufact!(A) -> LU

   ``lufact!`` is the same as ``lufact`` but saves space by overwriting the input A, instead of creating a copy.  For sparse ``A`` the ``nzval`` field is not overwritten but the index fields, ``colptr`` and ``rowval`` are decremented in place, converting from 1-based indices to 0-based indices.

.. function:: chol(A, [LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``L`` (Lower), ``A = L*L'``. If ``LU`` is ``U`` (Upper), ``A = R'*R``.

.. function:: cholfact(A, [LU]) -> Cholesky

   Compute the Cholesky factorization of a dense symmetric positive-definite matrix ``A`` and return a ``Cholesky`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. The triangular matrix can be obtained from the factorization ``F`` with: ``F[:L]`` and ``F[:U]``. The following functions are available for ``Cholesky`` objects: ``size``, ``\``, ``inv``, ``det``. A ``LAPACK.PosDefException`` error is thrown in case the matrix is not positive definite.

.. function:: cholfact(A, [ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix ``A``.  If ``A`` is Hermitian its Cholesky factor is determined.  If ``A`` is not Hermitian the Cholesky factor of ``A*A'`` is determined. A fill-reducing permutation is used.  Methods for ``size``, ``solve``, ``\``, ``findn_nzs``, ``diag``, ``det`` and ``logdet``.  One of the solve methods includes an integer argument that can be used to solve systems involving parts of the factorization only.  The optional boolean argument, ``ll`` determines whether the factorization returned is of the ``A[p,p] = L*L'`` form, where ``L`` is lower triangular or ``A[p,p] = diagmm(L,D)*L'`` form where ``L`` is unit lower triangular and ``D`` is a non-negative vector.  The default is LDL.

.. function:: cholfact!(A, [LU]) -> Cholesky

   ``cholfact!`` is the same as ``cholfact`` but saves space by overwriting the input A, instead of creating a copy.

..  function:: cholpfact(A, [LU]) -> CholeskyPivoted

   Compute the pivoted Cholesky factorization of a symmetric positive semi-definite matrix ``A`` and return a ``CholeskyPivoted`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. The triangular factors containted in the factorization ``F`` can be obtained with ``F[:L]`` and ``F[:U]``, whereas the permutation can be obtained with ``F[:P]`` or ``F[:p]``. The following functions are available for ``CholeskyPivoted`` objects: ``size``, ``\``, ``inv``, ``det``. A ``LAPACK.RankDeficientException`` error is thrown in case the matrix is rank deficient.

.. function:: cholpfact!(A, [LU]) -> CholeskyPivoted

   ``cholpfact!`` is the same as ``cholpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qr(A, [thin]) -> Q, R

   Compute the QR factorization of ``A`` such that ``A = Q*R``. Also see ``qrfact``. The default is to compute a thin factorization.

.. function:: qrfact(A)

   Compute the QR factorization of ``A`` and return a ``QR`` object. The coomponents of the factorization ``F`` can be accessed as follows: the orthogonal matrix ``Q`` can be extracted with ``F[:Q]`` and the triangular matrix ``R`` with ``F[:R]``. The following functions are available for ``QR`` objects: ``size``, ``\``. When ``Q`` is extracted, the resulting type is the ``QRPackedQ`` object, and has the ``*`` operator overloaded to support efficient multiplication by ``Q`` and ``Q'``.

.. function:: qrfact!(A)

   ``qrfact!`` is the same as ``qrfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qrp(A, [thin]) -> Q, R, P

   Compute the QR factorization of ``A`` with pivoting, such that ``A*P = Q*R``, Also see ``qrpfact``. The default is to compute a thin factorization.

.. function:: qrpfact(A) -> QRPivoted

   Compute the QR factorization of ``A`` with pivoting and return a ``QRPivoted`` object. The components of the factorization ``F`` can be accessed as follows: the orthogonal matrix ``Q`` can be extracted with ``F[:Q]``, the triangular matrix ``R`` with ``F[:R]``, and the permutation with ``F[:P]`` or ``F[:p]``. The following functions are available for ``QRPivoted`` objects: ``size``, ``\``. When ``Q`` is extracted, the resulting type is the ``QRPivotedQ`` object, and has the ``*`` operator overloaded to support efficient multiplication by ``Q`` and ``Q'``. A ``QRPivotedQ`` matrix can be converted into a regular matrix with ``full``.

.. function:: qrpfact!(A) -> QRPivoted

   ``qrpfact!`` is the same as ``qrpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then ``B*B == A`` within roundoff error.

.. function:: eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

.. function:: eigvals(A)

   Returns the eigenvalues of ``A``.

.. function:: eigmax(A)

   Returns the largest eigenvalue of ``A``.

.. function:: eigmin(A)

   Returns the smallest eigenvalue of ``A``.

.. function:: eigvecs(A, [eigvals])

   Returns the eigenvectors of ``A``.

   For SymTridiagonal matrices, if the optional vector of eigenvalues ``eigvals`` is specified, returns the specific corresponding eigenvectors.

.. function:: eigfact(A)

   Compute the eigenvalue decomposition of ``A`` and return an ``Eigen`` object. If ``F`` is the factorization object, the eigenvalues can be accessed with ``F[:values]`` and the eigenvectors with ``F[:vectors]``. The following functions are available for ``Eigen`` objects: ``inv``, ``det``.

.. function:: eigfact!(A)

   ``eigfact!`` is the same as ``eigfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: hessfact(A)

   Compute the Hessenberg decomposition of ``A`` and return a ``Hessenberg`` object. If ``F`` is the factorization object, the unitary matrix can be accessed with ``F[:Q]`` and the Hessenberg matrix with ``F[:H]``. When ``Q`` is extracted, the resulting type is the ``HessenbergQ`` object, and may be converted to a regular matrix with ``full``.

.. function:: hessfact!(A)

   ``hessfact!`` is the same as ``hessfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: schurfact(A) -> Schur

   Computes the Schur factorization of the matrix ``A``. The (quasi) triangular Schur factor can be obtained from the ``Schur`` object ``F`` with either ``F[:Schur]`` or ``F[:T]`` and the unitary/orthogonal Schur vectors can be obtained with ``F[:vectors]`` or ``F[:Z]`` such that ``A=F[:vectors]*F[:Schur]*F[:vectors]'``. The eigenvalues of ``A`` can be obtained with ``F[:values]``.

.. function:: schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   See schurfact

.. function:: schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``. The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and ``F[:T]``, the left unitary/orthogonal Schur vectors can be obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and ``B=F[:left]*F[:T]*F[:right]'``. The generalized eigenvalues of ``A`` and ``B`` can be obtained with ``F[:alpha]./F[:beta]``.

.. function:: schur(A,B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See schurfact

.. function:: svdfact(A, [thin]) -> SVD

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVD`` object. ``U``, ``S``, ``V`` and ``Vt`` can be obtained from the factorization ``F`` with ``F[:U]``, ``F[:S]``, ``F[:V]`` and ``F[:Vt]``, such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned. The algorithm produces ``Vt`` and hence ``Vt`` is more efficient to extract than ``V``. The default is to produce a thin decomposition.

.. function:: svdfact!(A, [thin]) -> SVD

   ``svdfact!`` is the same as ``svdfact`` but saves space by overwriting the input A, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned. The default is to produce a thin decomposition.

.. function:: svd(A, [thin]) -> U, S, V

   Compute the SVD of A, returning ``U``, vector ``S``, and ``V`` such that ``A == U*diagm(S)*V'``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdvals(A)

   Returns the singular values of ``A``.

.. function:: svdvals!(A)

   Returns the singular values of ``A``, while saving space by overwriting the input.

.. function:: svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of ``A`` and ``B``, returning a ``GeneralizedSVD`` Factorization object, such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.
   
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

.. function:: Bidiagonal(dv, ev, isupper)

   Constructs an upper (isupper=true) or lower (isupper=false) bidiagonal matrix
   using the given diagonal (dv) and off-diagonal (ev) vectors

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

