.. _stdlib-linalg:

Linear Algebra
--------------

.. module:: Base.LinAlg

.. currentmodule:: Base

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.  Sparse factorizations call functions from `SuiteSparse <http://www.suitesparse.com/>`_.

.. function:: *(A, B)
   :noindex:

   Matrix multiplication

.. function:: \\(A, B)
   :noindex:

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B`` when ``A`` is square.  The solver that is used depends upon the structure of ``A``.  A direct solver is used for upper- or lower triangular ``A``.  For Hermitian ``A`` (equivalent to symmetric ``A`` for non-complex ``A``) the BunchKaufman factorization is used.  Otherwise an LU factorization is used. For rectangular ``A`` the result is the minimum-norm least squares solution computed by reducing ``A`` to bidiagonal form and solving the bidiagonal least squares problem.  For sparse, square ``A`` the LU factorization (from UMFPACK) is used.

.. function:: dot(x, y)

   Compute the dot product. For complex vectors the first vector is conjugated.

.. function:: cross(x, y)

   Compute the cross product of two 3-vectors.

.. function:: rref(A)

   Compute the reduced row echelon form of the matrix A.

.. function:: factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch-Kaufman, Triangular) of A, based upon the type of the input matrix. The return value can then be reused for efficient solving of multiple systems. For example: ``A=factorize(A); x=A\\b; y=A\\C``.

.. function:: factorize!(A)

   ``factorize!`` is the same as :func:`factorize`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: lu(A) -> L, U, p

   Compute the LU factorization of ``A``, such that ``A[p,:] = L*U``.

.. function:: lufact(A) -> LU

   Compute the LU factorization of ``A``, returning an ``LU`` object for dense ``A`` or an ``UmfpackLU`` object for sparse ``A``. The individual components of the factorization ``F`` can be accesed by indexing: ``F[:L]``, ``F[:U]``, and ``F[:P]`` (permutation matrix) or ``F[:p]`` (permutation vector). An ``UmfpackLU`` object has additional components ``F[:q]`` (the left permutation vector) and ``Rs`` the vector of scaling factors. The following functions are available for both ``LU`` and ``UmfpackLU`` objects: ``size``, ``\`` and ``det``.  For ``LU`` there is also an ``inv`` method.  The sparse LU factorization is such that ``L*U`` is equal to``scale(Rs,A)[p,q]``.

.. function:: lufact!(A) -> LU

   ``lufact!`` is the same as :func:`lufact`, but saves space by overwriting the input A, instead of creating a copy.  For sparse ``A`` the ``nzval`` field is not overwritten but the index fields, ``colptr`` and ``rowval`` are decremented in place, converting from 1-based indices to 0-based indices.

.. function:: chol(A, [LU]) -> F

   Compute the Cholesky factorization of a symmetric positive definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``:L`` (Lower), ``A = L*L'``. If ``LU`` is ``:U`` (Upper), ``A = R'*R``.

.. function:: cholfact(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   Compute the Cholesky factorization of a dense symmetric positive (semi)definite matrix ``A`` and return either a ``Cholesky`` if ``pivot=false`` or ``CholeskyPivoted`` if ``pivot=true``. ``LU`` may be ``:L`` for using the lower part or ``:U`` for the upper part. The default is to use ``:U``. The triangular matrix can be obtained from the factorization ``F`` with: ``F[:L]`` and ``F[:U]``. The following functions are available for ``Cholesky`` objects: ``size``, ``\``, ``inv``, ``det``. For ``CholeskyPivoted`` there is also defined a ``rank``. If ``pivot=false`` a ``PosDefException`` exception is thrown in case the matrix is not positive definite. The argument ``tol`` determines the tolerance for determining the rank. For negative values, the tolerance is the machine precision.

.. function:: cholfact(A, [ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix ``A``.  If ``A`` is Hermitian its Cholesky factor is determined.  If ``A`` is not Hermitian the Cholesky factor of ``A*A'`` is determined. A fill-reducing permutation is used.  Methods for ``size``, ``solve``, ``\``, ``findn_nzs``, ``diag``, ``det`` and ``logdet``.  One of the solve methods includes an integer argument that can be used to solve systems involving parts of the factorization only.  The optional boolean argument, ``ll`` determines whether the factorization returned is of the ``A[p,p] = L*L'`` form, where ``L`` is lower triangular or ``A[p,p] = scale(L,D)*L'`` form where ``L`` is unit lower triangular and ``D`` is a non-negative vector.  The default is LDL.

.. function:: cholfact!(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   ``cholfact!`` is the same as :func:`cholfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: qr(A, [pivot=false,][thin=true]) -> Q, R, [p]

   Compute the (pivoted) QR factorization of ``A`` such that either ``A = Q*R`` or ``A[:,p] = Q*R``. Also see ``qrfact``. The default is to compute a thin factorization. Note that ``R`` is not extended with zeros when the full ``Q`` is requested. 

.. function:: qrfact(A,[pivot=false])

   Computes the QR factorization of ``A`` and returns either a ``QR`` type if ``pivot=false`` or ``QRPivoted`` type if ``pivot=true``. From a ``QR`` or  ``QRPivoted`` factorization ``F``, an orthogonal matrix ``F[:Q]`` and a triangular matrix ``F[:R]`` can be extracted. For ``QRPivoted`` it is also posiible to extract the permutation vector ``F[:p]`` or matrix ``F[:P]``.
   The following functions are available for the ``QR`` objects: ``size``, ``\``. When ``A`` is rectangular ``\`` will return a least squares solution and if the soultion is not unique, the one with smallest norm is returned.
   The orthogonal matrix ``Q=F[:Q]`` is a ``QRPackedQ`` type when ``F`` is a ``QR`` and a ``QRPivotedQ`` then ``F`` is a ``QRPivoted``. Both have the ``*`` operator overloaded to support efficient multiplication by ``Q`` and ``Q'``. Multiplication with respect to either thin or full ``Q`` is allowed, i.e. both ``F[:Q]*F[:R]`` and ``F[:Q]*A`` are supported. A ``Q`` matrix can be converted into a regular matrix with ``full`` which has a named argument ``thin``.

.. function:: qrfact!(A,[pivot=false])

   ``qrfact!`` is the same as :func:`qrfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: bkfact(A) -> BunchKaufman

   Compute the Bunch-Kaufman factorization of a real symmetric or complex Hermitian matrix ``A`` and return a ``BunchKaufman`` object. The following functions are available for ``BunchKaufman`` objects: ``size``, ``\``, ``inv``, ``issym``, ``ishermitian``.

.. function:: bkfact!(A) -> BunchKaufman

   ``bkfact!`` is the same as :func:`bkfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then ``B*B == A`` within roundoff error.

   ``sqrtm`` uses a polyalgorithm, computing the matrix square root using Schur factorizations (:func:`schurfact`) unless it detects the matrix to be Hermitian or real symmetric, in which case it computes the matrix square root from an eigendecomposition (:func:`eigfact`). In the latter situation for positive definite matrices, the matrix square root has ``Real`` elements, otherwise it has ``Complex`` elements.

.. function:: eig(A,[balance=:balance]) -> D, V

   Compute eigenvalues and eigenvectors of ``A``. See :func:`eigfact` for details on the ``balance`` keyword argument.

.. function:: eig(A, B) -> D, V

   Compute generalized eigenvalues and vectors of ``A`` with respect to ``B``.

.. function:: eigvals(A)

   Returns the eigenvalues of ``A``.

.. function:: eigmax(A)

   Returns the largest eigenvalue of ``A``.

.. function:: eigmin(A)

   Returns the smallest eigenvalue of ``A``.

.. function:: eigvecs(A, [eigvals,][balance=:balance])

   Returns the eigenvectors of ``A``.
   The ``balance`` keyword is the same as for :func:`eigfact`.

   For ``SymTridiagonal`` matrices, if the optional vector of eigenvalues ``eigvals`` is specified, returns the specific corresponding eigenvectors.

.. function:: eigfact(A,[balance=:balance])

   Compute the eigenvalue decomposition of ``A`` and return an ``Eigen`` object. If ``F`` is the factorization object, the eigenvalues can be accessed with ``F[:values]`` and the eigenvectors with ``F[:vectors]``. The following functions are available for ``Eigen`` objects: ``inv``, ``det``.
   
   For general non-symmetric matrices it is possible to specify how the matrix is balanced before the eigenvector calculation. Possible values are: ``:nobalance`` (do not balance), ``:permute`` (permute the matrix to become closer to upper triangular), ``:diagonal`` (scale the matrix by its diagonal elements to make rows and columns more equal in norm), and ``:balance`` (The default, i.e. both permute and scale the matrix).

.. function:: eigfact(A, B)

   Compute the generalized eigenvalue decomposition of ``A`` and ``B`` and return an ``GeneralizedEigen`` object. If ``F`` is the factorization object, the eigenvalues can be accessed with ``F[:values]`` and the eigenvectors with ``F[:vectors]``.

.. function:: eigfact!(A, [B])

   ``eigfact!`` is the same as :func:`eigfact`, but saves space by overwriting the input A (and B), instead of creating a copy.

.. function:: hessfact(A)

   Compute the Hessenberg decomposition of ``A`` and return a ``Hessenberg`` object. If ``F`` is the factorization object, the unitary matrix can be accessed with ``F[:Q]`` and the Hessenberg matrix with ``F[:H]``. When ``Q`` is extracted, the resulting type is the ``HessenbergQ`` object, and may be converted to a regular matrix with ``full``.

.. function:: hessfact!(A)

   ``hessfact!`` is the same as :func:`hessfact`, but saves space by overwriting the input A, instead of creating a copy.

.. function:: schurfact(A) -> Schur

   Computes the Schur factorization of the matrix ``A``. The (quasi) triangular Schur factor can be obtained from the ``Schur`` object ``F`` with either ``F[:Schur]`` or ``F[:T]`` and the unitary/orthogonal Schur vectors can be obtained with ``F[:vectors]`` or ``F[:Z]`` such that ``A=F[:vectors]*F[:Schur]*F[:vectors]'``. The eigenvalues of ``A`` can be obtained with ``F[:values]``.

.. function:: schurfact!(A)

   Computer the Schur factorization of ``A``, overwriting ``A`` in the process. See :func:`schurfact`

.. function:: schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   See :func:`schurfact`

.. function:: schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``. The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and ``F[:T]``, the left unitary/orthogonal Schur vectors can be obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and ``B=F[:left]*F[:T]*F[:right]'``. The generalized eigenvalues of ``A`` and ``B`` can be obtained with ``F[:alpha]./F[:beta]``.

.. function:: schur(A,B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See :func:`schurfact`

.. function:: svdfact(A, [thin]) -> SVD

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVD`` object. ``U``, ``S``, ``V`` and ``Vt`` can be obtained from the factorization ``F`` with ``F[:U]``, ``F[:S]``, ``F[:V]`` and ``F[:Vt]``, such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned. The algorithm produces ``Vt`` and hence ``Vt`` is more efficient to extract than ``V``. The default is to produce a thin decomposition.

.. function:: svdfact!(A, [thin]) -> SVD

   ``svdfact!`` is the same as :func:`svdfact`, but saves space by overwriting the input A, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned. The default is to produce a thin decomposition.

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

   Upper triangle of a matrix.

.. function:: triu!(M)

   Upper triangle of a matrix, overwriting M in the process.

.. function:: tril(M)

   Lower triangle of a matrix.

.. function:: tril!(M)

   Lower triangle of a matrix, overwriting M in the process.

.. function:: diagind(M[, k])

   A ``Range`` giving the indices of the ``k``-th diagonal of the matrix ``M``.

.. function:: diag(M[, k])

   The ``k``-th diagonal of a matrix, as a vector.

.. function:: diagm(v[, k])

   Construct a diagonal matrix and place ``v`` on the ``k``-th diagonal.

.. function:: scale(A, b), scale(b, A)

   Scale an array ``A`` by a scalar ``b``, returning a new array.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)``
   scales each column ``i`` of ``A`` by ``b[i]`` (similar to
   ``A*diagm(b)``), while ``scale!(b,A)`` scales each row ``i`` of
   ``A`` by ``b[i]`` (similar to ``diagm(b)*A``), returning a new array.

   Note: for large ``A``, ``scale`` can be much faster than ``A .* b`` or
   ``b .* A``, due to the use of BLAS.

.. function:: scale!(A, b), scale!(b, A)

   Scale an array ``A`` by a scalar ``b``, similar to ``scale`` but
   overwriting ``A`` in-place.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)``
   scales each column ``i`` of ``A`` by ``b[i]`` (similar to
   ``A*diagm(b)``), while ``scale!(b,A)`` scales each row ``i`` of
   ``A`` by ``b[i]`` (similar to ``diagm(b)*A``), again operating in-place
   on ``A``.

.. function:: Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal, respectively.  The result is of type ``Tridiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with ``full``.

.. function:: Bidiagonal(dv, ev, isupper)

   Constructs an upper (isupper=true) or lower (isupper=false) bidiagonal matrix
   using the given diagonal (dv) and off-diagonal (ev) vectors.  The result is of type ``Bidiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with ``full``.

.. function:: SymTridiagonal(d, du)

   Construct a real symmetric tridiagonal matrix from the diagonal and upper diagonal, respectively. The result is of type ``SymTridiagonal`` and provides efficient specialized eigensolvers, but may be converted into a regular matrix with ``full``.

.. function:: Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury matrix identity.

.. function:: rank(M)

   Compute the rank of a matrix.

.. function:: norm(A, [p])

   Compute the ``p``-norm of a vector or the operator norm of a matrix ``A``, defaulting to the ``p=2``-norm.

   For vectors, ``p`` can assume any numeric value (even though not all values produce a mathematically valid vector norm). In particular, `norm(A, Inf)`` returns the largest value in ``abs(A)``, whereas ``norm(A, -Inf)`` returns the smallest.

   For matrices, valid values of ``p`` are ``1``, ``2``, or ``Inf``. Use :func:`normfro` to compute the Frobenius norm.

.. function:: normfro(A)

   Compute the Frobenius norm of a matrix ``A``.

.. function:: cond(M, [p])

   Matrix condition number, computed using the ``p``-norm. ``p`` is 2 by default, if not provided. Valid values for ``p`` are ``1``, ``2``, or ``Inf``.

.. function:: trace(M)

   Matrix trace

.. function:: det(M)

   Matrix determinant

.. function:: logdet(M)

   Log of matrix determinant. Equivalent to ``log(det(M))``, but may provide increased accuracy and/or speed.

.. function:: inv(M)

   Matrix inverse

.. function:: pinv(M)

   Moore-Penrose pseudoinverse

.. function:: null(M)

   Basis for nullspace of ``M``.

.. function:: repmat(A, n, m)

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.

.. function:: repeat(A, inner = Int[], outer = Int[])

   Construct an array by repeating the entries of ``A``. The i-th element of ``inner`` specifies the number of times that the individual entries of the i-th dimension of ``A`` should be repeated. The i-th element of ``outer`` specifies the number of times that a slice along the i-th dimension of ``A`` should be repeated.

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

   Test whether a matrix is positive definite.

.. function:: isposdef!(A)

   Test whether a matrix is positive definite, overwriting ``A`` in the processes.

.. function:: istril(A)

   Test whether a matrix is lower triangular.

.. function:: istriu(A)

   Test whether a matrix is upper triangular.

.. function:: ishermitian(A)

   Test whether a matrix is Hermitian.

.. function:: transpose(A)

   The transposition operator (``.'``).

.. function:: ctranspose(A)

   The conjugate transposition operator (``'``).

.. function:: eigs(A; nev=6, which="LM", tol=0.0, maxiter=1000, sigma=0, ritzvec=true, op_part=:real,v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

   ``eigs`` computes eigenvalues ``d`` of A using Arnoldi factorization. The following keyword arguments are supported:
    * ``nev``: Number of eigenvalues
    * ``which``: type of eigenvalues ("LM", "SM")
    * ``tol``: tolerance (:math:`tol \le 0.0` defaults to ``DLAMCH('EPS')``)
    * ``maxiter``: Maximum number of iterations
    * ``sigma``: find eigenvalues close to ``sigma`` using shift and invert
    * ``ritzvec``: Returns the Ritz vectors ``v`` (eigenvectors) if ``true``
    * ``op_part``: which part of linear operator to use for real A (:real, :imag)
    * ``v0``: starting vector from which to start the Arnoldi iteration
   ``eigs`` returns the ``nev`` requested eigenvalues in ``d``, the corresponding Ritz vectors ``v`` (only if ``ritzvec=true``), the number of converged eigenvalues ``nconv``, the number of iterations ``niter`` and the number of matrix vector multiplications ``nmult``, as well as the final residual vector ``resid``.
    

.. function:: svds(A; nev=6, which="LA", tol=0.0, maxiter=1000, ritzvec=true)

   ``svds`` computes the singular values of A using Arnoldi factorization. The following keyword arguments are supported:
    * ``nsv``: Number of singular values
    * ``which``: type of singular values ("LA")
    * ``tol``: tolerance (:math:`tol \le 0.0` defaults to ``DLAMCH('EPS')``)
    * ``maxiter``: Maximum number of iterations
    * ``ritzvec``: Returns the singular vectors if ``true``

.. function:: peakflops(n; parallel=false)

   ``peakflops`` computes the peak flop rate of the computer by using BLAS double precision :func:`gemm!`. By default, if no arguments are specified, it multiplies a matrix of size ``n x n``, where ``n = 2000``. If the underlying BLAS is using multiple threads, higher flop rates are realized. The number of BLAS threads can be set with ``blas_set_num_threads(n)``.

   If the keyword argument ``parallel`` is set to ``true``, ``peakflops`` is run in parallel on all the worker processors. The flop rate of the entire parallel computer is returned. When running in parallel, only 1 BLAS thread is used. The argument ``n`` still refers to the size of the problem that is solved on each processor.

BLAS Functions
--------------

.. module:: Base.LinAlg.BLAS

This module provides wrappers for some of the BLAS functions for
linear algebra.  Those BLAS functions that overwrite one of the input
arrays have names ending in ``'!'``.

Usually a function has 4 methods defined, one each for ``Float64``,
``Float32``, ``Complex128`` and ``Complex64`` arrays.

.. currentmodule:: Base.LinAlg.BLAS

.. function:: dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of ``n`` elements of array
   ``X`` with stride ``incx`` and ``n`` elements of array ``Y`` with
   stride ``incy``.

.. function:: dotu(n, X, incx, Y, incy)

   Dot function for two complex vectors.

.. function:: dotc(n, X, incx, U, incy)

   Dot function for two complex vectors conjugating the first vector.

.. function:: blascopy!(n, X, incx, Y, incy)

   Copy ``n`` elements of array ``X`` with stride ``incx`` to array
   ``Y`` with stride ``incy``.  Returns ``Y``.

.. function:: nrm2(n, X, incx)

   2-norm of a vector consisting of ``n`` elements of array ``X`` with
   stride ``incx``.

.. function:: asum(n, X, incx)

   sum of the absolute values of the first ``n`` elements of array ``X`` with
   stride ``incx``.

.. function:: axpy!(n, a, X, incx, Y, incy)

   Overwrite ``Y`` with ``a*X + Y``.  Returns ``Y``.

.. function:: scal!(n, a, X, incx)

   Overwrite ``X`` with ``a*X``.  Returns ``X``.

.. function:: scal(n, a, X, incx)

   Returns ``a*X``.

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
   BLAS at http://www.netlib.org/lapack/explore-html/.

   Returns the updated ``y``.

.. function:: sbmv(uplo, k, alpha, A, x)

   Returns ``alpha*A*x`` where ``A`` is a symmetric band matrix of
   order ``size(A,2)`` with ``k`` super-diagonals stored in the
   argument ``A``.

.. function:: sbmv(uplo, k, A, x)

   Returns ``A*x`` where ``A`` is a symmetric band matrix of
   order ``size(A,2)`` with ``k`` super-diagonals stored in the
   argument ``A``.

.. function:: gemm!(tA, tB, alpha, A, B, beta, C)

   Update ``C`` as ``alpha*A*B + beta*C`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.  Returns the
   updated ``C``.

.. function:: gemm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.

.. function:: gemm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.

.. function:: gemv!(tA, alpha, A, x, beta, y)

   Update the vector ``y`` as ``alpha*A*x + beta*x`` or
   ``alpha*A'x + beta*x`` according to ``tA`` (transpose ``A``).
   Returns the updated ``y``.

.. function:: gemv(tA, alpha, A, x)

   Returns ``alpha*A*x`` or ``alpha*A'x`` according to ``tA``
   (transpose ``A``).

.. function:: gemv(tA, alpha, A, x)

   Returns ``A*x`` or ``A'x`` according to ``tA`` (transpose ``A``).

.. function:: symm!(side, ul, alpha, A, B, beta, C)

   Update ``C`` as ``alpha*A*B + beta*C`` or ``alpha*B*A + beta*C``
   according to ``side``. ``A`` is assumed to be symmetric.  Only the
   ``ul`` triangle of ``A`` is used.  Returns the updated ``C``.

.. function:: symm(side, ul, alpha, A, B)

   Returns ``alpha*A*B`` or ``alpha*B*A`` according to ``side``.
   ``A`` is assumed to be symmetric.  Only the ``ul`` triangle of
   ``A`` is used.

.. function:: symm(side, ul, A, B)

   Returns ``A*B`` or ``B*A`` according to ``side``.  ``A`` is assumed
   to be symmetric.  Only the ``ul`` triangle of ``A`` is used.

.. function:: symm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.

.. function:: symv!(ul, alpha, A, x, beta, y)

   Update the vector ``y`` as ``alpha*A*y + beta*y``. ``A`` is assumed
   to be symmetric.  Only the ``ul`` triangle of ``A`` is used.
   Returns the updated ``y``.

.. function:: symv(ul, alpha, A, x)

   Returns ``alpha*A*x``. ``A`` is assumed to be symmetric.  Only the
   ``ul`` triangle of ``A`` is used.

.. function:: symv(ul, A, x)

   Returns ``A*x``.  ``A`` is assumed to be symmetric.  Only the
   ``ul`` triangle of ``A`` is used.

.. function:: trmm!(side, ul, tA, dA, alpha, A, B)

   Update ``B`` as ``alpha*A*B`` or one of the other three variants
   determined by ``side`` (A on left or right) and ``tA`` (transpose A).
   Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if
   ``A`` is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated ``B``.

.. function:: trmm(side, ul, tA, dA, alpha, A, B)

   Returns ``alpha*A*B`` or one of the other three variants
   determined by ``side`` (A on left or right) and ``tA`` (transpose A).
   Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if
   ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: trsm!(side, ul, tA, dA, alpha, A, B)

   Overwrite ``B`` with the solution to ``A*X = alpha*B`` or one of
   the other three variants determined by ``side`` (A on left or
   right of ``X``) and ``tA`` (transpose A). Only the ``ul`` triangle
   of ``A`` is used.  ``dA`` indicates if ``A`` is unit-triangular
   (the diagonal is assumed to be all ones).  Returns the updated ``B``.

.. function:: trsm(side, ul, tA, dA, alpha, A, B)

   Returns the solution to ``A*X = alpha*B`` or one of
   the other three variants determined by ``side`` (A on left or
   right of ``X``) and ``tA`` (transpose A). Only the ``ul`` triangle
   of ``A`` is used.  ``dA`` indicates if ``A`` is unit-triangular
   (the diagonal is assumed to be all ones).

.. function:: trmv!(side, ul, tA, dA, alpha, A, b)

   Update ``b`` as ``alpha*A*b`` or one of the other three variants
   determined by ``side`` (A on left or right) and ``tA`` (transpose A).
   Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if
   ``A`` is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated ``b``.

.. function:: trmv(side, ul, tA, dA, alpha, A, b)

   Returns ``alpha*A*b`` or one of the other three variants
   determined by ``side`` (A on left or right) and ``tA`` (transpose A).
   Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if
   ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: trsv!(side, ul, tA, dA, alpha, A, b)

   Overwrite ``b`` with the solution to ``A*X = alpha*b`` or one of
   the other three variants determined by ``side`` (A on left or
   right of ``X``) and ``tA`` (transpose A). Only the ``ul`` triangle
   of ``A`` is used.  ``dA`` indicates if ``A`` is unit-triangular
   (the diagonal is assumed to be all ones).  Returns the updated ``b``.

.. function:: trsv(side, ul, tA, dA, alpha, A, b)

   Returns the solution to ``A*X = alpha*b`` or one of
   the other three variants determined by ``side`` (A on left or
   right of ``X``) and ``tA`` (transpose A). Only the ``ul`` triangle
   of ``A`` is used.  ``dA`` indicates if ``A`` is unit-triangular
   (the diagonal is assumed to be all ones).

.. function:: blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.
