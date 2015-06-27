.. _stdlib-linalg:

****************
 Linear Algebra
****************

Standard Functions
------------------

.. module:: Base.LinAlg

.. currentmodule:: Base

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.  Sparse factorizations call functions from `SuiteSparse <http://faculty.cse.tamu.edu/davis/suitesparse.html>`_.

.. function:: *(s, t)

   Concatenate strings. The ``*`` operator is an alias to this function.


.. function:: \\(A, B)
   :noindex:

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B`` when ``A`` is square.  The solver that is used depends upon the structure of ``A``.  A direct solver is used for upper- or lower triangular ``A``.  For Hermitian ``A`` (equivalent to symmetric ``A`` for non-complex ``A``) the ``BunchKaufman`` factorization is used.  Otherwise an LU factorization is used. For rectangular ``A`` the result is the minimum-norm least squares solution computed by a pivoted QR factorization of ``A`` and a rank estimate of A based on the R factor.

   When ``A`` is sparse, a similar polyalgorithm is used. For indefinite matrices, the LDLt factorization does not use pivoting during the numerical factorization and therefore the procedure can fail even for invertible matrices.

.. function:: dot(x, y)

   Compute the dot product. For complex vectors, the first vector is conjugated.


.. function:: vecdot(x, y)

   For any iterable containers ``x`` and ``y`` (including arrays of any dimension) of numbers (or any element type for which ``dot`` is defined), compute the Euclidean dot product (the sum of


.. function:: cross(x, y)

   Compute the cross product of two 3-vectors.


.. function:: factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch- Kaufman, LowerTriangular, UpperTriangular) of A, based upon the type of the input matrix. The return value can then be reused for efficient solving of multiple systems. For example:


.. function:: full(QRCompactWYQ[, thin=true]) -> Matrix

   Converts an orthogonal or unitary matrix stored as a Optionally takes a ``thin`` Boolean argument, which if ``true`` omits the columns that span the rows of ``R`` in the QR factorization that are zero. The resulting matrix is the ``Q`` in a thin QR factorization (sometimes called the reduced QR factorization).  If ``false``, returns a ``Q`` that spans all rows of ``R`` in its corresponding QR factorization.


  Reconstruct the matrix ``A`` from the factorization ``F=factorize(A)``.

.. function:: lu(A) -> L, U, p

   Compute the LU factorization of ``A``, such that ``A[p,:] = L*U``.


.. function:: lufact(A[, pivot=Val{true}]) -> F

   Compute the LU factorization of ``A``. The return type of ``F`` depends on the type of ``A``. In most cases, if ``A`` is a subtype pivoting is chosen (default) the element type should also support examples are shown in the table below. The individual components of the factorization ``F`` can be accessed by indexing:


.. function:: lufact!(A) -> LU

   overwriting the input A, instead of creating a copy.  For sparse 1-based indices to 0-based indices.


.. function:: chol(A[, LU]) -> F

   Compute the Cholesky factorization of a symmetric positive definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``Val{:U}`` and ``A = F*F'``. ``LU`` defaults to ``Val{:U}``.


.. function:: cholfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the Cholesky factorization of a sparse positive definite matrix ``A``. A fill-reducing permutation is used.  ``F = cholfact(A)`` is most frequently used to solve systems of equations with ``F\b``, but also the methods ``diag``, ``det``, ``logdet`` are defined for ``F``.  You can also extract individual factors from ``F``, using ``F[:L]``.  However, since pivoting is on by default, the factorization is internally represented as ``A == P'*L*L'*P`` with a permutation matrix ``P``; using just ``L`` without accounting for ``P`` will give incorrect answers.  To include the effects of permutation, it's typically preferable to extact ``combined`` factors like ``PtL = F[:PtL]`` (the equivalent of ``P'*L``) and ``LtP = F[:UP]`` (the equivalent of ``L'*P``). Setting optional ``shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``.  If the ``perm`` argument is nonempty, it should be a permutation of *1:size(A,1)* giving the ordering to use (instead of CHOLMOD's default AMD ordering). The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.


.. function:: cholfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the Cholesky factorization of a sparse positive definite matrix ``A``. A fill-reducing permutation is used.  ``F = cholfact(A)`` is most frequently used to solve systems of equations with ``F\b``, but also the methods ``diag``, ``det``, ``logdet`` are defined for ``F``.  You can also extract individual factors from ``F``, using ``F[:L]``.  However, since pivoting is on by default, the factorization is internally represented as ``A == P'*L*L'*P`` with a permutation matrix ``P``; using just ``L`` without accounting for ``P`` will give incorrect answers.  To include the effects of permutation, it's typically preferable to extact ``combined`` factors like ``PtL = F[:PtL]`` (the equivalent of ``P'*L``) and ``LtP = F[:UP]`` (the equivalent of ``L'*P``). Setting optional ``shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``.  If the ``perm`` argument is nonempty, it should be a permutation of *1:size(A,1)* giving the ordering to use (instead of CHOLMOD's default AMD ordering). The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.


.. function:: cholfact!(A [,LU=:U [,pivot=Val{false}]][;tol=-1.0]) -> Cholesky

   overwriting the input ``A``, instead of creating a copy. different matrix ``F`` with the same structure when used as:


.. function:: ldltfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the LDLt factorization of a sparse symmetric or Hermitian matrix ``A``. A fill-reducing permutation is used.  ``F = ldltfact(A)`` is most frequently used to solve systems of equations with ``F\b``, but also the methods ``diag``, ``det``, ``logdet`` are defined for ``F``. You can also extract individual factors from the factorization is internally represented as ``A == P'*L*D*L'*P`` with a permutation matrix ``P``; using just ``L`` without accounting for ``P`` will give incorrect answers.  To include the effects of permutation, it's typically preferable to extact complete list of supported factors is ``:L, :PtL, :D, :UP, :U, :LD, Setting optional `shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``.  If the ``perm`` argument is nonempty, it should be a permutation of *1:size(A,1)* giving the ordering to use (instead of CHOLMOD's default AMD ordering). The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.


.. function:: ldltfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the LDLt factorization of a sparse symmetric or Hermitian matrix ``A``. A fill-reducing permutation is used.  ``F = ldltfact(A)`` is most frequently used to solve systems of equations with ``F\b``, but also the methods ``diag``, ``det``, ``logdet`` are defined for ``F``. You can also extract individual factors from the factorization is internally represented as ``A == P'*L*D*L'*P`` with a permutation matrix ``P``; using just ``L`` without accounting for ``P`` will give incorrect answers.  To include the effects of permutation, it's typically preferable to extact complete list of supported factors is ``:L, :PtL, :D, :UP, :U, :LD, Setting optional `shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``.  If the ``perm`` argument is nonempty, it should be a permutation of *1:size(A,1)* giving the ordering to use (instead of CHOLMOD's default AMD ordering). The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.


.. function:: qr(A[, pivot=Val{false}][;thin=true]) -> Q, R, [p]

   Compute the (pivoted) QR factorization of ``A`` such that either is to compute a thin factorization. Note that ``R`` is not extended with zeros when the full ``Q`` is requested.


.. function:: qrfact(A) -> SPQR.Factorization

   Compute the QR factorization of a sparse matrix ``A``. A fill- reducing permutation is used. The main application of this type is to solve least squares problems with ``\``. The function calls the C library SPQR and a few additional functions from the library are wrapped but not exported.


.. function:: qrfact(A) -> SPQR.Factorization

   Compute the QR factorization of a sparse matrix ``A``. A fill- reducing permutation is used. The main application of this type is to solve least squares problems with ``\``. The function calls the C library SPQR and a few additional functions from the library are wrapped but not exported.


.. function:: qrfact!(A[, pivot=Val{false}])

   instead of creating a copy.


.. function:: full(QRCompactWYQ[, thin=true]) -> Matrix

   Converts an orthogonal or unitary matrix stored as a Optionally takes a ``thin`` Boolean argument, which if ``true`` omits the columns that span the rows of ``R`` in the QR factorization that are zero. The resulting matrix is the ``Q`` in a thin QR factorization (sometimes called the reduced QR factorization).  If ``false``, returns a ``Q`` that spans all rows of ``R`` in its corresponding QR factorization.


.. function:: bkfact(A) -> BunchKaufman

   Compute the Bunch-Kaufman [Bunch1977] factorization of a real symmetric or complex Hermitian matrix ``A`` and return a


.. [Bunch1977] J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. `url <http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0>`_.

.. function:: bkfact!(A) -> BunchKaufman

   overwriting the input ``A``, instead of creating a copy.


.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then using Schur factorizations (``schurfact()``) unless it detects the matrix to be Hermitian or real symmetric, in which case it computes the matrix square root from an eigendecomposition (``eigfact()``). In the latter situation for positive definite matrices, the matrix square root has ``Real`` elements, otherwise it has ``Complex`` elements.


.. function:: eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of ``A`` with respect to ``B``. the factorization to a tuple; where possible, using ``eigfact()`` is recommended.


.. function:: eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of ``A`` with respect to ``B``. the factorization to a tuple; where possible, using ``eigfact()`` is recommended.


.. function:: eigvals(A,[irange,][vl,][vu])

   Returns the eigenvalues of ``A``. If ``A`` is ``Symmetric``, only a subset of the eigenvalues by specifying either a eigenvalues, or a pair ``vl`` and ``vu`` for the lower and upper boundaries of the eigenvalues. For general non-symmetric matrices it is possible to specify how the matrix is balanced before the eigenvector calculation. The option ``permute=true`` permutes the matrix to become closer to upper triangular, and ``scale=true`` scales the matrix by its diagonal elements to make rows and columns more equal in norm. The default is ``true`` for both options.


.. function:: eigmax(A)

   Returns the largest eigenvalue of ``A``.


.. function:: eigmin(A)

   Returns the smallest eigenvalue of ``A``.


.. function:: eigvecs(A, [eigvals,][permute=true,][scale=true]) -> Matrix

   Returns a matrix ``M`` whose columns are the eigenvectors of ``A``. k]``.) The `permute`` and ``scale`` keywords are the same as for For ``SymTridiagonal`` matrices, if the optional vector of eigenvalues ``eigvals`` is specified, returns the specific corresponding eigenvectors.


.. function:: eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of ``A`` and which contains the generalized eigenvalues in ``F[:values]`` and the generalized eigenvectors in the columns of the matrix obtained from the slice ``F[:vectors][:, k]``.)


.. function:: eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of ``A`` and which contains the generalized eigenvalues in ``F[:values]`` and the generalized eigenvectors in the columns of the matrix obtained from the slice ``F[:vectors][:, k]``.)


.. function:: eigfact!(A[, B])

   Same as ``eigfact()``, but saves space by overwriting the input


.. function:: hessfact(A)

   Compute the Hessenberg decomposition of ``A`` and return a unitary matrix can be accessed with ``F[:Q]`` and the Hessenberg matrix with ``F[:H]``. When ``Q`` is extracted, the resulting type is the ``HessenbergQ`` object, and may be converted to a regular matrix with ``full()``.


.. function:: hessfact!(A)

   overwriting the input A, instead of creating a copy.


.. function:: schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``. The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and


.. function:: schurfact!(A)

   Computes the Schur factorization of ``A``, overwriting ``A`` in the process. See ``schurfact()``


.. function:: schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See ``schurfact()``


.. function:: ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object.  See ``ordschur()``.


.. function:: ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object by overwriting the object with the new factorization.  See


.. function:: ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object.  See ``ordschur()``.


.. function:: ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object by overwriting the object with the new factorization.  See


.. function:: schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``. The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and


.. function:: schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See ``schurfact()``


.. function:: ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object.  See ``ordschur()``.


.. function:: ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object by overwriting the object with the new factorization.  See


.. function:: ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object.  See ``ordschur()``.


.. function:: ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur object by overwriting the object with the new factorization.  See


.. function:: svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of ``A`` and ``B``, returning a F[:U]*F[:D1]*F[:R0]*F[:Q]'` and `B = F[:V]*F[:D2]*F[:R0]*F[:Q]'`.


.. function:: svdfact!(A[, thin=true]) -> SVD

   overwriting the input A, instead of creating a copy. If ``thin`` is to produce a thin decomposition.


.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. The function returns the generalized SVD of ``A`` and such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.


.. function:: svdvals(A, B)

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``.


.. function:: svdvals!(A)

   Returns the singular values of ``A``, while saving space by overwriting the input.


.. function:: svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of ``A`` and ``B``, returning a F[:U]*F[:D1]*F[:R0]*F[:Q]'` and `B = F[:V]*F[:D2]*F[:R0]*F[:Q]'`.


.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. The function returns the generalized SVD of ``A`` and such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.


.. function:: svdvals(A, B)

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``.


.. function:: triu(M, k)

   Returns the upper triangle of ``M`` starting from the ``k``th superdiagonal.


.. function:: triu(M, k)

   Returns the upper triangle of ``M`` starting from the ``k``th superdiagonal.


.. function:: triu!(M, k)

   Returns the upper triangle of ``M`` starting from the ``k``th superdiagonal, overwriting ``M`` in the process.


.. function:: triu!(M, k)

   Returns the upper triangle of ``M`` starting from the ``k``th superdiagonal, overwriting ``M`` in the process.


.. function:: tril(M, k)

   Returns the lower triangle of ``M`` starting from the ``k``th subdiagonal.


.. function:: tril(M, k)

   Returns the lower triangle of ``M`` starting from the ``k``th subdiagonal.


.. function:: tril!(M, k)

   Returns the lower triangle of ``M`` starting from the ``k``th subdiagonal, overwriting ``M`` in the process.


.. function:: tril!(M, k)

   Returns the lower triangle of ``M`` starting from the ``k``th subdiagonal, overwriting ``M`` in the process.


.. function:: diagind(M[, k])

   A ``Range`` giving the indices of the ``k``th diagonal of the matrix ``M``.


.. function:: diag(M[, k])

   The ``k``th diagonal of a matrix, as a vector. Use ``diagm`` to construct a diagonal matrix.


.. function:: diagm(v[, k])

   Construct a diagonal matrix and place ``v`` on the ``k``th diagonal.


.. function:: scale(b, A)

   Scale an array ``A`` by a scalar ``b``, returning a new array. If ``A`` is a matrix and ``b`` is a vector, then ``scale(A,b)`` scales each column ``i`` of ``A`` by ``b[i]`` (similar to array. Note: for large ``A``, ``scale`` can be much faster than ``A .* b`` or ``b .* A``, due to the use of BLAS.


.. function:: scale(b, A)

   Scale an array ``A`` by a scalar ``b``, returning a new array. If ``A`` is a matrix and ``b`` is a vector, then ``scale(A,b)`` scales each column ``i`` of ``A`` by ``b[i]`` (similar to array. Note: for large ``A``, ``scale`` can be much faster than ``A .* b`` or ``b .* A``, due to the use of BLAS.


.. function:: scale!(b, A)

   Scale an array ``A`` by a scalar ``b``, similar to ``scale()`` but overwriting ``A`` in-place. If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)`` scales each column ``i`` of ``A`` by ``b[i]`` (similar to place on ``A``.


.. function:: scale!(b, A)

   Scale an array ``A`` by a scalar ``b``, similar to ``scale()`` but overwriting ``A`` in-place. If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)`` scales each column ``i`` of ``A`` by ``b[i]`` (similar to place on ``A``.


.. function:: Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal, respectively.  The result is of type but may be converted into a regular matrix with ``full()``.


.. function:: Bidiagonal(dv, ev, isupper)

   Constructs an upper (``isupper=true``) or lower (``isupper=false``) bidiagonal matrix using the given diagonal (``dv``) and off- diagonal (``ev``) vectors.  The result is of type ``Bidiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with ``full()``.


.. function:: SymTridiagonal(d, du)

   Construct a real symmetric tridiagonal matrix from the diagonal and upper diagonal, respectively. The result is of type but may be converted into a regular matrix with ``full()``.


.. function:: rank(M)

   Compute the rank of a matrix.


.. function:: norm(A[, p])

   Compute the ``p``-norm of a vector or the operator norm of a matrix For vectors, ``p`` can assume any numeric value (even though not all values produce a mathematically valid vector norm). In particular, ``norm(A, Inf)`` returns the largest value in For matrices, valid values of ``p`` are ``1``, ``2``, or ``Inf``. implemented.) Use ``vecnorm()`` to compute the Frobenius norm.


.. function:: vecnorm(A[, p])

   For any iterable container ``A`` (including arrays of any dimension) of numbers (or any element type for which ``norm`` is defined), compute the ``p``-norm (defaulting to ``p=2``) as if For example, if ``A`` is a matrix and ``p=2``, then this is equivalent to the Frobenius norm.


.. function:: cond(M[, p])

   Condition number of the matrix ``M``, computed using the operator


.. function:: condskeel(M[, x, p])

   Skeel condition number \kappa_S of the matrix ``M``, optionally with respect to the vector ``x``, as computed using the operator values for ``p`` are ``1``, ``2``, or ``Inf``. This quantity is also known in the literature as the Bauer condition number, relative condition number, or componentwise relative condition number.


.. function:: trace(M)

   Matrix trace


.. function:: det(M)

   Matrix determinant


.. function:: logdet(M)

   Log of matrix determinant. Equivalent to ``log(det(M))``, but may provide increased accuracy and/or speed.


.. function:: logabsdet(M)

   Log of absolute value of determinant of real matrix. Equivalent to ``(log(abs(det(M))), sign(det(M)))``, but may provide increased accuracy and/or speed.

.. function:: inv(M)

   Matrix inverse


.. function:: pinv(M[, tol])

   Computes the Moore-Penrose pseudoinverse. For matrices ``M`` with floating point elements, it is convenient to compute the pseudoinverse by inverting only singular values above a given threshold, ``tol``. The optimal choice of ``tol`` varies both with the value of ``M`` and the intended application of the pseudoinverse. The default value of ``tol`` is essentially machine epsilon for the real part of a matrix element multiplied by the larger matrix dimension. For inverting dense ill- conditioned matrices in a least-squares sense, ``tol = sqrt(eps(real(float(one(eltype(M))))))`` is recommended. For more information, see [8859], [B96], [S84], [KY88].


.. function:: nullspace(M)

   Basis for nullspace of ``M``.


.. function:: repmat(A, n, m)

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.


.. function:: repeat(A, inner = Int[], outer = Int[])

   Construct an array by repeating the entries of ``A``. The i-th element of ``inner`` specifies the number of times that the individual entries of the i-th dimension of ``A`` should be repeated. The i-th element of ``outer`` specifies the number of times that a slice along the i-th dimension of ``A`` should be repeated.


.. function:: kron(A, B)

   Kronecker tensor product of two vectors or two matrices.


.. function:: blkdiag(A...)

   Concatenate matrices block-diagonally. Currently only implemented for sparse matrices.


.. function:: linreg(x, y, w)

   Weighted least-squares linear regression.


.. function:: linreg(x, y, w)

   Weighted least-squares linear regression.


.. function:: expm(A)

   Matrix exponential.


.. function:: lyap(A, C)

   Computes the solution ``X`` to the continuous Lyapunov equation part and no two eigenvalues are negative complex conjugates of each other.


.. function:: sylvester(A, B, C)

   Computes the solution ``X`` to the Sylvester equation `AX + XB + C


.. function:: issym(A) -> Bool

   Test whether a matrix is symmetric.


.. function:: isposdef(A) -> Bool

   Test whether a matrix is positive definite.


.. function:: isposdef!(A) -> Bool

   Test whether a matrix is positive definite, overwriting ``A`` in the processes.


.. function:: istril(A) -> Bool

   Test whether a matrix is lower triangular.


.. function:: istriu(A) -> Bool

   Test whether a matrix is upper triangular.


.. function:: isdiag(A) -> Bool

   Test whether a matrix is diagonal.


.. function:: ishermitian(A) -> Bool

   Test whether a matrix is Hermitian.


.. function:: transpose(A)

   The transposition operator (``.'``).


.. function:: transpose!(dest, src)

   Transpose array ``src`` and store the result in the preallocated array ``dest``, which should have a size corresponding to supported and unexpected results will happen if *src* and *dest* have overlapping memory regions.


.. function:: ctranspose(A)

   The conjugate transposition operator (``'``).


.. function:: ctranspose!(dest, src)

   Conjugate transpose array ``src`` and store the result in the preallocated array ``dest``, which should have a size corresponding to ``(size(src,2),size(src,1))``. No in-place transposition is supported and unexpected results will happen if *src* and *dest* have overlapping memory regions.


.. function:: eigs(A[, B], ; nev=6, which="LM", tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0, ))) -> (d[, v], nconv, niter, nmult, resid)

   Computes eigenvalues ``d`` of ``A`` using Lanczos or Arnoldi iterations for real symmetric or general nonsymmetric matrices respectively. If ``B`` is provided, the generalized eigenproblem is solved. The following keyword arguments are supported: corresponding Ritz vectors ``v`` (only if ``ritzvec=true``), the number of converged eigenvalues ``nconv``, the number of iterations Note: The ``sigma`` and ``which`` keywords interact: the


.. function:: svds(A; nsv=6, ritzvec=true, tol=0.0, maxiter=1000) -> (left_sv, s, right_sv, nconv, niter, nmult, resid)

   Lanczos or Arnoldi iterations. Uses ``eigs()`` underneath. Inputs are:


.. function:: peakflops(n; parallel=false)

   double precision ``Base.LinAlg.BLAS.gemm!()``. By default, if no arguments are specified, it multiplies a matrix of size ``n x n``, where ``n = 2000``. If the underlying BLAS is using multiple threads, higher flop rates are realized. The number of BLAS threads can be set with ``blas_set_num_threads(n)``. If the keyword argument ``parallel`` is set to ``true``, flop rate of the entire parallel computer is returned. When running in parallel, only 1 BLAS thread is used. The argument ``n`` still refers to the size of the problem that is solved on each processor.


BLAS Functions
--------------

.. module:: Base.LinAlg.BLAS

:mod:`Base.LinAlg.BLAS` provides wrappers for some of the BLAS functions for
linear algebra.  Those BLAS functions that overwrite one of the input
arrays have names ending in ``'!'``.

Usually a function has 4 methods defined, one each for ``Float64``,
``Float32``, ``Complex128`` and ``Complex64`` arrays.

.. currentmodule:: Base.LinAlg.BLAS

.. function:: dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of ``n`` elements of array stride ``incy``.


.. function:: dotu(n, X, incx, Y, incy)

   Dot function for two complex vectors.


.. function:: dotc(n, X, incx, U, incy)

   Dot function for two complex vectors conjugating the first vector.


.. function:: blascopy!(n, X, incx, Y, incy)

   Copy ``n`` elements of array ``X`` with stride ``incx`` to array


.. function:: nrm2(n, X, incx)

   2-norm of a vector consisting of ``n`` elements of array ``X`` with stride ``incx``.


.. function:: asum(n, X, incx)

   sum of the absolute values of the first ``n`` elements of array


.. function:: axpy!(a, X, Y)

   Overwrite ``Y`` with ``a*X + Y``.  Returns ``Y``.


.. function:: scal!(n, a, X, incx)

   Overwrite ``X`` with ``a*X``.  Returns ``X``.


.. function:: scal(n, a, X, incx)

   Returns ``a*X``.


.. function:: ger!(alpha, x, y, A)

   Rank-1 update of the matrix ``A`` with vectors ``x`` and ``y`` as


.. function:: syr!(uplo, alpha, x, A)

   Rank-1 update of the symmetric matrix ``A`` with vector ``x`` as


.. function:: syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix ``C`` as ``alpha*A*A.' + beta*C`` or ``alpha*A.'*A + beta*C`` according to whether ``trans`` is 'N' or 'T'.  When ``uplo`` is 'U' the upper triangle of ``C`` is updated ('L' for lower triangle).  Returns ``C``.


.. function:: syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according to ``uplo`` ('U' or 'L'), of ``alpha*A*A.'`` or ``alpha*A.'*A``, according to ``trans`` ('N' or 'T').


.. function:: her!(uplo, alpha, x, A)

   Methods for complex arrays only.  Rank-1 update of the Hermitian matrix ``A`` with vector ``x`` as ``alpha*x*x' + A``.  When lower triangle). Returns ``A``.


.. function:: herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian matrix ``C`` as ``alpha*A*A' + beta*C`` or ``alpha*A'*A + beta*C`` according to whether ``trans`` is 'N' or 'T'.  When ``uplo`` is 'U' the upper triangle of ``C`` is updated ('L' for lower triangle). Returns ``C``.


.. function:: herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle or the lower triangle, according to ``uplo`` ('U' or 'L'), of


.. function:: gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector ``y`` as ``alpha*A*x + beta*y`` or ``alpha*A'*x + beta*y`` according to ``trans`` ('N' or 'T').  The matrix ``A`` is a general band matrix of dimension ``m`` by ``size(A,2)`` with updated ``y``.


.. function:: gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns ``alpha*A*x`` or ``alpha*A'*x`` according to ``trans`` ('N' or 'T'). The matrix ``A`` is a general band matrix of dimension diagonals.


.. function:: sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector ``y`` as ``alpha*A*x + beta*y`` where ``A`` is a a symmetric band matrix of order ``size(A,2)`` with ``k`` super- diagonals stored in the argument ``A``.  The storage layout for http://www.netlib.org/lapack/explore-html/. Returns the updated ``y``.


.. function:: sbmv(uplo, k, A, x)

   Returns ``A*x`` where ``A`` is a symmetric band matrix of order


.. function:: sbmv(uplo, k, A, x)

   Returns ``A*x`` where ``A`` is a symmetric band matrix of order


.. function:: gemm!(tA, tB, alpha, A, B, beta, C)

   Update ``C`` as ``alpha*A*B + beta*C`` or the other three variants according to ``tA`` (transpose ``A``) and ``tB``.  Returns the updated ``C``.


.. function:: gemm(tA, tB, A, B)

   Returns ``A*B`` or the other three variants according to ``tA``


.. function:: gemm(tA, tB, A, B)

   Returns ``A*B`` or the other three variants according to ``tA``


.. function:: gemv!(tA, alpha, A, x, beta, y)

   Update the vector ``y`` as ``alpha*A*x + beta*y`` or ``alpha*A'x + beta*y`` according to ``tA`` (transpose ``A``). Returns the updated


.. function:: gemv(tA, A, x)

   Returns ``A*x`` or ``A'x`` according to ``tA`` (transpose ``A``).


.. function:: gemv(tA, A, x)

   Returns ``A*x`` or ``A'x`` according to ``tA`` (transpose ``A``).


.. function:: symm!(side, ul, alpha, A, B, beta, C)

   Update ``C`` as ``alpha*A*B + beta*C`` or ``alpha*B*A + beta*C`` according to ``side``. ``A`` is assumed to be symmetric.  Only the


.. function:: symm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants according to


.. function:: symm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants according to


.. function:: symm(tA, tB, alpha, A, B)

   Returns ``alpha*A*B`` or the other three variants according to


.. function:: symv!(ul, alpha, A, x, beta, y)

   Update the vector ``y`` as ``alpha*A*x + beta*y``. ``A`` is assumed to be symmetric.  Only the ``ul`` triangle of ``A`` is used. Returns the updated ``y``.


.. function:: symv(ul, A, x)

   Returns ``A*x``.  ``A`` is assumed to be symmetric.  Only the


.. function:: symv(ul, A, x)

   Returns ``A*x``.  ``A`` is assumed to be symmetric.  Only the


.. function:: trmm!(side, ul, tA, dA, alpha, A, B)

   Update ``B`` as ``alpha*A*B`` or one of the other three variants determined by ``side`` (A on left or right) and ``tA`` (transpose A). Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if Returns the updated ``B``.


.. function:: trmm(side, ul, tA, dA, alpha, A, B)

   Returns ``alpha*A*B`` or one of the other three variants determined by ``side`` (A on left or right) and ``tA`` (transpose A). Only the unit-triangular (the diagonal is assumed to be all ones).


.. function:: trsm!(side, ul, tA, dA, alpha, A, B)

   Overwrite ``B`` with the solution to ``A*X = alpha*B`` or one of the other three variants determined by ``side`` (A on left or right of ``X``) and ``tA`` (transpose A). Only the ``ul`` triangle of diagonal is assumed to be all ones).  Returns the updated ``B``.


.. function:: trsm(side, ul, tA, dA, alpha, A, B)

   Returns the solution to ``A*X = alpha*B`` or one of the other three variants determined by ``side`` (A on left or right of ``X``) and assumed to be all ones).


.. function:: trmv!(side, ul, tA, dA, alpha, A, b)

   Update ``b`` as ``alpha*A*b`` or one of the other three variants determined by ``side`` (A on left or right) and ``tA`` (transpose A). Only the ``ul`` triangle of ``A`` is used.  ``dA`` indicates if Returns the updated ``b``.


.. function:: trmv(side, ul, tA, dA, alpha, A, b)

   Returns ``alpha*A*b`` or one of the other three variants determined by ``side`` (A on left or right) and ``tA`` (transpose A). Only the unit-triangular (the diagonal is assumed to be all ones).


.. function:: trsv!(ul, tA, dA, A, b)

   Overwrite ``b`` with the solution to ``A*x = b`` or one of the other two variants determined by ``tA`` (transpose A) and ``ul`` triangular (the diagonal is assumed to be all ones).  Returns the updated ``b``.


.. function:: trsv(ul, tA, dA, A, b)

   Returns the solution to ``A*x = b`` or one of the other two variants determined by ``tA`` (transpose A) and ``ul`` (triangle of diagonal is assumed to be all ones).


.. function:: blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.


.. data:: I

   An object of type ``UniformScaling``, representing an identity matrix of any size.
