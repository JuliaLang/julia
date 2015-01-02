.. _stdlib-linalg:

Linear Algebra
--------------

.. module:: Base.LinAlg

.. currentmodule:: Base

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.  Sparse factorizations call functions from `SuiteSparse <http://faculty.cse.tamu.edu/davis/suitesparse.html>`_.

.. function:: *(A, B)
   :noindex:

   Matrix multiplication

.. function:: \\(A, B)
   :noindex:

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B`` when ``A`` is square.  The solver that is used depends upon the structure of ``A``.  A direct solver is used for upper- or lower triangular ``A``.  For Hermitian ``A`` (equivalent to symmetric ``A`` for non-complex ``A``) the ``BunchKaufman`` factorization is used.  Otherwise an LU factorization is used. For rectangular ``A`` the result is the minimum-norm least squares solution computed by a pivoted QR factorization of ``A`` and a rank estimate of A based on the R factor. For sparse, square ``A`` the LU factorization (from UMFPACK) is used.

.. function:: dot(x, y)
              ⋅(x,y)

   Compute the dot product. For complex vectors, the first vector is conjugated.

.. function:: cross(x, y)
              ×(x,y)

   Compute the cross product of two 3-vectors.

.. function:: rref(A)

   Compute the reduced row echelon form of the matrix A.

.. function:: factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch-Kaufman, Triangular) of A, based upon the type of the input matrix. The return value can then be reused for efficient solving of multiple systems. For example: ``A=factorize(A); x=A\\b; y=A\\C``.

.. function:: factorize!(A)

   ``factorize!`` is the same as :func:`factorize`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: lu(A) -> L, U, p

   Compute the LU factorization of ``A``, such that ``A[p,:] = L*U``.

.. function:: lufact(A, [pivot=true]) -> F

   Compute the LU factorization of ``A``. The return type of ``F`` depends on the type of ``A``. In most cases, if ``A`` is a subtype ``S`` of AbstractMatrix with an element type ``T``` supporting ``+``, ``-``, ``*`` and ``/`` the return type is ``LU{T,S{T}}``. If pivoting is chosen (default) the element type should also support ``abs`` and ``<``. When ``A`` is sparse and have element of type ``Float32``, ``Float64``, ``Complex{Float32}``, or ``Complex{Float64}`` the return type is ``UmfpackLU``. Some examples are shown in the table below.

      ======================= ========================= ========================================
      Type of input ``A``     Type of output ``F``      Relationship between ``F`` and ``A``
      ----------------------- ------------------------- ----------------------------------------
      :func:`Matrix`           ``LU``                   ``F[:L]*F[:U] == A[F[:p], :]``
      :func:`Tridiagonal`      ``LU{T,Tridiagonal{T}}``  N/A
      :func:`SparseMatrixCSC`  ``UmfpackLU``            ``F[:L]*F[:U] == F[:Rs] .* A[F[:p], F[:q]]``
      ======================= ========================= ========================================

   The individual components of the factorization ``F`` can be accessed by indexing:

      =========== ======================================= ====== ======================== =============
      Component   Description                             ``LU`` ``LU{T,Tridiagonal{T}}`` ``UmfpackLU``
      ----------- --------------------------------------- ------ ------------------------ -------------
      ``F[:L]``   ``L`` (lower triangular) part of ``LU``    ✓                                     ✓
      ``F[:U]``   ``U`` (upper triangular) part of ``LU``    ✓                                     ✓
      ``F[:p]``   (right) permutation ``Vector``             ✓                                     ✓
      ``F[:P]``   (right) permutation ``Matrix``             ✓              
      ``F[:q]``   left permutation ``Vector``                                                      ✓
      ``F[:Rs]``  ``Vector`` of scaling factors                                                    ✓
      ``F[:(:)]`` ``(L,U,p,q,Rs)`` components                                                      ✓
      =========== ======================================= ====== ======================== =============

      ================== ====== ======================== =============
      Supported function ``LU`` ``LU{T,Tridiagonal{T}}`` ``UmfpackLU``
      ------------------ ------ ------------------------ -------------
           ``/``            ✓
           ``\``            ✓                       ✓             ✓
           ``cond``         ✓                                     ✓
           ``det``          ✓                       ✓             ✓
           ``size``         ✓                       ✓
      ================== ====== ======================== =============

.. function:: lufact!(A) -> LU

   ``lufact!`` is the same as :func:`lufact`, but saves space by overwriting the input A, instead of creating a copy.  For sparse ``A`` the ``nzval`` field is not overwritten but the index fields, ``colptr`` and ``rowval`` are decremented in place, converting from 1-based indices to 0-based indices.

.. function:: chol(A, [LU]) -> F

   Compute the Cholesky factorization of a symmetric positive definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``:L`` (Lower), ``A = L*L'``. If ``LU`` is ``:U`` (Upper), ``A = R'*R``.

.. function:: cholfact(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   Compute the Cholesky factorization of a dense symmetric positive (semi)definite matrix ``A`` and return either a ``Cholesky`` if ``pivot=false`` or ``CholeskyPivoted`` if ``pivot=true``. ``LU`` may be ``:L`` for using the lower part or ``:U`` for the upper part. The default is to use ``:U``. The triangular matrix can be obtained from the factorization ``F`` with: ``F[:L]`` and ``F[:U]``. The following functions are available for ``Cholesky`` objects: ``size``, ``\``, ``inv``, ``det``. For ``CholeskyPivoted`` there is also defined a ``rank``. If ``pivot=false`` a ``PosDefException`` exception is thrown in case the matrix is not positive definite. The argument ``tol`` determines the tolerance for determining the rank. For negative values, the tolerance is the machine precision.

.. function:: cholfact(A, [ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix ``A``.  If ``A`` is Hermitian its Cholesky factor is determined.  If ``A`` is not Hermitian the Cholesky factor of ``A*A'`` is determined. A fill-reducing permutation is used.  Methods for ``size``, ``solve``, ``\``, ``findn_nzs``, ``diag``, ``det`` and ``logdet`` are available for ``CholmodFactor`` objects.  One of the solve methods includes an integer argument that can be used to solve systems involving parts of the factorization only.  The optional boolean argument, ``ll`` determines whether the factorization returned is of the ``A[p,p] = L*L'`` form, where ``L`` is lower triangular or ``A[p,p] = L*Diagonal(D)*L'`` form where ``L`` is unit lower triangular and ``D`` is a non-negative vector.  The default is LDL. The symbolic factorization can also be reused for other matrices with the same structure as ``A`` by calling ``cholfact!``.

.. function:: cholfact!(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   ``cholfact!`` is the same as :func:`cholfact`, but saves space by overwriting the input ``A``, instead of creating a copy. ``cholfact!`` can also reuse the symbolic factorization from a different matrix ``F`` with the same structure when used as: ``cholfact!(F::CholmodFactor, A)``.

.. function:: ldltfact(A) -> LDLtFactorization

   Compute a factorization of a positive definite matrix ``A`` such that ``A=L*Diagonal(d)*L'`` where ``L`` is a unit lower triangular matrix and ``d`` is a vector with non-negative elements.

.. function:: qr(A, [pivot=false,][thin=true]) -> Q, R, [p]

   Compute the (pivoted) QR factorization of ``A`` such that either ``A = Q*R`` or ``A[:,p] = Q*R``. Also see ``qrfact``. The default is to compute a thin factorization. Note that ``R`` is not extended with zeros when the full ``Q`` is requested. 

.. function:: qrfact(A,[pivot=false]) -> F

   Computes the QR factorization of ``A``. The return type of ``F`` depends on the element type of ``A`` and whether pivoting is specified (with ``pivot=true``).

      ================ ================= ========= =====================================
      Return type      ``eltype(A)``     ``pivot``  Relationship between ``F`` and ``A``
      ---------------- ----------------- --------- -------------------------------------
      ``QR``           not ``BlasFloat`` either     ``A==F[:Q]*F[:R]``
      ``QRCompactWY``  ``BlasFloat``     ``false``  ``A==F[:Q]*F[:R]``
      ``QRPivoted``    ``BlasFloat``     ``true``   ``A[:,F[:p]]==F[:Q]*F[:R]``
      ================ ================= ========= =====================================

   ``BlasFloat`` refers to any of: ``Float32``, ``Float64``, ``Complex64`` or ``Complex128``.

   The individual components of the factorization ``F`` can be accessed by indexing:

      =========== ============================================= ================== ===================== ==================
      Component   Description                                   ``QR``             ``QRCompactWY``       ``QRPivoted``
      ----------- --------------------------------------------- ------------------ --------------------- ------------------
      ``F[:Q]``   ``Q`` (orthogonal/unitary) part of ``QR``      ✓ (``QRPackedQ``)  ✓ (``QRCompactWYQ``)  ✓ (``QRPackedQ``)
      ``F[:R]``   ``R`` (upper right triangular) part of ``QR``  ✓                  ✓                     ✓
      ``F[:p]``   pivot ``Vector``                                                                        ✓
      ``F[:P]``   (pivot) permutation ``Matrix``                                                          ✓
      =========== ============================================= ================== ===================== ==================

   The following functions are available for the ``QR`` objects: ``size``, ``\``. When ``A`` is rectangular, ``\`` will return a least squares solution and if the solution is not unique, the one with smallest norm is returned.

   Multiplication with respect to either thin or full ``Q`` is allowed, i.e. both ``F[:Q]*F[:R]`` and ``F[:Q]*A`` are supported. A ``Q`` matrix can be converted into a regular matrix with :func:`full` which has a named argument ``thin``.

   .. note::

      ``qrfact`` returns multiple types because LAPACK uses several representations that minimize the memory storage requirements of products of Householder elementary reflectors, so that the ``Q`` and ``R`` matrices can be stored compactly rather as two separate dense matrices.

      The data contained in ``QR`` or ``QRPivoted`` can be used to construct the ``QRPackedQ`` type, which is a compact representation of the rotation matrix:

         .. math::

            Q = \prod_{i=1}^{\min(m,n)} (I - \tau_i v_i v_i^T)

      where :math:`\tau_i` is the scale factor and :math:`v_i` is the projection vector associated with the :math:`i^{th}` Householder elementary reflector.

      The data contained in ``QRCompactWY`` can be used to construct the ``QRCompactWYQ`` type, which is a compact representation of the rotation matrix

         .. math::

            Q = I + Y T Y^T

      where ``Y`` is :math:`m \times r` lower trapezoidal and ``T`` is :math:`r \times r` upper triangular. The *compact WY* representation [Schreiber1989]_ is not to be confused with the older, *WY* representation [Bischof1987]_. (The LAPACK documentation uses ``V`` in lieu of ``Y``.)

   .. [Bischof1987] C Bischof and C Van Loan, The WY representation for products of Householder matrices, SIAM J Sci Stat Comput 8 (1987), s2-s13. doi:10.1137/0908009
   .. [Schreiber1989] R Schreiber and C Van Loan, A storage-efficient WY representation for products of Householder transformations, SIAM J Sci Stat Comput 10 (1989), 53-57. doi:10.1137/0910005

.. function:: qrfact!(A,[pivot=false])

   ``qrfact!`` is the same as :func:`qrfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: bkfact(A) -> BunchKaufman

   Compute the Bunch-Kaufman [Bunch1977]_ factorization of a real symmetric or complex Hermitian matrix ``A`` and return a ``BunchKaufman`` object. The following functions are available for ``BunchKaufman`` objects: ``size``, ``\``, ``inv``, ``issym``, ``ishermitian``.

.. [Bunch1977] J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. `url <http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0>`_.

.. function:: bkfact!(A) -> BunchKaufman

   ``bkfact!`` is the same as :func:`bkfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then ``B*B == A`` within roundoff error.

   ``sqrtm`` uses a polyalgorithm, computing the matrix square root using Schur factorizations (:func:`schurfact`) unless it detects the matrix to be Hermitian or real symmetric, in which case it computes the matrix square root from an eigendecomposition (:func:`eigfact`). In the latter situation for positive definite matrices, the matrix square root has ``Real`` elements, otherwise it has ``Complex`` elements.

.. function:: eig(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> D, V

   Computes eigenvalues and eigenvectors of ``A``. See :func:`eigfact` for
   details on the ``balance`` keyword argument.
   
   .. doctest::

      julia> eig([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
      ([1.0,3.0,18.0],
      3x3 Array{Float64,2}:
       1.0  0.0  0.0
       0.0  1.0  0.0
       0.0  0.0  1.0)
   
   ``eig`` is a wrapper around :func:`eigfact`, extracting all parts of the
   factorization to a tuple; where possible, using :func:`eigfact` is
   recommended.

.. function:: eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of ``A`` with respect to ``B``.
    
   ``eig`` is a wrapper around :func:`eigfact`, extracting all parts of the
   factorization to a tuple; where possible, using :func:`eigfact` is
   recommended.

.. function:: eigvals(A,[irange,][vl,][vu])

   Returns the eigenvalues of ``A``. If ``A`` is :class:`Symmetric`,
   :class:`Hermitian` or :class:`SymTridiagonal`, it is possible to calculate
   only a subset of the eigenvalues by specifying either a :class:`UnitRange`
   ``irange`` covering indices of the sorted eigenvalues, or a pair ``vl`` and
   ``vu`` for the lower and upper boundaries of the eigenvalues.

   For general non-symmetric matrices it is possible to specify how the matrix
   is balanced before the eigenvector calculation. The option ``permute=true``
   permutes the matrix to become closer to upper triangular, and ``scale=true``
   scales the matrix by its diagonal elements to make rows and columns more
   equal in norm. The default is ``true`` for both options.

.. function:: eigmax(A)

   Returns the largest eigenvalue of ``A``.

.. function:: eigmin(A)

   Returns the smallest eigenvalue of ``A``.

.. function:: eigvecs(A, [eigvals,][permute=true,][scale=true]) -> Matrix

   Returns a matrix ``M`` whose columns are the eigenvectors of ``A``.
   (The ``k``th eigenvector can be obtained from the slice ``M[:, k]``.)
   The ``permute`` and ``scale`` keywords are the same as for :func:`eigfact`.

   For :class:`SymTridiagonal` matrices, if the optional vector of eigenvalues
   ``eigvals`` is specified, returns the specific corresponding eigenvectors.

.. function:: eigfact(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> Eigen

   Computes the eigenvalue decomposition of ``A``, returning an ``Eigen``
   factorization object ``F`` which contains the eigenvalues in ``F[:values]``
   and the eigenvectors in the columns of the matrix ``F[:vectors]``. (The
   ``k``th eigenvector can be obtained from the slice ``F[:vectors][:, k]``.)
 
   The following functions are available for ``Eigen`` objects: ``inv``,
   ``det``.

   If ``A`` is :class:`Symmetric`, :class:`Hermitian` or :class:`SymTridiagonal`,
   it is possible to calculate only a subset of the eigenvalues by specifying
   either a :class:`UnitRange` ``irange`` covering indices of the sorted
   eigenvalues or a pair ``vl`` and ``vu`` for the lower and upper boundaries
   of the eigenvalues.

   For general nonsymmetric matrices it is possible to specify how the matrix
   is balanced before the eigenvector calculation. The option ``permute=true``
   permutes the matrix to become closer to upper triangular, and ``scale=true``
   scales the matrix by its diagonal elements to make rows and columns more
   equal in norm. The default is ``true`` for both options.

.. function:: eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of ``A`` and ``B``,
   returning a ``GeneralizedEigen`` factorization object ``F`` which contains
   the generalized eigenvalues in ``F[:values]`` and the generalized
   eigenvectors in the columns of the matrix ``F[:vectors]``. (The ``k``th
   generalized eigenvector can be obtained from the slice ``F[:vectors][:,
   k]``.)

.. function:: eigfact!(A, [B])

   Same as :func:`eigfact`, but saves space by overwriting the input ``A`` (and
   ``B``), instead of creating a copy.

.. function:: hessfact(A)

   Compute the Hessenberg decomposition of ``A`` and return a ``Hessenberg`` object. If ``F`` is the factorization object, the unitary matrix can be accessed with ``F[:Q]`` and the Hessenberg matrix with ``F[:H]``. When ``Q`` is extracted, the resulting type is the ``HessenbergQ`` object, and may be converted to a regular matrix with :func:`full`.

.. function:: hessfact!(A)

   ``hessfact!`` is the same as :func:`hessfact`, but saves space by overwriting the input A, instead of creating a copy.

.. function:: schurfact(A) -> Schur

   Computes the Schur factorization of the matrix ``A``. The (quasi) triangular Schur factor can be obtained from the ``Schur`` object ``F`` with either ``F[:Schur]`` or ``F[:T]`` and the unitary/orthogonal Schur vectors can be obtained with ``F[:vectors]`` or ``F[:Z]`` such that ``A=F[:vectors]*F[:Schur]*F[:vectors]'``. The eigenvalues of ``A`` can be obtained with ``F[:values]``.

.. function:: schurfact!(A)

   Computes the Schur factorization of ``A``, overwriting ``A`` in the process. See :func:`schurfact`

.. function:: schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   See :func:`schurfact`

.. function:: ordschur(Q, T, select) -> Schur

   Reorders the Schur factorization of a real matrix ``A=Q*T*Q'`` according to the logical array ``select`` returning a Schur object ``F``. The selected eigenvalues appear in the leading diagonal of ``F[:Schur]`` and the the corresponding leading columns of ``F[:vectors]`` form an orthonormal basis of the corresponding right invariant subspace. A complex conjugate pair of eigenvalues must be either both included or excluded via ``select``. 

.. function:: ordschur!(Q, T, select) -> Schur

   Reorders the Schur factorization of a real matrix ``A=Q*T*Q'``, overwriting ``Q`` and ``T`` in the process. See :func:`ordschur`

.. function:: ordschur(S, select) -> Schur

   Reorders the Schur factorization ``S`` of type ``Schur``.

.. function:: ordschur!(S, select) -> Schur

   Reorders the Schur factorization ``S`` of type ``Schur``, overwriting ``S`` in the process. See :func:`ordschur`

.. function:: schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``. The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and ``F[:T]``, the left unitary/orthogonal Schur vectors can be obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and ``B=F[:left]*F[:T]*F[:right]'``. The generalized eigenvalues of ``A`` and ``B`` can be obtained with ``F[:alpha]./F[:beta]``.

.. function:: schur(A,B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See :func:`schurfact`

.. function:: svdfact(A, [thin=true]) -> SVD

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVD`` object. ``U``, ``S``, ``V`` and ``Vt`` can be obtained from the factorization ``F`` with ``F[:U]``, ``F[:S]``, ``F[:V]`` and ``F[:Vt]``, such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned. The algorithm produces ``Vt`` and hence ``Vt`` is more efficient to extract than ``V``. The default is to produce a thin decomposition.

.. function:: svdfact!(A, [thin=true]) -> SVD

   ``svdfact!`` is the same as :func:`svdfact`, but saves space by overwriting the input A, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned. The default is to produce a thin decomposition.

.. function:: svd(A, [thin=true]) -> U, S, V

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. Computes the SVD of A, returning ``U``, vector ``S``, and ``V`` such that ``A == U*diagm(S)*V'``. If ``thin`` is ``true``, an economy mode decomposition is returned. The default is to produce a thin decomposition.

.. function:: svdvals(A)

   Returns the singular values of ``A``.

.. function:: svdvals!(A)

   Returns the singular values of ``A``, while saving space by overwriting the input.

.. function:: svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of ``A`` and ``B``, returning a ``GeneralizedSVD`` Factorization object ``F``, such that ``A = F[:U]*F[:D1]*F[:R0]*F[:Q]'`` and ``B = F[:V]*F[:D2]*F[:R0]*F[:Q]'``.

.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. The function returns the generalized SVD of ``A`` and ``B``, returning ``U``, ``V``, ``Q``, ``D1``, ``D2``, and ``R0`` such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.

.. function:: svdvals(A, B)

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``.

.. function:: triu(M)

   Upper triangle of a matrix.

.. function:: triu!(M)

   Upper triangle of a matrix, overwriting ``M`` in the process.

.. function:: tril(M)

   Lower triangle of a matrix.

.. function:: tril!(M)

   Lower triangle of a matrix, overwriting ``M`` in the process.

.. function:: diagind(M[, k])

   A ``Range`` giving the indices of the ``k``-th diagonal of the matrix ``M``.

.. function:: diag(M[, k])

   The ``k``-th diagonal of a matrix, as a vector. Use ``diagm`` to construct a diagonal matrix.

.. function:: diagm(v[, k])

   Construct a diagonal matrix and place ``v`` on the ``k``-th diagonal.

.. function:: scale(A, b)
.. function:: scale(b, A)

   Scale an array ``A`` by a scalar ``b``, returning a new array.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale(A,b)``
   scales each column ``i`` of ``A`` by ``b[i]`` (similar to
   ``A*diagm(b)``), while ``scale(b,A)`` scales each row ``i`` of
   ``A`` by ``b[i]`` (similar to ``diagm(b)*A``), returning a new array.

   Note: for large ``A``, ``scale`` can be much faster than ``A .* b`` or
   ``b .* A``, due to the use of BLAS.

.. function:: scale!(A, b)
.. function:: scale!(b, A)

   Scale an array ``A`` by a scalar ``b``, similar to :func:`scale` but
   overwriting ``A`` in-place.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)``
   scales each column ``i`` of ``A`` by ``b[i]`` (similar to
   ``A*diagm(b)``), while ``scale!(b,A)`` scales each row ``i`` of
   ``A`` by ``b[i]`` (similar to ``diagm(b)*A``), again operating in-place
   on ``A``.

.. function:: Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal, respectively.  The result is of type ``Tridiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with :func:`full`.

.. function:: Bidiagonal(dv, ev, isupper)

   Constructs an upper (``isupper=true``) or lower (``isupper=false``) bidiagonal matrix
   using the given diagonal (``dv``) and off-diagonal (``ev``) vectors.  The result is of type ``Bidiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with :func:`full`.

.. function:: SymTridiagonal(d, du)

   Construct a real symmetric tridiagonal matrix from the diagonal and upper diagonal, respectively. The result is of type ``SymTridiagonal`` and provides efficient specialized eigensolvers, but may be converted into a regular matrix with :func:`full`.

.. function:: Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury matrix identity.

.. function:: rank(M)

   Compute the rank of a matrix.

.. function:: norm(A, [p])

   Compute the ``p``-norm of a vector or the operator norm of a matrix ``A``, defaulting to the ``p=2``-norm.

   For vectors, ``p`` can assume any numeric value (even though not all values produce a mathematically valid vector norm). In particular, ``norm(A, Inf)`` returns the largest value in ``abs(A)``, whereas ``norm(A, -Inf)`` returns the smallest.

   For matrices, valid values of ``p`` are ``1``, ``2``, or ``Inf``. (Note that for sparse matrices, ``p=2`` is currently not implemented.) Use :func:`vecnorm` to compute the Frobenius norm.

.. function:: vecnorm(A, [p])

   For any iterable container ``A`` (including arrays of any dimension)
   of numbers, compute the ``p``-norm (defaulting to ``p=2``) as if ``A``
   were a vector of the corresponding length.

   For example, if ``A`` is a matrix and ``p=2``, then this is equivalent
   to the Frobenius norm.

.. function:: cond(M, [p])

   Condition number of the matrix ``M``, computed using the operator ``p``-norm. Valid values for ``p`` are ``1``, ``2`` (default), or ``Inf``.

.. function:: condskeel(M, [x, p])

   .. math::
      \kappa_S(M, p) & = \left\Vert \left\vert M \right\vert \left\vert M^{-1} \right\vert  \right\Vert_p \\
      \kappa_S(M, x, p) & = \left\Vert \left\vert M \right\vert \left\vert M^{-1} \right\vert \left\vert x \right\vert \right\Vert_p

   Skeel condition number :math:`\kappa_S` of the matrix ``M``, optionally with respect to the vector ``x``, as computed using the operator ``p``-norm. ``p`` is ``Inf`` by default, if not provided. Valid values for ``p`` are ``1``, ``2``, or ``Inf``.

   This quantity is also known in the literature as the Bauer condition number, relative condition number, or componentwise relative condition number.

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

.. function:: blkdiag(A...)

   Concatenate matrices block-diagonally. Currently only implemented for sparse matrices.

.. function:: linreg(x, y) -> [a; b]

   Linear Regression. Returns ``a`` and ``b`` such that ``a+b*x`` is the closest line to the given points ``(x,y)``. In other words, this function determines parameters ``[a, b]`` that minimize the squared error between ``y`` and ``a+b*x``. 

   **Example**::

      using PyPlot;
      x = float([1:12])
      y = [5.5; 6.3; 7.6; 8.8; 10.9; 11.79; 13.48; 15.02; 17.77; 20.81; 22.0; 22.99]
      a, b = linreg(x,y) # Linear regression
      plot(x, y, "o") # Plot (x,y) points
      plot(x, [a+b*i for i in x]) # Plot the line determined by the linear regression

.. function:: linreg(x, y, w)

   Weighted least-squares linear regression.

.. function:: expm(A)

   Matrix exponential.

.. function:: lyap(A, C)

   Computes the solution ``X`` to the continuous Lyapunov equation ``AX + XA' + C = 0``, where no eigenvalue of ``A`` has a zero real part and no two eigenvalues are negative complex conjugates of each other. 

.. function:: sylvester(A, B, C)

   Computes the solution ``X`` to the Sylvester equation ``AX + XB + C = 0``, where ``A``, ``B`` and ``C`` have compatible dimensions and ``A`` and ``-B`` have no eigenvalues with equal real part.

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

.. function:: ishermitian(A) -> Bool

   Test whether a matrix is Hermitian.

.. function:: transpose(A)

   The transposition operator (``.'``).

.. function:: transpose!(dest,src)

   Transpose array ``src`` and store the result in the preallocated array ``dest``, which should have a size corresponding to ``(size(src,2),size(src,1))``. No in-place transposition is supported and unexpected results will happen if `src` and `dest` have overlapping memory regions.

.. function:: ctranspose(A)

   The conjugate transposition operator (``'``).

.. function:: ctranspose!(dest,src)

   Conjugate transpose array ``src`` and store the result in the preallocated array ``dest``, which should have a size corresponding to ``(size(src,2),size(src,1))``. No in-place transposition is supported and unexpected results will happen if `src` and `dest` have overlapping memory regions.

.. function:: eigs(A, [B,]; nev=6, which="LM", tol=0.0, maxiter=1000, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

   ``eigs`` computes eigenvalues ``d`` of ``A`` using Lanczos or Arnoldi iterations for real symmetric or general nonsymmetric matrices respectively. If ``B`` is provided, the generalized eigen-problem is solved.  The following keyword arguments are supported:
    * ``nev``: Number of eigenvalues
    * ``ncv``: Number of Krylov vectors used in the computation; should satisfy ``nev+1 <= ncv <= n`` for real symmetric problems and ``nev+2 <= ncv <= n`` for other problems; default is ``ncv = max(20,2*nev+1)``.
    * ``which``: type of eigenvalues to compute. See the note below.

      ========= ======================================================================================================================
      ``which`` type of eigenvalues
      --------- ----------------------------------------------------------------------------------------------------------------------
      ``:LM``   eigenvalues of largest magnitude (default)
      ``:SM``   eigenvalues of smallest magnitude
      ``:LR``   eigenvalues of largest real part
      ``:SR``   eigenvalues of smallest real part
      ``:LI``   eigenvalues of largest imaginary part (nonsymmetric or complex ``A`` only)
      ``:SI``   eigenvalues of smallest imaginary part (nonsymmetric or complex ``A`` only)
      ``:BE``   compute half of the eigenvalues from each end of the spectrum, biased in favor of the high end. (real symmetric ``A`` only)
      ========= ======================================================================================================================

    * ``tol``: tolerance (:math:`tol \le 0.0` defaults to ``DLAMCH('EPS')``)
    * ``maxiter``: Maximum number of iterations (default = 300)
    * ``sigma``: Specifies the level shift used in inverse iteration. If ``nothing`` (default), defaults to ordinary (forward) iterations. Otherwise, find eigenvalues close to ``sigma`` using shift and invert iterations.
    * ``ritzvec``: Returns the Ritz vectors ``v`` (eigenvectors) if ``true``
    * ``v0``: starting vector from which to start the iterations

   ``eigs`` returns the ``nev`` requested eigenvalues in ``d``, the corresponding Ritz vectors ``v`` (only if ``ritzvec=true``), the number of converged eigenvalues ``nconv``, the number of iterations ``niter`` and the number of matrix vector multiplications ``nmult``, as well as the final residual vector ``resid``.
   
   .. note:: The ``sigma`` and ``which`` keywords interact: the description of eigenvalues searched for by ``which`` do _not_ necessarily refer to the eigenvalues of ``A``, but rather the linear operator constructed by the specification of the iteration mode implied by ``sigma``. 

      =============== ================================== ==================================
      ``sigma``       iteration mode                     ``which`` refers to eigenvalues of
      --------------- ---------------------------------- ----------------------------------
      ``nothing``     ordinary (forward)                 :math:`A`
      real or complex inverse with level shift ``sigma`` :math:`(A - \sigma I )^{-1}`
      =============== ================================== ==================================

.. function:: peakflops(n; parallel=false)

   ``peakflops`` computes the peak flop rate of the computer by using double precision :func:`Base.LinAlg.BLAS.gemm!`. By default, if no arguments are specified, it multiplies a matrix of size ``n x n``, where ``n = 2000``. If the underlying BLAS is using multiple threads, higher flop rates are realized. The number of BLAS threads can be set with ``blas_set_num_threads(n)``.

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

.. function:: gemm(tA, tB, A, B)

   Returns ``A*B`` or the other three variants
   according to ``tA`` (transpose ``A``) and ``tB``.

.. function:: gemv!(tA, alpha, A, x, beta, y)

   Update the vector ``y`` as ``alpha*A*x + beta*y`` or
   ``alpha*A'x + beta*y`` according to ``tA`` (transpose ``A``).
   Returns the updated ``y``.

.. function:: gemv(tA, alpha, A, x)

   Returns ``alpha*A*x`` or ``alpha*A'x`` according to ``tA``
   (transpose ``A``).

.. function:: gemv(tA, A, x)

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

   Update the vector ``y`` as ``alpha*A*x + beta*y``. ``A`` is assumed
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

.. function:: trsv!(ul, tA, dA, A, b)

   Overwrite ``b`` with the solution to ``A*x = b`` or one of the other two 
   variants determined by ``tA`` (transpose A) and ``ul`` (triangle of ``A`` 
   used).  ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed 
   to be all ones).  Returns the updated ``b``.

.. function:: trsv(ul, tA, dA, A, b)

   Returns the solution to ``A*x = b`` or one of the other two variants 
   determined by ``tA`` (transpose A) and ``ul`` (triangle of ``A`` is used.) 
   ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be 
   all ones).


.. function:: blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.
