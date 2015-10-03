.. _stdlib-linalg:

****************
 Linear Algebra
****************

Standard Functions
------------------

.. module:: Base.LinAlg

.. currentmodule:: Base

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.  Sparse factorizations call functions from `SuiteSparse <http://faculty.cse.tamu.edu/davis/suitesparse.html>`_.

.. function:: *(A, B)

   .. Docstring generated from Julia source

   Matrix multiplication

.. function:: \\(A, B)

   .. Docstring generated from Julia source

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B`` when ``A`` is square.  The solver that is used depends upon the structure of ``A``.  A direct solver is used for upper or lower triangular ``A``.  For Hermitian ``A`` (equivalent to symmetric ``A`` for non-complex ``A``) the ``BunchKaufman`` factorization is used.  Otherwise an LU factorization is used. For rectangular ``A`` the result is the minimum-norm least squares solution computed by a pivoted QR factorization of ``A`` and a rank estimate of ``A`` based on the R factor.

   When ``A`` is sparse, a similar polyalgorithm is used. For indefinite matrices, the ``LDLt`` factorization does not use pivoting during the numerical factorization and therefore the procedure can fail even for invertible matrices.

.. function:: dot(x, y)
              ⋅(x,y)

   .. Docstring generated from Julia source

   Compute the dot product. For complex vectors, the first vector is conjugated.

.. function:: vecdot(x, y)

   .. Docstring generated from Julia source

   For any iterable containers ``x`` and ``y`` (including arrays of any dimension) of numbers (or any element type for which ``dot`` is defined), compute the Euclidean dot product (the sum of ``dot(x[i],y[i])``\ ) as if they were vectors.

.. function:: cross(x, y)
              ×(x,y)

   .. Docstring generated from Julia source

   Compute the cross product of two 3-vectors.

.. function:: factorize(A)

   .. Docstring generated from Julia source

   Compute a convenient factorization (including LU, Cholesky, Bunch-Kaufman, LowerTriangular, UpperTriangular) of ``A``\ , based upon the type of the input matrix. The return value can then be reused for efficient solving of multiple systems. For example: ``A=factorize(A); x=A\b; y=A\C``\ .

.. function:: full(F)

   .. Docstring generated from Julia source

   Reconstruct the matrix ``A`` from the factorization ``F=factorize(A)``.

.. function:: lu(A) -> L, U, p

   .. Docstring generated from Julia source

   Compute the LU factorization of ``A``\ , such that ``A[p,:] = L*U``\ .

.. function:: lufact(A [,pivot=Val{true}]) -> F

   .. Docstring generated from Julia source

   Compute the LU factorization of ``A``. The return type of ``F`` depends on the type of ``A``. In most cases, if ``A`` is a subtype ``S`` of AbstractMatrix with an element type ``T`` supporting ``+``, ``-``, ``*`` and ``/`` the return type is ``LU{T,S{T}}``. If pivoting is chosen (default) the element type should also support ``abs`` and ``<``. When ``A`` is sparse and have element of type ``Float32``, ``Float64``, ``Complex{Float32}``, or ``Complex{Float64}`` the return type is ``UmfpackLU``. Some examples are shown in the table below.

   ======================= ========================= ========================================
   Type of input ``A``     Type of output ``F``      Relationship between ``F`` and ``A``
   ======================= ========================= ========================================
   :func:`Matrix`           ``LU``                   ``F[:L]*F[:U] == A[F[:p], :]``
   :func:`Tridiagonal`      ``LU{T,Tridiagonal{T}}`` ``F[:L]*F[:U] == A[F[:p], :]``
   :func:`SparseMatrixCSC`  ``UmfpackLU``            ``F[:L]*F[:U] == (F[:Rs] .* A)[F[:p], F[:q]]``
   ======================= ========================= ========================================

   The individual components of the factorization ``F`` can be accessed by indexing:

   =========== ======================================= ====== ======================== =============
   Component   Description                             ``LU`` ``LU{T,Tridiagonal{T}}`` ``UmfpackLU``
   =========== ======================================= ====== ======================== =============
   ``F[:L]``   ``L`` (lower triangular) part of ``LU``    ✓            ✓                        ✓
   ``F[:U]``   ``U`` (upper triangular) part of ``LU``    ✓            ✓                        ✓
   ``F[:p]``   (right) permutation ``Vector``             ✓            ✓                        ✓
   ``F[:P]``   (right) permutation ``Matrix``             ✓            ✓
   ``F[:q]``   left permutation ``Vector``                                                      ✓
   ``F[:Rs]``  ``Vector`` of scaling factors                                                    ✓
   ``F[:(:)]`` ``(L,U,p,q,Rs)`` components                                                      ✓
   =========== ======================================= ====== ======================== =============

   ================== ====== ======================== =============
   Supported function ``LU`` ``LU{T,Tridiagonal{T}}`` ``UmfpackLU``
   ================== ====== ======================== =============
        ``/``            ✓
        ``\``            ✓                       ✓             ✓
        ``cond``         ✓                                     ✓
        ``det``          ✓                       ✓             ✓
        ``logdet``       ✓                       ✓
        ``logabsdet``    ✓                       ✓
        ``size``         ✓                       ✓
   ================== ====== ======================== =============

.. function:: lufact!(A) -> LU

   .. Docstring generated from Julia source

   ``lufact!`` is the same as :func:`lufact`, but saves space by overwriting the input ``A``, instead of creating a copy.  For sparse ``A`` the ``nzval`` field is not overwritten but the index fields, ``colptr`` and ``rowval`` are decremented in place, converting from 1-based indices to 0-based indices.

.. function:: chol(A, [LU]) -> F

   .. Docstring generated from Julia source

   Compute the Cholesky factorization of a symmetric positive definite matrix ``A`` and return the matrix ``F``\ . If ``LU`` is ``Val{:U}`` (Upper), ``F`` is of type ``UpperTriangular`` and ``A = F'*F``\ . If ``LU`` is ``Val{:L}`` (Lower), ``F`` is of type ``LowerTriangular`` and ``A = F*F'``\ . ``LU`` defaults to ``Val{:U}``\ .

.. function:: cholfact(A, [LU=:U[,pivot=Val{false}]][;tol=-1.0]) -> Cholesky

   .. Docstring generated from Julia source

   Compute the Cholesky factorization of a dense symmetric positive (semi)definite matrix ``A`` and return either a ``Cholesky`` if ``pivot==Val{false}`` or ``CholeskyPivoted`` if ``pivot==Val{true}``\ . ``LU`` may be ``:L`` for using the lower part or ``:U`` for the upper part. The default is to use ``:U``\ . The triangular matrix can be obtained from the factorization ``F`` with: ``F[:L]`` and ``F[:U]``\ . The following functions are available for ``Cholesky`` objects: ``size``\ , ``\``\ , ``inv``\ , ``det``\ . For ``CholeskyPivoted`` there is also defined a ``rank``\ . If ``pivot==Val{false}`` a ``PosDefException`` exception is thrown in case the matrix is not positive definite. The argument ``tol`` determines the tolerance for determining the rank. For negative values, the tolerance is the machine precision.

.. function:: cholfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   .. Docstring generated from Julia source

   Compute the Cholesky factorization of a sparse positive definite matrix ``A``\ . A fill-reducing permutation is used. ``F = cholfact(A)`` is most frequently used to solve systems of equations with ``F\b``\ , but also the methods ``diag``\ , ``det``\ , ``logdet`` are defined for ``F``\ . You can also extract individual factors from ``F``\ , using ``F[:L]``\ . However, since pivoting is on by default, the factorization is internally represented as ``A == P'*L*L'*P`` with a permutation matrix ``P``\ ; using just ``L`` without accounting for ``P`` will give incorrect answers. To include the effects of permutation, it's typically preferable to extact "combined" factors like ``PtL = F[:PtL]`` (the equivalent of ``P'*L``\ ) and ``LtP = F[:UP]`` (the equivalent of ``L'*P``\ ).

   Setting optional ``shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``\ . If the ``perm`` argument is nonempty, it should be a permutation of ``1:size(A,1)`` giving the ordering to use (instead of CHOLMOD's default AMD ordering).

   The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.

.. function:: cholfact!(A [,LU=:U [,pivot=Val{false}]][;tol=-1.0]) -> Cholesky

   .. Docstring generated from Julia source

   ``cholfact!`` is the same as :func:`cholfact`, but saves space by overwriting the input ``A``, instead of creating a copy. ``cholfact!`` can also reuse the symbolic factorization from a different matrix ``F`` with the same structure when used as: ``cholfact!(F::CholmodFactor, A)``.

.. function:: ldltfact(::SymTridiagonal) -> LDLt

   .. Docstring generated from Julia source

   Compute an ``LDLt`` factorization of a real symmetric tridiagonal matrix such that ``A = L*Diagonal(d)*L'`` where ``L`` is a unit lower triangular matrix and ``d`` is a vector. The main use of an ``LDLt`` factorization ``F = ldltfact(A)`` is to solve the linear system of equations ``Ax = b`` with ``F\b``\ .

.. function:: ldltfact(::Union{SparseMatrixCSC,Symmetric{Float64,SparseMatrixCSC{Flaot64,SuiteSparse_long}},Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},SuiteSparse_long}}}; shift=0, perm=Int[]) -> CHOLMOD.Factor

   .. Docstring generated from Julia source

   Compute the ``LDLt`` factorization of a sparse symmetric or Hermitian matrix. A fill-reducing permutation is used. ``F = ldltfact(A)`` is most frequently used to solve systems of equations ``A*x = b`` with ``F\b``\ . The returned factorization object ``F`` also supports the methods ``diag``\ , ``det``\ , and ``logdet``\ . You can extract individual factors from ``F`` using ``F[:L]``\ . However, since pivoting is on by default, the factorization is internally represented as ``A == P'*L*D*L'*P`` with a permutation matrix ``P``\ ; using just ``L`` without accounting for ``P`` will give incorrect answers. To include the effects of permutation, it's typically preferable to extact "combined" factors like ``PtL = F[:PtL]`` (the equivalent of ``P'*L``\ ) and ``LtP = F[:UP]`` (the equivalent of ``L'*P``\ ). The complete list of supported factors is ``:L, :PtL, :D, :UP, :U, :LD, :DU, :PtLD, :DUP``\ .

   Setting optional ``shift`` keyword argument computes the factorization of ``A+shift*I`` instead of ``A``\ . If the ``perm`` argument is nonempty, it should be a permutation of ``1:size(A,1)`` giving the ordering to use (instead of CHOLMOD's default AMD ordering).

   The function calls the C library CHOLMOD and many other functions from the library are wrapped but not exported.

.. function:: ldltfact!(::SymTridiagonal) -> LDLt

   .. Docstring generated from Julia source

   Same as ``ldltfact``\ , but saves space by overwriting the input ``A``\ , instead of creating a copy.

.. function:: qr(A [,pivot=Val{false}][;thin=true]) -> Q, R, [p]

   .. Docstring generated from Julia source

   Compute the (pivoted) QR factorization of ``A`` such that either ``A = Q*R`` or ``A[:,p] = Q*R``\ . Also see ``qrfact``\ . The default is to compute a thin factorization. Note that ``R`` is not extended with zeros when the full ``Q`` is requested.

.. function:: qrfact(A [,pivot=Val{false}]) -> F

   .. Docstring generated from Julia source

   Computes the QR factorization of ``A``. The return type of ``F`` depends on the element type of ``A`` and whether pivoting is specified (with ``pivot==Val{true}``).

   ================ ================= ============== =====================================
   Return type      ``eltype(A)``     ``pivot``      Relationship between ``F`` and ``A``
   ================ ================= ============== =====================================
   ``QR``           not ``BlasFloat`` either          ``A==F[:Q]*F[:R]``
   ``QRCompactWY``  ``BlasFloat``     ``Val{false}``  ``A==F[:Q]*F[:R]``
   ``QRPivoted``    ``BlasFloat``     ``Val{true}``   ``A[:,F[:p]]==F[:Q]*F[:R]``
   ================ ================= ============== =====================================

   ``BlasFloat`` refers to any of: ``Float32``, ``Float64``, ``Complex64`` or ``Complex128``.

   The individual components of the factorization ``F`` can be accessed by indexing:

   =========== ============================================= ================== ===================== ==================
   Component   Description                                   ``QR``             ``QRCompactWY``       ``QRPivoted``
   =========== ============================================= ================== ===================== ==================
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

   .. [Bischof1987] C Bischof and C Van Loan, "The WY representation for products
      of Householder matrices", SIAM J Sci Stat Comput 8 (1987), s2-s13.
      `doi:10.1137/0908009 <http://dx.doi.org/10.1137/0908009>`_
   .. [Schreiber1989] R Schreiber and C Van Loan, "A storage-efficient WY
      representation for products of Householder transformations",
      SIAM J Sci Stat Comput 10 (1989), 53-57.
      `doi:10.1137/0910005 <http://dx.doi.org/10.1137/0910005>`_

.. function:: qrfact(A) -> SPQR.Factorization

   .. Docstring generated from Julia source

   Compute the QR factorization of a sparse matrix ``A``. A fill-reducing permutation is used. The main application of this type is to solve least squares problems with ``\``. The function calls the C library SPQR and a few additional functions from the library are wrapped but not exported.

.. function:: qrfact!(A [,pivot=Val{false}])

   .. Docstring generated from Julia source

   ``qrfact!`` is the same as :func:`qrfact` when ``A`` is a subtype of ``StridedMatrix``, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: full(QRCompactWYQ[, thin=true]) -> Matrix

   .. Docstring generated from Julia source

   Converts an orthogonal or unitary matrix stored as a ``QRCompactWYQ``
   object, i.e. in the compact WY format [Bischof1987]_, to a dense matrix.

   Optionally takes a ``thin`` Boolean argument, which if ``true`` omits the
   columns that span the rows of ``R`` in the QR factorization that are zero.
   The resulting matrix is the ``Q`` in a thin QR factorization (sometimes
   called the reduced QR factorization).  If ``false``, returns a ``Q`` that
   spans all rows of ``R`` in its corresponding QR factorization.

.. function:: bkfact(A) -> BunchKaufman

   .. Docstring generated from Julia source

   Compute the Bunch-Kaufman [Bunch1977]_ factorization of a real symmetric or complex Hermitian matrix ``A`` and return a ``BunchKaufman`` object. The following functions are available for ``BunchKaufman`` objects: ``size``, ``\``, ``inv``, ``issym``, ``ishermitian``.

   .. [Bunch1977] J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. `url <http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0>`_.

.. function:: bkfact!(A) -> BunchKaufman

   .. Docstring generated from Julia source

   ``bkfact!`` is the same as :func:`bkfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: eig(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> D, V

   .. Docstring generated from Julia source

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

   .. Docstring generated from Julia source

   Computes generalized eigenvalues and vectors of ``A`` with respect to ``B``.

   ``eig`` is a wrapper around :func:`eigfact`, extracting all parts of the
   factorization to a tuple; where possible, using :func:`eigfact` is
   recommended.

.. function:: eigvals(A,[irange,][vl,][vu]) -> values

   .. Docstring generated from Julia source

   Returns the eigenvalues of ``A``\ . If ``A`` is ``Symmetric``\ , ``Hermitian`` or ``SymTridiagonal``\ , it is possible to calculate only a subset of the eigenvalues by specifying either a ``UnitRange`` ``irange`` covering indices of the sorted eigenvalues, or a pair ``vl`` and ``vu`` for the lower and upper boundaries of the eigenvalues.

   For general non-symmetric matrices it is possible to specify how the matrix is balanced before the eigenvector calculation. The option ``permute=true`` permutes the matrix to become closer to upper triangular, and ``scale=true`` scales the matrix by its diagonal elements to make rows and columns moreequal in norm. The default is ``true`` for both options.

.. function:: eigvals!(A,[irange,][vl,][vu]) -> values

   .. Docstring generated from Julia source

   Same as ``eigvals``\ , but saves space by overwriting the input ``A`` (and ``B``\ ), instead of creating a copy.

.. function:: eigmax(A)

   .. Docstring generated from Julia source

   Returns the largest eigenvalue of ``A``\ .

.. function:: eigmin(A)

   .. Docstring generated from Julia source

   Returns the smallest eigenvalue of ``A``\ .

.. function:: eigvecs(A, [eigvals,][permute=true,][scale=true]) -> Matrix

   .. Docstring generated from Julia source

   Returns a matrix ``M`` whose columns are the eigenvectors of ``A``.
   (The ``k``\ th eigenvector can be obtained from the slice ``M[:, k]``.)
   The ``permute`` and ``scale`` keywords are the same as for :func:`eigfact`.

   For :class:`SymTridiagonal` matrices, if the optional vector of eigenvalues
   ``eigvals`` is specified, returns the specific corresponding eigenvectors.

.. function:: eigfact(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> Eigen

   .. Docstring generated from Julia source

   Computes the eigenvalue decomposition of ``A``, returning an ``Eigen``
   factorization object ``F`` which contains the eigenvalues in ``F[:values]``
   and the eigenvectors in the columns of the matrix ``F[:vectors]``.
   (The ``k``\ th eigenvector can be obtained from the slice ``F[:vectors][:, k]``.)

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

   .. Docstring generated from Julia source

   Computes the generalized eigenvalue decomposition of ``A`` and ``B``,
   returning a ``GeneralizedEigen`` factorization object ``F`` which contains
   the generalized eigenvalues in ``F[:values]`` and the generalized
   eigenvectors in the columns of the matrix ``F[:vectors]``. (The ``k``\ th
   generalized eigenvector can be obtained from the slice ``F[:vectors][:,
   k]``.)

.. function:: eigfact!(A, [B])

   .. Docstring generated from Julia source

   Same as :func:`eigfact`, but saves space by overwriting the input ``A`` (and
   ``B``), instead of creating a copy.

.. function:: hessfact(A)

   .. Docstring generated from Julia source

   Compute the Hessenberg decomposition of ``A`` and return a ``Hessenberg`` object. If ``F`` is the factorization object, the unitary matrix can be accessed with ``F[:Q]`` and the Hessenberg matrix with ``F[:H]``. When ``Q`` is extracted, the resulting type is the ``HessenbergQ`` object, and may be converted to a regular matrix with :func:`full`.

.. function:: hessfact!(A)

   .. Docstring generated from Julia source

   ``hessfact!`` is the same as :func:`hessfact`, but saves space by overwriting the input ``A``, instead of creating a copy.

.. function:: schurfact(A) -> Schur

   .. Docstring generated from Julia source

   Computes the Schur factorization of the matrix ``A``\ . The (quasi) triangular Schur factor can be obtained from the ``Schur`` object ``F`` with either ``F[:Schur]`` or ``F[:T]`` and the unitary/orthogonal Schur vectors can be obtained with ``F[:vectors]`` or ``F[:Z]`` such that ``A=F[:vectors]*F[:Schur]*F[:vectors]'``\ . The eigenvalues of ``A`` can be obtained with ``F[:values]``\ .

.. function:: schurfact!(A)

   .. Docstring generated from Julia source

   Computes the Schur factorization of ``A``, overwriting ``A`` in the process. See :func:`schurfact`

.. function:: schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   .. Docstring generated from Julia source

   See :func:`schurfact`

.. function:: ordschur(Q, T, select) -> Schur

   .. Docstring generated from Julia source

   Reorders the Schur factorization of a real matrix ``A=Q*T*Q'`` according to the logical array ``select`` returning a Schur object ``F``. The selected eigenvalues appear in the leading diagonal of ``F[:Schur]`` and the the corresponding leading columns of ``F[:vectors]`` form an orthonormal basis of the corresponding right invariant subspace. A complex conjugate pair of eigenvalues must be either both included or excluded via ``select``.

.. function:: ordschur!(Q, T, select) -> Schur

   .. Docstring generated from Julia source

   Reorders the Schur factorization of a real matrix ``A=Q*T*Q'``, overwriting ``Q`` and ``T`` in the process. See :func:`ordschur`

.. function:: ordschur(S, select) -> Schur

   .. Docstring generated from Julia source

   Reorders the Schur factorization ``S`` of type ``Schur``.

.. function:: ordschur!(S, select) -> Schur

   .. Docstring generated from Julia source

   Reorders the Schur factorization ``S`` of type ``Schur``, overwriting ``S`` in the process. See :func:`ordschur`

.. function:: schurfact(A, B) -> GeneralizedSchur

   .. Docstring generated from Julia source

   Computes the Generalized Schur (or QZ) factorization of the matrices ``A`` and ``B``\ . The (quasi) triangular Schur factors can be obtained from the ``Schur`` object ``F`` with ``F[:S]`` and ``F[:T]``\ , the left unitary/orthogonal Schur vectors can be obtained with ``F[:left]`` or ``F[:Q]`` and the right unitary/orthogonal Schur vectors can be obtained with ``F[:right]`` or ``F[:Z]`` such that ``A=F[:left]*F[:S]*F[:right]'`` and ``B=F[:left]*F[:T]*F[:right]'``\ . The generalized eigenvalues of ``A`` and ``B`` can be obtained with ``F[:alpha]./F[:beta]``\ .

.. function:: schur(A,B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   .. Docstring generated from Julia source

   See :func:`schurfact`

.. function:: ordschur(S, T, Q, Z, select) -> GeneralizedSchur

   .. Docstring generated from Julia source

   Reorders the Generalized Schur factorization of a matrix ``(A, B) = (Q*S*Z^{H}, Q*T*Z^{H})`` according to the logical array ``select`` and returns a GeneralizedSchur object ``GS``.  The selected eigenvalues appear in the leading diagonal of both ``(GS[:S], GS[:T])`` and the left and right unitary/orthogonal Schur vectors are also reordered such that ``(A, B) = GS[:Q]*(GS[:S], GS[:T])*GS[:Z]^{H}`` still holds and the generalized eigenvalues of ``A`` and ``B`` can still be obtained with ``GS[:alpha]./GS[:beta]``.

.. function:: ordschur!(S, T, Q, Z, select) -> GeneralizedSchur

   .. Docstring generated from Julia source

   Reorders the Generalized Schur factorization of a matrix by overwriting the matrices ``(S, T, Q, Z)`` in the process.  See :func:`ordschur`.

.. function:: ordschur(GS, select) -> GeneralizedSchur

   .. Docstring generated from Julia source

   Reorders the Generalized Schur factorization of a Generalized Schur object.  See :func:`ordschur`.

.. function:: ordschur!(GS, select) -> GeneralizedSchur

   .. Docstring generated from Julia source

   Reorders the Generalized Schur factorization of a Generalized Schur object by overwriting the object with the new factorization.  See :func:`ordschur`.

.. function:: svdfact(A, [thin=true]) -> SVD

   .. Docstring generated from Julia source

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVD`` object. ``U``\ , ``S``\ , ``V`` and ``Vt`` can be obtained from the factorization ``F`` with ``F[:U]``\ , ``F[:S]``\ , ``F[:V]`` and ``F[:Vt]``\ , such that ``A = U*diagm(S)*Vt``\ . If ``thin`` is ``true``\ , an economy mode decomposition is returned. The algorithm produces ``Vt`` and hence ``Vt`` is more efficient to extract than ``V``\ . The default is to produce a thin decomposition.

.. function:: svdfact!(A, [thin=true]) -> SVD

   .. Docstring generated from Julia source

   ``svdfact!`` is the same as :func:`svdfact`, but saves space by overwriting the input ``A``, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned. The default is to produce a thin decomposition.

.. function:: svd(A, [thin=true]) -> U, S, V

   .. Docstring generated from Julia source

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. Computes the SVD of ``A``\ , returning ``U``\ , vector ``S``\ , and ``V`` such that ``A == U*diagm(S)*V'``\ . If ``thin`` is ``true``\ , an economy mode decomposition is returned. The default is to produce a thin decomposition.

.. function:: svdvals(A)

   .. Docstring generated from Julia source

   Returns the singular values of ``A``\ .

.. function:: svdvals!(A)

   .. Docstring generated from Julia source

   Returns the singular values of ``A``\ , while saving space by overwriting the input.

.. function:: svdfact(A, B) -> GeneralizedSVD

   .. Docstring generated from Julia source

   Compute the generalized SVD of ``A`` and ``B``\ , returning a ``GeneralizedSVD`` Factorization object ``F``\ , such that ``A = F[:U]*F[:D1]*F[:R0]*F[:Q]'`` and ``B = F[:V]*F[:D2]*F[:R0]*F[:Q]'``\ .

.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   .. Docstring generated from Julia source

   Wrapper around ``svdfact`` extracting all parts the factorization to a tuple. Direct use of ``svdfact`` is therefore generally more efficient. The function returns the generalized SVD of ``A`` and ``B``\ , returning ``U``\ , ``V``\ , ``Q``\ , ``D1``\ , ``D2``\ , and ``R0`` such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``\ .

.. function:: svdvals(A, B)

   .. Docstring generated from Julia source

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``\ .

.. function:: givens{T}(::T, ::T, ::Integer, ::Integer) -> {Givens, T}

   .. Docstring generated from Julia source

   Computes the tuple ``(G, r) = givens(f, g, i1, i2)`` where ``G`` is a Givens rotation and ``r`` is a scalar such that ``G*x=y`` with ``x[i1]=f``\ , ``x[i2]=g``\ , ``y[i1]=r``\ , and ``y[i2]=0``\ . The cosine and sine of the rotation angle can be extracted from the ``Givens`` type with ``G.c`` and ``G.s`` respectively. The arguments ``f`` and ``g`` can be either ``Float32``\ , ``Float64``\ , ``Complex{Float32}``\ , or ``Complex{Float64}``\ . The ``Givens`` type supports left multiplication ``G*A`` and conjugated transpose right multiplication ``A*G'``\ . The type doesn't have a ``size`` and can therefore be multiplied with matrices of arbitrary size as long as ``i2<=size(A,2)`` for ``G*A`` or ``i2<=size(A,1)`` for ``A*G'``\ .

.. function:: givens{T}(::AbstractArray{T}, ::Integer, ::Integer, ::Integer) -> {Givens, T}

   .. Docstring generated from Julia source

   Computes the tuple ``(G, r) = givens(A, i1, i2, col)`` where ``G`` is Givens rotation and ``r`` is a scalar such that ``G*A[:,col]=y`` with ``y[i1]=r``\ , and ``y[i2]=0``\ . The cosine and sine of the rotation angle can be extracted from the ``Givens`` type with ``G.c`` and ``G.s`` respectively. The element type of ``A`` can be either ``Float32``\ , ``Float64``\ , ``Complex{Float32}``\ , or ``Complex{Float64}``\ . The ``Givens`` type supports left multiplication ``G*A`` and conjugated transpose right multiplication ``A*G'``\ . The type doesn't have a ``size`` and can therefore be multiplied with matrices of arbitrary size as long as ``i2<=size(A,2)`` for ``G*A`` or ``i2<=size(A,1)`` for ``A*G'``\ .

.. function:: triu(M)

   .. Docstring generated from Julia source

   Upper triangle of a matrix.

.. function:: triu(M, k)

   .. Docstring generated from Julia source

   Returns the upper triangle of ``M`` starting from the ``k``\ th superdiagonal.

.. function:: triu!(M)

   .. Docstring generated from Julia source

   Upper triangle of a matrix, overwriting ``M`` in the process.

.. function:: triu!(M, k)

   .. Docstring generated from Julia source

   Returns the upper triangle of ``M`` starting from the ``k``\ th superdiagonal, overwriting ``M`` in the process.

.. function:: tril(M)

   .. Docstring generated from Julia source

   Lower triangle of a matrix.

.. function:: tril(M, k)

   .. Docstring generated from Julia source

   Returns the lower triangle of ``M`` starting from the ``k``\ th superdiagonal.

.. function:: tril!(M)

   .. Docstring generated from Julia source

   Lower triangle of a matrix, overwriting ``M`` in the process.

.. function:: tril!(M, k)

   .. Docstring generated from Julia source

   Returns the lower triangle of ``M`` starting from the ``k``\ th superdiagonal, overwriting ``M`` in the process.

.. function:: diagind(M[, k])

   .. Docstring generated from Julia source

   A ``Range`` giving the indices of the ``k``\ th diagonal of the matrix ``M``\ .

.. function:: diag(M[, k])

   .. Docstring generated from Julia source

   The ``k``\ th diagonal of a matrix, as a vector. Use ``diagm`` to construct a diagonal matrix.

.. function:: diagm(v[, k])

   .. Docstring generated from Julia source

   Construct a diagonal matrix and place ``v`` on the ``k``\ th diagonal.

.. function:: scale(A, b)
              scale(b, A)

   .. Docstring generated from Julia source

   Scale an array ``A`` by a scalar ``b``\ , returning a new array.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale(A,b)`` scales each column ``i`` of ``A`` by ``b[i]`` (similar to ``A*diagm(b)``\ ), while ``scale(b,A)`` scales each row ``i`` of ``A`` by ``b[i]`` (similar to ``diagm(b)*A``\ ), returning a new array.

   Note: for large ``A``\ , ``scale`` can be much faster than ``A .* b`` or ``b .* A``\ , due to the use of BLAS.

.. function:: scale!(A, b)
              scale!(b, A)

   .. Docstring generated from Julia source

   Scale an array ``A`` by a scalar ``b``, similar to :func:`scale` but
   overwriting ``A`` in-place.

   If ``A`` is a matrix and ``b`` is a vector, then ``scale!(A,b)``
   scales each column ``i`` of ``A`` by ``b[i]`` (similar to
   ``A*diagm(b)``), while ``scale!(b,A)`` scales each row ``i`` of
   ``A`` by ``b[i]`` (similar to ``diagm(b)*A``), again operating in-place
   on ``A``.

.. function:: Tridiagonal(dl, d, du)

   .. Docstring generated from Julia source

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal, respectively.  The result is of type ``Tridiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with :func:`full`.

.. function:: Bidiagonal(dv, ev, isupper)

   .. Docstring generated from Julia source

   Constructs an upper (``isupper=true``) or lower (``isupper=false``) bidiagonal matrix
   using the given diagonal (``dv``) and off-diagonal (``ev``) vectors.  The result is of type ``Bidiagonal`` and provides efficient specialized linear solvers, but may be converted into a regular matrix with :func:`full`.

.. function:: SymTridiagonal(d, du)

   .. Docstring generated from Julia source

   Construct a real symmetric tridiagonal matrix from the diagonal and upper diagonal, respectively. The result is of type ``SymTridiagonal`` and provides efficient specialized eigensolvers, but may be converted into a regular matrix with :func:`full`.

.. function:: rank(M)

   .. Docstring generated from Julia source

   Compute the rank of a matrix.

.. function:: norm(A, [p])

   .. Docstring generated from Julia source

   Compute the ``p``-norm of a vector or the operator norm of a matrix ``A``, defaulting to the ``p=2``-norm.

   For vectors, ``p`` can assume any numeric value (even though not all values produce a mathematically valid vector norm). In particular, ``norm(A, Inf)`` returns the largest value in ``abs(A)``, whereas ``norm(A, -Inf)`` returns the smallest.

   For matrices, the matrix norm induced by the vector ``p``-norm is used, where valid values of ``p`` are ``1``, ``2``, or ``Inf``. (Note that for sparse matrices, ``p=2`` is currently not implemented.) Use :func:`vecnorm` to compute the Frobenius norm.

.. function:: vecnorm(A, [p])

   .. Docstring generated from Julia source

   For any iterable container ``A`` (including arrays of any dimension) of numbers (or any element type for which ``norm`` is defined), compute the ``p``\ -norm (defaulting to ``p=2``\ ) as if ``A`` were a vector of the corresponding length.

   For example, if ``A`` is a matrix and ``p=2``\ , then this is equivalent to the Frobenius norm.

.. function:: cond(M, [p])

   .. Docstring generated from Julia source

   Condition number of the matrix ``M``\ , computed using the operator ``p``\ -norm. Valid values for ``p`` are ``1``\ , ``2`` (default), or ``Inf``\ .

.. function:: condskeel(M, [x, p])

   .. Docstring generated from Julia source

   .. math::

       \kappa_S(M, p) & = \left\Vert \left\vert M \right\vert \left\vert M^{-1} \right\vert  \right\Vert_p \\
       \kappa_S(M, x, p) & = \left\Vert \left\vert M \right\vert \left\vert M^{-1} \right\vert \left\vert x \right\vert \right\Vert_p

   Skeel condition number :math:`\kappa_S` of the matrix ``M``\ , optionally with respect to the vector ``x``\ , as computed using the operator ``p``\ -norm. ``p`` is ``Inf`` by default, if not provided. Valid values for ``p`` are ``1``\ , ``2``\ , or ``Inf``\ .

   This quantity is also known in the literature as the Bauer condition number, relative condition number, or componentwise relative condition number.

.. function:: trace(M)

   .. Docstring generated from Julia source

   Matrix trace

.. function:: det(M)

   .. Docstring generated from Julia source

   Matrix determinant

.. function:: logdet(M)

   .. Docstring generated from Julia source

   Log of matrix determinant. Equivalent to ``log(det(M))``\ , but may provide increased accuracy and/or speed.

.. function:: logabsdet(M)

   .. Docstring generated from Julia source

   Log of absolute value of determinant of real matrix. Equivalent to ``(log(abs(det(M))), sign(det(M)))``\ , but may provide increased accuracy and/or speed.

.. function:: inv(M)

   .. Docstring generated from Julia source

   Matrix inverse

.. function:: pinv(M[, tol])

   .. Docstring generated from Julia source

   Computes the Moore-Penrose pseudoinverse.

   For matrices ``M`` with floating point elements, it is convenient to compute
   the pseudoinverse by inverting only singular values above a given threshold,
   ``tol``.

   The optimal choice of ``tol`` varies both with the value of ``M``
   and the intended application of the pseudoinverse. The default value of
   ``tol`` is ``eps(real(float(one(eltype(M)))))*maximum(size(A))``,
   which is essentially machine epsilon for the real part of a matrix element
   multiplied by the larger matrix dimension.
   For inverting dense ill-conditioned matrices in a least-squares sense,
   ``tol = sqrt(eps(real(float(one(eltype(M))))))`` is recommended.

   For more information, see [8859]_, [B96]_, [S84]_, [KY88]_.

   .. [8859] Issue 8859, "Fix least squares", https://github.com/JuliaLang/julia/pull/8859
   .. [B96] Åke Björck, "Numerical Methods for Least Squares Problems",
      SIAM Press, Philadelphia, 1996, "Other Titles in Applied Mathematics", Vol. 51.
      `doi:10.1137/1.9781611971484 <http://epubs.siam.org/doi/book/10.1137/1.9781611971484>`_
   .. [S84] G. W. Stewart, "Rank Degeneracy", SIAM Journal on
      Scientific and Statistical Computing, 5(2), 1984, 403-413.
      `doi:10.1137/0905030 <http://epubs.siam.org/doi/abs/10.1137/0905030>`_
   .. [KY88] Konstantinos Konstantinides and Kung Yao, "Statistical analysis
      of effective singular values in matrix rank determination", IEEE
      Transactions on Acoustics, Speech and Signal Processing, 36(5), 1988,
      757-763.
      `doi:10.1109/29.1585 <http://dx.doi.org/10.1109/29.1585>`_

.. function:: nullspace(M)

   .. Docstring generated from Julia source

   Basis for nullspace of ``M``\ .

.. function:: repmat(A, n, m)

   .. Docstring generated from Julia source

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.

.. function:: repeat(A, inner = Int[], outer = Int[])

   .. Docstring generated from Julia source

   Construct an array by repeating the entries of ``A``\ . The i-th element of ``inner`` specifies the number of times that the individual entries of the i-th dimension of ``A`` should be repeated. The i-th element of ``outer`` specifies the number of times that a slice along the i-th dimension of ``A`` should be repeated.

.. function:: kron(A, B)

   .. Docstring generated from Julia source

   Kronecker tensor product of two vectors or two matrices.

.. function:: blkdiag(A...)

   .. Docstring generated from Julia source

   Concatenate matrices block-diagonally. Currently only implemented for sparse matrices.

.. function:: linreg(x, y) -> a, b

   .. Docstring generated from Julia source

   Perform linear regression. Returns ``a`` and ``b`` such that ``a + b*x`` is the closest straight line to the given points ``(x, y)``\ , i.e., such that the squared error between ``y`` and ``a + b*x`` is minimized.

   **Example**:

   .. code-block:: julia

          using PyPlot
          x = [1.0:12.0;]
          y = [5.5, 6.3, 7.6, 8.8, 10.9, 11.79, 13.48, 15.02, 17.77, 20.81, 22.0, 22.99]
          a, b = linreg(x, y)          # Linear regression
          plot(x, y, "o")              # Plot (x, y) points
          plot(x, [a+b*i for i in x])  # Plot line determined by linear regression

.. function:: linreg(x, y, w)

   .. Docstring generated from Julia source

   Weighted least-squares linear regression.

.. function:: expm(A)

   .. Docstring generated from Julia source

   Compute the matrix exponential of ``A``, defined by

   .. math::

      e^A = \sum_{n=0}^{\infty} \frac{A^n}{n!}.

   For symmetric or Hermitian ``A``, an eigendecomposition (:func:`eigfact`) is used, otherwise the scaling and squaring algorithm (see [H05]_) is chosen.

   .. [H05] Nicholas J. Higham, "The squaring and scaling method for the matrix
      exponential revisited", SIAM Journal on Matrix Analysis and Applications,
      26(4), 2005, 1179-1193.
      `doi:10.1137/090768539 <http://dx.doi.org/10.1137/090768539>`_

.. function:: logm(A)

   .. Docstring generated from Julia source

   If ``A`` has no negative real eigenvalue, compute the principal matrix logarithm of ``A``, i.e. the unique matrix :math:`X` such that :math:`e^X = A` and :math:`-\pi < Im(\lambda) < \pi` for all the eigenvalues :math:`\lambda` of :math:`X`. If ``A`` has nonpositive eigenvalues, a warning is printed and whenever possible a nonprincipal matrix function is returned.

   If ``A`` is symmetric or Hermitian, its eigendecomposition (:func:`eigfact`) is used, if ``A`` is triangular an improved version of the inverse scaling and squaring method is employed (see [AH12]_ and [AHR13]_). For general matrices, the complex Schur form (:func:`schur`) is computed and the triangular algorithm is used on the triangular factor.

   .. [AH12] Awad H. Al-Mohy and Nicholas J. Higham, "Improved inverse  scaling
      and squaring algorithms for the matrix logarithm", SIAM Journal on
      Scientific Computing, 34(4), 2012, C153-C169.
      `doi:10.1137/110852553 <http://dx.doi.org/10.1137/110852553>`_
   .. [AHR13] Awad H. Al-Mohy, Nicholas J. Higham and Samuel D. Relton,
      "Computing the Fréchet derivative of the matrix logarithm and estimating
      the condition number", SIAM Journal on Scientific Computing, 35(4), 2013,
      C394-C410.
      `doi:10.1137/120885991 <http://dx.doi.org/10.1137/120885991>`_

.. function:: sqrtm(A)

   .. Docstring generated from Julia source

   If ``A`` has no negative real eigenvalues, compute the principal matrix square root of ``A``, that is the unique matrix :math:`X` with eigenvalues having positive real part such that :math:`X^2 = A`. Otherwise, a nonprincipal square root is returned.

   If ``A`` is symmetric or Hermitian, its eigendecomposition (:func:`eigfact`) is used to compute the square root. Otherwise, the square root is determined by means of the Björck-Hammarling method, which computes the complex Schur form (:func:`schur`) and then the complex square root of the triangular factor.

   .. [BH83] Åke Björck and Sven Hammarling, "A Schur method for the square root
      of a matrix", Linear Algebra and its Applications, 52-53, 1983, 127-140.
      `doi:10.1016/0024-3795(83)80010-X <http://dx.doi.org/10.1016/0024-3795(83)80010-X>`_

.. function:: lyap(A, C)

   .. Docstring generated from Julia source

   Computes the solution ``X`` to the continuous Lyapunov equation ``AX + XA' + C = 0``\ , where no eigenvalue of ``A`` has a zero real part and no two eigenvalues are negative complex conjugates of each other.

.. function:: sylvester(A, B, C)

   .. Docstring generated from Julia source

   Computes the solution ``X`` to the Sylvester equation ``AX + XB + C = 0``\ , where ``A``\ , ``B`` and ``C`` have compatible dimensions and ``A`` and ``-B`` have no eigenvalues with equal real part.

.. function:: issym(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is symmetric.

.. function:: isposdef(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is positive definite.

.. function:: isposdef!(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is positive definite, overwriting ``A`` in the processes.

.. function:: istril(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is lower triangular.

.. function:: istriu(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is upper triangular.

.. function:: isdiag(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is diagonal.

.. function:: ishermitian(A) -> Bool

   .. Docstring generated from Julia source

   Test whether a matrix is Hermitian.

.. function:: transpose(A)

   .. Docstring generated from Julia source

   The transposition operator (``.'``\ ).

.. function:: transpose!(dest,src)

   .. Docstring generated from Julia source

   Transpose array ``src`` and store the result in the preallocated array ``dest``\ , which should have a size corresponding to ``(size(src,2),size(src,1))``\ . No in-place transposition is supported and unexpected results will happen if ``src`` and ``dest`` have overlapping memory regions.

.. function:: ctranspose(A)

   .. Docstring generated from Julia source

   The conjugate transposition operator (``'``\ ).

.. function:: ctranspose!(dest,src)

   .. Docstring generated from Julia source

   Conjugate transpose array ``src`` and store the result in the preallocated array ``dest``\ , which should have a size corresponding to ``(size(src,2),size(src,1))``\ . No in-place transposition is supported and unexpected results will happen if ``src`` and ``dest`` have overlapping memory regions.

.. function:: eigs(A; nev=6, ncv=max(20,2*nev+1), which="LM", tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

   .. Docstring generated from Julia source

   Computes eigenvalues ``d`` of ``A`` using Lanczos or Arnoldi iterations for
   real symmetric or general nonsymmetric matrices respectively.

   The following keyword arguments are supported:
    * ``nev``: Number of eigenvalues
    * ``ncv``: Number of Krylov vectors used in the computation; should satisfy ``nev+1 <= ncv <= n`` for real symmetric problems and ``nev+2 <= ncv <= n`` for other problems, where ``n`` is the size of the input matrix ``A``. The default is ``ncv = max(20,2*nev+1)``.

      Note that these restrictions limit the input matrix ``A`` to be of dimension at least 2.
    * ``which``: type of eigenvalues to compute. See the note below.

      ========= ======================================================================================================================
      ``which`` type of eigenvalues
      ========= ======================================================================================================================
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
      =============== ================================== ==================================
      ``nothing``     ordinary (forward)                 :math:`A`
      real or complex inverse with level shift ``sigma`` :math:`(A - \sigma I )^{-1}`
      =============== ================================== ==================================

.. function:: eigs(A, B; nev=6, ncv=max(20,2*nev+1), which="LM", tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

   .. Docstring generated from Julia source

   Computes generalized eigenvalues ``d`` of ``A`` and ``B`` using Lanczos or Arnoldi iterations for
   real symmetric or general nonsymmetric matrices respectively.

   The following keyword arguments are supported:
    * ``nev``: Number of eigenvalues
    * ``ncv``: Number of Krylov vectors used in the computation; should satisfy ``nev+1 <= ncv <= n`` for real symmetric problems and ``nev+2 <= ncv <= n`` for other problems, where ``n`` is the size of the input matrices ``A`` and ``B``. The default is ``ncv = max(20,2*nev+1)``.

      Note that these restrictions limit the input matrix ``A`` to be of dimension at least 2.
    * ``which``: type of eigenvalues to compute. See the note below.

      ========= ======================================================================================================================
      ``which`` type of eigenvalues
      ========= ======================================================================================================================
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

   .. note:: The ``sigma`` and ``which`` keywords interact: the description of eigenvalues searched for by ``which`` do _not_ necessarily refer to the eigenvalue problem :math:`Av = Bv\lambda`, but rather the linear operator constructed by the specification of the iteration mode implied by ``sigma``.

      =============== ================================== ==================================
      ``sigma``       iteration mode                     ``which`` refers to the problem
      =============== ================================== ==================================
      ``nothing``     ordinary (forward)                 :math:`Av = Bv\lambda`
      real or complex inverse with level shift ``sigma`` :math:`(A - \sigma B )^{-1}B = v\nu`
      =============== ================================== ==================================

.. function:: svds(A; nsv=6, ritzvec=true, tol=0.0, maxiter=1000) -> (left_sv, s, right_sv, nconv, niter, nmult, resid)

   .. Docstring generated from Julia source

   ``svds`` computes largest singular values ``s`` of ``A`` using Lanczos or Arnoldi iterations.
   Uses :func:`eigs` underneath.

   Inputs are:
    * ``A``: Linear operator. It can either subtype of ``AbstractArray`` (e.g., sparse matrix) or duck typed. For duck typing ``A`` has to support ``size(A)``, ``eltype(A)``, ``A * vector`` and ``A' * vector``.
    * ``nsv``: Number of singular values.
    * ``ritzvec``: Whether to return the left and right singular vectors ``left_sv`` and ``right_sv``, default is ``true``. If ``false`` the singular vectors are omitted from the output.
    * ``tol``: tolerance, see :func:`eigs`.
    * ``maxiter``: Maximum number of iterations, see :func:`eigs`.

   **Example**::

      X = sprand(10, 5, 0.2)
      svds(X, nsv = 2)

.. function:: peakflops(n; parallel=false)

   .. Docstring generated from Julia source

   ``peakflops`` computes the peak flop rate of the computer by using double precision :func:`Base.LinAlg.BLAS.gemm!`. By default, if no arguments are specified, it multiplies a matrix of size ``n x n``, where ``n = 2000``. If the underlying BLAS is using multiple threads, higher flop rates are realized. The number of BLAS threads can be set with ``blas_set_num_threads(n)``.

   If the keyword argument ``parallel`` is set to ``true``, ``peakflops`` is run in parallel on all the worker processors. The flop rate of the entire parallel computer is returned. When running in parallel, only 1 BLAS thread is used. The argument ``n`` still refers to the size of the problem that is solved on each processor.

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

   .. Docstring generated from Julia source

   Dot product of two vectors consisting of ``n`` elements of array ``X`` with stride ``incx`` and ``n`` elements of array ``Y`` with stride ``incy``\ .

.. function:: dotu(n, X, incx, Y, incy)

   .. Docstring generated from Julia source

   Dot function for two complex vectors.

.. function:: dotc(n, X, incx, U, incy)

   .. Docstring generated from Julia source

   Dot function for two complex vectors conjugating the first vector.

.. function:: blascopy!(n, X, incx, Y, incy)

   .. Docstring generated from Julia source

   Copy ``n`` elements of array ``X`` with stride ``incx`` to array ``Y`` with stride ``incy``\ . Returns ``Y``\ .

.. function:: nrm2(n, X, incx)

   .. Docstring generated from Julia source

   2-norm of a vector consisting of ``n`` elements of array ``X`` with stride ``incx``\ .

.. function:: asum(n, X, incx)

   .. Docstring generated from Julia source

   sum of the absolute values of the first ``n`` elements of array ``X`` with stride ``incx``\ .

.. function:: axpy!(a, X, Y)

   .. Docstring generated from Julia source

   Overwrite ``Y`` with ``a*X + Y``\ . Returns ``Y``\ .

.. function:: scal!(n, a, X, incx)

   .. Docstring generated from Julia source

   Overwrite ``X`` with ``a*X``\ . Returns ``X``\ .

.. function:: scal(n, a, X, incx)

   .. Docstring generated from Julia source

   Returns ``a*X``\ .

.. function:: ger!(alpha, x, y, A)

   .. Docstring generated from Julia source

   Rank-1 update of the matrix ``A`` with vectors ``x`` and ``y`` as ``alpha*x*y' + A``\ .

.. function:: syr!(uplo, alpha, x, A)

   .. Docstring generated from Julia source

   Rank-1 update of the symmetric matrix ``A`` with vector ``x`` as ``alpha*x*x.' + A``\ . When ``uplo`` is 'U' the upper triangle of ``A`` is updated ('L' for lower triangle). Returns ``A``\ .

.. function:: syrk!(uplo, trans, alpha, A, beta, C)

   .. Docstring generated from Julia source

   Rank-k update of the symmetric matrix ``C`` as ``alpha*A*A.' + beta*C`` or ``alpha*A.'*A + beta*C`` according to whether ``trans`` is 'N' or 'T'. When ``uplo`` is 'U' the upper triangle of ``C`` is updated ('L' for lower triangle). Returns ``C``\ .

.. function:: syrk(uplo, trans, alpha, A)

   .. Docstring generated from Julia source

   Returns either the upper triangle or the lower triangle, according to ``uplo`` ('U' or 'L'), of ``alpha*A*A.'`` or ``alpha*A.'*A``\ , according to ``trans`` ('N' or 'T').

.. function:: her!(uplo, alpha, x, A)

   .. Docstring generated from Julia source

   Methods for complex arrays only. Rank-1 update of the Hermitian matrix ``A`` with vector ``x`` as ``alpha*x*x' + A``\ . When ``uplo`` is 'U' the upper triangle of ``A`` is updated ('L' for lower triangle). Returns ``A``\ .

.. function:: herk!(uplo, trans, alpha, A, beta, C)

   .. Docstring generated from Julia source

   Methods for complex arrays only. Rank-k update of the Hermitian matrix ``C`` as ``alpha*A*A' + beta*C`` or ``alpha*A'*A + beta*C`` according to whether ``trans`` is 'N' or 'T'. When ``uplo`` is 'U' the upper triangle of ``C`` is updated ('L' for lower triangle). Returns ``C``\ .

.. function:: herk(uplo, trans, alpha, A)

   .. Docstring generated from Julia source

   Methods for complex arrays only. Returns either the upper triangle or the lower triangle, according to ``uplo`` ('U' or 'L'), of ``alpha*A*A'`` or ``alpha*A'*A``\ , according to ``trans`` ('N' or 'T').

.. function:: gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   .. Docstring generated from Julia source

   Update vector ``y`` as ``alpha*A*x + beta*y`` or ``alpha*A'*x + beta*y`` according to ``trans`` ('N' or 'T'). The matrix ``A`` is a general band matrix of dimension ``m`` by ``size(A,2)`` with ``kl`` sub-diagonals and ``ku`` super-diagonals. Returns the updated ``y``\ .

.. function:: gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   .. Docstring generated from Julia source

   Returns ``alpha*A*x`` or ``alpha*A'*x`` according to ``trans`` ('N' or 'T'). The matrix ``A`` is a general band matrix of dimension ``m`` by ``size(A,2)`` with ``kl`` sub-diagonals and ``ku`` super-diagonals.

.. function:: sbmv!(uplo, k, alpha, A, x, beta, y)

   .. Docstring generated from Julia source

   Update vector ``y`` as ``alpha*A*x + beta*y`` where ``A`` is a a symmetric band matrix of order ``size(A,2)`` with ``k`` super-diagonals stored in the argument ``A``\ . The storage layout for ``A`` is described the reference BLAS module, level-2 BLAS at <http://www.netlib.org/lapack/explore-html/>.

   Returns the updated ``y``\ .

.. function:: sbmv(uplo, k, alpha, A, x)

   .. Docstring generated from Julia source

   Returns ``alpha*A*x`` where ``A`` is a symmetric band matrix of order ``size(A,2)`` with ``k`` super-diagonals stored in the argument ``A``\ .

.. function:: sbmv(uplo, k, A, x)

   .. Docstring generated from Julia source

   Returns ``A*x`` where ``A`` is a symmetric band matrix of order ``size(A,2)`` with ``k`` super-diagonals stored in the argument ``A``\ .

.. function:: gemm!(tA, tB, alpha, A, B, beta, C)

   .. Docstring generated from Julia source

   Update ``C`` as ``alpha*A*B + beta*C`` or the other three variants according to ``tA`` (transpose ``A``\ ) and ``tB``\ . Returns the updated ``C``\ .

.. function:: gemm(tA, tB, alpha, A, B)

   .. Docstring generated from Julia source

   Returns ``alpha*A*B`` or the other three variants according to ``tA`` (transpose ``A``\ ) and ``tB``\ .

.. function:: gemm(tA, tB, A, B)

   .. Docstring generated from Julia source

   Returns ``A*B`` or the other three variants according to ``tA`` (transpose ``A``\ ) and ``tB``\ .

.. function:: gemv!(tA, alpha, A, x, beta, y)

   .. Docstring generated from Julia source

   Update the vector ``y`` as ``alpha*A*x + beta*y`` or ``alpha*A'x + beta*y`` according to ``tA`` (transpose ``A``\ ). Returns the updated ``y``\ .

.. function:: gemv(tA, alpha, A, x)

   .. Docstring generated from Julia source

   Returns ``alpha*A*x`` or ``alpha*A'x`` according to ``tA`` (transpose ``A``\ ).

.. function:: gemv(tA, A, x)

   .. Docstring generated from Julia source

   Returns ``A*x`` or ``A'x`` according to ``tA`` (transpose ``A``\ ).

.. function:: symm!(side, ul, alpha, A, B, beta, C)

   .. Docstring generated from Julia source

   Update ``C`` as ``alpha*A*B + beta*C`` or ``alpha*B*A + beta*C`` according to ``side``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used. Returns the updated ``C``\ .

.. function:: symm(side, ul, alpha, A, B)

   .. Docstring generated from Julia source

   Returns ``alpha*A*B`` or ``alpha*B*A`` according to ``side``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used.

.. function:: symm(side, ul, A, B)

   .. Docstring generated from Julia source

   Returns ``A*B`` or ``B*A`` according to ``side``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used.

.. function:: symm(tA, tB, alpha, A, B)

   .. Docstring generated from Julia source

   Returns ``alpha*A*B`` or the other three variants according to ``tA`` (transpose ``A``\ ) and ``tB``\ .

.. function:: symv!(ul, alpha, A, x, beta, y)

   .. Docstring generated from Julia source

   Update the vector ``y`` as ``alpha*A*x + beta*y``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used. Returns the updated ``y``\ .

.. function:: symv(ul, alpha, A, x)

   .. Docstring generated from Julia source

   Returns ``alpha*A*x``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used.

.. function:: symv(ul, A, x)

   .. Docstring generated from Julia source

   Returns ``A*x``\ . ``A`` is assumed to be symmetric. Only the ``ul`` triangle of ``A`` is used.

.. function:: trmm!(side, ul, tA, dA, alpha, A, B)

   .. Docstring generated from Julia source

   Update ``B`` as ``alpha*A*B`` or one of the other three variants determined by ``side`` (``A`` on left or right) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones). Returns the updated ``B``\ .

.. function:: trmm(side, ul, tA, dA, alpha, A, B)

   .. Docstring generated from Julia source

   Returns ``alpha*A*B`` or one of the other three variants determined by ``side`` (``A`` on left or right) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: trsm!(side, ul, tA, dA, alpha, A, B)

   .. Docstring generated from Julia source

   Overwrite ``B`` with the solution to ``A*X = alpha*B`` or one of the other three variants determined by ``side`` (``A`` on left or right of ``X``\ ) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones). Returns the updated ``B``\ .

.. function:: trsm(side, ul, tA, dA, alpha, A, B)

   .. Docstring generated from Julia source

   Returns the solution to ``A*X = alpha*B`` or one of the other three variants determined by ``side`` (``A`` on left or right of ``X``\ ) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: trmv!(side, ul, tA, dA, alpha, A, b)

   .. Docstring generated from Julia source

   Update ``b`` as ``alpha*A*b`` or one of the other three variants determined by ``side`` (``A`` on left or right) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones). Returns the updated ``b``\ .

.. function:: trmv(side, ul, tA, dA, alpha, A, b)

   .. Docstring generated from Julia source

   Returns ``alpha*A*b`` or one of the other three variants determined by ``side`` (``A`` on left or right) and ``tA`` (transpose ``A``\ ). Only the ``ul`` triangle of ``A`` is used. ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: trsv!(ul, tA, dA, A, b)

   .. Docstring generated from Julia source

   Overwrite ``b`` with the solution to ``A*x = b`` or one of the other two variants determined by ``tA`` (transpose ``A``\ ) and ``ul`` (triangle of ``A`` used). ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones). Returns the updated ``b``\ .

.. function:: trsv(ul, tA, dA, A, b)

   .. Docstring generated from Julia source

   Returns the solution to ``A*x = b`` or one of the other two variants determined by ``tA`` (transpose ``A``\ ) and ``ul`` (triangle of ``A`` is used.) ``dA`` indicates if ``A`` is unit-triangular (the diagonal is assumed to be all ones).

.. function:: blas_set_num_threads(n)

   .. Docstring generated from Julia source

   Set the number of threads the BLAS library should use.

.. data:: I

   An object of type ``UniformScaling``, representing an identity matrix of any size.

LAPACK Functions
----------------

.. module:: Base.LinAlg.LAPACK

:mod:`Base.LinAlg.LAPACK` provides wrappers for some of the LAPACK functions for
linear algebra.  Those functions that overwrite one of the input
arrays have names ending in ``'!'``.

Usually a function has 4 methods defined, one each for ``Float64``,
``Float32``, ``Complex128`` and ``Complex64`` arrays.

Note that the LAPACK API provided by Julia can and will change in the future. Since
this API is not user-facing, there is no commitment to support/deprecate this specific
set of functions in future releases.

.. currentmodule:: Base.LinAlg.LAPACK

.. function:: gbtrf!(kl, ku, m, AB) -> (AB, ipiv)

   .. Docstring generated from Julia source

   Compute the LU factorization of a banded matrix ``AB``\ . ``kl`` is the first subdiagonal containing a nonzero band, ``ku`` is the last superdiagonal containing one, and ``m`` is the first dimension of the matrix ``AB``\ . Returns the LU factorization in-place and ``ipiv``\ , the vector of pivots used.

.. function:: gbtrs!(trans, kl, ku, m, AB, ipiv, B)

   .. Docstring generated from Julia source

   Solve the equation ``AB * X = B``\ . ``trans`` determines the orientation of ``AB``\ . It may be ``N`` (no transpose), ``T`` (transpose), or ``C`` (conjugate transpose). ``kl`` is the first subdiagonal containing a nonzero band, ``ku`` is the last superdiagonal containing one, and ``m`` is the first dimension of the matrix ``AB``\ . ``ipiv`` is the vector of pivots returned from ``gbtrf!``\ . Returns the vector or matrix ``X``\ , overwriting ``B`` in-place.

.. function:: gebal!(job, A) -> (ilo, ihi, scale)

   .. Docstring generated from Julia source

   Balance the matrix ``A`` before computing its eigensystem or Schur factorization. ``job`` can be one of ``N`` (``A`` will not be permuted or scaled), ``P`` (``A`` will only be permuted), ``S`` (``A`` will only be scaled), or ``B`` (``A`` will be both permuted and scaled). Modifies ``A`` in-place and returns ``ilo``\ , ``ihi``\ , and ``scale``\ . If permuting was turned on, ``A[i,j] = 0`` if ``j > i`` and ``1 < j < ilo`` or ``j > ihi``\ . ``scale`` contains information about the scaling/permutations performed.

.. function:: gebak!(job, side, ilo, ihi, scale, V)

   .. Docstring generated from Julia source

   Transform the eigenvectors ``V`` of a matrix balanced using ``gebal!`` to the unscaled/unpermuted eigenvectors of the original matrix. Modifies ``V`` in-place. ``side`` can be ``L`` (left eigenvectors are transformed) or ``R`` (right eigenvectors are transformed).

.. function:: gebrd!(A) -> (A, d, e, tauq, taup)

   .. Docstring generated from Julia source

   Reduce ``A`` in-place to bidiagonal form ``A = QBP'``\ . Returns ``A``\ , containing the bidiagonal matrix ``B``\ ; ``d``\ , containing the diagonal elements of ``B``\ ; ``e``\ , containing the off-diagonal elements of ``B``\ ; ``tauq``\ , containing the elementary reflectors representing ``Q``\ ; and ``taup``\ , containing the elementary reflectors representing ``P``\ .

.. function:: gelqf!(A, tau)

   .. Docstring generated from Julia source

   Compute the ``LQ`` factorization of ``A``\ , ``A = LQ``\ . ``tau`` contains scalars which parameterize the elementary reflectors of the factorization. ``tau`` must have length greater than or equal to the smallest dimension of ``A``\ .

   Returns ``A`` and ``tau`` modified in-place.

.. function:: gelqf!(A) -> (A, tau)

   .. Docstring generated from Julia source

   Compute the ``LQ`` factorization of ``A``\ , ``A = LQ``\ .

   Returns ``A``\ , modified in-place, and ``tau``\ , which contains scalars which parameterize the elementary reflectors of the factorization.

.. function:: geqlf!(A, tau)

   .. Docstring generated from Julia source

   Compute the ``QL`` factorization of ``A``\ , ``A = QL``\ . ``tau`` contains scalars which parameterize the elementary reflectors of the factorization. ``tau`` must have length greater than or equal to the smallest dimension of ``A``\ .

   Returns ``A`` and ``tau`` modified in-place.

.. function:: geqlf!(A) -> (A, tau)

   .. Docstring generated from Julia source

   Compute the ``QL`` factorization of ``A``\ , ``A = QL``\ .

   Returns ``A``\ , modified in-place, and ``tau``\ , which contains scalars which parameterize the elementary reflectors of the factorization.

.. function:: geqrf!(A, tau)

   .. Docstring generated from Julia source

   Compute the ``QR`` factorization of ``A``\ , ``A = QR``\ . ``tau`` contains scalars which parameterize the elementary reflectors of the factorization. ``tau`` must have length greater than or equal to the smallest dimension of ``A``\ .

   Returns ``A`` and ``tau`` modified in-place.

.. function:: geqrf!(A) -> (A, tau)

   .. Docstring generated from Julia source

   Compute the ``QR`` factorization of ``A``\ , ``A = QR``\ .

   Returns ``A``\ , modified in-place, and ``tau``\ , which contains scalars which parameterize the elementary reflectors of the factorization.

.. function:: geqp3!(A, jpvt, tau)

   .. Docstring generated from Julia source

   Compute the pivoted ``QR`` factorization of ``A``\ , ``AP = QR`` using BLAS level 3. ``P`` is a pivoting matrix, represented by ``jpvt``\ . ``tau`` stores the elementary reflectors. ``jpvt`` must have length length greater than or equal to ``n`` if ``A`` is an ``(m x n)`` matrix. ``tau`` must have length greater than or equal to the smallest dimension of ``A``\ .

   ``A``\ , ``jpvt``\ , and ``tau`` are modified in-place.

.. function:: geqp3!(A, jpvt) -> (A, jpvt, tau)

   .. Docstring generated from Julia source

   Compute the pivoted ``QR`` factorization of ``A``\ , ``AP = QR`` using BLAS level 3. ``P`` is a pivoting matrix, represented by ``jpvt``\ . ``jpvt`` must have length greater than or equal to ``n`` if ``A`` is an ``(m x n)`` matrix.

   Returns ``A`` and ``jpvt``\ , modified in-place, and ``tau``\ , which stores the elementary reflectors.

.. function:: geqp3!(A) -> (A, jpvt, tau)

   .. Docstring generated from Julia source

   Compute the pivoted ``QR`` factorization of ``A``\ , ``AP = QR`` using BLAS level 3.

   Returns ``A``\ , modified in-place, ``jpvt``\ , which represents the pivoting matrix ``P``\ , and ``tau``\ , which stores the elementary reflectors.

.. function:: gerqf!(A, tau)

   .. Docstring generated from Julia source

   Compute the ``RQ`` factorization of ``A``\ , ``A = RQ``\ . ``tau`` contains scalars which parameterize the elementary reflectors of the factorization. ``tau`` must have length greater than or equal to the smallest dimension of ``A``\ .

   Returns ``A`` and ``tau`` modified in-place.

.. function:: gerqf!(A) -> (A, tau)

   .. Docstring generated from Julia source

   Compute the ``RQ`` factorization of ``A``\ , ``A = RQ``\ .

   Returns ``A``\ , modified in-place, and ``tau``\ , which contains scalars which parameterize the elementary reflectors of the factorization.

.. function:: geqrt!(A, T)

   .. Docstring generated from Julia source

   Compute the blocked ``QR`` factorization of ``A``\ , ``A = QR``\ . ``T`` contains upper triangular block reflectors which parameterize the elementary reflectors of the factorization. The first dimension of ``T`` sets the block size and it must be between 1 and ``n``\ . The second dimension of ``T`` must equal the smallest dimension of ``A``\ .

   Returns ``A`` and ``T`` modified in-place.

.. function:: geqrt!(A, nb) -> (A, T)

   .. Docstring generated from Julia source

   Compute the blocked ``QR`` factorization of ``A``\ , ``A = QR``\ . ``nb`` sets the block size and it must be between 1 and ``n``\ , the second dimension of ``A``\ .

   Returns ``A``\ , modified in-place, and ``T``\ , which contains upper triangular block reflectors which parameterize the elementary reflectors of the factorization.

.. function:: geqrt3!(A, T)

   .. Docstring generated from Julia source

   Recursively computes the blocked ``QR`` factorization of ``A``\ , ``A = QR``\ . ``T`` contains upper triangular block reflectors which parameterize the elementary reflectors of the factorization.  The first dimension of ``T`` sets the block size and it must be between 1 and ``n``\ . The second dimension of ``T`` must equal the smallest dimension of ``A``\ .

   Returns ``A`` and ``T`` modified in-place.

.. function:: geqrt3!(A) -> (A, T)

   .. Docstring generated from Julia source

   Recursively computes the blocked ``QR`` factorization of ``A``\ , ``A = QR``\ .

   Returns ``A``\ , modified in-place, and ``T``\ , which contains upper triangular block reflectors which parameterize the elementary reflectors of the factorization.

.. function:: getrf!(A) -> (A, ipiv, info)

   .. Docstring generated from Julia source

   Compute the pivoted ``LU`` factorization of ``A``\ , ``A = LU``\ .

   Returns ``A``\ , modified in-place, ``ipiv``\ , the pivoting information, and an ``info`` code which indicates success (``info = 0``\ ), a singular value in ``U`` (``info = i``\ , in which case ``U[i,i]`` is singular), or an error code (``info < 0``\ ).

.. function:: tzrzf!(A) -> (A, tau)

   .. Docstring generated from Julia source

   Transforms the upper trapezoidal matrix ``A`` to upper triangular form in-place. Returns ``A`` and ``tau``\ , the scalar parameters for the elementary reflectors of the transformation.

.. function:: ormrz!(side, trans, A, tau, C)

   .. Docstring generated from Julia source

   Multiplies the matrix ``C`` by ``Q`` from the transformation supplied by ``tzrzf!``\ . Depending on ``side`` or ``trans`` the multiplication can be left-sided (``side = L, Q*C``\ ) or right-sided (``side = R, C*Q``\ ) and ``Q`` can be unmodified (``trans = N``\ ), transposed (``trans = T``\ ), or conjugate transposed (``trans = C``\ ). Returns matrix ``C`` which is modified in-place with the result of the multiplication.

.. function:: gels!(trans, A, B) -> (F, B, ssr)

   .. Docstring generated from Julia source

   Solves the linear equation ``A * X = B``\ , ``A.' * X =B``\ , or ``A' * X = B`` using a QR or LQ factorization. Modifies the matrix/vector ``B`` in place with the solution. ``A`` is overwritten with its ``QR`` or ``LQ`` factorization. ``trans`` may be one of ``N`` (no modification), ``T`` (transpose), or ``C`` (conjugate transpose). ``gels!`` searches for the minimum norm/least squares solution. ``A`` may be under or over determined. The solution is returned in ``B``\ .

.. function:: gesv!(A, B) -> (B, A, ipiv)

   .. Docstring generated from Julia source

   Solves the linear equation ``A * X = B`` where ``A`` is a square matrix using the ``LU`` factorization of ``A``\ . ``A`` is overwritten with its ``LU`` factorization and ``B`` is overwritten with the solution ``X``\ . ``ipiv`` contains the pivoting information for the ``LU`` factorization of ``A``\ .

.. function:: getrs!(trans, A, ipiv, B)

   .. Docstring generated from Julia source

   Solves the linear equation ``A * X = B``\ , ``A.' * X =B``\ , or ``A' * X = B`` for square ``A``\ . Modifies the matrix/vector ``B`` in place with the solution. ``A`` is the ``LU`` factorization from ``getrf!``\ , with ``ipiv`` the pivoting information. ``trans`` may be one of ``N`` (no modification), ``T`` (transpose), or ``C`` (conjugate transpose).

.. function:: getri!(A, ipiv)

   .. Docstring generated from Julia source

   Computes the inverse of ``A``\ , using its ``LU`` factorization found by ``getrf!``\ . ``ipiv`` is the pivot information output and ``A`` contains the ``LU`` factorization of ``getrf!``\ . ``A`` is overwritten with its inverse.

.. function:: gesvx!(fact, trans, A, AF, ipiv, equed, R, C, B) -> (X, equed, R, C, B, rcond, ferr, berr, work)

   .. Docstring generated from Julia source

   Solves the linear equation ``A * X = B`` (``trans = N``\ ), ``A.' * X =B`` (``trans = T``\ ), or ``A' * X = B`` (``trans = C``\ ) using the ``LU`` factorization of ``A``\ . ``fact`` may be ``E``\ , in which case ``A`` will be equilibrated and copied to ``AF``\ ; ``F``\ , in which case ``AF`` and ``ipiv`` from a previous ``LU`` factorization are inputs; or ``N``\ , in which case ``A`` will be copied to ``AF`` and then factored. If ``fact = F``\ , ``equed`` may be ``N``\ , meaning ``A`` has not been equilibrated; ``R``\ , meaning ``A`` was multiplied by ``diagm(R)`` from the left; ``C``\ , meaning ``A`` was multiplied by ``diagm(C)`` from the right; or ``B``\ , meaning ``A`` was multiplied by ``diagm(R)`` from the left and ``diagm(C)`` from the right. If ``fact = F`` and ``equed = R`` or ``B`` the elements of ``R`` must all be positive. If ``fact = F`` and ``equed = C`` or ``B`` the elements of ``C`` must all be positive.

   Returns the solution ``X``\ ; ``equed``\ , which is an output if ``fact`` is not ``N``\ , and describes the equilibration that was performed; ``R``\ , the row equilibration diagonal; ``C``\ , the column equilibration diagonal; ``B``\ , which may be overwritten with its equilibrated form ``diagm(R)*B`` (if ``trans = N`` and ``equed = R,B``\ ) or ``diagm(C)*B`` (if ``trans = T,C`` and ``equed = C,B``\ ); ``rcond``\ , the reciprocal condition number of ``A`` after equilbrating; ``ferr``\ , the forward error bound for each solution vector in ``X``\ ; ``berr``\ , the forward error bound for each solution vector in ``X``\ ; and ``work``\ , the reciprocal pivot growth factor.

.. function:: gesvx!(A, B)

   .. Docstring generated from Julia source

   The no-equilibration, no-transpose simplification of ``gesvx!``\ .

.. function:: gelsd!(A, B, rcond) -> (B, rnk)

   .. Docstring generated from Julia source

   Computes the least norm solution of ``A * X = B`` by finding the ``SVD`` factorization of ``A``\ , then dividing-and-conquering the problem. ``B`` is overwritten with the solution ``X``\ . Singular values below ``rcond`` will be treated as zero. Returns the solution in ``B`` and the effective rank of ``A`` in ``rnk``\ .

.. function:: gelsy!(A, B, rcond) -> (B, rnk)

   .. Docstring generated from Julia source

   Computes the least norm solution of ``A * X = B`` by finding the full ``QR`` factorization of ``A``\ , then dividing-and-conquering the problem. ``B`` is overwritten with the solution ``X``\ . Singular values below ``rcond`` will be treated as zero. Returns the solution in ``B`` and the effective rank of ``A`` in ``rnk``\ .

.. function:: gglse!(A, c, B, d) -> (X,res)

   .. Docstring generated from Julia source

   Solves the equation ``A * x = c`` where ``x`` is subject to the equality constraint ``B * x = d``\ . Uses the formula ``||c - A*x||^2 = 0`` to solve. Returns ``X`` and the residual sum-of-squares.

.. function:: geev!(jobvl, jobvr, A) -> (W, VL, VR)

   .. Docstring generated from Julia source

   Finds the eigensystem of ``A``\ . If ``jobvl = N``\ , the left eigenvectors of ``A`` aren't computed. If ``jobvr = N``\ , the right eigenvectors of ``A`` aren't computed. If ``jobvl = V`` or ``jobvr = V``\ , the corresponding eigenvectors are computed. Returns the eigenvalues in ``W``\ , the right eigenvectors in ``VR``\ , and the left eigenvectors in ``VL``\ .

.. function:: gesdd!(job, A) -> (U, S, VT)

   .. Docstring generated from Julia source

   Finds the singular value decomposition of ``A``\ , ``A = U * S * V'``\ , using a divide and conquer approach. If ``job = A``\ , all the columns of ``U`` and the rows of ``V'`` are computed. If ``job = N``\ , no columns of ``U`` or rows of ``V'`` are computed. If ``job = O``\ , ``A`` is overwritten with the columns of (thin) ``U`` and the rows of (thin) ``V'``\ . If ``job = S``\ , the columns of (thin) ``U`` and the rows of (thin) ``V'`` are computed and returned separately.

.. function:: gesvd!(jobu, jobvt, A) -> (U, S, VT)

   .. Docstring generated from Julia source

   Finds the singular value decomposition of ``A``\ , ``A = U * S * V'``\ . If ``jobu = A``\ , all the columns of ``U`` are computed. If ``jobvt = A`` all the rows of ``V'`` are computed. If ``jobu = N``\ , no columns of ``U`` are computed. If ``jobvt = N`` no rows of ``V'`` are computed. If ``jobu = O``\ , ``A`` is overwritten with the columns of (thin) ``U``\ . If ``jobvt = O``\ , ``A`` is overwritten with the rows of (thin) ``V'``\ . If ``jobu = S``\ , the columns of (thin) ``U`` are computed and returned separately. If ``jobvt = S`` the rows of (thin) ``V'`` are computed and returned separately. ``jobu`` and ``jobvt`` can't both be ``O``\ .

   Returns ``U``\ , ``S``\ , and ``Vt``\ , where ``S`` are the singular values of ``A``\ .

.. function:: ggsvd!(jobu, jobv, jobq, A, B) -> (U, V, Q, alpha, beta, k, l, R)

   .. Docstring generated from Julia source

   Finds the generalized singular value decomposition of ``A`` and ``B``\ , ``U'*A*Q = D1*R`` and ``V'*B*Q = D2*R``\ . ``D1`` has ``alpha`` on its diagonal and ``D2`` has ``beta`` on its diagonal. If ``jobu = U``\ , the orthogonal/unitary matrix ``U`` is computed. If ``jobv = V`` the orthogonal/unitary matrix ``V`` is computed. If ``jobq = Q``\ , the orthogonal/unitary matrix ``Q`` is computed. If ``job{u,v,q} = N``\ , that matrix is not computed.

.. function:: geevx!(balanc, jobvl, jobvr, sense, A) -> (A, w, VL, VR, ilo, ihi, scale, abnrm, rconde, rcondv)

   .. Docstring generated from Julia source

   Finds the eigensystem of ``A`` with matrix balancing. If ``jobvl = N``\ , the left eigenvectors of ``A`` aren't computed. If ``jobvr = N``\ , the right eigenvectors of ``A`` aren't computed. If ``jobvl = V`` or ``jobvr = V``\ , the corresponding eigenvectors are computed. If ``balanc = N``\ , no balancing is performed. If ``balanc = P``\ , ``A`` is permuted but not scaled. If ``balanc = S``\ , ``A`` is scaled but not permuted. If ``balanc = B``\ , ``A`` is permuted and scaled. If ``sense = N``\ , no reciprocal condition numbers are computed. If ``sense = E``\ , reciprocal condition numbers are computed for the eigenvalues only. If ``sense = V``\ , reciprocal condition numbers are computed for the right eigenvectors only. If ``sense = B``\ , reciprocal condition numbers are computed for the right eigenvectors and the eigenvectors. If ``sense = E,B``\ , the right and left eigenvectors must be computed.

.. function:: ggev!(jobvl, jobvr, A, B) -> (alpha, beta, vl, vr)

   .. Docstring generated from Julia source

   Finds the generalized eigendecomposition of ``A`` and ``B``\ . If ``jobvl = N``\ , the left eigenvectors aren't computed. If ``jobvr = N``\ , the right eigenvectors aren't computed. If ``jobvl = V`` or ``jobvr = V``\ , the corresponding eigenvectors are computed.

.. function:: gtsv!(dl, d, du, B)

   .. Docstring generated from Julia source

   Solves the equation ``A * X = B`` where ``A`` is a tridiagonal matrix with ``dl`` on the subdiagonal, ``d`` on the diagonal, and ``du`` on the superdiagonal.

   Overwrites ``B`` with the solution ``X`` and returns it.

.. function:: gttrf!(dl, d, du) -> (dl, d, du, du2, ipiv)

   .. Docstring generated from Julia source

   Finds the ``LU`` factorization of a tridiagonal matrix with ``dl`` on the subdiagonal, ``d`` on the diagonal, and ``du`` on the superdiagonal.

   Modifies ``dl``\ , ``d``\ , and ``du`` in-place and returns them and the second superdiagonal ``du2`` and the pivoting vector ``ipiv``\ .

.. function:: gttrs!(trans, dl, d, du, du2, ipiv, B)

   .. Docstring generated from Julia source

   Solves the equation ``A * X = B`` (``trans = N``\ ), ``A.' * X = B`` (``trans = T``\ ), or ``A' * X = B`` (``trans = C``\ ) using the ``LU`` factorization computed by ``gttrf!``\ . ``B`` is overwritten with the solution ``X``\ .

.. function:: orglq!(A, tau, k = length(tau))

   .. Docstring generated from Julia source

   Explicitly finds the matrix ``Q`` of a ``LQ`` factorization after calling ``gelqf!`` on ``A``\ . Uses the output of ``gelqf!``\ . ``A`` is overwritten by ``Q``\ .

.. function:: orgqr!(A, tau, k = length(tau))

   .. Docstring generated from Julia source

   Explicitly finds the matrix ``Q`` of a ``QR`` factorization after calling ``geqrf!`` on ``A``\ . Uses the output of ``geqrf!``\ . ``A`` is overwritten by ``Q``\ .

.. function:: orgql!(A, tau, k = length(tau))

   .. Docstring generated from Julia source

   Explicitly finds the matrix ``Q`` of a ``QL`` factorization after calling ``geqlf!`` on ``A``\ . Uses the output of ``geqlf!``\ . ``A`` is overwritten by ``Q``\ .

.. function:: orgrq!(A, tau, k = length(tau))

   .. Docstring generated from Julia source

   Explicitly finds the matrix ``Q`` of a ``RQ`` factorization after calling ``gerqf!`` on ``A``\ . Uses the output of ``gerqf!``\ . ``A`` is overwritten by ``Q``\ .

.. function:: ormlq!(side, trans, A, tau, C)

   .. Docstring generated from Julia source

   Computes ``Q * C`` (``trans = N``\ ), ``Q.' * C`` (``trans = T``\ ), ``Q' * C`` (``trans = C``\ ) for ``side = L`` or the equivalent right-sided multiplication for ``side = R`` using ``Q`` from a ``LQ`` factorization of ``A`` computed using ``gelqf!``\ . ``C`` is overwritten.

.. function:: ormqr!(side, trans, A, tau, C)

   .. Docstring generated from Julia source

   Computes ``Q * C`` (``trans = N``\ ), ``Q.' * C`` (``trans = T``\ ), ``Q' * C`` (``trans = C``\ ) for ``side = L`` or the equivalent right-sided multiplication for ``side = R`` using ``Q`` from a ``QR`` factorization of ``A`` computed using ``geqrf!``\ . ``C`` is overwritten.

.. function:: ormql!(side, trans, A, tau, C)

   .. Docstring generated from Julia source

   Computes ``Q * C`` (``trans = N``\ ), ``Q.' * C`` (``trans = T``\ ), ``Q' * C`` (``trans = C``\ ) for ``side = L`` or the equivalent right-sided multiplication for ``side = R`` using ``Q`` from a ``QL`` factorization of ``A`` computed using ``geqlf!``\ . ``C`` is overwritten.

.. function:: ormrq!(side, trans, A, tau, C)

   .. Docstring generated from Julia source

   Computes ``Q * C`` (``trans = N``\ ), ``Q.' * C`` (``trans = T``\ ), ``Q' * C`` (``trans = C``\ ) for ``side = L`` or the equivalent right-sided multiplication for ``side = R`` using ``Q`` from a ``RQ`` factorization of ``A`` computed using ``gerqf!``\ . ``C`` is overwritten.

.. function:: gemqrt!(side, trans, V, T, C)

   .. Docstring generated from Julia source

   Computes ``Q * C`` (``trans = N``\ ), ``Q.' * C`` (``trans = T``\ ), ``Q' * C`` (``trans = C``\ ) for ``side = L`` or the equivalent right-sided multiplication for ``side = R`` using ``Q`` from a ``QR`` factorization of ``A`` computed using ``geqrt!``\ . ``C`` is overwritten.

.. function:: posv!(uplo, A, B) -> (A, B)

   .. Docstring generated from Julia source

   Finds the solution to ``A * X = B`` where ``A`` is a symmetric or Hermitian positive definite matrix. If ``uplo = U`` the upper Cholesky decomposition of ``A`` is computed. If ``uplo = L`` the lower Cholesky decomposition of ``A`` is computed. ``A`` is overwritten by its Cholesky decomposition. ``B`` is overwritten with the solution ``X``\ .

.. function:: potrf!(uplo, A)

   .. Docstring generated from Julia source

   Computes the Cholesky (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) decomposition of positive-definite matrix ``A``\ . ``A`` is overwritten and returned with an info code.

.. function:: potri!(uplo, A)

   .. Docstring generated from Julia source

   Computes the inverse of positive-definite matrix ``A`` after calling ``potrf!`` to find its (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) Cholesky decomposition.

   ``A`` is overwritten by its inverse and returned.

.. function:: potrs!(uplo, A, B)

   .. Docstring generated from Julia source

   Finds the solution to ``A * X = B`` where ``A`` is a symmetric or Hermitian positive definite matrix whose Cholesky decomposition was computed by ``potrf!``\ . If ``uplo = U`` the upper Cholesky decomposition of ``A`` was computed. If ``uplo = L`` the lower Cholesky decomposition of ``A`` was computed. ``B`` is overwritten with the solution ``X``\ .

.. function:: pstrf!(uplo, A, tol) -> (A, piv, rank, info)

   .. Docstring generated from Julia source

   Computes the (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) pivoted Cholesky decomposition of positive-definite matrix ``A`` with a user-set tolerance ``tol``\ . ``A`` is overwritten by its Cholesky decomposition.

   Returns ``A``\ , the pivots ``piv``\ , the rank of ``A``\ , and an ``info`` code. If ``info = 0``\ , the factorization succeeded. If ``info = i > 0 `, then `A`` is indefinite or rank-deficient.

.. function:: ptsv!(D, E, B)

   .. Docstring generated from Julia source

   Solves ``A * X = B`` for positive-definite tridiagonal ``A``\ . ``D`` is the diagonal of ``A`` and ``E`` is the off-diagonal. ``B`` is overwritten with the solution ``X`` and returned.

.. function:: pttrf!(D, E)

   .. Docstring generated from Julia source

   Computes the LDLt factorization of a positive-definite tridiagonal matrix with ``D`` as diagonal and ``E`` as off-diagonal. ``D`` and ``E`` are overwritten and returned.

.. function:: pttrs!(D, E, B)

   .. Docstring generated from Julia source

   Solves ``A * X = B`` for positive-definite tridiagonal ``A`` with diagonal ``D`` and off-diagonal ``E`` after computing ``A``\ 's LDLt factorization using ``pttrf!``\ . ``B`` is overwritten with the solution ``X``\ .

.. function:: trtri!(uplo, diag, A)

   .. Docstring generated from Julia source

   Finds the inverse of (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) triangular matrix ``A``\ . If ``diag = N``\ , ``A`` has non-unit diagonal elements. If ``diag = U``\ , all diagonal elements of ``A`` are one. ``A`` is overwritten with its inverse.

.. function:: trtrs!(uplo, trans, diag, A, B)

   .. Docstring generated from Julia source

   Solves ``A * X = B`` (``trans = N``\ ), ``A.' * X = B`` (``trans = T``\ ), or ``A' * X = B`` (``trans = C``\ ) for (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) triangular matrix ``A``\ . If ``diag = N``\ , ``A`` has non-unit diagonal elements. If ``diag = U``\ , all diagonal elements of ``A`` are one. ``B`` is overwritten with the solution ``X``\ .

.. function:: trcon!(norm, uplo, diag, A)

   .. Docstring generated from Julia source

   Finds the reciprocal condition number of (upper if ``uplo = U``\ , lower if ``uplo = L``\ ) triangular matrix ``A``\ . If ``diag = N``\ , ``A`` has non-unit diagonal elements. If ``diag = U``\ , all diagonal elements of ``A`` are one. If ``norm = I``\ , the condition number is found in the infinity norm. If ``norm = O`` or ``1``\ , the condition number is found in the one norm.

.. function:: trevc!(side, howmny, select, T, VL = similar(T), VR = similar(T))

   .. Docstring generated from Julia source

   Finds the eigensystem of an upper triangular matrix ``T``\ . If ``side = R``\ , the right eigenvectors are computed. If ``side = L``\ , the left eigenvectors are computed. If ``side = B``\ , both sets are computed. If ``howmny = A``\ , all eigenvectors are found. If ``howmny = B``\ , all eigenvectors are found and backtransformed using ``VL`` and ``VR``\ . If ``howmny = S``\ , only the eigenvectors corresponding to the values in ``select`` are computed.

.. function:: trrfs!(uplo, trans, diag, A, B, X, Ferr, Berr) -> (Ferr, Berr)

   .. Docstring generated from Julia source

   Estimates the error in the solution to ``A * X = B`` (``trans = N``\ ), ``A.' * X = B`` (``trans = T``\ ), ``A' * X = B`` (``trans = C``\ ) for ``side = L``\ , or the equivalent equations a right-handed ``side = R`` ``X * A`` after computing ``X`` using ``trtrs!``\ . If ``uplo = U``\ , ``A`` is upper triangular. If ``uplo = L``\ , ``A`` is lower triangular. If ``diag = N``\ , ``A`` has non-unit diagonal elements. If ``diag = U``\ , all diagonal elements of ``A`` are one. ``Ferr`` and ``Berr`` are optional inputs. ``Ferr`` is the forward error and ``Berr`` is the backward error, each component-wise.

.. function:: stev!(job, dv, ev) -> (dv, Zmat)

   .. Docstring generated from Julia source

   Computes the eigensystem for a symmetric tridiagonal matrix with ``dv`` as diagonal and ``ev`` as off-diagonal. If ``job = N`` only the eigenvalues are found and returned in ``dv``\ . If ``job = V`` then the eigenvectors are also found and returned in ``Zmat``\ .

.. function:: stebz!(range, order, vl, vu, il, iu, abstol, dv, ev) -> (dv, iblock, isplit)

   .. Docstring generated from Julia source

   Computes the eigenvalues for a symmetric tridiagonal matrix with ``dv`` as diagonal and ``ev`` as off-diagonal. If ``range = A``\ , all the eigenvalues are found. If ``range = V``\ , the eigenvalues in the half-open interval ``(vl, vu]`` are found. If ``range = I``\ , the eigenvalues with indices between ``il`` and ``iu`` are found. If ``order = B``\ , eigvalues are ordered within a block. If ``order = E``\ , they are ordered across all the blocks. ``abstol`` can be set as a tolerance for convergence.

.. function:: stegr!(jobz, range, dv, ev, vl, vu, il, iu) -> (w, Z)

   .. Docstring generated from Julia source

   Computes the eigenvalues (``jobz = N``\ ) or eigenvalues and eigenvectors (``jobz = V``\ ) for a symmetric tridiagonal matrix with ``dv`` as diagonal and ``ev`` as off-diagonal. If ``range = A``\ , all the eigenvalues are found. If ``range = V``\ , the eigenvalues in the half-open interval ``(vl, vu]`` are found. If ``range = I``\ , the eigenvalues with indices between ``il`` and ``iu`` are found. The eigenvalues are returned in ``w`` and the eigenvectors in ``Z``\ .

.. function:: stein!(dv, ev_in, w_in, iblock_in, isplit_in)

   .. Docstring generated from Julia source

   Computes the eigenvectors for a symmetric tridiagonal matrix with ``dv`` as diagonal and ``ev_in`` as off-diagonal. ``w_in`` specifies the input eigenvalues for which to find corresponding eigenvectors. ``iblock_in`` specifies the submatrices corresponding to the eigenvalues in ``w_in``\ . ``isplit_in`` specifies the splitting points between the submatrix blocks.

.. function:: syconv!(uplo, A, ipiv) -> (A, work)

   .. Docstring generated from Julia source

   Converts a symmetric matrix ``A`` (which has been factorized into a triangular matrix) into two matrices ``L`` and ``D``\ . If ``uplo = U``\ , ``A`` is upper triangular. If ``uplo = L``\ , it is lower triangular. ``ipiv`` is the pivot vector from the triangular factorization. ``A`` is overwritten by ``L`` and ``D``\ .

.. function:: sysv!(uplo, A, B) -> (B, A, ipiv)

   .. Docstring generated from Julia source

   Finds the solution to ``A * X = B`` for symmetric matrix ``A``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``B`` is overwritten by the solution ``X``\ . ``A`` is overwritten by its Bunch-Kaufman factorization. ``ipiv`` contains pivoting information about the factorization.

.. function:: sytrf!(uplo, A) -> (A, ipiv)

   .. Docstring generated from Julia source

   Computes the Bunch-Kaufman factorization of a symmetric matrix ``A``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored.

   Returns ``A``\ , overwritten by the factorization, and a pivot vector ``ipiv``\ .

.. function:: sytri!(uplo, A, ipiv)

   .. Docstring generated from Julia source

   Computes the inverse of a symmetric matrix ``A`` using the results of ``sytrf!``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``A`` is overwritten by its inverse.

.. function:: sytrs!(uplo, A, ipiv, B)

   .. Docstring generated from Julia source

   Solves the equation ``A * X = B`` for a symmetric matrix ``A`` using the results of ``sytrf!``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``B`` is overwritten by the solution ``X``\ .

.. function:: hesv!(uplo, A, B) -> (B, A, ipiv)

   .. Docstring generated from Julia source

   Finds the solution to ``A * X = B`` for Hermitian matrix ``A``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``B`` is overwritten by the solution ``X``\ . ``A`` is overwritten by its Bunch-Kaufman factorization. ``ipiv`` contains pivoting information about the factorization.

.. function:: hetrf!(uplo, A) -> (A, ipiv)

   .. Docstring generated from Julia source

   Computes the Bunch-Kaufman factorization of a Hermitian matrix ``A``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored.

   Returns ``A``\ , overwritten by the factorization, and a pivot vector.

.. function:: hetri!(uplo, A, ipiv)

   .. Docstring generated from Julia source

   Computes the inverse of a Hermitian matrix ``A`` using the results of ``sytrf!``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``A`` is overwritten by its inverse.

.. function:: hetrs!(uplo, A, ipiv, B)

   .. Docstring generated from Julia source

   Solves the equation ``A * X = B`` for a Hermitian matrix ``A`` using the results of ``sytrf!``\ . If ``uplo = U``\ , the upper half of ``A`` is stored. If ``uplo = L``\ , the lower half is stored. ``B`` is overwritten by the solution ``X``\ .

.. function:: syev!(jobz, uplo, A)

   .. Docstring generated from Julia source

   Finds the eigenvalues (``jobz = N``\ ) or eigenvalues and eigenvectors (``jobz = V``\ ) of a symmetric matrix ``A``\ . If ``uplo = U``\ , the upper triangle of ``A`` is used. If ``uplo = L``\ , the lower triangle of ``A`` is used.

.. function:: syevr!(jobz, range, uplo, A, vl, vu, il, iu, abstol) -> (W, Z)

   .. Docstring generated from Julia source

   Finds the eigenvalues (``jobz = N``\ ) or eigenvalues and eigenvectors (``jobz = V``\ ) of a symmetric matrix ``A``\ . If ``uplo = U``\ , the upper triangle of ``A`` is used. If ``uplo = L``\ , the lower triangle of ``A`` is used. If ``range = A``\ , all the eigenvalues are found. If ``range = V``\ , the eigenvalues in the half-open interval ``(vl, vu]`` are found. If ``range = I``\ , the eigenvalues with indices between ``il`` and ``iu`` are found. ``abstol`` can be set as a tolerance for convergence.

   The eigenvalues are returned in ``W`` and the eigenvectors in ``Z``\ .

.. function:: sygvd!(jobz, range, uplo, A, vl, vu, il, iu, abstol) -> (w, A, B)

   .. Docstring generated from Julia source

   Finds the generalized eigenvalues (``jobz = N``\ ) or eigenvalues and eigenvectors (``jobz = V``\ ) of a symmetric matrix ``A`` and symmetric positive-definite matrix ``B``\ . If ``uplo = U``\ , the upper triangles of ``A`` and ``B`` are used. If ``uplo = L``\ , the lower triangles of ``A`` and ``B`` are used. If ``itype = 1``\ , the problem to solve is ``A * x = lambda * B * x``\ . If ``itype = 2``\ , the problem to solve is ``A * B * x = lambda * x``\ . If ``itype = 3``\ , the problem to solve is ``B * A * x = lambda * x``\ .

.. function:: bdsqr!(uplo, d, e_, Vt, U, C) -> (d, Vt, U, C)

   .. Docstring generated from Julia source

   Computes the singular value decomposition of a bidiagonal matrix with ``d`` on the diagonal and ``e_`` on the off-diagonal. If ``uplo = U``\ , ``e_`` is the superdiagonal. If ``uplo = L``\ , ``e_`` is the subdiagonal. Can optionally also compute the product ``Q' * C``\ .

   Returns the singular values in ``d``\ , and the matrix ``C`` overwritten with ``Q' * C``\ .

.. function:: bdsdc!(uplo, compq, d, e_) -> (d, e, u, vt, q, iq)

   .. Docstring generated from Julia source

   Computes the singular value decomposition of a bidiagonal matrix with ``d`` on the diagonal and ``e_`` on the off-diagonal using a divide and conqueq method. If ``uplo = U``\ , ``e_`` is the superdiagonal. If ``uplo = L``\ , ``e_`` is the subdiagonal. If ``compq = N``\ , only the singular values are found. If ``compq = I``\ , the singular values and vectors are found. If ``compq = P``\ , the singular values and vectors are found in compact form. Only works for real types.

   Returns the singular values in ``d``\ , and if ``compq = P``\ , the compact singular vectors in ``iq``\ .

.. function:: gecon!(normtype, A, anorm)

   .. Docstring generated from Julia source

   Finds the reciprocal condition number of matrix ``A``\ . If ``normtype = I``\ , the condition number is found in the infinity norm. If ``normtype = O`` or ``1``\ , the condition number is found in the one norm. ``A`` must be the result of ``getrf!`` and ``anorm`` is the norm of ``A`` in the relevant norm.

.. function:: gehrd!(ilo, ihi, A) -> (A, tau)

   .. Docstring generated from Julia source

   Converts a matrix ``A`` to Hessenberg form. If ``A`` is balanced with ``gebal!`` then ``ilo`` and ``ihi`` are the outputs of ``gebal!``\ . Otherwise they should be ``ilo = 1`` and ``ihi = size(A,2)``\ . ``tau`` contains the elementary reflectors of the factorization.

.. function:: orghr!(ilo, ihi, A, tau)

   .. Docstring generated from Julia source

   Explicitly finds ``Q``\ , the orthogonal/unitary matrix from ``gehrd!``\ . ``ilo``\ , ``ihi``\ , ``A``\ , and ``tau`` must correspond to the input/output to ``gehrd!``\ .

.. function:: gees!(jobvs, A) -> (A, vs, w)

   .. Docstring generated from Julia source

   Computes the eigenvalues (``jobvs = N``\ ) or the eigenvalues and Schur vectors (``jobvs = V``\ ) of matrix ``A``\ . ``A`` is overwritten by its Schur form.

   Returns ``A``\ , ``vs`` containing the Schur vectors, and ``w``\ , containing the eigenvalues.

.. function:: gges!(jobvsl, jobvsr, A, B) -> (A, B, alpha, beta, vsl, vsr)

   .. Docstring generated from Julia source

   Computes the generalized eigenvalues, generalized Schur form, left Schur vectors (``jobsvl = V``\ ), or right Schur vectors (``jobvsr = V``\ ) of ``A`` and ``B``\ .

   The generalized eigenvalues are returned in ``alpha`` and ``beta``\ . The left Schur vectors are returned in ``vsl`` and the right Schur vectors are returned in ``vsr``\ .

.. function:: trexc!(compq, ifst, ilst, T, Q) -> (T, Q)

   .. Docstring generated from Julia source

   Reorder the Schur factorization of a matrix. If ``compq = V``\ , the Schur vectors ``Q`` are reordered. If ``compq = N`` they are not modified. ``ifst`` and ``ilst`` specify the reordering of the vectors.

.. function:: trsen!(compq, job, select, T, Q) -> (T, Q, w)

   .. Docstring generated from Julia source

   Reorder the Schur factorization of a matrix and optionally finds reciprocal condition numbers. If ``job = N``\ , no condition numbers are found. If ``job = E``\ , only the condition number for this cluster of eigenvalues is found. If ``job = V``\ , only the condition number for the invariant subspace is found. If ``job = B`` then the condition numbers for the cluster and subspace are found. If ``compq = V`` the Schur vectors ``Q`` are updated. If ``compq = N`` the Schur vectors are not modified. ``select`` determines which eigenvalues are in the cluster.

   Returns ``T``\ , ``Q``\ , and reordered eigenvalues in ``w``\ .

.. function:: tgsen!(select, S, T, Q, Z) -> (S, T, alpha, beta, Q, Z)

   .. Docstring generated from Julia source

   Reorders the vectors of a generalized Schur decomposition. ``select`` specifices the eigenvalues in each cluster.

.. function:: trsyl!(transa, transb, A, B, C, isgn=1) -> (C, scale)

   .. Docstring generated from Julia source

   Solves the Sylvester matrix equation ``A * X +/- X * B = scale*C`` where ``A`` and ``B`` are both quasi-upper triangular. If ``transa = N``\ , ``A`` is not modified. If ``transa = T``\ , ``A`` is transposed. If ``transa = C``\ , ``A`` is conjugate transposed. Similarly for ``transb`` and ``B``\ . If ``isgn = 1``\ , the equation ``A * X + X * B = scale * C`` is solved. If ``isgn = -1``\ , the equation ``A * X - X * B = scale * C`` is solved.

   Returns ``X`` (overwriting ``C``\ ) and ``scale``\ .

