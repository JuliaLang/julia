.. currentmodule:: Base

****************
 Linear algebra 
****************

Matrix factorizations
=====================

`Matrix factorizations (a.k.a. matrix decompositions) <http://en.wikipedia.org/wiki/Matrix_decomposition>`_
compute the factorization of a matrix into a product of matrices, and
are one of the central concepts in linear algebra.

The following table summarizes the types of matrix factorizations that have been
implemented in Julia. Details of their associated methods can be found
in the :ref:`stdlib-linalg` section of the standard library documentation.

======================== ======
:class:`Cholesky`        `Cholesky factorization <http://en.wikipedia.org/wiki/Cholesky_decomposition>`_
:class:`CholeskyPivoted` `Pivoted <http://en.wikipedia.org/wiki/Pivot_element>`_ Cholesky factorization
:class:`LU`              `LU factorization <http://en.wikipedia.org/wiki/LU_decomposition>`_
:class:`LUTridiagonal`   LU factorization for Tridiagonal matrices
:class:`UmfpackLU`       LU factorization for sparse matrices (computed by UMFPack)
:class:`QR`              `QR factorization <http://en.wikipedia.org/wiki/QR_decomposition>`_
:class:`QRCompactWY`     Compact WY form of the QR factorization
:class:`QRPivoted`       Pivoted `QR factorization <http://en.wikipedia.org/wiki/QR_decomposition>`_
:class:`Hessenberg`      `Hessenberg decomposition <http://mathworld.wolfram.com/HessenbergDecomposition.html>`_
:class:`Eigen`           `Spectral decomposition <http://en.wikipedia.org/wiki/Eigendecomposition_(matrix)>`_
:class:`SVD`             `Singular value decomposition <http://en.wikipedia.org/wiki/Singular_value_decomposition>`_
:class:`GeneralizedSVD`  `Generalized SVD <http://en.wikipedia.org/wiki/Generalized_singular_value_decomposition#Higher_order_version>`_
======================== ======

Special matrices 
================

`Matrices with special symmetries and structures <http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=3274>`_
arise often in linear algebra and are frequently associated with
various matrix factorizations.
Julia features a rich collection of special matrix types, which allow for fast
computation with specialized routines that are specially developed for
particular matrix types.

The following tables summarize the types of special matrices that have been
implemented in Julia, as well as whether hooks to various optimized methods
for them in LAPACK are available.

======================= ==================================================================================
:class:`Hermitian`      `Hermitian matrix <http://en.wikipedia.org/wiki/Hermitian_matrix>`_
:class:`Triangular`     Upper/lower `triangular matrix <http://en.wikipedia.org/wiki/Triangular_matrix>`_
:class:`Tridiagonal`    `Tridiagonal matrix <http://en.wikipedia.org/wiki/Tridiagonal_matrix>`_
:class:`SymTridiagonal` Symmetric tridiagonal matrix
:class:`Bidiagonal`     Upper/lower `bidiagonal matrix <http://en.wikipedia.org/wiki/Bidiagonal_matrix>`_
:class:`Diagonal`       `Diagonal matrix <http://en.wikipedia.org/wiki/Diagonal_matrix>`_
:class:`UniformScaling` `Uniform scaling operator <http://en.wikipedia.org/wiki/Uniform_scaling>`_
======================= ==================================================================================

Elementary operations
---------------------

+-------------------------+-------+-------+-------+-------+--------------------------------+
| Matrix type             | ``+`` | ``-`` | ``*`` | ``\`` | Other functions with           |
|                         |       |       |       |       | optimized methods              |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`Hermitian`      |       |       |       |   MV  | :func:`inv`,                   |
|                         |       |       |       |       | :func:`sqrtm`, :func:`expm`    |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`Triangular`     |       |       |  MV   |   MV  | :func:`inv`, :func:`det`       |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`SymTridiagonal` |   M   |   M   |  MS   |   MV  | :func:`eigmax`, :func:`eigmin` |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`Tridiagonal`    |   M   |   M   |  MS   |   MV  |                                |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`Bidiagonal`     |   M   |   M   |  MS   |   MV  |                                |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`Diagonal`       |   M   |   M   |  MV   |   MV  | :func:`inv`, :func:`det`,      |
|                         |       |       |       |       | :func:`logdet`, :func:`/`      |
+-------------------------+-------+-------+-------+-------+--------------------------------+
| :class:`UniformScaling` |   M   |   M   |  MVS  |  MVS  | :func:`/`                      |
+-------------------------+-------+-------+-------+-------+--------------------------------+

Legend:

+------------+---------------------------------------------------------------+
| M (matrix) | An optimized method for matrix-matrix operations is available |
+------------+---------------------------------------------------------------+
| V (vector) | An optimized method for matrix-vector operations is available |
+------------+---------------------------------------------------------------+
| S (scalar) | An optimized method for matrix-scalar operations is available |
+------------+---------------------------------------------------------------+

Matrix factorizations
---------------------

+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| Matrix type             | LAPACK | :func:`eig` | :func:`eigvals` | :func:`eigvecs` | :func:`svd` | :func:`svdvals` |
+=========================+========+=============+=================+=================+=============+=================+
| :class:`Hermitian`      |   HE   |             |       ARI       |                 |             |                 |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| :class:`Triangular`     |   TR   |             |                 |                 |             |                 |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| :class:`SymTridiagonal` |   ST   |      A      |       ARI       |       AV        |             |                 |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| :class:`Tridiagonal`    |   GT   |             |                 |                 |             |                 |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| :class:`Bidiagonal`     |   BD   |             |                 |                 |      A      |         A       |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+
| :class:`Diagonal`       |   DI   |             |        A        |                 |             |                 |
+-------------------------+--------+-------------+-----------------+-----------------+-------------+-----------------+

Legend:

+--------------+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| A (all)      | An optimized method to find all the characteristic values and/or vectors is available                                             | e.g. ``eigvals(M)``    |
+--------------+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| R (range)    | An optimized method to find the ``il``:sup:`th` through the ``ih``:sup:`th` characteristic values are available                   | ``eigvals(M, il, ih)`` |
+--------------+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| I (interval) | An optimized method to find the characteristic values in the interval [``vl``, ``vh``] is available                               | ``eigvals(M, vl, vh)`` |
+--------------+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| V (vectors)  | An optimized method to find the characteristic vectors corresponding to the characteristic values ``x=[x1, x2,...]`` is available | ``eigvecs(M, x)``      |
+--------------+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+

The uniform scaling operator
----------------------------
A :class:`UniformScaling` operator represents a scalar times the identity operator, ``λ*I``. The identity operator  :class:`I` is defined as a constant and is an instance of :class:`UniformScaling`. The size of these operators are generic and match the other matrix in the binary operations :obj:`+`, :obj:`-`, :obj:`*` and :obj:`\\`. For ``A+I`` and ``A-I`` this means that ``A`` must be square. Multiplication with the identity operator :class: `I` is a noop (except for checking that the scaling factor is one) and therefore almost without overhead.

