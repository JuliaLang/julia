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

=================== ===========
``Cholesky``        `Cholesky factorization <http://en.wikipedia.org/wiki/Cholesky_decomposition>`_
``CholeskyPivoted`` `Pivoted <http://en.wikipedia.org/wiki/Pivot_element>`_ Cholesky factorization
``LU``              `LU factorization <http://en.wikipedia.org/wiki/LU_decomposition>`_
``QRPivoted``       Pivoted `QR factorization <http://en.wikipedia.org/wiki/QR_decomposition>`_
``Hessenberg``      `Hessenberg decomposition <http://mathworld.wolfram.com/HessenbergDecomposition.html>`_
``Eigen``           `Spectral decomposition <http://en.wikipedia.org/wiki/Eigendecomposition_(matrix)>`_
``SVD``             `Singular value decomposition <http://en.wikipedia.org/wiki/Singular_value_decomposition>`_
``GeneralizedSVD``  `Generalized SVD <http://en.wikipedia.org/wiki/Generalized_singular_value_decomposition#Higher_order_version>`_
=================== ===========

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

+--------------------+-----------------------------------------------------------------------------------+
| ``Hermitian``      | `Hermitian matrix <http://en.wikipedia.org/wiki/Hermitian_matrix>`_               |
+--------------------+-----------------------------------------------------------------------------------+
| ``Triangular``     | Upper/lower `triangular matrix <http://en.wikipedia.org/wiki/Triangular_matrix>`_ |
+--------------------+-----------------------------------------------------------------------------------+
| ``Tridiagonal``    | `Tridiagonal matrix <http://en.wikipedia.org/wiki/Tridiagonal_matrix>`_           | 
+--------------------+-----------------------------------------------------------------------------------+
| ``SymTridiagonal`` | Symmetric tridiagonal matrix                                                      |
+--------------------+-----------------------------------------------------------------------------------+
| ``Bidiagonal``     | Upper/lower `bidiagonal matrix <http://en.wikipedia.org/wiki/Bidiagonal_matrix>`_ | 
+--------------------+-----------------------------------------------------------------------------------+
| ``Diagonal``       | `Diagonal matrix <http://en.wikipedia.org/wiki/Diagonal_matrix>`_                 |
+--------------------+-----------------------------------------------------------------------------------+


Elementary operations
---------------------

+--------------------+-------+-------+-------+-------+---------------------+
| Matrix type        | ``+`` | ``-`` | ``*`` | ``\`` | Other functions with|
|                    |       |       |       |       | optimized methods   |
+--------------------+-------+-------+-------+-------+---------------------+
| ``Hermitian``      |       |       |       |   XY  | ``inv``,            |
|                    |       |       |       |       | ``sqrtm``, ``expm`` |
+--------------------+-------+-------+-------+-------+---------------------+
| ``Triangular``     |       |       |  XY   |   XY  | ``inv``, ``det``    |
+--------------------+-------+-------+-------+-------+---------------------+
| ``SymTridiagonal`` |   X   |   X   |  XZ   |   XY  | ``eigmax/min``      |
+--------------------+-------+-------+-------+-------+---------------------+
| ``Tridiagonal``    |   X   |   X   |  XZ   |   XY  |                     |
+--------------------+-------+-------+-------+-------+---------------------+
| ``Bidiagonal``     |   X   |   X   |  XZ   |   XY  |                     |
+--------------------+-------+-------+-------+-------+---------------------+
| ``Diagonal``       |   X   |   X   |  XY   |   XY  | ``inv``, ``det``,   |
|                    |       |       |       |       | ``logdet``, ``/``   |
+--------------------+-------+-------+-------+-------+---------------------+

Legend:

+---+---------------------------------------------------------------+
| X | An optimized method for matrix-matrix operations is available |
+---+---------------------------------------------------------------+
| Y | An optimized method for matrix-vector operations is available |
+---+---------------------------------------------------------------+
| Z | An optimized method for matrix-scalar operations is available |
+---+---------------------------------------------------------------+

Matrix factorizations
---------------------

+--------------------+--------+-------------------------------------+-----------------------------+
| Matrix type        | LAPACK | Eigensystems                        | Singular values and vectors |
|                    |  Name  +---------+-------------+-------------+---------+-------------------+
|                    |        | ``eig`` | ``eigvals`` | ``eigvecs`` | ``svd`` | ``svdvals``       |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``Hermitian``      |   HE   |         |     ABC     |             |         |                   |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``Triangular``     |   TR   |         |             |             |         |                   |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``SymTridiagonal`` |   ST   |    A    |     ABC     |     AD      |         |                   |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``Tridiagonal``    |   GT   |         |             |             |         |                   |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``Bidiagonal``     |   BD   |         |             |             |    A    |         A         |
+--------------------+--------+-------------------------------------+-----------------------------+
| ``Diagonal``       |   DI   |         |      A      |             |         |                   |
+--------------------+--------+-------------------------------------+-----------------------------+

Legend:

+---+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| A | An optimized method to find all the characteristic values and/or vectors is available                                             | e.g. ``eigvals(M)``    |
+---+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| B | An optimized method to find the ``il``:sup:`th` through the ``ih``:sup:`th` characteristic values are available                   | ``eigvals(M, il, ih)`` |
+---+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| C | An optimized method to find the characteristic values in the interval [``vl``, ``vh``] is available                               | ``eigvals(M, vl, vh)`` |
+---+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+
| D | An optimized method to find the characteristic vectors corresponding to the characteristic values ``x=[x1, x2,...]`` is available | ``eigvecs(M, x)``      |
+---+-----------------------------------------------------------------------------------------------------------------------------------+------------------------+

