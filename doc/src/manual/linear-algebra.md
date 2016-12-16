# Linear algebra

## Matrix factorizations

[Matrix factorizations (a.k.a. matrix decompositions)](https://en.wikipedia.org/wiki/Matrix_decomposition)
compute the factorization of a matrix into a product of matrices, and are one of the central concepts
in linear algebra.

The following table summarizes the types of matrix factorizations that have been implemented in
Julia. Details of their associated methods can be found in the [Linear Algebra](@ref) section
of the standard library documentation.

| Type              | Description                                                                                                    |
|:----------------- |:-------------------------------------------------------------------------------------------------------------- |
| `Cholesky`        | [Cholesky factorization](https://en.wikipedia.org/wiki/Cholesky_decomposition)                                 |
| `CholeskyPivoted` | [Pivoted](https://en.wikipedia.org/wiki/Pivot_element) Cholesky factorization                                  |
| `LU`              | [LU factorization](https://en.wikipedia.org/wiki/LU_decomposition)                                             |
| `LUTridiagonal`   | LU factorization for Tridiagonal matrices                                                                      |
| `UmfpackLU`       | LU factorization for sparse matrices (computed by UMFPack)                                                     |
| `QR`              | [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                             |
| `QRCompactWY`     | Compact WY form of the QR factorization                                                                        |
| `QRPivoted`       | Pivoted [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                     |
| `Hessenberg`      | [Hessenberg decomposition](http://mathworld.wolfram.com/HessenbergDecomposition.html)                          |
| `Eigen`           | [Spectral decomposition](https://en.wikipedia.org/wiki/Eigendecomposition_(matrix))                            |
| `SVD`             | [Singular value decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)                     |
| `GeneralizedSVD`  | [Generalized SVD](https://en.wikipedia.org/wiki/Generalized_singular_value_decomposition#Higher_order_version) |

## Special matrices

[Matrices with special symmetries and structures](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=3274)
arise often in linear algebra and are frequently associated with various matrix factorizations.
Julia features a rich collection of special matrix types, which allow for fast computation with
specialized routines that are specially developed for particular matrix types.

The following tables summarize the types of special matrices that have been implemented in Julia,
as well as whether hooks to various optimized methods for them in LAPACK are available.

| Type                     | Description                                                                      |
|:------------------------ |:-------------------------------------------------------------------------------- |
| [`Hermitian`](@ref)      | [Hermitian matrix](https://en.wikipedia.org/wiki/Hermitian_matrix)               |
| `UpperTriangular`        | Upper [triangular matrix](https://en.wikipedia.org/wiki/Triangular_matrix)       |
| `LowerTriangular`        | Lower [triangular matrix](https://en.wikipedia.org/wiki/Triangular_matrix)       |
| [`Tridiagonal`](@ref)    | [Tridiagonal matrix](https://en.wikipedia.org/wiki/Tridiagonal_matrix)           |
| [`SymTridiagonal`](@ref) | Symmetric tridiagonal matrix                                                     |
| [`Bidiagonal`](@ref)     | Upper/lower [bidiagonal matrix](https://en.wikipedia.org/wiki/Bidiagonal_matrix) |
| [`Diagonal`](@ref)       | [Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)                 |
| `UniformScaling`         | [Uniform scaling operator](https://en.wikipedia.org/wiki/Uniform_scaling)        |

### Elementary operations

| Matrix type              | `+` | `-` | `*` | `\` | Other functions with optimized methods                              |
|:------------------------ |:--- |:--- |:--- |:--- |:------------------------------------------------------------------- |
| [`Hermitian`](@ref)      |     |     |     | MV  | [`inv()`](@ref), [`sqrtm()`](@ref), [`expm()`](@ref)                |
| `UpperTriangular`        |     |     | MV  | MV  | [`inv()`](@ref), [`det()`](@ref)                                    |
| `LowerTriangular`        |     |     | MV  | MV  | [`inv()`](@ref), [`det()`](@ref)                                    |
| [`SymTridiagonal`](@ref) | M   | M   | MS  | MV  | [`eigmax()`](@ref), [`eigmin()`](@ref)                              |
| [`Tridiagonal`](@ref)    | M   | M   | MS  | MV  |                                                                     |
| [`Bidiagonal`](@ref)     | M   | M   | MS  | MV  |                                                                     |
| [`Diagonal`](@ref)       | M   | M   | MV  | MV  | [`inv()`](@ref), [`det()`](@ref), [`logdet()`](@ref), [`/()`](@ref) |
| `UniformScaling`         | M   | M   | MVS | MVS | [`/()`](@ref)                                                       |

Legend:

| Key        | Description                                                   |
|:---------- |:------------------------------------------------------------- |
| M (matrix) | An optimized method for matrix-matrix operations is available |
| V (vector) | An optimized method for matrix-vector operations is available |
| S (scalar) | An optimized method for matrix-scalar operations is available |

### Matrix factorizations

| Matrix type              | LAPACK | [`eig()`](@ref) | [`eigvals()`](@ref) | [`eigvecs()`](@ref) | [`svd()`](@ref) | [`svdvals()`](@ref) |
|:------------------------ |:------ |:--------------- |:------------------- |:------------------- |:--------------- |:------------------- |
| [`Hermitian`](@ref)      | HE     |                 | ARI                 |                     |                 |                     |
| `UpperTriangular`        | TR     | A               | A                   | A                   |                 |                     |
| `LowerTriangular`        | TR     | A               | A                   | A                   |                 |                     |
| [`SymTridiagonal`](@ref) | ST     | A               | ARI                 | AV                  |                 |                     |
| [`Tridiagonal`](@ref)    | GT     |                 |                     |                     |                 |                     |
| [`Bidiagonal`](@ref)     | BD     |                 |                     |                     | A               | A                   |
| [`Diagonal`](@ref)       | DI     |                 | A                   |                     |                 |                     |

Legend:

| Key          | Description                                                                                                                     | Example              |
|:------------ |:------------------------------------------------------------------------------------------------------------------------------- |:-------------------- |
| A (all)      | An optimized method to find all the characteristic values and/or vectors is available                                           | e.g. `eigvals(M)`    |
| R (range)    | An optimized method to find the `il`th through the `ih`th characteristic values are available                                   | `eigvals(M, il, ih)` |
| I (interval) | An optimized method to find the characteristic values in the interval [`vl`, `vh`] is available                                 | `eigvals(M, vl, vh)` |
| V (vectors)  | An optimized method to find the characteristic vectors corresponding to the characteristic values `x=[x1, x2,...]` is available | `eigvecs(M, x)`      |

### The uniform scaling operator

A `UniformScaling` operator represents a scalar times the identity operator, `λ*I`. The identity
operator  `I` is defined as a constant and is an instance of `UniformScaling`. The size of these
operators are generic and match the other matrix in the binary operations [`+`](@ref), [`-`](@ref),
[`*`](@ref) and [`\`](@ref). For `A+I` and `A-I` this means that `A` must be square. Multiplication
with the identity operator `I` is a noop (except for checking that the scaling factor is one)
and therefore almost without overhead.
