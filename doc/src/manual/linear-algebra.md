# Linear algebra

In addition to (and as part of) its support for multi-dimensional arrays, Julia provides native implementations
of many common and useful linear algebra operations. Basic operations, such as [`trace`](@ref), [`det`](@ref),
and [`inv`](@ref) are all supported:

```jldoctest
julia> A = [1 2 3; 4 1 6; 7 8 1]
3×3 Array{Int64,2}:
 1  2  3
 4  1  6
 7  8  1

julia> trace(A)
3

julia> det(A)
104.0

julia> inv(A)
3×3 Array{Float64,2}:
 -0.451923   0.211538    0.0865385
  0.365385  -0.192308    0.0576923
  0.240385   0.0576923  -0.0673077
```

As well as other useful operations, such as finding eigenvalues or eigenvectors:

```jldoctest
julia> A = [1.5 2 -4; 3 -1 -6; -10 2.3 4]
3×3 Array{Float64,2}:
   1.5   2.0  -4.0
   3.0  -1.0  -6.0
 -10.0   2.3   4.0

julia> eigvals(A)
3-element Array{Complex{Float64},1}:
  9.31908+0.0im
 -2.40954+2.72095im
 -2.40954-2.72095im

julia> eigvecs(A)
3×3 Array{Complex{Float64},2}:
 -0.488645+0.0im  0.182546-0.39813im   0.182546+0.39813im
 -0.540358+0.0im  0.692926+0.0im       0.692926-0.0im
   0.68501+0.0im  0.254058-0.513301im  0.254058+0.513301im
```

In addition, Julia provides many [factorizations](@ref man-linalg-factorizations) which can be used to
speed up problems such as linear solve or matrix exponentiation by pre-factorizing a matrix into a form
more amenable (for performance or memory reasons) to the problem. See the documentation on [`factorize`](@ref)
for more information. As an example:

```jldoctest
julia> A = [1.5 2 -4; 3 -1 -6; -10 2.3 4]
3×3 Array{Float64,2}:
   1.5   2.0  -4.0
   3.0  -1.0  -6.0
 -10.0   2.3   4.0

julia> factorize(A)
Base.LinAlg.LU{Float64,Array{Float64,2}} with factors L and U:
[1.0 0.0 0.0; -0.15 1.0 0.0; -0.3 -0.132196 1.0]
[-10.0 2.3 4.0; 0.0 2.345 -3.4; 0.0 0.0 -5.24947]
```

Since `A` is not Hermitian, symmetric, triangular, tridiagonal, or bidiagonal, an LU factorization may be the
best we can do. Compare with:

```jldoctest
julia> B = [1.5 2 -4; 2 -1 -3; -4 -3 5]
3×3 Array{Float64,2}:
  1.5   2.0  -4.0
  2.0  -1.0  -3.0
 -4.0  -3.0   5.0

julia> factorize(B)
Base.LinAlg.BunchKaufman{Float64,Array{Float64,2}}([-1.64286 0.142857 -0.8; 2.0 -2.8 -0.6; -4.0 -3.0 5.0], [1, 2, 3], 'U', true, false, 0)
```

Here, Julia was able to detect that `B` is in fact symmetric, and used a more appropriate factorization.
Often it's possible to write more efficient code for a matrix that is known to have certain properties e.g.
it is symmetric, or tridiagonal. Julia provides some special types so that you can "tag" matrices as having
these properties. For instance:

```jldoctest
julia> B = [1.5 2 -4; 2 -1 -3; -4 -3 5]
3×3 Array{Float64,2}:
  1.5   2.0  -4.0
  2.0  -1.0  -3.0
 -4.0  -3.0   5.0

julia> sB = Symmetric(B)
3×3 Symmetric{Float64,Array{Float64,2}}:
  1.5   2.0  -4.0
  2.0  -1.0  -3.0
 -4.0  -3.0   5.0
```

`sB` has been tagged as a matrix that's (real) symmetric, so for later operations we might perform on it,
such as eigenfactorization or computing matrix-vector products, efficiencies can be found by only referencing
half of it. For example:

```jldoctest
julia> B = [1.5 2 -4; 2 -1 -3; -4 -3 5]
3×3 Array{Float64,2}:
  1.5   2.0  -4.0
  2.0  -1.0  -3.0
 -4.0  -3.0   5.0

julia> sB = Symmetric(B)
3×3 Symmetric{Float64,Array{Float64,2}}:
  1.5   2.0  -4.0
  2.0  -1.0  -3.0
 -4.0  -3.0   5.0

julia> x = [1; 2; 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> sB\x
3-element Array{Float64,1}:
 -1.73913
 -1.1087
 -1.45652
```
The `\` operation here performs the linear solution. Julia's parser provides convenient dispatch
to specialized methods for the *transpose* of a matrix left-divided by a vector, or for the various combinations
of transpose operations in matrix-matrix solutions. Many of these are further specialized for certain special
matrix types. For example, `A\B` will end up calling [`Base.LinAlg.A_ldiv_B!`](@ref) while `A'\B` will end up calling
[`Base.LinAlg.Ac_ldiv_B`](@ref), even though we used the same left-division operator. This works for matrices too: `A.'\B.'`
would call [`Base.LinAlg.At_ldiv_Bt`](@ref). The left-division operator is pretty powerful and it's easy to write compact,
readable code that is flexible enough to solve all sorts of systems of linear equations.

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

## [Matrix factorizations](@id man-linalg-factorizations)

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
| `LUTridiagonal`   | LU factorization for [`Tridiagonal`](@ref) matrices                                                            |
| `UmfpackLU`       | LU factorization for sparse matrices (computed by UMFPack)                                                     |
| `QR`              | [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                             |
| `QRCompactWY`     | Compact WY form of the QR factorization                                                                        |
| `QRPivoted`       | Pivoted [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                     |
| `Hessenberg`      | [Hessenberg decomposition](http://mathworld.wolfram.com/HessenbergDecomposition.html)                          |
| `Eigen`           | [Spectral decomposition](https://en.wikipedia.org/wiki/Eigendecomposition_(matrix))                            |
| `SVD`             | [Singular value decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)                     |
| `GeneralizedSVD`  | [Generalized SVD](https://en.wikipedia.org/wiki/Generalized_singular_value_decomposition#Higher_order_version) |

