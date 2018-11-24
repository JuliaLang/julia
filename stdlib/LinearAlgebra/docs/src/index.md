# Linear Algebra

```@meta
DocTestSetup = :(using LinearAlgebra)
```

In addition to (and as part of) its support for multi-dimensional arrays, Julia provides native implementations
of many common and useful linear algebra operations which can be loaded with `using LinearAlgebra`. Basic operations, such as [`tr`](@ref), [`det`](@ref),
and [`inv`](@ref) are all supported:

```jldoctest
julia> A = [1 2 3; 4 1 6; 7 8 1]
3×3 Array{Int64,2}:
 1  2  3
 4  1  6
 7  8  1

julia> tr(A)
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
julia> A = [-4. -17.; 2. 2.]
2×2 Array{Float64,2}:
 -4.0  -17.0
  2.0    2.0

julia> eigvals(A)
2-element Array{Complex{Float64},1}:
 -1.0 + 5.0im
 -1.0 - 5.0im

julia> eigvecs(A)
2×2 Array{Complex{Float64},2}:
  0.945905+0.0im        0.945905-0.0im
 -0.166924-0.278207im  -0.166924+0.278207im
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
LU{Float64,Array{Float64,2}}
L factor:
3×3 Array{Float64,2}:
  1.0    0.0       0.0
 -0.15   1.0       0.0
 -0.3   -0.132196  1.0
U factor:
3×3 Array{Float64,2}:
 -10.0  2.3     4.0
   0.0  2.345  -3.4
   0.0  0.0    -5.24947
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
BunchKaufman{Float64,Array{Float64,2}}
D factor:
3×3 Tridiagonal{Float64,Array{Float64,1}}:
 -1.64286   0.0   ⋅
  0.0      -2.8  0.0
   ⋅        0.0  5.0
U factor:
3×3 UnitUpperTriangular{Float64,Array{Float64,2}}:
 1.0  0.142857  -0.8
  ⋅   1.0       -0.6
  ⋅    ⋅         1.0
permutation:
3-element Array{Int64,1}:
 1
 2
 3
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
 -1.7391304347826084
 -1.1086956521739126
 -1.4565217391304346
```
The `\` operation here performs the linear solution. The left-division operator is pretty powerful and it's easy to write compact, readable code that is flexible enough to solve all sorts of systems of linear equations.

## Special matrices

[Matrices with special symmetries and structures](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=3274)
arise often in linear algebra and are frequently associated with various matrix factorizations.
Julia features a rich collection of special matrix types, which allow for fast computation with
specialized routines that are specially developed for particular matrix types.

The following tables summarize the types of special matrices that have been implemented in Julia,
as well as whether hooks to various optimized methods for them in LAPACK are available.

| Type                      | Description                                                                      |
|:------------------------- |:-------------------------------------------------------------------------------- |
| [`Symmetric`](@ref)       | [Symmetric matrix](https://en.wikipedia.org/wiki/Symmetric_matrix)               |
| [`Hermitian`](@ref)       | [Hermitian matrix](https://en.wikipedia.org/wiki/Hermitian_matrix)               |
| [`UpperTriangular`](@ref) | Upper [triangular matrix](https://en.wikipedia.org/wiki/Triangular_matrix)       |
| [`LowerTriangular`](@ref) | Lower [triangular matrix](https://en.wikipedia.org/wiki/Triangular_matrix)       |
| [`Tridiagonal`](@ref)     | [Tridiagonal matrix](https://en.wikipedia.org/wiki/Tridiagonal_matrix)           |
| [`SymTridiagonal`](@ref)  | Symmetric tridiagonal matrix                                                     |
| [`Bidiagonal`](@ref)      | Upper/lower [bidiagonal matrix](https://en.wikipedia.org/wiki/Bidiagonal_matrix) |
| [`Diagonal`](@ref)        | [Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)                 |
| [`UniformScaling`](@ref)  | [Uniform scaling operator](https://en.wikipedia.org/wiki/Uniform_scaling)        |

### Elementary operations

| Matrix type               | `+` | `-` | `*` | `\` | Other functions with optimized methods                      |
|:------------------------- |:--- |:--- |:--- |:--- |:----------------------------------------------------------- |
| [`Symmetric`](@ref)       |     |     |     | MV  | [`inv`](@ref), [`sqrt`](@ref), [`exp`](@ref)                |
| [`Hermitian`](@ref)       |     |     |     | MV  | [`inv`](@ref), [`sqrt`](@ref), [`exp`](@ref)                |
| [`UpperTriangular`](@ref) |     |     | MV  | MV  | [`inv`](@ref), [`det`](@ref)                                |
| [`LowerTriangular`](@ref) |     |     | MV  | MV  | [`inv`](@ref), [`det`](@ref)                                |
| [`SymTridiagonal`](@ref)  | M   | M   | MS  | MV  | [`eigmax`](@ref), [`eigmin`](@ref)                          |
| [`Tridiagonal`](@ref)     | M   | M   | MS  | MV  |                                                             |
| [`Bidiagonal`](@ref)      | M   | M   | MS  | MV  |                                                             |
| [`Diagonal`](@ref)        | M   | M   | MV  | MV  | [`inv`](@ref), [`det`](@ref), [`logdet`](@ref), [`/`](@ref) |
| [`UniformScaling`](@ref)  | M   | M   | MVS | MVS | [`/`](@ref)                                                 |

Legend:

| Key        | Description                                                   |
|:---------- |:------------------------------------------------------------- |
| M (matrix) | An optimized method for matrix-matrix operations is available |
| V (vector) | An optimized method for matrix-vector operations is available |
| S (scalar) | An optimized method for matrix-scalar operations is available |

### Matrix factorizations

| Matrix type               | LAPACK | [`eigen`](@ref) | [`eigvals`](@ref) | [`eigvecs`](@ref) | [`svd`](@ref) | [`svdvals`](@ref) |
|:------------------------- |:------ |:------------- |:----------------- |:----------------- |:------------- |:----------------- |
| [`Symmetric`](@ref)       | SY     |               | ARI               |                   |               |                   |
| [`Hermitian`](@ref)       | HE     |               | ARI               |                   |               |                   |
| [`UpperTriangular`](@ref) | TR     | A             | A                 | A                 |               |                   |
| [`LowerTriangular`](@ref) | TR     | A             | A                 | A                 |               |                   |
| [`SymTridiagonal`](@ref)  | ST     | A             | ARI               | AV                |               |                   |
| [`Tridiagonal`](@ref)     | GT     |               |                   |                   |               |                   |
| [`Bidiagonal`](@ref)      | BD     |               |                   |                   | A             | A                 |
| [`Diagonal`](@ref)        | DI     |               | A                 |                   |               |                   |

Legend:

| Key          | Description                                                                                                                     | Example              |
|:------------ |:------------------------------------------------------------------------------------------------------------------------------- |:-------------------- |
| A (all)      | An optimized method to find all the characteristic values and/or vectors is available                                           | e.g. `eigvals(M)`    |
| R (range)    | An optimized method to find the `il`th through the `ih`th characteristic values are available                                   | `eigvals(M, il, ih)` |
| I (interval) | An optimized method to find the characteristic values in the interval [`vl`, `vh`] is available                                 | `eigvals(M, vl, vh)` |
| V (vectors)  | An optimized method to find the characteristic vectors corresponding to the characteristic values `x=[x1, x2,...]` is available | `eigvecs(M, x)`      |

### The uniform scaling operator

A [`UniformScaling`](@ref) operator represents a scalar times the identity operator, `λ*I`. The identity
operator `I` is defined as a constant and is an instance of `UniformScaling`. The size of these
operators are generic and match the other matrix in the binary operations [`+`](@ref), [`-`](@ref),
[`*`](@ref) and [`\`](@ref). For `A+I` and `A-I` this means that `A` must be square. Multiplication
with the identity operator `I` is a noop (except for checking that the scaling factor is one)
and therefore almost without overhead.

To see the `UniformScaling` operator in action:

```jldoctest
julia> U = UniformScaling(2);

julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> a + U
2×2 Array{Int64,2}:
 3  2
 3  6

julia> a * U
2×2 Array{Int64,2}:
 2  4
 6  8

julia> [a U]
2×4 Array{Int64,2}:
 1  2  2  0
 3  4  0  2

julia> b = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> b - U
ERROR: DimensionMismatch("matrix is not square: dimensions are (2, 3)")
Stacktrace:
[...]
```

## [Matrix factorizations](@id man-linalg-factorizations)

[Matrix factorizations (a.k.a. matrix decompositions)](https://en.wikipedia.org/wiki/Matrix_decomposition)
compute the factorization of a matrix into a product of matrices, and are one of the central concepts
in linear algebra.

The following table summarizes the types of matrix factorizations that have been implemented in
Julia. Details of their associated methods can be found in the [Standard Functions](@ref) section
of the Linear Algebra documentation.

| Type              | Description                                                                                                    |
|:----------------- |:-------------------------------------------------------------------------------------------------------------- |
| `Cholesky`        | [Cholesky factorization](https://en.wikipedia.org/wiki/Cholesky_decomposition)                                 |
| `CholeskyPivoted` | [Pivoted](https://en.wikipedia.org/wiki/Pivot_element) Cholesky factorization                                  |
| `LU`              | [LU factorization](https://en.wikipedia.org/wiki/LU_decomposition)                                             |
| `LUTridiagonal`   | LU factorization for [`Tridiagonal`](@ref) matrices                                                            |
| `QR`              | [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                             |
| `QRCompactWY`     | Compact WY form of the QR factorization                                                                        |
| `QRPivoted`       | Pivoted [QR factorization](https://en.wikipedia.org/wiki/QR_decomposition)                                     |
| `Hessenberg`      | [Hessenberg decomposition](http://mathworld.wolfram.com/HessenbergDecomposition.html)                          |
| `Eigen`           | [Spectral decomposition](https://en.wikipedia.org/wiki/Eigendecomposition_(matrix))                            |
| `SVD`             | [Singular value decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)                     |
| `GeneralizedSVD`  | [Generalized SVD](https://en.wikipedia.org/wiki/Generalized_singular_value_decomposition#Higher_order_version) |




## Standard Functions

Linear algebra functions in Julia are largely implemented by calling functions from [LAPACK](http://www.netlib.org/lapack/).
 Sparse factorizations call functions from [SuiteSparse](http://faculty.cse.tamu.edu/davis/suitesparse.html).

```@docs
Base.:*(::AbstractMatrix, ::AbstractMatrix)
Base.:\(::AbstractMatrix, ::AbstractVecOrMat)
LinearAlgebra.dot
LinearAlgebra.cross
LinearAlgebra.factorize
LinearAlgebra.Diagonal
LinearAlgebra.Bidiagonal
LinearAlgebra.SymTridiagonal
LinearAlgebra.Tridiagonal
LinearAlgebra.Symmetric
LinearAlgebra.Hermitian
LinearAlgebra.LowerTriangular
LinearAlgebra.UpperTriangular
LinearAlgebra.UniformScaling
LinearAlgebra.lu
LinearAlgebra.lu!
LinearAlgebra.cholesky
LinearAlgebra.cholesky!
LinearAlgebra.lowrankupdate
LinearAlgebra.lowrankdowndate
LinearAlgebra.lowrankupdate!
LinearAlgebra.lowrankdowndate!
LinearAlgebra.ldlt
LinearAlgebra.ldlt!
LinearAlgebra.qr
LinearAlgebra.qr!
LinearAlgebra.QR
LinearAlgebra.QRCompactWY
LinearAlgebra.QRPivoted
LinearAlgebra.lq!
LinearAlgebra.lq
LinearAlgebra.bunchkaufman
LinearAlgebra.bunchkaufman!
LinearAlgebra.eigvals
LinearAlgebra.eigvals!
LinearAlgebra.eigmax
LinearAlgebra.eigmin
LinearAlgebra.eigvecs
LinearAlgebra.eigen
LinearAlgebra.eigen!
LinearAlgebra.hessenberg
LinearAlgebra.hessenberg!
LinearAlgebra.schur!
LinearAlgebra.schur
LinearAlgebra.ordschur
LinearAlgebra.ordschur!
LinearAlgebra.svd
LinearAlgebra.svd!
LinearAlgebra.svdvals
LinearAlgebra.svdvals!
LinearAlgebra.Givens
LinearAlgebra.givens
LinearAlgebra.triu
LinearAlgebra.triu!
LinearAlgebra.tril
LinearAlgebra.tril!
LinearAlgebra.diagind
LinearAlgebra.diag
LinearAlgebra.diagm
LinearAlgebra.rank
LinearAlgebra.norm
LinearAlgebra.opnorm
LinearAlgebra.normalize!
LinearAlgebra.normalize
LinearAlgebra.cond
LinearAlgebra.condskeel
LinearAlgebra.tr
LinearAlgebra.det
LinearAlgebra.logdet
LinearAlgebra.logabsdet
Base.inv(::AbstractMatrix)
LinearAlgebra.pinv
LinearAlgebra.nullspace
Base.kron
LinearAlgebra.exp(::StridedMatrix{<:LinearAlgebra.BlasFloat})
LinearAlgebra.log(::StridedMatrix)
LinearAlgebra.sqrt(::StridedMatrix{<:Real})
LinearAlgebra.cos(::StridedMatrix{<:Real})
LinearAlgebra.sin(::StridedMatrix{<:Real})
LinearAlgebra.sincos(::StridedMatrix{<:Real})
LinearAlgebra.tan(::StridedMatrix{<:Real})
LinearAlgebra.sec(::StridedMatrix)
LinearAlgebra.csc(::StridedMatrix)
LinearAlgebra.cot(::StridedMatrix)
LinearAlgebra.cosh(::StridedMatrix)
LinearAlgebra.sinh(::StridedMatrix)
LinearAlgebra.tanh(::StridedMatrix)
LinearAlgebra.sech(::StridedMatrix)
LinearAlgebra.csch(::StridedMatrix)
LinearAlgebra.coth(::StridedMatrix)
LinearAlgebra.acos(::StridedMatrix)
LinearAlgebra.asin(::StridedMatrix)
LinearAlgebra.atan(::StridedMatrix)
LinearAlgebra.asec(::StridedMatrix)
LinearAlgebra.acsc(::StridedMatrix)
LinearAlgebra.acot(::StridedMatrix)
LinearAlgebra.acosh(::StridedMatrix)
LinearAlgebra.asinh(::StridedMatrix)
LinearAlgebra.atanh(::StridedMatrix)
LinearAlgebra.asech(::StridedMatrix)
LinearAlgebra.acsch(::StridedMatrix)
LinearAlgebra.acoth(::StridedMatrix)
LinearAlgebra.lyap
LinearAlgebra.sylvester
LinearAlgebra.issuccess
LinearAlgebra.issymmetric
LinearAlgebra.isposdef
LinearAlgebra.isposdef!
LinearAlgebra.istril
LinearAlgebra.istriu
LinearAlgebra.isdiag
LinearAlgebra.ishermitian
Base.transpose
LinearAlgebra.transpose!
Base.adjoint
LinearAlgebra.adjoint!
Base.copy(::Union{Transpose,Adjoint})
LinearAlgebra.stride1
LinearAlgebra.checksquare
LinearAlgebra.peakflops
```

## Low-level matrix operations

In many cases there are in-place versions of matrix operations that allow you to supply
a pre-allocated output vector or matrix.  This is useful when optimizing critical code in order
to avoid the overhead of repeated allocations. These in-place operations are suffixed with `!`
below (e.g. `mul!`) according to the usual Julia convention.

```@docs
LinearAlgebra.mul!
LinearAlgebra.lmul!
LinearAlgebra.rmul!
LinearAlgebra.ldiv!
LinearAlgebra.rdiv!
```

## BLAS Functions

In Julia (as in much of scientific computation), dense linear-algebra operations are based on
the [LAPACK library](http://www.netlib.org/lapack/), which in turn is built on top of basic linear-algebra
building-blocks known as the [BLAS](http://www.netlib.org/blas/). There are highly optimized
implementations of BLAS available for every computer architecture, and sometimes in high-performance
linear algebra routines it is useful to call the BLAS functions directly.

`LinearAlgebra.BLAS` provides wrappers for some of the BLAS functions. Those BLAS functions
that overwrite one of the input arrays have names ending in `'!'`.  Usually, a BLAS function has
four methods defined, for [`Float64`](@ref), [`Float32`](@ref), `ComplexF64`, and `ComplexF32` arrays.

### [BLAS Character Arguments](@id stdlib-blas-chars)
Many BLAS functions accept arguments that determine whether to transpose an argument (`trans`),
which triangle of a matrix to reference (`uplo` or `ul`),
whether the diagonal of a triangular matrix can be assumed to
be all ones (`dA`) or which side of a matrix multiplication
the input argument belongs on (`side`). The possibilities are:

#### [Multplication Order](@id stdlib-blas-side)
| `side` | Meaning                                                             |
|:-------|:--------------------------------------------------------------------|
| `'L'`  | The argument goes on the *left* side of a matrix-matrix operation.  |
| `'R'`  | The argument goes on the *right* side of a matrix-matrix operation. |

#### [Triangle Referencing](@id stdlib-blas-uplo)
| `uplo`/`ul` | Meaning                                               |
|:------------|:------------------------------------------------------|
| `'U'`       | Only the *upper* triangle of the matrix will be used. |
| `'L'`       | Only the *lower* triangle of the matrix will be used. |

#### [Transposition Operation](@id stdlib-blas-trans)
| `trans`/`tX` | Meaning                                                 |
|:-------------|:--------------------------------------------------------|
| `'N'`        | The input matrix `X` is not transposed or conjugated.   |
| `'T'`        | The input matrix `X` will be transposed.                |
| `'C'`        | The input matrix `X` will be conjugated and transposed. |

#### [Unit Diagonal](@id stdlib-blas-diag)
| `diag`/`dX` | Meaning                                                   |
|:------------|:----------------------------------------------------------|
| `'N'`       | The diagonal values of the matrix `X` will be read.       |
| `'U'`       | The diagonal of the matrix `X` is assumed to be all ones. |

```@docs
LinearAlgebra.BLAS
LinearAlgebra.BLAS.dotu
LinearAlgebra.BLAS.dotc
LinearAlgebra.BLAS.blascopy!
LinearAlgebra.BLAS.nrm2
LinearAlgebra.BLAS.asum
LinearAlgebra.axpy!
LinearAlgebra.BLAS.scal!
LinearAlgebra.BLAS.scal
LinearAlgebra.BLAS.ger!
LinearAlgebra.BLAS.syr!
LinearAlgebra.BLAS.syrk!
LinearAlgebra.BLAS.syrk
LinearAlgebra.BLAS.her!
LinearAlgebra.BLAS.herk!
LinearAlgebra.BLAS.herk
LinearAlgebra.BLAS.gbmv!
LinearAlgebra.BLAS.gbmv
LinearAlgebra.BLAS.sbmv!
LinearAlgebra.BLAS.sbmv(::Any, ::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.sbmv(::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.gemm!
LinearAlgebra.BLAS.gemm(::Any, ::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.gemm(::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.gemv!
LinearAlgebra.BLAS.gemv(::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.gemv(::Any, ::Any, ::Any)
LinearAlgebra.BLAS.symm!
LinearAlgebra.BLAS.symm(::Any, ::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.symm(::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.symv!
LinearAlgebra.BLAS.symv(::Any, ::Any, ::Any, ::Any)
LinearAlgebra.BLAS.symv(::Any, ::Any, ::Any)
LinearAlgebra.BLAS.trmm!
LinearAlgebra.BLAS.trmm
LinearAlgebra.BLAS.trsm!
LinearAlgebra.BLAS.trsm
LinearAlgebra.BLAS.trmv!
LinearAlgebra.BLAS.trmv
LinearAlgebra.BLAS.trsv!
LinearAlgebra.BLAS.trsv
LinearAlgebra.BLAS.set_num_threads
LinearAlgebra.I
```

## LAPACK Functions

`LinearAlgebra.LAPACK` provides wrappers for some of the LAPACK functions for linear algebra.
 Those functions that overwrite one of the input arrays have names ending in `'!'`.

Usually a function has 4 methods defined, one each for [`Float64`](@ref), [`Float32`](@ref),
`ComplexF64` and `ComplexF32` arrays.

Note that the LAPACK API provided by Julia can and will change in the future. Since this API is
not user-facing, there is no commitment to support/deprecate this specific set of functions in
future releases.

```@docs
LinearAlgebra.LAPACK
LinearAlgebra.LAPACK.gbtrf!
LinearAlgebra.LAPACK.gbtrs!
LinearAlgebra.LAPACK.gebal!
LinearAlgebra.LAPACK.gebak!
LinearAlgebra.LAPACK.gebrd!
LinearAlgebra.LAPACK.gelqf!
LinearAlgebra.LAPACK.geqlf!
LinearAlgebra.LAPACK.geqrf!
LinearAlgebra.LAPACK.geqp3!
LinearAlgebra.LAPACK.gerqf!
LinearAlgebra.LAPACK.geqrt!
LinearAlgebra.LAPACK.geqrt3!
LinearAlgebra.LAPACK.getrf!
LinearAlgebra.LAPACK.tzrzf!
LinearAlgebra.LAPACK.ormrz!
LinearAlgebra.LAPACK.gels!
LinearAlgebra.LAPACK.gesv!
LinearAlgebra.LAPACK.getrs!
LinearAlgebra.LAPACK.getri!
LinearAlgebra.LAPACK.gesvx!
LinearAlgebra.LAPACK.gelsd!
LinearAlgebra.LAPACK.gelsy!
LinearAlgebra.LAPACK.gglse!
LinearAlgebra.LAPACK.geev!
LinearAlgebra.LAPACK.gesdd!
LinearAlgebra.LAPACK.gesvd!
LinearAlgebra.LAPACK.ggsvd!
LinearAlgebra.LAPACK.ggsvd3!
LinearAlgebra.LAPACK.geevx!
LinearAlgebra.LAPACK.ggev!
LinearAlgebra.LAPACK.gtsv!
LinearAlgebra.LAPACK.gttrf!
LinearAlgebra.LAPACK.gttrs!
LinearAlgebra.LAPACK.orglq!
LinearAlgebra.LAPACK.orgqr!
LinearAlgebra.LAPACK.orgql!
LinearAlgebra.LAPACK.orgrq!
LinearAlgebra.LAPACK.ormlq!
LinearAlgebra.LAPACK.ormqr!
LinearAlgebra.LAPACK.ormql!
LinearAlgebra.LAPACK.ormrq!
LinearAlgebra.LAPACK.gemqrt!
LinearAlgebra.LAPACK.posv!
LinearAlgebra.LAPACK.potrf!
LinearAlgebra.LAPACK.potri!
LinearAlgebra.LAPACK.potrs!
LinearAlgebra.LAPACK.pstrf!
LinearAlgebra.LAPACK.ptsv!
LinearAlgebra.LAPACK.pttrf!
LinearAlgebra.LAPACK.pttrs!
LinearAlgebra.LAPACK.trtri!
LinearAlgebra.LAPACK.trtrs!
LinearAlgebra.LAPACK.trcon!
LinearAlgebra.LAPACK.trevc!
LinearAlgebra.LAPACK.trrfs!
LinearAlgebra.LAPACK.stev!
LinearAlgebra.LAPACK.stebz!
LinearAlgebra.LAPACK.stegr!
LinearAlgebra.LAPACK.stein!
LinearAlgebra.LAPACK.syconv!
LinearAlgebra.LAPACK.sysv!
LinearAlgebra.LAPACK.sytrf!
LinearAlgebra.LAPACK.sytri!
LinearAlgebra.LAPACK.sytrs!
LinearAlgebra.LAPACK.hesv!
LinearAlgebra.LAPACK.hetrf!
LinearAlgebra.LAPACK.hetri!
LinearAlgebra.LAPACK.hetrs!
LinearAlgebra.LAPACK.syev!
LinearAlgebra.LAPACK.syevr!
LinearAlgebra.LAPACK.sygvd!
LinearAlgebra.LAPACK.bdsqr!
LinearAlgebra.LAPACK.bdsdc!
LinearAlgebra.LAPACK.gecon!
LinearAlgebra.LAPACK.gehrd!
LinearAlgebra.LAPACK.orghr!
LinearAlgebra.LAPACK.gees!
LinearAlgebra.LAPACK.gges!
LinearAlgebra.LAPACK.trexc!
LinearAlgebra.LAPACK.trsen!
LinearAlgebra.LAPACK.tgsen!
LinearAlgebra.LAPACK.trsyl!
```

```@meta
DocTestSetup = nothing
```
