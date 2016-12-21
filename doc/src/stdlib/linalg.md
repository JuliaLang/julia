# Linear Algebra

## Standard Functions

Linear algebra functions in Julia are largely implemented by calling functions from [LAPACK](http://www.netlib.org/lapack/).
 Sparse factorizations call functions from [SuiteSparse](http://faculty.cse.tamu.edu/davis/suitesparse.html).

```@docs
Base.:*(::AbstractArray, ::AbstractArray)
Base.:\(::AbstractArray, ::Any)
Base.LinAlg.dot
Base.LinAlg.vecdot
Base.LinAlg.cross
Base.LinAlg.factorize
Base.LinAlg.Diagonal
Base.LinAlg.Bidiagonal
Base.LinAlg.SymTridiagonal
Base.LinAlg.Tridiagonal
Base.LinAlg.Symmetric
Base.LinAlg.Hermitian
Base.LinAlg.lu
Base.LinAlg.lufact
Base.LinAlg.lufact!
Base.LinAlg.chol
Base.LinAlg.cholfact
Base.LinAlg.cholfact!
Base.LinAlg.lowrankupdate
Base.LinAlg.lowrankdowndate
Base.LinAlg.lowrankupdate!
Base.LinAlg.lowrankdowndate!
Base.LinAlg.ldltfact
Base.LinAlg.ldltfact!
Base.LinAlg.qr
Base.LinAlg.qr!
Base.LinAlg.qrfact
Base.LinAlg.qrfact!
Base.LinAlg.lqfact!
Base.LinAlg.lqfact
Base.LinAlg.lq
Base.LinAlg.bkfact
Base.LinAlg.bkfact!
Base.LinAlg.eig
Base.LinAlg.eigvals
Base.LinAlg.eigvals!
Base.LinAlg.eigmax
Base.LinAlg.eigmin
Base.LinAlg.eigvecs
Base.LinAlg.eigfact
Base.LinAlg.eigfact!
Base.LinAlg.hessfact
Base.LinAlg.hessfact!
Base.LinAlg.schurfact
Base.LinAlg.schurfact!
Base.LinAlg.schur
Base.LinAlg.ordschur
Base.LinAlg.ordschur!
Base.LinAlg.svdfact
Base.LinAlg.svdfact!
Base.LinAlg.svd
Base.LinAlg.svdvals
Base.LinAlg.Givens
Base.LinAlg.givens
Base.LinAlg.triu
Base.LinAlg.triu!
Base.LinAlg.tril
Base.LinAlg.tril!
Base.LinAlg.diagind
Base.LinAlg.diag
Base.LinAlg.diagm
Base.LinAlg.scale!
Base.LinAlg.rank
Base.LinAlg.norm
Base.LinAlg.vecnorm
Base.LinAlg.normalize!
Base.LinAlg.normalize
Base.LinAlg.cond
Base.LinAlg.condskeel
Base.LinAlg.trace
Base.LinAlg.det
Base.LinAlg.logdet
Base.LinAlg.logabsdet
Base.inv
Base.LinAlg.pinv
Base.LinAlg.nullspace
Base.repmat
Base.repeat
Base.kron
Base.SparseArrays.blkdiag
Base.LinAlg.linreg
Base.LinAlg.expm
Base.LinAlg.logm
Base.LinAlg.sqrtm
Base.LinAlg.lyap
Base.LinAlg.sylvester
Base.LinAlg.issymmetric
Base.LinAlg.isposdef
Base.LinAlg.isposdef!
Base.LinAlg.istril
Base.LinAlg.istriu
Base.LinAlg.isdiag
Base.LinAlg.ishermitian
Base.LinAlg.RowVector
Base.transpose
Base.transpose!
Base.ctranspose
Base.ctranspose!
Base.LinAlg.eigs(::Any)
Base.LinAlg.eigs(::Any, ::Any)
Base.LinAlg.svds
Base.LinAlg.peakflops
```

## Low-level matrix operations

Matrix operations involving transpositions operations like `A' \ B` are converted by the Julia
parser into calls to specially named functions like [`Ac_ldiv_B`](@ref). If you want to overload these
operations for your own types, then it is useful to know the names of these functions.

Also, in many cases there are in-place versions of matrix operations that allow you to supply
a pre-allocated output vector or matrix.  This is useful when optimizing critical code in order
to avoid the overhead of repeated allocations. These in-place operations are suffixed with `!`
below (e.g. [`A_mul_B!`](@ref)) according to the usual Julia convention.

```@docs
Base.LinAlg.A_ldiv_B!
Base.A_ldiv_Bc
Base.A_ldiv_Bt
Base.LinAlg.A_mul_B!
Base.A_mul_Bc
Base.A_mul_Bt
Base.A_rdiv_Bc
Base.A_rdiv_Bt
Base.Ac_ldiv_B
Base.LinAlg.Ac_ldiv_B!
Base.Ac_ldiv_Bc
Base.Ac_mul_B
Base.Ac_mul_Bc
Base.Ac_rdiv_B
Base.Ac_rdiv_Bc
Base.At_ldiv_B
Base.LinAlg.At_ldiv_B!
Base.At_ldiv_Bt
Base.At_mul_B
Base.At_mul_Bt
Base.At_rdiv_B
Base.At_rdiv_Bt
```

## BLAS Functions

In Julia (as in much of scientific computation), dense linear-algebra operations are based on
the [LAPACK library](http://www.netlib.org/lapack/), which in turn is built on top of basic linear-algebra
building-blocks known as the [BLAS](http://www.netlib.org/blas/).  There are highly optimized
implementations of BLAS available for every computer architecture, and sometimes in high-performance
linear algebra routines it is useful to call the BLAS functions directly.

`Base.LinAlg.BLAS` provides wrappers for some of the BLAS functions. Those BLAS functions
that overwrite one of the input arrays have names ending in `'!'`.  Usually, a BLAS function has
four methods defined, for `Float64`, `Float32`, `Complex128`, and `Complex64` arrays.

### [BLAS Character Arguments](@id stdlib-blas-chars)
Many BLAS functions accept arguments that determine whether to transpose an argument (`trans`),
which triangle of a matrix to reference (`uplo` or `ul`),
whether the diagonal of a triangular matrix can be assumed to
be all ones (`dA`) or which side of a matrix multiplication
the input argument belongs on (`side`). The possiblities are:

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
Base.LinAlg.BLAS.dotu
Base.LinAlg.BLAS.dotc
Base.LinAlg.BLAS.blascopy!
Base.LinAlg.BLAS.nrm2
Base.LinAlg.BLAS.asum
Base.LinAlg.axpy!
Base.LinAlg.BLAS.scal!
Base.LinAlg.BLAS.scal
Base.LinAlg.BLAS.ger!
Base.LinAlg.BLAS.syr!
Base.LinAlg.BLAS.syrk!
Base.LinAlg.BLAS.syrk
Base.LinAlg.BLAS.her!
Base.LinAlg.BLAS.herk!
Base.LinAlg.BLAS.herk
Base.LinAlg.BLAS.gbmv!
Base.LinAlg.BLAS.gbmv
Base.LinAlg.BLAS.sbmv!
Base.LinAlg.BLAS.sbmv(::Any, ::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.sbmv(::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.gemm!
Base.LinAlg.BLAS.gemm(::Any, ::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.gemm(::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.gemv!
Base.LinAlg.BLAS.gemv(::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.gemv(::Any, ::Any, ::Any)
Base.LinAlg.BLAS.symm!
Base.LinAlg.BLAS.symm(::Any, ::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.symm(::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.symm(::Char, ::Char, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.symv!
Base.LinAlg.BLAS.symv(::Any, ::Any, ::Any, ::Any)
Base.LinAlg.BLAS.symv(::Any, ::Any, ::Any)
Base.LinAlg.BLAS.trmm!
Base.LinAlg.BLAS.trmm
Base.LinAlg.BLAS.trsm!
Base.LinAlg.BLAS.trsm
Base.LinAlg.BLAS.trmv!
Base.LinAlg.BLAS.trmv
Base.LinAlg.BLAS.trsv!
Base.LinAlg.BLAS.trsv
Base.LinAlg.BLAS.set_num_threads
Base.LinAlg.I
```

## LAPACK Functions

`Base.LinAlg.LAPACK` provides wrappers for some of the LAPACK functions for linear algebra.
 Those functions that overwrite one of the input arrays have names ending in `'!'`.

Usually a function has 4 methods defined, one each for `Float64`, `Float32`, `Complex128` and
`Complex64` arrays.

Note that the LAPACK API provided by Julia can and will change in the future. Since this API is
not user-facing, there is no commitment to support/deprecate this specific set of functions in
future releases.

```@docs
Base.LinAlg.LAPACK.gbtrf!
Base.LinAlg.LAPACK.gbtrs!
Base.LinAlg.LAPACK.gebal!
Base.LinAlg.LAPACK.gebak!
Base.LinAlg.LAPACK.gebrd!
Base.LinAlg.LAPACK.gelqf!
Base.LinAlg.LAPACK.geqlf!
Base.LinAlg.LAPACK.geqrf!
Base.LinAlg.LAPACK.geqp3!
Base.LinAlg.LAPACK.gerqf!
Base.LinAlg.LAPACK.geqrt!
Base.LinAlg.LAPACK.geqrt3!
Base.LinAlg.LAPACK.getrf!
Base.LinAlg.LAPACK.tzrzf!
Base.LinAlg.LAPACK.ormrz!
Base.LinAlg.LAPACK.gels!
Base.LinAlg.LAPACK.gesv!
Base.LinAlg.LAPACK.getrs!
Base.LinAlg.LAPACK.getri!
Base.LinAlg.LAPACK.gesvx!
Base.LinAlg.LAPACK.gelsd!
Base.LinAlg.LAPACK.gelsy!
Base.LinAlg.LAPACK.gglse!
Base.LinAlg.LAPACK.geev!
Base.LinAlg.LAPACK.gesdd!
Base.LinAlg.LAPACK.gesvd!
Base.LinAlg.LAPACK.ggsvd!
Base.LinAlg.LAPACK.ggsvd3!
Base.LinAlg.LAPACK.geevx!
Base.LinAlg.LAPACK.ggev!
Base.LinAlg.LAPACK.gtsv!
Base.LinAlg.LAPACK.gttrf!
Base.LinAlg.LAPACK.gttrs!
Base.LinAlg.LAPACK.orglq!
Base.LinAlg.LAPACK.orgqr!
Base.LinAlg.LAPACK.orgql!
Base.LinAlg.LAPACK.orgrq!
Base.LinAlg.LAPACK.ormlq!
Base.LinAlg.LAPACK.ormqr!
Base.LinAlg.LAPACK.ormql!
Base.LinAlg.LAPACK.ormrq!
Base.LinAlg.LAPACK.gemqrt!
Base.LinAlg.LAPACK.posv!
Base.LinAlg.LAPACK.potrf!
Base.LinAlg.LAPACK.potri!
Base.LinAlg.LAPACK.potrs!
Base.LinAlg.LAPACK.pstrf!
Base.LinAlg.LAPACK.ptsv!
Base.LinAlg.LAPACK.pttrf!
Base.LinAlg.LAPACK.pttrs!
Base.LinAlg.LAPACK.trtri!
Base.LinAlg.LAPACK.trtrs!
Base.LinAlg.LAPACK.trcon!
Base.LinAlg.LAPACK.trevc!
Base.LinAlg.LAPACK.trrfs!
Base.LinAlg.LAPACK.stev!
Base.LinAlg.LAPACK.stebz!
Base.LinAlg.LAPACK.stegr!
Base.LinAlg.LAPACK.stein!
Base.LinAlg.LAPACK.syconv!
Base.LinAlg.LAPACK.sysv!
Base.LinAlg.LAPACK.sytrf!
Base.LinAlg.LAPACK.sytri!
Base.LinAlg.LAPACK.sytrs!
Base.LinAlg.LAPACK.hesv!
Base.LinAlg.LAPACK.hetrf!
Base.LinAlg.LAPACK.hetri!
Base.LinAlg.LAPACK.hetrs!
Base.LinAlg.LAPACK.syev!
Base.LinAlg.LAPACK.syevr!
Base.LinAlg.LAPACK.sygvd!
Base.LinAlg.LAPACK.bdsqr!
Base.LinAlg.LAPACK.bdsdc!
Base.LinAlg.LAPACK.gecon!
Base.LinAlg.LAPACK.gehrd!
Base.LinAlg.LAPACK.orghr!
Base.LinAlg.LAPACK.gees!
Base.LinAlg.LAPACK.gges!
Base.LinAlg.LAPACK.trexc!
Base.LinAlg.LAPACK.trsen!
Base.LinAlg.LAPACK.tgsen!
Base.LinAlg.LAPACK.trsyl!
```
