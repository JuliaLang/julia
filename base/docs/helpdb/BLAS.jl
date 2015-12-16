# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.LinAlg.BLAS

"""
    ger!(alpha, x, y, A)

Rank-1 update of the matrix `A` with vectors `x` and `y` as `alpha*x*y' + A`.
"""
LinAlg.BLAS.ger!

"""
    gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` or `alpha*A'*x + beta*y` according to `trans` ('N'
or 'T'). The matrix `A` is a general band matrix of dimension `m` by `size(A,2)` with `kl`
sub-diagonals and `ku` super-diagonals. Returns the updated `y`.
"""
LinAlg.BLAS.gbmv!

"""
    gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

Returns `alpha*A*x` or `alpha*A'*x` according to `trans` ('N' or 'T'). The matrix `A` is a
general band matrix of dimension `m` by `size(A,2)` with `kl` sub-diagonals and `ku`
super-diagonals.
"""
LinAlg.BLAS.gbmv

"""
    gemm!(tA, tB, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or the other three variants according to `tA` (transpose
`A`) and `tB`. Returns the updated `C`.
"""
LinAlg.BLAS.gemm!

"""
    gemv!(tA, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y` or `alpha*A'x + beta*y` according to `tA`
(transpose `A`). Returns the updated `y`.
"""
LinAlg.BLAS.gemv!

"""
    blascopy!(n, X, incx, Y, incy)

Copy `n` elements of array `X` with stride `incx` to array `Y` with stride `incy`. Returns `Y`.
"""
LinAlg.BLAS.blascopy!

"""
    scal!(n, a, X, incx)

Overwrite `X` with `a*X`. Returns `X`.
"""
LinAlg.BLAS.scal!

"""
    gemv(tA, alpha, A, x)

Returns `alpha*A*x` or `alpha*A'x` according to `tA` (transpose `A`).
"""
LinAlg.BLAS.gemv(tA, alpha, A, x)

"""
    gemv(tA, A, x)

Returns `A*x` or `A'x` according to `tA` (transpose `A`).
"""
LinAlg.BLAS.gemv(tA, A, x)

"""
    syr!(uplo, alpha, x, A)

Rank-1 update of the symmetric matrix `A` with vector `x` as `alpha*x*x.' + A`. When `uplo`
is 'U' the upper triangle of `A` is updated ('L' for lower triangle). Returns `A`.
"""
LinAlg.BLAS.syr!

"""
    trsm!(side, ul, tA, dA, alpha, A, B)

Overwrite `B` with the solution to `A*X = alpha*B` or one of the other three variants
determined by `side` (`A` on left or right of `X`) and `tA` (transpose `A`). Only the `ul`
triangle of `A` is used. `dA` indicates if `A` is unit-triangular (the diagonal is assumed
to be all ones). Returns the updated `B`.
"""
LinAlg.BLAS.trsm!

"""
    trsv!(ul, tA, dA, A, b)

Overwrite `b` with the solution to `A*x = b` or one of the other two variants determined by
`tA` (transpose `A`) and `ul` (triangle of `A` used). `dA` indicates if `A` is
unit-triangular (the diagonal is assumed to be all ones). Returns the updated `b`.
"""
LinAlg.BLAS.trsv!

"""
    her!(uplo, alpha, x, A)

Methods for complex arrays only. Rank-1 update of the Hermitian matrix `A` with vector `x`
as `alpha*x*x' + A`. When `uplo` is 'U' the upper triangle of `A` is updated ('L' for lower
triangle). Returns `A`.
"""
LinAlg.BLAS.her!

"""
    trsv(ul, tA, dA, A, b)

Returns the solution to `A*x = b` or one of the other two variants determined by `tA`
(transpose `A`) and `ul` (triangle of `A` is used.) `dA` indicates if `A` is unit-triangular
(the diagonal is assumed to be all ones).
"""
LinAlg.BLAS.trsv

"""
    dot(n, X, incx, Y, incy)

Dot product of two vectors consisting of `n` elements of array `X` with stride `incx` and
`n` elements of array `Y` with stride `incy`.
"""
LinAlg.BLAS.dot

"""
    dotu(n, X, incx, Y, incy)

Dot function for two complex vectors.
"""
LinAlg.BLAS.dotu

"""
    herk!(uplo, trans, alpha, A, beta, C)

Methods for complex arrays only. Rank-k update of the Hermitian matrix `C` as `alpha*A*A' +
beta*C` or `alpha*A'*A + beta*C` according to whether `trans` is 'N' or 'T'. When `uplo` is
'U' the upper triangle of `C` is updated ('L' for lower triangle). Returns `C`.
"""
LinAlg.BLAS.herk!

"""
    trmv(side, ul, tA, dA, alpha, A, b)

Returns `alpha*A*b` or one of the other three variants determined by `side` (`A` on left or
right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA` indicates if
`A` is unit-triangular (the diagonal is assumed to be all ones).
"""
LinAlg.BLAS.trmv

"""
    symv(ul, alpha, A, x)

Returns `alpha*A*x`. `A` is assumed to be symmetric. Only the `ul` triangle of `A` is used.
"""
LinAlg.BLAS.symv(ul, alpha, A, x)

"""
    symv(ul, A, x)

Returns `A*x`. `A` is assumed to be symmetric. Only the `ul` triangle of `A` is used.
"""
LinAlg.BLAS.symv(ul, A, x)

"""
    dotc(n, X, incx, U, incy)

Dot function for two complex vectors conjugating the first vector.
"""
LinAlg.BLAS.dotc

"""
    axpy!(a, X, Y)

Overwrite `Y` with `a*X + Y`. Returns `Y`.
"""
LinAlg.BLAS.axpy!

"""
    syrk!(uplo, trans, alpha, A, beta, C)

Rank-k update of the symmetric matrix `C` as `alpha*A*A.' + beta*C` or `alpha*A.'*A +
beta*C` according to whether `trans` is 'N' or 'T'. When `uplo` is 'U' the upper triangle of
`C` is updated ('L' for lower triangle). Returns `C`.
"""
LinAlg.BLAS.syrk!

"""
    sbmv(uplo, k, alpha, A, x)

Returns `alpha*A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
"""
LinAlg.BLAS.sbmv(uplo, k, alpha, A, x)

"""
    sbmv(uplo, k, A, x)

Returns `A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
"""
LinAlg.BLAS.sbmv(uplo, k, A, x)

"""
    sbmv!(uplo, k, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` where `A` is a a symmetric band matrix of order
`size(A,2)` with `k` super-diagonals stored in the argument `A`. The storage layout for `A`
is described the reference BLAS module, level-2 BLAS at
<http://www.netlib.org/lapack/explore-html/>.

Returns the updated `y`.
"""
LinAlg.BLAS.sbmv!

"""
    symv!(ul, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y`. `A` is assumed to be symmetric. Only the `ul`
triangle of `A` is used. Returns the updated `y`.
"""
LinAlg.BLAS.symv!

"""
    symm(side, ul, alpha, A, B)

Returns `alpha*A*B` or `alpha*B*A` according to `side`. `A` is assumed to be symmetric. Only
the `ul` triangle of `A` is used.
"""
LinAlg.BLAS.symm(side, ul, alpha, A, B)

"""
    symm(side, ul, A, B)

Returns `A*B` or `B*A` according to `side`. `A` is assumed to be symmetric. Only the `ul`
triangle of `A` is used.
"""
LinAlg.BLAS.symm(side, ul, A, B)

"""
    symm(tA, tB, alpha, A, B)

Returns `alpha*A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
LinAlg.BLAS.symm(tA::Char, tB::Char, alpha, A, B)

"""
    herk(uplo, trans, alpha, A)

Methods for complex arrays only. Returns either the upper triangle or the lower triangle,
according to `uplo` ('U' or 'L'), of `alpha*A*A'` or `alpha*A'*A`, according to `trans` ('N'
or 'T').
"""
LinAlg.BLAS.herk

"""
    syrk(uplo, trans, alpha, A)

Returns either the upper triangle or the lower triangle, according to `uplo` ('U' or 'L'),
of `alpha*A*A.'` or `alpha*A.'*A`, according to `trans` ('N' or 'T').
"""
LinAlg.BLAS.syrk

"""
    trsm(side, ul, tA, dA, alpha, A, B)

Returns the solution to `A*X = alpha*B` or one of the other three variants determined by
`side` (`A` on left or right of `X`) and `tA` (transpose `A`). Only the `ul` triangle of `A`
is used. `dA` indicates if `A` is unit-triangular (the diagonal is assumed to be all ones).
"""
LinAlg.BLAS.trsm

"""
    blas_set_num_threads(n)

Set the number of threads the BLAS library should use.
"""
LinAlg.BLAS.blas_set_num_threads

"""
    asum(n, X, incx)

Sum of the absolute values of the first `n` elements of array `X` with stride `incx`.
"""
LinAlg.BLAS.asum

"""
    trmv!(side, ul, tA, dA, alpha, A, b)

Update `b` as `alpha*A*b` or one of the other three variants determined by `side` (`A` on
left or right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA`
indicates if `A` is unit-triangular (the diagonal is assumed to be all ones). Returns the
updated `b`.
"""
LinAlg.BLAS.trmv!

"""
    gemm(tA, tB, alpha, A, B)

Returns `alpha*A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
LinAlg.BLAS.gemm(tA, tB, alpha, A, B)

"""
    gemm(tA, tB, A, B)

Returns `A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
LinAlg.BLAS.gemm(tA, tB, A, B)

"""
    symm!(side, ul, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or `alpha*B*A + beta*C` according to `side`. `A` is
assumed to be symmetric. Only the `ul` triangle of `A` is used. Returns the updated `C`.
"""
LinAlg.BLAS.symm!

"""
    scal(n, a, X, incx)

Returns `a*X`.
"""
LinAlg.BLAS.scal

"""
    nrm2(n, X, incx)

2-norm of a vector consisting of `n` elements of array `X` with stride `incx`.
"""
LinAlg.BLAS.nrm2

"""
    trmm!(side, ul, tA, dA, alpha, A, B)

Update `B` as `alpha*A*B` or one of the other three variants determined by `side` (`A` on
left or right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA`
indicates if `A` is unit-triangular (the diagonal is assumed to be all ones). Returns the
updated `B`.
"""
LinAlg.BLAS.trmm!

"""
    trmm(side, ul, tA, dA, alpha, A, B)

Returns `alpha*A*B` or one of the other three variants determined by `side` (`A` on left or
right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA` indicates if
`A` is unit-triangular (the diagonal is assumed to be all ones).
"""
LinAlg.BLAS.trmm
