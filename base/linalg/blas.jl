# This file is a part of Julia. License is MIT: http://julialang.org/license

module BLAS

import Base: copy!, @blasfunc
import Base.LinAlg: axpy!, dot

export
# Level 1
    asum,
    blascopy!,
    dotc,
    dotu,
    scal!,
    scal,
    nrm2,
    iamax,
# Level 2
    gbmv!,
    gbmv,
    gemv!,
    gemv,
    hemv!,
    hemv,
    sbmv!,
    sbmv,
    symv!,
    symv,
    trsv!,
    trsv,
    trmv!,
    trmv,
    ger!,
    syr!,
    her!,
# Level 3
    herk!,
    herk,
    her2k!,
    her2k,
    gemm!,
    gemm,
    symm!,
    symm,
    hemm!,
    hemm,
    syrk!,
    syrk,
    syr2k!,
    syr2k,
    trmm!,
    trmm,
    trsm!,
    trsm


const libblas = Base.libblas_name
const liblapack = Base.liblapack_name

import ..LinAlg: BlasReal, BlasComplex, BlasFloat, BlasInt, DimensionMismatch, checksquare, axpy!

"""
    blas_set_num_threads(n)

Set the number of threads the BLAS library should use.
"""
blas_set_num_threads

# Level 1
## copy

"""
    blascopy!(n, X, incx, Y, incy)

Copy `n` elements of array `X` with stride `incx` to array `Y` with stride `incy`. Returns `Y`.
"""
function blascopy! end

for (fname, elty) in ((:dcopy_,:Float64),
                      (:scopy_,:Float32),
                      (:zcopy_,:Complex128),
                      (:ccopy_,:Complex64))
    @eval begin
        # SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
        function blascopy!(n::Integer, DX::Union{Ptr{$elty},StridedArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},StridedArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, DX, &incx, DY, &incy)
            DY
        end
    end
end

## scal

"""
    scal!(n, a, X, incx)

Overwrite `X` with `a*X`. Returns `X`.
"""
function scal! end

"""
    scal(n, a, X, incx)

Returns `a*X`.
"""
function scal end

for (fname, elty) in ((:dscal_,:Float64),
                      (:sscal_,:Float32),
                      (:zscal_,:Complex128),
                      (:cscal_,:Complex64))
    @eval begin
        # SUBROUTINE DSCAL(N,DA,DX,INCX)
        function scal!(n::Integer, DA::$elty, DX::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, &DA, DX, &incx)
            DX
        end
    end
end
scal(n, DA, DX, incx) = scal!(n, DA, copy(DX), incx)

## dot

"""
    dot(n, X, incx, Y, incy)

Dot product of two vectors consisting of `n` elements of array `X` with stride `incx` and
`n` elements of array `Y` with stride `incy`.
"""
function dot end

"""
    dotc(n, X, incx, U, incy)

Dot function for two complex vectors conjugating the first vector.
"""
function dotc end

"""
    dotu(n, X, incx, Y, incy)

Dot function for two complex vectors.
"""
function dotu end

for (fname, elty) in ((:ddot_,:Float64),
                      (:sdot_,:Float32))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dot(n::Integer, DX::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},DenseArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), $elty,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, DX, &incx, DY, &incy)
        end
    end
end
for (fname, elty) in ((:cblas_zdotc_sub,:Complex128),
                      (:cblas_cdotc_sub,:Complex64))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dotc(n::Integer, DX::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},DenseArray{$elty}}, incy::Integer)
            result = Array($elty, 1)
            ccall((@blasfunc($fname), libblas), Void,
                (BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}),
                 n, DX, incx, DY, incy, result)
            result[1]
        end
    end
end
for (fname, elty) in ((:cblas_zdotu_sub,:Complex128),
                      (:cblas_cdotu_sub,:Complex64))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dotu(n::Integer, DX::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},DenseArray{$elty}}, incy::Integer)
            result = Array($elty, 1)
            ccall((@blasfunc($fname), libblas), Void,
                (BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}),
                 n, DX, incx, DY, incy, result)
            result[1]
        end
    end
end
function dot{T<:BlasReal}(DX::Union{DenseArray{T},StridedVector{T}}, DY::Union{DenseArray{T},StridedVector{T}})
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    dot(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end
function dotc{T<:BlasComplex}(DX::Union{DenseArray{T},StridedVector{T}}, DY::Union{DenseArray{T},StridedVector{T}})
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    dotc(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end
function dotu{T<:BlasComplex}(DX::Union{DenseArray{T},StridedVector{T}}, DY::Union{DenseArray{T},StridedVector{T}})
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    dotu(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end

## nrm2

"""
    nrm2(n, X, incx)

2-norm of a vector consisting of `n` elements of array `X` with stride `incx`.
"""
function nrm2 end

for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:Complex128,:Float64),
                                (:scnrm2_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE DNRM2(N,X,INCX)
        function nrm2(n::Integer, X::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), $ret_type,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, X, &incx)
        end
    end
end
nrm2(x::StridedVector) = nrm2(length(x), pointer(x), stride(x,1))
nrm2(x::Array) = nrm2(length(x), pointer(x), 1)

## asum

"""
    asum(n, X, incx)

Sum of the absolute values of the first `n` elements of array `X` with stride `incx`.
"""
function asum end

for (fname, elty, ret_type) in ((:dasum_,:Float64,:Float64),
                                (:sasum_,:Float32,:Float32),
                                (:dzasum_,:Complex128,:Float64),
                                (:scasum_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE ASUM(N, X, INCX)
        function asum(n::Integer, X::Union{Ptr{$elty},DenseArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), $ret_type,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, X, &incx)
        end
    end
end
asum(x::StridedVector) = asum(length(x), pointer(x), stride(x,1))
asum(x::Array) = asum(length(x), pointer(x), 1)

## axpy

"""
    axpy!(a, X, Y)

Overwrite `Y` with `a*X + Y`. Returns `Y`.
"""
function axpy! end

for (fname, elty) in ((:daxpy_,:Float64),
                      (:saxpy_,:Float32),
                      (:zaxpy_,:Complex128),
                      (:caxpy_,:Complex64))
    @eval begin
                # SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
                # DY <- DA*DX + DY
                #*     .. Scalar Arguments ..
                #      DOUBLE PRECISION DA
                #      INTEGER INCX,INCY,N
                #*     .. Array Arguments ..
                #      DOUBLE PRECISION DX(*),DY(*)
        function axpy!(n::Integer, alpha::($elty), dx::Union{Ptr{$elty}, DenseArray{$elty}}, incx::Integer, dy::Union{Ptr{$elty}, DenseArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, &alpha, dx, &incx, dy, &incy)
            dy
        end
    end
end
function axpy!{T<:BlasFloat,Ta<:Number}(alpha::Ta, x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}})
    if length(x) != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    axpy!(length(x), convert(T,alpha), pointer(x), stride(x, 1), pointer(y), stride(y, 1))
    y
end

function axpy!{T<:BlasFloat,Ta<:Number,Ti<:Integer}(alpha::Ta, x::Array{T}, rx::Union{UnitRange{Ti},Range{Ti}},
                                         y::Array{T}, ry::Union{UnitRange{Ti},Range{Ti}})

    if length(rx) != length(ry)
        throw(DimensionMismatch("ranges of differing lengths"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(ArgumentError("range out of bounds for x, of length $(length(x))"))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(ArgumentError("range out of bounds for y, of length $(length(y))"))
    end
    axpy!(length(rx), convert(T, alpha), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
    y
end

## iamax
for (fname, elty) in ((:idamax_,:Float64),
                      (:isamax_,:Float32),
                      (:izamax_,:Complex128),
                      (:icamax_,:Complex64))
    @eval begin
        function iamax(n::Integer, dx::Union{Ptr{$elty}, DenseArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas),BlasInt,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                &n, dx, &incx)
        end
    end
end
iamax(dx::StridedVector) = iamax(length(dx), pointer(dx), stride(dx,1))
iamax(dx::Array) = iamax(length(dx), pointer(dx), 1)

# Level 2
## mv
### gemv
for (fname, elty) in ((:dgemv_,:Float64),
                      (:sgemv_,:Float32),
                      (:zgemv_,:Complex128),
                      (:cgemv_,:Complex64))
    @eval begin
             #SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             #*     .. Scalar Arguments ..
             #      DOUBLE PRECISION ALPHA,BETA
             #      INTEGER INCX,INCY,LDA,M,N
             #      CHARACTER TRANS
             #*     .. Array Arguments ..
             #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function gemv!(trans::Char, alpha::($elty), A::StridedVecOrMat{$elty}, X::StridedVector{$elty}, beta::($elty), Y::StridedVector{$elty})
            m,n = size(A,1),size(A,2)
            if trans == 'N' && (length(X) != n || length(Y) != m)
                throw(DimensionMismatch("A has dimensions $(size(A)), X has length $(length(X)) and Y has length $(length(Y))"))
            elseif trans == 'C' && (length(X) != m || length(Y) != n)
                throw(DimensionMismatch("A' has dimensions $n, $m, X has length $(length(X)) and Y has length $(length(Y))"))
            elseif trans == 'T' && (length(X) != m || length(Y) != n)
                throw(DimensionMismatch("A.' has dimensions $n, $m, X has length $(length(X)) and Y has length $(length(Y))"))
            end
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &trans, &size(A,1), &size(A,2), &alpha,
                 A, &max(1,stride(A,2)), X, &stride(X,1),
                 &beta, Y, &stride(Y,1))
            Y
        end
        function gemv(trans::Char, alpha::($elty), A::StridedMatrix{$elty}, X::StridedVector{$elty})
            gemv!(trans, alpha, A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
        function gemv(trans::Char, A::StridedMatrix{$elty}, X::StridedVector{$elty})
            gemv!(trans, one($elty), A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
    end
end

"""
    gemv!(tA, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y` or `alpha*A'x + beta*y` according to `tA`
(transpose `A`). Returns the updated `y`.
"""
gemv!

"""
    gemv(tA, alpha, A, x)

Returns `alpha*A*x` or `alpha*A'x` according to `tA` (transpose `A`).
"""
gemv(tA, alpha, A, x)

"""
    gemv(tA, A, x)

Returns `A*x` or `A'x` according to `tA` (transpose `A`).
"""
gemv(tA, A, x)

### (GB) general banded matrix-vector multiplication

"""
    gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` or `alpha*A'*x + beta*y` according to `trans` ('N'
or 'T'). The matrix `A` is a general band matrix of dimension `m` by `size(A,2)` with `kl`
sub-diagonals and `ku` super-diagonals. Returns the updated `y`.
"""
function gbmv! end

"""
    gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

Returns `alpha*A*x` or `alpha*A'*x` according to `trans` ('N' or 'T'). The matrix `A` is a
general band matrix of dimension `m` by `size(A,2)` with `kl` sub-diagonals and `ku`
super-diagonals.
"""
function gbmv end

for (fname, elty) in ((:dgbmv_,:Float64),
                      (:sgbmv_,:Float32),
                      (:zgbmv_,:Complex128),
                      (:cgbmv_,:Complex64))
    @eval begin
             # SUBROUTINE DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,KL,KU,LDA,M,N
             #       CHARACTER TRANS
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function gbmv!(trans::Char, m::Integer, kl::Integer, ku::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, beta::($elty), y::StridedVector{$elty})
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}),
                 &trans, &m, &size(A,2), &kl,
                 &ku, &alpha, A, &max(1,stride(A,2)),
                 x, &stride(x,1), &beta, y, &stride(y,1))
            y
        end
        function gbmv(trans::Char, m::Integer, kl::Integer, ku::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = size(A,2)
            leny = trans == 'N' ? m : n
            gbmv!(trans, m, kl, ku, alpha, A, x, zero($elty), similar(x, $elty, leny))
        end
        function gbmv(trans::Char, m::Integer, kl::Integer, ku::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            gbmv(trans, m, kl, ku, one($elty), A, x)
        end
    end
end

### symv

"""
    symv!(ul, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y`. `A` is assumed to be symmetric. Only the `ul`
triangle of `A` is used. Returns the updated `y`.
"""
function symv! end

for (fname, elty, lib) in ((:dsymv_,:Float64,libblas),
                           (:ssymv_,:Float32,libblas),
                           (:zsymv_,:Complex128,liblapack),
                           (:csymv_,:Complex64,liblapack))
    # Note that the complex symv are not BLAS but auiliary functions in LAPACK
    @eval begin
             #      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             #     .. Scalar Arguments ..
             #      DOUBLE PRECISION ALPHA,BETA
             #      INTEGER INCX,INCY,LDA,N
             #      CHARACTER UPLO
             #     .. Array Arguments ..
             #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function symv!(uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty},beta::($elty), y::StridedVector{$elty})
            m, n = size(A)
            if m != n
                throw(DimensionMismatch("matrix A is $m by $n but must be square"))
            end
            if n != length(x)
                throw(DimensionMismatch("A has size $(size(A)), and x has length $(length(x))"))
            end
            if m != length(y)
                throw(DimensionMismatch("A has size $(size(A)), and y has length $(length(y))"))
            end
            ccall((@blasfunc($fname), $lib), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &alpha, A,
                 &max(1,stride(A,2)), x, &stride(x,1), &beta,
                 y, &stride(y,1))
            y
        end
        function symv(uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
                symv!(uplo, alpha, A, x, zero($elty), similar(x))
        end
        function symv(uplo::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            symv(uplo, one($elty), A, x)
        end
    end
end

"""
    symv(ul, alpha, A, x)

Returns `alpha*A*x`. `A` is assumed to be symmetric. Only the `ul` triangle of `A` is used.
"""
symv(ul, alpha, A, x)

"""
    symv(ul, A, x)

Returns `A*x`. `A` is assumed to be symmetric. Only the `ul` triangle of `A` is used.
"""
symv(ul, A, x)

### hemv
for (fname, elty) in ((:zhemv_,:Complex128),
                      (:chemv_,:Complex64))
    @eval begin
        function hemv!(uplo::Char, α::$elty, A::StridedMatrix{$elty}, x::StridedVector{$elty}, β::$elty, y::StridedVector{$elty})
            m, n = size(A)
            if m != n
                throw(DimensionMismatch("matrix A is $m by $n but must be square"))
            end
            if n != length(x)
                throw(DimensionMismatch("A has size $(size(A)), and x has length $(length(x))"))
            end
            if m != length(y)
                throw(DimensionMismatch("A has size $(size(A)), and y has length $(length(y))"))
            end
            lda = max(1, stride(A, 2))
            incx = stride(x, 1)
            incy = stride(y, 1)
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}),
                &uplo, &n, &α, A,
                &lda, x, &incx, &β,
                y, &incy)
            y
        end
        function hemv(uplo::Char, α::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            hemv!(uplo, α, A, x, zero($elty), similar(x))
        end
        function hemv(uplo::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            hemv(uplo, one($elty), A, x)
        end
    end
end

### sbmv, (SB) symmetric banded matrix-vector multiplication
for (fname, elty) in ((:dsbmv_,:Float64),
                      (:ssbmv_,:Float32))
    @eval begin
             #       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,K,LDA,N
             #       CHARACTER UPLO
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function sbmv!(uplo::Char, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, beta::($elty), y::StridedVector{$elty})
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &size(A,2), &k, &alpha,
                 A, &max(1,stride(A,2)), x, &stride(x,1),
                 &beta, y, &stride(y,1))
            y
        end
        function sbmv(uplo::Char, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = size(A,2)
            sbmv!(uplo, k, alpha, A, x, zero($elty), similar(x, $elty, n))
        end
        function sbmv(uplo::Char, k::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            sbmv(uplo, k, one($elty), A, x)
        end
    end
end

"""
    sbmv(uplo, k, alpha, A, x)

Returns `alpha*A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
"""
sbmv(uplo, k, alpha, A, x)

"""
    sbmv(uplo, k, A, x)

Returns `A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
"""
sbmv(uplo, k, A, x)

"""
    sbmv!(uplo, k, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` where `A` is a a symmetric band matrix of order
`size(A,2)` with `k` super-diagonals stored in the argument `A`. The storage layout for `A`
is described the reference BLAS module, level-2 BLAS at
<http://www.netlib.org/lapack/explore-html/>.

Returns the updated `y`.
"""
sbmv!

### hbmv, (HB) Hermitian banded matrix-vector multiplication
for (fname, elty) in ((:zhbmv_,:Complex128),
                      (:chbmv_,:Complex64))
    @eval begin
             #       SUBROUTINE ZHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,K,LDA,N
             #       CHARACTER UPLO
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function hbmv!(uplo::Char, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, beta::($elty), y::StridedVector{$elty})
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &size(A,2), &k, &alpha,
                 A, &max(1,stride(A,2)), x, &stride(x,1),
                 &beta, y, &stride(y,1))
            y
        end
        function hbmv(uplo::Char, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = size(A,2)
            hbmv!(uplo, k, alpha, A, x, zero($elty), similar(x, $elty, n))
        end
        function hbmv(uplo::Char, k::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            hbmv(uplo, k, one($elty), A, x)
        end
    end
end

### trmv, Triangular matrix-vector multiplication

"""
    trmv(side, ul, tA, dA, alpha, A, b)

Returns `alpha*A*b` or one of the other three variants determined by `side` (`A` on left or
right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA` indicates if
`A` is unit-triangular (the diagonal is assumed to be all ones).
"""
function trmv end

"""
    trmv!(side, ul, tA, dA, alpha, A, b)

Update `b` as `alpha*A*b` or one of the other three variants determined by `side` (`A` on
left or right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA`
indicates if `A` is unit-triangular (the diagonal is assumed to be all ones). Returns the
updated `b`.
"""
function trmv! end

for (fname, elty) in ((:dtrmv_,:Float64),
                        (:strmv_,:Float32),
                        (:ztrmv_,:Complex128),
                        (:ctrmv_,:Complex64))
    @eval begin
                #       SUBROUTINE DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,LDA,N
                #       CHARACTER DIAG,TRANS,UPLO
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION A(LDA,*),X(*)
        function trmv!(uplo::Char, trans::Char, diag::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = checksquare(A)
            if n != length(x)
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &diag, &n,
                 A, &max(1,stride(A,2)), x, &max(1,stride(x, 1)))
            x
        end
        function trmv(uplo::Char, trans::Char, diag::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            trmv!(uplo, trans, diag, A, copy(x))
        end
    end
end

### trsv, Triangular matrix-vector solve

"""
    trsv!(ul, tA, dA, A, b)

Overwrite `b` with the solution to `A*x = b` or one of the other two variants determined by
`tA` (transpose `A`) and `ul` (triangle of `A` used). `dA` indicates if `A` is
unit-triangular (the diagonal is assumed to be all ones). Returns the updated `b`.
"""
function trsv! end

"""
    trsv(ul, tA, dA, A, b)

Returns the solution to `A*x = b` or one of the other two variants determined by `tA`
(transpose `A`) and `ul` (triangle of `A` is used.) `dA` indicates if `A` is unit-triangular
(the diagonal is assumed to be all ones).
"""
function trsv end

for (fname, elty) in ((:dtrsv_,:Float64),
                        (:strsv_,:Float32),
                        (:ztrsv_,:Complex128),
                        (:ctrsv_,:Complex64))
    @eval begin
                #       SUBROUTINE DTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
                #       .. Scalar Arguments ..
                #       INTEGER INCX,LDA,N
                #       CHARACTER DIAG,TRANS,UPLO
                #       .. Array Arguments ..
                #       DOUBLE PRECISION A(LDA,*),X(*)
        function trsv!(uplo::Char, trans::Char, diag::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = checksquare(A)
            if n != length(x)
                throw(DimensionMismatch("size of A is $n != length(x) = $(length(x))"))
            end
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &diag, &n,
                 A, &max(1,stride(A,2)), x, &1)
            x
        end
        function trsv(uplo::Char, trans::Char, diag::Char, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            trsv!(uplo, trans, diag, A, copy(x))
        end
    end
end

### ger

"""
    ger!(alpha, x, y, A)

Rank-1 update of the matrix `A` with vectors `x` and `y` as `alpha*x*y' + A`.
"""
function ger! end

for (fname, elty) in ((:dger_,:Float64),
                      (:sger_,:Float32),
                      (:zgerc_,:Complex128),
                      (:cgerc_,:Complex64))
    @eval begin
        function ger!(α::$elty, x::StridedVector{$elty}, y::StridedVector{$elty}, A::StridedMatrix{$elty})
            m, n = size(A)
            if m != length(x) || n != length(y)
                throw(DimensionMismatch("A has size ($m,$n), x has length $(length(x)), y has length $(length(y))"))
            end
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}),
                 &m, &n, &α, x,
                 &1, y, &1, A,
                 &max(1,stride(A,2)))
            A
        end
    end
end

### syr

"""
    syr!(uplo, alpha, x, A)

Rank-1 update of the symmetric matrix `A` with vector `x` as `alpha*x*x.' + A`. When `uplo`
is 'U' the upper triangle of `A` is updated ('L' for lower triangle). Returns `A`.
"""
function syr! end

for (fname, elty, lib) in ((:dsyr_,:Float64,libblas),
                           (:ssyr_,:Float32,libblas),
                           (:zsyr_,:Complex128,liblapack),
                           (:csyr_,:Complex64,liblapack))
    @eval begin
        function syr!(uplo::Char, α::$elty, x::StridedVector{$elty}, A::StridedMatrix{$elty})
            n = checksquare(A)
            if length(x) != n
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            ccall((@blasfunc($fname), $lib), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &α, x,
                 &1, A, &max(1,stride(A,2)))
            A
        end
    end
end

### her

"""
    her!(uplo, alpha, x, A)

Methods for complex arrays only. Rank-1 update of the Hermitian matrix `A` with vector `x`
as `alpha*x*x' + A`. When `uplo` is 'U' the upper triangle of `A` is updated ('L' for lower
triangle). Returns `A`.
"""
function her! end

for (fname, elty, relty) in ((:zher_,:Complex128, :Float64),
                             (:cher_,:Complex64, :Float32))
    @eval begin
        function her!(uplo::Char, α::$relty, x::StridedVector{$elty}, A::StridedMatrix{$elty})
            n = checksquare(A)
            if length(x) != n
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &α, x,
                 &1, A, &max(1,stride(A,2)))
            A
        end
    end
end

# Level 3
## (GE) general matrix-matrix multiplication

"""
    gemm!(tA, tB, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or the other three variants according to `tA` (transpose
`A`) and `tB`. Returns the updated `C`.
"""
function gemm! end

for (gemm, elty) in
        ((:dgemm_,:Float64),
         (:sgemm_,:Float32),
         (:zgemm_,:Complex128),
         (:cgemm_,:Complex64))
    @eval begin
             # SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER K,LDA,LDB,LDC,M,N
             #       CHARACTER TRANSA,TRANSB
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function gemm!(transA::Char, transB::Char, alpha::($elty), A::StridedVecOrMat{$elty}, B::StridedVecOrMat{$elty}, beta::($elty), C::StridedVecOrMat{$elty})
#           if any([stride(A,1), stride(B,1), stride(C,1)] .!= 1)
#               error("gemm!: BLAS module requires contiguous matrix columns")
#           end  # should this be checked on every call?
            m = size(A, transA == 'N' ? 1 : 2)
            k = size(A, transA == 'N' ? 2 : 1)
            n = size(B, transB == 'N' ? 2 : 1)
            if m != size(C,1) || n != size(C,2)
                throw(DimensionMismatch("A has size ($m,$k), B has size ($k,$n), C has size $(size(C))"))
            end
            ccall((@blasfunc($gemm), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}),
                 &transA, &transB, &m, &n,
                 &k, &alpha, A, &max(1,stride(A,2)),
                 B, &max(1,stride(B,2)), &beta, C,
                 &max(1,stride(C,2)))
            C
        end
        function gemm(transA::Char, transB::Char, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            gemm!(transA, transB, alpha, A, B, zero($elty), similar(B, $elty, (size(A, transA == 'N' ? 1 : 2), size(B, transB == 'N' ? 2 : 1))))
        end
        function gemm(transA::Char, transB::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            gemm(transA, transB, one($elty), A, B)
        end
    end
end

"""
    gemm(tA, tB, alpha, A, B)

Returns `alpha*A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
gemm(tA, tB, alpha, A, B)

"""
    gemm(tA, tB, A, B)

Returns `A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
gemm(tA, tB, A, B)


## (SY) symmetric matrix-matrix and matrix-vector multiplication
for (mfname, elty) in ((:dsymm_,:Float64),
                       (:ssymm_,:Float32),
                       (:zsymm_,:Complex128),
                       (:csymm_,:Complex64))
    @eval begin
             #     SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #     .. Scalar Arguments ..
             #     DOUBLE PRECISION ALPHA,BETA
             #     INTEGER LDA,LDB,LDC,M,N
             #     CHARACTER SIDE,UPLO
             #     .. Array Arguments ..
             #     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function symm!(side::Char, uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty}, beta::($elty), C::StridedMatrix{$elty})
            m, n = size(C)
            j = checksquare(A)
            if j != (side == 'L' ? m : n)
                throw(DimensionMismatch("A has size $(size(A)), C has size ($m,$n)"))
            end
            if size(B,2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)) but needs to match second dimension of C, $n"))
            end
            ccall((@blasfunc($mfname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &side, &uplo, &m, &n,
                 &alpha, A, &max(1,stride(A,2)), B,
                 &max(1,stride(B,2)), &beta, C, &max(1,stride(C,2)))
            C
        end
        function symm(side::Char, uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            symm!(side, uplo, alpha, A, B, zero($elty), similar(B))
        end
        function symm(side::Char, uplo::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            symm(side, uplo, one($elty), A, B)
        end
    end
end

"""
    symm(side, ul, alpha, A, B)

Returns `alpha*A*B` or `alpha*B*A` according to `side`. `A` is assumed to be symmetric. Only
the `ul` triangle of `A` is used.
"""
symm(side, ul, alpha, A, B)

"""
    symm(side, ul, A, B)

Returns `A*B` or `B*A` according to `side`. `A` is assumed to be symmetric. Only the `ul`
triangle of `A` is used.
"""
symm(side, ul, A, B)

"""
    symm(tA, tB, alpha, A, B)

Returns `alpha*A*B` or the other three variants according to `tA` (transpose `A`) and `tB`.
"""
symm(tA::Char, tB::Char, alpha, A, B)

"""
    symm!(side, ul, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or `alpha*B*A + beta*C` according to `side`. `A` is
assumed to be symmetric. Only the `ul` triangle of `A` is used. Returns the updated `C`.
"""
symm!

## (HE) Hermitian matrix-matrix and matrix-vector multiplication
for (mfname, elty) in ((:zhemm_,:Complex128),
                       (:chemm_,:Complex64))
    @eval begin
             #     SUBROUTINE DHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #     .. Scalar Arguments ..
             #     DOUBLE PRECISION ALPHA,BETA
             #     INTEGER LDA,LDB,LDC,M,N
             #     CHARACTER SIDE,UPLO
             #     .. Array Arguments ..
             #     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function hemm!(side::Char, uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty}, beta::($elty), C::StridedMatrix{$elty})
            m, n = size(C)
            j = checksquare(A)
            if j != (side == 'L' ? m : n)
                throw(DimensionMismatch("A has size $(size(A)), C has size ($m,$n)"))
            end
            if size(B,2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)) but needs to match second dimension of C, $n"))
            end
            ccall((@blasfunc($mfname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &side, &uplo, &m, &n,
                 &alpha, A, &max(1,stride(A,2)), B,
                 &max(1,stride(B,2)), &beta, C, &max(1,stride(C,2)))
            C
        end
        function hemm(side::Char, uplo::Char, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            hemm!(side, uplo, alpha, A, B, zero($elty), similar(B))
        end
        function hemm(side::Char, uplo::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            hemm(side, uplo, one($elty), A, B)
        end
    end
end

## syrk

"""
    syrk!(uplo, trans, alpha, A, beta, C)

Rank-k update of the symmetric matrix `C` as `alpha*A*A.' + beta*C` or `alpha*A.'*A +
beta*C` according to whether `trans` is 'N' or 'T'. When `uplo` is 'U' the upper triangle of
`C` is updated ('L' for lower triangle). Returns `C`.
"""
function syrk! end

"""
    syrk(uplo, trans, alpha, A)

Returns either the upper triangle or the lower triangle, according to `uplo` ('U' or 'L'),
of `alpha*A*A.'` or `alpha*A.'*A`, according to `trans` ('N' or 'T').
"""
function syrk end

for (fname, elty) in ((:dsyrk_,:Float64),
                      (:ssyrk_,:Float32),
                      (:zsyrk_,:Complex128),
                      (:csyrk_,:Complex64))
   @eval begin
       # SUBROUTINE DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       REAL ALPHA,BETA
       #       INTEGER K,LDA,LDC,N
       #       CHARACTER TRANS,UPLO
       # *     .. Array Arguments ..
       #       REAL A(LDA,*),C(LDC,*)
       function syrk!(uplo::Char, trans::Char,
                      alpha::($elty), A::StridedVecOrMat{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall((@blasfunc($fname), libblas), Void,
                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                  Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &max(1,stride(A,2)), &beta,
                 C, &max(1,stride(C,2)))
            C
        end
    end
end
function syrk(uplo::Char, trans::Char, alpha::Number, A::StridedVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syrk!(uplo, trans, convert(T,alpha), A, zero(T), similar(A, T, (n, n)))
end
syrk(uplo::Char, trans::Char, A::StridedVecOrMat) = syrk(uplo, trans, one(eltype(A)), A)

"""
    herk!(uplo, trans, alpha, A, beta, C)

Methods for complex arrays only. Rank-k update of the Hermitian matrix `C` as `alpha*A*A' +
beta*C` or `alpha*A'*A + beta*C` according to whether `trans` is 'N' or 'T'. When `uplo` is
'U' the upper triangle of `C` is updated ('L' for lower triangle). Returns `C`.
"""
function herk! end

"""
    herk(uplo, trans, alpha, A)

Methods for complex arrays only. Returns either the upper triangle or the lower triangle,
according to `uplo` ('U' or 'L'), of `alpha*A*A'` or `alpha*A'*A`, according to `trans` ('N'
or 'T').
"""
function herk end

for (fname, elty, relty) in ((:zherk_, :Complex128, :Float64),
                             (:cherk_, :Complex64, :Float32))
   @eval begin
       # SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       REAL ALPHA,BETA
       #       INTEGER K,LDA,LDC,N
       #       CHARACTER TRANS,UPLO
       # *     ..
       # *     .. Array Arguments ..
       #       COMPLEX A(LDA,*),C(LDC,*)
       function herk!(uplo::Char, trans::Char, α::$relty, A::StridedVecOrMat{$elty},
                      β::$relty, C::StridedMatrix{$elty})
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n
               throw(DimensionMismatch("the matrix to update has dimension $n but the implied dimension of the update is $(size(A, trans == 'N' ? 1 : 2))"))
           end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall((@blasfunc($fname), libblas), Void,
                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                  Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &α, A, &max(1,stride(A,2)), &β,
                 C, &max(1,stride(C,2)))
           C
       end
       function herk(uplo::Char, trans::Char, α::$relty, A::StridedVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           herk!(uplo, trans, α, A, zero($relty), similar(A, (n,n)))
       end
       herk(uplo::Char, trans::Char, A::StridedVecOrMat{$elty}) = herk(uplo, trans, one($relty), A)
   end
end

## syr2k
for (fname, elty) in ((:dsyr2k_,:Float64),
                      (:ssyr2k_,:Float32),
                      (:zsyr2k_,:Complex128),
                      (:csyr2k_,:Complex64))
    @eval begin
             #       SUBROUTINE DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #
             #       .. Scalar Arguments ..
             #       REAL PRECISION ALPHA,BETA
             #       INTEGER K,LDA,LDB,LDC,N
             #       CHARACTER TRANS,UPLO
             #       ..
             #       .. Array Arguments ..
             #       REAL PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function syr2k!(uplo::Char, trans::Char,
                        alpha::($elty), A::StridedVecOrMat{$elty}, B::StridedVecOrMat{$elty},
                        beta::($elty), C::StridedMatrix{$elty})
            n = checksquare(C)
            nn = size(A, trans == 'N' ? 1 : 2)
            if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
            k  = size(A, trans == 'N' ? 2 : 1)
            ccall((@blasfunc($fname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), &beta,
                 C, &max(1,stride(C,2)))
            C
        end
    end
end
function syr2k(uplo::Char, trans::Char, alpha::Number, A::StridedVecOrMat, B::StridedVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syr2k!(uplo, trans, convert(T,alpha), A, B, zero(T), similar(A, T, (n, n)))
end
syr2k(uplo::Char, trans::Char, A::StridedVecOrMat, B::StridedVecOrMat) = syr2k(uplo, trans, one(eltype(A)), A, B)

for (fname, elty1, elty2) in ((:zher2k_,:Complex128,:Float64), (:cher2k_,:Complex64,:Float32))
   @eval begin
       # SUBROUTINE CHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
       #
       #       .. Scalar Arguments ..
       #       COMPLEX ALPHA
       #       REAL BETA
       #       INTEGER K,LDA,LDB,LDC,N
       #       CHARACTER TRANS,UPLO
       #       ..
       #       .. Array Arguments ..
       #       COMPLEX A(LDA,*),B(LDB,*),C(LDC,*)
       function her2k!(uplo::Char, trans::Char, alpha::($elty1),
                       A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1},
                       beta::($elty2), C::StridedMatrix{$elty1})
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall((@blasfunc($fname), libblas), Void,
                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty1}, Ptr{$elty1}, Ptr{BlasInt}, Ptr{$elty1}, Ptr{BlasInt},
                  Ptr{$elty2},  Ptr{$elty1}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)),
                 &beta, C, &max(1,stride(C,2)))
           C
       end
       function her2k(uplo::Char, trans::Char, alpha::($elty1), A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1})
           n = size(A, trans == 'N' ? 1 : 2)
           her2k!(uplo, trans, alpha, A, B, zero($elty2), similar(A, $elty1, (n,n)))
       end
       her2k(uplo::Char, trans::Char, A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1}) = her2k(uplo, trans, one($elty1), A, B)
   end
end

## (TR) Triangular matrix and vector multiplication and solution

"""
    trmm!(side, ul, tA, dA, alpha, A, B)

Update `B` as `alpha*A*B` or one of the other three variants determined by `side` (`A` on
left or right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA`
indicates if `A` is unit-triangular (the diagonal is assumed to be all ones). Returns the
updated `B`.
"""
function trmm! end

"""
    trmm(side, ul, tA, dA, alpha, A, B)

Returns `alpha*A*B` or one of the other three variants determined by `side` (`A` on left or
right) and `tA` (transpose `A`). Only the `ul` triangle of `A` is used. `dA` indicates if
`A` is unit-triangular (the diagonal is assumed to be all ones).
"""
function trmm end

"""
    trsm!(side, ul, tA, dA, alpha, A, B)

Overwrite `B` with the solution to `A*X = alpha*B` or one of the other three variants
determined by `side` (`A` on left or right of `X`) and `tA` (transpose `A`). Only the `ul`
triangle of `A` is used. `dA` indicates if `A` is unit-triangular (the diagonal is assumed
to be all ones). Returns the updated `B`.
"""
function trsm! end

"""
    trsm(side, ul, tA, dA, alpha, A, B)

Returns the solution to `A*X = alpha*B` or one of the other three variants determined by
`side` (`A` on left or right of `X`) and `tA` (transpose `A`). Only the `ul` triangle of `A`
is used. `dA` indicates if `A` is unit-triangular (the diagonal is assumed to be all ones).
"""
function trsm end

for (mmname, smname, elty) in
        ((:dtrmm_,:dtrsm_,:Float64),
         (:strmm_,:strsm_,:Float32),
         (:ztrmm_,:ztrsm_,:Complex128),
         (:ctrmm_,:ctrsm_,:Complex64))
    @eval begin
        #       SUBROUTINE DTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA
        #       INTEGER LDA,LDB,M,N
        #       CHARACTER DIAG,SIDE,TRANSA,UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),B(LDB,*)
        function trmm!(side::Char, uplo::Char, transa::Char, diag::Char, alpha::Number,
                       A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            m, n = size(B)
            nA = checksquare(A)
            if nA != (side == 'L' ? m : n)
                throw(DimensionMismatch("size of A, $(size(A)), doesn't match $side size of B with dims, $(size(B))"))
            end
            ccall((@blasfunc($mmname), libblas), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &side, &uplo, &transa, &diag, &m, &n,
                  &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)))
            B
        end
        function trmm(side::Char, uplo::Char, transa::Char, diag::Char,
                      alpha::$elty, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            trmm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
        #       SUBROUTINE DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA
        #       INTEGER LDA,LDB,M,N
        #       CHARACTER DIAG,SIDE,TRANSA,UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),B(LDB,*)
        function trsm!(side::Char, uplo::Char, transa::Char, diag::Char,
                       alpha::$elty, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            m, n = size(B)
            k = checksquare(A)
            if k != (side == 'L' ? m : n)
                throw(DimensionMismatch("size of A is ($k,$k), size of B is ($m,$n), side is $side, and transa='$transa'"))
            end
            ccall((@blasfunc($smname), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8},
                 Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &side, &uplo, &transa, &diag,
                 &m, &n, &alpha, A,
                 &max(1,stride(A,2)), B, &max(1,stride(B,2)))
            B
        end
        function trsm(side::Char, uplo::Char, transa::Char, diag::Char, alpha::$elty, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            trsm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
    end
end

end # module

function copy!{T<:BlasFloat,Ti<:Integer}(dest::Array{T}, rdest::Union{UnitRange{Ti},Range{Ti}},
                                          src::Array{T}, rsrc::Union{UnitRange{Ti},Range{Ti}})
    if minimum(rdest) < 1 || maximum(rdest) > length(dest)
        throw(ArgumentError("range out of bounds for dest, of length $(length(dest))"))
    end
    if minimum(rsrc) < 1 || maximum(rsrc) > length(src)
        throw(ArgumentError("range out of bounds for src, of length $(length(src))"))
    end
    if length(rdest) != length(rsrc)
        throw(DimensionMismatch("ranges must be of the same length"))
    end
    BLAS.blascopy!(length(rsrc), pointer(src)+(first(rsrc)-1)*sizeof(T), step(rsrc),
                   pointer(dest)+(first(rdest)-1)*sizeof(T), step(rdest))
    dest
end
