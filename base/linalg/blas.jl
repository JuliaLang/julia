module BLAS

import ..axpy!
import Base: copy!, blasfunc

export
# Level 1
    asum,
    axpy!,
    dot,
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
    syrk!,
    syrk,
    syr2k!,
    syr2k,
    trmm!,
    trmm,
    trsm!,
    trsm


const libblas = Base.libblas_name

import ..LinAlg: BlasReal, BlasComplex, BlasFloat, BlasChar, BlasInt, blas_int, DimensionMismatch, chksquare, axpy!

# Level 1
## copy
for (fname, elty) in ((:dcopy_,:Float64),
                      (:scopy_,:Float32),
                      (:zcopy_,:Complex128),
                      (:ccopy_,:Complex64))
    @eval begin
        # SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
        function blascopy!(n::Integer, DX::Union(Ptr{$elty},StridedArray{$elty}), incx::Integer, DY::Union(Ptr{$elty},StridedArray{$elty}), incy::Integer)
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, DX, &incx, DY, &incy)
            DY
        end
    end
end

## scal
for (fname, elty) in ((:dscal_,:Float64),
                      (:sscal_,:Float32),
                      (:zscal_,:Complex128),
                      (:cscal_,:Complex64))
    @eval begin
        # SUBROUTINE DSCAL(N,DA,DX,INCX)
        function scal!(n::Integer, DA::$elty, DX::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer)
            ccall(($(blasfunc(fname)), libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, &DA, DX, &incx)
            DX
        end
    end
end
scal(n, DA, DX, incx) = scal!(n, DA, copy(DX), incx)

## dot
for (fname, elty) in ((:ddot_,:Float64),
                      (:sdot_,:Float32))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dot(n::Integer, DX::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer, DY::Union(Ptr{$elty},DenseArray{$elty}), incy::Integer)
            ccall(($(blasfunc(fname)), libblas), $elty,
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
        function dotc(n::Integer, DX::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer, DY::Union(Ptr{$elty},DenseArray{$elty}), incy::Integer)
            result = Array($elty, 1)
            ccall(($(blasfunc(fname)), libblas), $elty,
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
        function dotu(n::Integer, DX::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer, DY::Union(Ptr{$elty},DenseArray{$elty}), incy::Integer)
            result = Array($elty, 1)
            ccall(($(blasfunc(fname)), libblas), $elty,
                (BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}),
                 n, DX, incx, DY, incy, result)
            result[1]
        end
    end
end
function dot{T<:BlasReal}(DX::Union(DenseArray{T},StridedVector{T}), DY::Union(DenseArray{T},StridedVector{T}))
    n = length(DX)
    n == length(DY) || throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    dot(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end
function dotc{T<:BlasComplex}(DX::Union(DenseArray{T},StridedVector{T}), DY::Union(DenseArray{T},StridedVector{T}))
    n = length(DX)
    n == length(DY) || throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    dotc(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end
function dotu{T<:BlasComplex}(DX::Union(DenseArray{T},StridedVector{T}), DY::Union(DenseArray{T},StridedVector{T}))
    n = length(DX)
    n == length(DY) || throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    dotu(n, pointer(DX), stride(DX, 1), pointer(DY), stride(DY, 1))
end

## nrm2
for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:Complex128,:Float64),
                                (:scnrm2_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE DNRM2(N,X,INCX)
        function nrm2(n::Integer, X::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer)
            ccall(($(blasfunc(fname)), libblas), $ret_type,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, X, &incx)
        end
    end
end
nrm2(x::StridedVector) = nrm2(length(x), pointer(x), stride(x,1))
nrm2(x::Array) = nrm2(length(x), pointer(x), 1)

## asum
for (fname, elty, ret_type) in ((:dasum_,:Float64,:Float64),
                                (:sasum_,:Float32,:Float32),
                                (:dzasum_,:Complex128,:Float64),
                                (:scasum_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE ASUM(N, X, INCX)
        function asum(n::Integer, X::Union(Ptr{$elty},DenseArray{$elty}), incx::Integer)
            ccall(($(blasfunc(fname)), libblas), $ret_type,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, X, &incx)
        end
    end
end
asum(x::StridedVector) = asum(length(x), pointer(x), stride(x,1))
asum(x::Array) = asum(length(x), pointer(x), 1)

## axpy
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
        function axpy!(n::Integer, alpha::($elty), dx::Union(Ptr{$elty}, DenseArray{$elty}), incx::Integer, dy::Union(Ptr{$elty}, DenseArray{$elty}), incy::Integer)
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &n, &alpha, dx, &incx, dy, &incy)
            dy
        end
    end
end
function axpy!{T<:BlasFloat,Ta<:Number}(alpha::Ta, x::Union(DenseArray{T},StridedVector{T}), y::Union(DenseArray{T},StridedVector{T}))
    length(x) == length(y) || throw(DimensionMismatch("x has length $length(x), but y has length $length(y)"))
    axpy!(length(x), convert(T,alpha), pointer(x), stride(x, 1), pointer(y), stride(y, 1))
    y
end

function axpy!{T<:BlasFloat,Ta<:Number,Ti<:Integer}(alpha::Ta, x::Array{T}, rx::Union(UnitRange{Ti},Range{Ti}),
                                         y::Array{T}, ry::Union(UnitRange{Ti},Range{Ti}))

    length(rx)==length(ry) || throw(DimensionMismatch())

    if minimum(rx) < 1 || maximum(rx) > length(x) || minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError())
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
        function iamax(n::Integer, dx::Union(Ptr{$elty}, DenseArray{$elty}), incx::Integer)
            ccall(($(blasfunc(fname)), libblas),BlasInt,
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
        function gemv!(trans::BlasChar, alpha::($elty), A::StridedVecOrMat{$elty}, X::StridedVector{$elty}, beta::($elty), Y::StridedVector{$elty})
            m,n = size(A,1),size(A,2)
            length(X) == (trans == 'N' ? n : m) && length(Y) == (trans == 'N' ? m : n) || throw(DimensionMismatch())
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &trans, &size(A,1), &size(A,2), &alpha,
                 A, &max(1,stride(A,2)), X, &stride(X,1),
                 &beta, Y, &stride(Y,1))
            Y
        end
        function gemv(trans::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, X::StridedVector{$elty})
            gemv!(trans, alpha, A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
        function gemv(trans::BlasChar, A::StridedMatrix{$elty}, X::StridedVector{$elty})
            gemv!(trans, one($elty), A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
    end
end

### (GB) general banded matrix-vector multiplication
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
        function gbmv!(trans::BlasChar, m::Integer, kl::Integer, ku::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, beta::($elty), y::StridedVector{$elty})
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}),
                 &trans, &m, &size(A,2), &kl,
                 &ku, &alpha, A, &max(1,stride(A,2)),
                 x, &stride(x,1), &beta, y, &stride(y,1))
            y
        end
        function gbmv(trans::BlasChar, m::Integer, kl::Integer, ku::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = size(A,2)
            leny = trans == 'N' ? m : n
            gbmv!(trans, m, kl, ku, alpha, A, x, zero($elty), similar(x, $elty, leny))
        end
        function gbmv(trans::BlasChar, m::Integer, kl::Integer, ku::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            gbmv(trans, m, kl, ku, one($elty), A, x)
        end
    end
end

### symv
for (fname, elty) in ((:dsymv_,:Float64),
                      (:ssymv_,:Float32),
                      (:zsymv_,:Complex128),
                      (:csymv_,:Complex64))
    # Note that the complex symv are not BLAS but auiliary functions in LAPACK
    @eval begin
             #      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             #     .. Scalar Arguments ..
             #      DOUBLE PRECISION ALPHA,BETA
             #      INTEGER INCX,INCY,LDA,N
             #      CHARACTER UPLO
             #     .. Array Arguments ..
             #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function symv!(uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty},beta::($elty), y::StridedVector{$elty})
            m, n = size(A)
            if m != n throw(DimensionMismatch("Matrix A is $m by $n but must be square")) end
            if m != length(x) || m != length(y) throw(DimensionMismatch()) end
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &alpha, A,
                 &max(1,stride(A,2)), x, &stride(x,1), &beta,
                 y, &stride(y,1))
            y
        end
        function symv(uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
                symv!(uplo, alpha, A, x, zero($elty), similar(x))
        end
        function symv(uplo::BlasChar, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            symv(uplo, one($elty), A, x)
        end
    end
end

### hemv
for (fname, elty) in ((:zhemv_,:Complex128),
                      (:chemv_,:Complex64))
    @eval begin
        function hemv!(uplo::Char, α::$elty, A::StridedMatrix{$elty}, x::StridedVector{$elty}, β::$elty, y::StridedVector{$elty})
            n = size(A, 2)
            n == length(x) || throw(DimensionMismatch())
            size(A, 1) == length(y) || throw(DimensionMismatch())
            lda = max(1, stride(A, 2))
            incx = stride(x, 1)
            incy = stride(y, 1)
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}),
                &uplo, &n, &α, A,
                &lda, x, &incx, &β,
                y, &incy)
            y
        end
        function hemv(uplo::BlasChar, α::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            hemv!(uplo, α, A, x, zero($elty), similar(x))
        end
        function hemv(uplo::BlasChar, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            hemv(uplo, one($elty), A, x)
        end
    end
end

### sbmv, (SB) symmetric banded matrix-vector multiplication
for (fname, elty) in ((:dsbmv_,:Float64),
                      (:ssbmv_,:Float32),
                      (:zsbmv_,:Complex128),
                      (:csbmv_,:Complex64))
    @eval begin
             #       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,K,LDA,N
             #       CHARACTER UPLO
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function sbmv!(uplo::BlasChar, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, beta::($elty), y::StridedVector{$elty})
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &size(A,2), &k, &alpha,
                 A, &max(1,stride(A,2)), x, &stride(x,1),
                 &beta, y, &stride(y,1))
            y
        end
        function sbmv(uplo::BlasChar, k::Integer, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n = size(A,2)
            sbmv!(uplo, k, alpha, A, x, zero($elty), similar(x, $elty, n))
        end
        function sbmv(uplo::BlasChar, k::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
            sbmv(uplo, k, one($elty), A, x)
        end
    end
end

### trmv, Triangular matrix-vector multiplication
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
            n = chksquare(A)
            if n != length(x)
                throw(DimensionMismatch("length(x)=$(length(x))does not match size(A)=$(size(A))"))
            end
            ccall(($(blasfunc(fname)), libblas), Void,
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
            n = chksquare(A)
            n==length(x) || throw(DimensionMismatch("size of A is $n != length(x) = $(length(x))"))
            ccall(($(blasfunc(fname)), libblas), Void,
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
for (fname, elty) in ((:dger_,:Float64),
                      (:sger_,:Float32),
                      (:zgerc_,:Complex128),
                      (:cgerc_,:Complex64))
    @eval begin
        function ger!(α::$elty, x::StridedVector{$elty}, y::StridedVector{$elty}, A::StridedMatrix{$elty})
            m, n = size(A)
            m == length(x) || throw(DimensionMismatch())
            n == length(y) || throw(DimensionMismatch())
            ccall(($(blasfunc(fname)), libblas), Void,
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
for (fname, elty) in ((:dsyr_,:Float64),
                      (:ssyr_,:Float32),
                      (:zsyr_,:Complex128),
                      (:csyr_,:Complex64))
    @eval begin
        function syr!(uplo::Char, α::$elty, x::StridedVector{$elty}, A::StridedMatrix{$elty})
            n = chksquare(A)
            length(x) == n || throw(DimensionMismatch("Length of vector must be the same as the matrix dimensions"))
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &α, x,
                 &1, A, &max(1,stride(A,2)))
            A
        end
    end
end

### her
for (fname, elty) in ((:zher_,:Complex128),
                      (:cher_,:Complex64))
    @eval begin
        function her!(uplo::Char, α::$elty, x::StridedVector{$elty}, A::StridedMatrix{$elty})
            n = chksquare(A)
            length(x) == A || throw(DimensionMismatch("Length of vector must be the same as the matrix dimensions"))
            ccall(($(blasfunc(fname)), libblas), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &α, x,
                 &1, A, &max(1,stride(A,2)))
            A
        end
    end
end

# Level 3
## (GE) general matrix-matrix multiplication
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
        function gemm!(transA::BlasChar, transB::BlasChar, alpha::($elty), A::StridedVecOrMat{$elty}, B::StridedVecOrMat{$elty}, beta::($elty), C::StridedVecOrMat{$elty})
#           if any([stride(A,1), stride(B,1), stride(C,1)] .!= 1)
#               error("gemm!: BLAS module requires contiguous matrix columns")
#           end  # should this be checked on every call?
            m = size(A, transA == 'N' ? 1 : 2)
            k = size(A, transA == 'N' ? 2 : 1)
            n = size(B, transB == 'N' ? 2 : 1)
            if m != size(C,1) || n != size(C,2)
                throw(DimensionMismatch())
            end
            ccall(($(blasfunc(gemm)), libblas), Void,
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
        function gemm(transA::BlasChar, transB::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            gemm!(transA, transB, alpha, A, B, zero($elty), similar(B, $elty, (size(A, transA == 'N' ? 1 : 2), size(B, transB == 'N' ? 2 : 1))))
        end
        function gemm(transA::BlasChar, transB::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            gemm(transA, transB, one($elty), A, B)
        end
    end
end

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
        function symm!(side::BlasChar, uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty}, beta::($elty), C::StridedMatrix{$elty})
            m, n = size(C)
            j = chksquare(A)
            if j != (side == 'L' ? m : n) || size(B,2) != n throw(DimensionMismatch()) end
            ccall(($(blasfunc(mfname)), libblas), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &side, &uplo, &m, &n,
                 &alpha, A, &max(1,stride(A,2)), B,
                 &max(1,stride(B,2)), &beta, C, &max(1,stride(C,2)))
            C
        end
        function symm(side::BlasChar, uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            symm!(side, uplo, alpha, A, B, zero($elty), similar(B))
        end
        function symm(side::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            symm(side, uplo, one($elty), A, B)
        end
    end
end

## syrk
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
       function syrk!(uplo::BlasChar, trans::BlasChar,
                      alpha::($elty), A::StridedVecOrMat{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           n = chksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n throw(DimensionMismatch("syrk!")) end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(blasfunc(fname)), libblas), Void,
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
function syrk(uplo::BlasChar, trans::BlasChar, alpha::Number, A::StridedVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syrk!(uplo, trans, convert(T,alpha), A, zero(T), similar(A, T, (n, n)))
end
syrk(uplo::BlasChar, trans::BlasChar, A::StridedVecOrMat) = syrk(uplo, trans, one(eltype(A)), A)

for (fname, elty) in ((:zherk_,:Complex128), (:cherk_,:Complex64))
   @eval begin
       # SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       REAL ALPHA,BETA
       #       INTEGER K,LDA,LDC,N
       #       CHARACTER TRANS,UPLO
       # *     ..
       # *     .. Array Arguments ..
       #       COMPLEX A(LDA,*),C(LDC,*)
       function herk!(uplo::BlasChar, trans::BlasChar, alpha::($elty), A::StridedVecOrMat{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           n = chksquare(C)
           n == size(A, trans == 'N' ? 1 : 2) || throw(DimensionMismatch("herk!"))
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(blasfunc(fname)), libblas), Void,
                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                  Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &max(1,stride(A,2)), &beta,
                 C, &max(1,stride(C,2)))
           C
       end
       function herk(uplo::BlasChar, trans::BlasChar, alpha::($elty), A::StridedVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           herk!(uplo, trans, alpha, A, zero($elty), similar(A, $elty, (n,n)))
       end
       herk(uplo::BlasChar, trans::BlasChar, A::StridedVecOrMat{$elty}) = herk(uplo, trans, one($elty), A)
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
        function syr2k!(uplo::BlasChar, trans::BlasChar,
                        alpha::($elty), A::StridedVecOrMat{$elty}, B::StridedVecOrMat{$elty},
                        beta::($elty), C::StridedMatrix{$elty})
            n = chksquare(C)
            nn = size(A, trans == 'N' ? 1 : 2)
            if nn != n throw(DimensionMismatch("syr2k!")) end
            k  = size(A, trans == 'N' ? 2 : 1)
            ccall(($(blasfunc(fname)), libblas), Void,
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
function syr2k(uplo::BlasChar, trans::BlasChar, alpha::Number, A::StridedVecOrMat, B::StridedVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syr2k!(uplo, trans, convert(T,alpha), A, B, zero(T), similar(A, T, (n, n)))
end
syr2k(uplo::BlasChar, trans::BlasChar, A::StridedVecOrMat, B::StridedVecOrMat) = syr2k(uplo, trans, one(eltype(A)), A, B)

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
       function her2k!(uplo::BlasChar, trans::BlasChar, alpha::($elty1),
                       A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1},
                       beta::($elty2), C::StridedMatrix{$elty1})
           n = chksquare(C)
           n == size(A, trans == 'N' ? 1 : 2) || throw(DimensionMismatch("her2k!"))
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(blasfunc(fname)), libblas), Void,
                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty1}, Ptr{$elty1}, Ptr{BlasInt}, Ptr{$elty1}, Ptr{BlasInt},
                  Ptr{$elty2},  Ptr{$elty1}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)),
                 &beta, C, &max(1,stride(C,2)))
           C
       end
       function her2k(uplo::BlasChar, trans::BlasChar, alpha::($elty1), A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1})
           n = size(A, trans == 'N' ? 1 : 2)
           her2k!(uplo, trans, alpha, A, B, zero($elty2), similar(A, $elty1, (n,n)))
       end
       her2k(uplo::BlasChar, trans::BlasChar, A::StridedVecOrMat{$elty1}, B::StridedVecOrMat{$elty1}) = her2k(uplo, trans, one($elty1), A, B)
   end
end

## (TR) Triangular matrix and vector multiplication and solution
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
            nA = chksquare(A)
            if nA != (side == 'L' ? m : n) throw(DimensionMismatch("trmm!")) end
            ccall(($(blasfunc(mmname)), libblas), Void,
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
            k = chksquare(A)
            k==(side == 'L' ? m : n) || throw(DimensionMismatch("size of A is $n, size(B)=($m,$n) and transa='$transa'"))
            ccall(($(blasfunc(smname)), libblas), Void,
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

function copy!{T<:BlasFloat,Ti<:Integer}(dest::Array{T}, rdest::Union(UnitRange{Ti},Range{Ti}),
                                          src::Array{T}, rsrc::Union(UnitRange{Ti},Range{Ti}))
    if minimum(rdest) < 1 || maximum(rdest) > length(dest) || minimum(rsrc) < 1 || maximum(rsrc) > length(src)
        throw(BoundsError())
    end
    length(rdest)==length(rsrc) || throw(DimensionMismatch("Ranges must be of the same length"))
    BLAS.blascopy!(length(rsrc), pointer(src)+(first(rsrc)-1)*sizeof(T), step(rsrc),
                   pointer(dest)+(first(rdest)-1)*sizeof(T), step(rdest))
    dest
end
