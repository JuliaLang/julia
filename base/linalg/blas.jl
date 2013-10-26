module BLAS

import Base.copy!

export
       scal!,
       scal,
       dot,
       asum,
       nrm2,
       axpy!,
       syrk!,
       syrk,
       herk!,
       herk,
       gbmv!,
       gbmv,
       sbmv!,
       sbmv,
       gemm!,
       gemm,
       symm!,
       symm,
       symv!,
       symv

const libblas = Base.libblas_name

import ..LinAlg: BlasFloat, BlasChar, BlasInt, blas_int, DimensionMismatch

for (fname, elty) in ((:dcopy_,:Float64), (:scopy_,:Float32),
                      (:zcopy_,:Complex128), (:ccopy_,:Complex64))
    @eval begin
        # SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
        function blascopy!(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer,
                       DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, DX, &incx, DY, &incy)
            DY
        end
    end
end

for (fname, elty) in ((:dscal_,:Float64),    (:sscal_,:Float32),
                      (:zscal_,:Complex128), (:cscal_,:Complex64))
    @eval begin
        # SUBROUTINE DSCAL(N,DA,DX,INCX)
        function scal!(n::Integer, DA::$elty, 
                       DX::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, &DA, DX, &incx)
            return DX
        end
    end
end
scal(n, DA, DX, incx) = scal!(n, DA, copy(DX), incx)

# In case DX is complex, and DA is real, use dscal/sscal to save flops
for (fname, elty, celty) in ((:sscal_, :Float32, :Complex64),
                             (:dscal_, :Float64, :Complex128))
    @eval begin
        function scal!(n::Integer, DA::$elty,
                       DX::Union(Ptr{$celty},Array{$celty}), incx::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$celty}, Ptr{BlasInt}),
                  &(2*n), &DA, DX, &incx)
            return DX
        end
    end
end

# dot
for (fname, elty) in ((:ddot_,:Float64), (:sdot_,:Float32))
    @eval begin
        #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
        # *     .. Scalar Arguments ..
        #       INTEGER INCX,INCY,N
        # *     ..
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION DX(*),DY(*)
        function dot(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer,
                     DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), $elty,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, DX, &incx, DY, &incy)
        end
    end
end
for (fname, elty, relty) in ((:zdotc_,:Complex128,:Float64), (:cdotc_,:Complex64,:Float32))
    @eval begin
        #       DOUBLE COMPLEX FUNCTION ZDOTC(N,ZX,INCX,ZY,INCY)
        # *     .. Scalar Arguments ..
        #       INTEGER INCX,INCY,N
        # *     ..
        # *     .. Array Arguments ..
        #       DOUBLE COMPLEX ZX(*),ZY(*)
        function dot(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer,
                     DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            convert($elty, ccall(($(string(fname)),libblas), Complex{$relty},
                                 (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                                 &n, DX, &incx, DY, &incy))
        end
    end
end
function dot{T<:BlasFloat}(DX::Array{T}, DY::Array{T})
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    dot(n, DX, 1, DY, 1)
end

for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:Complex128,:Float64),
                                (:scnrm2_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE DNRM2(N,X,INCX)
        function nrm2(n::Integer, X::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            ccall(($(string(fname)),libblas), $ret_type,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, X, &incx)
        end
    end
end
nrm2(A::Array) = nrm2(length(A), A, 1)

for (fname, elty, ret_type) in ((:dasum_,:Float64,:Float64),
                                (:sasum_,:Float32,:Float32),
                                (:dzasum_,:Complex128,:Float64),
                                (:scasum_,:Complex64,:Float32))
    @eval begin
        # SUBROUTINE ASUM(N, X, INCX)
        function asum(n::Integer, X::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            ccall(($(string(fname)),libblas), $ret_type,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, X, &incx)
        end
    end
end
asum(A::Array) = asum(length(A), A, 1)

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
        function axpy!(n::Integer, alpha::($elty),
                       dx::Union(Ptr{$elty},Array{$elty}), incx::Integer,
                       dy::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, &alpha, dx, &incx, dy, &incy)
            dy
        end
    end
end
function axpy!{T,Ta<:Number}(alpha::Ta, x::Array{T}, y::Array{T})
    if length(x) != length(y)
        error("Inputs should be of the same length")
    end
    return axpy!(length(x), convert(T,alpha), x, 1, y, 1)
end

function axpy!{T,Ta<:Number,Ti<:Integer}(alpha::Ta, x::Array{T}, rx::Union(Range1{Ti},Range{Ti}),
                                         y::Array{T}, ry::Union(Range1{Ti},Range{Ti}))

    if length(rx) != length(ry)
        error("Ranges should be of the same length")
    end

    if minimum(rx) < 1 || maximum(rx) > length(x) || minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError())
    end
    axpy!(length(rx), convert(T, alpha), pointer(x)+(first(rx)-1)*sizeof(T),
          step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

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
           m, n = size(C)
           if m != n error("syrk!: matrix C must be square") end
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n error("syrk!: dimension mismatch") end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, 
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
    syrk!(uplo, trans, convert(T,alpha), A, zero(T), Array(T, (n, n)))
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
           m, n = size(C)
           if m != n error("syrk!: matrix C must be square") end
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n error("syrk!: dimension mismatch") end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, 
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, 
                  Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &trans, &n, &k, 
                 &alpha, A, &max(1,stride(A,2)), &beta, 
                 C, &max(1,stride(C,2)))
           C
       end
       function herk(uplo::BlasChar, trans::BlasChar, alpha::($elty), A::StridedVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           herk!(uplo, trans, alpha, A, zero($elty), Array($elty, (n,n)))
       end
       herk(uplo::BlasChar, trans::BlasChar, A::StridedVecOrMat{$elty}) = herk(uplo, trans, one($elty), A)
   end
end

# (GB) general banded matrix-vector multiplication
for (fname, elty) in ((:dgbmv_,:Float64), (:sgbmv_,:Float32),
                      (:zgbmv_,:Complex128), (:cgbmv_,:Complex64))
   @eval begin
       # SUBROUTINE DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
       # *     .. Scalar Arguments ..
       #       DOUBLE PRECISION ALPHA,BETA
       #       INTEGER INCX,INCY,KL,KU,LDA,M,N
       #       CHARACTER TRANS
       # *     .. Array Arguments ..
       #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
       function gbmv!(trans::BlasChar, m::Integer, kl::Integer, ku::Integer,
                      alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty},
                      beta::($elty), y::StridedVector{$elty})
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &trans, &m, &size(A,2), &kl, &ku, 
                 &alpha, A, &max(1,stride(A,2)), x, &stride(x,1), 
                 &beta, y, &stride(y,1))
           y
       end
       function gbmv(trans::BlasChar, m::Integer, kl::Integer, ku::Integer,
                     alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
           n = stride(A,2)
           gbmv!(trans, m, kl, ku, alpha, A, x, zero($elty), Array($elty, n))
       end
       function gbmv(trans::BlasChar, m::Integer, kl::Integer, ku::Integer,
                     A::StridedMatrix{$elty}, x::StridedVector{$elty})
           gbmv(trans, m, kl, ku, one($elty), A, x)
       end
   end
end

# (SB) symmetric banded matrix-vector multiplication
for (fname, elty) in ((:dsbmv_,:Float64), (:ssbmv_,:Float32),
                      (:zsbmv_,:Complex128), (:csbmv_,:Complex64))
   @eval begin
       #       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
       # *     .. Scalar Arguments ..
       #       DOUBLE PRECISION ALPHA,BETA
       #       INTEGER INCX,INCY,K,LDA,N
       #       CHARACTER UPLO
       # *     .. Array Arguments ..
       #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
       function sbmv!(uplo::BlasChar, k::Integer,
                      alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, 
                      beta::($elty), y::StridedVector{$elty})
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &size(A,2), &k, &alpha, A, &max(1,stride(A,2)), x, &stride(x,1),
                 &beta, y, &stride(y,1))
           y
       end
       function sbmv(uplo::BlasChar, k::Integer, alpha::($elty), A::StridedMatrix{$elty},
                     x::StridedVector{$elty})
           n = size(A,2)
           sbmv!(uplo, k, alpha, A, x, zero($elty), Array($elty, n))
       end
       function sbmv(uplo::BlasChar, k::Integer, A::StridedMatrix{$elty}, x::StridedVector{$elty})
           sbmv(uplo, k, one($elty), A, x)
       end
   end
end

# (GE) general matrix-matrix and matrix-vector multiplication
for (gemm, gemv, elty) in
    ((:dgemm_,:dgemv_,:Float64),
     (:sgemm_,:sgemv_,:Float32),
     (:zgemm_,:zgemv_,:Complex128),
     (:cgemm_,:cgemv_,:Complex64))
   @eval begin
       # SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       DOUBLE PRECISION ALPHA,BETA
       #       INTEGER K,LDA,LDB,LDC,M,N
       #       CHARACTER TRANSA,TRANSB
       # *     .. Array Arguments ..
       #       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
       function gemm!(transA::BlasChar, transB::BlasChar,
                      alpha::($elty), A::StridedVecOrMat{$elty},
                      B::StridedMatrix{$elty},
                      beta::($elty), C::StridedVecOrMat{$elty})
#           if any([stride(A,1), stride(B,1), stride(C,1)] .!= 1)
#               error("gemm!: BLAS module requires contiguous matrix columns")
#           end  # should this be checked on every call?
           m = size(A, transA == 'N' ? 1 : 2)
           k = size(A, transA == 'N' ? 2 : 1)
           n = size(B, transB == 'N' ? 2 : 1)
           if m != size(C,1) || n != size(C,2)
               error("gemm!: mismatched dimensions")
           end
           ccall(($(string(gemm)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &transA, &transB, &m, &n, &k, &alpha, A, &max(1,stride(A,2)),
                 B, &max(1,stride(B,2)), &beta, C, &max(1,stride(C,2)))
           C
       end
       function gemm(transA::BlasChar, transB::BlasChar,
                     alpha::($elty), A::StridedMatrix{$elty},
                     B::StridedMatrix{$elty})
           gemm!(transA, transB, alpha, A, B, zero($elty),
                 Array($elty, (size(A, transA == 'N' ? 1 : 2),
                               size(B, transB == 'N' ? 2 : 1))))
       end
       function gemm(transA::BlasChar, transB::BlasChar,
                     A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
           gemm(transA, transB, one($elty), A, B)
       end
       #SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
       #*     .. Scalar Arguments ..
       #      DOUBLE PRECISION ALPHA,BETA
       #      INTEGER INCX,INCY,LDA,M,N
       #      CHARACTER TRANS
       #*     .. Array Arguments ..
       #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
       function gemv!(trans::BlasChar,
                      alpha::($elty), A::StridedMatrix{$elty},
                      X::StridedVector{$elty},
                      beta::($elty), Y::StridedVector{$elty})
           ccall(($(string(gemv)),libblas), Void,
                 (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{BlasInt},
                  Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &trans, &size(A,1), &size(A,2), &alpha, A, &max(1,stride(A,2)),
                 X, &stride(X,1), &beta, Y, &stride(Y,1))
           Y
       end
       function gemv(trans::BlasChar,
                     alpha::($elty), A::StridedMatrix{$elty},
                     X::StridedVector{$elty})
           gemv!(trans, alpha, A, X, zero($elty),
                 Array($elty, size(A, (trans == 'N' ? 1 : 2))))
       end
       function gemv(trans::BlasChar, A::StridedMatrix{$elty}, X::StridedVector{$elty})
           gemv!(trans, one($elty), A, X, zero($elty),
                 Array($elty, size(A, (trans == 'N' ? 1 : 2))))
       end
   end
end

# (SY) symmetric matrix-matrix and matrix-vector multiplication
for (mfname, vfname, elty) in ((:dsymm_,:dsymv_,:Float64),
                               (:ssymm_,:ssymv_,:Float32),
                               (:zsymm_,:zsymv_,:Complex128),
                               (:csymm_,:csymv_,:Complex64))
   @eval begin
       #     SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
       #     .. Scalar Arguments ..
       #     DOUBLE PRECISION ALPHA,BETA
       #     INTEGER LDA,LDB,LDC,M,N
       #     CHARACTER SIDE,UPLO
       #     .. Array Arguments ..
       #     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
       function symm!(side::BlasChar, uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty},
                      B::StridedMatrix{$elty}, beta::($elty), C::StridedMatrix{$elty})
           m, n = size(C)
           k, j = size(A)
           if k != j error("symm!: matrix A is $k by $j but must be square") end
           if j != (side == 'L' ? m : n) || size(B,2) != n error("symm!: Dimension mismatch") end
           ccall(($(string(mfname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                  Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &side, &uplo, &m, &n, &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)),
                 &beta, C, &max(1,stride(C,2)))
           C
       end
       function symm(side::BlasChar, uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty},
                     B::StridedMatrix{$elty})
           symm!(side, uplo, alpha, A, B, zero($elty), similar(B))
       end
       function symm(side::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
           symm(side, uplo, one($elty), A, B)
       end
       #      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
       #     .. Scalar Arguments ..
       #      DOUBLE PRECISION ALPHA,BETA
       #      INTEGER INCX,INCY,LDA,N
       #      CHARACTER UPLO
       #     .. Array Arguments ..
       #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
       function symv!(uplo::BlasChar, alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty},
                      beta::($elty), y::StridedVector{$elty})
           m, n = size(A)
           if m != n error("symm!: matrix A is $m by $n but must be square") end
           if m != length(x) || m != length(y) error("symm!: dimension mismatch") end
           ccall(($(string(vfname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                 &uplo, &n, &alpha, A, &max(1,stride(A,2)), x, &stride(x,1), &beta, y, &stride(y,1))
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

# (TR) Triangular matrix and vector multiplication and solution
for (mmname, mvname, smname, svname, elty) in
    ((:dtrmm_,:dtrmv_,:dtrsm_,:dtrsv_,:Float64),
     (:strmm_,:strmv_,:strsm_,:strsv_,:Float32),
     (:ztrmm_,:ztrmv_,:ztrsm_,:ztrsv_,:Complex128),
     (:ctrmm_,:ctrmv_,:ctrsm_,:ctrsv_,:Complex64))
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
            m,n = size(B)
            mA,nA = size(A)
            if mA != nA throw(DimensionMismatch("matrix must be square")) end
            if nA != (side == 'L' ? m : n) throw(DimensionMismatch("")) end
            ccall(($(string(mmname)), libblas), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &side, &uplo, &transa, &diag, &m, &n,
                  &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)))
            B
        end
        function trmm(side::Char, uplo::Char, transa::Char, diag::Char,
                      alpha::$elty, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            trmm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
        #       SUBROUTINE DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        # *     .. Scalar Arguments ..
        #       INTEGER INCX,LDA,N
        #       CHARACTER DIAG,TRANS,UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),X(*)
        function trmv!(uplo::Char, trans::Char, diag::Char,
                       A::StridedMatrix{$elty}, x::StridedVector{$elty})
            n,m = size(A)
            if m != n throw(DimensionMismatch("size(A)=$(size(A)) should be square")) end
            if n != length(x)
                throw(DimensionMismatch("length(x)=$(length(x))does not match size(A)=$(size(A))"))
            end
            ccall(($(string(mvname)), libblas), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &trans, &diag, &n, A, &max(1,stride(A,2)), x, &1)
            x
        end
        function trmv(uplo::Char, trans::Char, diag::Char,
                      A::StridedMatrix{$elty}, x::StridedVector{$elty})
            trmv!(uplo, trans, diag, A, copy(x))
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
            m,n = size(B); k,l = size(A)
            if k != l throw(DimensionMismatch("size(A)=$(size(A)) should be square")) end
            if k != (side == 'L' ? m : n)
                throw(DimensionMismatch("size of A is $n, size(B)=($m,$n) and transa='$transa'"))
            end
            ccall(($(string(smname)), libblas), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &side, &uplo, &transa, &diag, &m, &n,
                  &alpha, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)))
            B
        end
        function trsm(side::Char, uplo::Char, transa::Char, diag::Char,
                       alpha::$elty, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            trsm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
        #       SUBROUTINE DTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        #       .. Scalar Arguments ..
        #       INTEGER INCX,LDA,N
        #       CHARACTER DIAG,TRANS,UPLO
        #       .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),X(*)
        function trsv!(uplo::Char, trans::Char, diag::Char,
                       A::StridedMatrix{$elty}, x::StridedVector{$elty})
            m,n = size(A)
            if m != n throw(DimensionMismatch("size(A)=$(size(A)) should be square")) end
            if n != length(x)
                throw(DimensionMismatch("size of A is $n != length(x) = $(length(x))"))
            end
            ccall(($(string(svname)), libblas), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &trans, &diag, &n, A, &max(1,stride(A,2)), x, &1)
            x
        end
        function trsv(uplo::Char, trans::Char, diag::Char,
                       A::StridedMatrix{$elty}, x::StridedVector{$elty})
            trsv!(uplo, trans, diag, A, copy(x))
        end
    end
end

end # module

function copy!{T<:BlasFloat,Ti<:Integer}(dest::Array{T}, rdest::Union(Range1{Ti},Range{Ti}), 
                                         src::Array{T}, rsrc::Union(Range1{Ti},Range{Ti}))
    if minimum(rdest) < 1 || maximum(rdest) > length(dest) || minimum(rsrc) < 1 || maximum(rsrc) > length(src)
        throw(BoundsError())
    end
    if length(rdest) != length(rsrc)
        error("Ranges must be of the same length")
    end
    BLAS.blascopy!(length(rsrc), pointer(src)+(first(rsrc)-1)*sizeof(T), step(rsrc),
                   pointer(dest)+(first(rdest)-1)*sizeof(T), step(rdest))
    return dest
end
