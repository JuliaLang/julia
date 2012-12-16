typealias LapackType Union(Float64,Float32,Complex128,Complex64)

module BLAS

export copy!,
       scal!,
       scal,
       dot,
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

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
for (fname, elty) in ((:dcopy_,:Float64), (:scopy_,:Float32),
                      (:zcopy_,:Complex128), (:ccopy_,:Complex64))
    @eval begin
        function copy!(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer, DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, DX, &incx, DY, &incy)
            DY
        end
    end
end

# SUBROUTINE DSCAL(N,DA,DX,INCX)
for (fname, elty) in ((:dscal_,:Float64),    (:sscal_,:Float32),
                      (:zscal_,:Complex128), (:cscal_,:Complex64))
    @eval begin
        function scal!(n::Integer, DA::$elty, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                  &n, &DA, DX, &incx)
            DX
        end
        function scal(n::Integer, DA::$elty, DX_orig::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            DX = copy(DX_orig)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                  &n, &DA, DX, &incx)
            DX
        end
    end
end

# ccall is unable to return complex values (Issue #85)
#@blas_dot :zdotc_ Complex128
#@blas_dot :cdotc_ Complex64
# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
for (fname, elty) in ((:ddot_,:Float64), (:sdot_,:Float32))
    @eval begin
        function dot(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer, DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), $elty,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, DX, &incx, DY, &incy)
        end
    end
end

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:Complex128,:Float64),
                                (:scnrm2_,:Complex64,:Float32))
    @eval begin
        function nrm2(n::Integer, X::Union(Ptr{$elty},Array{$elty}), incx::Integer)
            ccall(($(string(fname)),libblas), $ret_type,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, X, &incx)
        end
    end
end

# SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
# DY <- DA*DX + DY
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION DA
#      INTEGER INCX,INCY,N
#*     .. Array Arguments ..
#      DOUBLE PRECISION DX(*),DY(*)
for (fname, elty) in ((:daxpy_,:Float64), (:saxpy_,:Float32),
                      (:zaxpy_,:Complex128), (:caxpy_,:Complex64))
    @eval begin
        function axpy!(n::Integer, alpha::($elty),
                       dx::Union(Ptr{$elty},Array{$elty}), incx::Integer,
                       dy::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(($(string(fname)),libblas), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, &alpha, dx, &incx, dy, &incy)
            return dy
        end
    end
end

function axpy!{T,Ta<:Number}(alpha::Ta, x::Array{T}, y::Array{T})
    if length(x) != length(y)
        error("Inputs should be of the same length")
    end
    return axpy!(length(x), convert(T, alpha), x, 1, y, 1)
end

function axpy!{T,Ta<:Number,Ti<:Integer}(alpha::Ta, x::Array{T}, rx::Union(Range1{Ti},Range{Ti}), y::Array{T}, ry::Union(Range1{Ti},Range{Ti}))

    if length(rx) != length(ry)
        error("Ranges should be of the same length")
    end

    if min(rx) < 1 || max(rx) > length(x) || min(ry) < 1 || max(ry) > length(y)
        throw(BoundsError())
    end

    return axpy!(length(rx), convert(T, alpha), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end


# SUBROUTINE DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       REAL ALPHA,BETA
#       INTEGER K,LDA,LDC,N
#       CHARACTER TRANS,UPLO
# *     ..
# *     .. Array Arguments ..
#       REAL A(LDA,*),C(LDC,*)
for (fname, elty) in ((:dsyrk_,:Float64), (:ssyrk_,:Float32),
                      (:zsyrk_,:Complex128), (:csyrk_,:Complex64))
   @eval begin
       function syrk!(uplo, trans, alpha::($elty), A::StridedVecOrMat{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           m, n = size(C)
           if m != n error("syrk!: matrix C must be square") end
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n error("syrk!: dimension mismatch") end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k, &alpha, A, &stride(A,2), &beta, C, &stride(C,2))
           C
       end
       function syrk(uplo, trans, alpha::($elty), A::StridedVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           k = size(A, trans == 'N' ? 2 : 1)
           C = Array($elty, (n, n)) 
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k, &alpha, A, &stride(A,2), &0., C, &stride(C,2))
           C
       end
   end
end

# SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       REAL ALPHA,BETA
#       INTEGER K,LDA,LDC,N
#       CHARACTER TRANS,UPLO
# *     ..
# *     .. Array Arguments ..
#       COMPLEX A(LDA,*),C(LDC,*)
for (fname, elty) in ((:zherk_,:Complex128), (:cherk_,:Complex64))
   @eval begin
       function herk!(uplo, trans, alpha::($elty), A::StridedVecOrMat{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           m, n = size(C)
           if m != n error("syrk!: matrix C must be square") end
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n error("syrk!: dimension mismatch") end
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k, &alpha, A, &stride(A,2), &beta, C, &stride(C,2))
           C
       end
       function herk(uplo, trans, alpha::($elty), A::StridedVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           k = size(A, trans == 'N' ? 2 : 1)
           C = Array($elty, (n, n)) 
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k, &alpha, A, &stride(A,2), &0., C, &stride(C,2))
           C
       end
   end
end

# (GB) general banded matrix-vector multiplication
# SUBROUTINE DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER INCX,INCY,KL,KU,LDA,M,N
#       CHARACTER TRANS
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
for (fname, elty) in ((:dgbmv_,:Float64), (:sgbmv_,:Float32),
                      (:zgbmv_,:Complex128), (:cgbmv_,:Complex64))
   @eval begin
       function gbmv!(trans, m::Integer, kl::Integer, ku::Integer,
                      alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty},
                      beta::($elty), y::StridedVector{$elty})
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &trans, &m, &size(A,2), &kl, &ku, &alpha, A, &stride(A,2),
                 x, &stride(x,1), &beta, y, &stride(y,1))
           y
       end
       function gbmv(trans, m::Integer, kl::Integer, ku::Integer,
                     alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty})
           n = stride(A,2)
           y = Array($elty, n)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &trans, &m, &n, &kl, &ku, &alpha, A, &stride(A,2),
                 x, &stride(x,1), &0., y, &1)
           y
       end

   end
end

# (SB) symmetric banded matrix-vector multiplication
#       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER INCX,INCY,K,LDA,N
#       CHARACTER UPLO
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
for (fname, elty) in ((:dsbmv_,:Float64), (:ssbmv_,:Float32),
                      (:zsbmv_,:Complex128), (:csbmv_,:Complex64))
   @eval begin
       function sbmv!(uplo, k::Integer,
                      alpha::($elty), A::StridedMatrix{$elty}, x::StridedVector{$elty}, 
                      beta::($elty), y::StridedVector{$elty})
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                 Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &size(A,2), &k, &alpha, A, &stride(A,2), x, &stride(x,1), &beta, y, &stride(y,1))
           y
       end
       function sbmv(uplo, k::Integer, alpha::($elty), A::StridedMatrix{$elty},
                     x::StridedVector{$elty})
           n = size(A,2)
           y = Array($elty, n)
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                 Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &size(A,2), &k, &alpha, A, &stride(A,2), x, &stride(x,1), &0., y, &1)
           y
       end
   end
end

# (GE) general matrix-matrix multiplication
# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
for (fname, elty) in ((:dgemm_,:Float64), (:sgemm_,:Float32),
                      (:zgemm_,:Complex128), (:cgemm_,:Complex64))
   @eval begin
       function gemm!(transA, transB, alpha::($elty), A::StridedMatrix{$elty},
                      B::StridedMatrix{$elty}, beta::($elty), C::StridedMatrix{$elty})
           m = size(A, transA == 'N' ? 1 : 2)
           k = size(A, transA == 'N' ? 2 : 1)
           n = size(B, transB == 'N' ? 2 : 1)
           if m != size(C,1) || n != size(C,2) error("gemm!: mismatched dimensions") end
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &transA, &transB, &m, &n, &k, &alpha, A, &stride(A,2),
                 B, &stride(B,2), &beta, C, &stride(C,2))
           C
       end
       function gemm(transA, transB, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
           m = size(A, transA == 'N' ? 1 : 2)
           k = size(A, transA == 'N' ? 2 : 1)
           if k != size(B, transB == 'N' ? 1 : 2) error("gemm!: mismatched dimensions") end
           n = size(B, transB == 'N' ? 2 : 1)
           C = Array($elty, (m, n))
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &transA, &transB, &m, &n, &k, &alpha, A, &stride(A,2),
                 B, &stride(B,2), &0., C, &stride(C,2))
           C
       end
   end
end

#SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION ALPHA,BETA
#      INTEGER INCX,INCY,LDA,M,N
#      CHARACTER TRANS
#*     .. Array Arguments ..
#      DOUBLE PRECISION A(LDA,*),X(*),Y(*)

for (fname, elty) in ((:dgemv_,:Float64), (:sgemv_,:Float32),
                      (:zgemv_,:Complex128), (:cgemv_,:Complex64))
   @eval begin
       function gemv!(trans, alpha::($elty), A::StridedMatrix{$elty},
                      X::StridedVector{$elty}, beta::($elty), Y::StridedVector{$elty})
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &trans, &size(A,1), &size(A,2), &alpha, A, &stride(A,2),
                 X, &stride(X,1), &beta, Y, &stride(Y,1))
           Y
       end
       function gemv!(trans, alpha::($elty), A::StridedMatrix{$elty}, X::StridedVector{$elty})
           Y = Array($elty, size(A,1))
           ccall(($(string(fname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &trans, &size(A,1), &size(A,2), &alpha, A, &stride(A,2),
                 X, &stride(X,1), &0., Y, &1)
           Y
       end
   end
end

# (SY) symmetric matrix-matrix and matrix-vector multiplication

#     SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#     .. Scalar Arguments ..
#     DOUBLE PRECISION ALPHA,BETA
#     INTEGER LDA,LDB,LDC,M,N
#     CHARACTER SIDE,UPLO
#     .. Array Arguments ..
#     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

#      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#     .. Scalar Arguments ..
#      DOUBLE PRECISION ALPHA,BETA
#      INTEGER INCX,INCY,LDA,N
#      CHARACTER UPLO
#     .. Array Arguments ..
#      DOUBLE PRECISION A(LDA,*),X(*),Y(*)

for (vfname, mfname, elty) in
    ((:dsymv_,:dsymm_,:Float64),
     (:ssymv_,:ssymm_,:Float32),
     (:zsymv_,:zsymm_,:Complex128),
     (:csymv_,:csymm_,:Complex64))
   @eval begin
       function symv!(uplo, alpha::($elty), A::StridedMatrix{$elty}, X::StridedVector{$elty},
                      beta::($elty), Y::StridedVector{$elty})
           m, n = size(A)
           if m != n error("symm!: matrix A is $m by $n but must be square") end
           if m != length(X) || m != length(Y) error("symm!: dimension mismatch") end
           ccall(($(string(vfname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                 Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &n, &alpha, A, &stride(A,2), X, &stride(X,1), &beta, Y, &stride(Y,1))
           Y
       end
       function symv(uplo, alpha::($elty), A::StridedMatrix{$elty}, X::StridedVector{$elty})
           symv!(uplo, alpha, A, X, zero($elty), similar(X))
       end
       function symm!(side, uplo, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty},
                      beta::($elty), C::StridedMatrix{$elty})
           side = uppercase(convert(Char, side))
           m, n = size(C)
           k, j = size(A)
           if k != j error("symm!: matrix A is $k by $j but must be square") end
           if j != (side == 'L' ? m : n) || size(B,2) != n error("symm!: Dimension mismatch") end
           ccall(($(string(mfname)),libblas), Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                 Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &side, &uplo, &m, &n, &alpha, A, &stride(A,2), B, &stride(B,2),
                 &beta, C, &stride(C,2))
           C
       end
       function symm(side, uplo, alpha::($elty), A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
           symm!(side, uplo, alpha, A, B, zero($elty), similar(B))
       end
   end
end

end # module

# Use BLAS copy for small arrays where it is faster than memcpy, and for strided copying

function copy_to{T<:LapackType}(dest::Ptr{T}, src::Ptr{T}, n::Integer)
    if n < 200
        BLAS.copy!(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest, src, n*sizeof(T))
    end
    return dest
end

function copy_to{T<:LapackType}(dest::Array{T}, src::Array{T})
    n = numel(src)
    if n > numel(dest); throw(BoundsError()); end
    copy_to(pointer(dest), pointer(src), n)
    return dest
end

function copy_to{T<:LapackType,Ti<:Integer}(dest::Array{T}, rdest::Union(Range1{Ti},Range{Ti}), 
                                            src::Array{T}, rsrc::Union(Range1{Ti},Range{Ti}))
    if min(rdest) < 1 || max(rdest) > length(dest) || min(rsrc) < 1 || max(rsrc) > length(src)
        throw(BoundsError())
    end
    if length(rdest) != length(rsrc)
        error("Ranges must be of the same length")
    end
    BLAS.copy!(length(rsrc), pointer(src)+(first(rsrc)-1)*sizeof(T), step(rsrc),
              pointer(dest)+(first(rdest)-1)*sizeof(T), step(rdest))
    return dest
end
