import Base.LinAlg: BlasInt, BlasFloat, BlasReal

for (mv, mm, T) in ((:mkl_scscmv_, :mkl_scscmm_, :Float32),
                    (:mkl_dcscmv_, :mkl_dcscmm_, :Float64),
                    (:mkl_ccscmv_, :mkl_ccscmm_, :Complex64),
                    (:mkl_zcscmv_, :mkl_zcscmm_, :Complex128))
  @eval begin
      function cscmv!(transa::Char, α::$T, matdescra::String, A::SparseMatrixCSC{$T, BlasInt}, x::StridedVector{$T}, β::$T, y::StridedVector{$T})
          trns = uppercase(transa)
          in(trns, ['N','T','C']) || error("uppercase(transa) is '$trns', must be 'N' or 'T'")
          length(x) == (trns == 'T' ? A.m : A.n) ||
              throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with vector of length $(length(x))"))
          length(y) == (trns == 'T' ? A.n : A.m) ||
              throw(DimensionMismatch("Vector of length $(A.m) added to vector of length $(length(y))"))
          ccall(($(string(mv)), Base.BLAS.libblas), Void,
              (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T},
               Ptr{UInt8}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}),
              &transa, &A.m, &A.n, &α,
              matdescra, A.nzval, A.rowval, pointer(A.colptr, 1),
              pointer(A.colptr, 2), x, &β, y)
          return y
      end

      function cscmm!(transa::Char, α::$T, matdescra::String, A::SparseMatrixCSC{$T, BlasInt}, B::StridedMatrix{$T}, β::$T, C::StridedMatrix{$T})
          mB, nB = size(B)
          mC, nC = size(C)
          A.n == mB || throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with matrix with $(mB) rows"))
          A.m == mC || throw(DimensionMismatch("Matrix with $(A.m) rows added to matrix with $(mC) rows"))
          nB == nC || throw(DimensionMismatch("Matrix with $(nB) columns added to matrix with $(nC) columns"))
          ccall(($(string(mm)), Base.BLAS.libblas), Void,
              (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{$T}, Ptr{UInt8}, Ptr{$T}, Ptr{BlasInt},
               Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
               Ptr{$T}, Ptr{$T}, Ptr{BlasInt}),
              &transa, &A.m, &nC, &A.n,
              &α, matdescra, A.nzval, A.rowval,
              pointer(A.colptr, 1), pointer(A.colptr, 2), B, &mB,
              &β, C, &mC)
          return C
        end
    end
end

for (f, C, transp) in ((:A_mul_B, 'N', false),
                       (:Ac_mul_B, 'C', true),
                       (:At_mul_B, 'T', true))
    @eval begin
        function $(Symbol(f,:!))(α::T, A::SparseMatrixCSC{T,BlasInt}, B::StridedVecOrMat{T}, β::T, C::StridedVecOrMat{T}) where T <: BlasFloat
            _check_A_mul_B(A, B, C, $transp)
            isa(B,AbstractVector) ?
                cscmv!($C, α, "GUUF", A ,B, β, C) :
                cscmm!($C, α, "GUUF", A ,B, β, C)
        end
    end
end
