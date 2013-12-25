module SparseBLAS

import ..LinAlg: BlasFloat, BlasChar, BlasInt, blas_int, DimensionMismatch

const libblas = Base.libblas_name

for (mv, sv, mm, sm, T) in ((:mkl_scscmv_, :mkl_scscsv_, :mkl_scscmm_, :mkl_scscsm_, :Float32), 
			   			    (:mkl_dcscmv_, :mkl_dcscsv_, :mkl_dcscmm_, :mkl_dcscsm_, :Float64),
			   			    (:mkl_ccscmv_, :mkl_ccscsv_, :mkl_ccscmm_, :mkl_ccscsm_, :Complex64),
			   			    (:mkl_zcscmv_, :mkl_zcscsv_, :mkl_zcscmm_, :mkl_zcscsm_, :Complex128))
	@eval begin
		function cscmv!(transa::BlasChar, α::$T, matdescra::ASCIIString, A::SparseMatrixCSC{$T, BlasInt}, x::StridedVector{$T}, β::$T, y::StridedVector{$T})
			length(x) == A.n || throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with vector of length $(length(x))"))
			length(y) == A.m || throw(DimensionMismatch("Vector of length $(A.m) added to vector of length $(length(y))")) # 
			ccall(($(string(mv)), libblas), Void,
				(Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T},
				 Ptr{Uint8}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
				 Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}),
				&transa, &A.m, &A.n, &α,
				matdescra, A.nzval, A.rowval, pointer(A.colptr, 1),
				pointer(A.colptr, 2), x, &β, y)
			return y
		end

		function cscsv!(transa::BlasChar, α::$T, matdescra::ASCIIString, A::SparseMatrixCSC{$T, BlasInt}, x::StridedVector{$T}, y::StridedVector{$T})
			A.m == A.n || throw(DimensionMismatch("Matrix must be square"))
			length(x) == A.n || throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with vector of length $(length(x))"))
			length(y) >= A.m || throw(DimensionMismatch("Vector of length $(A.m) assigned to vector of length $(length(y))"))
			ccall(($(string(sv)), libblas), Void,
				(Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{Uint8},
				 Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
				 Ptr{$T}, Ptr{$T}),
				&transa, &A.m, &α, matdescra, 
				A.nzval, A.rowval, pointer(A.colptr, 1), pointer(A.colptr, 2), 
				x, y)
			return y
		end

		function cscmm!(transa::BlasChar, α::$T, matdescra::ASCIIString, A::SparseMatrixCSC{$T, BlasInt}, B::StridedMatrix{$T}, β::$T, C::StridedMatrix{$T})
			mB, nB = size(B)
			mC, nC = size(C)
			A.n == mB || throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with matrix with $(mB) rows"))
			A.m == mC || throw(DimensionMismatch("Matrix with $(A.m) rows added to matrix with $(mC) rows"))
			nB == nC || throw(DimensionMismatch("Matrix with $(nB) columns added to matrix with $(nC) columns"))
			ccall(($(string(mm)), libblas), Void,
				(Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
				 Ptr{$T}, Ptr{Uint8}, Ptr{$T}, Ptr{BlasInt}, 
				 Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
				 Ptr{$T}, Ptr{$T}, Ptr{BlasInt}),
				&transa, &A.m, &nC, &A.n,
				&α, matdescra, A.nzval, A.rowval, 
				pointer(A.colptr, 1), pointer(A.colptr, 2), B, &mB,
				&β, C, &mC)
			return C
		end

		function cscsm!(transa::BlasChar, α::$T, matdescra::ASCIIString, A::SparseMatrixCSC{$T, BlasInt}, B::StridedMatrix{$T}, C::StridedMatrix{$T})
			mB, nB = size(B)
			mC, nC = size(C)
			A.m == A.n || throw(DimensionMismatch("Matrix must be square"))
			A.n == mB || throw(DimensionMismatch("Matrix with $(A.n) columns multiplied with matrix with $(mB) rows"))
			A.m <= mC || throw(DimensionMismatch("Matrix with $(A.m) rows assigned to matrix with $(mC) rows"))
			nB <= nC || throw(DimensionMismatch("Matrix with $(nB) columns assigned to matrix with $(nC) columns"))
			ccall(($(string(sm)), libblas), Void,
				(Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, 
				 Ptr{Uint8}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, 
				 Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
				 Ptr{BlasInt}),
				&transa, &A.n, &nC, &α, 
				matdescra, A.nzval, A.rowval, pointer(A.colptr, 1), 
				pointer(A.colptr, 2), B, &mB, C,
				&mC)
			return C
		end
	end
end
end #module