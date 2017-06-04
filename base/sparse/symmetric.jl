# This file is a part of Julia. License is MIT: https://julialang.org/license

(*)(A::Symmetric{TA,SparseMatrixCSC{TA,S}}, x::StridedVecOrMat{Tx}) where {TA,S,Tx} = A_mul_B(A, x)

function A_mul_B!(α::Number, A::Symmetric{TA,SparseMatrixCSC{TA,S}}, B::StridedVecOrMat, β::Number, C::StridedVecOrMat) where {TA,S}
    A.data.n == size(B, 1) || throw(DimensionMismatch())
    A.data.m == size(C, 1) || throw(DimensionMismatch())
    A.uplo == 'U' ? A_mul_B_U_kernel!(α, A, B, β, C) : A_mul_B_L_kernel!(α, A, B, β, C)
end

function A_mul_B(A::Symmetric{TA,SparseMatrixCSC{TA,S}}, x::StridedVector{Tx}) where {TA,S,Tx}
    T = promote_type(TA, Tx)
    A_mul_B!(one(T), A, x, zero(T), similar(x, T, A.data.n))
end

function A_mul_B(A::Symmetric{TA,SparseMatrixCSC{TA,S}}, B::StridedMatrix{Tx}) where {TA,S,Tx}
    T = promote_type(TA, Tx)
    A_mul_B!(one(T), A, B, zero(T), similar(B, T, (A.data.n, size(B, 2))))
end

function A_mul_B_U_kernel!(α::Number, A::Symmetric{TA,SparseMatrixCSC{TA,S}}, B::StridedVecOrMat, β::Number, C::StridedVecOrMat) where {TA,S}
    colptr = A.data.colptr
    rowval = A.data.rowval
    nzval = A.data.nzval
    if β != 1
        β != 0 ? scale!(C, β) : fill!(C, zero(eltype(C)))
    end
    @inbounds for k = 1 : size(C, 2)
        @inbounds for col = 1 : A.data.n
            αxj = α * B[col, k]
            tmp = TA(0)
            @inbounds for j = colptr[col] : (colptr[col + 1] - 1)
                row = rowval[j]
                row > col && break  # assume indices are sorted
                a = nzval[j]
                C[row, k] += a * αxj
                row == col || (tmp += a * B[row, k])
            end
            C[col, k] += α * tmp
        end
    end
    C
end

function A_mul_B_L_kernel!(α::Number, A::Symmetric{TA,SparseMatrixCSC{TA,S}}, B::StridedVecOrMat, β::Number, C::StridedVecOrMat) where {TA,S}
    colptr = A.data.colptr
    rowval = A.data.rowval
    nzval = A.data.nzval
    if β != 1
        β != 0 ? scale!(C, β) : fill!(C, zero(eltype(C)))
    end
    @inbounds for k = 1 : size(C, 2)
        @inbounds for col = 1 : A.data.n
            αxj = α * B[col, k]
            tmp = TA(0)
            @inbounds for j = (colptr[col + 1] - 1) : -1 : colptr[col]
                row = rowval[j]
                row < col && break
                a = nzval[j]
                C[row, k] += a * αxj
                row == col || (tmp += a * B[row, k])
            end
            C[col, k] += α * tmp
        end
    end
    C
end
