## Triangular
type Triangular{T<:BlasFloat} <: AbstractMatrix{T}
    UL::Matrix{T}
    uplo::Char
    unitdiag::Char
    function Triangular(A::Matrix{T}, uplo::Char, unitdiag::Char)
        if size(A, 1) != size(A, 2) throw(LAPACK.DimensionMismatch("Matrix must be square")) end
        return new(A, uplo, unitdiag)
    end
end
Triangular{T<:BlasFloat}(A::Matrix{T}, uplo::Char, unitdiag::Char) = Triangular{T}(A, uplo, unitdiag)
Triangular(A::Matrix, uplo::Char, unitdiag::Bool) = Triangular(A, uplo, unitdiag ? 'U' : 'N')
Triangular(A::Matrix, uplo::Char) = Triangular(A, uplo, all(diag(A) .== 1) ? true : false)
function Triangular(A::Matrix)
    if istriu(A) return Triangular(A, 'U') end
    if istril(A) return Triangular(A, 'L') end
    error("Matrix is not triangular")
end

size(A::Triangular, args...) = size(A.UL, args...)
function full(A::Triangular)
    if 
        istril(A) return tril(A.UL)
    else
        return triu(A.UL)
    end
end
print_matrix(io::IO, A::Triangular) = print_matrix(io, full(A))

istril(A::Triangular) = A.uplo == 'L'
istriu(A::Triangular) = A.uplo == 'U'

# Vector multiplication
*(A::Triangular, b::Vector) = BLAS.trmv(A.uplo, 'N', A.unitdiag, A.UL, b)
Ac_mul_B{T<:Union(Complex128, Complex64)}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'C', A.unitdiag, A.UL, b)
At_mul_B{T<:Union(Float64, Float32)}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'T', A.unitdiag, A.UL, b)

# Matrix multiplication
*(A::Triangular, B::StridedMatrix) = BLAS.trmm('L', A.uplo, 'N', A.unitdiag, 1.0, A.UL, B)
*(A::StridedMatrix, B::Triangular) = BLAS.trmm('R', B.uplo, 'N', B.unitdiag, 1.0, A, B.UL)
Ac_mul_B{T<:Union(Complex128, Complex64)}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'C', A.unitdiag, 1.0, A.UL, B)
Ac_mul_B{T<:Union(Float64, Float32)}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'T', A.unitdiag, 1.0, A.UL, B)
A_mul_Bc{T<:Union(Complex128, Complex64)}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'C', B.unitdiag, 1.0, A, B.UL)
A_mul_Bc{T<:Union(Float64, Float32)}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'T', B.unitdiag, 1.0, A, B.UL)

function \(A::Triangular, B::StridedVecOrMat)
    r, info = LAPACK.trtrs!(A.uplo, 'N', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end
function Ac_ldiv_B{T<:Union(Float64, Float32)}(A::Triangular{T}, B::StridedVecOrMat{T}) 
    r, info = LAPACK.trtrs!(A.uplo, 'T', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end
function Ac_ldiv_B{T<:Union(Complex128, Complex64)}(A::Triangular{T}, B::StridedVecOrMat{T})
    r, info = LAPACK.trtrs!(A.uplo, 'C', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end

det(A::Triangular) = prod(diag(A.UL))

inv(A::Triangular) = LAPACK.trtri!(A.uplo, A.unitdiag, copy(A.UL))[1]
