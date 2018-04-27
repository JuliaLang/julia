# This file is a part of Julia. License is MIT: https://julialang.org/license

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/cholmod.jl, to deprecate
@eval SuiteSparse.CHOLMOD begin
    Base.Ac_ldiv_B(A::RealHermSymComplexHermF64SSL, B::StridedVecOrMat) = \(adjoint(A), B)
    Base.Ac_ldiv_B(L::Factor, B::Dense) = \(adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::VecOrMat) = \(adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::Sparse) = \(adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::SparseVecOrMat) = \(adjoint(L), B)
    Base.Ac_ldiv_B(L::FactorComponent, B) = \(adjoint(L), B)
    Base.Ac_ldiv_B(L::FactorComponent, B::RowVector) = \(adjoint(L), B)
    Base.Ac_mul_B(A::Sparse, B::Dense) = *(adjoint(A), B)
    Base.Ac_mul_B(A::Sparse, B::VecOrMat) =  *(adjoint(A), B)
    Base.Ac_mul_B(A::Sparse, B::Sparse) = *(adjoint(A), B)
    Base.A_mul_Bc(A::Sparse{Tv}, B::Sparse{Tv}) where {Tv<:VRealTypes} = *(A, adjoint(B))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/umfpack.jl, to deprecate
@eval SuiteSparse.UMFPACK begin
    using LinearAlgebra: Adjoint, Transpose
    LinearAlgebra.A_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        LinearAlgebra.ldiv!(X, lu, B)
    LinearAlgebra.At_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        LinearAlgebra.ldiv!(X, transpose(lu), B)
    LinearAlgebra.Ac_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        LinearAlgebra.ldiv!(X, adjoint(lu), B)
    LinearAlgebra.A_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        LinearAlgebra.ldiv!(X, lu, B)
    LinearAlgebra.At_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        LinearAlgebra.ldiv!(X, transpose(lu), B)
    LinearAlgebra.Ac_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        LinearAlgebra.ldiv!(X, adjoint(lu), B)
    LinearAlgebra.A_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = LinearAlgebra.ldiv!(B, lu, copy(B))
    LinearAlgebra.At_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = LinearAlgebra.ldiv!(B, transpose(lu), copy(B))
    LinearAlgebra.Ac_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = LinearAlgebra.ldiv!(B, adjoint(lu), copy(B))
    LinearAlgebra.A_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = LinearAlgebra.ldiv!(B, lu, copy(B))
    LinearAlgebra.At_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = LinearAlgebra.ldiv!(B, transpose(lu), copy(B))
    LinearAlgebra.Ac_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = LinearAlgebra.ldiv!(B, adjoint(lu), copy(B))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/spqr.jl, to deprecate
@eval SuiteSparse.SPQR begin
    using LinearAlgebra: Adjoint, Transpose
    LinearAlgebra.A_mul_Bc!(A::StridedMatrix, Q::QRSparseQ) = LinearAlgebra.mul!(A, adjoint(Q))
    LinearAlgebra.Ac_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = LinearAlgebra.mul!(adjoint(Q), A)
    LinearAlgebra.A_mul_B!(A::StridedMatrix, Q::QRSparseQ) = LinearAlgebra.mul!(A, Q)
    LinearAlgebra.A_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = LinearAlgebra.mul!(Q, A)
end
