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
    using Base.LinAlg: Adjoint, Transpose
    Base.A_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, lu, B)
    Base.At_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, transpose(lu), B)
    Base.Ac_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, adjoint(lu), B)
    Base.A_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, lu, B)
    Base.At_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, transpose(lu), B)
    Base.Ac_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, adjoint(lu), B)
    Base.A_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(lu, B)
    Base.At_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(transpose(lu), B)
    Base.Ac_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(adjoint(lu), B)
    Base.A_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(lu, B)
    Base.At_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(transpose(lu), B)
    Base.Ac_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(adjoint(lu), B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/spqr.jl, to deprecate
@eval SuiteSparse.SPQR begin
    using Base.LinAlg: Adjoint, Transpose
    Base.A_mul_Bc!(A::StridedMatrix, Q::QRSparseQ) = Base.LinAlg.mul!(A, adjoint(Q))
    Base.Ac_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = Base.LinAlg.mul!(adjoint(Q), A)
    Base.A_mul_B!(A::StridedMatrix, Q::QRSparseQ) = Base.LinAlg.mul!(A, Q)
    Base.A_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = Base.LinAlg.mul!(Q, A)
end
