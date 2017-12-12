# This file is a part of Julia. License is MIT: https://julialang.org/license

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/cholmod.jl, to deprecate
@eval SuiteSparse.CHOLMOD begin
    using Base.LinAlg: Adjoint, Transpose
    Base.Ac_ldiv_B(A::RealHermSymComplexHermF64SSL, B::StridedVecOrMat) = \(Adjoint(A), B)
    Base.Ac_ldiv_B(L::Factor, B::Dense) = \(Adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::VecOrMat) = \(Adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::Sparse) = \(Adjoint(L), B)
    Base.Ac_ldiv_B(L::Factor, B::SparseVecOrMat) = \(Adjoint(L), B)
    Base.Ac_ldiv_B(L::FactorComponent, B) = \(Adjoint(L), B)
    Base.Ac_ldiv_B(L::FactorComponent, B::RowVector) = \(Adjoint(L), B)
    Base.Ac_mul_B(A::Sparse, B::Dense) = *(Adjoint(A), B)
    Base.Ac_mul_B(A::Sparse, B::VecOrMat) =  *(Adjoint(A), B)
    Base.Ac_mul_B(A::Sparse, B::Sparse) = *(Adjoint(A), B)
    Base.A_mul_Bc(A::Sparse{Tv}, B::Sparse{Tv}) where {Tv<:VRealTypes} = *(A, Adjoint(B))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/umfpack.jl, to deprecate
@eval SuiteSparse.UMFPACK begin
    using Base.LinAlg: Adjoint, Transpose
    Base.A_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, lu, B)
    Base.At_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, Transpose(lu), B)
    Base.Ac_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
        Base.LinAlg.ldiv!(X, Adjoint(lu), B)
    Base.A_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, lu, B)
    Base.At_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, Transpose(lu), B)
    Base.Ac_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
        Base.LinAlg.ldiv!(X, Adjoint(lu), B)
    Base.A_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(lu, B)
    Base.At_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(Transpose(lu), B)
    Base.Ac_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Base.LinAlg.ldiv!(Adjoint(lu), B)
    Base.A_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(lu, B)
    Base.At_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(Transpose(lu), B)
    Base.Ac_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Base.LinAlg.ldiv!(Adjoint(lu), B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/spqr.jl, to deprecate
@eval SuiteSparse.SPQR begin
    using Base.LinAlg: Adjoint, Transpose
    Base.A_mul_Bc!(A::StridedMatrix, Q::QRSparseQ) = Base.LinAlg.mul!(A, Adjoint(Q))
    Base.Ac_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = Base.LinAlg.mul!(Adjoint(Q), A)
    Base.A_mul_B!(A::StridedMatrix, Q::QRSparseQ) = Base.LinAlg.mul!(A, Q)
    Base.A_mul_B!(Q::QRSparseQ, A::StridedVecOrMat) = Base.LinAlg.mul!(Q, A)
end
