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

# deprecate lufact to lu
@eval SuiteSparse.UMFPACK begin
    @deprecate(lufact(A::SparseMatrixCSC), lu(A))
    @deprecate(lufact(S::SparseMatrixCSC{<:UMFVTypes,<:UMFITypes}), lu(S))
    @deprecate(lufact(A::SparseMatrixCSC{<:Union{Float16,Float32},Ti}) where {Ti<:UMFITypes}, lu(A))
    @deprecate(lufact(A::SparseMatrixCSC{<:Union{ComplexF16,ComplexF32},Ti}) where {Ti<:UMFITypes}, lu(A))
    @deprecate(lufact(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}}}) where {T<:AbstractFloat}, lu(A))
end

# deprecate qrfact to qr
@eval SuiteSparse.SPQR begin
    import LinearAlgebra: qrfact
    @deprecate(qrfact(A::SparseMatrixCSC{Tv}; tol = _default_tol(A)) where {Tv<:Union{ComplexF64,Float64}}, qr(A; tol=tol))
    @deprecate(qrfact(A::SparseMatrixCSC; tol = _default_tol(A)), qr(A; tol=tol))
end

# deprecate ldltfact to ldlt
@eval SuiteSparse.CHOLMOD begin
    import LinearAlgebra: ldltfact
    @deprecate(ldltfact(A::Sparse; shift::Real=0.0, perm::AbstractVector{SuiteSparse_long}=SuiteSparse_long[]), ldlt(A; shift=shift, perm=perm))
    @deprecate(ldltfact(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}},
                        Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
                        Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
                        Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
                        kws...) where {T<:Real},
                ldlt(A; kws...))
end

# deprecate ldltfact! to ldlt!
@eval SuiteSparse.CHOLMOD begin
    import LinearAlgebra: ldltfact!
    @deprecate(ldltfact!(F::Factor{Tv}, A::Sparse{Tv}; shift::Real=0.0) where Tv, ldlt!(F, A; shift=shift))
    @deprecate(ldltfact!(F::Factor, A::Union{SparseMatrixCSC{T},
                         SparseMatrixCSC{Complex{T}},
                         Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
                         Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
                         Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
                         shift = 0.0) where {T<:Real},
               ldlt!(F, A; shift=shift))
end

# deprecate cholfact to cholesky
@eval SuiteSparse.CHOLMOD begin
    import LinearAlgebra: cholfact
    @deprecate(cholfact(A::Sparse; shift::Real=0.0, perm::AbstractVector{SuiteSparse_long}=SuiteSparse_long[]), cholesky(A; shift=shift, perm=perm))
    @deprecate(cholfact(A::Union{SparseMatrixCSC{T}, SparseMatrixCSC{Complex{T}},
                        Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
                        Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
                        Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
                        kws...) where {T<:Real},
               cholesky(A; kws...))
end

# deprecate cholfact! to cholesky!
@eval SuiteSparse.CHOLMOD begin
    import LinearAlgebra: cholfact!
    @deprecate(cholfact!(F::Factor{Tv}, A::Sparse{Tv}; shift::Real=0.0) where Tv, cholesky!(F, A; shift=shift))
    @deprecate(cholfact!(F::Factor, A::Union{SparseMatrixCSC{T},
                        SparseMatrixCSC{Complex{T}},
                        Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
                        Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
                        Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
                        shift = 0.0) where {T<:Real},
                cholesyk!(F, A; shift=shift))
end
