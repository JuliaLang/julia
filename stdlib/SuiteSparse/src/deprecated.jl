# This file is a part of Julia. License is MIT: https://julialang.org/license

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from src/cholmod.jl, to deprecate
@eval SuiteSparse.CHOLMOD begin
    using Base.LinAlg: Adjoint, Transpose
    Ac_ldiv_B(A::RealHermSymComplexHermF64SSL, B::StridedVecOrMat) = \(Adjoint(A), B)
    Ac_ldiv_B(L::Factor, B::Dense) = \(Adjoint(L), B)
    Ac_ldiv_B(L::Factor, B::VecOrMat) = \(Adjoint(L), B)
    Ac_ldiv_B(L::Factor, B::Sparse) = \(Adjoint(L), B)
    Ac_ldiv_B(L::Factor, B::SparseVecOrMat) = \(Adjoint(L), B)
    Ac_ldiv_B(L::FactorComponent, B) = \(Adjoint(L), B)
    Ac_ldiv_B(L::FactorComponent, B::RowVector) = \(Adjoint(L), B)
    Ac_mul_B(A::Sparse, B::Dense) = *(Adjoint(A), B)
    Ac_mul_B(A::Sparse, B::VecOrMat) =  *(Adjoint(A), B)
    Ac_mul_B(A::Sparse, B::Sparse) = *(Adjoint(A), B)
    A_mul_Bc(A::Sparse{Tv}, B::Sparse{Tv}) where {Tv<:VRealTypes} = *(A, Adjoint(B))
end
