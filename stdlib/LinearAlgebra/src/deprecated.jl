# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

@deprecate mul!(C, A, B, α, β) addmul!(C, A, B, α, β)

# To be deprecated in 2.0
rank(A::AbstractMatrix, tol::Real) = rank(A,rtol=tol)
nullspace(A::AbstractVector, tol::Real) = nullspace(reshape(A, length(A), 1), rtol= tol)
nullspace(A::AbstractMatrix, tol::Real) = nullspace(A, rtol=tol)
pinv(A::AbstractMatrix{T}, tol::Real) where T = pinv(A, rtol=tol)
