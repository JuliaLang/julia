# This file is a part of Julia. License is MIT: https://julialang.org/license

# To be deprecated in 2.0
rank(A::AbstractMatrix, tol::Real) = rank(A,reltol=tol)
nullspace(A::AbstractVector, tol::Real) = nullspace(reshape(A, length(A), 1), reltol= tol)
nullspace(A::AbstractMatrix, tol::Real) = nullspace(A, reltol=tol)
pinv(A::AbstractMatrix{T}, tol::Real) where T = pinv(A, reltol=tol)
