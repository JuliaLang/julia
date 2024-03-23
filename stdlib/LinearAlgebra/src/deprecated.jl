# This file is a part of Julia. License is MIT: https://julialang.org/license

# To be deprecated in 2.0
rank(A::AbstractMatrix, tol::Real) = rank(A,rtol=tol)
nullspace(A::AbstractVector, tol::Real) = nullspace(reshape(A, length(A), 1), rtol= tol)
nullspace(A::AbstractMatrix, tol::Real) = nullspace(A, rtol=tol)
pinv(A::AbstractMatrix{T}, tol::Real) where T = pinv(A, rtol=tol)

# To be removed in 2.0
@deprecate /(x::Number, v::AbstractVector) x*pinv(v)
@deprecate (\)(a::AbstractVector, b::AbstractArray) pinv(a) * b
