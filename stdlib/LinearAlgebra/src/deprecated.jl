# This file is a part of Julia. License is MIT: https://julialang.org/license

# To be deprecated in 2.0
rank(A::AbstractMatrix, tol::Real) = rank(A,rtol=tol)
