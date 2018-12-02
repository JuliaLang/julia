# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

# 2.0 deprecations
function rank(A::AbstractMatrix, tol::Real)
    depwarn(string("`rank` without any keywords has been deprecated. Consider using ",
        "the keywords `rtol` or `atol` instead."), :rank)
    rank(A,rtol=tol)
end
