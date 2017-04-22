# This file is a part of Julia. License is MIT: https://julialang.org/license

export LAPACKException,
       ARPACKException,
       SingularException,
       PosDefException,
       RankDeficientException

mutable struct LAPACKException <: Exception
    info::BlasInt
end

mutable struct ARPACKException <: Exception
    info::String
end

function ARPACKException(i::Integer)
    if i == -8
        return ARPACKException("error return from calculation of a real Schur form.")
    elseif i == -9
        return ARPACKException("error return from calculation of eigenvectors.")
    elseif i == -14
        return ARPACKException("did not find any eigenvalues to sufficient accuracy. Try with a different starting vector or more Lanczos vectors by increasing the value of ncv.")
    end
    return ARPACKException("unspecified ARPACK error: $i")
end

mutable struct SingularException <: Exception
    info::BlasInt
end

mutable struct PosDefException <: Exception
    info::BlasInt
end

mutable struct RankDeficientException <: Exception
    info::BlasInt
end
