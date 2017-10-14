# This file is a part of Julia. License is MIT: https://julialang.org/license

export LAPACKException,
       ARPACKException,
       SingularException,
       PosDefException,
       RankDeficientException

struct LAPACKException <: Exception
    info::BlasInt
end

struct ARPACKException <: Exception
    info::BlasInt
end

function Base.showerror(io::IO, ex::ARPACKException)
    print(io, "ARPACKException: ")
    if ex.info == -8
        print(io, "error return from calculation of a real Schur form.")
    elseif ex.info == -9
        print(io, "error return from calculation of eigenvectors.")
    elseif ex.info == -14
        print(io, string("did not find any eigenvalues to sufficient accuracy. ",
            "Try with a different starting vector or more Lanczos vectors ",
            "by increasing the value of ncv."))
    else
        print(io, "unspecified ARPACK error: $(ex.info)")
    end
end

struct SingularException <: Exception
    info::BlasInt
end

struct PosDefException <: Exception
    info::BlasInt
end
function Base.showerror(io::IO, ex::PosDefException)
    print(io, "PosDefException: matrix is not ")
    if ex.info == -1
        print(io, "Hermitian")
    else
        print(io, "positive definite")
    end
    print(io, "; Cholesky factorization failed.")
end

struct RankDeficientException <: Exception
    info::BlasInt
end
