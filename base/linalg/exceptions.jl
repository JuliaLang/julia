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

"""
   SingularException(info)

The matrix passed is a singular matrix. Optional argument INFO is an INTEGER > 0  
      if INFO = i, The matrix is exactly singular, and division by zero 
      will occur if it is used to solve a system of equations.
"""
struct SingularException <: Exception
    info::BlasInt
end

function Base.showerror(io::IO, ex::SingularException)
    print(io, "SingularException($ex.info): matrix is singular")
end

"""
    PosDefException(info)

Optional Argument info is an Integer. 
If info == -1: The matrix is not Hermitian.
Else: The matrix is not positive definite.


"""
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
