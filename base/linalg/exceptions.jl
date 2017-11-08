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

If pivoting is enabled the factorization failed 
because the matrix passed is a singular matrix. 
Optional argument info is an Integer > 0  
if info == i, The matrix is exactly singular, and division by zero 
will occur if it is used to solve a system of equations.

Else if pivoting is not enabled the factorization failed because of zero pivot.

"""
struct SingularException <: Exception
    info::BlasInt
end

function Base.showerror(io::IO, ex::SingularException)
    print(io, "SingularException($ex.info): If pivoting is enabled the matrix is singular and hence factorization failed, 
           else the factorization failed because it hit a zero pivot.")
end

"""
    PosDefException(info)

Optional Argument info is an Integer. 
If info == -1: The matrix is not Hermitian.
Else if info == 0: The matrix is Positive Definite.
Else if info>0: The Cholesky factorization for the matrix failed at the $(info) 
                 diagonal element which indicates that the matrix is not positive definite.
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
