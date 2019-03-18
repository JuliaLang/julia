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
    SingularException

Exception thrown when the input matrix has one or more zero-valued eigenvalues, and is not invertible.
A linear solve involving such a matrix cannot be computed.
The `info` field indicates the location of (one of) the singular value(s).
"""
struct SingularException <: Exception
    info::BlasInt
end

"""
    PosDefException

Exception thrown when the input matrix was not [positive definite](https://en.wikipedia.org/wiki/Definiteness_of_a_matrix).
Some linear algebra functions and factorizations are only applicable to positive definite matrices.
The `info` field indicates the location of (one of) the eigenvalue(s) which is (are) less than/equal to 0.
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
