# This file is a part of Julia. License is MIT: https://julialang.org/license

export LAPACKException,
       SingularException,
       PosDefException,
       RankDeficientException,
       ZeroPivotException

struct LAPACKException <: Exception
    info::BlasInt
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

"""
    ZeroPivotException <: Exception

Exception thrown when a matrix factorization/solve encounters a zero in a pivot (diagonal)
position and cannot proceed.  This may *not* mean that the matrix is singular:
it may be fruitful to switch to a diffent factorization such as pivoted LU
that can re-order variables to eliminate spurious zero pivots.
The `info` field indicates the location of (one of) the zero pivot(s).
"""
struct ZeroPivotException <: Exception
    info::BlasInt
end
function Base.showerror(io::IO, ex::ZeroPivotException)
    print(io, "ZeroPivotException: factorization encountered one or more zero pivots. Consider switching to a pivoted LU factorization.")
end