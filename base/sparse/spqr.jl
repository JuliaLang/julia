# This file is a part of Julia. License is MIT: http://julialang.org/license

module SPQR

import Base: \

# ordering options */
const ORDERING_FIXED   = Int32(0)
const ORDERING_NATURAL = Int32(1)
const ORDERING_COLAMD  = Int32(2)
const ORDERING_GIVEN   = Int32(3) # only used for C/C++ interface
const ORDERING_CHOLMOD = Int32(4) # CHOLMOD best-effort (COLAMD, METIS,...)
const ORDERING_AMD     = Int32(5) # AMD(A'*A)
const ORDERING_METIS   = Int32(6) # metis(A'*A)
const ORDERING_DEFAULT = Int32(7) # SuiteSparseQR default ordering
const ORDERING_BEST    = Int32(8) # try COLAMD, AMD, and METIS; pick best
const ORDERING_BESTAMD = Int32(9) # try COLAMD and AMD; pick best#

# Let [m n] = size of the matrix after pruning singletons.  The default
# ordering strategy is to use COLAMD if m <= 2*n.  Otherwise, AMD(A'A) is
# tried.  If there is a high fill-in with AMD then try METIS(A'A) and take
# the best of AMD and METIS.  METIS is not tried if it isn't installed.

# tol options
const DEFAULT_TOL = Int32(-2) # if tol <= -2, the default tol is used
const NO_TOL      = Int32(-1) # if -2 < tol < 0, then no tol is used

# for qmult, method can be 0,1,2,3:
const QTX = Int32(0)
const QX  = Int32(1)
const XQT = Int32(2)
const XQ  = Int32(3)

# system can be 0,1,2,3:  Given Q*R=A*E from SuiteSparseQR_factorize:
const RX_EQUALS_B    = Int32(0) # solve R*X=B      or X = R\B
const RETX_EQUALS_B  = Int32(1) # solve R*E'*X=B   or X = E*(R\B)
const RTX_EQUALS_B   = Int32(2) # solve R'*X=B     or X = R'\B
const RTX_EQUALS_ETB = Int32(3) # solve R'*X=E'*B  or X = R'\(E'*B)


using ..SparseArrays: SparseMatrixCSC
using ..SparseArrays.CHOLMOD: C_Dense, C_Sparse, Dense, ITypes, Sparse, VTypes, common

import Base: size
import Base.LinAlg: qrfact
import ..SparseArrays.CHOLMOD: convert, free!



immutable C_Factorization{Tv<:VTypes}
    xtype::Cint
    factors::Ptr{Tv}
end

type Factorization{Tv<:VTypes} <: Base.LinAlg.Factorization{Tv}
    m::Int
    n::Int
    p::Ptr{C_Factorization{Tv}}
    function Factorization(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}})
        if p == C_NULL
            throw(ArgumentError("factorization failed for unknown reasons. Please submit a bug report."))
        end
        new(m, n, p)
    end
end
Factorization{Tv<:VTypes}(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}}) = Factorization{Tv}(m, n, p)

size(F::Factorization) = (F.m, F.n)
function size(F::Factorization, i::Integer)
    if i < 1
        throw(ArgumentError("dimension must be positive"))
    end
    if i == 1
        return F.m
    elseif i == 2
        return F.n
    end
    return 1
end

function free!{Tv<:VTypes}(F::Factorization{Tv})
    ccall((:SuiteSparseQR_C_free, :libspqr), Cint,
        (Ptr{Ptr{C_Factorization{Tv}}}, Ptr{Void}),
            &F.p, common()) == 1
end

function backslash{Tv<:VTypes}(ordering::Integer, tol::Real, A::Sparse{Tv}, B::Dense{Tv})
    m, n  = size(A)
    if m != size(B, 1)
        throw(DimensionMismatch("left hand side and right hand side must have same number of rows"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_backslash, :libspqr), Ptr{C_Dense{Tv}},
        (Cint, Cdouble, Ptr{C_Sparse{Tv}}, Ptr{C_Dense{Tv}}, Ptr{Void}),
            ordering, tol, A.p, B.p, common()))
    finalizer(d, free!)
    d
end

function factorize{Tv<:VTypes}(ordering::Integer, tol::Real, A::Sparse{Tv})
    s = unsafe_load(A.p)
    if s.stype != 0
        throw(ArgumentError("stype must be zero"))
    end
    f = Factorization(size(A)..., ccall((:SuiteSparseQR_C_factorize, :libspqr), Ptr{C_Factorization{Tv}},
        (Cint, Cdouble, Ptr{Sparse{Tv}}, Ptr{Void}),
            ordering, tol, A.p, common()))
    finalizer(f, free!)
    f
end

function solve{Tv<:VTypes}(system::Integer, QR::Factorization{Tv}, B::Dense{Tv})
    m, n = size(QR)
    mB = size(B, 1)
    if (system == RX_EQUALS_B || system == RETX_EQUALS_B) && m != mB
        throw(DimensionMismatch("number of rows in factorized matrix must equal number of rows in right hand side"))
    elseif (system == RTX_EQUALS_ETB || system == RTX_EQUALS_B) && n != mB
        throw(DimensionMismatch("number of columns in factorized matrix must equal number of rows in right hand side"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_solve, :libspqr), Ptr{C_Dense{Tv}},
        (Cint, Ptr{C_Factorization{Tv}}, Ptr{C_Dense{Tv}}, Ptr{Void}),
            system, QR.p, B.p, common()))
    finalizer(d, free!)
    d
end

function qmult{Tv<:VTypes}(method::Integer, QR::Factorization{Tv}, X::Dense{Tv})
    mQR, nQR = size(QR)
    mX, nX = size(X)
    if (method == QTX || method == QX) && mQR != mX
        throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $mX rows"))
    elseif (method == XQT || method == XQ) && mQR != nX
        throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $nX columns"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_qmult, :libspqr), Ptr{C_Dense{Tv}},
            (Cint, Ptr{C_Factorization{Tv}}, Ptr{C_Dense{Tv}}, Ptr{Void}),
                method, QR.p, X.p, common()))
    finalizer(d, free!)
    d
end

qrfact(A::SparseMatrixCSC) = factorize(ORDERING_DEFAULT, DEFAULT_TOL, Sparse(A, 0))

function (\){T}(F::Factorization{T}, B::StridedVecOrMat{T})
    QtB = qmult(QTX, F, Dense(B))
    convert(typeof(B), solve(RETX_EQUALS_B, F, QtB))
end
(\){T,S}(F::Factorization{T}, B::StridedVecOrMat{S}) = F\convert(AbstractArray{T}, B)

end # module
