# This file is a part of Julia. License is MIT: https://julialang.org/license

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
using ..SparseArrays.CHOLMOD

import Base: size
import Base.LinAlg: qr, qrfact
import ..CHOLMOD: convert, free!

struct C_Factorization{Tv<:CHOLMOD.VTypes} <: CHOLMOD.SuiteSparseStruct
    xtype::Cint
    factors::Ptr{Tv}
end

mutable struct Factorization{Tv<:CHOLMOD.VTypes} <: Base.LinAlg.Factorization{Tv}
    m::Int
    n::Int
    p::Ptr{C_Factorization{Tv}}
    function Factorization{Tv}(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}}) where Tv<:CHOLMOD.VTypes
        if p == C_NULL
            throw(ArgumentError("factorization failed for unknown reasons. Please submit a bug report."))
        end
        new(m, n, p)
    end
end
Factorization(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}}) where Tv<:CHOLMOD.VTypes = Factorization{Tv}(m, n, p)

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

function free!(F::Factorization{Tv}) where {Tv<:VTypes}
    ccall((:SuiteSparseQR_C_free, :libspqr), Cint,
        (Ptr{Ptr{C_Factorization{Tv}}}, Ptr{Void}),
            &F.p, CHOLMOD.common()) == 1
end

function backslash(ordering::Integer, tol::Real, A::Sparse{Tv}, B::Dense{Tv}) where {Tv<:VTypes}
    m, n  = size(A)
    if m != size(B, 1)
        throw(DimensionMismatch("left hand side and right hand side must have same number of rows"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_backslash, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
        (Cint, Cdouble, Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
            ordering, tol, get(A.p), get(B.p), CHOLMOD.common()))
    finalizer(d, free!)
    d
end

function factorize(ordering::Integer, tol::Real, A::Sparse{Tv}) where {Tv<:VTypes}
    s = unsafe_load(A.p)
    if s.stype != 0
        throw(ArgumentError("stype must be zero"))
    end
    f = Factorization(size(A)..., ccall((:SuiteSparseQR_C_factorize, :libspqr), Ptr{C_Factorization{Tv}},
        (Cint, Cdouble, Ptr{Sparse{Tv}}, Ptr{Void}),
            ordering, tol, get(A.p), CHOLMOD.common()))
    finalizer(f, free!)
    f
end

function solve(system::Integer, QR::Factorization{Tv}, B::Dense{Tv}) where {Tv<:VTypes}
    m, n = size(QR)
    mB = size(B, 1)
    if (system == RX_EQUALS_B || system == RETX_EQUALS_B) && m != mB
        throw(DimensionMismatch("number of rows in factorized matrix must equal number of rows in right hand side"))
    elseif (system == RTX_EQUALS_ETB || system == RTX_EQUALS_B) && n != mB
        throw(DimensionMismatch("number of columns in factorized matrix must equal number of rows in right hand side"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_solve, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
        (Cint, Ptr{C_Factorization{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
            system, get(QR.p), get(B.p), CHOLMOD.common()))
    finalizer(d, free!)
    d
end

function qmult(method::Integer, QR::Factorization{Tv}, X::Dense{Tv}) where {Tv<:VTypes}
    mQR, nQR = size(QR)
    mX, nX = size(X)
    if (method == QTX || method == QX) && mQR != mX
        throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $mX rows"))
    elseif (method == XQT || method == XQ) && mQR != nX
        throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $nX columns"))
    end
    d = Dense(ccall((:SuiteSparseQR_C_qmult, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
            (Cint, Ptr{C_Factorization{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
                method, get(QR.p), get(X.p), CHOLMOD.common()))
    finalizer(d, free!)
    d
end
    # int ordering,           // all, except 3:given treated as 0:fixed
    # double tol,             // columns with 2-norm <= tol are treated as 0
    # Long econ,              // e = max(min(m,econ),rank(A))
    # int getCTX,             // 0: Z=C (e-by-k), 1: Z=C', 2: Z=X (e-by-k)
    # cholmod_sparse *A,      // m-by-n sparse matrix to factorize
    # cholmod_sparse *Bsparse,// sparse m-by-k B
    # cholmod_dense  *Bdense, // dense  m-by-k B
    # // outputs:
    # cholmod_sparse **Zsparse,   // sparse Z
    # cholmod_dense  **Zdense,    // dense Z
    # cholmod_sparse **R,     // R factor, e-by-n
    # Long **E,               // size n column permutation, NULL if identity
    # cholmod_sparse **H,     // m-by-nh Householder vectors
    # Long **HPinv,           // size m row permutation
    # cholmod_dense **HTau,   // 1-by-nh Householder coefficients
    # cholmod_common *cc

function _qr!(ordering, tol, econ, getCTX, A::Ptr{CHOLMOD.C_Sparse{Tv}},
    Bsparse::Ptr{CHOLMOD.C_Sparse{Tv}}        = Ptr{CHOLMOD.C_Sparse{Tv}}(C_NULL),
    Bdense::Ptr{CHOLMOD.C_Dense{Tv}}          = Ptr{CHOLMOD.C_Dense{Tv}}(C_NULL),
    Zsparse::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}   = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    Zdense::Ref{Ptr{CHOLMOD.C_Dense{Tv}}}     = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL),
    R::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}         = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    E::Ref{Ptr{CHOLMOD.SuiteSparse_long}}     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}(C_NULL),
    H::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}         = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    HPinv::Ref{Ptr{CHOLMOD.SuiteSparse_long}} = Ref{Ptr{CHOLMOD.SuiteSparse_long}}(C_NULL),
    HTau::Ref{Ptr{CHOLMOD.C_Dense{Tv}}}       = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)) where Tv<:CHOLMOD.VTypes

    AA   = unsafe_load(A)
    m, n = AA.nrow, AA.ncol
    rnk  = ccall((:SuiteSparseQR_C, :libspqr), CHOLMOD.SuiteSparse_long,
        (Cint, Cdouble, CHOLMOD.SuiteSparse_long, Cint,
         Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}},
         Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}}, Ptr{Ptr{CHOLMOD.C_Dense{Tv}}}, Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}},
         Ptr{Ptr{CHOLMOD.SuiteSparse_long}}, Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}}, Ptr{Ptr{CHOLMOD.SuiteSparse_long}},
         Ptr{Ptr{CHOLMOD.C_Dense{Tv}}}, Ptr{Void}),
        ordering,       # all, except 3:given treated as 0:fixed
        tol,            # columns with 2-norm <= tol treated as 0
        econ,           # e = max(min(m,econ),rank(A))
        getCTX,         # 0: Z=C (e-by-k), 1: Z=C', 2: Z=X (e-by-k)
        A,              # m-by-n sparse matrix to factorize
        Bsparse,        # sparse m-by-k B
        Bdense,         # dense  m-by-k B
        # /* outputs: */
        Zsparse,        # sparse Z
        Zdense,         # dense Z
        R,              # e-by-n sparse matrix */
        E,              # size n column perm, NULL if identity */
        H,              # m-by-nh Householder vectors
        HPinv,          # size m row permutation
        HTau,           # 1-by-nh Householder coefficients
        CHOLMOD.common()) # /* workspace and parameters */

    if rnk < 0
        error("Sparse QR factorization failed")
    end

    e = E[]
    if e == C_NULL
        _E = Vector{CHOLMOD.SuiteSparse_long}()
    else
        _E = Vector{CHOLMOD.SuiteSparse_long}(n)
        for i in 1:n
            @inbounds _E[i] = unsafe_load(e, i) + 1
        end
        # Free memory allocated by SPQR. This call will make sure that the
        # correct deallocator function is called and that the memory count in
        # the common struct is updated
        ccall((:cholmod_l_free, :libcholmod), Void,
            (Csize_t, Cint, Ptr{CHOLMOD.SuiteSparse_long}, Ptr{Void}),
            n, sizeof(CHOLMOD.SuiteSparse_long), e, CHOLMOD.common())
    end
    hpinv = HPinv[]
    if hpinv == C_NULL
        _HPinv = Vector{CHOLMOD.SuiteSparse_long}()
    else
        _HPinv = Vector{CHOLMOD.SuiteSparse_long}(m)
        for i in 1:m
            @inbounds _HPinv[i] = unsafe_load(hpinv, i) + 1
        end
        # Free memory allocated by SPQR. This call will make sure that the
        # correct deallocator function is called and that the memory count in
        # the common struct is updated
        ccall((:cholmod_l_free, :libcholmod), Void,
            (Csize_t, Cint, Ptr{CHOLMOD.SuiteSparse_long}, Ptr{Void}),
            m, sizeof(CHOLMOD.SuiteSparse_long), hpinv, CHOLMOD.common())
    end

    return rnk, _E, _HPinv
end
# _qr{Tv<:CHOLMOD.VTypes}(Q, R, E, A::Sparse{Tv}, ordering, tol, econ) =
#     _qr!(Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(),
#         Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(),
#         Ref{Ptr{CHOLMOD.SuiteSparse_long}}(),
#         A, ordering, tol, econ)
function Base.LinAlg.qr(A::SparseMatrixCSC{Tv}, ::Type{Val{true}}; tol = size(A, 1)*eps()) where Tv <: CHOLMOD.VTypes

    R     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    E     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    H     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    HPinv = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    HTau  = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)

    r, p, hpinv = _qr!(ORDERING_DEFAULT, tol, 0, 0, CHOLMOD.Sparse(A).p,
        Ptr{CHOLMOD.C_Sparse{Tv}}(C_NULL),
        Ptr{CHOLMOD.C_Dense{Tv}}(C_NULL),
        Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
        Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL),
        R, E, H, HPinv, HTau)
    return SparseMatrixCSC(Sparse(H[])), Array(CHOLMOD.Dense(HTau[])), SparseMatrixCSC(Sparse(R[])), p, hpinv
end
# function Base.LinAlg.qr(A::SparseMatrixCSC, ::Type{Val{false}})
#     Q, R, p, r = _qr(CHOLMOD.Sparse(A), ORDERING_FIXED, 0.0, size(A, 2))
#     return SparseMatrixCSC(Q), SparseMatrixCSC(R)
# end
Base.LinAlg.qr(A::SparseMatrixCSC) = qr(A, Val{true})
# qrR(A::SparseMatrixCSC{Tv}, ::Type{Val{true}}; tol = size(A, 1)*eps()) where Tv =
#     _qr!(Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(0), Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(), Ref{Ptr{CHOLMOD.SuiteSparse_long}}(),
#         CHOLMOD.Sparse(A), ORDERING_DEFAULT, tol, 0)
# function Base.rank(A::SparseMatrixCSC; tol = size(A, 1)*eps())
#     Q, R, p, r = _qr(CHOLMOD.Sparse(A), ORDERING_DEFAULT, tol, 0)
#     return r
# end

### High level API
qrfact(A::SparseMatrixCSC, ::Type{Val{true}}) = factorize(ORDERING_DEFAULT, DEFAULT_TOL, Sparse(A, 0))

"""
    qrfact(A) -> SPQR.Factorization

Compute the `QR` factorization of a sparse matrix `A`. A fill-reducing permutation is used.
The main application of this type is to solve least squares problems with [`\\`](@ref). The function
calls the C library SPQR and a few additional functions from the library are wrapped but not
exported.
"""
qrfact(A::SparseMatrixCSC) = qrfact(A, Val(true))

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
#
# This definition is similar to the definition in factorization.jl except that
# here we have to use \ instead of A_ldiv_B! because of limitations in SPQR

## Two helper methods
_ret_size(F::Factorization, b::AbstractVector) = (size(F, 2),)
_ret_size(F::Factorization, B::AbstractMatrix) = (size(F, 2), size(B, 2))

function (\)(F::Factorization{Float64}, B::VecOrMat{Complex{Float64}})
# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      ->       |y1|y2|y3|y4|     ->      |x2|y2|     ->    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    c2r = reshape(transpose(reinterpret(Float64, B, (2, length(B)))), size(B, 1), 2*size(B, 2))
    x = F\c2r

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      <-       |y1|y2|y3|y4|     <-      |x2|y2|     <-    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    return reinterpret(Complex{Float64}, transpose(reshape(x, (length(x) >> 1), 2)), _ret_size(F, B))
end

function (\)(F::Factorization{T}, B::StridedVecOrMat{T}) where {T<:VTypes}
    QtB = qmult(QTX, F, Dense(B))
    convert(typeof(B), solve(RETX_EQUALS_B, F, QtB))
end

(\)(F::Factorization, B::StridedVecOrMat) = F\convert(AbstractArray{eltype(F)}, B)

end # module
