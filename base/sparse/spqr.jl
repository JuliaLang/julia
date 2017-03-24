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
# import ..CHOLMOD: convert, free!

# struct C_Factorization{Tv<:CHOLMOD.VTypes} <: CHOLMOD.SuiteSparseStruct
#     xtype::Cint
#     factors::Ptr{Tv}
# end

# mutable struct Factorization{Tv<:CHOLMOD.VTypes} <: Base.LinAlg.Factorization{Tv}
#     m::Int
#     n::Int
#     p::Ptr{C_Factorization{Tv}}
#     function Factorization{Tv}(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}}) where {Tv<:CHOLMOD.VTypes}
#         if p == C_NULL
#             throw(ArgumentError("factorization failed for unknown reasons. Please submit a bug report."))
#         end
#         new(m, n, p)
#     end
# end
# Factorization(m::Integer, n::Integer, p::Ptr{C_Factorization{Tv}}) where {Tv<:CHOLMOD.VTypes} = Factorization{Tv}(m, n, p)

# size(F::Factorization) = (F.m, F.n)
# function size(F::Factorization, i::Integer)
#     if i < 1
#         throw(ArgumentError("dimension must be positive"))
#     end
#     if i == 1
#         return F.m
#     elseif i == 2
#         return F.n
#     end
#     return 1
# end

# function free!(F::Factorization{Tv}) where {Tv<:CHOLMOD.VTypes}
#     ccall((:SuiteSparseQR_C_free, :libspqr), Cint,
#         (Ptr{Ptr{C_Factorization{Tv}}}, Ptr{Void}),
#             &F.p, CHOLMOD.common()) == 1
# end

# function backslash(ordering::Integer, tol::Real, A::Sparse{Tv}, B::Dense{Tv}) where {Tv<:CHOLMOD.VTypes}
#     m, n  = size(A)
#     if m != size(B, 1)
#         throw(DimensionMismatch("left hand side and right hand side must have same number of rows"))
#     end
#     d = Dense(ccall((:SuiteSparseQR_C_backslash, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
#         (Cint, Cdouble, Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
#             ordering, tol, get(A.p), get(B.p), CHOLMOD.common()))
#     finalizer(d, free!)
#     d
# end

# function factorize(ordering::Integer, tol::Real, A::Sparse{Tv}) where {Tv<:CHOLMOD.VTypes}
#     s = unsafe_load(A.p)
#     if s.stype != 0
#         throw(ArgumentError("stype must be zero"))
#     end
#     f = Factorization(size(A)..., ccall((:SuiteSparseQR_C_factorize, :libspqr), Ptr{C_Factorization{Tv}},
#         (Cint, Cdouble, Ptr{Sparse{Tv}}, Ptr{Void}),
#             ordering, tol, get(A.p), CHOLMOD.common()))
#     finalizer(f, free!)
#     f
# end

# function solve(system::Integer, QR::Factorization{Tv}, B::Dense{Tv}) where {Tv<:CHOLMOD.VTypes}
#     m, n = size(QR)
#     mB = size(B, 1)
#     if (system == RX_EQUALS_B || system == RETX_EQUALS_B) && m != mB
#         throw(DimensionMismatch("number of rows in factorized matrix must equal number of rows in right hand side"))
#     elseif (system == RTX_EQUALS_ETB || system == RTX_EQUALS_B) && n != mB
#         throw(DimensionMismatch("number of columns in factorized matrix must equal number of rows in right hand side"))
#     end
#     d = Dense(ccall((:SuiteSparseQR_C_solve, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
#         (Cint, Ptr{C_Factorization{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
#             system, get(QR.p), get(B.p), CHOLMOD.common()))
#     finalizer(d, free!)
#     d
# end

# function qmult(method::Integer, QR::Factorization{Tv}, X::Dense{Tv}) where {Tv<:CHOLMOD.VTypes}
#     mQR, nQR = size(QR)
#     mX, nX = size(X)
#     if (method == QTX || method == QX) && mQR != mX
#         throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $mX rows"))
#     elseif (method == XQT || method == XQ) && mQR != nX
#         throw(DimensionMismatch("Q matrix size $mQR and dense matrix has $nX columns"))
#     end
#     d = Dense(ccall((:SuiteSparseQR_C_qmult, :libspqr), Ptr{CHOLMOD.C_Dense{Tv}},
#             (Cint, Ptr{C_Factorization{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}}, Ptr{Void}),
#                 method, get(QR.p), get(X.p), CHOLMOD.common()))
#     finalizer(d, free!)
#     d
# end

function _qr!(ordering, tol, econ, getCTX, A::Ptr{CHOLMOD.C_Sparse{Tv}},
    Bsparse::Ptr{CHOLMOD.C_Sparse{Tv}}        = Ptr{CHOLMOD.C_Sparse{Tv}}(C_NULL),
    Bdense::Ptr{CHOLMOD.C_Dense{Tv}}          = Ptr{CHOLMOD.C_Dense{Tv}}(C_NULL),
    Zsparse::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}   = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    Zdense::Ref{Ptr{CHOLMOD.C_Dense{Tv}}}     = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL),
    R::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}         = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    E::Ref{Ptr{CHOLMOD.SuiteSparse_long}}     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}(C_NULL),
    H::Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}         = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
    HPinv::Ref{Ptr{CHOLMOD.SuiteSparse_long}} = Ref{Ptr{CHOLMOD.SuiteSparse_long}}(C_NULL),
    HTau::Ref{Ptr{CHOLMOD.C_Dense{Tv}}}       = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)) where {Tv<:CHOLMOD.VTypes}

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

# Such that A[invperm(p), q] = (I - H[:,1]*τ[1]*H[:,1]')*...*(I - H[:,k]*τ[k]*H[:,k]')*R
# with k=size(H,2).
struct QRSparse{Tv,Ti} <: LinAlg.Factorization{Tv}
    factors::SparseMatrixCSC{Tv,Ti}
    τ::Vector{Tv}
    R::SparseMatrixCSC{Tv,Ti}
    p::Vector{Ti}
    q::Vector{Ti}
end

Base.size(F::QRSparse) = (size(F.factors, 1), size(F.R, 2))
function Base.size(F::QRSparse, i::Integer)
    if i == 1
        return size(F.factors, 1)
    elseif i == 2
        return size(F.R, 2)
    elseif i > 2
        return 1
    else
        throw(ArgumentError("second argument must be 1 or 2"))
    end
end

struct QRSparseQ{Tv<:CHOLMOD.VTypes,Ti<:Integer} <: LinAlg.AbstractQ{Tv}
    factors::SparseMatrixCSC{Tv,Ti}
    τ::Vector{Tv}
end

Base.size(Q::QRSparseQ) = (size(Q.factors, 1), size(Q.factors, 1))

function Base.LinAlg.qrfact(A::SparseMatrixCSC{Tv}, ::Type{Val{true}}; tol = size(A, 1)*eps()) where {Tv <: CHOLMOD.VTypes}

    R     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    E     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    H     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    HPinv = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    HTau  = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)

    # SPQR doesn't accept symmetric matrices so we explicitly call the Sparse constructor with 0
    r, p, hpinv = _qr!(ORDERING_DEFAULT, tol, 0, 0, CHOLMOD.Sparse(A, 0).p,
        Ptr{CHOLMOD.C_Sparse{Tv}}(C_NULL),
        Ptr{CHOLMOD.C_Dense{Tv}}(C_NULL),
        Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
        Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL),
        R, E, H, HPinv, HTau)
    return QRSparse(SparseMatrixCSC(Sparse(H[])), vec(Array(CHOLMOD.Dense(HTau[]))), SparseMatrixCSC(Sparse(R[])), p, hpinv)
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

function Base.A_mul_B!(Q::QRSparseQ, A::StridedVecOrMat)
    if size(A, 1) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    for l in size(Q.factors, 2):-1:1
        τl = -Q.τ[l]'
        h = view(Q.factors, :, l)
        for j in 1:size(A, 2)
            a = view(A, :, j)
            LinAlg.axpy!(τl*dot(h, a), h, a)
        end
    end
    return A
end

function Base.A_mul_B!(A::StridedMatrix, Q::QRSparseQ)
    if size(A, 2) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    tmp = similar(A, size(A, 1))
    for l in 1:size(Q.factors, 2)
        τl = -Q.τ[l]'
        h = view(Q.factors, :, l)
        A_mul_B!(tmp, A, h)
        LinAlg.lowrankupdate!(A, tmp, h, τl)
    end
    return A
end

function Base.Ac_mul_B!(Q::QRSparseQ, A::StridedVecOrMat)
    if size(A, 1) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    for l in 1:size(Q.factors, 2)
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        for j in 1:size(A, 2)
            a = view(A, :, j)
            LinAlg.axpy!(τl'*dot(h, a), h, a)
        end
    end
    return A
end

function Base.A_mul_Bc!(A::StridedMatrix, Q::QRSparseQ)
    if size(A, 2) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    tmp = similar(A, size(A, 1))
    for l in size(Q.factors, 2):-1:1
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        A_mul_B!(tmp, A, h)
        LinAlg.lowrankupdate!(A, tmp, h, τl)
    end
    return A
end

# function qrfact2(A::SparseMatrixCSC{Tv}; tol = size(A, 1)*eps()) where {Tv <: CHOLMOD.VTypes}
#     R     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
#     E     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
#     H     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
#     HPinv = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
#     HTau  = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)

#     r, q, p = _qr!(ORDERING_DEFAULT, tol, 0, 0, CHOLMOD.Sparse(A).p,
#         Ptr{CHOLMOD.C_Sparse{Tv}}(C_NULL),
#         Ptr{CHOLMOD.C_Dense{Tv}}(C_NULL),
#         Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}(C_NULL),
#         Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL),
#         R, E, H, HPinv, HTau)
#     QRSparse(SparseMatrixCSC(Sparse(H[])), vec(Array(CHOLMOD.Dense(HTau[]))),
#              SparseMatrixCSC(Sparse(R[])), p, q)
# end

Base.LinAlg.getq(F::QRSparse) = QRSparseQ(F.factors, F.τ)

function Base.getindex(F::QRSparse, k::Symbol)
    if k == :Q
        return LinAlg.getq(F)
    else
        throw(ArgumentError("k must be :Q"))
    end
end

### High level API
# qrfact(A::SparseMatrixCSC, ::Type{Val{true}}) = factorize(ORDERING_DEFAULT, DEFAULT_TOL, Sparse(A, 0))

# """
#     qrfact(A) -> SPQR.Factorization

# Compute the `QR` factorization of a sparse matrix `A`. A fill-reducing permutation is used.
# The main application of this type is to solve least squares problems with [`\\`](@ref). The function
# calls the C library SPQR and a few additional functions from the library are wrapped but not
# exported.
# """
qrfact(A::SparseMatrixCSC) = qrfact(A, Val{true})

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
#
# This definition is similar to the definition in factorization.jl except that
# here we have to use \ instead of A_ldiv_B! because of limitations in SPQR

## Two helper methods
_ret_size(F::QRSparse, b::AbstractVector) = (size(F, 2),)
_ret_size(F::QRSparse, B::AbstractMatrix) = (size(F, 2), size(B, 2))

function (\)(F::QRSparse{Float64}, B::VecOrMat{Complex{Float64}})
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

# function (\)(F::Factorization{T}, B::StridedVecOrMat{T}) where {T<:CHOLMOD.VTypes}
#     QtB = qmult(QTX, F, Dense(B))
#     convert(typeof(B), solve(RETX_EQUALS_B, F, QtB))
# end

# (\)(F::Factorization, B::StridedVecOrMat) = F\convert(AbstractArray{eltype(F)}, B)

# function (\)(F::QRSparse{T}, B::StridedVecOrMat{T}) where {T<:CHOLMOD.VTypes}
#     rnk = maximum(F.R.rowval)
#     x0  = LinAlg.getq(F)'*B[invperm(F.q)]
#     A_ldiv_B!(UpperTriangular(F.R[Base.OneTo(rnk),Base.OneTo(rnk)]), view(x0, Base.OneTo(rnk)))
#     x0[rnk + 1:size(F.R,2),:] = 0
#     convert(typeof(B), x0[invperm(F.p)])
# end

function _ldiv_basic(F::QRSparse, B::StridedVecOrMat)
    if size(F, 1) != size(B, 1)
        throw(DimensionMismatch("size(F) = $(size(F)) but size(B) = $(size(B))"))
    end
    # The rank of F equal to the number of rows in R
    rnk = size(F.R,1)
    # allocate an array for the return value large enough to hold B and X
    # For overdetermined problem, B is larger than X and vice versa
    X   = similar(B, ntuple(i -> i == 1 ? max(size(F, 2), size(B, 1)) : size(B, 2), Val{ndims(B)}))
    # Fill will zeros. These will eventually become the zeros in the basic solution
    # fill!(X, 0)
    # Apply left permutation to the solution and store in X
    for j in 1:size(B, 2)
        for i in 1:length(F.q)
            @inbounds X[F.q[i], j] = B[i, j]
        end
    end
    # Make a view into x correstonding to the size of b
    X0 = view(X, 1:size(B, 1), :)
    # Apply Q' to B
    Ac_mul_B!(LinAlg.getq(F), X0)
    # Zero out to get basic solution
    X[rnk + 1:end, :] = 0
    # Solve R*X = B
    A_ldiv_B!(UpperTriangular(view(F.R, :, Base.OneTo(rnk))), view(X0, Base.OneTo(rnk), :))
    # Apply right permutation and extract solution from x
    return getindex(X, ntuple(i -> i == 1 ? invperm(F.p) : :, Val{ndims(B)})...)
end
# function _ldiv_basic(F::QRSparse, B::StridedMatrix)
#     rnk = size(F.R,1)
#     x0  = Ac_mul_B!(LinAlg.getq(F), B[invperm(F.q),:])
#     A_ldiv_B!(UpperTriangular(view(F.R, :, Base.OneTo(rnk))), view(x0, Base.OneTo(rnk), :))
#     x0[rnk + 1:end, :] = 0
#     return x0[invperm(F.p),:]
# end
(\)(F::QRSparse{T}, B::StridedVecOrMat{T}) where {T} = _ldiv_basic(F, B)
(\)(F::QRSparse, B::StridedVecOrMat) = F\convert(AbstractArray{eltype(F)}, B)


# inv(A'A)*A'b
# Pl*A*Pr = Q*R
# A = Pl'*Q*R*Pr'
# inv(Pr*R'R*Pr')*Pr*R'*Q'*Pl*b
# Pr*inv(R)*Q'*Pl*b

# A*x = b
# Pl'*Q*R*Pr'*x = b
# R*pr'*x = Q'*Pl*b
# pr'*x = inv(R)*Q'*Pl*b

# [R0 R1]*[x0;x1] = b
# R0*x0 + R1*x1 = b
# x0 + inv(R0)*R1*x1 = inv(R0)*b
# R0*inv(R0)*(b - R1*x1) + R1*x1 = b

# x0'x0 + x1'x1
# (inv(R0)*(b - R1*x1))'(inv(R0)*(b - R1*x1)) + x1'x1
# (b - R1*x1)'*inv(R0)'inv(R0)*(b - R1*x1) + x1'x1
# b'inv(R0)'inv(R0)*b + x1'R1'*inv(R0)'inv(R0)*R1*x1 -2*x1'R1*inv(R0)'inv(R0)*b + x1'x1

# (R1'inv(R0)'*inv(R0)*R1 + I)*x1 = R1*inv(R0)'inv(R0)*b
# (R2'R2 + I)*x1 = R2'b2
# x1 = inv(R2'R2 + I)*R2'b2
# x0 = b2 - R2*x1

end # module
