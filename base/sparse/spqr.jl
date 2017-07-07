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

using ..SparseArrays: SparseMatrixCSC
using ..SparseArrays.CHOLMOD

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

# Such that A[invperm(p), q] = (I - H[:,1]*τ[1]*H[:,1]')*...*(I - H[:,k]*τ[k]*H[:,k]')*R
# with k=size(H,2).
struct QRSparse{Tv,Ti} <: LinAlg.Factorization{Tv}
    factors::SparseMatrixCSC{Tv,Ti}
    τ::Vector{Tv}
    R::SparseMatrixCSC{Tv,Ti}
    cpiv::Vector{Ti}
    rpivinv::Vector{Ti}
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

# From SPQR manula p. 6
_default_tol(A::SparseMatrixCSC) =
    20*sum(size(A))*eps(real(eltype(A)))*maximum(norm(view(A, :, i))^2 for i in 1:size(A, 2))

function Base.LinAlg.qrfact(A::SparseMatrixCSC{Tv}, ::Type{Val{true}}; tol = _default_tol(A)) where {Tv <: CHOLMOD.VTypes}
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

"""
    qrfact(A) -> QRSparse

Compute the `QR` factorization of a sparse matrix `A`. Fill-reducing row and column permutations
are used such that `F[:R] = F[:Q]'*A[F[:prow],F[:pcol]]`. The main application of this type is to
solve least squares or underdetermined problems with [`\\`](@ref). The function calls the C library SPQR.

# Examples
```jldoctest
julia> A = sparse([1,2,3,4], [1,1,2,2], ones(4))
4×2 SparseMatrixCSC{Float64,Int64} with 4 stored entries:
  [1, 1]  =  1.0
  [2, 1]  =  1.0
  [3, 2]  =  1.0
  [4, 2]  =  1.0

julia> qrfact(A)
Base.SparseArrays.SPQR.QRSparse{Float64,Int64}(
  [1, 1]  =  1.0
  [4, 1]  =  0.414214
  [2, 2]  =  1.0
  [3, 2]  =  0.414214, [1.70711, 1.70711],
  [1, 1]  =  -1.41421
  [2, 2]  =  -1.41421, [1, 2], [1, 4, 2, 3])
```
"""
Base.LinAlg.qrfact(A::SparseMatrixCSC; tol = _default_tol(A)) = qrfact(A, Val{true}, tol = tol)

Base.LinAlg.qr(A::SparseMatrixCSC; tol = _default_tol(A)) = qr(A, Val{true}, tol = tol)

function Base.A_mul_B!(Q::QRSparseQ, A::StridedVecOrMat)
    if size(A, 1) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    for l in size(Q.factors, 2):-1:1
        τl = -Q.τ[l]
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
        τl = -Q.τ[l]
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
        LinAlg.lowrankupdate!(A, tmp, h, τl')
    end
    return A
end

Base.LinAlg.getq(F::QRSparse) = QRSparseQ(F.factors, F.τ)

"""
    getindex
"""
function Base.getindex(F::QRSparse, d::Symbol)
    if d == :Q
        return LinAlg.getq(F)
    elseif d == :R
        return F.R
    elseif d == :prow
        return invperm(F.rpivinv)
    elseif d == :pcol
        return F.cpiv
    else
        throw(KeyError(d))
    end
end

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
        for i in 1:length(F.rpivinv)
            @inbounds X[F.rpivinv[i], j] = B[i, j]
        end
    end

    # Make a view into x corresponding to the size of B
    X0 = view(X, 1:size(B, 1), :)

    # Apply Q' to B
    Ac_mul_B!(LinAlg.getq(F), X0)

    # Zero out to get basic solution
    X[rnk + 1:end, :] = 0

    # Solve R*X = B
    A_ldiv_B!(UpperTriangular(view(F.R, :, Base.OneTo(rnk))), view(X0, Base.OneTo(rnk), :))

    # Apply right permutation and extract solution from x
    return getindex(X, ntuple(i -> i == 1 ? invperm(F.cpiv) : :, Val{ndims(B)})...)
end

(\)(F::QRSparse{T}, B::StridedVecOrMat{T}) where {T} = _ldiv_basic(F, B)
"""
    (\)(F::QRSparse, B::StridedVecOrMat)

Solve the least squares problem ``\min\|Ax - b\|^2`` or the linear system of equations
``Ax=b`` when `F` is the sparse QR factorization of ``A``. A basic solution is returned
when the problem is underdetermined.

# Examples
```jldoctest
julia> A = sparse([1,2,4], [1,1,1], ones(3), 4, 2)
4×2 SparseMatrixCSC{Float64,Int64} with 3 stored entries:
  [1, 1]  =  1.0
  [2, 1]  =  1.0
  [4, 1]  =  1.0

julia> qrfact(A)\ones(4)
2-element Array{Float64,1}:
 1.0
 0.0
```
"""
(\)(F::QRSparse, B::StridedVecOrMat) = F\convert(AbstractArray{eltype(F)}, B)

end # module
