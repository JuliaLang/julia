# This file is a part of Julia. License is MIT: https://julialang.org/license

module SPQR
using SuiteSparse_jll

import Base: \
using Base: require_one_based_indexing
using LinearAlgebra

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
# the best of AMD and METIS. METIS is not tried if it isn't installed.

using SparseArrays
using SparseArrays: getcolptr
using ..SuiteSparse.CHOLMOD
using ..SuiteSparse.CHOLMOD: change_stype!, free!

function _qr!(ordering::Integer, tol::Real, econ::Integer, getCTX::Integer,
        A::Sparse{Tv},
        Bsparse::Union{Sparse{Tv}                      , Ptr{Cvoid}} = C_NULL,
        Bdense::Union{Dense{Tv}                        , Ptr{Cvoid}} = C_NULL,
        Zsparse::Union{Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}  , Ptr{Cvoid}} = C_NULL,
        Zdense::Union{Ref{Ptr{CHOLMOD.C_Dense{Tv}}}    , Ptr{Cvoid}} = C_NULL,
        R::Union{Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}        , Ptr{Cvoid}} = C_NULL,
        E::Union{Ref{Ptr{CHOLMOD.SuiteSparse_long}}    , Ptr{Cvoid}} = C_NULL,
        H::Union{Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}        , Ptr{Cvoid}} = C_NULL,
        HPinv::Union{Ref{Ptr{CHOLMOD.SuiteSparse_long}}, Ptr{Cvoid}} = C_NULL,
        HTau::Union{Ref{Ptr{CHOLMOD.C_Dense{Tv}}}      , Ptr{Cvoid}} = C_NULL) where {Tv<:CHOLMOD.VTypes}

    AA   = unsafe_load(pointer(A))
    m, n = AA.nrow, AA.ncol
    rnk  = ccall((:SuiteSparseQR_C, libspqr), CHOLMOD.SuiteSparse_long,
        (Cint, Cdouble, CHOLMOD.SuiteSparse_long, Cint,
         Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Sparse{Tv}}, Ptr{CHOLMOD.C_Dense{Tv}},
         Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}}, Ptr{Ptr{CHOLMOD.C_Dense{Tv}}}, Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}},
         Ptr{Ptr{CHOLMOD.SuiteSparse_long}}, Ptr{Ptr{CHOLMOD.C_Sparse{Tv}}}, Ptr{Ptr{CHOLMOD.SuiteSparse_long}},
         Ptr{Ptr{CHOLMOD.C_Dense{Tv}}}, Ptr{Cvoid}),
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
        CHOLMOD.common_struct[Threads.threadid()]) # /* workspace and parameters */

    if rnk < 0
        error("Sparse QR factorization failed")
    end

    e = E[]
    if e == C_NULL
        _E = Vector{CHOLMOD.SuiteSparse_long}()
    else
        _E = Vector{CHOLMOD.SuiteSparse_long}(undef, n)
        for i in 1:n
            @inbounds _E[i] = unsafe_load(e, i) + 1
        end
        # Free memory allocated by SPQR. This call will make sure that the
        # correct deallocator function is called and that the memory count in
        # the common struct is updated
        ccall((:cholmod_l_free, libcholmod), Cvoid,
            (Csize_t, Cint, Ptr{CHOLMOD.SuiteSparse_long}, Ptr{Cvoid}),
            n, sizeof(CHOLMOD.SuiteSparse_long), e, CHOLMOD.common_struct[Threads.threadid()])
    end
    hpinv = HPinv[]
    if hpinv == C_NULL
        _HPinv = Vector{CHOLMOD.SuiteSparse_long}()
    else
        _HPinv = Vector{CHOLMOD.SuiteSparse_long}(undef, m)
        for i in 1:m
            @inbounds _HPinv[i] = unsafe_load(hpinv, i) + 1
        end
        # Free memory allocated by SPQR. This call will make sure that the
        # correct deallocator function is called and that the memory count in
        # the common struct is updated
        ccall((:cholmod_l_free, libcholmod), Cvoid,
            (Csize_t, Cint, Ptr{CHOLMOD.SuiteSparse_long}, Ptr{Cvoid}),
            m, sizeof(CHOLMOD.SuiteSparse_long), hpinv, CHOLMOD.common_struct[Threads.threadid()])
    end

    return rnk, _E, _HPinv
end

# Struct for storing sparse QR from SPQR such that
# A[invperm(rpivinv), cpiv] = (I - factors[:,1]*τ[1]*factors[:,1]')*...*(I - factors[:,k]*τ[k]*factors[:,k]')*R
# with k = size(factors, 2).
struct QRSparse{Tv,Ti} <: LinearAlgebra.Factorization{Tv}
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
        throw(ArgumentError("second argument must be positive"))
    end
end
Base.axes(F::QRSparse) = map(Base.OneTo, size(F))

struct QRSparseQ{Tv<:CHOLMOD.VTypes,Ti<:Integer} <: LinearAlgebra.AbstractQ{Tv}
    factors::SparseMatrixCSC{Tv,Ti}
    τ::Vector{Tv}
    n::Int # Number of columns in original matrix
end

Base.size(Q::QRSparseQ) = (size(Q.factors, 1), size(Q.factors, 1))
Base.axes(Q::QRSparseQ) = map(Base.OneTo, size(Q))

Matrix{T}(Q::QRSparseQ) where {T} = lmul!(Q, Matrix{T}(I, size(Q, 1), min(size(Q, 1), Q.n)))

# From SPQR manual p. 6
_default_tol(A::SparseMatrixCSC) =
    20*sum(size(A))*eps(real(eltype(A)))*maximum(norm(view(A, :, i)) for i in 1:size(A, 2))

"""
    qr(A) -> QRSparse

Compute the `QR` factorization of a sparse matrix `A`. Fill-reducing row and column permutations
are used such that `F.R = F.Q'*A[F.prow,F.pcol]`. The main application of this type is to
solve least squares or underdetermined problems with [`\\`](@ref). The function calls the C library SPQR.

!!! note
    `qr(A::SparseMatrixCSC)` uses the SPQR library that is part of SuiteSparse.
    As this library only supports sparse matrices with [`Float64`](@ref) or
    `ComplexF64` elements, as of Julia v1.4 `qr` converts `A` into a copy that is
    of type `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}` as appropriate.

# Examples
```jldoctest
julia> A = sparse([1,2,3,4], [1,1,2,2], [1.0,1.0,1.0,1.0])
4×2 SparseMatrixCSC{Float64,Int64} with 4 stored entries:
  [1, 1]  =  1.0
  [2, 1]  =  1.0
  [3, 2]  =  1.0
  [4, 2]  =  1.0

julia> qr(A)
Base.SparseArrays.SPQR.QRSparse{Float64,Int64}
Q factor:
4×4 Base.SparseArrays.SPQR.QRSparseQ{Float64,Int64}:
 -0.707107   0.0        0.0       -0.707107
  0.0       -0.707107  -0.707107   0.0
  0.0       -0.707107   0.707107   0.0
 -0.707107   0.0        0.0        0.707107
R factor:
2×2 SparseMatrixCSC{Float64,Int64} with 2 stored entries:
  [1, 1]  =  -1.41421
  [2, 2]  =  -1.41421
Row permutation:
4-element Array{Int64,1}:
 1
 3
 4
 2
Column permutation:
2-element Array{Int64,1}:
 1
 2
```
"""
function LinearAlgebra.qr(A::SparseMatrixCSC{Tv}; tol=_default_tol(A)) where {Tv <: CHOLMOD.VTypes}
    R     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    E     = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    H     = Ref{Ptr{CHOLMOD.C_Sparse{Tv}}}()
    HPinv = Ref{Ptr{CHOLMOD.SuiteSparse_long}}()
    HTau  = Ref{Ptr{CHOLMOD.C_Dense{Tv}}}(C_NULL)

    # SPQR doesn't accept symmetric matrices so we explicitly set the stype
    r, p, hpinv = _qr!(ORDERING_DEFAULT, tol, 0, 0, Sparse(A, 0),
        C_NULL, C_NULL, C_NULL, C_NULL,
        R, E, H, HPinv, HTau)

    R_ = SparseMatrixCSC(Sparse(R[]))
    return QRSparse(SparseMatrixCSC(Sparse(H[])),
                    vec(Array(CHOLMOD.Dense(HTau[]))),
                    SparseMatrixCSC(min(size(A)...),
                                    size(R_, 2),
                                    getcolptr(R_),
                                    rowvals(R_),
                                    nonzeros(R_)),
                    p, hpinv)
end
LinearAlgebra.qr(A::SparseMatrixCSC{<:Union{Float16,Float32}}; tol=_default_tol(A)) =
    qr(convert(SparseMatrixCSC{Float64}, A); tol=tol)
LinearAlgebra.qr(A::SparseMatrixCSC{<:Union{ComplexF16,ComplexF32}}; tol=_default_tol(A)) =
    qr(convert(SparseMatrixCSC{ComplexF64}, A); tol=tol)
LinearAlgebra.qr(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}}};
   tol=_default_tol(A)) where {T<:AbstractFloat} =
    throw(ArgumentError(string("matrix type ", typeof(A), "not supported. ",
    "Try qr(convert(SparseMatrixCSC{Float64/ComplexF64,Int}, A)) for ",
    "sparse floating point QR using SPQR or qr(Array(A)) for generic ",
    "dense QR.")))
LinearAlgebra.qr(A::SparseMatrixCSC; tol=_default_tol(A)) = qr(float(A); tol=tol)

function LinearAlgebra.lmul!(Q::QRSparseQ, A::StridedVecOrMat)
    if size(A, 1) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    for l in size(Q.factors, 2):-1:1
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        for j in 1:size(A, 2)
            a = view(A, :, j)
            LinearAlgebra.axpy!(τl*dot(h, a), h, a)
        end
    end
    return A
end

function LinearAlgebra.rmul!(A::StridedMatrix, Q::QRSparseQ)
    if size(A, 2) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    tmp = similar(A, size(A, 1))
    for l in 1:size(Q.factors, 2)
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        LinearAlgebra.mul!(tmp, A, h)
        LinearAlgebra.lowrankupdate!(A, tmp, h, τl)
    end
    return A
end

function LinearAlgebra.lmul!(adjQ::Adjoint{<:Any,<:QRSparseQ}, A::StridedVecOrMat)
    Q = adjQ.parent
    if size(A, 1) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    for l in 1:size(Q.factors, 2)
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        for j in 1:size(A, 2)
            a = view(A, :, j)
            LinearAlgebra.axpy!(τl'*dot(h, a), h, a)
        end
    end
    return A
end

function LinearAlgebra.rmul!(A::StridedMatrix, adjQ::Adjoint{<:Any,<:QRSparseQ})
    Q = adjQ.parent
    if size(A, 2) != size(Q, 1)
        throw(DimensionMismatch("size(Q) = $(size(Q)) but size(A) = $(size(A))"))
    end
    tmp = similar(A, size(A, 1))
    for l in size(Q.factors, 2):-1:1
        τl = -Q.τ[l]
        h = view(Q.factors, :, l)
        LinearAlgebra.mul!(tmp, A, h)
        LinearAlgebra.lowrankupdate!(A, tmp, h, τl')
    end
    return A
end

"""
    getproperty(F::QRSparse, d::Symbol)

Extract factors of a QRSparse factorization. Possible values of `d` are
- `Q` : `QRSparseQ` matrix of the ``Q`` factor in Householder form
- `R` : `UpperTriangular` ``R`` factor
- `prow` : Vector of the row permutations applied to the factorized matrix
- `pcol` : Vector of the column permutations applied to the factorized matrix

# Examples
```jldoctest
julia> F = qr(sparse([1,3,2,3,4], [1,1,2,3,4], [1.0,2.0,3.0,4.0,5.0]));

julia> F.Q
4×4 Base.SparseArrays.SPQR.QRSparseQ{Float64,Int64}:
 1.0  0.0  0.0  0.0
 0.0  1.0  0.0  0.0
 0.0  0.0  1.0  0.0
 0.0  0.0  0.0  1.0

julia> F.R
4×4 SparseMatrixCSC{Float64,Int64} with 5 stored entries:
  [1, 1]  =  3.0
  [2, 2]  =  4.0
  [3, 3]  =  5.0
  [2, 4]  =  2.0
  [4, 4]  =  1.0

julia> F.prow
4-element Array{Int64,1}:
 2
 3
 4
 1

julia> F.pcol
4-element Array{Int64,1}:
 2
 3
 4
 1
```
"""
@inline function Base.getproperty(F::QRSparse, d::Symbol)
    if d === :Q
        return QRSparseQ(F.factors, F.τ, size(F, 2))
    elseif d === :prow
        return invperm(F.rpivinv)
    elseif d === :pcol
        return F.cpiv
    else
        getfield(F, d)
    end
end

function Base.propertynames(F::QRSparse, private::Bool=false)
    public = (:R, :Q, :prow, :pcol)
    private ? ((public ∪ fieldnames(typeof(F)))...,) : public
end

function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, F::QRSparse)
    summary(io, F); println(io)
    println(io, "Q factor:")
    show(io, mime, F.Q)
    println(io, "\nR factor:")
    show(io, mime, F.R)
    println(io, "\nRow permutation:")
    show(io, mime, F.prow)
    println(io, "\nColumn permutation:")
    show(io, mime, F.pcol)
end

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
#
# This definition is similar to the definition in factorization.jl except that
# here we have to use \ instead of ldiv! because of limitations in SPQR

## Two helper methods
_ret_size(F::QRSparse, b::AbstractVector) = (size(F, 2),)
_ret_size(F::QRSparse, B::AbstractMatrix) = (size(F, 2), size(B, 2))

LinearAlgebra.rank(F::QRSparse) = reduce(max, view(rowvals(F.R), 1:nnz(F.R)), init = eltype(rowvals(F.R))(0))
LinearAlgebra.rank(S::SparseMatrixCSC) = rank(qr(S))

function (\)(F::QRSparse{T}, B::VecOrMat{Complex{T}}) where T<:LinearAlgebra.BlasReal
# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      ->       |y1|y2|y3|y4|     ->      |x2|y2|     ->    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    require_one_based_indexing(F, B)
    c2r = reshape(copy(transpose(reinterpret(T, reshape(B, (1, length(B)))))), size(B, 1), 2*size(B, 2))
    x = F\c2r

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      <-       |y1|y2|y3|y4|     <-      |x2|y2|     <-    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    return collect(reshape(reinterpret(Complex{T}, copy(transpose(reshape(x, (length(x) >> 1), 2)))), _ret_size(F, B)))
end

function _ldiv_basic(F::QRSparse, B::StridedVecOrMat)
    if size(F, 1) != size(B, 1)
        throw(DimensionMismatch("size(F) = $(size(F)) but size(B) = $(size(B))"))
    end

    # The rank of F equal might be reduced
    rnk = rank(F)

    # allocate an array for the return value large enough to hold B and X
    # For overdetermined problem, B is larger than X and vice versa
    X   = similar(B, ntuple(i -> i == 1 ? max(size(F, 2), size(B, 1)) : size(B, 2), Val(ndims(B))))

    # Fill will zeros. These will eventually become the zeros in the basic solution
    # fill!(X, 0)
    # Apply left permutation to the solution and store in X
    for j in 1:size(B, 2)
        for i in 1:length(F.rpivinv)
            @inbounds X[F.rpivinv[i], j] = B[i, j]
        end
    end

    # Make a view into X corresponding to the size of B
    X0 = view(X, 1:size(B, 1), :)

    # Apply Q' to B
    LinearAlgebra.lmul!(adjoint(F.Q), X0)

    # Zero out to get basic solution
    X[rnk + 1:end, :] .= 0

    # Solve R*X = B
    LinearAlgebra.ldiv!(UpperTriangular(F.R[Base.OneTo(rnk), Base.OneTo(rnk)]),
                        view(X0, Base.OneTo(rnk), :))

    # Apply right permutation and extract solution from X
    return getindex(X, ntuple(i -> i == 1 ? invperm(F.cpiv) : :, Val(ndims(B)))...)
end

(\)(F::QRSparse{T}, B::StridedVecOrMat{T}) where {T} = _ldiv_basic(F, B)
"""
    (\\)(F::QRSparse, B::StridedVecOrMat)

Solve the least squares problem ``\\min\\|Ax - b\\|^2`` or the linear system of equations
``Ax=b`` when `F` is the sparse QR factorization of ``A``. A basic solution is returned
when the problem is underdetermined.

# Examples
```jldoctest
julia> A = sparse([1,2,4], [1,1,1], [1.0,1.0,1.0], 4, 2)
4×2 SparseMatrixCSC{Float64,Int64} with 3 stored entries:
  [1, 1]  =  1.0
  [2, 1]  =  1.0
  [4, 1]  =  1.0

julia> qr(A)\\fill(1.0, 4)
2-element Array{Float64,1}:
 1.0
 0.0
```
"""
(\)(F::QRSparse, B::StridedVecOrMat) = F\convert(AbstractArray{eltype(F)}, B)

end # module
