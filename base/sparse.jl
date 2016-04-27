# This file is a part of Julia. License is MIT: http://julialang.org/license

module SparseArrays

using Base: ReshapedArray
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular, PosDefException

import Base: +, -, *, \, &, |, $, .+, .-, .*, ./, .\, .^, .<, .!=, ==
import Base: A_mul_B!, Ac_mul_B, Ac_mul_B!, At_mul_B, At_mul_B!
import Base: A_mul_Bc, A_mul_Bt, Ac_mul_Bc, At_mul_Bt
import Base: At_ldiv_B, Ac_ldiv_B, A_ldiv_B!
import Base.LinAlg: At_ldiv_B!, Ac_ldiv_B!

import Base: @get!, acos, acosd, acot, acotd, acsch, asech, asin, asind, asinh,
    atan, atand, atanh, broadcast!, chol, conj!, cos, cosc, cosd, cosh, cospi, cot,
    cotd, coth, countnz, csc, cscd, csch, ctranspose!, diag, diff, done, dot, eig,
    exp10, exp2, eye, findn, floor, hash, indmin, inv, issymmetric, istril, istriu,
    log10, log2, lu, maxabs, minabs, next, sec, secd, sech, show, showarray, sin,
    sinc, sind, sinh, sinpi, squeeze, start, sum, sumabs, sumabs2, summary, tan,
    tand, tanh, trace, transpose!, tril!, triu!, trunc, vecnorm, writemime, abs, abs2,
    broadcast, ceil, complex, cond, conj, convert, copy, copy!, ctranspose, diagm,
    exp, expm1, factorize, find, findmax, findmin, findnz, float, full, getindex,
    hcat, hvcat, imag, indmax, ishermitian, kron, length, log, log1p, max, min,
    maximum, minimum, norm, one, promote_eltype, real, reinterpret, reshape, rot180,
    rotl90, rotr90, round, scale!, setindex!, similar, size, transpose, tril,
    triu, vcat, vec

import Base.Broadcast: eltype_plus, broadcast_shape

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blkdiag, dense, droptol!, dropzeros!, etree,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm, speye, spones,
    sprand, sprandn, spzeros, symperm, nnz

include("sparse/abstractsparse.jl")
include("sparse/sparsematrix.jl")
include("sparse/sparsevector.jl")

include("sparse/linalg.jl")
if Base.USE_GPL_LIBS
    include("sparse/umfpack.jl")
    include("sparse/cholmod.jl")
    include("sparse/spqr.jl")
end

# point users to SuiteSparse
const SUITESPARSE_END_STRING = string(" has been moved to the package SuiteSparse.jl.\n",
          "Run Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5-")

"""
    etree(A[, post])
Compute the elimination tree of a symmetric sparse matrix `A` from `triu(A)`
and, optionally, its post-ordering permutation.
Note: This function has been moved to the SuiteSparse.jl package.
"""
function etree{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, postorder::Bool)
    if isdefined(Main, :SuiteSparse)
        Main.SuiteSparse.etree(A, postorder)
    else
        error("etree(A[, post])", SUITESPARSE_END_STRING)
    end
end

etree(A::SparseMatrixCSC) = etree(A, false)

function ereach{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, k::Integer, parent::Vector{Ti})
    if isdefined(Main, :SuiteSparse)
        Main.SuiteSparse.ereach(A, k, parent)
    else
        error("ereach(A, k, parent)", SUITESPARSE_END_STRING)
    end
end

function csc_permute{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti}, q::Vector{Ti})
    if isdefined(Main, :SuiteSparse)
        Main.SuiteSparse.csc_permute(A, pinv, q)
    else
        error("csc_permute(A, pinv, q)", SUITESPARSE_END_STRING)
    end
end

"""
    symperm(A, p)
Return the symmetric permutation of `A`, which is `A[p,p]`. `A` should be
symmetric, sparse, and only contain nonzeros in the upper triangular part of the
matrix is stored. This algorithm ignores the lower triangular part of the
matrix. Only the upper triangular part of the result is returned.
Note: This function has been moved to the SuiteSparse.jl package.
"""
function symperm{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti})
    if isdefined(Main, :SuiteSparse)
        Main.SuiteSparse.symperm(A, pinv)
    else
        error("symperm(A, pinv)", SUITESPARSE_END_STRING)
    end
end

end
