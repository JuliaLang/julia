# This file is a part of Julia. License is MIT: http://julialang.org/license

module SparseArrays

using Base: ReshapedArray, promote_op, setindex_shape_check, to_shape, tail
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular, PosDefException

import Base: +, -, *, \, /, &, |, xor, ==
import Base: A_mul_B!, Ac_mul_B, Ac_mul_B!, At_mul_B, At_mul_B!
import Base: A_mul_Bc, A_mul_Bt, Ac_mul_Bc, At_mul_Bt
import Base: At_ldiv_B, Ac_ldiv_B, A_ldiv_B!
import Base.LinAlg: At_ldiv_B!, Ac_ldiv_B!

import Base: @get!, acos, acosd, acot, acotd, acsch, asech, asin, asind, asinh,
    atan, atand, atanh, broadcast!, chol, conj!, cos, cosc, cosd, cosh, cospi, cot,
    cotd, coth, countnz, csc, cscd, csch, ctranspose!, diag, diff, done, dot, eig,
    exp10, exp2, eye, findn, floor, hash, indmin, inv, issymmetric, istril, istriu,
    log10, log2, lu, next, sec, secd, sech, show, sin,
    sinc, sind, sinh, sinpi, squeeze, start, sum, summary, tan,
    tand, tanh, trace, transpose!, tril!, triu!, trunc, vecnorm, abs, abs2,
    broadcast, ceil, complex, cond, conj, convert, copy, copy!, ctranspose, diagm,
    exp, expm1, factorize, find, findmax, findmin, findnz, float, full, getindex,
    vcat, hcat, hvcat, cat, imag, indmax, ishermitian, kron, length, log, log1p, max, min,
    maximum, minimum, norm, one, promote_eltype, real, reinterpret, reshape, rot180,
    rotl90, rotr90, round, scale!, setindex!, similar, size, transpose, tril,
    triu, vec, permute!, map, map!

import Base.Broadcast: broadcast_indices

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blkdiag, dense, droptol!, dropzeros!, dropzeros,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm, speye, spones,
    sprand, sprandn, spzeros, nnz, permute

include("abstractsparse.jl")
include("sparsematrix.jl")
include("sparsevector.jl")
include("higherorderfns.jl")

include("linalg.jl")
if Base.USE_GPL_LIBS
    include("umfpack.jl")
    include("cholmod.jl")
    include("spqr.jl")
end

end
