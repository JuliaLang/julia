# This file is a part of Julia. License is MIT: http://julialang.org/license

module SparseArrays

using Base: Func, AddFun, OrFun, ConjFun, IdFun
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular, PosDefException

import Base: +, -, *, &, |, $, .+, .-, .*, ./, .\, .^, .<, .!=, ==
import Base: A_mul_B!, Ac_mul_B, Ac_mul_B!, At_mul_B, At_mul_B!, A_ldiv_B!

import Base: @get!, acos, acosd, acot, acotd, acsch, asech, asin, asind, asinh,
    atan, atand, atanh, broadcast!, chol, conj!, cos, cosc, cosd, cosh, cospi, cot,
    cotd, coth, countnz, csc, cscd, csch, ctranspose!, diag, diff, done, dot, eig,
    exp10, exp2, eye, findn, floor, hash, indmin, inv, issym, istril, istriu, log10,
    log2, lu, maxabs, minabs, next, sec, secd, sech, show, showarray, sin, sinc,
    sind, sinh, sinpi, squeeze, start, sum, sumabs, sumabs2, summary, tan, tand,
    tanh, trace, transpose!, tril!, triu!, trunc, vecnorm, writemime, abs, abs2,
    broadcast, call, ceil, complex, cond, conj, convert, copy, ctranspose, diagm,
    exp, expm1, factorize, find, findmax, findmin, findnz, float, full, getindex,
    hcat, hvcat, imag, indmax, ishermitian, kron, length, log, log1p, max, min,
    maximum, minimum, norm, one, promote_eltype, real, reinterpret, reshape, rot180,
    rotl90, rotr90, round, scale, scale!, setindex!, similar, size, transpose, tril,
    triu, vcat, vec

import Base.Broadcast: eltype_plus, broadcast_shape

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blkdiag, dense, droptol!, dropzeros!, etree,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm, speye, spones,
    sprand, sprandbool, sprandn, spzeros, symperm, nnz

include("sparse/abstractsparse.jl")
include("sparse/sparsematrix.jl")
include("sparse/sparsevector.jl")
include("sparse/csparse.jl")

include("sparse/linalg.jl")
if Base.USE_GPL_LIBS
    include("sparse/umfpack.jl")
    include("sparse/cholmod.jl")
    include("sparse/spqr.jl")
end

end
