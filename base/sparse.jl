# This file is a part of Julia. License is MIT: http://julialang.org/license

module SparseMatrix

using Base: Func, AddFun, OrFun, ConjFun, IdFun
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular, PosDefException

import Base: +, -, *, &, |, $, .+, .-, .*, ./, .\, .^, .<, .!=, ==
import Base: A_mul_B!, Ac_mul_B, Ac_mul_B!, At_mul_B!, A_ldiv_B!
import Base: @get!, abs, abs2, broadcast, ceil, complex, cond, conj, convert, copy,
    ctranspose, diagm, exp, expm1, factorize, find, findmax, findmin, findnz, float,
    full, getindex, hcat, hvcat, imag, indmax, ishermitian, kron, length, log, log1p,
    max, min, norm, one, promote_eltype, real, reinterpret, reshape, rot180, rotl90,
    rotr90, round, scale, scale!, setindex!, similar, size, transpose, tril, triu, vcat,
    vec
import Base.Broadcast: eltype_plus, broadcast_shape

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector, SparseMatrixCSC,
       blkdiag, dense, droptol!, dropzeros!, etree, issparse, nnz, nonzeros, nzrange,
       rowvals, sparse, sparsevec, spdiagm, speye, spones, sprand, sprandbool, sprandn,
       spzeros, symperm

include("sparse/abstractsparse.jl")
include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

include("sparse/linalg.jl")
if Base.USE_GPL_LIBS
    include("sparse/umfpack.jl")
    include("sparse/cholmod.jl")
    include("sparse/spqr.jl")
end

end # module SparseMatrix
