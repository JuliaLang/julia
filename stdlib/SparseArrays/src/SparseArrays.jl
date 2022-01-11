# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Support for sparse arrays. Provides `AbstractSparseArray` and subtypes.
"""
module SparseArrays

using Base: ReshapedArray, promote_op, setindex_shape_check, to_shape, tail,
    require_one_based_indexing, promote_eltype
using Base.Sort: Forward
using LinearAlgebra
using LinearAlgebra: AdjOrTrans, matprod

import Base: +, -, *, \, /, &, |, xor, ==, zero
import LinearAlgebra: mul!, ldiv!, rdiv!, cholesky, adjoint!, diag, eigen, dot,
    issymmetric, istril, istriu, lu, tr, transpose!, tril!, triu!, isbanded,
    cond, diagm, factorize, ishermitian, norm, opnorm, lmul!, rmul!, tril, triu

import Base: adjoint, argmin, argmax, Array, broadcast, circshift!, complex, Complex,
    conj, conj!, convert, copy, copy!, copyto!, count, diff, findall, findmax, findmin,
    float, getindex, imag, inv, kron, kron!, length, map, maximum, minimum, permute!, real,
    rot180, rotl90, rotr90, setindex!, show, similar, size, sum, transpose,
    vcat, hcat, hvcat, cat, vec

using Random: default_rng, AbstractRNG, randsubseq, randsubseq!

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blockdiag, droptol!, dropzeros!, dropzeros,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm,
    sprand, sprandn, spzeros, nnz, permute, findnz,
    sparse_hcat, sparse_vcat, sparse_hvcat

include("abstractsparse.jl")
include("sparsematrix.jl")
include("sparseconvert.jl")
include("sparsevector.jl")
include("higherorderfns.jl")
include("linalg.jl")
include("deprecated.jl")


zero(a::AbstractSparseArray) = spzeros(eltype(a), size(a)...)

LinearAlgebra.diagzero(D::Diagonal{<:AbstractSparseMatrix{T}},i,j) where {T} =
    spzeros(T, size(D.diag[i], 1), size(D.diag[j], 2))

end
