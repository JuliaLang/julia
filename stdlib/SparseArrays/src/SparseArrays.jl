# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Support for sparse arrays. Provides `AbstractSparseArray` and subtypes.
"""
module SparseArrays

using Base: ReshapedArray, promote_op, setindex_shape_check, to_shape, tail,
    require_one_based_indexing
using Base.Sort: Forward
using LinearAlgebra

import Base: +, -, *, \, /, &, |, xor, ==, zero
import LinearAlgebra: mul!, ldiv!, rdiv!, cholesky, adjoint!, diag, eigen, dot,
    issymmetric, istril, istriu, lu, tr, transpose!, tril!, triu!,
    cond, diagm, factorize, ishermitian, norm, opnorm, lmul!, rmul!, tril, triu, matprod

import Base: acos, acosd, acot, acotd, acsch, asech, asin, asind, asinh,
    atan, atand, atanh, broadcast!, conj!, cos, cosc, cosd, cosh, cospi, cot,
    cotd, coth, count, csc, cscd, csch,
    exp10, exp2, findprev, findnext, floor, hash, argmin, inv,
    log10, log2, sec, secd, sech, show,
    sin, sinc, sind, sinh, sinpi, dropdims, sum, summary, tan,
    tand, tanh, trunc, abs, abs2,
    broadcast, ceil, complex, conj, convert, copy, copyto!, adjoint,
    exp, expm1, findall, findmax, findmin, float, getindex,
    vcat, hcat, hvcat, cat, imag, argmax, kron, kron!, length, log, log1p, max, min,
    maximum, minimum, one, promote_eltype, real, reshape, rot180,
    rotl90, rotr90, round, setindex!, similar, size, transpose,
    vec, permute!, map, map!, Array, diff, circshift!, circshift

using Random: default_rng, AbstractRNG, randsubseq, randsubseq!

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blockdiag, droptol!, dropzeros!, dropzeros,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm,
    sprand, sprandn, spzeros, nnz, permute, findnz

include("abstractsparse.jl")
include("sparsematrix.jl")
include("sparseconvert.jl")
include("sparsevector.jl")
include("higherorderfns.jl")
include("linalg.jl")
include("deprecated.jl")


# temporarily moved here and commented out from from base/linalg/diagonal.jl, base/linalg/tridiag.jl
# and base/linalg/bidiag.jl due to their usage of spzeros
similar(B::Bidiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)
similar(D::Diagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)
similar(S::SymTridiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)
similar(M::Tridiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)

zero(a::AbstractSparseArray) = spzeros(eltype(a), size(a)...)

const BiTriSym = Union{Bidiagonal,SymTridiagonal,Tridiagonal}
function *(A::BiTriSym, B::BiTriSym)
    TS = promote_op(matprod, eltype(A), eltype(B))
    mul!(similar(A, TS, size(A)...), A, B)
end

end
