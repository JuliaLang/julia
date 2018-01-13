# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Support for sparse arrays. Provides `AbstractSparseArray` and subtypes.
"""
module SparseArrays

using Base: ReshapedArray, promote_op, setindex_shape_check, to_shape, tail
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular, PosDefException, fillstored!

import Base: +, -, *, \, /, &, |, xor, ==
import Base.LinAlg: mul!, ldiv!, rdiv!

import Base: @get!, acos, acosd, acot, acotd, acsch, asech, asin, asind, asinh,
    atan, atand, atanh, broadcast!, chol, conj!, cos, cosc, cosd, cosh, cospi, cot,
    cotd, coth, count, csc, cscd, csch, adjoint!, diag, diff, done, dot, eig,
    exp10, exp2, findn, findprev, findnext, floor, hash, indmin, inv,
    issymmetric, istril, istriu, log10, log2, lu, next, sec, secd, sech, show,
    sin, sinc, sind, sinh, sinpi, squeeze, start, sum, summary, tan,
    tand, tanh, trace, transpose!, tril!, triu!, trunc, vecnorm, abs, abs2,
    broadcast, ceil, complex, cond, conj, convert, copy, copyto!, adjoint, diagm,
    exp, expm1, factorize, find, findmax, findmin, findnz, float, getindex,
    vcat, hcat, hvcat, cat, imag, indmax, ishermitian, kron, length, log, log1p, max, min,
    maximum, minimum, norm, one, promote_eltype, real, reshape, rot180,
    rotl90, rotr90, round, scale!, setindex!, similar, size, transpose, tril,
    triu, vec, permute!, map, map!, Array

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
    SparseMatrixCSC, SparseVector, blkdiag, droptol!, dropzeros!, dropzeros,
    issparse, nonzeros, nzrange, rowvals, sparse, sparsevec, spdiagm,
    sprand, sprandn, spzeros, nnz, permute

include("abstractsparse.jl")
include("sparsematrix.jl")
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

end
