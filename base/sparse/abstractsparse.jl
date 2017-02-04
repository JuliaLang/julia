# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N}

typealias AbstractSparseVector{Tv,Ti} AbstractSparseArray{Tv,Ti,1}
typealias AbstractSparseMatrix{Tv,Ti} AbstractSparseArray{Tv,Ti,2}

"""
    issparse(S)

Returns `true` if `S` is sparse, and `false` otherwise.
"""
issparse(A::AbstractArray) = false
issparse(S::AbstractSparseArray) = true

issparse{T, A<:AbstractSparseMatrix}(S::Symmetric{T, A}) = true
issparse{T, A<:AbstractSparseMatrix}(S::Hermitian{T, A}) = true
issparse{T, A<:AbstractSparseMatrix}(S::LowerTriangular{T, A}) = true
issparse{T, A<:AbstractSparseMatrix}(S::LinAlg.UnitLowerTriangular{T, A}) = true
issparse{T, A<:AbstractSparseMatrix}(S::UpperTriangular{T, A}) = true
issparse{T, A<:AbstractSparseMatrix}(S::LinAlg.UnitUpperTriangular{T, A}) = true

indtype{Tv,Ti}(S::AbstractSparseArray{Tv,Ti}) = (Base.@_pure_meta; Ti)
