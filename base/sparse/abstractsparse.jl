# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N} end

const AbstractSparseVector{Tv,Ti} = AbstractSparseArray{Tv,Ti,1}
const AbstractSparseMatrix{Tv,Ti} = AbstractSparseArray{Tv,Ti,2}

"""
    issparse(S)

Returns `true` if `S` is sparse, and `false` otherwise.
"""
issparse(A::AbstractArray) = false
issparse(S::AbstractSparseArray) = true

issparse(S::Symmetric{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::Hermitian{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LowerTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinAlg.UnitLowerTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::UpperTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinAlg.UnitUpperTriangular{<:Any,<:AbstractSparseMatrix}) = true

indtype(S::AbstractSparseArray{<:Any,Ti}) where {Ti} = Ti

function Base.reinterpret(::Type, A::AbstractSparseArray)
    error("""
          `reinterpret` on sparse arrays is discontinued.
          Try reinterpreting the value itself instead.
          """)
end
