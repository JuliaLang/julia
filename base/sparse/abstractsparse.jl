# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N} end

const AbstractSparseVector{Tv,Ti} = Union{AbstractSparseArray{Tv,Ti,1}, Base.ReinterpretArray{Tv,1,T,<:AbstractSparseArray{T,Ti,1}} where T}
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
issparse(S::Base.ReinterpretArray) = issparse(S.parent)

indtype(S::AbstractSparseArray{<:Any,Ti}) where {Ti} = Ti

nonzeros(A::Base.ReinterpretArray{T}) where {T} = reinterpret(T, nonzeros(A.parent))
function nonzeroinds(A::Base.ReinterpretArray{T,N,S} where {N}) where {T,S}
    if sizeof(T) == sizeof(S)
        return nonzeroinds(A.parent)
    elseif sizeof(T) > sizeof(S)
        unique(map(nonzeroinds(A.parent)) do ind
            div(ind, div(sizeof(T), sizeof(S)))
        end)
    else
        map(nonzeroinds(A.parent)) do ind
            ind * div(sizeof(S), sizeof(T))
        end
    end
end

function Base.reinterpret(::Type{T}, m::AbstractSparseArray{S}) where {T, S}
    (sizeof(T) == sizeof(S)) ||
        throw(ArgumentError("sparse array reinterpret is only supported for element types of the same size"))
    invoke(Base.reinterpret, Tuple{Type, AbstractArray}, T, m)
end
