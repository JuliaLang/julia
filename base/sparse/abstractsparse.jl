# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: union!, unique

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

indtype{Tv,Ti}(S::AbstractSparseArray{Tv,Ti}) = Ti

function union!{Tv,Ti,N}(s::AbstractSet, x::AbstractSparseArray{Tv,Ti,N})
    vals = nonzeros(x)
    length(vals) == length(x) ? union!(s, vals) : union!(s, [zero(Tv); vals])
end

# assumes that AbstractSparseArray type stores nonzeros in column major order
# this assumption is subject to change in the future
function unique{Tv,Ti,N}(x::AbstractSparseArray{Tv,Ti,N})
    vals = nonzeros(x)
    if length(x) == length(vals)
        return unique(vals)
    else
        # find the first zero valued element
        i = findfirst(x, zero(Tv))
        # insert first zero in sequence of potential uniques
        return unique([vals[1:i-1]; zero(Tv); vals[i:end]])
    end
end
