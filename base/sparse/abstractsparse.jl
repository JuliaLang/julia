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

# The following two methods should be overloaded by concrete types to avoid
# allocating the I = find(...)
_sparse_findnext(v::AbstractSparseArray, i) = (I = find(v); n = searchsortedfirst(I, i); n<=length(I) ? I[n] : 0)
_sparse_findprev(v::AbstractSparseArray, i) = (I = find(v); n = searchsortedlast(I, i);  n>0          ? I[n] : 0)

function findnext(v::AbstractSparseArray, i::Int)
    j = _sparse_findnext(v, i)
    if j == 0
        return 0
    end
    while v[j] == 0
        j = _sparse_findnext(v, j+1)
        if j == 0
            return 0
        end
    end
    return j
end

function findprev(v::AbstractSparseArray, i::Int)
    j = _sparse_findprev(v, i)
    if j == 0
        return 0
    end
    while v[j] == 0
        j = _sparse_findprev(v, j-1)
        if j == 0
            return 0
        end
    end
    return j
end
