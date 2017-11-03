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

# The following two methods should be overloaded by concrete types to avoid
# allocating the I = find(...)
_sparse_findnextnz(v::AbstractSparseArray, i) = (I = find(!iszero, v); n = searchsortedfirst(I, i); n<=length(I) ? I[n] : 0)
_sparse_findprevnz(v::AbstractSparseArray, i) = (I = find(!iszero, v); n = searchsortedlast(I, i);  n>0          ? I[n] : 0)

function findnext(f::typeof(!iszero), v::AbstractSparseArray, i::Int)
    j = _sparse_findnextnz(v, i)
    if j == 0
        return 0
    end
    while !f(v[j])
        j = _sparse_findnextnz(v, j+1)
        if j == 0
            return 0
        end
    end
    return j
end

function findprev(f::typeof(!iszero), v::AbstractSparseArray, i::Int)
    j = _sparse_findprevnz(v, i)
    if j == 0
        return 0
    end
    while !f(v[j])
        j = _sparse_findprevnz(v, j-1)
        if j == 0
            return 0
        end
    end
    return j
end
