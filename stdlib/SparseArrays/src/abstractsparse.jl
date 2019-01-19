# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N} end

const AbstractSparseVector{Tv,Ti} = AbstractSparseArray{Tv,Ti,1}
const AbstractSparseMatrix{Tv,Ti} = AbstractSparseArray{Tv,Ti,2}

"""
    issparse(S)

Returns `true` if `S` is sparse, and `false` otherwise.

# Examples
```jldoctest
julia> sv = sparsevec([1, 4], [2.3, 2.2], 10)
10-element SparseVector{Float64,Int64} with 2 stored entries:
  [1 ]  =  2.3
  [4 ]  =  2.2

julia> issparse(sv)
true

julia> issparse(Array(sv))
false
```
"""
issparse(A::AbstractArray) = false
issparse(S::AbstractSparseArray) = true

issparse(S::LinearAlgebra.Symmetric{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinearAlgebra.Hermitian{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinearAlgebra.LowerTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinearAlgebra.UnitLowerTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinearAlgebra.UpperTriangular{<:Any,<:AbstractSparseMatrix}) = true
issparse(S::LinearAlgebra.UnitUpperTriangular{<:Any,<:AbstractSparseMatrix}) = true

indtype(S::AbstractSparseArray{<:Any,Ti}) where {Ti} = Ti

function Base.reinterpret(::Type, A::AbstractSparseArray)
    error("""
          `reinterpret` on sparse arrays is discontinued.
          Try reinterpreting the value itself instead.
          """)
end

# The following two methods should be overloaded by concrete types to avoid
# allocating the I = findall(...)
_sparse_findnextnz(v::AbstractSparseArray, i::Integer) = (I = findall(!iszero, v); n = searchsortedfirst(I, i); n<=length(I) ? I[n] : nothing)
_sparse_findprevnz(v::AbstractSparseArray, i::Integer) = (I = findall(!iszero, v); n = searchsortedlast(I, i);  !iszero(n)   ? I[n] : nothing)

function findnext(f::typeof(!iszero), v::AbstractSparseArray, i::Integer)
    j = _sparse_findnextnz(v, i)
    while j !== nothing && !f(v[j])
        j = _sparse_findnextnz(v, j+1)
    end
    return j
end

function findprev(f::typeof(!iszero), v::AbstractSparseArray, i::Integer)
    j = _sparse_findprevnz(v, i)
    while j !== nothing && !f(v[j])
        j = _sparse_findprevnz(v, j-1)
    end
    return j
end

"""
    findnz(A)

Return a tuple `(I, J, V)` where `I` and `J` are the row and column indices of the stored
("structurally non-zero") values in sparse matrix `A`, and `V` is a vector of the values.

# Examples
```jldoctest
julia> A = sparse([1 2 0; 0 0 3; 0 4 0])
3×3 SparseMatrixCSC{Int64,Int64} with 4 stored entries:
  [1, 1]  =  1
  [1, 2]  =  2
  [3, 2]  =  4
  [2, 3]  =  3

julia> findnz(A)
([1, 1, 3, 2], [1, 2, 2, 3], [1, 2, 4, 3])
```
"""
function findnz end
