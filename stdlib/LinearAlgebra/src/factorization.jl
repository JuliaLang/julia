# This file is a part of Julia. License is MIT: https://julialang.org/license

## Matrix factorizations and decompositions
"""
    LinearAlgebra.Factorization

Abstract type for [matrix factorizations](https://en.wikipedia.org/wiki/Matrix_decomposition)
a.k.a. matrix decompositions.
See [online documentation](@ref man-linalg-factorizations) for a list of available
matrix factorizations.
"""
abstract type Factorization{T} end

eltype(::Type{<:Factorization{T}}) where {T} = T
size(F::Adjoint{<:Any,<:Factorization}) = reverse(size(parent(F)))
size(F::Transpose{<:Any,<:Factorization}) = reverse(size(parent(F)))

checkpositivedefinite(info) = info == 0 || throw(PosDefException(info))
checknonsingular(info, ::RowMaximum) = info == 0 || throw(SingularException(info))
checknonsingular(info, ::NoPivot) = info == 0 || throw(ZeroPivotException(info))
checknonsingular(info) = checknonsingular(info, RowMaximum())

"""
    issuccess(F::Factorization)

Test that a factorization of a matrix succeeded.

!!! compat "Julia 1.6"
    `issuccess(::CholeskyPivoted)` requires Julia 1.6 or later.

```jldoctest
julia> F = cholesky([1 0; 0 1]);

julia> LinearAlgebra.issuccess(F)
true

julia> F = lu([1 0; 0 0]; check = false);

julia> LinearAlgebra.issuccess(F)
false
```
"""
issuccess(F::Factorization)

function logdet(F::Factorization)
    d, s = logabsdet(F)
    return d + log(s)
end

function det(F::Factorization)
    d, s = logabsdet(F)
    return exp(d)*s
end

convert(::Type{T}, f::T) where {T<:Factorization} = f
convert(::Type{T}, f::Factorization) where {T<:Factorization} = T(f)

convert(::Type{T}, f::Factorization) where {T<:AbstractArray} = T(f)

### General promotion rules
Factorization{T}(F::Factorization{T}) where {T} = F
# This is a bit odd since the return is not a Factorization but it works well in generic code
Factorization{T}(A::Adjoint{<:Any,<:Factorization}) where {T} =
    adjoint(Factorization{T}(parent(A)))
inv(F::Factorization{T}) where {T} = (n = size(F, 1); ldiv!(F, Matrix{T}(I, n, n)))

Base.hash(F::Factorization, h::UInt) = mapreduce(f -> hash(getfield(F, f)), hash, 1:nfields(F); init=h)
Base.:(==)(  F::T, G::T) where {T<:Factorization} = all(f -> getfield(F, f) == getfield(G, f), 1:nfields(F))
Base.isequal(F::T, G::T) where {T<:Factorization} = all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))::Bool

function Base.show(io::IO, x::Adjoint{<:Any,<:Factorization})
    print(io, "Adjoint of ")
    show(io, parent(x))
end
function Base.show(io::IO, x::Transpose{<:Any,<:Factorization})
    print(io, "Transpose of ")
    show(io, parent(x))
end
function Base.show(io::IO, ::MIME"text/plain", x::Adjoint{<:Any,<:Factorization})
    print(io, "Adjoint of ")
    show(io, MIME"text/plain"(), parent(x))
end
function Base.show(io::IO, ::MIME"text/plain", x::Transpose{<:Any,<:Factorization})
    print(io, "Transpose of ")
    show(io, MIME"text/plain"(), parent(x))
end

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns or rows
function (\)(F::Factorization{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    require_one_based_indexing(B)
    c2r = reshape(copy(transpose(reinterpret(T, reshape(B, (1, length(B)))))), size(B, 1), 2*size(B, 2))
    x = ldiv!(F, c2r)
    return reshape(copy(reinterpret(Complex{T}, copy(transpose(reshape(x, div(length(x), 2), 2))))), _ret_size(F, B))
end
function (/)(B::VecOrMat{Complex{T}}, F::Factorization{T}) where T<:BlasReal
    require_one_based_indexing(B)
    x = rdiv!(copy(reinterpret(T, B)), F)
    return copy(reinterpret(Complex{T}, x))
end

function \(F::Union{Factorization, Adjoint{<:Any,<:Factorization}}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    ldiv!(F, copy_similar(B, TFB))
end

function /(B::AbstractMatrix, F::Union{Factorization, Adjoint{<:Any,<:Factorization}})
    require_one_based_indexing(B)
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    rdiv!(copy_similar(B, TFB), F)
end
/(adjB::AdjointAbsVec, adjF::Adjoint{<:Any,<:Factorization}) = adjoint(adjF.parent \ adjB.parent)
/(B::TransposeAbsVec, adjF::Adjoint{<:Any,<:Factorization}) = adjoint(adjF.parent \ adjoint(B))


function ldiv!(Y::AbstractVector, A::Factorization, B::AbstractVector)
    require_one_based_indexing(Y, B)
    m, n = size(A, 1), size(A, 2)
    if m > n
        Bc = copy(B)
        ldiv!(A, Bc)
        return copyto!(Y, 1, Bc, 1, n)
    else
        return ldiv!(A, copyto!(Y, B))
    end
end
function ldiv!(Y::AbstractMatrix, A::Factorization, B::AbstractMatrix)
    require_one_based_indexing(Y, B)
    m, n = size(A, 1), size(A, 2)
    if m > n
        Bc = copy(B)
        ldiv!(A, Bc)
        return copyto!(Y, view(Bc, 1:n, :))
    else
        copyto!(view(Y, 1:m, :), view(B, 1:m, :))
        return ldiv!(A, Y)
    end
end

# fallback methods for transposed solves
\(F::Transpose{<:Any,<:Factorization{<:Real}}, B::AbstractVecOrMat) = adjoint(F.parent) \ B
\(F::Transpose{<:Any,<:Factorization}, B::AbstractVecOrMat) = conj.(adjoint(F.parent) \ conj.(B))

/(B::AbstractMatrix, F::Transpose{<:Any,<:Factorization{<:Real}}) = B / adjoint(F.parent)
/(B::AbstractMatrix, F::Transpose{<:Any,<:Factorization}) = conj.(conj.(B) / adjoint(F.parent))
/(B::AdjointAbsVec, F::Transpose{<:Any,<:Factorization{<:Real}}) = B / adjoint(F.parent)
/(B::TransposeAbsVec, F::Transpose{<:Any,<:Factorization{<:Real}}) = B / adjoint(F.parent)
/(B::AdjointAbsVec, F::Transpose{<:Any,<:Factorization}) = conj.(conj.(B) / adjoint(F.parent))
/(B::TransposeAbsVec, F::Transpose{<:Any,<:Factorization}) = conj.(conj.(B) / adjoint(F.parent))
