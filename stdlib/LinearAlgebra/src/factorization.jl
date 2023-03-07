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

"""
    AdjointFactorization

Lazy wrapper type for the adjoint of the underlying `Factorization` object. Usually, the
`AdjointFactorization` constructor should not be called directly, use
[`adjoint(:: Factorization)`](@ref) instead.
"""
struct AdjointFactorization{T,S<:Factorization} <: Factorization{T}
    parent::S
end
AdjointFactorization(F::Factorization) =
    AdjointFactorization{Base.promote_op(adjoint,eltype(F)),typeof(F)}(F)

"""
    TransposeFactorization

Lazy wrapper type for the transpose of the underlying `Factorization` object. Usually, the
`TransposeFactorization` constructor should not be called directly, use
[`transpose(:: Factorization)`](@ref) instead.
"""
struct TransposeFactorization{T,S<:Factorization} <: Factorization{T}
    parent::S
end
TransposeFactorization(F::Factorization) =
    TransposeFactorization{Base.promote_op(adjoint,eltype(F)),typeof(F)}(F)

eltype(::Type{<:Factorization{T}}) where {T} = T
size(F::AdjointFactorization) = reverse(size(parent(F)))
size(F::TransposeFactorization) = reverse(size(parent(F)))
size(F::Union{AdjointFactorization,TransposeFactorization}, d::Integer) = d in (1, 2) ? size(F)[d] : 1
parent(F::Union{AdjointFactorization,TransposeFactorization}) = F.parent

"""
    adjoint(F::Factorization)

Lazy adjoint of the factorization `F`. By default, returns an
[`AdjointFactorization`](@ref) wrapper.
"""
adjoint(F::Factorization) = AdjointFactorization(F)
"""
    transpose(F::Factorization)

Lazy transpose of the factorization `F`. By default, returns a [`TransposeFactorization`](@ref),
except for `Factorization`s with real `eltype`, in which case returns an [`AdjointFactorization`](@ref).
"""
transpose(F::Factorization) = TransposeFactorization(F)
transpose(F::Factorization{<:Real}) = AdjointFactorization(F)
adjoint(F::AdjointFactorization) = F.parent
transpose(F::TransposeFactorization) = F.parent
transpose(F::AdjointFactorization{<:Real}) = F.parent
conj(A::TransposeFactorization) = adjoint(A.parent)
conj(A::AdjointFactorization) = transpose(A.parent)

checkpositivedefinite(info) = info == 0 || throw(PosDefException(info))
checknonsingular(info, ::RowMaximum) = info == 0 || throw(SingularException(info))
checknonsingular(info, ::RowNonZero) = info == 0 || throw(SingularException(info))
checknonsingular(info, ::NoPivot) = info == 0 || throw(ZeroPivotException(info))
checknonsingular(info) = checknonsingular(info, RowMaximum())

"""
    issuccess(F::Factorization)

Test that a factorization of a matrix succeeded.

!!! compat "Julia 1.6"
    `issuccess(::CholeskyPivoted)` requires Julia 1.6 or later.

```jldoctest
julia> F = cholesky([1 0; 0 1]);

julia> issuccess(F)
true

julia> F = lu([1 0; 0 0]; check = false);

julia> issuccess(F)
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
convert(::Type{T}, f::Factorization) where {T<:Factorization} = T(f)::T

convert(::Type{T}, f::Factorization) where {T<:AbstractArray} = T(f)::T

### General promotion rules
Factorization{T}(F::Factorization{T}) where {T} = F
# This no longer looks odd since the return _is_ a Factorization!
Factorization{T}(A::AdjointFactorization) where {T} =
    adjoint(Factorization{T}(parent(A)))
Factorization{T}(A::TransposeFactorization) where {T} =
    transpose(Factorization{T}(parent(A)))
inv(F::Factorization{T}) where {T} = (n = size(F, 1); ldiv!(F, Matrix{T}(I, n, n)))

Base.hash(F::Factorization, h::UInt) = mapreduce(f -> hash(getfield(F, f)), hash, 1:nfields(F); init=h)
Base.:(==)(  F::T, G::T) where {T<:Factorization} = all(f -> getfield(F, f) == getfield(G, f), 1:nfields(F))
Base.isequal(F::T, G::T) where {T<:Factorization} = all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))::Bool

function Base.show(io::IO, x::AdjointFactorization)
    print(io, "adjoint of ")
    show(io, parent(x))
end
function Base.show(io::IO, x::TransposeFactorization)
    print(io, "transpose of ")
    show(io, parent(x))
end
function Base.show(io::IO, ::MIME"text/plain", x::AdjointFactorization)
    print(io, "adjoint of ")
    show(io, MIME"text/plain"(), parent(x))
end
function Base.show(io::IO, ::MIME"text/plain", x::TransposeFactorization)
    print(io, "transpose of ")
    show(io, MIME"text/plain"(), parent(x))
end

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns or rows
function (\)(F::Factorization{T}, B::VecOrMat{Complex{T}}) where {T<:BlasReal}
    require_one_based_indexing(B)
    c2r = reshape(copy(transpose(reinterpret(T, reshape(B, (1, length(B)))))), size(B, 1), 2*size(B, 2))
    x = ldiv!(F, c2r)
    return reshape(copy(reinterpret(Complex{T}, copy(transpose(reshape(x, div(length(x), 2), 2))))), _ret_size(F, B))
end
# don't do the reinterpretation for [Adjoint/Transpose]Factorization
(\)(F::TransposeFactorization{T}, B::VecOrMat{Complex{T}}) where {T<:BlasReal} =
    conj!(adjoint(parent(F)) \ conj.(B))
(\)(F::AdjointFactorization{T}, B::VecOrMat{Complex{T}}) where {T<:BlasReal} =
    @invoke \(F::typeof(F), B::VecOrMat)

function (/)(B::VecOrMat{Complex{T}}, F::Factorization{T}) where {T<:BlasReal}
    require_one_based_indexing(B)
    x = rdiv!(copy(reinterpret(T, B)), F)
    return copy(reinterpret(Complex{T}, x))
end
# don't do the reinterpretation for [Adjoint/Transpose]Factorization
(/)(B::VecOrMat{Complex{T}}, F::TransposeFactorization{T}) where {T<:BlasReal} =
    conj!(adjoint(parent(F)) \ conj.(B))
(/)(B::VecOrMat{Complex{T}}, F::AdjointFactorization{T}) where {T<:BlasReal} =
    @invoke /(B::VecOrMat{Complex{T}}, F::Factorization{T})

function (\)(F::Factorization, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    TFB = typeof(oneunit(eltype(F)) \ oneunit(eltype(B)))
    ldiv!(F, copy_similar(B, TFB))
end
(\)(F::TransposeFactorization, B::AbstractVecOrMat) = conj!(adjoint(F.parent) \ conj.(B))

function (/)(B::AbstractMatrix, F::Factorization)
    require_one_based_indexing(B)
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    rdiv!(copy_similar(B, TFB), F)
end
(/)(A::AbstractMatrix, F::AdjointFactorization) = adjoint(adjoint(F) \ adjoint(A))
(/)(A::AbstractMatrix, F::TransposeFactorization) = transpose(transpose(F) \ transpose(A))

function ldiv!(Y::AbstractVector, A::Factorization, B::AbstractVector)
    require_one_based_indexing(Y, B)
    m, n = size(A)
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
    m, n = size(A)
    if m > n
        Bc = copy(B)
        ldiv!(A, Bc)
        return copyto!(Y, view(Bc, 1:n, :))
    else
        copyto!(view(Y, 1:m, :), view(B, 1:m, :))
        return ldiv!(A, Y)
    end
end
