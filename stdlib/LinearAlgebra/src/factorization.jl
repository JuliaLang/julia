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

# convenience methods
## return only the solution of a least squares problem while avoiding promoting
## vectors to matrices.
_cut_B(x::AbstractVector, r::UnitRange) = length(x)  > length(r) ? x[r]   : x
_cut_B(X::AbstractMatrix, r::UnitRange) = size(X, 1) > length(r) ? X[r,:] : X

## append right hand side with zeros if necessary
_zeros(::Type{T}, b::AbstractVector, n::Integer) where {T} = zeros(T, max(length(b), n))
_zeros(::Type{T}, B::AbstractMatrix, n::Integer) where {T} = zeros(T, max(size(B, 1), n), size(B, 2))

# General fallback definition for handling under- and overdetermined system as well as square problems
function \(adjF::Union{<:Factorization,Adjoint{<:Any,<:Factorization}}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    m, n = size(adjF)
    if m != size(B, 1)
        throw(DimensionMismatch("arguments must have the same number of rows"))
    end
    # F = adjF.parent
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(adjF)))

    # For wide problem we (often) compute a minimum norm solution. The solution
    # is larger than the right hand side so we use size(adjF, 2).
    BB = _zeros(TFB, B, n)

    if n > size(B, 1)
        # Underdetermined
        fill!(BB, 0)
        copyto!(view(BB, 1:m, :), B)
    else
        copyto!(BB, B)
    end

    ldiv!(adjF, BB)

    # For tall problems, we compute a least sqaures solution so only part
    # of the rhs should be returned from \ while ldiv! uses (and returns)
    # the complete rhs
    return _cut_B(BB, 1:n)
end

function /(B::AbstractMatrix, F::Factorization)
    require_one_based_indexing(B)
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    BB = similar(B, TFB, size(B))
    copyto!(BB, B)
    rdiv!(BB, F)
end
function /(B::AbstractMatrix, adjF::Adjoint{<:Any,<:Factorization})
    require_one_based_indexing(B)
    F = adjF.parent
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    BB = similar(B, TFB, size(B))
    copyto!(BB, B)
    rdiv!(BB, adjoint(F))
end
/(adjB::AdjointAbsVec, adjF::Adjoint{<:Any,<:Factorization}) = adjoint(adjF.parent \ adjB.parent)
/(B::TransposeAbsVec, adjF::Adjoint{<:Any,<:Factorization}) = adjoint(adjF.parent \ adjoint(B))

# support the same 3-arg idiom as in our other in-place A_*_B functions:
function ldiv!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)
    require_one_based_indexing(Y, B)
    m, n = size(A, 1), size(A, 2)
    if m > n
        Bc = copy(B)
        ldiv!(A, Bc)
        return copyto!(Y, view(Bc, 1:n, :))
    else
        return ldiv!(A, copyto!(Y, view(B, 1:m, :)))
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
