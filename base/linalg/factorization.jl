# This file is a part of Julia. License is MIT: https://julialang.org/license

## Matrix factorizations and decompositions

abstract type Factorization{T} end

eltype(::Type{Factorization{T}}) where {T} = T
transpose(F::Factorization) = error("transpose not implemented for $(typeof(F))")
adjoint(F::Factorization) = error("adjoint not implemented for $(typeof(F))")

macro assertposdef(A, info)
   :($(esc(info)) == 0 ? $(esc(A)) : throw(PosDefException($(esc(info)))))
end

macro assertnonsingular(A, info)
   :($(esc(info)) == 0 ? $(esc(A)) : throw(SingularException($(esc(info)))))
end

"""
    issuccess(F::Factorization)

Test that a factorization of a matrix succeeded.

```jldoctest
julia> F = cholfact([1 0; 0 1]);

julia> LinAlg.issuccess(F)
true

julia> F = lufact([1 0; 0 0]);

julia> LinAlg.issuccess(F)
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

Base.hash(F::Factorization, h::UInt) = mapreduce(f -> hash(getfield(F, f)), hash, h, 1:nfields(F))
Base.:(==)(  F::T, G::T) where {T<:Factorization} = all(f -> getfield(F, f) == getfield(G, f), 1:nfields(F))
Base.isequal(F::T, G::T) where {T<:Factorization} = all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\)(F::Factorization{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    c2r = reshape(copy(transpose(reinterpret(T, reshape(B, (1, length(B)))))), size(B, 1), 2*size(B, 2))
    x = ldiv!(F, c2r)
    return reshape(copy(reinterpret(Complex{T}, copy(transpose(reshape(x, div(length(x), 2), 2))))), _ret_size(F, B))
end

function \(F::Factorization, B::AbstractVecOrMat)
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    BB = similar(B, TFB, size(B))
    copyto!(BB, B)
    ldiv!(F, BB)
end
function \(adjF::Adjoint{<:Any,<:Factorization}, B::AbstractVecOrMat)
    F = adjF.parent
    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    BB = similar(B, TFB, size(B))
    copyto!(BB, B)
    ldiv!(adjoint(F), BB)
end

# support the same 3-arg idiom as in our other in-place A_*_B functions:
ldiv!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat) = ldiv!(A, copyto!(Y, B))
ldiv!(Y::AbstractVecOrMat, adjA::Adjoint{<:Any,<:Factorization}, B::AbstractVecOrMat) =
    (A = adjA.parent; ldiv!(adjoint(A), copyto!(Y, B)))
ldiv!(Y::AbstractVecOrMat, transA::Transpose{<:Any,<:Factorization}, B::AbstractVecOrMat) =
    (A = transA.parent; ldiv!(transpose(A), copyto!(Y, B)))

# fallback methods for transposed solves
\(F::Transpose{<:Any,<:Factorization{<:Real}}, B::AbstractVecOrMat) = adjoint(F.parent) \ B
\(F::Transpose{<:Any,<:Factorization}, B::AbstractVecOrMat) = conj.(adjoint(F.parent) \ conj.(B))