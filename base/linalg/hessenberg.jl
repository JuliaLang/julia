# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Hessenberg{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    Hessenberg{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} =
        new(factors, τ)
end
Hessenberg(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = Hessenberg{T,typeof(factors)}(factors, τ)

Hessenberg(A::StridedMatrix) = Hessenberg(LAPACK.gehrd!(A)...)


"""
    hessfact!(A) -> Hessenberg

`hessfact!` is the same as [`hessfact`](@ref), but saves space by overwriting
the input `A`, instead of creating a copy.
"""
hessfact!(A::StridedMatrix{<:BlasFloat}) = Hessenberg(A)

hessfact(A::StridedMatrix{<:BlasFloat}) = hessfact!(copy(A))

"""
    hessfact(A) -> Hessenberg

Compute the Hessenberg decomposition of `A` and return a `Hessenberg` object. If `F` is the
factorization object, the unitary matrix can be accessed with `F[:Q]` and the Hessenberg
matrix with `F[:H]`. When `Q` is extracted, the resulting type is the `HessenbergQ` object,
and may be converted to a regular matrix with [`convert(Array, _)`](@ref)
 (or `Array(_)` for short).

# Examples
```jldoctest
julia> A = [4. 9. 7.; 4. 4. 1.; 4. 3. 2.]
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0

julia> F = hessfact(A);

julia> F[:Q] * F[:H] * F[:Q]'
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0
```
"""
function hessfact(A::StridedMatrix{T}) where T
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return hessfact!(copy_oftype(A, S))
end

struct HessenbergQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::S
    τ::Vector{T}
    HessenbergQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
HessenbergQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = HessenbergQ{T,typeof(factors)}(factors, τ)
HessenbergQ(A::Hessenberg) = HessenbergQ(A.factors, A.τ)
size(A::HessenbergQ, d) = size(A.factors, d)
size(A::HessenbergQ) = size(A.factors)

function getindex(A::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(A)
    d == :H && return triu(A.factors, -1)
    throw(KeyError(d))
end

function getindex(A::HessenbergQ, i::Integer, j::Integer)
    x = zeros(eltype(A), size(A, 1))
    x[i] = 1
    y = zeros(eltype(A), size(A, 2))
    y[j] = 1
    return dot(x, A_mul_B!(A, y))
end

## reconstruct the original matrix
Matrix(A::HessenbergQ{<:BlasFloat}) = LAPACK.orghr!(1, size(A.factors, 1), copy(A.factors), A.τ)
Array(A::HessenbergQ) = Matrix(A)
AbstractMatrix(F::Hessenberg) = (fq = Array(F[:Q]); (fq * F[:H]) * fq')
AbstractArray(F::Hessenberg) = AbstractMatrix(F)
Matrix(F::Hessenberg) = Array(AbstractArray(F))
Array(F::Hessenberg) = Matrix(F)

A_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
A_mul_B!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
Ac_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X)
A_mul_Bc!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X)


function (*)(Q::HessenbergQ{T}, X::StridedVecOrMat{S}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return A_mul_B!(Q, copy_oftype(X, TT))
end
function (*)(X::StridedVecOrMat{S}, Q::HessenbergQ{T}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return A_mul_B!(copy_oftype(X, TT), Q)
end
function Ac_mul_B(Q::HessenbergQ{T}, X::StridedVecOrMat{S}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return Ac_mul_B!(Q, copy_oftype(X, TT))
end
function A_mul_Bc(X::StridedVecOrMat{S}, Q::HessenbergQ{T}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return A_mul_Bc!(copy_oftype(X, TT), Q)
end
