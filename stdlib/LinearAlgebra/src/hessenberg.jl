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
factorization object, the unitary matrix can be accessed with `F.Q` and the Hessenberg
matrix with `F.H`. When `Q` is extracted, the resulting type is the `HessenbergQ` object,
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

julia> F.Q * F.H * F.Q'
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0
```
"""
hessfact(A::StridedMatrix{T}) where T =
    hessfact!(copy_oftype(A, eigtype(T)))

struct HessenbergQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::S
    τ::Vector{T}
    HessenbergQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
HessenbergQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = HessenbergQ{T,typeof(factors)}(factors, τ)
HessenbergQ(A::Hessenberg) = HessenbergQ(A.factors, A.τ)
size(A::HessenbergQ, d) = size(A.factors, d)
size(A::HessenbergQ) = size(A.factors)

function getproperty(F::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(F)
    d == :H && return triu(getfield(F, :factors), -1)
    return getfield(F, d)
end

Base.propertynames(F::Hessenberg, private::Bool=false) =
    (:Q, :H, (private ? fieldnames(typeof(F)) : ())...)

function getindex(A::HessenbergQ, i::Integer, j::Integer)
    x = zeros(eltype(A), size(A, 1))
    x[i] = 1
    y = zeros(eltype(A), size(A, 2))
    y[j] = 1
    return dot(x, lmul!(A, y))
end

## reconstruct the original matrix
Matrix(A::HessenbergQ{<:BlasFloat}) = LAPACK.orghr!(1, size(A.factors, 1), copy(A.factors), A.τ)
Array(A::HessenbergQ) = Matrix(A)
AbstractMatrix(F::Hessenberg) = (fq = Array(F.Q); (fq * F.H) * fq')
AbstractArray(F::Hessenberg) = AbstractMatrix(F)
Matrix(F::Hessenberg) = Array(AbstractArray(F))
Array(F::Hessenberg) = Matrix(F)

lmul!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
rmul!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
lmul!(adjQ::Adjoint{<:Any,<:HessenbergQ{T}}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('L', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))
rmul!(X::StridedMatrix{T}, adjQ::Adjoint{<:Any,<:HessenbergQ{T}}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('R', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))

function (*)(Q::HessenbergQ{T}, X::StridedVecOrMat{S}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return lmul!(Q, copy_oftype(X, TT))
end
function (*)(X::StridedVecOrMat{S}, Q::HessenbergQ{T}) where {T,S}
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return rmul!(copy_oftype(X, TT), Q)
end
function *(adjQ::Adjoint{<:Any,<:HessenbergQ{T}}, X::StridedVecOrMat{S}) where {T,S}
    Q = adjQ.parent
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return lmul!(adjoint(Q), copy_oftype(X, TT))
end
function *(X::StridedVecOrMat{S}, adjQ::Adjoint{<:Any,<:HessenbergQ{T}}) where {T,S}
    Q = adjQ.parent
    TT = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    return rmul!(copy_oftype(X, TT), adjoint(Q))
end
