# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Hessenberg{T,S<:AbstractMatrix{T}} <: Factorization{T}
    factors::S
    τ::Vector{T}

    function Hessenberg{T,S}(factors, τ) where {T,S<:AbstractMatrix{T}}
        @assert !has_offset_axes(factors, τ)
        new{T,S}(factors, τ)
    end
end
Hessenberg(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = Hessenberg{T,typeof(factors)}(factors, τ)
function Hessenberg{T}(factors::AbstractMatrix, τ::AbstractVector) where {T}
    Hessenberg(convert(AbstractMatrix{T}, factors), convert(Vector{T}, v))
end

Hessenberg(A::StridedMatrix) = Hessenberg(LAPACK.gehrd!(A)...)

# iteration for destructuring into components
Base.iterate(S::Hessenberg) = (S.Q, Val(:H))
Base.iterate(S::Hessenberg, ::Val{:H}) = (S.H, Val(:done))
Base.iterate(S::Hessenberg, ::Val{:done}) = nothing

"""
    hessenberg!(A) -> Hessenberg

`hessenberg!` is the same as [`hessenberg`](@ref), but saves space by overwriting
the input `A`, instead of creating a copy.
"""
hessenberg!(A::StridedMatrix{<:BlasFloat}) = Hessenberg(A)

hessenberg(A::StridedMatrix{<:BlasFloat}) = hessenberg!(copy(A))

"""
    hessenberg(A) -> Hessenberg

Compute the Hessenberg decomposition of `A` and return a `Hessenberg` object. If `F` is the
factorization object, the unitary matrix can be accessed with `F.Q` and the Hessenberg
matrix with `F.H`. When `Q` is extracted, the resulting type is the `HessenbergQ` object,
and may be converted to a regular matrix with [`convert(Array, _)`](@ref)
 (or `Array(_)` for short).

Iterating the decomposition produces the factors `F.Q` and `F.H`.

# Examples
```jldoctest
julia> A = [4. 9. 7.; 4. 4. 1.; 4. 3. 2.]
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0

julia> F = hessenberg(A);

julia> F.Q * F.H * F.Q'
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0

julia> q, h = F; # destructuring via iteration

julia> q == F.Q && h == F.H
true
```
"""
hessenberg(A::StridedMatrix{T}) where T =
    hessenberg!(copy_oftype(A, eigtype(T)))

struct HessenbergQ{T,S<:AbstractMatrix} <: AbstractQ{T}
    factors::S
    τ::Vector{T}
    function HessenbergQ{T,S}(factors, τ) where {T,S<:AbstractMatrix}
        @assert !has_offset_axes(factors)
        new(factors, τ)
    end
end
HessenbergQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = HessenbergQ{T,typeof(factors)}(factors, τ)
HessenbergQ(A::Hessenberg) = HessenbergQ(A.factors, A.τ)

function getproperty(F::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(F)
    d == :H && return triu(getfield(F, :factors), -1)
    return getfield(F, d)
end

Base.propertynames(F::Hessenberg, private::Bool=false) =
    (:Q, :H, (private ? fieldnames(typeof(F)) : ())...)

## reconstruct the original matrix
Matrix{T}(Q::HessenbergQ) where {T} = convert(Matrix{T}, LAPACK.orghr!(1, size(Q.factors, 1), copy(Q.factors), Q.τ))
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
