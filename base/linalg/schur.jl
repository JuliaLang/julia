# This file is a part of Julia. License is MIT: http://julialang.org/license

# Schur decomposition
immutable Schur{Ty,S<:AbstractMatrix} <: Factorization{Ty}
    T::S
    Z::S
    values::Vector
    Schur{Ty,S}(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where {Ty,S} = new(T, Z, values)
end
Schur(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where Ty = Schur{Ty, typeof(T)}(T, Z, values)

"""
    schurfact!(A::StridedMatrix) -> F::Schur

Same as [`schurfact`](@ref) but uses the input argument as workspace.
"""
schurfact!{T<:BlasFloat}(A::StridedMatrix{T}) = Schur(LinAlg.LAPACK.gees!('V', A)...)

"""
    schurfact(A::StridedMatrix) -> F::Schur

Computes the Schur factorization of the matrix `A`. The (quasi) triangular Schur factor can
be obtained from the `Schur` object `F` with either `F[:Schur]` or `F[:T]` and the
orthogonal/unitary Schur vectors can be obtained with `F[:vectors]` or `F[:Z]` such that
`A = F[:vectors]*F[:Schur]*F[:vectors]'`. The eigenvalues of `A` can be obtained with `F[:values]`.

# Example

```jldoctest
julia> A = [-2. 1. 3.; 2. 1. -1.; -7. 2. 7.]
3×3 Array{Float64,2}:
 -2.0  1.0   3.0
  2.0  1.0  -1.0
 -7.0  2.0   7.0

julia> F = schurfact(A)
Base.LinAlg.Schur{Float64,Array{Float64,2}}([2.0 0.801792 6.63509; -8.55988e-11 2.0 8.08286; 0.0 0.0 1.99999], [0.577351 0.154299 -0.801784; 0.577346 -0.77152 0.267262; 0.577354 0.617211 0.534522], Complex{Float64}[2.0+8.28447e-6im, 2.0-8.28447e-6im, 1.99999+0.0im])

julia> F[:vectors] * F[:Schur] * F[:vectors]'
3×3 Array{Float64,2}:
 -2.0  1.0   3.0
  2.0  1.0  -1.0
 -7.0  2.0   7.0
```
"""
schurfact{T<:BlasFloat}(A::StridedMatrix{T}) = schurfact!(copy(A))
function schurfact{T}(A::StridedMatrix{T})
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    return schurfact!(copy_oftype(A, S))
end

function getindex(F::Schur, d::Symbol)
    if d == :T || d == :Schur
        return F.T
    elseif d == :Z || d == :vectors
        return F.Z
    elseif d == :values
        return F.values
    else
        throw(KeyError(d))
    end
end

"""
    schur(A::StridedMatrix) -> T::Matrix, Z::Matrix, λ::Vector

Computes the Schur factorization of the matrix `A`. The methods return the (quasi)
triangular Schur factor `T` and the orthogonal/unitary Schur vectors `Z` such that
`A = Z*T*Z'`. The eigenvalues of `A` are returned in the vector `λ`.

See [`schurfact`](@ref).

# Example

```jldoctest
julia> A = [-2. 1. 3.; 2. 1. -1.; -7. 2. 7.]
3×3 Array{Float64,2}:
 -2.0  1.0   3.0
  2.0  1.0  -1.0
 -7.0  2.0   7.0

julia> T, Z, lambda = schur(A)
([2.0 0.801792 6.63509; -8.55988e-11 2.0 8.08286; 0.0 0.0 1.99999], [0.577351 0.154299 -0.801784; 0.577346 -0.77152 0.267262; 0.577354 0.617211 0.534522], Complex{Float64}[2.0+8.28447e-6im, 2.0-8.28447e-6im, 1.99999+0.0im])

julia> Z * T * Z'
3×3 Array{Float64,2}:
 -2.0  1.0   3.0
  2.0  1.0  -1.0
 -7.0  2.0   7.0
```
"""
function schur(A::StridedMatrix)
    SchurF = schurfact(A)
    SchurF[:T], SchurF[:Z], SchurF[:values]
end

"""
    ordschur!(F::Schur, select::Union{Vector{Bool},BitVector}) -> F::Schur

Same as [`ordschur`](@ref) but overwrites the factorization `F`.
"""
function ordschur!(schur::Schur, select::Union{Vector{Bool},BitVector})
    _, _, vals = ordschur!(schur.T, schur.Z, select)
    schur[:values][:] = vals
    return schur
end

"""
    ordschur(F::Schur, select::Union{Vector{Bool},BitVector}) -> F::Schur

Reorders the Schur factorization `F` of a matrix `A = Z*T*Z'` according to the logical array
`select` returning the reordered factorization `F` object. The selected eigenvalues appear
in the leading diagonal of `F[:Schur]` and the corresponding leading columns of
`F[:vectors]` form an orthogonal/unitary basis of the corresponding right invariant
subspace. In the real case, a complex conjugate pair of eigenvalues must be either both
included or both excluded via `select`.
"""
ordschur(schur::Schur, select::Union{Vector{Bool},BitVector}) =
    Schur(ordschur(schur.T, schur.Z, select)...)

"""
    ordschur!(T::StridedMatrix, Z::StridedMatrix, select::Union{Vector{Bool},BitVector}) -> T::StridedMatrix, Z::StridedMatrix, λ::Vector

Same as [`ordschur`](@ref) but overwrites the input arguments.
"""
ordschur!{Ty<:BlasFloat}(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) =
    LinAlg.LAPACK.trsen!(convert(Vector{BlasInt}, select), T, Z)

"""
    ordschur(T::StridedMatrix, Z::StridedMatrix, select::Union{Vector{Bool},BitVector}) -> T::StridedMatrix, Z::StridedMatrix, λ::Vector

Reorders the Schur factorization of a real matrix `A = Z*T*Z'` according to the logical
array `select` returning the reordered matrices `T` and `Z` as well as the vector of
eigenvalues `λ`. The selected eigenvalues appear in the leading diagonal of `T` and the
corresponding leading columns of `Z` form an orthogonal/unitary basis of the corresponding
right invariant subspace. In the real case, a complex conjugate pair of eigenvalues must be
either both included or both excluded via `select`.
"""
ordschur{Ty<:BlasFloat}(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) =
    ordschur!(copy(T), copy(Z), select)

immutable GeneralizedSchur{Ty,M<:AbstractMatrix} <: Factorization{Ty}
    S::M
    T::M
    alpha::Vector
    beta::Vector{Ty}
    Q::M
    Z::M
    function GeneralizedSchur{Ty,M}(S::AbstractMatrix{Ty}, T::AbstractMatrix{Ty}, alpha::Vector,
                                    beta::Vector{Ty}, Q::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}) where {Ty,M}
        new(S, T, alpha, beta, Q, Z)
    end
end
function GeneralizedSchur(S::AbstractMatrix{Ty}, T::AbstractMatrix{Ty}, alpha::Vector,
                          beta::Vector{Ty}, Q::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}) where Ty
    GeneralizedSchur{Ty, typeof(S)}(S, T, alpha, beta, Q, Z)
end

"""
    schurfact!(A::StridedMatrix, B::StridedMatrix) -> F::GeneralizedSchur

Same as [`schurfact`](@ref) but uses the input matrices `A` and `B` as workspace.
"""
schurfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) =
    GeneralizedSchur(LinAlg.LAPACK.gges!('V', 'V', A, B)...)

"""
    schurfact(A::StridedMatrix, B::StridedMatrix) -> F::GeneralizedSchur

Computes the Generalized Schur (or QZ) factorization of the matrices `A` and `B`. The
(quasi) triangular Schur factors can be obtained from the `Schur` object `F` with `F[:S]`
and `F[:T]`, the left unitary/orthogonal Schur vectors can be obtained with `F[:left]` or
`F[:Q]` and the right unitary/orthogonal Schur vectors can be obtained with `F[:right]` or
`F[:Z]` such that `A=F[:left]*F[:S]*F[:right]'` and `B=F[:left]*F[:T]*F[:right]'`. The
generalized eigenvalues of `A` and `B` can be obtained with `F[:alpha]./F[:beta]`.
"""
schurfact{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T}) = schurfact!(copy(A),copy(B))
function schurfact{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))), TB)
    return schurfact!(copy_oftype(A, S), copy_oftype(B, S))
end

"""
    ordschur!(F::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) -> F::GeneralizedSchur

Same as `ordschur` but overwrites the factorization `F`.
"""
function ordschur!(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector})
    _, _, α, β, _, _ = ordschur!(gschur.S, gschur.T, gschur.Q, gschur.Z, select)
    gschur[:alpha][:] = α
    gschur[:beta][:] = β
    return gschur
end

"""
    ordschur(F::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) -> F::GeneralizedSchur

Reorders the Generalized Schur factorization `F` of a matrix pair `(A, B) = (Q*S*Z', Q*T*Z')`
according to the logical array `select` and returns a GeneralizedSchur object `F`. The
selected eigenvalues appear in the leading diagonal of both `F[:S]` and `F[:T]`, and the
left and right orthogonal/unitary Schur vectors are also reordered such that
`(A, B) = F[:Q]*(F[:S], F[:T])*F[:Z]'` still holds and the generalized eigenvalues of `A`
and `B` can still be obtained with `F[:alpha]./F[:beta]`.
"""
ordschur(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) =
    GeneralizedSchur(ordschur(gschur.S, gschur.T, gschur.Q, gschur.Z, select)...)

"""
    ordschur!(S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, select) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

Same as [`ordschur`](@ref) but overwrites the factorization the input arguments.
"""
ordschur!{Ty<:BlasFloat}(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) =
        LinAlg.LAPACK.tgsen!(convert(Vector{BlasInt}, select), S, T, Q, Z)

"""
    ordschur(S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, select) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

Reorders the Generalized Schur factorization of a matrix pair `(A, B) = (Q*S*Z', Q*T*Z')`
according to the logical array `select` and returns the matrices `S`, `T`, `Q`, `Z` and
vectors `α` and `β`.  The selected eigenvalues appear in the leading diagonal of both `S`
and `T`, and the left and right unitary/orthogonal Schur vectors are also reordered such
that `(A, B) = Q*(S, T)*Z'` still holds and the generalized eigenvalues of `A` and `B` can
still be obtained with `α./β`.
"""
ordschur{Ty<:BlasFloat}(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) =
        ordschur!(copy(S), copy(T), copy(Q), copy(Z), select)

function getindex(F::GeneralizedSchur, d::Symbol)
    if d == :S
        return F.S
    elseif d == :T
        return F.T
    elseif d == :alpha
        return F.alpha
    elseif d == :beta
        return F.beta
    elseif d == :values
        return F.alpha./F.beta
    elseif d == :Q || d == :left
        return F.Q
    elseif d == :Z || d == :right
        return F.Z
    else
        throw(KeyError(d))
    end
end

"""
    schur(A::StridedMatrix, B::StridedMatrix) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

See [`schurfact`](@ref).
"""
function schur(A::StridedMatrix, B::StridedMatrix)
    SchurF = schurfact(A, B)
    SchurF[:S], SchurF[:T], SchurF[:Q], SchurF[:Z], SchurF[:alpha], SchurF[:beta]
end

# Conversion
convert(::Type{AbstractMatrix}, F::Schur) = (F.Z * F.T) * F.Z'
convert(::Type{AbstractArray}, F::Schur) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::Schur) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::Schur) = convert(Matrix, F)
full(F::Schur) = convert(AbstractArray, F)

copy(F::Schur) = Schur(copy(F.T), copy(F.Z), copy(F.values))
copy(F::GeneralizedSchur) = GeneralizedSchur(copy(F.S), copy(F.T), copy(F.alpha), copy(F.beta), copy(F.Q), copy(F.Z))
