# This file is a part of Julia. License is MIT: http://julialang.org/license

# Schur decomposition
immutable Schur{Ty<:BlasFloat, S<:AbstractMatrix} <: Factorization{Ty}
    T::S
    Z::S
    values::Vector
    Schur(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) = new(T, Z, values)
end
Schur{Ty}(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) = Schur{Ty, typeof(T)}(T, Z, values)

schurfact!{T<:BlasFloat}(A::StridedMatrix{T}) = Schur(LinAlg.LAPACK.gees!('V', A)...)
schurfact{T<:BlasFloat}(A::StridedMatrix{T}) = schurfact!(copy(A))
function schurfact{T}(A::StridedMatrix{T})
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    return schurfact!(copy_oftype(A, S))
end

function getindex(F::Schur, d::Symbol)
    (d == :T || d == :Schur) && return F.T
    (d == :Z || d == :vectors) && return F.Z
    d == :values && return F.values
    throw(KeyError(d))
end

function schur(A::StridedMatrix)
    SchurF = schurfact(A)
    SchurF[:T], SchurF[:Z], SchurF[:values]
end

ordschur!{Ty<:BlasFloat}(Q::StridedMatrix{Ty}, T::StridedMatrix{Ty}, select::Union(Vector{Bool},BitVector)) = Schur(LinAlg.LAPACK.trsen!(convert(Vector{BlasInt}, select), T , Q)...)
ordschur{Ty<:BlasFloat}(Q::StridedMatrix{Ty}, T::StridedMatrix{Ty}, select::Union(Vector{Bool},BitVector)) = ordschur!(copy(Q), copy(T), select)
ordschur!{Ty<:BlasFloat}(schur::Schur{Ty}, select::Union(Vector{Bool},BitVector)) = (res=ordschur!(schur.Z, schur.T, select); schur[:values][:]=res[:values]; res)
ordschur{Ty<:BlasFloat}(schur::Schur{Ty}, select::Union(Vector{Bool},BitVector)) = ordschur(schur.Z, schur.T, select)

immutable GeneralizedSchur{Ty<:BlasFloat, M<:AbstractMatrix} <: Factorization{Ty}
    S::M
    T::M
    alpha::Vector
    beta::Vector{Ty}
    Q::M
    Z::M
    GeneralizedSchur(S::AbstractMatrix{Ty}, T::AbstractMatrix{Ty}, alpha::Vector, beta::Vector{Ty}, Q::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}) = new(S, T, alpha, beta, Q, Z)
end
GeneralizedSchur{Ty}(S::AbstractMatrix{Ty}, T::AbstractMatrix{Ty}, alpha::Vector, beta::Vector{Ty}, Q::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}) = GeneralizedSchur{Ty, typeof(S)}(S, T, alpha, beta, Q, Z)

schurfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = GeneralizedSchur(LinAlg.LAPACK.gges!('V', 'V', A, B)...)
schurfact{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T}) = schurfact!(copy(A),copy(B))
function schurfact{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))), TB)
    return schurfact!(copy_oftype(A, S), copy_oftype(B, S))
end

ordschur!{Ty<:BlasFloat}(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union(Vector{Bool},BitVector)) = GeneralizedSchur(LinAlg.LAPACK.tgsen!(convert(Vector{BlasInt}, select), S, T, Q, Z)...)
ordschur{Ty<:BlasFloat}(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union(Vector{Bool},BitVector)) = ordschur!(copy(S), copy(T), copy(Q), copy(Z), select)
ordschur!{Ty<:BlasFloat}(gschur::GeneralizedSchur{Ty}, select::Union(Vector{Bool},BitVector)) = (res=ordschur!(gschur.S, gschur.T, gschur.Q, gschur.Z, select); gschur[:alpha][:]=res[:alpha]; gschur[:beta][:]=res[:beta]; res)
ordschur{Ty<:BlasFloat}(gschur::GeneralizedSchur{Ty}, select::Union(Vector{Bool},BitVector)) = ordschur(gschur.S, gschur.T, gschur.Q, gschur.Z, select)

function getindex(F::GeneralizedSchur, d::Symbol)
    d == :S && return F.S
    d == :T && return F.T
    d == :alpha && return F.alpha
    d == :beta && return F.beta
    d == :values && return F.alpha./F.beta
    (d == :Q || d == :left) && return F.Q
    (d == :Z || d == :right) && return F.Z
    throw(KeyError(d))
end

function schur(A::StridedMatrix, B::StridedMatrix)
    SchurF = schurfact(A, B)
    SchurF[:S], SchurF[:T], SchurF[:Q], SchurF[:Z]
end

full(F::Schur) = (F.Z * F.T) * F.Z'
