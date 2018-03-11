# This file is a part of Julia. License is MIT: https://julialang.org/license

# Schur decomposition
struct Schur{Ty,S<:AbstractMatrix} <: Factorization{Ty}
    T::S
    Z::S
    values::Vector
    Schur{Ty,S}(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where {Ty,S} = new(T, Z, values)
end
Schur(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where {Ty} = Schur{Ty, typeof(T)}(T, Z, values)

"""
    schurfact!(A::StridedMatrix) -> F::Schur

Same as [`schurfact`](@ref) but uses the input argument `A` as workspace.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> F = schurfact!(A)
Schur{Float64,Array{Float64,2}}
T factor:
2×2 Array{Float64,2}:
 3.0   9.0
 0.0  -2.0
Z factor:
2×2 Array{Float64,2}:
  0.961524  0.274721
 -0.274721  0.961524
eigenvalues:
2-element Array{Float64,1}:
  3.0
 -2.0

julia> A
2×2 Array{Float64,2}:
 3.0   9.0
 0.0  -2.0
```
"""
schurfact!(A::StridedMatrix{<:BlasFloat}) = Schur(LinearAlgebra.LAPACK.gees!('V', A)...)

"""
    schurfact(A::StridedMatrix) -> F::Schur

Computes the Schur factorization of the matrix `A`. The (quasi) triangular Schur factor can
be obtained from the `Schur` object `F` with either `F.Schur` or `F.T` and the
orthogonal/unitary Schur vectors can be obtained with `F.vectors` or `F.Z` such that
`A = F.vectors * F.Schur * F.vectors'`. The eigenvalues of `A` can be obtained with `F.values`.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> F = schurfact(A)
Schur{Float64,Array{Float64,2}}
T factor:
2×2 Array{Float64,2}:
 3.0   9.0
 0.0  -2.0
Z factor:
2×2 Array{Float64,2}:
  0.961524  0.274721
 -0.274721  0.961524
eigenvalues:
2-element Array{Float64,1}:
  3.0
 -2.0

julia> F.vectors * F.Schur * F.vectors'
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0
```
"""
schurfact(A::StridedMatrix{<:BlasFloat}) = schurfact!(copy(A))
schurfact(A::StridedMatrix{T}) where T = schurfact!(copy_oftype(A, eigtype(T)))

function getproperty(F::Schur, d::Symbol)
    if d == :Schur
        return getfield(F, :T)
    elseif d == :vectors
        return getfield(F, :Z)
    else
        getfield(F, d)
    end
end

Base.propertynames(F::Schur) =
    (:Schur, :vectors, fieldnames(typeof(F))...)

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::Schur)
    println(io, summary(F))
    println(io, "T factor:")
    show(io, mime, F.T)
    println(io, "\nZ factor:")
    show(io, mime, F.Z)
    println(io, "\neigenvalues:")
    show(io, mime, F.values)
end

"""
    schur(A::StridedMatrix) -> T::Matrix, Z::Matrix, λ::Vector

Computes the Schur factorization of the matrix `A`. The methods return the (quasi)
triangular Schur factor `T` and the orthogonal/unitary Schur vectors `Z` such that
`A = Z * T * Z'`. The eigenvalues of `A` are returned in the vector `λ`.

See [`schurfact`](@ref).

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> T, Z, lambda = schur(A)
([3.0 9.0; 0.0 -2.0], [0.961524 0.274721; -0.274721 0.961524], [3.0, -2.0])

julia> Z * Z'
2×2 Array{Float64,2}:
 1.0  0.0
 0.0  1.0

julia> Z * T * Z'
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0
```
"""
function schur(A::StridedMatrix)
    SchurF = schurfact(A)
    SchurF.T, SchurF.Z, SchurF.values
end
schur(A::Symmetric) = schur(copyto!(similar(parent(A)), A))
schur(A::Hermitian) = schur(copyto!(similar(parent(A)), A))
schur(A::UpperTriangular) = schur(copyto!(similar(parent(A)), A))
schur(A::LowerTriangular) = schur(copyto!(similar(parent(A)), A))
schur(A::Tridiagonal) = schur(Matrix(A))


"""
    ordschur!(F::Schur, select::Union{Vector{Bool},BitVector}) -> F::Schur

Same as [`ordschur`](@ref) but overwrites the factorization `F`.
"""
function ordschur!(schur::Schur, select::Union{Vector{Bool},BitVector})
    _, _, vals = ordschur!(schur.T, schur.Z, select)
    schur.values[:] = vals
    return schur
end

"""
    ordschur(F::Schur, select::Union{Vector{Bool},BitVector}) -> F::Schur

Reorders the Schur factorization `F` of a matrix `A = Z*T*Z'` according to the logical array
`select` returning the reordered factorization `F` object. The selected eigenvalues appear
in the leading diagonal of `F.Schur` and the corresponding leading columns of
`F.vectors` form an orthogonal/unitary basis of the corresponding right invariant
subspace. In the real case, a complex conjugate pair of eigenvalues must be either both
included or both excluded via `select`.
"""
ordschur(schur::Schur, select::Union{Vector{Bool},BitVector}) =
    Schur(ordschur(schur.T, schur.Z, select)...)

"""
    ordschur!(T::StridedMatrix, Z::StridedMatrix, select::Union{Vector{Bool},BitVector}) -> T::StridedMatrix, Z::StridedMatrix, λ::Vector

Same as [`ordschur`](@ref) but overwrites the input arguments.
"""
ordschur!(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
    LinearAlgebra.LAPACK.trsen!(convert(Vector{BlasInt}, select), T, Z)[1:3]

"""
    ordschur(T::StridedMatrix, Z::StridedMatrix, select::Union{Vector{Bool},BitVector}) -> T::StridedMatrix, Z::StridedMatrix, λ::Vector

Reorders the Schur factorization of a real matrix `A = Z*T*Z'` according to the logical
array `select` returning the reordered matrices `T` and `Z` as well as the vector of
eigenvalues `λ`. The selected eigenvalues appear in the leading diagonal of `T` and the
corresponding leading columns of `Z` form an orthogonal/unitary basis of the corresponding
right invariant subspace. In the real case, a complex conjugate pair of eigenvalues must be
either both included or both excluded via `select`.
"""
ordschur(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
    ordschur!(copy(T), copy(Z), select)

struct GeneralizedSchur{Ty,M<:AbstractMatrix} <: Factorization{Ty}
    S::M
    T::M
    α::Vector
    β::Vector{Ty}
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
schurfact!(A::StridedMatrix{T}, B::StridedMatrix{T}) where {T<:BlasFloat} =
    GeneralizedSchur(LinearAlgebra.LAPACK.gges!('V', 'V', A, B)...)

"""
    schurfact(A::StridedMatrix, B::StridedMatrix) -> F::GeneralizedSchur

Computes the Generalized Schur (or QZ) factorization of the matrices `A` and `B`. The
(quasi) triangular Schur factors can be obtained from the `Schur` object `F` with `F.S`
and `F.T`, the left unitary/orthogonal Schur vectors can be obtained with `F.left` or
`F.Q` and the right unitary/orthogonal Schur vectors can be obtained with `F.right` or
`F.Z` such that `A=F.left*F.S*F.right'` and `B=F.left*F.T*F.right'`. The
generalized eigenvalues of `A` and `B` can be obtained with `F.α./F.β`.
"""
schurfact(A::StridedMatrix{T},B::StridedMatrix{T}) where {T<:BlasFloat} = schurfact!(copy(A),copy(B))
function schurfact(A::StridedMatrix{TA}, B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA), TB)
    return schurfact!(copy_oftype(A, S), copy_oftype(B, S))
end

"""
    ordschur!(F::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) -> F::GeneralizedSchur

Same as `ordschur` but overwrites the factorization `F`.
"""
function ordschur!(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector})
    _, _, α, β, _, _ = ordschur!(gschur.S, gschur.T, gschur.Q, gschur.Z, select)
    gschur.α[:] = α
    gschur.β[:] = β
    return gschur
end

"""
    ordschur(F::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) -> F::GeneralizedSchur

Reorders the Generalized Schur factorization `F` of a matrix pair `(A, B) = (Q*S*Z', Q*T*Z')`
according to the logical array `select` and returns a GeneralizedSchur object `F`. The
selected eigenvalues appear in the leading diagonal of both `F.S` and `F.T`, and the
left and right orthogonal/unitary Schur vectors are also reordered such that
`(A, B) = F.Q*(F.S, F.T)*F.Z'` still holds and the generalized eigenvalues of `A`
and `B` can still be obtained with `F.α./F.β`.
"""
ordschur(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) =
    GeneralizedSchur(ordschur(gschur.S, gschur.T, gschur.Q, gschur.Z, select)...)

"""
    ordschur!(S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, select) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

Same as [`ordschur`](@ref) but overwrites the factorization the input arguments.
"""
ordschur!(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
        LinearAlgebra.LAPACK.tgsen!(convert(Vector{BlasInt}, select), S, T, Q, Z)

"""
    ordschur(S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, select) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

Reorders the Generalized Schur factorization of a matrix pair `(A, B) = (Q*S*Z', Q*T*Z')`
according to the logical array `select` and returns the matrices `S`, `T`, `Q`, `Z` and
vectors `α` and `β`.  The selected eigenvalues appear in the leading diagonal of both `S`
and `T`, and the left and right unitary/orthogonal Schur vectors are also reordered such
that `(A, B) = Q*(S, T)*Z'` still holds and the generalized eigenvalues of `A` and `B` can
still be obtained with `α./β`.
"""
ordschur(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
        ordschur!(copy(S), copy(T), copy(Q), copy(Z), select)

function getproperty(F::GeneralizedSchur, d::Symbol)
    if d == :values
        return getfield(F, :α) ./ getfield(F, :β)
    elseif d == :alpha
        return getfield(F, :α)
    elseif d == :beta
        return getfield(F, :β)
    elseif d == :left
        return getfield(F, :Q)
    elseif d == :right
        return getfield(F, :Z)
    else
        getfield(F, d)
    end
end

Base.propertynames(F::GeneralizedSchur) =
    (:values, :left, :right, fieldnames(typeof(F))...)

"""
    schur(A::StridedMatrix, B::StridedMatrix) -> S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix, α::Vector, β::Vector

See [`schurfact`](@ref).
"""
function schur(A::StridedMatrix, B::StridedMatrix)
    SchurF = schurfact(A, B)
    SchurF.S, SchurF.T, SchurF.Q, SchurF.Z, SchurF.α, SchurF.β
end

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::GeneralizedSchur)
    println(io, summary(F))
    println(io, "S factor:")
    show(io, mime, F.S)
    println(io, "\nT factor:")
    show(io, mime, F.T)
    println(io, "\nQ factor:")
    show(io, mime, F.Q)
    println(io, "\nZ factor:")
    show(io, mime, F.Z)
    println(io, "\nα:")
    show(io, mime, F.α)
    println(io, "\nβ:")
    show(io, mime, F.β)
end

# Conversion
AbstractMatrix(F::Schur) = (F.Z * F.T) * F.Z'
AbstractArray(F::Schur) = AbstractMatrix(F)
Matrix(F::Schur) = Array(AbstractArray(F))
Array(F::Schur) = Matrix(F)

copy(F::Schur) = Schur(copy(F.T), copy(F.Z), copy(F.values))
copy(F::GeneralizedSchur) = GeneralizedSchur(copy(F.S), copy(F.T), copy(F.α), copy(F.β), copy(F.Q), copy(F.Z))
