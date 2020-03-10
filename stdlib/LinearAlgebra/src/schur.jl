# This file is a part of Julia. License is MIT: https://julialang.org/license

# Schur decomposition
"""
    Schur <: Factorization

Matrix factorization type of the Schur factorization of a matrix `A`. This is the
return type of [`schur(_)`](@ref), the corresponding matrix factorization function.

If `F::Schur` is the factorization object, the (quasi) triangular Schur factor can
be obtained via either `F.Schur` or `F.T` and the orthogonal/unitary Schur vectors
via `F.vectors` or `F.Z` such that `A = F.vectors * F.Schur * F.vectors'`. The
eigenvalues of `A` can be obtained with `F.values`.

Iterating the decomposition produces the components `F.T`, `F.Z`, and `F.values`.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> F = schur(A)
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

julia> t, z, vals = F; # destructuring via iteration

julia> t == F.T && z == F.Z && vals == F.values
true
```
"""
struct Schur{Ty,S<:AbstractMatrix} <: Factorization{Ty}
    T::S
    Z::S
    values::Vector
    Schur{Ty,S}(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where {Ty,S} = new(T, Z, values)
end
Schur(T::AbstractMatrix{Ty}, Z::AbstractMatrix{Ty}, values::Vector) where {Ty} = Schur{Ty, typeof(T)}(T, Z, values)

# iteration for destructuring into components
Base.iterate(S::Schur) = (S.T, Val(:Z))
Base.iterate(S::Schur, ::Val{:Z}) = (S.Z, Val(:values))
Base.iterate(S::Schur, ::Val{:values}) = (S.values, Val(:done))
Base.iterate(S::Schur, ::Val{:done}) = nothing

"""
    schur!(A::StridedMatrix) -> F::Schur

Same as [`schur`](@ref) but uses the input argument `A` as workspace.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> F = schur!(A)
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
schur!(A::StridedMatrix{<:BlasFloat}) = Schur(LinearAlgebra.LAPACK.gees!('V', A)...)

"""
    schur(A::StridedMatrix) -> F::Schur

Computes the Schur factorization of the matrix `A`. The (quasi) triangular Schur factor can
be obtained from the `Schur` object `F` with either `F.Schur` or `F.T` and the
orthogonal/unitary Schur vectors can be obtained with `F.vectors` or `F.Z` such that
`A = F.vectors * F.Schur * F.vectors'`. The eigenvalues of `A` can be obtained with `F.values`.

Iterating the decomposition produces the components `F.T`, `F.Z`, and `F.values`.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Array{Float64,2}:
  5.0   7.0
 -2.0  -4.0

julia> F = schur(A)
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

julia> t, z, vals = F; # destructuring via iteration

julia> t == F.T && z == F.Z && vals == F.values
true
```
"""
schur(A::StridedMatrix{<:BlasFloat}) = schur!(copy(A))
schur(A::StridedMatrix{T}) where T = schur!(copy_oftype(A, eigtype(T)))

schur(A::Symmetric) = schur(copyto!(similar(parent(A)), A))
schur(A::Symmetric{<:Real}) = _schur(A)
schur(A::Hermitian) = _schur(A)
function _schur(A::RealHermSymComplexHerm)
    F = eigen(A; sortby=nothing)
    return Schur(typeof(F.vectors)(Diagonal(F.values)), F.vectors, F.values)
end
schur(A::UpperTriangular) = schur(copyto!(similar(parent(A)), A))
schur(A::LowerTriangular) = schur(copyto!(similar(parent(A)), A))
schur(A::Tridiagonal) = schur(Matrix(A))

function getproperty(F::Schur, d::Symbol)
    if d === :Schur
        return getfield(F, :T)
    elseif d === :vectors
        return getfield(F, :Z)
    else
        getfield(F, d)
    end
end

Base.propertynames(F::Schur) =
    (:Schur, :vectors, fieldnames(typeof(F))...)

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::Schur)
    summary(io, F); println(io)
    println(io, "T factor:")
    show(io, mime, F.T)
    println(io, "\nZ factor:")
    show(io, mime, F.Z)
    println(io, "\neigenvalues:")
    show(io, mime, F.values)
end

"""
    ordschur!(F::Schur, select::Union{Vector{Bool},BitVector}) -> F::Schur

Same as [`ordschur`](@ref) but overwrites the factorization `F`.
"""
function ordschur!(schur::Schur, select::Union{Vector{Bool},BitVector})
    _, _, vals = _ordschur!(schur.T, schur.Z, select)
    schur.values[:] = vals
    return schur
end

_ordschur(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
    _ordschur!(copy(T), copy(Z), select)

_ordschur!(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
    LinearAlgebra.LAPACK.trsen!(convert(Vector{BlasInt}, select), T, Z)[1:3]

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
    Schur(_ordschur(schur.T, schur.Z, select)...)

"""
    GeneralizedSchur <: Factorization

Matrix factorization type of the generalized Schur factorization of two matrices
`A` and `B`. This is the return type of [`schur(_, _)`](@ref), the corresponding
matrix factorization function.

If `F::GeneralizedSchur` is the factorization object, the (quasi) triangular Schur
factors can be obtained via `F.S` and `F.T`, the left unitary/orthogonal Schur
vectors via `F.left` or `F.Q`, and the right unitary/orthogonal Schur vectors can
be obtained with `F.right` or `F.Z` such that `A=F.left*F.S*F.right'` and
`B=F.left*F.T*F.right'`. The generalized eigenvalues of `A` and `B` can be obtained
with `F.α./F.β`.

Iterating the decomposition produces the components `F.S`, `F.T`, `F.Q`, `F.Z`,
`F.α`, and `F.β`.
"""
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

# iteration for destructuring into components
Base.iterate(S::GeneralizedSchur) = (S.S, Val(:T))
Base.iterate(S::GeneralizedSchur, ::Val{:T}) = (S.T, Val(:Q))
Base.iterate(S::GeneralizedSchur, ::Val{:Q}) = (S.Q, Val(:Z))
Base.iterate(S::GeneralizedSchur, ::Val{:Z}) = (S.Z, Val(:α))
Base.iterate(S::GeneralizedSchur, ::Val{:α}) = (S.α, Val(:β))
Base.iterate(S::GeneralizedSchur, ::Val{:β}) = (S.β, Val(:done))
Base.iterate(S::GeneralizedSchur, ::Val{:done}) = nothing

"""
    schur!(A::StridedMatrix, B::StridedMatrix) -> F::GeneralizedSchur

Same as [`schur`](@ref) but uses the input matrices `A` and `B` as workspace.
"""
schur!(A::StridedMatrix{T}, B::StridedMatrix{T}) where {T<:BlasFloat} =
    GeneralizedSchur(LinearAlgebra.LAPACK.gges!('V', 'V', A, B)...)

"""
    schur(A::StridedMatrix, B::StridedMatrix) -> F::GeneralizedSchur

Computes the Generalized Schur (or QZ) factorization of the matrices `A` and `B`. The
(quasi) triangular Schur factors can be obtained from the `Schur` object `F` with `F.S`
and `F.T`, the left unitary/orthogonal Schur vectors can be obtained with `F.left` or
`F.Q` and the right unitary/orthogonal Schur vectors can be obtained with `F.right` or
`F.Z` such that `A=F.left*F.S*F.right'` and `B=F.left*F.T*F.right'`. The
generalized eigenvalues of `A` and `B` can be obtained with `F.α./F.β`.

Iterating the decomposition produces the components `F.S`, `F.T`, `F.Q`, `F.Z`,
`F.α`, and `F.β`.
"""
schur(A::StridedMatrix{T},B::StridedMatrix{T}) where {T<:BlasFloat} = schur!(copy(A),copy(B))
function schur(A::StridedMatrix{TA}, B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA), TB)
    return schur!(copy_oftype(A, S), copy_oftype(B, S))
end

"""
    ordschur!(F::GeneralizedSchur, select::Union{Vector{Bool},BitVector}) -> F::GeneralizedSchur

Same as `ordschur` but overwrites the factorization `F`.
"""
function ordschur!(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector})
    _, _, α, β, _, _ = _ordschur!(gschur.S, gschur.T, gschur.Q, gschur.Z, select)
    gschur.α[:] = α
    gschur.β[:] = β
    return gschur
end

_ordschur(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
        _ordschur!(copy(S), copy(T), copy(Q), copy(Z), select)

_ordschur!(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector}) where {Ty<:BlasFloat} =
        LinearAlgebra.LAPACK.tgsen!(convert(Vector{BlasInt}, select), S, T, Q, Z)

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
    GeneralizedSchur(_ordschur(gschur.S, gschur.T, gschur.Q, gschur.Z, select)...)

function getproperty(F::GeneralizedSchur, d::Symbol)
    if d === :values
        return getfield(F, :α) ./ getfield(F, :β)
    elseif d === :alpha
        return getfield(F, :α)
    elseif d === :beta
        return getfield(F, :β)
    elseif d === :left
        return getfield(F, :Q)
    elseif d === :right
        return getfield(F, :Z)
    else
        getfield(F, d)
    end
end

Base.propertynames(F::GeneralizedSchur) =
    (:values, :left, :right, fieldnames(typeof(F))...)

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::GeneralizedSchur)
    summary(io, F); println(io)
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
