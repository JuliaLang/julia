# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractQ{T} end

struct AdjointQ{T,S<:AbstractQ{T}} <: AbstractQ{T}
    Q::S
end

parent(adjQ::AdjointQ) = adjQ.Q
eltype(::Type{<:AbstractQ{T}}) where {T} = T
ndims(::AbstractQ) = 2

# inversion/adjoint/transpose
inv(Q::AbstractQ) = Q'
adjoint(Q::AbstractQ) = AdjointQ(Q)
transpose(Q::AbstractQ{<:Real}) = AdjointQ(Q)
transpose(Q::AbstractQ) = error("transpose not implemented for $(typeof(Q)). Consider using adjoint instead of transpose.")
adjoint(adjQ::AdjointQ) = adjQ.Q

# promotion with AbstractMatrix, at least for equal eltypes
promote_rule(::Type{<:AbstractMatrix{T}}, ::Type{<:AbstractQ{T}}) where {T} =
    (@inline; Union{AbstractMatrix{T},AbstractQ{T}})

# conversion
AbstractQ{S}(Q::AbstractQ{S}) where {S} = Q
# the following eltype promotion needs to be defined for each subtype
# convert(::Type{AbstractQ{T}}, Q::QType) where {T} = QType{T}(Q)
convert(::Type{AbstractQ{T}}, Q::AbstractQ{T}) where {T} = Q
convert(::Type{AbstractQ{T}}, adjQ::AdjointQ{T}) where {T} = adjQ
convert(::Type{AbstractQ{T}}, adjQ::AdjointQ) where {T} = convert(AbstractQ{T}, adjQ.Q)'

# ... to matrix
Matrix{T}(Q::AbstractQ) where {T} = convert(Matrix{T}, Q*I) # generic fallback, yields square matrix
Matrix{T}(adjQ::AdjointQ{S}) where {T,S} = convert(Matrix{T}, lmul!(adjQ, Matrix{S}(I, size(adjQ))))
Matrix(Q::AbstractQ{T}) where {T} = Matrix{T}(Q)
Array{T}(Q::AbstractQ) where {T} = Matrix{T}(Q)
Array(Q::AbstractQ) = Matrix(Q)
convert(::Type{T}, Q::AbstractQ) where {T<:Array} = T(Q)
convert(::Type{T}, Q::AbstractQ) where {T<:Matrix} = T(Q)
# legacy
@deprecate(convert(::Type{AbstractMatrix{T}}, Q::AbstractQ) where {T},
    convert(LinearAlgebra.AbstractQ{T}, Q))

function size(Q::AbstractQ, dim::Integer)
    if dim < 1
        throw(BoundsError())
    elseif dim <= 2 # && 1 <= dim
        return size(Q)[dim]
    else # 2 < dim
        return 1
    end
end
size(adjQ::AdjointQ) = reverse(size(adjQ.Q))

# pseudo-array behaviour, required for indexing with `begin` or `end`
axes(Q::AbstractQ) = map(Base.oneto, size(Q))
axes(Q::AbstractQ, d::Integer) = d in (1, 2) ? axes(Q)[d] : Base.OneTo(1)

copymutable(Q::AbstractQ{T}) where {T} = lmul!(Q, Matrix{T}(I, size(Q)))
copy(Q::AbstractQ) = copymutable(Q)

# getindex
@inline function getindex(Q::AbstractQ, inds...)
    @boundscheck Base.checkbounds_indices(Bool, axes(Q), inds) || Base.throw_boundserror(Q, inds)
    return _getindex(Q, inds...)
end
@inline getindex(Q::AbstractQ, ::Colon) = copymutable(Q)[:]
@inline getindex(Q::AbstractQ, ::Colon, ::Colon) = copy(Q)

@inline _getindex(Q::AbstractQ, inds...) = @inbounds copymutable(Q)[inds...]
@inline function _getindex(Q::AbstractQ, ::Colon, J::AbstractVector{<:Integer})
    Y = zeros(eltype(Q), size(Q, 2), length(J))
    @inbounds for (i,j) in enumerate(J)
        Y[j,i] = oneunit(eltype(Q))
    end
    lmul!(Q, Y)
end
@inline _getindex(Q::AbstractQ, I::AbstractVector{Int}, J::AbstractVector{Int}) = @inbounds Q[:,J][I,:]
@inline function _getindex(Q::AbstractQ, ::Colon, j::Int)
    y = zeros(eltype(Q), size(Q, 2))
    y[j] = oneunit(eltype(Q))
    lmul!(Q, y)
end
@inline _getindex(Q::AbstractQ, i::Int, j::Int) = @inbounds Q[:,j][i]

# needed because AbstractQ does not subtype AbstractMatrix
qr(Q::AbstractQ{T}, arg...; kwargs...) where {T} = qr!(Matrix{_qreltype(T)}(Q), arg...; kwargs...)
lq(Q::AbstractQ{T}, arg...; kwargs...) where {T} = lq!(Matrix{lq_eltype(T)}(Q), arg...; kwargs...)
hessenberg(Q::AbstractQ{T}) where {T} = hessenberg!(Matrix{eigtype(T)}(Q))

# needed when used interchangeably with AbstractMatrix (analogous to views of ranges)
view(A::AbstractQ, I...) = getindex(A, I...)

# specialization avoiding the fallback using slow `getindex`
function copyto!(dest::AbstractMatrix, src::AbstractQ)
    copyto!(dest, I)
    lmul!(src, dest)
end
# needed to resolve method ambiguities
function copyto!(dest::PermutedDimsArray{T,2,perm}, src::AbstractQ) where {T,perm}
    if perm == (1, 2)
        copyto!(parent(dest), src)
    else
        @assert perm == (2, 1) # there are no other permutations of two indices
        if T <: Real
            copyto!(parent(dest), I)
            lmul!(src', parent(dest))
        else
            # LAPACK does not offer inplace lmul!(transpose(Q), B) for complex Q
            tmp = similar(parent(dest))
            copyto!(tmp, I)
            rmul!(tmp, src)
            permutedims!(parent(dest), tmp, (2, 1))
        end
    end
    return dest
end

function show(io::IO, ::MIME{Symbol("text/plain")}, Q::AbstractQ)
    print(io, Base.dims2string(size(Q)), ' ', summary(Q))
end

# multiplication
(*)(Q::AbstractQ, J::UniformScaling) = Q*J.λ
function (*)(Q::AbstractQ, b::Number)
    T = promote_type(eltype(Q), typeof(b))
    lmul!(convert(AbstractQ{T}, Q), Matrix{T}(b*I, size(Q)))
end
function (*)(A::AbstractQ, B::AbstractVecOrMat)
    T = promote_type(eltype(A), eltype(B))
    lmul!(convert(AbstractQ{T}, A), copy_similar(B, T))
end

(*)(J::UniformScaling, Q::AbstractQ) = J.λ*Q
function (*)(a::Number, Q::AbstractQ)
    T = promote_type(typeof(a), eltype(Q))
    rmul!(Matrix{T}(a*I, size(Q)), convert(AbstractQ{T}, Q))
end
*(a::AbstractVector, Q::AbstractQ) = reshape(a, length(a), 1) * Q
function (*)(A::AbstractMatrix, Q::AbstractQ)
    T = promote_type(eltype(A), eltype(Q))
    return rmul!(copy_similar(A, T), convert(AbstractQ{T}, Q))
end
(*)(u::AdjointAbsVec, Q::AbstractQ) = (Q'u')'

### Q*Q (including adjoints)
*(Q::AbstractQ, P::AbstractQ) = Q * (P*I)

### mul!
function mul!(C::AbstractVecOrMat{T}, Q::AbstractQ{T}, B::Union{AbstractVecOrMat{T},AbstractQ{T}}) where {T}
    require_one_based_indexing(C, B)
    mB = size(B, 1)
    mC = size(C, 1)
    if mB < mC
        inds = CartesianIndices(axes(B))
        copyto!(view(C, inds), B)
        C[CartesianIndices((mB+1:mC, axes(C, 2)))] .= zero(T)
        return lmul!(Q, C)
    else
        return lmul!(Q, copyto!(C, B))
    end
end
mul!(C::AbstractVecOrMat{T}, A::AbstractVecOrMat{T}, Q::AbstractQ{T}) where {T} = rmul!(copyto!(C, A), Q)
mul!(C::AbstractVecOrMat{T}, adjQ::AdjointQ{T}, B::AbstractVecOrMat{T}) where {T} = lmul!(adjQ, copyto!(C, B))
mul!(C::AbstractVecOrMat{T}, A::AbstractVecOrMat{T}, adjQ::AdjointQ{T}) where {T} = rmul!(copyto!(C, A), adjQ)

### division
\(Q::AbstractQ, A::AbstractVecOrMat) = Q'*A
/(A::AbstractVecOrMat, Q::AbstractQ) = A*Q'
ldiv!(Q::AbstractQ, A::AbstractVecOrMat) = lmul!(Q', A)
ldiv!(C::AbstractVecOrMat, Q::AbstractQ, A::AbstractVecOrMat) = mul!(C, Q', A)
rdiv!(A::AbstractVecOrMat, Q::AbstractQ) = rmul!(A, Q')

logabsdet(Q::AbstractQ) = (d = det(Q); return log(abs(d)), sign(d))
function logdet(A::AbstractQ)
    d, s = logabsdet(A)
    return d + log(s)
end

###########################################################
################ Q from QR decompositions #################
###########################################################

"""
    QRPackedQ <: LinearAlgebra.AbstractQ

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QR`](@ref) or
[`QRPivoted`](@ref) format.
"""
struct QRPackedQ{T,S<:AbstractMatrix{T},C<:AbstractVector{T}} <: AbstractQ{T}
    factors::S
    τ::C

    function QRPackedQ{T,S,C}(factors, τ) where {T,S<:AbstractMatrix{T},C<:AbstractVector{T}}
        require_one_based_indexing(factors, τ)
        new{T,S,C}(factors, τ)
    end
end
QRPackedQ(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T} =
    QRPackedQ{T,typeof(factors),typeof(τ)}(factors, τ)
QRPackedQ{T}(factors::AbstractMatrix, τ::AbstractVector) where {T} =
    QRPackedQ(convert(AbstractMatrix{T}, factors), convert(AbstractVector{T}, τ))
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(QRPackedQ{T,S}(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T,S},
           QRPackedQ{T,S,typeof(τ)}(factors, τ), false)

"""
    QRCompactWYQ <: LinearAlgebra.AbstractQ

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QRCompactWY`](@ref)
format.
"""
struct QRCompactWYQ{S, M<:AbstractMatrix{S}, C<:AbstractMatrix{S}} <: AbstractQ{S}
    factors::M
    T::C

    function QRCompactWYQ{S,M,C}(factors, T) where {S,M<:AbstractMatrix{S},C<:AbstractMatrix{S}}
        require_one_based_indexing(factors, T)
        new{S,M,C}(factors, T)
    end
end
QRCompactWYQ(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S} =
    QRCompactWYQ{S,typeof(factors),typeof(T)}(factors, T)
QRCompactWYQ{S}(factors::AbstractMatrix, T::AbstractMatrix) where {S} =
    QRCompactWYQ(convert(AbstractMatrix{S}, factors), convert(AbstractMatrix{S}, T))
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(QRCompactWYQ{S,M}(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S,M},
           QRCompactWYQ{S,M,typeof(T)}(factors, T), false)

QRPackedQ{T}(Q::QRPackedQ) where {T} = QRPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
QRCompactWYQ{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ(convert(AbstractMatrix{S}, Q.factors), convert(AbstractMatrix{S}, Q.T))

AbstractQ{S}(Q::QRPackedQ) where {S} = QRPackedQ{S}(Q)
AbstractQ{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ{S}(Q)
# override generic square fallback
Matrix{T}(Q::Union{QRCompactWYQ{S},QRPackedQ{S}}) where {T,S} =
    convert(Matrix{T}, lmul!(Q, Matrix{S}(I, size(Q, 1), min(size(Q.factors)...))))
Matrix(Q::Union{QRCompactWYQ{S},QRPackedQ{S}}) where {S} = Matrix{S}(Q)

convert(::Type{AbstractQ{T}}, Q::QRPackedQ) where {T} = QRPackedQ{T}(Q)
convert(::Type{AbstractQ{T}}, Q::QRCompactWYQ) where {T} = QRCompactWYQ{T}(Q)

size(Q::Union{QRCompactWYQ,QRPackedQ}, dim::Integer) =
    size(Q.factors, dim == 2 ? 1 : dim)
size(Q::Union{QRCompactWYQ,QRPackedQ}) = (n = size(Q.factors, 1); (n, n))

## Multiplication
### QB
lmul!(A::QRCompactWYQ{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.gemqrt!('L', 'N', A.factors, A.T, B)
lmul!(A::QRPackedQ{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormqr!('L', 'N', A.factors, A.τ, B)
function lmul!(A::QRPackedQ, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    if mA != mB
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but B has dimensions ($mB, $nB)"))
    end
    Afactors = A.factors
    @inbounds begin
        for k = min(mA,nA):-1:1
            for j = 1:nB
                vBj = B[k,j]
                for i = k+1:mB
                    vBj += conj(Afactors[i,k])*B[i,j]
                end
                vBj = A.τ[k]*vBj
                B[k,j] -= vBj
                for i = k+1:mB
                    B[i,j] -= Afactors[i,k]*vBj
                end
            end
        end
    end
    B
end

### QcB
lmul!(adjQ::AdjointQ{<:Any,<:QRCompactWYQ{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
    (Q = adjQ.Q; LAPACK.gemqrt!('L', 'T', Q.factors, Q.T, B))
lmul!(adjQ::AdjointQ{<:Any,<:QRCompactWYQ{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
    (Q = adjQ.Q; LAPACK.gemqrt!('L', 'C', Q.factors, Q.T, B))
lmul!(adjQ::AdjointQ{<:Any,<:QRPackedQ{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
    (Q = adjQ.Q; LAPACK.ormqr!('L', 'T', Q.factors, Q.τ, B))
lmul!(adjQ::AdjointQ{<:Any,<:QRPackedQ{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
    (Q = adjQ.Q; LAPACK.ormqr!('L', 'C', Q.factors, Q.τ, B))
function lmul!(adjA::AdjointQ{<:Any,<:QRPackedQ}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    A = adjA.Q
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    if mA != mB
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but B has dimensions ($mB, $nB)"))
    end
    Afactors = A.factors
    @inbounds begin
        for k = 1:min(mA,nA)
            for j = 1:nB
                vBj = B[k,j]
                for i = k+1:mB
                    vBj += conj(Afactors[i,k])*B[i,j]
                end
                vBj = conj(A.τ[k])*vBj
                B[k,j] -= vBj
                for i = k+1:mB
                    B[i,j] -= Afactors[i,k]*vBj
                end
            end
        end
    end
    B
end

### AQ
rmul!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T,<:StridedMatrix}) where {T<:BlasFloat} =
    LAPACK.gemqrt!('R', 'N', B.factors, B.T, A)
rmul!(A::StridedVecOrMat{T}, B::QRPackedQ{T,<:StridedMatrix}) where {T<:BlasFloat} =
    LAPACK.ormqr!('R', 'N', B.factors, B.τ, A)
function rmul!(A::AbstractMatrix, Q::QRPackedQ)
    require_one_based_indexing(A)
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    if nA != mQ
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but matrix Q has dimensions ($mQ, $nQ)"))
    end
    Qfactors = Q.factors
    @inbounds begin
        for k = 1:min(mQ,nQ)
            for i = 1:mA
                vAi = A[i,k]
                for j = k+1:mQ
                    vAi += A[i,j]*Qfactors[j,k]
                end
                vAi = vAi*Q.τ[k]
                A[i,k] -= vAi
                for j = k+1:nA
                    A[i,j] -= vAi*conj(Qfactors[j,k])
                end
            end
        end
    end
    A
end

### AQc
rmul!(A::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:QRCompactWYQ{T}}) where {T<:BlasReal} =
    (Q = adjQ.Q; LAPACK.gemqrt!('R', 'T', Q.factors, Q.T, A))
rmul!(A::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:QRCompactWYQ{T}}) where {T<:BlasComplex} =
    (Q = adjQ.Q; LAPACK.gemqrt!('R', 'C', Q.factors, Q.T, A))
rmul!(A::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:QRPackedQ{T}}) where {T<:BlasReal} =
    (Q = adjQ.Q; LAPACK.ormqr!('R', 'T', Q.factors, Q.τ, A))
rmul!(A::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:QRPackedQ{T}}) where {T<:BlasComplex} =
    (Q = adjQ.Q; LAPACK.ormqr!('R', 'C', Q.factors, Q.τ, A))
function rmul!(A::AbstractMatrix, adjQ::AdjointQ{<:Any,<:QRPackedQ})
    require_one_based_indexing(A)
    Q = adjQ.Q
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    if nA != mQ
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but matrix Q has dimensions ($mQ, $nQ)"))
    end
    Qfactors = Q.factors
    @inbounds begin
        for k = min(mQ,nQ):-1:1
            for i = 1:mA
                vAi = A[i,k]
                for j = k+1:mQ
                    vAi += A[i,j]*Qfactors[j,k]
                end
                vAi = vAi*conj(Q.τ[k])
                A[i,k] -= vAi
                for j = k+1:nA
                    A[i,j] -= vAi*conj(Qfactors[j,k])
                end
            end
        end
    end
    A
end

det(Q::QRPackedQ) = _det_tau(Q.τ)
det(Q::QRCompactWYQ) =
    prod(i -> _det_tau(_diagview(Q.T[:, i:min(i + size(Q.T, 1), size(Q.T, 2))])),
         1:size(Q.T, 1):size(Q.T, 2))

_diagview(A) = @view A[diagind(A)]

# Compute `det` from the number of Householder reflections.  Handle
# the case `Q.τ` contains zeros.
_det_tau(τs::AbstractVector{<:Real}) =
    isodd(count(!iszero, τs)) ? -one(eltype(τs)) : one(eltype(τs))

# In complex case, we need to compute the non-unit eigenvalue `λ = 1 - c*τ`
# (where `c = v'v`) of each Householder reflector.  As we know that the
# reflector must have the determinant of 1, it must satisfy `abs2(λ) == 1`.
# Combining this with the constraint `c > 0`, it turns out that the eigenvalue
# (hence the determinant) can be computed as `λ = -sign(τ)^2`.
# See: https://github.com/JuliaLang/julia/pull/32887#issuecomment-521935716
_det_tau(τs) = prod(τ -> iszero(τ) ? one(τ) : -sign(τ)^2, τs)

###########################################################
######## Q from Hessenberg decomposition ##################
###########################################################

"""
    HessenbergQ <: AbstractQ

Given a [`Hessenberg`](@ref) factorization object `F`, `F.Q` returns
a `HessenbergQ` object, which is an implicit representation of the unitary
matrix `Q` in the Hessenberg factorization `QHQ'` represented by `F`.
This `F.Q` object can be efficiently multiplied by matrices or vectors,
and can be converted to an ordinary matrix type with `Matrix(F.Q)`.
"""
struct HessenbergQ{T,S<:AbstractMatrix,W<:AbstractVector,sym} <: AbstractQ{T}
    uplo::Char
    factors::S
    τ::W
    function HessenbergQ{T,S,W,sym}(uplo::AbstractChar, factors, τ) where {T,S<:AbstractMatrix,W<:AbstractVector,sym}
        new(uplo, factors, τ)
    end
end
HessenbergQ(F::Hessenberg{<:Any,<:UpperHessenberg,S,W}) where {S,W} = HessenbergQ{eltype(F.factors),S,W,false}(F.uplo, F.factors, F.τ)
HessenbergQ(F::Hessenberg{<:Any,<:SymTridiagonal,S,W}) where {S,W} = HessenbergQ{eltype(F.factors),S,W,true}(F.uplo, F.factors, F.τ)

size(Q::HessenbergQ, dim::Integer) = size(getfield(Q, :factors), dim == 2 ? 1 : dim)
size(Q::HessenbergQ) = size(Q, 1), size(Q, 2)

# HessenbergQ from LAPACK/BLAS (as opposed to Julia libraries like GenericLinearAlgebra)
const BlasHessenbergQ{T,sym} = HessenbergQ{T,<:StridedMatrix{T},<:StridedVector{T},sym} where {T<:BlasFloat,sym}

## reconstruct the original matrix
Matrix{T}(Q::BlasHessenbergQ{<:Any,false}) where {T} = convert(Matrix{T}, LAPACK.orghr!(1, size(Q.factors, 1), copy(Q.factors), Q.τ))
Matrix{T}(Q::BlasHessenbergQ{<:Any,true}) where {T} = convert(Matrix{T}, LAPACK.orgtr!(Q.uplo, copy(Q.factors), Q.τ))

lmul!(Q::BlasHessenbergQ{T,false}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
rmul!(X::StridedVecOrMat{T}, Q::BlasHessenbergQ{T,false}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
lmul!(adjQ::AdjointQ{<:Any,<:BlasHessenbergQ{T,false}}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (Q = adjQ.Q; LAPACK.ormhr!('L', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))
rmul!(X::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:BlasHessenbergQ{T,false}}) where {T<:BlasFloat} =
    (Q = adjQ.Q; LAPACK.ormhr!('R', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))

lmul!(Q::BlasHessenbergQ{T,true}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormtr!('L', Q.uplo, 'N', Q.factors, Q.τ, X)
rmul!(X::StridedVecOrMat{T}, Q::BlasHessenbergQ{T,true}) where {T<:BlasFloat} =
    LAPACK.ormtr!('R', Q.uplo, 'N', Q.factors, Q.τ, X)
lmul!(adjQ::AdjointQ{<:Any,<:BlasHessenbergQ{T,true}}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (Q = adjQ.Q; LAPACK.ormtr!('L', Q.uplo, ifelse(T<:Real, 'T', 'C'), Q.factors, Q.τ, X))
rmul!(X::StridedVecOrMat{T}, adjQ::AdjointQ{<:Any,<:BlasHessenbergQ{T,true}}) where {T<:BlasFloat} =
    (Q = adjQ.Q; LAPACK.ormtr!('R', Q.uplo, ifelse(T<:Real, 'T', 'C'), Q.factors, Q.τ, X))

lmul!(Q::HessenbergQ{T}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T} = rmul!(X', Q')'
rmul!(X::Adjoint{T,<:StridedVecOrMat{T}}, Q::HessenbergQ{T}) where {T} = lmul!(Q', X')'
lmul!(adjQ::AdjointQ{<:Any,<:HessenbergQ{T}}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T}  = rmul!(X', adjQ')'
rmul!(X::Adjoint{T,<:StridedVecOrMat{T}}, adjQ::AdjointQ{<:Any,<:HessenbergQ{T}}) where {T} = lmul!(adjQ', X')'

# flexible left-multiplication (and adjoint right-multiplication)
function (*)(Q::Union{QRPackedQ,QRCompactWYQ,HessenbergQ}, b::AbstractVector)
    T = promote_type(eltype(Q), eltype(b))
    if size(Q.factors, 1) == length(b)
        bnew = copy_similar(b, T)
    elseif size(Q.factors, 2) == length(b)
        bnew = [b; zeros(T, size(Q.factors, 1) - length(b))]
    else
        throw(DimensionMismatch("vector must have length either $(size(Q.factors, 1)) or $(size(Q.factors, 2))"))
    end
    lmul!(convert(AbstractQ{T}, Q), bnew)
end
function (*)(Q::Union{QRPackedQ,QRCompactWYQ,HessenbergQ}, B::AbstractMatrix)
    T = promote_type(eltype(Q), eltype(B))
    if size(Q.factors, 1) == size(B, 1)
        Bnew = copy_similar(B, T)
    elseif size(Q.factors, 2) == size(B, 1)
        Bnew = [B; zeros(T, size(Q.factors, 1) - size(B,1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of matrix must have size either $(size(Q.factors, 1)) or $(size(Q.factors, 2))"))
    end
    lmul!(convert(AbstractQ{T}, Q), Bnew)
end
function (*)(A::AbstractMatrix, adjQ::AdjointQ{<:Any,<:Union{QRPackedQ,QRCompactWYQ,HessenbergQ}})
    Q = adjQ.Q
    T = promote_type(eltype(A), eltype(adjQ))
    adjQQ = convert(AbstractQ{T}, adjQ)
    if size(A, 2) == size(Q.factors, 1)
        AA = copy_similar(A, T)
        return rmul!(AA, adjQQ)
    elseif size(A, 2) == size(Q.factors, 2)
        return rmul!([A zeros(T, size(A, 1), size(Q.factors, 1) - size(Q.factors, 2))], adjQQ)
    else
        throw(DimensionMismatch("matrix A has dimensions $(size(A)) but Q-matrix B has dimensions $(size(adjQ))"))
    end
end
(*)(u::AdjointAbsVec, Q::AdjointQ{<:Any,<:Union{QRPackedQ,QRCompactWYQ,HessenbergQ}}) = (Q'u')'

det(Q::HessenbergQ) = _det_tau(Q.τ)

###########################################################
################ Q from LQ decomposition ##################
###########################################################

struct LQPackedQ{T,S<:AbstractMatrix{T},C<:AbstractVector{T}} <: AbstractQ{T}
    factors::S
    τ::C
end

LQPackedQ{T}(Q::LQPackedQ) where {T} = LQPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
@deprecate(AbstractMatrix{T}(Q::LQPackedQ) where {T},
    convert(AbstractQ{T}, Q),
    false)
Matrix{T}(A::LQPackedQ) where {T} = convert(Matrix{T}, LAPACK.orglq!(copy(A.factors), A.τ))
convert(::Type{AbstractQ{T}}, Q::LQPackedQ) where {T} = LQPackedQ{T}(Q)

# size(Q::LQPackedQ) yields the shape of Q's square form
size(Q::LQPackedQ) = (n = size(Q.factors, 2); return n, n)

## Multiplication
### QB / QcB
lmul!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} = LAPACK.ormlq!('L','N',A.factors,A.τ,B)
lmul!(adjA::AdjointQ{<:Any,<:LQPackedQ{T}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
    (A = adjA.Q; LAPACK.ormlq!('L', 'T', A.factors, A.τ, B))
lmul!(adjA::AdjointQ{<:Any,<:LQPackedQ{T}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
    (A = adjA.Q; LAPACK.ormlq!('L', 'C', A.factors, A.τ, B))

function (*)(adjA::AdjointQ{<:Any,<:LQPackedQ}, B::AbstractVector)
    A = adjA.Q
    T = promote_type(eltype(A), eltype(B))
    if length(B) == size(A.factors, 2)
        C = copy_similar(B, T)
    elseif length(B) == size(A.factors, 1)
        C = [B; zeros(T, size(A.factors, 2) - size(A.factors, 1), size(B, 2))]
    else
        throw(DimensionMismatch("length of B, $(length(B)), must equal one of the dimensions of A, $(size(A))"))
    end
    lmul!(convert(AbstractQ{T}, adjA), C)
end
function (*)(adjA::AdjointQ{<:Any,<:LQPackedQ}, B::AbstractMatrix)
    A = adjA.Q
    T = promote_type(eltype(A), eltype(B))
    if size(B,1) == size(A.factors,2)
        C = copy_similar(B, T)
    elseif size(B,1) == size(A.factors,1)
        C = [B; zeros(T, size(A.factors, 2) - size(A.factors, 1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of B, $(size(B,1)), must equal one of the dimensions of A, $(size(A))"))
    end
    lmul!(convert(AbstractQ{T}, adjA), C)
end

# in-place right-application of LQPackedQs
# these methods require that the applied-to matrix's (A's) number of columns
# match the number of columns (nQ) of the LQPackedQ (Q) (necessary for in-place
# operation, and the underlying LAPACK routine (ormlq) treats the implicit Q
# as its (nQ-by-nQ) square form)
rmul!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasFloat} =
    LAPACK.ormlq!('R', 'N', B.factors, B.τ, A)
rmul!(A::StridedMatrix{T}, adjB::AdjointQ{<:Any,<:LQPackedQ{T}}) where {T<:BlasReal} =
    (B = adjB.Q; LAPACK.ormlq!('R', 'T', B.factors, B.τ, A))
rmul!(A::StridedMatrix{T}, adjB::AdjointQ{<:Any,<:LQPackedQ{T}}) where {T<:BlasComplex} =
    (B = adjB.Q; LAPACK.ormlq!('R', 'C', B.factors, B.τ, A))

# out-of-place right application of LQPackedQs
#
# these methods: (1) check whether the applied-to matrix's (A's) appropriate dimension
# (columns for A_*, rows for Ac_*) matches the number of columns (nQ) of the LQPackedQ (Q),
# and if so effectively apply Q's square form to A without additional shenanigans; and
# (2) if the preceding dimensions do not match, check whether the appropriate dimension of
# A instead matches the number of rows of the matrix of which Q is a factor (i.e.
# size(Q.factors, 1)), and if so implicitly apply Q's truncated form to A by zero extending
# A as necessary for check (1) to pass (if possible) and then applying Q's square form
#
function (*)(A::AbstractVector, Q::LQPackedQ)
    T = promote_type(eltype(A), eltype(Q))
    if 1 == size(Q.factors, 2)
        C = copy_similar(A, T)
    elseif 1 == size(Q.factors, 1)
        C = zeros(T, length(A), size(Q.factors, 2))
        copyto!(C, 1, A, 1, length(A))
    else
        _rightappdimmismatch("columns")
    end
    return rmul!(C, convert(AbstractQ{T}, Q))
end
function (*)(A::AbstractMatrix, Q::LQPackedQ)
    T = promote_type(eltype(A), eltype(Q))
    if size(A, 2) == size(Q.factors, 2)
        C = copy_similar(A, T)
    elseif size(A, 2) == size(Q.factors, 1)
        C = zeros(T, size(A, 1), size(Q.factors, 2))
        copyto!(C, 1, A, 1, length(A))
    else
        _rightappdimmismatch("columns")
    end
    return rmul!(C, convert(AbstractQ{T}, Q))
end
function (*)(adjA::AdjointAbsMat, Q::LQPackedQ)
    A = adjA.parent
    T = promote_type(eltype(A), eltype(Q))
    if size(A, 1) == size(Q.factors, 2)
        C = copy_similar(adjA, T)
    elseif size(A, 1) == size(Q.factors, 1)
        C = zeros(T, size(A, 2), size(Q.factors, 2))
        adjoint!(view(C, :, 1:size(A, 1)), A)
    else
        _rightappdimmismatch("rows")
    end
    return rmul!(C, convert(AbstractQ{T}, Q))
end
(*)(u::AdjointAbsVec, Q::LQPackedQ) = (Q'u')'

_rightappdimmismatch(rowsorcols) =
    throw(DimensionMismatch(string("the number of $(rowsorcols) of the matrix on the left ",
        "must match either (1) the number of columns of the (LQPackedQ) matrix on the right ",
        "or (2) the number of rows of that (LQPackedQ) matrix's internal representation ",
        "(the factorization's originating matrix's number of rows)")))

# In LQ factorization, `Q` is expressed as the product of the adjoint of the
# reflectors.  Thus, `det` has to be conjugated.
det(Q::LQPackedQ) = conj(_det_tau(Q.τ))
