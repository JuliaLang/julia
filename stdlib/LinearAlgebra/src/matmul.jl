# This file is a part of Julia. License is MIT: https://julialang.org/license

# Matrix-matrix multiplication

AdjOrTransStridedMat{T} = Union{Adjoint{T, <:StridedMatrix}, Transpose{T, <:StridedMatrix}}
StridedMaybeAdjOrTransMat{T} = Union{StridedMatrix{T}, Adjoint{T, <:StridedMatrix}, Transpose{T, <:StridedMatrix}}

# matmul.jl: Everything to do with dense matrix multiplication

matprod(x, y) = x*y + x*y

# dot products

dot(x::StridedVecLike{T}, y::StridedVecLike{T}) where {T<:BlasReal} = BLAS.dot(x, y)
dot(x::StridedVecLike{T}, y::StridedVecLike{T}) where {T<:BlasComplex} = BLAS.dotc(x, y)

function dot(x::Vector{T}, rx::AbstractRange{TI}, y::Vector{T}, ry::AbstractRange{TI}) where {T<:BlasReal,TI<:Integer}
    if length(rx) != length(ry)
        throw(DimensionMismatch(lazy"length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    GC.@preserve x y BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

function dot(x::Vector{T}, rx::AbstractRange{TI}, y::Vector{T}, ry::AbstractRange{TI}) where {T<:BlasComplex,TI<:Integer}
    if length(rx) != length(ry)
        throw(DimensionMismatch(lazy"length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    GC.@preserve x y BLAS.dotc(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

function *(transx::Transpose{<:Any,<:StridedVector{T}}, y::StridedVector{T}) where {T<:BlasComplex}
    x = transx.parent
    return BLAS.dotu(x, y)
end

# Matrix-vector multiplication
function (*)(A::StridedMatrix{T}, x::StridedVector{S}) where {T<:BlasFloat,S<:Real}
    TS = promote_op(matprod, T, S)
    y = isconcretetype(TS) ? convert(AbstractVector{TS}, x) : x
    mul!(similar(x, TS, size(A,1)), A, y)
end
function (*)(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x,TS,axes(A,1)),A,x)
end

# these will throw a DimensionMismatch unless B has 1 row (or 1 col for transposed case):
(*)(a::AbstractVector, tB::Transpose{<:Any,<:AbstractMatrix}) = reshape(a, length(a), 1) * tB
(*)(a::AbstractVector, adjB::Adjoint{<:Any,<:AbstractMatrix}) = reshape(a, length(a), 1) * adjB
(*)(a::AbstractVector, B::AbstractMatrix) = reshape(a, length(a), 1) * B

@inline mul!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T},
             alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemv!(y, 'N', A, x, alpha, beta)

# Complex matrix times real vector.
# Reinterpret the matrix as a real matrix and do real matvec compuation.
@inline mul!(y::StridedVector{Complex{T}}, A::StridedVecOrMat{Complex{T}}, x::StridedVector{T},
        alpha::Number, beta::Number) where {T<:BlasReal} =
    gemv!(y, 'N', A, x, alpha, beta)

# Real matrix times complex vector.
# Multiply the matrix with the real and imaginary parts separately
@inline mul!(y::StridedVector{Complex{T}}, A::StridedMaybeAdjOrTransMat{T}, x::StridedVector{Complex{T}},
        alpha::Number, beta::Number) where {T<:BlasReal} =
    gemv!(y, A isa StridedArray ? 'N' : 'T', A isa StridedArray ? A : parent(A), x, alpha, beta)

@inline mul!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector,
             alpha::Number, beta::Number) =
    generic_matvecmul!(y, 'N', A, x, MulAddMul(alpha, beta))

function *(tA::Transpose{<:Any,<:StridedMatrix{T}}, x::StridedVector{S}) where {T<:BlasFloat,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x, TS, size(tA, 1)), tA, convert(AbstractVector{TS}, x))
end
function *(tA::Transpose{<:Any,<:AbstractMatrix{T}}, x::AbstractVector{S}) where {T,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x, TS, size(tA, 1)), tA, x)
end
@inline mul!(y::StridedVector{T}, tA::Transpose{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemv!(y, 'T', tA.parent, x, alpha, beta)
@inline mul!(y::AbstractVector, tA::Transpose{<:Any,<:AbstractVecOrMat}, x::AbstractVector,
                      alpha::Number, beta::Number) =
    generic_matvecmul!(y, 'T', tA.parent, x, MulAddMul(alpha, beta))

function *(adjA::Adjoint{<:Any,<:StridedMatrix{T}}, x::StridedVector{S}) where {T<:BlasFloat,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x, TS, size(adjA, 1)), adjA, convert(AbstractVector{TS}, x))
end
function *(adjA::Adjoint{<:Any,<:AbstractMatrix{T}}, x::AbstractVector{S}) where {T,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x, TS, size(adjA, 1)), adjA, x)
end

@inline mul!(y::StridedVector{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasReal} =
    mul!(y, transpose(adjA.parent), x, alpha, beta)
@inline mul!(y::StridedVector{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasComplex} =
    gemv!(y, 'C', adjA.parent, x, alpha, beta)
@inline mul!(y::AbstractVector, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, x::AbstractVector,
                      alpha::Number, beta::Number) =
    generic_matvecmul!(y, 'C', adjA.parent, x, MulAddMul(alpha, beta))

# Vector-Matrix multiplication
(*)(x::AdjointAbsVec,   A::AbstractMatrix) = (A'*x')'
(*)(x::TransposeAbsVec, A::AbstractMatrix) = transpose(transpose(A)*transpose(x))

_parent(A) = A
_parent(A::Adjoint) = parent(A)
_parent(A::Transpose) = parent(A)

"""
    *(A::AbstractMatrix, B::AbstractMatrix)

Matrix multiplication.

# Examples
```jldoctest
julia> [1 1; 0 1] * [1 0; 1 1]
2×2 Matrix{Int64}:
 2  1
 1  1
```
"""
function (*)(A::AbstractMatrix, B::AbstractMatrix)
    TS = promote_op(matprod, eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))), A, B)
end
# optimization for dispatching to BLAS, e.g. *(::Matrix{Float32}, ::Matrix{Float64})
# but avoiding the case *(::Matrix{<:BlasComplex}, ::Matrix{<:BlasReal})
# which is better handled by reinterpreting rather than promotion
function (*)(A::StridedMaybeAdjOrTransMat{<:BlasReal}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         wrapperop(A)(convert(AbstractArray{TS}, _parent(A))),
         wrapperop(B)(convert(AbstractArray{TS}, _parent(B))))
end
function (*)(A::StridedMaybeAdjOrTransMat{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasComplex})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         wrapperop(A)(convert(AbstractArray{TS}, _parent(A))),
         wrapperop(B)(convert(AbstractArray{TS}, _parent(B))))
end

@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                      alpha::Number, beta::Number) where {T<:BlasFloat}
    return gemm_wrapper!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
end
# Complex Matrix times real matrix: We use that it is generally faster to reinterpret the
# first matrix as a real matrix and carry out real matrix matrix multiply
function (*)(A::StridedMatrix{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         convert(AbstractArray{TS}, A),
         wrapperop(B)(convert(AbstractArray{real(TS)}, _parent(B))))
end
function (*)(A::AdjOrTransStridedMat{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         copymutable_oftype(A, TS), # remove AdjOrTrans to use reinterpret trick below
         wrapperop(B)(convert(AbstractArray{real(TS)}, _parent(B))))
end
# the following case doesn't seem to benefit from the translation A*B = (B' * A')'
function (*)(A::StridedMatrix{<:BlasReal}, B::StridedMatrix{<:BlasComplex})
    temp = real(B)
    R = A * temp
    temp .= imag.(B)
    I = A * temp
    Complex.(R, I)
end
(*)(A::AdjOrTransStridedMat{<:BlasReal}, B::StridedMatrix{<:BlasComplex}) = copy(transpose(transpose(B) * parent(A)))
(*)(A::StridedMaybeAdjOrTransMat{<:BlasReal}, B::AdjOrTransStridedMat{<:BlasComplex}) = copy(wrapperop(B)(parent(B) * transpose(A)))

"""
    muladd(A, y, z)

Combined multiply-add, `A*y .+ z`, for matrix-matrix or matrix-vector multiplication.
The result is always the same size as `A*y`, but `z` may be smaller, or a scalar.

!!! compat "Julia 1.6"
     These methods require Julia 1.6 or later.

# Examples
```jldoctest
julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; z=[0, 100];

julia> muladd(A, B, z)
2×2 Matrix{Float64}:
   3.0    3.0
 107.0  107.0
```
"""
function Base.muladd(A::AbstractMatrix, y::AbstractVecOrMat, z::Union{Number, AbstractArray})
    Ay = A * y
    for d in 1:ndims(Ay)
        # Same error as Ay .+= z would give, to match StridedMatrix method:
        size(z,d) > size(Ay,d) && throw(DimensionMismatch("array could not be broadcast to match destination"))
    end
    for d in ndims(Ay)+1:ndims(z)
        # Similar error to what Ay + z would give, to match (Any,Any,Any) method:
        size(z,d) > 1 && throw(DimensionMismatch(string("dimensions must match: z has dims ",
            axes(z), ", must have singleton at dim ", d)))
    end
    Ay .+ z
end

function Base.muladd(u::AbstractVector, v::AdjOrTransAbsVec, z::Union{Number, AbstractArray})
    if size(z,1) > length(u) || size(z,2) > length(v)
        # Same error as (u*v) .+= z:
        throw(DimensionMismatch("array could not be broadcast to match destination"))
    end
    for d in 3:ndims(z)
        # Similar error to (u*v) + z:
        size(z,d) > 1 && throw(DimensionMismatch(string("dimensions must match: z has dims ",
            axes(z), ", must have singleton at dim ", d)))
    end
    (u .* v) .+ z
end

Base.muladd(x::AdjointAbsVec, A::AbstractMatrix, z::Union{Number, AbstractVecOrMat}) =
    muladd(A', x', z')'
Base.muladd(x::TransposeAbsVec, A::AbstractMatrix, z::Union{Number, AbstractVecOrMat}) =
    transpose(muladd(transpose(A), transpose(x), transpose(z)))

function Base.muladd(A::StridedMaybeAdjOrTransMat{<:Number}, y::AbstractVector{<:Number}, z::Union{Number, AbstractVector})
    T = promote_type(eltype(A), eltype(y), eltype(z))
    C = similar(A, T, axes(A,1))
    C .= z
    mul!(C, A, y, true, true)
end

function Base.muladd(A::StridedMaybeAdjOrTransMat{<:Number}, B::StridedMaybeAdjOrTransMat{<:Number}, z::Union{Number, AbstractVecOrMat})
    T = promote_type(eltype(A), eltype(B), eltype(z))
    C = similar(A, T, axes(A,1), axes(B,2))
    C .= z
    mul!(C, A, B, true, true)
end

"""
    mul!(Y, A, B) -> Y

Calculates the matrix-matrix or matrix-vector product ``AB`` and stores the result in `Y`,
overwriting the existing value of `Y`. Note that `Y` must not be aliased with either `A` or
`B`.

# Examples
```jldoctest
julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; Y = similar(B); mul!(Y, A, B);

julia> Y
2×2 Matrix{Float64}:
 3.0  3.0
 7.0  7.0
```

# Implementation
For custom matrix and vector types, it is recommended to implement
5-argument `mul!` rather than implementing 3-argument `mul!` directly
if possible.
"""
@inline function mul!(C, A, B)
    return mul!(C, A, B, true, false)
end

"""
    mul!(C, A, B, α, β) -> C

Combined inplace matrix-matrix or matrix-vector multiply-add ``A B α + C β``.
The result is stored in `C` by overwriting it.  Note that `C` must not be
aliased with either `A` or `B`.

!!! compat "Julia 1.3"
    Five-argument `mul!` requires at least Julia 1.3.

# Examples
```jldoctest
julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; C=[1.0 2.0; 3.0 4.0];

julia> mul!(C, A, B, 100.0, 10.0) === C
true

julia> C
2×2 Matrix{Float64}:
 310.0  320.0
 730.0  740.0
```
"""
@inline mul!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat,
             alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))

"""
    rmul!(A, B)

Calculate the matrix-matrix product ``AB``, overwriting `A`, and return the result.
Here, `B` must be of special matrix type, like, e.g., [`Diagonal`](@ref),
[`UpperTriangular`](@ref) or [`LowerTriangular`](@ref), or of some orthogonal type,
see [`QR`](@ref).

# Examples
```jldoctest
julia> A = [0 1; 1 0];

julia> B = LinearAlgebra.UpperTriangular([1 2; 0 3]);

julia> LinearAlgebra.rmul!(A, B);

julia> A
2×2 Matrix{Int64}:
 0  3
 1  2

julia> A = [1.0 2.0; 3.0 4.0];

julia> F = qr([0 1; -1 0]);

julia> rmul!(A, F.Q)
2×2 Matrix{Float64}:
 2.0  1.0
 4.0  3.0
```
"""
rmul!(A, B)

"""
    lmul!(A, B)

Calculate the matrix-matrix product ``AB``, overwriting `B`, and return the result.
Here, `A` must be of special matrix type, like, e.g., [`Diagonal`](@ref),
[`UpperTriangular`](@ref) or [`LowerTriangular`](@ref), or of some orthogonal type,
see [`QR`](@ref).

# Examples
```jldoctest
julia> B = [0 1; 1 0];

julia> A = LinearAlgebra.UpperTriangular([1 2; 0 3]);

julia> LinearAlgebra.lmul!(A, B);

julia> B
2×2 Matrix{Int64}:
 2  1
 3  0

julia> B = [1.0 2.0; 3.0 4.0];

julia> F = qr([0 1; -1 0]);

julia> lmul!(F.Q, B)
2×2 Matrix{Float64}:
 3.0  4.0
 1.0  2.0
```
"""
lmul!(A, B)

@inline function mul!(C::StridedMatrix{T}, tA::Transpose{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    A = tA.parent
    if A === B
        return syrk_wrapper!(C, 'T', A, MulAddMul(alpha, beta))
    else
        return gemm_wrapper!(C, 'T', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline mul!(C::AbstractMatrix, tA::Transpose{<:Any,<:AbstractVecOrMat}, B::AbstractVecOrMat,
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'T', 'N', tA.parent, B, MulAddMul(alpha, beta))

@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, tB::Transpose{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    B = tB.parent
    if A === B
        return syrk_wrapper!(C, 'N', A, MulAddMul(alpha, beta))
    else
        return gemm_wrapper!(C, 'N', 'T', A, B, MulAddMul(alpha, beta))
    end
end
# Complex matrix times (transposed) real matrix. Reinterpret the first matrix to real for efficiency.
@inline mul!(C::StridedMatrix{Complex{T}}, A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
                    alpha::Number, beta::Number) where {T<:BlasReal} =
    gemm_wrapper!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
@inline mul!(C::StridedMatrix{Complex{T}}, A::StridedVecOrMat{Complex{T}}, tB::Transpose{<:Any,<:StridedVecOrMat{T}},
                    alpha::Number, beta::Number) where {T<:BlasReal} =
    gemm_wrapper!(C, 'N', 'T', A, parent(tB), MulAddMul(alpha, beta))

# collapsing the following two defs with C::AbstractVecOrMat yields ambiguities
@inline mul!(C::AbstractVector, A::AbstractVecOrMat, tB::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'T', A, tB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::AbstractVecOrMat, tB::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'T', A, tB.parent, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, tA::Transpose{<:Any,<:StridedVecOrMat{T}}, tB::Transpose{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemm_wrapper!(C, 'T', 'T', tA.parent, tB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, tA::Transpose{<:Any,<:AbstractVecOrMat}, tB::Transpose{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'T', 'T', tA.parent, tB.parent, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, tA::Transpose{<:Any,<:StridedVecOrMat{T}}, adjB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemm_wrapper!(C, 'T', 'C', tA.parent, adjB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, tA::Transpose{<:Any,<:AbstractVecOrMat}, tB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'T', 'C', tA.parent, tB.parent, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Real, beta::Real) where {T<:BlasReal} =
    mul!(C, transpose(adjA.parent), B, alpha, beta)
@inline function mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Number, beta::Number) where {T<:BlasComplex}
    A = adjA.parent
    if A === B
        return herk_wrapper!(C, 'C', A, MulAddMul(alpha, beta))
    else
        return gemm_wrapper!(C, 'C', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, B::AbstractVecOrMat,
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'C', 'N', adjA.parent, B, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:StridedVecOrMat{<:BlasReal}},
                 alpha::Number, beta::Number) where {T<:BlasFloat} =
    mul!(C, A, transpose(adjB.parent), alpha, beta)
@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasComplex}
    B = adjB.parent
    if A === B
        return herk_wrapper!(C, 'N', A, MulAddMul(alpha, beta))
    else
        return gemm_wrapper!(C, 'N', 'C', A, B, MulAddMul(alpha, beta))
    end
end
@inline mul!(C::AbstractMatrix, A::AbstractVecOrMat, adjB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'C', A, adjB.parent, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, adjB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemm_wrapper!(C, 'C', 'C', adjA.parent, adjB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, adjB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'C', 'C', adjA.parent, adjB.parent, MulAddMul(alpha, beta))

@inline mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, tB::Transpose{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemm_wrapper!(C, 'C', 'T', adjA.parent, tB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, tB::Transpose{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'C', 'T', adjA.parent, tB.parent, MulAddMul(alpha, beta))

# Supporting functions for matrix multiplication

# copy transposed(adjoint) of upper(lower) side-digonals. Optionally include diagonal.
@inline function copytri!(A::AbstractMatrix, uplo::AbstractChar, conjugate::Bool=false, diag::Bool=false)
    n = checksquare(A)
    off = diag ? 0 : 1
    if uplo == 'U'
        for i = 1:n, j = (i+off):n
            A[j,i] = conjugate ? adjoint(A[i,j]) : transpose(A[i,j])
        end
    elseif uplo == 'L'
        for i = 1:n, j = (i+off):n
            A[i,j] = conjugate ? adjoint(A[j,i]) : transpose(A[j,i])
        end
    else
        throw(ArgumentError(lazy"uplo argument must be 'U' (upper) or 'L' (lower), got $uplo"))
    end
    A
end

function gemv!(y::StridedVector{T}, tA::AbstractChar, A::StridedVecOrMat{T}, x::StridedVector{T},
               α::Number=true, β::Number=false) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
        stride(A, 1) == 1 && abs(stride(A, 2)) >= size(A, 1) &&
        !iszero(stride(x, 1)) # We only check input's stride here.
        return BLAS.gemv!(tA, alpha, A, x, beta, y)
    else
        return generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

function gemv!(y::StridedVector{Complex{T}}, tA::AbstractChar, A::StridedVecOrMat{Complex{T}}, x::StridedVector{T},
    α::Number = true, β::Number = false) where {T<:BlasReal}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
        stride(A, 1) == 1 && abs(stride(A, 2)) >= size(A, 1) &&
        stride(y, 1) == 1 && tA == 'N' && # reinterpret-based optimization is valid only for contiguous `y`
        !iszero(stride(x, 1))
        BLAS.gemv!(tA, alpha, reinterpret(T, A), x, beta, reinterpret(T, y))
        return y
    else
        return generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

function gemv!(y::StridedVector{Complex{T}}, tA::AbstractChar, A::StridedVecOrMat{T}, x::StridedVector{Complex{T}},
    α::Number = true, β::Number = false) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    @views if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
        stride(A, 1) == 1 && abs(stride(A, 2)) >= size(A, 1) &&
        !iszero(stride(x, 1))
        xfl = reinterpret(reshape, T, x) # Use reshape here.
        yfl = reinterpret(reshape, T, y)
        BLAS.gemv!(tA, alpha, A, xfl[1, :], beta, yfl[1, :])
        BLAS.gemv!(tA, alpha, A, xfl[2, :], beta, yfl[2, :])
        return y
    else
        return generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

function syrk_wrapper!(C::StridedMatrix{T}, tA::AbstractChar, A::StridedVecOrMat{T},
        _add = MulAddMul()) where {T<:BlasFloat}
    nC = checksquare(C)
    if tA == 'T'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'T'
    end
    if nC != mA
        throw(DimensionMismatch(lazy"output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0 || iszero(_add.alpha)
        return _rmul_or_fill!(C, _add.beta)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C, tA, tAt, A, A, _add)
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C, tA, tAt, A, A, _add)
    end

    # BLAS.syrk! only updates symmetric C
    # alternatively, make non-zero β a show-stopper for BLAS.syrk!
    if iszero(_add.beta) || issymmetric(C)
        alpha, beta = promote(_add.alpha, _add.beta, zero(T))
        if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(C, 1) == 1 &&
            stride(A, 2) >= size(A, 1) &&
            stride(C, 2) >= size(C, 1))
            return copytri!(BLAS.syrk!('U', tA, alpha, A, beta, C), 'U')
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, _add)
end

function herk_wrapper!(C::Union{StridedMatrix{T}, StridedMatrix{Complex{T}}}, tA::AbstractChar, A::Union{StridedVecOrMat{T}, StridedVecOrMat{Complex{T}}},
        _add = MulAddMul()) where {T<:BlasReal}
    nC = checksquare(C)
    if tA == 'C'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'C'
    end
    if nC != mA
        throw(DimensionMismatch(lazy"output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0 || iszero(_add.alpha)
        return _rmul_or_fill!(C, _add.beta)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C, tA, tAt, A, A, _add)
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C, tA, tAt, A, A, _add)
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Matrix{T}(undef, mA, mA)

    if iszero(_add.beta) || issymmetric(C)
        alpha, beta = promote(_add.alpha, _add.beta, zero(T))
        if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(C, 1) == 1 &&
            stride(A, 2) >= size(A, 1) &&
            stride(C, 2) >= size(C, 1))
            return copytri!(BLAS.herk!('U', tA, alpha, A, beta, C), 'U', true)
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, _add)
end

function gemm_wrapper(tA::AbstractChar, tB::AbstractChar,
                      A::StridedVecOrMat{T},
                      B::StridedVecOrMat{T}) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = similar(B, T, mA, nB)
    gemm_wrapper!(C, tA, tB, A, B)
end

function gemm_wrapper!(C::StridedVecOrMat{T}, tA::AbstractChar, tB::AbstractChar,
                       A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                       _add = MulAddMul()) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB
        throw(DimensionMismatch(lazy"A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    if mA == 0 || nA == 0 || nB == 0 || iszero(_add.alpha)
        if size(C) != (mA, nB)
            throw(DimensionMismatch(lazy"C has dimensions $(size(C)), should have ($mA,$nB)"))
        end
        return _rmul_or_fill!(C, _add.beta)
    end

    if mA == 2 && nA == 2 && nB == 2
        return matmul2x2!(C, tA, tB, A, B, _add)
    end
    if mA == 3 && nA == 3 && nB == 3
        return matmul3x3!(C, tA, tB, A, B, _add)
    end

    alpha, beta = promote(_add.alpha, _add.beta, zero(T))
    if (alpha isa Union{Bool,T} &&
        beta isa Union{Bool,T} &&
        stride(A, 1) == stride(B, 1) == stride(C, 1) == 1 &&
        stride(A, 2) >= size(A, 1) &&
        stride(B, 2) >= size(B, 1) &&
        stride(C, 2) >= size(C, 1))
        return BLAS.gemm!(tA, tB, alpha, A, B, beta, C)
    end
    generic_matmatmul!(C, tA, tB, A, B, _add)
end

function gemm_wrapper!(C::StridedVecOrMat{Complex{T}}, tA::AbstractChar, tB::AbstractChar,
                       A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
                       _add = MulAddMul()) where {T<:BlasReal}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB
        throw(DimensionMismatch(lazy"A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    if mA == 0 || nA == 0 || nB == 0 || iszero(_add.alpha)
        if size(C) != (mA, nB)
            throw(DimensionMismatch(lazy"C has dimensions $(size(C)), should have ($mA,$nB)"))
        end
        return _rmul_or_fill!(C, _add.beta)
    end

    if mA == 2 && nA == 2 && nB == 2
        return matmul2x2!(C, tA, tB, A, B, _add)
    end
    if mA == 3 && nA == 3 && nB == 3
        return matmul3x3!(C, tA, tB, A, B, _add)
    end

    alpha, beta = promote(_add.alpha, _add.beta, zero(T))

    # Make-sure reinterpret-based optimization is BLAS-compatible.
    if (alpha isa Union{Bool,T} &&
        beta isa Union{Bool,T} &&
        stride(A, 1) == stride(B, 1) == stride(C, 1) == 1 &&
        stride(A, 2) >= size(A, 1) &&
        stride(B, 2) >= size(B, 1) &&
        stride(C, 2) >= size(C, 1)) && tA == 'N'
        BLAS.gemm!(tA, tB, alpha, reinterpret(T, A), B, beta, reinterpret(T, C))
        return C
    end
    generic_matmatmul!(C, tA, tB, A, B, _add)
end

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::AbstractChar, M::AbstractVecOrMat) = (size(M, t=='N' ? 1 : 2), size(M, t=='N' ? 2 : 1))

function copyto!(B::AbstractVecOrMat, ir_dest::AbstractUnitRange{Int}, jr_dest::AbstractUnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::AbstractUnitRange{Int}, jr_src::AbstractUnitRange{Int})
    if tM == 'N'
        copyto!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        LinearAlgebra.copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(@view B[ir_dest, jr_dest])
    end
    B
end

function copy_transpose!(B::AbstractMatrix, ir_dest::AbstractUnitRange{Int}, jr_dest::AbstractUnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::AbstractUnitRange{Int}, jr_src::AbstractUnitRange{Int})
    if tM == 'N'
        LinearAlgebra.copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copyto!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(@view B[ir_dest, jr_dest])
    end
    B
end

# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the generic version is also called as fallback for
#       strides != 1 cases

function generic_matvecmul!(C::AbstractVector{R}, tA, A::AbstractVecOrMat, B::AbstractVector,
                            _add::MulAddMul = MulAddMul()) where R
    require_one_based_indexing(C, A, B)
    mB = length(B)
    mA, nA = lapack_size(tA, A)
    if mB != nA
        throw(DimensionMismatch(lazy"matrix A has dimensions ($mA,$nA), vector B has length $mB"))
    end
    if mA != length(C)
        throw(DimensionMismatch(lazy"result C has length $(length(C)), needs length $mA"))
    end

    Astride = size(A, 1)

    @inbounds begin
    if tA == 'T'  # fastest case
        if nA == 0
            for k = 1:mA
                _modify!(_add, false, C, k)
            end
        else
            for k = 1:mA
                aoffs = (k-1)*Astride
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
                for i = 1:nA
                    s += transpose(A[aoffs+i]) * B[i]
                end
                _modify!(_add, s, C, k)
            end
        end
    elseif tA == 'C'
        if nA == 0
            for k = 1:mA
                _modify!(_add, false, C, k)
            end
        else
            for k = 1:mA
                aoffs = (k-1)*Astride
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
                for i = 1:nA
                    s += A[aoffs + i]'B[i]
                end
                _modify!(_add, s, C, k)
            end
        end
    else # tA == 'N'
        for i = 1:mA
            if !iszero(_add.beta)
                C[i] *= _add.beta
            elseif mB == 0
                C[i] = false
            else
                C[i] = zero(A[i]*B[1] + A[i]*B[1])
            end
        end
        for k = 1:mB
            aoffs = (k-1)*Astride
            b = _add(B[k], false)
            for i = 1:mA
                C[i] += A[aoffs + i] * b
            end
        end
    end
    end # @inbounds
    C
end

function generic_matmatmul(tA, tB, A::AbstractVecOrMat{T}, B::AbstractMatrix{S}) where {T,S}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = similar(B, promote_op(matprod, T, S), mA, nB)
    generic_matmatmul!(C, tA, tB, A, B)
end

const tilebufsize = 10800  # Approximately 32k/3

function generic_matmatmul!(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix,
                            _add::MulAddMul=MulAddMul())
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    mC, nC = size(C)

    if iszero(_add.alpha)
        return _rmul_or_fill!(C, _add.beta)
    end
    if mA == nA == mB == nB == mC == nC == 2
        return matmul2x2!(C, tA, tB, A, B, _add)
    end
    if mA == nA == mB == nB == mC == nC == 3
        return matmul3x3!(C, tA, tB, A, B, _add)
    end
    _generic_matmatmul!(C, tA, tB, A, B, _add)
end

generic_matmatmul!(C::AbstractVecOrMat, tA, tB, A::AbstractVecOrMat, B::AbstractVecOrMat, _add::MulAddMul) =
    _generic_matmatmul!(C, tA, tB, A, B, _add)

function _generic_matmatmul!(C::AbstractVecOrMat{R}, tA, tB, A::AbstractVecOrMat{T}, B::AbstractVecOrMat{S},
                             _add::MulAddMul) where {T,S,R}
    require_one_based_indexing(C, A, B)
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if mB != nA
        throw(DimensionMismatch(lazy"matrix A has dimensions ($mA,$nA), matrix B has dimensions ($mB,$nB)"))
    end
    if size(C,1) != mA || size(C,2) != nB
        throw(DimensionMismatch(lazy"result C has dimensions $(size(C)), needs ($mA,$nB)"))
    end

    if iszero(_add.alpha) || isempty(A) || isempty(B)
        return _rmul_or_fill!(C, _add.beta)
    end

    tile_size = 0
    if isbitstype(R) && isbitstype(T) && isbitstype(S) && (tA == 'N' || tB != 'N')
        tile_size = floor(Int, sqrt(tilebufsize / max(sizeof(R), sizeof(S), sizeof(T), 1)))
    end
    @inbounds begin
    if tile_size > 0
        sz = (tile_size, tile_size)
        Atile = Array{T}(undef, sz)
        Btile = Array{S}(undef, sz)

        z1 = zero(A[1, 1]*B[1, 1] + A[1, 1]*B[1, 1])
        z = convert(promote_type(typeof(z1), R), z1)

        if mA < tile_size && nA < tile_size && nB < tile_size
            copy_transpose!(Atile, 1:nA, 1:mA, tA, A, 1:mA, 1:nA)
            copyto!(Btile, 1:mB, 1:nB, tB, B, 1:mB, 1:nB)
            for j = 1:nB
                boff = (j-1)*tile_size
                for i = 1:mA
                    aoff = (i-1)*tile_size
                    s = z
                    for k = 1:nA
                        s += Atile[aoff+k] * Btile[boff+k]
                    end
                    _modify!(_add, s, C, (i,j))
                end
            end
        else
            Ctile = Array{R}(undef, sz)
            for jb = 1:tile_size:nB
                jlim = min(jb+tile_size-1,nB)
                jlen = jlim-jb+1
                for ib = 1:tile_size:mA
                    ilim = min(ib+tile_size-1,mA)
                    ilen = ilim-ib+1
                    fill!(Ctile, z)
                    for kb = 1:tile_size:nA
                        klim = min(kb+tile_size-1,mB)
                        klen = klim-kb+1
                        copy_transpose!(Atile, 1:klen, 1:ilen, tA, A, ib:ilim, kb:klim)
                        copyto!(Btile, 1:klen, 1:jlen, tB, B, kb:klim, jb:jlim)
                        for j=1:jlen
                            bcoff = (j-1)*tile_size
                            for i = 1:ilen
                                aoff = (i-1)*tile_size
                                s = z
                                for k = 1:klen
                                    s += Atile[aoff+k] * Btile[bcoff+k]
                                end
                                Ctile[bcoff+i] += s
                            end
                        end
                    end
                    if isone(_add.alpha) && iszero(_add.beta)
                        copyto!(C, ib:ilim, jb:jlim, Ctile, 1:ilen, 1:jlen)
                    else
                        C[ib:ilim, jb:jlim] .= @views _add.(Ctile[1:ilen, 1:jlen], C[ib:ilim, jb:jlim])
                    end
                end
            end
        end
    else
        # Multiplication for non-plain-data uses the naive algorithm
        if tA == 'N'
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[i, 1]*B[1, j] + A[i, 1]*B[1, j])
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += A[i, k]*B[k, j]
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[i, 1]*transpose(B[j, 1]) + A[i, 1]*transpose(B[j, 1]))
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += A[i, k] * transpose(B[j, k])
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            else
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[i, 1]*B[j, 1]' + A[i, 1]*B[j, 1]')
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += A[i, k]*B[j, k]'
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            end
        elseif tA == 'T'
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    z2 = zero(transpose(A[1, i])*B[1, j] + transpose(A[1, i])*B[1, j])
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += transpose(A[k, i]) * B[k, j]
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    z2 = zero(transpose(A[1, i])*transpose(B[j, 1]) + transpose(A[1, i])*transpose(B[j, 1]))
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += transpose(A[k, i]) * transpose(B[j, k])
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            else
                for i = 1:mA, j = 1:nB
                    z2 = zero(transpose(A[1, i])*B[j, 1]' + transpose(A[1, i])*B[j, 1]')
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += transpose(A[k, i]) * adjoint(B[j, k])
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            end
        else
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[1, i]'*B[1, j] + A[1, i]'*B[1, j])
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += A[k, i]'B[k, j]
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[1, i]'*transpose(B[j, 1]) + A[1, i]'*transpose(B[j, 1]))
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += adjoint(A[k, i]) * transpose(B[j, k])
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            else
                for i = 1:mA, j = 1:nB
                    z2 = zero(A[1, i]'*B[j, 1]' + A[1, i]'*B[j, 1]')
                    Ctmp = convert(promote_type(R, typeof(z2)), z2)
                    for k = 1:nA
                        Ctmp += A[k, i]'B[j, k]'
                    end
                    _modify!(_add, Ctmp, C, (i,j))
                end
            end
        end
    end
    end # @inbounds
    C
end


# multiply 2x2 matrices
function matmul2x2(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}
    matmul2x2!(similar(B, promote_op(matprod, T, S), 2, 2), tA, tB, A, B)
end

function matmul2x2!(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix,
                    _add::MulAddMul = MulAddMul())
    require_one_based_indexing(C, A, B)
    if !(size(A) == size(B) == size(C) == (2,2))
        throw(DimensionMismatch(lazy"A has size $(size(A)), B has size $(size(B)), C has size $(size(C))"))
    end
    @inbounds begin
    if tA == 'T'
        # TODO making these lazy could improve perf
        A11 = copy(transpose(A[1,1])); A12 = copy(transpose(A[2,1]))
        A21 = copy(transpose(A[1,2])); A22 = copy(transpose(A[2,2]))
    elseif tA == 'C'
        # TODO making these lazy could improve perf
        A11 = copy(A[1,1]'); A12 = copy(A[2,1]')
        A21 = copy(A[1,2]'); A22 = copy(A[2,2]')
    else
        A11 = A[1,1]; A12 = A[1,2]; A21 = A[2,1]; A22 = A[2,2]
    end
    if tB == 'T'
        # TODO making these lazy could improve perf
        B11 = copy(transpose(B[1,1])); B12 = copy(transpose(B[2,1]))
        B21 = copy(transpose(B[1,2])); B22 = copy(transpose(B[2,2]))
    elseif tB == 'C'
        # TODO making these lazy could improve perf
        B11 = copy(B[1,1]'); B12 = copy(B[2,1]')
        B21 = copy(B[1,2]'); B22 = copy(B[2,2]')
    else
        B11 = B[1,1]; B12 = B[1,2];
        B21 = B[2,1]; B22 = B[2,2]
    end
    _modify!(_add, A11*B11 + A12*B21, C, (1,1))
    _modify!(_add, A11*B12 + A12*B22, C, (1,2))
    _modify!(_add, A21*B11 + A22*B21, C, (2,1))
    _modify!(_add, A21*B12 + A22*B22, C, (2,2))
    end # inbounds
    C
end

# Multiply 3x3 matrices
function matmul3x3(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}
    matmul3x3!(similar(B, promote_op(matprod, T, S), 3, 3), tA, tB, A, B)
end

function matmul3x3!(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix,
                    _add::MulAddMul = MulAddMul())
    require_one_based_indexing(C, A, B)
    if !(size(A) == size(B) == size(C) == (3,3))
        throw(DimensionMismatch(lazy"A has size $(size(A)), B has size $(size(B)), C has size $(size(C))"))
    end
    @inbounds begin
    if tA == 'T'
        # TODO making these lazy could improve perf
        A11 = copy(transpose(A[1,1])); A12 = copy(transpose(A[2,1])); A13 = copy(transpose(A[3,1]))
        A21 = copy(transpose(A[1,2])); A22 = copy(transpose(A[2,2])); A23 = copy(transpose(A[3,2]))
        A31 = copy(transpose(A[1,3])); A32 = copy(transpose(A[2,3])); A33 = copy(transpose(A[3,3]))
    elseif tA == 'C'
        # TODO making these lazy could improve perf
        A11 = copy(A[1,1]'); A12 = copy(A[2,1]'); A13 = copy(A[3,1]')
        A21 = copy(A[1,2]'); A22 = copy(A[2,2]'); A23 = copy(A[3,2]')
        A31 = copy(A[1,3]'); A32 = copy(A[2,3]'); A33 = copy(A[3,3]')
    else
        A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3]
        A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3]
        A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3]
    end

    if tB == 'T'
        # TODO making these lazy could improve perf
        B11 = copy(transpose(B[1,1])); B12 = copy(transpose(B[2,1])); B13 = copy(transpose(B[3,1]))
        B21 = copy(transpose(B[1,2])); B22 = copy(transpose(B[2,2])); B23 = copy(transpose(B[3,2]))
        B31 = copy(transpose(B[1,3])); B32 = copy(transpose(B[2,3])); B33 = copy(transpose(B[3,3]))
    elseif tB == 'C'
        # TODO making these lazy could improve perf
        B11 = copy(B[1,1]'); B12 = copy(B[2,1]'); B13 = copy(B[3,1]')
        B21 = copy(B[1,2]'); B22 = copy(B[2,2]'); B23 = copy(B[3,2]')
        B31 = copy(B[1,3]'); B32 = copy(B[2,3]'); B33 = copy(B[3,3]')
    else
        B11 = B[1,1]; B12 = B[1,2]; B13 = B[1,3]
        B21 = B[2,1]; B22 = B[2,2]; B23 = B[2,3]
        B31 = B[3,1]; B32 = B[3,2]; B33 = B[3,3]
    end

    _modify!(_add, A11*B11 + A12*B21 + A13*B31, C, (1,1))
    _modify!(_add, A11*B12 + A12*B22 + A13*B32, C, (1,2))
    _modify!(_add, A11*B13 + A12*B23 + A13*B33, C, (1,3))

    _modify!(_add, A21*B11 + A22*B21 + A23*B31, C, (2,1))
    _modify!(_add, A21*B12 + A22*B22 + A23*B32, C, (2,2))
    _modify!(_add, A21*B13 + A22*B23 + A23*B33, C, (2,3))

    _modify!(_add, A31*B11 + A32*B21 + A33*B31, C, (3,1))
    _modify!(_add, A31*B12 + A32*B22 + A33*B32, C, (3,2))
    _modify!(_add, A31*B13 + A32*B23 + A33*B33, C, (3,3))
    end # inbounds
    C
end

const RealOrComplex = Union{Real,Complex}

# Three-argument *
"""
    *(A, B::AbstractMatrix, C)
    A * B * C * D

Chained multiplication of 3 or 4 matrices is done in the most efficient sequence,
based on the sizes of the arrays. That is, the number of scalar multiplications needed
for `(A * B) * C` (with 3 dense matrices) is compared to that for `A * (B * C)`
to choose which of these to execute.

If the last factor is a vector, or the first a transposed vector, then it is efficient
to deal with these first. In particular `x' * B * y` means `(x' * B) * y`
for an ordinary column-major `B::Matrix`. Unlike `dot(x, B, y)`, this
allocates an intermediate array.

If the first or last factor is a number, this will be fused with the matrix
multiplication, using 5-arg [`mul!`](@ref).

See also [`muladd`](@ref), [`dot`](@ref).

!!! compat "Julia 1.7"
    These optimisations require at least Julia 1.7.
"""
*(A::AbstractMatrix, B::AbstractMatrix, x::AbstractVector) = A * (B*x)

*(tu::AdjOrTransAbsVec, B::AbstractMatrix, v::AbstractVector) = (tu*B) * v
*(tu::AdjOrTransAbsVec, B::AdjOrTransAbsMat, v::AbstractVector) = tu * (B*v)

*(A::AbstractMatrix, x::AbstractVector, γ::Number) = mat_vec_scalar(A,x,γ)
*(A::AbstractMatrix, B::AbstractMatrix, γ::Number) = mat_mat_scalar(A,B,γ)
*(α::RealOrComplex, B::AbstractMatrix{<:RealOrComplex}, C::AbstractVector{<:RealOrComplex}) =
    mat_vec_scalar(B,C,α)
*(α::RealOrComplex, B::AbstractMatrix{<:RealOrComplex}, C::AbstractMatrix{<:RealOrComplex}) =
    mat_mat_scalar(B,C,α)

*(α::Number, u::AbstractVector, tv::AdjOrTransAbsVec) = broadcast(*, α, u, tv)
*(u::AbstractVector, tv::AdjOrTransAbsVec, γ::Number) = broadcast(*, u, tv, γ)
*(u::AbstractVector, tv::AdjOrTransAbsVec, C::AbstractMatrix) = u * (tv*C)

*(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix) = _tri_matmul(A,B,C)
*(tv::AdjOrTransAbsVec, B::AbstractMatrix, C::AbstractMatrix) = (tv*B) * C

function _tri_matmul(A,B,C,δ=nothing)
    n,m = size(A)
    # m,k == size(B)
    k,l = size(C)
    costAB_C = n*m*k + n*k*l  # multiplications, allocations n*k + n*l
    costA_BC = m*k*l + n*m*l  #                              m*l + n*l
    if costA_BC < costAB_C
        isnothing(δ) ? A * (B*C) : A * mat_mat_scalar(B,C,δ)
    else
        isnothing(δ) ? (A*B) * C : mat_mat_scalar(A*B, C, δ)
    end
end

# Fast path for two arrays * one scalar is opt-in, via mat_vec_scalar and mat_mat_scalar.

mat_vec_scalar(A, x, γ) = A * (x * γ)  # fallback
mat_vec_scalar(A::StridedMaybeAdjOrTransMat, x::StridedVector, γ) = _mat_vec_scalar(A, x, γ)
mat_vec_scalar(A::AdjOrTransAbsVec, x::StridedVector, γ) = (A * x) * γ

function _mat_vec_scalar(A, x, γ)
    T = promote_type(eltype(A), eltype(x), typeof(γ))
    C = similar(A, T, axes(A,1))
    mul!(C, A, x, γ, false)
end

mat_mat_scalar(A, B, γ) = (A*B) * γ # fallback
mat_mat_scalar(A::StridedMaybeAdjOrTransMat, B::StridedMaybeAdjOrTransMat, γ) =
    _mat_mat_scalar(A, B, γ)

function _mat_mat_scalar(A, B, γ)
    T = promote_type(eltype(A), eltype(B), typeof(γ))
    C = similar(A, T, axes(A,1), axes(B,2))
    mul!(C, A, B, γ, false)
end

mat_mat_scalar(A::AdjointAbsVec, B, γ) = (γ' * (A * B)')' # preserving order, adjoint reverses
mat_mat_scalar(A::AdjointAbsVec{<:RealOrComplex}, B::StridedMaybeAdjOrTransMat{<:RealOrComplex}, γ::RealOrComplex) =
    mat_vec_scalar(B', A', γ')'

mat_mat_scalar(A::TransposeAbsVec, B, γ) = transpose(γ * transpose(A * B))
mat_mat_scalar(A::TransposeAbsVec{<:RealOrComplex}, B::StridedMaybeAdjOrTransMat{<:RealOrComplex}, γ::RealOrComplex) =
    transpose(mat_vec_scalar(transpose(B), transpose(A), γ))


# Four-argument *, by type
*(α::Number, β::Number, C::AbstractMatrix, x::AbstractVector) = (α*β) * C * x
*(α::Number, β::Number, C::AbstractMatrix, D::AbstractMatrix) = (α*β) * C * D
*(α::Number, B::AbstractMatrix, C::AbstractMatrix, x::AbstractVector) = α * B * (C*x)
*(α::Number, vt::AdjOrTransAbsVec, C::AbstractMatrix, x::AbstractVector) = α * (vt*C*x)
*(α::RealOrComplex, vt::AdjOrTransAbsVec{<:RealOrComplex}, C::AbstractMatrix{<:RealOrComplex}, D::AbstractMatrix{<:RealOrComplex}) =
    (α*vt*C) * D # solves an ambiguity

*(A::AbstractMatrix, x::AbstractVector, γ::Number, δ::Number) = A * x * (γ*δ)
*(A::AbstractMatrix, B::AbstractMatrix, γ::Number, δ::Number) = A * B * (γ*δ)
*(A::AbstractMatrix, B::AbstractMatrix, x::AbstractVector, δ::Number, ) = A * (B*x*δ)
*(vt::AdjOrTransAbsVec, B::AbstractMatrix, x::AbstractVector, δ::Number) = (vt*B*x) * δ
*(vt::AdjOrTransAbsVec, B::AbstractMatrix, C::AbstractMatrix, δ::Number) = (vt*B) * C * δ

*(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix, x::AbstractVector) = A * B * (C*x)
*(vt::AdjOrTransAbsVec, B::AbstractMatrix, C::AbstractMatrix, D::AbstractMatrix) = (vt*B) * C * D
*(vt::AdjOrTransAbsVec, B::AbstractMatrix, C::AbstractMatrix, x::AbstractVector) = vt * B * (C*x)

# Four-argument *, by size
*(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix, δ::Number) = _tri_matmul(A,B,C,δ)
*(α::RealOrComplex, B::AbstractMatrix{<:RealOrComplex}, C::AbstractMatrix{<:RealOrComplex}, D::AbstractMatrix{<:RealOrComplex}) =
    _tri_matmul(B,C,D,α)
*(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix, D::AbstractMatrix) =
    _quad_matmul(A,B,C,D)

function _quad_matmul(A,B,C,D)
    c1 = _mul_cost((A,B),(C,D))
    c2 = _mul_cost(((A,B),C),D)
    c3 = _mul_cost(A,(B,(C,D)))
    c4 = _mul_cost((A,(B,C)),D)
    c5 = _mul_cost(A,((B,C),D))
    cmin = min(c1,c2,c3,c4,c5)
    if c1 == cmin
        (A*B) * (C*D)
    elseif c2 == cmin
        ((A*B) * C) * D
    elseif c3 == cmin
        A * (B * (C*D))
    elseif c4 == cmin
        (A * (B*C)) * D
    else
        A * ((B*C) * D)
    end
end
@inline _mul_cost(A::AbstractMatrix) = 0
@inline _mul_cost((A,B)::Tuple) = _mul_cost(A,B)
@inline _mul_cost(A,B) = _mul_cost(A) + _mul_cost(B) + *(_mul_sizes(A)..., last(_mul_sizes(B)))
@inline _mul_sizes(A::AbstractMatrix) = size(A)
@inline _mul_sizes((A,B)::Tuple) = first(_mul_sizes(A)), last(_mul_sizes(B))
