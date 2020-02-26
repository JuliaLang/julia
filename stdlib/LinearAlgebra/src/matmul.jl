# This file is a part of Julia. License is MIT: https://julialang.org/license

# matmul.jl: Everything to do with dense matrix multiplication

matprod(x, y) = x*y + x*y

# dot products

dot(x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}}) where {T<:BlasReal} = BLAS.dot(x, y)
dot(x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}}) where {T<:BlasComplex} = BLAS.dotc(x, y)

function dot(x::Vector{T}, rx::Union{UnitRange{TI},AbstractRange{TI}}, y::Vector{T}, ry::Union{UnitRange{TI},AbstractRange{TI}}) where {T<:BlasReal,TI<:Integer}
    if length(rx) != length(ry)
        throw(DimensionMismatch("length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    GC.@preserve x y BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

function dot(x::Vector{T}, rx::Union{UnitRange{TI},AbstractRange{TI}}, y::Vector{T}, ry::Union{UnitRange{TI},AbstractRange{TI}}) where {T<:BlasComplex,TI<:Integer}
    if length(rx) != length(ry)
        throw(DimensionMismatch("length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
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
function *(a::AbstractVector, transB::Transpose{<:Any,<:AbstractMatrix})
    B = transB.parent
    reshape(a,length(a),1)*transpose(B)
end
function *(a::AbstractVector, adjB::Adjoint{<:Any,<:AbstractMatrix})
    B = adjB.parent
    reshape(a,length(a),1)*adjoint(B)
end
(*)(a::AbstractVector, B::AbstractMatrix) = reshape(a,length(a),1)*B

@inline mul!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T},
             alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemv!(y, 'N', A, x, alpha, beta)
# Complex matrix times real vector. Reinterpret the matrix as a real matrix and do real matvec compuation.
for elty in (Float32,Float64)
    @eval begin
        @inline function mul!(y::StridedVector{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, x::StridedVector{$elty},
                              alpha::Real, beta::Real)
            Afl = reinterpret($elty, A)
            yfl = reinterpret($elty, y)
            mul!(yfl, Afl, x, alpha, beta)
            return y
        end
    end
end
@inline mul!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector,
             alpha::Number, beta::Number) =
    generic_matvecmul!(y, 'N', A, x, MulAddMul(alpha, beta))

function *(transA::Transpose{<:Any,<:StridedMatrix{T}}, x::StridedVector{S}) where {T<:BlasFloat,S}
    A = transA.parent
    TS = promote_op(matprod, T, S)
    mul!(similar(x,TS,size(A,2)), transpose(A), convert(AbstractVector{TS}, x))
end
function *(transA::Transpose{<:Any,<:AbstractMatrix{T}}, x::AbstractVector{S}) where {T,S}
    A = transA.parent
    TS = promote_op(matprod, T, S)
    mul!(similar(x,TS,size(A,2)), transpose(A), x)
end
@inline function mul!(y::StridedVector{T}, transA::Transpose{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasFloat}
    A = transA.parent
    return gemv!(y, 'T', A, x, alpha, beta)
end
@inline function mul!(y::AbstractVector, transA::Transpose{<:Any,<:AbstractVecOrMat}, x::AbstractVector,
                      alpha::Number, beta::Number)
    A = transA.parent
    return generic_matvecmul!(y, 'T', A, x, MulAddMul(alpha, beta))
end

function *(adjA::Adjoint{<:Any,<:StridedMatrix{T}}, x::StridedVector{S}) where {T<:BlasFloat,S}
    A = adjA.parent
    TS = promote_op(matprod, T, S)
    mul!(similar(x,TS,size(A,2)), adjoint(A) ,convert(AbstractVector{TS},x))
end
function *(adjA::Adjoint{<:Any,<:AbstractMatrix{T}}, x::AbstractVector{S}) where {T,S}
    A = adjA.parent
    TS = promote_op(matprod, T, S)
    mul!(similar(x,TS,size(A,2)), adjoint(A), x)
end

@inline function mul!(y::StridedVector{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasReal}
    A = adjA.parent
    return mul!(y, transpose(A), x, alpha, beta)
end
@inline function mul!(y::StridedVector{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, x::StridedVector{T},
                      alpha::Number, beta::Number) where {T<:BlasComplex}
    A = adjA.parent
    return gemv!(y, 'C', A, x, alpha, beta)
end
@inline function mul!(y::AbstractVector, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, x::AbstractVector,
                      alpha::Number, beta::Number)
    A = adjA.parent
    return generic_matvecmul!(y, 'C', A, x, MulAddMul(alpha, beta))
end

# Vector-Matrix multiplication
(*)(x::AdjointAbsVec,   A::AbstractMatrix) = (A'*x')'
(*)(x::TransposeAbsVec, A::AbstractMatrix) = transpose(transpose(A)*transpose(x))

# Matrix-matrix multiplication

"""
    *(A::AbstractMatrix, B::AbstractMatrix)

Matrix multiplication.

# Examples
```jldoctest
julia> [1 1; 0 1] * [1 0; 1 1]
2×2 Array{Int64,2}:
 2  1
 1  1
```
"""
function (*)(A::AbstractMatrix, B::AbstractMatrix)
    TS = promote_op(matprod, eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A,1), size(B,2))), A, B)
end
# optimization for dispatching to BLAS, e.g. *(::Matrix{Float32}, ::Matrix{Float64})
# but avoiding the case *(::Matrix{<:BlasComplex}, ::Matrix{<:BlasReal})
# which is better handled by reinterpreting rather than promotion
function (*)(A::StridedMatrix{<:BlasReal}, B::StridedMatrix{<:BlasFloat})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A,1), size(B,2))), convert(AbstractArray{TS}, A), convert(AbstractArray{TS}, B))
end
function (*)(A::StridedMatrix{<:BlasComplex}, B::StridedMatrix{<:BlasComplex})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A,1), size(B,2))), convert(AbstractArray{TS}, A), convert(AbstractArray{TS}, B))
end

@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                      alpha::Number, beta::Number) where {T<:BlasFloat}
    return gemm_wrapper!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
end
# Complex Matrix times real matrix: We use that it is generally faster to reinterpret the
# first matrix as a real matrix and carry out real matrix matrix multiply
for elty in (Float32,Float64)
    @eval begin
        @inline function mul!(C::StridedMatrix{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, B::StridedVecOrMat{$elty},
                              alpha::Real, beta::Real)
            Afl = reinterpret($elty, A)
            Cfl = reinterpret($elty, C)
            mul!(Cfl, Afl, B, alpha, beta)
            return C
        end
    end
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
2×2 Array{Float64,2}:
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
2×2 Array{Float64,2}:
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
2×2 Array{Int64,2}:
 0  3
 1  2

julia> A = [1.0 2.0; 3.0 4.0];

julia> F = qr([0 1; -1 0]);

julia> rmul!(A, F.Q)
2×2 Array{Float64,2}:
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
2×2 Array{Int64,2}:
 2  1
 3  0

julia> B = [1.0 2.0; 3.0 4.0];

julia> F = qr([0 1; -1 0]);

julia> lmul!(F.Q, B)
2×2 Array{Float64,2}:
 3.0  4.0
 1.0  2.0
```
"""
lmul!(A, B)

@inline function mul!(C::StridedMatrix{T}, transA::Transpose{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    A = transA.parent
    if A===B
        return syrk_wrapper!(C, 'T', A, alpha, beta)
    else
        return gemm_wrapper!(C, 'T', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::AbstractMatrix, transA::Transpose{<:Any,<:AbstractVecOrMat}, B::AbstractVecOrMat,
                 alpha::Number, beta::Number)
    A = transA.parent
    return generic_matmatmul!(C, 'T', 'N', A, B, MulAddMul(alpha, beta))
end

@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, transB::Transpose{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    B = transB.parent
    if A===B
        return syrk_wrapper!(C, 'N', A, alpha, beta)
    else
        return gemm_wrapper!(C, 'N', 'T', A, B, MulAddMul(alpha, beta))
    end
end
# Complex matrix times transposed real matrix. Reinterpret the first matrix to real for efficiency.
for elty in (Float32,Float64)
    @eval begin
        @inline function mul!(C::StridedMatrix{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, transB::Transpose{<:Any,<:StridedVecOrMat{$elty}},
                         alpha::Real, beta::Real)
            Afl = reinterpret($elty, A)
            Cfl = reinterpret($elty, C)
            mul!(Cfl, Afl, transB, alpha, beta)
            return C
        end
    end
end
# collapsing the following two defs with C::AbstractVecOrMat yields ambiguities
@inline mul!(C::AbstractVector, A::AbstractVecOrMat, transB::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'T', A, transB.parent, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::AbstractVecOrMat, transB::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    generic_matmatmul!(C, 'N', 'T', A, transB.parent, MulAddMul(alpha, beta))

@inline function mul!(C::StridedMatrix{T}, transA::Transpose{<:Any,<:StridedVecOrMat{T}}, transB::Transpose{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    A = transA.parent
    B = transB.parent
    return gemm_wrapper!(C, 'T', 'T', A, B, MulAddMul(alpha, beta))
end
@inline function mul!(C::AbstractMatrix, transA::Transpose{<:Any,<:AbstractVecOrMat}, transB::Transpose{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number)
    A = transA.parent
    B = transB.parent
    return generic_matmatmul!(C, 'T', 'T', A, B, MulAddMul(alpha, beta))
end

@inline function mul!(C::StridedMatrix{T}, transA::Transpose{<:Any,<:StridedVecOrMat{T}}, transB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    A = transA.parent
    B = transB.parent
    return gemm_wrapper!(C, 'T', 'C', A, B, MulAddMul(alpha, beta))
end
@inline function mul!(C::AbstractMatrix, transA::Transpose{<:Any,<:AbstractVecOrMat}, transB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number)
    A = transA.parent
    B = transB.parent
    return generic_matmatmul!(C, 'T', 'C', A, B, MulAddMul(alpha, beta))
end

@inline function mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Real, beta::Real) where {T<:BlasReal}
    A = adjA.parent
    return mul!(C, transpose(A), B, alpha, beta)
end
@inline function mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, B::StridedVecOrMat{T},
                 alpha::Number, beta::Number) where {T<:BlasComplex}
    A = adjA.parent
    if A===B
        return herk_wrapper!(C, 'C', A, alpha, beta)
    else
        return gemm_wrapper!(C, 'C', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, B::AbstractVecOrMat,
                 alpha::Number, beta::Number)
    A = adjA.parent
    return generic_matmatmul!(C, 'C', 'N', A, B, MulAddMul(alpha, beta))
end

@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:StridedVecOrMat{<:BlasReal}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    B = adjB.parent
    return mul!(C, A, transpose(B), alpha, beta)
end
@inline function mul!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasComplex}
    B = adjB.parent
    if A === B
        return herk_wrapper!(C, 'N', A, alpha, beta)
    else
        return gemm_wrapper!(C, 'N', 'C', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::AbstractMatrix, A::AbstractVecOrMat, adjB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number)
    B = adjB.parent
    return generic_matmatmul!(C, 'N', 'C', A, B, MulAddMul(alpha, beta))
end

@inline function mul!(C::StridedMatrix{T}, adjA::Adjoint{<:Any,<:StridedVecOrMat{T}}, adjB::Adjoint{<:Any,<:StridedVecOrMat{T}},
                 alpha::Number, beta::Number) where {T<:BlasFloat}
    A = adjA.parent
    B = adjB.parent
    return gemm_wrapper!(C, 'C', 'C', A, B, MulAddMul(alpha, beta))
end
@inline function mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, adjB::Adjoint{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number)
    A = adjA.parent
    B = adjB.parent
    return generic_matmatmul!(C, 'C', 'C', A, B, MulAddMul(alpha, beta))
end
@inline function mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractVecOrMat}, transB::Transpose{<:Any,<:AbstractVecOrMat},
                 alpha::Number, beta::Number)
    A = adjA.parent
    B = transB.parent
    return generic_matmatmul!(C, 'C', 'T', A, B, MulAddMul(alpha, beta))
end
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
        throw(ArgumentError("uplo argument must be 'U' (upper) or 'L' (lower), got $uplo"))
    end
    A
end

function gemv!(y::StridedVector{T}, tA::AbstractChar, A::StridedVecOrMat{T}, x::StridedVector{T},
               α::Number=true, β::Number=false) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    if nA != length(x)
        throw(DimensionMismatch("second dimension of A, $nA, does not match length of x, $(length(x))"))
    end
    if mA != length(y)
        throw(DimensionMismatch("first dimension of A, $mA, does not match length of y, $(length(y))"))
    end
    if mA == 0
        return y
    end
    if nA == 0
        return _rmul_or_fill!(y, β)
    end

    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T} && stride(A, 1) == 1 && stride(A, 2) >= size(A, 1)
        return BLAS.gemv!(tA, alpha, A, x, beta, y)
    else
        return generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

function syrk_wrapper!(C::StridedMatrix{T}, tA::AbstractChar, A::StridedVecOrMat{T},
                       α::Number=true, β::Number=false) where {T<:BlasFloat}
    nC = checksquare(C)
    if tA == 'T'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'T'
    end
    if nC != mA
        throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0 || iszero(α)
        return _rmul_or_fill!(C, β)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C, tA, tAt, A, A, MulAddMul(α, β))
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C, tA, tAt, A, A, MulAddMul(α, β))
    end

    # BLAS.syrk! only updates symmetric C
    # alternatively, make non-zero β a show-stopper for BLAS.syrk!
    if iszero(β) || issymmetric(C)
        alpha, beta = promote(α, β, zero(T))
        if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(C, 1) == 1 &&
            stride(A, 2) >= size(A, 1) &&
            stride(C, 2) >= size(C, 1))
            return copytri!(BLAS.syrk!('U', tA, alpha, A, beta, C), 'U')
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, MulAddMul(α, β))
end

function herk_wrapper!(C::Union{StridedMatrix{T}, StridedMatrix{Complex{T}}}, tA::AbstractChar, A::Union{StridedVecOrMat{T}, StridedVecOrMat{Complex{T}}},
                       α::Number=true, β::Number=false) where {T<:BlasReal}
    nC = checksquare(C)
    if tA == 'C'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'C'
    end
    if nC != mA
        throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0
        return _rmul_or_fill!(C, β)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C, tA, tAt, A, A, MulAddMul(α, β))
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C, tA, tAt, A, A, MulAddMul(α, β))
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Matrix{T}(undef, mA, mA)

    if iszero(β) || issymmetric(C)
        alpha, beta = promote(α, β, zero(T))
        if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(C, 1) == 1 &&
            stride(A, 2) >= size(A, 1) &&
            stride(C, 2) >= size(C, 1))
            return copytri!(BLAS.herk!('U', tA, alpha, A, beta, C), 'U', true)
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, MulAddMul(α, β))
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
        throw(DimensionMismatch("A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    if mA == 0 || nA == 0 || nB == 0 || iszero(_add.alpha)
        if size(C) != (mA, nB)
            throw(DimensionMismatch("C has dimensions $(size(C)), should have ($mA,$nB)"))
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

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::AbstractChar, M::AbstractVecOrMat) = (size(M, t=='N' ? 1 : 2), size(M, t=='N' ? 2 : 1))

function copyto!(B::AbstractVecOrMat, ir_dest::UnitRange{Int}, jr_dest::UnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::UnitRange{Int}, jr_src::UnitRange{Int})
    if tM == 'N'
        copyto!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        LinearAlgebra.copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(B)
    end
    B
end

function copy_transpose!(B::AbstractMatrix, ir_dest::UnitRange{Int}, jr_dest::UnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::UnitRange{Int}, jr_src::UnitRange{Int})
    if tM == 'N'
        LinearAlgebra.copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copyto!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(B)
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
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA), vector B has length $mB"))
    end
    if mA != length(C)
        throw(DimensionMismatch("result C has length $(length(C)), needs length $mA"))
    end

    Astride = size(A, 1)

    @inbounds begin
    if tA == 'T'  # fastest case
        for k = 1:mA
            aoffs = (k-1)*Astride
            if mB == 0
                s = zero(R)
            else
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
            end
            for i = 1:nA
                s += transpose(A[aoffs+i]) * B[i]
            end
            _modify!(_add, s, C, k)
        end
    elseif tA == 'C'
        for k = 1:mA
            aoffs = (k-1)*Astride
            if mB == 0
                s = zero(R)
            else
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
            end
            for i = 1:nA
                s += A[aoffs + i]'B[i]
            end
            _modify!(_add, s, C, k)
        end
    else # tA == 'N'
        for i = 1:mA
            if !iszero(_add.beta)
                C[i] *= _add.beta
            elseif mB == 0
                C[i] = zero(R)
            else
                C[i] = zero(A[i]*B[1] + A[i]*B[1])
            end
        end
        for k = 1:mB
            aoffs = (k-1)*Astride
            b = _add(B[k], 0)
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
# per-thread arrays of buffers resized by __init__ if needed
const Abuf = [Vector{UInt8}(undef, tilebufsize)]
const Bbuf = [Vector{UInt8}(undef, tilebufsize)]
const Cbuf = [Vector{UInt8}(undef, tilebufsize)]

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
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA), matrix B has dimensions ($mB,$nB)"))
    end
    if size(C,1) != mA || size(C,2) != nB
        throw(DimensionMismatch("result C has dimensions $(size(C)), needs ($mA,$nB)"))
    end

    if iszero(_add.alpha) || isempty(A) || isempty(B)
        return _rmul_or_fill!(C, _add.beta)
    end

    tile_size = 0
    if isbitstype(R) && isbitstype(T) && isbitstype(S) && (tA == 'N' || tB != 'N')
        tile_size = floor(Int, sqrt(tilebufsize / max(sizeof(R), sizeof(S), sizeof(T))))
    end
    @inbounds begin
    if tile_size > 0
        sz = (tile_size, tile_size)
        # FIXME: This code is completely invalid!!!
        Atile = unsafe_wrap(Array, convert(Ptr{T}, pointer(Abuf[Threads.threadid()])), sz)
        Btile = unsafe_wrap(Array, convert(Ptr{S}, pointer(Bbuf[Threads.threadid()])), sz)

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
            # FIXME: This code is completely invalid!!!
            Ctile = unsafe_wrap(Array, convert(Ptr{R}, pointer(Cbuf[Threads.threadid()])), sz)
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
        throw(DimensionMismatch("A has size $(size(A)), B has size $(size(B)), C has size $(size(C))"))
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
        throw(DimensionMismatch("A has size $(size(A)), B has size $(size(B)), C has size $(size(C))"))
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
