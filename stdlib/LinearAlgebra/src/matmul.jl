# This file is a part of Julia. License is MIT: https://julialang.org/license

# matmul.jl: Everything to do with dense matrix multiplication

# unused internal constant, here for legacy reasons
const tilebufsize = 10800  # Approximately 32k/3

# Matrix-matrix multiplication

AdjOrTransStridedMat{T} = Union{Adjoint{<:Any, <:StridedMatrix{T}}, Transpose{<:Any, <:StridedMatrix{T}}}
StridedMaybeAdjOrTransMat{T} = Union{StridedMatrix{T}, Adjoint{<:Any, <:StridedMatrix{T}}, Transpose{<:Any, <:StridedMatrix{T}}}
StridedMaybeAdjOrTransVecOrMat{T} = Union{StridedVecOrMat{T}, AdjOrTrans{<:Any, <:StridedVecOrMat{T}}}

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
function (*)(A::StridedMaybeAdjOrTransMat{T}, x::StridedVector{S}) where {T<:BlasFloat,S<:Real}
    TS = promote_op(matprod, T, S)
    y = isconcretetype(TS) ? convert(AbstractVector{TS}, x) : x
    mul!(similar(x, TS, size(A,1)), A, y)
end
function (*)(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}
    TS = promote_op(matprod, T, S)
    mul!(similar(x, TS, axes(A,1)), A, x)
end

# these will throw a DimensionMismatch unless B has 1 row (or 1 col for transposed case):
(*)(a::AbstractVector, tB::TransposeAbsMat) = reshape(a, length(a), 1) * tB
(*)(a::AbstractVector, adjB::AdjointAbsMat) = reshape(a, length(a), 1) * adjB
(*)(a::AbstractVector, B::AbstractMatrix) = reshape(a, length(a), 1) * B

# Add a level of indirection and specialize _mul! to avoid ambiguities in mul!
@inline mul!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector,
                alpha::Number, beta::Number) = _mul!(y, A, x, alpha, beta)

_mul!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector,
                alpha::Number, beta::Number) =
    generic_matvecmul!(y, wrapper_char(A), _unwrap(A), x, alpha, beta)
# BLAS cases
# equal eltypes
generic_matvecmul!(y::StridedVector{T}, tA, A::StridedVecOrMat{T}, x::StridedVector{T},
                alpha::Number, beta::Number) where {T<:BlasFloat} =
    gemv!(y, tA, A, x, alpha, beta)
generic_matvecmul!(y::StridedVector{T}, tA, A::StridedVecOrMat{T}, x::StridedVector{T},
                _add::MulAddMul = MulAddMul()) where {T<:BlasFloat} =
    gemv!(y, tA, A, x, _add.alpha, _add.beta)
# Real (possibly transposed) matrix times complex vector.
# Multiply the matrix with the real and imaginary parts separately
generic_matvecmul!(y::StridedVector{Complex{T}}, tA, A::StridedVecOrMat{T}, x::StridedVector{Complex{T}},
                alpha::Number, beta::Number) where {T<:BlasReal} =
    gemv!(y, tA, A, x, alpha, beta)
generic_matvecmul!(y::StridedVector{Complex{T}}, tA, A::StridedVecOrMat{T}, x::StridedVector{Complex{T}},
                _add::MulAddMul = MulAddMul()) where {T<:BlasReal} =
    gemv!(y, tA, A, x, _add.alpha, _add.beta)
# Complex matrix times real vector.
# Reinterpret the matrix as a real matrix and do real matvec computation.
# works only in cooperation with BLAS when A is untransposed (tA == 'N')
# but that check is included in gemv! anyway
generic_matvecmul!(y::StridedVector{Complex{T}}, tA, A::StridedVecOrMat{Complex{T}}, x::StridedVector{T},
                alpha::Number, beta::Number) where {T<:BlasReal} =
    gemv!(y, tA, A, x, alpha, beta)
generic_matvecmul!(y::StridedVector{Complex{T}}, tA, A::StridedVecOrMat{Complex{T}}, x::StridedVector{T},
                _add::MulAddMul = MulAddMul()) where {T<:BlasReal} =
    gemv!(y, tA, A, x, _add.alpha, _add.beta)

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
2×2 Matrix{Int64}:
 2  1
 1  1
```
"""
function (*)(A::AbstractMatrix, B::AbstractMatrix)
    TS = promote_op(matprod, eltype(A), eltype(B))
    mul!(matprod_dest(A, B, TS), A, B)
end

matprod_dest(A, B, TS) = similar(B, TS, (size(A, 1), size(B, 2)))

# optimization for dispatching to BLAS, e.g. *(::Matrix{Float32}, ::Matrix{Float64})
# but avoiding the case *(::Matrix{<:BlasComplex}, ::Matrix{<:BlasReal})
# which is better handled by reinterpreting rather than promotion
function (*)(A::StridedMaybeAdjOrTransMat{<:BlasReal}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         wrapperop(A)(convert(AbstractArray{TS}, _unwrap(A))),
         wrapperop(B)(convert(AbstractArray{TS}, _unwrap(B))))
end
function (*)(A::StridedMaybeAdjOrTransMat{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasComplex})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         wrapperop(A)(convert(AbstractArray{TS}, _unwrap(A))),
         wrapperop(B)(convert(AbstractArray{TS}, _unwrap(B))))
end

# Complex Matrix times real matrix: We use that it is generally faster to reinterpret the
# first matrix as a real matrix and carry out real matrix matrix multiply
function (*)(A::StridedMatrix{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         convert(AbstractArray{TS}, A),
         wrapperop(B)(convert(AbstractArray{real(TS)}, _unwrap(B))))
end
function (*)(A::AdjOrTransStridedMat{<:BlasComplex}, B::StridedMaybeAdjOrTransMat{<:BlasReal})
    TS = promote_type(eltype(A), eltype(B))
    mul!(similar(B, TS, (size(A, 1), size(B, 2))),
         copymutable_oftype(A, TS), # remove AdjOrTrans to use reinterpret trick below
         wrapperop(B)(convert(AbstractArray{real(TS)}, _unwrap(B))))
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
        size(z,d) > 1 && throw(DimensionMismatch(string("z has dims ",
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
        size(z,d) > 1 && throw(DimensionMismatch(string("z has dims ",
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

Calculates the matrix-matrix or matrix-vector product ``A B`` and stores the result in `Y`,
overwriting the existing value of `Y`. Note that `Y` must not be aliased with either `A` or
`B`.

# Examples
```jldoctest
julia> A = [1.0 2.0; 3.0 4.0]; B = [1.0 1.0; 1.0 1.0]; Y = similar(B);

julia> mul!(Y, A, B) === Y
true

julia> Y
2×2 Matrix{Float64}:
 3.0  3.0
 7.0  7.0

julia> Y == A * B
true
```

# Implementation
For custom matrix and vector types, it is recommended to implement
5-argument `mul!` rather than implementing 3-argument `mul!` directly
if possible.
"""
mul!(C, A, B) = mul!(C, A, B, true, false)

"""
    mul!(C, A, B, α, β) -> C

Combined inplace matrix-matrix or matrix-vector multiply-add ``A B α + C β``.
The result is stored in `C` by overwriting it.  Note that `C` must not be
aliased with either `A` or `B`.

!!! compat "Julia 1.3"
    Five-argument `mul!` requires at least Julia 1.3.

# Examples
```jldoctest
julia> A = [1.0 2.0; 3.0 4.0]; B = [1.0 1.0; 1.0 1.0]; C = [1.0 2.0; 3.0 4.0];

julia> α, β = 100.0, 10.0;

julia> mul!(C, A, B, α, β) === C
true

julia> C
2×2 Matrix{Float64}:
 310.0  320.0
 730.0  740.0

julia> C_original = [1.0 2.0; 3.0 4.0]; # A copy of the original value of C

julia> C == A * B * α + C_original * β
true
```
"""
@inline mul!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat, α::Number, β::Number) = _mul!(C, A, B, α, β)
# Add a level of indirection and specialize _mul! to avoid ambiguities in mul!
@inline _mul!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat, α::Number, β::Number) =
    generic_matmatmul_wrapper!(
        C,
        wrapper_char(A),
        wrapper_char(B),
        _unwrap(A),
        _unwrap(B),
        α, β,
        Val(wrapper_char_NTC(A) & wrapper_char_NTC(B))
    )

# this indirection allows is to specialize on the types of the wrappers of A and B to some extent,
# even though the wrappers are stripped off in mul!
# By default, we ignore the wrapper info and forward the arguments to generic_matmatmul!
Base.@constprop :aggressive function generic_matmatmul_wrapper!(C, tA, tB, A, B, α, β, @nospecialize(val))
    generic_matmatmul!(C, tA, tB, A, B, α, β)
end


"""
    rmul!(A, B)

Calculate the matrix-matrix product ``AB``, overwriting `A`, and return the result.
Here, `B` must be of special matrix type, like, e.g., [`Diagonal`](@ref),
[`UpperTriangular`](@ref) or [`LowerTriangular`](@ref), or of some orthogonal type,
see [`QR`](@ref).

# Examples
```jldoctest
julia> A = [0 1; 1 0];

julia> B = UpperTriangular([1 2; 0 3]);

julia> rmul!(A, B);

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

julia> A = UpperTriangular([1 2; 0 3]);

julia> lmul!(A, B);

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

# We may inline the matmul2x2! and matmul3x3! calls for `α == true`
# to simplify the @stable_muladdmul branches
function matmul2x2or3x3_nonzeroalpha!(C, tA, tB, A, B, α, β)
    if size(C) == size(A) == size(B) == (2,2)
        matmul2x2!(C, tA, tB, A, B, α, β)
        return true
    end
    if size(C) == size(A) == size(B) == (3,3)
        matmul3x3!(C, tA, tB, A, B, α, β)
        return true
    end
    return false
end
function matmul2x2or3x3_nonzeroalpha!(C, tA, tB, A, B, α::Bool, β)
    if size(C) == size(A) == size(B) == (2,2)
        Aelements, Belements = _matmul2x2_elements(C, tA, tB, A, B)
        @stable_muladdmul _modify2x2!(Aelements, Belements, C, MulAddMul(true, β))
        return true
    end
    if size(C) == size(A) == size(B) == (3,3)
        Aelements, Belements = _matmul3x3_elements(C, tA, tB, A, B)
        @stable_muladdmul _modify3x3!(Aelements, Belements, C, MulAddMul(true, β))
        return true
    end
    return false
end

# THE one big BLAS dispatch. This is split into two methods to improve latency
Base.@constprop :aggressive function generic_matmatmul_wrapper!(C::StridedMatrix{T}, tA, tB, A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                                    α::Number, β::Number, ::Val{true}) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if any(iszero, size(A)) || any(iszero, size(B)) || iszero(α)
        if size(C) != (mA, nB)
            throw(DimensionMismatch(lazy"C has dimensions $(size(C)), should have ($mA,$nB)"))
        end
        return _rmul_or_fill!(C, β)
    end
    matmul2x2or3x3_nonzeroalpha!(C, tA, tB, A, B, α, β) && return C
    # We convert the chars to uppercase to potentially unwrap a WrapperChar,
    # and extract the char corresponding to the wrapper type
    tA_uc, tB_uc = uppercase(tA), uppercase(tB)
    # the map in all ensures constprop by acting on tA and tB individually, instead of looping over them.
    if tA_uc == 'T' && tB_uc == 'N' && A === B
        return syrk_wrapper!(C, 'T', A, α, β)
    elseif tA_uc == 'N' && tB_uc == 'T' && A === B
        return syrk_wrapper!(C, 'N', A, α, β)
    elseif tA_uc == 'C' && tB_uc == 'N' && A === B
        return herk_wrapper!(C, 'C', A, α, β)
    elseif tA_uc == 'N' && tB_uc == 'C' && A === B
        return herk_wrapper!(C, 'N', A, α, β)
    else
        return gemm_wrapper!(C, tA, tB, A, B, α, β)
    end
end
Base.@constprop :aggressive function generic_matmatmul_wrapper!(C::StridedMatrix{T}, tA, tB, A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                                    α::Number, β::Number, ::Val{false}) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if any(iszero, size(A)) || any(iszero, size(B)) || iszero(α)
        if size(C) != (mA, nB)
            throw(DimensionMismatch(lazy"C has dimensions $(size(C)), should have ($mA,$nB)"))
        end
        return _rmul_or_fill!(C, β)
    end
    matmul2x2or3x3_nonzeroalpha!(C, tA, tB, A, B, α, β) && return C
    # We convert the chars to uppercase to potentially unwrap a WrapperChar,
    # and extract the char corresponding to the wrapper type
    tA_uc, tB_uc = uppercase(tA), uppercase(tB)
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        if tA_uc == 'S' && tB_uc == 'N'
            return BLAS.symm!('L', tA == 'S' ? 'U' : 'L', alpha, A, B, beta, C)
        elseif tA_uc == 'N' && tB_uc == 'S'
            return BLAS.symm!('R', tB == 'S' ? 'U' : 'L', alpha, B, A, beta, C)
        elseif tA_uc == 'H' && tB_uc == 'N'
            return BLAS.hemm!('L', tA == 'H' ? 'U' : 'L', alpha, A, B, beta, C)
        elseif tA_uc == 'N' && tB_uc == 'H'
            return BLAS.hemm!('R', tB == 'H' ? 'U' : 'L', alpha, B, A, beta, C)
        end
    end
    return _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul(α, β))
end
# legacy method
Base.@constprop :aggressive generic_matmatmul!(C::StridedMatrix{T}, tA, tB, A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
        _add::MulAddMul = MulAddMul()) where {T<:BlasFloat} =
    generic_matmatmul!(C, tA, tB, A, B, _add.alpha, _add.beta)

function generic_matmatmul_wrapper!(C::StridedVecOrMat{Complex{T}}, tA, tB, A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
                    α::Number, β::Number, ::Val{true}) where {T<:BlasReal}
    gemm_wrapper!(C, tA, tB, A, B, α, β)
end
Base.@constprop :aggressive function generic_matmatmul_wrapper!(C::StridedVecOrMat{Complex{T}}, tA, tB, A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
                    α::Number, β::Number, ::Val{false}) where {T<:BlasReal}
    _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul(α, β))
end
# legacy method
Base.@constprop :aggressive generic_matmatmul!(C::StridedVecOrMat{Complex{T}}, tA, tB, A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
        _add::MulAddMul = MulAddMul()) where {T<:BlasReal} =
    generic_matmatmul!(C, tA, tB, A, B, _add.alpha, _add.beta)

# Supporting functions for matrix multiplication

# copy transposed(adjoint) of upper(lower) side-diagonals. Optionally include diagonal.
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

_fullstride2(A, f=identity) = f(stride(A, 2)) >= size(A, 1)
# for some standard StridedArrays, the _fullstride2 condition is known to hold at compile-time
# We specialize the function for certain StridedArray subtypes
_fullstride2(A::StridedArrayStdSubArray, ::typeof(abs)) = true
_fullstride2(A::StridedArrayStdSubArrayIncr, ::typeof(identity)) = true

Base.@constprop :aggressive function gemv!(y::StridedVector{T}, tA::AbstractChar,
                A::StridedVecOrMat{T}, x::StridedVector{T},
               α::Number=true, β::Number=false) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
        stride(A, 1) == 1 && _fullstride2(A, abs) &&
        !iszero(stride(x, 1)) && # We only check input's stride here.
        if tA_uc in ('N', 'T', 'C')
            return BLAS.gemv!(tA, alpha, A, x, beta, y)
        elseif tA_uc == 'S'
            return BLAS.symv!(tA == 'S' ? 'U' : 'L', alpha, A, x, beta, y)
        elseif tA_uc == 'H'
            return BLAS.hemv!(tA == 'H' ? 'U' : 'L', alpha, A, x, beta, y)
        end
    end
    if tA_uc in ('S', 'H')
        # re-wrap again and use plain ('N') matvec mul algorithm,
        # because _generic_matvecmul! can't handle the HermOrSym cases specifically
        return @stable_muladdmul _generic_matvecmul!(y, 'N', wrap(A, tA), x, MulAddMul(α, β))
    else
        return @stable_muladdmul _generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

Base.@constprop :aggressive function gemv!(y::StridedVector{Complex{T}}, tA::AbstractChar, A::StridedVecOrMat{Complex{T}}, x::StridedVector{T},
    α::Number = true, β::Number = false) where {T<:BlasReal}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
            stride(A, 1) == 1 && _fullstride2(A, abs) &&
            stride(y, 1) == 1 && tA_uc == 'N' && # reinterpret-based optimization is valid only for contiguous `y`
            !iszero(stride(x, 1))
        BLAS.gemv!(tA, alpha, reinterpret(T, A), x, beta, reinterpret(T, y))
        return y
    else
        Anew, ta = tA_uc in ('S', 'H') ? (wrap(A, tA), oftype(tA, 'N')) : (A, tA)
        return @stable_muladdmul _generic_matvecmul!(y, ta, Anew, x, MulAddMul(α, β))
    end
end

Base.@constprop :aggressive function gemv!(y::StridedVector{Complex{T}}, tA::AbstractChar,
        A::StridedVecOrMat{T}, x::StridedVector{Complex{T}},
        α::Number = true, β::Number = false) where {T<:BlasReal}
    mA, nA = lapack_size(tA, A)
    nA != length(x) &&
        throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match length of x, $(length(x))"))
    mA != length(y) &&
        throw(DimensionMismatch(lazy"first dimension of A, $mA, does not match length of y, $(length(y))"))
    mA == 0 && return y
    nA == 0 && return _rmul_or_fill!(y, β)
    alpha, beta = promote(α, β, zero(T))
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    @views if alpha isa Union{Bool,T} && beta isa Union{Bool,T} &&
            stride(A, 1) == 1 && _fullstride2(A, abs) &&
            !iszero(stride(x, 1)) && tA_uc in ('N', 'T', 'C')
        xfl = reinterpret(reshape, T, x) # Use reshape here.
        yfl = reinterpret(reshape, T, y)
        BLAS.gemv!(tA, alpha, A, xfl[1, :], beta, yfl[1, :])
        BLAS.gemv!(tA, alpha, A, xfl[2, :], beta, yfl[2, :])
        return y
    elseif tA_uc in ('S', 'H')
        # re-wrap again and use plain ('N') matvec mul algorithm,
        # because _generic_matvecmul! can't handle the HermOrSym cases specifically
        return @stable_muladdmul _generic_matvecmul!(y, 'N', wrap(A, tA), x, MulAddMul(α, β))
    else
        return @stable_muladdmul _generic_matvecmul!(y, tA, A, x, MulAddMul(α, β))
    end
end

# the aggressive constprop pushes tA and tB into gemm_wrapper!, which is needed for wrap calls within it
# to be concretely inferred
Base.@constprop :aggressive function syrk_wrapper!(C::StridedMatrix{T}, tA::AbstractChar, A::StridedVecOrMat{T},
        alpha::Number, beta::Number) where {T<:BlasFloat}
    nC = checksquare(C)
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    if tA_uc == 'T'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'T'
    end
    if nC != mA
        throw(DimensionMismatch(lazy"output matrix has size: $(nC), but should have size $(mA)"))
    end

    # BLAS.syrk! only updates symmetric C
    # alternatively, make non-zero β a show-stopper for BLAS.syrk!
    if iszero(beta) || issymmetric(C)
        α, β = promote(alpha, beta, zero(T))
        if (alpha isa Union{Bool,T} &&
                beta isa Union{Bool,T} &&
                stride(A, 1) == stride(C, 1) == 1 &&
                _fullstride2(A) && _fullstride2(C))
            return copytri!(BLAS.syrk!('U', tA, alpha, A, beta, C), 'U')
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, alpha, beta)
end
# legacy method
syrk_wrapper!(C::StridedMatrix{T}, tA::AbstractChar, A::StridedVecOrMat{T}, _add::MulAddMul = MulAddMul()) where {T<:BlasFloat} =
    syrk_wrapper!(C, tA, A, _add.alpha, _add.beta)

# the aggressive constprop pushes tA and tB into gemm_wrapper!, which is needed for wrap calls within it
# to be concretely inferred
Base.@constprop :aggressive function herk_wrapper!(C::Union{StridedMatrix{T}, StridedMatrix{Complex{T}}}, tA::AbstractChar, A::Union{StridedVecOrMat{T}, StridedVecOrMat{Complex{T}}},
        α::Number, β::Number) where {T<:BlasReal}
    nC = checksquare(C)
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    if tA_uc == 'C'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'C'
    end
    if nC != mA
        throw(DimensionMismatch(lazy"output matrix has size: $(nC), but should have size $(mA)"))
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Matrix{T}(undef, mA, mA)

    if iszero(β) || issymmetric(C)
        alpha, beta = promote(α, β, zero(T))
        if (alpha isa Union{Bool,T} &&
                beta isa Union{Bool,T} &&
                stride(A, 1) == stride(C, 1) == 1 &&
                _fullstride2(A) && _fullstride2(C))
            return copytri!(BLAS.herk!('U', tA, alpha, A, beta, C), 'U', true)
        end
    end
    return gemm_wrapper!(C, tA, tAt, A, A, α, β)
end
# legacy method
herk_wrapper!(C::Union{StridedMatrix{T}, StridedMatrix{Complex{T}}}, tA::AbstractChar, A::Union{StridedVecOrMat{T}, StridedVecOrMat{Complex{T}}},
        _add::MulAddMul = MulAddMul()) where {T<:BlasReal} =
    herk_wrapper!(C, tA, A, _add.alpha, _add.beta)

# Aggressive constprop helps propagate the values of tA and tB into wrap, which
# makes the calls concretely inferred
Base.@constprop :aggressive function gemm_wrapper(tA::AbstractChar, tB::AbstractChar,
                      A::StridedVecOrMat{T},
                      B::StridedVecOrMat{T}) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = similar(B, T, mA, nB)
    # We convert the chars to uppercase to potentially unwrap a WrapperChar,
    # and extract the char corresponding to the wrapper type
    tA_uc, tB_uc = uppercase(tA), uppercase(tB)
    # the map in all ensures constprop by acting on tA and tB individually, instead of looping over them.
    if all(map(in(('N', 'T', 'C')), (tA_uc, tB_uc)))
        gemm_wrapper!(C, tA, tB, A, B, true, false)
    else
        _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul())
    end
end

# Aggressive constprop helps propagate the values of tA and tB into wrap, which
# makes the calls concretely inferred
Base.@constprop :aggressive function gemm_wrapper!(C::StridedVecOrMat{T}, tA::AbstractChar, tB::AbstractChar,
                       A::StridedVecOrMat{T}, B::StridedVecOrMat{T},
                       α::Number, β::Number) where {T<:BlasFloat}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB
        throw(DimensionMismatch(lazy"A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    alpha, beta = promote(α, β, zero(T))
    if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(B, 1) == stride(C, 1) == 1 &&
            _fullstride2(A) && _fullstride2(B) && _fullstride2(C))
        return BLAS.gemm!(tA, tB, alpha, A, B, beta, C)
    end
    _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul(α, β))
end
# legacy method
gemm_wrapper!(C::StridedVecOrMat{T}, tA::AbstractChar, tB::AbstractChar,
        A::StridedVecOrMat{T}, B::StridedVecOrMat{T}, _add::MulAddMul = MulAddMul()) where {T<:BlasFloat} =
    gemm_wrapper!(C, tA, tB, A, B, _add.alpha, _add.beta)

# Aggressive constprop helps propagate the values of tA and tB into wrap, which
# makes the calls concretely inferred
Base.@constprop :aggressive function gemm_wrapper!(C::StridedVecOrMat{Complex{T}}, tA::AbstractChar, tB::AbstractChar,
                       A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T},
                       α::Number, β::Number) where {T<:BlasReal}
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB
        throw(DimensionMismatch(lazy"A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    alpha, beta = promote(α, β, zero(T))

    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char

    # Make-sure reinterpret-based optimization is BLAS-compatible.
    if (alpha isa Union{Bool,T} &&
            beta isa Union{Bool,T} &&
            stride(A, 1) == stride(B, 1) == stride(C, 1) == 1 &&
            _fullstride2(A) && _fullstride2(B) && _fullstride2(C) && tA_uc == 'N')
        BLAS.gemm!(tA, tB, alpha, reinterpret(T, A), B, beta, reinterpret(T, C))
        return C
    end
    _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul(α, β))
end
# legacy method
gemm_wrapper!(C::StridedVecOrMat{Complex{T}}, tA::AbstractChar, tB::AbstractChar,
        A::StridedVecOrMat{Complex{T}}, B::StridedVecOrMat{T}, _add::MulAddMul = MulAddMul()) where {T<:BlasReal} =
    gemm_wrapper!(C, tA, tB, A, B, _add.alpha, _add.beta)

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::AbstractChar, M::AbstractVecOrMat) = (size(M, t=='N' ? 1 : 2), size(M, t=='N' ? 2 : 1))

"""
    copyto!(B::AbstractMatrix, ir_dest::AbstractUnitRange, jr_dest::AbstractUnitRange,
            tM::AbstractChar,
            M::AbstractVecOrMat, ir_src::AbstractUnitRange, jr_src::AbstractUnitRange) -> B

Efficiently copy elements of matrix `M` to `B` conditioned on the character
parameter `tM` as follows:

| `tM` | Destination | Source |
| --- | :--- | :--- |
| `'N'` | `B[ir_dest, jr_dest]` | `M[ir_src, jr_src]` |
| `'T'` | `B[ir_dest, jr_dest]` | `transpose(M)[ir_src, jr_src]` |
| `'C'` | `B[ir_dest, jr_dest]` | `adjoint(M)[ir_src, jr_src]` |

The elements `B[ir_dest, jr_dest]` are overwritten. Furthermore, the index range
parameters must satisfy `length(ir_dest) == length(ir_src)` and
`length(jr_dest) == length(jr_src)`.

See also [`copy_transpose!`](@ref) and [`copy_adjoint!`](@ref).
"""
function copyto!(B::AbstractVecOrMat, ir_dest::AbstractUnitRange{Int}, jr_dest::AbstractUnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::AbstractUnitRange{Int}, jr_src::AbstractUnitRange{Int})
    tM_uc = uppercase(tM) # potentially convert a WrapperChar to a Char
    if tM_uc == 'N'
        copyto!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    elseif tM_uc == 'T'
        copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
    else
        copy_adjoint!(B, ir_dest, jr_dest, M, jr_src, ir_src)
    end
    B
end

"""
    copy_transpose!(B::AbstractMatrix, ir_dest::AbstractUnitRange, jr_dest::AbstractUnitRange,
                    tM::AbstractChar,
                    M::AbstractVecOrMat, ir_src::AbstractUnitRange, jr_src::AbstractUnitRange) -> B

Efficiently copy elements of matrix `M` to `B` conditioned on the character
parameter `tM` as follows:

| `tM` | Destination | Source |
| --- | :--- | :--- |
| `'N'` | `B[ir_dest, jr_dest]` | `transpose(M)[jr_src, ir_src]` |
| `'T'` | `B[ir_dest, jr_dest]` | `M[jr_src, ir_src]` |
| `'C'` | `B[ir_dest, jr_dest]` | `conj(M)[jr_src, ir_src]` |

The elements `B[ir_dest, jr_dest]` are overwritten. Furthermore, the index
range parameters must satisfy `length(ir_dest) == length(jr_src)` and
`length(jr_dest) == length(ir_src)`.

See also [`copyto!`](@ref) and [`copy_adjoint!`](@ref).
"""
function copy_transpose!(B::AbstractMatrix, ir_dest::AbstractUnitRange{Int}, jr_dest::AbstractUnitRange{Int}, tM::AbstractChar, M::AbstractVecOrMat, ir_src::AbstractUnitRange{Int}, jr_src::AbstractUnitRange{Int})
    tM_uc = uppercase(tM) # potentially convert a WrapperChar to a Char
    if tM_uc == 'N'
        copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copyto!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM_uc == 'C' && conj!(@view B[ir_dest, jr_dest])
    end
    B
end

# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the generic version is also called as fallback for
#       strides != 1 cases

generic_matvecmul!(C::AbstractVector, tA, A::AbstractVecOrMat, B::AbstractVector, alpha::Number, beta::Number) =
    @stable_muladdmul generic_matvecmul!(C, tA, A, B, MulAddMul(alpha, beta))
@inline function generic_matvecmul!(C::AbstractVector, tA, A::AbstractVecOrMat, B::AbstractVector,
                                    _add::MulAddMul = MulAddMul())
    tA_uc = uppercase(tA) # potentially convert a WrapperChar to a Char
    Anew, ta = tA_uc in ('S', 'H') ? (wrap(A, tA), oftype(tA, 'N')) : (A, tA)
    return _generic_matvecmul!(C, ta, Anew, B, _add)
end

function _generic_matvecmul!(C::AbstractVector, tA, A::AbstractVecOrMat, B::AbstractVector,
                            _add::MulAddMul = MulAddMul())
    require_one_based_indexing(C, A, B)
    @assert tA in ('N', 'T', 'C')
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
                firstterm = transpose(A[aoffs + 1])*B[1]
                s = zero(firstterm + firstterm)
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
                firstterm = A[aoffs + 1]'B[1]
                s = zero(firstterm + firstterm)
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
            b = _add(B[k])
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
    generic_matmatmul!(C, tA, tB, A, B, true, false)
end

# aggressive const prop makes mixed eltype mul!(C, A, B) invoke _generic_matmatmul! directly
# legacy method
Base.@constprop :aggressive generic_matmatmul!(C::AbstractVecOrMat, tA, tB, A::AbstractVecOrMat, B::AbstractVecOrMat, _add::MulAddMul = MulAddMul()) =
    _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), _add)
Base.@constprop :aggressive generic_matmatmul!(C::AbstractVecOrMat, tA, tB, A::AbstractVecOrMat, B::AbstractVecOrMat, α::Number, β::Number) =
    _generic_matmatmul!(C, wrap(A, tA), wrap(B, tB), MulAddMul(α, β))

@noinline function _generic_matmatmul!(C::AbstractVecOrMat{R}, A::AbstractVecOrMat{T}, B::AbstractVecOrMat{S},
                             _add::MulAddMul) where {T,S,R}
    AxM = axes(A, 1)
    AxK = axes(A, 2) # we use two `axes` calls in case of `AbstractVector`
    BxK = axes(B, 1)
    BxN = axes(B, 2)
    CxM = axes(C, 1)
    CxN = axes(C, 2)
    if AxM != CxM
        throw(DimensionMismatch(lazy"matrix A has axes ($AxM,$AxK), matrix C has axes ($CxM,$CxN)"))
    end
    if AxK != BxK
        throw(DimensionMismatch(lazy"matrix A has axes ($AxM,$AxK), matrix B has axes ($BxK,$CxN)"))
    end
    if BxN != CxN
        throw(DimensionMismatch(lazy"matrix B has axes ($BxK,$BxN), matrix C has axes ($CxM,$CxN)"))
    end
    if isbitstype(R) && sizeof(R) ≤ 16 && !(A isa Adjoint || A isa Transpose)
        _rmul_or_fill!(C, _add.beta)
        (iszero(_add.alpha) || isempty(A) || isempty(B)) && return C
        @inbounds for n in BxN, k in BxK
            Balpha = B[k,n]*_add.alpha
            @simd for m in AxM
                C[m,n] = muladd(A[m,k], Balpha, C[m,n])
            end
        end
    elseif isbitstype(R) && sizeof(R) ≤ 16 && ((A isa Adjoint && B isa Adjoint) || (A isa Transpose && B isa Transpose))
        _rmul_or_fill!(C, _add.beta)
        (iszero(_add.alpha) || isempty(A) || isempty(B)) && return C
        t = wrapperop(A)
        pB = parent(B)
        pA = parent(A)
        tmp = similar(C, CxN)
        ci = first(CxM)
        ta = t(_add.alpha)
        for i in AxM
            mul!(tmp, pB, view(pA, :, i))
            C[ci,:] .+= t.(ta .* tmp)
            ci += 1
        end
    else
        if iszero(_add.alpha) || isempty(A) || isempty(B)
            return _rmul_or_fill!(C, _add.beta)
        end
        a1 = first(AxK)
        b1 = first(BxK)
        @inbounds for i in AxM, j in BxN
            z2 = zero(A[i, a1]*B[b1, j] + A[i, a1]*B[b1, j])
            Ctmp = convert(promote_type(R, typeof(z2)), z2)
            @simd for k in AxK
                Ctmp = muladd(A[i, k], B[k, j], Ctmp)
            end
            _modify!(_add, Ctmp, C, (i,j))
        end
    end
    return C
end


# multiply 2x2 matrices
Base.@constprop :aggressive function matmul2x2(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}
    matmul2x2!(similar(B, promote_op(matprod, T, S), 2, 2), tA, tB, A, B)
end

function __matmul_checks(C, A, B, sz)
    require_one_based_indexing(C, A, B)
    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end
    if !(size(A) == size(B) == size(C) == sz)
        throw(DimensionMismatch(lazy"A has size $(size(A)), B has size $(size(B)), C has size $(size(C))"))
    end
    return nothing
end

# separate function with the core of matmul2x2! that doesn't depend on a MulAddMul
Base.@constprop :aggressive function _matmul2x2_elements(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix)
    __matmul_checks(C, A, B, (2,2))
    __matmul2x2_elements(tA, tB, A, B)
end
Base.@constprop :aggressive function __matmul2x2_elements(tA, A::AbstractMatrix)
    @inbounds begin
    tA_uc = uppercase(tA) # possibly unwrap a WrapperChar
    if tA_uc == 'N'
        A11 = A[1,1]; A12 = A[1,2]; A21 = A[2,1]; A22 = A[2,2]
    elseif tA_uc == 'T'
        # TODO making these lazy could improve perf
        A11 = copy(transpose(A[1,1])); A12 = copy(transpose(A[2,1]))
        A21 = copy(transpose(A[1,2])); A22 = copy(transpose(A[2,2]))
    elseif tA_uc == 'C'
        # TODO making these lazy could improve perf
        A11 = copy(A[1,1]'); A12 = copy(A[2,1]')
        A21 = copy(A[1,2]'); A22 = copy(A[2,2]')
    elseif tA_uc == 'S'
        if isuppercase(tA) # tA == 'S'
            A11 = symmetric(A[1,1], :U); A12 = A[1,2]
            A21 = copy(transpose(A[1,2])); A22 = symmetric(A[2,2], :U)
        else
            A11 = symmetric(A[1,1], :L); A12 = copy(transpose(A[2,1]))
            A21 = A[2,1]; A22 = symmetric(A[2,2], :L)
        end
    elseif tA_uc == 'H'
        if isuppercase(tA) # tA == 'H'
            A11 = hermitian(A[1,1], :U); A12 = A[1,2]
            A21 = copy(adjoint(A[1,2])); A22 = hermitian(A[2,2], :U)
        else # if tA == 'h'
            A11 = hermitian(A[1,1], :L); A12 = copy(adjoint(A[2,1]))
            A21 = A[2,1]; A22 = hermitian(A[2,2], :L)
        end
    end
    end # inbounds
    A11, A12, A21, A22
end
Base.@constprop :aggressive __matmul2x2_elements(tA, tB, A, B) = __matmul2x2_elements(tA, A), __matmul2x2_elements(tB, B)

function _modify2x2!(Aelements, Belements, C, _add)
    (A11, A12, A21, A22), (B11, B12, B21, B22) = Aelements, Belements
    @inbounds begin
    _modify!(_add, A11*B11 + A12*B21, C, (1,1))
    _modify!(_add, A21*B11 + A22*B21, C, (2,1))
    _modify!(_add, A11*B12 + A12*B22, C, (1,2))
    _modify!(_add, A21*B12 + A22*B22, C, (2,2))
    end # inbounds
    C
end
Base.@constprop :aggressive function matmul2x2!(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix,
                    α = true, β = false)
    Aelements, Belements = _matmul2x2_elements(C, tA, tB, A, B)
    @stable_muladdmul _modify2x2!(Aelements, Belements, C, MulAddMul(α, β))
    C
end

# Multiply 3x3 matrices
Base.@constprop :aggressive function matmul3x3(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}
    matmul3x3!(similar(B, promote_op(matprod, T, S), 3, 3), tA, tB, A, B)
end

# separate function with the core of matmul3x3! that doesn't depend on a MulAddMul
Base.@constprop :aggressive function _matmul3x3_elements(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix)
    __matmul_checks(C, A, B, (3,3))
    __matmul3x3_elements(tA, tB, A, B)
end
Base.@constprop :aggressive function __matmul3x3_elements(tA, A::AbstractMatrix)
    @inbounds begin
    tA_uc = uppercase(tA) # possibly unwrap a WrapperChar
    if tA_uc == 'N'
        A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3]
        A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3]
        A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3]
    elseif tA_uc == 'T'
        # TODO making these lazy could improve perf
        A11 = copy(transpose(A[1,1])); A12 = copy(transpose(A[2,1])); A13 = copy(transpose(A[3,1]))
        A21 = copy(transpose(A[1,2])); A22 = copy(transpose(A[2,2])); A23 = copy(transpose(A[3,2]))
        A31 = copy(transpose(A[1,3])); A32 = copy(transpose(A[2,3])); A33 = copy(transpose(A[3,3]))
    elseif tA_uc == 'C'
        # TODO making these lazy could improve perf
        A11 = copy(A[1,1]'); A12 = copy(A[2,1]'); A13 = copy(A[3,1]')
        A21 = copy(A[1,2]'); A22 = copy(A[2,2]'); A23 = copy(A[3,2]')
        A31 = copy(A[1,3]'); A32 = copy(A[2,3]'); A33 = copy(A[3,3]')
    elseif tA_uc == 'S'
        if isuppercase(tA) # tA == 'S'
            A11 = symmetric(A[1,1], :U); A12 = A[1,2]; A13 = A[1,3]
            A21 = copy(transpose(A[1,2])); A22 = symmetric(A[2,2], :U); A23 = A[2,3]
            A31 = copy(transpose(A[1,3])); A32 = copy(transpose(A[2,3])); A33 = symmetric(A[3,3], :U)
        else
            A11 = symmetric(A[1,1], :L); A12 = copy(transpose(A[2,1])); A13 = copy(transpose(A[3,1]))
            A21 = A[2,1]; A22 = symmetric(A[2,2], :L); A23 = copy(transpose(A[3,2]))
            A31 = A[3,1]; A32 = A[3,2]; A33 = symmetric(A[3,3], :L)
        end
    elseif tA_uc == 'H'
        if isuppercase(tA) # tA == 'H'
            A11 = hermitian(A[1,1], :U); A12 = A[1,2]; A13 = A[1,3]
            A21 = copy(adjoint(A[1,2])); A22 = hermitian(A[2,2], :U); A23 = A[2,3]
            A31 = copy(adjoint(A[1,3])); A32 = copy(adjoint(A[2,3])); A33 = hermitian(A[3,3], :U)
        else # if tA == 'h'
            A11 = hermitian(A[1,1], :L); A12 = copy(adjoint(A[2,1])); A13 = copy(adjoint(A[3,1]))
            A21 = A[2,1]; A22 = hermitian(A[2,2], :L); A23 = copy(adjoint(A[3,2]))
            A31 = A[3,1]; A32 = A[3,2]; A33 = hermitian(A[3,3], :L)
        end
    end
    end # inbounds
    A11, A12, A13, A21, A22, A23, A31, A32, A33
end
Base.@constprop :aggressive __matmul3x3_elements(tA, tB, A, B) = __matmul3x3_elements(tA, A), __matmul3x3_elements(tB, B)

function _modify3x3!(Aelements, Belements, C, _add)
    (A11, A12, A13, A21, A22, A23, A31, A32, A33),
        (B11, B12, B13, B21, B22, B23, B31, B32, B33) = Aelements, Belements
    @inbounds begin
    _modify!(_add, A11*B11 + A12*B21 + A13*B31, C, (1,1))
    _modify!(_add, A21*B11 + A22*B21 + A23*B31, C, (2,1))
    _modify!(_add, A31*B11 + A32*B21 + A33*B31, C, (3,1))

    _modify!(_add, A11*B12 + A12*B22 + A13*B32, C, (1,2))
    _modify!(_add, A21*B12 + A22*B22 + A23*B32, C, (2,2))
    _modify!(_add, A31*B12 + A32*B22 + A33*B32, C, (3,2))

    _modify!(_add, A11*B13 + A12*B23 + A13*B33, C, (1,3))
    _modify!(_add, A21*B13 + A22*B23 + A23*B33, C, (2,3))
    _modify!(_add, A31*B13 + A32*B23 + A33*B33, C, (3,3))
    end # inbounds
    C
end
Base.@constprop :aggressive function matmul3x3!(C::AbstractMatrix, tA, tB, A::AbstractMatrix, B::AbstractMatrix,
                    α = true, β = false)

    Aelements, Belements = _matmul3x3_elements(C, tA, tB, A, B)
    @stable_muladdmul _modify3x3!(Aelements, Belements, C, MulAddMul(α, β))
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
