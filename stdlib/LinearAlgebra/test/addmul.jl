# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestAddmul

using Base: rtoldefault
using Test
using LinearAlgebra
using LinearAlgebra: AbstractTriangular
using SparseArrays
using Random

_rand(::Type{T}) where {T <: AbstractFloat} = T(randn())
_rand(::Type{T}) where {F, T <: Complex{F}} = T(_rand(F), _rand(F))
_rand(::Type{T}) where {T <: Integer} =
    T(rand(max(typemin(T), -10):min(typemax(T), 10)))
_rand(::Type{BigInt}) = BigInt(_rand(Int))

function _rand(A::Type{<:Array}, shape)
    T = eltype(A)
    data = T[_rand(T) for _ in 1:prod(shape)]
    return copy(reshape(data, shape))
end

constructor_of(::Type{T}) where T = getfield(parentmodule(T), nameof(T))

function _rand(A::Type{<: AbstractArray}, shape)
    data = _rand(Array{eltype(A)}, shape)
    T = constructor_of(A)
    if A <: Union{Bidiagonal, Hermitian, Symmetric}
        return T(data, rand([:U, :L]))
        # Maybe test with both :U and :L?
    end
    return T(data)
end

_rand(A::Type{<: SymTridiagonal{T}}, shape) where {T} =
    SymTridiagonal(_rand(Symmetric{T}, shape))

const FloatOrC = Union{AbstractFloat, Complex{<: AbstractFloat}}
const IntegerOrC = Union{Integer, Complex{<: Integer}}
const LTri = Union{LowerTriangular, UnitLowerTriangular, Diagonal}
const UTri = Union{UpperTriangular, UnitUpperTriangular, Diagonal}

needsquare(::Type{<:Matrix}) = false
needsquare(::Type) = true

testdata = []

sizecandidates = 1:4
floattypes = [
    Float64, Float32, ComplexF64, ComplexF32,  # BlasFloat
    BigFloat,
]
inttypes = [
    Int,
    BigInt,
]
# `Bool` can be added to `inttypes` but it's hard to handle
# `InexactError` bug that is mentioned in:
# https://github.com/JuliaLang/julia/issues/30094#issuecomment-440175887
alleltypes = [floattypes; inttypes]
celtypes = [Float64, ComplexF64, BigFloat, Int]

mattypes = [
    Matrix,
    Bidiagonal,
    Diagonal,
    Hermitian,
    LowerTriangular,
    SymTridiagonal,
    Symmetric,
    Tridiagonal,
    UnitLowerTriangular,
    UnitUpperTriangular,
    UpperTriangular,
]

isnanfillable(::AbstractArray) = false
isnanfillable(::Array{<:AbstractFloat}) = true
isnanfillable(A::AbstractArray{<:AbstractFloat}) = parent(A) isa Array

"""
Sample `n` elements from `S` on average but make sure at least one
element is sampled.
"""
function sample(S, n::Real)
    length(S) <= n && return S
    xs = randsubseq(S, n / length(S))
    return length(xs) > 0 ? xs : rand(S, 1)  # sample at least one
end

function inputeltypes(celt, alleltypes = alleltypes)
    # Skip if destination type is "too small"
    celt <: Bool && return []
    filter(alleltypes) do aelt
        celt <: Real && aelt <: Complex && return false
        !(celt <: BigFloat) && aelt <: BigFloat && return false
        !(celt <: BigInt) && aelt <: BigInt && return false
        celt <: IntegerOrC && aelt <: FloatOrC && return false
        if celt <: IntegerOrC && !(celt <: BigInt)
            typemin(celt) > typemin(aelt) && return false
            typemax(celt) < typemax(aelt) && return false
        end
        return true
    end
end
# Note: using `randsubseq` instead of `rand` to avoid repetition.

function inputmattypes(cmat, mattypes = mattypes)
    # Skip if destination type is "too small"
    cmat <: Union{Bidiagonal, Tridiagonal, SymTridiagonal,
                  UnitLowerTriangular, UnitUpperTriangular,
                  Hermitian, Symmetric} && return []
    filter(mattypes) do amat
        cmat <: Diagonal && (amat <: Diagonal || return false)
        cmat <: LowerTriangular && (amat <: LTri || return false)
        cmat <: UpperTriangular && (amat <: UTri || return false)
        return true
    end
end

n_samples = 1.5
# n_samples = Inf  # to try all combinations
for cmat in mattypes,
    amat in sample(inputmattypes(cmat), n_samples),
    bmat in sample(inputmattypes(cmat), n_samples),
    celt in celtypes,
    aelt in sample(inputeltypes(celt), n_samples),
    belt in sample(inputeltypes(celt), n_samples)

    push!(testdata, (cmat{celt}, amat{aelt}, bmat{belt}))
end

@testset "mul!(::$TC, ::$TA, ::$TB, α, β)" for (TC, TA, TB) in testdata
    if needsquare(TA)
        na1 = na2 = rand(sizecandidates)
    else
        na1, na2 = rand(sizecandidates, 2)
    end
    if needsquare(TB)
        nb2 = na2
    elseif needsquare(TC)
        nb2 = na1
    else
        nb2 = rand(sizecandidates)
    end
    asize = (na1, na2)
    bsize = (na2, nb2)
    csize = (na1, nb2)

    @testset for α in Any[true, eltype(TC)(1), _rand(eltype(TC))],
                 β in Any[false, eltype(TC)(0), _rand(eltype(TC))]

        C = _rand(TC, csize)
        A = _rand(TA, asize)
        B = _rand(TB, bsize)

        # This is similar to how `isapprox` choose `rtol` (when
        # `atol=0`) but consider all number types involved:
        rtol = max(rtoldefault.(real.(eltype.((C, A, B))))...,
                   rtoldefault.(real.(typeof.((α, β))))...)

        Cc = copy(C)
        Ac = Matrix(A)
        Bc = Matrix(B)
        returned_mat = mul!(C, A, B, α, β)
        @test returned_mat === C
        @test collect(returned_mat) ≈ α * Ac * Bc + β * Cc  rtol=rtol

        y = C[:, 1]
        x = B[:, 1]
        yc = Vector(y)
        xc = Vector(x)
        returned_vec = mul!(y, A, x, α, β)
        @test returned_vec === y
        @test collect(returned_vec) ≈ α * Ac * xc + β * yc  rtol=rtol

        if TC <: Matrix
            @testset "adjoint and transpose" begin
                @testset for fa in [identity, adjoint, transpose],
                             fb in [identity, adjoint, transpose]
                    fa === fb === identity && continue

                    Af = fa === identity ? A : fa(_rand(TA, reverse(asize)))
                    Bf = fb === identity ? B : fb(_rand(TB, reverse(bsize)))

                    Ac = collect(Af)
                    Bc = collect(Bf)
                    Cc = collect(C)

                    returned_mat = mul!(C, Af, Bf, α, β)
                    @test returned_mat === C
                    @test collect(returned_mat) ≈ α * Ac * Bc + β * Cc  rtol=rtol
                end
            end
        end

        if isnanfillable(C)
            @testset "β = 0 ignores C .= NaN" begin
                parent(C) .= NaN
                Ac = Matrix(A)
                Bc = Matrix(B)
                returned_mat = mul!(C, A, B, α, zero(eltype(C)))
                @test returned_mat === C
                @test collect(returned_mat) ≈ α * Ac * Bc  rtol=rtol
            end
        end

        if isnanfillable(A)
            @testset "α = 0 ignores A .= NaN" begin
                parent(A) .= NaN
                Cc = copy(C)
                returned_mat = mul!(C, A, B, zero(eltype(A)), β)
                @test returned_mat === C
                @test collect(returned_mat) ≈ β * Cc  rtol=rtol
            end
        end
    end
end

end  # module
