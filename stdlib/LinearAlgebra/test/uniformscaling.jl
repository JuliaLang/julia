# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestUniformscaling

using Test, LinearAlgebra, Random, SparseArrays

Random.seed!(123)

@testset "basic functions" begin
    @test I[1,1] == 1 # getindex
    @test I[1,2] == 0 # getindex
    @test I === I' # transpose
    @test ndims(I) == 2
    @test one(UniformScaling{Float32}) == UniformScaling(one(Float32))
    @test zero(UniformScaling{Float32}) == UniformScaling(zero(Float32))
    @test eltype(one(UniformScaling{Float32})) == Float32
    @test zero(UniformScaling(rand(ComplexF64))) == zero(UniformScaling{ComplexF64})
    @test one(UniformScaling(rand(ComplexF64))) == one(UniformScaling{ComplexF64})
    @test eltype(one(UniformScaling(rand(ComplexF64)))) == ComplexF64
    @test -one(UniformScaling(2)) == UniformScaling(-1)
    @test sparse(3I,4,5) == sparse(1:4, 1:4, 3, 4, 5)
    @test sparse(3I,5,4) == sparse(1:4, 1:4, 3, 5, 4)
    @test opnorm(UniformScaling(1+im)) ≈ sqrt(2)
end

@testset "conjugation of UniformScaling" begin
    @test conj(UniformScaling(1))::UniformScaling{Int} == UniformScaling(1)
    @test conj(UniformScaling(1.0))::UniformScaling{Float64} == UniformScaling(1.0)
    @test conj(UniformScaling(1+1im))::UniformScaling{Complex{Int}} == UniformScaling(1-1im)
    @test conj(UniformScaling(1.0+1.0im))::UniformScaling{Complex{Float64}} == UniformScaling(1.0-1.0im)
end

@testset "isdiag, istriu, istril, issymmetric, ishermitian, isposdef, isapprox" begin
    @test isdiag(I)
    @test istriu(I)
    @test istril(I)
    @test issymmetric(I)
    @test issymmetric(UniformScaling(complex(1.0,1.0)))
    @test ishermitian(I)
    @test !ishermitian(UniformScaling(complex(1.0,1.0)))
    @test isposdef(I)
    @test !isposdef(-I)
    @test isposdef(UniformScaling(complex(1.0, 0.0)))
    @test !isposdef(UniformScaling(complex(1.0, 1.0)))
    @test UniformScaling(4.00000000000001) ≈ UniformScaling(4.0)
    @test UniformScaling(4.32) ≈ UniformScaling(4.3) rtol=0.1 atol=0.01
    @test UniformScaling(4.32) ≈ 4.3 * [1 0; 0 1] rtol=0.1 atol=0.01
    @test UniformScaling(4.32) ≈ 4.3 * [1 0; 0 1] rtol=0.1 atol=0.01 norm=norm
    @test 4.3 * [1 0; 0 1] ≈ UniformScaling(4.32) rtol=0.1 atol=0.01
    @test [4.3201 0.002;0.001 4.32009] ≈ UniformScaling(4.32) rtol=0.1 atol=0.
    @test UniformScaling(4.32) ≉ fill(4.3,2,2) rtol=0.1 atol=0.01
    @test UniformScaling(4.32) ≈ 4.32 * [1 0; 0 1]
end

@testset "arithmetic with Number" begin
    α = randn()
    @test α + I == α + 1
    @test I + α == α + 1
    @test α - I == α - 1
    @test I - α == 1 - α
    @test α .* UniformScaling(1.0) == UniformScaling(1.0) .* α
    @test UniformScaling(α)./α == UniformScaling(1.0)
    @test α * UniformScaling(1.0) == UniformScaling(1.0) * α
    @test UniformScaling(α)/α == UniformScaling(1.0)
end

@testset "det and logdet" begin
    @test det(I) === true
    @test det(1.0I) === 1.0
    @test det(0I) === 0
    @test det(0.0I) === 0.0
    @test logdet(I) == 0
    @test_throws ArgumentError det(2I)
end

@test copy(UniformScaling(one(Float64))) == UniformScaling(one(Float64))
@test sprint(show,MIME"text/plain"(),UniformScaling(one(ComplexF64))) == "LinearAlgebra.UniformScaling{Complex{Float64}}\n(1.0 + 0.0im)*I"
@test sprint(show,MIME"text/plain"(),UniformScaling(one(Float32))) == "LinearAlgebra.UniformScaling{Float32}\n1.0*I"
@test sprint(show,UniformScaling(one(ComplexF64))) == "LinearAlgebra.UniformScaling{Complex{Float64}}(1.0 + 0.0im)"
@test sprint(show,UniformScaling(one(Float32))) == "LinearAlgebra.UniformScaling{Float32}(1.0f0)"

let
    λ = complex(randn(),randn())
    J = UniformScaling(λ)
    @testset "transpose, conj, inv" begin
        @test ndims(J) == 2
        @test transpose(J) == J
        @test J * [1 0; 0 1] == conj(*(adjoint(J), [1 0; 0 1])) # ctranpose (and A(c)_mul_B)
        @test I + I === UniformScaling(2) # +
        @test inv(I) == I
        @test inv(J) == UniformScaling(inv(λ))
        @test cond(I) == 1
        @test cond(J) == (λ ≠ zero(λ) ? one(real(λ)) : oftype(real(λ), Inf))
    end

    @testset "copyto!" begin
        A = Matrix{Int}(undef, (3,3))
        @test copyto!(A, I) == one(A)
        B = Matrix{ComplexF64}(undef, (1,2))
        @test copyto!(B, J) == [λ zero(λ)]
    end

    @testset "binary ops with matrices" begin
        B = bitrand(2, 2)
        @test B + I == B + Matrix(I, size(B))
        @test I + B == B + Matrix(I, size(B))
        AA = randn(2, 2)
        for SS in (sprandn(3,3, 0.5), sparse(Int(1)I, 3, 3))
            for (A, S) in ((AA, SS), (view(AA, 1:2, 1:2), view(SS, 1:3, 1:3)))
                I22 = Matrix(I, size(A))
                @test @inferred(A + I) == A + I22
                @test @inferred(I + A) == A + I22
                @test @inferred(I - I) === UniformScaling(0)
                @test @inferred(B - I) == B - I22
                @test @inferred(I - B) == I22 - B
                @test @inferred(A - I) == A - I22
                @test @inferred(I - A) == I22 - A
                @test @inferred(I*J) === UniformScaling(λ)
                @test @inferred(B*J) == B*λ
                @test @inferred(J*B) == B*λ
                @test @inferred(I*A) !== A # Don't alias
                @test @inferred(I*S) !== S # Don't alias
                @test @inferred(A*I) !== A # Don't alias
                @test @inferred(S*I) !== S # Don't alias

                @test @inferred(S*J) == S*λ
                @test @inferred(J*S) == S*λ
                @test @inferred(A*J) == A*λ
                @test @inferred(J*A) == A*λ
                @test @inferred(J*fill(1, 3)) == fill(λ, 3)
                @test @inferred(λ*J) === UniformScaling(λ*J.λ)
                @test @inferred(J*λ) === UniformScaling(λ*J.λ)
                @test @inferred(J/I) === J
                @test @inferred(I/A) == inv(A)
                @test @inferred(A/I) == A
                @test @inferred(I/λ) === UniformScaling(1/λ)
                @test @inferred(I\J) === J

                if isa(A, Array)
                    T = LowerTriangular(randn(3,3))
                else
                    T = LowerTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == Array(T) + J
                @test @inferred(J + T) == J + Array(T)
                @test @inferred(T - J) == Array(T) - J
                @test @inferred(J - T) == J - Array(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = LinearAlgebra.UnitLowerTriangular(randn(3,3))
                else
                    T = LinearAlgebra.UnitLowerTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == Array(T) + J
                @test @inferred(J + T) == J + Array(T)
                @test @inferred(T - J) == Array(T) - J
                @test @inferred(J - T) == J - Array(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = UpperTriangular(randn(3,3))
                else
                    T = UpperTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == Array(T) + J
                @test @inferred(J + T) == J + Array(T)
                @test @inferred(T - J) == Array(T) - J
                @test @inferred(J - T) == J - Array(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = LinearAlgebra.UnitUpperTriangular(randn(3,3))
                else
                    T = LinearAlgebra.UnitUpperTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == Array(T) + J
                @test @inferred(J + T) == J + Array(T)
                @test @inferred(T - J) == Array(T) - J
                @test @inferred(J - T) == J - Array(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = Hermitian(randn(3,3))
                else
                    T = Hermitian(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == Array(T) + J
                @test @inferred(J + T) == J + Array(T)
                @test @inferred(T - J) == Array(T) - J
                @test @inferred(J - T) == J - Array(T)

                @test @inferred(I\A) == A
                @test @inferred(A\I) == inv(A)
                @test @inferred(λ\I) === UniformScaling(1/λ)
            end
        end
    end
end

@testset "hcat and vcat" begin
    @test_throws ArgumentError hcat(I)
    @test_throws ArgumentError [I I]
    @test_throws ArgumentError vcat(I)
    @test_throws ArgumentError [I; I]
    @test_throws ArgumentError [I I; I]
    for T in (Matrix, SparseMatrixCSC)
        A = T(rand(3,4))
        B = T(rand(3,3))
        C = T(rand(0,3))
        D = T(rand(2,0))
        @test (hcat(A, 2I))::T == hcat(A, Matrix(2I, 3, 3))
        @test (vcat(A, 2I))::T == vcat(A, Matrix(2I, 4, 4))
        @test (hcat(C, 2I))::T == C
        @test (vcat(D, 2I))::T == D
        @test (hcat(I, 3I, A, 2I))::T == hcat(Matrix(I, 3, 3), Matrix(3I, 3, 3), A, Matrix(2I, 3, 3))
        @test (vcat(I, 3I, A, 2I))::T == vcat(Matrix(I, 4, 4), Matrix(3I, 4, 4), A, Matrix(2I, 4, 4))
        @test (hvcat((2,1,2), B, 2I, I, 3I, 4I))::T ==
            hvcat((2,1,2), B, Matrix(2I, 3, 3), Matrix(I, 6, 6), Matrix(3I, 3, 3), Matrix(4I, 3, 3))
        @test hvcat((3,1), C, C, I, 3I)::T == hvcat((2,1), C, C, Matrix(3I, 6,6))
        @test hvcat((2,2,2), I, 2I, 3I, 4I, C, C)::T ==
            hvcat((2,2,2), Matrix(I, 3, 3), Matrix(2I, 3,3 ), Matrix(3I, 3,3), Matrix(4I, 3,3), C, C)
        @test hvcat((2,2,4), C, C, I, 2I, 3I, 4I, 5I, D)::T ==
            hvcat((2,2,4), C, C, Matrix(I, 3, 3), Matrix(2I,3,3),
                Matrix(3I, 2, 2), Matrix(4I, 2, 2), Matrix(5I,2,2), D)
        @test (hvcat((2,3,2), B, 2I, C, C, I, 3I, 4I))::T ==
            hvcat((2,2,2), B, Matrix(2I, 3, 3), C, C, Matrix(3I, 3, 3), Matrix(4I, 3, 3))
        @test hvcat((3,2,1), C, C, I, B ,3I, 2I)::T ==
            hvcat((2,2,1), C, C, B, Matrix(3I,3,3), Matrix(2I,6,6))
    end
end

@testset "Matrix/Array construction from UniformScaling" begin
    I2_33 = [2 0 0; 0 2 0; 0 0 2]
    I2_34 = [2 0 0 0; 0 2 0 0; 0 0 2 0]
    I2_43 = [2 0 0; 0 2 0; 0 0 2; 0 0 0]
    for ArrType in (Matrix, Array)
        @test ArrType(2I, 3, 3)::Matrix{Int} == I2_33
        @test ArrType(2I, 3, 4)::Matrix{Int} == I2_34
        @test ArrType(2I, 4, 3)::Matrix{Int} == I2_43
        @test ArrType(2.0I, 3, 3)::Matrix{Float64} == I2_33
        @test ArrType{Real}(2I, 3, 3)::Matrix{Real} == I2_33
        @test ArrType{Float64}(2I, 3, 3)::Matrix{Float64} == I2_33
    end
end

@testset "Diagonal construction from UniformScaling" begin
    @test Diagonal(2I, 3)::Diagonal{Int} == Matrix(2I, 3, 3)
    @test Diagonal(2.0I, 3)::Diagonal{Float64} == Matrix(2I, 3, 3)
    @test Diagonal{Real}(2I, 3)::Diagonal{Real} == Matrix(2I, 3, 3)
    @test Diagonal{Float64}(2I, 3)::Diagonal{Float64} == Matrix(2I, 3, 3)
end

@testset "equality comparison of matrices with UniformScaling" begin
    # AbstractMatrix methods
    diagI = Diagonal(fill(1, 3))
    rdiagI = view(diagI, 1:2, 1:3)
    bidiag = Bidiagonal(fill(2, 3), fill(2, 2), :U)
    @test diagI  ==  I == diagI  # test isone(I) path / equality
    @test 2diagI !=  I != 2diagI # test isone(I) path / inequality
    @test 0diagI == 0I == 0diagI # test iszero(I) path / equality
    @test 2diagI != 0I != 2diagI # test iszero(I) path / inequality
    @test 2diagI == 2I == 2diagI # test generic path / equality
    @test 0diagI != 2I != 0diagI # test generic path / inequality on diag
    @test bidiag != 2I != bidiag # test generic path / inequality off diag
    @test rdiagI !=  I != rdiagI # test square matrix check
    # StridedMatrix specialization
    denseI = [1 0 0; 0 1 0; 0 0 1]
    rdenseI = [1 0 0 0; 0 1 0 0; 0 0 1 0]
    alltwos = fill(2, (3, 3))
    @test denseI  ==  I == denseI  # test isone(I) path / equality
    @test 2denseI !=  I != 2denseI # test isone(I) path / inequality
    @test 0denseI == 0I == 0denseI # test iszero(I) path / equality
    @test 2denseI != 0I != 2denseI # test iszero(I) path / inequality
    @test 2denseI == 2I == 2denseI # test generic path / equality
    @test 0denseI != 2I != 0denseI # test generic path / inequality on diag
    @test alltwos != 2I != alltwos # test generic path / inequality off diag
    @test rdenseI !=  I != rdenseI # test square matrix check
end

@testset "operations involving I should preserve eltype" begin
    @test isa(Int8(1) + I, Int8)
    @test isa(Float16(1) + I, Float16)
    @test eltype(Int8(1)I) == Int8
    @test eltype(Float16(1)I) == Float16
    @test eltype(fill(Int8(1), 2, 2)I) == Int8
    @test eltype(fill(Float16(1), 2, 2)I) == Float16
    @test eltype(fill(Int8(1), 2, 2) + I) == Int8
    @test eltype(fill(Float16(1), 2, 2) + I) == Float16
end

@testset "test that UniformScaling is applied correctly for matrices of matrices" begin
    LL = Bidiagonal(fill(0*I, 3), fill(1*I, 2), :L)
    @test (I - LL')\[[0], [0], [1]] == (I - LL)'\[[0], [0], [1]] == fill([1], 3)
end

# Ensure broadcasting of I is an error (could be made to work in the future)
@testset "broadcasting of I (#23197)" begin
    @test_throws MethodError I .+ 1
    @test_throws MethodError I .+ [1 1; 1 1]
end

@testset "in-place mul! and div! methods" begin
    J = randn()*I
    A = randn(4, 3)
    C = similar(A)
    target_mul = J * A
    target_div = A / J
    @test mul!(C, J, A) == target_mul
    @test mul!(C, A, J) == target_mul
    @test lmul!(J, copyto!(C, A)) == target_mul
    @test rmul!(copyto!(C, A), J) == target_mul
    @test ldiv!(J, copyto!(C, A)) == target_div
    @test rdiv!(copyto!(C, A), J) == target_div
end

@testset "Construct Diagonal from UniformScaling" begin
    @test size(I(3)) === (3,3)
    @test I(3) isa Diagonal
    @test I(3) == [1 0 0; 0 1 0; 0 0 1]
end

end # module TestUniformscaling
