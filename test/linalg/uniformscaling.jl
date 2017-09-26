# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

srand(123)

@testset "basic functions" begin
    @test I[1,1] == 1 # getindex
    @test I[1,2] == 0 # getindex
    @test I === I' # transpose
    @test ndims(I) == 2
    @test one(UniformScaling{Float32}) == UniformScaling(one(Float32))
    @test zero(UniformScaling{Float32}) == UniformScaling(zero(Float32))
    @test eltype(one(UniformScaling{Float32})) == Float32
    @test zero(UniformScaling(rand(Complex128))) == zero(UniformScaling{Complex128})
    @test one(UniformScaling(rand(Complex128))) == one(UniformScaling{Complex128})
    @test eltype(one(UniformScaling(rand(Complex128)))) == Complex128
    @test -one(UniformScaling(2)) == UniformScaling(-1)
    @test sparse(3I,4,5) == spdiagm(fill(3,4),0,4,5)
    @test sparse(3I,5,4) == spdiagm(fill(3,4),0,5,4)
    @test norm(UniformScaling(1+im)) ≈ sqrt(2)
end

@testset "istriu, istril, issymmetric, ishermitian, isapprox" begin
    @test istriu(I)
    @test istril(I)
    @test issymmetric(I)
    @test issymmetric(UniformScaling(complex(1.0,1.0)))
    @test ishermitian(I)
    @test !ishermitian(UniformScaling(complex(1.0,1.0)))
    @test UniformScaling(4.00000000000001) ≈ UniformScaling(4.0)
    @test UniformScaling(4.32) ≈ UniformScaling(4.3) rtol=0.1 atol=0.01
    @test UniformScaling(4.32) ≈ 4.3*eye(2) rtol=0.1 atol=0.01
    @test UniformScaling(4.32) ≈ 4.3*eye(2) rtol=0.1 atol=0.01 norm=norm
    @test 4.3*eye(2) ≈ UniformScaling(4.32) rtol=0.1 atol=0.01
    @test [4.3201 0.002;0.001 4.32009] ≈ UniformScaling(4.32) rtol=0.1 atol=0.
    @test UniformScaling(4.32) ≉ 4.3*ones(2,2) rtol=0.1 atol=0.01
end

@testset "* and / with number" begin
    α = randn()
    @test α .* UniformScaling(1.0) == UniformScaling(1.0) .* α
    @test UniformScaling(α)./α == UniformScaling(1.0)
end

@testset "det and logdet" begin
    @test det(I) === 1
    @test det(1.0I) === 1.0
    @test det(0I) === 0
    @test det(0.0I) === 0.0
    @test logdet(I) == 0
    @test_throws ArgumentError det(2I)
end

@test copy(UniformScaling(one(Float64))) == UniformScaling(one(Float64))
@test sprint(show,UniformScaling(one(Complex128))) == "UniformScaling{Complex{Float64}}\n(1.0 + 0.0im)*I"
@test sprint(show,UniformScaling(one(Float32))) == "UniformScaling{Float32}\n1.0*I"

let
    λ = complex(randn(),randn())
    J = UniformScaling(λ)
    @testset "transpose, conj, inv" begin
        @test ndims(J) == 2
        @test transpose(J) == J
        @test J*eye(2) == conj(J'eye(2)) # ctranpose (and A(c)_mul_B)
        @test I + I === UniformScaling(2) # +
        @test inv(I) == I
        @test inv(J) == UniformScaling(inv(λ))
        @test cond(I) == 1
        @test cond(J) == (λ ≠ zero(λ) ? one(real(λ)) : oftype(real(λ), Inf))
    end

    @testset "binary ops with matrices" begin
        B = bitrand(2, 2)
        @test B + I == B + eye(B)
        @test I + B == B + eye(B)
        AA = randn(2, 2)
        for SS in (sprandn(3,3, 0.5), speye(Int, 3))
            for (A, S) in ((AA, SS), (view(AA, 1:2, 1:2), view(SS, 1:3, 1:3)))
                @test @inferred(A + I) == A + eye(A)
                @test @inferred(I + A) == A + eye(A)
                @test @inferred(I - I) === UniformScaling(0)
                @test @inferred(B - I) == B - eye(B)
                @test @inferred(I - B) == eye(B) - B
                @test @inferred(A - I) == A - eye(A)
                @test @inferred(I - A) == eye(A) - A
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
                @test @inferred(J*ones(3)) == ones(3)*λ
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
                @test @inferred(T + J) == full(T) + J
                @test @inferred(J + T) == J + full(T)
                @test @inferred(T - J) == full(T) - J
                @test @inferred(J - T) == J - full(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = LinAlg.UnitLowerTriangular(randn(3,3))
                else
                    T = LinAlg.UnitLowerTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == full(T) + J
                @test @inferred(J + T) == J + full(T)
                @test @inferred(T - J) == full(T) - J
                @test @inferred(J - T) == J - full(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = UpperTriangular(randn(3,3))
                else
                    T = UpperTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == full(T) + J
                @test @inferred(J + T) == J + full(T)
                @test @inferred(T - J) == full(T) - J
                @test @inferred(J - T) == J - full(T)
                @test @inferred(T\I) == inv(T)

                if isa(A, Array)
                    T = LinAlg.UnitUpperTriangular(randn(3,3))
                else
                    T = LinAlg.UnitUpperTriangular(view(randn(3,3), 1:3, 1:3))
                end
                @test @inferred(T + J) == full(T) + J
                @test @inferred(J + T) == J + full(T)
                @test @inferred(T - J) == full(T) - J
                @test @inferred(J - T) == J - full(T)
                @test @inferred(T\I) == inv(T)

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
        @test (hcat(A,2I))::T == hcat(A,2eye(3,3))
        @test (vcat(A,2I))::T == vcat(A,2eye(4,4))
        @test (hcat(I,3I,A,2I))::T == hcat(eye(3,3),3eye(3,3),A,2eye(3,3))
        @test (vcat(I,3I,A,2I))::T == vcat(eye(4,4),3eye(4,4),A,2eye(4,4))
        @test (hvcat((2,1,2),B,2I,I,3I,4I))::T ==
            hvcat((2,1,2),B,2eye(3,3),eye(6,6),3eye(3,3),4eye(3,3))
    end
end

@testset "chol" begin
    for T in (Float64, Complex64, BigFloat, Int)
        λ = T(4)
        @test chol(λ*I) ≈ √λ*I
        @test_throws LinAlg.PosDefException chol(-λ*I)
    end
end
