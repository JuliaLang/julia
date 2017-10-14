# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Test givens rotations
@testset for elty in (Float32, Float64, Complex64, Complex128)
    if elty <: Real
        raw_A = convert(Matrix{elty}, randn(10,10))
    else
        raw_A = convert(Matrix{elty}, complex.(randn(10,10),randn(10,10)))
    end
    @testset for A in (raw_A, view(raw_A, 1:10, 1:10))
        Ac = copy(A)
        R = Base.LinAlg.Rotation(Base.LinAlg.Givens{elty}[])
        for j = 1:8
            for i = j+2:10
                G, _ = givens(A, j+1, i, j)
                A_mul_B!(G, A)
                A_mul_Bc!(A, G)
                A_mul_B!(G, R)

                @test A_mul_B!(G,eye(elty,10,10)) == [G[i,j] for i=1:10,j=1:10]

                @testset "transposes" begin
                    @test adjoint(G)*G*eye(10) ≈ eye(elty, 10)
                    @test adjoint(R)*(R*eye(10)) ≈ eye(elty, 10)
                    @test_throws ErrorException transpose(G)
                    @test_throws ErrorException transpose(R)
                end
            end
        end
        @test_throws ArgumentError givens(A, 3, 3, 2)
        @test_throws ArgumentError givens(one(elty),zero(elty),2,2)
        G, _ = givens(one(elty),zero(elty),11,12)
        @test_throws DimensionMismatch A_mul_B!(G, A)
        @test_throws DimensionMismatch A_mul_Bc!(A,G)
        @test abs.(A) ≈ abs.(hessfact(Ac)[:H])
        @test norm(R*eye(elty, 10)) ≈ one(elty)

        G, _ = givens(one(elty),zero(elty),9,10)
        @test adjoint(G*eye(elty,10))*(G*eye(elty,10)) ≈ eye(elty, 10)
        K, _ = givens(zero(elty),one(elty),9,10)
        @test adjoint(K*eye(elty,10))*(K*eye(elty,10)) ≈ eye(elty, 10)

        @testset "Givens * vectors" begin
            if isa(A, Array)
                x = A[:, 1]
            else
                x = view(A, 1:10, 1)
            end
            G, r = givens(x[2], x[4], 2, 4)
            @test (G*x)[2] ≈ r
            @test abs((G*x)[4]) < eps(real(elty))
            @inferred givens(x[2], x[4], 2, 4)

            G, r = givens(x, 2, 4)
            @test (G*x)[2] ≈ r
            @test abs((G*x)[4]) < eps(real(elty))
            @inferred givens(x, 2, 4)

            G, r = givens(x, 4, 2)
            @test (G*x)[4] ≈ r
            @test abs((G*x)[2]) < eps(real(elty))
        end
    end
end
