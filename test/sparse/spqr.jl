# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.SparseArrays.SPQR
using Base.SparseArrays.CHOLMOD

@testset "Sparse QR" begin
m, n = 100, 10
nn = 100

@test size(qrfact(sprandn(m, n, 0.1))[:Q]) == (m, m)

@testset "element type of A: $eltyA" for eltyA in (Float64, Complex{Float64})
    if eltyA <: Real
        A = sparse([1:n; rand(1:m, nn - n)], [1:n; rand(1:n, nn - n)], randn(nn), m, n)
    else
        A = sparse([1:n; rand(1:m, nn - n)], [1:n; rand(1:n, nn - n)], complex.(randn(nn), randn(nn)), m, n)
    end

    F = qrfact(A)
    @test size(F) == (m,n)
    @test size(F, 1) == m
    @test size(F, 2) == n
    @test size(F, 3) == 1
    @test_throws ArgumentError size(F, 0)

    @testset "getindex" begin
        @test istriu(F[:R])
        @test isperm(F[:pcol])
        @test isperm(F[:prow])
        @test_throws KeyError F[:T]
    end

    @testset "apply Q" begin
        Q = F[:Q]
        @test Q'*(Q*eye(m)) ≈ eye(m)
        @test (eye(m)*Q)*Q' ≈ eye(m)

        # test that Q'Pl*A*Pr = R
        R0 = Q'*full(A[F[:prow], F[:pcol]])
        @test R0[1:n, :] ≈ F[:R]
        @test norm(R0[n + 1:end, :], 1) < 1e-12

        @test_throws DimensionMismatch A_mul_B!(Q, eye(m + 1))
        @test_throws DimensionMismatch Ac_mul_B!(Q, eye(m + 1))
        @test_throws DimensionMismatch A_mul_B!(eye(m + 1), Q)
        @test_throws DimensionMismatch A_mul_Bc!(eye(m + 1), Q)
    end

    @testset "element type of B: $eltyB" for eltyB in (Int, Float64, Complex{Float64})
        if eltyB == Int
            B = rand(1:10, m, 2)
        elseif eltyB <: Real
            B = randn(m, 2)
        else
            B = complex.(randn(m, 2), randn(m, 2))
        end

        @inferred A\B
        @test A\B[:,1] ≈ Array(A)\B[:,1]
        @test A\B ≈ Array(A)\B
        @test_throws DimensionMismatch A\B[1:m-1,:]
        @test A[1:9,:]*(A[1:9,:]\ones(eltyB, 9)) ≈ ones(9) # Underdetermined system
    end

    # Make sure that conversion to Sparse doesn't use SuiteSparse's symmetric flag
    @test qrfact(sparse(eye(eltyA, 5)))\ones(eltyA, 5) == ones(5)
end

@testset "basic solution of rank deficient ls" begin
    A = sprandn(m, 5, 0.9)*sprandn(5, n, 0.9)
    b = randn(m)
    xs = A\b
    xd = full(A)\b

    # check that basic solution has more zeros
    @test countnz(xs) < countnz(xd)
    @test A*xs ≈ A*xd
end

end
