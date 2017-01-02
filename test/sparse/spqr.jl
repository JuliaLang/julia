# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.SparseArrays.SPQR
using Base.SparseArrays.CHOLMOD

let
m, n = 100, 10
nn = 100

for eltyA in (Float64, Complex{Float64})
    for eltyB in (Int, Float64, Complex{Float64})
        if eltyA <: Real
            A = sparse([1:n; rand(1:m, nn - n)], [1:n; rand(1:n, nn - n)], randn(nn), m, n)
        else
            A = sparse([1:n; rand(1:m, nn - n)], [1:n; rand(1:n, nn - n)], complex.(randn(nn), randn(nn)), m, n)
        end
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

        if eltyA == eltyB # promotions not defined for unexported methods
            @test qrfact(sparse(eye(eltyA, 5)))\ones(eltyA, 5) == ones(5)
            @test_throws ArgumentError SPQR.factorize(SPQR.ORDERING_DEFAULT, SPQR.DEFAULT_TOL, CHOLMOD.Sparse(sparse(eye(eltyA, 5))))
            @test_throws ArgumentError SPQR.Factorization(1, 1, convert(Ptr{SPQR.C_Factorization{eltyA}}, C_NULL))
            F = qrfact(A)
            @test size(F) == (m,n)
            @test size(F, 1) == m
            @test size(F, 2) == n
            @test size(F, 3) == 1
            @test_throws ArgumentError size(F, 0)

            # low level wrappers
            @test_throws DimensionMismatch SPQR.solve(SPQR.RX_EQUALS_B, F, CHOLMOD.Dense(B'))
            @test_throws DimensionMismatch SPQR.solve(SPQR.RTX_EQUALS_B, F, CHOLMOD.Dense(B))
            @test_throws DimensionMismatch SPQR.qmult(SPQR.QX, F, CHOLMOD.Dense(B'))
            @test_throws DimensionMismatch SPQR.qmult(SPQR.XQ, F, CHOLMOD.Dense(B))
            @test A\B ≈ SPQR.backslash(SPQR.ORDERING_DEFAULT, SPQR.DEFAULT_TOL, CHOLMOD.Sparse(A), CHOLMOD.Dense(B))
            @test_throws DimensionMismatch SPQR.backslash(SPQR.ORDERING_DEFAULT, SPQR.DEFAULT_TOL, CHOLMOD.Sparse(A), CHOLMOD.Dense(B[1:m-1,:]))
        end
    end
end

# Issue 14134
F = qrfact(sprandn(10,5,0.5))
b = IOBuffer()
serialize(b, F)
seekstart(b)
@test_throws ArgumentError deserialize(b)\ones(10)

end
