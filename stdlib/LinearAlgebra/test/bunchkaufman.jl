# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBunchKaufman

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted
using Base: getproperty

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

Random.seed!(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

@testset "$eltya argument A" for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = transpose(a) + a                  # symmetric indefinite
    aher = a' + a                  # Hermitian indefinite
    apd  = a' * a                  # Positive-definite
    for (a, a2, aher, apd) in ((a, a2, aher, apd),
                               (view(a, 1:n, 1:n),
                                view(a2, 1:n, 1:n),
                                view(aher, 1:n, 1:n),
                                view(apd , 1:n, 1:n)))
        ε = εa = eps(abs(float(one(eltya))))

        # check that factorize gives a Bunch-Kaufman
        @test isa(factorize(asym), LinearAlgebra.BunchKaufman)
        @test isa(factorize(aher), LinearAlgebra.BunchKaufman)
        @testset "$uplo Bunch-Kaufman factor of indefinite matrix" for uplo in (:L, :U)
            bc1 = bunchkaufman(Hermitian(aher, uplo))
            @test LinearAlgebra.issuccess(bc1)
            @test logabsdet(bc1)[1] ≈ log(abs(det(bc1)))
            if eltya <: Real
                @test logabsdet(bc1)[2] == sign(det(bc1))
            else
                @test logabsdet(bc1)[2] ≈ sign(det(bc1))
            end
            @test inv(bc1)*aher ≈ Matrix(I, n, n)
            @testset for rook in (false, true)
                @test inv(bunchkaufman(Symmetric(transpose(a) + a, uplo), rook))*(transpose(a) + a) ≈ Matrix(I, n, n)
                if eltya <: BlasFloat
                    # test also bunchkaufman! without explicit type tag
                    # no bunchkaufman! method for Int ... yet
                    @test inv(bunchkaufman!(transpose(a) + a, rook))*(transpose(a) + a) ≈ Matrix(I, n, n)
                end
                @test size(bc1) == size(bc1.LD)
                @test size(bc1, 1) == size(bc1.LD, 1)
                @test size(bc1, 2) == size(bc1.LD, 2)
                if eltya <: BlasReal
                    @test_throws ArgumentError bunchkaufman(a)
                end
                # Test extraction of factors
                if eltya <: Real
                    @test getproperty(bc1, uplo)*bc1.D*getproperty(bc1, uplo)' ≈ aher[bc1.p, bc1.p]
                    @test getproperty(bc1, uplo)*bc1.D*getproperty(bc1, uplo)' ≈ bc1.P*aher*bc1.P'
                end

                bc1 = bunchkaufman(Symmetric(asym, uplo))
                @test getproperty(bc1, uplo)*bc1.D*transpose(getproperty(bc1, uplo)) ≈ asym[bc1.p, bc1.p]
                @test getproperty(bc1, uplo)*bc1.D*transpose(getproperty(bc1, uplo)) ≈ bc1.P*asym*transpose(bc1.P)
                @test_throws ErrorException bc1.Z
                @test_throws ArgumentError uplo == :L ? bc1.U : bc1.L
            end
            # test Base.iterate
            ref_objs = (bc1.D, uplo == :L ? bc1.L : bc1.U, bc1.p)
            for (bki, bkobj) in enumerate(bc1)
                @test bkobj == ref_objs[bki]
            end
            if eltya <: BlasFloat
                @test convert(LinearAlgebra.BunchKaufman{eltya}, bc1) === bc1
                @test convert(LinearAlgebra.Factorization{eltya}, bc1) === bc1
                if eltya <: BlasReal
                    @test convert(LinearAlgebra.Factorization{Float16}, bc1) == convert(LinearAlgebra.BunchKaufman{Float16}, bc1)
                elseif eltya <: BlasComplex
                    @test convert(LinearAlgebra.Factorization{ComplexF16}, bc1) == convert(LinearAlgebra.BunchKaufman{ComplexF16}, bc1)
                end
            end
            @test Base.propertynames(bc1) == (:p, :P, :L, :U, :D)
        end

        @testset "$eltyb argument B" for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
            b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
            for b in (b, view(b, 1:n, 1:2))
                εb = eps(abs(float(one(eltyb))))
                ε = max(εa,εb)

                @testset "$uplo Bunch-Kaufman factor of indefinite matrix" for uplo in (:L, :U)
                    bc1 = bunchkaufman(Hermitian(aher, uplo))
                    @test aher*(bc1\b) ≈ b atol=1000ε
                end

                @testset "$uplo Bunch-Kaufman factors of a pos-def matrix" for uplo in (:U, :L)
                    @testset "rook pivoting: $rook" for rook in (false, true)
                        bc2 = bunchkaufman(Hermitian(apd, uplo), rook)
                        @test LinearAlgebra.issuccess(bc2)
                        bks = split(sprint(show, "text/plain", bc2), "\n")
                        @test bks[1] == summary(bc2)
                        @test bks[2] == "D factor:"
                        @test bks[4+n] == "$uplo factor:"
                        @test bks[6+2n] == "permutation:"
                        @test logdet(bc2) ≈ log(det(bc2))
                        @test logabsdet(bc2)[1] ≈ log(abs(det(bc2)))
                        @test logabsdet(bc2)[2] == sign(det(bc2))
                        @test inv(bc2)*apd ≈ Matrix(I, n, n)
                        @test apd*(bc2\b) ≈ b rtol=eps(cond(apd))
                        @test ishermitian(bc2) == !issymmetric(bc2)
                    end
                end
            end
        end
    end
end

@testset "Singular matrices" begin
    R = Float64[1 0; 0 0]
    C = ComplexF64[1 0; 0 0]
    for A in (R, Symmetric(R), C, Hermitian(C))
        @test_throws SingularException bunchkaufman(A)
        @test_throws SingularException bunchkaufman!(copy(A))
        @test_throws SingularException bunchkaufman(A; check = true)
        @test_throws SingularException bunchkaufman!(copy(A); check = true)
        @test !issuccess(bunchkaufman(A; check = false))
        @test !issuccess(bunchkaufman!(copy(A); check = false))
    end
    F = bunchkaufman(R; check = false)
    @test sprint(show, "text/plain", F) == "Failed factorization of type $(typeof(F))"
end

@testset "test example due to @timholy in PR 15354" begin
    A = rand(6,5); A = complex(A'*A) # to avoid calling the real-lhs-complex-rhs method
    F = cholesky(A);
    v6 = rand(ComplexF64, 6)
    v5 = view(v6, 1:5)
    @test F\v5 == F\v6[1:5]
end

@testset "issue #32080" begin
    A = Symmetric([-5 -9 9; -9 4 1; 9 1 2])
    B = bunchkaufman(A, true)
    @test B.U * B.D * B.U' ≈ A[B.p, B.p]
end

@test_throws DomainError logdet(bunchkaufman([-1 -1; -1 1]))
@test logabsdet(bunchkaufman([8 4; 4 2]; check = false))[1] == -Inf

@testset "0x0 matrix" begin
    for ul in (:U, :L)
        B = bunchkaufman(Symmetric(ones(0, 0), ul))
        @test isa(B, BunchKaufman)
        @test B.D == Tridiagonal([], [], [])
        @test B.P == ones(0, 0)
        @test B.p == []
        if ul == :U
            @test B.U == UnitUpperTriangular(ones(0, 0))
            @test_throws ArgumentError B.L
        else
            @test B.L == UnitLowerTriangular(ones(0, 0))
            @test_throws ArgumentError B.U
        end
    end
end

end # module TestBunchKaufman
