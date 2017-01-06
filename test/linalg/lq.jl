# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal


n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

@testset for eltya in (Float32, Float64, Complex64, Complex128)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    @testset for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        α = rand(eltya)
        aα = fill(α,1,1)
        @test lqfact(α)[:L]*lqfact(α)[:Q] ≈ lqfact(aα)[:L]*lqfact(aα)[:Q]
        @test lq(α)[1]*lq(α)[2] ≈ lqfact(aα)[:L]*lqfact(aα)[:Q]
        @test abs(lqfact(α)[:Q][1,1]) ≈ one(eltya)
        tab = promote_type(eltya,eltyb)

        for i = 1:2
            let a = i == 1 ? a : view(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : view(b, 1:n - 1), n = i == 1 ? n : n - 1
                lqa   = lqfact(a)
                l,q   = lqa[:L], lqa[:Q]
                qra   = qrfact(a)
                @testset "Basic ops" begin
                    @test size(lqa,1) == size(a,1)
                    @test size(lqa,3) == 1
                    @test size(lqa[:Q],3) == 1
                    @test Base.LinAlg.getq(lqa) == lqa[:Q]
                    @test_throws KeyError lqa[:Z]
                    @test full(lqa') ≈ a'
                    @test lqa * lqa' ≈ a * a'
                    @test lqa' * lqa ≈ a' * a
                    @test q*full(q, thin = false)' ≈ eye(eltya,n)
                    @test l*q ≈ a
                    @test full(lqa) ≈ a
                    @test full(copy(lqa)) ≈ a
                end
                @testset "Binary ops" begin
                    @test a*(lqa\b) ≈ b atol=3000ε
                    @test lqa*b ≈ qra[:Q]*qra[:R]*b atol=3000ε
                    @test A_mul_Bc(eye(eltyb,size(q.factors,2)),q)*full(q,thin=false) ≈ eye(n) atol=5000ε
                    if eltya != Int
                        @test eye(eltyb,n)*q ≈ convert(AbstractMatrix{tab},q)
                    end
                    @test q*b ≈ full(q,thin=false)*b atol=100ε
                    @test q.'*b ≈ full(q,thin=false).'*b atol=100ε
                    @test q'*b ≈ full(q,thin=false)'*b atol=100ε
                    @test a*q ≈ a*full(q,thin=false) atol=100ε
                    @test a*q.' ≈ a*full(q,thin=false).' atol=100ε
                    @test a*q' ≈ a*full(q,thin=false)' atol=100ε
                    @test_throws DimensionMismatch q*b[1:n1 + 1]
                    @test_throws DimensionMismatch Ac_mul_B(q,ones(eltya,n+2,n+2))
                    @test_throws DimensionMismatch ones(eltyb,n+2,n+2)*q
                end
            end
        end

        @testset "Matmul with LQ factorizations" begin
            lqa = lqfact(a[:,1:n1])
            l,q = lqa[:L], lqa[:Q]
            @test full(q)*full(q)' ≈ eye(eltya,n1)
            @test (full(q,thin=false)'*full(q,thin=false))[1:n1,:] ≈ eye(eltya,n1,n)
            @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
            @test Ac_mul_B!(q,full(q)) ≈ eye(eltya,n1)
            @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
            @test_throws BoundsError size(q,-1)
        end
    end
end
