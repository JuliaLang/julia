# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

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

@testset for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    raw_a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    raw_a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = raw_a' + raw_a                  # symmetric indefinite
    apd  = raw_a' * raw_a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    @testset for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        raw_b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa, εb)
        tab = promote_type(eltya, eltyb)

        @testset "QR decomposition of a Number" begin
            α = rand(eltyb)
            aα = fill(α, 1, 1)
            @test qrfact(α)[:Q] * qrfact(α)[:R] ≈ qrfact(aα)[:Q] * qrfact(aα)[:R]
            @test abs(qrfact(α)[:Q][1,1]) ≈ one(eltyb)
        end

        for (a, b) in ((raw_a, raw_b),
               (view(raw_a, 1:n-1, 1:n-1), view(raw_b, 1:n-1, 1)))
            a_1 = size(a, 1)
            @testset "QR decomposition (without pivoting)" begin
                qra   = @inferred qrfact(a)
                @inferred qr(a)
                q, r  = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test q'*full(q, thin=false) ≈ eye(a_1)
                @test q*full(q, thin=false)' ≈ eye(a_1)
                @test q'*eye(a_1)' ≈ full(q, thin=false)'
                @test full(q, thin=false)'q ≈ eye(a_1)
                @test eye(a_1)'q' ≈ full(q, thin=false)'
                @test q*r ≈ a
                @test a*(qra\b) ≈ b atol=3000ε
                @test full(qra) ≈ a
                @test A_mul_Bc(eye(eltyb, size(q.factors, 2)), q) * full(q, thin=false) ≈ eye(size(q.factors, 2)) atol=5000ε
                if eltya != Int
                    @test eye(eltyb, a_1)*q ≈ convert(AbstractMatrix{tab}, q)
                    ac = copy(a)
                    @test qrfact!(a[:, 1:5])\b == qrfact!(view(ac, :, 1:5))\b
                end
                rstring = sprint(show, r)
                qstring = sprint(show, q)
                @test sprint(show, qra) == "$(typeof(qra)) with factors Q and R:\n$qstring\n$rstring"
            end
            @testset "Thin QR decomposition (without pivoting)" begin
                qra   = @inferred qrfact(a[:, 1:n1], Val(false))
                @inferred qr(a[:, 1:n1], Val(false))
                q,r   = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test q'*full(q, thin=false) ≈ eye(a_1)
                @test q'*full(q) ≈ eye(a_1, n1)
                @test q*r ≈ a[:, 1:n1]
                @test q*b[1:n1] ≈ full(q)*b[1:n1] atol=100ε
                @test q*b ≈ full(q, thin=false)*b atol=100ε
                @test_throws DimensionMismatch q*b[1:n1 + 1]
                @test_throws DimensionMismatch b[1:n1 + 1]*q'
                @test A_mul_Bc(UpperTriangular(eye(eltyb, size(q.factors,2))), q)*full(q, thin=false) ≈ eye(n1, a_1) atol=5000ε
                if eltya != Int
                    @test eye(eltyb, a_1)*q ≈ convert(AbstractMatrix{tab},q)
                end
            end
            @testset "(Automatic) Fat (pivoted) QR decomposition" begin
                @inferred qrfact(a, Val(true))
                @inferred qr(a, Val(true))

                qrpa  = factorize(a[1:n1,:])
                q,r = qrpa[:Q], qrpa[:R]
                @test_throws KeyError qrpa[:Z]
                p = qrpa[:p]
                @test q'*full(q, thin=false) ≈ eye(n1)
                @test q*full(q, thin=false)' ≈ eye(n1)
                @test (UpperTriangular(eye(eltya,size(q,2)))*q')*full(q, thin=false) ≈ eye(n1)
                @test q*r ≈ (isa(qrpa,QRPivoted) ? a[1:n1,p] : a[1:n1,:])
                @test q*r[:,invperm(p)] ≈ a[1:n1,:]
                @test q*r*qrpa[:P].' ≈ a[1:n1,:]
                @test a[1:n1,:]*(qrpa\b[1:n1]) ≈ b[1:n1] atol=5000ε
                @test full(qrpa) ≈ a[1:5,:]
                @test_throws DimensionMismatch q*b[1:n1+1]
                @test_throws DimensionMismatch b[1:n1+1]*q'
                if eltya != Int
                    @test eye(eltyb,n1)*q ≈ convert(AbstractMatrix{tab},q)
                end
            end
            @testset "(Automatic) Thin (pivoted) QR decomposition" begin
                qrpa  = factorize(a[:,1:n1])
                q,r = qrpa[:Q], qrpa[:R]
                @test_throws KeyError qrpa[:Z]
                p = qrpa[:p]
                @test q'*full(q, thin=false) ≈ eye(a_1)
                @test q*full(q, thin=false)' ≈ eye(a_1)
                @test q*r ≈ a[:,p]
                @test q*r[:,invperm(p)] ≈ a[:,1:n1]
                @test full(qrpa) ≈ a[:,1:5]
                @test_throws DimensionMismatch q*b[1:n1+1]
                @test_throws DimensionMismatch b[1:n1+1]*q'
                @test A_mul_Bc(UpperTriangular(eye(eltyb, size(q.factors, 2))), q)*full(q, thin=false) ≈ eye(n1, a_1) atol=5000ε
                if eltya != Int
                    @test eye(eltyb, a_1)*q ≈ convert(AbstractMatrix{tab},q)
                end
            end
        end
        if eltya != Int
            @testset "Matmul with QR factorizations" begin
                a = raw_a
                qrpa = factorize(a[:,1:n1])
                q, r = qrpa[:Q], qrpa[:R]
                @test A_mul_B!(full(q, thin=false)',q) ≈ eye(n)
                @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
                @test A_mul_Bc!(full(q, thin=false),q) ≈ eye(n)
                @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
                @test_throws BoundsError size(q,-1)
                @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(q,zeros(eltya,n1+1))
                @test_throws DimensionMismatch Base.LinAlg.Ac_mul_B!(q,zeros(eltya,n1+1))

                qra = qrfact(a[:,1:n1], Val(false))
                q, r = qra[:Q], qra[:R]
                @test A_mul_B!(full(q, thin=false)',q) ≈ eye(n)
                @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
                @test A_mul_Bc!(full(q, thin=false),q) ≈ eye(n)
                @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
                @test_throws BoundsError size(q,-1)
                @test_throws DimensionMismatch q * eye(Int8,n+4)
            end
        end
    end
end

@testset "transpose errors" begin
    @test_throws ErrorException transpose(qrfact(randn(3,3)))
    @test_throws ErrorException adjoint(qrfact(randn(3,3)))
    @test_throws ErrorException transpose(qrfact(randn(3,3), Val(false)))
    @test_throws ErrorException adjoint(qrfact(randn(3,3), Val(false)))
    @test_throws ErrorException transpose(qrfact(big.(randn(3,3))))
    @test_throws ErrorException adjoint(qrfact(big.(randn(3,3))))
end

@testset "Issue 7304" begin
    A = [-√.5 -√.5; -√.5 √.5]
    Q = full(qrfact(A)[:Q])
    @test vecnorm(A-Q) < eps()
end

@testset "qr on AbstractVector" begin
    vr = [3.0, 4.0]
    for Tr in (Float32, Float64)
        for T in (Tr, Complex{Tr})
            v = convert(Vector{T}, vr)
            nv, nm = qr(v)
            @test norm(nv - [0.6, 0.8], Inf) < eps(Tr)
            @test nm == 5.0
        end
    end
end

@testset "QR on Ints" begin
    @test qr(Int[]) == (Int[],1)
    @test Base.LinAlg.qr!(Int[1]) == (Int[1],1)

    B = rand(7,2)
    @test (1:7)\B ≈ collect(1:7)\B
end

@testset "Issue 16520" begin
    @test_throws DimensionMismatch ones(3,2)\(1:5)
end

@testset "Issue 22810" begin
    A = zeros(1, 2)
    B = zeros(1, 1)
    @test A \ B == zeros(2, 1)
    @test qrfact(A, Val(true)) \ B == zeros(2, 1)
end
