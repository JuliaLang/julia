# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestQR

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted, rmul!, lmul!

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

# helper functions to unambiguously recover explicit forms of an implicit QR Q
squareQ(Q::LinearAlgebra.AbstractQ) = (sq = size(Q.factors, 1); lmul!(Q, Matrix{eltype(Q)}(I, sq, sq)))
rectangularQ(Q::LinearAlgebra.AbstractQ) = convert(Array, Q)

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
    raw_a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    raw_a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = raw_a' + raw_a                  # symmetric indefinite
    apd  = raw_a' * raw_a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    @testset for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
        raw_b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa, εb)
        tab = promote_type(eltya, eltyb)

        @testset "QR decomposition of a Number" begin
            α = rand(eltyb)
            aα = fill(α, 1, 1)
            @test qr(α).Q * qr(α).R ≈ qr(aα).Q * qr(aα).R
            @test abs(qr(α).Q[1,1]) ≈ one(eltyb)
        end

        for (a, b) in ((raw_a, raw_b),
               (view(raw_a, 1:n-1, 1:n-1), view(raw_b, 1:n-1, 1)))
            a_1 = size(a, 1)
            @testset "QR decomposition (without pivoting)" begin
                qra   = @inferred qr(a)
                @inferred qr(a)
                q, r  = qra.Q, qra.R
                @test_throws ErrorException qra.Z
                @test q'*squareQ(q) ≈ Matrix(I, a_1, a_1)
                @test q*squareQ(q)' ≈ Matrix(I, a_1, a_1)
                @test q'*Matrix(1.0I, a_1, a_1)' ≈ squareQ(q)'
                @test squareQ(q)'q ≈ Matrix(I, a_1, a_1)
                @test Matrix(1.0I, a_1, a_1)'q' ≈ squareQ(q)'
                @test q*r ≈ a
                @test a*(qra\b) ≈ b atol=3000ε
                @test Array(qra) ≈ a
                sq = size(q.factors, 2)
                @test *(Matrix{eltyb}(I, sq, sq), adjoint(q)) * squareQ(q) ≈ Matrix(I, sq, sq) atol=5000ε
                if eltya != Int
                    @test Matrix{eltyb}(I, a_1, a_1)*q ≈ convert(AbstractMatrix{tab}, q)
                    ac = copy(a)
                    @test qr!(a[:, 1:5])\b == qr!(view(ac, :, 1:5))\b
                end
                qrstring = sprint((t, s) -> show(t, "text/plain", s), qra)
                rstring  = sprint((t, s) -> show(t, "text/plain", s), r)
                qstring  = sprint((t, s) -> show(t, "text/plain", s), q)
                @test qrstring == "$(summary(qra))\nQ factor:\n$qstring\nR factor:\n$rstring"
                # iterate
                q, r = qra
                @test q*r ≈ a
                # property names
                @test Base.propertynames(qra)       == (:R, :Q)
            end
            @testset "Thin QR decomposition (without pivoting)" begin
                qra   = @inferred qr(a[:, 1:n1], Val(false))
                @inferred qr(a[:, 1:n1], Val(false))
                q,r   = qra.Q, qra.R
                @test_throws ErrorException qra.Z
                @test q'*squareQ(q) ≈ Matrix(I, a_1, a_1)
                @test q'*rectangularQ(q) ≈ Matrix(I, a_1, n1)
                @test q*r ≈ a[:, 1:n1]
                @test q*b[1:n1] ≈ rectangularQ(q)*b[1:n1] atol=100ε
                @test q*b ≈ squareQ(q)*b atol=100ε
                if eltya != Int
                    @test Array{eltya}(q) ≈ Matrix(q)
                end
                @test_throws DimensionMismatch q*b[1:n1 + 1]
                @test_throws DimensionMismatch b[1:n1 + 1]*q'
                sq = size(q.factors, 2)
                @test *(UpperTriangular(Matrix{eltyb}(I, sq, sq)), adjoint(q))*squareQ(q) ≈ Matrix(I, n1, a_1) atol=5000ε
                if eltya != Int
                    @test Matrix{eltyb}(I, a_1, a_1)*q ≈ convert(AbstractMatrix{tab},q)
                end
                # iterate
                q, r = qra
                @test q*r ≈ a[:, 1:n1]
                # property names
                @test Base.propertynames(qra)       == (:R, :Q)
            end
            @testset "(Automatic) Fat (pivoted) QR decomposition" begin
                @inferred qr(a, Val(true))

                qrpa  = factorize(a[1:n1,:])
                q,r = qrpa.Q, qrpa.R
                @test_throws ErrorException qrpa.Z
                p = qrpa.p
                @test q'*squareQ(q) ≈ Matrix(I, n1, n1)
                @test q*squareQ(q)' ≈ Matrix(I, n1, n1)
                sq = size(q, 2);
                @test (UpperTriangular(Matrix{eltya}(I, sq, sq))*q')*squareQ(q) ≈ Matrix(I, n1, n1)
                @test q*r ≈ (isa(qrpa,QRPivoted) ? a[1:n1,p] : a[1:n1,:])
                @test q*r[:,invperm(p)] ≈ a[1:n1,:]
                @test q*r*transpose(qrpa.P) ≈ a[1:n1,:]
                @test a[1:n1,:]*(qrpa\b[1:n1]) ≈ b[1:n1] atol=5000ε
                @test Array(qrpa) ≈ a[1:5,:]
                if eltya != Int
                    @test Array{eltya}(q) ≈ Matrix(q)
                end
                @test_throws DimensionMismatch q*b[1:n1+1]
                @test_throws DimensionMismatch b[1:n1+1]*q'
                if eltya != Int
                    @test Matrix{eltyb}(I, n1, n1)*q ≈ convert(AbstractMatrix{tab},q)
                end
                # iterate
                q, r, p = qrpa
                @test q*r[:,invperm(p)] ≈ a[1:n1,:]
                # property names
                @test Base.propertynames(qrpa)       == (:R, :Q, :p, :P)
            end
            @testset "(Automatic) Thin (pivoted) QR decomposition" begin
                qrpa  = factorize(a[:,1:n1])
                q,r = qrpa.Q, qrpa.R
                @test_throws ErrorException qrpa.Z
                p = qrpa.p
                @test q'*squareQ(q) ≈ Matrix(I, a_1, a_1)
                @test q*squareQ(q)' ≈ Matrix(I, a_1, a_1)
                @test q*r ≈ a[:,p]
                @test q*r[:,invperm(p)] ≈ a[:,1:n1]
                @test Array(qrpa) ≈ a[:,1:5]
                if eltya != Int
                    @test Array{eltya}(q) ≈ Matrix(q)
                end
                @test_throws DimensionMismatch q*b[1:n1+1]
                @test_throws DimensionMismatch b[1:n1+1]*q'
                sq = size(q.factors, 2)
                @test *(UpperTriangular(Matrix{eltyb}(I, sq, sq)), adjoint(q))*squareQ(q) ≈ Matrix(I, n1, a_1) atol=5000ε
                if eltya != Int
                    @test Matrix{eltyb}(I, a_1, a_1)*q ≈ convert(AbstractMatrix{tab},q)
                end
                qrstring = sprint((t, s) -> show(t, "text/plain", s), qrpa)
                rstring  = sprint((t, s) -> show(t, "text/plain", s), r)
                qstring  = sprint((t, s) -> show(t, "text/plain", s), q)
                pstring  = sprint((t, s) -> show(t, "text/plain", s), p)
                @test qrstring == "$(summary(qrpa))\nQ factor:\n$qstring\nR factor:\n$rstring\npermutation:\n$pstring"
                # iterate
                q, r, p = qrpa
                @test q*r[:,invperm(p)] ≈ a[:,1:n1]
                # property names
                @test Base.propertynames(qrpa)       == (:R, :Q, :p, :P)
            end
        end
        if eltya != Int
            @testset "Matmul with QR factorizations" begin
                a = raw_a
                qrpa = factorize(a[:,1:n1])
                q, r = qrpa.Q, qrpa.R
                @test rmul!(copy(squareQ(q)'), q) ≈ Matrix(I, n, n)
                @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1),q)
                @test rmul!(squareQ(q), adjoint(q)) ≈ Matrix(I, n, n)
                @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1), adjoint(q))
                @test_throws ErrorException size(q,-1)
                @test_throws DimensionMismatch LinearAlgebra.lmul!(q,zeros(eltya,n1+1))
                @test_throws DimensionMismatch LinearAlgebra.lmul!(adjoint(q), zeros(eltya,n1+1))

                b = similar(a); rand!(b)
                c = similar(a)
                @test mul!(c, q, b) ≈ q*b
                @test mul!(c, q', b) ≈ q'*b
                @test mul!(c, b, q) ≈ b*q
                @test mul!(c, b, q') ≈ b*q'
                @test_throws DimensionMismatch mul!(Matrix{eltya}(I, n+1, n), q, b)

                qra = qr(a[:,1:n1], Val(false))
                q, r = qra.Q, qra.R
                @test rmul!(copy(squareQ(q)'), q) ≈ Matrix(I, n, n)
                @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1),q)
                @test rmul!(squareQ(q), adjoint(q)) ≈ Matrix(I, n, n)
                @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1),adjoint(q))
                @test_throws ErrorException size(q,-1)
                @test_throws DimensionMismatch q * Matrix{Int8}(I, n+4, n+4)

                @test mul!(c, q, b) ≈ q*b
                @test mul!(c, q', b) ≈ q'*b
                @test mul!(c, b, q) ≈ b*q
                @test mul!(c, b, q') ≈ b*q'
                @test_throws DimensionMismatch mul!(Matrix{eltya}(I, n+1, n), q, b)
            end
        end
    end
end

@testset "transpose errors" begin
    @test_throws MethodError transpose(qr(randn(3,3)))
    @test_throws MethodError adjoint(qr(randn(3,3)))
    @test_throws MethodError transpose(qr(randn(3,3), Val(false)))
    @test_throws MethodError adjoint(qr(randn(3,3), Val(false)))
    @test_throws MethodError transpose(qr(big.(randn(3,3))))
    @test_throws MethodError adjoint(qr(big.(randn(3,3))))
end

@testset "Issue 7304" begin
    A = [-√.5 -√.5; -√.5 √.5]
    Q = rectangularQ(qr(A).Q)
    @test norm(A-Q) < eps()
end

@testset "qr on AbstractVector" begin
    vr = [3.0, 4.0]
    for Tr in (Float32, Float64)
        for T in (Tr, Complex{Tr})
            v = convert(Vector{T}, vr)
            nv, nm = qr(v)
            @test norm(nv - [-0.6 -0.8; -0.8 0.6], Inf) < eps(Tr)
            @test nm == fill(-5.0, 1, 1)
        end
    end
end

@testset "QR on Ints" begin
    # not sure what to do about this edge case now that we build decompositions
    # for qr(...), so for now just commenting this out
    # @test qr(Int[]) == (Int[],1)

    B = rand(7,2)
    @test (1:7)\B ≈ Vector(1:7)\B
end

@testset "Issue 16520" begin
    @test_throws DimensionMismatch Matrix{Float64}(undef,3,2)\(1:5)
end

@testset "Issue 22810" begin
    A = zeros(1, 2)
    B = zeros(1, 1)
    @test A \ B == zeros(2, 1)
    @test qr(A, Val(true)) \ B == zeros(2, 1)
end

@testset "Issue 24107" begin
    A = rand(200,2)
    @test A \ range(0, stop=1, length=200) == A \ Vector(range(0, stop=1, length=200))
end

@testset "Issue 24589. Promotion of rational matrices" begin
    A = rand(1//1:5//5, 4,3)
    @test first(qr(A)) == first(qr(float(A)))
end

@testset "Issue Test Factorization fallbacks for rectangular problems" begin
    A  = randn(3,2)
    Ac = copy(A')
    b  = randn(3)
    b0 = copy(b)
    c  = randn(2)
    @test A \b ≈ ldiv!(c, qr(A ), b)
    @test b == b0
    c0 = copy(c)
    @test Ac\c ≈ ldiv!(b, qr(Ac, Val(true)), c)
    @test c0 == c
end

@testset "det(Q::Union{QRCompactWYQ, QRPackedQ})" begin
    # 40 is the number larger than the default block size 36 of QRCompactWY
    @testset for n in [1:3; 40], m in [1:3; 40], pivot in [false, true]
        @testset "real" begin
            @testset for k in 0:min(n, m, 5)
                A = cat(Array(I(k)), randn(n - k, m - k); dims=(1, 2))
                Q, = qr(A, Val(pivot))
                @test det(Q) ≈ det(collect(Q))
                @test abs(det(Q)) ≈ 1
            end
        end
        @testset "complex" begin
            @testset for k in 0:min(n, m, 5)
                A = cat(Array(I(k)), randn(ComplexF64, n - k, m - k); dims=(1, 2))
                Q, = qr(A, Val(pivot))
                @test det(Q) ≈ det(collect(Q))
                @test abs(det(Q)) ≈ 1
            end
        end
    end
end

end # module TestQR
