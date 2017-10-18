# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

@testset "Simple svdvals / svdfact tests" begin
    ≊(x,y) = isapprox(x,y,rtol=1e-15)

    m1 = [2 0; 0 0]
    m2 = [2 -2; 1 1]/sqrt(2)
    m2c = Complex.([2 -2; 1 1]/sqrt(2))
    @test @inferred(svdvals(m1))  ≊ [2, 0]
    @test @inferred(svdvals(m2))  ≊ [2, 1]
    @test @inferred(svdvals(m2c)) ≊ [2, 1]

    sf1 = svdfact(m1)
    sf2 = svdfact(m2)
    @test sf1.S ≊ [2, 0]
    @test sf2.S ≊ [2, 1]
    # U & Vt are unitary
    @test sf1.U*sf1.U'   ≊ eye(2)
    @test sf1.Vt*sf1.Vt' ≊ eye(2)
    @test sf2.U*sf2.U'   ≊ eye(2)
    @test sf2.Vt*sf2.Vt' ≊ eye(2)
    # SVD not uniquely determined, so just test we can reconstruct the
    # matrices from the factorization as expected.
    @test sf1.U*Diagonal(sf1.S)*sf1.Vt' ≊ m1
    @test sf2.U*Diagonal(sf2.S)*sf2.Vt' ≊ m2
end

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2

@testset for eltya in (Float32, Float64, Complex64, Complex128, Int)
    aa = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    aa2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = aa'+aa                  # symmetric indefinite
    apd  = aa'*aa                 # symmetric positive-definite
    for (a, a2) in ((aa, aa2), (view(aa, 1:n, 1:n), view(aa2, 1:n, 1:n)))
        ε = εa = eps(abs(float(one(eltya))))

        usv = svdfact(a)
        @testset "singular value decomposition" begin
            @test usv[:S] === svdvals(usv)
            @test usv[:U] * (Diagonal(usv[:S]) * usv[:Vt]) ≈ a
            @test AbstractArray(usv) ≈ a
            @test usv[:Vt]' ≈ usv[:V]
            @test_throws KeyError usv[:Z]
            b = rand(eltya,n)
            @test usv\b ≈ a\b

            if eltya <: BlasFloat
                svdz = svdfact!(ones(eltya,0,0))
                @test svdz[:U] ≈ eye(eltya,0,0)
                @test svdz[:S] ≈ real(zeros(eltya,0))
                @test svdz[:Vt] ≈ eye(eltya,0,0)
            end
        end
        @testset "Generalized svd" begin
            a_svd = a[1:n1, :]
            gsvd = svdfact(a,a_svd)
            @test gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' ≈ a
            @test gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' ≈ a_svd
            @test usv[:Vt]' ≈ usv[:V]
            @test_throws KeyError usv[:Z]
            @test_throws KeyError gsvd[:Z]
            @test gsvd[:vals] ≈ svdvals(a,a_svd)
            α = eltya == Int ? -1 : rand(eltya)
            β = svdfact(α)
            @test β[:S] == [abs(α)]
            @test svdvals(α) == abs(α)
            u,v,q,d1,d2,r0 = svd(a,a_svd)
            @test u ≈ gsvd[:U]
            @test v ≈ gsvd[:V]
            @test d1 ≈ gsvd[:D1]
            @test d2 ≈ gsvd[:D2]
            @test q ≈ gsvd[:Q]
            @test gsvd[:a].^2 + gsvd[:b].^2 ≈ ones(eltya,length(gsvd[:a]))

            #testing the other layout for D1 & D2
            b = rand(eltya,n,2*n)
            c = rand(eltya,n,2*n)
            gsvd = svdfact(b,c)
            @test gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' ≈ b
            @test gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' ≈ c
        end
    end
    if eltya <: Base.LinAlg.BlasReal
        @testset "Number input" begin
            x, y = randn(eltya, 2)
            @test svdfact(x)    == svdfact(fill(x, 1, 1))
            @test svdvals(x)    == first(svdvals(fill(x, 1, 1)))
            @test svd(x)        == first.(svd(fill(x, 1, 1)))
            @test svdfact(x, y) == svdfact(fill(x, 1, 1), fill(y, 1, 1))
            @test svdvals(x, y) ≈  first(svdvals(fill(x, 1, 1), fill(y, 1, 1)))
            @test svd(x, y)     == first.(svd(fill(x, 1, 1), fill(y, 1, 1)))
        end
    end
    if eltya != Int
        @testset "isequal, ==, and hash" begin
            x, y   = rand(eltya), convert(eltya, NaN)
            Fx, Fy = svdfact(x), svdfact(y)
            @test   Fx == Fx
            @test !(Fy == Fy)
            @test isequal(Fy, Fy)
            @test hash(Fx)          == hash(Fx)
            @test hash(Fx, UInt(1)) == hash(Fx, UInt(1))
            @test hash(Fy)          == hash(Fy)
            @test hash(Fy, UInt(1)) == hash(Fy, UInt(1))
        end
    end
end
