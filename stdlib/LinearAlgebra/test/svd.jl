# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSVD

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted

@testset "Simple svdvals / svd tests" begin
    ≊(x,y) = isapprox(x,y,rtol=1e-15)

    m1 = [2 0; 0 0]
    m2 = [2 -2; 1 1]/sqrt(2)
    m2c = Complex.([2 -2; 1 1]/sqrt(2))
    @test @inferred(svdvals(m1))  ≊ [2, 0]
    @test @inferred(svdvals(m2))  ≊ [2, 1]
    @test @inferred(svdvals(m2c)) ≊ [2, 1]

    sf1 = svd(m1)
    sf2 = svd(m2)
    @test sf1.S ≊ [2, 0]
    @test sf2.S ≊ [2, 1]
    # U & Vt are unitary
    I22 = Matrix(I, 2, 2)
    @test sf1.U*sf1.U'   ≊ I22
    @test sf1.Vt*sf1.Vt' ≊ I22
    @test sf2.U*sf2.U'   ≊ I22
    @test sf2.Vt*sf2.Vt' ≊ I22
    # SVD not uniquely determined, so just test we can reconstruct the
    # matrices from the factorization as expected.
    @test sf1.U*Diagonal(sf1.S)*sf1.Vt' ≊ m1
    @test sf2.U*Diagonal(sf2.S)*sf2.Vt' ≊ m2

    @test ldiv!([0., 0.], svd(Matrix(I, 2, 2)), [1., 1.]) ≊ [1., 1.]
    @test inv(svd(Matrix(I, 2, 2))) ≈ I
    @test inv(svd([1 2; 3 4])) ≈ [-2.0 1.0; 1.5 -0.5]
    @test inv(svd([1 0 1; 0 1 0])) ≈ [0.5 0.0; 0.0 1.0; 0.5 0.0]
    @test_throws SingularException inv(svd([0 0; 0 0]))
    @test inv(svd([1+2im 3+4im; 5+6im 7+8im])) ≈ [-0.5 + 0.4375im 0.25 - 0.1875im; 0.375 - 0.3125im -0.125 + 0.0625im]
end

n = 10

Random.seed!(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
    aa = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    asym = aa' + aa                 # symmetric indefinite
    for a in (aa, view(aa, 1:n, 1:n))
        usv = svd(a)
        @testset "singular value decomposition" begin
            @test usv.S === svdvals(usv)
            @test usv.U * (Diagonal(usv.S) * usv.Vt) ≈ a
            @test convert(Array, usv) ≈ a
            @test usv.Vt' ≈ usv.V
            @test_throws ErrorException usv.Z
            b = rand(eltya,n)
            @test usv\b ≈ a\b
            @test Base.propertynames(usv) == (:U, :S, :V, :Vt)
            @test size(usv) == size(a)
            if eltya <: BlasFloat
                svdz = svd!(Matrix{eltya}(undef,0,0))
                @test svdz.U ≈ Matrix{eltya}(I, 0, 0)
                @test svdz.S ≈ real(zeros(eltya,0))
                @test svdz.Vt ≈ Matrix{eltya}(I, 0, 0)
            end
        end
        @testset "singular value decomposition of adjoint/transpose" begin
            for transform in (adjoint, transpose)
                usv = svd(transform(a))
                @test usv.S === svdvals(usv)
                @test usv.U * (Diagonal(usv.S) * usv.Vt) ≈ transform(a)
                @test convert(Array, usv) ≈ transform(a)
                @test usv.Vt' ≈ usv.V
                @test_throws ErrorException usv.Z
                b = rand(eltya,n)
                @test usv\b ≈ transform(a)\b
            end
        end
        @testset "Generalized svd" begin
            a_svd = a[1:div(n, 2), :]
            gsvd = svd(a,a_svd)
            @test Base.propertynames(gsvd) == (:alpha, :beta, :vals, :S, :D1, :D2, :R0, :U, :V, :Q, :a, :b, :k, :l, :R)
            @test gsvd.U*gsvd.D1*gsvd.R*gsvd.Q' ≈ a
            @test gsvd.V*gsvd.D2*gsvd.R*gsvd.Q' ≈ a_svd
            @test usv.Vt' ≈ usv.V
            @test_throws ErrorException usv.Z
            @test_throws ErrorException gsvd.Z
            @test gsvd.vals ≈ svdvals(a,a_svd)
            α = eltya == Int ? -1 : rand(eltya)
            β = svd(α)
            @test β.S == [abs(α)]
            @test svdvals(α) == abs(α)
            u,v,q,d1,d2,r0 = svd(a,a_svd)
            @test u ≈ gsvd.U
            @test v ≈ gsvd.V
            @test d1 ≈ gsvd.D1
            @test d2 ≈ gsvd.D2
            @test q ≈ gsvd.Q
            @test gsvd.a.^2 + gsvd.b.^2 ≈ fill(1, length(gsvd.a))
            @test gsvd.alpha.^2 + gsvd.beta.^2 ≈ ones(eltya, length(gsvd.a))
            #testing the other layout for D1 & D2
            b = rand(eltya,n,2*n)
            c = rand(eltya,n,2*n)
            gsvd = svd(b,c)
            @test gsvd.U*gsvd.D1*gsvd.R*gsvd.Q' ≈ b
            @test gsvd.V*gsvd.D2*gsvd.R*gsvd.Q' ≈ c
        end
    end
    @testset "singular value decomposition of Hermitian/real-Symmetric" begin
        for T in (eltya <: Real ? (Symmetric, Hermitian) : (Hermitian,))
            usv = svd(T(asym))
            @test usv.S === svdvals(usv)
            @test usv.U * (Diagonal(usv.S) * usv.Vt) ≈ T(asym)
            @test convert(Array, usv) ≈ T(asym)
            @test usv.Vt' ≈ usv.V
            @test_throws ErrorException usv.Z
            b = rand(eltya,n)
            @test usv\b ≈ T(asym)\b
        end
    end
    if eltya <: LinearAlgebra.BlasReal
        @testset "Number input" begin
            x, y = randn(eltya, 2)
            @test svd(x)    == svd(fill(x, 1, 1))
            @test svdvals(x)    == first(svdvals(fill(x, 1, 1)))
            @test svd(x, y) == svd(fill(x, 1, 1), fill(y, 1, 1))
            @test svdvals(x, y) ≈  first(svdvals(fill(x, 1, 1), fill(y, 1, 1)))
        end
    end
    if eltya != Int
        @testset "isequal, ==, and hash" begin
            x, y   = rand(eltya), convert(eltya, NaN)
            Fx, Fy = svd(x), svd(y)
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



@testset "SVD Algorithms" begin
    ≊(x,y) = isapprox(x,y,rtol=1e-15)

    x = [0.1 0.2; 0.3 0.4]

    for alg in [LinearAlgebra.QRIteration(), LinearAlgebra.DivideAndConquer()]
        sx1 = svd(x, alg = alg)
        @test sx1.U * Diagonal(sx1.S) * sx1.Vt ≊ x
        @test sx1.V * sx1.Vt ≊ I
        @test sx1.U * sx1.U' ≊ I
        @test all(sx1.S .≥ 0)

        sx2 = svd!(copy(x), alg = alg)
        @test sx2.U * Diagonal(sx2.S) * sx2.Vt ≊ x
        @test sx2.V * sx2.Vt ≊ I
        @test sx2.U * sx2.U' ≊ I
        @test all(sx2.S .≥ 0)
    end
end

@testset "REPL printing of SVD" begin
    svdd = svd(randn(3, 3))
    svdstring = sprint((t, s) -> show(t, "text/plain", s), svdd)
    ustring = sprint((t, s) -> show(t, "text/plain", s), svdd.U)
    sstring = sprint((t, s) -> show(t, "text/plain", s), svdd.S)
    vtstring = sprint((t, s) -> show(t, "text/plain", s), svdd.Vt)
    @test svdstring == "$(summary(svdd))\nU factor:\n$ustring\nsingular values:\n$sstring\nVt factor:\n$vtstring"
end

@testset "REPL printing of Generalized SVD" begin
    a = randn(3, 3)
    b = randn(3, 3)
    svdd = svd(a, b)
    svdstring = sprint((t, s) -> show(t, "text/plain", s), svdd)
    ustring = sprint((t, s) -> show(t, "text/plain", s), svdd.U)
    qstring = sprint((t, s) -> show(t, "text/plain", s), svdd.Q)
    vstring = sprint((t, s) -> show(t, "text/plain", s), svdd.V)
    d1string = sprint((t, s) -> show(t, "text/plain", s), svdd.D1)
    d2string = sprint((t, s) -> show(t, "text/plain", s), svdd.D2)
    r0string = sprint((t, s) -> show(t, "text/plain", s), svdd.R0)
    @test svdstring == "$(summary(svdd))\nU factor:\n$ustring\nV factor:\n$vstring\nQ factor:\n$qstring\nD1 factor:\n$d1string\nD2 factor:\n$d2string\nR0 factor:\n$r0string"
end

@testset "c-tor with varying input eltypes" begin
    A = randn(Float64, 10, 10)
    U, S, V = svd(A)
    Ut = convert.(Float16, U)
    Vt = convert.(Float32, V)
    svdc = SVD{ComplexF32}(Ut, S, Vt)
    @test svdc isa SVD{ComplexF32}
    Uc, Sc, Vc = svdc
    @test Uc * diagm(0=>Sc) * transpose(V) ≈ complex.(A) rtol=1e-3
end

end # module TestSVD
