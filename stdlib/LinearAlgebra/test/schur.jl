# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSchur

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

Random.seed!(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    asym = a' + a                 # symmetric indefinite
    apd  = a' * a                 # symmetric positive-definite
    for (a, asym, apd) in ((a, asym, apd),
                           (view(a, 1:n, 1:n),
                            view(asym, 1:n, 1:n),
                            view(apd, 1:n, 1:n)))
        ε = εa = eps(abs(float(one(eltya))))

        d,v = eigen(a)
        f   = schur(a)
        @test f.vectors*f.Schur*f.vectors' ≈ a
        @test sort(real(f.values)) ≈ sort(real(d))
        @test sort(imag(f.values)) ≈ sort(imag(d))
        @test istriu(f.Schur) || eltype(a)<:Real
        @test convert(Array, f) ≈ a
        @test_throws ErrorException f.A

        sch, vecs, vals = schur(UpperTriangular(triu(a)))
        @test vecs*sch*vecs' ≈ triu(a)
        sch, vecs, vals = schur(LowerTriangular(tril(a)))
        @test vecs*sch*vecs' ≈ tril(a)
        sch, vecs, vals = schur(Hermitian(asym))
        @test vecs*sch*vecs' ≈ asym
        sch, vecs, vals = schur(Symmetric(a + transpose(a)))
        @test vecs*sch*vecs' ≈ a + transpose(a)
        sch, vecs, vals = schur(Tridiagonal(a + transpose(a)))
        @test vecs*sch*vecs' ≈ Tridiagonal(a + transpose(a))
        sch, vecs, vals = schur(Bidiagonal(a, :U))
        @test vecs*sch*vecs' ≈ Bidiagonal(a, :U)
        sch, vecs, vals = schur(Bidiagonal(a, :L))
        @test vecs*sch*vecs' ≈ Bidiagonal(a, :L)

        tstring = sprint((t, s) -> show(t, "text/plain", s), f.T)
        zstring = sprint((t, s) -> show(t, "text/plain", s), f.Z)
        vstring = sprint((t, s) -> show(t, "text/plain", s), f.values)
        fstring = sprint((t, s) -> show(t, "text/plain", s), f)
        @test fstring == "$(summary(f))\nT factor:\n$tstring\nZ factor:\n$(zstring)\neigenvalues:\n$vstring"
        @testset "Reorder Schur" begin
            # use asym for real schur to enforce tridiag structure
            # avoiding partly selection of conj. eigenvalues
            ordschura = eltya <: Complex ? a : asym
            S = schur(ordschura)
            select = bitrand(n)
            O = ordschur(S, select)
            sum(select) != 0 && @test S.values[findall(select)] ≈ O.values[1:sum(select)]
            @test O.vectors*O.Schur*O.vectors' ≈ ordschura
            @test_throws ErrorException f.A
            Snew = LinearAlgebra.Schur(S.T, S.Z, S.values)
            SchurNew = ordschur!(copy(Snew), select)
            @test O.vectors ≈ SchurNew.vectors
            @test O.Schur ≈ SchurNew.Schur
        end

        if isa(a, Array)
            a1_sf = a[1:n1, 1:n1]
            a2_sf = a[n1+1:n2, n1+1:n2]
        else
            a1_sf = view(a, 1:n1, 1:n1)
            a2_sf = view(a, n1+1:n2, n1+1:n2)
        end
        @testset "Generalized Schur" begin
            f = schur(a1_sf, a2_sf)
            @test f.Q*f.S*f.Z' ≈ a1_sf
            @test f.Q*f.T*f.Z' ≈ a2_sf
            @test istriu(f.S) || eltype(a)<:Real
            @test istriu(f.T) || eltype(a)<:Real
            @test_throws ErrorException f.A

            sstring = sprint((t, s) -> show(t, "text/plain", s), f.S)
            tstring = sprint((t, s) -> show(t, "text/plain", s), f.T)
            qstring = sprint((t, s) -> show(t, "text/plain", s), f.Q)
            zstring = sprint((t, s) -> show(t, "text/plain", s), f.Z)
            αstring = sprint((t, s) -> show(t, "text/plain", s), f.α)
            βstring = sprint((t, s) -> show(t, "text/plain", s), f.β)
            fstring = sprint((t, s) -> show(t, "text/plain", s), f)
            @test fstring == "$(summary(f))\nS factor:\n$sstring\nT factor:\n$(tstring)\nQ factor:\n$(qstring)\nZ factor:\n$(zstring)\nα:\n$αstring\nβ:\n$βstring"
        end
        @testset "Reorder Generalized Schur" begin
            NS = schur(a1_sf, a2_sf)
            # Currently just testing with selecting gen eig values < 1
            select = abs2.(NS.values) .< 1
            m = sum(select)
            S = ordschur(NS, select)
            # Make sure that the new factorization still factors matrix
            @test S.Q*S.S*S.Z' ≈ a1_sf
            @test S.Q*S.T*S.Z' ≈ a2_sf
            # Make sure that we have sorted it correctly
            @test NS.values[findall(select)] ≈ S.values[1:m]

            Snew = LinearAlgebra.GeneralizedSchur(NS.S, NS.T, NS.alpha, NS.beta, NS.Q, NS.Z)
            SchurNew = ordschur!(copy(Snew), select)
            @test S.Q ≈ SchurNew.Q
            @test S.S ≈ SchurNew.S
            @test S.T ≈ SchurNew.T
            @test S.Z ≈ SchurNew.Z
            @test S.alpha ≈ SchurNew.alpha
            @test S.beta  ≈ SchurNew.beta
            sS,sT,sQ,sZ = schur(a1_sf,a2_sf)
            @test NS.Q ≈ sQ
            @test NS.T ≈ sT
            @test NS.S ≈ sS
            @test NS.Z ≈ sZ
        end
    end
    @testset "0x0 matrix" for A in (zeros(eltya, 0, 0), view(rand(eltya, 2, 2), 1:0, 1:0))
        T, Z, λ = LinearAlgebra.schur(A)
        @test T == A
        @test Z == A
        @test λ == zeros(0)
    end
end

end # module TestSchur
