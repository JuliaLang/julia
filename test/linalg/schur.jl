# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "schur" begin
debug = false

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2

for eltya in (Float32, Float64, Complex64, Complex128, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    for atype in ("Array", "SubArray")
        if atype == "Array"
            a = a
        else
            a = view(a, 1:n, 1:n)
            asym = view(asym, 1:n, 1:n)
            apd = view(apd, 1:n, 1:n)
        end
        ε = εa = eps(abs(float(one(eltya))))

    debug && println("\ntype of a: ", eltya, "\n")
    debug && println("Schur")
        d,v = eig(a)
        f   = schurfact(a)
        @test f[:vectors]*f[:Schur]*f[:vectors]' ≈ a
        @test sort(real(f[:values])) ≈ sort(real(d))
        @test sort(imag(f[:values])) ≈ sort(imag(d))
        @test istriu(f[:Schur]) || eltype(a)<:Real
        @test full(f) ≈ a
        @test_throws KeyError f[:A]

    debug && println("Reorder Schur")
        # use asym for real schur to enforce tridiag structure
        # avoiding partly selection of conj. eigenvalues
        ordschura = eltya <: Complex ? a : asym
        S = schurfact(ordschura)
        select = bitrand(n)
        O = ordschur(S, select)
        sum(select) != 0 && @test S[:values][find(select)] ≈ O[:values][1:sum(select)]
        @test O[:vectors]*O[:Schur]*O[:vectors]' ≈ ordschura
        @test_throws KeyError f[:A]
        Snew = Base.LinAlg.Schur(S.T, S.Z, S.values)
        SchurNew = ordschur!(copy(Snew), select)
        @test O[:vectors] ≈ SchurNew[:vectors]
        @test O[:Schur] ≈ SchurNew[:Schur]

    debug && println("Generalized Schur")
        if atype == "Array"
            a1_sf = a[1:n1, 1:n1]
            a2_sf = a[n1+1:n2, n1+1:n2]
        else
            a1_sf = view(a, 1:n1, 1:n1)
            a2_sf = view(a, n1+1:n2, n1+1:n2)
        end
        f = schurfact(a1_sf, a2_sf)
        @test f[:Q]*f[:S]*f[:Z]' ≈ a1_sf
        @test f[:Q]*f[:T]*f[:Z]' ≈ a2_sf
        @test istriu(f[:S]) || eltype(a)<:Real
        @test istriu(f[:T]) || eltype(a)<:Real
        @test_throws KeyError f[:A]
    debug && println("Reorder Generalized Schur")
        NS = schurfact(a1_sf, a2_sf)
        # Currently just testing with selecting gen eig values < 1
        select = abs2(NS[:values]) .< 1
        m = sum(select)
        S = ordschur(NS, select)
        # Make sure that the new factorization stil factors matrix
        @test S[:Q]*S[:S]*S[:Z]' ≈ a1_sf
        @test S[:Q]*S[:T]*S[:Z]' ≈ a2_sf
        # Make sure that we have sorted it correctly
        @test NS[:values][find(select)] ≈ S[:values][1:m]

        Snew = Base.LinAlg.GeneralizedSchur(NS.S, NS.T, NS.alpha, NS.beta, NS.Q, NS.Z)
        SchurNew = ordschur!(copy(Snew), select)
        @test S[:Q] ≈ SchurNew[:Q]
        @test S[:S] ≈ SchurNew[:S]
        @test S[:T] ≈ SchurNew[:T]
        @test S[:Z] ≈ SchurNew[:Z]
        @test S[:alpha] ≈ SchurNew[:alpha]
        @test S[:beta] ≈ SchurNew[:beta]
        sS,sT,sQ,sZ = schur(a1_sf,a2_sf)
        @test NS[:Q] ≈ sQ
        @test NS[:T] ≈ sT
        @test NS[:S] ≈ sS
        @test NS[:Z] ≈ sZ
    end
end
end
