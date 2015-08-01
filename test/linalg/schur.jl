# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false
using Base.Test

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
    ε = εa = eps(abs(float(one(eltya))))

debug && println("\ntype of a: ", eltya, "\n")
debug && println("Schur")
    d,v = eig(a)
    f   = schurfact(a)
    @test_approx_eq f[:vectors]*f[:Schur]*f[:vectors]' a
    @test_approx_eq sort(real(f[:values])) sort(real(d))
    @test_approx_eq sort(imag(f[:values])) sort(imag(d))
    @test istriu(f[:Schur]) || eltype(a)<:Real
    @test_approx_eq full(f) a
    @test_throws KeyError f[:A]

debug && println("Reorder Schur")
    # use asym for real schur to enforce tridiag structure
    # avoiding partly selection of conj. eigenvalues
    ordschura = eltya <: Complex ? a : asym
    S = schurfact(ordschura)
    select = bitrand(n)
    O = ordschur(S, select)
    sum(select) != 0 && @test_approx_eq S[:values][find(select)] O[:values][1:sum(select)]
    @test_approx_eq O[:vectors]*O[:Schur]*O[:vectors]' ordschura
    @test_throws KeyError f[:A]

debug && println("Generalized Schur")
    a1_sf = a[1:n1, 1:n1]
    a2_sf = a[n1+1:n2, n1+1:n2]
    f = schurfact(a1_sf, a2_sf)
    @test_approx_eq f[:Q]*f[:S]*f[:Z]' a1_sf
    @test_approx_eq f[:Q]*f[:T]*f[:Z]' a2_sf
    @test istriu(f[:S]) || eltype(a)<:Real
    @test istriu(f[:T]) || eltype(a)<:Real
    @test_throws KeyError f[:A]

debug && println("Reorder Generalized Schur")
    a1_sf = a[1:n1, 1:n1]
    a2_sf = a[n1+1:n2, n1+1:n2]
    NS = schurfact(a1_sf, a2_sf)
    # Currently just testing with selecting gen eig values < 1
    select = abs2(NS[:values]) .< 1
    m = sum(select)
    S = ordschur(NS, select)
    # Make sure that the new factorization stil factors matrix
    @test_approx_eq S[:Q]*S[:S]*S[:Z]' a1_sf
    @test_approx_eq S[:Q]*S[:T]*S[:Z]' a2_sf
    # Make sure that we have sorted it correctly
    @test_approx_eq NS[:values][find(select)] S[:values][1:m]
end
