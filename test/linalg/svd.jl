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
a2real = randn(n,n)/2
a2img  = randn(n,n)/2

for eltya in (Float32, Float64, Complex64, Complex128, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

debug && println("\ntype of a: ", eltya, "\n")

debug && println("singular value decomposition")
    usv = svdfact(a)
    @test_approx_eq usv[:U]*scale(usv[:S],usv[:Vt]) a
    @test_approx_eq full(usv) a
    @test_approx_eq usv[:Vt]' usv[:V]
    @test_throws KeyError usv[:Z]
    b = rand(eltya,n)
    @test_approx_eq usv\b a\b

debug && println("Generalized svd")
    a_svd = a[1:n1, :]
    gsvd = svdfact(a,a_svd)
    @test_approx_eq gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' a
    @test_approx_eq gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' a_svd
    @test_approx_eq usv[:Vt]' usv[:V]
    @test_throws KeyError usv[:Z]
    α = eltya == Int ? -1 : rand(eltya)
    β = svdfact(α)
    @test β[:S] == [abs(α)]
    @test svdvals(α) == [abs(α)]
end
