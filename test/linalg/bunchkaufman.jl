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
breal = randn(n,2)/2
bimg  = randn(n,2)/2

for eltya in (Float32, Float64, Complex64, Complex128, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")

debug && println("(Automatic) Bunch-Kaufman factor of indefinite matrix")
        bc1 = factorize(asym)
        @test_approx_eq inv(bc1) * asym eye(n)
        @test_approx_eq_eps asym * (bc1\b) b 1000ε
        @test_approx_eq inv(bkfact(a.' + a)) * (a.' + a) eye(n)
        @test size(bc1) == size(bc1.LD)
        @test size(bc1,1) == size(bc1.LD,1)
        @test size(bc1,2) == size(bc1.LD,2)
        if eltya <: BlasReal
            @test_throws ArgumentError bkfact(a)
        end
debug && println("Bunch-Kaufman factors of a pos-def matrix")
        bc2 = bkfact(apd)
        @test_approx_eq inv(bc2) * apd eye(n)
        @test_approx_eq_eps apd * (bc2\b) b 150000ε
        @test ishermitian(bc2) == !issym(bc2)

    end
end
