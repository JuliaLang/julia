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

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
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
debug && println("QR decomposition (without pivoting)")
        for i = 1:2
            let a = i == 1 ? a : sub(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : sub(b, 1:n - 1), n = i == 1 ? n : n - 1
                qra   = @inferred qrfact(a)
                @inferred qr(a)
                q, r  = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test_approx_eq q'*full(q, thin = false) eye(n)
                @test_approx_eq q*full(q, thin = false)' eye(n)
                @test_approx_eq q*r a
                @test_approx_eq_eps a*(qra\b) b 3000ε
                @test_approx_eq full(qra) a

debug && println("Thin QR decomposition (without pivoting)")
                qra   = @inferred qrfact(a[:,1:n1], Val{false})
                @inferred qr(a[:,1:n1], Val{false})
                q,r   = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test_approx_eq q'*full(q, thin=false) eye(n)
                @test_approx_eq q'*full(q) eye(n, n1)
                @test_approx_eq q*r a[:,1:n1]
                @test_approx_eq_eps q*b[1:n1] full(q)*b[1:n1] 100ε
                @test_approx_eq_eps q*b full(q, thin=false)*b 100ε
                @test_throws DimensionMismatch q*b[1:n1 + 1]

debug && println("(Automatic) Fat (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
                if eltya <: BlasFloat
                    @inferred qrfact(a, Val{true})
                    @inferred qr(a, Val{true})
                end
                qrpa  = factorize(a[1:n1,:])
                q,r = qrpa[:Q], qrpa[:R]
                @test_throws KeyError qrpa[:Z]
                if isa(qrpa,QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
                @test_approx_eq q'*full(q, thin=false) eye(n1)
                @test_approx_eq q*full(q, thin=false)' eye(n1)
                @test_approx_eq q*r isa(qrpa,QRPivoted) ? a[1:n1,p] : a[1:n1,:]
                @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[1:n1,:]
                @test_approx_eq_eps a[1:n1,:]*(qrpa\b[1:n1]) b[1:n1] 5000ε
                @test_approx_eq full(qrpa) a[1:5,:]

debug && println("(Automatic) Thin (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
                qrpa  = factorize(a[:,1:n1])
                q,r = qrpa[:Q], qrpa[:R]
                @test_throws KeyError qrpa[:Z]
                if isa(qrpa, QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
                @test_approx_eq q'*full(q, thin=false) eye(n)
                @test_approx_eq q*full(q, thin=false)' eye(n)
                @test_approx_eq q*r isa(qrpa, QRPivoted) ? a[:,p] : a[:,1:n1]
                @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[:,1:n1]
                @test_approx_eq full(qrpa) a[:,1:5]
            end
        end

debug && println("Matmul with QR factorizations")
        if eltya != Int
            qrpa  = factorize(a[:,1:n1])
            q,r = qrpa[:Q], qrpa[:R]
            @test_approx_eq A_mul_B!(full(q,thin=false)',q) eye(n)
            @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
            @test_approx_eq A_mul_Bc!(full(q,thin=false),q) eye(n)
            @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
            @test_throws BoundsError size(q,-1)

            qra   = qrfact(a[:,1:n1], Val{false})
            q,r   = qra[:Q], qra[:R]
            @test_approx_eq A_mul_B!(full(q,thin=false)',q) eye(n)
            @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
            @test_approx_eq A_mul_Bc!(full(q,thin=false),q) eye(n)
            @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
            @test_throws BoundsError size(q,-1)

            @test_throws DimensionMismatch q * eye(Int8,n+4)
        end
    end
end

# Because transpose(x) == x
@test_throws ErrorException transpose(qrfact(randn(3,3)))
@test_throws ErrorException ctranspose(qrfact(randn(3,3)))
@test_throws ErrorException transpose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException ctranspose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException transpose(qrfact(big(randn(3,3))))
@test_throws ErrorException ctranspose(qrfact(big(randn(3,3))))

#Issue 7304
let
    A=[-√.5 -√.5; -√.5 √.5]
    Q=full(qrfact(A)[:Q])
    @test vecnorm(A-Q) < eps()
end
