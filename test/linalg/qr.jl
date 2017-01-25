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
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        α = rand(eltyb)
        aα = fill(α,1,1)
        @test qrfact(α)[:Q]*qrfact(α)[:R] ≈ qrfact(aα)[:Q]*qrfact(aα)[:R]
        @test abs(qrfact(α)[:Q][1,1]) ≈ one(eltyb)
        tab = promote_type(eltya,eltyb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")
debug && println("QR decomposition (without pivoting)")
        for i = 1:2
            let a = i == 1 ? a : view(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : view(b, 1:n - 1), n = i == 1 ? n : n - 1
                qra   = @inferred qrfact(a)
                @inferred qr(a)
                q, r  = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test q'*full(q, thin=false) ≈ eye(n)
                @test q*full(q, thin=false)' ≈ eye(n)
                @test q'*eye(n)' ≈ full(q, thin=false)'
                @test full(q, thin=false)'q ≈ eye(n)
                @test eye(n)'q' ≈ full(q, thin=false)'
                @test q*r ≈ a
                @test a*(qra\b) ≈ b atol=3000ε
                @test full(qra) ≈ a
                @test A_mul_Bc(eye(eltyb,size(q.factors,2)),q)*full(q,thin=false) ≈ eye(n) atol=5000ε
                if eltya != Int
                    @test eye(eltyb,n)*q ≈ convert(AbstractMatrix{tab},q)
                    ac = copy(a)
                    @test qrfact!(a[:, 1:5])\b == qrfact!(view(ac, :, 1:5))\b
                end

debug && println("Thin QR decomposition (without pivoting)")
                qra   = @inferred qrfact(a[:,1:n1], Val{false})
                @inferred qr(a[:,1:n1], Val{false})
                q,r   = qra[:Q], qra[:R]
                @test_throws KeyError qra[:Z]
                @test q'*full(q, thin=false) ≈ eye(n)
                @test q'*full(q) ≈ eye(n,n1)
                @test q*r ≈ a[:,1:n1]
                @test q*b[1:n1] ≈ full(q)*b[1:n1] atol=100ε
                @test q*b ≈ full(q,thin=false)*b atol=100ε
                @test_throws DimensionMismatch q*b[1:n1 + 1]
                @test_throws DimensionMismatch b[1:n1 + 1]*q'
                @test A_mul_Bc(UpperTriangular(eye(eltyb,size(q.factors,2))),q)*full(q,thin=false) ≈ eye(n1,n) atol=5000ε
                if eltya != Int
                    @test eye(eltyb,n)*q ≈ convert(AbstractMatrix{tab},q)
                end

debug && println("(Automatic) Fat (pivoted) QR decomposition")
                @inferred qrfact(a, Val{true})
                @inferred qr(a, Val{true})

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

debug && println("(Automatic) Thin (pivoted) QR decomposition")
                qrpa  = factorize(a[:,1:n1])
                q,r = qrpa[:Q], qrpa[:R]
                @test_throws KeyError qrpa[:Z]
                p = qrpa[:p]
                @test q'*full(q, thin=false) ≈ eye(n)
                @test q*full(q, thin=false)' ≈ eye(n)
                @test q*r ≈ a[:,p]
                @test q*r[:,invperm(p)] ≈ a[:,1:n1]
                @test full(qrpa) ≈ a[:,1:5]
                @test_throws DimensionMismatch q*b[1:n1+1]
                @test_throws DimensionMismatch b[1:n1+1]*q'
                @test A_mul_Bc(UpperTriangular(eye(eltyb,size(q.factors,2))),q)*full(q,thin=false) ≈ eye(n1,n) atol=5000ε
                if eltya != Int
                    @test eye(eltyb,n)*q ≈ convert(AbstractMatrix{tab},q)
                end
            end
        end

debug && println("Matmul with QR factorizations")
        if eltya != Int
            qrpa = factorize(a[:,1:n1])
            q, r = qrpa[:Q], qrpa[:R]
            @test A_mul_B!(full(q, thin=false)',q) ≈ eye(n)
            @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
            @test A_mul_Bc!(full(q, thin=false),q) ≈ eye(n)
            @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
            @test_throws BoundsError size(q,-1)
            @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(q,zeros(eltya,n1+1))
            @test_throws DimensionMismatch Base.LinAlg.Ac_mul_B!(q,zeros(eltya,n1+1))

            qra = qrfact(a[:,1:n1], Val{false})
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

# Because transpose(x) == x
@test_throws ErrorException transpose(qrfact(randn(3,3)))
@test_throws ErrorException ctranspose(qrfact(randn(3,3)))
@test_throws ErrorException transpose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException ctranspose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException transpose(qrfact(big.(randn(3,3))))
@test_throws ErrorException ctranspose(qrfact(big.(randn(3,3))))

# Issue 7304
let
    A = [-√.5 -√.5; -√.5 √.5]
    Q = full(qrfact(A)[:Q])
    @test vecnorm(A-Q) < eps()
end

let
    debug && println("qr on AbstractVector")

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

@test qr(Int[]) == (Int[],1)
@test Base.LinAlg.qr!(Int[1]) == (Int[1],1)

B = rand(7,2)
@test (1:7)\B ≈ collect(1:7)\B

# Issue 16520
@test_throws DimensionMismatch ones(3,2)\(1:5)
