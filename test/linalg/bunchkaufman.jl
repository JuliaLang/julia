# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "bunchkaufman" begin
debug = false

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
    for atype in ("Array", "SubArray")
        asym = a'+a                  # symmetric indefinite
        apd  = a'*a                 # symmetric positive-definite
        if atype == "Array"
            a = a
            a2 = a2
        else
            a = view(a, 1:n, 1:n)
            a2 = view(a2, 1:n, 1:n)
            asym = view(asym, 1:n, 1:n)
            apd = view(apd, 1:n, 1:n)
        end
        ε = εa = eps(abs(float(one(eltya))))

        for eltyb in (Float32, Float64, Complex64, Complex128, Int)
            b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
            for btype in ("Array", "SubArray")
                if btype == "Array"
                    b = b
                else
                    b = view(b, 1:n, 1:2)
                end

                εb = eps(abs(float(one(eltyb))))
                ε = max(εa,εb)

    debug && println("\neltype of a: ", eltya, " eltype of b: ", eltyb)
    debug && println("  type of a: ", atype, " type of b: ", btype, "\n")

    debug && println("(Automatic) Bunch-Kaufman factor of indefinite matrix")
                bc1 = factorize(asym)
                @test inv(bc1)*asym ≈ eye(n)
                @test_approx_eq_eps asym*(bc1\b) b 1000ε
                for rook in (false, true)
                    @test inv(bkfact(a.'+a, :U, true, rook))*(a.'+a) ≈ eye(n)
                    @test size(bc1) == size(bc1.LD)
                    @test size(bc1,1) == size(bc1.LD,1)
                    @test size(bc1,2) == size(bc1.LD,2)
                    if eltya <: BlasReal
                        @test_throws ArgumentError bkfact(a)
                    end
                end

    debug && println("Bunch-Kaufman factors of a pos-def matrix")
                for rook in (false, true)
                    bc2 = bkfact(apd, :U, issymmetric(apd), rook)
                    @test inv(bc2)*apd ≈ eye(n)
                    @test_approx_eq_eps apd * (bc2\b) b 150000ε
                    @test ishermitian(bc2) == !issymmetric(bc2)
                end
            end
        end
    end
end


debug && println("Bunch-Kaufman factors of a singular matrix")
let
    As1 = ones(n, n)
    As2 = complex(ones(n, n))
    As3 = complex(ones(n, n))
    As3[end, 1] += im
    As3[1, end] -= im

    for As = (As1, As2, As3)
        for Astype in ("Array", "SubArray")
            if Astype == "Array"
                As = As
            else
                As = view(As, 1:n, 1:n)
            end

            for rook in (false, true)
                F = bkfact(As, :U, issymmetric(As), rook)
                @test det(F) == 0
                @test_throws LinAlg.SingularException inv(F)
                @test_throws LinAlg.SingularException F \ ones(size(As, 1))
            end
        end
    end
end

# test example due to @timholy in PR 15354
let
    A = rand(6,5); A = complex(A'*A) # to avoid calling the real-lhs-complex-rhs method
    F = cholfact(A);
    v6 = rand(Complex128, 6)
    v5 = view(v6, 1:5)
    @test F\v5 == F\v6[1:5]
end
end
