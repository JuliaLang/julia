# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2

@testset for eltya in (Float32, Float64, Complex64, Complex128, Int)
    aa = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    asym = aa'+aa                  # symmetric indefinite
    apd  = aa'*aa                 # symmetric positive-definite
    @testset for atype in ("Array", "SubArray")
        if atype == "Array"
            a = aa
        else
            a = view(aa, 1:n, 1:n)
            asym = view(asym, 1:n, 1:n)
            apd = view(apd, 1:n, 1:n)
        end
        ε = εa = eps(abs(float(one(eltya))))

        α = rand(eltya)
        β = rand(eltya)
        eab = eig(α,β)
        @test eab[1] == eigvals(fill(α,1,1),fill(β,1,1))
        @test eab[2] == eigvecs(fill(α,1,1),fill(β,1,1))

        d,v   = eig(a)
        @testset "non-symmetric eigen decomposition" begin
            for i in 1:size(a,2)
                @test a*v[:,i] ≈ d[i]*v[:,i]
            end
            f = eigfact(a)
            @test det(a) ≈ det(f)
            @test inv(a) ≈ inv(f)
            @test eigvals(f) === f[:values]
            @test eigvecs(f) === f[:vectors]

            num_fact = eigfact(one(eltya))
            @test num_fact.values[1] == one(eltya)
            h = asym
            @test minimum(eigvals(h)) ≈ eigmin(h)
            @test maximum(eigvals(h)) ≈ eigmax(h)
            @test_throws DomainError eigmin(a - a')
            @test_throws DomainError eigmax(a - a')
        end
        @testset "symmetric generalized eigenproblem" begin
            if atype == "Array"
                asym_sg = asym[1:n1, 1:n1]
                a_sg = a[:,n1+1:n2]
            else
                asym_sg = view(asym, 1:n1, 1:n1)
                a_sg = view(a, 1:n, n1+1:n2)
            end
            f = eigfact(asym_sg, a_sg'a_sg)
            @test asym_sg*f[:vectors] ≈ (a_sg'a_sg*f[:vectors]) * Diagonal(f[:values])
            @test f[:values] ≈ eigvals(asym_sg, a_sg'a_sg)
            @test prod(f[:values]) ≈ prod(eigvals(asym_sg/(a_sg'a_sg))) atol=200ε
            @test eigvecs(asym_sg, a_sg'a_sg) == f[:vectors]
            @test eigvals(f) === f[:values]
            @test eigvecs(f) === f[:vectors]
            @test_throws KeyError f[:Z]

            d,v = eig(asym_sg, a_sg'a_sg)
            @test d == f[:values]
            @test v == f[:vectors]
        end
        @testset "Non-symmetric generalized eigenproblem" begin
            if atype == "Array"
                a1_nsg = a[1:n1, 1:n1]
                a2_nsg = a[n1+1:n2, n1+1:n2]
            else
                a1_nsg = view(a, 1:n1, 1:n1)
                a2_nsg = view(a, n1+1:n2, n1+1:n2)
            end
            f = eigfact(a1_nsg, a2_nsg)
            @test a1_nsg*f[:vectors] ≈ (a2_nsg*f[:vectors]) * Diagonal(f[:values])
            @test f[:values] ≈ eigvals(a1_nsg, a2_nsg)
            @test prod(f[:values]) ≈ prod(eigvals(a1_nsg/a2_nsg)) atol=50000ε
            @test eigvecs(a1_nsg, a2_nsg) == f[:vectors]
            @test_throws KeyError f[:Z]

            d,v = eig(a1_nsg, a2_nsg)
            @test d == f[:values]
            @test v == f[:vectors]
        end
    end
end

@testset "eigenvalue computations with NaNs" begin
    for eltya in (NaN16, NaN32, NaN)
        @test_throws(ArgumentError, eig(fill(eltya, 1, 1)))
        @test_throws(ArgumentError, eig(fill(eltya, 2, 2)))
        test_matrix = rand(typeof(eltya),3,3)
        test_matrix[2,2] = eltya
        @test_throws(ArgumentError, eig(test_matrix))
    end
end

# test a matrix larger than 140-by-140 for #14174
let aa = rand(200, 200)
    for atype in ("Array", "SubArray")
        if atype == "Array"
            a = aa
        else
            a = view(aa, 1:n, 1:n)
        end
        f = eigfact(a)
        @test a ≈ f[:vectors] * Diagonal(f[:values]) / f[:vectors]
    end
end
