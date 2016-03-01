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
    asym = a'+a                 # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    α = rand(eltya)
    β = rand(eltya)
    eab = eig(α,β)
    @test eab[1] == eigvals(fill(α,1,1),fill(β,1,1))
    @test eab[2] == eigvecs(fill(α,1,1),fill(β,1,1))

debug && println("\ntype of a: ", eltya,"\n")
debug && println("non-symmetric eigen decomposition")
    d,v   = eig(a)
    for i in 1:size(a,2) @test_approx_eq a*v[:,i] d[i]*v[:,i] end
    f = eigfact(a)
    @test_approx_eq det(a) det(f)
    @test_approx_eq inv(a) inv(f)
    @test eigvals(f) === f[:values]
    @test eigvecs(f) === f[:vectors]

    num_fact = eigfact(one(eltya))
    @test num_fact.values[1] == one(eltya)
    h = a + a'
    @test_approx_eq minimum(eigvals(h)) eigmin(h)
    @test_approx_eq maximum(eigvals(h)) eigmax(h)
    @test_throws DomainError eigmin(a - a')
    @test_throws DomainError eigmax(a - a')

debug && println("symmetric generalized eigenproblem")
    asym_sg = asym[1:n1, 1:n1]
    a_sg = a[:,n1+1:n2]
    f = eigfact(asym_sg, a_sg'a_sg)
    @test_approx_eq asym_sg*f[:vectors] (a_sg'a_sg*f[:vectors]) * Diagonal(f[:values])
    @test_approx_eq f[:values] eigvals(asym_sg, a_sg'a_sg)
    @test_approx_eq_eps prod(f[:values]) prod(eigvals(asym_sg/(a_sg'a_sg))) 200ε
    @test eigvecs(asym_sg, a_sg'a_sg) == f[:vectors]
    @test eigvals(f) === f[:values]
    @test eigvecs(f) === f[:vectors]
    @test_throws KeyError f[:Z]

    d,v = eig(asym_sg, a_sg'a_sg)
    @test d == f[:values]
    @test v == f[:vectors]

debug && println("Non-symmetric generalized eigenproblem")
    a1_nsg = a[1:n1, 1:n1]
    a2_nsg = a[n1+1:n2, n1+1:n2]
    f = eigfact(a1_nsg, a2_nsg)
    @test_approx_eq a1_nsg*f[:vectors] (a2_nsg*f[:vectors]) * Diagonal(f[:values])
    @test_approx_eq f[:values] eigvals(a1_nsg, a2_nsg)
    @test_approx_eq_eps prod(f[:values]) prod(eigvals(a1_nsg/a2_nsg)) 50000ε
    @test eigvecs(a1_nsg, a2_nsg) == f[:vectors]
    @test_throws KeyError f[:Z]

    d,v = eig(a1_nsg, a2_nsg)
    @test d == f[:values]
    @test v == f[:vectors]
end

# test a matrix larger than 140-by-140 for #14174
let a = rand(200, 200)
    f = eigfact(a)
    @test a ≈ f[:vectors] * Diagonal(f[:values]) / f[:vectors]
end
