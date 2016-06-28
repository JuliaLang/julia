# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "svd" begin
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

for eltya in (Float32, Float64, Complex64, Complex128, Int)
    aa = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    aa2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    asym = aa'+aa                  # symmetric indefinite
    apd  = aa'*aa                 # symmetric positive-definite
    for atype in ("Array", "SubArray")
        if atype == "Array"
            a = aa
            a2 = aa2
        else
            a = view(aa, 1:n, 1:n)
            a2 = view(aa2, 1:n, 1:n)
        end
        ε = εa = eps(abs(float(one(eltya))))

    debug && println("\ntype of a: ", eltya, "\n")

    debug && println("singular value decomposition")
        usv = svdfact(a)
        @test usv[:S] === svdvals(usv)
        @test usv[:U] * (Diagonal(usv[:S]) * usv[:Vt]) ≈ a
        @test full(usv) ≈ a
        @test usv[:Vt]' ≈ usv[:V]
        @test_throws KeyError usv[:Z]
        b = rand(eltya,n)
        @test usv\b ≈ a\b

        if eltya <: BlasFloat
            svdz = svdfact!(ones(eltya,0,0))
            @test svdz[:U] ≈ eye(eltya,0,0)
            @test svdz[:S] ≈ real(zeros(eltya,0))
            @test svdz[:Vt] ≈ eye(eltya,0,0)
        end

    debug && println("Generalized svd")
        a_svd = a[1:n1, :]
        gsvd = svdfact(a,a_svd)
        @test gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' ≈ a
        @test gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' ≈ a_svd
        @test usv[:Vt]' ≈ usv[:V]
        @test_throws KeyError usv[:Z]
        @test_throws KeyError gsvd[:Z]
        @test gsvd[:vals] ≈ svdvals(a,a_svd)
        α = eltya == Int ? -1 : rand(eltya)
        β = svdfact(α)
        @test β[:S] == [abs(α)]
        @test svdvals(α) == abs(α)
        u,v,q,d1,d2,r0 = svd(a,a_svd)
        @test u ≈ gsvd[:U]
        @test v ≈ gsvd[:V]
        @test d1 ≈ gsvd[:D1]
        @test d2 ≈ gsvd[:D2]
        @test q ≈ gsvd[:Q]
        @test gsvd[:a].^2 + gsvd[:b].^2 ≈ ones(eltya,length(gsvd[:a]))

        #testing the other layout for D1 & D2
        b = rand(eltya,n,2*n)
        c = rand(eltya,n,2*n)
        gsvd = svdfact(b,c)
        @test gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' ≈ b
        @test gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' ≈ c
    end
end
end
