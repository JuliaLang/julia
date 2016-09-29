# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

let n = 10
    srand(1234321)

    Areal  = randn(n,n)/2
    Aimg   = randn(n,n)/2

    for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
        A = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex(Areal, Aimg) :
                    Areal)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")

        if eltya != BigFloat
            H = hessfact(A)
            @test size(H[:Q], 1) == size(A, 1)
            @test size(H[:Q], 2) == size(A, 2)
            @test size(H[:Q]) == size(A)
            @test_throws KeyError H[:Z]
            @test full(H) ≈ A
            @test (H[:Q] * H[:H]) * H[:Q]' ≈ A
            @test (H[:Q]' *A) * H[:Q] ≈ H[:H]
            #getindex for HessenbergQ
            @test H[:Q][1,1] ≈ full(H[:Q])[1,1]
        end
    end
end
