# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHessenberg

using Test, LinearAlgebra, Random

let n = 10
    srand(1234321)

    Areal  = randn(n,n)/2
    Aimg   = randn(n,n)/2

    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
        A = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex.(Areal, Aimg) :
                    Areal)

        if eltya != BigFloat
            H = hessfact(A)
            @test size(H.Q, 1) == size(A, 1)
            @test size(H.Q, 2) == size(A, 2)
            @test size(H.Q) == size(A)
            @test_throws ErrorException H.Z
            @test convert(Array, H) ≈ A
            @test (H.Q * H.H) * H.Q' ≈ A
            @test (H.Q' *A) * H.Q ≈ H.H
            #getindex for HessenbergQ
            @test H.Q[1,1] ≈ Array(H.Q)[1,1]
        end
    end
end

end # module TestHessenberg
