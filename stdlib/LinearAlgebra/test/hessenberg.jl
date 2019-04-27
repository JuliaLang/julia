# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHessenberg

using Test, LinearAlgebra, Random

let n = 10
    Random.seed!(1234321)

    Areal  = randn(n,n)/2
    Aimg   = randn(n,n)/2
    b = randn(n)
    B = randn(n,3)

    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
        A = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex.(Areal, Aimg) :
                    Areal)

        H = hessenberg(A)
        @test size(H.Q, 1) == size(A, 1)
        @test size(H.Q, 2) == size(A, 2)
        @test size(H.Q) == size(A)
        @test_throws ErrorException H.Z
        @test convert(Array, H) ≈ A
        @test (H.Q * H.H) * H.Q' ≈ A
        @test (H.Q' *A) * H.Q ≈ H.H
        #getindex for HessenbergQ
        @test H.Q[1,1] ≈ Array(H.Q)[1,1]

        @test convert(Array, 2.3 * H) ≈ 2.3 * A ≈ convert(Array, H * 2.3)
        @test convert(Array, H + 2.3I) ≈ A + 2.3I ≈ convert(Array, 2.3I + H)
        @test convert(Array, H + (2.3+4im)*I) ≈ A + (2.3+4im)*I ≈ convert(Array, (2.3+4im)*I + H)
        @test convert(Array, H - 2.3I) ≈ A - 2.3I ≈ -convert(Array, 2.3I - H)
        @test convert(Array, -H) == -convert(Array, H)
        @test H \ b ≈ A \ b
        @test H \ B ≈ A \ B
        @test (H - (3+4im)*I) \ B ≈ (A - (3+4im)*I) \ B
    end
end

end # module TestHessenberg
