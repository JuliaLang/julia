# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHessenberg

using Test, LinearAlgebra, Random

# for tuple tests below
≅(x,y) = all(p -> p[1] ≈ p[2], zip(x,y))

let n = 10
    Random.seed!(1234321)

    Areal  = randn(n,n)/2
    Aimg   = randn(n,n)/2
    b_ = randn(n)
    B_ = randn(n,3)

    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
        A = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex.(Areal, Aimg) :
                    Areal)

        H = hessenberg(A)
        eltyh = eltype(H)
        @test size(H.Q, 1) == size(A, 1)
        @test size(H.Q, 2) == size(A, 2)
        @test size(H.Q) == size(A)
        @test_throws ErrorException H.Z
        @test convert(Array, H) ≈ A
        @test (H.Q * H.H) * H.Q' ≈ A
        @test (H.Q' *A) * H.Q ≈ H.H
        #getindex for HessenbergQ
        @test H.Q[1,1] ≈ Array(H.Q)[1,1]

        @test convert(Array, 2 * H) ≈ 2 * A ≈ convert(Array, H * 2)
        @test convert(Array, H + 2I) ≈ A + 2I ≈ convert(Array, 2I + H)
        @test convert(Array, H + (2+4im)I) ≈ A + (2+4im)I ≈ convert(Array, (2+4im)I + H)
        @test convert(Array, H - 2I) ≈ A - 2I ≈ -convert(Array, 2I - H)
        @test convert(Array, -H) == -convert(Array, H)

        b = convert(Vector{eltype(H)}, b_)
        B = convert(Matrix{eltype(H)}, B_)
        @test H \ b ≈ A \ b ≈ H \ complex(b)
        @test H \ B ≈ A \ B ≈ H \ complex(B)
        @test (H - I) \ B ≈ (A - I) \ B
        @test (H - (3+4im)I) \ B ≈ (A - (3+4im)I) \ B
        @test b' / H ≈ b' / A ≈ complex.(b') / H
        @test B' / H ≈ B' / A ≈ complex(B') / H
        @test B' / (H - I) ≈ B' / (A - I)
        @test B' / (H - (3+4im)I) ≈ B' / (A - (3+4im)I)
        @test (H - (3+4im)I)' \ B ≈ (A - (3+4im)I)' \ B
        @test B' / (H - (3+4im)I)' ≈ B' / (A - (3+4im)I)'

        for shift in (0,1,3+4im)
            @test det(H + shift*I) ≈ det(A + shift*I)
            @test logabsdet(H + shift*I) ≅ logabsdet(A + shift*I)
        end
    end
end

# check logdet on a matrix that has a positive determinant
let A = [0.5 0.1 0.9 0.4; 0.9 0.7 0.5 0.4; 0.3 0.4 0.9 0.0; 0.4 0.0 0.0 0.5]
    @test logdet(hessenberg(A)) ≈ logdet(A) ≈ -3.5065578973199822
end

end # module TestHessenberg
