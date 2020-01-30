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

    # UpperHessenberg methods not covered by the tests below
    @testset "UpperHessenberg" begin
        A = Areal
        H = UpperHessenberg(A)
        AH = triu(A,-1)
        @test Matrix(H) == H == AH
        for x in (2,2+3im)
            @test x*H == H*x == x*AH
            for op in (+,-)
                @test op(H,x*I) == op(AH,x*I) == op(op(x*I,H))
                @test op(H,x*I)*x == op(AH,x*I)*x == x*op(H,x*I)
            end
        end
        @test [H[i,j] for i=1:size(H,1), j=1:size(H,2)] == triu(A,-1)
        H1 = LinearAlgebra.fillstored!(copy(H), 1)
        @test H1 == triu(fill(1, n,n), -1)
        @test tril(H1.data,-2) == tril(H.data,-2)
        A2, H2 = copy(A), copy(H)
        A2[1:4,3]=H2[1:4,3]=1:4
        H2[5,3]=0
        @test H2 == triu(A2,-1)
        @test_throws ArgumentError H[5,3]=1
    end

    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int), herm in (false, true)
        A_ = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex.(Areal, Aimg) :
                    Areal)
        A = herm ? Hermitian(A_ + A_') : A_

        H = hessenberg(A)
        eltyh = eltype(H)
        @test size(H.Q, 1) == size(A, 1)
        @test size(H.Q, 2) == size(A, 2)
        @test size(H.Q) == size(A)
        @test_throws ErrorException H.Z
        @test convert(Array, H) ≈ A
        @test (H.Q * H.H) * H.Q' ≈ A ≈ (Matrix(H.Q) * Matrix(H.H)) * Matrix(H.Q)'
        @test (H.Q' *A) * H.Q ≈ H.H
        #getindex for HessenbergQ
        @test H.Q[1,1] ≈ Array(H.Q)[1,1]

        #iterate
        q,h = H
        @test q == H.Q
        @test h == H.H

        @test convert(Array, 2 * H) ≈ 2 * A ≈ convert(Array, H * 2)
        @test convert(Array, H + 2I) ≈ A + 2I ≈ convert(Array, 2I + H)
        @test convert(Array, H + (2+4im)I) ≈ A + (2+4im)I ≈ convert(Array, (2+4im)I + H)
        @test convert(Array, H - 2I) ≈ A - 2I ≈ -convert(Array, 2I - H)
        @test convert(Array, -H) == -convert(Array, H)
        @test convert(Array, 2*(H + (2+4im)I)) ≈ 2A + (4+8im)I

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

        HM = Matrix(h)
        @test dot(b, h, b) ≈ dot(h'b, b) ≈ dot(b, HM, b) ≈ dot(HM'b, b)
        c = b .+ 1
        @test dot(b, h, c) ≈ dot(h'b, c) ≈ dot(b, HM, c) ≈ dot(HM'b, c)
    end
end

# check logdet on a matrix that has a positive determinant
let A = [0.5 0.1 0.9 0.4; 0.9 0.7 0.5 0.4; 0.3 0.4 0.9 0.0; 0.4 0.0 0.0 0.5]
    @test logdet(hessenberg(A)) ≈ logdet(A) ≈ -3.5065578973199822
end

end # module TestHessenberg
