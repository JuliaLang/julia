# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHessenberg

using Test, LinearAlgebra, Random

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

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
        @test UpperHessenberg(H) === H
        @test parent(H) === A
        @test Matrix(H) == Array(H) == H == AH
        @test real(H) == real(AH)
        @test real(UpperHessenberg{ComplexF64}(A)) == H
        @test real(UpperHessenberg{ComplexF64}(H)) == H
        sim = similar(H, ComplexF64)
        @test sim isa UpperHessenberg{ComplexF64}
        @test size(sim) == size(H)
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
        Hc = UpperHessenberg(Areal + im .* Aimg)
        AHc = triu(Areal + im .* Aimg,-1)
        @test real(Hc) == real(AHc)
        @test imag(Hc) == imag(AHc)
        @test Array(copy(adjoint(Hc))) == adjoint(Array(Hc))
        @test Array(copy(transpose(Hc))) == transpose(Array(Hc))
        @test rmul!(copy(Hc), 2.0) == lmul!(2.0, copy(Hc))
        H = UpperHessenberg(Areal)
        @test Array(Hc + H) == Array(Hc) + Array(H)
        @test Array(Hc - H) == Array(Hc) - Array(H)
        @testset "Preserve UpperHessenberg shape (issue #39388)" begin
            for H = (UpperHessenberg(Areal), UpperHessenberg(Furlong.(Areal)))
                if eltype(H) <: Furlong
                    A = Furlong.(rand(n,n))
                    d = Furlong.(rand(n))
                    dl = Furlong.(rand(n-1))
                    du = Furlong.(rand(n-1))
                    us = Furlong(1)*I
                else
                    A = rand(n,n)
                    d = rand(n)
                    dl = rand(n-1)
                    du = rand(n-1)
                    us = 1*I
                end
                @testset "$op" for op = (+,-)
                    for x = (us, Diagonal(d), Bidiagonal(d,dl,:U), Bidiagonal(d,dl,:L),
                             Tridiagonal(dl,d,du), SymTridiagonal(d,dl),
                             UpperTriangular(A), UnitUpperTriangular(A))
                        @test op(H,x) == op(Array(H),x)
                        @test op(x,H) == op(x,Array(H))
                        @test op(H,x) isa UpperHessenberg
                        @test op(x,H) isa UpperHessenberg
                    end
                end
                A = randn(n,n)
                d = randn(n)
                dl = randn(n-1)
                @testset "Multiplication/division" begin
                    for x = (5, 5I, Diagonal(d), Bidiagonal(d,dl,:U),
                             UpperTriangular(A), UnitUpperTriangular(A))
                        @test H*x == Array(H)*x broken = eltype(H) <: Furlong && x isa Bidiagonal
                        @test x*H == x*Array(H) broken = eltype(H) <: Furlong && x isa Bidiagonal
                        @test H/x == Array(H)/x broken = eltype(H) <: Furlong && x isa Union{Bidiagonal, Diagonal, UpperTriangular}
                        @test x\H == x\Array(H) broken = eltype(H) <: Furlong && x isa Union{Bidiagonal, Diagonal, UpperTriangular}
                        @test H*x isa UpperHessenberg broken = eltype(H) <: Furlong && x isa Bidiagonal
                        @test x*H isa UpperHessenberg broken = eltype(H) <: Furlong && x isa Bidiagonal
                        @test H/x isa UpperHessenberg broken = eltype(H) <: Furlong && x isa Union{Bidiagonal, Diagonal}
                        @test x\H isa UpperHessenberg broken = eltype(H) <: Furlong && x isa Union{Bidiagonal, Diagonal}
                    end
                    x = Bidiagonal(d, dl, :L)
                    @test H*x == Array(H)*x
                    @test x*H == x*Array(H)
                    @test H/x == Array(H)/x broken = eltype(H) <: Furlong
                    @test_broken x\H == x\Array(H) # issue 40037
                end
            end
        end
    end

    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int), herm in (false, true)
        A_ = eltya == Int ?
                rand(1:7, n, n) :
                convert(Matrix{eltya}, eltya <: Complex ?
                    complex.(Areal, Aimg) :
                    Areal)
        A = herm ? Hermitian(A_ + A_') : A_

        H = hessenberg(A)
        @test Hessenberg(H) === H
        eltyh = eltype(H)
        @test size(H.Q, 1) == size(A, 1)
        @test size(H.Q, 2) == size(A, 2)
        @test size(H.Q) == size(A)
        @test size(H) == size(A)
        @test_throws ErrorException H.Z
        @test convert(Array, H) ≈ A
        @test (H.Q * H.H) * H.Q' ≈ A ≈ (Matrix(H.Q) * Matrix(H.H)) * Matrix(H.Q)'
        @test (H.Q' *A) * H.Q ≈ H.H
        #getindex for HessenbergQ
        @test H.Q[1,1] ≈ Array(H.Q)[1,1]

        # REPL show
        hessstring = sprint((t, s) -> show(t, "text/plain", s), H)
        qstring = sprint((t, s) -> show(t, "text/plain", s), H.Q)
        hstring = sprint((t, s) -> show(t, "text/plain", s), H.H)
        @test hessstring == "$(summary(H))\nQ factor:\n$qstring\nH factor:\n$hstring"

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

@testset "Base.propertynames" begin
    F =  hessenberg([4. 9. 7.; 4. 4. 1.; 4. 3. 2.])
    @test Base.propertynames(F) == (:Q, :H, :μ)
    @test Base.propertynames(F, true) == (:Q, :H, :μ, :τ, :factors, :uplo)
end

end # module TestHessenberg
