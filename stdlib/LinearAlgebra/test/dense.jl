# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDense

using InteractiveUtils
using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal

@testset "Check that non-floats are correctly promoted" begin
    @test [1 0 0; 0 1 0]\[1,1] ≈ [1;1;0]
end

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

Random.seed!(1234321)

@testset "Matrix condition number" begin
    ainit = rand(n,n)
    @testset "for $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
        ainit = convert(Matrix{elty}, ainit)
        for a in (copy(ainit), view(ainit, 1:n, 1:n))
            @test cond(a,1) ≈ 4.837320054554436e+02 atol=0.01
            @test cond(a,2) ≈ 1.960057871514615e+02 atol=0.01
            @test cond(a,Inf) ≈ 3.757017682707787e+02 atol=0.01
            @test cond(a[:,1:5]) ≈ 10.233059337453463 atol=0.01
            @test_throws ArgumentError cond(a,3)
        end
    end
    @testset "Singular matrices" for p in (1, 2, Inf)
        @test cond(zeros(Int, 2, 2), p) == Inf
        @test cond(zeros(2, 2), p) == Inf
        @test cond([0 0; 1 1], p) == Inf
        @test cond([0. 0.; 1. 1.], p) == Inf
    end
    @testset "Issue #33547, condition number of 2x2 matrix" begin
        M = [1.0 -2.0; -2.0 -1.5]
        @test cond(M, 1) ≈ 2.227272727272727
    end
end

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

@testset "For A containing $eltya" for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
    ainit = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    ainit2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    ε = εa = eps(abs(float(one(eltya))))

    apd  = ainit'*ainit # symmetric positive-definite
    @testset "Positive definiteness" begin
        @test !isposdef(ainit)
        @test isposdef(apd)
        if eltya != Int # cannot perform cholesky! for Matrix{Int}
            @test !isposdef!(copy(ainit))
            @test isposdef!(copy(apd))
        end
    end
    @testset "For b containing $eltyb" for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
        binit = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)
        for (a, b) in ((copy(ainit), copy(binit)), (view(ainit, 1:n, 1:n), view(binit, 1:n, 1:2)))
            @testset "Solve square general system of equations" begin
                κ = cond(a,1)
                x = a \ b
                @test_throws DimensionMismatch b'\b
                @test_throws DimensionMismatch b\b'
                @test norm(a*x - b, 1)/norm(b) < ε*κ*n*2 # Ad hoc, revisit!
                @test zeros(eltya,n)\fill(eltya(1),n) ≈ (zeros(eltya,n,1)\fill(eltya(1),n,1))[1,1]
            end

            @testset "Test nullspace" begin
                a15null = nullspace(a[:,1:n1]')
                @test rank([a[:,1:n1] a15null]) == 10
                @test norm(a[:,1:n1]'a15null,Inf) ≈ zero(eltya) atol=300ε
                @test norm(a15null'a[:,1:n1],Inf) ≈ zero(eltya) atol=400ε
                @test size(nullspace(b), 2) == 0
                @test size(nullspace(b, rtol=0.001), 2) == 0
                @test size(nullspace(b, atol=100*εb), 2) == 0
                @test size(nullspace(b, 100*εb), 2) == 0
                @test nullspace(zeros(eltya,n)) == Matrix(I, 1, 1)
                @test nullspace(zeros(eltya,n), 0.1) == Matrix(I, 1, 1)
                # test empty cases
                @test nullspace(zeros(n, 0)) == Matrix(I, 0, 0)
                @test nullspace(zeros(0, n)) == Matrix(I, n, n)
            end
        end
    end # for eltyb

@testset "Test diagm for vectors" begin
    @test diagm(zeros(50)) == diagm(0 => zeros(50))
    @test diagm(ones(50)) == diagm(0 => ones(50))
    v = randn(500)
    @test diagm(v) == diagm(0 => v)
    @test diagm(500, 501, v) == diagm(500, 501, 0 => v)
end

@testset "Non-square diagm" begin
    x = [7, 8]
    for m=1:4, n=2:4
        if m < 2 || n < 3
            @test_throws DimensionMismatch diagm(m,n, 0 => x,  1 => x)
            @test_throws DimensionMismatch diagm(n,m, 0 => x,  -1 => x)
        else
            M = zeros(m,n)
            M[1:2,1:3] = [7 7 0; 0 8 8]
            @test diagm(m,n, 0 => x,  1 => x) == M
            @test diagm(n,m, 0 => x,  -1 => x) == M'
        end
    end
end

@testset "Test pinv (rtol, atol)" begin
    M = [1 0 0; 0 1 0; 0 0 0]
    @test pinv(M,atol=1)== zeros(3,3)
    @test pinv(M,rtol=0.5)== M
end

    for (a, a2) in ((copy(ainit), copy(ainit2)), (view(ainit, 1:n, 1:n), view(ainit2, 1:n, 1:n)))
        @testset "Test pinv" begin
            pinva15 = pinv(a[:,1:n1])
            @test a[:,1:n1]*pinva15*a[:,1:n1] ≈ a[:,1:n1]
            @test pinva15*a[:,1:n1]*pinva15 ≈ pinva15
            pinva15 = pinv(a[:,1:n1]') # the Adjoint case
            @test a[:,1:n1]'*pinva15*a[:,1:n1]' ≈ a[:,1:n1]'
            @test pinva15*a[:,1:n1]'*pinva15 ≈ pinva15

            @test size(pinv(Matrix{eltya}(undef,0,0))) == (0,0)
        end

        @testset "Lyapunov/Sylvester" begin
            x = lyap(a, a2)
            @test -a2 ≈ a*x + x*a'
            x2 = sylvester(a[1:3, 1:3], a[4:n, 4:n], a2[1:3,4:n])
            @test -a2[1:3, 4:n] ≈ a[1:3, 1:3]*x2 + x2*a[4:n, 4:n]
        end

        @testset "Matrix square root" begin
            asq = sqrt(a)
            @test asq*asq ≈ a
            asym = a + a' # symmetric indefinite
            asymsq = sqrt(asym)
            @test asymsq*asymsq ≈ asym
        end

        @testset "Powers" begin
            if eltya <: AbstractFloat
                z = zero(eltya)
                t = convert(eltya,2)
                r = convert(eltya,2.5)
                @test a^z ≈ Matrix(I, size(a))
                @test a^t ≈ a^2
                @test Matrix{eltya}(I, n, n)^r ≈ Matrix(I, size(a))
            end
        end
    end # end for loop over arraytype

    @testset "Factorize" begin
        d = rand(eltya,n)
        e = rand(eltya,n-1)
        e2 = rand(eltya,n-1)
        f = rand(eltya,n-2)
        A = diagm(0 => d)
        @test factorize(A) == Diagonal(d)
        A += diagm(-1 => e)
        @test factorize(A) == Bidiagonal(d,e,:L)
        A += diagm(-2 => f)
        @test factorize(A) == LowerTriangular(A)
        A = diagm(0 => d, 1 => e)
        @test factorize(A) == Bidiagonal(d,e,:U)
        if eltya <: Real
            A = diagm(0 => d, 1 => e, -1 => e)
            @test Matrix(factorize(A)) ≈ Matrix(factorize(SymTridiagonal(d,e)))
            A = diagm(0 => d, 1 => e, -1 => e, 2 => f, -2 => f)
            @test inv(factorize(A)) ≈ inv(factorize(Symmetric(A)))
        end
        A = diagm(0 => d, 1 => e, -1 => e2)
        @test Matrix(factorize(A)) ≈ Matrix(factorize(Tridiagonal(e2,d,e)))
        A = diagm(0 => d, 1 => e, 2 => f)
        @test factorize(A) == UpperTriangular(A)
    end
end # for eltya

@testset "test out of bounds triu/tril" begin
    local m, n = 5, 7
    ainit = rand(m, n)
    for a in (copy(ainit), view(ainit, 1:m, 1:n))
        @test triu(a, -m) == a
        @test triu(a, n + 2) == zero(a)
        @test tril(a, -m - 2) == zero(a)
        @test tril(a, n) == a
    end
end

@testset "triu M > N case bug fix" begin
    mat=[1 2;
         3 4;
         5 6;
         7 8]
    res=[1 2;
         3 4;
         0 6;
         0 0]
    @test triu(mat, -1) == res
end

@testset "Tests norms" begin
    nnorm = 10
    mmat = 10
    nmat = 8
    @testset "For $elty" for elty in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat}, Int32, Int64, BigInt)
        x = fill(elty(1),10)
        @testset "Vector" begin
            xs = view(x,1:2:10)
            @test norm(x, -Inf) ≈ 1
            @test norm(x, -1) ≈ 1/10
            @test norm(x, 0) ≈ 10
            @test norm(x, 1) ≈ 10
            @test norm(x, 2) ≈ sqrt(10)
            @test norm(x, 3) ≈ cbrt(10)
            @test norm(x, Inf) ≈ 1
            if elty <: LinearAlgebra.BlasFloat
                @test norm(x, 1:4) ≈ 2
                @test_throws BoundsError norm(x,-1:4)
                @test_throws BoundsError norm(x,1:11)
            end
            @test norm(xs, -Inf) ≈ 1
            @test norm(xs, -1) ≈ 1/5
            @test norm(xs, 0) ≈ 5
            @test norm(xs, 1) ≈ 5
            @test norm(xs, 2) ≈ sqrt(5)
            @test norm(xs, 3) ≈ cbrt(5)
            @test norm(xs, Inf) ≈ 1
        end

        @testset "Issue #12552:" begin
            if real(elty) <: AbstractFloat
                for p in [-Inf,-1,1,2,3,Inf]
                    @test isnan(norm(elty[0,NaN],p))
                    @test isnan(norm(elty[NaN,0],p))
                end
            end
        end

        @testset "Number" begin
            norm(x[1:1]) === norm(x[1], -Inf)
            norm(x[1:1]) === norm(x[1], 0)
            norm(x[1:1]) === norm(x[1], 1)
            norm(x[1:1]) === norm(x[1], 2)
            norm(x[1:1]) === norm(x[1], Inf)
        end

        @testset "Absolute homogeneity, triangle inequality, & vectorized versions" begin
            for i = 1:10
                xinit = elty <: Integer ? convert(Vector{elty}, rand(1:10, nnorm)) :
                        elty <: Complex ? convert(Vector{elty}, complex.(randn(nnorm), randn(nnorm))) :
                        convert(Vector{elty}, randn(nnorm))
                yinit = elty <: Integer ? convert(Vector{elty}, rand(1:10, nnorm)) :
                        elty <: Complex ? convert(Vector{elty}, complex.(randn(nnorm), randn(nnorm))) :
                        convert(Vector{elty}, randn(nnorm))
                α = elty <: Integer ? randn() :
                    elty <: Complex ? convert(elty, complex(randn(),randn())) :
                    convert(elty, randn())
                for (x, y) in ((copy(xinit), copy(yinit)), (view(xinit,1:2:nnorm), view(yinit,1:2:nnorm)))
                    # Absolute homogeneity
                    @test norm(α*x,-Inf) ≈ abs(α)*norm(x,-Inf)
                    @test norm(α*x,-1) ≈ abs(α)*norm(x,-1)
                    @test norm(α*x,1) ≈ abs(α)*norm(x,1)
                    @test norm(α*x) ≈ abs(α)*norm(x) # two is default
                    @test norm(α*x,3) ≈ abs(α)*norm(x,3)
                    @test norm(α*x,Inf) ≈ abs(α)*norm(x,Inf)

                    # Triangle inequality
                    @test norm(x + y,1) <= norm(x,1) + norm(y,1)
                    @test norm(x + y) <= norm(x) + norm(y) # two is default
                    @test norm(x + y,3) <= norm(x,3) + norm(y,3)
                    @test norm(x + y,Inf) <= norm(x,Inf) + norm(y,Inf)

                    # Against vectorized versions
                    @test norm(x,-Inf) ≈ minimum(abs.(x))
                    @test norm(x,-1) ≈ inv(sum(1 ./ abs.(x)))
                    @test norm(x,0) ≈ sum(x .!= 0)
                    @test norm(x,1) ≈ sum(abs.(x))
                    @test norm(x) ≈ sqrt(sum(abs2.(x)))
                    @test norm(x,3) ≈ cbrt(sum(abs.(x).^3.))
                    @test norm(x,Inf) ≈ maximum(abs.(x))
                end
            end
        end

        @testset "Matrix (Operator) opnorm" begin
            A = fill(elty(1),10,10)
            As = view(A,1:5,1:5)
            @test opnorm(A, 1) ≈ 10
            elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test opnorm(A, 2) ≈ 10
            @test opnorm(A, Inf) ≈ 10
            @test opnorm(As, 1) ≈ 5
            elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test opnorm(As, 2) ≈ 5
            @test opnorm(As, Inf) ≈ 5
        end

        @testset "Absolute homogeneity, triangle inequality, & norm" begin
            for i = 1:10
                Ainit = elty <: Integer ? convert(Matrix{elty}, rand(1:10, mmat, nmat)) :
                        elty <: Complex ? convert(Matrix{elty}, complex.(randn(mmat, nmat), randn(mmat, nmat))) :
                        convert(Matrix{elty}, randn(mmat, nmat))
                Binit = elty <: Integer ? convert(Matrix{elty}, rand(1:10, mmat, nmat)) :
                        elty <: Complex ? convert(Matrix{elty}, complex.(randn(mmat, nmat), randn(mmat, nmat))) :
                        convert(Matrix{elty}, randn(mmat, nmat))
                α = elty <: Integer ? randn() :
                    elty <: Complex ? convert(elty, complex(randn(),randn())) :
                    convert(elty, randn())
                for (A, B) in ((copy(Ainit), copy(Binit)), (view(Ainit,1:nmat,1:nmat), view(Binit,1:nmat,1:nmat)))
                    # Absolute homogeneity
                    @test norm(α*A,1) ≈ abs(α)*norm(A,1)
                    elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(α*A) ≈ abs(α)*norm(A) # two is default
                    @test norm(α*A,Inf) ≈ abs(α)*norm(A,Inf)

                    # Triangle inequality
                    @test norm(A + B,1) <= norm(A,1) + norm(B,1)
                    elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(A + B) <= norm(A) + norm(B) # two is default
                    @test norm(A + B,Inf) <= norm(A,Inf) + norm(B,Inf)

                    # norm
                    for p in (-Inf, Inf, (-2:3)...)
                        if norm(A, p) != norm(vec(A), p)
                            @show A
                            @show typeof(A)
                            @show size(A)
                            @show p
                            @show InteractiveUtils.@which norm(A,p)
                            @show InteractiveUtils.@which norm(vec(A),p)
                            @show InteractiveUtils.@which LinearAlgebra.norm2(A)
                            @show InteractiveUtils.@which LinearAlgebra.norm2(vec(A))
                            @show norm(A, p)
                            @show norm(vec(A), p)
                            @show LinearAlgebra.norm2(A)
                            @show LinearAlgebra.norm2(vec(A))
                            error()
                        end
                        @test norm(A, p) == norm(vec(A), p)
                    end
                end
            end

            @testset "issue #10234" begin
                if elty <: AbstractFloat || elty <: Complex
                    z = zeros(elty, 100)
                    z[1] = -Inf
                    for p in [-2,-1.5,-1,-0.5,0.5,1,1.5,2,Inf]
                        @test norm(z, p) == (p < 0 ? 0 : Inf)
                        @test norm(elty[Inf],p) == Inf
                    end
                end
            end
        end
    end

    @testset "issue #10234" begin
        @test norm(Any[Inf],-2) == norm(Any[Inf],-1) == norm(Any[Inf],1) == norm(Any[Inf],1.5) == norm(Any[Inf],2) == norm(Any[Inf],Inf) == Inf
    end

    @testset "overflow/underflow in norms" begin
        @test norm(Float64[1e-300, 1], -3)*1e300 ≈ 1
        @test norm(Float64[1e300, 1], 3)*1e-300 ≈ 1
    end
end

## Issue related tests
@testset "issue #1447" begin
    A = [1.0+0.0im 0; 0 1]
    B = pinv(A)
    for i = 1:4
        @test A[i] ≈ B[i]
    end
end

@testset "issue #2246" begin
    A = [1 2 0 0; 0 1 0 0; 0 0 0 0; 0 0 0 0]
    Asq = sqrt(A)
    @test Asq*Asq ≈ A
    A2 = view(A, 1:2, 1:2)
    A2sq = sqrt(A2)
    @test A2sq*A2sq ≈ A2

    N = 3
    @test log(det(Matrix(1.0I, N, N))) ≈ logdet(Matrix(1.0I, N, N))
end

@testset "issue #2637" begin
    a = [1, 2, 3]
    b = [4, 5, 6]
    @test kron(Matrix(I, 2, 2), Matrix(I, 2, 2)) == Matrix(I, 4, 4)
    @test kron(a,b) == [4,5,6,8,10,12,12,15,18]
    @test kron(a',b') == [4 5 6 8 10 12 12 15 18]
    @test kron(a,b')  == [4 5 6; 8 10 12; 12 15 18]
    @test kron(a',b)  == [4 8 12; 5 10 15; 6 12 18]
    @test kron(a, Matrix(1I, 2, 2)) == [1 0; 0 1; 2 0; 0 2; 3 0; 0 3]
    @test kron(Matrix(1I, 2, 2), a) == [ 1 0; 2 0; 3 0; 0 1; 0 2; 0 3]
    @test kron(Matrix(1I, 2, 2), 2) == Matrix(2I, 2, 2)
    @test kron(3, Matrix(1I, 3, 3)) == Matrix(3I, 3, 3)
    @test kron(a,2) == [2, 4, 6]
    @test kron(b',2) == [8 10 12]
end

@testset "kron!" begin
    a = [1.0, 0.0]
    b = [0.0, 1.0]
    @test kron!([1.0, 0.0], b, 0.5) == [0.0; 0.5]
    @test kron!([1.0, 0.0], 0.5, b) == [0.0; 0.5]
    c = Vector{Float64}(undef, 4)
    kron!(c, a, b)
    @test c == [0.0; 1.0; 0.0; 0.0]
    c = Matrix{Float64}(undef, 2, 2)
    kron!(c, a, b')
    @test c == [0.0 1.0; 0.0 0.0]
end

@testset "kron adjoint" begin
    a = [1+im, 2, 3]
    b = [4, 5, 6+7im]
    @test kron(a', b') isa Adjoint
    @test kron(a', b') == kron(a, b)'
    @test kron(transpose(a), b') isa Transpose
    @test kron(transpose(a), b') == kron(permutedims(a), collect(b'))
    @test kron(transpose(a), transpose(b)) isa Transpose
    @test kron(transpose(a), transpose(b)) == transpose(kron(a, b))
end

@testset "issue #4796" begin
    dim=2
    S=zeros(Complex,dim,dim)
    T=zeros(Complex,dim,dim)
    fill!(T, 1)
    z = 2.5 + 1.5im
    S[1] = z
    @test S*T == [z z; 0 0]

    # similar issue for Array{Real}
    @test Real[1 2] * Real[1.5; 2.0] == Real[5.5]
end

@testset "Matrix exponential" begin
    @testset "Tests for $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A1  = convert(Matrix{elty}, [4 2 0; 1 4 1; 1 1 4])
        eA1 = convert(Matrix{elty}, [147.866622446369 127.781085523181  127.781085523182;
                                     183.765138646367 183.765138646366  163.679601723179;
                                      71.797032399996  91.8825693231832 111.968106246371]')
        @test exp(A1) ≈ eA1

        A2  = convert(Matrix{elty},
                      [29.87942128909879    0.7815750847907159 -2.289519314033932;
                       0.7815750847907159 25.72656945571064    8.680737820540137;
                       -2.289519314033932   8.680737820540137  34.39400925519054])
        eA2 = convert(Matrix{elty},
                      [  5496313853692458.0 -18231880972009236.0 -30475770808580460.0;
                       -18231880972009252.0  60605228702221920.0 101291842930249760.0;
                       -30475770808580480.0 101291842930249728.0 169294411240851968.0])
        @test exp(A2) ≈ eA2

        A3  = convert(Matrix{elty}, [-131 19 18;-390 56 54;-387 57 52])
        eA3 = convert(Matrix{elty}, [-1.50964415879218 -5.6325707998812  -4.934938326092;
                                      0.367879439109187 1.47151775849686  1.10363831732856;
                                      0.135335281175235 0.406005843524598 0.541341126763207]')
        @test exp(A3) ≈ eA3

        A4 = convert(Matrix{elty}, [0.25 0.25; 0 0])
        eA4 = convert(Matrix{elty}, [1.2840254166877416 0.2840254166877415; 0 1])
        @test exp(A4) ≈ eA4

        A5 = convert(Matrix{elty}, [0 0.02; 0 0])
        eA5 = convert(Matrix{elty}, [1 0.02; 0 1])
        @test exp(A5) ≈ eA5

        # Hessenberg
        @test hessenberg(A1).H ≈ convert(Matrix{elty},
                                                 [4.000000000000000  -1.414213562373094  -1.414213562373095
                                                  -1.414213562373095   4.999999999999996  -0.000000000000000
                                                  0  -0.000000000000002   3.000000000000000])
    end

    @testset "Additional tests for $elty" for elty in (Float64, ComplexF64)
        A4  = convert(Matrix{elty}, [1/2 1/3 1/4 1/5+eps();
                                     1/3 1/4 1/5 1/6;
                                     1/4 1/5 1/6 1/7;
                                     1/5 1/6 1/7 1/8])
        @test exp(log(A4)) ≈ A4

        A5  = convert(Matrix{elty}, [1 1 0 1; 0 1 1 0; 0 0 1 1; 1 0 0 1])
        @test exp(log(A5)) ≈ A5

        A6  = convert(Matrix{elty}, [-5 2 0 0 ; 1/2 -7 3 0; 0 1/3 -9 4; 0 0 1/4 -11])
        @test exp(log(A6)) ≈ A6

        A7  = convert(Matrix{elty}, [1 0 0 1e-8; 0 1 0 0; 0 0 1 0; 0 0 0 1])
        @test exp(log(A7)) ≈ A7
    end

    @testset "Integer promotion tests" begin
        for (elty1, elty2) in ((Int64, Float64), (Complex{Int64}, ComplexF64))
            A4int  = convert(Matrix{elty1}, [1 2; 3 4])
            A4float  = convert(Matrix{elty2}, A4int)
            @test exp(A4int) == exp(A4float)
        end
    end

    @testset "^ tests" for elty in (Float32, Float64, ComplexF32, ComplexF64, Int32, Int64)
        # should all be exact as the lhs functions are simple aliases
        @test ℯ^(fill(elty(2), (4,4))) == exp(fill(elty(2), (4,4)))
        @test 2^(fill(elty(2), (4,4))) == exp(log(2)*fill(elty(2), (4,4)))
        @test 2.0^(fill(elty(2), (4,4))) == exp(log(2.0)*fill(elty(2), (4,4)))
    end

    A8 = 100 * [-1+1im 0 0 1e-8; 0 1 0 0; 0 0 1 0; 0 0 0 1]
    @test exp(log(A8)) ≈ A8
end

@testset "Matrix trigonometry" begin
    @testset "Tests for $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A1  = convert(Matrix{elty}, [3 2 0; 1 3 1; 1 1 3])
        A2  = convert(Matrix{elty},
                      [3.975884257819758 0.15631501695814318 -0.4579038628067864;
                       0.15631501695814318 4.545313891142127 1.7361475641080275;
                       -0.4579038628067864 1.7361475641080275 6.478801851038108])
        A3 = convert(Matrix{elty}, [0.25 0.25; 0 0])
        A4 = convert(Matrix{elty}, [0 0.02; 0 0])

        cosA1 = convert(Matrix{elty},[-0.18287716254368605 -0.29517205254584633 0.761711400552759;
                                      0.23326967400345625 0.19797853773269333 -0.14758602627292305;
                                      0.23326967400345636 0.6141253742798355 -0.5637328628200653])
        sinA1 = convert(Matrix{elty}, [0.2865568596627417 -1.107751980582015 -0.13772915374386513;
                                       -0.6227405671629401 0.2176922827908092 -0.5538759902910078;
                                       -0.6227405671629398 -0.6916051440348725 0.3554214365346742])
        @test cos(A1) ≈ cosA1
        @test sin(A1) ≈ sinA1

        cosA2 = convert(Matrix{elty}, [-0.6331745163802187 0.12878366262380136 -0.17304181968301532;
                                       0.12878366262380136 -0.5596234510748788 0.5210483146041339;
                                       -0.17304181968301532 0.5210483146041339 0.002263776356015268])
        sinA2 = convert(Matrix{elty},[-0.6677253518411841 -0.32599318928375437 0.020799609079003523;
                                      -0.32599318928375437 -0.04568726058081066 0.5388748740270427;
                                      0.020799609079003523 0.5388748740270427 0.6385462428126032])
        @test cos(A2) ≈ cosA2
        @test sin(A2) ≈ sinA2

        cosA3 = convert(Matrix{elty}, [0.9689124217106446 -0.031087578289355197; 0.0 1.0])
        sinA3 = convert(Matrix{elty}, [0.24740395925452285 0.24740395925452285; 0.0 0.0])
        @test cos(A3) ≈ cosA3
        @test sin(A3) ≈ sinA3

        cosA4 = convert(Matrix{elty}, [1.0 0.0; 0.0 1.0])
        sinA4 = convert(Matrix{elty}, [0.0 0.02; 0.0 0.0])
        @test cos(A4) ≈ cosA4
        @test sin(A4) ≈ sinA4

        # Identities
        for (i, A) in enumerate((A1, A2, A3, A4))
            @test sincos(A) == (sin(A), cos(A))
            @test cos(A)^2 + sin(A)^2 ≈ Matrix(I, size(A))
            @test cos(A) ≈ cos(-A)
            @test sin(A) ≈ -sin(-A)
            @test tan(A) ≈ sin(A) / cos(A)
            @test cos(A) ≈ real(exp(im*A))
            @test sin(A) ≈ imag(exp(im*A))
            @test cosh(A) ≈ 0.5 * (exp(A) + exp(-A))
            @test sinh(A) ≈ 0.5 * (exp(A) - exp(-A))
            @test cosh(A) ≈ cosh(-A)
            @test sinh(A) ≈ -sinh(-A)

            # Some of the following identities fail for A3, A4 because the matrices are singular
            if i in (1, 2)
                @test sec(A) ≈ inv(cos(A))
                @test csc(A) ≈ inv(sin(A))
                @test cot(A) ≈ inv(tan(A))
                @test sech(A) ≈ inv(cosh(A))
                @test csch(A) ≈ inv(sinh(A))
                @test coth(A) ≈ inv(tanh(A))
            end
            # The following identities fail for A1, A2 due to rounding errors;
            # probably needs better algorithm for the general case
            if i in (3, 4)
                @test cosh(A)^2 - sinh(A)^2 ≈ Matrix(I, size(A))
                @test tanh(A) ≈ sinh(A) / cosh(A)
            end
        end
    end

    @testset "Additional tests for $elty" for elty in (ComplexF32, ComplexF64)
        A5 = convert(Matrix{elty}, [1im 2; 0.02+0.5im 3])

        @test sincos(A5) == (sin(A5), cos(A5))

        @test cos(A5)^2 + sin(A5)^2 ≈ Matrix(I, size(A5))
        @test cosh(A5)^2 - sinh(A5)^2 ≈ Matrix(I, size(A5))
        @test cos(A5)^2 + sin(A5)^2 ≈ Matrix(I, size(A5))
        @test tan(A5) ≈ sin(A5) / cos(A5)
        @test tanh(A5) ≈ sinh(A5) / cosh(A5)

        @test sec(A5) ≈ inv(cos(A5))
        @test csc(A5) ≈ inv(sin(A5))
        @test cot(A5) ≈ inv(tan(A5))
        @test sech(A5) ≈ inv(cosh(A5))
        @test csch(A5) ≈ inv(sinh(A5))
        @test coth(A5) ≈ inv(tanh(A5))

        @test cos(A5) ≈ 0.5 * (exp(im*A5) + exp(-im*A5))
        @test sin(A5) ≈ -0.5im * (exp(im*A5) - exp(-im*A5))
        @test cosh(A5) ≈ 0.5 * (exp(A5) + exp(-A5))
        @test sinh(A5) ≈ 0.5 * (exp(A5) - exp(-A5))
    end

    @testset "Additional tests for $elty" for elty in (Int32, Int64, Complex{Int32}, Complex{Int64})
        A1 = convert(Matrix{elty}, [1 2; 3 4])
        A2 = convert(Matrix{elty}, [1 2; 2 1])

        cosA1 = convert(Matrix{float(elty)}, [0.855423165077998 -0.11087638101074865;
                                              -0.16631457151612294 0.689108593561875])
        cosA2 = convert(Matrix{float(elty)}, [-0.22484509536615283 -0.7651474012342925;
                                              -0.7651474012342925 -0.22484509536615283])

        @test cos(A1) ≈ cosA1
        @test cos(A2) ≈ cosA2

        sinA1 = convert(Matrix{float(elty)}, [-0.46558148631373036 -0.14842445991317652;
                                              -0.22263668986976476 -0.6882181761834951])
        sinA2 = convert(Matrix{float(elty)}, [-0.3501754883740146 0.4912954964338818;
                                              0.4912954964338818 -0.3501754883740146])

        @test sin(A1) ≈ sinA1
        @test sin(A2) ≈ sinA2
    end

    @testset "Inverse functions for $elty" for elty in (Float32, Float64)
        A1 = convert(Matrix{elty}, [0.244637  -0.63578;
                                    0.22002    0.189026])
        A2 = convert(Matrix{elty}, [1.11656   -0.098672   0.158485;
                                    -0.098672   0.100933  -0.107107;
                                    0.158485  -0.107107   0.612404])

        for A in (A1, A2)
            @test cos(acos(cos(A))) ≈ cos(A)
            @test sin(asin(sin(A))) ≈ sin(A)
            @test tan(atan(tan(A))) ≈ tan(A)
            @test cosh(acosh(cosh(A))) ≈ cosh(A)
            @test sinh(asinh(sinh(A))) ≈ sinh(A)
            @test tanh(atanh(tanh(A))) ≈ tanh(A)
            @test sec(asec(sec(A))) ≈ sec(A)
            @test csc(acsc(csc(A))) ≈ csc(A)
            @test cot(acot(cot(A))) ≈ cot(A)
            @test sech(asech(sech(A))) ≈ sech(A)
            @test csch(acsch(csch(A))) ≈ csch(A)
            @test coth(acoth(coth(A))) ≈ coth(A)
        end
    end

    @testset "Inverse functions for $elty" for elty in (ComplexF32, ComplexF64)
        A1 = convert(Matrix{elty}, [ 0.143721-0.0im       -0.138386-0.106905im;
                                     -0.138386+0.106905im   0.306224-0.0im])
        A2 = convert(Matrix{elty}, [1im 2; 0.02+0.5im 3])
        A3 = convert(Matrix{elty}, [0.138721-0.266836im 0.0971722-0.13715im 0.205046-0.137136im;
                                    -0.0154974-0.00358254im 0.152163-0.445452im 0.0314575-0.536521im;
                                    -0.387488+0.0294059im -0.0448773+0.114305im 0.230684-0.275894im])
        for A in (A1, A2, A3)
            @test cos(acos(cos(A))) ≈ cos(A)
            @test sin(asin(sin(A))) ≈ sin(A)
            @test tan(atan(tan(A))) ≈ tan(A)
            @test cosh(acosh(cosh(A))) ≈ cosh(A)
            @test sinh(asinh(sinh(A))) ≈ sinh(A)
            @test tanh(atanh(tanh(A))) ≈ tanh(A)
            @test sec(asec(sec(A))) ≈ sec(A)
            @test csc(acsc(csc(A))) ≈ csc(A)
            @test cot(acot(cot(A))) ≈ cot(A)
            @test sech(asech(sech(A))) ≈ sech(A)
            @test csch(acsch(csch(A))) ≈ csch(A)
            @test coth(acoth(coth(A))) ≈ coth(A)

            # Definition of principal values (Aprahamian & Higham, 2016, pp. 4-5)
            abstol = sqrt(eps(real(elty))) * norm(acosh(A))
            @test all(z -> (0 < real(z) < π ||
                            abs(real(z)) < abstol && imag(z) >= 0 ||
                            abs(real(z) - π) < abstol && imag(z) <= 0),
                      eigen(acos(A)).values)
            @test all(z -> (-π/2 < real(z) < π/2 ||
                            abs(real(z) + π/2) < abstol && imag(z) >= 0 ||
                            abs(real(z) - π/2) < abstol && imag(z) <= 0),
                      eigen(asin(A)).values)
            @test all(z -> (-π < imag(z) < π && real(z) > 0 ||
                            0 <= imag(z) < π && abs(real(z)) < abstol ||
                            abs(imag(z) - π) < abstol && real(z) >= 0),
                      eigen(acosh(A)).values)
            @test all(z -> (-π/2 < imag(z) < π/2 ||
                            abs(imag(z) + π/2) < abstol && real(z) <= 0 ||
                            abs(imag(z) - π/2) < abstol && real(z) <= 0),
                      eigen(asinh(A)).values)
        end
    end
end

@testset "issue 5116" begin
    A9  = [0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
    eA9 = [-0.999786072879326  -0.065407069689389   0.0   0.0
           0.006540706968939  -0.999786072879326   0.0   0.0
           0.0                 0.0                 1.0   0.0
           0.013081413937878  -3.999572145758650   0.0   1.0]
    @test exp(A9) ≈ eA9

    A10  = [ 0. 0. 0. 0. ; 0. 0. -im 0.; 0. im 0. 0.; 0. 0. 0. 0.]
    eA10 = [ 1.0+0.0im   0.0+0.0im                 0.0+0.0im                0.0+0.0im
            0.0+0.0im   1.543080634815244+0.0im   0.0-1.175201193643801im  0.0+0.0im
            0.0+0.0im   0.0+1.175201193643801im   1.543080634815243+0.0im  0.0+0.0im
            0.0+0.0im   0.0+0.0im                 0.0+0.0im                1.0+0.0im]
    @test exp(A10) ≈ eA10
end

@testset "Additional matrix logarithm tests" for elty in (Float64, ComplexF64)
    A11 = convert(Matrix{elty}, [3 2; -5 -3])
    @test exp(log(A11)) ≈ A11

    A12 = convert(Matrix{elty}, [1 -1; 1 -1])
    @test typeof(log(A12)) == Array{ComplexF64, 2}

    A13 = convert(Matrix{elty}, [2 0; 0 2])
    @test typeof(log(A13)) == Array{elty, 2}

    T = elty == Float64 ? Symmetric : Hermitian
    @test typeof(log(T(A13))) == T{elty, Array{elty, 2}}

    A1  = convert(Matrix{elty}, [4 2 0; 1 4 1; 1 1 4])
    logA1 = convert(Matrix{elty}, [1.329661349 0.5302876358 -0.06818951543;
                                    0.2310490602 1.295566591 0.2651438179;
                                    0.2310490602 0.1969543025 1.363756107])
    @test log(A1) ≈ logA1
    @test exp(log(A1)) ≈ A1

    A4  = convert(Matrix{elty}, [1/2 1/3 1/4 1/5+eps();
                                 1/3 1/4 1/5 1/6;
                                 1/4 1/5 1/6 1/7;
                                 1/5 1/6 1/7 1/8])
    logA4 = convert(Matrix{elty}, [-1.73297159 1.857349738 0.4462766564 0.2414170219;
                                    1.857349738 -5.335033737 2.994142974 0.5865285289;
                                    0.4462766564 2.994142974 -7.351095988 3.318413247;
                                    0.2414170219 0.5865285289 3.318413247 -5.444632124])
    @test log(A4) ≈ logA4
    @test exp(log(A4)) ≈ A4
end

@testset "issue #7181" begin
    A = [ 1  5  9
          2  6 10
          3  7 11
          4  8 12 ]
    @test diag(A,-5) == []
    @test diag(A,-4) == []
    @test diag(A,-3) == [4]
    @test diag(A,-2) == [3,8]
    @test diag(A,-1) == [2,7,12]
    @test diag(A, 0) == [1,6,11]
    @test diag(A, 1) == [5,10]
    @test diag(A, 2) == [9]
    @test diag(A, 3) == []
    @test diag(A, 4) == []

    @test diag(zeros(0,0)) == []
    @test diag(zeros(0,0),1) == []
    @test diag(zeros(0,0),-1) == []

    @test diag(zeros(1,0)) == []
    @test diag(zeros(1,0),-1) == []
    @test diag(zeros(1,0),1) == []
    @test diag(zeros(1,0),-2) == []

    @test diag(zeros(0,1)) == []
    @test diag(zeros(0,1),1) == []
    @test diag(zeros(0,1),-1) == []
    @test diag(zeros(0,1),2) == []
end

@testset "Matrix to real power" for elty in (Float64, ComplexF64)
# Tests proposed at Higham, Deadman: Testing Matrix Function Algorithms Using Identities, March 2014
    #Aa : only positive real eigenvalues
    Aa = convert(Matrix{elty}, [5 4 2 1; 0 1 -1 -1; -1 -1 3 0; 1 1 -1 2])

    #Ab : both positive and negative real eigenvalues
    Ab = convert(Matrix{elty}, [1 2 3; 4 7 1; 2 1 4])

    #Ac : complex eigenvalues
    Ac = convert(Matrix{elty}, [5 4 2 1;0 1 -1 -1;-1 -1 3 6;1 1 -1 5])

    #Ad : defective Matrix
    Ad = convert(Matrix{elty}, [3 1; 0 3])

    #Ah : Hermitian Matrix
    Ah = convert(Matrix{elty}, [3 1; 1 3])
    if elty <: LinearAlgebra.BlasComplex
        Ah += [0 im; -im 0]
    end

    #ADi : Diagonal Matrix
    ADi = convert(Matrix{elty}, [3 0; 0 3])
    if elty <: LinearAlgebra.BlasComplex
        ADi += [im 0; 0 im]
    end

    for A in (Aa, Ab, Ac, Ad, Ah, ADi)
        @test A^(1/2) ≈ sqrt(A)
        @test A^(-1/2) ≈ inv(sqrt(A))
        @test A^(3/4) ≈ sqrt(A) * sqrt(sqrt(A))
        @test A^(-3/4) ≈ inv(A) * sqrt(sqrt(A))
        @test A^(17/8) ≈ A^2 * sqrt(sqrt(sqrt(A)))
        @test A^(-17/8) ≈ inv(A^2 * sqrt(sqrt(sqrt(A))))
        @test (A^0.2)^5 ≈ A
        @test (A^(2/3))*(A^(1/3)) ≈ A
        @test (A^im)^(-im) ≈ A
    end
end

@testset "diagonal integer matrix to real power" begin
    A = Matrix(Diagonal([1, 2, 3]))
    @test A^2.3 ≈ float(A)^2.3
end

@testset "issue #23366 (Int Matrix to Int power)" begin
    @testset "Tests for $elty" for elty in (Int128, Int16, Int32, Int64, Int8,
                                            UInt128, UInt16, UInt32, UInt64, UInt8,
                                            BigInt)
        #@info "Testing $elty"
        @test elty[1 1;1 0]^-1 == [0  1;  1 -1]
        @test elty[1 1;1 0]^-2 == [1 -1; -1  2]
        @test (@inferred elty[1 1;1 0]^2) == elty[2 1;1 1]
        I_ = elty[1 0;0 1]
        @test I_^-1 == I_
        if !(elty<:Unsigned)
            @test (@inferred (-I_)^-1) == -I_
            @test (@inferred (-I_)^-2) == I_
        end
        # make sure that type promotion for ^(::Matrix{<:Integer}, ::Integer)
        # is analogous to type promotion for ^(::Integer, ::Integer)
        # e.g. [1 1;1 0]^big(10000) should return Matrix{BigInt}, the same
        # way as 2^big(10000) returns BigInt
        for elty2 = (Int64, BigInt)
            TT = Base.promote_op(^, elty, elty2)
            @test (@inferred elty[1 1;1 0]^elty2(1))::Matrix{TT} == [1 1;1 0]
        end
    end
end

@testset "Least squares solutions" begin
    a = [fill(1, 20) 1:20 1:20]
    b = reshape(Matrix(1.0I, 8, 5), 20, 2)
    @testset "Tests for type $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
        a = convert(Matrix{elty}, a)
        b = convert(Matrix{elty}, b)

        # Vector rhs
        x = a[:,1:2]\b[:,1]
        @test ((a[:,1:2]*x-b[:,1])'*(a[:,1:2]*x-b[:,1]))[1] ≈ convert(elty, 2.546616541353384)

        # Matrix rhs
        x = a[:,1:2]\b
        @test det((a[:,1:2]*x-b)'*(a[:,1:2]*x-b)) ≈ convert(elty, 4.437969924812031)

        # Rank deficient
        x = a\b
        @test det((a*x-b)'*(a*x-b)) ≈ convert(elty, 4.437969924812031)

        # Underdetermined minimum norm
        x = convert(Matrix{elty}, [1 0 0; 0 1 -1]) \ convert(Vector{elty}, [1,1])
        @test x ≈ convert(Vector{elty}, [1, 0.5, -0.5])

        # symmetric, positive definite
        @test inv(convert(Matrix{elty}, [6. 2; 2 1])) ≈ convert(Matrix{elty}, [0.5 -1; -1 3])

        # symmetric, indefinite
        @test inv(convert(Matrix{elty}, [1. 2; 2 1])) ≈ convert(Matrix{elty}, [-1. 2; 2 -1]/3)
    end
end

function test_rdiv_pinv_consistency(a, b)
    @test (a*b)/b ≈ a*(b/b) ≈ (a*b)*pinv(b) ≈ a*(b*pinv(b))
    @test typeof((a*b)/b) == typeof(a*(b/b)) == typeof((a*b)*pinv(b)) == typeof(a*(b*pinv(b)))
end
function test_ldiv_pinv_consistency(a, b)
    @test a\(a*b) ≈ (a\a)*b ≈ (pinv(a)*a)*b ≈ pinv(a)*(a*b)
    @test typeof(a\(a*b)) == typeof((a\a)*b) == typeof((pinv(a)*a)*b) == typeof(pinv(a)*(a*b))
end
function test_div_pinv_consistency(a, b)
    test_rdiv_pinv_consistency(a, b)
    test_ldiv_pinv_consistency(a, b)
end

@testset "/ and \\ consistency with pinv for vectors" begin
    @testset "Tests for type $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
        c = rand(elty, 5)
        r = (elty <: Complex ? adjoint : transpose)(rand(elty, 5))
        cm = rand(elty, 5, 1)
        rm = rand(elty, 1, 5)
        @testset "dot products" begin
            test_div_pinv_consistency(r, c)
            test_div_pinv_consistency(rm, c)
            test_div_pinv_consistency(r, cm)
            test_div_pinv_consistency(rm, cm)
        end
        @testset "outer products" begin
            test_div_pinv_consistency(c, r)
            test_div_pinv_consistency(cm, rm)
        end
        @testset "matrix/vector" begin
            m = rand(5, 5)
            test_ldiv_pinv_consistency(m, c)
            test_rdiv_pinv_consistency(r, m)
        end
    end
end

@testset "test ops on Numbers for $elty" for elty in [Float32,Float64,ComplexF32,ComplexF64]
    a = rand(elty)
    @test isposdef(one(elty))
    @test lyap(one(elty),a) == -a/2
end

@testset "strides" begin
    a = rand(10)
    b = view(a,2:2:10)
    @test LinearAlgebra.stride1(a) == 1
    @test LinearAlgebra.stride1(b) == 2
end

@testset "inverse of Adjoint" begin
    A = randn(n, n)

    @test @inferred(inv(A'))*A'                     ≈ I
    @test @inferred(inv(transpose(A)))*transpose(A) ≈ I

    B = complex.(A, randn(n, n))

    @test @inferred(inv(B'))*B'                     ≈ I
    @test @inferred(inv(transpose(B)))*transpose(B) ≈ I
end

@testset "Factorize fallback for Adjoint/Transpose" begin
    a = rand(Complex{Int8}, n, n)
    @test Array(transpose(factorize(Transpose(a)))) ≈ Array(factorize(a))
    @test transpose(factorize(transpose(a))) == factorize(a)
    @test Array(adjoint(factorize(Adjoint(a)))) ≈ Array(factorize(a))
    @test adjoint(factorize(adjoint(a))) == factorize(a)
end

@testset "Matrix log issue #32313" begin
    for A in ([30 20; -50 -30], [10.0im 0; 0 -10.0im], randn(6,6))
        @test exp(log(A)) ≈ A
    end
end

@testset "Matrix log PR #33245" begin
    # edge case for divided difference
    A1 = triu(ones(3,3),1) + diagm([1.0, -2eps()-1im, -eps()+0.75im])
    @test exp(log(A1)) ≈ A1
    # case where no sqrt is needed (s=0)
    A2 = [1.01 0.01 0.01; 0 1.01 0.01; 0 0 1.01]
    @test exp(log(A2)) ≈ A2
end

struct TypeWithoutZero end
Base.zero(::Type{TypeWithoutZero}) = TypeWithZero()
struct TypeWithZero end
Base.promote_rule(::Type{TypeWithoutZero}, ::Type{TypeWithZero}) = TypeWithZero
Base.zero(::Type{<:Union{TypeWithoutZero, TypeWithZero}}) = TypeWithZero()
Base.:+(x::TypeWithZero, ::TypeWithoutZero) = x

@testset "diagm for type with no zero" begin
    @test diagm(0 => [TypeWithoutZero()]) isa Matrix{TypeWithZero}
end

end # module TestDense
