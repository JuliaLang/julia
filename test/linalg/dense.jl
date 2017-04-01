# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

@testset "Check that non-floats are correctly promoted" begin
    @test [1 0 0; 0 1 0]\[1,1] ≈ [1;1;0]
end

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

@testset "Matrix condition number" begin
    ainit = rand(n,n)
    @testset "for $elty" for elty in (Float32, Float64, Complex64, Complex128)
        ainit = convert(Matrix{elty}, ainit)
        for arraytype in ("Array", "SubArray")
            if arraytype == "Array"
                a = ainit
            else
                a = view(ainit, 1:n, 1:n)
            end
            @test cond(a,1) ≈ 4.837320054554436e+02 atol=0.01
            @test cond(a,2) ≈ 1.960057871514615e+02 atol=0.01
            @test cond(a,Inf) ≈ 3.757017682707787e+02 atol=0.01
            @test cond(a[:,1:5]) ≈ 10.233059337453463 atol=0.01
            @test_throws ArgumentError cond(a,3)
        end
    end
end

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

@testset "For A containing $eltya" for eltya in (Float32, Float64, Complex64, Complex128, Int)
    ainit = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    ainit2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    ε = εa = eps(abs(float(one(eltya))))

    apd  = ainit'*ainit # symmetric positive-definite
    @testset "Positive definiteness" begin
        @test isposdef(apd,:U)
    end
    @testset "For b containing $eltyb" for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        binit = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        for arraytype in ("Array", "SubArray")
            if arraytype == "Array"
                a = ainit
                b = binit
            else
                a = view(ainit, 1:n, 1:n)
                b = view(binit, 1:n, 1:2)
            end

            @testset "Solve square general system of equations" begin
                κ = cond(a,1)
                x = a \ b
                @test_throws DimensionMismatch b'\b
                @test_throws DimensionMismatch b\b'
                @test norm(a*x - b, 1)/norm(b) < ε*κ*n*2 # Ad hoc, revisit!
                @test zeros(eltya,n)\ones(eltya,n) ≈ zeros(eltya,n,1)\ones(eltya,n,1)
            end

            @testset "Test nullspace" begin
                a15null = nullspace(a[:,1:n1]')
                @test rank([a[:,1:n1] a15null]) == 10
                @test norm(a[:,1:n1]'a15null,Inf) ≈ zero(eltya) atol=300ε
                @test norm(a15null'a[:,1:n1],Inf) ≈ zero(eltya) atol=400ε
                @test size(nullspace(b), 2) == 0
                @test nullspace(zeros(eltya,n)) == eye(eltya,1)
            end
        end
    end # for eltyb

    for arraytype in ("Array", "SubArray")
        if arraytype == "Array"
            a = ainit
            a2 = ainit2
        else
            a = view(ainit, 1:n, 1:n)
            a2 = view(ainit2, 1:n, 1:n)
        end

        @testset "Test pinv" begin
            pinva15 = pinv(a[:,1:n1])
            @test a[:,1:n1]*pinva15*a[:,1:n1] ≈ a[:,1:n1]
            @test pinva15*a[:,1:n1]*pinva15 ≈ pinva15

            @test size(pinv(ones(eltya,0,0))) == (0,0)
        end

        @testset "Lyapunov/Sylvester" begin
            x = lyap(a, a2)
            @test -a2 ≈ a*x + x*a'
            x2 = sylvester(a[1:3, 1:3], a[4:n, 4:n], a2[1:3,4:n])
            @test -a2[1:3, 4:n] ≈ a[1:3, 1:3]*x2 + x2*a[4:n, 4:n]
        end

        @testset "Matrix square root" begin
            asq = sqrtm(a)
            @test asq*asq ≈ a
            asym = a'+a # symmetric indefinite
            asymsq = sqrtm(asym)
            @test asymsq*asymsq ≈ asym
        end

        @testset "Powers" begin
            if eltya <: AbstractFloat
                z = zero(eltya)
                t = convert(eltya,2)
                r = convert(eltya,2.5)
                @test a^z ≈ eye(a)
                @test a^t ≈ a^2
                @test eye(eltya,n,n)^r ≈ eye(a)
            end
        end
    end # end for loop over arraytype

    @testset "Numbers" begin
        α = rand(eltya)
        A = zeros(eltya,1,1)
        A[1,1] = α
        @test diagm(α) == A # Test behavior of `diagm` when passed a scalar
        @test expm(α) == exp(α) # `expm` should behave like `exp` with scalar argument
    end

    @testset "Factorize" begin
        d = rand(eltya,n)
        e = rand(eltya,n-1)
        e2 = rand(eltya,n-1)
        f = rand(eltya,n-2)
        A = diagm(d)
        @test factorize(A) == Diagonal(d)
        A += diagm(e,-1)
        @test factorize(A) == Bidiagonal(d,e,false)
        A += diagm(f,-2)
        @test factorize(A) == LowerTriangular(A)
        A = diagm(d) + diagm(e,1)
        @test factorize(A) == Bidiagonal(d,e,true)
        if eltya <: Real
            A = diagm(d) + diagm(e,1) + diagm(e,-1)
            @test full(factorize(A)) ≈ full(factorize(SymTridiagonal(d,e)))
            A = diagm(d) + diagm(e,1) + diagm(e,-1) + diagm(f,2) + diagm(f,-2)
            @test inv(factorize(A)) ≈ inv(factorize(Symmetric(A)))
        end
        A = diagm(d) + diagm(e,1) + diagm(e2,-1)
        @test full(factorize(A)) ≈ full(factorize(Tridiagonal(e2,d,e)))
        A = diagm(d) + diagm(e,1) + diagm(f,2)
        @test factorize(A) == UpperTriangular(A)
    end
end # for eltya

@testset "test triu/tril bounds checking" begin
    ainit = rand(5,7)
    for arraytype in ("Array", "SubArray")
        if arraytype == "Array"
            a = ainit
        else
            a = view(ainit, 1:size(ainit, 1), 1:size(ainit, 2))
        end
        @test_throws(ArgumentError,triu(a,8))
        @test_throws(ArgumentError,triu(a,-6))
        @test_throws(ArgumentError,tril(a,8))
        @test_throws(ArgumentError,tril(a,-6))
    end
end

@testset "Test gradient for $elty" for elty in (Int32, Int64, Float32, Float64, Complex64, Complex128)
    if elty <: Real
        x = convert(Vector{elty}, [1:3;])
        g = ones(elty, 3)
    else
        x = convert(Vector{elty}, complex.([1:3;], [1:3;]))
        g = convert(Vector{elty}, complex.(ones(3), ones(3)))
    end
    xsub = view(x, 1:size(x, 1))
    @test gradient(x) ≈ g
    @test gradient(xsub) ≈ g # Test gradient on SubArray
    @test gradient(ones(elty,1)) == zeros(elty,1)
end

@testset "Tests norms" begin
    nnorm = 10
    mmat = 10
    nmat = 8
    @testset "For $elty" for elty in (Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat}, Int32, Int64, BigInt)
        x = ones(elty,10)
        @testset "Vector" begin
            xs = view(x,1:2:10)
            @test norm(x, -Inf) ≈ 1
            @test norm(x, -1) ≈ 1/10
            @test norm(x, 0) ≈ 10
            @test norm(x, 1) ≈ 10
            @test norm(x, 2) ≈ sqrt(10)
            @test norm(x, 3) ≈ cbrt(10)
            @test norm(x, Inf) ≈ 1
            if elty <: Base.LinAlg.BlasFloat
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
                for arraytype in ("Array", "SubArray")
                    if arraytype == "Array"
                        x = xinit
                        y = yinit
                    else
                        x = view(xinit,1:2:nnorm)
                        y = view(yinit,1:2:nnorm)
                    end
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
                    @test norm(x,-1) ≈ inv(sum(1./abs.(x)))
                    @test norm(x,0) ≈ sum(x .!= 0)
                    @test norm(x,1) ≈ sum(abs.(x))
                    @test norm(x) ≈ sqrt(sum(abs2.(x)))
                    @test norm(x,3) ≈ cbrt(sum(abs.(x).^3.))
                    @test norm(x,Inf) ≈ maximum(abs.(x))
                end
            end
        end

        @testset "Matrix (Operator)" begin
            A = ones(elty,10,10)
            As = view(A,1:5,1:5)
            @test norm(A, 1) ≈ 10
            elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(A, 2) ≈ 10
            @test norm(A, Inf) ≈ 10
            @test norm(As, 1) ≈ 5
            elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(As, 2) ≈ 5
            @test norm(As, Inf) ≈ 5
        end

        @testset "Absolute homogeneity, triangle inequality, & vecnorm" begin
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

                for arraytype in ("Array", "SubArray")
                    if arraytype == "Array"
                        A = Ainit
                        B = Binit
                    else
                        A = view(Ainit,1:nmat,1:nmat)
                        B = view(Binit,1:nmat,1:nmat)
                    end

                    # Absolute homogeneity
                    @test norm(α*A,1) ≈ abs(α)*norm(A,1)
                    elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(α*A) ≈ abs(α)*norm(A) # two is default
                    @test norm(α*A,Inf) ≈ abs(α)*norm(A,Inf)

                    # Triangle inequality
                    @test norm(A + B,1) <= norm(A,1) + norm(B,1)
                    elty <: Union{BigFloat,Complex{BigFloat},BigInt} || @test norm(A + B) <= norm(A) + norm(B) # two is default
                    @test norm(A + B,Inf) <= norm(A,Inf) + norm(B,Inf)

                    # vecnorm:
                    for p = -2:3
                        @test norm(reshape(A, length(A)), p) == vecnorm(A, p)
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
    A = [1.+0.0im 0; 0 1]
    B = pinv(A)
    for i = 1:4
        @test A[i] ≈ B[i]
    end
end

@testset "issue #2246" begin
    A = [1 2 0 0; 0 1 0 0; 0 0 0 0; 0 0 0 0]
    Asq = sqrtm(A)
    @test Asq*Asq ≈ A
    A2 = view(A, 1:2, 1:2)
    A2sq = sqrtm(A2)
    @test A2sq*A2sq ≈ A2

    N = 3
    @test log(det(eye(N))) ≈ logdet(eye(N))
end

@testset "issue #2637" begin
    a = [1, 2, 3]
    b = [4, 5, 6]
    @test kron(eye(2),eye(2)) == eye(4)
    @test kron(a,b) == [4,5,6,8,10,12,12,15,18]
    @test kron(a',b') == [4 5 6 8 10 12 12 15 18]
    @test kron(a,b')  == [4 5 6; 8 10 12; 12 15 18]
    @test kron(a',b)  == [4 8 12; 5 10 15; 6 12 18]
    @test kron(a,eye(2)) == [1 0; 0 1; 2 0; 0 2; 3 0; 0 3]
    @test kron(eye(2),a) == [ 1 0; 2 0; 3 0; 0 1; 0 2; 0 3]
    @test kron(eye(2),2) == 2*eye(2)
    @test kron(3,eye(3)) == 3*eye(3)
    @test kron(a,2) == [2, 4, 6]
    @test kron(b',2) == [8 10 12]
end

@testset "issue #4796" begin
    dim=2
    S=zeros(Complex,dim,dim)
    T=zeros(Complex,dim,dim)
    T[:] = 1
    z = 2.5 + 1.5im
    S[1] = z
    @test S*T == [z z; 0 0]

    # similar issue for Array{Real}
    @test Real[1 2] * Real[1.5; 2.0] == Real[5.5]
end

@testset "Matrix exponential" begin
    @testset "Tests for $elty" for elty in (Float32, Float64, Complex64, Complex128)
        A1  = convert(Matrix{elty}, [4 2 0; 1 4 1; 1 1 4])
        eA1 = convert(Matrix{elty}, [147.866622446369 127.781085523181  127.781085523182;
                                     183.765138646367 183.765138646366  163.679601723179;
                                     71.797032399996  91.8825693231832 111.968106246371]')
        @test expm(A1) ≈ eA1

        A2  = convert(Matrix{elty},
                      [29.87942128909879    0.7815750847907159 -2.289519314033932;
                       0.7815750847907159 25.72656945571064    8.680737820540137;
                       -2.289519314033932   8.680737820540137  34.39400925519054])
        eA2 = convert(Matrix{elty},
                      [  5496313853692458.0 -18231880972009236.0 -30475770808580460.0;
                       -18231880972009252.0  60605228702221920.0 101291842930249760.0;
                       -30475770808580480.0 101291842930249728.0 169294411240851968.0])
        @test expm(A2) ≈ eA2

        A3  = convert(Matrix{elty}, [-131 19 18;-390 56 54;-387 57 52])
        eA3 = convert(Matrix{elty}, [-1.50964415879218 -5.6325707998812  -4.934938326092;
                                     0.367879439109187 1.47151775849686  1.10363831732856;
                                     0.135335281175235 0.406005843524598 0.541341126763207]')
        @test expm(A3) ≈ eA3

        A4 = convert(Matrix{elty}, [0.25 0.25; 0 0])
        eA4 = convert(Matrix{elty}, [1.2840254166877416 0.2840254166877415; 0 1])
        @test expm(A4) ≈ eA4

        A5 = convert(Matrix{elty}, [0 0.02; 0 0])
        eA5 = convert(Matrix{elty}, [1 0.02; 0 1])
        @test expm(A5) ≈ eA5

        # Hessenberg
        @test hessfact(A1)[:H] ≈ convert(Matrix{elty},
                                                 [4.000000000000000  -1.414213562373094  -1.414213562373095
                                                  -1.414213562373095   4.999999999999996  -0.000000000000000
                                                  0  -0.000000000000002   3.000000000000000])
    end

    @testset "Additional tests for $elty" for elty in (Float64, Complex{Float64})
        A4  = convert(Matrix{elty}, [1/2 1/3 1/4 1/5+eps();
                                     1/3 1/4 1/5 1/6;
                                     1/4 1/5 1/6 1/7;
                                     1/5 1/6 1/7 1/8])
        @test expm(logm(A4)) ≈ A4

        A5  = convert(Matrix{elty}, [1 1 0 1; 0 1 1 0; 0 0 1 1; 1 0 0 1])
        @test expm(logm(A5)) ≈ A5

        A6  = convert(Matrix{elty}, [-5 2 0 0 ; 1/2 -7 3 0; 0 1/3 -9 4; 0 0 1/4 -11])
        @test expm(logm(A6)) ≈ A6

        A7  = convert(Matrix{elty}, [1 0 0 1e-8; 0 1 0 0; 0 0 1 0; 0 0 0 1])
        @test expm(logm(A7)) ≈ A7
    end

    A8 = 100 * [-1+1im 0 0 1e-8; 0 1 0 0; 0 0 1 0; 0 0 0 1]
    @test expm(logm(A8)) ≈ A8
end

@testset "issue 5116" begin
    A9  = [0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
    eA9 = [-0.999786072879326  -0.065407069689389   0.0   0.0
           0.006540706968939  -0.999786072879326   0.0   0.0
           0.0                 0.0                 1.0   0.0
           0.013081413937878  -3.999572145758650   0.0   1.0]
    @test expm(A9) ≈ eA9

    A10  = [ 0. 0. 0. 0. ; 0. 0. -im 0.; 0. im 0. 0.; 0. 0. 0. 0.]
    eA10 = [ 1.0+0.0im   0.0+0.0im                 0.0+0.0im                0.0+0.0im
            0.0+0.0im   1.543080634815244+0.0im   0.0-1.175201193643801im  0.0+0.0im
            0.0+0.0im   0.0+1.175201193643801im   1.543080634815243+0.0im  0.0+0.0im
            0.0+0.0im   0.0+0.0im                 0.0+0.0im                1.0+0.0im]
    @test expm(A10) ≈ eA10
end

@testset "issue #7181" begin
    A = [ 1  5  9
          2  6 10
          3  7 11
          4  8 12 ]
    @test_throws ArgumentError diag(A, -5)
    @test diag(A,-4) == []
    @test diag(A,-3) == [4]
    @test diag(A,-2) == [3,8]
    @test diag(A,-1) == [2,7,12]
    @test diag(A, 0) == [1,6,11]
    @test diag(A, 1) == [5,10]
    @test diag(A, 2) == [9]
    @test diag(A, 3) == []
    @test_throws ArgumentError diag(A, 4)

    @test diag(zeros(0,0)) == []
    @test_throws ArgumentError diag(zeros(0,0),1)
    @test_throws ArgumentError diag(zeros(0,0),-1)

    @test diag(zeros(1,0)) == []
    @test diag(zeros(1,0),-1) == []
    @test_throws ArgumentError diag(zeros(1,0),1)
    @test_throws ArgumentError diag(zeros(1,0),-2)

    @test diag(zeros(0,1)) == []
    @test diag(zeros(0,1),1) == []
    @test_throws ArgumentError diag(zeros(0,1),-1)
    @test_throws ArgumentError diag(zeros(0,1),2)
end

@testset "Least squares solutions" begin
    a = [ones(20) 1:20 1:20]
    b = reshape(eye(8, 5), 20, 2)
    @testset "Tests for type $elty" for elty in (Float32, Float64, Complex64, Complex128)
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

@testset "test ops on Numbers for $elty" for elty in [Float32,Float64,Complex64,Complex128]
    a = rand(elty)
    @test expm(a) == exp(a)
    @test isposdef(one(elty))
    @test sqrtm(a) == sqrt(a)
    @test logm(a) ≈ log(a)
    @test lyap(one(elty),a) == -a/2
end

@testset "stride1" begin
    a = rand(10)
    b = view(a,2:2:10)
    @test Base.LinAlg.stride1(b) == 2
end
