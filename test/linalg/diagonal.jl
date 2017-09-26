# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import Base.LinAlg: BlasFloat, BlasComplex, SingularException, A_rdiv_B!, A_rdiv_Bt!,
    A_rdiv_Bc!

n=12 #Size of matrix problem to test
srand(1)

@testset for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    dd=convert(Vector{elty}, randn(n))
    vv=convert(Vector{elty}, randn(n))
    UU=convert(Matrix{elty}, randn(n,n))
    if elty <: Complex
        dd+=im*convert(Vector{elty}, randn(n))
        vv+=im*convert(Vector{elty}, randn(n))
        UU+=im*convert(Matrix{elty}, randn(n,n))
    end
    D = Diagonal(dd)
    DM = diagm(dd)

    @testset "constructor" begin
        for x in (dd, GenericArray(dd))
            @test Diagonal(x)::Diagonal{elty,typeof(x)} == DM
            @test Diagonal(x).diag === x
            @test Diagonal{elty}(x)::Diagonal{elty,typeof(x)} == DM
            @test Diagonal{elty}(x).diag === x
        end
    end

    @testset "Basic properties" begin
        @test eye(Diagonal{elty},n) == Diagonal(ones(elty,n))
        @test_throws ArgumentError size(D,0)
        @test typeof(convert(Diagonal{Complex64},D)) <: Diagonal{Complex64}
        @test typeof(convert(AbstractMatrix{Complex64},D)) <: Diagonal{Complex64}

        @test Array(real(D)) == real(DM)
        @test Array(abs.(D)) == abs.(DM)
        @test Array(imag(D)) == imag(DM)

        @test parent(D) == dd
        @test diag(D) == dd
        @test D[1,1] == dd[1]
        @test D[1,2] == 0

        @test issymmetric(D)
        @test istriu(D)
        @test istril(D)
        if elty <: Real
            @test ishermitian(D)
        end
    end

    @testset "Simple unary functions" begin
        for op in (-,)
            @test op(D)==op(DM)
        end

        for func in (det, trace)
            @test func(D) ≈ func(DM) atol=n^2*eps(relty)*(1+(elty<:Complex))
        end
        if relty <: BlasFloat
            for func in (exp,)
                @test func(D) ≈ func(DM) atol=n^3*eps(relty)
            end
            @test log(Diagonal(abs.(D.diag))) ≈ log(abs.(DM)) atol=n^3*eps(relty)
        end
        if elty <: BlasComplex
            for func in (logdet, sqrt)
                @test func(D) ≈ func(DM) atol=n^2*eps(relty)*2
            end
        end
    end

    @testset "Linear solve" begin
        for (v, U) in ((vv, UU), (view(vv, 1:n), view(UU, 1:n, 1:2)))
            @test D*v ≈ DM*v atol=n*eps(relty)*(1+(elty<:Complex))
            @test D*U ≈ DM*U atol=n^2*eps(relty)*(1+(elty<:Complex))

            @test U.'*D ≈ U.'*Array(D)
            @test U'*D ≈ U'*Array(D)

            if relty != BigFloat
                atol_two = 2n^2 * eps(relty) * (1 + (elty <: Complex))
                atol_three = 2n^3 * eps(relty) * (1 + (elty <: Complex))
                @test D\v ≈ DM\v atol=atol_two
                @test D\U ≈ DM\U atol=atol_three
                @test A_ldiv_B!(D, copy(v)) ≈ DM\v atol=atol_two
                @test At_ldiv_B!(D, copy(v)) ≈ DM\v atol=atol_two
                @test Ac_ldiv_B!(conj(D), copy(v)) ≈ DM\v atol=atol_two
                @test A_ldiv_B!(D, copy(U)) ≈ DM\U atol=atol_three
                @test At_ldiv_B!(D, copy(U)) ≈ DM\U atol=atol_three
                @test Ac_ldiv_B!(conj(D), copy(U)) ≈ DM\U atol=atol_three
                Uc = adjoint(U)
                target = scale!(Uc, inv.(D.diag))
                @test A_rdiv_B!(Uc, D) ≈ target atol=atol_three
                @test_throws DimensionMismatch A_rdiv_B!(eye(elty, n-1), D)
                @test_throws SingularException A_rdiv_B!(Uc, zeros(D))
                @test A_rdiv_Bt!(Uc, D) ≈ target atol=atol_three
                @test A_rdiv_Bc!(Uc, conj(D)) ≈ target atol=atol_three
                @test A_ldiv_B!(D, eye(D)) ≈ D\eye(D) atol=atol_three
                @test_throws DimensionMismatch A_ldiv_B!(D, ones(elty, n + 1))
                @test_throws SingularException A_ldiv_B!(Diagonal(zeros(relty, n)), copy(v))
                b = rand(elty, n, n)
                b = sparse(b)
                @test A_ldiv_B!(D, copy(b)) ≈ Array(D)\Array(b)
                @test_throws SingularException A_ldiv_B!(Diagonal(zeros(elty, n)), copy(b))
                b = view(rand(elty, n), collect(1:n))
                b2 = copy(b)
                c = A_ldiv_B!(D, b)
                d = Array(D)\b2
                @test c ≈ d
                @test_throws SingularException A_ldiv_B!(Diagonal(zeros(elty, n)), b)
                b = rand(elty, n+1, n+1)
                b = sparse(b)
                @test_throws DimensionMismatch A_ldiv_B!(D, copy(b))
                b = view(rand(elty, n+1), collect(1:n+1))
                @test_throws DimensionMismatch A_ldiv_B!(D, b)
            end
        end
    end
    d = convert(Vector{elty}, randn(n))
    D2 = Diagonal(d)
    DM2= diagm(d)
    @testset "Binary operations" begin
        for op in (+, -, *)
            @test Array(op(D, D2)) ≈ op(DM, DM2)
        end
        @testset "with plain numbers" begin
            a = rand()
            @test Array(a*D) ≈ a*DM
            @test Array(D*a) ≈ DM*a
            @test Array(D/a) ≈ DM/a
            if relty <: BlasFloat
                b = rand(elty,n,n)
                b = sparse(b)
                @test A_mul_B!(copy(D), copy(b)) ≈ Array(D)*Array(b)
                @test At_mul_B!(copy(D), copy(b)) ≈ Array(D).'*Array(b)
                @test Ac_mul_B!(copy(D), copy(b)) ≈ Array(D)'*Array(b)
            end
        end

        #a few missing mults
        bd = Bidiagonal(D2)
        @test D*D2.' ≈ Array(D)*Array(D2).'
        @test D2*D.' ≈ Array(D2)*Array(D).'
        @test D2*D' ≈ Array(D2)*Array(D)'

        #division of two Diagonals
        @test D/D2 ≈ Diagonal(D.diag./D2.diag)
        @test D\D2 ≈ Diagonal(D2.diag./D.diag)

        # Performance specialisations for A*_mul_B!
        vvv = similar(vv)
        @test (r = full(D) * vv   ; A_mul_B!(vvv, D, vv)  ≈ r ≈ vvv)
        @test (r = full(D)' * vv  ; Ac_mul_B!(vvv, D, vv) ≈ r ≈ vvv)
        @test (r = full(D).' * vv ; At_mul_B!(vvv, D, vv) ≈ r ≈ vvv)

        UUU = similar(UU)
        @test (r = full(D) * UU   ; A_mul_B!(UUU, D, UU) ≈ r ≈ UUU)
        @test (r = full(D)' * UU  ; Ac_mul_B!(UUU, D, UU) ≈ r ≈ UUU)
        @test (r = full(D).' * UU ; At_mul_B!(UUU, D, UU) ≈ r ≈ UUU)

        # make sure that A_mul_B{c,t}! works with B as a Diagonal
        VV = Array(D)
        DD = copy(D)
        r  = VV * full(D)
        @test Array(A_mul_B!(VV, DD)) ≈ r ≈ Array(D)*Array(D)
        DD = copy(D)
        r  = VV * (Array(D).')
        @test Array(A_mul_Bt!(VV, DD)) ≈ r
        DD = copy(D)
        r  = VV * (Array(D)')
        @test Array(A_mul_Bc!(VV, DD)) ≈ r
    end
    @testset "triu/tril" begin
        @test istriu(D)
        @test istril(D)
        @test triu(D,1)  == zeros(D)
        @test triu(D,0)  == D
        @test triu(D,-1) == D
        @test tril(D,1)  == D
        @test tril(D,-1) == zeros(D)
        @test tril(D,0)  == D
        @test_throws ArgumentError tril(D, -n - 2)
        @test_throws ArgumentError tril(D, n)
        @test_throws ArgumentError triu(D, -n)
        @test_throws ArgumentError triu(D, n + 2)
    end

    # factorize
    @test factorize(D) == D

    @testset "Eigensystem" begin
        eigD = eigfact(D)
        @test Diagonal(eigD[:values]) ≈ D
        @test eigD[:vectors] == eye(D)
    end

    @testset "ldiv" begin
        v = rand(n + 1)
        @test_throws DimensionMismatch D\v
        v = rand(n)
        @test D\v ≈ DM\v
        V = rand(n + 1, n)
        @test_throws DimensionMismatch D\V
        V = rand(n, n)
        @test D\V ≈ DM\V
    end

    @testset "conj and transpose" begin
        @test transpose(D) == D
        if elty <: BlasComplex
            @test Array(conj(D)) ≈ conj(DM)
            @test adjoint(D) == conj(D)
        end
        # Translates to Ac/t_mul_B, which is specialized after issue 21286
        @test(D' * vv == conj(D) * vv)
        @test(D.' * vv == D * vv)
    end

    #logdet
    if relty <: Real
        ld=convert(Vector{relty},rand(n))
        @test logdet(Diagonal(ld)) ≈ logdet(diagm(ld))
    end

    @testset "similar" begin
        @test isa(similar(D), Diagonal{elty})
        @test isa(similar(D, Int), Diagonal{Int})
        @test isa(similar(D, (3,2)), Matrix{elty})
        @test isa(similar(D, Int, (3,2)), Matrix{Int})
    end

    # Issue number 10036
    # make sure issymmetric/ishermitian work for
    # non-real diagonal matrices
    @testset "issymmetric/hermitian for complex Diagonal" begin
        @test issymmetric(D2)
        @test ishermitian(D2)
        if elty <: Complex
            dc = d + im*convert(Vector{elty}, ones(n))
            D3 = Diagonal(dc)
            @test issymmetric(D3)
            @test !ishermitian(D3)
        end
    end

    @testset "svd (#11120/#11247)" begin
        U, s, V = svd(D)
        @test (U*Diagonal(s))*V' ≈ D
        @test svdvals(D) == s
        @test svdfact(D)[:V] == V
    end
end

@testset "svdvals and eigvals (#11120/#11247)" begin
    D = Diagonal(Matrix{Float64}[randn(3,3), randn(2,2)])
    @test sort([svdvals(D)...;], rev = true) ≈ svdvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
    @test [eigvals(D)...;] ≈ eigvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
end

@testset "isposdef" begin
    @test isposdef(Diagonal(1.0 .+ rand(n)))
    @test !isposdef(Diagonal(-1.0 * rand(n)))
end

@testset "getindex" begin
    d = randn(n)
    D = Diagonal(d)
    # getindex bounds checking
    @test_throws BoundsError D[0, 0]
    @test_throws BoundsError D[-1, -2]
    @test_throws BoundsError D[n, n + 1]
    @test_throws BoundsError D[n + 1, n]
    @test_throws BoundsError D[n + 1, n + 1]
    # getindex on and off the diagonal
    for i in 1:n, j in 1:n
        @test D[i, j] == (i == j ? d[i] : 0)
    end
end

@testset "setindex!" begin
    d = randn(n)
    D = Diagonal(d)
    # setindex! bounds checking
    @test_throws BoundsError D[0, 0] = 0
    @test_throws BoundsError D[-1 , -2] = 0
    @test_throws BoundsError D[n, n + 1] = 0
    @test_throws BoundsError D[n + 1, n] = 0
    @test_throws BoundsError D[n + 1, n + 1] = 0
    for i in 1:n, j in 1:n
        if i == j
            # setindex on! the diagonal
            @test ((D[i, j] = i) == i; D[i, j] == i)
        else
            # setindex! off the diagonal
            @test ((D[i, j] = 0) == 0; iszero(D[i, j]))
            @test_throws ArgumentError D[i, j] = 1
        end
    end
end

@testset "inverse" begin
    for d in (randn(n), [1, 2, 3], [1im, 2im, 3im])
        D = Diagonal(d)
        @test inv(D) ≈ inv(Array(D))
    end
    @test_throws SingularException inv(Diagonal(zeros(n)))
    @test_throws SingularException inv(Diagonal([0, 1, 2]))
    @test_throws SingularException inv(Diagonal([0im, 1im, 2im]))
end

# allow construct from range
@test all(Diagonal(linspace(1,3,3)) .== Diagonal([1.0,2.0,3.0]))

# Issue 12803
for t in (Float32, Float64, Int, Complex{Float64}, Rational{Int})
    @test Diagonal(Matrix{t}[ones(t, 2, 2), ones(t, 3, 3)])[2,1] == zeros(t, 3, 2)
end

# Issue 15401
@test eye(5) \ Diagonal(ones(5)) == eye(5)

@testset "Triangular and Diagonal" begin
    for T in (LowerTriangular(randn(5,5)), LinAlg.UnitLowerTriangular(randn(5,5)))
        D = Diagonal(randn(5))
        @test T*D   == Array(T)*Array(D)
        @test T'D   == Array(T)'*Array(D)
        @test T.'D  == Array(T).'*Array(D)
        @test D*T'  == Array(D)*Array(T)'
        @test D*T.' == Array(D)*Array(T).'
        @test D*T   == Array(D)*Array(T)
    end
end

let D1 = Diagonal(rand(5)), D2 = Diagonal(rand(5))
    @test_throws MethodError A_mul_B!(D1,D2)
    @test_throws MethodError At_mul_B!(D1,D2)
    @test_throws MethodError Ac_mul_B!(D1,D2)
end

@testset "multiplication of QR Q-factor and Diagonal (#16615 spot test)" begin
    D = Diagonal(randn(5))
    Q = qrfact(randn(5, 5))[:Q]
    @test D * Q' == Array(D) * Q'
    Q = qrfact(randn(5, 5), Val(true))[:Q]
    @test_throws MethodError A_mul_B!(Q, D)
end

@testset "block diagonal matrices" begin
    D = Diagonal([[1 2; 3 4], [1 2; 3 4]])
    Dherm = Diagonal([[1 1+im; 1-im 1], [1 1+im; 1-im 1]])
    Dsym = Diagonal([[1 1+im; 1+im 1], [1 1+im; 1+im 1]])
    @test D' == Diagonal([[1 3; 2 4], [1 3; 2 4]])
    @test D.' == Diagonal([[1 3; 2 4], [1 3; 2 4]])
    @test Dherm' == Dherm
    @test Dherm.' == Diagonal([[1 1-im; 1+im 1], [1 1-im; 1+im 1]])
    @test Dsym' == Diagonal([[1 1-im; 1-im 1], [1 1-im; 1-im 1]])
    @test Dsym.' == Dsym

    v = [[1, 2], [3, 4]]
    @test Dherm' * v == Dherm * v
    @test D.' * v == [[7, 10], [15, 22]]

    @test issymmetric(D) == false
    @test issymmetric(Dherm) == false
    @test issymmetric(Dsym) == true

    @test ishermitian(D) == false
    @test ishermitian(Dherm) == true
    @test ishermitian(Dsym) == false

    @test exp(D) == Diagonal([exp([1 2; 3 4]), exp([1 2; 3 4])])
    @test log(D) == Diagonal([log([1 2; 3 4]), log([1 2; 3 4])])
    @test sqrt(D) == Diagonal([sqrt([1 2; 3 4]), sqrt([1 2; 3 4])])
end

@testset "multiplication with Symmetric/Hermitian" begin
    for T in (Float64, Complex128)
        D = Diagonal(randn(T, n))
        A = randn(T, n, n); A = A'A
        S = Symmetric(A)
        H = Hermitian(A)
        for f in (*, Ac_mul_B, A_mul_Bc, Ac_mul_Bc, At_mul_B, A_mul_Bt, At_mul_Bt)
            @test f(D, S) ≈ f(Matrix(D), Matrix(S))
            @test f(D, H) ≈ f(Matrix(D), Matrix(H))
            @test f(S, D) ≈ f(Matrix(S), Matrix(D))
            @test f(S, H) ≈ f(Matrix(S), Matrix(H))
        end
    end
end

@testset "multiplication of transposes of Diagonal (#22428)" begin
    for T in (Float64, Complex{Float64})
        D = Diagonal(randn(T, 5, 5))
        B = Diagonal(randn(T, 5, 5))
        DD = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        BB = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        fullDD = copy!(Matrix{Matrix{T}}(2, 2), DD)
        fullBB = copy!(Matrix{Matrix{T}}(2, 2), BB)
        for f in (*, Ac_mul_B, A_mul_Bc, Ac_mul_Bc, At_mul_B, A_mul_Bt, At_mul_Bt)
            @test f(D, B)::typeof(D) ≈ f(Matrix(D), Matrix(B)) atol=2 * eps()
            @test f(DD, BB)::typeof(DD) == f(fullDD, fullBB)
        end
    end
end

@testset "Diagonal of a RowVector (#23649)" begin
    @test Diagonal([1,2,3].') == Diagonal([1 2 3])
end
