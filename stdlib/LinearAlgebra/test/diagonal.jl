# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDiagonal

using Test, LinearAlgebra, SparseArrays, Random
using LinearAlgebra: mul!, rmul!, lmul!, ldiv!, rdiv!, BlasFloat, BlasComplex, SingularException

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
    DM = Matrix(Diagonal(dd))

    @testset "constructor" begin
        for x in (dd, GenericArray(dd))
            @test Diagonal(x)::Diagonal{elty,typeof(x)} == DM
            @test Diagonal(x).diag === x
            @test Diagonal{elty}(x)::Diagonal{elty,typeof(x)} == DM
            @test Diagonal{elty}(x).diag === x
        end
        # issue #26178
        @test_throws MethodError convert(Diagonal, [1, 2, 3, 4])
    end

    @testset "Basic properties" begin
        @test_throws ArgumentError size(D,0)
        @test typeof(convert(Diagonal{ComplexF32},D)) <: Diagonal{ComplexF32}
        @test typeof(convert(AbstractMatrix{ComplexF32},D)) <: Diagonal{ComplexF32}

        @test Array(real(D)) == real(DM)
        @test Array(abs.(D)) == abs.(DM)
        @test Array(imag(D)) == imag(DM)

        @test parent(D) == dd
        @test D[1,1] == dd[1]
        @test D[1,2] == 0

        @test issymmetric(D)
        @test istriu(D)
        @test istril(D)
        if elty <: Real
            @test ishermitian(D)
        end
    end

    @testset "diag" begin
        @test_throws ArgumentError diag(D,  n+1)
        @test_throws ArgumentError diag(D, -n-1)
        @test (@inferred diag(D))::typeof(dd) == dd
        @test (@inferred diag(D, 0))::typeof(dd) == dd
        @test (@inferred diag(D, 1))::typeof(dd) == zeros(elty, n-1)
        DG = Diagonal(GenericArray(dd))
        @test (@inferred diag(DG))::typeof(GenericArray(dd)) == GenericArray(dd)
        @test (@inferred diag(DG, 1))::typeof(GenericArray(dd)) == GenericArray(zeros(elty, n-1))
    end


    @testset "Simple unary functions" begin
        for op in (-,)
            @test op(D)==op(DM)
        end

        for func in (det, tr)
            @test func(D) ≈ func(DM) atol=n^2*eps(relty)*(1+(elty<:Complex))
        end
        if relty <: BlasFloat
            for func in (exp, sinh, cosh, tanh, sech, csch, coth)
                @test func(D) ≈ func(DM) atol=n^3*eps(relty)
            end
            @test log(Diagonal(abs.(D.diag))) ≈ log(abs.(DM)) atol=n^3*eps(relty)
        end
        if elty <: BlasComplex
            for func in (logdet, sqrt, sin, cos, tan, sec, csc, cot,
                         asin, acos, atan, asec, acsc, acot,
                         asinh, acosh, atanh, asech, acsch, acoth)
                @test func(D) ≈ func(DM) atol=n^2*eps(relty)*2
            end
        end
    end

    @testset "Linear solve" begin
        for (v, U) in ((vv, UU), (view(vv, 1:n), view(UU, 1:n, 1:2)))
            @test D*v ≈ DM*v atol=n*eps(relty)*(1+(elty<:Complex))
            @test D*U ≈ DM*U atol=n^2*eps(relty)*(1+(elty<:Complex))

            @test transpose(U)*D ≈ transpose(U)*Array(D)
            @test U'*D ≈ U'*Array(D)

            if relty != BigFloat
                atol_two = 2n^2 * eps(relty) * (1 + (elty <: Complex))
                atol_three = 2n^3 * eps(relty) * (1 + (elty <: Complex))
                @test D\v ≈ DM\v atol=atol_two
                @test D\U ≈ DM\U atol=atol_three
                @test ldiv!(D, copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(transpose(D), copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(adjoint(conj(D)), copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(D, copy(U)) ≈ DM\U atol=atol_three
                @test ldiv!(transpose(D), copy(U)) ≈ DM\U atol=atol_three
                @test ldiv!(adjoint(conj(D)), copy(U)) ≈ DM\U atol=atol_three
                Uc = copy(U')
                target = rmul!(Uc, Diagonal(inv.(D.diag)))
                @test rdiv!(Uc, D) ≈ target atol=atol_three
                @test_throws DimensionMismatch rdiv!(Matrix{elty}(I, n-1, n-1), D)
                @test_throws SingularException rdiv!(Uc, Diagonal(fill!(similar(D.diag), 0)))
                @test rdiv!(Uc, transpose(D)) ≈ target atol=atol_three
                @test rdiv!(Uc, adjoint(conj(D))) ≈ target atol=atol_three
                @test ldiv!(D, Matrix{eltype(D)}(I, size(D))) ≈ D \ Matrix{eltype(D)}(I, size(D)) atol=atol_three
                @test_throws DimensionMismatch ldiv!(D, fill(elty(1), n + 1))
                @test_throws SingularException ldiv!(Diagonal(zeros(relty, n)), copy(v))
                b = rand(elty, n, n)
                b = sparse(b)
                @test ldiv!(D, copy(b)) ≈ Array(D)\Array(b)
                @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), copy(b))
                b = view(rand(elty, n), Vector(1:n))
                b2 = copy(b)
                c = ldiv!(D, b)
                d = Array(D)\b2
                @test c ≈ d
                @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), b)
                b = rand(elty, n+1, n+1)
                b = sparse(b)
                @test_throws DimensionMismatch ldiv!(D, copy(b))
                b = view(rand(elty, n+1), Vector(1:n+1))
                @test_throws DimensionMismatch ldiv!(D, b)
            end
        end
    end
    d = convert(Vector{elty}, randn(n))
    D2 = Diagonal(d)
    DM2= Matrix(Diagonal(d))
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
                @test lmul!(copy(D), copy(b)) ≈ Array(D)*Array(b)
                @test lmul!(transpose(copy(D)), copy(b)) ≈ transpose(Array(D))*Array(b)
                @test lmul!(adjoint(copy(D)), copy(b)) ≈ Array(D)'*Array(b)
            end
        end

        #a few missing mults
        bd = Bidiagonal(D2)
        @test D*transpose(D2) ≈ Array(D)*transpose(Array(D2))
        @test D2*transpose(D) ≈ Array(D2)*transpose(Array(D))
        @test D2*D' ≈ Array(D2)*Array(D)'

        #division of two Diagonals
        @test D/D2 ≈ Diagonal(D.diag./D2.diag)
        @test D\D2 ≈ Diagonal(D2.diag./D.diag)

        # Performance specialisations for A*_mul_B!
        vvv = similar(vv)
        @test (r = Matrix(D) * vv   ; mul!(vvv, D, vv)  ≈ r ≈ vvv)
        @test (r = Matrix(D)' * vv  ; mul!(vvv, adjoint(D), vv) ≈ r ≈ vvv)
        @test (r = transpose(Matrix(D)) * vv ; mul!(vvv, transpose(D), vv) ≈ r ≈ vvv)

        UUU = similar(UU)
        @test (r = Matrix(D) * UU   ; mul!(UUU, D, UU) ≈ r ≈ UUU)
        @test (r = Matrix(D)' * UU  ; mul!(UUU, adjoint(D), UU) ≈ r ≈ UUU)
        @test (r = transpose(Matrix(D)) * UU ; mul!(UUU, transpose(D), UU) ≈ r ≈ UUU)

        # make sure that mul!(A, {Adj|Trans}(B)) works with B as a Diagonal
        VV = Array(D)
        DD = copy(D)
        r  = VV * Matrix(D)
        @test Array(rmul!(VV, DD)) ≈ r ≈ Array(D)*Array(D)
        DD = copy(D)
        r  = VV * transpose(Array(D))
        @test Array(rmul!(VV, transpose(DD))) ≈ r
        DD = copy(D)
        r  = VV * Array(D)'
        @test Array(rmul!(VV, adjoint(DD))) ≈ r
    end
    @testset "triu/tril" begin
        @test istriu(D)
        @test istril(D)
        @test iszero(triu(D,1))
        @test triu(D,0)  == D
        @test triu(D,-1) == D
        @test tril(D,1)  == D
        @test iszero(tril(D,-1))
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
        @test Diagonal(eigD.values) ≈ D
        @test eigD.vectors == Matrix(I, size(D))
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
        @test(transpose(D) * vv == D * vv)
    end

    #logdet
    if relty <: Real
        ld=convert(Vector{relty},rand(n))
        @test logdet(Diagonal(ld)) ≈ logdet(Matrix(Diagonal(ld)))
    end

    @testset "similar" begin
        @test isa(similar(D), Diagonal{elty})
        @test isa(similar(D, Int), Diagonal{Int})
        @test isa(similar(D, (3,2)), SparseMatrixCSC{elty})
        @test isa(similar(D, Int, (3,2)), SparseMatrixCSC{Int})
    end

    # Issue number 10036
    # make sure issymmetric/ishermitian work for
    # non-real diagonal matrices
    @testset "issymmetric/hermitian for complex Diagonal" begin
        @test issymmetric(D2)
        @test ishermitian(D2)
        if elty <: Complex
            dc = d .+ elty(1im)
            D3 = Diagonal(dc)
            @test issymmetric(D3)
            @test !ishermitian(D3)
        end
    end

    @testset "svd (#11120/#11247)" begin
        U, s, V = svd(D)
        @test (U*Diagonal(s))*V' ≈ D
        @test svdvals(D) == s
        @test svdfact(D).V == V
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
@test all(Diagonal(range(1, stop=3, length=3)) .== Diagonal([1.0,2.0,3.0]))

# Issue 12803
for t in (Float32, Float64, Int, Complex{Float64}, Rational{Int})
    @test Diagonal(Matrix{t}[fill(t(1), 2, 2), fill(t(1), 3, 3)])[2,1] == zeros(t, 3, 2)
end

# Issue 15401
@test Matrix(1.0I, 5, 5) \ Diagonal(fill(1.,5)) == Matrix(I, 5, 5)

@testset "Triangular and Diagonal" begin
    for T in (LowerTriangular(randn(5,5)), LinearAlgebra.UnitLowerTriangular(randn(5,5)))
        D = Diagonal(randn(5))
        @test T*D   == Array(T)*Array(D)
        @test T'D   == Array(T)'*Array(D)
        @test transpose(T)*D  == transpose(Array(T))*Array(D)
        @test D*T'  == Array(D)*Array(T)'
        @test D*transpose(T) == Array(D)*transpose(Array(T))
        @test D*T   == Array(D)*Array(T)
    end
end

let D1 = Diagonal(rand(5)), D2 = Diagonal(rand(5))
    @test LinearAlgebra.rmul!(copy(D1),D2) == D1*D2
    @test LinearAlgebra.lmul!(D1,copy(D2)) == D1*D2
    @test LinearAlgebra.rmul!(copy(D1),transpose(D2)) == D1*transpose(D2)
    @test LinearAlgebra.lmul!(transpose(D1),copy(D2)) == transpose(D1)*D2
    @test LinearAlgebra.rmul!(copy(D1),adjoint(D2)) == D1*adjoint(D2)
    @test LinearAlgebra.lmul!(adjoint(D1),copy(D2)) == adjoint(D1)*D2
end

@testset "multiplication of QR Q-factor and Diagonal (#16615 spot test)" begin
    D = Diagonal(randn(5))
    Q = qrfact(randn(5, 5)).Q
    @test D * Q' == Array(D) * Q'
    Q = qrfact(randn(5, 5), Val(true)).Q
    @test_throws ArgumentError lmul!(Q, D)
end

@testset "block diagonal matrices" begin
    D = Diagonal([[1 2; 3 4], [1 2; 3 4]])
    Dherm = Diagonal([[1 1+im; 1-im 1], [1 1+im; 1-im 1]])
    Dsym = Diagonal([[1 1+im; 1+im 1], [1 1+im; 1+im 1]])
    @test adjoint(D) == Diagonal([[1 3; 2 4], [1 3; 2 4]])
    @test transpose(D) == Diagonal([[1 3; 2 4], [1 3; 2 4]])
    @test adjoint(Dherm) == Dherm
    @test transpose(Dherm) == Diagonal([[1 1-im; 1+im 1], [1 1-im; 1+im 1]])
    @test adjoint(Dsym) == Diagonal([[1 1-im; 1-im 1], [1 1-im; 1-im 1]])
    @test transpose(Dsym) == Dsym

    v = [[1, 2], [3, 4]]
    @test Dherm' * v == Dherm * v
    @test transpose(D) * v == [[7, 10], [15, 22]]

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
    for T in (Float64, ComplexF64)
        D = Diagonal(randn(T, n))
        A = randn(T, n, n); A = A'A
        S = Symmetric(A)
        H = Hermitian(A)
        for (transform1, transform2) in ((identity,  identity),
                (identity,  adjoint  ), (adjoint,   identity ), (adjoint,   adjoint  ),
                (identity,  transpose), (transpose, identity ), (transpose, transpose) )
            @test *(transform1(D), transform2(S)) ≈ *(transform1(Matrix(D)), transform2(Matrix(S)))
            @test *(transform1(D), transform2(H)) ≈ *(transform1(Matrix(D)), transform2(Matrix(H)))
            @test *(transform1(S), transform2(D)) ≈ *(transform1(Matrix(S)), transform2(Matrix(D)))
            @test *(transform1(S), transform2(H)) ≈ *(transform1(Matrix(S)), transform2(Matrix(H)))
        end
    end
end

@testset "multiplication of transposes of Diagonal (#22428)" begin
    for T in (Float64, Complex{Float64})
        D = Diagonal(randn(T, 5, 5))
        B = Diagonal(randn(T, 5, 5))
        DD = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        BB = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        fullDD = copyto!(Matrix{Matrix{T}}(undef, 2, 2), DD)
        fullBB = copyto!(Matrix{Matrix{T}}(undef, 2, 2), BB)
        for (transform1, transform2) in ((identity,  identity),
                (identity,  adjoint  ), (adjoint,   identity ), (adjoint,   adjoint  ),
                (identity,  transpose), (transpose, identity ), (transpose, transpose) )
            @test *(transform1(D), transform2(B))::typeof(D) ≈ *(transform1(Matrix(D)), transform2(Matrix(B))) atol=2 * eps()
            @test *(transform1(DD), transform2(BB))::typeof(DD) == *(transform1(fullDD), transform2(fullBB))
        end
    end
end

@testset "Diagonal of adjoint/transpose vectors (#23649)" begin
    @test Diagonal(adjoint([1, 2, 3])) == Diagonal([1 2 3])
    @test Diagonal(transpose([1, 2, 3])) == Diagonal([1 2 3])
end

end # module TestDiagonal
