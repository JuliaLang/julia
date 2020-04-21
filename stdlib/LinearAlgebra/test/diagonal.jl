# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDiagonal

using Test, LinearAlgebra, SparseArrays, Random
using LinearAlgebra: mul!, mul!, rmul!, lmul!, ldiv!, rdiv!, BlasFloat, BlasComplex, SingularException

n=12 #Size of matrix problem to test
Random.seed!(1)

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
            @test Diagonal{elty}(D) === D
        end
        @test eltype(Diagonal{elty}([1,2,3,4])) == elty
        @test isa(Diagonal{elty,Vector{elty}}(GenericArray([1,2,3,4])), Diagonal{elty,Vector{elty}})
        DI = Diagonal([1,2,3,4])
        @test Diagonal(DI) === DI
        @test isa(Diagonal{elty}(DI), Diagonal{elty})
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
        @test isdiag(D)
        @test isdiag(Diagonal([[1 0; 0 1], [1 0; 0 1]]))
        @test !isdiag(Diagonal([[1 0; 0 1], [1 0; 1 1]]))
        @test istriu(D)
        @test istriu(D, -1)
        @test !istriu(D, 1)
        @test istriu(Diagonal(zero(diag(D))), 1)
        @test istril(D)
        @test !istril(D, -1)
        @test istril(D, 1)
        @test istril(Diagonal(zero(diag(D))), -1)
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
                # this method tests AbstractMatrix/AbstractVec for second arg
                Usym_bad = Symmetric(ones(elty, n+1, n+1))
                @test_throws DimensionMismatch ldiv!(D, copy(Usym_bad))

                @test ldiv!(zero(v), D, copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(zero(v), transpose(D), copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(zero(v), adjoint(conj(D)), copy(v)) ≈ DM\v atol=atol_two
                @test ldiv!(zero(U), D, copy(U)) ≈ DM\U atol=atol_three
                @test ldiv!(zero(U), transpose(D), copy(U)) ≈ DM\U atol=atol_three
                @test ldiv!(zero(U), adjoint(conj(D)), copy(U)) ≈ DM\U atol=atol_three

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

        # QR \ Diagonal
        A = rand(elty, n, n)
        qrA = qr(A)
        @test qrA \ D ≈ A \ D

        # HermOrSym
        A     = rand(elty, n, n)
        Asym  = Symmetric(A + transpose(A), :U)
        Aherm = Hermitian(A + adjoint(A), :U)
        for op in (+, -)
            @test op(Asym, D) isa Symmetric
            @test Array(op(Asym, D)) ≈ Array(Symmetric(op(Array(Asym), Array(D))))
            @test op(D, Asym) isa Symmetric
            @test Array(op(D, Asym)) ≈ Array(Symmetric(op(Array(D), Array(Asym))))
            if !(elty <: Real)
                Dr = real(D)
                @test op(Aherm, Dr) isa Hermitian
                @test Array(op(Aherm, Dr)) ≈ Array(Hermitian(op(Array(Aherm), Array(Dr))))
                @test op(Dr, Aherm) isa Hermitian
                @test Array(op(Dr, Aherm)) ≈ Array(Hermitian(op(Array(Dr), Array(Aherm))))
            end
        end
        @test Array(D*Transpose(Asym)) ≈ Array(D) * Array(transpose(Asym))
        @test Array(D*Adjoint(Asym)) ≈ Array(D) * Array(adjoint(Asym))
        @test Array(D*Transpose(Aherm)) ≈ Array(D) * Array(transpose(Aherm))
        @test Array(D*Adjoint(Aherm)) ≈ Array(D) * Array(adjoint(Aherm))
        @test Array(Transpose(Asym)*Transpose(D)) ≈ Array(transpose(Asym)) * Array(transpose(D))
        @test Array(Transpose(D)*Transpose(Asym)) ≈ Array(transpose(D)) * Array(transpose(Asym))
        @test Array(Adjoint(Aherm)*Adjoint(D)) ≈ Array(adjoint(Aherm)) * Array(adjoint(D))
        @test Array(Adjoint(D)*Adjoint(Aherm)) ≈ Array(adjoint(D)) * Array(adjoint(Aherm))

        # Performance specialisations for A*_mul_B!
        vvv = similar(vv)
        @test (r = Matrix(D) * vv   ; mul!(vvv, D, vv)  ≈ r ≈ vvv)
        @test (r = Matrix(D)' * vv  ; mul!(vvv, adjoint(D), vv) ≈ r ≈ vvv)
        @test (r = transpose(Matrix(D)) * vv ; mul!(vvv, transpose(D), vv) ≈ r ≈ vvv)

        UUU = similar(UU)
        for transformA in (identity, adjoint, transpose)
            for transformD in (identity, Adjoint, Transpose, adjoint, transpose)
                @test mul!(UUU, transformA(UU), transformD(D)) ≈  transformA(UU) * Matrix(transformD(D))
                @test mul!(UUU, transformD(D), transformA(UU)) ≈  Matrix(transformD(D)) * transformA(UU)
            end
        end

        alpha = elty(randn())  # randn(elty) does not work with BigFloat
        beta = elty(randn())
        @test begin
            vvv = similar(vv)
            vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
            r = alpha * Matrix(D) * vv + beta * vvv
            mul!(vvv, D, vv, alpha, beta)  ≈ r ≈ vvv
        end
        @test begin
            vvv = similar(vv)
            vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
            r = alpha * Matrix(D)' * vv + beta * vvv
            mul!(vvv, adjoint(D), vv, alpha, beta) ≈ r ≈ vvv
        end
        @test begin
            vvv = similar(vv)
            vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
            r = alpha * transpose(Matrix(D)) * vv + beta * vvv
            mul!(vvv, transpose(D), vv, alpha, beta) ≈ r ≈ vvv
        end

        @test begin
            UUU = similar(UU)
            UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
            r = alpha * Matrix(D) * UU + beta * UUU
            mul!(UUU, D, UU, alpha, beta) ≈ r ≈ UUU
        end
        @test begin
            UUU = similar(UU)
            UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
            r = alpha * Matrix(D)' * UU + beta * UUU
            mul!(UUU, adjoint(D), UU, alpha, beta) ≈ r ≈ UUU
        end
        @test begin
            UUU = similar(UU)
            UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
            r = alpha * transpose(Matrix(D)) * UU + beta * UUU
            mul!(UUU, transpose(D), UU, alpha, beta) ≈ r ≈ UUU
        end

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

        # kron
        D3 = Diagonal(convert(Vector{elty}, rand(n÷2)))
        DM3= Matrix(D3)
        @test Matrix(kron(D, D3)) ≈ kron(DM, DM3)
        M4 = rand(elty, n÷2, n÷2)
        @test kron(D3, M4) ≈ kron(DM3, M4)
        @test kron(M4, D3) ≈ kron(M4, DM3)
    end
    @testset "iszero, isone, triu, tril" begin
        Dzero = Diagonal(zeros(elty, 10))
        Done = Diagonal(ones(elty, 10))
        Dmix = Diagonal(zeros(elty, 10))
        Dmix[end,end] = one(elty)
        @test iszero(Dzero)
        @test !isone(Dzero)
        @test !iszero(Done)
        @test isone(Done)
        @test !iszero(Dmix)
        @test !isone(Dmix)
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
        eigD = eigen(D)
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

    # logdet and logabsdet
    if relty <: Real
        lD = Diagonal(convert(Vector{relty}, rand(n)))
        lM = Matrix(lD)
        @test logdet(lD) ≈ logdet(lM)
        d1, s1 = @inferred logabsdet(lD)
        d2, s2 = logabsdet(lM)
        @test d1 ≈ d2
        @test s1 == s2
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
        @test svd(D).V == V
    end

end

@testset "svdvals and eigvals (#11120/#11247)" begin
    D = Diagonal(Matrix{Float64}[randn(3,3), randn(2,2)])
    @test sort([svdvals(D)...;], rev = true) ≈ svdvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
    @test sort([eigvals(D)...;], by=LinearAlgebra.eigsortby) ≈ eigvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
end

@testset "eigmin (#27847)" begin
    for _ in 1:100
        d = randn(rand(1:10))
        D = Diagonal(d)
        @test eigmin(D) == minimum(d)
    end
end

@testset "isposdef" begin
    @test isposdef(Diagonal(1.0 .+ rand(n)))
    @test !isposdef(Diagonal(-1.0 * rand(n)))
    @test isposdef(Diagonal(complex(1.0, 0.0) .+ rand(n)))
    @test !isposdef(Diagonal(complex(1.0, 1.0) .+ rand(n)))
    @test isposdef(Diagonal([[1 0; 0 1], [1 0; 0 1]]))
    @test !isposdef(Diagonal([[1 0; 0 1], [1 0; 1 1]]))
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
    function _test_matrix(type)
        if type == Int
            return rand(1:9, 5, 5)
        else
            return randn(type, 5, 5)
        end
    end
    types = (Float64, Int, ComplexF64)
    for ta in types
        D = Diagonal(_test_matrix(ta))
        for tb in types
            B = _test_matrix(tb)
            Tmats = (LowerTriangular(B), UnitLowerTriangular(B), UpperTriangular(B), UnitUpperTriangular(B))
            restypes = (LowerTriangular, LowerTriangular, UpperTriangular, UpperTriangular)
            for (T, rtype) in zip(Tmats, restypes)
                adjtype = (rtype == LowerTriangular) ? UpperTriangular : LowerTriangular

                # Triangular * Diagonal
                R = T * D
                @test R ≈ Array(T) * Array(D)
                @test isa(R, rtype)

                # Diagonal * Triangular
                R = D * T
                @test R ≈ Array(D) * Array(T)
                @test isa(R, rtype)

                # Adjoint of Triangular * Diagonal
                R = T' * D
                @test R ≈ Array(T)' * Array(D)
                @test isa(R, adjtype)

                # Diagonal * Adjoint of Triangular
                R = D * T'
                @test R ≈ Array(D) * Array(T)'
                @test isa(R, adjtype)

                # Transpose of Triangular * Diagonal
                R = transpose(T) * D
                @test R ≈ transpose(Array(T)) * Array(D)
                @test isa(R, adjtype)

                # Diagonal * Transpose of Triangular
                R = D * transpose(T)
                @test R ≈ Array(D) * transpose(Array(T))
                @test isa(R, adjtype)
            end
        end
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
    Q = qr(randn(5, 5)).Q
    @test D * Q' == Array(D) * Q'
    Q = qr(randn(5, 5), Val(true)).Q
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

    @test tr(D) == 10
    @test det(D) == 4
end

@testset "linear solve for block diagonal matrices" begin
    D = Diagonal([rand(2,2) for _ in 1:5])
    b = [rand(2,2) for _ in 1:5]
    B = [rand(2,2) for _ in 1:5, _ in 1:5]
    @test ldiv!(D, copy(b)) ≈ Diagonal(inv.(D.diag)) * b
    @test ldiv!(D, copy(B)) ≈ Diagonal(inv.(D.diag)) * B
    @test rdiv!(copy(B), D) ≈ B * Diagonal(inv.(D.diag))
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
                (identity,  transpose), (transpose, identity ), (transpose, transpose),
                (identity,  Adjoint  ), (Adjoint,   identity ), (Adjoint,   Adjoint  ),
                (identity,  Transpose), (Transpose, identity ), (Transpose, Transpose))
            @test *(transform1(D), transform2(B))::typeof(D) ≈ *(transform1(Matrix(D)), transform2(Matrix(B))) atol=2 * eps()
            @test *(transform1(DD), transform2(BB))::typeof(DD) == *(transform1(fullDD), transform2(fullBB))
        end
        M = randn(T, 5, 5)
        MM = [randn(T, 2, 2) for _ in 1:2, _ in 1:2]
        for transform in (identity, adjoint, transpose, Adjoint, Transpose)
            @test lmul!(transform(D), copy(M)) ≈ *(transform(Matrix(D)), M)
            @test rmul!(copy(M), transform(D)) ≈ *(M, transform(Matrix(D)))
            @test lmul!(transform(DD), copy(MM)) ≈ *(transform(fullDD), MM)
            @test rmul!(copy(MM), transform(DD)) ≈ *(MM, transform(fullDD))
        end
    end
end

@testset "Diagonal of adjoint/transpose vectors (#23649)" begin
    @test Diagonal(adjoint([1, 2, 3])) == Diagonal([1 2 3])
    @test Diagonal(transpose([1, 2, 3])) == Diagonal([1 2 3])
end

@testset "Multiplication with Adjoint and Transpose vectors (#26863)" begin
    x = collect(1:2)
    xt = transpose(x)
    A = reshape([[1 2; 3 4], zeros(Int,2,2), zeros(Int, 2, 2), [5 6; 7 8]], 2, 2)
    D = Diagonal(A)
    @test x'*D == x'*A == copy(x')*D == copy(x')*A
    @test xt*D == xt*A == copy(xt)*D == copy(xt)*A
    y = [x, x]
    yt = transpose(y)
    @test y'*D*y == (y'*D)*y == (y'*A)*y
    @test yt*D*y == (yt*D)*y == (yt*A)*y
end

@testset "Triangular division by Diagonal #27989" begin
    K = 5
    for elty in (Float32, Float64, ComplexF32, ComplexF64)
        U = UpperTriangular(randn(elty, K, K))
        L = LowerTriangular(randn(elty, K, K))
        D = Diagonal(randn(elty, K))
        @test (U / D)::UpperTriangular{elty} == UpperTriangular(Matrix(U) / Matrix(D))
        @test (L / D)::LowerTriangular{elty} == LowerTriangular(Matrix(L) / Matrix(D))
        @test (D \ U)::UpperTriangular{elty} == UpperTriangular(Matrix(D) \ Matrix(U))
        @test (D \ L)::LowerTriangular{elty} == LowerTriangular(Matrix(D) \ Matrix(L))
    end
end

@testset "eigenvalue sorting" begin
    D = Diagonal([0.4, 0.2, -1.3])
    @test eigvals(D) == eigen(D).values == [0.4, 0.2, -1.3] # not sorted by default
    @test eigvals(Matrix(D)) == eigen(Matrix(D)).values == [-1.3, 0.2, 0.4] # sorted even if diagonal special case is detected
    E = eigen(D, sortby=abs) # sortby keyword supported for eigen(::Diagonal)
    @test E.values == [0.2, 0.4, -1.3]
    @test E.vectors == [0 1 0; 1 0 0; 0 0 1]
end

@testset "sum, mapreduce" begin
    D = Diagonal([1,2,3])
    Ddense = Matrix(D)
    @test sum(D) == 6
    @test_throws ArgumentError sum(D, dims=0)
    @test sum(D, dims=1) == sum(Ddense, dims=1)
    @test sum(D, dims=2) == sum(Ddense, dims=2)
    @test sum(D, dims=3) == sum(Ddense, dims=3)
    @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))
    @test mapreduce(one, min, D, dims=1) == mapreduce(one, min, Ddense, dims=1)
    @test mapreduce(one, min, D, dims=2) == mapreduce(one, min, Ddense, dims=2)
    @test mapreduce(one, min, D, dims=3) == mapreduce(one, min, Ddense, dims=3)
    @test typeof(mapreduce(one, min, D, dims=1)) == typeof(mapreduce(one, min, Ddense, dims=1))
    @test mapreduce(zero, max, D, dims=1) == mapreduce(zero, max, Ddense, dims=1)
    @test mapreduce(zero, max, D, dims=2) == mapreduce(zero, max, Ddense, dims=2)
    @test mapreduce(zero, max, D, dims=3) == mapreduce(zero, max, Ddense, dims=3)
    @test typeof(mapreduce(zero, max, D, dims=1)) == typeof(mapreduce(zero, max, Ddense, dims=1))

    D = Diagonal(Int[])
    Ddense = Matrix(D)
    @test sum(D) == 0
    @test_throws ArgumentError sum(D, dims=0)
    @test sum(D, dims=1) == sum(Ddense, dims=1)
    @test sum(D, dims=2) == sum(Ddense, dims=2)
    @test sum(D, dims=3) == sum(Ddense, dims=3)
    @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))

    D = Diagonal(Int[2])
    Ddense = Matrix(D)
    @test sum(D) == 2
    @test_throws ArgumentError sum(D, dims=0)
    @test sum(D, dims=1) == sum(Ddense, dims=1)
    @test sum(D, dims=2) == sum(Ddense, dims=2)
    @test sum(D, dims=3) == sum(Ddense, dims=3)
    @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))
end

@testset "logabsdet for generic eltype" begin
    d = Any[1, -2.0, -3.0]
    D = Diagonal(d)
    d1, s1 = logabsdet(D)
    @test d1 ≈ sum(log ∘ abs, d)
    @test s1 == prod(sign, d)
end

end # module TestDiagonal
