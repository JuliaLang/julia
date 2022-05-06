# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDiagonal

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasFloat, BlasComplex

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

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
            for func in (exp, cis, sinh, cosh, tanh, sech, csch, coth)
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

    @testset "Two-dimensional Euler formula for Diagonal" begin
        @test cis(Diagonal([π, π])) ≈ -I
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
                @test ldiv!(D, copy(b)) ≈ Array(D)\Array(b)
                @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), copy(b))
                b = view(rand(elty, n), Vector(1:n))
                b2 = copy(b)
                c = ldiv!(D, b)
                d = Array(D)\b2
                @test c ≈ d
                @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), b)
                b = rand(elty, n+1, n+1)
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
            if elty <: Real
                @test Array(abs.(D)^a) ≈ abs.(DM)^a
            else
                @test Array(D^a) ≈ DM^a
            end
            @test Diagonal(1:100)^2 == Diagonal((1:100).^2)
            p = 3
            @test Diagonal(1:100)^p == Diagonal((1:100).^p)
            @test Diagonal(1:100)^(-1) == Diagonal(inv.(1:100))
            @test Diagonal(1:100)^2.0 == Diagonal((1:100).^2.0)
            @test Diagonal(1:100)^(2.0+0im) == Diagonal((1:100).^(2.0+0im))
        end

        if relty <: BlasFloat
            for b in (rand(elty,n,n), rand(elty,n))
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
        @test Array(D*transpose(Asym)) ≈ Array(D) * Array(transpose(Asym))
        @test Array(D*adjoint(Asym)) ≈ Array(D) * Array(adjoint(Asym))
        @test Array(D*transpose(Aherm)) ≈ Array(D) * Array(transpose(Aherm))
        @test Array(D*adjoint(Aherm)) ≈ Array(D) * Array(adjoint(Aherm))
        @test Array(transpose(Asym)*transpose(D)) ≈ Array(transpose(Asym)) * Array(transpose(D))
        @test Array(transpose(D)*transpose(Asym)) ≈ Array(transpose(D)) * Array(transpose(Asym))
        @test Array(adjoint(Aherm)*adjoint(D)) ≈ Array(adjoint(Aherm)) * Array(adjoint(D))
        @test Array(adjoint(D)*adjoint(Aherm)) ≈ Array(adjoint(D)) * Array(adjoint(Aherm))

        # Performance specialisations for A*_mul_B!
        vvv = similar(vv)
        @test (r = Matrix(D) * vv   ; mul!(vvv, D, vv)  ≈ r ≈ vvv)
        @test (r = Matrix(D)' * vv  ; mul!(vvv, adjoint(D), vv) ≈ r ≈ vvv)
        @test (r = transpose(Matrix(D)) * vv ; mul!(vvv, transpose(D), vv) ≈ r ≈ vvv)

        UUU = similar(UU)
        for transformA in (identity, adjoint, transpose)
            for transformD in (identity, adjoint, transpose)
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
        X = [ones(1,1) for i in 1:2, j in 1:2]
        @test kron(I(2), X)[1,3] == zeros(1,1)
        X = [ones(2,2) for i in 1:2, j in 1:2]
        @test kron(I(2), X)[1,3] == zeros(2,2)
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
        @test Diagonal(eigD.values) == D
        @test eigD.vectors == Matrix(I, size(D))
        eigsortD = eigen(D, sortby=LinearAlgebra.eigsortby)
        @test eigsortD.values !== D.diag
        @test eigsortD.values == sort(D.diag, by=LinearAlgebra.eigsortby)
        @test Matrix(eigsortD) == D
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
        @test logdet(Diagonal(relty[-1,-2])) ≈ log(2)
        @test_throws DomainError logdet(Diagonal(relty[-1,-2,-3]))
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

    @testset "svd/eigen with Diagonal{Furlong}" begin
        Du = Furlong.(D)
        @test Du isa Diagonal{<:Furlong{1}}
        F = svd(Du)
        U, s, V = F
        @test map(x -> x.val, Matrix(F)) ≈ map(x -> x.val, Du)
        @test svdvals(Du) == s
        @test U isa AbstractMatrix{<:Furlong{0}}
        @test V isa AbstractMatrix{<:Furlong{0}}
        @test s isa AbstractVector{<:Furlong{1}}
        E = eigen(Du)
        vals, vecs = E
        @test Matrix(E) == Du
        @test vals isa AbstractVector{<:Furlong{1}}
        @test vecs isa AbstractMatrix{<:Furlong{0}}
    end
end

@testset "rdiv! (#40887)" begin
    @test rdiv!(Matrix(Diagonal([2.0, 3.0])), Diagonal(2:3)) == Diagonal([1.0, 1.0])
    @test rdiv!(fill(3.0, 3, 3), 3.0I(3)) == ones(3,3)
end

@testset "kron (issue #40595)" begin
    # custom array type to test that kron on Diagonal matrices preserves types of the parents if possible
    struct KronTestArray{T, N, AT} <: AbstractArray{T, N}
        data::AT
    end
    KronTestArray(data::AbstractArray) = KronTestArray{eltype(data), ndims(data), typeof(data)}(data)
    Base.size(A::KronTestArray) = size(A.data)
    LinearAlgebra.kron(A::KronTestArray, B::KronTestArray) = KronTestArray(kron(A.data, B.data))
    Base.getindex(K::KronTestArray{<:Any,N}, i::Vararg{Int,N}) where {N} = K.data[i...]

    A = KronTestArray([1, 2, 3]);
    @test kron(A, A) isa KronTestArray
    Ad = Diagonal(A);
    @test kron(Ad, Ad).diag isa KronTestArray
    @test kron(Ad, Ad).diag == kron([1, 2, 3], [1, 2, 3])
end

@testset "svdvals and eigvals (#11120/#11247)" begin
    D = Diagonal(Matrix{Float64}[randn(3,3), randn(2,2)])
    @test sort([svdvals(D)...;], rev = true) ≈ svdvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
    @test sort([eigvals(D)...;], by=LinearAlgebra.eigsortby) ≈ eigvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
end

@testset "eigvals should return a copy of the diagonal" begin
    D = Diagonal([1, 2, 3])
    lam = eigvals(D)
    D[3,3] = 4 # should not affect lam
    @test lam == [1, 2, 3]
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
for t in (Float32, Float64, Int, ComplexF64, Rational{Int})
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

@testset "multiplication of a Diagonal with a Matrix" begin
    A = collect(reshape(1:8, 4, 2));
    B = BigFloat.(A);
    DL = Diagonal(collect(axes(A, 1)));
    DR = Diagonal(Float16.(collect(axes(A, 2))));

    @test DL * A == collect(DL) * A
    @test A * DR == A * collect(DR)
    @test DL * B == collect(DL) * B
    @test B * DR == B * collect(DR)

    A = reshape([ones(2,2), ones(2,2)*2, ones(2,2)*3, ones(2,2)*4], 2, 2)
    Ac = collect(A)
    D = Diagonal([collect(reshape(1:4, 2, 2)), collect(reshape(5:8, 2, 2))])
    Dc = collect(D)
    @test A * D == Ac * Dc
    @test D * A == Dc * Ac
    @test D * D == Dc * Dc

    AS = similar(A)
    mul!(AS, A, D, true, false)
    @test AS == A * D

    D2 = similar(D)
    mul!(D2, D, D)
    @test D2 == D * D

    copyto!(D2, D)
    lmul!(D, D2)
    @test D2 == D * D
    copyto!(D2, D)
    rmul!(D2, D)
    @test D2 == D * D
end

@testset "multiplication of QR Q-factor and Diagonal (#16615 spot test)" begin
    D = Diagonal(randn(5))
    Q = qr(randn(5, 5)).Q
    @test D * Q' == Array(D) * Q'
    Q = qr(randn(5, 5), ColumnNorm()).Q
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
    @test cis(D) == Diagonal([cis([1 2; 3 4]), cis([1 2; 3 4])])
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
    for T in (Float64, ComplexF64)
        D = Diagonal(randn(T, 5, 5))
        B = Diagonal(randn(T, 5, 5))
        DD = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        BB = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
        fullDD = copyto!(Matrix{Matrix{T}}(undef, 2, 2), DD)
        fullBB = copyto!(Matrix{Matrix{T}}(undef, 2, 2), BB)
        for (transform1, transform2) in ((identity,  identity),
                (identity,  adjoint  ), (adjoint,   identity ), (adjoint,   adjoint  ),
                (identity,  transpose), (transpose, identity ), (transpose, transpose))
            @test *(transform1(D), transform2(B))::typeof(D) ≈ *(transform1(Matrix(D)), transform2(Matrix(B))) atol=2 * eps()
            @test *(transform1(DD), transform2(BB))::typeof(DD) == *(transform1(fullDD), transform2(fullBB))
        end
        M = randn(T, 5, 5)
        MM = [randn(T, 2, 2) for _ in 1:2, _ in 1:2]
        for transform in (identity, adjoint, transpose)
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

@testset "Multiplication with adjoint and transpose vectors (#26863)" begin
    x = collect(1:2)
    xt = transpose(x)
    A = reshape([[1 2; 3 4], zeros(Int,2,2), zeros(Int, 2, 2), [5 6; 7 8]], 2, 2)
    D = Diagonal(A)
    @test x'*D == x'*A == collect(x')*D == collect(x')*A
    @test xt*D == xt*A == collect(xt)*D == collect(xt)*A
    outadjxD = similar(x'*D); outtrxD = similar(xt*D);
    mul!(outadjxD, x', D)
    @test outadjxD == x'*D
    mul!(outtrxD, xt, D)
    @test outtrxD == xt*D

    D1 = Diagonal([[1 2; 3 4]])
    @test D1 * x' == D1 * collect(x') == collect(D1) * collect(x')
    @test D1 * xt == D1 * collect(xt) == collect(D1) * collect(xt)
    outD1adjx = similar(D1 * x'); outD1trx = similar(D1 * xt);
    mul!(outadjxD, D1, x')
    @test outadjxD == D1*x'
    mul!(outtrxD, D1, xt)
    @test outtrxD == D1*xt

    y = [x, x]
    yt = transpose(y)
    @test y'*D*y == (y'*D)*y == (y'*A)*y
    @test yt*D*y == (yt*D)*y == (yt*A)*y
    outadjyD = similar(y'*D); outtryD = similar(yt*D);
    outadjyD2 = similar(collect(y'*D)); outtryD2 = similar(collect(yt*D));
    mul!(outadjyD, y', D)
    mul!(outadjyD2, y', D)
    @test outadjyD == outadjyD2 == y'*D
    mul!(outtryD, yt, D)
    mul!(outtryD2, yt, D)
    @test outtryD == outtryD2 == yt*D
end

@testset "Multiplication of single element Diagonal (#36746, #40726)" begin
    @test_throws DimensionMismatch Diagonal(randn(1)) * randn(5)
    @test_throws DimensionMismatch Diagonal(randn(1)) * Diagonal(randn(3, 3))
    A = [1 0; 0 2]
    v = [3, 4]
    @test Diagonal(A) * v == A * v
    @test Diagonal(A) * Diagonal(A) == A * A
    @test_throws DimensionMismatch [1 0;0 1] * Diagonal([2 3])   # Issue #40726
    @test_throws DimensionMismatch lmul!(Diagonal([1]), [1,2,3]) # nearby
end

@testset "Triangular division by Diagonal #27989" begin
    K = 5
    for elty in (Float32, Float64, ComplexF32, ComplexF64)
        U = UpperTriangular(randn(elty, K, K))
        L = LowerTriangular(randn(elty, K, K))
        D = Diagonal(randn(elty, K))
        @test (U / D)::UpperTriangular{elty} ≈ UpperTriangular(Matrix(U) / Matrix(D)) rtol=2eps(real(elty))
        @test (L / D)::LowerTriangular{elty} ≈ LowerTriangular(Matrix(L) / Matrix(D)) rtol=2eps(real(elty))
        @test (D \ U)::UpperTriangular{elty} == UpperTriangular(Matrix(D) \ Matrix(U))
        @test (D \ L)::LowerTriangular{elty} == LowerTriangular(Matrix(D) \ Matrix(L))
    end
end

@testset "(Sym)Tridiagonal division by Diagonal" begin
    for K in (5, 1), elty in (Float64, ComplexF32), overlength in (1, 0)
        S = SymTridiagonal(randn(elty, K), randn(elty, K-overlength))
        T = Tridiagonal(randn(elty, K-1), randn(elty, K), randn(elty, K-1))
        D = Diagonal(randn(elty, K))
        D0 = Diagonal(zeros(elty, K))
        @test (D \ S)::Tridiagonal{elty} == Tridiagonal(Matrix(D) \ Matrix(S))
        @test (D \ T)::Tridiagonal{elty} == Tridiagonal(Matrix(D) \ Matrix(T))
        @test (S / D)::Tridiagonal{elty} ≈ Tridiagonal(Matrix(S) / Matrix(D)) rtol=2eps(real(elty))
        @test (T / D)::Tridiagonal{elty} ≈ Tridiagonal(Matrix(T) / Matrix(D)) rtol=2eps(real(elty))
        @test_throws SingularException D0 \ S
        @test_throws SingularException D0 \ T
        @test_throws SingularException S / D0
        @test_throws SingularException T / D0
    end
    # 0-length case
    S = SymTridiagonal(Float64[], Float64[])
    T = Tridiagonal(Float64[], Float64[], Float64[])
    D = Diagonal(Float64[])
    @test (D \ S)::Tridiagonal{Float64} == T
    @test (D \ T)::Tridiagonal{Float64} == T
    @test (S / D)::Tridiagonal{Float64} == T
    @test (T / D)::Tridiagonal{Float64} == T
    # matrix eltype case
    K = 5
    for elty in (Float64, ComplexF32), overlength in (1, 0)
        S = SymTridiagonal([rand(elty, 2, 2) for _ in 1:K], [rand(elty, 2, 2) for _ in 1:K-overlength])
        T = Tridiagonal([rand(elty, 2, 2) for _ in 1:K-1], [rand(elty, 2, 2) for _ in 1:K], [rand(elty, 2, 2) for _ in 1:K-1])
        D = Diagonal(randn(elty, K))
        SM = fill(zeros(elty, 2, 2), K, K)
        TM = copy(SM)
        SM[1,1] = S[1,1]; TM[1,1] = T[1,1]
        for j in 2:K
            SM[j,j-1] = S[j,j-1]; SM[j,j] = S[j,j]; SM[j-1,j] = S[j-1,j]
            TM[j,j-1] = T[j,j-1]; TM[j,j] = T[j,j]; TM[j-1,j] = T[j-1,j]
        end
        for (M, Mm) in ((S, SM), (T, TM))
            DS = D \ M
            @test DS isa Tridiagonal
            DM = D \ Mm
            for i in -1:1; @test diag(DS, i) ≈ diag(DM, i) end
            DS = M / D
            @test DS isa Tridiagonal
            DM = Mm / D
            for i in -1:1; @test diag(DS, i) ≈ diag(DM, i) end
        end
    end
    # eltype promotion case
    S = SymTridiagonal(rand(-20:20, K), rand(-20:20, K-1))
    T = Tridiagonal(rand(-20:20, K-1), rand(-20:20, K), rand(-20:20, K-1))
    D = Diagonal(rand(1:20, K))
    @test (D \ S)::Tridiagonal{Float64} == Tridiagonal(Matrix(D) \ Matrix(S))
    @test (D \ T)::Tridiagonal{Float64} == Tridiagonal(Matrix(D) \ Matrix(T))
    @test (S / D)::Tridiagonal{Float64} ≈ Tridiagonal(Matrix(S) / Matrix(D)) rtol=2eps()
    @test (T / D)::Tridiagonal{Float64} ≈ Tridiagonal(Matrix(T) / Matrix(D)) rtol=2eps()
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

@testset "Empty (#35424)" begin
    @test zeros(0)'*Diagonal(zeros(0))*zeros(0) === 0.0
    @test transpose(zeros(0))*Diagonal(zeros(Complex{Int}, 0))*zeros(0) === 0.0 + 0.0im
    @test dot(zeros(Int32, 0), Diagonal(zeros(Int, 0)), zeros(Int16, 0)) === 0
end

@testset "Diagonal(undef)" begin
    d = Diagonal{Float32}(undef, 2)
    @test length(d.diag) == 2
end

@testset "permutedims (#39447)" begin
    for D in (Diagonal(zeros(5)), Diagonal(zeros(5) .+ 1im), Diagonal([[1,2],[3,4]]))
        @test permutedims(D) === permutedims(D,(1,2)) === permutedims(D,(2,1)) === D
        @test_throws ArgumentError permutedims(D,(1,3))
    end
end

@testset "Inner product" begin
    A = Diagonal(rand(10) .+ im)
    B = Diagonal(rand(10) .+ im)
    @test dot(A, B) ≈ dot(Matrix(A), B)
    @test dot(A, B) ≈ dot(A, Matrix(B))
    @test dot(A, B) ≈ dot(Matrix(A), Matrix(B))
    @test dot(A, B) ≈ conj(dot(B, A))
end

@testset "eltype relaxation(#41015)" begin
    A = rand(3,3)
    for trans in (identity, adjoint, transpose)
        @test ldiv!(trans(I(3)), A) == A
        @test rdiv!(A, trans(I(3))) == A
    end
end

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :ImmutableArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ImmutableArrays.jl"))
using .Main.ImmutableArrays

@testset "Conversion to AbstractArray" begin
    # tests corresponding to #34995
    d = ImmutableArray([1, 2, 3, 4])
    D = Diagonal(d)

    @test convert(AbstractArray{Float64}, D)::Diagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == D
    @test convert(AbstractMatrix{Float64}, D)::Diagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == D
end

@testset "divisions functionality" for elty in (Int, Float64, ComplexF64)
    B = Diagonal(rand(elty,5,5))
    x = rand(elty)
    @test \(x, B) ≈ /(B, x) rtol=2eps()
end

@testset "promotion" begin
    for (v1, v2) in (([true], [1]), ([zeros(2,2)], [zeros(Int, 2,2)]))
        T = promote_type(eltype(v1), eltype(v2))
        V = promote_type(typeof(v1), typeof(v2))
        d1 = Diagonal(v1)
        d2 = Diagonal(v2)
        v = [d1, d2]
        @test (@inferred eltype(v)) == Diagonal{T, V}
    end
    # test for a type for which promote_type doesn't lead to a concrete eltype
    struct MyArrayWrapper{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
       a :: A
    end
    Base.size(M::MyArrayWrapper) = size(M.a)
    Base.axes(M::MyArrayWrapper) = axes(M.a)
    Base.length(M::MyArrayWrapper) = length(M.a)
    Base.getindex(M::MyArrayWrapper, i::Int...) = M.a[i...]
    Base.setindex!(M::MyArrayWrapper, v, i::Int...) = M.a[i...] = v
    d1 = Diagonal(MyArrayWrapper(1:3))
    d2 = Diagonal(MyArrayWrapper(1.0:3.0))
    c = [d1, d2]
    @test c[1] == d1
    @test c[2] == d2
end

@testset "zero and one" begin
    D1 = Diagonal(rand(3))
    @test D1 + zero(D1) == D1
    @test D1 * one(D1) == D1
    @test D1 * oneunit(D1) == D1
    @test oneunit(D1) isa typeof(D1)
    D2 = Diagonal([collect(reshape(1:4, 2, 2)), collect(reshape(5:8, 2, 2))])
    @test D2 + zero(D2) == D2
    @test D2 * one(D2) == D2
    @test D2 * oneunit(D2) == D2
    @test oneunit(D2) isa typeof(D2)
    D3 = Diagonal([D2, D2]);
    @test D3 + zero(D3) == D3
    @test D3 * one(D3) == D3
    @test D3 * oneunit(D3) == D3
    @test oneunit(D3) isa typeof(D3)
end

@testset "AbstractTriangular" for (Tri, UTri) in ((UpperTriangular, UnitUpperTriangular), (LowerTriangular, UnitLowerTriangular))
    A = randn(4, 4)
    TriA = Tri(A)
    UTriA = UTri(A)
    D = Diagonal(1.0:4.0)
    DM = Matrix(D)
    DMF = factorize(DM)
    outTri = similar(TriA)
    out = similar(A)
    # 2 args
    for fun in (*, rmul!, rdiv!, /)
        @test fun(copy(TriA), D)::Tri == fun(Matrix(TriA), D)
        @test fun(copy(UTriA), D)::Tri == fun(Matrix(UTriA), D)
    end
    for fun in (*, lmul!, ldiv!, \)
        @test fun(D, copy(TriA))::Tri == fun(D, Matrix(TriA))
        @test fun(D, copy(UTriA))::Tri == fun(D, Matrix(UTriA))
    end
    # 3 args
    @test outTri === ldiv!(outTri, D, TriA)::Tri == ldiv!(out, D, Matrix(TriA))
    @test outTri === ldiv!(outTri, D, UTriA)::Tri == ldiv!(out, D, Matrix(UTriA))
    @test outTri === mul!(outTri, D, TriA)::Tri == mul!(out, D, Matrix(TriA))
    @test outTri === mul!(outTri, D, UTriA)::Tri == mul!(out, D, Matrix(UTriA))
    @test outTri === mul!(outTri, TriA, D)::Tri == mul!(out, Matrix(TriA), D)
    @test outTri === mul!(outTri, UTriA, D)::Tri == mul!(out, Matrix(UTriA), D)
    # 5 args
    @test outTri === mul!(outTri, D, TriA, 2, 1)::Tri == mul!(out, D, Matrix(TriA), 2, 1)
    @test outTri === mul!(outTri, D, UTriA, 2, 1)::Tri == mul!(out, D, Matrix(UTriA), 2, 1)
    @test outTri === mul!(outTri, TriA, D, 2, 1)::Tri == mul!(out, Matrix(TriA), D, 2, 1)
    @test outTri === mul!(outTri, UTriA, D, 2, 1)::Tri == mul!(out, Matrix(UTriA), D, 2, 1)
end

end # module TestDiagonal
