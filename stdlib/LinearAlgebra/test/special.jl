# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSpecial

using Test, LinearAlgebra, Random
using LinearAlgebra: rmul!

n= 10 #Size of matrix to test
Random.seed!(1)

@testset "Interconversion between special matrix types" begin
    a = [1.0:n;]
    A = Diagonal(a)
    @testset for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Matrix]
       @test Matrix(convert(newtype, A)) == Matrix(A)
       @test Matrix(convert(newtype, Diagonal(GenericArray(a)))) == Matrix(A)
    end

    @testset for isupper in (true, false)
        A = Bidiagonal(a, [1.0:n-1;], ifelse(isupper, :U, :L))
        for newtype in [Bidiagonal, Tridiagonal, Matrix]
           @test Matrix(convert(newtype, A)) == Matrix(A)
           @test Matrix(newtype(A)) == Matrix(A)
        end
        @test_throws ArgumentError convert(SymTridiagonal, A)
        tritype = isupper ? UpperTriangular : LowerTriangular
        @test Matrix(tritype(A)) == Matrix(A)

        A = Bidiagonal(a, zeros(n-1), ifelse(isupper, :U, :L)) #morally Diagonal
        for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Matrix]
           @test Matrix(convert(newtype, A)) == Matrix(A)
           @test Matrix(newtype(A)) == Matrix(A)
        end
        @test Matrix(tritype(A)) == Matrix(A)
    end

    A = SymTridiagonal(a, [1.0:n-1;])
    for newtype in [Tridiagonal, Matrix]
       @test Matrix(convert(newtype, A)) == Matrix(A)
    end
    for newtype in [Diagonal, Bidiagonal]
       @test_throws ArgumentError convert(newtype,A)
    end
    A = SymTridiagonal(a, zeros(n-1))
    @test Matrix(convert(Bidiagonal,A)) == Matrix(A)

    A = Tridiagonal(zeros(n-1), [1.0:n;], zeros(n-1)) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Matrix]
       @test Matrix(convert(newtype, A)) == Matrix(A)
    end
    A = Tridiagonal(fill(1., n-1), [1.0:n;], fill(1., n-1)) #not morally Diagonal
    for newtype in [SymTridiagonal, Matrix]
       @test Matrix(convert(newtype, A)) == Matrix(A)
    end
    for newtype in [Diagonal, Bidiagonal]
        @test_throws ArgumentError convert(newtype,A)
    end
    A = Tridiagonal(zeros(n-1), [1.0:n;], fill(1., n-1)) #not morally Diagonal
    @test Matrix(convert(Bidiagonal, A)) == Matrix(A)
    A = UpperTriangular(Tridiagonal(zeros(n-1), [1.0:n;], fill(1., n-1)))
    @test Matrix(convert(Bidiagonal, A)) == Matrix(A)
    A = Tridiagonal(fill(1., n-1), [1.0:n;], zeros(n-1)) #not morally Diagonal
    @test Matrix(convert(Bidiagonal, A)) == Matrix(A)
    A = LowerTriangular(Tridiagonal(fill(1., n-1), [1.0:n;], zeros(n-1)))
    @test Matrix(convert(Bidiagonal, A)) == Matrix(A)
    @test_throws ArgumentError convert(SymTridiagonal,A)

    A = LowerTriangular(Matrix(Diagonal(a))) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, LowerTriangular, Matrix]
        @test Matrix(convert(newtype, A)) == Matrix(A)
    end
    A = UpperTriangular(Matrix(Diagonal(a))) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, UpperTriangular, Matrix]
        @test Matrix(convert(newtype, A)) == Matrix(A)
    end
    A = UpperTriangular(triu(rand(n,n)))
    for newtype in [Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal]
        @test_throws ArgumentError convert(newtype,A)
    end


    # test operations/constructors (not conversions) permitted in the docs
    dl = [1., 1.]
    d = [-2., -2., -2.]
    T = Tridiagonal(dl, d, -dl)
    S = SymTridiagonal(d, dl)
    Bu = Bidiagonal(d, dl, :U)
    Bl = Bidiagonal(d, dl, :L)
    D = Diagonal(d)
    M = [-2. 0. 0.; 1. -2. 0.; -1. 1. -2.]
    U = UpperTriangular(M)
    L = LowerTriangular(Matrix(M'))

    for A in (T, S, Bu, Bl, D, U, L, M)
        Adense = Matrix(A)
        B = Symmetric(A)
        Bdense = Matrix(B)
        for (C,Cdense) in ((A,Adense), (B,Bdense))
            @test Diagonal(C) == Diagonal(Cdense)
            @test Bidiagonal(C, :U) == Bidiagonal(Cdense, :U)
            @test Bidiagonal(C, :L) == Bidiagonal(Cdense, :L)
            @test Tridiagonal(C) == Tridiagonal(Cdense)
            @test UpperTriangular(C) == UpperTriangular(Cdense)
            @test LowerTriangular(C) == LowerTriangular(Cdense)
        end
    end

    @testset "Matrix constructor for !isa(zero(T), T)" begin
        # the following models JuMP.jl's VariableRef and AffExpr, resp.
        struct TypeWithoutZero end
        struct TypeWithZero end
        Base.promote_rule(::Type{TypeWithoutZero}, ::Type{TypeWithZero}) = TypeWithZero
        Base.convert(::Type{TypeWithZero}, ::TypeWithoutZero) = TypeWithZero()
        Base.zero(::Type{<:Union{TypeWithoutZero, TypeWithZero}}) = TypeWithZero()
        LinearAlgebra.symmetric(::TypeWithoutZero, ::Symbol) = TypeWithoutZero()
        Base.transpose(::TypeWithoutZero) = TypeWithoutZero()
        d  = fill(TypeWithoutZero(), 3)
        du = fill(TypeWithoutZero(), 2)
        dl = fill(TypeWithoutZero(), 2)
        D  = Diagonal(d)
        Bu = Bidiagonal(d, du, :U)
        Bl = Bidiagonal(d, dl, :L)
        Tri = Tridiagonal(dl, d, du)
        Sym = SymTridiagonal(d, dl)
        for M in (D, Bu, Bl, Tri, Sym)
            @test Matrix(M) == zeros(TypeWithZero, 3, 3)
        end
    end
end

@testset "Binary ops among special types" begin
    a=[1.0:n;]
    A=Diagonal(a)
    Spectypes = [Diagonal, Bidiagonal, Tridiagonal, Matrix]
    for (idx, type1) in enumerate(Spectypes)
        for type2 in Spectypes
           B = convert(type1,A)
           C = convert(type2,A)
           @test Matrix(B + C) ≈ Matrix(A + A)
           @test Matrix(B - C) ≈ Matrix(A - A)
       end
    end
    B = SymTridiagonal(a, fill(1., n-1))
    for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
        @test Matrix(B + convert(Spectype,A)) ≈ Matrix(B + A)
        @test Matrix(convert(Spectype,A) + B) ≈ Matrix(B + A)
        @test Matrix(B - convert(Spectype,A)) ≈ Matrix(B - A)
        @test Matrix(convert(Spectype,A) - B) ≈ Matrix(A - B)
    end

    C = rand(n,n)
    for TriType in [LinearAlgebra.UnitLowerTriangular, LinearAlgebra.UnitUpperTriangular, UpperTriangular, LowerTriangular]
        D = TriType(C)
        for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
            @test Matrix(D + convert(Spectype,A)) ≈ Matrix(D + A)
            @test Matrix(convert(Spectype,A) + D) ≈ Matrix(A + D)
            @test Matrix(D - convert(Spectype,A)) ≈ Matrix(D - A)
            @test Matrix(convert(Spectype,A) - D) ≈ Matrix(A - D)
        end
    end

    UpTri = UpperTriangular(rand(20,20))
    LoTri = LowerTriangular(rand(20,20))
    Diag = Diagonal(rand(20,20))
    Tridiag = Tridiagonal(rand(20, 20))
    UpBi = Bidiagonal(rand(20,20), :U)
    LoBi = Bidiagonal(rand(20,20), :L)
    Sym = SymTridiagonal(rand(20), rand(19))
    Dense = rand(20, 20)
    mats = Any[UpTri, LoTri, Diag, Tridiag, UpBi, LoBi, Sym, Dense]

    for op in (+,-,*)
        for A in mats
            for B in mats
                @test (op)(A, B) ≈ (op)(Matrix(A), Matrix(B)) ≈ Matrix((op)(A, B))
            end
        end
    end
end

@testset "+ and - among structured matrices with different container types" begin
    diag = 1:5
    offdiag = 1:4
    uniformscalingmats = [UniformScaling(3), UniformScaling(1.0), UniformScaling(3//5), UniformScaling(ComplexF64(1.3, 3.5))]
    mats = Any[Diagonal(diag), Bidiagonal(diag, offdiag, 'U'), Bidiagonal(diag, offdiag, 'L'), Tridiagonal(offdiag, diag, offdiag), SymTridiagonal(diag, offdiag)]
    for T in [ComplexF64, Int64, Rational{Int64}, Float64]
        push!(mats, Diagonal(Vector{T}(diag)))
        push!(mats, Bidiagonal(Vector{T}(diag), Vector{T}(offdiag), 'U'))
        push!(mats, Bidiagonal(Vector{T}(diag), Vector{T}(offdiag), 'L'))
        push!(mats, Tridiagonal(Vector{T}(offdiag), Vector{T}(diag), Vector{T}(offdiag)))
        push!(mats, SymTridiagonal(Vector{T}(diag), Vector{T}(offdiag)))
    end

    for op in (+,*) # to do: fix when operation is - and the matrix has a range as the underlying representation and we get a step size of 0.
        for A in mats
            for B in mats
                @test (op)(A, B) ≈ (op)(Matrix(A), Matrix(B)) ≈ Matrix((op)(A, B))
            end
        end
    end
    for op in (+,-)
        for A in mats
            for B in uniformscalingmats
                @test (op)(A, B) ≈ (op)(Matrix(A), B) ≈ Matrix((op)(A, B))
                @test (op)(B, A) ≈ (op)(B, Matrix(A)) ≈ Matrix((op)(B, A))
            end
        end
    end
end


@testset "Triangular Types and QR" begin
    for typ in (UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular)
        a = rand(n,n)
        atri = typ(a)
        matri = Matrix(atri)
        b = rand(n,n)
        qrb = qr(b, ColumnNorm())
        @test atri * qrb.Q ≈ matri * qrb.Q ≈ rmul!(copy(atri), qrb.Q)
        @test atri * qrb.Q' ≈ matri * qrb.Q' ≈ rmul!(copy(atri), qrb.Q')
        @test qrb.Q * atri ≈ qrb.Q * matri ≈ lmul!(qrb.Q, copy(atri))
        @test qrb.Q' * atri ≈ qrb.Q' * matri ≈ lmul!(qrb.Q', copy(atri))
        qrb = qr(b, NoPivot())
        @test atri * qrb.Q ≈ matri * qrb.Q ≈ rmul!(copy(atri), qrb.Q)
        @test atri * qrb.Q' ≈ matri * qrb.Q' ≈ rmul!(copy(atri), qrb.Q')
        @test qrb.Q * atri ≈ qrb.Q * matri ≈ lmul!(qrb.Q, copy(atri))
        @test qrb.Q' * atri ≈ qrb.Q' * matri ≈ lmul!(qrb.Q', copy(atri))
    end
end

@testset "concatenations of combinations of special and other matrix types" begin
    N = 4
    # Test concatenating pairwise combinations of special matrices
    diagmat = Diagonal(1:N)
    bidiagmat = Bidiagonal(1:N, 1:(N-1), :U)
    tridiagmat = Tridiagonal(1:(N-1), 1:N, 1:(N-1))
    symtridiagmat = SymTridiagonal(1:N, 1:(N-1))
    specialmats = (diagmat, bidiagmat, tridiagmat, symtridiagmat)
    for specialmata in specialmats, specialmatb in specialmats
        MA = Matrix(specialmata); MB = Matrix(specialmatb)
        @test hcat(specialmata, specialmatb) == hcat(MA, MB)
        @test vcat(specialmata, specialmatb) == vcat(MA, MB)
        @test hvcat((1,1), specialmata, specialmatb) == hvcat((1,1), MA, MB)
        @test cat(specialmata, specialmatb; dims=(1,2)) == cat(MA, MB; dims=(1,2))
    end
    # Test concatenating pairwise combinations of special matrices with sparse matrices,
    # dense matrices, or dense vectors
    densevec = fill(1., N)
    densemat = diagm(0 => densevec)
    for specialmat in specialmats
        SM = Matrix(specialmat)
        # --> Tests applicable only to pairs of matrices
        @test vcat(specialmat, densemat) == vcat(SM, densemat)
        @test vcat(densemat, specialmat) == vcat(densemat, SM)
        # --> Tests applicable also to pairs including vectors
        for specialmat in specialmats, othermatorvec in (densemat, densevec)
            SM = Matrix(specialmat); OM = Array(othermatorvec)
            @test hcat(specialmat, othermatorvec) == hcat(SM, OM)
            @test hcat(othermatorvec, specialmat) == hcat(OM, SM)
            @test hvcat((2,), specialmat, othermatorvec) == hvcat((2,), SM, OM)
            @test hvcat((2,), othermatorvec, specialmat) == hvcat((2,), OM, SM)
            @test cat(specialmat, othermatorvec; dims=(1,2)) == cat(SM, OM; dims=(1,2))
            @test cat(othermatorvec, specialmat; dims=(1,2)) == cat(OM, SM; dims=(1,2))
        end
    end
end

@testset "concatenations of annotated types" begin
    N = 4
    # The tested annotation types
    testfull = Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))
    utriannotations = (UpperTriangular, UnitUpperTriangular)
    ltriannotations = (LowerTriangular, UnitLowerTriangular)
    triannotations = (utriannotations..., ltriannotations...)
    symannotations = (Symmetric, Hermitian)
    annotations = testfull ? (triannotations..., symannotations...) : (LowerTriangular, Symmetric)
    # Concatenations involving these types, un/annotated
    diagmat = Diagonal(1:N)
    bidiagmat = Bidiagonal(1:N, 1:(N-1), :U)
    tridiagmat = Tridiagonal(1:(N-1), 1:N, 1:(N-1))
    symtridiagmat = SymTridiagonal(1:N, 1:(N-1))
    specialconcatmats = testfull ? (diagmat, bidiagmat, tridiagmat, symtridiagmat) : (diagmat,)
    # Concatenations involving strictly these types, un/annotated
    densevec = fill(1., N)
    densemat = fill(1., N, N)
    # Annotated collections
    annodmats = [annot(densemat) for annot in annotations]
    annospcmats = [annot(spcmat) for annot in annotations, spcmat in specialconcatmats]
    # Test concatenations of pairwise combinations of annotated special matrices
    for annospcmata in annospcmats, annospcmatb in annospcmats
        AM = Array(annospcmata); BM = Array(annospcmatb)
        @test vcat(annospcmata, annospcmatb) == vcat(AM, BM)
        @test hcat(annospcmata, annospcmatb) == hcat(AM, BM)
        @test hvcat((2,), annospcmata, annospcmatb) == hvcat((2,), AM, BM)
        @test cat(annospcmata, annospcmatb; dims=(1,2)) == cat(AM, BM; dims=(1,2))
    end
    # Test concatenations of pairwise combinations of annotated special matrices and other matrix/vector types
    for annospcmat in annospcmats
        AM = Array(annospcmat)
        # --> Tests applicable to pairs including only matrices
        for othermat in (densemat, annodmats..., specialconcatmats...)
            OM = Array(othermat)
            @test vcat(annospcmat, othermat) == vcat(AM, OM)
            @test vcat(othermat, annospcmat) == vcat(OM, AM)
        end
        # --> Tests applicable to pairs including other vectors or matrices
        for other in (densevec, densemat, annodmats..., specialconcatmats...)
            OM = Array(other)
            @test hcat(annospcmat, other) == hcat(AM, OM)
            @test hcat(other, annospcmat) == hcat(OM, AM)
            @test hvcat((2,), annospcmat, other) == hvcat((2,), AM, OM)
            @test hvcat((2,), other, annospcmat) == hvcat((2,), OM, AM)
            @test cat(annospcmat, other; dims=(1,2)) == cat(AM, OM; dims=(1,2))
            @test cat(other, annospcmat; dims=(1,2)) == cat(OM, AM; dims=(1,2))
        end
    end
    # Test concatenations strictly involving un/annotated dense matrices/vectors
    for densemata in (densemat, annodmats...)
        AM = Array(densemata)
        # --> Tests applicable to pairs including only matrices
        for densematb in (densemat, annodmats...)
            BM = Array(densematb)
            @test vcat(densemata, densematb) == vcat(AM, BM)
            @test vcat(densematb, densemata) == vcat(BM, AM)
        end
        # --> Tests applicable to pairs including vectors or matrices
        for otherdense in (densevec, densemat, annodmats...)
            OM = Array(otherdense)
            @test hcat(densemata, otherdense) == hcat(AM, OM)
            @test hcat(otherdense, densemata) == hcat(OM, AM)
            @test hvcat((2,), densemata, otherdense) == hvcat((2,), AM, OM)
            @test hvcat((2,), otherdense, densemata) == hvcat((2,), OM, AM)
            @test cat(densemata, otherdense; dims=(1,2)) == cat(AM, OM; dims=(1,2))
            @test cat(otherdense, densemata; dims=(1,2)) == cat(OM, AM; dims=(1,2))
        end
    end
end

# for testing types with a dimension
const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

@testset "zero and one for structured matrices" begin
    for elty in (Int64, Float64, ComplexF64)
        D = Diagonal(rand(elty, 10))
        Bu = Bidiagonal(rand(elty, 10), rand(elty, 9), 'U')
        Bl = Bidiagonal(rand(elty, 10), rand(elty, 9), 'L')
        T = Tridiagonal(rand(elty, 9),rand(elty, 10), rand(elty, 9))
        S = SymTridiagonal(rand(elty, 10), rand(elty, 9))
        mats = Any[D, Bu, Bl, T, S]
        for A in mats
            @test iszero(zero(A))
            @test isone(one(A))
            @test zero(A) == zero(Matrix(A))
            @test one(A) == one(Matrix(A))
        end

        @test zero(D) isa Diagonal
        @test one(D) isa Diagonal

        @test zero(Bu) isa Bidiagonal
        @test one(Bu) isa Bidiagonal
        @test zero(Bl) isa Bidiagonal
        @test one(Bl) isa Bidiagonal
        @test zero(Bu).uplo == one(Bu).uplo == Bu.uplo
        @test zero(Bl).uplo == one(Bl).uplo == Bl.uplo

        @test zero(T) isa Tridiagonal
        @test one(T) isa Tridiagonal
        @test zero(S) isa SymTridiagonal
        @test one(S) isa SymTridiagonal
    end

    # ranges
    D = Diagonal(1:10)
    Bu = Bidiagonal(1:10, 1:9, 'U')
    Bl = Bidiagonal(1:10, 1:9, 'L')
    T = Tridiagonal(1:9, 1:10, 1:9)
    S = SymTridiagonal(1:10, 1:9)
    mats = [D, Bu, Bl, T, S]
    for A in mats
        @test iszero(zero(A))
        @test isone(one(A))
        @test zero(A) == zero(Matrix(A))
        @test one(A) == one(Matrix(A))
    end

    @test zero(D) isa Diagonal
    @test one(D) isa Diagonal

    @test zero(Bu) isa Bidiagonal
    @test one(Bu) isa Bidiagonal
    @test zero(Bl) isa Bidiagonal
    @test one(Bl) isa Bidiagonal
    @test zero(Bu).uplo == one(Bu).uplo == Bu.uplo
    @test zero(Bl).uplo == one(Bl).uplo == Bl.uplo

    @test zero(T) isa Tridiagonal
    @test one(T) isa Tridiagonal
    @test zero(S) isa SymTridiagonal
    @test one(S) isa SymTridiagonal

    # eltype with dimensions
    D0 = Diagonal{Furlong{0, Int64}}([1, 2, 3, 4])
    Bu0 = Bidiagonal{Furlong{0, Int64}}([1, 2, 3, 4], [1, 2, 3], 'U')
    Bl0 =  Bidiagonal{Furlong{0, Int64}}([1, 2, 3, 4], [1, 2, 3], 'L')
    T0 = Tridiagonal{Furlong{0, Int64}}([1, 2, 3], [1, 2, 3, 4], [1, 2, 3])
    S0 = SymTridiagonal{Furlong{0, Int64}}([1, 2, 3, 4], [1, 2, 3])
    F2 = Furlongs.Furlong{2}(1)
    D2 = Diagonal{Furlong{2, Int64}}([1, 2, 3, 4].*F2)
    Bu2 = Bidiagonal{Furlong{2, Int64}}([1, 2, 3, 4].*F2, [1, 2, 3].*F2, 'U')
    Bl2 =  Bidiagonal{Furlong{2, Int64}}([1, 2, 3, 4].*F2, [1, 2, 3].*F2, 'L')
    T2 = Tridiagonal{Furlong{2, Int64}}([1, 2, 3].*F2, [1, 2, 3, 4].*F2, [1, 2, 3].*F2)
    S2 = SymTridiagonal{Furlong{2, Int64}}([1, 2, 3, 4].*F2, [1, 2, 3].*F2)
    mats = Any[D0, Bu0, Bl0, T0, S0, D2, Bu2, Bl2, T2, S2]
    for A in mats
        @test iszero(zero(A))
        @test isone(one(A))
        @test zero(A) == zero(Matrix(A))
        @test one(A) == one(Matrix(A))
        @test eltype(one(A)) == typeof(one(eltype(A)))
    end
end

@testset "== for structured matrices" begin
    diag = rand(10)
    offdiag = rand(9)
    D = Diagonal(rand(10))
    Bup = Bidiagonal(diag, offdiag, 'U')
    Blo = Bidiagonal(diag, offdiag, 'L')
    Bupd = Bidiagonal(diag, zeros(9), 'U')
    Blod = Bidiagonal(diag, zeros(9), 'L')
    T = Tridiagonal(offdiag, diag, offdiag)
    Td = Tridiagonal(zeros(9), diag, zeros(9))
    Tu = Tridiagonal(zeros(9), diag, offdiag)
    Tl = Tridiagonal(offdiag, diag, zeros(9))
    S = SymTridiagonal(diag, offdiag)
    Sd = SymTridiagonal(diag, zeros(9))

    mats = [D, Bup, Blo, Bupd, Blod, T, Td, Tu, Tl, S, Sd]

    for a in mats
        for b in mats
            @test (a == b) == (Matrix(a) == Matrix(b)) == (b == a) == (Matrix(b) == Matrix(a))
        end
    end
end

@testset "BiTriSym*Q' and Q'*BiTriSym" begin
    dl = [1, 1, 1]
    d = [1, 1, 1, 1]
    D = Diagonal(d)
    Bi = Bidiagonal(d, dl, :L)
    Tri = Tridiagonal(dl, d, dl)
    Sym = SymTridiagonal(d, dl)
    F = qr(ones(4, 1))
    A = F.Q'
    for A in (F.Q, F.Q'), B in (D, Bi, Tri, Sym)
        @test B*A ≈ Matrix(B)*A
        @test A*B ≈ A*Matrix(B)
    end
end

@testset "Ops on SymTridiagonal ev has the same length as dv" begin
    x = rand(3)
    y = rand(3)
    z = rand(2)

    S = SymTridiagonal(x, y)
    T = Tridiagonal(z, x, z)
    Bu = Bidiagonal(x, z, :U)
    Bl = Bidiagonal(x, z, :L)

    Ms = Matrix(S)
    Mt = Matrix(T)
    Mbu = Matrix(Bu)
    Mbl = Matrix(Bl)

    @test S + T ≈ Ms + Mt
    @test T + S ≈ Mt + Ms
    @test S + Bu ≈ Ms + Mbu
    @test Bu + S ≈ Mbu + Ms
    @test S + Bl ≈ Ms + Mbl
    @test Bl + S ≈ Mbl + Ms
end

@testset "Ensure Strided * (Sym)Tridiagonal is Dense" begin
    x = rand(3)
    y = rand(3)
    z = rand(2)

    l = rand(12, 12)
    # strided but not a Matrix
    v = @view l[1:4:end, 1:4:end]
    M_v = Matrix(v)
    m = rand(3, 3)

    S = SymTridiagonal(x, y)
    T = Tridiagonal(z, x, z)
    M_S = Matrix(S)
    M_T = Matrix(T)

    @test m * T ≈ m * M_T
    @test m * S ≈ m * M_S
    @test v * T ≈ M_v * T
    @test v * S ≈ M_v * S

    @test m * T isa Matrix
    @test m * S isa Matrix
    @test v * T isa Matrix
    @test v * S isa Matrix
end

end # module TestSpecial
