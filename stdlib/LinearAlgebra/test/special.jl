# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSpecial

using Test, LinearAlgebra, SparseArrays, Random
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
    mats = [UpTri, LoTri, Diag, Tridiag, UpBi, LoBi, Sym, Dense]

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
    uniformscalingmats = [UniformScaling(3), UniformScaling(1.0), UniformScaling(3//5), UniformScaling(Complex{Float64}(1.3, 3.5))]
    mats = [Diagonal(diag), Bidiagonal(diag, offdiag, 'U'), Bidiagonal(diag, offdiag, 'L'), Tridiagonal(offdiag, diag, offdiag), SymTridiagonal(diag, offdiag)]
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
    for typ in [UpperTriangular,LowerTriangular,LinearAlgebra.UnitUpperTriangular,LinearAlgebra.UnitLowerTriangular]
        a = rand(n,n)
        atri = typ(a)
        b = rand(n,n)
        qrb = qr(b,Val(true))
        @test *(atri, adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
        @test rmul!(copy(atri), adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
        qrb = qr(b,Val(false))
        @test *(atri, adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
        @test rmul!(copy(atri), adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
    end
end

# should all yield sparse arrays
@testset "concatenations of combinations of special and other matrix types" begin
    N = 4
    # Test concatenating pairwise combinations of special matrices
    diagmat = Diagonal(1:N)
    bidiagmat = Bidiagonal(1:N, 1:(N-1), :U)
    tridiagmat = Tridiagonal(1:(N-1), 1:N, 1:(N-1))
    symtridiagmat = SymTridiagonal(1:N, 1:(N-1))
    specialmats = (diagmat, bidiagmat, tridiagmat, symtridiagmat)
    for specialmata in specialmats, specialmatb in specialmats
        @test issparse(hcat(specialmata, specialmatb))
        @test issparse(vcat(specialmata, specialmatb))
        @test issparse(hvcat((1,1), specialmata, specialmatb))
        @test issparse(cat(specialmata, specialmatb; dims=(1,2)))
    end
    # Test concatenating pairwise combinations of special matrices with sparse matrices,
    # dense matrices, or dense vectors
    densevec = fill(1., N)
    densemat = diagm(0 => densevec)
    spmat = spdiagm(0 => densevec)
    for specialmat in specialmats
        # --> Tests applicable only to pairs of matrices
        for othermat in (spmat, densemat)
            @test issparse(vcat(specialmat, othermat))
            @test issparse(vcat(othermat, specialmat))
        end
        # --> Tests applicable also to pairs including vectors
        for specialmat in specialmats, othermatorvec in (spmat, densemat, densevec)
            @test issparse(hcat(specialmat, othermatorvec))
            @test issparse(hcat(othermatorvec, specialmat))
            @test issparse(hvcat((2,), specialmat, othermatorvec))
            @test issparse(hvcat((2,), othermatorvec, specialmat))
            @test issparse(cat(specialmat, othermatorvec; dims=(1,2)))
            @test issparse(cat(othermatorvec, specialmat; dims=(1,2)))
        end
    end
end

# Test that concatenations of annotated sparse/special matrix types with other matrix
# types yield sparse arrays, and that the code which effects that does not make concatenations
# strictly involving un/annotated dense matrices yield sparse arrays
#
# TODO: As with the associated code, these tests should be moved to a more appropriate
# location, particularly some future equivalent of base/linalg/special.jl dedicated to
# intereactions between a broader set of matrix types
@testset "concatenations of annotated types" begin
    N = 4
    # The tested annotation types
    testfull = Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))
    utriannotations = (UpperTriangular, LinearAlgebra.UnitUpperTriangular)
    ltriannotations = (LowerTriangular, LinearAlgebra.UnitLowerTriangular)
    triannotations = (utriannotations..., ltriannotations...)
    symannotations = (Symmetric, Hermitian)
    annotations = testfull ? (triannotations..., symannotations...) : (LowerTriangular, Symmetric)
    # Concatenations involving these types, un/annotated, should yield sparse arrays
    spvec = spzeros(N)
    spmat = sparse(1.0I, N, N)
    diagmat = Diagonal(1:N)
    bidiagmat = Bidiagonal(1:N, 1:(N-1), :U)
    tridiagmat = Tridiagonal(1:(N-1), 1:N, 1:(N-1))
    symtridiagmat = SymTridiagonal(1:N, 1:(N-1))
    sparseconcatmats = testfull ? (spmat, diagmat, bidiagmat, tridiagmat, symtridiagmat) : (spmat, diagmat)
    # Concatenations involving strictly these types, un/annotated, should yield dense arrays
    densevec = fill(1., N)
    densemat = fill(1., N, N)
    # Annotated collections
    annodmats = [annot(densemat) for annot in annotations]
    annospcmats = [annot(spcmat) for annot in annotations, spcmat in sparseconcatmats]
    # Test that concatenations of pairwise combinations of annotated sparse/special
    # yield sparse matrices
    for annospcmata in annospcmats, annospcmatb in annospcmats
        @test issparse(vcat(annospcmata, annospcmatb))
        @test issparse(hcat(annospcmata, annospcmatb))
        @test issparse(hvcat((2,), annospcmata, annospcmatb))
        @test issparse(cat(annospcmata, annospcmatb; dims=(1,2)))
    end
    # Test that concatenations of pairwise combinations of annotated sparse/special
    # matrices and other matrix/vector types yield sparse matrices
    for annospcmat in annospcmats
        # --> Tests applicable to pairs including only matrices
        for othermat in (densemat, annodmats..., sparseconcatmats...)
            @test issparse(vcat(annospcmat, othermat))
            @test issparse(vcat(othermat, annospcmat))
        end
        # --> Tests applicable to pairs including other vectors or matrices
        for other in (spvec, densevec, densemat, annodmats..., sparseconcatmats...)
            @test issparse(hcat(annospcmat, other))
            @test issparse(hcat(other, annospcmat))
            @test issparse(hvcat((2,), annospcmat, other))
            @test issparse(hvcat((2,), other, annospcmat))
            @test issparse(cat(annospcmat, other; dims=(1,2)))
            @test issparse(cat(other, annospcmat; dims=(1,2)))
        end
    end
    # The preceding tests should cover multi-way combinations of those types, but for good
    # measure test a few multi-way combinations involving those types
    @test issparse(vcat(spmat, densemat, annospcmats[1], annodmats[2]))
    @test issparse(vcat(densemat, spmat, annodmats[1], annospcmats[2]))
    @test issparse(hcat(spvec, annodmats[1], annospcmats[3], densevec, diagmat))
    @test issparse(hcat(annodmats[2], annospcmats[4], spvec, densevec, diagmat))
    @test issparse(hvcat((5,), diagmat, densevec, spvec, annodmats[1], annospcmats[1]))
    @test issparse(hvcat((5,), spvec, annodmats[2], diagmat, densevec, annospcmats[2]))
    @test issparse(cat(annodmats[1], diagmat, annospcmats[3], densevec, spvec; dims=(1,2)))
    @test issparse(cat(spvec, diagmat, densevec, annospcmats[4], annodmats[2]; dims=(1,2)))
    # Test that concatenations strictly involving un/annotated dense matrices/vectors
    # yield dense arrays
    for densemata in (densemat, annodmats...)
        # --> Tests applicable to pairs including only matrices
        for densematb in (densemat, annodmats...)
            @test !issparse(vcat(densemata, densematb))
            @test !issparse(vcat(densematb, densemata))
        end
        # --> Tests applicable to pairs including vectors or matrices
        for otherdense in (densevec, densemat, annodmats...)
            @test !issparse(hcat(densemata, otherdense))
            @test !issparse(hcat(otherdense, densemata))
            @test !issparse(hvcat((2,), densemata, otherdense))
            @test !issparse(hvcat((2,), otherdense, densemata))
            @test !issparse(cat(densemata, otherdense; dims=(1,2)))
            @test !issparse(cat(otherdense, densemata; dims=(1,2)))
        end
    end
end
@testset "vcat of Vectors with SparseVectors should yield SparseVector (#22225)" begin
    @test isa((@inferred vcat(Float64[], spzeros(1))), SparseVector)
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
    D = Diagonal{Furlong{2, Int64}}([1, 2, 3, 4])
    Bu = Bidiagonal{Furlong{2, Int64}}([1, 2, 3, 4], [1, 2, 3], 'U')
    Bl =  Bidiagonal{Furlong{2, Int64}}([1, 2, 3, 4], [1, 2, 3], 'L')
    T = Tridiagonal{Furlong{2, Int64}}([1, 2, 3], [1, 2, 3, 4], [1, 2, 3])
    S = SymTridiagonal{Furlong{2, Int64}}([1, 2, 3, 4], [1, 2, 3])
    mats = [D, Bu, Bl, T, S]
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

end # module TestSpecial
