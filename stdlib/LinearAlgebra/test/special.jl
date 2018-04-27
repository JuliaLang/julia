# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSpecial

using Test, LinearAlgebra, SparseArrays, Random
using LinearAlgebra: rmul!

n= 10 #Size of matrix to test
srand(1)

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
end

@testset "Triangular Types and QR" begin
    for typ in [UpperTriangular,LowerTriangular,LinearAlgebra.UnitUpperTriangular,LinearAlgebra.UnitLowerTriangular]
        a = rand(n,n)
        atri = typ(a)
        b = rand(n,n)
        qrb = qrfact(b,Val(true))
        @test *(atri, adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
        @test rmul!(copy(atri), adjoint(qrb.Q)) ≈ Matrix(atri) * qrb.Q'
        qrb = qrfact(b,Val(false))
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
        @test issparse(cat((1,2), specialmata, specialmatb))
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
            @test issparse(cat((1,2), specialmat, othermatorvec))
            @test issparse(cat((1,2), othermatorvec, specialmat))
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
        @test issparse(cat((1,2), annospcmata, annospcmatb))
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
            @test issparse(cat((1,2), annospcmat, other))
            @test issparse(cat((1,2), other, annospcmat))
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
    @test issparse(cat((1,2), annodmats[1], diagmat, annospcmats[3], densevec, spvec))
    @test issparse(cat((1,2), spvec, diagmat, densevec, annospcmats[4], annodmats[2]))
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
            @test !issparse(cat((1,2), densemata, otherdense))
            @test !issparse(cat((1,2), otherdense, densemata))
        end
    end
end
@testset "vcat of Vectors with SparseVectors should yield SparseVector (#22225)" begin
    @test isa((@inferred vcat(Float64[], spzeros(1))), SparseVector)
end

end # module TestSpecial
