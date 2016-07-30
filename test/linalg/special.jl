# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test
debug = false

n= 10 #Size of matrix to test
srand(1)

debug && println("Test interconversion between special matrix types")
let a=[1.0:n;]
   A=Diagonal(a)
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Matrix]
       debug && println("newtype is $(newtype)")
       @test full(convert(newtype, A)) == full(A)
   end

   for isupper in (true, false)
       debug && println("isupper is $(isupper)")
       A=Bidiagonal(a, [1.0:n-1;], isupper)
       for newtype in [Bidiagonal, Tridiagonal, Matrix]
           debug && println("newtype is $(newtype)")
           @test full(convert(newtype, A)) == full(A)
           @test full(newtype(A)) == full(A)
       end
       @test_throws ArgumentError convert(SymTridiagonal, A)
       tritype = isupper ? UpperTriangular : LowerTriangular
       @test full(tritype(A)) == full(A)

       A=Bidiagonal(a, zeros(n-1), isupper) #morally Diagonal
       for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Matrix]
           debug && println("newtype is $(newtype)")
           @test full(convert(newtype, A)) == full(A)
           @test full(newtype(A)) == full(A)
       end
       @test full(tritype(A)) == full(A)
   end

   A = SymTridiagonal(a, [1.0:n-1;])
   for newtype in [Tridiagonal, Matrix]
       @test full(convert(newtype, A)) == full(A)
   end
   for newtype in [Diagonal, Bidiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
   A = SymTridiagonal(a, zeros(n-1))
   @test full(convert(Bidiagonal,A)) == full(A)

   A = Tridiagonal(zeros(n-1), [1.0:n;], zeros(n-1)) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Matrix]
       @test full(convert(newtype, A)) == full(A)
   end
   A = Tridiagonal(ones(n-1), [1.0:n;], ones(n-1)) #not morally Diagonal
   for newtype in [SymTridiagonal, Matrix]
       @test full(convert(newtype, A)) == full(A)
   end
   for newtype in [Diagonal, Bidiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
   A = Tridiagonal(zeros(n-1), [1.0:n;], ones(n-1)) #not morally Diagonal
   @test full(convert(Bidiagonal, A)) == full(A)
   A = UpperTriangular(Tridiagonal(zeros(n-1), [1.0:n;], ones(n-1)))
   @test full(convert(Bidiagonal, A)) == full(A)
   A = Tridiagonal(ones(n-1), [1.0:n;], zeros(n-1)) #not morally Diagonal
   @test full(convert(Bidiagonal, A)) == full(A)
   A = LowerTriangular(Tridiagonal(ones(n-1), [1.0:n;], zeros(n-1)))
   @test full(convert(Bidiagonal, A)) == full(A)
   @test_throws ArgumentError convert(SymTridiagonal,A)

   A = LowerTriangular(full(Diagonal(a))) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, LowerTriangular, Matrix]
       @test full(convert(newtype, A)) == full(A)
   end
   A = UpperTriangular(full(Diagonal(a))) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, UpperTriangular, Matrix]
       @test full(convert(newtype, A)) == full(A)
   end
   A = UpperTriangular(triu(rand(n,n)))
   for newtype in [Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
end

# Binary ops among special types
let a=[1.0:n;]
   A=Diagonal(a)
   Spectypes = [Diagonal, Bidiagonal, Tridiagonal, Matrix]
   for (idx, type1) in enumerate(Spectypes)
       for type2 in Spectypes
           B = convert(type1,A)
           C = convert(type2,A)
           @test full(B + C) ≈ full(A + A)
           @test full(B - C) ≈ full(A - A)
       end
   end
   B = SymTridiagonal(a, ones(n-1))
   for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
       @test full(B + convert(Spectype,A)) ≈ full(B + A)
       @test full(convert(Spectype,A) + B) ≈ full(B + A)
       @test full(B - convert(Spectype,A)) ≈ full(B - A)
       @test full(convert(Spectype,A) - B) ≈ full(A - B)
   end

   C = rand(n,n)
   for TriType in [Base.LinAlg.UnitLowerTriangular, Base.LinAlg.UnitUpperTriangular, UpperTriangular, LowerTriangular]
       D = TriType(C)
       for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
           @test full(D + convert(Spectype,A)) ≈ full(D + A)
           @test full(convert(Spectype,A) + D) ≈ full(A + D)
           @test full(D - convert(Spectype,A)) ≈ full(D - A)
           @test full(convert(Spectype,A) - D) ≈ full(A - D)
       end
   end
end

#Triangular Types and QR
for typ in [UpperTriangular,LowerTriangular,Base.LinAlg.UnitUpperTriangular,Base.LinAlg.UnitLowerTriangular]
    a = rand(n,n)
    atri = typ(a)
    b = rand(n,n)
    qrb = qrfact(b,Val{true})
    @test Base.LinAlg.A_mul_Bc(atri,qrb[:Q]) ≈ full(atri) * qrb[:Q]'
    @test Base.LinAlg.A_mul_Bc!(copy(atri),qrb[:Q]) ≈ full(atri) * qrb[:Q]'
    qrb = qrfact(b,Val{false})
    @test Base.LinAlg.A_mul_Bc(atri,qrb[:Q]) ≈ full(atri) * qrb[:Q]'
    @test Base.LinAlg.A_mul_Bc!(copy(atri),qrb[:Q]) ≈ full(atri) * qrb[:Q]'
end

# Test that concatenations of combinations of special and other matrix types yield sparse arrays
let
    N = 4
    # Test concatenating pairwise combinations of special matrices
    diagmat = Diagonal(ones(N))
    bidiagmat = Bidiagonal(ones(N), ones(N-1), true)
    tridiagmat = Tridiagonal(ones(N-1), ones(N), ones(N-1))
    symtridiagmat = SymTridiagonal(ones(N), ones(N-1))
    specialmats = (diagmat, bidiagmat, tridiagmat, symtridiagmat)
    for specialmata in specialmats, specialmatb in specialmats
        @test issparse(hcat(specialmata, specialmatb))
        @test issparse(vcat(specialmata, specialmatb))
        @test issparse(hvcat((1,1), specialmata, specialmatb))
        @test issparse(cat((1,2), specialmata, specialmatb))
    end
    # Test concatenating pairwise combinations of special matrices with sparse matrices,
    # dense matrices, or dense vectors
    densevec = ones(N)
    densemat = diagm(ones(N))
    spmat = spdiagm(ones(N))
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
