# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test
debug = false

n= 10 #Size of matrix to test
srand(1)

debug && println("Test interconversion between special matrix types")
let a=[1.0:n;]
   A=Diagonal(a)
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, LowerTriangular, UpperTriangular, Matrix]
       debug && println("newtype is $(newtype)")
       @test full(convert(newtype, A)) == full(A)
   end
   for newtype in [Base.LinAlg.UnitUpperTriangular, Base.LinAlg.UnitLowerTriangular]
       @test_throws ArgumentError convert(newtype, A)
       @test full(convert(newtype, Diagonal(ones(n)))) == eye(n)
   end

   for isupper in (true, false)
       debug && println("isupper is $(isupper)")
       A=Bidiagonal(a, [1.0:n-1;], isupper)
       for newtype in [Bidiagonal, Tridiagonal, isupper ? UpperTriangular : LowerTriangular, Matrix]
           debug && println("newtype is $(newtype)")
           @test full(convert(newtype, A)) == full(A)
           @test full(newtype(A)) == full(A)
       end
       @test_throws ArgumentError convert(SymTridiagonal, A)
       A=Bidiagonal(a, zeros(n-1), isupper) #morally Diagonal
       for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, isupper ? UpperTriangular : LowerTriangular, Matrix]
           debug && println("newtype is $(newtype)")
           @test full(convert(newtype, A)) == full(A)
           @test full(newtype(A)) == full(A)
       end
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
   A = Diagonal(a)
   for newtype in [UpperTriangular, LowerTriangular]
       @test full(convert(newtype,A)) == full(A)
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

# Test conversion from Bidiagonal to <:AbstractTriangular
let
    lowerbidiagmat = Bidiagonal(rand(3), rand(2), false)
    upperbidiagmat = Bidiagonal(rand(3), rand(2), true)
    unitlowerbidiagmat = Bidiagonal(ones(3), rand(2), false)
    unitupperbidiagmat = Bidiagonal(ones(3), rand(2), true)
    # test that conversion from upper(lower) bidiagonal to lower(upper) [unit]triangular throws
    @test_throws ArgumentError convert(UpperTriangular, unitlowerbidiagmat)
    @test_throws ArgumentError convert(LowerTriangular, unitupperbidiagmat)
    @test_throws ArgumentError convert(Base.LinAlg.UnitUpperTriangular, unitlowerbidiagmat)
    @test_throws ArgumentError convert(Base.LinAlg.UnitLowerTriangular, unitupperbidiagmat)
    # test that conversion from non-unit bidiag to unit triangular throws
    @test_throws ArgumentError convert(Base.LinAlg.UnitUpperTriangular, upperbidiagmat)
    @test_throws ArgumentError convert(Base.LinAlg.UnitLowerTriangular, lowerbidiagmat)
    # test that conversion from bidiagonal to triangular preserves bidiagonal storage structure
    @test typeof(convert(UpperTriangular, upperbidiagmat)) ==
        UpperTriangular{eltype(upperbidiagmat),Bidiagonal{eltype(upperbidiagmat)}}
    @test typeof(convert(LowerTriangular, lowerbidiagmat)) ==
        LowerTriangular{eltype(lowerbidiagmat),Bidiagonal{eltype(lowerbidiagmat)}}
    @test typeof(convert(Base.LinAlg.UnitUpperTriangular, unitupperbidiagmat)) ==
        Base.LinAlg.UnitUpperTriangular{eltype(unitupperbidiagmat),Bidiagonal{eltype(unitupperbidiagmat)}}
    @test typeof(convert(Base.LinAlg.UnitLowerTriangular, unitlowerbidiagmat)) ==
        Base.LinAlg.UnitLowerTriangular{eltype(unitlowerbidiagmat),Bidiagonal{eltype(unitlowerbidiagmat)}}
end
