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
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   for newtype in [Base.LinAlg.UnitUpperTriangular, Base.LinAlg.UnitLowerTriangular]
       @test_throws ArgumentError convert(newtype, A)
       @test convert(Array, convert(newtype, Diagonal(ones(n)))) == eye(n)
   end

   for isupper in (true, false)
       debug && println("isupper is $(isupper)")
       A=Bidiagonal(a, [1.0:n-1;], isupper)
       for newtype in [Bidiagonal, Tridiagonal, isupper ? UpperTriangular : LowerTriangular, Matrix]
           debug && println("newtype is $(newtype)")
           @test convert(Array, convert(newtype, A)) == convert(Array, A)
           @test convert(Array, newtype(A)) == convert(Array, A)
       end
       @test_throws ArgumentError convert(SymTridiagonal, A)
       A=Bidiagonal(a, zeros(n-1), isupper) #morally Diagonal
       for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, isupper ? UpperTriangular : LowerTriangular, Matrix]
           debug && println("newtype is $(newtype)")
           @test convert(Array, convert(newtype, A)) == convert(Array, A)
           @test convert(Array, newtype(A)) == convert(Array, A)
       end
   end

   A = SymTridiagonal(a, [1.0:n-1;])
   for newtype in [Tridiagonal, Matrix]
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   for newtype in [Diagonal, Bidiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
   A = SymTridiagonal(a, zeros(n-1))
   @test convert(Array, convert(Bidiagonal,A)) == convert(Array, A)

   A = Tridiagonal(zeros(n-1), [1.0:n;], zeros(n-1)) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Matrix]
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   A = Tridiagonal(ones(n-1), [1.0:n;], ones(n-1)) #not morally Diagonal
   for newtype in [SymTridiagonal, Matrix]
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   for newtype in [Diagonal, Bidiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
   A = Tridiagonal(zeros(n-1), [1.0:n;], ones(n-1)) #not morally Diagonal
   @test convert(Array, convert(Bidiagonal, A)) == convert(Array, A)
   A = UpperTriangular(Tridiagonal(zeros(n-1), [1.0:n;], ones(n-1)))
   @test convert(Array, convert(Bidiagonal, A)) == convert(Array, A)
   A = Tridiagonal(ones(n-1), [1.0:n;], zeros(n-1)) #not morally Diagonal
   @test convert(Array, convert(Bidiagonal, A)) == convert(Array, A)
   A = LowerTriangular(Tridiagonal(ones(n-1), [1.0:n;], zeros(n-1)))
   @test convert(Array, convert(Bidiagonal, A)) == convert(Array, A)
   @test_throws ArgumentError convert(SymTridiagonal,A)

   A = LowerTriangular(convert(Array, Diagonal(a))) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, LowerTriangular, Matrix]
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   A = UpperTriangular(convert(Array, Diagonal(a))) #morally Diagonal
   for newtype in [Diagonal, Bidiagonal, SymTridiagonal, UpperTriangular, Matrix]
       @test convert(Array, convert(newtype, A)) == convert(Array, A)
   end
   A = UpperTriangular(triu(rand(n,n)))
   for newtype in [Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal]
       @test_throws ArgumentError convert(newtype,A)
   end
   A = Diagonal(a)
   for newtype in [UpperTriangular, LowerTriangular]
       @test convert(Array, convert(newtype,A)) == convert(Array, A)
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
           @test convert(Array, B + C) ≈ convert(Array, A + A)
           @test convert(Array, B - C) ≈ convert(Array, A - A)
       end
   end
   B = SymTridiagonal(a, ones(n-1))
   for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
       @test convert(Array, B + convert(Spectype,A)) ≈ convert(Array, B + A)
       @test convert(Array, convert(Spectype,A) + B) ≈ convert(Array, B + A)
       @test convert(Array, B - convert(Spectype,A)) ≈ convert(Array, B - A)
       @test convert(Array, convert(Spectype,A) - B) ≈ convert(Array, A - B)
   end

   C = rand(n,n)
   for TriType in [Base.LinAlg.UnitLowerTriangular, Base.LinAlg.UnitUpperTriangular, UpperTriangular, LowerTriangular]
       D = TriType(C)
       for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
           @test convert(Array, D + convert(Spectype,A)) ≈ convert(Array, D + A)
           @test convert(Array, convert(Spectype,A) + D) ≈ convert(Array, A + D)
           @test convert(Array, D - convert(Spectype,A)) ≈ convert(Array, D - A)
           @test convert(Array, convert(Spectype,A) - D) ≈ convert(Array, A - D)
       end
   end
end

#Triangular Types and QR
for typ in [UpperTriangular,LowerTriangular,Base.LinAlg.UnitUpperTriangular,Base.LinAlg.UnitLowerTriangular]
    a = rand(n,n)
    atri = typ(a)
    b = rand(n,n)
    qrb = qrfact(b,Val{true})
    @test Base.LinAlg.A_mul_Bc(atri,qrb[:Q]) ≈ convert(Array, atri) * qrb[:Q]'
    @test Base.LinAlg.A_mul_Bc!(copy(atri),qrb[:Q]) ≈ convert(Array, atri) * qrb[:Q]'
    qrb = qrfact(b,Val{false})
    @test Base.LinAlg.A_mul_Bc(atri,qrb[:Q]) ≈ convert(Array, atri) * qrb[:Q]'
    @test Base.LinAlg.A_mul_Bc!(copy(atri),qrb[:Q]) ≈ convert(Array, atri) * qrb[:Q]'
end
