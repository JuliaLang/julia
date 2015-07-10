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
            @test_approx_eq full(B + C) full(A + A)
            @test_approx_eq full(B - C) full(A - A)
        end
    end
    B = SymTridiagonal(a, ones(n-1))
    for Spectype in [Diagonal, Bidiagonal, Tridiagonal, Matrix]
        @test_approx_eq full(B + convert(Spectype,A)) full(B + A)
        @test_approx_eq full(convert(Spectype,A) + B) full(B + A)
        @test_approx_eq full(B - convert(Spectype,A)) full(B - A)
        @test_approx_eq full(convert(Spectype,A) - B) full(A - B)
    end
end

# Test generic cholfact!
for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    if elty <: Complex
        A = complex(randn(5,5), randn(5,5))
    else
        A = randn(5,5)
    end
    A = convert(Matrix{elty}, A'A)
    for ul in (:U, :L)
        @test_approx_eq full(cholfact(A, ul)[ul]) full(invoke(Base.LinAlg.chol!, Tuple{AbstractMatrix, Type{Val{ul}}},copy(A), Val{ul}))
    end
end

# Because transpose(x) == x
@test_throws ErrorException transpose(qrfact(randn(3,3)))
@test_throws ErrorException ctranspose(qrfact(randn(3,3)))
@test_throws ErrorException transpose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException ctranspose(qrfact(randn(3,3), Val{false}))
@test_throws ErrorException transpose(qrfact(big(randn(3,3))))
@test_throws ErrorException ctranspose(qrfact(big(randn(3,3))))
@test_throws ErrorException transpose(sub(sprandn(10, 10, 0.3), 1:4, 1:4))
@test_throws ErrorException ctranspose(sub(sprandn(10, 10, 0.3), 1:4, 1:4))

# test diag
let A = eye(4)
    @test diag(A) == ones(4)
    @test diag(sub(A, 1:3, 1:3)) == ones(3)
end

# test triu/tril bounds checking
A = rand(5,7)
@test_throws(BoundsError,triu(A,8))
@test_throws(BoundsError,triu(A,-6))
@test_throws(BoundsError,tril(A,8))
@test_throws(BoundsError,tril(A,-6))

# test generic axpy
x = ['a','b','c','d','e']
y = ['a','b','c','d','e']
α = 'f'
@test_throws DimensionMismatch Base.LinAlg.axpy!(α,x,['g'])
@test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(-1:5),y,collect(1:7))
@test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(1:7),y,collect(1:7))
