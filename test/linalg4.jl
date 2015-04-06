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

    for isupper in (true, false)
        debug && println("isupper is $(isupper)")
        A=Bidiagonal(a, [1.0:n-1;], isupper)
        for newtype in [Bidiagonal, Tridiagonal, isupper ? UpperTriangular : LowerTriangular, Matrix]
            debug && println("newtype is $(newtype)")
            @test full(convert(newtype, A)) == full(A)
            @test full(newtype(A)) == full(A)
        end
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

    A = Tridiagonal(zeros(n-1), [1.0:n;], zeros(n-1)) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Matrix]
        @test full(convert(newtype, A)) == full(A)
    end

    A = LowerTriangular(full(Diagonal(a))) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, LowerTriangular, Matrix]
        @test full(convert(newtype, A)) == full(A)
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
        @test_approx_eq full(cholfact(A, ul)[ul]) full(invoke(Base.LinAlg.chol!, (AbstractMatrix,Symbol),copy(A), ul))
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
