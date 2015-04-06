debug = false

import Base.LinAlg
import Base.LinAlg: BlasComplex, BlasFloat, BlasReal

srand(1)

#Test equivalence of eigenvectors/singular vectors taking into account possible phase (sign) differences
function test_approx_eq_vecs{S<:Real,T<:Real}(a::StridedVecOrMat{S}, b::StridedVecOrMat{T}, error=nothing)
    n = size(a, 1)
    @test n==size(b,1) && size(a,2)==size(b,2)
    error==nothing && (error=n^3*(eps(S)+eps(T)))
    for i=1:n
        ev1, ev2 = a[:,i], b[:,i]
        deviation = min(abs(norm(ev1-ev2)),abs(norm(ev1+ev2)))
        if !isnan(deviation)
            @test_approx_eq_eps deviation 0.0 error
        end
    end
end

##############################
# Tests for special matrices #
##############################

n=12 #Size of matrix problem to test

debug && println("Diagonal matrices")
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    debug && println("elty is $(elty), relty is $(relty)")
    d=convert(Vector{elty}, randn(n))
    v=convert(Vector{elty}, randn(n))
    U=convert(Matrix{elty}, randn(n,n))
    if elty <: Complex
        d+=im*convert(Vector{elty}, randn(n))
        v+=im*convert(Vector{elty}, randn(n))
        U+=im*convert(Matrix{elty}, randn(n,n))
    end
    D = Diagonal(d)
    DM = diagm(d)

    debug && println("Linear solve")
    @test_approx_eq_eps D*v DM*v n*eps(relty)*(elty<:Complex ? 2:1)
    @test_approx_eq_eps D*U DM*U n^2*eps(relty)*(elty<:Complex ? 2:1)
    if relty != BigFloat
        @test_approx_eq_eps D\v DM\v 2n^2*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps D\U DM\U 2n^3*eps(relty)*(elty<:Complex ? 2:1)
    end

    debug && println("Simple unary functions")
    for func in (det, trace)
        @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)*(elty<:Complex ? 2:1)
    end
    if relty <: BlasFloat
        for func in (expm,)
            @test_approx_eq_eps func(D) func(DM) n^3*eps(relty)
        end
    end
    if elty <: BlasComplex
        for func in (logdet, sqrtm)
            @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)*2
        end
    end
    debug && println("Binary operations")
    d = convert(Vector{elty}, randn(n))
    D2 = Diagonal(d)
    DM2= diagm(d)
    for op in (+, -, *)
        @test_approx_eq full(op(D, D2)) op(DM, DM2)
    end

    #10036
    @test issym(D2)
    @test ishermitian(D2)
    if elty <: Complex
        dc = d + im*convert(Vector{elty}, ones(n))
        D3 = Diagonal(dc)
        @test issym(D3)
        @test !ishermitian(D3)
    end
end


debug && println("Test interconversion between special matrix types")
a=[1.0:n;]
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
A = eye(4)
@test diag(A) == ones(4)
@test diag(sub(A, 1:3, 1:3)) == ones(3)
