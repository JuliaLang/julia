# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestAbstractQ

using Test
using LinearAlgebra
using LinearAlgebra: AbstractQ, AdjointQ
import LinearAlgebra: lmul!, rmul!
import Base: size, convert

n = 5

@testset "custom AbstractQ type" begin
    struct MyQ{T,S<:AbstractQ{T}} <: AbstractQ{T}
        Q::S
    end
    MyQ{T}(Q::AbstractQ) where {T} = (P = convert(AbstractQ{T}, Q); MyQ{T,typeof(P)}(P))
    MyQ(Q::MyQ) = Q

    Base.size(Q::MyQ) = size(Q.Q)
    LinearAlgebra.lmul!(Q::MyQ, B::AbstractVecOrMat) = lmul!(Q.Q, B)
    LinearAlgebra.lmul!(adjQ::AdjointQ{<:Any,<:MyQ}, B::AbstractVecOrMat) = lmul!(parent(adjQ).Q', B)
    LinearAlgebra.rmul!(A::AbstractVecOrMat, Q::MyQ) = rmul!(A, Q.Q)
    LinearAlgebra.rmul!(A::AbstractVecOrMat, adjQ::AdjointQ{<:Any,<:MyQ}) = rmul!(A, parent(adjQ).Q')
    Base.convert(::Type{AbstractQ{T}}, Q::MyQ) where {T} = MyQ{T}(Q.Q)
    LinearAlgebra.det(Q::MyQ) = det(Q.Q)

    for T in (Float64, ComplexF64)
        A = rand(T, n, n)
        F = qr(A)
        Q = MyQ(F.Q)
        @test ndims(Q) == 2
        T <: Real && @test transpose(Q) == adjoint(Q)
        T <: Complex && @test_throws ErrorException transpose(Q)
        @test convert(AbstractQ{complex(T)}, Q) isa MyQ{complex(T)}
        @test convert(AbstractQ{complex(T)}, Q') isa AdjointQ{<:complex(T),<:MyQ{complex(T)}}
        @test *(Q) == Q
        @test Q*I ≈ Q.Q*I rtol=2eps(real(T))
        @test Q'*I ≈ Q.Q'*I rtol=2eps(real(T))
        @test I*Q ≈ Q.Q*I rtol=2eps(real(T))
        @test I*Q' ≈ I*Q.Q' rtol=2eps(real(T))
        @test Q^3 ≈ Q*Q*Q
        @test Q^2 ≈ Q*Q
        @test Q^1 == Q
        @test Q^(-1) == Q'
        @test (Q')^(-1) == Q
        @test (Q')^2 ≈ Q'*Q'
        @test abs(det(Q)) ≈ 1
        @test logabsdet(Q)[1] ≈ 0 atol=2n*eps(real(T))
        y = rand(T, n)
        @test Q * y ≈ Q.Q * y ≈ Q' \ y ≈ ldiv!(Q', copy(y)) ≈ ldiv!(zero(y), Q', y)
        @test Q'y ≈ Q.Q' * y ≈ Q \ y ≈ ldiv!(Q, copy(y)) ≈ ldiv!(zero(y), Q, y)
        @test y'Q ≈ y'Q.Q ≈ y' / Q'
        @test y'Q' ≈ y'Q.Q' ≈ y' / Q
        y = Matrix(y')
        @test y*Q ≈ y*Q.Q ≈ y / Q' ≈ rdiv!(copy(y), Q')
        @test y*Q' ≈ y*Q.Q' ≈ y / Q ≈ rdiv!(copy(y), Q)
        Y = rand(T, n, n); X = similar(Y)
        for transQ in (identity, adjoint), transY in (identity, adjoint), Y in (Y, Y')
            @test mul!(X, transQ(Q), transY(Y)) ≈ transQ(Q) * transY(Y) ≈ transQ(Q.Q) * transY(Y)
            @test mul!(X, transY(Y), transQ(Q)) ≈ transY(Y) * transQ(Q) ≈ transY(Y) * transQ(Q.Q)
        end
        @test convert(Matrix, Q) ≈ Matrix(Q) ≈ Q[:,:] ≈ copyto!(zeros(T, size(Q)), Q) ≈ Q.Q*I
        @test convert(Matrix, Q') ≈ Matrix(Q') ≈ (Q')[:,:] ≈ copyto!(zeros(T, size(Q)), Q') ≈ Q.Q'*I
        @test Q[1,:] == Q.Q[1,:] == view(Q, 1, :)
        @test Q[:,1] == Q.Q[:,1] == view(Q, :, 1)
        @test Q[1,1] == Q.Q[1,1]
        @test Q[:] == Q.Q[:]
        @test Q[:,1:3] == Q.Q[:,1:3] == view(Q, :, 1:3)
        @test Q[:,1:3] ≈ Matrix(Q)[:,1:3]
        @test Q[2:3,2:3] == view(Q, 2:3, 2:3) ≈ Matrix(Q)[2:3,2:3]
        @test_throws BoundsError Q[0,1]
        @test_throws BoundsError Q[n+1,1]
        @test_throws BoundsError Q[1,0]
        @test_throws BoundsError Q[1,n+1]
        @test_throws BoundsError Q[:,1:n+1]
        @test_throws BoundsError Q[:,0:n]
        for perm in ((1, 2), (2, 1))
            P = PermutedDimsArray(zeros(T, size(Q)), perm)
            @test copyto!(P, Q) ≈ Matrix(Q)
        end
        x = randn(T)
        @test x * Q ≈ (x*I)*Q ≈ x * Q.Q
        @test Q * x ≈ Q*(x*I) ≈ Q.Q * x
        @test x * Q' ≈ (x*I)* Q' ≈ x * Q.Q'
        @test Q' * x ≈ Q'*(x*I) ≈ Q.Q' * x
        x = rand(T, 1)
        Q = MyQ(qr(rand(T, 1, 1)).Q)
        @test x * Q ≈ x * Q.Q
        @test x * Q' ≈ x * Q.Q'
        @test Q * x ≈ Q.Q * x
        @test Q' * x ≈ Q.Q' * x
    end
    A = randn(Float64, 5, 3)
    F = qr(A)
    Q = MyQ(F.Q)
    Prect = Matrix(F.Q)
    Psquare = collect(F.Q)
    @test Q == Prect
    @test Q == Psquare
    @test Q == F.Q*I
    @test Q ≈ Prect
    @test Q ≈ Psquare
    @test Q ≈ F.Q*I

    @testset "similar" begin
        QS = similar(Q)
        @test QS isa Matrix{eltype(Q)}
        @test size(QS) == size(Q)

        QS = similar(Q, Int8)
        @test QS isa Matrix{Int8}
        @test size(QS) == size(Q)

        QS = similar(Q, 1)
        @test QS isa Vector{eltype(Q)}
        @test size(QS) == (1,)

        QS = similar(Q, Int8, 2)
        @test QS isa Vector{Int8}
        @test size(QS) == (2,)

        QS = similar(Q, Int8, ())
        @test QS isa Array{Int8,0}

        QS = similar(Q, ())
        @test QS isa Array{eltype(Q),0}
    end

    # matrix division
    q, r = F
    R = randn(Float64, 5, 5)
    @test q / r ≈ Matrix(q) / r
    @test_throws DimensionMismatch MyQ(q) / r # doesn't have size flexibility
    @test q / R ≈ collect(q) / R
    @test copy(r') \ q' ≈ (q / r)'
    @test_throws DimensionMismatch copy(r') \ MyQ(q')
    @test r \ q' ≈ r \ Matrix(q)'
    @test R \ q' ≈ R \ MyQ(q') ≈ R \ collect(q')
    @test R \ q ≈ R \ MyQ(q) ≈ R \ collect(q)
    B = copy(A')
    G = lq(B)
    l, q = G
    L = R
    @test l \ q ≈ l \ Matrix(q)
    @test_throws DimensionMismatch l \ MyQ(q)
    @test L \ q ≈ L \ collect(q)
    @test q' / copy(l') ≈ (l \ q)'
    @test_throws DimensionMismatch MyQ(q') / copy(l')
    @test q' / l ≈ Matrix(q)' / l
    @test q' / L ≈ MyQ(q') / L ≈ collect(q)' / L
    @test q / L ≈ Matrix(q) / L
    @test MyQ(q) / L ≈ collect(q) / L
end

end # module
