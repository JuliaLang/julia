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
    LinearAlgebra.rmul!(A::AbstractMatrix, Q::MyQ) = rmul!(A, Q.Q)
    LinearAlgebra.rmul!(A::AbstractMatrix, adjQ::AdjointQ{<:Any,<:MyQ}) = rmul!(A, parent(adjQ).Q')
    Base.convert(::Type{AbstractQ{T}}, Q::MyQ) where {T} = MyQ{T}(Q.Q)

    for T in (Float64, ComplexF64)
        A = rand(T, n, n)
        F = qr(A)
        Q = MyQ(F.Q)
        @test convert(AbstractQ{complex(T)}, Q) isa MyQ{complex(T)}
        @test convert(AbstractQ{complex(T)}, Q') isa AdjointQ{<:complex(T),<:MyQ{complex(T)}}
        @test Q*I ≈ Q.Q*I rtol=2eps()
        @test Q'*I ≈ Q.Q'*I rtol=2eps()
        @test I*Q ≈ Q.Q*I rtol=2eps()
        @test I*Q' ≈ I*Q.Q' rtol=2eps()
        y = rand(T, n)
        @test Q * y ≈ Q.Q * y
        @test Q' * y ≈ Q.Q' * y
        Y = rand(T, n, n); X = similar(Y)
        for transQ in (identity, adjoint), transY in (identity, adjoint), Y in (Y, Y')
            @test mul!(X, transQ(Q), transY(Y)) ≈ transQ(Q) * transY(Y) ≈ transQ(Q.Q) * transY(Y)
            @test mul!(X, transY(Y), transQ(Q)) ≈ transY(Y) * transQ(Q) ≈ transY(Y) * transQ(Q.Q)
        end
        @test Matrix(Q) ≈ Q[:,:] ≈ copyto!(zeros(T, size(Q)), Q) ≈ Q.Q*I
        @test Matrix(Q') ≈ (Q')[:,:] ≈ copyto!(zeros(T, size(Q)), Q') ≈ Q.Q'*I
        @test Q[1,:] == Q.Q[1,:]
        @test Q[:,1] == Q.Q[:,1]
        @test Q[1,1] == Q.Q[1,1]
        @test Q[:] == Q.Q[:]
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
end

end # module
