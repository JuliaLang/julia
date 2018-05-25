module TestStructuredBroadcast
using Test, LinearAlgebra

@testset "broadcast[!] over combinations of scalars, structured matrices, and dense vectors/matrices" begin
    N = 10
    s = rand()
    fV = rand(N)
    fA = rand(N, N)
    Z = copy(fA)
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), :U)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    U = UpperTriangular(rand(N,N))
    L = LowerTriangular(rand(N,N))
    structuredarrays = (D, B, T, U, L)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test (Q = broadcast(sin, X); typeof(Q) == typeof(X) && Q == broadcast(sin, fX))
        @test broadcast!(sin, Z, X) == broadcast(sin, fX)
        @test (Q = broadcast(cos, X); Q isa Matrix && Q == broadcast(cos, fX))
        @test broadcast!(cos, Z, X) == broadcast(cos, fX)
        @test (Q = broadcast(*, s, X); typeof(Q) == typeof(X) && Q == broadcast(*, s, fX))
        @test broadcast!(*, Z, s, X) == broadcast(*, s, fX)
        @test (Q = broadcast(+, fV, fA, X); Q isa Matrix && Q == broadcast(+, fV, fA, fX))
        @test broadcast!(+, Z, fV, fA, X) == broadcast(+, fV, fA, fX)
        @test (Q = broadcast(*, s, fV, fA, X); Q isa Matrix && Q == broadcast(*, s, fV, fA, fX))
        @test broadcast!(*, Z, s, fV, fA, X) == broadcast(*, s, fV, fA, fX)
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test broadcast(+, X, Y) == broadcast(+, fX, fY)
            @test broadcast!(+, Z, X, Y) == broadcast(+, fX, fY)
            @test broadcast(*, X, Y) == broadcast(*, fX, fY)
            @test broadcast!(*, Z, X, Y) == broadcast(*, fX, fY)
        end
    end
    diagonals = (D, B, T)
    fdiagonals = map(Array, diagonals)
    for (X, fX) in zip(diagonals, fdiagonals)
        for (Y, fY) in zip(diagonals, fdiagonals)
            @test broadcast(+, X, Y)::Union{Diagonal,Bidiagonal,Tridiagonal} == broadcast(+, fX, fY)
            @test broadcast!(+, Z, X, Y) == broadcast(+, fX, fY)
            @test broadcast(*, X, Y)::Union{Diagonal,Bidiagonal,Tridiagonal} == broadcast(*, fX, fY)
            @test broadcast!(*, Z, X, Y) == broadcast(*, fX, fY)
        end
    end
end

@testset "broadcast! where the destination is a structured matrix" begin
    N = 5
    A = rand(N, N)
    sA = A + copy(A')
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), :U)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    @test broadcast!(sin, copy(D), D) == Diagonal(sin.(D))
    @test broadcast!(sin, copy(B), B) == Bidiagonal(sin.(B), :U)
    @test broadcast!(sin, copy(T), T) == Tridiagonal(sin.(T))
    @test broadcast!(*, copy(D), D, A) == Diagonal(broadcast(*, D, A))
    @test broadcast!(*, copy(B), B, A) == Bidiagonal(broadcast(*, B, A), :U)
    @test broadcast!(*, copy(T), T, A) == Tridiagonal(broadcast(*, T, A))
end

@testset "map[!] over combinations of structured matrices" begin
    N = 10
    fA = rand(N, N)
    Z = copy(fA)
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), :U)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    U = UpperTriangular(rand(N,N))
    L = LowerTriangular(rand(N,N))
    structuredarrays = (D, B, T, U, L)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test (Q = map(sin, X); typeof(Q) == typeof(X) && Q == map(sin, fX))
        @test map!(sin, Z, X) == map(sin, fX)
        @test (Q = map(cos, X); Q isa Matrix && Q == map(cos, fX))
        @test map!(cos, Z, X) == map(cos, fX)
        @test (Q = map(+, fA, X); Q isa Matrix && Q == map(+, fA, fX))
        @test map!(+, Z, fA, X) == map(+, fA, fX)
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test map(+, X, Y) == map(+, fX, fY)
            @test map!(+, Z, X, Y) == map(+, fX, fY)
            @test map(*, X, Y) == map(*, fX, fY)
            @test map!(*, Z, X, Y) == map(*, fX, fY)
            @test map(+, X, fA, Y) == map(+, fX, fA, fY)
            @test map!(+, Z, X, fA, Y) == map(+, fX, fA, fY)
        end
    end
    diagonals = (D, B, T)
    fdiagonals = map(Array, diagonals)
    for (X, fX) in zip(diagonals, fdiagonals)
        for (Y, fY) in zip(diagonals, fdiagonals)
            @test map(+, X, Y)::Union{Diagonal,Bidiagonal,Tridiagonal} == broadcast(+, fX, fY)
            @test map!(+, Z, X, Y) == broadcast(+, fX, fY)
            @test map(*, X, Y)::Union{Diagonal,Bidiagonal,Tridiagonal} == broadcast(*, fX, fY)
            @test map!(*, Z, X, Y) == broadcast(*, fX, fY)
        end
    end
end

end
