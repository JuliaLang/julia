# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    M = Matrix(rand(N,N))
    structuredarrays = (D, B, T, U, L, M)
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

        @test X .* 2.0 == X .* (2.0,) == fX .* 2.0
        @test X .* 2.0 isa typeof(X)
        @test X .* (2.0,) isa typeof(X)
        @test isequal(X .* Inf, fX .* Inf)

        two = 2
        @test X .^ 2 ==  X .^ (2,) == fX .^ 2 == X .^ two
        @test X .^ 2 isa typeof(X)
        @test X .^ (2,) isa typeof(X)
        @test X .^ two isa typeof(X)
        @test X .^ 0 == fX .^ 0
        @test X .^ -1 == fX .^ -1

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
    Bu = Bidiagonal(rand(N), rand(N - 1), :U)
    Bl = Bidiagonal(rand(N), rand(N - 1), :L)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    ◣ = LowerTriangular(rand(N,N))
    ◥ = UpperTriangular(rand(N,N))
    M = Matrix(rand(N,N))

    @test broadcast!(sin, copy(D), D) == Diagonal(sin.(D))
    @test broadcast!(sin, copy(Bu), Bu) == Bidiagonal(sin.(Bu), :U)
    @test broadcast!(sin, copy(Bl), Bl) == Bidiagonal(sin.(Bl), :L)
    @test broadcast!(sin, copy(T), T) == Tridiagonal(sin.(T))
    @test broadcast!(sin, copy(◣), ◣) == LowerTriangular(sin.(◣))
    @test broadcast!(sin, copy(◥), ◥) == UpperTriangular(sin.(◥))
    @test broadcast!(sin, copy(M), M) == Matrix(sin.(M))
    @test broadcast!(*, copy(D), D, A) == Diagonal(broadcast(*, D, A))
    @test broadcast!(*, copy(Bu), Bu, A) == Bidiagonal(broadcast(*, Bu, A), :U)
    @test broadcast!(*, copy(Bl), Bl, A) == Bidiagonal(broadcast(*, Bl, A), :L)
    @test broadcast!(*, copy(T), T, A) == Tridiagonal(broadcast(*, T, A))
    @test broadcast!(*, copy(◣), ◣, A) == LowerTriangular(broadcast(*, ◣, A))
    @test broadcast!(*, copy(◥), ◥, A) == UpperTriangular(broadcast(*, ◥, A))
    @test broadcast!(*, copy(M), M, A) == Matrix(broadcast(*, M, A))

    @test_throws ArgumentError broadcast!(cos, copy(D), D) == Diagonal(sin.(D))
    @test_throws ArgumentError broadcast!(cos, copy(Bu), Bu) == Bidiagonal(sin.(Bu), :U)
    @test_throws ArgumentError broadcast!(cos, copy(Bl), Bl) == Bidiagonal(sin.(Bl), :L)
    @test_throws ArgumentError broadcast!(cos, copy(T), T) == Tridiagonal(sin.(T))
    @test_throws ArgumentError broadcast!(cos, copy(◣), ◣) == LowerTriangular(sin.(◣))
    @test_throws ArgumentError broadcast!(cos, copy(◥), ◥) == UpperTriangular(sin.(◥))
    @test_throws ArgumentError broadcast!(+, copy(D), D, A) == Diagonal(broadcast(*, D, A))
    @test_throws ArgumentError broadcast!(+, copy(Bu), Bu, A) == Bidiagonal(broadcast(*, Bu, A), :U)
    @test_throws ArgumentError broadcast!(+, copy(Bl), Bl, A) == Bidiagonal(broadcast(*, Bl, A), :L)
    @test_throws ArgumentError broadcast!(+, copy(T), T, A) == Tridiagonal(broadcast(*, T, A))
    @test_throws ArgumentError broadcast!(+, copy(◣), ◣, A) == LowerTriangular(broadcast(*, ◣, A))
    @test_throws ArgumentError broadcast!(+, copy(◥), ◥, A) == UpperTriangular(broadcast(*, ◥, A))
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
    M = Matrix(rand(N,N))
    structuredarrays = (M, D, B, T, U, L)
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

@testset "Issue #33397" begin
    N = 5
    U = UpperTriangular(rand(N, N))
    L = LowerTriangular(rand(N, N))
    UnitU = UnitUpperTriangular(rand(N, N))
    UnitL = UnitLowerTriangular(rand(N, N))
    D = Diagonal(rand(N))
    @test U .+ L .+ D == U + L + D
    @test L .+ U .+ D == L + U + D
    @test UnitU .+ UnitL .+ D == UnitU + UnitL + D
    @test UnitL .+ UnitU .+ D == UnitL + UnitU + D
    @test U .+ UnitL .+ D == U + UnitL + D
    @test L .+ UnitU .+ D == L + UnitU + D
    @test L .+ U .+ L .+ U == L + U + L + U
    @test U .+ L .+ U .+ L == U + L + U + L
    @test L .+ UnitL .+ UnitU .+ U .+ D == L + UnitL + UnitU + U + D
    @test L .+ U .+ D .+ D .+ D .+ D == L + U + D + D + D + D
end
@testset "Broadcast Returned Types" begin
    # Issue 35245
    N = 3
    dV = rand(N)
    evu = rand(N-1)
    evl = rand(N-1)

    Bu = Bidiagonal(dV, evu, :U)
    Bl = Bidiagonal(dV, evl, :L)
    T = Tridiagonal(evl, dV * 2, evu)

    @test typeof(Bu .+ Bl) <: Tridiagonal
    @test typeof(Bl .+ Bu) <: Tridiagonal
    @test typeof(Bu .+ Bu) <: Bidiagonal
    @test typeof(Bl .+ Bl) <: Bidiagonal
    @test Bu .+ Bl == T
    @test Bl .+ Bu == T
    @test Bu .+ Bu == Bidiagonal(dV * 2, evu * 2, :U)
    @test Bl .+ Bl == Bidiagonal(dV * 2, evl * 2, :L)


    @test typeof(Bu .* Bl) <: Tridiagonal
    @test typeof(Bl .* Bu) <: Tridiagonal
    @test typeof(Bu .* Bu) <: Bidiagonal
    @test typeof(Bl .* Bl) <: Bidiagonal

    @test Bu .* Bl == Tridiagonal(zeros(N-1), dV .* dV, zeros(N-1))
    @test Bl .* Bu == Tridiagonal(zeros(N-1), dV .* dV, zeros(N-1))
    @test Bu .* Bu == Bidiagonal(dV .* dV, evu .* evu, :U)
    @test Bl .* Bl == Bidiagonal(dV .* dV, evl .* evl, :L)

    Bu2 =  Bu .* 2
    @test typeof(Bu2) <: Bidiagonal && Bu2.uplo == 'U'
    Bu2 = 2 .* Bu
    @test typeof(Bu2) <: Bidiagonal && Bu2.uplo == 'U'
    Bl2 =  Bl .* 2
    @test typeof(Bl2) <: Bidiagonal && Bl2.uplo == 'L'
    Bu2 = 2 .* Bl
    @test typeof(Bl2) <: Bidiagonal && Bl2.uplo == 'L'

    # Example of Nested Brodacasts
    tmp = (1 .* 2) .* (Bidiagonal(1:3, 1:2, 'U') .* (3 .* 4)) .* (5 .* Bidiagonal(1:3, 1:2, 'L'))
    @test typeof(tmp) <: Tridiagonal

end

struct Zero36193 end
Base.iszero(::Zero36193) = true
LinearAlgebra.iszerodefined(::Type{Zero36193}) = true
@testset "PR #36193" begin
    f(::Union{Int, Zero36193}) = Zero36193()
    function test(el)
        M = [el el
             el el]
        v = [el, el]
        U = UpperTriangular(M)
        L = LowerTriangular(M)
        D = Diagonal(v)
        for (T, A) in [(UpperTriangular, U), (LowerTriangular, L), (Diagonal, D)]
            @test identity.(A) isa typeof(A)
            @test map(identity, A) isa typeof(A)
            @test f.(A) isa T{Zero36193}
            @test map(f, A) isa T{Zero36193}
        end
    end
    # This should not need `zero(::Type{Zero36193})` to be defined
    test(1)
    Base.zero(::Type{Zero36193}) = Zero36193()
    # This should not need `==(::Zero36193, ::Int)` to be defined as `iszerodefined`
    # returns true.
    test(Zero36193())
end

# structured broadcast with function returning non-number type
@test tuple.(Diagonal([1, 2])) == [(1,) (0,); (0,) (2,)]

end
