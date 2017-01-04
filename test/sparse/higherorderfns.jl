# This file is a part of Julia. License is MIT: http://julialang.org/license
#
# These tests cover the higher order functions specialized for sparse arrays defined in
# base/sparse/higherorderfns.jl, particularly map[!]/broadcast[!] for SparseVectors and
# SparseMatrixCSCs at present.

@testset "map[!] implementation specialized for a single (input) sparse vector/matrix" begin
    N, M = 10, 12
    # (also the implementation for broadcast[!] over a single (input) sparse vector/matrix)
    for shapeA in ((N,), (N, M))
        A = sprand(shapeA..., 0.4); fA = Array(A)
        # --> test map entry point
        @test map(sin, A) == sparse(map(sin, fA))
        @test map(cos, A) == sparse(map(cos, fA))
        # --> test map! entry point
        fX = copy(fA); X = sparse(fX)
        map!(sin, X, A); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(sin, X, A)) == 0
        @test map!(sin, X, A) == sparse(map!(sin, fX, fA))
        @test map!(cos, X, A) == sparse(map!(cos, fX, fA))
        @test_throws DimensionMismatch map!(sin, X, spzeros((shapeA .- 1)...))
    end
end

@testset "map[!] implementation specialized for a pair of (input) sparse vectors/matrices" begin
    N, M = 10, 12
    f(x, y) = x + y + 1
    for shapeA in ((N,), (N, M))
        A, Bo = sprand(shapeA..., 0.3), sprand(shapeA..., 0.3)
        B = ndims(Bo) == 1 ? SparseVector{Float32, Int32}(Bo) : SparseMatrixCSC{Float32,Int32}(Bo)
        # use different types to check internal type stability via allocation tests below
        fA, fB = map(Array, (A, B))
        # --> test map entry point
        @test map(+, A, B) == sparse(map(+, fA, fB))
        @test map(*, A, B) == sparse(map(*, fA, fB))
        @test map(f, A, B) == sparse(map(f, fA, fB))
        @test_throws DimensionMismatch map(+, A, spzeros((shapeA .- 1)...))
        # --> test map! entry point
        fX = map(+, fA, fB); X = sparse(fX)
        map!(+, X, A, B); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(+, X, A, B)) == 0
        @test map!(+, X, A, B) == sparse(map!(+, fX, fA, fB))
        fX = map(*, fA, fB); X = sparse(fX)
        map!(*, X, A, B); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(*, X, A, B)) == 0
        @test map!(*, X, A, B) == sparse(map!(*, fX, fA, fB))
        @test map!(f, X, A, B) == sparse(map!(f, fX, fA, fB))
        @test_throws DimensionMismatch map!(f, X, A, spzeros((shapeA .- 1)...))
    end
end

@testset "map[!] implementation capable of handling >2 (input) sparse vectors/matrices" begin
    N, M = 10, 12
    f(x, y, z) = x + y + z + 1
    for shapeA in ((N,), (N, M))
        A, B, Co = sprand(shapeA..., 0.2), sprand(shapeA..., 0.2), sprand(shapeA..., 0.2)
        C = ndims(Co) == 1 ? SparseVector{Float32,Int32}(Co) : SparseMatrixCSC{Float32,Int32}(Co)
        # use different types to check internal type stability via allocation tests below
        fA, fB, fC = map(Array, (A, B, C))
        # --> test map entry point
        @test map(+, A, B, C) == sparse(map(+, fA, fB, fC))
        @test map(*, A, B, C) == sparse(map(*, fA, fB, fC))
        @test map(f, A, B, C) == sparse(map(f, fA, fB, fC))
        @test_throws DimensionMismatch map(+, A, B, spzeros(N, M - 1))
        # --> test map! entry point
        fX = map(+, fA, fB, fC); X = sparse(fX)
        map!(+, X, A, B, C); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(+, X, A, B, C)) == 0
        @test map!(+, X, A, B, C) == sparse(map!(+, fX, fA, fB, fC))
        fX = map(*, fA, fB, fC); X = sparse(fX)
        map!(*, X, A, B, C); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(*, X, A, B, C)) == 0
        @test map!(*, X, A, B, C) == sparse(map!(*, fX, fA, fB, fC))
        @test map!(f, X, A, B, C) == sparse(map!(f, fX, fA, fB, fC))
        @test_throws DimensionMismatch map!(f, X, A, B, spzeros((shapeA .- 1)...))
    end
end

@testset "broadcast[!] implementation specialized for a single (input) sparse vector/matrix" begin
    # broadcast[!] for a single sparse vector/matrix falls back to map[!], tested extensively
    # above. here we simply lightly exercise the relevant broadcast[!] entry points.
    N, M, p = 10, 12, 0.4
    a, A = sprand(N, p), sprand(N, M, p)
    fa, fA = Array(a), Array(A)
    @test broadcast(sin, a) == sparse(broadcast(sin, fa))
    @test broadcast(sin, A) == sparse(broadcast(sin, fA))
    @test broadcast!(sin, copy(a), a) == sparse(broadcast!(sin, copy(fa), fa))
    @test broadcast!(sin, copy(A), A) == sparse(broadcast!(sin, copy(fA), fA))
end

@testset "broadcast[!] implementation specialized for pairs of (input) sparse vectors/matrices" begin
    N, M, p = 10, 12, 0.3
    f(x, y) = x + y + 1
    mats = (sprand(N, M, p), sprand(N, 1, p), sprand(1, M, p), sprand(1, 1, 1.0), spzeros(1, 1))
    vecs = (sprand(N, p), sprand(1, 1.0), spzeros(1))
    tens = (mats..., vecs...)
    for Xo in tens
        X = ndims(Xo) == 1 ? SparseVector{Float32,Int32}(Xo) : SparseMatrixCSC{Float32,Int32}(Xo)
        # use different types to check internal type stability via allocation tests below
        shapeX, fX = size(X), Array(X)
        for Y in tens
            fY = Array(Y)
            # --> test broadcast entry point
            @test broadcast(+, X, Y) == sparse(broadcast(+, fX, fY))
            @test broadcast(*, X, Y) == sparse(broadcast(*, fX, fY))
            @test broadcast(f, X, Y) == sparse(broadcast(f, fX, fY))
            # TODO strengthen this test, avoiding dependence on checking whether
            # broadcast_indices throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.broadcast_indices(spzeros((shapeX .- 1)...), Y)
            catch
                @test_throws DimensionMismatch broadcast(+, spzeros((shapeX .- 1)...), Y)
            end
            # --> test broadcast! entry point / +-like zero-preserving op
            fZ = broadcast(+, fX, fY); Z = sparse(fZ)
            broadcast!(+, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(+, Z, X, Y)) == 0
            @test broadcast!(+, Z, X, Y) == sparse(broadcast!(+, fZ, fX, fY))
            # --> test broadcast! entry point / *-like zero-preserving op
            fZ = broadcast(*, fX, fY); Z = sparse(fZ)
            broadcast!(*, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(*, Z, X, Y)) == 0
            @test broadcast!(*, Z, X, Y) == sparse(broadcast!(*, fZ, fX, fY))
            # --> test broadcast! entry point / not zero-preserving op
            fZ = broadcast(f, fX, fY); Z = sparse(fZ)
            broadcast!(f, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(f, Z, X, Y)) == 0
            @test broadcast!(f, Z, X, Y) == sparse(broadcast!(f, fZ, fX, fY))
            # --> test shape checks for both broadcast and broadcast! entry points
            # TODO strengthen this test, avoiding dependence on checking whether
            # broadcast_indices throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.check_broadcast_indices(indices(Z), spzeros((shapeX .- 1)...), Y)
            catch
                @test_throws DimensionMismatch broadcast!(f, Z, spzeros((shapeX .- 1)...), Y)
            end
        end
    end
end

@testset "broadcast[!] implementation capable of handling >2 (input) sparse vectors/matrices" begin
    N, M, p = 10, 12, 0.3
    f(x, y, z) = x + y + z + 1
    mats = (sprand(N, M, p), sprand(N, 1, p), sprand(1, M, p), sprand(1, 1, 1.0), spzeros(1, 1))
    vecs = (sprand(N, p), sprand(1, 1.0), spzeros(1))
    tens = (mats..., vecs...)
    for Xo in tens
        X = ndims(Xo) == 1 ? SparseVector{Float32,Int32}(Xo) : SparseMatrixCSC{Float32,Int32}(Xo)
        # use different types to check internal type stability via allocation tests below
        shapeX, fX = size(X), Array(X)
        for Y in tens, Z in tens
            fY, fZ = Array(Y), Array(Z)
            # --> test broadcast entry point
            @test broadcast(+, X, Y, Z) == sparse(broadcast(+, fX, fY, fZ))
            @test broadcast(*, X, Y, Z) == sparse(broadcast(*, fX, fY, fZ))
            @test broadcast(f, X, Y, Z) == sparse(broadcast(f, fX, fY, fZ))
            # TODO strengthen this test, avoiding dependence on checking whether
            # broadcast_indices throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.broadcast_indices(spzeros((shapeX .- 1)...), Y, Z)
            catch
                @test_throws DimensionMismatch broadcast(+, spzeros((shapeX .- 1)...), Y, Z)
            end
            # --> test broadcast! entry point / +-like zero-preserving op
            fQ = broadcast(+, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(+, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test (@allocated broadcast!(+, Q, X, Y, Z)) == 0
            @test broadcast!(+, Q, X, Y, Z) == sparse(broadcast!(+, fQ, fX, fY, fZ))
            # --> test broadcast! entry point / *-like zero-preserving op
            fQ = broadcast(*, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(*, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test (@allocated broadcast!(*, Q, X, Y, Z)) == 0
            @test broadcast!(*, Q, X, Y, Z) == sparse(broadcast!(*, fQ, fX, fY, fZ))
            # --> test broadcast! entry point / not zero-preserving op
            fQ = broadcast(f, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(f, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test_broken (@allocated broadcast!(f, Q, X, Y, Z)) == 0
            # the preceding test allocates 16 bytes in the entry point for broadcast!, but
            # none of the earlier tests of the same code path allocate. no allocation shows
            # up with --track-allocation=user. allocation shows up on the first line of the
            # entry point for broadcast! with --track-allocation=all, but that first line
            # almost certainly should not allocate. so not certain what's going on.
            @test broadcast!(f, Q, X, Y, Z) == sparse(broadcast!(f, fQ, fX, fY, fZ))
            # --> test shape checks for both broadcast and broadcast! entry points
            # TODO strengthen this test, avoiding dependence on checking whether
            # broadcast_indices throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.check_broadcast_indices(indices(Q), spzeros((shapeX .- 1)...), Y, Z)
            catch
                @test_throws DimensionMismatch broadcast!(f, Q, spzeros((shapeX .- 1)...), Y, Z)
            end
        end
    end
end

@testset "sparse map/broadcast with result eltype not a concrete subtype of Number (#19561/#19589)" begin
    intoneorfloatzero(x) = x != 0.0 ? Int(1) : Float64(x)
    stringorfloatzero(x) = x != 0.0 ? "Hello" : Float64(x)
    @test map(intoneorfloatzero, speye(4)) == sparse(map(intoneorfloatzero, eye(4)))
    @test map(stringorfloatzero, speye(4)) == sparse(map(stringorfloatzero, eye(4)))
    @test broadcast(intoneorfloatzero, speye(4)) == sparse(broadcast(intoneorfloatzero, eye(4)))
    @test broadcast(stringorfloatzero, speye(4)) == sparse(broadcast(stringorfloatzero, eye(4)))
end

# Older tests of sparse broadcast, now largely covered by the tests above
@testset "assorted tests of sparse broadcast over two input arguments" begin
    N, p = 10, 0.3
    A, B, CF = sprand(N, N, p), sprand(N, N, p), rand(N, N)
    AF, BF, C = Array(A), Array(B), sparse(CF)

    @test A .* B == AF .* BF
    @test A[1,:] .* B == AF[1,:] .* BF
    @test A[:,1] .* B == AF[:,1] .* BF
    @test A .* B[1,:] == AF .*  BF[1,:]
    @test A .* B[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test A[1,:] .* BF == AF[1,:] .* BF
    @test A[:,1] .* BF == AF[:,1] .* BF
    @test A .* BF[1,:] == AF .*  BF[1,:]
    @test A .* BF[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test AF[1,:] .* B == AF[1,:] .* BF
    @test AF[:,1] .* B == AF[:,1] .* BF
    @test AF .* B[1,:] == AF .*  BF[1,:]
    @test AF .* B[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test A[1,:] .* B == AF[1,:] .* BF
    @test A[:,1] .* B == AF[:,1] .* BF
    @test A .* B[1,:] == AF .*  BF[1,:]
    @test A .* B[:,1] == AF .*  BF[:,1]

    @test A .* 3 == AF .* 3
    @test 3 .* A == 3 .* AF
    @test A[1,:] .* 3 == AF[1,:] .* 3
    @test A[:,1] .* 3 == AF[:,1] .* 3

    @test A .- 3 == AF .- 3
    @test 3 .- A == 3 .- AF
    @test A .- B == AF .- BF
    @test A - AF == zeros(AF)
    @test AF - A == zeros(AF)
    @test A[1,:] .- B == AF[1,:] .- BF
    @test A[:,1] .- B == AF[:,1] .- BF
    @test A .- B[1,:] == AF .-  BF[1,:]
    @test A .- B[:,1] == AF .-  BF[:,1]

    @test A .+ 3 == AF .+ 3
    @test 3 .+ A == 3 .+ AF
    @test A .+ B == AF .+ BF
    @test A + AF == AF + A
    @test (A .< B) == (AF .< BF)
    @test (A .!= B) == (AF .!= BF)

    @test A ./ 3 == AF ./ 3
    @test A .\ 3 == AF .\ 3
    @test 3 ./ A == 3 ./ AF
    @test 3 .\ A == 3 .\ AF
    @test A .\ C == AF .\ CF
    @test A ./ C == AF ./ CF
    @test A ./ CF[:,1] == AF ./ CF[:,1]
    @test A .\ CF[:,1] == AF .\ CF[:,1]
    @test BF ./ C == BF ./ CF
    @test BF .\ C == BF .\ CF

    @test A .^ 3 == AF .^ 3
    @test 3 .^ A == 3 .^ AF
    @test A .^ BF[:,1] == AF .^ BF[:,1]
    @test BF[:,1] .^ A == BF[:,1] .^ AF

    @test spzeros(0,0)  + spzeros(0,0) == zeros(0,0)
    @test spzeros(0,0)  * spzeros(0,0) == zeros(0,0)
    @test spzeros(1,0) .+ spzeros(2,1) == zeros(2,0)
    @test spzeros(1,0) .* spzeros(2,1) == zeros(2,0)
    @test spzeros(1,2) .+ spzeros(0,1) == zeros(0,2)
    @test spzeros(1,2) .* spzeros(0,1) == zeros(0,2)
end
