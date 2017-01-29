# This file is a part of Julia. License is MIT: http://julialang.org/license
#
# These tests cover the higher order functions specialized for sparse arrays defined in
# base/sparse/higherorderfns.jl, particularly map[!]/broadcast[!] for SparseVectors and
# SparseMatrixCSCs at present.

@testset "map[!] implementation specialized for a single (input) sparse vector/matrix" begin
    N, M = 10, 12
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

@testset "broadcast! implementation specialized for solely an output sparse vector/matrix (no inputs)" begin
    N, M, p = 10, 12, 0.4
    V, C = sprand(N, p), sprand(N, M, p)
    fV, fC = Array(V), Array(C)
    @test broadcast!(() -> 0, V) == sparse(broadcast!(() -> 0, fV))
    @test broadcast!(() -> 0, C) == sparse(broadcast!(() -> 0, fC))
    @test let z = 0, fz = 0; broadcast!(() -> z += 1, V) == broadcast!(() -> fz += 1, fV); end
    @test let z = 0, fz = 0; broadcast!(() -> z += 1, C) == broadcast!(() -> fz += 1, fC); end
end

@testset "broadcast implementation specialized for a single (input) sparse vector/matrix" begin
    # broadcast for a single (input) sparse vector/matrix falls back to map, tested
    # extensively above. here we simply lightly exercise the relevant broadcast entry
    # point.
    N, M, p = 10, 12, 0.4
    a, A = sprand(N, p), sprand(N, M, p)
    fa, fA = Array(a), Array(A)
    @test broadcast(sin, a) == sparse(broadcast(sin, fa))
    @test broadcast(sin, A) == sparse(broadcast(sin, fA))
end

@testset "broadcast! implementation specialized for a single (input) sparse vector/matrix" begin
    N, M, p = 10, 12, 0.3
    f(x, y) = x + y + 1
    mats = (sprand(N, M, p), sprand(N, 1, p), sprand(1, M, p), sprand(1, 1, 1.0), spzeros(1, 1))
    vecs = (sprand(N, p), sprand(1, 1.0), spzeros(1))
    # --> test with matrix destination (Z/fZ)
    fZ = Array(first(mats))
    for Xo in (mats..., vecs...)
        X = ndims(Xo) == 1 ? SparseVector{Float32,Int32}(Xo) : SparseMatrixCSC{Float32,Int32}(Xo)
        shapeX, fX = size(X), Array(X)
        # --> test broadcast! entry point / zero-preserving op
        broadcast!(sin, fZ, fX); Z = sparse(fZ)
        broadcast!(sin, Z, X); Z = sparse(fZ) # warmup for @allocated
        @test (@allocated broadcast!(sin, Z, X)) == 0
        @test broadcast!(sin, Z, X) == sparse(broadcast!(sin, fZ, fX))
        # --> test broadcast! entry point / not-zero-preserving op
        broadcast!(cos, fZ, fX); Z = sparse(fZ)
        broadcast!(cos, Z, X); Z = sparse(fZ) # warmup for @allocated
        @test (@allocated broadcast!(cos, Z, X)) == 0
        @test broadcast!(cos, Z, X) == sparse(broadcast!(cos, fZ, fX))
        # --> test shape checks for broadcast! entry point
        # TODO strengthen this test, avoiding dependence on checking whether
        # broadcast_indices throws to determine whether sparse broadcast should throw
        try
            Base.Broadcast.check_broadcast_indices(indices(Z), spzeros((shapeX .- 1)...))
        catch
            @test_throws DimensionMismatch broadcast!(sin, Z, spzeros((shapeX .- 1)...))
        end
    end
    # --> test with vector destination (V/fV)
    fV = Array(first(vecs))
    for Xo in vecs # vector target
        X = SparseVector{Float32,Int32}(Xo)
        shapeX, fX = size(X), Array(X)
        # --> test broadcast! entry point / zero-preserving op
        broadcast!(sin, fV, fX); V = sparse(fV)
        broadcast!(sin, V, X); V = sparse(fV) # warmup for @allocated
        @test (@allocated broadcast!(sin, V, X)) == 0
        @test broadcast!(sin, V, X) == sparse(broadcast!(sin, fV, fX))
        # --> test broadcast! entry point / not-zero-preserving
        broadcast!(cos, fV, fX); V = sparse(fV)
        broadcast!(cos, V, X); V = sparse(fV) # warmup for @allocated
        @test (@allocated broadcast!(cos, V, X)) == 0
        @test broadcast!(cos, V, X) == sparse(broadcast!(cos, fV, fX))
        # --> test shape checks for broadcast! entry point
        # TODO strengthen this test, avoiding dependence on checking whether
        # broadcast_indices throws to determine whether sparse broadcast should throw
        try
            Base.Broadcast.check_broadcast_indices(indices(V), spzeros((shapeX .- 1)...))
        catch
            @test_throws DimensionMismatch broadcast!(sin, V, spzeros((shapeX .- 1)...))
        end
    end
    # Tests specific to #19895, i.e. for broadcast!(identity, C, A) specializations
    Z = copy(first(mats)); fZ = Array(Z)
    V = copy(first(vecs)); fV = Array(V)
    for X in (mats..., vecs...)
        @test broadcast!(identity, Z, X) == sparse(broadcast!(identity, fZ, Array(X)))
        X isa SparseVector && @test broadcast!(identity, V, X) == sparse(broadcast!(identity, fV, Array(X)))
    end
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
            # additional info: occurs for broadcast!(f, Z, X) for Z and X of different
            # shape, but not for Z and X of the same shape.
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

@testset "broadcast[!] over combinations of scalars and sparse vectors/matrices" begin
    N, M, p = 10, 12, 0.5
    elT = Float64
    s = Float32(2.0)
    V = sprand(elT, N, p)
    A = sprand(elT, N, M, p)
    fV, fA = Array(V), Array(A)
    # test combinations involving one to three scalars and one to five sparse vectors/matrices
    spargseq, dargseq = Iterators.cycle((A, V)), Iterators.cycle((fA, fV))
    for nargs in 1:5 # number of tensor arguments
        nargsl = cld(nargs, 2) # number in "left half" of tensor arguments
        nargsr = fld(nargs, 2) # number in "right half" of tensor arguments
        spargsl = tuple(Iterators.take(spargseq, nargsl)...) # "left half" of tensor args
        spargsr = tuple(Iterators.take(spargseq, nargsr)...) # "right half" of tensor args
        dargsl = tuple(Iterators.take(dargseq, nargsl)...) # "left half" of tensor args, densified
        dargsr = tuple(Iterators.take(dargseq, nargsr)...) # "right half" of tensor args, densified
        for (sparseargs, denseargs) in ( # argument combinations including scalars
                # a few combinations involving one scalar
                ((s, spargsl..., spargsr...), (s, dargsl..., dargsr...)),
                ((spargsl..., s, spargsr...), (dargsl..., s, dargsr...)),
                ((spargsl..., spargsr..., s), (dargsl..., dargsr..., s)),
                # a few combinations involving two scalars
                ((s, spargsl..., s, spargsr...), (s, dargsl..., s, dargsr...)),
                ((s, spargsl..., spargsr..., s), (s, dargsl..., dargsr..., s)),
                ((spargsl..., s, spargsr..., s), (dargsl..., s, dargsr..., s)),
                ((s, s, spargsl..., spargsr...), (s, s, dargsl..., dargsr...)),
                ((spargsl..., s, s, spargsr...), (dargsl..., s, s, dargsr...)),
                ((spargsl..., spargsr..., s, s), (dargsl..., dargsr..., s, s)),
                # a few combinations involving three scalars
                ((s, spargsl..., s, spargsr..., s), (s, dargsl..., s, dargsr..., s)),
                ((s, spargsl..., s, s, spargsr...), (s, dargsl..., s, s, dargsr...)),
                ((spargsl..., s, s, spargsr..., s), (dargsl..., s, s, dargsr..., s)),
                ((spargsl..., s, s, s, spargsr...), (dargsl..., s, s, s, dargsr...)), )
            # test broadcast entry point
            @test broadcast(*, sparseargs...) == sparse(broadcast(*, denseargs...))
            @test isa(@inferred(broadcast(*, sparseargs...)), SparseMatrixCSC{elT})
            # test broadcast! entry point
            fX = broadcast(*, sparseargs...); X = sparse(fX)
            @test broadcast!(*, X, sparseargs...) == sparse(broadcast!(*, fX, denseargs...))
            @test isa(@inferred(broadcast!(*, X, sparseargs...)), SparseMatrixCSC{elT})
            X = sparse(fX) # reset / warmup for @allocated test
            @test_broken (@allocated broadcast!(*, X, sparseargs...)) == 0
            # This test (and the analog below) fails for three reasons:
            # (1) In all cases, generating the closures that capture the scalar arguments
            #   results in allocation, not sure why.
            # (2) In some cases, though _broadcast_eltype (which wraps _return_type)
            #   consistently provides the correct result eltype when passed the closure
            #   that incorporates the scalar arguments to broadcast (and, with #19667,
            #   is inferable, so the overall return type from broadcast is inferred),
            #   in some cases inference seems unable to determine the return type of
            #   direct calls to that closure. This issue causes variables in both the
            #   broadcast[!] entry points (fofzeros = f(_zeros_eltypes(args...)...)) and
            #   the driver routines (Cx in _map_zeropres! and _broadcast_zeropres!) to have
            #   inferred type Any, resulting in allocation and lackluster performance.
            # (3) The sparseargs... splat in the call above allocates a bit, but of course
            #   that issue is negligible and perhaps could be accounted for in the test.
        end
    end
    # test combinations at the limit of inference (eight arguments net)
    for (sparseargs, denseargs) in (
            ((s, s, s, A, s, s, s, s), (s, s, s, fA, s, s, s, s)), # seven scalars, one sparse matrix
            ((s, s, V, s, s, A, s, s), (s, s, fV, s, s, fA, s, s)), # six scalars, two sparse vectors/matrices
            ((s, s, V, s, A, s, V, s), (s, s, fV, s, fA, s, fV, s)), # five scalars, three sparse vectors/matrices
            ((s, V, s, A, s, V, s, A), (s, fV, s, fA, s, fV, s, fA)), # four scalars, four sparse vectors/matrices
            ((s, V, A, s, V, A, s, A), (s, fV, fA, s, fV, fA, s, fA)), # three scalars, five sparse vectors/matrices
            ((V, A, V, s, A, V, A, s), (fV, fA, fV, s, fA, fV, fA, s)), # two scalars, six sparse vectors/matrices
            ((V, A, V, A, s, V, A, V), (fV, fA, fV, fA, s, fV, fA, fV)) ) # one scalar, seven sparse vectors/matrices
        # test broadcast entry point
        @test broadcast(*, sparseargs...) == sparse(broadcast(*, denseargs...))
        @test isa(@inferred(broadcast(*, sparseargs...)), SparseMatrixCSC{elT})
        # test broadcast! entry point
        fX = broadcast(*, sparseargs...); X = sparse(fX)
        @test broadcast!(*, X, sparseargs...) == sparse(broadcast!(*, fX, denseargs...))
        @test isa(@inferred(broadcast!(*, X, sparseargs...)), SparseMatrixCSC{elT})
        X = sparse(fX) # reset / warmup for @allocated test
        @test_broken (@allocated broadcast!(*, X, sparseargs...)) == 0
        # please see the note a few lines above re. this @test_broken
    end
end

@testset "broadcast[!] over combinations of scalars, sparse arrays, structured matrices, and dense vectors/matrices" begin
    N, p = 10, 0.4
    s = rand()
    V = sprand(N, p)
    A = sprand(N, N, p)
    Z = copy(A)
    sparsearrays = (V, A)
    fV, fA = map(Array, sparsearrays)
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), true)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    S = SymTridiagonal(rand(N), rand(N - 1))
    structuredarrays = (D, B, T, S)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test (Q = broadcast(sin, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(sin, fX)))
        @test broadcast!(sin, Z, X) == sparse(broadcast(sin, fX))
        @test (Q = broadcast(cos, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(cos, fX)))
        @test broadcast!(cos, Z, X) == sparse(broadcast(cos, fX))
        @test (Q = broadcast(*, s, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, s, fX)))
        @test broadcast!(*, Z, s, X) == sparse(broadcast(*, s, fX))
        @test (Q = broadcast(+, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fV, fA, fX)))
        @test broadcast!(+, Z, V, A, X) == sparse(broadcast(+, fV, fA, fX))
        @test (Q = broadcast(*, s, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, s, fV, fA, fX)))
        @test broadcast!(*, Z, s, V, A, X) == sparse(broadcast(*, s, fV, fA, fX))
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test (Q = broadcast(+, X, Y); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fX, fY)))
            @test broadcast!(+, Z, X, Y) == sparse(broadcast(+, fX, fY))
            @test (Q = broadcast(*, X, Y); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, fX, fY)))
            @test broadcast!(*, Z, X, Y) == sparse(broadcast(*, fX, fY))
        end
    end
    C = Array(sprand(N, 0.4))
    M = Array(sprand(N, N, 0.4))
    densearrays = (C, M)
    fD, fB = Array(D), Array(B)
    for X in densearrays
        @test (Q = broadcast(+, D, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fD, X)))
        @test broadcast!(+, Z, D, X) == sparse(broadcast(+, fD, X))
        @test (Q = broadcast(*, s, B, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, s, fB, X)))
        @test broadcast!(*, Z, s, B, X) == sparse(broadcast(*, s, fB, X))
        @test (Q = broadcast(+, V, B, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fV, fB, X)))
        @test broadcast!(+, Z, V, B, X) == sparse(broadcast(+, fV, fB, X))
        @test (Q = broadcast(+, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fV, fA, X)))
        @test broadcast!(+, Z, V, A, X) == sparse(broadcast(+, fV, fA, X))
        @test (Q = broadcast(*, s, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, s, fV, fA, X)))
        @test broadcast!(*, Z, s, V, A, X) == sparse(broadcast(*, s, fV, fA, X))
    end
end

@testset "map[!] over combinations of sparse and structured matrices" begin
    N, p = 10, 0.4
    A = sprand(N, N, p)
    Z, fA = copy(A), Array(A)
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), true)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    S = SymTridiagonal(rand(N), rand(N - 1))
    structuredarrays = (D, B, T, S)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test (Q = map(sin, X); Q isa SparseMatrixCSC && Q == sparse(map(sin, fX)))
        @test map!(sin, Z, X) == sparse(map(sin, fX))
        @test (Q = map(cos, X); Q isa SparseMatrixCSC && Q == sparse(map(cos, fX)))
        @test map!(cos, Z, X) == sparse(map(cos, fX))
        @test (Q = map(+, A, X); Q isa SparseMatrixCSC && Q == sparse(map(+, fA, fX)))
        @test map!(+, Z, A, X) == sparse(map(+, fA, fX))
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test (Q = map(+, X, Y); Q isa SparseMatrixCSC && Q == sparse(map(+, fX, fY)))
            @test map!(+, Z, X, Y) == sparse(map(+, fX, fY))
            @test (Q = map(*, X, Y); Q isa SparseMatrixCSC && Q == sparse(map(*, fX, fY)))
            @test map!(*, Z, X, Y) == sparse(map(*, fX, fY))
            @test (Q = map(+, X, A, Y); Q isa SparseMatrixCSC && Q == sparse(map(+, fX, fA, fY)))
            @test map!(+, Z, X, A, Y) == sparse(map(+, fX, fA, fY))
        end
    end
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
