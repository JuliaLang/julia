# This file is a part of Julia. License is MIT: https://julialang.org/license

# These tests cover the higher order functions specialized for sparse arrays defined in
# base/sparse/higherorderfns.jl, particularly map[!]/broadcast[!] for SparseVectors and
# SparseMatrixCSCs at present.

module HigherOrderFnsTests

using Test
using SparseArrays
using LinearAlgebra
using Random
include("forbidproperties.jl")

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
        @test (@allocated map!(+, X, A, B)) < 300
        @test map!(+, X, A, B) == sparse(map!(+, fX, fA, fB))
        fX = map(*, fA, fB); X = sparse(fX)
        map!(*, X, A, B); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(*, X, A, B)) < 300
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
        @test (@allocated map!(+, X, A, B, C)) < 300
        @test map!(+, X, A, B, C) == sparse(map!(+, fX, fA, fB, fC))
        fX = map(*, fA, fB, fC); X = sparse(fX)
        map!(*, X, A, B, C); X = sparse(fX) # warmup for @allocated
        @test (@allocated map!(*, X, A, B, C)) < 300
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
    # also test the typed broadcast
    @test broadcast(convert, Float32, A) == sparse(broadcast(convert, Float32, fA))
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
        @test (@allocated broadcast!(sin, Z, X)) < 300
        @test broadcast!(sin, Z, X) == sparse(broadcast!(sin, fZ, fX))
        # --> test broadcast! entry point / not-zero-preserving op
        broadcast!(cos, fZ, fX); Z = sparse(fZ)
        broadcast!(cos, Z, X); Z = sparse(fZ) # warmup for @allocated
        @test (@allocated broadcast!(cos, Z, X)) < 300
        @test broadcast!(cos, Z, X) == sparse(broadcast!(cos, fZ, fX))
        # --> test shape checks for broadcast! entry point
        # TODO strengthen this test, avoiding dependence on checking whether
        # check_broadcast_axes throws to determine whether sparse broadcast should throw
        try
            Base.Broadcast.check_broadcast_axes(axes(Z), spzeros((shapeX .- 1)...))
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
        @test (@allocated broadcast!(sin, V, X)) < 300
        @test broadcast!(sin, V, X) == sparse(broadcast!(sin, fV, fX))
        # --> test broadcast! entry point / not-zero-preserving
        broadcast!(cos, fV, fX); V = sparse(fV)
        broadcast!(cos, V, X); V = sparse(fV) # warmup for @allocated
        @test (@allocated broadcast!(cos, V, X)) < 300
        @test broadcast!(cos, V, X) == sparse(broadcast!(cos, fV, fX))
        # --> test shape checks for broadcast! entry point
        # TODO strengthen this test, avoiding dependence on checking whether
        # check_broadcast_axes throws to determine whether sparse broadcast should throw
        try
            Base.Broadcast.check_broadcast_axes(axes(V), spzeros((shapeX .- 1)...))
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
    fZ = Array(first(mats))
    for Xo in tens
        X = ndims(Xo) == 1 ? SparseVector{Float32,Int32}(Xo) : SparseMatrixCSC{Float32,Int32}(Xo)
        # use different types to check internal type stability via allocation tests below
        shapeX, fX = size(X), Array(X)
        for Y in tens
            fY = Array(Y)
            # --> test broadcast entry point
            @test broadcast(+, X, Y) == sparse(broadcast(+, fX, fY))
            @test broadcast(-, X, Y) == sparse(broadcast(-, fX, fY))
            @test broadcast(*, X, Y) == sparse(broadcast(*, fX, fY))
            @test broadcast(f, X, Y) == sparse(broadcast(f, fX, fY))
            # TODO strengthen this test, avoiding dependence on checking whether
            # check_broadcast_axes throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.combine_axes(spzeros((shapeX .- 1)...), Y)
            catch
                @test_throws DimensionMismatch broadcast(+, spzeros((shapeX .- 1)...), Y)
            end
            # --> test broadcast! entry point / +-like zero-preserving op
            broadcast!(+, fZ, fX, fY); Z = sparse(fZ)
            broadcast!(+, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(+, Z, X, Y)) < 300
            @test broadcast!(+, Z, X, Y) == sparse(broadcast!(+, fZ, fX, fY))
            # --> test broadcast! entry point / *-like zero-preserving op
            broadcast!(*, fZ, fX, fY); Z = sparse(fZ)
            broadcast!(*, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(*, Z, X, Y)) < 300
            @test broadcast!(*, Z, X, Y) == sparse(broadcast!(*, fZ, fX, fY))
            # --> test broadcast! entry point / not zero-preserving op
            broadcast!(f, fZ, fX, fY); Z = sparse(fZ)
            broadcast!(f, Z, X, Y); Z = sparse(fZ) # warmup for @allocated
            @test (@allocated broadcast!(f, Z, X, Y)) < 300
            @test broadcast!(f, Z, X, Y) == sparse(broadcast!(f, fZ, fX, fY))
            # --> test shape checks for both broadcast and broadcast! entry points
            # TODO strengthen this test, avoiding dependence on checking whether
            # check_broadcast_axes throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.check_broadcast_axes(axes(Z), spzeros((shapeX .- 1)...), Y)
            catch
                @test_throws DimensionMismatch broadcast!(f, Z, spzeros((shapeX .- 1)...), Y)
            end
        end
    end

    # fix#23857
    @test sparse([1; 0]) ./ [1] == sparse([1.0; 0.0])
    @test isequal(sparse([1 2; 1 0]) ./ [1; 0], sparse([1.0 2; Inf NaN]))
    @test sparse([1  0]) ./ [1] == sparse([1.0 0.0])
    @test isequal(sparse([1 2; 1 0]) ./ [1 0], sparse([1.0 Inf; 1 NaN]))

    @test sparse([1]) .\ sparse([1; 0]) == sparse([1.0; 0.0])
    @test isequal(sparse([1; 0]) .\ sparse([1 2; 1 0]), sparse([1.0 2; Inf NaN]))
    @test sparse([1]) .\ sparse([1  0]) == sparse([1.0 0.0])
    @test isequal(sparse([1 0]) .\ sparse([1 2; 1 0]), sparse([1.0 Inf; 1 NaN]))

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
            # check_broadcast_axes throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.combine_axes(spzeros((shapeX .- 1)...), Y, Z)
            catch
                @test_throws DimensionMismatch broadcast(+, spzeros((shapeX .- 1)...), Y, Z)
            end
            # --> test broadcast! entry point / +-like zero-preserving op
            fQ = broadcast(+, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(+, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test (@allocated broadcast!(+, Q, X, Y, Z)) < 300
            @test broadcast!(+, Q, X, Y, Z) == sparse(broadcast!(+, fQ, fX, fY, fZ))
            # --> test broadcast! entry point / *-like zero-preserving op
            fQ = broadcast(*, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(*, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test (@allocated broadcast!(*, Q, X, Y, Z)) < 300
            @test broadcast!(*, Q, X, Y, Z) == sparse(broadcast!(*, fQ, fX, fY, fZ))
            # --> test broadcast! entry point / not zero-preserving op
            fQ = broadcast(f, fX, fY, fZ); Q = sparse(fQ)
            broadcast!(f, Q, X, Y, Z); Q = sparse(fQ) # warmup for @allocated
            @test (@allocated broadcast!(f, Q, X, Y, Z)) < 300
            @test broadcast!(f, Q, X, Y, Z) == sparse(broadcast!(f, fQ, fX, fY, fZ))
            # --> test shape checks for both broadcast and broadcast! entry points
            # TODO strengthen this test, avoiding dependence on checking whether
            # check_broadcast_axes throws to determine whether sparse broadcast should throw
            try
                Base.Broadcast.check_broadcast_axes(axes(Q), spzeros((shapeX .- 1)...), Y, Z)
            catch
                @test_throws DimensionMismatch broadcast!(f, Q, spzeros((shapeX .- 1)...), Y, Z)
            end
        end
    end
end

@testset "sparse map/broadcast with result eltype not a concrete subtype of Number (#19561/#19589)" begin
    N = 4
    A, fA = sparse(1.0I, N, N), Matrix(1.0I, N, N)
    B, fB = spzeros(1, N), zeros(1, N)
    intorfloat_zeropres(xs...) = all(iszero, xs) ? zero(Float64) : Int(1)
    intorfloat_notzeropres(xs...) = all(iszero, xs) ? Int(1) : zero(Float64)
    for fn in (intorfloat_zeropres, intorfloat_notzeropres)
        @test map(fn, A) == sparse(map(fn, fA))
        @test broadcast(fn, A) == sparse(broadcast(fn, fA))
        @test broadcast(fn, A, B) == sparse(broadcast(fn, fA, fB))
        @test broadcast(fn, B, A) == sparse(broadcast(fn, fB, fA))
    end
    for fn in (intorfloat_zeropres,)
        @test broadcast(fn, A, B, A) == sparse(broadcast(fn, fA, fB, fA))
    end
end

@testset "broadcast[!] over combinations of scalars and sparse vectors/matrices" begin
    N, M, p = 10, 12, 0.5
    elT = Float64
    s = Float32(2.0)
    V = sprand(elT, N, p)
    Vᵀ = transpose(sprand(elT, 1, N, p))
    A = sprand(elT, N, M, p)
    Aᵀ = transpose(sprand(elT, M, N, p))
    fV, fA, fVᵀ, fAᵀ = Array(V), Array(A), Array(Vᵀ), Array(Aᵀ)
    # test combinations involving one to three scalars and one to five sparse vectors/matrices
    spargseq, dargseq = Iterators.cycle((A, V, Aᵀ, Vᵀ)), Iterators.cycle((fA, fV, fAᵀ, fVᵀ))
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
            # And broadcasting over Transposes currently requires making a CSC copy, so we must account for that in the bounds
            @test (@allocated broadcast!(*, X, sparseargs...)) <= (sum(x->isa(x, Transpose) ? @allocated(SparseMatrixCSC(x)) + 128 : 0, sparseargs) + 128 + 900) # about zero to 3k bytes
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
        @test (@allocated broadcast!(*, X, sparseargs...)) <= 900
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
    B = Bidiagonal(rand(N), rand(N - 1), :U)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    S = SymTridiagonal(rand(N), rand(N - 1))
    structuredarrays = (D, B, T, S)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test (Q = broadcast(+, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(+, fV, fA, fX)))
        @test broadcast!(+, Z, V, A, X) == sparse(broadcast(+, fV, fA, fX))
        @test (Q = broadcast(*, s, V, A, X); Q isa SparseMatrixCSC && Q == sparse(broadcast(*, s, fV, fA, fX)))
        @test broadcast!(*, Z, s, V, A, X) == sparse(broadcast(*, s, fV, fA, fX))
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test broadcast!(+, Z, X, Y) == sparse(broadcast(+, fX, fY))
            @test broadcast!(*, Z, X, Y) == sparse(broadcast(*, fX, fY))
        end
    end
    C = Array(sprand(N, 0.4))
    M = Array(sprand(N, N, 0.4))
    densearrays = (C, M)
    fD, fB = Array(D), Array(B)
    for X in densearrays
        @test broadcast!(+, Z, D, X) == sparse(broadcast(+, fD, X))
        @test broadcast!(*, Z, s, B, X) == sparse(broadcast(*, s, fB, X))
        @test broadcast(+, V, B, X)::SparseMatrixCSC == sparse(broadcast(+, fV, fB, X))
        @test broadcast!(+, Z, V, B, X) == sparse(broadcast(+, fV, fB, X))
        @test broadcast(+, V, A, X)::SparseMatrixCSC == sparse(broadcast(+, fV, fA, X))
        @test broadcast!(+, Z, V, A, X) == sparse(broadcast(+, fV, fA, X))
        @test broadcast(*, s, V, A, X)::SparseMatrixCSC == sparse(broadcast(*, s, fV, fA, X))
        @test broadcast!(*, Z, s, V, A, X) == sparse(broadcast(*, s, fV, fA, X))
        # Issue #20954 combinations of sparse arrays and Adjoint/Transpose vectors
        if X isa Vector
            @test broadcast(+, A, X')::SparseMatrixCSC == sparse(broadcast(+, fA, X'))
            @test broadcast(*, V, X')::SparseMatrixCSC == sparse(broadcast(*, fV, X'))
        end
    end
    @test V .+ ntuple(identity, N) isa Vector
    @test A .+ ntuple(identity, N) isa Matrix
end

@testset "map[!] over combinations of sparse and structured matrices" begin
    N, p = 10, 0.4
    A = sprand(N, N, p)
    Z, fA = copy(A), Array(A)
    D = Diagonal(rand(N))
    B = Bidiagonal(rand(N), rand(N - 1), :U)
    T = Tridiagonal(rand(N - 1), rand(N), rand(N - 1))
    S = SymTridiagonal(rand(N), rand(N - 1))
    structuredarrays = (D, B, T, S)
    fstructuredarrays = map(Array, structuredarrays)
    for (X, fX) in zip(structuredarrays, fstructuredarrays)
        @test map!(sin, Z, X) == sparse(map(sin, fX))
        @test map!(cos, Z, X) == sparse(map(cos, fX))
        @test (Q = map(+, A, X); Q isa SparseMatrixCSC && Q == sparse(map(+, fA, fX)))
        @test map!(+, Z, A, X) == sparse(map(+, fA, fX))
        for (Y, fY) in zip(structuredarrays, fstructuredarrays)
            @test map!(+, Z, X, Y) == sparse(map(+, fX, fY))
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
    @test A - AF == zeros(size(AF))
    @test AF - A == zeros(size(AF))
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

@testset "sparse vector broadcast of two arguments" begin
    sv1, sv5 = sprand(1, 1.), sprand(5, 1.)
    for (sa, sb) in ((sv1, sv1), (sv1, sv5), (sv5, sv1), (sv5, sv5))
        fa, fb = Vector(sa), Vector(sb)
        for f in (+, -, *, min, max)
            @test @inferred(broadcast(f, sa, sb))::SparseVector == broadcast(f, fa, fb)
            @test @inferred(broadcast(f, Vector(sa), sb))::SparseVector == broadcast(f, fa, fb)
            @test @inferred(broadcast(f, sa, Vector(sb)))::SparseVector == broadcast(f, fa, fb)
            @test @inferred(broadcast(f, SparseMatrixCSC(sa), sb))::SparseMatrixCSC == broadcast(f, reshape(fa, Val(2)), fb)
            @test @inferred(broadcast(f, sa, SparseMatrixCSC(sb)))::SparseMatrixCSC == broadcast(f, fa, reshape(fb, Val(2)))
            if length(fa) == length(fb)
                @test @inferred(map(f, sa, sb))::SparseVector == broadcast(f, fa, fb)
            end
        end
        if length(fa) == length(fb)
            for f in (+, -)
                @test @inferred(f(sa, sb))::SparseVector == f(fa, fb)
                @test @inferred(f(Vector(sa), sb))::SparseVector == f(fa, fb)
                @test @inferred(f(sa, Vector(sb)))::SparseVector == f(fa, fb)
            end
        end
    end
end

@testset "aliasing and indexed assignment or broadcast!" begin
    A = sparsevec([0, 0, 1, 1])
    B = sparsevec([1, 1, 0, 0])
    A .+= B
    @test A == sparse([1,1,1,1])

    A = sprandn(10, 10, 0.1)
    fA = Array(A)
    b = randn(10);
    broadcast!(/, A, A, b)
    @test A == fA ./ Array(b)

    a = sparse([1,3,5])
    b = sparse([3,1,2])
    a[b] = a
    @test a == [3,5,1]
    a = sparse([3,2,1])
    a[a] = [4,5,6]
    @test a == [6,5,4]

    A = sparse([1,2,3,4])
    V = view(A, A)
    @test V == A
    V[1] = 2
    @test V == A == [2,2,3,4]
    V[1] = 2^30
    @test V == A == [2^30, 2, 3, 4]

    A = sparse([2,1,4,3])
    V = view(A, :)
    A[V] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = sparse([2,1,4,3])
    R = reshape(view(A, :), 2, 2)
    A[R] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = sparse([2,1,4,3])
    R = reshape(A, 2, 2)
    A[R] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    # And broadcasting
    a = sparse([1,3,5])
    b = sparse([3,1,2])
    a[b] .= a
    @test a == [3,5,1]
    a = sparse([3,2,1])
    a[a] .= [4,5,6]
    @test a == [6,5,4]

    A = sparse([2,1,4,3])
    V = view(A, :)
    A[V] .= (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = sparse([2,1,4,3])
    R = reshape(view(A, :), 2, 2)
    A[R] .= reshape((1:4) .+ 2^30, 2, 2)
    @test A == [2,1,4,3] .+ 2^30

    A = sparse([2,1,4,3])
    R = reshape(A, 2, 2)
    A[R] .= reshape((1:4) .+ 2^30, 2, 2)
    @test A == [2,1,4,3] .+ 2^30
end

@testset "1-dimensional 'opt-out' (non) sparse broadcasting" begin
    # SparseArrays intentionally only promotes to sparse for limited array types
    # More support may be added in the future, but for now let's make sure that
    # broadcast still performs as expected (issue #26977)
    A = spzeros(5)
    @test A .+ (1:5) == 1:5
    @test A .* 2 .+ view(collect(1:10), 1:5) == 1:5
    @test 2 .* A .+ view(1:10, 1:5) == 1:5
    @test (A .+ (1:5)) .* 2 == 2:2:10
    @test ((1:5) .+ A) .* 2 == 2:2:10
    @test 2 .* ((1:5) .+ A) == 2:2:10
    @test 2 .* (A .+ (1:5)) == 2:2:10

    @test Diagonal(spzeros(5)) \ view(rand(10), 1:5) == [Inf,Inf,Inf,Inf,Inf]
end

@testset "Issue #27836" begin
    @test minimum(sparse([1, 2], [1, 2], ones(Int32, 2)), dims = 1) isa Matrix
end

@testset "Issue #30118" begin
    @test ((_, x) -> x).(Int, spzeros(3)) == spzeros(3)
    @test ((_, _, x) -> x).(Int, Int, spzeros(3)) == spzeros(3)
    @test ((_, _, _, x) -> x).(Int, Int, Int, spzeros(3)) == spzeros(3)
    @test_broken ((_, _, _, _, x) -> x).(Int, Int, Int, Int, spzeros(3)) == spzeros(3)
end

using SparseArrays.HigherOrderFns: SparseVecStyle, SparseMatStyle

@testset "Issue #30120: method ambiguity" begin
    # HigherOrderFns._copy(f) was ambiguous.  It may be impossible to
    # invoke this from dot notation and it is an error anyway.  But
    # when someone invokes it by accident, we want it to produce a
    # meaningful error.
    err = try
        copy(Broadcast.Broadcasted{SparseVecStyle}(rand, ()))
    catch err
        err
    end
    @test err isa MethodError
    @test !occursin("is ambiguous", sprint(showerror, err))
    @test occursin("no method matching _copy(::typeof(rand))", sprint(showerror, err))
end

@testset "Sparse outer product, for type $T and vector $op" for
         op in (transpose, adjoint),
         T in (Float64, ComplexF64)
    m, n, p = 100, 250, 0.1
    A = sprand(T, m, n, p)
    a, b = view(A, :, 1), sprand(T, m, p)
    av, bv = Vector(a), Vector(b)
    v = @inferred a .* op(b)
    w = @inferred b .* op(a)
    @test issparse(v)
    @test issparse(w)
    @test v == av .* op(bv)
    @test w == bv .* op(av)
end

@testset "issue #31758: out of bounds write in _map_zeropres!" begin
    y = sparsevec([2,7], [1., 2.], 10)
    x1 = sparsevec(fill(1.0, 10))
    x2 = sparsevec([2,7], [1., 2.], 10)
    x3 = sparsevec(fill(1.0, 10))
    f(x, y, z) = x == y == z == 0 ? 0.0 : NaN
    y .= f.(x1, x2, x3)
    @test all(isnan, y)
end

@testset "Vec/Mat Style" begin
    @test SparseVecStyle(Val(0)) == SparseVecStyle()
    @test SparseVecStyle(Val(1)) == SparseVecStyle()
    @test SparseVecStyle(Val(2)) == SparseMatStyle()
    @test SparseVecStyle(Val(3)) == Broadcast.DefaultArrayStyle{3}()
    @test SparseMatStyle(Val(0)) == SparseMatStyle()
    @test SparseMatStyle(Val(1)) == SparseMatStyle()
    @test SparseMatStyle(Val(2)) == SparseMatStyle()
    @test SparseMatStyle(Val(3)) == Broadcast.DefaultArrayStyle{3}()
end

end # module
