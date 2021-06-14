# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestFactorization
using Test, LinearAlgebra

@testset "equality for factorizations - $f" for f in Any[
    bunchkaufman,
    cholesky,
    x -> cholesky(x, Val(true)),
    eigen,
    hessenberg,
    lq,
    lu,
    qr,
    x -> qr(x, ColumnNorm()),
    svd,
    schur,
]
    A = randn(3, 3)
    A = A * A' # ensure A is pos. def. and symmetric
    F, G = f(A), f(A)

    @test F == G
    @test isequal(F, G)
    @test hash(F) == hash(G)

    f === hessenberg && continue

    F = typeof(F).name.wrapper(Base.mapany(1:nfields(F)) do i
        x = getfield(F, i)
        return x isa AbstractArray{Float64} ? Float32.(x) : x
    end...)
    G = typeof(G).name.wrapper(Base.mapany(1:nfields(G)) do i
        x = getfield(G, i)
        return x isa AbstractArray{Float64} ? Float64.(Float32.(x)) : x
    end...)

    @test F == G
    @test isequal(F, G)
    @test hash(F) == hash(G)
end

@testset "hash collisions" begin
    A, v = randn(2, 2), randn(2)
    F, G = LQ(A, v), QR(A, v)
    @test !isequal(F, G)
    @test hash(F) != hash(G)
end

end
