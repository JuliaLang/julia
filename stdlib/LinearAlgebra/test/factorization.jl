# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestFactorization
using Test, LinearAlgebra

@testset "equality for factorizations - $f" for f in Any[
    bunchkaufman,
    cholesky,
    x -> cholesky(x, RowMaximum()),
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

    # change all arrays in F to have eltype Float32
    F = typeof(F).name.wrapper(Base.mapany(1:nfields(F)) do i
        x = getfield(F, i)
        return x isa AbstractArray{Float64} ? Float32.(x) : x
    end...)
    # round all arrays in G to the nearest Float64 representable as Float32
    G = typeof(G).name.wrapper(Base.mapany(1:nfields(G)) do i
        x = getfield(G, i)
        return x isa AbstractArray{Float64} ? Float64.(Float32.(x)) : x
    end...)

    @test F == G broken=!(f === eigen || f === qr)
    @test isequal(F, G) broken=!(f === eigen || f === qr)
    @test hash(F) == hash(G)
end

@testset "size for factorizations - $f" for f in Any[
    bunchkaufman,
    cholesky,
    x -> cholesky(x, RowMaximum()),
    hessenberg,
    lq,
    lu,
    qr,
    x -> qr(x, ColumnNorm()),
    svd,
]
    A = randn(3, 3)
    A = A * A' # ensure A is pos. def. and symmetric
    F = f(A)
    tF = Transpose(F)
    aF = Adjoint(F)
    @test size(F) == size(A)
    @test size(tF) == size(Transpose(A))
    @test size(aF) == size(Adjoint(A))
end

@testset "equality of QRCompactWY" begin
    A = rand(100, 100)
    F, G = qr(A), qr(A)

    @test F == G
    @test isequal(F, G)
    @test hash(F) == hash(G)

    G.T[28, 100] = 42

    @test F != G
    @test !isequal(F, G)
    @test hash(F) != hash(G)
end

end
