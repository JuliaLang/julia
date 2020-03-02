# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLDLT

using Test, LinearAlgebra, Random

Random.seed!(123)

@testset "REPL printing of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    ldltstring = sprint((t, s) -> show(t, "text/plain", s), F)
    lstring = sprint((t, s) -> show(t, "text/plain", s), F.L)
    dstring = sprint((t, s) -> show(t, "text/plain", s), F.D)
    @test ldltstring == "$(summary(F))\nL factor:\n$lstring\nD factor:\n$dstring"
end

@testset "Factorization of LDLT" begin
    S = SymTridiagonal([3., 4., 5.], [1., 2.])
    F = ldlt(S)
    @test Factorization{eltype(F)}(F) == F
end
end # module TestLDLT
