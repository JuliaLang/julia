# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLDLT

using Test, LinearAlgebra, Random

Random.seed!(123)

@testset "Factorization conversions of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    @test Factorization{eltype(S)}(F) === F
    @test Array(Factorization{complex(eltype(S))}(F)) ≈ Array(ldlt(complex(S)))
    @test eltype(Factorization{complex(eltype(S))}) == complex(eltype(S))
end

@testset "eltype conversions of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    Fc = LDLt{ComplexF32}(F.data)
    @test Fc isa LDLt{ComplexF32}
    @test Array(Fc) ≈ ComplexF32.(Array(S))
end

@testset "Accessing fields of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    @test getproperty(F, :L) == transpose(getproperty(F, :Lt))
    @test getproperty(F, :d) == diag(getproperty(F, :D), 0)
end

@testset "REPL printing of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    ldltstring = sprint((t, s) -> show(t, "text/plain", s), F)
    lstring = sprint((t, s) -> show(t, "text/plain", s), F.L)
    dstring = sprint((t, s) -> show(t, "text/plain", s), F.D)
    @test ldltstring == "$(summary(F))\nL factor:\n$lstring\nD factor:\n$dstring"
end

end # module TestLDLT
