# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLDLT

using Test, LinearAlgebra, Random

Random.seed!(123)

@testset "REPL printing of LDLT" begin
    S = SymTridiagonal(randn(5), randn(4))
    F = ldlt(S)
    ldltstring = sprint((t, s) -> show(t, "text/plain", s), F)
    lstring = sprint((t, s) -> show(t, "text/plain", s), UnitLowerTriangular(F.data))
    dstring = sprint((t, s) -> show(t, "text/plain", s), F.data.dv)
    @test ldltstring == "$(summary(F))\nL factor:\n$lstring\ndiagonal values:\n$dstring"
end

end # module TestLDLT
