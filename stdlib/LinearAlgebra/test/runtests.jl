# This file is a part of Julia. License is MIT: https://julialang.org/license
using Test, LinearAlgebra

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end

@testset "Docstrings" begin
    undoc = Docs.undocumented_names(LinearAlgebra)
    @test_broken isempty(undoc)
    @test undoc == [:ColumnNorm, :LAPACKException, :NoPivot, :RankDeficientException, :RowMaximum, :RowNonZero, :copy_transpose!]
end
