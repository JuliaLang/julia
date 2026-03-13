# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests that the libdl embedding example runs without error
using Test

@test length(ARGS) == 1

@testset "libdl embedding" begin
    embedded_cmd_path = abspath(ARGS[1])
    p = cd(@__DIR__) do
        run(pipeline(Cmd([embedded_cmd_path]), stdin=devnull, stdout=devnull, stderr=devnull), wait=false)
    end
    @test success(p)
end
