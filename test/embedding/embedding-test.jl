# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests the output of the embedding example is correct
using Test

if Sys.iswindows()
    # libjulia needs to be in the same directory as the embedding executable or in path
    ENV["PATH"] = string(Sys.BINDIR, ";", ENV["PATH"])
end

@test length(ARGS) == 1

@testset "embedding example" begin
    out = Pipe()
    err = Pipe()
    embedded_cmd_path = abspath(ARGS[1])
    p = cd(@__DIR__) do
        run(pipeline(Cmd([embedded_cmd_path]), stdin=devnull, stdout=out, stderr=err), wait=false)
    end
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    @test readline(err) == "MethodError: no method matching this_function_has_no_methods()"
    @test success(p)
    lines = fetch(out_task)
    @test length(lines) == 11
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    @test lines[2] == "sqrt(2.0) in C: 1.414214e+00"
    @test lines[3] == "sqrt(2.0) in C: 1.414214e+00"
    @test lines[4] == "sqrt(2.0) in C: 1.414214e+00"
    @test lines[9] == "called bar"
    @test lines[10] == "calling new bar"
    @test lines[11] == "      From worker 2:\tTaking over the world..."
    @test readline(err) == "exception caught from C"
end
