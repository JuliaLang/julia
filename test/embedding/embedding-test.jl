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
    p = run(pipeline(Cmd(ARGS), stdin=devnull, stdout=out, stderr=err), wait=false)
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err = read(err, String)
    @test err == "MethodError: no method matching this_function_has_no_methods()\n"
    @test success(p)
    lines = fetch(out_task)
    @test length(lines) == 10
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    @test lines[8] == "called bar"
    @test lines[9] == "calling new bar"
    @test lines[10] == "      From worker 2:\tTaking over the world..."
end
