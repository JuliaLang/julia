# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests the output of the embedding example is correct
using Test

if Sys.iswindows()
    # libjulia needs to be in the same directory as the embedding executable or in path
    ENV["PATH"] = string(JULIA_HOME, ";", ENV["PATH"])
end

@test length(ARGS) == 1
@testset "embedding example" begin
    stdout = Pipe()
    stderr = Pipe()
    p = spawn(pipeline(Cmd(ARGS), stdin=DevNull, stdout=stdout, stderr=stderr))
    close(stdout.in)
    close(stderr.in)
    stdout_task = @async readlines(stdout)
    stderr = read(stderr, String)
    @test stderr == "MethodError: no method matching this_function_has_no_methods()\n"
    @test success(p)
    lines = wait(stdout_task)
    @test length(lines) == 10
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    @test lines[8] == "called bar"
    @test lines[9] == "calling new bar"
    @test lines[10] == "\tFrom worker 2:\tTaking over the world..."
end
