# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests the output of the embedding example is correct
using Base.Test

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
    @test length(lines) == 9
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    @test lines[8] == "called bar"
    @test lines[9] == "calling new bar"
end
