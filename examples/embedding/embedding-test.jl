# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests the output of the embedding example is correct
using Base.Test

@test length(ARGS) == 1
let
    stdout = Pipe()
    stderr = Pipe()
    p = spawn(pipeline(Cmd(ARGS), stdin=DevNull, stdout=stdout, stderr=stderr))
    close(stdout.in)
    close(stderr.in)
    stderr_task = @async readlines(stderr)
    lines = readlines(stdout)
    @test length(lines) == 6
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    lines = wait(stderr_task)
    @test lines == ["UndefVarError(:this_function_does_not_exist)"]
    @test success(p)
end
