# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests signal handling in --handle-signals=minimal mode
using Test

@test length(ARGS) == 1

@testset "minimal signal handling" begin
    out = Pipe()
    err = Pipe()
    embedded_cmd_path = abspath(ARGS[1])
    p = cd(@__DIR__) do
        run(pipeline(Cmd([embedded_cmd_path]), stdin=devnull, stdout=out, stderr=err), wait=false)
    end
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err_task = @async readlines(err)
    @test success(p)
    lines = fetch(out_task)

    # Test 1: SIGSEGV is forwarded to pre-existing handler
    @test lines[1] == "SIGSEGV forwarded: OK"

    # Test 2: Multi-threaded workload completes (safepoints work)
    @test startswith(lines[2], "threading: nthreads=")
    nthreads = parse(Int, match(r"nthreads=(\d+)", lines[2]).captures[1])
    @test nthreads >= 2

    # Test 3: SIGINFO/SIGUSR1 handler not installed in minimal mode
    @test endswith(lines[3], "handler: SIG_DFL")
end
