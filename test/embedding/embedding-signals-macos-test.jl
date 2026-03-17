# This file is a part of Julia. License is MIT: https://julialang.org/license

@assert length(ARGS) == 1
embedded_cmd_path = abspath(ARGS[1])

using Test

@testset "Mach exception forwarding" begin
    out = Pipe()
    err = Pipe()
    cmd = Cmd([embedded_cmd_path])
    p = cd(@__DIR__) do
        run(pipeline(cmd, stdin=devnull, stdout=out, stderr=err), wait=false)
    end
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err_task = @async readlines(err)
    @test success(p)
    lines = fetch(out_task)
    err_lines = fetch(err_task)

    if !isempty(err_lines)
        @warn "stderr output from embedding test" err_lines
    end

    # Test 1: Mach exception forwarded to pre-existing handler
    @test lines[1] == "Mach exception forwarded: OK"

    # Test 2: Multi-threaded workload completes (safepoints work)
    @test startswith(lines[2], "threading: nthreads=")
    nthreads = parse(Int, match(r"nthreads=(\d+)", lines[2]).captures[1])
    @test nthreads >= 2

    # Test 3: Exception parameters preserved through forwarding
    @test lines[3] == "Parameters preserved: OK"
end
