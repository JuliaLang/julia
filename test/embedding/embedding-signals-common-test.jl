# This file is a part of Julia. License is MIT: https://julialang.org/license

# Shared test logic for embedding-signals tests.
# Included by the platform-specific test scripts, which must define:
#   embedded_cmd_path::String - path to the embedding-signals binary

using Test

@testset "minimal signal handling" begin
    out = Pipe()
    err = Pipe()
    cmd = Cmd([embedded_cmd_path])
    if Sys.iswindows()
        # Spawn in a new process group so the child can use
        # GenerateConsoleCtrlEvent without sending Ctrl-C to the test harness.
        cmd = Cmd(cmd; detach=true)
    end
    p = cd(@__DIR__) do
        run(pipeline(cmd, stdin=devnull, stdout=out, stderr=err), wait=false)
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

    # Test 3: I/O-like signal handlers not installed in minimal mode
    if Sys.iswindows()
        @test startswith(lines[3], "ConsoleCtrlHandler: OK")
    else
        @test endswith(lines[3], "handler: SIG_DFL")
    end
end
