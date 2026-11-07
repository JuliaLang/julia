
## Tests that compilation in the interactive session startup are as expected

using Test
Base.include(@__MODULE__, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty

if !Sys.iswindows()
    # TODO: reenable this on Windows. Without it we're not checking that Windows startup has no compilation.
    # On Windows CI runners using `open_fake_pty` is causing:
    # ----
    # `stty: 'standard input': Inappropriate ioctl for device
    # Unhandled Task ERROR: failed process: Process(`stty raw -echo onlcr -ocrnl opost`, ProcessExited(1)) [1]
    # ----
    @testset "No interactive startup compilation" begin
        f, _ = mktemp()

        # start an interactive session, ensuring `TERM` is unset since it can trigger
        # different amounts of precompilation stemming from `base/terminfo.jl` depending
        # on the value, making the test here unreliable
        cmd = addenv(`$(Base.julia_cmd()) --trace-compile=$f -q --startup-file=no -i`,
                     Dict("TERM" => ""))
        pts, ptm = open_fake_pty()
        p = run(cmd, pts, pts, pts; wait=false)
        Base.close_stdio(pts)
        std = readuntil(ptm, "julia>")
        # check for newlines instead of equality with "julia>" because color may be on
        occursin("\n", std) && @info "There was output before the julia prompt:\n$std"
        @async write(ptm, "\n")  # another prompt
        readuntil(ptm, "julia>")
        @async write(ptm, "\n")  # another prompt
        readuntil(ptm, "julia>")
        tracecompile_out = read(f, String)
        close(ptm) # close after reading so we don't get precompiles from error shutdown
        wait(p) # the session exits on the hangup

        # given this test checks that startup is snappy, it's best to add workloads to
        # contrib/generate_precompile.jl rather than increase this number. But if that's not
        # possible, it'd be helpful to add a comment with the statement and a reason below
        expected_precompiles = 0

        n_precompiles = count(r"precompile\(", tracecompile_out)

        @test n_precompiles <= expected_precompiles

        if n_precompiles == 0
            @debug "REPL: trace compile output: (none)"
        elseif n_precompiles > expected_precompiles
            @info "REPL: trace compile output:\n$tracecompile_out"
        else
            @debug "REPL: trace compile output:\n$tracecompile_out"
        end
        # inform if lowered
        if expected_precompiles > 0 && (n_precompiles < expected_precompiles)
            @info "REPL: Actual number of precompiles has dropped below expected." n_precompiles expected_precompiles
        end

    end

    @testset "Interactive session exits cleanly when its terminal goes away" begin
        errf, errio = mktemp()
        cmd = addenv(`$(Base.julia_cmd()) -q --startup-file=no -i`, Dict("TERM" => ""))
        pts, ptm = open_fake_pty()
        p = run(pipeline(cmd; stdin=pts, stdout=pts, stderr=errio); wait=false)
        Base.close_stdio(pts)
        close(errio)
        try
            readuntil(ptm, "julia>")
            close(ptm) # hang up the terminal
            exited = timedwait(() -> process_exited(p), 60) === :ok
            @test exited
            if exited
                @test success(p)
                # no failed writes to the dead terminal reported on the way out
                @test !occursin(r"IOError|fatal"i, read(errf, String))
            end
        finally
            process_running(p) && kill(p, Base.SIGKILL)
            wait(p)
        end
    end
end
