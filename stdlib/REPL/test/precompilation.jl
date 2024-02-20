
## Tests that compilation in the interactive session startup are as expected

using Test
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
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

        # start an interactive session
        cmd = `$(Base.julia_cmd()[1]) --trace-compile=$f -q --startup-file=no -i`
        pts, ptm = open_fake_pty()
        p = run(cmd, pts, pts, pts; wait=false)
        Base.close_stdio(pts)
        std = readuntil(ptm, "julia>")
        # check for newlines instead of equality with "julia>" because color may be on
        occursin("\n", std) && @info "There was output before the julia prompt:\n$std"
        sleep(1) # sometimes precompiles output just after prompt appears
        tracecompile_out = read(f, String)
        close(ptm) # close after reading so we don't get precompiles from error shutdown

        expected_precompiles = 1

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
end
