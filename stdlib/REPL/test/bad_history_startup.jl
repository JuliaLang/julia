# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test that interactive mode starts up without error when history file is bad

using Test

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :FakePTYs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FakePTYs.jl"))
import .Main.FakePTYs: with_fake_pty

@testset "Bad history file startup" begin
    mktempdir() do tmpdir
        # Create a bad history file
        hist_file = joinpath(tmpdir, "repl_history.jl")
        write(hist_file, "{ invalid json content\nmore bad content\n")

        julia_exe = Base.julia_cmd()[1]

        # Test interactive Julia startup with bad history file
        with_fake_pty() do pts, ptm
            # Set up environment with our bad history file
            nENV = copy(ENV)
            nENV["JULIA_HISTORY"] = hist_file

            # Start Julia in interactive mode
            p = run(detach(setenv(`$julia_exe --startup-file=no --color=no -q`, nENV)), pts, pts, pts, wait=false)
            Base.close_stdio(pts)

            # Read output until we get the prompt, which indicates successful startup
            output = readuntil(ptm, "julia> ", keep=true)
            # println("====== subprocess output ======")
            # println(output)
            # println("====== end subprocess output ======")

            # Test conditions:
            # 1. We should see the invalid history file error
            has_history_error = occursin("Invalid history file", output) ||
                              occursin("Invalid character", output)
            @test has_history_error

            # 2. We should NOT see UndefRefError (the bug being fixed)
            has_undef_error = occursin("UndefRefError", output)
            @test !has_undef_error

            # 3. We should see the "Disabling history file" message if the fix works
            has_disable_message = occursin("Disabling history file for this session", output)
            @test has_disable_message

            # Send exit command to clean shutdown
            if isopen(ptm)
                write(ptm, "exit()\n")
            else
                @warn "PTY master is already closed before sending exit command"
            end

            # Read any remaining output until the process exits
            try
                read(ptm, String)
            catch ex
                # Handle platform-specific EOF behavior
                if ex isa Base.IOError && ex.code == Base.UV_EIO
                    # This is expected on some platforms (e.g., Linux)
                else
                    rethrow()
                end
            end

            # Wait for process to finish
            wait(p)

            @test p.exitcode == 0
        end
    end
end
