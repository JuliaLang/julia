# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# https://github.com/JuliaLang/julia/pull/42973
@testset "spawn and wait *a lot* of tasks in @profile" begin
    # Not using threads_exec.jl for better isolation, reproducibility, and a
    # tighter timeout.
    script = "profile_spawnmany_exec.jl"
    cmd_base = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no $script`
    @testset for n in [20000, 200000, 2000000]
        cmd = ignorestatus(addenv(Cmd(cmd_base; dir = @__DIR__), "NTASKS" => n))
        cmd = pipeline(cmd; stdout = stderr, stderr)
        proc = run(cmd; wait = false)
        done = Threads.Atomic{Bool}(false)
        timeout = false
        timer = Timer(200) do _
            timeout = true
            for sig in (Base.SIGQUIT, Base.SIGKILL)
                for _ in 1:3
                    kill(proc, sig)
                    sleep(1)
                    if done[]
                        @warn "Terminating `$script` required signal $sig"
                        return
                    end
                end
            end
        end
        try
            wait(proc)
        finally
            @atomic done[] = true
            close(timer)
        end
        if !success(proc) || timeout
            @error "A \"spawn and wait lots of tasks\" test failed" n proc.exitcode proc.termsignal success(proc) timeout
        end
        @test success(proc)
        @test !timeout
    end
end
