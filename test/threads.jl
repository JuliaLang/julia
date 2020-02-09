# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Base.Threads

let p, cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no threads_exec.jl`
    # test both nthreads==1 and nthreads>1. spawn a process to test whichever
    # case we are not running currently.
    other_nthreads = nthreads() == 1 ? 4 : 1
    p = run(pipeline(setenv(cmd, "JULIA_NUM_THREADS" => other_nthreads), stdout = stdout, stderr = stderr),
            wait = false)
    include("threads_exec.jl")
    if !success(p)
        error("threads test failed with nthreads == $other_nthreads")
    end
end

# issue #34415 - make sure external affinity settings work
if Sys.islinux() && Sys.CPU_THREADS > 1 && Sys.which("taskset") !== nothing
    run_with_affinity(spec) = readchomp(`taskset -c $spec $(Base.julia_cmd()) -e "run(\`taskset -p \$(getpid())\`)"`)
    @test endswith(run_with_affinity("1"), "2")
    @test endswith(run_with_affinity("0,1"), "3")
end
