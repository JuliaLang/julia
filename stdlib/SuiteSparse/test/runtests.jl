# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random
using SuiteSparse, LinearAlgebra, SparseArrays

if Base.USE_GPL_LIBS
    include("umfpack.jl")
    include("cholmod.jl")
    include("spqr.jl")

    # Test multithreaded execution
    let p, cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no threads.jl`
        # test both nthreads==1 and nthreads>1. spawn a process to test whichever
        # case we are not running currently.
        other_nthreads = Threads.nthreads() == 1 ? 4 : 1
        p = run(
                pipeline(
                    setenv(
                        cmd,
                        "JULIA_NUM_THREADS" => other_nthreads,
                        dir=@__DIR__()),
                    stdout = stdout,
                    stderr = stderr),
                wait = false)
        include("threads.jl")
        if !success(p)
            error("SuiteSparse threads test failed with nthreads == $other_nthreads")
        end
    end
end
