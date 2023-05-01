# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

function run_gctest(file)
    let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no $file`
        @testset for test_nthreads in (1, 2, 4)
            new_env = copy(ENV)
            new_env["JULIA_NUM_THREADS"] = string(test_nthreads)
            new_env["JULIA_NUM_GC_THREADS"] = string(test_nthreads)
            @test success(run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr)))
        end
    end
end

@testset "GC threads" begin
    run_gctest("gc/binarytree.jl")
    run_gctest("gc/linkedlist.jl")
    run_gctest("gc/objarray.jl")
    run_gctest("gc/chunks.jl")
end
