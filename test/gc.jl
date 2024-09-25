# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

function run_gctest(file)
    let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no $file`
        @testset for test_nthreads in (1, 2, 4)
            @testset for test_nithreads in (0, 1)
                @testset for concurrent_sweep in (0, 1)
                    new_env = copy(ENV)
                    new_env["JULIA_NUM_THREADS"] = "$test_nthreads,$test_nithreads"
                    new_env["JULIA_NUM_GC_THREADS"] = "$(test_nthreads),$(concurrent_sweep)"
                    @test success(run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr)))
                end
            end
        end
    end
end

function run_nonzero_page_utilization_test()
    GC.gc()
    page_utilization = Base.gc_page_utilization_data()
    # at least one of the pools should have nonzero page_utilization
    @test any(page_utilization .> 0)
end

# !!! note:
#     Since we run our tests on 32bit OS as well we confine ourselves
#     to parameters that allocate about 512MB of objects. Max RSS is lower
#     than that.
@testset "GC threads" begin
    run_gctest("gc/binarytree.jl")
    run_gctest("gc/linkedlist.jl")
    run_gctest("gc/objarray.jl")
    run_gctest("gc/chunks.jl")
    run_nonzero_page_utilization_test()
end

@testset "Base.GC docstrings" begin
    @test isempty(Docs.undocumented_names(GC))
end

#testset doesn't work here because this needs to run in top level
#Check that we ensure objects in toplevel exprs are rooted
global dims54422 = [] # allocate the Binding
GC.gc(); GC.gc(); # force the binding to be old
GC.enable(false); # prevent new objects from being old
@eval begin
    Base.Experimental.@force_compile # use the compiler
    dims54422 = $([])
    nothing
end
GC.enable(true); GC.gc(false) # incremental collection
@test typeof(dims54422) == Vector{Any}
@test isempty(dims54422)
