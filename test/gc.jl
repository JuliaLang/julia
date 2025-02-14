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

function run_pg_size_test()
    page_size = @ccall jl_get_pg_size()::UInt64
    # supported page sizes: 4KB and 16KB
    @test page_size == (1 << 12) || page_size == (1 << 14)
end

function issue_54275_alloc_string()
    String(UInt8['a' for i in 1:10000000])
end

function issue_54275_test()
    GC.gc(true)
    baseline = Base.gc_live_bytes()
    live_bytes_has_grown_too_much = false
    for _ in 1:10
        issue_54275_alloc_string()
        GC.gc(true)
        if Base.gc_live_bytes() - baseline > 1_000_000
            live_bytes_has_grown_too_much = true
            break
        end
    end
    @test !live_bytes_has_grown_too_much
end

function full_sweep_reasons_test()
    GC.gc()
    reasons = Base.full_sweep_reasons()
    @test reasons[:FULL_SWEEP_REASON_FORCED_FULL_SWEEP] >= 1
    @test keys(reasons) == Set(Base.FULL_SWEEP_REASONS)
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
end

#FIXME: Issue #57103 disabling tests for MMTk, since
# they rely on information that is specific to the stock GC.
@static if Base.USING_STOCK_GC
@testset "GC page metrics" begin
    run_nonzero_page_utilization_test()
    run_pg_size_test()
end

@testset "issue-54275" begin
    issue_54275_test()
end

@testset "Full GC reasons" begin
    full_sweep_reasons_test()
end
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
