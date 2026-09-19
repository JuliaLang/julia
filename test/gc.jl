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
    run_gctest("gc/copyto.jl")
end

@testset "errno survives allocation" begin
    # The threads contend for the allocator's locks, and `:static` keeps each task on its
    # thread (errno is per-thread) between setting errno and reading it back.
    prog = """
        Threads.@threads :static for _ in 1:Threads.nthreads()
            keep = Vector{Vector{UInt8}}(undef, 2000) # keeps the allocations from being optimized out
            for i in eachindex(keep)
                Libc.errno(0xc0ffee)
                keep[i] = Vector{UInt8}(undef, 1024)
                Libc.errno() == 0xc0ffee || exit(1)
            end
        end
        """
    cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no -t16 -e $prog`
    @test success(cmd)
end

# While finalizers run, the list of pending ones is rooted as a GC frame of
# its own kind, whose slots may carry tags that other frames must not be
# scanned for. Collecting and walking the stack from inside such a batch has
# to leave the rest of the list intact.
# atomic: finalizer batches of different threads may run concurrently
const FIN_CFUNC_RAN = Threads.Atomic{Int}(0)
const FIN_JULIA_RAN = Threads.Atomic{Int}(0)
const FIN_NESTED_RAN = Threads.Atomic{Int}(0)
const FIN_BACKTRACE_FRAMES = Threads.Atomic{Int}(0)

fin_cfunc_callback(::Ptr{Cvoid}) = (Threads.atomic_add!(FIN_CFUNC_RAN, 1); nothing)
const FIN_CFUNC_PTR = @cfunction(fin_cfunc_callback, Cvoid, (Ptr{Cvoid},))

fin_julia_callback(@nospecialize(_)) = (Threads.atomic_add!(FIN_JULIA_RAN, 1); nothing)
fin_nested_callback(@nospecialize(_)) = (Threads.atomic_add!(FIN_NESTED_RAN, 1); nothing)
fin_collect_callback(@nospecialize(_)) = (GC.gc(true); nothing)
fin_backtrace_callback(@nospecialize(_)) = (FIN_BACKTRACE_FRAMES[] = length(backtrace()); nothing)

# `@noinline` and a separate frame so the registered objects are unreachable
# by the time the caller collects.
@noinline function register_finalizer_batch(n)
    for i in 1:n
        # `@cfunction` entries and Julia closures are tagged differently in
        # the finalizer list, so both must appear in the same batch
        finalizer(isodd(i) ? FIN_CFUNC_PTR : fin_julia_callback, Ref(i))
    end
    finalizer(fin_collect_callback, Ref(0))
    finalizer(fin_backtrace_callback, Ref(0))
    nested = Ref(0)
    finalizer(fin_nested_callback, nested)
    finalizer(_ -> finalize(nested), Ref(0))
    nothing
end

@testset "collection during finalization" begin
    n = 200
    FIN_CFUNC_RAN[] = 0
    FIN_JULIA_RAN[] = 0
    FIN_NESTED_RAN[] = 0
    FIN_BACKTRACE_FRAMES[] = 0
    register_finalizer_batch(n)
    GC.gc(true)
    GC.gc(true)
    GC.gc(true)
    @test FIN_CFUNC_RAN[] == count(isodd, 1:n)
    @test FIN_JULIA_RAN[] == count(iseven, 1:n)
    # explicit `finalize` from inside a finalizer, i.e. a nested batch
    @test FIN_NESTED_RAN[] == 1
    # stack walking works while a finalizer-list frame is installed
    @test FIN_BACKTRACE_FRAMES[] > 0
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

@testset "GC Always Full" begin
    prog = "using Test;\n
        for _ in 1:10; GC.gc(); end;\n
        reasons = Base.full_sweep_reasons();\n
        @test reasons[:FULL_SWEEP_REASON_SWEEP_ALWAYS_FULL] >= 10;"
    cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no --gc-sweep-always-full -e $prog`
    @test success(cmd)
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
