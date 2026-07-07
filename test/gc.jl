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

function run_gc_aba_sweep_crash_oracle()
    nthreads = parse(Int, get(ENV, "JULIA_GC_ABA_SWEEP_THREADS",
        string(min(max(4 * Sys.CPU_THREADS, 8), 64))))
    ngcthreads = parse(Int, get(ENV, "JULIA_GC_ABA_SWEEP_GCTHREADS",
        string(min(max(Sys.CPU_THREADS, 2), 8))))
    stress = get(ENV, "CI_STRESS", "") == "1"
    iters = something(tryparse(Int, get(ENV, "JULIA_GC_ABA_SWEEP_ITERS", "")),
        stress ? 24_000_000 : 200_000)
    heap_hint = get(ENV, "JULIA_GC_ABA_HEAP_HINT", "32M")
    timeout_s = something(tryparse(Float64, get(ENV, "JULIA_GC_ABA_SWEEP_TIMEOUT", "")),
        stress ? 300.0 : 60.0)
    attempts = something(tryparse(Int, get(ENV, "JULIA_GC_ABA_SWEEP_ATTEMPTS", "")),
        stress ? 20 : 1)
    seed = 0x9e3779b9

    prog = """
        sizefor(t) = (4, 6, 8, 12, 16, 24, 32, 48)[(t & 7) + 1]

        @noinline function hammer(seed, iters, wsize)
            r = seed
            acc = 0
            a = Vector{Int}(undef, wsize)
            b = Vector{Int}(undef, wsize)
            for i in 1:iters
                r = xor(r, r << 13); r = xor(r, r >> 7); r = xor(r, r << 17)
                c = Vector{Int}(undef, wsize)
                @inbounds c[1] = i
                @inbounds c[wsize] = r
                acc += @inbounds a[1] + b[wsize]
                a = b
                b = c
            end
            return acc
        end

        function main()
            tasks = Vector{Task}(undef, Threads.nthreads())
            s = 0
            for t in 1:Threads.nthreads()
                tasks[t] = Threads.@spawn hammer(Int($(seed)) * t + 1, $iters, sizefor(t))
            end
            for t in tasks
                s += fetch(t)
            end
            println("GC_ABA_DONE iterations=$iters checksum=\$(s)")
            return s
        end

        main()
        exit(0)
    """

    cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no -t $nthreads --gcthreads=$ngcthreads,1 --heap-size-hint=$heap_hint -e $prog`
    rerun_cmd = "JULIA_GC_ABA_SWEEP_THREADS=$nthreads JULIA_GC_ABA_SWEEP_GCTHREADS=$ngcthreads JULIA_GC_ABA_SWEEP_ITERS=$iters JULIA_GC_ABA_HEAP_HINT=$heap_hint JULIA_GC_ABA_SWEEP_TIMEOUT=$timeout_s JULIA_GC_ABA_SWEEP_ATTEMPTS=$attempts make test-revise-gc"

    function run_with_timeout(cmd, timeout_s)
        output_file = tempname()
        proc = open(output_file, "w") do output
            run(pipeline(ignorestatus(cmd); stdout=output, stderr=output), wait=false)
        end
        deadline = time() + timeout_s
        timed_out = false
        while !process_exited(proc)
            if time() >= deadline
                timed_out = true
                kill(proc)
                break
            end
            sleep(0.1)
        end
        wait(proc)
        output = read(output_file, String)
        rm(output_file; force=true)
        return (; ok=!timed_out && success(proc), timed_out, output)
    end

    result = nothing
    attempt = 0
    for i in 1:attempts
        attempt = i
        result = run_with_timeout(cmd, timeout_s)
        result.ok || break
    end
    done_match = match(r"GC_ABA_DONE iterations=(\d+)", result.output)
    iterations_before_failure = done_match === nothing ? "unknown (configured iters=$iters)" : done_match.captures[1]
    cpu_model = isempty(Sys.cpu_info()) ? "unknown" : Sys.cpu_info()[1].model
    kernel = try readchomp(`uname -srvm`) catch; "$(Sys.KERNEL) $(Sys.MACHINE)" end
    runtime_versions = "julia=$(VERSION) git=$(Base.GIT_VERSION_INFO.commit_short)"
    library_versions = "llvm=$(isdefined(Base, :libllvm_version) ? Base.libllvm_version : "unknown")"
    scheduler_settings = "julia_threads=$nthreads gc_threads=$ngcthreads,1 cpu_threads=$(Sys.CPU_THREADS)"
    gc_settings = "heap_size_hint=$heap_hint gcthreads=$ngcthreads,1"
    timing_thresholds = "timeout_s=$timeout_s attempts=$attempts"
    input_size = "iters=$iters size_classes=(4,6,8,12,16,24,32,48)"

    if !result.ok
        @error "GC concurrent sweep reuse crash oracle failed" cmd rerun_cmd nthreads ngcthreads attempt attempts cpu_model gpu_model="unknown" kernel runtime_versions library_versions gc_settings scheduler_settings timing_thresholds input_size random_seed=string(seed, base=16) iterations_before_failure timed_out=result.timed_out output=result.output
    else
        @info "GC concurrent sweep reuse crash oracle passed" nthreads ngcthreads iters heap_hint timeout_s attempts stress
    end
    @test result.ok
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

@testset "GC concurrent sweep reuse stress smoke" begin
    run_gc_aba_sweep_crash_oracle()
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
