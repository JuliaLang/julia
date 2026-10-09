# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: CodeInstance, MethodInstance
using Test

struct TestOwner end
const owner = TestOwner()

function compile_no_deps(f, argtypes)
    @nospecialize
    mi = Base.method_instance(f, argtypes)
    source, _ = only(code_typed(f, argtypes))
    ci = CodeInstance(
        mi, owner, source.rettype, #=exctype=#Any, #=inferred_const=#nothing,
        #=inferred=#nothing, #=const_flags=#Int32(0), source.min_world,
        #=max_world=#typemax(UInt), #=effects=#UInt32(0),
        #=analysis_results=#nothing, source.debuginfo, source.edges
    )
    # Insert the CI into the global cache (necessary before adding to JIT)
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, ci)
    ccall(:jl_add_codeinsts_to_jit, Cvoid, (Any, Any), Any[ci], Any[source])
    ci
end

function check_edges_not_compiled(ci::CodeInstance, target)
    @nospecialize
    for e in ci.edges
        e isa CodeInstance || continue
        e.def isa MethodInstance || continue
        e.def.def isa Method || continue
        if e.def.def.sig <: Tuple{typeof(target), Vararg}
            e.invoke == Ptr{Nothing}(0) || return false
            e.specptr == Ptr{Nothing}(0) || return false
        end
    end
    true
end

# Test fptr1 -> tojlinvoke trampoline
module M1
    @noinline foo(xs...) = xs[2]
    bar(x) = 2*foo(x, x, x, x, x, x)
end
ci = compile_no_deps(M1.bar, (Int,))
@test check_edges_not_compiled(ci, M1.foo)
@test invoke(M1.bar, ci, 100) == 200

# Test specsig -> tojlinvoke trampoline
module M2
    @noinline foo(x) = x+100
    bar(x) = 2*foo(x)
end
ci = compile_no_deps(M2.bar, (Int,))
@test check_edges_not_compiled(ci, M2.foo)
@test invoke(M2.bar, ci, 5) == 210

# Compilation batches must stay closed under invoke edges whose CodeInstance is
# globally cached but whose source is only visible to another interpreter:
# `return_types` infers `bar` (and its edge `foo`) into the global cache without
# compiling anything, so the subsequent `precompile` batch sees `foo` as a
# sourceless cached edge. It must compile `foo` alongside `bar` rather than
# linking the call as a permanently-boxing `tojlinvoke` trampoline.
module M3
    @noinline foo(x) = x+1
    bar(x) = foo(x)
end
Base.return_types(M3.bar, (Int,))
let mi = Base.method_instance(M3.foo, (Int,))
    ci = mi.cache
    # Precondition for the scenario: inference cached foo's CodeInstance without
    # compiling it. If this fails, the setup no longer produces a sourceless
    # cached edge and the test needs a new way to construct one.
    @test ci isa CodeInstance
    @test ci.invoke == Ptr{Nothing}(0)
    @test precompile(M3.bar, (Int,))
    @test ci.invoke != Ptr{Nothing}(0)
    @test ci.specptr != Ptr{Nothing}(0)
end

# When runtime dispatch caches compiled code onto an exact-signature
# MethodInstance by copying it from the widened compileable MethodInstance
# (`copy_to_mi_cache`), a specsig specptr must not be adopted: it is ABI'd to
# the widened signature, and the copy would advertise
# JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR without JL_CI_FLAGS_SPECPTR_SPECIALIZED,
# tripping the flag-consistency assert in `JuliaOJIT::linkCISymbol` when a
# batch later links a call target to it. Only the boxed-ABI invoke wrapper may
# be copied.
@noinline copyspecsig(@nospecialize(x)) = x === nothing ? 0 : 1
let m = only(methods(copyspecsig))
    # the exact (non-normalized) specialization runtime dispatch would mint
    mi = ccall(:jl_specializations_get_linfo, Ref{MethodInstance},
               (Any, Any, Any), m, Tuple{typeof(copyspecsig), Int}, Core.svec())
    args = Any[1]
    # TRIGGER_FOREIGN forces the copy onto `mi` even for matching sparams
    @test ccall(:jl_invoke, Any, (Any, Ptr{Any}, UInt32, Any),
                copyspecsig, args, length(args), mi) === 1
    known_invokes = Ptr{Cvoid}[
        unsafe_load(cglobal(:jl_fptr_args_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_const_return_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_sparam_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_interpret_call_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_wait_for_compiled_addr, Ptr{Cvoid})),
    ]
    for spec in Base.specializations(m)
        ci = isdefined(spec, :cache, :acquire) ? (@atomic :acquire spec.cache) : nothing
        while ci isa CodeInstance
            flags = @atomic :acquire ci.flags
            invoke = @atomic :acquire ci.invoke
            specptr = @atomic :acquire ci.specptr
            if !iszero(flags & 0x02) && invoke != C_NULL && specptr != C_NULL
                # INVOKE_MATCHES_SPECPTR requires SPECPTR_SPECIALIZED to agree
                # with the invoke pointer's api
                @test (invoke ∉ known_invokes) == !iszero(flags & 0x01)
            end
            ci = isdefined(ci, :next, :acquire) ? (@atomic :acquire ci.next) : nothing
        end
    end
end

# External symbol renames must keep JITLink's external symbol map consistent.
@testset "JITLink external symbol rename" begin
    jitlink_rename_resolve(chunks, i, x) =
        jitlink_rename_resolve(Base.tail(chunks), i,
            map(tuple, x, i[1] === Colon() ? (1, (), 1) : (1, 1, ())))
    jitlink_rename_resolve(::Tuple{}, i, x) = x
    function jitlink_rename_reproducer(i)
        x = jitlink_rename_resolve(Base.inferencebarrier(true) ? (1,) :
            map(+, Tuple([]), (1, 1)), i, ((), (), ()))
        y = Base.inferencebarrier(true) ? :a : :b
        if y === :a; elseif y === :b
            jitlink_rename_reproducer(x[3])
        else
            0[x[2]]
        end
    end
    @test precompile(jitlink_rename_reproducer, (Tuple{Colon},))
end

# Each `eval` must compile (because of the ccall) a top-level thunk.  The
# CodeInstance for this thunk becomes garbage-collectable after being invoked,
# but before returning, because of wait().  If the invoke must return for the
# CodeInstance address to be unregistered from the JIT, this will crash.  Credit
# to @vtjnash for this example.
function test_gc_codeinst()
    for i=1:10000
        @async eval(:(ccall(:sqrt, Float64, (Float64,), $i); wait()))
        i % 100 == 0 && GC.gc()
    end
    true
end
@test test_gc_codeinst()

# A small cache must evict in bounded transactions and still exit with writes pending.
@testset "object-cache bounded eviction and shutdown" begin
    mktempdir() do dir
        logfile = joinpath(dir, "objcache.log")
        script = """
            for i in 1:600
                f = Symbol(:objcache_eviction_, i)
                @eval \$f(x) = x + \$i
                @eval \$f(1)
            end
            print(ccall(:jl_objcache_kv_enabled, Cint, ()) != 0 ? "enabled" : "disabled")
        """
        cmd = addenv(
            `$(Base.julia_cmd()) --startup-file=no --color=no -e $script`,
            "JULIA_OBJCACHE" => "1",
            "JULIA_OBJCACHE_PATH" => joinpath(dir, "cache"),
            "JULIA_OBJCACHE_CAPACITY" => string(512 << 10),
            "JULIA_OBJCACHE_LOG" => logfile,
        )
        outpath = joinpath(dir, "stdout")
        errpath = joinpath(dir, "stderr")
        completed = ok = false
        open(outpath, "w") do stdout
            open(errpath, "w") do stderr
                proc = run(pipeline(cmd; stdout, stderr), wait=false)
                completed = timedwait(() -> process_exited(proc), 180; pollint=0.05) === :ok
                process_running(proc) && kill(proc, Base.SIGKILL)
                wait(proc)
                ok = completed && success(proc)
            end
        end
        if !ok
            @info "object-cache child failed" stdout=read(outpath, String) stderr=read(errpath, String)
        end
        @test completed
        @test ok
        status = read(outpath, String)
        @test status in ("enabled", "disabled")
        lines = isfile(logfile) ? readlines(logfile) : String[]
        nevicted = count(startswith("evict,"), lines)
        if status == "disabled"
            @test_skip false
        else
            @test nevicted > 0
            batches = [parse(Int, split(line, ',')[2]) for line in lines
                       if startswith(line, "evict_batch,")]
            @test length(batches) > 1
            @test all(n -> 1 <= n <= 64, batches)
        end
    end
end

# The default database belongs to one target; an explicit path is used verbatim.
@testset "object-cache per-target directory" begin
    mktempdir() do depot
        script = "print(ccall(:jl_objcache_kv_enabled, Cint, ()) != 0)"
        cmd = addenv(
            `$(Base.julia_cmd()) --startup-file=no -e $script`,
            "JULIA_DEPOT_PATH" => depot,
            "JULIA_OBJCACHE" => "1",
            "JULIA_OBJCACHE_PATH" => nothing,
        )
        enabled = read(cmd, String)
        @test enabled in ("true", "false")
        if enabled == "false"
            @test_skip false
        else
            cachedir = joinpath(depot, "cache", "v$(VERSION.major).$(VERSION.minor)", "objcache-lmdb1")
            entries = readdir(cachedir)
            @test length(entries) == 1
            targetdir = joinpath(cachedir, only(entries))
            @test isfile(joinpath(targetdir, "data.mdb"))
            @test isfile(joinpath(targetdir, "lock.mdb"))
            @test !isfile(joinpath(cachedir, "data.mdb"))

            explicit = joinpath(depot, "explicit")
            @test read(addenv(cmd, "JULIA_OBJCACHE_PATH" => explicit), String) == "true"
            @test isfile(joinpath(explicit, "data.mdb"))
            @test isfile(joinpath(explicit, "lock.mdb"))
        end
    end
end

# `--prune-objcache` removes the entries not used recently and shrinks the cache file.
@testset "object-cache pruning" begin
    mktempdir() do dir
        cache = joinpath(dir, "cache")
        data = joinpath(cache, "data.mdb")
        julia(args...) = addenv(`$(Base.julia_cmd()) --startup-file=no $args`,
                                "JULIA_OBJCACHE" => "1", "JULIA_OBJCACHE_PATH" => cache)
        kv_get = """ccall(:jl_objcache_kv_get, Any, (Cstring, Ptr{UInt8}, Csize_t), "prune-test", "key", 3)"""
        # a large entry, so that removing it clearly shrinks the file
        put_script = """
            value = fill(0x2a, 4 << 20)
            ccall(:jl_objcache_kv_put, Cint, (Cstring, Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
                  "prune-test", "key", 3, value, length(value))
            # the write is queued, so wait until it is stored
            for _ in 1:600
                $kv_get === nothing || break
                sleep(0.1)
            end
            print($kv_get !== nothing)
        """
        has_entry() = read(julia("-e", "print($kv_get !== nothing)"), String) == "true"
        compact_dirs() = filter(startswith("compact."), readdir(cache))

        if read(julia("-e", put_script), String) != "true"
            @test_skip false # the cache is disabled here
            return
        end
        full_size = filesize(data)

        # nothing is older than an hour, so only the file is compacted
        @test startswith(read(julia("--prune-objcache=1h"), String), "Removed 0 entries")
        @test has_entry()
        @test isempty(compact_dirs())

        # entries cannot age during a test, so remove them all with a cutoff in the future
        prune_all = "print(ccall(:jl_objcache_prune, Int64, (Int64,), floor(Int64, time()) + 10^6))"
        @test parse(Int, read(julia("-e", prune_all), String)) > 0
        @test !has_entry()
        # removing entries alone does not shrink the file
        @test filesize(data) >= full_size

        @test occursin("shrinks", read(julia("--prune-objcache"), String))
        @test filesize(data) < full_size ÷ 2
        @test isempty(compact_dirs())
        # the replaced database still works
        @test read(julia("-e", put_script), String) == "true"

        # a process that has the cache open keeps the file from being replaced
        holder = open(julia("-e", "ccall(:jl_objcache_kv_enabled, Cint, ()); print(\"ready\"); readline()"), "r+")
        try
            @test readuntil(holder, "ready") == ""
            err = IOBuffer()
            @test success(pipeline(julia("--prune-objcache"); stdout=devnull, stderr=err))
            @test occursin("in use by another process", String(take!(err)))
            @test has_entry()
            @test isempty(compact_dirs())
        finally
            println(holder)
            wait(holder)
        end
    end

    @test occursin("disabled", read(addenv(`$(Base.julia_cmd()) --startup-file=no --prune-objcache`,
                                           "JULIA_OBJCACHE" => "0"), String))
    @test !success(pipeline(`$(Base.julia_cmd()) --startup-file=no --prune-objcache=7x`; stderr=devnull))
end

sleep(5)  # Avoids problems where we don't respond to Distributed.jl fast enough
