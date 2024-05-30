# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using Base.Threads

include("print_process_affinity.jl") # import `uv_thread_getaffinity`

# simple sanity tests for locks under cooperative concurrent access
let lk = ReentrantLock()
    c1 = Event()
    c2 = Event()
    @test trylock(lk)
    @test trylock(lk)
    t1 = @async (notify(c1); lock(lk); unlock(lk); trylock(lk))
    t2 = @async (notify(c2); trylock(lk))
    wait(c1)
    wait(c2)
    @test t1.queue === lk.cond_wait.waitq
    @test t2.queue !== lk.cond_wait.waitq
    @test istaskdone(t2)
    @test !fetch(t2)
    unlock(lk)
    @test t1.queue === lk.cond_wait.waitq
    unlock(lk)
    @test t1.queue !== lk.cond_wait.waitq
    @test fetch(t1)
end

let e = Event(), started1 = Event(false), started2 = Event(false)
    for i = 1:3
        done1 = false
        done2 = false
        t1 = @async (notify(started1); wait(e); done1 = true)
        t2 = @async (notify(started2); wait(e); done2 = true)
        wait(started1)
        wait(started2)
        sleep(0.1)
        @test !done1 && !done2
        notify(e)
        wait(t1)
        @test done1
        wait(t2)
        @test done2
        wait(e)
        notify(e)
        reset(e)
    end
end

let e = Event(true), started1 = Event(true), started2 = Event(true), done = Event(true)
    for i = 1:3
        done1 = false
        done2 = false
        t1 = @async (notify(started1); wait(e); done1 = true; notify(done))
        t2 = @async (notify(started2); wait(e); done2 = true; notify(done))
        wait(started1)
        wait(started2)
        sleep(0.1)
        @test !done1 && !done2
        notify(e)
        wait(done)
        @test done1 ⊻ done2
        done1 ? wait(t1) : wait(t2)
        notify(e)
        wait(t1)
        @test done1
        wait(t2)
        @test done2
        wait(done)
    end
end

let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no threads_exec.jl`
    for test_nthreads in (1, 2, 4, 4) # run once to try single-threaded mode, then try a couple times to trigger bad races
        new_env = copy(ENV)
        new_env["JULIA_NUM_THREADS"] = string(test_nthreads)
        run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr))
    end
end

# Timing-sensitive tests can fail on CI due to occasional unexpected delays,
# so this test is disabled.
#=
let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no threadpool_latency.jl`
    for test_nthreads in (1, 2)
        new_env = copy(ENV)
        new_env["JULIA_NUM_THREADS"] = string(test_nthreads, ",1")
        run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr))
    end
end
=#
let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no threadpool_use.jl`
    new_env = copy(ENV)
    new_env["JULIA_NUM_THREADS"] = "1,1"
    run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr))
end

function run_with_affinity(cpus)
    script = joinpath(@__DIR__, "print_process_affinity.jl")
    return readchomp(setcpuaffinity(`$(Base.julia_cmd()) $script`, cpus))
end

# issue #34415 - make sure external affinity settings work
if Sys.islinux()
    const SYS_rrcall_check_presence = 1008
    global running_under_rr() = 0 == ccall(:syscall, Int,
        (Int, Int, Int, Int, Int, Int, Int),
        SYS_rrcall_check_presence, 0, 0, 0, 0, 0, 0)
else
    global running_under_rr() = false
end
# Note also that libuv does not support affinity in macOS and it is known to
# hang in FreeBSD. So, it's tested only in Linux and Windows:
const AFFINITY_SUPPORTED = (Sys.islinux() || Sys.iswindows()) && !running_under_rr()

if AFFINITY_SUPPORTED
    allowed_cpus = findall(uv_thread_getaffinity())
    if length(allowed_cpus) ≥ 2
        @test run_with_affinity(allowed_cpus[1:1]) == "$(allowed_cpus[1])"
        @test run_with_affinity(allowed_cpus[1:2]) == "$(allowed_cpus[1]),$(allowed_cpus[2])"
    end
end

function get_nthreads(options = ``; cpus = nothing)
    cmd = `$(Base.julia_cmd()) --startup-file=no $(options)`
    cmd = `$cmd -e "print(Threads.threadpoolsize())"`
    cmd = addenv(cmd, "JULIA_EXCLUSIVE" => "0", "JULIA_NUM_THREADS" => "auto")
    if cpus !== nothing
        cmd = setcpuaffinity(cmd, cpus)
    end
    return parse(Int, read(cmd, String))
end

@testset "nthreads determined based on CPU affinity" begin
    if AFFINITY_SUPPORTED
        allowed_cpus = findall(uv_thread_getaffinity())
        if length(allowed_cpus) ≥ 2
            @test get_nthreads() ≥ 2
            @test get_nthreads(cpus = allowed_cpus[1:1]) == 1
            @test get_nthreads(cpus = allowed_cpus[2:2]) == 1
            @test get_nthreads(cpus = allowed_cpus[1:2]) == 2
            @test get_nthreads(`-t1`, cpus = allowed_cpus[1:1]) == 1
            @test get_nthreads(`-t1`, cpus = allowed_cpus[2:2]) == 1
            @test get_nthreads(`-t1`, cpus = allowed_cpus[1:2]) == 1

            if length(allowed_cpus) ≥ 3
                @test get_nthreads(cpus = allowed_cpus[1:2:3]) == 2
                @test get_nthreads(cpus = allowed_cpus[2:3])   == 2
            end
        end
    end
end

# issue #34769
function idle_callback(handle)
    idle = Base.@handle_as handle UvTestIdle
    if idle.active
        idle.count += 1
        if idle.count == 1
            # We want to hit the case where we're allowing
            # the thread to go to sleep, which only happens
            # after some default amount of time (DEFAULT_THREAD_SLEEP_THRESHOLD)
            # so spend that amount of time here.
            Libc.systemsleep(0.004)
        elseif idle.count >= 10
            lock(idle.cond)
            try
                notify(idle.cond, true)
            finally
                unlock(idle.cond)
            end
            idle.active = false
        end
    end
    nothing
end

mutable struct UvTestIdle
    handle::Ptr{Cvoid}
    cond::Base.ThreadSynchronizer
    isopen::Bool
    active::Bool
    count::Int

    function UvTestIdle()
        this = new(Libc.malloc(Base._sizeof_uv_idle), Base.ThreadSynchronizer(), true, false, 0)
        Base.iolock_begin()
        Base.associate_julia_struct(this.handle, this)
        err = ccall(:uv_idle_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
            Base.eventloop(), this.handle)
        if err != 0
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_idle_init", err))
        end
        err = ccall(:uv_idle_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
            this.handle, @cfunction(idle_callback, Cvoid, (Ptr{Cvoid},)))
        if err != 0
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_idle_start", err))
        end
        finalizer(Base.uvfinalize, this)
        Base.iolock_end()
        return this
    end
end
Base.unsafe_convert(::Type{Ptr{Cvoid}}, idle::UvTestIdle) = idle.handle

function Base.uvfinalize(t::UvTestIdle)
    Base.iolock_begin()
    Base.lock(t.cond)
    try
        if t.handle != C_NULL
            Base.disassociate_julia_struct(t.handle) # not going to call the usual close hooks
            if t.isopen
                t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
            end
            t.handle = C_NULL
            notify(t.cond, false)
        end
    finally
        unlock(t.cond)
    end
    Base.iolock_end()
    nothing
end

function Base.close(idle::UvTestIdle)
    Base.uvfinalize(idle)
end

function Base.wait(idle::UvTestIdle)
    Base.iolock_begin()
    Base.preserve_handle(idle)
    Base.lock(idle.cond)
    try
        idle.active = true
        Base.iolock_end()
        wait(idle.cond)
    finally
        Base.unlock(idle.cond)
        Base.iolock_begin()
        Base.unpreserve_handle(idle)
        Base.iolock_end()
    end
end

# Spawn another process as a watchdog. If this test fails, it'll unrecoverably
# hang in the event loop. Another process needs to kill it
cmd = """
    @async (Base.wait_readnb(stdin, 1); exit())
    sleep(100)
    isopen(stdin) || exit()
    println(stderr, "ERROR: Killing threads test due to watchdog expiry")
    ccall(:uv_kill, Cint, (Cint, Cint), $(getpid()), Base.SIGTERM)
"""
proc = open(pipeline(`$(Base.julia_cmd()) -e $cmd`; stderr=stderr); write=true)

let idle=UvTestIdle()
    wait(idle)
    close(idle)
end

@threads for i = 1:1
    let idle=UvTestIdle()
        wait(idle)
        close(idle)
    end
end

@test process_running(proc)

# We don't need the watchdog anymore
close(proc.in)

# https://github.com/JuliaLang/julia/pull/42973
@testset "spawn and wait *a lot* of tasks in @profile" begin
    # Not using threads_exec.jl for better isolation, reproducibility, and a
    # tighter timeout.
    script = "profile_spawnmany_exec.jl"
    cmd_base = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no $script`
    @testset for n in [20000, 200000, 2000000]
        cmd = ignorestatus(setenv(cmd_base, "NTASKS" => n; dir = @__DIR__))
        cmd = pipeline(cmd; stdout = stderr, stderr)
        proc = run(cmd; wait = false)
        done = Threads.Atomic{Bool}(false)
        timeout = false
        timer = Timer(100) do _
            timeout = true
            for sig in [Base.SIGTERM, Base.SIGHUP, Base.SIGKILL]
                for _ in 1:1000
                    kill(proc, sig)
                    if done[]
                        if sig != Base.SIGTERM
                            @warn "Terminating `$script` required signal $sig"
                        end
                        return
                    end
                    sleep(0.001)
                end
            end
        end
        try
            wait(proc)
        finally
            done[] = true
            close(timer)
        end
        if ( !success(proc) ) || ( timeout )
            @error "A \"spawn and wait lots of tasks\" test failed" n proc.exitcode proc.termsignal success(proc) timeout
        end
        if Sys.iswindows() || Sys.isapple()
            # Known failure: https://github.com/JuliaLang/julia/issues/43124
            @test_skip success(proc)
        else
            @test success(proc)
            @test !timeout
        end
    end
end

@testset "bad arguments to @threads" begin
    @test_throws ArgumentError @macroexpand(@threads 1 2) # wrong number of args
    @test_throws ArgumentError @macroexpand(@threads 1) # arg isn't an Expr
    @test_throws ArgumentError @macroexpand(@threads if true 1 end) # arg doesn't start with for
end

@testset "rand_ptls underflow" begin
    @test Base.Partr.cong(UInt32(0)) == 0
end

@testset "num_stack_mappings metric" begin
    @test @ccall(jl_get_num_stack_mappings()::Cint) >= 1
    # There must be at least two: one for the root test task and one for the async task:
    @test fetch(@async(@ccall(jl_get_num_stack_mappings()::Cint))) >= 2
end

@testset "Base.Threads docstrings" begin
    @test isempty(Docs.undocumented_names(Threads))
end

@testset "wait failed task" begin
    @testset "wait without throw keyword" begin
        t = Threads.@spawn error("Error")
        @test_throws TaskFailedException wait(t)
    end

    @testset "wait with throw=false" begin
        t = Threads.@spawn error("Error")
        wait(t; throw=false)
        @test istaskfailed(t)
    end
end

@testset "jl_*affinity" begin
    cpumasksize = @ccall uv_cpumask_size()::Cint
    if !Sys.iswindows() && cpumasksize > 0 # otherwise affinities are not supported on the platform (UV_ENOTSUP)
        mask = zeros(Cchar, cpumasksize);
        jl_getaffinity = (tid, mask, cpumasksize) -> ccall(:jl_getaffinity, Int32, (Int16, Ptr{Cchar}, Int32), tid, mask, cpumasksize)
        jl_setaffinity = (tid, mask, cpumasksize) -> ccall(:jl_setaffinity, Int32, (Int16, Ptr{Cchar}, Int32), tid, mask, cpumasksize)
        @test jl_getaffinity(1, mask, cpumasksize) == 0
        fill!(mask, 1)
        @test jl_setaffinity(1, mask, cpumasksize) == 0
    end
end
