# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using Base.Threads

include("print_process_affinity.jl") # import `uv_thread_getaffinity`

# whether `t` currently has an armed wait registration on `waitee`
function parked_on(t::Task, @nospecialize(waitee))
    w = @atomic t.waiting_on
    waitee isa Task && (waitee = waitee.donenotify)
    return w isa Base.WaitEntry && Base._find_slot(w, waitee) != 0
end

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
    # wait for the task to park in the queue (it may be spinning)
    @test timedwait(() -> parked_on(t1, lk.cond_wait), 10.0) == :ok
    @test !parked_on(t2, lk.cond_wait)
    @test istaskdone(t2)
    @test !fetch(t2)
    unlock(lk)
    @test parked_on(t1, lk.cond_wait)
    unlock(lk)
    @test timedwait(() -> istaskdone(t1), 10.0) == :ok
    @test !parked_on(t1, lk.cond_wait)
    @test fetch(t1)
end

# the wake-claim protocol: an interrupted waiter's stale registration is
# skipped by notify and collected lazily; a task whose interrupted wait
# left a stale registration behind can immediately park elsewhere
@testset "wait registration claim protocol" begin
    # A task can have only one armed wait registration at a time.
    let
        c1 = Threads.Condition()
        c2 = Threads.Condition()
        t = @task :done
        lock(c1)
        lock(c2)
        try
            w = Base.schedule_on_notify!(c1, t)
            @test_throws ConcurrencyViolationError Base.schedule_on_notify!(c2, t)
            @test (@atomic t.waiting_on) === w
            @test length(Base.waitqueue(c1)) == 1
            @test isempty(c2.waitq)
            @test notify(c1) == 1
            @test notify(c2) == 0
        finally
            unlock(c2)
            unlock(c1)
        end
        @test fetch(t) === :done
    end
    let
        target1 = @task nothing
        target2 = @task nothing
        waiter = @task nothing
        Base.schedule_on_notify!(target1, waiter)
        w = @atomic waiter.waiting_on
        @test_throws ConcurrencyViolationError Base.schedule_on_notify!(target2, waiter)
        @test (@atomic waiter.waiting_on) === w
        @test length(Base.waitqueue(target1)) == 1
        donenotify2 = target2.donenotify::Base.ThreadSynchronizer
        @test isempty(donenotify2.waitq)
        gotlock = trylock(donenotify2)
        # Release either our probe or the lock leaked by a regression.
        unlock(donenotify2)
        @test gotlock
        schedule(target1)
        schedule(target2)
        wait(target1)
        wait(target2)
        wait(waiter)
    end
    # Claimed entries do not count as waiters while awaiting lazy removal.
    let
        cond = Threads.Condition()
        stale = @task nothing
        live = @task nothing
        lock(cond)
        try
            w_stale = Base.schedule_on_notify!(cond, stale)
            @test !isempty(cond)
            @test Base.claim_wait(stale, w_stale)
            @test !isempty(cond.waitq)
            @test isempty(cond)
            w_live = Base.schedule_on_notify!(cond, live)
            @test !isempty(cond)
            @test Base.claim_wait(live, w_live)
            @test isempty(cond)
            notified = notify(cond)
            @test notified == 0
            @test isempty(cond.waitq)
        finally
            notify(cond)
            unlock(cond)
        end
    end
    # interrupting a parked waiter claims its wake and (when the waitee's
    # lock is free) eagerly unlinks its entry, so nothing stale remains and
    # its cached entry stays reusable
    let cond = Threads.Condition()
        t = @async begin
            lock(cond)
            try
                wait(cond)
            finally
                unlock(cond)
            end
        end
        yield() # let t park
        @test parked_on(t, cond)
        @test !isempty(cond.waitq)
        w = @atomic t.waiting_on
        schedule(t, ErrorException("interrupt"), error=true)
        # eager unlink: entry gone before t even runs
        @test !parked_on(t, cond)
        @test isempty(cond.waitq)
        @test Base._slot_owner(w::Base.WaitEntry, 1) === nothing # freed for reuse
        lock(cond)
        @test notify(cond) == 0
        unlock(cond)
        yield() # let t run its cleanup
        @test istaskfailed(t)
    end
    # if the interrupter cannot take the waitee's lock, the claimed entry is
    # left linked and collected lazily: a notify skips (and pops) it instead
    # of double-waking the task
    let cond = Threads.Condition()
        t = @async begin
            lock(cond)
            try
                wait(cond)
            finally
                unlock(cond)
            end
        end
        yield()
        @test parked_on(t, cond)
        # the lock must be held by a *different* task: a reentrant trylock by
        # the interrupter itself would succeed and unlink eagerly
        holder_go = Event()
        holder = @async (lock(cond); wait(holder_go); unlock(cond))
        yield() # holder takes cond's lock, parks on holder_go
        schedule(t, ErrorException("interrupt"), error=true)
        @test !parked_on(t, cond)
        @test !isempty(cond.waitq) # stale entry awaiting lazy collection
        notify(holder_go)
        wait(holder)
        lock(cond)
        @test notify(cond) == 0 # skipped (and popped), not woken twice
        @test isempty(cond.waitq)
        unlock(cond)
        yield() # let t finish its cleanup
        @test istaskfailed(t)
    end
    # a task interrupted while waiting on a Threads.Condition must be able to
    # park on the condition's (contended) lock during its cleanup, while its
    # stale registration is still linked: the busy cached entry forces a
    # fresh registration
    let cond = Threads.Condition()
        t = @async begin
            lock(cond)
            try
                wait(cond)
            finally
                unlock(cond)
            end
        end
        yield() # let t park on the condition
        @test parked_on(t, cond)
        w1 = @atomic t.waiting_on
        # a different task must hold cond's lock so the interrupter's trylock
        # unlink fails (leaving the corpse) and t's cleanup relock must park
        holder_go = Event()
        holder = @async (lock(cond); wait(holder_go); unlock(cond))
        yield() # holder takes cond's lock, parks on holder_go
        schedule(t, ErrorException("interrupt"), error=true)
        @test !isempty(cond.waitq) # corpse remains: trylock unlink failed
        # t resumes and its cleanup relock parks on the ReentrantLock (it may
        # spin-yield in slowlock first, so poll for the parked state)
        @test timedwait(() -> parked_on(t, cond.lock.cond_wait), 10.0) == :ok
        @test (@atomic t.waiting_on) !== w1 # cached entry busy -> fresh one
        @test !isempty(cond.waitq) # corpse still linked while t is reparked
        notify(holder_go)
        wait(holder)
        @test timedwait(() -> istaskdone(t), 10.0) == :ok
        @test istaskfailed(t)
        @test isempty(cond.waitq) # collected by t's cleanup
    end
    # A stale entry popped before interrupted-wait cleanup must not be reused
    # while its old notifier could still retain its identity for a wake claim.
    let cond = Threads.Condition()
        ready = Event()
        t = @async begin
            lock(cond)
            try
                notify(ready)
                wait(cond)
            finally
                unlock(cond)
            end
        end
        wait(ready)
        @test parked_on(t, cond)
        w = @atomic t.waiting_on
        @test w isa Base.WaitEntry
        lock(cond)
        try
            # interrupt from a helper task while we hold the lock, so the
            # interrupter's opportunistic trylock unlink fails and the corpse
            # stays linked for us (the "notifier") to pop
            wait(@async schedule(t, ErrorException("interrupt"), error=true))
            @test popfirst!(Base.waitqueue(cond)) === w
            @test Base._slot_owner(w::Base.WaitEntry, 1) === nothing
            for _ in 1:1000
                parked_on(t, cond.lock.cond_wait) && break
                yield()
            end
            @test parked_on(t, cond.lock.cond_wait)
            @test (@atomic t.waiting_on) !== w
        finally
            unlock(cond)
        end
        wait(t; throw=false)
        @test istaskfailed(t)
    end
    # a task waiting on another task can be interrupted, and the waited-on
    # task's completion then does not count the stale registration as handled
    let e = Event()
        target = @async wait(e)
        t = @async wait(target)
        yield()
        @test parked_on(t, target)
        schedule(t, ErrorException("interrupt"), error=true)
        yield()
        @test istaskfailed(t)
        notify(e)
        @test timedwait(() -> istaskdone(target), 10.0) == :ok
    end
    # scheduling (or yielding to) a task with an armed wait registration is a
    # concurrency violation: it must be woken through its registration's queue
    # (or by claiming, as `schedule(t, exc, error=true)` does)
    let cond = Threads.Condition()
        t = @async @lock cond wait(cond)
        @test timedwait(() -> parked_on(t, cond), 10.0) == :ok
        @test_throws ConcurrencyViolationError schedule(t)
        @test_throws ConcurrencyViolationError schedule(t, :wrong)
        @test_throws ConcurrencyViolationError yield(t)
        @lock cond notify(cond, :legit)
        @test fetch(t) === :legit
    end
end

# the cached WaitEntry makes the steady-state park/wake cycle allocation-free:
# the same entry object is recycled across parks, and nothing on the park or
# notify path allocates (in particular, the queue-identity witness must be a
# mutable object - storing an immutable waitee would re-box it every park).
# Shielded: a script session runs under the ambient ^C episode token, which
# would route every park through the cancellable arm - the cancellable
# steady state is measured separately below.
@testset "wait registration recycling is allocation-free" begin
 Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
    ping = Event(true)
    pong = Event(true)
    n = 500
    t = Threads.@spawn for i in 1:(n + 20)
        wait(ping)
        notify(pong)
    end
    driver() = (notify(ping); wait(pong))
    for i in 1:20 # warmup: compile and populate both tasks' cached entries
        driver()
    end
    w_spawned = t.cached_wait_entry
    w_driver = current_task().cached_wait_entry
    @test w_spawned isa Base.WaitEntry
    @test w_driver isa Base.WaitEntry
    measure() = @allocated for i in 1:n
        driver()
    end
    allocated = measure()
    wait(t)
    # the cached entries were recycled, not replaced
    @test t.cached_wait_entry === w_spawned
    @test current_task().cached_wait_entry === w_driver
    @test Base._slot_owner(w_driver::Base.WaitEntry, 1) === nothing # free between parks
    if Base.JLOptions().code_coverage == 0
        @test allocated == 0
    end
 end
end

# ... and the same holds for the cancellable park of a task governed by a
# token (the default inside a ^C episode, i.e. every interactive or script
# session): the cached cancel entry recycles with its sticky source
# registration, and the per-waitable phases stay unboxed (the park driver's
# tuple recursion; a dynamic tuple iteration would re-box the SourceWait
# every park)
@testset "cancellable park recycling is allocation-free" begin
 src = Base.CancellationTokenSource()
 Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
    ping = Event(true)
    pong = Event(true)
    n = 500
    t = Threads.@spawn for i in 1:(n + 20)
        wait(ping)
        notify(pong)
    end
    driver() = (notify(ping); wait(pong))
    for i in 1:20 # warmup: compile and populate both tasks' cached entries
        driver()
    end
    w_driver = current_task().cached_cancel_entry
    @test w_driver isa Base.WaitEntry
    measure() = @allocated for i in 1:n
        driver()
    end
    allocated = measure()
    wait(t)
    @test current_task().cached_cancel_entry === w_driver
    if Base.JLOptions().code_coverage == 0
        @test allocated == 0
    end
 end
end

@testset "unbuffered channel: interrupted taker is skipped" begin
    ch = Channel{Int}(0)
    t1 = @async take!(ch)
    yield() # t1 parks as a taker
    @test parked_on(t1, ch.cond_take)
    schedule(t1, ErrorException("interrupt"), error=true)
    t2 = @async take!(ch)
    yield() # t2 parks as a taker, behind t1's stale entry
    put!(ch, 42) # must skip t1's claimed entry and hand off to t2
    @test fetch(t2) == 42
    @test istaskfailed(t1)
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


let cmd1 = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no threads_exec.jl`,
    cmd2 = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no -e 'print(Threads.threadpoolsize(:default), ",", Threads.threadpoolsize(:interactive))'`
    for (test_nthreads, test_nthreadsi) in (
            (1, 0),
            (1, 1),
            (2, 0),
            (2, 1),
            (4, 0),
            (4, 0)) # try a couple times to trigger bad races
        new_env = copy(ENV)
        new_env["JULIA_NUM_THREADS"] = string(test_nthreads, ",", test_nthreadsi)
        run(pipeline(setenv(cmd1, new_env), stdout = stdout, stderr = stderr))
        threads_config = "$test_nthreads,$test_nthreadsi"
        # threads set via env var
        @test chomp(read(setenv(cmd2, new_env), String)) == threads_config
        # threads set via -t
        @test chomp(read(`$cmd2 -t$test_nthreads,$test_nthreadsi`, String)) == threads_config
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

function get_nthreads(options = ``; cpus = nothing, exclusive = false)
    cmd = `$(Base.julia_cmd()) --startup-file=no $(options)`
    cmd = `$cmd -e "print(Threads.threadpoolsize())"`
    cmd = addenv(cmd, "JULIA_EXCLUSIVE" => exclusive ? "1" : "0",
        "JULIA_NUM_THREADS" => "auto")
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
            @test get_nthreads(exclusive = true) ≥ 2
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

@testset "bad arguments to @threads" begin
    @test_throws ArgumentError @macroexpand(@threads 1 2) # wrong number of args
    @test_throws ArgumentError @macroexpand(@threads 1) # arg isn't an Expr
    @test_throws ArgumentError @macroexpand(@threads if true 1 end) # arg doesn't start with for
    # Test bad arguments for comprehensions
    @test_throws ArgumentError @macroexpand(@threads [i for i in 1:10] 2) # wrong number of args
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
    if cpumasksize > 0 # otherwise affinities are not supported on the platform (UV_ENOTSUP)
        jl_getaffinity = (tid, mask, cpumasksize) -> ccall(:jl_getaffinity, Int32, (Int16, Ptr{Cchar}, Int32), tid, mask, cpumasksize)
        jl_setaffinity = (tid, mask, cpumasksize) -> ccall(:jl_setaffinity, Int32, (Int16, Ptr{Cchar}, Int32), tid, mask, cpumasksize)
        mask = zeros(Cchar, cpumasksize)
        @test jl_getaffinity(0, mask, cpumasksize) == 0
        @test !all(iszero, mask)
        @test jl_setaffinity(0, mask, cpumasksize) == 0
    end
end

@testset "io_thread" begin
    function io_thread_test()
        # This test creates a thread that does IO and then blocks the main julia thread
        # This test hangs if you don't spawn an IO thread.
        # It hanging or not is technically a race but I haven't seen julia win that race yet.
        cmd = """
        Base.Experimental.make_io_thread()
        function callback()::Cvoid
            println("Running a command")
            run(`echo 42`)
            return
        end
        function call_on_thread(callback::Ptr{Nothing})
            tid = UInt[0]
            threadwork = @cfunction function(arg::Ptr{Cvoid})
                current_task().donenotify = Base.ThreadSynchronizer()
                Base.errormonitor(current_task())
                println("Calling Julia from thread")
                ccall(arg, Cvoid, ())
                nothing
            end Cvoid (Ptr{Cvoid},)
            err = @ccall uv_thread_create(tid::Ptr{UInt}, threadwork::Ptr{Cvoid}, callback::Ptr{Cvoid})::Cint
            err == 0 || Base.uv_error("uv_thread_create", err)
            gc_state = @ccall jl_gc_safe_enter()::Int8
            err = @ccall uv_thread_join(tid::Ptr{UInt})::Cint
            @ccall jl_gc_safe_leave(gc_state::Int8)::Cvoid
            err == 0 || Base.uv_error("uv_thread_join", err)
            return
        end
        function main()
            callback_ptr = @cfunction(callback, Cvoid, ())
            call_on_thread(callback_ptr)
            println("Done")
        end
        main()

        """
        proc = run(pipeline(`$(Base.julia_cmd()) -e $cmd`), wait=false)
        t = Timer(60) do t; kill(proc); end;
        @test success(proc)
        close(t)
        return true
    end
    @test io_thread_test()
end

# Make sure default number of BLAS threads respects CPU affinity: issue #55572.
@testset "LinearAlgebra number of default threads" begin
    if AFFINITY_SUPPORTED
        allowed_cpus = findall(uv_thread_getaffinity())
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -E 'using LinearAlgebra; BLAS.get_num_threads()'`,
                     # Remove all variables which could affect the default number of threads
                     "OPENBLAS_NUM_THREADS"=>nothing,
                     "GOTO_NUM_THREADS"=>nothing,
                     "OMP_NUM_THREADS"=>nothing)
        for n in 1:min(length(allowed_cpus), 8) # Cap to 8 to avoid too many tests on large systems
            @test readchomp(setcpuaffinity(cmd, allowed_cpus[1:n])) == string(max(1, n ÷ 2))
        end
    end
end

let once = OncePerProcess(() -> return [nothing])
    @test typeof(once) <: OncePerProcess{Vector{Nothing}}
    x = @inferred once()
    @test x === once()
    @atomic once.state = 0xff
    @test_throws ErrorException("invalid state for OncePerProcess") once()
    @test_throws ErrorException("OncePerProcess initializer failed previously") once()
    @atomic once.state = 0x01
    @test x === once()
end
let once1 = OncePerProcess(BigFloat), once2 = OncePerProcess{BigFloat}(BigFloat)
    # Using a type as a constructor should create a OncePerProcess with
    # Type{...} as its initializer (rather than DataType)
    @test typeof(once1) <: OncePerProcess{BigFloat,Type{BigFloat}}
    @test typeof(once2) <: OncePerProcess{BigFloat,Type{BigFloat}}
end
let once = OncePerProcess{Int}(() -> error("expected"))
    @test_throws ErrorException("expected") once()
    @test_throws ErrorException("OncePerProcess initializer failed previously") once()
end

let e = Base.Event(true),
    started = Channel{Int16}(Inf),
    finish = Channel{Nothing}(Inf),
    exiting = Channel{Nothing}(Inf),
    starttest2 = Base.Event(),
    once = OncePerThread() do
        push!(started, threadid())
        take!(finish)
        return [nothing]
    end
    alls = OncePerThread() do
        return [nothing]
    end
    @test typeof(once) <: OncePerThread{Vector{Nothing}}
    push!(finish, nothing)
    @test_throws ArgumentError once[0]
    x = @inferred once()
    @test_throws ArgumentError once[0]
    @test x === once() === fetch(@async once()) === once[threadid()]
    @test take!(started) == threadid()
    @test isempty(started)
    tids = zeros(UInt, 50)
    newthreads = zeros(Int16, length(tids))
    onces = Vector{Vector{Nothing}}(undef, length(tids))
    allonces = Vector{Vector{Vector{Nothing}}}(undef, length(tids))
    # allocate closure memory to last until all threads are started
    cls = [function cl()
            GC.gc(false) # stress test the GC-safepoint mechanics of jl_adopt_thread
            try
                newthreads[i] = threadid()
                local y = once()
                onces[i] = y
                @test x !== y === once() === once[threadid()]
                wait(starttest2)
                allonces[i] = Vector{Nothing}[alls[tid] for tid in newthreads]
            catch ex
                close(started, ErrorException("failed"))
                close(finish, ErrorException("failed"))
                @lock stderr Base.display_error(current_exceptions())
            end
            push!(exiting, nothing)
            GC.gc(false) # stress test the GC-safepoint mechanics of jl_delete_thread
            nothing
        end
    for i = 1:length(tids)]
    GC.@preserve cls begin # this memory must survive until each corresponding thread exits (waitallthreads / uv_thread_join)
        Base.preserve_handle(cls)
        for i = 1:length(tids)
            function threadcallclosure(tid::Ref{UInt}, cl::Ref{F}) where {F} # create sparam so we can reference the type of cl in the ccall type
                threadwork = @cfunction cl -> cl() Cvoid (Ref{F},) # create a cfunction that specializes on cl as an argument and calls it
                err = @ccall uv_thread_create(tid::Ptr{UInt}, threadwork::Ptr{Cvoid}, cl::Ref{F})::Cint # call that on a thread
                err == 0 || Base.uv_error("uv_thread_create", err)
                nothing
            end
            threadcallclosure(Ref(tids, i), Ref(cls, i))
        end
        @noinline function waitallthreads(tids, cls)
            for i = 1:length(tids)
                tid = Ref(tids, i)
                tidp = Base.unsafe_convert(Ptr{UInt}, tid)::Ptr{UInt}
                gc_state = @ccall jl_gc_safe_enter()::Int8
                GC.@preserve tid err = @ccall uv_thread_join(tidp::Ptr{UInt})::Cint
                @ccall jl_gc_safe_leave(gc_state::Int8)::Cvoid
                err == 0 || Base.uv_error("uv_thread_join", err)
            end
            Base.unpreserve_handle(cls)
        end
        try
            # let them finish in batches of 10
            for i = 1:length(tids) ÷ 10
                for i = 1:10
                    newid = take!(started)
                    @test newid != threadid()
                end
                for i = 1:10
                    push!(finish, nothing)
                end
            end
            @test isempty(started)
            # now run the second part of the test where they all try to access the other threads elements
            notify(starttest2)
        finally
            for _ = 1:length(tids)
                # run IO loop until all threads are close to exiting
                take!(exiting)
            end
            waitallthreads(tids, cls)
        end
    end
    @test isempty(started)
    @test isempty(finish)
    @test length(IdSet{eltype(onces)}(onces)) == length(onces) # make sure every object is unique
    allexpected = Vector{Nothing}[alls[tid] for tid in newthreads]
    @test length(IdSet{eltype(allexpected)}(allexpected)) == length(allexpected) # make sure every object is unique
    @test all(i -> allonces[i] !== allexpected && all(j -> allonces[i][j] === allexpected[j], eachindex(allexpected)), eachindex(allonces)) # make sure every thread saw the same elements
    @test_throws ArgumentError once[Threads.maxthreadid() + 1]
    @test_throws ArgumentError once[-1]

end
let once1 = OncePerThread(BigFloat), once2 = OncePerThread{BigFloat}(BigFloat)
    # Using a type as a constructor should create a OncePerThread with
    # Type{...} as its initializer (rather than DataType)
    @test typeof(once1) <: OncePerThread{BigFloat,Type{BigFloat}}
    @test typeof(once2) <: OncePerThread{BigFloat,Type{BigFloat}}
end
let once = OncePerThread{Int}(() -> error("expected"))
    @test_throws ErrorException("expected") once()
    @test_throws ErrorException("OncePerThread initializer failed previously") once()
end

let once = OncePerTask(() -> return [nothing])
    @test typeof(once) <: OncePerTask{Vector{Nothing}}
    x = @inferred once()
    @test x === once() !== fetch(@async once())
    delete!(task_local_storage(), once)
    @test x !== once() === once()
end
let once1 = OncePerTask(BigFloat), once2 = OncePerTask{BigFloat}(BigFloat)
    # Using a type as a constructor should create a OncePerTask with
    # Type{...} as its initializer (rather than DataType)
    @test typeof(once1) <: OncePerTask{BigFloat,Type{BigFloat}}
    @test typeof(once2) <: OncePerTask{BigFloat,Type{BigFloat}}
end
let once = OncePerTask{Int}(() -> error("expected"))
    @test_throws ErrorException("expected") once()
    @test_throws ErrorException("expected") once()
end

# Exercise `save_stack`/`restore_stack` under GC pressure: a missing write-barrier
# when storing the freshly-allocated copy-stack buffer into `lastt->ctx.stkbuf`
# let the GC collect the buffer while the (old) task still referenced it.
let code = """
    function f(x)
        x in (0, 1) && return x
        a = Threads.@spawn f(x - 2)
        b = Threads.@spawn f(x - 1)
        return fetch(a) + fetch(b)
    end
    print(@sync f(25))
    """
    cmd = addenv(`$(Base.julia_cmd()) --depwarn=error --startup-file=no -t4 -e $code`,
                 "JULIA_COPY_STACKS" => "1")
    # JULIA_COPY_STACKS is broken on Windows (#35147)
    @test read(cmd, String) == "75025" skip=Sys.iswindows()
end

# Regression test for the macOS mach-exception safepoint path: a thread resumed
# from a safepoint used to re-execute the faulting poll load, and under
# back-to-back collections could fault again every time without making
# progress, starving the main thread in the hijack/restore dance
# (`jl_mach_restore_trigger`). The child keeps collections back-to-back while
# the main task must keep crossing safepoint polls to spawn and reap workers;
# on affected builds it hangs and the timer fires.
@testset "no starvation at safepoint polls under back-to-back collections" begin
    code = """
    function work(n)
        s = 0.0
        for i in 1:n
            s += sqrt(i)
        end
        return s
    end
    function main(iters)
        done = Threads.Atomic{Bool}(false)
        gcspam = Threads.@spawn while !done[]
            GC.gc(false)
            yield()
        end
        nworkers = max(2, Threads.nthreads() - 1)
        for i in 1:iters
            tasks = [Threads.@spawn work(10_000) for _ in 1:nworkers]
            foreach(wait, tasks)
        end
        done[] = true
        wait(gcspam)
    end
    main(300)
    """
    cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no -t8 -e $code`
    cmd = ignorestatus(setenv(cmd; dir = @__DIR__))
    cmd = pipeline(cmd; stdout = stderr, stderr)
    proc = run(cmd; wait = false)
    # a passing run takes a few seconds; the timeout only fires on starvation
    timeout = false
    timer = Timer(120) do t
        timeout = true
        # a starved process ignores SIGTERM-level requests; go straight to the
        # backtrace-dumping and then killing signals
        for sig in (Base.SIGQUIT, Base.SIGKILL)
            for _ in 1:3
                process_running(proc) || return
                kill(proc, sig)
                sleep(1)
            end
        end
    end
    try
        wait(proc)
    finally
        close(timer)
    end
    if !success(proc) || timeout
        @error "The safepoint poll starvation test failed" proc.exitcode proc.termsignal timeout
    end
    @test success(proc)
    @test !timeout
end
