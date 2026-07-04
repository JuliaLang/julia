# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: cancel!, CancellationRequest, CANCEL_REQUEST_SAFE, CANCEL_REQUEST_ACK,
    CANCEL_REQUEST_ABANDON_EXTERNAL, CANCEL_REQUEST_ABANDON_ALL

const collatz_code = quote
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n ÷ 2)
    function find_collatz_counterexample()
        i = 1
        while true
            j = i
            while true
                Base.@cancel_check
                j = collatz(j)
                j == 1 && break
                j == i && error("$j is a collatz counterexample")
            end
            i += 1
        end
    end
    @noinline function find_collatz_counterexample_inner()
        i = 1
        while true
            j = i
            while true
                j = collatz(j)
                j == 1 && break
                j == i && return j
            end
            i += 1
        end
    end
    function find_collatz_counterexample2()
        # A single cancellation point at function entry; interrupting the inner
        # (checkless) loop requires the reset_ctx mechanism.
        Base.@cancel_check
        return find_collatz_counterexample_inner()
    end
end
eval(collatz_code)

# wait a little, so cancellation targets are (most likely) started and parked
spin(n=4) = for _ in 1:n; yield(); end

@testset "cancellation of waiting tasks" begin
    # Cancellation of a task that was never started
    t = @task nothing
    @test cancel!(t)
    @test t.state === :cancelled
    @test istaskfailed(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # Scheduling a cancelled task transitions it to failed, without running it
    schedule(t)
    @test t.state === :failed

    # Cancellation of `sleep`
    t = @async sleep(1000)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest

    # Cancellation of a task blocked on a Channel
    c = Channel{Int}(0)
    t = @async take!(c)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # The channel remains usable
    t2 = @async take!(c)
    put!(c, 7)
    @test fetch(t2) == 7

    # Cancellation of a task waiting on another task propagates
    t_in = @async sleep(1000)
    t = @async wait(t_in)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test istaskdone(t_in) && istaskfailed(t_in)
end

@testset "cancellation of lock and condition waits" begin
    # Task blocked in lock(::ReentrantLock)
    lk = ReentrantLock()
    lock(lk)
    t = @async lock(lk)
    spin()
    # let it spin through the fast path and park
    @test timedwait(() -> t.queue !== nothing, 5.0) == :ok
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # the lock remains functional
    unlock(lk)
    @test trylock(lk)
    unlock(lk)
    t2 = @async (lock(lk); unlock(lk); true)
    @test fetch(t2)

    # Task blocked in put! on a full channel
    c = Channel{Int}(1)
    put!(c, 1)
    t = @async put!(c, 2)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @test take!(c) == 1
    put!(c, 3) # channel remains functional
    @test take!(c) == 3

    # Task blocked in wait(::Threads.Condition)
    cond = Threads.Condition()
    t = @async @lock cond wait(cond)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @lock cond notify(cond) # still functional (no waiters)

    # Task blocked in wait(::Base.Process); the process itself keeps running
    p = run(`sleep 1000`; wait=false)
    t = @async wait(p)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @test process_running(p)
    kill(p); wait(p)

    # Task blocked in waitany
    t1 = @async sleep(1000)
    t2 = @async sleep(1000)
    t = @async waitany([t1, t2])
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # the awaited tasks remain unaffected and cancellable
    cancel!(t1); cancel!(t2)
    @test_throws TaskFailedException wait(t1)
    @test_throws TaskFailedException wait(t2)
end

@testset "cancellation of stdlib waits (Sockets, FileWatching, Semaphore)" begin
    # Base.Semaphore: a cancelled acquire does not leak a permit
    sem = Base.Semaphore(1)
    Base.acquire(sem)
    t = @async Base.acquire(sem)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    Base.release(sem)
    Base.acquire(sem) # the permit is still available
    Base.release(sem)

    # Sockets.accept
    Sockets = Base.require(Base.PkgId(Base.UUID("6462fe0b-24de-5631-8697-dd941f90decc"), "Sockets"))
    port, server = Sockets.listenany(Sockets.localhost, 0)
    t = @async Sockets.accept(server)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # the server keeps accepting afterwards
    t2 = @async Sockets.accept(server)
    sock = Sockets.connect(Sockets.localhost, port)
    @test fetch(t2) isa Sockets.TCPSocket
    close(sock); close(server)

    # FileWatching: fd polling and file watching
    FileWatching = Base.require(Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"))
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    fd = Base._fd(p.out)
    t = @async FileWatching.wait(fd; readable=true) # nothing is ever written
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    close(p)

    path = tempname()
    touch(path)
    t = @async FileWatching.watch_file(path, 100.0) # the file never changes
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    rm(path)

    # Distributed: a (local) never-fulfilled Future wait; remote waits go
    # through the same channel-based wait path on the caller side
    Distributed = Base.require(Base.PkgId(Base.UUID("8ba89e20-285c-5b6f-9357-94700520ee1b"), "Distributed"))
    fut = Distributed.Future()
    t = @async fetch(fut)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    put!(fut, 1) # the future remains usable
    @test fetch(fut) == 1
end

@testset "cancellation of computing tasks" begin
    # Polling cancellation via @cancel_check
    t = Threads.@spawn find_collatz_counterexample()
    sleep(0.2)
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
end

@testset "structured cancellation of @sync" begin
    t = @async begin
        @sync begin
            @async sleep(1000)
            @async sleep(1000)
        end
    end
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CompositeException
    @test length(t.result.exceptions) == 2
end

@testset "unfriendly cancellation modes" begin
    # Acknowledgment preserves the request's severity.
    seen = Ref{Any}(nothing)
    t = @async try
        sleep(1000)
    catch e
        seen[] = (e, Base.acknowledged_cancellation_severity(), Base.abandoning_external_waits())
        rethrow()
    end
    spin()
    cancel!(t, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    e, sev, abandoning = seen[]
    @test e === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test sev === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test abandoning

    # SAFE acknowledgments report SAFE severity and permit external waits.
    seen2 = Ref{Any}(nothing)
    t2 = @async try
        sleep(1000)
    catch
        seen2[] = (Base.acknowledged_cancellation_severity(), Base.abandoning_external_waits())
        rethrow()
    end
    spin()
    cancel!(t2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test seen2[] === (CANCEL_REQUEST_SAFE, false)

    # ABANDON_ALL freezes a parked task immediately: no unwind, no cleanup.
    cleanup_ran = Ref(false)
    t3 = @async try
        sleep(1000)
    finally
        cleanup_ran[] = true
    end
    spin()
    @test cancel!(t3, CANCEL_REQUEST_ABANDON_ALL)
    @test istaskdone(t3)
    @test t3.state === :abandoned
    @test istaskfailed(t3)
    @test !cleanup_ran[]
    @test_throws TaskFailedException wait(t3)

    # ABANDON_ALL of a never-started task completes it too.
    t4 = @task nothing
    @test cancel!(t4, CANCEL_REQUEST_ABANDON_ALL)
    @test istaskdone(t4)

    # ABANDON_EXTERNAL interrupts a blocked stream write without waiting for
    # the write's cancellation to complete.
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        big = zeros(UInt8, 200_000_000)
        tw = @async write(p, big)
        spin()
        cancel!(tw, CANCEL_REQUEST_ABANDON_EXTERNAL)
        @test timedwait(() -> istaskdone(tw), 10.0) == :ok
        @test istaskfailed(tw)
        @test tw.result === CANCEL_REQUEST_ABANDON_EXTERNAL
    finally
        close(p)
    end

    # Episode classification for the ^C escalation ladder.
    @test Base.sigint_active_severity(nothing) === nothing
    @test Base.sigint_active_severity(UInt8(0x00)) === nothing # fresh C-side ^C marker
    @test Base.sigint_active_severity(CANCEL_REQUEST_SAFE) === CANCEL_REQUEST_SAFE
    @test Base.sigint_active_severity(CancellationRequest(0x80)) === CANCEL_REQUEST_SAFE
    @test Base.sigint_active_severity(CANCEL_REQUEST_ABANDON_EXTERNAL) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test Base.sigint_active_severity(CancellationRequest(0x83)) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test Base.sigint_active_severity(CANCEL_REQUEST_ABANDON_ALL) === CANCEL_REQUEST_ABANDON_ALL
    @test Base.sigint_active_severity(CancellationRequest(0x84)) === CANCEL_REQUEST_ABANDON_ALL
end

@testset "unfriendly cancellation of Experimental.@sync" begin
    # ABANDON_EXTERNAL propagates to the children, which are internal and
    # therefore awaited.
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t = @async begin
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(t, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test_throws TaskFailedException wait(t)
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])

    # ABANDON_ALL freezes the parent, which never even begins propagation -
    # the children must be frozen by the caller's own escalation if desired.
    t3 = Ref{Task}()
    tp = @async begin
        Base.Experimental.@sync begin
            t3[] = @async sleep(1000)
        end
    end
    spin()
    @test cancel!(tp, CANCEL_REQUEST_ABANDON_ALL)
    @test tp.state === :abandoned
    @test cancel!(t3[], CANCEL_REQUEST_ABANDON_ALL)
    @test t3[].state === :abandoned
end

@testset "structured cancellation of Experimental.@sync" begin
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t = @async begin
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # cancellation propagated to the children
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])
end

@testset "cancellation of blocked stream writes" begin
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        # A write far exceeding the OS pipe buffer blocks until cancelled
        big = zeros(UInt8, 200_000_000)
        t = @async write(p, big)
        sleep(0.5)
        @test t.queue === p.in
        cancel!(t)
        @test_throws TaskFailedException wait(t)
        @test t.result isa CancellationRequest
    finally
        close(p)
    end
end

@testset "cancellation of closewrite (shutdown) waits" begin
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        # A blocked write keeps the shutdown request (which queues behind it)
        # from completing; the closewrite wait must still be interruptible.
        big = zeros(UInt8, 200_000_000)
        tw = @async write(p, big)
        sleep(0.5)
        @test tw.queue === p.in
        ts = @async closewrite(p.in)
        @test timedwait(() -> ts.queue === p.in, 5.0) == :ok
        cancel!(ts)
        @test_throws TaskFailedException wait(ts)
        @test ts.result isa CancellationRequest
        cancel!(tw)
        @test_throws TaskFailedException wait(tw)
    finally
        close(p)
    end
end

@testset "acknowledged requests do not re-trigger" begin
    t = @async begin
        try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
        end
        # The request was delivered (and acknowledged); this task can still
        # perform IO and sleep.
        sleep(0.01)
        Base.conform_cancellation_request(@atomic :acquire current_task().cancellation_request) === CANCEL_REQUEST_ACK
    end
    spin()
    cancel!(t)
    @test fetch(t)
end

# Tests that need real thread parallelism (asynchronous interruption through
# the reset_ctx mechanism, task abandonment) always run with 2 threads,
# regardless of how the test driver was started.
@testset "threaded cancellation (subprocess with -t2)" begin
    cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no --threads=2 $(joinpath(@__DIR__, "cancellation_exec.jl"))`
    p = run(pipeline(cmd, stdout=stdout, stderr=stderr), wait=false)
    # A cancellation-delivery regression can wedge the child completely (a
    # surviving spin loop blocks GC's stop-the-world, which also blocks all
    # signal processing), in which case not even SIGTERM gets through.
    # SIGKILL it rather than hanging the test suite.
    if timedwait(() -> process_exited(p), 240.0) !== :ok
        kill(p, Base.SIGKILL)
    end
    wait(p)
    @test success(p)
end

@testset "^C" begin
    function run_with_sigint(code::String, delays; forcekill::Bool=false)
        out = Pipe()
        p = run(pipeline(`$(Base.julia_cmd()) --startup-file=no -e $code`, stdout=out, stderr=out), wait=false)
        close(out.in)
        reader = @async read(out, String)
        killer = @async begin
            for d in delays
                sleep(d)
                process_running(p) && kill(p, Base.SIGINT)
            end
            if forcekill
                # e.g. an abandoned script has nothing left to run and idles
                sleep(3)
                process_running(p) && kill(p, Base.SIGKILL)
            end
        end
        wait(p)
        wait(killer)
        return fetch(reader), p
    end

    # Catching ^C in a script
    output, p = run_with_sigint("""
        try
            sleep(100)
            println("FAIL: not cancelled")
        catch e
            println("caught: ", typeof(e))
        end
        println("continued")
    """, [1.0])
    @test occursin("caught: Base.CancellationRequest", output)
    @test occursin("continued", output)
    @test p.exitcode == 0

    # Uncaught ^C produces a proper error report
    output, p = run_with_sigint("sleep(100)", [1.0])
    @test occursin("CancellationRequest: Safe Cancellation (CANCEL_REQUEST_SAFE)", output)
    @test p.exitcode == 1

    # ^C propagates through @sync, cancelling compute-bound and sleeping tasks
    output, p = run_with_sigint("""
        $(string(collatz_code))
        try
            @sync begin
                @async sleep(10000)
                @async find_collatz_counterexample()
            end
        catch e
            println(typeof(e))
        end
        """, [1.5])
    @test occursin("CompositeException", output)
    @test p.exitcode == 0

    # Escalation: an unresponsive process warns after 1s, and a second ^C
    # abandons the stuck task; with the interactive evaluator gone, the
    # process exits like an uncaught ^C
    output, p = run_with_sigint("""
        x = Ref(1.0)
        while true
            x[] = x[] * 1.0000001 + 0.1
        end
        """, [1.0, 2.5]; forcekill=true)
    @test occursin("failed to acknowledge SIGINT", output)
    @test occursin("Abandoning current task", output)
    @test p.exitcode == 128 + 2
end

if Sys.isunix()
    @testset "^C escalation ladder in the REPL (pty), $(isempty(tflags) ? "default threads" : join(tflags, " "))" for tflags in ([], ["-t2"])
        isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
        pts, ptm = Main.FakePTYs.open_fake_pty()
        env = copy(ENV)
        env["TERM"] = "dumb"
        env["JULIA_HISTORY"] = tempname()
        # Cover both thread topologies: the default session (a single default
        # thread, which the rescued backend monopolizes in episode 2 - only
        # the interactive-pool listener can escalate) and -t2 (the victim and
        # the listener compete inside a wider default pool).
        p = run(detach(setenv(`$(Base.julia_cmd()) -i -q --startup-file=no --color=no $tflags`, env)),
                pts, pts, pts; wait=false)
        ccall(:close, Cint, (Cint,), pts) # only the child owns the pts now

        transcript_lock = ReentrantLock()
        transcript = UInt8[]
        reader = @async try
            while true
                chunk = readavailable(ptm)
                isempty(chunk) && break
                @lock transcript_lock append!(transcript, chunk)
            end
        catch # pty closes when the child exits
        end
        cursor = Ref(1)
        snapshot() = @lock transcript_lock String(copy(transcript))
        function expect(needle::String; timeout::Real=30.0)
            status = timedwait(timeout; pollint=0.05) do
                idx = findnext(needle, snapshot(), cursor[])
                idx === nothing && return false
                cursor[] = last(idx) + 1
                return true
            end
            if status !== :ok
                @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
            end
            @test status == :ok
        end
        sendline(s) = write(ptm, s * "\n")

        expect("julia> ")
        # A task that acknowledges SAFE cancellation but hangs in its cleanup:
        # walks the full escalation ladder with a guided message per rung.
        # Two episodes: the second exercises the ladder on a *rescued*
        # session (fresh backend task, abandoned root task).
        for episode in 1:2
            sendline("println(\"EVAL-START\"); try; sleep(1000); finally; x = Ref(1.0); while x[] > 0; x[] = x[] * 1.0000001 + 0.1; end; end")
            expect("EVAL-START") # the evaluation is running (robust under load)
            sleep(0.5)           # ... and parked in sleep(1000)
            kill(p, Base.SIGINT) # press 1: SAFE, delivered silently
            expect("Press ^C again to also stop waiting for external resources"; timeout=6.0)
            if episode == 1
                # On-demand thread backtraces during the episode (^T sends
                # SIGINFO where the tty supports it - BSD/mac; SIGUSR1 elsewhere)
                kill(p, Sys.isbsd() ? Base.SIGINFO : Base.SIGUSR1)
                expect("signal ("; timeout=10.0) # the backtrace dump header
            end
            kill(p, Base.SIGINT) # press 2: ABANDON_EXTERNAL
            expect("No longer waiting for external resources")
            expect("Press ^C again to forcibly abandon"; timeout=6.0)
            kill(p, Base.SIGINT) # press 3: ABANDON_ALL freezes the task
            expect("Abandoning the current task")
            expect("CancellationRequest")
            expect("julia> ")
            # the rescued session works
            sendline("$episode + $episode")
            expect(string(2episode))
            expect("julia> ")
        end
        # ... and the session exits cleanly on ^D
        write(ptm, "\x04") # ^D (EOF)
        @test timedwait(() -> process_exited(p), 15.0) == :ok
        @test success(p)
        close(ptm)
        wait(reader)
    end

    @testset "^C in the REPL (pty)" begin
        isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
        pts, ptm = Main.FakePTYs.open_fake_pty()

        # Interactive julia on the pty; drive it like a user pressing ^C.
        env = copy(ENV)
        env["TERM"] = "dumb"
        env["JULIA_HISTORY"] = tempname()
        p = run(detach(setenv(`$(Base.julia_cmd()) -i -q --startup-file=no --color=no`, env)),
                pts, pts, pts; wait=false)
        ccall(:close, Cint, (Cint,), pts) # only the child owns the pts now

        transcript_lock = ReentrantLock()
        transcript = UInt8[]
        reader = @async try
            while true
                chunk = readavailable(ptm)
                isempty(chunk) && break
                @lock transcript_lock append!(transcript, chunk)
            end
        catch # pty closes when the child exits
        end
        cursor = Ref(1)
        snapshot() = @lock transcript_lock String(copy(transcript))
        function expect(needle::String; timeout::Real=30.0)
            status = timedwait(timeout; pollint=0.05) do
                idx = findnext(needle, snapshot(), cursor[])
                idx === nothing && return false
                cursor[] = last(idx) + 1
                return true
            end
            if status !== :ok
                @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
            end
            @test status == :ok
        end
        sendline(s) = write(ptm, s * "\n")

        expect("julia> ")

        # ^C interrupts a sleeping REPL evaluation and reports it
        sendline("println(\"EVAL-1\"); sleep(1000)")
        expect("EVAL-1") # the evaluation is running (robust under load)
        sleep(0.5)       # ... and parked in sleep(1000)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")

        # the REPL evaluates normally afterwards
        sendline("6 * 7")
        expect("42")
        expect("julia> ")

        # a spinning evaluation triggers the escalation warning; the second
        # ^C abandons it and the REPL is rescued with a fresh backend
        sendline("println(\"EVAL-2\"); xr = Ref(1.0); while true; xr[] = xr[] * 1.0000001 + 0.1; end")
        expect("EVAL-2") # the evaluation is running (robust under load)
        sleep(0.5)       # ... and spinning
        kill(p, Base.SIGINT)
        expect("failed to acknowledge SIGINT"; timeout=15.0)
        kill(p, Base.SIGINT)
        expect("Abandoning current task")
        expect("julia> ")

        # the rescued REPL still evaluates
        sendline("3 + 4")
        expect("7")
        expect("julia> ")

        # and ^C still works after the rescue
        sendline("println(\"EVAL-3\"); sleep(1000)")
        expect("EVAL-3")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")

        sendline("exit()")
        @test success(p)
        close(ptm)
        wait(reader)
    end
end
