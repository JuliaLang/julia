# This file is a part of Julia. License is MIT: https://julialang.org/license

# Cancellation tests that require multiple threads (run from cancellation.jl
# in a subprocess with -t2, so they are exercised even when the test driver
# itself is single-threaded).

using Test
using Base: cancel!, CancellationRequest

@assert Threads.nthreads() > 1

@noinline function find_collatz_counterexample_inner()
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n ÷ 2)
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

@testset "async interruption of checkless loops (reset_ctx)" begin
    t = Threads.@spawn find_collatz_counterexample2()
    sleep(0.5)
    cancel!(t)
    sleep(0.5)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
end

@testset "task abandonment wakes waiters" begin
    started = Base.Event()
    victim = Threads.@spawn begin
        notify(started)
        x = Ref(1.0)
        while true
            x[] = x[] * 1.0000001 + 0.1
        end
    end
    wait(started)
    watcher = @async wait(victim)
    sleep(0.5) # make sure the victim is actually spinning on its thread
    rescue = Task(() -> (while true; wait(); end))
    rescue.sticky = false
    Base.unsafe_abandon!(victim, rescue)
    @test timedwait(() -> istaskdone(victim), 5.0) == :ok
    @test victim.state === :abandoned
    @test istaskfailed(victim)
    # the watcher must be woken (abandoned tasks skip the regular
    # completion path)
    @test timedwait(() -> istaskdone(watcher), 5.0) == :ok
    @test_throws TaskFailedException fetch(watcher)
end
