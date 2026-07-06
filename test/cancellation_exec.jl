# This file is a part of Julia. License is MIT: https://julialang.org/license

# Cancellation tests that require multiple threads (run from cancellation.jl
# in a subprocess with -t2, so they are exercised even when the test driver
# itself is single-threaded).

using Test
using Libdl
using Base: cancel!, CancellationRequest, CancellationToken, CancellationTokenSource,
    CANCEL_TOKEN
using Base.ScopedValues: with

@assert Threads.nthreads() > 1

# Start `f` as a task governed by a fresh cancellation source (non-sticky,
# explicitly on the default pool - a compute-bound victim must not land on
# the interactive/io thread).
function cancellable_spawn(f)
    src = CancellationTokenSource()
    t = with(() -> Threads.@spawn(f()), CANCEL_TOKEN => CancellationToken(src))
    return t, src
end

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
    t, src = cancellable_spawn(find_collatz_counterexample2)
    sleep(0.5)
    cancel!(src)
    sleep(0.5)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest

    # The same, arriving through the second parent of a linked source: the
    # running-compute scan walks the parent DAG (not just a chain) to find
    # the bound task.
    p1 = CancellationTokenSource()
    p2 = CancellationTokenSource()
    lsrc = CancellationTokenSource(CancellationToken(p1), CancellationToken(p2))
    tl = with(CANCEL_TOKEN => CancellationToken(lsrc)) do
        Threads.@spawn find_collatz_counterexample2()
    end
    sleep(0.5)
    cancel!(p2)
    sleep(0.5)
    @test_throws TaskFailedException wait(tl)
    @test tl.result isa CancellationRequest
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

@testset "ABANDON_ALL freezes a running task" begin
    started = Base.Event()
    # The victim has no cancellation points in its loop: only freezing can
    # stop it promptly. The task-start check published its binding to the
    # scope's token, which is how the cancellation walk finds it.
    victim, src = cancellable_spawn() do
        notify(started)
        x = Ref(1.0)
        while x[] > 0
            x[] = x[] * 1.0000001 + 0.1
        end
    end
    wait(started)
    sleep(0.5) # make sure it is spinning on its thread
    @test cancel!(src, Base.CANCEL_REQUEST_ABANDON_ALL)
    @test timedwait(() -> istaskdone(victim), 10.0) == :ok
    @test victim.state === :abandoned
    @test istaskfailed(victim)
end

@testset "asynchronous cancellation hooks" begin
    # The handler runs on the cancelling thread, with the registered state and
    # the cancelled task as arguments.
    seen = Channel{Any}(4)
    entered = Base.Event()
    t, src = cancellable_spawn(() -> Base.with_cancellation_hook(
        () -> (notify(entered); sleep(1000)),
        (st, tsk) -> put!(seen, (st, tsk)), :mystate))
    wait(entered) # `f` runs only after the hook is registered
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    st, tsk = take!(seen)
    @test st === :mystate
    @test tsk === t

    # The hook is deregistered when the protected region exits.
    fired = Threads.Atomic{Int}(0)
    entered2 = Base.Event()
    t2, src2 = cancellable_spawn() do
        Base.with_cancellation_hook(() -> nothing, (st, tsk) -> Threads.atomic_add!(fired, 1), nothing)
        notify(entered2)
        sleep(1000)
    end
    wait(entered2)
    cancel!(src2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test fired[] == 0

    # A cancellation already pending at registration is thrown before `f` runs.
    t3 = Threads.@spawn begin
        src3 = CancellationTokenSource()
        cancel!(src3)
        ran = false
        threw = try
            with(CANCEL_TOKEN => CancellationToken(src3)) do
                Base.with_cancellation_hook(() -> (ran = true), (st, tsk) -> nothing, nothing)
            end
            false
        catch e
            e isa CancellationRequest || rethrow()
            true
        end
        (ran, threw)
    end
    @test fetch(t3) === (false, true)
end

mutable struct BlasCancelScope
    @atomic slot::Ptr{Csize_t}
end

@testset "BLAS cancellation via cancellation hooks" begin
    # Requires an OpenBLAS with the cancellation patch (source build); the
    # BinaryBuilder library does not have it, so skip gracefully.
    blas = Libdl.dlopen_e("libopenblas64_")
    tok_f = blas == C_NULL ? C_NULL : Libdl.dlsym_e(blas, :openblas_cancel_token)
    if tok_f == C_NULL
        @warn "patched OpenBLAS not available; skipping BLAS cancellation tests"
    else
        cancel_f = Libdl.dlsym(blas, :openblas_cancel)
        dgemm_f = Libdl.dlsym(blas, :dgemm_64_)

        function gemm!(C::Matrix{Float64}, A::Matrix{Float64}, B::Matrix{Float64})
            m = Int64(size(A, 1)); k = Int64(size(A, 2)); n = Int64(size(B, 2))
            ccall(dgemm_f, Cvoid,
                  (Ref{UInt8}, Ref{UInt8}, Ref{Int64}, Ref{Int64}, Ref{Int64},
                   Ref{Float64}, Ptr{Float64}, Ref{Int64}, Ptr{Float64}, Ref{Int64},
                   Ref{Float64}, Ptr{Float64}, Ref{Int64}, Clong, Clong),
                  UInt8('N'), UInt8('N'), m, n, k, 1.0, A, m, B, k, 0.0, C, m, 1, 1)
            return C
        end

        # correctness sanity + timing baseline
        n = 12000
        A = rand(n, n); B = rand(n, n); C = zeros(n, n)
        gemm!(C, A, B) # warm up the thread pool
        tbase = @elapsed gemm!(C, A, B)

        # The canceller-side handler: load the issuing thread's current
        # generation, re-check the protected call is still in flight, and
        # cancel exactly that generation (stale requests are neutralized by
        # the library's compare-exchange).
        blas_cancel_handler = function (scope::BlasCancelScope, @nospecialize(task))
            slot = @atomic :acquire scope.slot
            slot == C_NULL && return
            loaded = unsafe_load(slot)
            (@atomic :acquire scope.slot) == slot || return
            ccall(cancel_f, Cvoid, (Ptr{Csize_t}, Csize_t), slot, loaded)
            nothing
        end

        # A bystander BLAS operation with no binding must be unaffected.
        nb = 4000
        A2 = rand(nb, nb); B2 = rand(nb, nb); C2 = zeros(nb, nb)
        bystander_started = Base.Event()
        bystander = Threads.@spawn (notify(bystander_started); gemm!(C2, A2, B2))

        started = Base.Event()
        t, tsrc = cancellable_spawn() do
            scope = BlasCancelScope(C_NULL)
            Base.with_cancellation_hook(blas_cancel_handler, scope) do
                # Fetch the slot on the executing OS thread; no yields between
                # here and the ccall, so it is the slot the BLAS driver's
                # generation bump targets.
                slot = ccall(tok_f, Ptr{Csize_t}, ())
                @atomic :release scope.slot = slot
                try
                    notify(started)
                    gemm!(C, A, B)
                finally
                    @atomic :release scope.slot = C_NULL
                end
            end
        end
        wait(bystander_started)
        wait(started)
        telapsed = @elapsed begin
            cancel!(tsrc)
            @test timedwait(() -> istaskdone(t), 60.0) == :ok
        end
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
        # The gemm was abandoned early (block-granularity latency; give slack
        # for scheduling noise)
        @test telapsed < tbase * 0.75

        # The bystander completed unharmed with a correct result.
        wait(bystander)
        r, c = rand(1:nb), rand(1:nb)
        @test isapprox(C2[r, c], @views sum(A2[r, :] .* B2[:, c]); rtol=1e-8)

        # The library stays healthy for subsequent (uncancelled) use - no
        # reset needed: the next operation advances past the dead generation.
        a = rand(16, 16); b = rand(16, 16); c2 = zeros(16, 16)
        gemm!(c2, a, b)
        @test c2 ≈ [sum(a[i, l] * b[l, j] for l in 1:16) for i in 1:16, j in 1:16]
    end
end

@testset "cancel storm" begin
    # Hammer cancellation against tasks in every wait state, from a tight
    # loop with concurrent GC pressure. This is a regression test for the
    # class of races that only show up under load (e.g. an uninitialized or
    # stale reset_ctx being consumed by the cancellation signal).
    function polling_loop()
        x = 1
        while true
            Base.@cancel_check
            x += 1
        end
    end
    function checkless_loop()
        Base.@cancel_check
        # No cancellation points in the loop below: only asynchronous
        # delivery (reset_ctx) can interrupt it. Bound the iteration count:
        # a safepoint-free spin loop blocks GC's stop-the-world, and if GC
        # wins the race against cancel! (the canceller parks at a safepoint
        # before it manages to deliver), an unbounded loop would wedge the
        # process. With the bound, that race degrades to a transient stall.
        x = Ref(1.0)
        i = 0
        while x[] > 0 && i < 2_000_000_000
            x[] = x[] * 1.0000001 + 0.1
            i += 1
        end
    end
    stop = Ref(false)
    garbage = @async begin # allocation/GC pressure
        while !stop[]
            zeros(UInt8, 1 << 20)
            yield()
        end
    end
    held = ReentrantLock()
    lock(held)
    function make_victims()
        src = CancellationTokenSource()
        late = Ref{Task}()
        ts = with(CANCEL_TOKEN => CancellationToken(src)) do
            late[] = Task(() -> nothing) # scheduled only after the cancellation
            Task[
                @async(sleep(1000)),            # timer wait
                @async(take!(Channel{Int}(0))), # condition wait
                @async(lock(held)),             # lock wait (the test holds `held`)
                Threads.@spawn(polling_loop()),
                Threads.@spawn(checkless_loop()),
                late[],
            ]
        end
        return ts, src, late[]
    end
    deadline = time() + 10
    rounds = 0
    failures = 0
    while time() < deadline
        ts, src, late = make_victims()
        # vary the delivery window: sometimes cancel immediately (tasks not
        # yet started/parked), sometimes after they have parked/started
        # spinning
        rounds % 2 == 0 && sleep(0.05)
        cancel!(src)
        cancel!(src) # double-cancellation must be harmless
        Base.redeliver!(src) # explicit redelivery must be harmless too
        # a task scheduled into an already-cancelled scope dies at start
        schedule(late)
        for t in ts
            if timedwait(() -> istaskdone(t), 20.0) !== :ok
                failures += 1
                @error "cancelled task failed to complete" t t.state
            end
        end
        rounds += 1
    end
    stop[] = true
    wait(garbage)
    unlock(held)
    @test failures == 0
    @test rounds > 0
end
