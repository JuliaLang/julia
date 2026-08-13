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

# Deliveries to a foreign call's cancellation guard are best-effort (a signal
# is skipped e.g. while a suspend handshake holds the per-thread request
# slot); redeliver until the victim observes the cancellation, mirroring the
# ^C ladder's level-triggered re-press.
# The generous timeout is pure safety margin: on an oversubscribed CI host
# the victim's threads can go unscheduled for long stretches.
function wait_cancelled(t::Task, src::CancellationTokenSource; timeout::Float64 = 120.0)
    deadline = time() + timeout
    while timedwait(() -> istaskdone(t), 2.0) !== :ok && time() < deadline
        Base.redeliver!(src)
    end
    return istaskdone(t)
end

const libccalltest = "libccalltest"

# Asynchronous delivery of foreign-call cancellation handlers is
# platform-gated (JL_HAVE_CANCEL_HANDLER_DELIVERY in julia_threads.h). On the
# other platforms a pending cancellation is only recovered level-triggered at
# the task's next cancellation point, so a foreign spin that terminates only
# when its handler runs would spin forever - and an unstoppable victim pins
# its worker thread for the rest of the process.
const HAVE_CANCEL_HANDLER_DELIVERY =
    ccall(:jl_have_cancel_handler_delivery, Cint, ()) != 0

# A Julia foreign-call cancellation handler, C-callable via @cfunction. It
# runs like a signal handler on the thread executing the annotated call:
# no allocation, locks, yields or I/O - and integer-only code, satisfying
# the preserve_all register contract trivially.
function julia_cancelspin_handler(state::Ptr{Cvoid}, sev::UInt8)
    p = Ptr{Int64}(state)
    unsafe_store!(p, Int64(sev) + 1, 2)
    unsafe_store!(p, Int64(1), 1)
    nothing
end

# BLAS cancellation handler: it executes on the thread blocked in the dgemm,
# where the library's thread-local cancel token is exactly the one the
# in-flight operation is bound to. The function pointers are pre-resolved
# into globals (a handler must not dlsym - that takes locks).
const BLAS_TOK_F = Ref(C_NULL)
const BLAS_CANCEL_F = Ref(C_NULL)
function blas_cancel_handler(::Ptr{Cvoid}, ::UInt8)
    slot = ccall(BLAS_TOK_F[], Ptr{Csize_t}, ())
    ccall(BLAS_CANCEL_F[], Cvoid, (Ptr{Csize_t}, Csize_t), slot, unsafe_load(slot))
    nothing
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
function find_collatz_counterexample()
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n ÷ 2)
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

# A compute-bound victim only observes the cancellation at its own polling
# points; the cancelling task needs a thread of its own to run on, which is
# why this lives in the threaded subprocess.
@testset "cancellation of computing tasks" begin
    # Polling cancellation via @cancel_check
    t, src = cancellable_spawn(find_collatz_counterexample)
    sleep(0.2)
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
end

@testset "async interruption of checkless loops (reset_ctx)" begin
    t, src = cancellable_spawn(find_collatz_counterexample2)
    sleep(0.5)
    cancel!(src)
    sleep(0.5)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
end

@testset "foreign-call cancellation handlers" begin
    lib = Libdl.dlopen(libccalltest)
    c_handler = Libdl.dlsym(lib, :cancelspin_handler)

    # The handler-stopped spins run only where the handler is actually
    # delivered asynchronously; elsewhere they would spin forever (see
    # HAVE_CANCEL_HANDLER_DELIVERY above).
    if HAVE_CANCEL_HANDLER_DELIVERY
    # The handler runs on the thread blocked inside the annotated call: it
    # stops the foreign spin and the call returns. The annotation itself
    # implies no cancellation point - the pre-call `@cancel_check` binds the
    # governing source (which gates delivery), and the post-call one throws
    # the delivered cancellation (so the partial result never escapes).
    cell = Ref((Int64(0), Int64(0)))
    t, src = cancellable_spawn() do
        Base.@cancel_check
        r = @ccall cancel_handler=(c_handler, cell) libccalltest.cancelspin_wait(cell::Ref{NTuple{2, Int64}})::Int64
        Base.@cancel_check
        r
    end
    sleep(0.5) # let the task get into the foreign spin
    cancel!(src)
    @test wait_cancelled(t, src)
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    @test cell[][1] == 1 # the handler stopped the spin ...
    # ... and saw severity SAFE (the handler records severity + 1)
    @test cell[][2] == Int64(Base.CANCEL_REQUEST_SAFE.request) + 1

    # The same with a handler written in Julia (via @cfunction), cancelled at
    # an escalated severity, which the handler receives as its argument.
    jh = @cfunction(julia_cancelspin_handler, Cvoid, (Ptr{Cvoid}, UInt8))
    cell2 = Ref((Int64(0), Int64(0)))
    t2, src2 = cancellable_spawn() do
        Base.@cancel_check
        r = @ccall cancel_handler=(jh, cell2) libccalltest.cancelspin_wait(cell2::Ref{NTuple{2, Int64}})::Int64
        Base.@cancel_check
        r
    end
    sleep(0.5)
    cancel!(src2, Base.CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test wait_cancelled(t2, src2)
    @test istaskfailed(t2)
    @test t2.result isa CancellationRequest
    @test cell2[][1] == 1
    @test cell2[][2] == Int64(Base.CANCEL_REQUEST_ABANDON_EXTERNAL.request) + 1
    end # HAVE_CANCEL_HANDLER_DELIVERY

    # A cancellation already pending at a caller-placed pre-call check
    # throws there: the foreign function is never entered and the handler
    # never runs.
    src3 = CancellationTokenSource()
    cancel!(src3)
    cell3 = Ref((Int64(0), Int64(0)))
    threw = try
        with(CANCEL_TOKEN => CancellationToken(src3)) do
            Base.@cancel_check
            @ccall cancel_handler=(c_handler, cell3) libccalltest.cancelspin_wait(cell3::Ref{NTuple{2, Int64}})::Int64
        end
        false
    catch e
        e isa CancellationRequest || rethrow()
        true
    end
    @test threw
    @test cell3[] === (Int64(0), Int64(0))
end

@testset "BLAS cancellation via the cancel_handler annotation" begin
    # Requires an OpenBLAS with the cancellation patch (source build; the
    # BinaryBuilder library does not have it) and asynchronous handler
    # delivery - without the latter the dgemm cannot be stopped mid-flight
    # and the cancellation-latency checks below cannot hold.
    blas = Libdl.dlopen_e("libopenblas64_")
    tok_f = blas == C_NULL ? C_NULL : Libdl.dlsym_e(blas, :openblas_cancel_token)
    if !HAVE_CANCEL_HANDLER_DELIVERY
        @warn "no asynchronous cancel-handler delivery on this platform; skipping BLAS cancellation tests"
    elseif tok_f == C_NULL
        @warn "patched OpenBLAS not available; skipping BLAS cancellation tests"
    else
        BLAS_TOK_F[] = tok_f
        BLAS_CANCEL_F[] = Libdl.dlsym(blas, :openblas_cancel)
        dgemm_f = Libdl.dlsym(blas, :dgemm_64_)
        bh = @cfunction(blas_cancel_handler, Cvoid, (Ptr{Cvoid}, UInt8))

        function gemm!(C::Matrix{Float64}, A::Matrix{Float64}, B::Matrix{Float64})
            m = Int64(size(A, 1)); k = Int64(size(A, 2)); n = Int64(size(B, 2))
            ccall(dgemm_f, Cvoid,
                  (Ref{UInt8}, Ref{UInt8}, Ref{Int64}, Ref{Int64}, Ref{Int64},
                   Ref{Float64}, Ptr{Float64}, Ref{Int64}, Ptr{Float64}, Ref{Int64},
                   Ref{Float64}, Ptr{Float64}, Ref{Int64}, Clong, Clong),
                  UInt8('N'), UInt8('N'), m, n, k, 1.0, A, m, B, k, 0.0, C, m, 1, 1)
            return C
        end
        # The cancellable variant: the annotation publishes the handler for
        # exactly the duration of the call. Since the handler runs on the
        # thread executing the dgemm, no slot handshake is needed - it
        # cancels its own thread's current generation. The bracketing
        # @cancel_checks are the caller's: the first binds the governing
        # source (gating delivery), the second throws a delivered
        # cancellation so the garbage output never escapes.
        function gemm_cancellable!(C::Matrix{Float64}, A::Matrix{Float64}, B::Matrix{Float64})
            m = Int64(size(A, 1)); k = Int64(size(A, 2)); n = Int64(size(B, 2))
            Base.@cancel_check
            @ccall cancel_handler=(bh, C_NULL) $dgemm_f(
                UInt8('N')::Ref{UInt8}, UInt8('N')::Ref{UInt8},
                m::Ref{Int64}, n::Ref{Int64}, k::Ref{Int64},
                1.0::Ref{Float64}, A::Ptr{Float64}, m::Ref{Int64},
                B::Ptr{Float64}, k::Ref{Int64},
                0.0::Ref{Float64}, C::Ptr{Float64}, m::Ref{Int64},
                1::Clong, 1::Clong)::Cvoid
            Base.@cancel_check
            return C
        end

        # correctness sanity + timing baseline; grow the problem until the
        # baseline is long enough to cancel mid-flight. A large fixed size
        # (12000 formerly) burns minutes on an oversubscribed CI box - long
        # enough to blow through the parent's subprocess watchdog.
        n = 3000
        A = rand(n, n); B = rand(n, n); C = zeros(n, n)
        gemm!(C, A, B) # warm up the thread pool
        tbase = @elapsed gemm!(C, A, B)
        while tbase < 1.0 && n < 12000
            n *= 2
            A = rand(n, n); B = rand(n, n); C = zeros(n, n)
            tbase = @elapsed gemm!(C, A, B)
        end

        # A bystander BLAS operation with no binding must be unaffected.
        nb = 2000
        A2 = rand(nb, nb); B2 = rand(nb, nb); C2 = zeros(nb, nb)
        bystander_started = Base.Event()
        bystander = Threads.@spawn (notify(bystander_started); gemm!(C2, A2, B2))

        started = Base.Event()
        t, tsrc = cancellable_spawn() do
            notify(started)
            gemm_cancellable!(C, A, B)
        end
        wait(bystander_started)
        wait(started)
        sleep(0.3) # make sure the dgemm (and its guard) are in flight
        telapsed = @elapsed begin
            cancel!(tsrc)
            @test wait_cancelled(t, tsrc; timeout = 60.0)
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

@testset "BigInt/GMP cancellation at SAFE (reset regions)" begin
    # No explicit cancellation points in the loop body: cancellation reaches
    # it through the annotated MPZ entry points - either their own
    # cancellation point, an asynchronous reset landing inside audited
    # libgmp compute (the reset region stays published across the annotated
    # call), or the deferring allocation hooks chaining into the reset on
    # exit.
    function bigmul_loop(nbits)
        b = big(3)^(nbits ÷ 2)
        m = big(10)^(nbits ÷ 8)
        while true
            b = (b * b) % m
        end
    end
    t, src = cancellable_spawn(() -> bigmul_loop(1_000_000))
    sleep(1.0) # let it get into the multiply/mod cycle
    cancel!(src)
    @test wait_cancelled(t, src)
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    # the library stays healthy: correct arithmetic after the cancellation
    @test factorial(big(30)) == prod(big(1):big(30))
    @test string(big(2)^128) == "340282366920938463463374607431768211456"

    # Allocation-churn storm: small, allocation-dominated BigInt work
    # hammered by cancellation. Deliveries frequently land inside the
    # deferring jl_gmp_counted_* hooks (the handler region), exercising the
    # defer-and-chain path; correctness is "no crash, no corruption, clean
    # arithmetic afterwards".
    deadline = time() + 8
    rounds = 0
    while time() < deadline
        gsrc = CancellationTokenSource()
        gt = with(CANCEL_TOKEN => CancellationToken(gsrc)) do
            Threads.@spawn begin
                b = big(1)
                m = big(10)^60
                while true
                    b = (b + big(12345))^2 % m
                end
            end
        end
        rounds % 3 == 0 || sleep(0.02)
        cancel!(gsrc)
        @test wait_cancelled(gt, gsrc)
        @test istaskfailed(gt)
        rounds += 1
    end
    @test rounds > 0
    @test factorial(big(20)) == 2432902008176640000
end

@testset "root-task first cancellation point inside a reset_safe call" begin
    # Regression test: a task's first cancellation point under a *new*
    # source takes the binding-publication (rebind) path, whose write
    # barrier is an ordinary call - the region publication must come after
    # it, or the freshly published reset region is cleared again and the
    # protected foreign call runs unprotected. The fresh source below forces
    # the rebind; running on the root task mirrors `julia -e` / REPL
    # foreground work (spawned tasks are pre-bound by their start-up check
    # and never caught this).
    src = CancellationTokenSource()
    done = Threads.Atomic{Bool}(false)
    canceller = Threads.@spawn begin
        t0 = time()
        while time() < t0 + 1.0
            # busy-wait: timers must not be needed to cancel a stuck root task
        end
        cancel!(src)
        while !done[] && time() < t0 + 60.0
            t1 = time()
            while time() < t1 + 0.1
            end
            Base.redeliver!(src)
        end
    end
    t0 = time()
    cancelled = try
        with(CANCEL_TOKEN => CancellationToken(src)) do
            # ~20s of uninterrupted libgmp compute if not cancelled
            Base.GMP.MPZ.fac_ui(UInt(40_000_000))
        end
        false
    catch e
        @test e isa CancellationRequest
        true
    finally
        done[] = true
    end
    @test cancelled
    @test time() - t0 < 15.0 # cancelled promptly, not at call completion
    wait(canceller)
    # the library stays healthy afterwards
    @test factorial(big(25)) == prod(big(1):big(25))
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
            if timedwait(() -> istaskdone(t), 60.0) !== :ok
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

# NOTE (port): the ABANDON_ALL freeze testset runs LAST: a freeze is a
# hail-mary that may leave the process inconsistent by design (e.g. the
# victim frozen inside an allocator or JIT-internal lock that the abandon
# refusal checks cannot see), so no later testset may depend on
# post-freeze process health.
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
    @test timedwait(() -> istaskdone(victim), 60.0) == :ok
    @test victim.state === :abandoned
    @test istaskfailed(victim)
end
