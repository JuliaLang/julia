# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: cancel!, CancellationRequest, CancellationToken, CancellationTokenSource,
    CANCEL_REQUEST_SAFE, CANCEL_REQUEST_ABANDON_EXTERNAL, CANCEL_REQUEST_ABANDON_ALL,
    CANCEL_TOKEN
using Base.ScopedValues: with, ScopedValue
using Libdl

# Threads.@spawn-style cancellable task (non-sticky, explicitly on the
# default pool - a compute-bound victim must not land on the interactive/io
# thread); returns (task, source).
function cancellable_spawn(f)
    src = CancellationTokenSource()
    t = with(() -> Threads.@spawn(f()), CANCEL_TOKEN => CancellationToken(src))
    return t, src
end

@testset "cancellation token graph semantics" begin
    # cancel! marks all descendants, level-triggered
    root = CancellationTokenSource()
    child = CancellationTokenSource(CancellationToken(root))
    grandchild = CancellationTokenSource(CancellationToken(child))
    @test !Base.iscancelled(grandchild)
    @test Base.cancel_severity(grandchild) === nothing
    @test cancel!(root)
    @test Base.iscancelled(root) && Base.iscancelled(child) && Base.iscancelled(grandchild)
    @test !cancel!(root) # idempotent at the same severity

    # a source attached under an already-cancelled parent is born cancelled
    late = CancellationTokenSource(CancellationToken(child))
    @test Base.iscancelled(late)
    @test Base.cancel_severity(late) === CANCEL_REQUEST_SAFE

    # escalation is monotonic and propagates down
    @test cancel!(root, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.cancel_severity(grandchild) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test !cancel!(grandchild, CANCEL_REQUEST_SAFE) # never de-escalates
    @test Base.cancel_severity(grandchild) === CANCEL_REQUEST_ABANDON_EXTERNAL

    # invalid severities are rejected
    @test_throws ArgumentError cancel!(CancellationTokenSource(), CancellationRequest(0x2))
    @test_throws ArgumentError cancel!(CancellationTokenSource(), CancellationRequest(0x7f))

    # walk a source's (weak, intrusive) child list
    function live_children(src::CancellationTokenSource)
        kids = CancellationTokenSource[]
        c = @atomic src.child_head
        while c !== nothing
            c = c::CancellationTokenSource
            push!(kids, c)
            c = Base._cancel_next_child(src, c)
        end
        return kids
    end

    # children are held weakly: a child that becomes unreachable is spliced
    # out of its parents' child lists by the GC, while an escaped token
    # keeps its source attached (cancellation still reaches whoever can
    # observe it)
    @noinline function make_children(root)
        CancellationTokenSource(CancellationToken(root)) # unreachable after return
        c = CancellationTokenSource(CancellationToken(root))
        return CancellationToken(c) # only the token escapes
    end
    root2 = CancellationTokenSource()
    kept = CancellationTokenSource(CancellationToken(root2))
    escaped_tok = make_children(root2)
    GC.gc() # splices the collected child out of root2's list
    cancel!(root2)
    @test Base.iscancelled(kept)
    @test Base.iscancelled(escaped_tok)
    kids = live_children(root2)
    @test length(kids) == 2 # kept + escaped; the dead child was spliced out
    @test kept in kids && escaped_tok.source in kids

    # linked sources: a source with several parents is cancelled by any of
    # them (the graph is a DAG, not just a tree)
    la = CancellationTokenSource()
    lb = CancellationTokenSource()
    linked = CancellationTokenSource(CancellationToken(la), CancellationToken(lb))
    @test linked.nparents == 2
    @test Base._cancel_parent(linked, 1) === la && Base._cancel_parent(linked, 2) === lb
    @test !Base.iscancelled(CancellationToken(linked))
    @test cancel!(lb)
    @test Base.iscancelled(CancellationToken(linked))
    @test !Base.iscancelled(CancellationToken(la))
    # escalation propagates through the other parent too
    @test cancel!(la, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.cancel_severity(linked) === CANCEL_REQUEST_ABANDON_EXTERNAL

    # born cancelled at the highest severity among the parents
    lc = CancellationTokenSource()
    ld = CancellationTokenSource()
    cancel!(ld, CANCEL_REQUEST_ABANDON_EXTERNAL)
    born = CancellationTokenSource(CancellationToken(lc), CancellationToken(ld))
    @test Base.cancel_severity(born) === CANCEL_REQUEST_ABANDON_EXTERNAL

    # a diamond converges: the shared descendant is cancelled exactly once
    # from the root
    droot = CancellationTokenSource()
    dl = CancellationTokenSource(CancellationToken(droot))
    dr = CancellationTokenSource(CancellationToken(droot))
    dd = CancellationTokenSource(CancellationToken(dl), CancellationToken(dr))
    cancel!(droot)
    @test Base.iscancelled(dd)
    @test Base.cancel_severity(dd) === CANCEL_REQUEST_SAFE

    # duplicate parents collapse to the single-parent form
    dup = CancellationTokenSource(CancellationToken(droot), CancellationToken(droot))
    @test dup.nparents == 1
    @test Base._cancel_parent(dup, 1) === droot
    @test Base.iscancelled(CancellationToken(dup)) # born under the cancelled root

    # attachment and GC splicing keep the sibling lists consistent across
    # many children coming and going
    sroot = CancellationTokenSource()
    skeep = CancellationTokenSource[]
    for i in 1:1000
        c = CancellationTokenSource(CancellationToken(sroot))
        i % 7 == 0 && push!(skeep, c)
        i % 250 == 0 && GC.gc(false)
    end
    GC.gc()
    @test length(live_children(sroot)) >= length(skeep)
    cancel!(sroot)
    @test all(Base.iscancelled, skeep)

    # deep chains cancel without recursion depth issues
    deep_root = CancellationTokenSource()
    node = deep_root
    chain = CancellationTokenSource[]
    for _ in 1:50_000
        node = CancellationTokenSource(CancellationToken(node))
        push!(chain, node) # keep them alive
    end
    cancel!(deep_root)
    @test Base.iscancelled(chain[end])

    # the current scoped token is discoverable, and `=> nothing` scopes it out
    tok = CancellationToken(CancellationTokenSource())
    @test with(() -> CANCEL_TOKEN[], CANCEL_TOKEN => tok) === tok
    @test with(() -> CANCEL_TOKEN[], CANCEL_TOKEN => nothing) === nothing
    # an unrelated nested scope inherits the governing token
    inherited = with(CANCEL_TOKEN => tok) do
        with(() -> CANCEL_TOKEN[], ScopedValue(0) => 1)
    end
    @test inherited === tok
end

@testset "cancel! repairs partially-cancelled subgraphs" begin
    # Simulate a cancel! whose descendant walk never ran (e.g. the cancelling
    # task torn down mid-walk): the state is raised, but no child is.
    root = CancellationTokenSource()
    child = CancellationTokenSource(CancellationToken(root))
    @test Base._raise_state!(root, 0x1)
    @test !Base.iscancelled(child)
    # A repeated cancel! loses the state transition (returns false) but
    # must still perform the full walk itself.
    @test !cancel!(root)
    @test Base.iscancelled(child)
end

@testset "concurrent child construction is level-triggered" begin
    # A child constructed concurrently with cancel! must end up cancelled,
    # whichever side wins the race: either the walk sees it in the child
    # list, or its constructor observes the already-cancelled parent.
    nspawners = max(Threads.nthreads() - 1, 1)
    for trial in 1:20
        root = CancellationTokenSource()
        tok = CancellationToken(root)
        go = Threads.Event()
        tasks = map(1:nspawners) do _
            Threads.@spawn begin
                wait(go)
                kids = CancellationTokenSource[]
                for _ in 1:500
                    push!(kids, CancellationTokenSource(tok))
                end
                kids
            end
        end
        notify(go)
        cancel!(root)
        for t in tasks
            @test all(Base.iscancelled, fetch(t))
        end
    end
end

@testset "cancellation source GC with dying parents" begin
    # Parents dying in the same cycle as their children: the unlink pass
    # writes into the dead parents' memory, which the sweep must keep
    # valid through the cycle.
    for _ in 1:5
        for _ in 1:1000
            r = CancellationTokenSource()
            m = CancellationTokenSource(CancellationToken(r))
            CancellationTokenSource(CancellationToken(m))
        end
        GC.gc()
    end
    GC.gc()
    GC.gc() # pages now hold no sources: the sweep flag must clear, not pin them
    # big-object sources (many parents) take the deferred-free path
    let
        parents = [CancellationTokenSource() for _ in 1:100]
        cancel!(parents[1])
        big = CancellationTokenSource(map(CancellationToken, parents)...)
        @test Base.iscancelled(big)
        parents = nothing
        big = nothing
    end
    GC.gc()
    GC.gc()
    # a survivor amid heavy churn stays correctly linked throughout
    root = CancellationTokenSource()
    keep = CancellationTokenSource(CancellationToken(root))
    for _ in 1:10_000
        CancellationTokenSource(CancellationToken(root))
    end
    GC.gc()
    GC.gc()
    cancel!(root)
    @test Base.iscancelled(keep)
end

@testset "cancellation source memory accounting" begin
    a = CancellationTokenSource()
    b = CancellationTokenSource()
    # instances are variable-sized, so (like String or Memory) the type has
    # no definite size and inference must not fold an instance's sizeof
    @test_throws ErrorException Core.sizeof(CancellationTokenSource)
    @test Base.infer_return_type(Core.sizeof, Tuple{CancellationTokenSource}) == Int
    base = Core.sizeof(a)
    linksz = 3 * sizeof(Ptr{Cvoid})
    c2 = CancellationTokenSource(CancellationToken(a), CancellationToken(b))
    @test Core.sizeof(c2) == base + 2 * linksz
    # summarysize charges the link tail and the (strong) parents, but not
    # the (weak) children
    @test Base.summarysize(c2) == base + 2 * linksz + 2 * base
    @test Base.summarysize(c2; count=true) == 3
    @test Base.summarysize(a) == base # a's children are weak: c2 not charged
    # the hidden parent references go through the regular traversal policy
    one = CancellationTokenSource(CancellationToken(a))
    @test Base.summarysize(one; exclude=CancellationTokenSource) == base + linksz
    @test Base.summarysize(one; exclude=CancellationTokenSource, count=true) == 1
    # deep parent chains are traversed iteratively, not by recursion
    node = CancellationTokenSource()
    for _ in 1:100_000
        node = CancellationTokenSource(CancellationToken(node))
    end
    @test Base.summarysize(node) >= 100_001 * base + 100_000 * linksz
end

@testset "cancellation points" begin
    # @cancel_check with no scoped token is a no-op
    @test with(() -> (Base.@cancel_check; :ran), CANCEL_TOKEN => nothing) === :ran
    @test (Base.@cancel_check; :ran) === :ran

    # a cancellation point under a cancelled scope throws the request
    src = CancellationTokenSource()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    err = with(CANCEL_TOKEN => CancellationToken(src)) do
        try
            Base.@cancel_check
            nothing
        catch e
            e
        end
    end
    @test err isa CancellationRequest
    @test err == CANCEL_REQUEST_ABANDON_EXTERNAL

    # the explicit-token form checks the given token, ignoring the scope
    live = CancellationToken(CancellationTokenSource())
    dead = CancellationToken(src)
    with(CANCEL_TOKEN => dead) do
        @test (Base.@cancel_check(live); :ran) === :ran
    end
    @test_throws CancellationRequest Base.@cancel_check(dead)
    @test (Base.@cancel_check(nothing); :ran) === :ran

    # level-triggered: after catching one request, the next point throws again
    with(CANCEL_TOKEN => dead) do
        caught = 0
        for _ in 1:2
            try
                Base.@cancel_check
            catch e
                e isa CancellationRequest || rethrow()
                caught += 1
            end
        end
        @test caught == 2
        # shielding scopes the token out
        with(CANCEL_TOKEN => nothing) do
            @test (Base.@cancel_check; :ran) === :ran
        end
    end

    # a cancellation against a nested source also throws from the nested
    # scope's cancellation points
    qroot = CancellationTokenSource()
    qchild = CancellationTokenSource(CancellationToken(qroot))
    cancel!(qroot)
    @test_throws CancellationRequest with(() -> Base.@cancel_check,
                                          CANCEL_TOKEN => CancellationToken(qchild))
end

@testset "scoped-default token cache" begin
    # The per-task cache (Task.bound_cancel_default over bound_cancel_token)
    # must be invisible: default_cancel_token() always resolves to what the
    # CANCEL_TOKEN scoped lookup would return under the current scope.
    src = CancellationTokenSource()
    tok = CancellationToken(src)

    # the ambient default outside our scopes (the test harness may itself
    # run under a governing token, e.g. the ^C episode source)
    ambient = Base.default_cancel_token()

    # warm lookups agree with the scope, and the cache invariant holds
    with(CANCEL_TOKEN => tok) do
        t1 = Base.default_cancel_token()
        t2 = Base.default_cancel_token()
        @test t1 === tok && t2 === tok
        ct = current_task()
        @test getfield(ct, :bound_cancel_default) === 0x01
        @test (@atomic :monotonic ct.bound_cancel_token) === src
    end
    # after the scope exits, the ambient default is back
    @test Base.default_cancel_token() === ambient

    # a cancellation point that publishes a *different* explicit source must
    # not leave the cache claiming it as the scoped default
    other = CancellationTokenSource()
    with(CANCEL_TOKEN => tok) do
        @test Base.default_cancel_token() === tok      # warm the cache
        Base.@cancel_check CancellationToken(other)    # rebind to `other`
        @test Base.default_cancel_token() === tok      # still the scoped one
        Base.@cancel_check nothing                     # explicit clear
        @test Base.default_cancel_token() === tok
    end

    # the same-source cancellation point keeps a warm cache intact
    with(CANCEL_TOKEN => tok) do
        @test Base.default_cancel_token() === tok
        Base.@cancel_check
        @test getfield(current_task(), :bound_cancel_default) === 0x01
        @test Base.default_cancel_token() === tok
    end

    # scope changes that do not touch CANCEL_TOKEN still resolve correctly
    sv = ScopedValue(0)
    with(CANCEL_TOKEN => tok) do
        @test Base.default_cancel_token() === tok
        with(sv => 1) do
            @test Base.default_cancel_token() === tok
        end
        @test Base.default_cancel_token() === tok
    end

    # nesting and shielding, with warm caches at every level
    inner = CancellationTokenSource()
    with(CANCEL_TOKEN => tok) do
        @test Base.default_cancel_token() === tok
        with(CANCEL_TOKEN => CancellationToken(inner)) do
            @test Base.default_cancel_token() === CancellationToken(inner)
        end
        @test Base.default_cancel_token() === tok
        with(CANCEL_TOKEN => nothing) do
            @test Base.default_cancel_token() === nothing
        end
        @test Base.default_cancel_token() === tok
    end

    # an exceptional unwind out of an inner scope restores the outer default
    with(CANCEL_TOKEN => tok) do
        @test Base.default_cancel_token() === tok
        try
            with(CANCEL_TOKEN => CancellationToken(inner)) do
                @test Base.default_cancel_token() === CancellationToken(inner)
                error("unwind")
            end
        catch err
            @test err == ErrorException("unwind")
        end
        @test Base.default_cancel_token() === tok
    end

    # a task spawned under the scope resolves its inherited default
    with(CANCEL_TOKEN => tok) do
        t = Threads.@spawn Base.default_cancel_token()
        @test fetch(t) === tok
    end

    # a warm cache still observes cancellation promptly (the cache holds the
    # source; its state is what the check reads)
    psrc = CancellationTokenSource()
    with(CANCEL_TOKEN => CancellationToken(psrc)) do
        @test Base.default_cancel_token() === CancellationToken(psrc)
        Base.@cancel_check
        cancel!(psrc)
        @test_throws CancellationRequest Base.@cancel_check
    end
end

@testset "cooperative cancellation of running tasks" begin
    # a @cancel_check polling loop is stopped cross-thread by cancel!
    started = Threads.Atomic{Bool}(false)
    t, src = cancellable_spawn() do
        started[] = true
        while true
            Base.@cancel_check
            yield() # let the canceller run when there is only one thread
        end
    end
    @test timedwait(() -> started[], 30.0) == :ok
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 30.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest

    # the scoped token is inherited through nested task spawns
    inner_result = Ref{Any}(nothing)
    t2, src2 = cancellable_spawn() do
        inner = Threads.@spawn begin
            while true
                Base.@cancel_check
                yield()
            end
        end
        # A cancellable wait would be interrupted by the delivery before
        # `inner` observes the cancellation at its own cancellation point;
        # the assertion is about `inner`'s own observation, so wait for its
        # completion shielded.
        Base._wait(inner, nothing)
        inner_result[] = inner.result
    end
    spin_started = timedwait(() -> istaskstarted(t2), 30.0)
    @test spin_started == :ok
    cancel!(src2)
    @test timedwait(() -> istaskdone(t2), 30.0) == :ok
    @test inner_result[] isa CancellationRequest

    # the hoisted-token form polls the explicit token
    src3 = CancellationTokenSource()
    tok3 = CancellationToken(src3)
    t3 = Threads.@spawn begin
        while true
            Base.@cancel_check tok3
            yield()
        end
    end
    @test timedwait(() -> istaskstarted(t3), 30.0) == :ok
    cancel!(src3, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test timedwait(() -> istaskdone(t3), 30.0) == :ok
    @test t3.result isa CancellationRequest
    @test t3.result == CANCEL_REQUEST_ABANDON_EXTERNAL
end

# An effect-free (hence reset-safe) non-inlined spin: the enclosing compiled
# cancellation region stays published across the call, so an asynchronous
# reset can interrupt it even though it performs no checks of its own.
@noinline function _pure_spin(x::Int)
    a = x
    while a >= 0
        a = (a + 1) & typemax(Int)
    end
    return a
end

function _reset_victim(started::Threads.Atomic{Bool})
    Threads.atomic_xchg!(started, true)
    # The compiled cancellation point establishes the reset region and binds
    # the scoped source; nothing between it and the reset-safe call below
    # tears the region down.
    Base.@cancel_check
    return _pure_spin(0)
end

@testset "asynchronous reset delivery" begin
    # `cancel!` delivers the shootdown itself: the reset (a signal on Unix,
    # suspend + context redirect on Windows/Darwin) longjmps back to the
    # reset point, whose re-executed check observes the cancellation and
    # throws. The victim never polls, and a signal racing the point's own
    # execution is covered by the point's level-triggered check.
    # Needs a second default-pool thread to keep the driver running while
    # the victim spins.
    if Threads.nthreads(:default) >= 2
        started = Threads.Atomic{Bool}(false)
        t, src = cancellable_spawn(() -> _reset_victim(started))
        @test timedwait(() -> started[], 30.0) == :ok
        cancel!(src)
        # rely on the test harness watchdog for hangs, like other testsets
        wait(t; throw=false)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
    end
end

# An effect-free (hence reset-safe) non-inlined spin that allocates every
# iteration. Each allocation unpublishes the enclosing region around the
# allocator (a reset-safe GC entry point) and republishes it afterwards; the
# republish re-checks the task's bound source, so a cancellation is picked up
# even though the spin never polls and no signal is ever sent.
Base.@assume_effects :effect_free @noinline _obs(r::Base.RefValue{Int}) = r[]
Base.@assume_effects :effect_free @noinline function _alloc_spin(x::Int)
    a = x
    while a >= 0
        a = (_obs(Base.RefValue(a)) + 1) & typemax(Int)
    end
    return a
end

function _alloc_victim(started::Threads.Atomic{Bool})
    Threads.atomic_xchg!(started, true)
    Base.@cancel_check
    return _alloc_spin(0)
end

# A compiled cancellation point that rebinds the task's bound_cancel_token
# to an unrelated source, as a finalizer might do while running inside an
# allocation that has the region temporarily unpublished.
@noinline _rebind_point(src::CancellationTokenSource) = (Core.cancellation_point!(src); nothing)

@testset "finalizer rebinding does not detach the region's source" begin
    # Finalizers run synchronously by a collection triggered inside an
    # allocation may execute nested cancellation points and rebind the
    # task's bound_cancel_token field. The finalizer machinery brackets that
    # state (finalizers hijack the task, so it is on them to save/restore
    # it), so delivery - including the allocator's republish self-check -
    # never observes the rebinding: cancelling the original source must
    # still kill the victim, with no signal sent.
    if Threads.nthreads(:default) >= 2
        srcB = CancellationTokenSource()  # never cancelled
        started = Threads.Atomic{Bool}(false)
        stop = Threads.Atomic{Bool}(false)
        t, src = cancellable_spawn(() -> _alloc_victim(started))
        churner = Threads.@spawn begin
            while !stop[]
                obj = Ref(0)
                finalizer(x -> _rebind_point(srcB), obj)
                obj = nothing
                GC.gc(false)
                yield()
            end
        end
        @test timedwait(() -> started[], 30.0) == :ok
        # raise the state without cancel!'s shootdown walk: the republish
        # self-check must deliver on its own despite the finalizers'
        # (bracketed) rebinding
        Base._raise_state!(src, 0x1)
        # rely on the test harness watchdog for hangs, like other testsets
        wait(t; throw=false)
        stop[] = true
        wait(churner)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
    end
end

@testset "preempt shootdown" begin
    # A preempt shootdown resets the task to its cancellation point without
    # any source being cancelled: the re-executed point observes the
    # JL_RESET_CODE_PREEMPT setjmp return (the 0x40 status bit), yields
    # cooperatively, and resumes - it must never kill the task. A subsequent
    # cancellation then must.
    if Threads.nthreads(:default) >= 2
        started = Threads.Atomic{Bool}(false)
        t, src = cancellable_spawn(() -> _reset_victim(started))
        @test timedwait(() -> started[], 30.0) == :ok
        for _ in 1:50
            tid = ccall(:jl_get_task_tid, Int16, (Any,), t)
            tid >= 0 && ccall(:jl_send_preempt_signal, Cvoid, (Int16,), tid)
            sleep(0.005)
        end
        @test !istaskdone(t)
        cancel!(src) # shoots down the (re-established) region itself
        # rely on the test harness watchdog for hangs, like other testsets
        wait(t; throw=false)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
    end
end

@testset "cancel! delivers to allocating regions" begin
    # An allocating reset region additionally self-recovers: were the
    # shootdown ever lost, the next allocation's republish re-check would
    # perform the delivery.
    if Threads.nthreads(:default) >= 2
        started = Threads.Atomic{Bool}(false)
        t, src = cancellable_spawn(() -> _alloc_victim(started))
        @test timedwait(() -> started[], 30.0) == :ok
        cancel!(src)
        # rely on the test harness watchdog for hangs, like other testsets
        wait(t; throw=false)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
    end
end

@testset "reset region republish self-delivery" begin
    # A cancellation that arrives while an allocation holds the region
    # unpublished finds no reset context and is dropped by its sender; the
    # allocator's republish must re-check the bound source and perform the
    # missed delivery itself, or the wakeup would be lost. This makes an
    # allocating reset region cancellable without any signal at all - to
    # isolate that path from cancel!'s automatic shootdown, raise the
    # source's state directly.
    if Threads.nthreads(:default) >= 2
        started = Threads.Atomic{Bool}(false)
        t, src = cancellable_spawn(() -> _alloc_victim(started))
        @test timedwait(() -> started[], 30.0) == :ok
        Base._raise_state!(src, 0x1)
        # rely on the test harness watchdog for hangs, like other testsets
        wait(t; throw=false)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
    end
end
## Request-delivery tests (cancellation of waiting tasks)

# Start `f` as an @async-style (sticky, co-scheduled) task governed by a
# fresh cancellation source; returns (task, source).
function cancellable(f)
    src = CancellationTokenSource()
    t = with(() -> @async(f()), CANCEL_TOKEN => CancellationToken(src))
    return t, src
end

# wait a little, so cancellation targets are (most likely) started and parked
spin(n=4) = for _ in 1:n; yield(); end

# wait for `t` and assert it failed with the delivered CancellationRequest
function expect_cancelled(t::Task)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
end

# a Pipe with both ends linked and async-capable
function linked_pipe()
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    return p
end

# a portable long-running command (Windows has no `sleep` binary)
sleep_cmd(secs::Real) = `$(Base.julia_cmd()) --startup-file=no -e "sleep($secs)"`

# far exceeds any OS pipe buffer, so an uncancelled pipe write parks, while
# staying a single uv request (well under Base.MAX_OS_WRITE)
const BIG_WRITE = 8_000_000

# whether `t` is parked (its wait registration is enqueued on some waitee)
is_parked(t::Task) = (w = @atomic :acquire t.waiting_on; w isa Base.WaitEntry && Base._slot_owner(w, 1) !== nothing)
parked_on(t::Task, @nospecialize(x)) = (w = @atomic :acquire t.waiting_on; x isa Task && (x = x.donenotify); w isa Base.WaitEntry && Base._find_slot(w, x) != 0)

# The entries currently on `src`'s waiter list (test-only: assumes no
# concurrent walk while traversing)
function registry_entries(src::CancellationTokenSource)
    entries = Base.WaitEntry[]
    w = @atomic src.waiters_head
    while w isa Base.WaitEntry
        push!(entries, w)
        w = Base._slot_next(w, Base._find_slot(w, src))
    end
    return entries
end

@testset "waiter registry: sticky registration" begin
    src = CancellationTokenSource()
    c = Channel{Int}(0)
    done = Channel{Nothing}(1)
    t = with(CANCEL_TOKEN => CancellationToken(src)) do
        @async begin
            take!(c)   # cancellable park #1: registers
            put!(done, nothing)
            take!(c)   # cancellable park #2: sticky re-arm, no new push
        end
    end
    @test timedwait(() -> is_parked(t), 10.0) == :ok
    entries = registry_entries(src)
    @test length(entries) == 1
    w1 = entries[1]
    @test Base._find_slot(w1, src) != 0
    @test w1 === t.cached_cancel_entry
    put!(c, 1)
    take!(done)
    # a normal wake does no registry work: the registration stays in place
    @test registry_entries(src) == [w1]
    @test Base._find_slot(w1, src) != 0
    # and the second park re-arms the same registered entry
    @test timedwait(() -> is_parked(t), 10.0) == :ok
    @test registry_entries(src) == [w1]
    @test (@atomic t.waiting_on) === w1
    put!(c, 2)
    wait(t)
    # the entry of a completed task is collected by the next walk
    cancel!(src)
    @test isempty(registry_entries(src))
end

@testset "waiter registry: shielded parks stay shielded" begin
    # shielded parks arm a distinct entry from cancellable parks, so the
    # walk's expected-entry claim CAS structurally cannot land on a shield -
    # the sticky source registration stays linked but unarmed meanwhile
    src = CancellationTokenSource()
    c = Channel{Int}(0)
    t = with(CANCEL_TOKEN => CancellationToken(src)) do
        @async begin
            take!(c)                 # cancellable park
            take!(c; cancel=nothing) # shielded park of a distinct entry
        end
    end
    @test timedwait(() -> is_parked(t), 10.0) == :ok
    wc = t.cached_cancel_entry
    @test wc isa Base.WaitEntry && (@atomic t.waiting_on) === wc
    put!(c, 1)
    @test timedwait(() -> (x = @atomic t.waiting_on;
                           x isa Base.WaitEntry && Base._slot_owner(x, 1) !== nothing), 10.0) == :ok
    @test (@atomic t.waiting_on) === t.cached_wait_entry # the plain entry
    @test (@atomic t.waiting_on) !== wc
    @test Base._find_slot(wc, src) != 0      # still registered (sticky)
    cancel!(src)
    spin()
    @test !istaskdone(t)                     # the shielded wait is untouched
    put!(c, 2)
    wait(t)
end

@testset "subscriptions die with their birth source" begin
    # schedule_on_notify! subscribes a not-yet-started task; its birth
    # scope's source governs it: cancelled => the task dies at start
    src = CancellationTokenSource()
    c = Threads.Condition()
    t = with(() -> Task(() -> 42), CANCEL_TOKEN => CancellationToken(src))
    t.sticky = false
    @lock c Base.schedule_on_notify!(c, t)
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest

    # born cancelled: refused at subscribe, never enqueued
    t2 = with(() -> Task(() -> 42), CANCEL_TOKEN => CancellationToken(src))
    t2.sticky = false
    @lock c Base.schedule_on_notify!(c, t2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test t2.result isa CancellationRequest

    # a shielded subscriber survives the (already cancelled) scope and
    # starts on notify
    t3 = with(() -> Task(() -> 7), CANCEL_TOKEN => nothing)
    t3.sticky = false
    @lock c Base.schedule_on_notify!(c, t3)
    @lock c notify(c)
    @test timedwait(() -> istaskdone(t3), 10.0) == :ok
    @test fetch(t3) == 7

    # the task-completion variant: subscribe to a done task under a
    # cancelled source - killed on the fast path too
    donesrc = CancellationTokenSource()
    host = @async 1
    wait(host)
    t4 = with(() -> Task(() -> 42), CANCEL_TOKEN => CancellationToken(donesrc))
    t4.sticky = false
    cancel!(donesrc)
    Base.schedule_on_notify!(host, t4)
    @test timedwait(() -> istaskdone(t4), 10.0) == :ok
    @test t4.result isa CancellationRequest

    # a started task cannot be subscribed
    started = Base.Event()
    t5 = @async (notify(started); sleep(0.05); 1)
    wait(started)
    @test_throws ConcurrencyViolationError (@lock c Base.schedule_on_notify!(c, t5))
    wait(t5)

    # neither can a merely-queued one: @async has scheduled it, so its
    # own first park owns the arm slot (a subscription arm would leak the
    # sleep timer's cond lock and wedge the process at close)
    t6 = @async (sleep(0.01); 1)
    @test_throws ConcurrencyViolationError (@lock c Base.schedule_on_notify!(c, t6))
    wait(t6)
end

@testset "waiter registry: refused arm cannot leak into a shield" begin
    # the race: a cancellation walk observes an armed cancellable park
    # and judges it eligible; the park is then
    # claimed away (here: by the registration's own refusal) and the task
    # immediately re-parks shielded. The walk's claim CAS must fail against
    # the shielded arm. Hammered, since the window is two walk instructions.
    for _ in 1:200
        src = CancellationTokenSource()
        tok = CancellationToken(src)
        c = Channel{Int}(0)
        entered = Threads.Event()
        t = @async begin
            notify(entered)
            # cancellable wait racing the cancel below: either thrown at
            # entry, refused at registration, or claimed by the walk
            try
                take!(c; cancel=tok)
            catch err
                err isa CancellationRequest || rethrow()
            end
            # immediately re-park shielded; a stale-eligibility claim from
            # the same walk must not be able to interrupt this
            take!(c; cancel=nothing)
        end
        wait(entered)
        cancel!(src)
        put!(c, 1)   # completes the shielded park (or the pre-cancel take!)
        # a leaked claim throws CancellationRequest out of the shielded
        # take! and fails the task
        wait(t)
        @test !istaskfailed(t)
    end
end

@testset "waiter registry: token migration rebinds the cached entry" begin
    src1 = CancellationTokenSource()
    src2 = CancellationTokenSource()
    c = Channel{Int}(0)
    step = Channel{Nothing}(1)
    t = @async begin
        with(CANCEL_TOKEN => CancellationToken(src1)) do
            take!(c)
        end
        put!(step, nothing)
        with(CANCEL_TOKEN => CancellationToken(src2)) do
            take!(c)
        end
    end
    @test timedwait(() -> is_parked(t), 10.0) == :ok
    w = t.cached_cancel_entry
    @test w isa Base.WaitEntry && Base._find_slot(w, src1) != 0
    @test registry_entries(src1) == [w]
    put!(c, 1)
    take!(step)
    @test timedwait(() -> (x = @atomic t.waiting_on;
                           x isa Base.WaitEntry && Base._slot_owner(x, 1) !== nothing &&
                           Base._find_slot(x, src2) != 0), 10.0) == :ok
    # parking under the new source physically unregistered the cached entry
    # from the old one and rebound it
    @test t.cached_cancel_entry === w
    @test Base._find_slot(w, src2) != 0
    @test isempty(registry_entries(src1))
    @test registry_entries(src2) == [w]
    put!(c, 2)
    wait(t)
end

@testset "waiter registry: pruning" begin
    # entries of completed tasks are pruned once enough dead registrations
    # accumulate (the dead-count threshold), without any cancellation
    src = CancellationTokenSource()
    tok = CancellationToken(src)
    for _ in 1:80
        c = Channel{Int}(0)
        t = with(() -> @async(take!(c)), CANCEL_TOKEN => tok)
        @test timedwait(() -> is_parked(t), 10.0) == :ok
        put!(c, 1)
        wait(t)
    end
    @test length(registry_entries(src)) < 40

    # single-use wait_with_timeout registrations are retired at wait exit
    # and collected the same way
    src2 = CancellationTokenSource()
    cond = Threads.Condition()
    with(CANCEL_TOKEN => CancellationToken(src2)) do
        for _ in 1:80
            lock(cond)
            try
                @test Base.Experimental.wait_with_timeout(cond; timeout=0.001) === :timed_out
            finally
                unlock(cond)
            end
        end
    end
    @test length(registry_entries(src2)) < 40
end

@testset "waiter registry: registration racing cancellation" begin
    # hammer the push/arm-vs-cancel window: every task must observe the
    # cancellation exactly once, whether it lost before parking (refusal),
    # while parking, or parked
    for _ in 1:50
        src = CancellationTokenSource()
        ts = Task[]
        for _ in 1:8
            push!(ts, with(() -> @async(sleep(10)), CANCEL_TOKEN => CancellationToken(src)))
        end
        cancel!(src)
        for t in ts
            @test timedwait(() -> istaskdone(t), 10.0) == :ok
            @test istaskfailed(t) && t.result isa CancellationRequest
        end
    end
end

@testset "waiter registry: multi-slot wait-any entries" begin
    # waitany parks through a single WaitEntryN with one slot per waited
    # task plus the cancellation-source slot
    src = CancellationTokenSource()
    c = Channel{Int}(0)
    ts = [@async take!(c) for _ in 1:3]
    wa = with(CANCEL_TOKEN => CancellationToken(src)) do
        @async waitany(ts)
    end
    @test timedwait(() -> (x = @atomic wa.waiting_on; x isa Base.WaitEntryN), 10.0) == :ok
    w = (@atomic wa.waiting_on)::Base.WaitEntryN
    @test Base._nslots(w) == 4
    @test count(i -> Base._slot_owner(w, i) isa Base.ThreadSynchronizer, 1:4) == 3
    @test registry_entries(src) == [w]
    # first completion wins; the entry is withdrawn from every waitq and
    # retired
    put!(c, 1)
    done, remaining = fetch(wa)
    @test length(done) == 1 && length(remaining) == 2
    @test (@atomic :monotonic w.task) === nothing
    @test count(i -> Base._slot_owner(w, i) isa Base.ThreadSynchronizer, 1:4) == 0
    # the retired source registration is collected by the next walk
    cancel!(src)
    @test isempty(registry_entries(src))
    for t in ts
        istaskdone(t) || put!(c, 0)
    end
    foreach(wait, ts)

    # cancellation of the scope interrupts waitany and cleans up all slots;
    # the waited tasks (in a different scope) are unaffected
    src2 = CancellationTokenSource()
    c2 = Channel{Int}(0)
    ts2 = [@async take!(c2) for _ in 1:3]
    wa2 = with(CANCEL_TOKEN => CancellationToken(src2)) do
        @async waitany(ts2)
    end
    @test timedwait(() -> (x = @atomic wa2.waiting_on; x isa Base.WaitEntryN), 10.0) == :ok
    w2 = (@atomic wa2.waiting_on)::Base.WaitEntryN
    cancel!(src2)
    @test timedwait(() -> istaskdone(wa2), 10.0) == :ok
    @test istaskfailed(wa2) && wa2.result isa CancellationRequest
    for t in ts2
        @test !istaskdone(t)
        @test Base._find_slot(w2, t.donenotify) == 0
    end
    for _ in ts2
        put!(c2, 0)
    end
    foreach(wait, ts2)

    # waitall re-arms the same registered entry across completions
    c3 = Channel{Int}(0)
    ts3 = [@async take!(c3) for _ in 1:3]
    wa3 = @async waitall(ts3)
    @test timedwait(() -> (x = @atomic wa3.waiting_on; x isa Base.WaitEntryN), 10.0) == :ok
    w3 = (@atomic wa3.waiting_on)::Base.WaitEntryN
    for _ in 1:3
        put!(c3, 0)
    end
    done3, remaining3 = fetch(wa3)
    @test length(done3) == 3 && isempty(remaining3)
    @test (@atomic wa3.waiting_on) === nothing
    @test count(i -> Base._slot_owner(w3, i) isa Base.ThreadSynchronizer, 1:Base._nslots(w3)) == 0
end

@testset "level-triggered delivery and shielding" begin
    # cancellation is uniformly level-triggered: after catching the request,
    # unshielded waits under the cancelled scope keep throwing; shielded
    # cleanup proceeds, and the severity remains observable under the shield
    src = CancellationTokenSource()
    phase = Ref{Any}(:init)
    t = with(CANCEL_TOKEN => CancellationToken(src)) do
        @async try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
            phase[] = :caught
            rethrew = try
                sleep(1000)
                false
            catch e2
                e2 isa CancellationRequest
            end
            sleep(0.01; cancel=nothing) # shielded cleanup is permitted
            rethrew &= Base.cancel_severity(CANCEL_TOKEN[]::CancellationToken) === CANCEL_REQUEST_SAFE
            phase[] = rethrew ? :done : :no_retrigger
        end
    end
    spin()
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test phase[] === :done

    # an internal teardown re-park (min_severity) is woken only by
    # escalation: the convention is that a teardown which acknowledged
    # severity `s` stages the floor `s + 0x01` (min_severity is the lowest
    # severity that may wake the wait), so a re-cancel at the acknowledged
    # severity must leave the task parked
    srcm = CancellationTokenSource()
    cancel!(srcm)  # acknowledged severity: SAFE (0x1)
    inner = @async sleep(5)
    tm = @async Base._wait(inner, CancellationToken(srcm); min_severity=0x02)
    @test timedwait(() -> is_parked(tm), 10.0) == :ok
    cancel!(srcm)  # re-cancel at the acknowledged severity: no wake
    spin()
    @test !istaskdone(tm)
    cancel!(srcm, CANCEL_REQUEST_ABANDON_EXTERNAL)  # escalation wakes it
    expect_cancelled(tm)
end

# a parked watcher keeps the watched source reachable through its live
# token reference alone (no local rooting in the caller's frame)
@noinline function _spawn_watcher_on_child(parent)
    child = CancellationTokenSource(CancellationToken(parent))
    t = @async wait(CancellationToken(child); cancel=nothing)
    @assert timedwait(() -> parked_on(t, child), 10.0) == :ok
    return t
end

@testset "waiting for a token as an event" begin
    # an already-cancelled token: immediate value return, no throw
    src = CancellationTokenSource()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    req = wait(CancellationToken(src); cancel=nothing)
    @test req isa CancellationRequest
    @test req.request == CANCEL_REQUEST_ABANDON_EXTERNAL.request

    # a parked watcher is completed (not interrupted) by the cancellation
    src = CancellationTokenSource()
    t = @async wait(CancellationToken(src); cancel=nothing)
    @test timedwait(() -> parked_on(t, src), 10.0) == :ok
    @test !istaskdone(t)
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test !istaskfailed(t)
    @test fetch(t) isa CancellationRequest

    # ... including at ABANDON_ALL: a watcher observes the source but does
    # not run under it, so the freeze never applies to it
    src = CancellationTokenSource()
    t = @async wait(CancellationToken(src); cancel=nothing)
    @test timedwait(() -> parked_on(t, src), 10.0) == :ok
    cancel!(src, CANCEL_REQUEST_ABANDON_ALL)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test !istaskfailed(t)
    @test fetch(t).request == CANCEL_REQUEST_ABANDON_ALL.request

    # ancestor cancellation reaches a watcher on a descendant source
    parent = CancellationTokenSource()
    child = CancellationTokenSource(CancellationToken(parent))
    t = @async wait(CancellationToken(child); cancel=nothing)
    @test timedwait(() -> parked_on(t, child), 10.0) == :ok
    cancel!(parent)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test fetch(t) isa CancellationRequest

    # ordinary cancel semantics: the governing token (inherited from the
    # scope) interrupts the wait, leaving the watched token untouched
    watched = CancellationTokenSource()
    gov = CancellationTokenSource()
    t = with(CANCEL_TOKEN => CancellationToken(gov)) do
        @async wait(CancellationToken(watched))
    end
    @test timedwait(() -> parked_on(t, watched), 10.0) == :ok
    cancel!(gov)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    @test !Base.iscancelled(CancellationToken(watched))

    # a watcher governed by an *ancestor* of the watched source is
    # interrupted, not completed: the ancestor's own walk claims it before
    # descending to the watched child (this is why callback watchers shield)
    outer = CancellationTokenSource()
    inner = CancellationTokenSource(CancellationToken(outer))
    t = with(CANCEL_TOKEN => CancellationToken(outer)) do
        @async wait(CancellationToken(inner))
    end
    @test timedwait(() -> parked_on(t, inner), 10.0) == :ok
    cancel!(outer)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)

    # waiting under the same token is refused, explicitly and inherited
    srcs = CancellationTokenSource()
    toks = CancellationToken(srcs)
    @test_throws ArgumentError wait(toks; cancel=toks)
    @test_throws ArgumentError with(() -> wait(toks), CANCEL_TOKEN => toks)

    # the callback pattern: a shielded watcher performs its action during
    # the cancellation
    src = CancellationTokenSource()
    fired = Base.Event()
    watcher = @async begin
        wait(CancellationToken(src); cancel=nothing)
        notify(fired)
    end
    @test timedwait(() -> parked_on(watcher, src), 10.0) == :ok
    cancel!(src)
    wait(fired; cancel=nothing)
    wait(watcher)
    @test !istaskfailed(watcher)

    # a parked watcher keeps the watched source attached to the tree
    # (observability == reachability), so an ancestor cancellation still
    # reaches it after a GC
    parent2 = CancellationTokenSource()
    t2 = _spawn_watcher_on_child(parent2)
    GC.gc()
    cancel!(parent2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test fetch(t2) isa CancellationRequest
end

@testset "cancellation of waiting tasks" begin
    # Cancellation of `sleep`
    t, src = cancellable(() -> sleep(1000))
    spin()
    cancel!(src)
    expect_cancelled(t)

    # After catching the request, cleanup that must block shields itself
    t2, src2 = cancellable() do
        try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
            sleep(0.01; cancel=nothing) # shielded: parking for cleanup
            return :cleanup_ok
        end
    end
    spin()
    cancel!(src2)
    @test fetch(t2) === :cleanup_ok

    # Cancellation of a task blocked on a Channel
    c = Channel{Int}(0)
    t, src = cancellable(() -> take!(c))
    spin()
    cancel!(src)
    expect_cancelled(t)
    # The channel remains usable
    t2 = @async take!(c)
    put!(c, 7)
    @test fetch(t2) == 7

    # Cancelling a scope reaches a task waiting on another task; the waited-on
    # task (in the same scope) is cancelled through the same tree
    local t_in
    t, src = cancellable() do
        t_in = @async sleep(1000)
        wait(t_in)
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test timedwait(() -> istaskdone(t_in), 10.0) == :ok
    @test istaskfailed(t_in)

    # ... but a task waited on from a *different* scope is unaffected by the
    # waiter's cancellation
    t_out = @async sleep(5)
    t, src = cancellable(() -> wait(t_out))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test !istaskdone(t_out)
    wait(t_out)
    @test istaskdone(t_out) && !istaskfailed(t_out)
end

@testset "cancellation of lock and condition waits" begin
    # Task blocked in lock(::ReentrantLock)
    lk = ReentrantLock()
    lock(lk)
    t, src = cancellable(() -> lock(lk))
    spin()
    # let it spin through the fast path and park
    @test timedwait(() -> is_parked(t), 5.0) == :ok
    cancel!(src)
    expect_cancelled(t)
    # the lock remains functional
    unlock(lk)
    @test trylock(lk)
    unlock(lk)
    t2 = @async (lock(lk); unlock(lk); true)
    @test fetch(t2)

    # Task blocked in put! on a full channel
    c = Channel{Int}(1)
    put!(c, 1)
    t, src = cancellable(() -> put!(c, 2))
    spin()
    cancel!(src)
    expect_cancelled(t)
    @test take!(c) == 1
    put!(c, 3) # channel remains functional
    @test take!(c) == 3

    # Task blocked in wait(::Threads.Condition)
    cond = Threads.Condition()
    t, src = cancellable(() -> @lock cond wait(cond))
    spin()
    cancel!(src)
    expect_cancelled(t)
    @lock cond notify(cond) # still functional (no waiters)

    # Task blocked in wait(::Base.Process); the process itself keeps running
    p = run(sleep_cmd(1000); wait=false)
    t, src = cancellable(() -> wait(p))
    spin()
    cancel!(src)
    expect_cancelled(t)
    @test process_running(p)
    kill(p); wait(p)

    # Task blocked in waitany; the awaited tasks live in different scopes and
    # remain unaffected by the waiter's cancellation
    t1, src1 = cancellable(() -> sleep(1000))
    t2, src2 = cancellable(() -> sleep(1000))
    t, src = cancellable(() -> waitany([t1, t2]))
    spin()
    cancel!(src)
    expect_cancelled(t)
    @test !istaskdone(t1) && !istaskdone(t2)
    # their own scopes' cancellation reaches them
    cancel!(src1); cancel!(src2)
    @test_throws TaskFailedException wait(t1)
    @test_throws TaskFailedException wait(t2)
end

# stdlibs exercised below (loaded through the loader, like other Base tests,
# to avoid a test-environment dependency)
const Sockets = Base.require(Base.PkgId(Base.UUID("6462fe0b-24de-5631-8697-dd941f90decc"), "Sockets"))
const FileWatching = Base.require(Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"))

@testset "cancellation of stdlib waits (Sockets, FileWatching, Semaphore)" begin
    # Base.Semaphore: a cancelled acquire does not leak a permit
    sem = Base.Semaphore(1)
    Base.acquire(sem)
    t, src = cancellable(() -> Base.acquire(sem))
    spin()
    cancel!(src)
    expect_cancelled(t)
    Base.release(sem)
    Base.acquire(sem) # the permit is still available
    Base.release(sem)

    # Sockets.accept
    port, server = Sockets.listenany(Sockets.localhost, 0)
    t, src = cancellable(() -> Sockets.accept(server))
    spin()
    cancel!(src)
    expect_cancelled(t)
    # the server keeps accepting afterwards
    t2 = @async Sockets.accept(server)
    sock = Sockets.connect(Sockets.localhost, port)
    @test fetch(t2) isa Sockets.TCPSocket
    close(sock); close(server)

    # FileWatching: fd polling and file watching
    if !Sys.iswindows() # fd polling requires a socket on Windows (ENOTSOCK)
        p = linked_pipe()
        fd = Base._fd(p.out)
        t, src = cancellable(() -> FileWatching.wait(fd; readable=true)) # nothing is ever written
        spin()
        cancel!(src)
        expect_cancelled(t)
        close(p)
    end

    path = tempname()
    touch(path)
    t, src = cancellable(() -> FileWatching.watch_file(path, 100.0)) # the file never changes
    spin()
    cancel!(src)
    expect_cancelled(t)
    rm(path)
end

@testset "explicit cancel keyword arguments" begin
    cancelled_src = CancellationTokenSource()
    cancel!(cancelled_src)
    ctok = CancellationToken(cancelled_src)

    # a pre-cancelled token throws at entry, before any side effect
    p = linked_pipe()
    @test_throws CancellationRequest read(p.out, 10; cancel=ctok)
    @test_throws CancellationRequest read(p.out; cancel=ctok)
    @test_throws CancellationRequest read(p.out, String; cancel=ctok)
    @test_throws CancellationRequest read(p.out, UInt8; cancel=ctok)
    @test_throws CancellationRequest read!(p.out, zeros(UInt8, 4); cancel=ctok)
    @test_throws CancellationRequest readbytes!(p.out, zeros(UInt8, 4); cancel=ctok)
    @test_throws CancellationRequest readline(p.out; cancel=ctok)
    @test_throws CancellationRequest readuntil(p.out, 0x0a; cancel=ctok)
    @test_throws CancellationRequest readavailable(p.out; cancel=ctok)
    @test_throws CancellationRequest eof(p.out; cancel=ctok)
    @test_throws CancellationRequest write(p.in, zeros(UInt8, 8); cancel=ctok)
    @test_throws CancellationRequest write(p.in, "hello"; cancel=ctok)
    @test_throws CancellationRequest write(p.in, "a", "b"; cancel=ctok)
    @test_throws CancellationRequest flush(p.in; cancel=ctok)
    @test_throws CancellationRequest sleep(10; cancel=ctok)
    @test_throws CancellationRequest wait(Timer(10); cancel=ctok)
    @test_throws CancellationRequest run(sleep_cmd(5); cancel=ctok)
    @test_throws CancellationRequest success(sleep_cmd(5); cancel=ctok)
    @test_throws CancellationRequest read(sleep_cmd(5); cancel=ctok)
    @test_throws CancellationRequest readchomp(sleep_cmd(5); cancel=ctok)
    @test_throws CancellationRequest Sockets.getalladdrinfo("localhost"; cancel=ctok)
    @test_throws CancellationRequest Sockets.getaddrinfo("localhost"; cancel=ctok)
    @test_throws CancellationRequest Sockets.getnameinfo(Sockets.localhost; cancel=ctok)
    @test_throws CancellationRequest FileWatching.watch_file(tempdir(), 5.0; cancel=ctok)
    @test_throws CancellationRequest FileWatching.poll_fd(Base._fd(p.out), 5.0; readable=true, cancel=ctok)

    # `cancel = nothing` shadows an (already cancelled) outer scope
    write(p.in, "ab\n")
    with(CANCEL_TOKEN => ctok) do
        @test read(p.out, 2; cancel=nothing) == b"ab"
    end
    close(p)

    # live cancellation through an explicit token: blocked read
    p2 = linked_pipe()
    src = CancellationTokenSource()
    t = @async read(p2.out, 10; cancel=CancellationToken(src))
    spin()
    cancel!(src)
    expect_cancelled(t)
    close(p2)

    # live cancellation: blocked write - `writepartial` returns the
    # partial byte count; the cancellation is delivered at the writer's
    # next cancellation point
    p3 = linked_pipe()
    src3 = CancellationTokenSource()
    big = zeros(UInt8, BIG_WRITE)
    tok3 = CancellationToken(src3)
    nwritten3 = Ref{Any}(nothing)
    t3 = @async begin
        nwritten3[] = writepartial(p3.in, big; cancel=tok3)
        Base.@cancel_check tok3
    end
    sleep(0.5)
    cancel!(src3)
    expect_cancelled(t3)
    @test nwritten3[] isa Int
    @test 0 <= nwritten3[] < length(big)
    close(p3)

    # split writes (multiple outstanding uv requests, forced via a tiny
    # chunk size): cancellation sweeps the queued chunks tail-first and
    # the reported count is exactly the bytes on the wire
    let p = linked_pipe()
        srcs = CancellationTokenSource()
        toks = CancellationToken(srcs)
        n = 1 << 20
        data = rand(UInt8, n)
        chunk = UInt(4096)
        writer = @async GC.@preserve data begin
            Base.iolock_begin()
            Base._uv_write_wait(p.in, pointer(data), UInt(n), toks, data, true, chunk)
        end
        # observable progress gate: a successful read proves the writer is
        # past its entry check with the chunks submitted (so the
        # cancellation below interrupts the wait rather than the entry),
        # then the kernel buffer refills and the writer parks with most
        # chunks still queued in libuv
        head = read(p.out, Int(chunk))
        @test head == data[1:Int(chunk)]
        @test timedwait(() -> (@atomic :monotonic writer.waiting_on) !== nothing, 20.0) == :ok
        cancel!(srcs)
        accepted = fetch(writer)::Int
        if Sys.iswindows()
            # the OS pipe buffer can absorb the entire write before the
            # cancellation lands (its quota is advisory and grows); the
            # sweep then settles an already-completed write in full
            @test length(head) <= accepted <= n
        else
            @test length(head) <= accepted < n
        end
        # the stream survives the sweep: a follow-up write goes through
        extra = rand(UInt8, 1000)
        drained = @async read(p.out)
        write(p.in, extra)
        close(p.in)
        received = vcat(head, fetch(drained)::Vector{UInt8})
        # the accepted count is a clean prefix of the data, and the
        # follow-up write arrives intact after it. On Windows the OS may
        # underreport a cancelled write's count, so only assert exactness
        # elsewhere.
        if !Sys.iswindows()
            @test length(received) == accepted + length(extra)
        end
        @test length(received) >= accepted + length(extra)
        @test received[1:accepted] == data[1:accepted]
        @test received[(end - length(extra) + 1):end] == extra
        close(p.out)
    end

    # split write, uncancelled: the countdown wake delivers the exact
    # total and the requests are settled cleanly
    let p = linked_pipe()
        n = 1 << 18
        data = rand(UInt8, n)
        drained = @async read(p.out)
        accepted = GC.@preserve data begin
            Base.iolock_begin()
            Base._uv_write_wait(p.in, pointer(data), UInt(n), nothing, data, false, UInt(4096))
        end
        @test accepted == n
        close(p.in)
        @test fetch(drained) == data
        close(p.out)
    end

    # live cancellation: Sockets.accept and recv with explicit tokens
    port, server = Sockets.listenany(Sockets.localhost, 0)
    src4 = CancellationTokenSource()
    t4 = @async Sockets.accept(server; cancel=CancellationToken(src4))
    spin()
    cancel!(src4)
    expect_cancelled(t4)
    close(server)

    udp = Sockets.UDPSocket()
    Sockets.bind(udp, Sockets.localhost, 0)
    src5 = CancellationTokenSource()
    t5 = @async Sockets.recv(udp; cancel=CancellationToken(src5))
    spin()
    cancel!(src5)
    expect_cancelled(t5)
    close(udp)

    # live cancellation: FileWatching.watch_file with an explicit token
    path = tempname()
    touch(path)
    src6 = CancellationTokenSource()
    t6 = @async FileWatching.watch_file(path, 100.0; cancel=CancellationToken(src6))
    spin()
    cancel!(src6)
    expect_cancelled(t6)
    rm(path)

    # live cancellation: run with an explicit token; the child process is
    # not reaped by the cancelled wait
    src7 = CancellationTokenSource()
    t7 = @async run(sleep_cmd(5); cancel=CancellationToken(src7))
    sleep(0.5)
    cancel!(src7)
    expect_cancelled(t7)
end

@testset "cancellation of blocked stream writes" begin
    p = linked_pipe()
    try
        # A write far exceeding the OS pipe buffer blocks until cancelled.
        # `write` throws the CancellationRequest (after the in-flight
        # request is resolved - for a SAFE cancellation only once the
        # completion callback has provably released the buffer); callers
        # prepared for short counts use `writepartial`, which returns the
        # partial byte count and leaves delivery to the next cancellation
        # point.
        big = zeros(UInt8, BIG_WRITE)
        t, src = cancellable() do
            write(p, big)
        end
        @test timedwait(() -> parked_on(t, p.in), 10.0) == :ok
        cancel!(src)
        expect_cancelled(t)
    finally
        close(p)
    end
    p2 = linked_pipe()
    try
        big = zeros(UInt8, BIG_WRITE)
        nwritten = Ref{Any}(nothing)
        t, src = cancellable() do
            nwritten[] = writepartial(p2.in, big)
            Base.@cancel_check
        end
        @test timedwait(() -> parked_on(t, p2.in), 10.0) == :ok
        cancel!(src)
        expect_cancelled(t)
        @test nwritten[] isa Int
        @test 0 <= nwritten[] < length(big)
    finally
        close(p2)
    end
end

@testset "cancellation of closewrite (shutdown) waits" begin
    p = linked_pipe()
    try
        # A blocked write keeps the shutdown request (which queues behind it)
        # from completing; the closewrite wait must still be interruptible.
        big = zeros(UInt8, BIG_WRITE)
        # (the cancelled write itself throws the request)
        tw, srcw = cancellable(() -> write(p, big))
        @test timedwait(() -> parked_on(tw, p.in), 10.0) == :ok
        ts, srcs = cancellable(() -> closewrite(p.in))
        @test timedwait(() -> parked_on(ts, p.in), 5.0) == :ok
        cancel!(srcs)
        expect_cancelled(ts)
        cancel!(srcw)
        @test_throws TaskFailedException wait(tw)
    finally
        close(p)
    end
end

@testset "cancelled condition waiter reacquiring a contended lock" begin
    # A cancelled `wait(::Threads.Condition)` must rethrow the
    # CancellationRequest after reacquiring the condition lock, even when
    # the reacquire is contended: the waiter's stale (lazily collected)
    # condition-queue entry stays linked while it parks on the lock with a
    # fresh wait entry, and must not corrupt either queue.
    cond = Threads.Condition()
    src = Base.CancellationTokenSource()
    waiter_result = Channel{Any}(1)
    waiter = Threads.@spawn begin
        try
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
                lock(cond)
                try
                    wait(cond)
                finally
                    unlock(cond)
                end
            end
            put!(waiter_result, :completed)
        catch e
            put!(waiter_result, e)
        end
    end
    # wait until the waiter is parked on the condition
    @test timedwait(10) do
        lock(cond)
        parked = !isempty(cond)
        unlock(cond)
        parked
    end === :ok
    # a holder keeps the condition lock while the cancellation is delivered
    held = Base.Event()
    release = Base.Event()
    holder = Threads.@spawn begin
        lock(cond)
        notify(held)
        wait(release; cancel=nothing)
        unlock(cond)
    end
    wait(held)
    Base.cancel!(src)
    # give the woken waiter time to reach the contended reacquire and park
    sleep(0.5)
    notify(release)
    wait(waiter)
    result = take!(waiter_result)
    @test result isa Base.CancellationRequest
    # the condition lock must be intact and uncontended afterwards
    @test trylock(cond.lock)
    unlock(cond.lock)
    wait(holder)
    # the waiter's registration(s) - including an entry orphaned when the
    # contended relock parked and cached a replacement - must not be
    # retained by the long-lived source: the next walk collects them
    @test timedwait(() -> istaskdone(waiter), 10.0) == :ok
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL) # escalation forces a walk
    @test isempty(registry_entries(src))
end

@testset "pre-cancelled ambient token refuses a spun-in lock acquisition" begin
    # the slow path can acquire by spinning when the holder releases during
    # the spin window; a resolved cancelled token must refuse that
    # acquisition just like the park path's refusal does
    deadsrc = CancellationTokenSource()
    cancel!(deadsrc)
    lk = ReentrantLock()
    lock(lk)
    t = with(CANCEL_TOKEN => CancellationToken(deadsrc)) do
        @async lock(lk)
    end
    spin() # let it reach the slow path while we hold the lock
    unlock(lk) # release during its spin/park window
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t) && t.result isa CancellationRequest
    # whichever path it took (spin-acquire refusal or park refusal), the
    # lock is not left held
    @test trylock(lk)
    unlock(lk)
end

@testset "WaitEntryN memory accounting and deepcopy" begin
    # variable-sized (like CancellationTokenSource): the type has no
    # definite size, instances charge their slot tail
    @test_throws ErrorException Core.sizeof(Core.WaitEntryN)
    @test Base.infer_return_type(Core.sizeof, Tuple{Core.WaitEntryN}) == Int
    base = Core.sizeof(Base.WaitEntryN(nothing, 0))
    slotsz = 2 * sizeof(Ptr{Cvoid}) + sizeof(UInt64)
    w = Base.WaitEntryN(nothing, 4)
    @test Core.sizeof(w) == base + 4 * slotsz
    @test Base.summarysize(w) == base + 4 * slotsz
    @test Base.summarysize(w; count=true) == 1
    # generic deepcopy would allocate only the fixed size and let the GC
    # scan a nonexistent tail; the custom method goes through the allocator
    d = deepcopy(w)
    @test d isa Core.WaitEntryN && d !== w
    @test Base._nslots(d) == 4
    @test all(i -> Base._slot_owner(d, i) === nothing, 1:4)
    GC.gc(true)
    # the task reference is kept (Task deepcopy is the identity); reached
    # through a containing object like any graph edge
    dv = deepcopy(Any[Base.WaitEntryN(current_task(), 2)])
    e = dv[1]::Core.WaitEntryN
    @test (@atomic :monotonic e.task) === current_task()
    @test Base._nslots(e) == 2
    GC.gc(true)
end

@testset "cancel keyword does not bypass specialized methods" begin
    # BitArray binary I/O keeps the packed format under a `cancel` call
    B = BitVector([isodd(i) for i in 1:129])
    plain = (io = IOBuffer(); write(io, B); take!(io))
    kw = (io = IOBuffer(); write(io, B; cancel=nothing); take!(io))
    @test kw == plain
    @test length(kw) == sizeof(B.chunks) # packed, not one byte per bit
    B2 = falses(129)
    @test read!(IOBuffer(kw), B2; cancel=nothing) == B
    @test B2 == B
    # a short stream still fails cleanly through the packed method
    @test_throws Union{EOFError, DimensionMismatch} read!(IOBuffer(kw[1:8]), falses(129); cancel=nothing)

    # vector-delimiter readuntil selects the vector method, not the scalar
    # catch-all (which would try to read Vector-typed values)
    @test readuntil(IOBuffer("abcXYdef"), b"XY"; cancel=nothing) == b"abc"
    @test readuntil(IOBuffer("abcXYdef"), b"XY"; keep=true, cancel=nothing) == b"abcXY"

    # IOStream methods accept `cancel` (entry check only)
    mktemp() do path, io
        write(io, "line1\nline2\n")
        flush(io); seekstart(io)
        @test readuntil(io, '\n'; cancel=nothing) == "line1"
        @test readline(io; cancel=nothing) == "line2"
        seekstart(io)
        @test read(io, 5; cancel=nothing) == b"line1"
        @test readbytes!(io, zeros(UInt8, 2); cancel=nothing) == 2
        @test !eof(io; cancel=nothing)
        @test readavailable(io; cancel=nothing) == b"ine2\n"
        @test eof(io; cancel=nothing)
    end

    # explicit-token writes to in-memory buffers keep working
    io = IOBuffer()
    @test write(io, "hi"; cancel=nothing) == 2
    @test write(io, UInt8[0x21]; cancel=nothing) == 1
    @test takestring!(io) == "hi!"

    # BufferStream specializations (BufferStream <: LibuvStream, whose
    # methods reference uv state a BufferStream does not have)
    bs = Base.BufferStream()
    write(bs, UInt8[0x01, 0x02, 0x03])
    @test readuntil(bs, 0x03; cancel=nothing) == UInt8[0x01, 0x02]
    write(bs, "xyz"; cancel=nothing)
    flush(bs; cancel=nothing)
    @test read(bs, UInt8; cancel=nothing) == UInt8('x')
    byte9 = UInt8[0x09]
    @test GC.@preserve(byte9, unsafe_write(bs, pointer(byte9), UInt(1); cancel=nothing)) == 1
    closewrite(bs; cancel=nothing)
    @test !eof(bs; cancel=nothing)
    @test read(bs; cancel=nothing) == UInt8['y', 'z', 0x09]
    @test eof(bs; cancel=nothing)
    # a parked BufferStream readuntil is cancellable through its token
    bs2 = Base.BufferStream()
    src = CancellationTokenSource()
    t = @async readuntil(bs2, 0x0a; cancel=CancellationToken(src))
    spin()
    cancel!(src)
    expect_cancelled(t)
    close(bs2)

    # compound-Pipe forwarders reach the endpoint methods
    p = linked_pipe()
    cancelled_src = CancellationTokenSource()
    cancel!(cancelled_src)
    ctok = CancellationToken(cancelled_src)
    @test_throws CancellationRequest eof(p; cancel=ctok)
    @test_throws CancellationRequest flush(p; cancel=ctok)
    @test_throws CancellationRequest read(p, UInt8; cancel=ctok)
    @test_throws CancellationRequest readavailable(p; cancel=ctok)
    @test write(p, UInt8['o', 'k']) == 2
    flush(p; cancel=nothing)
    @test read(p, UInt8; cancel=nothing) == UInt8('o')
    close(p)
end

@testset "explicit tokens and shields thread through call chains" begin
    deadsrc = CancellationTokenSource()
    cancel!(deadsrc)
    dead = CancellationToken(deadsrc)

    # (a) an explicit live token cancels the blocked operation while the
    # ambient scope stays clean
    c = Channel{Int}(0)
    src1 = CancellationTokenSource()
    t1 = @async take!(c; cancel=CancellationToken(src1))
    spin()
    cancel!(src1)
    expect_cancelled(t1)

    p = linked_pipe()
    src2 = CancellationTokenSource()
    t2 = @async read(p.out, String; cancel=CancellationToken(src2))
    spin()
    cancel!(src2)
    expect_cancelled(t2) # the inner read honors the operation's token

    # (b) `cancel = nothing` operations complete under a cancelled ambient
    # scope (the shield covers the preliminary lock, not just the wait)
    c2 = Channel{Int}(0)
    t3 = with(() -> @async(take!(c2; cancel=nothing)), CANCEL_TOKEN => dead)
    put!(c2, 42)
    @test fetch(t3) == 42

    e = Base.Event()
    t4 = with(() -> @async(begin wait(e; cancel=nothing); :ok end), CANCEL_TOKEN => dead)
    spin()
    notify(e)
    @test fetch(t4) === :ok

    sem = Base.Semaphore(1)
    with(CANCEL_TOKEN => dead) do
        Base.acquire(sem; cancel=nothing)
    end
    Base.release(sem)

    write(p.in, "hello\nrest")
    with(CANCEL_TOKEN => dead) do
        # readline's inner copyuntil calls run under the shield
        @test readline(p.out; cancel=nothing) == "hello"
        @test read(p.out, 4; cancel=nothing) == b"rest"
    end
    close(p)

    # a shielded run() under a cancelled ambient scope must spawn and
    # complete (the spawn primitive honors the resolved shield)
    with(CANCEL_TOKEN => dead) do
        proc = run(sleep_cmd(0); cancel=nothing)
        @test success(proc; cancel=nothing)
    end

    # (c) operations with an explicit live token do not throw from the
    # cancelled ambient scope
    with(CANCEL_TOKEN => dead) do
        live = CancellationToken(CancellationTokenSource())
        c3 = Channel{Int}(1)
        put!(c3, 7; cancel=live)
        @test fetch(c3; cancel=live) == 7
        @test take!(c3; cancel=live) == 7
        e2 = Base.Event()
        notify(e2)
        wait(e2; cancel=live)
    end
end

@testset "cancelled flush requeues unwritten bytes" begin
    p = linked_pipe()
    try
        Base.buffer_writes(p.in, 64)
        # a big direct write fills the OS pipe buffer and parks, so the
        # flush below queues behind it and cannot complete
        big = zeros(UInt8, BIG_WRITE)
        tw, srcw = cancellable(() -> write(p.in, big))
        @test timedwait(() -> parked_on(tw, p.in), 10.0) == :ok
        data = UInt8['a', 'b', 'c', 'd']
        write(p.in, data) # lands in the send buffer
        @test bytesavailable(p.in.sendbuf) == 4
        srcf = CancellationTokenSource()
        tf = @async flush(p.in; cancel=CancellationToken(srcf))
        @test timedwait(() -> parked_on(tf, p.in), 10.0) == :ok
        cancel!(srcf)
        # flush throws, but must not discard the bytes: the unwritten tail
        # is back in the send buffer for a later flush to retry
        expect_cancelled(tf)
        @test bytesavailable(p.in.sendbuf) == 4
        cancel!(srcw)
        @test_throws TaskFailedException wait(tw)
    finally
        close(p)
    end
end

@testset "cancelled recvfrom stops reception (no dropped datagram)" begin
    # bind the receiver to a known free port (found via listenany, like the
    # Sockets tests; retried in case another process grabs it in between)
    local udp, port
    for attempt in 1:10
        port, tcpserver = Sockets.listenany(Sockets.localhost, 0)
        close(tcpserver)
        udp = Sockets.UDPSocket()
        Sockets.bind(udp, Sockets.localhost, port) && break
        close(udp)
        attempt == 10 && error("could not bind a UDP test port")
    end
    src = CancellationTokenSource()
    t = @async Sockets.recvfrom(udp; cancel=CancellationToken(src))
    @test timedwait(() -> is_parked(t), 10.0) == :ok
    cancel!(src)
    expect_cancelled(t)
    # the cancelled waiter stopped continuous reception on unwind, so a
    # datagram arriving now stays in the kernel buffer instead of being
    # consumed-and-dropped by the callback ...
    sender = Sockets.UDPSocket()
    Sockets.send(sender, Sockets.localhost, port, UInt8['p', 'i', 'n', 'g'])
    spin(20)
    # ... and a fresh recvfrom receives it
    t2 = @async Sockets.recvfrom(udp)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test fetch(t2)[2] == b"ping"
    close(sender)
    close(udp)
end

const libccalltest = "libccalltest"

# A Julia foreign-call cancellation handler, C-callable via @cfunction. It
# runs like a signal handler on the thread executing the annotated call:
# no allocation, locks, yields or I/O.
function julia_cancelspin_handler(state::Ptr{Cvoid}, sev::UInt8)
    p = Ptr{Int64}(state)
    unsafe_store!(p, Int64(sev) + 1, 2)
    unsafe_store!(p, Int64(1), 1)
    nothing
end

# Handler deliveries are best-effort (skipped e.g. while one is already in
# flight, or when a suspend handshake holds the per-thread request slot),
# and a foreign spin never reaches the cancellation point that would
# recover a miss - so redeliver, mirroring the ^C ladder's level-triggered
# re-press. `cancel!` on an already-cancelled source re-runs its
# propagation walk and shootdown.
function wait_cancelled(t::Task, src::CancellationTokenSource; timeout::Float64 = 60.0)
    deadline = time() + timeout
    while timedwait(() -> istaskdone(t), 2.0) !== :ok && time() < deadline
        cancel!(src)
    end
    return istaskdone(t)
end

@testset "foreign-call cancellation handlers" begin
    lib = Libdl.dlopen("libccalltest")
    c_handler = Libdl.dlsym(lib, :cancelspin_handler)

    if Threads.nthreads(:default) >= 2
        # The handler runs on the thread blocked inside the annotated call:
        # it stops the foreign spin and the call returns. The annotation
        # itself implies no cancellation point - the pre-call @cancel_check
        # binds the governing source (which gates delivery), and the
        # post-call one is this caller's chosen way to observe the
        # delivered cancellation (so the partial result never escapes).
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
        # ... and saw severity SAFE (the handler records sev + 1)
        @test cell[][2] == Int64(CANCEL_REQUEST_SAFE.request) + 1

        # The same with a handler written in Julia (via @cfunction),
        # cancelled at an escalated severity, which the handler receives as
        # its argument.
        jh = @cfunction(julia_cancelspin_handler, Cvoid, (Ptr{Cvoid}, UInt8))
        cell2 = Ref((Int64(0), Int64(0)))
        t2, src2 = cancellable_spawn() do
            Base.@cancel_check
            r = @ccall cancel_handler=(jh, cell2) libccalltest.cancelspin_wait(cell2::Ref{NTuple{2, Int64}})::Int64
            Base.@cancel_check
            r
        end
        sleep(0.5)
        cancel!(src2, CANCEL_REQUEST_ABANDON_EXTERNAL)
        @test wait_cancelled(t2, src2)
        @test istaskfailed(t2)
        @test t2.result isa CancellationRequest
        @test cell2[][1] == 1
        @test cell2[][2] == Int64(CANCEL_REQUEST_ABANDON_EXTERNAL.request) + 1

        # Without a post-call check, an aborted call's (partial) result is
        # the caller's to interpret: the task completes normally with
        # whatever the foreign function returned after its handler-driven
        # early-out.
        cell4 = Ref((Int64(0), Int64(0)))
        t4, src4 = cancellable_spawn() do
            Base.@cancel_check
            @ccall cancel_handler=(c_handler, cell4) libccalltest.cancelspin_wait(cell4::Ref{NTuple{2, Int64}})::Int64
        end
        sleep(0.5)
        cancel!(src4)
        @test wait_cancelled(t4, src4)
        @test !istaskfailed(t4)
        @test fetch(t4) isa Int64
        @test cell4[][1] == 1
    end

    # A cancellation already pending at a caller-placed pre-call check
    # throws there: the foreign function is never entered and the handler
    # never runs. (This path is platform-independent.)
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

@testset "BigInt/GMP cancellation via reset regions" begin
    # No explicit cancellation points in the loop body: cancellation reaches
    # it through the annotated MPZ entry points - either their own
    # cancellation point, an asynchronous reset landing inside audited
    # libgmp compute (the reset region stays published across the annotated
    # call), or the allocation hooks republishing the region on exit.
    function bigmul_loop(nbits)
        b = big(3)^(nbits ÷ 2)
        m = big(10)^(nbits ÷ 8)
        while true
            b = (b * b) % m
        end
    end
    if Threads.nthreads(:default) >= 2
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
        # hammered by cancellation. Deliveries frequently race the
        # jl_gmp_counted_* hooks, exercising their unpublish/republish path;
        # correctness is "no crash, no corruption, clean arithmetic
        # afterwards".
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
end

@testset "cancellation lands inside audited libgmp compute" begin
    # A single long computation-carrying GMP call: the reset region
    # published by the annotated call's own cancellation point must survive
    # into the call (the point's binding bookkeeping - a task's first point
    # under a new source takes the write-barrier path - must come before the
    # region publication), and an asynchronous cancellation unwinds out of
    # the foreign computation promptly instead of waiting for it to finish.
    if Threads.nthreads(:default) >= 2
        started = Base.Event()
        t, src = cancellable_spawn() do
            notify(started)
            # ~tens of seconds of uninterrupted libgmp compute if not cancelled
            Base.GMP.MPZ.fac_ui(UInt(40_000_000))
        end
        wait(started)
        sleep(1.0) # let it get deep into the fac_ui call
        t0 = time()
        cancel!(src)
        @test wait_cancelled(t, src)
        @test istaskfailed(t)
        @test t.result isa CancellationRequest
        @test time() - t0 < 15.0 # cancelled promptly, not at call completion
        # the library stays healthy afterwards
        @test factorial(big(25)) == prod(big(1):big(25))
    end
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

@testset "BLAS cancellation via the cancel_handler annotation" begin
    # Requires an OpenBLAS with the cancellation extension. (This exercises
    # the raw mechanism without depending on any stdlib; LinearAlgebra's
    # BLAS wrappers will carry the same annotation.)
    blas = Libdl.dlopen_e("libopenblas64_")
    tok_f = blas == C_NULL ? C_NULL : Libdl.dlsym_e(blas, :openblas_cancel_token)
    if Threads.nthreads(:default) < 2
        @warn "BLAS cancellation tests need a second default-pool thread; skipping"
    elseif tok_f == C_NULL
        @warn "patched OpenBLAS (64-bit interface) not available; skipping BLAS cancellation tests"
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
        # baseline is long enough to cancel mid-flight (a large fixed size
        # burns minutes on an oversubscribed CI box).
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

## Structured cancellation of @sync / Threads.@threads scopes


@testset "structured cancellation of @sync" begin
    t, src = cancellable() do
        @sync begin
            @async sleep(1000)
            @async sleep(1000)
        end
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CompositeException
    @test length(t.result.exceptions) == 2
end

@testset "escalation during @sync teardown keeps awaiting internal tasks" begin
    # A SAFE cancellation parks the @sync teardown on a child that has no
    # cancellation points; an ABANDON_EXTERNAL escalation must re-arm that
    # wait - internal tasks are still awaited at ABANDON_EXTERNAL - rather
    # than unwind the @sync while the child is still running.
    stop = Ref(false)
    started = Base.Event()
    t, src = cancellable() do
        @sync begin
            @async begin
                notify(started)
                while !stop[]
                    yield() # no cancellation points: ignores SAFE/ABANDON_EXTERNAL
                end
            end
        end
    end
    wait(started)
    cancel!(src)
    spin(20)
    @test !istaskdone(t)
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    spin(20)
    @test !istaskdone(t)
    stop[] = true
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    req = t.result
    @test req isa CancellationRequest
    @test req.request == CANCEL_REQUEST_ABANDON_EXTERNAL.request
end

@testset "unfriendly cancellation of Experimental.@sync" begin
    # ABANDON_EXTERNAL propagates through the token tree to the children.
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t, src = cancellable() do
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test_throws TaskFailedException wait(t)
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])
end

@testset "structured cancellation of Experimental.@sync" begin
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t, src = cancellable() do
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # cancellation propagated to the children
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])
end

## ^C handling

# A deep compute kernel for the ^C subprocess scenarios.
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
end
eval(collatz_code)

@testset "unfriendly cancellation modes" begin
    # The delivered request carries its severity.
    seen = Ref{Any}(nothing)
    t, src = cancellable() do
        try
            sleep(1000)
        catch e
            seen[] = e
            rethrow()
        end
    end
    spin()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test seen[] === CANCEL_REQUEST_ABANDON_EXTERNAL

    # SAFE deliveries carry SAFE severity.
    seen2 = Ref{Any}(nothing)
    t2, src2 = cancellable() do
        try
            sleep(1000)
        catch e
            seen2[] = e
            rethrow()
        end
    end
    spin()
    cancel!(src2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test seen2[] === CANCEL_REQUEST_SAFE


    # ABANDON_EXTERNAL interrupts a blocked stream write without waiting for
    # the write's cancellation to complete.
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        big = zeros(UInt8, 200_000_000)
        tw, srcw = cancellable(() -> write(p, big))
        spin()
        cancel!(srcw, CANCEL_REQUEST_ABANDON_EXTERNAL)
        @test timedwait(() -> istaskdone(tw), 10.0) == :ok
        @test istaskfailed(tw)
        @test tw.result === CANCEL_REQUEST_ABANDON_EXTERNAL
    finally
        close(p)
    end
end

@testset "^C episode severity classification" begin
    src = CancellationTokenSource()
    @test Base.sigint_active_severity(src) === nothing
    @test cancel!(src)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_SAFE
    @test cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test cancel!(src, CANCEL_REQUEST_ABANDON_ALL)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_ALL
    # severities never de-escalate
    @test !cancel!(src, CANCEL_REQUEST_SAFE)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_ALL
end


Sys.isunix() && @testset "^C" begin
    # Children run the bare executable with default flags, NOT julia_cmd():
    # inherited suite flags (e.g. --check-bounds=yes) invalidate the
    # sysimage's native code, and these scenarios assert interactive ^C
    # semantics, not the flag matrix.
    exe = joinpath(Sys.BINDIR, Base.julia_exename())
    function run_with_sigint(code::String, delays;
                             open_stdin::Bool=false, threads::Int=0)
        # A readiness marker printed from user code proves the runtime is up
        # (signal handling armed, the script started) before any SIGINT is
        # sent - on a loaded machine startup alone can outlast the first delay
        # and an early SIGINT kills the child with no output at all.
        code = "println(\"CHILD-READY\")\n" * code
        out = Pipe()
        cmd = threads > 0 ?
            `$exe --startup-file=no --threads=$threads -e $code` :
            `$exe --startup-file=no -e $code`
        inpipe = open_stdin ? Pipe() : devnull
        p = run(pipeline(cmd, stdin=inpipe, stdout=out, stderr=out), wait=false)
        close(out.in)
        open_stdin && close(inpipe.out)
        readuntil(out, "CHILD-READY\n") # returns early (at EOF) if the child dies
        reader = @async read(out, String)
        killer = @async begin
            for d in delays
                sleep(d)
                process_running(p) && kill(p, Base.SIGINT)
            end
        end
        wait(p)
        open_stdin && close(inpipe.in)
        wait(killer)
        return fetch(reader), p
    end

    # Catching ^C in a script: continuing requires re-arming a fresh ^C
    # epoch (the script's cancelled scope stays cancelled otherwise)
    output, p = run_with_sigint("""
        try
            sleep(100)
            println("FAIL: not cancelled")
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println("caught: ", typeof(e))
                println("continued")
                sleep(0.1) # cancellable operations work again
            end
        end
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
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println(typeof(e))
            end
        end
        """, [1.5])
    @test occursin("CompositeException", output)
    @test p.exitcode == 0

    # ^C with a stray @async task pending is catchable and the script exits
    # cleanly - historically a "fatal: error thrown and no exception handler
    # available" (issues #29369, #45055)
    output, p = run_with_sigint("""
        @async println("Hello!")
        try
            println("Hit ctrl-c!")
            sleep(10)
        catch err
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                showerror(stdout, err); println()
                println("done")
            end
        end
    """, [1.0])
    @test occursin("Hello!", output)
    @test occursin("CancellationRequest", output)
    @test occursin("done", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 0

    # ^C during a blocked read from stdin reports and exits - historically a
    # fatal unhandled InterruptException on the second press (issue #43451)
    output, p = run_with_sigint("read(stdin)", [1.0]; open_stdin=true)
    @test occursin("CancellationRequest", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 1

    # A rapid second press while the first cancellation is still unwinding
    # or reporting must not crash the process (issue #50045). The second
    # press may cancel the error-report epoch itself, in which case the
    # fallback note appears instead of the report.
    output, p = run_with_sigint("sleep(100)", [1.0, 0.1])
    @test occursin("CancellationRequest", output) ||
        occursin("displaying the error report failed", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 1

    # ^C stops a swarm of print-flooding tasks and the script continues
    # (issue #47839)
    output, p = run_with_sigint("""
        ts = [@async (while true; println("hi"); end) for _ in 1:20]
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                for t in ts
                    try; wait(t); catch; end
                end
                println("ALL-STOPPED")
            end
        end
    """, [1.5])
    @test occursin("ALL-STOPPED", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 0

    # ^C on a Threads.@threads loop raises a catchable CompositeException
    # instead of killing the process (issue #56462)
    output, p = run_with_sigint("""
        try
            Threads.@threads for i in 1:8
                sleep(100)
            end
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println("caught: ", typeof(e))
                println("session-alive")
            end
        end
    """, [1.5]; threads=4)
    @test occursin("caught: CompositeException", output)
    @test occursin("session-alive", output)
    @test !occursin("fatal", output)
    @test !occursin("attempt to switch to exited task", output)
    @test p.exitcode == 0

    # A watcher task on the ^C episode token is the supported shape for a
    # user-defined interrupt handler (superseding the design of #49541): the
    # ^C completes - rather than unwinds - its wait, and its reaction runs
    # under its own shielded scope
    output, p = run_with_sigint("""
        tok = Base.CANCEL_TOKEN[]
        w = Threads.@spawn Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
            req = wait(tok)
            println("HANDLER-RAN ", typeof(req))
        end
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                wait(w)
                println("DONE")
            end
        end
    """, [1.0])
    @test occursin("HANDLER-RAN Base.CancellationRequest", output)
    @test occursin("DONE", output)
    @test p.exitcode == 0
end


# Real console-^C coverage on Windows: CTRL_C_EVENT can only target process
# group 0 ("every process on this console"), so the scenario gets a console
# of its own - a detached host (no console) allocates a fresh one, spawns
# the victim into it, ignores ^C itself (only AFTER the spawn: the ignore
# flag is inherited), and generates the real event. Pipes are handle-based
# and work independently of consoles.
Sys.iswindows() && @testset "^C via the Windows console" begin
    host_script = raw"""
        victim_code = ARGS[1]  # already carries the CHILD-READY marker
        delays = parse.(Float64, split(ARGS[2], ","))
        # Detach from any inherited (possibly hidden - libuv spawns piped
        # children with CREATE_NO_WINDOW) console before allocating the
        # scenario's own.
        ccall(:FreeConsole, stdcall, Int32, ())
        @assert ccall(:AllocConsole, stdcall, Int32, ()) != 0
        # detach()'s CREATE_NEW_PROCESS_GROUP set this process's inherited
        # Ctrl-C-ignore flag; clear it BEFORE spawning the victim (the flag
        # is inherited).
        @assert ccall(:SetConsoleCtrlHandler, stdcall, Int32, (Ptr{Cvoid}, Int32), C_NULL, 0) != 0
        out = Pipe()
        exe = joinpath(Sys.BINDIR, Base.julia_exename())
        cmd = `$exe --startup-file=no -e $victim_code`
        p = run(pipeline(cmd, stdin=devnull, stdout=out, stderr=out), wait=false)
        close(out.in)
        reader = @async read(out, String)
        readuntil(out, "CHILD-READY\n")
        # Ignore ^C in the host - only now, so the victim did not inherit
        # the ignore flag.
        @assert ccall(:SetConsoleCtrlHandler, stdcall, Int32, (Ptr{Cvoid}, Int32), C_NULL, 1) != 0
        for d in delays
            sleep(d)
            if Base.process_running(p)
                # CTRL_C_EVENT (0) to group 0: everyone on our fresh console
                @assert ccall(:GenerateConsoleCtrlEvent, stdcall, Int32, (UInt32, UInt32), 0x00000000, 0x00000000) != 0
            end
        end
        wait(p)
        print(stdout, fetch(reader))
        # last line: the victim's exit code, for the outer test to parse
        print(stdout, "\nVICTIM-EXIT=", p.exitcode)
        """
    function run_with_console_ctrl_c(code::String, delays)
        # the readiness marker proves the victim's runtime is up before the
        # first event is generated (see the unix testset)
        code = "println(\"CHILD-READY\")\n" * code
        # bare executable for the host (and, transitively, the victim):
        # suite flags would put both in recompile-everything mode
        exe = joinpath(Sys.BINDIR, Base.julia_exename())
        cmd = `$exe --startup-file=no -e $host_script $code $(join(delays, ","))`
        out = Pipe()
        p = run(pipeline(detach(cmd), stdin=devnull, stdout=out, stderr=out), wait=false)
        close(out.in)
        output = read(out, String)
        wait(p)
        m = match(r"VICTIM-EXIT=(-?\d+)\s*$", output)
        @test m !== nothing
        return output, m === nothing ? -1 : parse(Int, m.captures[1])
    end

    # a real console ^C is delivered as a cancellation and is catchable;
    # re-arming a fresh episode lets the script continue
    output, exitcode = run_with_console_ctrl_c("""
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println("caught: ", typeof(e))
                println("continued")
            end
        end
    """, [1.0])
    @test occursin("caught: Base.CancellationRequest", output)
    @test occursin("continued", output)
    @test exitcode == 0

    # an uncaught console ^C produces the standard error report and exit code
    output, exitcode = run_with_console_ctrl_c("sleep(100)", [1.0])
    @test occursin("CancellationRequest", output)
    @test !occursin("fatal", output)
    @test exitcode == 1
end

Sys.isunix() && @testset "^C in the REPL (pty)" begin
    isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
    pts, ptm = Main.FakePTYs.open_fake_pty()

    # Interactive julia on the pty; drive it like a user pressing ^C.
    env = copy(ENV)
    env["TERM"] = "dumb"
    env["JULIA_HISTORY"] = tempname()
    # bare executable: see the "^C" testset's note on inherited suite flags
    exe = joinpath(Sys.BINDIR, Base.julia_exename())
    p = run(detach(setenv(`$exe -i -q --startup-file=no --color=no`, env)),
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
    # generous default: the first error display JIT-compiles the whole
    # stacktrace-printing path, which can take a long time on slow hosts
    function expect(needle::String; timeout::Real=120.0)
        status = timedwait(timeout; pollint=0.05) do
            idx = findnext(needle, snapshot(), cursor[])
            idx === nothing && return false
            cursor[] = last(idx) + 1
            return true
        end
        if status !== :ok
            if process_running(p)
                # collect diagnostics into the CI log: SIGQUIT makes the
                # session dump all task backtraces onto the pty and exit
                kill(p, 3) # SIGQUIT
                timedwait(() -> istaskdone(reader), 20.0) # pty EOF: dump drained
            end
            @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
        end
        @test status == :ok
    end
    sendline(s) = write(ptm, s * "\n")

    expect("julia> ")

    # a SIGINT at an idle prompt (^C or an external `kill -INT`) must
    # not disturb the session (issue #42072)
    kill(p, Base.SIGINT)
    sleep(0.5)
    sendline("20 + 21")
    expect("41")
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

    # a background task from an earlier evaluation belongs to an earlier
    # ^C epoch: interrupting the current evaluation leaves it running
    # (issue #25790)
    sendline("global bgc = Ref(0); global bg = @async while true; sleep(0.01); bgc[] += 1; end; println(\"BG-UP\")")
    expect("BG-UP")
    expect("julia> ")
    sendline("println(\"EVAL-4\"); sleep(1000)")
    expect("EVAL-4")
    sleep(0.5)
    kill(p, Base.SIGINT)
    expect("CancellationRequest")
    expect("julia> ")
    sendline("print(\"bg-done=\", istaskdone(bg)); c0 = bgc[]; sleep(0.3); println(\"; bg-alive=\", bgc[] > c0)")
    expect("bg-done=false; bg-alive=true")
    expect("julia> ")

    # ^C during an in-evaluation terminal read recovers the prompt
    # (the class of issue #58105's "Install package?" prompt)
    sendline("println(\"EVAL-5\"); readline()")
    expect("EVAL-5")
    sleep(0.5)
    kill(p, Base.SIGINT)
    expect("CancellationRequest")
    expect("julia> ")

    # ^C while parked in a server accept recovers, leaving the server
    # usable (the class of issue #58689)
    sendline("using Sockets; global srv = listen(Sockets.localhost, 0); println(\"LISTENING\"); accept(srv)")
    expect("LISTENING")
    sleep(0.5)
    kill(p, Base.SIGINT)
    expect("CancellationRequest")
    expect("julia> ")
    sendline("println(\"srv-open=\", isopen(srv)); close(srv)")
    expect("srv-open=true")
    expect("julia> ")

    # cancelling a BigInt computation never yanks control out of libgmp in
    # an unsafe spot the way the old asynchronous InterruptException
    # delivery could (corrupting the heap - issue #56545): the loop is
    # deliberately checkless, so delivery lands on an MPZ entry point's own
    # cancellation point, inside audited libgmp compute (unwound via the
    # published reset region), or when an allocation hook republishes the
    # region on exit - and BigInt arithmetic in the session works correctly
    # afterwards
    sendline("println(\"EVAL-6\"); let b = big(3); while true; b = b*b % (big(10)^200); end; end")
    expect("EVAL-6")
    sleep(0.5)
    kill(p, Base.SIGINT)
    expect("CancellationRequest")
    expect("julia> ")
    sendline("println(string(factorial(big(30))))")
    expect("265252859812191058636308480000000")
    expect("julia> ")

    sendline("exit()")
    # Never let success(p) hang the suite on a wedged session.
    if timedwait(() -> process_exited(p), 60.0) !== :ok
        kill(p, Base.SIGKILL)
    end
    @test success(p)
    close(ptm)
    wait(reader)
end

Sys.isunix() && @testset "SIGTERM graceful termination" begin
    # Bare executable, as in the "^C" testset: these scenarios assert the
    # termination semantics, not the suite's flag matrix.
    exe = joinpath(Sys.BINDIR, Base.julia_exename())
    function run_with_sigterm(code::String, delays)
        # The readiness marker proves the runtime is up before the first
        # SIGTERM is sent (see the "^C" testset's run_with_sigint).
        code = "println(\"CHILD-READY\")\n" * code
        out = Pipe()
        p = run(pipeline(`$exe --startup-file=no -e $code`,
                         stdin=devnull, stdout=out, stderr=out), wait=false)
        close(out.in)
        readuntil(out, "CHILD-READY\n") # returns early (at EOF) if the child dies
        reader = @async read(out, String)
        killer = @async begin
            for d in delays
                sleep(d)
                process_running(p) && kill(p) # SIGTERM
            end
        end
        exited = timedwait(() -> process_exited(p), 60.0)
        exited === :ok || kill(p, Base.SIGKILL) # never wedge the suite
        wait(p)
        wait(killer)
        return fetch(reader), p, exited
    end

    # SIGTERM cancels the process root source: the parked sleep unwinds
    # quietly (no error report), the atexit hooks run in the ordinary exit
    # path, and the process still dies by SIGTERM (the runtime re-raises it
    # at the end of the teardown) - issue #62774
    output, p, exited = run_with_sigterm("""
        atexit(() -> println("ATEXIT-RAN"))
        sleep(100)
        println("FAIL: not terminated")
    """, [0.5])
    @test exited === :ok
    @test occursin("ATEXIT-RAN", output)
    @test !occursin("FAIL", output)
    @test !occursin("CancellationRequest", output)
    @test p.termsignal == Base.SIGTERM

    # `finally` blocks of the governed work run before the exit (shielded,
    # like any cleanup that must still block - here: write to a pipe - under
    # its own cancelled scope)
    output, p, exited = run_with_sigterm("""
        try
            sleep(100)
        finally
            Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
                println("FINALLY-RAN")
            end
        end
    """, [0.5])
    @test exited === :ok
    @test occursin("FINALLY-RAN", output)
    @test p.termsignal == Base.SIGTERM

    # cleanup code can distinguish a terminating process from an ordinary
    # cancellation - and catching the request does not avert the exit
    output, p, exited = run_with_sigterm("""
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
                println("terminating=", Base.process_terminating())
                println("terminate-request=",
                        e isa Base.CancellationRequest && Base.is_terminate_request(e))
            end
        end
    """, [0.5])
    @test exited === :ok
    @test occursin("terminating=true", output)
    @test occursin("terminate-request=true", output)
    @test p.termsignal == Base.SIGTERM

    # work shielded from cancellation cannot be unwound by the graceful
    # request; a repeat SIGTERM exits abruptly instead
    output, p, exited = run_with_sigterm("""
        Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
            sleep(100)
        end
    """, [0.5, 3.0])
    @test exited === :ok
    @test p.termsignal == Base.SIGTERM

    # a SIGTERM at any point during startup must terminate the process:
    # before Base registers the root source it exits abruptly, afterwards
    # through the graceful path - it must never wedge (the failure mode of
    # issue #62774: a teardown hijacked onto a half-initialized thread 0
    # deadlocking on runtime locks)
    for delay in (0.0, 0.05, 0.2)
        p = run(pipeline(`$exe --startup-file=no -e 'sleep(60)'`,
                         stdin=devnull, stdout=devnull, stderr=devnull), wait=false)
        delay > 0 && sleep(delay)
        process_running(p) && kill(p)
        exited = timedwait(() -> process_exited(p), 60.0)
        exited === :ok || kill(p, Base.SIGKILL)
        wait(p)
        @test exited === :ok
        @test p.termsignal == Base.SIGTERM || p.exitcode == 128 + Base.SIGTERM
    end
end

Sys.isunix() && @testset "SIGTERM in the REPL (pty)" begin
    isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")

    # Interactive julia on a pty (like the "^C in the REPL" testset); the
    # session must shut down cleanly on SIGTERM, dying by the signal.
    function repl_sigterm_session(drive!::Function)
        pts, ptm = Main.FakePTYs.open_fake_pty()
        env = copy(ENV)
        env["TERM"] = "dumb"
        env["JULIA_HISTORY"] = tempname()
        exe = joinpath(Sys.BINDIR, Base.julia_exename())
        p = run(detach(setenv(`$exe -i -q --startup-file=no --color=no`, env)),
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
        snapshot() = @lock transcript_lock String(copy(transcript))
        cursor = Ref(1)
        function expect(needle::String; timeout::Real=120.0)
            status = timedwait(timeout; pollint=0.05) do
                idx = findnext(needle, snapshot(), cursor[])
                idx === nothing && return false
                cursor[] = last(idx) + 1
                return true
            end
            if status !== :ok && process_running(p)
                kill(p, 3) # SIGQUIT: dump task backtraces into the CI log
                timedwait(() -> istaskdone(reader), 20.0)
                @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
            end
            @test status == :ok
        end
        sendline(s) = write(ptm, s * "\n")
        expect("julia> ")
        drive!(p, expect, sendline)
        kill(p) # SIGTERM
        exited = timedwait(() -> process_exited(p), 60.0)
        exited === :ok || kill(p, Base.SIGKILL)
        wait(p)
        @test exited === :ok
        @test p.termsignal == Base.SIGTERM
        close(ptm)
        wait(reader)
    end

    # SIGTERM at an idle prompt: the frontend's terminal read unwinds and
    # the session exits
    repl_sigterm_session() do p, expect, sendline
    end

    # SIGTERM during an evaluation: the evaluation unwinds (with no error
    # report) and the session exits
    repl_sigterm_session() do p, expect, sendline
        sendline("println(\"EVAL-1\"); sleep(1000)")
        expect("EVAL-1")
        sleep(0.5)
    end
end
