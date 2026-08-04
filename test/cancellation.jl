# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: cancel!, CancellationRequest, CancellationToken, CancellationTokenSource,
    CANCEL_REQUEST_SAFE, CANCEL_REQUEST_ABANDON_EXTERNAL, CANCEL_REQUEST_ABANDON_ALL,
    CANCEL_TOKEN
using Base.ScopedValues: with, ScopedValue

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
        try
            wait(inner)
        catch
        end
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
