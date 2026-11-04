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
