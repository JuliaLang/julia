# This file is a part of Julia. License is MIT: https://julialang.org/license

## Cancellation tokens
#
# Cancellation is organized around *cancellation token sources*
# (`Core.CancellationTokenSource`): level-triggered condition nodes arranged
# in a tree. Cancelling a source cancels its whole subtree; the cancelled
# state is monotonic (severities only escalate, never reset) and waiters that
# register after cancellation are woken immediately. Following the .NET
# split, a `CancellationToken` is the observe/wait view handed to code that
# may only *react* to cancellation; the source is the capability to *request*
# it.
#
# The token governing a piece of code is carried dynamically as a scoped
# value (see `CANCEL_TOKEN`): blocking APIs default their `cancel` keyword
# argument to the scoped token, and `@cancel_check` resolves it at every
# check. Tasks inherit their creating task's scope, so cancellation scopes
# propagate to child tasks without explicit plumbing.
#
# TODO(compiler): a scoped-value lookup is a `Core.current_scope()` read plus
# a persistent-dict (HAMT) lookup. The optimizer currently only folds
# `current_scope()` when the enclosing `@with` is visible in the same
# (post-inlining) frame; for inherited scopes the lookup stays in hot loops.
# Teaching the compiler to CSE/hoist `current_scope()` + `KeyValue.get` for
# inherited scopes (sound: the scope is enter/leave-balanced and
# task-private) would make per-iteration `@cancel_check` in tight loops
# cheap. Until then, hot loops can hoist manually via the
# `@cancel_check tok` form.

const CancellationTokenSource = Core.CancellationTokenSource

"""
    CancellationToken(src::CancellationTokenSource)

The observe side of a [`CancellationTokenSource`](@ref): code holding a
token can be interrupted by - and can query ([`iscancelled`](@ref)) -
cancellation of the associated source, but cannot request cancellation
itself. Only the holder of the *source* can call [`cancel!`](@ref).

A token takes effect in one of two ways: pass it as the `cancel` keyword
argument of a specific blocking operation, or scope it over a whole
computation via the [`CANCEL_TOKEN`](@ref) scoped value (spawned tasks
inherit it). Either way, once the source is cancelled the affected
operations throw a [`CancellationRequest`](@ref).

See the manual chapter on [Task Cancellation](@ref man-cancellation) for an
overview.
"""
struct CancellationToken
    source::CancellationTokenSource
end

"""
    CancellationRequest

The exception thrown by blocking operations and cancellation points whose
governing cancellation token has been cancelled. The `request` field records
the severity
([`CANCEL_REQUEST_SAFE`](@ref), [`CANCEL_REQUEST_ABANDON_EXTERNAL`](@ref) or
[`CANCEL_REQUEST_ABANDON_ALL`](@ref)) as observed at delivery time; the
source may escalate afterwards.
"""
struct CancellationRequest
    request::UInt8
end

"""
    CANCEL_REQUEST_SAFE

Request safe cancellation. Code observing the cancellation will request safe
cancellation of any resources it is waiting for and wait for the cancellation
of such resources to be completed.

As a result, if either the cancelled code or any of its dependent resources
are currently unable to process cancellation, the request may hang and a more
aggressive cancellation severity may be required. However, in general _SAFE
should be tried first.
"""
const CANCEL_REQUEST_SAFE = CancellationRequest(0x0)

"""
    CANCEL_REQUEST_ABANDON_EXTERNAL

Request a cancellation that will cease waiting for any external resources
(e.g. I/O objects) without going through a safe cancellation procedure for
such resources. However, internal computational tasks are still awaited.

This is a middleground between CANCEL_REQUEST_SAFE and
CANCEL_REQUEST_ABANDON_ALL, as external I/O is often engineered for
robustness in case of sudden disappearance of peers.
"""
const CANCEL_REQUEST_ABANDON_EXTERNAL = CancellationRequest(0x3)

"""
    CANCEL_REQUEST_ABANDON_ALL

Request a cancellation that will cease waiting for all external resources,
and give up on tasks that have not responded to the cancellation: they are
frozen in place and never scheduled again.

!!! warning
    If any cancelled task has acquired locks or other resources that are
    contested, this method of cancellation may leak such resources and create
    deadlocks in future code. It is intended as a last-resort method to
    recover a system, but the necessity of this operation should in general
    be considered a bug (e.g. due to insufficient cancellation points in
    computationally-heavy code).
"""
const CANCEL_REQUEST_ABANDON_ALL = CancellationRequest(0x4)

# The state byte of a cancelled source is (STATE_CANCELLED_BIT | severity);
# status bytes returned by `Core.cancellation_point!` additionally use
# STATUS_PREEMPT_BIT to report a pending cooperative-yield request.
const STATE_CANCELLED_BIT = 0x80
const STATUS_PREEMPT_BIT = 0x40
const SEVERITY_MASK = 0x3f

severity(cr::CancellationRequest) = cr.request & SEVERITY_MASK

"""
    cancel_severity(src::CancellationTokenSource) -> Union{Nothing, CancellationRequest}
    cancel_severity(tok::CancellationToken)

Return `nothing` if the source has not been cancelled, or a
`CancellationRequest` recording the current (monotonically escalating)
severity if it has.
"""
function cancel_severity(src::CancellationTokenSource)
    st = @atomic :acquire src.state
    st == 0x00 && return nothing
    return CancellationRequest(st & SEVERITY_MASK)
end
cancel_severity(tok::CancellationToken) = cancel_severity(tok.source)

"""
    iscancelled(src::CancellationTokenSource)::Bool
    iscancelled(tok::CancellationToken)::Bool

Whether the source has been cancelled (level-triggered: once cancelled, a
source stays cancelled).
"""
iscancelled(src::CancellationTokenSource) = (@atomic :monotonic src.state) != 0x00
iscancelled(tok::CancellationToken) = iscancelled(tok.source)

## Source construction and tree linkage


@eval function _new_cancel_source(parents::Union{Nothing, CancellationTokenSource, Core.SimpleVector})
    return $(Expr(:new, :CancellationTokenSource,
                  :parents, nothing, nothing, nothing,
                  0x00, 0x00, 0x00, nothing))
end

"""
    CancellationTokenSource() -> CancellationTokenSource
    CancellationTokenSource(parents::CancellationToken...)

Create a new cancellation token source. With no arguments the source is a
standalone root; given one or more parent tokens, the new source is linked
underneath each of them, so that cancellation of *any* parent (or any of its
ancestors) also cancels the new source - at the highest severity requested
among them. Sources therefore form a directed acyclic graph; a source
created under an already-cancelled parent is born cancelled. Linking one
source under several parents is how an operation respects two independent
lifetimes at once (say, a request scope and the connection it arrived on).

A child source stays linked to its parents for exactly as long as it is
reachable - a held token, waiting or running work governed by it all keep it
alive. Once nothing can observe it any more, it is garbage collected and
thereby drops out of the graph; there is no explicit detach operation.

Use [`CancellationToken`](@ref)`(src)` for the observe/wait view, and
[`cancel!`](@ref)`(src)` to request cancellation.
"""
function CancellationTokenSource(parent::CancellationToken)
    src = _new_cancel_source(parent.source)
    attach_child!(parent.source, src)
    return src
end
function CancellationTokenSource(parent::CancellationToken, rest::CancellationToken...)
    srcs = CancellationTokenSource[parent.source]
    for tok in rest
        any(s -> s === tok.source, srcs) || push!(srcs, tok.source)
    end
    length(srcs) == 1 && return CancellationTokenSource(parent)
    child = _new_cancel_source(Core.svec(srcs...))
    for p in srcs
        attach_child!(p, child)
    end
    return child
end
CancellationTokenSource(::Nothing) = _new_cancel_source(nothing)
CancellationTokenSource() = _new_cancel_source(nothing)

# Spinlock on a source's `_lock` byte. These are leaf locks: no code may
# block, allocate unboundedly, or take another source's lock while holding
# one, except for the parent->child order used by the cancellation walk.
function _lock_source(src::CancellationTokenSource)
    while true
        (@atomicreplace :acquire :monotonic src._lock 0x00 => 0x01).success && return
        while (@atomic :monotonic src._lock) != 0x00
            ccall(:jl_cpu_suspend, Cvoid, ())
            ccall(:jl_gc_safepoint, Cvoid, ())
        end
    end
end
_unlock_source(src::CancellationTokenSource) = (@atomic :release src._lock = 0x00; nothing)

# Link `child` under `parent`. The child inherits the parent's cancellation
# state at link time (a node attached under an already-cancelled parent is
# born cancelled) - together with `cancel!` marking each node *before*
# draining it, this makes registration level-triggered under concurrent
# cancellation.
function attach_child!(parent::CancellationTokenSource, child::CancellationTokenSource)
    wr = WeakRef(child) # allocated outside the spinlock
    _lock_source(parent)
    pst = @atomic :monotonic parent.state
    if pst != 0x00
        _raise_state!(child, pst & SEVERITY_MASK)
    end
    kids = parent.children
    if kids === nothing
        kids = WeakRef[]
        parent.children = kids
    else
        kids = kids::Vector{WeakRef}
        # Amortized pruning: whenever the list length reaches a power of two,
        # drop entries whose child has been garbage collected. This bounds
        # the list at roughly twice the live-child count with O(1) amortized
        # attach cost, without any explicit detach operation.
        n = length(kids)
        if n >= 8 && (n & (n - 1)) == 0
            filter!(w -> w.value !== nothing, kids)
        end
    end
    push!(kids, wr)
    _unlock_source(parent)
    return child
end

# CAS-max the source's state to (STATE_CANCELLED_BIT | sev). Returns true if
# the state was raised, false if it was already at (or above) the severity.
function _raise_state!(src::CancellationTokenSource, sev::UInt8)
    old = @atomic :monotonic src.state
    while true
        if old != 0x00 && (old & SEVERITY_MASK) >= sev
            return false
        end
        old, success = @atomicreplace :acquire_release :monotonic src.state old => (STATE_CANCELLED_BIT | sev)
        success && return true
    end
end

## The per-task wait state (the "wait node", folded into `Task`)
#
# Every task carries the two halves of a cancellable wait as dedicated
# fields (a task only ever waits on one thing at a time):
#  - the *edge* half: intrusive `wait_next`/`wait_queue` links that enqueue
#    the task in a waitee's wait queue (a `WaitQueue`), where a completion
#    (`notify`, a libuv callback, task termination) finds it;
#  - the *level* half: intrusive `wait_tnext`/`wait_tprev` links that
#    register the task in a cancellation token source's waiter list, where
#    the cancellation walk finds it.
# These are deliberately distinct from the scheduler's `next`/`queue` links:
# a canceller claims a parked waiter and schedules it *without* holding the
# waitee's lock, so the task sits in a workqueue while its wait-queue entry
# is still linked (stale entries are unlinked lazily).
# The `wait_state` byte arbitrates the wake: whoever CASes WAITING ->
# (NOTIFIED | CANCELLED) owns waking the task; the loser does nothing.
# Steady-state blocking waits allocate nothing.
#
# Lifecycle invariant: the *waiter* always unlinks itself from both halves
# (under the respective locks) before leaving the wait function. The
# cancellation walk unlinks from the waiter list when it claims a task;
# `notify`/completions unlink from the wait queue when they pop one. Both
# unlink operations tolerate the entry already being gone.

const WAITNODE_IDLE      = 0x00
const WAITNODE_WAITING   = 0x01  # enqueued; wake not yet claimed
const WAITNODE_NOTIFIED  = 0x02  # completion claimed the wake
const WAITNODE_CANCELLED = 0x03  # cancellation claimed the wake

## Delivery semantics
#
# Cancellation is uniformly level-triggered: while the governing token is
# cancelled, every cancellation point and every blocking-operation entry
# check throws the `CancellationRequest`. There is no per-task
# acknowledgement state; cleanup code that must block under a cancelled
# scope explicitly shields itself (`cancel = nothing`, or scoping
# `CANCEL_TOKEN => nothing` over a block), and the interactive machinery re-arms
# with a *fresh* episode source between epochs (see `sigint_new_episode!`),
# detaching any still-unwinding work from the ^C target.

# Record that a cancellation of `src` at severity `sev` was delivered to
# (observed by) some task: either thrown at one of its cancellation points,
# or handed to it by the cancellation walk waking its parked wait. Feeds the
# ^C episode state machine ("was the request ever seen?"), so the bits are
# propagated to every ancestor: a delivery against a nested scope's source
# acknowledges the episode source too - otherwise the SIGINT classifier
# would misread a successfully delivered cancellation as unacknowledged and
# escalate a repeat ^C to abandonment.
function _mark_delivered!(src::CancellationTokenSource, sev::UInt8)
    bit = 0x01 << sev
    # fast path: no parents (the common episode-source case)
    @atomic :monotonic src.delivered |= bit
    p = src.parents
    p === nothing && return nothing
    # Iterative ancestor walk (a Vector worklist - deep chains must not
    # recurse - deduplicated so a reconverging linked graph is marked once
    # per node). Delivery is rare; the allocation is fine.
    pending = CancellationTokenSource[]
    seen = IdSet{CancellationTokenSource}()
    push!(seen, src)
    _push_parents!(pending, seen, p)
    while !isempty(pending)
        node = pop!(pending)
        @atomic :monotonic node.delivered |= bit
        np = node.parents
        np === nothing || _push_parents!(pending, seen, np)
    end
    return nothing
end

function _push_parents!(pending::Vector{CancellationTokenSource},
                        seen::IdSet{CancellationTokenSource}, @nospecialize(p))
    if p isa CancellationTokenSource
        if !(p in seen)
            push!(seen, p)
            push!(pending, p)
        end
    elseif p isa Core.SimpleVector
        for i in 1:length(p)
            c = p[i]
            c isa CancellationTokenSource || continue
            if !(c in seen)
                push!(seen, c)
                push!(pending, c)
            end
        end
    end
    return nothing
end

# The severity of the current dynamic scope's cancellation, or `nothing` if
# the scope is not cancelled (or there is no scoped token).
function ambient_cancel_severity()
    src = default_cancel_source()
    src === nothing && return nothing
    return cancel_severity(src)
end

# Whether the current dynamic scope was cancelled at a severity that directs
# it to abandon external (I/O) waits without safe teardown. Shielded
# external teardown consults this to skip waiting for completions entirely.
function abandoning_external_waits(t::Task=current_task())
    sev = ambient_cancel_severity()
    return sev !== nothing && sev.request >= CANCEL_REQUEST_ABANDON_EXTERNAL.request
end

## Waiter registration (level half)

"""
    register_cancellation!(src::CancellationTokenSource, w::Task;
                           min_severity::UInt8=0x00)::Bool

Register the waiting task `w` on `src`'s waiter list so that cancellation
of `src` (or an ancestor) wakes it. Returns `false` - without registering - if `src`
is already cancelled at a severity `>= min_severity` (level trigger for late
registrants: the caller must deliver the cancellation instead of parking).
"""
function register_cancellation!(src::CancellationTokenSource, w::Task;
                                min_severity::UInt8=0x00)
    _lock_source(src)
    st = @atomic :monotonic src.state
    if st != 0x00
        sev = st & SEVERITY_MASK
        if sev >= min_severity
            _unlock_source(src)
            return false
        end
    end
    w.wait_token = src
    w.wait_min_severity = min_severity
    w.wait_tprev = src.waiters_tail
    w.wait_tnext = nothing
    if src.waiters_tail === nothing
        src.waiters_head = w
    else
        (src.waiters_tail::Task).wait_tnext = w
    end
    src.waiters_tail = w
    _unlock_source(src)
    return true
end

# Remove `w` from `src`'s waiter list; a no-op if the cancellation walk
# already unlinked it.
function unregister_cancellation!(src::CancellationTokenSource, w::Task)
    _lock_source(src)
    if w.wait_tprev !== nothing || w.wait_tnext !== nothing || src.waiters_head === w
        _unlink_waiter!(src, w)
    end
    w.wait_token = nothing
    _unlock_source(src)
    return nothing
end

# caller must hold src's lock and have checked membership
function _unlink_waiter!(src::CancellationTokenSource, w::Task)
    prev = w.wait_tprev
    next = w.wait_tnext
    if prev === nothing
        src.waiters_head = next
    else
        (prev::Task).wait_tnext = next
    end
    if next === nothing
        src.waiters_tail = prev
    else
        (next::Task).wait_tprev = prev
    end
    w.wait_tnext = nothing
    w.wait_tprev = nothing
    return nothing
end

# Watcher (`wait(::CancellationToken)`) registration: park `w` on `src`'s
# watcher list, whose parked tasks the cancellation walk *completes* -
# delivering the `CancellationRequest` as a value - in contrast to the
# waiter list above, whose parked tasks it interrupts with the request as
# an exception. Watchers link singly through the task's `wait_next` field,
# with `wait_queue` identifying the source as the waitee; the task's level
# half (`wait_token`/`wait_tnext`/`wait_tprev`) stays free for the ordinary
# registration on the wait's own governing token. Returns `false` (without
# parking) if `src` is already cancelled.
function _register_watcher!(src::CancellationTokenSource, w::Task)
    _lock_source(src)
    st = @atomic :monotonic src.state
    if st != 0x00
        _unlock_source(src)
        return false
    end
    w.wait_queue = src
    w.wait_next = src.watchers
    src.watchers = w
    _unlock_source(src)
    return true
end

# Remove `w` from `src`'s watcher list; a no-op if the cancellation walk
# already emptied it.
function _unregister_watcher!(src::CancellationTokenSource, w::Task)
    _lock_source(src)
    p = src.watchers
    if p === w
        src.watchers = w.wait_next
    else
        while p !== nothing
            p = p::Task
            n = p.wait_next
            if n === w
                p.wait_next = w.wait_next
                break
            end
            p = n
        end
    end
    w.wait_next = nothing
    w.wait_queue = nothing
    _unlock_source(src)
    return nothing
end

## Cancellation

"""
    cancel!(src::CancellationTokenSource,
            request::CancellationRequest=CANCEL_REQUEST_SAFE)::Bool

Cancel `src` and its whole subtree at the given severity. Level-triggered
and monotonic: waiters (and future registrants) observe the cancellation
until the source is discarded, and repeated calls only have an effect when
they *escalate* the severity ([`CANCEL_REQUEST_SAFE`](@ref) ->
[`CANCEL_REQUEST_ABANDON_EXTERNAL`](@ref) ->
[`CANCEL_REQUEST_ABANDON_ALL`](@ref)), re-notifying waiters at the higher
severity. Returns whether the call changed the state.

Tasks blocked in cancellable operations governed by a token of the subtree
are woken with a [`CancellationRequest`](@ref) thrown into them, and running
computations are interrupted at their cancellation points (see
[`@cancel_check`](@ref)). At `CANCEL_REQUEST_ABANDON_ALL`, tasks are not
woken but frozen in place instead.
"""
function cancel!(src::CancellationTokenSource,
                 request::CancellationRequest=CANCEL_REQUEST_SAFE)
    sev = request.request
    if !(sev == 0x0 || sev == 0x3 || sev == 0x4)
        throw(ArgumentError("invalid cancellation severity $(repr(request.request))"))
    end
    _raise_state!(src, sev) || return false
    # Pair with the compiler-order-only publication of `bound_cancel_token`
    # (and of foreign-call cancellation guards) at cancellation points: after
    # this fence, either we observe the binding of a running task, or its
    # next cancellation point observes our state write.
    Threads.atomic_fence_heavy()
    # Mark and drain the subtree (each node is marked before its children so
    # a concurrent attach_child! is level-triggered), then interrupt running
    # computations bound to it.
    _cancel_walk!(src, sev)
    _cancel_running!(src, sev)
    return true
end

# The first walk of a fresh cancellation deduplicates by state advance
# (`visited === nothing`); a redelivery walks an already-raised subtree, so
# it deduplicates by an explicit visited set instead.
function _cancel_walk!(src::CancellationTokenSource, sev::UInt8,
                       visited::Union{Nothing, IdSet{CancellationTokenSource}}=nothing)
    # Iterative worklist (no recursion): a deep source chain must not
    # overflow the canceller's stack, and a reconverging ("linked") graph
    # must visit each node once, not once per path.
    pending = CancellationTokenSource[src]
    visited === nothing || push!(visited, src)
    while !isempty(pending)
        _cancel_walk_node!(pop!(pending), sev, pending, visited)
    end
    return nothing
end

function _cancel_walk_node!(node::CancellationTokenSource, sev::UInt8,
                            pending::Vector{CancellationTokenSource},
                            visited::Union{Nothing, IdSet{CancellationTokenSource}})
    children = nothing
    towake = nothing
    tonotify = nothing
    _lock_source(node)
    # Deliver at least the node's current severity: a concurrent higher-
    # severity cancel! may have raised the state after this walk's own
    # transition, and its walk can find the waiters already unlinked by
    # this one - the claim below must then honor the escalated request.
    st = @atomic :acquire node.state
    stsev = st & SEVERITY_MASK
    sev < stsev && (sev = stsev)
    creq = CancellationRequest(sev)
    # Claim and unlink waiters at this node. The actual wakes happen after
    # the lock is released: waking may take other locks (a frozen task's
    # donenotify), and waiters take their waitee's lock *before* this node's
    # lock, so waking under it could deadlock.
    w = node.waiters_head
    while w !== nothing
        w = w::Task
        wnext = w.wait_tnext
        if w.wait_min_severity <= sev
            claimed = (@atomicreplace w.wait_state WAITNODE_WAITING => WAITNODE_CANCELLED).success
            _unlink_waiter!(node, w)
            w.wait_token = nothing
            if claimed
                towake = (w, towake)
            end
            # !claimed: a completion won the race; the waiter resumes
            # normally and unregisters its (now unlinked) node itself.
        end
        w = wnext
    end
    # Claim and unlink watchers (tasks in `wait(::CancellationToken)`). This
    # cancellation is the event they wait *for*, so they are woken with the
    # request as a value - and, unlike waiters, never frozen: a watcher
    # observes this source but does not run under it.
    w = node.watchers
    while w !== nothing
        w = w::Task
        wnext = w.wait_next
        claimed = (@atomicreplace w.wait_state WAITNODE_WAITING => WAITNODE_NOTIFIED).success
        w.wait_next = nothing
        w.wait_queue = nothing
        if claimed
            tonotify = (w, tonotify)
        end
        # !claimed: the wait's own governing token was cancelled first (or
        # its task frozen); its epilogue unlinks nothing - we just did.
        w = wnext
    end
    node.watchers = nothing
    # Snapshot the live children (compacting away collected ones) and queue
    # the ones whose state this walk advanced; a child whose state was
    # already at (or above) this severity has been walked - or is being
    # walked - by whoever advanced it, so revisiting it would make a
    # reconverging graph exponential.
    kids = node.children
    if kids !== nothing
        kids = kids::Vector{WeakRef}
        n = length(kids)
        i = 1
        for j in 1:n
            c = kids[j].value
            c === nothing && continue
            kids[i] = kids[j]
            i += 1
            c = c::CancellationTokenSource
            advanced = _raise_state!(c, sev)
            if visited === nothing ? advanced : !(c in visited)
                visited === nothing || push!(visited, c)
                children = (c, children)
            end
        end
        i <= n && resize!(kids, i - 1)
    end
    _unlock_source(node)
    while towake !== nothing
        (t, towake) = towake::Tuple{Task, Any}
        if sev >= CANCEL_REQUEST_ABANDON_ALL.request
            # do not wake the task; freeze it in place
            freeze_task!(t, creq, node)
        else
            _mark_delivered!(node, sev)
            schedule(t, creq, error=true)
        end
    end
    while tonotify !== nothing
        (t, tonotify) = tonotify::Tuple{Task, Any}
        _mark_delivered!(node, sev)
        schedule(t, creq)
    end
    while children !== nothing
        (c, children) = children::Tuple{CancellationTokenSource, Any}
        push!(pending, c)
    end
    return nothing
end

# Interrupt computations currently running on some thread whose published
# bound token lies in the cancelled subtree: send the cancellation signal
# that unwinds compiled code to its most recent cancellation point, or runs
# the cancellation handler of a foreign call in progress (see the
# `cancel_handler` option of `@ccall`).
# Re-run the delivery walk for an already-cancelled source at its current
# severity: wakes waiters that registered without observing the cancellation
# and re-sends the interruption signal to bound running computations (the
# signal-based delivery is best-effort and can be missed while a reset point
# is unpublished). Used by the ^C machinery when a repeat press arrives
# within the escalation grace period. Returns whether the source was
# cancelled at all.
function redeliver!(src::CancellationTokenSource)
    st = @atomic :acquire src.state
    st == 0x00 && return false
    sev = st & SEVERITY_MASK
    Threads.atomic_fence_heavy()
    _cancel_walk!(src, sev, IdSet{CancellationTokenSource}())
    _cancel_running!(src, sev)
    return true
end

function _cancel_running!(src::CancellationTokenSource, sev::UInt8)
    creq = CancellationRequest(sev)
    ct = current_task()
    self_bound = false
    tasks = ccall(:jl_cancel_collect_bound, Any, (Any,), src)::Vector{Any}
    for t in tasks
        t = t::Task
        istaskdone(t) && continue
        if t === ct
            # The canceller itself is governed by the cancelled subtree;
            # deliver to ourselves last (below), so that the remaining tasks
            # are still processed.
            self_bound = true
        elseif sev >= CANCEL_REQUEST_ABANDON_ALL.request
            freeze_task!(t, creq, src)
        else
            tid = ccall(:jl_get_task_tid, Int16, (Any,), t)
            if tid >= 0
                # Best-effort: the signal only delivers to a published
                # interruptible-region context (a compiled reset point, or a
                # foreign call carrying a cancellation handler); a miss is
                # recovered level-triggered at the task's next cancellation
                # point.
                ccall(:jl_send_cancellation_signal, Cvoid, (Int16,), tid)
            end
        end
    end
    if self_bound && sev >= CANCEL_REQUEST_ABANDON_ALL.request
        # Self-cancellation with ABANDON_ALL: unwind with the request.
        _mark_delivered!(src, sev)
        throw(creq)
    end
    # A SAFE/ABANDON_EXTERNAL self-cancellation is observed at the caller's
    # next cancellation point (level-triggered).
    return nothing
end

## Cancellation points

# The slow path of `@cancel_check`: `st` is the status byte returned by
# `Core.cancellation_point!(src)`.
@noinline function handle_cancellation!(src::Union{Nothing, CancellationTokenSource}, st::UInt8)
    ct = current_task()
    if st & STATUS_PREEMPT_BIT != 0x00
        # consume the cooperative-yield request
        @atomic :monotonic ct.preempt_request = 0x00
    end
    if st & STATE_CANCELLED_BIT == 0x00
        # preempt-only: let another task (e.g. a canceller sharing this
        # thread) run, then resume
        yield()
        return nothing
    end
    src = src::CancellationTokenSource
    st = @atomic :acquire src.state
    sev = st & SEVERITY_MASK
    _mark_delivered!(src, sev)
    throw(CancellationRequest(sev))
end

"""
    @cancel_check
    @cancel_check token

Explicit cancellation point: checks whether the cancellation token governing
the current computation has been cancelled and, if so, throws the
corresponding [`CancellationRequest`](@ref). Long-running computational code
should place these in its hot loops so that it can be cancelled (e.g. by
^C).

The one-argument form checks an explicitly provided
`Union{Nothing, CancellationToken}` instead of resolving the scoped default
token; use it to hoist the token lookup out of a tight loop.
"""
macro cancel_check()
    quote
        local s = default_cancel_source()
        local st = Core.cancellation_point!(s)::UInt8
        st != 0x00 && handle_cancellation!(s, st)
        nothing
    end
end

macro cancel_check(tok)
    quote
        local t = $(esc(tok))
        local s = t === nothing ? nothing : (t::CancellationToken).source
        local st = Core.cancellation_point!(s)::UInt8
        st != 0x00 && handle_cancellation!(s, st)
        nothing
    end
end

# Throw the `CancellationRequest` if `src` is cancelled (level-triggered:
# no per-task state is consulted). This is the entry check of every blocking
# API taking a `cancel` keyword argument: it must run *before* the operation
# has any side effects. Unlike `@cancel_check` this is not a compiled
# cancellation point (it opens no async-interruptible region).
@inline function checkcancel(src::CancellationTokenSource)
    st = @atomic :monotonic src.state
    st == 0x00 && return nothing
    handle_cancellation!(src, st)
    return nothing
end
checkcancel(::Nothing) = nothing
checkcancel(tok::CancellationToken) = checkcancel(tok.source)

# Called by the runtime when a task starts under a dynamic scope, before its
# body runs: a task spawned into an already-cancelled scope observes the
# cancellation immediately (and in particular a task spawned into an
# ABANDON_ALL-frozen scope never runs user code).
function start_task_cancel_check()
    s = default_cancel_source()
    s === nothing && return nothing
    st = Core.cancellation_point!(s)::UInt8
    st != 0x00 && handle_cancellation!(s, st)
    return nothing
end

## The scoped default token

# The scoped-value key under which the governing cancellation token is
# carried. `AbstractScopedValue` so the ScopedValues API (`@with
# Base.CANCEL_TOKEN => tok ...`) works on it; the accessors below avoid the
# ScopedValues module so they are usable during early bootstrap.
struct CancelTokenKey <: AbstractScopedValue{Union{Nothing, CancellationToken}} end

"""
    CANCEL_TOKEN

The scoped value carrying the [`CancellationToken`](@ref) that governs the
current dynamic extent, or `nothing` if there is none. Blocking operations
default their `cancel` keyword argument to it, [`@cancel_check`](@ref)
checks it, and tasks spawned within a scope inherit it.

Establish a governing token with the standard scoped-value API
([`ScopedValues.@with`](@ref) / [`ScopedValues.with`](@ref)):

```julia
using Base.ScopedValues

src = Base.CancellationTokenSource()
with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
    ...   # blocking operations in here are cancellable via `cancel!(src)`
end
```

Scoping `Base.CANCEL_TOKEN => nothing` instead *shields* the enclosed code
from an outer (possibly cancelled) token, making its blocking operations
non-cancellable; use this for cleanup that must complete while the
surrounding computation is being cancelled.

The current value can be read with `Base.CANCEL_TOKEN[]`, for example to
hand the governing token across a boundary that does not preserve dynamic
scope (a `ccall` callback, a queue consumed by unrelated tasks, another
process).
"""
const CANCEL_TOKEN = CancelTokenKey()

@inline function default_cancel_token()
    scope = Core.current_scope()::Union{Scope, Nothing}
    scope === nothing && return nothing
    v = KeyValue.get(scope.values, CANCEL_TOKEN)
    v === nothing && return nothing
    return something(v)::Union{Nothing, CancellationToken}
end

@inline function default_cancel_source()
    tok = default_cancel_token()
    tok === nothing && return nothing
    return (tok::CancellationToken).source
end

## `cancel` keyword-argument plumbing

# The sentinel default for `cancel` keyword arguments: "use the scoped
# default token". Resolution to a concrete token happens once, at the first
# potential-block point of an operation, so fast paths never pay for the
# scope lookup. `cancel = nothing` makes a wait explicitly non-cancellable.
#
# N.B.: a resolved token (`Union{Nothing, CancellationToken}`) is passed
# through *positional* arguments internally: passing the union as a keyword
# argument builds an abstractly-typed NamedTuple whose kwcall the optimizer
# cannot devirtualize (which, among other things, breaks `juliac --trim`).
struct UseDefaultToken end
const DEFAULT_CANCEL = UseDefaultToken()
const CancelTokenArg = Union{UseDefaultToken, CancellationToken, Nothing}
const MaybeToken = Union{Nothing, CancellationToken}

@inline resolve_cancel_token(::UseDefaultToken) = default_cancel_token()
@inline resolve_cancel_token(tok::Union{CancellationToken, Nothing}) = tok

# The entry check of a public API taking a `cancel` keyword argument:
# resolve the token and throw if it is already cancelled (uniformly
# level-triggered for the scoped default and explicit tokens alike).
@inline function check_cancel_arg(cancel::CancelTokenArg)
    tok = resolve_cancel_token(cancel)
    tok === nothing || checkcancel(tok.source)
    return tok
end

# Run `f()` in a dynamic scope governed by `tok` - the raw `Scope` form of
# `ScopedValues.@with(CANCEL_TOKEN => tok, f())`, which is not yet available
# at this point of bootstrap.
@eval function _run_with_cancel_token(f, tok::Union{Nothing, CancellationToken})
    $(Expr(:tryfinally, :(f()), nothing,
           :(Scope(Core.current_scope()::Union{Nothing, Scope}, CANCEL_TOKEN => tok))))
end

# Implementation of a `cancel` keyword argument as dynamic-scope sugar: with
# the default sentinel, run `f()` as-is (zero overhead; `f`'s blocking points
# resolve the scoped token themselves); with an explicit argument, check it
# (throwing before any side effect) and run `f()` in a scope governed by it.
# This composes with *any* implementation underneath `f` - including methods
# of user-defined types that know nothing about cancellation keywords - as
# long as its blocking points use the standard wait machinery. Passing
# `cancel = nothing` shadows an outer token, making `f`'s waits
# non-cancellable.
@inline function _with_cancel_arg(f, cancel::CancelTokenArg)
    cancel === DEFAULT_CANCEL && return f()
    tok = check_cancel_arg(cancel)
    return _run_with_cancel_token(f, tok)
end

## Waiting for cancellation as an event

"""
    wait(tok::CancellationToken; cancel=...)

Block until `tok`'s source is cancelled, and return the corresponding
[`CancellationRequest`](@ref) as an ordinary value; return immediately if it
already is. This inverts the usual delivery - cancellation of `tok` is the
event this operation waits *for*, not an interruption of it - and is the
building block of the watcher-task ("cancellation callback") pattern; see
the manual chapter on [Task Cancellation](@ref man-cancellation).

The wait itself accepts the standard `cancel` keyword argument (defaulting
to the scoped token) and is interrupted by that token like any other
blocking operation. Waiting on the token that also governs the wait is
refused with an `ArgumentError`, since completing and interrupting the wait
would be the same event; pass `cancel = nothing` to wait for `tok`
unconditionally.
"""
function wait(tok::CancellationToken; cancel::CancelTokenArg=DEFAULT_CANCEL)
    src = tok.source
    gov = resolve_cancel_token(cancel)
    govsrc = gov === nothing ? nothing : gov.source
    if govsrc === src
        throw(ArgumentError(
            "cannot wait for a token's cancellation under the same governing token; " *
            "pass `cancel = nothing` to wait for it unconditionally"))
    end
    govsrc === nothing || checkcancel(govsrc)
    st = @atomic :acquire src.state
    if st != 0x00
        sev = st & SEVERITY_MASK
        _mark_delivered!(src, sev)
        return CancellationRequest(sev)
    end
    ct = current_task()
    @atomic :monotonic ct.wait_state = WAITNODE_WAITING
    if !_register_watcher!(src, ct)
        # cancelled between the fast path and registration
        @atomic :monotonic ct.wait_state = WAITNODE_IDLE
        st = @atomic :acquire src.state
        sev = st & SEVERITY_MASK
        _mark_delivered!(src, sev)
        return CancellationRequest(sev)
    end
    if govsrc !== nothing && !register_cancellation!(govsrc, ct)
        # The watcher registration above made this task claimable: the
        # target's cancellation walk may already have claimed our wake and
        # scheduled us. We must not throw while scheduled - reclaim our own
        # wake, or park once to absorb the resume.
        if (@atomicreplace ct.wait_state WAITNODE_WAITING => WAITNODE_IDLE).success
            _unregister_watcher!(src, ct)
        else
            wait() # absorb the producer's schedule (discard the value)
            @atomic :monotonic ct.wait_state = WAITNODE_IDLE
        end
        checkcancel(govsrc) # delivers the cancellation (throws)
        error("cancellation registration refused, but the source is not cancelled")
    end
    ret = try
        wait()
    catch
        govsrc === nothing || unregister_cancellation!(govsrc, ct)
        _unregister_watcher!(src, ct)
        @atomic :monotonic ct.wait_state = WAITNODE_IDLE
        rethrow()
    end
    govsrc === nothing || unregister_cancellation!(govsrc, ct)
    _unregister_watcher!(src, ct)
    @atomic :monotonic ct.wait_state = WAITNODE_IDLE
    return ret::CancellationRequest
end
