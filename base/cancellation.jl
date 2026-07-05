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
    CancellationToken

The observe/wait view of a [`CancellationTokenSource`](@ref). Code holding
only a token can check for, and wait on, cancellation of the associated
source, but cannot request cancellation itself.
"""
struct CancellationToken
    source::CancellationTokenSource
end

"""
    CancellationRequest

The exception thrown into (or returned to) code whose governing cancellation
token has been cancelled. The `request` field records the severity
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

Request a cancellation that will cease waiting for all external resources and
all unacknowledged internal tasks. Such tasks will be frozen and become
unschedulable in the future.

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

const SOURCE_CLOSED_BIT = 0x01

@eval function _new_cancel_source(parent::Union{Nothing, CancellationTokenSource})
    return $(Expr(:new, :CancellationTokenSource,
                  :parent, nothing, nothing, nothing, nothing, nothing,
                  0x00, 0x00, 0x00, 0x00))
end

"""
    CancellationTokenSource() -> CancellationTokenSource
    CancellationTokenSource(parent::CancellationToken)

Create a new cancellation token source. With no arguments the source is a
standalone root; given a parent token, the new source is linked underneath it
so that cancellation of the parent (or any of its ancestors) also cancels the
new source. A source created under an already-cancelled parent is born
cancelled.

Call [`close!`](@ref) when the source's scope ends to unlink it from its
parent; otherwise it remains reachable from (and kept alive by) the parent.

Use [`CancellationToken`](@ref)`(src)` for the observe/wait view, and
[`cancel!`](@ref)`(src)` to request cancellation.
"""
function CancellationTokenSource(parent::CancellationToken)
    src = _new_cancel_source(parent.source)
    attach_child!(parent.source, src)
    return src
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
    _lock_source(parent)
    pst = @atomic :monotonic parent.state
    if pst != 0x00
        _raise_state!(child, pst & SEVERITY_MASK)
    end
    first = parent.children
    child.nextsib = first
    child.prevsib = nothing
    if first !== nothing
        (first::CancellationTokenSource).prevsib = child
    end
    parent.children = child
    _unlock_source(parent)
    return child
end

"""
    close!(src::CancellationTokenSource)

Unlink `src` from its parent source, ending its participation in the
cancellation tree. This is the deterministic disposal point for sources
created for a dynamic scope (e.g. a `@sync` block): without it, the source
would remain linked (and alive) until its parent dies. Idempotent. The
source's own cancelled state is unaffected, as are its children.
"""
function close!(src::CancellationTokenSource)
    parent = src.parent
    parent === nothing && return nothing
    parent = parent::CancellationTokenSource
    _lock_source(parent)
    if src.flags & SOURCE_CLOSED_BIT == 0x00
        src.flags |= SOURCE_CLOSED_BIT
        prev = src.prevsib
        next = src.nextsib
        if prev === nothing
            parent.children = next
        else
            (prev::CancellationTokenSource).nextsib = next
        end
        if next !== nothing
            (next::CancellationTokenSource).prevsib = prev
        end
        src.nextsib = nothing
        src.prevsib = nothing
    end
    _unlock_source(parent)
    return nothing
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

## The per-task wait node (the "combined waitable")
#
# A `WaitNode` fuses the two halves of a cancellable wait into one object:
#  - the *edge* half: intrusive `next`/`queue` links that enqueue the node in
#    a waitee's wait queue (`IntrusiveLinkedList{WaitNode}`), where a
#    completion (`notify`, a libuv callback, task termination) finds it;
#  - the *level* half: intrusive `tnext`/`tprev` links that register the node
#    in a cancellation token source's waiter list, where the cancellation
#    walk finds it.
# The `state` byte arbitrates the wake: whoever CASes WAITING -> (NOTIFIED |
# CANCELLED) owns waking the task; the loser does nothing. One node is cached
# per task (`getwaitnode`) and reused across waits - a task only ever waits
# on one thing at a time - so steady-state blocking waits allocate nothing.
#
# Lifecycle invariant: the *waiter* always unlinks its own node from both
# halves (under the respective locks) before leaving the wait function, so
# the cached node is free for reuse. The cancellation walk unlinks from the
# waiter list when it claims a node; `notify`/completions unlink from the
# wait queue when they pop one. Both unlink operations tolerate the node
# already being gone.

const WAITNODE_IDLE      = 0x00
const WAITNODE_WAITING   = 0x01  # enqueued; wake not yet claimed
const WAITNODE_NOTIFIED  = 0x02  # completion claimed the wake
const WAITNODE_CANCELLED = 0x03  # cancellation claimed the wake

mutable struct WaitNode
    const task::Task
    # edge half (waitee queue; protected by the waitee's lock)
    next::Union{WaitNode, Nothing}
    queue::Any
    # level half (token waiter list; protected by the source's `_lock`)
    token::Union{Nothing, CancellationTokenSource}
    tnext::Union{WaitNode, Nothing}
    tprev::Union{WaitNode, Nothing}
    # minimum severity that may interrupt this wait (used by cancellation
    # *teardown* waits, which re-park after acknowledging a severity and must
    # only be woken by an escalation)
    min_severity::UInt8
    @atomic state::UInt8
    # for waits on raw libuv requests: the uv request pointer (low bit tags a
    # shutdown request)
    uvreq::Ptr{Cvoid}
    WaitNode(t::Task) = new(t, nothing, nothing, nothing, nothing, nothing,
                            0x00, WAITNODE_IDLE, C_NULL)
end

@inline function getwaitnode(ct::Task=current_task())
    w = ct.waitnode
    w === nothing || return w::WaitNode
    w = WaitNode(ct)
    ct.waitnode = w
    return w
end

## Per-task acknowledgement
#
# Delivery of a cancellation to a task acknowledges it: the task's
# `cancellation_ack` field records (source, severity), and cancellation
# points do not re-throw a request the task is already handling. Escalation
# (a higher severity) re-triggers. The recorded source and the source being
# checked are compared along the parent chain: while unwinding, cleanup code
# runs under intermediate scopes whose sources were cancelled by the same
# tree walk (ancestors of the acknowledged source), and may itself open new
# child scopes (born cancelled, descendants) - neither must re-throw.

struct CancelAck
    source::CancellationTokenSource
    severity::UInt8
end

# Whether `a` and `b` lie on a common parent chain (either may be the
# ancestor). `parent` links are const, so this walk is safe without locks.
function _on_parent_chain(a::CancellationTokenSource, b::CancellationTokenSource)
    s = b
    while true
        s === a && return true
        p = s.parent
        p === nothing && break
        s = p::CancellationTokenSource
    end
    s = a
    while true
        s === b && return true
        p = s.parent
        p === nothing && return false
        s = p::CancellationTokenSource
    end
end

function ack_covers(t::Task, src::CancellationTokenSource, sev::UInt8)
    ack = @atomic :acquire t.cancellation_ack
    ack === nothing && return false
    ack = ack::CancelAck
    ack.severity >= sev || return false
    return _on_parent_chain(ack.source, src)
end

# Record that `t` has (or is being) delivered severity `sev` of `src`'s
# cancellation. May be called by the task itself (at a cancellation point)
# or by a canceller waking the parked task; CAS resolves the race in favor
# of the higher severity. Also records the delivery on the source (feeding
# e.g. the ^C episode state machine).
# TODO: propagate the delivered bits up the parent chain, so that a delivery
# against a nested scope's source is visible on the episode source too.
function ack!(t::Task, src::CancellationTokenSource, sev::UInt8)
    @atomic :monotonic src.delivered |= (0x01 << sev)
    new = CancelAck(src, sev)
    old = @atomic :acquire t.cancellation_ack
    while true
        if old !== nothing && (old::CancelAck).severity > sev
            return nothing
        end
        old, success = @atomicreplace :acquire_release :acquire t.cancellation_ack old => new
        success && return nothing
    end
end

"""
    cancelled_token()::Union{Nothing, CancellationToken}

While handling a `CancellationRequest`, returns the token whose cancellation
the current task most recently acknowledged (i.e. the token whose
cancellation is being handled). Returns `nothing` if the task has never
acknowledged a cancellation.
"""
function cancelled_token(t::Task=current_task())
    ack = @atomic :acquire t.cancellation_ack
    ack === nothing && return nothing
    return CancellationToken((ack::CancelAck).source)
end

# The severity the current task has acknowledged for the request it is
# currently handling (or nothing).
function acknowledged_cancellation_severity(t::Task=current_task())
    ack = @atomic :acquire t.cancellation_ack
    ack === nothing && return nothing
    return CancellationRequest((ack::CancelAck).severity)
end

# Whether the current task is unwinding from (or continuing under) a
# cancellation whose severity directs it to abandon external (I/O) waits
# without safe teardown. External wait entry points consult this to skip
# parking entirely.
function abandoning_external_waits(t::Task=current_task())
    ack = @atomic :monotonic t.cancellation_ack
    ack === nothing && return false
    return (ack::CancelAck).severity >= CANCEL_REQUEST_ABANDON_EXTERNAL.request
end

## Waiter registration (level half)

"""
    register_cancellation!(src::CancellationTokenSource, w::WaitNode;
                           min_severity::UInt8=0x00)::Bool

Register `w` on `src`'s waiter list so that cancellation of `src` (or an
ancestor) wakes `w.task`. Returns `false` - without registering - if `src`
is already cancelled at a severity `>= min_severity` that the task has not
already acknowledged (level trigger for late registrants: the caller must
deliver the cancellation instead of parking).
"""
function register_cancellation!(src::CancellationTokenSource, w::WaitNode;
                                min_severity::UInt8=0x00)
    _lock_source(src)
    st = @atomic :monotonic src.state
    if st != 0x00
        sev = st & SEVERITY_MASK
        if sev >= min_severity && !ack_covers(w.task, src, sev)
            _unlock_source(src)
            return false
        end
    end
    w.token = src
    w.min_severity = min_severity
    w.tprev = src.waiters_tail
    w.tnext = nothing
    if src.waiters_tail === nothing
        src.waiters_head = w
    else
        (src.waiters_tail::WaitNode).tnext = w
    end
    src.waiters_tail = w
    _unlock_source(src)
    return true
end

# Remove `w` from `src`'s waiter list; a no-op if the cancellation walk
# already unlinked it.
function unregister_cancellation!(src::CancellationTokenSource, w::WaitNode)
    _lock_source(src)
    if w.tprev !== nothing || w.tnext !== nothing || src.waiters_head === w
        _unlink_waiter!(src, w)
    end
    w.token = nothing
    _unlock_source(src)
    return nothing
end

# caller must hold src's lock and have checked membership
function _unlink_waiter!(src::CancellationTokenSource, w::WaitNode)
    prev = w.tprev
    next = w.tnext
    if prev === nothing
        src.waiters_head = next
    else
        (prev::WaitNode).tnext = next
    end
    if next === nothing
        src.waiters_tail = prev
    else
        (next::WaitNode).tprev = prev
    end
    w.tnext = nothing
    w.tprev = nothing
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

Parked tasks waiting under the subtree are woken with a
[`CancellationRequest`](@ref) thrown into them; running computations bound
to a subtree token (via their cancellation points) are interrupted
asynchronously and their [`with_cancellation_hook`](@ref) handlers are
invoked; at `CANCEL_REQUEST_ABANDON_ALL` tasks are frozen/abandoned instead.
"""
function cancel!(src::CancellationTokenSource,
                 request::CancellationRequest=CANCEL_REQUEST_SAFE)
    sev = request.request & SEVERITY_MASK
    if !(sev == 0x0 || sev == 0x3 || sev == 0x4)
        throw(ArgumentError("invalid cancellation severity $(repr(request.request))"))
    end
    _raise_state!(src, sev) || return false
    # Pair with the compiler-order-only publication of `bound_cancel_token`
    # at cancellation points (and the fence_light in with_cancellation_hook):
    # after this fence, either we observe the binding/hook of a running task,
    # or its next cancellation point observes our state write.
    Threads.atomic_fence_heavy()
    # Mark and drain the subtree (each node is marked before its children so
    # a concurrent attach_child! is level-triggered), then interrupt running
    # computations bound to it.
    _cancel_walk!(src, sev)
    _cancel_running!(src, sev)
    return true
end

# Invoke `t`'s asynchronous cancellation hook (if any) on the cancelling
# thread; see `with_cancellation_hook`. A failing hook must not disrupt the
# rest of request delivery, and reporting its error must not park the
# canceller (which may be the ^C listener on a monopolized thread).
function _invoke_cancellation_hook!(t::Task)
    hook = @atomic :acquire t.cancellation_hook
    hook === nothing && return nothing
    istaskdone(t) && return nothing
    (handler, state) = hook::Tuple{Any, Any}
    try
        invokelatest(handler, state, t)
    catch err
        try
            Core.print(Core.stderr, "ERROR: cancellation hook handler threw: ")
            @ccall jl_(err::Any)::Cvoid
        catch
        end
    end
    nothing
end

function _cancel_walk!(node::CancellationTokenSource, sev::UInt8)
    creq = CancellationRequest(sev)
    children = nothing
    towake = nothing
    _lock_source(node)
    # Claim and unlink waiters at this node. The actual wakes happen after
    # the lock is released: waking may take other locks (a frozen task's
    # donenotify), and waiters take their waitee's lock *before* this node's
    # lock, so waking under it could deadlock.
    w = node.waiters_head
    while w !== nothing
        w = w::WaitNode
        wnext = w.tnext
        if w.min_severity <= sev && !ack_covers(w.task, node, sev)
            claimed = (@atomicreplace w.state WAITNODE_WAITING => WAITNODE_CANCELLED).success
            _unlink_waiter!(node, w)
            w.token = nothing
            if claimed
                towake = (w.task, towake)
            end
            # !claimed: a completion won the race; the waiter resumes
            # normally and unregisters its (now unlinked) node itself.
        end
        w = wnext
    end
    # snapshot children, then recurse without holding this node's lock
    c = node.children
    while c !== nothing
        c = c::CancellationTokenSource
        _raise_state!(c, sev)
        children = (c, children)
        c = c.nextsib
    end
    _unlock_source(node)
    while towake !== nothing
        (t, towake) = towake::Tuple{Task, Any}
        # A task parked inside a with_cancellation_hook region still gets its
        # hook invoked (the registered external operation may be in flight on
        # its behalf).
        _invoke_cancellation_hook!(t)
        if sev >= CANCEL_REQUEST_ABANDON_ALL.request
            # do not wake the task; freeze it in place
            freeze_task!(t, creq, node)
        else
            ack!(t, node, sev)
            schedule(t, creq, error=true)
        end
    end
    while children !== nothing
        (c, children) = children::Tuple{CancellationTokenSource, Any}
        _cancel_walk!(c, sev)
    end
    return nothing
end

# Interrupt computations currently running on some thread whose published
# bound token lies in the cancelled subtree: invoke their asynchronous
# cancellation hooks (foreign waits) and send the cancellation signal that
# unwinds compiled code to its most recent cancellation point.
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
    _cancel_walk!(src, sev)
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
        # This handler is called on the *cancelling* thread; the handler
        # contract requires thread safety and tolerance of spurious/multiple
        # invocation.
        ack_covers(t, src, sev) || _invoke_cancellation_hook!(t)
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
                # Best-effort: the signal only unwinds published (reset-safe)
                # regions; a miss is recovered level-triggered at the task's
                # next cancellation point.
                ccall(:jl_send_cancellation_signal, Cvoid, (Int16,), tid)
            end
        end
    end
    if self_bound && sev >= CANCEL_REQUEST_ABANDON_ALL.request && !ack_covers(ct, src, sev)
        # Self-cancellation with ABANDON_ALL: unwind with the request.
        ack!(ct, src, sev)
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
    ack_covers(ct, src, sev) && return nothing
    ack!(ct, src, sev)
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

# Throw (with acknowledgement) if `src` is cancelled and the current task is
# not already handling that cancellation. This is the entry check of every
# blocking API taking a `cancel` keyword argument: it must run *before* the
# operation has any side effects. Unlike `@cancel_check` this is not a
# compiled cancellation point (it opens no async-interruptible region).
@inline function checkcancel(src::CancellationTokenSource)
    st = @atomic :monotonic src.state
    st == 0x00 && return nothing
    handle_cancellation!(src, st)
    return nothing
end
checkcancel(::Nothing) = nothing
checkcancel(tok::CancellationToken) = checkcancel(tok.source)

# Whether `src` is cancelled at a severity `t` has not acknowledged - i.e.
# whether `checkcancel(src)` would throw. For blocking APIs that must release
# resources (locks) before delivering the cancellation.
function cancel_pending(src::CancellationTokenSource, t::Task=current_task())
    st = @atomic :monotonic src.state
    st == 0x00 && return false
    return !ack_covers(t, src, st & SEVERITY_MASK)
end
cancel_pending(::Nothing, t::Task=current_task()) = false

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
current dynamic extent. Blocking APIs default their `cancel` keyword
argument to this token, and [`@cancel_check`](@ref) checks it. Scope a token
over a computation with `@with(Base.CANCEL_TOKEN => tok, ...)`.
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

"""
    cancellation_token()::Union{Nothing, CancellationToken}

The [`CancellationToken`](@ref) governing the current dynamic extent, or
`nothing` if there is none. Pass this to another task or thread to let it
observe cancellation of the current scope.
"""
cancellation_token() = default_cancel_token()

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

# The entry check of a public API taking a `cancel` keyword argument,
# returning the resolved token. The two forms differ deliberately:
#  - the scoped default is acknowledgement-gated (`checkcancel`): a task
#    already handling the cancellation of its scope may still perform
#    blocking cleanup work under it;
#  - an *explicitly* passed token that is already cancelled always throws:
#    passing `cancel = tok` states that this operation must not run once
#    `tok` fired, no matter who asks.
@inline function check_cancel_arg(cancel::CancelTokenArg)
    if cancel === DEFAULT_CANCEL
        tok = default_cancel_token()
        tok === nothing || checkcancel(tok.source)
        return tok
    end
    tok = resolve_cancel_token(cancel)
    if tok !== nothing
        st = @atomic :acquire tok.source.state
        st == 0x00 || throw(CancellationRequest(st & SEVERITY_MASK))
    end
    return tok
end

@eval function with_cancel_token(f, tok::Union{Nothing, CancellationToken})
    $(Expr(:tryfinally, :(f()), nothing,
           :(Scope(Core.current_scope()::Union{Nothing, Scope}, CANCEL_TOKEN => tok))))
end

"""
    with_cancel_token(f, tok::Union{Nothing, CancellationToken})

Run `f()` in a new dynamic scope in which `tok` is the governing cancellation
token (the closure equivalent of `@with Base.CANCEL_TOKEN => tok f()`,
available during early bootstrap).
"""
with_cancel_token

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
    return with_cancel_token(f, tok)
end
