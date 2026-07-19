# This file is a part of Julia. License is MIT: https://julialang.org/license

## Cancellation tokens
#
# Cancellation is organized around *cancellation token sources*
# (`Core.CancellationTokenSource`): level-triggered condition nodes arranged
# in a tree. Cancelling a source cancels its whole subtree; the cancelled
# state is monotonic (severities only escalate, never reset). Following the
# .NET split, a `CancellationToken` is the observe view handed to code that
# may only *react* to cancellation; the source is the capability to *request*
# it.
#
# The token governing a piece of code is carried dynamically as a scoped
# value (see `CANCEL_TOKEN`), which `@cancel_check` resolves at every check.
# Tasks inherit their creating task's scope, so cancellation scopes propagate
# to child tasks without explicit plumbing.
#
# This file provides the data model and the cooperative (polling) checks.
# Delivery — waking tasks parked in blocking operations, interrupting
# running computations, `cancel` keyword arguments on the blocking APIs, and
# the ^C machinery — builds on top of it separately.
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

A token takes effect by scoping it over a computation via the
[`CANCEL_TOKEN`](@ref) scoped value (spawned tasks inherit it): once the
source is cancelled, the computation's cancellation points (see
[`@cancel_check`](@ref)) throw a [`CancellationRequest`](@ref).
"""
struct CancellationToken
    source::CancellationTokenSource
end

"""
    CancellationRequest

The exception thrown by cancellation points whose governing cancellation
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

# The state byte of a cancelled source is (STATE_CANCELLED_BIT | severity).
# The 0x40 bit is reserved (status bytes of compiled cancellation points use
# it to report a pending cooperative-yield request).
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
                  :parents, nothing, 0x00, 0x00, 0x00))
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
reachable - a held token, or work governed by it, keeps it alive. Once
nothing can observe it any more, it is garbage collected and thereby drops
out of the graph; there is no explicit detach operation.

Use [`CancellationToken`](@ref)`(src)` for the observe view, and
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

## Delivery bookkeeping
#
# Cancellation is uniformly level-triggered: while the governing token is
# cancelled, every cancellation point throws the `CancellationRequest`.
# There is no per-task acknowledgement state; code that must keep running
# under a cancelled scope shields itself by scoping `CANCEL_TOKEN => nothing`
# over the block.

# Record that a cancellation of `src` at severity `sev` was delivered to
# (observed by) some task, i.e. thrown at one of its cancellation points.
# Feeds the (upcoming) ^C episode state machine ("was the request ever
# seen?"), so the bits are propagated to every ancestor: a delivery against a
# nested scope's source acknowledges the episode source too - otherwise the
# SIGINT classifier would misread a successfully delivered cancellation as
# unacknowledged and escalate a repeat ^C to abandonment.
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

## Cancellation

"""
    cancel!(src::CancellationTokenSource,
            request::CancellationRequest=CANCEL_REQUEST_SAFE)::Bool

Cancel `src` and its whole subtree at the given severity. Level-triggered
and monotonic: observers (including future registrants) see the cancellation
until the source is discarded, and repeated calls only have an effect when
they *escalate* the severity ([`CANCEL_REQUEST_SAFE`](@ref) ->
[`CANCEL_REQUEST_ABANDON_EXTERNAL`](@ref) ->
[`CANCEL_REQUEST_ABANDON_ALL`](@ref)). Returns whether the call changed the
state.

Computations governed by a token of the subtree observe the cancellation at
their cancellation points (see [`@cancel_check`](@ref)), which throw a
[`CancellationRequest`](@ref).
"""
function cancel!(src::CancellationTokenSource,
                 request::CancellationRequest=CANCEL_REQUEST_SAFE)
    sev = request.request
    if !(sev == 0x0 || sev == 0x3 || sev == 0x4)
        throw(ArgumentError("invalid cancellation severity $(repr(request.request))"))
    end
    _raise_state!(src, sev) || return false
    # Pairs with the compiler-order-only publication of per-task token
    # bindings at compiled cancellation points (upcoming): after this fence,
    # either the canceller observes the binding of a running task, or the
    # task's next cancellation point observes our state write.
    Threads.atomic_fence_heavy()
    # Mark the subtree: each node is marked before its children so a
    # concurrent attach_child! is level-triggered.
    _cancel_walk!(src, sev)
    return true
end

function _cancel_walk!(src::CancellationTokenSource, sev::UInt8)
    # Iterative worklist (no recursion): a deep source chain must not
    # overflow the canceller's stack, and a reconverging ("linked") graph
    # must visit each node once, not once per path.
    pending = CancellationTokenSource[src]
    while !isempty(pending)
        _cancel_walk_node!(pop!(pending), sev, pending)
    end
    return nothing
end

function _cancel_walk_node!(node::CancellationTokenSource, sev::UInt8,
                            pending::Vector{CancellationTokenSource})
    children = nothing
    _lock_source(node)
    # Advance at least to the node's current severity: a concurrent higher-
    # severity cancel! may have raised the state after this walk's own
    # transition; its walk skips children this one already advanced, so this
    # walk must carry the escalated severity onward.
    st = @atomic :acquire node.state
    stsev = st & SEVERITY_MASK
    sev < stsev && (sev = stsev)
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
            if _raise_state!(c, sev)
                children = (c, children)
            end
        end
        i <= n && resize!(kids, i - 1)
    end
    _unlock_source(node)
    while children !== nothing
        (c, children) = children::Tuple{CancellationTokenSource, Any}
        push!(pending, c)
    end
    return nothing
end

## Cancellation points

# The slow path of `@cancel_check`: `st` is the (non-zero) state byte of the
# governing source.
@noinline function handle_cancellation!(src::CancellationTokenSource, st::UInt8)
    # re-read: deliver the severity current at throw time, not the one the
    # fast path happened to observe
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
should place these in its hot loops so that it can be cancelled.

The one-argument form checks an explicitly provided
`Union{Nothing, CancellationToken}` instead of resolving the scoped default
token; use it to hoist the token lookup out of a tight loop.
"""
macro cancel_check()
    quote
        # also a GC safepoint, so that a tight polling loop cannot starve a
        # concurrent stop-the-world (compiled cancellation points will emit
        # one as well)
        ccall(:jl_gc_safepoint, Cvoid, ())
        checkcancel(default_cancel_source())
        nothing
    end
end

macro cancel_check(tok)
    quote
        local t = $(esc(tok))
        ccall(:jl_gc_safepoint, Cvoid, ())
        checkcancel(t === nothing ? nothing : (t::CancellationToken).source)
        nothing
    end
end

# Throw the `CancellationRequest` if `src` is cancelled (level-triggered:
# no per-task state is consulted).
@inline function checkcancel(src::CancellationTokenSource)
    st = @atomic :monotonic src.state
    st == 0x00 && return nothing
    handle_cancellation!(src, st)
    return nothing
end
checkcancel(::Nothing) = nothing
checkcancel(tok::CancellationToken) = checkcancel(tok.source)

## The scoped default token

# The scoped-value key under which the governing cancellation token is
# carried. `AbstractScopedValue` so the ScopedValues API (`@with
# Base.CANCEL_TOKEN => tok ...`) works on it; the accessors below avoid the
# ScopedValues module so they are usable during early bootstrap.
struct CancelTokenKey <: AbstractScopedValue{Union{Nothing, CancellationToken}} end

"""
    CANCEL_TOKEN

The scoped value carrying the [`CancellationToken`](@ref) that governs the
current dynamic extent, or `nothing` if there is none. [`@cancel_check`](@ref)
checks it, and tasks spawned within a scope inherit it.

Establish a governing token with the standard scoped-value API
([`ScopedValues.@with`](@ref) / [`ScopedValues.with`](@ref)):

```julia
using Base.ScopedValues

src = Base.CancellationTokenSource()
with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
    ...   # cancellation points in here observe `cancel!(src)`
end
```

Scoping `Base.CANCEL_TOKEN => nothing` instead *shields* the enclosed code
from an outer (possibly cancelled) token; use this for cleanup that must
complete while the surrounding computation is being cancelled.

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
