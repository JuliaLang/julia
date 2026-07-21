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
#
# A source is a variable-sized object: its fixed fields (`child_head`,
# `state`, `nparents`) are followed by `nparents` {parent,
# next, pprev} link entries (see `jl_cancel_source_t` in julia_threads.h).
# The `parent` slots are strong, const references - a child keeps its
# parents alive - while `child_head` and the `next`/`pprev` slots form
# intrusive per-parent sibling lists of *weak* references: when a source is
# collected, the sweep - which visits every dead object anyway - detects it
# and unlinks it from its parents' lists in O(1) via the `pprev`
# back-pointer, so there is no explicit detach operation, the collector's
# work is proportional to the number of sources that actually died, and, at
# any point the mutator can observe, the lists contain only live sources.
#
# The lists are lock-free: mutators only ever prepend (in the C constructor
# `jl_new_cancel_source`, via CAS on `child_head`); removal happens only
# inside the collector with the world stopped. Construction and cancellation
# synchronize with seq_cst operations so that attachment is level-triggered:
# either the canceller's walk observes the new child, or the constructor
# observes the cancelled state (and the child is born cancelled).

# Construction is the builtin `Core._new_cancel_source(parents...)`
# (jl_new_cancel_source): it allocates the object with one link entry per
# argument and performs the linking and state inheritance in C, where the
# absence of safepoints between publishing a link and reading the parent's
# state can be guaranteed.

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
CancellationTokenSource(parent::CancellationToken) =
    Core._new_cancel_source(parent.source)::CancellationTokenSource
function CancellationTokenSource(parent::CancellationToken, rest::CancellationToken...)
    srcs = CancellationTokenSource[parent.source]
    for tok in rest
        any(s -> s === tok.source, srcs) || push!(srcs, tok.source)
    end
    return Core._new_cancel_source(srcs...)::CancellationTokenSource
end
CancellationTokenSource(::Nothing) = Core._new_cancel_source()::CancellationTokenSource
CancellationTokenSource() = Core._new_cancel_source()::CancellationTokenSource

# The i-th (1-based) parent of `src`. Parent links are strong and const, so
# these reads need no synchronization.
_cancel_parent(src::CancellationTokenSource, i::Int) =
    ccall(:jl_cancel_source_parent, Any, (Any, Csize_t), src, i - 1)::CancellationTokenSource

# The sibling after `child` on `parent`'s child list (`nothing` at its end).
# Weak, but safe to traverse from Julia: the returned reference is rooted the
# moment the ccall returns, and the GC's splice pass keeps the lists free of
# collected entries at every safepoint, so a traversal (re-)started from a
# rooted node only ever sees live sources.
_cancel_next_child(parent::CancellationTokenSource, child::CancellationTokenSource) =
    ccall(:jl_cancel_source_next_child, Any, (Any, Any), parent, child)::Union{Nothing, CancellationTokenSource}

# CAS-max the source's state to (STATE_CANCELLED_BIT | sev). Returns true if
# the state was raised, false if it was already at (or above) the severity.
# seq_cst: pairs with the (child-list publication; state read) sequence in
# `jl_new_cancel_source` - see the walk in `_cancel_walk_node!`.
function _raise_state!(src::CancellationTokenSource, sev::UInt8)
    old = @atomic :monotonic src.state
    while true
        if old != 0x00 && (old & SEVERITY_MASK) >= sev
            return false
        end
        old, success = @atomicreplace :sequentially_consistent :monotonic src.state old => (STATE_CANCELLED_BIT | sev)
        success && return true
    end
end

## Cancellation
#
# Cancellation is uniformly level-triggered: while the governing token is
# cancelled, every cancellation point throws the `CancellationRequest`.
# There is no per-task acknowledgement state; code that must keep running
# under a cancelled scope shields itself by scoping `CANCEL_TOKEN => nothing`
# over the block.

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

When `cancel!` returns, every source currently in the subtree has been
advanced to (at least) the requested severity by this very call - even a
call that returns `false` performs the full walk, so that a repeated
`cancel!` also repairs a subtree whose earlier cancellation was cut short
(say, by the cancelling task itself being torn down mid-walk).
"""
function cancel!(src::CancellationTokenSource,
                 request::CancellationRequest=CANCEL_REQUEST_SAFE)
    sev = request.request
    if !(sev == 0x0 || sev == 0x3 || sev == 0x4)
        throw(ArgumentError("invalid cancellation severity $(repr(request.request))"))
    end
    raised = _raise_state!(src, sev)
    # Pairs with the compiler-order-only publication of per-task token
    # bindings at compiled cancellation points (upcoming): after this fence,
    # either the canceller observes the binding of a running task, or the
    # task's next cancellation point observes our state write.
    Threads.atomic_fence_heavy()
    # Mark the subtree: each node is marked before its children so a
    # concurrent construction of a child source is level-triggered. The walk
    # does not depend on `raised`: when a concurrent equal-severity cancel!
    # wins the state transition, its walk may still be in flight (or may
    # never finish), so the loser must provide the on-return guarantee with
    # its own traversal.
    _cancel_walk!(src, sev)
    return raised
end

function _cancel_walk!(src::CancellationTokenSource, sev::UInt8)
    # Iterative worklist (no recursion): a deep source chain must not
    # overflow the canceller's stack. The visited set - rather than pruning
    # on nodes whose state some walk already advanced - makes a reconverging
    # ("linked") graph linear in edges while keeping the walk self-contained:
    # this call visits every reachable node itself instead of trusting
    # another (possibly interrupted) walk to have done so.
    visited = IdSet{CancellationTokenSource}()
    push!(visited, src)
    pending = CancellationTokenSource[src]
    while !isempty(pending)
        _cancel_walk_node!(pop!(pending), sev, pending, visited)
    end
    return nothing
end

function _cancel_walk_node!(node::CancellationTokenSource, sev::UInt8,
                            pending::Vector{CancellationTokenSource},
                            visited::IdSet{CancellationTokenSource})
    # Advance at least to the node's current severity: a concurrent higher-
    # severity cancel! may have raised the state after this walk's own
    # transition, and this walk may reach parts of the subtree before that
    # one does, so it carries the escalated severity onward (best effort;
    # the escalator's own walk is what guarantees its severity).
    st = @atomic :acquire node.state
    stsev = st & SEVERITY_MASK
    sev < stsev && (sev = stsev)
    # Walk the node's (weak, intrusive) child list. The seq_cst `child_head`
    # read below (paired with the seq_cst state CAS in `cancel!` - the read
    # it performs is seq_cst even when the transition was lost) closes the
    # race against a concurrent attach: a child that this read misses was
    # published after the state write that made this walk reach `node`, so
    # its constructor observes a cancelled parent and the child is born at
    # (at least) this severity. Children attached concurrently *during* the
    # walk are prepended before the list positions already traversed and are
    # likewise born cancelled.
    c = @atomic node.child_head
    while c !== nothing
        c = c::CancellationTokenSource
        _raise_state!(c, sev)
        if !(c in visited)
            push!(visited, c)
            push!(pending, c)
        end
        c = _cancel_next_child(node, c)
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
scope (a `ccall` callback, a queue consumed by unrelated tasks). Note that
this only connects code within the same process: *serializing* a token
(e.g. shipping it to a worker process) copies its source graph - the copy
reflects the cancellation state as of serialization, and is independent
thereafter, so cancelling the original does not cancel the copy.
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
