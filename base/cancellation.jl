# This file is a part of Julia. License is MIT: https://julialang.org/license

## Cancellation tokens
#
# Cancellation is organized around *cancellation token sources*
# (`Core.CancellationTokenSource`): level-triggered condition nodes arranged
# in a DAG (a source may have several parents). Cancelling a source cancels
# all of its descendants; the cancelled state is monotonic (severities only
# escalate, never reset). Following the
# .NET split, a `CancellationToken` is the observe view handed to code that
# may only *react* to cancellation; the source is the capability to *request*
# it.
#
# For convenience, the scoped value `CANCEL_TOKEN` carries the default
# cancellation token governing the current dynamic extent.

const CancellationTokenSource = Core.CancellationTokenSource

"""
    CancellationToken(src::CancellationTokenSource)

The observe side of a [`CancellationTokenSource`](@ref): code holding a
token can be interrupted by - and can query ([`iscancelled`](@ref)) -
cancellation of the associated source, but cannot request cancellation
itself. Only the holder of the *source* can call [`cancel!`](@ref).

A token takes effect by scoping it over a computation via the
[`CANCEL_TOKEN`](@ref) scoped value or by explicit token passing.
Once the associated source is cancelled, the computation's cancellation points (see
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
struct CancellationRequest <: Exception
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
const CANCEL_REQUEST_SAFE = CancellationRequest(0x1)

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

# The status byte reported by a cancellation point (`Core.cancellation_point!`)
# is the governing source's state byte (0 while uncancelled, the severity once
# cancelled) with the 0x40 bit merged in when the task has a pending
# cooperative-yield request. The mask recovers the severity half of such a
# status byte; source state reads themselves need no masking.
const STATUS_PREEMPT_BIT = 0x40
const SEVERITY_MASK = 0x3f

# A request's payload is the plain severity; kept as an accessor for the
# delivery layer's comparisons.
severity(cr::CancellationRequest) = cr.request

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
    return CancellationRequest(st)
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

## Source construction and graph linkage

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

# CAS-max the source's state to `sev` (a valid, nonzero severity). Returns
# true if the state was raised, false if it was already at (or above) the
# severity.
# seq_cst: pairs with the (child-list publication; state read) sequence in
# `jl_new_cancel_source` - see the walk in `_cancel_walk_node!`.
function _raise_state!(src::CancellationTokenSource, sev::UInt8)
    old = @atomic :monotonic src.state
    while true
        if old >= sev
            return false
        end
        old, success = @atomicreplace :sequentially_consistent :monotonic src.state old => sev
        success && return true
    end
end

## Wait registrations (used by condition.jl and every parked wait)

# A task's registration on the things it waits for. An entry carries
# uniform {owner, next, aux} slots (see the accessors below): a waitee
# slot's fields are protected by the waitee's lock (its `owner` holds the
# queue's identity - see `waitqueue` - while enqueued, acting as the "am I
# registered, and on what" witness, and `nothing` otherwise), a
# cancellation-source slot's by the registration protocol further below;
# `task` is written by the owning task before enqueueing, so it is ordered
# by the same lock for lock-holding readers - but it is also read (and
# scrubbed) by walkers that hold none of the owner's locks (the
# cancellation walk, `_waitq_isempty`), so like the slot owners it is an
# atomic field accessed relaxed: the values are identity witnesses whose
# staleness the protocols tolerate (a claim is validated by the
# `waiting_on` CAS, never by the `task` read alone).
#
# The wake-claim protocol: a parked task `t` points to its current
# registration through the atomic field `t.waiting_on`. Whoever wants to wake
# it must first claim the wake by atomically clearing that field:
#
#   - `notify` (holding the waitee's lock) pops an entry `w` and claims via
#     CAS(t.waiting_on, w => nothing). The expected-value CAS makes stale
#     entries harmless: if `t` was interrupted and has since registered
#     elsewhere, the CAS fails and the popped corpse is simply dropped.
#   - an interrupter (`schedule(t, exc, error=true)`) claims via an
#     unconditional swap: it is directed at the *task*, not at any particular
#     wait, so claiming whatever `t` is currently registered on is correct.
#     It then opportunistically unlinks the claimed entry under the waitee's
#     lock via `trylock` (see `try_unlink_claimed!`); if the lock is
#     unavailable the entry stays linked and is collected lazily, either by
#     the interrupted task's own wait cleanup or by the `notify` that pops
#     and drops it. The cancellation walk claims like `notify` (expected-entry
#     CAS) but leaves the entry linked for the same lazy collection.
#   - wake sources directed at one *specific* wait (e.g. the timeout task of
#     `Experimental.wait_with_timeout`) must register the wait with a fresh,
#     single-use entry: single-use-ness is what guarantees their
#     expected-value CAS cannot mistakenly claim a later, unrelated wait.
#
# Entries are heap objects. A task whose interrupted wait left a stale registration
# behind can immediately register anew - e.g. park on a lock during its cleanup - with a fresh entry.
# A wait registration is a small set of {owner, link, aux} slots - one per
# thing the wait is registered on, with `owner` doubling as the membership
# witness ("am I registered, and on what") - sharing the task's single
# wake-claim word. Entries come in kinds by slot count: the 1-slot kind
# covers plain parks (one waitee slot), the 2-slot kind adds the
# cancellation-source slot for cancellable parks. (A variable-slot "many"
# kind for wait-any over arbitrary waitables - waitany, timeouts - is
# planned to follow; consumers locate their slot by scanning for their own
# identity, so links always point at whole entries.) The kinds share their
# leading layout; `WaitEntry` is the union of all kinds, and hot paths
# union-split on it.
typegroup
    mutable struct WaitEntry1
        @atomic task::Union{Task, Nothing}
        @atomic owner1::Any
        next1::Union{WaitEntry1, WaitEntry2, Core.WaitEntryN, Nothing}
        aux1::UInt64
        WaitEntry1(task::Union{Task, Nothing}) = new(task, nothing, nothing, 0x0)
    end
    mutable struct WaitEntry2
        @atomic task::Union{Task, Nothing}
        @atomic owner1::Any
        next1::Union{WaitEntry1, WaitEntry2, Core.WaitEntryN, Nothing}
        aux1::UInt64
        @atomic owner2::Any
        next2::Union{WaitEntry1, WaitEntry2, Core.WaitEntryN, Nothing}
        aux2::UInt64
        WaitEntry2(task::Union{Task, Nothing}) =
            new(task, nothing, nothing, 0x0, nothing, nothing, 0x0)
    end
end
const WaitEntryN = Core.WaitEntryN
WaitEntryN(task::Union{Task, Nothing}, nslots::Integer) =
    ccall(:jl_new_wait_entry, Any, (Any, Csize_t), task, nslots)::WaitEntryN
const WaitEntry = Union{WaitEntry1, WaitEntry2, WaitEntryN}

## Uniform slot access
#
# Every kind is a `task` plus `_nslots` uniform {owner, next, aux} slots
# (1-based). `task` and the slot `owner`s are atomic, accessed relaxed
# (walkers read them without the owner's locks - see the notes above and
# below); the `owner` doubles as the membership witness (`nothing` = free
# slot). `next` and `aux` are plain, each protected by its owner's
# discipline: waitee slots by the waitee's lock, cancellation-source slots
# by the registration protocol below, and free slots by the owning task.
# Lists link whole entries: a traversal locates its slot in each entry by
# scanning for its own identity (`_find_slot`), so an entry may be
# registered on several waitables at once (wait-any). At most one slot per
# owner per entry.
_nslots(w::WaitEntry1) = 1
_nslots(w::WaitEntry2) = 2
_nslots(w::WaitEntryN) = Int(w.nslots)

# Slot owners are read by threads that do not hold the slot's protecting
# lock (walks and unlink sweeps scan every slot of an entry for their own
# identity), so they are atomic fields accessed relaxed: the values are
# identity witnesses whose staleness the protocols tolerate, and where an
# owner read carries ordering obligations, the ordering is supplied
# elsewhere (the waitee's lock, or the seq_cst publish/recheck dance of
# `register_cancellation!`).
@inline _slot_owner(w::WaitEntry1, i::Int) = @atomic :monotonic w.owner1
@inline _slot_owner(w::WaitEntry2, i::Int) =
    i == 1 ? (@atomic :monotonic w.owner1) : (@atomic :monotonic w.owner2)
@inline _slot_owner(w::WaitEntryN, i::Int) =
    ccall(:jl_wait_entry_slot_owner, Any, (Any, Csize_t), w, i - 1)

@inline function _set_slot_owner!(w::WaitEntry1, i::Int, @nospecialize(v))
    @atomic :monotonic w.owner1 = v
    return nothing
end
@inline function _set_slot_owner!(w::WaitEntry2, i::Int, @nospecialize(v))
    if i == 1
        @atomic :monotonic w.owner1 = v
    else
        @atomic :monotonic w.owner2 = v
    end
    return nothing
end
@inline _set_slot_owner!(w::WaitEntryN, i::Int, @nospecialize(v)) =
    ccall(:jl_wait_entry_set_slot_owner, Cvoid, (Any, Csize_t, Any), w, i - 1, v)

@inline _slot_next(w::WaitEntry1, i::Int) = w.next1
@inline _slot_next(w::WaitEntry2, i::Int) = i == 1 ? w.next1 : w.next2
@inline _slot_next(w::WaitEntryN, i::Int) =
    ccall(:jl_wait_entry_slot_next, Any, (Any, Csize_t), w, i - 1)::Union{WaitEntry, Nothing}

@inline _set_slot_next!(w::WaitEntry1, i::Int, v::Union{WaitEntry, Nothing}) = (w.next1 = v; nothing)
@inline function _set_slot_next!(w::WaitEntry2, i::Int, v::Union{WaitEntry, Nothing})
    i == 1 ? (w.next1 = v) : (w.next2 = v)
    return nothing
end
@inline _set_slot_next!(w::WaitEntryN, i::Int, v::Union{WaitEntry, Nothing}) =
    ccall(:jl_wait_entry_set_slot_next, Cvoid, (Any, Csize_t, Any), w, i - 1,
          v === nothing ? nothing : v)

@inline _slot_aux(w::WaitEntry1, i::Int) = w.aux1
@inline _slot_aux(w::WaitEntry2, i::Int) = i == 1 ? w.aux1 : w.aux2
@inline _slot_aux(w::WaitEntryN, i::Int) =
    ccall(:jl_wait_entry_slot_aux, UInt64, (Any, Csize_t), w, i - 1)

@inline _set_slot_aux!(w::WaitEntry1, i::Int, v::UInt64) = (w.aux1 = v; nothing)
@inline function _set_slot_aux!(w::WaitEntry2, i::Int, v::UInt64)
    i == 1 ? (w.aux1 = v) : (w.aux2 = v)
    return nothing
end
@inline _set_slot_aux!(w::WaitEntryN, i::Int, v::UInt64) =
    ccall(:jl_wait_entry_set_slot_aux, Cvoid, (Any, Csize_t, UInt64), w, i - 1, v)

# The slot registered on `owner`, or 0.
@inline _find_slot(w::WaitEntry1, @nospecialize(owner)) =
    (@atomic :monotonic w.owner1) === owner ? 1 : 0
@inline _find_slot(w::WaitEntry2, @nospecialize(owner)) =
    (@atomic :monotonic w.owner1) === owner ? 1 :
    (@atomic :monotonic w.owner2) === owner ? 2 : 0
function _find_slot(w::WaitEntryN, @nospecialize(owner))
    for i in 1:_nslots(w)
        _slot_owner(w, i) === owner && return i
    end
    return 0
end

# The first free slot, or 0.
@inline _free_slot(w::WaitEntry1) = (@atomic :monotonic w.owner1) === nothing ? 1 : 0
@inline _free_slot(w::WaitEntry2) =
    (@atomic :monotonic w.owner1) === nothing ? 1 :
    (@atomic :monotonic w.owner2) === nothing ? 2 : 0
_free_slot(w::WaitEntryN) = _find_slot(w, nothing)

@noinline _slot_overflow_error() =
    throw(ConcurrencyViolationError("wait entry has no free slot for this registration"))

# Claim a free slot for `owner` (which must not already have one).
@inline function _acquire_slot!(w::WaitEntry, @nospecialize(owner))
    i = _free_slot(w)
    i == 0 && _slot_overflow_error()
    _set_slot_owner!(w, i, owner)
    return i
end

# A reference to one slot of a wait entry, with `owner`/`next`/`aux`
# property access (owner accesses are relaxed-atomic; see above), and a
# vector view of an entry's slots. `slots(w)[i].owner === src`,
# `slot.next = x`, `for slot in slots(w)` are the intended spellings for
# code that is generic over the slot count.
struct WaitSlotRef{T<:WaitEntry}
    entry::T
    i::Int
end
@inline function getproperty(s::WaitSlotRef, f::Symbol)
    f === :owner && return _slot_owner(getfield(s, :entry), getfield(s, :i))
    f === :next && return _slot_next(getfield(s, :entry), getfield(s, :i))
    f === :aux && return _slot_aux(getfield(s, :entry), getfield(s, :i))
    return getfield(s, f)
end
@inline function setproperty!(s::WaitSlotRef, f::Symbol, @nospecialize(v))
    if f === :owner
        _set_slot_owner!(getfield(s, :entry), getfield(s, :i), v)
    elseif f === :next
        _set_slot_next!(getfield(s, :entry), getfield(s, :i), v::Union{WaitEntry, Nothing})
    elseif f === :aux
        _set_slot_aux!(getfield(s, :entry), getfield(s, :i), v::UInt64)
    else
        throw(FieldError(WaitSlotRef, f))
    end
    return v
end

struct WaitSlots{T<:WaitEntry} <: AbstractVector{WaitSlotRef{T}}
    entry::T
end
size(s::WaitSlots) = (_nslots(getfield(s, :entry)),)
@inline getindex(s::WaitSlots, i::Int) = WaitSlotRef(getfield(s, :entry), i)
slots(w::WaitEntry) = WaitSlots(w)

# Free slot `i` of `w`: `next` and `aux` are cleared first (a freed slot's
# payload must not leak into its next registration - e.g. the uv write
# cancel flag, see base/stream.jl) and the `owner` witness last - its
# clearing is what hands the slot back for reuse.
@inline function _release_slot!(w::WaitEntry, i::Int)
    _set_slot_next!(w, i, nothing)
    _set_slot_aux!(w, i, UInt64(0))
    _set_slot_owner!(w, i, nothing)
    return nothing
end

# The entry after `w` on `owner`'s list (`nothing` at its end, or -
# defensively - when `w` has no slot for `owner`).
@inline function _next_on(w::WaitEntry, @nospecialize(owner))
    i = _find_slot(w, owner)
    return i == 0 ? nothing : _slot_next(w, i)
end

# Set/clear a bare waitee witness (a registration that marks the entry as
# in-use for `x` without linking it into any list - the libuv request
# waits): the slot's `owner` is the reuse gate, `next` stays free.
_set_wait_witness!(w::WaitEntry, @nospecialize(x)) = (_acquire_slot!(w, x); nothing)
function _clear_wait_witness!(w::WaitEntry, @nospecialize(x))
    i = _find_slot(w, x)
    i == 0 || _release_slot!(w, i)
    return nothing
end

# Release the waitee witness of a uv request wait from its completion
# callback, which holds the entry but not the waitee: uv entries carry one
# witness slot alongside an optional cancellation-source slot, which stays
# registered (sticky).
function _clear_uv_witness!(w::WaitEntry)
    for slot in slots(w)
        o = slot.owner
        (o === nothing || o isa CancellationTokenSource) && continue
        slot.next = nothing
        slot.aux = UInt64(0) # e.g. the uv write cancel flag; see _release_slot!
        slot.owner = nothing
        break
    end
    return nothing
end

# Return `waiter`'s cached wait entry for a *plain* (non-cancellable) park,
# or a fresh (and newly cached) one if the cached one is still in use.
#
# Plain parks arm a *distinct* entry from cancellable parks
# (`_cancel_wait_entry`), and that identity split is what keeps shields
# shielded: the cancellation walk's only sound eligibility gate is its
# expected-entry claim CAS, so an entry registered on a source must never
# be armed for a wait that is not cancellable under it. (Aux data outside
# the claim word - the severity floors - is read racily by the walk and may
# be judged against an adjacent arm of the same entry; that misfire is
# tolerable for a teardown wait's floor, never for a shield. See the model
# in tla/WaitClaim.tla: sharing one entry lets a walk claim a shielded
# re-arm using the previous arm's eligibility.)
function _cached_wait_entry(waiter::Task)
    w = waiter.cached_wait_entry
    if w isa WaitEntry1 && (@atomic :monotonic w.owner1) === nothing
        @atomic :monotonic w.task = waiter
    else
        w = WaitEntry1(waiter)
        waiter.cached_wait_entry = w
    end
    return w
end

# Return the entry for a cancellable park of `waiter` governed by `src`,
# with the minimum delivery severity staged on the source slot. Must run
# before the arm: the cancellation walk reads the slot's aux only through
# an armed entry, so it has to be in place when `waiting_on` is published.
# Convention: `min_severity` is the lowest severity that may wake the wait
# (inclusive comparisons on both the registration and walk sides); a
# teardown wait that already acknowledged a delivery at severity `s` stages
# `s + 0x01` so only an escalation wakes it.
#
# The cached entry is reused when its sticky source slot is compatible:
# already on `src` (the common case - a task parking repeatedly under its
# ambient token pays no registry work after the first park), or free. When
# it is bound to a *different* source (the task's governing token changed),
# the owner unregisters it - the one O(list) operation on a park path, paid
# once per token migration - and rebinds. A cache entry stuck on a wait
# queue (stale registration from an interrupted wait, not yet collected)
# leaves a fresh single-use entry whose source registration becomes an
# unarmed corpse for pruning.
function _cancel_wait_entry(waiter::Task, src::CancellationTokenSource,
                            min_severity::UInt8)
    w = waiter.cached_cancel_entry
    if w isa WaitEntry2 && (@atomic :monotonic w.owner1) === nothing
        o = @atomic :monotonic w.owner2
        if !(o === src || o === nothing)
            # rebind: physically drop the stale sticky registration first
            unregister_cancellation!(o::CancellationTokenSource, w)
        end
        @atomic :monotonic w.task = waiter
    elseif w isa WaitEntry2
        w = WaitEntry2(waiter)
    else
        w = WaitEntry2(waiter)
        waiter.cached_cancel_entry = w
    end
    w.aux2 = UInt64(min_severity)
    return w
end

@noinline function _wait_registration_error()
    throw(ConcurrencyViolationError("Task is already registered on a wait queue"))
end

# Publish `w` as `waiter`'s only armed wait registration.
function _arm_wait(waiter::Task, w::WaitEntry)
    armed = @atomicreplace :release :monotonic waiter.waiting_on nothing => w
    armed.success || _wait_registration_error()
    return w
end

# Claim the wake of the wait that `w` was registered for (returns whether the
# claim succeeded). `w` must be an entry armed for `t` by `_wait2`.
function claim_wait(t::Task, w::WaitEntry)
    return (@atomicreplace t.waiting_on w => nothing).success
end

## Waiter registration
#
# The waiter list is lock-free on every hot path (like the source's child
# list): registration is a CAS push of the entry onto `waiters_head`, a
# normal wakeup does no registry work at all (the entry stays registered -
# armed/unarmed is tracked by the task's `waiting_on` claim word), and a
# repeat park under the same source re-arms the already-registered entry
# with no shared-memory operation. Only walks (cancellation delivery,
# pruning, and the owner-side `unregister_cancellation!`) rewrite links,
# serialized by the source's `walk_lock`, which no park or wakeup ever
# takes.

# Serialize walks on `src` (cancellation delivery, pruning, owner-side
# unregistration). Never taken on park/wake paths; contended walkers sleep.
# The lock object is installed lazily by the first walker, so sources that
# are never walked stay two words smaller than a ReentrantLock.
function _walk_lock(src::CancellationTokenSource)
    l = @atomic :acquire src.walk_lock
    l === nothing || return l::ReentrantLock
    newl = ReentrantLock()
    old, ok = @atomicreplace :acquire_release :acquire src.walk_lock nothing => newl
    return ok ? newl : old::ReentrantLock
end
# A walk delivers (or collects for) cancellations, possibly of the very
# scope the walking task runs under: the acquire must be shielded.
_lock_walk(src::CancellationTokenSource) = lock(_walk_lock(src); cancel=nothing)
_trylock_walk(src::CancellationTokenSource) = trylock(_walk_lock(src))
_unlock_walk(src::CancellationTokenSource) = unlock(_walk_lock(src))

# Pruning: dead registrations (retired entries, entries of completed tasks)
# are counted where they die - `retire_cancellation_entry!` and the task
# teardown hook - and the incrementer that trips the threshold runs a prune
# walk, so corpses cannot accumulate without bound on a long-lived source
# that never gets cancelled. Sources are mostly scope-lifetime, so in
# practice the dominant reclaimer is the source dying.
const _PRUNE_DEAD_THRESHOLD = UInt32(16)

# Account a newly dead registration on `src` and prune when enough have
# accumulated. `>=`, not `==`: when the incrementer that crosses the
# threshold loses `_try_prune!`'s trylock to a concurrent walk, corpses that
# walk had already passed remain counted here, and the next death must retry
# the prune rather than let the count sail past the threshold forever.
function _note_dead_registration!(src::CancellationTokenSource)
    dc = @atomic :monotonic src.dead_count += UInt32(1)
    dc >= _PRUNE_DEAD_THRESHOLD && _try_prune!(src)
    return nothing
end

# Relaxed load of the waiter-list head (an `Any` field whose non-entry
# initial value means the empty list). The walk's seq_cst head read stays
# spelled out at its use site.
@inline function _waiters_head(src::CancellationTokenSource)
    h = @atomic :monotonic src.waiters_head
    return h isa WaitEntry ? h : nothing
end

# Register the armed wait entry `w` (see `_cancel_wait_entry`/`_arm_wait`)
# on `src`'s waiter list, so the cancellation walk can find and claim the
# parked task, and close the arm-vs-cancel race. Returns `false` when `src`
# was already cancelled at `w.min_severity` or above *and* this call claimed
# the wake back: the caller must withdraw its waitee-side state and deliver
# the refusal itself (`_deliver_refused_cancellation`); the sticky source
# registration stays. Returns `true` when the park should proceed -
# including when a concurrent cancellation walk claimed the freshly armed
# entry, in which case its wake delivers the request into the park.
function register_cancellation!(src::CancellationTokenSource, w::WaitEntry)
    i = _find_slot(w, src)
    if i == 0
        # First park under `src`: claim a slot (the slot's `owner` is the
        # push ticket - setting it before the publishing CAS orders it for
        # any walk that can see the entry) and publish the entry with a
        # lock-free push.
        slot = slots(w)[_acquire_slot!(w, src)]
        while true
            h = _waiters_head(src)
            slot.next = h
            # seq_cst, pairing with the cancellation walk's state-write-
            # then-head-read: if the walk's head read misses this push, this
            # push is later in the total order, so the state recheck below
            # observes the raised state (same dance as the child-attach in
            # jl_new_cancel_source).
            if (@atomicreplace :sequentially_consistent :monotonic src.waiters_head h => w).success
                break
            end
        end
    else
        slot = slots(w)[i]
        # Sticky re-arm (the entry is already registered on `src`): upgrade
        # this thread's arm-then-recheck to the store-load ordering the
        # race argument needs (the arm CAS itself is only `release`).
        Core.Intrinsics.atomic_fence(:sequentially_consistent, :system)
    end
    # Post-publication recheck: either the concurrent cancellation walk
    # observes our push/arm, or we observe its state write here.
    st = @atomic :sequentially_consistent src.state
    if st != 0x00 && st >= slot.aux % UInt8
        t = (@atomic :monotonic w.task)::Task
        if (@atomicreplace t.waiting_on w => nothing).success
            return false
        end
        # A concurrent walk claimed the wake first; its schedule delivers
        # the request into the park.
    end
    return true
end

# Owner-side physical unregistration of `w` from `src`'s waiter list: an
# O(list) walk under the walk lock. Used on the rare paths that must drop a
# sticky registration eagerly - rebinding the cached cancel entry after a
# token migration - not on any wakeup path (normal wakeups leave the
# registration in place). `w` must be unarmed and owned by the caller.
function unregister_cancellation!(src::CancellationTokenSource, w::WaitEntry)
    wi = _find_slot(w, src)
    wi == 0 && return nothing
    wslot = slots(w)[wi]
    _lock_walk(src)
    prev = nothing # the predecessor's slot for `src`, once past the head
    x = _waiters_head(src)
    while x isa WaitEntry
        xi = x === w ? wi : _find_slot(x, src)
        # a linked entry always has a slot for this source (see
        # _walk_waiters!); bail out without touching the structure otherwise
        xi == 0 && break
        slot = slots(x)[xi]
        xnext = slot.next
        if x === w
            if prev === nothing
                # racing pushes prepend; retrying against the fresh head
                # re-finds `w`'s predecessor
                if !(@atomicreplace :monotonic :monotonic src.waiters_head x => xnext).success
                    x = _waiters_head(src)
                    continue
                end
            else
                prev.next = xnext
            end
            wslot.next = nothing
            break
        end
        prev = slot
        x = xnext
    end
    wslot.owner = nothing
    _unlock_walk(src)
    return nothing
end

# Deliver the cancellation that made `register_cancellation!` refuse (the
# caller has already withdrawn its waitee-side registration and released
# any locks the throw must not hold).
@noinline function _deliver_refused_cancellation(src::CancellationTokenSource)
    checkcancel(src)
    error("cancellation registration refused, but the source is not cancelled")
end

# Mark the single-use entry `w` as done with its (sticky) source
# registrations: walks then collect it like an entry of a completed task.
# For per-call entries (`Experimental.wait_with_timeout`, waitany) whose
# task may live - and keep registering - indefinitely.
function retire_cancellation_entry!(w::WaitEntry)
    @atomic :monotonic w.task = nothing
    for slot in slots(w)
        o = slot.owner
        o isa CancellationTokenSource && _note_dead_registration!(o)
    end
    return nothing
end

# Prune walk: collect unarmed corpses (completed tasks, retired entries).
# Never delivers - delivery is `cancel!`'s walk, and any registrant racing
# a cancellation is covered by its own state recheck.
@noinline function _try_prune!(src::CancellationTokenSource)
    _trylock_walk(src) || return nothing  # a running walk collects anyway
    _walk_waiters!(src, 0x00)
    _unlock_walk(src)
    return nothing
end

## Cancellation
#
# Cancellation is uniformly level-triggered: while the governing token is
# cancelled, every cancellation point throws the `CancellationRequest`.

"""
    cancel!(src::CancellationTokenSource,
            request::CancellationRequest=CANCEL_REQUEST_SAFE)::Bool

Cancel `src` and all of its descendants at the given severity. Level-triggered
and monotonic: observers (including future registrants) see the cancellation
until the source is discarded, and repeated calls only have an effect when
they *escalate* the severity ([`CANCEL_REQUEST_SAFE`](@ref) ->
[`CANCEL_REQUEST_ABANDON_EXTERNAL`](@ref) ->
[`CANCEL_REQUEST_ABANDON_ALL`](@ref)). Returns whether the call changed the
state.

Computations governed by a token of `src` or a descendant observe the cancellation at
their cancellation points (see [`@cancel_check`](@ref)), which throw a
[`CancellationRequest`](@ref).

When `cancel!` returns, every source currently reachable from `src` has
been advanced to (at least) the requested severity by this very call.
"""
function cancel!(src::CancellationTokenSource,
                 request::CancellationRequest=CANCEL_REQUEST_SAFE)
    sev = request.request
    if !(sev == 0x1 || sev == 0x3 || sev == 0x4)
        throw(ArgumentError("invalid cancellation severity $(repr(request.request))"))
    end
    raised = _raise_state!(src, sev)
    # Mark the cancelled subgraph (waking parked waiters): each node is
    # marked before its children so a concurrent construction of a child
    # source is level-triggered.
    _cancel_walk!(src, sev)
    Threads.atomic_fence_heavy()
    # Shoot down any task now bound to a cancelled source: threads inside a
    # compiled cancellation region are asynchronously reset to their
    # cancellation point, which observes the cancellation and throws.
    # Best-effort: a task the walk misses recovers level-triggered at its
    # next cancellation point (or reset-safe allocation).
    ccall(:jl_shootdown_cancelled_tasks, Cvoid, ())
    return raised
end

function _cancel_walk!(src::CancellationTokenSource, sev::UInt8)
    # Iterative worklist (no recursion): a deep source chain must not
    # overflow the canceller's stack, and a reconverging ("linked") graph
    # must deliver at each node once, not once per path (deduplicated by the
    # visited set).
    visited = IdSet{CancellationTokenSource}()
    push!(visited, src)
    pending = CancellationTokenSource[src]
    while !isempty(pending)
        _cancel_walk_node!(pop!(pending), sev, pending, visited)
    end
    return nothing
end

# Walk `node`'s waiter list under the walk lock: physically unlink the
# registrations of completed tasks and retired entries (the only removal in
# the registry - a live task's registration is sticky and stays linked
# between parks) and, when `sev` is nonzero, claim armed waiters eligible
# at that severity. Returns the claimed tasks as a `(t, rest)` cons list;
# the caller wakes them after releasing the walk lock.
function _walk_waiters!(node::CancellationTokenSource, sev::UInt8)
    @atomic :monotonic node.dead_count = UInt32(0)
    towake = nothing
    prev = nothing # the predecessor's slot for `node`, once past the head
    # seq_cst: the S-ordered counterpart of a registrant's seq_cst push -
    # a push this read misses is later in the total order, so that
    # registrant's state recheck observes the cancellation (see
    # register_cancellation!); pairs with the seq_cst state read the caller
    # performed under the walk lock.
    x = @atomic :sequentially_consistent node.waiters_head
    w = x isa WaitEntry ? x : nothing
    while w isa WaitEntry
        wi = _find_slot(w, node)
        # a linked entry always has a slot for this source (its owner is
        # the membership witness, cleared only on unlink, under this lock);
        # bail out without touching the structure if that is ever violated
        wi == 0 && break
        slot = slots(w)[wi]
        wnext = slot.next
        t = @atomic :monotonic w.task
        if t === nothing || istaskdone(t)
            # Unlink. Interior links are rewritten only under the walk lock,
            # so they are plain stores; unlinking the head races concurrent
            # pushes and simply keeps the entry for the next walk when it
            # loses.
            unlinked = if prev === nothing
                (@atomicreplace :monotonic :monotonic node.waiters_head w => wnext).success
            else
                prev.next = wnext
                true
            end
            if unlinked
                # The slot `owner` is cleared last: it is the membership
                # witness whose clearing hands the (dead) slot back.
                slot.next = nothing
                t === nothing || (@atomic :monotonic w.task = nothing)
                slot.owner = nothing
            else
                prev = slot
            end
        else
            # Claim order: the armed check must come first - the slot's aux
            # (the minimum delivery severity) is only meaningful for the arm
            # it was staged for, and reading it after observing the arm pins
            # it to that arm or a later one. An arm this (seq_cst, pairing
            # with the re-arm fence in register_cancellation!) read misses
            # is covered by its own post-arm state recheck, so skipping it
            # here is sound. The claim CAS itself may still land on a
            # *later* arm of the same entry than the one whose aux was
            # judged - which is why every arm of a source-linked entry must
            # be a cancellable park under it (shields arm a distinct entry;
            # see _cached_wait_entry and tla/WaitClaim.tla): the worst
            # misfire is then a spurious below-floor wake into a teardown
            # re-park, which handles it like any interruption of its wait
            # (conservatively, e.g. by detaching the awaited request).
            if sev != 0x00 && (@atomic :sequentially_consistent t.waiting_on) === w &&
                    slot.aux % UInt8 <= sev
                if (@atomicreplace t.waiting_on w => nothing).success
                    towake = ((t, w), towake)
                end
                # a lost claim: a completion or interrupter won the race;
                # the waiter resumes through that wake
            end
            prev = slot
        end
        w = wnext
    end
    return towake
end

function _cancel_walk_node!(node::CancellationTokenSource, sev::UInt8,
                            pending::Vector{CancellationTokenSource},
                            visited::IdSet{CancellationTokenSource})
    _lock_walk(node)
    # Deliver at least the node's current severity: a concurrent higher-
    # severity cancel! may have raised the state after this walk's own
    # transition, and its walk can run before this one takes the walk lock -
    # the claims below must then honor the escalated request. (Read under
    # the walk lock so the claim section cannot act on a stale, lower
    # severity.) seq_cst, matching the C-side propagate: when this walk's
    # own raise of the node was lost, this load is the walk's sole operation
    # on the state that can order the winner's write before the child_head
    # read below; it also pairs with the seq_cst push/arm-fence in
    # `register_cancellation!` (state write before walk on this side, push
    # or arm before state recheck on the registrant's).
    st = @atomic :sequentially_consistent node.state
    sev < st && (sev = st)
    creq = CancellationRequest(sev)
    towake = _walk_waiters!(node, sev)
    # The actual wakes happen after the walk lock is released: holding it
    # across `schedule` would only stretch the window against concurrent
    # escalation walks. Claimed entries stay registered (sticky); the
    # claimed *waitee*-queue entry also stays linked, and the waiter's own
    # cleanup (or a later notify) lazily unlinks it.
    _unlock_walk(node)
    while towake !== nothing
        ((t, w), towake) = towake::Tuple{Tuple{Task, WaitEntry}, Any}
        # N.B.: an ABANDON_ALL request currently also delivers by
        # interruption; freezing the task in place instead arrives with the
        # escalation machinery. The delivery is claim-scoped - the claim
        # above is the wake ticket, so no re-claiming swap - and dropped
        # when the task has re-armed meanwhile (see deliver_claimed_wake!).
        deliver_claimed_wake!(t, w, creq)
    end
    # Walk the node's (weak, intrusive) child list, advancing every child to
    # this severity and queueing the ones not yet visited (a reconverging
    # graph must deliver at each node once, not once per path). The seq_cst
    # `child_head` read below (paired with the seq_cst state access above)
    # closes the race against a concurrent attach: a child that this read
    # misses was published after our state write, so its constructor
    # observes that write and the child is born at (at least) this severity.
    # Children attached concurrently *during* the walk are prepended before
    # the list positions already traversed and are likewise born cancelled.
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

# The slow path of `@cancel_check`: `st` is the (non-zero) status byte
# reported by the cancellation point.
@noinline function handle_cancellation!(src::Union{Nothing, CancellationTokenSource}, st::UInt8)
    ct = current_task()
    if st & STATUS_PREEMPT_BIT != 0x00
        # consume the cooperative-yield request
        @atomic :monotonic ct.preempt_request = 0x00
    end
    if st & SEVERITY_MASK == 0x00
        # preempt-only (a pending yield request, or a preempt shootdown that
        # reset this point): let another task run, then resume
        yield()
        return nothing
    end
    src = src::CancellationTokenSource
    # re-read: deliver the severity current at throw time, not the one the
    # fast path happened to observe
    st = @atomic :acquire src.state
    throw(CancellationRequest(st))
end

"""
    Core.cancellation_point!(src::Union{Nothing, Core.CancellationTokenSource})::UInt8

Check the cancellation state of `src` (see [`@cancel_check`](@ref)),
additionally giving the optimizer license to establish this point as a
cancellation reset point: when compiled, the source is published as the token
binding governing the current computation, and the runtime may asynchronously
unwind execution to the nearest preceding cancellation point when the source
is cancelled. Returns a status byte: `0x00` if nothing is pending, the
(nonzero) severity if `src` is cancelled, with the `0x40` bit set if a
cooperative yield (preemption) was requested.

!!! warning
    `src` must remain otherwise reachable (e.g. through the scope binding
    that supplied it, as [`@cancel_check`](@ref) guarantees) for the dynamic
    extent of the region this point establishes: copies of the binding saved
    by exception handlers are not GC-scanned.
"""
Core.cancellation_point!

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
        # compiled cancellation points are also GC safepoints, so that a tight
        # polling loop cannot starve a concurrent stop-the-world.
        # The loop re-executes the point whenever the slow path returns (a
        # preempt-only status yields and resumes): the slow-path call tears
        # down the point's reset region, so passing the point again is what
        # re-establishes it before the code the region is meant to cover.
        local s = default_cancel_source()
        while true
            local st = Core.cancellation_point!(s)::UInt8
            st == 0x00 && break
            handle_cancellation!(s, st)
        end
        nothing
    end
end

macro cancel_check(tok)
    quote
        local t = $(esc(tok))
        local s = t === nothing ? nothing : (t::CancellationToken).source
        while true
            local st = Core.cancellation_point!(s)::UInt8
            st == 0x00 && break
            handle_cancellation!(s, st)
        end
        nothing
    end
end

# Throw the `CancellationRequest` if `src` is cancelled (level-triggered:
# no per-task state is consulted). Unlike `@cancel_check` this is not a
# compiled cancellation point (it opens no async-interruptible region).
# This is the entry check of every blocking API taking a `cancel` keyword
# argument: it must run *before* the operation has any side effects.
@inline function checkcancel(src::CancellationTokenSource)
    st = @atomic :monotonic src.state
    st == 0x00 && return nothing
    handle_cancellation!(src, st)
    return nothing
end
checkcancel(::Nothing) = nothing
checkcancel(tok::CancellationToken) = checkcancel(tok.source)

## CANCEL_TOKEN
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

The current value can be read with `Base.CANCEL_TOKEN[]`.
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

# The source of a resolved token (`nothing` stays `nothing`).
cancel_source(tok::CancellationToken) = tok.source
cancel_source(::Nothing) = nothing

# The entry check of a public API taking a `cancel` keyword argument:
# resolve the token and throw if it is already cancelled (uniformly
# level-triggered for the scoped default and explicit tokens alike).
@inline function check_cancel_arg(cancel::CancelTokenArg)
    tok = resolve_cancel_token(cancel)
    tok === nothing || checkcancel(tok.source)
    return tok
end

# The lighter entry check for APIs with a non-blocking fast path: an
# explicitly passed token is resolved and checked (throwing when already
# cancelled) right here, while the scoped-default sentinel passes through
# untouched, deferring the scope lookup - and its (level-triggered) check -
# to the operation's first potential-block point, so fast paths pay neither.
# Spelled `cancel = precheck_cancel_arg(cancel)` at the top of such APIs.
@inline precheck_cancel_arg(cancel::CancelTokenArg) =
    cancel isa UseDefaultToken ? cancel : check_cancel_arg(cancel)
