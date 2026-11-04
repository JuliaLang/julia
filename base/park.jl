# This file is a part of Julia. License is MIT: https://julialang.org/license

## The parked-wait driver
#
# Every parked wait proceeds in six phases, uniformly over the set of
# things it waits for:
#
#   1. CHECK    the caller's fast-path satisfaction tests - no allocation,
#               no publication (stays at the call sites).
#   2. ENTRY    allocate the wait entry or reuse the task's cached one;
#               stage per-slot aux (severity floors) that the claim side
#               may read through an already-linked entry.
#   3. ARM      publish the entry as the task's sole claimable
#               registration: CAS(t.waiting_on, nothing => w).
#   4. ENQUEUE  register the entry with every waitable, each under that
#               waitable's own protection discipline.
#   5. RECHECK  re-test satisfaction where the enqueue could have raced
#               the firing; on a hit, self-claim: won => withdraw and
#               return (or deliver the refusal); lost => a waker owns our
#               wake - suspend and consume it.
#   6. SUSPEND  release the locks that must not be held while parked,
#               `wait()`, then reacquire per policy and settle.
#
# A *waitable* is the identity a slot's `owner` records. The waitable
# protocol is ordinary generic functions - the set is open; each call
# site's concrete container type carries the dispatch:
#
#   wait_enqueue!(x, w, first) -> Bool   phase 4, under x's discipline;
#                                        false = x already fired and
#                                        declined (one-shot waitables)
#   wait_recheck(x, w) -> Bool           phase 5, after ALL enqueues;
#                                        vacuous default
#   wait_dequeue!(x, w, why) -> Nothing  withdraw x's slot. Lock
#                                        discipline is per `why`: on
#                                        WAKE_VALUE/WAKE_FIRED the caller
#                                        holds the kind's protection; on
#                                        WAKE_INTERRUPTED/WAKE_WITHDRAWN
#                                        the method takes it itself
#
# The driver takes and releases no locks: lock choreography is plain
# caller code (`unlockall`/`relockall` around `wait_safe_interrupt`,
# `iolock_end`/`iolock_begin` at the uv sites, ...), so there is no
# release/reacquire protocol and no relock policy.
#
# Lock rule: from phase 3 on the entry is the task's armed registration,
# so a lock acquisition *inside* phase 4 must never park (a nested park
# would try to arm a second registration). Parking locks must be
# caller-held before `park!` begins (the `wait(c)` contract); locks a
# `wait_enqueue!` method takes itself must be spin locks. The cleanup's
# reacquire is exempt: it runs disarmed and may itself park with a fresh
# entry (the cache-blank dance below exists for exactly that).

# why-codes for wait_dequeue!
const WAKE_VALUE       = 0x00  # normal wake consumed (lazy settle)
const WAKE_FIRED       = 0x01  # fired; self-claim won; no suspend
const WAKE_INTERRUPTED = 0x03  # exceptional wake (cleanup path)
const WAKE_WITHDRAWN   = 0x04  # withdraw! - the caller is done waiting

function wait_enqueue! end
function wait_dequeue! end
wait_recheck(@nospecialize(x), w::WaitEntry) = false

# Lock acquisition for self-protecting `wait_dequeue!` methods (the
# cleanup/withdraw paths): must not observe cancellation - the cleanup
# may be unwinding the very CancellationRequest a cancellable acquire
# would rethrow. The `ReentrantLock` method (lock.jl) shields itself.
_uncancellable_lock(l) = lock(l)

## The cancellation source as a waitable
#
# `SourceWait(src, floor)` in a park's waitables makes the wait
# cancellable under `src` at severities >= `floor`: its enqueue is the
# sticky lock-free registration, its recheck is the seq_cst state read
# closing the arm-vs-cancel race (a Dekker - both sides need their
# seq_cst upgrade), and its fired outcome throws the refusal.
# Registrations are sticky: no dequeue on any path - collection is the
# walk's job (prune/dead accounting).
struct SourceWait
    src::CancellationTokenSource
    floor::UInt8
end

## Entry acquisition - the cache contract
#
# The two canonical tuple shapes are served from the task's caches; any
# other iterable gets a fresh, single-use entry. Freshness is what makes
# specific-wait wakers (a timeout's expected-entry CAS) safe: entry
# identity scopes a claim to this wait - the same principle that gives
# shielded parks a distinct entry from cancellable ones.
acquire_wait_entry!(ct::Task, ws::Tuple{Any}) = _cached_wait_entry(ct)
acquire_wait_entry!(ct::Task, ws::Tuple{Any, SourceWait}) =
    _cancel_wait_entry(ct, ws[2].src, ws[2].floor)
acquire_wait_entry!(ct::Task, ws) = WaitEntryN(ct, length(ws))

# Retire a fresh (uncached) entry that is done waiting: its sticky source
# registrations become prunable corpses. Cached entries stay live.
function release_wait_entry!(ct::Task, w::WaitEntry)
    (w === ct.cached_wait_entry || w === ct.cached_cancel_entry) && return nothing
    # idempotent: several exits may withdraw the same fresh entry
    (@atomic :monotonic w.task) === nothing && return nothing
    retire_cancellation_entry!(w)
    return nothing
end

# Self-claim: take back the wake of our own armed registration.
disarm!(ct::Task, w::WaitEntry) = (@atomicreplace ct.waiting_on w => nothing).success

## The verbs

"""
    park!(ws, w::WaitEntry, first::Bool) -> Bool

Phases 3-5 of the protocol over the flat waitable iterable `ws`: arm
`w`, enqueue it with every waitable, and run the rechecks. Returns
`true` when the task is parked - a wake is (or will be) in flight, and
the caller must suspend through [`wait_safe_interrupt`](@ref) to consume
it. Returns `false` when a waitable fired and the self-claim won:
nothing is in flight, and the caller owns the outcome (for a fired
source, re-checking cancellation - `checkcancel` - throws the refusal
exactly like the entry check) as well as every remaining registration
(usually `withdraw!(ws, w, WAKE_FIRED)` under its still-held locks). A
fired recheck whose self-claim *loses* returns `true`: the concurrent
claimer's wake delivers the outcome.

The driver takes and releases no locks: protection the caller holds is
held throughout - which is what makes the enqueue/recheck window sound -
and lock choreography around the suspend is plain caller code. The one
dequeue the driver performs itself is the *fired slot* on the `false`
path (self-protecting; a done-but-still-linked predicate would re-fire
on every `repark!` until its notify drains).
"""
function park!(ws, w::WaitEntry, first::Bool)
    ct = current_task()
    _arm_wait(ct, w)                                     # 3
    fx = _enqueue_until_fired(ws, w, first)              # 4
    fx === nothing && (fx = _recheck_until_fired(ws, w)) # 5
    if fx !== nothing && disarm!(ct, w)
        wait_dequeue!(fx, w, WAKE_FIRED)
        return false
    end
    return true
end

# Per-waitable phases. Unrolled by tuple recursion: a generic `for x in ws`
# loop would re-box every element through its dynamic tuple index.
@inline _enqueue_until_fired(ws::Tuple{}, w::WaitEntry, first::Bool) = nothing
@inline function _enqueue_until_fired(ws::Tuple, w::WaitEntry, first::Bool)
    x = ws[1]
    wait_enqueue!(x, w, first) || return x
    return _enqueue_until_fired(tail(ws), w, first)
end
@inline function _enqueue_until_fired(ws, w::WaitEntry, first::Bool)
    for x in ws
        wait_enqueue!(x, w, first) || return x
    end
    return nothing
end
@inline _recheck_until_fired(ws::Tuple{}, w::WaitEntry) = nothing
@inline function _recheck_until_fired(ws::Tuple, w::WaitEntry)
    x = ws[1]
    wait_recheck(x, w) && return x
    return _recheck_until_fired(tail(ws), w)
end
@inline function _recheck_until_fired(ws, w::WaitEntry)
    for x in ws
        wait_recheck(x, w) && return x
    end
    return nothing
end
@inline _dequeue_each!(ws::Tuple{}, w::WaitEntry, why::UInt8) = nothing
@inline function _dequeue_each!(ws::Tuple, w::WaitEntry, why::UInt8)
    wait_dequeue!(ws[1], w, why)
    return _dequeue_each!(tail(ws), w, why)
end
@inline function _dequeue_each!(ws, w::WaitEntry, why::UInt8)
    for x in ws
        wait_dequeue!(x, w, why)
    end
    return nothing
end

"""
    repark!(ws, w::WaitEntry) -> Bool

Re-park on the still-enqueued registration `w`: arm and recheck, with
the same `Bool` contract as [`park!`](@ref). For the multi-wait loop -
the caller's bookkeeping between wakes runs unarmed (a completion
landing there pops-and-drops the unarmed entry; the recheck here catches
the fired predicate before suspending, so nothing is lost).
"""
function repark!(ws, w::WaitEntry)
    ct = current_task()
    _arm_wait(ct, w)
    fx = _recheck_until_fired(ws, w)
    if fx !== nothing && disarm!(ct, w)
        wait_dequeue!(fx, w, WAKE_FIRED)
        return false
    end
    return true
end

"""
    wait_safe_interrupt(ws, w::WaitEntry)

Phase 6: suspend and consume exactly one wake of the park `park!`
armed, returning its payload. This is the only legal way to suspend on
an armed park - a raw `wait()` would miss the interrupted-wait cleanup.
The caller must have released any parking locks it holds (plain caller
code, e.g. `unlockall`); on a normal wake it returns with no locks
touched, and the caller reacquires per its own contract.

On an exceptional resume (an interrupter's claim, a delivered
cancellation, a raw `throwto`) the cleanup runs here, then the exception
propagates: disarm; blank the entry's cache slot; withdraw every
registration through its kind's *self-protecting* dequeue - each takes
its own lock, and those round-trips serialize any claimer's in-flight
schedule - and only then drop a claimed-and-enqueued wake this unwind
will never consume (dropping earlier would race the in-flight claimer
and leak the wake into the task's next park); finally restore or retire
the entry. Because the registrations are already withdrawn when the
exception reaches the caller, its catch owes nothing to the protocol -
it only restores whatever lock contract its own callers require
(reacquiring shielded, and only what that contract demands: an unwind
that is itself a cancellation should not sleep on locks it does not
need).
"""
function wait_safe_interrupt(ws, w::WaitEntry)
    ct = current_task()
    local r
    try
        r = wait()
    catch
        interrupted_park_cleanup!(ct, ws, w)
        rethrow()
    end
    return r
end

"""
    withdraw!(ws, w::WaitEntry, why::UInt8=WAKE_WITHDRAWN)

Withdraw every registration of `w` per its kind's policy and release the
entry (retiring it when fresh; idempotent). The lock discipline follows
`why`: `WAKE_VALUE`/`WAKE_FIRED` run under the caller's still-held
protection (the lazy settle after a wake; the fired branch), while
`WAKE_WITHDRAWN` dequeues are self-protecting (leaving a multi-wait).
"""
function withdraw!(ws, w::WaitEntry, why::UInt8=WAKE_WITHDRAWN)
    _dequeue_each!(ws, w, why)
    release_wait_entry!(current_task(), w)
    return nothing
end

# The interrupted-wait cleanup (see wait_safe_interrupt). In order:
#  1. Disarm the registration - before any reacquire below can register a
#     new wait. When the disarm loses, a claimer got the wake: its
#     schedule is either already enqueued or still in flight under the
#     waitee's protection.
#  2. Blank the entry's cache slot: a notifier may have popped the stale
#     entry without scheduling us and may still retain its identity for
#     the wake-claim CAS, so `w` must not be reused (e.g. by a park
#     inside a self-protecting dequeue's lock acquire) before the
#     unlinks below.
#  3. Withdraw every registration through its kind's self-protecting
#     dequeue; the per-kind lock round-trips serialize any claimer's
#     in-flight schedule ...
#  4. ... which is what makes the pending-wake drop here deterministic:
#     a wake claimed-and-enqueued by such a claimer must not leak into
#     this task's next wait. (A claim-less raw wake delivered outside any
#     lock - the documented-unsafe `schedule(t, exc, error=true)` of a
#     running task - can still land after this drop; that hazard is the
#     primitive's, not this path's.)
#  5. Restore `w` to its cache slot unless a nested park cached a
#     replacement - then `w` is unreachable garbage (unarmed, off every
#     waitq), so retire it; fresh entries are always retired.
function interrupted_park_cleanup!(ct::Task, ws, w::WaitEntry)
    @atomicreplace ct.waiting_on w => nothing
    was_plain = ct.cached_wait_entry === w
    was_cancel = !was_plain && ct.cached_cancel_entry === w
    was_plain && (ct.cached_wait_entry = nothing)
    was_cancel && (ct.cached_cancel_entry = nothing)
    _dequeue_each!(ws, w, WAKE_INTERRUPTED)
    q = ct.queue
    q === nothing || list_deletefirst!(q::StickyWorkqueue, ct)
    if was_plain
        if ct.cached_wait_entry === nothing
            ct.cached_wait_entry = w
        else
            retire_cancellation_entry!(w)
        end
    elseif was_cancel
        if ct.cached_cancel_entry === nothing
            ct.cached_cancel_entry = w
        else
            retire_cancellation_entry!(w)
        end
    else
        release_wait_entry!(ct, w)
    end
    return nothing
end

## SourceWait methods (the struct and its doc live above, before the
## entry-acquisition contract that dispatches on it)

function wait_enqueue!(x::SourceWait, w::WaitEntry, first::Bool)
    src = x.src
    i = _find_slot(w, src)
    if i == 0
        # First registration under `src`: claim a slot (the slot's `owner`
        # is the push ticket) and stage the floor - pre-publication, so
        # any walk that can see the slot sees its aux - then publish with
        # a lock-free push. seq_cst, pairing with the cancellation walk's
        # state-write-then-head-read: if the walk's head read misses this
        # push, this push is later in the total order, so the recheck
        # below observes the raised state.
        i = _acquire_slot!(w, src)
        _set_slot_aux!(w, i, UInt64(x.floor))
        slot = slots(w)[i]
        while true
            h = _waiters_head(src)
            slot.next = h
            if (@atomicreplace :sequentially_consistent :monotonic src.waiters_head h => w).success
                break
            end
        end
        # approximate list length, feeding the scaled prune threshold
        # (resynced by every walk; see _note_dead_registration!)
        @atomic :monotonic src.reg_count += UInt32(1)
    else
        # Sticky re-arm (already registered): upgrade this thread's
        # arm-then-recheck to the store-load ordering the race argument
        # needs (the arm CAS itself is only `release`).
        Core.Intrinsics.atomic_fence(:sequentially_consistent, :system)
    end
    return true
end

function wait_recheck(x::SourceWait, w::WaitEntry)
    # Post-publication recheck: either the concurrent cancellation walk
    # observes our push/arm, or we observe its state write here.
    st = @atomic :sequentially_consistent x.src.state
    return st != 0x00 && st >= x.floor
end

wait_dequeue!(x::SourceWait, w::WaitEntry, why::UInt8) = nothing
