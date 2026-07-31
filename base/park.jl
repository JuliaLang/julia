# This file is a part of Julia. License is MIT: https://julialang.org/license

## The parked-wait driver
#
# Every parked wait proceeds in six phases, uniformly over the set of
# things it waits for (see also the formal model of the claim protocol,
# tla/WaitClaim.tla, whose parker process is this driver):
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
#   wait_fired(x, w)                     fired outcome, after a WON
#                                        self-claim: predicate kinds
#                                        return, the source throws
#   wait_fired_throws(x) -> Bool         compile-time kind property: does
#                                        wait_fired throw? (drives the
#                                        full-withdraw-before-throw rule)
#   wait_dequeue!(x, w, why) -> Nothing  withdraw x's slot per policy
#   wait_release!(x) -> token            suspend bracket for protection
#   wait_reacquire!(x, token)            held across phases 4-5
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
const WAKE_FIRED       = 0x01  # fired; self-claim won; loop continues
const WAKE_REFUSED     = 0x02  # throwing fired (source refusal)
const WAKE_INTERRUPTED = 0x03  # exceptional wake (cleanup path)
const WAKE_WITHDRAWN   = 0x04  # withdraw! - the caller is done waiting

function wait_enqueue! end
function wait_dequeue! end
function wait_fired end
wait_recheck(@nospecialize(x), w::WaitEntry) = false
wait_fired_throws(@nospecialize(x)) = false
wait_release!(@nospecialize(x)) = nothing
wait_reacquire!(@nospecialize(x), token) = nothing

## The cancellation source as a waitable
#
# `SourceWait(src, floor)` in a park's waitables makes the wait
# cancellable under `src` at severities >= `floor`: its enqueue is the
# sticky lock-free registration, its recheck is the seq_cst state read
# closing the arm-vs-cancel race (the Dekker whose two seq_cst upgrades
# are shown necessary in tla/WaitDekkerTSO.tla), and its fired outcome
# throws the refusal. Registrations are sticky: no dequeue on any path -
# collection is the walk's job (prune/dead accounting).
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
    retire_cancellation_entry!(w)
    return nothing
end

# Self-claim: take back the wake of our own armed registration.
disarm!(ct::Task, w::WaitEntry) = (@atomicreplace ct.waiting_on w => nothing).success

## The verbs

"""
    park!(ws, [w::WaitEntry,] relock::Bool, first::Bool)

Park the current task on the waitables `ws` (any flat iterable - one
element per slot) through the six-phase protocol. Returns the consumed
wake's payload, or `nothing` when a waitable fired before the suspend
(callers rescan their own predicates; a value-carrying wait cannot be
ambiguous - its only fireable tuple-mate is the source, which throws).

`relock` selects whether the phase-4 protection is reacquired after the
wake (and kept across an exceptional unwind); `first` puts the waiter at
the front of FIFO waitee queues. Locks the caller holds across `park!`
follow the per-kind release/reacquire hooks; on the throwing fired path
(a source refusal) they are released first when `relock` is false.
"""
# The one-shot form owns the entry lifecycle: the entry never escapes,
# so a fresh one is retired on normal return (a cached one is left in
# its cache). The explicit-entry form leaves the lifecycle to the caller
# (the multi-wait loop retires through `withdraw!`). Exceptional exits
# retire through the refusal path or the interrupted cleanup.
@inline function park!(ws, relock::Bool, first::Bool)
    ct = current_task()
    w = acquire_wait_entry!(ct, ws)
    r = park!(ws, w, relock, first)
    release_wait_entry!(ct, w)
    return r
end

function park!(ws, w::WaitEntry, relock::Bool, first::Bool)
    ct = current_task()
    _arm_wait(ct, w)                                     # 3
    fired = false
    local fx
    for x in ws                                          # 4
        if !wait_enqueue!(x, w, first)
            fired = true; fx = x
            break
        end
    end
    if !fired
        for x in ws                                      # 5
            if wait_recheck(x, w)
                fired = true; fx = x
                break
            end
        end
    end
    if fired && disarm!(ct, w)
        if wait_fired_throws(fx)
            # the throwing fired path (the refusal): full withdrawal under
            # the still-held phase-4 protection, release per policy, throw
            for x in ws
                wait_dequeue!(x, w, WAKE_REFUSED)
            end
            release_wait_entry!(ct, w)
            if !relock
                for x in ws
                    wait_release!(x)
                end
            end
            wait_fired(fx, w)
            error("wait_fired of a throwing waitable kind returned")
        end
        # returning fired: eagerly dequeue the fired slot (so a later
        # repark! recheck cannot re-fire on it) and let the caller rescan
        wait_dequeue!(fx, w, WAKE_FIRED)
        return nothing
    end
    # not fired, or the self-claim lost (a claimer owns our wake): suspend
    toks = map(wait_release!, ws)                        # 6
    local r
    try
        r = wait()
    catch
        interrupted_park_cleanup!(ct, ws, toks, w, relock)
        rethrow()
    end
    if relock
        for (x, t) in zip(ws, toks)
            wait_reacquire!(x, t)
        end
        for x in ws
            wait_dequeue!(x, w, WAKE_VALUE)
        end
    end
    return r
end

"""
    repark!(ws, w::WaitEntry)

Re-park on the still-enqueued registration `w`: arm, run the phase-5
rechecks, and suspend unless one fired. For the multi-wait loop - the
caller's bookkeeping between wakes runs unarmed (a completion landing
there pops-and-drops the unarmed entry; the recheck here catches the
fired predicate before suspending, so nothing is lost). All of `ws` must
be transient kinds: nothing may be held across the suspend.
"""
function repark!(ws, w::WaitEntry)
    ct = current_task()
    _arm_wait(ct, w)
    fired = false
    local fx
    for x in ws
        if wait_recheck(x, w)
            fired = true; fx = x
            break
        end
    end
    if fired && disarm!(ct, w)
        if wait_fired_throws(fx)
            for x in ws
                wait_dequeue!(x, w, WAKE_REFUSED)
            end
            release_wait_entry!(ct, w)
            wait_fired(fx, w)
            error("wait_fired of a throwing waitable kind returned")
        end
        wait_dequeue!(fx, w, WAKE_FIRED)
        return nothing
    end
    local r
    try
        r = wait()
    catch
        interrupted_park_cleanup!(ct, ws, nothing, w, true)
        rethrow()
    end
    return r
end

"""
    withdraw!(ws, w::WaitEntry)

Leave the wait: dequeue every registration per its policy and retire the
entry if it was fresh. With `repark!` owning the arm, every caller
decision point runs unarmed, so no disarm is needed here.
"""
function withdraw!(ws, w::WaitEntry)
    for x in ws
        wait_dequeue!(x, w, WAKE_WITHDRAWN)
    end
    release_wait_entry!(current_task(), w)
    return nothing
end

# Cleanup of a park that was resumed without a wake having been delivered
# through its registration `w`: an interrupter claimed the wake (leaving
# the entry linked for us to clean up), the task got a raw `throwto`, or
# a cancellation was delivered. The caller rethrows afterwards. In order:
#  1. Disarm the registration - before the reacquire in step 3 can
#     register a new wait. When the disarm loses, a claimer got the wake:
#     its schedule is either already enqueued or still in flight under
#     the waitee's lock.
#  2. Blank the entry's cache slot: a notifier may have popped the stale
#     entry without scheduling us and may still retain its identity for
#     the wake-claim CAS, so `w` must not be reused (e.g. by a park
#     inside the reacquire) before the unlink below.
#  3. Reacquire per kind and unlink `w` (a no-op where a `notify` already
#     popped and dropped it).
#  4. Drop a claimed-and-enqueued wake this unwind will never consume, so
#     a later wait of this task does not consume it spuriously. This runs
#     after the reacquire on purpose: a claimer that claimed our wake did
#     so under the waitee's protection, so once the reacquire returns its
#     enqueue has landed and the drop is deterministic - dropping before
#     the reacquire would race the in-flight schedule and leak the wake
#     into the task's next wait. (If the reacquire itself parked and
#     consumed the stale wake as a spurious lock wake, its acquire loop
#     re-tested and re-parked - lock parks tolerate spurious wakes - and
#     there is nothing left to drop. A claim-less raw wake delivered
#     outside any lock - the documented-unsafe `schedule(t, exc,
#     error=true)` of a running task - can still land after this drop;
#     that hazard is the primitive's, not this path's.)
#  5. Now `w` is safe to reuse: restore it to its cache unless the
#     reacquire parked and cached a replacement - then `w` is unreachable
#     garbage (unarmed, off every waitq), so retire it: its sticky source
#     registration must be counted dead for pruning, or a long-lived
#     source would retain the task through the orphaned entry. Fresh
#     entries are always retired.
function interrupted_park_cleanup!(ct::Task, ws, toks, w::WaitEntry, relock::Bool)
    @atomicreplace ct.waiting_on w => nothing
    was_plain = ct.cached_wait_entry === w
    was_cancel = !was_plain && ct.cached_cancel_entry === w
    was_plain && (ct.cached_wait_entry = nothing)
    was_cancel && (ct.cached_cancel_entry = nothing)
    if toks === nothing
        for x in ws
            wait_reacquire!(x, nothing)
        end
    else
        for (x, t) in zip(ws, toks)
            wait_reacquire!(x, t)
        end
    end
    for x in ws
        wait_dequeue!(x, w, WAKE_INTERRUPTED)
    end
    q = ct.queue
    q === nothing || list_deletefirst!(q::StickyWorkqueue, ct)
    if !relock
        for x in ws
            wait_release!(x)
        end
    end
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
        retire_cancellation_entry!(w)
    end
    return nothing
end

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

wait_fired_throws(x::SourceWait) = true
wait_fired(x::SourceWait, w::WaitEntry) = _deliver_refused_cancellation(x.src)
wait_dequeue!(x::SourceWait, w::WaitEntry, why::UInt8) = nothing
