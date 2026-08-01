# This file is a part of Julia. License is MIT: https://julialang.org/license

## thread/task locking abstraction

@noinline function concurrency_violation()
    # can be useful for debugging
    #try; error(); catch; ccall(:jlbacktrace, Cvoid, ()); end
    throw(ConcurrencyViolationError("lock must be held"))
end

"""
    AbstractLock

Abstract supertype describing types that
implement the synchronization primitives:
[`lock`](@ref), [`trylock`](@ref), [`unlock`](@ref), and [`islocked`](@ref).
"""
abstract type AbstractLock end
function lock end
function unlock end
function trylock end
function islocked end
unlockall(l::AbstractLock) = unlock(l) # internal function for implementing `wait`
relockall(l::AbstractLock, state::Nothing) = lock(l) # internal function for implementing `wait`
# Restore all but one level of a hold captured by `unlockall` - the
# enclosing frames' levels, consuming the waiting frame's own - for the
# exceptional unwind out of the internal wait layer. A no-op when that
# frame's level was the only one (the common case: a depth-1 cancellation
# unwind performs no lock operation at all). The state stays opaque;
# shielded like `relockall` - a restore has no correct cancellable use.
relockall_but_one(l::AbstractLock, state::Nothing) = nothing
assert_havelock(l::AbstractLock, tid::Integer) =
    (islocked(l) && tid == Threads.threadid()) ? nothing : concurrency_violation()
assert_havelock(l::AbstractLock, tid::Task) =
    (islocked(l) && tid === current_task()) ? nothing : concurrency_violation()
assert_havelock(l::AbstractLock, tid::Nothing) = concurrency_violation()

"""
    AlwaysLockedST

This struct does not implement a real lock, but instead
pretends to be always locked on the original thread it was allocated on,
and simply ignores all other interactions.
It also does not synchronize tasks; for that use a real lock such as [`ReentrantLock`](@ref).
This can be used in the place of a real lock to, instead, simply and cheaply assert
that the operation is only occurring on a single cooperatively-scheduled thread.
It is thus functionally equivalent to allocating a real, recursive, task-unaware lock
immediately calling `lock` on it, and then never calling a matching `unlock`,
except that calling `lock` from another thread will throw a concurrency violation exception.
"""
struct AlwaysLockedST <: AbstractLock
    ownertid::Int16
    AlwaysLockedST() = new(Threads.threadid())
end
assert_havelock(l::AlwaysLockedST) = assert_havelock(l, l.ownertid)
lock(l::AlwaysLockedST) = assert_havelock(l)
unlock(l::AlwaysLockedST) = assert_havelock(l)
trylock(l::AlwaysLockedST) = l.ownertid == Threads.threadid()
islocked(::AlwaysLockedST) = true


## condition variables

# (The WaitEntry registration type and the wake-claim protocol live in
# cancellation.jl, which is included earlier in bootstrap: registrations
# carry the cancellation half of a parked wait.)

"""
    GenericCondition

Abstract implementation of a condition object
for synchronizing task objects with a given lock.
"""
mutable struct GenericCondition{L<:AbstractLock}
    # mutable for identity only
    const waitq::IntrusiveLinkedList{WaitEntry}
    const lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{WaitEntry}(), L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{WaitEntry}(), l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}(IntrusiveLinkedList{WaitEntry}(), l)
end

waitqueue(c::GenericCondition) = ILLRef(c.waitq, c)

"""
    try_unlink_claimed!(w::WaitEntry)

Opportunistically attempt to unlink a wait entry from its queue. This is a memory pressure
optimization. If the queue is locked by another task, the entry will remain linked and will
be unlinked upon the next wakeup attempt.
"""
function try_unlink_claimed!(w::WaitEntry)
    ok = true
    for slot in slots(w)
        q = slot.owner
        q === nothing && continue
        # Manual split for --trim (every waitq's identity is its
        # condition - see waitqueue)
        if q isa GenericCondition{Threads.SpinLock}
            _try_unlink_from!(q, w) || (ok = false)
        elseif q isa GenericCondition{ReentrantLock}
            _try_unlink_from!(q, w) || (ok = false)
        elseif q isa GenericCondition{AlwaysLockedST}
            _try_unlink_from!(q, w) || (ok = false)
        elseif q isa CancellationTokenSource
            # sticky source registrations stay in place
        else
            ok = false
        end
    end
    return ok
end

function _try_unlink_from!(c::GenericCondition, w::WaitEntry)
    trylock(c.lock) || return false
    try
        list_deletefirst!(waitqueue(c), w)
    finally
        unlock(c.lock)
    end
    return true
end

show(io::IO, c::GenericCondition) = print(io, GenericCondition, "(", c.lock, ")")

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
# (the `cancel`-forwarding lock method for ReentrantLock-backed conditions
# lives in lock.jl, after ReentrantLock is defined)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

lock(f, c::GenericCondition) = lock(f, c.lock)

## GenericCondition as a waitable (see base/park.jl): the lock-carried
## kind - its lock is caller-held from before phase 4 into the suspend
## bracket, which is what makes its recheck vacuous.
function wait_enqueue!(c::GenericCondition, w::WaitEntry, first::Bool)
    if first
        pushfirst!(waitqueue(c), w)
    else
        push!(waitqueue(c), w)
    end
    return true
end
function wait_dequeue!(c::GenericCondition, w::WaitEntry, why::UInt8)
    # a no-op when a notify already popped the entry. WAKE_VALUE/WAKE_FIRED
    # run under the caller's held lock (the settle after a wake; the fired
    # branch); the cleanup/withdraw whys take it themselves - shielded, a
    # cleanup may be unwinding the very request a cancellable acquire would
    # rethrow
    if why == WAKE_INTERRUPTED || why == WAKE_WITHDRAWN
        _uncancellable_lock(c.lock)
        try
            list_deletefirst!(waitqueue(c), w)
        finally
            unlock(c.lock)
        end
    else
        list_deletefirst!(waitqueue(c), w)
    end
    return nothing
end

@noinline _fresh_waiter_error() =
    throw(ConcurrencyViolationError("schedule_on_notify! requires a fresh task: never started, scheduled, or armed"))

# Freshness is stricter than `!istaskstarted`: a task `schedule`/`@async`
# has merely ENQUEUED hasn't run yet, but arming it here would collide
# with the arm its own first park performs once it starts (that park's
# `_arm_wait` CAS fails and unwinds a wait's lock choreography from the
# outside). Reject anything started, queued, or already armed.
_assert_fresh_waiter(waiter::Task) =
    if istaskstarted(waiter) || waiter.queue !== nothing ||
       (@atomic :monotonic waiter.waiting_on) !== nothing
        _fresh_waiter_error()
    end

# Start `waiter` with the (level-triggered) cancellation of its birth
# source: a subscribed task whose governing source is cancelled dies
# instead of running its body - a never-started task raises the scheduled
# exception at start.
function _schedule_subscription_cancelled(waiter::Task, src::CancellationTokenSource)
    st = @atomic :acquire src.state
    schedule(waiter, CancellationRequest(st), error=true)
    return nothing
end

# Subscribe the not-yet-started task `waiter` to `c`'s next notify: arm
# its registration and enqueue it, so the notify's claim-and-schedule is
# the task's first schedule - a start trigger; the current task does not
# suspend. Waits of the *current* task go through `park!` (base/park.jl).
#
# Subscriptions are governed by the *waiter's* birth cancellation source
# (the CANCEL_TOKEN of the scope captured at its construction): if that
# source is - or becomes, while still subscribed - cancelled, the task
# dies with the CancellationRequest instead of starting. Cleanup-class
# subscribers (Timer/AsyncCondition callback tasks, channel close hooks,
# errormonitor, REPL teardown) are constructed under
# `CANCEL_TOKEN => nothing` and so are never killed this way.
# Returns the registration entry (or `nothing` when the subscription was
# refused and the waiter scheduled to die).
function schedule_on_notify!(c::GenericCondition, waiter::Task, first::Bool=false)
    assert_havelock(c)
    _assert_fresh_waiter(waiter)
    src = _birth_cancel_source(waiter)
    if src !== nothing && iscancelled(src)
        # born cancelled: never enqueue anything
        _schedule_subscription_cancelled(waiter, src)
        return nothing
    end
    w = src === nothing ? _cached_wait_entry(waiter) :
                          _cancel_wait_entry(waiter, src, 0x00)
    _arm_wait(waiter, w)
    if first
        pushfirst!(waitqueue(c), w)
    else
        push!(waitqueue(c), w)
    end
    if src !== nothing
        # the sticky source registration + the publish-then-recheck dance,
        # exactly like a park's phases 4-5 - but claiming back the *waiter's*
        # arm on refusal
        sw = SourceWait(src, 0x00)
        wait_enqueue!(sw, w, false)
        if wait_recheck(sw, w) && disarm!(waiter, w)
            list_deletefirst!(waitqueue(c), w)   # under the held lock
            _schedule_subscription_cancelled(waiter, src)
            return nothing
        end
        # a lost disarm means a concurrent walk claimed the fresh arm: its
        # delivery kills the waiter at start
    end
    # since this is similar to schedule, we should observe the sticky bit now
    if waiter.sticky && Threads.threadid(waiter) == 0 && !GC.in_finalizer()
        # Issue #41324
        # t.sticky && tid == 0 is a task that needs to be co-scheduled with
        # the parent task. If the parent (current_task) is not sticky we must
        # set it to be sticky.
        # XXX: Ideally we would be able to unset this
        current_task().sticky = true
        tid = Threads.threadid()
        ccall(:jl_set_task_tid, Cint, (Any, Cint), waiter, tid-1)
    end
    return w
end

"""
    wait([x])

Block the current task until some event occurs.

* [`Channel`](@ref): Wait for a value to be appended to the channel.
* [`Condition`](@ref): Wait for [`notify`](@ref) on a condition and return the `val`
  parameter passed to `notify`. See the `Condition`-specific docstring of `wait` for
  the exact behavior.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](@ref): Wait for a `Task` to finish. See the `Task`-specific docstring of `wait` for
  the exact behavior.
* [`RawFD`](@ref): Wait for changes on a file descriptor (see the `FileWatching` package).

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to [`schedule`](@ref) or [`yieldto`](@ref).

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before
proceeding.
"""
function wait end

"""
    wait(c::GenericCondition; first::Bool=false, cancel=Base.DEFAULT_CANCEL)

Wait for [`notify`](@ref) on `c` and return the `val` parameter passed to `notify`.

If the keyword `first` is set to `true`, the waiter will be put _first_
in line to wake up on `notify`. Otherwise, `wait` has first-in-first-out (FIFO) behavior.

The `cancel` keyword argument controls which cancellation token may interrupt
the wait (throwing the [`CancellationRequest`](@ref) into the waiter): by
default the scoped token (see `Base.CANCEL_TOKEN`); pass a
[`CancellationToken`](@ref) to override it, or `nothing` to make the wait
non-cancellable.
"""
function wait(c::GenericCondition; first::Bool=false,
              cancel::CancelTokenArg=DEFAULT_CANCEL)
    # Check the caller contract here, not just in the internal layer: a
    # violation (not locked, or locked by another task) must propagate
    # with the lock state untouched - the restore in the catch below is
    # only correct for throws that consumed this frame's lock level.
    assert_havelock(c)
    tok = check_cancel_arg(cancel)   # an entry refusal throws lock-held
    try
        return wait(c, tok; first)
    catch
        # the internal layer throws having released this frame's lock
        # level; the public contract is rethrow-with-lock-held (callers
        # are written `lock(c); try ... finally unlock(c)`), so restore
        # one shielded level
        _uncancellable_lock(c.lock)
        rethrow()
    end
end

# (The interrupted-wait cleanup lives in base/park.jl as
# interrupted_park_cleanup!, shared by every park site.)

# `min_severity` is the lowest severity that may wake (cancel) this wait -
# the comparisons are inclusive on both the registration and walk sides. A
# teardown wait that re-parks after acknowledging a delivery at severity
# `s` must therefore pass `s + 0x01` (exclusive staging), so a re-cancel at
# the acknowledged severity leaves it parked and only an escalation wakes
# it; see e.g. _uv_write_cancelled_finish.
function wait(c::GenericCondition, tok::MaybeToken; first::Bool=false,
              min_severity::UInt8=0x00)
    ct = current_task()
    assert_havelock(c)
    src = cancel_source(tok)
    # Entry check: throw before enqueueing anything (skipped for teardown
    # waits that re-park after acknowledging a severity). Like every throw
    # out of this internal layer, the waiting frame's own lock level is
    # released first - callers use the `locked && unlock` idiom; the
    # public kwarg method shims the old lock-held contract back on.
    if src !== nothing && min_severity == 0x00 && iscancelled(src)
        unlock(c.lock)
        checkcancel(src)
        error("cancelled source did not throw")
    end
    if src === nothing
        ws = (c,)
        w = _cached_wait_entry(ct)
    else
        ws = (c, SourceWait(src, min_severity))
        w = _cancel_wait_entry(ct, src, min_severity)
    end
    local parked::Bool
    try
        parked = park!(ws, w, first)
    catch
        # an arm-phase throw (contract misuse: this task already carries
        # a foreign registration) happens before any suspend. Nothing is
        # enqueued to settle, but this frame's lock level must still be
        # consumed like every other throw out of this layer - the
        # caller's `locked && unlock` idiom would otherwise leak a held
        # lock, and a leaked SpinLock wedges the process on the next
        # lock of the same object
        disarm!(ct, w)
        withdraw!(ws, w, WAKE_FIRED)
        unlock(c.lock)
        rethrow()
    end
    if !parked
        # the source refused at the registration recheck (the only
        # fireable waitable here): withdraw under the still-held lock,
        # release this frame's level, and deliver like the entry check
        withdraw!(ws, w, WAKE_FIRED)
        unlock(c.lock)
        checkcancel(src)
        error("park fired without a cancelled source")
    end
    lockstate = unlockall(c.lock)
    r = try
        wait_safe_interrupt(ws, w)
    catch
        # the cleanup already withdrew every registration; restore the
        # enclosing frames' hold, consuming this waiting frame's level -
        # a depth-1 unwind (the common case, e.g. the cancellation being
        # delivered) touches no lock at all
        relockall_but_one(c.lock, lockstate)
        rethrow()
    end
    relockall(c.lock, lockstate)
    withdraw!(ws, w, WAKE_VALUE)   # lazy settle under the reacquired lock
    return r
end

"""
    notify(condition, val=nothing; all=true, error=false)

Wake up tasks waiting for a condition, passing them `val`. If `all` is `true` (the default),
all waiting tasks are woken, otherwise only one is. If `error` is `true`, the passed value
is raised as an exception in the woken tasks.

Return the count of tasks woken up. Return 0 if no tasks are waiting on `condition`.
"""
@constprop :none notify(c::GenericCondition, @nospecialize(arg = nothing); all=true, error=false) = notify(c, arg, all, error)
function notify(c::GenericCondition, @nospecialize(arg), all, error)
    assert_havelock(c)
    cnt = 0
    while !isempty(c.waitq)
        w = popfirst!(waitqueue(c))
        # An entry whose wake was already claimed by an interrupter does not
        # count as woken: drop it and continue to the next waiter (the
        # interrupted task resumes via whatever its claimer scheduled and
        # will find its entry already unlinked).
        t = @atomic :monotonic w.task
        if !(t isa Task && claim_wait(t, w))
            continue
        end
        schedule(t, arg, error=error)
        cnt += 1
        all || break
    end
    return cnt
end

notify_error(c::GenericCondition, err) = notify(c, err, true, true)

"""
    isempty(condition)

Return `true` if no tasks are waiting on the condition, `false` otherwise.
"""
isempty(c::GenericCondition) = _waitq_isempty(waitqueue(c))


# default (Julia v1.0) is currently single-threaded
# (although it uses MT-safe versions, when possible)
"""
    Condition()

Create an edge-triggered event source that tasks can wait for. Tasks that call [`wait`](@ref) on a
`Condition` are suspended and queued. Tasks are woken up when [`notify`](@ref) is later called on
the `Condition`. Waiting on a condition can return a value or raise an error if the optional arguments
of [`notify`](@ref) are used. Edge triggering means that only tasks waiting at the time [`notify`](@ref)
is called can be woken up. For level-triggered notifications, you must keep extra state to keep
track of whether a notification has happened. The [`Channel`](@ref) and [`Threads.Event`](@ref) types do
this, and can be used for level-triggered events.

This object is NOT thread-safe. See [`Threads.Condition`](@ref) for a thread-safe version.
"""
const Condition = GenericCondition{AlwaysLockedST}

show(io::IO, ::Condition) = print(io, Condition, "()")

lock(c::GenericCondition{AlwaysLockedST}) =
    throw(ArgumentError("`Condition` is not thread-safe. Please use `Threads.Condition` instead for multi-threaded code."))
unlock(c::GenericCondition{AlwaysLockedST}) =
    throw(ArgumentError("`Condition` is not thread-safe. Please use `Threads.Condition` instead for multi-threaded code."))
