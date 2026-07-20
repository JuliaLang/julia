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
relockall(l::AbstractLock, token::Nothing) = lock(l) # internal function for implementing `wait`
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

# A task's registration on a wait queue. All fields are plain: `next` and
# `queue` are protected by the waitee's lock (`queue` holds the queue's
# identity - see `waitqueue` - while the entry is enqueued, acting as the
# "am I registered, and on what" witness, and `nothing` otherwise); `task`
# is written by the owning task before enqueueing, so it is ordered by the
# same lock for lock-holding readers.
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
#     The claimed entry stays linked - the interrupter may not touch the queue
#     without its lock - and is unlinked lazily, either by the interrupted
#     task's own wait cleanup or by the `notify` that pops and drops it.
#   - wake sources directed at one *specific* wait (e.g. the timeout task of
#     `Experimental.wait_with_timeout`) must register the wait with a fresh,
#     single-use entry: single-use-ness is what guarantees their
#     expected-value CAS cannot mistakenly claim a later, unrelated wait.
#
# Entries are heap objects (rather than links folded into the Task) so that a
# task whose interrupted wait left a stale registration behind can immediately
# register anew - e.g. park on a lock during its cleanup - with a fresh entry.
# To keep the common park allocation-free, each task caches one entry
# (`t.cached_wait_entry`) and reuses it whenever it is free, i.e. not still
# linked into some queue (`w.queue === nothing`). Reuse requires the owning
# task to be synchronized with the unlinker: either the task unlinked the entry
# itself, or the unlinker subsequently scheduled it. Interrupted-wait cleanup
# temporarily removes its entry from the cache before relocking, since another
# task may unlink that stale entry without being the task that scheduled us.
mutable struct WaitEntry
    task::Union{Task, Nothing}
    next::Union{WaitEntry, Nothing}
    queue::Any
    WaitEntry(task::Union{Task, Nothing}) = new(task, nothing, nothing)
end

# Return the cached entry of `waiter` if it is free, else a fresh (and newly
# cached) one.
function _cached_wait_entry(waiter::Task)
    w = waiter.cached_wait_entry
    if w isa WaitEntry && w.queue === nothing
        w.task = waiter
    else
        w = WaitEntry(waiter)
        waiter.cached_wait_entry = w
    end
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

"""
    GenericCondition

Abstract implementation of a condition object
for synchronizing task objects with a given lock.
"""
struct GenericCondition{L<:AbstractLock}
    waitq::IntrusiveLinkedList{WaitEntry}
    lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{WaitEntry}(), L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{WaitEntry}(), l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}(IntrusiveLinkedList{WaitEntry}(), l)
end

# The queue identity recorded in entries (the membership witness) must be a
# mutable, identity-stable object: an immutable waitee would be re-boxed on
# every park, allocating in the steady state. Plain condition waits therefore
# use the waitq list itself as the identity; waits on behalf of a richer
# (mutable) waitee - e.g. waiting on a Task via its donenotify - record that
# waitee instead (see `waitqueue(::Task)`).
waitqueue(c::GenericCondition) = ILLRef(c.waitq, c.waitq)

show(io::IO, c::GenericCondition) = print(io, GenericCondition, "(", c.lock, ")")

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

lock(f, c::GenericCondition) = lock(f, c.lock)

# have waiter wait for c: register `waiter` on c's wait queue (with `waitee`
# recorded as the queue identity) and arm the registration for a wake.
# Returns the registration entry.
function _wait2(c::GenericCondition, waiter::Task, first::Bool=false;
                waitee=c.waitq, entry::Union{WaitEntry, Nothing}=nothing)
    ct = current_task()
    assert_havelock(c)
    w = entry === nothing ? _cached_wait_entry(waiter) : entry
    _arm_wait(waiter, w)
    if first
        pushfirst!(ILLRef(c.waitq, waitee), w)
    else
        push!(ILLRef(c.waitq, waitee), w)
    end
    # since _wait2 is similar to schedule, we should observe the sticky bit now
    if waiter.sticky && Threads.threadid(waiter) == 0 && !GC.in_finalizer()
        # Issue #41324
        # t.sticky && tid == 0 is a task that needs to be co-scheduled with
        # the parent task. If the parent (current_task) is not sticky we must
        # set it to be sticky.
        # XXX: Ideally we would be able to unset this
        ct.sticky = true
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
    wait(c::GenericCondition; first::Bool=false)

Wait for [`notify`](@ref) on `c` and return the `val` parameter passed to `notify`.

If the keyword `first` is set to `true`, the waiter will be put _first_
in line to wake up on `notify`. Otherwise, `wait` has first-in-first-out (FIFO) behavior.
"""
function wait(c::GenericCondition; first::Bool=false, waitee=c.waitq)
    ct = current_task()
    assert_havelock(c)
    w = _wait2(c, ct, first; waitee)
    token = unlockall(c.lock)
    ret = try
        wait()
    catch
        # We were resumed without a wake having been delivered through our
        # registration: either an interrupter claimed it (leaving the entry
        # linked for us to clean up), or we got a raw `throwto`. Disarm the
        # registration first - before the relock below can register a new
        # wait - then unlink our entry (a no-op if a `notify` already popped
        # and dropped it).
        @atomicreplace ct.waiting_on w => nothing
        # Do not reuse `w` while relocking: a notifier may have popped this
        # stale entry without scheduling us and may still retain its identity
        # for the wake-claim CAS. If relocking does not need to park and cache
        # a replacement, restore `w` once cleanup under the old lock makes it
        # safe to reuse again.
        was_cached = ct.cached_wait_entry === w
        was_cached && (ct.cached_wait_entry = nothing)
        relockall(c.lock, token)
        list_deletefirst!(ILLRef(c.waitq, waitee), w)
        if was_cached && ct.cached_wait_entry === nothing
            ct.cached_wait_entry = w
        end
        rethrow()
    end
    # a normal wake implies our claim was won and our entry already unlinked
    relockall(c.lock, token)
    return ret
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
        t = w.task
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
function isempty(c::GenericCondition)
    for w in c.waitq
        t = w.task
        t isa Task && (@atomic t.waiting_on) === w && return false
    end
    return true
end


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
