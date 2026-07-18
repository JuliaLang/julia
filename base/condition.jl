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

"""
    GenericCondition

Abstract implementation of a condition object
for synchronizing task objects with a given lock.
"""
struct GenericCondition{L<:AbstractLock}
    waitq::WaitQueue
    lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}(WaitQueue(), L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}(WaitQueue(), l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}(WaitQueue(), l)
end

waitqueue(c::GenericCondition) = WaitQueueRef(c.waitq, c)

show(io::IO, c::GenericCondition) = print(io, GenericCondition, "(", c.lock, ")")

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

lock(f, c::GenericCondition) = lock(f, c.lock)

# have waiter wait for c: enqueue the waiter on c's wait queue (with
# `waitee` recorded as the queue identity), arming it for a completion wake.
# Returns the waiter.
function _wait2(c::GenericCondition, waiter::Task, waitee=c, first::Bool=false)
    ct = current_task()
    assert_havelock(c)
    w = waiter
    @atomic :monotonic w.wait_state = WAITNODE_WAITING
    if first
        pushfirst!(withwaitee(waitqueue(c), waitee), w)
    else
        push!(withwaitee(waitqueue(c), waitee), w)
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
wait(c::GenericCondition; first::Bool=false, waitee=c,
     cancel::CancelTokenArg=DEFAULT_CANCEL, min_severity::UInt8=0x00) =
    wait(c, check_cancel_arg(cancel); first, waitee, min_severity)

function wait(c::GenericCondition, tok::MaybeToken; first::Bool=false, waitee=c,
              min_severity::UInt8=0x00)
    ct = current_task()
    assert_havelock(c)
    src = tok === nothing ? nothing : tok.source
    # entry check: throw before enqueueing anything (skipped for teardown
    # waits that re-park after acknowledging a severity)
    src === nothing || min_severity != 0x00 || checkcancel(src)
    w = _wait2(c, ct, waitee, first)
    if src !== nothing && !register_cancellation!(src, w; min_severity=min_severity)
        list_deletefirst!(withwaitee(waitqueue(c), waitee), w)
        @atomic :monotonic w.wait_state = WAITNODE_IDLE
        checkcancel(src) # delivers the cancellation (acknowledge + throw)
        error("cancellation registration refused, but the source is not cancelled")
    end
    token = unlockall(c.lock)

    ret = try
        wait()
    catch
        relockall(c.lock, token)
        src === nothing || unregister_cancellation!(src, w)
        # This cleans up our entry in the waitqueue if we were cancelled or
        # resumed from an unexpected `throwto`; a no-op if a completion
        # already popped the node.
        list_deletefirst!(withwaitee(waitqueue(c), waitee), w)
        @atomic :monotonic w.wait_state = WAITNODE_IDLE
        rethrow()
    end

    relockall(c.lock, token)
    src === nothing || unregister_cancellation!(src, w)
    list_deletefirst!(withwaitee(waitqueue(c), waitee), w)
    @atomic :monotonic w.wait_state = WAITNODE_IDLE
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
    while !isempty(waitqueue(c))
        w = popfirst!(waitqueue(c))
        # A node whose wake was already claimed by a canceller does not count
        # as woken: continue to the next waiter (the cancelled task resumes
        # via the exception the canceller scheduled).
        (@atomicreplace w.wait_state WAITNODE_WAITING => WAITNODE_NOTIFIED).success || continue
        schedule(w, arg, error=error)
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
isempty(c::GenericCondition) = isempty(c.waitq)


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
