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
for synchronizing tasks objects with a given lock.
"""
struct GenericCondition{L<:AbstractLock}
    waitq::IntrusiveLinkedList{Task}
    lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{Task}(), L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}(IntrusiveLinkedList{Task}(), l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}(IntrusiveLinkedList{Task}(), l)
end

show(io::IO, c::GenericCondition) = print(io, GenericCondition, "(", c.lock, ")")

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

lock(f, c::GenericCondition) = lock(f, c.lock)

# have waiter wait for c
function _wait2(c::GenericCondition, waiter::Task, first::Bool=false)
    ct = current_task()
    assert_havelock(c)
    if first
        pushfirst!(c.waitq, waiter)
    else
        push!(c.waitq, waiter)
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
    return
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

# wait with timeout
#
# The behavior of wait changes if a timeout is specified. There are
# three concurrent entities that can interact:
# 1. Task W: the task that calls wait w/timeout.
# 2. Task T: the task created to handle a timeout.
# 3. Task N: the task that notifies the Condition being waited on.
#
# Typical flow:
# - W enters the Condition's wait queue.
# - W creates T and stops running (calls wait()).
# - T, when scheduled, waits on a Timer.
# - Two common outcomes:
#   - N notifies the Condition.
#     - W starts running, closes the Timer, sets waiter_left and returns
#       the notify'ed value.
#     - The closed Timer throws an EOFError to T which simply ends.
#   - The Timer expires.
#     - T starts running and locks the Condition.
#     - T confirms that waiter_left is unset and that W is still in the
#       Condition's wait queue; it then removes W from the wait queue,
#       sets dosched to true and unlocks the Condition.
#     - If dosched is true, T schedules W with the special :timed_out
#       value.
#     - T ends.
#     - W runs and returns :timed_out.
#
# Some possible interleavings:
# - N notifies the Condition but the Timer expires and T starts running
#   before W:
#   - W closing the expired Timer is benign.
#   - T will find that W is no longer in the Condition's wait queue
#     (which is protected by a lock) and will not schedule W.
# - N notifies the Condition; W runs and calls wait on the Condition
#   again before the Timer expires:
#   - W sets waiter_left before leaving. When T runs, it will find that
#     waiter_left is set and will not schedule W.
#
# The lock on the Condition's wait queue and waiter_left together
# ensure proper synchronization and behavior of the tasks involved.

"""
    wait(c::GenericCondition; first::Bool=false, timeout::Real=0.0)

Wait for [`notify`](@ref) on `c` and return the `val` parameter passed to `notify`.

If the keyword `first` is set to `true`, the waiter will be put _first_
in line to wake up on `notify`. Otherwise, `wait` has first-in-first-out (FIFO) behavior.

If `timeout` is specified, cancel the `wait` when it expires and return
`:timed_out`. The minimum value for `timeout` is 0.001 seconds, i.e. 1
millisecond.
"""
function wait(c::GenericCondition; first::Bool=false, timeout::Real=0.0)
    timeout == 0.0 || timeout ≥ 1e-3 || throw(ArgumentError("timeout must be ≥ 1 millisecond"))

    ct = current_task()
    _wait2(c, ct, first)
    token = unlockall(c.lock)

    timer::Union{Timer, Nothing} = nothing
    waiter_left::Union{Threads.Atomic{Bool}, Nothing} = nothing
    if timeout > 0.0
        timer = Timer(timeout)
        waiter_left = Threads.Atomic{Bool}(false)
        # start a task to wait on the timer
        t = Task() do
            try
                wait(timer)
            catch e
                # if the timer was closed, the waiting task has been scheduled; do nothing
                e isa EOFError && return
            end
            dosched = false
            lock(c.lock)
            # Confirm that the waiting task is still in the wait queue and remove it. If
            # the task is not in the wait queue, it must have been notified already so we
            # don't do anything here.
            if !waiter_left[] && ct.queue == c.waitq
                dosched = true
                Base.list_deletefirst!(c.waitq, ct)
            end
            unlock(c.lock)
            # send the waiting task a timeout
            dosched && schedule(ct, :timed_out)
        end
        t.sticky = false
        Threads._spawn_set_thrpool(t, :interactive)
        schedule(t)
    end

    try
        res = wait()
        if timer !== nothing
            close(timer)
            waiter_left[] = true
        end
        return res
    catch
        q = ct.queue; q === nothing || Base.list_deletefirst!(q::IntrusiveLinkedList{Task}, ct)
        rethrow()
    finally
        relockall(c.lock, token)
    end
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
        t = popfirst!(c.waitq)
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
