# This file is a part of Julia. License is MIT: https://julialang.org/license

## thread/task locking abstraction

@noinline function concurrency_violation()
    # can be useful for debugging
    #try; error(); catch; ccall(:jlbacktrace, Cvoid, ()); end
    error("concurrency violation detected")
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
It also does not synchronize tasks; for that use a real lock such as [`RecursiveLock`](@ref).
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
for synchonizing tasks objects with a given lock.
"""
struct GenericCondition{L<:AbstractLock}
    waitq::InvasiveLinkedList{Task}
    lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}(InvasiveLinkedList{Task}(), L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}(InvasiveLinkedList{Task}(), l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}(InvasiveLinkedList{Task}(), l)
end

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

lock(f, c::GenericCondition) = lock(f, c.lock)
unlock(f, c::GenericCondition) = unlock(f, c.lock)

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* [`Channel`](@ref): Wait for a value to be appended to the channel.
* [`Condition`](@ref): Wait for [`notify`](@ref) on a condition.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](@ref): Wait for a `Task` to finish. If the task fails with an exception, a
  `TaskFailedException` (which wraps the failed task) is thrown.
* [`RawFD`](@ref): Wait for changes on a file descriptor (see the `FileWatching` package).

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to [`schedule`](@ref) or [`yieldto`](@ref).

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before
proceeding.
"""
function wait(c::GenericCondition)
    ct = current_task()
    assert_havelock(c)
    push!(c.waitq, ct)
    token = unlockall(c.lock)
    try
        return wait()
    catch
        ct.queue === nothing || list_deletefirst!(ct.queue, ct)
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
notify(c::GenericCondition, @nospecialize(arg = nothing); all=true, error=false) = notify(c, arg, all, error)
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

n_waiters(c::GenericCondition) = length(c.waitq)

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
the `Condition`. Edge triggering means that only tasks waiting at the time [`notify`](@ref) is
called can be woken up. For level-triggered notifications, you must keep extra state to keep
track of whether a notification has happened. The [`Channel`](@ref) and [`Threads.Event`](@ref) types do
this, and can be used for level-triggered events.

This object is NOT thread-safe. See [`Threads.Condition`](@ref) for a thread-safe version.
"""
const Condition = GenericCondition{AlwaysLockedST}

lock(c::GenericCondition{AlwaysLockedST}) =
    throw(ArgumentError("`Condition` is not thread-safe. Please use `Threads.Condition` instead for multi-threaded code."))
unlock(c::GenericCondition{AlwaysLockedST}) =
    throw(ArgumentError("`Condition` is not thread-safe. Please use `Threads.Condition` instead for multi-threaded code."))
