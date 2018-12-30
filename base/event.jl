# This file is a part of Julia. License is MIT: https://julialang.org/license

## thread/task locking abstraction

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
assert_havelock(l::AbstractLock) = assert_havelock(l, Threads.threadid())
assert_havelock(l::AbstractLock, tid::Integer) =
    (islocked(l) && tid == Threads.threadid()) ? nothing : error("concurrency violation detected")
assert_havelock(l::AbstractLock, tid::Task) =
    (islocked(l) && tid === current_task()) ? nothing : error("concurrency violation detected")
assert_havelock(l::AbstractLock, tid::Nothing) = error("concurrency violation detected")

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
    waitq::Vector{Any}
    lock::L

    GenericCondition{L}() where {L<:AbstractLock} = new{L}([], L())
    GenericCondition{L}(l::L) where {L<:AbstractLock} = new{L}([], l)
    GenericCondition(l::AbstractLock) = new{typeof(l)}([], l)
end

assert_havelock(c::GenericCondition) = assert_havelock(c.lock)
lock(c::GenericCondition) = lock(c.lock)
unlock(c::GenericCondition) = unlock(c.lock)
trylock(c::GenericCondition) = trylock(c.lock)
islocked(c::GenericCondition) = islocked(c.lock)

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* [`Channel`](@ref): Wait for a value to be appended to the channel.
* [`Condition`](@ref): Wait for [`notify`](@ref) on a condition.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](@ref): Wait for a `Task` to finish. If the task fails with an exception, the
  exception is propagated (re-thrown in the task that called `wait`).
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
        filter!(x->x!==ct, c.waitq)
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
    if all
        cnt = length(c.waitq)
        for t in c.waitq
            schedule(t, arg, error=error)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        cnt = 1
        t = popfirst!(c.waitq)
        schedule(t, arg, error=error)
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
track of whether a notification has happened. The [`Channel`](@ref) and [`Event`](@ref) types do
this, and can be used for level-triggered events.

This object is NOT thread-safe. See [`Threads.Condition`](@ref) for a thread-safe version.
"""
const Condition = GenericCondition{AlwaysLockedST}


## scheduler and work queue

global const Workqueue = Task[]

function enq_work(t::Task)
    t.state == :runnable || error("schedule: Task not runnable")
    ccall(:uv_stop, Cvoid, (Ptr{Cvoid},), eventloop())
    push!(Workqueue, t)
    t.state = :queued
    return t
end

schedule(t::Task) = enq_work(t)

"""
    schedule(t::Task, [val]; error=false)

Add a [`Task`](@ref) to the scheduler's queue. This causes the task to run constantly when the system
is otherwise idle, unless the task performs a blocking operation such as [`wait`](@ref).

If a second argument `val` is provided, it will be passed to the task (via the return value of
[`yieldto`](@ref)) when it runs again. If `error` is `true`, the value is raised as an exception in
the woken task.

# Examples
```jldoctest
julia> a5() = sum(i for i in 1:1000);

julia> b = Task(a5);

julia> istaskstarted(b)
false

julia> schedule(b);

julia> yield();

julia> istaskstarted(b)
true

julia> istaskdone(b)
true
```
"""
function schedule(t::Task, arg; error=false)
    # schedule a task to be (re)started with the given value or exception
    if error
        t.exception = arg
    else
        t.result = arg
    end
    return enq_work(t)
end

# fast version of `schedule(t, arg); wait()`
function schedule_and_wait(t::Task, arg=nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    if isempty(Workqueue)
        return yieldto(t, arg)
    else
        t.result = arg
        push!(Workqueue, t)
        t.state = :queued
    end
    return wait()
end

"""
    yield()

Switch to the scheduler to allow another scheduled task to run. A task that calls this
function is still runnable, and will be restarted immediately if there are no other runnable
tasks.
"""
yield() = (enq_work(current_task()); wait())

"""
    yield(t::Task, arg = nothing)

A fast, unfair-scheduling version of `schedule(t, arg); yield()` which
immediately yields to `t` before calling the scheduler.
"""
function yield(t::Task, @nospecialize x = nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    t.result = x
    enq_work(current_task())
    return try_yieldto(ensure_rescheduled, Ref(t))
end

"""
    yieldto(t::Task, arg = nothing)

Switch to the given task. The first time a task is switched to, the task's function is
called with no arguments. On subsequent switches, `arg` is returned from the task's last
call to `yieldto`. This is a low-level call that only switches tasks, not considering states
or scheduling in any way. Its use is discouraged.
"""
function yieldto(t::Task, @nospecialize x = nothing)
    t.result = x
    return try_yieldto(identity, Ref(t))
end

function try_yieldto(undo, reftask::Ref{Task})
    try
        ccall(:jl_switchto, Cvoid, (Any,), reftask)
    catch
        undo(reftask[])
        rethrow()
    end
    ct = current_task()
    exc = ct.exception
    if exc !== nothing
        ct.exception = nothing
        throw(exc)
    end
    result = ct.result
    ct.result = nothing
    return result
end

# yield to a task, throwing an exception in it
function throwto(t::Task, @nospecialize exc)
    t.exception = exc
    return yieldto(t)
end

function ensure_rescheduled(othertask::Task)
    ct = current_task()
    if ct !== othertask && othertask.state == :runnable
        # we failed to yield to othertask
        # return it to the head of the queue to be scheduled later
        pushfirst!(Workqueue, othertask)
        othertask.state = :queued
    end
    if ct.state == :queued
        # if the current task was queued,
        # also need to return it to the runnable state
        # before throwing an error
        i = findfirst(t->t===ct, Workqueue)
        i === nothing || deleteat!(Workqueue, i)
        ct.state = :runnable
    end
    nothing
end

@noinline function poptask()
    t = popfirst!(Workqueue)
    if t.state != :queued
        # assume this somehow got queued twice,
        # probably broken now, but try discarding this switch and keep going
        # can't throw here, because it's probably not the fault of the caller to wait
        # and don't want to use print() here, because that may try to incur a task switch
        ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Int32...),
            "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state != :queued\n")
        return
    end
    t.state = :runnable
    return Ref(t)
end

function wait()
    while true
        if isempty(Workqueue)
            c = process_events(true)
            if c == 0 && eventloop() != C_NULL && isempty(Workqueue)
                # if there are no active handles and no runnable tasks, just
                # wait for signals.
                pause()
            end
        else
            reftask = poptask()
            if reftask !== nothing
                result = try_yieldto(ensure_rescheduled, reftask)
                process_events(false)
                # return when we come out of the queue
                return result
            end
        end
    end
    # unreachable
end

if Sys.iswindows()
    pause() = ccall(:Sleep, stdcall, Cvoid, (UInt32,), 0xffffffff)
else
    pause() = ccall(:pause, Cvoid, ())
end
