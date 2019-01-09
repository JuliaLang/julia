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



## async event notifications

"""
    AsyncCondition()

Create a async condition that wakes up tasks waiting for it
(by calling [`wait`](@ref) on the object)
when notified from C by a call to `uv_async_send`.
Waiting tasks are woken with an error when the object is closed (by [`close`](@ref).
Use [`isopen`](@ref) to check whether it is still active.
"""
mutable struct AsyncCondition
    handle::Ptr{Cvoid}
    cond::Condition
    isopen::Bool

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), Condition(), true)
        associate_julia_struct(this.handle, this)
        finalizer(uvfinalize, this)
        err = ccall(:uv_async_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
            eventloop(), this, uv_jl_asynccb::Ptr{Cvoid})
        if err != 0
            #TODO: this codepath is currently not tested
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_async_init", err))
        end
        return this
    end
end

"""
    AsyncCondition(callback::Function)

Create a async condition that calls the given `callback` function. The `callback` is passed one argument,
the async condition object itself.
"""
function AsyncCondition(cb::Function)
    async = AsyncCondition()
    waiter = Task(function()
        while isopen(async)
            success = try
                wait(async)
                true
            catch exc # ignore possible exception on close()
                isa(exc, EOFError) || rethrow()
            end
            success && cb(async)
        end
    end)
    # must start the task right away so that it can wait for the AsyncCondition before
    # we re-enter the event loop. this avoids a race condition. see issue #12719
    yield(waiter)
    return async
end

## timer-based notifications

"""
    Timer(delay; interval = 0)

Create a timer that wakes up tasks waiting for it (by calling [`wait`](@ref) on the timer object).

Waiting tasks are woken after an initial delay of `delay` seconds, and then repeating with the given
`interval` in seconds. If `interval` is equal to `0`, the timer is only triggered once. When
the timer is closed (by [`close`](@ref) waiting tasks are woken with an error. Use [`isopen`](@ref)
to check whether a timer is still active.
"""
mutable struct Timer
    handle::Ptr{Cvoid}
    cond::Condition
    isopen::Bool

    function Timer(timeout::Real; interval::Real = 0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        interval ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $interval seconds"))

        this = new(Libc.malloc(_sizeof_uv_timer), Condition(), true)
        err = ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), this)
        if err != 0
            #TODO: this codepath is currently not tested
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_timer_init", err))
        end

        associate_julia_struct(this.handle, this)
        finalizer(uvfinalize, this)

        ccall(:uv_update_time, Cvoid, (Ptr{Cvoid},), eventloop())
        ccall(:uv_timer_start,  Cint,  (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64),
              this, uv_jl_timercb::Ptr{Cvoid},
              UInt64(round(timeout * 1000)) + 1, UInt64(round(interval * 1000)))
        return this
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, t::Timer) = t.handle
unsafe_convert(::Type{Ptr{Cvoid}}, async::AsyncCondition) = async.handle

function wait(t::Union{Timer, AsyncCondition})
    isopen(t) || throw(EOFError())
    stream_wait(t, t.cond)
end

isopen(t::Union{Timer, AsyncCondition}) = t.isopen

function close(t::Union{Timer, AsyncCondition})
    if t.handle != C_NULL && isopen(t)
        t.isopen = false
        isa(t, Timer) && ccall(:uv_timer_stop, Cint, (Ptr{Cvoid},), t)
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
    end
    nothing
end

function uvfinalize(t::Union{Timer, AsyncCondition})
    if t.handle != C_NULL
        disassociate_julia_struct(t.handle) # not going to call the usual close hooks
        close(t)
        t.handle = C_NULL
    end
    t.isopen = false
    nothing
end

function _uv_hook_close(t::Union{Timer, AsyncCondition})
    uvfinalize(t)
    notify_error(t.cond, EOFError())
    nothing
end

function uv_asynccb(handle::Ptr{Cvoid})
    async = @handle_as handle AsyncCondition
    notify(async.cond)
    nothing
end

function uv_timercb(handle::Ptr{Cvoid})
    t = @handle_as handle Timer
    if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Cvoid},), t) == 0
        # timer is stopped now
        close(t)
    end
    notify(t.cond)
    nothing
end

"""
    sleep(seconds)

Block the current task for a specified number of seconds. The minimum sleep time is 1
millisecond or input of `0.001`.
"""
function sleep(sec::Real)
    sec ≥ 0 || throw(ArgumentError("cannot sleep for $sec seconds"))
    wait(Timer(sec))
    nothing
end

# timer with repeated callback
"""
    Timer(callback::Function, delay; interval = 0)

Create a timer that wakes up tasks waiting for it (by calling [`wait`](@ref) on the timer object) and
calls the function `callback`.

Waiting tasks are woken and the function `callback` is called after an initial delay of `delay` seconds,
and then repeating with the given `interval` in seconds. If `interval` is equal to `0`, the timer
is only triggered once. The function `callback` is called with a single argument, the timer itself.
When the timer is closed (by [`close`](@ref) waiting tasks are woken with an error. Use [`isopen`](@ref)
to check whether a timer is still active.

# Examples

Here the first number is printed after a delay of two seconds, then the following numbers are printed quickly.

```julia-repl
julia> begin
           i = 0
           cb(timer) = (global i += 1; println(i))
           t = Timer(cb, 2, interval = 0.2)
           wait(t)
           sleep(0.5)
           close(t)
       end
1
2
3
```
"""
function Timer(cb::Function, timeout::Real; interval::Real = 0.0)
    t = Timer(timeout, interval = interval)
    waiter = Task(function()
        while isopen(t)
            success = try
                wait(t)
                true
            catch exc # ignore possible exception on close()
                isa(exc, EOFError) || rethrow()
                false
            end
            success && cb(t)
        end
    end)
    # must start the task right away so that it can wait for the Timer before
    # we re-enter the event loop. this avoids a race condition. see issue #12719
    yield(waiter)
    return t
end
