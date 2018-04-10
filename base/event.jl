# This file is a part of Julia. License is MIT: https://julialang.org/license

## condition variables

"""
    Condition()

Create an edge-triggered event source that tasks can wait for. Tasks that call [`wait`](@ref) on a
`Condition` are suspended and queued. Tasks are woken up when [`notify`](@ref) is later called on
the `Condition`. Edge triggering means that only tasks waiting at the time [`notify`](@ref) is
called can be woken up. For level-triggered notifications, you must keep extra state to keep
track of whether a notification has happened. The [`Channel`](@ref) type does
this, and so can be used for level-triggered events.
"""
mutable struct Condition
    waitq::Vector{Any}

    Condition() = new([])
end

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* [`Channel`](@ref): Wait for a value to be appended to the channel.
* [`Condition`](@ref): Wait for [`notify`](@ref) on a condition.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](@ref): Wait for a `Task` to finish. If the task fails with an exception, the
  exception is propagated (re-thrown in the task that called `wait`).
* `RawFD`: Wait for changes on a file descriptor (see the `FileWatching` package).

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to [`schedule`](@ref) or [`yieldto`](@ref).

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before
proceeding.
"""
function wait(c::Condition)
    ct = current_task()

    push!(c.waitq, ct)

    try
        return wait()
    catch
        filter!(x->x!==ct, c.waitq)
        rethrow()
    end
end

"""
    notify(condition, val=nothing; all=true, error=false)

Wake up tasks waiting for a condition, passing them `val`. If `all` is `true` (the default),
all waiting tasks are woken, otherwise only one is. If `error` is `true`, the passed value
is raised as an exception in the woken tasks.

Return the count of tasks woken up. Return 0 if no tasks are waiting on `condition`.
"""
notify(c::Condition, @nospecialize(arg = nothing); all=true, error=false) = notify(c, arg, all, error)
function notify(c::Condition, arg, all, error)
    cnt = 0
    if all
        cnt = length(c.waitq)
        for t in c.waitq
            error ? schedule(t, arg, error=error) : schedule(t, arg)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        cnt = 1
        t = popfirst!(c.waitq)
        error ? schedule(t, arg, error=error) : schedule(t, arg)
    end
    cnt
end

notify_error(c::Condition, err) = notify(c, err, true, true)

n_waiters(c::Condition) = length(c.waitq)

# schedule an expression to run asynchronously, with minimal ceremony
"""
    @schedule

Wrap an expression in a [`Task`](@ref) and add it to the local machine's scheduler queue.
Similar to [`@async`](@ref) except that an enclosing `@sync` does NOT wait for tasks
started with an `@schedule`.
"""
macro schedule(expr)
    thunk = esc(:(()->($expr)))
    :(enq_work(Task($thunk)))
end

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
    catch e
        undo(reftask[])
        rethrow(e)
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
            throw(UVError("uv_async_init", err))
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
                isa(exc, EOFError) || rethrow(exc)
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

Waiting tasks are woken after an intial delay of `delay` seconds, and then repeating with the given
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
            throw(UVError("uv_timer_init", err))
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

Waiting tasks are woken and the function `callback` is called after an intial delay of `delay` seconds,
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
                isa(exc, EOFError) || rethrow(exc)
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
