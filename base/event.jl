# This file is a part of Julia. License is MIT: http://julialang.org/license

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
type Condition
    waitq::Vector{Any}

    Condition() = new([])
end

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

Returns the count of tasks woken up. Returns 0 if no tasks are waiting on `condition`.
"""
notify(c::Condition, arg::ANY=nothing; all=true, error=false) = notify(c, arg, all, error)
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
        t = shift!(c.waitq)
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

global const Workqueue = Any[]

function enq_work(t::Task)
    t.state == :runnable || error("schedule: Task not runnable")
    ccall(:uv_stop, Void, (Ptr{Void},), eventloop())
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
julia> a5() = det(rand(1000, 1000));

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

# fast version of schedule(t,v);wait()
function schedule_and_wait(t::Task, v=nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    if isempty(Workqueue)
        return yieldto(t, v)
    else
        t.result = v
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
    yieldto(t::Task, arg = nothing)

Switch to the given task. The first time a task is switched to, the task's function is
called with no arguments. On subsequent switches, `arg` is returned from the task's last
call to `yieldto`. This is a low-level call that only switches tasks, not considering states
or scheduling in any way. Its use is discouraged.
"""
yieldto(t::Task, x::ANY = nothing) = ccall(:jl_switchto, Any, (Any, Any), t, x)

# yield to a task, throwing an exception in it
function throwto(t::Task, exc)
    t.exception = exc
    yieldto(t)
end

function wait()
    while true
        if isempty(Workqueue)
            c = process_events(true)
            if c==0 && eventloop()!=C_NULL && isempty(Workqueue)
                # if there are no active handles and no runnable tasks, just
                # wait for signals.
                pause()
            end
        else
            t = shift!(Workqueue)
            if t.state != :queued
                # assume this somehow got queued twice,
                # probably broken now, but try discarding this switch and keep going
                # can't throw here, because it's probably not the fault of the caller to wait
                # and don't want to use print() here, because that may try to incur a task switch
                ccall(:jl_safe_printf, Void, (Ptr{UInt8}, Vararg{Int32}),
                    "\nWARNING: Workqueue inconsistency detected: shift!(Workqueue).state != :queued\n")
                continue
            end
            arg = t.result
            t.result = nothing
            t.state = :runnable
            local result
            try
                result = yieldto(t, arg)
                current_task().state == :runnable || throw(AssertionError("current_task().state == :runnable"))
            catch e
                ct = current_task()
                if ct.state == :queued
                    if t.state == :runnable
                        # assume we failed to queue t
                        # return it to the queue to be scheduled later
                        t.result = arg
                        t.state = :queued
                        push!(Workqueue, t)
                    end
                    # return ourself to the runnable state
                    i = findfirst(Workqueue, ct)
                    i == 0 || deleteat!(Workqueue, i)
                    ct.state = :runnable
                end
                rethrow(e)
            end
            process_events(false)
            # return when we come out of the queue
            return result
        end
    end
    assert(false)
end

if is_windows()
    pause() = ccall(:Sleep, stdcall, Void, (UInt32,), 0xffffffff)
else
    pause() = ccall(:pause, Void, ())
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
type AsyncCondition
    handle::Ptr{Void}
    cond::Condition

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), Condition())
        associate_julia_struct(this.handle, this)
        preserve_handle_new(this)
        err = ccall(:uv_async_init, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void}),
            eventloop(), this, uv_jl_asynccb::Ptr{Void})
        this
    end
end

unsafe_convert(::Type{Ptr{Void}}, async::AsyncCondition) = async.handle

function wait(async::AsyncCondition)
    isopen(async) || throw(EOFError())
    wait(async.cond)
end

isopen(t::AsyncCondition) = (t.handle != C_NULL)

close(t::AsyncCondition) = ccall(:jl_close_uv, Void, (Ptr{Void},), t)

function _uv_hook_close(async::AsyncCondition)
    async.handle = C_NULL
    unpreserve_handle(async)
    notify_error(async.cond, EOFError())
    nothing
end

function uv_asynccb(handle::Ptr{Void})
    async = @handle_as handle AsyncCondition
    notify(async.cond)
    nothing
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
            catch # ignore possible exception on close()
                false
            end
            success && cb(async)
        end
    end)
    # must start the task right away so that it can wait for the AsyncCondition before
    # we re-enter the event loop. this avoids a race condition. see issue #12719
    enq_work(current_task())
    yieldto(waiter)
    return async
end

## timer-based notifications

"""
    Timer(delay, repeat=0)

Create a timer that wakes up tasks waiting for it (by calling [`wait`](@ref) on the timer object) at
a specified interval.  Times are in seconds.  Waiting tasks are woken with an error when the
timer is closed (by [`close`](@ref). Use [`isopen`](@ref) to check whether a timer is still active.
"""
type Timer
    handle::Ptr{Void}
    cond::Condition
    isopen::Bool

    function Timer(timeout::Real, repeat::Real=0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        repeat ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $repeat seconds"))

        this = new(Libc.malloc(_sizeof_uv_timer), Condition(), true)
        err = ccall(:uv_timer_init, Cint, (Ptr{Void}, Ptr{Void}), eventloop(), this)
        if err != 0
            #TODO: this codepath is currently not tested
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(UVError("uv_make_timer",err))
        end

        associate_julia_struct(this.handle, this)
        preserve_handle_new(this)

        ccall(:uv_update_time, Void, (Ptr{Void},), eventloop())
        ccall(:uv_timer_start,  Cint,  (Ptr{Void}, Ptr{Void}, UInt64, UInt64),
              this, uv_jl_timercb::Ptr{Void},
              UInt64(round(timeout * 1000)) + 1, UInt64(round(repeat * 1000)))
        return this
    end
end

unsafe_convert(::Type{Ptr{Void}}, t::Timer) = t.handle

function wait(t::Timer)
    isopen(t) || throw(EOFError())
    wait(t.cond)
end

isopen(t::Timer) = t.isopen

function close(t::Timer)
    if t.handle != C_NULL
        t.isopen = false
        ccall(:uv_timer_stop, Cint, (Ptr{Void},), t)
        ccall(:jl_close_uv, Void, (Ptr{Void},), t)
    end
    nothing
end

function _uv_hook_close(t::Timer)
    unpreserve_handle(t)
    disassociate_julia_struct(t)
    t.handle = C_NULL
    t.isopen = false
    notify_error(t.cond, EOFError())
    nothing
end

function uv_timercb(handle::Ptr{Void})
    t = @handle_as handle Timer
    if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Void},), t) == 0
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
    Timer(callback::Function, delay, repeat=0)

Create a timer to call the given `callback` function. The `callback` is passed one argument,
the timer object itself. The callback will be invoked after the specified initial `delay`,
and then repeating with the given `repeat` interval. If `repeat` is `0`, the timer is only
triggered once. Times are in seconds. A timer is stopped and has its resources freed by
calling [`close`](@ref) on it.
"""
function Timer(cb::Function, timeout::Real, repeat::Real=0.0)
    t = Timer(timeout, repeat)
    waiter = Task(function()
        while isopen(t)
            success = try
                wait(t)
                true
            catch # ignore possible exception on close()
                false
            end
            success && cb(t)
        end
    end)
    # must start the task right away so that it can wait for the Timer before
    # we re-enter the event loop. this avoids a race condition. see issue #12719
    enq_work(current_task())
    yieldto(waiter)
    return t
end
