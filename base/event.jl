# This file is a part of Julia. License is MIT: http://julialang.org/license

## condition variables

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

notify(c::Condition, arg::ANY=nothing; all=true, error=false) = notify(c, arg, all, error)
function notify(c::Condition, arg, all, error)
    if all
        for t in c.waitq
            schedule(t, arg, error=error)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        t = shift!(c.waitq)
        schedule(t, arg, error=error)
    end
    nothing
end

notify1(c::Condition, arg=nothing) = notify(c, arg, all=false)

notify_error(c::Condition, err) = notify(c, err, error=true)
notify1_error(c::Condition, err) = notify(c, err, error=true, all=false)


# schedule an expression to run asynchronously, with minimal ceremony
macro schedule(expr)
    expr = :(()->($expr))
    :(enq_work(Task($(esc(expr)))))
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

yield() = (enq_work(current_task()); wait())

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
            t.state == :queued || throw(AssertionError("shift!(Workqueue).state == :queued"))
            arg = t.result
            t.result = nothing
            t.state = :runnable
            result = yieldto(t, arg)
            current_task().state == :runnable || throw(AssertionError("current_task().state == :runnable"))
            process_events(false)
            # return when we come out of the queue
            return result
        end
    end
    assert(false)
end

function pause()
    @unix_only    ccall(:pause, Void, ())
    @windows_only ccall(:Sleep,stdcall, Void, (UInt32,), 0xffffffff)
end


## async event notifications

"""
    AsyncCondition()

Create a async condition that wakes up tasks waiting for it (by calling `wait` on the object)
when notified from C by a call to uv_async_send.
Waiting tasks are woken with an error when the object is closed (by `close`).
Use `isopen` to check whether it is still active.
"""
type AsyncCondition
    handle::Ptr{Void}
    cond::Condition

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), Condition())
        associate_julia_struct(this.handle, this)
        preserve_handle(this)
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

Create a timer that wakes up tasks waiting for it (by calling `wait` on the timer object) at
a specified interval.  Times are in seconds.  Waiting tasks are woken with an error when the
timer is closed (by `close`). Use `isopen` to check whether a timer is still active.
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
        preserve_handle(this)

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
calling `close` on it.
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
