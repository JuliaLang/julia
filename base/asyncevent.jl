# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    cond::ThreadSynchronizer
    isopen::Bool

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), ThreadSynchronizer(), true)
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
        lock(async.cond)
        try
            while isopen(async)
                success = try
                    stream_wait(async, async.cond)
                    true
                catch exc # ignore possible exception on close()
                    isa(exc, EOFError) || rethrow()
                finally
                    unlock(async.cond)
                end
                success && cb(async)
                lock(async.cond)
            end
        finally
            unlock(async.cond)
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
    cond::ThreadSynchronizer
    isopen::Bool

    function Timer(timeout::Real; interval::Real = 0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        interval ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $interval seconds"))

        this = new(Libc.malloc(_sizeof_uv_timer), ThreadSynchronizer(), true)
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
    lock(t.cond)
    try
        isopen(t) || throw(EOFError())
        stream_wait(t, t.cond)
    finally
        unlock(t.cond)
    end
end

isopen(t::Union{Timer, AsyncCondition}) = t.isopen

function close(t::Union{Timer, AsyncCondition})
    if t.handle != C_NULL && isopen(t)
        t.isopen = false
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
    lock(t.cond)
    try
        uvfinalize(t)
        notify_error(t.cond, EOFError())
    finally
        unlock(t.cond)
    end
    nothing
end

function uv_asynccb(handle::Ptr{Cvoid})
    async = @handle_as handle AsyncCondition
    lock(async.cond)
    try
        notify(async.cond)
    finally
        unlock(async.cond)
    end
    nothing
end

function uv_timercb(handle::Ptr{Cvoid})
    t = @handle_as handle Timer
    lock(t.cond)
    try
        if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Cvoid},), t) == 0
            # timer is stopped now
            close(t)
        end
        notify(t.cond)
    finally
        unlock(t.cond)
    end
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

"""
    timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

Waits until `testcb` returns `true` or for `secs` seconds, whichever is earlier.
`testcb` is polled every `pollint` seconds.
"""
function timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)
    pollint > 0 || throw(ArgumentError("cannot set pollint to $pollint seconds"))
    start = time()
    done = Channel(1)
    timercb(aw) = begin
        try
            if testcb()
                put!(done, :ok)
            elseif (time() - start) > secs
                put!(done, :timed_out)
            end
        catch e
            put!(done, :error)
        finally
            isready(done) && close(aw)
        end
    end

    if !testcb()
        t = Timer(timercb, pollint, interval = pollint)
        ret = fetch(done)
        close(t)
    else
        ret = :ok
    end
    ret
end
