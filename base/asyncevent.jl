# This file is a part of Julia. License is MIT: https://julialang.org/license

## async event notifications

"""
    AsyncCondition()

Create a async condition that wakes up tasks waiting for it
(by calling [`wait`](@ref) on the object)
when notified from C by a call to `uv_async_send`.
Waiting tasks are woken with an error when the object is closed (by [`close`](@ref)).
Use [`isopen`](@ref) to check whether it is still active.

This provides an implicit acquire & release memory ordering between the sending and waiting threads.
"""
mutable struct AsyncCondition
    @atomic handle::Ptr{Cvoid}
    cond::ThreadSynchronizer
    @atomic isopen::Bool
    @atomic set::Bool

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), ThreadSynchronizer(), true, false)
        iolock_begin()
        associate_julia_struct(this.handle, this)
        err = ccall(:uv_async_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
            eventloop(), this, @cfunction(uv_asynccb, Cvoid, (Ptr{Cvoid},)))
        if err != 0
            #TODO: this codepath is currently not tested
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_async_init", err))
        end
        finalizer(uvfinalize, this)
        iolock_end()
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
    t = @task begin
        unpreserve_handle(async)
        while _trywait(async)
            cb(async)
            isopen(async) || return
        end
    end
    # here we are mimicking parts of _trywait, in coordination with task `t`
    preserve_handle(async)
    @lock async.cond begin
        if async.set
            schedule(t)
        else
            _wait2(async.cond, t)
        end
    end
    return async
end

## timer-based notifications

"""
    Timer(delay; interval = 0)

Create a timer that wakes up tasks waiting for it (by calling [`wait`](@ref) on the timer object).

Waiting tasks are woken after an initial delay of at least `delay` seconds, and then repeating after
at least `interval` seconds again elapse. If `interval` is equal to `0`, the timer is only triggered
once. When the timer is closed (by [`close`](@ref)) waiting tasks are woken with an error. Use
[`isopen`](@ref) to check whether a timer is still active.

!!! note
    `interval` is subject to accumulating time skew. If you need precise events at a particular
    absolute time, create a new timer at each expiration with the difference to the next time computed.

!!! note
    A `Timer` requires yield points to update its state. For instance, `isopen(t::Timer)` cannot be
    used to timeout a non-yielding while loop.

"""
mutable struct Timer
    @atomic handle::Ptr{Cvoid}
    cond::ThreadSynchronizer
    @atomic isopen::Bool
    @atomic set::Bool

    function Timer(timeout::Real; interval::Real = 0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        interval ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $interval seconds"))
        # libuv has a tendency to timeout 1 ms early, so we need +1 on the timeout (in milliseconds), unless it is zero
        timeoutms = ceil(UInt64, timeout * 1000) + !iszero(timeout)
        intervalms = ceil(UInt64, interval * 1000)
        loop = eventloop()

        this = new(Libc.malloc(_sizeof_uv_timer), ThreadSynchronizer(), true, false)
        associate_julia_struct(this.handle, this)
        iolock_begin()
        err = ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), loop, this)
        @assert err == 0
        finalizer(uvfinalize, this)
        ccall(:uv_update_time, Cvoid, (Ptr{Cvoid},), loop)
        err = ccall(:uv_timer_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64),
            this, @cfunction(uv_timercb, Cvoid, (Ptr{Cvoid},)),
            timeoutms, intervalms)
        @assert err == 0
        iolock_end()
        return this
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, t::Timer) = t.handle
unsafe_convert(::Type{Ptr{Cvoid}}, async::AsyncCondition) = async.handle

function _trywait(t::Union{Timer, AsyncCondition})
    set = t.set
    if set
        # full barrier now for AsyncCondition
        t isa Timer || Core.Intrinsics.atomic_fence(:acquire_release)
    else
        t.isopen || return false
        t.handle == C_NULL && return false
        iolock_begin()
        set = t.set
        if !set
            preserve_handle(t)
            lock(t.cond)
            try
                set = t.set
                if !set && t.isopen && t.handle != C_NULL
                    iolock_end()
                    set = wait(t.cond)
                    unlock(t.cond)
                    iolock_begin()
                    lock(t.cond)
                end
            finally
                unlock(t.cond)
                unpreserve_handle(t)
            end
        end
        iolock_end()
    end
    @atomic :monotonic t.set = false
    return set
end

function wait(t::Union{Timer, AsyncCondition})
    _trywait(t) || throw(EOFError())
    nothing
end


isopen(t::Union{Timer, AsyncCondition}) = t.isopen && t.handle != C_NULL

function close(t::Union{Timer, AsyncCondition})
    iolock_begin()
    if isopen(t)
        @atomic :monotonic t.isopen = false
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
    end
    iolock_end()
    nothing
end

function uvfinalize(t::Union{Timer, AsyncCondition})
    iolock_begin()
    lock(t.cond)
    try
        if t.handle != C_NULL
            disassociate_julia_struct(t.handle) # not going to call the usual close hooks anymore
            if t.isopen
                @atomic :monotonic t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t.handle)
            end
            @atomic :monotonic t.handle = C_NULL
            notify(t.cond, false)
        end
    finally
        unlock(t.cond)
    end
    iolock_end()
    nothing
end

function _uv_hook_close(t::Union{Timer, AsyncCondition})
    lock(t.cond)
    try
        @atomic :monotonic t.isopen = false
        Libc.free(@atomicswap :monotonic t.handle = C_NULL)
        notify(t.cond, false)
    finally
        unlock(t.cond)
    end
    nothing
end

function uv_asynccb(handle::Ptr{Cvoid})
    async = @handle_as handle AsyncCondition
    lock(async.cond) # acquire barrier
    try
        @atomic :release async.set = true
        notify(async.cond, true)
    finally
        unlock(async.cond)
    end
    nothing
end

function uv_timercb(handle::Ptr{Cvoid})
    t = @handle_as handle Timer
    lock(t.cond)
    try
        @atomic :monotonic t.set = true
        if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Cvoid},), t) == 0
            # timer is stopped now
            close(t)
        end
        notify(t.cond, true)
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

Create a timer that runs the function `callback` at each timer expiration.

Waiting tasks are woken and the function `callback` is called after an initial delay of `delay`
seconds, and then repeating with the given `interval` in seconds. If `interval` is equal to `0`, the
callback is only run once. The function `callback` is called with a single argument, the timer
itself. Stop a timer by calling `close`. The `cb` may still be run one final time, if the timer has
already expired.

# Examples

Here the first number is printed after a delay of two seconds, then the following numbers are
printed quickly.

```julia-repl
julia> begin
           i = 0
           cb(timer) = (global i += 1; println(i))
           t = Timer(cb, 2, interval=0.2)
           wait(t)
           sleep(0.5)
           close(t)
       end
1
2
3
```
"""
function Timer(cb::Function, timeout::Real; interval::Real=0.0)
    timer = Timer(timeout, interval=interval)
    t = @task begin
        unpreserve_handle(timer)
        while _trywait(timer)
            try
                cb(timer)
            catch err
                write(stderr, "Error in Timer:\n")
                showerror(stderr, err, catch_backtrace())
                return
            end
            isopen(timer) || return
        end
    end
    # here we are mimicking parts of _trywait, in coordination with task `t`
    preserve_handle(timer)
    @lock timer.cond begin
        if timer.set
            schedule(t)
        else
            _wait2(timer.cond, t)
        end
    end
    return timer
end

"""
    timedwait(testcb, timeout::Real; pollint::Real=0.1)

Waits until `testcb()` returns `true` or `timeout` seconds have passed, whichever is earlier.
The test function is polled every `pollint` seconds. The minimum value for `pollint` is 0.001 seconds,
that is, 1 millisecond.

Returns :ok or :timed_out
"""
function timedwait(testcb, timeout::Real; pollint::Real=0.1)
    pollint >= 1e-3 || throw(ArgumentError("pollint must be ≥ 1 millisecond"))
    start = time_ns()
    ns_timeout = 1e9 * timeout

    testcb() && return :ok

    t = Timer(pollint, interval=pollint)
    while _trywait(t) # stop if we ever get closed
        if testcb()
            close(t)
            return :ok
        elseif (time_ns() - start) > ns_timeout
            close(t)
            break
        end
    end
    return :timed_out
end
