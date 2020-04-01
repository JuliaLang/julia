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
    set::Bool

    function AsyncCondition()
        this = new(Libc.malloc(_sizeof_uv_async), ThreadSynchronizer(), true, false)
        iolock_begin()
        associate_julia_struct(this.handle, this)
        err = ccall(:uv_async_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
            eventloop(), this, uv_jl_asynccb::Ptr{Cvoid})
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
    @async while _trywait(async)
            cb(async)
            isopen(async) || return
        end
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
    set::Bool

    function Timer(timeout::Real; interval::Real = 0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        interval ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $interval seconds"))
        timeout = UInt64(round(timeout * 1000)) + 1
        interval = UInt64(round(interval * 1000))
        loop = eventloop()

        this = new(Libc.malloc(_sizeof_uv_timer), ThreadSynchronizer(), true, false)
        associate_julia_struct(this.handle, this)
        iolock_begin()
        err = ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), loop, this)
        @assert err == 0
        finalizer(uvfinalize, this)
        ccall(:uv_update_time, Cvoid, (Ptr{Cvoid},), loop)
        err = ccall(:uv_timer_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64),
            this, uv_jl_timercb::Ptr{Cvoid}, timeout, interval)
        @assert err == 0
        iolock_end()
        return this
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, t::Timer) = t.handle
unsafe_convert(::Type{Ptr{Cvoid}}, async::AsyncCondition) = async.handle

function _trywait(t::Union{Timer, AsyncCondition})
    set = t.set
    if !set
        t.handle == C_NULL && return false
        iolock_begin()
        set = t.set
        if !set
            preserve_handle(t)
            lock(t.cond)
            try
                set = t.set
                if !set
                    if t.handle != C_NULL
                        iolock_end()
                        set = wait(t.cond)
                        unlock(t.cond)
                        iolock_begin()
                        lock(t.cond)
                    end
                end
            finally
                unlock(t.cond)
                unpreserve_handle(t)
            end
        end
        iolock_end()
    end
    t.set = false
    return set
end

function wait(t::Union{Timer, AsyncCondition})
    _trywait(t) || throw(EOFError())
    nothing
end


isopen(t::Union{Timer, AsyncCondition}) = t.isopen

function close(t::Union{Timer, AsyncCondition})
    iolock_begin()
    if t.handle != C_NULL && isopen(t)
        t.isopen = false
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
            disassociate_julia_struct(t.handle) # not going to call the usual close hooks
            if t.isopen
                t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
            end
            t.handle = C_NULL
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
        t.isopen = false
        t.handle = C_NULL
        notify(t.cond, t.set)
    finally
        unlock(t.cond)
    end
    nothing
end

function uv_asynccb(handle::Ptr{Cvoid})
    async = @handle_as handle AsyncCondition
    lock(async.cond)
    try
        async.set = true
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
        t.set = true
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
    @async while _trywait(timer)
            cb(timer)
            isopen(timer) || return
        end
    return timer
end

"""
    timedwait(testcb::Function, secs::Real; pollint::Real=0.1)

Waits until `testcb` returns `true` or for `secs` seconds, whichever is earlier.
`testcb` is polled every `pollint` seconds. The minimum duration for `secs` and `pollint` is
1 millisecond or `0.001`.

Returns :ok or :timed_out
"""
function timedwait(testcb::Function, secs::Real; pollint::Real=0.1)
    pollint >= 1e-3 || throw(ArgumentError("pollint must be ≥ 1 millisecond"))
    start = time_ns()
    nsecs = 1e9 * secs
    done = Channel(1)
    function timercb(aw)
        try
            if testcb()
                put!(done, (:ok, nothing))
            elseif (time_ns() - start) > nsecs
                put!(done, (:timed_out, nothing))
            end
        catch e
            put!(done, (:error, CapturedException(e, catch_backtrace())))
        finally
            isready(done) && close(aw)
        end
        nothing
    end

    try
        testcb() && return :ok
    catch e
        throw(CapturedException(e, catch_backtrace()))
    end

    t = Timer(timercb, pollint, interval = pollint)
    ret, e = fetch(done)
    close(t)

    ret === :error && throw(e)

    return ret
end
