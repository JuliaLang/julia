# This file is a part of Julia. License is MIT: https://julialang.org/license

## async event notifications

"""
    AsyncCondition()

Create an async condition that wakes up tasks waiting for it
(by calling [`wait`](@ref) on the object)
when notified from C by a call to `uv_async_send`.
Waiting tasks are woken with an error when the object is closed (by [`close`](@ref)).
Use [`isopen`](@ref) to check whether it is still active. A closed condition is inactive and will
not wake up tasks.

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

Create an async condition that calls the given `callback` function. The `callback` is passed one argument,
the async condition object itself.
"""
AsyncCondition(cb::Function) = run_callback_task(cb, AsyncCondition())

function run_callback_task(cb::Function, async)
    # Shielded like the `Timer` callback task below: this task owns the
    # handle's lifetime and must survive a cancelled constructing scope.
    t = ScopedValues.with(CANCEL_TOKEN => nothing) do
        @task begin
            unpreserve_handle(async)
            while _trywait(async)
                cb(async)
                isopen(async) || return
            end
        end
    end
    # here we are mimicking parts of _trywait, in coordination with task `t`
    preserve_handle(async)
    @lock async.cond begin
        if async.set
            schedule(t)
        else
            schedule_on_notify!(async.cond, t)
        end
    end
    return async
end

## signal notifications

"""
    SignalCondition(signum::Integer)

Create a condition that wakes up tasks waiting for it (by calling [`wait`](@ref) on the
object) when the process receives the signal `signum`, for example `Base.SIGTERM` or
`Base.SIGHUP`. Several deliveries that arrive before a waiting task runs are reported once.

While any `SignalCondition` for `signum` is open, the signal no longer has its usual
effect: receiving `SIGTERM`, for instance, no longer exits Julia. Closing the last one with
[`close`](@ref) restores it. A handler therefore usually ends by calling [`exit`](@ref)
itself once it has shut down cleanly. Some effects happen regardless: a stopped process
still resumes on `SIGCONT`.

The signals that have a named constant in `Base` can be watched as follows:

| Signal          | Linux      | macOS, BSD | Windows | Usual effect                          |
|:--------------- |:---------- |:---------- |:------- |:------------------------------------- |
| `Base.SIGHUP`   | yes        | yes        | yes     | exit                                  |
| `Base.SIGINT`   | yes        | yes        | yes     | interrupt running code (see below)    |
| `Base.SIGQUIT`  | yes        | yes        | no      | print backtraces and exit             |
| `Base.SIGUSR1`  | yes        | yes        | no      | profile peek on Linux, exit on macOS  |
| `Base.SIGUSR2`  | no         | macOS only | no      | exit                                  |
| `Base.SIGALRM`  | yes        | yes        | no      | exit                                  |
| `Base.SIGTERM`  | yes        | yes        | no      | exit                                  |
| `Base.SIGCHLD`  | yes        | yes        | no      | none                                  |
| `Base.SIGCONT`  | yes        | yes        | no      | resume if stopped (always happens)    |
| `Base.SIGWINCH` | yes        | yes        | yes     | none                                  |
| `Base.SIGINFO`  | no         | yes        | no      | profile peek                          |

Without a `SignalCondition`, Ctrl-C (`SIGINT`) interrupts running code through task
cancellation (see [`CancellationToken`](@ref)), or exits a script. Watching `SIGINT` replaces
both: Ctrl-C then only notifies the condition, which is a safer place to stop work. If the
condition's task cannot run, for example because other code never yields, then on Unix
Ctrl-\\ (`SIGQUIT`) still stops Julia, unless `SIGQUIT` is also being watched.

The signals Julia itself relies on, such as `SIGSEGV`, `SIGPIPE` and `SIGUSR2` outside
macOS, cannot be watched, nor can the signals that cannot be caught (`SIGKILL`, `SIGSTOP`).
Watching one throws an `ArgumentError`.

On Windows, libuv reports console events as signals: Ctrl+C as `SIGINT`, Ctrl+Break as
`SIGBREAK` (21), closing the console as `SIGHUP`, and resizing it as `SIGWINCH`. No other
signals are available there. While `SIGHUP` is watched, closing the console no longer exits
Julia by itself, so the handler should call [`exit`](@ref); Windows ends the process a few
seconds after the console closes in any case.

Other signals can be watched by number. For real-time signals use
`Base.sigrtmin() + n`, like `SIGRTMIN+n` in C (see [`Base.sigrtmin`](@ref)). The numbers of
the rest, such as `SIGPROF`, differ between platforms; see `signal.h` or `man 7 signal`.
Watching `SIGTSTP` stops Ctrl-Z from suspending Julia.

Handling a signal needs the event loop to run, so a task busy in a loop that never yields
delays it until it does.

# Examples
```julia
term = Base.SignalCondition(Base.SIGTERM)
Threads.@spawn begin
    wait(term)
    save_checkpoint()
    exit(0)
end
```

!!! compat "Julia 1.14"
    `SignalCondition` requires at least Julia 1.14.
"""
mutable struct SignalCondition
    @atomic handle::Ptr{Cvoid}
    cond::ThreadSynchronizer
    @atomic isopen::Bool
    @atomic set::Bool
    signum::Cint

    function SignalCondition(signum::Integer)
        if !(typemin(Cint) <= signum <= typemax(Cint)) ||
                ccall(:jl_signal_is_reserved, Cint, (Cint,), signum) != 0
            throw(ArgumentError("signal $signum cannot be watched"))
        end
        this = new(Libc.malloc(_sizeof_uv_signal), ThreadSynchronizer(), true, false, signum)
        iolock_begin()
        associate_julia_struct(this.handle, this)
        err = ccall(:uv_signal_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), this)
        if err != 0
            Libc.free(this.handle)
            this.handle = C_NULL
            iolock_end()
            throw(_UVError("uv_signal_init", err))
        end
        finalizer(uvfinalize, this)
        err = ccall(:jl_start_signal_watcher, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cint),
            this, @cfunction(uv_signalcb, Cvoid, (Ptr{Cvoid}, Cint)), signum)
        iolock_end()
        if err != 0
            close(this)
            throw(_UVError("uv_signal_start", err))
        end
        return this
    end
end

"""
    SignalCondition(callback::Function, signum::Integer)

Create a [`SignalCondition`](@ref) that calls `callback` when the process receives the
signal `signum`. Several deliveries that arrive before the callback runs are reported once,
so the number of calls is not a count of signals. The `callback` is passed one argument, the
condition object itself.

# Examples
```julia
Base.SignalCondition(Base.SIGHUP) do _
    reload_config()
end
```
"""
SignalCondition(cb::Function, signum::Integer) = run_callback_task(cb, SignalCondition(signum))

function show(io::IO, s::SignalCondition)
    state = isopen(s) ? "open" : "closed"
    name = signal_name(s.signum)
    signal = name === nothing ? string(s.signum) : "$name ($(s.signum))"
    print(io, "SignalCondition($signal, $state)")
end

## timer-based notifications

"""
    Timer(delay; interval = 0)

Create a timer that wakes up tasks waiting for it (by calling [`wait`](@ref) on the timer object).

Waiting tasks are woken after an initial delay of at least `delay` seconds, and then repeating after
at least `interval` seconds again elapse. If `interval` is equal to `0`, the timer is only triggered
once. When closing (by [`close`](@ref)) either a repeating timer or a one-shot timer before it has
triggered, waiting tasks are woken with an error. After a one-shot timer triggers, all subsequent calls
to [`wait`](@ref) return immediately, even if it is closed.
Use [`isopen`](@ref) to check whether a timer is still active. An inactive timer will not fire.
Use `t.timeout` and `t.interval` to read the setup conditions of a `Timer` `t`.

```julia-repl
julia> t = Timer(1.0; interval=0.5)
Timer (open, timeout: 1.0 s, interval: 0.5 s) @0x000000010f4e6e90

julia> isopen(t)
true

julia> t.timeout
1.0

julia> close(t)

julia> isopen(t)
false
```

!!! note
    `interval` is subject to accumulating time skew. If you need precise events at a particular
    absolute time, create a new timer at each expiration with the difference to the next time computed.

!!! note
    A `Timer` requires yield points to update its state. For instance, `isopen(t::Timer)` cannot be
    used to timeout a non-yielding while loop.

!!! compat "Julia 1.12"
    The `timeout` and `interval` readable properties were added in Julia 1.12.

!!! compat "Julia 1.14"
    Prior to Julia 1.14, only the first call to `wait` on a triggered one-shot timer returned,
    and subsequent calls threw an `EOFError`.

"""
mutable struct Timer
    @atomic handle::Ptr{Cvoid}
    cond::ThreadSynchronizer
    @atomic isopen::Bool
    @atomic set::Bool
    timeout_ms::UInt64
    interval_ms::UInt64

    function Timer(timeout::Real; interval::Real = 0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        interval ≥ 0 || throw(ArgumentError("timer cannot have negative repeat interval of $interval seconds"))
        # libuv has a tendency to timeout 1 ms early, so we need +1 on the timeout (in milliseconds), unless it is zero
        timeoutms = ceil(UInt64, timeout * 1000) + !iszero(timeout)
        intervalms = ceil(UInt64, interval * 1000)
        loop = eventloop()

        this = new(Libc.malloc(_sizeof_uv_timer), ThreadSynchronizer(), true, false, timeoutms, intervalms)
        associate_julia_struct(this.handle, this)
        iolock_begin()
        err = ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), loop, this)
        @assert err == 0 "failed to initialize timer"
        finalizer(uvfinalize, this)
        ccall(:uv_update_time, Cvoid, (Ptr{Cvoid},), loop)
        err = ccall(:uv_timer_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64),
            this, @cfunction(uv_timercb, Cvoid, (Ptr{Cvoid},)),
            timeoutms, intervalms)
        @assert err == 0 "failed to start timer"
        iolock_end()
        return this
    end
end
function getproperty(t::Timer, f::Symbol)
    if f == :timeout
        t.timeout_ms == 0 && return 0.0
        return (t.timeout_ms - 1) / 1000 # remove the +1ms compensation from the constructor
    elseif f == :interval
        return t.interval_ms / 1000
    else
        return getfield(t, f)
    end
end
propertynames(::Timer) = (:handle, :cond, :isopen, :set, :timeout, :timeout_ms, :interval, :interval_ms)

function show(io::IO, t::Timer)
    state = isopen(t) ? "open" : "closed"
    interval = t.interval
    interval_str = interval > 0 ? ", interval: $(t.interval) s" : ""
    print(io, "Timer ($state, timeout: $(t.timeout) s$interval_str) @0x$(string(convert(UInt, pointer_from_objref(t)), base = 16, pad = Sys.WORD_SIZE>>2))")
end

unsafe_convert(::Type{Ptr{Cvoid}}, t::Timer) = t.handle
unsafe_convert(::Type{Ptr{Cvoid}}, async::AsyncCondition) = async.handle
unsafe_convert(::Type{Ptr{Cvoid}}, s::SignalCondition) = s.handle

# if this returns true, the object has been signaled
# if this returns false, the object is closed
# a cancellation of the governing token is thrown as a CancellationRequest
_trywait(t::Union{Timer, AsyncCondition, SignalCondition}; cancel::CancelTokenArg=DEFAULT_CANCEL) =
    _trywait(t, resolve_cancel_token(cancel))
function _trywait(t::Union{Timer, AsyncCondition, SignalCondition}, tok::MaybeToken)
    set = t.set
    if set
        # full barrier now for AsyncCondition
        t isa Timer || Core.Intrinsics.atomic_fence(:acquire_release, :system)
    else
        if !isopen(t)
            # the :acquire read of isopen pairs with the :release store in uv_timercb, which
            # sets `set` beforehand: a waiter observing the trigger-initiated close of a
            # one-shot timer cannot miss the trigger on this recheck
            set = t.set
            if !set
                close(t) # wait for the close to complete
                return false
            end
        end
        iolock_begin()
        set = t.set
        if !set
            preserve_handle(t)
            lock(t.cond)
            locked = true
            try
                set = t.set
                while !set && t.handle != C_NULL # wait for set or handle, but not the isopen flag
                    iolock_end()
                    locked = false
                    ret = wait(t.cond, tok)
                    locked = true
                    unlock(t.cond)
                    locked = false
                    iolock_begin()
                    lock(t.cond)
                    locked = true
                    if ret isa Bool
                        set = ret
                        break
                    end
                    # A wakeup that did not come from this object's notify:
                    # re-check the state and re-park.
                    set = t.set
                end
            finally
                locked && unlock(t.cond)
                unpreserve_handle(t)
            end
        end
        iolock_end()
    end
    if !(t isa Timer && iszero(t.interval_ms))
        # if there are multiple waiters, an unspecified number may short-circuit past here
        @atomic :monotonic t.set = false
    end
    return set
end

waitqueue(t::Union{Timer, AsyncCondition, SignalCondition}) = waitqueue(t.cond)

wait(t::Union{Timer, AsyncCondition, SignalCondition}; cancel::CancelTokenArg=DEFAULT_CANCEL) =
    wait(t, check_cancel_arg(cancel))
function wait(t::Union{Timer, AsyncCondition, SignalCondition}, tok::MaybeToken)
    ok = _trywait(t, tok)
    @cancel_check(tok)
    ok || throw(EOFError())
    nothing
end


isopen(t::Union{Timer, AsyncCondition, SignalCondition}) = @atomic :acquire t.isopen

"""
    close(t::Union{Timer, AsyncCondition, SignalCondition})

Close an object `t` and thus mark it as inactive. Once a timer or condition is inactive, it will not produce
a new event.

See also [`isopen`](@ref).
"""
function close(t::Union{Timer, AsyncCondition, SignalCondition})
    t.handle == C_NULL && !t.isopen && return # short-circuit path, :monotonic
    iolock_begin()
    if t.handle != C_NULL
        if t.isopen
            @atomic :release t.isopen = false
            ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
        end
        # implement _trywait here without the auto-reset function, just waiting for the final close signal
        preserve_handle(t)
        lock(t.cond)
        locked = true
        try
            while t.handle != C_NULL
                iolock_end()
                # close is the cleanup primitive: its (bounded) completion
                # wait is shielded from cancellation
                locked = false
                wait(t.cond, nothing)
                locked = true
                unlock(t.cond)
                locked = false
                iolock_begin()
                lock(t.cond)
                locked = true
            end
        finally
            locked && unlock(t.cond)
            unpreserve_handle(t)
        end
    elseif t.isopen
        @atomic :release t.isopen = false
    end
    iolock_end()
    nothing
end

function uvfinalize(t::Union{Timer, AsyncCondition, SignalCondition})
    iolock_begin()
    lock(t.cond)
    try
        if t.handle != C_NULL
            disassociate_julia_struct(t.handle) # not going to call the usual close hooks anymore
            if t.isopen
                @atomic :release t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t.handle) # this will call Libc.free
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

function _uv_hook_close(t::Union{Timer, AsyncCondition, SignalCondition})
    lock(t.cond)
    try
        handle = t.handle
        @atomic :release t.isopen = false
        @atomic :monotonic t.handle = C_NULL
        Libc.free(handle)
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

function uv_signalcb(handle::Ptr{Cvoid}, ::Cint)
    s = @handle_as handle SignalCondition
    lock(s.cond)
    try
        @atomic :release s.set = true
        notify(s.cond, true)
    finally
        unlock(s.cond)
    end
    nothing
end

function uv_timercb(handle::Ptr{Cvoid})
    t = @handle_as handle Timer
    lock(t.cond)
    try
        # this store must stay ordered before the :release store of isopen below, so that
        # a waiter observing the close in _trywait is guaranteed to also observe `set`
        @atomic :monotonic t.set = true
        if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Cvoid},), t) == 0
            # timer is stopped now
            if t.isopen
                @atomic :release t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
            end
        end
        notify(t.cond, true)
    finally
        unlock(t.cond)
    end
    nothing
end

"""
    sleep(seconds; cancel=Base.DEFAULT_CANCEL)

Block the current task for a specified number of seconds. The minimum sleep time is 1
millisecond or input of `0.001`.

A cancellation of the governing token (by default the scoped token, see
[`CancellationToken`](@ref)) interrupts the sleep by throwing the
[`CancellationRequest`](@ref).
"""
function sleep(sec::Real; cancel::CancelTokenArg=DEFAULT_CANCEL)
    sec ≥ 0 || throw(ArgumentError("cannot sleep for $sec seconds"))
    tok = check_cancel_arg(cancel)
    t = Timer(sec)
    try
        wait(t, tok)
    finally
        close(t)
    end
    nothing
end

# timer with repeated callback
"""
    Timer(callback::Function, delay; interval = 0, spawn::Union{Nothing,Bool}=nothing)

Create a timer that runs the function `callback` at each timer expiration.

Waiting tasks are woken and the function `callback` is called after an initial delay of `delay`
seconds, and then repeating with the given `interval` in seconds. If `interval` is equal to `0`, the
callback is only run once. The function `callback` is called with a single argument, the timer
itself. Stop a timer by calling `close`. The `callback` may still be run one final time, if the timer
has already expired.

If `spawn` is `true`, the created task will be spawned, meaning that it will be allowed
to move thread, which avoids the side-effect of forcing the parent task to get stuck to the thread
it is on. If `spawn` is `nothing` (default), the task will be spawned if the parent task isn't sticky.

!!! compat "Julia 1.12"
    The `spawn` argument was introduced in Julia 1.12.

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
function Timer(cb::Function, timeout; spawn::Union{Nothing,Bool}=nothing, kwargs...)
    sticky = spawn === nothing ? current_task().sticky : !spawn
    timer = Timer(timeout; kwargs...)
    # The callback task carries the timer's lifetime (its preserve is
    # balanced in the body): shield it from the constructing scope's
    # cancellation token, so a cancelled scope can neither leak the preserve
    # nor stop the timer - `close(timer)` is what ends it.
    t = ScopedValues.with(CANCEL_TOKEN => nothing) do
        @task begin
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
    end
    t.sticky = sticky
    # here we are mimicking parts of _trywait, in coordination with task `t`
    preserve_handle(timer)
    @lock timer.cond begin
        if timer.set
            schedule(t)
        else
            schedule_on_notify!(timer.cond, t)
        end
    end
    return timer
end

"""
    timedwait(testcb, timeout::Real; pollint::Real=0.1)

Wait until `testcb()` returns `true` or `timeout` seconds have passed, whichever is earlier.
The test function is polled every `pollint` seconds. The minimum value for `pollint` is 0.001 seconds,
that is, 1 millisecond.

Return `:ok` or `:timed_out`.

# Examples
```jldoctest
julia> cb() = (sleep(5); return);

julia> t = @async cb();

julia> timedwait(()->istaskdone(t), 1)
:timed_out

julia> timedwait(()->istaskdone(t), 6.5)
:ok
```
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
        elseif (time_ns() -% start) > ns_timeout
            close(t)
            break
        end
    end
    return :timed_out
end

## A deadline as a waitable (see base/park.jl): `TimeoutWait(dt)` in a
## park's waitables wakes the wait with `:timed_out` after `dt` seconds.
## Its enqueue - running inside `park!`, after the arm - starts the timer
## and spawns the claimer task, which wakes the parked task through the
## standard expected-entry claim CAS. That CAS is a *specific-wait* waker:
## it is only sound against a fresh, single-use entry (which the entry
## cache contract supplies automatically - any non-canonical waitable
## shape gets a fresh entry), since entry identity is what scopes the
## claim to this wait and not a later one of the same task. The claimed
## waitq registration is left for the driver's lazy settle; the dequeue
## closes the timer on every exit path.
mutable struct TimeoutWait
    const timeout::Float64
    timer::Union{Timer, Nothing}
    TimeoutWait(timeout::Real) = new(Float64(timeout), nothing)
end

function wait_enqueue!(x::TimeoutWait, w::WaitEntry, first::Bool)
    ct = current_task()
    timer = Timer(x.timeout)
    x.timer = timer
    t = Task() do
        try
            # not cancellable: internal mechanism; closing the timer wakes
            # this task on every exit path of the governed wait
            wait(timer; cancel=nothing)
        catch e
            # a closed timer means the wait ended first; do nothing
            e isa EOFError && return
            rethrow()
        end
        if (@atomicreplace ct.waiting_on w => nothing).success
            schedule(ct, :timed_out)
        end
    end
    t.sticky = false
    Threads._spawn_set_thrpool(t, :interactive)
    schedule(t)
    return true
end

function wait_dequeue!(x::TimeoutWait, w::WaitEntry, why::UInt8)
    timer = x.timer
    timer === nothing || close(timer)
    x.timer = nothing
    return nothing
end
