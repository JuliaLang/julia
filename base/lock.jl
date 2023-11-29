# This file is a part of Julia. License is MIT: https://julialang.org/license

const ThreadSynchronizer = GenericCondition{Threads.SpinLock}

# This bit is set in the `havelock` of a `ReentrantLock` when that lock is locked by some task.
const LOCKED_BIT = 0b01
# This bit is set in the `havelock` of a `ReentrantLock` just before parking a task. A task is being
# parked if it wants to lock the lock, but it is currently being held by some other task.
const PARKED_BIT = 0b10

# Spins without yielding the thread to the OS.
#
# Instead, the backoff is simply capped at a maximum value. This can be
# used to improve throughput in `compare_exchange` loops that have high
# contention.
@inline function spin(iteration::Int)
    if iteration >= 10
        return 10
    end

    for _ in 1:(1 << iteration)
        ccall(:jl_cpu_pause, Cvoid, ())
    end
    return iteration + 1
end

# Advisory reentrant lock
"""
    ReentrantLock()

Creates a re-entrant lock for synchronizing [`Task`](@ref)s. The same task can
acquire the lock as many times as required (this is what the "Reentrant" part
of the name means). Each [`lock`](@ref) must be matched with an [`unlock`](@ref).

Calling `lock` will also inhibit running of finalizers on that thread until the
corresponding `unlock`. Use of the standard lock pattern illustrated below
should naturally be supported, but beware of inverting the try/lock order or
missing the try block entirely (e.g. attempting to return with the lock still
held):

This provides a acquire/release memory ordering on lock/unlock calls.

```
lock(l)
try
    <atomic work>
finally
    unlock(l)
end
```

If [`!islocked(lck::ReentrantLock)`](@ref islocked) holds, [`trylock(lck)`](@ref trylock)
succeeds unless there are other tasks attempting to hold the lock "at the same time."
"""
mutable struct ReentrantLock <: AbstractLock
    # offset = 16
    @atomic locked_by::Union{Task, Nothing}
    # offset32 = 20, offset64 = 24
    reentrancy_cnt::UInt32
    # offset32 = 24, offset64 = 28
    #
    # This atomic integer holds the current state of the lock instance. Only the two lowest bits
    # are used. See `LOCKED_BIT` and `PARKED_BIT` for the bitmask for these bits.
    #
    # # State table:
    #
    # PARKED_BIT | LOCKED_BIT | Description
    #     0      |     0      | The lock is not locked, nor is anyone waiting for it.
    # -----------+------------+------------------------------------------------------------------
    #     0      |     1      | The lock is locked by exactly one task. No other task is
    #            |            | waiting for it.
    # -----------+------------+------------------------------------------------------------------
    #     1      |     0      | The lock is not locked. One or more tasks are parked.
    # -----------+------------+------------------------------------------------------------------
    #     1      |     1      | The lock is locked by exactly one task. One or more tasks are
    #            |            | parked waiting for the lock to become available.
    #            |            | In this state, PARKED_BIT is only ever cleared when the cond_wait lock
    #            |            | is held (i.e. on unlock). This ensures that
    #            |            | we never end up in a situation where there are parked tasks but
    #            |            | PARKED_BIT is not set (which would result in those tasks
    #            |            | potentially never getting woken up).
    @atomic havelock::UInt8
    # offset32 = 28, offset64 = 32
    cond_wait::ThreadSynchronizer # 2 words
    # offset32 = 36, offset64 = 48
    # sizeof32 = 20, sizeof64 = 32
    # now add padding to make this a full cache line to minimize false sharing between objects
    _::NTuple{Int === Int32 ? 2 : 3, Int}
    # offset32 = 44, offset64 = 72 == sizeof+offset
    # sizeof32 = 28, sizeof64 = 56

    ReentrantLock() = new(nothing, 0x0000_0000, 0x00, ThreadSynchronizer())
end

assert_havelock(l::ReentrantLock) = assert_havelock(l, l.locked_by)

"""
    islocked(lock) -> Status (Boolean)

Check whether the `lock` is held by any task/thread.
This function alone should not be used for synchronization. However, `islocked` combined
with [`trylock`](@ref) can be used for writing the test-and-test-and-set or exponential
backoff algorithms *if it is supported by the `typeof(lock)`* (read its documentation).

# Extended help

For example, an exponential backoff can be implemented as follows if the `lock`
implementation satisfied the properties documented below.

```julia
nspins = 0
while true
    while islocked(lock)
        GC.safepoint()
        nspins += 1
        nspins > LIMIT && error("timeout")
    end
    trylock(lock) && break
    backoff()
end
```

## Implementation

A lock implementation is advised to define `islocked` with the following properties and note
it in its docstring.

* `islocked(lock)` is data-race-free.
* If `islocked(lock)` returns `false`, an immediate invocation of `trylock(lock)` must
  succeed (returns `true`) if there is no interference from other tasks.
"""
function islocked end
# Above docstring is a documentation for the abstract interface and not the one specific to
# `ReentrantLock`.

function islocked(rl::ReentrantLock)
    return (@atomic :monotonic rl.havelock) & LOCKED_BIT != 0
end

"""
    trylock(lock) -> Success (Boolean)

Acquire the lock if it is available,
and return `true` if successful.
If the lock is already locked by a different task/thread,
return `false`.

Each successful `trylock` must be matched by an [`unlock`](@ref).

Function `trylock` combined with [`islocked`](@ref) can be used for writing the
test-and-test-and-set or exponential backoff algorithms *if it is supported by the
`typeof(lock)`* (read its documentation).
"""
function trylock end
# Above docstring is a documentation for the abstract interface and not the one specific to
# `ReentrantLock`.

@inline function trylock(rl::ReentrantLock)
    ct = current_task()
    if rl.locked_by === ct
        #@assert rl.havelock !== 0x00
        rl.reentrancy_cnt += 0x0000_0001
        return true
    end
    return _trylock(rl, ct)
end
@noinline function _trylock(rl::ReentrantLock, ct::Task)
    state = @atomic :monotonic rl.havelock
    while true
        if state & LOCKED_BIT != 0
            return false
        end

        GC.disable_finalizers()
        result = (@atomicreplace :acquire :monotonic rl.havelock state => state | LOCKED_BIT)
        if result.success
            #@assert rl.locked_by === nothing
            #@assert rl.reentrancy_cnt === 0
            rl.reentrancy_cnt = 0x0000_0001
            @atomic :release rl.locked_by = ct
            return true
        else
            state = result.old
        end
        GC.enable_finalizers()
    end
end

"""
    lock(lock)

Acquire the `lock` when it becomes available.
If the lock is already locked by a different task/thread,
wait for it to become available.

Each `lock` must be matched by an [`unlock`](@ref).
"""
@inline function lock(rl::ReentrantLock)
    trylock(rl) || (@noinline function slowlock(rl::ReentrantLock)
        c = rl.cond_wait
        ct = current_task()
        iteration = 0
        state = @atomic :monotonic rl.havelock
        while true
            # Grab the lock if it isn't locked, even if there is a queue on it
            if state & LOCKED_BIT == 0
                GC.disable_finalizers()
                result = (@atomicreplace :acquire :monotonic rl.havelock state => (state | LOCKED_BIT))
                if result.success
                    rl.reentrancy_cnt = 0x0000_0001
                    @atomic :release rl.locked_by = ct
                    return
                else
                    state = result.old
                end
                GC.enable_finalizers()
                continue
            end

            # If there is no queue, try spinning a few times
            iteration += 1
            if state & PARKED_BIT == 0 && iteration < 10
                state = @atomic :monotonic rl.havelock
                continue
            end

            # If still not locked, set the parked bit
            if state & PARKED_BIT == 0
                result = (@atomicreplace :monotonic :monotonic rl.havelock state => (state | PARKED_BIT))
                if !result.success
                    state = result.old
                    continue
                end
            end

            # With the parked bit set, lock the `cond_wait`
            lock(c.lock)
            try
                # Last check before we wait to make sure `unlock` did not win the race
                # to the `cond_wait` lock and cleared the parked bit
                state = @atomic :acquire rl.havelock
                if state != LOCKED_BIT | PARKED_BIT
                    continue
                end

                # It was locked, so now wait for the unlock to notify us
                wait(c)
            finally
                unlock(c.lock)
            end

            # Loop back and try locking again
            iteration = 0
            state = @atomic :monotonic rl.havelock
        end
    end)(rl)
    return
end

"""
    unlock(lock)

Releases ownership of the `lock`.

If this is a recursive lock which has been acquired before, decrement an
internal counter and return immediately.
"""
@inline function unlock(rl::ReentrantLock)
    rl.locked_by === current_task() ||
        error(rl.reentrancy_cnt == 0x0000_0000 ? "unlock count must match lock count" : "unlock from wrong thread")
    (@noinline function _unlock(rl::ReentrantLock)
        n = rl.reentrancy_cnt - 0x0000_0001
        rl.reentrancy_cnt = n
        if n == 0x0000_00000
            @atomic :monotonic rl.locked_by = nothing
            result = (@atomicreplace :release :monotonic rl.havelock LOCKED_BIT => 0x00)
            if result.success
                return true
            else
                (@noinline function notifywaiters(rl)
                    cond_wait = rl.cond_wait
                    lock(cond_wait)
                    try

                        tasks_notified = notify(cond_wait, all=false)
                        if tasks_notified == 0
                            # Either:
                            # - We won the race to the `cond_wait` lock as a task was about to park
                            # - We are the last task on the queue
                            #
                            # Unlock anyway as any parking task will retry

                            @atomic :release rl.havelock = 0x00
                        elseif isempty(cond_wait.waitq)
                            # We notified the last task, unlock and unset the parked bit
                            @atomic :release rl.havelock = 0x00
                        else
                            # There are more tasks on the queue, we unlock and keep the parked bit set
                            @atomic :release rl.havelock = PARKED_BIT
                            notify(cond_wait, all=false)
                        end
                    finally
                        unlock(cond_wait)
                    end
                end)(rl)
                return true
            end
        end
        return false
    end)(rl) && GC.enable_finalizers()
    nothing
end

function unlockall(rl::ReentrantLock)
    n = @atomicswap :not_atomic rl.reentrancy_cnt = 0x0000_0001
    unlock(rl)
    return n
end

function relockall(rl::ReentrantLock, n::UInt32)
    lock(rl)
    old = @atomicswap :not_atomic rl.reentrancy_cnt = n
    old == 0x0000_0001 || concurrency_violation()
    return
end

"""
    lock(f::Function, lock)

Acquire the `lock`, execute `f` with the `lock` held, and release the `lock` when `f`
returns. If the lock is already locked by a different task/thread, wait for it to become
available.

When this function returns, the `lock` has been released, so the caller should
not attempt to `unlock` it.

!!! compat "Julia 1.7"
    Using a [`Channel`](@ref) as the second argument requires Julia 1.7 or later.
"""
function lock(f, l::AbstractLock)
    lock(l)
    try
        return f()
    finally
        unlock(l)
    end
end

function trylock(f, l::AbstractLock)
    if trylock(l)
        try
            return f()
        finally
            unlock(l)
        end
    end
    return false
end

"""
    @lock l expr

Macro version of `lock(f, l::AbstractLock)` but with `expr` instead of `f` function.
Expands to:
```julia
lock(l)
try
    expr
finally
    unlock(l)
end
```
This is similar to using [`lock`](@ref) with a `do` block, but avoids creating a closure
and thus can improve the performance.
"""
macro lock(l, expr)
    quote
        temp = $(esc(l))
        lock(temp)
        try
            $(esc(expr))
        finally
            unlock(temp)
        end
    end
end

"""
    @lock_nofail l expr

Equivalent to `@lock l expr` for cases in which we can guarantee that the function
will not throw any error. In this case, avoiding try-catch can improve the performance.
See [`@lock`](@ref).
"""
macro lock_nofail(l, expr)
    quote
        temp = $(esc(l))
        lock(temp)
        val = $(esc(expr))
        unlock(temp)
        val
    end
end

@eval Threads begin
    """
        Threads.Condition([lock])

    A thread-safe version of [`Base.Condition`](@ref).

    To call [`wait`](@ref) or [`notify`](@ref) on a `Threads.Condition`, you must first call
    [`lock`](@ref) on it. When `wait` is called, the lock is atomically released during
    blocking, and will be reacquired before `wait` returns. Therefore idiomatic use
    of a `Threads.Condition` `c` looks like the following:

    ```
    lock(c)
    try
        while !thing_we_are_waiting_for
            wait(c)
        end
    finally
        unlock(c)
    end
    ```

    !!! compat "Julia 1.2"
        This functionality requires at least Julia 1.2.
    """
    const Condition = Base.GenericCondition{Base.ReentrantLock}

    """
    Special note for [`Threads.Condition`](@ref):

    The caller must be holding the [`lock`](@ref) that owns a `Threads.Condition` before calling this method.
    The calling task will be blocked until some other task wakes it,
    usually by calling [`notify`](@ref) on the same `Threads.Condition` object.
    The lock will be atomically released when blocking (even if it was locked recursively),
    and will be reacquired before returning.
    """
    wait(c::Condition)
end

"""
    Semaphore(sem_size)

Create a counting semaphore that allows at most `sem_size`
acquires to be in use at any time.
Each acquire must be matched with a release.

This provides a acquire & release memory ordering on acquire/release calls.
"""
mutable struct Semaphore
    sem_size::Int
    curr_cnt::Int
    cond_wait::Threads.Condition
    Semaphore(sem_size) = sem_size > 0 ? new(sem_size, 0, Threads.Condition()) : throw(ArgumentError("Semaphore size must be > 0"))
end

"""
    acquire(s::Semaphore)

Wait for one of the `sem_size` permits to be available,
blocking until one can be acquired.
"""
function acquire(s::Semaphore)
    lock(s.cond_wait)
    try
        while s.curr_cnt >= s.sem_size
            wait(s.cond_wait)
        end
        s.curr_cnt = s.curr_cnt + 1
    finally
        unlock(s.cond_wait)
    end
    return
end

"""
    acquire(f, s::Semaphore)

Execute `f` after acquiring from Semaphore `s`,
and `release` on completion or error.

For example, a do-block form that ensures only 2
calls of `foo` will be active at the same time:

```julia
s = Base.Semaphore(2)
@sync for _ in 1:100
    Threads.@spawn begin
        Base.acquire(s) do
            foo()
        end
    end
end
```

!!! compat "Julia 1.8"
    This method requires at least Julia 1.8.

"""
function acquire(f, s::Semaphore)
    acquire(s)
    try
        return f()
    finally
        release(s)
    end
end

"""
    release(s::Semaphore)

Return one permit to the pool,
possibly allowing another task to acquire it
and resume execution.
"""
function release(s::Semaphore)
    lock(s.cond_wait)
    try
        s.curr_cnt > 0 || error("release count must match acquire count")
        s.curr_cnt -= 1
        notify(s.cond_wait; all=false)
    finally
        unlock(s.cond_wait)
    end
    return
end


"""
    Event([autoreset=false])

Create a level-triggered event source. Tasks that call [`wait`](@ref) on an
`Event` are suspended and queued until [`notify`](@ref) is called on the `Event`.
After `notify` is called, the `Event` remains in a signaled state and
tasks will no longer block when waiting for it, until `reset` is called.

If `autoreset` is true, at most one task will be released from `wait` for
each call to `notify`.

This provides an acquire & release memory ordering on notify/wait.

!!! compat "Julia 1.1"
    This functionality requires at least Julia 1.1.

!!! compat "Julia 1.8"
    The `autoreset` functionality and memory ordering guarantee requires at least Julia 1.8.
"""
mutable struct Event
    const notify::ThreadSynchronizer
    const autoreset::Bool
    @atomic set::Bool
    Event(autoreset::Bool=false) = new(ThreadSynchronizer(), autoreset, false)
end

function wait(e::Event)
    if e.autoreset
        (@atomicswap :acquire_release e.set = false) && return
    else
        (@atomic e.set) && return # full barrier also
    end
    lock(e.notify) # acquire barrier
    try
        if e.autoreset
            (@atomicswap :acquire_release e.set = false) && return
        else
            e.set && return
        end
        wait(e.notify)
    finally
        unlock(e.notify) # release barrier
    end
    nothing
end

function notify(e::Event)
    lock(e.notify) # acquire barrier
    try
        if e.autoreset
            if notify(e.notify, all=false) == 0
                @atomic :release e.set = true
            end
        elseif !e.set
            @atomic :release e.set = true
            notify(e.notify)
        end
    finally
        unlock(e.notify)
    end
    nothing
end

"""
    reset(::Event)

Reset an [`Event`](@ref) back into an un-set state. Then any future calls to `wait` will
block until [`notify`](@ref) is called again.
"""
function reset(e::Event)
    @atomic e.set = false # full barrier
    nothing
end

@eval Threads begin
    import .Base: Event
    export Event
end
