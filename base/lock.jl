# This file is a part of Julia. License is MIT: https://julialang.org/license

const ThreadSynchronizer = GenericCondition{Threads.SpinLock}

# Advisory reentrant lock
"""
    ReentrantLock()

Creates a re-entrant lock for synchronizing [`Task`](@ref)s.
The same task can acquire the lock as many times as required.
Each [`lock`](@ref) must be matched with an [`unlock`](@ref).
"""
mutable struct ReentrantLock <: AbstractLock
    locked_by::Union{Task, Nothing}
    cond_wait::ThreadSynchronizer
    reentrancy_cnt::Int

    ReentrantLock() = new(nothing, ThreadSynchronizer(), 0)
end

assert_havelock(l::ReentrantLock) = assert_havelock(l, l.locked_by)

"""
    islocked(lock) -> Status (Boolean)

Check whether the `lock` is held by any task/thread.
This should not be used for synchronization (see instead [`trylock`](@ref)).
"""
function islocked(rl::ReentrantLock)
    return rl.reentrancy_cnt != 0
end

"""
    trylock(lock) -> Success (Boolean)

Acquire the lock if it is available,
and return `true` if successful.
If the lock is already locked by a different task/thread,
return `false`.

Each successful `trylock` must be matched by an [`unlock`](@ref).
"""
function trylock(rl::ReentrantLock)
    t = current_task()
    if t === rl.locked_by
        rl.reentrancy_cnt += 1
        return true
    end
    lock(rl.cond_wait)
    if rl.reentrancy_cnt == 0
        rl.locked_by = t
        rl.reentrancy_cnt = 1
        got = true
    else
        got = false
    end
    unlock(rl.cond_wait)
    return got
end

"""
    lock(lock)

Acquire the `lock` when it becomes available.
If the lock is already locked by a different task/thread,
wait for it to become available.

Each `lock` must be matched by an [`unlock`](@ref).
"""
function lock(rl::ReentrantLock)
    t = current_task()
    if t === rl.locked_by
        rl.reentrancy_cnt += 1
    else
        lock(rl.cond_wait)
        while true
            if rl.reentrancy_cnt == 0
                rl.locked_by = t
                rl.reentrancy_cnt = 1
                break
            end
            try
                wait(rl.cond_wait)
            catch
                unlock(rl.cond_wait)
                rethrow()
            end
        end
        unlock(rl.cond_wait)
    end
    return
end

"""
    unlock(lock)

Releases ownership of the `lock`.

If this is a recursive lock which has been acquired before, decrement an
internal counter and return immediately.
"""
function unlock(rl::ReentrantLock)
    t = current_task()
    n = rl.reentrancy_cnt
    n == 0 && error("unlock count must match lock count")
    rl.locked_by === t || error("unlock from wrong thread")
    if n > 1
        rl.reentrancy_cnt = n - 1
    else
        lock(rl.cond_wait)
        rl.reentrancy_cnt = 0
        rl.locked_by = nothing
        if !isempty(rl.cond_wait.waitq)
            try
                notify(rl.cond_wait)
            catch
                unlock(rl.cond_wait)
                rethrow()
            end
        end
        unlock(rl.cond_wait)
    end
    return
end

function unlockall(rl::ReentrantLock)
    t = current_task()
    n = rl.reentrancy_cnt
    rl.locked_by === t || error("unlock from wrong thread")
    n == 0 && error("unlock count must match lock count")
    lock(rl.cond_wait)
    rl.reentrancy_cnt = 0
    rl.locked_by = nothing
    if !isempty(rl.cond_wait.waitq)
        try
            notify(rl.cond_wait)
        catch
            unlock(rl.cond_wait)
            rethrow()
        end
    end
    unlock(rl.cond_wait)
    return n
end

function relockall(rl::ReentrantLock, n::Int)
    t = current_task()
    lock(rl)
    n1 = rl.reentrancy_cnt
    rl.reentrancy_cnt = n
    n1 == 1 || concurrency_violation()
    return
end

"""
    lock(f::Function, lock)

Acquire the `lock`, execute `f` with the `lock` held, and release the `lock` when `f`
returns. If the lock is already locked by a different task/thread, wait for it to become
available.

When this function returns, the `lock` has been released, so the caller should
not attempt to `unlock` it.
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
    Event()

Create a level-triggered event source. Tasks that call [`wait`](@ref) on an
`Event` are suspended and queued until `notify` is called on the `Event`.
After `notify` is called, the `Event` remains in a signaled state and
tasks will no longer block when waiting for it.

!!! compat "Julia 1.1"
    This functionality requires at least Julia 1.1.
"""
mutable struct Event
    notify::Threads.Condition
    set::Bool
    Event() = new(Threads.Condition(), false)
end

function wait(e::Event)
    e.set && return
    lock(e.notify)
    try
        while !e.set
            wait(e.notify)
        end
    finally
        unlock(e.notify)
    end
    nothing
end

function notify(e::Event)
    lock(e.notify)
    try
        if !e.set
            e.set = true
            notify(e.notify)
        end
    finally
        unlock(e.notify)
    end
    nothing
end

@eval Threads begin
    import .Base: Event
    export Event
end
