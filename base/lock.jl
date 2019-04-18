# This file is a part of Julia. License is MIT: https://julialang.org/license

# Advisory reentrant lock
"""
    ReentrantLock()

Creates a re-entrant lock for synchronizing [`Task`](@ref)s.
The same task can acquire the lock as many times as required.
Each [`lock`](@ref) must be matched with an [`unlock`](@ref).
"""
mutable struct ReentrantLock <: AbstractLock
    locked_by::Union{Task, Nothing}
    cond_wait::GenericCondition{Threads.SpinLock}
    reentrancy_cnt::Int

    ReentrantLock() = new(nothing, GenericCondition{Threads.SpinLock}(), 0)
end


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
    lock(rl.cond_wait)
    try
        if rl.reentrancy_cnt == 0
            rl.locked_by = t
            rl.reentrancy_cnt = 1
            return true
        elseif t == notnothing(rl.locked_by)
            rl.reentrancy_cnt += 1
            return true
        end
        return false
    finally
        unlock(rl.cond_wait)
    end
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
    lock(rl.cond_wait)
    try
        while true
            if rl.reentrancy_cnt == 0
                rl.locked_by = t
                rl.reentrancy_cnt = 1
                return
            elseif t == notnothing(rl.locked_by)
                rl.reentrancy_cnt += 1
                return
            end
            wait(rl.cond_wait)
        end
    finally
        unlock(rl.cond_wait)
    end
end

"""
    unlock(lock)

Releases ownership of the `lock`.

If this is a recursive lock which has been acquired before, decrement an
internal counter and return immediately.
"""
function unlock(rl::ReentrantLock)
    t = current_task()
    rl.reentrancy_cnt == 0 && error("unlock count must match lock count")
    rl.locked_by == t || error("unlock from wrong thread")
    lock(rl.cond_wait)
    try
        rl.reentrancy_cnt -= 1
        if rl.reentrancy_cnt == 0
            rl.locked_by = nothing
            notify(rl.cond_wait)
        end
    finally
        unlock(rl.cond_wait)
    end
    return
end

function unlockall(rl::ReentrantLock)
    t = current_task()
    n = rl.reentrancy_cnt
    rl.locked_by == t || error("unlock from wrong thread")
    n == 0 && error("unlock count must match lock count")
    lock(rl.cond_wait)
    try
        rl.reentrancy_cnt = 0
        rl.locked_by = nothing
        notify(rl.cond_wait)
    finally
        unlock(rl.cond_wait)
    end
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

@eval Threads begin
    """
        Threads.Condition([lock])

    A thread-safe version of [`Base.Condition`](@ref).

    !!! compat "Julia 1.2"
        This functionality requires at least Julia 1.2.
    """
    const Condition = Base.GenericCondition{Base.ReentrantLock}

    """
    Special note for [`Threads.Condition`](@ref):

    The caller must be holding the [`lock`](@ref) that owns `c` before calling this method.
    The calling task will be blocked until some other task wakes it,
    usually by calling [`notify`](@ref)` on the same Condition object.
    The lock will be atomically released when blocking (even if it was locked recursively),
    and will be reacquired before returning.
    """
    wait(c::Condition)
end

const ThreadSynchronizer = GenericCondition{Threads.SpinLock}

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
