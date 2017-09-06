# This file is a part of Julia. License is MIT: https://julialang.org/license

# Advisory reentrant lock
"""
    ReentrantLock()

Creates a reentrant lock for synchronizing Tasks.
The same task can acquire the lock as many times as required.
Each `lock` must be matched with an `unlock`.

This lock is NOT threadsafe. See `Threads.Mutex` for a threadsafe lock.
"""
mutable struct ReentrantLock
    locked_by::Union{Some{Task}, Void}
    cond_wait::Condition
    reentrancy_cnt::Int

    ReentrantLock() = new(nothing, Condition(), 0)
end

"""
    islocked(the_lock) -> Status (Boolean)

Check whether the lock is held by any task/thread.
This should not be used for synchronization (see instead `trylock`).
"""
function islocked(rl::ReentrantLock)
    return rl.reentrancy_cnt != 0
end

"""
    trylock(the_lock) -> Success (Boolean)

Acquires the lock if it is available,
returning `true` if successful.
If the lock is already locked by a different task/thread,
returns `false`.

Each successful `trylock` must be matched by an `unlock`.
"""
function trylock(rl::ReentrantLock)
    t = current_task()
    if rl.reentrancy_cnt == 0
        rl.locked_by = Some(t)
        rl.reentrancy_cnt = 1
        return true
    elseif t == get(rl.locked_by)
        rl.reentrancy_cnt += 1
        return true
    end
    return false
end

"""
    lock(the_lock)

Acquires the lock when it becomes available.
If the lock is already locked by a different task/thread,
it waits for it to become available.

Each `lock` must be matched by an `unlock`.
"""
function lock(rl::ReentrantLock)
    t = current_task()
    while true
        if rl.reentrancy_cnt == 0
            rl.locked_by = Some(t)
            rl.reentrancy_cnt = 1
            return
        elseif t == get(rl.locked_by)
            rl.reentrancy_cnt += 1
            return
        end
        wait(rl.cond_wait)
    end
end

"""
    unlock(the_lock)

Releases ownership of the lock.

If this is a recursive lock which has been acquired before, it
just decrements an internal counter and returns immediately.
"""
function unlock(rl::ReentrantLock)
    if rl.reentrancy_cnt == 0
        error("unlock count must match lock count")
    end
    rl.reentrancy_cnt -= 1
    if rl.reentrancy_cnt == 0
        rl.locked_by = nothing
        notify(rl.cond_wait)
    end
    return
end

function lock(f, l)
    lock(l)
    try
        return f()
    finally
        unlock(l)
    end
end

function trylock(f, l)
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
    Semaphore(sem_size)

Creates a counting semaphore that allows at most `sem_size`
acquires to be in use at any time.
Each acquire must be mached with a release.

This construct is NOT threadsafe.
"""
mutable struct Semaphore
    sem_size::Int
    curr_cnt::Int
    cond_wait::Condition
    Semaphore(sem_size) = sem_size > 0 ? new(sem_size, 0, Condition()) : throw(ArgumentError("Semaphore size must be > 0"))
end

"""
    acquire(s::Semaphore)

Wait for one of the `sem_size` permits to be available,
blocking until one can be acquired.
"""
function acquire(s::Semaphore)
    while true
        if s.curr_cnt < s.sem_size
            s.curr_cnt = s.curr_cnt + 1
            return
        else
            wait(s.cond_wait)
        end
    end
end

"""
    release(s::Semaphore)

Return one permit to the pool,
possibly allowing another task to acquire it
and resume execution.
"""
function release(s::Semaphore)
    @assert s.curr_cnt > 0 "release count must match acquire count"
    s.curr_cnt -= 1
    notify(s.cond_wait; all=false)
end
