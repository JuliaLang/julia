# This file is a part of Julia. License is MIT: http://julialang.org/license

# Advisory reentrant lock
type ReentrantLock
    locked_by::Nullable{Task}
    cond_wait::Condition
    reentrancy_cnt::Int

    ReentrantLock() = new(nothing, Condition(), 0)
end

function lock(rl::ReentrantLock)
    t = current_task()
    while true
        if rl.reentrancy_cnt == 0
            rl.locked_by = t
            rl.reentrancy_cnt = 1
            return
        elseif t == get(rl.locked_by)
            rl.reentrancy_cnt += 1
            return
        end
        wait(rl.cond_wait)
    end
end

function unlock(rl::ReentrantLock)
    rl.reentrancy_cnt = rl.reentrancy_cnt - 1
    if rl.reentrancy_cnt < 0
        error("unlock count must match lock count")
    end
    if rl.reentrancy_cnt == 0
        rl.locked_by = nothing
        notify(rl.cond_wait)
    end
    return rl
end

type Semaphore
    sem_size::Int
    curr_cnt::Int
    cond_wait::Condition
    Semaphore(sem_size) = new(sem_size, 0, Condition())
end

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

function release(s::Semaphore)
    s.curr_cnt = s.curr_cnt - 1
    notify(s.cond_wait; all=false)
end

