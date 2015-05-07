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
