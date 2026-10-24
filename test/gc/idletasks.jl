# This file is a part of Julia. License is MIT: https://julialang.org/license

# Young collections skip suspended old tasks unless they reference young objects or have
# run since the last collection. Check that objects only reachable from the stack of a
# task that ran in between survive.

struct Node
    v::Int
    next::Union{Node,Nothing}
end

function build(n, seed)
    l = nothing
    for i in 1:n
        l = Node(seed + i, l)
    end
    return l
end

function check(l, n, seed)
    s = c = 0
    while l !== nothing
        c > n && return false # a freed list may have been reused into a cycle
        s += l.v
        c += 1
        l = l.next
    end
    return c == n && s == n * seed + n * (n + 1) ÷ 2
end

function worker(ch, i, rounds, bad)
    held = build(20, i) # only reachable from this task's stack
    for r in 1:rounds
        take!(ch)
        check(held, 20, i + r - 1) || Threads.atomic_add!(bad, 1)
        held = build(20, i + r)
    end
end

function run_idletasks(ntasks, rounds)
    bad = Threads.Atomic{Int}(0)
    chans = [Channel{Int}(1) for _ in 1:ntasks]
    tasks = [Threads.@spawn worker(chans[i], i, rounds, bad) for i in 1:ntasks]
    GC.gc(true)
    for r in 1:rounds
        for i in 1:ntasks
            put!(chans[i], r)
            i % 50 == 0 && GC.gc(false)
        end
        GC.gc(false)
        r % 5 == 0 && GC.gc(true)
    end
    foreach(wait, tasks)
    return bad[]
end

# Deterministic version. Each round resumes every task once to allocate a list held only on
# its stack and waits until the task has suspended again before collecting. The two young
# collections at the end of a round promote the list and then drop the (now clean) task from
# the remset, so the next resume must put it back.
function coordinated_worker(go, done, i, rounds)
    ok = true
    held = nothing
    for r in 1:rounds
        take!(go)
        r > 1 && (ok &= check(held, 20, i + r - 1))
        held = build(20, i + r)
        put!(done, i)
    end
    take!(go)
    return ok & check(held, 20, i + rounds)
end

function run_coordinated(ntasks, rounds)
    gos = [Channel{Int}(0) for _ in 1:ntasks]
    done = Channel{Int}(ntasks)
    tasks = [Threads.@spawn coordinated_worker(gos[i], done, i, rounds) for i in 1:ntasks]
    GC.gc(true)
    for r in 1:rounds
        for i in 1:ntasks
            put!(gos[i], r)
            take!(done)
        end
        GC.gc(false)
        junk = [build(20, -i) for i in 1:ntasks] # reuse any wrongly freed memory
        GC.gc(false)
    end
    foreach(ch -> put!(ch, 0), gos)
    return count(!fetch, tasks)
end

@assert run_coordinated(200, 5) == 0
@assert run_idletasks(200, 10) == 0
