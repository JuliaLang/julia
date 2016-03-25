# This file is a part of Julia. License is MIT: http://julialang.org/license

type WorkerPool
    channel::RemoteChannel{Channel{Int}}
    count::Int

    # Create a shared queue of available workers
    WorkerPool() = new(RemoteChannel(()->Channel{Int}(typemax(Int))), 0)
end


"""
    WorkerPool(workers)

Create a WorkerPool from a vector of worker ids.
"""
function WorkerPool(workers::Vector{Int})
    pool = WorkerPool()

    # Add workers to the pool
    for w in workers
        put!(pool, w)
    end

    return pool
end


put!(pool::WorkerPool, w::Int) = (pool.count += 1; put!(pool.channel, w))
put!(pool::WorkerPool, w::Worker) = put!(pool, w.id)

length(pool::WorkerPool) = pool.count

isready(pool::WorkerPool) = isready(pool.channel)


"""
    remotecall_fetch(f, pool::WorkerPool, args...)

Call `f(args...)` on one of the workers in `pool`.
"""
function remotecall_fetch(f, pool::WorkerPool, args...)
    worker = take!(pool.channel)
    try
        remotecall_fetch(f, worker, args...)
    finally
        put!(pool.channel, worker)
    end
end


"""
    default_worker_pool()

WorkerPool containing idle `workers()` (used by `remote(f)`).
"""
_default_worker_pool = Nullable{WorkerPool}()
function default_worker_pool()
    if isnull(_default_worker_pool) && myid() == 1
        set_default_worker_pool(WorkerPool())
    end
    return get(_default_worker_pool)
end

function set_default_worker_pool(p::WorkerPool)
    global _default_worker_pool = Nullable(p)
end


"""
    remote([::WorkerPool], f) -> Function

Returns a lambda that executes function `f` on an available worker
using `remotecall_fetch`.
"""
remote(f) = (args...)->remotecall_fetch(f, default_worker_pool(), args...)
remote(p::WorkerPool, f) = (args...)->remotecall_fetch(f, p, args...)
