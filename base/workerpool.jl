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

function remotecall_pool(rc_f, f, pool::WorkerPool, args...; kwargs...)
    # Find an active worker
    worker = 0
    while true
        if pool.count == 0
            if pool === default_worker_pool()
                # No workers, the master process is used as a worker
                worker = 1
                break
            else
                throw(ErrorException("No active worker available in pool"))
            end
        end

        worker = take!(pool.channel)
        if worker in procs()
            break
        else
            pool.count = pool.count - 1
        end
    end

    try
        rc_f(f, worker, args...; kwargs...)
    finally
        if worker != 1
            put!(pool.channel, worker)
        end
    end
end


"""
    remotecall(f, pool::WorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Returns a `Future`.
"""
remotecall(f, pool::WorkerPool, args...; kwargs...) = remotecall_pool(remotecall, f, pool, args...; kwargs...)


"""
    remotecall_wait(f, pool::WorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Waits for completion, returns a `Future`.
"""
remotecall_wait(f, pool::WorkerPool, args...; kwargs...) = remotecall_pool(remotecall_wait, f, pool, args...; kwargs...)


"""
    remotecall_fetch(f, pool::WorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Waits for completion and returns the result.
"""
remotecall_fetch(f, pool::WorkerPool, args...; kwargs...) = remotecall_pool(remotecall_fetch, f, pool, args...; kwargs...)

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
remote(f) = (args...; kwargs...)->remotecall_fetch(f, default_worker_pool(), args...; kwargs...)
remote(p::WorkerPool, f) = (args...; kwargs...)->remotecall_fetch(f, p, args...; kwargs...)
