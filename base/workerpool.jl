# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractWorkerPool

# An AbstractWorkerPool should implement
#
# `push!` - add a new worker to the overall pool (available + busy)
# `put!` - put back a worker to the available pool
# `take!` - take a worker from the available pool (to be used for remote function execution)
# `length` - number of workers available in the overall pool
# `isready` - return false if a `take!` on the pool would block, else true
#
# The default implementations of the above (on a AbstractWorkerPool) require fields
#    channel::RemoteChannel{Channel{Int}}
#    workers::Set{Int}
#

type WorkerPool <: AbstractWorkerPool
    channel::RemoteChannel{Channel{Int}}
    workers::Set{Int}

    # Create a shared queue of available workers
    WorkerPool() = new(RemoteChannel(()->Channel{Int}(typemax(Int))), Set{Int}())
end


"""
    WorkerPool(workers)

Create a WorkerPool from a vector of worker ids.
"""
function WorkerPool(workers::Vector{Int})
    pool = WorkerPool()

    # Add workers to the pool
    for w in workers
        push!(pool, w)
    end

    return pool
end

push!(pool::AbstractWorkerPool, w::Int) = (push!(pool.workers, w); put!(pool.channel, w); pool)
push!(pool::AbstractWorkerPool, w::Worker) = push!(pool, w.id)
length(pool::AbstractWorkerPool) = length(pool.workers)
isready(pool::AbstractWorkerPool) = isready(pool.channel)

put!(pool::AbstractWorkerPool, w::Int) = (put!(pool.channel, w); pool)

function workers(pool::AbstractWorkerPool)
    if length(pool) == 0 && pool === default_worker_pool()
        return [1]
    else
        return collect(pool.workers)
    end
end

function nworkers(pool::AbstractWorkerPool)
    if length(pool) == 0 && pool === default_worker_pool()
        return 1
    else
        return length(pool.workers)
    end
end

function take!(pool::AbstractWorkerPool)
    # Find an active worker
    worker = 0
    while true
        if length(pool) == 0
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
            delete!(pool.workers, worker) # Remove invalid worker from pool
        end
    end
    return worker
end

function remotecall_pool(rc_f, f, pool::AbstractWorkerPool, args...; kwargs...)
    worker = take!(pool)
    try
        rc_f(f, worker, args...; kwargs...)
    finally
        # In case of default_worker_pool, the master is implictly considered a worker
        # till the time new workers are added, and it is not added back to the available pool.
        # However, it is perfectly valid for other pools to `push!` any worker (including 1)
        # to the pool. Confirm the same before making a worker available.
        worker in pool.workers && put!(pool, worker)
    end
end

"""
    remotecall(f, pool::AbstractWorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Returns a `Future`.
"""
remotecall(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall, f, pool, args...; kwargs...)


"""
    remotecall_wait(f, pool::AbstractWorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Waits for completion, returns a `Future`.
"""
remotecall_wait(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall_wait, f, pool, args...; kwargs...)


"""
    remotecall_fetch(f, pool::AbstractWorkerPool, args...; kwargs...)

Call `f(args...; kwargs...)` on one of the workers in `pool`. Waits for completion and returns the result.
"""
remotecall_fetch(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall_fetch, f, pool, args...; kwargs...)

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
    remote([::AbstractWorkerPool], f) -> Function

Returns a lambda that executes function `f` on an available worker
using `remotecall_fetch`.
"""
remote(f) = (args...; kwargs...)->remotecall_fetch(f, default_worker_pool(), args...; kwargs...)
remote(p::AbstractWorkerPool, f) = (args...; kwargs...)->remotecall_fetch(f, p, args...; kwargs...)

type CachingPool <: AbstractWorkerPool
    channel::RemoteChannel{Channel{Int}}
    workers::Set{Int}

    # Mapping between a tuple (worker_id, f) and a remote_ref
    map_obj2ref::Dict{Tuple{Int, Function}, RemoteChannel}

    function CachingPool()
        wp = new(RemoteChannel(()->Channel{Int}(typemax(Int))), Set{Int}(), Dict{Int, Function}())
        finalizer(wp, clear!)
        wp
    end
end

"""
    CachingPool(workers::Vector{Int})

An implementation of an `AbstractWorkerPool`. `remote`, `remotecall_fetch`, `pmap` and other
remote calls which execute functions remotely, benefit from caching the serialized/deserialized
functions on the worker nodes, especially for closures which capture large amounts of data.

The remote cache is maintained for the lifetime of the returned `CachingPool` object. To clear the
cache earlier, use `clear!(pool)`.

For global variables, only the bindings are captured in a closure, not the data.
`let` blocks can be used to capture global data.

For example:
```
const foo=rand(10^8);
wp=CachingPool(workers())
let foo=foo
    pmap(wp, i->sum(foo)+i, 1:100);
end
```

The above would transfer `foo` only once to each worker.

"""
function CachingPool(workers::Vector{Int})
    pool = CachingPool()
    for w in workers
        push!(pool, w)
    end
    return pool
end

CachingPool(wp::WorkerPool) = CachingPool(workers(wp))

"""
    clear!(pool::CachingPool) -> pool

Removes all cached functions from all participating workers.
"""
function clear!(pool::CachingPool)
    for (_,rr) in pool.map_obj2ref
        finalize(rr)
    end
    empty!(pool.map_obj2ref)
    pool
end

exec_from_cache(rr::RemoteChannel, args...; kwargs...) = fetch(rr)(args...; kwargs...)
function exec_from_cache(f_ref::Tuple{Function, RemoteChannel}, args...; kwargs...)
    put!(f_ref[2], f_ref[1])        # Cache locally
    f_ref[1](args...; kwargs...)
end

function remotecall_pool(rc_f, f, pool::CachingPool, args...; kwargs...)
    worker = take!(pool)
    f_ref = get(pool.map_obj2ref, (worker, f), (f, RemoteChannel(worker)))
    isa(f_ref, Tuple) && (pool.map_obj2ref[(worker, f)] = f_ref[2])   # Add to tracker

    try
        rc_f(exec_from_cache, worker, f_ref, args...; kwargs...)
    finally
        # ensure that we do not add pid 1 back if it is not registered.
        worker in pool.workers && put!(pool, worker)
    end
end
