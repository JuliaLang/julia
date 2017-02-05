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
#    channel::Channel{Int}
#    workers::Set{Int}
#

type WorkerPool <: AbstractWorkerPool
    channel::Channel{Int}
    workers::Set{Int}
    ref::RemoteChannel

    WorkerPool(c::Channel, ref::RemoteChannel) = new(c, Set{Int}(), ref)
end

function WorkerPool()
    wp = WorkerPool(Channel{Int}(typemax(Int)), RemoteChannel())
    put!(wp.ref, wp)
    wp
end

"""
    WorkerPool(workers::Vector{Int})

Create a WorkerPool from a vector of worker ids.
"""
function WorkerPool(workers::Vector{Int})
    pool = WorkerPool()
    foreach(w->push!(pool, w), workers)
    return pool
end

# On workers where this pool has been serialized to, instantiate with a dummy local channel.
WorkerPool(ref::RemoteChannel) = WorkerPool(Channel{Int}(1), ref)

function serialize(S::AbstractSerializer, pool::WorkerPool)
    # Allow accessing a worker pool from other processors. When serialized,
    # initialize the `ref` to point to self and only send the ref.
    # Other workers will forward all put!, take!, calls to the process owning
    # the ref (and hence the pool).
    Serializer.serialize_type(S, typeof(pool))
    serialize(S, pool.ref)
end

deserialize{T<:WorkerPool}(S::AbstractSerializer, t::Type{T}) = T(deserialize(S))

wp_local_push!(pool::AbstractWorkerPool, w::Int) = (push!(pool.workers, w); put!(pool.channel, w); pool)
wp_local_length(pool::AbstractWorkerPool) = length(pool.workers)
wp_local_isready(pool::AbstractWorkerPool) = isready(pool.channel)

function wp_local_put!(pool::AbstractWorkerPool, w::Int)
    # In case of default_worker_pool, the master is implictly considered a worker, i.e.,
    # it is not present in pool.workers.
    # Confirm the that the worker is part of a pool before making it available.
    w in pool.workers && put!(pool.channel, w)
    w
end

function wp_local_workers(pool::AbstractWorkerPool)
    if length(pool) == 0 && pool === default_worker_pool()
        return [1]
    else
        return collect(pool.workers)
    end
end

function wp_local_nworkers(pool::AbstractWorkerPool)
    if length(pool) == 0 && pool === default_worker_pool()
        return 1
    else
        return length(pool.workers)
    end
end

function wp_local_take!(pool::AbstractWorkerPool)
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
        put!(pool, worker)
    end
end

# Check if pool is local or remote and forward calls if required.
# NOTE: remotecall_fetch does it automatically, but this will be more efficient as
# it avoids the overhead associated with a local remotecall.

for func = (:length, :isready, :workers, :nworkers, :take!)
    func_local = Symbol(string("wp_local_", func))
    @eval begin
        function ($func)(pool::WorkerPool)
            if pool.ref.where != myid()
                return remotecall_fetch(ref->($func_local)(fetch(ref)), pool.ref.where, pool.ref)
            else
                return ($func_local)(pool)
            end
        end

        # default impl
        ($func)(pool::AbstractWorkerPool) = ($func_local)(pool)
    end
end

for func = (:push!, :put!)
    func_local = Symbol(string("wp_local_", func))
    @eval begin
        function ($func)(pool::WorkerPool, w::Int)
            if pool.ref.where != myid()
                return remotecall_fetch((ref, w)->($func_local)(fetch(ref), w), pool.ref.where, pool.ref, w)
            else
                return ($func_local)(pool, w)
            end
        end

        # default impl
        ($func)(pool::AbstractWorkerPool, w::Int) = ($func_local)(pool, w)
    end
end


"""
    remotecall(f, pool::AbstractWorkerPool, args...; kwargs...) -> Future

`WorkerPool` variant of `remotecall(f, pid, ....)`. Waits for and takes a free worker from `pool` and performs a `remotecall` on it.
"""
remotecall(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall, f, pool, args...; kwargs...)


"""
    remotecall_wait(f, pool::AbstractWorkerPool, args...; kwargs...) -> Future

`WorkerPool` variant of `remotecall_wait(f, pid, ....)`. Waits for and takes a free worker from `pool` and
performs a `remotecall_wait` on it.
"""
remotecall_wait(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall_wait, f, pool, args...; kwargs...)


"""
    remotecall_fetch(f, pool::AbstractWorkerPool, args...; kwargs...) -> result

`WorkerPool` variant of `remotecall_fetch(f, pid, ....)`. Waits for and takes a free worker from `pool` and
performs a `remotecall_fetch` on it.
"""
remotecall_fetch(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remotecall_fetch, f, pool, args...; kwargs...)

"""
    remote_do(f, pool::AbstractWorkerPool, args...; kwargs...) -> nothing

`WorkerPool` variant of `remote_do(f, pid, ....)`. Waits for and takes a free worker from `pool` and
performs a `remote_do` on it.
"""
remote_do(f, pool::AbstractWorkerPool, args...; kwargs...) = remotecall_pool(remote_do, f, pool, args...; kwargs...)

const _default_worker_pool = Ref{Nullable}(Nullable{WorkerPool}())

"""
    default_worker_pool()

`WorkerPool` containing idle `workers()` - used by `remote(f)` and [`pmap`](@ref) (by default).
"""
function default_worker_pool()
    # On workers retrieve the default worker pool from the master when accessed
    # for the first time
    if isnull(_default_worker_pool[])
        if myid() == 1
            _default_worker_pool[] = Nullable(WorkerPool())
        else
            _default_worker_pool[] = Nullable(remotecall_fetch(()->default_worker_pool(), 1))
        end
    end
    return get(_default_worker_pool[])
end

"""
    remote([::AbstractWorkerPool], f) -> Function

Returns an anonymous function that executes function `f` on an available worker
using [`remotecall_fetch`](@ref).
"""
remote(f) = (args...; kwargs...)->remotecall_fetch(f, default_worker_pool(), args...; kwargs...)
remote(p::AbstractWorkerPool, f) = (args...; kwargs...)->remotecall_fetch(f, p, args...; kwargs...)

type CachingPool <: AbstractWorkerPool
    channel::Channel{Int}
    workers::Set{Int}

    # Mapping between a tuple (worker_id, f) and a remote_ref
    map_obj2ref::Dict{Tuple{Int, Function}, RemoteChannel}

    function CachingPool()
        wp = new(Channel{Int}(typemax(Int)), Set{Int}(), Dict{Int, Function}())
        finalizer(wp, clear!)
        wp
    end
end

serialize(s::AbstractSerializer, cp::CachingPool) = throw(ErrorException("CachingPool objects are not serializable."))

"""
    CachingPool(workers::Vector{Int})

An implementation of an `AbstractWorkerPool`.
[`remote`](@ref), [`remotecall_fetch`](@ref),
[`pmap`](@ref) (and other remote calls which execute functions remotely)
benefit from caching the serialized/deserialized functions on the worker nodes,
especially closures (which may capture large amounts of data).

The remote cache is maintained for the lifetime of the returned `CachingPool` object.
To clear the cache earlier, use `clear!(pool)`.

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
        put!(pool, worker)
    end
end
