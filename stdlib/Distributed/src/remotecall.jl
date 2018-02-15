# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    client_refs

Tracks whether a particular `AbstractRemoteRef`
(identified by its RRID) exists on this worker.

The `client_refs` lock is also used to synchronize access to `.refs` and associated `clientset` state.
"""
const client_refs = WeakKeyDict{Any, Nothing}() # used as a WeakKeySet

abstract type AbstractRemoteRef end

"""
    Future(pid::Integer=myid())

Create a `Future` on process `pid`.
The default `pid` is the current process.
"""
mutable struct Future <: AbstractRemoteRef
    where::Int
    whence::Int
    id::Int
    v::Union{Some{Any}, Nothing}

    Future(w::Int, rrid::RRID, v::Union{Some, Nothing}=nothing) =
        (r = new(w,rrid.whence,rrid.id,v); return test_existing_ref(r))

    Future(t::NTuple{4, Any}) = new(t[1],t[2],t[3],t[4])  # Useful for creating dummy, zeroed-out instances
end

"""
    RemoteChannel(pid::Integer=myid())

Make a reference to a `Channel{Any}(1)` on process `pid`.
The default `pid` is the current process.

    RemoteChannel(f::Function, pid::Integer=myid())

Create references to remote channels of a specific size and type. `f` is a function that
when executed on `pid` must return an implementation of an `AbstractChannel`.

For example, `RemoteChannel(()->Channel{Int}(10), pid)`, will return a reference to a
channel of type `Int` and size 10 on `pid`.

The default `pid` is the current process.
"""
mutable struct RemoteChannel{T<:AbstractChannel} <: AbstractRemoteRef
    where::Int
    whence::Int
    id::Int

    function RemoteChannel{T}(w::Int, rrid::RRID) where T<:AbstractChannel
        r = new(w, rrid.whence, rrid.id)
        return test_existing_ref(r)
    end

    function RemoteChannel{T}(t::Tuple) where T<:AbstractChannel
        return new(t[1],t[2],t[3])
    end
end

function test_existing_ref(r::AbstractRemoteRef)
    found = getkey(client_refs, r, nothing)
    if found !== nothing
        @assert r.where > 0
        if isa(r, Future) && found.v === nothing && r.v !== nothing
            # we have recd the value from another source, probably a deserialized ref, send a del_client message
            send_del_client(r)
            found.v = r.v
        end
        return found::typeof(r)
    end

    client_refs[r] = nothing
    finalizer(finalize_ref, r)
    return r
end

function finalize_ref(r::AbstractRemoteRef)
    if r.where > 0 # Handle the case of the finalizer having been called manually
        if islocked(client_refs)
            # delay finalizer for later, when it's not already locked
            finalizer(finalize_ref, r)
            return nothing
        end
        delete!(client_refs, r)
        if isa(r, RemoteChannel)
            send_del_client(r)
        else
            # send_del_client only if the reference has not been set
            r.v === nothing && send_del_client(r)
            r.v = nothing
        end
        r.where = 0
    end
    nothing
end

Future(w::LocalProcess) = Future(w.id)
Future(w::Worker) = Future(w.id)
Future(pid::Integer=myid()) = Future(pid, RRID())

RemoteChannel(pid::Integer=myid()) = RemoteChannel{Channel{Any}}(pid, RRID())

function RemoteChannel(f::Function, pid::Integer=myid())
    remotecall_fetch(pid, f, RRID()) do f, rrid
        rv=lookup_ref(rrid, f)
        RemoteChannel{typeof(rv.c)}(myid(), rrid)
    end
end

hash(r::AbstractRemoteRef, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::AbstractRemoteRef, s::AbstractRemoteRef) = (r.whence==s.whence && r.id==s.id)

"""
    remoteref_id(r::AbstractRemoteRef) -> RRID

`Future`s and `RemoteChannel`s are identified by fields:

* `where` - refers to the node where the underlying object/storage
  referred to by the reference actually exists.

* `whence` - refers to the node the remote reference was created from.
  Note that this is different from the node where the underlying object
  referred to actually exists. For example calling `RemoteChannel(2)`
  from the master process would result in a `where` value of 2 and
  a `whence` value of 1.

* `id` is unique across all references created from the worker specified by `whence`.

Taken together,  `whence` and `id` uniquely identify a reference across all workers.

`remoteref_id` is a low-level API which returns a `RRID`
object that wraps `whence` and `id` values of a remote reference.
"""
remoteref_id(r::AbstractRemoteRef) = RRID(r.whence, r.id)

"""
    channel_from_id(id) -> c

A low-level API which returns the backing `AbstractChannel` for an `id` returned by
[`remoteref_id`](@ref).
The call is valid only on the node where the backing channel exists.
"""
function channel_from_id(id)
    rv = lock(client_refs) do
        return get(PGRP.refs, id, false)
    end
    if rv === false
        throw(ErrorException("Local instance of remote reference not found"))
    end
    return rv.c
end

lookup_ref(rrid::RRID, f=def_rv_channel) = lookup_ref(PGRP, rrid, f)
function lookup_ref(pg, rrid, f)
    return lock(client_refs) do
        rv = get(pg.refs, rrid, false)
        if rv === false
            # first we've heard of this ref
            rv = RemoteValue(invokelatest(f))
            pg.refs[rrid] = rv
            push!(rv.clientset, rrid.whence)
        end
        return rv
    end::RemoteValue
end

"""
    isready(rr::Future)

Determine whether a [`Future`](@ref) has a value stored to it.

If the argument `Future` is owned by a different node, this call will block to wait for the answer.
It is recommended to wait for `rr` in a separate task instead
or to use a local [`Channel`](@ref) as a proxy:

    c = Channel(1)
    @async put!(c, remotecall_fetch(long_computation, p))
    isready(c)  # will not block
"""
function isready(rr::Future)
    rr.v === nothing || return true

    rid = remoteref_id(rr)
    return if rr.where == myid()
        isready(lookup_ref(rid).c)
    else
        remotecall_fetch(rid->isready(lookup_ref(rid).c), rr.where, rid)
    end
end

"""
    isready(rr::RemoteChannel, args...)

Determine whether a [`RemoteChannel`](@ref) has a value stored to it.
Note that this function can cause race conditions, since by the
time you receive its result it may no longer be true. However,
it can be safely used on a [`Future`](@ref) since they are assigned only once.
"""
function isready(rr::RemoteChannel, args...)
    rid = remoteref_id(rr)
    return if rr.where == myid()
        isready(lookup_ref(rid).c, args...)
    else
        remotecall_fetch(rid->isready(lookup_ref(rid).c, args...), rr.where, rid)
    end
end

del_client(rr::AbstractRemoteRef) = del_client(remoteref_id(rr), myid())

del_client(id, client) = del_client(PGRP, id, client)
function del_client(pg, id, client)
    lock(client_refs) do
        rv = get(pg.refs, id, false)
        if rv !== false
            delete!(rv.clientset, client)
            if isempty(rv.clientset)
                delete!(pg.refs, id)
                #print("$(myid()) collected $id\n")
            end
        end
    end
    nothing
end

function del_clients(pairs::Vector)
    for p in pairs
        del_client(p[1], p[2])
    end
end

any_gc_flag = Condition()
function start_gc_msgs_task()
    @schedule while true
        wait(any_gc_flag)
        flush_gc_msgs()
    end
end

function send_del_client(rr)
    if rr.where == myid()
        del_client(rr)
    elseif id_in_procs(rr.where) # process only if a valid worker
        w = worker_from_id(rr.where)
        push!(w.del_msgs, (remoteref_id(rr), myid()))
        w.gcflag = true
        notify(any_gc_flag)
    end
end

function add_client(id, client)
    lock(client_refs) do
        rv = lookup_ref(id)
        push!(rv.clientset, client)
    end
    nothing
end

function add_clients(pairs::Vector)
    for p in pairs
        add_client(p[1], p[2]...)
    end
end

function send_add_client(rr::AbstractRemoteRef, i)
    if rr.where == myid()
        add_client(remoteref_id(rr), i)
    elseif (i != rr.where) && id_in_procs(rr.where)
        # don't need to send add_client if the message is already going
        # to the processor that owns the remote ref. it will add_client
        # itself inside deserialize().
        w = worker_from_id(rr.where)
        push!(w.add_msgs, (remoteref_id(rr), i))
        w.gcflag = true
        notify(any_gc_flag)
    end
end

channel_type(rr::RemoteChannel{T}) where {T} = T

serialize(s::ClusterSerializer, f::Future) = serialize(s, f, f.v === nothing)
serialize(s::ClusterSerializer, rr::RemoteChannel) = serialize(s, rr, true)
function serialize(s::ClusterSerializer, rr::AbstractRemoteRef, addclient)
    if addclient
        p = worker_id_from_socket(s.io)
        (p !== rr.where) && send_add_client(rr, p)
    end
    invoke(serialize, Tuple{ClusterSerializer, Any}, s, rr)
end

function deserialize(s::ClusterSerializer, t::Type{<:Future})
    f = deserialize_rr(s,t)
    Future(f.where, RRID(f.whence, f.id), f.v) # ctor adds to client_refs table
end

function deserialize(s::ClusterSerializer, t::Type{<:RemoteChannel})
    rr = deserialize_rr(s,t)
    # call ctor to make sure this rr gets added to the client_refs table
    RemoteChannel{channel_type(rr)}(rr.where, RRID(rr.whence, rr.id))
end

function deserialize_rr(s, t)
    rr = invoke(deserialize, Tuple{ClusterSerializer, DataType}, s, t)
    if rr.where == myid()
        # send_add_client() is not executed when the ref is being
        # serialized to where it exists
        add_client(remoteref_id(rr), myid())
    end
    rr
end

# Future and RemoteChannel are serializable only in a running cluster.
# Serialize zeroed-out values to non ClusterSerializer objects
function serialize(s::AbstractSerializer, ::Future)
    zero_fut = Future((0,0,0,nothing))
    invoke(serialize, Tuple{AbstractSerializer, Any}, s, zero_fut)
end

function serialize(s::AbstractSerializer, ::RemoteChannel)
    zero_rc = RemoteChannel{Channel{Any}}((0,0,0))
    invoke(serialize, Tuple{AbstractSerializer, Any}, s, zero_rc)
end


# make a thunk to call f on args in a way that simulates what would happen if
# the function were sent elsewhere
function local_remotecall_thunk(f, args, kwargs)
    if isempty(args) && isempty(kwargs)
        return f
    end
    return ()->f(args...; kwargs...)
end

function remotecall(f, w::LocalProcess, args...; kwargs...)
    rr = Future(w)
    schedule_call(remoteref_id(rr), local_remotecall_thunk(f, args, kwargs))
    return rr
end

function remotecall(f, w::Worker, args...; kwargs...)
    rr = Future(w)
    send_msg(w, MsgHeader(remoteref_id(rr)), CallMsg{:call}(f, args, kwargs))
    return rr
end

"""
    remotecall(f, id::Integer, args...; kwargs...) -> Future

Call a function `f` asynchronously on the given arguments on the specified process.
Return a [`Future`](@ref).
Keyword arguments, if any, are passed through to `f`.
"""
remotecall(f, id::Integer, args...; kwargs...) = remotecall(f, worker_from_id(id), args...; kwargs...)

function remotecall_fetch(f, w::LocalProcess, args...; kwargs...)
    v=run_work_thunk(local_remotecall_thunk(f,args, kwargs), false)
    return isa(v, RemoteException) ? throw(v) : v
end

function remotecall_fetch(f, w::Worker, args...; kwargs...)
    # can be weak, because the program will have no way to refer to the Ref
    # itself, it only gets the result.
    oid = RRID()
    rv = lookup_ref(oid)
    rv.waitingfor = w.id
    send_msg(w, MsgHeader(RRID(0,0), oid), CallMsg{:call_fetch}(f, args, kwargs))
    v = take!(rv)
    lock(client_refs) do
        delete!(PGRP.refs, oid)
    end
    return isa(v, RemoteException) ? throw(v) : v
end

"""
    remotecall_fetch(f, id::Integer, args...; kwargs...)

Perform `fetch(remotecall(...))` in one message.
Keyword arguments, if any, are passed through to `f`.
Any remote exceptions are captured in a
[`RemoteException`](@ref) and thrown.

See also [`fetch`](@ref) and [`remotecall`](@ref).
"""
remotecall_fetch(f, id::Integer, args...; kwargs...) =
    remotecall_fetch(f, worker_from_id(id), args...; kwargs...)

remotecall_wait(f, w::LocalProcess, args...; kwargs...) = wait(remotecall(f, w, args...; kwargs...))

function remotecall_wait(f, w::Worker, args...; kwargs...)
    prid = RRID()
    rv = lookup_ref(prid)
    rv.waitingfor = w.id
    rr = Future(w)
    send_msg(w, MsgHeader(remoteref_id(rr), prid), CallWaitMsg(f, args, kwargs))
    v = fetch(rv.c)
    lock(client_refs) do
        delete!(PGRP.refs, prid)
    end
    isa(v, RemoteException) && throw(v)
    return rr
end

"""
    remotecall_wait(f, id::Integer, args...; kwargs...)

Perform a faster `wait(remotecall(...))` in one message on the `Worker` specified by worker id `id`.
Keyword arguments, if any, are passed through to `f`.

See also [`wait`](@ref) and [`remotecall`](@ref).
"""
remotecall_wait(f, id::Integer, args...; kwargs...) =
    remotecall_wait(f, worker_from_id(id), args...; kwargs...)

function remote_do(f, w::LocalProcess, args...; kwargs...)
    # the LocalProcess version just performs in local memory what a worker
    # does when it gets a :do message.
    # same for other messages on LocalProcess.
    thk = local_remotecall_thunk(f, args, kwargs)
    schedule(Task(thk))
    nothing
end

function remote_do(f, w::Worker, args...; kwargs...)
    send_msg(w, MsgHeader(), RemoteDoMsg(f, args, kwargs))
    nothing
end


"""
    remote_do(f, id::Integer, args...; kwargs...) -> nothing

Executes `f` on worker `id` asynchronously.
Unlike [`remotecall`](@ref), it does not store the
result of computation, nor is there a way to wait for its completion.

A successful invocation indicates that the request has been accepted for execution on
the remote node.

While consecutive `remotecall`s to the same worker are serialized in the order they are
invoked, the order of executions on the remote worker is undetermined. For example,
`remote_do(f1, 2); remotecall(f2, 2); remote_do(f3, 2)` will serialize the call
to `f1`, followed by `f2` and `f3` in that order. However, it is not guaranteed that `f1`
is executed before `f3` on worker 2.

Any exceptions thrown by `f` are printed to [`stderr`](@ref) on the remote worker.

Keyword arguments, if any, are passed through to `f`.
"""
remote_do(f, id::Integer, args...; kwargs...) = remote_do(f, worker_from_id(id), args...; kwargs...)

# have the owner of rr call f on it
function call_on_owner(f, rr::AbstractRemoteRef, args...)
    rid = remoteref_id(rr)
    if rr.where == myid()
        f(rid, args...)
    else
        remotecall_fetch(f, rr.where, rid, args...)
    end
end

function wait_ref(rid, callee, args...)
    v = fetch_ref(rid, args...)
    if isa(v, RemoteException)
        if myid() == callee
            throw(v)
        else
            return v
        end
    end
    nothing
end

"""
    wait(r::Future)

Wait for a value to become available for the specified future.
"""
wait(r::Future) = (r.v !== nothing && return r; call_on_owner(wait_ref, r, myid()); r)

"""
    wait(r::RemoteChannel, args...)

Wait for a value to become available on the specified remote channel.
"""
wait(r::RemoteChannel, args...) = (call_on_owner(wait_ref, r, myid(), args...); r)

function fetch(r::Future)
    r.v !== nothing && return coalesce(r.v)
    v = call_on_owner(fetch_ref, r)
    r.v = Some(v)
    send_del_client(r)
    v
end

fetch_ref(rid, args...) = fetch(lookup_ref(rid).c, args...)
fetch(r::RemoteChannel, args...) = call_on_owner(fetch_ref, r, args...)

"""
    fetch(x)

Waits and fetches a value from `x` depending on the type of `x`:

* [`Future`](@ref): Wait for and get the value of a `Future`. The fetched value is cached locally.
  Further calls to `fetch` on the same reference return the cached value. If the remote value
  is an exception, throws a [`RemoteException`](@ref) which captures the remote exception and backtrace.
* [`RemoteChannel`](@ref): Wait for and get the value of a remote reference. Exceptions raised are
  same as for a `Future` .

Does not remove the item fetched.
"""
fetch(@nospecialize x) = x

isready(rv::RemoteValue, args...) = isready(rv.c, args...)

"""
    put!(rr::Future, v)

Store a value to a [`Future`](@ref) `rr`.
`Future`s are write-once remote references.
A `put!` on an already set `Future` throws an `Exception`.
All asynchronous remote calls return `Future`s and set the
value to the return value of the call upon completion.
"""
function put!(rr::Future, v)
    rr.v !== nothing && error("Future can be set only once")
    call_on_owner(put_future, rr, v, myid())
    rr.v = Some(v)
    rr
end
function put_future(rid, v, callee)
    rv = lookup_ref(rid)
    isready(rv) && error("Future can be set only once")
    put!(rv, v)
    # The callee has the value and hence can be removed from the remote store.
    del_client(rid, callee)
    nothing
end


put!(rv::RemoteValue, args...) = put!(rv.c, args...)
put_ref(rid, args...) = (put!(lookup_ref(rid), args...); nothing)

"""
    put!(rr::RemoteChannel, args...)

Store a set of values to the [`RemoteChannel`](@ref).
If the channel is full, blocks until space is available.
Return the first argument.
"""
put!(rr::RemoteChannel, args...) = (call_on_owner(put_ref, rr, args...); rr)

# take! is not supported on Future

take!(rv::RemoteValue, args...) = take!(rv.c, args...)
function take_ref(rid, callee, args...)
    v=take!(lookup_ref(rid), args...)
    isa(v, RemoteException) && (myid() == callee) && throw(v)
    v
end

"""
    take!(rr::RemoteChannel, args...)

Fetch value(s) from a [`RemoteChannel`](@ref) `rr`,
removing the value(s) in the process.
"""
take!(rr::RemoteChannel, args...) = call_on_owner(take_ref, rr, myid(), args...)

# close is not supported on Future

close_ref(rid) = (close(lookup_ref(rid).c); nothing)
close(rr::RemoteChannel) = call_on_owner(close_ref, rr)

getindex(r::RemoteChannel) = fetch(r)
getindex(r::Future) = fetch(r)

getindex(r::Future, args...) = getindex(fetch(r), args...)
function getindex(r::RemoteChannel, args...)
    if r.where == myid()
        return getindex(fetch(r), args...)
    end
    return remotecall_fetch(getindex, r.where, r, args...)
end
