# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Serializer: reset_state


# todo:
# * fetch/wait latency seems to be excessive
# * message aggregation
# * timer events
# - send pings at some interval to detect failed/hung machines
# * integrate event loop with other kinds of i/o (non-messages)
# * serializing closures
# * recover from i/o errors
# * handle remote execution errors
# * all-to-all communication
# * distributed GC
# * call&wait and call&fetch combined messages
# * aggregate GC messages
# * dynamically adding nodes (then always start with 1 and grow)

## workers and message i/o ##
# Messages
abstract AbstractMsg

let REF_ID::Int = 1
    global next_ref_id
    next_ref_id() = (id = REF_ID; REF_ID += 1; id)
end

immutable RRID
    whence::Int
    id::Int

    RRID() = RRID(myid(),next_ref_id())
    RRID(whence, id) = new(whence,id)
end
hash(r::RRID, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::RRID, s::RRID) = (r.whence==s.whence && r.id==s.id)

## Wire format description
#
# Each message has three parts, which are written in order to the worker's stream.
#  1) A header of type MsgHeader is serialized to the stream (via `serialize`).
#  2) A message of type AbstractMsg is then serialized.
#  3) Finally, a fixed bounday of 10 bytes is written.

# Message header stored separately from body to be able to send back errors if
# a deserialization error occurs when reading the message body.
immutable MsgHeader
    response_oid::RRID
    notify_oid::RRID
    MsgHeader(respond_oid=RRID(0,0), notify_oid=RRID(0,0)) =
        new(respond_oid, notify_oid)
end

# Special oid (0,0) uses to indicate a null ID.
# Used instead of Nullable to decrease wire size of header.
null_id(id) =  id == RRID(0, 0)

immutable CallMsg{Mode} <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable CallWaitMsg <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable RemoteDoMsg <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable ResultMsg <: AbstractMsg
    value::Any
end


# Worker initialization messages
immutable IdentifySocketMsg <: AbstractMsg
    from_pid::Int
end

immutable IdentifySocketAckMsg <: AbstractMsg
end

immutable JoinPGRPMsg <: AbstractMsg
    self_pid::Int
    other_workers::Array
    topology::Symbol
    enable_threaded_blas::Bool
end
immutable JoinCompleteMsg <: AbstractMsg
    cpu_cores::Int
    ospid::Int
end

# Avoiding serializing AbstractMsg containers results in a speedup
# of approximately 10%. Can be removed once module Serializer
# has been suitably improved.

# replace  CallMsg{Mode} with specific invocations
const msgtypes = filter!(x->x!=CallMsg, subtypes(AbstractMsg))
push!(msgtypes, CallMsg{:call}, CallMsg{:call_fetch})

for (idx, tname) in enumerate(msgtypes)
    nflds = length(fieldnames(tname))
    @eval begin
        function serialize(s::AbstractSerializer, o::$tname)
            write(s.io, UInt8($idx))
            for fld in fieldnames($tname)
                serialize(s, getfield(o, fld))
            end
        end

        function deserialize_msg(s::AbstractSerializer, ::Type{$tname})
            data=Array{Any,1}($nflds)
            for i in 1:$nflds
                data[i] = deserialize(s)
            end
            return $tname(data...)
        end
    end
end

function deserialize_msg(s::AbstractSerializer)
    idx = read(s.io, UInt8)
    t = msgtypes[idx]
    return eval(current_module(), Expr(:body, Expr(:return, Expr(:call, deserialize_msg, QuoteNode(s), QuoteNode(t)))))
end

function send_msg_unknown(s::IO, header, msg)
    error("attempt to send to unknown socket")
end

function send_msg(s::IO, header, msg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg(worker_from_id(id), header, msg)
    end
    send_msg_unknown(s, header, msg)
end

function send_msg_now(s::IO, header, msg::AbstractMsg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg_now(worker_from_id(id), header, msg)
    end
    send_msg_unknown(s, header, msg)
end

abstract ClusterManager

type WorkerConfig
    # Common fields relevant to all cluster managers
    io::Nullable{IO}
    host::Nullable{AbstractString}
    port::Nullable{Integer}

    # Used when launching additional workers at a host
    count::Nullable{Union{Int, Symbol}}
    exename::Nullable{AbstractString}
    exeflags::Nullable{Cmd}

    # External cluster managers can use this to store information at a per-worker level
    # Can be a dict if multiple fields need to be stored.
    userdata::Nullable{Any}

    # SSHManager / SSH tunnel connections to workers
    tunnel::Nullable{Bool}
    bind_addr::Nullable{AbstractString}
    sshflags::Nullable{Cmd}
    max_parallel::Nullable{Integer}

    # Used by Local/SSH managers
    connect_at::Nullable{Any}

    process::Nullable{Process}
    ospid::Nullable{Integer}

    # Private dictionary used to store temporary information by Local/SSH managers.
    environ::Nullable{Dict}

    # Connections to be setup depending on the network topology requested
    ident::Nullable{Any}      # Worker as identified by the Cluster Manager.
    # List of other worker idents this worker must connect with. Used with topology T_CUSTOM.
    connect_idents::Nullable{Array}

    # Run multithreaded blas on worker
    enable_threaded_blas::Nullable{Bool}

    function WorkerConfig()
        wc = new()
        for n in 1:length(WorkerConfig.types)
            T = eltype(fieldtype(WorkerConfig, n))
            setfield!(wc, n, Nullable{T}())
        end
        wc
    end
end

@enum WorkerState W_CREATED W_CONNECTED W_TERMINATING W_TERMINATED
type Worker
    id::Int
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    gcflag::Bool
    state::WorkerState
    c_state::Condition      # wait for state changes
    ct_time::Float64        # creation time

    r_stream::IO
    w_stream::IO
    w_serializer::ClusterSerializer  # writes can happen from any task hence store the
                                     # serializer as part of the Worker object
    manager::ClusterManager
    config::WorkerConfig
    version::Nullable{VersionNumber}  # Julia version of the remote process

    function Worker(id::Int, r_stream::IO, w_stream::IO, manager::ClusterManager;
                             version=Nullable{VersionNumber}(), config=WorkerConfig())
        w = Worker(id)
        w.r_stream = r_stream
        w.w_stream = buffer_writes(w_stream)
        w.w_serializer = ClusterSerializer(w.w_stream)
        w.manager = manager
        w.config = config
        w.version = version
        set_worker_state(w, W_CONNECTED)
        register_worker_streams(w)
        w
    end

    function Worker(id::Int)
        @assert id > 0
        if haskey(map_pid_wrkr, id)
            return map_pid_wrkr[id]
        end
        w=new(id, [], [], false, W_CREATED, Condition(), time())
        register_worker(w)
        w
    end

    Worker() = Worker(get_next_pid())
end

function set_worker_state(w, state)
    w.state = state
    notify(w.c_state; all=true)
end

function send_msg_now(w::Worker, header, msg)
    send_msg_(w, header, msg, true)
end

function send_msg(w::Worker, header, msg)
    send_msg_(w, header, msg, false)
end

function flush_gc_msgs(w::Worker)
    if !isdefined(w, :w_stream)
        return
    end
    w.gcflag = false
    new_array = Any[]
    msgs = w.add_msgs
    w.add_msgs = new_array
    if !isempty(msgs)
        remote_do(add_clients, w, msgs)
    end

    # del_msgs gets populated by finalizers, so be very careful here about ordering of allocations
    new_array = Any[]
    msgs = w.del_msgs
    w.del_msgs = new_array
    if !isempty(msgs)
        #print("sending delete of $msgs\n")
        remote_do(del_clients, w, msgs)
    end
end

function check_worker_state(w::Worker)
    if w.state == W_CREATED
        if PGRP.topology == :all_to_all
            # Since higher pids connect with lower pids, the remote worker
            # may not have connected to us yet. Wait for some time.
            timeout =  worker_timeout() - (time() - w.ct_time)
            timeout <= 0 && error("peer $(w.id) has not connected to $(myid())")

            @schedule (sleep(timeout); notify(w.c_state; all=true))
            wait(w.c_state)
            w.state == W_CREATED && error("peer $(w.id) didn't connect to $(myid()) within $timeout seconds")
        else
            error("peer $(w.id) is not connected to $(myid()). Topology : " * string(PGRP.topology))
        end
    end
end

# Boundary inserted between messages on the wire, used for recovering
# from deserialization errors. Picked arbitrarily.
# A size of 10 bytes indicates ~ ~1e24 possible boundaries, so chance of collision
# with message contents is negligible.
const MSG_BOUNDARY = UInt8[0x79, 0x8e, 0x8e, 0xf5, 0x6e, 0x9b, 0x2e, 0x97, 0xd5, 0x7d]

# Faster serialization/deserialization of MsgHeader and RRID
function serialize_hdr_raw(io, hdr)
    write(io, hdr.response_oid.whence, hdr.response_oid.id, hdr.notify_oid.whence, hdr.notify_oid.id)
end

function deserialize_hdr_raw(io)
    data = Array{Int,1}(4)
    read!(io, data)
    return MsgHeader(RRID(data[1], data[2]), RRID(data[3], data[4]))
end

function send_msg_(w::Worker, header, msg, now::Bool)
    check_worker_state(w)
    io = w.w_stream
    lock(io.lock)
    try
        reset_state(w.w_serializer)
        serialize_hdr_raw(io, header)
        eval(current_module(), Expr(:body, Expr(:return, Expr(:call, serialize, QuoteNode(w.w_serializer), QuoteNode(msg)))))  # io is wrapped in w_serializer
        write(io, MSG_BOUNDARY)

        if !now && w.gcflag
            flush_gc_msgs(w)
        else
            flush(io)
        end
    finally
        unlock(io.lock)
    end
end

function flush_gc_msgs()
    try
        for w in (PGRP::ProcessGroup).workers
            if isa(w,Worker) && w.gcflag && (w.state == W_CONNECTED)
                flush_gc_msgs(w)
            end
        end
    catch e
        bt = catch_backtrace()
        @schedule showerror(STDERR, e, bt)
    end
end

function send_connection_hdr(w::Worker, cookie=true)
    # For a connection initiated from the remote side to us, we only send the version,
    # else when we initiate a connection we first send the cookie followed by our version.
    # The remote side validates the cookie.
    if cookie
        write(w.w_stream, LPROC.cookie)
    end
    write(w.w_stream, rpad(VERSION_STRING, HDR_VERSION_LEN)[1:HDR_VERSION_LEN])
end

## process group creation ##

type LocalProcess
    id::Int
    bind_addr::AbstractString
    bind_port::UInt16
    cookie::AbstractString
    LocalProcess() = new(1)
end

const LPROC = LocalProcess()

const HDR_VERSION_LEN=16
const HDR_COOKIE_LEN=16

"""
    Base.cluster_cookie() -> cookie

Returns the cluster cookie.
"""
cluster_cookie() = LPROC.cookie

"""
    Base.cluster_cookie(cookie) -> cookie

Sets the passed cookie as the cluster cookie, then returns it.
"""
function cluster_cookie(cookie)
    # The cookie must be an ASCII string with length <=  HDR_COOKIE_LEN
    assert(isascii(cookie))
    assert(length(cookie) <= HDR_COOKIE_LEN)

    cookie = rpad(cookie, HDR_COOKIE_LEN)

    LPROC.cookie = cookie
    cookie
end

const map_pid_wrkr = Dict{Int, Union{Worker, LocalProcess}}()
const map_sock_wrkr = ObjectIdDict()
const map_del_wrkr = Set{Int}()

let next_pid = 2    # 1 is reserved for the client (always)
    global get_next_pid
    function get_next_pid()
        retval = next_pid
        next_pid += 1
        retval
    end
end

type ProcessGroup
    name::AbstractString
    workers::Array{Any,1}
    refs::Dict                  # global references
    topology::Symbol

    ProcessGroup(w::Array{Any,1}) = new("pg-default", w, Dict(), :all_to_all)
end
const PGRP = ProcessGroup([])

function topology(t)
    assert(t in [:all_to_all, :master_slave, :custom])
    if (PGRP.topology==t) || ((myid()==1) && (nprocs()==1)) || (myid() > 1)
        PGRP.topology = t
    else
        error("Workers with Topology $(PGRP.topology) already exist. Requested Topology $(t) cannot be set.")
    end
    t
end

get_bind_addr(pid::Integer) = get_bind_addr(worker_from_id(pid))
get_bind_addr(w::LocalProcess) = LPROC.bind_addr
function get_bind_addr(w::Worker)
    if isnull(w.config.bind_addr)
        if w.id != myid()
            w.config.bind_addr = remotecall_fetch(get_bind_addr, w.id, w.id)
        end
    end
    get(w.config.bind_addr)
end

"""
    myid()

Get the id of the current process.
"""
myid() = LPROC.id

"""
    nprocs()

Get the number of available processes.
"""
nprocs() = length(PGRP.workers)

"""
    nworkers()

Get the number of available worker processes. This is one less than `nprocs()`. Equal to
`nprocs()` if `nprocs() == 1`.
"""
function nworkers()
    n = nprocs()
    n == 1 ? 1 : n-1
end

"""
    procs()

Returns a list of all process identifiers.
"""
procs() = Int[x.id for x in PGRP.workers]

"""
    procs(pid::Integer)

Returns a list of all process identifiers on the same physical node.
Specifically all workers bound to the same ip-address as `pid` are returned.
"""
function procs(pid::Integer)
    if myid() == 1
        if (pid == 1) || (isa(map_pid_wrkr[pid].manager, LocalManager))
            Int[x.id for x in filter(w -> (w.id==1) || (isa(w.manager, LocalManager)), PGRP.workers)]
        else
            ipatpid = get_bind_addr(pid)
            Int[x.id for x in filter(w -> get_bind_addr(w) == ipatpid, PGRP.workers)]
        end
    else
        remotecall_fetch(procs, 1, pid)
    end
end

"""
    workers()

Returns a list of all worker process identifiers.
"""
function workers()
    allp = procs()
    if nprocs() == 1
       allp
    else
       filter(x -> x != 1, allp)
    end
end

"""
    rmprocs(pids...; waitfor=typemax(Int))

Removes the specified workers. Note that only process 1 can add or remove
workers.

Argument `waitfor` specifies how long to wait for the workers to shut down:
    - If unspecified, `rmprocs` will wait until all requested `pids` are removed.
    - An `ErrorException` is raised if all workers cannot be terminated before
      the requested `waitfor` seconds.
    - With a `waitfor` value of 0, the call returns immediately with the workers
      scheduled for removal in a different task. The scheduled `Task` object is
      returned. The user should call `wait` on the task before invoking any other
      parallel calls.
"""
function rmprocs(pids...; waitfor=typemax(Int))
    # Only pid 1 can add and remove processes
    if myid() != 1
        throw(ErrorException("only process 1 can add and remove processes"))
    end

    pids = vcat(pids...)
    if waitfor == 0
        t = @schedule _rmprocs(pids, typemax(Int))
        yield()
        return t
    else
        _rmprocs(pids, waitfor)
        # return a dummy task object that user code can wait on.
        return @schedule nothing
    end
end

function _rmprocs(pids, waitfor)
    lock(worker_lock)
    try
        rmprocset = []
        for p in vcat(pids...)
            if p == 1
                warn("rmprocs: process 1 not removed")
            else
                if haskey(map_pid_wrkr, p)
                    w = map_pid_wrkr[p]
                    set_worker_state(w, W_TERMINATING)
                    kill(w.manager, p, w.config)
                    push!(rmprocset, w)
                end
            end
        end

        start = time()
        while (time() - start) < waitfor
            all(w -> w.state == W_TERMINATED, rmprocset) && break
            sleep(min(0.1, waitfor - (time() - start)))
        end

        unremoved = [wrkr.id for wrkr in filter(w -> w.state != W_TERMINATED, rmprocset)]
        if length(unremoved) > 0
            estr = string("rmprocs: pids ", unremoved, " not terminated after ", waitfor, " seconds.")
            throw(ErrorException(estr))
        end
    finally
        unlock(worker_lock)
    end
end


"""
    ProcessExitedException()

After a client Julia process has exited, further attempts to reference the dead child will
throw this exception.
"""
ProcessExitedException()
type ProcessExitedException <: Exception end

worker_from_id(i) = worker_from_id(PGRP, i)
function worker_from_id(pg::ProcessGroup, i)
    if in(i, map_del_wrkr)
        throw(ProcessExitedException())
    end
    if !haskey(map_pid_wrkr,i)
        if myid() == 1
            error("no process with id $i exists")
        end
        w = Worker(i)
        map_pid_wrkr[i] = w
    else
        w = map_pid_wrkr[i]
    end
    w
end

"""
    Base.worker_id_from_socket(s) -> pid

A low-level API which given a `IO` connection or a `Worker`,
returns the `pid` of the worker it is connected to.
This is useful when writing custom [`serialize`](@ref) methods for a type,
which optimizes the data written out depending on the receiving process id.
"""
function worker_id_from_socket(s)
    w = get(map_sock_wrkr, s, nothing)
    if isa(w,Worker)
        if s === w.r_stream || s === w.w_stream
            return w.id
        end
    end
    if isa(s,IOStream) && fd(s)==-1
        # serializing to a local buffer
        return myid()
    end
    return -1
end

register_worker(w) = register_worker(PGRP, w)
function register_worker(pg, w)
    push!(pg.workers, w)
    map_pid_wrkr[w.id] = w
end

function register_worker_streams(w)
    map_sock_wrkr[w.r_stream] = w
    map_sock_wrkr[w.w_stream] = w
end

deregister_worker(pid) = deregister_worker(PGRP, pid)
function deregister_worker(pg, pid)
    pg.workers = filter(x -> !(x.id == pid), pg.workers)
    w = pop!(map_pid_wrkr, pid, nothing)
    if isa(w, Worker)
        if isdefined(w, :r_stream)
            pop!(map_sock_wrkr, w.r_stream, nothing)
            if w.r_stream != w.w_stream
                pop!(map_sock_wrkr, w.w_stream, nothing)
            end
        end

        if myid() == 1
            # Notify the cluster manager of this workers death
            manage(w.manager, w.id, w.config, :deregister)
            if PGRP.topology != :all_to_all
                for rpid in workers()
                    try
                        remote_do(deregister_worker, rpid, pid)
                    catch
                    end
                end
            end
        end
    end
    push!(map_del_wrkr, pid)

    # delete this worker from our remote reference client sets
    ids = []
    tonotify = []
    lock(client_refs) do
        for (id,rv) in pg.refs
            if in(pid,rv.clientset)
                push!(ids, id)
            end
            if rv.waitingfor == pid
                push!(tonotify, (id,rv))
            end
        end
        for id in ids
            del_client(pg, id, pid)
        end

        # throw exception to tasks waiting for this pid
        for (id,rv) in tonotify
            notify_error(rv.c, ProcessExitedException())
            delete!(pg.refs, id)
        end
    end
end

## remote refs ##

"""
    client_refs

Tracks whether a particular `AbstractRemoteRef`
(identified by its RRID) exists on this worker.

The `client_refs` lock is also used to synchronize access to `.refs` and associated `clientset` state.
"""
const client_refs = WeakKeyDict{Any, Void}() # used as a WeakKeySet

abstract AbstractRemoteRef

type Future <: AbstractRemoteRef
    where::Int
    whence::Int
    id::Int
    v::Nullable{Any}

    Future(w::Int, rrid::RRID) = Future(w, rrid, Nullable{Any}())
    Future(w::Int, rrid::RRID, v) = (r = new(w,rrid.whence,rrid.id,v); return test_existing_ref(r))
end

type RemoteChannel{T<:AbstractChannel} <: AbstractRemoteRef
    where::Int
    whence::Int
    id::Int

    RemoteChannel(w::Int, rrid::RRID) = (r = new(w, rrid.whence, rrid.id); return test_existing_ref(r))
end

function test_existing_ref(r::AbstractRemoteRef)
    found = getkey(client_refs, r, nothing)
    if found !== nothing
        @assert r.where > 0
        if isa(r, Future) && isnull(found.v) && !isnull(r.v)
            # we have recd the value from another source, probably a deserialized ref, send a del_client message
            send_del_client(r)
            found.v = r.v
        end
        return found::typeof(r)
    end

    client_refs[r] = nothing
    finalizer(r, finalize_ref)
    return r
end

function finalize_ref(r::AbstractRemoteRef)
    if r.where > 0 # Handle the case of the finalizer having been called manually
        islocked(client_refs) && return finalizer(r, finalize_ref) # delay finalizer for later, when it's not already locked
        delete!(client_refs, r)
        if isa(r, RemoteChannel)
            send_del_client(r)
        else
            # send_del_client only if the reference has not been set
            isnull(r.v) && send_del_client(r)
            r.v = Nullable{Any}()
        end
        r.where = 0
    end
    nothing
end

Future(w::LocalProcess) = Future(w.id)
Future(w::Worker) = Future(w.id)

"""
    Future(pid::Integer=myid())

Create a `Future` on process `pid`.
The default `pid` is the current process.
"""
Future(pid::Integer=myid()) = Future(pid, RRID())

"""
    RemoteChannel(pid::Integer=myid())

Make a reference to a `Channel{Any}(1)` on process `pid`.
The default `pid` is the current process.
"""
RemoteChannel(pid::Integer=myid()) = RemoteChannel{Channel{Any}}(pid, RRID())

"""
    RemoteChannel(f::Function, pid::Integer=myid())

Create references to remote channels of a specific size and type. `f()` is a function that
when executed on `pid` must return an implementation of an `AbstractChannel`.

For example, `RemoteChannel(()->Channel{Int}(10), pid)`, will return a reference to a
channel of type `Int` and size 10 on `pid`.

The default `pid` is the current process.
"""
function RemoteChannel(f::Function, pid::Integer=myid())
    remotecall_fetch(pid, f, RRID()) do f, rrid
        rv=lookup_ref(rrid, f)
        RemoteChannel{typeof(rv.c)}(myid(), rrid)
    end
end

hash(r::AbstractRemoteRef, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::AbstractRemoteRef, s::AbstractRemoteRef) = (r.whence==s.whence && r.id==s.id)

"""
    Base.remoteref_id(r::AbstractRemoteRef) -> RRID

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

`Base.remoteref_id` is a low-level API which returns a `Base.RRID`
object that wraps `whence` and `id` values of a remote reference.
"""
remoteref_id(r::AbstractRemoteRef) = RRID(r.whence, r.id)

"""
    Base.channel_from_id(id) -> c

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
            rv = RemoteValue(eval(Main, Expr(:body, Expr(:return, Expr(:call, f)))))
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
    !isnull(rr.v) && return true

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
    elseif rr.where in procs() # process only if a valid worker
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
    elseif (i != rr.where) && (rr.where in procs())
        # don't need to send add_client if the message is already going
        # to the processor that owns the remote ref. it will add_client
        # itself inside deserialize().
        w = worker_from_id(rr.where)
        push!(w.add_msgs, (remoteref_id(rr), i))
        w.gcflag = true
        notify(any_gc_flag)
    end
end

channel_type{T}(rr::RemoteChannel{T}) = T

serialize(s::AbstractSerializer, f::Future) = serialize(s, f, isnull(f.v))
serialize(s::AbstractSerializer, rr::RemoteChannel) = serialize(s, rr, true)
function serialize(s::AbstractSerializer, rr::AbstractRemoteRef, addclient)
    if addclient
        p = worker_id_from_socket(s.io)
        (p !== rr.where) && send_add_client(rr, p)
    end
    invoke(serialize, Tuple{AbstractSerializer, Any}, s, rr)
end

function deserialize{T<:Future}(s::AbstractSerializer, t::Type{T})
    f = deserialize_rr(s,t)
    Future(f.where, RRID(f.whence, f.id), f.v) # ctor adds to client_refs table
end

function deserialize{T<:RemoteChannel}(s::AbstractSerializer, t::Type{T})
    rr = deserialize_rr(s,t)
    # call ctor to make sure this rr gets added to the client_refs table
    RemoteChannel{channel_type(rr)}(rr.where, RRID(rr.whence, rr.id))
end

function deserialize_rr(s, t)
    rr = invoke(deserialize, Tuple{AbstractSerializer, DataType}, s, t)
    if rr.where == myid()
        # send_add_client() is not executed when the ref is being
        # serialized to where it exists
        add_client(remoteref_id(rr), myid())
    end
    rr
end

# data stored by the owner of a remote reference
def_rv_channel() = Channel(1)
type RemoteValue
    c::AbstractChannel
    clientset::IntSet # Set of workerids that have a reference to this channel.
                      # Keeping ids instead of a count aids in cleaning up upon
                      # a worker exit.

    waitingfor::Int   # processor we need to hear from to fill this, or 0

    RemoteValue(c) = new(c, IntSet(), 0)
end

wait(rv::RemoteValue) = wait(rv.c)

## core messages: do, call, fetch, wait, ref, put! ##
type RemoteException <: Exception
    pid::Int
    captured::CapturedException
end

"""
    RemoteException(captured)

Exceptions on remote computations are captured and rethrown locally.  A `RemoteException`
wraps the `pid` of the worker and a captured exception. A `CapturedException` captures the
remote exception and a serializable form of the call stack when the exception was raised.
"""
RemoteException(captured) = RemoteException(myid(), captured)
function showerror(io::IO, re::RemoteException)
    (re.pid != myid()) && print(io, "On worker ", re.pid, ":\n")
    showerror(io, re.captured)
end

function run_work_thunk(thunk, print_error)
    local result
    try
        result = thunk()
    catch err
        ce = CapturedException(err, catch_backtrace())
        result = RemoteException(ce)
        print_error && showerror(STDERR, ce)
    end
    return result
end
function run_work_thunk(rv::RemoteValue, thunk)
    put!(rv, run_work_thunk(thunk, false))
    nothing
end

function schedule_call(rid, thunk)
    return lock(client_refs) do
        rv = RemoteValue(def_rv_channel())
        (PGRP::ProcessGroup).refs[rid] = rv
        push!(rv.clientset, rid.whence)
        @schedule run_work_thunk(rv, thunk)
        return rv
    end
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
Returns a [`Future`](@ref).
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

Any exceptions thrown by `f` are printed to [`STDERR`](@ref) on the remote worker.

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
wait(r::Future) = (!isnull(r.v) && return r; call_on_owner(wait_ref, r, myid()); r)
wait(r::RemoteChannel, args...) = (call_on_owner(wait_ref, r, myid(), args...); r)

function fetch_future(rid, callee)
    rv = lookup_ref(rid)
    v = fetch(rv.c)
    del_client(rid, callee)
    v
end
function fetch(r::Future)
    !isnull(r.v) && return get(r.v)
    v=call_on_owner(fetch_future, r, myid())
    r.v=v
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
fetch(x::ANY) = x

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
    !isnull(rr.v) && error("Future can be set only once")
    call_on_owner(put_future, rr, v, myid())
    rr.v = v
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
Returns its first argument.
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
removing the value(s) in the processs.
"""
take!(rr::RemoteChannel, args...) = call_on_owner(take_ref, rr, myid(), args...)

# close is not supported on Future

close_ref(rid) = (close(lookup_ref(rid).c); nothing)
close(rr::RemoteChannel) = call_on_owner(close_ref, rr)


function deliver_result(sock::IO, msg, oid, value)
    #print("$(myid()) sending result $oid\n")
    if msg === :call_fetch || isa(value, RemoteException)
        val = value
    else
        val = :OK
    end
    try
        send_msg_now(sock, MsgHeader(oid), ResultMsg(val))
    catch e
        # terminate connection in case of serialization error
        # otherwise the reading end would hang
        print(STDERR, "fatal error on ", myid(), ": ")
        display_error(e, catch_backtrace())
        wid = worker_id_from_socket(sock)
        close(sock)
        if myid()==1
            rmprocs(wid)
        elseif wid == 1
            exit(1)
        else
            remote_do(rmprocs, 1, wid)
        end
    end
end

## message event handlers ##
function process_messages(r_stream::TCPSocket, w_stream::TCPSocket, incoming::Bool=true)
    @schedule process_tcp_streams(r_stream, w_stream, incoming)
end

function process_tcp_streams(r_stream::TCPSocket, w_stream::TCPSocket, incoming::Bool)
    disable_nagle(r_stream)
    wait_connected(r_stream)
    if r_stream != w_stream
        disable_nagle(w_stream)
        wait_connected(w_stream)
    end
    message_handler_loop(r_stream, w_stream, incoming)
end

"""
    Base.process_messages(r_stream::IO, w_stream::IO, incoming::Bool=true)

Called by cluster managers using custom transports. It should be called when the custom
transport implementation receives the first message from a remote worker. The custom
transport must manage a logical connection to the remote worker and provide two
`IO` objects, one for incoming messages and the other for messages addressed to the
remote worker.
If `incoming` is `true`, the remote peer initiated the connection.
Whichever of the pair initiates the connection sends the cluster cookie and its
Julia version number to perform the authentication handshake.

See also [`cluster_cookie`](@ref).
"""
function process_messages(r_stream::IO, w_stream::IO, incoming::Bool=true)
    @schedule message_handler_loop(r_stream, w_stream, incoming)
end

function message_handler_loop(r_stream::IO, w_stream::IO, incoming::Bool)
    wpid=0          # the worker r_stream is connected to.
    boundary = similar(MSG_BOUNDARY)
    try
        version = process_hdr(r_stream, incoming)
        serializer = ClusterSerializer(r_stream)

        # The first message will associate wpid with r_stream
        header = deserialize_hdr_raw(r_stream)
        msg = deserialize_msg(serializer)
        handle_msg(msg, header, r_stream, w_stream, version)
        wpid = worker_id_from_socket(r_stream)
        @assert wpid > 0

        readbytes!(r_stream, boundary, length(MSG_BOUNDARY))

        while true
            reset_state(serializer)
            header = deserialize_hdr_raw(r_stream)
            # println("header: ", header)

            try
                msg = deserialize_msg(serializer)
            catch e
                # Deserialization error; discard bytes in stream until boundary found
                boundary_idx = 1
                while true
                    # This may throw an EOF error if the terminal boundary was not written
                    # correctly, triggering the higher-scoped catch block below
                    byte = read(r_stream, UInt8)
                    if byte == MSG_BOUNDARY[boundary_idx]
                        boundary_idx += 1
                        if boundary_idx > length(MSG_BOUNDARY)
                            break
                        end
                    else
                        boundary_idx = 1
                    end
                end
                remote_err = RemoteException(myid(), CapturedException(e, catch_backtrace()))
                # println("Deserialization error. ", remote_err)
                if !null_id(header.response_oid)
                    ref = lookup_ref(header.response_oid)
                    put!(ref, remote_err)
                end
                if !null_id(header.notify_oid)
                    deliver_result(w_stream, :call_fetch, header.notify_oid, remote_err)
                end
                continue
            end
            readbytes!(r_stream, boundary, length(MSG_BOUNDARY))

            # println("got msg: ", typeof(msg))
            handle_msg(msg, header, r_stream, w_stream, version)
        end
    catch e
        # Check again as it may have been set in a message handler but not propagated to the calling block above
        wpid = worker_id_from_socket(r_stream)
        if (wpid < 1)
            println(STDERR, e, CapturedException(e, catch_backtrace()))
            println(STDERR, "Process($(myid())) - Unknown remote, closing connection.")
        else
            werr = worker_from_id(wpid)
            oldstate = werr.state
            set_worker_state(werr, W_TERMINATED)

            # If unhandleable error occurred talking to pid 1, exit
            if wpid == 1
                if isopen(w_stream)
                    print(STDERR, "fatal error on ", myid(), ": ")
                    display_error(e, catch_backtrace())
                end
                exit(1)
            end

            # Will treat any exception as death of node and cleanup
            # since currently we do not have a mechanism for workers to reconnect
            # to each other on unhandled errors
            deregister_worker(wpid)
        end

        isopen(r_stream) && close(r_stream)
        isopen(w_stream) && close(w_stream)

        if (myid() == 1) && (wpid > 1)
            if oldstate != W_TERMINATING
                println(STDERR, "Worker $wpid terminated.")
                rethrow(e)
            end
        end

        return nothing
    end
end

function process_hdr(s, validate_cookie)
    if validate_cookie
        cookie = read(s, HDR_COOKIE_LEN)
        if length(cookie) < HDR_COOKIE_LEN
            error("Cookie read failed. Connection closed by peer.")
        end

        self_cookie = cluster_cookie()
        for i in 1:HDR_COOKIE_LEN
            if UInt8(self_cookie[i]) != cookie[i]
                error("Process($(myid())) - Invalid connection credentials sent by remote.")
            end
        end
    end

    # When we have incompatible julia versions trying to connect to each other,
    # and can be detected, raise an appropriate error.
    # For now, just return the version.
    version = read(s, HDR_VERSION_LEN)
    if length(version) < HDR_VERSION_LEN
        error("Version read failed. Connection closed by peer.")
    end

    return VersionNumber(strip(String(version)))
end

function handle_msg(msg::CallMsg{:call}, header, r_stream, w_stream, version)
    schedule_call(header.response_oid, ()->msg.f(msg.args...; msg.kwargs...))
end
function handle_msg(msg::CallMsg{:call_fetch}, header, r_stream, w_stream, version)
    @schedule begin
        v = run_work_thunk(()->msg.f(msg.args...; msg.kwargs...), false)
        deliver_result(w_stream, :call_fetch, header.notify_oid, v)
    end
end

function handle_msg(msg::CallWaitMsg, header, r_stream, w_stream, version)
    @schedule begin
        rv = schedule_call(header.response_oid, ()->msg.f(msg.args...; msg.kwargs...))
        deliver_result(w_stream, :call_wait, header.notify_oid, fetch(rv.c))
    end
end

function handle_msg(msg::RemoteDoMsg, header, r_stream, w_stream, version)
    @schedule run_work_thunk(()->msg.f(msg.args...; msg.kwargs...), true)
end

function handle_msg(msg::ResultMsg, header, r_stream, w_stream, version)
    put!(lookup_ref(header.response_oid), msg.value)
end

function handle_msg(msg::IdentifySocketMsg, header, r_stream, w_stream, version)
    # register a new peer worker connection
    w=Worker(msg.from_pid, r_stream, w_stream, cluster_manager; version=version)
    send_connection_hdr(w, false)
    send_msg_now(w, MsgHeader(), IdentifySocketAckMsg())
end

function handle_msg(msg::IdentifySocketAckMsg, header, r_stream, w_stream, version)
    w = map_sock_wrkr[r_stream]
    w.version = version
end

function handle_msg(msg::JoinPGRPMsg, header, r_stream, w_stream, version)
    LPROC.id = msg.self_pid
    controller = Worker(1, r_stream, w_stream, cluster_manager; version=version)
    register_worker(LPROC)
    topology(msg.topology)

    if !msg.enable_threaded_blas
        disable_threaded_libs()
    end

    wait_tasks = Task[]
    for (connect_at, rpid) in msg.other_workers
        wconfig = WorkerConfig()
        wconfig.connect_at = connect_at

        let rpid=rpid, wconfig=wconfig
            t = @async connect_to_peer(cluster_manager, rpid, wconfig)
            push!(wait_tasks, t)
        end
    end

    for wt in wait_tasks; wait(wt); end

    send_connection_hdr(controller, false)
    send_msg_now(controller, MsgHeader(RRID(0,0), header.notify_oid), JoinCompleteMsg(Sys.CPU_CORES, getpid()))
end

function connect_to_peer(manager::ClusterManager, rpid::Int, wconfig::WorkerConfig)
    try
        (r_s, w_s) = connect(manager, rpid, wconfig)
        w = Worker(rpid, r_s, w_s, manager; config=wconfig)
        process_messages(w.r_stream, w.w_stream, false)
        send_connection_hdr(w, true)
        send_msg_now(w, MsgHeader(), IdentifySocketMsg(myid()))
    catch e
        display_error(e, catch_backtrace())
        println(STDERR, "Error [$e] on $(myid()) while connecting to peer $rpid. Exiting.")
        exit(1)
    end
end

function handle_msg(msg::JoinCompleteMsg, header, r_stream, w_stream, version)
    w = map_sock_wrkr[r_stream]
    environ = get(w.config.environ, Dict())
    environ[:cpu_cores] = msg.cpu_cores
    w.config.environ = environ
    w.config.ospid = msg.ospid
    w.version = version

    ntfy_channel = lookup_ref(header.notify_oid)
    put!(ntfy_channel, w.id)

    push!(default_worker_pool(), w.id)
end

function disable_threaded_libs()
    BLAS.set_num_threads(1)
end

worker_timeout() = parse(Float64, get(ENV, "JULIA_WORKER_TIMEOUT", "60.0"))


## worker creation and setup ##

# The entry point for julia worker processes. does not return. Used for TCP transport.
# Cluster managers implementing their own transport will provide their own.
# Argument is descriptor to write listening port # to.
start_worker(cookie::AbstractString) = start_worker(STDOUT, cookie)
function start_worker(out::IO, cookie::AbstractString)
    # we only explicitly monitor worker STDOUT on the console, so redirect
    # stderr to stdout so we can see the output.
    # at some point we might want some or all worker output to go to log
    # files instead.
    # Currently disabled since this caused processes to spin instead of
    # exit when process 1 shut down. Don't yet know why.
    #redirect_stderr(STDOUT)

    init_worker(cookie)
    interface = IPv4(LPROC.bind_addr)
    if LPROC.bind_port == 0
        (actual_port,sock) = listenany(interface, UInt16(9009))
        LPROC.bind_port = actual_port
    else
        sock = listen(interface, LPROC.bind_port)
    end
    @schedule while isopen(sock)
        client = accept(sock)
        process_messages(client, client, true)
    end
    print(out, "julia_worker:")  # print header
    print(out, "$(dec(LPROC.bind_port))#") # print port
    print(out, LPROC.bind_addr)
    print(out, '\n')
    flush(out)
    # close STDIN; workers will not use it
    #close(STDIN)

    disable_nagle(sock)

    if ccall(:jl_running_on_valgrind,Cint,()) != 0
        println(out, "PID = $(getpid())")
    end

    try
        # To prevent hanging processes on remote machines, newly launched workers exit if the
        # master process does not connect in time.
        # TODO : Make timeout configurable.
        check_master_connect()
        while true; wait(); end
    catch err
        print(STDERR, "unhandled exception on $(myid()): $(err)\nexiting.\n")
    end

    close(sock)
    exit(0)
end


function redirect_worker_output(ident, stream)
    @schedule while !eof(stream)
        line = readline(stream)
        if startswith(line, "\tFrom worker ")
            # STDOUT's of "additional" workers started from an initial worker on a host are not available
            # on the master directly - they are routed via the initial worker's STDOUT.
            print(line)
        else
            print("\tFrom worker $(ident):\t$line")
        end
    end
end


# The default TCP transport relies on the worker listening on a free
# port available and printing its bind address and port.
# The master process uses this to connect to the worker and subsequently
# setup a all-to-all network.
function read_worker_host_port(io::IO)
    while true
        conninfo = readline(io)
        bind_addr, port = parse_connection_info(conninfo)
        if bind_addr != ""
            return bind_addr, port
        end
    end
end

function parse_connection_info(str)
    m = match(r"^julia_worker:(\d+)#(.*)", str)
    if m !== nothing
        (m.captures[2], parse(Int16, m.captures[1]))
    else
        ("", Int16(-1))
    end
end

"""
    init_worker(cookie::AbstractString, manager::ClusterManager=DefaultClusterManager())

Called by cluster managers implementing custom transports. It initializes a newly launched
process as a worker. Command line argument `--worker` has the effect of initializing a
process as a worker using TCP/IP sockets for transport.
`cookie` is a [`cluster_cookie`](@ref).
"""
function init_worker(cookie::AbstractString, manager::ClusterManager=DefaultClusterManager())
    # On workers, the default cluster manager connects via TCP sockets. Custom
    # transports will need to call this function with their own manager.
    global cluster_manager
    cluster_manager = manager

    # Since our pid has yet to be set, ensure no RemoteChannel / Future  have been created or addprocs() called.
    assert(nprocs() <= 1)
    assert(isempty(PGRP.refs))
    assert(isempty(client_refs))

    # System is started in head node mode, cleanup related entries
    empty!(PGRP.workers)
    empty!(map_pid_wrkr)

    cluster_cookie(cookie)
    nothing
end


# The main function for adding worker processes.
# `manager` is of type ClusterManager. The respective managers are responsible
# for launching the workers. All keyword arguments (plus a few default values)
# are available as a dictionary to the `launch` methods
#
# Only one addprocs can be in progress at any time
#
const worker_lock = ReentrantLock()

"""
    addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

Launches worker processes via the specified cluster manager.

For example, Beowulf clusters are supported via a custom cluster manager implemented in
the package `ClusterManagers.jl`.

The number of seconds a newly launched worker waits for connection establishment from the
master can be specified via variable `JULIA_WORKER_TIMEOUT` in the worker process's
environment. Relevant only when using TCP/IP as transport.
"""
function addprocs(manager::ClusterManager; kwargs...)
    lock(worker_lock)
    try
        addprocs_locked(manager::ClusterManager; kwargs...)
    finally
        unlock(worker_lock)
    end
end

function addprocs_locked(manager::ClusterManager; kwargs...)
    params = merge(default_addprocs_params(), AnyDict(kwargs))
    topology(Symbol(params[:topology]))

    # References to launched workers, filled when each worker is fully initialized and
    # has connected to all nodes.
    launched_q = Int[]   # Asynchronously filled by the launch method

    # The `launch` method should add an object of type WorkerConfig for every
    # worker launched. It provides information required on how to connect
    # to it.
    launched = WorkerConfig[]
    launch_ntfy = Condition()

    # call manager's `launch` is a separate task. This allows the master
    # process initiate the connection setup process as and when workers come
    # online
    t_launch = @schedule launch(manager, params, launched, launch_ntfy)

    @sync begin
        while true
            if isempty(launched)
                istaskdone(t_launch) && break
                @schedule (sleep(1); notify(launch_ntfy))
                wait(launch_ntfy)
            end

            if !isempty(launched)
                wconfig = shift!(launched)
                let wconfig=wconfig
                    @async setup_launched_worker(manager, wconfig, launched_q)
                end
            end
        end
    end

    wait(t_launch)      # catches any thrown errors from the launch task

    # Since all worker-to-worker setups may not have completed by the time this
    # function returns to the caller, send the complete list to all workers.
    # Useful for nprocs(), nworkers(), etc to return valid values on the workers.
    all_w = workers()
    for pid in all_w
        remote_do(set_valid_processes, pid, all_w)
    end

    sort!(launched_q)
end

function set_valid_processes(plist::Array{Int})
    for pid in setdiff(plist, workers())
        myid() != pid && Worker(pid)
    end
end

default_addprocs_params() = AnyDict(
    :topology => :all_to_all,
    :dir      => pwd(),
    :exename  => joinpath(JULIA_HOME,julia_exename()),
    :exeflags => ``,
    :enable_threaded_blas => false)


function setup_launched_worker(manager, wconfig, launched_q)
    pid = create_worker(manager, wconfig)
    push!(launched_q, pid)

    # When starting workers on remote multi-core hosts, `launch` can (optionally) start only one
    # process on the remote machine, with a request to start additional workers of the
    # same type. This is done by setting an appropriate value to `WorkerConfig.cnt`.
    cnt = get(wconfig.count, 1)
    if cnt === :auto
        cnt = get(wconfig.environ)[:cpu_cores]
    end
    cnt = cnt - 1   # Removing self from the requested number

    if cnt > 0
        launch_n_additional_processes(manager, pid, wconfig, cnt, launched_q)
    end
end


function launch_n_additional_processes(manager, frompid, fromconfig, cnt, launched_q)
    @sync begin
        exename = get(fromconfig.exename)
        exeflags = get(fromconfig.exeflags, ``)
        cmd = `$exename $exeflags`

        new_addresses = remotecall_fetch(launch_additional, frompid, cnt, cmd)
        for address in new_addresses
            (bind_addr, port) = address

            wconfig = WorkerConfig()
            for x in [:host, :tunnel, :sshflags, :exeflags, :exename, :enable_threaded_blas]
                setfield!(wconfig, x, getfield(fromconfig, x))
            end
            wconfig.bind_addr = bind_addr
            wconfig.port = port

            let wconfig=wconfig
                @async begin
                    pid = create_worker(manager, wconfig)
                    remote_do(redirect_output_from_additional_worker, frompid, pid, port)
                    push!(launched_q, pid)
                end
            end
        end
    end
end

function create_worker(manager, wconfig)
    # only node 1 can add new nodes, since nobody else has the full list of address:port
    assert(LPROC.id == 1)

    # initiate a connect. Does not wait for connection completion in case of TCP.
    w = Worker()

    (r_s, w_s) = connect(manager, w.id, wconfig)
    w = Worker(w.id, r_s, w_s, manager; config=wconfig)
    # install a finalizer to perform cleanup if necessary
    finalizer(w, (w)->if myid() == 1 manage(w.manager, w.id, w.config, :finalize) end)

    # set when the new worker has finshed connections with all other workers
    ntfy_oid = RRID()
    rr_ntfy_join = lookup_ref(ntfy_oid)
    rr_ntfy_join.waitingfor = myid()

    # Start a new task to handle inbound messages from connected worker in master.
    # Also calls `wait_connected` on TCP streams.
    process_messages(w.r_stream, w.w_stream, false)

    # send address information of all workers to the new worker.
    # Cluster managers set the address of each worker in `WorkerConfig.connect_at`.
    # A new worker uses this to setup an all-to-all network if topology :all_to_all is specified.
    # Workers with higher pids connect to workers with lower pids. Except process 1 (master) which
    # initiates connections to all workers.

    # Connection Setup Protocol:
    # - Master sends 16-byte cookie followed by 16-byte version string and a JoinPGRP message to all workers
    # - On each worker
    #   - Worker responds with a 16-byte version followed by a JoinCompleteMsg
    #   - Connects to all workers less than its pid. Sends the cookie, version and an IdentifySocket message
    #   - Workers with incoming connection requests write back their Version and an IdentifySocketAckMsg message
    # - On master, receiving a JoinCompleteMsg triggers rr_ntfy_join (signifies that worker setup is complete)

    join_list = []
    if PGRP.topology == :all_to_all
        # need to wait for lower worker pids to have completed connecting, since the numerical value
        # of pids is relevant to the connection process, i.e., higher pids connect to lower pids and they
        # require the value of config.connect_at which is set only upon connection completion
        for jw in PGRP.workers
            if (jw.id != 1) && (jw.id < w.id)
                (jw.state == W_CREATED) && wait(jw.c_state)
                push!(join_list, jw)
            end
        end

    elseif PGRP.topology == :custom
        # wait for requested workers to be up before connecting to them.
        filterfunc(x) = (x.id != 1) && isdefined(x, :config) && (get(x.config.ident) in get(wconfig.connect_idents, []))

        wlist = filter(filterfunc, PGRP.workers)
        while length(wlist) < length(get(wconfig.connect_idents, []))
            sleep(1.0)
            wlist = filter(filterfunc, PGRP.workers)
        end

        for wl in wlist
            (wl.state == W_CREATED) && wait(wl.c_state)
            push!(join_list, wl)
        end
    end

    all_locs = map(x -> isa(x, Worker) ? (get(x.config.connect_at, ()), x.id) : ((), x.id, true), join_list)
    send_connection_hdr(w, true)
    join_message = JoinPGRPMsg(w.id, all_locs, PGRP.topology, get(wconfig.enable_threaded_blas, false))
    send_msg_now(w, MsgHeader(RRID(0,0), ntfy_oid), join_message)

    @schedule manage(w.manager, w.id, w.config, :register)
    wait(rr_ntfy_join)
    lock(client_refs) do
        delete!(PGRP.refs, ntfy_oid)
    end

    return w.id
end


# Called on the first worker on a remote host. Used to optimize launching
# of multiple workers on a remote host (to leverage multi-core)

additional_io_objs=Dict()
function launch_additional(np::Integer, cmd::Cmd)
    io_objs = Vector{Any}(np)
    addresses = Vector{Any}(np)

    for i in 1:np
        io, pobj = open(pipeline(detach(cmd), stderr=STDERR), "r")
        io_objs[i] = io
    end

    for (i,io) in enumerate(io_objs)
        (host, port) = read_worker_host_port(io)
        addresses[i] = (host, port)
        additional_io_objs[port] = io
    end

    return addresses
end

function redirect_output_from_additional_worker(pid, port)
    io = additional_io_objs[port]
    redirect_worker_output("$pid", io)
    delete!(additional_io_objs, port)
    nothing
end

## higher-level functions: spawn, pmap, pfor, etc. ##

let nextidx = 0
    global chooseproc
    function chooseproc(thunk::Function)
        p = -1
        if p == -1
            p = workers()[(nextidx % nworkers()) + 1]
            nextidx += 1
        end
        p
    end
end

spawnat(p, thunk) = sync_add(remotecall(thunk, p))

spawn_somewhere(thunk) = spawnat(chooseproc(thunk),thunk)

macro spawn(expr)
    expr = localize_vars(esc(:(()->($expr))), false)
    :(spawn_somewhere($expr))
end

macro spawnat(p, expr)
    expr = localize_vars(esc(:(()->($expr))), false)
    :(spawnat($(esc(p)), $expr))
end

"""
    @fetch

Equivalent to `fetch(@spawn expr)`.
See [`fetch`](@ref) and [`@spawn`](@ref).
"""
macro fetch(expr)
    expr = localize_vars(esc(:(()->($expr))), false)
    quote
        thunk = $expr
        remotecall_fetch(thunk, chooseproc(thunk))
    end
end

"""
    @fetchfrom

Equivalent to `fetch(@spawnat p expr)`.
See [`fetch`](@ref) and [`@spawnat`](@ref).
"""
macro fetchfrom(p, expr)
    expr = localize_vars(esc(:(()->($expr))), false)
    :(remotecall_fetch($expr, $(esc(p))))
end

"""
    @everywhere expr

Execute an expression under `Main` everywhere. Equivalent to calling
`eval(Main, expr)` on all processes. Errors on any of the processes are collected into a
`CompositeException` and thrown. For example :

    @everywhere bar=1

will define `Main.bar` on all processes.

Unlike [`@spawn`](@ref) and [`@spawnat`](@ref),
`@everywhere` does not capture any local variables. Prefixing
`@everywhere` with [`@eval`](@ref) allows us to broadcast
local variables using interpolation :

    foo = 1
    @eval @everywhere bar=\$foo

The expression is evaluated under `Main` irrespective of where `@everywhere` is called from.
For example :

    module FooBar
        foo() = @everywhere bar()=myid()
    end
    FooBar.foo()

will result in `Main.bar` being defined on all processes and not `FooBar.bar`.
"""
macro everywhere(ex)
    quote
        sync_begin()
        for pid in workers()
            async_run_thunk(()->remotecall_fetch(eval_ew_expr, pid, $(Expr(:quote,ex))))
            yield() # ensure that the remotecall_fetch has been started
        end

        # execute locally last as we do not want local execution to block serialization
        # of the request to remote nodes.
        if nprocs() > 1
            async_run_thunk(()->eval_ew_expr($(Expr(:quote,ex))))
        end

        sync_end()
    end
end

eval_ew_expr(ex) = (eval(Main, ex); nothing)

# Statically split range [1,N] into equal sized chunks for np processors
function splitrange(N::Int, np::Int)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Array{UnitRange{Int}}(nchunks)
    lo = 1
    for i in 1:nchunks
        hi = lo + each - 1
        if extras > 0
            hi += 1
            extras -= 1
        end
        chunks[i] = lo:hi
        lo = hi+1
    end
    return chunks
end

function preduce(reducer, f, R)
    N = length(R)
    chunks = splitrange(N, nworkers())
    all_w = workers()[1:length(chunks)]

    w_exec = Task[]
    for (idx,pid) in enumerate(all_w)
        t = Task(()->remotecall_fetch(f, pid, reducer, R, first(chunks[idx]), last(chunks[idx])))
        schedule(t)
        push!(w_exec, t)
    end
    reduce(reducer, [wait(t) for t in w_exec])
end

function pfor(f, R)
    [@spawn f(R, first(c), last(c)) for c in splitrange(length(R), nworkers())]
end

function make_preduce_body(var, body)
    quote
        function (reducer, R, lo::Int, hi::Int)
            $(esc(var)) = R[lo]
            ac = $(esc(body))
            if lo != hi
                for $(esc(var)) in R[(lo+1):hi]
                    ac = reducer(ac, $(esc(body)))
                end
            end
            ac
        end
    end
end

function make_pfor_body(var, body)
    quote
        function (R, lo::Int, hi::Int)
            for $(esc(var)) in R[lo:hi]
                $(esc(body))
            end
        end
    end
end

"""
    @parallel

A parallel for loop of the form :

    @parallel [reducer] for var = range
        body
    end

The specified range is partitioned and locally executed across all workers. In case an
optional reducer function is specified, `@parallel` performs local reductions on each worker
with a final reduction on the calling process.

Note that without a reducer function, `@parallel` executes asynchronously, i.e. it spawns
independent tasks on all available workers and returns immediately without waiting for
completion. To wait for completion, prefix the call with [`@sync`](@ref), like :

    @sync @parallel for var = range
        body
    end
"""
macro parallel(args...)
    na = length(args)
    if na==1
        loop = args[1]
    elseif na==2
        reducer = args[1]
        loop = args[2]
    else
        throw(ArgumentError("wrong number of arguments to @parallel"))
    end
    if !isa(loop,Expr) || loop.head !== :for
        error("malformed @parallel loop")
    end
    var = loop.args[1].args[1]
    r = loop.args[1].args[2]
    body = loop.args[2]
    if na==1
        thecall = :(pfor($(make_pfor_body(var, body)), $(esc(r))))
    else
        thecall = :(preduce($(esc(reducer)), $(make_preduce_body(var, body)), $(esc(r))))
    end
    localize_vars(thecall)
end


function check_master_connect()
    timeout = worker_timeout()
    # If we do not have at least process 1 connect to us within timeout
    # we log an error and exit, unless we're running on valgrind
    if ccall(:jl_running_on_valgrind,Cint,()) != 0
        return
    end
    @schedule begin
        start = time()
        while !haskey(map_pid_wrkr, 1) && (time() - start) < timeout
            sleep(1.0)
        end

        if !haskey(map_pid_wrkr, 1)
            print(STDERR, "Master process (id 1) could not connect within $timeout seconds.\nexiting.\n")
            exit(1)
        end
    end
end

"""
    timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

Waits until `testcb` returns `true` or for `secs` seconds, whichever is earlier.
`testcb` is polled every `pollint` seconds.
"""
function timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)
    pollint > 0 || throw(ArgumentError("cannot set pollint to $pollint seconds"))
    start = time()
    done = Channel(1)
    timercb(aw) = begin
        try
            if testcb()
                put!(done, :ok)
            elseif (time() - start) > secs
                put!(done, :timed_out)
            end
        catch e
            put!(done, :error)
        finally
            isready(done) && close(aw)
        end
    end

    if !testcb()
        t = Timer(timercb, pollint, pollint)
        ret = fetch(done)
        close(t)
    else
        ret = :ok
    end
    ret
end

function interrupt(pid::Integer)
    assert(myid() == 1)
    w = map_pid_wrkr[pid]
    if isa(w, Worker)
        manage(w.manager, w.id, w.config, :interrupt)
    end
end

"""
    interrupt(pids::Integer...)

Interrupt the current executing task on the specified workers. This is equivalent to
pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.
"""
interrupt(pids::Integer...) = interrupt([pids...])

"""
    interrupt(pids::AbstractVector=workers())

Interrupt the current executing task on the specified workers. This is equivalent to
pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.
"""
function interrupt(pids::AbstractVector=workers())
    assert(myid() == 1)
    @sync begin
        for pid in pids
            @async interrupt(pid)
        end
    end
end


function disable_nagle(sock)
    # disable nagle on all OSes
    ccall(:uv_tcp_nodelay, Cint, (Ptr{Void}, Cint), sock.handle, 1)
    @static if is_linux()
        # tcp_quickack is a linux only option
        if ccall(:jl_tcp_quickack, Cint, (Ptr{Void}, Cint), sock.handle, 1) < 0
            warn_once("Parallel networking unoptimized ( Error enabling TCP_QUICKACK : ", Libc.strerror(Libc.errno()), " )")
        end
    end
end

function check_same_host(pids)
    if myid() != 1
        return remotecall_fetch(check_same_host, 1, pids)
    else
        # We checkfirst if all test pids have been started using the local manager,
        # else we check for the same bind_to addr. This handles the special case
        # where the local ip address may change - as during a system sleep/awake
        if all(p -> (p==1) || (isa(map_pid_wrkr[p].manager, LocalManager)), pids)
            return true
        else
            first_bind_addr = get(map_pid_wrkr[pids[1]].config.bind_addr)
            return all(p -> (p != 1) && (get(map_pid_wrkr[p].config.bind_addr) == first_bind_addr), pids[2:end])
        end
    end
end

function terminate_all_workers()
    myid() != 1 && return

    if nprocs() > 1
        try
            rmprocs(workers(); waitfor=5.0)
        catch _ex
            warn("Forcibly interrupting busy workers")
            # Might be computation bound, interrupt them and try again
            interrupt(workers())
            try
                rmprocs(workers(); waitfor=5.0)
            catch _ex2
                warn("Unable to terminate all workers")
            end
        end
    end
end

getindex(r::RemoteChannel) = fetch(r)
getindex(r::Future) = fetch(r)

getindex(r::Future, args...) = getindex(fetch(r), args...)
function getindex(r::RemoteChannel, args...)
    if r.where == myid()
        return getindex(fetch(r), args...)
    end
    return remotecall_fetch(getindex, r.where, r, args...)
end
