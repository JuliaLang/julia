# This file is a part of Julia. License is MIT: http://julialang.org/license

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

type CallMsg{Mode} <: AbstractMsg
    f::Function
    args::Tuple
    response_oid::Tuple
end
type CallWaitMsg <: AbstractMsg
    f::Function
    args::Tuple
    response_oid::Tuple
    notify_oid::Tuple
end
type RemoteDoMsg <: AbstractMsg
    f::Function
    args::Tuple
end
type ResultMsg <: AbstractMsg
    response_oid::Tuple
    value::Any
end

# Worker initialization messages
type IdentifySocketMsg <: AbstractMsg
    from_pid::Int
end
type JoinPGRPMsg <: AbstractMsg
    self_pid::Int
    other_workers::Array
    self_is_local::Bool
    notify_oid::Tuple
    topology::Symbol
end
type JoinCompleteMsg <: AbstractMsg
    notify_oid::Tuple
    cpu_cores::Int
    ospid::Int
end


function send_msg_unknown(s::IO, msg)
    error("attempt to send to unknown socket")
end

function send_msg(s::IO, msg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg(worker_from_id(id), msg)
    end
    send_msg_unknown(s, msg)
end

function send_msg_now(s::IO, msg::AbstractMsg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg_now(worker_from_id(id), msg)
    end
    send_msg_unknown(s, msg)
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

    r_stream::AsyncStream
    w_stream::AsyncStream
    manager::ClusterManager
    config::WorkerConfig

    function Worker(id, r_stream, w_stream, manager, config)
        w = Worker(id)
        w.r_stream = r_stream
        w.w_stream = buffer_writes(w_stream)
        w.manager = manager
        w.config = config
        set_worker_state(w, W_CONNECTED)
        register_worker_streams(w)
        w
    end

    function Worker(id)
        if haskey(map_pid_wrkr, id)
            return map_pid_wrkr[id]
        end
        w=new(id, [], [], false, W_CREATED, Condition(), time())
        register_worker(w)
        w
    end

    Worker() = Worker(get_next_pid())
end

Worker(id, r_stream, w_stream, manager) = Worker(id, r_stream, w_stream, manager, WorkerConfig())

function set_worker_state(w, state)
    w.state = state
    notify(w.c_state; all=true)
end

function send_msg_now(w::Worker, msg)
    send_msg_(w, msg, true)
end

function send_msg(w::Worker, msg)
    send_msg_(w, msg, false)
end

function flush_gc_msgs(w::Worker)
    if !isdefined(w, :w_stream)
        return
    end
    w.gcflag = false
    msgs = copy(w.add_msgs)
    if !isempty(msgs)
        empty!(w.add_msgs)
        remote_do(w, add_clients, msgs)
    end

    msgs = copy(w.del_msgs)
    if !isempty(msgs)
        empty!(w.del_msgs)
        #print("sending delete of $msgs\n")
        remote_do(w, del_clients, msgs)
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


function send_msg_(w::Worker, msg, now::Bool)
    check_worker_state(w)
    io = w.w_stream
    lock(io.lock)
    try
        serialize(io, msg)

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

## process group creation ##

type LocalProcess
    id::Int
    bind_addr::AbstractString
    bind_port::UInt16
    LocalProcess() = new(1)
end

const LPROC = LocalProcess()

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
            w.config.bind_addr = remotecall_fetch(w.id, get_bind_addr, w.id)
        end
    end
    get(w.config.bind_addr)
end

myid() = LPROC.id

nprocs() = length(PGRP.workers)
function nworkers()
    n = nprocs()
    n == 1 ? 1 : n-1
end

procs() = Int[x.id for x in PGRP.workers]
function procs(pid::Integer)
    if myid() == 1
        if (pid == 1) || (isa(map_pid_wrkr[pid].manager, LocalManager))
            Int[x.id for x in filter(w -> (w.id==1) || (isa(w.manager, LocalManager)), PGRP.workers)]
        else
            ipatpid = get_bind_addr(pid)
            Int[x.id for x in filter(w -> get_bind_addr(w) == ipatpid, PGRP.workers)]
        end
    else
        remotecall_fetch(1, procs, pid)
    end
end

function workers()
    allp = procs()
    if nprocs() == 1
       allp
    else
       filter(x -> x != 1, allp)
    end
end

function rmprocs(args...; waitfor = 0.0)
    # Only pid 1 can add and remove processes
    if myid() != 1
        error("only process 1 can add and remove processes")
    end

    rmprocset = []
    for i in vcat(args...)
        if i == 1
            warn("rmprocs: process 1 not removed")
        else
            if haskey(map_pid_wrkr, i)
                w = map_pid_wrkr[i]
                set_worker_state(w, W_TERMINATING)
                kill(w.manager, i, w.config)
                push!(rmprocset, w)
            end
        end
    end

    start = time()
    while (time() - start) < waitfor
        if all(w -> w.state == W_TERMINATED, rmprocset)
            break;
        else
            sleep(0.1)
        end
    end

    ((waitfor > 0) && any(w -> w.state != W_TERMINATED, rmprocset)) ? :timed_out : :ok
end


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

function worker_id_from_socket(s)
    w = get(map_sock_wrkr, s, nothing)
    if isa(w,Worker)
        if is(s, w.r_stream) || is(s, w.w_stream)
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
                        remote_do(rpid, deregister_worker, pid)
                    catch
                    end
                end
            end
        end
    end
    push!(map_del_wrkr, pid)

    # delete this worker from our RemoteRef client sets
    ids = []
    tonotify = []
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

## remote refs ##

const client_refs = WeakKeyDict()

type RemoteRef{RemoteStore}
    where::Int
    whence::Int
    id::Int
    # TODO: cache value if it's fetched, but don't serialize the cached value

    function RemoteRef(w, wh, id)
        r = new(w,wh,id)
        found = getkey(client_refs, r, false)
        if !is(found,false)
            return found
        end
        client_refs[r] = true
        finalizer(r, send_del_client)
        r
    end
end

let REF_ID::Int = 1
    global next_ref_id
    next_ref_id() = (id = REF_ID; REF_ID += 1; id)

    global next_rrid_tuple
    next_rrid_tuple() = (myid(),next_ref_id())
end

RemoteRef(w::LocalProcess) = RemoteRef(w.id)
RemoteRef(w::Worker) = RemoteRef(w.id)
function RemoteRef(pid::Integer=myid())
    rrid = next_rrid_tuple()
    RemoteRef{Channel{Any}}(pid, rrid[1], rrid[2])
end

function RemoteRef(f::Function, pid::Integer=myid())
    remotecall_fetch(pid, (f, rrid) -> begin
        rv=lookup_ref(rrid, f)
        RemoteRef{typeof(rv.c)}(myid(), rrid[1], rrid[2])
    end, f, next_rrid_tuple())
end

hash(r::RemoteRef, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::RemoteRef, s::RemoteRef) = (r.whence==s.whence && r.id==s.id)

rr2id(r::RemoteRef) = (r.whence, r.id)

lookup_ref(id, f=def_rv_channel) = lookup_ref(PGRP, id, f)
function lookup_ref(pg, id, f)
    rv = get(pg.refs, id, false)
    if rv === false
        # first we've heard of this ref
        rv = RemoteValue(f)
        pg.refs[id] = rv
        push!(rv.clientset, id[1])
    end
    rv
end

function isready(rr::RemoteRef, args...)
    rid = rr2id(rr)
    if rr.where == myid()
        isready(lookup_ref(rid).c, args...)
    else
        remotecall_fetch(rr.where, id->isready(lookup_ref(rid).c, args...), rid)
    end
end

del_client(id, client) = del_client(PGRP, id, client)
function del_client(pg, id, client)
    rv = lookup_ref(id)
    delete!(rv.clientset, client)
    if isempty(rv.clientset)
        delete!(pg.refs, id)
        #print("$(myid()) collected $id\n")
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

function send_del_client(rr::RemoteRef)
    if rr.where == myid()
        del_client(rr2id(rr), myid())
    else
        if in(rr.where, map_del_wrkr)
            # for a removed worker, don't bother
            return
        end
        w = worker_from_id(rr.where)
        push!(w.del_msgs, (rr2id(rr), myid()))
        w.gcflag = true
        notify(any_gc_flag)
    end
end

function add_client(id, client)
    #println("$(myid()) adding client $client to $id")
    rv = lookup_ref(id)
    push!(rv.clientset, client)
    nothing
end

function add_clients(pairs::Vector)
    for p in pairs
        add_client(p[1], p[2])
    end
end

function send_add_client(rr::RemoteRef, i)
    if rr.where == myid()
        add_client(rr2id(rr), i)
    elseif i != rr.where
        # don't need to send add_client if the message is already going
        # to the processor that owns the remote ref. it will add_client
        # itself inside deserialize().
        w = worker_from_id(rr.where)
        #println("$(myid()) adding $((rr2id(rr), i)) for $(rr.where)")
        push!(w.add_msgs, (rr2id(rr), i))
        w.gcflag = true
        notify(any_gc_flag)
    end
end

function serialize(s::SerializationState, rr::RemoteRef)
    i = worker_id_from_socket(s.io)
    #println("$(myid()) serializing $rr to $i")
    if i != -1
        #println("send add $rr to $i")
        send_add_client(rr, i)
    end
    invoke(serialize, Tuple{SerializationState, Any}, s, rr)
end

function deserialize(s::SerializationState, t::Type{RemoteRef})
    rr = invoke(deserialize, Tuple{SerializationState, DataType}, s, t)
    where = rr.where
    if where == myid()
        add_client(rr2id(rr), myid())
    end
    # call ctor to make sure this rr gets added to the client_refs table
    RemoteRef(where, rr.whence, rr.id)
end

# data stored by the owner of a RemoteRef
def_rv_channel() = Channel(1)
type RemoteValue
    c::AbstractChannel
    clientset::IntSet
    waitingfor::Int   # processor we need to hear from to fill this, or 0

    RemoteValue(f::Function) = new(f(), IntSet(), 0)
end

wait(rv::RemoteValue) = wait(rv.c)

## core messages: do, call, fetch, wait, ref, put! ##
type RemoteException <: Exception
    pid::Int
    captured::CapturedException
end

RemoteException(captured) = RemoteException(myid(), captured)
function show(io::IO, re::RemoteException)
    (re.pid != myid()) && print(io, "On worker ", re.pid, ":\n")
    show(io, re.captured)
end


function run_work_thunk(thunk, print_error)
    local result
    try
        result = thunk()
    catch err
        ce = CapturedException(err, catch_backtrace())
        result = RemoteException(ce)
        print_error && print(STDERR, ce)
    end
    result
end
function run_work_thunk(rv::RemoteValue, thunk)
    put!(rv, run_work_thunk(thunk, false))
    nothing
end

function schedule_call(rid, thunk)
    rv = RemoteValue(def_rv_channel)
    (PGRP::ProcessGroup).refs[rid] = rv
    push!(rv.clientset, rid[1])
    schedule(@task(run_work_thunk(rv,thunk)))
    rv
end

#localize_ref(b::Box) = Box(localize_ref(b.contents))

#function localize_ref(r::RemoteRef)
#    if r.where == myid()
#        fetch(r)
#    else
#        r
#    end
#end

#localize_ref(x) = x

# make a thunk to call f on args in a way that simulates what would happen if
# the function were sent elsewhere
function local_remotecall_thunk(f, args)
    if isempty(args)
        return f
    end
    return ()->f(args...)

    # TODO: this seems to be capable of causing deadlocks by waiting on
    # Refs buried inside the closure that we don't want to wait on yet.
    # linfo = ccall(:jl_closure_linfo, Any, (Any,), f)
    # if isa(linfo,LambdaStaticData)
    #     env = ccall(:jl_closure_env, Any, (Any,), f)
    #     buf = memio()
    #     serialize(buf, env)
    #     seek(buf, 0)
    #     env = deserialize(buf)
    #     f = ccall(:jl_new_closure, Any, (Ptr{Void}, Any, Any),
    #               C_NULL, env, linfo)::Function
    # end
    # f(map(localize_ref,args)...)
end

function remotecall(w::LocalProcess, f, args...)
    rr = RemoteRef(w)
    schedule_call(rr2id(rr), local_remotecall_thunk(f,args))
    rr
end

function remotecall(w::Worker, f, args...)
    rr = RemoteRef(w)
    #println("$(myid()) asking for $rr")
    send_msg(w, CallMsg{:call}(f, args, rr2id(rr)))
    rr
end

remotecall(id::Integer, f, args...) = remotecall(worker_from_id(id), f, args...)

# faster version of fetch(remotecall(...))
function remotecall_fetch(w::LocalProcess, f, args...)
    v=run_work_thunk(local_remotecall_thunk(f,args), false)
    isa(v, RemoteException) ? throw(v) : v
end

function remotecall_fetch(w::Worker, f, args...)
    # can be weak, because the program will have no way to refer to the Ref
    # itself, it only gets the result.
    oid = next_rrid_tuple()
    rv = lookup_ref(oid)
    rv.waitingfor = w.id
    send_msg(w, CallMsg{:call_fetch}(f, args, oid))
    v = take!(rv)
    delete!(PGRP.refs, oid)
    isa(v, RemoteException) ? throw(v) : v
end

remotecall_fetch(id::Integer, f, args...) =
    remotecall_fetch(worker_from_id(id), f, args...)

# faster version of wait(remotecall(...))
remotecall_wait(w::LocalProcess, f, args...) = wait(remotecall(w,f,args...))

function remotecall_wait(w::Worker, f, args...)
    prid = next_rrid_tuple()
    rv = lookup_ref(prid)
    rv.waitingfor = w.id
    rr = RemoteRef(w)
    send_msg(w, CallWaitMsg(f, args, rr2id(rr), prid))
    wait(rv)
    delete!(PGRP.refs, prid)
    rr
end

remotecall_wait(id::Integer, f, args...) =
    remotecall_wait(worker_from_id(id), f, args...)

function remote_do(w::LocalProcess, f, args...)
    # the LocalProcess version just performs in local memory what a worker
    # does when it gets a :do message.
    # same for other messages on LocalProcess.
    thk = local_remotecall_thunk(f, args)
    schedule(Task(thk))
    nothing
end

function remote_do(w::Worker, f, args...)
    send_msg(w, RemoteDoMsg(f, args))
    nothing
end

remote_do(id::Integer, f, args...) = remote_do(worker_from_id(id), f, args...)

# have the owner of rr call f on it
function call_on_owner(f, rr::RemoteRef, args...)
    rid = rr2id(rr)
    if rr.where == myid()
        f(rid, args...)
    else
        remotecall_fetch(rr.where, f, rid, args...)
    end
end

wait_ref(rid, args...) = (wait(lookup_ref(rid).c, args...); nothing)
wait(r::RemoteRef, args...) = (call_on_owner(wait_ref, r, args...); r)

fetch_ref(rid, args...) = fetch(lookup_ref(rid).c, args...)
fetch(r::RemoteRef, args...) = call_on_owner(fetch_ref, r, args...)
fetch(x::ANY) = x

# storing a value to a RemoteRef
put!(rv::RemoteValue, args...) = put!(rv.c, args...)
put_ref(rid, args...) = put!(lookup_ref(rid), args...)
put!(rr::RemoteRef, args...) = (call_on_owner(put_ref, rr, args...); rr)

take!(rv::RemoteValue, args...) = take!(rv.c, args...)
take_ref(rid, args...) = take!(lookup_ref(rid), args...)
take!(rr::RemoteRef, args...) = call_on_owner(take_ref, rr, args...)

close_ref(rid) = (close(lookup_ref(rid).c); nothing)
close(rr::RemoteRef) = call_on_owner(close_ref, rr)


function deliver_result(sock::IO, msg, oid, value)
    #print("$(myid()) sending result $oid\n")
    if is(msg,:call_fetch)
        val = value
    else
        val = oid
    end
    try
        send_msg_now(sock, ResultMsg(oid, val))
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
            remote_do(1, rmprocs, wid)
        end
    end
end

## message event handlers ##
process_messages(r_stream::TCPSocket, w_stream::TCPSocket) = @schedule process_tcp_streams(r_stream, w_stream)

function process_tcp_streams(r_stream::TCPSocket, w_stream::TCPSocket)
        disable_nagle(r_stream)
        wait_connected(r_stream)
        if r_stream != w_stream
            disable_nagle(w_stream)
            wait_connected(w_stream)
        end
        message_handler_loop(r_stream, w_stream)
end

process_messages(r_stream::AsyncStream, w_stream::AsyncStream) = @schedule message_handler_loop(r_stream, w_stream)

function message_handler_loop(r_stream::AsyncStream, w_stream::AsyncStream)
    global PGRP
    global cluster_manager

    try
        while true
            msg = deserialize(r_stream)
            # println("got msg: ", msg)
            handle_msg(msg, r_stream, w_stream)
        end
    catch e
        iderr = worker_id_from_socket(r_stream)
        if (iderr < 1)
            print(STDERR, "Socket from unknown remote worker in worker ", myid())
        else
            werr = worker_from_id(iderr)
            oldstate = werr.state
            set_worker_state(werr, W_TERMINATED)

            # If error occured talking to pid 1, commit harakiri
            if iderr == 1
                if isopen(w_stream)
                    print(STDERR, "fatal error on ", myid(), ": ")
                    display_error(e, catch_backtrace())
                end
                exit(1)
            end

            # Will treat any exception as death of node and cleanup
            # since currently we do not have a mechanism for workers to reconnect
            # to each other on unhandled errors
            deregister_worker(iderr)
        end

        if isopen(r_stream) close(r_stream) end
        if isopen(w_stream) close(w_stream) end

        if (myid() == 1) && (iderr > 1)
            if oldstate != W_TERMINATING
                println(STDERR, "Worker $iderr terminated.")
                rethrow(e)
            end
        end

        return nothing
    end
end

handle_msg(msg::CallMsg{:call}, r_stream, w_stream) = schedule_call(msg.response_oid, ()->msg.f(msg.args...))
function handle_msg(msg::CallMsg{:call_fetch}, r_stream, w_stream)
    @schedule begin
        v = run_work_thunk(()->msg.f(msg.args...), false)
        deliver_result(w_stream, :call_fetch, msg.response_oid, v)
    end
end

function handle_msg(msg::CallWaitMsg, r_stream, w_stream)
    @schedule begin
        rv = schedule_call(msg.response_oid, ()->msg.f(msg.args...))
        deliver_result(w_stream, :call_wait, msg.notify_oid, wait(rv))
    end
end

handle_msg(msg::RemoteDoMsg, r_stream, w_stream) = @schedule run_work_thunk(()->msg.f(msg.args...), true)

handle_msg(msg::ResultMsg, r_stream, w_stream) = put!(lookup_ref(msg.response_oid), msg.value)

handle_msg(msg::IdentifySocketMsg, r_stream, w_stream) = Worker(msg.from_pid, r_stream, w_stream, cluster_manager)

function handle_msg(msg::JoinPGRPMsg, r_stream, w_stream)
    LPROC.id = msg.self_pid
    controller = Worker(1, r_stream, w_stream, cluster_manager)
    register_worker(LPROC)
    topology(msg.topology)

    wait_tasks = Task[]
    for (connect_at, rpid, r_is_local) in msg.other_workers
        wconfig = WorkerConfig()
        wconfig.connect_at = connect_at
        wconfig.environ = AnyDict(:self_is_local=>msg.self_is_local, :r_is_local=>r_is_local)

        let rpid=rpid, wconfig=wconfig
            t = @async connect_to_peer(cluster_manager, rpid, wconfig)
            push!(wait_tasks, t)
        end
    end

    for wt in wait_tasks; wait(wt); end

    send_msg_now(controller, JoinCompleteMsg(msg.notify_oid, Sys.CPU_CORES, getpid()))
end

function connect_to_peer(manager::ClusterManager, rpid::Int, wconfig::WorkerConfig)
    try
        (r_s, w_s) = connect(manager, rpid, wconfig)
        w = Worker(rpid, r_s, w_s, manager, wconfig)
        process_messages(w.r_stream, w.w_stream)
        send_msg_now(w, IdentifySocketMsg(myid()))
    catch e
        println(STDERR, "Error [$e] on $(myid()) while connecting to peer $rpid. Exiting.")
        exit(1)
    end
end

function handle_msg(msg::JoinCompleteMsg, r_stream, w_stream)
    w = map_sock_wrkr[r_stream]

    environ = get(w.config.environ, Dict())
    environ[:cpu_cores] = msg.cpu_cores
    w.config.environ = environ
    w.config.ospid = msg.ospid

    ntfy_channel = lookup_ref(msg.notify_oid)
    put!(ntfy_channel, w.id)
end

function disable_threaded_libs()
    blas_set_num_threads(1)
end

worker_timeout() = parse(Float64, get(ENV, "JULIA_WORKER_TIMEOUT", "60.0"))


## worker creation and setup ##

# The entry point for julia worker processes. does not return. Used for TCP transport.
# Cluster managers implementing their own transport will provide their own.
# Argument is descriptor to write listening port # to.
start_worker() = start_worker(STDOUT)
function start_worker(out::IO)
    # we only explicitly monitor worker STDOUT on the console, so redirect
    # stderr to stdout so we can see the output.
    # at some point we might want some or all worker output to go to log
    # files instead.
    # Currently disabled since this caused processes to spin instead of
    # exit when process 1 shut down. Don't yet know why.
    #redirect_stderr(STDOUT)

    init_worker()
    if LPROC.bind_port == 0
        (actual_port,sock) = listenany(UInt16(9009))
        LPROC.bind_port = actual_port
    else
        sock = listen(LPROC.bind_port)
    end
    @schedule while isopen(sock)
        client = accept(sock)
        process_messages(client, client)
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

function init_worker(manager::ClusterManager=DefaultClusterManager())
    # On workers, the default cluster manager connects via TCP sockets. Custom
    # transports will need to call this function with their own manager.
    global cluster_manager
    cluster_manager = manager
    disable_threaded_libs()

    # Since our pid has yet to be set, ensure no RemoteRefs have been created or addprocs() called.
    assert(nprocs() <= 1)
    assert(isempty(PGRP.refs))
    assert(isempty(client_refs))

    # System is started in head node mode, cleanup entries related to the same
    empty!(PGRP.workers)
    empty!(map_pid_wrkr)
end


# The main function for adding worker processes.
# `manager` is of type ClusterManager. The respective managers are responsible
# for launching the workers. All keyword arguments (plus a few default values)
# are available as a dictionary to the `launch` methods
function addprocs(manager::ClusterManager; kwargs...)

    params = merge(default_addprocs_params(), AnyDict(kwargs))
    topology(symbol(params[:topology]))

    # some libs by default start as many threads as cores which leads to
    # inefficient use of cores in a multi-process model.
    # Should be a keyword arg?
    disable_threaded_libs()

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
            if length(launched) == 0
                istaskdone(t_launch) && break
                @schedule (sleep(1); notify(launch_ntfy))
                wait(launch_ntfy)
            end

            if (length(launched) > 0)
                wconfig = shift!(launched)
                let wconfig=wconfig
                    @async setup_launched_worker(manager, wconfig, launched_q)
                end
            end
        end
    end

    wait(t_launch)      # catches any thrown errors from the launch task

    # Let all workers know the current set of valid workers. Useful
    # for nprocs(), nworkers(), etc to return valid values on the workers.
    # Since all worker-to-worker setups may not have completed by the time this
    # function returns to the caller.
    all_w = workers()
    for pid in all_w
        remote_do(pid, set_valid_processes, all_w)
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
    :exeflags => ``)


function setup_launched_worker(manager, wconfig, launched_q)
    pid = create_worker(manager, wconfig)
    push!(launched_q, pid)

    # When starting workers on remote multi-core hosts, `launch` can (optionally) start only one
    # process on the remote machine, with a request to start additional workers of the
    # same type. This is done by setting an appropriate value to `WorkerConfig.cnt`.
    cnt = get(wconfig.count, 1)
    if cnt == :auto
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

        new_addresses = remotecall_fetch(frompid, launch_additional, cnt, cmd)
        for address in new_addresses
            (bind_addr, port) = address

            wconfig = WorkerConfig()
            for x in [:host, :tunnel, :sshflags, :exeflags, :exename]
                setfield!(wconfig, x, getfield(fromconfig, x))
            end
            wconfig.bind_addr = bind_addr
            wconfig.port = port

            let wconfig=wconfig
                @async begin
                    pid = create_worker(manager, wconfig)
                    remote_do(frompid, redirect_output_from_additional_worker, pid, port)
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
    w = Worker(w.id, r_s, w_s, manager, wconfig)
    # install a finalizer to perform cleanup if necessary
    finalizer(w, (w)->if myid() == 1 manage(w.manager, w.id, w.config, :finalize) end)

    # set when the new worker has finshed connections with all other workers
    ntfy_oid = next_rrid_tuple()
    rr_ntfy_join = lookup_ref(ntfy_oid)
    rr_ntfy_join.waitingfor = myid()

    # Start a new task to handle inbound messages from connected worker in master.
    # Also calls `wait_connected` on TCP streams.
    process_messages(w.r_stream, w.w_stream)

    # send address information of all workers to the new worker.
    # Cluster managers set the address of each worker in `WorkerConfig.connect_at`.
    # A new worker uses this to setup a all-to-all network. Workers with higher pids connect to
    # workers with lower pids. Except process 1 (master) which initiates connections
    # to all workers.
    # Flow:
    # - master sends (:join_pgrp, list_of_all_worker_addresses) to all workers
    # - On each worker
    #   - each worker sends a :identify_socket to all workers less than its pid
    #   - each worker then sends a :join_complete back to the master along with its OS_PID and NUM_CORES
    # - once master receives a :join_complete it triggers rr_ntfy_join (signifies that worker setup is complete)

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

    all_locs = map(x -> isa(x, Worker) ? (get(x.config.connect_at, ()), x.id, isa(x.manager, LocalManager)) : ((), x.id, true), join_list)
    send_msg_now(w, JoinPGRPMsg(w.id, all_locs, isa(w.manager, LocalManager), ntfy_oid, PGRP.topology))

    @schedule manage(w.manager, w.id, w.config, :register)
    wait(rr_ntfy_join)
    delete!(PGRP.refs, ntfy_oid)

    w.id
end


# Called on the first worker on a remote host. Used to optimize launching
# of multiple workers on a remote host (to leverage multi-core)

additional_io_objs=Dict()
function launch_additional(np::Integer, cmd::Cmd)
    io_objs = cell(np)
    addresses = cell(np)

    for i in 1:np
        io, pobj = open(detach(cmd), "r")
        io_objs[i] = io
    end

    for (i,io) in enumerate(io_objs)
        (host, port) = read_worker_host_port(io)
        addresses[i] = (host, port)
        additional_io_objs[port] = io
    end

    addresses
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
        env = thunk.env
        if isa(env,Tuple)
            for v in env
                if isa(v,Box)
                    v = v.contents
                end
                if isa(v,RemoteRef)
                    p = v.where; break
                end
            end
        end
        if p == -1
            p = workers()[(nextidx % nworkers()) + 1]
            nextidx += 1
        end
        p
    end
end

spawnat(p, thunk) = sync_add(remotecall(p, thunk))

spawn_somewhere(thunk) = spawnat(chooseproc(thunk),thunk)

macro spawn(expr)
    expr = localize_vars(:(()->($expr)), false)
    :(spawn_somewhere($(esc(expr))))
end

macro spawnat(p, expr)
    expr = localize_vars(:(()->($expr)), false)
    :(spawnat($(esc(p)), $(esc(expr))))
end

macro fetch(expr)
    expr = localize_vars(:(()->($expr)), false)
    quote
        thunk = $(esc(expr))
        remotecall_fetch(chooseproc(thunk), thunk)
    end
end

macro fetchfrom(p, expr)
    expr = localize_vars(:(()->($expr)), false)
    :(remotecall_fetch($(esc(p)), $(esc(expr))))
end

macro everywhere(ex)
    quote
        sync_begin()
        thunk = ()->(eval(Main,$(Expr(:quote,ex))); nothing)
        for pid in workers()
            async_run_thunk(()->remotecall_fetch(pid, thunk))
            yield() # ensure that the remotecall_fetch has been started
        end

        # execute locally last.
        if nprocs() > 1
            async_run_thunk(thunk)
        end

        sync_end()
    end
end

function pmap_static(f, lsts...)
    np = nprocs()
    n = length(lsts[1])
    Any[ remotecall(PGRP.workers[(i-1)%np+1].id, f, map(L->L[i], lsts)...) for i = 1:n ]
end

pmap(f) = f()

# dynamic scheduling by creating a local task to feed work to each processor
# as it finishes.
# example unbalanced workload:
# rsym(n) = (a=rand(n,n);a*a')
# L = {rsym(200),rsym(1000),rsym(200),rsym(1000),rsym(200),rsym(1000),rsym(200),rsym(1000)};
# pmap(eig, L);
function pmap(f, lsts...; err_retry=0, err_stop=true, pids = workers())

    results = Dict{Int,Any}()

    busy_workers = fill(false, length(pids))
    busy_workers_ntfy = Condition()

    retryqueue = []

    tasks = enumerate(zip(lsts...))
    state = start(tasks)
    abort_tasks() = state = nothing

    function next_task() 

        if state != nothing && !done(tasks, state)
            ((idx, args), state) = next(tasks, state)
            return (idx, args, 0)
        elseif !isempty(retryqueue)
            return shift!(retryqueue)
        end

        # Handles the condition where we have finished processing the requested
        # lsts as well as any retryqueue entries, but there are still some jobs
        # active that may result in an error and have to be retried.
        while any(busy_workers)
            wait(busy_workers_ntfy)
            if !isempty(retryqueue)
                return shift!(retryqueue)
            end
        end

        return nothing
    end

    @sync begin
        for (pididx, wpid) in enumerate(pids)
            @async begin
                while (task = next_task()) != nothing

                    (idx, args, retry_count) = task
                    busy_workers[pididx] = true

                    try
                        results[idx] = remotecall_fetch(wpid, f, args...)
                    catch ex
                        if retry_count < err_retry
                            push!(retryqueue, (idx, args, retry_count + 1))
                        elseif err_stop
                            abort_tasks()
                            rethrow(ex)
                        else
                            results[idx] = ex
                        end
                    finally
                        busy_workers[pididx] = false
                        notify(busy_workers_ntfy; all=true)
                    end
                end
            end
        end
    end

    [results[x] for x in 1:length(results)]
end

# Statically split range [1,N] into equal sized chunks for np processors
function splitrange(N::Int, np::Int)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Array(UnitRange{Int}, nchunks)
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

function preduce(reducer, f, N::Int)
    chunks = splitrange(N, nworkers())
    all_w = workers()[1:length(chunks)]

    w_exec = Task[]
    for (idx,pid) in enumerate(all_w)
        t = Task(()->remotecall_fetch(pid, f, first(chunks[idx]), last(chunks[idx])))
        schedule(t)
        push!(w_exec, t)
    end
    reduce(reducer, [wait(t) for t in w_exec])
end

function pfor(f, N::Int)
    [@spawn f(first(c), last(c)) for c in splitrange(N, nworkers())]
end

function make_preduce_body(reducer, var, body, R)
    quote
        function (lo::Int, hi::Int)
            $(esc(var)) = ($R)[lo]
            ac = $(esc(body))
            if lo != hi
                for $(esc(var)) in ($R)[(lo+1):hi]
                    ac = ($(esc(reducer)))(ac, $(esc(body)))
                end
            end
            ac
        end
    end
end

function make_pfor_body(var, body, R)
    quote
        function (lo::Int, hi::Int)
            for $(esc(var)) in ($R)[lo:hi]
                $(esc(body))
            end
        end
    end
end

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
    if !isa(loop,Expr) || !is(loop.head,:for)
        error("malformed @parallel loop")
    end
    var = loop.args[1].args[1]
    r = loop.args[1].args[2]
    body = loop.args[2]
    if na==1
        thecall = :(pfor($(make_pfor_body(var, body, :therange)), length(therange)))
    else
        thecall = :(preduce($(esc(reducer)),
                            $(make_preduce_body(reducer, var, body, :therange)), length(therange)))
    end
    localize_vars(quote therange = $(esc(r)); $thecall; end)
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


function timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)
    pollint > 0 || throw(ArgumentError("cannot set pollint to $pollint seconds"))
    start = time()
    done = RemoteRef()
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
interrupt(pids::Integer...) = interrupt([pids...])

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
    @linux_only begin
        # tcp_quickack is a linux only option
        if ccall(:jl_tcp_quickack, Cint, (Ptr{Void}, Cint), sock.handle, 1) < 0
            warn_once("Parallel networking unoptimized ( Error enabling TCP_QUICKACK : ", Libc.strerror(Libc.errno()), " )")
        end
    end
end

function check_same_host(pids)
    if myid() != 1
        return remotecall_fetch(1, check_same_host, pids)
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
    if myid() != 1
        return
    end

    if nprocs() > 1
        ret = rmprocs(workers(); waitfor=0.5)
        if ret != :ok
            warn("Forcibly interrupting busy workers")
            # Might be computation bound, interrupt them and try again
            interrupt(workers())
            ret = rmprocs(workers(); waitfor=0.5)
            if ret != :ok
                warn("Unable to terminate all workers")
            end
        end
    end
end

getindex(r::RemoteRef) = fetch(r)
function getindex(r::RemoteRef, args...)
    if r.where == myid()
        return getindex(fetch(r), args...)
    end
    return remotecall_fetch(r.where, getindex, r, args...)
end
