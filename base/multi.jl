## multi.jl - multiprocessing
##
## julia starts with one process, and processors can be added using:
##   addprocs(n)                         using exec
##   addprocs({"host1","host2",...})     using remote execution
##
## remotecall(w, func, args...) -
##     tell a worker to call a function on the given arguments.
##     returns a RemoteRef to the result.
##
## remote_do(w, f, args...) - remote function call with no result
##
## wait(rr) - wait for a RemoteRef to be finished computing
##
## fetch(rr) - wait for and get the value of a RemoteRef
##
## remotecall_fetch(w, func, args...) - faster fetch(remotecall(...))
##
## pmap(func, lst) -
##     call a function on each element of lst (some 1-d thing), in
##     parallel.
##
## RemoteRef() - create an uninitialized RemoteRef on the local processor
##
## RemoteRef(p) - ...or on a particular processor
##
## put!(r, val) - store a value to an uninitialized RemoteRef
##
## @spawn expr -
##     evaluate expr somewhere. returns a RemoteRef. all variables in expr
##     are copied to the remote processor.
##
## @spawnat p expr - @spawn specifying where to run
##
## @async expr -
##     run expr as an asynchronous task on the local processor
##
## @parallel (r) for i=1:n ... end -
##     parallel loop. the results from each iteration are reduced using (r).
##
## @everywhere expr - run expr everywhere.

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

function send_msg_unknown(s::IO, kind, args)
    error("attempt to send to unknown socket")
end

function send_msg(s::IO, kind, args...)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg(worker_from_id(id), kind, args...)
    end
    send_msg_unknown(s, kind, args)
end

function send_msg_now(s::IO, kind, args...)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg_now(worker_from_id(id), kind, args...)
    end
    send_msg_unknown(s, kind, args)
end

abstract ClusterManager

type WorkerConfig
    # Common fields relevant to all cluster managers
    io::Nullable{IO}
    host::Nullable{AbstractString}
    port::Nullable{Integer}

    # Used when launching additional workers at a host
    count::Nullable{Union(Int, Symbol)}
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

    function WorkerConfig()
        wc = new()
        for n in fieldnames(WorkerConfig)
            T = eltype(fieldtype(WorkerConfig, n))
            setfield!(wc, n, Nullable{T}())
        end
        wc
    end
end



type Worker
    id::Int
    r_stream::AsyncStream
    w_stream::AsyncStream
    manager::ClusterManager
    config::WorkerConfig

    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    gcflag::Bool

    Worker(id, r_stream, w_stream, manager, config) =
        new(id, r_stream, buffer_writes(w_stream), manager, config, [], [], false)
end

Worker(id, r_stream, w_stream, manager) = Worker(id, r_stream, w_stream, manager, WorkerConfig())



function send_msg_now(w::Worker, kind, args...)
    send_msg_(w, kind, args, true)
end

function send_msg(w::Worker, kind, args...)
    send_msg_(w, kind, args, false)
end

function flush_gc_msgs(w::Worker)
    w.gcflag = false
    msgs = copy(w.add_msgs)
    if !isempty(msgs)
        empty!(w.add_msgs)
        remote_do(w, add_clients, msgs...)
    end

    msgs = copy(w.del_msgs)
    if !isempty(msgs)
        empty!(w.del_msgs)
        #print("sending delete of $msgs\n")
        remote_do(w, del_clients, msgs...)
    end
end

function send_msg_(w::Worker, kind, args, now::Bool)
    #println("Sending msg $kind")
    io = w.w_stream
    lock(io) do io
        serialize(io, kind)
        for arg in args
            serialize(io, arg)
        end

        if !now && w.gcflag
            flush_gc_msgs(w)
        else
            flush(io)
        end
    end
end

function flush_gc_msgs()
    for w in (PGRP::ProcessGroup).workers
        if isa(w,Worker)
            k = w::Worker
            if k.gcflag
                flush_gc_msgs(k)
            end
        end
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

const map_pid_wrkr = Dict{Int, Union(Worker, LocalProcess)}()
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

    # global references
    refs::Dict

    ProcessGroup(w::Array{Any,1}) = new("pg-default", w, Dict())
end
const PGRP = ProcessGroup([])

get_bind_addr(pid::Integer) = get_bind_addr(worker_from_id(pid))
get_bind_addr(w::LocalProcess) = LPROC.bind_addr
function get_bind_addr(w::Worker)
    if isnull(w.config.bind_addr)
        if w.id != myid()
            w.config.bind_addr = remotecall_fetch(w.id, get_bind_addr, w.id)
        end
    end
    w.config.bind_addr
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

rmprocset = Set()
function rmprocs(args...; waitfor = 0.0)
    # Only pid 1 can add and remove processes
    if myid() != 1
        error("only process 1 can add and remove processes")
    end

    global rmprocset
    empty!(rmprocset)

    for i in vcat(args...)
        if i == 1
            warn("rmprocs: process 1 not removed")
        else
            if haskey(map_pid_wrkr, i)
                push!(rmprocset, i)
                w = map_pid_wrkr[i]
                kill(w.manager, i, w.config)
            end
        end
    end

    start = time()
    while (time() - start) < waitfor
        if length(rmprocset) == 0
            break;
        else
            sleep(0.1)
        end
    end

    ((waitfor > 0) && (length(rmprocset) > 0)) ? :timed_out : :ok
end


type ProcessExitedException <: Exception end

worker_from_id(i) = worker_from_id(PGRP, i)
function worker_from_id(pg::ProcessGroup, i)
#   Processes with pids > ours, have to connect to us. May not have happened. Wait for some time.
    if in(i, map_del_wrkr)
        throw(ProcessExitedException())
    end
    if myid()==1 && !haskey(map_pid_wrkr,i)
        error("no process with id $i exists")
    end
    start = time()
    while (!haskey(map_pid_wrkr, i) && ((time() - start) < 60.0))
        sleep(0.1)
        yield()
    end
    map_pid_wrkr[i]
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
    if isa(w, Worker)
        map_sock_wrkr[w.r_stream] = w
        map_sock_wrkr[w.w_stream] = w
    end
end

deregister_worker(pid) = deregister_worker(PGRP, pid)
function deregister_worker(pg, pid)
    pg.workers = filter(x -> !(x.id == pid), pg.workers)
    w = pop!(map_pid_wrkr, pid, nothing)
    if isa(w, Worker)
        pop!(map_sock_wrkr, w.r_stream)
        if w.r_stream != w.w_stream
            pop!(map_sock_wrkr, w.w_stream)
        end

        # Notify the cluster manager of this workers death
        if myid() == 1
            manage(w.manager, w.id, w.config, :deregister)
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
        notify_error(rv.full, ProcessExitedException())
        delete!(pg.refs, id)
    end
end

## remote refs ##

const client_refs = WeakKeyDict()

type RemoteRef
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

    REQ_ID::Int = 0
    function RemoteRef(pid::Integer)
        rr = RemoteRef(pid, myid(), REQ_ID)
        REQ_ID += 1
        if mod(REQ_ID,200) == 0
            # force gc after making a lot of refs since they take up
            # space on the machine where they're stored, yet the client
            # is responsible for freeing them.
            gc()
        end
        rr
    end

    RemoteRef(w::LocalProcess) = RemoteRef(w.id)
    RemoteRef(w::Worker) = RemoteRef(w.id)
    RemoteRef() = RemoteRef(myid())

    global next_id
    next_id() = (id=(myid(),REQ_ID); REQ_ID+=1; id)
end

hash(r::RemoteRef, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::RemoteRef, s::RemoteRef) = (r.whence==s.whence && r.id==s.id)

rr2id(r::RemoteRef) = (r.whence, r.id)

lookup_ref(id) = lookup_ref(PGRP, id)
function lookup_ref(pg, id)
    rv = get(pg.refs, id, false)
    if rv === false
        # first we've heard of this ref
        rv = RemoteValue()
        pg.refs[id] = rv
        push!(rv.clientset, id[1])
    end
    rv
end

function isready(rr::RemoteRef)
    rid = rr2id(rr)
    if rr.where == myid()
        lookup_ref(rid).done
    else
        remotecall_fetch(rr.where, id->lookup_ref(id).done, rid)
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

function del_clients(pairs::(Any,Any)...)
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

function add_clients(pairs::(Any,Any)...)
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

function serialize(s, rr::RemoteRef)
    i = worker_id_from_socket(s)
    #println("$(myid()) serializing $rr to $i")
    if i != -1
        #println("send add $rr to $i")
        send_add_client(rr, i)
    end
    invoke(serialize, (Any, Any), s, rr)
end

function deserialize(s, t::Type{RemoteRef})
    rr = invoke(deserialize, (Any, DataType), s, t)
    where = rr.where
    if where == myid()
        add_client(rr2id(rr), myid())
    end
    # call ctor to make sure this rr gets added to the client_refs table
    RemoteRef(where, rr.whence, rr.id)
end

# data stored by the owner of a RemoteRef
type RemoteValue
    done::Bool
    result
    full::Condition   # waiting for a value
    empty::Condition  # waiting for value to be removed
    clientset::IntSet
    waitingfor::Int   # processor we need to hear from to fill this, or 0

    RemoteValue() = new(false, nothing, Condition(), Condition(), IntSet(), 0)
end

function work_result(rv::RemoteValue)
    v = rv.result
    if isa(v,WeakRef)
        v = v.value
    end
    v
end

function wait_full(rv::RemoteValue)
    while !rv.done
        wait(rv.full)
    end
    return work_result(rv)
end

function wait_empty(rv::RemoteValue)
    while rv.done
        wait(rv.empty)
    end
    return nothing
end

## core messages: do, call, fetch, wait, ref, put! ##

function run_work_thunk(thunk)
    local result
    try
        result = thunk()
    catch err
        print(STDERR, "exception on ", myid(), ": ")
        display_error(err,catch_backtrace())
        result = err
    end
    result
end
function run_work_thunk(rv::RemoteValue, thunk)
    put!(rv, run_work_thunk(thunk))
    nothing
end

function schedule_call(rid, thunk)
    rv = RemoteValue()
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
    send_msg(w, :call, rr2id(rr), f, args)
    rr
end

remotecall(id::Integer, f, args...) = remotecall(worker_from_id(id), f, args...)

# faster version of fetch(remotecall(...))
function remotecall_fetch(w::LocalProcess, f, args...)
    run_work_thunk(local_remotecall_thunk(f,args))
end

function remotecall_fetch(w::Worker, f, args...)
    # can be weak, because the program will have no way to refer to the Ref
    # itself, it only gets the result.
    oid = next_id()
    rv = lookup_ref(oid)
    rv.waitingfor = w.id
    send_msg(w, :call_fetch, oid, f, args)
    v = wait_full(rv)
    delete!(PGRP.refs, oid)
    v
end

remotecall_fetch(id::Integer, f, args...) =
    remotecall_fetch(worker_from_id(id), f, args...)

# faster version of wait(remotecall(...))
remotecall_wait(w::LocalProcess, f, args...) = wait(remotecall(w,f,args...))

function remotecall_wait(w::Worker, f, args...)
    prid = next_id()
    rv = lookup_ref(prid)
    rv.waitingfor = w.id
    rr = RemoteRef(w)
    send_msg(w, :call_wait, rr2id(rr), prid, f, args)
    wait_full(rv)
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
    send_msg(w, :do, f, args)
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

wait_ref(rid) = (wait_full(lookup_ref(rid)); nothing)
wait(r::RemoteRef) = (call_on_owner(wait_ref, r); r)

fetch_ref(rid) = wait_full(lookup_ref(rid))
fetch(r::RemoteRef) = call_on_owner(fetch_ref, r)
fetch(x::ANY) = x

# storing a value to a RemoteRef
function put!(rv::RemoteValue, val::ANY)
    wait_empty(rv)
    rv.result = val
    rv.done = true
    notify_full(rv)
    rv
end

put_ref(rid, v) = put!(lookup_ref(rid), v)
put!(rr::RemoteRef, val::ANY) = (call_on_owner(put_ref, rr, val); rr)

function take!(rv::RemoteValue)
    wait_full(rv)
    val = rv.result
    rv.done = false
    rv.result = nothing
    notify_empty(rv)
    val
end

take_ref(rid) = take!(lookup_ref(rid))
take!(rr::RemoteRef) = call_on_owner(take_ref, rr)

function deliver_result(sock::IO, msg, oid, value)
    #print("$(myid()) sending result $oid\n")
    if is(msg,:call_fetch)
        val = value
    else
        val = oid
    end
    try
        send_msg_now(sock, :result, oid, val)
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

# notify waiters that a certain job has finished or RemoteRef has been emptied
notify_full (rv::RemoteValue) = notify(rv.full, work_result(rv))
notify_empty(rv::RemoteValue) = notify(rv.empty)

## message event handlers ##

# activity on accept fd
function accept_handler(server::TCPServer, status::Int32)
    if status == -1
        error("an error occured during the creation of the server")
    end
    client = accept_nonblock(server)
    process_messages(client, client)
end

function process_messages(r_stream::TCPSocket, w_stream::TCPSocket; kwargs...)
    @schedule begin
        disable_nagle(r_stream)
        start_reading(r_stream)
        wait_connected(r_stream)
        if r_stream != w_stream
            disable_nagle(w_stream)
            wait_connected(w_stream)
        end
        create_message_handler_loop(r_stream, w_stream; kwargs...)
    end
end

function process_messages(r_stream::AsyncStream, w_stream::AsyncStream; kwargs...)
    create_message_handler_loop(r_stream, w_stream; kwargs...)
end

function create_message_handler_loop(r_stream::AsyncStream, w_stream::AsyncStream; ntfy_join_complete=nothing) #returns immediately
    @schedule begin
        global PGRP
        global cluster_manager

        try
            while true
                msg = deserialize(r_stream)
                #println("got msg: ",msg)
                # handle message
                if is(msg, :call)
                    id = deserialize(r_stream)
                    #print("$(myid()) got id $id\n")
                    f0 = deserialize(r_stream)
                    #print("$(myid()) got call $f0\n")
                    args0 = deserialize(r_stream)
                    #print("$(myid()) got args $args0\n")
                    let f=f0, args=args0
                        schedule_call(id, ()->f(args...))
                    end
                elseif is(msg, :call_fetch)
                    id = deserialize(r_stream)
                    f = deserialize(r_stream)
                    args = deserialize(r_stream)
                    let f=f, args=args, id=id, msg=msg
                        @schedule begin
                            v = run_work_thunk(()->f(args...))
                            deliver_result(w_stream, msg, id, v)
                            v
                        end
                    end
                elseif is(msg, :call_wait)
                    id = deserialize(r_stream)
                    notify_id = deserialize(r_stream)
                    f = deserialize(r_stream)
                    args = deserialize(r_stream)
                    let f=f, args=args, id=id, msg=msg, notify_id=notify_id
                        @schedule begin
                            rv = schedule_call(id, ()->f(args...))
                            deliver_result(w_stream, msg, notify_id, wait_full(rv))
                        end
                    end
                elseif is(msg, :do)
                    f = deserialize(r_stream)
                    args = deserialize(r_stream)
                    #print("got args: $args\n")
                    let f=f, args=args
                        @schedule begin
                            run_work_thunk(RemoteValue(), ()->f(args...))
                        end
                    end
                elseif is(msg, :result)
                    # used to deliver result of wait or fetch
                    oid = deserialize(r_stream)
                    #print("$(myid()) got $msg $oid\n")
                    val = deserialize(r_stream)
                    put!(lookup_ref(oid), val)
                elseif is(msg, :identify_socket)
                    otherid = deserialize(r_stream)
                    register_worker(Worker(otherid, r_stream, w_stream, cluster_manager))
                elseif is(msg, :join_pgrp)
                    self_pid = LPROC.id = deserialize(r_stream)
                    locs = deserialize(r_stream)
                    self_is_local = deserialize(r_stream)
                    controller = Worker(1, r_stream, w_stream, cluster_manager)
                    register_worker(controller)
                    register_worker(LPROC)

                    for (connect_at, rpid, r_is_local) in locs
                        if (rpid < self_pid) && (!(rpid == 1))
                            # Connect processes with lower pids
                            wconfig = WorkerConfig()
                            wconfig.connect_at = connect_at
                            wconfig.environ = AnyDict(:self_is_local=>self_is_local, :r_is_local=>r_is_local)

                            (r_s, w_s) = connect(cluster_manager, rpid, wconfig)
                            w = Worker(rpid, r_s, w_s, cluster_manager, wconfig)
                            register_worker(w)
                            process_messages(w.r_stream, w.w_stream)
                            send_msg_now(w, :identify_socket, self_pid)
                        else
                            # Processes with higher pids connect to us. Don't do anything just yet
                            continue
                        end
                    end

                    send_msg_now(controller, :join_complete, Sys.CPU_CORES, getpid())

                elseif is(msg, :join_complete)
                    w = map_sock_wrkr[r_stream]

                    environ = get(w.config.environ, Dict())
                    environ[:cpu_cores] = deserialize(r_stream)
                    w.config.environ = environ

                    w.config.ospid = deserialize(r_stream)

                    put!(ntfy_join_complete, w.id)
                    ntfy_join_complete = nothing    # so that it gets gc'ed
                end

            end # end of while
        catch e
            iderr = worker_id_from_socket(r_stream)
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

            if isopen(r_stream) close(r_stream) end
            if isopen(w_stream) close(w_stream) end

            if (myid() == 1)
                global rmprocset
                if in(iderr, rmprocset)
                    delete!(rmprocset, iderr)
                else
                    println(STDERR, "Worker $iderr terminated.")
                    rethrow(e)
                end
            end

            return nothing
        end
    end
end

function disable_threaded_libs()
    blas_set_num_threads(1)
end

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
        (actual_port,sock) = listenany(uint16(9009))
        LPROC.bind_port = actual_port
    else
        sock = listen(LPROC.bind_port)
    end
    sock.ccb = accept_handler
    print(out, "julia_worker:")  # print header
    print(out, "$(dec(LPROC.bind_port))#") # print port
    print(out, LPROC.bind_addr)
    print(out, '\n')
    flush(out)
    # close STDIN; workers will not use it
    #close(STDIN)

    disable_nagle(sock)

    try
        # To prevent hanging processes on remote machines, newly launched workers exit if the
        # master process does not connect in time.
        # TODO : Make timeout configurable.
        check_master_connect(60.0)
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
    io.line_buffered = true
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
    if m != nothing
        (m.captures[2], parseint(Int16, m.captures[1]))
    else
        ("", int16(-1))
    end
end

function init_worker(manager::ClusterManager=DefaultClusterManager())
    # On workers, the default cluster manager connects via TCP sockets. Custom
    # transports will need to call this function with their own manager.
    global cluster_manager
    cluster_manager = manager
    disable_threaded_libs()
end


# The main function for adding worker processes.
# `manager` is of type ClusterManager. The respective managers are responsible
# for launching the workers. All keyword arguments (plus a few default values)
# are available as a dictionary to the `launch` methods
function addprocs(manager::ClusterManager; kwargs...)
    params = merge(default_addprocs_params(), AnyDict(kwargs))

    # some libs by default start as many threads as cores which leads to
    # inefficient use of cores in a multi-process model.
    # Should be a keyword arg?
    disable_threaded_libs()

    # References to launched workers, filled when each worker is fully initialized and
    # has connected to all nodes.
    rr_launched = RemoteRef[]   # Asynchronously filled by the launch method

    start_cluster_workers(manager, params, rr_launched)

    # Wait for all workers to be fully connected
    sort!([fetch(rr) for rr in rr_launched])
end


default_addprocs_params() = AnyDict(
    :dir      => pwd(),
    :exename  => joinpath(JULIA_HOME,julia_exename()),
    :exeflags => ``)


function start_cluster_workers(manager, params, rr_launched)
    # The `launch` method should add an object of type WorkerConfig for every
    # worker launched. It provides information required on how to connect
    # to it.
    launched = WorkerConfig[]
    launch_ntfy = Condition()

    # call manager's `launch` is a separate task. This allows the master
    # process initiate the connection setup process as and when workers come
    # online
    t = @schedule try
            launch(manager, params, launched, launch_ntfy)
        catch e
            print(STDERR, "Error launching workers with $(typeof(manager)) : $e\n")
        end

    # When starting workers on remote multi-core hosts, `launch` can (optionally) start only one
    # process on the remote machine, with a request to start additional workers of the
    # same type. This is done by setting an appropriate value to `WorkerConfig.cnt`.
    workers_with_additional = []  # List of workers with additional on-host workers requested

    while true
        if length(launched) == 0
            if istaskdone(t)
                break
            end
            @schedule (sleep(1); notify(launch_ntfy))
            wait(launch_ntfy)
        end

        if length(launched) > 0
            wconfig = shift!(launched)
            w = connect_n_create_worker(manager, get_next_pid(), wconfig)
            rr = setup_worker(PGRP, w)
            cnt = get(w.config.count, 1)
            if (cnt == :auto) || (cnt > 1)
                push!(workers_with_additional, (w, rr))
            end

            push!(rr_launched, rr)
        end
    end

    # Perform the launch of additional workers in parallel.
    additional_workers = []     # List of workers launched via the "additional" method
    @sync begin
        for (w, rr) in workers_with_additional
            let w=w, rr=rr
                @async begin
                    wait(rr)  # :cpu_cores below is set only after we get a setup complete
                              # message from the new worker.
                    cnt = get(w.config.count)
                    if cnt == :auto
                        cnt = get(w.config.environ)[:cpu_cores]
                    end
                    cnt = cnt - 1   # Removing self from the requested number

                    exename = get(w.config.exename)
                    exeflags = get(w.config.exeflags, ``)
                    cmd = `$exename $exeflags`

                    npids = [get_next_pid() for x in 1:cnt]
                    new_workers = remotecall_fetch(w.id, launch_additional, cnt, npids, cmd)
                    push!(additional_workers, (w, new_workers))
                end
            end
        end
    end

    # connect each of the additional workers with each other
    process_additional(additional_workers, rr_launched)
end

function process_additional(additional_workers, rr_launched::Array)
    # keyword argument `max_parallel` is only relevant for concurrent ssh connections to a unique host
    # Post launch, ssh from master to workers is used only if tunnel is true

    while length(additional_workers) > 0
        all_new_workers=[]
        for (w_initial, new_workers) in additional_workers
            num_new_w = length(new_workers)
            tunnel = get(w_initial.config.tunnel, false)
            maxp = get(w_initial.config.max_parallel, 0)

            if tunnel && (maxp > 0)
                num_in_p = min(maxp, num_new_w)
            else
                num_in_p = num_new_w   # Do not rate-limit connect
            end

            for i in 1:num_in_p
                (pid, bind_addr, port) = shift!(new_workers)

                wconfig = WorkerConfig()
                for x in [:host, :tunnel, :sshflags, :exeflags, :exename]
                    setfield!(wconfig, x, getfield(w_initial.config, x))
                end
                wconfig.bind_addr = bind_addr
                wconfig.port = port

                new_w = connect_n_create_worker(w_initial.manager, pid, wconfig)
                push!(all_new_workers, new_w)
            end
        end

        rr_list=[]
        for new_w in all_new_workers
            rr=setup_worker(PGRP, new_w)
            push!(rr_list, rr)
            push!(rr_launched, rr)
        end

        # It is important to wait for all of newly launched workers to finish
        # connection setup, so that all the workers are aware of all other workers
        [wait(rr) for rr in rr_list]

        filter!(x->((w_initial, new_workers) = x; length(new_workers) > 0), additional_workers)
    end
end

function connect_n_create_worker(manager, pid, wconfig)
    # initiate a connect. Does not wait for connection completion in case of TCP.
    (r_s, w_s) = connect(manager, pid, wconfig)

    w = Worker(pid, r_s, w_s, manager, wconfig)
    register_worker(w)
    # install a finalizer to perform cleanup if necessary
    finalizer(w, (w)->if myid() == 1 manage(w.manager, w.id, w.config, :finalize) end)
    w
end

function setup_worker(pg::ProcessGroup, w)
    # only node 1 can add new nodes, since nobody else has the full list of address:port
    assert(LPROC.id == 1)

    # set when the new worker has finshed connections with all other workers
    rr_join = RemoteRef()

    # Start a new task to handle inbound messages from connected worker in master.
    # Also calls `wait_connected` on TCP streams.
    process_messages(w.r_stream, w.w_stream; ntfy_join_complete=rr_join)

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
    # - once master receives a :join_complete it triggers rr_join (signifies that worker setup is complete)
    all_locs = map(x -> isa(x, Worker) ? (get(x.config.connect_at, ()), x.id, isa(x.manager, LocalManager)) : ((), x.id, true), pg.workers)
    send_msg_now(w, :join_pgrp, w.id, all_locs, isa(w.manager, LocalManager))

    @schedule manage(w.manager, w.id, w.config, :register)
    rr_join
end


# Called on the first worker on a remote host. Used to optimize launching
# of multiple workers on a remote host (to leverage multi-core)
function launch_additional(np::Integer, pids::Array, cmd::Cmd)
    assert(np == length(pids))

    io_objs = cell(np)
    addresses = cell(np)

    for i in 1:np
        io, pobj = open(detach(cmd), "r")
        io_objs[i] = io
    end

    for (i,io) in enumerate(io_objs)
        (host, port) = read_worker_host_port(io)
        addresses[i] = (pids[i], host, port)

        let io=io, pid=pids[i]
            redirect_worker_output("$pid", io)
        end
    end

    addresses
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

function at_each(f, args...)
    for w in PGRP.workers
        sync_add(remotecall(w.id, f, args...))
    end
end

macro everywhere(ex)
    quote
        @sync begin
            at_each(()->eval(Main,$(Expr(:quote,ex))))
        end
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
function pmap(f, lsts...; err_retry=true, err_stop=false, pids = workers())
    len = length(lsts)

    results = Dict{Int,Any}()

    retryqueue = []
    task_in_err = false
    is_task_in_error() = task_in_err
    set_task_in_error() = (task_in_err = true)

    nextidx = 0
    getnextidx() = (nextidx += 1)

    states = [start(lsts[idx]) for idx in 1:len]
    function getnext_tasklet()
        if is_task_in_error() && err_stop
            return nothing
        elseif !any(idx->done(lsts[idx],states[idx]), 1:len)
            nxts = [next(lsts[idx],states[idx]) for idx in 1:len]
            for idx in 1:len; states[idx] = nxts[idx][2]; end
            nxtvals = [x[1] for x in nxts]
            return (getnextidx(), nxtvals)
        elseif !isempty(retryqueue)
            return shift!(retryqueue)
        else
            return nothing
        end
    end

    @sync begin
        for wpid in pids
            @async begin
                tasklet = getnext_tasklet()
                while (tasklet != nothing)
                    (idx, fvals) = tasklet
                    try
                        result = remotecall_fetch(wpid, f, fvals...)
                        if isa(result, Exception)
                            ((wpid == myid()) ? rethrow(result) : throw(result))
                        else
                            results[idx] = result
                        end
                    catch ex
                        if err_retry
                            push!(retryqueue, (idx,fvals, ex))
                        else
                            results[idx] = ex
                        end
                        set_task_in_error()
                        break # remove this worker from accepting any more tasks
                    end

                    tasklet = getnext_tasklet()
                end
            end
        end
    end

    for failure in retryqueue
        results[failure[1]] = failure[3]
    end
    [results[x] for x in 1:nextidx]
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
    results = cell(length(chunks))
    for i in 1:length(chunks)
        results[i] = @spawn f(first(chunks[i]), last(chunks[i]))
    end
    mapreduce(fetch, reducer, results)
end

function pfor(f, N::Int)
    [@spawn f(first(c), last(c)) for c in splitrange(N, nworkers())]
end

function make_preduce_body(reducer, var, body, ran)
    localize_vars(
    quote
        function (lo::Int, hi::Int)
            R = $(esc(ran))
            $(esc(var)) = R[lo]
            ac = $(esc(body))
            if lo != hi
                for $(esc(var)) in R[(lo+1):hi]
                    ac = ($(esc(reducer)))(ac, $(esc(body)))
                end
            end
            ac
        end
    end
                  )
end

function make_pfor_body(var, body, ran)
    localize_vars(
    quote
        function (lo::Int, hi::Int)
            for $(esc(var)) in ($(esc(ran)))[lo:hi]
                $(esc(body))
            end
        end
    end
                  )
end

macro parallel(args...)
    na = length(args)
    if na==1
        loop = args[1]
        if isa(loop,Expr) && loop.head === :comprehension
            ex = loop.args[1]
            loop.args[1] = esc(ex)
            nd = length(loop.args)-1
            ranges = map(e->esc(e.args[2]), loop.args[2:end])
            for i=1:nd
                var = loop.args[1+i].args[1]
                loop.args[1+i] = :( $(esc(var)) = ($(ranges[i]))[I[$i]] )
            end
            return :( DArray((I::(UnitRange{Int}...))->($loop),
                             tuple($(map(r->:(length($r)),ranges)...))) )
        end
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
        quote
            pfor($(make_pfor_body(var, body, r)), length($(esc(r))))
        end
    else
        quote
            preduce($(esc(reducer)),
                    $(make_preduce_body(reducer, var, body, r)), length($(esc(r))))
        end
    end
end


function check_master_connect(timeout)
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
            isready(done) && stop_timer(aw)
        end
    end

    if !testcb()
        t = Timer(timercb)
        start_timer(t, pollint, pollint)
        ret = fetch(done)
        stop_timer(t)
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
            warn_once("Parallel networking unoptimized ( Error enabling TCP_QUICKACK : ", strerror(errno()), " )")
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
