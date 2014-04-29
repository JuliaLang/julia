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
# - more indexing
# * take!() to empty a Ref (full/empty variables)
# * have put!() wait on non-empty Refs
# - removing nodes
# - more dynamic scheduling
# * fetch/wait latency seems to be excessive
# * message aggregation
# - timer events
# - send pings at some interval to detect failed/hung machines
# - integrate event loop with other kinds of i/o (non-messages)
# ? method_missing for waiting (getindex/setindex!/localdata seems to cover a lot)
# * serializing closures
# * recover from i/o errors
# * handle remote execution errors
# * all-to-all communication
# * distributed GC
# * call&wait and call&fetch combined messages
# * aggregate GC messages
# * dynamically adding nodes (then always start with 1 and grow)
# * add readline to event loop
# * GOs/darrays on a subset of nodes

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

type Worker
    host::ByteString
    port::Uint16
    socket::TcpSocket
    sendbuf::IOBuffer
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    id::Int
    gcflag::Bool
    bind_addr::IpAddr
    manage::Function
    config::Dict
    
    Worker(host::String, port::Integer, sock::TcpSocket, id::Int) =
        new(bytestring(host), uint16(port), sock, IOBuffer(), {}, {}, id, false)
end
Worker(host::String, port::Integer, sock::TcpSocket) =
    Worker(host, port, sock, 0)
function Worker(host::String, port::Integer) 
    # Connect to the loopback port if requested host has the same ipaddress as self.
    if host == string(LPROC.bind_addr)
        w = Worker(host, port, connect("127.0.0.1", uint16(port)))
    else
        w = Worker(host, port, connect(host, uint16(port)))
    end
    # Avoid calling getaddrinfo if possible - involves a DNS lookup
    # host may be a stringified ipv4 / ipv6 address or a dns name
    if host == "localhost"
        w.bind_addr = parseip("127.0.0.1")
    else
        try 
            w.bind_addr = parseip(host)
        catch
            w.bind_addr = getaddrinfo(host) 
        end
    end
    w
end
function Worker(host::String, bind_addr::String, port::Integer, tunnel_user::String, sshflags) 
    w = Worker(host, port, connect("localhost",
                               ssh_tunnel(tunnel_user, host, bind_addr, uint16(port), sshflags)))
    w.bind_addr = parseip(bind_addr)
    w
end


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

#TODO: Move to different Thread
function enq_send_req(sock::TcpSocket,buf,now::Bool)
    arr=takebuf_array(buf)
    write(sock,arr)
    #TODO implement "now"
end

function send_msg_(w::Worker, kind, args, now::Bool)
    #println("Sending msg $kind")
    buf = w.sendbuf
    serialize(buf, kind)
    for arg in args
        serialize(buf, arg)
    end

    if !now && w.gcflag
        flush_gc_msgs(w)
    else
        enq_send_req(w.socket,buf,now)
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
    bind_addr::IpAddr
    LocalProcess() = new()
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
    name::String
    workers::Array{Any,1}

    # global references
    refs::Dict

    ProcessGroup(w::Array{Any,1}) = new("pg-default", w, Dict())
end
const PGRP = ProcessGroup({})

get_bind_addr(pid::Integer) = get_bind_addr(worker_from_id(pid))
function get_bind_addr(w::Union(Worker, LocalProcess)) 
    if !isdefined(w, :bind_addr) 
        if w.id != myid()
            w.bind_addr = remotecall_fetch(w.id, get_bind_addr, w.id)
        else
            error("LPROC.bind_addr not defined") # Should never happend since LPROC.bind_addr 
                                           # is defined early on during process init.
        end
    end
    w.bind_addr
end 

function add_workers(pg::ProcessGroup, ws::Array{Any,1})
    # NOTE: currently only node 1 can add new nodes, since nobody else
    # has the full list of address:port
    assert(LPROC.id == 1)
    for w in ws
        w.id = get_next_pid()
        register_worker(w)
        create_message_handler_loop(w.socket) 
    end

    all_locs = map(x -> isa(x, Worker) ? (string(x.bind_addr), x.port, x.id, x.manage == manage_local_worker) : ("", 0, x.id, true), pg.workers)
    
    for w in ws
        send_msg_now(w, :join_pgrp, w.id, all_locs, w.manage == manage_local_worker)
    end
    for w in ws
        @schedule begin
            w.manage(w.id, w.config, :register)
        end
    end 
    [w.id for w in ws]
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
        if (pid == 1) || (map_pid_wrkr[pid].manage == manage_local_worker)
            Int[x.id for x in filter(w -> (w.id==1) || (w.manage == manage_local_worker), PGRP.workers)]
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
    
    for i in [args...]
        if i == 1
            warn("rmprocs: process 1 not removed")
        else
            if haskey(map_pid_wrkr, i)
                push!(rmprocset, i)
                remote_do(i, exit)
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
        if is(s, w.socket) || is(s, w.sendbuf)
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
        map_sock_wrkr[w.socket] = w
        map_sock_wrkr[w.sendbuf] = w
    end
end

deregister_worker(pid) = deregister_worker(PGRP, pid)
function deregister_worker(pg, pid)
    pg.workers = filter(x -> !(x.id == pid), pg.workers)
    w = pop!(map_pid_wrkr, pid, nothing)
    if isa(w, Worker) 
        pop!(map_sock_wrkr, w.socket)
        pop!(map_sock_wrkr, w.sendbuf)
        
        # Notify the cluster manager of this workers death
        if myid() == 1
            w.manage(w.id, w.config, :deregister)
        end
    end
    push!(map_del_wrkr, pid)

    # delete this worker from our RemoteRef client sets
    ids = {}
    tonotify = {}
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

hash(r::RemoteRef) = hash(r.whence)+3*hash(r.id)
isequal(r::RemoteRef, s::RemoteRef) = (r.whence==s.whence && r.id==s.id)

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

# is a ref uninitialized? (for locally-owned refs only)
#function ref_uninitialized(id)
#    wi = lookup_ref(id)
#    !wi.done && is(wi.thunk,bottom_func)
#end
#ref_uninitialized(r::RemoteRef) = (assert(r.where==myid());
#                                   ref_uninitialized(rr2id(r)))

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

# storing a value to a Ref
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

# notify waiters that a certain job has finished or Ref has been emptied
notify_full (rv::RemoteValue) = notify(rv.full, work_result(rv))
notify_empty(rv::RemoteValue) = notify(rv.empty)

## message event handlers ##

# activity on accept fd
function accept_handler(server::TcpServer, status::Int32)
    if status == -1
        error("an error occured during the creation of the server")
    end
    client = accept_nonblock(server)
    create_message_handler_loop(client)
end

function create_message_handler_loop(sock::AsyncStream) #returns immediately
    schedule(@task begin
        global PGRP
        #println("message_handler_loop")
        disable_nagle(sock)
        start_reading(sock)
        wait_connected(sock)
        
        try
            while true
                msg = deserialize(sock)
                # println("got msg: ",msg)
                # handle message
                if is(msg, :call)
                    id = deserialize(sock)
                    #print("$(myid()) got id $id\n")
                    f0 = deserialize(sock)
                    #print("$(myid()) got call $f0\n")
                    args0 = deserialize(sock)
                    #print("$(myid()) got args $args0\n")
                    let f=f0, args=args0
                        schedule_call(id, ()->f(args...))
                    end
                elseif is(msg, :call_fetch)
                    id = deserialize(sock)
                    f = deserialize(sock)
                    args = deserialize(sock)
                    @schedule begin
                        v = run_work_thunk(()->f(args...))
                        deliver_result(sock, msg, id, v)
                        v
                    end
                elseif is(msg, :call_wait)
                    id = deserialize(sock)
                    notify_id = deserialize(sock)
                    f = deserialize(sock)
                    args = deserialize(sock)
                    @schedule begin
                        rv = schedule_call(id, ()->f(args...))
                        deliver_result(sock, msg, notify_id, wait_full(rv))
                    end
                elseif is(msg, :do)
                    f = deserialize(sock)
                    args = deserialize(sock)
                    #print("got args: $args\n")
                    @schedule begin
                        run_work_thunk(RemoteValue(), ()->f(args...))
                    end
                elseif is(msg, :result)
                    # used to deliver result of wait or fetch
                    oid = deserialize(sock)
                    #print("$(myid()) got $msg $oid\n")
                    val = deserialize(sock)
                    put!(lookup_ref(oid), val)
                elseif is(msg, :identify_socket)
                    otherid = deserialize(sock)
                    register_worker(Worker("", 0, sock, otherid))
                elseif is(msg, :join_pgrp)
                    # first connection; get process group info from client
                    self_pid = LPROC.id = deserialize(sock)
                    locs = deserialize(sock)
                    self_is_local = deserialize(sock)
                    #print("\nLocation: ",locs,"\nId:",myid(),"\n")
                    # joining existing process group
                    
                    register_worker(Worker("", 0, sock, 1))
                    register_worker(LPROC)
                    
                    for (rhost, rport, rpid, r_is_local) in locs
                        if (rpid < self_pid) && (!(rpid == 1))
                            # Connect to them
                            if self_is_local && r_is_local
                                # If on localhost, use the loopback address - this addresses
                                # the special case of system suspend wherein the local ip 
                                # may be changed upon system awake.
                                w = Worker("127.0.0.1", rport)
                            else
                                w = Worker(rhost, rport)
                            end
                            w.id = rpid
                            register_worker(w)
                            create_message_handler_loop(w.socket)
                            send_msg_now(w, :identify_socket, self_pid)
                        else
                            # Others will connect to us. Don't do anything just yet
                            continue
                        end
                    end
                end
                
            end # end of while
        catch e
            iderr = worker_id_from_socket(sock)
            # If error occured talking to pid 1, commit harakiri
            if iderr == 1
                if isopen(sock)
                    print(STDERR, "fatal error on ", myid(), ": ")
                    display_error(e, catch_backtrace())
                end
                exit(1)
            end
            
            # Will treat any exception as death of node and cleanup
            # since currently we do not have a mechanism for workers to reconnect
            # to each other on unhandled errors
            deregister_worker(iderr)
            
            if isopen(sock) close(sock) end
            
            if (myid() == 1) 
                global rmprocset
                if in(iderr, rmprocset)
                    delete!(rmprocset, iderr)
                else
                    println("Worker $iderr terminated.") 
                    rethrow(e)
                end
            end

            return nothing
        end
    end)
end

function disable_threaded_libs()
    blas_set_num_threads(1)
end

## worker creation and setup ##

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
start_worker() = start_worker(STDOUT)
function start_worker(out::IO)
    # we only explicitly monitor worker STDOUT on the console, so redirect
    # stderr to stdout so we can see the output.
    # at some point we might want some or all worker output to go to log
    # files instead.
    # Currently disabled since this caused processes to spin instead of
    # exit when process 1 shut down. Don't yet know why.
    #redirect_stderr(STDOUT)

    (actual_port,sock) = listenany(uint16(9009))
    sock.ccb = accept_handler
    print(out, "julia_worker:")  # print header
    print(out, "$(dec(actual_port))#") # print port
    print(out, LPROC.bind_addr)
    print(out, '\n')
    flush(out)
    # close STDIN; workers will not use it
    #close(STDIN)

    disable_threaded_libs()

    ccall(:jl_install_sigint_handler, Void, ())
    disable_nagle(sock)

    try
        check_master_connect(60.0)
        while true; wait(); end
    catch err
        print(STDERR, "unhandled exception on $(myid()): $(err)\nexiting.\n")
    end

    close(sock)
    exit(0)
end

function start_cluster_workers(np::Integer, config::Dict, cman::ClusterManager)
    ws = cell(np)
    
    # Get the cluster manager to launch the instance
    (insttype, instances) = cman.launch(cman, np, config)
    
    if insttype == :io_only
        read_cb_response(inst) = 
        begin
            (host, port) = read_worker_host_port(inst[1])
            inst[1], host, port, host, inst[2]
        end
    elseif  insttype == :io_host
        read_cb_response(inst) = 
        begin
            (bind_addr, port) = read_worker_host_port(inst[1])
            inst[1], bind_addr, port, inst[2], inst[3]
        end
    elseif insttype == :io_host_port
        read_cb_response(inst) = (inst[1], inst[2], inst[3], inst[2], inst[4])
    elseif insttype == :host_port
        read_cb_response(inst) = (nothing, inst[1], inst[2], inst[1], inst[3])
    else
        error("unsupported format from ClusterManager callback")
    end
    
    for i=1:np
        (io, bind_addr, port, pubhost, wconfig) = read_cb_response(instances[i])
        ws[i] = create_worker(bind_addr, port, pubhost, io, wconfig, cman.manage)
    end
    ws
end

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

function create_worker(bind_addr, port, pubhost, stream, config, manage)
    tunnel = config[:tunnel]
    
    s = split(pubhost,'@')
    user = ""
    if length(s) > 1
        user = s[1]
        pubhost = s[2]
    else
        if haskey(ENV, "USER")
            user = ENV["USER"]
        elseif tunnel
            error("USER must be specified either in the environment or as part of the hostname when tunnel option is used")
        end
    end
    
    if tunnel
        sshflags = config[:sshflags]
        w = Worker(pubhost, bind_addr, port, user, sshflags)
    else
        w = Worker(bind_addr, port)
    end
    
    w.config = config
    w.manage = manage
    
    if isa(stream, AsyncStream)
        let wrker = w
            # redirect console output from workers to the client's stdout:
            @async begin
                while !eof(stream)
                    line = readline(stream)
                    print("\tFrom worker $(wrker.id):\t$line")
                end
            end
        end
    end

    # install a finalizer to perform cleanup if necessary
    finalizer(w, (w)->if myid() == 1 w.manage(w.id, w.config, :finalize) end)

    w
end


function parse_connection_info(str)
    m = match(r"^julia_worker:(\d+)#(.*)", str)
    if m != nothing
        (m.captures[2], parseint(Int16, m.captures[1]))
    else
        ("", int16(-1))
    end
end

tunnel_port = 9201
# establish an SSH tunnel to a remote worker
# returns P such that localhost:P connects to host:port
function ssh_tunnel(user, host, bind_addr, port, sshflags)
    global tunnel_port
    localp = tunnel_port::Int
    while !success(detach(`ssh -f -o ExitOnForwardFailure=yes $sshflags $(user)@$host -L $localp:$bind_addr:$(int(port)) sleep 60`)) && localp < 10000
        localp += 1
    end
    
    if localp >= 10000
        error("unable to assign a local tunnel port between 9201 and 10000")
    end
    
    tunnel_port = localp+1
    localp
end


immutable LocalManager <: ClusterManager
    launch::Function
    manage::Function

    LocalManager() = new(launch_local_workers, manage_local_worker)
end

show(io::IO, cman::LocalManager) = println("LocalManager()")

function launch_local_workers(cman::LocalManager, np::Integer, config::Dict)
    dir = config[:dir]
    exename = config[:exename]
    exeflags = config[:exeflags]

    io_objs = cell(np)
    configs = cell(np)

    # start the processes first...
    for i in 1:np
        io, pobj = readsfrom(detach(`$(dir)/$(exename) --bind-to $(LPROC.bind_addr) $exeflags`))
        io_objs[i] = io
        configs[i] = merge(config, {:process => pobj})
    end

    # ...and then read the host:port info. This optimizes overall start times.
    return (:io_only, collect(zip(io_objs, configs)))
end

function manage_local_worker(id::Integer, config::Dict, op::Symbol)
    if op == :interrupt
        kill(config[:process], 2)
    end
end

immutable SSHManager <: ClusterManager
    launch::Function
    manage::Function
    machines::AbstractVector

    SSHManager(; machines=[]) = new(launch_ssh_workers, manage_ssh_worker, machines)
end

show(io::IO, cman::SSHManager) = println("SSHManager(machines=", cman.machines, ")")

function launch_ssh_workers(cman::SSHManager, np::Integer, config::Dict)
    dir = config[:dir]
    exename = config[:exename]
    exeflags_base = config[:exeflags]

    io_objs = cell(np)
    configs = cell(np)

    # start the processes first...

    for i in 1:np
        # machine could be of the format [user@]host[:port] bind_addr
        machine_bind = split(cman.machines[i])
        if length(machine_bind) > 1
            exeflags = `--bind-to $(machine_bind[2]) $exeflags_base`
        else
            exeflags = exeflags_base
        end
        machine_def = machine_bind[1]
        
        machine_def = split(machine_def, ':')
        portopt = length(machine_def) == 2 ? ` -p $(machine_def[2]) ` : ``
        config[:sshflags] = `$(config[:sshflags]) $portopt`
        
        sshflags = config[:sshflags]
        host = cman.machines[i] = machine_def[1]
        
        # Build up the ssh command
        cmd = `cd $dir && $exename $exeflags`          # launch julia
        cmd = `sh -l -c $(shell_escape(cmd))`                   # shell to launch under
        cmd = `ssh -n $sshflags $host $(shell_escape(cmd))`     # use ssh to remote launch
        
        io, pobj = readsfrom(detach(cmd))
        io_objs[i] = io
        configs[i] = merge(config, {:machine => cman.machines[i]})
    end

    # ...and then read the host:port info. This optimizes overall start times.
    # For ssh, the tunnel connection, if any, has to be with the specified machine name.
    # but the port needs to be forwarded to the bound hostname/ip-address
    return (:io_host, collect(zip(io_objs, cman.machines, configs)))
end

function manage_ssh_worker(id::Integer, config::Dict, op::Symbol)
    if op == :interrupt
        if haskey(config, :ospid)
            machine = config[:machine]
            if !success(`ssh -n $(config[:sshflags]) $machine "kill -2 $(config[:ospid])"`)
                println("Error sending a Ctrl-C to julia worker $id on $machine")
            end
        else
            # This state can happen immediately after an addprocs
            println("Worker $id cannot be presently interrupted.")
        end
    elseif op == :register
        config[:ospid] = remotecall_fetch(id, getpid)
    end
end

# start and connect to processes via SSH.
# optionally through an SSH tunnel.
# the tunnel is only used from the head (process 1); the nodes are assumed
# to be mutually reachable without a tunnel, as is often the case in a cluster.
function addprocs_internal(np::Integer;
                  tunnel=false, dir=JULIA_HOME,
                  exename=(ccall(:jl_is_debugbuild,Cint,())==0?"./julia":"./julia-debug"),
                  sshflags::Cmd=``, cman=LocalManager(), exeflags=``)
                  
    config={:dir=>dir, :exename=>exename, :exeflags=>`$exeflags --worker`, :tunnel=>tunnel, :sshflags=>sshflags}
    disable_threaded_libs()
    add_workers(PGRP, start_cluster_workers(np, config, cman))
end

addprocs(np::Integer; kwargs...) = addprocs_internal(np; kwargs...)

function addprocs(machines::AbstractVector; kwargs...)
    cman_defined = any(x -> begin k,v = x; k==:cman end, kwargs)
    if cman_defined
        error("custom cluster managers unsupported on the ssh interface")
    else
        addprocs_internal(length(machines); cman=SSHManager(machines=machines), kwargs...)
    end
end


## higher-level functions: spawn, pmap, pfor, etc. ##

let nextidx = 1
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
    { remotecall(PGRP.workers[(i-1)%np+1].id, f, map(L->L[i], lsts)...) for i = 1:n }
end

pmap(f) = f()

# dynamic scheduling by creating a local task to feed work to each processor
# as it finishes.
# example unbalanced workload:
# rsym(n) = (a=rand(n,n);a*a')
# L = {rsym(200),rsym(1000),rsym(200),rsym(1000),rsym(200),rsym(1000),rsym(200),rsym(1000)};
# pmap(eig, L);
function pmap(f, lsts...; err_retry=true, err_stop=false)
    len = length(lsts)

    results = Dict{Int,Any}()

    retryqueue = {}
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
        for wpid in workers()
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
    for c in splitrange(N, nworkers())
        @spawn f(first(c), last(c))
    end
    nothing
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

## demos ##

# fv(a)=eig(a)[2][2]
# A=randn(800,800);A=A*A';
# pmap(fv, {A,A,A})

#all2all() = at_each(hello_from, myid())

#hello_from(i) = print("message from $i to $(myid())\n")

# monte carlo estimate of pi
# function buffon(niter)
#     nc =
#     @parallel (+) for i=1:niter
#         rand() <= sin(rand()*pi/2) ? 1 : 0
#     end
#     2/(nc/niter)
# end


function check_master_connect(timeout)
    # If we do not have at least process 1 connect to us within timeout
    # we log an error and exit
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
    timercb(aw, status) = begin
        try
            if testcb()
                put!(done, :ok)
            elseif (time() - start) > secs
                put!(done, :timed_out)
            elseif status != 0
                put!(done, :error)
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
        w.manage(w.id, w.config, :interrupt)
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
        if all(p -> (p==1) || (map_pid_wrkr[p].manage == manage_local_worker), pids)
            return true
        else
            first_bind_addr = map_pid_wrkr[pids[1]].bind_addr
            return all(p -> (p != 1) && (map_pid_wrkr[p].bind_addr == first_bind_addr), pids[2:end])
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
