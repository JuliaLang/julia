## multi.jl - multiprocessing
##
## julia starts with one process, and processors can be added using:
##   addprocs(n)                         using exec
##   addprocs({"host1","host2",...})     using remote execution
##   addprocs_scyld(n)                   using Scyld ClusterWare
##   addprocs_sge(n)                     using Sun Grid Engine batch queue
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
## put(r, val) - store a value to an uninitialized RemoteRef
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
# * take() to empty a Ref (full/empty variables)
# * have put() wait on non-empty Refs
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

type Worker
    host::ByteString
    port::Uint16
    socket::TcpSocket
    sendbuf::IOBuffer
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    id::Int
    gcflag::Bool
    
    Worker(host::String, port::Integer, sock::TcpSocket, id::Int) =
        new(bytestring(host), uint16(port), sock, IOBuffer(), {}, {}, id, false)
end
Worker(host::String, port::Integer, sock::TcpSocket) =
    Worker(host, port, sock, 0)
Worker(host::String, port::Integer) =
    Worker(host, port, connect(host,uint16(port)))
Worker(host::String, port::Integer, tunnel_user::String, sshflags) =
    Worker(host, port, connect("localhost",
                               ssh_tunnel(tunnel_user, host, uint16(port), sshflags)))


function send_msg_now(w::Worker, kind, args...)
    send_msg_(w, kind, args, true)
end

function send_msg(w::Worker, kind, args...)
    send_msg_(w, kind, args, false)
end

function flush_gc_msgs(w::Worker)
    w.gcflag = false
    msgs = w.add_msgs
    if !isempty(msgs)
        empty!(w.add_msgs)
        remote_do(w, add_clients, msgs...)
    end

    msgs = w.del_msgs
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
end

const LPROC = LocalProcess(0)

const map_pid_wrkr = Dict{Int, Union(Worker, LocalProcess)}()
const map_sock_wrkr = Dict{Socket, Union(Worker, LocalProcess)}()

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

function add_workers(pg::ProcessGroup, w::Array{Any,1})
    # NOTE: currently only node 1 can add new nodes, since nobody else
    # has the full list of address:port
    assert(LPROC.id == 1)
    for i=1:length(w)
        w[i].id = get_next_pid()
        register_worker(w[i])
        create_message_handler_loop(w[i].socket)
    end
    all_locs = map(x -> isa(x, Worker) ? (x.host,x.port, x.id) : ("", 0, x.id), pg.workers)
    for i=1:length(w)
        send_msg_now(w[i], :join_pgrp, w[i].id, all_locs)
    end
    :ok
end

myid() = LPROC.id

nprocs() = length(PGRP.workers)
function nworkers() 
    n = nprocs()
    n == 1 ? 1 : n-1
end

procs() = Int[x.id for x in PGRP.workers]

function workers()
    allp = procs()
    if nprocs() == 1
       allp 
    else
       filter(x -> x != 1, allp)
    end
end

function rmprocs(args...)
    # Only pid 1 can add and remove processes
    if myid() != 1
        error("only process 1 can add and remove processes")
    end
    
    for i in [args...]
        if haskey(map_pid_wrkr, i)
            remotecall(i, exit)
        end
    end
end


worker_from_id(i) = worker_from_id(PGRP, i)
function worker_from_id(pg::ProcessGroup, i)
#   Processes with pids > ours, have to connect to us. May not have happened. Wait for some time.
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
    if isa(w, Worker) map_sock_wrkr[w.socket] = w end
end

deregister_worker(pid) = deregister_worker(PGRP, pid)
function deregister_worker(pg, pid)
    pg.workers = filter(x -> !(x.id == pid), pg.workers)
    w = delete!(map_pid_wrkr, pid, nothing)
    if isa(w, Worker) delete!(map_sock_wrkr, w.socket) end
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

    global WeakRemoteRef
    function WeakRemoteRef(w, wh, id)
        return new(w, wh, id)
    end

    function WeakRemoteRef(pid::Integer)
        rr = WeakRemoteRef(pid, myid(), REQ_ID)
        REQ_ID += 1
        if mod(REQ_ID,200) == 0
            gc()
        end
        rr
    end

    WeakRemoteRef(w::LocalProcess) = WeakRemoteRef(myid())
    WeakRemoteRef(w::Worker) = WeakRemoteRef(w.id)
    WeakRemoteRef() = WeakRemoteRef(myid())
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
        add!(rv.clientset, id[1])
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

any_gc_flag = false

function send_del_client(rr::RemoteRef)
    if rr.where == myid()
        del_client(rr2id(rr), myid())
    else
        w = worker_from_id(rr.where)
        push!(w.del_msgs, (rr2id(rr), myid()))
        w.gcflag = true
        global any_gc_flag = true
    end
end

function add_client(id, client)
    rv = lookup_ref(id)
    add!(rv.clientset, client)
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
        push!(w.add_msgs, (rr2id(rr), i))
        w.gcflag = true
        global any_gc_flag = true
    end
end

function serialize(s, rr::RemoteRef)
    i = worker_id_from_socket(s)
    if i != -1
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

# wait on a local proxy condition for a remote ref
function wait_full(rr::RemoteRef)
    oid = rr2id(rr)
    cv = get(Waiting, oid, false)
    if cv === false
        cv = Condition()
        Waiting[oid] = cv
    end
    wait(cv)
end

# data stored by the owner of a RemoteRef
type RemoteValue
    done::Bool
    result
    full::Condition   # waiting for a value
    empty::Condition  # waiting for value to be removed
    clientset::IntSet

    RemoteValue() = new(false, nothing, Condition(), Condition(), IntSet())
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

## core messages: do, call, fetch, wait, ref, put ##

function run_work_thunk(rv::RemoteValue, thunk)
    local result
    try
        result = thunk()
    catch err
        print(STDERR, "exception on ", myid(), ": ")
        display_error(err,catch_backtrace())
        println(STDERR)
        result = err
    end
    put(rv, result)
end

function schedule_call(rid, thunk)
    rv = RemoteValue()
    (PGRP::ProcessGroup).refs[rid] = rv
    add!(rv.clientset, rid[1])
    enq_work(@task(run_work_thunk(rv,thunk)))
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
    rr = WeakRemoteRef(w)
    oid = rr2id(rr)
    rv = schedule_call(oid, local_remotecall_thunk(f,args))
    wait_full(rv)
end

function remotecall_fetch(w::Worker, f, args...)
    # can be weak, because the program will have no way to refer to the Ref
    # itself, it only gets the result.
    rr = WeakRemoteRef(w)
    oid = rr2id(rr)
    send_msg(w, :call_fetch, oid, f, args)
    wait_full(rr)
end

remotecall_fetch(id::Integer, f, args...) =
    remotecall_fetch(worker_from_id(id), f, args...)

# faster version of wait(remotecall(...))
remotecall_wait(w::LocalProcess, f, args...) = wait(remotecall(w,f,args...))

function remotecall_wait(w::Worker, f, args...)
    rr = RemoteRef(w)
    oid = rr2id(rr)
    send_msg(w, :call_wait, oid, f, args)
    wait_full(rr)
    rr
end

remotecall_wait(id::Integer, f, args...) =
    remotecall_wait(worker_from_id(id), f, args...)

function remote_do(w::LocalProcess, f, args...)
    # the LocalProcess version just performs in local memory what a worker
    # does when it gets a :do message.
    # same for other messages on LocalProcess.
    thk = local_remotecall_thunk(f, args)
    enq_work(Task(thk))
    nothing
end

function remote_do(w::Worker, f, args...)
    send_msg(w, :do, f, args)
    nothing
end

remote_do(id::Integer, f, args...) = remote_do(worker_from_id(id), f, args...)

function sync_msg(verb::Symbol, r::RemoteRef)
    pg = (PGRP::ProcessGroup)
    oid = rr2id(r)
    if r.where==myid() || isa(worker_from_id(r.where), LocalProcess)
        rv = lookup_ref(oid)
        wait_full(rv)
        return is(verb,:fetch) ? work_result(rv) : r
    end
    send_msg(worker_from_id(r.where), verb, oid)
    # yield to event loop, return here when answer arrives
    v = wait_full(r)
    return is(verb,:fetch) ? v : r
end

wait(r::RemoteRef) = sync_msg(:wait, r)
fetch(r::RemoteRef) = sync_msg(:fetch, r)
fetch(x::ANY) = x

# storing a value to a Ref
put_ref(rid, v) = put(lookup_ref(rid), v)
function put(rv::RemoteValue, val::ANY)
    wait_empty(rv)
    rv.result = val
    rv.done = true
    notify_full(rv)
end

function put(rr::RemoteRef, val::ANY)
    rid = rr2id(rr)
    if rr.where == myid()
        put_ref(rid, val)
    else
        remotecall_fetch(rr.where, put_ref, rid, val)
    end
    val
end

take_ref(rid) = take(lookup_ref(rid))
function take(rv::RemoteValue)
    wait_full(rv)
    val = rv.result
    rv.done = false
    notify_empty(rv)
    val
end

function take(rr::RemoteRef)
    rid = rr2id(rr)
    if rr.where == myid()
        take_ref(rid)
    else
        remotecall_fetch(rr.where, take_ref, rid)
    end
end

## work queue ##

function enq_work(t::Task)
    ccall(:uv_stop,Void,(Ptr{Void},),eventloop())
    unshift!(Workqueue, t)
end

function perform_work()
    perform_work(pop!(Workqueue))
end

function perform_work(t::Task)
    if !isdefined(t, :result)
        # starting new task
        yieldto(t)
    else
        # continuing interrupted work item
        arg = t.result
        t.result = nothing
        t.runnable = true
        yieldto(t, arg)
    end
    t = current_task().last
    if !istaskdone(t) && t.runnable
        # still runnable; return to queue
        enq_work(t)
    end
end

function deliver_result(sock::IO, msg, oid, value)
    #print("$(myid()) sending result $oid\n")
    if is(msg,:fetch) || is(msg,:call_fetch)
        val = value
    else
        val = oid
    end
    try
        send_msg_now(sock, :result, msg, oid, val)
    catch err
        # send exception in case of serialization error; otherwise
        # request side would hang.
        send_msg_now(sock, :result, msg, oid, err)
    end
end

# notify waiters that a certain job has finished or Ref has been emptied
notify_full (rv::RemoteValue) = notify(rv.full, work_result(rv))
notify_empty(rv::RemoteValue) = notify(rv.empty)

## message event handlers ##

# activity on accept fd
function accept_handler(server::TcpServer, status::Int32)
    if status == -1
        error("An error occured during the creation of the server")
    end
    client = accept_nonblock(server)
    create_message_handler_loop(client)
end

type DisconnectException <: Exception end

# schedule an expression to run asynchronously, with minimal ceremony
macro schedule(expr)
    expr = localize_vars(:(()->($expr)), false)
    :(enq_work(Task($(esc(expr)))))
end

function create_message_handler_loop(sock::AsyncStream) #returns immediately
    enq_work(@task begin
        global PGRP
        #println("message_handler_loop")
        start_reading(sock)
        wait_connected(sock)
        try
            while true
                msg = deserialize(sock)
                #println("got msg: ",msg)
                # handle message
                if is(msg, :call) || is(msg, :call_fetch) || is(msg, :call_wait)
                    id = deserialize(sock)
                    f0 = deserialize(sock)
                    args0 = deserialize(sock)
                    #print("$(myid()) got call $id\n")
                    let f=f0, args=args0, m=msg, rid=id
                        if m === :call_fetch || m === :call_wait
                            schedule_call(id, ()->(v = f(args...);
                                                   deliver_result(sock,m,rid,v);
                                                   v))
                        else
                            schedule_call(id, ()->f(args...))
                        end
                    end
                elseif is(msg, :do)
                    f = deserialize(sock)
                    args = deserialize(sock)
                    #print("got args: $args\n")
                    let func=f, ar=args
                        enq_work(@task(run_work_thunk(RemoteValue(),
                                                      ()->apply(func, ar))))
                    end
                elseif is(msg, :result)
                    # used to deliver result of wait or fetch
                    mkind = deserialize(sock)
                    oid = deserialize(sock)
                    #print("$(myid()) got $msg $oid\n")
                    val = deserialize(sock)
                    cv = get(Waiting, oid, false)
                    if cv !== false
                        notify(cv, val)
                        if isempty(cv.waitq)
                            delete!(Waiting, oid)
                        end
                    end
                elseif is(msg, :identify_socket)
                    otherid = deserialize(sock)
                    register_worker(Worker("", 0, sock, otherid))
                elseif is(msg, :join_pgrp)
                    # first connection; get process group info from client
                    self_pid = LPROC.id = deserialize(sock)
                    locs = deserialize(sock)
                    #print("\nLocation: ",locs,"\nId:",myid(),"\n")
                    # joining existing process group
                    
                    register_worker(Worker("", 0, sock, 1))
                    register_worker(LPROC)
                    
                    for (rhost, rport, rpid) in locs
                        if (rpid < self_pid) && (!(rpid == 1))
                            # Connect to them
                            w = Worker(rhost, rport)
                            w.id = rpid
                            register_worker(w)
                            create_message_handler_loop(w.socket)
                            send_msg_now(w, :identify_socket, self_pid)
                        else
                            # Others will connect to us. Don't do anything just yet
                            continue
                        end
                        
                    end
                else
                    # the synchronization messages
                    oid = deserialize(sock)::(Int,Int)
                    #print("$(myid()) got $msg $oid\n")
                    rv = lookup_ref(oid)
                    if rv.done
                        deliver_result(sock, msg, oid, work_result(rv))
                    else
                        # TODO: should store the worker here, not the socket,
                        # so we don't need to look up the worker later
                        @schedule begin
                            deliver_result(sock, msg, oid, wait_full(rv))
                        end
                    end
                end
            end # end of while
        catch e
            iderr = worker_id_from_socket(sock)
            # If pid 1 is disconnected, commit harakiri
            if (iderr == 1) exit() end
            
            if isa(e,EOFError)
                deregister_worker(iderr)
                
                if (myid() == 1) println("Worker $iderr terminated.") end
                
                #TODO : Notify all RemoteRefs linked to this Worker who just died....
                # How?
                
                return nothing
            else
                # TODO : Treat any exception as death of node / major screw-up and cleanup?
                rethrow(e)
            end
        end
    end)
end

function disable_parallel_libs()
    blas_set_num_threads(1)
end

## worker creation and setup ##

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
start_worker() = start_worker(STDOUT)
function start_worker(out::IO)
    default_port = uint16(9009)
    (actual_port,sock) = open_any_tcp_port(accept_handler,default_port) 
    write(out, "julia_worker:")  # print header
    write(out, "$(dec(actual_port))#") # print port
    write(out, bind_addr)      #TODO: print hostname
    write(out, '\n')
    # close STDIN; workers will not use it
    #close(STDIN)

    disable_parallel_libs()

    ccall(:jl_install_sigint_handler, Void, ())

    global const Scheduler = current_task()

    try
        event_loop(false)
    catch err
        print(STDERR, "unhandled exception on $(myid()): $(err)\nexiting.\n")
    end

    close(sock)
    exit(0)
end

function start_remote_workers(machines, cmds, tunnel=false, sshflags=``)
    n = length(cmds)
    outs = cell(n)
    w = cell(n)
    for i=1:n
        outs[i],_ = readsfrom(cmds[i])
        outs[i].line_buffered = true
    end
    for i=1:n
        local hostname::String, port::Int16
        stream = outs[i]
        stream.line_buffered = true
        while true
            conninfo = readline(stream)
            private_hostname, port = parse_connection_info(conninfo)
            if private_hostname != ""
                break
            end
        end
        
        s = split(machines[i],'@')
        if length(s) > 1
            user = s[1]
            hostname = s[2]
        else
            user = ENV["USER"]
            hostname = s[1]
        end
        
        if tunnel
            w[i] = Worker(hostname, port, user, sshflags)
        else
            w[i] = Worker(hostname, port)
        end
        let wrker = w[i]
            # redirect console output from workers to the client's stdout:
            start_reading(stream,function(stream::AsyncStream,nread::Int)
                if nread>0
                    try
                        line = readbytes(stream.buffer, nread)
                        print("\tFrom worker $(wrker.id):\t",line)
                    catch err
                        println(STDERR,"\tError parsing reply from worker $(wrker.id):\t",err)
                        return false
                    end
                end
                true
            end)
        end
    end
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
function ssh_tunnel(user, host, port, sshflags)
    global tunnel_port
    localp = tunnel_port::Int
    while !success(detach(`ssh -f -o ExitOnForwardFailure=yes $sshflags $(user)@$host -L $localp:$host:$(int(port)) -N`)) && localp < 10000
        localp += 1
    end
    
    if localp >= 10000
        error("Unable to assign a local tunnel port between 9201 and 10000")
    end
    
    tunnel_port = localp+1
    localp
end

#function worker_ssh_cmd(host, key)
#    `ssh -i $key -n $host "bash -l -c \"cd $JULIA_HOME && ./julia-release-basic --worker\""`
#end

# start and connect to processes via SSH.
# optionally through an SSH tunnel.
# the tunnel is only used from the head (process 1); the nodes are assumed
# to be mutually reachable without a tunnel, as is often the case in a cluster.
function addprocs(machines::AbstractVector;
                  tunnel=false, dir=JULIA_HOME, exename="./julia-release-basic", sshflags::Cmd=``)
    add_workers(PGRP,
        start_remote_workers(machines,
            map(m->detach(`ssh -n $sshflags $m "bash -l -c \"cd $dir && $exename --worker\""`),
                machines),
            tunnel, sshflags))
end

#function addprocs_ssh(machines, keys)
#    if !(isa(keys, Array)) && isa(machines,Array)
#        key = keys
#        keys = [ key for x = 1:length(machines)]
#        cmdargs = { {machines[x],keys[x]} for x = 1:length(machines)}
#    else
#        cmdargs = {{machines,keys}}
#    end #if/else
#    add_workers(PGRP, start_remote_workers(machines, map(x->worker_ssh_cmd(x[1],x[2]), cmdargs)))
#end

worker_local_cmd() = `$JULIA_HOME/julia-release-basic --bind-to $bind_addr --worker`

function addprocs(np::Integer)
    disable_parallel_libs()
    add_workers(PGRP, start_remote_workers({ "localhost" for i=1:np },
                                           { worker_local_cmd() for i=1:np }))
end

function start_scyld_workers(np::Integer)
    home = JULIA_HOME
    beomap_cmd = `beomap --no-local --np $np`
    out,beomap_proc = readsfrom(beomap_cmd)
    wait(beomap_proc)
    if !success(beomap_proc)
        error("node availability inaccessible (could not run beomap)")
    end
    nodes = split(chomp(readline(out)),':')
    outs = cell(np)
    for (i,node) in enumerate(nodes)
        cmd = detach(`bpsh $node sh -l -c "cd $home && ./julia-release-basic --worker"`)
        outs[i],_ = readsfrom(cmd)
        outs[i].line_buffered = true
    end
    workers = cell(np)
    for (i,stream) in enumerate(outs)
        local hostname::String, port::Int16
        stream.line_buffered = true
        while true
            conninfo = readline(stream)
            hostname, port = parse_connection_info(conninfo)
            if hostname != ""
                break
            end
        end
        workers[i] = Worker(hostname, port)
        let worker = workers[i]
            # redirect console output from workers to the client's stdout:
            start_reading(stream,function(stream::AsyncStream,nread::Int)
                if(nread>0)
                    try
                        line = readbytes(stream.buffer, nread)
                        print("\tFrom worker $(worker.id):\t",line)
                    catch err
                        println(STDERR,"\tError parsing reply from worker $(worker.id):\t",err)
                        return false
                    end
                end
                true
            end)
        end
    end
    workers
end

function addprocs_scyld(np::Integer)
    disable_parallel_libs()
    add_workers(PGRP, start_scyld_workers(np))
end

function start_sge_workers(n)
    home = JULIA_HOME
    sgedir = joinpath(pwd(),"SGE")
    run(`mkdir -p $sgedir`)
    qsub_cmd = `echo $home/julia-release-basic --worker` |> `qsub -N JULIA -terse -cwd -j y -o $sgedir -t 1:$n`
    out,qsub_proc = readsfrom(qsub_cmd)
    if !success(qsub_proc)
        error("batch queue not available (could not run qsub)")
    end
    id = chomp(split(readline(out),'.')[1])
    println("job id is $id")
    print("waiting for job to start");
    workers = cell(n)
    for i=1:n
        # wait for each output stream file to get created
        fname = "$sgedir/JULIA.o$(id).$(i)"
        local fl, hostname, port
        fexists = false
        sleep(0.5)
        while !fexists
            try
                fl = open(fname)
                try
                    conninfo = readline(fl)
                    hostname, port = parse_connection_info(conninfo)
                finally
                    close(fl)
                end
                fexists = (hostname != "")
            catch
                print(".");
                sleep(0.5)
            end
        end
        #print("hostname=$hostname, port=$port\n")
        workers[i] = Worker(hostname, port)
    end
    print("\n")
    workers
end

addprocs_sge(n) = add_workers(PGRP, start_sge_workers(n))

## higher-level functions: spawn, pmap, pfor, etc. ##

sync_begin() = task_local_storage(:SPAWNS, ({}, get(task_local_storage(), :SPAWNS, ())))

function sync_end()
    spawns = get(task_local_storage(), :SPAWNS, ())
    if is(spawns,())
        error("sync_end() without sync_begin()")
    end
    refs = spawns[1]
    task_local_storage(:SPAWNS, spawns[2])
    for r in refs
        wait(r)
    end
end

macro sync(block)
    quote
        sync_begin()
        v = $(esc(block))
        sync_end()
        v
    end
end

function sync_add(r)
    spawns = get(task_local_storage(), :SPAWNS, ())
    if !is(spawns,())
        push!(spawns[1], r)
    end
    r
end

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
            if nextidx > nprocs()
               p = PGRP.workers[1].id
               nextidx = 2
            else 
               p = PGRP.workers[nextidx].id
               nextidx += 1
            end
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


function spawnlocal(thunk)
    rr = RemoteRef(myid())
    sync_add(rr)
    rid = rr2id(rr)
    rv = RemoteValue()
    (PGRP::ProcessGroup).refs[rid] = rv
    add!(rv.clientset, rid[1])
    # add to the *front* of the queue, work first
    push!(Workqueue, @task(run_work_thunk(rv,thunk)))
    yield()
    rr
end

macro async(expr)
    expr = localize_vars(:(()->($expr)), false)
    :(spawnlocal($(esc(expr))))
end

macro spawnlocal(expr)
    warn_once("@spawnlocal is deprecated, use @async instead.")
    :(@async $(esc(expr)))
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
function pmap(f, lsts...)
    np = nprocs()
    n = length(lsts[1])
    results = cell(n)
    i = 1
    # function to produce the next work item from the queue.
    # in this case it's just an index.
    nextidx() = (idx=i; i+=1; idx)
    @sync begin
        for p=1:np
            wpid = PGRP.workers[p].id
            if wpid != myid() || np == 1
                @async begin
                    while true
                        idx = nextidx()
                        if idx > n
                            break
                        end
                        results[idx] = remotecall_fetch(wpid, f,
                                                        map(L->L[idx], lsts)...)
                    end
                end
            end
        end
    end
    results
end

# Statically split range [1,N] into equal sized chunks for np processors
function splitrange(N::Int, np::Int)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Array(Range1{Int}, nchunks)
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
    chunks = splitrange(N, nprocs())
    results = cell(length(chunks))
    for i in 1:length(chunks)
        results[i] = @spawn f(first(chunks[i]), last(chunks[i]))
    end
    mapreduce(fetch, reducer, results)
end

function pfor(f, N::Int)
    for c in splitrange(N, nprocs())
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
            ranges = map(e->esc(e.args[2]), loop.args[2:])
            for i=1:nd
                var = loop.args[1+i].args[1]
                loop.args[1+i] = :( $(esc(var)) = ($(ranges[i]))[I[$i]] )
            end
            return :( DArray((I::(Range1{Int}...))->($loop),
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

## event processing, I/O and work scheduling ##

function yield(args...)
    ct = current_task()
    # preserve Task.last across calls to the scheduler
    prev = ct.last
    v = yieldto(Scheduler, args...)
    ct.last = prev
    return v
end

function event_loop(isclient)
    iserr, lasterr, bt = false, nothing, {}
    while true
        try
            if iserr
                display_error(lasterr, bt)
                println(STDERR)
                iserr, lasterr, bt = false, nothing, {}
            else
                while true
                    if isempty(Workqueue)
                        if any_gc_flag
                            flush_gc_msgs()
                        end
                        process_events(true)
                    else
                        perform_work()
                        process_events(false)
                    end
                end
            end
        catch err
            iserr, lasterr = true, err
            bt = catch_backtrace()
            if isa(err,DisconnectException)
                # TODO: wake up tasks waiting for failed process
                if !isclient
                    return
                end
            elseif isclient && isa(err,InterruptException)
                # root task is waiting for something on client. allow C-C
                # to interrupt.
                interrupt_waiting_task(roottask,err)
                iserr, lasterr = false, nothing
            end
        end
    end
end

# force a task to stop waiting, providing with_value as the value of
# whatever it's waiting for.
function interrupt_waiting_task(t::Task, with_value)
    if !t.runnable
        t.result = with_value
        enq_work(t)
    end
end
