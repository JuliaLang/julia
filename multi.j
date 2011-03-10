## multi.j - multiprocessing
##
## higher-level interface:
##
## ProcessGroup(np) - create a group of np processors
##
## remote_call(w, func, args...) -
##     tell a worker to call a function on the given arguments.
##     returns a RemoteRef to the result.
##
## remote_do(w, f, args...) - remote function call with no result
##
## wait(rr) - wait for a RemoteRef to be finished computing
##
## fetch(rr) - wait for and get the value of a RemoteRef
##
## pmap(func, lst) -
##     call a function on each element of lst (some 1-d thing), in
##     parallel.

## message i/o ##

function send_msg(s::IOStream, buf::IOStream, kind, args)
    truncate(buf, 0)
    serialize(buf, kind)
    for arg=args
        serialize(buf, arg)
    end
    ccall(:jl_enq_send_req, Void, (Ptr{Void}, Ptr{Void}),
          s.ios, buf.ios)
    #ccall(:ios_write_direct, PtrInt, (Ptr{Void}, Ptr{Void}),
    #      s.ios, buf.ios)
end

SENDBUF = memio()
send_msg(s::IOStream, kind, args...) = send_msg(s, SENDBUF, kind, args)

# todo:
# * add readline to event loop
# - GOs/darrays on a subset of nodes
# - dynamically adding nodes (then always start with 1 and just grow/shrink)
# - method_missing for waiting (ref/assign/localdata seems to cover a lot)
# - more dynamic scheduling
# * call&wait and call&fetch combined messages
# * aggregate GC messages
# - fetch/wait latency seems to be excessive
# * recover from i/o errors
# * handle remote execution errors
# * all-to-all communication
# * distributed GC
# - send pings at some interval to detect failed/hung machines
# - integrate event loop with other kinds of i/o (non-messages)
# * serializing closures

## process group creation ##

type Worker
    host::String
    port::Int16
    fd::Int32
    socket::IOStream
    sendbuf::IOStream
    id::Int32
    del_msgs::Dequeue

    Worker() = Worker("localhost", start_local_worker())

    Worker(host, port) = Worker(host, port, connect_to_worker(host,port)...)

    Worker(host, port, fd, sock) = new(host, port, fd, sock, memio(), 0, deq())
end

send_msg(w::Worker, kind, args...) = send_msg(w.socket, w.sendbuf, kind, args)

type LocalProcess
end

type Location
    host::String
    port::Int16
end

type ProcessGroup
    myid::Int32
    workers::Array{Any,1}
    locs::Array{Any,1}
    np::Int32

    # event loop state
    refs
    client

    function ProcessGroup(np::Int)
        # n local workers
        w = { Worker() | i=1:np }
        ProcessGroup(w, np)
    end

    function ProcessGroup(machines)
        # workers on remote machines
        w = start_remote_workers(machines)
        np = length(machines)
        ProcessGroup(w, np)
    end

    function ProcessGroup(myid::Int32, w::Array{Any,1}, locs::Array{Any,1},
                          np::Int32)
        return new(myid, w, locs, np, HashTable(), ())
    end

    function ProcessGroup(w::Array{Any,1}, np::Int)
        # "client side", or initiator of process group
        locs = map(x->Location(x.host,x.port), w)
        global PGRP = ProcessGroup(0, w, locs, np)
        PGRP.client = LocalProcess()
        sockets = HashTable()
        handler = fd->message_handler(fd, sockets)
        for i=1:np
            w[i].id = i
            send_msg(w[i], i, locs)
            sockets[w[i].fd] = w[i].socket
            add_fd_handler(w[i].fd, handler)
        end
        PGRP
    end

    function ProcessGroup(myid, locs, sockets)
        # joining existing process group
        np = length(locs)
        w = cell(np)
        w[myid] = LocalProcess()
        PGRP = ProcessGroup(myid, w, locs, np)
        handler = fd->message_handler(fd, sockets)
        for i=(myid+1):np
            w[i] = Worker(locs[i].host, locs[i].port)
            w[i].id = i
            sockets[w[i].fd] = w[i].socket
            add_fd_handler(w[i].fd, handler)
            send_msg(w[i], :identify_socket, myid)
        end
        PGRP
    end
end

myid() = (global PGRP; isbound(:PGRP) ? (PGRP::ProcessGroup).myid : -1)

function worker_id_from_socket(s)
    global PGRP
    for i=1:PGRP.np
        w = PGRP.workers[i]
        if isa(w,Worker)
            if is(s, w.socket) || is(s, w.sendbuf)
                return i
            end
        end
    end
    if isa(PGRP.client,Worker) && (is(s,PGRP.client.socket) ||
                                   is(s,PGRP.client.sendbuf))
        return 0
    end
    return -1
end

function worker_from_id(id)
    global PGRP
    id==0 ? PGRP.client : PGRP.workers[id]
end

# establish a Worker connection for processes that connected to us
function identify_socket(otherid, fd, sock)
    global PGRP
    i = otherid
    locs = PGRP.locs
    @assert i < PGRP.myid
    PGRP.workers[i] = Worker(locs[i].host, locs[i].port, fd, sock)
    PGRP.workers[i].id = i
    #write(stdout_stream, latin1("$(PGRP.myid) heard from $i\n"))
    ()
end

## remote refs and core messages: do, call, fetch, wait ##

client_refs = WeakKeyHashTable()

type RemoteRef
    where::Int32
    whence::Int32
    id::Int32
    # TODO: cache value if it's fetched, but don't serialize the cached value

    function RemoteRef(w, wh, id)
        r = new(w,wh,id)
        found = key(client_refs, r, false)
        if bool(found)
            return found
        end
        client_refs[r] = true
        finalizer(r, send_del_client)
        r
    end

    global WeakRemoteRef
    function WeakRemoteRef(w, wh, id)
        return new(w, wh, id)
    end
end

hash(r::RemoteRef) = hash(r.whence)+3*hash(r.id)
isequal(r::RemoteRef, s::RemoteRef) = (r.whence==s.whence && r.id==s.id)

rr2id(r::RemoteRef) = (r.whence, r.id)

function lookup_ref(id)
    global PGRP
    wi = get(PGRP.refs, id, ())
    if is(wi, ())
        # first we've heard of this ref
        wi = WorkItem(+)  # this WorkItem is just a place holder
        PGRP.refs[id] = wi
        add(wi.clientset, id[1])
    end
    wi
end

function del_client(id, client)
    global PGRP
    wi = lookup_ref(id)
    del(wi.clientset, client)
    if isempty(wi.clientset)
        del(PGRP.refs, id)
        #print("$(myid()) collected $id\n")
    end
    ()
end

function del_clients(pairs::(Any,Any)...)
    for p=pairs
        del_client(p[1], p[2])
    end
end

function send_del_client(rr::RemoteRef)
    if rr.where == myid()
        del_client(rr2id(rr), myid())
    else
        W = worker_from_id(rr.where)
        push(W.del_msgs, (rr2id(rr), myid()))
        if length(W.del_msgs) >= 16
            #print("sending delete of $(W.del_msgs)\n")
            remote_do(rr.where, del_clients, W.del_msgs...)
            del_all(W.del_msgs)
        end
    end
end

function add_client(id, client)
    global PGRP
    wi = lookup_ref(id)
    add(wi.clientset, client)
    ()
end

function send_add_client(i, rr::RemoteRef)
    if rr.where == myid()
        add_client(rr2id(rr), i)
    elseif i != rr.where
        # don't need to send add_client if the message is already going
        # to the processor that owns the remote ref. it will add_client
        # itself inside deserialize().
        remote_do(rr.where, add_client, rr2id(rr), i)
    end
end

function serialize(s, rr::RemoteRef)
    i = worker_id_from_socket(s)
    if i != -1
        send_add_client(i, rr)
    end
    invoke(serialize, (Any, Any), s, rr)
end

function deserialize(s, t::Type{RemoteRef})
    rr = force(invoke(deserialize, (Any, Type), s, t))
    rid = rr2id(rr)
    where = rr.where
    rr = ()
    function ()
        if where == myid()
            wi = lookup_ref(rid)
            if !wi.done
                wait(WeakRemoteRef(where, rid[1], rid[2]))
            end
            v = wi.result
            # NOTE: this duplicates work_result()
            if isa(v,WeakRef)
                v = v.value
            end
            if isa(v,GlobalObject)
                add_client(rid, myid())
                v = v.local_identity
            end
            return v
        else
            # make sure this rr gets added to the client_refs table
            RemoteRef(where, rid[1], rid[2])
        end
    end
end

REQ_ID = 0

function assign_rr(w::Worker)
    global REQ_ID
    rr = RemoteRef(w.id, myid(), REQ_ID)
    REQ_ID += 1
    rr
end

function assign_rr(w::LocalProcess)
    global REQ_ID
    rr = RemoteRef(myid(), myid(), REQ_ID)
    REQ_ID += 1
    rr
end

assign_rr(id::Int) = assign_rr(worker_from_id(id))
remote_ref(x) = assign_rr(x)

schedule_call(rid, f_thk, args_thk) =
    schedule_call(rid, ()->apply(force(f_thk),force(args_thk)))

function schedule_call(rid, thunk)
    global PGRP
    wi = WorkItem(thunk)
    PGRP.refs[rid] = wi
    add(wi.clientset, rid[1])
    enq_work(wi)
    wi
end

function remote_call(w::LocalProcess, f, args...)
    rr = assign_rr(w)
    schedule_call(rr2id(rr), ()->f(args...))
    rr
end

function remote_call(w::Worker, f, args...)
    rr = assign_rr(w)
    send_msg(w, :call, rr2id(rr), f, args)
    rr
end

remote_call(id::Int, f, args...) = remote_call(worker_from_id(id), f, args...)

# faster version of fetch(remote_call(...))
remote_call_fetch(w::LocalProcess, f, args...) = f(args...)

function remote_call_fetch(w::Worker, f, args...)
    rr = assign_rr(w)
    oid = rr2id(rr)
    send_msg(w, :call_fetch, oid, f, args)
    yieldto(Scheduler, WaitFor(:fetch, oid))
end

remote_call_fetch(id::Int, f, args...) =
    remote_call_fetch(worker_from_id(id), f, args...)

# faster version of wait(remote_call(...))
remote_call_wait(w::LocalProcess, f, args...) = wait(remote_call(w,f,args...))

function remote_call_wait(w::Worker, f, args...)
    rr = assign_rr(w)
    oid = rr2id(rr)
    send_msg(w, :call_wait, oid, f, args)
    yieldto(Scheduler, WaitFor(:wait, oid))
end

remote_call_wait(id::Int, f, args...) =
    remote_call_wait(worker_from_id(id), f, args...)

function remote_do(w::LocalProcess, f, args...)
    # the LocalProcess version just performs in local memory what a worker
    # does when it gets a :do message.
    # same for other messages on LocalProcess.
    enq_work(WorkItem(()->apply(f,args)))
    ()
end

function remote_do(w::Worker, f, args...)
    send_msg(w, :do, f, args)
    ()
end

remote_do(id::Int, f, args...) = remote_do(worker_from_id(id), f, args...)

function sync_msg(verb::Symbol, r::RemoteRef)
    global PGRP
    # NOTE: if other workers request stuff from the client
    # (id 0), they won't get it until the user yield()s somehow.
    # this should be fixed.
    oid = rr2id(r)
    if r.where==myid() || (r.where > 0 && isa(PGRP.workers[r.where],
                                              LocalProcess))
        wi = lookup_ref(oid)
        if wi.done
            return is(verb,:fetch) ? work_result(wi) : r
        else
            # add to WorkItem's notify list
            wi.notify = ((), verb, oid, wi.notify)
        end
    elseif r.where == 0
        send_msg(PGRP.client, verb, oid)
    else
        send_msg(PGRP.workers[r.where], verb, oid)
    end
    # yield to event loop, return here when answer arrives
    v = yieldto(Scheduler, WaitFor(verb, oid))
    return is(verb,:fetch) ? force(v) : r
end

wait(r::RemoteRef) = sync_msg(:wait, r)
fetch(r::RemoteRef) = sync_msg(:fetch, r)
fetch(x) = x

# writing to an uninitialized ref
function put_ref(id, val)
    wi = lookup_ref(rid)
    if wi.done
        error("invalid put()")
    end
    wi.result = val
    wi.done = true
    notify_done(wi)
end

function put(rr::RemoteRef, val)
    rid = rr2id(rr)
    if rr.where == myid()
        put_ref(rid, val)
    else
        remote_do(rr.where, put_ref, rid, val)
    end
    val
end

yield() = yieldto(Scheduler)

## higher-level functions ##

at_each(f, args...) = at_each(PGRP, f, args...)

function at_each(grp::ProcessGroup, f, args...)
    w = grp.workers
    np = grp.np
    for i=1:np
        remote_do(w[i], f, args...)
    end
end

pmap(f, lsts...) = pmap(PGRP, f, lsts...)
pmap(grp::ProcessGroup, f) = f()

function pmap(grp::ProcessGroup, f, lsts...)
    np = grp.np
    { remote_call(grp.workers[(i-1)%np+1], f, map(L->L[i], lsts)...) |
     i = 1:length(lsts[1]) }
end

## worker event loop ##

type WorkItem
    thunk::Function
    task   # the Task working on this item, or ()
    done::Bool
    result
    notify
    argument  # value to pass task next time it is restarted
    clientset::IntSet
    requeue::Bool

    WorkItem(thunk::Function) = new(thunk, (), false, (), (), (), IntSet(64),
                                    true)
    WorkItem(task::Task) = new(()->(), task, false, (), (), (), IntSet(64),
                               true)
end

function work_result(w::WorkItem)
    v = w.result
    if isa(v,WeakRef)
        v = v.value
    end
    if isa(v,GlobalObject)
        v = v.local_identity
    end
    v
end

type FinalValue
    value
end

type WaitFor
    msg::Symbol
    oid
end

# to be used as a re-usable Task for executing thunks
# if a work item finishes, you get a FinalValue. if you get something else,
# the thunk was interrupted and is not done yet.
function taskrunner()
    parent = current_task().parent
    result = ()
    while true
        (parent, thunk) = yieldto(parent, FinalValue(result))
        result = ()
        result = thunk()
    end
end

function deliver_result(sock::IOStream, msg, oid, value)
    if is(msg,:fetch)
        val = value
    else
        @assert is(msg, :wait)
        val = oid
    end
    try
        send_msg(sock, :result, msg, oid, val)
    catch e
        # send exception in case of serialization error; otherwise
        # request side would hang.
        send_msg(sock, :result, msg, oid, e)
    end
end

function deliver_result(sock::(), msg, oid, value_thunk)
    global Waiting
    # restart task that's waiting on oid
    jobs = get(Waiting, oid, ())
    newjobs = ()  # waiting list with one removed
    found = false
    while !is(jobs,())
        if jobs[1]==msg && !found
            found = true
            job = jobs[2]
            job.argument = value_thunk
            enq_work(job)
        else
            newjobs = (jobs[1], jobs[2], newjobs)
        end
        jobs = jobs[3]
    end
    Waiting[oid] = newjobs
    if is(newjobs,())
        del(Waiting, oid)
    end
    ()
end

function enq_work(wi::WorkItem)
    global Workqueue
    enq(Workqueue, wi)
end

enq_work(f::Function) = enq_work(WorkItem(f))
enq_work(t::Task) = enq_work(WorkItem(t))

let runner = ()
global perform_work
function perform_work()
    global Workqueue
    job = pop(Workqueue)
    perform_work(job)
end

function perform_work(job::WorkItem)
    global Waiting, Workqueue
    local result
    try
        if isa(job.task,Task)
            # continuing interrupted work item
            arg = job.argument
            job.argument = ()
            result = yieldto(job.task, arg)
        else
            if is(runner,())
                # make new task to use
                runner = Task(taskrunner, 1024*1024)
                yieldto(runner)
            end
            job.task = runner
            result = yieldto(runner, current_task(), job.thunk)
        end
    catch e
        #show(e)
        print("exception on ", myid(), ": ")
        show(e)
        result = FinalValue(e)
        job.task = ()  # task is toast. would be better to reuse it somehow.
    end
    if isa(result,FinalValue)
        # job done
        job.done = true
        job.result = result.value
    end
    if job.done
        runner = job.task  # Task now free to be shared
        job.task = ()
        # do notifications
        notify_done(job)
    else
        # job interrupted
        if is(job.task,runner)
            # need to continue, so this task can't be shared yet
            runner = ()
        end
        if isa(result,WaitFor)
            # add to waiting set to wait on a sync event
            wf::WaitFor = result
            Waiting[wf.oid] = (wf.msg, job, get(Waiting, wf.oid, ()))
        elseif !task_done(job.task) && job.requeue
            # otherwise return to queue
            enq_work(job)
        end
    end
end
end

function notify_done(job::WorkItem)
    while !is(job.notify,())
        (sock, msg, oid, job.notify) = job.notify
        let wr = work_result(job)
            if is(sock,())
                deliver_result(sock, msg, oid, ()->wr)
            else
                deliver_result(sock, msg, oid, wr)
            end
        end
    end
end

function make_scheduled(t::Task)
    enq_work(WorkItem(t))
    t
end

## worker creation and setup ##

# activity on accept fd
function accept_handler(accept_fd, sockets)
    global PGRP
    connectfd = ccall(dlsym(libc, :accept), Int32,
                      (Int32, Ptr{Void}, Ptr{Void}),
                      accept_fd, C_NULL, C_NULL)
    #print("accepted.\n")
    if connectfd==-1
        print("accept error: ", strerror(), "\n")
    else
        first = isempty(sockets)
        sock = fdio(connectfd)
        sockets[connectfd] = sock
        if first
            # first connection; get process group info from client
            _myid = force(deserialize(sock))
            locs = force(deserialize(sock))
            PGRP = ProcessGroup(_myid, locs, sockets)
            PGRP.client = Worker("", 0, connectfd, sock)
        end
        add_fd_handler(connectfd, fd->message_handler(fd, sockets))
    end
end

type DisconnectException <: Exception end

# activity on message socket
function message_handler(fd, sockets)
    global PGRP
    refs = PGRP.refs
    sock = sockets[fd]
    first = true
    while first || nb_available(sock)>0
        first = false
        try
            msg = force(deserialize(sock))
            #print("$(myid()) got ", tuple(msg, args[1],
            #                              map(typeof,args[2:])), "\n")
            # handle message
            if is(msg, :call) || is(msg, :call_fetch) || is(msg, :call_wait)
                id = force(deserialize(sock))
                f = deserialize(sock)
                args = deserialize(sock)
                wi = schedule_call(id, f, args)
                if is(msg, :call_fetch)
                    wi.notify = (sock, :fetch, id, wi.notify)
                elseif is(msg, :call_wait)
                    wi.notify = (sock, :wait, id, wi.notify)
                end
            elseif is(msg, :do)
                f = deserialize(sock)
                args = deserialize(sock)
                let func=f, ar=args
                    enq_work(WorkItem(()->apply(force(func),force(ar))))
                end
            elseif is(msg, :result)
                # used to deliver result of wait or fetch
                mkind = force(deserialize(sock))
                oid = force(deserialize(sock))
                val = deserialize(sock)
                deliver_result((), mkind, oid, val)
            elseif is(msg, :identify_socket)
                otherid = force(deserialize(sock))
                identify_socket(otherid, fd, sock)
            else
                # the synchronization messages
                oid = force(deserialize(sock))
                wi = lookup_ref(oid)
                if wi.done
                    deliver_result(sock, msg, oid, work_result(wi))
                else
                    # add to WorkItem's notify list
                    wi.notify = (sock, msg, oid, wi.notify)
                end
            end
        catch e
            if isa(e,EOFError)
                #print("eof. $(myid()) exiting\n")
                del_fd_handler(fd)
                # TODO: remove machine from group
                throw(DisconnectException())
            else
                print("deserialization error: ", e, "\n")
                read(sock, Uint8, nb_available(sock))
                #while nb_available(sock) > 0 #|| select(sock)
                #    read(sock, Uint8)
                #end
            end
        end
    end
end

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
function start_worker(wrfd)
    ccall(:jl_start_io_thread, Void, ())
    port = [int16(9009)]
    sockfd = ccall(:open_any_tcp_port, Int32, (Ptr{Int16},), port)
    if sockfd == -1
        error("could not bind socket")
    end
    io = fdio(wrfd)
    write(io, port[1])
    flush(io)
    #close(io)
    # close stdin; workers will not use it
    ccall(dlsym(libc, :close), Int32, (Int32,), 0)

    global Workqueue = Queue()
    global Waiting = HashTable(64)
    global Scheduler = current_task()

    worker_sockets = HashTable()
    add_fd_handler(sockfd, fd->accept_handler(fd, worker_sockets))

    try
        event_loop(false)
    catch e
        print("unhandled exception on $(myid()): $e\nexiting.\n")
    end

    ccall(dlsym(libc, :close), Int32, (Int32,), sockfd)
    ccall(dlsym(libc, :exit) , Void , (Int32,), 0)
end

# establish an SSH tunnel to a remote worker
# returns P such that localhost:P connects to host:port
function worker_tunnel(host, port)
    localp = 9201
    while !run(`ssh -f -o ExitOnForwardFailure=yes julia@$host -L $localp:$host:$port -N`)
        localp += 1
    end
    localp
end

worker_ssh_command(host) =
    `ssh $host "bash -l -c \"julia -e start_worker\(1\)\""`

function start_remote_worker(host)
    proc = worker_ssh_command(host)
    out = cmd_stdout_stream(proc)
    spawn(proc)
    Worker(host, read(out, Int16))
end

function start_remote_workers(machines)
    cmds = map(worker_ssh_command, machines)
    outs = map(cmd_stdout_stream, cmds)
    for c = cmds
        spawn(c)
    end
    { Worker(machines[i], read(outs[i],Int16)) | i=1:length(machines) }
end

function start_local_worker()
    fds = Array(Int32, 2)
    ccall(dlsym(libc, :pipe), Int32, (Ptr{Int32},), fds)
    rdfd = fds[1]
    wrfd = fds[2]

    if fork()==0
        start_worker(wrfd)
    end
    io = fdio(rdfd)
    port = read(io, Int16)
    ccall(dlsym(libc,:close), Int32, (Int32,), rdfd)
    ccall(dlsym(libc,:close), Int32, (Int32,), wrfd)
    #print("started worker on port ", port, "\n")
    sleep(0.1)
    port
end

function connect_to_worker(hostname, port)
    fd = ccall(:connect_to_host, Int32,
               (Ptr{Uint8}, Int16), hostname, port)
    if fd == -1
        error("could not connect to $hostname:$port, errno=$(errno())\n")
    end
    (fd, fdio(fd))
end

## global objects and collective operations ##

type GlobalObject
    local_identity
    refs::Array{RemoteRef,1}

    function GlobalObject(rids, initializer)
        global PGRP
        go = new((), Array(RemoteRef, PGRP.np))

        mi = myid()
        myrid = rids[mi]
        # make our reference to it weak so we can detect when there are
        # no local users of the object.
        #if !has(PGRP.refs,myrid)
        #    error("GlobalObject was hoping to find something")
        #end
        wi = lookup_ref(myrid)
        function del_go_client(go)
            if has(wi.clientset, mi)
                for i=1:PGRP.np
                    send_del_client(go.refs[i])
                end
            end
            if !isempty(wi.clientset)
                # still has some remote clients, restore finalizer & stay alive
                finalizer(go, del_go_client)
            end
        end
        finalizer(go, del_go_client)
        for i=1:length(rids)
            go.refs[i] = WeakRemoteRef(i, rids[i][1], rids[i][2])
        end
        # NOTE: this is put(go.refs[mi], WeakRef(go))
        go.local_identity = initializer(go)
        wi.result = WeakRef(go)
        wi.done = true
        notify_done(wi)
    end

    # initializer is a function that will be called on the new G.O., and its
    # result will be used to set go.local_identity
    function GlobalObject(initializer::Function)
        # makes remote object cycles, but we can take advantage of the known
        # topology to avoid fully-general cycle collection.
        # . keep a weak table of all client RemoteRefs, unique them
        # . send add_client when adding a new client for an object
        # . send del_client when an RR is collected
        # . the RemoteRefs inside a GlobalObject are weak
        #   . initially the creator of the GO is the only client
        #     everybody has {creator} as the client set
        #   . when a GO is sent, add a client to everybody
        #     . sender knows whether recipient is a client already by
        #       looking at the client set for its own copy, so it can
        #       avoid the client add message in this case.
        #   . send del_client when there are no references to the GO
        #     except the one in PGRP.refs
        #     . done by adding a finalizer to the GO that revives it by
        #       reregistering the finalizer until the client set is empty
        global PGRP
        r = Array(RemoteRef, PGRP.np)
        for i=1:length(r)
            # create a set of refs to be initialized by GlobalObject above
            r[i] = remote_ref(i)
        end
        rids = { rr2id(r[i]) | i=1:length(r) }
        for i=1:length(r)
            remote_do(i, GlobalObject, rids, initializer)
        end
        if myid()==0
            go = new((), r)
            go.local_identity = initializer(go)
            go.local_identity
        else
            fetch(r[myid()])
        end
    end

    GlobalObject() = GlobalObject(identity)
end

show(g::GlobalObject) = print("GlobalObject()")

function serialize(s, g::GlobalObject)
    global PGRP
    # a GO is sent to a machine by sending just the RemoteRef for its
    # copy. much smaller message.
    i = worker_id_from_socket(s)
    if i != -1
        mi = myid()
        if mi==0
            addnew = true
        else
            myref = g.refs[mi]
            wi = PGRP.refs[rr2id(myref)]
            addnew = !has(wi.clientset, i)
        end
        if addnew
            # adding new client to this GO
            for p=1:PGRP.np
                if p != i
                    send_add_client(p, g.refs[p])
                end
            end
        end
        serialize(s, g.refs[i])
    else
        # TODO: be able to make GlobalObjects that span only a subset of all
        # processors, and allow them to have outside clients.
        error("global object cannot be sent outside its process group")
        #invoke(serialize, (Any, Any), s, g)
    end
end

spawnat(p, thunk) = remote_call(p, thunk)

let lastp = 1
    global spawn
    function spawn(thunk::Function)
        p = -1
        env = ccall(:jl_closure_env, Any, (Any,), thunk)
        if isa(env,Tuple)
            for v = env
                if isa(v,Box)
                    v = v.contents
                end
                if isa(v,RemoteRef)
                    p = v.where; break
                end
            end
        end
        if p == -1
            p = lastp; lastp += 1
            global PGRP
            if lastp > PGRP.np
                lastp = 1
            end
        end
        spawnat(p, thunk)
    end
end

macro spawn(thk); :(spawn(()->($thk))); end

## demos ##

fv(a)=eig(a)[2][2]
# g = ProcessGroup(3)
# A=randn(800,800);A=A*A';
# pmap(fv, {A,A,A})

all2all() = at_each(hello_from, myid())

hello_from(i) = print("message from $i to $(myid())\n")

## event processing ##

fd_handlers = HashTable()

add_fd_handler(fd, H) = (fd_handlers[fd]=H)
del_fd_handler(fd) = del(fd_handlers, fd)

function event_loop(client)
    fdset = FDSet()
    iserr, lasterr = false, ()
    
    while true
        try
            if iserr
                show(lasterr)
                iserr, lasterr = false, ()
            end
            while true
                del_all(fdset)
                for (fd,_) = fd_handlers
                    add(fdset, fd)
                end
                
                nselect = select_read(fdset, isempty(Workqueue) ? 10 : 0)
                if nselect == 0
                    if !isempty(Workqueue)
                        perform_work()
                    end
                else
                    for fd=0:(fdset.nfds-1)
                        if has(fdset,fd)
                            h = fd_handlers[fd]
                            h(fd)
                        end
                    end
                end
            end
        catch e
            if isa(e,DisconnectException) && !client
                return()
            end
            iserr, lasterr = true, e
        end
    end
end

roottask = current_task()

function repl_callback(ast, show_value)
    # use root task to execute user input
    cmd = WorkItem(roottask)
    cmd.argument = (ast, show_value)
    cmd.requeue = false
    perform_work(cmd)
end

# start as a node that accepts interactive input
function start_client()
    ccall(:jl_start_io_thread, Void, ())
    try
        global Workqueue = Queue()
        global Waiting = HashTable(64)
        global Scheduler = Task(()->event_loop(true), 1024*1024)

        while true
            add_fd_handler(STDIN.fd, fd->ccall(:jl_stdin_callback, Void, ()))
            (ast, show_value) = yield()
            del_fd_handler(STDIN.fd)
            ccall(:jl_eval_user_input, Void, (Any, Int32),
                  ast, show_value)
        end
    catch e
        show(e)
    end
end
