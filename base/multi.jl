## multi.jl - multiprocessing
##
## julia starts with one process, and processors can be added using:
##   addprocs_local(n)                     using exec
##   addprocs_ssh({"host1","host2",...})   using remote execution
##   addprocs_sge(n)                       using Sun Grid Engine batch queue
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
## remote_call_fetch(w, func, args...) - faster fetch(remote_call(...))
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
## @spawnlocal expr -
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
# ? method_missing for waiting (ref/assign/localdata seems to cover a lot)
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

function send_msg_unknown(s::IOStream, kind, args)
    error("attempt to send to unknown socket")
end

function send_msg(s::IOStream, kind, args...)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg(worker_from_id(id), kind, args...)
    end
    send_msg_unknown(s, kind, args)
end

function send_msg_now(s::IOStream, kind, args...)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg_now(worker_from_id(id), kind, args...)
    end
    send_msg_unknown(s, kind, args)
end

type Worker
    host::String
    port::Int16
    fd::Int32
    socket::IOStream
    sendbuf::IOStream
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    id::Int
    gcflag::Bool
    
    function Worker(host::ByteString, port)
        fd = ccall(:connect_to_host, Int32, (Ptr{Uint8}, Int16), host, port)
        if fd == -1
            error("could not connect to $host:$port, errno=$(errno())\n")
        end
        Worker(host, port, fd, fdio(fd, true))
    end

    Worker(host,port,fd,sock,id) = new(host, port, fd, sock, memio(), {}, {}, id, false)
    Worker(host,port,fd,sock) = Worker(host,port,fd,sock,0)
end

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
        del_all(w.add_msgs)
        remote_do(w, add_clients, msgs...)
    end

    msgs = w.del_msgs
    if !isempty(msgs)
        del_all(w.del_msgs)
        #print("sending delete of $msgs\n")
        remote_do(w, del_clients, msgs...)
    end
end

function send_msg_(w::Worker, kind, args, now::Bool)
    buf = w.sendbuf
    ccall(:jl_buf_mutex_lock, Void, (Ptr{Void},), buf.ios)
    serialize(buf, kind)
    for arg in args
        serialize(buf, arg)
    end
    ccall(:jl_buf_mutex_unlock, Void, (Ptr{Void},), buf.ios)

    if !now && w.gcflag
        flush_gc_msgs(w)
    else
        ccall(:jl_enq_send_req, Void, (Ptr{Void}, Ptr{Void}, Int32),
              w.socket.ios, w.sendbuf.ios, now ? int32(1) : int32(0))
    end
end

function flush_gc_msgs()
    for w = (PGRP::ProcessGroup).workers
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
end

type Location
    host::String
    port::Int16
    Location(h,p::Integer) = new(h,int16(p))
end

type ProcessGroup
    myid::Int
    workers::Array{Any,1}
    locs::Array{Any,1}
    np::Int

    # global references
    refs::Dict

    function ProcessGroup(myid::Integer, w::Array{Any,1}, locs::Array{Any,1})
        return new(myid, w, locs, length(w), Dict())
    end
end

function add_workers(PGRP::ProcessGroup, w::Array{Any,1})
    n = length(w)
    locs = map(x->Location(x.host,x.port), w)
    # NOTE: currently only node 1 can add new nodes, since nobody else
    # has the full list of address:port
    newlocs = [PGRP.locs, locs]
    sockets = Dict()
    handler = fd->message_handler(fd, sockets)
    for i=1:n
        push(PGRP.workers, w[i])
        w[i].id = PGRP.np+i
        send_msg_now(w[i], w[i].id, newlocs)
        sockets[w[i].fd] = w[i].socket
        add_fd_handler(w[i].fd, handler)
    end
    PGRP.locs = newlocs
    PGRP.np += n
    :ok
end

function _jl_join_pgroup(myid, locs, sockets)
    # joining existing process group
    np = length(locs)
    w = cell(np)
    w[myid] = LocalProcess()
    handler = fd->message_handler(fd, sockets)
    for i = 2:(myid-1)
        w[i] = Worker(locs[i].host, locs[i].port)
        w[i].id = i
        sockets[w[i].fd] = w[i].socket
        add_fd_handler(w[i].fd, handler)
        send_msg_now(w[i], :identify_socket, myid)
    end
    for i = (myid+1):np
        w[i] = nothing
    end
    ProcessGroup(myid, w, locs)
end

myid() = (global PGRP; (PGRP::ProcessGroup).myid)
nprocs() = (global PGRP; (PGRP::ProcessGroup).np)

function worker_from_id(i)
    pg = PGRP::ProcessGroup
    while i > length(pg.workers) || pg.workers[i]===nothing
        sleep(0.1)
        yield()
    end
    pg.workers[i]
end

function worker_id_from_socket(s)
    global PGRP
    for i=1:nprocs()
        w = worker_from_id(i)
        if isa(w,Worker)
            if is(s, w.socket) || is(s, w.sendbuf)
                return i
            end
        end
    end
    if isa(s,IOStream) && fd(s)==-1
        # serializing to a local buffer
        return myid()
    end
    return -1
end

# establish a Worker connection for processes that connected to us
function _jl_identify_socket(otherid, fd, sock)
    global PGRP
    i = otherid
    #locs = PGRP.locs
    @assert i > PGRP.myid
    d = i-length(PGRP.workers)
    if d > 0
        grow(PGRP.workers, d)
        PGRP.workers[(end-d+1):end] = nothing
        PGRP.np += d
    end
    PGRP.workers[i] = Worker("", 0, fd, sock, i)
    #write(stdout_stream, "$(PGRP.myid) heard from $i\n")
    nothing
end

## remote refs and core messages: do, call, fetch, wait, ref, put ##

const _jl_client_refs = WeakKeyDict()

type RemoteRef
    where::Int
    whence::Int
    id::Int
    # TODO: cache value if it's fetched, but don't serialize the cached value

    function RemoteRef(w, wh, id)
        r = new(w,wh,id)
        found = key(_jl_client_refs, r, false)
        if !is(found,false)
            return found
        end
        _jl_client_refs[r] = true
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

    RemoteRef(w::LocalProcess) = RemoteRef(myid())
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

bottom_func() = assert(false)

function lookup_ref(id)
    GRP = PGRP::ProcessGroup
    wi = get(GRP.refs, id, ())
    if is(wi, ())
        # first we've heard of this ref
        wi = WorkItem(bottom_func)
        # this WorkItem is just for storing the result value
        GRP.refs[id] = wi
        add(wi.clientset, id[1])
    end
    wi
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
        remote_call_fetch(rr.where, id->lookup_ref(id).done, rid)
    end
end

function del_client(id, client)
    global PGRP
    wi = lookup_ref(id)
    del(wi.clientset, client)
    if isempty(wi.clientset)
        del((PGRP::ProcessGroup).refs, id)
        #print("$(myid()) collected $id\n")
    end
    nothing
end

function del_clients(pairs::(Any,Any)...)
    for p in pairs
        del_client(p[1], p[2])
    end
end

function send_del_client(rr::RemoteRef)
    if rr.where == myid()
        del_client(rr2id(rr), myid())
    else
        w = worker_from_id(rr.where)
        push(w.del_msgs, (rr2id(rr), myid()))
        w.gcflag = true
    end
end

function add_client(id, client)
    wi = lookup_ref(id)
    add(wi.clientset, client)
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
        push(w.add_msgs, (rr2id(rr), i))
        w.gcflag = true
    end
end

function serialize(s, rr::RemoteRef)
    i = worker_id_from_socket(s)
    if i != -1
        send_add_client(rr, i)
    end
    invoke(serialize, (Any, Any), s, rr)
end

type GORef
    whence
    id
end

# special type for serializing references to GlobalObjects.
# Needed because we want to always wait for the G.O. to be computed and
# return it. in contrast, deserialize() for RemoteRef needs to avoid waiting
# on uninitialized RemoteRefs since that might cause a deadlock, while G.O.s
# are a special case where we know waiting on the RR is OK.
function deserialize(s, t::Type{GORef})
    gr = force(invoke(deserialize, (Any, CompositeKind), s, t))
    rid = (gr.whence, gr.id)
    add_client(rid, myid())
    function ()
        wi = lookup_ref(rid)
        if !wi.done
            wait(WeakRemoteRef(myid(), rid[1], rid[2]))
        end
        v = wi.result
        if isa(v,WeakRef)
            v = v.value
        end
        assert(isa(v,GlobalObject))
        return v.local_identity
    end
end

function deserialize(s, t::Type{RemoteRef})
    rr = force(invoke(deserialize, (Any, CompositeKind), s, t))
    rid = rr2id(rr)
    where = rr.where
    if where == myid()
        add_client(rid, myid())
    end
    function ()
        if where == myid()
            wi = lookup_ref(rid)
            if !wi.done
                if !is(wi.thunk,bottom_func)
                    #println("$(myid()) waiting for $where,$(rid[1]),$(rid[2])")
                    wait(WeakRemoteRef(where, rid[1], rid[2]))
                    #println("...ok")
                else
                    return RemoteRef(where, rid[1], rid[2])
                end
            end
            v = wi.result
            # NOTE: this duplicates work_result()
            if isa(v,WeakRef)
                v = v.value
            end
            if isa(v,GlobalObject)
                if !anyp(r->(r.whence==rid[1] && r.id==rid[2]), v.refs)
                    # ref not part of the GlobalObject, so it needs to
                    # manage its own lifetime
                    RemoteRef(where, rid[1], rid[2])
                else
                    # here the GlobalObject's finalizer will handle removing
                    # the client ref we added with add_client above.
                end
                v = v.local_identity
            else
                # make a RemoteRef so a finalizer is set up to remove us
                # as a client. the RR has been converted to its value, so
                # we don't need it any more unless there is another reference
                # to this RR somewhere on our system.
                RemoteRef(where, rid[1], rid[2])
            end
            return v
        else
            # make sure this rr gets added to the _jl_client_refs table
            RemoteRef(where, rid[1], rid[2])
        end
    end
end

schedule_call(rid, f_thk, args_thk) =
    schedule_call(rid, ()->apply(force(f_thk),force(args_thk)))

function schedule_call(rid, thunk)
    global PGRP
    wi = WorkItem(thunk)
    (PGRP::ProcessGroup).refs[rid] = wi
    add(wi.clientset, rid[1])
    enq_work(wi)
    wi
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
function local_remote_call_thunk(f, args)
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
    #     env = force(deserialize(buf))
    #     f = ccall(:jl_new_closure, Any, (Ptr{Void}, Any, Any),
    #               C_NULL, env, linfo)::Function
    # end
    # f(map(localize_ref,args)...)
end

function remote_call(w::LocalProcess, f, args...)
    rr = RemoteRef(w)
    schedule_call(rr2id(rr), local_remote_call_thunk(f,args))
    rr
end

function remote_call(w::Worker, f, args...)
    rr = RemoteRef(w)
    #println("$(myid()) asking for $rr")
    send_msg(w, :call, rr2id(rr), f, args)
    rr
end

remote_call(id::Integer, f, args...) = remote_call(worker_from_id(id), f, args...)

# faster version of fetch(remote_call(...))
function remote_call_fetch(w::LocalProcess, f, args...)
    rr = WeakRemoteRef(w)
    oid = rr2id(rr)
    wi = schedule_call(oid, local_remote_call_thunk(f,args))
    wi.notify = ((), :call_fetch, oid, wi.notify)
    force(yield(WaitFor(:call_fetch, rr)))
end

function remote_call_fetch(w::Worker, f, args...)
    # can be weak, because the program will have no way to refer to the Ref
    # itself, it only gets the result.
    rr = WeakRemoteRef(w)
    oid = rr2id(rr)
    send_msg(w, :call_fetch, oid, f, args)
    force(yield(WaitFor(:call_fetch, rr)))
end

remote_call_fetch(id::Integer, f, args...) =
    remote_call_fetch(worker_from_id(id), f, args...)

# faster version of wait(remote_call(...))
remote_call_wait(w::LocalProcess, f, args...) = wait(remote_call(w,f,args...))

function remote_call_wait(w::Worker, f, args...)
    rr = RemoteRef(w)
    oid = rr2id(rr)
    send_msg(w, :call_wait, oid, f, args)
    yield(WaitFor(:wait, rr))
end

remote_call_wait(id::Integer, f, args...) =
    remote_call_wait(worker_from_id(id), f, args...)

function remote_do(w::LocalProcess, f, args...)
    # the LocalProcess version just performs in local memory what a worker
    # does when it gets a :do message.
    # same for other messages on LocalProcess.
    enq_work(WorkItem(local_remote_call_thunk(f, args)))
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
    if r.where==myid() || isa(pg.workers[r.where], LocalProcess)
        wi = lookup_ref(oid)
        if wi.done
            if is(verb,:fetch)
                return work_result(wi)
            else
                return r
            end
        else
            # add to WorkItem's notify list
            wi.notify = ((), verb, oid, wi.notify)
        end
    else
        send_msg(worker_from_id(r.where), verb, oid)
    end
    # yield to event loop, return here when answer arrives
    v = yield(WaitFor(verb, r))
    return is(verb,:fetch) ? force(v) : r
end

wait(r::RemoteRef) = sync_msg(:wait, r)
fetch(r::RemoteRef) = sync_msg(:fetch, r)
fetch(x::ANY) = x

# writing to an uninitialized ref
function put_ref(rid, val::ANY)
    wi = lookup_ref(rid)
    if wi.done
        wi.notify = ((), :take, rid, wi.notify)
        yield(WaitFor(:take, RemoteRef(myid(), rid[1], rid[2])))
    end
    wi.result = val
    wi.done = true
    notify_done(wi)
end

function put(rr::RemoteRef, val::ANY)
    rid = rr2id(rr)
    if rr.where == myid()
        put_ref(rid, val)
    else
        remote_call_fetch(rr.where, put_ref, rid, val)
    end
    val
end

function take_ref(rid)
    wi = lookup_ref(rid)
    if !wi.done
        wait(RemoteRef(myid(), rid[1], rid[2]))
    end
    val = wi.result
    wi.done = false
    notify_empty(wi)
    val
end

function take(rr::RemoteRef)
    rid = rr2id(rr)
    if rr.where == myid()
        take_ref(rid)
    else
        remote_call_fetch(rr.where, take_ref, rid)
    end
end

## work queue ##

type WorkItem
    thunk::Function
    task   # the Task working on this item, or ()
    done::Bool
    result
    notify::Tuple
    argument  # value to pass task next time it is restarted
    clientset::IntSet

    WorkItem(thunk::Function) = new(thunk, (), false, (), (), (), IntSet())
    WorkItem(task::Task) = new(()->(), task, false, (), (), (), IntSet())
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

type WaitFor
    msg::Symbol
    rr
end

function enq_work(wi::WorkItem)
    global Workqueue
    enqueue(Workqueue, wi)
end

enq_work(f::Function) = enq_work(WorkItem(f))
enq_work(t::Task) = enq_work(WorkItem(t))

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
            job.task.runnable = true
            result = is(arg,()) ? yieldto(job.task) : yieldto(job.task, arg)
        else
            job.task = Task(job.thunk)
            job.task.tls = nothing
            result = yieldto(job.task)
        end
    catch e
        #show(e)
        print("exception on ", myid(), ": ")
        show(e)
        println()
        result = e
    end
    # restart job by yielding back to whatever task just switched to us
    job.task = current_task().last
    if istaskdone(job.task)
        # job done
        job.done = true
        job.result = result
    end
    if job.done
        job.task = ()
        # do notifications
        notify_done(job)
        job.thunk = bottom_func  # avoid reference retention
    elseif isa(result,WaitFor)
        job.task.runnable = false
        wf::WaitFor = result
        if wf.msg === :consume
            P = wf.rr::Task
            # queue consumers waiting for producer P. note that in order
            # to avoid scheduling overhead for a typical produce/consume,
            # there is no fairness unless consumers explicitly yield().
            if P.consumers === nothing
                P.consumers = {job}
            else
                enqueue(P.consumers, job)
            end
        else
            # add to waiting set to wait on a sync event
            rr = wf.rr
            #println("$(myid()) waiting for $rr")
            oid = rr2id(rr)
            waitinfo = (wf.msg, job, rr)
            waiters = get(Waiting, oid, false)
            if isequal(waiters,false)
                Waiting[oid] = {waitinfo}
            else
                push(waiters, waitinfo)
            end
        end
    elseif job.task.runnable
        # otherwise return to queue
        enq_work(job)
    end
end

function deliver_result(sock::IOStream, msg, oid, value)
    #print("$(myid()) sending result $oid\n")
    if is(msg,:fetch) || is(msg,:call_fetch)
        val = value
    else
        @assert is(msg, :wait)
        val = oid
    end
    try
        send_msg_now(sock, :result, msg, oid, val)
    catch e
        # send exception in case of serialization error; otherwise
        # request side would hang.
        send_msg_now(sock, :result, msg, oid, e)
    end
end

const _jl_empty_cell_ = {}
function deliver_result(sock::(), msg, oid, value_thunk)
    global Waiting
    # restart task that's waiting on oid
    jobs = get(Waiting, oid, _jl_empty_cell_)
    for i = 1:length(jobs)
        j = jobs[i]
        if j[1]==msg
            job = j[2]
            job.argument = value_thunk
            enq_work(job)
            del(jobs, i)
            break
        end
    end
    if isempty(jobs) && !is(jobs,_jl_empty_cell_)
        del(Waiting, oid)
    end
    nothing
end

notify_done (job::WorkItem) = notify_done(job, false)
notify_empty(job::WorkItem) = notify_done(job, true)

# notify waiters that a certain job has finished or Ref has been emptied
function notify_done(job::WorkItem, take)
    newnot = ()
    while !is(job.notify,())
        (sock, msg, oid, job.notify) = job.notify
        if take == is(msg,:take)
            let wr = work_result(job)
                if is(sock,())
                    deliver_result(sock, msg, oid, ()->wr)
                else
                    deliver_result(sock, msg, oid, wr)
                end
                if is(msg,:call_fetch)
                    # can delete the ref right away since we know it is
                    # unreferenced by the client
                    del((PGRP::ProcessGroup).refs, oid)
                end
            end
        else
            newnot = (sock, msg, oid, newnot)
        end
    end
    job.notify = newnot
    nothing
end

## message event handlers ##

# activity on accept fd
function accept_handler(accept_fd, sockets)
    global PGRP
    connectfd = ccall(:accept, Int32, (Int32, Ptr{Void}, Ptr{Void}),
                      accept_fd, C_NULL, C_NULL)
    #print("accepted.\n")
    if connectfd==-1
        print("accept error: ", strerror(), "\n")
    else
        first = isempty(sockets)
        sock = fdio(connectfd, true)
        sockets[connectfd] = sock
        if first
            # first connection; get process group info from client
            _myid = force(deserialize(sock))
            locs = force(deserialize(sock))
            PGRP = _jl_join_pgroup(_myid, locs, sockets)
            PGRP.workers[1] = Worker("", 0, connectfd, sock, 1)
        end
        add_fd_handler(connectfd, fd->message_handler(fd, sockets))
    end
end

type DisconnectException <: Exception end

# activity on message socket
function message_handler(fd, sockets)
    global PGRP
    refs = (PGRP::ProcessGroup).refs
    sock = sockets[fd]
    first = true
    while first || nb_available(sock)>0
        first = false
        try
            msg = force(deserialize(sock))
            #print("$(myid()) got $msg\n")
            # handle message
            if is(msg, :call) || is(msg, :call_fetch) || is(msg, :call_wait)
                id = force(deserialize(sock))
                f = deserialize(sock)
                args = deserialize(sock)
                #print("$(myid()) got call $id\n")
                wi = schedule_call(id, f, args)
                if is(msg, :call_fetch)
                    wi.notify = (sock, :call_fetch, id, wi.notify)
                elseif is(msg, :call_wait)
                    wi.notify = (sock, :wait, id, wi.notify)
                end
            elseif is(msg, :do)
                f = deserialize(sock)
                args = deserialize(sock)
                #print("$(myid()) got $args\n")
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
                _jl_identify_socket(otherid, fd, sock)
            else
                # the synchronization messages
                oid = force(deserialize(sock))::(Int,Int)
                wi = lookup_ref(oid)
                if wi.done
                    deliver_result(sock, msg, oid, work_result(wi))
                else
                    # add to WorkItem's notify list
                    # TODO: should store the worker here, not the socket,
                    # so we don't need to look up the worker later
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

## worker creation and setup ##

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
start_worker() = start_worker(1)
function start_worker(wrfd)
    port = [int16(9009)]
    sockfd = ccall(:open_any_tcp_port, Int32, (Ptr{Int16},), port)
    if sockfd == -1
        error("could not bind socket")
    end
    io = fdio(wrfd)
    write(io, "julia_worker:")    # print header
    write(io, "$(dec(port[1]))#") # print port
    write(io, getipaddr())        # print hostname
    write(io, '\n')
    flush(io)
    # close stdin; workers will not use it
    ccall(:close, Int32, (Int32,), 0)

    global const Scheduler = current_task()

    worker_sockets = Dict()
    add_fd_handler(sockfd, fd->accept_handler(fd, worker_sockets))

    try
        event_loop(false)
    catch e
        print("unhandled exception on $(myid()): $e\nexiting.\n")
    end

    ccall(:close, Int32, (Int32,), sockfd)
    ccall(:exit , Void , (Int32,), 0)
end

# establish an SSH tunnel to a remote worker
# returns P such that localhost:P connects to host:port
# function worker_tunnel(host, port)
#     localp = 9201
#     while !success(`ssh -f -o ExitOnForwardFailure=yes julia@$host -L $localp:$host:$port -N`)
#         localp += 1
#     end
#     localp
# end

function start_remote_workers(machines, cmds)
    n = length(cmds)
    outs = cell(n)
    for i=1:n
        let fd = read_from(cmds[i]).fd
            let stream = fdio(fd, true)
                outs[i] = stream
                # redirect console output from workers to the client's stdout
                add_fd_handler(fd, fd->write(stdout_stream, readline(stream)))
            end
        end
    end
    for c in cmds
        spawn(c)
    end
    w = cell(n)
    for i=1:n
        local hostname::String, port::Int16
        while true
            conninfo = readline(outs[i])
            hostname, port = parse_connection_info(conninfo)
            if hostname != ""
                break
            end
        end
        w[i] = Worker(hostname, port)
    end
    w
end

function parse_connection_info(str)
    m = match(r"^julia_worker:(\d+)#(.*)", str)
    if m != nothing
        (m.captures[2], parse_int(Int16, m.captures[1]))
    else
        ("", int16(-1))
    end
end

function worker_ssh_cmd(host)
    `ssh -n $host "bash -l -c \"cd $JULIA_HOME && ./julia-release-basic --worker\""`
end #func

function worker_ssh_cmd(host, key)
    `ssh -i $key -n $host "bash -l -c \"cd $JULIA_HOME && ./julia-release-basic --worker\""`
end #func

function addprocs_ssh(machines) 
    add_workers(PGRP, start_remote_workers(machines, map(worker_ssh_cmd, machines)))
end #func

function addprocs_ssh(machines, keys)
    if !(isa(keys, Array)) && isa(machines,Array)
        key = keys
        keys = [ key for x = 1:numel(machines)]
        cmdargs = { {machines[x],keys[x]} for x = 1:numel(machines)}
    else
        cmdargs = {{machines,keys}}
    end #if/else
    add_workers(PGRP, start_remote_workers(machines, map(x->worker_ssh_cmd(x[1],x[2]), cmdargs)))
end #func

worker_local_cmd() = `$JULIA_HOME/julia-release-basic --worker`

addprocs_local(np::Integer) =
    add_workers(PGRP, start_remote_workers({ "localhost" for i=1:np },
                                           { worker_local_cmd() for i=1:np }))


function start_sge_workers(n)
    home = JULIA_HOME
    sgedir = "$home/../../SGE"
    run(`mkdir -p $sgedir`)
    qsub_cmd = `qsub -N JULIA -terse -e $sgedir -o $sgedir -t 1:$n`
    `echo $home/julia-release-basic --worker` | qsub_cmd
    out = cmd_stdout_stream(qsub_cmd)
    if !success(qsub_cmd)
        error("batch queue not available (could not run qsub)")
    end
    id = split(readline(out),'.')[1]
    println("job id is $id")
    print("waiting for job to start"); flush(stdout_stream)
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
                catch e
                    close(fl)
                    throw(e)
                end
                close(fl)
                fexists = (hostname != "")
            catch
                print("."); flush(stdout_stream)
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

#include("vcloud.jl")

## global objects and collective operations ##

type GlobalObject
    local_identity
    refs::Array{RemoteRef,1}

    global init_GlobalObject
    function init_GlobalObject(mi, procs, rids, initializer)
        np = length(procs)
        refs = Array(RemoteRef, np)
        local myrid

        for i=1:np
            refs[i] = WeakRemoteRef(procs[i], rids[i][1], rids[i][2])
            if procs[i] == mi
                myrid = rids[i]
            end
        end
        init_GlobalObject(mi, procs, rids, initializer, refs, myrid)
    end
    function init_GlobalObject(mi, procs, rids, initializer, refs, myrid)
        np = length(procs)
        go = new((), refs)

        # doing this lookup_ref is what adds the creating node to the client
        # set of all the Refs. so WeakRemoteRef is a bit of a misnomer.
        wi = lookup_ref(myrid)
        function del_go_client(go)
            if has(wi.clientset, mi)
                #println("$(myid()) trying to delete $(go.refs)")
                for i=1:np
                    send_del_client(go.refs[i])
                end
            end
            if !isempty(wi.clientset)
                # still has some remote clients, restore finalizer & stay alive
                finalizer(go, del_go_client)
            end
        end
        finalizer(go, del_go_client)
        go.local_identity = initializer(go)
        # make our reference to it weak so we can detect when there are
        # no local users of the object.
        # NOTE: this is put(go.refs[mi], WeakRef(go))
        wi.result = WeakRef(go)
        wi.done = true
        notify_done(wi)
        go
    end

    # initializer is a function that will be called on the new G.O., and its
    # result will be used to set go.local_identity
    function GlobalObject(procs, initializer::Function)
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
        np = length(procs)
        r = Array(RemoteRef, np)
        mi = myid()
        participate = false
        midx = 0
        for i=1:np
            # create a set of refs to be initialized by GlobalObject above
            # these can be weak since their lifetimes are managed by the
            # GlobalObject and its finalizer
            r[i] = WeakRemoteRef(procs[i])
            if procs[i] == mi
                participate = true
                midx = i
            end
        end
        rids = { rr2id(r[i]) for i=1:np }
        for p in procs
            if p != mi
                remote_do(p, init_GlobalObject, p, procs, rids, initializer)
            end
        end
        if !participate
            go = new((), r)
            go.local_identity = initializer(go)  # ???
            go
        else
            init_GlobalObject(mi, procs, rids, initializer, r, rr2id(r[midx]))
        end
    end

    function GlobalObject(initializer::Function)
        GlobalObject(1:nprocs(), initializer)
    end
    GlobalObject() = GlobalObject(identity)
end

show(g::GlobalObject) = (r = g.refs[myid()];
                         print("GlobalObject($(r.whence),$(r.id))"))

function is_go_member(g::GlobalObject, p::Integer)
    for i=1:length(g.refs)
        r = g.refs[i]
        if r.where == p
            return r
        end
    end
    return false
end

const _jl_temp_goref = GORef(0,0)
function serialize(s, g::GlobalObject)
    global PGRP
    # a GO is sent to a machine by sending just the RemoteRef for its
    # copy. much smaller message.
    i = worker_id_from_socket(s)
    if i == -1
        error("global object cannot be sent outside its process group")
    end
    ri = is_go_member(g, i)
    if is(ri, false)
        li = g.local_identity
        g.local_identity = nothing
        invoke(serialize, (Any, Any), s, g)
        g.local_identity = li
        return
    end
    mi = myid()
    myref = is_go_member(g, mi)
    if is(myref, false)
        # if I don't own a piece of this GO, I can't tell whether an
        # add_client of the destination node is necessary. therefore I
        # have to do one to be conservative.
        addnew = true
    else
        wi = PGRP.refs[rr2id(myref)]
        addnew = !has(wi.clientset, i)
    end
    if addnew
        # adding new client to this GO
        # node doing the serializing is responsible for notifying others of
        # new references.
        for rr = g.refs
            send_add_client(rr, i)
        end
    end
    _jl_temp_goref.whence = ri.whence
    _jl_temp_goref.id = ri.id
    serialize(s, _jl_temp_goref)
end

localize(g::GlobalObject) = g.local_identity
fetch(g::GlobalObject) = g.local_identity
#localize_ref(g::GlobalObject) = g.local_identity

broadcast(x) = GlobalObject(g->x)

function ref(g::GlobalObject, args...)
    g.local_identity[args...]
end

## higher-level functions: spawn, pmap, pfor, etc. ##

sync_begin() = tls(:SPAWNS, ({}, get(tls(), :SPAWNS, ())))

function sync_end()
    spawns = get(tls(), :SPAWNS, ())
    if is(spawns,())
        error("sync_end() without sync_begin()")
    end
    refs = spawns[1]
    tls(:SPAWNS, spawns[2])
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
    spawns = get(tls(), :SPAWNS, ())
    if !is(spawns,())
        push(spawns[1], r)
    end
    r
end

spawnat(p, thunk) = sync_add(remote_call(p, thunk))

let lastp = 1
    global spawn
    function spawn(thunk::Function)
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
            p = lastp; lastp += 1
            if lastp > nprocs()
                lastp = 1
            end
        end
        spawnat(p, thunk)
    end
end

find_vars(e) = find_vars(e, {})
function find_vars(e, lst)
    if isa(e,Symbol)
        if !isbound(e) || isconst(e)
            # exclude global constants
        else
            push(lst, e)
        end
    elseif isa(e,Expr)
        for x in e.args
            find_vars(x,lst)
        end
    end
    lst
end

# wrap an expression in "let a=a,b=b,..." for each var it references
function localize_vars(expr)
    v = find_vars(expr)
    # requires a special feature of the front end that knows how to insert
    # the correct variables. the list of free variables cannot be computed
    # from a macro.
    Expr(:localize, {:(()->($expr)), v...}, Any)
end

macro spawn(expr)
    expr = localize_vars(:(()->($expr)))
    :(spawn($(esc(expr))))
end

function spawnlocal(thunk)
    global Workqueue
    global PGRP
    rr = RemoteRef(myid())
    sync_add(rr)
    rid = rr2id(rr)
    wi = WorkItem(thunk)
    (PGRP::ProcessGroup).refs[rid] = wi
    add(wi.clientset, rid[1])
    push(Workqueue, wi)   # add to the *front* of the queue, work first
    yield()
    rr
end

macro spawnlocal(expr)
    expr = localize_vars(:(()->($expr)))
    :(spawnlocal($(esc(expr))))
end

macro spawnat(p, expr)
    expr = localize_vars(:(()->($expr)))
    :(spawnat($p, $(esc(expr))))
end

function at_each(f, args...)
    for i=1:nprocs()
        sync_add(remote_call(i, f, args...))
    end
end

macro everywhere(ex)
    quote
        @sync begin
            at_each(()->eval($(expr(:quote,ex))))
        end
    end
end

function pmap_static(f, lsts...)
    np = nprocs()
    n = length(lsts[1])
    { remote_call((i-1)%np+1, f, map(L->L[i], lsts)...) for i = 1:n }
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
    next_idx() = (idx=i; i+=1; idx)
    @sync begin
        for p=1:np
            @spawnat myid() begin
                while true
                    idx = next_idx()
                    if idx > n
                        break
                    end
                    results[idx] = remote_call_fetch(p, f,
                                                     map(L->L[idx], lsts)...)
                end
            end
        end
    end
    results
end

function preduce(reducer, f, r::Range1{Int})
    np = nprocs()
    N = length(r)
    each = div(N,np)
    rest = rem(N,np)
    if each < 1
        return fetch(@spawn f(first(r), first(r)+N-1))
    end
    results = cell(np)
    for i=1:np
        lo = first(r) + (i-1)*each
        hi = lo + each-1
        if i==np
            hi += rest
        end
        results[i] = @spawn f(lo, hi)
    end
    mapreduce(reducer, fetch, results)
end

function pfor(f, r::Range1{Int})
    np = nprocs()
    N = length(r)
    each = div(N,np)
    rest = rem(N,np)
    if each < 1
        @spawn f(first(r), first(r)+N-1)
        return
    end
    for i=1:np
        lo = first(r) + (i-1)*each
        hi = lo + each-1
        if i==np
            hi += rest
        end
        @spawn f(lo,hi)
    end
    nothing
end

function make_preduce_body(reducer, var, body)
    localize_vars(
    quote
        function (lo::Int, hi::Int)
            $(esc(var)) = lo
            ac = $(esc(body))
            for $(esc(var)) = (lo+1):hi
                ac = ($(esc(reducer)))(ac, $(esc(body)))
            end
            ac
        end
    end
                  )
end

function make_pfor_body(var, body)
    localize_vars(
    quote
        function (lo::Int, hi::Int)
            for $(esc(var)) = lo:hi
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
            pfor($(make_pfor_body(var, body)), $(esc(r)))
        end
    else
        quote
            preduce($(esc(reducer)),
                    $(make_preduce_body(reducer, var, body)), $(esc(r)))
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

function make_scheduled(t::Task)
    t.parent = Scheduler
    enq_work(WorkItem(t))
    t
end

function yield(args...)
    ct = current_task()
    # preserve Task.last across calls to the scheduler
    prev = ct.last
    v = yieldto(Scheduler, args...)
    ct.last = prev
    return v
end

const _jl_fd_handlers = Dict()

add_fd_handler(fd::Int32, H) = (_jl_fd_handlers[fd]=H)
del_fd_handler(fd::Int32) = del(_jl_fd_handlers, fd)

function event_loop(isclient)
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
                for (fd,_) = _jl_fd_handlers
                    add(fdset, fd)
                end

                bored = isempty(Workqueue)
                if bored
                    flush_gc_msgs()
                end
                nselect = select_read(fdset, bored ? 10.0 : 0.0)
                if nselect == 0
                    if !isempty(Workqueue)
                        perform_work()
                    end
                else
                    for fd=int32(0):int32(fdset.nfds-1)
                        if has(fdset,fd)
                            h = _jl_fd_handlers[fd]
                            h(fd)
                        end
                    end
                end
            end
        catch e
            if isa(e,DisconnectException)
                # TODO: wake up tasks waiting for failed process
                if !isclient
                    return
                end
            elseif isclient && isa(e,InterruptException) &&
                !has(_jl_fd_handlers, STDIN.fd)
                # root task is waiting for something on client. allow C-C
                # to interrupt.
                interrupt_waiting_task(_jl_roottask_wi, e)
            end
            iserr, lasterr = true, e
        end
    end
end

# force a task to stop waiting, providing with_value as the value of
# whatever it's waiting for.
function interrupt_waiting_task(wi::WorkItem, with_value)
    global Waiting
    for (oid, jobs) = Waiting
        for j in jobs
            if is(j[2], wi)
                deliver_result((), j[1], oid, ()->with_value)
                return
            end
        end
    end
end
