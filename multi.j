## multi.j - multiprocessing
##
## higher-level interface:
##
## Worker() - create a new local worker
##
## remote_apply(w, func, args...) -
##     tell a worker to call a function on the given arguments.
##     for now, functions are passed as symbols, e.g. :randn
##     returns a Future.
##
## wait(f) - wait for, then return the value represented by a Future
##
## pmap(pool, func, lst) -
##     call a function on each element of lst (some 1-d thing), in
##     parallel. pool is a list of available Workers.
##
## lower-level interface:
##
## send_msg(socket, x) - send a Julia object through a socket
## recv_msg(socket) - read the next Julia object from a socket

## message i/o ##

recv_msg(s) = deserialize(s)
function send_msg(s, x)
    buf = memio()
    serialize(buf, x)
    ccall(:ios_write_direct, Int32, (Ptr{Void}, Ptr{Void}),
          s.ios, buf.ios)
    ()
end

# todo:
# * recover from i/o errors
# * handle remote execution errors
# * all-to-all communication
# - send pings at some interval to detect failed/hung machines
# - integrate event loop with other kinds of i/o (non-messages)
# - serializing closures

## process group creation ##

struct Worker
    host::String
    port::Int16
    fd::Int32
    socket::IOStream

    Worker() = Worker("localhost", start_local_worker())

    Worker(host) = Worker(host,
                          host=="localhost" ?
                          start_local_worker() :
                          start_remote_worker(host))

    Worker(host, port) = Worker(host, port, connect_to_worker(host,port)...)

    Worker(host, port, fd, sock) = new(host, port, fd, sock)
end

struct LocalProcess
end

struct Location
    host::String
    port::Int16
end

struct ProcessGroup
    myid::Int32
    workers::Array{Any,1}
end

myid() = (global PGRP; isbound(:PGRP) ? PGRP.myid : -1)

function foreach(f, itr)
    for x=itr
        f(x)
    end
    ()
end

function make_process_group(np)
    w = { Worker() | i=1:np }
    global PGRP = ProcessGroup(0, w)
    locs = map(x->Location(x.host,x.port), w)
    for i=1:np
        remote_do(w[i], :join_process_group, i, locs)
    end
    PGRP
end

function join_process_group(myid, locs, sockets)
    np = length(locs)
    w = cell(np)
    w[myid] = LocalProcess()
    global PGRP = ProcessGroup(myid, w)
    global PLOCS = locs
    for i=(myid+1):np
        w[i] = Worker(locs[i].host, locs[i].port)
        sockets[w[i].fd] = w[i].socket
        remote_do(w[i], :identify_socket, myid)
    end
    ()
end

# establish a Worker connection for processes that connected to us
function identify_socket(otherid, fd, sock)
    global PGRP, PLOCS
    i = otherid
    assert(i < PGRP.myid)
    PGRP.workers[i] = Worker(PLOCS[i].host, PLOCS[i].port, fd, sock)
    #write(stdout_stream, latin1("$(PGRP.myid) heard from $i\n"))
    ()
end

## remote refs and core messages: do, call, fetch, wait ##

struct RemoteRef
    where::Int32
    whence::Int32
    id::Int32
    # TODO: cache value if it's fetched, but don't serialize the cached value
end

function remote_do(w::LocalProcess, f, args...)
    apply(eval(f), args)
    ()
end

function remote_call(w::LocalProcess, f, args...)
    return spawn(()->apply(eval(f), args))
end

fetch(t::Task) = wait(t)

function remote_do(w::Worker, f, args...)
    send_msg(w.socket, (:do, tuple(f, args...)))
    ()
end

REQ_ID = 0

function remote_call(w::Worker, f, args...)
    global REQ_ID, PGRP
    wid = 0
    for i=1:length(PGRP.workers)
        if is(w,PGRP.workers[i])
            wid = i
            break
        end
    end
    id = (myid(), REQ_ID)
    REQ_ID += 1
    send_msg(w.socket, (:call, tuple(id, f, args...)))
    RemoteRef(wid, myid(), id[2])
end

function msg_roundtrip(verb::Symbol, r::RemoteRef)
    global PGRP
    w = PGRP.workers[r.where]
    send_msg(w.socket, (verb, (r,)))
    if myid()==0
        # not a worker, all responses are synchronous
        (msg, args) = recv_msg(w.socket)
        assert(is(msg,:result))
        args[3]
    else
        # yield to worker loop, return here when answer arrives
        # TODO: make sure this yields to main event loop, not just parent
        yieldto(current_task().parent, WaitFor(verb, r))
    end
end

wait(r::RemoteRef) = msg_roundtrip(:sync, r)
fetch(r::RemoteRef) = msg_roundtrip(:fetch, r)

## higher-level functions ##

at_each(fname::Symbol, args...) = at_each(PGRP, fname, args...)

function at_each(grp::ProcessGroup, fname, args...)
    w = grp.workers
    np = length(w)
    fut = cell(np)
    for i=1:np
        remote_do(w[i], fname, args...)
    end
end

pmap(f::Symbol, lst) = pmap(PGRP, f, lst)

function pmap(grp::ProcessGroup, f, lst)
    np = length(grp.workers)
    { remote_call(grp.workers[(i-1)%np+1], f, lst[i]) |
     i = 1:length(lst) }
end

## worker event loop ##

struct WorkItem
    thunk
    task   # the Task working on this item, or ()
    done::Bool
    result
    notify
    argument  # value to pass task next time it is restarted

    WorkItem(thunk) = new(thunk, (), false, (), (), ())
end

struct FinalValue
    value
end

struct WaitFor
    msg::Symbol
    ref::RemoteRef
end

# to be used as a re-usable Task for executing thunks
# if a work item finishes, you get a FinalValue. if you get something else,
# the thunk was interrupted and is not done yet.
function taskrunner()
    parent = current_task().parent
    result = ()
    while true
        (parent, thunk) = yieldto(parent, FinalValue(result))
        result = thunk()
    end
end

function deliver_result(sock, msg, oid, value)
    if is(msg,:fetch) || isa(value,Number)
        val = value
    else
        assert(is(msg, :sync))
        val = oid
    end
    send_msg(sock, (:result, (msg, oid, val)))
end

function perform_work(workqueue, waiting, runner)
    job = pop(workqueue)
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
                runner = Task(taskrunner, 512*1024)
                yieldto(runner)
            end
            job.task = runner
            result = yieldto(runner, current_task(), job.thunk)
        end
    catch e
        #show(e)
        print("exception on ", myid(), ": ")
        dump(e)
        result = FinalValue(e)
        job.task = ()  # task is toast. would be better to reuse it somehow.
    end
    if isa(result,FinalValue)
        # job done
        job.done = true
        job.result = result.value
        runner = job.task  # Task now free to be shared
        job.task = ()
        # do notifications
        while !is(job.notify,())
            (sock, msg, oid, job.notify) = job.notify
            deliver_result(sock, msg, oid, job.result)
        end
    else
        # job interrupted
        if is(job.task,runner)
            # need to continue, so this task can't be shared yet
            runner = ()
        end
        if isa(result,WaitFor)
            # add to waiting set to wait on a sync event
            wf::WaitFor = result
            oid = (wf.ref.whence, wf.ref.id)
            waiting[oid] = (wf.msg, job, get(waiting, oid, ()))
        else
            # otherwise return to queue
            enq(workqueue, job)
        end
    end
    return (workqueue, waiting, runner)
end

function jl_worker_loop(accept_fd)
    sockets = HashTable()  # connections to peers
    fdset = FDSet()        # set of FDs for a select call
    refs = HashTable()     # locally-owned objects with remote refs
    waiting = HashTable()  # refs our tasks are waiting for events on
    workqueue = Queue()    # queue of runnable tasks
    runner = ()            # a reusable Task object

    while true
        del_all(fdset)
        add(fdset, accept_fd)
        for (fd,_) = sockets
            add(fdset, fd)
        end

        # if no work to do, block waiting for requests. otherwise just poll,
        # so we can get right to work if there are no new requests.
        nselect = select_read(fdset, isempty(workqueue) ? 2 : 0)
        if nselect == 0
            # no i/o requests; do some work
            if !isempty(workqueue)
                (workqueue, waiting, runner) =
                    perform_work(workqueue, waiting, runner)
            end
        end

        if has(fdset, accept_fd)
            connectfd = ccall(dlsym(libc, :accept), Int32,
                              (Int32, Ptr{Void}, Ptr{Void}),
                              accept_fd, C_NULL, C_NULL)
            #print("accepted.\n")
            if connectfd==-1
                print("accept error: ", strerror(), "\n")
            else
                sockets[connectfd] = fdio(connectfd)
            end
        end

        for (fd, sock) = sockets
            if has(fdset, fd) || nb_available(sock)>0
                #print("nb= ", nb_available(sock), "\n")
                try
                    (msg, args) = recv_msg(sock)
                    #print("$(myid()) got ", tuple(msg, args[1],
                    #                              map(typeof,args[2:])), "\n")
                    # handle message
                    if is(msg, :call)
                        id = args[1]
                        f = args[2]
                        let func=eval(f), ar=args[3:]
                            wi = WorkItem(()->apply(func,ar))
                            refs[id] = wi
                            enq(workqueue, wi)
                        end
                    elseif is(msg, :do)
                        f = args[1]
                        if is(f,:identify_socket)
                            # special case
                            args = (0, args[2], fd, sock)
                        elseif is(f,:join_process_group)
                            # special case
                            args = (0, args[2], args[3], sockets)
                        end
                        let func=eval(f), ar=args[2:]
                            enq(workqueue, WorkItem(()->apply(func,ar)))
                        end
                    elseif is(msg, :result)
                        # used to deliver result of sync or fetch
                        mkind = args[1]
                        oid = args[2]
                        val = args[3]
                        # restart task that's waiting on oid
                        jobs = get(waiting,oid,())
                        newjobs = ()  # waiting list with one removed
                        found = false
                        while !is(jobs,())
                            if jobs[1]==mkind && !found
                                found = true
                                job = jobs[2]
                                job.argument = val
                                enq(workqueue,job)
                            else
                                newjobs = (jobs[1], jobs[2], newjobs)
                            end
                            jobs = jobs[3]
                        end
                        waiting[oid] = newjobs
                    else
                        # the synchronization messages
                        rr = args[1]::RemoteRef
                        oid = (rr.whence, rr.id)
                        wi = refs[oid]
                        if wi.done
                            deliver_result(sock, msg, oid, wi.result)
                        else
                            # add to WorkItem's notify list
                            wi.notify = (sock, msg, oid, wi.notify)
                        end
                    end
                catch e
                    if isa(e,EOFError)
                        #print("eof. exiting\n")
                        return()
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
    end
end

## worker creation and setup ##

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
function start_worker(wrfd)
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

    jl_worker_loop(sockfd)

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

function start_remote_worker(host)
    proc = `ssh -f julia@$host julia -e 'start_worker(1)'`
    port = 9009 # TODO: run proc and read real port
    port
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

## demos ##

# g = make_process_group(3)
# fv(a)=eig(a)[2][2]
# A=randn(800,800);A=A*A';
# pmap(g, :fv, {A,A,A})

all2all() = at_each(:hello_from, myid())

hello_from(i) = print("message from $i to $(myid())\n")

BARRIER_COUNT = 0
hit_barrier() = (global BARRIER_COUNT; BARRIER_COUNT+=1; ())
function barrier()
    print("$(myid()) reached barrier\n")
    global PGRP, BARRIER_COUNT
    at_each(:hit_barrier)
    while BARRIER_COUNT < length(PGRP.workers)
        yieldto(current_task().parent)
    end
    BARRIER_COUNT = 0
    print("$(myid()) passed barrier\n")
end

function barrier_demo()
    system("sleep $(myid())")
    barrier()
end
