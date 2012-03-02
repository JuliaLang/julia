type WorkItem
    thunk::Function
    task   # the Task working on this item, or ()
    done::Bool
    result
    notify::Tuple
    argument  # value to pass task next time it is restarted
    clientset::IntSet

    WorkItem(thunk::Function) = new(thunk, (), false, (), (), (), IntSet(64))
    WorkItem(task::Task) = new(()->(), task, false, (), (), (), IntSet(64))
end

type LocalProcess
end

type Location
    host::String
    port::Int16
    Location(h,p::Integer) = new(h,int16(p))
end

type Worker
    host::String
    port::Int16
    fd::Int32
    socket::AsyncStream
    sendbuf::IOStream
    id::Int
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    gcflag::Bool

    function Worker(host, port)
        fd = ccall(:connect_to_host, Int32,
                   (Ptr{Uint8}, Int16), host, port)
        if fd == -1
            error("could not connect to $host:$port, errno=$(errno())\n")
        end
        Worker(host, port, fd, fdio(fd, true))
    end

    Worker(host,port,fd,sock,id) = new(host, port, fd, sock, memio(), id,
                                       {}, {}, false)
    Worker(host,port,fd,sock) = Worker(host,port,fd,sock,0)
end

type ProcessGroup
    myid::Int
    workers::Array{Any,1}
    locs::Array{Any,1}
    np::Int

    # global references
    refs::HashTable

    function ProcessGroup(myid::Integer, w::Array{Any,1}, locs::Array{Any,1})
        return new(myid, w, locs, length(w), HashTable())
    end
end

type RemoteRef
    where::Int
    whence::Int
    id::Int
    # TODO: cache value if it's fetched, but don't serialize the cached value

    function RemoteRef(w, wh, id)
        r = new(w,wh,id)
        found = key(_jl_client_refs, r, false)
        if bool(found)
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
        rids = { rr2id(r[i]) | i=1:np }
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
    v = gensym()
    quote
        sync_begin()
        $v = $block
        sync_end()
        $v
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
        env = ccall(:jl_closure_env, Any, (Any,), thunk)
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
        foreach(x->find_vars(x,lst), e.args)
    end
    lst
end

# wrap an expression in "let a=a,b=b,..." for each var it references
function localize_vars(expr)
    v = find_vars(expr)
    # requires a special feature of the front end that knows how to insert
    # the correct variables. the list of free variables cannot be computed
    # from a macro.
    Expr(:localize,
         {:(()->($expr)), v...},
         Any)
end

macro spawn(expr)
    expr = localize_vars(:(()->($expr)))
    :(spawn($expr))
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
    :(spawnlocal($expr))
end

macro spawnat(p, expr)
    expr = localize_vars(:(()->($expr)))
    :(spawnat($p, $expr))
end


