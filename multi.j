recv_msg(s) = deserialize(s)
send_msg(s, x) = (serialize(s, x); flush(s))

wait_msg(fd) =
    ccall(dlsym(JuliaDLHandle,"jl_wait_msg"), Int32, (Int32,), fd)==0

# todo:
# - recover from i/o errors
# - handle remote execution errors
# - send pings at some interval to detect failed/hung machines
# - asynch i/o with coroutines to overlap computing with communication

function jl_worker(fd)
    sock = fdio(fd)

    while true
        if !wait_msg(fd)
            break
        end
        (f, args) = recv_msg(sock)
        # handle message
        result = apply(eval(f), args)
        send_msg(sock, result)
    end
end

function start_local_worker()
    fds = Array(Int32, 2)
    ccall(dlsym(libc,"pipe"), Int32, (Ptr{Int32},), fds)
    rdfd = fds[1]
    wrfd = fds[2]

    if fork()==0
        port = Array(Int16, 1)
        port[1] = 9009
        sockfd = ccall(dlsym(JuliaDLHandle,"open_any_tcp_port"), Int32,
                       (Ptr{Int16},), port)
        if sockfd == -1
            error("could not bind socket")
        end
        io = fdio(wrfd)
        write(io, port[1])
        close(io)
        # close stdin; workers will not use it
        ccall(dlsym(libc,"close"), Int32, (Int32,), 0)

        connectfd = ccall(dlsym(libc,"accept"), Int32,
                          (Int32, Ptr{Void}, Ptr{Void}),
                          sockfd, C_NULL, C_NULL)
        jl_worker(connectfd)
        ccall(dlsym(libc,"close"), Int32, (Int32,), connectfd)
        ccall(dlsym(libc,"close"), Int32, (Int32,), sockfd)
        ccall(dlsym(libc,"exit") , Void , (Int32,), 0)
    end
    io = fdio(rdfd)
    port = read(io, Int16)
    close(io)
    #print("started worker on port ", port, "\n")
    port
end

function connect_to_worker(hostname, port)
    fdio(ccall(dlsym(JuliaDLHandle,"connect_to_host"), Int32,
               (Ptr{Uint8}, Int16), hostname, port))
end

struct Worker
    host::String
    port::Int16
    socket::IOStream
    requests::Queue
    busy::Bool
    maxid::Int32
    pending::Int32
    completed::BTree{Int32,Any}

    function Worker()
        host = "localhost"
        port = start_local_worker()
        sock = connect_to_worker(host, port)
        new(host, port, sock, Queue(), false, 0, 0, BTree(Int32,Any))
    end
end

struct Future
    where::Worker
    id::Int32
    done::Bool
    val
end

function remote_apply(w::Worker, f, args...)
    w.maxid += 1
    nid = w.maxid
    if !w.busy
        send_msg(w.socket, (f, args))
        w.pending = nid
        w.busy = true
    else
        enq(w.requests, Pair(nid, (f, args)))
    end
    Future(w, nid, false, ())
end

function wait(f::Future)
    if f.done
        return f.val
    end
    w = f.where
    if has(w.completed,f.id)
        v = w.completed[f.id]
        del(w.completed,f.id)
        f.done = true
        f.val = v
        return v
    end
    while true
        if !w.busy
            error("invalid Future")
        end
        current = w.pending
        result = recv_msg(w.socket)

        if isempty(w.requests)
            w.busy = false
            w.pending = 0
        else
            # handle next request
            p = pop(w.requests)
            # send remote apply message p.b
            send_msg(w.socket, p.b)
            # w.busy = true  # already true
            w.pending = p.a
        end

        if current == f.id
            f.done = true
            f.val = result
            return result
        else
            # store result, allowing out-of-order retrieval
            w.completed[current] = result
        end
    end
end

function pmap(wpool, fname, lst)
    fut = { remote_apply(wpool[(i-1)%length(wpool)+1], fname, lst[i]) |
           i = 1:length(lst) }
    for i=1:length(fut)
        fut[i] = wait(fut[i])
    end
    fut
end
