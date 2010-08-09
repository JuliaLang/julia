function recv_msg(s)
    len = read(s, Int32)
    return read(s, Uint8, len)
end

function send_msg(s, msg)
    t = IOTally()
    write(t, msg)
    write(s, int32(t.nbytes))
    write(s, msg)
    flush(s)
end

wait_msg(fd) =
    ccall(dlsym(JuliaDLHandle,"jl_wait_msg"), Int32, (Int32,), fd)==0

function jl_worker(fd)
    sock = fdio(fd)

    while true
        if !wait_msg(fd)
            break
        end
        msg = recv_msg(sock)
        # handle message
        if msg == "quit"
            break
        end
        print("got message: ", msg, "\n")
        send_msg(sock, "OK")
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
    print("started worker on port ", port, "\n")
    port
end

function connect_to_worker(hostname, port)
    fdio(ccall(dlsym(JuliaDLHandle,"connect_to_host"), Int32,
               (Ptr{Uint8}, Int16), hostname, port))
end

function worker_demo()
    sock = connect_to_worker("localhost", start_local_worker())

    send_msg(sock, "hello, worker process.")
    reply = recv_msg(sock)
    print("got reply: ", reply, "\n")

    send_msg(sock, "quit")
    close(sock)
end
