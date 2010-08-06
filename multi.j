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

function start_worker()
    port = ccall(dlsym(JuliaDLHandle,"jl_start_worker"), Int16, ())
    fd = ccall(dlsym(JuliaDLHandle,"connect_to_host"), Int32,
               (Ptr{Uint8}, Int16), "localhost", port)

    return fdio(fd)
end

function worker_demo()
    sock = start_worker()

    send_msg(sock, "hello, worker process.")
    reply = recv_msg(sock)
    print("got reply: ", reply, "\n")

    send_msg(sock, "quit")
end
