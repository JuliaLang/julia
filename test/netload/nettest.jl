# Run various networking tests checking to see how we perform under large loads
addprocs(1)

function test_connect_disconnect(exp)
    print("Testing 10^$exp connect/disconnects:\n")

    (port, server) = listenany(8000)
    server_exited = RemoteRef()

    @spawnat(1, begin
        clients_served = 0
        print("\t[SERVER] Started on port $(port), with PID $(getpid())\n")
        put!(server_exited, false)
        while (clients_served < 10^exp)
            close(accept(server))
            clients_served += 1
        end
        put!(server_exited, true)
    end)

    # Wait for the server
    take!(server_exited)
    print("\t[CLIENT] Connecting repeatedly to port $(port)\n")
    for i in 1:10^exp
        close(connect("localhost", port))
    end
    print("\t[CLIENT] Finished with 10^$exp connections\n")

    close(server)
    fetch(server_exited)
    print("OK\n")
end

# Perform first test
test_connect_disconnect(4)




function test_send(exp)
    (port, server) = listenany(8000)

    @assert exp > 4
    size = 10^exp
    block = 10^(exp - 4)

    print("Testing open, send of 10^$exp bytes and closing:\n")

    rr_rcvd = RemoteRef()

    @spawnat(1, begin
        print("\t[SERVER] Started on port $(port)\n")
        
        put!(rr_rcvd, false)
        serv_sock = accept(server)
        bread = 0
        while bread < size
            serv_data = read(serv_sock, Uint8, block)
            @assert length(serv_data) == block
            bread += block
        end
        close(serv_sock)
        put!(rr_rcvd, bread)
        print("\t[SERVER] Received $(bread) of $(size) bytes\n")
    end)

    # wait for the server
    take!(rr_rcvd)
    print("\t[CLIENT] Connecting to port $(port)\n")
    cli_sock = connect("localhost", port)
    data = fill!(zeros(Uint8, block), int8(65))
    bsent = 0
    while bsent < size
        write(cli_sock, data)
        bsent += block
    end
    close(cli_sock)
    print("\t[CLIENT] Transmitted $(bsent) bytes\n")

    brcvd = fetch(rr_rcvd)
    close(server)

    if brcvd != bsent
        print("\t[ERROR] Received bytes ($(brcvd)) != sent bytes ($(bsent))\n")
    else
        print("OK\n")
    end
end

# Run second test on a gigabyte of data
test_send(9)



# Utility function for test_bidirectional() that simultaneously transmits and
# receives 10^exp bits of data over s
function xfer(s, exp)
    @assert exp > 4
    xfer_size = 10^exp
    xfer_block = 10^(exp - 4)

    bsent = 0
    bread = 0
    
    @sync begin
        # Create an asynchronous task that can modify bread properly
        recv_task = @task begin
            while bread < xfer_size
                data = read(s, Uint8, xfer_block)
                @assert length(data) == xfer_block
                bread += xfer_block
            end
        end
        Base.sync_add(recv_task)
        Base.enq_work(recv_task)
        
        send_task = @task begin
            # write in chunks of xfer_block
            data = fill!(zeros(Uint8, xfer_block), int8(65))
            while bsent < xfer_size
                write(s, data)
                bsent += xfer_block
            end
        end
        Base.sync_add(send_task)
        Base.enq_work(send_task)
    end

    return (bsent, bread)
end

function test_bidirectional(exp)
    print("Testing 10^$exp bytes of concurrent bidirectional transfers:\n")
    (port, server) = listenany(8000)

    # For both the server and the client, we will transfer/receive 10^exp bytes
    rr_server = RemoteRef()

    @spawnat(1, begin
        local bsent, bread
        print("\t[SERVER] Started on port $(port)\n")
        put!(rr_server, true)
        serv_sock = accept(server)
        (bsent, bread) = xfer(serv_sock, exp)
        close(serv_sock)
        put!(rr_server, (bsent, bread))
        print("\t[SERVER] Transmitted $(bsent) and received $(bread) bytes\n")
    end)

    # Wait for the server
    take!(rr_server)
    print("\t[CLIENT] Connecting to port $(port)\n")
    cli_sock = connect("localhost", port)
    (bsent, bread) = xfer(cli_sock, exp)
    print("\t[SERVER] Transmitted $(bsent) and received $(bread) bytes\n")
    close(cli_sock)

    (serv_bsent, serv_bread) = take!(rr_server)
    close(server)

    if serv_bsent != bread || serv_bread != bsent
        print("\t[ERROR] Data was not faithfully transmitted!")
    else
        print("OK\n")
    end
end

# Test 1GB of bidirectional data....
test_bidirectional(9)
