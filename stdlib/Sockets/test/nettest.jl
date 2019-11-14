# This file is a part of Julia. License is MIT: https://julialang.org/license

using Distributed, Sockets

# Run various networking tests checking to see how we perform under large loads
addprocs(1)

function test_connect_disconnect(exp)
    print("Testing 10^$exp connect/disconnects:\n")

    (port, server) = listenany(8000)
    server_started = RemoteChannel()
    server_exited = RemoteChannel()
    client_exited = RemoteChannel()

    @async begin
        clients_served = 0
        print("\t\t\t[SERVER] Started on port $(port), with PID $(getpid())\n")
        put!(server_started, false)
        while (clients_served < 10^exp)
            close(accept(server))
            clients_served += 1
        end
        put!(server_exited, true)
        print("\t\t\t[SERVER] Finished serving $(clients_served) clients\n")
    end

    # Wait for the server
    @spawnat(2, begin
        take!(server_started)
        print("[CLIENT] Connecting repeatedly to port $(port)\n")
        for i in 1:10^exp
            close(connect("localhost", port))
        end
        print("[CLIENT] Finished with 10^$exp connections\n")
        put!(client_exited,true)
    end)

    fetch(client_exited)
    close(server)
    fetch(server_exited)
    print("OK\n")
end

# Perform first test
test_connect_disconnect(5)


function test_send(exp)
    (port, server) = listenany(8000)

    @assert exp > 4
    size = 10^exp
    block = 10^(exp - 4)

    print("Testing open, send of 10^$exp bytes and closing:\n")

    server_started = RemoteChannel()
    server_exited = RemoteChannel()
    client_exited = RemoteChannel()

    @async begin
        print("\t\t\t[SERVER] Started on port $(port)\n")
        put!(server_started, false)
        serv_sock = accept(server)
        bread = 0
        while bread < size
            serv_data = read(serv_sock, UInt8, block)
            @assert length(serv_data) == block
            bread += block
        end
        close(serv_sock)
        print("\t\t\t[SERVER] Received 10^$(log10(bread))B of 10^$(exp)B\n")
        put!(server_exited, bread)
    end

    @spawnat(2, begin
        # wait for the server
        take!(server_started)
        print("[CLIENT] Connecting to port $(port)\n")
        cli_sock = connect("localhost", port)
        data = fill!(zeros(UInt8, block), Int8(65))
        cli_bsent = 0
        while cli_bsent < size
            write(cli_sock, data)
            cli_bsent += block
        end
        close(cli_sock)
        print("[CLIENT] Transmitted 10^$(log10(cli_bsent))B of 10^$(exp)B\n")
        put!(client_exited, cli_bsent)
    end)

    brcvd = take!(server_exited)
    bsent = take!(client_exited)
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
@everywhere function xfer(s, exp)
    @assert exp > 4
    xfer_size = 10^exp
    xfer_block = 10^(exp - 4)

    bsent = 0
    bread = 0

    # Create an asynchronous task that can modify bread properly
    recv_task = @async begin
        while bread < xfer_size
            data = read(s, UInt8, xfer_block)
            @assert length(data) == xfer_block
            bread += xfer_block
        end
    end

    send_task = @async begin
        # write in chunks of xfer_block
        data = fill!(zeros(UInt8, xfer_block), Int8(65))
        while bsent < xfer_size
            write(s, data)
            bsent += xfer_block
        end
    end
    fetch(recv_task)
    fetch(send_task)

    return (bsent, bread)
end

function test_bidirectional(exp)
    print("Testing 10^$exp bytes of concurrent bidirectional transfers:\n")
    (port, server) = listenany(8000)

    # For both the server and the client, we will transfer/receive 10^exp bytes
    server_started = RemoteChannel()
    server_exited = RemoteChannel()
    client_exited = RemoteChannel()

    @async begin
        print("\t\t\t[SERVER] Started on port $(port)\n")
        put!(server_started, true)
        server_sock = accept(server)
        (bsent, bread) = xfer(server_sock, exp)
        close(server_sock)
        print("\t\t\t[SERVER] Transmitted 10^$(log10(bsent))B and received 10^$(log10(bread))B\n")
        put!(server_exited, (bsent, bread))
    end

    @spawnat(2, begin
        # Wait for the server
        take!(server_started)
        print("[CLIENT] Connecting to port $(port)\n")
        client_sock = connect("localhost", port)
        (bsent, bread) = xfer(client_sock, exp)
        close(client_sock)
        print("[CLIENT] Transmitted 10^$(log10(bsent))B and received 10^$(log10(bread))B\n")
        put!(client_exited, (bsent,bread))
    end)

    (serv_bsent, serv_bread) = take!(server_exited)
    (cli_bsent, cli_bread) = take!(client_exited)
    close(server)

    if serv_bsent != cli_bread || serv_bread != cli_bsent
        print("\t[ERROR] Data was not faithfully transmitted!")
    else
        print("OK\n")
    end
end

# Test 1GB of bidirectional data
test_bidirectional(9)
