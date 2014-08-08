
addprocs(1)


exp=5
print("Testing repeated 10^$exp connect/disconnects....")

(port, server) = listenany(8000)

@spawnat(1, 
    begin
        while (true)
            close(accept(server))
        end
    end
    )

@sync begin
    @async begin 
        for i in 1:10^exp
            close(connect("localhost", port)); 
#            println("finished $i")
        end
    end
end

close(server)
println(" : OK")


(port, server) = listenany(8000)

exp=9
size = 10^exp
block = 10^(exp - 4)

print("Testing open, send of 10^$exp bytes and closing. Other side should recv complete amount...")

rr_rcd = RemoteRef()
rr_sent = RemoteRef()

@spawnat(1, 
    begin
        while (true)
            s=accept(server)
            bread = 0
            while bread < size
                data = read(s, Uint8, block)
                @assert length(data) == block
                bread += block
            end
#            println("process $(myid()) received $bread bytes")
            close(s)
            put!(rr_rcd, true)
        end
    end)


@spawnat(2, 
    begin
        s = connect("localhost", port)    
        data = fill!(zeros(Uint8, block), int8(65))    
        bwritten = 0
        while bwritten < size
            write(s, data)
            bwritten += block
        end
        close(s)
#        println("process $(myid()) sent $bwritten bytes")
        put!(rr_sent, true)
    end)

wait(rr_rcd)
wait(rr_sent)

close(server)

println(": OK")
    
    
require("sockxfer.jl")
xfer_exp = 9
print("Testing 10^$(xfer_exp) bytes of concurrent bidirectional transfers over a single socket connection...")


(port, server) = listenany(8000)

rr_server = RemoteRef()
rr_client = RemoteRef()


@spawnat(1, 
    begin
        while (true)
            s=accept(server) 
            @async begin xfer(s, xfer_exp); put!(rr_server, true); take!(rr_client); close(s); put!(rr_server, true);   end
        end
    end)



@spawnat(workers()[1], begin s = connect("localhost", port); xfer(s, xfer_exp); put!(rr_client, true); take!(rr_server); close(s); put!(rr_client, true);  end)

take!(rr_server)
take!(rr_client)

close(server)

println(" : OK")

@unix_only include("memtest.jl")
