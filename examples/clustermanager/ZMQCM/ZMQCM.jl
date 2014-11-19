module ZMQCM

import Base: launch, manage, connect_m2w, connect_w2w
export connect_m2w, connect_w2w, launch, manage, start_broker, start_master, start_worker

using ZMQ

const BROKER_SUB_PORT = 8100
const BROKER_PUB_PORT = 8101

type ZMQCMan <: ClusterManager
    map_zmq_julia::Dict{Int, Tuple}
    ctx
    pub
    sub
    zid_self
    ZMQCMan() = new(Dict{Int, Tuple}(),Dict{Int, Tuple}())
end

const manager = ZMQCMan()

function init_node(zid=0)
    manager.ctx = Context(1)
    pub=Socket(manager.ctx, PUB)    # Outbound
    connect(pub, "tcp://127.0.0.1:$BROKER_SUB_PORT")

    sub=Socket(manager.ctx, SUB)    # In bound
    connect(sub, "tcp://127.0.0.1:$BROKER_PUB_PORT")
    ZMQ.set_subscribe(sub, string(zid))

    manager.pub = pub
    manager.sub = sub
    manager.zid_self = zid

    (pub, sub)
end

function setup_connection(zid)
    try
        read_stream=BufferStream()
        write_stream=BufferStream()

        manager.map_zmq_julia[zid] = (read_stream, write_stream)

        @schedule begin
            while true
                data = readavailable(write_stream)

                msg = Message(data)

#                println("Sending from $(manager.zid_self) to $zid")

                ZMQ.send(manager.pub, Message(string(zid)), SNDMORE)
                ZMQ.send(manager.pub, Message(string(manager.zid_self)), SNDMORE)
                    ZMQ.send(manager.pub, msg)
            end
        end
        (read_stream, write_stream)
    catch e
        Base.show_backtrace(STDOUT,catch_backtrace())
        println(e)
        rethrow(e)
    end
end

# BROKER
function start_broker()
    ctx=Context(1)
    xpub=Socket(ctx, XPUB)
    xsub=Socket(ctx, XSUB)

    ZMQ.bind(xsub, "tcp://127.0.0.1:$(BROKER_SUB_PORT)")
    ZMQ.bind(xpub, "tcp://127.0.0.1:$(BROKER_PUB_PORT)")

    ccall((:zmq_proxy, :libzmq), Cint,  (Ptr{Void}, Ptr{Void}, Ptr{Void}), xpub.data, xsub.data, C_NULL)

    # control never comes here
    ZMQ.close(xpub)
    ZMQ.close(xsub)
    ZMQ.close(ctx)
end

# MASTER
function start_master(np)
    pub, sub = init_node()
    @schedule begin
        while true
            zid = int(bytestring(ZMQ.recv(sub)))
            assert(zid == manager.zid_self)

            from_zid = int(bytestring(ZMQ.recv(sub)))
            data = ZMQ.recv(sub)

#            println("master recv data from $from_zid")

            (r_s, w_s) = manager.map_zmq_julia[from_zid]
            write(r_s, convert(Ptr{Uint8}, data), length(data))
        end
    end

    addprocs(np; manager=manager)
end


function launch{T<:ZMQCMan}(::Type{T}, config::Dict, launched::Array, c::Condition)
#    println("launch{T<:ZMQCMan}")
    l2 = []
    for i in 1:config[:np]
        io, pobj = open(`julia worker.jl $i`, "r")
        wconfig = copy(config)
        wconfig[:zid] = i
        wconfig[:io] = io
        push!(l2, wconfig)
    end
    sleep(2)   # Give time for the workers to connect
    append!(launched, l2)
    notify(c)
end

function connect_m2w{T<:ZMQCMan}(::Type{T}, pid::Int, config::Dict)
#    println("connect_m2w")
    zid = config[:zid]
    manager = config[:manager]

    print_worker_stdout(config[:io], pid)
    (r_s, w_s) = setup_connection(zid)

    config[:connect_at] = zid # This will be useful in the worker-to-worker connection setup.
    (r_s, w_s)
end

# WORKER
function start_worker(zid)
#    println("start_worker")
    Base.init_worker(ZMQCMan)
    pub, sub = init_node(zid)

    while true
        zid = int(bytestring(ZMQ.recv(sub)))
        assert(zid == manager.zid_self)

        from_zid = int(bytestring(ZMQ.recv(sub)))
        data = ZMQ.recv(sub)

#        println("worker recv data from $from_zid")

        streams = get(manager.map_zmq_julia, from_zid, nothing)
        if streams == nothing
            # First time..
            (r_s, w_s) = setup_connection(from_zid)
            Base.process_messages(r_s, w_s)
        else
            (r_s, w_s) = streams
        end

        write(r_s, convert(Ptr{Uint8}, data), length(data))
    end
end

function connect_w2w{T<:ZMQCMan}(::Type{T}, pid::Int, config::Dict)
#    println("connect_w2w")
    zid = config[:connect_at]
    setup_connection(zid)
end


function manage{T<:ZMQCMan}(::Type{T}, id::Int, config::Dict, op)
    nothing
end

function print_worker_stdout(io, pid)
    @async begin
        while !eof(io)
            line = readline(io)
            print("\tFrom worker $(pid):\t$line")
        end
    end
end

function test_broker(zself, zrem)
    init_node(zself)
    (r, w) = setup_connection(zrem)

    @schedule begin
        while true
            println("Sending from ", zself)
            write(w, "FROM : $zself")
            sleep(1)
        end
    end
    while true
        zid = int(bytestring(ZMQ.recv(manager.sub)))
        data = ZMQ.recv(manager.sub)

        println("recv data ", ASCIIString(data), ", zid: $zid")
    end
end


end