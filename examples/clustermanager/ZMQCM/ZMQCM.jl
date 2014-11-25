module ZMQCM

import Base: launch, manage, connect_m2w, connect_w2w
export connect_m2w, connect_w2w, launch, manage, start_broker, start_master, start_worker

using ZMQ

const BROKER_SUB_PORT = 8100
const BROKER_PUB_PORT = 8101

const SELF_INITIATED = 0
const REMOTE_INITIATED = 1

const PAYLOAD_MSG = "J"
const CONTROL_MSG = "Z"

const REQUEST_ACK = "R"
const ACK_MSG = "A"

type ZMQCMan <: ClusterManager
    map_zmq_julia::Dict{Int, Tuple}
    c::Condition
    isfree::Bool
    ctx
    pub
    sub
    zid_self
    ZMQCMan() = new(Dict{Int, Tuple}(), Condition(), true)
end

const manager = ZMQCMan()

function lock_for_send()
    if manager.isfree == true
        manager.isfree = false
    else
        while manager.isfree == false
            wait(manager.c)
            if manager.isfree == true
                manager.isfree = false
                return
            end
        end
    end
end

function release_lock_for_send()
    manager.isfree = true
    notify(manager.c, all=true)
end

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

function send_data(zid, mtype, data)
    lock_for_send()
    ZMQ.send(manager.pub, Message(string(zid)), SNDMORE)
    ZMQ.send(manager.pub, Message(string(manager.zid_self)), SNDMORE)
    #println("Sending message of type $mtype to $zid")
    ZMQ.send(manager.pub, Message(mtype), SNDMORE)
    ZMQ.send(manager.pub, Message(data))
    release_lock_for_send()
end

function setup_connection(zid, initiated_by)
    try
        read_stream=BufferStream()
        write_stream=BufferStream()

        if initiated_by == REMOTE_INITIATED
            test_remote = false
        else
            test_remote = true
        end

        manager.map_zmq_julia[zid] = (read_stream, write_stream, test_remote)

        @schedule begin
            while true
                (r_s, w_s, do_test_remote) = manager.map_zmq_julia[zid]
                if do_test_remote
                    send_data(zid, CONTROL_MSG, REQUEST_ACK)
                    sleep(0.5)
                else
                    break
                end
            end
            (r_s, w_s, do_test_remote) = manager.map_zmq_julia[zid]

            while true
                data = readavailable(w_s)
                send_data(zid, PAYLOAD_MSG, data)
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

function recv_data()
    try
        #println("On $(manager.zid_self) waiting to recv message")
        zid = int(bytestring(ZMQ.recv(manager.sub)))
        assert(zid == manager.zid_self)

        from_zid = int(bytestring(ZMQ.recv(manager.sub)))
        mtype = bytestring(ZMQ.recv(manager.sub))

        #println("$zid received message of type $mtype from $from_zid")

        data = ZMQ.recv(manager.sub)
        if mtype == CONTROL_MSG
            cmsg = bytestring(data)
            if cmsg == REQUEST_ACK
                #println("$from_zid REQUESTED_ACK from $zid")
                # send back a control_msg
                send_data(from_zid, CONTROL_MSG, ACK_MSG)
            elseif cmsg == ACK_MSG
                #println("$zid got ACK_MSG from $from_zid")
                (r_s, w_s, test_remote) = manager.map_zmq_julia[from_zid]
                manager.map_zmq_julia[from_zid] = (r_s, w_s, false)
            else
                error("Unknown control message : ", cmsg)
            end
            data = ""
        end

        (from_zid, data)
    catch e
        Base.show_backtrace(STDOUT,catch_backtrace())
        println(e)
        rethrow(e)
    end

end

# MASTER
function start_master(np)
    init_node()
    @schedule begin
        try
            while true
                (from_zid, data) = recv_data()

                #println("master recv data from $from_zid")

                (r_s, w_s, t_r) = manager.map_zmq_julia[from_zid]
                write(r_s, convert(Ptr{Uint8}, data), length(data))
            end
        catch e
            Base.show_backtrace(STDOUT,catch_backtrace())
            println(e)
            rethrow(e)
        end
    end

    addprocs(np; manager=manager)
end


function launch{T<:ZMQCMan}(::Type{T}, config::Dict, launched::Array, c::Condition)
    #println("launch{T<:ZMQCMan}")
    l2 = []
    for i in 1:config[:np]
        io, pobj = open(`julia worker.jl $i`, "r")
        wconfig = copy(config)
        wconfig[:zid] = i
        wconfig[:io] = io
        push!(l2, wconfig)
    end
    append!(launched, l2)
    notify(c)
end

function connect_m2w{T<:ZMQCMan}(::Type{T}, pid::Int, config::Dict)
    #println("connect_m2w")
    zid = config[:zid]
    manager = config[:manager]

    print_worker_stdout(config[:io], pid)
    (r_s, w_s) = setup_connection(zid, SELF_INITIATED)

    config[:connect_at] = zid # This will be useful in the worker-to-worker connection setup.
    (r_s, w_s)
end

# WORKER
function start_worker(zid)
    #println("start_worker")
    Base.init_worker(ZMQCMan)
    init_node(zid)

    while true
        (from_zid, data) = recv_data()

        #println("worker recv data from $from_zid")

        streams = get(manager.map_zmq_julia, from_zid, nothing)
        if streams == nothing
            # First time..
            (r_s, w_s) = setup_connection(from_zid, REMOTE_INITIATED)
            Base.process_messages(r_s, w_s)
        else
            (r_s, w_s, t_r) = streams
        end

        write(r_s, convert(Ptr{Uint8}, data), length(data))
    end
end

function connect_w2w{T<:ZMQCMan}(::Type{T}, pid::Int, config::Dict)
    #println("connect_w2w")
    zid = config[:connect_at]
    setup_connection(zid, SELF_INITIATED)
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

# function test_broker(zself, zrem)
#     init_node(zself)
#     (r, w) = setup_connection(zrem)
#
#     @schedule begin
#         while true
#             println("Sending from ", zself)
#             write(w, "FROM : $zself")
#             sleep(1)
#         end
#     end
#     while true
#         zid = int(bytestring(ZMQ.recv(manager.sub)))
#         data = ZMQ.recv(manager.sub)
#
#         println("recv data ", ASCIIString(data), ", zid: $zid")
#     end
# end
#

end