# This file is a part of Julia. License is MIT: http://julialang.org/license

using ZMQ

import Base: launch, manage, connect, kill

const BROKER_SUB_PORT = 8100
const BROKER_PUB_PORT = 8101

const SELF_INITIATED = 0
const REMOTE_INITIATED = 1

const PAYLOAD_MSG = "J"
const CONTROL_MSG = "Z"

const REQUEST_ACK = "R"
const ACK_MSG = "A"
const KILL_MSG = "K"

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

    ccall((:zmq_proxy, :libzmq), Cint,  (Ptr{Void}, Ptr{Void}, Ptr{Void}), xsub.data, xpub.data, C_NULL)
#    proxy(xsub, xpub)

    # control never comes here
    ZMQ.close(xpub)
    ZMQ.close(xsub)
    ZMQ.close(ctx)
end

function recv_data()
    try
        #println("On $(manager.zid_self) waiting to recv message")
        zid = parse(Int,String(ZMQ.recv(manager.sub)))
        assert(zid == manager.zid_self)

        from_zid = parse(Int,String(ZMQ.recv(manager.sub)))
        mtype = String(ZMQ.recv(manager.sub))

        #println("$zid received message of type $mtype from $from_zid")

        data = ZMQ.recv(manager.sub)
        if mtype == CONTROL_MSG
            cmsg = String(data)
            if cmsg == REQUEST_ACK
                #println("$from_zid REQUESTED_ACK from $zid")
                # send back a control_msg
                send_data(from_zid, CONTROL_MSG, ACK_MSG)
            elseif cmsg == ACK_MSG
                #println("$zid got ACK_MSG from $from_zid")
                (r_s, w_s, test_remote) = manager.map_zmq_julia[from_zid]
                manager.map_zmq_julia[from_zid] = (r_s, w_s, false)
            elseif cmsg == KILL_MSG
                exit(0)
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
                unsafe_write(r_s, pointer(data), length(data))
            end
        catch e
            Base.show_backtrace(STDOUT,catch_backtrace())
            println(e)
            rethrow(e)
        end
    end

    addprocs(manager; np=np)
end


function launch(manager::ZMQCMan, params::Dict, launched::Array, c::Condition)
    #println("launch $(params[:np])")
    for i in 1:params[:np]
        io, pobj = open(`$(params[:exename]) worker.jl $i $(Base.cluster_cookie())`, "r")

        wconfig = WorkerConfig()
        wconfig.userdata = Dict(:zid=>i, :io=>io)
        push!(launched, wconfig)
        notify(c)
    end
end

function connect(manager::ZMQCMan, pid::Int, config::WorkerConfig)
    #println("connect_m2w")
    if myid() == 1
        zid = get(config.userdata)[:zid]
        config.connect_at = zid # This will be useful in the worker-to-worker connection setup.

        print_worker_stdout(get(config.userdata)[:io], pid)
    else
        #println("connect_w2w")
        zid = get(config.connect_at)
        config.userdata = Dict{Symbol, Any}(:zid=>zid)
    end

    streams = setup_connection(zid, SELF_INITIATED)

    udata = get(config.userdata)
    udata[:streams] = streams

    streams
end

# WORKER
function start_worker(zid, cookie)
    #println("start_worker")
    Base.init_worker(cookie, ZMQCMan())
    init_node(zid)

    while true
        (from_zid, data) = recv_data()

        #println("worker recv data from $from_zid")

        streams = get(manager.map_zmq_julia, from_zid, nothing)
        if streams === nothing
            # First time..
            (r_s, w_s) = setup_connection(from_zid, REMOTE_INITIATED)
            Base.process_messages(r_s, w_s)
        else
            (r_s, w_s, t_r) = streams
        end

        unsafe_write(r_s, pointer(data), length(data))
    end
end

function manage(manager::ZMQCMan, id::Int, config::WorkerConfig, op)
    nothing
end

function kill(manager::ZMQCMan, pid::Int, config::WorkerConfig)
    send_data(get(config.userdata)[:zid], CONTROL_MSG, KILL_MSG)
    (r_s, w_s) = get(config.userdata)[:streams]
    close(r_s)
    close(w_s)

    # remove from our map
    delete!(manager.map_zmq_julia, get(config.userdata)[:zid])

    nothing
end


function print_worker_stdout(io, pid)
    @schedule while !eof(io)
        line = readline(io)
        println("\tFrom worker $(pid):\t$line")
    end
end

