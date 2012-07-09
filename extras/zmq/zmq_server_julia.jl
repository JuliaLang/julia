require("zmq/zmq_serialize_julia.jl")

function zmq_respond_error(responder::ZMQSocket, thiserr::Exception)
    if isa(thiserr, ZMQStateError)
        # ZMQ state is corrupted, so we can't reliably report to the
        # client. Report this error on the server.
        throw(thiserr)
    else
        println("Reporting this error to the client: ", thiserr)
        # Report the error to the client
        zmq_serialize(responder, thiserr)
    end
end

function zmq_parse_eval(str::ASCIIString)
    p, indx = parse(str)
    if indx < length(str)
        error("Could not completely parse string", str)
    end
    eval(p)
end

function run_server(endpoint::ASCIIString)
    zctx = ZMQContext()
    responder = ZMQSocket(zctx, ZMQ_REP)
    zmq_bind(responder, endpoint)

    while true
        # Get the next command
        local ex
        local ismulti
        try
            ex = zmq_deserialize(responder)
        catch thiserr
            zmq_respond_error(responder, thiserr)
            continue
        end
        # Execute the command
        local ret
        try
            ret = eval(ex)
        catch thiserr
            zmq_respond_error(responder, thiserr)
            continue
        end
        # Send the results back
        try
            zmq_serialize(responder, ret)
        catch thiserr
            zmq_respond_error(responder, thiserr)
        end
    end
end
run_server() = run_server("tcp://*:5555")
