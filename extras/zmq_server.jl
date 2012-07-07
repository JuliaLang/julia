require("zmq.jl")
# If you want to overload/extend the serializer, load your new
# serializer file(s) here

function zmq_respond_error(responder::ZMQSocket, thiserr::Exception, msg::ASCIIString)
    if isa(thiserr, ZMQStateError)
        # ZMQ state is corrupted, so we can't reliably report to the
        # client. Report this error on the server.
        throw(thiserr)
    else
        # Report the error to the client
        flag = isempty(msg) ? 0 : ZMQ_SNDMORE
        zmq_serialize(responder, thiserr, flag)
        if !isempty(msg)
            send(responder, ZMQMessage(msg))
        end
    end
end
zmq_respond_error(responder::ZMQSocket, thiserr::Exception) = zmq_respond_error(responder, thiserr, "")

function run_server(endpoint::ASCIIString)
    zctx = ZMQContext()
    responder = ZMQSocket(zctx, ZMQ_REP)
    zmq_bind(responder, endpoint)

    while true
        # Get the next command
        local ex
        local ismulti
        try
            ex, ismulti = zmq_deserialize(responder)
        catch thiserr
            zmq_respond_error(responder, thiserr, "Remote error: deserializing inputs")
            continue
        end
        # Execute the command
        local ret
        try
            ret = eval(ex)
        catch thiserr
            zmq_respond_error(responder, thiserr, "Remote error: executing command")
            continue
        end
        # Send the results back
        if isequal(ret, nothing)
            # There is no return, just send an empty acknowledgement
            send(responder, ZMQMessage(0))
        elseif isa(ret, Tuple)
            # Multiple outputs, serialize them each individually
            flag = ZMQ_SNDMORE
            for i = 1:length(ret)
                if i == length(ret)
                    flag = 0
                end
                try
                    zmq_serialize(responder, ret[i], flag)
                catch thiserr
                    zmq_respond_error(responder, thiserr, "Remote error: serializing output")
                    break
                end
            end
        else
            # Single output
            try
                zmq_serialize(responder, ret, 0)
            catch thiserr
                zmq_respond_error(responder, thiserr, "Remote error: serializing output")
            end
        end
    end
end
run_server() = run_server("tcp://*:5555")
