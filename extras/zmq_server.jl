require("zmq.jl")
# If you want to overload/extend the serializer, load your new
# serializer file(s) here

function run_server(endpoint::ASCIIString)
    zctx = ZMQContext()
    responder = ZMQSocket(zctx, ZMQ_REP)
    zmq_bind(responder, endpoint)

    while true
        # Get the next command
        args, ismulti = zmq_deserialize(responder)
        if !ismulti
            args = {args}
        end
        # Execute the command
        ret = eval(expr(:call, args))
        # Send the results back
        if isa(ret, Tuple)
            flag = ZMQ_SNDMORE
            for i = 1:length(ret)
                if i == length(ret)
                    flag = 0
                end
                zmq_serialize(responder, ret[i], flag)
            end
        else
            zmq_serialize(responder, ret, 0)
        end
    end
end
run_server() = run_server("tcp://*:5555")
