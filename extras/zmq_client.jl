require("zmq.jl")
# If you want to overload/extend the serializer, load your new
# serializer file(s) here

function launch_client(endpoint::ASCIIString)
    zctx = ZMQContext()
    requester = ZMQSocket(zctx, ZMQ_REQ)
    zmq_connect(requester, endpoint)

    return zctx, requester
end
launch_client() = launch_client("tcp://localhost:5555")

function zmqcall(requester::ZMQSocket, func::Symbol, args...)
    flag = ZMQ_SNDMORE
    if isempty(args)
        flag = 0
    end
    zmq_serialize(requester, func, flag)
    for i = 1:length(args)
        if i == length(args)
            flag = 0
        end
        zmq_serialize(requester, args[i], flag)
    end
    ret, ismulti = zmq_deserialize(requester)
    if ismulti
        # Convert to tuple
        return ntuple(length(ret), i->ret[i])
    else
        return ret
    end
end
