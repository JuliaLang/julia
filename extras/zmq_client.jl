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

function zmq_spawn(requester, ex)
    if !isa(requester, ZMQSocket)
        error("Must use a ZMQSocket")
    end
    if !isa(ex, Expr)
        error("Must send an expression")
    end
    if ex.head != :call
        error("The expression must be a function call, with no return values")
    end
    flag = ZMQ_SNDMORE
    for i = 1:length(ex.args)
        if i == length(ex.args)
            flag = 0
        end
        zmq_serialize(requester, ex.args[i], flag)
    end
    ret, ismulti = zmq_deserialize(requester)
    if ismulti
        # Convert to tuple
        return ntuple(length(ret), i->ret[i])
    else
        return ret
    end
end
