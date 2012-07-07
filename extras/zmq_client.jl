require("zmq.jl")
# If you want to override/extend the serializer, load your new
# serializer file(s) here

function launch_client(endpoint::ASCIIString)
    zctx = ZMQContext()
    requester = ZMQSocket(zctx, ZMQ_REQ)
    zmq_connect(requester, endpoint)

    return zctx, requester
end
launch_client() = launch_client("tcp://localhost:5555")

function _zmq_return_values(requester::ZMQSocket)
    ret, ismulti = zmq_deserialize(requester)
    if ismulti
        # Convert to tuple
        return ntuple(length(ret), i->ret[i])
    else
        return ret
    end
end

# Run svd remotely:
#   A = randn(3,5)
#   U, S, V = zmqcall(requester, :svd, A)
function zmqcall(requester::ZMQSocket, func::Symbol, args...)
    ex = expr(:call, {func, args...})
    zmq_serialize(requester, ex, 0)
    _zmq_return_values(requester)
end

# Remotely parse a string and execute it, e.g., 
#    str = "x = randn(7); sort(x)"
#    y = zmqparse(requester, str)
# Within Julia it's presumably better to use a quote block, but this
# simulates what will surely be the "easy way" from other languages
function zmqparse(requester::ZMQSocket, str::ASCIIString)
    zmsg = ZMQMessage("ToParse")
    send(requester, zmsg, ZMQ_SNDMORE)
    zmsg = ZMQMessage(str)
    send(requester, zmsg, 0)
    _zmq_return_values(requester)
end
