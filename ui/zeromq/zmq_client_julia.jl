require("zeromq/zmq_serialize_julia")

function launch_client(endpoint::ASCIIString)
    zctx = ZMQContext()
    requester = ZMQSocket(zctx, ZMQ_REQ)
    connect(requester, endpoint)
    return zctx, requester
end
launch_client() = launch_client("tcp://localhost:5555")

# Run svd remotely:
#   A = randn(3,5)
#   U, S, V = zmqcall(requester, :svd, A)
function zmqcall(requester::ZMQSocket, func::Symbol, args...)
    ex = expr(:call, {func, args...})
    zmq_serialize(requester, ex)
    zmq_deserialize(requester)
end

# Remotely parse a string and execute it, e.g., 
#    str = "x = randn(7); sort(x)"
#    y = zmqparse(requester, str)
# Within Julia it may be better to use a quote block, but this will
# surely be the easy way from other languages
function zmqparse(requester::ZMQSocket, str::ASCIIString)
    ex = :(parse_eval($str))
    zmq_serialize(requester, ex)
    zmq_deserialize(requester)
end

# Set a variable in the remote session
function zmqsetvar(requester::ZMQSocket, var::Symbol, val)
    ex = :($var = $val)
    zmq_serialize(requester, ex)
    zmq_deserialize(requester)
end

# Get a variable in the remote session
function zmqgetvar(requester::ZMQSocket, var::Symbol)
    ex = :($var)
    zmq_serialize(requester, ex)
    zmq_deserialize(requester)
end
