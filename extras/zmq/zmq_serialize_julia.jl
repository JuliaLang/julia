# Using Julia's built-in serializer as the ZMQ protocol

require("zmq.jl")
require("iostring.jl")

function zmq_serialize(socket::ZMQSocket, x, flag::Integer)
    s = IOString()
    serialize(s, x)
    # Having built s, we'd like to avoid a second copy operation when
    # we create the ZMQMessage. This is possible using the ZMQ library
    # C function zmq_msg_init_data(), but this function requires a
    # callback as one of its arguments, and hence a C-wrapper. This
    # also requires that we prevent s from being garbage-collected
    # until the callback is called. So the C wrapper should increase
    # the reference count on s, and the callback should decrease it.
    # Here, for simplicity we just make another copy.
    zmsg = ZMQMessage(s.data)
    send(socket, zmsg, flag)
end
zmq_serialize(socket::ZMQSocket, x) = zmq_serialize(socket, x, 0)

function zmq_deserialize(socket::ZMQSocket)
    s = IOString(ASCIIString[recv(socket)])
    xf = deserialize(s)
    if isa(xf, Function)
        x = xf()  # deserializer returns a thunk
    else
        x = xf
    end
    # Exceptions should be thrown to support transmission of errors
    # across the socket
    # The utility of the error message would be higher if the server
    # attaches extra information. See the non-Julia version in
    # zmq_server.jl.
    if isa(x, Exception)
        throw(x)
    elseif isa(x, Tuple)
        for i = 1:length(x)
            if isa(x[i], Exception)
                throw(x[i])
            end
        end
    end
    return x
end
