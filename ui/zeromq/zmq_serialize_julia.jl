# Using Julia's built-in serializer as the ZMQ protocol

require("zeromq/zmq")

function zmq_serialize(socket::ZMQSocket, x, flag::Integer)
    s = memio()
    serialize(s, x)
    # Having built s, we'd like to avoid a second copy operation when
    # we create the ZMQMessage. This is possible using the ZMQ library
    # C function zmq_msg_init_data(), but this function requires a
    # callback as one of its arguments, and hence a C-wrapper. This
    # also requires that we prevent s from being garbage-collected
    # until the callback is called. So the C wrapper should increase
    # the reference count on s, and the callback should decrease it.
    # Here, for simplicity we just make another copy.
    zmsg = ZMQMessage(takebuf_string(s))
    send(socket, zmsg, flag)
end
zmq_serialize(socket::ZMQSocket, x) = zmq_serialize(socket, x, 0)

function zmq_deserialize(socket::ZMQSocket)
    zmsg = recv(socket)
    s = convert(IOStream, zmsg)
    seek(s, 0)  # rewind to beginning
    xf = deserialize(s)
    if isa(xf, Function)
        x = xf()  # deserializer returns a thunk
    else
        x = xf
    end
    # Exceptions should be thrown to support transmission of errors
    # across the socket
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
