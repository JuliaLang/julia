# Support for ZeroMQ, a network and interprocess communication library

_jl_libzmq = dlopen("libzmq")

_jl_zmq_version = dlsym(_jl_libzmq, :zmq_version)
_jl_zmq_init = dlsym(_jl_libzmq, :zmq_init)
_jl_zmq_term = dlsym(_jl_libzmq, :zmq_term)
_jl_zmq_errno = dlsym(_jl_libzmq, :zmq_errno)
_jl_zmq_strerror = dlsym(_jl_libzmq, :zmq_strerror)
_jl_zmq_socket = dlsym(_jl_libzmq, :zmq_socket)
_jl_zmq_close = dlsym(_jl_libzmq, :zmq_close)
_jl_zmq_getsockopt = dlsym(_jl_libzmq, :zmq_getsockopt)
_jl_zmq_setsockopt = dlsym(_jl_libzmq, :zmq_setsockopt)
_jl_zmq_bind = dlsym(_jl_libzmq, :zmq_bind)
_jl_zmq_connect = dlsym(_jl_libzmq, :zmq_connect)
_jl_zmq_msg_init_size = dlsym(_jl_libzmq, :zmq_msg_init_size)
_jl_zmq_msg_init = dlsym(_jl_libzmq, :zmq_msg_init)
_jl_zmq_msg_data = dlsym(_jl_libzmq, :zmq_msg_data)
_jl_zmq_msg_size = dlsym(_jl_libzmq, :zmq_msg_size)
_jl_zmq_msg_close = dlsym(_jl_libzmq, :zmq_msg_close)
_jl_zmq_send = dlsym(_jl_libzmq, :zmq_send)
_jl_zmq_recv = dlsym(_jl_libzmq, :zmq_recv)
const ZMQ_MAX_VSM_SIZE = 30
# This next should be replaced by a ccall, when packages can have C code
const _jl_zmq_msg_t_size = ZMQ_MAX_VSM_SIZE + sizeof(Uint) + 2


# A server will report most errors to the client over a ZMQSocket, but
# errors in ZMQ state can't be reported because the socket may be
# corrupted. Therefore, we need an exception type for errors that
# should be reported locally.
type ZMQStateError <: Exception
    msg::String
end
show(io, thiserr::ZMQStateError) = print(io, "ZMQ: ", thiserr.msg)

# Basic functions
function jl_zmq_error_str()
    errno = ccall(_jl_zmq_errno, Int, ())
    c_strerror = ccall (_jl_zmq_strerror, Ptr{Uint8}, (Int,), errno)
    if c_strerror != C_NULL
        strerror = cstring(c_strerror)
        return strerror
    else 
        return "Unknown error"
    end
end

let major = zeros(Int, 1), minor = zeros(Int, 1), patch = zeros(Int, 1)
global zmq_version
function zmq_version()
    ccall(_jl_zmq_version, Void, (Ptr{Int}, Ptr{Int}, Ptr{Int}), major, minor, patch)
    return (major[1], minor[1], patch[1])
end
end

# Contexts
type ZMQContext
    data::Ptr{Void}

    function ZMQContext(n::Integer)
        p = ccall(_jl_zmq_init, Ptr{Void},  (Int,), int(n))
        if p == C_NULL
	    throw(ZMQStateError(jl_zmq_error_str()))
        end
        zctx = new(p)
        finalizer(zctx, close)
        return zctx
    end
end
ZMQContext() = ZMQContext(1)
function close(ctx::ZMQContext)
    rc = ccall(_jl_zmq_term, Int,  (Ptr{Void},), ctx.data)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end 


# Sockets
type ZMQSocket
    data::Ptr{Void}

    function ZMQSocket(ctx::ZMQContext, typ::Integer)
        p = ccall(_jl_zmq_socket, Ptr{Void},  (Ptr{Void}, Int), ctx.data, int(typ))
        if p == C_NULL
	    throw(ZMQStateError(jl_zmq_error_str()))
        end
        socket = new(p)
        finalizer(socket, close)
        return socket
    end
end
function close(socket::ZMQSocket)
    rc = ccall(_jl_zmq_close, Int,  (Ptr{Void},), socket.data)
    if rc != 0
	throw(ZMQStateError(jl_zmq_error_str()))
    end
end

# Missing from below:
#   rcvtimeo
#   sndtimeo
#   fd

# Getting and setting socket options
# Socket options of integer type
let u64p = zeros(Uint64, 1), i64p = zeros(Int64, 1), ip = zeros(Int, 1), u32p = zeros(Uint32, 1), sz = zeros(Uint, 1)
for (fset, fget, k, p) in {
    (:zmq_setsockopt_hwm,          :zmq_getsockopt_hwm,           1, u64p)
    (:zmq_setsockopt_swap,         :zmq_getsockopt_swap,          3, i64p)
    (:zmq_setsockopt_affinity,     :zmq_getsockopt_affinity,      4, u64p)
    (:zmq_setsockopt_rate,         :zmq_getsockopt_rate,          8, i64p)
    (:zmq_setsockopt_recovery_ivl, :zmq_getsockopt_recovery_ivl,  9, i64p)
    (:_zmq_setsockopt_mcast_loop,  :_zmq_getsockopt_mcast_loop,  10, i64p)
    (:zmq_setsockopt_sndbuf,       :zmq_getsockopt_sndbuf,       11, u64p)
    (:zmq_setsockopt_rcvbuf,       :zmq_getsockopt_rcvbuf,       12, u64p)
    (:_zmq_setsockopt_rcvmore,     :_zmq_getsockopt_rcvmore,     13, i64p)
    (:zmq_setsockopt_events,       :zmq_getsockopt_events,       15, u32p)
    (:zmq_setsockopt_type,         :zmq_getsockopt_type,         16,   ip)
    (:zmq_setsockopt_linger,       :zmq_getsockopt_linger,       17,   ip)
    (:zmq_setsockopt_reconnect_ivl,:zmq_getsockopt_reconnect_ivl,18,   ip)
    (:zmq_setsockopt_backlog,      :zmq_getsockopt_backlog,      19,   ip)
    (:zmq_setsockopt_recovery_ivl_msec,:zmq_getsockopt_recovery_ivl_msec,20,i64p)
    (:zmq_setsockopt_reconnect_ivl_max,:zmq_getsockopt_reconnect_ivl_max,21,ip)
  }
    if fset != nothing
        @eval global ($fset)
        @eval function ($fset)(socket::ZMQSocket, option_val::Integer)
            ($p)[1] = option_val
            rc = ccall(_jl_zmq_setsockopt, Int,
                       (Ptr{Void}, Int, Ptr{Void}, Uint),
                       socket.data, $k, $p, sizeof(eltype($p)))
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
        end
    end
    if fget != nothing
        @eval global($fget)
        @eval function ($fget)(socket::ZMQSocket)
            ($sz)[1] = sizeof(eltype($p))
            rc = ccall(_jl_zmq_getsockopt, Int,
                       (Ptr{Void}, Int, Ptr{Void}, Ptr{Uint}),
                       socket.data, $k, $p, $sz)
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
            return int(($p)[1])
        end
    end        
end
end  # let
# For some functions, the publicly-visible verions should require &
# return boolean
zmq_setsockopt_mcast_loop(socket::ZMQSocket, val::Bool) = _zmq_setsockopt_mcast_loop(socket, val)
zmq_getsockopt_mcast_loop(socket::ZMQSocket) = bool(_zmq_getsockopt_mcast_loop(socket))
zmq_setsockopt_rcvmore(socket::ZMQSocket, val::Bool) = _zmq_setsockopt_rcvmore(socket, val)
zmq_getsockopt_rcvmore(socket::ZMQSocket) = bool(_zmq_getsockopt_rcvmore(socket))
# And now a convenience function
ismore(socket::ZMQSocket) = zmq_getsockopt_rcvmore(socket)


# Socket options of string type
let u8ap = zeros(Uint8, 255), sz = zeros(Uint, 1)
for (fset, fget, k) in {
    (:zmq_setsockopt_identity,     :zmq_getsockopt_identity,      5)
    (:zmq_setsockopt_subscribe,    nothing,                       6)
    (:zmq_setsockopt_unsubscribe,  nothing,                       7)
  }
    if fset != nothing
        @eval global ($fset)
        @eval function ($fset)(socket::ZMQSocket, option_val::ByteString)
            if length(option_val) > 255
                throw(ZMQStateError("option value too large"))
            end
            rc = ccall(_jl_zmq_setsockopt, Int,
                       (Ptr{Void}, Int, Ptr{Uint8}, Uint),
                       socket.data, $k, option_val, length(option_val))
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
        end
    end
    if fget != nothing
        @eval global ($fget)
        @eval function ($fget)(socket::ZMQSocket)
            ($sz)[1] = length($u8ap)
            rc = ccall(_jl_zmq_getsockopt, Int,
                       (Ptr{Void}, Int, Ptr{Uint8}, Ptr{Uint}),
                       socket.data, $k, $u8ap, $sz)
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
            return cstring(convert(Ptr{Uint8}, $u8ap), int(($sz)[1]))
        end
    end        
end
end  # let
    
function zmq_bind(socket::ZMQSocket, endpoint::String)
    rc = ccall(_jl_zmq_bind, Int, (Ptr{Void}, Ptr{Uint8}), socket.data, cstring(endpoint))
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end

function zmq_connect(socket::ZMQSocket, endpoint::String)
    rc=ccall(_jl_zmq_connect, Int, (Ptr{Void}, Ptr{Uint8}), socket.data, cstring(endpoint))
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end


# Messages
typealias ByteArray Union(Array{Uint8,1}, ByteString)
type ZMQMessage
    obj::Vector{Uint8}

    # Create an empty message (for receive)
    function ZMQMessage()
        obj = Array(Uint8, _jl_zmq_msg_t_size)
        rc = ccall(_jl_zmq_msg_init, Int, (Ptr{Void},), obj)
        if rc != 0
            throw(ZMQStateError(jl_zmq_error_str()))
        end
        zmsg = new(obj)
        finalizer(zmsg, close)
        return zmsg
    end
    # Create a message with a given buffer size (for send)
    function ZMQMessage(len::Integer)
        obj = Array(Uint8, _jl_zmq_msg_t_size)
        rc = ccall(_jl_zmq_msg_init_size, Int, (Ptr{Void}, Uint), obj, uint(len))
        if rc != 0
            throw(ZMQStateError(jl_zmq_error_str()))
        end
        zmsg = new(obj)
        finalizer(zmsg, close)
        return zmsg
    end
end
# Construct a message from a string (including copying the string)
function ZMQMessage(data::ByteArray)
    len = length(data)
    zmsg = ZMQMessage(len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Uint8}, Ptr{Uint8}, Uint),
          msg_data(zmsg), data, len)
    return zmsg
end
# Construct a message from a Array{Uint8} (including copying the data)
# In many cases it's more efficient to allocate the zmsg first and
# then build the data in-place, but this is here for convenience
function ZMQMessage(data::Array{Uint8, 1})
    len = length(data)
    zmsg = ZMQMessage(len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Uint8}, Ptr{Uint8}, Uint),
          msg_data(zmsg), data, len)
    return zmsg
end
function ref(::Type{Uint8}, zmsg::ZMQMessage)
    len = msg_size(zmsg)
    data = Array(Uint8, len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
          data, msg_data(zmsg), len)
    return data
end
# Convert message to string with ASCIIString[zmsg]
ref(::Type{ASCIIString}, zmsg::ZMQMessage) = cstring(msg_data(zmsg), msg_size(zmsg))
# Close a message. You should not need to call this manually (let the
# finalizer do it).
function close(zmsg::ZMQMessage)
    rc = ccall(_jl_zmq_msg_close, Int, (Ptr{Uint8},), zmsg.obj)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
# Low-level functions
# Extract a pointer to the ByteArray data in a message
msg_data(zmsg::ZMQMessage) = ccall(_jl_zmq_msg_data, Ptr{Uint8}, (Ptr{Uint8},), zmsg.obj)
# Determine the number of bytes in a message
msg_size(zmsg::ZMQMessage) = ccall(_jl_zmq_msg_size, Int, (Ptr{Uint8},) , zmsg.obj)

## Send/receive messages
#
# Julia defines two types of ZMQ messages: "raw" and "serialized". A "raw"
# message is just a plain ZeroMQ message, used for sending a sequence
# of bytes. You send these with the following:
#   send(socket, zmsg)
#   zmsg = recv(socket)
send(socket::ZMQSocket, zmsg::ZMQMessage) = send(socket, zmsg, 0)
function send(socket::ZMQSocket, zmsg::ZMQMessage, noblock::Bool, sndmore::Bool)

    flag::Int = 0;
    if (noblock) flag = flag | ZMQ_NOBLOCK ; end
    if (sndmore) flag = flag | ZMQ_SNDMORE ; end
    send(socket, zmsg, flag)
end
function send(socket::ZMQSocket, zmsg::ZMQMessage, flag::Int)
    rc = ccall(_jl_zmq_send, Int, (Ptr{Void}, Ptr{Uint8}, Int),
               socket.data, zmsg.obj, flag)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
recv(socket::ZMQSocket) = recv(socket, 0)
function recv(socket::ZMQSocket, noblock::Bool)
    flag::Int = 0;
    if (noblock) flag = flag | ZMQ_NOBLOCK ; end
    recv(socket, flag)
end
function recv(socket::ZMQSocket, flag::Int)
    zmsg = ZMQMessage()
    rc = ccall(_jl_zmq_recv, Int, (Ptr{Void}, Ptr{Void}, Int32),
               socket.data, zmsg.obj, flag)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
    return zmsg
end


# A "serialized" message includes information needed to interpret the
# data. For example, sending an array requires information about the
# element type and dimensions. See zmq_serialize.jl.



## Constants

#Socket Types
const ZMQ_PAIR = 0
const ZMQ_PUB = 1
const ZMQ_SUB = 2
const ZMQ_REQ = 3
const ZMQ_REP = 4
const ZMQ_DEALER = 5
const ZMQ_ROUTER = 6
const ZMQ_PULL = 7
const ZMQ_PUSH = 8
const ZMQ_XPUB = 9
const ZMQ_XSUB = 10
const ZMQ_XREQ = ZMQ_DEALER        
const ZMQ_XREP = ZMQ_ROUTER        
const ZMQ_UPSTREAM = ZMQ_PULL      
const ZMQ_DOWNSTREAM = ZMQ_PUSH    

#Send/Recv Options
const ZMQ_NOBLOCK = 1
const ZMQ_SNDMORE = 2

#IO Multiplexing
const ZMQ_POLLIN = 1
const ZMQ_POLLOUT = 2
const ZMQ_POLLERR = 4

#Built in devices
const ZMQ_STREAMER = 1
const ZMQ_FORWARDER = 2
const ZMQ_QUEUE = 3
