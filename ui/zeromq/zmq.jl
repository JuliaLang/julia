# Support for ZeroMQ, a network and interprocess communication library

_jl_libzmq = dlopen("libzmq")

_jl_zmq_version = dlsym(_jl_libzmq, :zmq_version)
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
    errno = ccall(_jl_zmq_errno, Int32, ())
    c_strerror = ccall (_jl_zmq_strerror, Ptr{Uint8}, (Int32,), errno)
    if c_strerror != C_NULL
        strerror = bytestring(c_strerror)
        return strerror
    else 
        return "Unknown error"
    end
end

let major = zeros(Int32, 1), minor = zeros(Int32, 1), patch = zeros(Int32, 1)
global zmq_version
function zmq_version()
    ccall(_jl_zmq_version, Void, (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}), major, minor, patch)
    return (major[1], minor[1], patch[1])
end
end

_zmq_major, _zmq_minor, _zmq_path = zmq_version()

# Version-dependent library symbols
if _zmq_major < 3
    # Version 2 context functions (now deprecated)
    _jl_zmq_init = dlsym(_jl_libzmq, :zmq_init)
    _jl_zmq_term = dlsym(_jl_libzmq, :zmq_term)
else
    # Version 3 context functions
    _jl_zmq_ctx_new     = dlsym(_jl_libzmq, :zmq_ctx_new)
    _jl_zmq_ctx_destroy = dlsym(_jl_libzmq, :zmq_ctx_destroy)
    _jl_zmq_ctx_get = dlsym(_jl_libzmq, :zmq_ctx_get)
    _jl_zmq_ctx_set = dlsym(_jl_libzmq, :zmq_ctx_set)
    # Version 3 socket functions
    _jl_zmq_disconnect = dlsym(_jl_libzmq, :zmq_disconnect)
    _jl_zmq_unbind = dlsym(_jl_libzmq, :zmq_unbind)
    # Version 3 message functions
    _jl_zmq_msg_get = dlsym(_jl_libzmq, :zmq_msg_get)
    _jl_zmq_msg_set = dlsym(_jl_libzmq, :zmq_msg_set)
    _jl_zmq_msg_more = dlsym(_jl_libzmq, :zmq_msg_more)
end

## Contexts ##
# Provide the same constructor API for version 2 and version 3, even
# though the underlying functions are changing
if _zmq_major < 3
global ZMQContext
type ZMQContext
    data::Ptr{Void}

    function ZMQContext(n::Integer)
        p = ccall(_jl_zmq_init, Ptr{Void},  (Int32,), n)
        if p == C_NULL
	    throw(ZMQStateError(jl_zmq_error_str()))
        end
        zctx = new(p)
        finalizer(zctx, close)
        return zctx
    end
end
else  # Versions 3 and higher
global ZMQContext
type ZMQContext
    data::Ptr{Void}

    function ZMQContext(n::Integer)
        p = ccall(_jl_zmq_ctx_new, Ptr{Void},  ())
        if p == C_NULL
	    throw(ZMQStateError(jl_zmq_error_str()))
        end
        zctx = new(p)
        finalizer(zctx, close)
        set(zctx, ZMQ_IO_THREADS, n)
        return zctx
    end
end
end
ZMQContext() = ZMQContext(1)
if _zmq_major < 3
global close
function close(ctx::ZMQContext)
    rc = ccall(_jl_zmq_term, Int32,  (Ptr{Void},), ctx.data)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
else  # Versions 3 and higher
global close
function close(ctx::ZMQContext)
    rc = ccall(_jl_zmq_ctx_destroy, Int32,  (Ptr{Void},), ctx.data)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
global get
function get(ctx::ZMQContext, option::Integer)
    val = ccall(_jl_zmq_ctx_get, Int32, (Ptr{Void}, Int32), ctx.data, option)
    if val < 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
    return val
end
global set
function set(ctx::ZMQContext, option::Integer, value::Integer)
    rc = ccall(_jl_zmq_ctx_set, Int32, (Ptr{Void}, Int32, Int32), ctx.data, option, value)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
end


## Sockets ##
type ZMQSocket
    data::Ptr{Void}

    function ZMQSocket(ctx::ZMQContext, typ::Integer)
        p = ccall(_jl_zmq_socket, Ptr{Void},  (Ptr{Void}, Int32), ctx.data, typ)
        if p == C_NULL
	    throw(ZMQStateError(jl_zmq_error_str()))
        end
        socket = new(p)
        finalizer(socket, close)
        return socket
    end
end
function close(socket::ZMQSocket)
    rc = ccall(_jl_zmq_close, Int32,  (Ptr{Void},), socket.data)
    if rc != 0
	throw(ZMQStateError(jl_zmq_error_str()))
    end
end


# Getting and setting socket options
# Socket options of integer type
let u64p = zeros(Uint64, 1), i64p = zeros(Int64, 1), ip = zeros(Int32, 1), u32p = zeros(Uint32, 1), sz = zeros(Uint, 1)
opslist = {
    (:set_affinity,                :get_affinity,                 4, u64p)
    (nothing,                      :get_fd,                      14,   ip)
    (:set_type,                    :get_type,                    16,   ip)
    (:set_linger,                  :get_linger,                  17,   ip)
    (:set_reconnect_ivl,           :get_reconnect_ivl,           18,   ip)
    (:set_backlog,                 :get_backlog,                 19,   ip)
    (:set_reconnect_ivl_max,       :get_reconnect_ivl_max,       21,   ip)
  }
major, minor, patch = zmq_version()
if major == 2
    opslist = vcat(opslist, {
    (:set_hwm,                     :get_hwm,                      1, u64p)
    (:set_swap,                    :get_swap,                     3, i64p)
    (:set_rate,                    :get_rate,                     8, i64p)
    (:set_recovery_ivl,            :get_recovery_ivl,             9, i64p)
    (:_zmq_setsockopt_mcast_loop,  :_zmq_getsockopt_mcast_loop,  10, i64p)
    (:set_sndbuf,                  :get_sndbuf,                  11, u64p)
    (:set_rcvbuf,                  :get_rcvbuf,                  12, u64p)
    (nothing,                      :_zmq_getsockopt_rcvmore,     13, i64p)
    (nothing,                      :get_events,                  15, u32p)
    (:set_recovery_ivl_msec,       :get_recovery_ivl_msec,       20, i64p)
    })
elseif major == 3
    opslist = vcat(opslist, {
    (:set_rate,                    :get_rate,                     8,   ip)
    (:set_recovery_ivl,            :get_recovery_ivl,             9,   ip)
    (:set_sndbuf,                  :get_sndbuf,                  11,   ip)
    (:set_rcvbuf,                  :get_rcvbuf,                  12,   ip)
    (nothing,                      :_zmq_getsockopt_rcvmore,     13,   ip)
    (nothing,                      :get_events,                  15,   ip)
    (:set_maxmsgsize,              :get_maxmsgsize,              22,   ip)
    (:set_sndhwm,                  :get_sndhwm,                  23,   ip)
    (:set_rcvhwm,                  :get_rcvhwm,                  24,   ip)
    (:set_multicast_hops,          :get_multicast_hops,          25,   ip)
    (:set_ipv4only,                :get_ipv4only,                31,   ip)
    (:set_tcp_keepalive,           :get_tcp_keepalive,           34,   ip)
    (:set_tcp_keepalive_idle,      :get_tcp_keepalive_idle,      35,   ip)
    (:set_tcp_keepalive_cnt,       :get_tcp_keepalive_cnt,       36,   ip)
    (:set_tcp_keepalive_intvl,     :get_tcp_keepalive_intvl,     37,   ip)
    })
end
if major > 2 || (major == 2 && minor > 1)
    opslist = vcat(opslist, {
    (:set_rcvtimeo,                :get_rcvtimeo,                27,   ip)
    (:set_sndtimeo,                :get_sndtimeo,                28,   ip)
    })
end
    
for (fset, fget, k, p) in opslist
    if fset != nothing
        @eval global ($fset)
        @eval function ($fset)(socket::ZMQSocket, option_val::Integer)
            ($p)[1] = option_val
            rc = ccall(_jl_zmq_setsockopt, Int32,
                       (Ptr{Void}, Int32, Ptr{Void}, Uint),
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
            rc = ccall(_jl_zmq_getsockopt, Int32,
                       (Ptr{Void}, Int32, Ptr{Void}, Ptr{Uint}),
                       socket.data, $k, $p, $sz)
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
            return int(($p)[1])
        end
    end        
end
# For some functions, the publicly-visible versions should require &
# return boolean
if major == 2
    global set_mcast_loop
    set_mcast_loop(socket::ZMQSocket, val::Bool) = _zmq_setsockopt_mcast_loop(socket, val)
    global get_mcast_loop
    get_mcast_loop(socket::ZMQSocket) = bool(_zmq_getsockopt_mcast_loop(socket))
end
end  # let
# More functions with boolean prototypes
get_rcvmore(socket::ZMQSocket) = bool(_zmq_getsockopt_rcvmore(socket))
# And a convenience function
ismore(socket::ZMQSocket) = get_rcvmore(socket)


# Socket options of string type
let u8ap = zeros(Uint8, 255), sz = zeros(Uint, 1)
major, minor, patch = zmq_version()
opslist = {
    (:set_identity,                :get_identity,                5)
    (:set_subscribe,               nothing,                      6)
    (:set_unsubscribe,             nothing,                      7)
    }
if major == 3
    opslist = vcat(opslist, {
    (nothing,                      :get_last_endpoint,          32)
    (:set_tcp_accept_filter,       nothing,                     38)
    })
end
for (fset, fget, k) in opslist
    if fset != nothing
        @eval global ($fset)
        @eval function ($fset)(socket::ZMQSocket, option_val::ByteString)
            if length(option_val) > 255
                throw(ZMQStateError("option value too large"))
            end
            rc = ccall(_jl_zmq_setsockopt, Int32,
                       (Ptr{Void}, Int32, Ptr{Uint8}, Uint),
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
            rc = ccall(_jl_zmq_getsockopt, Int32,
                       (Ptr{Void}, Int32, Ptr{Uint8}, Ptr{Uint}),
                       socket.data, $k, $u8ap, $sz)
            if rc != 0
                throw(ZMQStateError(jl_zmq_error_str()))
            end
            return bytestring(convert(Ptr{Uint8}, $u8ap), int(($sz)[1]))
        end
    end        
end
end  # let
    


function bind(socket::ZMQSocket, endpoint::String)
    rc = ccall(_jl_zmq_bind, Int32, (Ptr{Void}, Ptr{Uint8}), socket.data, endpoint)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end

function connect(socket::ZMQSocket, endpoint::String)
    rc=ccall(_jl_zmq_connect, Int32, (Ptr{Void}, Ptr{Uint8}), socket.data, endpoint)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end


## Messages ##
typealias ByteArray Union(Array{Uint8,1}, ByteString)
type ZMQMessage
    obj::Vector{Uint8}

    # Create an empty message (for receive)
    function ZMQMessage()
        obj = Array(Uint8, _jl_zmq_msg_t_size)
        rc = ccall(_jl_zmq_msg_init, Int32, (Ptr{Void},), obj)
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
        rc = ccall(_jl_zmq_msg_init_size, Int32, (Ptr{Void}, Uint), obj, uint(len))
        if rc != 0
            throw(ZMQStateError(jl_zmq_error_str()))
        end
        zmsg = new(obj)
        finalizer(zmsg, close)
        return zmsg
    end
end
# Construct a message from a string (including copying the string)
# In many cases it's more efficient to allocate the zmsg first and
# then build the data in-place, but this is here for convenience
function ZMQMessage(data::ByteArray)
    len = length(data)
    zmsg = ZMQMessage(len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Uint8}, Ptr{Uint8}, Uint),
          msg_data(zmsg), data, len)
    return zmsg
end
# Construct a message from a Array{Uint8} (including copying the data)
function ZMQMessage(data::Array{Uint8, 1})
    len = length(data)
    zmsg = ZMQMessage(len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Uint8}, Ptr{Uint8}, Uint),
          msg_data(zmsg), data, len)
    return zmsg
end
# Convert message to array of Uint8 with Uint8[zmsg]
# Copies the data
function ref(::Type{Uint8}, zmsg::ZMQMessage)
    len = msg_size(zmsg)
    data = Array(Uint8, len)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
          data, msg_data(zmsg), len)
    return data
end
# Convert message to string with ASCIIString[zmsg]
# Copies the data
ref(::Type{ASCIIString}, zmsg::ZMQMessage) = bytestring(msg_data(zmsg), msg_size(zmsg))
# Build an IOStream from a message
# Copies the data
function convert(::Type{IOStream}, zmsg::ZMQMessage)
    len = msg_size(zmsg)
    a = pointer_to_array(msg_data(zmsg), (len,))
    s = memio()
    write(s, a)
    return s
end
# Close a message. You should not need to call this manually (let the
# finalizer do it).
function close(zmsg::ZMQMessage)
    rc = ccall(_jl_zmq_msg_close, Int32, (Ptr{Uint8},), zmsg.obj)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
# Low-level functions
# Extract a pointer to the ByteArray data in a message
msg_data(zmsg::ZMQMessage) = ccall(_jl_zmq_msg_data, Ptr{Uint8}, (Ptr{Uint8},), zmsg.obj)
# Determine the number of bytes in a message
msg_size(zmsg::ZMQMessage) = ccall(_jl_zmq_msg_size, Int, (Ptr{Uint8},) , zmsg.obj)
if _zmq_major > 2
global get
function get(zmsg::ZMQMessage, property::Integer)
    val = ccall(_jl_zmq_msg_get, Int32, (Ptr{Void}, Int32), zmsg.data, property)
    if val < 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
global set
function set(zmsg::ZMQMessage, property::Integer, value::Integer)
    rc = ccall(_jl_zmq_msg_set, Int32, (Ptr{Void}, Int32, Int32), zmsg.data, property, value)
    if rc < 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
end

## Send/receive messages
#
# Julia defines two types of ZMQ messages: "raw" and "serialized". A "raw"
# message is just a plain ZeroMQ message, used for sending a sequence
# of bytes. You send these with the following:
#   send(socket, zmsg)
#   zmsg = recv(socket)
send(socket::ZMQSocket, zmsg::ZMQMessage) = send(socket, zmsg, int32(0))
function send(socket::ZMQSocket, zmsg::ZMQMessage, noblock::Bool, sndmore::Bool)

    flag::Int32 = 0;
    if (noblock) flag = flag | ZMQ_NOBLOCK ; end
    if (sndmore) flag = flag | ZMQ_SNDMORE ; end
    send(socket, zmsg, flag)
end
function send(socket::ZMQSocket, zmsg::ZMQMessage, flag::Integer)
    rc = ccall(_jl_zmq_send, Int32, (Ptr{Void}, Ptr{Uint8}, Int32),
               socket.data, zmsg.obj, flag)
    if rc != 0
        throw(ZMQStateError(jl_zmq_error_str()))
    end
end
recv(socket::ZMQSocket) = recv(socket, int32(0))
function recv(socket::ZMQSocket, noblock::Bool)
    flag::Int32 = 0;
    if (noblock) flag = flag | ZMQ_NOBLOCK ; end
    recv(socket, flag)
end
function recv(socket::ZMQSocket, flag::Integer)
    zmsg = ZMQMessage()
    rc = ccall(_jl_zmq_recv, Int32, (Ptr{Void}, Ptr{Void}, Int32),
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

# Context options
const ZMQ_IO_THREADS = 1
const ZMQ_MAX_SOCKETS = 2

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

#Message options
const ZMQ_MORE = 1

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
