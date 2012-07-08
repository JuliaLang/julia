# A ZMQ serializer with a public protocol

require("zmq.jl")

# A serialized message includes information needed to interpret the
# data. For example, sending an array requires information about the
# data type and dimensions. Serialized messages are sent as ZeroMQ
# multi-part messages. The first part is a string indicating which
# deserializer is to be used. For example, "Array" indicates an array
# transmitted using a protocol defined below. However, you can also
# define external protocols, e.g., "ArrayNumpyJSON", etc.
# For example, to define an alternate Array protocol:
#   type MyArrayProtocol; end   # a dummy type for dispatch
#   _zmq_serialize_dict[Array] = "MyArrayProtocol"
#   _zmq_deserialize_dict["MyArrayProtocol"] = MyArrayProtocol
# Then write the following functions:
#   zmq_serialize(socket::ZMQSocket, x::Array)  # overrides the default
#   zmq_deserialize(socket::ZMQSocket, ::Type{MyArrayProtocol})
# using whatever encoding scheme you wish.


## Serializer protocol versioning

# Version-checking could be replaced by several alternatives: one
# interesting choice would be to have the client send the complete
# serializer/deserializer code as an ASCII message, to be parsed by
# the server's Julia parser and then eval-ed. This would guarantee
# that the two are using the same version, at the cost of increasing
# the startup time.
const _zmq_serialize_version = uint16(0)
const _zmq_serialize_versionchars = b"PROTOCOL VERSION"
let
const reqstr = [_zmq_serialize_versionchars, uint8('?'), uint8('?')]
const verstr = [_zmq_serialize_versionchars, uint8(_zmq_serialize_version & 0xff), uint8((_zmq_serialize_version >> 8) & 0xff)]
global zmq_serialize_request_version
global zmq_serialize_send_version
zmq_serialize_request_version(socket::ZMQSocket) = send(socket, ZMQMessage(reqstr))
zmq_serialize_send_version(socket::ZMQSocket) = send(socket, ZMQMessage(verstr))
end
function zmq_serialize_get_version(socket::ZMQSocket)
    versiondata = Uint8[recv(socket)]
    for i = 1:length(_zmq_serialize_versionchars)
        @assert versiondata[i] == _zmq_serialize_versionchars[i]
    end
    return uint16(versiondata[end-1] | (uint16(versiondata[end]) << 8))
end


## Serializer dictionaries

type ToParse; end  # dummy type for strings to be parsed by Julia parser

_zmq_serialize_dict = Dict{Type, String}()
_zmq_deserialize_dict = Dict{String, Type}()
for (obj, tag) in {
    (Char,              "c"                   )
    (Int8,              "b"                   )
    (Uint8,             "B"                   )
    (Bool,              "?"                   )
    (Int16,             "h"                   )
    (Uint16,            "H"                   )
    (Int32,             "i"                   )
    (Uint32,            "I"                   )
    (Int64,             "q"                   )
    (Uint64,            "Q"                   )
    (Float32,           "f"                   )
    (Float64,           "d"                   )
    (ASCIIString,       "s"                   )
    (Any,               "Any"                 )
    (Array,             "Array"               )
    (Symbol,            "Symbol"              )
    (Expr,              "Expr"                )
    (ToParse,           "ToParse"             )
    (Exception,         "Exception"           )
    }
    _zmq_serialize_dict[obj] = tag
    _zmq_deserialize_dict[tag] = obj
end

## Serializer methods

function zmq_assert_more(socket::ZMQSocket, tag::ASCIIString)
    if !ismore(socket)
        error("Incomplete serialized type with tag ", tag)
    end
end

# Generic deserializer
# Deserialize one object
function zmq_deserialize1(socket::ZMQSocket)
    tag = ASCIIString[recv(socket)]
    typ = _zmq_deserialize_dict[tag]
    zmq_assert_more(socket, tag)
    zmq_deserialize(socket, typ)  # hand work off to specific type
end
# Deserialize complete multi-part message
function zmq_deserialize(socket::ZMQSocket)
    ret = zmq_deserialize1(socket)
    ismulti = false
    if ismore(socket)
        ismulti = true
        ret = {ret}
        while ismore(socket)
            tmp = zmq_deserialize1(socket)
            push(ret, tmp)
        end
    end
    return ret, ismulti
end


# Specific types
# Serializer for BitsTypes 
function zmq_serialize(socket::ZMQSocket, x, flag::Integer)
    zmsg = ZMQMessage(_zmq_serialize_dict[typeof(x)])
    send(socket, zmsg, ZMQ_SNDMORE | flag)
    b = tobytes(x)
    len = nbytes(x)
    zmsg = ZMQMessage(len)
    a = pointer_to_array(msg_data(zmsg), (len,))
    for i = 1:len
        a[i] = nthbyte(b, i)
    end
    send(socket, zmsg, flag)
end
# It's better not to provide a convenience function omitting the
# flag, to avoid the forgetting-to-pass-the-flag-through bug

function zmq_deserialize{T <: Integer}(socket::ZMQSocket, ::Type{T})
    x = zero(T)
    len = sizeof(x)
    zmsg = recv(socket)
    if msg_size(zmsg) != len
        error("Deserializing ", T, " did not have enough bytes")
    end
    a = pointer_to_array(msg_data(zmsg), (len,))
    for i = 1:len
        x |= (convert(T,a[i])<<((i-1)<<3))
    end
    return x
end
# Char? This is a little tricky, skip for now
function zmq_deserialize(socket::ZMQSocket, ::Type{Float32})
    b = zmq_deserialize(socket, Int32)
    x = frombytes(b, Float32)
end
function zmq_deserialize(socket::ZMQSocket, ::Type{Float64})
    b = zmq_deserialize(socket, Int64)
    x = frombytes(b, Float64)
end
zmq_deserialize(socket::ZMQSocket, ::Type{ASCIIString}) = ASCIIString[recv(socket)]

# Array
function zmq_serialize_array_header{T}(socket::ZMQSocket, x::Array{T}, flag)
    thisflag = flag | ZMQ_SNDMORE
    # Send the element type
    zmsg = ZMQMessage(_zmq_serialize_dict[eltype(x)])
    send(socket, zmsg, thisflag)
    # Send the dimensions, as a sequence of Int64. You extract the
    # dimensionality from the length of the message.
    nd = ndims(x)
    len = sizeof(Int64)*nd
    zmsg = ZMQMessage(len)
    za = pointer_to_array(msg_data(zmsg), (len,))
    istore = 1
    for i = 1:nd
        sz::Int64 = size(x, i)
        for j = 1:sizeof(Int64)
            za[istore] = nthbyte(sz, j)
            istore += 1
        end
    end
    send(socket, zmsg, thisflag)
end
function zmq_serialize{T}(socket::ZMQSocket, x::Array{T}, flag::Integer)
    flagmore = flag | ZMQ_SNDMORE
    # Send the tag
    zmsg = ZMQMessage(_zmq_serialize_dict[Array])
    send(socket, zmsg, flagmore)
    # Send the element type and dimensions
    zmq_serialize_array_header(socket, x, flag)
    if isa(T, BitsKind)
        len = sizeof(T) * numel(x)
        zmsg = ZMQMessage(len)
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), 
              msg_data(zmsg), x, len)
        send(socket, zmsg, flag)
    else
        for i = 1:numel(x)
            thisflag = (i < numel(x) ? flagmore : flag)
            zmq_serialize(socket, x[i], thisflag)
        end
    end
end
function zmq_deserialize_array_header(socket::ZMQSocket)
    # Get the element type
    typetag = ASCIIString[recv(socket)]
    T = _zmq_deserialize_dict[typetag]
    # Get the dimensions
    zmq_assert_more(socket, "Array header")
    zmsg = recv(socket)
    len = msg_size(zmsg)
    za = pointer_to_array(msg_data(zmsg), (len,))
    nd = div(len, sizeof(Int64))
    if nd*sizeof(Int64) != len
        error("Array dimensions are not encoded as Int64")
    end
    dims = ntuple(nd, i -> _zmq_nth_int64(za, i))
    return T, dims
end
function _zmq_nth_int64(a::Array{Uint8, 1}, n::Integer)
    x = zero(Int64)
    offset = (n-1)*sizeof(Int64)
    for i = 1:sizeof(Int64)
        x |= (convert(Int64,a[i+offset])<<((i-1)<<3))
    end
    return x
end
function zmq_deserialize(socket::ZMQSocket, ::Type{Array})    
    T, dims = zmq_deserialize_array_header(socket)
    x = Array(T, dims...)
    if isa(T, BitsKind)
        zmq_assert_more(socket, "Array data")
        zmsg = recv(socket)
        len = msg_size(zmsg)
        if len != sizeof(T) * prod(dims)
            error("Array data is of wrong size")
        end
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), 
              x, msg_data(zmsg), len)
        return x
    else
        for i = 1:numel(x)
            zmq_assert_more(socket, "Array element")
            x[i] = zmq_deserialize1(socket)
        end
    end
end


# Symbol
function zmq_serialize(socket::ZMQSocket, s::Symbol, flag::Integer)
    # Send the tag
    zmsg = ZMQMessage(_zmq_serialize_dict[typeof(s)])
    send(socket, zmsg, flag | ZMQ_SNDMORE)
    # Send the symbol text
    zmsg = ZMQMessage(string(s))
    send(socket, zmsg, flag)
end
function zmq_deserialize(socket::ZMQSocket, ::Type{Symbol})
    # Get the symbol
    zmq_assert_more(socket, "Symbol")
    zmsg = recv(socket)
    return symbol(ASCIIString[zmsg])
end


# Expr
function zmq_serialize(socket::ZMQSocket, ex::Expr, flag::Integer)
    flagmore = flag | ZMQ_SNDMORE
    # Send the tag
    zmsg = ZMQMessage(_zmq_serialize_dict[typeof(ex)])
    send(socket, zmsg, flagmore)
    # Send the head
    zmsg = ZMQMessage(string(ex.head))
    send(socket, zmsg, flagmore)
    # Send the # of arguments as an Int64
    nargs::Int64 = length(ex.args)
    len = sizeof(Int64)
    zmsg = ZMQMessage(len)
    a = pointer_to_array(msg_data(zmsg), (len,))
    for i = 1:len
        a[i] = nthbyte(nargs, i)
    end
    thisflag = (nargs > 0 ? flagmore : flag)
    send(socket, zmsg, thisflag)
    # Send each argument as a fully-serialized value
    for i = 1:nargs
        thisflag = (i == nargs ? flag : flagmore)
        zmq_serialize(socket, ex.args[i], thisflag)
    end
end
function zmq_deserialize(socket::ZMQSocket, ::Type{Expr})
    # Get the head
    zmq_assert_more(socket, "Expr head")
    zmsg = recv(socket)
    head = symbol(ASCIIString[zmsg])
    # Get the # of arguments
    zmq_assert_more(socket, "Expr nargs")
    zmsg = recv(socket)
    len = msg_size(zmsg)
    if len != sizeof(Int64)
        error("Expr: number of arguments is incorrectly encoded")
    end
    a = pointer_to_array(msg_data(zmsg), (len,))
    n = _zmq_nth_int64(a, 1)
    # Get the arguments
    args = cell(n)
    for i = 1:n
        zmq_assert_more(socket, "Expr arg")
        args[i] = zmq_deserialize1(socket)
    end
    return expr(head, args)
end

# ToParse
#
# This supports running of more complex Julia programs from other
# languages.  Within Julia it's much better to send a quote block, so
# no "serialize" variant has been written (but see zmqparse in zmq_client)
function zmq_deserialize(socket::ZMQSocket, ::Type{ToParse})
    # Get the string and parse it
    zmq_assert_more(socket, "ToParse")
    zmsg = recv(socket)
    str = ASCIIString[zmsg]
    ex, len = parse(str)
    if len < length(str)
        error("Could not parse entire string")
    end
    return ex
end


# Exception
# Here all exceptions are given a standard form: three raw
# (unserialized) strings,
#    exception type
#    exception message
#    server message  (optional)
# Upon deserialization, these get packed into a custom exception type.
#
# Note: if an exception is sent, it should terminate the multipart
# message. The "flag" input for the serializer allows the server
# to attach an additional message, for example indicating in what
# stage the exception occurred.
# Consider: send the full backtrace? How to get it?
type ZMQRemoteError <: Exception
    exname::String
    exmsg::String
    servermsg::String
end
function show(io, ex::ZMQRemoteError)
    if !isempty(ex.servermsg)
        println(io, ex.servermsg)
    end
    println(io, ex.exname)
    println(io, ex.exmsg)
end
function zmq_serialize(socket::ZMQSocket, ex::Exception, flag::Integer)
    flagmore = flag | ZMQ_SNDMORE
    # Send the tag
    zmsg = ZMQMessage(_zmq_serialize_dict[Exception])
    send(socket, zmsg, flagmore)
    # Send the exception type text
    zmsg = ZMQMessage(string(typeof(ex)))
    send(socket, zmsg, flagmore)
    # Send the exception message
    s = string(ex)
    zmsg = ZMQMessage(s)
    send(socket, zmsg, flag)
end
function zmq_deserialize(socket::ZMQSocket, ::Type{Exception})
    # Get the exception type
    zmq_assert_more(socket, "Exception type")
    zmsg = recv(socket)
    exname = ASCIIString[zmsg]
    # Get the exception message
    zmq_assert_more(socket, "Exception message")
    zmsg = recv(socket)
    exmsg = ASCIIString[zmsg]
    if ismore(socket)
        # Get the server message
        zmsg = recv(socket)
        servermsg = ASCIIString[zmsg]
    else
        servermsg = ""
    end
    # If there is any more, trash it (to reduce the risk that the ZMQ
    # state gets corrupted)
    iter = 1
    while ismore(socket) && iter < 1000
        recv(socket)
        iter += 1
    end
    throw(ZMQRemoteError(exname, exmsg, servermsg))
end
