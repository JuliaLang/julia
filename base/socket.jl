## SOCKETS ##
include("ipnetwork.jl")

abstract Socket <: AsyncStream

type TCPSocket <: Socket
    handle::Ptr{Void}
    status::Int
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Condition
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition

    TCPSocket(handle) = new(
        handle,
        StatusUninit,
        true,
        PipeBuffer(),
        false, Condition(),
        false, Condition(),
        false, Condition()
    )
end
function TCPSocket()
    this = TCPSocket(c_malloc(_sizeof_uv_tcp))
    associate_julia_struct(this.handle,this)
    finalizer(this,uvfinalize)
    err = ccall(:uv_tcp_init,Cint,(Ptr{Void},Ptr{Void}),
                  eventloop(),this.handle)
    if err != 0
        c_free(this.handle)
        this.handle = C_NULL
        error(UVError("failed to create tcp socket",err))
    end
    this.status = StatusInit
    this
end

type TCPServer <: UVServer
    handle::Ptr{Void}
    status::Int
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition

    TCPServer(handle) = new(
        handle,
        StatusUninit,
        false, Condition(),
        false, Condition()
    )
end
function TCPServer()
    this = TCPServer(c_malloc(_sizeof_uv_tcp))
    associate_julia_struct(this.handle, this)
    finalizer(this,uvfinalize)
    err = ccall(:uv_tcp_init,Cint,(Ptr{Void},Ptr{Void}),
                  eventloop(),this.handle)
    if err != 0
        c_free(this.handle)
        this.handle = C_NULL
        error(UVError("failed to create tcp server",err))
    end
    this.status = StatusInit
    this
end

function uvfinalize(uv)
    close(uv)
    disassociate_julia_struct(uv)
    uv.handle = 0
end

isreadable(io::TCPSocket) = true
iswritable(io::TCPSocket) = true

show(io::IO,sock::TCPSocket) = print(io,"TCPSocket(",uv_status_string(sock),", ",
    nb_available(sock.buffer)," bytes waiting)")

show(io::IO,sock::TCPServer) = print(io,"TCPServer(",uv_status_string(sock),")")

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_connect_raw(sock::TCPSocket,sockaddr::Ptr{Void}) =
    ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void}),sock.handle,sockaddr)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) =
    ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::UInt16) =
    ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},UInt16),ptr,port)

accept(server::TCPServer) = accept(server, TCPSocket())
accept(server::PipeServer) = accept(server, Pipe())

##

bind(sock::TCPServer, addr::InetAddr) = bind(sock,addr.host,addr.port)

_bind(sock::TCPServer, host::IPv4, port::UInt16) = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, UInt16, UInt32, Cuint),
            sock.handle, hton(port), hton(host.host), 0)

_bind(sock::TCPServer, host::IPv6, port::UInt16) = ccall(:jl_tcp_bind6, Int32, (Ptr{Void}, UInt16, Ptr{UInt128}, Cuint),
            sock.handle, hton(port), &hton(host.host), 0)

# UDP

type UDPSocket <: Socket
    handle::Ptr{Void}
    status::Int
    recvnotify::Condition
    sendnotify::Condition
    closenotify::Condition

    UDPSocket(handle::Ptr) = new(
        handle,
        StatusUninit,
        Condition(),
        Condition(),
        Condition()
    )
end
function UDPSocket()
    this = UDPSocket(c_malloc(_sizeof_uv_udp))
    associate_julia_struct(this.handle, this)
    err = ccall(:uv_udp_init,Cint,(Ptr{Void},Ptr{Void}),
                  eventloop(),this.handle)
    finalizer(this, uvfinalize)
    if err != 0
        c_free(this.handle)
        this.handle = C_NULL
        error(UVError("failed to create udp socket",err))
    end
    this.status = StatusInit
    this
end

function uvfinalize(uv::Union(TTY,Pipe,PipeServer,TCPServer,TCPSocket,UDPSocket))
    if (uv.status != StatusUninit && uv.status != StatusInit)
        close(uv)
    end
    disassociate_julia_struct(uv)
    uv.handle = 0
end

function _uv_hook_close(sock::UDPSocket)
    sock.handle = 0
    sock.status = StatusClosed
    notify(sock.closenotify)
    notify(sock.sendnotify)
    notify_error(sock.recvnotify,EOFError())
end

# Disables dual stack mode. Only available when using ipv6 binf
const UV_UDP_IPV6ONLY = 1

# Indicates message was truncated because read buffer was too small. The
# remainder was discarded by the OS.
const UV_UDP_PARTIAL = 2

function bind(sock::Union(TCPServer,UDPSocket), host::IPv4, port::Integer)
    @assert sock.status == StatusInit
    err = _bind(sock,host,uint16(port))
    if err < 0
        if err != UV_EADDRINUSE && err != UV_EACCES
            error(UVError("bind",err))
        else
            return false
        end
    end
    sock.status = StatusOpen
    true
end

_bind(sock::UDPSocket, host::IPv4, port::UInt16) = ccall(:jl_udp_bind, Int32, (Ptr{Void}, UInt16, UInt32, UInt32),
            sock.handle, hton(port), hton(host.host), 0)

_bind(sock::UDPSocket, host::IPv6, port::UInt16, flags::UInt32 = uint32(0)) = ccall(:jl_udp_bind6, Int32, (Ptr{Void}, UInt16, Ptr{UInt128}, UInt32),
            sock.handle, hton(port), &hton(host.host), flags)

function bind(sock::UDPSocket, host::IPv6, port::UInt16; ipv6only = false)
    @assert sock.status == StatusInit
    err = _bind(sock,host,ipv6only ? UV_UDP_IPV6ONLY : 0)
    if err < 0
        if err != UV_EADDRINUSE && err != UV_EACCES
            error(UVError("bind",err))
        else
            return false
        end
    end
    sock.status = StatusOpen
    true
end


function setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)
    if sock.status == StatusUninit
        error("Cannot set options on unitialized socket")
    end
    if multicast_loop !== nothing
        uv_error("multicast_loop",ccall(:uv_udp_set_multicast_loop,Cint,(Ptr{Void},Cint),sock.handle,multicast_loop) < 0)
    end
    if multicast_ttl !== nothing
        uv_error("multicast_ttl",ccall(:uv_udp_set_multicast_ttl,Cint,(Ptr{Void},Cint),sock.handle,multicast_ttl))
    end
    if enable_broadcast !== nothing
        uv_error("enable_broadcast",ccall(:uv_udp_set_broadcast,Cint,(Ptr{Void},Cint),sock.handle,enable_broadcast))
    end
    if ttl !== nothing
        uv_error("ttl",ccall(:uv_udp_set_ttl,Cint,(Ptr{Void},Cint),sock.handle,ttl))
    end
end

_uv_hook_alloc_buf(sock::UDPSocket,size::UInt) = (c_malloc(size),size)

function _recv_start(sock::UDPSocket)
    if ccall(:uv_is_active,Cint,(Ptr{Void},),sock.handle) == 0
        uv_error("recv_start",ccall(:uv_udp_recv_start,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),
                                        sock.handle,cglobal(:jl_uv_alloc_buf),cglobal(:jl_uv_recvcb)))
    end
end

_recv_stop(sock::UDPSocket) = uv_error("recv_stop",ccall(:uv_udp_recv_stop,Cint,(Ptr{Void},),sock.handle))

function recv(sock::UDPSocket)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen
        error("Invalid socket state")
    end
    _recv_start(sock)
    stream_wait(sock,sock.recvnotify)::Vector{UInt8}
end

function _uv_hook_recv(sock::UDPSocket, nread::Int, buf_addr::Ptr{Void}, buf_size::UInt, addr::Ptr{Void}, flags::Int32)
    if flags & UV_UDP_PARTIAL > 0
        # TODO: Decide what to do in this case. For now throw an error
        c_free(buf_addr)
        notify_error(sock.recvnotify,"Partial message received")
    end
    buf = pointer_to_array(convert(Ptr{UInt8},buf_addr),int(buf_size),true)
    notify(sock.recvnotify,buf[1:nread])
end

function _send(sock::UDPSocket,ipaddr::IPv4,port::UInt16,buf)
    ccall(:jl_udp_send,Cint,(Ptr{Void},UInt16,UInt32,Ptr{UInt8},Csize_t),sock.handle,hton(port),hton(ipaddr.host),buf,sizeof(buf))
end

function _send(sock::UDPSocket,ipaddr::IPv6,port::UInt16,buf)
    ccall(:jl_udp_send6,Cint,(Ptr{Void},UInt16,Ptr{UInt128},Ptr{UInt8},Csize_t),sock.handle,hton(port),&hton(ipaddr.host),buf,sizeof(buf))
end

function send(sock::UDPSocket,ipaddr,port,msg)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen
        error("Invalid socket state")
    end
    uv_error("send",_send(sock,ipaddr,uint16(port),msg))
    stream_wait(sock,sock.sendnotify)
    nothing
end

function _uv_hook_send(sock::UDPSocket,status::Cint)
    if status < 0
        notify_error(sock.sendnotify,UVError("UDP send failed",status))
    end
    notify(sock.sendnotify)
end

##

callback_dict = ObjectIdDict()

function _uv_hook_getaddrinfo(cb::Function, addrinfo::Ptr{Void}, status::Int32)
    pop!(callback_dict,cb) # using pop forces an error if cb not in callback_dict
    if status != 0 || addrinfo == C_NULL
        cb(UVError("getaddrinfo callback",status))
        return
    end
    freeaddrinfo = addrinfo
    while addrinfo != C_NULL
        sockaddr = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
        if ccall(:jl_sockaddr_is_ip4,Int32,(Ptr{Void},),sockaddr) == 1
            cb(IPv4(ntoh(ccall(:jl_sockaddr_host4,UInt32,(Ptr{Void},),sockaddr))))
            break
        #elseif ccall(:jl_sockaddr_is_ip6,Int32,(Ptr{Void},),sockaddr) == 1
        #    host = Array(UInt128,1)
        #    scope_id = ccall(:jl_sockaddr_host6,UInt32,(Ptr{Void},Ptr{UInt128}),sockaddr,host)
        #    cb(IPv6(ntoh(host[1])))
        #    break
        end
        addrinfo = ccall(:jl_next_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
    end
    ccall(:uv_freeaddrinfo,Void,(Ptr{Void},),freeaddrinfo)
end

function getaddrinfo(cb::Function, host::ASCIIString)
    callback_dict[cb] = cb
    uv_error("getaddrinfo",ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Any),
        eventloop(), host, C_NULL, cb))
end
getaddrinfo(cb::Function, host::AbstractString) = getaddrinfo(cb,ascii(host))

function getaddrinfo(host::ASCIIString)
    c = Condition()
    getaddrinfo(host) do IP
        notify(c,IP)
    end
    ip = wait(c)
    isa(ip,UVError) && throw(ip)
    return ip::IPAddr
end
getaddrinfo(host::AbstractString) = getaddrinfo(ascii(host))

const _sizeof_uv_interface_address = ccall(:jl_uv_sizeof_interface_address,Int32,())

function getipaddr()
    addr = Array(Ptr{UInt8},1)
    count = Array(Int32,1)
    lo_present = false
    err = ccall(:jl_uv_interface_addresses,Int32,(Ptr{Ptr{UInt8}},Ptr{Int32}),addr,count)
    addr, count = addr[1],count[1]
    if err != 0
        ccall(:uv_free_interface_addresses,Void,(Ptr{UInt8},Int32),addr,count)
        throw(UVError("getlocalip",err))
    end
    for i = 0:(count-1)
        current_addr = addr + i*_sizeof_uv_interface_address
        if 1 == ccall(:jl_uv_interface_address_is_internal,Int32,(Ptr{UInt8},),current_addr)
            lo_present = true
            continue
        end
        sockaddr = ccall(:jl_uv_interface_address_sockaddr,Ptr{Void},(Ptr{UInt8},),current_addr)
        if ccall(:jl_sockaddr_in_is_ip4,Int32,(Ptr{Void},),sockaddr) == 1
            return IPv4(ntoh(ccall(:jl_sockaddr_host4,UInt32,(Ptr{Void},),sockaddr)))
        # Uncomment to enbable IPv6
        #elseif ccall(:jl_sockaddr_in_is_ip6,Int32,(Ptr{Void},),sockaddr) == 1
        #   host = Array(UInt128,1)
        #   ccall(:jl_sockaddr_host6,UInt32,(Ptr{Void},Ptr{UInt128}),sockaddrr,host)
        #   return IPv6(ntoh(host[1]))
        end
    end
    ccall(:uv_free_interface_addresses,Void,(Ptr{UInt8},Int32),addr,count)
    lo_present ? ip"127.0.0.1" : error("No networking interface available")
end

##

function connect!(sock::TCPSocket, host::IPv4, port::Integer)
    @assert sock.status == StatusInit
    if !(0 <= port <= typemax(UInt16))
        throw(DomainError())
    end
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},UInt32,UInt16),
                 sock.handle,hton(host.host),hton(uint16(port))))
    sock.status = StatusConnecting
end

function connect!(sock::TCPSocket, host::IPv6, port::Integer)
    @assert sock.status == StatusInit
    if !(0 <= port <= typemax(UInt16))
        throw(DomainError())
    end
    uv_error("connect",ccall(:jl_tcp6_connect,Int32,(Ptr{Void},Ptr{UInt128},UInt16),
                 sock.handle,&hton(host.host),hton(uint16(port))))
    sock.status = StatusConnecting
end

# Default Host to localhost
connect(sock::TCPSocket, port::Integer) = connect(sock,IPv4(127,0,0,1),port)
connect(port::Integer) = connect(IPv4(127,0,0,1),port)

# Valid connect signatures for TCP
connect(host::AbstractString, port::Integer) = connect(TCPSocket(),host,port)
connect(addr::IPAddr, port::Integer) = connect(TCPSocket(),addr,port)
connect(addr::InetAddr) = connect(TCPSocket(),addr)

default_connectcb(sock,status) = nothing

function connect!(sock::TCPSocket, host::AbstractString, port::Integer)
    @assert sock.status == StatusInit
    ipaddr = getaddrinfo(host)
    sock.status = StatusInit
    connect!(sock,ipaddr,port)
    sock.status = StatusConnecting
    sock
end

##

listen(sock::UVServer; backlog::Integer=BACKLOG_DEFAULT) = (uv_error("listen",_listen(sock;backlog=backlog)); sock)

function listen(addr; backlog::Integer=BACKLOG_DEFAULT)
    sock = TCPServer()
    !bind(sock,addr) && error("cannot bind to port; may already be in use or access denied")
    uv_error("listen",_listen(sock;backlog=backlog))
    sock
end
listen(port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(IPv4(uint32(0)),port;backlog=backlog)
listen(host::IPAddr, port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(InetAddr(host,port);backlog=backlog)

listen(cb::Callback,args...; backlog::Integer=BACKLOG_DEFAULT) = (sock=listen(args...;backlog=backlog);sock.ccb=cb;sock)
listen(cb::Callback,sock::Socket; backlog::Integer=BACKLOG_DEFAULT) = (sock.ccb=cb;listen(sock;backlog=backlog))

##

function accept_nonblock(server::TCPServer,client::TCPSocket)
    @assert client.status == StatusInit
    err = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server.handle,client.handle)
    if err == 0
        client.status = StatusOpen
    end
    err
end
function accept_nonblock(server::TCPServer)
    client = TCPSocket()
    uv_error("accept", accept_nonblock(server, client))
    client
end

## Utility functions

function listenany(default_port)
    addr = InetAddr(IPv4(uint32(0)),default_port)
    while true
        sock = TCPServer()
        if bind(sock,addr) && _listen(sock) == 0
            return (addr.port,sock)
        end
        close(sock)
        addr.port += 1
        if addr.port==default_port
            error("no ports available")
        end
    end
end
