# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sockets

export
    accept,
    bind,
    connect,
    getaddrinfo,
    getalladdrinfo,
    getnameinfo,
    getipaddr,
    getpeername,
    getsockname,
    listen,
    listenany,
    recv,
    recvfrom,
    send,
    TCPSocket,
    UDPSocket,
    @ip_str,
    IPAddr,
    IPv4,
    IPv6

import Base: isless, show, print, parse, bind, convert, isreadable, iswritable, alloc_buf_hook, _uv_hook_close

using Base: LibuvStream, LibuvServer, PipeEndpoint, @handle_as, uv_error, associate_julia_struct, uvfinalize,
    notify_error, stream_wait, uv_req_data, uv_req_set_data, preserve_handle, unpreserve_handle, UVError,
    eventloop, StatusUninit, StatusInit, StatusConnecting, StatusOpen, StatusClosing, StatusClosed, StatusActive,
    uv_status_string, check_open, wait_connected,
    UV_EINVAL, UV_ENOMEM, UV_ENOBUFS, UV_EAGAIN, UV_ECONNABORTED, UV_EADDRINUSE, UV_EACCES, UV_EADDRNOTAVAIL,
    UV_EAI_ADDRFAMILY, UV_EAI_AGAIN, UV_EAI_BADFLAGS,
    UV_EAI_BADHINTS, UV_EAI_CANCELED, UV_EAI_FAIL,
    UV_EAI_FAMILY, UV_EAI_NODATA, UV_EAI_NONAME,
    UV_EAI_OVERFLOW, UV_EAI_PROTOCOL, UV_EAI_SERVICE,
    UV_EAI_SOCKTYPE, UV_EAI_MEMORY

include("IPAddr.jl")
include("addrinfo.jl")

"""
    TCPSocket(; delay=true)

Open a TCP socket using libuv. If `delay` is true, libuv delays creation of the
socket's file descriptor till the first [`bind`](@ref) call. `TCPSocket` has various
fields to denote the state of the socket as well as its send/receive buffers.
"""
mutable struct TCPSocket <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    connectnotify::Condition
    closenotify::Condition
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock
    throttle::Int

    function TCPSocket(handle::Ptr{Cvoid}, status)
        tcp = new(
                handle,
                status,
                PipeBuffer(),
                Condition(),
                Condition(),
                Condition(),
                nothing,
                ReentrantLock(),
                Base.DEFAULT_READ_BUFFER_SZ)
        associate_julia_struct(tcp.handle, tcp)
        finalizer(uvfinalize, tcp)
        return tcp
    end
end

# kw arg "delay": if true, libuv delays creation of the socket fd till the first bind call
function TCPSocket(; delay=true)
    tcp = TCPSocket(Libc.malloc(Base._sizeof_uv_tcp), StatusUninit)
    af_spec = delay ? 0 : 2   # AF_UNSPEC is 0, AF_INET is 2
    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp socket", err)
    tcp.status = StatusInit
    return tcp
end

mutable struct TCPServer <: LibuvServer
    handle::Ptr{Cvoid}
    status::Int
    connectnotify::Condition
    closenotify::Condition

    function TCPServer(handle::Ptr{Cvoid}, status)
        tcp = new(
            handle,
            status,
            Condition(),
            Condition())
        associate_julia_struct(tcp.handle, tcp)
        finalizer(uvfinalize, tcp)
        return tcp
    end
end

# Keyword arg "delay": if true, libuv delays creation of socket fd till bind.
# It can be set to false if there is a need to set socket options before
# further calls to `bind` and `listen`, e.g. `SO_REUSEPORT`.
function TCPServer(; delay=true)
    tcp = TCPServer(Libc.malloc(Base._sizeof_uv_tcp), StatusUninit)
    af_spec = delay ? 0 : 2   # AF_UNSPEC is 0, AF_INET is 2
    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp server", err)
    tcp.status = StatusInit
    return tcp
end

isreadable(io::TCPSocket) = isopen(io) || bytesavailable(io) > 0
iswritable(io::TCPSocket) = isopen(io) && io.status != StatusClosing

"""
    accept(server[,client])

Accepts a connection on the given server and returns a connection to the client. An
uninitialized client stream may be provided, in which case it will be used instead of
creating a new stream.
"""
accept(server::TCPServer) = accept(server, TCPSocket())

# UDP
"""
    UDPSocket()

Open a UDP socket using libuv. `UDPSocket` has various
fields to denote the state of the socket.
"""
mutable struct UDPSocket <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    recvnotify::Condition
    sendnotify::Condition
    closenotify::Condition

    function UDPSocket(handle::Ptr{Cvoid}, status)
        udp = new(
            handle,
            status,
            Condition(),
            Condition(),
            Condition())
        associate_julia_struct(udp.handle, udp)
        finalizer(uvfinalize, udp)
        return udp
    end
end
function UDPSocket()
    this = UDPSocket(Libc.malloc(Base._sizeof_uv_udp), StatusUninit)
    err = ccall(:uv_udp_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
                eventloop(), this.handle)
    uv_error("failed to create udp socket", err)
    this.status = StatusInit
    return this
end

show(io::IO, stream::UDPSocket) = print(io, typeof(stream), "(", uv_status_string(stream), ")")

function _uv_hook_close(sock::UDPSocket)
    sock.handle = C_NULL
    sock.status = StatusClosed
    notify(sock.closenotify)
    notify(sock.sendnotify)
    notify_error(sock.recvnotify,EOFError())
end

# Disables dual stack mode.
const UV_TCP_IPV6ONLY = 1

# Disables dual stack mode. Only available when using ipv6 binf
const UV_UDP_IPV6ONLY = 1

# Indicates message was truncated because read buffer was too small. The
# remainder was discarded by the OS.
const UV_UDP_PARTIAL = 2

# Indicates if SO_REUSEADDR will be set when binding the handle in uv_udp_bind. This sets
# the SO_REUSEPORT socket flag on the BSDs and OS X. On other Unix platforms, it sets the
# SO_REUSEADDR flag. What that means is that multiple threads or processes can bind to the
# same address without error (provided they all set the flag) but only the last one to bind
# will receive any traffic, in effect "stealing" the port from the previous listener.
const UV_UDP_REUSEADDR = 4

##

_bind(sock::TCPServer, host::IPv4, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_tcp_bind, Int32, (Ptr{Cvoid}, UInt16, UInt32, Cuint),
            sock.handle, hton(port), hton(host.host), flags)

_bind(sock::TCPServer, host::IPv6, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_tcp_bind6, Int32, (Ptr{Cvoid}, UInt16, Ptr{UInt128}, Cuint),
            sock.handle, hton(port), Ref(hton(host.host)), flags)

_bind(sock::UDPSocket, host::IPv4, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_udp_bind, Int32, (Ptr{Cvoid}, UInt16, UInt32, UInt32),
            sock.handle, hton(port), hton(host.host), flags)

_bind(sock::UDPSocket, host::IPv6, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_udp_bind6, Int32, (Ptr{Cvoid}, UInt16, Ptr{UInt128}, UInt32),
            sock.handle, hton(port), Ref(hton(host.host)), flags)

"""
    bind(socket::Union{UDPSocket, TCPSocket}, host::IPAddr, port::Integer; ipv6only=false, reuseaddr=false, kws...)

Bind `socket` to the given `host:port`. Note that `0.0.0.0` will listen on all devices.

* The `ipv6only` parameter disables dual stack mode. If `ipv6only=true`, only an IPv6 stack is created.
* If `reuseaddr=true`, multiple threads or processes can bind to the same address without error
  if they all set `reuseaddr=true`, but only the last to bind will receive any traffic.
"""
function bind(sock::Union{TCPServer, UDPSocket}, host::IPAddr, port::Integer; ipv6only = false, reuseaddr = false, kws...)
    if sock.status != StatusInit
        error("$(typeof(sock)) is not in initialization state")
    end
    flags = 0
    if isa(host,IPv6) && ipv6only
        flags |= isa(sock, UDPSocket) ? UV_UDP_IPV6ONLY : UV_TCP_IPV6ONLY
    end
    if isa(sock, UDPSocket) && reuseaddr
        flags |= UV_UDP_REUSEADDR
    end
    err = _bind(sock, host, UInt16(port), UInt32(flags))
    if err < 0
        if err != UV_EADDRINUSE && err != UV_EACCES && err != UV_EADDRNOTAVAIL
            #TODO: this codepath is not currently tested
            throw(UVError("bind", err))
        else
            return false
        end
    end
    sock.status = StatusOpen
    isa(sock, UDPSocket) && setopt(sock; kws...)
    return true
end

bind(sock::TCPServer, addr::InetAddr) = bind(sock, addr.host, addr.port)

"""
    setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

Set UDP socket options.

* `multicast_loop`: loopback for multicast packets (default: `true`).
* `multicast_ttl`: TTL for multicast packets (default: `nothing`).
* `enable_broadcast`: flag must be set to `true` if socket will be used for broadcast
  messages, or else the UDP system will return an access error (default: `false`).
* `ttl`: Time-to-live of packets sent on the socket (default: `nothing`).
"""
function setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)
    if sock.status == StatusUninit
        error("Cannot set options on uninitialized socket")
    end
    if multicast_loop !== nothing
        uv_error("multicast_loop",ccall(:uv_udp_set_multicast_loop,Cint,(Ptr{Cvoid},Cint),sock.handle,multicast_loop) < 0)
    end
    if multicast_ttl !== nothing
        uv_error("multicast_ttl",ccall(:uv_udp_set_multicast_ttl,Cint,(Ptr{Cvoid},Cint),sock.handle,multicast_ttl))
    end
    if enable_broadcast !== nothing
        uv_error("enable_broadcast",ccall(:uv_udp_set_broadcast,Cint,(Ptr{Cvoid},Cint),sock.handle,enable_broadcast))
    end
    if ttl !== nothing
        uv_error("ttl",ccall(:uv_udp_set_ttl,Cint,(Ptr{Cvoid},Cint),sock.handle,ttl))
    end
end

"""
    recv(socket::UDPSocket)

Read a UDP packet from the specified socket, and return the bytes received. This call blocks.
"""
function recv(sock::UDPSocket)
    addr, data = recvfrom(sock)
    return data
end

"""
    recvfrom(socket::UDPSocket) -> (address, data)

Read a UDP packet from the specified socket, returning a tuple of `(address, data)`, where
`address` will be either IPv4 or IPv6 as appropriate.
"""
function recvfrom(sock::UDPSocket)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen && sock.status != StatusActive
        error("UDPSocket is not initialized and open")
    end
    if ccall(:uv_is_active, Cint, (Ptr{Cvoid},), sock.handle) == 0
        uv_error("recv_start", ccall(:uv_udp_recv_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
                                    sock.handle, Base.uv_jl_alloc_buf::Ptr{Cvoid}, uv_jl_recvcb::Ptr{Cvoid}))
    end
    sock.status = StatusActive
    return stream_wait(sock, sock.recvnotify)::Tuple{Union{IPv4, IPv6}, Vector{UInt8}}
end

alloc_buf_hook(sock::UDPSocket, size::UInt) = (Libc.malloc(size), size)

function uv_recvcb(handle::Ptr{Cvoid}, nread::Cssize_t, buf::Ptr{Cvoid}, addr::Ptr{Cvoid}, flags::Cuint)
    # C signature documented as (*uv_udp_recv_cb)(...)
    sock = @handle_as handle UDPSocket
    if nread < 0
        Libc.free(buf_addr)
        notify_error(sock.recvnotify, UVError("recv", nread))
    elseif flags & UV_UDP_PARTIAL > 0
        Libc.free(buf_addr)
        notify_error(sock.recvnotify, "Partial message received")
    else
        buf_addr = ccall(:jl_uv_buf_base, Ptr{Cvoid}, (Ptr{Cvoid},), buf)
        buf_size = ccall(:jl_uv_buf_len, Csize_t, (Ptr{Cvoid},), buf)
        # need to check the address type in order to convert to a Julia IPAddr
        addrout = if addr == C_NULL
                      IPv4(0)
                  elseif ccall(:jl_sockaddr_in_is_ip4, Cint, (Ptr{Cvoid},), addr) == 1
                      IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Cvoid},), addr)))
                  else
                      tmp = [UInt128(0)]
                      ccall(:jl_sockaddr_host6, UInt32, (Ptr{Cvoid}, Ptr{UInt8}), addr, pointer(tmp))
                      IPv6(ntoh(tmp[1]))
                  end
        buf = unsafe_wrap(Array, convert(Ptr{UInt8}, buf_addr), Int(nread), own = true)
        notify(sock.recvnotify, (addrout, buf))
    end
    ccall(:uv_udp_recv_stop, Cint, (Ptr{Cvoid},), sock.handle)
    sock.status = StatusOpen
    nothing
end

function _send(sock::UDPSocket, ipaddr::IPv4, port::UInt16, buf)
    ccall(:jl_udp_send, Cint, (Ptr{Cvoid}, UInt16, UInt32, Ptr{UInt8}, Csize_t, Ptr{Cvoid}),
          sock.handle, hton(port), hton(ipaddr.host), buf, sizeof(buf), uv_jl_sendcb::Ptr{Cvoid})
end

function _send(sock::UDPSocket, ipaddr::IPv6, port::UInt16, buf)
    ccall(:jl_udp_send6, Cint, (Ptr{Cvoid}, UInt16, Ref{UInt128}, Ptr{UInt8}, Csize_t, Ptr{Cvoid}),
          sock.handle, hton(port), hton(ipaddr.host), buf, sizeof(buf), uv_jl_sendcb::Ptr{Cvoid})
end

"""
    send(socket::UDPSocket, host, port::Integer, msg)

Send `msg` over `socket` to `host:port`.
"""
function send(sock::UDPSocket,ipaddr,port,msg)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen && sock.status != StatusActive
        error("UDPSocket is not initialized and open")
    end
    uv_error("send", _send(sock, ipaddr, UInt16(port), msg))
    stream_wait(sock, sock.sendnotify)
    nothing
end

function uv_sendcb(handle::Ptr{Cvoid}, status::Cint)
    sock = @handle_as handle UDPSocket
    if status < 0
        notify_error(sock.sendnotify, UVError("UDP send failed", status))
    end
    notify(sock.sendnotify)
    Libc.free(handle)
    nothing
end


#from `connect`
function uv_connectcb(conn::Ptr{Cvoid}, status::Cint)
    hand = ccall(:jl_uv_connect_handle, Ptr{Cvoid}, (Ptr{Cvoid},), conn)
    sock = @handle_as hand LibuvStream
    if status >= 0
        if !(sock.status == StatusClosed || sock.status == StatusClosing)
            sock.status = StatusOpen
        end
        notify(sock.connectnotify)
    else
        ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), hand)
        err = UVError("connect", status)
        notify_error(sock.connectnotify, err)
    end
    Libc.free(conn)
    nothing
end

function connect!(sock::TCPSocket, host::IPv4, port::Integer)
    if sock.status != StatusInit
        error("TCPSocket is not in initialization state")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    uv_error("connect", ccall(:jl_tcp4_connect, Int32, (Ptr{Cvoid}, UInt32, UInt16, Ptr{Cvoid}),
                             sock.handle, hton(host.host), hton(UInt16(port)), uv_jl_connectcb::Ptr{Cvoid}))
    sock.status = StatusConnecting
    nothing
end

function connect!(sock::TCPSocket, host::IPv6, port::Integer)
    if sock.status != StatusInit
        error("TCPSocket is not in initialization state")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    uv_error("connect", ccall(:jl_tcp6_connect, Int32, (Ptr{Cvoid}, Ref{UInt128}, UInt16, Ptr{Cvoid}),
                              sock.handle, hton(host.host), hton(UInt16(port)), uv_jl_connectcb::Ptr{Cvoid}))
    sock.status = StatusConnecting
    nothing
end

connect!(sock::TCPSocket, addr::InetAddr) = connect!(sock, addr.host, addr.port)

# Default Host to localhost

"""
    connect([host], port::Integer) -> TCPSocket

Connect to the host `host` on port `port`.
"""
connect(sock::TCPSocket, port::Integer) = connect(sock, localhost, port)
connect(port::Integer) = connect(localhost, port)

# Valid connect signatures for TCP
connect(host::AbstractString, port::Integer) = connect(TCPSocket(), host, port)
connect(addr::IPAddr, port::Integer) = connect(TCPSocket(), addr, port)
connect(addr::InetAddr) = connect(TCPSocket(), addr)

function connect!(sock::TCPSocket, host::AbstractString, port::Integer)
    if sock.status != StatusInit
        error("TCPSocket is not in initialization state")
    end
    ipaddr = getaddrinfo(host)
    sock.status = StatusInit
    connect!(sock,ipaddr,port)
    sock.status = StatusConnecting
    return sock
end

function connect(sock::LibuvStream, args...)
    connect!(sock, args...)
    wait_connected(sock)
    return sock
end

##

const BACKLOG_DEFAULT = 511

"""
    listen([addr, ]port::Integer; backlog::Integer=BACKLOG_DEFAULT) -> TCPServer

Listen on port on the address specified by `addr`.
By default this listens on `localhost` only.
To listen on all interfaces pass `IPv4(0)` or `IPv6(0)` as appropriate.
`backlog` determines how many connections can be pending (not having
called [`accept`](@ref)) before the server will begin to
reject them. The default value of `backlog` is 511.
"""
function listen(addr; backlog::Integer=BACKLOG_DEFAULT)
    sock = TCPServer()
    !bind(sock, addr) && error("cannot bind to port; may already be in use or access denied")
    listen(sock; backlog=backlog)
    return sock
end
listen(port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(localhost, port; backlog=backlog)
listen(host::IPAddr, port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(InetAddr(host, port); backlog=backlog)

function listen(callback, server::Union{TCPSocket, UDPSocket})
    @async begin
        local client = TCPSocket()
        while isopen(server)
            err = accept_nonblock(server, client)
            if err == 0
                callback(client)
                client = TCPSocket()
            elseif err != UV_EAGAIN
                uv_error("accept", err)
            else
                stream_wait(server, server.connectnotify)
            end
        end
    end
    return sock
end

function listen(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    uv_error("listen", trylisten(sock))
    return sock
end

# from `listen`
function uv_connectioncb(stream::Ptr{Cvoid}, status::Cint)
    sock = @handle_as stream LibuvServer
    if status >= 0
        notify(sock.connectnotify)
    else
        err = UVError("connection", status)
        notify_error(sock.connectnotify, err)
    end
    nothing
end

function trylisten(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    check_open(sock)
    err = ccall(:uv_listen, Cint, (Ptr{Cvoid}, Cint, Ptr{Cvoid}),
                sock, backlog, uv_jl_connectioncb::Ptr{Cvoid})
    sock.status = StatusActive
    return err
end

##

function accept_nonblock(server::TCPServer, client::TCPSocket)
    if client.status != StatusInit
        error("client TCPSocket is not in initialization state")
    end
    err = ccall(:uv_accept, Int32, (Ptr{Cvoid}, Ptr{Cvoid}), server.handle, client.handle)
    if err == 0
        client.status = StatusOpen
    end
    return err
end

function accept_nonblock(server::TCPServer)
    client = TCPSocket()
    uv_error("accept", accept_nonblock(server, client))
    return client
end

function accept(server::LibuvServer, client::LibuvStream)
    if server.status != StatusActive
        throw(ArgumentError("server not connected, make sure \"listen\" has been called"))
    end
    while isopen(server)
        err = accept_nonblock(server, client)
        if err == 0
            return client
        elseif err != UV_EAGAIN
            uv_error("accept", err)
        end
        stream_wait(server, server.connectnotify)
    end
    uv_error("accept", UV_ECONNABORTED)
end

## Utility functions

const localhost = ip"127.0.0.1"

"""
    listenany([host::IPAddr,] port_hint) -> (UInt16, TCPServer)

Create a `TCPServer` on any port, using hint as a starting point. Returns a tuple of the
actual port that the server was created on and the server itself.
"""
function listenany(host::IPAddr, default_port)
    addr = InetAddr(host, default_port)
    while true
        sock = TCPServer()
        if bind(sock, addr) && trylisten(sock) == 0
            if default_port == 0
                _addr, port = getsockname(sock)
                return (port, sock)
            end
            return (addr.port, sock)
        end
        close(sock)
        addr = InetAddr(addr.host, addr.port + 1)
        if addr.port == default_port
            error("no ports available")
        end
    end
end

listenany(default_port) = listenany(localhost, default_port)

"""
    getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr, UInt16)

Get the IP address and port that the given socket is bound to.
"""
getsockname(sock::Union{TCPSocket, TCPServer}) = _sockname(sock, true)


"""
    getpeername(sock::TCPSocket) -> (IPAddr, UInt16)

Get the IP address and port of the remote endpoint that the given
socket is connected to. Valid only for connected TCP sockets.
"""
getpeername(sock::TCPSocket) = _sockname(sock, false)

function _sockname(sock, self=true)
    rport = Ref{Cushort}(0)
    raddress = zeros(UInt8, 16)
    rfamily = Ref{Cuint}(0)

    if self
        r = ccall(:jl_tcp_getsockname, Int32,
                (Ptr{Cvoid}, Ref{Cushort}, Ptr{Cvoid}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    else
        r = ccall(:jl_tcp_getpeername, Int32,
                (Ptr{Cvoid}, Ref{Cushort}, Ptr{Cvoid}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    end
    uv_error("cannot obtain socket name", r)
    if r == 0
        port = ntoh(rport[])
        af_inet6 = @static if Sys.iswindows() # AF_INET6 in <sys/socket.h>
            23
        elseif Sys.isapple()
            30
        elseif Sys.KERNEL ∈ (:FreeBSD, :DragonFly)
            28
        elseif Sys.KERNEL ∈ (:NetBSD, :OpenBSD)
            24
        else
            10
        end

        if rfamily[] == 2 # AF_INET
            addrv4 = raddress[1:4]
            naddr = ntoh(unsafe_load(Ptr{Cuint}(pointer(addrv4)), 1))
            addr = IPv4(naddr)
        elseif rfamily[] == af_inet6
            naddr = ntoh(unsafe_load(Ptr{UInt128}(pointer(raddress)), 1))
            addr = IPv6(naddr)
        else
            error(string("unsupported address family: ", getindex(rfamily)))
        end
    else
        error("cannot obtain socket name")
    end
    return addr, port
end

# domain sockets

include("PipeServer.jl")

# libuv callback handles

function __init__()
    global uv_jl_getaddrinfocb = cfunction(uv_getaddrinfocb, Cvoid, Tuple{Ptr{Cvoid}, Cint, Ptr{Cvoid}})
    global uv_jl_getnameinfocb = cfunction(uv_getnameinfocb, Cvoid, Tuple{Ptr{Cvoid}, Cint, Cstring, Cstring})
    global uv_jl_recvcb        = cfunction(uv_recvcb, Cvoid, Tuple{Ptr{Cvoid}, Cssize_t, Ptr{Cvoid}, Ptr{Cvoid}, Cuint})
    global uv_jl_sendcb        = cfunction(uv_sendcb, Cvoid, Tuple{Ptr{Cvoid}, Cint})
    global uv_jl_connectioncb  = cfunction(uv_connectioncb, Cvoid, Tuple{Ptr{Cvoid}, Cint})
    global uv_jl_connectcb     = cfunction(uv_connectcb, Cvoid, Tuple{Ptr{Cvoid}, Cint})
end

# deprecations

@deprecate convert(dt::Type{<:Integer}, ip::IPAddr)  dt(ip)

@noinline function getaddrinfo(callback::Function, host::AbstractString)
    Base.depwarn("`getaddrinfo` with a callback function is deprecated, wrap code in `@async` instead for deferred execution.", :getaddrinfo)
    @async begin
        r = getaddrinfo(host)
        callback(r)
    end
    nothing
end

end
