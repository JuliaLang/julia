# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Support for sockets. Provides [`IPAddr`](@ref) and subtypes, [`TCPSocket`](@ref), and [`UDPSocket`](@ref).
"""
module Sockets

export
    accept,
    bind,
    connect,
    getaddrinfo,
    getalladdrinfo,
    getnameinfo,
    getipaddr,
    getipaddrs,
    islinklocaladdr,
    getpeername,
    getsockname,
    listen,
    listenany,
    recv,
    recvfrom,
    send,
    join_multicast_group,
    leave_multicast_group,
    TCPSocket,
    UDPSocket,
    @ip_str,
    IPAddr,
    IPv4,
    IPv6

import Base: isless, show, print, parse, bind, convert, isreadable, iswritable, alloc_buf_hook, _uv_hook_close

using Base: LibuvStream, LibuvServer, PipeEndpoint, @handle_as, uv_error, associate_julia_struct, uvfinalize,
    notify_error, uv_req_data, uv_req_set_data, preserve_handle, unpreserve_handle, _UVError, IOError,
    eventloop, StatusUninit, StatusInit, StatusConnecting, StatusOpen, StatusClosing, StatusClosed, StatusActive,
    preserve_handle, unpreserve_handle, iolock_begin, iolock_end,
    uv_status_string, check_open, OS_HANDLE, RawFD,
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
    cond::Base.ThreadSynchronizer
    readerror::Any
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock # advisory lock
    throttle::Int

    function TCPSocket(handle::Ptr{Cvoid}, status)
        tcp = new(
                handle,
                status,
                PipeBuffer(),
                Base.ThreadSynchronizer(),
                nothing,
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
    iolock_begin()
    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp socket", err)
    tcp.status = StatusInit
    iolock_end()
    return tcp
end

function TCPSocket(fd::OS_HANDLE)
    tcp = TCPSocket()
    iolock_begin()
    err = ccall(:uv_tcp_open, Int32, (Ptr{Cvoid}, OS_HANDLE), pipe.handle, fd)
    uv_error("tcp_open", err)
    tcp.status = StatusOpen
    iolock_end()
    return tcp
end
if OS_HANDLE != RawFD
    TCPSocket(fd::RawFD) = TCPSocket(Libc._get_osfhandle(fd))
end


mutable struct TCPServer <: LibuvServer
    handle::Ptr{Cvoid}
    status::Int
    cond::Base.ThreadSynchronizer

    function TCPServer(handle::Ptr{Cvoid}, status)
        tcp = new(
            handle,
            status,
            Base.ThreadSynchronizer())
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
    iolock_begin()
    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp server", err)
    tcp.status = StatusInit
    iolock_end()
    return tcp
end

isreadable(io::TCPSocket) = isopen(io) || bytesavailable(io) > 0
iswritable(io::TCPSocket) = isopen(io) && io.status != StatusClosing

"""
    accept(server[, client])

Accepts a connection on the given server and returns a connection to the client. An
uninitialized client stream may be provided, in which case it will be used instead of
creating a new stream.
"""
accept(server::TCPServer) = accept(server, TCPSocket())

function accept(callback, server::LibuvServer)
    task = @async try
            while true
                client = accept(server)
                callback(client)
            end
        catch ex
            # accept below may explicitly throw UV_ECONNABORTED:
            # filter that out since we expect that error
            if !(ex isa IOError && ex.code == UV_ECONNABORTED) || isopen(server)
                rethrow()
            end
        end
    return task # caller is responsible for checking for errors
end


# UDP
"""
    UDPSocket()

Open a UDP socket using libuv. `UDPSocket` has various
fields to denote the state of the socket.
"""
mutable struct UDPSocket <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    recvnotify::Base.ThreadSynchronizer
    cond::Base.ThreadSynchronizer

    function UDPSocket(handle::Ptr{Cvoid}, status)
        cond = Base.ThreadSynchronizer()
        udp = new(handle, status, Base.ThreadSynchronizer(cond.lock), cond)
        associate_julia_struct(udp.handle, udp)
        finalizer(uvfinalize, udp)
        return udp
    end
end
function UDPSocket()
    this = UDPSocket(Libc.malloc(Base._sizeof_uv_udp), StatusUninit)
    iolock_begin()
    err = ccall(:uv_udp_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
                eventloop(), this.handle)
    uv_error("failed to create udp socket", err)
    this.status = StatusInit
    iolock_end()
    return this
end

show(io::IO, stream::UDPSocket) = print(io, typeof(stream), "(", uv_status_string(stream), ")")

function _uv_hook_close(sock::UDPSocket)
    sock.handle = C_NULL
    lock(sock.cond)
    try
        sock.status = StatusClosed
        notify(sock.cond)
        notify_error(sock.recvnotify, EOFError())
    finally
        unlock(sock.cond)
    end
    nothing
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

function _bind(sock::Union{TCPServer, TCPSocket}, host::Union{IPv4, IPv6}, port::UInt16, flags::UInt32=UInt32(0))
    host_in = Ref(hton(host.host))
    return ccall(:jl_tcp_bind, Int32, (Ptr{Cvoid}, UInt16, Ptr{Cvoid}, Cuint, Cint),
            sock, hton(port), host_in, flags, host isa IPv6)
end

function _bind(sock::UDPSocket, host::Union{IPv4, IPv6}, port::UInt16, flags::UInt32=UInt32(0))
    host_in = Ref(hton(host.host))
    return ccall(:jl_udp_bind, Int32, (Ptr{Cvoid}, UInt16, Ptr{Cvoid}, Cuint, Cint),
            sock, hton(port), host_in, flags, host isa IPv6)
end

"""
    bind(socket::Union{TCPServer, UDPSocket, TCPSocket}, host::IPAddr, port::Integer; ipv6only=false, reuseaddr=false, kws...)

Bind `socket` to the given `host:port`. Note that `0.0.0.0` will listen on all devices.

* The `ipv6only` parameter disables dual stack mode. If `ipv6only=true`, only an IPv6 stack is created.
* If `reuseaddr=true`, multiple threads or processes can bind to the same address without error
  if they all set `reuseaddr=true`, but only the last to bind will receive any traffic.
"""
function bind(sock::Union{TCPServer, UDPSocket, TCPSocket}, host::IPAddr, port::Integer; ipv6only = false, reuseaddr = false, kws...)
    if sock.status != StatusInit
        error("$(typeof(sock)) is not in initialization state")
    end
    flags = 0
    if isa(host, IPv6) && ipv6only
        flags |= isa(sock, UDPSocket) ? UV_UDP_IPV6ONLY : UV_TCP_IPV6ONLY
    end
    if isa(sock, UDPSocket) && reuseaddr
        flags |= UV_UDP_REUSEADDR
    end
    iolock_begin()
    err = _bind(sock, host, UInt16(port), UInt32(flags))
    if err < 0
        iolock_end()
        if err != UV_EADDRINUSE && err != UV_EACCES && err != UV_EADDRNOTAVAIL
            #TODO: this codepath is not currently tested
            throw(_UVError("bind", err))
        else
            return false
        end
    end
    if isa(sock, TCPServer) || isa(sock, UDPSocket)
        sock.status = StatusOpen
    end
    isa(sock, UDPSocket) && setopt(sock; kws...)
    iolock_end()
    return true
end

bind(sock::TCPServer, addr::InetAddr) = bind(sock, addr.host, addr.port)

"""
    setopt(sock::UDPSocket; multicast_loop=nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

Set UDP socket options.

* `multicast_loop`: loopback for multicast packets (default: `true`).
* `multicast_ttl`: TTL for multicast packets (default: `nothing`).
* `enable_broadcast`: flag must be set to `true` if socket will be used for broadcast
  messages, or else the UDP system will return an access error (default: `false`).
* `ttl`: Time-to-live of packets sent on the socket (default: `nothing`).
"""
function setopt(sock::UDPSocket; multicast_loop=nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)
    iolock_begin()
    if sock.status == StatusUninit
        error("Cannot set options on uninitialized socket")
    end
    if multicast_loop !== nothing
        uv_error("multicast_loop", ccall(:uv_udp_set_multicast_loop, Cint, (Ptr{Cvoid}, Cint), sock.handle, multicast_loop) < 0)
    end
    if multicast_ttl !== nothing
        uv_error("multicast_ttl", ccall(:uv_udp_set_multicast_ttl, Cint, (Ptr{Cvoid}, Cint), sock.handle, multicast_ttl))
    end
    if enable_broadcast !== nothing
        uv_error("enable_broadcast", ccall(:uv_udp_set_broadcast, Cint, (Ptr{Cvoid}, Cint), sock.handle, enable_broadcast))
    end
    if ttl !== nothing
        uv_error("ttl", ccall(:uv_udp_set_ttl, Cint, (Ptr{Cvoid}, Cint), sock.handle, ttl))
    end
    iolock_end()
    nothing
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
    recvfrom(socket::UDPSocket) -> (host_port, data)

Read a UDP packet from the specified socket, returning a tuple of `(host_port, data)`, where
`host_port` will be an InetAddr{IPv4} or InetAddr{IPv6}, as appropriate.

!!! compat "Julia 1.3"
    Prior to Julia version 1.3, the first returned value was an address (`IPAddr`).
    In version 1.3 it was changed to an `InetAddr`.
"""
function recvfrom(sock::UDPSocket)
    iolock_begin()
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen && sock.status != StatusActive
        error("UDPSocket is not initialized and open")
    end
    if ccall(:uv_is_active, Cint, (Ptr{Cvoid},), sock.handle) == 0
        err = ccall(:uv_udp_recv_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
                    sock, Base.uv_jl_alloc_buf::Ptr{Cvoid}, uv_jl_recvcb::Ptr{Cvoid})
        uv_error("recv_start", err)
    end
    sock.status = StatusActive
    lock(sock.recvnotify)
    iolock_end()
    try
        From = Union{InetAddr{IPv4}, InetAddr{IPv6}}
        Data = Vector{UInt8}
        from, data = wait(sock.recvnotify)::Tuple{From, Data}
        return (from, data)
    finally
        unlock(sock.recvnotify)
    end
end

alloc_buf_hook(sock::UDPSocket, size::UInt) = (Libc.malloc(size), size) # size is always 64k from libuv

function uv_recvcb(handle::Ptr{Cvoid}, nread::Cssize_t, buf::Ptr{Cvoid}, addr::Ptr{Cvoid}, flags::Cuint)
    sock = @handle_as handle UDPSocket
    lock(sock.recvnotify)
    try
        buf_addr = ccall(:jl_uv_buf_base, Ptr{UInt8}, (Ptr{Cvoid},), buf)
        if nread == 0 && addr == C_NULL
            Libc.free(buf_addr)
        elseif nread < 0
            Libc.free(buf_addr)
            notify_error(sock.recvnotify, _UVError("recv", nread))
        elseif flags & UV_UDP_PARTIAL > 0
            Libc.free(buf_addr)
            notify_error(sock.recvnotify, "Partial message received")
        else
            buf_size = Int(ccall(:jl_uv_buf_len, Csize_t, (Ptr{Cvoid},), buf))
            if buf_size - nread < 16384 # waste at most 16k (note: buf_size is currently always 64k)
                buf = unsafe_wrap(Array, buf_addr, nread, own=true)
            else
                buf = Vector{UInt8}(undef, nread)
                GC.@preserve buf unsafe_copyto!(pointer(buf), buf_addr, nread)
                Libc.free(buf_addr)
            end
            # need to check the address type in order to convert to a Julia IPAddr
            host = IPv4(0)
            port = UInt16(0)
            if ccall(:jl_sockaddr_is_ip4, Cint, (Ptr{Cvoid},), addr) == 1
                host = IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Cvoid},), addr)))
                port = ntoh(ccall(:jl_sockaddr_port4, UInt16, (Ptr{Cvoid},), addr))
            elseif ccall(:jl_sockaddr_is_ip6, Cint, (Ptr{Cvoid},), addr) == 1
                tmp = Ref{UInt128}(0)
                scope_id = ccall(:jl_sockaddr_host6, UInt32, (Ptr{Cvoid}, Ptr{UInt128}), addr, tmp)
                host = IPv6(ntoh(tmp[]))
                port = ntoh(ccall(:jl_sockaddr_port6, UInt16, (Ptr{Cvoid},), addr))
            end
            from = InetAddr(host, port)
            notify(sock.recvnotify, (from, buf), all=false)
        end
        if sock.status == StatusActive && isempty(sock.recvnotify)
            sock.status = StatusOpen
            ccall(:uv_udp_recv_stop, Cint, (Ptr{Cvoid},), sock)
        end
    finally
        unlock(sock.recvnotify)
    end
    nothing
end

function _send_async(sock::UDPSocket, ipaddr::Union{IPv4, IPv6}, port::UInt16, buf)
    req = Libc.malloc(Base._sizeof_uv_udp_send)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    host_in = Ref(hton(ipaddr.host))
    err = ccall(:jl_udp_send, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt16, Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Ptr{Cvoid}, Cint),
            req, sock, hton(port), host_in, buf, sizeof(buf), Base.uv_jl_writecb_task::Ptr{Cvoid}, ipaddr isa IPv6)
    if err < 0
        Libc.free(req)
        uv_error("send", err)
    end
    return req
end

"""
    send(socket::UDPSocket, host::IPAddr, port::Integer, msg)

Send `msg` over `socket` to `host:port`.
"""
function send(sock::UDPSocket, ipaddr::IPAddr, port::Integer, msg)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    iolock_begin()
    if sock.status != StatusInit && sock.status != StatusOpen && sock.status != StatusActive
        error("UDPSocket is not initialized and open")
    end
    uvw = _send_async(sock, ipaddr, UInt16(port), msg)
    ct = current_task()
    preserve_handle(ct)
    Base.sigatomic_begin()
    uv_req_set_data(uvw, ct)
    iolock_end()
    status = try
        Base.sigatomic_end()
        wait()::Cint
    finally
        Base.sigatomic_end()
        iolock_begin()
        ct.queue === nothing || list_deletefirst!(ct.queue, ct)
        if uv_req_data(uvw) != C_NULL
            # uvw is still alive,
            # so make sure we won't get spurious notifications later
            uv_req_set_data(uvw, C_NULL)
        else
            # done with uvw
            Libc.free(uvw)
        end
        iolock_end()
        unpreserve_handle(ct)
    end
    uv_error("send", status)
    nothing
end


#from `connect`
function uv_connectcb(conn::Ptr{Cvoid}, status::Cint)
    hand = ccall(:jl_uv_connect_handle, Ptr{Cvoid}, (Ptr{Cvoid},), conn)
    sock = @handle_as hand LibuvStream
    lock(sock.cond)
    try
        if status >= 0 # success
            if !(sock.status == StatusClosed || sock.status == StatusClosing)
                sock.status = StatusOpen
            end
        else
            sock.readerror = _UVError("connect", status) # TODO: perhaps we should not reuse readerror for this
            if !(sock.status == StatusClosed || sock.status == StatusClosing)
                ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), hand)
                sock.status = StatusClosing
            end
        end
        notify(sock.cond)
    finally
        unlock(sock.cond)
    end
    Libc.free(conn)
    nothing
end

function connect!(sock::TCPSocket, host::Union{IPv4, IPv6}, port::Integer)
    iolock_begin()
    if sock.status != StatusInit
        error("TCPSocket is not in initialization state")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    host_in = Ref(hton(host.host))
    uv_error("connect", ccall(:jl_tcp_connect, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, UInt16, Ptr{Cvoid}, Cint),
                              sock, host_in, hton(UInt16(port)), uv_jl_connectcb::Ptr{Cvoid}, host isa IPv6))
    sock.status = StatusConnecting
    iolock_end()
    nothing
end

connect!(sock::TCPSocket, addr::InetAddr) = connect!(sock, addr.host, addr.port)

function wait_connected(x::LibuvStream)
    iolock_begin()
    check_open(x)
    isopen(x) || x.readerror === nothing || throw(x.readerror)
    preserve_handle(x)
    lock(x.cond)
    try
        while x.status == StatusConnecting
            iolock_end()
            wait(x.cond)
            unlock(x.cond)
            iolock_begin()
            lock(x.cond)
        end
        isopen(x) || x.readerror === nothing || throw(x.readerror)
    finally
        unlock(x.cond)
        unpreserve_handle(x)
    end
    iolock_end()
    nothing
end

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
    connect!(sock, ipaddr, port)
    return sock
end

function connect(sock::LibuvStream, args...)
    connect!(sock, args...)
    wait_connected(sock)
    return sock
end

"""
    nagle(socket::Union{TCPServer, TCPSocket}, enable::Bool)

Enables or disables Nagle's algorithm on a given TCP server or socket.
"""
function nagle(sock::Union{TCPServer, TCPSocket}, enable::Bool)
    # disable or enable Nagle's algorithm on all OSes
    Sockets.iolock_begin()
    Sockets.check_open(sock)
    err = ccall(:uv_tcp_nodelay, Cint, (Ptr{Cvoid}, Cint), sock.handle, Cint(!enable))
    # TODO: check err
    Sockets.iolock_end()
    return err
end

"""
    quickack(socket::Union{TCPServer, TCPSocket}, enable::Bool)

On Linux systems, the TCP_QUICKACK is disabled or enabled on `socket`.
"""
function quickack(sock::Union{TCPServer, TCPSocket}, enable::Bool)
    Sockets.iolock_begin()
    Sockets.check_open(sock)
    @static if Sys.islinux()
        # tcp_quickack is a linux only option
        if ccall(:jl_tcp_quickack, Cint, (Ptr{Cvoid}, Cint), sock.handle, Cint(enable)) < 0
            @warn "Networking unoptimized ( Error enabling TCP_QUICKACK : $(Libc.strerror(Libc.errno())) )" maxlog=1
        end
    end
    Sockets.iolock_end()
    nothing
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
    bind(sock, addr) || error("cannot bind to port; may already be in use or access denied")
    listen(sock; backlog=backlog)
    return sock
end
listen(port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(localhost, port; backlog=backlog)
listen(host::IPAddr, port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(InetAddr(host, port); backlog=backlog)

function listen(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    uv_error("listen", trylisten(sock))
    return sock
end

# from `listen`
function uv_connectioncb(stream::Ptr{Cvoid}, status::Cint)
    sock = @handle_as stream LibuvServer
    lock(sock.cond)
    try
        if status >= 0
            notify(sock.cond)
        else
            notify_error(sock.cond, _UVError("connection", status))
        end
    finally
        unlock(sock.cond)
    end
    nothing
end

function trylisten(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    iolock_begin()
    check_open(sock)
    err = ccall(:uv_listen, Cint, (Ptr{Cvoid}, Cint, Ptr{Cvoid}),
                sock, backlog, uv_jl_connectioncb::Ptr{Cvoid})
    sock.status = StatusActive
    iolock_end()
    return err
end

##

function accept_nonblock(server::TCPServer, client::TCPSocket)
    iolock_begin()
    if client.status != StatusInit
        error("client TCPSocket is not in initialization state")
    end
    err = ccall(:uv_accept, Int32, (Ptr{Cvoid}, Ptr{Cvoid}), server.handle, client.handle)
    if err == 0
        client.status = StatusOpen
    end
    iolock_end()
    return err
end

function accept_nonblock(server::TCPServer)
    client = TCPSocket()
    uv_error("accept", accept_nonblock(server, client))
    return client
end

function accept(server::LibuvServer, client::LibuvStream)
    iolock_begin()
    if server.status != StatusActive && server.status != StatusClosing && server.status != StatusClosed
        throw(ArgumentError("server not connected, make sure \"listen\" has been called"))
    end
    while isopen(server)
        err = accept_nonblock(server, client)
        if err == 0
            iolock_end()
            return client
        elseif err != UV_EAGAIN
            uv_error("accept", err)
        end
        preserve_handle(server)
        lock(server.cond)
        iolock_end()
        try
            wait(server.cond)
        finally
            unlock(server.cond)
            unpreserve_handle(server)
        end
        iolock_begin()
    end
    uv_error("accept", UV_ECONNABORTED)
    nothing
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

function udp_set_membership(sock::UDPSocket, group_addr::String,
                            interface_addr::Union{Nothing, String}, operation)
    if interface_addr === nothing
        interface_addr = C_NULL
    end
    r = ccall(:uv_udp_set_membership, Cint,
              (Ptr{Cvoid}, Cstring, Cstring, Cint),
              sock.handle, group_addr, interface_addr, operation)
    uv_error("uv_udp_set_membership", r)
    return
end

"""
    join_multicast_group(sock::UDPSocket, group_addr, interface_addr = nothing)

Join a socket to a particular multicast group defined by `group_addr`.
If `interface_addr` is given, specifies a particular interface for multi-homed
systems.  Use `leave_multicast_group()` to disable reception of a group.
"""
function join_multicast_group(sock::UDPSocket, group_addr::String,
                              interface_addr::Union{Nothing, String} = nothing)
    return udp_set_membership(sock, group_addr, interface_addr, 1)
end
function join_multicast_group(sock::UDPSocket, group_addr::IPAddr,
                              interface_addr::Union{Nothing, IPAddr} = nothing)
    if interface_addr !== nothing
        interface_addr = string(interface_addr)
    end
    return join_multicast_group(sock, string(group_addr), interface_addr)
end

"""
    leave_multicast_group(sock::UDPSocket, group_addr, interface_addr = nothing)

Remove a socket from  a particular multicast group defined by `group_addr`.
If `interface_addr` is given, specifies a particular interface for multi-homed
systems.  Use `join_multicast_group()` to enable reception of a group.
"""
function leave_multicast_group(sock::UDPSocket, group_addr::String,
                               interface_addr::Union{Nothing, String} = nothing)
    return udp_set_membership(sock, group_addr, interface_addr, 0)
end
function leave_multicast_group(sock::UDPSocket, group_addr::IPAddr,
                               interface_addr::Union{Nothing, IPAddr} = nothing)
    if interface_addr !== nothing
        interface_addr = string(interface_addr)
    end
    return leave_multicast_group(sock, string(group_addr), interface_addr)
end

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

    iolock_begin()
    if self
        r = ccall(:jl_tcp_getsockname, Int32,
                (Ptr{Cvoid}, Ref{Cushort}, Ptr{Cvoid}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    else
        r = ccall(:jl_tcp_getpeername, Int32,
                (Ptr{Cvoid}, Ref{Cushort}, Ptr{Cvoid}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    end
    iolock_end()
    uv_error("cannot obtain socket name", r)
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
        error(string("unsupported address family: ", rfamily[]))
    end
    return addr, port
end

# domain sockets

include("PipeServer.jl")

# libuv callback handles

function __init__()
    global uv_jl_getaddrinfocb = @cfunction(uv_getaddrinfocb, Cvoid, (Ptr{Cvoid}, Cint, Ptr{Cvoid}))
    global uv_jl_getnameinfocb = @cfunction(uv_getnameinfocb, Cvoid, (Ptr{Cvoid}, Cint, Cstring, Cstring))
    global uv_jl_recvcb        = @cfunction(uv_recvcb, Cvoid, (Ptr{Cvoid}, Cssize_t, Ptr{Cvoid}, Ptr{Cvoid}, Cuint))
    global uv_jl_connectioncb  = @cfunction(uv_connectioncb, Cvoid, (Ptr{Cvoid}, Cint))
    global uv_jl_connectcb     = @cfunction(uv_connectcb, Cvoid, (Ptr{Cvoid}, Cint))
end

end
