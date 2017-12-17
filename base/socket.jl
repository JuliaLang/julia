# This file is a part of Julia. License is MIT: https://julialang.org/license

## IP ADDRESS HANDLING ##
abstract type IPAddr end

Base.isless(a::T, b::T) where {T<:IPAddr} = isless(a.host, b.host)
Base.convert(dt::Type{<:Integer}, ip::IPAddr) = dt(ip.host)

struct IPv4 <: IPAddr
    host::UInt32
    IPv4(host::UInt32) = new(host)
    IPv4(a::UInt8,b::UInt8,c::UInt8,d::UInt8) = new(UInt32(a)<<24|
                                                    UInt32(b)<<16|
                                                    UInt32(c)<<8|
                                                    d)
    function IPv4(a::Integer,b::Integer,c::Integer,d::Integer)
        if !(0<=a<=255 && 0<=b<=255 && 0<=c<=255 && 0<=d<=255)
            throw(ArgumentError("IPv4 field out of range (must be 0-255)"))
        end
        IPv4(UInt8(a),UInt8(b),UInt8(c),UInt8(d))
    end
end

"""
    IPv4(host::Integer) -> IPv4

Returns an IPv4 object from ip address `host` formatted as an [`Integer`](@ref).

```jldoctest
julia> IPv4(3223256218)
ip"192.30.252.154"
```
"""
function IPv4(host::Integer)
    if host < 0
        throw(ArgumentError("IPv4 address must be positive"))
    elseif typemax(typeof(host)) > typemax(UInt32) && host > typemax(UInt32)
        throw(ArgumentError("IPv4 address must fit within 32 bits"))
    else
        return IPv4(UInt32(host))
    end
end

# constructor: ("1.2.3.4")
IPv4(str::AbstractString) = parse(IPv4, str)

show(io::IO,ip::IPv4) = print(io,"ip\"",ip,"\"")
print(io::IO,ip::IPv4) = print(io,dec((ip.host&(0xFF000000))>>24),".",
                                  dec((ip.host&(0xFF0000))>>16),".",
                                  dec((ip.host&(0xFF00))>>8),".",
                                  dec(ip.host&0xFF))

struct IPv6 <: IPAddr
    host::UInt128
    IPv6(host::UInt128) = new(host)
    IPv6(a::UInt16,b::UInt16,c::UInt16,d::UInt16,
     e::UInt16,f::UInt16,g::UInt16,h::UInt16) = new(UInt128(a)<<(7*16)|
                            UInt128(b)<<(6*16)|
                            UInt128(c)<<(5*16)|
                            UInt128(d)<<(4*16)|
                            UInt128(e)<<(3*16)|
                            UInt128(f)<<(2*16)|
                            UInt128(g)<<(1*16)|
                            h)
    function IPv6(a::Integer,b::Integer,c::Integer,d::Integer,
          e::Integer,f::Integer,g::Integer,h::Integer)
        if !(0<=a<=0xFFFF && 0<=b<=0xFFFF && 0<=c<=0xFFFF && 0<=d<=0xFFFF &&
             0<=e<=0xFFFF && 0<=f<=0xFFFF && 0<=g<=0xFFFF && 0<=h<=0xFFFF)
            throw(ArgumentError("IPv6 field out of range (must be 0-65535)"))
        end
        IPv6(UInt16(a),UInt16(b),UInt16(c),UInt16(d),
             UInt16(e),UInt16(f),UInt16(g),UInt16(h))
    end
end

"""
    IPv6(host::Integer) -> IPv6

Returns an IPv6 object from ip address `host` formatted as an [`Integer`](@ref).

```jldoctest
julia> IPv6(3223256218)
ip"::c01e:fc9a"
```
"""
function IPv6(host::Integer)
    if host < 0
        throw(ArgumentError("IPv6 address must be positive"))
        # We allow passing bigger integer types, but need to be careful to avoid overflow
        # Let's hope promotion rules are sensible
    elseif typemax(typeof(host)) > typemax(UInt128) && host > typemax(UInt128)
        throw(ArgumentError("IPv6 address must fit within 128 bits"))
    else
        return IPv6(UInt128(host))
    end
end

IPv6(str::AbstractString) = parse(IPv6, str)

# Suppress leading '0's and "0x"
print_ipv6_field(io,field::UInt16) = print(io,hex(field))

print_ipv6_field(io,ip,i) = print_ipv6_field(io,ipv6_field(ip,i))
function ipv6_field(ip::IPv6,i)
    if i < 0 || i > 7
        throw(BoundsError())
    end
    UInt16(ip.host&(UInt128(0xFFFF)<<(i*16))>>(i*16))
end

show(io::IO, ip::IPv6) = print(io,"ip\"",ip,"\"")
# RFC 5952 compliant show function
# http://tools.ietf.org/html/rfc5952
function print(io::IO,ip::IPv6)
    i = 8
    m = 0
    longest_sub_i = -1
    while i!=0
        i-=1
        field = ipv6_field(ip,i)
        if field == 0 && longest_sub_i == -1
            # Find longest subsequence of 0
            longest_sub_i,j,m,c = i,i,1,1
            while j != 0
                j-=1
                if ipv6_field(ip,j) == 0
                    c += 1
                else
                    c = 0
                end
                if c > m
                    if j+c != longest_sub_i+1
                        longest_sub_i = j+c-1
                    end
                    m = c
                end
            end
            # Prevent single 0 from contracting to :: as required
            if m == 1
                longest_sub_i = 9
            end
        end
        if i == longest_sub_i
            print(io,":")
            i -= m-1
            if i == 0
                print(io,":")
                break
            end
        else
            if i != 7
                print(io,":")
            end
            print_ipv6_field(io,field)
        end
    end
end

# Parsing

const ipv4_leading_zero_error = """
Leading zeros in IPv4 addresses are disallowed due to ambiguity.
If the address is in octal or hexadecimal, convert it to decimal, otherwise remove the leading zero.
"""

function parse(::Type{IPv4}, str::AbstractString)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields
        if isempty(f)
            throw(ArgumentError("empty field in IPv4 address"))
        end
        if length(f) > 1 && f[1] == '0'
            throw(ArgumentError(ipv4_leading_zero_error))
        else
            r = parse(Int,f,10)
        end
        if i != length(fields)
            if r < 0 || r > 255
                throw(ArgumentError("IPv4 field out of range (must be 0-255)"))
            end
            ret |= UInt32(r) << ((4-i)*8)
        else
            if r > ((UInt64(1)<<((5-length(fields))*8))-1)
                throw(ArgumentError("IPv4 field too large"))
            end
            ret |= r
        end
        i+=1
    end
    IPv4(ret)
end

function parseipv6fields(fields,num_fields)
    if length(fields) > num_fields
        throw(ArgumentError("too many fields in IPv6 address"))
    end
    cf = 7
    ret = UInt128(0)
    for f in fields
        if isempty(f)
            # ::abc:... and ..:abc::
            if cf != 7 && cf != 0
                cf -= num_fields-length(fields)
            end
            cf -= 1
            continue
        end
        ret |= UInt128(parse(Int,f,16))<<(cf*16)
        cf -= 1
    end
    ret
end
parseipv6fields(fields) = parseipv6fields(fields,8)

function parse(::Type{IPv6}, str::AbstractString)
    fields = split(str,':')
    if length(fields) > 8
        throw(ArgumentError("too many fields in IPv6 address"))
    elseif length(fields) == 8
        return IPv6(parseipv6fields(fields))
    elseif in('.',fields[end])
        return IPv6((parseipv6fields(fields[1:(end-1)],6))
            | parse(IPv4, fields[end]).host )
    else
        return IPv6(parseipv6fields(fields))
    end
end

#
# This support IPv4 addresses in the common dot (IPv4) or colon (IPv6)
# separated formats. Most other common formats use a standard integer encoding
# of the appropriate size and should use the appropriate constructor
#

function parse(::Type{IPAddr}, str::AbstractString)
    if ':' in str
        return parse(IPv6, str)
    else
        return parse(IPv4, str)
    end
end

macro ip_str(str)
    return parse(IPAddr, str)
end

struct InetAddr{T<:IPAddr}
    host::T
    port::UInt16
end

InetAddr(ip::IPAddr, port) = InetAddr{typeof(ip)}(ip, port)

## SOCKETS ##

mutable struct TCPSocket <: LibuvStream
    handle::Ptr{Void}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    connectnotify::Condition
    closenotify::Condition
    sendbuf::Union{IOBuffer, Void}
    lock::ReentrantLock
    throttle::Int

    function TCPSocket(handle::Ptr{Void}, status)
        tcp = new(
                handle,
                status,
                PipeBuffer(),
                Condition(),
                Condition(),
                Condition(),
                nothing,
                ReentrantLock(),
                DEFAULT_READ_BUFFER_SZ)
        associate_julia_struct(tcp.handle, tcp)
        finalizer(uvfinalize, tcp)
        return tcp
    end
end

# kw arg "delay": if true, libuv delays creation of the socket fd till the first bind call
function TCPSocket(; delay=true)
    tcp = TCPSocket(Libc.malloc(_sizeof_uv_tcp), StatusUninit)
    af_spec = delay ? 0 : 2   # AF_UNSPEC is 0, AF_INET is 2

    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Void}, Ptr{Void}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp socket", err)
    tcp.status = StatusInit
    return tcp
end

mutable struct TCPServer <: LibuvServer
    handle::Ptr{Void}
    status::Int
    connectnotify::Condition
    closenotify::Condition

    function TCPServer(handle::Ptr{Void}, status)
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
    tcp = TCPServer(Libc.malloc(_sizeof_uv_tcp), StatusUninit)
    af_spec = delay ? 0 : 2   # AF_UNSPEC is 0, AF_INET is 2
    err = ccall(:uv_tcp_init_ex, Cint, (Ptr{Void}, Ptr{Void}, Cuint),
                eventloop(), tcp.handle, af_spec)
    uv_error("failed to create tcp server", err)
    tcp.status = StatusInit
    return tcp
end

isreadable(io::TCPSocket) = isopen(io) || nb_available(io) > 0
iswritable(io::TCPSocket) = isopen(io) && io.status != StatusClosing

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_connect_raw(sock::TCPSocket, sockaddr::Ptr{Void}) =
    ccall(:jl_connect_raw, Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}), sock.handle, sockaddr, uv_jl_connectcb::Ptr{Void})
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) =
    ccall(:jl_sockaddr_from_addrinfo, Ptr{Void}, (Ptr{Void},), addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void}, port::UInt16) =
    ccall(:jl_sockaddr_set_port, Void, (Ptr{Void}, UInt16), ptr, port)

"""
    accept(server[,client])

Accepts a connection on the given server and returns a connection to the client. An
uninitialized client stream may be provided, in which case it will be used instead of
creating a new stream.
"""
accept(server::TCPServer) = accept(server, TCPSocket())

# Libuv will internally reset the readable and writable flags on
# this pipe after it has successfully accepted the connection, to
# remember that before that this is an invalid pipe
accept(server::PipeServer) = accept(server, init_pipe!(PipeEndpoint();
    readable=false, writable=false, julia_only=true))

# UDP

mutable struct UDPSocket <: LibuvStream
    handle::Ptr{Void}
    status::Int
    recvnotify::Condition
    sendnotify::Condition
    closenotify::Condition

    function UDPSocket(handle::Ptr{Void}, status)
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
    this = UDPSocket(Libc.malloc(_sizeof_uv_udp), StatusUninit)
    err = ccall(:uv_udp_init, Cint, (Ptr{Void}, Ptr{Void}),
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

_bind(sock::TCPServer, host::IPv4, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, UInt16, UInt32, Cuint),
            sock.handle, hton(port), hton(host.host), flags)

_bind(sock::TCPServer, host::IPv6, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_tcp_bind6, Int32, (Ptr{Void}, UInt16, Ptr{UInt128}, Cuint),
            sock.handle, hton(port), Ref(hton(host.host)), flags)

_bind(sock::UDPSocket, host::IPv4, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_udp_bind, Int32, (Ptr{Void}, UInt16, UInt32, UInt32),
            sock.handle, hton(port), hton(host.host), flags)

_bind(sock::UDPSocket, host::IPv6, port::UInt16, flags::UInt32 = UInt32(0)) = ccall(:jl_udp_bind6, Int32, (Ptr{Void}, UInt16, Ptr{UInt128}, UInt32),
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
    if ccall(:uv_is_active, Cint, (Ptr{Void},), sock.handle) == 0
        uv_error("recv_start", ccall(:uv_udp_recv_start, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                                    sock.handle, uv_jl_alloc_buf::Ptr{Void}, uv_jl_recvcb::Ptr{Void}))
    end
    sock.status = StatusActive
    return stream_wait(sock, sock.recvnotify)::Tuple{Union{IPv4, IPv6}, Vector{UInt8}}
end

alloc_buf_hook(sock::UDPSocket, size::UInt) = (Libc.malloc(size), size)

function uv_recvcb(handle::Ptr{Void}, nread::Cssize_t, buf::Ptr{Void}, addr::Ptr{Void}, flags::Cuint)
    # C signature documented as (*uv_udp_recv_cb)(...)
    sock = @handle_as handle UDPSocket
    if nread < 0
        Libc.free(buf_addr)
        notify_error(sock.recvnotify, UVError("recv", nread))
    elseif flags & UV_UDP_PARTIAL > 0
        Libc.free(buf_addr)
        notify_error(sock.recvnotify, "Partial message received")
    else
        buf_addr = ccall(:jl_uv_buf_base, Ptr{Void}, (Ptr{Void},), buf)
        buf_size = ccall(:jl_uv_buf_len, Csize_t, (Ptr{Void},), buf)
        # need to check the address type in order to convert to a Julia IPAddr
        addrout = if addr == C_NULL
                      IPv4(0)
                  elseif ccall(:jl_sockaddr_in_is_ip4, Cint, (Ptr{Void},), addr) == 1
                      IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Void},), addr)))
                  else
                      tmp = [UInt128(0)]
                      ccall(:jl_sockaddr_host6, UInt32, (Ptr{Void}, Ptr{UInt8}), addr, pointer(tmp))
                      IPv6(ntoh(tmp[1]))
                  end
        buf = unsafe_wrap(Array, convert(Ptr{UInt8}, buf_addr), Int(nread), true)
        notify(sock.recvnotify, (addrout, buf))
    end
    ccall(:uv_udp_recv_stop, Cint, (Ptr{Void},), sock.handle)
    sock.status = StatusOpen
    nothing
end

function _send(sock::UDPSocket, ipaddr::IPv4, port::UInt16, buf)
    ccall(:jl_udp_send, Cint, (Ptr{Void}, UInt16, UInt32, Ptr{UInt8}, Csize_t, Ptr{Void}),
          sock.handle, hton(port), hton(ipaddr.host), buf, sizeof(buf), uv_jl_sendcb::Ptr{Void})
end

function _send(sock::UDPSocket, ipaddr::IPv6, port::UInt16, buf)
    ccall(:jl_udp_send6, Cint, (Ptr{Void}, UInt16, Ref{UInt128}, Ptr{UInt8}, Csize_t, Ptr{Void}),
          sock.handle, hton(port), hton(ipaddr.host), buf, sizeof(buf), uv_jl_sendcb::Ptr{Void})
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

function uv_sendcb(handle::Ptr{Void}, status::Cint)
    sock = @handle_as handle UDPSocket
    if status < 0
        notify_error(sock.sendnotify, UVError("UDP send failed", status))
    end
    notify(sock.sendnotify)
    Libc.free(handle)
    nothing
end

##

struct DNSError <: Exception
    host::String
    code::Int32
end

function show(io::IO, err::DNSError)
    print(io, "DNSError: ", err.host, ", ", struverror(err.code),
                                      " (", uverrorname(err.code), ")")
end

function uv_getaddrinfocb(req::Ptr{Void}, status::Cint, addrinfo::Ptr{Void})
    data = uv_req_data(req)
    if data != C_NULL
        t = unsafe_pointer_to_objref(data)::Task
        uv_req_set_data(req, C_NULL)
        if status != 0 || addrinfo == C_NULL
            schedule(t, UVError("getaddrinfocb", status))
        else
            freeaddrinfo = addrinfo
            addrs = IPAddr[]
            while addrinfo != C_NULL
                sockaddr = ccall(:jl_sockaddr_from_addrinfo, Ptr{Void}, (Ptr{Void},), addrinfo)
                if ccall(:jl_sockaddr_is_ip4, Int32, (Ptr{Void},), sockaddr) == 1
                    ip4addr = ccall(:jl_sockaddr_host4, UInt32, (Ptr{Void},), sockaddr)
                    push!(addrs, IPv4(ntoh(ip4addr)))
                elseif ccall(:jl_sockaddr_is_ip6, Int32, (Ptr{Void},), sockaddr) == 1
                    ip6addr = Ref{UInt128}()
                    scope_id = ccall(:jl_sockaddr_host6, UInt32, (Ptr{Void}, Ptr{UInt128}), sockaddr, ip6addr)
                    push!(addrs, IPv6(ntoh(ip6addr[])))
                end
                addrinfo = ccall(:jl_next_from_addrinfo, Ptr{Void}, (Ptr{Void},), addrinfo)
            end
            ccall(:uv_freeaddrinfo, Void, (Ptr{Void},), freeaddrinfo)
            schedule(t, addrs)
        end
    else
        # no owner for this req, safe to just free it
        Libc.free(req)
    end
    nothing
end

"""
    getalladdrinfo(host::AbstractString) -> Vector{IPAddr}

Gets all of the IP addresses of the `host`.
Uses the operating system's underlying getaddrinfo implementation, which may do a DNS lookup.
"""
function getalladdrinfo(host::String)
    isascii(host) || error("non-ASCII hostname: $host")
    req = Libc.malloc(_sizeof_uv_getaddrinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    status = ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Ptr{Void}, Cstring, Ptr{Void}, Ptr{Void}),
                   eventloop(), req, host, #=service=#C_NULL, uv_jl_getaddrinfocb::Ptr{Void})
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getaddrinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getaddrinfo", status)
    end
    ct = current_task()
    preserve_handle(ct)
    r = try
        uv_req_set_data(req, ct)
        wait()
    finally
        if uv_req_data(req) != C_NULL
            # req is still alive,
            # so make sure we don't get spurious notifications later
            uv_req_set_data(req, C_NULL)
            ccall(:uv_cancel, Int32, (Ptr{Void},), req) # try to let libuv know we don't care anymore
        else
            # done with req
            Libc.free(req)
        end
        unpreserve_handle(ct)
    end
    if isa(r, UVError)
        code = r.code
        if code in (UV_EAI_ADDRFAMILY, UV_EAI_AGAIN, UV_EAI_BADFLAGS,
                    UV_EAI_BADHINTS, UV_EAI_CANCELED, UV_EAI_FAIL,
                    UV_EAI_FAMILY, UV_EAI_NODATA, UV_EAI_NONAME,
                    UV_EAI_OVERFLOW, UV_EAI_PROTOCOL, UV_EAI_SERVICE,
                    UV_EAI_SOCKTYPE)
            throw(DNSError(host, code))
        elseif code == UV_EAI_MEMORY
            throw(OutOfMemoryError())
        else
            throw(UVError("getaddrinfo", code))
        end
    end
    return r::Vector{IPAddr}
end
getalladdrinfo(host::AbstractString) = getalladdrinfo(String(host))

"""
    getalladdrinfo(host::AbstractString, IPAddr=IPv4) -> IPAddr

Gets the first IP address of the `host` of the specified IPAddr type.
Uses the operating system's underlying getaddrinfo implementation, which may do a DNS lookup.
"""
function getaddrinfo(host::String, T::Type{<:IPAddr})
    addrs = getalladdrinfo(host)
    for addr in addrs
        if addr isa T
            return addr
        end
    end
    throw(DNSError(host, UV_EAI_NONAME))
end
getaddrinfo(host::AbstractString, T::Type{<:IPAddr}) = getaddrinfo(String(host), T)
getaddrinfo(host::AbstractString) = getaddrinfo(String(host), IPv4)

function uv_getnameinfocb(req::Ptr{Void}, status::Cint, hostname::Cstring, service::Cstring)
    data = uv_req_data(req)
    if data != C_NULL
        t = unsafe_pointer_to_objref(data)::Task
        uv_req_set_data(req, C_NULL)
        if status != 0
            schedule(t, UVError("getnameinfocb", status))
        else
            schedule(t, unsafe_string(hostname))
        end
    else
        # no owner for this req, safe to just free it
        Libc.free(req)
    end
    nothing
end

"""
    getnameinfo(host::IPAddr) -> String

Performs a reverse-lookup for IP address to return a hostname and service
using the operating system's underlying getnameinfo implementation.
"""
function getnameinfo(address::Union{IPv4, IPv6})
    req = Libc.malloc(_sizeof_uv_getnameinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    ev = eventloop()
    port = hton(UInt16(0))
    flags = 0
    uvcb = uv_jl_getnameinfocb::Ptr{Void}
    status = UV_EINVAL
    if address isa IPv4
        status = ccall(:jl_getnameinfo, Int32, (Ptr{Void}, Ptr{Void}, UInt32, UInt16, Cint, Ptr{Void}),
                       ev, req, hton(address.host), port, flags, uvcb)
    elseif address isa IPv6
        status = ccall(:jl_getnameinfo6, Int32, (Ptr{Void}, Ptr{Void}, Ref{UInt128}, UInt16, Cint, Ptr{Void}),
                       ev, req, hton(address.host), port, flags, uvcb)
    end
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getnameinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getnameinfo", status)
    end
    ct = current_task()
    preserve_handle(ct)
    r = try
        uv_req_set_data(req, ct)
        wait()
    finally
        if uv_req_data(req) != C_NULL
            # req is still alive,
            # so make sure we don't get spurious notifications later
            uv_req_set_data(req, C_NULL)
            ccall(:uv_cancel, Int32, (Ptr{Void},), req) # try to let libuv know we don't care anymore
        else
            # done with req
            Libc.free(req)
        end
        unpreserve_handle(ct)
    end
    if isa(r, UVError)
        code = r.code
        if code in (UV_EAI_ADDRFAMILY, UV_EAI_AGAIN, UV_EAI_BADFLAGS,
                    UV_EAI_BADHINTS, UV_EAI_CANCELED, UV_EAI_FAIL,
                    UV_EAI_FAMILY, UV_EAI_NODATA, UV_EAI_NONAME,
                    UV_EAI_OVERFLOW, UV_EAI_PROTOCOL, UV_EAI_SERVICE,
                    UV_EAI_SOCKTYPE)
            throw(DNSError(repr(address), code))
        elseif code == UV_EAI_MEMORY
            throw(OutOfMemoryError())
        else
            throw(UVError("getnameinfo", code))
        end
    end
    return r::String
end


const _sizeof_uv_interface_address = ccall(:jl_uv_sizeof_interface_address,Int32,())

"""
    getipaddr() -> IPAddr

Get the IP address of the local machine.
"""
function getipaddr()
    addr_ref = Ref{Ptr{UInt8}}(C_NULL)
    count_ref = Ref{Int32}(1)
    lo_present = false
    err = ccall(:jl_uv_interface_addresses, Int32, (Ref{Ptr{UInt8}}, Ref{Int32}), addr_ref, count_ref)
    uv_error("getlocalip", err)
    addr, count = addr_ref[], count_ref[]
    for i = 0:(count-1)
        current_addr = addr + i*_sizeof_uv_interface_address
        if 1 == ccall(:jl_uv_interface_address_is_internal, Int32, (Ptr{UInt8},), current_addr)
            lo_present = true
            continue
        end
        sockaddr = ccall(:jl_uv_interface_address_sockaddr, Ptr{Void}, (Ptr{UInt8},), current_addr)
        if ccall(:jl_sockaddr_in_is_ip4, Int32, (Ptr{Void},), sockaddr) == 1
            rv = IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Void},), sockaddr)))
            ccall(:uv_free_interface_addresses, Void, (Ptr{UInt8}, Int32), addr, count)
            return rv
        # Uncomment to enbable IPv6
        #elseif ccall(:jl_sockaddr_in_is_ip6, Int32, (Ptr{Void},), sockaddr) == 1
        #   host = Vector{UInt128}(uninitialized, 1)
        #   ccall(:jl_sockaddr_host6, UInt32, (Ptr{Void}, Ptr{UInt128}), sockaddrr, host)
        #   return IPv6(ntoh(host[1]))
        end
    end
    ccall(:uv_free_interface_addresses, Void, (Ptr{UInt8}, Int32), addr, count)
    return lo_present ? localhost : error("No networking interface available")
end

##

function connect!(sock::TCPSocket, host::IPv4, port::Integer)
    if sock.status != StatusInit
        error("TCPSocket is not in initialization state")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    uv_error("connect", ccall(:jl_tcp4_connect, Int32, (Ptr{Void}, UInt32, UInt16, Ptr{Void}),
                             sock.handle, hton(host.host), hton(UInt16(port)), uv_jl_connectcb::Ptr{Void}))
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
    uv_error("connect", ccall(:jl_tcp6_connect, Int32, (Ptr{Void}, Ref{UInt128}, UInt16, Ptr{Void}),
                              sock.handle, hton(host.host), hton(UInt16(port)), uv_jl_connectcb::Ptr{Void}))
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

default_connectcb(sock, status) = nothing

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

##

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

##

function accept_nonblock(server::TCPServer, client::TCPSocket)
    if client.status != StatusInit
        error("client TCPSocket is not in initialization state")
    end
    err = ccall(:uv_accept, Int32, (Ptr{Void}, Ptr{Void}), server.handle, client.handle)
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
                (Ptr{Void}, Ref{Cushort}, Ptr{Void}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    else
        r = ccall(:jl_tcp_getpeername, Int32,
                (Ptr{Void}, Ref{Cushort}, Ptr{Void}, Ref{Cuint}),
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
