## IP ADDRESS HANDLING ##
abstract IpAddr

immutable IPv4 <: IpAddr
    host::Uint32
    IPv4(host::Uint32) = new(host)
    IPv4(a::Uint8,b::Uint8,c::Uint8,d::Uint8) = new(uint32(a)<<24|
                                                    uint32(b)<<16|
                                                    uint32(c)<<8|
                                                    d)
    function IPv4(a::Integer,b::Integer,c::Integer,d::Integer)
        if !(0<=a<=255 && 0<=b<=255 && 0<=c<=255 && 0<=d<=255)
            throw(DomainError())
        end
        IPv4(uint8(a),uint8(b),uint8(c),uint8(d))
    end
end

function IPv4(host::Integer)
    if host < 0
        error("IP address must not be negative")
    elseif typemax(typeof(host)) > typemax(Uint32) && host > typemax(Uint32) 
        error("IPv4 address must fit within 32 bits")
    else
        return IPv4(uint32(host))
    end
end

show(io::IO,ip::IPv4) = print(io,"ip\"",ip,"\"")
print(io::IO,ip::IPv4) = print(io,dec((ip.host&(0xFF000000))>>24),".",
                                  dec((ip.host&(0xFF0000))>>16),".",
                                  dec((ip.host&(0xFF00))>>8),".",
                                  dec(ip.host&0xFF))

immutable IPv6 <: IpAddr
    host::Uint128
    IPv6(host::Uint128) = new(host)
    IPv6(a::Uint16,b::Uint16,c::Uint16,d::Uint16,
     e::Uint16,f::Uint16,g::Uint16,h::Uint16) = new(uint128(a)<<(7*16)|
                            uint128(b)<<(6*16)|
                            uint128(c)<<(5*16)|
                            uint128(d)<<(4*16)|
                            uint128(e)<<(3*16)|
                            uint128(f)<<(2*16)|
                            uint128(g)<<(1*16)|
                            h)
    function IPv6(a::Integer,b::Integer,c::Integer,d::Integer,
          e::Integer,f::Integer,g::Integer,h::Integer)
    if !(0<=a<=0xFFFF && 0<=b<=0xFFFF && 0<=c<=0xFFFF && 0<=d<=0xFFFF &&
         0<=e<=0xFFFF && 0<=f<=0xFFFF && 0<=g<=0xFFFF && 0<=h<=0xFFFF)
        throw(DomainError())
    end
    IPv6(uint16(a),uint16(b),uint16(c),uint16(d),
         uint16(e),uint16(f),uint16(g),uint16(h))
    end
end

function IPv6(host::Integer)
    if host < 0
        error("IP address must not be negative")
        # We allow passing bigger integer types, but need to be careful to avoid overflow
        # Let's hope promotion rules are sensible
    elseif typemax(typeof(host)) > typemax(Uint128) && host > typemax(Uint128) 
        error("IPv6 address must fit within 128 bits")
    else
        return IPv6(uint128(host))
    end
end

# Suppress leading '0's and "0x"
print_ipv6_field(io,field::Uint16) = print(io,hex(field))

print_ipv6_field(io,ip,i) = print_ipv6_field(io,ipv6_field(ip,i))
function ipv6_field(ip::IPv6,i) 
    if i < 0 || i > 7
        throw(BoundsError())
    end
    uint16(ip.host&(uint128(0xFFFF)<<(i*16))>>(i*16))
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

function parseipv4(str)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields 
        if length(f) == 0
            error("empty field in IPv4 address")
        end
        if f[1] == '0'
            if length(f) >= 2 && f[2] == 'x'
                if length(f) > 8 # 2+(3*2) - prevent parseint from overflowing on 32bit
                    error("IPv4 field too large")
                end
                r = parseint(f[3:end],16)
            else 
                if length(f) > 9 # 1+8 - prevent parseint from overflowing on 32bit
                    error("IPv4 field too large")
                end
                r = parseint(f,8)
            end
        else
            r = parseint(f,10)
        end
        if i != length(fields)
            if r < 0 || r > 255
                error("IPv4 field out of range (must be 0-255)")
            end
            ret |= uint32(r) << ((4-i)*8)
        else
            if r > ((uint64(1)<<((5-length(fields))*8))-1)
                error("IPv4 field too large")
            end
            ret |= r
        end
        i+=1
    end
    IPv4(ret)
end

function parseipv6fields(fields,num_fields)
    if length(fields) > num_fields
        error("too many fields in IPv6 address")
    end
    cf = 7
    ret = uint128(0)
    for f in fields 
        if f == ""
            # ::abc:... and ..:abc::
            if cf != 7 && cf != 0
                cf -= num_fields-length(fields)               
            end
            cf -= 1
            continue
        end
        ret |= uint128(parseint(f,16))<<(cf*16)
        cf -= 1
    end
    ret
end
parseipv6fields(fields) = parseipv6fields(fields,8)

function parseipv6(str)
    fields = split(str,':')
    if length(fields) > 8
        error("too many fields in IPv6 address")
    elseif length(fields) == 8
        return IPv6(parseipv6fields(fields))
    elseif in('.',fields[end])
        return IPv6((parseipv6fields(fields[1:(end-1)],6))
            | parseipv4(fields[end]).host )
    else
        return IPv6(parseipv6fields(fields))
    end
end

# 
# This support IPv4 addresses in the common dot (IPv4) or colon (IPv6)
# separated formats. Most other common formats use a standard integer encoding
# of the appropriate size and should use the appropriate constructor
#

function parseip(str)
    if in(':',str)
        # IPv6 Address
        return parseipv6(str)
    else
        # IPv4 Address
        return parseipv4(str)
    end
end

macro ip_str(str)
    return parseip(str)
end

type InetAddr
    host::IpAddr
    port::Uint16
    function InetAddr(host,port)
        if !(0 <= port <= typemax(Uint16))
            throw(DomainError())
        end
        new(host,uint16(port))
    end
end


## SOCKETS ##

abstract Socket <: AsyncStream

type TcpSocket <: Socket
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
    TcpSocket(handle) = new(
        handle,
        StatusUninit,
        true,
        PipeBuffer(),
        false,Condition(),
        false,Condition(),
        false,Condition())
end
function TcpSocket()
    this = TcpSocket(c_malloc(_sizeof_uv_tcp))
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

type TcpServer <: UVServer
    handle::Ptr{Void}
    status::Int
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    TcpServer(handle) = new(
        handle,
        StatusUninit,
        false,Condition(),
        false,Condition())
end
function TcpServer()
    this = TcpServer(c_malloc(_sizeof_uv_tcp))
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

# Internal version of close that doesn't error when called on an unitialized socket, as well as disassociating the socket immidiately
# This is fine because if we're calling this from a finalizer, nobody can be possibly waiting for the close to go through
function uvfinalize(uv)
    close(uv)
    disassociate_julia_struct(uv)
    uv.handle = 0
end

function uvfinalize(uv::Union(TTY,Pipe,TcpServer,TcpSocket))
    if (uv.status != StatusUninit && uv.status != StatusInit)
        close(uv)
    end
    disassociate_julia_struct(uv)
    uv.handle = 0
end

isreadable(io::TcpSocket) = true
iswritable(io::TcpSocket) = true

show(io::IO,sock::TcpSocket) = print(io,"TcpSocket(",uv_status_string(sock),", ",
    nb_available(sock.buffer)," bytes waiting)")

show(io::IO,sock::TcpServer) = print(io,"TcpServer(",uv_status_string(sock),")")

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void}) = 
    ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void}),sock.handle,sockaddr)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) = 
    ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = 
    ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)

accept(server::TcpServer) = accept(server, TcpSocket())
accept(server::PipeServer) = accept(server, Pipe())

##

bind(sock::TcpServer, addr::InetAddr) = bind(sock,addr.host,addr.port)

_bind(sock::TcpServer, host::IPv4, port::Uint16) = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, Uint16, Uint32, Cuint),
            sock.handle, hton(port), hton(host.host), 0)

_bind(sock::TcpServer, host::IPv6, port::Uint16) = ccall(:jl_tcp_bind6, Int32, (Ptr{Void}, Uint16, Ptr{Uint128}, Cuint),
            sock.handle, hton(port), &hton(host.host), 0)

# UDP 

type UdpSocket <: Socket
    handle::Ptr{Void}
    status::Int
    recvnotify::Condition
    sendnotify::Condition
    closenotify::Condition
    UdpSocket(handle::Ptr) = new(handle, StatusUninit, Condition(), Condition(), Condition())
end

function UdpSocket()
    this = UdpSocket(c_malloc(_sizeof_uv_udp))
    associate_julia_struct(this.handle, this)
    err = ccall(:uv_udp_init,Cint,(Ptr{Void},Ptr{Void}),
                  eventloop(),this.handle)
    if err != 0 
        c_free(this.handle)
        this.handle = C_NULL
        error(UVError("failed to create udp socket",err))
    end
    this.status = StatusInit
    this
end

function _uv_hook_close(sock::UdpSocket)
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

function bind(sock::Union(TcpServer,UdpSocket), host::IPv4, port::Integer)
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

_bind(sock::UdpSocket, host::IPv4, port::Uint16) = ccall(:jl_udp_bind, Int32, (Ptr{Void}, Uint16, Uint32, Uint32),
            sock.handle, hton(port), hton(host.host), 0)

_bind(sock::UdpSocket, host::IPv6, port::Uint16, flags::Uint32 = uint32(0)) = ccall(:jl_udp_bind6, Int32, (Ptr{Void}, Uint16, Ptr{Uint128}, Uint32),
            sock.handle, hton(port), &hton(host.host), flags)

function bind(sock::UdpSocket, host::IPv6, port::Uint16; ipv6only = false)
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


function setopt(sock::UdpSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)
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

_uv_hook_alloc_buf(sock::UdpSocket,size::Uint) = (c_malloc(size),size)

function _recv_start(sock::UdpSocket)
    if ccall(:uv_is_active,Cint,(Ptr{Void},),sock.handle) == 0
        uv_error("recv_start",ccall(:uv_udp_recv_start,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),
                                        sock.handle,cglobal(:jl_uv_alloc_buf),cglobal(:jl_uv_recvcb)))
    end
end

_recv_stop(sock::UdpSocket) = uv_error("recv_stop",ccall(:uv_udp_recv_stop,Cint,(Ptr{Void},),sock.handle))

function recv(sock::UdpSocket)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen 
        error("Invalid socket state")
    end
    _recv_start(sock)
    stream_wait(sock,sock.recvnotify)::Vector{Uint8}
end

function _uv_hook_recv(sock::UdpSocket, nread::Int, buf_addr::Ptr{Void}, buf_size::Uint, addr::Ptr{Void}, flags::Int32)
    if flags & UV_UDP_PARTIAL > 0
        # TODO: Decide what to do in this case. For now throw an error
        c_free(buf_addr)
        notify_error(sock.recvnotify,"Partial message received")
    end
    buf = pointer_to_array(convert(Ptr{Uint8},buf_addr),int(buf_size),true)
    notify(sock.recvnotify,buf[1:nread])
end

function _send(sock::UdpSocket,ipaddr::IPv4,port::Uint16,buf) 
    ccall(:jl_udp_send,Cint,(Ptr{Void},Uint16,Uint32,Ptr{Uint8},Csize_t),sock.handle,hton(port),hton(ipaddr.host),buf,sizeof(buf))
end

function _send(sock::UdpSocket,ipaddr::IPv6,port::Uint16,buf) 
    ccall(:jl_udp_send6,Cint,(Ptr{Void},Uint16,Ptr{Uint128},Ptr{Uint8},Csize_t),sock.handle,hton(port),&hton(ipaddr.host),buf,sizeof(buf))
end

function send(sock::UdpSocket,ipaddr,port,msg)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen 
        error("Invalid socket state")
    end
    uv_error("send",_send(sock,ipaddr,uint16(port),msg))
    stream_wait(sock,sock.sendnotify)
    nothing
end

function _uv_hook_send(sock::UdpSocket,status::Cint)
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
            cb(IPv4(ntoh(ccall(:jl_sockaddr_host4,Uint32,(Ptr{Void},),sockaddr))))
            break
        #elseif ccall(:jl_sockaddr_is_ip6,Int32,(Ptr{Void},),sockaddr) == 1
        #    host = Array(Uint128,1)
        #    scope_id = ccall(:jl_sockaddr_host6,Uint32,(Ptr{Void},Ptr{Uint128}),sockaddr,host)
        #    cb(IPv6(ntoh(host[1])))
        #    break
        end
        addrinfo = ccall(:jl_next_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
    end
    ccall(:uv_freeaddrinfo,Void,(Ptr{Void},),freeaddrinfo)
end

function getaddrinfo(cb::Function, host::ASCIIString)
    callback_dict[cb] = cb
    uv_error("getaddrinfo",ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}, Any),
        eventloop(), host, C_NULL, cb))
end
getaddrinfo(cb::Function, host::String) = getaddrinfo(cb,ascii(host))

function getaddrinfo(host::ASCIIString)
    c = Condition()
    getaddrinfo(host) do IP
        notify(c,IP)
    end
    ip = wait(c)
    isa(ip,UVError) && throw(ip)
    return ip::IpAddr
end
getaddrinfo(host::String) = getaddrinfo(ascii(host))

const _sizeof_uv_interface_address = ccall(:jl_uv_sizeof_interface_address,Int32,())

function getipaddr()
    addr = Array(Ptr{Uint8},1)
    count = Array(Int32,1)
    err = ccall(:jl_uv_interface_addresses,Int32,(Ptr{Ptr{Uint8}},Ptr{Int32}),addr,count)
    addr, count = addr[1],count[1]
    if err != 0
        ccall(:uv_free_interface_addresses,Void,(Ptr{Uint8},Int32),addr,count)
        throw(UVError("getlocalip",err))
    end
    for i = 0:(count-1)
        current_addr = addr + i*_sizeof_uv_interface_address
        if 1 == ccall(:jl_uv_interface_address_is_internal,Int32,(Ptr{Uint8},),current_addr)
            continue
        end
        sockaddr = ccall(:jl_uv_interface_address_sockaddr,Ptr{Void},(Ptr{Uint8},),current_addr)
        if ccall(:jl_sockaddr_in_is_ip4,Int32,(Ptr{Void},),sockaddr) == 1
            return IPv4(ntoh(ccall(:jl_sockaddr_host4,Uint32,(Ptr{Void},),sockaddr)))
        # Uncomment to enbable IPv6
        #elseif ccall(:jl_sockaddr_in_is_ip6,Int32,(Ptr{Void},),sockaddr) == 1
        #   host = Array(Uint128,1)
        #   ccall(:jl_sockaddr_host6,Uint32,(Ptr{Void},Ptr{Uint128}),sockaddrr,host)
        #   return IPv6(ntoh(host[1]))
        end
    end
    ccall(:uv_free_interface_addresses,Void,(Ptr{Uint8},Int32),addr,count)
    return ip"127.0.0.1"
end

##

function connect!(sock::TcpSocket, host::IPv4, port::Integer)
    @assert sock.status == StatusInit
    if !(0 <= port <= typemax(Uint16))
        throw(DomainError())
    end
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16),
                 sock.handle,hton(host.host),hton(uint16(port))))
    sock.status = StatusConnecting
end

function connect!(sock::TcpSocket, host::IPv6, port::Integer)
    @assert sock.status == StatusInit
    if !(0 <= port <= typemax(Uint16))
        throw(DomainError())
    end
    uv_error("connect",ccall(:jl_tcp6_connect,Int32,(Ptr{Void},Ptr{Uint128},Uint16),
                 sock.handle,&hton(host.host),hton(uint16(port))))
    sock.status = StatusConnecting
end

# Default Host to localhost
connect(sock::TcpSocket, port::Integer) = connect(sock,IPv4(127,0,0,1),port)
connect(port::Integer) = connect(IPv4(127,0,0,1),port)

# Valid connect signatures for TCP
connect(host::String, port::Integer) = connect(TcpSocket(),host,port)
connect(addr::IpAddr, port::Integer) = connect(TcpSocket(),addr,port)
connect(addr::InetAddr) = connect(TcpSocket(),addr)

default_connectcb(sock,status) = nothing

function connect!(sock::TcpSocket, host::String, port::Integer)
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
    sock = TcpServer()
    !bind(sock,addr) && error("cannot bind to port; may already be in use or access denied")
    uv_error("listen",_listen(sock;backlog=backlog))
    sock
end
listen(port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(IPv4(uint32(0)),port;backlog=backlog)
listen(host::IpAddr, port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(InetAddr(host,port);backlog=backlog)

listen(cb::Callback,args...; backlog::Integer=BACKLOG_DEFAULT) = (sock=listen(args...;backlog=backlog);sock.ccb=cb;sock)
listen(cb::Callback,sock::Socket; backlog::Integer=BACKLOG_DEFAULT) = (sock.ccb=cb;listen(sock;backlog=backlog))

##

function accept_nonblock(server::TcpServer,client::TcpSocket)
    @assert client.status == StatusInit
    err = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server.handle,client.handle)
    if err == 0
        client.status = StatusOpen
    end
    err
end
function accept_nonblock(server::TcpServer)
    client = TcpSocket()
    uv_error("accept", accept_nonblock(server, client))
    client
end

## Utility functions

function listenany(default_port)
    addr = InetAddr(IPv4(uint32(0)),default_port)
    while true
        sock = TcpServer()
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
