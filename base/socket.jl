# This file is a part of Julia. License is MIT: http://julialang.org/license

## IP ADDRESS HANDLING ##
abstract IPAddr

Base.isless{T<:IPAddr}(a::T, b::T) = isless(a.host, b.host)
Base.convert{T<:Integer}(dt::Type{T}, ip::IPAddr) = dt(ip.host)

immutable IPv4 <: IPAddr
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
IPv4(ipstr::AbstractString) = parseipv4(ipstr)

show(io::IO,ip::IPv4) = print(io,"ip\"",ip,"\"")
print(io::IO,ip::IPv4) = print(io,dec((ip.host&(0xFF000000))>>24),".",
                                  dec((ip.host&(0xFF0000))>>16),".",
                                  dec((ip.host&(0xFF00))>>8),".",
                                  dec(ip.host&0xFF))

immutable IPv6 <: IPAddr
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

IPv6(ipstr::AbstractString) = parseipv6(ipstr)

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

function parseipv4(str)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields
        if isempty(f)
            throw(ArgumentError("empty field in IPv4 address"))
        end
        if f[1] == '0'
            if length(f) >= 2 && f[2] == 'x'
                if length(f) > 8 # 2+(3*2) - prevent parseint from overflowing on 32bit
                    throw(ArgumentError("IPv4 field too large"))
                end
                r = parse(Int,f[3:end],16)
            else
                if length(f) > 9 # 1+8 - prevent parseint from overflowing on 32bit
                    throw(ArgumentError("IPv4 field too large"))
                end
                r = parse(Int,f,8)
            end
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
        if f == ""
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

function parseipv6(str)
    fields = split(str,':')
    if length(fields) > 8
        throw(ArgumentError("too many fields in IPv6 address"))
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

immutable InetAddr
    host::IPAddr
    port::UInt16

    function InetAddr(host, port::Integer)
        if !(0 <= port <= typemax(UInt16))
            throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
        end
        new(host,UInt16(port))
    end
end


## SOCKETS ##

type TCPSocket <: LibuvStream
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
    sendbuf::Nullable{IOBuffer}
    lock::ReentrantLock
    throttle::Int

    TCPSocket(handle) = new(
        handle,
        StatusUninit,
        true,
        PipeBuffer(),
        false, Condition(),
        false, Condition(),
        false, Condition(),
        nothing,
        ReentrantLock(),
        DEFAULT_READ_BUFFER_SZ
    )
end
function TCPSocket()
    this = TCPSocket(Libc.malloc(_sizeof_uv_tcp))
    associate_julia_struct(this.handle,this)
    finalizer(this,uvfinalize)
    err = ccall(:uv_tcp_init,Cint,(Ptr{Void},Ptr{Void}),
                  eventloop(),this.handle)
    if err != 0
        #TODO: this codepath is not currently tested
        Libc.free(this.handle)
        this.handle = C_NULL
        throw(UVError("failed to create tcp socket",err))
    end
    this.status = StatusInit
    this
end

type TCPServer <: LibuvServer
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
    this = TCPServer(Libc.malloc(_sizeof_uv_tcp))
    associate_julia_struct(this.handle, this)
    finalizer(this,uvfinalize)
    err = ccall(:uv_tcp_init,Cint,(Ptr{Void},Ptr{Void}),
                eventloop(),this.handle)
    if err != 0
        #TODO: this codepath is not currently tested
        Libc.free(this.handle)
        this.handle = C_NULL
        throw(UVError("failed to create tcp server",err))
    end
    this.status = StatusInit
    this
end

isreadable(io::TCPSocket) = isopen(io) || nb_available(io) > 0
iswritable(io::TCPSocket) = isopen(io) && io.status != StatusClosing

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_connect_raw(sock::TCPSocket,sockaddr::Ptr{Void}) =
    ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void},Ptr{Void}),sock.handle,sockaddr,uv_jl_connectcb::Ptr{Void})
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) =
    ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::UInt16) =
    ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},UInt16),ptr,port)

accept(server::TCPServer) = accept(server, TCPSocket())

# Libuv will internally reset the readable and writable flags on
# this pipe after it has successfully accepted the connection, to
# remember that before that this is an invalid pipe
accept(server::PipeServer) = accept(server, init_pipe!(PipeEndpoint();
    readable=false, writable=false, julia_only=true))

# UDP

type UDPSocket <: LibuvStream
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
    this = UDPSocket(Libc.malloc(_sizeof_uv_udp))
    associate_julia_struct(this.handle, this)
    err = ccall(:uv_udp_init,Cint,(Ptr{Void},Ptr{Void}),
                eventloop(),this.handle)
    finalizer(this, uvfinalize)
    if err != 0
        #TODO: this codepath is not currently tested
        Libc.free(this.handle)
        this.handle = C_NULL
        throw(UVError("failed to create udp socket",err))
    end
    this.status = StatusInit
    this
end

function uvfinalize(uv::Union{TTY,PipeEndpoint,PipeServer,TCPServer,TCPSocket,UDPSocket})
    if (uv.status != StatusUninit && uv.status != StatusInit)
        close(uv)
    end
    disassociate_julia_struct(uv)
    uv.handle = C_NULL
end

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

function bind(sock::Union{TCPServer, UDPSocket}, host::IPAddr, port::Integer; ipv6only = false, reuseaddr = false, kws...)
    if sock.status != StatusInit
        error("$(typeof(sock)) is not initialized")
    end
    flags = 0
    if isa(host,IPv6) && ipv6only
        flags |= isa(sock, UDPSocket) ? UV_UDP_IPV6ONLY : UV_TCP_IPV6ONLY
    end
    if isa(sock, UDPSocket) && reuseaddr
        flags |= UV_UDP_REUSEADDR
    end
    err = _bind(sock,host,UInt16(port),UInt32(flags))
    if err < 0
        if err != UV_EADDRINUSE && err != UV_EACCES && err != UV_EADDRNOTAVAIL
            #TODO: this codepath is not currently tested
            throw(UVError("bind",err))
        else
            return false
        end
    end
    sock.status = StatusOpen
    isa(sock, UDPSocket) && setopt(sock; kws...)
    true
end

bind(sock::TCPServer, addr::InetAddr) = bind(sock,addr.host,addr.port)

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

alloc_buf_hook(sock::UDPSocket,size::UInt) = (Libc.malloc(size),size)

function _recv_start(sock::UDPSocket)
    if ccall(:uv_is_active,Cint,(Ptr{Void},),sock.handle) == 0
        uv_error("recv_start",ccall(:uv_udp_recv_start,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),
                                    sock.handle,uv_jl_alloc_buf::Ptr{Void},uv_jl_recvcb::Ptr{Void}))
    end
end

_recv_stop(sock::UDPSocket) = uv_error("recv_stop",ccall(:uv_udp_recv_stop,Cint,(Ptr{Void},),sock.handle))

function recv(sock::UDPSocket)
    addr, data = recvfrom(sock)
    data
end

function recvfrom(sock::UDPSocket)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen
        error("UDPSocket is not initialized and open")
    end
    _recv_start(sock)
    stream_wait(sock,sock.recvnotify)::Tuple{Union{IPv4, IPv6}, Vector{UInt8}}
end


function uv_recvcb(handle::Ptr{Void}, nread::Cssize_t, buf::Ptr{Void}, addr::Ptr{Void}, flags::Cuint)
    sock = @handle_as handle UDPSocket
    buf_addr = ccall(:jl_uv_buf_base, Ptr{Void}, (Ptr{Void},), buf)
    buf_size = ccall(:jl_uv_buf_len, Csize_t, (Ptr{Void},), buf)
    # C signature documented as (*uv_udp_recv_cb)(...)
    if flags & UV_UDP_PARTIAL > 0
        Libc.free(buf_addr)
        notify_error(sock.recvnotify,"Partial message received")
    end

    # need to check the address type in order to convert to a Julia IPAddr
    addrout = if (addr == C_NULL)
                  IPv4(0)
              elseif ccall(:jl_sockaddr_in_is_ip4, Cint, (Ptr{Void},), addr) == 1
                  IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Void},), addr)))
              else
                  tmp = [UInt128(0)]
                  ccall(:jl_sockaddr_host6, UInt32, (Ptr{Void}, Ptr{UInt8}), addr, pointer(tmp))
                  IPv6(ntoh(tmp[1]))
              end
    buf = pointer_to_array(convert(Ptr{UInt8},buf_addr),Int(buf_size),true)
    notify(sock.recvnotify,(addrout,buf[1:nread]))
    nothing
end

function _send(sock::UDPSocket,ipaddr::IPv4,port::UInt16,buf)
    ccall(:jl_udp_send, Cint, (Ptr{Void},UInt16,UInt32,Ptr{UInt8},Csize_t,Ptr{Void}),
          sock.handle,hton(port),hton(ipaddr.host),buf,sizeof(buf),uv_jl_sendcb::Ptr{Void})
end

function _send(sock::UDPSocket,ipaddr::IPv6,port::UInt16,buf)
    ccall(:jl_udp_send6, Cint, (Ptr{Void},UInt16,Ptr{UInt128},Ptr{UInt8},Csize_t,Ptr{Void}),
          sock.handle,hton(port),&hton(ipaddr.host),buf,sizeof(buf),uv_jl_sendcb::Ptr{Void})
end

function send(sock::UDPSocket,ipaddr,port,msg)
    # If the socket has not been bound, it will be bound implicitly to ::0 and a random port
    if sock.status != StatusInit && sock.status != StatusOpen
        error("UDPSocket is not initialized and open")
    end
    uv_error("send",_send(sock,ipaddr,UInt16(port),msg))
    stream_wait(sock,sock.sendnotify)
    nothing
end

function uv_sendcb(handle::Ptr{Void}, status::Cint)
    sock = @handle_as handle UDPSocket
    if status < 0
        notify_error(sock.sendnotify,UVError("UDP send failed",status))
    end
    notify(sock.sendnotify)
    Libc.free(handle)
    nothing
end

##

callback_dict = ObjectIdDict()

function uv_getaddrinfocb(req::Ptr{Void}, status::Cint, addrinfo::Ptr{Void})
    data = ccall(:jl_uv_getaddrinfo_data, Ptr{Void}, (Ptr{Void},), req)
    data == C_NULL && return
    cb = unsafe_pointer_to_objref(data)::Function
    pop!(callback_dict,cb) # using pop forces an error if cb not in callback_dict
    if status != 0 || addrinfo == C_NULL
        cb(UVError("getaddrinfo callback",status))
    else
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
    Libc.free(req)
    nothing
end

function getaddrinfo(cb::Function, host::ASCIIString)
    callback_dict[cb] = cb
    uv_error("getaddrinfo",ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Cstring, Ptr{UInt8}, Any, Ptr{Void}),
                                 eventloop(), host, C_NULL, cb, uv_jl_getaddrinfocb::Ptr{Void}))
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
    addr[1] = C_NULL
    count = zeros(Int32,1)
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
            rv = IPv4(ntoh(ccall(:jl_sockaddr_host4,UInt32,(Ptr{Void},),sockaddr)))
            ccall(:uv_free_interface_addresses,Void,(Ptr{UInt8},Int32),addr,count)
            return rv
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
    if sock.status != StatusInit
        error("TCPSocket is not initialized")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},UInt32,UInt16,Ptr{Void}),
                             sock.handle,hton(host.host),hton(UInt16(port)),uv_jl_connectcb::Ptr{Void}))
    sock.status = StatusConnecting
end

function connect!(sock::TCPSocket, host::IPv6, port::Integer)
    if sock.status != StatusInit
        error("TCPSocket is not initialized")
    end
    if !(0 <= port <= typemax(UInt16))
        throw(ArgumentError("port out of range, must be 0 ≤ port ≤ 65535, got $port"))
    end
    uv_error("connect",ccall(:jl_tcp6_connect,Int32,(Ptr{Void},Ptr{UInt128},UInt16,Ptr{Void}),
                             sock.handle,&hton(host.host),hton(UInt16(port)),uv_jl_connectcb::Ptr{Void}))
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
    if sock.status != StatusInit
        error("TCPSocket is not initialized")
    end
    ipaddr = getaddrinfo(host)
    sock.status = StatusInit
    connect!(sock,ipaddr,port)
    sock.status = StatusConnecting
    sock
end

##

listen(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT) = (uv_error("listen",_listen(sock;backlog=backlog)); sock)

function listen(addr; backlog::Integer=BACKLOG_DEFAULT)
    sock = TCPServer()
    !bind(sock,addr) && error("cannot bind to port; may already be in use or access denied")
    uv_error("listen",_listen(sock;backlog=backlog))
    sock
end
listen(port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(IPv4(UInt32(0)),port;backlog=backlog)
listen(host::IPAddr, port::Integer; backlog::Integer=BACKLOG_DEFAULT) = listen(InetAddr(host,port);backlog=backlog)

listen(cb::Callback,args...; backlog::Integer=BACKLOG_DEFAULT) = (sock=listen(args...;backlog=backlog);sock.ccb=cb;sock)
listen(cb::Callback,sock::Union{TCPSocket,UDPSocket}; backlog::Integer=BACKLOG_DEFAULT) = (sock.ccb=cb;listen(sock;backlog=backlog))

##

function accept_nonblock(server::TCPServer,client::TCPSocket)
    if client.status != StatusInit
        error("client TCPSocket is not initialized")
    end
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
    addr = InetAddr(IPv4(UInt32(0)),default_port)
    while true
        sock = TCPServer()
        if bind(sock,addr) && _listen(sock) == 0
            return (addr.port,sock)
        end
        close(sock)
        addr = InetAddr(addr.host, addr.port + 1)
        if addr.port==default_port
            error("no ports available")
        end
    end
end

function getsockname(sock::Union{TCPServer,TCPSocket})
    rport = Ref{Cushort}(0)
    raddress = zeros(UInt8, 16)
    rfamily = Ref{Cuint}(0)
    r = if isa(sock, TCPServer)
        ccall(:jl_tcp_getsockname, Int32,
                (Ptr{Void}, Ref{Cushort}, Ptr{Void}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    else
        ccall(:jl_tcp_getpeername, Int32,
                (Ptr{Void}, Ref{Cushort}, Ptr{Void}, Ref{Cuint}),
                sock.handle, rport, raddress, rfamily)
    end
    uv_error("cannot obtain socket name", r);
    if r == 0
        port = ntoh(rport[])
        if rfamily[] == 2 # AF_INET
            addrv4 = raddress[1:4]
            naddr = ntoh(unsafe_load(Ptr{Cuint}(pointer(addrv4)), 1))
            addr = IPv4(naddr)
        elseif rfamily[] == @windows? 23 : (@osx? 30 : 10) # AF_INET6
            naddr = ntoh(unsafe_load(Ptr{UInt128}(pointer(raddress)), 1))
            addr = IPv6(naddr)
        else
            error("unsupported address family: $(getindex(rfamily))")
        end
    else
        error("cannot obtain socket name")
    end
    return addr, port
end
