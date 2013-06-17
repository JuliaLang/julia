## IP ADDRESS HANDLING ##
abstract IpAddr

type IPv4 <: IpAddr
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
        error("IP address may not be negative")
    elseif typemax(typeof(host)) > typemax(Uint32) && host > typemax(Uint32) 
        error("Number to large for IP Address")
    else
        return IPv4(uint32(host))
    end
end

show(io::IO,ip::IPv4) = print(io,"IPv4(",dec((ip.host&(0xFF000000))>>24),".",
                                         dec((ip.host&(0xFF0000))>>16),".",
                                         dec((ip.host&(0xFF00))>>8),".",
                                         dec(ip.host&0xFF),")")

isequal(a::IPv4,b::IPv4) = a.host == b.host
hash(ip::IPv4) = hash(ip.host)

type IPv6 <: IpAddr
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
        error("IP address may not be negative")
    # We allow passing bigger integer types, but need to be careful to avoid overflow
    # Let's hope promotion rules are sensible
    elseif typemax(typeof(host)) > typemax(Uint128) && host > typemax(Uint128) 
        error("Number to large for IP Address")
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

# RFC 5952 compliant show function
# http://tools.ietf.org/html/rfc5952
function show(io::IO,ip::IPv6)
    print(io,"IPv6(")
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
    print(io,")")
end

isequal(a::IPv6,b::IPv6) = a.host == b.host
hash(ip::IPv6) = hash(ip.host)

# Parsing

function parse_ipv4(str)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields 
        if length(f) == 0
            error("Empty field in IPv4")
        end
        if f[1] == '0'
            if length(f) >= 2 && f[2] == 'x'
                r = parseint(f[3:end],16)
            else 
                r = parseint(f,8)
            end
        else
            r = parseint(f,10)
        end
        if i != length(fields)
            if r < 0 || r > 255
                error("Invalid IPv4 field")
            end
            ret |= uint32(r) << ((4-i)*8)
        else
            if r > ((uint64(1)<<((5-length(f))*8))-1)
                error("IPv4 field too large")
            end
            ret |= r
        end
        i+=1
    end
    IPv4(ret)
end

function parse_ipv6fields(fields,num_fields)
    if length(fields) > num_fields
        error("Too many fields in IPv6 address")
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
parse_ipv6fields(fields) = parse_ipv6fields(fields,8)

function parse_ipv6(str)
    fields = split(str,':')
    if length(fields) > 8
        error("Too many fields in IPv6 address")
    elseif length(fields) == 8
        return IPv6(parse_ipv6fields(fields))
    elseif contains(fields[end],'.')
        return IPv6((parse_ipv6fields(fields[1:(end-1)],6))
            | parse_ipv4(fields[end]).host )
    else
        return IPv6(parse_ipv6fields(fields))
    end
end

# 
# This support IPv4 addresses in the common dot (IPv4) or colon (IPv6)
# separated formats. Most other common formats use a standard integer encoding
# of the appropriate size and should use the appropriate constructor
#
macro ip_str(str)
    if contains(str,':')
        # IPv6 Address
        return parse_ipv6(str)
    else
        # IPv4 Address
        return parse_ipv4(str)
    end
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
    open::Bool
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Condition
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    TcpSocket(handle,open)=new(handle,open,true,PipeBuffer(),empty_callback,
                               Condition(),empty_callback,Condition(),empty_callback,Condition())
    function TcpSocket()
        this = TcpSocket(C_NULL,false)
        this.handle = c_malloc(_sizeof_uv_tcp)
        err = ccall(:uv_tcp_init,Int32,(Ptr{Void},Ptr{Void}),eventloop(),this.handle)
        if err == -1
            c_free(this.handle)
            throw(UVError("Failed to initialize TcpSocket"))
        end
        associate_julia_struct(this.handle,this)
        this
    end
end

type TcpServer <: UVServer
    handle::Ptr{Void}
    open::Bool
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    TcpServer(handle,open) = new(handle,open,empty_callback,Condition(),empty_callback,Condition())

    function TcpServer()
        this = TcpServer(C_NULL,false)
        this.handle = c_malloc(_sizeof_uv_tcp)
        err = ccall(:uv_tcp_init,Int32,(Ptr{Void},Ptr{Void}),eventloop(),this.handle)
        if err == -1
            c_free(this.handle)
            throw(UVError("Failed to initialize TcpServer"))
        end
        associate_julia_struct(this.handle,this)
        this
    end
end

#type UdpSocket <: Socket
#    handle::Ptr{Void}
#    open::Bool
#    line_buffered::Bool
#    buffer::IOBuffer
#    readcb::Callback
#    readnotify::Condition
#    ccb::Callback
#    connectnotify::Condition
#    closecb::Callback
#    closenotify::Condition
#end


show(io::IO,sock::TcpSocket) = print(io,"TcpSocket(",sock.open?"connected,":
				     "disconnected,",nb_available(sock.buffer),
				     " bytes waiting)")

show(io::IO,sock::TcpServer) = print(io,"TcpServer(",sock.open?"listening)":
                     "not listening)")

#show(io::IO,sock::UdpSocket) = print(io,"UdpSocket(",sock.open?"connected,":
#				     "disconnected,",nb_available(sock.buffer),
#				     " bytes waiting)")

_jl_tcp_init(loop::Ptr{Void}) = ccall(:jl_tcp_init,Ptr{Void},(Ptr{Void},),loop)
_jl_udp_init(loop::Ptr{Void}) = ccall(:jl_udp_init,Ptr{Void},(Ptr{Void},),loop)

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) = 
    ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = 
    ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)

accept(server::TcpServer) = accept(server, TcpSocket())


##

bind(sock::TcpServer, addr::InetAddr) = bind(sock,addr.host,addr.port)
bind(sock::TcpServer, host::IpAddr, port) = bind(sock, InetAddr(host,port))

const UV_SUCCESS = 0
const UV_EADDRINUSE = 5

function bind(sock::TcpServer, host::IPv4, port::Uint16)
    err = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, Uint16, Uint32),
	        sock.handle, hton(port), hton(host.host))
    if(err == -1 && uv_lasterror().uv_code != UV_EADDRINUSE)
        throw(UVError("bind"))
    end
    err != -1
end

bind(sock::TcpServer, host::IPv6, port::Uint16) =
    error("IPv6 Support not fully implemented")

function default_getaddrinfo_cb(req,status,addrinfo)
    data = uv_req_data(req)
    cb = unsafe_pointer_to_objref(data)::Function
    if(status!=0)
        throw(UVError("getaddrinfo callback"))
    end
    sockaddr = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
    if(ccall(:jl_sockaddr_is_ip4,Int32,(Ptr{Void},),sockaddr)==1)
        cb(IPv4(ntoh(ccall(:jl_sockaddr_host4,Uint32,(Ptr{Void},),sockaddr))))
    else
        cb(IPv6(ntoh(ccall(:jl_sockaddr_host6,Uint128,(Ptr{Void},),sockaddr))))
    end
    c_free(req)
end

callback_dict = ObjectIdDict()

jl_getaddrinfo(loop::Ptr{Void}, host::ByteString, service::Ptr{Void},
               cb::Function) =
        ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}, Any, Ptr{Void}),
	      loop,      host,       service,    cb,
          cfunction(default_getaddrinfo_cb,Void,(Ptr{Void},Int32,Ptr{Void})))

function getaddrinfo(cb::Function,host::ASCIIString)
    jl_getaddrinfo(eventloop(),host,C_NULL,cb)
end

function getaddrinfo(host::ASCIIString)
    c = Condition()
    getaddrinfo(host) do IP
	notify(c,IP)
    end
    ip = wait(c)
    return ip
end

##
function default_connect_cb(handle,status)
    data = uv_req_data(handle)
    if data != C_NULL
        stream = unsafe_pointer_to_objref(data)::AsyncStream
        stream.ccb(stream,status)
    end
    nothing
end

function connect(cb::Function, sock::TcpSocket, host::IPv4, port::Uint16)
    sock.ccb = cb
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16,Any,Ptr{Void}),
		sock.handle,hton(host.host),hton(port),sock,
            cfunction(default_connect_cb,Void,(Ptr{Void},Int32))) == -1)
end

function connect(sock::TcpSocket, host::IPv4, port::Uint16) 
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16,Any,Ptr{Void}),
		sock.handle,hton(host.host),hton(port),sock,
            cfunction(default_connect_cb,Void,(Ptr{Void},Int32))) == -1)
    wait_connected(sock)
end

function connect(sock::TcpSocket, host::ASCIIString, port::Integer)
    ipaddr = getaddrinfo(host)
    connect(sock,ipaddr,port)
end

# Default Host to localhost
connect(sock::TcpSocket, port::Integer) = conenct(sock,IPv4(127,0,0,1),port)
connect(port::Integer) = connect(IPv4(127,0,0,1),port)

function default_connectcb(sock,status)
    if status == -1
        notify(sock.connectnotify,uv_lasterror())
    end
    sock.open=true
    notify(sock.connectnotify,UV_error_t(0,0))
end 

function connect(cb::Function, sock::TcpSocket, host::ASCIIString, port)
    getaddrinfo(host) do ipaddr
	connect(cb,sock,ipaddr,port)
    end
end


for (args,forward_args) in (((:(addr::InetAddr),), (:(addr.host),:(addr.port))),
			    ((:(host::IpAddr),:port),(:(InetAddr(host,port)),)),
			    ((:(addr::InetAddr),), (:(addr.host),:(addr.port))),
			    ((:(host::ASCIIString),:port), (:host,:port)))
    @eval begin
	connect(sock::Socket,$(args...)) = connect(sock,$(forward_args...))
	connect(cb::Function,sock::Socket,$(args...)) =
	connect(cb,sock,$(forward_args...))
	function connect($(args...))
	    sock = TcpSocket()
            sock.ccb = default_connectcb
	    connect(sock,$(forward_args...))
	    sock
	end
	function connect(cb::Function,$(args...))
	    sock = TcpSocket()
            sock.ccb = default_connectcb
	    connect(cb,sock,$(forward_args...))
	    sock
	end
    end
end

##

function listen(host::IPv4, port::Uint16; backlog::Integer=511)
    sock = TcpServer()
    uv_error("listen",!bind(sock,host,port))
    uv_error("listen",!listen!(sock;backlog=backlog))
    sock
end
listen(port::Integer; backlog::Integer=511) = listen(IPv4(uint32(0)),uint16(port);backlog=backlog)
listen(addr::InetAddr; backlog::Integer=511) = listen(addr.host,addr.port;backlog=backlog)
listen(host::IpAddr, port::Uint16; backlog::Integer=511) = listen(InetAddr(host,port);backlog=backlog)

listen(cb::Callback,args...; backlog::Integer=511) = (sock=listen(args...;backlog=backlog);sock.ccb=cb;sock)
listen(cb::Callback,sock::Socket; backlog::Integer=511) = (sock.ccb=cb;listen(sock;backlog=backlog))

##

_jl_tcp_accept(server::Ptr{Void},client::Ptr{Void}) =
    ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
function accept_nonblock(server::TcpServer,client::TcpSocket)
    err = _jl_tcp_accept(server.handle,client.handle)
    if err == 0
        client.open = true
    end
    err
end
function accept_nonblock(server::TcpServer)
    client = TcpSocket()
    uv_error("accept",_jl_tcp_accept(server.handle,client.handle) == -1)
    client.open = true
    client
end

## Utility functions

function open_any_tcp_port(cb::Callback,default_port)
    addr = InetAddr(IPv4(uint32(0)),default_port)
    while true
        sock = TcpServer()
        sock.ccb = cb
        if (bind(sock,addr) && listen!(sock))
            return (addr.port,sock)
        end
        err = uv_lasterror()
        sock.open = true #need to make close() work
        close(sock)
        if (err.uv_code != UV_SUCCESS && err.uv_code != UV_EADDRINUSE)
            throw(UVError("open_any_tcp_port",err))
        end
	    addr.port += 1
        if (addr.port==default_port)
            error("Not a single port is available.")
        end
    end
end
open_any_tcp_port(default_port) = open_any_tcp_port(false,default_port)

## END OF LIBUV code

function close(x::Union(NamedPipe,PipeServer,TcpSocket,TcpServer))
    if ccall(:uv_is_closing,Int32,(Ptr{Void},),x.handle) == 1
        println("Not closeing: ",x)
        return nothing
    end
    if ccall(:uv_is_writable,Int32,(Ptr{Void},),x.handle) == 0 
        #not writeable
        _close(x)
    elseif isopen(x)
        # Already mark as closed so the stream knows we're done
        x.open = false
        req = c_malloc(_sizeof_uv_shutdown)
        uv_req_set_data(req,x)
        err = ccall(:uv_shutdown,Int32,(Ptr{Void},Ptr{Void},Ptr{Void}),req,handle(x),
            cfunction(default_shutdown_cb,Void,(Ptr{Void},Int32)))
        if err != 0
            c_free(req)
            err = UVError("close")
            _close(x)
            throw(err)
        end
    end
    nothing
end
