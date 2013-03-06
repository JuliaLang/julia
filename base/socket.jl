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

show(io::IO,ip::IPv4) = print(io,"IPv4(",dec((ip.host&(0xFF000000))>>24),".",
                                         dec((ip.host&(0xFF0000))>>16),".",
                                         dec((ip.host&(0xFF00))>>8),".",
                                         dec(ip.host&0xFF),")")

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

show(io::IO,ip::IPv6) = print(io,"IPv6(",
                        hex((ip.host&(uint128(0xFFFF)<<(7*16)))>>(7*16)),":",
                        hex((ip.host&(uint128(0xFFFF)<<(6*16)))>>(7*16)),":",
                        hex((ip.host&(uint128(0xFFFF)<<(5*16)))>>(7*16)),":",
                        hex((ip.host&(uint128(0xFFFF)<<(4*16)))>>(7*16)),":",
                        hex((ip.host&(uint128(0xFFFF)<<(3*16)))>>(7*16)),":",
                        hex((ip.host&(uint128(0xFFFF)<<(2*16)))>>(7*16)),":",
                        hex((ip.host&0xFFFF), ")"))

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
    readnotify::Vector{WaitTask}
    ccb::Callback
    connectnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    TcpSocket(handle,open)=new(handle,open,true,PipeBuffer(),false,
                               WaitTask[],false,WaitTask[],false,WaitTask[])
    function TcpSocket()
        this = TcpSocket(C_NULL,false)
        this.handle = ccall(:jl_make_tcp,Ptr{Void},(Ptr{Void},Any),
                            eventloop(),this)
        if (this.handle == C_NULL)
            error("Failed to start reading: ",_uv_lasterror())
        end
        this
    end
end

type UdpSocket <: Socket
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Vector{WaitTask}
    ccb::Callback
    connectnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    UdpSocket(handle,open)=new(handle,open,true,PipeBuffer(),false,WaitTask[],
                               false,WaitTask[],false,WaitTask[])
    function UdpSocket()
        this = UdpSocket(C_NULL,false)
        this.handle = ccall(:jl_make_tcp,Ptr{Void},(Ptr{Void},Any),
                            eventloop(),this)
        this
    end
end

show(io::IO,sock::TcpSocket) = print(io,"TcpSocket(",sock.open?"connected,":
				     "disconnected,",nb_available(sock.buffer),
				     " bytes waiting)")

show(io::IO,sock::UdpSocket) = print(io,"UdpSocket(",sock.open?"connected,":
				     "disconnected,",nb_available(sock.buffer),
				     " bytes waiting)")
_jl_tcp_init(loop::Ptr{Void}) = ccall(:jl_tcp_init,Ptr{Void},(Ptr{Void},),loop)
_jl_udp_init(loop::Ptr{Void}) = ccall(:jl_udp_init,Ptr{Void},(Ptr{Void},),loop)

## VARIOUS METHODS TO BE MOVED TO BETTER LOCATION

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void}) = 
    ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void}),sock.handle,sockaddr)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) = 
    ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = 
    ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)


## WAITING ##

function wait_accept(server::TcpSocket)
    client = TcpSocket()
    err = accept(server,client)
    if err == 0
        return client
    else
        err = _uv_lasterror()
        if err != 4 #EAGAIN
            error("accept: ", err, "\n")
        end
    end
    wt = WaitTask()
    while true
        push!(server.connectnotify,wt)
        args = yield(wt)
        if isa(args,InterruptException)
            error(args)
        end
        status = args[2]::Int32
        if status == -1
            error("listen: ", _uv_lasterror(), "\n")
        end
        err = accept(server,client)
        if err == 0
            return client
        else
            err = _uv_lasterror()
            if err != 4 #EAGAIN
                error("accept: ", err, "\n")
            end
        end
    end
end

##

bind(sock::Socket, addr::InetAddr) = bind(sock,addr.host,addr.port)
bind(sock::Socket, host::IpAddr, port) = bind(sock, InetAddr(host,port))

const UV_SUCCESS = 0
const UV_EADDRINUSE = 5

function bind(sock::TcpSocket, host::IPv4, port::Uint16)
    err = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, Uint16, Uint32),
	        sock.handle, hton(port), hton(host.host))
    if(err == -1 && _uv_lasterror() != UV_EADDRINUSE)
        throw(UVError("bind"))
    end
    err != -1
end

bind(sock::TcpSocket, host::IPv6, port::Uint16) = 
    error("IPv6 Support not fully implemented")

##

callback_dict = ObjectIdDict()

function _uv_hook_getaddrinfo(cb::Function,addrinfo::Ptr{Void}, status::Int32)
    delete!(callback_dict,cb)
    if(status==-1)
        throw(UVError("getaddrinfo callback"))
    end
    sockaddr = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
    if(ccall(:jl_sockaddr_is_ip4,Int,(Ptr{Void},),sockaddr)==1)
        cb(IPv4(ntoh(ccall(:jl_sockaddr_host4,Uint32,(Ptr{Void},),sockaddr))))
    else
        cb(IPv6(ntoh(ccall(:jl_sockaddr_host6,Uint128,(Ptr{Void},),sockaddr))))
    end
end

jl_getaddrinfo(loop::Ptr{Void}, host::ByteString, service::Ptr{Void},
               cb::Function) =
        ccall(:jl_getaddrinfo, Int32, (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}, Any),
	      loop,      host,       service,    cb)

getaddrinfo(cb::Function,host::ASCIIString) = begin
    callback_dict[cb] = cb
    jl_getaddrinfo(eventloop(),host,C_NULL,cb)
end

function getaddrinfo(host::ASCIIString)
    wt = WaitTask()
    getaddrinfo(host) do IP
	tasknotify([wt],IP)
    end
    (ip,) = yield(wt)
    return ip
end

##

function connect(cb::Function, sock::TcpSocket, host::IPv4, port::Uint16)
    sock.ccb = cb
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16),
			     sock.handle,hton(host.host),hton(port)) == -1)
end

function connect(sock::TcpSocket, host::IPv4, port::Uint16) 
    uv_error("connect",ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16),
			     sock.handle,hton(host.host),hton(port)) == -1)
    wait_connected(sock)
end

function connect(sock::TcpSocket, host::ASCIIString, port)
    ipaddr = getaddrinfo(host)
    connect(sock,ipaddr,port)
end

function default_connectcb(sock,status)
    if status == -1
        throw(UVError("connect callback"))
    end
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

function listen(host::IPv4, port::Uint16)
    sock = TcpSocket()
    uv_error("listen",!bind(sock,host,port))
    listen(sock)
    sock
end
listen(port::Integer) = listen(IPv4(uint32(0)),uint16(port))
listen(addr::InetAddr) = listen(addr.host,addr.port)
listen(host::IpAddr, port::Uint16) = listen(InetAddr(host,port))

listen(cb::Callback,args...) = (sock=listen(args...);sock.ccb=cb;sock)
listen(cb::Callback,sock::Socket) = (sock.ccb=cb;listen(sock))

##

_jl_tcp_accept(server::Ptr{Void},client::Ptr{Void}) =
    ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
function accept(server::TcpSocket,client::TcpSocket)
    err = _jl_tcp_accept(server.handle,client.handle)
    if err == 0
        client.open = true
    end
    err
end
function accept(server::TcpSocket)
    client = TcpSocket()
    uv_error("accept",_jl_tcp_accept(server.handle,client.handle) == -1)
    client.open = true
    client
end

## Utility functions

function open_any_tcp_port(cb::Callback,default_port)
    addr = InetAddr(IPv4(uint32(0)),default_port)
    while true
        sock = TcpSocket()

        if (bind(sock,addr) && listen(cb,sock))
            return (addr.port,sock)
        end
        err = _uv_lasterror()
        system = _uv_lastsystemerror()
        sock.open = true #need to make close() work
        close(sock)
        if (err != UV_SUCCESS && err != UV_EADDRINUSE)
            throw(UVError("open_any_tcp_port",err,system))
        end
	addr.port += 1
        if (addr.port==default_port)
            error("Not a single port is available.")
        end
    end
end
open_any_tcp_port(default_port) = open_any_tcp_port(false,default_port)
