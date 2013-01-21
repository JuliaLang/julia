#TODO: Move stdio detection from C to Julia (might require some Clang magic)
include("uv_constants.jl")

## types ##

typealias Executable Union(Vector{ByteString},Function)
typealias Callback Union(Function,Bool)
type WaitTask 
    task::Task
    filter::Callback #runs task only if false
    localdata::Any
    WaitTask(task::Task,test::Callback,localdata) = new(task,test,localdata)
    WaitTask(task::Task) = new(task, false, nothing)
end

abstract AsyncStream <: Stream

typealias UVHandle Ptr{Void}
typealias UVStream AsyncStream

const _sizeof_uv_pipe = ccall(:jl_sizeof_uv_pipe_t,Int32,())

eof(s::AsyncStream) = !s.open && nb_available(s.buffer)<=0

type NamedPipe <: AsyncStream
    handle::Ptr{Void}
    buffer::Buffer
    open::Bool
    line_buffered::Bool
    readcb::Callback
    readnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    NamedPipe() = new(C_NULL,PipeString(),false,true,false,WaitTask[],false,WaitTask[])
end

show(io::IO,stream::NamedPipe) = print(io,"NamedPipe(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

type TTY <: AsyncStream
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::Buffer
    readcb::Callback
    readnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    TTY(handle,open)=new(handle,open,true,PipeString(),false,WaitTask[],false,WaitTask[])
end

show(io::IO,stream::TTY) = print(io,"TTY(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

abstract Socket <: AsyncStream

type TcpSocket <: Socket
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::Buffer
    readcb::Callback
    readnotify::Vector{WaitTask}
    ccb::Callback
    connectnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    TcpSocket(handle,open)=new(handle,open,true,PipeString(),false,WaitTask[],false,WaitTask[],false,WaitTask[])
    function TcpSocket()
        this = TcpSocket(C_NULL,false)
        this.handle = ccall(:jl_make_tcp,Ptr{Void},(Ptr{Void},TcpSocket),globalEventLoop(),this)
        if(this.handle == C_NULL)
            error("Failed to start reading: ",_uv_lasterror())
        end
        this
    end
end

show(io::IO,sock::TcpSocket) = print(io,"TcpSocket(",sock.open?"connected,":"disconnected,",nb_available(sock.buffer)," bytes waiting)")

type UdpSocket <: Socket
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::Buffer
    readcb::Callback
    readnotify::Vector{WaitTask}
    ccb::Callback
    connectnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    UdpSocket(handle,open)=new(handle,open,true,PipeString(),false,WaitTask[],false,WaitTask[],false,WaitTask[])
    function UdpSocket()
        this = UdpSocket(C_NULL,false)
        this.handle = ccall(:jl_make_tcp,Ptr{Void},(Ptr{Void},UdpSocket),globalEventLoop(),this)
        this
    end
end

uvtype(::AsyncStream) = UV_STREAM
uvhandle(stream::AsyncStream) = stream.handle

show(io::IO,sock::UdpSocket) = print(io,"TcpSocket(",sock.open?"connected,":"disconnected,",nb_available(sock.buffer)," bytes waiting)")

copy(s::TTY) = TTY(s.handle,s.open)

convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)
handle(s::AsyncStream) = s.handle
handle(s::Ptr{Void}) = s

make_stdout_stream() = _uv_tty2tty(ccall(:jl_stdout_stream, Ptr{Void}, ()))

function _uv_tty2tty(handle::Ptr{Void})
    tty = TTY(handle,true)
    tty.line_buffered = false
    ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},TTY),handle,tty)
    tty
end

#macro init_stdio()
#begin
    const STDIN  = _uv_tty2tty(ccall(:jl_stdin_stream ,Ptr{Void},()))
    const STDOUT = _uv_tty2tty(ccall(:jl_stdout_stream,Ptr{Void},()))
    const STDERR = _uv_tty2tty(ccall(:jl_stderr_stream,Ptr{Void},()))
    OUTPUT_STREAM = STDOUT
#end
#end

#@init_stdio

## SOCKETS ##

function _init_buf(stream::AsyncStream)
    if(!isa(stream.buf,IOStream))
        stream.buf=memio()
    end
end

_jl_tcp_init(loop::Ptr{Void}) = ccall(:jl_tcp_init,Ptr{Void},(Ptr{Void},),loop)
_jl_udp_init(loop::Ptr{Void}) = ccall(:jl_udp_init,Ptr{Void},(Ptr{Void},),loop)

abstract IpAddr

type Ip4Addr <: IpAddr
    port::Uint16
    host::Uint32
end

type Ip6Addr <: IpAddr
    port::Uint16
    host::Array{Uint8,1} #this should be fixed at 16 bytes if fixed size arrays are implemented
    flow_info::Uint32
    scope::Uint32
end

function tasknotify(waittasks::Vector{WaitTask}, args...)
    newwts = WaitTask[]
    ct = current_task()
    for wt in waittasks
        f = wt.filter
        if (isa(f, Function) ? f(wt.localdata, args...) : f) === false
            work = WorkItem(wt.task)
            work.argument = args
            enq_work(work)
        else
            push!(newwts,wt)
        end
    end
    grow!(waittasks,length(newwts)-length(waittasks))
    waittasks[:] = newwts
end

wait_connect_filter(w::AsyncStream, args...) = !w.open
wait_readable_filter(w::AsyncStream, args...) = nb_available(w.buffer) <= 0
wait_readnb_filter(w::(AsyncStream,Int), args...) = w[1].open && (nb_available(w[1].buffer) < w[2])
wait_readline_filter(w::AsyncStream, args...) = w.open && (memchr(w.buffer,'\n') <= 0)

macro waitfilter(fcn,notify,filter_fcn,types)
    quote
        function $(esc(filter_fcn))(x::Vector, args...)
            for a=x
                if $(esc(filter_fcn))(a, args...)
                    return true
                end
            end
            return false
        end
        function $(esc(fcn))(x::Union($(esc(types)),Vector))
            ct = current_task()
            tw = WaitTask(ct, $(esc(filter_fcn)), x)
            args = ()
            while $(esc(filter_fcn))(x)
                if isa(x,Vector)
                    for a = x
                        if isa(a,Tuple)
                            a = a[1]
                        end
                        if $(esc(filter_fcn))(a)
                            push!(getfield(a,$(expr(:quote,notify))),tw)
                        end
                    end
                else
                    a = x
                    if isa(a,Tuple)
                        a = a[1]
                    end
                    push!(getfield(a,$(expr(:quote,notify))),tw)
                end
                ct.runnable = false
                args = yield()
                if isa(x,Vector)
                    for a = x
                        if isa(a,Tuple)
                            a = a[1]
                        end
                        a = getfield(a,$(expr(:quote,notify)))
                        i = findfirst(a, tw)
                        if i > 0 delete!(a, i) end
                    end
                else
                    a = x
                    if isa(a,Tuple)
                        a = a[1]
                    end
                    a = getfield(a,$(expr(:quote,notify)))
                    i = findfirst(a, tw)
                    if i > 0 delete!(a, i) end
                end
                if isa(args,InterruptException)
                    error(args)
                end
            end
            args
        end
    end
end

#general form of generated calls is: wait_<for_event>(o::NotificationObject, [args::AsRequired...])
@waitfilter wait_connected connectnotify wait_connect_filter AsyncStream
@waitfilter wait_readable readnotify wait_readable_filter AsyncStream
@waitfilter wait_readline readnotify wait_readline_filter AsyncStream
@waitfilter wait_readnb readnotify wait_readnb_filter (AsyncStream,Int)

wait_readnb(a::AsyncStream,b::Int) = wait_readnb((a,b))
function wait_accept(server::TcpSocket)
    client = TcpSocket()
    err = accept(server,client)
    if err == 0
        return client
    else
        err = _uv_lasterror()
        if err != 4 #EAGAIN
            error("accept error: ", err, "\n")
        end
    end
    ct = current_task()
    tw = WaitTask(ct)
    while true
        push!(server.connectnotify,tw)
        ct.runnable = false
        args = yield()
        if isa(args,InterruptException)
            error(args)
        end
        status = args[2]::Int32
        if status == -1
            error("listen error: ", _uv_lasterror(), "\n")
        end
        err = accept(server,client)
        if err == 0
            return client
        else
            err = _uv_lasterror()
            if err != 4 #EAGAIN
                error("accept error: ", err, "\n")
            end
        end
    end
end
    
#from `connect`
function _uv_hook_connectcb(sock::AsyncStream, status::Int32)
    if status != -1
        sock.open = true
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock, status)
    end
    tasknotify(sock.connectnotify, sock, status)
end
#from `listen`
function _uv_hook_connectioncb(sock::AsyncStream, status::Int32)
    if(isa(sock.ccb,Function))
        sock.ccb(sock,status)
    end
    tasknotify(sock.connectnotify, sock, status)
end

listen(sock::AsyncStream,backlog::Integer) = ccall(:jl_listen,Int32,(Ptr{Void},Int32),sock.handle,backlog)
listen(sock::AsyncStream) = listen(sock,4)

bind(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp_bind,Int32,(Ptr{Void},Uint32,Uint16),sock.handle,hton(addr.port),addr.host)

_jl_tcp_accept(server::Ptr{Void},client::Ptr{Void}) = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
function accept(server::TcpSocket,client::TcpSocket)
    err = _jl_tcp_accept(server.handle,client.handle)
    if err == 0
        client.open = true
    end
    err
end
connect(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp4_connect,Int32,(Ptr{Void},Uint32,Uint16),sock.handle,addr.host,hton(addr.port))

function open_any_tcp_port(preferred_port::Uint16,cb::Callback)
    addr = Ip4Addr(preferred_port,uint32(0)) #bind prefereed port on all adresses
    while true
        socket = TcpSocket()
        if bind(socket,addr)!=0
            error("open_any_tcp_port: could not bind to socket")
        end
        socket.ccb = cb
        if(listen(socket) == 0)
            return (addr.port,socket)
        end
        socket.open = true
        close(socket)
        addr.port+=1;
    end
end
open_any_tcp_port(preferred_port::Integer,cb::Callback)=open_any_tcp_port(uint16(preferred_port),cb)

## BUFFER ##
## Allocate a simple buffer
function alloc_request(buffer::IOString, recommended_size::Int32)
    ensureroom(buffer, int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    return (pointer(buffer.data, ptr), length(buffer.data)-ptr+1)
end
function _uv_hook_alloc_buf(stream::AsyncStream, recommended_size::Int32)
    (buf,size) = alloc_request(stream.buffer, recommended_size)
    assert(size>0) # because libuv requires this (TODO: possibly stop reading too if it fails)
    (buf,int32(size))
end

function notify_filled(buffer::IOString, nread::Int, base::Ptr{Void}, len::Int32)
    if buffer.append
        buffer.size += nread
    else
        buffer.ptr += nread
    end
end
function notify_filled(stream::AsyncStream, nread::Int)
    more = true
    while more
        if isa(stream.readcb,Function)
            nreadable = (stream.line_buffered ? int(memchr(stream.buffer, '\n')) : nb_available(stream.buffer))
            if nreadable > 0
                more = stream.readcb(stream, nreadable)
            else
                more = false
            end
        else
            more = false
        end
    end
end

function _uv_hook_readcb(stream::AsyncStream, nread::Int, base::Ptr{Void}, len::Int32)
    if(nread == -1)
        if(_uv_lasterror() != 1) #UV_EOF == 1
           error = UVError("readcb")
           close(stream)
           throw(error)
        end
        close(stream)
        tasknotify(stream.readnotify, stream)
        #EOF
    else
        notify_filled(stream.buffer, nread, base, len)
        notify_filled(stream, nread)
        tasknotify(stream.readnotify, stream)
    end
end
##########################################
# Async Workers
##########################################

abstract AsyncWork

type SingleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function SingleAsyncWork(loop::Ptr{Void},cb::Function)
        if(loop == C_NULL)
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=ccall(:jl_make_async,Ptr{Void},(Ptr{Void},SingleAsyncWork),loop,this)
        this
    end
end

type IdleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function IdleAsyncWork(loop::Ptr{Void},cb::Function)
        this=new(cb)
        this.handle=ccall(:jl_make_idle,Ptr{Void},(Ptr{Void},IdleAsyncWork),loop,this)
        this
    end
end

type TimeoutAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function TimeoutAsyncWork(loop::Ptr{Void},cb::Function)
        this=new(cb)
        this.handle=ccall(:jl_make_timer,Ptr{Void},(Ptr{Void},TimeoutAsyncWork),loop,this)
        this
    end
end

const dummySingleAsync = SingleAsyncWork(C_NULL,()->nothing)

function _uv_hook_close(uv::AsyncStream)
    uv.handle = 0
    uv.open = false
    if isa(uv.closecb, Function) uv.closecb(uv) end
    tasknotify(uv.closenotify, uv)
end
_uv_hook_close(uv::AsyncWork) = (uv.handle = 0; nothing)

# This serves as a common callback for all async classes
_uv_hook_asynccb(async::AsyncWork, status::Int32) = async.cb(status)

function startTimer(timer::TimeoutAsyncWork,timeout::Int64,repeat::Int64)
    ccall(:jl_timer_start,Int32,(Ptr{Void},Int64,Int64),timer.handle,timeout,repeat)
end

function stopTimer(timer::TimeoutAsyncWork)
    ccall(:jl_timer_stop,Int32,(Ptr{Void},),timer.handle)
end

assignIdleAsyncWork(work::IdleAsyncWork,cb::Function) = ccall(:jl_idle_start,Ptr{Void},(Ptr{Void},),work.handle)

function add_idle_cb(loop::Ptr{Void},cb::Function)
    work = initIdleAsyncWork(loop)
    assignIdleAsyncWork(work,cb)
    work
end

function queueAsync(work::SingleAsyncWork)
    ccall(:jl_async_send,Void,(Ptr{Void},),work.handle)
end

## event loop ##
globalEventLoop() = ccall(:jl_global_event_loop,Ptr{Void},())
#mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},()) # this would be fine, but is nowhere supported

function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
run_event_loop() = run_event_loop(globalEventLoop())

##pipe functions
malloc_pipe() = c_malloc(_sizeof_uv_pipe)
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool,pipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Bool,Bool,AsyncStream), read_end, 0, readable_julia_only, pipe)
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Bool,Bool,AsyncStream), write_end, 1, readable_julia_only, pipe)
    error = ccall(:uv_pipe_link, Int, (Ptr{Void}, Ptr{Void}), read_end, write_end)
    if error != 0 # don't use assert here as $string isn't be defined yet
        error("uv_pipe_link failed")
    end
end

function link_pipe(read_end2::NamedPipe,readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool)
    if(read_end2.handle == C_NULL)
        read_end2.handle = malloc_pipe()
    end
    link_pipe(read_end2.handle,readable_julia_only,write_end,writeable_julia_only,read_end2)
    read_end2.open = true
end
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::NamedPipe,writeable_julia_only::Bool)
    if(write_end.handle == C_NULL)
        write_end.handle = malloc_pipe()
    end
    link_pipe(read_end,readable_julia_only,write_end.handle,writeable_julia_only,write_end)
    write_end.open = true
end
close_pipe_sync(handle::UVHandle) = ccall(:uv_pipe_close_sync,Void,(UVHandle,),handle)

function close(stream::AsyncStream)
    if stream.open
        stream.open = false
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
    end
end

##stream functions

start_reading(stream::AsyncStream) = (stream.handle != 0 ? ccall(:jl_start_reading,Int32,(Ptr{Void},),handle(stream)) : int32(0))
function start_reading(stream::AsyncStream,cb::Function)
    start_reading(stream)
    stream.readcb = cb
    nread = nb_available(stream.buffer)
    if nread > 0
        notify_filled(stream,nread)
    end
end
start_reading(stream::AsyncStream,cb::Bool) = (start_reading(stream); stream.readcb = cb)

stop_reading(stream::AsyncStream) = ccall(:uv_read_stop,Bool,(Ptr{Void},),handle(stream))

function readall(stream::AsyncStream)
    start_reading(stream)
    wait_close(stream)
    return takebuf_string(stream.buffer)
end

function read{T}(this::AsyncStream, a::Array{T})
    if isa(T, BitsKind)
        nb = length(a)*sizeof(T)
        buf = this.buffer
        assert(buf.seekable == false)
        assert(buf.maxsize >= nb)
        wait_readnb(this,nb)
        read(this.buffer, a)
        return a
    else
        #error("Read from Buffer only supports bits types or arrays of bits types; got $T.")
        error("Read from Buffer only supports bits types or arrays of bits types")
    end
end

function read(this::AsyncStream,::Type{Uint8})
    buf = this.buffer
    assert(buf.seekable == false)
    wait_readnb(this,1)
    read(buf,Uint8)
end

function readline(this::AsyncStream)
    buf = this.buffer
    assert(buf.seekable == false)
    start_reading(this)
    wait_readline(this)
    readline(buf)
end

function finish_read(pipe::NamedPipe)
    close(pipe) #handles to UV and ios will be invalid after this point
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end


## low-level calls

write(s::AsyncStream, b::ASCIIString) =
    int(ccall(:jl_puts, Int32, (Ptr{Uint8},Ptr{Void}),b.data,handle(s)))
write(s::AsyncStream, b::Uint8) =
    int(ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b, handle(s)))
write(s::AsyncStream, c::Char) =
    int(ccall(:jl_pututf8, Int32, (Ptr{Void},Char), handle(s), c))
function write{T}(s::AsyncStream, a::Array{T})
    if(isa(T,BitsKind))
        ccall(:jl_write, Int, (Ptr{Void}, Ptr{Void}, Uint32), handle(s), a, uint(length(a)*sizeof(T)))
    else
        invoke(write,(IO,Array),s,a)
    end
end
write(s::AsyncStream, p::Ptr, nb::Integer) = ccall(:jl_write, Int, (Ptr{Void}, Ptr{Void}, Uint), handle(s), p, uint(nb))
_write(s::AsyncStream, p::Ptr{Void}, nb::Integer) = ccall(:jl_write, Int, (Ptr{Void}, Ptr{Void}, Uint), handle(s), p, uint(nb))

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void}) = ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void}),sock.handle,sockaddr)
_jl_getaddrinfo(loop::Ptr{Void},host::ByteString,service::Ptr{Void},cb::Function) = ccall(:jl_getaddrinfo,Int32,(Ptr{Void},Ptr{Uint8},Ptr{Uint8},Function),loop,host,service,cb)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)

## Libuv error handling
_uv_lasterror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)
_uv_lasterror() = _uv_lasterror(globalEventLoop())
_uv_lastsystemerror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)
_uv_lastsystemerror() = _uv_lasterror(globalEventLoop())

type UVError <: Exception
    prefix::String
    uv_code::Int32
    system_code::Int32
    UVError(p::String)=new(p,_uv_lasterror(),_uv_lastsystemerror())
    UVError(p::String,uv::Int,system::Int)=new(p,uv,system)
end

struverror(err::UVError) = bytestring(ccall(:jl_uv_strerror,Ptr{Uint8},(Int32,Int32),err.uv_code,err.system_code))
uverrorname(err::UVError) = bytestring(ccall(:jl_uv_err_name,Ptr{Uint8},(Int32,Int32),err.uv_code,err.system_code))

uv_error(prefix, b::Bool) = b?throw(UVError(string(prefix))):nothing
uv_error(prefix) = uv_error(prefix, _uv_lasterror() != 0)

show(io::IO, e::UVError) = print(io, e.prefix*": "*struverror(e)*" ("*uverrorname(e)*")")

function getaddrinfo_callback(sock::TcpSocket,status::Int32,host::ByteString,port::Uint16,addrinfo_list::Ptr{Void})
    #println("getaddrinfo_callback")
    if(status==-1)
        error("Name lookup failed "*host)
    end
    sockaddr = _jl_sockaddr_from_addrinfo(addrinfo_list) #only use first entry of the list for now
    _jl_sockaddr_set_port(sockaddr,hton(port))
    err = _jl_connect_raw(sock,sockaddr)
    if(err != 0)
        error("Failed to connect to host "*host*":"*string(port)*" #"*string(_uv_lasterror()))
    end
end

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

function connect_to_host(host::ByteString,port::Integer) #TODO: handle errors
    port = uint16(port)
    sock = TcpSocket()
    err = _jl_getaddrinfo(globalEventLoop(),host,C_NULL,
        (addrinfo::Ptr{Void},status::Int32) -> getaddrinfo_callback(sock,status,host,port,addrinfo))
    if(err!=0)
        error("Failed to  initilize request to resolve hostname: ",host)
    end
    wait_connected(sock)
    return sock
end
