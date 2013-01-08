#TODO: Move stdio detection from C to Julia (might require some Clang magic)

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
typealias RawOrBoxedHandle Union(UVHandle,UVStream)
typealias StdIOSet (RawOrBoxedHandle, RawOrBoxedHandle, RawOrBoxedHandle)

const _sizeof_uv_pipe = ccall(:jl_sizeof_uv_pipe_t,Int32,())

abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Executable
    ignorestatus::Bool
    Cmd(exec::Executable) = new(exec,false)
end

const STDIN_NO  = 0
const STDOUT_NO = 1
const STDERR_NO = 2

type StreamRedirect <: AbstractCmd
	cmd::AbstractCmd
	stream::UVStream
	stream_no::Int
end

type OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

ignorestatus(cmd::Cmd) = (cmd.ignorestatus=true; cmd)
ignorestatus(cmd::Union(OrCmds,AndCmds)) = (ignorestatus(cmd.a); ignorestatus(cmd.b); cmd)


type Process
    cmd::Cmd
    handle::Ptr{Void}
    in::AsyncStream
    out::AsyncStream
    err::AsyncStream
    exit_code::Int32
    term_signal::Int32
    exitcb::Callback
    exitnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    function Process(cmd::Cmd,handle::Ptr{Void},in::RawOrBoxedHandle,out::RawOrBoxedHandle,err::RawOrBoxedHandle)
        if(!isa(in,AsyncStream))
            in=null_handle
        end
        if(!isa(out,AsyncStream))
            out=null_handle
        end
        if(!isa(err,AsyncStream))
            err=null_handle
        end
        new(cmd,handle,in,out,err,-2,-2,false,WaitTask[],false,WaitTask[])
    end
end

type ProcessChain
    processes::Vector{Process}
    in::UVStream
    out::UVStream
    err::UVStream
    ProcessChain(stdios::StdIOSet) = new(Process[],stdios[1],stdios[2],stdios[3])
end
typealias ProcessChainOrNot Union(Bool,ProcessChain)

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

show(io,stream::NamedPipe) = print(io,"NamedPipe(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

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

show(io,stream::TTY) = print(io,"TTY(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

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

show(io,sock::TcpSocket) = print(io,"TcpSocket(",sock.open?"connected,":"disconnected,",nb_available(sock.buffer)," bytes waiting)")

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

show(io,sock::UdpSocket) = print(io,"TcpSocket(",sock.open?"connected,":"disconnected,",nb_available(sock.buffer)," bytes waiting)")

copy(s::TTY) = TTY(s.handle,s.open)

#SpawnNullStream is Singleton
type SpawnNullStream <: AsyncStream end
const null_handle = SpawnNullStream()
SpawnNullStream() = null_handle
copy(::SpawnNullStream) = null_handle

convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)
handle(s::AsyncStream) = s.handle
handle(::SpawnNullStream) = C_NULL
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
    const stdin_stream  = STDIN
    const stdout_stream = STDOUT
    const stderr_stream = STDERR
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
            push(newwts,wt)
        end
    end
    grow(waittasks,length(newwts)-length(waittasks))
    waittasks[:] = newwts
end

wait_exit_filter(p::Process, args...) = !process_exited(p)
wait_connect_filter(w::AsyncStream, args...) = !w.open
wait_close_filter(w::Union(AsyncStream,Process), args...) = w.open
wait_readable_filter(w::AsyncStream, args...) = nb_available(w.buffer) <= 0
wait_readnb_filter(w::(AsyncStream,Int), args...) = w[1].open && (nb_available(w[1].buffer) < w[2])
wait_readline_filter(w::AsyncStream, args...) = w.open && (memchr(w.buffer,'\n') <= 0)

#general form of generated calls is: wait_<for_event>(o::NotificationObject, [args::AsRequired...])
for (fcn, notify, filter_fcn, types) in
    ((:wait_exit,:closenotify,:wait_exit_filter,:Process), #close happens almost immediately after exit, but gives I/O time to finish
     (:wait_connected,:connectnotify,:wait_connect_filter,:AsyncStream),
     (:wait_close,:closenotify,:wait_close_filter,:(Union(AsyncStream,Process))),
     (:wait_readable,:readnotify,:wait_readable_filter,:AsyncStream),
     (:wait_readline,:readnotify,:wait_readline_filter,:AsyncStream),
     (:wait_readnb,:readnotify,:wait_readnb_filter,:(AsyncStream,Int)))
    @eval begin
        function $filter_fcn(x::Vector{$types}, args...)
            for a=x
                if $filter_fcn(a, args...)
                    return true
                end
            end
            return false
        end
        function $fcn(x::Union($types,Vector{$types}))
            ct = current_task()
            tw = WaitTask(ct, $filter_fcn, x)
            args = ()
            while $filter_fcn(x)
                if isa(x,Vector)
                    for a = x
                        if isa(a,Tuple)
                            a = a[1]
                        end
                        if $filter_fcn(a)
                            push(getfield(a,$(expr(:quote,notify))),tw)
                        end
                    end
                else
                    a = x
                    if isa(a,Tuple)
                        a = a[1]
                    end
                    push(getfield(a,$(expr(:quote,notify))),tw)
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
                        if i > 0 del(a, i) end
                    end
                else
                    a = x
                    if isa(a,Tuple)
                        a = a[1]
                    end
                    a = getfield(a,$(expr(:quote,notify)))
                    i = findfirst(a, tw)
                    if i > 0 del(a, i) end
                end
                if isa(args,InterruptException)
                    error(args)
                end
            end
            args
        end
    end
end
wait_exit(x::ProcessChain) = wait_exit(x.processes)
function wait_read(x::AsyncStream)
    ct = current_task()
    tw = WaitTask(ct)
    push(x.readnotify,tw)
    ct.runnable = false
    yield()
end
wait_success(x::ProcessChain) = wait_success(x.processes)
function wait_success(x::Union(Process,Vector{Process}))
    wait_exit(x)
    kill(x)
    success(x)
end
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
        push(server.connectnotify,tw)
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
        close(stream)
        if(isa(stream.closecb,Function))
            stream.closecb()
        end
        if(_uv_lasterror() != 1) #UV_EOF == 1
            error("Failed to start reading: ",_uv_lasterror(globalEventLoop()))
        end
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

## process status ##
process_running(s::Process) = s.exit_code == -2
process_running(s::Vector{Process}) = all(map(process_running,s))
process_running(s::ProcessChain) = process_running(s.processes)

process_exit_status(s::Process) = s.exit_code
process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(map(process_exited,s))
process_exited(s::ProcessChain) = process_running(s.processes)

process_term_signal(s::Process) = s.term_signal
process_signaled(s::Process) = (s.term_signal > 0)

#process_stopped (s::Process) = false #not supported by libuv. Do we need this?
#process_stop_signal(s::Process) = false #not supported by libuv. Do we need this?

function process_status(s::Process)
    process_running (s) ? "ProcessRunning" :
    process_signaled(s) ? "ProcessSignaled("*string(process_term_signal(s))*")" :
    #process_stopped (s) ? "ProcessStopped("*string(process_stop_signal(s))*")" :
    process_exited  (s) ? "ProcessExited("*string(process_exit_status(s))*")" :
    error("process status error")
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
        nb = numel(a)*sizeof(T)
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

show(io, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")

function finish_read(pipe::NamedPipe)
    close(pipe) #handles to UV and ios will be invalid after this point
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function _jl_spawn(cmd::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, loop::Ptr{Void}, pp::Process,
        in::Ptr{Void}, out::Ptr{Void}, err::Ptr{Void})
    return ccall(:jl_spawn, Ptr{Void},
        (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Void}, Process, Ptr{Void}, Ptr{Void}, Ptr{Void}),
         cmd,        argv,            loop,      pp,       in,        out,       err)
end

function _uv_hook_return_spawn(proc::Process, exit_status::Int32, term_signal::Int32)
    proc.exit_code = exit_status
    proc.term_signal = term_signal
    if isa(proc.exitcb, Function) proc.exitcb(proc, exit_status, term_signal) end
    tasknotify(proc.exitnotify, proc)
end

function _uv_hook_close(proc::Process)
    proc.handle = 0
    if isa(proc.closecb, Function) proc.closecb(proc) end
    tasknotify(proc.closenotify, proc)
end

function spawn(pc::ProcessChainOrNot,cmd::Cmd,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    loop = globalEventLoop()
    close_in,close_out,close_err = false,false,false
    if(isa(stdios[1],NamedPipe)&&stdios[1].handle==C_NULL)
        in = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #in = c_malloc(_sizeof_uv_pipe)
        link_pipe(in,false,stdios[1],true)
        close_in = true
    else
        in = handle(stdios[1])
    end
    if(isa(stdios[2],NamedPipe)&&stdios[2].handle==C_NULL)
        out = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #out = c_malloc(_sizeof_uv_pipe)
        link_pipe(stdios[2],false,out,true)
        close_out = true
    else
        out = handle(stdios[2])
    end
    if(isa(stdios[3],NamedPipe)&&stdios[3].handle==C_NULL)
        err = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #err = c_malloc(_sizof_uv_pipe)
        link_pipe(stdios[3],false,err,true)
        close_err = true
    else
        err = handle(stdios[3])
    end
    pp = Process(cmd,C_NULL,stdios[1],stdios[2],stdios[3]);
    ptrs = _jl_pre_exec(cmd.exec)
    pp.exitcb = exitcb
    pp.closecb = closecb
    pp.handle=_jl_spawn(ptrs[1], convert(Ptr{Ptr{Uint8}}, ptrs), loop, pp,
        in,out,err)
    if pc != false
        push(pc.processes, pp)
    end
    if(close_in)
        close_pipe_sync(in)
        #c_free(in)
    end
    if(close_out)
        close_pipe_sync(out)
        #c_free(out)
    end
    if(close_err)
        close_pipe_sync(err)
        #c_free(err)
    end
    pp
end

function spawn(pc::ProcessChainOrNot,redirect::StreamRedirect,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
	spawn(pc,redirect.cmd,(redirect.stream_no==STDIN_NO ?redirect.stream:stdios[1],
						   redirect.stream_no==STDOUT_NO?redirect.stream:stdios[2],
						   redirect.stream_no==STDERR_NO?redirect.stream:stdios[3]),exitcb,closecb)
end

function spawn(pc::ProcessChainOrNot,cmds::OrCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    out_pipe = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
    in_pipe = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
    #out_pipe = c_malloc(_sizeof_uv_pipe)
    #in_pipe = c_malloc(_sizeof_uv_pipe)
    link_pipe(in_pipe,false,out_pipe,false,null_handle)
    if pc == false
        pc = ProcessChain(stdios)
    end
    try
        spawn(pc, cmds.a, (stdios[1], out_pipe, stdios[3]), exitcb, closecb)
        spawn(pc, cmds.b, (in_pipe, stdios[2], stdios[3]), exitcb, closecb)
    catch err
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
        rethrow(err)
    end
    close_pipe_sync(out_pipe)
    close_pipe_sync(in_pipe)
    pc
end

function spawn(pc::ProcessChainOrNot,cmds::AndCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    if pc == false
        pc = ProcessChain(stdios)
    end
    close_in,close_out,close_err = false,false,false
    if(isa(stdios[1],NamedPipe)&&stdios[1].handle==C_NULL)
        in = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #in = c_malloc(_sizeof_uv_pipe)
        link_pipe(in,false,stdios[1],true)
        close_in = true
    else
        in = handle(stdios[1])
    end
    if(isa(stdios[2],NamedPipe)&&stdios[2].handle==C_NULL)
        out = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #out = c_malloc(_sizeof_uv_pipe)
        link_pipe(stdios[2],false,out,true)
        close_out = true
    else
        out = handle(stdios[2])
    end
    if(isa(stdios[3],NamedPipe)&&stdios[3].handle==C_NULL)
        err = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #err = c_malloc(_sizof_uv_pipe)
        link_pipe(stdios[3],false,err,true)
        close_err = true
    else
        err = handle(stdios[3])
    end
    spawn(pc, cmds.a, (in,out,err), exitcb, closecb)
    spawn(pc, cmds.b, (in,out,err), exitcb, closecb)
    if(close_in)
        close_pipe_sync(in)
        #c_free(in)
    end
    if(close_out)
        close_pipe_sync(out)
        #c_free(out)
    end
    if(close_err)
        close_pipe_sync(err)
        #c_free(err)
    end
    pp
    pc
end

function reinit_stdio()
    STDIN.handle  = ccall(:jl_stdin_stream ,Ptr{Void},())
    STDOUT.handle = ccall(:jl_stdout_stream,Ptr{Void},())
    STDERR.handle = ccall(:jl_stderr_stream,Ptr{Void},())
    STDIN.buffer = PipeString()
    STDOUT.buffer = PipeString()
    STDERR.buffer = PipeString()
    for stream in (STDIN,STDOUT,STDERR)
        ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},TTY),stream.handle,stream)
    end
end

# INTERNAL
# returns a tuple of function arguments to spawn:
# (stdios, exitcb, closecb)
# |       |        \ The function to be called once the uv handle is closed
# |       \ The function to be called once the process exits
# \ A set of up to 256 stdio instructions, where each entry can be either:
#   | - An AsyncStream to be passed to the child
#   | - true: This will let the child inherit the parents' io (only valid for 0-2)
#   \ - false: None (for 3-255) or /dev/null (for 0-2)


for (sym, stdin, stdout, stderr) in {(:spawn_opts_inherit, STDIN,STDOUT,STDERR),
                       (:spawn_opts_swallow, null_handle,null_handle,null_handle)}
@eval begin
 ($sym)(stdios::StdIOSet,exitcb::Callback,closecb::Callback) = (stdios,exitcb,closecb)
 ($sym)(stdios::StdIOSet,exitcb::Callback) = (stdios,exitcb,false)
 ($sym)(stdios::StdIOSet) = (stdios,false,false)
 ($sym)() = (($stdin,$stdout,$stderr),false,false)
 ($sym)(in::UVStream) = ((isa(in,AsyncStream)?in:$stdin,$stdout,$stderr),false,false)
 ($sym)(in::UVStream,out::UVStream) = ((isa(in,AsyncStream)?in:$stdin,isa(out,AsyncStream)?out:$stdout,$stderr),false,false)
 ($sym)(in::UVStream,out::UVStream,err::UVStream) = ((isa(in,AsyncStream)?in:$stdin,isa(out,AsyncStream)?out:$stdout,isa(err,AsyncStream)?err:$stderr),false,false)
end
end

spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,args...) = spawn(pc,cmds,spawn_opts_swallow(args...)...)
spawn(cmds::AbstractCmd,args...) = spawn(false,cmds,spawn_opts_swallow(args...)...)

spawn_nostdin(pc::ProcessChainOrNot,cmd::AbstractCmd,out::UVStream) = spawn(pc,cmd,(null_handle,out,null_handle),false,false)
spawn_nostdin(cmd::AbstractCmd,out::UVStream) = spawn(false,cmd,(null_handle,out,null_handle),false,false)

#returns a pipe to read from the last command in the pipelines
read_from(cmds::AbstractCmd)=read_from(cmds, null_handle)
function read_from(cmds::AbstractCmd, stdin::AsyncStream)
    out = NamedPipe()
    processes = spawn(false, cmds, (stdin,out,STDERR))
    start_reading(out)
    (out, processes)
end

write_to(cmds::AbstractCmd) = write_to(cmds, null_handle)
function write_to(cmds::AbstractCmd, stdout::UVStream)
    in = NamedPipe()
    processes = spawn(false, cmds, (in,stdout,null_handle))
    (in, processes)
end

readall(cmd::AbstractCmd) = readall(cmd, null_handle)
function readall(cmd::AbstractCmd,stdin::AsyncStream)
    (out,pc)=read_from(cmd, stdin)
    if !wait_success(pc)
        pipeline_error(pc)
    end
    return takebuf_string(out.buffer)
end

writeall(cmd::AbstractCmd, stdout::String) = writeall(cmd, stdout, null_handle)
function writeall(cmd::AbstractCmd, stdin::String, stdout::AsyncStream)
    (in,pc) = write_to(cmd, stdout)
    write(in, stdin)
    close(in)
    if !wait_success(pc)
        pipeline_error(pc)
    end
    return true
end

function run(cmds::AbstractCmd,args...)
    ps = spawn(cmds,spawn_opts_inherit(args...)...)
    success = wait_success(ps)
    if success
        return true
    else
        return pipeline_error(ps)
    end
end

success(proc::Process) = (assert(process_exited(proc)); proc.exit_code==0)
success(procs::Vector{Process}) = all(map(success, procs))
success(procs::ProcessChain) = success(procs.processes)
success(cmd::AbstractCmd) = wait_success(spawn(cmd))

function pipeline_error(proc::Process)
    if !proc.cmd.ignorestatus
        error("failed process: ",proc," [",proc.exit_code,"]")
    end
    true
end

function pipeline_error(procs::ProcessChain)
    failed = Process[]
    for p = procs.processes
        if !success(p) && !p.cmd.ignorestatus
            push(failed, p)
        end
    end
    if numel(failed)==0 return true end
    if numel(failed)==1 pipeline_error(failed[1]) end
    msg = "failed processes:"
    for proc in failed
        msg = string(msg,"\n  ",proc," [",proc.exit_code,"]")
    end
    error(msg)
    return false
end

function exec(thunk::Function)
    try
        thunk()
    catch err
        show(err)
        exit(0xff)
    end
    exit(0)
end

_jl_kill(p::Process,signum::Integer) = ccall(:uv_process_kill,Int32,(Ptr{Void},Int32),p.handle,signum)
function kill(p::Process,signum::Integer)
    if process_running(p)
        _jl_kill(p, signum)
    else
        int32(-1)
    end
end
kill(ps::Vector{Process}) = map(kill, ps)
kill(ps::ProcessChain) = map(kill, ps.processes)
kill(p::Process) = kill(p,15) #SIGTERM

function _contains_newline(bufptr::Ptr{Void},len::Int32)
    return (ccall(:memchr,Ptr{Uint8},(Ptr{Void},Int32,Uint),bufptr,'\n',len)!=C_NULL)
end


# WARNING: do not call this and keep the returned array of pointers
# around longer than the args vector and then use array of pointers.
# this could cause a segfault. this is really just for use by the
# spawn function below so that we can exec more efficiently.
#
function _jl_pre_exec(args::Vector{ByteString})
    if length(args) < 1
        error("exec: too few words to exec")
    end
    ptrs = Array(Ptr{Uint8}, length(args)+1)
    for i = 1:length(args)
        ptrs[i] = args[i].data
    end
    ptrs[length(args)+1] = C_NULL
    return ptrs
end

## implementation of `cmd` syntax ##

arg_gen(x::String) = ByteString[x]
arg_gen(cmd::Cmd)  = cmd.exec

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push(vals,string(x))
        end
        return vals
    else
        return ByteString[string(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push(vals,bytestring(strcat(h,t)))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args,arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($(shell_parse(str))))
end

## low-level calls

write(s::AsyncStream, b::ASCIIString) =
    ccall(:jl_puts, Int32, (Ptr{Uint8},Ptr{Void}),b.data,handle(s))
write(s::AsyncStream, b::Uint8) =
    ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b, handle(s))
write(s::AsyncStream, c::Char) =
    ccall(:jl_pututf8, Int32, (Ptr{Void},Char), handle(s), c)
write{T<:BitsKind}(s::AsyncStream, a::Array{T}) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint32),handle(s), a, uint(numel(a)*sizeof(T)))
write(s::AsyncStream, p::Ptr, nb::Integer) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),handle(s), p, uint(nb))
_write(s::AsyncStream, p::Ptr{Void}, nb::Integer) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),handle(s),p,uint(nb))

(&)(left::AbstractCmd,right::AbstractCmd) = AndCmds(left,right)
(|)(src::AbstractCmd,dest::AbstractCmd) = OrCmds(src,dest)
(<)(left::AbstractCmd,right::UVStream) = StreamRedirect(left,right,STDIN_NO)
(>)(left::AbstractCmd,right::UVStream) = StreamRedirect(left,right,STDOUT_NO)


function each_line(cmd::AbstractCmd,stdin)
    out = NamedPipe()
    processes = spawn(false, cmd, (stdin,out,STDERR))
    EachLine(out)
end
each_line(cmd::AbstractCmd) = each_line(cmd,SpawnNullStream())

function show(io, cmd::Cmd)
    if isa(cmd.exec,Vector{ByteString})
        esc = shell_escape(cmd.exec...)
        print(io,'`')
        for c in esc
            if c == '`'
                print(io,'\\')
            end
            print(io,c)
        end
        print(io,'`')
    else
        print(io, cmd.exec)
    end
end

function show(io, cmds::OrCmds)
    if isa(cmds.a, AndCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" | ")
    if isa(cmds.b, AndCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

function show(io, cmds::AndCmds)
    if isa(cmds.a, OrCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" & ")
    if isa(cmds.b, OrCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void}) = ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void}),sock.handle,sockaddr)
_jl_getaddrinfo(loop::Ptr{Void},host::ByteString,service::Ptr{Void},cb::Function) = ccall(:jl_getaddrinfo,Int32,(Ptr{Void},Ptr{Uint8},Ptr{Uint8},Function),loop,host,service,cb)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr{Void}) = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr{Void},),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)
_uv_lasterror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)
_uv_lasterror() = _uv_lasterror(globalEventLoop())

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

function connect_to_host(host::ByteString,port::Uint16) #TODO: handle errors
    sock = TcpSocket()
    err = _jl_getaddrinfo(globalEventLoop(),host,C_NULL,
        (addrinfo::Ptr{Void},status::Int32) -> getaddrinfo_callback(sock,status,host,port,addrinfo))
    if(err!=0)
        error("Failed to  initilize request to resolve hostname: ",host)
    end
    wait_connected(sock)
    return sock
end

## UV based file operations ##

module FS
using Base

export File, open, close, unlink, write,
    JL_O_WRONLY, JL_O_RDONLY, JL_O_RDWR, JL_O_APPEND, JL_O_CREAT, JL_O_EXCL,
    JL_O_TRUNC, JL_O_TEMPORARY, JL_O_SHORT_LIVED, JL_O_SEQUENTIAL, JL_O_RANDOM

import Base.show, Base.open, Base.close, Base.write

include("file_constants.jl")

abstract AbstractFile <: IO

const _sizeof_uv_fs_t = ccall(:jl_sizeof_uv_fs_t,Int32,())

type File <: AbstractFile
    path::String
    open::Bool
    handle::Int32
    File(path::String) = new(path,false,-1)
end

type AsyncFile <: AbstractFile
    path::String
    open::Bool
end

#TODO proper error translation
uv_err_to_string(err::Int32) = string(err)
uv_error(err::Int32) = err != 0 ? error(uv_err_to_string(err)) : nothing

_uv_fs_result(req) = ccall(:jl_uv_fs_result,Int32,(Ptr{Void},),req)

function open(f::File,flags::Integer)
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_open,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Int32,Int32,Ptr{Void}),
                         globalEventLoop(),req,bytestring(f.path),flags,0,C_NULL)
    uv_error(err)
    f.handle = _uv_fs_result(req)
    f.open = true
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    f
end
open(f::String,flags::Integer) = open(File(string))

function close(f::File)
    if(!f.open)
        error("File is already closed")
    end
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_close,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Void}),
                         globalEventLoop(),req,f.handle,C_NULL)
    uv_error(err)
    f.handle = -1
    f.open = false
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    f
end

function unlink(p::String)
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_unlink,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Ptr{Void}),
                         globalEventLoop(),req,bytestring(p),C_NULL)
    uv_error(err)
end
function unlink(f::File)
    if(f.open)
        close(f)
    end
    unlink(f.path)
    f
end

function write(f::File,buf::Ptr{Uint8},len::Int32,offset::Int64)
    if(!f.open)
        error("File is not open")
    end
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_close,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Int32,Int64,Ptr{Void}),
                         globalEventLoop(),req,f.handle,buf,len,offset,C_NULL)
    uv_error(err)
    f
end

end

ccall(:jl_get_uv_hooks, Void, ())
