#TODO: Move stdio detection from C to Julia (might require some Clang magic)
include("uv_constants.jl")

## types ##
typealias Executable Union(Vector{ByteString},Function)
typealias Callback Union(Function,Bool)

abstract AsyncStream <: IO
abstract UVServer

typealias UVHandle Ptr{Void}
typealias UVStream AsyncStream

#Wrapper for an OS file descriptor (on both Unix and Windows)
immutable RawFD
    fd::Int32
    RawFD(fd::Integer) = new(int32(fd))
end

function uv_sizeof_handle(handle)
    if !(UV_UNKNOWN_HANDLE < handle < UV_HANDLE_TYPE_MAX)
        throw(DomainError())
    end
    ccall(:uv_handle_size,Csize_t,(Int32,),handle)
end

function uv_sizeof_req(req)
    if !(UV_UNKNOWN_REQ < req < UV_REQ_TYPE_MAX)
        throw(DomainError())
    end
    ccall(:uv_req_size,Csize_t,(Int32,),req)
end

for h in uv_handle_types
@eval const $(symbol("_sizeof_"*lowercase(string(h)))) = uv_sizeof_handle($h)
end
for r in uv_req_types
@eval const $(symbol("_sizeof_"*lowercase(string(r)))) = uv_sizeof_req($r)
end

function eof(s::AsyncStream)
    wait_readnb(s,1)
    !isopen(s) && nb_available(s.buffer)<=0
end

const StatusUninit      = 0 # handle is allocated, but not initialized
const StatusInit        = 1 # handle is valid, but not connected/active
const StatusConnecting  = 2 # handle is in process of connecting
const StatusOpen        = 3 # handle is usable
const StatusActive      = 4 # handle is listening for read/write/connect events
const StatusClosing     = 5 # handle is closing / being closed
const StatusClosed      = 6 # handle is closed
function uv_status_string(x)
    s = x.status
    if x.handle == C_NULL
        if s == StatusClosed
            return "closed"
        elseif s == StatusUninit
            return "null"
        end
        return "invalid status"
    elseif s == StatusUninit
        return "uninit"
    elseif s == StatusInit
        return "init"
    elseif s == StatusConnecting
        return "connecting"
    elseif s == StatusOpen
        return "open"
    elseif s == StatusActive
        return "active"
    elseif s == StatusClosing
        return "closing"
    elseif s == StatusClosed
        return "closed"
    end
    return "invalid status"
end

type NamedPipe <: AsyncStream
    handle::Ptr{Void}
    status::Int
    buffer::IOBuffer
    line_buffered::Bool
    readcb::Callback
    readnotify::Condition
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    NamedPipe(handle) = new(
        handle,
        StatusUninit,
        PipeBuffer(),
        true,
        false,Condition(),
        false,Condition(),
        false,Condition())
end
function NamedPipe()
    handle = malloc_pipe()
    try
        return init_pipe!(NamedPipe(handle);readable=true)
    catch
        c_free(handle)
        rethrow()
    end
end

type PipeServer <: UVServer
    handle::Ptr{Void}
    status::Int
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    PipeServer(handle) = new(
        handle,
        StatusUninit,
        false,Condition(),
        false,Condition())
end

function init_pipe!(pipe::Union(NamedPipe,PipeServer);readable::Bool=false,writeable=false,julia_only=true)
    if pipe.handle == C_NULL || pipe.status != StatusUninit
        error("Failed to initialize pipe")
    end
    if 0 != ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32,Any), pipe.handle, writeable,readable,julia_only,pipe) 
        throw(UVError("init_pipe"))
    end
    pipe.status = StatusInit
    pipe
end

function PipeServer()
    handle = malloc_pipe()
    try
        return init_pipe!(PipeServer(handle);readable=true)
    catch
        c_free(handle)
        rethrow()
    end
end

show(io::IO,stream::NamedPipe) = print(io,"NamedPipe(",uv_status_string(stream),", ",
    nb_available(stream.buffer)," bytes waiting)")
show(io::IO,stream::PipeServer) = print(io,"PipeServer(",uv_status_string(stream),")")

type TTY <: AsyncStream
    handle::Ptr{Void}
    status::Int
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Condition
    closecb::Callback
    closenotify::Condition
    TTY(handle) = new(
        handle,
        StatusUninit,
        true,
        PipeBuffer(),
        false,Condition(),
        false,Condition())
end

show(io::IO,stream::TTY) = print(io,"TTY(",uv_status_string(stream),", ",
    nb_available(stream.buffer)," bytes waiting)")

uvtype(::AsyncStream) = UV_STREAM
uvhandle(stream::AsyncStream) = stream.handle

convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)
handle(s::AsyncStream) = s.handle
handle(s::Ptr{Void}) = s

make_stdout_stream() = _uv_tty2tty(ccall(:jl_stdout_stream, Ptr{Void}, ()))

associate_julia_struct(handle::Ptr{Void},jlobj::ANY) = 
    ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},Any),handle,jlobj)
disassociate_julia_struct(handle::Ptr{Void}) = 
    ccall(:jl_uv_disassociate_julia_struct,Void,(Ptr{Void},),handle)

function init_stdio(handle,fd)
    t = ccall(:jl_uv_handle_type,Int32,(Ptr{Void},),handle)
    if t == UV_FILE
        return File(RawFD(fd))
    else
        if t == UV_TTY
            ret = TTY(handle)
        elseif t == UV_TCP
            ret = TcpSocket(handle)
        elseif t == UV_NAMED_PIPE
            ret = NamedPipe(handle)
        end
        ret.status = StatusOpen
        ret.line_buffered = false  
        ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},Any),ret.handle,ret)     
        return ret
    end
end

function reinit_stdio()
    global uv_jl_asynccb = cglobal(:jl_uv_asynccb)
    global uv_jl_alloc_buf = cglobal(:jl_uv_alloc_buf)
    global uv_jl_readcb = cglobal(:jl_uv_readcb)
    global uv_jl_connectioncb = cglobal(:jl_uv_connectioncb)
    global uv_jl_connectcb = cglobal(:jl_uv_connectcb)
    global uv_eventloop = ccall(:jl_global_event_loop, Ptr{Void}, ())
    global STDIN = init_stdio(ccall(:jl_stdin_stream ,Ptr{Void},()),0)
    global STDOUT = init_stdio(ccall(:jl_stdout_stream,Ptr{Void},()),1)
    global STDERR = init_stdio(ccall(:jl_stderr_stream,Ptr{Void},()),2)
end

flush(::TTY) = nothing

function isopen(x)
    assert(x.status != StatusUninit && x.status != StatusInit,
        "UV object is not in a valid state")
    x.status != StatusClosed
end

function wait_connected(x)
    assert(isopen(x), "UV object not in a valid state")
    while x.status == StatusConnecting
        wait(x.connectnotify)
        assert(isopen(x), "UV object not in a valid state")
    end
end

function wait_readbyte(x::AsyncStream, c::Uint8)
    while isopen(x) && search(x.buffer,c) <= 0
        start_reading(x)
        wait(x.readnotify)
    end
end

wait_readline(x) = wait_readbyte(x, uint8('\n'))

function wait_readnb(x::AsyncStream, nb::Int)
    while isopen(x) && nb_available(x.buffer) < nb
        start_reading(x)
        wait(x.readnotify)
    end
end

#from `connect`
function _uv_hook_connectcb(sock::AsyncStream, status::Int32)
    @assert sock.status == StatusConnecting
    if status != -1
        sock.status = StatusOpen
        err = ()
    else
        sock.status = StatusInit
        err = UVError("connect")
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock, status)
    end
    err===() ? notify(sock.connectnotify) : notify_error(sock.connectnotify, err)
end

#from `listen`
function _uv_hook_connectioncb(sock::UVServer, status::Int32)
    local err
    if status != -1
        err = ()
    else
        err = UVError("connection")
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock,status)
    end
    err===() ? notify(sock.connectnotify) : notify_error(sock.connectnotify, err)
end

## BUFFER ##
## Allocate a simple buffer
function alloc_request(buffer::IOBuffer, recommended_size::Int32)
    ensureroom(buffer, int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    return (pointer(buffer.data, ptr), length(buffer.data)-ptr+1)
end
function _uv_hook_alloc_buf(stream::AsyncStream, recommended_size::Int32)
    (buf,size) = alloc_request(stream.buffer, recommended_size)
    @assert size>0 # because libuv requires this (TODO: possibly stop reading too if it fails)
    (buf,int32(size))
end

function notify_filled(buffer::IOBuffer, nread::Int, base::Ptr{Void}, len::Int32)
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
            nreadable = (stream.line_buffered ? int(search(stream.buffer, '\n')) : nb_available(stream.buffer))
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
    if nread == -1
        if _uv_lasterror() != 1 #UV_EOF == 1
            err = UVError("readcb")
            close(stream)
            notify_error(stream.readnotify, err)
        else
            close(stream)
            notify(stream.readnotify)
        end
    else
        notify_filled(stream.buffer, nread, base, len)
        notify_filled(stream, nread)
        notify(stream.readnotify)
    end
end
##########################################
# Async Workers
##########################################

abstract AsyncWork

type SingleAsyncWork <: AsyncWork
    handle::Ptr{Void}
    cb::Function
    function SingleAsyncWork(cb::Function)
        this = new(c_malloc(_sizeof_uv_async), cb)
        err = ccall(:uv_async_init,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),eventloop(),this.handle,uv_jl_asynccb::Ptr{Void})
        associate_julia_struct(this.handle, this)
        this
    end
end

type IdleAsyncWork <: AsyncWork
    handle::Ptr{Void}
    cb::Function
    function IdleAsyncWork(cb::Function)
        this = new(c_malloc(_sizeof_uv_idle), cb)
        if 0 != ccall(:uv_idle_init,Cint,(Ptr{Void},Ptr{Void}),eventloop(),this)
            c_free(this.handle)
            this.handle = C_NULL
            error(UVError("uv_make_timer"))
        end
        finalizer(this,close)
        this
    end
end

type TimeoutAsyncWork <: AsyncWork
    handle::Ptr{Void}
    cb::Function
    function TimeoutAsyncWork(cb::Function)
        this = new(c_malloc(_sizeof_uv_timer), cb)
        if 0 != ccall(:uv_timer_init,Cint,(Ptr{Void},Ptr{Void}),eventloop(),this.handle)
            c_free(this.handle)
            this.handle = C_NULL
            error(UVError("uv_make_timer"))
        end
        finalizer(this,close)
        this
    end
end

close(t::TimeoutAsyncWork) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

function _uv_hook_close(uv::Union(AsyncStream,UVServer))
    uv.handle = 0
    uv.status = StatusClosed
    if isa(uv.closecb, Function)
        uv.closecb(uv)
    end
    notify(uv.closenotify)
    try notify(uv.readnotify) end
    try notify(uv.connectnotify) end
end
_uv_hook_close(uv::AsyncWork) = (uv.handle = C_NULL; nothing)

# This serves as a common callback for all async classes
function _uv_hook_asynccb(async::AsyncWork, status::Int32)
    if isa(async, TimeoutAsyncWork)
        if ccall(:uv_timer_get_repeat, Uint64, (Ptr{Void},), async.handle) == 0
            # timer is stopped now
            disassociate_julia_struct(async.handle) # we want gc to be able to cleanup
        end
    end
    try
        async.cb(async, status)
    catch err
        #bt = catch_backtrace()
        if isa(err, MethodError)
            warn_once("Async callbacks should take an AsyncWork object as the first argument")
            async.cb(status)
            return
        end
        rethrow(err)
    end
    nothing
end

function start_timer(timer::TimeoutAsyncWork, timeout::Real, repeat::Real)
    associate_julia_struct(timer.handle, timer) # we don't want gc to cleanup
    ccall(:uv_update_time,Void,(Ptr{Void},),eventloop())
    ccall(:uv_timer_start,Cint,(Ptr{Void},Ptr{Void},Uint64,Uint64),
        timer.handle, uv_jl_asynccb::Ptr{Void}, uint64(round(timeout*1000))+1, uint64(round(repeat*1000)))
end

function stop_timer(timer::TimeoutAsyncWork)
    ccall(:uv_timer_stop,Cint,(Ptr{Void},),timer.handle)
    disassociate_julia_struct(timer.handle) # we want gc to be able to cleanup
end

function sleep(sec::Real)
    w = Condition()
    timer = TimeoutAsyncWork((tmr,status)->notify(w, status!=0?UVError("timer"):()))
    start_timer(timer, float(sec), 0)
    try
        wait(w)
    finally
        stop_timer(timer)
    end
    nothing
end

function add_idle_cb(cb::Function)
    work = IdleAsyncWork(cb)
    associate_julia_struct(work.handle, work) # we don't want gc to cleanup
    ccall(:uv_idle_start,Cint,(Ptr{Void},Ptr{Void}),work.handle,uv_jl_asynccb::Ptr{Void})
    work
end

function queueAsync(work::SingleAsyncWork)
    ccall(:uv_async_send,Cint,(Ptr{Void},),work.handle)
end

## event loop ##
eventloop() = global uv_eventloop::Ptr{Void}
#mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},()) # this would probably be fine, but is nowhere supported

function run_event_loop()
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),eventloop())
end
function process_events(block::Bool)
    loop = eventloop()
    if block
        ccall(:jl_run_once,Int32,(Ptr{Void},),loop)
    else
        ccall(:jl_process_events,Int32,(Ptr{Void},),loop)
    end
end

## pipe functions ##
malloc_pipe() = c_malloc(_sizeof_uv_named_pipe)
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool,pipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    #TODO: this is probably not freeing memory properly after errors
    if 0 != ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32,Any), read_end, 0, 1, readable_julia_only, pipe)
        error(UVError("init_pipe"))
    end
    if 0 != ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32,Any), write_end, 1, 0, readable_julia_only, pipe)
        error(UVError("init_pipe"))
    end
    if 0 != ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end)
        error(UVError("uv_pipe_link"))
    end
end

function link_pipe(read_end2::NamedPipe,readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool)
    if read_end2.handle == C_NULL
        read_end2.handle = malloc_pipe()
    end
    link_pipe(read_end2.handle,readable_julia_only,write_end,writeable_julia_only,read_end2)
    read_end2.status = StatusOpen
end
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::NamedPipe,writeable_julia_only::Bool)
    if write_end.handle == C_NULL
        write_end.handle = malloc_pipe()
    end
    link_pipe(read_end,readable_julia_only,write_end.handle,writeable_julia_only,write_end)
    write_end.status = StatusOpen
end
close_pipe_sync(handle::UVHandle) = ccall(:uv_pipe_close_sync,Void,(UVHandle,),handle)

_uv_hook_isopen(stream::Union(AsyncStream,UVServer)) = int32(stream.status!=StatusUninit && stream.status!=StatusClosed && stream.status!=StatusClosing)

function close(stream::Union(AsyncStream,UVServer))
    if bool(_uv_hook_isopen(stream))
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.status = StatusClosing
    end
    nothing
end

## stream functions ##
function start_reading(stream::AsyncStream)
    if stream.status == StatusOpen
        ret = ccall(:uv_read_start,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),
            handle(stream),uv_jl_alloc_buf::Ptr{Void},uv_jl_readcb::Ptr{Void})
        stream.status = StatusActive
        ret
    elseif stream.status == StatusActive
        int32(0)
    else
        int32(-1)
    end
end
function start_reading(stream::AsyncStream, cb::Function)
    start_reading(stream)
    stream.readcb = cb
    nread = nb_available(stream.buffer)
    if nread > 0
        notify_filled(stream, nread)
    end
    nothing
end
start_reading(stream::AsyncStream, cb::Bool) = (start_reading(stream); stream.readcb = cb; nothing)

function stop_reading(stream::AsyncStream)
    if stream.status == StatusActive
        ret = ccall(:uv_read_stop,Cint,(Ptr{Void},),stream.handle)
        stream.status = StatusOpen
        ret
    elseif stream.status == StatusOpen
        int32(0)
    else
        int32(-1)
    end
end

function readall(stream::AsyncStream)
    start_reading(stream)
    wait_close(stream)
    return takebuf_string(stream.buffer)
end

function read{T}(this::AsyncStream, a::Array{T})
    assert(isbits(T),"Read from Buffer only supports bits types or arrays of bits types")
    nb = length(a)*sizeof(T)
    buf = this.buffer
    @assert buf.seekable == false
    @assert buf.maxsize >= nb
    start_reading(this)
    wait_readnb(this,nb)
    read(this.buffer, a)
    return a
end

function read(this::AsyncStream,::Type{Uint8})
    buf = this.buffer
    @assert buf.seekable == false
    wait_readnb(this,1)
    read(buf,Uint8)
end

function readline(this::AsyncStream)
    buf = this.buffer
    @assert buf.seekable == false
    wait_readline(this)
    readline(buf)
end

function readavailable(this::AsyncStream)
    buf = this.buffer
    @assert buf.seekable == false
    wait_readnb(this,1)
    takebuf_string(buf)
end

function readuntil(this::AsyncStream,c::Uint8)
    buf = this.buffer
    @assert buf.seekable == false
    wait_readbyte(this,c)
    readuntil(buf,c)
end

#function finish_read(pipe::NamedPipe)
#    close(pipe) #handles to UV and ios will be invalid after this point
#end
#
#function finish_read(state::(NamedPipe,ByteString))
#    finish_read(state...)
#end


## low-level calls ##

function write(s::AsyncStream, b::Uint8)
    assert(isopen(s),"UV object is not in a valid state")
    int(ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b, handle(s)))
end
function write(s::AsyncStream, c::Char)
    assert(isopen(s),"UV object is not in a valid state")
    int(ccall(:jl_pututf8, Int32, (Ptr{Void},Uint32), handle(s), c))
end
function write{T}(s::AsyncStream, a::Array{T})
    assert(isopen(s),"UV object is not in a valid state")
    if isbits(T)
        int(ccall(:jl_write, Uint, (Ptr{Void}, Ptr{Void}, Uint), handle(s), a, uint(length(a)*sizeof(T))))
    else
        invoke(write,(IO,Array),s,a)
    end
end
function write(s::AsyncStream, p::Ptr, nb::Integer)
    assert(isopen(s),"UV object is not in a valid state")
    int(ccall(:jl_write, Uint, (Ptr{Void},Ptr{Void},Uint), handle(s), p, uint(nb)))
end
function _write(s::AsyncStream, p::Ptr{Void}, nb::Integer)
    assert(isopen(s),"UV object is not in a valid state")
    int(ccall(:jl_write, Uint, (Ptr{Void},Ptr{Void},Uint), handle(s), p, uint(nb)))
end

function _uv_hook_writecb(s::AsyncStream,status::Int32) 
    if status == -1
        close(s)
    end
end

## Libuv error handling ##
_uv_lasterror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)
_uv_lasterror() = _uv_lasterror(eventloop())
_uv_lastsystemerror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)
_uv_lastsystemerror() = _uv_lasterror(eventloop())

type UV_error_t
    uv_code::Int32
    system_code::Int32
end
type UVError <: Exception
    prefix::String
    s::UV_error_t
    UVError(p::String,e::UV_error_t)=new(p,e)
end
UVError(p::String) = UVError(p,_uv_lasterror(),_uv_lastsystemerror())
UVError(p::String,uv::Integer,system::Integer) = UVError(p,UV_error_t(uv,system))

struverror(err::UVError) = bytestring(ccall(:jl_uv_strerror,Ptr{Uint8},(Int32,Int32),err.s.uv_code,err.s.system_code))
uverrorname(err::UVError) = bytestring(ccall(:jl_uv_err_name,Ptr{Uint8},(Int32,Int32),err.s.uv_code,err.s.system_code))

uv_error(prefix, e::UV_error_t) = e.uv_code != 0 ? throw(UVError(string(prefix),e)) : nothing
uv_error(prefix, b::Bool) = b ? throw(UVError(string(prefix))) : nothing
uv_error(prefix) = uv_error(prefix, _uv_lasterror() != 0)

show(io::IO, e::UVError) = print(io, e.prefix*": "*struverror(e)*" ("*uverrorname(e)*")")


## server functions ##

function accept_nonblock(server::PipeServer,client::NamedPipe)
    err = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server.handle,client.handle)
    if err == 0
        client.status = StatusOpen
    end
    err
end
function accept_nonblock(server::PipeServer)
    client = NamedPipe()
    uv_error("accept", accept_nonblock(server,client) != 0)
    client
end

const UV_EAGAIN = 4
function accept(server::UVServer, client::AsyncStream)
    if server.status != StatusActive 
        error("accept: Server not connected. Did you `listen`?")
    end
    @assert client.status == StatusInit
    while true
        if accept_nonblock(server,client) == 0
            return client
        elseif _uv_lasterror() != UV_EAGAIN
            uv_error("accept")
        end
        wait(server.connectnotify)
    end
end

const BACKLOG_DEFAULT = 511

function listen!(sock::UVServer; backlog::Integer=BACKLOG_DEFAULT)
    if 0 == ccall(:uv_listen, Cint, (Ptr{Void}, Cint, Ptr{Void}),
        sock.handle, backlog, uv_jl_connectioncb::Ptr{Void})
        sock.status = StatusActive
        true
    else
        false
    end
end

function bind(server::PipeServer, name::ASCIIString)
    @assert server.status == StatusInit
    if 0 != ccall(:uv_pipe_bind, Int32, (Ptr{Void}, Ptr{Uint8}),
            server.handle, name)
        if (err=_uv_lasterror()) != UV_EADDRINUSE && err != UV_EACCES
            error(UVError("bind"))
        else
            return false
        end
    end
    server.status = StatusOpen
    true
end


function listen(path::ByteString)
    sock = PipeServer()
    uv_error("listen", !bind(sock, path))
    uv_error("listen", !listen!(sock))
    sock
end

function connect!(sock::NamedPipe,path::ByteString)
    @assert sock.status == StatusInit
    req = c_malloc(_sizeof_uv_connect)
    ccall(:uv_pipe_connect, Void, (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Ptr{Void}), req, sock.handle, path, uv_jl_connectcb::Ptr{Void})
    sock.status = StatusConnecting
    sock
end

function connect(cb::Function,sock::AsyncStream,args...)
    sock.ccb = cb
    connect!(sock,args...)
end

function connect(cb::Function,args...)
    sock.ccb = cb
    connect(args...)
end

function connect(sock::AsyncStream, args...)
    connect!(sock,args...)
    wait_connected(sock)
    sock
end


connect(path::ByteString) = connect(NamedPipe(),path)
