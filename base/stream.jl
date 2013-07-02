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
    start_reading(s)
    wait_readnb(s,1)
    !s.open && nb_available(s.buffer)<=0
end

type NamedPipe <: AsyncStream
    handle::Ptr{Void}
    open::Bool
    buffer::IOBuffer
    line_buffered::Bool
    readcb::Callback
    readnotify::Condition
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    NamedPipe(handle, open) = new(handle,open,PipeBuffer(),true,false,Condition(),false,
                      Condition(),false,Condition())
    NamedPipe() = NamedPipe(C_NULL,false)
end

type PipeServer <: UVServer
    handle::Ptr{Void}
    open::Bool
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    PipeServer(handle,open) = new(handle,open,false,Condition(),false,Condition())
    function PipeServer()
        this = PipeServer(malloc_pipe(),false)
        ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Int32,Int32,Any), this.handle, 0, 1, this)
        if this.handle == C_NULL
            throw(UVError("Failed to create pipe server"))
        end
        this
    end
end

show(io::IO,stream::NamedPipe) = print(io,"NamedPipe(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")
show(io::IO,stream::PipeServer) = print(io,"PipeServer(",stream.open?"listening)":"not listening)")

type TTY <: AsyncStream
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Condition
    closecb::Callback
    closenotify::Condition
    TTY(handle,open)=new(handle,open,true,PipeBuffer(),false,Condition(),false,Condition())
end

show(io::IO,stream::TTY) = print(io,"TTY(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

uvtype(::AsyncStream) = UV_STREAM
uvhandle(stream::AsyncStream) = stream.handle

copy(s::TTY) = TTY(s.handle,s.open)

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
            ret = TTY(handle,true)
        elseif t == UV_TCP
            ret = TcpSocket(handle,true)
        elseif t == UV_NAMED_PIPE
            ret = NamedPipe(handle, true)
        end
        ret.line_buffered = false  
        ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},Any),ret.handle,ret)     
        return ret
    end
end

function reinit_stdio()
    global STDIN, STDERR, STDOUT
    STDIN = init_stdio(ccall(:jl_stdin_stream ,Ptr{Void},()),0)
    STDOUT = init_stdio(ccall(:jl_stdout_stream,Ptr{Void},()),1)
    STDERR = init_stdio(ccall(:jl_stderr_stream,Ptr{Void},()),2)
end

flush(::TTY) = nothing

function wait_connected(x)
    while !x.open
        err = wait(x.connectnotify)
        if err.uv_code != 0
            throw(UVError("connect",err))
        end
    end
end

function wait_readbyte(x::AsyncStream, c::Uint8)
    while !(!x.open || (search(x.buffer,c) > 0))
        wait(x.readnotify)
    end
end

wait_readline(x) = wait_readbyte(x, uint8('\n'))

function wait_readnb(x::AsyncStream, nb::Int)
    while !(!x.open || (nb_available(x.buffer) >= nb))
        wait(x.readnotify)
    end
end

#from `connect`
function _uv_hook_connectcb(sock::AsyncStream, status::Int32)
    if status != -1
        sock.open = true
        err = UV_error_t(int32(0),int32(0))
    else
        err = UV_error_t(_uv_lasterror(),_uv_lastsystemerror())
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock, status)
    end
    notify(sock.connectnotify, err)
end

#from `listen`
function _uv_hook_connectioncb(sock::UVServer, status::Int32)
    local err
    if status != -1
        sock.open = true
        err = UV_error_t(int32(0),int32(0))
    else
        err = UV_error_t(_uv_lasterror(),_uv_lastsystemerror())
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock,status)
    end
    notify(sock.connectnotify, err)
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
    assert(size>0) # because libuv requires this (TODO: possibly stop reading too if it fails)
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
           error = UVError("readcb")
           close(stream)
           throw(error)
        end
        close(stream)
        notify(stream.readnotify)
        #EOF
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
    cb::Function
    handle::Ptr{Void}
    function SingleAsyncWork(loop::Ptr{Void},cb::Function)
        if loop == C_NULL
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=ccall(:jl_make_async,Ptr{Void},(Ptr{Void},Any),loop,this)
        finalizer(this,close)
        this
    end
end
SingleAsyncWork(cb::Function) = SingleAsyncWork(eventloop(),cb)

type IdleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function IdleAsyncWork(loop::Ptr{Void},cb::Function)
        this=new(cb)
        this.handle=ccall(:jl_make_idle,Ptr{Void},(Ptr{Void},Any),loop,this)
        finalizer(this,close)
        this
    end
end
IdleAsyncWork(cb::Function) = IdleAsyncWork(eventloop(),cb)

type TimeoutAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function TimeoutAsyncWork(loop::Ptr{Void},cb::Function)
        this=new(cb)
        this.handle=ccall(:jl_make_timer,Ptr{Void},(Ptr{Void},Any),loop,this)
        finalizer(this,close)
        this
    end
end
TimeoutAsyncWork(cb::Function) = TimeoutAsyncWork(eventloop(),cb)

close(t::TimeoutAsyncWork) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)



function _uv_hook_close(uv::Union(AsyncStream,UVServer))
    uv.handle = 0
    uv.open = false
    if isa(uv.closecb, Function) uv.closecb(uv) end
    notify(uv.closenotify)
end
_uv_hook_close(uv::AsyncWork) = (uv.handle = 0; nothing)

# This serves as a common callback for all async classes
_uv_hook_asynccb(async::AsyncWork, status::Int32) = async.cb(status)


function start_timer(timer::TimeoutAsyncWork,timeout::Int64,repeat::Int64)
    associate_julia_struct(timer.handle,timer)
    ccall(:uv_update_time,Void,(Ptr{Void},),eventloop())
    ccall(:jl_timer_start,Int32,(Ptr{Void},Int64,Int64),timer.handle,timeout+1,repeat)
end

function stop_timer(timer::TimeoutAsyncWork)
    disassociate_julia_struct(timer.handle)
    ccall(:uv_timer_stop,Int32,(Ptr{Void},),timer.handle)
end

function sleep(sec::Real)
    w = Condition()
    timer = TimeoutAsyncWork(status->notify(w, status))
    start_timer(timer, int64(iround(sec*1000)), int64(0))
    local st
    try
        st = wait(w)
    finally
        stop_timer(timer)
    end
    if st != 0
        error("timer error")
    end
    nothing
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
eventloop() = ccall(:jl_global_event_loop,Ptr{Void},())
#mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},()) # this would be fine, but is nowhere supported

function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
function process_events(block::Bool,loop::Ptr{Void})
    if block
        ccall(:jl_run_once,Int32,(Ptr{Void},),loop)
    else
        ccall(:jl_process_events,Int32,(Ptr{Void},),loop)        
    end
end
process_events(block::Bool) = process_events(block,eventloop())
run_event_loop() = run_event_loop(eventloop())

##pipe functions
malloc_pipe() = c_malloc(_sizeof_uv_named_pipe)
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool,pipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Int32,Int32,Any), read_end, 0, readable_julia_only, pipe)
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Int32,Int32,Any), write_end, 1, readable_julia_only, pipe)
    error = ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end)
    if error != 0 # don't use assert here as $string isn't be defined yet
        error("uv_pipe_link failed")
    end
end

function link_pipe(read_end2::NamedPipe,readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool)
    if read_end2.handle == C_NULL
        read_end2.handle = malloc_pipe()
    end
    link_pipe(read_end2.handle,readable_julia_only,write_end,writeable_julia_only,read_end2)
    read_end2.open = true
end
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::NamedPipe,writeable_julia_only::Bool)
    if write_end.handle == C_NULL
        write_end.handle = malloc_pipe()
    end
    link_pipe(read_end,readable_julia_only,write_end.handle,writeable_julia_only,write_end)
    write_end.open = true
end
close_pipe_sync(handle::UVHandle) = ccall(:uv_pipe_close_sync,Void,(UVHandle,),handle)

function isopen(stream::Union(AsyncStream,UVServer))
    stream.open
end

_uv_hook_isopen(stream::Union(AsyncStream,UVServer)) = int32(isopen(stream))

function close(stream::Union(AsyncStream,UVServer))
    if stream.open
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.open = false
    end
end

##stream functions

start_reading(stream::AsyncStream) = (stream.handle != C_NULL ? ccall(:jl_start_reading,Int32,(Ptr{Void},),handle(stream)) : int32(0))
function start_reading(stream::AsyncStream,cb::Function)
    start_reading(stream)
    stream.readcb = cb
    nread = nb_available(stream.buffer)
    if nread > 0
        notify_filled(stream,nread)
    end
end
start_reading(stream::AsyncStream,cb::Bool) = (start_reading(stream); stream.readcb = cb)

stop_reading(stream::AsyncStream) = ccall(:uv_read_stop,Int32,(Ptr{Void},),handle(stream))

function readall(stream::AsyncStream)
    start_reading(stream)
    wait_close(stream)
    return takebuf_string(stream.buffer)
end

function read{T}(this::AsyncStream, a::Array{T})
    if isbits(T)
        nb = length(a)*sizeof(T)
        buf = this.buffer
        assert(buf.seekable == false)
        assert(buf.maxsize >= nb)
        start_reading(this)
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
    start_reading(this)
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

function readavailable(this::AsyncStream)
    buf = this.buffer
    assert(buf.seekable == false)
    start_reading(this)
    wait_readnb(this,1)
    takebuf_string(buf)
end

function readuntil(this::AsyncStream,c::Uint8)
    buf = this.buffer
    assert(buf.seekable == false)
    start_reading(this)
    wait_readbyte(this,c)
    readuntil(buf,c)
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
    int(ccall(:jl_pututf8, Int32, (Ptr{Void},Uint32), handle(s), c))
function write{T}(s::AsyncStream, a::Array{T})
    if isbits(T)
        int(ccall(:jl_write, Uint, (Ptr{Void}, Ptr{Void}, Uint), handle(s), a, uint(length(a)*sizeof(T))))
    else
        invoke(write,(IO,Array),s,a)
    end
end
write(s::AsyncStream, p::Ptr, nb::Integer) = 
    int(ccall(:jl_write, Uint, (Ptr{Void},Ptr{Void},Uint), handle(s), p, uint(nb)))
_write(s::AsyncStream, p::Ptr{Void}, nb::Integer) = 
    int(ccall(:jl_write, Uint, (Ptr{Void},Ptr{Void},Uint), handle(s), p, uint(nb)))

## Libuv error handling
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

_jl_pipe_accept(server::Ptr{Void},client::Ptr{Void}) =
    ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
function accept_nonblock(server::PipeServer,client::NamedPipe)
    err = _jl_pipe_accept(server.handle,client.handle)
    if err == 0
        client.open = true
    end
    err
end
function accept_nonblock(server::PipeServer)
    client = NamedPipe()
    uv_error("accept",_jl_pipe_accept(server.handle,client.handle) == -1)
    client.open = true
    client
end

const UV_EAGAIN = 4
function accept(server::UVServer, client::AsyncStream)
    if !server.open
        error("accept: Server not connected. Did you `listen`?")
    end
    while true
        if accept_nonblock(server,client) == 0
            return client
        else
            uv_error("accept:",_uv_lasterror()!=UV_EAGAIN)
        end
        err = wait(server.connectnotify)
        if err.uv_code == -1
            throw(UVError("accept",err))
        end
    end
end

const BACKLOG_DEFAULT = 511

function listen!(sock::UVServer; backlog::Integer=BACKLOG_DEFAULT)
    err = ccall(:jl_listen, Int32, (Ptr{Void}, Int32), sock.handle, backlog)
    err != -1 ? (sock.open = true): false
end

function listen(path::ASCIIString)
    sock = PipeServer()
    uv_error("listen",bind(sock, path))
    uv_error("listen",!listen!(sock))
    sock
end

function connect(cb::Function,sock::NamedPipe,path::ASCIIString)
    sock.ccb = cb
    error("Unimplemented on this branch")
end
