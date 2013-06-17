#TODO: Move stdio detection from C to Julia (might require some Clang magic)
include("uv_constants.jl")

empty_callback(args...) = nothing

## types ##
typealias Executable Union(Vector{ByteString},Function)
typealias Callback Function

abstract AsyncStream <: IO
abstract UVServer

typealias UVHandle Ptr{Void}
typealias UVStream AsyncStream

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

function default_close_cb(handle)
    data = uv_handle_data(handle)
    if data != C_NULL
        close_cb(unsafe_pointer_to_objref(data))
    end
    c_free(handle)
    nothing
end

function _close(t::Union(AsyncStream,UVServer)) 
    if ccall(:uv_is_closing,Int32,(Ptr{Void},),t.handle) == 1
        return nothing
    end
    ccall(:uv_close,Void,(Ptr{Void},Ptr{Void}),t.handle,
        cfunction(default_close_cb,Void,(Ptr{Void},)))
    t.open = false
    nothing
end
close(t::Union(AsyncStream,UVServer)) =_close(t) 

function eof(s::AsyncStream)
    start_reading(s)
    wait_readnb(s,1)
    !s.open && nb_available(s.buffer)<=0
end

type NamedPipe <: AsyncStream
    handle::Ptr{Void}
    buffer::IOBuffer
    open::Bool
    line_buffered::Bool
    readcb::Callback
    readnotify::Condition
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    NamedPipe(handle::Ptr{Void},open::Bool) = new(handle,PipeBuffer(),open,true,empty_callback,Condition(),empty_callback,
                      Condition(),empty_callback,Condition())
    function NamedPipe(args...)
        this = NamedPipe(malloc_pipe(),false)
        associate_julia_struct(this.handle,this)
        if this.handle == C_NULL
            throw(UVError("Failed to create named pipe"))
        end
        init_pipe(this.handle,args...)
        this
    end
    NamedPipe() = NamedPipe(C_NULL,false)
end

type PipeServer <: UVServer
    handle::Ptr{Void}
    open::Bool
    ccb::Callback
    connectnotify::Condition
    closecb::Callback
    closenotify::Condition
    PipeServer(handle,open) = new(handle,open,empty_callback,Condition(),empty_callback,Condition())
    function PipeServer()
        this = PipeServer(malloc_pipe(),false)
        associate_julia_struct(this.handle,this)
        if this.handle == C_NULL
            throw(UVError("Failed to create pipe server"))
        end
        init_pipe(this.handle,false,true)
        this
    end
end

function  default_shutdown_cb(req,status)
    _close(unsafe_pointer_to_objref(uv_req_data(req)))
    nothing
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
    TTY(handle,open)=new(handle,open,true,PipeBuffer(),empty_callback,Condition(),empty_callback,Condition())
end

show(io::IO,stream::TTY) = print(io,"TTY(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")


function default_fm_cb(handle,filename,events,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        fdw = unsafe_pointer_to_objref(data)::FileMonitor
        fdw.cb(filename,status,events)
    end
    nothing
end

type FileMonitor
    handle::Ptr{Void}
    cb::Callback
    function FileMonitor(cb, file)
        handle = c_malloc(_sizeof_uv_fs_events)
        err = ccall(:uv_fs_event_init,Int32, (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32), 
            eventloop(),handle,file,cfunction(default_fm_cb,Void,(Ptr{Void},Ptr{Void},Int32,Int32)),0)
        if err == -1
            c_free(handle)
            throw(UVError("FileMonitor"))
        end
        this = new(handle,cb)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this        
    end
    FileMonitor(file) = FileMonitor(empty_callback,file)
end

const UV_READABLE = 1
const UV_WRITEABLE = 2

#Wrapper for an OS file descriptor (on both Unix and Windows)
immutable OS_FD
    fd::Int32
end

convert(::Type{Int32},fd::OS_FD) = fd.fd 

#Wrapper for an OS file descriptor (for Windows)
@windows_only immutable OS_SOCKET
    handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows...
end

abstract UVPollingWatcher

type PollingFileWatcher <: UVPollingWatcher
    handle::Ptr{Void}
    file::ASCIIString
    cb::Callback
    function PollingFileWatcher(cb, file)
        handle = c_malloc(_sizeof_uv_fs_poll)
        err = ccall(:uv_fs_poll_init,Int32,(Ptr{Void},Ptr{Void}),eventloop(),handle)
        if err == -1
            c_free(handle)
            throw(UVError("PollingFileWatcher"))
        end
        this = new(handle, file, cb)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this
    end  
    PollingFileWatcher(file) =  PollingFileWatcher(empty_callback,file)
end

type FDWatcher <: UVPollingWatcher
    handle::Ptr{Void}
    cb::Callback
    function FDWatcher(fd::OS_FD)
        handle = c_malloc(_sizeof_uv_poll)
        err = ccall(:uv_poll_init,Int32,(Ptr{Void},Ptr{Void},Int32),eventloop(),handle,fd.fd)
        if err == -1
            c_free(handle)
            throw(UVError("FDWatcher"))
        end
        this = new(handle,empty_callback)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this
    end
    @windows_only function FDWatcher(fd::OS_SOCKET)
        handle = c_malloc(_sizeof_uv_poll)
        err = ccall(:uv_poll_init_socket,Int32,(Ptr{Void},   Ptr{Void}, Ptr{Void}),
                                                eventloop(), handle,    fd.handle)
        if err == -1
            c_free(handle)
            throw(UVError("FDWatcher"))
        end
        this = new(handle,empty_callback)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this
    end
end

function default_fdw_cb(handle,status,events)
    data = uv_handle_data(handle)
    if data != C_NULL
        fdw = unsafe_pointer_to_objref(data)::FDWatcher
        fdw.cb(status,events)
    end
    nothing
end

function start_watching(t::FDWatcher, events)
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (FD)",
        ccall(:uv_poll_start,Int32,(Ptr{Void},Int32),t.handle,events,
            cfunction(default_fdw_cb,Void,(Ptr{Void},Int32,Int32)))==-1)
end
start_watching(f::Function, t::FDWatcher, events) = (t.cb = f; start_watching(t,events))

function default_pfw_cb(handle,status,prev,cur)
    data = uv_handle_data(handle)
    if data != C_NULL
        fdw = unsafe_pointer_to_objref(data)::PollingFileWatcher
        fdw.cb(status,prev,cur)
    end
    nothing
end

function start_watching(t::PollingFileWatcher, interval)
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (File)",
        ccall(:uv_fs_poll_start,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Uint32),
            cfunction(default_fdw_cb,Void,(Ptr{Void},Int32,Ptr{Void},Ptr{Void})),
                t.handle,t.file,interval)==-1)
end
start_watching(f::Function, t::PollingFileWatcher, interval) = (t.cb = f;start_watching(t,interval))

function stop_watching(t::FDWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (FD)",
        ccall(:uv_poll_stop,Int32,(Ptr{Void},),t.handle)==-1)
end

function stop_watching(t::PollingFileWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (File)",
        ccall(:uv_fs_poll_stop,Int32,(Ptr{Void},),t.handle)==-1)
end

close_cb(uv::FileMonitor) = (uv.handle = 0; nothing)
close_cb(uv::UVPollingWatcher) = (uv.handle = 0; nothing)

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

function _uv_tty2tty(handle::Ptr{Void})
    tty = TTY(handle,true)
    tty.line_buffered = false
    associate_julia_struct(handle,tty)
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

function _init_buf(stream::AsyncStream)
    if(!isa(stream.buf,IOStream))
        stream.buf=memio()
    end
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

function read_error(err)
    if err.uv_code == 0 || err.uv_code == 1 #EOF
        return nothing
    else
        throw(UVError("read",err))
    end
end

function wait_readbyte(x::AsyncStream, c::Uint8)
    while !(!x.open || (search(x.buffer,c) > 0))
        read_error(wait(x.readnotify))
    end
end

wait_readline(x) = wait_readbyte(x, uint8('\n'))

function wait_readnb(x::AsyncStream, nb::Int)
    while !(!x.open || (nb_available(x.buffer) >= nb))
        read_error(wait(x.readnotify))
    end
end

## BUFFER ##
## Allocate a simple buffer
function alloc_request(buffer::IOBuffer, recommended_size::Csize_t)
    ensureroom(buffer, int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    return (pointer(buffer.data, ptr), length(buffer.data)-ptr+1)
end

function notify_filled(buffer::IOBuffer, nread::Int, base::Ptr, len::Csize_t)
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
                if !isa(more,Bool)
                    more = false
                end
            else
                more = false
            end
        else
            more = false
        end
    end
end

##########################################
# Async Workers
##########################################

abstract AsyncWork

uv_handle_data(handle) = ccall(:jl_uv_handle_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_data(handle) = ccall(:jl_uv_req_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_set_data(req,data) = ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Any),req,data)

function default_async_cb(handle,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        async = unsafe_pointer_to_objref(data)::SingleAsyncWork
        async.cb(status)
    end
    nothing
end

type SingleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function SingleAsyncWork(loop::Ptr{Void},cb::Function)
        if(loop == C_NULL)
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=c_malloc(_sizeof_uv_async)
        err=ccall(:uv_async_init,Int32,(Ptr{Void},Ptr{Void},Ptr{Void}),loop,this.handle,
                cfunction(default_async_cb,Void,(Ptr{Void},Int32)))
        if err==-1
            c_free(this.handle)
            throw(UVError("SingleAsyncWork"))
        end
        associate_julia_struct(this.handle,this)
        finalizer(this,close)
        this
    end
end
SingleAsyncWork(cb::Function) = SingleAsyncWork(eventloop(),cb)

function default_idle_cb(handle,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        async = unsafe_pointer_to_objref(data)::IdleAsyncWork
        async.cb(status)
    end
    nothing
end

type IdleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function IdleAsyncWork(loop::Ptr{Void},cb::Function)
        if(loop == C_NULL)
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=c_malloc(_sizeof_uv_idle)
        err=ccall(:uv_idle_init,Int32,(Ptr{Void},Ptr{Void}),loop,this.handle)
        if err==-1
            c_free(this.handle)
            throw(UVError("IdleAsyncWork"))
        end
        disassociate_julia_struct(this.handle) #Will be set by start
        finalizer(this,close)
        this
    end
end
IdleAsyncWork(cb::Function) = IdleAsyncWork(eventloop(),cb)

type TimeoutAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function TimeoutAsyncWork(loop::Ptr{Void},cb::Function)
        if(loop == C_NULL)
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=c_malloc(_sizeof_uv_timer)
        err=ccall(:uv_timer_init,Int32,(Ptr{Void},Ptr{Void}),loop,this.handle)
        if err==-1
            c_free(this.handle)
            throw(UVError("TimeoutAsyncWork"))
        end
        disassociate_julia_struct(this.handle) #Will be set by start
        finalizer(this,close)
        this
    end
end
TimeoutAsyncWork(cb::Function) = TimeoutAsyncWork(eventloop(),cb)

function poll_fd(s, events::Integer, timeout_ms::Integer)
    wt = Condition()

    fdw = FDWatcher(s)
    start_watching((status, events) -> notify(wt, (:poll, status, events)), fdw, events)
    
    if (timeout_ms > 0)
        timer = TimeoutAsyncWork(status -> notify(wt, (:timeout, status)))
        start_timer(timer, int64(timeout_ms), int64(0))
    end

    local args
    try
        args = wait(wt)
    finally
        if (timeout_ms > 0) stop_timer(timer) end
        stop_watching(fdw)
    end

    if (args[2] == 0)
        if (args[1] == :poll) return args[3] end
        if (args[1] == :timeout) return 0 end
    end

    error("Error while polling") 
end

function poll_file(s, interval::Integer, timeout_ms::Integer)
    wt = Condition()

    pfw = PollingFileWatcher(s)
    start_watching((status,prev,cur) -> notify(wt, (:poll, status)), pfw, interval)
    
    if (timeout_ms > 0)
        timer = TimeoutAsyncWork(status -> notify(wt, (:timeout, status)))
        start_timer(timer, int64(timeout_ms), int64(0))
    end

    local args
    try
        args = wait(wt)
    finally
        if (timeout_ms > 0) stop_timer(timer) end
        stop_watching(pfw)
    end

    if (args[2] == 0)
        if (args[1] == :poll) return 1 end
        if (args[1] == :timeout) return 0 end
    end

    error("error while polling")
end

function watch_file(cb, s; poll=false)
    if poll
        pfw = PollingFileWatcher(cb,s)
        start_watching(pfw)
        return pfw
    else 
        return FileMonitor(cb,s)
    end
end

function close_cb(uv::Union(AsyncStream,UVServer))
    uv.handle = 0
    uv.open = false
    if isa(uv.closecb, Function) uv.closecb(uv) end
    notify(uv.closenotify)
end
close_cb(uv::AsyncWork) = (uv.handle = 0; nothing)

# This serves as a common callback for all async classes
function default_timer_cb(handle,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        async = unsafe_pointer_to_objref(data)::TimeoutAsyncWork
        async.cb(status)
    end
    nothing
end

# units are in ms
function start_timer(timer::TimeoutAsyncWork,timeout::Int64,repeat::Int64)
    associate_julia_struct(timer.handle,timer)
    ccall(:uv_update_time,Void,(Ptr{Void},),eventloop())
    uv_error("start_timer",ccall(:uv_timer_start,Int32,(Ptr{Void},Ptr{Void},Int64,Int64),
        timer.handle,cfunction(default_timer_cb,Void,(Ptr{Void},Int32)),timeout,repeat)==-1)
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

function queueAsync(work::SingleAsyncWork)
    ccall(:uv_async_send,Void,(Ptr{Void},),work.handle)
end

## event loop ##
eventloop() = unsafe_load(cglobal(:jl_io_loop,Ptr{Void}))

function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
function process_events(block::Bool,loop::Ptr{Void})
    if(block)
        ccall(:jl_run_once,Int32,(Ptr{Void},),loop)
    else
        ccall(:jl_process_events,Int32,(Ptr{Void},),loop)        
    end
end
process_events(block::Bool) = process_events(block,eventloop())
run_event_loop() = run_event_loop(eventloop())

##pipe functions
malloc_pipe() = c_malloc(_sizeof_uv_named_pipe)

const UV_PIPE_IPC          = 0x01
const UV_PIPE_SPAWN_SAFE   = 0x02
const UV_PIPE_READABLE     = 0x04
const UV_PIPE_WRITEABLE    = 0x08

function init_pipe(pipe,writeable::Bool,julia_only::Bool)
    flags = writeable ? UV_PIPE_WRITEABLE : UV_PIPE_READABLE
    if !julia_only
        flags |= UV_PIPE_SPAWN_SAFE
    end
    uv_error("init_pipe",ccall(:uv_pipe_init,Int32,(Ptr{Void},Ptr{Void},Int32),eventloop(),pipe,flags)==-1)
end

function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool,pipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    init_pipe(read_end,false,readable_julia_only)
    init_pipe(write_end,true,writeable_julia_only)
    associate_julia_struct(read_end,pipe)
    associate_julia_struct(write_end,pipe)
    uv_error("link_pipe",ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end)==-1)
end

function link_pipe(read_end2::NamedPipe,readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool)
    if(read_end2.handle == C_NULL)
        read_end2.handle = malloc_pipe()
        associate_julia_struct(read_end2.handle,read_end2)
    end
    link_pipe(read_end2.handle,readable_julia_only,write_end,writeable_julia_only,read_end2)
    read_end2.open = true
end
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::NamedPipe,writeable_julia_only::Bool)
    if(write_end.handle == C_NULL)
        write_end.handle = malloc_pipe()
        associate_julia_struct(write_end.handle,write_end)
    end
    link_pipe(read_end,readable_julia_only,write_end.handle,writeable_julia_only,write_end)
    write_end.open = true
end
close_pipe_sync(handle::UVHandle) = ccall(:uv_pipe_close_sync,Void,(UVHandle,),handle)

function isopen(stream::Union(AsyncStream,UVServer))
    stream.open
end

##stream functions

@unix_only immutable UvBuffer
    base::Ptr{Uint8}
    len::Csize_t
    UvBuffer(base, len) = new(base,len)
end

@windows_only immutable UvBuffer
    len::Culong
    base::Ptr{Uint8}
    UvBuffer(base, len) = new(len,base)
end

function default_alloc_buf_cb(handle,suggested_size)
    data = uv_handle_data(handle)
    if data != C_NULL
        stream = unsafe_pointer_to_objref(data)::AsyncStream
        (buf,size) = alloc_request(stream.buffer, suggested_size)
        @assert size>0 # because libuv requires this (TODO: possibly stop reading too if it fails)
        return UvBuffer(buf,size)
    end
    return UvBuffer(0,0)
end

function default_read_cb(handle, nread, buf) 
    data = uv_handle_data(handle)
    if data != C_NULL
        stream = unsafe_pointer_to_objref(data)::AsyncStream
        if(nread == -1)
            close(stream)
            notify(stream.readnotify,uv_lasterror())
        else
            notify_filled(stream.buffer, nread, buf.base, buf.len)
            notify_filled(stream, nread)
            notify(stream.readnotify,UV_error_t(0,0))
        end
    end
    nothing
end

function start_reading(stream::AsyncStream)
    @assert stream.handle != C_NULL
    ccall(:uv_read_start,Int32,(Ptr{Void},Ptr{Void},Ptr{Void}),handle(stream),
        cfunction(default_alloc_buf_cb,UvBuffer,(Ptr{Void},Csize_t)),
        cfunction(default_read_cb,Void,(Ptr{Void},Cssize_t,UvBuffer)))
end

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


type UV_error_t
    uv_code::Int32
    system_code::Int32
    UV_error_t(uv_code,system_code) = new(int32(uv_code),int32(system_code))
end

## Libuv error handling
uv_lasterror(loop) = ccall(:uv_last_error,Base.UV_error_t,(Ptr{Void},),loop)
uv_lasterror() = uv_lasterror(eventloop())

type UVError <: Exception
    prefix::String
    s::UV_error_t
    UVError(p::String,e::UV_error_t)=new(p,e)
end
UVError(p::String) = UVError(p,uv_lasterror())
UVError(p::String,uv::Integer,system::Integer) = UVError(p,UV_error_t(uv,system))

struverror(err::UVError) = bytestring(ccall(:uv_strerror,Ptr{Uint8},(UV_error_t,),err.s))
uverrorname(err::UVError) = bytestring(ccall(:uv_err_name,Ptr{Uint8},(UV_error_t,),err.s))

uv_error(prefix, e::UV_error_t) = e.uv_code != 0 ? throw(UVError(string(prefix),e)) : nothing
uv_error(prefix, b::Bool) = b ? throw(UVError(string(prefix))) : nothing
uv_error(prefix) = uv_error(prefix, uv_lasterror().uv_code != 0)

show(io::IO, e::UVError) = print(io, e.prefix*": "*struverror(e)*" ("*uverrorname(e)*")")

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

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
            uv_error("accept:",_uv_lasterror().uv_code!=UV_EAGAIN)
        end
        err = wait(server.connectnotify)
        if err.uv_code != -1
            throw(UVError("accept",err))
        end
    end
end

function default_connection_cb(handle,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        server = unsafe_pointer_to_objref(data)::UVServer
        server.ccb(server,status)
    end
    nothing
end

function listen!(sock::UVServer; backlog::Integer=511)
    err = ccall(:uv_listen, Int32, (Ptr{Void}, Int32, Ptr{Void}), sock.handle, backlog,
        cfunction(default_connection_cb,Void,(Ptr{Void},Int32)))
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
