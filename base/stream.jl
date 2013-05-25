#TODO: Move stdio detection from C to Julia (might require some Clang magic)
include("uv_constants.jl")

## types ##
typealias Executable Union(Vector{ByteString},Function)
typealias Callback Union(Function,Bool)
type WaitTask 
    filter::Callback #runs task only if false
    localdata::Any
    job

    WaitTask(forwhat, test::Callback) = new(test, forwhat)
    WaitTask() = new(false, nothing)
end

abstract AsyncStream <: IO

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
@eval const $(symbol("_sizeof_"*lowercase(string(r)))) = uv_sizeof_handle($r)
end

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
    readnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    NamedPipe() = new(C_NULL,PipeBuffer(),false,true,false,WaitTask[],false,
                      WaitTask[])
end

show(io::IO,stream::NamedPipe) = print(io,"NamedPipe(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")

type TTY <: AsyncStream
    handle::Ptr{Void}
    open::Bool
    line_buffered::Bool
    buffer::IOBuffer
    readcb::Callback
    readnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    TTY(handle,open)=new(handle,open,true,PipeBuffer(),false,WaitTask[],false,WaitTask[])
end

show(io::IO,stream::TTY) = print(io,"TTY(",stream.open?"connected,":"disconnected,",nb_available(stream.buffer)," bytes waiting)")


type FileMonitor
    handle::Ptr{Void}
    cb::Callback
    function FileMonitor(cb, file)
        handle = c_malloc(_sizeof_uv_fs_events)
        err = ccall(:jl_fs_event_init,Int32, (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32), eventloop(),handle,file,0)
        if err == -1
            c_free(handle)
            throw(UVError("FileMonitor"))
        end
        this = new(handle,cb)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this        
    end
    FileMonitor(file) = FileMonitor(false,file)
end

close(t::FileMonitor) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

const UV_READABLE = 1
const UV_WRITEABLE = 2

#Wrapper for an OS file descriptor (on both Unix and Windows)
immutable OS_FD
    fd::Int32
end

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
    PollingFileWatcher(file) =  PollingFileWatcher(false,file)
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
        this = new(handle,false)
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
        this = new(handle,false)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this
    end
end

close(t::UVPollingWatcher) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

function start_watching(t::FDWatcher, events)
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (FD)",
        ccall(:jl_poll_start,Int32,(Ptr{Void},Int32),t.handle,events)==-1)
end
start_watching(f::Function, t::FDWatcher, events) = (t.cb = f; start_watching(t,events))

function start_watching(t::PollingFileWatcher, interval) 
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (File)",
        ccall(:jl_fs_poll_start,Int32,(Ptr{Void},Ptr{Uint8},Uint32),t.handle,t.file,interval)==-1)
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

function _uv_hook_fseventscb(t::FileMonitor,filename::Ptr,events::Int32,status::Int32)
    if(isa(t.cb,Function))
        # bytestring(convert(Ptr{Uint8},filename)) - seems broken at the moment - got NULL
        t.cb(status, events, status)
    end
end

function _uv_hook_pollcb(t::FDWatcher,status::Int32,events::Int32)
    if(isa(t.cb,Function))
        t.cb(status, events)
    end
end
function _uv_hook_fspollcb(t::PollingFileWatcher,status::Int32,prev::Ptr,cur::Ptr)
    if(isa(t.cb,Function))
        t.cb(status, Stat(convert(Ptr{Uint8},prev)), Stat(convert(Ptr{Uint8},cur)))
    end
end

_uv_hook_close(uv::FileMonitor) = (uv.handle = 0; nothing)
_uv_hook_close(uv::UVPollingWatcher) = (uv.handle = 0; nothing)

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

function tasknotify(waittasks::Vector{WaitTask}, args...)
    newwts = WaitTask[]
    ct = current_task()
    for wt in waittasks
        f = wt.filter
        if (isa(f, Function) ? f(wt.localdata, args...) : f) === false
            work = wt.job
            work.argument = args
            enq_work(work)
        else
            push!(newwts,wt)
        end
    end
    resize!(waittasks,length(newwts))
    waittasks[:] = newwts
end

wait_connect_filter(w::AsyncStream, args...) = !w.open
wait_readnb_filter(w::(AsyncStream,Int), args...) = w[1].open && (nb_available(w[1].buffer) < w[2])
wait_readbyte_filter(w::(AsyncStream,Uint8), args...) = w[1].open && (search(w[1].buffer,w[2]) <= 0)
wait_readline_filter(w::AsyncStream, args...) = w.open && (search(w.buffer,'\n') <= 0)

function wait(forwhat::Vector, notify_list_name, filter_fcn)
    args = ()
    for x in forwhat
        args = wait(x, notify_list_name, filter_fcn)
    end
    args
end

function wait(forwhat, notify_list_name, filter_fcn)
    args = ()
    while filter_fcn(forwhat)
        assert(current_task() != Scheduler, "Cannot execute blocking function from Scheduler")
        thing = isa(forwhat,Tuple) ? forwhat[1] : forwhat
        wt = WaitTask(forwhat, filter_fcn)
        push!(thing.(notify_list_name), wt)
        args = yield(wt)
        if isa(args,InterruptException)
            error(args)
        end
    end
    args
end

wait_connected(x) = wait(x, :connectnotify, wait_connect_filter)
wait_readline(x) = wait(x, :readnotify, wait_readline_filter)
wait_readnb(x::(AsyncStream,Int)) = wait(x, :readnotify, wait_readnb_filter)
wait_readnb(x::AsyncStream,b::Int) = wait_readnb((x,b))
wait_readbyte(x::AsyncStream,c::Uint8) = wait((x,c), :readnotify, wait_readbyte_filter)
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

uv_handle_data(handle) = ccall(:jl_uv_handle_data,Ptr{Void},(Ptr{Void},),handle)

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

close(t::TimeoutAsyncWork) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

function poll_fd(s, events::Integer, timeout_ms::Integer)
    wt = WaitTask()

    fdw = FDWatcher(s)
    start_watching((status, events) -> tasknotify([wt], :poll, status, events), fdw, events)
    
    if (timeout_ms > 0)
        timer = TimeoutAsyncWork(status -> tasknotify([wt], :timeout, status))
        start_timer(timer, int64(timeout_ms), int64(0))
    end

    args = yield(wt)

    if (timeout_ms > 0) stop_timer(timer) end

    stop_watching(fdw)
    if isa(args,InterruptException)
        rethrow(args)
    end

    if (args[2] != 0) error ("fd in error") end 
    if (args[1] == :poll) return args[3] end
    if (args[1] == :timeout) return 0 end

    error("Error while polling") 
end

function poll_file(s, interval::Integer, timeout_ms::Integer)
    wt = WaitTask()

    pfw = PollingFileWatcher(s)
    start_watching((status,prev,cur) -> tasknotify([wt], :poll, status), pfw, interval)
    
    if (timeout_ms > 0)
        timer = TimeoutAsyncWork(status -> tasknotify([wt], :timeout, status))
        start_timer(timer, int64(timeout_ms), int64(0))
    end

    args = yield(wt)

    if (timeout_ms > 0) stop_timer(timer) end

    stop_watching(pfw)
    if isa(args,InterruptException)
        rethrow(args)
    end

    if (args[2] != 0) error ("fd in error") end 
    if (args[1] == :poll) return 1 end
    if (args[1] == :timeout) return 0 end

    error("Error while polling")
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

function _uv_hook_close(uv::AsyncStream)
    uv.handle = 0
    uv.open = false
    if isa(uv.closecb, Function) uv.closecb(uv) end
    tasknotify(uv.closenotify, uv)
end
_uv_hook_close(uv::AsyncWork) = (uv.handle = 0; nothing)

# This serves as a common callback for all async classes
_uv_hook_asynccb(async::AsyncWork, status::Int32) = async.cb(status)

function default_timer_cb(handle,status)
    data = uv_handle_data(handle)
    if data != C_NULL
        async = unsafe_pointer_to_objref(data)::IdleAsyncWork
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
    timer = TimeoutAsyncWork(status->tasknotify([wt], status))
    wt = WaitTask(timer, false)
    start_timer(timer, int64(iround(sec*1000)), int64(0))
    args = yield(wt)
    stop_timer(timer)
    if isa(args,InterruptException)
        error(args)
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

function isopen(stream::AsyncStream)
    stream.open
end

_uv_hook_isopen(stream::AsyncStream) = int32(isopen(stream))

function close(stream::AsyncStream)
    if stream.open
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.open = false
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

const char_cache = Array(Uint8,256)
for i=1:256
    char_cache[i] = uint8(i-1)
end

write_length(x) = length(x)
write_pointer(x) = pointer(x)
write_length(::Uint8) = 1
write_pointer(c::Uint8) = pointer(char_cache)+c

## low-level calls

# String are considered immutable
write(s::AsyncStream, b::ASCIIString) = _write!(s,b.data)
write(s::AsyncStream, b::Uint8) = _write!(s,b)
function write(s::AsyncStream, c::Char)
    if c < 0x80
        write(s,uint8(c))
    else
        a = Array(Uint32,1)
        a[1] = uint32(c)
        _write!(s,reinterpret(Uint8,a))
    end
end
function write{T}(s::AsyncStream, a::Array{T})
    if isbits(T)
        _write!(s,reinterpret(Uint8,copy(a)))
    else
        invoke(write,(IO,Array),s,a)
    end
end
function write(s::AsyncStream, p::Ptr, nb::Integer)
    b = Array(Uint8,nb)
    unsafe_copy!(pointer(b),p,nb)
    _write!(s,b)
end

# 
# Low-level write. b may not be modified after this call (until the callback is called).
# The callback function is responsible for freeing the request.
# This function is not intended to be exported as some care is required
# to use it properly 
#

immutable UvBuffer
    base::Ptr{Uint8}
    len::Csize_t
end

function _write!(s::AsyncStream, b, cb::Function)
    Base.sigatomic_begin()
    req = Base.c_malloc(_sizeof_uv_write)
    n = write_length(b)
    buf = UvBuffer(write_pointer(b),convert(Csize_t,n))
    # uvw->data
    ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Ptr{Void}),req,pointer_from_objref(b))
    #unsafe_store!(convert(Ptr{Ptr{Void}},req),pointer_from_objref(b),0)
    err = ccall(:uv_write,Int32,(Ptr{Void},Ptr{Void},Ptr{UvBuffer},Int32,Ptr{Void}),
        req,handle(s),&buf,1,cfunction(cb,Void,(Ptr{Void},Int32)))
    Base.sigatomic_end()
    uv_error("_write!",err==-1)
    n 
end

function default_write_cb(req::Ptr{Void},status::Int32)
    c_free(req)
end

_write!(s,b) = _write!(s,b,default_write_cb)

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

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

function listen(sock::AsyncStream, backlog::Integer)
    err = ccall(:jl_listen, Int32, (Ptr{Void}, Int32), sock.handle, backlog)
    err != -1 ? (sock.open = true): false
end
listen(sock::AsyncStream) = listen(sock, 511) # same default as node.js
