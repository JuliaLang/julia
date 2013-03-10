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

const _sizeof_uv_pipe = ccall(:jl_sizeof_uv_pipe_t,Int32,())

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

uvtype(::AsyncStream) = UV_STREAM
uvhandle(stream::AsyncStream) = stream.handle

copy(s::TTY) = TTY(s.handle,s.open)

convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)
handle(s::AsyncStream) = s.handle
handle(s::Ptr{Void}) = s

make_stdout_stream() = _uv_tty2tty(ccall(:jl_stdout_stream, Ptr{Void}, ()))

function _uv_tty2tty(handle::Ptr{Void})
    tty = TTY(handle,true)
    tty.line_buffered = false
    ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},Any),handle,tty)
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
wait_readable_filter(w::AsyncStream, args...) = nb_available(w.buffer) <= 0
wait_readnb_filter(w::(AsyncStream,Int), args...) = w[1].open && (nb_available(w[1].buffer) < w[2])
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
wait_readable(x) = wait(x, :readnotify, wait_readable_filter)
wait_readline(x) = wait(x, :readnotify, wait_readline_filter)
wait_readnb(x::(AsyncStream,Int)) = wait(x, :readnotify, wait_readnb_filter)
wait_readnb(x::AsyncStream,b::Int) = wait_readnb((x,b))

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

type SingleAsyncWork <: AsyncWork
    cb::Function
    handle::Ptr{Void}
    function SingleAsyncWork(loop::Ptr{Void},cb::Function)
        if(loop == C_NULL)
            return new(cb,C_NULL)
        end
        this=new(cb)
        this.handle=ccall(:jl_make_async,Ptr{Void},(Ptr{Void},Any),loop,this)
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
        this
    end
end
TimeoutAsyncWork(cb::Function) = TimeoutAsyncWork(eventloop(),cb)

function _uv_hook_close(uv::AsyncStream)
    uv.handle = 0
    uv.open = false
    if isa(uv.closecb, Function) uv.closecb(uv) end
    tasknotify(uv.closenotify, uv)
end
_uv_hook_close(uv::AsyncWork) = (uv.handle = 0; nothing)

# This serves as a common callback for all async classes
_uv_hook_asynccb(async::AsyncWork, status::Int32) = async.cb(status)

function start_timer(timer::TimeoutAsyncWork,timeout::Int64,repeat::Int64)
    ccall(:jl_timer_start,Int32,(Ptr{Void},Int64,Int64),timer.handle,timeout,repeat)
end

function stop_timer(timer::TimeoutAsyncWork)
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
eventloop() = ccall(:jl_global_event_loop,Ptr{Void},())
#mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},()) # this would be fine, but is nowhere supported

function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
run_event_loop() = run_event_loop(eventloop())

##pipe functions
malloc_pipe() = c_malloc(_sizeof_uv_pipe)
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writeable_julia_only::Bool,pipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Bool,Bool,Any), read_end, 0, readable_julia_only, pipe)
    ccall(:jl_init_pipe, Ptr{Void}, (Ptr{Void},Bool,Bool,Any), write_end, 1, readable_julia_only, pipe)
    error = ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end)
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
    if isbits(T)
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
    if isbits(T)
        ccall(:jl_write, Int, (Ptr{Void}, Ptr{Void}, Uint32), handle(s), a, uint(length(a)*sizeof(T)))
    else
        invoke(write,(IO,Array),s,a)
    end
end
write(s::AsyncStream, p::Ptr, nb::Integer) = 
    ccall(:jl_write, Int,(Ptr{Void}, Ptr{Void}, Uint),handle(s), p, uint(nb))
_write(s::AsyncStream, p::Ptr{Void}, nb::Integer) = 
    ccall(:jl_write, Int,(Ptr{Void}, Ptr{Void}, Uint),handle(s),p,uint(nb))

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

listen(sock::AsyncStream,backlog::Integer) = (err = ccall(:jl_listen,Int32,(Ptr{Void},Int32),sock.handle,backlog); err != -1 ? (sock.open = true): false)
listen(sock::AsyncStream) = listen(sock,4)
#listen(path::String)
