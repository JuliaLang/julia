#TODO: Move stdio detection from C to Julia (might require some Clang magic)
include("uv_constants.jl")

## types ##
typealias Callback Union(Function,Bool)

abstract AsyncStream <: IO
abstract UVServer

typealias UVHandle Ptr{Void}
typealias UVStream AsyncStream

# A dict of all libuv handles that are being waited on somewhere in the system
# and should thus not be garbage collected
const uvhandles = ObjectIdDict()

preserve_handle(x) = uvhandles[x] = get(uvhandles,x,0)+1
unpreserve_handle(x) = (v = uvhandles[x]; v == 1 ? pop!(uvhandles,x) : (uvhandles[x] = v-1); nothing)

#Wrapper for an OS file descriptor (on both Unix and Windows)
immutable RawFD
    fd::Int32
    RawFD(fd::Integer) = new(int32(fd))
    RawFD(fd::RawFD) = fd
end

convert(::Type{Int32}, fd::RawFD) = fd.fd

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

nb_available(s::AsyncStream) = nb_available(s.buffer)

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
const StatusEOF         = 7 # handle is a TTY that has seen an EOF event
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
    elseif s == StatusEOF
        return "eof"
    end
    return "invalid status"
end

uv_handle_data(handle) = ccall(:jl_uv_handle_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_data(handle) = ccall(:jl_uv_req_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_set_data(req,data) = ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Any),req,data)
uv_req_set_data(req,data::Ptr{Void}) = ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Ptr{Void}),req,data)

type Pipe <: AsyncStream
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
    Pipe(handle) = new(
        handle,
        StatusUninit,
        PipeBuffer(),
        true,
        false,Condition(),
        false,Condition(),
        false,Condition())
end
function Pipe()
    handle = c_malloc(_sizeof_uv_named_pipe)
    try
        ret = Pipe(handle)
        associate_julia_struct(ret.handle,ret)
        finalizer(ret,uvfinalize)
        return init_pipe!(ret;readable=true)
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

function init_pipe!(pipe::Union(Pipe,PipeServer);readable::Bool=false,writable=false,julia_only=true)
    if pipe.handle == C_NULL
        error("failed to initialize pipe")
    elseif pipe.status != StatusUninit
        error("pipe is already initialized")
    end
    uv_error("init_pipe",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), pipe.handle, writable,readable,julia_only))
    pipe.status = StatusInit
    pipe
end

function PipeServer()
    handle = c_malloc(_sizeof_uv_named_pipe)
    try
        ret = PipeServer(handle)
        associate_julia_struct(ret.handle,ret)
        finalizer(ret,uvfinalize)
        return init_pipe!(ret;readable=true)
    catch
        c_free(handle)
        c_free(handle-1)
        rethrow()
    end
end

show(io::IO,stream::Pipe) = print(io,"Pipe(",uv_status_string(stream),", ",
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
    @windows_only ispty::Bool
    function TTY(handle)
        tty = new(
            handle,
            StatusUninit,
            true,
            PipeBuffer(),
            false,Condition(),
            false,Condition())
        @windows_only tty.ispty = bool(ccall(:jl_ispty, Cint, (Ptr{Void},), handle))
        tty
    end
end

function TTY(fd::RawFD; readable::Bool = false)
    handle = c_malloc(_sizeof_uv_tty)
    ret = TTY(handle)
    associate_julia_struct(handle,ret)
    finalizer(ret,uvfinalize)
    # This needs to go after associate_julia_struct so that there
    # is no garbage in the ->data field
    uv_error("TTY",ccall(:uv_tty_init,Int32,(Ptr{Void},Ptr{Void},Int32,Int32),eventloop(),handle,fd.fd,readable))
    ret.status = StatusOpen
    ret.line_buffered = false
    ret
end

# note that uv_is_readable/writable work for any subtype of
# uv_stream_t, including uv_tty_t and uv_pipe_t
isreadable(io::Union(Pipe,TTY)) =
    bool(ccall(:uv_is_readable, Cint, (Ptr{Void},), io.handle))
iswritable(io::Union(Pipe,TTY)) =
    bool(ccall(:uv_is_writable, Cint, (Ptr{Void},), io.handle))

nb_available(stream::UVStream) = nb_available(stream.buffer)

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
disassociate_julia_struct(uv) = disassociate_julia_struct(uv.handle)
disassociate_julia_struct(handle::Ptr{Void}) =
    handle != C_NULL && ccall(:jl_uv_disassociate_julia_struct,Void,(Ptr{Void},),handle)

function init_stdio(handle)
    t = ccall(:jl_uv_handle_type,Int32,(Ptr{Void},),handle)
    if t == UV_FILE
        return fdio(ccall(:jl_uv_file_handle,Int32,(Ptr{Void},),handle))
    else
        if t == UV_TTY
            ret = TTY(handle)
        elseif t == UV_TCP
            ret = TCPSocket(handle)
        elseif t == UV_NAMED_PIPE
            ret = Pipe(handle)
        else
            error("FATAL: stdio type ($t) invalid")
        end
        ret.status = StatusOpen
        ret.line_buffered = false
        associate_julia_struct(ret.handle,ret)
        finalizer(ret,uvfinalize)
        return ret
    end
end

function stream_wait(x,c...)
    preserve_handle(x)
    try
        return wait(c...)
    finally
        unpreserve_handle(x)
    end
end

function reinit_stdio()
    global uv_jl_asynccb = cglobal(:jl_uv_asynccb)
    global uv_jl_alloc_buf = cglobal(:jl_uv_alloc_buf)
    global uv_jl_readcb = cglobal(:jl_uv_readcb)
    global uv_jl_connectioncb = cglobal(:jl_uv_connectioncb)
    global uv_jl_connectcb = cglobal(:jl_uv_connectcb)
    global uv_jl_writecb = cglobal(:jl_uv_writecb)
    global uv_jl_writecb_task = cglobal(:jl_uv_writecb_task)
    global uv_eventloop = ccall(:jl_global_event_loop, Ptr{Void}, ())
    global STDIN = init_stdio(ccall(:jl_stdin_stream ,Ptr{Void},()))
    global STDOUT = init_stdio(ccall(:jl_stdout_stream,Ptr{Void},()))
    global STDERR = init_stdio(ccall(:jl_stderr_stream,Ptr{Void},()))
end

flush(::AsyncStream) = nothing

function isopen(x::Union(AsyncStream,UVServer))
    if !(x.status != StatusUninit && x.status != StatusInit)
        error("I/O object not initialized")
    end
    x.status != StatusClosed && x.status != StatusEOF
end

function check_open(x)
    if !isopen(x) || x.status == StatusClosing
        error("stream is closed or unusable")
    end
end

function wait_connected(x)
    check_open(x)
    while x.status == StatusConnecting
        stream_wait(x,x.connectnotify)
        check_open(x)
    end
end

function wait_readbyte(x::AsyncStream, c::UInt8)
    while isopen(x) && search(x.buffer,c) <= 0
        start_reading(x)
        stream_wait(x,x.readnotify)
    end
end

function wait_readnb(x::AsyncStream, nb::Int)
    while isopen(x) && nb_available(x.buffer) < nb
        start_reading(x)
        stream_wait(x,x.readnotify)
    end
end

wait_close(x) = if isopen(x) stream_wait(x,x.closenotify); end

#from `connect`
function _uv_hook_connectcb(sock::AsyncStream, status::Int32)
    @assert sock.status == StatusConnecting
    if status >= 0
        sock.status = StatusOpen
        err = ()
    else
        sock.status = StatusInit
        err = UVError("connect",status)
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock, status)
    end
    err===() ? notify(sock.connectnotify) : notify_error(sock.connectnotify, err)
end

#from `listen`
function _uv_hook_connectioncb(sock::UVServer, status::Int32)
    local err
    if status >= 0
        err = ()
    else
        err = UVError("connection",status)
    end
    if isa(sock.ccb,Function)
        sock.ccb(sock,status)
    end
    err===() ? notify(sock.connectnotify) : notify_error(sock.connectnotify, err)
end

## BUFFER ##
## Allocate a simple buffer
function alloc_request(buffer::IOBuffer, recommended_size::UInt)
    ensureroom(buffer, int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    return (pointer(buffer.data, ptr), length(buffer.data)-ptr+1)
end
function _uv_hook_alloc_buf(stream::AsyncStream, recommended_size::UInt)
    (buf,size) = alloc_request(stream.buffer, recommended_size)
    @assert size>0 # because libuv requires this (TODO: possibly stop reading too if it fails)
    (buf,uint(size))
end

function notify_filled(buffer::IOBuffer, nread::Int, base::Ptr{Void}, len::UInt)
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

const READ_BUFFER_SZ=10485760           # 10 MB
function _uv_hook_readcb(stream::AsyncStream, nread::Int, base::Ptr{Void}, len::UInt)
    if nread < 0
        if nread != UV_EOF
            # This is a fatal connectin error. Shutdown requests as per the usual
            # close function won't work and libuv will fail with an assertion failure
            ccall(:jl_forceclose_uv,Void,(Ptr{Void},),stream.handle)
            notify_error(stream.readnotify, UVError("readcb",nread))
        else
            if isa(stream,TTY)
                stream.status = StatusEOF
                notify(stream.readnotify)
                notify(stream.closenotify)
            else
                close(stream)
            end
        end
    else
        notify_filled(stream.buffer, nread, base, len)
        notify_filled(stream, nread)
        notify(stream.readnotify)
    end

    # Stop reading when
    # 1) when we have an infinite buffer, and we have accumulated a lot of unread data OR
    # 2) we have an alternate buffer that has reached its limit.
    if (is_maxsize_unlimited(stream.buffer) && (nb_available(stream.buffer) > READ_BUFFER_SZ )) ||
       (nb_available(stream.buffer) == stream.buffer.maxsize)
        stop_reading(stream)
    end
end

reseteof(x::IO) = nothing
function reseteof(x::TTY)
    if x.status == StatusEOF
        x.status = StatusOpen
    end
    nothing
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
        associate_julia_struct(this.handle, this)
        preserve_handle(this)
        err = ccall(:uv_async_init,Cint,(Ptr{Void},Ptr{Void},Ptr{Void}),eventloop(),this.handle,uv_jl_asynccb::Ptr{Void})
        this
    end
end

type Timer <: AsyncWork
    handle::Ptr{Void}
    cb::Function
    function Timer(cb::Function)
        this = new(c_malloc(_sizeof_uv_timer), cb)
        # We don't want to set a julia struct, but we also
        # want to make sure there's no garbage data in the
        # ->data field
        disassociate_julia_struct(this.handle)
        err = ccall(:uv_timer_init,Cint,(Ptr{Void},Ptr{Void}),eventloop(),this.handle)
        if err != 0
            c_free(this.handle)
            this.handle = C_NULL
            error(UVError("uv_make_timer",err))
        end
        finalizer(this,uvfinalize)
        this
    end
end

close(t::Timer) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

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
function _uv_hook_asynccb(async::AsyncWork)
    if isa(async, Timer)
        if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Void},), async.handle) == 0
            # timer is stopped now
            disassociate_julia_struct(async.handle)
        end
    end
    try
        async.cb(async)
    catch
    end
    nothing
end

function start_timer(timer::Timer, timeout::Real, repeat::Real)
    associate_julia_struct(timer.handle, timer)
    preserve_handle(timer)
    ccall(:uv_update_time,Void,(Ptr{Void},),eventloop())
    ccall(:uv_timer_start,Cint,(Ptr{Void},Ptr{Void},UInt64,UInt64),
          timer.handle, uv_jl_asynccb::Ptr{Void}, uint64(round(timeout*1000))+1, uint64(round(repeat*1000)))
end

function stop_timer(timer::Timer)
    # ignore multiple calls to stop_timer
    !haskey(uvhandles, timer) && return
    timer.handle == C_NULL && return

    ccall(:uv_timer_stop,Cint,(Ptr{Void},),timer.handle)
    disassociate_julia_struct(timer.handle)
    unpreserve_handle(timer)
end

function sleep(sec::Real)
    w = Condition()
    timer = Timer(function (tmr)
        notify(w)
    end)
    start_timer(timer, float(sec), 0)
    try
        stream_wait(timer,w)
    finally
        stop_timer(timer)
    end
    nothing
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
function malloc_julia_pipe(x)
    x.handle = c_malloc(_sizeof_uv_named_pipe)
    associate_julia_struct(x.handle,x)
    finalizer(x,uvfinalize)
end

_link_pipe(read_end::Ptr{Void},write_end::Ptr{Void}) = uv_error("pipe_link",ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end))

function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writable_julia_only::Bool,readpipe::AsyncStream,writepipe::AsyncStream)
    #make the pipe an unbuffered stream for now
    #TODO: this is probably not freeing memory properly after errors
    uv_error("init_pipe",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    uv_error("init_pipe(2)",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end,write_end)
end

function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Ptr{Void},writable_julia_only::Bool)
    uv_error("init_pipe",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    uv_error("init_pipe(2)",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end,write_end)
end

function link_pipe(read_end::Pipe,readable_julia_only::Bool,write_end::Ptr{Void},writable_julia_only::Bool)
    if read_end.handle == C_NULL
        malloc_julia_pipe(read_end)
    end
    init_pipe!(read_end; readable = true, writable = false, julia_only = readable_julia_only)
    uv_error("init_pipe",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end.handle,write_end)
    read_end.status = StatusOpen
end
function link_pipe(read_end::Ptr{Void},readable_julia_only::Bool,write_end::Pipe,writable_julia_only::Bool)
    if write_end.handle == C_NULL
        malloc_julia_pipe(write_end)
    end
    uv_error("init_pipe",ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    init_pipe!(write_end; readable = false, writable = true, julia_only = writable_julia_only)
    _link_pipe(read_end,write_end.handle)
    write_end.status = StatusOpen
end
function link_pipe(read_end::Pipe,readable_julia_only::Bool,write_end::Pipe,writable_julia_only::Bool)
    if write_end.handle == C_NULL
        malloc_julia_pipe(write_end)
    end
    if read_end.handle == C_NULL
        malloc_julia_pipe(read_end)
    end
    init_pipe!(read_end; readable = true, writable = false, julia_only = readable_julia_only)
    init_pipe!(write_end; readable = false, writable = true, julia_only = writable_julia_only)
    _link_pipe(read_end.handle,write_end.handle)
    write_end.status = StatusOpen
    read_end.status = StatusOpen
    nothing
end
close_pipe_sync(p::Pipe) = (ccall(:uv_pipe_close_sync,Void,(Ptr{Void},),p.handle); p.status = StatusClosed)
close_pipe_sync(handle::UVHandle) = ccall(:uv_pipe_close_sync,Void,(UVHandle,),handle)

_uv_hook_isopen(stream) = int32(stream.status != StatusUninit && stream.status != StatusInit && isopen(stream))

function close(stream::Union(AsyncStream,UVServer))
    if isopen(stream) && stream.status != StatusClosing
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.status = StatusClosing
    end
    nothing
end

## stream functions ##
function start_reading(stream::AsyncStream)
    if stream.status == StatusOpen
        if !isreadable(stream)
            error("tried to read a stream that is not readable")
        end
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

function read!{T}(s::AsyncStream, a::Array{T})
    isbits(T) || error("read from buffer only supports bits types or arrays of bits types")
    nb = length(a) * sizeof(T)
    read!(s, reshape(reinterpret(UInt8, a), nb))
    return a
end

function read!(s::AsyncStream, a::Vector{UInt8})
    nb = length(a)
    sbuf = s.buffer
    @assert sbuf.seekable == false
    @assert sbuf.maxsize >= nb

    if nb_available(sbuf) >= nb
        return read!(sbuf, a)
    end

    if nb <= 65536 # Arbitrary 64K limit under which we are OK with copying the array from the stream's buffer
        wait_readnb(s,nb)
        read!(sbuf, a)
    else
        stop_reading(s) # Just playing it safe, since we are going to switch buffers.
        newbuf = PipeBuffer(a, nb)
        newbuf.size = 0
        s.buffer = newbuf
        write(newbuf, sbuf)
        wait_readnb(s,nb)
        s.buffer = sbuf
    end
    return a
end

function read{T}(s::AsyncStream, ::Type{T}, dims::Dims)
    isbits(T) || error("read from buffer only supports bits types or arrays of bits types")
    nb = prod(dims)*sizeof(T)
    a = read!(s, Array(UInt8, nb))
    reshape(reinterpret(T, a), dims)
end

function read(this::AsyncStream,::Type{UInt8})
    buf = this.buffer
    @assert buf.seekable == false
    wait_readnb(this,1)
    read(buf,UInt8)
end

readline(this::AsyncStream) = readuntil(this, '\n')

readline() = readline(STDIN)

function readavailable(this::AsyncStream)
    buf = this.buffer
    @assert buf.seekable == false
    wait_readnb(this,1)
    takebuf_string(buf)
end

function readuntil(this::AsyncStream,c::UInt8)
    buf = this.buffer
    @assert buf.seekable == false
    wait_readbyte(this,c)
    readuntil(buf,c)
end

#function finish_read(pipe::Pipe)
#    close(pipe) #handles to UV and ios will be invalid after this point
#end
#
#function finish_read(state::(Pipe,ByteString))
#    finish_read(state...)
#end

macro uv_write(n,call)
    esc(quote
        check_open(s)
        uvw = c_malloc(_sizeof_uv_write+$(n))
        err = $call
        if err < 0
            c_free(uvw)
            uv_error("write", err)
        end
    end)
end

## low-level calls ##

function write!{T}(s::AsyncStream, a::Array{T})
    if isbits(T)
        n = uint(length(a)*sizeof(T))
        @uv_write n ccall(:jl_write_no_copy, Int32, (Ptr{Void}, Ptr{Void}, UInt, Ptr{Void}, Ptr{Void}), handle(s), a, n, uvw, uv_jl_writecb::Ptr{Void})
        return int(length(a)*sizeof(T))
    else
        throw(MethodError(write,(s,a)))
    end
end
function write!(s::AsyncStream, p::Ptr, nb::Integer)
    @uv_write nb ccall(:jl_write_no_copy, Int32, (Ptr{Void}, Ptr{Void}, UInt, Ptr{Void}, Ptr{Void}), handle(s), p, nb, uvw, uv_jl_writecb::Ptr{Void})
    return nb
end
write!(s::AsyncStream, string::ByteString) = write!(s,string.data)

function _uv_hook_writecb(s::AsyncStream, req::Ptr{Void}, status::Int32)
    if status < 0
        err = UVError("write",status)
        showerror(STDERR, err, backtrace())
    end
    nothing
end

function write(s::AsyncStream, b::UInt8)
    @uv_write 1 ccall(:jl_putc_copy, Int32, (UInt8, Ptr{Void}, Ptr{Void}, Ptr{Void}), b, handle(s), uvw, uv_jl_writecb_task::Ptr{Void})
    ct = current_task()
    uv_req_set_data(uvw,ct)
    ct.state = :waiting
    stream_wait(ct)
    return 1
end
function write(s::AsyncStream, c::Char)
    nb = utf8sizeof(c)
    @uv_write nb ccall(:jl_pututf8_copy, Int32, (Ptr{Void}, UInt32, Ptr{Void}, Ptr{Void}), handle(s), c, uvw, uv_jl_writecb_task::Ptr{Void})
    ct = current_task()
    uv_req_set_data(uvw,ct)
    ct.state = :waiting
    stream_wait(ct)
    return nb
end
function write{T}(s::AsyncStream, a::Array{T})
    if isbits(T)
        n = uint(length(a)*sizeof(T))
        @uv_write n ccall(:jl_write_no_copy, Int32, (Ptr{Void}, Ptr{Void}, UInt, Ptr{Void}, Ptr{Void}), handle(s), a, n, uvw, uv_jl_writecb_task::Ptr{Void})
        ct = current_task()
        uv_req_set_data(uvw,ct)
        ct.state = :waiting
        stream_wait(ct)
        return int(length(a)*sizeof(T))
    else
        check_open(s)
        invoke(write,(IO,Array),s,a)
    end
end
function write(s::AsyncStream, p::Ptr, nb::Integer)
    @uv_write nb ccall(:jl_write_no_copy, Int32, (Ptr{Void}, Ptr{Void}, UInt, Ptr{Void}, Ptr{Void}), handle(s), p, nb, uvw, uv_jl_writecb_task::Ptr{Void})
    ct = current_task()
    uv_req_set_data(uvw,ct)
    ct.state = :waiting
    stream_wait(ct)
    return int(nb)
end

function _uv_hook_writecb_task(s::AsyncStream,req::Ptr{Void},status::Int32)
    d = uv_req_data(req)
    if status < 0
        err = UVError("write",status)
        if d != C_NULL
            schedule(unsafe_pointer_to_objref(d)::Task,err,error=true)
        else
            showerror(STDERR, err, backtrace())
        end
    elseif d != C_NULL
        schedule(unsafe_pointer_to_objref(d)::Task)
    end
end

## Libuv error handling ##
type UVError <: Exception
    prefix::AbstractString
    code::Int32
    UVError(p::AbstractString,code::Integer)=new(p,int32(code))
end

struverror(err::UVError) = bytestring(ccall(:uv_strerror,Ptr{UInt8},(Int32,),err.code))
uverrorname(err::UVError) = bytestring(ccall(:uv_err_name,Ptr{UInt8},(Int32,),err.code))

uv_error(prefix::Symbol, c::Integer) = uv_error(string(prefix),c)
uv_error(prefix::AbstractString, c::Integer) = c < 0 ? throw(UVError(prefix,c)) : nothing
show(io::IO, e::UVError) = print(io, e.prefix*": "*struverror(e)*" ("*uverrorname(e)*")")


## server functions ##

function accept_nonblock(server::PipeServer,client::Pipe)
    if client.status != StatusInit
        error(client.status == StatusUninit ? "client is not initialized" :
              "client is already in use or has been closed")
    end
    err = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server.handle,client.handle)
    if err == 0
        client.status = StatusOpen
    end
    err
end
function accept_nonblock(server::PipeServer)
    client = Pipe()
    uv_error("accept", accept_nonblock(server,client) != 0)
    client
end

function accept(server::UVServer, client::AsyncStream)
    if server.status != StatusActive
        throw(ArgumentError("server not connected; make sure \"listen\" has been called"))
    end
    while isopen(server)
        err = accept_nonblock(server,client)
        if err == 0
            return client
        elseif err != UV_EAGAIN
            uv_error("accept",err)
        end
        stream_wait(server,server.connectnotify)
    end
    uv_error("accept", UV_ECONNABORTED)
end

const BACKLOG_DEFAULT = 511

function _listen(sock::UVServer; backlog::Integer=BACKLOG_DEFAULT)
    err = ccall(:uv_listen, Cint, (Ptr{Void}, Cint, Ptr{Void}),
                sock.handle, backlog, uv_jl_connectioncb::Ptr{Void})
    sock.status = StatusActive
    err
end

function bind(server::PipeServer, name::ASCIIString)
    @assert server.status == StatusInit
    err = ccall(:uv_pipe_bind, Int32, (Ptr{Void}, Ptr{UInt8}),
                server.handle, name)
    if err != 0
        if err != UV_EADDRINUSE && err != UV_EACCES
            error(UVError("bind",err))
        else
            return false
        end
    end
    server.status = StatusOpen
    true
end


function listen(path::ByteString)
    sock = PipeServer()
    bind(sock, path) || error("could not listen on path $path")
    uv_error("listen", _listen(sock))
    sock
end

function connect!(sock::Pipe, path::ByteString)
    @assert sock.status == StatusInit
    req = c_malloc(_sizeof_uv_connect)
    uv_req_set_data(req,C_NULL)
    ccall(:uv_pipe_connect, Void, (Ptr{Void}, Ptr{Void}, Ptr{UInt8}, Ptr{Void}), req, sock.handle, path, uv_jl_connectcb::Ptr{Void})
    sock.status = StatusConnecting
    sock
end
connect!(sock::Pipe, path::AbstractString) = connect(sock,bytestring(path))

function connect(sock::AsyncStream, args...)
    connect!(sock,args...)
    wait_connected(sock)
    sock
end

connect(path::AbstractString) = connect(Pipe(),path)

dup(x::RawFD) = RawFD(ccall((@windows? :_dup : :dup),Int32,(Int32,),x.fd))
dup(src::RawFD,target::RawFD) = systemerror("dup",-1==
    ccall((@windows? :_dup2 : :dup2),Int32,
    (Int32,Int32),src.fd,target.fd))

_fd(x::IOStream) = RawFD(fd(x))
@unix_only _fd(x::AsyncStream) = RawFD(ccall(:jl_uv_handle,Int32,(Ptr{Void},),x.handle))
@windows_only _fd(x::AsyncStream) = WindowsRawSocket(
    ccall(:jl_uv_handle,Ptr{Void},(Ptr{Void},),x.handle))

for (x,writable,unix_fd,c_symbol) in ((:STDIN,false,0,:jl_uv_stdin),(:STDOUT,true,1,:jl_uv_stdout),(:STDERR,true,2,:jl_uv_stderr))
    f = symbol("redirect_"*lowercase(string(x)))
    _f = symbol(string("_",f))
    @eval begin
        function ($_f)(stream)
            global $x
            @windows? (
                ccall(:SetStdHandle,stdcall,Int32,(Int32,Ptr{Void}),
                    $(-10-unix_fd),_get_osfhandle(_fd(stream)).handle) :
                dup(_fd(stream),  RawFD($unix_fd)) )
            $x = stream
        end
        function ($f)(handle::AsyncStream)
            $(_f)(handle)
            unsafe_store!(cglobal($(Expr(:quote,c_symbol)),Ptr{Void}),
                handle.handle)
            handle
        end
        ($f)(handle::IOStream) = ($_f)(handle)
        function ($f)()
            read,write = (Pipe(C_NULL), Pipe(C_NULL))
            link_pipe(read,$(writable),write,$(!writable))
            ($f)($(writable? :write : :read))
            (read,write)
        end
    end
end

mark(x::AsyncStream)     = mark(x.buffer)
unmark(x::AsyncStream)   = unmark(x.buffer)
reset(x::AsyncStream)    = reset(x.buffer)
ismarked(x::AsyncStream) = ismarked(x.buffer)
