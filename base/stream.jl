# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Libc: RawFD, dup
if is_windows()
    import .Libc: WindowsRawSocket
end

## types ##
abstract IOServer
abstract LibuvServer <: IOServer
abstract LibuvStream <: IO

# IO
# +- AbstractIOBuffer{T<:AbstractArray{UInt8,1}} (not exported)
# +- AbstractPipe (not exported)
# .  +- Pipe
# .  +- Process (not exported)
# .  +- ProcessChain (not exported)
# +- Base64DecodePipe
# +- Base64EncodePipe
# +- BufferStream
# +- DevNullStream (not exported)
# +- Filesystem.File
# +- LibuvStream (not exported)
# .  +- PipeEndpoint (not exported)
# .  +- TCPSocket
# .  +- TTY (not exported)
# .  +- UDPSocket
# +- IOBuffer = Base.AbstractIOBuffer{Array{UInt8,1}}
# +- IOStream

# IOServer
# +- LibuvServer
# .  +- PipeServer
# .  +- TCPServer

# Redirectable = Union{IO, FileRedirect, Libc.RawFD} (not exported)

function stream_wait(x, c...) # for x::LibuvObject
    preserve_handle(x)
    try
        return wait(c...)
    finally
        unpreserve_handle(x)
    end
end

nb_available(s::LibuvStream) = nb_available(s.buffer)

function eof(s::LibuvStream)
    if isopen(s) # fast path
        nb_available(s) > 0 && return false
    else
        return nb_available(s) <= 0
    end
    wait_readnb(s,1)
    return !isopen(s) && nb_available(s) <= 0
end

const DEFAULT_READ_BUFFER_SZ = 10485760           # 10 MB

const StatusUninit      = 0 # handle is allocated, but not initialized
const StatusInit        = 1 # handle is valid, but not connected/active
const StatusConnecting  = 2 # handle is in process of connecting
const StatusOpen        = 3 # handle is usable
const StatusActive      = 4 # handle is listening for read/write/connect events
const StatusClosing     = 5 # handle is closing / being closed
const StatusClosed      = 6 # handle is closed
const StatusEOF         = 7 # handle is a TTY that has seen an EOF event
const StatusPaused      = 8 # handle is Active, but not consuming events, and will transition to Open if it receives an event
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
    elseif s == StatusPaused
        return "paused"
    elseif s == StatusClosing
        return "closing"
    elseif s == StatusClosed
        return "closed"
    elseif s == StatusEOF
        return "eof"
    end
    return "invalid status"
end

type PipeEndpoint <: LibuvStream
    handle::Ptr{Void}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    connectnotify::Condition
    closenotify::Condition
    sendbuf::Nullable{IOBuffer}
    lock::ReentrantLock
    throttle::Int

    PipeEndpoint() = PipeEndpoint(Libc.malloc(_sizeof_uv_named_pipe), StatusUninit)
    function PipeEndpoint(handle::Ptr{Void}, status)
        p = new(handle,
                status,
                PipeBuffer(),
                Condition(),
                Condition(),
                Condition(),
                nothing,
                ReentrantLock(),
                DEFAULT_READ_BUFFER_SZ)
        associate_julia_struct(handle, p)
        finalizer(p, uvfinalize)
        return p
    end
end

type PipeServer <: LibuvServer
    handle::Ptr{Void}
    status::Int
    connectnotify::Condition
    closenotify::Condition
    function PipeServer(handle::Ptr{Void}, status)
        p = new(handle,
                status,
                Condition(),
                Condition())
        associate_julia_struct(p.handle, p)
        finalizer(p, uvfinalize)
        return p
    end
end

typealias LibuvPipe Union{PipeEndpoint, PipeServer}

function PipeServer()
    p = PipeServer(Libc.malloc(_sizeof_uv_named_pipe), StatusUninit)
    return init_pipe!(p; readable=true)
end

type TTY <: LibuvStream
    handle::Ptr{Void}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    closenotify::Condition
    sendbuf::Nullable{IOBuffer}
    lock::ReentrantLock
    throttle::Int
    @static if is_windows(); ispty::Bool; end
    TTY() = TTY(Libc.malloc(_sizeof_uv_tty), StatusUninit)
    function TTY(handle::Ptr{Void}, status)
        tty = new(
            handle,
            status,
            PipeBuffer(),
            Condition(),
            Condition(),
            nothing, ReentrantLock(),
            DEFAULT_READ_BUFFER_SZ)
        associate_julia_struct(handle, tty)
        finalizer(tty, uvfinalize)
        @static if is_windows()
            tty.ispty = ccall(:jl_ispty, Cint, (Ptr{Void},), handle) != 0
        end
        return tty
    end
end

function TTY(fd::RawFD; readable::Bool = false)
    tty = TTY()
    # This needs to go after associate_julia_struct so that there
    # is no garbage in the ->data field
    err = ccall(:uv_tty_init, Int32, (Ptr{Void}, Ptr{Void}, Int32, Int32),
            eventloop(), tty.handle, fd.fd, readable)
    uv_error("TTY", err)
    tty.status = StatusOpen
    return tty
end

show(io::IO, stream::LibuvServer) = print(io, typeof(stream), "(",
    _fd(stream), " ",
    uv_status_string(stream), ")")
show(io::IO, stream::LibuvStream) = print(io, typeof(stream), "(",
    _fd(stream), " ",
    uv_status_string(stream), ", ",
    nb_available(stream.buffer)," bytes waiting)")

# Shared LibuvStream object interface

function isreadable(io::LibuvStream)
    nb_available(io) > 0 && return true
    isopen(io) || return false
    return ccall(:uv_is_readable, Cint, (Ptr{Void},), io.handle) != 0
end

function iswritable(io::LibuvStream)
    isopen(io) || return false
    io.status == StatusClosing && return false
    return ccall(:uv_is_writable, Cint, (Ptr{Void},), io.handle) != 0
end

lock(s::LibuvStream) = lock(s.lock)
unlock(s::LibuvStream) = unlock(s.lock)

uvtype(::LibuvStream) = UV_STREAM
uvhandle(stream::LibuvStream) = stream.handle
unsafe_convert(::Type{Ptr{Void}}, s::Union{LibuvStream, LibuvServer}) = s.handle

function init_stdio(handle::Ptr{Void})
    t = ccall(:jl_uv_handle_type, Int32, (Ptr{Void},), handle)
    if t == UV_FILE
        return fdio(ccall(:jl_uv_file_handle, Int32, (Ptr{Void},), handle))
#       Replace ios.c file with libuv file?
#       return File(RawFD(ccall(:jl_uv_file_handle,Int32,(Ptr{Void},),handle)))
    else
        if t == UV_TTY
            ret = TTY(handle, StatusOpen)
        elseif t == UV_TCP
            ret = TCPSocket(handle, StatusOpen)
        elseif t == UV_NAMED_PIPE
            ret = PipeEndpoint(handle, StatusOpen)
        else
            throw(ArgumentError("invalid stdio type: $t"))
        end
        return ret
    end
end

function isopen(x::Union{LibuvStream, LibuvServer})
    if x.status == StatusUninit || x.status == StatusInit
        throw(ArgumentError("$x is not initialized"))
    end
    x.status != StatusClosed && x.status != StatusEOF
end

function check_open(x::Union{LibuvStream, LibuvServer})
    if !isopen(x) || x.status == StatusClosing
        throw(ArgumentError("stream is closed or unusable"))
    end
end

function wait_connected(x::Union{LibuvStream, LibuvServer})
    check_open(x)
    while x.status == StatusConnecting
        stream_wait(x, x.connectnotify)
        check_open(x)
    end
end

function wait_readbyte(x::LibuvStream, c::UInt8)
    if isopen(x) # fast path
        search(x.buffer, c) > 0 && return
    else
        return
    end
    preserve_handle(x)
    try
        while isopen(x) && search(x.buffer, c) <= 0
            start_reading(x) # ensure we are reading
            wait(x.readnotify)
        end
    finally
        if isempty(x.readnotify.waitq)
            stop_reading(x) # stop reading iff there are currently no other read clients of the stream
        end
        unpreserve_handle(x)
    end
    nothing
end

function wait_readnb(x::LibuvStream, nb::Int)
    if isopen(x) # fast path
        nb_available(x.buffer) >= nb && return
    else
        return
    end
    oldthrottle = x.throttle
    preserve_handle(x)
    try
        while isopen(x) && nb_available(x.buffer) < nb
            x.throttle = max(nb, x.throttle)
            start_reading(x) # ensure we are reading
            wait(x.readnotify)
        end
    finally
        if isempty(x.readnotify.waitq)
            stop_reading(x) # stop reading iff there are currently no other read clients of the stream
        end
        if oldthrottle <= x.throttle <= nb
            x.throttle = oldthrottle
        end
        unpreserve_handle(x)
    end
    nothing
end

function wait_close(x::Union{LibuvStream, LibuvServer})
    if isopen(x)
        stream_wait(x, x.closenotify)
    end
    nothing
end

function close(stream::Union{LibuvStream, LibuvServer})
    if stream.status == StatusInit
        ccall(:jl_forceclose_uv, Void, (Ptr{Void},), stream.handle)
    elseif isopen(stream)
        if stream.status != StatusClosing
            ccall(:jl_close_uv, Void, (Ptr{Void},), stream.handle)
            stream.status = StatusClosing
        end
        if uv_handle_data(stream) != C_NULL
            stream_wait(stream, stream.closenotify)
        end
    end
    nothing
end

function uvfinalize(uv::Union{LibuvStream, LibuvServer})
    if uv.handle != C_NULL
        disassociate_julia_struct(uv.handle) # not going to call the usual close hooks
        if uv.status != StatusUninit
            close(uv)
        else
            Libc.free(uv.handle)
        end
        uv.status = StatusClosed
        uv.handle = C_NULL
    end
    nothing
end

if is_windows()
    ispty(s::TTY) = s.ispty
    ispty(s::IO) = false
end

"    displaysize(io) -> (lines, columns)
Return the nominal size of the screen that may be used for rendering output to this io object"
displaysize(io::IO) = displaysize()
displaysize() = (parse(Int, get(ENV, "LINES",   "24")),
                 parse(Int, get(ENV, "COLUMNS", "80")))::Tuple{Int, Int}

function displaysize(io::TTY)
    local h::Int, w::Int
    default_size = displaysize()

    @static if is_windows()
        if ispty(io)
            # io is actually a libuv pipe but a cygwin/msys2 pty
            try
                h, w = map(x -> parse(Int, x), split(readstring(open(Base.Cmd(String["stty", "size"]), "r", io)[1])))
                h > 0 || (h = default_size[1])
                w > 0 || (w = default_size[2])
                return h, w
            catch
                return default_size
            end
        end
    end

    s1 = Ref{Int32}(0)
    s2 = Ref{Int32}(0)
    Base.uv_error("size (TTY)", ccall(:uv_tty_get_winsize,
                                      Int32, (Ptr{Void}, Ptr{Int32}, Ptr{Int32}),
                                      io, s1, s2) != 0)
    w, h = s1[], s2[]
    h > 0 || (h = default_size[1])
    w > 0 || (w = default_size[2])
    return h, w
end


### Libuv callbacks ###

#from `connect`
function uv_connectcb(conn::Ptr{Void}, status::Cint)
    hand = ccall(:jl_uv_connect_handle, Ptr{Void}, (Ptr{Void},), conn)
    sock = @handle_as hand LibuvStream
    @assert sock.status == StatusConnecting
    if status >= 0
        sock.status = StatusOpen
        notify(sock.connectnotify)
    else
        sock.status = StatusInit
        err = UVError("connect", status)
        notify_error(sock.connectnotify, err)
    end
    Libc.free(conn)
    nothing
end

# from `listen`
function uv_connectioncb(stream::Ptr{Void}, status::Cint)
    sock = @handle_as stream LibuvServer
    if status >= 0
        notify(sock.connectnotify)
    else
        err = UVError("connection", status)
        notify_error(sock.connectnotify, err)
    end
    nothing
end

## BUFFER ##
## Allocate space in buffer (for immediate use)
function alloc_request(buffer::IOBuffer, recommended_size::UInt)
    ensureroom(buffer, Int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    nb = length(buffer.data) - ptr + 1
    return (pointer(buffer.data, ptr), nb)
end

notify_filled(buffer::IOBuffer, nread::Int, base::Ptr{Void}, len::UInt) = notify_filled(buffer, nread)

function notify_filled(buffer::IOBuffer, nread::Int)
    if buffer.append
        buffer.size += nread
    else
        buffer.ptr += nread
    end
end

alloc_buf_hook(stream::LibuvStream, size::UInt) = alloc_request(stream.buffer, UInt(size))

function uv_alloc_buf(handle::Ptr{Void}, size::Csize_t, buf::Ptr{Void})
    hd = uv_handle_data(handle)
    if hd == C_NULL
        ccall(:jl_uv_buf_set_len, Void, (Ptr{Void}, Csize_t), buf, 0)
        return nothing
    end
    stream = unsafe_pointer_to_objref(hd)::LibuvStream

    local data::Ptr{Void}, newsize::Csize_t
    if stream.status != StatusActive
        data = C_NULL
        newsize = 0
    else
        (data, newsize) = alloc_buf_hook(stream, UInt(size))
        if data == C_NULL
            newsize = 0
        end
    end

    ccall(:jl_uv_buf_set_base, Void, (Ptr{Void}, Ptr{Void}), buf, data)
    ccall(:jl_uv_buf_set_len, Void, (Ptr{Void}, Csize_t), buf, newsize)
    nothing
end

function uv_readcb(handle::Ptr{Void}, nread::Cssize_t, buf::Ptr{Void})
    stream_unknown_type = @handle_as handle LibuvStream
    nrequested = ccall(:jl_uv_buf_len, Csize_t, (Ptr{Void},), buf)
    function readcb_specialized(stream::LibuvStream, nread::Int, nrequested::UInt)
        if nread < 0
            if nread == UV_ENOBUFS && nrequested == 0
                # remind the client that stream.buffer is full
                notify(stream.readnotify)
            elseif nread == UV_EOF
                if isa(stream, TTY)
                    stream.status = StatusEOF # libuv called uv_stop_reading already
                    notify(stream.readnotify)
                    notify(stream.closenotify)
                elseif stream.status != StatusClosing
                    # begin shutdown of the stream
                    ccall(:jl_close_uv, Void, (Ptr{Void},), stream.handle)
                    stream.status = StatusClosing
                end
            else
                # This is a fatal connection error. Shutdown requests as per the usual
                # close function won't work and libuv will fail with an assertion failure
                ccall(:jl_forceclose_uv, Void, (Ptr{Void},), stream)
                notify_error(stream.readnotify, UVError("read", nread))
            end
        else
            notify_filled(stream.buffer, nread)
            notify(stream.readnotify)
        end

        # Stop background reading when
        # 1) there's nobody paying attention to the data we are reading
        # 2) we have accumulated a lot of unread data OR
        # 3) we have an alternate buffer that has reached its limit.
        if stream.status == StatusPaused ||
           (stream.status == StatusActive &&
            ((nb_available(stream.buffer) >= stream.throttle) ||
             (nb_available(stream.buffer) >= stream.buffer.maxsize)))
            # save cycles by stopping kernel notifications from arriving
            ccall(:uv_read_stop, Cint, (Ptr{Void},), stream)
            stream.status = StatusOpen
        end
        nothing
    end
    readcb_specialized(stream_unknown_type, Int(nread), UInt(nrequested))
end

function reseteof(x::TTY)
    if x.status == StatusEOF
        x.status = StatusOpen
    end
    nothing
end

function _uv_hook_close(uv::Union{LibuvStream, LibuvServer})
    uv.handle = C_NULL
    uv.status = StatusClosed
    # notify any listeners that exist on this libuv stream type
    notify(uv.closenotify)
    isdefined(uv, :readnotify) && notify(uv.readnotify)
    isdefined(uv, :connectnotify) && notify(uv.connectnotify)
    nothing
end


##########################################
# Pipe Abstraction
#  (composed of two half-pipes: .in and .out)
##########################################

type Pipe <: AbstractPipe
    in::PipeEndpoint # writable
    out::PipeEndpoint # readable
end
Pipe() = Pipe(PipeEndpoint(), PipeEndpoint())
pipe_reader(p::Pipe) = p.out
pipe_writer(p::Pipe) = p.in

function link_pipe(pipe::Pipe;
               julia_only_read = false,
               julia_only_write = false)
     link_pipe(pipe.out, julia_only_read, pipe.in, julia_only_write)
end

show(io::IO, stream::Pipe) = print(io,
    "Pipe(",
    _fd(stream.in), " ",
    uv_status_string(stream.in), " => ",
    _fd(stream.out), " ",
    uv_status_string(stream.out), ", ",
    nb_available(stream), " bytes waiting)")


## Functions for PipeEndpoint and PipeServer ##

function init_pipe!(pipe::LibuvPipe;
                    readable::Bool = false,
                    writable::Bool = false,
                    julia_only::Bool = true)
    if pipe.status != StatusUninit
        error("pipe is already initialized")
    end
    err = ccall(:jl_init_pipe, Cint,
        (Ptr{Void}, Int32, Int32, Int32),
        pipe.handle, writable, readable, julia_only)
    uv_error(
        if readable && writable
            "init_pipe(ipc)"
        elseif readable
            "init_pipe(read)"
        elseif writable
            "init_pipe(write)"
        else
            "init_pipe(none)"
        end, err)
    pipe.status = StatusInit
    return pipe
end

function _link_pipe(read_end::Ptr{Void}, write_end::Ptr{Void})
    uv_error("pipe_link",
        ccall(:uv_pipe_link, Int32, (Ptr{Void}, Ptr{Void}), read_end, write_end))
    nothing
end

function link_pipe(read_end::Ptr{Void}, readable_julia_only::Bool,
                   write_end::Ptr{Void}, writable_julia_only::Bool,
                   readpipe::PipeEndpoint, writepipe::PipeEndpoint)
    #make the pipe an unbuffered stream for now
    #TODO: this is probably not freeing memory properly after errors
    uv_error("init_pipe(read)",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    uv_error("init_pipe(write)",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end, write_end)
    nothing
end

function link_pipe(read_end::Ptr{Void}, readable_julia_only::Bool,
                   write_end::Ptr{Void}, writable_julia_only::Bool)
    uv_error("init_pipe(read)",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    uv_error("init_pipe(write)",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end,write_end)
    nothing
end

function link_pipe(read_end::PipeEndpoint, readable_julia_only::Bool,
                   write_end::Ptr{Void}, writable_julia_only::Bool)
    init_pipe!(read_end;
        readable = true, writable = false, julia_only = readable_julia_only)
    uv_error("init_pipe",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), write_end, 1, 0, writable_julia_only))
    _link_pipe(read_end.handle, write_end)
    read_end.status = StatusOpen
    nothing
end

function link_pipe(read_end::Ptr{Void}, readable_julia_only::Bool,
                   write_end::PipeEndpoint, writable_julia_only::Bool)
    uv_error("init_pipe",
        ccall(:jl_init_pipe, Cint, (Ptr{Void},Int32,Int32,Int32), read_end, 0, 1, readable_julia_only))
    init_pipe!(write_end;
        readable = false, writable = true, julia_only = writable_julia_only)
    _link_pipe(read_end, write_end.handle)
    write_end.status = StatusOpen
    nothing
end

function link_pipe(read_end::PipeEndpoint, readable_julia_only::Bool,
                   write_end::PipeEndpoint, writable_julia_only::Bool)
    init_pipe!(read_end;
        readable = true, writable = false, julia_only = readable_julia_only)
    init_pipe!(write_end;
        readable = false, writable = true, julia_only = writable_julia_only)
    _link_pipe(read_end.handle, write_end.handle)
    write_end.status = StatusOpen
    read_end.status = StatusOpen
    nothing
end

function close_pipe_sync(p::PipeEndpoint)
    ccall(:uv_pipe_close_sync, Void, (Ptr{Void},), p.handle)
    p.status = StatusClosed
    nothing
end

function close_pipe_sync(handle::Ptr{Void})
    return ccall(:uv_pipe_close_sync, Void, (Ptr{Void},), handle)
end

## Functions for any LibuvStream ##

# flow control

function start_reading(stream::LibuvStream)
    if stream.status == StatusOpen
        if !isreadable(stream)
            error("tried to read a stream that is not readable")
        end
        ret = ccall(:uv_read_start, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                    stream, uv_jl_alloc_buf::Ptr{Void}, uv_jl_readcb::Ptr{Void})
        stream.status = StatusActive
        return ret
    elseif stream.status == StatusPaused
        stream.status = StatusActive
        return Int32(0)
    elseif stream.status == StatusActive
        return Int32(0)
    else
        return Int32(-1)
    end
end

if is_windows()
    # the low performance version of stop_reading is required
    # on Windows due to a NT kernel bug that we can't use a blocking
    # stream for non-blocking (overlapped) calls,
    # and a ReadFile call blocking on one thread
    # causes all other operations on that stream to lockup
    function stop_reading(stream::LibuvStream)
        if stream.status == StatusActive
            ccall(:uv_read_stop, Cint, (Ptr{Void},), stream)
            stream.status = StatusOpen
        end
        nothing
    end
else
    function stop_reading(stream::LibuvStream)
        if stream.status == StatusActive
            stream.status = StatusPaused
        end
        nothing
    end
end

# bulk read / write

readbytes!(s::LibuvStream, a::Vector{UInt8}, nb = length(a)) = readbytes!(s, a, Int(nb))
function readbytes!(s::LibuvStream, a::Vector{UInt8}, nb::Int)
    sbuf = s.buffer
    @assert sbuf.seekable == false
    @assert sbuf.maxsize >= nb

    if nb_available(sbuf) >= nb
        return readbytes!(sbuf, a, nb)
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_readnb(s, nb)
        return readbytes!(sbuf, a, nb)
    else
        try
            stop_reading(s) # Just playing it safe, since we are going to switch buffers.
            newbuf = PipeBuffer(a, #=maxsize=# nb)
            newbuf.size = 0 # reset the write pointer to the beginning
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_readnb(s, Int(nb))
            compact(newbuf)
            return nb_available(newbuf)
        finally
            s.buffer = sbuf
            if !isempty(s.readnotify.waitq)
                start_reading(s) # resume reading iff there are currently other read clients of the stream
            end
        end
    end
    @assert false # unreachable
end

function read(stream::LibuvStream)
    wait_readnb(stream, typemax(Int))
    return take!(stream.buffer)
end

function unsafe_read(s::LibuvStream, p::Ptr{UInt8}, nb::UInt)
    sbuf = s.buffer
    @assert sbuf.seekable == false
    @assert sbuf.maxsize >= nb

    if nb_available(sbuf) >= nb
        return unsafe_read(sbuf, p, nb)
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_readnb(s, Int(nb))
        unsafe_read(sbuf, p, nb)
    else
        try
            stop_reading(s) # Just playing it safe, since we are going to switch buffers.
            newbuf = PipeBuffer(unsafe_wrap(Array, p, nb), #=maxsize=# Int(nb))
            newbuf.size = 0 # reset the write pointer to the beginning
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_readnb(s, Int(nb))
            nb == nb_available(newbuf) || throw(EOFError())
        finally
            s.buffer = sbuf
            if !isempty(s.readnotify.waitq)
                start_reading(s) # resume reading iff there are currently other read clients of the stream
            end
        end
    end
    nothing
end

function read(this::LibuvStream, ::Type{UInt8})
    wait_readnb(this, 1)
    buf = this.buffer
    @assert buf.seekable == false
    return read(buf, UInt8)
end

function readavailable(this::LibuvStream)
    wait_readnb(this, 1)
    buf = this.buffer
    @assert buf.seekable == false
    return take!(buf)
end

function readuntil(this::LibuvStream, c::UInt8)
    wait_readbyte(this, c)
    buf = this.buffer
    @assert buf.seekable == false
    return readuntil(buf, c)
end

uv_write(s::LibuvStream, p::Vector{UInt8}) = uv_write(s, pointer(p), UInt(sizeof(p)))
function uv_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    check_open(s)
    uvw = Libc.malloc(_sizeof_uv_write)
    uv_req_set_data(uvw,C_NULL)
    err = ccall(:jl_uv_write,
                Int32,
                (Ptr{Void}, Ptr{Void}, UInt, Ptr{Void}, Ptr{Void}),
                s, p, n, uvw,
                uv_jl_writecb_task::Ptr{Void})
    if err < 0
        Libc.free(uvw)
        uv_error("write", err)
    end
    ct = current_task()
    uv_req_set_data(uvw,ct)
    stream_wait(ct)
    return Int(n)
end

# Optimized send
# - smaller writes are buffered, final uv write on flush or when buffer full
# - large isbits arrays are unbuffered and written directly

function unsafe_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    if isnull(s.sendbuf)
        return uv_write(s, p, UInt(n))
    end

    buf = get(s.sendbuf)
    totb = nb_available(buf) + n
    if totb < buf.maxsize
        nb = unsafe_write(buf, p, n)
    else
        flush(s)
        if n > buf.maxsize
            nb = uv_write(s, p, n)
        else
            nb = unsafe_write(buf, p, n)
        end
    end
    return nb
end

function flush(s::LibuvStream)
    if isnull(s.sendbuf)
        return
    end
    buf = get(s.sendbuf)
    if nb_available(buf) > 0
        arr = take!(buf)        # Array of UInt8s
        uv_write(s, arr)
    end
    return
end

buffer_writes(s::LibuvStream, bufsize) = (s.sendbuf=PipeBuffer(bufsize); s)

## low-level calls to libuv ##

write(s::LibuvStream, b::UInt8) = write(s, Ref{UInt8}(b))

function uv_writecb_task(req::Ptr{Void}, status::Cint)
    d = uv_req_data(req)
    if d != C_NULL
        if status < 0
            err = UVError("write",status)
            schedule(unsafe_pointer_to_objref(d)::Task,err,error=true)
        else
            schedule(unsafe_pointer_to_objref(d)::Task)
        end
    end
    Libc.free(req)
    nothing
end

## server functions ##

function accept_nonblock(server::PipeServer,client::PipeEndpoint)
    if client.status != StatusInit
        error(client.status == StatusUninit ?
              "client is not initialized" :
              "client is already in use or has been closed")
    end
    err = ccall(:uv_accept, Int32, (Ptr{Void}, Ptr{Void}), server.handle, client.handle)
    if err == 0
        client.status = StatusOpen
    end
    return err
end

function accept_nonblock(server::PipeServer)
    client = init_pipe!(PipeEndpoint(); readable=true, writable=true, julia_only=true)
    uv_error("accept", accept_nonblock(server, client) != 0)
    return client
end

function accept(server::LibuvServer, client::LibuvStream)
    if server.status != StatusActive
        throw(ArgumentError("server not connected, make sure \"listen\" has been called"))
    end
    while isopen(server)
        err = accept_nonblock(server, client)
        if err == 0
            return client
        elseif err != UV_EAGAIN
            uv_error("accept", err)
        end
        stream_wait(server, server.connectnotify)
    end
    uv_error("accept", UV_ECONNABORTED)
end

const BACKLOG_DEFAULT = 511

function listen(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    uv_error("listen", trylisten(sock))
    return sock
end

function trylisten(sock::LibuvServer; backlog::Integer=BACKLOG_DEFAULT)
    check_open(sock)
    err = ccall(:uv_listen, Cint, (Ptr{Void}, Cint, Ptr{Void}),
                sock, backlog, uv_jl_connectioncb::Ptr{Void})
    sock.status = StatusActive
    return err
end

function bind(server::PipeServer, name::AbstractString)
    @assert server.status == StatusInit
    err = ccall(:uv_pipe_bind, Int32, (Ptr{Void}, Cstring),
                server, name)
    if err != 0
        if err != UV_EADDRINUSE && err != UV_EACCES
            #TODO: this codepath is currently not tested
            throw(UVError("bind",err))
        else
            return false
        end
    end
    server.status = StatusOpen
    return true
end

"""
    listen(path::AbstractString) -> PipeServer

Create and listen on a named pipe / UNIX domain socket.
"""
function listen(path::AbstractString)
    sock = PipeServer()
    bind(sock, path) || throw(ArgumentError("could not listen on path $path"))
    return listen(sock)
end

function connect!(sock::PipeEndpoint, path::AbstractString)
    @assert sock.status == StatusInit
    req = Libc.malloc(_sizeof_uv_connect)
    uv_req_set_data(req,C_NULL)
    ccall(:uv_pipe_connect, Void, (Ptr{Void}, Ptr{Void}, Cstring, Ptr{Void}), req, sock.handle, path, uv_jl_connectcb::Ptr{Void})
    sock.status = StatusConnecting
    return sock
end

function connect(sock::LibuvStream, args...)
    connect!(sock, args...)
    wait_connected(sock)
    return sock
end

# Libuv will internally reset read/writability, which is uses to
# mark that this is an invalid pipe.

"""
    connect(path::AbstractString) -> PipeEndpoint

Connect to the named pipe / UNIX domain socket at `path`.
"""
connect(path::AbstractString) = connect(init_pipe!(PipeEndpoint(); readable=false, writable=false, julia_only=true),path)

const OS_HANDLE = is_windows() ? WindowsRawSocket : RawFD
const INVALID_OS_HANDLE = is_windows() ? WindowsRawSocket(Ptr{Void}(-1)) : RawFD(-1)
_fd(x::IOStream) = RawFD(fd(x))
function _fd(x::Union{LibuvStream, LibuvServer})
    fd = Ref{OS_HANDLE}(INVALID_OS_HANDLE)
    if x.status != StatusUninit && x.status != StatusClosed
        err = ccall(:uv_fileno, Int32, (Ptr{Void}, Ptr{OS_HANDLE}), x.handle, fd)
        # handle errors by returning INVALID_OS_HANDLE
    end
    return fd[]
end

for (x, writable, unix_fd, c_symbol) in
        ((:STDIN, false, 0, :jl_uv_stdin),
         (:STDOUT, true, 1, :jl_uv_stdout),
         (:STDERR, true, 2, :jl_uv_stderr))
    f = Symbol("redirect_",lowercase(string(x)))
    _f = Symbol("_",f)
    @eval begin
        function ($_f)(stream)
            global $x
            @static if is_windows()
                ccall(:SetStdHandle,stdcall,Int32,(Int32,Ptr{Void}),
                    $(-10 - unix_fd), Libc._get_osfhandle(_fd(stream)).handle)
            else
                dup(_fd(stream),  RawFD($unix_fd))
            end
            $x = stream
        end
        function ($f)(handle::Union{LibuvStream,IOStream})
            $(_f)(handle)
            unsafe_store!(cglobal($(Expr(:quote,c_symbol)),Ptr{Void}),
                handle.handle)
            handle
        end
        function ($f)()
            read,write = (PipeEndpoint(), PipeEndpoint())
            link_pipe(read,$(writable),write,$(!writable))
            ($f)($(writable? :write : :read))
            (read,write)
        end
    end
end

"""
    redirect_stdout([stream]) -> (rd, wr)

Create a pipe to which all C and Julia level [`STDOUT`](@ref) output
will be redirected.
Returns a tuple `(rd, wr)` representing the pipe ends.
Data written to [`STDOUT`](@ref) may now be read from the `rd` end of
the pipe. The `wr` end is given for convenience in case the old
[`STDOUT`](@ref) object was cached by the user and needs to be replaced
elsewhere.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stdout

"""
    redirect_stderr([stream]) -> (rd, wr)

Like [`redirect_stdout`](@ref), but for [`STDERR`](@ref).

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stderr

"""
    redirect_stdin([stream]) -> (rd, wr)

Like [`redirect_stdout`](@ref), but for [`STDIN`](@ref).
Note that the order of the return tuple is still `(rd, wr)`,
i.e. data to be read from [`STDIN`](@ref) may be written to `wr`.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stdin

for (F,S) in ((:redirect_stdin, :STDIN), (:redirect_stdout, :STDOUT), (:redirect_stderr, :STDERR))
    @eval function $F(f::Function, stream)
        STDOLD = $S
        local ret
        $F(stream)
        try
            ret = f()
        finally
            $F(STDOLD)
        end
        ret
    end
end

"""
    redirect_stdout(f::Function, stream)

Run the function `f` while redirecting [`STDOUT`](@ref) to `stream`.
Upon completion, [`STDOUT`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stdout(f::Function, stream)

"""
    redirect_stderr(f::Function, stream)

Run the function `f` while redirecting [`STDERR`](@ref) to `stream`.
Upon completion, [`STDERR`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stderr(f::Function, stream)

"""
    redirect_stdin(f::Function, stream)

Run the function `f` while redirecting [`STDIN`](@ref) to `stream`.
Upon completion, [`STDIN`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a `TCPSocket`.
"""
redirect_stdin(f::Function, stream)

mark(x::LibuvStream)     = mark(x.buffer)
unmark(x::LibuvStream)   = unmark(x.buffer)
reset(x::LibuvStream)    = reset(x.buffer)
ismarked(x::LibuvStream) = ismarked(x.buffer)

# BufferStream's are non-OS streams, backed by a regular IOBuffer
type BufferStream <: LibuvStream
    buffer::IOBuffer
    r_c::Condition
    close_c::Condition
    is_open::Bool
    buffer_writes::Bool
    lock::ReentrantLock

    BufferStream() = new(PipeBuffer(), Condition(), Condition(), true, false, ReentrantLock())
end

isopen(s::BufferStream) = s.is_open
function close(s::BufferStream)
    s.is_open = false
    notify(s.r_c)
    notify(s.close_c)
    nothing
end
uvfinalize(s::BufferStream) = nothing

read(s::BufferStream, ::Type{UInt8}) = (wait_readnb(s, 1); read(s.buffer, UInt8))
unsafe_read(s::BufferStream, a::Ptr{UInt8}, nb::UInt) = (wait_readnb(s, Int(nb)); unsafe_read(s.buffer, a, nb))
nb_available(s::BufferStream) = nb_available(s.buffer)

isreadable(s::BufferStream) = s.buffer.readable
iswritable(s::BufferStream) = s.buffer.writable

function wait_readnb(s::BufferStream, nb::Int)
    while isopen(s) && nb_available(s.buffer) < nb
        wait(s.r_c)
    end
end

show(io::IO, s::BufferStream) = print(io,"BufferStream() bytes waiting:",nb_available(s.buffer),", isopen:", s.is_open)

function wait_readbyte(s::BufferStream, c::UInt8)
    while isopen(s) && search(s.buffer,c) <= 0
        wait(s.r_c)
    end
end

wait_close(s::BufferStream) = if isopen(s); wait(s.close_c); end
start_reading(s::BufferStream) = Int32(0)
stop_reading(s::BufferStream) = nothing

write(s::BufferStream, b::UInt8) = write(s, Ref{UInt8}(b))
function unsafe_write(s::BufferStream, p::Ptr{UInt8}, nb::UInt)
    rv = unsafe_write(s.buffer, p, nb)
    !(s.buffer_writes) && notify(s.r_c)
    return rv
end

function eof(s::BufferStream)
    wait_readnb(s, 1)
    return !isopen(s) && nb_available(s)<=0
end

# If buffer_writes is called, it will delay notifying waiters till a flush is called.
buffer_writes(s::BufferStream, bufsize=0) = (s.buffer_writes=true; s)
flush(s::BufferStream) = (notify(s.r_c); nothing)
