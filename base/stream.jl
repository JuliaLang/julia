# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Libc: RawFD, dup
if Sys.iswindows()
    import .Libc: WindowsRawSocket
    const OS_HANDLE = WindowsRawSocket
    const INVALID_OS_HANDLE = WindowsRawSocket(Ptr{Cvoid}(-1))
else
    const OS_HANDLE = RawFD
    const INVALID_OS_HANDLE = RawFD(-1)
end


## types ##
abstract type IOServer end
abstract type LibuvServer <: IOServer end
abstract type LibuvStream <: IO end


# IO
# +- GenericIOBuffer{T<:AbstractArray{UInt8,1}} (not exported)
# +- AbstractPipe (not exported)
# .  +- Pipe
# .  +- Process (not exported)
# .  +- ProcessChain (not exported)
# +- BufferStream
# +- DevNullStream (not exported)
# +- Filesystem.File
# +- LibuvStream (not exported)
# .  +- PipeEndpoint (not exported)
# .  +- TCPSocket
# .  +- TTY (not exported)
# .  +- UDPSocket
# +- IOBuffer = Base.GenericIOBuffer{Array{UInt8,1}}
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

bytesavailable(s::LibuvStream) = bytesavailable(s.buffer)

function eof(s::LibuvStream)
    if isopen(s) # fast path
        bytesavailable(s) > 0 && return false
    else
        return bytesavailable(s) <= 0
    end
    wait_readnb(s,1)
    return !isopen(s) && bytesavailable(s) <= 0
end

# Limit our default maximum read and buffer size,
# to avoid DoS-ing ourself into an OOM situation
const DEFAULT_READ_BUFFER_SZ = 10485760 # 10 MB

# manually limit our write size, if the OS doesn't support full-size writes
if Sys.iswindows()
    const MAX_OS_WRITE = UInt(0x1FF0_0000) # 511 MB (determined semi-empirically, limited to 31 MB on XP)
else
    const MAX_OS_WRITE = UInt(typemax(Csize_t))
end


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

mutable struct PipeEndpoint <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    connectnotify::Condition
    closenotify::Condition
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock
    throttle::Int
    function PipeEndpoint(handle::Ptr{Cvoid}, status)
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
        finalizer(uvfinalize, p)
        return p
    end
end

function PipeEndpoint()
    pipe = PipeEndpoint(Libc.malloc(_sizeof_uv_named_pipe), StatusUninit)
    err = ccall(:uv_pipe_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cint), eventloop(), pipe.handle, 0)
    uv_error("failed to create pipe endpoint", err)
    pipe.status = StatusInit
    return pipe
end

mutable struct TTY <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    buffer::IOBuffer
    readnotify::Condition
    closenotify::Condition
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock
    throttle::Int
    @static if Sys.iswindows(); ispty::Bool; end
    function TTY(handle::Ptr{Cvoid}, status)
        tty = new(
            handle,
            status,
            PipeBuffer(),
            Condition(),
            Condition(),
            nothing,
            ReentrantLock(),
            DEFAULT_READ_BUFFER_SZ)
        associate_julia_struct(handle, tty)
        finalizer(uvfinalize, tty)
        @static if Sys.iswindows()
            tty.ispty = ccall(:jl_ispty, Cint, (Ptr{Cvoid},), handle) != 0
        end
        return tty
    end
end

function TTY(fd::RawFD; readable::Bool = false)
    tty = TTY(Libc.malloc(_sizeof_uv_tty), StatusUninit)
    # This needs to go after associate_julia_struct so that there
    # is no garbage in the ->data field
    err = ccall(:uv_tty_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, RawFD, Int32),
        eventloop(), tty.handle, fd, readable)
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
    bytesavailable(stream.buffer), " bytes waiting)")

# Shared LibuvStream object interface

function isreadable(io::LibuvStream)
    bytesavailable(io) > 0 && return true
    isopen(io) || return false
    return ccall(:uv_is_readable, Cint, (Ptr{Cvoid},), io.handle) != 0
end

function iswritable(io::LibuvStream)
    isopen(io) || return false
    io.status == StatusClosing && return false
    return ccall(:uv_is_writable, Cint, (Ptr{Cvoid},), io.handle) != 0
end

lock(s::LibuvStream) = lock(s.lock)
unlock(s::LibuvStream) = unlock(s.lock)

rawhandle(stream::LibuvStream) = stream.handle
unsafe_convert(::Type{Ptr{Cvoid}}, s::Union{LibuvStream, LibuvServer}) = s.handle

const Sockets_mod = Ref{Module}()

function init_stdio(handle::Ptr{Cvoid})
    t = ccall(:jl_uv_handle_type, Int32, (Ptr{Cvoid},), handle)
    if t == UV_FILE
        fd = ccall(:jl_uv_file_handle, OS_HANDLE, (Ptr{Cvoid},), handle)
        # TODO: Replace ios.c file with libuv fs?
        # return File(fd)
        @static if Sys.iswindows()
            # TODO: Get ios.c to understand native handles
            fd = ccall(:_open_osfhandle, RawFD, (WindowsRawSocket, Int32), fd, 0)
        end
        # TODO: Get fdio to work natively with file descriptors instead of integers
        return fdio(cconvert(Cint, fd))
    elseif t == UV_TTY
        return TTY(handle, StatusOpen)
    elseif t == UV_TCP
        if !isassigned(Sockets_mod)
            Sockets_mod[] = Base.require(Base, :Sockets)
        end
        return Sockets_mod[].TCPSocket(handle, StatusOpen)
    elseif t == UV_NAMED_PIPE
        return PipeEndpoint(handle, StatusOpen)
    else
        throw(ArgumentError("invalid stdio type: $t"))
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
        occursin(c, x.buffer) && return
    else
        return
    end
    preserve_handle(x)
    try
        while isopen(x) && !occursin(c, x.buffer)
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
        bytesavailable(x.buffer) >= nb && return
    else
        return
    end
    oldthrottle = x.throttle
    preserve_handle(x)
    try
        while isopen(x) && bytesavailable(x.buffer) < nb
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
        ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
    elseif isopen(stream)
        if stream.status != StatusClosing
            ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
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

if Sys.iswindows()
    ispty(s::TTY) = s.ispty
    ispty(s::IO) = false
end

"""
    displaysize([io::IO]) -> (lines, columns)

Return the nominal size of the screen that may be used for rendering output to
this `IO` object.
If no input is provided, the environment variables `LINES` and `COLUMNS` are read.
If those are not set, a default size of `(24, 80)` is returned.

# Examples
```jldoctest
julia> withenv("LINES" => 30, "COLUMNS" => 100) do
           displaysize()
       end
(30, 100)
```

To get your TTY size,

```julia
julia> displaysize(stdout)
(34, 147)
```
"""
displaysize(io::IO) = displaysize()
displaysize() = (parse(Int, get(ENV, "LINES",   "24")),
                 parse(Int, get(ENV, "COLUMNS", "80")))::Tuple{Int, Int}

function displaysize(io::TTY)
    local h::Int, w::Int
    default_size = displaysize()

    @static if Sys.iswindows()
        if ispty(io)
            # io is actually a libuv pipe but a cygwin/msys2 pty
            try
                h, w = parse.(Int, split(read(open(Base.Cmd(String["stty", "size"]), "r", io).out, String)))
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
                                      Int32, (Ptr{Cvoid}, Ptr{Int32}, Ptr{Int32}),
                                      io, s1, s2) != 0)
    w, h = s1[], s2[]
    h > 0 || (h = default_size[1])
    w > 0 || (w = default_size[2])
    return h, w
end

in(key_value::Pair{Symbol,Bool}, ::TTY) = key_value.first === :color && key_value.second === have_color
haskey(::TTY, key::Symbol) = key === :color
getindex(::TTY, key::Symbol) = key === :color ? have_color : throw(KeyError(key))
get(::TTY, key::Symbol, default) = key === :color ? have_color : default

### Libuv callbacks ###

## BUFFER ##
## Allocate space in buffer (for immediate use)
function alloc_request(buffer::IOBuffer, recommended_size::UInt)
    ensureroom(buffer, Int(recommended_size))
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    nb = length(buffer.data) - ptr + 1
    return (pointer(buffer.data, ptr), nb)
end

notify_filled(buffer::IOBuffer, nread::Int, base::Ptr{Cvoid}, len::UInt) = notify_filled(buffer, nread)

function notify_filled(buffer::IOBuffer, nread::Int)
    if buffer.append
        buffer.size += nread
    else
        buffer.ptr += nread
    end
end

function alloc_buf_hook(stream::LibuvStream, size::UInt)
    throttle = UInt(stream.throttle)
    return alloc_request(stream.buffer, (size > throttle) ? throttle : size)
end

function uv_alloc_buf(handle::Ptr{Cvoid}, size::Csize_t, buf::Ptr{Cvoid})
    hd = uv_handle_data(handle)
    if hd == C_NULL
        ccall(:jl_uv_buf_set_len, Cvoid, (Ptr{Cvoid}, Csize_t), buf, 0)
        return nothing
    end
    stream = unsafe_pointer_to_objref(hd)::LibuvStream

    local data::Ptr{Cvoid}, newsize::Csize_t
    if stream.status != StatusActive
        data = C_NULL
        newsize = 0
    else
        (data, newsize) = alloc_buf_hook(stream, UInt(size))
        if data == C_NULL
            newsize = 0
        end
        # avoid aliasing of `nread` with `errno` in uv_readcb
        # or exceeding the Win32 maximum uv_buf_t len
        maxsize = @static Sys.iswindows() ? typemax(Cint) : typemax(Cssize_t)
        newsize > maxsize && (newsize = maxsize)
    end

    ccall(:jl_uv_buf_set_base, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), buf, data)
    ccall(:jl_uv_buf_set_len, Cvoid, (Ptr{Cvoid}, Csize_t), buf, newsize)
    nothing
end

function uv_readcb(handle::Ptr{Cvoid}, nread::Cssize_t, buf::Ptr{Cvoid})
    stream_unknown_type = @handle_as handle LibuvStream
    nrequested = ccall(:jl_uv_buf_len, Csize_t, (Ptr{Cvoid},), buf)
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
                    ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
                    stream.status = StatusClosing
                end
            else
                # This is a fatal connection error. Shutdown requests as per the usual
                # close function won't work and libuv will fail with an assertion failure
                ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), stream)
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
            ((bytesavailable(stream.buffer) >= stream.throttle) ||
             (bytesavailable(stream.buffer) >= stream.buffer.maxsize)))
            # save cycles by stopping kernel notifications from arriving
            ccall(:uv_read_stop, Cint, (Ptr{Cvoid},), stream)
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

mutable struct Pipe <: AbstractPipe
    in::PipeEndpoint # writable
    out::PipeEndpoint # readable
end

"""
Construct an uninitialized Pipe object.

The appropriate end of the pipe will be automatically initialized if
the object is used in process spawning. This can be useful to easily
obtain references in process pipelines, e.g.:

```
julia> err = Pipe()

# After this `err` will be initialized and you may read `foo`'s
# stderr from the `err` pipe.
julia> run(pipeline(pipeline(`foo`, stderr=err), `cat`), wait=false)
```
"""
Pipe() = Pipe(PipeEndpoint(), PipeEndpoint())
pipe_reader(p::Pipe) = p.out
pipe_writer(p::Pipe) = p.in

function link_pipe!(pipe::Pipe;
                    reader_supports_async = false,
                    writer_supports_async = false)
     link_pipe!(pipe.out, reader_supports_async, pipe.in, writer_supports_async)
     return pipe
end

show(io::IO, stream::Pipe) = print(io,
    "Pipe(",
    _fd(stream.in), " ",
    uv_status_string(stream.in), " => ",
    _fd(stream.out), " ",
    uv_status_string(stream.out), ", ",
    bytesavailable(stream), " bytes waiting)")


## Functions for PipeEndpoint and PipeServer ##

function open_pipe!(p::PipeEndpoint, handle::OS_HANDLE, readable::Bool, writable::Bool)
    if p.status != StatusInit
        error("pipe is already in use or has been closed")
    end
    err = ccall(:jl_pipe_open, Int32, (Ptr{Cvoid}, OS_HANDLE, Cint, Cint), p.handle, handle, readable, writable)
    uv_error("open_pipe", err)
    p.status = StatusOpen
    return p
end


function link_pipe!(read_end::PipeEndpoint, reader_supports_async::Bool,
                    write_end::PipeEndpoint, writer_supports_async::Bool)
    rd, wr = link_pipe(reader_supports_async, writer_supports_async)
    try
        try
            open_pipe!(read_end, rd, true, false)
        catch e
            close_pipe_sync(rd)
            rethrow(e)
        end
        read_end.status = StatusOpen
        open_pipe!(write_end, wr, false, true)
    catch e
        close_pipe_sync(wr)
        rethrow(e)
    end
    write_end.status = StatusOpen
    nothing
end

function link_pipe(reader_supports_async::Bool, writer_supports_async::Bool)
    UV_NONBLOCK_PIPE = 0x40
    fildes = Ref{Pair{OS_HANDLE, OS_HANDLE}}(INVALID_OS_HANDLE => INVALID_OS_HANDLE) # read (in) => write (out)
    err = ccall(:uv_pipe, Int32, (Ptr{Pair{OS_HANDLE, OS_HANDLE}}, Cint, Cint),
                fildes,
                reader_supports_async * UV_NONBLOCK_PIPE,
                writer_supports_async * UV_NONBLOCK_PIPE)
    uv_error("pipe", err)
    return fildes[]
end

if Sys.iswindows()
    function close_pipe_sync(handle::WindowsRawSocket)
        ccall(:CloseHandle, stdcall, Cint, (WindowsRawSocket,), handle)
        nothing
    end
else
    function close_pipe_sync(handle::RawFD)
        ccall(:close, Cint, (RawFD,), handle)
        nothing
    end
end

## Functions for any LibuvStream ##

# flow control

function start_reading(stream::LibuvStream)
    if stream.status == StatusOpen
        if !isreadable(stream)
            error("tried to read a stream that is not readable")
        end
        # libuv may call the alloc callback immediately
        # for a TTY on Windows, so ensure the status is set first
        stream.status = StatusActive
        ret = ccall(:uv_read_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
                    stream, uv_jl_alloc_buf::Ptr{Cvoid}, uv_jl_readcb::Ptr{Cvoid})
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

if Sys.iswindows()
    # the low performance version of stop_reading is required
    # on Windows due to a NT kernel bug that we can't use a blocking
    # stream for non-blocking (overlapped) calls,
    # and a ReadFile call blocking on one thread
    # causes all other operations on that stream to lockup
    function stop_reading(stream::LibuvStream)
        if stream.status == StatusActive
            stream.status = StatusOpen
            ccall(:uv_read_stop, Cint, (Ptr{Cvoid},), stream)
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

    if bytesavailable(sbuf) >= nb
        return readbytes!(sbuf, a, nb)
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_readnb(s, nb)
        return readbytes!(sbuf, a, nb)
    else
        try
            stop_reading(s) # Just playing it safe, since we are going to switch buffers.
            newbuf = PipeBuffer(a, maxsize = nb)
            newbuf.size = 0 # reset the write pointer to the beginning
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_readnb(s, Int(nb))
            compact(newbuf)
            return bytesavailable(newbuf)
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

    if bytesavailable(sbuf) >= nb
        return unsafe_read(sbuf, p, nb)
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_readnb(s, Int(nb))
        unsafe_read(sbuf, p, nb)
    else
        try
            stop_reading(s) # Just playing it safe, since we are going to switch buffers.
            newbuf = PipeBuffer(unsafe_wrap(Array, p, nb), maxsize = Int(nb))
            newbuf.size = 0 # reset the write pointer to the beginning
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_readnb(s, Int(nb))
            nb == bytesavailable(newbuf) || throw(EOFError())
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

function readuntil(this::LibuvStream, c::UInt8; keep::Bool=false)
    wait_readbyte(this, c)
    buf = this.buffer
    @assert buf.seekable == false
    return readuntil(buf, c, keep=keep)
end

uv_write(s::LibuvStream, p::Vector{UInt8}) = uv_write(s, pointer(p), UInt(sizeof(p)))

function uv_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    uvw = uv_write_async(s, p, n)
    ct = current_task()
    preserve_handle(ct)
    try
        # wait for the last chunk to complete (or error)
        # assume that any errors would be sticky,
        # (so we don't need to monitor the error status of the intermediate writes)
        uv_req_set_data(uvw, ct)
        wait()
    finally
        if uv_req_data(uvw) != C_NULL
            # uvw is still alive,
            # so make sure we won't get spurious notifications later
            uv_req_set_data(uvw, C_NULL)
        else
            # done with uvw
            Libc.free(uvw)
        end
        unpreserve_handle(ct)
    end
    return Int(n)
end

# helper function for uv_write that returns the uv_write_t struct for the write
# rather than waiting on it
function uv_write_async(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    check_open(s)
    while true
        uvw = Libc.malloc(_sizeof_uv_write)
        uv_req_set_data(uvw, C_NULL) # in case we get interrupted before arriving at the wait call
        nwrite = min(n, MAX_OS_WRITE) # split up the write into chunks the OS can handle.
        # TODO: use writev, when that is added to uv-win
        err = ccall(:jl_uv_write,
                    Int32,
                    (Ptr{Cvoid}, Ptr{Cvoid}, UInt, Ptr{Cvoid}, Ptr{Cvoid}),
                    s, p, nwrite, uvw,
                    uv_jl_writecb_task::Ptr{Cvoid})
        if err < 0
            Libc.free(uvw)
            uv_error("write", err)
        end
        n -= nwrite
        p += nwrite
        if n == 0
            return uvw
        end
    end
end


# Optimized send
# - smaller writes are buffered, final uv write on flush or when buffer full
# - large isbits arrays are unbuffered and written directly

function unsafe_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    if s.sendbuf === nothing
        return uv_write(s, p, UInt(n))
    end

    buf = s.sendbuf
    totb = bytesavailable(buf) + n
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
    buf = s.sendbuf
    if buf !== nothing
        if bytesavailable(buf) > 0
            arr = take!(buf)        # Array of UInt8s
            uv_write(s, arr)
            return
        end
    end
    uv_write(s, Ptr{UInt8}(Base.eventloop()), UInt(0)) # zero write from a random pointer to flush current queue
    return
end

buffer_writes(s::LibuvStream, bufsize) = (s.sendbuf=PipeBuffer(bufsize); s)

## low-level calls to libuv ##

function write(s::LibuvStream, b::UInt8)
    buf = s.sendbuf
    if buf !== nothing
        if bytesavailable(buf) + 1 < buf.maxsize
            return write(buf, b)
        end
    end
    return write(s, Ref{UInt8}(b))
end

function uv_writecb_task(req::Ptr{Cvoid}, status::Cint)
    d = uv_req_data(req)
    if d != C_NULL
        uv_req_set_data(req, C_NULL) # let the Task know we got the writecb
        t = unsafe_pointer_to_objref(d)::Task
        if status < 0
            err = UVError("write", status)
            schedule(t, err, error=true)
        else
            schedule(t)
        end
    else
        # no owner for this req, safe to just free it
        Libc.free(req)
    end
    nothing
end

_fd(x::IOStream) = RawFD(fd(x))

function _fd(x::Union{LibuvStream, LibuvServer})
    fd = Ref{OS_HANDLE}(INVALID_OS_HANDLE)
    if x.status != StatusUninit && x.status != StatusClosed
        err = ccall(:uv_fileno, Int32, (Ptr{Cvoid}, Ptr{OS_HANDLE}), x.handle, fd)
        # handle errors by returning INVALID_OS_HANDLE
    end
    return fd[]
end

for (x, writable, unix_fd, c_symbol) in
        ((:stdin, false, 0, :jl_uv_stdin),
         (:stdout, true, 1, :jl_uv_stdout),
         (:stderr, true, 2, :jl_uv_stderr))
    f = Symbol("redirect_", lowercase(string(x)))
    _f = Symbol("_", f)
    Ux = Symbol(uppercase(string(x)))
    @eval begin
        function ($_f)(stream)
            global $x, $Ux
            posix_fd = _fd(stream)
            @static if Sys.iswindows()
                ccall(:SetStdHandle, stdcall, Int32, (Int32, OS_HANDLE),
                    $(-10 - unix_fd), Libc._get_osfhandle(posix_fd))
            end
            dup(posix_fd, RawFD($unix_fd))
            $Ux = $x = stream
            nothing
        end
        function ($f)(handle::Union{LibuvStream, IOStream})
            $(_f)(handle)
            unsafe_store!(cglobal($(Expr(:quote, c_symbol)), Ptr{Cvoid}),
                handle.handle)
            return handle
        end
        function ($f)()
            p = link_pipe!(Pipe())
            read, write = p.out, p.in
            ($f)($(writable ? :write : :read))
            return (read, write)
        end
    end
end

"""
    redirect_stdout([stream]) -> (rd, wr)

Create a pipe to which all C and Julia level [`stdout`](@ref) output
will be redirected.
Returns a tuple `(rd, wr)` representing the pipe ends.
Data written to [`stdout`](@ref) may now be read from the `rd` end of
the pipe. The `wr` end is given for convenience in case the old
[`stdout`](@ref) object was cached by the user and needs to be replaced
elsewhere.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stdout

"""
    redirect_stderr([stream]) -> (rd, wr)

Like [`redirect_stdout`](@ref), but for [`stderr`](@ref).

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stderr

"""
    redirect_stdin([stream]) -> (rd, wr)

Like [`redirect_stdout`](@ref), but for [`stdin`](@ref).
Note that the order of the return tuple is still `(rd, wr)`,
i.e. data to be read from [`stdin`](@ref) may be written to `wr`.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stdin

for (F,S) in ((:redirect_stdin, :stdin), (:redirect_stdout, :stdout), (:redirect_stderr, :stderr))
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

Run the function `f` while redirecting [`stdout`](@ref) to `stream`.
Upon completion, [`stdout`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stdout(f::Function, stream)

"""
    redirect_stderr(f::Function, stream)

Run the function `f` while redirecting [`stderr`](@ref) to `stream`.
Upon completion, [`stderr`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stderr(f::Function, stream)

"""
    redirect_stdin(f::Function, stream)

Run the function `f` while redirecting [`stdin`](@ref) to `stream`.
Upon completion, [`stdin`](@ref) is restored to its prior setting.

!!! note
    `stream` must be a `TTY`, a `Pipe`, or a socket.
"""
redirect_stdin(f::Function, stream)

mark(x::LibuvStream)     = mark(x.buffer)
unmark(x::LibuvStream)   = unmark(x.buffer)
reset(x::LibuvStream)    = reset(x.buffer)
ismarked(x::LibuvStream) = ismarked(x.buffer)

function peek(s::LibuvStream)
    mark(s)
    try read(s, UInt8)
    finally
        reset(s)
    end
end

# BufferStream's are non-OS streams, backed by a regular IOBuffer
mutable struct BufferStream <: LibuvStream
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
bytesavailable(s::BufferStream) = bytesavailable(s.buffer)

isreadable(s::BufferStream) = s.buffer.readable
iswritable(s::BufferStream) = s.buffer.writable

function wait_readnb(s::BufferStream, nb::Int)
    while isopen(s) && bytesavailable(s.buffer) < nb
        wait(s.r_c)
    end
end

show(io::IO, s::BufferStream) = print(io,"BufferStream() bytes waiting:",bytesavailable(s.buffer),", isopen:", s.is_open)

function wait_readbyte(s::BufferStream, c::UInt8)
    while isopen(s) && !occursin(c, s.buffer)
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
    return !isopen(s) && bytesavailable(s)<=0
end

# If buffer_writes is called, it will delay notifying waiters till a flush is called.
buffer_writes(s::BufferStream, bufsize=0) = (s.buffer_writes=true; s)
flush(s::BufferStream) = (notify(s.r_c); nothing)
