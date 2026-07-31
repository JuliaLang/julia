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
"""
    LibuvServer

An abstract type for IOServers handled by libuv.

If `server isa LibuvServer`, it must obey the following interface:

- `server.handle` must be a `Ptr{Cvoid}`
- `server.status` must be an `Int`
- `server.cond` must be a `GenericCondition`
"""
abstract type LibuvServer <: IOServer end

function getproperty(server::LibuvServer, name::Symbol)
    if name === :handle
        return getfield(server, :handle)::Ptr{Cvoid}
    elseif name === :status
        return getfield(server, :status)::Int
    elseif name === :cond
        return getfield(server, :cond)::GenericCondition
    else
        return getfield(server, name)
    end
end

"""
    LibuvStream

An abstract type for IO streams handled by libuv.

If `stream isa LibuvStream`, it must obey the following interface:

- `stream.handle`, if present, must be a `Ptr{Cvoid}`
- `stream.status`, if present, must be an `Int`
- `stream.buffer`, if present, must be an `IOBuffer`
- `stream.sendbuf`, if present, must be a `Union{Nothing,IOBuffer}`
- `stream.cond`, if present, must be a `GenericCondition`
- `stream.lock`, if present, must be an `AbstractLock`
- `stream.throttle`, if present, must be an `Int`
"""
abstract type LibuvStream <: IO end

function getproperty(stream::LibuvStream, name::Symbol)
    if name === :handle
        return getfield(stream, :handle)::Ptr{Cvoid}
    elseif name === :status
        return getfield(stream, :status)::Int
    elseif name === :buffer
        return getfield(stream, :buffer)::IOBuffer
    elseif name === :sendbuf
        return getfield(stream, :sendbuf)::Union{Nothing,IOBuffer}
    elseif name === :cond
        return getfield(stream, :cond)::GenericCondition
    elseif name === :lock
        return getfield(stream, :lock)::AbstractLock
    elseif name === :throttle
        return getfield(stream, :throttle)::Int
    else
        return getfield(stream, name)
    end
end

# IO
# +- GenericIOBuffer{T<:AbstractVector{UInt8}} (not exported)
# +- AbstractPipe (not exported)
# .  +- Pipe
# .  +- Process (not exported)
# .  +- ProcessChain (not exported)
# +- DevNull (not exported)
# +- Filesystem.File
# +- LibuvStream (not exported)
# .  +- PipeEndpoint (not exported)
# .  +- TCPSocket
# .  +- TTY (not exported)
# .  +- UDPSocket
# .  +- BufferStream (FIXME: 2.0)
# +- IOBuffer = Base.GenericIOBuffer{Vector{UInt8}}
# +- IOStream

# IOServer
# +- LibuvServer
# .  +- PipeServer
# .  +- TCPServer

# Redirectable = Union{IO, FileRedirect, Libc.RawFD} (not exported)

bytesavailable(s::LibuvStream) = bytesavailable(s.buffer)

function eof(s::LibuvStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    bytesavailable(s) > 0 && return false
    wait_readnb(s, 1, resolve_cancel_token(cancel))
    # This function is race-y if used from multiple threads, but we guarantee
    # it to never return true until the stream is definitively exhausted
    # and that we won't return true if there's a readerror pending (it'll instead get thrown).
    # This requires some careful ordering here (TODO: atomic loads)
    bytesavailable(s) > 0 && return false
    open = isreadable(s) # must precede readerror check
    s.readerror === nothing || throw(s.readerror)
    return !open
end

# Limit our default maximum read and buffer size,
# to avoid DoS-ing ourself into an OOM situation
const DEFAULT_READ_BUFFER_SZ = 10485760 # 10 MB

# manually limit our write size, if the OS doesn't support full-size writes
if Sys.iswindows()
    const MAX_OS_WRITE = UInt(0x1FF0_0000) # 511 MB (determined semi-empirically, limited to 31 MB on XP)
else
    const MAX_OS_WRITE = UInt(0x7FFF_0000) # almost 2 GB (both macOS and linux have this kernel restriction, although only macOS documents it)
end


const StatusUninit      = 0 # handle is allocated, but not initialized
const StatusInit        = 1 # handle is valid, but not connected/active
const StatusConnecting  = 2 # handle is in process of connecting
const StatusOpen        = 3 # handle is usable
const StatusActive      = 4 # handle is listening for read/write/connect events
const StatusClosing     = 5 # handle is closing / being closed
const StatusClosed      = 6 # handle is closed
const StatusEOF         = 7 # handle is a TTY that has seen an EOF event (pretends to be closed until reseteof is called)
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
    cond::ThreadSynchronizer
    readerror::Any
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock # advisory lock
    throttle::Int
    function PipeEndpoint(handle::Ptr{Cvoid}, status)
        p = new(handle,
                status,
                PipeBuffer(),
                ThreadSynchronizer(),
                nothing,
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
    iolock_begin()
    err = ccall(:uv_pipe_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cint), eventloop(), pipe.handle, 0)
    uv_error("failed to create pipe endpoint", err)
    pipe.status = StatusInit
    iolock_end()
    return pipe
end

function PipeEndpoint(fd::OS_HANDLE)
    pipe = PipeEndpoint()
    return open_pipe!(pipe, fd)
end
if OS_HANDLE != RawFD
    PipeEndpoint(fd::RawFD) = PipeEndpoint(Libc._get_osfhandle(fd))
end


mutable struct TTY <: LibuvStream
    handle::Ptr{Cvoid}
    status::Int
    buffer::IOBuffer
    cond::ThreadSynchronizer
    readerror::Any
    sendbuf::Union{IOBuffer, Nothing}
    lock::ReentrantLock # advisory lock
    throttle::Int
    raw_lock::ReentrantLock # exclusive access to raw mode
    @static if Sys.iswindows(); ispty::Bool; end
    function TTY(handle::Ptr{Cvoid}, status)
        tty = new(
            handle,
            status,
            PipeBuffer(),
            ThreadSynchronizer(),
            nothing,
            nothing,
            ReentrantLock(),
            DEFAULT_READ_BUFFER_SZ,
            ReentrantLock())
        associate_julia_struct(handle, tty)
        finalizer(uvfinalize, tty)
        @static if Sys.iswindows()
            tty.ispty = ccall(:jl_ispty, Cint, (Ptr{Cvoid},), handle) != 0
        end
        return tty
    end
end

function TTY(fd::OS_HANDLE)
    tty = TTY(Libc.malloc(_sizeof_uv_tty), StatusUninit)
    iolock_begin()
    err = ccall(:uv_tty_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, OS_HANDLE, Int32),
        eventloop(), tty.handle, fd, 0)
    uv_error("TTY", err)
    tty.status = StatusOpen
    iolock_end()
    return tty
end
if OS_HANDLE != RawFD
    TTY(fd::RawFD) = TTY(Libc._get_osfhandle(fd))
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
    io.status == StatusEOF && return false
    return ccall(:uv_is_readable, Cint, (Ptr{Cvoid},), io.handle) != 0
end

function iswritable(io::LibuvStream)
    isopen(io) || return false
    io.status == StatusClosing && return false
    return ccall(:uv_is_writable, Cint, (Ptr{Cvoid},), io.handle) != 0
end

lock(s::LibuvStream) = lock(s.lock)
unlock(s::LibuvStream) = unlock(s.lock)

setup_stdio(stream::Union{LibuvStream, LibuvServer}, ::Bool) = (stream, false)
rawhandle(stream::Union{LibuvStream, LibuvServer}) = stream.handle
unsafe_convert(::Type{Ptr{Cvoid}}, s::Union{LibuvStream, LibuvServer}) = s.handle

function init_stdio(handle::Ptr{Cvoid})
    iolock_begin()
    t = ccall(:jl_uv_handle_type, Int32, (Ptr{Cvoid},), handle)
    local io
    if t == UV_FILE
        fd = ccall(:jl_uv_file_handle, OS_HANDLE, (Ptr{Cvoid},), handle)
        # TODO: Replace ios.c file with libuv fs?
        # return File(fd)
        @static if Sys.iswindows()
            # TODO: Get ios.c to understand native handles
            fd = ccall(:_open_osfhandle, RawFD, (WindowsRawSocket, Int32), fd, 0)
        end
        # TODO: Get fdio to work natively with file descriptors instead of integers
        io = fdio(cconvert(Cint, fd))
    elseif t == UV_TTY
        io = TTY(handle, StatusOpen)
    elseif t == UV_TCP
        Sockets = require_stdlib(PkgId(UUID((0x6462fe0b_24de_5631, 0x8697_dd941f90decc)), "Sockets"))
        io = Sockets.TCPSocket(handle, StatusOpen)
    elseif t == UV_NAMED_PIPE
        io = PipeEndpoint(handle, StatusOpen)
    else
        throw(ArgumentError("invalid stdio type: $t"))
    end
    iolock_end()
    return io
end

"""
    open(fd::OS_HANDLE)::IO

Take a raw file descriptor and wrap it in a Julia-aware IO type,
and take ownership of the fd handle.
Call `open(Libc.dup(fd))` to avoid the ownership capture
of the original handle.

!!! warning
    Do not call this on a handle that's already owned by some
    other part of the system.
"""
function open(h::OS_HANDLE)
    iolock_begin()
    t = ccall(:uv_guess_handle, Cint, (OS_HANDLE,), h)
    local io
    if t == UV_FILE
        @static if Sys.iswindows()
            # TODO: Get ios.c to understand native handles
            h = ccall(:_open_osfhandle, RawFD, (WindowsRawSocket, Int32), h, 0)
        end
        # TODO: Get fdio to work natively with file descriptors instead of integers
        io = fdio(cconvert(Cint, h))
    elseif t == UV_TTY
        io = TTY(h)
    elseif t == UV_TCP
        Sockets = require_stdlib(PkgId(UUID((0x6462fe0b_24de_5631, 0x8697_dd941f90decc)), "Sockets"))
        io = Sockets.TCPSocket(h)
    elseif t == UV_NAMED_PIPE
        io = PipeEndpoint(h)
        @static if Sys.iswindows()
            if ccall(:jl_ispty, Cint, (Ptr{Cvoid},), io.handle) != 0
                # replace the Julia `PipeEndpoint` type with a `TTY` type,
                # if we detect that this is a cygwin pty object
                pipe_handle, pipe_status = io.handle, io.status
                io.status = StatusClosed
                io.handle = C_NULL
                io = TTY(pipe_handle, pipe_status)
            end
        end
    else
        throw(ArgumentError("invalid stdio type: $t"))
    end
    iolock_end()
    return io
end

if OS_HANDLE != RawFD
    function open(fd::RawFD)
        h = Libc.dup(Libc._get_osfhandle(fd)) # make a dup to steal ownership away from msvcrt
        try
            io = open(h)
            ccall(:_close, Cint, (RawFD,), fd) # on success, destroy the old libc handle
            return io
        catch ex
            ccall(:CloseHandle, stdcall, Cint, (OS_HANDLE,), h) # on failure, destroy the new nt handle
            rethrow(ex)
        end
    end
end

function isopen(x::Union{LibuvStream, LibuvServer})
    if x.status == StatusUninit || x.status == StatusInit || x.handle === C_NULL
        throw(ArgumentError("stream not initialized"))
    end
    return x.status != StatusClosed
end

function check_open(x::Union{LibuvStream, LibuvServer})
    if !isopen(x) || x.status == StatusClosing
        throw(IOError("stream is closed or unusable", 0))
    end
end

function wait_readnb(x::LibuvStream, nb::Int, tok::MaybeToken=default_cancel_token())
    # fast path before iolock acquire
    bytesavailable(x.buffer) >= nb && return
    open = isopen(x) && x.status != StatusEOF # must precede readerror check
    x.readerror === nothing || throw(x.readerror)
    open || return
    iolock_begin()
    # repeat fast path after iolock acquire, before other expensive work
    bytesavailable(x.buffer) >= nb && (iolock_end(); return)
    open = isopen(x) && x.status != StatusEOF
    x.readerror === nothing || throw(x.readerror)
    open || (iolock_end(); return)
    # now do the "real" work
    oldthrottle = x.throttle
    preserve_handle(x)
    lock(x.cond)
    try
        while bytesavailable(x.buffer) < nb
            x.readerror === nothing || throw(x.readerror)
            isopen(x) || break
            x.status == StatusEOF && break
            x.throttle = max(nb, x.throttle)
            start_reading(x) # ensure we are reading
            iolock_end()
            wait(x.cond, tok)
            unlock(x.cond)
            iolock_begin()
            lock(x.cond)
        end
    finally
        if isempty(x.cond)
            stop_reading(x) # stop reading iff there are currently no other read clients of the stream
        end
        if oldthrottle <= x.throttle <= nb
            # if we're interleaving readers, we might not get back to the "original" throttle
            # but we consider that an acceptable "risk", since we can't be quite sure what the intended value is now
            x.throttle = oldthrottle
        end
        unpreserve_handle(x)
        unlock(x.cond)
    end
    iolock_end()
    nothing
end

## Waits on libuv requests
#
# A libuv request (write, shutdown, UDP send, DNS lookup) is issued under the
# iolock with its data field C_NULL and completes by a callback (also run
# under the iolock) that wakes the parked issuer through the standard
# wake-claim protocol. The request's data field tracks who owns freeing it:
#   pointer to the wait entry - a waiter is parked on the request;
#   C_NULL                    - the completion callback has run; the waiter
#                               owns the request;
#   UV_REQ_DETACHED           - the waiter departed (interrupted/refused
#                               wait, or fire-and-forget): the callback owns
#                               freeing it, plus any buffer root recorded in
#                               `_detached_uvreq_roots`.

# Sentinel for a uv request's data field: see above.
const UV_REQ_DETACHED = Ptr{Cvoid}(UInt(0x1))

# Aux flag on a uv write wait's *witness* slot (the slot whose owner is the
# stream; its aux is otherwise unused): set, under the iolock, immediately
# before Julia itself issues `uv_cancel` on a write request it keeps
# awaiting, so the completion callback can tell a Julia-requested
# cancellation (UV_ECANCELED is then the expected outcome, delivered as a
# partial-count wake) from a close-induced one (a real write error - libuv
# also fails queued writes with UV_ECANCELED when the stream closes).
# Cleared whenever the slot is released for reuse (_release_slot!/
# _clear_uv_witness!). The detach path (_end_uvreq_wait!) does not set it:
# there the request is detached in the same iolock critical section, so no
# callback can observe a waiter - the flag only matters to waited requests.
const _UVREQ_AUX_CANCEL_REQUESTED = UInt64(0x1)

function _mark_uvreq_cancel_requested!(w::WaitEntry, @nospecialize(witness))
    i = _find_slot(w, witness)
    i == 0 || _set_slot_aux!(w, i, _slot_aux(w, i) | _UVREQ_AUX_CANCEL_REQUESTED)
    return nothing
end

# Whether the waiter flagged this request's cancellation as its own. Read
# by the completion callback (under the iolock) *before* the claim releases
# the witness slot.
function _uvreq_cancel_requested(req::Ptr{Cvoid})
    d = uv_req_data(req)
    (d == C_NULL || d == UV_REQ_DETACHED) && return false
    w = unsafe_pointer_to_objref(d)::WaitEntry
    for slot in slots(w)
        o = slot.owner
        (o === nothing || o isa CancellationTokenSource) && continue
        return slot.aux & _UVREQ_AUX_CANCEL_REQUESTED != 0
    end
    return false
end

# Buffers of detached write requests. A detached request keeps referencing
# the caller's memory until its completion callback runs - long after the
# issuing frame, whose GC.@preserve was the buffer's only root, has unwound.
# When the write's Julia owner is known it is rooted here, keyed by the
# request, and released by the completion callback. Guarded by the iolock
# (callbacks run under it). Raw-pointer writes (the generic unsafe_write
# interface) have no discoverable owner - their contract is pointer validity
# for the duration of the call - and detaching them retains the pre-existing
# hazard; owner-carrying entry points avoid it.
const _detached_uvreq_roots = IdDict{Ptr{Cvoid}, Any}()

function _root_detached_uvreq!(req::Ptr{Cvoid}, @nospecialize(owner))
    owner === nothing || (_detached_uvreq_roots[req] = owner)
    return nothing
end
_unroot_detached_uvreq!(req::Ptr{Cvoid}) =
    (isempty(_detached_uvreq_roots) || delete!(_detached_uvreq_roots, req); nothing)

# The entry check of a cancellable uv operation, made under the iolock
# before the request is issued: when `src` is already cancelled, release the
# iolock and throw the request - before the operation has any side effects.
function _iolocked_checkcancel(src::Union{Nothing, CancellationTokenSource})
    if src !== nothing && iscancelled(src)
        iolock_end()
        checkcancel(src)
    end
    return nothing
end

# Arm the wait for the issued uv request `req`: pick the wait entry (with a
# cancellation-source slot when governed by `src`), arm it, record `witness`
# (the waitee identity - the handle, or the request itself when there is
# none) as the entry's reuse gate, and point the request at the entry.
# Leaves the caller's iolock held, inside a sigatomic section.
function _begin_uvreq_wait!(src::Union{Nothing, CancellationTokenSource},
                            @nospecialize(witness), req::Ptr{Cvoid})
    ct = current_task()
    w = src === nothing ? _cached_wait_entry(ct) : _cancel_wait_entry(ct, src, 0x00)
    _arm_wait(ct, w)
    _set_wait_witness!(w, witness)
    preserve_handle(ct)
    sigatomic_begin()
    uv_req_set_data(req, pointer_from_objref(w))
    return w
end

# Wind down a uv request wait (caller holds the iolock, at the sigatomic
# level of the wait): resolve the request's ownership, release the wait
# witness, disarm the registration (a no-op when a claimer already did),
# and unwind the iolock, the sigatomic section, and the handle preservation.
# When the completion callback has already run (data == C_NULL) the request
# is ours to free; otherwise - the wait was interrupted or refused, or the
# task was resumed by an unexpected `schedule` - the request is detached for
# the callback to free, `uv_cancel`ed first when `trycancel` is set (so e.g.
# an abandoned write stops spamming the stream), with `owner` kept rooted
# until then. The sticky source registration needs no cleanup at all.
function _end_uvreq_wait!(w::WaitEntry, @nospecialize(witness), req::Ptr{Cvoid},
                          trycancel::Bool, @nospecialize(owner))
    ct = current_task()
    if uv_req_data(req) == C_NULL
        Libc.free(req)
    else
        trycancel && ccall(:uv_cancel, Cint, (Ptr{Cvoid},), req) # ignore any errors
        uv_req_set_data(req, UV_REQ_DETACHED)
        _root_detached_uvreq!(req, owner)
    end
    _clear_wait_witness!(w, witness)
    @atomicreplace ct.waiting_on w => nothing
    # Drop a claimed-and-enqueued wake an interrupted teardown will never
    # consume. The completion callback claims and schedules under the
    # iolock this function holds, so the drop is deterministic here (cf.
    # the interrupted cleanup in base/park.jl).
    q = ct.queue
    q === nothing || list_deletefirst!(q::StickyWorkqueue, ct)
    iolock_end()
    sigatomic_end()
    unpreserve_handle(ct)
    return nothing
end

## An in-flight libuv request as a waitable (see base/park.jl): the
## held-resource kind. The caller issues the request under the iolock and
## keeps it held into `park!` - the completion callback runs under that
## same lock, so the recheck is vacuous - and the request's data pointer
## doubles as the completion/detach handshake: the dequeue resolves its
## ownership on every exit path (callback already ran => we free it;
## otherwise it is detached to the callback, `uv_cancel`ed when
## requested, with the write's buffer kept rooted until then). The
## dequeue also consumes the reacquired iolock/sigatomic bracket.
struct UvReqWait
    req::Ptr{Cvoid}
    witness::Any      # the waitee identity (the handle, or the request)
    owner::Any        # buffer root for a detached request
    trycancel::Bool
end

function wait_enqueue!(x::UvReqWait, w::WaitEntry, first::Bool)
    ct = current_task()
    _set_wait_witness!(w, x.witness)
    preserve_handle(ct)
    sigatomic_begin()
    uv_req_set_data(x.req, pointer_from_objref(w))
    return true
end

wait_release!(x::UvReqWait) = (iolock_end(); sigatomic_end(); nothing)
wait_reacquire!(x::UvReqWait, token) = (sigatomic_begin(); iolock_begin(); nothing)

function wait_dequeue!(x::UvReqWait, w::WaitEntry, why::UInt8)
    ct = current_task()
    req = x.req
    if uv_req_data(req) == C_NULL
        Libc.free(req)
    else
        x.trycancel && ccall(:uv_cancel, Cint, (Ptr{Cvoid},), req) # ignore any errors
        uv_req_set_data(req, UV_REQ_DETACHED)
        _root_detached_uvreq!(req, x.owner)
    end
    _clear_wait_witness!(w, x.witness)
    iolock_end()
    sigatomic_end()
    unpreserve_handle(ct)
    return nothing
end

# Park on the completion of the issued uv request `req` and return the value
# its completion callback delivers. A cancellation of `src` interrupts the
# wait and throws the CancellationRequest - without parking at all when
# `src` got cancelled between the caller's entry check and the registration
# here; either way the request is left to its callback (see the dequeue
# above). The caller must have issued `req` under the still-held iolock -
# so the callback cannot have run yet - and gets it back with the iolock
# released.
function _wait_uvreq(src::Union{Nothing, CancellationTokenSource}, @nospecialize(witness),
                     req::Ptr{Cvoid}, trycancel::Bool, @nospecialize(owner))
    ct = current_task()
    uvw = UvReqWait(req, witness, owner, trycancel)
    if src === nothing
        return park!((uvw,), _cached_wait_entry(ct), true, false)
    else
        return park!((uvw, SourceWait(src, 0x00)), _cancel_wait_entry(ct, src, 0x00), true, false)
    end
end

# Completion-callback side of the wait (runs under the iolock): resolve
# request ownership and claim the parked waiter's wake. Returns the task to
# schedule, or `nothing` when there is nobody to wake - either no waiter
# remains (fire-and-forget, or the waiter departed and detached the request,
# which is freed here together with any recorded buffer root), or a
# canceller claimed the wake first (the waiter's teardown then observes
# data == C_NULL and takes over the request).
function _claim_uvreq_waiter(req::Ptr{Cvoid})
    d = uv_req_data(req)
    if d == C_NULL || d == UV_REQ_DETACHED
        _unroot_detached_uvreq!(req)
        Libc.free(req)
        return nothing
    end
    # mark the callback as done; the waiter (which inspects the request
    # under the iolock) owns freeing it
    uv_req_set_data(req, C_NULL)
    w = unsafe_pointer_to_objref(d)::WaitEntry
    t = @atomic :monotonic w.task
    if t isa Task && claim_wait(t, w)
        _clear_uv_witness!(w)
        return t
    end
    return nothing
end

function closewrite(s::LibuvStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    iolock_begin()
    if !iswritable(s)
        iolock_end()
        return
    end
    src = cancel_source(resolve_cancel_token(cancel))
    # entry check: throw before issuing the shutdown request
    _iolocked_checkcancel(src)
    req = Libc.malloc(_sizeof_uv_shutdown)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    err = ccall(:uv_shutdown, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
                req, s, @cfunction(uv_shutdowncb_task, Cvoid, (Ptr{Cvoid}, Cint)))
    if err < 0
        Libc.free(req)
        uv_error("shutdown", err)
    end
    # A shutdown request itself cannot be cancelled, so an interrupted wait
    # detaches it (its completion callback frees it).
    status = _wait_uvreq(src, s, req, false, nothing)::Cint
    if isopen(s)
        if status < 0 || ccall(:uv_is_readable, Cint, (Ptr{Cvoid},), s.handle) == 0
            close(s)
        end
    end
    if status < 0
        throw(_UVError("shutdown", status))
    end
    nothing
end

function wait_close(x::Union{LibuvStream, LibuvServer})
    preserve_handle(x)
    lock(x.cond)
    try
        while isopen(x)
            # close is the cleanup primitive: its completion wait is shielded
            # from cancellation (completion depends only on the event loop,
            # not on any peer, so this wait is bounded)
            wait(x.cond, nothing)
        end
    finally
        unlock(x.cond)
        unpreserve_handle(x)
    end
    nothing
end

function close(stream::Union{LibuvStream, LibuvServer})
    iolock_begin()
    if stream.status == StatusInit
        ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
        stream.status = StatusClosing
    elseif isopen(stream)
        if stream.status != StatusClosing
            ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
            stream.status = StatusClosing
        end
    end
    iolock_end()
    wait_close(stream)
    nothing
end

function uvfinalize(uv::Union{LibuvStream, LibuvServer})
    iolock_begin()
    if uv.handle != C_NULL
        disassociate_julia_struct(uv.handle) # not going to call the usual close hooks (so preserve_handle is not needed)
        if uv.status == StatusUninit
            Libc.free(uv.handle)
        elseif uv.status == StatusInit
            ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), uv.handle)
        elseif isopen(uv)
            if uv.status != StatusClosing
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), uv.handle)
            end
        elseif uv.status == StatusClosed
            Libc.free(uv.handle)
        end
        uv.handle = C_NULL
        uv.status = StatusClosed
    end
    iolock_end()
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

```julia-repl
julia> displaysize(stdout)
(34, 147)
```
"""
displaysize(io::IO) = displaysize()
displaysize() = (parse(Int, get(ENV, "LINES",   "24")),
                 parse(Int, get(ENV, "COLUMNS", "80")))::Tuple{Int, Int}

# This is a fancy way to de-specialize a call to `displaysize(io::IO)`
# which is unfortunately invalidated by REPL
#  (https://github.com/JuliaLang/julia/issues/56080)
#
# This makes the call less efficient, but avoids being invalidated by REPL.
displaysize_(io::IO) = Base.invoke_in_world(Base.tls_world_age(), displaysize, io)::Tuple{Int,Int}

function displaysize(io::TTY)
    check_open(io)

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
    iolock_begin()
    check_open(io)
    Base.uv_error("size (TTY)", ccall(:uv_tty_get_winsize,
                                      Int32, (Ptr{Cvoid}, Ptr{Int32}, Ptr{Int32}),
                                      io, s1, s2) != 0)
    iolock_end()
    w, h = s1[], s2[]
    h > 0 || (h = default_size[1])
    w > 0 || (w = default_size[2])
    return h, w
end

### Libuv callbacks ###

## BUFFER ##
## Allocate space in buffer (for immediate use)
function alloc_request(buffer::IOBuffer, recommended_size::UInt)
    ensureroom(buffer, recommended_size)
    ptr = buffer.append ? buffer.size + 1 : buffer.ptr
    start_offset = ptr - 1
    nb = max(0, min(length(buffer.data) - start_offset, buffer.maxsize - (start_offset - get_offset(buffer))))
    return (Ptr{Cvoid}(pointer(buffer.data, ptr)), nb)
end

notify_filled(buffer::IOBuffer, nread::Int, base::Ptr{Cvoid}, len::UInt) = notify_filled(buffer, nread)

function notify_filled(buffer::IOBuffer, nread::Int)
    if buffer.append
        buffer.size += nread
    else
        buffer.ptr += nread
        buffer.size = max(buffer.size, buffer.ptr - 1)
    end
    nothing
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
        lock(stream.cond)
        if nread < 0
            if nread == UV_ENOBUFS && nrequested == 0
                # remind the client that stream.buffer is full
                notify(stream.cond)
            elseif nread == UV_EOF # libuv called uv_stop_reading already
                if stream.status != StatusClosing
                    stream.status = StatusEOF
                    notify(stream.cond)
                    if stream isa TTY
                        # stream can still be used by reseteof (or possibly write)
                    elseif !(stream isa PipeEndpoint) && ccall(:uv_is_writable, Cint, (Ptr{Cvoid},), stream.handle) != 0
                        # stream can still be used by write
                    else
                        # underlying stream is no longer useful: begin finalization
                        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
                        stream.status = StatusClosing
                    end
                end
            else
                stream.readerror = _UVError("read", nread)
                notify(stream.cond)
                # This is a fatal connection error
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), stream.handle)
                stream.status = StatusClosing
            end
        else
            notify_filled(stream.buffer, nread)
            notify(stream.cond)
        end
        unlock(stream.cond)

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
    nothing
end

function reseteof(x::TTY)
    iolock_begin()
    if x.status == StatusEOF
        x.status = StatusOpen
    end
    iolock_end()
    nothing
end

function _uv_hook_close(uv::Union{LibuvStream, LibuvServer})
    lock(uv.cond)
    try
        uv.status = StatusClosed
        # notify any listeners that exist on this libuv stream type
        notify(uv.cond)
    finally
        unlock(uv.cond)
    end
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
    Pipe()

Construct an uninitialized Pipe object, especially for IO communication between multiple processes.

The appropriate end of the pipe will be automatically initialized if the object is used in
process spawning. This can be useful to easily obtain references in process pipelines, e.g.:

```
julia> err = Pipe()

# After this `err` will be initialized and you may read `foo`'s
# stderr from the `err` pipe, or pass `err` to other pipelines.
julia> run(pipeline(pipeline(`foo`, stderr=err), `cat`), wait=false)

# Now destroy the write half of the pipe, so that the read half will get EOF
julia> closewrite(err)

julia> read(err, String)
"stderr messages"
```

See also [`Base.link_pipe!`](@ref).
"""
Pipe() = Pipe(PipeEndpoint(), PipeEndpoint())
pipe_reader(p::Pipe) = p.out
pipe_writer(p::Pipe) = p.in

"""
    link_pipe!(pipe; reader_supports_async=false, writer_supports_async=false)

Initialize `pipe` and link the `in` endpoint to the `out` endpoint. The keyword
arguments `reader_supports_async`/`writer_supports_async` correspond to
`OVERLAPPED` on Windows and `O_NONBLOCK` on POSIX systems. They should be `true`
unless they'll be used by an external program (e.g. the output of a command
executed with [`run`](@ref)).
"""
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

closewrite(pipe::Pipe) = close(pipe.in)

## Functions for PipeEndpoint and PipeServer ##

function open_pipe!(p::PipeEndpoint, handle::OS_HANDLE)
    iolock_begin()
    if p.status != StatusInit
        error("pipe is already in use or has been closed")
    end
    err = ccall(:uv_pipe_open, Int32, (Ptr{Cvoid}, OS_HANDLE), p.handle, handle)
    uv_error("pipe_open", err)
    p.status = StatusOpen
    iolock_end()
    return p
end


function link_pipe!(read_end::PipeEndpoint, reader_supports_async::Bool,
                    write_end::PipeEndpoint, writer_supports_async::Bool)
    rd, wr = link_pipe(reader_supports_async, writer_supports_async)
    try
        try
            open_pipe!(read_end, rd)
        catch
            close_pipe_sync(rd)
            rethrow()
        end
        open_pipe!(write_end, wr)
    catch
        close_pipe_sync(wr)
        rethrow()
    end
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
    iolock_begin()
    if stream.status == StatusOpen
        if !isreadable(stream)
            error("tried to read a stream that is not readable")
        end
        # libuv may call the alloc callback immediately
        # for a TTY on Windows, so ensure the status is set first
        stream.status = StatusActive
        ret = ccall(:uv_read_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
                    stream, @cfunction(uv_alloc_buf, Cvoid, (Ptr{Cvoid}, Csize_t, Ptr{Cvoid})),
                    @cfunction(uv_readcb, Cvoid, (Ptr{Cvoid}, Cssize_t, Ptr{Cvoid})))
    elseif stream.status == StatusPaused
        stream.status = StatusActive
        ret = Int32(0)
    elseif stream.status == StatusActive
        ret = Int32(0)
    else
        ret = Int32(-1)
    end
    iolock_end()
    return ret
end

if Sys.iswindows()
    # the low performance version of stop_reading is required
    # on Windows due to an NT kernel bug that we can't use a blocking
    # stream for non-blocking (overlapped) calls,
    # and a ReadFile call blocking on one thread
    # causes all other operations on that stream to lockup
    function stop_reading(stream::LibuvStream)
        iolock_begin()
        if stream.status == StatusActive
            stream.status = StatusOpen
            ccall(:uv_read_stop, Cint, (Ptr{Cvoid},), stream)
        end
        iolock_end()
        nothing
    end
else
    function stop_reading(stream::LibuvStream)
        iolock_begin()
        if stream.status == StatusActive
            stream.status = StatusPaused
        end
        iolock_end()
        nothing
    end
end

# bulk read / write

readbytes!(s::LibuvStream, a::Vector{UInt8}, nb = length(a)) = readbytes!(s, a, Int(nb))
function readbytes!(s::LibuvStream, a::Vector{UInt8}, nb::Int; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    iolock_begin()
    sbuf = s.buffer
    @assert sbuf.seekable == false "buffer should not be seekable"
    @assert sbuf.maxsize >= nb "insufficient buffer size"

    function wait_locked(s, buf, nb, tok)
        while bytesavailable(buf) < nb
            s.readerror === nothing || throw(s.readerror)
            isopen(s) || break
            s.status != StatusEOF || break
            iolock_end()
            wait_readnb(s, nb, tok)
            iolock_begin()
        end
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_locked(s, sbuf, nb, tok)
    end
    if bytesavailable(sbuf) >= nb
        nread = readbytes!(sbuf, a, nb)
    else
        initsize = length(a)
        newbuf = _truncated_pipebuffer(a; maxsize=nb)
        nread = try
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_locked(s, newbuf, nb, tok)
            bytesavailable(newbuf)
        finally
            s.buffer = sbuf
        end
        _take!(a, _unsafe_take!(newbuf))
        length(a) >= initsize || resize!(a, initsize)
    end
    iolock_end()
    return nread
end

function read(stream::LibuvStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    wait_readnb(stream, typemax(Int), resolve_cancel_token(precheck_cancel_arg(cancel)))
    iolock_begin()
    bytes = take!(stream.buffer)
    iolock_end()
    return bytes
end

function unsafe_read(s::LibuvStream, p::Ptr{UInt8}, nb::UInt; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    iolock_begin()
    sbuf = s.buffer
    @assert sbuf.seekable == false "buffer should not be seekable"
    @assert sbuf.maxsize >= nb "insufficient buffer size"

    function wait_locked(s, buf, nb, tok)
        while bytesavailable(buf) < nb
            s.readerror === nothing || throw(s.readerror)
            isopen(s) || throw(EOFError())
            s.status != StatusEOF || throw(EOFError())
            iolock_end()
            wait_readnb(s, nb, tok)
            iolock_begin()
        end
    end

    if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
        wait_locked(s, sbuf, Int(nb), tok)
    end
    if bytesavailable(sbuf) >= nb
        unsafe_read(sbuf, p, nb)
    else
        newbuf = _truncated_pipebuffer(unsafe_wrap(Array, p, nb); maxsize=Int(nb))
        try
            s.buffer = newbuf
            write(newbuf, sbuf)
            wait_locked(s, newbuf, Int(nb), tok)
        finally
            s.buffer = sbuf
        end
    end
    iolock_end()
    nothing
end

function read(this::LibuvStream, ::Type{UInt8}; cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    iolock_begin()
    sbuf = this.buffer
    @assert sbuf.seekable == false "buffer should not be seekable"
    while bytesavailable(sbuf) < 1
        iolock_end()
        eof(this; cancel) && throw(EOFError())
        iolock_begin()
    end
    c = read(sbuf, UInt8)
    iolock_end()
    return c
end

function readavailable(this::LibuvStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    wait_readnb(this, 1, resolve_cancel_token(precheck_cancel_arg(cancel))) # unlike the other `read` family of functions, this one doesn't guarantee error reporting
    iolock_begin()
    buf = this.buffer
    @assert buf.seekable == false "buffer should not be seekable"
    bytes = take!(buf)
    iolock_end()
    return bytes
end

function copyuntil(out::IO, x::LibuvStream, c::UInt8; keep::Bool=false, cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    iolock_begin()
    buf = x.buffer
    @assert buf.seekable == false "buffer should not be seekable"
    if !occursin(c, buf) # fast path checks first
        x.readerror === nothing || throw(x.readerror)
        if isopen(x) && x.status != StatusEOF
            preserve_handle(x)
            lock(x.cond)
            try
                while !occursin(c, x.buffer)
                    x.readerror === nothing || throw(x.readerror)
                    isopen(x) || break
                    x.status != StatusEOF || break
                    start_reading(x) # ensure we are reading
                    iolock_end()
                    wait(x.cond, tok)
                    unlock(x.cond)
                    iolock_begin()
                    lock(x.cond)
                end
            finally
                if isempty(x.cond)
                    stop_reading(x) # stop reading iff there are currently no other read clients of the stream
                end
                unlock(x.cond)
                unpreserve_handle(x)
            end
        end
    end
    # thread the resolved token (or explicit shield): the in-memory copy's
    # write to `out` is itself token-gated
    copyuntil(out, buf, c; keep, cancel=tok)
    iolock_end()
    return out
end

uv_write(s::LibuvStream, p::Vector{UInt8}, cancel::CancelTokenArg=DEFAULT_CANCEL) =
    GC.@preserve p _uv_write_owned(s, pointer(p), UInt(sizeof(p)), cancel, p)

# Issue the write and wait for its completion, delivering a cancellation of
# `tok` by interrupting the wait (the caller adds the post-write cancellation
# point that turns a cancelled-but-completed write into a throw). The caller
# must have acquired the iolock, which is released before returning.
function _uv_write_wait(s::LibuvStream, p::Ptr{UInt8}, n::UInt,
                        tok::MaybeToken, @nospecialize(owner))
    src = cancel_source(tok)
    # entry check: throw before handing anything to libuv
    _iolocked_checkcancel(src)
    local uvw, lastn
    try
        uvw, lastn = uv_write_async(s, p, n)
    catch
        # E.g. the stream was (or gets) closed: release the iolock so that
        # errors propagate without it.
        iolock_end()
        rethrow()
    end
    # Whether the write went out as a single request: only then may a
    # cancellation `uv_cancel` it - see _uv_write_cancelled_finish. (The
    # wait is on the *last* request either way: completion callbacks run in
    # order unless a request is dequeued by uv_cancel.)
    single = lastn == n
    w = _begin_uvreq_wait!(src, s, uvw)
    if src !== nothing && !register_cancellation!(src, w)
        # The token was cancelled since the entry check and the wake was
        # claimed back for us: don't park. Resolve the in-flight write and
        # report the bytes that reached the OS; the caller observes the
        # cancellation at its next cancellation point.
        iolock_end()
        nwritten = _uv_write_cancelled_finish(s, w, uvw,
            severity(cancel_severity(src)::CancellationRequest), src, owner, single)
        return Int(n - lastn + nwritten)
    end
    iolock_end()
    local nwritten::Csize_t
    creq = nothing
    try
        sigatomic_end()
        # wait for the last chunk to complete (or error)
        # assume that any errors would be sticky,
        # (so we don't need to monitor the error status of the intermediate writes)
        nwritten = wait()::Csize_t
        sigatomic_begin()
    catch err
        # (catch restored the sigatomic level from the try entry)
        if err isa CancellationRequest
            # Cancellation is an expected outcome of a write, not an error:
            # resolve the request's ownership below and return the partial
            # count - the caller's next cancellation point (re)delivers the
            # request. (The delivery is level-triggered, so nothing is
            # lost by not propagating this throw.)
            creq = err
        else
            # interrupted by something other than a cancellation (an
            # interrupter or raw throwto); a split write must not be
            # uv_cancel'ed on detach (see _uv_write_cancelled_finish)
            iolock_begin()
            _end_uvreq_wait!(w, s, uvw, single, owner)
            rethrow()
        end
    end
    if creq !== nothing
        nwritten = _uv_write_cancelled_finish(s, w, uvw, severity(creq), src, owner, single)
    else
        # normal completion (or the wake lost the race to a completion that
        # ran anyway): the callback handed us the request to free
        iolock_begin()
        _end_uvreq_wait!(w, s, uvw, single, owner)
    end
    return Int(n - lastn + nwritten)
end

# Resolve the ownership of a cancelled write's uv request and return the
# last chunk's partial write count (0 when the request had to be detached
# without awaiting its completion). For SAFE cancellations this awaits the
# completion callback, so the caller's buffer is provably no longer in use
# by the OS when this returns; only a severity escalation interrupts that
# bounded wait. `single` says whether the write went out as one request;
# only then is the request `uv_cancel`ed. Enters with the iolock released
# at the sigatomic level of the interrupted wait; unwinds both and the
# handle preservation.
function _uv_write_cancelled_finish(s::LibuvStream, w::WaitEntry, uvw::Ptr{Cvoid},
                                    sev::UInt8, src::CancellationTokenSource,
                                    @nospecialize(owner), single::Bool)
    ct = current_task()
    nwritten::Csize_t = 0
    awaited = false
    iolock_begin()
    d = uv_req_data(uvw)
    if d == C_NULL
        # the completion callback already ran (it lost the wake claim): the
        # request is ours and knows the count
        awaited = true
        nwritten = ccall(:uv_write_nwritten, Csize_t, (Ptr{Cvoid},), uvw)
    else
        # The write is still in flight. Only a write issued as a *single*
        # request may be asked to cancel: our libuv patch dequeues the
        # targeted request out of order and completes it with UV_ECANCELED,
        # so cancelling the last chunk of a split write would say nothing
        # about the earlier chunks - they could still be queued, still
        # referencing the buffer, when the "cancelled" callback fires. For
        # a split write even a SAFE cancellation therefore awaits the last
        # callback *without* uv_cancel: nothing is dequeued, completion
        # stays in-order, and the last chunk's callback implies every
        # earlier chunk is done with the buffer.
        if single
            # flag the request as cancelled-by-us first (on the witness
            # slot's aux, under the iolock), so the completion callback can
            # tell our UV_ECANCELED from a close-induced one - see
            # uv_writecb_task
            _mark_uvreq_cancel_requested!(w, s)
            ccall(:uv_cancel, Cint, (Ptr{Cvoid},), uvw) # ignore any errors
        end
        if sev < severity(CANCEL_REQUEST_ABANDON_EXTERNAL)
            # SAFE cancellation: await the completion callback, so that the
            # caller's buffer is provably no longer in use when this
            # returns. Only a severity escalation may interrupt this bounded
            # teardown wait: stage the raised eligibility floor, then re-arm
            # the (still registered) entry - the re-park needs no new
            # registration.
            slots(w)[_find_slot(w, src)].aux = UInt64(sev + 0x01)
            _arm_wait(ct, w)
            if register_cancellation!(src, w)
                iolock_end()
                try
                    nwritten = wait()::Csize_t
                    awaited = true
                catch
                    # escalated (or interrupted) during the teardown wait:
                    # fall through to detach
                end
                iolock_begin()
                if !awaited && uv_req_data(uvw) == C_NULL
                    # the callback ran anyway (our wake lost a race)
                    awaited = true
                    nwritten = ccall(:uv_write_nwritten, Csize_t, (Ptr{Cvoid},), uvw)
                end
            end
            # (a refused registration means the state already escalated)
        end
    end
    # Not awaited (abandoning severity, or an escalation during teardown):
    # _end_uvreq_wait! detaches the request - the completion callback frees
    # it - keeping the written buffer rooted until it does; the last chunk's
    # count is then unknown and reported as 0. (No re-cancel: for a single
    # request the uv_cancel above was already issued; a split write must
    # never be uv_cancel'ed - the detached last chunk completing in order is
    # what keeps the rooted buffer valid for the earlier chunks.)
    _end_uvreq_wait!(w, s, uvw, false, owner)
    return nwritten
end

function uv_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt; cancel::CancelTokenArg=DEFAULT_CANCEL,
                  owner=nothing)
    return _uv_write_owned(s, p, n, cancel, owner)
end

# Positional core of uv_write: an `owner::Any` keyword call is not statically
# resolvable (the NamedTuple type is abstract), which breaks trimmed builds.
function _uv_write_owned(s::LibuvStream, p::Ptr{UInt8}, n::UInt, cancel::CancelTokenArg,
                         @nospecialize(owner))
    tok = resolve_cancel_token(cancel)
    # branch on the token explicitly: a Union-typed `tok` alongside the
    # deliberately unspecialized `owner` leaves the callee unresolvable
    # for trimmed builds
    nb = tok === nothing ? _uv_write_wait(s, p, n, nothing, owner) :
                           _uv_write_wait(s, p, n, tok, owner)
    # nb < n means the wait was cancelled: the write reports the partial
    # count (like a short write) and the cancellation itself is delivered at
    # the caller's next cancellation point - level-triggered, nothing is
    # lost by returning normally here.
    return nb
end

# helper function for uv_write that returns the uv_write_t struct of the last
# chunk's write (and that chunk's size) rather than waiting on it, caller must
# hold the iolock
function uv_write_async(s::LibuvStream, p::Ptr{UInt8}, n::UInt)
    check_open(s)
    while true
        uvw = Libc.malloc(_sizeof_uv_write)
        uv_req_set_data(uvw, C_NULL) # in case we get interrupted before arriving at the wait call
        nwrite = min(n, MAX_OS_WRITE) # split up the write into chunks the OS can handle.
        # TODO: use writev instead of a loop
        err = ccall(:jl_uv_write,
                    Int32,
                    (Ptr{Cvoid}, Ptr{Cvoid}, UInt, Ptr{Cvoid}, Ptr{Cvoid}),
                    s, p, nwrite, uvw,
                    @cfunction(uv_writecb_task, Cvoid, (Ptr{Cvoid}, Cint)))
        if err < 0
            Libc.free(uvw)
            uv_error("write", err)
        end
        n -= nwrite
        p += nwrite
        if n == 0
            return uvw, nwrite
        end
    end
end


# Optimized send
# - smaller writes are buffered, final uv write on flush or when buffer full
# - large isbits arrays are unbuffered and written directly

function write(s::LibuvStream, a::Vector{UInt8}; cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    # Like the generic unsafe_write fallback below, but carrying the buffer's
    # owner so a detached (abandoned) write keeps it rooted; see
    # _detached_uvreq_roots.
    GC.@preserve a begin
        return Int(_unsafe_write_owned(s, pointer(a), UInt(sizeof(a)), cancel, a))
    end
end

# The public String path must be owner-carrying for the same reason: the
# generic method (strings/io.jl) preserves the string only until
# unsafe_write returns, but an abandoning cancellation can detach the
# in-flight request, which then references the string until its completion
# callback runs.
function write(s::LibuvStream, str::Union{String, SubString{String}};
               cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    GC.@preserve str begin
        return Int(_unsafe_write_owned(s, pointer(str), UInt(sizeof(str)), cancel, str))
    end
end

# Owner-carrying counterpart of the generic `write(::IO, ::Array)` methods,
# again so a detached request keeps the array rooted (bits element types
# only; others error in the generic method anyway).
function write(s::LibuvStream, a::Array; cancel::CancelTokenArg=DEFAULT_CANCEL)
    if isbitstype(eltype(a))
        cancel = precheck_cancel_arg(cancel)
        GC.@preserve a begin
            return Int(_unsafe_write_owned(s, Ptr{UInt8}(pointer(a)), UInt(sizeof(a)), cancel, a))
        end
    end
    return invoke(write, Tuple{IO, AbstractArray}, s, a; cancel)
end

# Raw-pointer writes have no discoverable owner: the caller's documented
# contract is pointer validity for the duration of the call - which an
# abandoning cancellation extends past the return, until the detached
# request's completion callback has run (see _detached_uvreq_roots). The
# owner-carrying entry points above (Vector{UInt8}, Array, String) do not
# have this hazard and are what the public `write` paths use.
function unsafe_write(s::LibuvStream, p::Ptr{UInt8}, n::UInt; cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    return _unsafe_write_owned(s, p, n, cancel, nothing)
end

function _unsafe_write_owned(s::LibuvStream, p::Ptr{UInt8}, n::UInt, cancel::CancelTokenArg,
                             @nospecialize(owner))
    while true
        # try to add to the send buffer
        iolock_begin()
        buf = s.sendbuf
        buf === nothing && break
        totb = bytesavailable(buf) + n
        if totb < buf.maxsize
            nb = unsafe_write(buf, p, n)
            iolock_end()
            return nb
        end
        bytesavailable(buf) == 0 && break
        # perform flush(s)
        arr = take!(buf)
        nb = uv_write(s, arr, cancel)
        # a cancelled flush-write returns short: splice the unwritten tail
        # back (ahead of concurrent appends) so no buffered byte is lost -
        # the next iteration's write delivers the (level-triggered)
        # cancellation at its entry check
        nb < length(arr) && _requeue_unwritten!(s, arr, nb)
    end
    # perform the output to the kernel
    return _uv_write_owned(s, p, n, cancel, owner)
end

# Splice the unwritten tail of a cancelled buffered-flush write back to the
# *front* of `s.sendbuf` - ahead of anything appended while the write was in
# flight, preserving stream order. `arr` was taken off the send buffer and
# only its first `nwritten` bytes reached the stream.
function _requeue_unwritten!(s::LibuvStream, arr::Vector{UInt8}, nwritten::Int)
    nwritten < length(arr) || return nothing
    iolock_begin()
    buf = s.sendbuf
    if buf !== nothing
        appended = bytesavailable(buf) > 0 ? take!(buf) : nothing
        write(buf, @view arr[nwritten+1:end])
        appended === nothing || write(buf, appended)
    end
    iolock_end()
    return nothing
end

# Cancellation contract of flush: a cancellation interrupts the wait, but
# never silently discards data - buffered bytes that did not reach the
# stream are put back in the send buffer, the partial state is left
# consistent, and the CancellationRequest itself is delivered at the
# caller's next cancellation point (delivery is level-triggered).
function flush(s::LibuvStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    cancel = precheck_cancel_arg(cancel)
    iolock_begin()
    buf = s.sendbuf
    if buf !== nothing
        if bytesavailable(buf) > 0
            arr = take!(buf)
            nb = uv_write(s, arr, cancel)
            # cancelled short: requeue the unwritten tail rather than drop it
            nb < length(arr) && _requeue_unwritten!(s, arr, nb)
            return
        end
    end
    # zero write from a random pointer to flush current queue, ignoring any
    # errors from it: previously queued writes have already reported their
    # errors to their writers, and the peer closing the stream after a
    # completed exchange must not make flush throw
    try
        _uv_write_owned(s, Ptr{UInt8}(Base.eventloop()), UInt(0), cancel, nothing)
    catch ex
        ex isa IOError || rethrow()
    end
    return
end

function buffer_writes(s::LibuvStream, bufsize)
    sendbuf = PipeBuffer(bufsize)
    iolock_begin()
    s.sendbuf = sendbuf
    iolock_end()
    return s
end

## low-level calls to libuv ##

function write(s::LibuvStream, b::UInt8)
    buf = s.sendbuf
    if buf !== nothing
        iolock_begin()
        if bytesavailable(buf) + 1 < buf.maxsize
            n = write(buf, b)
            iolock_end()
            return n
        end
        iolock_end()
    end
    # carry the Ref as the owner (rather than the generic ownerless
    # `write(s, ::Ref)` path): an abandoning cancellation may detach the
    # in-flight request, which must keep the byte's storage rooted
    r = Ref{UInt8}(b)
    GC.@preserve r begin
        return Int(_unsafe_write_owned(s, unsafe_convert(Ptr{UInt8}, r), UInt(1), DEFAULT_CANCEL, r))
    end
end

function uv_writecb_task(req::Ptr{Cvoid}, status::Cint)
    # An expected-by-the-waiter UV_ECANCELED must be read off the entry
    # before the claim below releases the witness slot.
    cancel_requested = status == UV_ECANCELED && _uvreq_cancel_requested(req)
    t = _claim_uvreq_waiter(req)
    if t !== nothing
        if status == 0 || cancel_requested
            # For writes cancelled by the waiter, this is the partial write
            # count.
            schedule(t, ccall(:uv_write_nwritten, Csize_t, (Ptr{Cvoid},), req))
        else
            # A real error - including a close-induced UV_ECANCELED (libuv
            # fails still-queued writes with it when the stream closes),
            # which must not masquerade as a benign short write.
            schedule(t, _UVError("write", status); error=true)
        end
    end
    nothing
end

function uv_shutdowncb_task(req::Ptr{Cvoid}, status::Cint)
    t = _claim_uvreq_waiter(req)
    t === nothing || schedule(t, status)
    nothing
end


_fd(x::IOStream) = RawFD(fd(x))
_fd(x::Union{OS_HANDLE, RawFD}) = x

function _fd(x::Union{LibuvStream, LibuvServer})
    fd = Ref{OS_HANDLE}(INVALID_OS_HANDLE)
    if x.status != StatusUninit && x.status != StatusClosed && x.handle != C_NULL
        ccall(:uv_fileno, Int32, (Ptr{Cvoid}, Ptr{OS_HANDLE}), x.handle, fd)
        # handle errors by returning INVALID_OS_HANDLE
    end
    return fd[]
end

struct RedirectStdStream <: Function
    unix_fd::Int
    writable::Bool
end
for (f, writable, unix_fd) in
        ((:redirect_stdin, false, 0),
         (:redirect_stdout, true, 1),
         (:redirect_stderr, true, 2))
    @eval const ($f) = RedirectStdStream($unix_fd, $writable)
end
function _redirect_io_libc(stream, unix_fd::Int)
    posix_fd = _fd(stream)
    @static if Sys.iswindows()
        if 0 <= unix_fd <= 2
            ccall(:SetStdHandle, stdcall, Int32, (Int32, OS_HANDLE),
                -10 - unix_fd, Libc._get_osfhandle(posix_fd))
        end
    end
    GC.@preserve stream dup(posix_fd, RawFD(unix_fd))
    nothing
end
function _redirect_io_cglobal(handle::Union{LibuvStream, IOStream, Nothing}, unix_fd::Int)
    c_sym = unix_fd == 0 ? cglobal(:jl_uv_stdin, Ptr{Cvoid}) :
            unix_fd == 1 ? cglobal(:jl_uv_stdout, Ptr{Cvoid}) :
            unix_fd == 2 ? cglobal(:jl_uv_stderr, Ptr{Cvoid}) :
            C_NULL
    c_sym == C_NULL || unsafe_store!(c_sym, handle === nothing ? Ptr{Cvoid}(unix_fd) : handle.handle)
    nothing
end
function _redirect_io_global(io, unix_fd::Int)
    unix_fd == 0 && (global stdin = io)
    unix_fd == 1 && (global stdout = io)
    unix_fd == 2 && (global stderr = io)
    nothing
end
function (f::RedirectStdStream)(handle::Union{LibuvStream, IOStream})
    _redirect_io_libc(handle, f.unix_fd)
    _redirect_io_cglobal(handle, f.unix_fd)
    _redirect_io_global(handle, f.unix_fd)
    return handle
end
function (f::RedirectStdStream)(::DevNull)
    nulldev = @static Sys.iswindows() ? "NUL" : "/dev/null"
    handle = open(nulldev, write=f.writable)
    _redirect_io_libc(handle, f.unix_fd)
    close(handle) # handle has been dup'ed in _redirect_io_libc
    _redirect_io_cglobal(nothing, f.unix_fd)
    _redirect_io_global(devnull, f.unix_fd)
    return devnull
end
function (f::RedirectStdStream)(io::AbstractPipe)
    io2 = (f.writable ? pipe_writer : pipe_reader)(io)
    f(io2)
    _redirect_io_global(io, f.unix_fd)
    return io
end
function (f::RedirectStdStream)(p::Pipe)
    if p.in.status == StatusInit && p.out.status == StatusInit
        link_pipe!(p)
    end
    io2 = getfield(p, f.writable ? :in : :out)
    f(io2)
    return p
end
(f::RedirectStdStream)() = f(Pipe())

# Deprecate these in v2 (RedirectStdStream support)
iterate(p::Pipe) = (p.out, 1)
iterate(p::Pipe, i::Int) = i == 1 ? (p.in, 2) : nothing
getindex(p::Pipe, key::Int) = key == 1 ? p.out : key == 2 ? p.in : throw(KeyError(key))

"""
    redirect_stdout([stream]) -> stream

Create a pipe to which all C and Julia level [`stdout`](@ref) output
will be redirected. Return a stream representing the pipe ends.
Data written to [`stdout`](@ref) may now be read from the `rd` end of
the pipe.

!!! note
    `stream` must be a compatible objects, such as an `IOStream`, `TTY`,
    [`Pipe`](@ref), socket, or `devnull`.

See also [`redirect_stdio`](@ref).
"""
redirect_stdout

"""
    redirect_stderr([stream]) -> stream

Like [`redirect_stdout`](@ref), but for [`stderr`](@ref).

!!! note
    `stream` must be a compatible objects, such as an `IOStream`, `TTY`,
    [`Pipe`](@ref), socket, or `devnull`.

See also [`redirect_stdio`](@ref).
"""
redirect_stderr

"""
    redirect_stdin([stream]) -> stream

Like [`redirect_stdout`](@ref), but for [`stdin`](@ref).
Note that the direction of the stream is reversed.

!!! note
    `stream` must be a compatible objects, such as an `IOStream`, `TTY`,
    [`Pipe`](@ref), socket, or `devnull`.

See also [`redirect_stdio`](@ref).
"""
redirect_stdin

"""
    redirect_stdio(;stdin=stdin, stderr=stderr, stdout=stdout)

Redirect a subset of the streams `stdin`, `stderr`, `stdout`.
Each argument must be an `IOStream`, `TTY`, [`Pipe`](@ref), socket, or
`devnull`.

!!! compat "Julia 1.7"
    `redirect_stdio` requires Julia 1.7 or later.
"""
function redirect_stdio(;stdin=nothing, stderr=nothing, stdout=nothing)
    stdin  === nothing || redirect_stdin(stdin)
    stderr === nothing || redirect_stderr(stderr)
    stdout === nothing || redirect_stdout(stdout)
end

"""
    redirect_stdio(f; stdin=nothing, stderr=nothing, stdout=nothing)

Redirect a subset of the streams `stdin`, `stderr`, `stdout`,
call `f()` and restore each stream.

Possible values for each stream are:
* `nothing` indicating the stream should not be redirected.
* `path::AbstractString` redirecting the stream to the file at `path`.
* `io` an `IOStream`, `TTY`, [`Pipe`](@ref), socket, or `devnull`.

# Examples
```julia-repl
julia> redirect_stdio(stdout="stdout.txt", stderr="stderr.txt") do
           print("hello stdout")
           print(stderr, "hello stderr")
       end

julia> read("stdout.txt", String)
"hello stdout"

julia> read("stderr.txt", String)
"hello stderr"
```

# Edge cases

It is possible to pass the same argument to `stdout` and `stderr`:
```julia-repl
julia> redirect_stdio(stdout="log.txt", stderr="log.txt", stdin=devnull) do
    ...
end
```

However it is not supported to pass two distinct descriptors of the same file.
```julia-repl
julia> io1 = open("same/path", "w")

julia> io2 = open("same/path", "w")

julia> redirect_stdio(f, stdout=io1, stderr=io2) # not supported
```
Also the `stdin` argument may not be the same descriptor as `stdout` or `stderr`.
```julia-repl
julia> io = open(...)

julia> redirect_stdio(f, stdout=io, stdin=io) # not supported
```

!!! compat "Julia 1.7"
    `redirect_stdio` requires Julia 1.7 or later.
"""
function redirect_stdio(f; stdin=nothing, stderr=nothing, stdout=nothing)

    function resolve(new::Nothing, oldstream, mode)
        (new=nothing, close=false, old=nothing)
    end
    function resolve(path::AbstractString, oldstream,mode)
        (new=open(path, mode), close=true, old=oldstream)
    end
    function resolve(new, oldstream, mode)
        (new=new, close=false, old=oldstream)
    end

    same_path(x, y) = false
    function same_path(x::AbstractString, y::AbstractString)
        # if x = y = "does_not_yet_exist.txt" then samefile will return false
        (abspath(x) == abspath(y)) || samefile(x,y)
    end
    if same_path(stderr, stdin)
        throw(ArgumentError("stdin and stderr cannot be the same path"))
    end
    if same_path(stdout, stdin)
        throw(ArgumentError("stdin and stdout cannot be the same path"))
    end

    new_in , close_in , old_in  = resolve(stdin , Base.stdin , "r")
    new_out, close_out, old_out = resolve(stdout, Base.stdout, "w")
    if same_path(stderr, stdout)
        # make sure that in case stderr = stdout = "same/path"
        # only a single io is used instead of opening the same file twice
        new_err, close_err, old_err = new_out, false, Base.stderr
    else
        new_err, close_err, old_err = resolve(stderr, Base.stderr, "w")
    end

    redirect_stdio(; stderr=new_err, stdin=new_in, stdout=new_out)

    try
        return f()
    finally
        redirect_stdio(;stderr=old_err, stdin=old_in, stdout=old_out)
        close_err && close(new_err)
        close_in  && close(new_in )
        close_out && close(new_out)
    end
end

function (f::RedirectStdStream)(thunk::Function, stream)
    stdold = f.unix_fd == 0 ? stdin :
             f.unix_fd == 1 ? stdout :
             f.unix_fd == 2 ? stderr :
             throw(ArgumentError("Not implemented to get old handle of fd except for stdio"))
    f(stream)
    try
        return thunk()
    finally
        f(stdold)
    end
end


"""
    redirect_stdout(f::Function, stream)

Run the function `f` while redirecting [`stdout`](@ref) to `stream`.
Upon completion, [`stdout`](@ref) is restored to its prior setting.
"""
redirect_stdout(f::Function, stream)

"""
    redirect_stderr(f::Function, stream)

Run the function `f` while redirecting [`stderr`](@ref) to `stream`.
Upon completion, [`stderr`](@ref) is restored to its prior setting.
"""
redirect_stderr(f::Function, stream)

"""
    redirect_stdin(f::Function, stream)

Run the function `f` while redirecting [`stdin`](@ref) to `stream`.
Upon completion, [`stdin`](@ref) is restored to its prior setting.
"""
redirect_stdin(f::Function, stream)

mark(x::LibuvStream)     = mark(x.buffer)
unmark(x::LibuvStream)   = unmark(x.buffer)
reset(x::LibuvStream)    = reset(x.buffer)
ismarked(x::LibuvStream) = ismarked(x.buffer)

function peek(s::LibuvStream, ::Type{T}) where T
    mark(s)
    try read(s, T)
    finally
        reset(s)
    end
end

# BufferStream's are non-OS streams, backed by a regular IOBuffer
mutable struct BufferStream <: LibuvStream
    buffer::IOBuffer
    cond::Threads.Condition
    readerror::Any
    buffer_writes::Bool
    lock::ReentrantLock # advisory lock
    status::Int

    BufferStream() = new(PipeBuffer(), Threads.Condition(), nothing, false, ReentrantLock(), StatusActive)
end

isopen(s::BufferStream) = s.status != StatusClosed

# BufferStream <: LibuvStream, so every keyword-accepting LibuvStream
# method a `cancel=` call would select must be re-specialized here: keyword
# calls dispatch only among keyword-accepting methods, and the LibuvStream
# ones touch fields (handle, sendbuf) a BufferStream does not have.
function closewrite(s::BufferStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    # closing is in-memory and immediate: entry gate only
    precheck_cancel_arg(cancel)
    close(s)
end

function close(s::BufferStream)
    lock(s.cond) do
        s.status = StatusClosed
        notify(s.cond) # aka flush
        nothing
    end
end
uvfinalize(s::BufferStream) = nothing
setup_stdio(stream::BufferStream, child_readable::Bool) = invoke(setup_stdio, Tuple{IO, Bool}, stream, child_readable)

function read(s::BufferStream, ::Type{UInt8}; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    nread = lock(s.cond) do
        wait_readnb(s, 1, tok)
        read(s.buffer, UInt8)
    end
    return nread
end
function unsafe_read(s::BufferStream, a::Ptr{UInt8}, nb::UInt; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    lock(s.cond) do
        wait_readnb(s, Int(nb), tok)
        unsafe_read(s.buffer, a, nb)
        nothing
    end
end
bytesavailable(s::BufferStream) = bytesavailable(s.buffer)

isreadable(s::BufferStream) = (isopen(s) || bytesavailable(s) > 0) && s.buffer.readable
iswritable(s::BufferStream) = isopen(s) && s.buffer.writable

function wait_readnb(s::BufferStream, nb::Int, tok::MaybeToken=default_cancel_token())
    lock(s.cond) do
        while isopen(s) && bytesavailable(s.buffer) < nb
            wait(s.cond, tok)
        end
    end
end

function readavailable(this::BufferStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    bytes = lock(this.cond) do
        wait_readnb(this, 1, tok)
        buf = this.buffer
        @assert buf.seekable == false "buffer should not be seekable"
        take!(buf)
    end
    return bytes
end

function read(stream::BufferStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    bytes = lock(stream.cond) do
        wait_close(stream, tok)
        take!(stream.buffer)
    end
    return bytes
end

function readbytes!(s::BufferStream, a::Vector{UInt8}, nb::Int; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    sbuf = s.buffer
    @assert sbuf.seekable == false "buffer should not be seekable"
    @assert sbuf.maxsize >= nb "insufficient buffer size"

    function wait_locked(s, buf, nb, tok)
        while bytesavailable(buf) < nb
            s.readerror === nothing || throw(s.readerror)
            isopen(s) || break
            s.status != StatusEOF || break
            wait_readnb(s, nb, tok)
        end
    end

    bytes = lock(s.cond) do
        if nb <= SZ_UNBUFFERED_IO # Under this limit we are OK with copying the array from the stream's buffer
            wait_locked(s, sbuf, nb, tok)
        end
        if bytesavailable(sbuf) >= nb
            nread = readbytes!(sbuf, a, nb)
        else
            initsize = length(a)
            newbuf = _truncated_pipebuffer(a; maxsize=nb)
            nread = try
                s.buffer = newbuf
                write(newbuf, sbuf)
                wait_locked(s, newbuf, nb, tok)
                bytesavailable(newbuf)
            finally
                s.buffer = sbuf
            end
            _take!(a, _unsafe_take!(newbuf))
            length(a) >= initsize || resize!(a, initsize)
        end
        return nread
    end
    return bytes
end

show(io::IO, s::BufferStream) = print(io, "BufferStream(bytes waiting=", bytesavailable(s.buffer), ", isopen=", isopen(s), ")")

function readuntil(s::BufferStream, c::UInt8; keep::Bool=false, cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    bytes = lock(s.cond) do
        while isopen(s) && !occursin(c, s.buffer)
            # a cancelled wait unwinds with the buffer intact (nothing has
            # been consumed until the delimiter is present)
            wait(s.cond, tok)
        end
        readuntil(s.buffer, c, keep=keep)
    end
    return bytes
end

function wait_close(s::BufferStream, tok::MaybeToken=default_cancel_token())
    lock(s.cond) do
        while isopen(s)
            wait(s.cond, tok)
        end
    end
end

start_reading(s::BufferStream) = Int32(0)
stop_reading(s::BufferStream) = nothing

write(s::BufferStream, b::UInt8) = write(s, Ref{UInt8}(b))
# BufferStream writes are in-memory: no uv request can outlive the caller, so
# the LibuvStream method's detached-owner bookkeeping does not apply here.
function write(s::BufferStream, a::Vector{UInt8}; cancel::CancelTokenArg=DEFAULT_CANCEL)
    # the write itself is in-memory and cannot block for long (the advisory
    # lock's park runs under the ambient scope): the explicit token only
    # gates at entry
    precheck_cancel_arg(cancel)
    GC.@preserve a begin
        return Int(unsafe_write(s, pointer(a), UInt(sizeof(a))))
    end
end
# In-memory counterparts of the owner-carrying LibuvStream methods (which
# must not apply here: they track uv requests and the send buffer).
function write(s::BufferStream, str::Union{String, SubString{String}};
               cancel::CancelTokenArg=DEFAULT_CANCEL)
    precheck_cancel_arg(cancel)
    GC.@preserve str begin
        return Int(unsafe_write(s, pointer(str), UInt(sizeof(str))))
    end
end
function write(s::BufferStream, a::Array; cancel::CancelTokenArg=DEFAULT_CANCEL)
    precheck_cancel_arg(cancel)
    if isbitstype(eltype(a))
        GC.@preserve a begin
            return Int(unsafe_write(s, Ptr{UInt8}(pointer(a)), UInt(sizeof(a))))
        end
    end
    return invoke(write, Tuple{IO, AbstractArray}, s, a)
end
function unsafe_write(s::BufferStream, p::Ptr{UInt8}, nb::UInt; cancel::CancelTokenArg=DEFAULT_CANCEL)
    # in-memory write, cannot block for long: entry gate only (see the
    # BufferStream `write` above)
    precheck_cancel_arg(cancel)
    nwrite = lock(s.cond) do
        check_open(s)
        rv = unsafe_write(s.buffer, p, nb)
        s.buffer_writes || notify(s.cond)
        rv
    end
    return nwrite
end

function eof(s::BufferStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    tok = resolve_cancel_token(precheck_cancel_arg(cancel))
    bytesavailable(s) > 0 && return false
    iseof = lock(s.cond) do
        wait_readnb(s, 1, tok)
        return !isopen(s) && bytesavailable(s) <= 0
    end
    return iseof
end

# If buffer_writes is called, it will delay notifying waiters till a flush is called.
buffer_writes(s::BufferStream, bufsize=0) = (s.buffer_writes = true; s)
function flush(s::BufferStream; cancel::CancelTokenArg=DEFAULT_CANCEL)
    # in-memory flush (a notify), nothing to wait for: entry gate only
    precheck_cancel_arg(cancel)
    lock(s.cond) do
        check_open(s)
        notify(s.cond)
        nothing
    end
end

skip(s::BufferStream, n) = skip(s.buffer, n)

function reseteof(s::BufferStream)
    lock(s.cond) do
        s.status = StatusOpen
        nothing
    end
    nothing
end
