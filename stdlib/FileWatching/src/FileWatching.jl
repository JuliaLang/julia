# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Utilities for monitoring files and file descriptors for events.
"""
module FileWatching

export
    watch_file,
    poll_fd,
    poll_file,
    FileMonitor,
    PollingFileWatcher,
    FDWatcher

import Base: @handle_as, wait, close, uvfinalize, eventloop, notify_error, stream_wait,
    _sizeof_uv_poll, _sizeof_uv_fs_poll, _sizeof_uv_fs_event, _uv_hook_close, uv_error, UVError,
    associate_julia_struct, disassociate_julia_struct, isreadable, iswritable, |
import Base.Filesystem.StatStruct
if Sys.iswindows()
    import Base.WindowsRawSocket
end

# libuv file watching event flags
const UV_RENAME = 1
const UV_CHANGE = 2
struct FileEvent
    renamed::Bool
    changed::Bool
    timedout::Bool
end
FileEvent() = FileEvent(false, false, false)
FileEvent(flags::Integer) = FileEvent((flags & UV_RENAME) != 0,
                                  (flags & UV_CHANGE) != 0,
                                  (flags & FD_TIMEDOUT) != 0)
fetimeout() = FileEvent(false, false, true)

struct FDEvent
    readable::Bool
    writable::Bool
    disconnect::Bool
    timedout::Bool
end
# libuv file descriptor event flags
const UV_READABLE = 1
const UV_WRITABLE = 2
const UV_DISCONNECT = 4
const FD_TIMEDOUT = 8

isreadable(f::FDEvent) = f.readable
iswritable(f::FDEvent) = f.writable
FDEvent() = FDEvent(false, false, false, false)
FDEvent(flags::Integer) = FDEvent((flags & UV_READABLE) != 0,
                                  (flags & UV_WRITABLE) != 0,
                                  (flags & UV_DISCONNECT) != 0,
                                  (flags & FD_TIMEDOUT) != 0)
fdtimeout() = FDEvent(false, false, false, true)
|(a::FDEvent, b::FDEvent) =
    FDEvent(a.readable | b.readable,
            a.writable | b.writable,
            a.disconnect | b.disconnect,
            a.timedout | b.timedout)

mutable struct FileMonitor
    handle::Ptr{Cvoid}
    file::String
    notify::Condition
    active::Bool
    function FileMonitor(file::AbstractString)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, file, Condition(), false)
        associate_julia_struct(handle, this)
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(UVError("FileMonitor", err))
        end
        finalizer(uvfinalize, this)
        return this
    end
end

mutable struct PollingFileWatcher
    handle::Ptr{Cvoid}
    file::String
    interval::UInt32
    notify::Condition
    active::Bool
    busy_polling::Int32
    function PollingFileWatcher(file::AbstractString, interval::Float64=5.007) # same default as nodejs
        handle = Libc.malloc(_sizeof_uv_fs_poll)
        this = new(handle, file, round(UInt32, interval * 1000), Condition(), false, 0)
        associate_julia_struct(handle, this)
        err = ccall(:uv_fs_poll_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(UVError("PollingFileWatcher", err))
        end
        finalizer(uvfinalize, this)
        return this
    end
end

mutable struct _FDWatcher
    handle::Ptr{Cvoid}
    fdnum::Int # this is NOT the file descriptor
    refcount::Tuple{Int, Int}
    notify::Condition
    active::Tuple{Bool, Bool}
    events::Int32

    let FDWatchers = Vector{Any}()
        global _FDWatcher
        @static if Sys.isunix()
            function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
                if !readable && !writable
                    throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
                end
                fdnum = fd.fd + 1
                if fdnum > length(FDWatchers)
                    old_len = length(FDWatchers)
                    resize!(FDWatchers, fdnum)
                    FDWatchers[(old_len + 1):fdnum] = nothing
                elseif FDWatchers[fdnum] !== nothing
                    this = FDWatchers[fdnum]::_FDWatcher
                    this.refcount = (this.refcount[1] + Int(readable), this.refcount[2] + Int(writable))
                    return this
                end
                if ccall(:jl_uv_unix_fd_is_watched, Int32, (Int32, Ptr{Cvoid}, Ptr{Cvoid}), fd.fd, C_NULL, eventloop()) == 1
                    throw(ArgumentError("file descriptor $(fd.fd) is already being watched by libuv"))
                end

                handle = Libc.malloc(_sizeof_uv_poll)
                this = new(
                    handle,
                    fdnum,
                    (Int(readable), Int(writable)),
                    Condition(),
                    (false, false),
                    0)
                associate_julia_struct(handle, this)
                err = ccall(:uv_poll_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Int32), eventloop(), handle, fd.fd)
                if err != 0
                    Libc.free(handle)
                    throw(UVError("FDWatcher", err))
                end
                finalizer(uvfinalize, this)
                FDWatchers[fdnum] = this
                return this
            end
        end

        function Base.uvfinalize(t::_FDWatcher)
            if t.handle != C_NULL
                disassociate_julia_struct(t)
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t.handle)
                t.handle = C_NULL
            end
            t.refcount = (0, 0)
            t.active = (false, false)
            @static if Sys.isunix()
                if FDWatchers[t.fdnum] == t
                    FDWatchers[t.fdnum] = nothing
                end
            end
            notify(t.notify, fdtimeout())
            nothing
        end
    end

    @static if Sys.iswindows()
        function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
            handle = Libc._get_osfhandle(fd)
            return _FDWatcher(handle, readable, writable)
        end
        function _FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
            if !readable && !writable
                throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
            end

            handle = Libc.malloc(_sizeof_uv_poll)
            this = new(
                handle,
                0,
                (Int(readable), Int(writable)),
                Condition(),
                (false, false),
                0)
            associate_julia_struct(handle, this)
            err = ccall(:uv_poll_init_socket, Int32, (Ptr{Cvoid},   Ptr{Cvoid}, Ptr{Cvoid}),
                                                      eventloop(), handle,    fd.handle)
            if err != 0
                Libc.free(handle)
                throw(UVError("FDWatcher", err))
            end
            finalizer(uvfinalize, this)
            return this
        end
    end
end

mutable struct FDWatcher
    watcher::_FDWatcher
    readable::Bool
    writable::Bool
    # WARNING: make sure `close` has been manually called on this watcher before closing / destroying `fd`
    function FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
        this = new(_FDWatcher(fd, readable, writable), readable, writable)
        finalizer(close, this)
        return this
    end
    @static if Sys.iswindows()
        function FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
            this = new(_FDWatcher(fd, readable, writable), readable, writable)
            finalizer(close, this)
            return this
        end
    end
end


function close(t::_FDWatcher, readable::Bool, writable::Bool)
    if t.refcount != (0, 0)
        t.refcount = (t.refcount[1] - Int(readable), t.refcount[2] - Int(writable))
    end
    if t.refcount == (0, 0)
        uvfinalize(t)
    end
end

function close(t::FDWatcher)
    r, w = t.readable, t.writable
    t.readable = t.writable = false
    close(t.watcher, r, w)
end

function uvfinalize(uv::Union{FileMonitor, PollingFileWatcher})
    disassociate_julia_struct(uv)
    close(uv)
end

function close(t::Union{FileMonitor, PollingFileWatcher})
    if t.handle != C_NULL
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t.handle)
    end
end

function _uv_hook_close(uv::_FDWatcher)
    # fyi: jl_atexit_hook can cause this to get called too
    uv.handle = C_NULL
    uvfinalize(uv)
    nothing
end

function _uv_hook_close(uv::PollingFileWatcher)
    uv.handle = C_NULL
    uv.active = false
    notify(uv.notify, (StatStruct(), EOFError()))
    nothing
end

function _uv_hook_close(uv::FileMonitor)
    uv.handle = C_NULL
    uv.active = false
    notify(uv.notify, ("", FileEvent()))
    nothing
end

function __init__()
    global uv_jl_pollcb        = cfunction(uv_pollcb, Cvoid, Tuple{Ptr{Cvoid}, Cint, Cint})
    global uv_jl_fspollcb      = cfunction(uv_fspollcb, Cvoid, Tuple{Ptr{Cvoid}, Cint, Ptr{Cvoid}, Ptr{Cvoid}})
    global uv_jl_fseventscb    = cfunction(uv_fseventscb, Cvoid, Tuple{Ptr{Cvoid}, Ptr{Int8}, Int32, Int32})
end

function uv_fseventscb(handle::Ptr{Cvoid}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FileMonitor
    fname = filename == C_NULL ? "" : unsafe_string(convert(Ptr{UInt8}, filename))
    if status != 0
        notify_error(t.notify, UVError("FileMonitor", status))
    else
        notify(t.notify, (fname, FileEvent(events)))
    end
    nothing
end

function uv_pollcb(handle::Ptr{Cvoid}, status::Int32, events::Int32)
    t = @handle_as handle _FDWatcher
    if status != 0
        notify_error(t.notify, UVError("FDWatcher", status))
    else
        t.events |= events
        if t.active[1] || t.active[2]
            if isempty(t.notify.waitq)
                # if we keep hearing about events when nobody appears to be listening,
                # stop the poll to save cycles
                t.active = (false, false)
                ccall(:uv_poll_stop, Int32, (Ptr{Cvoid},), t.handle)
            end
        end
        notify(t.notify, FDEvent(events))
    end
    nothing
end

function uv_fspollcb(handle::Ptr{Cvoid}, status::Int32, prev::Ptr, curr::Ptr)
    t = @handle_as handle PollingFileWatcher
    if status == 0 || status != t.busy_polling
        t.busy_polling = status
        prev_stat = StatStruct(convert(Ptr{UInt8}, prev))
        curr_stat = (status == 0) ? StatStruct(convert(Ptr{UInt8}, curr)) : UVError("PollingFileWatcher", status)
        notify(t.notify, (prev_stat, curr_stat))
    end
    nothing
end


function start_watching(t::_FDWatcher)
    readable = t.refcount[1] > 0
    writable = t.refcount[2] > 0
    if t.active[1] != readable || t.active[2] != writable
        # make sure the READABLE / WRITEABLE state is updated
        uv_error("start_watching (File Handle)",
                 ccall(:uv_poll_start, Int32, (Ptr{Cvoid}, Int32, Ptr{Cvoid}),
                       t.handle,
                       (readable ? UV_READABLE : 0) | (writable ? UV_WRITABLE : 0),
                       uv_jl_pollcb::Ptr{Cvoid}))
        t.active = (readable, writable)
    end
    nothing
end

function start_watching(t::PollingFileWatcher)
    if !t.active
        uv_error("start_watching (File Path)",
                 ccall(:uv_fs_poll_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, UInt32),
                       t.handle, uv_jl_fspollcb::Ptr{Cvoid}, t.file, t.interval))
        t.active = true
    end
    nothing
end

function stop_watching(t::PollingFileWatcher)
    if t.active && isempty(t.notify.waitq)
        t.active = false
        uv_error("stop_watching (File Path)",
                 ccall(:uv_fs_poll_stop, Int32, (Ptr{Cvoid},), t.handle))
    end
    nothing
end

function start_watching(t::FileMonitor)
    if !t.active
        uv_error("start_watching (File Monitor)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32),
                       t.handle, uv_jl_fseventscb::Ptr{Cvoid}, t.file, 0))
        t.active = true
    end
    nothing
end

function stop_watching(t::FileMonitor)
    if t.active && isempty(t.notify.waitq)
        t.active = false
        uv_error("stop_watching (File Monitor)",
                 ccall(:uv_fs_event_stop, Int32, (Ptr{Cvoid},), t.handle))
    end
    nothing
end

function wait(fdw::FDWatcher)
    return wait(fdw.watcher, readable = fdw.readable, writable = fdw.writable)
end
function wait(fdw::_FDWatcher; readable=true, writable=true)
    events = FDEvent()
    while true
        if isa(events, FDEvent)
            events = events::FDEvent
            events |= FDEvent(fdw.events)
            haveevent = false
            if readable && isreadable(events)
                fdw.events &= ~UV_READABLE
                haveevent = true
            end
            if writable && iswritable(events)
                fdw.events &= ~UV_WRITABLE
                haveevent = true
            end
            if haveevent
                return events
            end
        else
            throw(events)
        end
        if fdw.refcount == (0, 0) # !open
            events = EOFError()
        else
            start_watching(fdw) # make sure the poll is active
            events = wait(fdw.notify)
        end
    end
end

function wait(fd::RawFD; readable=false, writable=false)
    fdw = _FDWatcher(fd, readable, writable)
    try
        return wait(fdw, readable=readable, writable=writable)
    finally
        close(fdw, readable, writable)
    end
end

if Sys.iswindows()
    function wait(socket::WindowsRawSocket; readable=false, writable=false)
        fdw = _FDWatcher(socket, readable, writable)
        try
            return wait(fdw, readable=readable, writable=writable)
        finally
            close(fdw, readable, writable)
        end
    end
end

function wait(pfw::PollingFileWatcher)
    start_watching(pfw)
    prevstat, currstat = stream_wait(pfw, pfw.notify)
    stop_watching(pfw)
    return prevstat, currstat
end

function wait(m::FileMonitor)
    start_watching(m)
    filename, events = stream_wait(m, m.notify)
    stop_watching(m)
    return filename, events
end

"""
    poll_fd(fd, timeout_s::Real=-1; readable=false, writable=false)

Monitor a file descriptor `fd` for changes in the read or write availability, and with a
timeout given by `timeout_s` seconds.

The keyword arguments determine which of read and/or write status should be monitored; at
least one of them must be set to `true`.

The returned value is an object with boolean fields `readable`, `writable`, and `timedout`,
giving the result of the polling.
"""
function poll_fd(s::Union{RawFD, Sys.iswindows() ? WindowsRawSocket : Union{}}, timeout_s::Real=-1; readable=false, writable=false)
    wt = Condition()
    fdw = _FDWatcher(s, readable, writable)
    try
        if timeout_s >= 0
            result::FDEvent = fdtimeout()

            @schedule begin
                try
                    result = wait(fdw, readable=readable, writable=writable)
                catch e
                    notify_error(wt, e)
                    return
                end
                notify(wt)
            end
            @schedule (sleep(timeout_s); notify(wt))

            wait(wt)
            return result
        else
            return wait(fdw, readable=readable, writable=writable)
        end
    finally
        close(fdw, readable, writable)
    end
end

"""
    watch_file(path::AbstractString, timeout_s::Real=-1)

Watch file or directory `path` for changes until a change occurs or `timeout_s` seconds have
elapsed.

The returned value is an object with boolean fields `changed`, `renamed`, and `timedout`,
giving the result of watching the file.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.
"""
function watch_file(s::AbstractString, timeout_s::Real=-1)
    wt = Condition()
    fm = FileMonitor(s)
    try
        if timeout_s >= 0
            result = fetimeout()

            @schedule begin
                try
                    _, result = wait(fm)
                catch e
                    notify_error(wt, e)
                    return
                end
                notify(wt)
            end
            @schedule (sleep(timeout_s); notify(wt))

            wait(wt)
            return result
        else
            return wait(fm)[2]
        end
    finally
        close(fm)
    end
end

"""
    poll_file(path::AbstractString, interval_s::Real=5.007, timeout_s::Real=-1) -> (previous::StatStruct, current)

Monitor a file for changes by polling every `interval_s` seconds until a change occurs or
`timeout_s` seconds have elapsed. The `interval_s` should be a long period; the default is
5.007 seconds.

Returns a pair of status objects `(previous, current)` when a change is detected.
The `previous` status is always a `StatStruct`, but it may have all of the fields zeroed
(indicating the file didn't previously exist, or wasn't previously accessible).

The `current` status object may be a `StatStruct`, an `EOFError` (indicating the timeout elapsed),
or some other `Exception` subtype (if the `stat` operation failed - for example, if the path does not exist).

To determine when a file was modified, compare `current isa StatStruct && mtime(prev) != mtime(current)` to detect
notification of changes. However, using [`watch_file`](@ref) for this operation is preferred, since
it is more reliable and efficient, although in some situations it may not be available.
"""
function poll_file(s::AbstractString, interval_seconds::Real=5.007, timeout_s::Real=-1)
    wt = Condition()
    pfw = PollingFileWatcher(s, Float64(interval_seconds))
    try
        if timeout_s >= 0
            result = :timeout

            @schedule begin
                try
                    statdiff = wait(pfw)
                    if statdiff[1] == StatStruct() && isa(statdiff[2], UVError)
                        # file didn't initially exist, continue watching for it to be created (or the error to change)
                        statdiff = wait(pfw)
                    end
                    result = statdiff
                catch e
                    notify_error(wt, e)
                    return
                end
                notify(wt)
            end
            @schedule (sleep(timeout_s); notify(wt))

            wait(wt)
            if result === :timeout
                return (StatStruct(), EOFError())
            end
            return result
        else
            return wait(pfw)
        end
    finally
        close(pfw)
    end
end

# deprecations

stop_watching(stream::_FDWatcher) = Base.depwarn("stop_watching(::_FDWatcher) should not be used", :stop_watching)

end
