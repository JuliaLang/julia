# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Utilities for monitoring files and file descriptors for events.
"""
module FileWatching

export
    # one-shot API (returns results):
    watch_file, # efficient for small numbers of files
    watch_folder, # efficient for large numbers of files
    unwatch_folder,
    poll_file, # very inefficient alternative to above
    poll_fd, # very efficient, unrelated to above
    # continuous API (returns objects):
    FileMonitor,
    FolderMonitor,
    PollingFileWatcher,
    FDWatcher

import Base: @handle_as, wait, close, eventloop, notify_error, IOError,
    _sizeof_uv_poll, _sizeof_uv_fs_poll, _sizeof_uv_fs_event, _uv_hook_close, uv_error, _UVError,
    iolock_begin, iolock_end, associate_julia_struct, disassociate_julia_struct,
    preserve_handle, unpreserve_handle, isreadable, iswritable, |
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
    FileEvent(r::Bool, c::Bool, t::Bool) = new(r, c, t)
end
FileEvent() = FileEvent(false, false, true)
FileEvent(flags::Integer) = FileEvent((flags & UV_RENAME) != 0,
                                      (flags & UV_CHANGE) != 0,
                                      false)
|(a::FileEvent, b::FileEvent) =
    FileEvent(a.renamed | b.renamed,
              a.changed | b.changed,
              a.timedout | b.timedout)

struct FDEvent
    readable::Bool
    writable::Bool
    disconnect::Bool
    timedout::Bool
    FDEvent(r::Bool, w::Bool, d::Bool, t::Bool) = new(r, w, d, t)
end
# libuv file descriptor event flags
const UV_READABLE = 1
const UV_WRITABLE = 2
const UV_DISCONNECT = 4

isreadable(f::FDEvent) = f.readable
iswritable(f::FDEvent) = f.writable
FDEvent() = FDEvent(false, false, false, true)
FDEvent(flags::Integer) = FDEvent((flags & UV_READABLE) != 0,
                                  (flags & UV_WRITABLE) != 0,
                                  (flags & UV_DISCONNECT) != 0,
                                  false)
|(a::FDEvent, b::FDEvent) =
    FDEvent(a.readable | b.readable,
            a.writable | b.writable,
            a.disconnect | b.disconnect,
            a.timedout | b.timedout)

mutable struct FileMonitor
    handle::Ptr{Cvoid}
    file::String
    notify::Base.ThreadSynchronizer
    events::Int32
    active::Bool
    FileMonitor(file::AbstractString) = FileMonitor(String(file))
    function FileMonitor(file::String)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, file, Base.ThreadSynchronizer(), 0, false)
        associate_julia_struct(handle, this)
        iolock_begin()
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(_UVError("FileMonitor", err))
        end
        iolock_end()
        finalizer(uvfinalize, this)
        return this
    end
end


mutable struct FolderMonitor
    handle::Ptr{Cvoid}
    notify::Channel{Any} # eltype = Union{Pair{String, FileEvent}, IOError}
    open::Bool
    FolderMonitor(folder::AbstractString) = FolderMonitor(String(folder))
    function FolderMonitor(folder::String)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, Channel(Inf), false)
        associate_julia_struct(handle, this)
        iolock_begin()
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(_UVError("FolderMonitor", err))
        end
        this.open = true
        finalizer(uvfinalize, this)
        uv_error("FolderMonitor (start)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32),
                       handle, uv_jl_fseventscb_folder::Ptr{Cvoid}, folder, 0))
        iolock_end()
        return this
    end
end

mutable struct PollingFileWatcher
    handle::Ptr{Cvoid}
    file::String
    interval::UInt32
    notify::Base.ThreadSynchronizer
    active::Bool
    curr_error::Int32
    curr_stat::StatStruct
    PollingFileWatcher(file::AbstractString, interval::Float64=5.007) = PollingFileWatcher(String(file), interval)
    function PollingFileWatcher(file::String, interval::Float64=5.007) # same default as nodejs
        handle = Libc.malloc(_sizeof_uv_fs_poll)
        this = new(handle, file, round(UInt32, interval * 1000), Base.ThreadSynchronizer(), false, 0, StatStruct())
        associate_julia_struct(handle, this)
        iolock_begin()
        err = ccall(:uv_fs_poll_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(_UVError("PollingFileWatcher", err))
        end
        finalizer(uvfinalize, this)
        iolock_end()
        return this
    end
end

mutable struct _FDWatcher
    handle::Ptr{Cvoid}
    fdnum::Int # this is NOT the file descriptor
    refcount::Tuple{Int, Int}
    notify::Base.ThreadSynchronizer
    events::Int32
    active::Tuple{Bool, Bool}

    let FDWatchers = Vector{Any}()
        global _FDWatcher, uvfinalize
        @static if Sys.isunix()
            function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
                if !readable && !writable
                    throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
                end
                fdnum = Core.Intrinsics.bitcast(Int32, fd) + 1
                iolock_begin()
                if fdnum > length(FDWatchers)
                    old_len = length(FDWatchers)
                    resize!(FDWatchers, fdnum)
                    FDWatchers[(old_len + 1):fdnum] .= nothing
                elseif FDWatchers[fdnum] !== nothing
                    this = FDWatchers[fdnum]::_FDWatcher
                    this.refcount = (this.refcount[1] + Int(readable), this.refcount[2] + Int(writable))
                    iolock_end()
                    return this
                end
                if ccall(:jl_uv_unix_fd_is_watched, Int32, (RawFD, Ptr{Cvoid}, Ptr{Cvoid}), fd, C_NULL, eventloop()) == 1
                    throw(ArgumentError("$(fd) is already being watched by libuv"))
                end

                handle = Libc.malloc(_sizeof_uv_poll)
                this = new(
                    handle,
                    fdnum,
                    (Int(readable), Int(writable)),
                    Base.ThreadSynchronizer(),
                    0,
                    (false, false))
                associate_julia_struct(handle, this)
                err = ccall(:uv_poll_init, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, RawFD), eventloop(), handle, fd)
                if err != 0
                    Libc.free(handle)
                    throw(_UVError("FDWatcher", err))
                end
                finalizer(uvfinalize, this)
                FDWatchers[fdnum] = this
                iolock_end()
                return this
            end
        end

        function uvfinalize(t::_FDWatcher)
            iolock_begin()
            lock(t.notify)
            try
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
                notify(t.notify, FDEvent())
            finally
                unlock(t.notify)
            end
            iolock_end()
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
                Base.ThreadSynchronizer(),
                0,
                (false, false))
            associate_julia_struct(handle, this)
            iolock_begin()
            err = ccall(:uv_poll_init, Int32, (Ptr{Cvoid},  Ptr{Cvoid}, WindowsRawSocket),
                                               eventloop(), handle,     fd)
            iolock_end()
            if err != 0
                Libc.free(handle)
                throw(_UVError("FDWatcher", err))
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
    iolock_begin()
    if t.refcount != (0, 0)
        t.refcount = (t.refcount[1] - Int(readable), t.refcount[2] - Int(writable))
    end
    if t.refcount == (0, 0)
        uvfinalize(t)
    end
    iolock_end()
    nothing
end

function close(t::FDWatcher)
    r, w = t.readable, t.writable
    t.readable = t.writable = false
    close(t.watcher, r, w)
end

function uvfinalize(uv::Union{FileMonitor, FolderMonitor, PollingFileWatcher})
    disassociate_julia_struct(uv)
    close(uv)
end

function close(t::Union{FileMonitor, FolderMonitor, PollingFileWatcher})
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
    lock(uv.notify)
    try
        uv.handle = C_NULL
        uv.active = false
        notify(uv.notify, StatStruct())
    finally
        unlock(uv.notify)
    end
    nothing
end

function _uv_hook_close(uv::FileMonitor)
    lock(uv.notify)
    try
        uv.handle = C_NULL
        uv.active = false
        notify(uv.notify, FileEvent())
    finally
        unlock(uv.notify)
    end
    nothing
end

function _uv_hook_close(uv::FolderMonitor)
    uv.open = false
    uv.handle = C_NULL
    close(uv.notify)
    nothing
end

function uv_fseventscb_file(handle::Ptr{Cvoid}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FileMonitor
    lock(t.notify)
    try
        if status != 0
            notify_error(t.notify, _UVError("FileMonitor", status))
        else
            t.events |= events
            notify(t.notify, FileEvent(events))
        end
    finally
        unlock(t.notify)
    end
    nothing
end

function uv_fseventscb_folder(handle::Ptr{Cvoid}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FolderMonitor
    if status != 0
        put!(t.notify, _UVError("FolderMonitor", status))
    else
        fname = (filename == C_NULL) ? "" : unsafe_string(convert(Cstring, filename))
        put!(t.notify, fname => FileEvent(events))
    end
    nothing
end

function uv_pollcb(handle::Ptr{Cvoid}, status::Int32, events::Int32)
    t = @handle_as handle _FDWatcher
    lock(t.notify)
    try
        if status != 0
            notify_error(t.notify, _UVError("FDWatcher", status))
        else
            t.events |= events
            if t.active[1] || t.active[2]
                if isempty(t.notify)
                    # if we keep hearing about events when nobody appears to be listening,
                    # stop the poll to save cycles
                    t.active = (false, false)
                    ccall(:uv_poll_stop, Int32, (Ptr{Cvoid},), t.handle)
                end
            end
            notify(t.notify, FDEvent(events))
        end
    finally
        unlock(t.notify)
    end
    nothing
end

function uv_fspollcb(handle::Ptr{Cvoid}, status::Int32, prev::Ptr, curr::Ptr)
    t = @handle_as handle PollingFileWatcher
    old_status = t.curr_error
    t.curr_error = status
    if status == 0
        t.curr_stat = StatStruct(convert(Ptr{UInt8}, curr))
    end
    if status == 0 || status != old_status
        prev_stat = StatStruct(convert(Ptr{UInt8}, prev))
        lock(t.notify)
        try
            notify(t.notify, prev_stat)
        finally
            unlock(t.notify)
        end
    end
    nothing
end

function __init__()
    global uv_jl_pollcb = @cfunction(uv_pollcb, Cvoid, (Ptr{Cvoid}, Cint, Cint))
    global uv_jl_fspollcb = @cfunction(uv_fspollcb, Cvoid, (Ptr{Cvoid}, Cint, Ptr{Cvoid}, Ptr{Cvoid}))
    global uv_jl_fseventscb_file = @cfunction(uv_fseventscb_file, Cvoid, (Ptr{Cvoid}, Ptr{Int8}, Int32, Int32))
    global uv_jl_fseventscb_folder = @cfunction(uv_fseventscb_folder, Cvoid, (Ptr{Cvoid}, Ptr{Int8}, Int32, Int32))
    nothing
end

function start_watching(t::_FDWatcher)
    iolock_begin()
    t.handle == C_NULL && return throw(ArgumentError("FDWatcher is closed"))
    readable = t.refcount[1] > 0
    writable = t.refcount[2] > 0
    if t.active[1] != readable || t.active[2] != writable
        # make sure the READABLE / WRITEABLE state is updated
        uv_error("FDWatcher (start)",
                 ccall(:uv_poll_start, Int32, (Ptr{Cvoid}, Int32, Ptr{Cvoid}),
                       t.handle,
                       (readable ? UV_READABLE : 0) | (writable ? UV_WRITABLE : 0),
                       uv_jl_pollcb::Ptr{Cvoid}))
        t.active = (readable, writable)
    end
    iolock_end()
    nothing
end

function start_watching(t::PollingFileWatcher)
    iolock_begin()
    t.handle == C_NULL && return throw(ArgumentError("PollingFileWatcher is closed"))
    if !t.active
        uv_error("PollingFileWatcher (start)",
                 ccall(:uv_fs_poll_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, UInt32),
                       t.handle, uv_jl_fspollcb::Ptr{Cvoid}, t.file, t.interval))
        t.active = true
    end
    iolock_end()
    nothing
end

function stop_watching(t::PollingFileWatcher)
    iolock_begin()
    lock(t.notify)
    try
        if t.active && isempty(t.notify)
            t.active = false
            uv_error("PollingFileWatcher (stop)",
                     ccall(:uv_fs_poll_stop, Int32, (Ptr{Cvoid},), t.handle))
        end
    finally
        unlock(t.notify)
    end
    iolock_end()
    nothing
end

function start_watching(t::FileMonitor)
    iolock_begin()
    t.handle == C_NULL && return throw(ArgumentError("FileMonitor is closed"))
    if !t.active
        uv_error("FileMonitor (start)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32),
                       t.handle, uv_jl_fseventscb_file::Ptr{Cvoid}, t.file, 0))
        t.active = true
    end
    iolock_end()
    nothing
end

function stop_watching(t::FileMonitor)
    iolock_begin()
    lock(t.notify)
    try
        if t.active && isempty(t.notify)
            t.active = false
            uv_error("FileMonitor (stop)",
                     ccall(:uv_fs_event_stop, Int32, (Ptr{Cvoid},), t.handle))
        end
    finally
        unlock(t.notify)
    end
    iolock_end()
    nothing
end

function wait(fdw::FDWatcher)
    GC.@preserve fdw begin
        return wait(fdw.watcher, readable = fdw.readable, writable = fdw.writable)
    end
end

function wait(fdw::_FDWatcher; readable=true, writable=true)
    events = FDEvent(Int32(0))
    iolock_begin()
    preserve_handle(fdw)
    lock(fdw.notify)
    try
        while true
            haveevent = false
            events |= FDEvent(fdw.events)
            if readable && isreadable(events)
                fdw.events &= ~UV_READABLE
                haveevent = true
            end
            if writable && iswritable(events)
                fdw.events &= ~UV_WRITABLE
                haveevent = true
            end
            if haveevent
                break
            end
            if fdw.refcount == (0, 0) # !open
                throw(EOFError())
            else
                start_watching(fdw) # make sure the poll is active
                iolock_end()
                events = wait(fdw.notify)::FDEvent
                unlock(fdw.notify)
                iolock_begin()
                lock(fdw.notify)
            end
        end
    finally
        unlock(fdw.notify)
        unpreserve_handle(fdw)
    end
    iolock_end()
    return events
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
    iolock_begin()
    preserve_handle(pfw)
    lock(pfw.notify)
    local prevstat
    try
        start_watching(pfw)
        iolock_end()
        prevstat = wait(pfw.notify)::StatStruct
        unlock(pfw.notify)
        iolock_begin()
        lock(pfw.notify)
    finally
        unlock(pfw.notify)
        unpreserve_handle(pfw)
    end
    stop_watching(pfw)
    iolock_end()
    if pfw.handle == C_NULL
        return prevstat, EOFError()
    elseif pfw.curr_error != 0
        return prevstat, _UVError("PollingFileWatcher", pfw.curr_error)
    else
        return prevstat, pfw.curr_stat
    end
end

function wait(m::FileMonitor)
    iolock_begin()
    preserve_handle(m)
    lock(m.notify)
    local events
    try
        start_watching(m)
        iolock_end()
        events = wait(m.notify)::FileEvent
        events |= FileEvent(m.events)
        m.events = 0
        unlock(m.notify)
        iolock_begin()
        lock(m.notify)
    finally
        unlock(m.notify)
        unpreserve_handle(m)
    end
    stop_watching(m)
    iolock_end()
    return events
end

function wait(m::FolderMonitor)
    m.handle == C_NULL && return throw(ArgumentError("FolderMonitor is closed"))
    if isready(m.notify)
        evt = take!(m.notify) # non-blocking fast-path
    else
        preserve_handle(m)
        evt = try
                take!(m.notify)
            catch ex
                unpreserve_handle(m)
                if ex isa InvalidStateException && ex.state === :closed
                    rethrow(EOFError()) # `wait(::Channel)` throws the wrong exception
                end
                rethrow()
            end
    end
    if evt isa Pair{String, FileEvent}
        return evt
    else
        throw(evt)
    end
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
            result::FDEvent = FDEvent()
            @async (sleep(timeout_s); notify(wt))
            @async begin
                try
                    result = wait(fdw, readable=readable, writable=writable)
                catch e
                    notify_error(wt, e)
                    return
                end
                notify(wt)
            end
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
    fm = FileMonitor(s)
    try
        if timeout_s >= 0
            @async (sleep(timeout_s); close(fm))
        end
        return wait(fm)
    finally
        close(fm)
    end
end

"""
    watch_folder(path::AbstractString, timeout_s::Real=-1)

Watches a file or directory `path` for changes until a change has occurred or `timeout_s`
seconds have elapsed.

This will continuing tracking changes for `path` in the background until
`unwatch_folder` is called on the same `path`.

The returned value is an pair where the first field is the name of the changed file (if available)
and the second field is an object with boolean fields `changed`, `renamed`, and `timedout`,
giving the event.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.
"""
watch_folder(s::AbstractString, timeout_s::Real=-1) = watch_folder(String(s), timeout_s)
function watch_folder(s::String, timeout_s::Real=-1)
    fm = get!(watched_folders, s) do
        return FolderMonitor(s)
    end
    if timeout_s >= 0 && !isready(fm.notify)
        if timeout_s <= 0.010
            # for very small timeouts, we can just sleep for the whole timeout-interval
            (timeout_s == 0) ? yield() : sleep(timeout_s)
            if !isready(fm.notify)
                return "" => FileEvent() # timeout
            end
            # fall-through to a guaranteed non-blocking fast-path call to wait
        else
            # If we may need to be able to cancel via a timeout,
            # create a second monitor object just for that purpose.
            # We still take the events from the primary stream.
            fm2 = FileMonitor(s)
            try
                @async (sleep(timeout_s); close(fm2))
                while isopen(fm.notify) && !isready(fm.notify)
                    fm2.handle == C_NULL && return "" => FileEvent() # timeout
                    wait(fm2)
                end
            finally
                close(fm2)
            end
            # guaranteed that next call to `wait(fm)` is non-blocking
            # since we haven't entered the libuv event loop yet
            # or the Base scheduler workqueue since last testing `isready`
        end
    end
    return wait(fm)
end

"""
    unwatch_folder(path::AbstractString)

Stop background tracking of changes for `path`.
It is not recommended to do this while another task is waiting for
`watch_folder` to return on the same path, as the result may be unpredictable.
"""
unwatch_folder(s::AbstractString) = unwatch_folder(String(s))
function unwatch_folder(s::String)
    fm = pop!(watched_folders, s, nothing)
    fm === nothing || close(fm)
    nothing
end

const watched_folders = Dict{String, FolderMonitor}()

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
    pfw = PollingFileWatcher(s, Float64(interval_seconds))
    try
        if timeout_s >= 0
            @async (sleep(timeout_s); close(pfw))
        end
        statdiff = wait(pfw)
        if isa(statdiff[2], IOError)
            # file didn't initially exist, continue watching for it to be created (or the error to change)
            statdiff = wait(pfw)
        end
        return statdiff
    finally
        close(pfw)
    end
end

end
