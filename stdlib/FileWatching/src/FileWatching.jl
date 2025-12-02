# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Utilities for monitoring files and file descriptors for events.
"""
module FileWatching

export
    # one-shot API (returns results, race-y):
    watch_file, # efficient for small numbers of files
    watch_folder, # efficient for large numbers of files
    unwatch_folder,
    poll_file, # very inefficient alternative to above
    poll_fd, # very efficient, unrelated to above
    # continuous API (returns objects):
    FileMonitor,
    FolderMonitor,
    PollingFileWatcher,
    FDWatcher,
    # pidfile:
    mkpidlock,
    trymkpidlock

import Base: @handle_as, wait, close, eventloop, notify_error, IOError,
    uv_req_data, uv_req_set_data, associate_julia_struct, disassociate_julia_struct,
    _sizeof_uv_poll, _sizeof_uv_fs, _sizeof_uv_fs_event, _uv_hook_close, uv_error, _UVError,
    iolock_begin, iolock_end, preserve_handle, unpreserve_handle,
    isreadable, iswritable, isopen, |, getproperty, propertynames
import Base.Filesystem: StatStruct, uv_fs_req_cleanup
if Sys.iswindows()
    import Base.WindowsRawSocket
end


# libuv file watching event flags
const UV_RENAME = Int32(1)
const UV_CHANGE = Int32(2)
struct FileEvent
    renamed::Bool
    changed::Bool
    timedout::Bool # aka canceled
    FileEvent(r::Bool, c::Bool, t::Bool) = new(r, c, t)
end
FileEvent() = FileEvent(false, false, true)
FileEvent(flags::Integer) = FileEvent((flags & UV_RENAME) != 0,
                                      (flags & UV_CHANGE) != 0,
                                      iszero(flags))
|(a::FileEvent, b::FileEvent) =
    FileEvent(a.renamed | b.renamed,
              a.changed | b.changed,
              a.timedout | b.timedout)

# libuv file descriptor event flags
const UV_READABLE = Int32(1)
const UV_WRITABLE = Int32(2)
const UV_DISCONNECT = Int32(4)
const UV_PRIORITIZED = Int32(8)
struct FDEvent
    events::Int32
    FDEvent(flags::Integer=0) = new(flags)
end

FDEvent(r::Bool, w::Bool, d::Bool, t::Bool) = FDEvent((UV_READABLE * r) | (UV_WRITABLE * w) | (UV_DISCONNECT * d)) # deprecated method

function getproperty(f::FDEvent, field::Symbol)
    events = getfield(f, :events)
    field === :readable && return (events & UV_READABLE) != 0
    field === :writable && return (events & UV_WRITABLE) != 0
    field === :disconnect && return (events & UV_DISCONNECT) != 0
    field === :prioritized && return (events & UV_PRIORITIZED) != 0
    field === :timedout && return events == 0
    field === :events && return Int(events)
    getfield(f, field)::Union{}
end
propertynames(f::FDEvent) = (:readable, :writable, :disconnect, :prioritized, :timedout, :events)

isreadable(f::FDEvent) = f.readable
iswritable(f::FDEvent) = f.writable
|(a::FDEvent, b::FDEvent) = FDEvent(getfield(a, :events) | getfield(b, :events))

# Callback functions

function uv_fseventscb_file(handle::Ptr{Cvoid}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FileMonitor
    lock(t.notify)
    try
        if status != 0
            t.ioerrno = status
            notify_error(t.notify, _UVError("FileMonitor", status))
            uvfinalize(t)
        elseif events != t.events
            events = t.events |= events
            notify(t.notify, all=false)
        end
    finally
        unlock(t.notify)
    end
    nothing
end

function uv_fseventscb_folder(handle::Ptr{Cvoid}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FolderMonitor
    lock(t.notify)
    try
        if status != 0
            notify_error(t.notify, _UVError("FolderMonitor", status))
        else
            fname = (filename == C_NULL) ? "" : unsafe_string(convert(Cstring, filename))
            push!(t.channel, fname => FileEvent(events))
            notify(t.notify)
        end
    finally
        unlock(t.notify)
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
            notify(t.notify, events)
        end
    finally
        unlock(t.notify)
    end
    nothing
end

function uv_fspollcb(req::Ptr{Cvoid})
    pfw = unsafe_pointer_to_objref(uv_req_data(req))::PollingFileWatcher
    pfw.active = false
    unpreserve_handle(pfw)
    @assert pointer(pfw.stat_req) == req
    r = Int32(ccall(:uv_fs_get_result, Cssize_t, (Ptr{Cvoid},), req))
    statbuf = ccall(:uv_fs_get_statbuf, Ptr{UInt8}, (Ptr{Cvoid},), req)
    curr_stat = StatStruct(pfw.file, statbuf, r)
    uv_fs_req_cleanup(req)
    lock(pfw.notify)
    try
        if !isempty(pfw.notify) # must discard the update if nobody watching
            if pfw.ioerrno != r || (r == 0 && pfw.prev_stat != curr_stat)
                if r == 0
                    pfw.prev_stat = curr_stat
                end
                pfw.ioerrno = r
                notify(pfw.notify, true)
            end
            pfw.timer = Timer(pfw.interval) do t
                # async task
                iolock_begin()
                lock(pfw.notify)
                try
                    if pfw.timer === t # use identity check to test if this callback is stale by the time we got the lock
                        pfw.timer = nothing
                        @assert !pfw.active
                        if isopen(pfw) && !isempty(pfw.notify)
                            preserve_handle(pfw)
                            uv_jl_fspollcb = @cfunction(uv_fspollcb, Cvoid, (Ptr{Cvoid},))
                            err = ccall(:uv_fs_stat, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                                eventloop(), pfw.stat_req, pfw.file, uv_jl_fspollcb::Ptr{Cvoid})
                            err == 0 || notify(pfw.notify, _UVError("PollingFileWatcher (start)", err), error=true) # likely just ENOMEM
                            pfw.active = true
                        end
                    end
                finally
                    unlock(pfw.notify)
                end
                iolock_end()
                nothing
            end
        end
    finally
        unlock(pfw.notify)
    end
    nothing
end

# Types

"""
    FileMonitor(path::AbstractString)

Watch file or directory `path` (which must exist) for changes until a change occurs. This
function does not poll the file system and instead uses platform-specific functionality to
receive notifications from the operating system (e.g. via inotify on Linux). See the NodeJS
documentation linked below for details.

`fm = FileMonitor(path)` acts like an auto-reset Event, so `wait(fm)` blocks until there has
been at least one event in the file originally at the given path and then returns an object
with boolean fields `renamed`, `changed`, `timedout` summarizing all changes that have
occurred since the last call to `wait` returned.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.
"""
mutable struct FileMonitor
    @atomic handle::Ptr{Cvoid}
    const file::String
    const notify::Base.ThreadSynchronizer
    events::Int32 # accumulator for events that occurred since the last wait call, similar to Event with autoreset
    ioerrno::Int32 # record the error, if any occurs (unlikely)
    FileMonitor(file::AbstractString) = FileMonitor(String(file))
    function FileMonitor(file::String)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, file, Base.ThreadSynchronizer(), 0, 0)
        associate_julia_struct(handle, this)
        iolock_begin()
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            uv_error("FileMonitor", err)
        end
        finalizer(uvfinalize, this)
        uv_jl_fseventscb_file = @cfunction(uv_fseventscb_file, Cvoid, (Ptr{Cvoid}, Ptr{Int8}, Int32, Int32))
        uv_error("FileMonitor (start)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32),
                       this.handle, uv_jl_fseventscb_file::Ptr{Cvoid}, file, 0))
        iolock_end()
        return this
    end
end


"""
    FolderMonitor(folder::AbstractString)

Watch a file or directory `path` for changes until a change has occurred. This function does
not poll the file system and instead uses platform-specific functionality to receive
notifications from the operating system (e.g. via inotify on Linux). See the NodeJS
documentation linked below for details.

This acts similar to a Channel, so calling `take!` (or `wait`) blocks until some change has
occurred. The `wait` function will return a pair where the first field is the name of the
changed file (if available) and the second field is an object with boolean fields `renamed`
and `changed`, giving the event that occurred on it.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.
"""
mutable struct FolderMonitor
    @atomic handle::Ptr{Cvoid}
    # notify::Channel{Any} # eltype = Union{Pair{String, FileEvent}, IOError}
    const notify::Base.ThreadSynchronizer
    const channel::Vector{Any} # eltype = Pair{String, FileEvent}
    FolderMonitor(folder::AbstractString) = FolderMonitor(String(folder))
    function FolderMonitor(folder::String)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, Base.ThreadSynchronizer(), [])
        associate_julia_struct(handle, this)
        iolock_begin()
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(_UVError("FolderMonitor", err))
        end
        finalizer(uvfinalize, this)
        uv_jl_fseventscb_folder = @cfunction(uv_fseventscb_folder, Cvoid, (Ptr{Cvoid}, Ptr{Int8}, Int32, Int32))
        uv_error("FolderMonitor (start)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32),
                       handle, uv_jl_fseventscb_folder::Ptr{Cvoid}, folder, 0))
        iolock_end()
        return this
    end
end

# this is similar to uv_fs_poll, but strives to avoid the design mistakes that make it unsuitable for any usable purpose
# https://github.com/libuv/libuv/issues/4543
"""
    PollingFileWatcher(path::AbstractString, interval_s::Real=5.007)

Monitor a file for changes by polling `stat` every `interval_s` seconds until a change
occurs or `timeout_s` seconds have elapsed. The `interval_s` should be a long period; the
default is 5.007 seconds. Call `stat` on it to get the most recent, but old, result.

This acts like an auto-reset Event, so calling `wait` blocks until the `stat` result has
changed since the previous value captured upon entry to the `wait` call. The `wait` function
will return a pair of status objects `(previous, current)` once any `stat` change is
detected since the previous time that `wait` was called. The `previous` status is always a
`StatStruct`, but it may have all of the fields zeroed (indicating the file didn't
previously exist, or wasn't previously accessible).

The `current` status object may be a `StatStruct`, an `EOFError` (if the wait is canceled by
closing this object), or some other `Exception` subtype (if the `stat` operation failed: for
example, if the path is removed). Note that `stat` value may be outdated if the file has
changed again multiple times.

Using [`FileMonitor`](@ref) for this operation is preferred, since it is more reliable and
efficient, although in some situations it may not be available.
"""
mutable struct PollingFileWatcher
    file::String
    interval::Float64
    const notify::Base.ThreadSynchronizer # lock protects all fields which can be changed (including interval and file, if you really must)
    timer::Union{Nothing,Timer}
    const stat_req::Memory{UInt8}
    active::Bool # whether there is already an uv_fspollcb in-flight, so to speak
    closed::Bool # whether the user has explicitly destroyed this
    ioerrno::Int32 # the stat errno as of the last result
    prev_stat::StatStruct # the stat as of the last successful result
    PollingFileWatcher(file::AbstractString, interval::Float64=5.007) = PollingFileWatcher(String(file), interval)
    function PollingFileWatcher(file::String, interval::Float64=5.007) # same default as nodejs
        stat_req = Memory{UInt8}(undef, Int(_sizeof_uv_fs))
        this = new(file, interval, Base.ThreadSynchronizer(), nothing, stat_req, false, false, 0, StatStruct())
        uv_req_set_data(stat_req, this)
        wait(this) # initialize with the current stat before return
        return this
    end
end

mutable struct _FDWatcher
    @atomic handle::Ptr{Cvoid}
    const fdnum::Int # this is NOT the file descriptor
    refcount::Tuple{Int, Int}
    const notify::Base.ThreadSynchronizer
    events::Int32
    active::Tuple{Bool, Bool}

    let FDWatchers = Vector{Any}() # n.b.: this structure and the refcount are protected by the iolock
        global _FDWatcher, uvfinalize
        @static if Sys.isunix()
            _FDWatcher(fd::RawFD, mask::FDEvent) = _FDWatcher(fd, mask.readable, mask.writable)
            function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
                fdnum = Core.Intrinsics.bitcast(Int32, fd) + 1
                if fdnum <= 0
                    throw(ArgumentError("Passed file descriptor fd=$(fd) is not a valid file descriptor"))
                elseif !readable && !writable
                    throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
                end

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
                    Int32(0),
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
                    @atomic :monotonic t.handle = C_NULL
                end
                t.refcount = (0, 0)
                t.active = (false, false)
                @static if Sys.isunix()
                    if FDWatchers[t.fdnum] === t
                        FDWatchers[t.fdnum] = nothing
                    end
                end
                notify(t.notify, Int32(0))
            finally
                unlock(t.notify)
            end
            iolock_end()
            nothing
        end
    end

    @static if Sys.iswindows()
        _FDWatcher(fd::RawFD, mask::FDEvent) = _FDWatcher(fd, mask.readable, mask.writable)
        function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
            fdnum = Core.Intrinsics.bitcast(Int32, fd) + 1
            if fdnum <= 0
                throw(ArgumentError("Passed file descriptor fd=$(fd) is not a valid file descriptor"))
            end

            handle = Libc._get_osfhandle(fd)
            return _FDWatcher(handle, readable, writable)
        end
        _FDWatcher(fd::WindowsRawSocket, mask::FDEvent) = _FDWatcher(fd, mask.readable, mask.writable)
        function _FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
            if fd == Base.INVALID_OS_HANDLE
                throw(ArgumentError("Passed file descriptor fd=$(fd) is not a valid file descriptor"))
            elseif !readable && !writable
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

"""
    FDWatcher(fd::Union{RawFD,WindowsRawSocket}, readable::Bool, writable::Bool)

Monitor a file descriptor `fd` for changes in the read or write availability.

The keyword arguments determine which of read and/or write status should be monitored; at
least one of them must be set to `true`.

The returned value is an object with boolean fields `readable`, `writable`, and `timedout`,
giving the result of the polling.

This acts like a level-set event, so calling `wait` blocks until one of those conditions is
met, but then continues to return without blocking until the condition is cleared (either
there is no more to read, or no more space in the write buffer, or both).

!!! warning
    You must call `close` manually, when finished with this object, before the fd
    argument is closed. Failure to do so risks serious crashes.
"""
mutable struct FDWatcher
    # WARNING: make sure `close` has been manually called on this watcher before closing / destroying `fd`
    const watcher::_FDWatcher
    mask::FDEvent
    function FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
        return FDWatcher(fd, FDEvent(readable, writable, false, false))
    end
    function FDWatcher(fd::RawFD, mask::FDEvent)
        this = new(_FDWatcher(fd, mask), mask)
        finalizer(close, this)
        return this
    end
    @static if Sys.iswindows()
        function FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
            return FDWatcher(fd, FDEvent(readable, writable, false, false))
        end
        function FDWatcher(fd::WindowsRawSocket, mask::FDEvent)
            this = new(_FDWatcher(fd, mask), mask)
            finalizer(close, this)
            return this
        end
    end
end

function getproperty(fdw::FDWatcher, s::Symbol)
    # support deprecated field names
    s === :readable && return getfield(fdw, :mask).readable
    s === :writable && return getfield(fdw, :mask).writable
    return getfield(fdw, s)
end

close(t::_FDWatcher, mask::FDEvent) = close(t, mask.readable, mask.writable)
function close(t::_FDWatcher, readable::Bool, writable::Bool)
    iolock_begin()
    if t.refcount != (0, 0)
        t.refcount = (t.refcount[1] - Int(readable), t.refcount[2] - Int(writable))
    end
    if t.refcount == (0, 0)
        uvfinalize(t)
    else
        @lock t.notify notify(t.notify, Int32(0))
    end
    iolock_end()
    nothing
end

function close(t::FDWatcher)
    mask = t.mask
    t.mask = FDEvent()
    close(t.watcher, mask)
end

function uvfinalize(uv::Union{FileMonitor, FolderMonitor})
    iolock_begin()
    handle = @atomicswap :monotonic uv.handle = C_NULL
    if handle != C_NULL
        disassociate_julia_struct(handle) # close (and free) without notify
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), handle)
    end
    iolock_end()
end

function close(t::Union{FileMonitor, FolderMonitor})
    iolock_begin()
    handle = t.handle
    if handle != C_NULL
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), handle)
    end
    iolock_end()
end

function close(pfw::PollingFileWatcher)
    timer = nothing
    lock(pfw.notify)
    try
        pfw.closed = true
        notify(pfw.notify, false)
        timer = pfw.timer
        pfw.timer = nothing
    finally
        unlock(pfw.notify)
    end
    timer === nothing || close(timer)
    nothing
end

function _uv_hook_close(uv::_FDWatcher)
    # fyi: jl_atexit_hook can cause this to get called too
    Libc.free(@atomicswap :monotonic uv.handle = C_NULL)
    uvfinalize(uv)
    nothing
end

function _uv_hook_close(uv::FileMonitor)
    lock(uv.notify)
    try
        Libc.free(@atomicswap :monotonic uv.handle = C_NULL)
        notify(uv.notify)
    finally
        unlock(uv.notify)
    end
    nothing
end

function _uv_hook_close(uv::FolderMonitor)
    lock(uv.notify)
    try
        Libc.free(@atomicswap :monotonic uv.handle = C_NULL)
        notify_error(uv.notify, EOFError())
    finally
        unlock(uv.notify)
    end
    nothing
end

isopen(fm::FileMonitor) = fm.handle != C_NULL
isopen(fm::FolderMonitor) = fm.handle != C_NULL
isopen(pfw::PollingFileWatcher) = !pfw.closed
isopen(pfw::_FDWatcher) = pfw.refcount != (0, 0)
isopen(pfw::FDWatcher) = !pfw.mask.timedout

Base.stat(pfw::PollingFileWatcher) = Base.checkstat(@lock pfw.notify pfw.prev_stat)

# n.b. this _wait may return spuriously early with a timedout event
function _wait(fdw::_FDWatcher, mask::FDEvent)
    iolock_begin()
    preserve_handle(fdw)
    lock(fdw.notify)
    try
        events = FDEvent(fdw.events & mask.events)
        if !isopen(fdw) # !open
            throw(EOFError())
        elseif events.timedout
            fdw.handle == C_NULL && throw(ArgumentError("FDWatcher is closed"))
            # start_watching to make sure the poll is active
            readable = fdw.refcount[1] > 0
            writable = fdw.refcount[2] > 0
            if fdw.active[1] != readable || fdw.active[2] != writable
                # make sure the READABLE / WRITEABLE state is updated
                uv_jl_pollcb = @cfunction(uv_pollcb, Cvoid, (Ptr{Cvoid}, Cint, Cint))
                uv_error("FDWatcher (start)",
                         ccall(:uv_poll_start, Int32, (Ptr{Cvoid}, Int32, Ptr{Cvoid}),
                               fdw.handle,
                               (readable ? UV_READABLE : 0) | (writable ? UV_WRITABLE : 0),
                               uv_jl_pollcb::Ptr{Cvoid}))
                fdw.active = (readable, writable)
            end
            iolock_end()
            return FDEvent(wait(fdw.notify)::Int32)
        else
            iolock_end()
            return events
        end
    finally
        unlock(fdw.notify)
        unpreserve_handle(fdw)
    end
end

function wait(fdw::_FDWatcher; readable=true, writable=true)
    return wait(fdw, FDEvent(readable, writable, false, false))
end
function wait(fdw::_FDWatcher, mask::FDEvent)
    while true
        mask.timedout && return mask
        events = _wait(fdw, mask)
        if !events.timedout
            @lock fdw.notify fdw.events &= ~events.events
            return events
        end
    end
end

function wait(fdw::FDWatcher)
    isopen(fdw) || throw(EOFError())
    while true
        events = GC.@preserve fdw _wait(fdw.watcher, fdw.mask)
        isopen(fdw) || throw(EOFError())
        if !events.timedout
            @lock fdw.watcher.notify fdw.watcher.events &= ~events.events
            return events
        end
    end
end

function wait(socket::RawFD; readable=false, writable=false)
    return wait(socket, FDEvent(readable, writable, false, false))
end
function wait(fd::RawFD, mask::FDEvent)
    fdw = _FDWatcher(fd, mask)
    try
        return wait(fdw, mask)
    finally
        close(fdw, mask)
    end
end


if Sys.iswindows()
    function wait(socket::WindowsRawSocket; readable=false, writable=false)
        return wait(socket, FDEvent(readable, writable, false, false))
    end
    function wait(socket::WindowsRawSocket, mask::FDEvent)
        fdw = _FDWatcher(socket, mask)
        try
            return wait(fdw, mask)
        finally
            close(fdw, mask)
        end
    end
end

function wait(pfw::PollingFileWatcher)
    iolock_begin()
    lock(pfw.notify)
    prevstat = pfw.prev_stat
    havechange = false
    timer = nothing
    try
        # we aren't too strict about the first interval after `wait`, but rather always
        # check right away to see if it had immediately changed again, and then repeatedly
        # after interval again until success
        pfw.closed && throw(ArgumentError("PollingFileWatcher is closed"))
        timer = pfw.timer
        pfw.timer = nothing # disable Timer callback
        # start_watching
        if !pfw.active
            preserve_handle(pfw)
            uv_jl_fspollcb = @cfunction(uv_fspollcb, Cvoid, (Ptr{Cvoid},))
            err = ccall(:uv_fs_stat, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                eventloop(), pfw.stat_req, pfw.file, uv_jl_fspollcb::Ptr{Cvoid})
            err == 0 || uv_error("PollingFileWatcher (start)", err) # likely just ENOMEM
            pfw.active = true
        end
        iolock_end()
        havechange = wait(pfw.notify)::Bool
        unlock(pfw.notify)
        iolock_begin()
    catch
        # stop_watching: cleanup any timers from before or after starting this wait before it failed, if there are no other watchers
        latetimer = nothing
        try
            if isempty(pfw.notify)
                latetimer = pfw.timer
                pfw.timer = nothing
            end
        finally
            unlock(pfw.notify)
        end
        if timer !== nothing || latetimer !== nothing
            iolock_end()
            timer === nothing || close(timer)
            latetimer === nothing || close(latetimer)
            iolock_begin()
        end
        rethrow()
    end
    iolock_end()
    timer === nothing || close(timer) # cleanup resources so we don't hang on exit
    if !havechange # user canceled by calling close
        return prevstat, EOFError()
    end
    # grab the most up-to-date stat result as of this time, even if it was a bit newer than
    # the notify call (unlikely, as there would need to be a concurrent call to wait)
    lock(pfw.notify)
    currstat = pfw.prev_stat
    ioerrno = pfw.ioerrno
    unlock(pfw.notify)
    if ioerrno == 0
        @assert currstat.ioerrno == 0
        return prevstat, currstat
    elseif ioerrno in (Base.UV_ENOENT, Base.UV_ENOTDIR, Base.UV_EINVAL)
        return prevstat, StatStruct(pfw.file, Ptr{UInt8}(0), ioerrno)
    else
        return prevstat, _UVError("PollingFileWatcher", ioerrno)
    end
end

function wait(m::FileMonitor)
    m.handle == C_NULL && throw(EOFError())
    preserve_handle(m)
    lock(m.notify)
    try
        while true
            m.handle == C_NULL && throw(EOFError())
            events = @atomicswap :not_atomic m.events = 0
            events == 0 || return FileEvent(events)
            if m.ioerrno != 0
                uv_error("FileMonitor", m.ioerrno)
            end
            wait(m.notify)
        end
    finally
        unlock(m.notify)
        unpreserve_handle(m)
    end
end

function wait(m::FolderMonitor)
    m.handle == C_NULL && throw(EOFError())
    preserve_handle(m)
    lock(m.notify)
    evt = try
            m.handle == C_NULL && throw(EOFError())
            while isempty(m.channel)
                wait(m.notify)
            end
            popfirst!(m.channel)
        finally
            unlock(m.notify)
            unpreserve_handle(m)
        end
    return evt::Pair{String, FileEvent}
end
Base.take!(m::FolderMonitor) = wait(m) # Channel-like API


"""
    poll_fd(fd, timeout_s::Real=-1; readable=false, writable=false)

Monitor a file descriptor `fd` for changes in the read or write availability, and with a
timeout given by `timeout_s` seconds.

The keyword arguments determine which of read and/or write status should be monitored; at
least one of them must be set to `true`.

The returned value is an object with boolean fields `readable`, `writable`, and `timedout`,
giving the result of the polling.

This is a thin wrapper over calling `wait` on a [`FDWatcher`](@ref), which implements the
functionality but requires the user to call `close` manually when finished with it, or risk
serious crashes.
"""
function poll_fd(s::Union{RawFD, Sys.iswindows() ? WindowsRawSocket : Union{}}, timeout_s::Real=-1; readable=false, writable=false)
    mask = FDEvent(readable, writable, false, false)
    mask.timedout && return mask
    fdw = _FDWatcher(s, mask)
    local timer
    # we need this flag to explicitly track whether we call `close` already, to update the internal refcount correctly
    timedout = false # TODO: make this atomic
    try
        if timeout_s >= 0
            # delay creating the timer until shortly before we start the poll wait
            timer = Timer(timeout_s) do t
                timedout && return
                timedout = true
                close(fdw, mask)
            end
            try
                while true
                    events = _wait(fdw, mask)
                    if timedout || !events.timedout
                        @lock fdw.notify fdw.events &= ~events.events
                        return events
                    end
                end
            catch ex
                ex isa EOFError || rethrow()
                return FDEvent()
            end
        else
            return wait(fdw, mask)
        end
    finally
        if @isdefined(timer)
            if !timedout
                timedout = true
                close(timer)
                close(fdw, mask)
            end
        else
            close(fdw, mask)
        end
    end
end

"""
    watch_file(path::AbstractString, timeout_s::Real=-1)

Watch file or directory `path` for changes until a change occurs or `timeout_s` seconds have
elapsed. This function does not poll the file system and instead uses platform-specific
functionality to receive notifications from the operating system (e.g. via inotify on Linux).
See the NodeJS documentation linked below for details.

The returned value is an object with boolean fields `renamed`, `changed`, and `timedout`,
giving the result of watching the file.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.

This is a thin wrapper over calling `wait` on a [`FileMonitor`](@ref). This function has a
small race window between consecutive calls to `watch_file` where the file might change
without being detected. To avoid this race, use

    fm = FileMonitor(path)
    wait(fm)

directly, re-using the same `fm` each time you `wait`.
"""
function watch_file(s::String, timeout_s::Float64=-1.0)
    fm = FileMonitor(s)
    local timer
    try
        if timeout_s >= 0
            timer = Timer(timeout_s) do t
                close(fm)
            end
        end
        try
            return wait(fm)
        catch ex
            ex isa EOFError && return FileEvent()
            rethrow()
        end
    finally
        close(fm)
        @isdefined(timer) && close(timer)
    end
end
watch_file(s::AbstractString, timeout_s::Real=-1) = watch_file(String(s), Float64(timeout_s))

"""
    watch_folder(path::AbstractString, timeout_s::Real=-1)

Watch a file or directory `path` for changes until a change has occurred or `timeout_s`
seconds have elapsed. This function does not poll the file system and instead uses platform-specific
functionality to receive notifications from the operating system (e.g. via inotify on Linux).
See the NodeJS documentation linked below for details.

This will continuing tracking changes for `path` in the background until
`unwatch_folder` is called on the same `path`.

The returned value is an pair where the first field is the name of the changed file (if available)
and the second field is an object with boolean fields `renamed`, `changed`, and `timedout`,
giving the event.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.

This function is a thin wrapper over calling `wait` on a [`FolderMonitor`](@ref), with added timeout support.
"""
watch_folder(s::AbstractString, timeout_s::Real=-1) = watch_folder(String(s), timeout_s)
function watch_folder(s::String, timeout_s::Real=-1)
    fm = @lock watched_folders get!(watched_folders[], s) do
        return FolderMonitor(s)
    end
    local timer
    if timeout_s >= 0
        @lock fm.notify isempty(fm.channel) || return popfirst!(fm.channel)
        if timeout_s <= 0.010
            # for very small timeouts, we can just sleep for the whole timeout-interval
            (timeout_s == 0) ? yield() : sleep(timeout_s)
            @lock fm.notify isempty(fm.channel) || return popfirst!(fm.channel)
            return "" => FileEvent() # timeout
        else
            timer = Timer(timeout_s) do t
                @lock fm.notify notify(fm.notify)
            end
        end
    end
    # inline a copy of `wait` with added support for checking timer
    fm.handle == C_NULL && throw(EOFError())
    preserve_handle(fm)
    lock(fm.notify)
    evt = try
            fm.handle == C_NULL && throw(EOFError())
            while isempty(fm.channel)
                if @isdefined(timer)
                    isopen(timer) || return "" => FileEvent() # timeout
                end
                wait(fm.notify)
            end
            popfirst!(fm.channel)
        finally
            unlock(fm.notify)
            unpreserve_handle(fm)
            @isdefined(timer) && close(timer)
        end
    return evt::Pair{String, FileEvent}
end

"""
    unwatch_folder(path::AbstractString)

Stop background tracking of changes for `path`.
It is not recommended to do this while another task is waiting for
`watch_folder` to return on the same path, as the result may be unpredictable.
"""
unwatch_folder(s::AbstractString) = unwatch_folder(String(s))
function unwatch_folder(s::String)
    fm = @lock watched_folders pop!(watched_folders[], s, nothing)
    fm === nothing || close(fm)
    nothing
end

const watched_folders = Lockable(Dict{String, FolderMonitor}())

"""
    poll_file(path::AbstractString, interval_s::Real=5.007, timeout_s::Real=-1) -> (previous::StatStruct, current)

Monitor a file for changes by polling every `interval_s` seconds until a change occurs or
`timeout_s` seconds have elapsed. The `interval_s` should be a long period; the default is
5.007 seconds.

Returns a pair of status objects `(previous, current)` when a change is detected.
The `previous` status is always a `StatStruct`, but it may have all of the fields zeroed
(indicating the file didn't previously exist, or wasn't previously accessible).

The `current` status object may be a `StatStruct`, an `EOFError` (indicating the timeout elapsed),
or some other `Exception` subtype (if the `stat` operation failed: for example, if the path does not exist).

To determine when a file was modified, compare `!(current isa StatStruct && prev == current)` to detect
notification of changes to the mtime or inode. However, using [`watch_file`](@ref) for this operation
is preferred, since it is more reliable and efficient, although in some situations it may not be available.

This is a thin wrapper over calling `wait` on a [`PollingFileWatcher`](@ref), which implements
the functionality, but this function has a small race window between consecutive calls to
`poll_file` where the file might change without being detected.
"""
function poll_file(s::AbstractString, interval_seconds::Real=5.007, timeout_s::Real=-1)
    pfw = PollingFileWatcher(s, Float64(interval_seconds))
    local timer
    try
        if timeout_s >= 0
            timer = Timer(timeout_s) do t
                close(pfw)
            end
        end
        return wait(pfw)
    finally
        close(pfw)
        @isdefined(timer) && close(timer)
    end
end

include("pidfile.jl")
import .Pidfile: mkpidlock, trymkpidlock

function __init__()
    Base.mkpidlock_hook = mkpidlock
    Base.trymkpidlock_hook = trymkpidlock
    Base.parse_pidfile_hook = Pidfile.parse_pidfile
    nothing
end

end
