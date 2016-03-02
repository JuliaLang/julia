# This file is a part of Julia. License is MIT: http://julialang.org/license

# filesystem operations

export
    watch_file,
    poll_fd,
    poll_file,
    FileMonitor,
    PollingFileWatcher,
    FDWatcher

import Base: @handle_as, wait, close, uvfinalize, eventloop, notify_error, stream_wait,
    _sizeof_uv_poll, _sizeof_uv_fs_poll, _sizeof_uv_fs_event, _uv_hook_close,
    associate_julia_struct, disassociate_julia_struct
@windows_only import Base.WindowsRawSocket

# libuv file watching event flags
const UV_RENAME = 1
const UV_CHANGE = 2
immutable FileEvent
    renamed::Bool
    changed::Bool
    timedout::Bool
end
FileEvent() = FileEvent(false, false, false)
FileEvent(flags::Integer) = FileEvent((flags & UV_RENAME) != 0,
                                  (flags & UV_CHANGE) != 0,
                                  (flags & FD_TIMEDOUT) != 0)
fetimeout() = FileEvent(false, false, true)

immutable FDEvent
    readable::Bool
    writable::Bool
    timedout::Bool
end
# libuv file descriptor event flags
const UV_READABLE = 1
const UV_WRITABLE = 2
const FD_TIMEDOUT = 4

isreadable(f::FDEvent) = f.readable
iswritable(f::FDEvent) = f.writable
FDEvent() = FDEvent(false, false, false)
FDEvent(flags::Integer) = FDEvent((flags & UV_READABLE) != 0,
                                  (flags & UV_WRITABLE) != 0,
                                  (flags & FD_TIMEDOUT) != 0)
fdtimeout() = FDEvent(false, false, true)

type FileMonitor
    handle::Ptr{Void}
    file::ByteString
    notify::Condition
    active::Bool
    function FileMonitor(file::AbstractString)
        handle = Libc.malloc(_sizeof_uv_fs_event)
        this = new(handle, file, Condition(), false)
        associate_julia_struct(handle, this)
        err = ccall(:uv_fs_event_init, Cint, (Ptr{Void}, Ptr{Void}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(UVError("PollingFileWatcher", err))
        end
        finalizer(this, uvfinalize)
        this
    end
end

type PollingFileWatcher
    handle::Ptr{Void}
    file::ByteString
    interval::UInt32
    notify::Condition
    active::Bool
    function PollingFileWatcher(file::AbstractString, interval::Float64=5.007) # same default as nodejs
        handle = Libc.malloc(_sizeof_uv_fs_poll)
        this = new(handle, file, round(UInt32, interval*1000), Condition(), false)
        associate_julia_struct(handle, this)
        err = ccall(:uv_fs_poll_init, Int32, (Ptr{Void}, Ptr{Void}), eventloop(), handle)
        if err != 0
            Libc.free(handle)
            throw(UVError("PollingFileWatcher", err))
        end
        finalizer(this, uvfinalize)
        return this
    end
end

type _FDWatcher
    handle::Ptr{Void}
    refcount::Tuple{Int, Int}
    notify::Condition
    active::Tuple{Bool, Bool}
    let FDWatchers = Vector{Any}()
        global _FDWatcher
        function _FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
            if !readable && !writable
                throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
            end
            if fd.fd+1 > length(FDWatchers)
                old_len = length(FDWatchers)
                resize!(FDWatchers, fd.fd+1)
                fill!(sub(FDWatchers, old_len+1:fd.fd+1), nothing)
            elseif FDWatchers[fd.fd + 1] !== nothing
                this = FDWatchers[fd.fd + 1]::_FDWatcher
                this.refcount = (this.refcount[1] + Int(readable), this.refcount[2] + Int(writable))
                return this
            end
            @unix_only if ccall(:jl_uv_unix_fd_is_watched, Int32, (Int32, Ptr{Void}, Ptr{Void}), fd.fd, C_NULL, eventloop()) == 1
                throw(ArgumentError("file descriptor $(fd.fd) is already being watched by libuv"))
            end
            handle = Libc.malloc(_sizeof_uv_poll)
            this = new(handle, (Int(readable), Int(writable)), Condition(), (false, false))
            associate_julia_struct(handle, this)
            err = ccall(:uv_poll_init, Int32, (Ptr{Void}, Ptr{Void}, Int32), eventloop(), handle, fd.fd)
            if err != 0
                Libc.free(handle)
                throw(UVError("FDWatcher",err))
            end
            FDWatchers[fd.fd] = this
            return this
        end
        @windows_only function _FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
            if !readable && !writable
                throw(ArgumentError("must specify at least one of readable or writable to create a FDWatcher"))
            end
            handle = Libc.malloc(_sizeof_uv_poll)
            this = new(handle, (Int(readable), Int(writable)), Condition(), (false, false))
            associate_julia_struct(handle, this)
            err = ccall(:uv_poll_init_socket,Int32,(Ptr{Void},   Ptr{Void}, Ptr{Void}),
                                                    eventloop(), handle,    fd.handle)
            if err != 0
                Libc.free(handle)
                throw(UVError("FDWatcher",err))
            end
            return this
        end
        global close
        function close(t::_FDWatcher, readable::Bool, writable::Bool)
            if t.handle != C_NULL
                t.refcount = (t.refcount[1] - Int(readable), t.refcount[2] - Int(writable))
                if t.refcount == (0, 0)
                    t.active = (false, false)
                    disassociate_julia_struct(t)
                    ccall(:jl_close_uv, Void, (Ptr{Void},), t.handle)
                    t.handle = C_NULL
                    fdnum = findfirst(FDWatchers, t)
                    if fdnum > 0
                        FDWatchers[fdnum] = nothing
                    end
                    notify(t.notify, FDEvent(true, true, false))
                    nothing
                end
            end
            nothing
        end
        global _uv_hook_close
        function _uv_hook_close(t::_FDWatcher)
            # jl_atexit_hook can cause this to get called
            t.refcount = (0, 0)
            close(t, false, false)
        end
    end
end
type FDWatcher
    watcher::_FDWatcher
    readable::Bool
    writable::Bool
    function FDWatcher(fd::RawFD, readable::Bool, writable::Bool)
        this = new(_FDWatcher(fd, readable, writable), readable, writable)
        finalizer(this, close)
        return this
    end
    @windows_only function FDWatcher(fd::WindowsRawSocket, readable::Bool, writable::Bool)
        this = new(_FDWatcher(fd, readable, writable), readable, writable)
        finalizer(this, close)
        return this
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
        ccall(:jl_close_uv, Void, (Ptr{Void},), t.handle)
    end
end

function _uv_hook_close(uv::PollingFileWatcher)
    uv.handle = C_NULL
    uv.active = false
    notify(uv.notify, (StatStruct(), StatStruct()))
    nothing
end

function _uv_hook_close(uv::FileMonitor)
    uv.handle = C_NULL
    uv.active = false
    notify(uv.notify, ("", FileEvent()))
    nothing
end

function __init__()
    global uv_jl_pollcb        = cfunction(uv_pollcb, Void, (Ptr{Void}, Cint, Cint))
    global uv_jl_fspollcb      = cfunction(uv_fspollcb, Void, (Ptr{Void}, Cint, Ptr{Void}, Ptr{Void}))
    global uv_jl_fseventscb    = cfunction(uv_fseventscb, Void, (Ptr{Void}, Ptr{Int8}, Int32, Int32))
end

function uv_fseventscb(handle::Ptr{Void}, filename::Ptr, events::Int32, status::Int32)
    t = @handle_as handle FileMonitor
    fname = filename == C_NULL ? "" : bytestring(convert(Ptr{UInt8}, filename))
    if status != 0
        notify_error(t.notify, UVError("FileMonitor", status))
    else
        notify(t.notify, (fname, FileEvent(events)))
    end
    nothing
end

function uv_pollcb(handle::Ptr{Void}, status::Int32, events::Int32)
    t = @handle_as handle _FDWatcher
    if status != 0
        notify_error(t.notify, UVError("FDWatcher", status))
    else
        notify(t.notify, FDEvent(events))
    end
    nothing
end

function uv_fspollcb(handle::Ptr{Void}, status::Int32, prev::Ptr, curr::Ptr)
    t = @handle_as handle PollingFileWatcher
    if status != 0
        notify_error(t.notify, UVError("PollingFileWatcher", status))
    else
        prev_stat = StatStruct(convert(Ptr{UInt8}, prev))
        curr_stat = StatStruct(convert(Ptr{UInt8}, curr))
        notify(t.notify, (prev_stat, curr_stat))
    end
    nothing
end


function start_watching(t::_FDWatcher)
    read, write = t.refcount[1] > 0, t.refcount[2] > 0
    if t.active[1] != read || t.active[2] != write
        uv_error("start_watching (FD)",
                 ccall(:uv_poll_start, Int32, (Ptr{Void}, Int32, Ptr{Void}),
                       t.handle,
                       (read ? UV_READABLE : 0) | (write ? UV_WRITABLE : 0),
                       uv_jl_pollcb::Ptr{Void}))
        t.active = (read, write)
    end
end

function stop_watching(t::_FDWatcher)
    if t.active[1] || t.active[2]
        if isempty(t.notify.waitq)
            t.active = (false, false)
            uv_error("start_watching (FD)",
                     ccall(:uv_poll_stop, Int32, (Ptr{Void},), t.handle))
        else
            start_watching(t) # make sure the READABLE / WRITEABLE state is updated
        end
    end
end

function start_watching(t::PollingFileWatcher)
    if !t.active
        uv_error("start_watching (File)",
                 ccall(:uv_fs_poll_start, Int32, (Ptr{Void}, Ptr{Void}, Cstring, UInt32),
                       t.handle, uv_jl_fspollcb::Ptr{Void}, t.file, t.interval))
        t.active = true
    end
end

function stop_watching(t::PollingFileWatcher)
    if t.active && isempty(t.notify.waitq)
        t.active = false
        uv_error("stop_watching (File)",
                 ccall(:uv_fs_poll_stop, Int32, (Ptr{Void},), t.handle))
    end
end

function start_watching(t::FileMonitor)
    if !t.active
        uv_error("start_watching (FileMonitor)",
                 ccall(:uv_fs_event_start, Int32, (Ptr{Void}, Ptr{Void}, Cstring, Int32),
                       t.handle, uv_jl_fseventscb::Ptr{Void}, t.file, 0))
        t.active = true
    end
end

function stop_watching(t::FileMonitor)
    if t.active && isempty(t.notify.waitq)
        t.active = false
        uv_error("stop_watching (FileMonitor)",
                 ccall(:uv_fs_event_stop, Int32, (Ptr{Void},), t.handle))
    end
end

function wait(fdw::FDWatcher)
    wait(fdw.watcher, readable = fdw.readable, writable = fdw.writable)
end
function wait(fdw::_FDWatcher; readable=true, writable=true)
    local events
    while fdw.refcount != (0, 0)
        start_watching(fdw)
        events = wait(fdw.notify)
        if isa(events, FDEvent) &&
            ((readable && isreadable(events)) || (writable && iswritable(events)))
            break
        end
    end
    stop_watching(fdw)
    events
end

function wait(fd::RawFD; readable=false, writable=false)
    fdw = _FDWatcher(fd, readable, writable)
    try
        return wait(fdw, readable=readable, writable=writable)
    finally
        close(fdw, readable, writable)
    end
end

@windows_only begin
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

function poll_fd(s::Union{RawFD, @windows ? WindowsRawSocket : Union{}}, timeout_s::Real=-1; readable=false, writable=false)
    wt = Condition()
    fdw = _FDWatcher(s, readable, writable)
    try
        if timeout_s >= 0
            result::FDEvent = fdtimeout()

            @schedule (result = wait(fdw, readable=readable, writable=writable); notify(wt))
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

function watch_file(s::AbstractString, timeout_s::Real=-1)
    wt = Condition()
    fm = FileMonitor(s)
    try
        if timeout_s >= 0
            result = fetimeout()

            @schedule (try result = wait(fm); catch e; notify_error(wt, e); end; notify(wt))
            @schedule (sleep(timeout_s); notify(wt))

            wait(wt)
            return result
        else
            return wait(fm)
        end
    finally
        close(fm)
    end
end

function poll_file(s::AbstractString, interval_seconds::Real=5.007, timeout_s::Real=-1)
    wt = Condition()
    pfw = PollingFileWatcher(s, Float64(interval_seconds))
    try
        if timeout_s >= 0
            result = :timeout

            @schedule (try result = wait(pfw); catch e; notify_error(wt, e); end; notify(wt))
            @schedule (sleep(timeout_s); notify(wt))

            wait(wt)
            if result === :timeout
                return (StatStruct(), StatStruct())
            end
            return result
        else
            return wait(pfw)
        end
    finally
        close(pfw)
    end
end
