type FileMonitor
    handle::Ptr{Void}
    cb::Callback
    open::Bool
    notify::Condition
    function FileMonitor(cb, file)
        handle = c_malloc(_sizeof_uv_fs_event)
        err = ccall(:jl_fs_event_init,Int32, (Ptr{Void}, Ptr{Void}, Ptr{UInt8}, Int32), eventloop(),handle,file,0)
        if err < 0
            c_free(handle)
            throw(UVError("FileMonitor",err))
        end
        this = new(handle,cb,false,Condition())
        associate_julia_struct(handle,this)
        finalizer(this,uvfinalize)
        this
    end
    FileMonitor(file) = FileMonitor(false,file)
end

function close(t::FileMonitor)
    if t.handle != C_NULL
        ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)
    end
end

immutable FileEvent
    renamed::Bool
    changed::Bool
    timedout::Bool
end
# libuv file watching event flags
const UV_RENAME = 1
const UV_CHANGE = 2
const FE_TIMEDOUT = 4
FileEvent() = FileEvent(false,false,false)
FileEvent(flags::Integer) = FileEvent((flags & UV_RENAME) != 0,
                                      (flags & UV_CHANGE) != 0,
                                      (flags & FE_TIMEDOUT) != 0)

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
FDEvent() = FDEvent(false,false,false)
FDEvent(flags::Integer) = FDEvent((flags & UV_READABLE) != 0,
                                  (flags & UV_WRITABLE) != 0,
                                  (flags & FD_TIMEDOUT) != 0)
fdtimeout() = FDEvent(false,false,true)

#Wrapper for an OS file descriptor (for Windows)
@windows_only immutable WindowsRawSocket
    handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows...
end

@windows_only export WindowsRawSocket

abstract UVPollingWatcher

type PollingFileWatcher <: UVPollingWatcher
    handle::Ptr{Void}
    file::AbstractString
    open::Bool
    notify::Condition
    cb::Callback
    function PollingFileWatcher(cb, file)
        handle = c_malloc(_sizeof_uv_fs_poll)
        err = ccall(:uv_fs_poll_init,Int32,(Ptr{Void},Ptr{Void}),eventloop(),handle)
        if err < 0
            c_free(handle)
            throw(UVError("PollingFileWatcher",err))
        end
        this = new(handle, file, false, Condition(), cb)
        associate_julia_struct(handle,this)
        finalizer(this,uvfinalize)
        this
    end
    PollingFileWatcher(file) =  PollingFileWatcher(false,file)
end

@unix_only typealias FDW_FD RawFD
@windows_only typealias FDW_FD WindowsRawSocket

@unix_only _get_osfhandle(fd::RawFD) = fd
@windows_only _get_osfhandle(fd::RawFD) = WindowsRawSocket(ccall(:_get_osfhandle,Ptr{Void},(Cint,),fd.fd))
@windows_only _get_osfhandle(fd::WindowsRawSocket) = fd

type FDWatcher <: UVPollingWatcher
    handle::Ptr{Void}
    fd::FDW_FD
    open::Bool
    notify::Condition
    cb::Callback
    events::FDEvent
    FDWatcher(handle::Ptr,fd,open::Bool,notify::Condition,cb::Callback,events::FDEvent) =
        new(handle,_get_osfhandle(fd),open,notify,cb,events)
end
function FDWatcher(fd::RawFD)
    handle = c_malloc(_sizeof_uv_poll)
    @unix_only if ccall(:jl_uv_unix_fd_is_watched,Int32,(Int32,Ptr{Void},Ptr{Void}),fd.fd,handle,eventloop()) == 1
        c_free(handle)
        error("file descriptor $(fd.fd) is already being watched by another watcher")
    end
    err = ccall(:uv_poll_init,Int32,(Ptr{Void},Ptr{Void},Int32),eventloop(),handle,fd.fd)
    if err < 0
        c_free(handle)
        throw(UVError("FDWatcher",err))
    end
    this = FDWatcher(handle,fd,false,Condition(),false,FDEvent())
    associate_julia_struct(handle,this)
    finalizer(this,uvfinalize)
    this
end
@windows_only function FDWatcher(fd::WindowsRawSocket)
    handle = c_malloc(_sizeof_uv_poll)
    err = ccall(:uv_poll_init_socket,Int32,(Ptr{Void},   Ptr{Void}, Ptr{Void}),
                                            eventloop(), handle,    fd.handle)
    if err < 0
        c_free(handle)
        throw(UVError("FDWatcher",err))
    end
    this = FDWatcher(handle,fd,false,Condition(),false,FDEvent())
    associate_julia_struct(handle,this)
    finalizer(this,uvfinalize)
    this
end

function fdw_wait_cb(fdw::FDWatcher, events::FDEvent, status)
    if status == -1
        notify_error(fdw.notify,UVError("FDWatcher",status))
    else
        notify(fdw.notify,events)
    end
end

function _wait(fdw::FDWatcher,readable,writable)
    if !readable && !writable
        error("must watch for at least one event")
    end
    events = FDEvent(readable | fdw.events.readable,
                     writable | fdw.events.writable,
                     fdw.events.timedout)
    if !fdw.open || (events != fdw.events)
        # (re)initialize fdw
        start_watching(fdw_wait_cb,fdw,events)
    end
    while true
        events = wait(fdw.notify)
        if isa(events, FDEvent) &&
            ((readable && isreadable(events)) || (writable && iswritable(events)))
            break
        end
    end
    if isempty(fdw.notify.waitq)
        stop_watching(fdw)
    end
    events
end

# On Unix we can only have one watcher per FD, so we need to keep an explicit
# list of them. On Windows, I think it is techincally possible to have more than one
# watcher per FD, but in order to keep compatibility, we do the same on windows as we do
# on unix

let
    global fdwatcher_init, wait
    @unix_only begin
        local fdwatcher_array
        function fdwatcher_init()
            fdwatcher_array = Array(FDWatcher,0)
        end

        function wait(fd::RawFD; readable=false, writable=false)
            old_length = length(fdwatcher_array)
            if fd.fd+1 > old_length
                resize!(fdwatcher_array,fd.fd+1)
            end
            if !isdefined(fdwatcher_array,fd.fd+1)
                fdwatcher_array[fd.fd+1] = FDWatcher(fd)
            end
            _wait(fdwatcher_array[fd.fd+1],readable,writable)
        end
    end
    @windows_only begin
        local fdwatcher_array
        function fdwatcher_init()
            fdwatcher_array = Dict{WindowsRawSocket,FDWatcher}()
        end

        function wait(fd::RawFD; readable=false, writable=false)
            wait(_get_osfhandle(fd); readable=readable, writable=writable)
        end

        function wait(socket::WindowsRawSocket; readable=false, writable=false)
            if !haskey(fdwatcher_array,socket.handle)
                fdwatcher_array[socket] = FDWatcher(socket)
            end
            _wait(fdwatcher_array[socket],readable,writable)
        end
    end
end

function pfw_wait_cb(pfw::PollingFileWatcher, prev, cur, status)
    if status < 0
        notify_error(pfw.notify,UVError("PollingFileWatcher",status))
    else
        notify(pfw.notify,(prev,cur))
    end
end

function wait(pfw::PollingFileWatcher; interval=2.0)
    if !pfw.open
        start_watching(pfw_wait_cb,pfw,interval)
    end
    prev,curr = stream_wait(pfw,pfw.notify)
    if isempty(pfw.notify.waitq)
        stop_watching(pfw)
    end
    (prev,curr)
end

function wait(m::FileMonitor)
    err, filename, events = stream_wait(m,m.notify)
    filename, events
end


close(t::UVPollingWatcher) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

function start_watching(t::FDWatcher, events::FDEvent)
    associate_julia_struct(t.handle, t)
    @unix_only if ccall(:jl_uv_unix_fd_is_watched,Int32,(Int32,Ptr{Void},Ptr{Void}),t.fd,t.handle,eventloop()) == 1
        error("cannot watch an FD more than once on Unix")
    end
    uv_error("start_watching (FD)",
        ccall(:jl_poll_start, Int32, (Ptr{Void},Int32), t.handle,
              events.readable*UV_READABLE + events.writable*UV_WRITABLE + events.timedout*FD_TIMEDOUT))
end
start_watching(f::Function, t::FDWatcher, events::FDEvent) = (t.cb = f; start_watching(t,events))

function start_watching(t::PollingFileWatcher, interval)
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (File)",
             ccall(:jl_fs_poll_start, Int32, (Ptr{Void},Ptr{UInt8},UInt32),
                   t.handle, t.file, round(UInt32,interval*1000)))
end
start_watching(f::Function, t::PollingFileWatcher, interval) = (t.cb = f;start_watching(t,interval))

function stop_watching(t::FDWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (FD)",
        ccall(:uv_poll_stop,Int32,(Ptr{Void},),t.handle))
end

function stop_watching(t::PollingFileWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (File)",
        ccall(:uv_fs_poll_stop,Int32,(Ptr{Void},),t.handle))
end

function _uv_hook_fseventscb(t::FileMonitor,filename::Ptr,events::Int32,status::Int32)
    fname = filename == C_NULL ? "" : bytestring(convert(Ptr{UInt8},filename))
    fe = FileEvent(events)
    if isa(t.cb,Function)
        t.cb(fname, fe, status)
    end
    if status < 0
        notify_error(t.notify,(UVError("FileMonitor",status), fname, fe))
    else
        notify(t.notify,(status, fname, fe))
    end
end

function _uv_hook_pollcb(t::FDWatcher,status::Int32,events::Int32)
    if isa(t.cb,Function)
        t.cb(t, FDEvent(events), status)
    end
end

function _uv_hook_fspollcb(t::PollingFileWatcher,status::Int32,prev::Ptr,cur::Ptr)
    if isa(t.cb,Function)
        t.cb(t, StatStruct(convert(Ptr{UInt8},prev)), StatStruct(convert(Ptr{UInt8},cur)), status)
    end
end

_uv_hook_close(uv::FileMonitor) = (uv.handle = 0; nothing)
_uv_hook_close(uv::UVPollingWatcher) = (uv.handle = 0; nothing)

function poll_fd(s, seconds::Real; readable=false, writable=false)
    wt = Condition()

    @schedule (args = wait(s; readable=readable, writable=writable); notify(wt,(:poll,args)))
    @schedule (sleep(seconds); notify(wt,(:timeout,fdtimeout())))

    _, ret = wait(wt)

    return ret
end

function poll_file(s, interval_seconds::Real, seconds::Real)
    wt = Condition()
    pfw = PollingFileWatcher(s)

    @schedule (wait(pfw;interval=interval_seconds); notify(wt,(:poll)))
    @schedule (sleep(seconds); notify(wt,(:timeout)))

    result = wait(wt)
    if result == :timeout
        stop_watching(pfw)
    end
    result == :poll
end

watch_file(s; poll=false) = watch_file(false, s, poll=poll)
function watch_file(cb, s; poll=false)
    if poll
        pfw = PollingFileWatcher(cb,s)
        start_watching(pfw)
        return pfw
    else
        return FileMonitor(cb,s)
    end
end
