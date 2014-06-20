type FileMonitor
    handle::Ptr{Void}
    cb::Callback
    open::Bool
    notify::Condition
    function FileMonitor(cb, file)
        handle = c_malloc(_sizeof_uv_fs_event)
        err = ccall(:jl_fs_event_init,Int32, (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32), eventloop(),handle,file,0)
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
    file::String
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

function wait(fdw::FDWatcher; readable=false,writable=false)
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

@unix_only begin
    function wait(fd::RawFD; readable=false, writable=false)
        fdw = FDWatcher(fd)
        try
            return wait(fdw,readable=readable,writable=writable)
        finally
            close(fdw)
        end
    end
end
@windows_only begin
    function wait(fd::RawFD; readable=false, writable=false)
        wait(_get_osfhandle(fd); readable=readable, writable=writable)
    end
    function wait(socket::WindowsRawSocket; readable=false, writable=false)
        fdw = FDWatcher(fd)
        try
            return wait(fdw,readable=readable,writable=writable)
        finally
            close(fdw)
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

function close(t::Union(FileMonitor,UVPollingWatcher))
    if t.handle != C_NULL
        ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)
        disassociate_julia_struct(t)
        t.handle = C_NULL
    end
end

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
             ccall(:jl_fs_poll_start, Int32, (Ptr{Void},Ptr{Uint8},Uint32),
                   t.handle, t.file, iround(interval*1000)))
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
    fname = filename == C_NULL ? "" : bytestring(convert(Ptr{Uint8},filename))
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
        t.cb(t, StatStruct(convert(Ptr{Uint8},prev)), StatStruct(convert(Ptr{Uint8},cur)), status)
    end
end

_uv_hook_close(uv::FileMonitor) = (uv.handle = 0; nothing)
_uv_hook_close(uv::UVPollingWatcher) = (uv.handle = 0; nothing)
function poll_fd(s, seconds::Real; readable=false, writable=false)
   result = wait(() -> wait(s; readable=readable, writable=writable), seconds)
   if !isa(result, FDEvent)
       return fdtimeout()
   end
   return result::FDEvent
end

function poll_file(s, interval_seconds::Real, seconds::Real)
    pfw = PollingFileWatcher(s)
    result = wait(() -> (wait(pfw;interval=interval_seconds); return :poll), seconds)
    result !== :poll && stop_watching(pfw)
    return result === :poll
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
