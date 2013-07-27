type FileMonitor
    handle::Ptr{Void}
    cb::Callback
    open::Bool
    notify::Condition
    function FileMonitor(cb, file)
        handle = c_malloc(_sizeof_uv_fs_event)
        err = ccall(:jl_fs_event_init,Int32, (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32), eventloop(),handle,file,0)
        if err == -1
            c_free(handle)
            throw(UVError("FileMonitor"))
        end
        this = new(handle,cb,false,Condition())
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this        
    end
    FileMonitor(file) = FileMonitor(false,file)
end

close(t::FileMonitor) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

const UV_READABLE = 1
const UV_WRITEABLE = 2

convert(::Type{Int32},fd::RawFD) = fd.fd 

#Wrapper for an OS file descriptor (for Windows)
@windows_only immutable WindowsRawSocket
    handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows...
end

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
        if err == -1
            c_free(handle)
            throw(UVError("PollingFileWatcher"))
        end
        this = new(handle, file, false, Condition(), cb)
        associate_julia_struct(handle,this)
        finalizer(this,close)
        this
    end  
    PollingFileWatcher(file) =  PollingFileWatcher(false,file)
end

@unix_only typealias FDW_FD RawFD 
@windows_only typealias FDW_FD WindowsRawSocket

@unix_only _get_osfhandle(fd::RawFD) = fd
@windows_only _get_osfhandle(fd::RawFD) = WindowsRawSocket(ccall(:_get_osfhandle,Ptr{Void},(Cint,),fd.fd))

type FDWatcher <: UVPollingWatcher
    handle::Ptr{Void}
    fd::FDW_FD
    open::Bool
    notify::Condition
    cb::Callback
    events::Int32
    FDWatcher(handle::Ptr,fd,open::Bool,notify::Condition,cb::Callback,events::Integer) =
        new(handle,_get_osfhandle(fd),open,notify,cb,int32(events))
end
function FDWatcher(fd::RawFD)
    handle = c_malloc(_sizeof_uv_poll)
    @unix_only if ccall(:jl_uv_unix_fd_is_watched,Int32,(Int32,Ptr{Void},Ptr{Void}),fd.fd,handle,eventloop()) == 1
        c_free(handle)
        error("FD is already being watched by another watcher")
    end
    err = ccall(:uv_poll_init,Int32,(Ptr{Void},Ptr{Void},Int32),eventloop(),handle,fd.fd)
    if err == -1
        c_free(handle)
        throw(UVError("FDWatcher"))
    end
    this = FDWatcher(handle,fd,false,Condition(),false,0)
    associate_julia_struct(handle,this)
    finalizer(this,close)
    this
end
@windows_only function FDWatcher(fd::WindowsRawSocket)
    handle = c_malloc(_sizeof_uv_poll)
    err = ccall(:uv_poll_init_socket,Int32,(Ptr{Void},   Ptr{Void}, Ptr{Void}),
                                            eventloop(), handle,    fd.handle)
    if err == -1
        c_free(handle)
        throw(UVError("FDWatcher"))
    end
    this = FDWatcher(handle,fd,false,Condition(),false,0)
    associate_julia_struct(handle,this)
    finalizer(this,close)
    this
end

function fdw_wait_cb(fdw::FDWatcher,status,events)
    if status == -1
        notify(fdw.notify,(UV_error_t(_uv_lasterror(),_uv_lastsystemerror()),events))
    else
        notify(fdw.notify,(UV_error_t(int32(0),int32(0)),events))
    end
end

function _wait(fdw::FDWatcher,readable,writeable)
    events = (readable ? UV_READABLE : 0) | 
             (writeable ? UV_WRITEABLE : 0)
    if events == 0
        error("Must be watching for at least one event")
    end
    events |= fdw.events
    if !fdw.open || (events != fdw.events)
        # (re)initialize fdw
        start_watching(fdw_wait_cb,fdw,events)
    end
    while true
        err, events = wait(fdw.notify)
        if err.uv_code != 0
            throw(UVError("wait (FD)",err))
        end
        if (readable && (events & UV_READABLE) != 0) ||
            (writeable && (events & UV_WRITEABLE) != 0)
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
    global fdwatcher_reinit
    const empty_watcher = FDWatcher(C_NULL,RawFD(-1),false,Condition(),false,0)
    @unix_only begin
        local fdwatcher_array = Array(FDWatcher,0)
        function fdwatcher_reinit()
            fdwatcher_array = Array(FDWatcher,0)
        end

        function wait(fd::RawFD; readable=false, writeable=false)
            old_length = length(fdwatcher_array)
            if fd.fd+1 > old_length
                resize!(fdwatcher_array,fd.fd+1)
                fdwatcher_array[old_length+1:fd.fd+1] = empty_watcher
            end
            if is(fdwatcher_array[fd.fd+1],empty_watcher)
                fdwatcher_array[fd.fd+1] = FDWatcher(fd)
            end
            _wait(fdwatcher_array[fd.fd+1],readable,writeable)
        end 
    end
    @windows_only begin
        local fdwatcher_array = Dict{WindowsRawSocket,FDWatcher}()
        function fdwatcher_reinit()
            fdwatcher_array = Dict{WindowsRawSocket,FDWatcher}()
        end

        function wait(fd::RawFD; readable=false, writeable=false)
            wait(_get_osfhandle(fd); readable=readable, writeable=writeable)
        end

        function wait(socket::WindowsRawSocket; readable=false, writeable=false)
            if !has(fdwatcher_array,socket.handle)
                fdwatcher_array[fd.handle] = FDWatcher(socket)
            end
            _wait(fdwatcher_array[fd.handle],readable,writeable)
        end 
    end
end

function pfw_wait_cb(pfw::PollingFileWatcher, status, prev, cur)
    if status == -1
        notify(pfw.notify,(UV_error_t(_uv_lasterror(),_uv_lastsystemerror()),prev,cur))
    else
        notify(pfw.notify,(UV_error_t(int32(0),int32(0)),prev,cur))
    end
end

function wait(pfw::PollingFileWatcher; interval=3.0)
    if !pfw.open
        start_watching(pfw_wait_cb,pfw,interval)
    end
    err,prev,curr = wait(pfw.notify)
    if err.uv_code != 0
        throw(UVError("wait (PollingFileWatcher)",err))
    end
    if isempty(pfw.notify.waitq)
        stop_watching(pfw)
    end
    (prev,curr)
end

function wait(m::FileMonitor)
    err, filename, events = wait(m.notify)
    if err.uv_code != 0
        throw(UVError("wait (FileMonitor)",err))
    end
    filename, events
end


close(t::UVPollingWatcher) = ccall(:jl_close_uv,Void,(Ptr{Void},),t.handle)

function start_watching(t::FDWatcher, events)
    associate_julia_struct(t.handle, t)
    @unix_only if ccall(:jl_uv_unix_fd_is_watched,Int32,(Int32,Ptr{Void},Ptr{Void}),t.fd,t.handle,eventloop()) == 1
        error("Cannot watch an FD more than once on Unix")
    end
    uv_error("start_watching (FD)",
        ccall(:jl_poll_start,Int32,(Ptr{Void},Int32),t.handle,events)==-1)
end
start_watching(f::Function, t::FDWatcher, events) = (t.cb = f; start_watching(t,events))

function start_watching(t::PollingFileWatcher, interval)
    associate_julia_struct(t.handle, t)
    uv_error("start_watching (File)",
        ccall(:jl_fs_poll_start,Int32,(Ptr{Void},Ptr{Uint8},Uint32),t.handle,t.file,interval)==-1)
end
start_watching(f::Function, t::PollingFileWatcher, interval) = (t.cb = f;start_watching(t,interval))

function stop_watching(t::FDWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (FD)",
        ccall(:uv_poll_stop,Int32,(Ptr{Void},),t.handle)==-1)
end

function stop_watching(t::PollingFileWatcher)
    disassociate_julia_struct(t.handle)
    uv_error("stop_watching (File)",
        ccall(:uv_fs_poll_stop,Int32,(Ptr{Void},),t.handle)==-1)
end

function _uv_hook_fseventscb(t::FileMonitor,filename::Ptr,events::Int32,status::Int32)
    if isa(t.cb,Function)
        # bytestring(convert(Ptr{Uint8},filename)) - seems broken at the moment - got NULL
        t.cb(status, events, status)
        if status == -1
            notify(t.notify,(UV_error_t(_uv_lasterror(),_uv_lastsystemerror()),bytestring(convert(Ptr{Uint8},filename)),events))
        else
            notify(t.notify,(UV_error_t(int32(0),int32(0)),bytestring(convert(Ptr{Uint8},filename)),events))
        end
    end
end

function _uv_hook_pollcb(t::FDWatcher,status::Int32,events::Int32)
    if isa(t.cb,Function)
        t.cb(t,status, events)
    end
end
function _uv_hook_fspollcb(t::PollingFileWatcher,status::Int32,prev::Ptr,cur::Ptr)
    if isa(t.cb,Function)
        t.cb(t, status, Stat(convert(Ptr{Uint8},prev)), Stat(convert(Ptr{Uint8},cur)))
    end
end

_uv_hook_close(uv::FileMonitor) = (uv.handle = 0; nothing)
_uv_hook_close(uv::UVPollingWatcher) = (uv.handle = 0; nothing)

function poll_fd(s, seconds::Real; readable=false, writeable=false)
    wt = Condition()

    @schedule (args = wait(s; readable=readable, writeable=writeable); notify(wt,(:poll,args)))
    @schedule (sleep(seconds); notify(wt,(:timeout,0)))

    _, ret = wait(wt)

    return ret
end

function poll_file(s, interval_seconds::Real, seconds::Real)
    wt = Condition()

    @schedule (wait(PollingFileWatcher(s);interval=interval_seconds); notify(wt,(:poll)))
    @schedule (sleep(seconds); notify(wt,(:timeout)))

    wait(wt) == :poll
end

function watch_file(cb, s; poll=false)
    if poll
        pfw = PollingFileWatcher(cb,s)
        start_watching(pfw)
        return pfw
    else 
        return FileMonitor(cb,s)
    end
end
