# This file is a part of Julia. License is MIT: http://julialang.org/license

module Libc

import Base: transcode

export FILE, TmStruct, strftime, strptime, getpid, gethostname, free, malloc, calloc, realloc,
    errno, strerror, flush_cstdio, systemsleep, time, transcode
if is_windows()
    export GetLastError, FormatMessage
end

include(string(length(Core.ARGS)>=2?Core.ARGS[2]:"","errno_h.jl"))  # include($BUILDROOT/base/errno_h.jl)

## RawFD ##

# Wrapper for an OS file descriptor (on both Unix and Windows)
struct RawFD
    fd::Int32
    RawFD(fd::Integer) = new(fd)
    RawFD(fd::RawFD) = fd
end

Base.cconvert(::Type{Int32}, fd::RawFD) = fd.fd

dup(x::RawFD) = RawFD(ccall((@static is_windows() ? :_dup : :dup), Int32, (Int32,), x.fd))
dup(src::RawFD, target::RawFD) = systemerror("dup", -1 ==
    ccall((@static is_windows() ? :_dup2 : :dup2), Int32,
    (Int32, Int32), src.fd, target.fd))

# Wrapper for an OS file descriptor (for Windows)
if is_windows()
    struct WindowsRawSocket
        handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows
    end
    Base.cconvert(::Type{Ptr{Void}}, fd::WindowsRawSocket) = fd.handle
    _get_osfhandle(fd::RawFD) = WindowsRawSocket(ccall(:_get_osfhandle,Ptr{Void},(Cint,),fd.fd))
    _get_osfhandle(fd::WindowsRawSocket) = fd
else
    _get_osfhandle(fd::RawFD) = fd
end

## FILE (not auto-finalized) ##

struct FILE
    ptr::Ptr{Void}
end

modestr(s::IO) = modestr(isreadable(s), iswritable(s))
modestr(r::Bool, w::Bool) = r ? (w ? "r+" : "r") : (w ? "w" : throw(ArgumentError("neither readable nor writable")))

function FILE(fd::RawFD, mode)
    FILEp = ccall((@static is_windows() ? :_fdopen : :fdopen), Ptr{Void}, (Cint, Cstring), fd, mode)
    systemerror("fdopen", FILEp == C_NULL)
    FILE(FILEp)
end

function FILE(s::IO)
    f = FILE(dup(RawFD(fd(s))),modestr(s))
    seek(f, position(s))
    f
end

Base.unsafe_convert(T::Union{Type{Ptr{Void}},Type{Ptr{FILE}}}, f::FILE) = convert(T, f.ptr)
Base.close(f::FILE) = systemerror("fclose", ccall(:fclose, Cint, (Ptr{Void},), f.ptr) != 0)
Base.convert(::Type{FILE}, s::IO) = FILE(s)

function Base.seek(h::FILE, offset::Integer)
    systemerror("fseek", ccall(:fseek, Cint, (Ptr{Void}, Clong, Cint),
                               h.ptr, offset, 0) != 0)
    h
end

Base.position(h::FILE) = ccall(:ftell, Clong, (Ptr{Void},), h.ptr)

# flush C stdio output from external libraries

"""
    flush_cstdio()

Flushes the C `stdout` and `stderr` streams (which may have been written to by external C code).
"""
flush_cstdio() = ccall(:jl_flush_cstdio, Void, ())

## time-related functions ##

# TODO: check for usleep errors?
if is_unix()
    systemsleep(s::Real) = ccall(:usleep, Int32, (UInt32,), round(UInt32, s*1e6))
elseif is_windows()
    function systemsleep(s::Real)
        ccall(:Sleep, stdcall, Void, (UInt32,), round(UInt32, s * 1e3))
        return Int32(0)
    end
else
    error("systemsleep undefined for this OS")
end

struct TimeVal
   sec::Int64
   usec::Int64
end

function TimeVal()
    tv = Ref{TimeVal}()
    status = ccall(:jl_gettimeofday, Cint, (Ref{TimeVal},), tv)
    status != 0 && error("unable to determine current time: ", status)
    return tv[]
end

"""
    TmStruct([seconds])

Convert a number of seconds since the epoch to broken-down format, with fields `sec`, `min`,
`hour`, `mday`, `month`, `year`, `wday`, `yday`, and `isdst`.
"""
mutable struct TmStruct
    sec::Int32
    min::Int32
    hour::Int32
    mday::Int32
    month::Int32
    year::Int32
    wday::Int32
    yday::Int32
    isdst::Int32
    # on some platforms the struct is 14 words, even though 9 are specified
    _10::Int32
    _11::Int32
    _12::Int32
    _13::Int32
    _14::Int32

    TmStruct(sec, min, hour, mday, month, year, wday, yday, isdst) =
        new(sec, min, hour, mday, month, year, wday, yday, isdst, 0,0,0,0,0)
    TmStruct() = new(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    function TmStruct(t::Real)
        t = floor(t)
        tm = TmStruct()
        # TODO: add support for UTC via gmtime_r()
        ccall(:localtime_r, Ptr{TmStruct}, (Ptr{Int}, Ptr{TmStruct}), &t, &tm)
        return tm
    end
end

"""
    strftime([format], time)

Convert time, given as a number of seconds since the epoch or a `TmStruct`, to a formatted
string using the given format. Supported formats are the same as those in the standard C
library.
"""
strftime(t) = strftime("%c", t)
strftime(fmt::AbstractString, t::Real) = strftime(fmt, TmStruct(t))
function strftime(fmt::AbstractString, tm::TmStruct)
    timestr = Array{UInt8}(128)
    n = ccall(:strftime, Int, (Ptr{UInt8}, Int, Cstring, Ptr{TmStruct}),
              timestr, length(timestr), fmt, &tm)
    if n == 0
        return ""
    end
    return String(timestr[1:n])
end

"""
    strptime([format], timestr)

Parse a formatted time string into a `TmStruct` giving the seconds, minute, hour, date, etc.
Supported formats are the same as those in the standard C library. On some platforms,
timezones will not be parsed correctly. If the result of this function will be passed to
`time` to convert it to seconds since the epoch, the `isdst` field should be filled in
manually. Setting it to `-1` will tell the C library to use the current system settings to
determine the timezone.
"""
strptime(timestr::AbstractString) = strptime("%c", timestr)
function strptime(fmt::AbstractString, timestr::AbstractString)
    tm = TmStruct()
    r = ccall(:strptime, Cstring, (Cstring, Cstring, Ptr{TmStruct}),
              timestr, fmt, &tm)
    # the following would tell mktime() that this is a local time, and that
    # it should try to guess the timezone. not sure if/how this should be
    # exposed in the API.
    # tm.isdst = -1
    if r == C_NULL
        # TODO: better error message
        throw(ArgumentError("invalid arguments"))
    end
    @static if is_apple()
        # if we didn't explicitly parse the weekday or year day, use mktime
        # to fill them in automatically.
        if !ismatch(r"([^%]|^)%(a|A|j|w|Ow)", fmt)
            ccall(:mktime, Int, (Ptr{TmStruct},), &tm)
        end
    end
    return tm
end

# system date in seconds

"""
    time(t::TmStruct)

Converts a `TmStruct` struct to a number of seconds since the epoch.
"""
time(tm::TmStruct) = Float64(ccall(:mktime, Int, (Ptr{TmStruct},), &tm))
time() = ccall(:jl_clock_now, Float64, ())

## process-related functions ##

"""
    getpid() -> Int32

Get Julia's process ID.
"""
getpid() = ccall(:jl_getpid, Int32, ())


# provides Julia functions to manipulate process resources provided by libc and
# as specified in getrlimit(2) and setrlimit(2)

@linux_only immutable Resources
    RLIMIT_CPU::Int # 0 cpu time per process
    RLIMIT_FSIZE::Int # 1 largest file size in bytes
    RLIMIT_DATA::Int # 2 data segment size in bytes
    RLIMIT_STACK::Int # 3 stack size in bytes
    RLIMIT_CORE::Int # 4 core file size in bytes
    RLIMIT_RSS::Int # 5 resident set size affects swapping processes that
                    # are exceeding their resident set size will be more
                    # likely to have physical memory taken from them
    RLIMIT_NPROC::Int # 6 number of processes
    RLIMIT_NOFILE::Int # 7 number of open files
    RLIMIT_MEMLOCK::Int # 8 locked-in-memory address space
    RLIMIT_AS::Int # 9 address space in bytes
    RLIMIT_LOCKS::Int # 10 Maximum number of file locks
    RLIMIT_SIGPENDING::Int # 11 Maximum number of pending signals
    RLIMIT_MSGQUEUE::Int # 12 Maximum bytes in POSIX message queues
    RLIMIT_NICE::Int # 13 Maximum nice priority allowed to raise to.
                     # Nice levels 19 .. -20 correspond to 0 .. 39
                     # values of this resource limit
    RLIMIT_RTPRIO::Int # 14 Maximum realtime priority allowed for
                        # non-priviledged processes
    RLIMIT_RTTIME::Int # 15 Maximum CPU time in µs that a process scheduled
                       # under a real-time scheduling policy may consume without
                       # making a blocking system call before being forcibly descheduled
    RLIM_NLIMITS::Int # 16 total number of resource limits
    Resources() = new(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
end

@osx_only immutable Resources
    RLIMIT_CPU::Int # 0 cpu time per process
    RLIMIT_FSIZE::Int # 1 file size
    RLIMIT_DATA::Int # 2 data segment size
    RLIMIT_STACK::Int # 3 stack size
    RLIMIT_CORE::Int # 4 core file size
    RLIMIT_AS::Int # 5 address space
    RLIMIT_RSS::Int # 5 resident set size
    RLIMIT_MEMLOCK::Int # 6 locked-in-memory address space
    RLIMIT_NPROC::Int # 7 number of processes
    RLIMIT_NOFILE::Int # 8 number of open files
    RLIM_NLIMITS::Int # 9 total number of resource limits
    Resources() = new(0,1,2,3,4,5,5,6,7,8,9)
end

type RLimit
    rlim_cur::Clong
    rlim_max::Clong
    RLimit() = new(0,0)
    RLimit(cur,max) = new(cur,max)
end

# perhaps an exception might be better
function verify_resource(resource::Int)
    if !in(resource,0:Resources().RLIM_NLIMITS)
        err = names(Resources) |>
              _->_[1:end-1] |>
              _->enumerate(_) |>
              _->[string(i[2]," = ",i[1] - 1) for i in _]
        error("Invalid resource specified: should be one of the following:\n$err")
    end
end

function getrlimit(resource::Int)
    verify_resource(resource)
    rlim = RLimit()
    rc = ccall(:getrlimit, Int, (Int, Ptr{RLimit}), resource, &rlim)
    if rc == 0
        rlim
    else
        error("failed with errorcode:",rc)
    end
end

function setrlimit(resource::Int,settings_tup::(Int,Int))
    verify_resource(resource)
    if !isa(settings_tup,(Int,Int))
        error("Expected a tuple of two integers (current,maximum)")
    end
    rlim = RLimit(settings_tup[1],settings_tup[2])
    rc = ccall(:setrlimit, Int, (Int, Ptr{RLimit}), resource, &rlim)
    if rc == 0
        rlim
    else
        error("failed with errorcode:",rc)
    end
end

@linux_only prlimit(pid::Int32, resource::Int) = prlimit(pid, resource, (nothing, nothing))
@linux_only function prlimit(pid::Int32, resource::Int, limits_tup)
    verify_resource(resource)
    rlim_old = RLimit()
    if isa(limits_tup,(Nothing,Nothing))
        rc = ccall(:prlimit, Int, (Int, Int, Ptr{RLimit}, Ptr{RLimit}),
                                   pid, resource, C_NULL, &rlim_old)
    elseif isa(limits_tup,(Int,Int))
        rlim_new = RLimit(limits_tup[1],limits_tup[2])
        rc = ccall(:prlimit, Int, (Int, Int, Ptr{RLimit}, Ptr{RLimit}),
                                   pid, resource, &rlim_new, &rlim_old)
    else
        error("Expected a tuple of two integers (current,maximum)")
    end

    if rc == 0
        (rlim_old.rlim_cur,rlim_old.rlim_max)
    else
        error("failed with errorcode:",rc)
    end
end

# override default show for RLimit type
import Base.show
show(io::IO, limit::RLimit) = print(io, string("Resource Limits(Current: ",
                                    limit.rlim_cur == typemax(Clong) ?
                                        "unlimited" : limit.rlim_cur,
                                    ", Maximum: ",
                                    limit.rlim_max == typemax(Clong) ?
                                        "unlimited" : limit.rlim_max,")"))


## network functions ##

"""
    gethostname() -> AbstractString

Get the local machine's host name.
"""
function gethostname()
    hn = Array{UInt8}(256)
    err = @static if is_windows()
        ccall(:gethostname, stdcall, Int32, (Ptr{UInt8}, UInt32), hn, length(hn))
    else
        ccall(:gethostname, Int32, (Ptr{UInt8}, UInt), hn, length(hn))
    end
    systemerror("gethostname", err != 0)
    return unsafe_string(pointer(hn))
end

## system error handling ##

"""
    errno([code])

Get the value of the C library's `errno`. If an argument is specified, it is used to set the
value of `errno`.

The value of `errno` is only valid immediately after a `ccall` to a C library routine that
sets it. Specifically, you cannot call `errno` at the next prompt in a REPL, because lots of
code is executed between prompts.
"""
errno() = ccall(:jl_errno, Cint, ())
errno(e::Integer) = ccall(:jl_set_errno, Void, (Cint,), e)

"""
    strerror(n=errno())

Convert a system call error code to a descriptive string
"""
strerror(e::Integer) = unsafe_string(ccall(:strerror, Cstring, (Int32,), e))
strerror() = strerror(errno())

"""
    GetLastError()

Call the Win32 `GetLastError` function [only available on Windows].
"""
function GetLastError end

"""
    FormatMessage(n=GetLastError())

Convert a Win32 system call error code to a descriptive string [only available on Windows].
"""
function FormatMessage end

if is_windows()
    GetLastError() = ccall(:GetLastError, stdcall, UInt32, ())

    function FormatMessage(e=GetLastError())
        const FORMAT_MESSAGE_ALLOCATE_BUFFER = UInt32(0x100)
        const FORMAT_MESSAGE_FROM_SYSTEM = UInt32(0x1000)
        const FORMAT_MESSAGE_IGNORE_INSERTS = UInt32(0x200)
        const FORMAT_MESSAGE_MAX_WIDTH_MASK = UInt32(0xFF)
        lpMsgBuf = Ref{Ptr{UInt16}}()
        lpMsgBuf[] = 0
        len = ccall(:FormatMessageW, stdcall, UInt32, (Cint, Ptr{Void}, Cint, Cint, Ptr{Ptr{UInt16}}, Cint, Ptr{Void}),
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                    C_NULL, e, 0, lpMsgBuf, 0, C_NULL)
        p = lpMsgBuf[]
        len == 0 && return ""
        buf = Array{UInt16}(len)
        unsafe_copy!(pointer(buf), p, len)
        ccall(:LocalFree, stdcall, Ptr{Void}, (Ptr{Void},), p)
        return transcode(String, buf)
    end
end

## Memory related ##

"""
    free(addr::Ptr)

Call `free` from the C standard library. Only use this on memory obtained from `malloc`, not
on pointers retrieved from other C libraries. `Ptr` objects obtained from C libraries should
be freed by the free functions defined in that library, to avoid assertion failures if
multiple `libc` libraries exist on the system.
"""
free(p::Ptr) = ccall(:free, Void, (Ptr{Void},), p)

"""
    malloc(size::Integer) -> Ptr{Void}

Call `malloc` from the C standard library.
"""
malloc(size::Integer) = ccall(:malloc, Ptr{Void}, (Csize_t,), size)

"""
    realloc(addr::Ptr, size::Integer) -> Ptr{Void}

Call `realloc` from the C standard library.

See warning in the documentation for `free` regarding only using this on memory originally
obtained from `malloc`.
"""
realloc(p::Ptr, size::Integer) = ccall(:realloc, Ptr{Void}, (Ptr{Void}, Csize_t), p, size)

"""
    calloc(num::Integer, size::Integer) -> Ptr{Void}

Call `calloc` from the C standard library.
"""
calloc(num::Integer, size::Integer) = ccall(:calloc, Ptr{Void}, (Csize_t, Csize_t), num, size)

free(p::Cstring) = free(convert(Ptr{UInt8}, p))
free(p::Cwstring) = free(convert(Ptr{Cwchar_t}, p))

end # module
