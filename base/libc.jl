# This file is a part of Julia. License is MIT: https://julialang.org/license

module Libc
@doc """
Interface to libc, the C standard library.
""" -> Libc

import Base: transcode

export FILE, TmStruct, strftime, strptime, getpid, gethostname, free, malloc, calloc, realloc,
    errno, strerror, flush_cstdio, systemsleep, time, transcode
if Sys.iswindows()
    export GetLastError, FormatMessage
end

include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "errno_h.jl"))  # include($BUILDROOT/base/errno_h.jl)

## RawFD ##

# Wrapper for an OS file descriptor (on both Unix and Windows)
struct RawFD
    fd::Int32
    RawFD(fd::Integer) = new(fd)
    RawFD(fd::RawFD) = fd
end

Base.cconvert(::Type{Int32}, fd::RawFD) = fd.fd

dup(x::RawFD) = RawFD(ccall((@static Sys.iswindows() ? :_dup : :dup), Int32, (Int32,), x.fd))
dup(src::RawFD, target::RawFD) = systemerror("dup", -1 ==
    ccall((@static Sys.iswindows() ? :_dup2 : :dup2), Int32,
                (Int32, Int32), src.fd, target.fd))

# Wrapper for an OS file descriptor (for Windows)
if Sys.iswindows()
    struct WindowsRawSocket
        handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows
    end
    Base.cconvert(::Type{Ptr{Void}}, fd::WindowsRawSocket) = fd.handle
    _get_osfhandle(fd::RawFD) = WindowsRawSocket(ccall(:_get_osfhandle, Ptr{Void}, (Cint,), fd.fd))
    _get_osfhandle(fd::WindowsRawSocket) = fd
    function dup(src::WindowsRawSocket)
        new_handle = Ref{Ptr{Void}}(-1)
        my_process = ccall(:GetCurrentProcess, stdcall, Ptr{Void}, ())
        DUPLICATE_SAME_ACCESS = 0x2
        status = ccall(:DuplicateHandle, stdcall, Int32,
            (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Ptr{Void}}, UInt32, Int32, UInt32),
            my_process, src.handle, my_process, new_handle, 0, false, DUPLICATE_SAME_ACCESS)
        status == 0 && error("dup failed: $(FormatMessage())")
        return new_handle[]
    end
    function dup(src::WindowsRawSocket, target::RawFD)
        fd = ccall(:_open_osfhandle, Int32, (Ptr{Void}, Int32), dup(src), 0)
        dup(RawFD(fd), target)
        ccall(:_close, Int32, (Int32,), fd)
        nothing
    end

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
    FILEp = ccall((@static Sys.iswindows() ? :_fdopen : :fdopen), Ptr{Void}, (Cint, Cstring), fd, mode)
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
if Sys.isunix()
    systemsleep(s::Real) = ccall(:usleep, Int32, (UInt32,), round(UInt32, s*1e6))
elseif Sys.iswindows()
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
        ccall(:localtime_r, Ptr{TmStruct}, (Ref{Int}, Ref{TmStruct}), t, tm)
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
    timestr = Base.StringVector(128)
    n = ccall(:strftime, Int, (Ptr{UInt8}, Int, Cstring, Ref{TmStruct}),
              timestr, length(timestr), fmt, tm)
    n == 0 && return ""
    return String(resize!(timestr,n))
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
    r = ccall(:strptime, Cstring, (Cstring, Cstring, Ref{TmStruct}), timestr, fmt, tm)
    # the following would tell mktime() that this is a local time, and that
    # it should try to guess the timezone. not sure if/how this should be
    # exposed in the API.
    # tm.isdst = -1
    if r == C_NULL
        # TODO: better error message
        throw(ArgumentError("invalid arguments"))
    end
    @static if Sys.isapple()
        # if we didn't explicitly parse the weekday or year day, use mktime
        # to fill them in automatically.
        if !ismatch(r"([^%]|^)%(a|A|j|w|Ow)", fmt)
            ccall(:mktime, Int, (Ref{TmStruct},), tm)
        end
    end
    return tm
end

# system date in seconds

"""
    time(t::TmStruct)

Converts a `TmStruct` struct to a number of seconds since the epoch.
"""
time(tm::TmStruct) = Float64(ccall(:mktime, Int, (Ref{TmStruct},), tm))

"""
    time()

Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution.
"""
time() = ccall(:jl_clock_now, Float64, ())

## process-related functions ##

"""
    getpid() -> Int32

Get Julia's process ID.
"""
getpid() = ccall(:jl_getpid, Int32, ())

## network functions ##

"""
    gethostname() -> AbstractString

Get the local machine's host name.
"""
function gethostname()
    hn = Vector{UInt8}(256)
    err = @static if Sys.iswindows()
        ccall(:gethostname, stdcall, Int32, (Ptr{UInt8}, UInt32), hn, length(hn))
    else
        ccall(:gethostname, Int32, (Ptr{UInt8}, UInt), hn, length(hn))
    end
    systemerror("gethostname", err != 0)
    return Base.@gc_preserve hn unsafe_string(pointer(hn))
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

if Sys.iswindows()
    GetLastError() = ccall(:GetLastError, stdcall, UInt32, ())

    function FormatMessage(e=GetLastError())
        FORMAT_MESSAGE_ALLOCATE_BUFFER = UInt32(0x100)
        FORMAT_MESSAGE_FROM_SYSTEM = UInt32(0x1000)
        FORMAT_MESSAGE_IGNORE_INSERTS = UInt32(0x200)
        FORMAT_MESSAGE_MAX_WIDTH_MASK = UInt32(0xFF)
        lpMsgBuf = Ref{Ptr{UInt16}}()
        lpMsgBuf[] = 0
        len = ccall(:FormatMessageW, stdcall, UInt32, (Cint, Ptr{Void}, Cint, Cint, Ptr{Ptr{UInt16}}, Cint, Ptr{Void}),
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                    C_NULL, e, 0, lpMsgBuf, 0, C_NULL)
        p = lpMsgBuf[]
        len == 0 && return ""
        buf = Vector{UInt16}(len)
        Base.@gc_preserve buf unsafe_copy!(pointer(buf), p, len)
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
