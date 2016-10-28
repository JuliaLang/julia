# This file is a part of Julia. License is MIT: http://julialang.org/license

module Libc

import Base: transcode

export FILE, getpid, gethostname, free, malloc, calloc, realloc,
    errno, strerror, flush_cstdio, systemsleep, time, transcode, ComputerTime

if is_windows()
    export GetLastError, FormatMessage
end

include(string(length(Core.ARGS)>=2?Core.ARGS[2]:"","errno_h.jl"))  # include($BUILDROOT/base/errno_h.jl)

## RawFD ##

# Wrapper for an OS file descriptor (on both Unix and Windows)
immutable RawFD
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
    immutable WindowsRawSocket
        handle::Ptr{Void}   # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows
    end
    Base.cconvert(::Type{Ptr{Void}}, fd::WindowsRawSocket) = fd.handle
    _get_osfhandle(fd::RawFD) = WindowsRawSocket(ccall(:_get_osfhandle,Ptr{Void},(Cint,),fd.fd))
    _get_osfhandle(fd::WindowsRawSocket) = fd
else
    _get_osfhandle(fd::RawFD) = fd
end

## FILE (not auto-finalized) ##

immutable FILE
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

"""
    ComputerTime

A type with two fields: whole `seconds` since epoch and additional
`microseconds`
"""
immutable ComputerTime
   seconds::Int64
   microseconds::Int64
end

"""
    now()

Return the `ComputerTime` of the current time in UTC.
"""
function now()
    tv = Ref{ComputerTime}()
    status = ccall(:jl_gettimeofday, Cint, (Ref{ComputerTime},), tv)
    status != 0 && error("unable to determine current time: ", status)
    return tv[]
end


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
    GetLastError() = ccall(:GetLastError,stdcall,UInt32,())

    function FormatMessage(e=GetLastError())
        const FORMAT_MESSAGE_ALLOCATE_BUFFER = UInt32(0x100)
        const FORMAT_MESSAGE_FROM_SYSTEM = UInt32(0x1000)
        const FORMAT_MESSAGE_IGNORE_INSERTS = UInt32(0x200)
        const FORMAT_MESSAGE_MAX_WIDTH_MASK = UInt32(0xFF)
        lpMsgBuf = Array(Ptr{UInt16})
        lpMsgBuf[1] = 0
        len = ccall(:FormatMessageW,stdcall,UInt32,(Cint, Ptr{Void}, Cint, Cint, Ptr{Ptr{UInt16}}, Cint, Ptr{Void}),
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                    C_NULL, e, 0, lpMsgBuf, 0, C_NULL)
        p = lpMsgBuf[1]
        len == 0 && return ""
        buf = Array{UInt16}(len)
        unsafe_copy!(pointer(buf), p, len)
        ccall(:LocalFree,stdcall,Ptr{Void},(Ptr{Void},),p)
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
