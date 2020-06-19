# This file is a part of Julia. License is MIT: https://julialang.org/license

module Libc
@doc """
Interface to libc, the C standard library.
""" Libc

import Base: transcode, windowserror
import Core.Intrinsics: bitcast

export FILE, TmStruct, strftime, strptime, getpid, gethostname, free, malloc, calloc, realloc,
    errno, strerror, flush_cstdio, systemsleep, time, transcode
if Sys.iswindows()
    export GetLastError, FormatMessage
end

include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "errno_h.jl"))  # include($BUILDROOT/base/errno_h.jl)

## RawFD ##

# Wrapper for an OS file descriptor (on both Unix and Windows)
"""
    RawFD

Primitive type which wraps the native OS file descriptor.
`RawFD`s can be passed to methods like [`stat`](@ref) to
discover information about the underlying file, and can
also be used to open streams, with the `RawFD` describing
the OS file backing the stream.
"""
primitive type RawFD 32 end
RawFD(fd::Integer) = bitcast(RawFD, Cint(fd))
RawFD(fd::RawFD) = fd
Base.cconvert(::Type{Cint}, fd::RawFD) = bitcast(Cint, fd)

dup(x::RawFD) = ccall((@static Sys.iswindows() ? :_dup : :dup), RawFD, (RawFD,), x)
dup(src::RawFD, target::RawFD) = systemerror("dup", -1 ==
    ccall((@static Sys.iswindows() ? :_dup2 : :dup2), Int32,
                (RawFD, RawFD), src, target))

show(io::IO, fd::RawFD) = print(io, "RawFD(", bitcast(UInt32, fd), ')')  # avoids invalidation via show_default

# Wrapper for an OS file descriptor (for Windows)
if Sys.iswindows()
    primitive type WindowsRawSocket sizeof(Ptr) * 8 end # On Windows file descriptors are HANDLE's and 64-bit on 64-bit Windows
    WindowsRawSocket(handle::Ptr{Cvoid}) = bitcast(WindowsRawSocket, handle)
    WindowsRawSocket(handle::WindowsRawSocket) = handle

    Base.cconvert(::Type{Ptr{Cvoid}}, fd::WindowsRawSocket) = bitcast(Ptr{Cvoid}, fd)
    _get_osfhandle(fd::RawFD) = ccall(:_get_osfhandle, WindowsRawSocket, (RawFD,), fd)
    _get_osfhandle(fd::WindowsRawSocket) = fd
    function dup(src::WindowsRawSocket)
        new_handle = Ref(WindowsRawSocket(Ptr{Cvoid}(-1)))
        my_process = ccall(:GetCurrentProcess, stdcall, Ptr{Cvoid}, ())
        DUPLICATE_SAME_ACCESS = 0x2
        status = ccall(:DuplicateHandle, stdcall, Int32,
            (Ptr{Cvoid}, WindowsRawSocket, Ptr{Cvoid}, Ptr{WindowsRawSocket}, UInt32, Int32, UInt32),
            my_process, src, my_process, new_handle, 0, false, DUPLICATE_SAME_ACCESS)
        windowserror("dup failed", status == 0)
        return new_handle[]
    end
    function dup(src::WindowsRawSocket, target::RawFD)
        fd = ccall(:_open_osfhandle, RawFD, (WindowsRawSocket, Int32), dup(src), 0)
        dup(fd, target)
        ccall(:_close, Int32, (RawFD,), fd)
        nothing
    end

else
    _get_osfhandle(fd::RawFD) = fd
end

## FILE (not auto-finalized) ##

struct FILE
    ptr::Ptr{Cvoid}
end

modestr(s::IO) = modestr(isreadable(s), iswritable(s))
modestr(r::Bool, w::Bool) = r ? (w ? "r+" : "r") : (w ? "w" : throw(ArgumentError("neither readable nor writable")))

function FILE(fd::RawFD, mode)
    FILEp = ccall((@static Sys.iswindows() ? :_fdopen : :fdopen), Ptr{Cvoid}, (Cint, Cstring), fd, mode)
    systemerror("fdopen", FILEp == C_NULL)
    FILE(FILEp)
end

function FILE(s::IO)
    f = FILE(dup(RawFD(fd(s))),modestr(s))
    seek(f, position(s))
    f
end

Base.unsafe_convert(T::Union{Type{Ptr{Cvoid}},Type{Ptr{FILE}}}, f::FILE) = convert(T, f.ptr)
Base.close(f::FILE) = systemerror("fclose", ccall(:fclose, Cint, (Ptr{Cvoid},), f.ptr) != 0)

function Base.seek(h::FILE, offset::Integer)
    systemerror("fseek", ccall(:fseek, Cint, (Ptr{Cvoid}, Clong, Cint),
                               h.ptr, offset, 0) != 0)
    h
end

Base.position(h::FILE) = ccall(:ftell, Clong, (Ptr{Cvoid},), h.ptr)

# flush C stdio output from external libraries

"""
    flush_cstdio()

Flushes the C `stdout` and `stderr` streams (which may have been written to by external C code).
"""
flush_cstdio() = ccall(:jl_flush_cstdio, Cvoid, ())

## time-related functions ##

# TODO: check for usleep errors?
if Sys.isunix()
    systemsleep(s::Real) = ccall(:usleep, Int32, (UInt32,), round(UInt32, s*1e6))
elseif Sys.iswindows()
    function systemsleep(s::Real)
        ccall(:Sleep, stdcall, Cvoid, (UInt32,), round(UInt32, s * 1e3))
        return Int32(0)
    end
else
    error("systemsleep undefined for this OS")
end
"""
    systemsleep(s::Real)

Suspends execution for `s` seconds.
This function does not yield to Julia's scheduler and therefore blocks
the Julia thread that it is running on for the duration of the sleep time.

See also: [`sleep`](@ref)
"""
systemsleep

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
# Use wcsftime instead of strftime to support different locales
function strftime(fmt::AbstractString, tm::TmStruct)
    wctimestr = Vector{Cwchar_t}(undef, 128)
    n = ccall(:wcsftime, Csize_t, (Ptr{Cwchar_t}, Csize_t, Cwstring, Ref{TmStruct}),
              wctimestr, length(wctimestr), fmt, tm)
    n == 0 && return ""
    return transcode(String, resize!(wctimestr, n))
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
        if !occursin(r"([^%]|^)%(a|A|j|w|Ow)", fmt)
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
    hn = Vector{UInt8}(undef, 256)
    err = @static if Sys.iswindows()
        ccall(:gethostname, stdcall, Int32, (Ptr{UInt8}, UInt32), hn, length(hn))
    else
        ccall(:gethostname, Int32, (Ptr{UInt8}, UInt), hn, length(hn))
    end
    systemerror("gethostname", err != 0)
    return GC.@preserve hn unsafe_string(pointer(hn))
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
errno(e::Integer) = ccall(:jl_set_errno, Cvoid, (Cint,), e)

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

    FormatMessage(e) = FormatMessage(UInt32(e))
    function FormatMessage(e::UInt32=GetLastError())
        FORMAT_MESSAGE_ALLOCATE_BUFFER = UInt32(0x100)
        FORMAT_MESSAGE_FROM_SYSTEM = UInt32(0x1000)
        FORMAT_MESSAGE_IGNORE_INSERTS = UInt32(0x200)
        FORMAT_MESSAGE_MAX_WIDTH_MASK = UInt32(0xFF)
        lpMsgBuf = Ref{Ptr{UInt16}}()
        lpMsgBuf[] = 0
        len = ccall(:FormatMessageW, stdcall, UInt32, (UInt32, Ptr{Cvoid}, UInt32, UInt32, Ptr{Ptr{UInt16}}, UInt32, Ptr{Cvoid}),
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                    C_NULL, e, 0, lpMsgBuf, 0, C_NULL)
        p = lpMsgBuf[]
        len == 0 && return ""
        buf = Vector{UInt16}(undef, len)
        GC.@preserve buf unsafe_copyto!(pointer(buf), p, len)
        ccall(:LocalFree, stdcall, Ptr{Cvoid}, (Ptr{Cvoid},), p)
        return transcode(String, buf)
    end
end

## Memory related ##

"""
    free(addr::Ptr)

Call `free` from the C standard library. Only use this on memory obtained from [`malloc`](@ref), not
on pointers retrieved from other C libraries. [`Ptr`](@ref) objects obtained from C libraries should
be freed by the free functions defined in that library, to avoid assertion failures if
multiple `libc` libraries exist on the system.
"""
free(p::Ptr) = ccall(:free, Cvoid, (Ptr{Cvoid},), p)

"""
    malloc(size::Integer) -> Ptr{Cvoid}

Call `malloc` from the C standard library.
"""
malloc(size::Integer) = ccall(:malloc, Ptr{Cvoid}, (Csize_t,), size)

"""
    realloc(addr::Ptr, size::Integer) -> Ptr{Cvoid}

Call `realloc` from the C standard library.

See warning in the documentation for [`free`](@ref) regarding only using this on memory originally
obtained from [`malloc`](@ref).
"""
realloc(p::Ptr, size::Integer) = ccall(:realloc, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), p, size)

"""
    calloc(num::Integer, size::Integer) -> Ptr{Cvoid}

Call `calloc` from the C standard library.
"""
calloc(num::Integer, size::Integer) = ccall(:calloc, Ptr{Cvoid}, (Csize_t, Csize_t), num, size)

free(p::Cstring) = free(convert(Ptr{UInt8}, p))
free(p::Cwstring) = free(convert(Ptr{Cwchar_t}, p))

## Random numbers ##

# To limit dependency on rand functionality implemented in the Random module,
# Libc.rand is used in file.jl, and could be used in error.jl (but it breaks a test)
"""
    rand([T::Type])

Interface to the C `rand()` function. If `T` is provided, generate a value of type `T`
by composing two calls to `rand()`. `T` can be `UInt32` or `Float64`.
"""
rand() = ccall(:rand, Cint, ())
@static if Sys.iswindows()
    # Windows RAND_MAX is 2^15-1
    rand(::Type{UInt32}) = ((rand() % UInt32) << 17) ⊻ ((rand() % UInt32) << 8) ⊻ (rand() % UInt32)
else
    # RAND_MAX is at least 2^15-1 in theory, but we assume 2^16-1
    # on non-Windows systems (in practice, it's 2^31-1)
    rand(::Type{UInt32}) = ((rand() % UInt32) << 16) ⊻ (rand() % UInt32)
end
rand(::Type{Float64}) = rand(UInt32) * 2.0^-32

"""
    srand([seed])

Interface to the C `srand(seed)` function.
"""
srand(seed=floor(Int, time()) % Cuint) = ccall(:srand, Cvoid, (Cuint,), seed)

end # module
