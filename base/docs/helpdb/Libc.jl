# This file is a part of Julia. License is MIT: http://julialang.org/license

# Libc

doc"""
    TmStruct([seconds])

Convert a number of seconds since the epoch to broken-down format, with fields `sec`, `min`, `hour`, `mday`, `month`, `year`, `wday`, `yday`, and `isdst`.
"""
Libc.TmStruct

doc"""
    dlext

File extension for dynamic libraries (e.g. dll, dylib, so) on the current platform.
"""
Libdl.dlext

doc"""
    time(t::TmStruct)

Converts a `TmStruct` struct to a number of seconds since the epoch.
"""
Libc.time

doc"""
    calloc(num::Integer, size::Integer) -> Ptr{Void}

Call `calloc` from the C standard library.
"""
Libc.calloc

doc"""
    strerror(n=errno())

Convert a system call error code to a descriptive string
"""
Libc.strerror

doc"""
    realloc(addr::Ptr, size::Integer) -> Ptr{Void}

Call `realloc` from the C standard library.

See warning in the documentation for `free` regarding only using this on memory originally obtained from `malloc`.
"""
Libc.realloc

doc"""
    free(addr::Ptr)

Call `free` from the C standard library. Only use this on memory obtained from `malloc`, not on pointers retrieved from other C libraries. `Ptr` objects obtained from C libraries should be freed by the free functions defined in that library, to avoid assertion failures if multiple `libc` libraries exist on the system.
"""
Libc.free

doc"""
    strftime([format], time)

Convert time, given as a number of seconds since the epoch or a `TmStruct`, to a formatted string using the given format. Supported formats are the same as those in the standard C library.
"""
Libc.strftime

doc"""
    errno([code])

Get the value of the C library's `errno`. If an argument is specified, it is used to set the value of `errno`.

The value of `errno` is only valid immediately after a `ccall` to a C library routine that sets it. Specifically, you cannot call `errno` at the next prompt in a REPL, because lots of code is executed between prompts.
"""
Libc.errno

doc"""
    malloc(size::Integer) -> Ptr{Void}

Call `malloc` from the C standard library.
"""
Libc.malloc

doc"""
    strptime([format], timestr)

Parse a formatted time string into a `TmStruct` giving the seconds, minute, hour, date, etc. Supported formats are the same as those in the standard C library. On some platforms, timezones will not be parsed correctly. If the result of this function will be passed to `time` to convert it to seconds since the epoch, the `isdst` field should be filled in manually. Setting it to `-1` will tell the C library to use the current system settings to determine the timezone.
"""
Libc.strptime

doc"""
    flush_cstdio()

Flushes the C `stdout` and `stderr` streams (which may have been written to by external C code).
"""
Libc.flush_cstdio

doc"""
```rst
..  msync(ptr, len, [flags])

Forces synchronization of the :func:`mmap`\ ped memory region from ``ptr`` to ``ptr+len``. Flags defaults to ``MS_SYNC``, but can be a combination of ``MS_ASYNC``, ``MS_SYNC``, or ``MS_INVALIDATE``. See your platform man page for specifics. The flags argument is not valid on Windows.

You may not need to call ``msync``, because synchronization is performed at intervals automatically by the operating system. However, you can call this directly if, for example, you are concerned about losing the result of a long-running calculation.
```
"""
Libc.msync

