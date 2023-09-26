# This file is a part of Julia. License is MIT: https://julialang.org/license

module Pidfile

import Base.Pidfile: trymkpidlock
export mkpidlock, trymkpidlock

"""
    mkpidlock([f::Function], at::String, [pid::Cint]; kwopts...)
    mkpidlock(at::String, proc::Process; kwopts...)

Create a pidfile lock for the path "at" for the current process
or the process identified by pid or proc. Can take a function to execute once locked,
for usage in `do` blocks, after which the lock will be automatically closed. If the lock fails
and `wait` is false, then an error is thrown.

The lock will be released by either `close`, a `finalizer`, or shortly after `proc` exits.
Make sure the return value is live through the end of the critical section of
your program, so the `finalizer` does not reclaim it early.

Optional keyword arguments:
 - `mode`: file access mode (modified by the process umask). Defaults to world-readable.
 - `poll_interval`: Specify the maximum time to between attempts (if `watch_file` doesn't work)
 - `stale_age`: Delete an existing pidfile (ignoring the lock) if it is older than this many seconds, based on its mtime.
     The file won't be deleted until 25x longer than this if the pid in the file appears that it may be valid.
     By default this is disabled (`stale_age` = 0), but a typical recommended value would be about 3-5x an
     estimated normal completion time.
 - `refresh`: Keeps a lock from becoming stale by updating the mtime every interval of time that passes.
     By default, this is set to `stale_age/2`, which is the recommended value.
 - `wait`: If true, block until we get the lock, if false, raise error if lock fails.
"""
function mkpidlock(at::String, pid::Cint; stale_age::Real=0, refresh::Real=stale_age/2, kwopts...)
    Base.Pidfile.mkpidlock(at, pid, open_exclusive; stale_age, refresh, kwopts...)
end

mkpidlock(at::String; kwopts...) = mkpidlock(at, getpid(); kwopts...)
mkpidlock(f::Function, at::String; kwopts...) = mkpidlock(f, at, getpid(); kwopts...)

function mkpidlock(f::Function, at::String, pid::Cint; kwopts...)
    lock = mkpidlock(at, pid; kwopts...)
    try
        return f()
    finally
        close(lock)
    end
end

function mkpidlock(at::String, proc::Base.Process; kwopts...)
    lock = mkpidlock(at, getpid(proc); kwopts...)
    closer = @async begin
        wait(proc)
        close(lock)
    end
    isdefined(Base, :errormonitor) && Base.errormonitor(closer)
    return lock
end

using ..FileWatching: watch_file
import Base.Pidfile: tryopen_exclusive, tryrmopenfile, tryopen_exclusive,
                     stale_pidfile, write_pidfile, parse_pidfile, stale_pidfile,
                     isvalidpid, LockMonitor

"""
    open_exclusive(path::String; mode, poll_interval, wait, stale_age) :: File

Create a new a file for read-write advisory-exclusive access.
If `wait` is `false` then error out if the lock files exist
otherwise block until we get the lock.

For a description of the keyword arguments, see [`mkpidlock`](@ref).
"""
function open_exclusive(path::String;
                        mode::Integer = 0o444 #= read-only =#,
                        poll_interval::Real = 10 #= seconds =#,
                        wait::Bool = true #= return on failure if false =#,
                        stale_age::Real = 0 #= disabled =#)
    # fast-path: just try to open it
    file = tryopen_exclusive(path, mode)
    file === nothing || return file
    if !wait
        if file === nothing && stale_age > 0
            if stale_age > 0 && stale_pidfile(path, stale_age)
                @warn "attempting to remove probably stale pidfile" path=path
                tryrmopenfile(path)
            end
            file = tryopen_exclusive(path, mode)
        end
        if file === nothing
            throw(PidlockedError("Failed to get pidfile lock for $(repr(path))."))
        else
            return file
        end
    end
    # fall-back: wait for the lock

    while true
        # start the file-watcher prior to checking for the pidfile existence
        t = @async try
            watch_file(path, poll_interval)
        catch ex
            isa(ex, IOError) || rethrow(ex)
            sleep(poll_interval) # if the watch failed, convert to just doing a sleep
        end
        # now try again to create it
        file = tryopen_exclusive(path, mode)
        file === nothing || return file
        Base.wait(t) # sleep for a bit before trying again
        if stale_age > 0 && stale_pidfile(path, stale_age)
            # if the file seems stale, try to remove it before attempting again
            # set stale_age to zero so we won't attempt again, even if the attempt fails
            stale_age -= stale_age
            @warn "attempting to remove probably stale pidfile" path=path
            tryrmopenfile(path)
        end
    end
end

end