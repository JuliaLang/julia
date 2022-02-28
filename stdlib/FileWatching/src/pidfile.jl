module Pidfile


export mkpidlock

using Base:
    IOError, UV_EEXIST, UV_ESRCH,
    Process

using Base.Libc: rand

using Base.Filesystem:
    File, open, JL_O_CREAT, JL_O_RDWR, JL_O_RDONLY, JL_O_EXCL,
    rename, samefile, path_separator

using ..FileWatching: watch_file
using Base.Sys: iswindows

"""
    mkpidlock([f::Function], at::String, [pid::Cint, proc::Process]; kwopts...)

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
 - `stale_age`: Delete an existing pidfile (ignoring the lock) if its mtime is older than this.
     The file won't be deleted until 25x longer than this if the pid in the file appears that it may be valid.
     By default this is disabled (`stale_age` = 0), but a typical recommended value would be about 3-5x an
     estimated normal completion time.
 - `refresh`: Keeps a lock from becoming stale by updating the mtime every interval of time that passes.
     By default, this is set to `stale_age/2`, which is the recommended value.
 - `wait`: If true, block until we get the lock, if false, raise error if lock fails.
"""
function mkpidlock end


# mutable only because we want to add a finalizer
mutable struct LockMonitor
    const path::String
    const fd::File
    const update::Union{Nothing,Timer}

    global function mkpidlock(at::String, pid::Cint; stale_age::Real=0, refresh::Real=stale_age/2, kwopts...)
        local lock
        atdir, atname = splitdir(at)
        isempty(atdir) && (atdir = pwd())
        at = realpath(atdir) * path_separator * atname
        fd = open_exclusive(at; stale_age=stale_age, kwopts...)
        update = nothing
        try
            write_pidfile(fd, pid)
            if refresh > 0
                # N.b.: to ensure our finalizer works we are careful to capture
                # `fd` here instead of `lock`.
                update = Timer(t -> isopen(t) && touch(fd), refresh; interval=refresh)
            end
            lock = new(at, fd, update)
            finalizer(close, lock)
        catch ex
            rm(at)
            close(fd)
            rethrow(ex)
        end
        return lock
    end
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

function mkpidlock(at::String, proc::Process; kwopts...)
    lock = mkpidlock(at, getpid(proc); kwopts...)
    closer = @async begin
        wait(proc)
        close(lock)
    end
    isdefined(Base, :errormonitor) && Base.errormonitor(closer)
    return lock
end

"""
    Base.touch(::Pidfile.LockMonitor)

Update the `mtime` on the lock, to indicate it is still fresh.

See also the `refresh` keyword in the [`mkpidlock`](@ref) constructor.
"""
Base.touch(lock::LockMonitor) = (touch(lock.fd); lock)

"""
    write_pidfile(io, pid)

Write our pidfile format to an open IO descriptor.
"""
function write_pidfile(io::IO, pid::Cint)
    print(io, "$pid $(gethostname())")
end

"""
    parse_pidfile(file::Union{IO, String}) => (pid, hostname, age)

Attempt to parse our pidfile format,
replaced an element with (0, "", 0.0), respectively, for any read that failed.
"""
function parse_pidfile(io::IO)
    fields = split(read(io, String), ' ', limit = 2)
    pid = tryparse(Cuint, fields[1])
    pid === nothing && (pid = Cuint(0))
    hostname = (length(fields) == 2) ? fields[2] : ""
    when = mtime(io)
    age = time() - when
    return (pid, hostname, age)
end

function parse_pidfile(path::String)
    try
        existing = open(path, JL_O_RDONLY)
        try
            return parse_pidfile(existing)
        finally
            close(existing)
        end
    catch ex
        isa(ex, EOFError) || isa(ex, IOError) || rethrow(ex)
        return (Cuint(0), "", 0.0)
    end
end

"""
    isvalidpid(hostname::String, pid::Cuint) :: Bool

Attempt to conservatively estimate whether pid is a valid process id.
"""
function isvalidpid(hostname::AbstractString, pid::Cuint)
    # can't inspect remote hosts
    (hostname == "" || hostname == gethostname()) || return true
    # pid < 0 is never valid (must be a parser error or different OS),
    # and would have a completely different meaning when passed to kill
    !iswindows() && pid > typemax(Cint) && return false
    # (similarly for pid 0)
    pid == 0 && return false
    # see if the process id exists by querying kill without sending a signal
    # and checking if it returned ESRCH (no such process)
    return ccall(:uv_kill, Cint, (Cuint, Cint), pid, 0) != UV_ESRCH
end

"""
    stale_pidfile(path::String, stale_age::Real) :: Bool

Helper function for `open_exclusive` for deciding if a pidfile is stale.
"""
function stale_pidfile(path::String, stale_age::Real)
    pid, hostname, age = parse_pidfile(path)
    age < -stale_age && @warn "filesystem time skew detected" path=path
    if age > stale_age
        if (age > stale_age * 25) || !isvalidpid(hostname, pid)
            return true
        end
    end
    return false
end

"""
    tryopen_exclusive(path::String, mode::Integer = 0o444) :: Union{Void, File}

Try to create a new file for read-write advisory-exclusive access,
return nothing if it already exists.
"""
function tryopen_exclusive(path::String, mode::Integer = 0o444)
    try
        return open(path, JL_O_RDWR | JL_O_CREAT | JL_O_EXCL, mode)
    catch ex
        (isa(ex, IOError) && ex.code == UV_EEXIST) || rethrow(ex)
    end
    return nothing
end

"""
    open_exclusive(path::String; mode, poll_interval, stale_age) :: File

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
            error("Failed to get pidfile lock for $(repr(path)).")
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

function _rand_filename(len::Int=4) # modified from Base.Libc
    slug = Base.StringVector(len)
    chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for i = 1:len
        slug[i] = chars[(Libc.rand() % length(chars)) + 1]
    end
    return String(slug)
end

function tryrmopenfile(path::String)
    # Deleting open file on Windows is a bit hard
    # if we want to reuse the name immediately after:
    # we need to first rename it, then delete it.
    if Sys.iswindows()
        try
            local rmpath
            rmdir, rmname = splitdir(path)
            while true
                rmpath = string(rmdir, isempty(rmdir) ? "" : path_separator,
                    "\$", _rand_filename(), rmname, ".deleted")
                ispath(rmpath) || break
            end
            rename(path, rmpath)
            path = rmpath
        catch ex
            isa(ex, IOError) || rethrow(ex)
        end
    end
    return try
        rm(path)
        true
    catch ex
        isa(ex, IOError) || rethrow(ex)
        false
    end
end

"""
    close(lock::LockMonitor)

Release a pidfile lock.
"""
function Base.close(lock::LockMonitor)
    update = lock.update
    update === nothing || close(update)
    isopen(lock.fd) || return false
    removed = false
    path = lock.path
    pathstat = try
            # Windows sometimes likes to return EACCES here,
            # if the path is in the process of being deleted
            stat(path)
        catch ex
            ex isa IOError || rethrow()
            removed = ex
            nothing
        end
    if pathstat !== nothing && samefile(stat(lock.fd), pathstat)
        # try not to delete someone else's lock
        try
            rm(path)
            removed = true
        catch ex
            ex isa IOError || rethrow()
            removed = ex
        end
    end
    close(lock.fd)
    havelock = removed === true
    havelock || @warn "failed to remove pidfile on close" path=path removed=removed
    return havelock
end

end # module
