# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct Process <: AbstractPipe
    cmd::Cmd
    handle::Ptr{Cvoid}
    in::IO
    out::IO
    err::IO
    exitcode::Int64
    termsignal::Int32
    exitnotify::ThreadSynchronizer
    function Process(cmd::Cmd, handle::Ptr{Cvoid})
        this = new(cmd, handle, devnull, devnull, devnull,
                   typemin(fieldtype(Process, :exitcode)),
                   typemin(fieldtype(Process, :termsignal)),
                   ThreadSynchronizer())
        finalizer(uvfinalize, this)
        return this
    end
end
pipe_reader(p::Process) = p.out
pipe_writer(p::Process) = p.in

# Represents a whole pipeline of any number of related processes
# so the entire pipeline can be treated as one entity
mutable struct ProcessChain <: AbstractPipe
    processes::Vector{Process}
    in::IO
    out::IO
    err::IO
    function ProcessChain()
        return new(Process[], devnull, devnull, devnull)
    end
end
pipe_reader(p::ProcessChain) = p.out
pipe_writer(p::ProcessChain) = p.in

# release ownership of the libuv handle
function uvfinalize(proc::Process)
    if proc.handle != C_NULL
        disassociate_julia_struct(proc.handle)
        ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), proc.handle)
        proc.handle = C_NULL
    end
    nothing
end

# called when the process dies
function uv_return_spawn(p::Ptr{Cvoid}, exit_status::Int64, termsignal::Int32)
    data = ccall(:jl_uv_process_data, Ptr{Cvoid}, (Ptr{Cvoid},), p)
    data == C_NULL && return
    proc = unsafe_pointer_to_objref(data)::Process
    proc.exitcode = exit_status
    proc.termsignal = termsignal
    ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), proc.handle)
    proc.handle = C_NULL
    lock(proc.exitnotify)
    try
        notify(proc.exitnotify)
    finally
        unlock(proc.exitnotify)
    end
    nothing
end

# called when the libuv handle is destroyed
function _uv_hook_close(proc::Process)
    proc.handle = C_NULL
    nothing
end

const SpawnIOs = Vector{Any} # convenience name for readability

# handle marshalling of `Cmd` arguments from Julia to C
@noinline function _spawn_primitive(file, cmd::Cmd, stdio::SpawnIOs)
    loop = eventloop()
    iohandles = Tuple{Cint, UInt}[ # assuming little-endian layout
        let h = rawhandle(io)
            h === C_NULL     ? (0x00, UInt(0)) :
            h isa OS_HANDLE  ? (0x02, UInt(cconvert(@static(Sys.iswindows() ? Ptr{Cvoid} : Cint), h))) :
            h isa Ptr{Cvoid} ? (0x04, UInt(h)) :
            error("invalid spawn handle $h from $io")
        end
        for io in stdio]
    handle = Libc.malloc(_sizeof_uv_process)
    disassociate_julia_struct(handle) # ensure that data field is set to C_NULL
    err = ccall(:jl_spawn, Int32,
              (Cstring, Ptr{Cstring}, Ptr{Cvoid}, Ptr{Cvoid},
               Ptr{Tuple{Cint, UInt}}, Int,
               UInt32, Ptr{Cstring}, Cstring, Ptr{Cvoid}),
        file, cmd.exec, loop, handle,
        iohandles, length(iohandles),
        cmd.flags,
        cmd.env === nothing ? ["$k=$v" for (k, v) in ENV] : cmd.env,
        isempty(cmd.dir) ? C_NULL : cmd.dir,
        uv_jl_return_spawn::Ptr{Cvoid})
    if err != 0
        ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), handle) # will call free on handle eventually
        throw(_UVError("could not spawn " * repr(cmd), err))
    end
    pp = Process(cmd, handle)
    associate_julia_struct(handle, pp)
    return pp
end

_spawn(cmds::AbstractCmd) = _spawn(cmds, Any[])

# optimization: we can spawn `Cmd` directly without allocating the ProcessChain
function _spawn(cmd::Cmd, stdios::SpawnIOs)
    isempty(cmd.exec) && throw(ArgumentError("cannot spawn empty command"))
    pp = setup_stdios(stdios) do stdios
        return _spawn_primitive(cmd.exec[1], cmd, stdios)
    end
    return pp
end

# assume that having a ProcessChain means that the stdio are setup
function _spawn(cmds::AbstractCmd, stdios::SpawnIOs)
    pp = setup_stdios(stdios) do stdios
        return _spawn(cmds, stdios, ProcessChain())
    end
    return pp
end

# helper function for making a copy of a SpawnIOs, with replacement
function _stdio_copy(stdios::SpawnIOs, fd::Int, @nospecialize replace)
    nio = max(fd, length(stdios))
    new = SpawnIOs(undef, nio)
    copyto!(fill!(new, devnull), stdios)
    new[fd] = replace
    return new
end

function _spawn(redirect::CmdRedirect, stdios::SpawnIOs, args...)
    fdnum = redirect.stream_no + 1
    io, close_io = setup_stdio(redirect.handle, redirect.readable)
    try
        stdios = _stdio_copy(stdios, fdnum, io)
        return _spawn(redirect.cmd, stdios, args...)
    finally
        close_io && close_stdio(io)
    end
end

function _spawn(cmds::OrCmds, stdios::SpawnIOs, chain::ProcessChain)
    in_pipe, out_pipe = link_pipe(false, false)
    try
        stdios_left = _stdio_copy(stdios, 2, out_pipe)
        _spawn(cmds.a, stdios_left, chain)
        stdios_right = _stdio_copy(stdios, 1, in_pipe)
        _spawn(cmds.b, stdios_right, chain)
    finally
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
    end
    return chain
end

function _spawn(cmds::ErrOrCmds, stdios::SpawnIOs, chain::ProcessChain)
    in_pipe, out_pipe = link_pipe(false, false)
    try
        stdios_left = _stdio_copy(stdios, 3, out_pipe)
        _spawn(cmds.a, stdios_left, chain)
        stdios_right = _stdio_copy(stdios, 1, in_pipe)
        _spawn(cmds.b, stdios_right, chain)
    finally
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
    end
    return chain
end

function _spawn(cmds::AndCmds, stdios::SpawnIOs, chain::ProcessChain)
    _spawn(cmds.a, stdios, chain)
    _spawn(cmds.b, stdios, chain)
    return chain
end

function _spawn(cmd::Cmd, stdios::SpawnIOs, chain::ProcessChain)
    isempty(cmd.exec) && throw(ArgumentError("cannot spawn empty command"))
    pp = _spawn_primitive(cmd.exec[1], cmd, stdios)
    push!(chain.processes, pp)
    return chain
end


# open the child end of each element of `stdios`, and initialize the parent end
function setup_stdios(f, stdios::SpawnIOs)
    nstdio = length(stdios)
    open_io = Vector{Any}(undef, nstdio)
    close_io = falses(nstdio)
    try
        for i in 1:nstdio
            open_io[i], close_io[i] = setup_stdio(stdios[i], i == 1)
        end
        pp = f(open_io)
        return pp
    finally
        for i in 1:nstdio
            close_io[i] && close_stdio(open_io[i])
        end
    end
end

function setup_stdio(stdio::PipeEndpoint, child_readable::Bool)
    if stdio.status == StatusInit
        # if the PipeEndpoint isn't open, set it to the parent end
        # and pass the other end to the child
        rd, wr = link_pipe(!child_readable, child_readable)
        try
            open_pipe!(stdio, child_readable ? wr : rd)
        catch ex
            close_pipe_sync(rd)
            close_pipe_sync(wr)
            rethrow(ex)
        end
        child = child_readable ? rd : wr
        return (child, true)
    end
    # if it's already open, assume that it's already the child end
    # (since we can't do anything else)
    return (stdio, false)
end

function setup_stdio(stdio::Pipe, child_readable::Bool)
    if stdio.in.status == StatusInit && stdio.out.status == StatusInit
        link_pipe!(stdio)
    end
    io = child_readable ? stdio.out : stdio.in
    return (io, false)
end

setup_stdio(stdio::AbstractPipe, readable::Bool) =
    setup_stdio(readable ? pipe_reader(stdio) : pipe_writer(stdio), readable)

function setup_stdio(stdio::IOStream, child_readable::Bool)
    io = RawFD(fd(stdio))
    return (io, false)
end

function setup_stdio(stdio::FileRedirect, child_readable::Bool)
    if child_readable
        attr = JL_O_RDONLY
        perm = zero(S_IRUSR)
    else
        attr = JL_O_WRONLY | JL_O_CREAT
        attr |= stdio.append ? JL_O_APPEND : JL_O_TRUNC
        perm = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH
    end
    io = Filesystem.open(stdio.filename, attr, perm)
    return (io, true)
end

# incrementally move data between an IOBuffer and a system Pipe
# TODO: probably more efficient (when valid) to use `stdio` directly as the
#       PipeEndpoint buffer field in some cases
function setup_stdio(stdio::Union{IOBuffer, BufferStream}, child_readable::Bool)
    parent = PipeEndpoint()
    rd, wr = link_pipe(!child_readable, child_readable)
    try
        open_pipe!(parent, child_readable ? wr : rd)
    catch ex
        close_pipe_sync(rd)
        close_pipe_sync(wr)
        rethrow(ex)
    end
    child = child_readable ? rd : wr
    try
        let in = (child_readable ? parent : stdio),
            out = (child_readable ? stdio : parent)
            @async try
                write(in, out)
            catch ex
                @warn "Process error" exception=(ex, catch_backtrace())
            finally
                close(parent)
            end
        end
    catch ex
        close_pipe_sync(child)
        rethrow(ex)
    end
    return (child, true)
end

function setup_stdio(io, child_readable::Bool)
    # if there is no specialization,
    # assume that rawhandle is defined for it
    return (io, false)
end

close_stdio(stdio::OS_HANDLE) = close_pipe_sync(stdio)
close_stdio(stdio) = close(stdio)

# INTERNAL
# pad out stdio to have at least three elements,
# passing either `devnull` or the corresponding `stdio`
# A Redirectable can be any of:
#   - A system IO handle, to be passed to the child
#   - An uninitialized pipe, to be created
#   - devnull (to pass /dev/null for 0-2, or to leave undefined for fd > 2)
#   - An Filesystem.File or IOStream object to redirect the output to
#   - A FileRedirect, containing a string specifying a filename to be opened for the child

spawn_opts_swallow(stdios::StdIOSet) = Any[stdios...]
spawn_opts_inherit(stdios::StdIOSet) = Any[stdios...]
spawn_opts_swallow(in::Redirectable=devnull, out::Redirectable=devnull, err::Redirectable=devnull) =
    Any[in, out, err]
# pass original descriptors to child processes by default, because we might
# have already exhausted and closed the libuv object for our standard streams.
# ref issue #8529
spawn_opts_inherit(in::Redirectable=RawFD(0), out::Redirectable=RawFD(1), err::Redirectable=RawFD(2)) =
    Any[in, out, err]

function eachline(cmd::AbstractCmd; keep::Bool=false)
    out = PipeEndpoint()
    processes = _spawn(cmd, Any[devnull, out, stderr])
    # if the user consumes all the data, also check process exit status for success
    ondone = () -> (success(processes) || pipeline_error(processes); nothing)
    return EachLine(out, keep=keep, ondone=ondone)::EachLine
end

"""
    open(command, mode::AbstractString, stdio=devnull)

Run `command` asynchronously. Like `open(command, stdio; read, write)` except specifying
the read and write flags via a mode string instead of keyword arguments.
Possible mode strings are:

| Mode | Description | Keywords                         |
|:-----|:------------|:---------------------------------|
| `r`  | read        | none                             |
| `w`  | write       | `write = true`                   |
| `r+` | read, write | `read = true, write = true`      |
| `w+` | read, write | `read = true, write = true`      |
"""
function open(cmds::AbstractCmd, mode::AbstractString, stdio::Redirectable=devnull)
    if mode == "r+" || mode == "w+"
        return open(cmds, stdio, read = true, write = true)
    elseif mode == "r"
        return open(cmds, stdio)
    elseif mode == "w"
        return open(cmds, stdio, write = true)
    else
        throw(ArgumentError("mode must be \"r\", \"w\", \"r+\", or \"w+\", not $(repr(mode))"))
    end
end

# return a Process object to read-to/write-from the pipeline
"""
    open(command, stdio=devnull; write::Bool = false, read::Bool = !write)

Start running `command` asynchronously, and return a `process::IO` object.  If `read` is
true, then reads from the process come from the process's standard output and `stdio` optionally
specifies the process's standard input stream.  If `write` is true, then writes go to
the process's standard input and `stdio` optionally specifies the process's standard output
stream.
The process's standard error stream is connected to the current global `stderr`.
"""
function open(cmds::AbstractCmd, stdio::Redirectable=devnull; write::Bool=false, read::Bool=!write)
    if read && write
        stdio === devnull || throw(ArgumentError("no stream can be specified for `stdio` in read-write mode"))
        in = PipeEndpoint()
        out = PipeEndpoint()
        processes = _spawn(cmds, Any[in, out, stderr])
        processes.in = in
        processes.out = out
    elseif read
        out = PipeEndpoint()
        processes = _spawn(cmds, Any[stdio, out, stderr])
        processes.out = out
    elseif write
        in = PipeEndpoint()
        processes = _spawn(cmds, Any[in, stdio, stderr])
        processes.in = in
    else
        stdio === devnull || throw(ArgumentError("no stream can be specified for `stdio` in no-access mode"))
        processes = _spawn(cmds, Any[devnull, devnull, stderr])
    end
    return processes
end

"""
    open(f::Function, command, args...; kwargs...)

Similar to `open(command, args...; kwargs...)`, but calls `f(stream)` on the resulting process
stream, then closes the input stream and waits for the process to complete.
Returns the value returned by `f`.
"""
function open(f::Function, cmds::AbstractCmd, args...; kwargs...)
    P = open(cmds, args...; kwargs...)
    ret = try
        f(P)
    catch
        kill(P)
        rethrow()
    finally
        close(P.in)
    end
    success(P) || pipeline_error(P)
    return ret
end

"""
    read(command::Cmd)

Run `command` and return the resulting output as an array of bytes.
"""
function read(cmd::AbstractCmd)
    procs = open(cmd, "r", devnull)
    bytes = read(procs.out)
    success(procs) || pipeline_error(procs)
    return bytes
end

"""
    read(command::Cmd, String)

Run `command` and return the resulting output as a `String`.
"""
read(cmd::AbstractCmd, ::Type{String}) = String(read(cmd))

"""
    run(command, args...; wait::Bool = true)

Run a command object, constructed with backticks (see the [Running External Programs](@ref)
section in the manual). Throws an error if anything goes wrong, including the process
exiting with a non-zero status (when `wait` is true).

If `wait` is false, the process runs asynchronously. You can later wait for it and check
its exit status by calling `success` on the returned process object.

When `wait` is false, the process' I/O streams are directed to `devnull`.
When `wait` is true, I/O streams are shared with the parent process.
Use [`pipeline`](@ref) to control I/O redirection.
"""
function run(cmds::AbstractCmd, args...; wait::Bool = true)
    if wait
        ps = _spawn(cmds, spawn_opts_inherit(args...))
        success(ps) || pipeline_error(ps)
    else
        stdios = spawn_opts_swallow(args...)
        ps = _spawn(cmds, stdios)
        # for each stdio input argument, guess whether the user
        # passed a `stdio` placeholder object as input, and thus
        # might be able to use the return AbstractProcess as an IO object
        # (this really only applies to PipeEndpoint, Pipe, TCPSocket, or an AbstractPipe wrapping one of those)
        if length(stdios) > 0
            in = stdios[1]
            isa(in, IO) && (ps.in = in)
            if length(stdios) > 1
                out = stdios[2]
                isa(out, IO) && (ps.out = out)
                if length(stdios) > 2
                    err = stdios[3]
                    isa(err, IO) && (ps.err = err)
                end
            end
        end
    end
    return ps
end

# some common signal numbers that are usually available on all platforms
# and might be useful as arguments to `kill` or testing against `Process.termsignal`
const SIGHUP  = 1
const SIGINT  = 2
const SIGQUIT = 3 # !windows
const SIGKILL = 9
const SIGPIPE = 13 # !windows
const SIGTERM = 15

function test_success(proc::Process)
    @assert process_exited(proc)
    if proc.exitcode < 0
        #TODO: this codepath is not currently tested
        throw(_UVError("could not start process " * repr(proc.cmd), proc.exitcode))
    end
    return proc.exitcode == 0 && (proc.termsignal == 0 || proc.termsignal == SIGPIPE)
end

function success(x::Process)
    wait(x)
    return test_success(x)
end
success(procs::Vector{Process}) = mapreduce(success, &, procs)
success(procs::ProcessChain) = success(procs.processes)

"""
    success(command)

Run a command object, constructed with backticks (see the [Running External Programs](@ref)
section in the manual), and tell whether it was successful (exited with a code of 0).
An exception is raised if the process cannot be started.
"""
success(cmd::AbstractCmd) = success(_spawn(cmd))


"""
    ProcessFailedException

Indicates problematic exit status of a process.
When running commands or pipelines, this is thrown to indicate
a nonzero exit code was returned (i.e. that the invoked process failed).
"""
struct ProcessFailedException <: Exception
    procs::Vector{Process}
end
ProcessFailedException(proc::Process) = ProcessFailedException([proc])

function showerror(io::IO, err::ProcessFailedException)
    if length(err.procs) == 1
        proc = err.procs[1]
        println(io, "failed process: ", proc, " [", proc.exitcode, "]")
    else
        println(io, "failed processes:")
        for proc in err.procs
            println(io, "  ", proc, " [", proc.exitcode, "]")
        end
    end
end

function pipeline_error(proc::Process)
    if !proc.cmd.ignorestatus
        throw(ProcessFailedException(proc))
    end
    nothing
end

function pipeline_error(procs::ProcessChain)
    failed = Process[]
    for p = procs.processes
        if !test_success(p) && !p.cmd.ignorestatus
            push!(failed, p)
        end
    end
    isempty(failed) && return nothing
    throw(ProcessFailedException(failed))
end

"""
    kill(p::Process, signum=Base.SIGTERM)

Send a signal to a process. The default is to terminate the process.
Returns successfully if the process has already exited, but throws an
error if killing the process failed for other reasons (e.g. insufficient
permissions).
"""
function kill(p::Process, signum::Integer)
    iolock_begin()
    if process_running(p)
        @assert p.handle != C_NULL
        err = ccall(:uv_process_kill, Int32, (Ptr{Cvoid}, Int32), p.handle, signum)
        if err != 0 && err != UV_ESRCH
            throw(_UVError("kill", err))
        end
    end
    iolock_end()
    nothing
end
kill(ps::Vector{Process}) = foreach(kill, ps)
kill(ps::ProcessChain) = foreach(kill, ps.processes)
kill(p::Process) = kill(p, SIGTERM)

"""
    getpid(process) -> Int32

Get the child process ID, if it still exists.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
function Libc.getpid(p::Process)
    # TODO: due to threading, this method is no longer synchronized with the user application
    iolock_begin()
    ppid = Int32(0)
    if p.handle != C_NULL
        ppid = ccall(:jl_uv_process_pid, Int32, (Ptr{Cvoid},), p.handle)
    end
    iolock_end()
    ppid <= 0 && throw(_UVError("getpid", UV_ESRCH))
    return ppid
end

## process status ##

"""
    process_running(p::Process)

Determine whether a process is currently running.
"""
process_running(s::Process) = s.handle != C_NULL
process_running(s::Vector{Process}) = any(process_running, s)
process_running(s::ProcessChain) = process_running(s.processes)

"""
    process_exited(p::Process)

Determine whether a process has exited.
"""
process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(process_exited, s)
process_exited(s::ProcessChain) = process_exited(s.processes)

process_signaled(s::Process) = (s.termsignal > 0)

function process_status(s::Process)
    return process_running(s) ? "ProcessRunning" :
           process_signaled(s) ? "ProcessSignaled(" * string(s.termsignal) * ")" :
           process_exited(s) ? "ProcessExited(" * string(s.exitcode) * ")" :
           error("process status error")
end

function wait(x::Process)
    process_exited(x) && return
    iolock_begin()
    if !process_exited(x)
        preserve_handle(x)
        lock(x.exitnotify)
        iolock_end()
        try
            wait(x.exitnotify)
        finally
            unlock(x.exitnotify)
            unpreserve_handle(x)
        end
    else
        iolock_end()
    end
    nothing
end

wait(x::ProcessChain) = foreach(wait, x.processes)

show(io::IO, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")

# allow the elements of the Cmd to be accessed as an array or iterator
for f in (:length, :firstindex, :lastindex, :keys, :first, :last, :iterate)
    @eval $f(cmd::Cmd) = $f(cmd.exec)
end
eltype(::Type{Cmd}) = eltype(fieldtype(Cmd, :exec))
for f in (:iterate, :getindex)
    @eval $f(cmd::Cmd, i) = $f(cmd.exec, i)
end
