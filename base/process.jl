# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractCmd end

# libuv process option flags
const UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS = UInt8(1 << 2)
const UV_PROCESS_DETACHED = UInt8(1 << 3)
const UV_PROCESS_WINDOWS_HIDE = UInt8(1 << 4)

struct Cmd <: AbstractCmd
    exec::Vector{String}
    ignorestatus::Bool
    flags::UInt32 # libuv process flags
    env::Union{Array{String},Nothing}
    dir::String
    Cmd(exec::Vector{String}) =
        new(exec, false, 0x00, nothing, "")
    Cmd(cmd::Cmd, ignorestatus, flags, env, dir) =
        new(cmd.exec, ignorestatus, flags, env,
            dir === cmd.dir ? dir : cstr(dir))
    function Cmd(cmd::Cmd; ignorestatus::Bool=cmd.ignorestatus, env=cmd.env, dir::AbstractString=cmd.dir,
                 detach::Bool = 0 != cmd.flags & UV_PROCESS_DETACHED,
                 windows_verbatim::Bool = 0 != cmd.flags & UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS,
                 windows_hide::Bool = 0 != cmd.flags & UV_PROCESS_WINDOWS_HIDE)
        flags = detach * UV_PROCESS_DETACHED |
                windows_verbatim * UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS |
                windows_hide * UV_PROCESS_WINDOWS_HIDE
        new(cmd.exec, ignorestatus, flags, byteenv(env),
            dir === cmd.dir ? dir : cstr(dir))
    end
end

has_nondefault_cmd_flags(c::Cmd) =
    c.ignorestatus ||
    c.flags != 0x00 ||
    c.env !== nothing ||
    c.dir !== ""

"""
    Cmd(cmd::Cmd; ignorestatus, detach, windows_verbatim, windows_hide, env, dir)

Construct a new `Cmd` object, representing an external program and arguments, from `cmd`,
while changing the settings of the optional keyword arguments:

* `ignorestatus::Bool`: If `true` (defaults to `false`), then the `Cmd` will not throw an
  error if the return code is nonzero.
* `detach::Bool`: If `true` (defaults to `false`), then the `Cmd` will be run in a new
  process group, allowing it to outlive the `julia` process and not have Ctrl-C passed to
  it.
* `windows_verbatim::Bool`: If `true` (defaults to `false`), then on Windows the `Cmd` will
  send a command-line string to the process with no quoting or escaping of arguments, even
  arguments containing spaces. (On Windows, arguments are sent to a program as a single
  "command-line" string, and programs are responsible for parsing it into arguments. By
  default, empty arguments and arguments with spaces or tabs are quoted with double quotes
  `"` in the command line, and `\\` or `"` are preceded by backslashes.
  `windows_verbatim=true` is useful for launching programs that parse their command line in
  nonstandard ways.) Has no effect on non-Windows systems.
* `windows_hide::Bool`: If `true` (defaults to `false`), then on Windows no new console
  window is displayed when the `Cmd` is executed. This has no effect if a console is
  already open or on non-Windows systems.
* `env`: Set environment variables to use when running the `Cmd`. `env` is either a
  dictionary mapping strings to strings, an array of strings of the form `"var=val"`, an
  array or tuple of `"var"=>val` pairs, or `nothing`. In order to modify (rather than
  replace) the existing environment, create `env` by `copy(ENV)` and then set
  `env["var"]=val` as desired.
* `dir::AbstractString`: Specify a working directory for the command (instead
  of the current directory).

For any keywords that are not specified, the current settings from `cmd` are used. Normally,
to create a `Cmd` object in the first place, one uses backticks, e.g.

    Cmd(`echo "Hello world"`, ignorestatus=true, detach=false)
"""
Cmd

hash(x::Cmd, h::UInt) = hash(x.exec, hash(x.env, hash(x.ignorestatus, hash(x.dir, hash(x.flags, h)))))
==(x::Cmd, y::Cmd) = x.exec == y.exec && x.env == y.env && x.ignorestatus == y.ignorestatus &&
                     x.dir == y.dir && isequal(x.flags, y.flags)

struct OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

struct ErrOrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    ErrOrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

struct AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

hash(x::AndCmds, h::UInt) = hash(x.a, hash(x.b, h))
==(x::AndCmds, y::AndCmds) = x.a == y.a && x.b == y.b

shell_escape(cmd::Cmd; special::AbstractString="") =
    shell_escape(cmd.exec..., special=special)
shell_escape_posixly(cmd::Cmd) =
    shell_escape_posixly(cmd.exec...)

function show(io::IO, cmd::Cmd)
    print_env = cmd.env !== nothing
    print_dir = !isempty(cmd.dir)
    (print_env || print_dir) && print(io, "setenv(")
    print(io, '`')
    join(io, map(cmd.exec) do arg
        replace(sprint(context=io) do io
            with_output_color(:underline, io) do io
                print_shell_word(io, arg, shell_special)
            end
        end, '`' => "\\`")
    end, ' ')
    print(io, '`')
    print_env && (print(io, ","); show(io, cmd.env))
    print_dir && (print(io, "; dir="); show(io, cmd.dir))
    (print_dir || print_env) && print(io, ")")
    nothing
end

function show(io::IO, cmds::Union{OrCmds,ErrOrCmds})
    print(io, "pipeline(")
    show(io, cmds.a)
    print(io, ", ")
    print(io, isa(cmds, ErrOrCmds) ? "stderr=" : "stdout=")
    show(io, cmds.b)
    print(io, ")")
end

function show(io::IO, cmds::AndCmds)
    show(io, cmds.a)
    print(io, " & ")
    show(io, cmds.b)
end

const STDIN_NO  = 0
const STDOUT_NO = 1
const STDERR_NO = 2

struct FileRedirect
    filename::String
    append::Bool
    FileRedirect(filename::AbstractString, append::Bool) = FileRedirect(convert(String, filename), append)
    function FileRedirect(filename::String, append::Bool)
        if lowercase(filename) == (@static Sys.iswindows() ? "nul" : "/dev/null")
            @warn "For portability use devnull instead of a file redirect" maxlog=1
        end
        return new(filename, append)
    end
end

# setup_stdio ≈ cconvert
# rawhandle ≈ unsafe_convert
rawhandle(::DevNull) = C_NULL
rawhandle(x::OS_HANDLE) = x
if OS_HANDLE !== RawFD
    rawhandle(x::RawFD) = Libc._get_osfhandle(x)
end

const Redirectable = Union{IO, FileRedirect, RawFD, OS_HANDLE}
const StdIOSet = NTuple{3, Redirectable}

struct CmdRedirect <: AbstractCmd
    cmd::AbstractCmd
    handle::Redirectable
    stream_no::Int
    readable::Bool
end
CmdRedirect(cmd, handle, stream_no) = CmdRedirect(cmd, handle, stream_no, stream_no == STDIN_NO)

function show(io::IO, cr::CmdRedirect)
    print(io, "pipeline(")
    show(io, cr.cmd)
    print(io, ", ")
    if cr.stream_no == STDOUT_NO
        print(io, "stdout")
    elseif cr.stream_no == STDERR_NO
        print(io, "stderr")
    elseif cr.stream_no == STDIN_NO
        print(io, "stdin")
    else
        print(io, cr.stream_no)
    end
    print(io, cr.readable ? "<" : ">")
    show(io, cr.handle)
    print(io, ")")
end

"""
    ignorestatus(command)

Mark a command object so that running it will not throw an error if the result code is non-zero.
"""
ignorestatus(cmd::Cmd) = Cmd(cmd, ignorestatus=true)
ignorestatus(cmd::Union{OrCmds,AndCmds}) =
    typeof(cmd)(ignorestatus(cmd.a), ignorestatus(cmd.b))

"""
    detach(command)

Mark a command object so that it will be run in a new process group, allowing it to outlive the julia process, and not have Ctrl-C interrupts passed to it.
"""
detach(cmd::Cmd) = Cmd(cmd; detach=true)

# like String(s), but throw an error if s contains NUL, since
# libuv requires NUL-terminated strings
function cstr(s)
    if Base.containsnul(s)
        throw(ArgumentError("strings containing NUL cannot be passed to spawned processes"))
    end
    return String(s)
end

# convert various env representations into an array of "key=val" strings
byteenv(env::AbstractArray{<:AbstractString}) =
    String[cstr(x) for x in env]
byteenv(env::AbstractDict) =
    String[cstr(string(k)*"="*string(v)) for (k,v) in env]
byteenv(env::Nothing) = nothing
byteenv(env::Union{AbstractVector{Pair{T}}, Tuple{Vararg{Pair{T}}}}) where {T<:AbstractString} =
    String[cstr(k*"="*string(v)) for (k,v) in env]

"""
    setenv(command::Cmd, env; dir="")

Set environment variables to use when running the given `command`. `env` is either a
dictionary mapping strings to strings, an array of strings of the form `"var=val"`, or zero
or more `"var"=>val` pair arguments. In order to modify (rather than replace) the existing
environment, create `env` by `copy(ENV)` and then setting `env["var"]=val` as desired, or
use `withenv`.

The `dir` keyword argument can be used to specify a working directory for the command.
"""
setenv(cmd::Cmd, env; dir="") = Cmd(cmd; env=byteenv(env), dir=dir)
setenv(cmd::Cmd, env::Pair{<:AbstractString}...; dir="") =
    setenv(cmd, env; dir=dir)
setenv(cmd::Cmd; dir="") = Cmd(cmd; dir=dir)

(&)(left::AbstractCmd, right::AbstractCmd) = AndCmds(left, right)
redir_out(src::AbstractCmd, dest::AbstractCmd) = OrCmds(src, dest)
redir_err(src::AbstractCmd, dest::AbstractCmd) = ErrOrCmds(src, dest)

# Stream Redirects
redir_out(dest::Redirectable, src::AbstractCmd) = CmdRedirect(src, dest, STDIN_NO)
redir_out(src::AbstractCmd, dest::Redirectable) = CmdRedirect(src, dest, STDOUT_NO)
redir_err(src::AbstractCmd, dest::Redirectable) = CmdRedirect(src, dest, STDERR_NO)

# File redirects
redir_out(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, false), STDOUT_NO)
redir_out(src::AbstractString, dest::AbstractCmd) = CmdRedirect(dest, FileRedirect(src, false), STDIN_NO)
redir_err(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, false), STDERR_NO)
redir_out_append(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, true), STDOUT_NO)
redir_err_append(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, true), STDERR_NO)

"""
    pipeline(command; stdin, stdout, stderr, append=false)

Redirect I/O to or from the given `command`. Keyword arguments specify which of the
command's streams should be redirected. `append` controls whether file output appends to the
file. This is a more general version of the 2-argument `pipeline` function.
`pipeline(from, to)` is equivalent to `pipeline(from, stdout=to)` when `from` is a command,
and to `pipeline(to, stdin=from)` when `from` is another kind of data source.

**Examples**:

```julia
run(pipeline(`dothings`, stdout="out.txt", stderr="errs.txt"))
run(pipeline(`update`, stdout="log.txt", append=true))
```
"""
function pipeline(cmd::AbstractCmd; stdin=nothing, stdout=nothing, stderr=nothing, append::Bool=false)
    if append && stdout === nothing && stderr === nothing
        throw(ArgumentError("append set to true, but no output redirections specified"))
    end
    if stdin !== nothing
        cmd = redir_out(stdin, cmd)
    end
    if stdout !== nothing
        cmd = append ? redir_out_append(cmd, stdout) : redir_out(cmd, stdout)
    end
    if stderr !== nothing
        cmd = append ? redir_err_append(cmd, stderr) : redir_err(cmd, stderr)
    end
    return cmd
end

pipeline(cmd::AbstractCmd, dest) = pipeline(cmd, stdout=dest)
pipeline(src::Union{Redirectable,AbstractString}, cmd::AbstractCmd) = pipeline(cmd, stdin=src)

"""
    pipeline(from, to, ...)

Create a pipeline from a data source to a destination. The source and destination can be
commands, I/O streams, strings, or results of other `pipeline` calls. At least one argument
must be a command. Strings refer to filenames. When called with more than two arguments,
they are chained together from left to right. For example, `pipeline(a,b,c)` is equivalent to
`pipeline(pipeline(a,b),c)`. This provides a more concise way to specify multi-stage
pipelines.

**Examples**:

```julia
run(pipeline(`ls`, `grep xyz`))
run(pipeline(`ls`, "out.txt"))
run(pipeline("out.txt", `grep xyz`))
```
"""
pipeline(a, b, c, d...) = pipeline(pipeline(a, b), c, d...)

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
    error = ccall(:jl_spawn, Int32,
              (Cstring, Ptr{Cstring}, Ptr{Cvoid}, Ptr{Cvoid},
               Ptr{Tuple{Cint, UInt}}, Int,
               UInt32, Ptr{Cstring}, Cstring, Ptr{Cvoid}),
        file, cmd.exec, loop, handle,
        iohandles, length(iohandles),
        cmd.flags,
        cmd.env === nothing ? C_NULL : cmd.env,
        isempty(cmd.dir) ? C_NULL : cmd.dir,
        uv_jl_return_spawn::Ptr{Cvoid})
    if error != 0
        ccall(:jl_forceclose_uv, Cvoid, (Ptr{Cvoid},), handle) # will call free on handle eventually
        throw(_UVError("could not spawn " * repr(cmd), error))
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
    open(f::Function, command, mode::AbstractString="r", stdio=devnull)

Similar to `open(command, mode, stdio)`, but calls `f(stream)` on the resulting process
stream, then closes the input stream and waits for the process to complete.
Returns the value returned by `f`.
"""
function open(f::Function, cmds::AbstractCmd, args...)
    P = open(cmds, args...)
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
    kill(p::Process, signum=SIGTERM)

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

## implementation of `cmd` syntax ##

arg_gen() = String[]
arg_gen(x::AbstractString) = String[cstr(x)]
function arg_gen(cmd::Cmd)
    if has_nondefault_cmd_flags(cmd)
        throw(ArgumentError("Non-default environment behavior is only permitted for the first interpolant."))
    end
    cmd.exec
end

function arg_gen(head)
    if isiterable(typeof(head))
        vals = String[]
        for x in head
            push!(vals, cstr(string(x)))
        end
        return vals
    else
        return String[cstr(string(head))]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = String[]
    for h = head, t = tail
        push!(vals, cstr(string(h,t)))
    end
    return vals
end

function cmd_gen(parsed)
    args = String[]
    if length(parsed) >= 1 && isa(parsed[1], Tuple{Cmd})
        cmd = parsed[1][1]
        (ignorestatus, flags, env, dir) = (cmd.ignorestatus, cmd.flags, cmd.env, cmd.dir)
        append!(args, cmd.exec)
        for arg in tail(parsed)
            append!(args, arg_gen(arg...))
        end
        return Cmd(Cmd(args), ignorestatus, flags, env, dir)
    else
        for arg in parsed
            append!(args, arg_gen(arg...))
        end
        return Cmd(args)
    end
end

"""
    @cmd str

Similar to `cmd`, generate a `Cmd` from the `str` string which represents the shell command(s) to be executed.
The [`Cmd`](@ref) object can be run as a process and can outlive the spawning julia process (see `Cmd` for more).

# Examples
```jldoctest
julia> cm = @cmd " echo 1 "
`echo 1`

julia> run(cm)
1
Process(`echo 1`, ProcessExited(0))
```
"""
macro cmd(str)
    return :(cmd_gen($(esc(shell_parse(str, special=shell_special)[1]))))
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
