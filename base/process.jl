# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractCmd

immutable Cmd <: AbstractCmd
    exec::Vector{ByteString}
    ignorestatus::Bool
    detach::Bool
    env::Union{Array{ByteString},Void}
    dir::UTF8String
    Cmd(exec::Vector{ByteString}) =
        new(exec, false, false, nothing, "")
    Cmd(cmd::Cmd, ignorestatus, detach, env, dir) =
        new(cmd.exec, ignorestatus, detach, env,
            dir === cmd.dir ? dir : cstr(dir))
    Cmd(cmd::Cmd; ignorestatus=cmd.ignorestatus, detach=cmd.detach, env=cmd.env, dir=cmd.dir) =
        new(cmd.exec, ignorestatus, detach, env,
            dir === cmd.dir ? dir : cstr(dir))
end

immutable OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

immutable ErrOrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    ErrOrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

immutable AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

shell_escape(cmd::Cmd) = shell_escape(cmd.exec...)

function show(io::IO, cmd::Cmd)
    print_env = cmd.env !== nothing
    print_dir = !isempty(cmd.dir)
    (print_env || print_dir) && print(io, "setenv(")
    esc = shell_escape(cmd)
    print(io, '`')
    for c in esc
        if c == '`'
            print(io, '\\')
        end
        print(io, c)
    end
    print(io, '`')
    print_env && (print(io, ","); show(io, cmd.env))
    print_dir && (print(io, "; dir="); show(io, cmd.dir))
    (print_dir || print_env) && print(io, ")")
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

immutable FileRedirect
    filename::AbstractString
    append::Bool
    function FileRedirect(filename, append)
        if lowercase(filename) == (@unix? "/dev/null" : "nul")
            warn_once("for portability use DevNull instead of a file redirect")
        end
        new(filename, append)
    end
end

immutable DevNullStream <: IO end
const DevNull = DevNullStream()
isreadable(::DevNullStream) = false
iswritable(::DevNullStream) = true
isopen(::DevNullStream) = true
read{T<:DevNullStream}(::T, args...) = throw(EOFErorr())
write{T<:DevNullStream}(::T, args...) = 0
close(::DevNullStream) = nothing
flush(::DevNullStream) = nothing
copy(::DevNullStream) = DevNull
wait_connected(::DevNullStream) = nothing
wait_readnb(::DevNullStream) = wait()
wait_readbyte(::DevNullStream) = wait()
wait_close(::DevNullStream) = wait()
eof(::DevNullStream) = true

uvhandle(::DevNullStream) = C_NULL
uvtype(::DevNullStream) = UV_STREAM

uvhandle(x::Ptr) = x
uvtype(::Ptr) = UV_STREAM

# Not actually a pointer, but that's how we pass it through the C API so it's fine
uvhandle(x::RawFD) = convert(Ptr{Void}, x.fd % UInt)
uvtype(x::RawFD) = UV_RAW_FD

typealias Redirectable Union{IO, FileRedirect, RawFD}
typealias StdIOSet NTuple{3, Union{Redirectable, Ptr{Void}}} # XXX: remove Ptr{Void} once libuv is refactored to use upstream release

immutable CmdRedirect <: AbstractCmd
    cmd::AbstractCmd
    handle::Redirectable
    stream_no::Int
end

function show(io::IO, cr::CmdRedirect)
    print(io, "pipeline(")
    show(io, cr.cmd)
    print(io, ", ")
    if cr.stream_no == STDOUT_NO
        print(io, "stdout=")
    elseif cr.stream_no == STDERR_NO
        print(io, "stderr=")
    elseif cr.stream_no == STDIN_NO
        print(io, "stdin=")
    end
    show(io, cr.handle)
    print(io, ")")
end


ignorestatus(cmd::Cmd) = Cmd(cmd, ignorestatus=true)
ignorestatus(cmd::Union{OrCmds,AndCmds}) =
    typeof(cmd)(ignorestatus(cmd.a), ignorestatus(cmd.b))
detach(cmd::Cmd) = Cmd(cmd, detach=true)

# like bytestring(s), but throw an error if s contains NUL, since
# libuv requires NUL-terminated strings
function cstr(s)
    if Base.containsnul(s)
        throw(ArgumentError("strings containing NUL cannot be passed to spawned processes"))
    end
    return bytestring(s)
end

function setenv{S<:ByteString}(cmd::Cmd, env::Array{S}; dir="")
    byteenv = ByteString[cstr(x) for x in env]
    return Cmd(cmd; env = byteenv, dir = dir)
end
function setenv(cmd::Cmd, env::Associative; dir="")
    byteenv = ByteString[cstr(string(k)*"="*string(v)) for (k,v) in env]
    return Cmd(cmd; env = byteenv, dir = dir)
end
function setenv{T<:AbstractString}(cmd::Cmd, env::Pair{T}...; dir="")
    byteenv = ByteString[cstr(k*"="*string(v)) for (k,v) in env]
    return Cmd(cmd; env = byteenv, dir = dir)
end
function setenv(cmd::Cmd; dir="")
    return Cmd(cmd; dir = dir)
end

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

function pipeline(cmd::AbstractCmd; stdin=nothing, stdout=nothing, stderr=nothing, append::Bool=false)
    if append && stdout === nothing && stderr === nothing
        error("append set to true, but no output redirections specified")
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

pipeline(a, b, c, d...) = pipeline(pipeline(a,b), c, d...)

type Process <: AbstractPipe
    cmd::Cmd
    handle::Ptr{Void}
    in::IO
    out::IO
    err::IO
    exitcode::Int64
    termsignal::Int32
    exitcb::Callback
    exitnotify::Condition
    closecb::Callback
    closenotify::Condition
    function Process(cmd::Cmd, handle::Ptr{Void},
                     in::Union{Redirectable, Ptr{Void}},
                     out::Union{Redirectable, Ptr{Void}},
                     err::Union{Redirectable, Ptr{Void}})
        if !isa(in, IO)
            in = DevNull
        end
        if !isa(out, IO)
            out = DevNull
        end
        if !isa(err, IO)
            err = DevNull
        end
        this = new(cmd, handle, in, out, err,
                   typemin(fieldtype(Process, :exitcode)),
                   typemin(fieldtype(Process, :termsignal)),
                   false, Condition(), false, Condition())
        finalizer(this, uvfinalize)
        this
    end
end

immutable ProcessChain <: AbstractPipe
    processes::Vector{Process}
    in::Redirectable
    out::Redirectable
    err::Redirectable
    ProcessChain(stdios::StdIOSet) = new(Process[], stdios[1], stdios[2], stdios[3])
end

function _jl_spawn(cmd, argv, loop::Ptr{Void}, pp::Process,
                   in, out, err)
    proc = Libc.malloc(_sizeof_uv_process)
    disassociate_julia_struct(proc)
    error = ccall(:jl_spawn, Int32,
        (Ptr{UInt8}, Ptr{Ptr{UInt8}}, Ptr{Void}, Ptr{Void}, Any, Int32,
         Ptr{Void}, Int32, Ptr{Void}, Int32, Ptr{Void}, Int32, Ptr{Ptr{UInt8}}, Ptr{UInt8}, Ptr{Void}),
        cmd, argv, loop, proc, pp, uvtype(in),
        uvhandle(in), uvtype(out), uvhandle(out), uvtype(err), uvhandle(err),
        pp.cmd.detach, pp.cmd.env === nothing ? C_NULL : pp.cmd.env, isempty(pp.cmd.dir) ? C_NULL : pp.cmd.dir,
        uv_jl_return_spawn::Ptr{Void})
    if error != 0
        ccall(:jl_forceclose_uv, Void, (Ptr{Void},), proc)
        throw(UVError("could not spawn "*string(pp.cmd), error))
    end
    associate_julia_struct(proc, pp)
    return proc
end

function uvfinalize(proc::Process)
    proc.handle != C_NULL && ccall(:jl_close_uv, Void, (Ptr{Void},), proc.handle)
    disassociate_julia_struct(proc)
    proc.handle = C_NULL
end

function uv_return_spawn(p::Ptr{Void}, exit_status::Int64, termsignal::Int32)
    data = ccall(:jl_uv_process_data, Ptr{Void}, (Ptr{Void},), p)
    data == C_NULL && return
    proc = unsafe_pointer_to_objref(data)::Process
    proc.exitcode = exit_status
    proc.termsignal = termsignal
    if isa(proc.exitcb, Function) proc.exitcb(proc, exit_status, termsignal) end
    ccall(:jl_close_uv, Void, (Ptr{Void},), proc.handle)
    notify(proc.exitnotify)
    nothing
end

function _uv_hook_close(proc::Process)
    proc.handle = C_NULL
    if isa(proc.closecb, Function) proc.closecb(proc) end
    notify(proc.closenotify)
end

function spawn(redirect::CmdRedirect, stdios::StdIOSet, exitcb::Callback, closecb::Callback; chain::Nullable{ProcessChain}=Nullable{ProcessChain}())
    spawn(redirect.cmd,
          (redirect.stream_no == STDIN_NO  ? redirect.handle : stdios[1],
           redirect.stream_no == STDOUT_NO ? redirect.handle : stdios[2],
           redirect.stream_no == STDERR_NO ? redirect.handle : stdios[3]),
          exitcb, closecb, chain=chain)
end

function spawn(cmds::OrCmds, stdios::StdIOSet, exitcb::Callback, closecb::Callback; chain::Nullable{ProcessChain}=Nullable{ProcessChain}())
    out_pipe = Libc.malloc(_sizeof_uv_named_pipe)
    in_pipe = Libc.malloc(_sizeof_uv_named_pipe)
    link_pipe(in_pipe, false, out_pipe, false)
    if isnull(chain)
        chain = Nullable(ProcessChain(stdios))
    end
    try
        spawn(cmds.a, (stdios[1], out_pipe, stdios[3]), exitcb, closecb, chain=chain)
        spawn(cmds.b, (in_pipe, stdios[2], stdios[3]), exitcb, closecb, chain=chain)
    finally
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
        Libc.free(out_pipe)
        Libc.free(in_pipe)
    end
    get(chain)
end

function spawn(cmds::ErrOrCmds, stdios::StdIOSet, exitcb::Callback, closecb::Callback; chain::Nullable{ProcessChain}=Nullable{ProcessChain}())
    out_pipe = Libc.malloc(_sizeof_uv_named_pipe)
    in_pipe = Libc.malloc(_sizeof_uv_named_pipe)
    link_pipe(in_pipe, false, out_pipe, false)
    if isnull(chain)
        chain = Nullable(ProcessChain(stdios))
    end
    try
        spawn(cmds.a, (stdios[1], stdios[2], out_pipe), exitcb, closecb, chain=chain)
        spawn(cmds.b, (in_pipe, stdios[2], stdios[3]), exitcb, closecb, chain=chain)
    finally
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
        Libc.free(out_pipe)
        Libc.free(in_pipe)
    end
    get(chain)
end

function setup_stdio(stdio::PipeEndpoint, readable::Bool)
    closeafter = false
    if stdio.handle == C_NULL
        io = Libc.malloc(_sizeof_uv_named_pipe)
        if readable
            link_pipe(io, false, stdio, true)
        else
            link_pipe(stdio, true, io, false)
        end
        closeafter = true
    else
        io = stdio.handle
    end
    return (io, closeafter)
end

function setup_stdio(stdio::Pipe, readable::Bool)
    if stdio.in.handle == C_NULL && stdio.out.handle == C_NULL
        link_pipe(stdio)
    end
    io = readable ? stdio.out : stdio.in
    return (io, false)
end

function setup_stdio(stdio::IOStream, readable::Bool)
    io = FS.File(RawFD(fd(stdio)))
    return (io, false)
end

function setup_stdio(stdio::FileRedirect, readable::Bool)
    if readable
        attr = JL_O_RDONLY
        perm = zero(S_IRUSR)
    else
        attr = JL_O_WRONLY | JL_O_CREAT
        attr |= stdio.append ? JL_O_APPEND : JL_O_TRUNC
        perm = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH
    end
    io = FS.open(stdio.filename, attr, perm)
    return (io, true)
end

function setup_stdio(io, readable::Bool)
    # if there is no specialization,
    # assume that uvhandle and uvtype are defined for it
    return io, false
end

function setup_stdio(stdio::Ptr{Void}, readable::Bool)
    return (stdio, false)
end

function close_stdio(stdio::Ptr{Void})
    close_pipe_sync(stdio)
    Libc.free(stdio)
end

function close_stdio(stdio)
    close(stdio)
end

function setup_stdio(anon::Function, stdio::StdIOSet)
    in, close_in = setup_stdio(stdio[1], true)
    out, close_out = setup_stdio(stdio[2], false)
    err, close_err = setup_stdio(stdio[3], false)
    anon(in, out, err)
    close_in  && close_stdio(in)
    close_out && close_stdio(out)
    close_err && close_stdio(err)
end

function spawn(cmd::Cmd, stdios::StdIOSet, exitcb::Callback, closecb::Callback; chain::Nullable{ProcessChain}=Nullable{ProcessChain}())
    loop = eventloop()
    pp = Process(cmd, C_NULL, stdios[1], stdios[2], stdios[3]);
    pp.exitcb = exitcb
    pp.closecb = closecb
    setup_stdio(stdios) do in, out, err
        pp.handle = _jl_spawn(cmd.exec[1], cmd.exec, loop, pp,
                              in, out, err)
    end
    if !isnull(chain)
        push!(get(chain).processes, pp)
    end
    pp
end

function spawn(cmds::AndCmds, stdios::StdIOSet, exitcb::Callback, closecb::Callback; chain::Nullable{ProcessChain}=Nullable{ProcessChain}())
    if isnull(chain)
        chain = Nullable(ProcessChain(stdios))
    end
    setup_stdio(stdios) do in, out, err
        spawn(cmds.a, (in,out,err), exitcb, closecb, chain=chain)
        spawn(cmds.b, (in,out,err), exitcb, closecb, chain=chain)
    end
    get(chain)
end

# INTERNAL
# returns a tuple of function arguments to spawn:
# (stdios, exitcb, closecb)
# |       |        \ The function to be called once the uv handle is closed
# |       \ The function to be called once the process exits
# \ A set of up to 256 stdio instructions, where each entry can be either:
#   | - An IO to be passed to the child
#   | - DevNull to pass /dev/null
#   | - An FS.File object to redirect the output to
#   \ - An ASCIIString specifying a filename to be opened

spawn_opts_swallow(stdios::StdIOSet, exitcb::Callback=false, closecb::Callback=false) =
    (stdios,exitcb,closecb)
spawn_opts_swallow(in::Redirectable=DevNull, out::Redirectable=DevNull, err::Redirectable=DevNull, args...) =
    (tuple(in,out,err,args...),false,false)
spawn_opts_inherit(stdios::StdIOSet, exitcb::Callback=false, closecb::Callback=false) =
    (stdios,exitcb,closecb)
# pass original descriptors to child processes by default, because we might
# have already exhausted and closed the libuv object for our standard streams.
# this caused issue #8529.
spawn_opts_inherit(in::Redirectable=RawFD(0), out::Redirectable=RawFD(1), err::Redirectable=RawFD(2), args...) =
    (tuple(in,out,err,args...),false,false)

spawn(cmds::AbstractCmd, args...; chain::Nullable{ProcessChain}=Nullable{ProcessChain}()) =
    spawn(cmds, spawn_opts_swallow(args...)...; chain=chain)
spawn(cmds::AbstractCmd, args...; chain::Nullable{ProcessChain}=Nullable{ProcessChain}()) =
    spawn(cmds, spawn_opts_swallow(args...)...; chain=chain)

function eachline(cmd::AbstractCmd, stdin)
    stdout = Pipe()
    processes = spawn(cmd, (stdin,stdout,STDERR))
    close(stdout.in)
    out = stdout.out
    # implicitly close after reading lines, since we opened
    return EachLine(out, ()->close(out))
end
eachline(cmd::AbstractCmd) = eachline(cmd, DevNull)

# return a Process object to read-to/write-from the pipeline
function open(cmds::AbstractCmd, mode::AbstractString="r", other::Redirectable=DevNull)
    if mode == "r"
        in = other
        out = io = Pipe()
        processes = spawn(cmds, (in,out,STDERR))
        close(out.in)
    elseif mode == "w"
        in = io = Pipe()
        out = other
        processes = spawn(cmds, (in,out,STDERR))
        close(in.out)
    else
        throw(ArgumentError("mode must be \"r\" or \"w\", not \"$mode\""))
    end
    return (io, processes)
end

function open(f::Function, cmds::AbstractCmd, args...)
    io, P = open(cmds, args...)
    ret = try
        f(io)
    catch
        kill(P)
        rethrow()
    finally
        close(io)
    end
    success(P) || pipeline_error(P)
    return ret
end

# TODO: deprecate this
function readandwrite(cmds::AbstractCmd)
    in = Pipe()
    out, processes = open(cmds, "r", in)
    (out, in, processes)
end

function readbytes(cmd::AbstractCmd, stdin::Redirectable=DevNull)
    out, procs = open(cmd, "r", stdin)
    bytes = readbytes(out)
    !success(procs) && pipeline_error(procs)
    return bytes
end

function readall(cmd::AbstractCmd, stdin::Redirectable=DevNull)
    return bytestring(readbytes(cmd, stdin))
end

function writeall(cmd::AbstractCmd, stdin::AbstractString, stdout::Redirectable=DevNull)
    open(cmd, "w", stdout) do io
        write(io, stdin)
    end
end

function run(cmds::AbstractCmd, args...)
    ps = spawn(cmds, spawn_opts_inherit(args...)...)
    success(ps) ? nothing : pipeline_error(ps)
end

const SIGPIPE = 13
function test_success(proc::Process)
    assert(process_exited(proc))
    if proc.exitcode < 0
        #TODO: this codepath is not currently tested
        throw(UVError("could not start process $(string(proc.cmd))", proc.exitcode))
    end
    proc.exitcode == 0 && (proc.termsignal == 0 || proc.termsignal == SIGPIPE)
end

function success(x::Process)
    wait(x)
    kill(x)
    test_success(x)
end
success(procs::Vector{Process}) = mapreduce(success, &, procs)
success(procs::ProcessChain) = success(procs.processes)
success(cmd::AbstractCmd) = success(spawn(cmd))

function pipeline_error(proc::Process)
    if !proc.cmd.ignorestatus
        error("failed process: ", proc, " [", proc.exitcode, "]")
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
    length(failed) == 0 && return nothing
    length(failed) == 1 && pipeline_error(failed[1])
    msg = "failed processes:"
    for proc in failed
        msg = string(msg, "\n  ", proc, " [", proc.exitcode, "]")
    end
    error(msg)
end

_jl_kill(p::Process, signum::Integer) = ccall(:uv_process_kill, Int32, (Ptr{Void},Int32), p.handle, signum)
function kill(p::Process, signum::Integer)
    if process_running(p)
        @assert p.handle != C_NULL
        _jl_kill(p, signum)
    else
        Int32(-1)
    end
end
kill(ps::Vector{Process}) = map(kill, ps)
kill(ps::ProcessChain) = map(kill, ps.processes)
kill(p::Process) = kill(p, 15) #SIGTERM

function _contains_newline(bufptr::Ptr{Void}, len::Int32)
    return (ccall(:memchr, Ptr{Void}, (Ptr{Void},Int32,Csize_t), bufptr, '\n', len) != C_NULL)
end

## process status ##
process_running(s::Process) = s.exitcode == typemin(fieldtype(Process, :exitcode))
process_running(s::Vector{Process}) = any(process_running, s)
process_running(s::ProcessChain) = process_running(s.processes)

process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(process_exited, s)
process_exited(s::ProcessChain) = process_exited(s.processes)

process_signaled(s::Process) = (s.termsignal > 0)

#process_stopped (s::Process) = false #not supported by libuv. Do we need this?
#process_stop_signal(s::Process) = false #not supported by libuv. Do we need this?

function process_status(s::Process)
    process_running(s) ? "ProcessRunning" :
    process_signaled(s) ? "ProcessSignaled("*string(s.termsignal)*")" :
    #process_stopped(s) ? "ProcessStopped("*string(process_stop_signal(s))*")" :
    process_exited(s) ? "ProcessExited("*string(s.exitcode)*")" :
    error("process status error")
end

## implementation of `cmd` syntax ##

arg_gen()          = ByteString[]
arg_gen(x::AbstractString) = ByteString[cstr(x)]
arg_gen(cmd::Cmd)  = cmd.exec

function arg_gen(head)
    if applicable(start, head)
        vals = ByteString[]
        for x in head
            push!(vals, cstr(string(x)))
        end
        return vals
    else
        return ByteString[cstr(string(head))]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push!(vals, cstr(bytestring(h, t)))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args, arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($(shell_parse(str)[1])))
end

wait(x::Process)      = if !process_exited(x); stream_wait(x, x.exitnotify); end
wait(x::ProcessChain) = for p in x.processes; wait(p); end

show(io::IO, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")
