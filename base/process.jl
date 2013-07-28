abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Executable
    ignorestatus::Bool
    detach::Bool
    env::Union(Array{ByteString},Nothing)
    Cmd(exec::Executable) = new(exec,false,false,nothing)
end

function eachline(cmd::AbstractCmd,stdin)
    out = NamedPipe(C_NULL)
    processes = spawn(false, cmd, (stdin,out,STDERR))
    # implicitly close after reading lines, since we opened
    EachLine(out, ()->close(out))
end
eachline(cmd::AbstractCmd) = eachline(cmd,SpawnNullStream())

type OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

function show(io::IO, cmd::Cmd)
    if isa(cmd.exec,Vector{ByteString})
        esc = shell_escape(cmd.exec...)
        print(io,'`')
        for c in esc
            if c == '`'
                print(io,'\\')
            end
            print(io,c)
        end
        print(io,'`')
    else
        print(io, cmd.exec)
    end
end

function show(io::IO, cmds::OrCmds)
    if isa(cmds.a, AndCmds) || isa(cmds.a, CmdRedirect)
        print(io,"(")
        show(io, cmds.a)
        print(io,")")
    else
        show(io, cmds.a)
    end
    print(io, " |> ")
    if isa(cmds.b, AndCmds) || isa(cmds.b, CmdRedirect)
        print(io,"(")
        show(io, cmds.b)
        print(io,")")
    else
        show(io, cmds.b)
    end
end

function show(io::IO, cmds::AndCmds)
    if isa(cmds.a, OrCmds) || isa(cmds.a, CmdRedirect)
        print(io,"(")
        show(io, cmds.a)
        print(io,")")
    else
        show(io, cmds.a)
    end
    print(io," & ")
    if isa(cmds.b, OrCmds) || isa(cmds.b, CmdRedirect)
        print(io,"(")
        show(io, cmds.b)
        print(io,")")
    else
        show(io, cmds.b)
    end
end

const STDIN_NO  = 0
const STDOUT_NO = 1
const STDERR_NO = 2

immutable FileRedirect
    filename::String
    append::Bool
end

typealias Redirectable Union(UVStream,FS.File,FileRedirect)

type CmdRedirect <: AbstractCmd
    cmd::AbstractCmd
    handle::Redirectable
    stream_no::Int
end

function show(io::IO, cr::CmdRedirect)
    if cr.stream_no == STDOUT_NO
        show(io,cr.cmd)
        print(io," |> ")
        show(io,cr.handle)
    elseif cr.stream_no == STDERR_NO
        show(io,cr.cmd)
        print(io," .> ")
        show(io,cr.handle)
    elseif cr.stream_no == STDIN_NO
        show(io,cr.handle)    
        print(io," |> ")
        show(io,cr.cmd)
    end
end


ignorestatus(cmd::Cmd) = (cmd.ignorestatus=true; cmd)
ignorestatus(cmd::Union(OrCmds,AndCmds)) = (ignorestatus(cmd.a); ignorestatus(cmd.b); cmd)
detach(cmd::Cmd) = (cmd.detach=true; cmd)

setenv{S<:ByteString}(cmd::Cmd, env::Array{S}) = (cmd.env = ByteString[x for x in env];cmd)
setenv(cmd::Cmd, env::Associative) = (cmd.env = ByteString[string(k)*"="*string(v) for (k,v) in env];cmd)

(&)(left::AbstractCmd,right::AbstractCmd) = AndCmds(left,right)
(|>)(src::AbstractCmd,dest::AbstractCmd) = OrCmds(src,dest)

# Stream Redirects
(|>)(dest::Redirectable,src::AbstractCmd) = CmdRedirect(src,dest,STDIN_NO)
(|>)(src::AbstractCmd,dest::Redirectable) = CmdRedirect(src,dest,STDOUT_NO)
(.>)(src::AbstractCmd,dest::Redirectable) = CmdRedirect(src,dest,STDERR_NO)

# File redirects
(|>)(src::AbstractCmd,dest::String) = CmdRedirect(src,FileRedirect(dest,false),STDOUT_NO)
(|>)(src::String,dest::AbstractCmd) = CmdRedirect(dest,FileRedirect(src,false),STDIN_NO)
(.>)(src::AbstractCmd,dest::String) = CmdRedirect(src,FileRedirect(dest,false),STDERR_NO)
(>>)(src::AbstractCmd,dest::String) = CmdRedirect(src,FileRedirect(dest,true),STDOUT_NO)
(.>>)(src::AbstractCmd,dest::String) = CmdRedirect(src,FileRedirect(dest,true),STDERR_NO)


typealias RawOrBoxedHandle Union(UVHandle,UVStream,FS.File,FileRedirect)
typealias StdIOSet (RawOrBoxedHandle, RawOrBoxedHandle, RawOrBoxedHandle)

type Process
    cmd::Cmd
    handle::Ptr{Void}
    in::AsyncStream
    out::AsyncStream
    err::AsyncStream
    exit_code::Int32
    term_signal::Int32
    exitcb::Callback
    exitnotify::Condition
    closecb::Callback
    closenotify::Condition
    function Process(cmd::Cmd,handle::Ptr{Void},in::RawOrBoxedHandle,out::RawOrBoxedHandle,err::RawOrBoxedHandle)
        if !isa(in,AsyncStream)
            in=null_handle
        end
        if !isa(out,AsyncStream)
            out=null_handle
        end
        if !isa(err,AsyncStream)
            err=null_handle
        end
        new(cmd,handle,in,out,err,typemin(Int32),typemin(Int32),false,Condition(),false,Condition())
    end
end

type ProcessChain
    processes::Vector{Process}
    in::Redirectable
    out::Redirectable
    err::Redirectable
    ProcessChain(stdios::StdIOSet) = new(Process[],stdios[1],stdios[2],stdios[3])
end
typealias ProcessChainOrNot Union(Bool,ProcessChain)

#SpawnNullStream is Singleton
type SpawnNullStream <: AsyncStream end
const null_handle = SpawnNullStream()
SpawnNullStream() = null_handle
copy(::SpawnNullStream) = null_handle
uvhandle(::SpawnNullStream) = C_NULL
uvhandle(x::Ptr) = x
uvtype(::Ptr) = UV_STREAM

function _jl_spawn(cmd::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, loop::Ptr{Void}, pp::Process,
                   in, out, err)
    proc = c_malloc(_sizeof_uv_process)
    error = ccall(:jl_spawn, Int32,
        (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Void}, Ptr{Void}, Any, Int32,
         Ptr{Void},    Int32,       Ptr{Void},     Int32,       Ptr{Void},
         Int32, Ptr{Ptr{Uint8}}),
         cmd,        argv,            loop,      proc,      pp,  uvtype(in),
         uvhandle(in), uvtype(out), uvhandle(out), uvtype(err), uvhandle(err),
         pp.cmd.detach, pp.cmd.env === nothing ? C_NULL : pp.cmd.env)
    if error != 0
        c_free(proc)
        throw(UVError("spawn",error))
    end
    associate_julia_struct(proc,pp)
    return proc
end

function _uv_hook_return_spawn(proc::Process, exit_status::Int32, term_signal::Int32)
    proc.exit_code = exit_status
    proc.term_signal = term_signal
    if isa(proc.exitcb, Function) proc.exitcb(proc, exit_status, term_signal) end
    ccall(:jl_close_uv,Void,(Ptr{Void},),proc.handle)
    notify(proc.exitnotify)
end

function _uv_hook_close(proc::Process)
    proc.handle = 0
    if isa(proc.closecb, Function) proc.closecb(proc) end
    notify(proc.closenotify)
end

function spawn(pc::ProcessChainOrNot,redirect::CmdRedirect,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
        spawn(pc,redirect.cmd,(redirect.stream_no==STDIN_NO ?redirect.handle:stdios[1],
                                                   redirect.stream_no==STDOUT_NO?redirect.handle:stdios[2],
                                                   redirect.stream_no==STDERR_NO?redirect.handle:stdios[3]),exitcb,closecb)
end

function spawn(pc::ProcessChainOrNot,cmds::OrCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    out_pipe = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
    in_pipe = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
    #out_pipe = c_malloc(_sizeof_uv_named_pipe)
    #in_pipe = c_malloc(_sizeof_uv_named_pipe)
    link_pipe(in_pipe,false,out_pipe,false,null_handle,null_handle)
    if pc == false
        pc = ProcessChain(stdios)
    end
    try
        spawn(pc, cmds.a, (stdios[1], out_pipe, stdios[3]), exitcb, closecb)
        spawn(pc, cmds.b, (in_pipe, stdios[2], stdios[3]), exitcb, closecb)
    catch err
        close_pipe_sync(out_pipe)
        close_pipe_sync(in_pipe)
        rethrow(err)
    end
    close_pipe_sync(out_pipe)
    close_pipe_sync(in_pipe)
    pc
end

macro setup_stdio()
    esc(
    quote
        close_in,close_out,close_err = false,false,false
        in,out,err = stdios
        if isa(stdios[1],NamedPipe) 
            if stdios[1].handle==C_NULL
                in = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
                #in = c_malloc(_sizeof_uv_named_pipe)
                link_pipe(in,false,stdios[1],true)
                close_in = true
            end
        elseif isa(stdios[1],FileRedirect)
            in = FS.open(stdios[1].filename,JL_O_RDONLY)
            close_in = true
        end
        if isa(stdios[2],NamedPipe)
            if stdios[2].handle==C_NULL
                out = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
                #out = c_malloc(_sizeof_uv_named_pipe)
                link_pipe(stdios[2],false,out,true)
                close_out = true
            end
        elseif isa(stdios[2],FileRedirect)
            out = FS.open(stdios[2].filename,JL_O_WRONLY|JL_O_CREAT|(stdios[2].append?JL_O_APPEND:JL_O_TRUNC),S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
            close_out = true
        end
        if isa(stdios[3],NamedPipe)
            if stdios[3].handle==C_NULL
                err = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
                #err = c_malloc(_sizeof_uv_named_pipe)
                link_pipe(stdios[3],false,err,true)
                close_err = true
            end
        elseif isa(stdios[3],FileRedirect)
            err = FS.open(stdios[3].filename,JL_O_WRONLY|JL_O_CREAT|(stdios[3].append?JL_O_APPEND:JL_O_TRUNC),S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
            close_err = true
        end
    end)
end

macro cleanup_stdio()
    esc(
    quote
        if close_in
            if isa(in,Ptr)
                close_pipe_sync(in)
            else
                close(in)
            end
        end
        if close_out
            if isa(out,Ptr)
                close_pipe_sync(out)
            else
                close(out)
            end
        end
        if close_err
            if isa(err,Ptr)
                close_pipe_sync(err)
            else
                close(err)
            end
        end
    end)
end

function spawn(pc::ProcessChainOrNot,cmd::Cmd,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    loop = eventloop()
    pp = Process(cmd,C_NULL,stdios[1],stdios[2],stdios[3]);
    @setup_stdio
    ptrs = _jl_pre_exec(cmd.exec)
    pp.exitcb = exitcb
    pp.closecb = closecb
    pp.handle = _jl_spawn(ptrs[1], convert(Ptr{Ptr{Uint8}}, ptrs), loop, pp,
                          in,out,err)
    @cleanup_stdio
    if isa(pc, ProcessChain)
        push!(pc.processes,pp)
    end
    pp
end

function spawn(pc::ProcessChainOrNot,cmds::AndCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    if pc == false
        pc = ProcessChain(stdios)
    end
    @setup_stdio
    spawn(pc, cmds.a, (in,out,err), exitcb, closecb)
    spawn(pc, cmds.b, (in,out,err), exitcb, closecb)
    @cleanup_stdio
    pc
end

# INTERNAL
# returns a tuple of function arguments to spawn:
# (stdios, exitcb, closecb)
# |       |        \ The function to be called once the uv handle is closed
# |       \ The function to be called once the process exits
# \ A set of up to 256 stdio instructions, where each entry can be either:
#   | - An AsyncStream to be passed to the child
#   | - null_handle to pass /dev/null
#   | - An FS.File object to redirect the output to
#   \ - An ASCIIString specifying a filename to be opened

spawn_opts_swallow(stdios::StdIOSet,exitcb::Callback=false,closecb::Callback=false) =
    (stdios,exitcb,closecb)
spawn_opts_swallow(in::Redirectable=null_handle,out::Redirectable=null_handle,err::Redirectable=null_handle,args...) = 
    (tuple(in,out,err,args...),false,false)
spawn_opts_inherit(stdios::StdIOSet,exitcb::Callback=false,closecb::Callback=false) =
    (stdios,exitcb,closecb)
spawn_opts_inherit(in::Redirectable=STDIN,out::Redirectable=STDOUT,err::Redirectable=STDERR,args...) = 
    (tuple(in,out,err,args...),false,false)

spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,args...) = spawn(pc,cmds,spawn_opts_swallow(args...)...)
spawn(cmds::AbstractCmd,args...) = spawn(false,cmds,spawn_opts_swallow(args...)...)

spawn_nostdin(pc::ProcessChainOrNot,cmd::AbstractCmd,out::UVStream) = spawn(pc,cmd,(null_handle,out,null_handle),false,false)
spawn_nostdin(cmd::AbstractCmd,out::UVStream) = spawn(false,cmd,(null_handle,out,null_handle),false,false)

#returns a pipe to read from the last command in the pipelines
readsfrom(cmds::AbstractCmd) = readsfrom(cmds, null_handle)
function readsfrom(cmds::AbstractCmd, stdin::AsyncStream)
    out = NamedPipe(C_NULL)
    processes = spawn(false, cmds, (stdin,out,STDERR))
    start_reading(out)
    (out, processes)
end

writesto(cmds::AbstractCmd) = writesto(cmds, null_handle)
function writesto(cmds::AbstractCmd, stdout::UVStream)
    in = NamedPipe(C_NULL)
    processes = spawn(false, cmds, (in,stdout,null_handle))
    (in, processes)
end

function readandwrite(cmds::AbstractCmd)
    in = NamedPipe(C_NULL)
    (out, processes) = readsfrom(cmds, in)
    return (out, in, processes)
end

readall(cmd::AbstractCmd) = readall(cmd, null_handle)
function readall(cmd::AbstractCmd,stdin::AsyncStream)
    (out,pc) = readsfrom(cmd, stdin)
    if !wait_success(pc)
        pipeline_error(pc)
    end
    wait_close(out)
    return takebuf_string(out.buffer)
end

writeall(cmd::AbstractCmd, stdin::String) = writeall(cmd, stdin, null_handle)
function writeall(cmd::AbstractCmd, stdin::String, stdout::AsyncStream)
    (in,pc) = writesto(cmd, stdout)
    write(in, stdin)
    close(in)
    if !wait_success(pc)
        pipeline_error(pc)
    end
    return true
end

function run(cmds::AbstractCmd,args...)
    ps = spawn(cmds,spawn_opts_inherit(args...)...)
    wait_success(ps) ? nothing : pipeline_error(ps)
end

const SIGPIPE = 13
function success(proc::Process)
    assert(process_exited(proc))
    if proc.exit_code < 0
        error(UVError("could not start process "*proc,proc.exit_code))
    end
    proc.exit_code==0 && (proc.term_signal == 0 || proc.term_signal == SIGPIPE)
end
success(procs::Vector{Process}) = all(success, procs)
success(procs::ProcessChain) = success(procs.processes)
success(cmd::AbstractCmd) = wait_success(spawn(cmd))

function pipeline_error(proc::Process)
    if !proc.cmd.ignorestatus
        error("failed process: ",proc," [",proc.exit_code,"]")
    end
    nothing
end

function pipeline_error(procs::ProcessChain)
    failed = Process[]
    for p = procs.processes
        if !success(p) && !p.cmd.ignorestatus
            push!(failed, p)
        end
    end
    if length(failed)==0 return nothing end
    if length(failed)==1 pipeline_error(failed[1]) end
    msg = "failed processes:"
    for proc in failed
        msg = string(msg,"\n  ",proc," [",proc.exit_code,"]")
    end
    error(msg)
end

_jl_kill(p::Process,signum::Integer) = ccall(:uv_process_kill,Int32,(Ptr{Void},Int32),p.handle,signum)
function kill(p::Process,signum::Integer)
    if process_running(p)
        @assert p.handle != C_NULL
        _jl_kill(p, signum)
    else
        int32(-1)
    end
end
kill(ps::Vector{Process}) = map(kill, ps)
kill(ps::ProcessChain) = map(kill, ps.processes)
kill(p::Process) = kill(p,15) #SIGTERM

function _contains_newline(bufptr::Ptr{Void},len::Int32)
    return (ccall(:memchr,Ptr{Void},(Ptr{Void},Int32,Csize_t),bufptr,'\n',len)!=C_NULL)
end

## process status ##
process_running(s::Process) = s.exit_code == typemin(Int32)
process_running(s::Vector{Process}) = all(process_running,s)
process_running(s::ProcessChain) = process_running(s.processes)

process_exit_status(s::Process) = s.exit_code
process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(process_exited,s)
process_exited(s::ProcessChain) = process_running(s.processes)

process_term_signal(s::Process) = s.term_signal
process_signaled(s::Process) = (s.term_signal > 0)

#process_stopped (s::Process) = false #not supported by libuv. Do we need this?
#process_stop_signal(s::Process) = false #not supported by libuv. Do we need this?

function process_status(s::Process)
    process_running (s) ? "ProcessRunning" :
    process_signaled(s) ? "ProcessSignaled("*string(process_term_signal(s))*")" :
    #process_stopped (s) ? "ProcessStopped("*string(process_stop_signal(s))*")" :
    process_exited  (s) ? "ProcessExited("*string(process_exit_status(s))*")" :
    error("process status error")
end

# WARNING: do not call this and keep the returned array of pointers
# around longer than the args vector and then use array of pointers.
# this could cause a segfault. this is really just for use by the
# spawn function below so that we can exec more efficiently.
#
function _jl_pre_exec(args::Vector{ByteString})
    if length(args) < 1
        error("exec: too few words to exec")
    end
    ptrs = Array(Ptr{Uint8}, length(args)+1)
    for i = 1:length(args)
        ptrs[i] = args[i].data
    end
    ptrs[length(args)+1] = C_NULL
    return ptrs
end

## implementation of `cmd` syntax ##

arg_gen()          = ByteString[]
arg_gen(x::String) = ByteString[x]
arg_gen(cmd::Cmd)  = cmd.exec

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push!(vals,string(x))
        end
        return vals
    else
        return ByteString[string(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push!(vals,bytestring(h,t))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args,arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($(shell_parse(str))))
end

wait_close(x) = if isopen(x) wait(x.closenotify); end

wait_exit(x::Process)      = if !process_exited(x); wait(x.exitnotify); end
wait_exit(x::ProcessChain) = for p in x.processes; wait_exit(p); end

function wait_success(x::Process)
    wait_exit(x)
    kill(x)
    success(x)
end
function wait_success(x::ProcessChain)
    s = true
    for p in x.processes
        s &= wait_success(p)
    end
    s
end

wait(p::Process) = (wait_exit(p); p.exit_code)

show(io::IO, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")
