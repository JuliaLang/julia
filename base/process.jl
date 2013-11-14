abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Vector{ByteString}
    ignorestatus::Bool
    detach::Bool
    env::Union(Array{ByteString},Nothing)
    Cmd(exec::Vector{ByteString}) = new(exec,false,false,nothing)
end

type OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type ErrOrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    ErrOrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

shell_escape(cmd::Cmd) = shell_escape(cmd.exec...)

function show(io::IO, cmd::Cmd)
    if isa(cmd.exec,Vector{ByteString})
        esc = shell_escape(cmd)
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
    function FileRedirect(filename,append)
        if lowercase(filename) == (@unix? "/dev/null" : "nul")
            warn_once("for portability use DevNull instead of a file redirect")
        end
        new(filename,append)
    end
end

immutable DevNullStream <: AsyncStream end
const DevNull = DevNullStream()
copy(::DevNullStream) = DevNull
uvhandle(::DevNullStream) = C_NULL
uvhandle(x::Ptr) = x
uvtype(::Ptr) = UV_STREAM
uvtype(::DevNullStream) = UV_STREAM

typealias Redirectable Union(UVStream,FS.File,FileRedirect,DevNullStream,IOStream)

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
(.>)(src::AbstractCmd,dest::AbstractCmd) = ErrOrCmds(src,dest)

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


typealias RawOrBoxedHandle Union(UVHandle,UVStream,Redirectable,IOStream)
typealias StdIOSet NTuple{3,RawOrBoxedHandle}

type Process
    cmd::Cmd
    handle::Ptr{Void}
    in::AsyncStream
    out::AsyncStream
    err::AsyncStream
    exitcode::Int32
    termsignal::Int32
    exitcb::Callback
    exitnotify::Condition
    closecb::Callback
    closenotify::Condition
    function Process(cmd::Cmd,handle::Ptr{Void},in::RawOrBoxedHandle,out::RawOrBoxedHandle,err::RawOrBoxedHandle)
        if !isa(in,AsyncStream) || in === DevNull
            in=DevNull
        end
        if !isa(out,AsyncStream) || out === DevNull
            out=DevNull
        end
        if !isa(err,AsyncStream) || err === DevNull
            err=DevNull
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

function _uv_hook_return_spawn(proc::Process, exit_status::Int32, termsignal::Int32)
    proc.exitcode = exit_status
    proc.termsignal = termsignal
    if isa(proc.exitcb, Function) proc.exitcb(proc, exit_status, termsignal) end
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
    link_pipe(in_pipe,false,out_pipe,false)
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

function spawn(pc::ProcessChainOrNot,cmds::ErrOrCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    out_pipe = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
    in_pipe = box(Ptr{Void},Intrinsics.jl_alloca(_sizeof_uv_named_pipe))
    #out_pipe = c_malloc(_sizeof_uv_named_pipe)
    #in_pipe = c_malloc(_sizeof_uv_named_pipe)
    link_pipe(in_pipe,false,out_pipe,false)
    if pc == false
        pc = ProcessChain(stdios)
    end
    try
        spawn(pc, cmds.a, (stdios[1], stdios[2], out_pipe), exitcb, closecb)
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
        if isa(stdios[1],Pipe) 
            if stdios[1].handle==C_NULL
                error("Pipes passed to spawn must be initialized!")
            end
        elseif isa(stdios[1],FileRedirect)
            in = FS.open(stdios[1].filename,JL_O_RDONLY)
            close_in = true
        elseif isa(stdios[1],IOStream)
            in = FS.File(RawFD(fd(stdios[1])))
        end
        if isa(stdios[2],Pipe)
            if stdios[2].handle==C_NULL
                error("Pipes passed to spawn must be initialized!")
            end
        elseif isa(stdios[2],FileRedirect)
            out = FS.open(stdios[2].filename,JL_O_WRONLY|JL_O_CREAT|(stdios[2].append?JL_O_APPEND:JL_O_TRUNC),S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
            close_out = true
        elseif isa(stdios[2],IOStream)
            out = FS.File(RawFD(fd(stdios[2])))
        end
        if isa(stdios[3],Pipe)
            if stdios[3].handle==C_NULL
                error("Pipes passed to spawn must be initialized!")
            end
        elseif isa(stdios[3],FileRedirect)
            err = FS.open(stdios[3].filename,JL_O_WRONLY|JL_O_CREAT|(stdios[3].append?JL_O_APPEND:JL_O_TRUNC),S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
            close_err = true
        elseif isa(stdios[3],IOStream)
            err = FS.File(RawFD(fd(stdios[3])))
        end
    end)
end

macro cleanup_stdio()
    esc(
    quote
        close_in && close(in)
        close_out && close(out)
        close_err && close(err)
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
#   | - DevNull to pass /dev/null
#   | - An FS.File object to redirect the output to
#   \ - An ASCIIString specifying a filename to be opened

spawn_opts_swallow(stdios::StdIOSet,exitcb::Callback=false,closecb::Callback=false) =
    (stdios,exitcb,closecb)
spawn_opts_swallow(in::Redirectable=DevNull,out::Redirectable=DevNull,err::Redirectable=DevNull,args...) =
    (tuple(in,out,err,args...),false,false)
spawn_opts_inherit(stdios::StdIOSet,exitcb::Callback=false,closecb::Callback=false) =
    (stdios,exitcb,closecb)
spawn_opts_inherit(in::Redirectable=STDIN,out::Redirectable=STDOUT,err::Redirectable=STDERR,args...) = 
    (tuple(in,out,err,args...),false,false)

spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,args...) = spawn(pc,cmds,spawn_opts_swallow(args...)...)
spawn(cmds::AbstractCmd,args...) = spawn(false,cmds,spawn_opts_swallow(args...)...)

macro tmp_rpipe(pipe,tmppipe,code,args...)
    esc(quote
        $pipe = Pipe(C_NULL)
        $tmppipe = Pipe(C_NULL)
        link_pipe($pipe,true,$tmppipe,false)
        r = begin
            $code
        end
        close_pipe_sync($tmppipe)
        r
    end)
end

macro tmp_wpipe(tmppipe,pipe,code)
    esc(quote
        $pipe = Pipe(C_NULL)
        $tmppipe = Pipe(C_NULL)
        link_pipe($tmppipe,false,$pipe,true)
        r = begin
            $code
        end
        close_pipe_sync($tmppipe)
        r
    end)
end

function eachline(cmd::AbstractCmd,stdin)
    @tmp_rpipe out tmp begin
        processes = spawn(false, cmd, (stdin,tmp,STDERR))
        # implicitly close after reading lines, since we opened
        EachLine(out, ()->close(out))
    end
end
eachline(cmd::AbstractCmd) = eachline(cmd,DevNull)

#returns a pipe to read from the last command in the pipelines
readsfrom(cmds::AbstractCmd) = readsfrom(cmds, DevNull)
function readsfrom(cmds::AbstractCmd, stdin::AsyncStream)
    processes = @tmp_rpipe out tmp spawn(false, cmds, (stdin,tmp,STDERR))
    start_reading(out)
    (out, processes)
end

function writesto(cmds::AbstractCmd, stdout::UVStream)
    processes = @tmp_wpipe tmp inpipe spawn(false, cmds, (tmp,stdout,STDERR))
    (inpipe, processes)
end
writesto(cmds::AbstractCmd) = writesto(cmds, DevNull)

function readandwrite(cmds::AbstractCmd)
    (out, processes) = @tmp_wpipe tmp inpipe readsfrom(cmds, tmp)
    (out, inpipe, processes)
end

function readbytes(cmd::AbstractCmd, stdin::AsyncStream=DevNull)
    (out,pc) = readsfrom(cmd, stdin)
    if !success(pc)
        pipeline_error(pc)
    end
    wait_close(out)
    return takebuf_array(out.buffer)
end

function readall(cmd::AbstractCmd, stdin::AsyncStream=DevNull)
    return bytestring(readbytes(cmd, stdin))
end

writeall(cmd::AbstractCmd, stdin::String) = writeall(cmd, stdin, DevNull)
function writeall(cmd::AbstractCmd, stdin::String, stdout::AsyncStream)
    (in,pc) = writesto(cmd, stdout)
    write(in, stdin)
    close(in)
    if !success(pc)
        pipeline_error(pc)
    end
    return true
end

function run(cmds::AbstractCmd,args...)
    ps = spawn(cmds,spawn_opts_inherit(args...)...)
    success(ps) ? nothing : pipeline_error(ps)
end

const SIGPIPE = 13
function test_success(proc::Process)
    assert(process_exited(proc))
    if proc.exitcode < 0
        error(UVError("could not start process "*string(proc.cmd),proc.exitcode))
    end
    proc.exitcode==0 && (proc.termsignal == 0 || proc.termsignal == SIGPIPE)
end

function success(x::Process)
    wait(x)
    kill(x)
    test_success(x)
end
success(procs::Vector{Process}) = all(success, procs)
success(procs::ProcessChain) = success(procs.processes)
success(cmd::AbstractCmd) = success(spawn(cmd))

function pipeline_error(proc::Process)
    if !proc.cmd.ignorestatus
        error("failed process: ",proc," [",proc.exitcode,"]")
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
    if length(failed)==0 return nothing end
    if length(failed)==1 pipeline_error(failed[1]) end
    msg = "failed processes:"
    for proc in failed
        msg = string(msg,"\n  ",proc," [",proc.exitcode,"]")
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
process_running(s::Process) = s.exitcode == typemin(Int32)
process_running(s::Vector{Process}) = any(process_running,s)
process_running(s::ProcessChain) = process_running(s.processes)

process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(process_exited,s)
process_exited(s::ProcessChain) = process_exited(s.processes)

process_signaled(s::Process) = (s.termsignal > 0)

#process_stopped (s::Process) = false #not supported by libuv. Do we need this?
#process_stop_signal(s::Process) = false #not supported by libuv. Do we need this?

function process_status(s::Process)
    process_running (s) ? "ProcessRunning" :
    process_signaled(s) ? "ProcessSignaled("*string(s.termsignal)*")" :
    #process_stopped (s) ? "ProcessStopped("*string(process_stop_signal(s))*")" :
    process_exited  (s) ? "ProcessExited("*string(s.exitcode)*")" :
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
    :(cmd_gen($(shell_parse(str)[1])))
end

wait(x::Process)      = if !process_exited(x); wait(x.exitnotify); end
wait(x::ProcessChain) = for p in x.processes; wait(p); end

show(io::IO, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")
