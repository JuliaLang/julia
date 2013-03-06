abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Executable
    ignorestatus::Bool
    Cmd(exec::Executable) = new(exec,false)
end

function each_line(cmd::AbstractCmd,stdin)
    out = NamedPipe()
    processes = spawn(false, cmd, (stdin,out,STDERR))
    EachLine(out)
end
each_line(cmd::AbstractCmd) = each_line(cmd,SpawnNullStream())

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
    if isa(cmds.a, AndCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" | ")
    if isa(cmds.b, AndCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

function show(io::IO, cmds::AndCmds)
    if isa(cmds.a, OrCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" & ")
    if isa(cmds.b, OrCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

const STDIN_NO  = 0
const STDOUT_NO = 1
const STDERR_NO = 2

typealias Redirectable Union(UVStream,FS.File)

type CmdRedirect <: AbstractCmd
        cmd::AbstractCmd
        handle::Redirectable
        stream_no::Int
end

ignorestatus(cmd::Cmd) = (cmd.ignorestatus=true; cmd)
ignorestatus(cmd::Union(OrCmds,AndCmds)) = (ignorestatus(cmd.a); ignorestatus(cmd.b); cmd)

(&)(left::AbstractCmd,right::AbstractCmd) = AndCmds(left,right)
(|)(src::AbstractCmd,dest::AbstractCmd) = OrCmds(src,dest)
(<)(left::AbstractCmd,right::Redirectable) = CmdRedirect(left,right,STDIN_NO)
(>)(left::AbstractCmd,right::Redirectable) = CmdRedirect(left,right,STDOUT_NO)
(<)(left::AbstractCmd,right::String) = left < FS.open(right,JL_O_RDONLY)
(>)(left::AbstractCmd,right::String) = left > FS.open(right,JL_O_WRONLY|JL_O_CREAT,S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
(>>)(left::AbstractCmd,right::String) = left > FS.open(right,JL_O_WRONLY|JL_O_APPEND|JL_O_CREAT,S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
(>)(left::Any,right::AbstractCmd) = right < left
(.>)(left::AbstractCmd,right::Redirectable) = CmdRedirect(left,right,STDERR_NO) 

typealias RawOrBoxedHandle Union(UVHandle,UVStream,FS.File)
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
    exitnotify::Vector{WaitTask}
    closecb::Callback
    closenotify::Vector{WaitTask}
    function Process(cmd::Cmd,handle::Ptr{Void},in::RawOrBoxedHandle,out::RawOrBoxedHandle,err::RawOrBoxedHandle)
        if(!isa(in,AsyncStream))
            in=null_handle
        end
        if(!isa(out,AsyncStream))
            out=null_handle
        end
        if(!isa(err,AsyncStream))
            err=null_handle
        end
        new(cmd,handle,in,out,err,-2,-2,false,WaitTask[],false,WaitTask[])
    end
end

type ProcessChain
    processes::Vector{Process}
    in::UVStream
    out::UVStream
    err::UVStream
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
    proc = c_malloc(ccall(:jl_sizeof_uv_process_t,Int64,()))
    error = ccall(:jl_spawn, Int32,
        (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Void}, Ptr{Void}, Any, Int32,
         Ptr{Void},    Int32,       Ptr{Void},     Int32,       Ptr{Void}),
         cmd,        argv,            loop,      proc, pp,      uvtype(in),
         uvhandle(in), uvtype(out), uvhandle(out), uvtype(err), uvhandle(err))
    if(error != 0)
        c_free(proc)
        throw(UVError("spawn"))
    end
    return proc
end

function _uv_hook_return_spawn(proc::Process, exit_status::Int32, term_signal::Int32)
    proc.exit_code = exit_status
    proc.term_signal = term_signal
    if isa(proc.exitcb, Function) proc.exitcb(proc, exit_status, term_signal) end
    ccall(:jl_close_uv,Void,(Ptr{Void},),proc.handle)
    tasknotify(proc.exitnotify, proc)
end

function _uv_hook_close(proc::Process)
    proc.handle = 0
    if isa(proc.closecb, Function) proc.closecb(proc) end
    tasknotify(proc.closenotify, proc)
end

function spawn(pc::ProcessChainOrNot,cmd::Cmd,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    loop = eventloop()
    close_in,close_out,close_err = false,false,false
    pp = Process(cmd,C_NULL,stdios[1],stdios[2],stdios[3]);
    in,out,err=stdios
    if(isa(stdios[1],NamedPipe)&&stdios[1].handle==C_NULL)
        in = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #in = c_malloc(_sizeof_uv_pipe)
        link_pipe(in,false,stdios[1],true)
        close_in = true
    end
    if(isa(stdios[2],NamedPipe)&&stdios[2].handle==C_NULL)
        out = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #out = c_malloc(_sizeof_uv_pipe)
        link_pipe(stdios[2],false,out,true)
        close_out = true
    end
    if(isa(stdios[3],NamedPipe)&&stdios[3].handle==C_NULL)
        err = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #err = c_malloc(_sizof_uv_pipe)
        link_pipe(stdios[3],false,err,true)
        close_err = true
    end
    ptrs = _jl_pre_exec(cmd.exec)
    pp.exitcb = exitcb
    pp.closecb = closecb
    pp.handle = _jl_spawn(ptrs[1], convert(Ptr{Ptr{Uint8}}, ptrs), loop, pp,
                          in,out,err)
    if pc != false
        push!(pc.processes, pp)
    end
    if(close_in)
        close_pipe_sync(in)
        #c_free(in)
    end
    if(close_out)
        close_pipe_sync(out)
        #c_free(out)
    end
    if(close_err)
        close_pipe_sync(err)
        #c_free(err)
    end
    pp
end

function spawn(pc::ProcessChainOrNot,redirect::CmdRedirect,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
        spawn(pc,redirect.cmd,(redirect.stream_no==STDIN_NO ?redirect.handle:stdios[1],
                                                   redirect.stream_no==STDOUT_NO?redirect.handle:stdios[2],
                                                   redirect.stream_no==STDERR_NO?redirect.handle:stdios[3]),exitcb,closecb)
end

function spawn(pc::ProcessChainOrNot,cmds::OrCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    out_pipe = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
    in_pipe = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
    #out_pipe = c_malloc(_sizeof_uv_pipe)
    #in_pipe = c_malloc(_sizeof_uv_pipe)
    link_pipe(in_pipe,false,out_pipe,false,null_handle)
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

function spawn(pc::ProcessChainOrNot,cmds::AndCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    if pc == false
        pc = ProcessChain(stdios)
    end
    close_in,close_out,close_err = false,false,false
    in,out,err = stdios
    if(isa(stdios[1],NamedPipe)&&stdios[1].handle==C_NULL)
        in = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #in = c_malloc(_sizeof_uv_pipe)
        link_pipe(in,false,stdios[1],true)
        close_in = true
    end
    if(isa(stdios[2],NamedPipe)&&stdios[2].handle==C_NULL)
        out = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #out = c_malloc(_sizeof_uv_pipe)
        link_pipe(stdios[2],false,out,true)
        close_out = true
    end
    if(isa(stdios[3],NamedPipe)&&stdios[3].handle==C_NULL)
        err = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_pipe)))
        #err = c_malloc(_sizof_uv_pipe)
        link_pipe(stdios[3],false,err,true)
        close_err = true
    end
    spawn(pc, cmds.a, (in,out,err), exitcb, closecb)
    spawn(pc, cmds.b, (in,out,err), exitcb, closecb)
    if(close_in)
        close_pipe_sync(in)
        #c_free(in)
    end
    if(close_out)
        close_pipe_sync(out)
        #c_free(out)
    end
    if(close_err)
        close_pipe_sync(err)
        #c_free(err)
    end
    pp
    pc
end

function reinit_stdio()
    STDIN.handle  = ccall(:jl_stdin_stream ,Ptr{Void},())
    STDOUT.handle = ccall(:jl_stdout_stream,Ptr{Void},())
    STDERR.handle = ccall(:jl_stderr_stream,Ptr{Void},())
    STDIN.buffer = PipeBuffer()
    STDOUT.buffer = PipeBuffer()
    STDERR.buffer = PipeBuffer()
    for stream in (STDIN,STDOUT,STDERR)
        ccall(:jl_uv_associate_julia_struct,Void,(Ptr{Void},Any),stream.handle,stream)
    end
end

# INTERNAL
# returns a tuple of function arguments to spawn:
# (stdios, exitcb, closecb)
# |       |        \ The function to be called once the uv handle is closed
# |       \ The function to be called once the process exits
# \ A set of up to 256 stdio instructions, where each entry can be either:
#   | - An AsyncStream to be passed to the child
#   | - true: This will let the child inherit the parents' io (only valid for 0-2)
#   \ - false: None (for 3-255) or /dev/null (for 0-2)


for (sym, stdin, stdout, stderr) in {(:spawn_opts_inherit, STDIN,STDOUT,STDERR),
                       (:spawn_opts_swallow, null_handle,null_handle,null_handle)}
@eval begin
 ($sym)(stdios::StdIOSet,exitcb::Callback,closecb::Callback) = (stdios,exitcb,closecb)
 ($sym)(stdios::StdIOSet,exitcb::Callback) = (stdios,exitcb,false)
 ($sym)(stdios::StdIOSet) = (stdios,false,false)
 ($sym)() = (($stdin,$stdout,$stderr),false,false)
 ($sym)(in::UVStream) = ((isa(in,AsyncStream)?in:$stdin,$stdout,$stderr),false,false)
 ($sym)(in::UVStream,out::UVStream) = ((isa(in,AsyncStream)?in:$stdin,isa(out,AsyncStream)?out:$stdout,$stderr),false,false)
 ($sym)(in::UVStream,out::UVStream,err::UVStream) = ((isa(in,AsyncStream)?in:$stdin,isa(out,AsyncStream)?out:$stdout,isa(err,AsyncStream)?err:$stderr),false,false)
end
end

spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,args...) = spawn(pc,cmds,spawn_opts_swallow(args...)...)
spawn(cmds::AbstractCmd,args...) = spawn(false,cmds,spawn_opts_swallow(args...)...)

spawn_nostdin(pc::ProcessChainOrNot,cmd::AbstractCmd,out::UVStream) = spawn(pc,cmd,(null_handle,out,null_handle),false,false)
spawn_nostdin(cmd::AbstractCmd,out::UVStream) = spawn(false,cmd,(null_handle,out,null_handle),false,false)

#returns a pipe to read from the last command in the pipelines
readsfrom(cmds::AbstractCmd) = readsfrom(cmds, null_handle)
function readsfrom(cmds::AbstractCmd, stdin::AsyncStream)
    out = NamedPipe()
    processes = spawn(false, cmds, (stdin,out,STDERR))
    start_reading(out)
    (out, processes)
end

writesto(cmds::AbstractCmd) = writesto(cmds, null_handle)
function writesto(cmds::AbstractCmd, stdout::UVStream)
    in = NamedPipe()
    processes = spawn(false, cmds, (in,stdout,null_handle))
    (in, processes)
end

function readandwrite(cmds::AbstractCmd)
    in = NamedPipe()
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

writeall(cmd::AbstractCmd, stdout::String) = writeall(cmd, stdout, null_handle)
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

success(proc::Process) = (assert(process_exited(proc)); proc.exit_code==0)
success(procs::Vector{Process}) = all(map(success, procs))
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
        _jl_kill(p, signum)
    else
        int32(-1)
    end
end
kill(ps::Vector{Process}) = map(kill, ps)
kill(ps::ProcessChain) = map(kill, ps.processes)
kill(p::Process) = kill(p,15) #SIGTERM

function _contains_newline(bufptr::Ptr{Void},len::Int32)
    return (ccall(:memchr,Ptr{Uint8},(Ptr{Void},Int32,Uint),bufptr,'\n',len)!=C_NULL)
end

## process status ##
process_running(s::Process) = s.exit_code == -2
process_running(s::Vector{Process}) = all(map(process_running,s))
process_running(s::ProcessChain) = process_running(s.processes)

process_exit_status(s::Process) = s.exit_code
process_exited(s::Process) = !process_running(s)
process_exited(s::Vector{Process}) = all(map(process_exited,s))
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

# Filters
wait_exit_filter(p::Process, args...) = !process_exited(p)
wait_close_filter(w::Union(AsyncStream,Process), args...) = w.open

wait_exit(x::Union(Process,Vector)) = wait(x, :closenotify, wait_exit_filter)
wait_close(x) = wait(x, :closenotify, wait_close_filter)

wait_exit(x::ProcessChain) = wait_exit(x.processes)
function wait_read(x::AsyncStream)
    wait(x, :readnotify, false)
end
wait_success(x::ProcessChain) = wait_success(x.processes)
function wait_success(x::Union(Process,Vector{Process}))
    wait_exit(x)
    kill(x)
    success(x)
end

show(io::IO, p::Process) = print(io, "Process(", p.cmd, ", ", process_status(p), ")")
