const UVHandle = Int32
const IOStreamHandle = Ptr{Void}

#types

typealias Executable Union(Vector{ByteString},Function)

type Cmd
    exec::Executable
end

type Cmds
    siblings::Union(Cmd,Set{Cmd})
    pipeline::Cmds
end

abstract AsyncStream

type NamedPipe <: AsyncStream
    handle::Int32
    buf::IOStream
    NamedPipe(handle::Int32,buf::IOStream) = new(handle,buf)
end

typealias StreamHandle Union(Int32,AsyncStream)

type Process
    handle::Int32
    in::StreamHandle
    out::StreamHandle
    err::StreamHandle
    exit_code::Int32
    term_signal::Int32
    Process(handle::Int32,in::StreamHandle,out::StreamHandle,err::StreamHandle)=new(handle,in,out,err,-1,-1)
end


# process status #
abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::Int32; end
type ProcessSignaled <: ProcessStatus; signal::Int32; end
type ProcessStopped  <: ProcessStatus; signal::Int32; end

process_exited  (s::Process) = (s.exit_code != -1)
process_signaled(s::Process) = (s.term_signal > 0)
process_stopped (s::Process) = 0 #not supported by libuv. Do we need this?

process_exit_status(s::Process) = s.exit_code
process_term_signal(s::Process) = s.term_signal
process_stop_signal(s::Process) = 0 #not supported by libuv. Do we need this?

function process_status(s::Int32)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## types

##event loop
function run_event_loop()
    ccall(:jl_run_event_loop,Void,())
end


##pipe functions

function make_pipe()
    NamedPipe(ccall(:jl_make_pipe,Int32,()),memio())
end

function close_pipe(pipe::NamedPipe)
    ccall(:jl_close_uv,Void,(Ptr{Int32},),pipe.handle)
end

##stream functions

function start_reading(stream::AsyncStream,cb::Int32)
    ccall(:jl_start_reading,Bool,(Int32,Ptr{Void},Ptr{Int32}),stream.handle.ios,stream.buf,cb)
end

function stop_reading(stream::AsyncStream,  cb::Int32)
    ccall(:jl_stop_reading,Bool.(Ptr{Int32},IOStreamHandle),cb)
end
stop_reading(stream::AsyncStream) = stop_reading(stream,0)

function readall(stream::AsyncStream)
start_reading(stream)
run_event_loop()
takebuf_string(stream.buf)
end

##process functions

_spawn(fname::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, in::Ptr{Int32}, out::Ptr{Int32}, exitcb::Ptr{Int32},closecb::Ptr{Int32}) = ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},Ptr{Int32}),fname,argv,in,out,exitcb,closecb)
function spawn(cmd::Cmd, cb::Int32)
    ptrs = _jl_pre_exec(cmd.exec)
    Process(_spawn(ptrs[1], ptrs, C_NULL, C_NULL, C_NULL)) #no pipes, no callback
end

#show(p::Process) = print("Process")


function finish_read(pipe::NamedPipe)
    close_pipe(pipe) #handles to UV and ios will be invalid after this point
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function process_exited(p::Process,h::Int32,e::Int32, t::Int32)
    p.exit_code=e
    p.term_signal=t
end

function readall(cmd::Cmd)
    ptrs = _jl_pre_exec(cmd.exec)
    out=make_pipe()
    pp=Process(0,0,0,0)
    pp.handle=ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},Ptr{Int32}),ptrs[1], ptrs, C_NULL, out.handle,make_callback(process_exited,(Int32,Int32,Int32),pp), make_callback(finish_read,(),out))
    pp.out=out;
    ccall(:jl_start_reading,Bool,(Ptr{Int32},Ptr{Void},Ptr{Int32}),out.handle,out.buf.ios,C_NULL)
    run_event_loop()
    show(pp.exit_code)
    return takebuf_string(out.buf)
end

function success(cmd::Cmd)
ptrs = _jl_pre_exec(cmd.exec)
pp=Process(0,0,0,0)
pp.handle=ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},Ptr{Int32}),ptrs[1], ptrs, C_NULL, C_NULL,make_callback(process_exited,(Int32,Int32,Int32),pp), C_NULL)
run_event_loop()
return (pp.exit_code==0)
end
run(cmd::Cmd)=success(cmd::Cmd)

function exec(thunk::Function)
    try
        thunk()
    catch e
        show(e)
        exit(0xff)
    end
    exit(0)
end

typealias Executable Vector{ByteString}

## process status ##

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

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push(vals,cstring(x))
        end
        return vals
    else
        return ByteString[cstring(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push(vals, cstring(strcat(h,t)))
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
    :(cmd_gen($_jl_shell_parse(str)))
end

