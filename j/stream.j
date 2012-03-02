typealias PtrSize Int64
const UVHandle = PtrSize
const IOStreamHandle = Ptr{Void}
localEventLoop() = ccall(:jl_local_event_loop,PtrSize,())
globalEventLoop() = ccall(:jl_global_event_loop,PtrSize,())

typealias Executable Union(Vector{ByteString},Function)

type Cmd
    exec::Executable
end

type Cmds
    siblings::Union(Cmd,Set{Cmd})
    pipeline::Cmds
end

abstract AsyncStream <: Stream

type NamedPipe <: AsyncStream
    handle::PtrSize
    buf::IOStream
    NamedPipe(handle::PtrSize,buf::IOStream) = new(handle,buf)
end

type tty <: AsyncStream
    handle::PtrSize
    buf::IOStream
end
typealias StreamHandle Union(PtrSize,AsyncStream)

make_stdout_stream() = tty(ccall(:jl_stdout, PtrSize, ()),memio())

function _uv_tty2tty(handle::PtrSize)
    tty(handle,memio())
end

STDIN  = _uv_tty2tty(ccall(:jl_stdin ,PtrSize,()))
STDERR = _uv_tty2tty(ccall(:jl_stderr,PtrSize,()))

type Process
    handle::PtrSize
    in::StreamHandle
    out::StreamHandle
    err::StreamHandle
    exit_code::PtrSize
    term_signal::PtrSize
    Process(handle::PtrSize,in::StreamHandle,out::StreamHandle,err::StreamHandle)=new(handle,in,out,err,-1,-1)
end

abstract AsyncWork

type SingleAsyncWork <: AsyncWork
    handle::PtrSize
end

type RepeatedAsyncWork <: AsyncWork
    handle::PtrSize
end

function createSingleAsyncWork(loop::PtrSize,cb::PtrSize)
    SingleAsyncWork(ccall(:jl_make_async,PtrSize,(Ptr{PtrSize},Ptr{PtrSize}),loop,cb))
end

function initRepeatedAsyncWork(loop::PtrSize)
    RepeatedAsyncWork(ccall(:jl_idle_init,PtrSize,(Ptr{PtrSize},),loop))
end

assignRepeatedAsyncWork(work::RepeatedAsyncWork,cb::PtrSize) = ccall(:jl_idle_start,PtrSize,(Ptr{PtrSize},Ptr{PtrSize}),work.handle,cb)

function add_idle_cb(loop::PtrSize,cb::PtrSize)
    work = initRepeatedAsyncWork(loop)
    assignRepeatedAsyncWork(work,cb)
    work
end

function queueAsync(work::SingleAsyncWork)
    ccall(:jl_async_send,Void,(Ptr{PtrSize},),work.handle)
end

# process status #
abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::PtrSize; end
type ProcessSignaled <: ProcessStatus; signal::PtrSize; end
type ProcessStopped  <: ProcessStatus; signal::PtrSize; end

process_exited  (s::Process) = (s.exit_code != -1)
process_signaled(s::Process) = (s.term_signal > 0)
process_stopped (s::Process) = 0 #not supported by libuv. Do we need this?

process_exit_status(s::Process) = s.exit_code
process_term_signal(s::Process) = s.term_signal
process_stop_signal(s::Process) = 0 #not supported by libuv. Do we need this?

function process_status(s::PtrSize)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## types

##event loop
function run_event_loop(loop::PtrSize)
    ccall(:jl_run_event_loop,Void,(Ptr{PtrSize},),loop)
end
run_event_loop() = run_event_loop(localEventLoop())


##pipe functions

function make_pipe()
    NamedPipe(ccall(:jl_make_pipe,PtrSize,()),memio())
end

function close_pipe(pipe::NamedPipe)
    ccall(:jl_close_uv,Void,(Ptr{PtrSize},),pipe.handle)
end

##stream functions

function start_reading(stream::AsyncStream,cb::PtrSize)
    ccall(:jl_start_reading,Bool,(PtrSize,Ptr{Void},Ptr{PtrSize}),stream.handle.ios,stream.buf,cb)
end

function stop_reading(stream::AsyncStream,  cb::PtrSize)
    ccall(:jl_stop_reading,Bool.(Ptr{PtrSize},IOStreamHandle),cb)
end
stop_reading(stream::AsyncStream) = stop_reading(stream,0)

function readall(stream::AsyncStream)
start_reading(stream)
run_event_loop()
takebuf_string(stream.buf)
end

##process functions

_spawn(fname::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, in::Ptr{PtrSize}, out::Ptr{PtrSize}, exitcb::Ptr{PtrSize},closecb::Ptr{PtrSize}) = ccall(:jl_spawn, PtrSize, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{PtrSize}, Ptr{PtrSize}, Ptr{PtrSize},Ptr{PtrSize}),fname,argv,in,out,exitcb,closecb)
function spawn(cmd::Cmd, cb::PtrSize)
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

function process_exited(p::Process,h::PtrSize,e::PtrSize, t::PtrSize)
    p.exit_code=e
    p.term_signal=t
end

function readall(cmd::Cmd)
    ptrs = _jl_pre_exec(cmd.exec)
    out=make_pipe()
    pp=Process(0,0,0,0)
    pp.handle=ccall(:jl_spawn, PtrSize, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{PtrSize}, Ptr{PtrSize}, Ptr{PtrSize},Ptr{PtrSize}),ptrs[1], ptrs, C_NULL, out.handle,make_callback(process_exited,(PtrSize,PtrSize,PtrSize),pp), make_callback(finish_read,(),out))
    pp.out=out;
    ccall(:jl_start_reading,Bool,(Ptr{PtrSize},Ptr{Void},Ptr{PtrSize}),out.handle,out.buf.ios,C_NULL)
    run_event_loop()
    return takebuf_string(out.buf)
end

function success(cmd::Cmd)
ptrs = _jl_pre_exec(cmd.exec)
pp=Process(0,0,0,0)
pp.handle=ccall(:jl_spawn, PtrSize, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{PtrSize}, Ptr{PtrSize}, Ptr{PtrSize},Ptr{PtrSize}),ptrs[1], ptrs, C_NULL, C_NULL,make_callback(process_exited,(PtrSize,PtrSize,PtrSize),pp), C_NULL)
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

## Async IO
current_output_stream() = ccall(:jl_current_output_stream_obj, AsyncStream, ())

set_current_output_stream(s::AsyncStream) =
    ccall(:jl_set_current_output_stream_obj, Void, (Any,), s)

function with_output_stream(s::AsyncStream, f::Function, args...)
    try
        set_current_output_stream(s)
        f(args...)
    catch e
        throw(e)
    end
end

# custom version for print_to_*
function _jl_with_output_stream(s::AsyncStream, f::Function, args...)
    try
        set_current_output_stream(s)
        f(args...)
    catch e
        # only add finalizer if takebuf doesn't happen
        finalizer(s, close)
        throw(e)
    end
end

## low-level calls
print(b::ASCIIString) = write(current_output_stream(),b)

write(s::AsyncStream, b::ASCIIString) =
    ccall(:jl_puts, PtrSize, (Ptr{Uint8},Ptr{PtrSize}),b.data,s.handle)

write(s::AsyncStream, b::Uint8) =
    ccall(:jl_putc, PtrSize, (Uint8, Ptr{PtrSize}), unit8(b), s.handle)

write(s::AsyncStream, c::Char) =
    ccall(:jl_pututf8, PtrSize, (Ptr{Uint8},Ptr{PtrSize}), c, s.handle)

function write{T}(s::AsyncStream, a::Array{T})
    if isa(T,BitsKind)
        ccall(:jl_write, Uint,
              (Ptr{Void}, Ptr{Void}, Uint),
              s.ios, a, uint(numel(a)*sizeof(T)))
    else
        invoke(write, (Any, Array), s, a)
    end
end

function write(s::AsyncStream, p::Ptr, nb::Integer)
    ccall(:jl_write, Uint,
          (Ptr{Void}, Ptr{Void}, Uint),
          s.ios, p, uint(nb))
end
