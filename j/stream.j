const UVHandle = Int32
const Callback = Int32 #Pointer to _jl_callback_t on the C side
const IOStreamHandle = Ptr{Void}

abstract AsyncStream

type NamedPipe <: AsyncStream
    handle::UVHandle
    buf::IOStream
end

function make_pipe()
    NamedPipe(ccall(:jl_make_pipe,UVHandle,()),memio())
end

function close_pipe(pipe::NamedPipe)
    ccall(:jl_close,Void,(UVHandle,),pipe)
end

function run_event_loop()
    ccall(:jl_run_event_loop,Void,())
end

function start_reading(stream::AsyncStream,cb::Callback)
    ccall(:jl_start_reading,Bool,(UVHandle,IOStreamHandle,Ptr{Callback}),stream.handle.ios,stream.buf,cb)
end
start_reading(stream::AsyncStream) = start_reading(stream,0)

function stop_reading(stream::AsyncStream,  cb::Callback)
    ccall(:jl_stop_reading,Bool.(Callback,IOStreamHandle),cb)
end
stop_reading(stream::AsyncStream) = stop_reading(stream,0)

function readall(stream::AsyncStream)
start_reading(stream)
run_even_loop()
takebuf_string(stream.buf)
end

type Process
    handle::Int32
    in::AsyncStream
    out::AsyncStream
    err::AsyncStream
end

function spawn(cmd::Cmd, cb::Callback)
    ptrs = _jl_pre_exec(cmd.exec)
    Process(ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Uint32}, Ptr{Uint32}), ptrs[1], ptrs, C_NULL, C_NULL))
end

function finish_read(pipe::NamedPipe,string::ByteString)
    string=takebuf_string(pipe.stream);
    close_pipe(pipe)
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function readall(cmd::Cmd)
    ptrs = _jl_pre_exec(cmd.exec)
    out=make_pipe()
    start_reading(out)
    string=ByteString()
    Process(ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, UVHandle, UVHandle, Callback), ptrs[1], ptrs, C_NULL, out.handle, make_callback(finish_read,(),(out,string)))); #close pipe on return (closes event loop)
    run_event_loop()
    string
end
