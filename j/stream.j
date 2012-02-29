const UVHandle = Int32
const IOStreamHandle = Ptr{Void}

abstract AsyncStream

type NamedPipe <: AsyncStream
    handle::Int32
    buf::IOStream
end

function make_pipe()
    NamedPipe(ccall(:jl_make_pipe,Int32,()),memio())
end

function close_pipe(pipe::NamedPipe)
    ccall(:jl_close,Void,(Int32,),pipe)
end

function run_event_loop()
    ccall(:jl_run_event_loop,Void,())
end

function start_reading(stream::AsyncStream,cb::Int32)
    ccall(:jl_start_reading,Bool,(Int32,Ptr{Void},Ptr{Int32}),stream.handle.ios,stream.buf,cb)
end

function stop_reading(stream::AsyncStream,  cb::Int32)
    ccall(:jl_stop_reading,Bool.(Ptr{Int32},IOStreamHandle),cb)
end
stop_reading(stream::AsyncStream) = stop_reading(stream,0)

function readall(stream::AsyncStream)
start_reading(stream)
run_even_loop()
takebuf_string(stream.buf)
end

typealias StreamHandle Union(Int32,AsyncStream)

type Process
    handle::Int32
    in::StreamHandle
    out::StreamHandle
    err::StreamHandle
end

function make_invalid_handle()

end

function spawn(cmd::Cmd, cb::Int32)
    ptrs = _jl_pre_exec(cmd.exec)
    Process(ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Uint32}, Ptr{Uint32}), ptrs[1], ptrs, C_NULL, C_NULL))
end

function finish_read(pipe::NamedPipe)
    close_pipe(pipe)
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function readall(cmd::Cmd)
    ptrs = _jl_pre_exec(cmd.exec)
    out=make_pipe()
    ccall(:jl_start_reading,Bool,(Ptr{Int32},Ptr{Void},Ptr{Int32}),out.handle,out.buf.ios,C_NULL)
    string=ByteString[]
    Process(int32(ccall(:jl_spawn, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}), ptrs[1], ptrs, C_NULL, out.handle, make_callback(finish_read,(),out))),0,out,0); #close pipe on return (closes event loop)
    run_event_loop()
    string
end
