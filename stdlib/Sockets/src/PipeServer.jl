# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct PipeServer <: LibuvServer
    handle::Ptr{Cvoid}
    status::Int
    cond::Base.ThreadSynchronizer
    function PipeServer(handle::Ptr{Cvoid}, status)
        p = new(handle,
                status,
                Base.ThreadSynchronizer())
        associate_julia_struct(p.handle, p)
        finalizer(uvfinalize, p)
        return p
    end
end

function PipeServer()
    pipe = PipeServer(Libc.malloc(Base._sizeof_uv_named_pipe), StatusUninit)
    iolock_begin()
    err = ccall(:uv_pipe_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cint), eventloop(), pipe.handle, 0)
    uv_error("failed to create pipe server", err)
    pipe.status = StatusInit
    iolock_end()
    return pipe
end

## server functions ##

accept(server::PipeServer) = accept(server, PipeEndpoint())

function accept_nonblock(server::PipeServer, client::PipeEndpoint)
    iolock_begin()
    if client.status != StatusInit
        error("client is already in use or has been closed")
    end
    err = ccall(:uv_accept, Int32, (Ptr{Cvoid}, Ptr{Cvoid}), server.handle, client.handle)
    if err == 0
        client.status = StatusOpen
    end
    iolock_end()
    return err
end

function accept_nonblock(server::PipeServer)
    client = PipeEndpoint()
    uv_error("accept", accept_nonblock(server, client) != 0)
    return client
end

function bind(server::PipeServer, name::AbstractString)
    iolock_begin()
    @assert server.status == StatusInit
    err = ccall(:uv_pipe_bind, Int32, (Ptr{Cvoid}, Cstring),
                server, name)
    if err != 0
        iolock_end()
        if err != UV_EADDRINUSE && err != UV_EACCES
            #TODO: this codepath is currently not tested
            throw(_UVError("bind", err))
        else
            return false
        end
    end
    server.status = StatusOpen
    iolock_end()
    return true
end

"""
    listen(path::AbstractString) -> PipeServer

Create and listen on a named pipe / UNIX domain socket.
"""
function listen(path::AbstractString)
    sock = PipeServer()
    bind(sock, path) || throw(ArgumentError("could not listen on path $path"))
    return listen(sock)
end

function connect!(sock::PipeEndpoint, path::AbstractString)
    iolock_begin()
    @assert sock.status == StatusInit
    req = Libc.malloc(Base._sizeof_uv_connect)
    uv_req_set_data(req, C_NULL)
    ccall(:uv_pipe_connect, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}), req, sock.handle, path,
          uv_jl_connectcb::Ptr{Cvoid})
    sock.status = StatusConnecting
    iolock_end()
    return sock
end

"""
    connect(path::AbstractString) -> PipeEndpoint

Connect to the named pipe / UNIX domain socket at `path`.
"""
connect(path::AbstractString) = connect(PipeEndpoint(), path)
