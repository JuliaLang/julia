# This file is a part of Julia. License is MIT: https://julialang.org/license

# Core definitions for interacting with the libuv library from Julia

include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "uv_constants.jl"))  # include($BUILDROOT/base/uv_constants.jl)

# convert UV handle data to julia object, checking for null
function uv_sizeof_handle(handle)
    if !(UV_UNKNOWN_HANDLE < handle < UV_HANDLE_TYPE_MAX)
        throw(DomainError(handle))
    end
    return ccall(:uv_handle_size, Csize_t, (Int32,), handle)
end

function uv_sizeof_req(req)
    if !(UV_UNKNOWN_REQ < req < UV_REQ_TYPE_MAX)
        throw(DomainError(req))
    end
    return ccall(:uv_req_size, Csize_t, (Int32,), req)
end

for h in uv_handle_types
@eval const $(Symbol("_sizeof_", lowercase(string(h)))) = uv_sizeof_handle($h)
end
for r in uv_req_types
@eval const $(Symbol("_sizeof_", lowercase(string(r)))) = uv_sizeof_req($r)
end

uv_handle_data(handle) = ccall(:jl_uv_handle_data, Ptr{Cvoid}, (Ptr{Cvoid},), handle)
uv_req_data(handle) = ccall(:jl_uv_req_data, Ptr{Cvoid}, (Ptr{Cvoid},), handle)
uv_req_set_data(req, data) = ccall(:jl_uv_req_set_data, Cvoid, (Ptr{Cvoid}, Any), req, data)
uv_req_set_data(req, data::Ptr{Cvoid}) = ccall(:jl_uv_req_set_data, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), req, data)

macro handle_as(hand, typ)
    return quote
        local data = uv_handle_data($(esc(hand)))
        data == C_NULL && return
        unsafe_pointer_to_objref(data)::($(esc(typ)))
    end
end

associate_julia_struct(handle::Ptr{Cvoid}, @nospecialize(jlobj)) =
    ccall(:jl_uv_associate_julia_struct, Cvoid, (Ptr{Cvoid}, Any), handle, jlobj)
disassociate_julia_struct(uv) = disassociate_julia_struct(uv.handle)
disassociate_julia_struct(handle::Ptr{Cvoid}) =
    handle != C_NULL && ccall(:jl_uv_disassociate_julia_struct, Cvoid, (Ptr{Cvoid},), handle)

iolock_begin() = ccall(:jl_iolock_begin, Cvoid, ())
iolock_end() = ccall(:jl_iolock_end, Cvoid, ())

# A dict of all libuv handles that are being waited on somewhere in the system
# and should thus not be garbage collected
const uvhandles = IdDict()
const preserve_handle_lock = Threads.SpinLock()
function preserve_handle(x)
    lock(preserve_handle_lock)
    v = get(uvhandles, x, 0)::Int
    uvhandles[x] = v + 1
    unlock(preserve_handle_lock)
    nothing
end
function unpreserve_handle(x)
    lock(preserve_handle_lock)
    v = uvhandles[x]::Int
    if v == 1
        pop!(uvhandles, x)
    else
        uvhandles[x] = v - 1
    end
    unlock(preserve_handle_lock)
    nothing
end

## Libuv error handling ##

struct IOError <: Exception
    msg::AbstractString
    code::Int32
    IOError(msg::AbstractString, code::Integer) = new(msg, code)
end

showerror(io::IO, e::IOError) = print(io, "IOError: ", e.msg)

function _UVError(pfx::AbstractString, code::Integer)
    code = Int32(code)
    IOError(string(pfx, ": ", struverror(code), " (", uverrorname(code), ")"), code)
end
function _UVError(pfx::AbstractString, code::Integer, sfxs::AbstractString...)
    code = Int32(code)
    IOError(string(pfx, ": ", struverror(code), " (", uverrorname(code), ")", " ", sfxs...), code)
end

struverror(err::Int32) = unsafe_string(ccall(:uv_strerror, Cstring, (Int32,), err))
uverrorname(err::Int32) = unsafe_string(ccall(:uv_err_name, Cstring, (Int32,), err))

uv_error(prefix::Symbol, c::Integer) = uv_error(string(prefix), c)
uv_error(prefix::AbstractString, c::Integer) = c < 0 ? throw(_UVError(prefix, c)) : nothing

## event loop ##

eventloop() = ccall(:jl_global_event_loop, Ptr{Cvoid}, ())

function process_events()
    return ccall(:jl_process_events, Int32, ())
end

function uv_alloc_buf end
function uv_readcb end
function uv_writecb_task end
function uv_return_spawn end
function uv_asynccb end
function uv_timercb end

function reinit_stdio()
    global stdin = init_stdio(ccall(:jl_stdin_stream, Ptr{Cvoid}, ()))
    global stdout = init_stdio(ccall(:jl_stdout_stream, Ptr{Cvoid}, ()))
    global stderr = init_stdio(ccall(:jl_stderr_stream, Ptr{Cvoid}, ()))
    opts = JLOptions()
    if opts.color != 0
        have_color = (opts.color == 1)
        if !isa(stdout, TTY)
            global stdout = IOContext(stdout, :color => have_color)
        end
        if !isa(stderr, TTY)
            global stderr = IOContext(stderr, :color => have_color)
        end
    end
    nothing
end

"""
    stdin

Global variable referring to the standard input stream.
"""
:stdin

"""
    stdout

Global variable referring to the standard out stream.
"""
:stdout

"""
    stderr

Global variable referring to the standard error stream.
"""
:stderr
