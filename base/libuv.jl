# This file is a part of Julia. License is MIT: https://julialang.org/license

# Core definitions for interacting with the libuv library from Julia

include(string(Base.BUILDROOT, "uv_constants.jl"))  # include($BUILDROOT/base/uv_constants.jl)

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

uv_handle_data(handle) = ccall(:uv_handle_get_data, Ptr{Cvoid}, (Ptr{Cvoid},), handle)
uv_req_data(handle) = ccall(:uv_req_get_data, Ptr{Cvoid}, (Ptr{Cvoid},), handle)
uv_req_set_data(req, data) = ccall(:uv_req_set_data, Cvoid, (Ptr{Cvoid}, Any), req, data)
uv_req_set_data(req, data::Ptr{Cvoid}) = ccall(:uv_handle_set_data, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), req, data)

macro handle_as(hand, typ)
    return quote
        local data = uv_handle_data($(esc(hand)))
        data == C_NULL && return
        unsafe_pointer_to_objref(data)::($(esc(typ)))
    end
end

function _uv_hook_close end

function associate_julia_struct(handle::Ptr{Cvoid}, jlobj::T) where T
    # This `cfunction` is not used anywhere, but it triggers compilation of this
    # MethodInstance for `--trim` so that it will be available when dispatched to
    # by `jl_uv_call_close_callback()`
    _ = @cfunction(Base._uv_hook_close, Cvoid, (Ref{T},))
    ccall(:jl_uv_associate_julia_struct, Cvoid, (Ptr{Cvoid}, Any), handle, jlobj)
end
disassociate_julia_struct(uv) = disassociate_julia_struct(uv.handle)
disassociate_julia_struct(handle::Ptr{Cvoid}) =
    handle != C_NULL && ccall(:jl_uv_disassociate_julia_struct, Cvoid, (Ptr{Cvoid},), handle)

iolock_begin() = ccall(:jl_iolock_begin, Cvoid, ())
iolock_end() = ccall(:jl_iolock_end, Cvoid, ())

# A dict of all libuv handles that are being waited on somewhere in the system
# and should thus not be garbage collected
const uvhandles = IdDict()
const preserve_handle_lock = Threads.SpinLock()
@nospecializeinfer function preserve_handle(@nospecialize(x))
    lock(preserve_handle_lock)
    v = get(uvhandles, x, 0)::Int
    uvhandles[x] = v + 1
    unlock(preserve_handle_lock)
    nothing
end
@nospecializeinfer function unpreserve_handle(@nospecialize(x))
    lock(preserve_handle_lock)
    v = get(uvhandles, x, 0)::Int
    if v == 0
        unlock(preserve_handle_lock)
        error("unbalanced call to unpreserve_handle for $(typeof(x))")
    elseif v == 1
        pop!(uvhandles, x)
    else
        uvhandles[x] = v - 1
    end
    unlock(preserve_handle_lock)
    nothing
end

## Libuv error handling ##

struct IOError <: Exception
    msg::String
    code::Int32
    IOError(msg::AbstractString, code::Integer) = new(msg, code)
end

function showerror(io::IO, e::IOError)
    print(io, "IOError: ", e.msg)
    if e.code == UV_ENOENT && '~' in e.msg
        print(io, "\nMany shells expand '~' to the home directory in unquoted strings. To replicate this behavior, call",
                  " `expanduser` to expand the '~' character to the user’s home directory.")
    end
end

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
uv_error(prefix::AbstractString, c::Integer) = c < 0 ? _uv_error(prefix, c) : nothing
_uv_error(prefix::AbstractString, c::Integer) = throw(_UVError(prefix, c))

## event loop ##

eventloop() = ccall(:jl_global_event_loop, Ptr{Cvoid}, ())

function uv_unref(h::Ptr{Cvoid})
    iolock_begin()
    ccall(:uv_unref, Cvoid, (Ptr{Cvoid},), h)
    iolock_end()
end

function uv_ref(h::Ptr{Cvoid})
    iolock_begin()
    ccall(:uv_ref, Cvoid, (Ptr{Cvoid},), h)
    iolock_end()
end

function process_events()
    return ccall(:jl_process_events, Int32, ())
end

function uv_alloc_buf end
function uv_readcb end
function uv_writecb_task end
function uv_shutdowncb_task end
function uv_return_spawn end
function uv_asynccb end
function uv_timercb end

reinit_stdio() = _reinit_stdio()
# we need this so it can be called by codegen to print errors, even after
# reinit_stdio has been redefined by the juliac build script.
function _reinit_stdio()
    global stdin = init_stdio(ccall(:jl_stdin_stream, Ptr{Cvoid}, ()))::IO
    global stdout = init_stdio(ccall(:jl_stdout_stream, Ptr{Cvoid}, ()))::IO
    global stderr = init_stdio(ccall(:jl_stderr_stream, Ptr{Cvoid}, ()))::IO
    opts = JLOptions()
    color = colored_text(opts)
    if !isnothing(color)
        if !isa(stdout, TTY)
            global stdout = IOContext(stdout, :color => color::Bool)
        end
        if !isa(stderr, TTY)
            global stderr = IOContext(stderr, :color => color::Bool)
        end
    end
    nothing
end

"""
    stdin::IO

Global variable referring to the standard input stream.
"""
:stdin

"""
    stdout::IO

Global variable referring to the standard out stream.
"""
:stdout

"""
    stderr::IO

Global variable referring to the standard error stream.
"""
:stderr
