# This file is a part of Julia. License is MIT: http://julialang.org/license

# Core definitions for interacting with the libuv library from Julia

include(string(length(Core.ARGS)>=2?Core.ARGS[2]:"","uv_constants.jl"))  # include($BUILDROOT/base/uv_constants.jl)

# convert UV handle data to julia object, checking for null
function uv_sizeof_handle(handle)
    if !(UV_UNKNOWN_HANDLE < handle < UV_HANDLE_TYPE_MAX)
        throw(DomainError())
    end
    ccall(:uv_handle_size,Csize_t,(Int32,),handle)
end

function uv_sizeof_req(req)
    if !(UV_UNKNOWN_REQ < req < UV_REQ_TYPE_MAX)
        throw(DomainError())
    end
    ccall(:uv_req_size,Csize_t,(Int32,),req)
end

for h in uv_handle_types
@eval const $(Symbol("_sizeof_",lowercase(string(h)))) = uv_sizeof_handle($h)
end
for r in uv_req_types
@eval const $(Symbol("_sizeof_",lowercase(string(r)))) = uv_sizeof_req($r)
end

uv_handle_data(handle) = ccall(:jl_uv_handle_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_data(handle) = ccall(:jl_uv_req_data,Ptr{Void},(Ptr{Void},),handle)
uv_req_set_data(req,data) = ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Any),req,data)
uv_req_set_data(req,data::Ptr{Void}) = ccall(:jl_uv_req_set_data,Void,(Ptr{Void},Ptr{Void}),req,data)

macro handle_as(hand, typ)
    quote
        data = uv_handle_data($(esc(hand)))
        data == C_NULL && return
        unsafe_pointer_to_objref(data)::($(esc(typ)))
    end
end

associate_julia_struct(handle::Ptr{Void}, jlobj::ANY) =
    ccall(:jl_uv_associate_julia_struct, Void, (Ptr{Void}, Any), handle, jlobj)
disassociate_julia_struct(uv) = disassociate_julia_struct(uv.handle)
disassociate_julia_struct(handle::Ptr{Void}) =
    handle != C_NULL && ccall(:jl_uv_disassociate_julia_struct, Void, (Ptr{Void},), handle)

# A dict of all libuv handles that are being waited on somewhere in the system
# and should thus not be garbage collected
const uvhandles = ObjectIdDict()
preserve_handle(x) = uvhandles[x] = get(uvhandles,x,0)::Int+1
preserve_handle_new(x) = uvhandles[x] = 1
unpreserve_handle(x) = (v = uvhandles[x]::Int; v == 1 ? pop!(uvhandles,x) : (uvhandles[x] = v-1); nothing)

## Libuv error handling ##

type UVError <: Exception
    prefix::AbstractString
    code::Int32
    UVError(p::AbstractString,code::Integer)=new(p,code)
end

struverror(err::Int32) = unsafe_string(ccall(:uv_strerror,Cstring,(Int32,),err))
struverror(err::UVError) = struverror(err.code)
uverrorname(err::Int32) = unsafe_string(ccall(:uv_err_name,Cstring,(Int32,),err))
uverrorname(err::UVError) = uverrorname(err.code)

uv_error(prefix::Symbol, c::Integer) = uv_error(string(prefix),c)
uv_error(prefix::AbstractString, c::Integer) = c < 0 ? throw(UVError(prefix,c)) : nothing
show(io::IO, e::UVError) = print(io, e.prefix*": "*struverror(e)*" ("*uverrorname(e)*")")

## event loop ##

eventloop() = uv_eventloop::Ptr{Void}
#mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},()) # this would probably be fine, but is nowhere supported

function run_event_loop()
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),eventloop())
end
function process_events(block::Bool)
    loop = eventloop()
    if block
        return ccall(:jl_run_once,Int32,(Ptr{Void},),loop)
    else
        return ccall(:jl_process_events,Int32,(Ptr{Void},),loop)
    end
end

function reinit_stdio()
    global uv_jl_alloc_buf     = cfunction(uv_alloc_buf, Void, (Ptr{Void}, Csize_t, Ptr{Void}))
    global uv_jl_readcb        = cfunction(uv_readcb, Void, (Ptr{Void}, Cssize_t, Ptr{Void}))
    global uv_jl_connectioncb  = cfunction(uv_connectioncb, Void, (Ptr{Void}, Cint))
    global uv_jl_connectcb     = cfunction(uv_connectcb, Void, (Ptr{Void}, Cint))
    global uv_jl_writecb_task  = cfunction(uv_writecb_task, Void, (Ptr{Void}, Cint))
    global uv_jl_getaddrinfocb = cfunction(uv_getaddrinfocb, Void, (Ptr{Void},Cint,Ptr{Void}))
    global uv_jl_recvcb        = cfunction(uv_recvcb, Void, (Ptr{Void}, Cssize_t, Ptr{Void}, Ptr{Void}, Cuint))
    global uv_jl_sendcb        = cfunction(uv_sendcb, Void, (Ptr{Void}, Cint))
    global uv_jl_return_spawn  = cfunction(uv_return_spawn, Void, (Ptr{Void}, Int64, Int32))
    global uv_jl_asynccb       = cfunction(uv_asynccb, Void, (Ptr{Void},))
    global uv_jl_timercb       = cfunction(uv_timercb, Void, (Ptr{Void},))

    global uv_eventloop = ccall(:jl_global_event_loop, Ptr{Void}, ())
    global STDIN = init_stdio(ccall(:jl_stdin_stream, Ptr{Void}, ()))
    global STDOUT = init_stdio(ccall(:jl_stdout_stream, Ptr{Void}, ()))
    global STDERR = init_stdio(ccall(:jl_stderr_stream, Ptr{Void}, ()))
end

"""
    STDIN

Global variable referring to the standard input stream.
"""
:STDIN

"""
    STDOUT

Global variable referring to the standard out stream.
"""
:STDOUT

"""
    STDERR

Global variable referring to the standard error stream.
"""
:STDERR
