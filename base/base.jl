# important core definitions

convert(T, x)               = convert_default(T, x, convert)
convert(T::Tuple, x::Tuple) = convert_tuple(T, x, convert)

ptr_arg_convert{T}(::Type{Ptr{T}}, x) = convert(T, x)

type ErrorException <: Exception
    msg::String
end

type SystemError <: Exception
    prefix::String
    errnum::Int32
    SystemError(p::String, e::Integer) = new(p, int32(e))
    SystemError(p::String) = new(p, errno())
end

type TypeError <: Exception
    func::Symbol
    context::String
    expected::Type
    got
end

type ParseError <: Exception
    msg::String
end

type ArgumentError <: Exception
    msg::String
end

type UnboundError <: Exception
    var::Symbol
end

type KeyError <: Exception
    key
end

type LoadError <: Exception
    file::String
    line::Int
    error
end

type MethodError <: Exception
    f
    args
end

type BackTrace <: Exception
    e
    trace::Array{Any,1}
end

show(io, bt::BackTrace) = show(io,bt.e)

method_missing(f, args...) = throw(MethodError(f, args))

ccall(:jl_get_system_hooks, Void, ())


int(x) = convert(Int, x)
int(x::Int) = x
uint(x) = convert(Uint, x)
uint(x::Uint) = x

# function version of field assignment
setfield(s, f, v) = (s.(f) = v)

hash(w::WeakRef) = hash(w.value)
isequal(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
isequal(w::WeakRef, v) = isequal(w.value, v)
isequal(w, v::WeakRef) = isequal(w, v.value)

finalizer(o, f::Function) = ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)

gc() = ccall(:jl_gc_collect, Void, ())
gc_enable() = ccall(:jl_gc_enable, Void, ())
gc_disable() = ccall(:jl_gc_disable, Void, ())

current_task() = ccall(:jl_get_current_task, Task, ())
istaskdone(t::Task) = t.done

cstring(str::ByteString) = str

# return an integer such that uid(x)==uid(y) iff is(x,y)
uid(x) = ccall(:jl_uid, Uint, (Any,), x)

dlsym(hnd, s::String) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym(hnd, s::Symbol) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlopen(s::String) = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), s)

identity(x) = x

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    n = 0
    for x = xs
        n += length(x)
    end
    out = Array(Any, n)
    i = 1
    for x = xs
        for y = x
            arrayset(out, i, y)
            i += 1
        end
    end
    out
end

append(xs...) = append_any(xs...)

macro thunk(ex); :(()->$ex); end
macro L_str(s); s; end

function compile_hint(f, args::Tuple)
    if isgeneric(f)
        ccall(:jl_compile_hint, Void, (Any, Any), f, args)
    end
end

# we share Array with Core so we can add definitions to it
const Array = eval(Core, :Array)

Array{T,N}(::Type{T}, d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)
Array{N}(T, d::NTuple{N,Int}) =
    (AT = Array{T,N};
     ccall(:jl_new_array, Any, (Any,Any), AT, d)::AT)

Array{T}(::Type{T}, m::Int) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Int,n::Int) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m,n)
Array{T}(::Type{T}, m::Int,n::Int,o::Int) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m,n,o)

Array(T, d::Int...) = Array(T, d)
Array(T, d::Integer...) = Array(T, convert((Int...), d))

Array{T}(::Type{T}, m::Integer) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Integer,n::Integer) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m, n)
Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m, n, o)
