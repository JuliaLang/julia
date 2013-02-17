# important core definitions

using Core.Intrinsics

import Core.Array  # to add methods

convert(T, x)               = convert_default(T, x, convert)
convert(T::Tuple, x::Tuple) = convert_tuple(T, x, convert)

ptr_arg_convert{T}(::Type{Ptr{T}}, x) = convert(T, x)
ptr_arg_convert(::Type{Ptr{Void}}, x) = x

# conversion used by ccall
cconvert(T, x) = convert(T, x)
# use the code in ccall.cpp to safely allocate temporary pointer arrays
cconvert{T}(::Type{Ptr{Ptr{T}}}, a::Array) = a
# TODO: for some reason this causes a strange type inference problem
#cconvert(::Type{Ptr{Uint8}}, s::String) = bytestring(s)

abstract IO

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

#type UnboundError <: Exception
#    var::Symbol
#end

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

type EOFError <: Exception end

type WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref, Any, (Any,), v)::WeakRef
end

ccall(:jl_get_system_hooks, Void, ())


int(x) = convert(Int, x)
int(x::Int) = x
uint(x) = convert(Uint, x)
uint(x::Uint) = x

# reflection

names(m::Module, all::Bool) = ccall(:jl_module_names, Array{Symbol,1}, (Any,Int32), m, all)
names(m::Module) = names(m,false)
module_name(m::Module) = ccall(:jl_module_name, Any, (Any,), m)::Symbol
module_parent(m::Module) = ccall(:jl_module_parent, Any, (Any,), m)::Module
function names(v)
    if typeof(v) === CompositeKind
        return v.names
    elseif typeof(typeof(v)) === CompositeKind
        return typeof(v).names
    else
        error("cannot call names() on a non-composite type")
    end
end

# index colon
type Colon
end
const (:) = Colon()

hash(w::WeakRef) = hash(w.value)
isequal(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
isequal(w::WeakRef, v) = isequal(w.value, v)
isequal(w, v::WeakRef) = isequal(w, v.value)

finalizer(o, f::Function) = ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)

gc() = ccall(:jl_gc_collect, Void, ())
gc_enable() = ccall(:jl_gc_enable, Void, ())
gc_disable() = ccall(:jl_gc_disable, Void, ())

bytestring(str::ByteString) = str

# return an integer such that object_id(x)==object_id(y) if is(x,y)
object_id(x::ANY) = ccall(:jl_object_id, Uint, (Any,), x)

const isimmutable = x->(isa(x,Tuple) || isa(x,Symbol) ||
                        isa(typeof(x),BitsKind))

dlsym(hnd, s::String) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym(hnd, s::Symbol) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym_e(hnd, s::Union(Symbol,String)) = ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlopen(s::String) = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), s)
dlclose(p::Ptr) = ccall(:uv_dlclose,Void,(Ptr{Void},),p)

cfunction(f::Function, r, a) =
    ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any), f, r, a)

identity(x) = x

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    out = Array(Any, 4)
    l = 4
    i = 1
    for x in xs
        for y in x
            if i > l
                ccall(:jl_array_grow_end, Void, (Any, Uint), out, 16)
                l += 16
            end
            arrayset(out, y, i)
            i += 1
        end
    end
    ccall(:jl_array_del_end, Void, (Any, Uint), out, l-i+1)
    out
end

macro thunk(ex); :(()->$(esc(ex))); end
macro L_str(s); s; end

function compile_hint(f, args::Tuple)
    if isgeneric(f)
        ccall(:jl_compile_hint, Void, (Any, Any), f, args)
    end
end

# NOTE: Base shares Array with Core so we can add definitions to it

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
