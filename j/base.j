# important core definitions

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

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = t.done

cstring(str::ByteString) = str

# return an integer such that uid(x)==uid(y) iff is(x,y)
uid(x) = ccall(:jl_uid, Uint, (Any,), x)

dlsym(hnd, s::String) =
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, cstring(s))

dlsym(hnd, s::Symbol) =
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}),
          hnd, convert(Ptr{Uint8}, s))

dlopen(fname::String) =
    ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), cstring(fname))

identity(x) = x

macro thunk(ex); :(()->$ex); end
macro L_str(s); s; end

function compile_hint(f, args::Tuple)
    if !isgeneric(f)
        return
    end
    args = map(t->isa(t,TypeConstructor) ? t.body : t, args)
    ccall(:jl_get_specialization, Any, (Any, Any), f, args)
    nothing
end

Array{T}  (::Type{T}, d::(Integer,)) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1},
          int(d[1]))
Array{T}  (::Type{T}, d::(Integer,Integer)) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2},
          int(d[1]), int(d[2]))

Array{T}  (::Type{T}, d::(Int,Int,Int)) =
    ccall(:jl_new_array, Array{T,3}, (Any,Any), Array{T,3}, d)
Array{T}  (::Type{T}, d::(Integer,Integer,Integer)) =
    ccall(:jl_new_arrayv, Array{T,3}, (Any,Int...), Array{T,3},
          int(d[1]), int(d[2]), int(d[3]))
Array{T}  (::Type{T}, d::(Int,Int,Int,Int)) =
    ccall(:jl_new_array, Array{T,4}, (Any,Any), Array{T,4}, d)
Array{T}  (::Type{T}, d::(Integer,Integer,Integer,Integer)) =
    ccall(:jl_new_arrayv, Array{T,4}, (Any,Int...), Array{T,4},
          int(d[1]), int(d[2]), int(d[3]), int(d[4]))

Array{T,N}(::Type{T}, d::NTuple{N,Integer}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N},
          convert((Int...), d))
Array{N}(T, d::NTuple{N,Integer}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N},
          convert((Int...), d))

Array{T}(::Type{T}, m::Integer) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1},
          int(m))
Array{T}(::Type{T}, m::Integer,n::Integer) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2},
          int(m), int(n))

Array{T}(::Type{T}, m::Integer, n::Integer, o::Integer) =
    ccall(:jl_new_arrayv, Array{T,3}, (Any,Int...), Array{T,3},
          int(m), int(n), int(o))
Array{T}(::Type{T}, m::Integer, n::Integer, o::Integer, p::Integer) =
    ccall(:jl_new_arrayv, Array{T,4}, (Any,Int...), Array{T,4},
          int(m), int(n), int(o), int(p))

Array(T, d::Integer...) = Array(T, d)
