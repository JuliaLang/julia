# important core definitions

using Core: Intrinsics, arraylen, arrayref, arrayset, arraysize,
            tuplelen, tupleref, convert_default, kwcall,
            typeassert, apply_type

import Core.Array  # to add methods

const NonTupleType = Union(DataType,UnionType,TypeConstructor)

typealias Callable Union(Function,DataType)

convert(T, x) = convert_default(T, x, convert)

convert(::(), ::()) = ()
convert(::Type{Tuple}, x::Tuple) = x

argtail(x, rest...) = rest
tupletail(x::Tuple) = argtail(x...)

convert(T::(Any, Any...), x::(Any, Any...)) =
    tuple(convert(T[1],x[1]), convert(tupletail(T), tupletail(x))...)

convert{T}(::Type{(T...)}, x::Tuple) = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)


ptr_arg_convert{T}(::Type{Ptr{T}}, x) = convert(T, x)
ptr_arg_convert(::Type{Ptr{Void}}, x) = x

# conversion used by ccall
cconvert(T, x) = convert(T, x)
# use the code in ccall.cpp to safely allocate temporary pointer arrays
cconvert{T}(::Type{Ptr{Ptr{T}}}, a::Array) = a
# convert strings to ByteString to pass as pointers
cconvert{P<:Union(Int8,Uint8)}(::Type{Ptr{P}}, s::String) = bytestring(s)

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

type DimensionMismatch <: Exception
    name::ASCIIString
end

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

# index colon
type Colon
end
const (:) = Colon()

hash(w::WeakRef) = hash(w.value)
isequal(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
isequal(w::WeakRef, v) = isequal(w.value, v)
isequal(w, v::WeakRef) = isequal(w, v.value)

function finalizer(o::ANY, f::Union(Function,Ptr))
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)
end

gc() = ccall(:jl_gc_collect, Void, ())
gc_enable() = ccall(:jl_gc_enable, Void, ())
gc_disable() = ccall(:jl_gc_disable, Void, ())

bytestring(str::ByteString) = str

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

function precompile(f, args::Tuple)
    if isgeneric(f)
        ccall(:jl_compile_hint, Void, (Any, Any), f, args)
    end
end

esc(e::ANY) = Expr(:escape, e)

macro boundscheck(yesno,blk)
    # hack: use this syntax since it avoids introducing line numbers
    :($(Expr(:boundscheck,yesno));
      $(esc(blk));
      $(Expr(:boundscheck,:pop)))
end

macro inbounds(blk)
    :(@boundscheck false $(esc(blk)))
end

# NOTE: Base shares Array with Core so we can add definitions to it

Array{T,N}(::Type{T}, d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)

Array{T}(::Type{T}, m::Int) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Int,n::Int) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m,n)
Array{T}(::Type{T}, m::Int,n::Int,o::Int) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m,n,o)

Array(T::Type, d::Int...) = Array(T, d)
Array(T::Type, d::Integer...) = Array(T, convert((Int...), d))

Array{T}(::Type{T}, m::Integer) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Integer,n::Integer) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m, n)
Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m, n, o)
