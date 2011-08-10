# commented-out definitions are implemented in C

#abstract Any <: Any
#abstract Type{T}

#abstract ...{T}
#Tuple = (Any...)

#type Symbol
#    #opaque
#end

#type TypeName
#    name::Symbol
#end

#type AbstractKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#end

#type CompositeKind <: AbstractKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#    names::Tuple
#    types::Tuple
#end

#type BitsKind <: AbstractKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#end

#type FuncKind <: Type
#    from::Type
#    to::Type
#end

#type UnionKind <: Type
#    types::Tuple
#end

#None = Union()

#type TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#type TypeConstructor
#    parameters::Tuple
#    body
#end

#abstract AbstractArray{T,N}

#type Array{T,N} <: AbstractArray{T,N}
#    dims::NTuple{N,Int32}
#end

#type Expr
#    head::Symbol
#    args::Array{Any,1}
#    type::Any
#end

#type SymbolNode
#    name::Symbol
#    type
#end

#type LineNumberNode
#    line::Long
#end

#type LabelNode
#    label::Long
#end

#type LambdaStaticData
#    ast::Expr
#    sparams::Tuple
#    tfunc
#    name::Symbol
#end

#type Box{T}
#    contents::T
#end

#bitstype {32|64} Ptr{T}

type Nothing; end
nothing = Nothing()

bitstype 8 Bool

abstract Number
abstract Real  <: Number
abstract Int   <: Real
abstract Uint  <: Int
abstract Float <: Real

bitstype 32 Char <: Uint

bitstype 8  Int8   <: Int
bitstype 8  Uint8  <: Uint
bitstype 16 Int16  <: Int
bitstype 16 Uint16 <: Uint
bitstype 32 Int32  <: Int
bitstype 32 Uint32 <: Uint
bitstype 64 Int64  <: Int
bitstype 64 Uint64 <: Uint

bitstype 32 Float32 <: Float
bitstype 64 Float64 <: Float

typealias Index Size
if is(Size,Int64)
    typealias Long Int64
    typealias Ulong Uint64
else
    typealias Long Int32
    typealias Ulong Uint32
end

long(x) = convert(Long, x)
long(x::Long) = x
ulong(x) = convert(Ulong, x)
ulong(x::Ulong) = x

# function version of field assignment
setfield(s, f, v) = (s.(f) = v)

type WeakRef
    value
end

hash(w::WeakRef) = hash(w.value)
isequal(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
isequal(w::WeakRef, v) = isequal(w.value, v)
isequal(w, v::WeakRef) = isequal(w, v.value)

abstract String

type ASCIIString <: String
    data::Array{Uint8,1}
    # ASCIIString(a::Array{Uint8,1}) = new(memcpy(a))
end
type UTF8String <: String
    data::Array{Uint8,1}
    # UTF8String(a::Array{Uint8,1}) = new(memcpy(a))
end

typealias ByteString Union(ASCIIString,UTF8String)

abstract Exception

type ErrorException <: Exception
    msg::String
end

type SystemError <: Exception
    prefix::String
    errnum::Int32
    SystemError(p::String, e::Int) = new(p, int32(e))
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

type BoundsError <: Exception
end

type UnboundError <: Exception
    var::Symbol
end

type KeyError <: Exception
    key
end

type LoadError <: Exception
    file::String
    line::Int32
    error
end

type MethodError <: Exception
    f
    args
end

type DivideByZeroError  <: Exception end
type MemoryError        <: Exception end
type IOError            <: Exception end
type StackOverflowError <: Exception end
type EOFError           <: Exception end
type UndefRefError      <: Exception end
type InterruptException <: Exception end

type UnionTooComplexError <: Exception
    types::Tuple
end

type BackTrace <: Exception
    e
    trace::Array{Any,1}
end

finalizer(o, f::Function) =
    ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)

gc() = ccall(:jl_gc_collect, Void, ())

cstring(str::ByteString) = str

# return an integer such that uid(x)==uid(y) iff is(x,y)
uid(x) = ccall(:jl_uid, Ulong, (Any,), x)

dlsym(hnd, s::String) =
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, cstring(s))

dlsym(hnd, s::Symbol) =
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}),
          hnd, convert(Ptr{Uint8}, s))

dlopen(fname::String) =
    ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), cstring(fname))

load(fname::String) =
    ccall(:jl_load, Void, (Ptr{Uint8},), cstring(fname))

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    n = 0
    for x = xs
        n += length(x)
    end
    out = Array{Any,1}((n,))
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

identity(x) = x

macro thunk(ex); :(()->$ex); end
macro L_str(s); s; end

method_missing(f, args...) = throw(MethodError(f, args))

Array{T}  (::Type{T}, d::(Size,)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,1}, d)::Array{T,1}
Array{T}  (::Type{T}, d::(Size,Size)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,2}, d)::Array{T,2}
Array{T}  (::Type{T}, d::(Size,Size,Size)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,3}, d)::Array{T,3}
Array{T}  (::Type{T}, d::(Size,Size,Size,Size)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,4}, d)::Array{T,4}
Array{T,N}(::Type{T}, d::NTuple{N,Size}) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,N}, d)::Array{T,N}

Array{T}(::Type{T}, m::Size) =
    ccall(:jl_alloc_array_1d, Any, (Any,Size), Array{T,1},
          long(m))::Array{T,1}
Array{T}(::Type{T}, m::Size,n::Size) =
    ccall(:jl_alloc_array_2d, Any, (Any,Size,Size), Array{T,2},
          long(m), long(n))::Array{T,2}
Array{T}(::Type{T}, m::Size,n::Size,o::Size)         = Array(T, (m,n,o))
Array{T}(::Type{T}, m::Size,n::Size,o::Size,p::Size) = Array(T, (m,n,o,p))

Array{N}(T, d::NTuple{N,Size})                       = Array{T,N}(d)
Array(T, d::Size...)                                 = Array(T, d)

function compile_hint(f, args::Tuple)
    if !isgeneric(f)
        return
    end
    args = map(t->isa(t,TypeConstructor) ? t.body : t, args)
    ccall(:jl_get_specialization, Any, (Any, Any), f, args)
    nothing
end
