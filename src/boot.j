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

# types for the front end

#type Expr
#    head::Symbol
#    args::Array{Any,1}
#    typ::Any
#end

#type LineNumberNode
#    line::Int
#end

#type LabelNode
#    label::Int
#end

#type GotoNode
#    label::Int
#end

#type QuoteNode
#    value
#end

#type TopNode
#    name::Symbol
#    typ
#end

type Nothing; end
const nothing = Nothing()

bitstype 8 Bool

abstract Number
abstract Real     <: Number
abstract Float    <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer

bitstype 32 Float32 <: Float
bitstype 64 Float64 <: Float

bitstype 32 Char <: Unsigned

bitstype 8  Int8   <: Signed
bitstype 8  Uint8  <: Unsigned
bitstype 16 Int16  <: Signed
bitstype 16 Uint16 <: Unsigned
bitstype 32 Int32  <: Signed
bitstype 32 Uint32 <: Unsigned
bitstype 64 Int64  <: Signed
bitstype 64 Uint64 <: Unsigned

if is(Int,Int64)
    typealias Uint Uint64
else
    typealias Uint Uint32
end

int(x) = convert(Int, x)
int(x::Int) = x
uint(x) = convert(Uint, x)
uint(x::Uint) = x

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
abstract DirectIndexString <: String

type ASCIIString <: DirectIndexString
    data::Array{Uint8,1}
    # ASCIIString(a::Array{Uint8,1}) = new(memcpy(a))
end
type UTF8String <: String
    data::Array{Uint8,1}
    # UTF8String(a::Array{Uint8,1}) = new(memcpy(a))
end

typealias ByteString Union(ASCIIString,UTF8String)

abstract Associative

type SymbolNode
    name::Symbol
    typ
end

abstract Exception

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

identity(x) = x

macro thunk(ex); :(()->$ex); end
macro L_str(s); s; end

method_missing(f, args...) = throw(MethodError(f, args))

Array{T}  (::Type{T}, d::(Integer,)) =
    ccall(:jl_alloc_array_1d, Any, (Any,Int), Array{T,1},
          int(d[1]))::Array{T,1}
Array{T}  (::Type{T}, d::(Integer,Integer)) =
    ccall(:jl_alloc_array_2d, Any, (Any,Int,Int), Array{T,2},
          int(d[1]), int(d[2]))::Array{T,2}

Array{T}  (::Type{T}, d::(Int,Int,Int)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,3}, d)::Array{T,3}
Array{T}  (::Type{T}, d::(Integer,Integer,Integer)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,3},
          (int(d[1]),int(d[2]),int(d[3])))::Array{T,3}
Array{T}  (::Type{T}, d::(Int,Int,Int,Int)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,4}, d)::Array{T,4}
Array{T}  (::Type{T}, d::(Integer,Integer,Integer,Integer)) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,4},
          (int(d[1]),int(d[2]),int(d[3]),int(d[4])))::Array{T,4}
Array{T,N}(::Type{T}, d::NTuple{N,Integer}) =
    ccall(:jl_new_array, Any, (Any,Any), Array{T,N},
          convert((Int...), d))::Array{T,N}

Array{T}(::Type{T}, m::Integer) =
    ccall(:jl_alloc_array_1d, Any, (Any,Int), Array{T,1},
          int(m))::Array{T,1}
Array{T}(::Type{T}, m::Int) =
    ccall(:jl_alloc_array_1d, Any, (Any,Int), Array{T,1},
          int(m))::Array{T,1}
Array{T}(::Type{T}, m::Integer,n::Integer) =
    ccall(:jl_alloc_array_2d, Any, (Any,Int,Int), Array{T,2},
          int(m), int(n))::Array{T,2}
Array{T}(::Type{T}, m::Int,n::Int) =
    ccall(:jl_alloc_array_2d, Any, (Any,Int,Int), Array{T,2},
          int(m), int(n))::Array{T,2}

Array{T}(::Type{T}, m::Int,n::Int,o::Int) = Array{T,3}(m,n,o)
Array{T}(::Type{T}, m::Integer, n::Integer, o::Integer) =
    Array{T,3}(int(m),int(n),int(o))
Array{T}(::Type{T}, m::Int,n::Int,o::Int,p::Int) = Array{T,4}(m,n,o,p)
Array{T}(::Type{T}, m::Integer, n::Integer, o::Integer, p::Integer) =
    Array{T,4}(int(m),int(n),int(o),int(p))

Array{N}(T, d::NTuple{N,Integer}) = Array{T,N}(convert((Int...),d))
Array(T, d::Integer...) = Array{T,length(d)}(convert((Int...),d))

function compile_hint(f, args::Tuple)
    if !isgeneric(f)
        return
    end
    args = map(t->isa(t,TypeConstructor) ? t.body : t, args)
    ccall(:jl_get_specialization, Any, (Any, Any), f, args)
    nothing
end
