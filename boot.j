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

#type TagKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#end

#type StructKind <: TagKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#    names::Tuple
#    types::Tuple
#end

#type BitsKind <: TagKind
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

#abstract Tensor{T,N}

#type Array{T,N} <: Tensor{T,N}
#    dims::NTuple{N,Int32}
#end

#type Expr
#    head::Symbol
#    args::Array{Any,1}
#    type::Any
#end

#type LambdaStaticData
#    ast::Expr
#    sparams::Tuple
#    tfunc
#    name::Symbol
#end

#bitstype {32|64} Ptr{T}

abstract Number
abstract Real   <: Number
abstract Int    <: Real
abstract Uint   <: Int
abstract Float  <: Real

bitstype 8  Bool
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

typealias Size Int32
typealias Index Int32

type WeakRef
    value
end

hash(w::WeakRef) = hash(w.value)
isequal(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
isequal(w::WeakRef, v) = isequal(w.value, v)
isequal(w, v::WeakRef) = isequal(w, v.value)

abstract String

type Latin1String <: String
    data::Array{Uint8,1}
end

type UTF8String <: String
    data::Array{Uint8,1}
end

typealias ByteString Union(Latin1String,UTF8String)

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

type DivideByZeroError  <: Exception end
type MemoryError        <: Exception end
type IOError            <: Exception end
type StackOverflowError <: Exception end
type EOFError           <: Exception end

finalizer(o, f::Function) =
    ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)

cstring(str::ByteString) = str

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

macro thunk(ex); :(()->$ex); end
macro L_str(s); s; end
