# commented-out definitions are implemented in C

#type Any <: Any
#type Type{T}

#type ...{T}
#Tuple = (Any...)

#struct Symbol
#    #opaque
#end

#struct TypeName
#    name::Symbol
#end

#struct TagKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#end

#struct StructKind <: TagKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#    names::Tuple
#    types::Tuple
#end

#struct BitsKind <: TagKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#end

#struct FuncKind <: Type
#    from::Type
#    to::Type
#end

#struct UnionKind <: Type
#    types::Tuple
#end

#None = Union()

#struct TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#struct TypeConstructor
#    parameters::Tuple
#    body
#end

#type Tensor{T,N}

#struct Array{T,N} <: Tensor{T,N}
#    dims::NTuple{N,Int32}
#end

#struct Expr
#    head::Symbol
#    args::Array{Any,1}
#    type::Any
#end

#struct LambdaStaticData
#    ast::Expr
#    sparams::Tuple
#    tfunc
#    name::Symbol
#end

#bitstype {32|64} Ptr{T}

type Number
type Real   <: Number
type Int    <: Real
type Uint   <: Int
type Float  <: Real

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

type String

struct Latin1String <: String
    data::Array{Uint8,1}
end

struct UTF8String <: String
    data::Array{Uint8,1}
end

typealias ByteString Union(Latin1String,UTF8String)

type Exception

struct ErrorException <: Exception
    msg::String
end

struct SystemError <: Exception
    prefix::String
    errnum::Int32
    SystemError(p::String, e::Int) = new(p, int32(e))
    SystemError(p::String) = new(p, errno())
end

struct TypeError <: Exception
    func::Symbol
    context::String
    expected::Type
    got
end

struct ParseError <: Exception
    msg::String
end

struct ArgumentError <: Exception
    msg::String
end

struct BoundsError <: Exception
end

struct UnboundError <: Exception
    var::Symbol
end

struct KeyError <: Exception
    key
end

struct LoadError <: Exception
    file::String
    line::Int32
    error
end

struct DivideByZeroError  <: Exception end
struct MemoryError        <: Exception end
struct IOError            <: Exception end
struct StackOverflowError <: Exception end
struct EOFError           <: Exception end

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
