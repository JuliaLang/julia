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

#type Module
#    name::Symbol
#end

#type LambdaStaticData
#    ast::Expr
#    sparams::Tuple
#    tfunc
#    name::Symbol
#    specializations
#    inferred
#    file::Symbol
#    line::Int
#    module::Module
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
#end

# type Task
#     parent::Task
#     tls::Any
#     done::Bool
# end

type Nothing; end
const nothing = Nothing()

const (===) = is

abstract Number
abstract Real     <: Number
abstract Float    <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer

bitstype 32 Float32 <: Float
bitstype 64 Float64 <: Float

bitstype 8  Bool <: Integer
bitstype 32 Char <: Integer

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
    const unboxwd = unbox64
    const boxsint = boxsi64
    const boxuint = boxui64
else
    typealias Uint Uint32
    const unboxwd = unbox32
    const boxsint = boxsi32
    const boxuint = boxui32
end

abstract Exception

type BoundsError        <: Exception end
type DivideByZeroError  <: Exception end
type DomainError        <: Exception end
type OverflowError      <: Exception end
type InexactError       <: Exception end
type MemoryError        <: Exception end
type IOError            <: Exception end
type StackOverflowError <: Exception end
type EOFError           <: Exception end
type UndefRefError      <: Exception end
type InterruptException <: Exception end

abstract String
abstract DirectIndexString <: String

# simple convert for use by constructors of types in Core
convert(T, x) = convert_default(T, x, convert)

type SymbolNode
    name::Symbol
    typ
    SymbolNode(name::Symbol, t::ANY) = new(name, t)
end

type WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref, WeakRef, (Any,), v)
end

type ASCIIString <: DirectIndexString
    data::Array{Uint8,1}
end

type UTF8String <: String
    data::Array{Uint8,1}
end

typealias ByteString Union(ASCIIString,UTF8String)

include(fname::ByteString) = ccall(:jl_load_, Void, (Any,), fname)
