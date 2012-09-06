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

#type CompositeKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#    names::Tuple
#    types::Tuple
#end

#type BitsKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
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

import Root

export ..., ANY, ASCIIString, AbstractArray, AbstractKind, Any, Array,
    BitsKind, Bool, BoundsError, Box, ByteString, Char, CompositeKind,
    Core, Root, DirectIndexString, DivideByZeroError, DomainError, EOFError,
    Exception, Expr, FloatingPoint, Float32, Float64, Function, GotoNode, IOError,
    InexactError, Integer, Int, Int8, Int16, Int32, Int64, Int128,
    InterruptException,
    IntrinsicFunction, LabelNode, LambdaStaticData, LineNumberNode,
    MemoryError, Method, MethodTable, Module, NTuple, None, Nothing, Number,
    OverflowError, Ptr, QuoteNode, Real, Signed, StackOverflowError, String,
    Symbol, SymbolNode, Task, Top, TopNode, Tuple, Type, TypeConstructor,
    TypeName, TypeVar, UTF8String, Uint, Uint8, Uint16, Uint32, Uint64, Uint128,
    Undef, UndefRefError, Union, UnionKind, Unsigned, Void, WeakRef,
    GetfieldNode,
    # functions
    setfield, applicable, apply, apply_type, arraylen, arrayref, arrayset,
    arraysize, convert_default, convert_tuple, eval, fieldtype, getfield,
    include, invoke, is, ===, isa, isbound, method_exists,
    subtype, throw, tuple, tuplelen, tupleref, typeassert, typeof, yieldto,
    # constants
    JULIA_HOME, nothing,
    # intrinsic functions
    ccall, abs_float, add_float, add_int, and_int, ashr_int,
    box, bswap_int, checked_fptosi32,
    checked_fptosi64, checked_fptoui32, checked_fptoui64, checked_sadd,
    checked_smul, checked_ssub, checked_uadd, checked_umul, checked_usub,
    copysign_float, ctlz_int, ctpop_int, cttz_int,
    div_float, eq_float, eq_int, eqfsi64, eqfui64, flipsign_int,
    fpext64, fpiseq32, fpiseq64, fpislt32, fpislt64,
    fpsiround32, fpsiround64, fptosi32, fptosi64, fptoui32, fptoui64,
    fptrunc32, fpuiround32, fpuiround64, le_float, lefsi64, lefui64, lesif64,
    leuif64, lshr_int, lt_float, ltfsi64, ltfui64, ltsif64, ltuif64, mul_float,
    mul_int, ne_float, ne_int, neg_float, neg_int, not_int, or_int, rem_float,
    sdiv_int, sext16, sext32, sext64, shl_int, sitofp32, sitofp64, sle_int,
    slt_int, smod_int, srem_int, sub_float, sub_int, trunc16, trunc32,
    trunc64, trunc8, trunc_int, udiv_int, uitofp32, uitofp64, ule_int, ult_int,
    unbox, urem_int, xor_int, zext16, zext32, zext64, sext_int, zext_int


type Nothing; end
const nothing = Nothing()

const (===) = is

abstract Number
abstract Real     <: Number
abstract FloatingPoint <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer

bitstype 32 Float32 <: FloatingPoint
bitstype 64 Float64 <: FloatingPoint

bitstype 8  Bool <: Integer
bitstype 32 Char <: Integer

bitstype 8   Int8    <: Signed
bitstype 8   Uint8   <: Unsigned
bitstype 16  Int16   <: Signed
bitstype 16  Uint16  <: Unsigned
bitstype 32  Int32   <: Signed
bitstype 32  Uint32  <: Unsigned
bitstype 64  Int64   <: Signed
bitstype 64  Uint64  <: Unsigned
bitstype 128 Int128  <: Signed
bitstype 128 Uint128 <: Unsigned

if is(Int,Int64)
    typealias Uint Uint64
else
    typealias Uint Uint32
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

type GetfieldNode
    value
    name::Symbol
    typ
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
