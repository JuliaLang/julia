# commented-out definitions are implemented in C

#abstract Any <: Any
#abstract Type{T}

#abstract Vararg{T}
#Tuple = (Any...)

#type Symbol
#    #opaque
#end

#type TypeName
#    name::Symbol
#end

#type DataType <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#    names::Tuple
#    types::Tuple
#    ctor
#    instance
#    size::Int32
#    abstract::Bool
#    mutable::Bool
#    pointerfree::Bool
#end

#type UnionType <: Type
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
#     last::Task
#     storage::Any
#     consumers
#     done::Bool
#     runnable::Bool
# end

import Main
import Intrinsics.ccall

export
    # key types
    Any, DataType, Vararg, ANY, NTuple, None, Top,
    Tuple, Type, TypeConstructor, TypeName, TypeVar, Union, UnionType, Void,
    AbstractArray,
    # special objects
    Box, Function, IntrinsicFunction, LambdaStaticData, Method, MethodTable,
    Module, Nothing, Symbol, Task, Array,
    # numeric types
    Bool, FloatingPoint, Float32, Float64, Number, Integer, Int, Int8, Int16,
    Int32, Int64, Int128, Ptr, Real, Signed, Uint, Uint8, Uint16, Uint32,
    Uint64, Uint128, Unsigned,
    # string types
    Char, ASCIIString, ByteString, DirectIndexString, String, UTF8String,
    # errors
    BoundsError, DivideByZeroError, DomainError, Exception,
    InexactError, InterruptException, MemoryError, OverflowError,
    StackOverflowError, UndefRefError,
    # AST representation
    Expr, GotoNode, LabelNode, LineNumberNode, QuoteNode, SymbolNode, TopNode,
    GetfieldNode,
    # object model functions
    apply, arraylen, arrayref, arrayset, arraysize, fieldtype, getfield,
    setfield, yieldto, throw, tuple, tuplelen, tupleref, is, ===, isdefined,
    convert_default, convert_tuple,
    # type reflection
    subtype, typeassert, typeof, apply_type, isa,
    # method reflection
    applicable, invoke, method_exists,
    # constants
    JULIA_HOME, nothing, Main,
    # intrinsics module
    Intrinsics
    #ccall, abs_float, add_float, add_int, and_int, ashr_int,
    #box, bswap_int, checked_fptosi32,
    #checked_fptosi64, checked_fptoui32, checked_fptoui64, checked_sadd,
    #checked_smul, checked_ssub, checked_uadd, checked_umul, checked_usub,
    #nan_dom_err, copysign_float, ctlz_int, ctpop_int, cttz_int,
    #div_float, eq_float, eq_int, eqfsi64, eqfui64, flipsign_int,
    #fpext64, fpiseq32, fpiseq64, fpislt32, fpislt64,
    #fpsiround32, fpsiround64, fptosi32, fptosi64, fptoui32, fptoui64,
    #fptrunc32, fpuiround32, fpuiround64, le_float, lefsi64, lefui64, lesif64,
    #leuif64, lshr_int, lt_float, ltfsi64, ltfui64, ltsif64, ltuif64, mul_float,
    #mul_int, ne_float, ne_int, neg_float, neg_int, not_int, or_int, rem_float,
    #sdiv_int, shl_int, sitofp32, sitofp64, sle_int, slt_int, smod_int,
    #srem_int, sub_float, sub_int, trunc_int, udiv_int, uitofp32, uitofp64,
    #ule_int, ult_int, unbox, urem_int, xor_int, sext_int, zext_int


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
type StackOverflowError <: Exception end
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

immutable ASCIIString <: DirectIndexString
    data::Array{Uint8,1}
end

immutable UTF8String <: String
    data::Array{Uint8,1}
end

typealias ByteString Union(ASCIIString,UTF8String)

include(fname::ByteString) = ccall(:jl_load_, Void, (Any,), fname)
