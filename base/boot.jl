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

#type TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#type TypeConstructor
#    parameters::Tuple
#    body
#end

#immutable Void
#end
#const nothing = Void()

#abstract AbstractArray{T,N}
#abstract DenseArray{T,N} <: AbstractArray{T,N}

#type Array{T,N} <: DenseArray{T,N}
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

#immutable LineNumberNode
#    line::Int
#end

#immutable LabelNode
#    label::Int
#end

#immutable GotoNode
#    label::Int
#end

#immutable QuoteNode
#    value
#end

#immutable TopNode
#    name::Symbol
#end

# type Task
#     parent::Task
#     last::Task
#     storage::Any
#     consumers
#     started::Bool
#     done::Bool
#     runnable::Bool
# end

import Core.Intrinsics.ccall

export
    # key types
    Any, DataType, Vararg, ANY, NTuple, Top,
    Tuple, Type, TypeConstructor, TypeName, TypeVar, Union, UnionType, Void,
    AbstractArray, DenseArray,
    # special objects
    Box, Function, IntrinsicFunction, LambdaStaticData, Method, MethodTable,
    Module, Symbol, Task, Array,
    # numeric types
    Bool, FloatingPoint, Float16, Float32, Float64, Number, Integer, Int, Int8, Int16,
    Int32, Int64, Int128, Ptr, Real, Signed, UInt, UInt8, UInt16, UInt32,
    UInt64, UInt128, Unsigned,
    # string types
    Char, ASCIIString, ByteString, DirectIndexString, AbstractString, UTF8String,
    # errors
    BoundsError, DivideError, DomainError, Exception,
    InexactError, InterruptException, MemoryError, OverflowError,
    StackOverflowError, UndefRefError, UndefVarError,
    # AST representation
    Expr, GotoNode, LabelNode, LineNumberNode, QuoteNode, SymbolNode, TopNode,
    GetfieldNode, NewvarNode,
    # object model functions
    fieldtype, getfield, setfield!, yieldto, throw, tuple, is, ===, isdefined,
    # arraylen, arrayref, arrayset, arraysize, tuplelen, tupleref,
    # _apply, kwcall,
    # sizeof    # not exported, to avoid conflicting with Base.sizeof
    # type reflection
    issubtype, typeof, isa,
    # typeassert, apply_type,
    # method reflection
    applicable, invoke, method_exists,
    # constants
    nothing, Main,
    # intrinsics module
    Intrinsics
    #ccall, cglobal, llvmcall, abs_float, add_float, add_int, and_int, ashr_int,
    #box, bswap_int, checked_fptosi, checked_fptoui, checked_sadd,
    #checked_smul, checked_ssub, checked_uadd, checked_umul, checked_usub,
    #checked_trunc_sint, checked_trunc_uint, check_top_bit,
    #nan_dom_err, copysign_float, ctlz_int, ctpop_int, cttz_int,
    #div_float, eq_float, eq_int, eqfsi64, eqfui64, flipsign_int, select_value,
    #sqrt_llvm, powi_llvm,
    #fpext, fpiseq, fpislt, fpsiround, fpuiround, fptosi, fptoui,
    #fptrunc, le_float, lefsi64, lefui64, lesif64,
    #leuif64, lshr_int, lt_float, ltfsi64, ltfui64, ltsif64, ltuif64, mul_float,
    #mul_int, ne_float, ne_int, neg_float, neg_int, not_int, or_int, rem_float,
    #sdiv_int, shl_int, sitofp, sle_int, slt_int, smod_int,
    #srem_int, sub_float, sub_int, trunc_int, udiv_int, uitofp,
    #ule_int, ult_int, unbox, urem_int, xor_int, sext_int, zext_int


const (===) = is

abstract Number
abstract Real     <: Number
abstract FloatingPoint <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer

bitstype 16 Float16 <: FloatingPoint
bitstype 32 Float32 <: FloatingPoint
bitstype 64 Float64 <: FloatingPoint

bitstype 8  Bool <: Integer
bitstype 32 Char

bitstype 8   Int8    <: Signed
bitstype 8   UInt8   <: Unsigned
bitstype 16  Int16   <: Signed
bitstype 16  UInt16  <: Unsigned
bitstype 32  Int32   <: Signed
bitstype 32  UInt32  <: Unsigned
bitstype 64  Int64   <: Signed
bitstype 64  UInt64  <: Unsigned
bitstype 128 Int128  <: Signed
bitstype 128 UInt128 <: Unsigned

if is(Int,Int64)
    typealias UInt UInt64
else
    typealias UInt UInt32
end

abstract Exception

type BoundsError        <: Exception end
type DivideError        <: Exception end
type DomainError        <: Exception end
type OverflowError      <: Exception end
type InexactError       <: Exception end
type MemoryError        <: Exception end
type StackOverflowError <: Exception end
type UndefRefError      <: Exception end
type UndefVarError      <: Exception
    var::Symbol
end
type InterruptException <: Exception end

abstract AbstractString
abstract DirectIndexString <: AbstractString

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
    data::Array{UInt8,1}
end

immutable UTF8String <: AbstractString
    data::Array{UInt8,1}
end

typealias ByteString Union(ASCIIString,UTF8String)

include(fname::ByteString) = ccall(:jl_load_, Any, (Any,), fname)

# constructors for built-in types

TypeVar(n::Symbol) =
    ccall(:jl_new_typevar, Any, (Any, Any, Any), n, Union(), Any)::TypeVar
TypeVar(n::Symbol, ub::ANY) =
    (isa(ub,Bool) ?
     ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, Union(), Any, ub)::TypeVar :
     ccall(:jl_new_typevar, Any, (Any, Any, Any), n, Union(), ub::Type)::TypeVar)
TypeVar(n::Symbol, lb::ANY, ub::ANY) =
    (isa(ub,Bool) ?
     ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, Union(), lb::Type, ub)::TypeVar :
     ccall(:jl_new_typevar, Any, (Any, Any, Any), n, lb::Type, ub::Type)::TypeVar)
TypeVar(n::Symbol, lb::ANY, ub::ANY, b::Bool) =
    ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, lb::Type, ub::Type, b)::TypeVar

TypeConstructor(p::ANY, t::ANY) = ccall(:jl_new_type_constructor, Any, (Any, Any), p::Tuple, t::Type)

Expr(args::ANY...) = _expr(args...)

LineNumberNode(n::Int) = ccall(:jl_new_struct, Any, (Any,Any...), LineNumberNode, n)::LineNumberNode
LabelNode(n::Int) = ccall(:jl_new_struct, Any, (Any,Any...), LabelNode, n)::LabelNode
GotoNode(n::Int) = ccall(:jl_new_struct, Any, (Any,Any...), GotoNode, n)::GotoNode
QuoteNode(x::ANY) = ccall(:jl_new_struct, Any, (Any,Any...), QuoteNode, x)::QuoteNode
NewvarNode(s::Symbol) = ccall(:jl_new_struct, Any, (Any,Any...), NewvarNode, s)::NewvarNode
TopNode(s::Symbol) = ccall(:jl_new_struct, Any, (Any,Any...), TopNode, s)::TopNode

Module(name::Symbol) = ccall(:jl_f_new_module, Any, (Any,), name)::Module
Module() = Module(:anonymous)

Task(f::ANY) = ccall(:jl_new_task, Any, (Any, Int), f::Function, 0)::Task

# simple convert for use by constructors of types in Core
convert(::Type{Any}, x::ANY) = x
convert{T}(::Type{T}, x::T) = x
