# This file is a part of Julia. License is MIT: http://julialang.org/license

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

#abstract Ref{T}
#bitstype {32|64} Ptr{T} <: Ref{T}

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
    Any, DataType, Vararg, ANY, NTuple,
    Tuple, Type, TypeConstructor, TypeName, TypeVar, Union, UnionType, Void,
    SimpleVector, AbstractArray, DenseArray,
    # special objects
    Box, Function, IntrinsicFunction, LambdaStaticData, Method, MethodTable,
    Module, Symbol, Task, Array, WeakRef,
    # numeric types
    Number, Real, Integer, Bool, Ref, Ptr,
    FloatingPoint, Float16, Float32, Float64,
    Signed, Int, Int8, Int16, Int32, Int64, Int128,
    Unsigned, UInt, UInt8, UInt16, UInt32, UInt64, UInt128,
    # string types
    Char, ASCIIString, ByteString, DirectIndexString, AbstractString, UTF8String,
    # errors
    BoundsError, DivideError, DomainError, Exception,
    InexactError, InterruptException, OutOfMemoryError, OverflowError,
    StackOverflowError, UndefRefError, UndefVarError,
    # AST representation
    Expr, GotoNode, LabelNode, LineNumberNode, QuoteNode, SymbolNode, TopNode,
    GlobalRef, NewvarNode, GenSym,
    # object model functions
    fieldtype, getfield, setfield!, nfields, throw, tuple, is, ===, isdefined,
    # arraylen, arrayref, arrayset, arraysize,
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
    #sqrt_llvm_fast,
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

immutable BoundsError        <: Exception
    a::Any
    i::Any
    BoundsError() = new()
    BoundsError(a::ANY) = new(a)
    BoundsError(a::ANY, i::ANY) = new(a,i)
end
immutable DivideError        <: Exception end
immutable DomainError        <: Exception end
immutable OverflowError      <: Exception end
immutable InexactError       <: Exception end
immutable OutOfMemoryError   <: Exception end
immutable StackOverflowError <: Exception end
immutable UndefRefError      <: Exception end
immutable UndefVarError      <: Exception
    var::Symbol
end
immutable InterruptException <: Exception end

abstract AbstractString
abstract DirectIndexString <: AbstractString

type SymbolNode
    name::Symbol
    typ
    SymbolNode(name::Symbol, t::ANY) = new(name, t)
end

immutable GlobalRef
    mod::Module
    name::Symbol
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

type WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref, Any, (Any,), v)::WeakRef
end

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

TypeConstructor(p::ANY, t::ANY) = ccall(:jl_new_type_constructor, Any, (Any, Any), p::SimpleVector, t::Type)

Expr(args::ANY...) = _expr(args...)

_new(typ::Symbol, argty::Symbol) = eval(:(Core.call(::Type{$typ}, n::$argty) = $(Expr(:new, typ, :n))))
_new(:LineNumberNode, :Int)
_new(:LabelNode, :Int)
_new(:GotoNode, :Int)
_new(:TopNode, :Symbol)
_new(:NewvarNode, :Symbol)
_new(:QuoteNode, :ANY)
_new(:GenSym, :Int)

Module(name::Symbol=:anonymous, std_imports::Bool=true) = ccall(:jl_f_new_module, Any, (Any, Bool), name, std_imports)::Module

Task(f::ANY) = ccall(:jl_new_task, Any, (Any, Int), f::Function, 0)::Task

# simple convert for use by constructors of types in Core
# note that there is no actual conversion defined here,
# so the methods and ccall's in Core aren't permitted to use convert
convert(::Type{Any}, x::ANY) = x
convert{T}(::Type{T}, x::T) = x
cconvert(T::Type, x) = convert(T, x)
unsafe_convert{T}(::Type{T}, x::T) = x

ccall(:jl_set_istopmod, Void, (Bool,), true)
