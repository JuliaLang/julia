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

#type Union <: Type
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

#type LambdaInfo
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

#abstract Ref{T}
#bitstype {32|64} Ptr{T} <: Ref{T}

# types for the front end

#type Expr
#    head::Symbol
#    args::Array{Any,1}
#    typ::Any
#end

#immutable LineNumberNode
#    file::Symbol
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

#immutable GlobalRef
#    mod::Module
#    name::Symbol
#end

# type Task
#     parent::Task
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
    Tuple, Type, TypeConstructor, TypeName, TypeVar, Union, Void,
    SimpleVector, AbstractArray, DenseArray,
    # special objects
    Function, LambdaInfo, Method, MethodTable,
    Module, Symbol, Task, Array, WeakRef,
    # numeric types
    Number, Real, Integer, Bool, Ref, Ptr,
    AbstractFloat, Float16, Float32, Float64,
    Signed, Int, Int8, Int16, Int32, Int64, Int128,
    Unsigned, UInt, UInt8, UInt16, UInt32, UInt64, UInt128,
    # string types
    Char, ASCIIString, ByteString, DirectIndexString, AbstractString, UTF8String,
    # errors
    BoundsError, DivideError, DomainError, Exception, InexactError,
    InterruptException, OutOfMemoryError, ReadOnlyMemoryError, OverflowError,
    StackOverflowError, SegmentationFault, UndefRefError, UndefVarError, TypeError,
    # AST representation
    Expr, GotoNode, LabelNode, LineNumberNode, QuoteNode, TopNode,
    GlobalRef, NewvarNode, GenSym, Slot,
    # object model functions
    fieldtype, getfield, setfield!, nfields, throw, tuple, is, ===, isdefined, eval,
    # sizeof    # not exported, to avoid conflicting with Base.sizeof
    # type reflection
    issubtype, typeof, isa,
    # method reflection
    applicable, invoke,
    # constants
    nothing, Main

const (===) = is

abstract Number
abstract Real     <: Number
abstract AbstractFloat <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer

bitstype 16 Float16 <: AbstractFloat
bitstype 32 Float32 <: AbstractFloat
bitstype 64 Float64 <: AbstractFloat

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

abstract AbstractString

function Typeof end
(f::typeof(Typeof))(x::ANY) = isa(x,Type) ? Type{x} : typeof(x)

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
immutable ReadOnlyMemoryError<: Exception end
immutable SegmentationFault  <: Exception end
immutable StackOverflowError <: Exception end
immutable UndefRefError      <: Exception end
immutable UndefVarError      <: Exception
    var::Symbol
end
immutable InterruptException <: Exception end
type TypeError <: Exception
    func::Symbol
    context::AbstractString
    expected::Type
    got
end

abstract DirectIndexString <: AbstractString

immutable ASCIIString <: DirectIndexString
    data::Array{UInt8,1}
    ASCIIString(d::Array{UInt8,1}) = new(d)
end

immutable UTF8String <: AbstractString
    data::Array{UInt8,1}
    UTF8String(d::Array{UInt8,1}) = new(d)
end

typealias ByteString Union{ASCIIString,UTF8String}

include(fname::ByteString) = ccall(:jl_load_, Any, (Any,), fname)

eval(e::ANY) = eval(Main, e)
eval(m::Module, e::ANY) = ccall(:jl_toplevel_eval_in, Any, (Any, Any), m, e)

kwfunc(f::ANY) = ccall(:jl_get_keyword_sorter, Any, (Any,), f)

kwftype(t::ANY) = typeof(ccall(:jl_get_kwsorter, Any, (Any,), t.name))

type Box
    contents::Any
    Box(x::ANY) = new(x)
    Box() = new()
end

# constructors for built-in types

type WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref, Any, (Any,), v)::WeakRef
end

TypeVar(n::Symbol) =
    ccall(:jl_new_typevar, Any, (Any, Any, Any), n, Union{}, Any)::TypeVar
TypeVar(n::Symbol, ub::ANY) =
    (isa(ub,Bool) ?
     ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, Union{}, Any, ub)::TypeVar :
     ccall(:jl_new_typevar, Any, (Any, Any, Any), n, Union{}, ub::Type)::TypeVar)
TypeVar(n::Symbol, lb::ANY, ub::ANY) =
    (isa(ub,Bool) ?
     ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, Union{}, lb::Type, ub)::TypeVar :
     ccall(:jl_new_typevar, Any, (Any, Any, Any), n, lb::Type, ub::Type)::TypeVar)
TypeVar(n::Symbol, lb::ANY, ub::ANY, b::Bool) =
    ccall(:jl_new_typevar_, Any, (Any, Any, Any, Any), n, lb::Type, ub::Type, b)::TypeVar

TypeConstructor(p::ANY, t::ANY) = ccall(:jl_new_type_constructor, Any, (Any, Any), p::SimpleVector, t::Type)

Void() = nothing

Expr(args::ANY...) = _expr(args...)

_new(typ::Symbol, argty::Symbol) = eval(:((::Type{$typ})(n::$argty) = $(Expr(:new, typ, :n))))
_new(:LabelNode, :Int)
_new(:GotoNode, :Int)
_new(:TopNode, :Symbol)
_new(:NewvarNode, :Slot)
_new(:QuoteNode, :ANY)
_new(:GenSym, :Int)
eval(:((::Type{LineNumberNode})(f::Symbol, l::Int) = $(Expr(:new, :LineNumberNode, :f, :l))))
eval(:((::Type{GlobalRef})(m::Module, s::Symbol) = $(Expr(:new, :GlobalRef, :m, :s))))
eval(:((::Type{Slot})(n::Int) = $(Expr(:new, :Slot, :n, Any))))
eval(:((::Type{Slot})(n::Int, t::ANY) = $(Expr(:new, :Slot, :n, :t))))

Module(name::Symbol=:anonymous, std_imports::Bool=true) = ccall(:jl_f_new_module, Any, (Any, Bool), name, std_imports)::Module

Task(f::ANY) = ccall(:jl_new_task, Any, (Any, Int), f, 0)::Task

# simple convert for use by constructors of types in Core
# note that there is no actual conversion defined here,
# so the methods and ccall's in Core aren't permitted to use convert
convert(::Type{Any}, x::ANY) = x
convert{T}(::Type{T}, x::T) = x
cconvert(T::Type, x) = convert(T, x)
unsafe_convert{T}(::Type{T}, x::T) = x

# primitive array constructors
(::Type{Array{T,N}}){T,N}(d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)
(::Type{Array{T,1}}){T}(m::Int) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
(::Type{Array{T,2}}){T}(m::Int, n::Int) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m, n)
(::Type{Array{T,3}}){T}(m::Int, n::Int, o::Int) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m, n, o)

(::Type{Array{T}}){T,N}(d::NTuple{N,Int}) = Array{T,N}(d)
(::Type{Array{T}}){T}(m::Int) = Array{T,1}(m)
(::Type{Array{T}}){T}(m::Int, n::Int) = Array{T,2}(m, n)
(::Type{Array{T}}){T}(m::Int, n::Int, o::Int) = Array{T,3}(m, n, o)

(::Type{Array{T,1}}){T}() = Array{T,1}(0)
(::Type{Array{T,2}}){T}() = Array{T,2}(0, 0)

# TODO: possibly turn these into deprecations
Array{T,N}(::Type{T}, d::NTuple{N,Int}) = Array{T}(d)
Array{T}(::Type{T}, d::Int...) = Array{T}(d)
Array{T}(::Type{T}, m::Int)               = Array{T,1}(m)
Array{T}(::Type{T}, m::Int,n::Int)        = Array{T,2}(m,n)
Array{T}(::Type{T}, m::Int,n::Int,o::Int) = Array{T,3}(m,n,o)

module TopModule
    # this defines the types that lowering expects to be defined in a (top) module
    # that are usually inherited from Core, but could be defined custom for a module
    using Core: Box, IntrinsicFunction, Builtin,
            arrayref, arrayset, arraysize,
            _expr, _apply, typeassert, apply_type, svec, kwfunc
    export Box, IntrinsicFunction, Builtin,
            arrayref, arrayset, arraysize,
            _expr, _apply, typeassert, apply_type, svec, kwfunc
end
using .TopModule
ccall(:jl_set_istopmod, Void, (Bool,), true)
