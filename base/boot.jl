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
#    a
#    b
#end

#type TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#type UnionAll
#    var::TypeVar
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

#type Method
#end

#type MethodInstance
#end

#type CodeInfo
#end

#type TypeMapLevel
#end

#type TypeMapEntry
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

export
    # key types
    Any, DataType, Vararg, ANY, NTuple,
    Tuple, Type, UnionAll, TypeName, TypeVar, Union, Void,
    SimpleVector, AbstractArray, DenseArray,
    # special objects
    Function, CodeInfo, Method, MethodTable, TypeMapEntry, TypeMapLevel,
    Module, Symbol, Task, Array, WeakRef, VecElement,
    # numeric types
    Number, Real, Integer, Bool, Ref, Ptr,
    AbstractFloat, Float16, Float32, Float64,
    Signed, Int, Int8, Int16, Int32, Int64, Int128,
    Unsigned, UInt, UInt8, UInt16, UInt32, UInt64, UInt128,
    # string types
    Char, DirectIndexString, AbstractString, String, IO,
    # errors
    ErrorException, BoundsError, DivideError, DomainError, Exception, InexactError,
    InterruptException, OutOfMemoryError, ReadOnlyMemoryError, OverflowError,
    StackOverflowError, SegmentationFault, UndefRefError, UndefVarError, TypeError,
    # AST representation
    Expr, GotoNode, LabelNode, LineNumberNode, QuoteNode,
    GlobalRef, NewvarNode, SSAValue, Slot, SlotNumber, TypedSlot,
    # object model functions
    fieldtype, getfield, setfield!, nfields, throw, tuple, ===, isdefined, eval,
    # sizeof    # not exported, to avoid conflicting with Base.sizeof
    # type reflection
    issubtype, typeof, isa, typeassert,
    # method reflection
    applicable, invoke,
    # constants
    nothing, Main

typealias AnyVector Array{Any,1}

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

if Int === Int64
    typealias UInt UInt64
else
    typealias UInt UInt32
end

function Typeof end
(f::typeof(Typeof))(x::ANY) = isa(x,Type) ? Type{x} : typeof(x)

abstract Exception
type ErrorException <: Exception
    msg::AbstractString
    ErrorException(msg::AbstractString) = new(msg)
end

Expr(args::ANY...) = _expr(args...)

macro _noinline_meta()
    Expr(:meta, :noinline)
end

immutable BoundsError        <: Exception
    a::Any
    i::Any
    BoundsError() = new()
    BoundsError(a::ANY) = (@_noinline_meta; new(a))
    BoundsError(a::ANY, i) = (@_noinline_meta; new(a,i))
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

String(s::String) = s  # no constructor yet

# This should always be inlined
getptls() = ccall(:jl_get_ptls_states, Ptr{Void}, ())

include(fname::String) = ccall(:jl_load_, Any, (Any,), fname)

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
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref_th, Ref{WeakRef},
                            (Ptr{Void}, Any), getptls(), v)
end

TypeVar(n::Symbol) =
    ccall(:jl_new_typevar, Ref{TypeVar}, (Any, Any, Any), n, Union{}, Any)
TypeVar(n::Symbol, ub::ANY) =
    ccall(:jl_new_typevar, Ref{TypeVar}, (Any, Any, Any), n, Union{}, ub)
TypeVar(n::Symbol, lb::ANY, ub::ANY) =
    ccall(:jl_new_typevar, Ref{TypeVar}, (Any, Any, Any), n, lb, ub)

UnionAll(v::TypeVar, t::ANY) = ccall(:jl_type_unionall, Any, (Any, Any), v, t)

Void() = nothing

immutable VecElement{T}
    value::T
    VecElement(value::T) = new(value) # disable converting constructor in Core
end
VecElement{T}(arg::T) = VecElement{T}(arg)

# used by lowering of splicing unquote
splicedexpr(hd::Symbol, args::Array{Any,1}) = (e=Expr(hd); e.args=args; e)

_new(typ::Symbol, argty::Symbol) = eval(:((::Type{$typ})(n::$argty) = $(Expr(:new, typ, :n))))
_new(:LabelNode, :Int)
_new(:GotoNode, :Int)
_new(:NewvarNode, :SlotNumber)
_new(:QuoteNode, :ANY)
_new(:SSAValue, :Int)
eval(:((::Type{LineNumberNode})(l::Int) = $(Expr(:new, :LineNumberNode, :l))))
eval(:((::Type{GlobalRef})(m::Module, s::Symbol) = $(Expr(:new, :GlobalRef, :m, :s))))
eval(:((::Type{SlotNumber})(n::Int) = $(Expr(:new, :SlotNumber, :n))))
eval(:((::Type{TypedSlot})(n::Int, t::ANY) = $(Expr(:new, :TypedSlot, :n, :t))))

Module(name::Symbol=:anonymous, std_imports::Bool=true) = ccall(:jl_f_new_module, Ref{Module}, (Any, Bool), name, std_imports)

Task(f::ANY) = ccall(:jl_new_task, Ref{Task}, (Any, Int), f, 0)

# simple convert for use by constructors of types in Core
# note that there is no actual conversion defined here,
# so the methods and ccall's in Core aren't permitted to use convert
convert(::Type{Any}, x::ANY) = x
convert{T}(::Type{T}, x::T) = x
cconvert{T}(::Type{T}, x) = convert(T, x)
unsafe_convert{T}(::Type{T}, x::T) = x

typealias NTuple{N,T} Tuple{Vararg{T,N}}


# primitive array constructors
(::Type{Array{T,N}}){T,N}(d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)
(::Type{Array{T,1}}){T}(d::NTuple{1,Int}) = Array{T,1}(getfield(d,1))
(::Type{Array{T,2}}){T}(d::NTuple{2,Int}) = Array{T,2}(getfield(d,1), getfield(d,2))
(::Type{Array{T,3}}){T}(d::NTuple{3,Int}) = Array{T,3}(getfield(d,1), getfield(d,2), getfield(d,3))
(::Type{Array{T,N}}){T,N}(d::Vararg{Int, N}) = ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)
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

# primitive Symbol constructors
function Symbol(s::String)
    return ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int),
                 ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s),
                 sizeof(s))
end
function Symbol(a::Array{UInt8,1})
    return ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int),
                 ccall(:jl_array_ptr, Ptr{UInt8}, (Any,), a),
                 Intrinsics.arraylen(a))
end

# docsystem basics
macro doc(x...)
    atdoc(x...)
end
macro __doc__(x)
    Expr(:escape, Expr(:block, Expr(:meta, :doc), x))
end
macro doc_str(s)
    Expr(:escape, s)
end
atdoc     = (str, expr) -> Expr(:escape, expr)
atdoc!(λ) = global atdoc = λ


# simple stand-alone print definitions for debugging
abstract IO
type CoreSTDOUT <: IO end
type CoreSTDERR <: IO end
const STDOUT = CoreSTDOUT()
const STDERR = CoreSTDERR()
io_pointer(::CoreSTDOUT) = Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stdout, Ptr{Void}), 1, 1)
io_pointer(::CoreSTDERR) = Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stderr, Ptr{Void}), 1, 1)

unsafe_write(io::IO, x::Ptr{UInt8}, nb::UInt) =
    (ccall(:jl_uv_puts, Void, (Ptr{Void}, Ptr{UInt8}, UInt), io_pointer(io), x, nb); nb)
unsafe_write(io::IO, x::Ptr{UInt8}, nb::Int) =
    (ccall(:jl_uv_puts, Void, (Ptr{Void}, Ptr{UInt8}, Int), io_pointer(io), x, nb); nb)
write(io::IO, x::UInt8) =
    (ccall(:jl_uv_putb, Void, (Ptr{Void}, UInt8), io_pointer(io), x); 1)
function write(io::IO, x::String)
    nb = sizeof(x)
    unsafe_write(io, ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), x), nb)
    return nb
end

show(io::IO, x::ANY) = ccall(:jl_static_show, Void, (Ptr{Void}, Any), io_pointer(io), x)
print(io::IO, x::Char) = ccall(:jl_uv_putc, Void, (Ptr{Void}, Char), io_pointer(io), x)
print(io::IO, x::String) = write(io, x)
print(io::IO, x::ANY) = show(io, x)
print(io::IO, x::ANY, a::ANY...) = (print(io, x); print(io, a...))
println(io::IO) = write(io, 0x0a) # 0x0a = '\n'
println(io::IO, x::ANY...) = (print(io, x...); println(io))

show(a::ANY) = show(STDOUT, a)
print(a::ANY...) = print(STDOUT, a...)
println(a::ANY...) = println(STDOUT, a...)

ccall(:jl_set_istopmod, Void, (Bool,), true)
