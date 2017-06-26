# This file is a part of Julia. License is MIT: https://julialang.org/license

# commented-out definitions are implemented in C

#abstract type Any <: Any end
#abstract type Type{T} end

#abstract type Vararg{T} end

#mutable struct Symbol
#    #opaque
#end

#mutable struct TypeName
#    name::Symbol
#end

#mutable struct DataType <: Type
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

#struct Union <: Type
#    a
#    b
#end

#mutable struct TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#struct UnionAll
#    var::TypeVar
#    body
#end

#struct Void
#end
#const nothing = Void()

#abstract type AbstractArray{T,N} end
#abstract type DenseArray{T,N} <: AbstractArray{T,N} end

#mutable struct Array{T,N} <: DenseArray{T,N}
#end

#mutable struct Module
#    name::Symbol
#end

#mutable struct Method
#end

#mutable struct MethodInstance
#end

#mutable struct CodeInfo
#end

#mutable struct TypeMapLevel
#end

#mutable struct TypeMapEntry
#end

#abstract type Ref{T} end
#primitive type Ptr{T} <: Ref{T} {32|64} end

# types for the front end

#mutable struct Expr
#    head::Symbol
#    args::Array{Any,1}
#    typ::Any
#end

#struct LineNumberNode
#    line::Int
#    file::Any # nominally Union{Symbol,Void}
#end

#struct LabelNode
#    label::Int
#end

#struct GotoNode
#    label::Int
#end

#struct QuoteNode
#    value
#end

#struct GlobalRef
#    mod::Module
#    name::Symbol
#end

#mutable struct Task
#    parent::Task
#    storage::Any
#    consumers
#    started::Bool
#    done::Bool
#    runnable::Bool
#end

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

const AnyVector = Array{Any,1}

abstract type Number end
abstract type Real     <: Number end
abstract type AbstractFloat <: Real end
abstract type Integer  <: Real end
abstract type Signed   <: Integer end
abstract type Unsigned <: Integer end

primitive type Float16 <: AbstractFloat 16 end
primitive type Float32 <: AbstractFloat 32 end
primitive type Float64 <: AbstractFloat 64 end

#primitive type Bool <: Integer 8 end
primitive type Char 32 end

primitive type Int8    <: Signed   8 end
#primitive type UInt8   <: Unsigned 8 end
primitive type Int16   <: Signed   16 end
primitive type UInt16  <: Unsigned 16 end
#primitive type Int32   <: Signed   32 end
primitive type UInt32  <: Unsigned 32 end
#primitive type Int64   <: Signed   64 end
primitive type UInt64  <: Unsigned 64 end
primitive type Int128  <: Signed   128 end
primitive type UInt128 <: Unsigned 128 end

if Int === Int64
    const UInt = UInt64
else
    const UInt = UInt32
end

function Typeof end
(f::typeof(Typeof))(x::ANY) = isa(x,Type) ? Type{x} : typeof(x)

abstract type Exception end
mutable struct ErrorException <: Exception
    msg::AbstractString
    ErrorException(msg::AbstractString) = new(msg)
end

Expr(args::ANY...) = _expr(args...)

macro _noinline_meta()
    Expr(:meta, :noinline)
end

struct BoundsError        <: Exception
    a::Any
    i::Any
    BoundsError() = new()
    BoundsError(a::ANY) = (@_noinline_meta; new(a))
    BoundsError(a::ANY, i) = (@_noinline_meta; new(a,i))
end
struct DivideError        <: Exception end
struct DomainError        <: Exception end
struct OverflowError      <: Exception end
struct InexactError       <: Exception end
struct OutOfMemoryError   <: Exception end
struct ReadOnlyMemoryError<: Exception end
struct SegmentationFault  <: Exception end
struct StackOverflowError <: Exception end
struct UndefRefError      <: Exception end
struct UndefVarError      <: Exception
    var::Symbol
end
struct InterruptException <: Exception end
mutable struct TypeError <: Exception
    func::Symbol
    context::AbstractString
    expected::Type
    got
end

abstract type DirectIndexString <: AbstractString end

String(s::String) = s  # no constructor yet

# This should always be inlined
getptls() = ccall(:jl_get_ptls_states, Ptr{Void}, ())

include(m::Module, fname::String) = ccall(:jl_load_, Any, (Any, Any), m, fname)

eval(e::ANY) = eval(Main, e)
eval(m::Module, e::ANY) = ccall(:jl_toplevel_eval_in, Any, (Any, Any), m, e)

kwfunc(f::ANY) = ccall(:jl_get_keyword_sorter, Any, (Any,), f)

kwftype(t::ANY) = typeof(ccall(:jl_get_kwsorter, Any, (Any,), t))

mutable struct Box
    contents::Any
    Box(x::ANY) = new(x)
    Box() = new()
end

# constructors for built-in types

mutable struct WeakRef
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

(::Type{Tuple{}})() = () # Tuple{}()

struct VecElement{T}
    value::T
    VecElement{T}(value::T) where {T} = new(value) # disable converting constructor in Core
end
VecElement(arg::T) where {T} = VecElement{T}(arg)

# used by lowering of splicing unquote
splicedexpr(hd::Symbol, args::Array{Any,1}) = (e=Expr(hd); e.args=args; e)

_new(typ::Symbol, argty::Symbol) = eval(:((::Type{$typ})(n::$argty) = $(Expr(:new, typ, :n))))
_new(:LabelNode, :Int)
_new(:GotoNode, :Int)
_new(:NewvarNode, :SlotNumber)
_new(:QuoteNode, :ANY)
_new(:SSAValue, :Int)
eval(:((::Type{LineNumberNode})(l::Int) = $(Expr(:new, :LineNumberNode, :l, nothing))))
eval(:((::Type{LineNumberNode})(l::Int, f::ANY) = $(Expr(:new, :LineNumberNode, :l, :f))))
eval(:((::Type{GlobalRef})(m::Module, s::Symbol) = $(Expr(:new, :GlobalRef, :m, :s))))
eval(:((::Type{SlotNumber})(n::Int) = $(Expr(:new, :SlotNumber, :n))))
eval(:((::Type{TypedSlot})(n::Int, t::ANY) = $(Expr(:new, :TypedSlot, :n, :t))))

Module(name::Symbol=:anonymous, std_imports::Bool=true) = ccall(:jl_f_new_module, Ref{Module}, (Any, Bool), name, std_imports)

Task(f::ANY) = ccall(:jl_new_task, Ref{Task}, (Any, Int), f, 0)

# simple convert for use by constructors of types in Core
# note that there is no actual conversion defined here,
# so the methods and ccall's in Core aren't permitted to use convert
convert(::Type{Any}, x::ANY) = x
convert(::Type{T}, x::T) where {T} = x
cconvert(::Type{T}, x) where {T} = convert(T, x)
unsafe_convert(::Type{T}, x::T) where {T} = x

const NTuple{N,T} = Tuple{Vararg{T,N}}


# primitive array constructors
Array{T,N}(d::NTuple{N,Int}) where {T,N} =
    ccall(:jl_new_array, Array{T,N}, (Any, Any), Array{T,N}, d)
Array{T,1}(d::NTuple{1,Int}) where {T} = Array{T,1}(getfield(d,1))
Array{T,2}(d::NTuple{2,Int}) where {T} = Array{T,2}(getfield(d,1), getfield(d,2))
Array{T,3}(d::NTuple{3,Int}) where {T} = Array{T,3}(getfield(d,1), getfield(d,2), getfield(d,3))
Array{T,N}(d::Vararg{Int,N}) where {T,N} = ccall(:jl_new_array, Array{T,N}, (Any, Any), Array{T,N}, d)
Array{T,1}(m::Int) where {T} = ccall(:jl_alloc_array_1d, Array{T,1}, (Any, Int), Array{T,1}, m)
Array{T,2}(m::Int, n::Int) where {T} =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any, Int, Int), Array{T,2}, m, n)
Array{T,3}(m::Int, n::Int, o::Int) where {T} =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any, Int, Int, Int), Array{T,3}, m, n, o)

Array{T}(d::NTuple{N,Int}) where {T,N} = Array{T,N}(d)
Array{T}(m::Int) where {T} = Array{T,1}(m)
Array{T}(m::Int, n::Int) where {T} = Array{T,2}(m, n)
Array{T}(m::Int, n::Int, o::Int) where {T} = Array{T,3}(m, n, o)

Array{T,1}() where {T} = Array{T,1}(0)

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
    atdoc(__source__, __module__, x...)
end
macro __doc__(x)
    Expr(:escape, Expr(:block, Expr(:meta, :doc), x))
end
macro doc_str(s)
    Expr(:escape, s)
end
atdoc     = (source, mod, str, expr) -> Expr(:escape, expr)
atdoc!(λ) = global atdoc = λ


# simple stand-alone print definitions for debugging
abstract type IO end
mutable struct CoreSTDOUT <: IO end
mutable struct CoreSTDERR <: IO end
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
print(io::IO, x::String) = (write(io, x); nothing)
print(io::IO, x::ANY) = show(io, x)
print(io::IO, x::ANY, a::ANY...) = (print(io, x); print(io, a...))
println(io::IO) = (write(io, 0x0a); nothing) # 0x0a = '\n'
println(io::IO, x::ANY...) = (print(io, x...); println(io))

show(a::ANY) = show(STDOUT, a)
print(a::ANY...) = print(STDOUT, a...)
println(a::ANY...) = println(STDOUT, a...)

ccall(:jl_set_istopmod, Void, (Any, Bool), Core, true)
