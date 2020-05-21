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

#struct Nothing
#end
#const nothing = Nothing()

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

#mutable struct CodeInstance
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
#end

#struct LineNumberNode
#    line::Int
#    file::Union{Symbol,Nothing}
#end

#struct LineInfoNode
#    method::Any
#    file::Symbol
#    line::Int
#    inlined_at::Int
#end

#struct GotoNode
#    label::Int
#end

#struct PiNode
#    val
#    typ
#end

#struct PhiNode
#    edges::Vector{Any}
#    values::Vector{Any}
#end

#struct PhiCNode
#    values::Vector{Any}
#end

#struct UpsilonNode
#    val
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
#    state::Symbol
#    donenotify::Any
#    result::Any
#    exception::Any
#    backtrace::Any
#    logstate::Any
#    code::Any
#end

export
    # key types
    Any, DataType, Vararg, NTuple,
    Tuple, Type, UnionAll, TypeVar, Union, Nothing, Cvoid,
    AbstractArray, DenseArray, NamedTuple,
    # special objects
    Function, Method,
    Module, Symbol, Task, Array, UndefInitializer, undef, WeakRef, VecElement,
    # numeric types
    Number, Real, Integer, Bool, Ref, Ptr,
    AbstractFloat, Float16, Float32, Float64,
    Signed, Int, Int8, Int16, Int32, Int64, Int128,
    Unsigned, UInt, UInt8, UInt16, UInt32, UInt64, UInt128,
    # string types
    AbstractChar, Char, AbstractString, String, IO,
    # errors
    ErrorException, BoundsError, DivideError, DomainError, Exception,
    InterruptException, InexactError, OutOfMemoryError, ReadOnlyMemoryError,
    OverflowError, StackOverflowError, SegmentationFault, UndefRefError, UndefVarError,
    TypeError, ArgumentError, MethodError, AssertionError, LoadError, InitError,
    UndefKeywordError,
    # AST representation
    Expr, QuoteNode, LineNumberNode, GlobalRef,
    # object model functions
    fieldtype, getfield, setfield!, nfields, throw, tuple, ===, isdefined, eval, ifelse,
    # sizeof    # not exported, to avoid conflicting with Base.sizeof
    # type reflection
    <:, typeof, isa, typeassert,
    # method reflection
    applicable, invoke,
    # constants
    nothing, Main

const getproperty = getfield
const setproperty! = setfield!

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
abstract type AbstractChar end
primitive type Char <: AbstractChar 32 end

primitive type Int8    <: Signed   8 end
#primitive type UInt8   <: Unsigned 8 end
primitive type Int16   <: Signed   16 end
primitive type UInt16  <: Unsigned 16 end
#primitive type Int32   <: Signed   32 end
#primitive type UInt32  <: Unsigned 32 end
#primitive type Int64   <: Signed   64 end
#primitive type UInt64  <: Unsigned 64 end
primitive type Int128  <: Signed   128 end
primitive type UInt128 <: Unsigned 128 end

if Int === Int64
    const UInt = UInt64
else
    const UInt = UInt32
end

function iterate end
function Typeof end
ccall(:jl_toplevel_eval_in, Any, (Any, Any),
      Core, quote
      (f::typeof(Typeof))(x) = ($(_expr(:meta,:nospecialize,:x)); isa(x,Type) ? Type{x} : typeof(x))
      end)

macro nospecialize(x)
    _expr(:meta, :nospecialize, x)
end

Expr(@nospecialize args...) = _expr(args...)

abstract type Exception end
struct ErrorException <: Exception
    msg::AbstractString
end

macro _inline_meta()
    Expr(:meta, :inline)
end

macro _noinline_meta()
    Expr(:meta, :noinline)
end

struct BoundsError <: Exception
    a::Any
    i::Any
    BoundsError() = new()
    BoundsError(@nospecialize(a)) = (@_noinline_meta; new(a))
    BoundsError(@nospecialize(a), i) = (@_noinline_meta; new(a,i))
end
struct DivideError         <: Exception end
struct OutOfMemoryError    <: Exception end
struct ReadOnlyMemoryError <: Exception end
struct SegmentationFault   <: Exception end
struct StackOverflowError  <: Exception end
struct UndefRefError       <: Exception end
struct UndefVarError <: Exception
    var::Symbol
end
struct InterruptException <: Exception end
struct DomainError <: Exception
    val
    msg::AbstractString
    DomainError(@nospecialize(val)) = (@_noinline_meta; new(val, ""))
    DomainError(@nospecialize(val), @nospecialize(msg)) = (@_noinline_meta; new(val, msg))
end
struct TypeError <: Exception
    # `func` is the name of the builtin function that encountered a type error,
    # the name of the type that hit an error in its definition or application, or
    # some other brief description of where the error happened.
    # `context` optionally adds extra detail, e.g. the name of the type parameter
    # that got a bad value.
    func::Symbol
    context::Union{AbstractString,Symbol}
    expected::Type
    got
    TypeError(func, context, @nospecialize(expected::Type), @nospecialize(got)) =
        new(func, context, expected, got)
end
TypeError(where, @nospecialize(expected::Type), @nospecialize(got)) =
    TypeError(Symbol(where), "", expected, got)
struct InexactError <: Exception
    func::Symbol
    T  # Type
    val
    InexactError(f::Symbol, @nospecialize(T), @nospecialize(val)) = (@_noinline_meta; new(f, T, val))
end
struct OverflowError <: Exception
    msg::AbstractString
end

struct ArgumentError <: Exception
    msg::AbstractString
end
struct UndefKeywordError <: Exception
    var::Symbol
end

struct MethodError <: Exception
    f
    args
    world::UInt
    MethodError(@nospecialize(f), @nospecialize(args), world::UInt) = new(f, args, world)
end
const typemax_UInt = ccall(:jl_typemax_uint, Any, (Any,), UInt)
MethodError(@nospecialize(f), @nospecialize(args)) = MethodError(f, args, typemax_UInt)

struct AssertionError <: Exception
    msg::AbstractString
end
AssertionError() = AssertionError("")

abstract type WrappedException <: Exception end

struct LoadError <: WrappedException
    file::AbstractString
    line::Int
    error
end

struct InitError <: WrappedException
    mod::Symbol
    error
end

String(s::String) = s  # no constructor yet

const Cvoid = Nothing
Nothing() = nothing

# This should always be inlined
getptls() = ccall(:jl_get_ptls_states, Ptr{Cvoid}, ())

include(m::Module, fname::String) = ccall(:jl_load_, Any, (Any, Any), m, fname)

eval(m::Module, @nospecialize(e)) = ccall(:jl_toplevel_eval_in, Any, (Any, Any), m, e)

kwfunc(@nospecialize(f)) = ccall(:jl_get_keyword_sorter, Any, (Any,), f)

kwftype(@nospecialize(t)) = typeof(ccall(:jl_get_kwsorter, Any, (Any,), t))

mutable struct Box
    contents::Any
    Box(@nospecialize(x)) = new(x)
    Box() = new()
end

# constructors for built-in types

mutable struct WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(@nospecialize(v)) = ccall(:jl_gc_new_weakref_th, Ref{WeakRef},
                                      (Ptr{Cvoid}, Any), getptls(), v)
end

TypeVar(n::Symbol) = _typevar(n, Union{}, Any)
TypeVar(n::Symbol, @nospecialize(ub)) = _typevar(n, Union{}, ub)
TypeVar(n::Symbol, @nospecialize(lb), @nospecialize(ub)) = _typevar(n, lb, ub)

UnionAll(v::TypeVar, @nospecialize(t)) = ccall(:jl_type_unionall, Any, (Any, Any), v, t)

Tuple{}() = ()

struct VecElement{T}
    value::T
    VecElement{T}(value::T) where {T} = new(value) # disable converting constructor in Core
end
VecElement(arg::T) where {T} = VecElement{T}(arg)

_new(typ::Symbol, argty::Symbol) = eval(Core, :($typ(@nospecialize n::$argty) = $(Expr(:new, typ, :n))))
_new(:GotoNode, :Int)
_new(:NewvarNode, :SlotNumber)
_new(:QuoteNode, :Any)
_new(:SSAValue, :Int)
eval(Core, :(LineNumberNode(l::Int) = $(Expr(:new, :LineNumberNode, :l, nothing))))
eval(Core, :(LineNumberNode(l::Int, @nospecialize(f)) = $(Expr(:new, :LineNumberNode, :l, :f))))
eval(Core, :(GlobalRef(m::Module, s::Symbol) = $(Expr(:new, :GlobalRef, :m, :s))))
eval(Core, :(SlotNumber(n::Int) = $(Expr(:new, :SlotNumber, :n))))
eval(Core, :(TypedSlot(n::Int, @nospecialize(t)) = $(Expr(:new, :TypedSlot, :n, :t))))
eval(Core, :(PhiNode(edges::Array{Any, 1}, values::Array{Any, 1}) = $(Expr(:new, :PhiNode, :edges, :values))))
eval(Core, :(PiNode(val, typ) = $(Expr(:new, :PiNode, :val, :typ))))
eval(Core, :(PhiCNode(values::Array{Any, 1}) = $(Expr(:new, :PhiCNode, :values))))
eval(Core, :(UpsilonNode(val) = $(Expr(:new, :UpsilonNode, :val))))
eval(Core, :(UpsilonNode() = $(Expr(:new, :UpsilonNode))))
eval(Core, :(LineInfoNode(@nospecialize(method), file::Symbol, line::Int, inlined_at::Int) =
             $(Expr(:new, :LineInfoNode, :method, :file, :line, :inlined_at))))

Module(name::Symbol=:anonymous, std_imports::Bool=true) = ccall(:jl_f_new_module, Ref{Module}, (Any, Bool), name, std_imports)

function _Task(@nospecialize(f), reserved_stack::Int, completion_future)
    return ccall(:jl_new_task, Ref{Task}, (Any, Any, Int), f, completion_future, reserved_stack)
end

# simple convert for use by constructors of types in Core
# note that there is no actual conversion defined here,
# so the methods and ccall's in Core aren't permitted to use convert
convert(::Type{Any}, @nospecialize(x)) = x
convert(::Type{T}, x::T) where {T} = x
cconvert(::Type{T}, x) where {T} = convert(T, x)
unsafe_convert(::Type{T}, x::T) where {T} = x

const NTuple{N,T} = Tuple{Vararg{T,N}}


## primitive Array constructors
struct UndefInitializer end
const undef = UndefInitializer()
# type and dimensionality specified, accepting dims as series of Ints
Array{T,1}(::UndefInitializer, m::Int) where {T} =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any, Int), Array{T,1}, m)
Array{T,2}(::UndefInitializer, m::Int, n::Int) where {T} =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any, Int, Int), Array{T,2}, m, n)
Array{T,3}(::UndefInitializer, m::Int, n::Int, o::Int) where {T} =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any, Int, Int, Int), Array{T,3}, m, n, o)
Array{T,N}(::UndefInitializer, d::Vararg{Int,N}) where {T,N} =
    ccall(:jl_new_array, Array{T,N}, (Any, Any), Array{T,N}, d)
# type and dimensionality specified, accepting dims as tuples of Ints
Array{T,1}(::UndefInitializer, d::NTuple{1,Int}) where {T} = Array{T,1}(undef, getfield(d,1))
Array{T,2}(::UndefInitializer, d::NTuple{2,Int}) where {T} = Array{T,2}(undef, getfield(d,1), getfield(d,2))
Array{T,3}(::UndefInitializer, d::NTuple{3,Int}) where {T} = Array{T,3}(undef, getfield(d,1), getfield(d,2), getfield(d,3))
Array{T,N}(::UndefInitializer, d::NTuple{N,Int}) where {T,N} = ccall(:jl_new_array, Array{T,N}, (Any, Any), Array{T,N}, d)
# type but not dimensionality specified
Array{T}(::UndefInitializer, m::Int) where {T} = Array{T,1}(undef, m)
Array{T}(::UndefInitializer, m::Int, n::Int) where {T} = Array{T,2}(undef, m, n)
Array{T}(::UndefInitializer, m::Int, n::Int, o::Int) where {T} = Array{T,3}(undef, m, n, o)
Array{T}(::UndefInitializer, d::NTuple{N,Int}) where {T,N} = Array{T,N}(undef, d)
# empty vector constructor
Array{T,1}() where {T} = Array{T,1}(undef, 0)


(Array{T,N} where T)(x::AbstractArray{S,N}) where {S,N} = Array{S,N}(x)

Array(A::AbstractArray{T,N})    where {T,N}   = Array{T,N}(A)
Array{T}(A::AbstractArray{S,N}) where {T,N,S} = Array{T,N}(A)

AbstractArray{T}(A::AbstractArray{S,N}) where {T,S,N} = AbstractArray{T,N}(A)

# primitive Symbol constructors
eval(Core, :(function Symbol(s::String)
    $(Expr(:meta, :pure))
    return ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int),
                 ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s),
                 sizeof(s))
end))
function Symbol(a::Array{UInt8,1})
    return ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int),
                 ccall(:jl_array_ptr, Ptr{UInt8}, (Any,), a),
                 Intrinsics.arraylen(a))
end
Symbol(s::Symbol) = s

# module providing the IR object model
module IR
export CodeInfo, MethodInstance, CodeInstance, GotoNode,
    NewvarNode, SSAValue, Slot, SlotNumber, TypedSlot,
    PiNode, PhiNode, PhiCNode, UpsilonNode, LineInfoNode

import Core: CodeInfo, MethodInstance, CodeInstance, GotoNode,
    NewvarNode, SSAValue, Slot, SlotNumber, TypedSlot,
    PiNode, PhiNode, PhiCNode, UpsilonNode, LineInfoNode

end

# docsystem basics
const unescape = Symbol("hygienic-scope")
macro doc(x...)
    docex = atdoc(__source__, __module__, x...)
    isa(docex, Expr) && docex.head === :escape && return docex
    return Expr(:escape, Expr(unescape, docex, typeof(atdoc).name.module))
end
macro __doc__(x)
    return Expr(:escape, Expr(:block, Expr(:meta, :doc), x))
end
atdoc     = (source, mod, str, expr) -> Expr(:escape, expr)
atdoc!(λ) = global atdoc = λ

# macros for big integer syntax
macro int128_str end
macro uint128_str end
macro big_str end

# macro for command syntax
macro cmd end


# simple stand-alone print definitions for debugging
abstract type IO end
struct CoreSTDOUT <: IO end
struct CoreSTDERR <: IO end
const stdout = CoreSTDOUT()
const stderr = CoreSTDERR()
io_pointer(::CoreSTDOUT) = Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stdout, Ptr{Cvoid}), 1, 1)
io_pointer(::CoreSTDERR) = Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stderr, Ptr{Cvoid}), 1, 1)

unsafe_write(io::IO, x::Ptr{UInt8}, nb::UInt) =
    (ccall(:jl_uv_puts, Cvoid, (Ptr{Cvoid}, Ptr{UInt8}, UInt), io_pointer(io), x, nb); nb)
unsafe_write(io::IO, x::Ptr{UInt8}, nb::Int) =
    (ccall(:jl_uv_puts, Cvoid, (Ptr{Cvoid}, Ptr{UInt8}, Int), io_pointer(io), x, nb); nb)
write(io::IO, x::UInt8) =
    (ccall(:jl_uv_putb, Cvoid, (Ptr{Cvoid}, UInt8), io_pointer(io), x); 1)
function write(io::IO, x::String)
    nb = sizeof(x)
    unsafe_write(io, ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), x), nb)
    return nb
end

show(io::IO, @nospecialize x) = ccall(:jl_static_show, Cvoid, (Ptr{Cvoid}, Any), io_pointer(io), x)
print(io::IO, x::AbstractChar) = ccall(:jl_uv_putc, Cvoid, (Ptr{Cvoid}, Char), io_pointer(io), x)
print(io::IO, x::String) = (write(io, x); nothing)
print(io::IO, @nospecialize x) = show(io, x)
print(io::IO, @nospecialize(x), @nospecialize a...) = (print(io, x); print(io, a...))
println(io::IO) = (write(io, 0x0a); nothing) # 0x0a = '\n'
println(io::IO, @nospecialize x...) = (print(io, x...); println(io))

show(@nospecialize a) = show(stdout, a)
print(@nospecialize a...) = print(stdout, a...)
println(@nospecialize a...) = println(stdout, a...)

struct GeneratedFunctionStub
    gen
    argnames::Array{Any,1}
    spnames::Union{Nothing, Array{Any,1}}
    line::Int
    file::Symbol
    expand_early::Bool
end

# invoke and wrap the results of @generated
function (g::GeneratedFunctionStub)(@nospecialize args...)
    body = g.gen(args...)
    if body isa CodeInfo
        return body
    end
    lam = Expr(:lambda, g.argnames,
               Expr(Symbol("scope-block"),
                    Expr(:block,
                         LineNumberNode(g.line, g.file),
                         Expr(:meta, :push_loc, g.file, Symbol("@generated body")),
                         Expr(:return, body),
                         Expr(:meta, :pop_loc))))
    if g.spnames === nothing
        return lam
    else
        return Expr(Symbol("with-static-parameters"), lam, g.spnames...)
    end
end

NamedTuple() = NamedTuple{(),Tuple{}}(())

NamedTuple{names}(args::Tuple) where {names} = NamedTuple{names,typeof(args)}(args)

using .Intrinsics: sle_int, add_int

eval(Core, :(NamedTuple{names,T}(args::T) where {names, T <: Tuple} =
             $(Expr(:splatnew, :(NamedTuple{names,T}), :args))))

# constructors for built-in types

import .Intrinsics: eq_int, trunc_int, lshr_int, sub_int, shl_int, bitcast, sext_int, zext_int, and_int

throw_inexacterror(f::Symbol, ::Type{T}, val) where {T} = (@_noinline_meta; throw(InexactError(f, T, val)))

function is_top_bit_set(x)
    @_inline_meta
    eq_int(trunc_int(UInt8, lshr_int(x, sub_int(shl_int(sizeof(x), 3), 1))), trunc_int(UInt8, 1))
end

function is_top_bit_set(x::Union{Int8,UInt8})
    @_inline_meta
    eq_int(lshr_int(x, 7), trunc_int(typeof(x), 1))
end

function check_top_bit(::Type{To}, x) where {To}
    @_inline_meta
    is_top_bit_set(x) && throw_inexacterror(:check_top_bit, To, x)
    x
end

function checked_trunc_sint(::Type{To}, x::From) where {To,From}
    @_inline_meta
    y = trunc_int(To, x)
    back = sext_int(From, y)
    eq_int(x, back) || throw_inexacterror(:trunc, To, x)
    y
end

function checked_trunc_uint(::Type{To}, x::From) where {To,From}
    @_inline_meta
    y = trunc_int(To, x)
    back = zext_int(From, y)
    eq_int(x, back) || throw_inexacterror(:trunc, To, x)
    y
end

toInt8(x::Int8)       = x
toInt8(x::Int16)      = checked_trunc_sint(Int8, x)
toInt8(x::Int32)      = checked_trunc_sint(Int8, x)
toInt8(x::Int64)      = checked_trunc_sint(Int8, x)
toInt8(x::Int128)     = checked_trunc_sint(Int8, x)
toInt8(x::UInt8)      = bitcast(Int8, check_top_bit(Int8, x))
toInt8(x::UInt16)     = checked_trunc_sint(Int8, check_top_bit(Int8, x))
toInt8(x::UInt32)     = checked_trunc_sint(Int8, check_top_bit(Int8, x))
toInt8(x::UInt64)     = checked_trunc_sint(Int8, check_top_bit(Int8, x))
toInt8(x::UInt128)    = checked_trunc_sint(Int8, check_top_bit(Int8, x))
toInt8(x::Bool)       = and_int(bitcast(Int8, x), Int8(1))
toInt16(x::Int8)      = sext_int(Int16, x)
toInt16(x::Int16)     = x
toInt16(x::Int32)     = checked_trunc_sint(Int16, x)
toInt16(x::Int64)     = checked_trunc_sint(Int16, x)
toInt16(x::Int128)    = checked_trunc_sint(Int16, x)
toInt16(x::UInt8)     = zext_int(Int16, x)
toInt16(x::UInt16)    = bitcast(Int16, check_top_bit(Int16, x))
toInt16(x::UInt32)    = checked_trunc_sint(Int16, check_top_bit(Int16, x))
toInt16(x::UInt64)    = checked_trunc_sint(Int16, check_top_bit(Int16, x))
toInt16(x::UInt128)   = checked_trunc_sint(Int16, check_top_bit(Int16, x))
toInt16(x::Bool)      = and_int(zext_int(Int16, x), Int16(1))
toInt32(x::Int8)      = sext_int(Int32, x)
toInt32(x::Int16)     = sext_int(Int32, x)
toInt32(x::Int32)     = x
toInt32(x::Int64)     = checked_trunc_sint(Int32, x)
toInt32(x::Int128)    = checked_trunc_sint(Int32, x)
toInt32(x::UInt8)     = zext_int(Int32, x)
toInt32(x::UInt16)    = zext_int(Int32, x)
toInt32(x::UInt32)    = bitcast(Int32, check_top_bit(Int32, x))
toInt32(x::UInt64)    = checked_trunc_sint(Int32, check_top_bit(Int32, x))
toInt32(x::UInt128)   = checked_trunc_sint(Int32, check_top_bit(Int32, x))
toInt32(x::Bool)      = and_int(zext_int(Int32, x), Int32(1))
toInt64(x::Int8)      = sext_int(Int64, x)
toInt64(x::Int16)     = sext_int(Int64, x)
toInt64(x::Int32)     = sext_int(Int64, x)
toInt64(x::Int64)     = x
toInt64(x::Int128)    = checked_trunc_sint(Int64, x)
toInt64(x::UInt8)     = zext_int(Int64, x)
toInt64(x::UInt16)    = zext_int(Int64, x)
toInt64(x::UInt32)    = zext_int(Int64, x)
toInt64(x::UInt64)    = bitcast(Int64, check_top_bit(Int64, x))
toInt64(x::UInt128)   = checked_trunc_sint(Int64, check_top_bit(Int64, x))
toInt64(x::Bool)      = and_int(zext_int(Int64, x), Int64(1))
toInt128(x::Int8)     = sext_int(Int128, x)
toInt128(x::Int16)    = sext_int(Int128, x)
toInt128(x::Int32)    = sext_int(Int128, x)
toInt128(x::Int64)    = sext_int(Int128, x)
toInt128(x::Int128)   = x
toInt128(x::UInt8)    = zext_int(Int128, x)
toInt128(x::UInt16)   = zext_int(Int128, x)
toInt128(x::UInt32)   = zext_int(Int128, x)
toInt128(x::UInt64)   = zext_int(Int128, x)
toInt128(x::UInt128)  = bitcast(Int128, check_top_bit(Int128, x))
toInt128(x::Bool)     = and_int(zext_int(Int128, x), Int128(1))
toUInt8(x::Int8)      = bitcast(UInt8, check_top_bit(UInt8, x))
toUInt8(x::Int16)     = checked_trunc_uint(UInt8, x)
toUInt8(x::Int32)     = checked_trunc_uint(UInt8, x)
toUInt8(x::Int64)     = checked_trunc_uint(UInt8, x)
toUInt8(x::Int128)    = checked_trunc_uint(UInt8, x)
toUInt8(x::UInt8)     = x
toUInt8(x::UInt16)    = checked_trunc_uint(UInt8, x)
toUInt8(x::UInt32)    = checked_trunc_uint(UInt8, x)
toUInt8(x::UInt64)    = checked_trunc_uint(UInt8, x)
toUInt8(x::UInt128)   = checked_trunc_uint(UInt8, x)
toUInt8(x::Bool)      = and_int(bitcast(UInt8, x), UInt8(1))
toUInt16(x::Int8)     = sext_int(UInt16, check_top_bit(UInt16, x))
toUInt16(x::Int16)    = bitcast(UInt16, check_top_bit(UInt16, x))
toUInt16(x::Int32)    = checked_trunc_uint(UInt16, x)
toUInt16(x::Int64)    = checked_trunc_uint(UInt16, x)
toUInt16(x::Int128)   = checked_trunc_uint(UInt16, x)
toUInt16(x::UInt8)    = zext_int(UInt16, x)
toUInt16(x::UInt16)   = x
toUInt16(x::UInt32)   = checked_trunc_uint(UInt16, x)
toUInt16(x::UInt64)   = checked_trunc_uint(UInt16, x)
toUInt16(x::UInt128)  = checked_trunc_uint(UInt16, x)
toUInt16(x::Bool)     = and_int(zext_int(UInt16, x), UInt16(1))
toUInt32(x::Int8)     = sext_int(UInt32, check_top_bit(UInt32, x))
toUInt32(x::Int16)    = sext_int(UInt32, check_top_bit(UInt32, x))
toUInt32(x::Int32)    = bitcast(UInt32, check_top_bit(UInt32, x))
toUInt32(x::Int64)    = checked_trunc_uint(UInt32, x)
toUInt32(x::Int128)   = checked_trunc_uint(UInt32, x)
toUInt32(x::UInt8)    = zext_int(UInt32, x)
toUInt32(x::UInt16)   = zext_int(UInt32, x)
toUInt32(x::UInt32)   = x
toUInt32(x::UInt64)   = checked_trunc_uint(UInt32, x)
toUInt32(x::UInt128)  = checked_trunc_uint(UInt32, x)
toUInt32(x::Bool)     = and_int(zext_int(UInt32, x), UInt32(1))
toUInt64(x::Int8)     = sext_int(UInt64, check_top_bit(UInt64, x))
toUInt64(x::Int16)    = sext_int(UInt64, check_top_bit(UInt64, x))
toUInt64(x::Int32)    = sext_int(UInt64, check_top_bit(UInt64, x))
toUInt64(x::Int64)    = bitcast(UInt64, check_top_bit(UInt64, x))
toUInt64(x::Int128)   = checked_trunc_uint(UInt64, x)
toUInt64(x::UInt8)    = zext_int(UInt64, x)
toUInt64(x::UInt16)   = zext_int(UInt64, x)
toUInt64(x::UInt32)   = zext_int(UInt64, x)
toUInt64(x::UInt64)   = x
toUInt64(x::UInt128)  = checked_trunc_uint(UInt64, x)
toUInt64(x::Bool)     = and_int(zext_int(UInt64, x), UInt64(1))
toUInt128(x::Int8)    = sext_int(UInt128, check_top_bit(UInt128, x))
toUInt128(x::Int16)   = sext_int(UInt128, check_top_bit(UInt128, x))
toUInt128(x::Int32)   = sext_int(UInt128, check_top_bit(UInt128, x))
toUInt128(x::Int64)   = sext_int(UInt128, check_top_bit(UInt128, x))
toUInt128(x::Int128)  = bitcast(UInt128, check_top_bit(UInt128, x))
toUInt128(x::UInt8)   = zext_int(UInt128, x)
toUInt128(x::UInt16)  = zext_int(UInt128, x)
toUInt128(x::UInt32)  = zext_int(UInt128, x)
toUInt128(x::UInt64)  = zext_int(UInt128, x)
toUInt128(x::UInt128) = x
toUInt128(x::Bool)    = and_int(zext_int(UInt128, x), UInt128(1))

# TODO: this is here to work around the 4 method limit in inference (#23210).
const BuiltinInts = Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8, Bool}
Int8(x::BuiltinInts)    = toInt8(x)::Int8
Int16(x::BuiltinInts)   = toInt16(x)::Int16
Int32(x::BuiltinInts)   = toInt32(x)::Int32
Int64(x::BuiltinInts)   = toInt64(x)::Int64
Int128(x::BuiltinInts)  = toInt128(x)::Int128
UInt8(x::BuiltinInts)   = toUInt8(x)::UInt8
UInt16(x::BuiltinInts)  = toUInt16(x)::UInt16
UInt32(x::BuiltinInts)  = toUInt32(x)::UInt32
UInt64(x::BuiltinInts)  = toUInt64(x)::UInt64
UInt128(x::BuiltinInts) = toUInt128(x)::UInt128

(::Type{T})(x::T) where {T<:Number} = x

Int(x::Ptr)  = bitcast(Int, x)
UInt(x::Ptr) = bitcast(UInt, x)
if Int === Int32
Int64(x::Ptr) = Int64(UInt32(x))
UInt64(x::Ptr) = UInt64(UInt32(x))
end
Ptr{T}(x::Union{Int,UInt,Ptr}) where {T} = bitcast(Ptr{T}, x)
Ptr{T}() where {T} = Ptr{T}(0)

Signed(x::UInt8)    = Int8(x)
Unsigned(x::Int8)   = UInt8(x)
Signed(x::UInt16)   = Int16(x)
Unsigned(x::Int16)  = UInt16(x)
Signed(x::UInt32)   = Int32(x)
Unsigned(x::Int32)  = UInt32(x)
Signed(x::UInt64)   = Int64(x)
Unsigned(x::Int64)  = UInt64(x)
Signed(x::UInt128)  = Int128(x)
Unsigned(x::Int128) = UInt128(x)

Signed(x::Union{Float32, Float64, Bool})   = Int(x)
Unsigned(x::Union{Float32, Float64, Bool}) = UInt(x)

Integer(x::Integer) = x
Integer(x::Union{Float32, Float64}) = Int(x)

# Binding for the julia parser, called as
#
#    Core._parse(text, filename, offset, options)
#
# Parse Julia code from the buffer `text`, starting at `offset` and attributing
# it to `filename`. `text` may be a `String` or `svec(ptr::Ptr{UInt8},
# len::Int)` for a raw unmanaged buffer. `options` should be one of `:atom`,
# `:statement` or `:all`, indicating how much the parser will consume.
#
# `_parse` must return an `svec` containing an `Expr` and the new offset as an
# `Int`.
#
# The internal jl_parse which will call into Core._parse if not `nothing`.
_parse = nothing

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Core, true)
