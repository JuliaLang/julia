const NonTupleType = Union(DataType,UnionType,TypeConstructor)

typealias Callable Union(Function,DataType)

const Bottom = Union()

# constructors for Core types in boot.jl
call(T::Type{BoundsError}) = Core.call(T)
call(T::Type{DivideError}) = Core.call(T)
call(T::Type{DomainError}) = Core.call(T)
call(T::Type{OverflowError}) = Core.call(T)
call(T::Type{InexactError}) = Core.call(T)
call(T::Type{MemoryError}) = Core.call(T)
call(T::Type{StackOverflowError}) = Core.call(T)
call(T::Type{UndefRefError}) = Core.call(T)
call(T::Type{UndefVarError}, var::Symbol) = Core.call(T, var)
call(T::Type{InterruptException}) = Core.call(T)
call(T::Type{SymbolNode}, name::Symbol, t::ANY) = Core.call(T, name, t)
call(T::Type{GetfieldNode}, value, name::Symbol, typ) = Core.call(T, value, name, typ)
call(T::Type{ASCIIString}, d::Array{UInt8,1}) = Core.call(T, d)
call(T::Type{UTF8String}, d::Array{UInt8,1}) = Core.call(T, d)
call(T::Type{TypeVar}, args...) = Core.call(T, args...)
call(T::Type{TypeConstructor}, args...) = Core.call(T, args...)
call(T::Type{Expr}, args::ANY...) = _expr(args...)
call(T::Type{LineNumberNode}, n::Int) = Core.call(T, n)
call(T::Type{LabelNode}, n::Int) = Core.call(T, n)
call(T::Type{GotoNode}, n::Int) = Core.call(T, n)
call(T::Type{QuoteNode}, x::ANY) = Core.call(T, x)
call(T::Type{NewvarNode}, s::Symbol) = Core.call(T, s)
call(T::Type{TopNode}, s::Symbol) = Core.call(T, s)
call(T::Type{Module}, args...) = Core.call(T, args...)
call(T::Type{Task}, f::ANY) = Core.call(T, f)

call{T}(::Type{T}, args...) = convert(T, args...)::T

convert{T}(::Type{T}, x::T) = x

convert(::(), ::()) = ()
convert(::Type{Tuple}, x::Tuple) = x

argtail(x, rest...) = rest
tail(x::Tuple) = argtail(x...)

convert(T::(Type, Type...), x::(Any, Any...)) =
    tuple(convert(T[1],x[1]), convert(tail(T), tail(x))...)
convert(T::(Any, Any...), x::(Any, Any...)) =
    tuple(convert(T[1],x[1]), convert(tail(T), tail(x))...)

convert{T}(::Type{(T...)}, x::Tuple) = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)


ptr_arg_convert{T}(::Type{Ptr{T}}, x) = convert(T, x)
ptr_arg_convert(::Type{Ptr{Void}}, x) = x

# conversion used by ccall
cconvert(T, x) = convert(T, x)
# use the code in ccall.cpp to safely allocate temporary pointer arrays
cconvert{T}(::Type{Ptr{Ptr{T}}}, a::Array) = a
# convert strings to ByteString to pass as pointers
cconvert{P<:Union(Int8,UInt8)}(::Type{Ptr{P}}, s::AbstractString) = bytestring(s)

reinterpret{T,S}(::Type{T}, x::S) = box(T,unbox(S,x))

abstract IO

type ErrorException <: Exception
    msg::AbstractString
end

type SystemError <: Exception
    prefix::AbstractString
    errnum::Int32
    SystemError(p::AbstractString, e::Integer) = new(p, int32(e))
    SystemError(p::AbstractString) = new(p, errno())
end

type TypeError <: Exception
    func::Symbol
    context::AbstractString
    expected::Type
    got
end

type ParseError <: Exception
    msg::AbstractString
end

type ArgumentError <: Exception
    msg::AbstractString
end

#type UnboundError <: Exception
#    var::Symbol
#end

type KeyError <: Exception
    key
end

type LoadError <: Exception
    file::AbstractString
    line::Int
    error
end

type MethodError <: Exception
    f
    args
end

type EOFError <: Exception end

type DimensionMismatch <: Exception
    msg::AbstractString
end
DimensionMismatch() = DimensionMismatch("")

# For passing constants through type inference
immutable Val{T}
end

type WeakRef
    value
    WeakRef() = WeakRef(nothing)
    WeakRef(v::ANY) = ccall(:jl_gc_new_weakref, Any, (Any,), v)::WeakRef
end

ccall(:jl_get_system_hooks, Void, ())


int(x) = convert(Int, x)
int(x::Int) = x
uint(x) = convert(UInt, x)
uint(x::UInt) = x

# index colon
type Colon
end
const (:) = Colon()

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

function finalizer(o::ANY, f::Union(Function,Ptr))
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)
end

finalize(o::ANY) = ccall(:jl_finalize, Void, (Any,), o)

gc() = ccall(:jl_gc_collect, Void, ())
gc_enable() = ccall(:jl_gc_enable, Void, ())
gc_disable() = ccall(:jl_gc_disable, Void, ())

bytestring(str::ByteString) = str

identity(x) = x

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    out = Array(Any, 4)
    l = 4
    i = 1
    for x in xs
        for y in x
            if i > l
                ccall(:jl_array_grow_end, Void, (Any, UInt), out, 16)
                l += 16
            end
            arrayset(out, y, i)
            i += 1
        end
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), out, l-i+1)
    out
end

# used by { } syntax
function cell_1d(xs::ANY...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,xs[i],i)
    end
    a
end

function cell_2d(nr, nc, xs::ANY...)
    a = Array(Any,nr,nc)
    for i=1:(nr*nc)
        arrayset(a,xs[i],i)
    end
    a
end

# simple Array{Any} operations needed for bootstrap
setindex!(A::Array{Any}, x::ANY, i::Real) = arrayset(A, x, to_index(i))

function length_checked_equal(args...)
    n = length(args[1])
    for i=2:length(args)
        if length(args[i]) != n
            error("argument dimensions must match")
        end
    end
    n
end

map(f::Callable, a::Array{Any,1}) = Any[ f(a[i]) for i=1:length(a) ]

macro thunk(ex); :(()->$(esc(ex))); end
macro L_str(s); s; end

function precompile(f::ANY, args::Tuple)
    if isa(f,DataType)
        args = tuple(Type{f}, args...)
        f = f.name.module.call
    end
    if isgeneric(f)
        ccall(:jl_compile_hint, Void, (Any, Any), f, args)
    end
end

esc(e::ANY) = Expr(:escape, e)

macro boundscheck(yesno,blk)
    # hack: use this syntax since it avoids introducing line numbers
    :($(Expr(:boundscheck,yesno));
      $(esc(blk));
      $(Expr(:boundscheck,:pop)))
end

macro inbounds(blk)
    :(@boundscheck false $(esc(blk)))
end

macro label(name::Symbol)
    Expr(:symboliclabel, name)
end

macro goto(name::Symbol)
    Expr(:symbolicgoto, name)
end

Array{T,N}(::Type{T}, d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)

Array{T}(::Type{T}, m::Int) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Int,n::Int) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m,n)
Array{T}(::Type{T}, m::Int,n::Int,o::Int) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m,n,o)

Array(T::Type, d::Int...) = Array(T, d)
Array(T::Type, d::Integer...) = Array(T, convert((Int...), d))

Array{T}(::Type{T}, m::Integer) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
Array{T}(::Type{T}, m::Integer,n::Integer) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m, n)
Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m, n, o)
