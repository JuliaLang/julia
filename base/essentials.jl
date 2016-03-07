# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract IO

typealias Callable Union{Function,DataType}

const Bottom = Union{}

# The real @inline macro is not available until after array.jl, so this
# internal macro splices the meta Expr directly into the function body.
macro _inline_meta()
    Expr(:meta, :inline)
end
macro _noinline_meta()
    Expr(:meta, :noinline)
end
macro _pure_meta()
    Expr(:meta, :pure)
end
# another version of inlining that propagates an inbounds context
macro _propagate_inbounds_meta()
    Expr(:meta, :inline, :propagate_inbounds)
end

convert{T}(::Type{T}, x::T) = x

convert(::Type{Tuple{}}, ::Tuple{}) = ()
convert(::Type{Tuple}, x::Tuple) = x
convert{T}(::Type{Tuple{Vararg{T}}}, x::Tuple) = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)

macro generated(f)
    isa(f, Expr) || error("invalid syntax; @generated must be used with a function definition")
    if is(f.head, :function) || (isdefined(:length) && is(f.head, :(=)) && length(f.args) == 2 && f.args[1].head == :call)
        f.head = :stagedfunction
        return Expr(:escape, f)
    else
        error("invalid syntax; @generated must be used with a function definition")
    end
end


tuple_type_head(::Type{Union{}}) = throw(MethodError(tuple_type_head, (Type{Union{}},)))
function tuple_type_head{T<:Tuple}(::Type{T})
    @_pure_meta
    T.parameters[1]
end

isvarargtype(t::ANY) = isa(t,DataType)&&is((t::DataType).name,Vararg.name)
isvatuple(t::DataType) = (n = length(t.parameters); n > 0 && isvarargtype(t.parameters[n]))
unwrapva(t::ANY) = isvarargtype(t) ? t.parameters[1] : t

function tuple_type_tail{T<:Tuple}(::Type{T})
    @_pure_meta
    if isvatuple(T) && length(T.parameters) == 1
        return T
    end
    Tuple{argtail(T.parameters...)...}
end

argtail(x, rest...) = rest
tail(x::Tuple) = argtail(x...)

convert{T<:Tuple{Any,Vararg{Any}}}(::Type{T}, x::Tuple{Any, Vararg{Any}}) =
    tuple(convert(tuple_type_head(T),x[1]), convert(tuple_type_tail(T), tail(x))...)
convert{T<:Tuple{Any,Vararg{Any}}}(::Type{T}, x::T) = x

oftype(x,c) = convert(typeof(x),c)

unsigned(x::Int) = reinterpret(UInt, x)
signed(x::UInt) = reinterpret(Int, x)

# conversions used by ccall
ptr_arg_cconvert{T}(::Type{Ptr{T}}, x) = cconvert(T, x)
ptr_arg_unsafe_convert{T}(::Type{Ptr{T}}, x) = unsafe_convert(T, x)
ptr_arg_unsafe_convert(::Type{Ptr{Void}}, x) = x

cconvert(T::Type, x) = convert(T, x) # do the conversion eagerly in most cases
cconvert{P<:Ptr}(::Type{P}, x) = x # but defer the conversion to Ptr to unsafe_convert
unsafe_convert{T}(::Type{T}, x::T) = x # unsafe_convert (like convert) defaults to assuming the convert occurred
unsafe_convert{P<:Ptr}(::Type{P}, x::Ptr) = convert(P, x)

reinterpret{T}(::Type{T}, x) = box(T, x)

sizeof(x) = Core.sizeof(x)

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

# simple Array{Any} operations needed for bootstrap
setindex!(A::Array{Any}, x::ANY, i::Int) = arrayset(A, x, i)

function length_checked_equal(args...)
    n = length(args[1])
    for i=2:length(args)
        if length(args[i]) != n
            error("argument dimensions must match")
        end
    end
    n
end

map(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i=1:length(a) ]

function precompile(f::ANY, args::Tuple)
    ccall(:jl_compile_hint, Void, (Any,), Tuple{Core.Typeof(f), args...})
end

function precompile(argt::Type)
    ccall(:jl_compile_hint, Void, (Any,), argt)
end

esc(e::ANY) = Expr(:escape, e)

macro boundscheck(blk)
    # hack: use this syntax since it avoids introducing line numbers
    :($(Expr(:boundscheck,true));
      $(esc(blk));
      $(Expr(:boundscheck,:pop)))
end

macro inbounds(blk)
    :($(Expr(:inbounds,true));
      $(esc(blk));
      $(Expr(:inbounds,:pop)))
end

macro label(name::Symbol)
    Expr(:symboliclabel, name)
end

macro goto(name::Symbol)
    Expr(:symbolicgoto, name)
end

# SimpleVector

function getindex(v::SimpleVector, i::Int)
    if !(1 <= i <= length(v))
        throw(BoundsError(v,i))
    end
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    x == C_NULL && throw(UndefRefError())
    return unsafe_pointer_to_objref(x)
end

length(v::SimpleVector) = v.length
endof(v::SimpleVector) = v.length
start(v::SimpleVector) = 1
next(v::SimpleVector,i) = (v[i],i+1)
done(v::SimpleVector,i) = (i > v.length)
isempty(v::SimpleVector) = (v.length == 0)

function ==(v1::SimpleVector, v2::SimpleVector)
    length(v1)==length(v2) || return false
    for i = 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

map(f, v::SimpleVector) = Any[ f(v[i]) for i = 1:length(v) ]

getindex(v::SimpleVector, I::AbstractArray) = svec(Any[ v[i] for i in I ]...)

function isassigned(v::SimpleVector, i::Int)
    1 <= i <= length(v) || return false
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    return x != C_NULL
end

# index colon
type Colon
end
const (:) = Colon()

# For passing constants through type inference
immutable Val{T}
end
