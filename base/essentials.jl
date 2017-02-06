# This file is a part of Julia. License is MIT: http://julialang.org/license

using Core: CodeInfo

typealias Callable Union{Function,Type}

const Bottom = Union{}

abstract AbstractSet{T}
abstract Associative{K,V}

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

convert(::Type{Any}, x::ANY) = x
convert{T}(::Type{T}, x::T) = x

convert(::Type{Tuple{}}, ::Tuple{}) = ()
convert(::Type{Tuple}, x::Tuple) = x
convert{T}(::Type{Tuple{Vararg{T}}}, x::Tuple) = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)

macro generated(f)
    isa(f, Expr) || error("invalid syntax; @generated must be used with a function definition")
    if f.head === :function || (isdefined(:length) && f.head === :(=) && length(f.args) == 2 && f.args[1].head == :call)
        f.head = :stagedfunction
        return Expr(:escape, f)
    else
        error("invalid syntax; @generated must be used with a function definition")
    end
end

argtail(x, rest...) = rest
tail(x::Tuple) = argtail(x...)

tuple_type_head(T::UnionAll) = tuple_type_head(T.body)
function tuple_type_head(T::DataType)
    @_pure_meta
    T.name === Tuple.name || throw(MethodError(tuple_type_head, (T,)))
    return unwrapva(T.parameters[1])
end
tuple_type_tail(T::UnionAll) = tuple_type_tail(T.body)
function tuple_type_tail(T::DataType)
    @_pure_meta
    T.name === Tuple.name || throw(MethodError(tuple_type_tail, (T,)))
    if isvatuple(T) && length(T.parameters) == 1
        return T
    end
    return Tuple{argtail(T.parameters...)...}
end

tuple_type_cons{S}(::Type{S}, ::Type{Union{}}) = Union{}
function tuple_type_cons{S,T<:Tuple}(::Type{S}, ::Type{T})
    @_pure_meta
    Tuple{S, T.parameters...}
end

function unwrap_unionall(a::ANY)
    while isa(a,UnionAll)
        a = a.body
    end
    return a
end

function rewrap_unionall(t::ANY, u::ANY)
    if !isa(u, UnionAll)
        return t
    end
    return UnionAll(u.var, rewrap_unionall(t, u.body))
end

# replace TypeVars in all enclosing UnionAlls with fresh TypeVars
function rename_unionall(u::ANY)
    if !isa(u,UnionAll)
        return u
    end
    body = rename_unionall(u.body)
    if body === u.body
        body = u
    else
        body = UnionAll(u.var, body)
    end
    var = u.var::TypeVar
    nv = TypeVar(var.name, var.lb, var.ub)
    return UnionAll(nv, body{nv})
end

const _va_typename = Vararg.body.body.name
function isvarargtype(t::ANY)
    t = unwrap_unionall(t)
    isa(t, DataType) && (t::DataType).name === _va_typename
end

isvatuple(t::DataType) = (n = length(t.parameters); n > 0 && isvarargtype(t.parameters[n]))
function unwrapva(t::ANY)
    t2 = unwrap_unionall(t)
    isvarargtype(t2) ? t2.parameters[1] : t
end

typename(a) = error("typename does not apply to this type")
typename(a::DataType) = a.name
function typename(a::Union)
    ta = typename(a.a)
    tb = typename(a.b)
    ta === tb ? tb : error("typename does not apply to unions whose components have different typenames")
end
typename(union::UnionAll) = typename(union.body)

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
unsafe_convert{T<:Ptr}(::Type{T}, x::T) = x  # to resolve ambiguity with the next method
unsafe_convert{P<:Ptr}(::Type{P}, x::Ptr) = convert(P, x)

reinterpret{T}(::Type{T}, x) = bitcast(T, x)
reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)
reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16,x)

sizeof(x) = Core.sizeof(x)

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    out = Array{Any}(4)
    l = 4
    i = 1
    for x in xs
        for y in x
            if i > l
                ccall(:jl_array_grow_end, Void, (Any, UInt), out, 16)
                l += 16
            end
            Core.arrayset(out, y, i)
            i += 1
        end
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), out, l-i+1)
    out
end

# simple Array{Any} operations needed for bootstrap
setindex!(A::Array{Any}, x::ANY, i::Int) = Core.arrayset(A, x, i)

map(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i=1:length(a) ]

function precompile(f::ANY, args::Tuple)
    ccall(:jl_compile_hint, Int32, (Any,), Tuple{Core.Typeof(f), args...}) != 0
end

function precompile(argt::Type)
    ccall(:jl_compile_hint, Int32, (Any,), argt) != 0
end

"""
    esc(e::ANY)

Only valid in the context of an `Expr` returned from a macro. Prevents the macro hygiene
pass from turning embedded variables into gensym variables. See the [Macros](@ref man-macros)
section of the Metaprogramming chapter of the manual for more details and examples.
"""
esc(e::ANY) = Expr(:escape, e)

macro boundscheck(blk)
    # hack: use this syntax since it avoids introducing line numbers
    :($(Expr(:boundscheck,true));
      $(esc(blk));
      $(Expr(:boundscheck,:pop)))
end

"""
    @inbounds(blk)

Eliminates array bounds checking within expressions.

In the example below the bound check of array A is skipped to improve performance.
```julia
function sum(A::AbstractArray)
    r = zero(eltype(A))
    for i = 1:length(A)
        @inbounds r += A[i]
    end
    return r
end
```
!!! Warning

    Using `@inbounds` may return incorrect results/crashes/corruption
    for out-of-bounds indices. The user is responsible for checking it manually.
"""
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
indices(v::SimpleVector) = (OneTo(length(v)),)
linearindices(v::SimpleVector) = indices(v, 1)
indices(v::SimpleVector, d) = d <= 1 ? indices(v)[d] : OneTo(1)

function ==(v1::SimpleVector, v2::SimpleVector)
    length(v1)==length(v2) || return false
    for i = 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

map(f, v::SimpleVector) = Any[ f(v[i]) for i = 1:length(v) ]

getindex(v::SimpleVector, I::AbstractArray) = Core.svec(Any[ v[i] for i in I ]...)

"""
    isassigned(array, i) -> Bool

Tests whether the given array has a value associated with index `i`. Returns `false`
if the index is out of bounds, or has an undefined reference.
"""
function isassigned end

function isassigned(v::SimpleVector, i::Int)
    1 <= i <= length(v) || return false
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    return x != C_NULL
end

"""
    Colon()

Colons (:) are used to signify indexing entire objects or dimensions at once.

Very few operations are defined on Colons directly; instead they are converted
by `to_indices` to an internal vector type (`Base.Slice`) to represent the
collection of indices they span before being used.
"""
immutable Colon
end
const (:) = Colon()

# For passing constants through type inference
immutable Val{T}
end

# used by interpolating quote and some other things in the front end
function vector_any(xs::ANY...)
    n = length(xs)
    a = Array{Any}(n)
    @inbounds for i = 1:n
        Core.arrayset(a,xs[i],i)
    end
    a
end

isempty(itr) = done(itr, start(itr))
