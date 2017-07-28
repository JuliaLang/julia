# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: CodeInfo

const Callable = Union{Function,Type}

const Bottom = Union{}

abstract type AbstractSet{T} end
abstract type Associative{K,V} end

# The real @inline macro is not available until after array.jl, so this
# internal macro splices the meta Expr directly into the function body.
macro _inline_meta()
    Expr(:meta, :inline)
end
macro _noinline_meta()
    Expr(:meta, :noinline)
end

"""
    @nospecialize

Applied to a function argument name, hints to the compiler that the method
should not be specialized for different types of that argument.
This is only a hint for avoiding excess code generation.
Can be applied to an argument within a formal argument list, or in the
function body.
When applied to an argument, the macro must wrap the entire argument
expression.
When used in a function body, the macro must occur in statement position and
before any code.

```julia
function example_function(@nospecialize x)
    ...
end

function example_function(@nospecialize(x = 1), y)
    ...
end

function example_function(x, y, z)
    @nospecialize x y
    ...
end
```
"""
macro nospecialize(var, vars...)
    if isa(var, Expr) && var.head === :(=)
        var.head = :kw
    end
    Expr(:meta, :nospecialize, var, vars...)
end

macro _pure_meta()
    Expr(:meta, :pure)
end
# another version of inlining that propagates an inbounds context
macro _propagate_inbounds_meta()
    Expr(:meta, :inline, :propagate_inbounds)
end

convert(::Type{Any}, @nospecialize(x)) = x
convert(::Type{T}, x::T) where {T} = x

convert(::Type{Tuple{}}, ::Tuple{}) = ()
convert(::Type{Tuple}, x::Tuple) = x
convert(::Type{Tuple{Vararg{T}}}, x::Tuple) where {T} = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)

"""
    @eval [mod,] ex

Evaluate an expression with values interpolated into it using `eval`.
If two arguments are provided, the first is the module to evaluate in.
"""
macro eval(ex)
    :(eval($__module__, $(Expr(:quote,ex))))
end
macro eval(mod, ex)
    :(eval($(esc(mod)), $(Expr(:quote,ex))))
end

argtail(x, rest...) = rest
tail(x::Tuple) = argtail(x...)

tuple_type_head(T::UnionAll) = (@_pure_meta; UnionAll(T.var, tuple_type_head(T.body)))
function tuple_type_head(T::DataType)
    @_pure_meta
    T.name === Tuple.name || throw(MethodError(tuple_type_head, (T,)))
    return unwrapva(T.parameters[1])
end
tuple_type_tail(T::UnionAll) = (@_pure_meta; UnionAll(T.var, tuple_type_tail(T.body)))
function tuple_type_tail(T::DataType)
    @_pure_meta
    T.name === Tuple.name || throw(MethodError(tuple_type_tail, (T,)))
    if isvatuple(T) && length(T.parameters) == 1
        return T
    end
    return Tuple{argtail(T.parameters...)...}
end

tuple_type_cons(::Type, ::Type{Union{}}) = Union{}
function tuple_type_cons(::Type{S}, ::Type{T}) where T<:Tuple where S
    @_pure_meta
    Tuple{S, T.parameters...}
end

function unwrap_unionall(@nospecialize(a))
    while isa(a,UnionAll)
        a = a.body
    end
    return a
end

function rewrap_unionall(@nospecialize(t), @nospecialize(u))
    if !isa(u, UnionAll)
        return t
    end
    return UnionAll(u.var, rewrap_unionall(t, u.body))
end

# replace TypeVars in all enclosing UnionAlls with fresh TypeVars
function rename_unionall(@nospecialize(u))
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
function isvarargtype(@nospecialize(t))
    t = unwrap_unionall(t)
    isa(t, DataType) && (t::DataType).name === _va_typename
end

isvatuple(t::DataType) = (n = length(t.parameters); n > 0 && isvarargtype(t.parameters[n]))
function unwrapva(@nospecialize(t))
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

convert(::Type{T}, x::Tuple{Any,Vararg{Any}}) where {T<:Tuple{Any,Vararg{Any}}} =
    tuple(convert(tuple_type_head(T),x[1]), convert(tuple_type_tail(T), tail(x))...)
convert(::Type{T}, x::T) where {T<:Tuple{Any,Vararg{Any}}} = x

oftype(x,c) = convert(typeof(x),c)

unsigned(x::Int) = reinterpret(UInt, x)
signed(x::UInt) = reinterpret(Int, x)

# conversions used by ccall
ptr_arg_cconvert(::Type{Ptr{T}}, x) where {T} = cconvert(T, x)
ptr_arg_unsafe_convert(::Type{Ptr{T}}, x) where {T} = unsafe_convert(T, x)
ptr_arg_unsafe_convert(::Type{Ptr{Void}}, x) = x

cconvert(T::Type, x) = convert(T, x) # do the conversion eagerly in most cases
cconvert(::Type{<:Ptr}, x) = x # but defer the conversion to Ptr to unsafe_convert
unsafe_convert(::Type{T}, x::T) where {T} = x # unsafe_convert (like convert) defaults to assuming the convert occurred
unsafe_convert(::Type{T}, x::T) where {T<:Ptr} = x  # to resolve ambiguity with the next method
unsafe_convert(::Type{P}, x::Ptr) where {P<:Ptr} = convert(P, x)

reinterpret(::Type{T}, x) where {T} = bitcast(T, x)
reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)
reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16,x)

sizeof(x) = Core.sizeof(x)

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    out = Vector{Any}(4)
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
setindex!(A::Array{Any}, @nospecialize(x), i::Int) = Core.arrayset(A, x, i)

function precompile(@nospecialize(f), args::Tuple)
    ccall(:jl_compile_hint, Int32, (Any,), Tuple{Core.Typeof(f), args...}) != 0
end

function precompile(argt::Type)
    ccall(:jl_compile_hint, Int32, (Any,), argt) != 0
end

"""
    esc(e)

Only valid in the context of an `Expr` returned from a macro. Prevents the macro hygiene
pass from turning embedded variables into gensym variables. See the [Macros](@ref man-macros)
section of the Metaprogramming chapter of the manual for more details and examples.
"""
esc(@nospecialize(e)) = Expr(:escape, e)

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

!!! warning

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

# TODO: add gc use intrinsic call instead of noinline
length(v::SimpleVector) = (@_noinline_meta; unsafe_load(convert(Ptr{Int},data_pointer_from_objref(v))))
endof(v::SimpleVector) = length(v)
start(v::SimpleVector) = 1
next(v::SimpleVector,i) = (v[i],i+1)
done(v::SimpleVector,i) = (i > length(v))
isempty(v::SimpleVector) = (length(v) == 0)
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

```jldoctest
julia> isassigned(rand(3, 3), 5)
true

julia> isassigned(rand(3, 3), 3 * 3 + 1)
false

julia> mutable struct Foo end

julia> v = similar(rand(3), Foo)
3-element Array{Foo,1}:
 #undef
 #undef
 #undef

julia> isassigned(v, 1)
false
```
"""
function isassigned end

function isassigned(v::SimpleVector, i::Int)
    @boundscheck 1 <= i <= length(v) || return false
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    return x != C_NULL
end

"""
    Colon()

Colons (:) are used to signify indexing entire objects or dimensions at once.

Very few operations are defined on Colons directly; instead they are converted
by [`to_indices`](@ref) to an internal vector type (`Base.Slice`) to represent the
collection of indices they span before being used.
"""
struct Colon
end
const (:) = Colon()

"""
    Val(c)

Return `Val{c}()`, which contains no run-time data. Types like this can be used to
pass the information between functions through the value `c`, which must be an `isbits`
value. The intent of this construct is to be able to dispatch on constants directly (at
compile time) without having to test the value of the constant at run time.

# Examples
```jldoctest
julia> f(::Val{true}) = "Good"
f (generic function with 1 method)

julia> f(::Val{false}) = "Bad"
f (generic function with 2 methods)

julia> f(Val(true))
"Good"
```
"""
struct Val{x}
end

Val(x) = (@_pure_meta; Val{x}())

# used by interpolating quote and some other things in the front end
function vector_any(@nospecialize xs...)
    n = length(xs)
    a = Vector{Any}(n)
    @inbounds for i = 1:n
        Core.arrayset(a,xs[i],i)
    end
    a
end

function as_kwargs(xs::Union{AbstractArray,Associative})
    n = length(xs)
    to = Vector{Any}(n*2)
    i = 1
    for (k, v) in xs
        to[i]   = k::Symbol
        to[i+1] = v
        i += 2
    end
    return to
end

function as_kwargs(xs)
    to = Vector{Any}(0)
    for (k, v) in xs
        ccall(:jl_array_ptr_1d_push2, Void, (Any, Any, Any), to, k::Symbol, v)
    end
    return to
end

isempty(itr) = done(itr, start(itr))

"""
    invokelatest(f, args...; kwargs...)

Calls `f(args...; kwargs...)`, but guarantees that the most recent method of `f`
will be executed.   This is useful in specialized circumstances,
e.g. long-running event loops or callback functions that may
call obsolete versions of a function `f`.
(The drawback is that `invokelatest` is somewhat slower than calling
`f` directly, and the type of the result cannot be inferred by the compiler.)
"""
function invokelatest(f, args...; kwargs...)
    # We use a closure (`inner`) to handle kwargs.
    inner() = f(args...; kwargs...)
    Core._apply_latest(inner)
end
