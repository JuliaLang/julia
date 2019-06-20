# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: CodeInfo, SimpleVector

const Callable = Union{Function,Type}

const Bottom = Union{}

"""
    AbstractSet{T}

Supertype for set-like types whose elements are of type `T`.
[`Set`](@ref), [`BitSet`](@ref) and other types are subtypes of this.
"""
abstract type AbstractSet{T} end

"""
    AbstractDict{K, V}

Supertype for dictionary-like types with keys of type `K` and values of type `V`.
[`Dict`](@ref), [`IdDict`](@ref) and other types are subtypes of this.
An `AbstractDict{K, V}` should be an iterator of `Pair{K, V}`.
"""
abstract type AbstractDict{K,V} end

# The real @inline macro is not available until after array.jl, so this
# internal macro splices the meta Expr directly into the function body.
macro _inline_meta()
    Expr(:meta, :inline)
end
macro _noinline_meta()
    Expr(:meta, :noinline)
end

macro _gc_preserve_begin(arg1)
    Expr(:gc_preserve_begin, esc(arg1))
end

macro _gc_preserve_end(token)
    Expr(:gc_preserve_end, esc(token))
end

"""
    @nospecialize

Applied to a function argument name, hints to the compiler that the method
should not be specialized for different types of that argument,
but instead to use precisely the declared type for each argument.
This is only a hint for avoiding excess code generation.
Can be applied to an argument within a formal argument list,
or in the function body.
When applied to an argument, the macro must wrap the entire argument expression.
When used in a function body, the macro must occur in statement position and
before any code.

When used without arguments, it applies to all arguments of the parent scope.
In local scope, this means all arguments of the containing function.
In global (top-level) scope, this means all methods subsequently defined in the current module.

Specialization can reset back to the default by using [`@specialize`](@ref).

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

@nospecialize
f(y) = [x for x in y]
@specialize
```
"""
macro nospecialize(vars...)
    if nfields(vars) === 1
        # in argument position, need to fix `@nospecialize x=v` to `@nospecialize (kw x v)`
        var = getfield(vars, 1)
        if isa(var, Expr) && var.head === :(=)
            var.head = :kw
        end
    end
    return Expr(:meta, :nospecialize, vars...)
end

"""
    @specialize

Reset the specialization hint for an argument back to the default.
For details, see [`@nospecialize`](@ref).
"""
macro specialize(vars...)
    if nfields(vars) === 1
        # in argument position, need to fix `@specialize x=v` to `@specialize (kw x v)`
        var = getfield(vars, 1)
        if isa(var, Expr) && var.head === :(=)
            var.head = :kw
        end
    end
    return Expr(:meta, :specialize, vars...)
end

"""
    @isdefined s -> Bool

Tests whether variable `s` is defined in the current scope.

See also [`isdefined`](@ref).

# Examples
```jldoctest
julia> @isdefined newvar
false

julia> newvar = 1
1

julia> @isdefined newvar
true

julia> function f()
           println(@isdefined x)
           x = 3
           println(@isdefined x)
       end
f (generic function with 1 method)

julia> f()
false
true
```
"""
macro isdefined(s::Symbol)
    return Expr(:escape, Expr(:isdefined, s))
end

macro _pure_meta()
    return Expr(:meta, :pure)
end
# another version of inlining that propagates an inbounds context
macro _propagate_inbounds_meta()
    return Expr(:meta, :inline, :propagate_inbounds)
end

function iterate end

"""
    convert(T, x)

Convert `x` to a value of type `T`.

If `T` is an [`Integer`](@ref) type, an [`InexactError`](@ref) will be raised if `x`
is not representable by `T`, for example if `x` is not integer-valued, or is outside the
range supported by `T`.

# Examples
```jldoctest
julia> convert(Int, 3.0)
3

julia> convert(Int, 3.5)
ERROR: InexactError: Int64(3.5)
Stacktrace:
[...]
```

If `T` is a [`AbstractFloat`](@ref) or [`Rational`](@ref) type,
then it will return the closest value to `x` representable by `T`.

```jldoctest
julia> x = 1/3
0.3333333333333333

julia> convert(Float32, x)
0.33333334f0

julia> convert(Rational{Int32}, x)
1//3

julia> convert(Rational{Int64}, x)
6004799503160661//18014398509481984
```

If `T` is a collection type and `x` a collection, the result of
`convert(T, x)` may alias all or part of `x`.
```jldoctest
julia> x = Int[1, 2, 3];

julia> y = convert(Vector{Int}, x);

julia> y === x
true
```
"""
function convert end

convert(::Type{Union{}}, x) = throw(MethodError(convert, (Union{}, x)))
convert(::Type{Any}, x) = x
convert(::Type{T}, x::T) where {T} = x
convert(::Type{Type}, x::Type) = x # the ssair optimizer is strongly dependent on this method existing to avoid over-specialization
                                   # in the absence of inlining-enabled
                                   # (due to fields typed as `Type`, which is generally a bad idea)
# These end up being called during bootstrap and then would be invalidated if not for the following:
convert(::Type{String}, x::String) = x

"""
    @eval [mod,] ex

Evaluate an expression with values interpolated into it using `eval`.
If two arguments are provided, the first is the module to evaluate in.
"""
macro eval(ex)
    :(Core.eval($__module__, $(Expr(:quote,ex))))
end
macro eval(mod, ex)
    :(Core.eval($(esc(mod)), $(Expr(:quote,ex))))
end

argtail(x, rest...) = rest

"""
    tail(x::Tuple)::Tuple

Return a `Tuple` consisting of all but the first component of `x`.

# Examples
```jldoctest
julia> Base.tail((1,2,3))
(2, 3)

julia> Base.tail(())
ERROR: ArgumentError: Cannot call tail on an empty tuple.
```
"""
tail(x::Tuple) = argtail(x...)
tail(::Tuple{}) = throw(ArgumentError("Cannot call tail on an empty tuple."))

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
    return isa(t, DataType) && (t::DataType).name === _va_typename
end

function isvatuple(@nospecialize(t))
    t = unwrap_unionall(t)
    if isa(t, DataType)
        n = length(t.parameters)
        return n > 0 && isvarargtype(t.parameters[n])
    end
    return false
end

function unwrapva(@nospecialize(t))
    # NOTE: this returns a related type, but it's NOT a subtype of the original tuple
    t2 = unwrap_unionall(t)
    return isvarargtype(t2) ? rewrap_unionall(t2.parameters[1], t) : t
end

function unconstrain_vararg_length(@nospecialize(va))
    # construct a new Vararg type where its length is unconstrained,
    # but its element type still captures any dependencies the input
    # element type may have had on the input length
    T = unwrap_unionall(va).parameters[1]
    return rewrap_unionall(Vararg{T}, va)
end

typename(a) = error("typename does not apply to this type")
typename(a::DataType) = a.name
function typename(a::Union)
    ta = typename(a.a)
    tb = typename(a.b)
    ta === tb || error("typename does not apply to unions whose components have different typenames")
    return tb
end
typename(union::UnionAll) = typename(union.body)

_tuple_error(T::Type, x) = (@_noinline_meta; throw(MethodError(convert, (T, x))))

convert(::Type{T}, x::T) where {T<:Tuple} = x
function convert(::Type{T}, x::NTuple{N,Any}) where {N, T<:Tuple}
    # First see if there could be any conversion of the input type that'd be a subtype of the output.
    # If not, we'll throw an explicit MethodError (otherwise, it might throw a typeassert).
    if typeintersect(NTuple{N,Any}, T) === Union{}
        _tuple_error(T, x)
    end
    cvt1(n) = (@_inline_meta; convert(fieldtype(T, n), getfield(x, n, #=boundscheck=#false)))
    return ntuple(cvt1, Val(N))::NTuple{N,Any}
end

# optimizations?
# converting to tuple types of fixed length
#convert(::Type{T}, x::T) where {N, T<:NTuple{N,Any}} = x
#convert(::Type{T}, x::NTuple{N,Any}) where {N, T<:NTuple{N,Any}} =
#    ntuple(n -> convert(fieldtype(T, n), x[n]), Val(N))
#convert(::Type{T}, x::Tuple{Vararg{Any}}) where {N, T<:NTuple{N,Any}} =
#    throw(MethodError(convert, (T, x)))
# converting to tuple types of indefinite length
#convert(::Type{Tuple{Vararg{V}}}, x::Tuple{Vararg{V}}) where {V} = x
#convert(::Type{NTuple{N, V}}, x::NTuple{N, V}) where {N, V} = x
#function convert(T::Type{Tuple{Vararg{V}}}, x::Tuple) where {V}
#    @isdefined(V) || (V = fieldtype(T, 1))
#    return map(t -> convert(V, t), x)
#end
#function convert(T::Type{NTuple{N, V}}, x::NTuple{N, Any}) where {N, V}
#    @isdefined(V) || (V = fieldtype(T, 1))
#    return map(t -> convert(V, t), x)
#end
# short tuples
#convert(::Type{Tuple{}}, ::Tuple{}) = ()
#convert(::Type{Tuple{S}}, x::Tuple{S}) where {S} = x
#convert(::Type{Tuple{S, T}}, x::Tuple{S, T}) where {S, T} = x
#convert(::Type{Tuple{S, T, U}}, x::Tuple{S, T, U}) where {S, T, U} = x
#convert(::Type{Tuple{S}}, x::Tuple{Any}) where {S} = (convert(S, x[1]),)
#convert(::Type{Tuple{S, T}}, x::Tuple{Any, Any}) where {S, T} = (convert(S, x[1]), convert(T, x[2]),)
#convert(::Type{Tuple{S, T, U}}, x::Tuple{Any, Any, Any}) where {S, T, U} = (convert(S, x[1]), convert(T, x[2]), convert(U, x[3]))
#convert(::Type{Tuple{}}, x::Tuple) = _tuple_error(Tuple{}, x)
#convert(::Type{Tuple{S}}, x::Tuple) = _tuple_error(Tuple{S}, x)
#convert(::Type{Tuple{S, T}}, x::Tuple{Any, Any}) where {S, T} =_tuple_error(Tuple{S, T}, x)
#convert(::Type{Tuple{S, T, U}}, x::Tuple{Any, Any, Any}) where {S, T, U} = _tuple_error(Tuple{S, T, U}, x)

"""
    oftype(x, y)

Convert `y` to the type of `x` (`convert(typeof(x), y)`).

# Examples
```jldoctest
julia> x = 4;

julia> y = 3.;

julia> oftype(x, y)
3

julia> oftype(y, x)
4.0
```
"""
oftype(x, y) = convert(typeof(x), y)

unsigned(x::Int) = reinterpret(UInt, x)
signed(x::UInt) = reinterpret(Int, x)

"""
    cconvert(T,x)

Convert `x` to a value to be passed to C code as type `T`, typically by calling `convert(T, x)`.

In cases where `x` cannot be safely converted to `T`, unlike [`convert`](@ref), `cconvert` may
return an object of a type different from `T`, which however is suitable for
[`unsafe_convert`](@ref) to handle. The result of this function should be kept valid (for the GC)
until the result of [`unsafe_convert`](@ref) is not needed anymore.
This can be used to allocate memory that will be accessed by the `ccall`.
If multiple objects need to be allocated, a tuple of the objects can be used as return value.

Neither `convert` nor `cconvert` should take a Julia object and turn it into a `Ptr`.
"""
function cconvert end

cconvert(T::Type, x) = convert(T, x) # do the conversion eagerly in most cases
cconvert(::Type{<:Ptr}, x) = x # but defer the conversion to Ptr to unsafe_convert
unsafe_convert(::Type{T}, x::T) where {T} = x # unsafe_convert (like convert) defaults to assuming the convert occurred
unsafe_convert(::Type{T}, x::T) where {T<:Ptr} = x  # to resolve ambiguity with the next method
unsafe_convert(::Type{P}, x::Ptr) where {P<:Ptr} = convert(P, x)

"""
    reinterpret(type, A)

Change the type-interpretation of a block of memory.
For arrays, this constructs a view of the array with the same binary data as the given
array, but with the specified element type.
For example,
`reinterpret(Float32, UInt32(7))` interprets the 4 bytes corresponding to `UInt32(7)` as a
[`Float32`](@ref).

# Examples
```jldoctest
julia> reinterpret(Float32, UInt32(7))
1.0f-44

julia> reinterpret(Float32, UInt32[1 2 3 4 5])
1×5 reinterpret(Float32, ::Matrix{UInt32}):
 1.0f-45  3.0f-45  4.0f-45  6.0f-45  7.0f-45
```
"""
reinterpret(::Type{T}, x) where {T} = bitcast(T, x)
reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)
reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16,x)

"""
    sizeof(T::DataType)
    sizeof(obj)

Size, in bytes, of the canonical binary representation of the given `DataType` `T`, if any.
Size, in bytes, of object `obj` if it is not `DataType`.

# Examples
```jldoctest
julia> sizeof(Float32)
4

julia> sizeof(ComplexF64)
16

julia> sizeof(1.0)
8

julia> sizeof([1.0:10.0;])
80
```

If `DataType` `T` does not have a specific size, an error is thrown.

```jldoctest
julia> sizeof(AbstractArray)
ERROR: Abstract type AbstractArray does not have a definite size.
Stacktrace:
[...]
```
"""
sizeof(x) = Core.sizeof(x)

# simple Array{Any} operations needed for bootstrap
@eval setindex!(A::Array{Any}, @nospecialize(x), i::Int) = arrayset($(Expr(:boundscheck)), A, x, i)

"""
    precompile(f, args::Tuple{Vararg{Any}})

Compile the given function `f` for the argument tuple (of types) `args`, but do not execute it.
"""
function precompile(@nospecialize(f), args::Tuple)
    ccall(:jl_compile_hint, Int32, (Any,), Tuple{Core.Typeof(f), args...}) != 0
end

function precompile(argt::Type)
    ccall(:jl_compile_hint, Int32, (Any,), argt) != 0
end

"""
    esc(e)

Only valid in the context of an [`Expr`](@ref) returned from a macro. Prevents the macro hygiene
pass from turning embedded variables into gensym variables. See the [Macros](@ref man-macros)
section of the Metaprogramming chapter of the manual for more details and examples.
"""
esc(@nospecialize(e)) = Expr(:escape, e)

"""
    @boundscheck(blk)

Annotates the expression `blk` as a bounds checking block, allowing it to be elided by [`@inbounds`](@ref).

!!! note
    The function in which `@boundscheck` is written must be inlined into
    its caller in order for `@inbounds` to have effect.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> @inline function g(A, i)
           @boundscheck checkbounds(A, i)
           return "accessing (\$A)[\$i]"
       end;

julia> f1() = return g(1:2, -1);

julia> f2() = @inbounds return g(1:2, -1);

julia> f1()
ERROR: BoundsError: attempt to access 2-element UnitRange{Int64} at index [-1]
Stacktrace:
 [1] throw_boundserror(::UnitRange{Int64}, ::Tuple{Int64}) at ./abstractarray.jl:455
 [2] checkbounds at ./abstractarray.jl:420 [inlined]
 [3] g at ./none:2 [inlined]
 [4] f1() at ./none:1
 [5] top-level scope

julia> f2()
"accessing (1:2)[-1]"
```

!!! warning

    The `@boundscheck` annotation allows you, as a library writer, to opt-in to
    allowing *other code* to remove your bounds checks with [`@inbounds`](@ref).
    As noted there, the caller must verify—using information they can access—that
    their accesses are valid before using `@inbounds`. For indexing into your
    [`AbstractArray`](@ref) subclasses, for example, this involves checking the
    indices against its [`size`](@ref). Therefore, `@boundscheck` annotations
    should only be added to a [`getindex`](@ref) or [`setindex!`](@ref)
    implementation after you are certain its behavior is correct.
"""
macro boundscheck(blk)
    return Expr(:if, Expr(:boundscheck), esc(blk))
end

"""
    @inbounds(blk)

Eliminates array bounds checking within expressions.

In the example below the in-range check for referencing
element `i` of array `A` is skipped to improve performance.

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
    Only use `@inbounds` when it is certain from the information locally available
    that all accesses are in bounds.
"""
macro inbounds(blk)
    return Expr(:block,
        Expr(:inbounds, true),
        Expr(:local, Expr(:(=), :val, esc(blk))),
        Expr(:inbounds, :pop),
        :val)
end

"""
    @label name

Labels a statement with the symbolic label `name`. The label marks the end-point
of an unconditional jump with [`@goto name`](@ref).
"""
macro label(name::Symbol)
    return esc(Expr(:symboliclabel, name))
end

"""
    @goto name

`@goto name` unconditionally jumps to the statement at the location [`@label name`](@ref).

`@label` and `@goto` cannot create jumps to different top-level statements. Attempts cause an
error. To still use `@goto`, enclose the `@label` and `@goto` in a block.
"""
macro goto(name::Symbol)
    return esc(Expr(:symbolicgoto, name))
end

# SimpleVector

function getindex(v::SimpleVector, i::Int)
    @boundscheck if !(1 <= i <= length(v))
        throw(BoundsError(v,i))
    end
    t = @_gc_preserve_begin v
    x = unsafe_load(convert(Ptr{Ptr{Cvoid}},pointer_from_objref(v)) + i*sizeof(Ptr))
    x == C_NULL && throw(UndefRefError())
    o = unsafe_pointer_to_objref(x)
    @_gc_preserve_end t
    return o
end

function length(v::SimpleVector)
    t = @_gc_preserve_begin v
    l = unsafe_load(convert(Ptr{Int},pointer_from_objref(v)))
    @_gc_preserve_end t
    return l
end
firstindex(v::SimpleVector) = 1
lastindex(v::SimpleVector) = length(v)
iterate(v::SimpleVector, i=1) = (length(v) < i ? nothing : (v[i], i + 1))
eltype(::Type{SimpleVector}) = Any
keys(v::SimpleVector) = OneTo(length(v))
isempty(v::SimpleVector) = (length(v) == 0)
axes(v::SimpleVector) = (OneTo(length(v)),)
axes(v::SimpleVector, d::Integer) = d <= 1 ? axes(v)[d] : OneTo(1)

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

Test whether the given array has a value associated with index `i`. Return `false`
if the index is out of bounds, or has an undefined reference.

# Examples
```jldoctest
julia> isassigned(rand(3, 3), 5)
true

julia> isassigned(rand(3, 3), 3 * 3 + 1)
false

julia> mutable struct Foo end

julia> v = similar(rand(3), Foo)
3-element Vector{Foo}:
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
    t = @_gc_preserve_begin v
    x = unsafe_load(convert(Ptr{Ptr{Cvoid}},pointer_from_objref(v)) + i*sizeof(Ptr))
    @_gc_preserve_end t
    return x != C_NULL
end


"""
    Colon()

Colons (:) are used to signify indexing entire objects or dimensions at once.

Very few operations are defined on Colons directly; instead they are converted
by [`to_indices`](@ref) to an internal vector type (`Base.Slice`) to represent the
collection of indices they span before being used.

The singleton instance of `Colon` is also a function used to construct ranges;
see [`:`](@ref).
"""
struct Colon <: Function
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

"""
    invokelatest(f, args...; kwargs...)

Calls `f(args...; kwargs...)`, but guarantees that the most recent method of `f`
will be executed.   This is useful in specialized circumstances,
e.g. long-running event loops or callback functions that may
call obsolete versions of a function `f`.
(The drawback is that `invokelatest` is somewhat slower than calling
`f` directly, and the type of the result cannot be inferred by the compiler.)
"""
function invokelatest(@nospecialize(f), @nospecialize args...; kwargs...)
    if isempty(kwargs)
        return Core._apply_latest(f, args)
    end
    # We use a closure (`inner`) to handle kwargs.
    inner() = f(args...; kwargs...)
    Core._apply_latest(inner)
end

"""
    invoke_in_world(world, f, args...; kwargs...)

Call `f(args...; kwargs...)` in a fixed world age, `world`.

This is useful for infrastructure running in the user's Julia session which is
not part of the user's program. For example, things related to the REPL, editor
support libraries, etc. In these cases it can be useful to prevent unwanted
method invalidation and recompilation latency, and to prevent the user from
breaking supporting infrastructure by mistake.

The current world age can be queried using [`Base.get_world_counter()`](@ref)
and stored for later use within the lifetime of the current Julia session, or
when serializing and reloading the system image.

Technically, `invoke_in_world` will prevent any function called by `f` from
being extended by the user during their Julia session. That is, generic
function method tables seen by `f` (and any functions it calls) will be frozen
as they existed at the given `world` age. In a sense, this is like the opposite
of [`invokelatest`](@ref).

!!! note
    It is not valid to store world ages obtained in precompilation for later use.
    This is because precompilation generates a "parallel universe" where the
    world age refers to system state unrelated to the main Julia session.
"""
function invoke_in_world(world::UInt, @nospecialize(f), @nospecialize args...; kwargs...)
    if isempty(kwargs)
        return Core._apply_in_world(world, f, args)
    end
    inner() = f(args...; kwargs...)
    Core._apply_in_world(world, inner)
end

# TODO: possibly make this an intrinsic
inferencebarrier(@nospecialize(x)) = Ref{Any}(x)[]

"""
    isempty(collection) -> Bool

Determine whether a collection is empty (has no elements).

# Examples
```jldoctest
julia> isempty([])
true

julia> isempty([1 2 3])
false
```
"""
function isempty(itr)
    d = isdone(itr)
    d !== missing && return d
    iterate(itr) === nothing
end

"""
    values(iterator)

For an iterator or collection that has keys and values, return an iterator
over the values.
This function simply returns its argument by default, since the elements
of a general iterator are normally considered its "values".

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2);

julia> values(d)
ValueIterator for a Dict{String,Int64} with 2 entries. Values:
  2
  1

julia> values([2])
1-element Vector{Int64}:
 2
```
"""
values(itr) = itr

"""
    Missing

A type with no fields whose singleton instance [`missing`](@ref) is used
to represent missing values.
"""
struct Missing end

"""
    missing

The singleton instance of type [`Missing`](@ref) representing a missing value.
"""
const missing = Missing()

"""
    ismissing(x)

Indicate whether `x` is [`missing`](@ref).
"""
ismissing(::Any) = false
ismissing(::Missing) = true

function popfirst! end

"""
    peek(stream[, T=UInt8])

Read and return a value of type `T` from a stream without advancing the current position
in the stream.

# Examples

```jldoctest
julia> b = IOBuffer("julia");

julia> peek(b)
0x6a

julia> position(b)
0

julia> peek(b, Char)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```

!!! compat "Julia 1.5"
    The method which accepts a type requires Julia 1.5 or later.
"""
function peek end

"""
    @__LINE__ -> Int

Expand to the line number of the location of the macrocall.
Return `0` if the line number could not be determined.
"""
macro __LINE__()
    return __source__.line
end

# Iteration
"""
    isdone(itr, state...) -> Union{Bool, Missing}

This function provides a fast-path hint for iterator completion.
This is useful for mutable iterators that want to avoid having elements
consumed, if they are not going to be exposed to the user (e.g. to check
for done-ness in `isempty` or `zip`). Mutable iterators that want to
opt into this feature should define an isdone method that returns
true/false depending on whether the iterator is done or not. Stateless
iterators need not implement this function. If the result is `missing`,
callers may go ahead and compute `iterate(x, state...) === nothing` to
compute a definite answer.
"""
isdone(itr, state...) = missing

"""
    iterate(iter [, state]) -> Union{Nothing, Tuple{Any, Any}}

Advance the iterator to obtain the next element. If no elements
remain, `nothing` should be returned. Otherwise, a 2-tuple of the
next element and the new iteration state should be returned.
"""
function iterate end

"""
    isiterable(T) -> Bool

Test if type `T` is an iterable collection type or not,
that is whether it has an `iterate` method or not.
"""
function isiterable(T)::Bool
    return hasmethod(iterate, Tuple{T})
end
