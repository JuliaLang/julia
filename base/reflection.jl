# This file is a part of Julia. License is MIT: https://julialang.org/license

# name and module reflection

"""
    nameof(m::Module) -> Symbol

Get the name of a `Module` as a `Symbol`.

# Examples
```jldoctest
julia> nameof(Base.Broadcast)
:Broadcast
```
"""
nameof(m::Module) = ccall(:jl_module_name, Ref{Symbol}, (Any,), m)

"""
    parentmodule(m::Module) -> Module

Get a module's enclosing `Module`. `Main` is its own parent.

# Examples
```jldoctest
julia> parentmodule(Main)
Main

julia> parentmodule(Base.Broadcast)
Base
```
"""
parentmodule(m::Module) = ccall(:jl_module_parent, Ref{Module}, (Any,), m)

"""
    moduleroot(m::Module) -> Module

Find the root module of a given module. This is the first module in the chain of
parent modules of `m` which is either a registered root module or which is its
own parent module.
"""
function moduleroot(m::Module)
    while true
        is_root_module(m) && return m
        p = parentmodule(m)
        p == m && return m
        m = p
    end
end

"""
    @__MODULE__ -> Module

Get the `Module` of the toplevel eval,
which is the `Module` code is currently being read from.
"""
macro __MODULE__()
    return __module__
end

"""
    fullname(m::Module)

Get the fully-qualified name of a module as a tuple of symbols. For example,

# Examples
```jldoctest
julia> fullname(Base.Iterators)
(:Base, :Iterators)

julia> fullname(Main)
(:Main,)
```
"""
function fullname(m::Module)
    mn = nameof(m)
    if m === Main || m === Base || m === Core
        return (mn,)
    end
    mp = parentmodule(m)
    if mp === m
        return (mn,)
    end
    return (fullname(mp)..., mn)
end

"""
    names(x::Module; all::Bool = false, imported::Bool = false)

Get an array of the names exported by a `Module`, excluding deprecated names.
If `all` is true, then the list also includes non-exported names defined in the module,
deprecated names, and compiler-generated names.
If `imported` is true, then names explicitly imported from other modules
are also included.

As a special case, all names defined in `Main` are considered \"exported\",
since it is not idiomatic to explicitly export names from `Main`.
"""
names(m::Module; all::Bool = false, imported::Bool = false) =
    sort!(ccall(:jl_module_names, Array{Symbol,1}, (Any, Cint, Cint), m, all, imported))

isexported(m::Module, s::Symbol) = ccall(:jl_module_exports_p, Cint, (Any, Any), m, s) != 0
isdeprecated(m::Module, s::Symbol) = ccall(:jl_is_binding_deprecated, Cint, (Any, Any), m, s) != 0
isbindingresolved(m::Module, var::Symbol) = ccall(:jl_binding_resolved_p, Cint, (Any, Any), m, var) != 0

function binding_module(m::Module, s::Symbol)
    p = ccall(:jl_get_module_of_binding, Ptr{Cvoid}, (Any, Any), m, s)
    p == C_NULL && return m
    return unsafe_pointer_to_objref(p)::Module
end

function resolve(g::GlobalRef; force::Bool=false)
    if force || isbindingresolved(g.mod, g.name)
        return GlobalRef(binding_module(g.mod, g.name), g.name)
    end
    return g
end

"""
    fieldname(x::DataType, i::Integer)

Get the name of field `i` of a `DataType`.

# Examples
```jldoctest
julia> fieldname(Rational, 1)
:num

julia> fieldname(Rational, 2)
:den
```
"""
function fieldname(t::DataType, i::Integer)
    names = isdefined(t, :names) ? t.names : t.name.names
    n_fields = length(names)
    field_label = n_fields == 1 ? "field" : "fields"
    i > n_fields && throw(ArgumentError("Cannot access field $i since type $t only has $n_fields $field_label."))
    i < 1 && throw(ArgumentError("Field numbers must be positive integers. $i is invalid."))
    return names[i]::Symbol
end

fieldname(t::UnionAll, i::Integer) = fieldname(unwrap_unionall(t), i)
fieldname(t::Type{<:Tuple}, i::Integer) =
    i < 1 || i > fieldcount(t) ? throw(BoundsError(t, i)) : Int(i)

"""
    fieldnames(x::DataType)

Get a tuple with the names of the fields of a `DataType`.

# Examples
```jldoctest
julia> fieldnames(Rational)
(:num, :den)
```
"""
fieldnames(t::DataType) = ntuple(i -> fieldname(t, i), fieldcount(t))
fieldnames(t::UnionAll) = fieldnames(unwrap_unionall(t))
fieldnames(t::Type{<:Tuple}) = ntuple(identity, fieldcount(t))

"""
    nameof(t::DataType) -> Symbol

Get the name of a (potentially `UnionAll`-wrapped) `DataType` (without its parent module)
as a symbol.

# Examples
```jldoctest
julia> module Foo
           struct S{T}
           end
       end
Foo

julia> nameof(Foo.S{T} where T)
:S
```
"""
nameof(t::DataType) = t.name.name
nameof(t::UnionAll) = nameof(unwrap_unionall(t))

"""
    parentmodule(t::DataType) -> Module

Determine the module containing the definition of a (potentially `UnionAll`-wrapped) `DataType`.

# Examples
```jldoctest
julia> module Foo
           struct Int end
       end
Foo

julia> parentmodule(Int)
Core

julia> parentmodule(Foo.Int)
Foo
```
"""
parentmodule(t::DataType) = t.name.module
parentmodule(t::UnionAll) = parentmodule(unwrap_unionall(t))

"""
    isconst(m::Module, s::Symbol) -> Bool

Determine whether a global is declared `const` in a given `Module`.
"""
isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Cint, (Any, Any), m, s) != 0

"""
    @isdefined s -> Bool

Tests whether variable `s` is defined in the current scope.

# Examples
```jldoctest
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
    return Expr(:isdefined, esc(s))
end

"""
    objectid(x)

Get a hash value for `x` based on object identity. `objectid(x)==objectid(y)` if `x === y`.
"""
objectid(@nospecialize(x)) = ccall(:jl_object_id, UInt, (Any,), x)

# concrete datatype predicates

struct DataTypeLayout
    nfields::UInt32
    alignment::UInt32
    # alignment : 28;
    # haspadding : 1;
    # pointerfree : 1;
    # fielddesc_type : 2;
end

"""
    Base.datatype_alignment(dt::DataType) -> Int

Memory allocation minimum alignment for instances of this type.
Can be called on any `isconcretetype`.
"""
function datatype_alignment(dt::DataType)
    @_pure_meta
    dt.layout == C_NULL && throw(UndefRefError())
    alignment = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment
    return Int(alignment & 0x1FF)
end

"""
    Base.datatype_haspadding(dt::DataType) -> Bool

Return whether the fields of instances of this type are packed in memory,
with no intervening padding bytes.
Can be called on any `isconcretetype`.
"""
function datatype_haspadding(dt::DataType)
    @_pure_meta
    dt.layout == C_NULL && throw(UndefRefError())
    alignment = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment
    return (alignment >> 9) & 1 == 1
end

"""
    Base.datatype_pointerfree(dt::DataType) -> Bool

Return whether instances of this type can contain references to gc-managed memory.
Can be called on any `isconcretetype`.
"""
function datatype_pointerfree(dt::DataType)
    @_pure_meta
    dt.layout == C_NULL && throw(UndefRefError())
    alignment = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment
    return (alignment >> 10) & 0xFFFFF == 0
end

"""
    Base.datatype_fielddesc_type(dt::DataType) -> Int

Return the size in bytes of each field-description entry in the layout array,
located at `(dt.layout + sizeof(DataTypeLayout))`.
Can be called on any `isconcretetype`.

See also [`Base.fieldoffset`](@ref).
"""
function datatype_fielddesc_type(dt::DataType)
    @_pure_meta
    dt.layout == C_NULL && throw(UndefRefError())
    alignment = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment
    return (alignment >> 30) & 3
end

"""
    isimmutable(v) -> Bool

Return `true` iff value `v` is immutable.  See [Mutable Composite Types](@ref)
for a discussion of immutability. Note that this function works on values, so if you give it
a type, it will tell you that a value of `DataType` is mutable.

# Examples
```jldoctest
julia> isimmutable(1)
true

julia> isimmutable([1,2])
false
```
"""
isimmutable(@nospecialize(x)) = (@_pure_meta; !typeof(x).mutable)

"""
    Base.isstructtype(T) -> Bool

Determine whether type `T` was declared as a struct type
(i.e. using the `struct` or `mutable struct` keyword).
"""
function isstructtype(@nospecialize(t::Type))
    @_pure_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false
    return length(t.types) != 0 || (t.size == 0 && !t.abstract)
end

"""
    Base.isprimitivetype(T) -> Bool

Determine whether type `T` was declared as a primitive type
(i.e. using the `primitive` keyword).
"""
function isprimitivetype(@nospecialize(t::Type))
    @_pure_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false
    return length(t.types) == 0 && t.size != 0 && !t.abstract
end

"""
    isbits(T)

Return `true` if type `T` is a "plain data" type,
meaning it is immutable and contains no references to other values,
only `primitive` types and other `isbits` types.
Typical examples are numeric types such as [`UInt8`](@ref),
[`Float64`](@ref), and [`Complex{Float64}`](@ref).
This category of types is significant since they are valid as type parameters,
may not track [`isdefined`](@ref) / [`isassigned`](@ref) status,
and have a defined layout that is compatible with C.

# Examples
```jldoctest
julia> isbits(Complex{Float64})
true

julia> isbits(Complex)
false
```
"""
isbits(@nospecialize(t::Type)) = (@_pure_meta; isa(t, DataType) && t.isbitstype)
isbits(@nospecialize x) = (@_pure_meta; typeof(x).isbitstype)

"""
    isdispatchtuple(T)

Determine whether type `T` is a tuple "leaf type",
meaning it could appear as a type signature in dispatch
and has no subtypes (or supertypes) which could appear in a call.
"""
isdispatchtuple(@nospecialize(t)) = (@_pure_meta; isa(t, DataType) && t.isdispatchtuple)

"""
    isconcretetype(T)

Determine whether type `T` is a concrete type, meaning it could have direct instances
(values `x` such that `typeof(x) === T`).

# Examples
```jldoctest
julia> isconcretetype(Complex)
false

julia> isconcretetype(Complex{Float32})
true

julia> isconcretetype(Vector{Complex})
true

julia> isconcretetype(Vector{Complex{Float32}})
true

julia> isconcretetype(Union{})
false

julia> isconcretetype(Union{Int,String})
false
```
"""
isconcretetype(@nospecialize(t)) = (@_pure_meta; isa(t, DataType) && t.isconcretetype)

"""
    Base.isabstracttype(T)

Determine whether type `T` was declared as an abstract type
(i.e. using the `abstract` keyword).

# Examples
```jldoctest
julia> Base.isabstracttype(AbstractArray)
true

julia> Base.isabstracttype(Vector)
false
```
"""
function isabstracttype(@nospecialize(t))
    @_pure_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    return isa(t, DataType) && t.abstract
end

"""
    Base.parameter_upper_bound(t::UnionAll, idx)

Determine the upper bound of a type parameter in the underlying datatype.
This method should generally not be relied upon:
code instead should usually use static parameters in dispatch to extract these values.

# Examples
```jldoctest
julia> struct Foo{T<:AbstractFloat, N}
           x::Tuple{T, N}
       end

julia> Base.parameter_upper_bound(Foo, 1)
AbstractFloat

julia> Base.parameter_upper_bound(Foo, 2)
Any
```
"""
function parameter_upper_bound(t::UnionAll, idx)
    @_pure_meta
    return rewrap_unionall((unwrap_unionall(t)::DataType).parameters[idx], t)
end

"""
    typeintersect(T, S)

Compute a type that contains the intersection of `T` and `S`. Usually this will be the
smallest such type or one close to it.
"""
typeintersect(@nospecialize(a),@nospecialize(b)) = (@_pure_meta; ccall(:jl_type_intersection, Any, (Any,Any), a, b))

"""
    fieldoffset(type, i)

The byte offset of field `i` of a type relative to the data start. For example, we could
use it in the following manner to summarize information about a struct:

```jldoctest
julia> structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:fieldcount(T)];

julia> structinfo(Base.Filesystem.StatStruct)
12-element Array{Tuple{UInt64,Symbol,DataType},1}:
 (0x0000000000000000, :device, UInt64)
 (0x0000000000000008, :inode, UInt64)
 (0x0000000000000010, :mode, UInt64)
 (0x0000000000000018, :nlink, Int64)
 (0x0000000000000020, :uid, UInt64)
 (0x0000000000000028, :gid, UInt64)
 (0x0000000000000030, :rdev, UInt64)
 (0x0000000000000038, :size, Int64)
 (0x0000000000000040, :blksize, Int64)
 (0x0000000000000048, :blocks, Int64)
 (0x0000000000000050, :mtime, Float64)
 (0x0000000000000058, :ctime, Float64)
```
"""
fieldoffset(x::DataType, idx::Integer) = (@_pure_meta; ccall(:jl_get_field_offset, Csize_t, (Any, Cint), x, idx))

"""
    fieldtype(T, name::Symbol | index::Int)

Determine the declared type of a field (specified by name or index) in a composite DataType `T`.

# Examples
```jldoctest
julia> struct Foo
           x::Int64
           y::String
       end

julia> fieldtype(Foo, :x)
Int64

julia> fieldtype(Foo, 2)
String
```
"""
fieldtype

"""
    fieldindex(T, name::Symbol, err:Bool=true)

Get the index of a named field, throwing an error if the field does not exist (when err==true)
or returning 0 (when err==false).

# Examples
```jldoctest
julia> struct Foo
           x::Int64
           y::String
       end

julia> Base.fieldindex(Foo, :z)
ERROR: type Foo has no field z
Stacktrace:
[...]

julia> Base.fieldindex(Foo, :z, false)
0
```
"""
function fieldindex(T::DataType, name::Symbol, err::Bool=true)
    return Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), T, name, err)+1)
end

"""
    fieldcount(t::Type)

Get the number of fields that an instance of the given type would have.
An error is thrown if the type is too abstract to determine this.
"""
function fieldcount(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = ccall(:jl_argument_datatype, Any, (Any,), t)
        if t === nothing
            error("type does not have a definite number of fields")
        end
        t = t::DataType
    elseif t == Union{}
        return 0
    end
    if !(t isa DataType)
        throw(TypeError(:fieldcount, "", Type, t))
    end
    if t.name === NamedTuple.body.body.name
        names, types = t.parameters
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount(types)
        end
        abstr = true
    else
        abstr = t.abstract || (t.name === Tuple.name && isvatuple(t))
    end
    if abstr
        error("type does not have a definite number of fields")
    end
    return length(t.types)
end

# return all instances, for types that can be enumerated

"""
    instances(T::Type)

Return a collection of all instances of the given type, if applicable. Mostly used for
enumerated types (see `@enum`).

# Example
```jldoctest
julia> @enum Color red blue green

julia> instances(Color)
(red::Color = 0, blue::Color = 1, green::Color = 2)
```
"""
function instances end

function to_tuple_type(@nospecialize(t))
    @_pure_meta
    if isa(t,Tuple) || isa(t,AbstractArray) || isa(t,SimpleVector)
        t = Tuple{t...}
    end
    if isa(t,Type) && t<:Tuple
        if !all(p->(isa(p,Type)||isa(p,TypeVar)), t.parameters)
            error("argument tuple type must contain only types")
        end
    else
        error("expected tuple type")
    end
    t
end

function signature_type(@nospecialize(f), @nospecialize(args))
    f_type = isa(f, Type) ? Type{f} : typeof(f)
    arg_types = isa(args, Type) ? args.parameters : args
    return Tuple{f_type, arg_types...}
end

"""
    code_lowered(f, types; generated = true)

Return an array of the lowered forms (IR) for the methods matching the given generic function
and type signature.

If `generated` is `false`, the returned `CodeInfo` instances will correspond to fallback
implementations. An error is thrown if no fallback implementation exists.
If `generated` is `true`, these `CodeInfo` instances will correspond to the method bodies
yielded by expanding the generators.

Note that an error will be thrown if `types` are not leaf types when `generated` is
`true` and the corresponding method is a `@generated` method.
"""
function code_lowered(@nospecialize(f), @nospecialize(t = Tuple); generated::Bool = true)
    return map(method_instances(f, t)) do m
        if generated && isgenerated(m)
            if isa(m, Core.MethodInstance)
                return Core.Compiler.get_staged(m)
            else # isa(m, Method)
                error("Could not expand generator for `@generated` method ", m, ". ",
                      "This can happen if the provided argument types (", t, ") are ",
                      "not leaf types, but the `generated` argument is `true`.")
            end
        end
        return uncompressed_ast(m)
    end
end

isgenerated(m::Method) = isdefined(m, :generator)
isgenerated(m::Core.MethodInstance) = isgenerated(m.def)

# low-level method lookup functions used by the compiler

unionlen(x::Union) = unionlen(x.a) + unionlen(x.b)
unionlen(@nospecialize(x)) = 1

_uniontypes(x::Union, ts) = (_uniontypes(x.a,ts); _uniontypes(x.b,ts); ts)
_uniontypes(@nospecialize(x), ts) = (push!(ts, x); ts)
uniontypes(@nospecialize(x)) = _uniontypes(x, Any[])

function _methods(@nospecialize(f), @nospecialize(t), lim::Int, world::UInt)
    tt = signature_type(f, t)
    return _methods_by_ftype(tt, lim, world)
end

function _methods_by_ftype(@nospecialize(t), lim::Int, world::UInt)
    return _methods_by_ftype(t, lim, world, UInt[typemin(UInt)], UInt[typemax(UInt)])
end
function _methods_by_ftype(@nospecialize(t), lim::Int, world::UInt, min::Array{UInt,1}, max::Array{UInt,1})
    return ccall(:jl_matching_methods, Any, (Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}), t, lim, 0, world, min, max)
end

# high-level, more convenient method lookup functions

# type for reflecting and pretty-printing a subset of methods
mutable struct MethodList
    ms::Array{Method,1}
    mt::Core.MethodTable
end

length(m::MethodList) = length(m.ms)
isempty(m::MethodList) = isempty(m.ms)
start(m::MethodList) = start(m.ms)
done(m::MethodList, s) = done(m.ms, s)
next(m::MethodList, s) = next(m.ms, s)

function MethodList(mt::Core.MethodTable)
    ms = Method[]
    visit(mt) do m
        push!(ms, m)
    end
    return MethodList(ms, mt)
end

"""
    methods(f, [types])

Returns the method table for `f`.

If `types` is specified, returns an array of methods whose types match.
"""
function methods(@nospecialize(f), @nospecialize(t))
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    t = to_tuple_type(t)
    world = typemax(UInt)
    return MethodList(Method[m[3] for m in _methods(f, t, -1, world)], typeof(f).name.mt)
end

methods(f::Core.Builtin) = MethodList(Method[], typeof(f).name.mt)

function methods_including_ambiguous(@nospecialize(f), @nospecialize(t))
    tt = signature_type(f, t)
    world = typemax(UInt)
    min = UInt[typemin(UInt)]
    max = UInt[typemax(UInt)]
    ms = ccall(:jl_matching_methods, Any, (Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}), tt, -1, 1, world, min, max)::Array{Any,1}
    return MethodList(Method[m[3] for m in ms], typeof(f).name.mt)
end
function methods(@nospecialize(f))
    # return all matches
    return methods(f, Tuple{Vararg{Any}})
end

function visit(f, mt::Core.MethodTable)
    mt.defs !== nothing && visit(f, mt.defs)
    nothing
end
function visit(f, mc::Core.TypeMapLevel)
    if mc.targ !== nothing
        e = mc.targ::Vector{Any}
        for i in 1:length(e)
            isassigned(e, i) && visit(f, e[i])
        end
    end
    if mc.arg1 !== nothing
        e = mc.arg1::Vector{Any}
        for i in 1:length(e)
            isassigned(e, i) && visit(f, e[i])
        end
    end
    mc.list !== nothing && visit(f, mc.list)
    mc.any !== nothing && visit(f, mc.any)
    nothing
end
function visit(f, d::Core.TypeMapEntry)
    while d !== nothing
        f(d.func)
        d = d.next
    end
    nothing
end

function length(mt::Core.MethodTable)
    n = 0
    visit(mt) do m
        n += 1
    end
    return n::Int
end
isempty(mt::Core.MethodTable) = (mt.defs === nothing)

uncompressed_ast(m::Method) = isdefined(m,:source) ? uncompressed_ast(m, m.source) :
                              isdefined(m,:generator) ? error("Method is @generated; try `code_lowered` instead.") :
                              error("Code for this Method is not available.")
uncompressed_ast(m::Method, s::CodeInfo) = s
uncompressed_ast(m::Method, s::Array{UInt8,1}) = ccall(:jl_uncompress_ast, Any, (Any, Any), m, s)::CodeInfo
uncompressed_ast(m::Core.MethodInstance) = uncompressed_ast(m.def)

function method_instances(@nospecialize(f), @nospecialize(t), world::UInt = typemax(UInt))
    tt = signature_type(f, t)
    results = Vector{Union{Method,Core.MethodInstance}}()
    for method_data in _methods_by_ftype(tt, -1, world)
        mtypes, msp, m = method_data
        instance = Core.Compiler.code_for_method(m, mtypes, msp, world, false)
        push!(results, ifelse(isa(instance, Core.MethodInstance), instance, m))
    end
    return results
end

# this type mirrors jl_cgparams_t (documented in julia.h)
struct CodegenParams
    cached::Cint

    track_allocations::Cint
    code_coverage::Cint
    static_alloc::Cint
    prefer_specsig::Cint

    module_setup::Any
    module_activation::Any
    raise_exception::Any

    CodegenParams(;cached::Bool=true,
                   track_allocations::Bool=true, code_coverage::Bool=true,
                   static_alloc::Bool=true, prefer_specsig::Bool=false,
                   module_setup=nothing, module_activation=nothing, raise_exception=nothing) =
        new(Cint(cached),
            Cint(track_allocations), Cint(code_coverage),
            Cint(static_alloc), Cint(prefer_specsig),
            module_setup, module_activation, raise_exception)
end

# give a decent error message if we try to instantiate a staged function on non-leaf types
function func_for_method_checked(m::Method, @nospecialize types)
    if isdefined(m, :generator) && !isdispatchtuple(types)
        error("cannot call @generated function `", m, "` ",
              "with abstract argument types: ", types)
    end
    return m
end

"""
    code_typed(f, types; optimize=true)

Returns an array of type-inferred lowered form (IR) for the methods matching the given
generic function and type signature. The keyword argument `optimize` controls whether
additional optimizations, such as inlining, are also applied.
"""
function code_typed(@nospecialize(f), @nospecialize(types=Tuple); optimize=true)
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    types = to_tuple_type(types)
    asts = []
    world = ccall(:jl_get_world_counter, UInt, ())
    params = Core.Compiler.Params(world)
    for x in _methods(f, types, -1, world)
        meth = func_for_method_checked(x[3], types)
        (_, code, ty) = Core.Compiler.typeinf_code(meth, x[1], x[2], optimize, optimize, params)
        code === nothing && error("inference not successful") # inference disabled?
        push!(asts, uncompressed_ast(meth, code) => ty)
    end
    return asts
end

function return_types(@nospecialize(f), @nospecialize(types=Tuple))
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    types = to_tuple_type(types)
    rt = []
    world = ccall(:jl_get_world_counter, UInt, ())
    params = Core.Compiler.Params(world)
    for x in _methods(f, types, -1, world)
        meth = func_for_method_checked(x[3], types)
        ty = Core.Compiler.typeinf_type(meth, x[1], x[2], true, params)
        ty === nothing && error("inference not successful") # inference disabled?
        push!(rt, ty)
    end
    return rt
end

"""
    which(f, types)

Returns the method of `f` (a `Method` object) that would be called for arguments of the given `types`.

If `types` is an abstract type, then the method that would be called by `invoke` is returned.
"""
function which(@nospecialize(f), @nospecialize(t))
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    t = to_tuple_type(t)
    tt = signature_type(f, t)
    m = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tt, typemax(UInt))
    if m === nothing
        error("no unique matching method found for the specified argument types")
    end
    return m.func::Method
end

"""
    which(module, symbol)

Return the module in which the binding for the variable referenced by `symbol` in `module` was created.
"""
function which(m::Module, s::Symbol)
    if !isdefined(m, s)
        error("\"$s\" is not defined in module $m")
    end
    return binding_module(m, s)
end

# function reflection
"""
    nameof(f::Function) -> Symbol

Get the name of a generic `Function` as a symbol, or `:anonymous`.
"""
nameof(f::Function) = typeof(f).name.mt.name

functionloc(m::Core.MethodInstance) = functionloc(m.def)

"""
    functionloc(m::Method)

Returns a tuple `(filename,line)` giving the location of a `Method` definition.
"""
function functionloc(m::Method)
    ln = m.line
    if ln <= 0
        error("could not determine location of method definition")
    end
    return (find_source_file(string(m.file)), ln)
end

"""
    functionloc(f::Function, types)

Returns a tuple `(filename,line)` giving the location of a generic `Function` definition.
"""
functionloc(@nospecialize(f), @nospecialize(types)) = functionloc(which(f,types))

function functionloc(@nospecialize(f))
    mt = methods(f)
    if isempty(mt)
        if isa(f, Function)
            error("function has no definitions")
        else
            error("object is not callable")
        end
    end
    if length(mt) > 1
        error("function has multiple methods; please specify a type signature")
    end
    return functionloc(first(mt))
end

"""
    parentmodule(f::Function) -> Module

Determine the module containing the (first) definition of a generic
function.
"""
parentmodule(f::Function) = parentmodule(typeof(f))

"""
    parentmodule(f::Function, types) -> Module

Determine the module containing a given definition of a generic function.
"""
function parentmodule(@nospecialize(f), @nospecialize(types))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    return first(m).module
end

"""
    hasmethod(f, Tuple type; world = typemax(UInt)) -> Bool

Determine whether the given generic function has a method matching the given
`Tuple` of argument types with the upper bound of world age given by `world`.

# Examples
```jldoctest
julia> hasmethod(length, Tuple{Array})
true
```
"""
function hasmethod(@nospecialize(f), @nospecialize(t); world = typemax(UInt))
    t = to_tuple_type(t)
    t = signature_type(f, t)
    return ccall(:jl_method_exists, Cint, (Any, Any, UInt), typeof(f).name.mt, t, world) != 0
end

"""
    isambiguous(m1, m2; ambiguous_bottom=false) -> Bool

Determine whether two methods `m1` and `m2` (typically of the same
function) are ambiguous.  This test is performed in the context of
other methods of the same function; in isolation, `m1` and `m2` might
be ambiguous, but if a third method resolving the ambiguity has been
defined, this returns `false`.

For parametric types, the `ambiguous_bottom` keyword argument controls whether
`Union{}` counts as an ambiguous intersection of type parameters – when `true`,
it is considered ambiguous, when `false` it is not.

# Examples
```jldoctest
julia> foo(x::Complex{<:Integer}) = 1
foo (generic function with 1 method)

julia> foo(x::Complex{<:Rational}) = 2
foo (generic function with 2 methods)

julia> m1, m2 = collect(methods(foo));

julia> typeintersect(m1.sig, m2.sig)
Tuple{#foo,Complex{Union{}}}

julia> Base.isambiguous(m1, m2, ambiguous_bottom=true)
true

julia> Base.isambiguous(m1, m2, ambiguous_bottom=false)
false
```
"""
function isambiguous(m1::Method, m2::Method; ambiguous_bottom::Bool=false)
    ti = typeintersect(m1.sig, m2.sig)
    ti === Bottom && return false
    if !ambiguous_bottom
        has_bottom_parameter(ti) && return false
    end
    ml = _methods_by_ftype(ti, -1, typemax(UInt))
    isempty(ml) && return true
    for m in ml
        if ti <: m[3].sig
            return false
        end
    end
    return true
end

"""
    delete_method(m::Method)

Make method `m` uncallable and force recompilation of any methods that use(d) it.
"""
function delete_method(m::Method)
    ccall(:jl_method_table_disable, Cvoid, (Any, Any), get_methodtable(m), m)
end

function get_methodtable(m::Method)
    ft = ccall(:jl_first_argument_datatype, Any, (Any,), m.sig)
    (ft::DataType).name.mt
end

"""
    has_bottom_parameter(t) -> Bool

Determine whether `t` is a Type for which one or more of its parameters is `Union{}`.
"""
function has_bottom_parameter(t::Type)
    ret = false
    for p in t.parameters
        ret |= (p == Bottom) || has_bottom_parameter(p)
    end
    ret
end
has_bottom_parameter(t::UnionAll) = has_bottom_parameter(unwrap_unionall(t))
has_bottom_parameter(t::Union) = has_bottom_parameter(t.a) & has_bottom_parameter(t.b)
has_bottom_parameter(t::TypeVar) = t.ub == Bottom || has_bottom_parameter(t.ub)
has_bottom_parameter(::Any) = false

min_world(m::Method) = reinterpret(UInt, m.min_world)
max_world(m::Method) = typemax(UInt)
min_world(m::Core.MethodInstance) = reinterpret(UInt, m.min_world)
max_world(m::Core.MethodInstance) = reinterpret(UInt, m.max_world)

"""
    propertynames(x, private=false)

Get a tuple or a vector of the properties (`x.property`) of an object `x`.
This is typically the same as [`fieldnames(typeof(x))`](@ref), but types
that overload [`getproperty`](@ref) should generally overload `propertynames`
as well to get the properties of an instance of the type.

`propertynames(x)` may return only "public" property names that are part
of the documented interface of `x`.   If you want it to also return "private"
fieldnames intended for internal use, pass `true` for the optional second argument.
REPL tab completion on `x.` shows only the `private=false` properties.
"""
propertynames(x) = fieldnames(typeof(x))
propertynames(m::Module) = names(m)
propertynames(x, private) = propertynames(x) # ignore private flag by default
