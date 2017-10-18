# This file is a part of Julia. License is MIT: https://julialang.org/license

# name and module reflection

"""
    module_name(m::Module) -> Symbol

Get the name of a `Module` as a `Symbol`.

# Examples
```jldoctest
julia> module_name(Base.LinAlg)
:LinAlg
```
"""
module_name(m::Module) = ccall(:jl_module_name, Ref{Symbol}, (Any,), m)

"""
    module_parent(m::Module) -> Module

Get a module's enclosing `Module`. `Main` is its own parent, as is `LastMain` after `workspace()`.

# Examples
```jldoctest
julia> module_parent(Main)
Main

julia> module_parent(Base.LinAlg.BLAS)
Base.LinAlg
```
"""
module_parent(m::Module) = ccall(:jl_module_parent, Ref{Module}, (Any,), m)

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
julia> fullname(Base.Pkg)
(:Base, :Pkg)

julia> fullname(Main)
()
```
"""
function fullname(m::Module)
    mn = module_name(m)
    if m === Main || m === Base || m === Core
        return (mn,)
    end
    mp = module_parent(m)
    if mp === m
        if mn !== :Main
            return (mn,)
        end
        # top-level module, not Main, called :Main => prior Main module
        n = (:Main,)
        this = Main
        while this !== m
            if isdefined(this, :LastMain)
                n = tuple(n..., :LastMain)
                this = this.LastMain
            else
                error("no reference to module ", mn)
            end
        end
        return n
    end
    return tuple(fullname(mp)..., mn)
end

"""
    names(x::Module, all::Bool=false, imported::Bool=false)

Get an array of the names exported by a `Module`, excluding deprecated names.
If `all` is true, then the list also includes non-exported names defined in the module,
deprecated names, and compiler-generated names.
If `imported` is true, then names explicitly imported from other modules
are also included.

As a special case, all names defined in `Main` are considered \"exported\",
since it is not idiomatic to explicitly export names from `Main`.
"""
names(m::Module, all::Bool=false, imported::Bool=false) = sort!(ccall(:jl_module_names, Array{Symbol,1}, (Any, Cint, Cint), m, all, imported))

isexported(m::Module, s::Symbol) = ccall(:jl_module_exports_p, Cint, (Any, Any), m, s) != 0
isdeprecated(m::Module, s::Symbol) = ccall(:jl_is_binding_deprecated, Cint, (Any, Any), m, s) != 0
isbindingresolved(m::Module, var::Symbol) = ccall(:jl_binding_resolved_p, Cint, (Any, Any), m, var) != 0

function binding_module(m::Module, s::Symbol)
    p = ccall(:jl_get_module_of_binding, Ptr{Void}, (Any, Any), m, s)
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
julia> fieldname(SparseMatrixCSC, 1)
:m

julia> fieldname(SparseMatrixCSC, 5)
:nzval
```
"""
function fieldname(t::DataType, i::Integer)
    n_fields = length(t.name.names)
    field_label = n_fields == 1 ? "field" : "fields"
    i > n_fields && throw(ArgumentError("Cannot access field $i since type $t only has $n_fields $field_label."))
    i < 1 && throw(ArgumentError("Field numbers must be positive integers. $i is invalid."))
    return t.name.names[i]::Symbol
end
fieldname(t::UnionAll, i::Integer) = fieldname(unwrap_unionall(t), i)
fieldname(t::Type{<:Tuple}, i::Integer) =
    i < 1 || i > fieldcount(t) ? throw(BoundsError(t, i)) : Int(i)

"""
    fieldnames(x::DataType)

Get an array of the fields of a `DataType`.

# Examples
```jldoctest
julia> fieldnames(Hermitian)
2-element Array{Symbol,1}:
 :data
 :uplo
```
"""
fieldnames(t::DataType) = Symbol[fieldname(t, n) for n in 1:fieldcount(t)]
fieldnames(t::UnionAll) = fieldnames(unwrap_unionall(t))
fieldnames(t::Type{<:Tuple}) = Int[n for n in 1:fieldcount(t)]

"""
    Base.datatype_name(t) -> Symbol

Get the name of a (potentially UnionAll-wrapped) `DataType` (without its parent module) as a symbol.

# Examples
```jldoctest
julia> module Foo
           struct S{T}
           end
       end
Foo

julia> Base.datatype_name(Foo.S{T} where T)
:S
```
"""
datatype_name(t::DataType) = t.name.name
datatype_name(t::UnionAll) = datatype_name(unwrap_unionall(t))

"""
    Base.datatype_module(t::DataType) -> Module

Determine the module containing the definition of a (potentially UnionAll-wrapped) `DataType`.

# Examples
```jldoctest
julia> module Foo
           struct Int end
       end
Foo

julia> Base.datatype_module(Int)
Core

julia> Base.datatype_module(Foo.Int)
Foo
```
"""
datatype_module(t::DataType) = t.name.module
datatype_module(t::UnionAll) = datatype_module(unwrap_unionall(t))

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
    object_id(x)

Get a hash value for `x` based on object identity. `object_id(x)==object_id(y)` if `x === y`.
"""
object_id(@nospecialize(x)) = ccall(:jl_object_id, UInt, (Any,), x)

struct DataTypeLayout
    nfields::UInt32
    alignment::UInt32
    # alignment : 28;
    # haspadding : 1;
    # pointerfree : 1;
    # fielddesc_type : 2;
end


# type predicates
datatype_alignment(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    Int(unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment & 0x1FF)

datatype_haspadding(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 9) & 1 == 1

datatype_pointerfree(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 10) & 0xFFFFF == 0

datatype_fielddesc_type(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 30) & 3

"""
    isimmutable(v)

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
isimmutable(@nospecialize(x)) = (@_pure_meta; (isa(x,Tuple) || !typeof(x).mutable))
isstructtype(t::DataType) = (@_pure_meta; length(t.types) != 0 || (t.size==0 && !t.abstract))
isstructtype(x) = (@_pure_meta; false)

"""
    isbits(T)

Return `true` if `T` is a "plain data" type, meaning it is immutable and contains no
references to other values. Typical examples are numeric types such as [`UInt8`](@ref),
[`Float64`](@ref), and [`Complex{Float64}`](@ref).

# Examples
```jldoctest
julia> isbits(Complex{Float64})
true

julia> isbits(Complex)
false
```
"""
isbits(t::DataType) = (@_pure_meta; !t.mutable & (t.layout != C_NULL) && datatype_pointerfree(t))
isbits(t::Type) = (@_pure_meta; false)
isbits(x) = (@_pure_meta; isbits(typeof(x)))

_isleaftype(@nospecialize(t)) = (@_pure_meta; isa(t, DataType) && t.isleaftype)

"""
    isconcrete(T)

Determine whether `T` is a concrete type, meaning it can have direct instances
(values `x` such that `typeof(x) === T`).

# Examples
```jldoctest
julia> isconcrete(Complex)
false

julia> isconcrete(Complex{Float32})
true

julia> isconcrete(Vector{Complex})
true

julia> isconcrete(Vector{Complex{Float32}})
true

julia> isconcrete(Union{})
false

julia> isconcrete(Union{Int,String})
false
```
"""
function isconcrete(@nospecialize(t))
    @_pure_meta
    return (isa(t, DataType) && !t.abstract &&
            !t.hasfreetypevars &&
            (t.name !== Tuple.name || all(isconcrete, t.parameters)))
end

"""
    Base.isabstract(T)

Determine whether `T` was declared as an abstract type (i.e. using the
`abstract` keyword).

# Examples
```jldoctest
julia> Base.isabstract(AbstractArray)
true

julia> Base.isabstract(Vector)
false
```
"""
function isabstract(@nospecialize(t))
    @_pure_meta
    t = unwrap_unionall(t)
    isa(t,DataType) && t.abstract
end

"""
    Base.parameter_upper_bound(t::UnionAll, idx)

Determine the upper bound of a type parameter in the underlying type. E.g.:

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
    rewrap_unionall(unwrap_unionall(t).parameters[idx], t)
end

"""
    typeintersect(T, S)

Compute a type that contains the intersection of `T` and `S`. Usually this will be the
smallest such type or one close to it.
"""
typeintersect(@nospecialize(a),@nospecialize(b)) = (@_pure_meta; ccall(:jl_type_intersection, Any, (Any,Any), a, b))
typeseq(@nospecialize(a),@nospecialize(b)) = (@_pure_meta; a<:b && b<:a)

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

type_alignment(x::DataType) = (@_pure_meta; ccall(:jl_get_alignment, Csize_t, (Any,), x))

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
    if t.abstract || (t.name === Tuple.name && isvatuple(t))
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

# subtypes
function _subtypes(m::Module, x::Union{DataType,UnionAll},
                   sts=Set{Union{DataType,UnionAll}}(), visited=Set{Module}())
    push!(visited, m)
    xt = unwrap_unionall(x)
    if !isa(xt, DataType)
        return sts
    end
    xt = xt::DataType
    for s in names(m, true)
        if isdefined(m, s) && !isdeprecated(m, s)
            t = getfield(m, s)
            if isa(t, DataType)
                t = t::DataType
                if t.name.name === s && supertype(t).name == xt.name
                    ti = typeintersect(t, x)
                    ti != Bottom && push!(sts, ti)
                end
            elseif isa(t, UnionAll)
                t = t::UnionAll
                tt = unwrap_unionall(t)
                isa(tt, DataType) || continue
                tt = tt::DataType
                if tt.name.name === s && supertype(tt).name == xt.name
                    ti = typeintersect(t, x)
                    ti != Bottom && push!(sts, ti)
                end
            elseif isa(t, Module)
                t = t::Module
                in(t, visited) || _subtypes(t, x, sts, visited)
            end
        end
    end
    return sts
end

function _subtypes_in(mods::Array, x::Union{DataType,UnionAll})
    if !isabstract(x)
        # Fast path
        return Union{DataType,UnionAll}[]
    end
    sts = Set{Union{DataType,UnionAll}}()
    visited = Set{Module}()
    for m in mods
        _subtypes(m, x, sts, visited)
    end
    return sort!(collect(sts), by=string)
end

subtypes(m::Module, x::Union{DataType,UnionAll}) = _subtypes_in([m], x)

"""
    subtypes(T::DataType)

Return a list of immediate subtypes of DataType `T`. Note that all currently loaded subtypes
are included, including those not visible in the current module.

# Examples
```jldoctest
julia> subtypes(Integer)
4-element Array{Union{DataType, UnionAll},1}:
 BigInt
 Bool
 Signed
 Unsigned
```
"""
subtypes(x::Union{DataType,UnionAll}) = _subtypes_in(loaded_modules_array(), x)

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
    code_lowered(f, types, expand_generated = true)

Return an array of lowered ASTs for the methods matching the given generic function and type signature.

If `expand_generated` is `false`, then the `CodeInfo` instances returned for `@generated`
methods will correspond to the generators' lowered ASTs. If `expand_generated` is `true`,
these `CodeInfo` instances will correspond to the lowered ASTs of the method bodies yielded
by expanding the generators.

Note that an error will be thrown if `types` are not leaf types when `expand_generated` is
`true` and the corresponding method is a `@generated` method.
"""
function code_lowered(@nospecialize(f), @nospecialize(t = Tuple), expand_generated::Bool = true)
    return map(method_instances(f, t)) do m
        if expand_generated && isgenerated(m)
            if isa(m, Core.MethodInstance)
                return Core.Inference.get_staged(m)
            else # isa(m, Method)
                error("Could not expand generator for `@generated` method ", m, ". ",
                      "This can happen if the provided argument types (", t, ") are ",
                      "not leaf types, but the `expand_generated` argument is `true`.")
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
    mt::MethodTable
end

length(m::MethodList) = length(m.ms)
isempty(m::MethodList) = isempty(m.ms)
start(m::MethodList) = start(m.ms)
done(m::MethodList, s) = done(m.ms, s)
next(m::MethodList, s) = next(m.ms, s)

function MethodList(mt::MethodTable)
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

function visit(f, mt::MethodTable)
    mt.defs !== nothing && visit(f, mt.defs)
    nothing
end
function visit(f, mc::TypeMapLevel)
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
function visit(f, d::TypeMapEntry)
    while d !== nothing
        f(d.func)
        d = d.next
    end
    nothing
end

function length(mt::MethodTable)
    n = 0
    visit(mt) do m
        n += 1
    end
    return n::Int
end
isempty(mt::MethodTable) = (mt.defs === nothing)

uncompressed_ast(m::Method) = uncompressed_ast(m, isdefined(m, :source) ? m.source : m.generator.inferred)
uncompressed_ast(m::Method, s::CodeInfo) = s
uncompressed_ast(m::Method, s::Array{UInt8,1}) = ccall(:jl_uncompress_ast, Any, (Any, Any), m, s)::CodeInfo
uncompressed_ast(m::Core.MethodInstance) = uncompressed_ast(m.def)

function method_instances(@nospecialize(f), @nospecialize(t), world::UInt = typemax(UInt))
    tt = signature_type(f, t)
    results = Vector{Union{Method,Core.MethodInstance}}()
    for method_data in _methods_by_ftype(tt, -1, world)
        mtypes, msp, m = method_data
        instance = Core.Inference.code_for_method(m, mtypes, msp, world, false)
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

# Printing code representations in IR and assembly
function _dump_function(@nospecialize(f), @nospecialize(t), native::Bool, wrapper::Bool,
                        strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol=:att,
                        optimize::Bool=true, params::CodegenParams=CodegenParams())
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    # get the MethodInstance for the method match
    world = typemax(UInt)
    meth = which(f, t)
    t = to_tuple_type(t)
    tt = signature_type(f, t)
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), tt, meth.sig)::SimpleVector
    meth = func_for_method_checked(meth, ti)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, ti, env, world)
    # get the code for it
    return _dump_function_linfo(linfo, world, native, wrapper, strip_ir_metadata, dump_module, syntax, optimize, params)
end

function _dump_function_linfo(linfo::Core.MethodInstance, world::UInt, native::Bool, wrapper::Bool,
                              strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol=:att,
                              optimize::Bool=true, params::CodegenParams=CodegenParams())
    if syntax != :att && syntax != :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    if native
        llvmf = ccall(:jl_get_llvmf_decl, Ptr{Void}, (Any, UInt, Bool, CodegenParams), linfo, world, wrapper, params)
    else
        llvmf = ccall(:jl_get_llvmf_defn, Ptr{Void}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, wrapper, optimize, params)
    end
    if llvmf == C_NULL
        error("could not compile the specified method")
    end

    if native
        str = ccall(:jl_dump_function_asm, Ref{String},
                    (Ptr{Void}, Cint, Ptr{UInt8}), llvmf, 0, syntax)
    else
        str = ccall(:jl_dump_function_ir, Ref{String},
                    (Ptr{Void}, Bool, Bool), llvmf, strip_ir_metadata, dump_module)
    end

    # TODO: use jl_is_cacheable_sig instead of isleaftype
    _isleaftype(linfo.specTypes) || (str = "; WARNING: This code may not match what actually runs.\n" * str)
    return str
end

"""
    code_llvm([io=STDOUT,], f, types)

Prints the LLVM bitcodes generated for running the method matching the given generic
function and type signature to `io`.

All metadata and dbg.* calls are removed from the printed bitcode. Use code_llvm_raw for the full IR.
"""
code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Tuple), strip_ir_metadata=true, dump_module=false) =
    print(io, _dump_function(f, types, false, false, strip_ir_metadata, dump_module))
code_llvm(@nospecialize(f), @nospecialize(types=Tuple)) = code_llvm(STDOUT, f, types)
code_llvm_raw(@nospecialize(f), @nospecialize(types=Tuple)) = code_llvm(STDOUT, f, types, false)

"""
    code_native([io=STDOUT,], f, types, syntax=:att)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.
Switch assembly syntax using `syntax` symbol parameter set to `:att` for AT&T syntax or `:intel` for Intel syntax.
"""
code_native(io::IO, @nospecialize(f), @nospecialize(types=Tuple), syntax::Symbol=:att) =
    print(io, _dump_function(f, types, true, false, false, false, syntax))
code_native(@nospecialize(f), @nospecialize(types=Tuple), syntax::Symbol=:att) = code_native(STDOUT, f, types, syntax)
code_native(::IO, ::Any, ::Symbol) = error("illegal code_native call") # resolve ambiguous call

# give a decent error message if we try to instantiate a staged function on non-leaf types
function func_for_method_checked(m::Method, @nospecialize types)
    if isdefined(m,:generator) && !isdefined(m,:source) && !_isleaftype(types)
        error("cannot call @generated function `", m, "` ",
              "with abstract argument types: ", types)
    end
    return m
end

"""
    code_typed(f, types; optimize=true)

Returns an array of lowered and type-inferred ASTs for the methods matching the given
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
    params = Core.Inference.InferenceParams(world)
    for x in _methods(f, types, -1, world)
        meth = func_for_method_checked(x[3], types)
        (_, code, ty) = Core.Inference.typeinf_code(meth, x[1], x[2], optimize, optimize, params)
        code === nothing && error("inference not successful") # Inference disabled?
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
    params = Core.Inference.InferenceParams(world)
    for x in _methods(f, types, -1, world)
        meth = func_for_method_checked(x[3], types)
        ty = Core.Inference.typeinf_type(meth, x[1], x[2], true, params)
        ty === nothing && error("inference not successful") # Inference disabled?
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
    which(symbol)

Return the module in which the binding for the variable referenced by `symbol` in module `Main` was created.
"""
which(s::Symbol) = which_module(Main, s)
# TODO: making this a method of which() causes a strange error
function which_module(m::Module, s::Symbol)
    if !isdefined(m, s)
        error("\"$s\" is not defined in module $m")
    end
    return binding_module(m, s)
end

# function reflection
"""
    Base.function_name(f::Function) -> Symbol

Get the name of a generic `Function` as a symbol, or `:anonymous`.
"""
function_name(f::Function) = typeof(f).name.mt.name

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
    Base.function_module(f::Function) -> Module

Determine the module containing the (first) definition of a generic
function.
"""
function_module(f::Function) = datatype_module(typeof(f))

"""
    Base.function_module(f::Function, types) -> Module

Determine the module containing a given definition of a generic function.
"""
function function_module(@nospecialize(f), @nospecialize(types))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    return first(m).module
end

"""
    method_exists(f, Tuple type, world=typemax(UInt)) -> Bool

Determine whether the given generic function has a method matching the given
`Tuple` of argument types with the upper bound of world age given by `world`.

# Examples
```jldoctest
julia> method_exists(length, Tuple{Array})
true
```
"""
function method_exists(@nospecialize(f), @nospecialize(t), world=typemax(UInt))
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
has_bottom_parameter(t::TypeVar) = has_bottom_parameter(t.ub)
has_bottom_parameter(::Any) = false

min_world(m::Method) = reinterpret(UInt, m.min_world)
max_world(m::Method) = typemax(UInt)
min_world(m::Core.MethodInstance) = reinterpret(UInt, m.min_world)
max_world(m::Core.MethodInstance) = reinterpret(UInt, m.max_world)
