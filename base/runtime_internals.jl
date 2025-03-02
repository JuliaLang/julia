# This file is a part of Julia. License is MIT: https://julialang.org/license

# name and module reflection

"""
    parentmodule(m::Module) -> Module

Get a module's enclosing `Module`. `Main` is its own parent.

See also: [`names`](@ref), [`nameof`](@ref), [`fullname`](@ref), [`@__MODULE__`](@ref).

# Examples
```jldoctest
julia> parentmodule(Main)
Main

julia> parentmodule(Base.Broadcast)
Base
```
"""
parentmodule(m::Module) = (@_total_meta; ccall(:jl_module_parent, Ref{Module}, (Any,), m))

is_root_module(m::Module) = parentmodule(m) === m || m === Compiler || (isdefined(Main, :Base) && m === Main.Base)

"""
    moduleroot(m::Module) -> Module

Find the root module of a given module. This is the first module in the chain of
parent modules of `m` which is either a registered root module or which is its
own parent module.
"""
function moduleroot(m::Module)
    @_total_meta
    while true
        is_root_module(m) && return m
        p = parentmodule(m)
        p === m && return m
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
    @_total_meta
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
    moduleloc(m::Module) -> LineNumberNode

Get the location of the `module` definition.
"""
function moduleloc(m::Module)
    line = Ref{Int32}(0)
    file = ccall(:jl_module_getloc, Ref{Symbol}, (Any, Ref{Int32}), m, line)
    return LineNumberNode(Int(line[]), file)
end

"""
    names(x::Module; all::Bool=false, imported::Bool=false, usings::Bool=false) -> Vector{Symbol}

Get a vector of the public names of a `Module`, excluding deprecated names.
If `all` is true, then the list also includes non-public names defined in the module,
deprecated names, and compiler-generated names.
If `imported` is true, then names explicitly imported from other modules
are also included.
If `usings` is true, then names explicitly imported via `using` are also included.
Names are returned in sorted order.

As a special case, all names defined in `Main` are considered \"public\",
since it is not idiomatic to explicitly mark names from `Main` as public.

!!! note
    `sym ∈ names(SomeModule)` does *not* imply `isdefined(SomeModule, sym)`.
    `names` may return symbols marked with `public` or `export`, even if
    they are not defined in the module.

!!! warning
    `names` may return duplicate names. The duplication happens, e.g. if an `import`ed name
    conflicts with an already existing identifier.

See also: [`Base.isexported`](@ref), [`Base.ispublic`](@ref), [`Base.@locals`](@ref), [`@__MODULE__`](@ref).
"""
names(m::Module; kwargs...) = sort!(unsorted_names(m; kwargs...))
unsorted_names(m::Module; all::Bool=false, imported::Bool=false, usings::Bool=false) =
    ccall(:jl_module_names, Array{Symbol,1}, (Any, Cint, Cint, Cint), m, all, imported, usings)

"""
    isexported(m::Module, s::Symbol) -> Bool

Returns whether a symbol is exported from a module.

See also: [`ispublic`](@ref), [`names`](@ref)

```jldoctest
julia> module Mod
           export foo
           public bar
       end
Mod

julia> Base.isexported(Mod, :foo)
true

julia> Base.isexported(Mod, :bar)
false

julia> Base.isexported(Mod, :baz)
false
```
"""
isexported(m::Module, s::Symbol) = ccall(:jl_module_exports_p, Cint, (Any, Any), m, s) != 0

"""
    ispublic(m::Module, s::Symbol) -> Bool

Returns whether a symbol is marked as public in a module.

Exported symbols are considered public.

!!! compat "Julia 1.11"
    This function and the notion of publicity were added in Julia 1.11.

See also: [`isexported`](@ref), [`names`](@ref)

```jldoctest
julia> module Mod
           export foo
           public bar
       end
Mod

julia> Base.ispublic(Mod, :foo)
true

julia> Base.ispublic(Mod, :bar)
true

julia> Base.ispublic(Mod, :baz)
false
```
"""
ispublic(m::Module, s::Symbol) = ccall(:jl_module_public_p, Cint, (Any, Any), m, s) != 0

# TODO: this is vaguely broken because it only works for explicit calls to
# `Base.deprecate`, not the @deprecated macro:
isdeprecated(m::Module, s::Symbol) = ccall(:jl_is_binding_deprecated, Cint, (Any, Any), m, s) != 0

function binding_module(m::Module, s::Symbol)
    p = ccall(:jl_get_module_of_binding, Ptr{Cvoid}, (Any, Any), m, s)
    p == C_NULL && return m
    return unsafe_pointer_to_objref(p)::Module
end

const _NAMEDTUPLE_NAME = NamedTuple.body.body.name

function _fieldnames(@nospecialize t)
    if t.name === _NAMEDTUPLE_NAME
        if t.parameters[1] isa Tuple
            return t.parameters[1]
        else
            throw(ArgumentError("type does not have definite field names"))
        end
    end
    return t.name.names
end

# N.B.: Needs to be synced with julia.h
const BINDING_KIND_CONST        = 0x0
const BINDING_KIND_CONST_IMPORT = 0x1
const BINDING_KIND_GLOBAL       = 0x2
const BINDING_KIND_IMPLICIT     = 0x3
const BINDING_KIND_EXPLICIT     = 0x4
const BINDING_KIND_IMPORTED     = 0x5
const BINDING_KIND_FAILED       = 0x6
const BINDING_KIND_DECLARED     = 0x7
const BINDING_KIND_GUARD        = 0x8
const BINDING_KIND_UNDEF_CONST  = 0x9
const BINDING_KIND_BACKDATED_CONST = 0xa

const BINDING_FLAG_EXPORTED     = 0x10
const BINDING_FLAG_DEPRECATED   = 0x20
const BINDING_FLAG_DEPWARN      = 0x40

const BINDING_KIND_MASK         = 0x0f
const BINDING_FLAG_MASK         = 0xf0

is_defined_const_binding(kind::UInt8) = (kind == BINDING_KIND_CONST || kind == BINDING_KIND_CONST_IMPORT || kind == BINDING_KIND_BACKDATED_CONST)
is_some_const_binding(kind::UInt8) = (is_defined_const_binding(kind) || kind == BINDING_KIND_UNDEF_CONST)
is_some_imported(kind::UInt8) = (kind == BINDING_KIND_IMPLICIT || kind == BINDING_KIND_EXPLICIT || kind == BINDING_KIND_IMPORTED)
is_some_guard(kind::UInt8) = (kind == BINDING_KIND_GUARD || kind == BINDING_KIND_FAILED || kind == BINDING_KIND_UNDEF_CONST)

function lookup_binding_partition(world::UInt, b::Core.Binding)
    ccall(:jl_get_binding_partition, Ref{Core.BindingPartition}, (Any, UInt), b, world)
end

function convert(::Type{Core.Binding}, gr::Core.GlobalRef)
    if isdefined(gr, :binding)
        return gr.binding
    else
        return ccall(:jl_get_module_binding, Ref{Core.Binding}, (Any, Any, Cint), gr.mod, gr.name, true)
    end
end

function lookup_binding_partition(world::UInt, gr::Core.GlobalRef)
    b = convert(Core.Binding, gr)
    return lookup_binding_partition(world, b)
end

partition_restriction(bpart::Core.BindingPartition) = ccall(:jl_bpart_get_restriction_value, Any, (Any,), bpart)

binding_kind(bpart::Core.BindingPartition) = ccall(:jl_bpart_get_kind, UInt8, (Any,), bpart)
binding_kind(m::Module, s::Symbol) = binding_kind(lookup_binding_partition(tls_world_age(), GlobalRef(m, s)))

"""
    delete_binding(mod::Module, sym::Symbol)

Force the binding `mod.sym` to be undefined again, allowing it be redefined.
Note that this operation is very expensive, requiring a full scan of all code in the system,
as well as potential recompilation of any methods that (may) have used binding
information.

!!! warning
    The implementation of this functionality is currently incomplete. Do not use
    this method on versions that contain this disclaimer except for testing.
"""
function delete_binding(mod::Module, sym::Symbol)
    ccall(:jl_disable_binding, Cvoid, (Any,), GlobalRef(mod, sym))
end

"""
    fieldname(x::DataType, i::Integer)

Get the name of field `i` of a `DataType`.

The return type is `Symbol`, except when `x <: Tuple`, in which case the index of the field is returned, of type `Int`.

# Examples
```jldoctest
julia> fieldname(Rational, 1)
:num

julia> fieldname(Rational, 2)
:den

julia> fieldname(Tuple{String,Int}, 2)
2
```
"""
function fieldname(t::DataType, i::Integer)
    throw_not_def_field() = throw(ArgumentError("type does not have definite field names"))
    function throw_field_access(t, i, n_fields)
        field_label = n_fields == 1 ? "field" : "fields"
        throw(ArgumentError("Cannot access field $i since type $t only has $n_fields $field_label."))
    end
    throw_need_pos_int(i) = throw(ArgumentError("Field numbers must be positive integers. $i is invalid."))

    isabstracttype(t) && throw_not_def_field()
    names = _fieldnames(t)
    n_fields = length(names)::Int
    i > n_fields && throw_field_access(t, i, n_fields)
    i < 1 && throw_need_pos_int(i)
    return @inbounds names[i]::Symbol
end

fieldname(t::UnionAll, i::Integer) = fieldname(unwrap_unionall(t), i)
fieldname(t::Type{<:Tuple}, i::Integer) =
    i < 1 || i > fieldcount(t) ? throw(BoundsError(t, i)) : Int(i)

"""
    fieldnames(x::DataType)

Get a tuple with the names of the fields of a `DataType`.

Each name is a `Symbol`, except when `x <: Tuple`, in which case each name (actually the
index of the field) is an `Int`.

See also [`propertynames`](@ref), [`hasfield`](@ref).

# Examples
```jldoctest
julia> fieldnames(Rational)
(:num, :den)

julia> fieldnames(typeof(1+im))
(:re, :im)

julia> fieldnames(Tuple{String,Int})
(1, 2)
```
"""
fieldnames(t::DataType) = (fieldcount(t); # error check to make sure type is specific enough
                           (_fieldnames(t)...,))::Tuple{Vararg{Symbol}}
fieldnames(t::UnionAll) = fieldnames(unwrap_unionall(t))
fieldnames(::Core.TypeofBottom) =
    throw(ArgumentError("The empty type does not have field names since it does not have instances."))
fieldnames(t::Type{<:Tuple}) = ntuple(identity, fieldcount(t))

"""
    hasfield(T::Type, name::Symbol)

Return a boolean indicating whether `T` has `name` as one of its own fields.

See also [`fieldnames`](@ref), [`fieldcount`](@ref), [`hasproperty`](@ref).

!!! compat "Julia 1.2"
     This function requires at least Julia 1.2.

# Examples
```jldoctest
julia> struct Foo
            bar::Int
       end

julia> hasfield(Foo, :bar)
true

julia> hasfield(Foo, :x)
false
```
"""
hasfield(T::Type, name::Symbol) = fieldindex(T, name, false) > 0

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
nameof(t::UnionAll) = nameof(unwrap_unionall(t))::Symbol

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
    isconst(g::GlobalRef)

Determine whether a global is `const` in a given module `m`, either
because it was declared constant or because it was imported from a
constant binding. Note that constant-ness is specific to a particular
world age, so the result of this function may not be assumed to hold
after a world age update.
"""
isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Cint, (Any, Any), m, s) != 0

function isconst(g::GlobalRef)
    return ccall(:jl_globalref_is_const, Cint, (Any,), g) != 0
end

"""
    isconst(t::DataType, s::Union{Int,Symbol}) -> Bool

Determine whether a field `s` is const in a given type `t`
in the sense that a read from said field is consistent
for egal objects. Note in particular that out-of-bounds
fields are considered const under this definition (because
they always throw).
"""
function isconst(@nospecialize(t::Type), s::Symbol)
    @_foldable_meta
    t = unwrap_unionall(t)
    isa(t, DataType) || return false
    return isconst(t, fieldindex(t, s, false))
end
function isconst(@nospecialize(t::Type), s::Int)
    @_foldable_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false # uncertain
    ismutabletype(t) || return true # immutable structs are always const
    1 <= s <= length(t.name.names) || return true # OOB reads are "const" since they always throw
    constfields = t.name.constfields
    constfields === C_NULL && return false
    s -= 1
    return unsafe_load(Ptr{UInt32}(constfields), 1 + s÷32) & (1 << (s%32)) != 0
end

"""
    isfieldatomic(t::DataType, s::Union{Int,Symbol}) -> Bool

Determine whether a field `s` is declared `@atomic` in a given type `t`.
"""
function isfieldatomic(@nospecialize(t::Type), s::Symbol)
    @_foldable_meta
    t = unwrap_unionall(t)
    isa(t, DataType) || return false
    return isfieldatomic(t, fieldindex(t, s, false))
end
function isfieldatomic(@nospecialize(t::Type), s::Int)
    @_foldable_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false # uncertain
    ismutabletype(t) || return false # immutable structs are never atomic
    1 <= s <= length(t.name.names) || return false # OOB reads are not atomic (they always throw)
    atomicfields = t.name.atomicfields
    atomicfields === C_NULL && return false
    s -= 1
    return unsafe_load(Ptr{UInt32}(atomicfields), 1 + s÷32) & (1 << (s%32)) != 0
end

"""
    @locals()

Construct a dictionary of the names (as symbols) and values of all local
variables defined as of the call site.

!!! compat "Julia 1.1"
    This macro requires at least Julia 1.1.

# Examples
```jldoctest
julia> let x = 1, y = 2
           Base.@locals
       end
Dict{Symbol, Any} with 2 entries:
  :y => 2
  :x => 1

julia> function f(x)
           local y
           show(Base.@locals); println()
           for i = 1:1
               show(Base.@locals); println()
           end
           y = 2
           show(Base.@locals); println()
           nothing
       end;

julia> f(42)
Dict{Symbol, Any}(:x => 42)
Dict{Symbol, Any}(:i => 1, :x => 42)
Dict{Symbol, Any}(:y => 2, :x => 42)
```
"""
macro locals()
    return Expr(:locals)
end

# concrete datatype predicates

datatype_fieldtypes(x::DataType) = ccall(:jl_get_fieldtypes, Core.SimpleVector, (Any,), x)

struct DataTypeLayout
    size::UInt32
    nfields::UInt32
    npointers::UInt32
    firstptr::Int32
    alignment::UInt16
    flags::UInt16
    # haspadding : 1;
    # fielddesc_type : 2;
    # arrayelem_isboxed : 1;
    # arrayelem_isunion : 1;
end

"""
    Base.datatype_alignment(dt::DataType) -> Int

Memory allocation minimum alignment for instances of this type.
Can be called on any `isconcretetype`, although for Memory it will give the
alignment of the elements, not the whole object.
"""
function datatype_alignment(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    alignment = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment
    return Int(alignment)
end

function uniontype_layout(@nospecialize T::Type)
    sz = RefValue{Csize_t}(0)
    algn = RefValue{Csize_t}(0)
    isinline = ccall(:jl_islayout_inline, Cint, (Any, Ptr{Csize_t}, Ptr{Csize_t}), T, sz, algn) != 0
    (isinline, Int(sz[]), Int(algn[]))
end

LLT_ALIGN(x, sz) = (x + sz - 1) & -sz

# amount of total space taken by T when stored in a container
function aligned_sizeof(@nospecialize T::Type)
    @_foldable_meta
    if isa(T, Union)
        if allocatedinline(T)
            # NOTE this check is equivalent to `isbitsunion(T)`, we can improve type
            # inference in the second branch with the outer `isa(T, Union)` check
            _, sz, al = uniontype_layout(T)
            return LLT_ALIGN(sz, al)
        end
    elseif allocatedinline(T)
        al = datatype_alignment(T)
        return LLT_ALIGN(Core.sizeof(T), al)
    end
    return Core.sizeof(Ptr{Cvoid})
end

gc_alignment(sz::Integer) = Int(ccall(:jl_alignment, Cint, (Csize_t,), sz))
gc_alignment(T::Type) = gc_alignment(Core.sizeof(T))

"""
    Base.datatype_haspadding(dt::DataType) -> Bool

Return whether the fields of instances of this type are packed in memory,
with no intervening padding bits (defined as bits whose value does not impact
the semantic value of the instance itself).
Can be called on any `isconcretetype`.
"""
function datatype_haspadding(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    flags = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).flags
    return flags & 1 == 1
end

"""
    Base.datatype_isbitsegal(dt::DataType) -> Bool

Return whether egality of the (non-padding bits of the) in-memory representation
of an instance of this type implies semantic egality of the instance itself.
This may not be the case if the type contains to other values whose egality is
independent of their identity (e.g. immutable structs, some types, etc.).
"""
function datatype_isbitsegal(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    flags = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).flags
    return (flags & (1<<5)) != 0
end

"""
    Base.datatype_nfields(dt::DataType) -> UInt32

Return the number of fields known to this datatype's layout. This may be
different from the number of actual fields of the type for opaque types.
Can be called on any `isconcretetype`.
"""
function datatype_nfields(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    return unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).nfields
end

"""
    Base.datatype_npointers(dt::DataType) -> Int

Return the number of pointers in the layout of a datatype.
"""
function datatype_npointers(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    return unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).npointers
end

"""
    Base.datatype_pointerfree(dt::DataType) -> Bool

Return whether instances of this type can contain references to gc-managed memory.
Can be called on any `isconcretetype`.
"""
function datatype_pointerfree(dt::DataType)
    @_foldable_meta
    return datatype_npointers(dt) == 0
end

"""
    Base.datatype_fielddesc_type(dt::DataType) -> Int

Return the size in bytes of each field-description entry in the layout array,
located at `(dt.layout + sizeof(DataTypeLayout))`.
Can be called on any `isconcretetype`.

See also [`fieldoffset`](@ref).
"""
function datatype_fielddesc_type(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    flags = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).flags
    return (flags >> 1) & 3
end

"""
    Base.datatype_arrayelem(dt::DataType) -> Int

Return the behavior of the trailing array types allocations.
Can be called on any `isconcretetype`, but only meaningful on `Memory`.

0 = inlinealloc
1 = isboxed
2 = isbitsunion
"""
function datatype_arrayelem(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    flags = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).flags
    return (flags >> 3) & 3
end

function datatype_layoutsize(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    size = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).size
    return size % Int
end


# For type stability, we only expose a single struct that describes everything
struct FieldDesc
    isforeign::Bool
    isptr::Bool
    size::UInt32
    offset::UInt32
end

struct FieldDescStorage{T}
    ptrsize::T
    offset::T
end
FieldDesc(fd::FieldDescStorage{T}) where {T} =
    FieldDesc(false, fd.ptrsize & 1 != 0,
              fd.ptrsize >> 1, fd.offset)

struct DataTypeFieldDesc
    dt::DataType
    function DataTypeFieldDesc(dt::DataType)
        dt.layout == C_NULL && throw(UndefRefError())
        new(dt)
    end
end

function getindex(dtfd::DataTypeFieldDesc, i::Int)
    layout_ptr = convert(Ptr{DataTypeLayout}, dtfd.dt.layout)
    fd_ptr = layout_ptr + Core.sizeof(DataTypeLayout)
    layout = unsafe_load(layout_ptr)
    fielddesc_type = (layout.flags >> 1) & 3
    nfields = layout.nfields
    @boundscheck ((1 <= i <= nfields) || throw(BoundsError(dtfd, i)))
    if fielddesc_type == 0
        return FieldDesc(unsafe_load(Ptr{FieldDescStorage{UInt8}}(fd_ptr), i))
    elseif fielddesc_type == 1
        return FieldDesc(unsafe_load(Ptr{FieldDescStorage{UInt16}}(fd_ptr), i))
    elseif fielddesc_type == 2
        return FieldDesc(unsafe_load(Ptr{FieldDescStorage{UInt32}}(fd_ptr), i))
    else
        # fielddesc_type == 3
        return FieldDesc(true, true, 0, 0)
    end
end

"""
    ismutable(v) -> Bool

Return `true` if and only if value `v` is mutable.  See [Mutable Composite Types](@ref)
for a discussion of immutability. Note that this function works on values, so if you
give it a `DataType`, it will tell you that a value of the type is mutable.

!!! note
    For technical reasons, `ismutable` returns `true` for values of certain special types
    (for example `String` and `Symbol`) even though they cannot be mutated in a permissible way.

See also [`isbits`](@ref), [`isstructtype`](@ref).

# Examples
```jldoctest
julia> ismutable(1)
false

julia> ismutable([1,2])
true
```

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5.
"""
ismutable(@nospecialize(x)) = (@_total_meta; (typeof(x).name::Core.TypeName).flags & 0x2 == 0x2)
# The type assertion above is required to fix some invalidations.
# See also https://github.com/JuliaLang/julia/issues/52134

"""
    ismutabletype(T) -> Bool

Determine whether type `T` was declared as a mutable type
(i.e. using `mutable struct` keyword).
If `T` is not a type, then return `false`.

!!! compat "Julia 1.7"
    This function requires at least Julia 1.7.
"""
function ismutabletype(@nospecialize t)
    @_total_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    return isa(t, DataType) && ismutabletypename(t.name)
end

ismutabletypename(tn::Core.TypeName) = tn.flags & 0x2 == 0x2

"""
    isstructtype(T) -> Bool

Determine whether type `T` was declared as a struct type
(i.e. using the `struct` or `mutable struct` keyword).
If `T` is not a type, then return `false`.
"""
function isstructtype(@nospecialize t)
    @_total_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false
    return !isprimitivetype(t) && !isabstracttype(t)
end

"""
    isprimitivetype(T) -> Bool

Determine whether type `T` was declared as a primitive type
(i.e. using the `primitive type` syntax).
If `T` is not a type, then return `false`.
"""
function isprimitivetype(@nospecialize t)
    @_total_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    isa(t, DataType) || return false
    return (t.flags & 0x0080) == 0x0080
end

"""
    isbitstype(T)

Return `true` if type `T` is a "plain data" type,
meaning it is immutable and contains no references to other values,
only `primitive` types and other `isbitstype` types.
Typical examples are numeric types such as [`UInt8`](@ref),
[`Float64`](@ref), and [`Complex{Float64}`](@ref).
This category of types is significant since they are valid as type parameters,
may not track [`isdefined`](@ref) / [`isassigned`](@ref) status,
and have a defined layout that is compatible with C.
If `T` is not a type, then return `false`.

See also [`isbits`](@ref), [`isprimitivetype`](@ref), [`ismutable`](@ref).

# Examples
```jldoctest
julia> isbitstype(Complex{Float64})
true

julia> isbitstype(Complex)
false
```
"""
isbitstype(@nospecialize t) = (@_total_meta; isa(t, DataType) && (t.flags & 0x0008) == 0x0008)

"""
    isbits(x)

Return `true` if `x` is an instance of an [`isbitstype`](@ref) type.
"""
isbits(@nospecialize x) = isbitstype(typeof(x))

"""
    objectid(x) -> UInt

Get a hash value for `x` based on object identity.

If `x === y` then `objectid(x) == objectid(y)`, and usually when `x !== y`, `objectid(x) != objectid(y)`.

See also [`hash`](@ref), [`IdDict`](@ref).
"""
function objectid(@nospecialize(x))
    @_total_meta
    return ccall(:jl_object_id, UInt, (Any,), x)
end

"""
    isdispatchtuple(T)

Determine whether type `T` is a tuple of concrete types,
meaning it could appear as a type signature in dispatch
and has no subtypes (or supertypes) which could appear in a call.
If `T` is not a type, then return `false`.
"""
isdispatchtuple(@nospecialize(t)) = (@_total_meta; isa(t, DataType) && (t.flags & 0x0004) == 0x0004)

datatype_ismutationfree(dt::DataType) = (@_total_meta; (dt.flags & 0x0100) == 0x0100)

"""
    Base.ismutationfree(T)

Determine whether type `T` is mutation free in the sense that no mutable memory
is reachable from this type (either in the type itself) or through any fields.
Note that the type itself need not be immutable. For example, an empty mutable
type is `ismutabletype`, but also `ismutationfree`.
If `T` is not a type, then return `false`.
"""
function ismutationfree(@nospecialize(t))
    t = unwrap_unionall(t)
    if isa(t, DataType)
        return datatype_ismutationfree(t)
    elseif isa(t, Union)
        return ismutationfree(t.a) && ismutationfree(t.b)
    end
    # TypeVar, etc.
    return false
end

datatype_isidentityfree(dt::DataType) = (@_total_meta; (dt.flags & 0x0200) == 0x0200)

"""
    Base.isidentityfree(T)

Determine whether type `T` is identity free in the sense that this type or any
reachable through its fields has non-content-based identity.
If `T` is not a type, then return `false`.
"""
function isidentityfree(@nospecialize(t))
    t = unwrap_unionall(t)
    if isa(t, DataType)
        return datatype_isidentityfree(t)
    elseif isa(t, Union)
        return isidentityfree(t.a) && isidentityfree(t.b)
    end
    # TypeVar, etc.
    return false
end

iskindtype(@nospecialize t) = (t === DataType || t === UnionAll || t === Union || t === typeof(Bottom))
isconcretedispatch(@nospecialize t) = isconcretetype(t) && !iskindtype(t)

using Core: has_free_typevars

# equivalent to isa(v, Type) && isdispatchtuple(Tuple{v}) || v === Union{}
# and is thus perhaps most similar to the old (pre-1.0) `isconcretetype` query
function isdispatchelem(@nospecialize v)
    return (v === Bottom) || (v === typeof(Bottom)) || isconcretedispatch(v) ||
        (isType(v) && !has_free_typevars(v))
end

const _TYPE_NAME = Type.body.name
isType(@nospecialize t) = isa(t, DataType) && t.name === _TYPE_NAME

"""
    isconcretetype(T)

Determine whether type `T` is a concrete type, meaning it could have direct instances
(values `x` such that `typeof(x) === T`).
Note that this is not the negation of `isabstracttype(T)`.
If `T` is not a type, then return `false`.

See also: [`isbits`](@ref), [`isabstracttype`](@ref), [`issingletontype`](@ref).

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
isconcretetype(@nospecialize(t)) = (@_total_meta; isa(t, DataType) && (t.flags & 0x0002) == 0x0002)

"""
    isabstracttype(T)

Determine whether type `T` was declared as an abstract type
(i.e. using the `abstract type` syntax).
Note that this is not the negation of `isconcretetype(T)`.
If `T` is not a type, then return `false`.

# Examples
```jldoctest
julia> isabstracttype(AbstractArray)
true

julia> isabstracttype(Vector)
false
```
"""
function isabstracttype(@nospecialize(t))
    @_total_meta
    t = unwrap_unionall(t)
    # TODO: what to do for `Union`?
    return isa(t, DataType) && (t.name.flags & 0x1) == 0x1
end

function is_datatype_layoutopaque(dt::DataType)
    datatype_nfields(dt) == 0 && !datatype_pointerfree(dt)
end

function is_valid_intrinsic_elptr(@nospecialize(ety))
    ety === Any && return true
    isconcretetype(ety) || return false
    ety <: Array && return false
    return !is_datatype_layoutopaque(ety)
end

"""
    Base.issingletontype(T)

Determine whether type `T` has exactly one possible instance; for example, a
struct type with no fields except other singleton values.
If `T` is not a concrete type, then return `false`.
"""
issingletontype(@nospecialize(t)) = (@_total_meta; isa(t, DataType) && isdefined(t, :instance) && datatype_layoutsize(t) == 0 && datatype_pointerfree(t))

"""
    typeintersect(T::Type, S::Type)

Compute a type that contains the intersection of `T` and `S`. Usually this will be the
smallest such type or one close to it.

A special case where exact behavior is guaranteed: when `T <: S`,
`typeintersect(S, T) == T == typeintersect(T, S)`.
"""
typeintersect(@nospecialize(a), @nospecialize(b)) = (@_total_meta; ccall(:jl_type_intersection, Any, (Any, Any), a::Type, b::Type))

morespecific(@nospecialize(a), @nospecialize(b)) = (@_total_meta; ccall(:jl_type_morespecific, Cint, (Any, Any), a::Type, b::Type) != 0)
morespecific(a::Method, b::Method) = ccall(:jl_method_morespecific, Cint, (Any, Any), a, b) != 0

"""
    fieldoffset(type, i)

The byte offset of field `i` of a type relative to the data start. For example, we could
use it in the following manner to summarize information about a struct:

```jldoctest
julia> structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:fieldcount(T)];

julia> structinfo(Base.Filesystem.StatStruct)
14-element Vector{Tuple{UInt64, Symbol, Type}}:
 (0x0000000000000000, :desc, Union{RawFD, String})
 (0x0000000000000008, :device, UInt64)
 (0x0000000000000010, :inode, UInt64)
 (0x0000000000000018, :mode, UInt64)
 (0x0000000000000020, :nlink, Int64)
 (0x0000000000000028, :uid, UInt64)
 (0x0000000000000030, :gid, UInt64)
 (0x0000000000000038, :rdev, UInt64)
 (0x0000000000000040, :size, Int64)
 (0x0000000000000048, :blksize, Int64)
 (0x0000000000000050, :blocks, Int64)
 (0x0000000000000058, :mtime, Float64)
 (0x0000000000000060, :ctime, Float64)
 (0x0000000000000068, :ioerrno, Int32)
```
"""
fieldoffset(x::DataType, idx::Integer) = (@_foldable_meta; ccall(:jl_get_field_offset, Csize_t, (Any, Cint), x, idx))

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
    Base.fieldindex(T, name::Symbol, err:Bool=true)

Get the index of a named field, throwing an error if the field does not exist (when err==true)
or returning 0 (when err==false).

# Examples
```jldoctest
julia> struct Foo
           x::Int64
           y::String
       end

julia> Base.fieldindex(Foo, :z)
ERROR: FieldError: type Foo has no field `z`, available fields: `x`, `y`
Stacktrace:
[...]

julia> Base.fieldindex(Foo, :z, false)
0
```
"""
function fieldindex(T::DataType, name::Symbol, err::Bool=true)
    return err ? _fieldindex_maythrow(T, name) : _fieldindex_nothrow(T, name)
end

function _fieldindex_maythrow(T::DataType, name::Symbol)
    @_foldable_meta
    @noinline
    return Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), T, name, true)+1)
end

function _fieldindex_nothrow(T::DataType, name::Symbol)
    @_total_meta
    @noinline
    return Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), T, name, false)+1)
end

function fieldindex(t::UnionAll, name::Symbol, err::Bool=true)
    t = argument_datatype(t)
    if t === nothing
        err && throw(ArgumentError("type does not have definite fields"))
        return 0
    end
    return fieldindex(t, name, err)
end

function argument_datatype(@nospecialize t)
    @_total_meta
    @noinline
    return ccall(:jl_argument_datatype, Any, (Any,), t)::Union{Nothing,DataType}
end

function datatype_fieldcount(t::DataType)
    if t.name === _NAMEDTUPLE_NAME
        names, types = t.parameters[1], t.parameters[2]
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount(types)
        end
        return nothing
    elseif isabstracttype(t)
        return nothing
    end
    if t.name === Tuple.name
        isvatuple(t) && return nothing
        return length(t.types)
    end
    # Equivalent to length(t.types), but `t.types` is lazy and we do not want
    # to be forced to compute it.
    return length(t.name.names)
end

"""
    fieldcount(t::Type)

Get the number of fields that an instance of the given type would have.
An error is thrown if the type is too abstract to determine this.
"""
function fieldcount(@nospecialize t)
    @_foldable_meta
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            throw(ArgumentError("type does not have a definite number of fields"))
        end
    elseif t === Union{}
        throw(ArgumentError("The empty type does not have a well-defined number of fields since it does not have instances."))
    end
    if !(t isa DataType)
        throw(TypeError(:fieldcount, DataType, t))
    end
    fcount = datatype_fieldcount(t)
    if fcount === nothing
        throw(ArgumentError("type does not have a definite number of fields"))
    end
    return fcount
end

function fieldcount_noerror(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            return nothing
        end
    elseif t === Union{}
        return 0
    end
    t isa DataType || return nothing
    if t.name === _NAMEDTUPLE_NAME
        names, types = t.parameters
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount_noerror(types)
        end
        return nothing
    elseif isabstracttype(t) || (t.name === Tuple.name && isvatuple(t))
        return nothing
    end
    return isdefined(t, :types) ? length(t.types) : length(t.name.names)
end


"""
    fieldtypes(T::Type)

The declared types of all fields in a composite DataType `T` as a tuple.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> struct Foo
           x::Int64
           y::String
       end

julia> fieldtypes(Foo)
(Int64, String)
```
"""
fieldtypes(T::Type) = (@_foldable_meta; ntupleany(i -> fieldtype(T, i), fieldcount(T)))

# return all instances, for types that can be enumerated

"""
    instances(T::Type)

Return a collection of all instances of the given type, if applicable. Mostly used for
enumerated types (see `@enum`).

# Examples
```jldoctest
julia> @enum Color red blue green

julia> instances(Color)
(red, blue, green)
```
"""
function instances end

function to_tuple_type(@nospecialize(t))
    if isa(t, Tuple) || isa(t, AbstractArray) || isa(t, SimpleVector)
        t = Tuple{t...}
    end
    if isa(t, Type) && t <: Tuple
        for p in (unwrap_unionall(t)::DataType).parameters
            if isa(p, Core.TypeofVararg)
                p = unwrapva(p)
            end
            if !(isa(p, Type) || isa(p, TypeVar))
                error("argument tuple type must contain only types")
            end
        end
    else
        error("expected tuple type")
    end
    t
end

function signature_type(@nospecialize(f), @nospecialize(argtypes))
    argtypes = to_tuple_type(argtypes)
    ft = Core.Typeof(f)
    u = unwrap_unionall(argtypes)::DataType
    return rewrap_unionall(Tuple{ft, u.parameters...}, argtypes)
end

function get_methodtable(m::Method)
    mt = ccall(:jl_method_get_table, Any, (Any,), m)
    if mt === nothing
        return nothing
    end
    return mt::Core.MethodTable
end

"""
    has_bottom_parameter(t) -> Bool

Determine whether `t` is a Type for which one or more of its parameters is `Union{}`.
"""
function has_bottom_parameter(t::DataType)
    for p in t.parameters
        has_bottom_parameter(p) && return true
    end
    return false
end
has_bottom_parameter(t::typeof(Bottom)) = true
has_bottom_parameter(t::UnionAll) = has_bottom_parameter(unwrap_unionall(t))
has_bottom_parameter(t::Union) = has_bottom_parameter(t.a) & has_bottom_parameter(t.b)
has_bottom_parameter(t::TypeVar) = has_bottom_parameter(t.ub)
has_bottom_parameter(::Any) = false

min_world(m::Core.CodeInstance) = m.min_world
max_world(m::Core.CodeInstance) = m.max_world
min_world(m::Core.CodeInfo) = m.min_world
max_world(m::Core.CodeInfo) = m.max_world

"""
    get_world_counter()

Returns the current maximum world-age counter. This counter is global and monotonically
increasing.
"""
get_world_counter() = ccall(:jl_get_world_counter, UInt, ())

"""
    tls_world_age()

Returns the world the [current_task()](@ref) is executing within.
"""
tls_world_age() = ccall(:jl_get_tls_world_age, UInt, ())

"""
    propertynames(x, private=false)

Get a tuple or a vector of the properties (`x.property`) of an object `x`.
This is typically the same as [`fieldnames(typeof(x))`](@ref), but types
that overload [`getproperty`](@ref) should generally overload `propertynames`
as well to get the properties of an instance of the type.

`propertynames(x)` may return only "public" property names that are part
of the documented interface of `x`.   If you want it to also return "private"
property names intended for internal use, pass `true` for the optional second argument.
REPL tab completion on `x.` shows only the `private=false` properties.

See also: [`hasproperty`](@ref), [`hasfield`](@ref).
"""
propertynames(x) = fieldnames(typeof(x))
propertynames(m::Module) = names(m)
propertynames(x, private::Bool) = propertynames(x) # ignore private flag by default
propertynames(x::Array) = () # hide the fields from tab completion to discourage calling `x.size` instead of `size(x)`, even though they are equivalent

"""
    hasproperty(x, s::Symbol)

Return a boolean indicating whether the object `x` has `s` as one of its own properties.

!!! compat "Julia 1.2"
     This function requires at least Julia 1.2.

See also: [`propertynames`](@ref), [`hasfield`](@ref).
"""
hasproperty(x, s::Symbol) = s in propertynames(x)

"""
    delete_method(m::Method)

Make method `m` uncallable and force recompilation of any methods that use(d) it.
"""
function delete_method(m::Method)
    ccall(:jl_method_table_disable, Cvoid, (Any, Any), get_methodtable(m), m)
end


# type for reflecting and pretty-printing a subset of methods
mutable struct MethodList <: AbstractArray{Method,1}
    ms::Array{Method,1}
    mt::Core.MethodTable
end

size(m::MethodList) = size(m.ms)
getindex(m::MethodList, i::Integer) = m.ms[i]

function MethodList(mt::Core.MethodTable)
    ms = Method[]
    visit(mt) do m
        push!(ms, m)
    end
    return MethodList(ms, mt)
end

"""
    methods(f, [types], [module])

Return the method table for `f`.

If `types` is specified, return an array of methods whose types match.
If `module` is specified, return an array of methods defined in that module.
A list of modules can also be specified as an array.

!!! compat "Julia 1.4"
    At least Julia 1.4 is required for specifying a module.

See also: [`which`](@ref), [`@which`](@ref Main.InteractiveUtils.@which) and [`methodswith`](@ref Main.InteractiveUtils.methodswith).
"""
function methods(@nospecialize(f), @nospecialize(t),
                 mod::Union{Tuple{Module},AbstractArray{Module},Nothing}=nothing)
    world = get_world_counter()
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    # Lack of specialization => a comprehension triggers too many invalidations via _collect, so collect the methods manually
    ms = Method[]
    for m in _methods(f, t, -1, world)::Vector
        m = m::Core.MethodMatch
        (mod === nothing || parentmodule(m.method) ∈ mod) && push!(ms, m.method)
    end
    MethodList(ms, typeof(f).name.mt)
end
methods(@nospecialize(f), @nospecialize(t), mod::Module) = methods(f, t, (mod,))

function methods_including_ambiguous(@nospecialize(f), @nospecialize(t))
    tt = signature_type(f, t)
    world = get_world_counter()
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    min = RefValue{UInt}(typemin(UInt))
    max = RefValue{UInt}(typemax(UInt))
    ms = _methods_by_ftype(tt, nothing, -1, world, true, min, max, Ptr{Int32}(C_NULL))::Vector
    return MethodList(Method[(m::Core.MethodMatch).method for m in ms], typeof(f).name.mt)
end

function methods(@nospecialize(f),
                 mod::Union{Module,AbstractArray{Module},Nothing}=nothing)
    # return all matches
    return methods(f, Tuple{Vararg{Any}}, mod)
end

# low-level method lookup functions used by the compiler

unionlen(@nospecialize(x)) = x isa Union ? unionlen(x.a) + unionlen(x.b) : 1

function _uniontypes(@nospecialize(x), ts::Array{Any,1})
    if x isa Union
        _uniontypes(x.a, ts)
        _uniontypes(x.b, ts)
    else
        push!(ts, x)
    end
    return ts
end
uniontypes(@nospecialize(x)) = _uniontypes(x, Any[])

function _methods(@nospecialize(f), @nospecialize(t), lim::Int, world::UInt)
    tt = signature_type(f, t)
    return _methods_by_ftype(tt, lim, world)
end

function _methods_by_ftype(@nospecialize(t), lim::Int, world::UInt)
    return _methods_by_ftype(t, nothing, lim, world)
end
function _methods_by_ftype(@nospecialize(t), mt::Union{Core.MethodTable, Nothing}, lim::Int, world::UInt)
    return _methods_by_ftype(t, mt, lim, world, false, RefValue{UInt}(typemin(UInt)), RefValue{UInt}(typemax(UInt)), Ptr{Int32}(C_NULL))
end
function _methods_by_ftype(@nospecialize(t), mt::Union{Core.MethodTable, Nothing}, lim::Int, world::UInt, ambig::Bool, min::Ref{UInt}, max::Ref{UInt}, has_ambig::Ref{Int32})
    return ccall(:jl_matching_methods, Any, (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ptr{Int32}), t, mt, lim, ambig, world, min, max, has_ambig)::Union{Vector{Any},Nothing}
end

hasgenerator(m::Method) = isdefined(m, :generator)
hasgenerator(m::Core.MethodInstance) = hasgenerator(m.def::Method)

function _uncompressed_ir(m::Method)
    s = m.source
    if s isa String
        s = ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Ptr{Cvoid}, Any), m, C_NULL, s)
    end
    return s::CodeInfo
end

_uncompressed_ir(codeinst::CodeInstance, s::String) =
    ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Any, Any), codeinst.def.def::Method, codeinst, s)

"""
    Base.generating_output([incremental::Bool])::Bool

Return `true` if the current process is being used to pre-generate a
code cache via any of the `--output-*` command line arguments. The optional
`incremental` argument further specifies the precompilation mode: when set
to `true`, the function will return `true` only for package precompilation;
when set to `false`, it will return `true` only for system image generation.

!!! compat "Julia 1.11"
    This function requires at least Julia 1.11.
"""
function generating_output(incremental::Union{Bool,Nothing}=nothing)
    ccall(:jl_generating_output, Cint, ()) == 0 && return false
    if incremental !== nothing
        JLOptions().incremental == incremental || return false
    end
    return true
end

const SLOT_USED = 0x8
ast_slotflag(@nospecialize(code), i) = ccall(:jl_ir_slotflag, UInt8, (Any, Csize_t), code, i - 1)

"""
    may_invoke_generator(method, atype, sparams) -> Bool

Computes whether or not we may invoke the generator for the given `method` on
the given `atype` and `sparams`. For correctness, all generated function are
required to return monotonic answers. However, since we don't expect users to
be able to successfully implement this criterion, we only call generated
functions on concrete types. The one exception to this is that we allow calling
generators with abstract types if the generator does not use said abstract type
(and thus cannot incorrectly use it to break monotonicity). This function
computes whether we are in either of these cases.

Unlike normal functions, the compilation heuristics still can't generate good dispatch
in some cases, but this may still allow inference not to fall over in some limited cases.
"""
function may_invoke_generator(mi::MethodInstance)
    return may_invoke_generator(mi.def::Method, mi.specTypes, mi.sparam_vals)
end
function may_invoke_generator(method::Method, @nospecialize(atype), sparams::SimpleVector)
    # If we have complete information, we may always call the generator
    isdispatchtuple(atype) && return true

    # We don't have complete information, but it is possible that the generator
    # syntactically doesn't make use of the information we don't have. Check
    # for that.

    # For now, only handle the (common, generated by the frontend case) that the
    # generator only has one method
    generator = method.generator
    isa(generator, Core.GeneratedFunctionStub) || return false
    tt = Tuple{typeof(generator.gen), Vararg{Any}}
    gen_mthds = _methods_by_ftype(tt, #=lim=#1, method.primary_world)
    gen_mthds isa Vector || return false
    length(gen_mthds) == 1 || return false

    generator_method = (first(gen_mthds)::Core.MethodMatch).method
    nsparams = length(sparams)
    isdefined(generator_method, :source) || return false
    code = generator_method.source
    nslots = ccall(:jl_ir_nslots, Int, (Any,), code)
    at = unwrap_unionall(atype)
    at isa DataType || return false
    (nslots >= 1 + length(sparams) + length(at.parameters)) || return false

    firstarg = 1
    for i = 1:nsparams
        if isa(sparams[i], TypeVar)
            if (ast_slotflag(code, firstarg + i) & SLOT_USED) != 0
                return false
            end
        end
    end
    nargs = Int(method.nargs)
    non_va_args = method.isva ? nargs - 1 : nargs
    for i = 1:non_va_args
        if !isdispatchelem(at.parameters[i])
            if (ast_slotflag(code, firstarg + i + nsparams) & SLOT_USED) != 0
                return false
            end
        end
    end
    if method.isva
        # If the va argument is used, we need to ensure that all arguments that
        # contribute to the va tuple are dispatchelemes
        if (ast_slotflag(code, firstarg + nargs + nsparams) & SLOT_USED) != 0
            for i = (non_va_args+1):length(at.parameters)
                if !isdispatchelem(at.parameters[i])
                    return false
                end
            end
        end
    end
    return true
end

# get a handle to the unique specialization object representing a particular instantiation of a call
# eliminate UnionAll vars that might be degenerate due to having identical bounds,
# or a concrete upper bound and appearing covariantly.
function subst_trivial_bounds(@nospecialize(atype))
    if !isa(atype, UnionAll)
        return atype
    end
    v = atype.var
    if isconcretetype(v.ub) || v.lb === v.ub
        subst = try
            atype{v.ub}
        catch
            # Note in rare cases a var bound might not be valid to substitute.
            nothing
        end
        if subst !== nothing
            return subst_trivial_bounds(subst)
        end
    end
    return UnionAll(v, subst_trivial_bounds(atype.body))
end

# If removing trivial vars from atype results in an equivalent type, use that
# instead. Otherwise we can get a case like issue #38888, where a signature like
#   f(x::S) where S<:Int
# gets cached and matches a concrete dispatch case.
function normalize_typevars(method::Method, @nospecialize(atype), sparams::SimpleVector)
    at2 = subst_trivial_bounds(atype)
    if at2 !== atype && at2 == atype
        atype = at2
        sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), at2, method.sig)::SimpleVector
        sparams = sp_[2]::SimpleVector
    end
    return Pair{Any,SimpleVector}(atype, sparams)
end

function get_nospecializeinfer_sig(method::Method, @nospecialize(atype), sparams::SimpleVector)
    isa(atype, DataType) || return method.sig
    mt = ccall(:jl_method_get_table, Any, (Any,), method)
    mt === nothing && return method.sig
    return ccall(:jl_normalize_to_compilable_sig, Any, (Any, Any, Any, Any, Cint),
        mt, atype, sparams, method, #=int return_if_compileable=#0)
end

is_nospecialized(method::Method) = method.nospecialize ≠ 0
is_nospecializeinfer(method::Method) = method.nospecializeinfer && is_nospecialized(method)
function specialize_method(method::Method, @nospecialize(atype), sparams::SimpleVector; preexisting::Bool=false)
    @inline
    if isa(atype, UnionAll)
        atype, sparams = normalize_typevars(method, atype, sparams)
    end
    if is_nospecializeinfer(method)
        atype = get_nospecializeinfer_sig(method, atype, sparams)
    end
    if preexisting
        # check cached specializations
        # for an existing result stored there
        return ccall(:jl_specializations_lookup, Any, (Any, Any), method, atype)::Union{Nothing,MethodInstance}
    end
    return ccall(:jl_specializations_get_linfo, Ref{MethodInstance}, (Any, Any, Any), method, atype, sparams)
end

function specialize_method(match::Core.MethodMatch; kwargs...)
    return specialize_method(match.method, match.spec_types, match.sparams; kwargs...)
end

hasintersect(@nospecialize(a), @nospecialize(b)) = typeintersect(a, b) !== Bottom

###########
# scoping #
###########

_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module


# high-level, more convenient method lookup functions

function visit(f, mt::Core.MethodTable)
    mt.defs !== nothing && visit(f, mt.defs)
    nothing
end
function visit(f, mc::Core.TypeMapLevel)
    function avisit(f, e::Memory{Any})
        for i in 2:2:length(e)
            isassigned(e, i) || continue
            ei = e[i]
            if ei isa Memory{Any}
                for j in 2:2:length(ei)
                    isassigned(ei, j) || continue
                    visit(f, ei[j])
                end
            else
                visit(f, ei)
            end
        end
    end
    if mc.targ !== nothing
        avisit(f, mc.targ::Memory{Any})
    end
    if mc.arg1 !== nothing
        avisit(f, mc.arg1::Memory{Any})
    end
    if mc.tname !== nothing
        avisit(f, mc.tname::Memory{Any})
    end
    if mc.name1 !== nothing
        avisit(f, mc.name1::Memory{Any})
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
struct MethodSpecializations
    specializations::Union{Nothing, Core.MethodInstance, Core.SimpleVector}
end
"""
    specializations(m::Method) → itr

Return an iterator `itr` of all compiler-generated specializations of `m`.
"""
specializations(m::Method) = MethodSpecializations(isdefined(m, :specializations) ? m.specializations : nothing)
function iterate(specs::MethodSpecializations)
    s = specs.specializations
    s === nothing && return nothing
    isa(s, Core.MethodInstance) && return (s, nothing)
    return iterate(specs, 0)
end
iterate(specs::MethodSpecializations, ::Nothing) = nothing
function iterate(specs::MethodSpecializations, i::Int)
    s = specs.specializations::Core.SimpleVector
    n = length(s)
    i >= n && return nothing
    item = nothing
    while i < n && item === nothing
        item = s[i+=1]
    end
    item === nothing && return nothing
    return (item, i)
end
length(specs::MethodSpecializations) = count(Returns(true), specs)

function length(mt::Core.MethodTable)
    n = 0
    visit(mt) do m
        n += 1
    end
    return n::Int
end
isempty(mt::Core.MethodTable) = (mt.defs === nothing)

uncompressed_ir(m::Method) = isdefined(m, :source) ? _uncompressed_ir(m) :
                             isdefined(m, :generator) ? error("Method is @generated; try `code_lowered` instead.") :
                             error("Code for this Method is not available.")

has_image_globalref(m::Method) = ccall(:jl_ir_flag_has_image_globalref, Bool, (Any,), m.source)
