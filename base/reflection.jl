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

is_root_module(m::Module) = parentmodule(m) === m || (isdefined(Main, :Base) && m === Main.Base)

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
    names(x::Module; all::Bool = false, imported::Bool = false)

Get a vector of the public names of a `Module`, excluding deprecated names.
If `all` is true, then the list also includes non-public names defined in the module,
deprecated names, and compiler-generated names.
If `imported` is true, then names explicitly imported from other modules
are also included. Names are returned in sorted order.

As a special case, all names defined in `Main` are considered \"public\",
since it is not idiomatic to explicitly mark names from `Main` as public.

!!! note
    `sym ∈ names(SomeModule)` does *not* imply `isdefined(SomeModule, sym)`.
    `names` will return symbols marked with `public` or `export`, even if
    they are not defined in the module.

See also: [`Base.isexported`](@ref), [`Base.ispublic`](@ref), [`Base.@locals`](@ref), [`@__MODULE__`](@ref).
"""
names(m::Module; all::Bool = false, imported::Bool = false) =
    sort!(unsorted_names(m; all, imported))
unsorted_names(m::Module; all::Bool = false, imported::Bool = false) =
    ccall(:jl_module_names, Array{Symbol,1}, (Any, Cint, Cint), m, all, imported)

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

"""
    isbindingresolved(m::Module, s::Symbol) -> Bool

Returns whether the binding of a symbol in a module is resolved.

See also: [`isexported`](@ref), [`ispublic`](@ref), [`isdeprecated`](@ref)

```jldoctest
julia> module Mod
           foo() = 17
       end
Mod

julia> Base.isbindingresolved(Mod, :foo)
true

julia> Base.isbindingresolved(Mod, :bar)
false
```
"""
isbindingresolved(m::Module, var::Symbol) = ccall(:jl_binding_resolved_p, Cint, (Any, Any), m, var) != 0

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

See also [`propertynames`](@ref), [`hasfield`](@ref).

# Examples
```jldoctest
julia> fieldnames(Rational)
(:num, :den)

julia> fieldnames(typeof(1+im))
(:re, :im)
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

Determine whether a global is declared `const` in a given module `m`.
"""
isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Cint, (Any, Any), m, s) != 0

function isconst(g::GlobalRef)
    return ccall(:jl_globalref_is_const, Cint, (Any,), g) != 0
end

"""
    isconst(t::DataType, s::Union{Int,Symbol}) -> Bool

Determine whether a field `s` is declared `const` in a given type `t`.
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
with no intervening padding bits (defined as bits whose value does not uniquely
impact the egal test when applied to the struct fields).
Can be called on any `isconcretetype`.
"""
function datatype_haspadding(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    flags = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).flags
    return flags & 1 == 1
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
    Base.datatype_pointerfree(dt::DataType) -> Bool

Return whether instances of this type can contain references to gc-managed memory.
Can be called on any `isconcretetype`.
"""
function datatype_pointerfree(dt::DataType)
    @_foldable_meta
    dt.layout == C_NULL && throw(UndefRefError())
    npointers = unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).npointers
    return npointers == 0
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

Determine whether type `T` is a tuple "leaf type",
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
has_free_typevars(@nospecialize(t)) = (@_total_meta; ccall(:jl_has_free_typevars, Cint, (Any,), t) != 0)

# equivalent to isa(v, Type) && isdispatchtuple(Tuple{v}) || v === Union{}
# and is thus perhaps most similar to the old (pre-1.0) `isleaftype` query
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
13-element Vector{Tuple{UInt64, Symbol, Type}}:
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
ERROR: type Foo has no field z
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

"""
    code_lowered(f, types; generated=true, debuginfo=:default)

Return an array of the lowered forms (IR) for the methods matching the given generic function
and type signature.

If `generated` is `false`, the returned `CodeInfo` instances will correspond to fallback
implementations. An error is thrown if no fallback implementation exists.
If `generated` is `true`, these `CodeInfo` instances will correspond to the method bodies
yielded by expanding the generators.

The keyword `debuginfo` controls the amount of code metadata present in the output.

Note that an error will be thrown if `types` are not leaf types when `generated` is
`true` and any of the corresponding methods are an `@generated` method.
"""
function code_lowered(@nospecialize(f), @nospecialize(t=Tuple); generated::Bool=true, debuginfo::Symbol=:default)
    if @isdefined(IRShow)
        debuginfo = IRShow.debuginfo(debuginfo)
    elseif debuginfo === :default
        debuginfo = :source
    end
    if debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    world = get_world_counter()
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    ret = CodeInfo[]
    for m in method_instances(f, t, world)
        if generated && hasgenerator(m)
            if may_invoke_generator(m)
                code = ccall(:jl_code_for_staged, Any, (Any, UInt), m, world)::CodeInfo
            else
                error("Could not expand generator for `@generated` method ", m, ". ",
                      "This can happen if the provided argument types (", t, ") are ",
                      "not leaf types, but the `generated` argument is `true`.")
            end
        else
            code = uncompressed_ir(m.def::Method)
            debuginfo === :none && remove_linenums!(code)
        end
        push!(ret, code)
    end
    return ret
end

hasgenerator(m::Method) = isdefined(m, :generator)
hasgenerator(m::Core.MethodInstance) = hasgenerator(m.def::Method)

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
    return _methods_by_ftype(t, nothing, lim, world)
end
function _methods_by_ftype(@nospecialize(t), mt::Union{Core.MethodTable, Nothing}, lim::Int, world::UInt)
    return _methods_by_ftype(t, mt, lim, world, false, RefValue{UInt}(typemin(UInt)), RefValue{UInt}(typemax(UInt)), Ptr{Int32}(C_NULL))
end
function _methods_by_ftype(@nospecialize(t), mt::Union{Core.MethodTable, Nothing}, lim::Int, world::UInt, ambig::Bool, min::Ref{UInt}, max::Ref{UInt}, has_ambig::Ref{Int32})
    return ccall(:jl_matching_methods, Any, (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ptr{Int32}), t, mt, lim, ambig, world, min, max, has_ambig)::Union{Vector{Any},Nothing}
end

# high-level, more convenient method lookup functions

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
function _uncompressed_ir(m::Method)
    s = m.source
    if s isa String
        s = ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Ptr{Cvoid}, Any), m, C_NULL, s)
    end
    return s::CodeInfo
end

# for backwards compat
const uncompressed_ast = uncompressed_ir
const _uncompressed_ast = _uncompressed_ir

function method_instances(@nospecialize(f), @nospecialize(t), world::UInt)
    tt = signature_type(f, t)
    results = Core.MethodInstance[]
    # this make a better error message than the typeassert that follows
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    for match in _methods_by_ftype(tt, -1, world)::Vector
        instance = Core.Compiler.specialize_method(match::Core.MethodMatch)
        push!(results, instance)
    end
    return results
end

function method_instance(@nospecialize(f), @nospecialize(t);
                         world=Base.get_world_counter(), method_table=nothing)
    tt = signature_type(f, t)
    mi = ccall(:jl_method_lookup_by_tt, Any,
                (Any, Csize_t, Any),
                tt, world, method_table)
    return mi::Union{Nothing, MethodInstance}
end

default_debug_info_kind() = unsafe_load(cglobal(:jl_default_debug_info_kind, Cint))

# this type mirrors jl_cgparams_t (documented in julia.h)
struct CodegenParams
    """
    If enabled, generate the necessary code to support the --track-allocations
    command line flag to julia itself. Note that the option itself does not enable
    allocation tracking. Rather, it merely generates the support code necessary
    to perform allocation tracking if requested by the command line option.
    """
    track_allocations::Cint

    """
    If enabled, generate the necessary code to support the --code-coverage
    command line flag to julia itself. Note that the option itself does not enable
    code coverage. Rather, it merely generates the support code necessary
    to code coverage if requested by the command line option.
    """
    code_coverage::Cint

    """
    If enabled, force the compiler to use the specialized signature
    for all generated functions, whenever legal. If disabled, the choice is made
    heuristically and specsig is only used when deemed profitable.
    """
    prefer_specsig::Cint

    """
    If enabled, enable emission of `.debug_names` sections.
    """
    gnu_pubnames::Cint

    """
    Controls what level of debug info to emit. Currently supported values are:
    - 0: no debug info
    - 1: full debug info
    - 2: Line tables only
    - 3: Debug directives only

    The integer values currently match the llvm::DICompilerUnit::DebugEmissionKind enum,
    although this is not guaranteed.
    """
    debug_info_kind::Cint

    """
    Controls the debug_info_level parameter, equivalent to the -g command line option.
    """
    debug_info_level::Cint

    """
    If enabled, generate a GC safepoint at the entry to every function. Emitting
    these extra safepoints can reduce the amount of time that other threads are
    waiting for the currently running thread to reach a safepoint. The cost for
    a safepoint is small, but non-zero. The option is enabled by default.
    """
    safepoint_on_entry::Cint

    """
    If enabled, add an implicit argument to each function call that is used to
    pass down the current task local state pointer. This argument is passed
    using the `swiftself` convention, which in the ordinary case means that the
    pointer is kept in a register and accesses are thus very fast. If this option
    is disabled, the task local state pointer must be loaded from thread local
    storage, which incurs a small amount of additional overhead. The option is enabled by
    default.
    """
    gcstack_arg::Cint

    """
    If enabled, use the Julia PLT mechanism to support lazy-resolution of `ccall`
    targets. The option may be disabled for use in environments where the julia
    runtime is unavailable, but is otherwise recommended to be enabled, even if
    lazy resolution is not required, as the Julia PLT mechanism may have superior
    performance compared to the native platform mechanism. The options is enabled by default.
    """
    use_jlplt::Cint

    """
    A pointer of type

    typedef jl_value_t *(*jl_codeinstance_lookup_t)(jl_method_instance_t *mi JL_PROPAGATES_ROOT,
    size_t min_world, size_t max_world);

    that may be used by external compilers as a callback to look up the code instance corresponding
    to a particular method instance.
    """
    lookup::Ptr{Cvoid}

    function CodegenParams(; track_allocations::Bool=true, code_coverage::Bool=true,
                   prefer_specsig::Bool=false,
                   gnu_pubnames::Bool=true, debug_info_kind::Cint = default_debug_info_kind(),
                   debug_info_level::Cint = Cint(JLOptions().debug_level), safepoint_on_entry::Bool=true,
                   gcstack_arg::Bool=true, use_jlplt::Bool=true,
                   lookup::Ptr{Cvoid}=unsafe_load(cglobal(:jl_rettype_inferred_addr, Ptr{Cvoid})))
        return new(
            Cint(track_allocations), Cint(code_coverage),
            Cint(prefer_specsig),
            Cint(gnu_pubnames), debug_info_kind,
            debug_info_level, Cint(safepoint_on_entry),
            Cint(gcstack_arg), Cint(use_jlplt),
            lookup)
    end
end

# this type mirrors jl_emission_params_t (documented in julia.h)
struct EmissionParams
    emit_metadata::Cint

    function EmissionParams(; emit_metadata::Bool=true)
        return new(Cint(emit_metadata))
    end
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

"""
    code_typed(f, types; kw...)

Returns an array of type-inferred lowered form (IR) for the methods matching the given
generic function and type signature.

# Keyword Arguments

- `optimize::Bool = true`: optional, controls whether additional optimizations,
  such as inlining, are also applied.
- `debuginfo::Symbol = :default`: optional, controls the amount of code metadata present
  in the output, possible options are `:source` or `:none`.

# Internal Keyword Arguments

This section should be considered internal, and is only for who understands Julia compiler
internals.

- `world::UInt = Base.get_world_counter()`: optional, controls the world age to use
  when looking up methods, use current world age if not specified.
- `interp::Core.Compiler.AbstractInterpreter = Core.Compiler.NativeInterpreter(world)`:
  optional, controls the abstract interpreter to use, use the native interpreter if not specified.

# Examples

One can put the argument types in a tuple to get the corresponding `code_typed`.

```julia
julia> code_typed(+, (Float64, Float64))
1-element Vector{Any}:
 CodeInfo(
1 ─ %1 = Base.add_float(x, y)::Float64
└──      return %1
) => Float64
```
"""
function code_typed(@nospecialize(f), @nospecialize(types=default_tt(f)); kwargs...)
    if isa(f, Core.OpaqueClosure)
        return code_typed_opaque_closure(f, types; kwargs...)
    end
    tt = signature_type(f, types)
    return code_typed_by_type(tt; kwargs...)
end

# returns argument tuple type which is supposed to be used for `code_typed` and its family;
# if there is a single method this functions returns the method argument signature,
# otherwise returns `Tuple` that doesn't match with any signature
function default_tt(@nospecialize(f))
    ms = methods(f)
    if length(ms) == 1
        return tuple_type_tail(only(ms).sig)
    else
        return Tuple
    end
end

"""
    code_typed_by_type(types::Type{<:Tuple}; ...)

Similar to [`code_typed`](@ref), except the argument is a tuple type describing
a full signature to query.
"""
function code_typed_by_type(@nospecialize(tt::Type);
                            optimize::Bool=true,
                            debuginfo::Symbol=:default,
                            world::UInt=get_world_counter(),
                            interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")
    if @isdefined(IRShow)
        debuginfo = IRShow.debuginfo(debuginfo)
    elseif debuginfo === :default
        debuginfo = :source
    end
    if debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    tt = to_tuple_type(tt)
    matches = _methods_by_ftype(tt, #=lim=#-1, world)::Vector
    asts = []
    for match in matches
        match = match::Core.MethodMatch
        (code, ty) = Core.Compiler.typeinf_code(interp, match, optimize)
        if code === nothing
            push!(asts, match.method => Any)
        else
            debuginfo === :none && remove_linenums!(code)
            push!(asts, code => ty)
        end
    end
    return asts
end

function get_oc_code_rt(oc::Core.OpaqueClosure, types, optimize::Bool)
    @nospecialize oc types
    ccall(:jl_is_in_pure_context, Bool, ()) &&
        error("code reflection cannot be used from generated functions")
    m = oc.source
    if isa(m, Method)
        if isdefined(m, :source)
            if optimize
                tt = Tuple{typeof(oc.captures), to_tuple_type(types).parameters...}
                mi = Core.Compiler.specialize_method(m, tt, Core.svec())
                interp = Core.Compiler.NativeInterpreter(m.primary_world)
                return Core.Compiler.typeinf_code(interp, mi, optimize)
            else
                code = _uncompressed_ir(m)
                return Pair{CodeInfo,Any}(code, typeof(oc).parameters[2])
            end
        else
            # OC constructed from optimized IR
            codeinst = m.specializations.cache
            return Pair{CodeInfo, Any}(codeinst.inferred, codeinst.rettype)
        end
    else
        error("encountered invalid Core.OpaqueClosure object")
    end
end

function code_typed_opaque_closure(oc::Core.OpaqueClosure, types;
                                   debuginfo::Symbol=:default,
                                   optimize::Bool=true,
                                   _...)
    @nospecialize oc types
    (code, rt) = get_oc_code_rt(oc, types, optimize)
    debuginfo === :none && remove_linenums!(code)
    return Any[Pair{CodeInfo,Any}(code, rt)]
end

"""
    code_ircode(f, [types])

Return an array of pairs of `IRCode` and inferred return type if type inference succeeds.
The `Method` is included instead of `IRCode` otherwise.

See also: [`code_typed`](@ref)

# Internal Keyword Arguments

This section should be considered internal, and is only for who understands Julia compiler
internals.

- `world::UInt = Base.get_world_counter()`: optional, controls the world age to use
  when looking up methods, use current world age if not specified.
- `interp::Core.Compiler.AbstractInterpreter = Core.Compiler.NativeInterpreter(world)`:
  optional, controls the abstract interpreter to use, use the native interpreter if not specified.
- `optimize_until::Union{Integer,AbstractString,Nothing} = nothing`: optional,
  controls the optimization passes to run.
  If it is a string, it specifies the name of the pass up to which the optimizer is run.
  If it is an integer, it specifies the number of passes to run.
  If it is `nothing` (default), all passes are run.

# Examples

One can put the argument types in a tuple to get the corresponding `code_ircode`.

```julia
julia> Base.code_ircode(+, (Float64, Int64))
1-element Vector{Any}:
 388 1 ─ %1 = Base.sitofp(Float64, _3)::Float64
    │   %2 = Base.add_float(_2, %1)::Float64
    └──      return %2
     => Float64

julia> Base.code_ircode(+, (Float64, Int64); optimize_until = "compact 1")
1-element Vector{Any}:
 388 1 ─ %1 = Base.promote(_2, _3)::Tuple{Float64, Float64}
    │   %2 = Core._apply_iterate(Base.iterate, Base.:+, %1)::Float64
    └──      return %2
     => Float64
```
"""
function code_ircode(@nospecialize(f), @nospecialize(types = default_tt(f)); kwargs...)
    if isa(f, Core.OpaqueClosure)
        error("OpaqueClosure not supported")
    end
    tt = signature_type(f, types)
    return code_ircode_by_type(tt; kwargs...)
end

"""
    code_ircode_by_type(types::Type{<:Tuple}; ...)

Similar to [`code_ircode`](@ref), except the argument is a tuple type describing
a full signature to query.
"""
function code_ircode_by_type(
    @nospecialize(tt::Type);
    world::UInt=get_world_counter(),
    interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world),
    optimize_until::Union{Integer,AbstractString,Nothing}=nothing,
)
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")
    tt = to_tuple_type(tt)
    matches = _methods_by_ftype(tt, #=lim=#-1, world)::Vector
    asts = []
    for match in matches
        match = match::Core.MethodMatch
        (code, ty) = Core.Compiler.typeinf_ircode(interp, match, optimize_until)
        if code === nothing
            push!(asts, match.method => Any)
        else
            push!(asts, code => ty)
        end
    end
    return asts
end

function _builtin_return_type(interp::Core.Compiler.AbstractInterpreter,
                              @nospecialize(f::Core.Builtin), @nospecialize(types))
    argtypes = Any[to_tuple_type(types).parameters...]
    rt = Core.Compiler.builtin_tfunction(interp, f, argtypes, nothing)
    return Core.Compiler.widenconst(rt)
end

function _builtin_effects(interp::Core.Compiler.AbstractInterpreter,
                          @nospecialize(f::Core.Builtin), @nospecialize(types))
    argtypes = Any[to_tuple_type(types).parameters...]
    rt = Core.Compiler.builtin_tfunction(interp, f, argtypes, nothing)
    return Core.Compiler.builtin_effects(Core.Compiler.typeinf_lattice(interp), f, argtypes, rt)
end

check_generated_context(world::UInt) =
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")

# TODO rename `Base.return_types` to `Base.infer_return_types`

"""
    Base.return_types(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::NativeInterpreter=Core.Compiler.NativeInterpreter(world)) -> rts::Vector{Any}

Return a list of possible return types for a given function `f` and argument types `types`.
The list corresponds to the results of type inference on all the possible method match
candidates for `f` and `types` (see also [`methods(f, types)`](@ref methods).

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `rts::Vector{Any}`: The list of return types that are figured out by inference on
  methods matching with the given `f` and `types`. The list's order matches the order
  returned by `methods(f, types)`.

# Examples

```julia
julia> Base.return_types(sum, Tuple{Vector{Int}})
1-element Vector{Any}:
 Int64

julia> methods(sum, (Union{Vector{Int},UnitRange{Int}},))
# 2 methods for generic function "sum" from Base:
 [1] sum(r::AbstractRange{<:Real})
     @ range.jl:1399
 [2] sum(a::AbstractArray; dims, kw...)
     @ reducedim.jl:1010

julia> Base.return_types(sum, (Union{Vector{Int},UnitRange{Int}},))
2-element Vector{Any}:
 Int64 # the result of inference on sum(r::AbstractRange{<:Real})
 Int64 # the result of inference on sum(a::AbstractArray; dims, kw...)
```

!!! warning
    The `Base.return_types` function should not be used from generated functions;
    doing so will result in an error.
"""
function return_types(@nospecialize(f), @nospecialize(types=default_tt(f));
                      world::UInt=get_world_counter(),
                      interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        _, rt = only(code_typed_opaque_closure(f, types))
        return Any[rt]
    end
    if isa(f, Core.Builtin)
        rt = _builtin_return_type(interp, f, types)
        return Any[rt]
    end
    rts = Any[]
    tt = signature_type(f, types)
    matches = _methods_by_ftype(tt, #=lim=#-1, world)::Vector
    for match in matches
        ty = Core.Compiler.typeinf_type(interp, match::Core.MethodMatch)
        push!(rts, something(ty, Any))
    end
    return rts
end

"""
    Base.infer_return_type(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> rt::Type

Returns an inferred return type of the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `rt::Type`: An inferred return type of the function call specified by the given call signature.

!!! note
    Note that, different from [`Base.return_types`](@ref), this doesn't give you the list
    return types of every possible method matching with the given `f` and `types`.
    It returns a single return type, taking into account all potential outcomes of
    any function call entailed by the given signature type.

# Examples

```julia
julia> checksym(::Symbol) = :symbol;

julia> checksym(x::Any) = x;

julia> Base.infer_return_type(checksym, (Union{Symbol,String},))
Union{String, Symbol}

julia> Base.return_types(checksym, (Union{Symbol,String},))
2-element Vector{Any}:
 Symbol
 Union{String, Symbol}
```

It's important to note the difference here: `Base.return_types` gives back inferred results
for each method that matches the given signature `checksum(::Union{Symbol,String})`.
On the other hand `Base.infer_return_type` returns one collective result that sums up all those possibilities.

!!! warning
    The `Base.infer_return_type` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_return_type(@nospecialize(f), @nospecialize(types=default_tt(f));
                           world::UInt=get_world_counter(),
                           interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return last(only(code_typed_opaque_closure(f, types)))
    end
    if isa(f, Core.Builtin)
        return _builtin_return_type(interp, f, types)
    end
    tt = signature_type(f, types)
    matches = Core.Compiler.findall(tt, Core.Compiler.method_table(interp))
    if matches === nothing
        # unanalyzable call, i.e. the interpreter world might be newer than the world where
        # the `f` is defined, return the unknown return type
        return Any
    end
    rt = Union{}
    for match in matches.matches
        ty = Core.Compiler.typeinf_type(interp, match::Core.MethodMatch)
        rt = Core.Compiler.tmerge(rt, something(ty, Any))
    end
    return rt
end

"""
    Base.infer_exception_types(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::NativeInterpreter=Core.Compiler.NativeInterpreter(world)) -> excts::Vector{Any}

Return a list of possible exception types for a given function `f` and argument types `types`.
The list corresponds to the results of type inference on all the possible method match
candidates for `f` and `types` (see also [`methods(f, types)`](@ref methods).
It works like [`Base.return_types`](@ref), but it infers the exception types instead of the return types.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `excts::Vector{Any}`: The list of exception types that are figured out by inference on
  methods matching with the given `f` and `types`. The list's order matches the order
  returned by `methods(f, types)`.

# Examples

```julia
julia> throw_if_number(::Number) = error("number is given");

julia> throw_if_number(::Any) = nothing;

julia> Base.infer_exception_types(throw_if_number, (Int,))
1-element Vector{Any}:
 ErrorException

julia> methods(throw_if_number, (Any,))
# 2 methods for generic function "throw_if_number" from Main:
 [1] throw_if_number(x::Number)
     @ REPL[1]:1
 [2] throw_if_number(::Any)
     @ REPL[2]:1

julia> Base.infer_exception_types(throw_if_number, (Any,))
2-element Vector{Any}:
 ErrorException # the result of inference on `throw_if_number(::Number)`
 Union{}        # the result of inference on `throw_if_number(::Any)`
```

!!! warning
    The `Base.infer_exception_types` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_exception_types(@nospecialize(f), @nospecialize(types=default_tt(f));
                               world::UInt=get_world_counter(),
                               interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return Any[Any] # TODO
    end
    if isa(f, Core.Builtin)
        effects = _builtin_effects(interp, f, types)
        exct = Core.Compiler.is_nothrow(effects) ? Union{} : Any
        return Any[exct]
    end
    excts = Any[]
    tt = signature_type(f, types)
    matches = _methods_by_ftype(tt, #=lim=#-1, world)::Vector
    for match in matches
        match = match::Core.MethodMatch
        frame = Core.Compiler.typeinf_frame(interp, match, #=run_optimizer=#false)
        if frame === nothing
            exct = Any
        else
            exct = Core.Compiler.widenconst(frame.result.exc_result)
        end
        push!(excts, exct)
    end
    return excts
end

_may_throw_methoderror(matches#=::Core.Compiler.MethodLookupResult=#) =
    matches.ambig || !any(match::Core.MethodMatch->match.fully_covers, matches.matches)

"""
    Base.infer_exception_type(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> exct::Type

Returns the type of exception potentially thrown by the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `exct::Type`: The inferred type of exception that can be thrown by the function call
  specified by the given call signature.

!!! note
    Note that, different from [`Base.infer_exception_types`](@ref), this doesn't give you the list
    exception types for every possible matching method with the given `f` and `types`.
    It returns a single exception type, taking into account all potential outcomes of
    any function call entailed by the given signature type.

# Examples

```julia
julia> f1(x) = x * 2;

julia> Base.infer_exception_type(f1, (Int,))
Union{}
```

The exception inferred as `Union{}` indicates that `f1(::Int)` will not throw any exception.

```julia
julia> f2(x::Int) = x * 2;

julia> Base.infer_exception_type(f2, (Integer,))
MethodError
```

This case is pretty much the same as with `f1`, but there's a key difference to note. For
`f2`, the argument type is limited to `Int`, while the argument type is given as `Tuple{Integer}`.
Because of this, taking into account the chance of the method error entailed by the call
signature, the exception type is widened to `MethodError`.

!!! warning
    The `Base.infer_exception_type` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_exception_type(@nospecialize(f), @nospecialize(types=default_tt(f));
                              world::UInt=get_world_counter(),
                              interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return Any # TODO
    end
    if isa(f, Core.Builtin)
        effects = _builtin_effects(interp, f, types)
        return Core.Compiler.is_nothrow(effects) ? Union{} : Any
    end
    tt = signature_type(f, types)
    matches = Core.Compiler.findall(tt, Core.Compiler.method_table(interp))
    if matches === nothing
        # unanalyzable call, i.e. the interpreter world might be newer than the world where
        # the `f` is defined, return the unknown exception type
        return Any
    end
    exct = Union{}
    if _may_throw_methoderror(matches)
        # account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
        exct = Core.Compiler.tmerge(exct, MethodError)
    end
    for match in matches.matches
        match = match::Core.MethodMatch
        frame = Core.Compiler.typeinf_frame(interp, match, #=run_optimizer=#false)
        frame === nothing && return Any
        exct = Core.Compiler.tmerge(exct, Core.Compiler.widenconst(frame.result.exc_result))
    end
    return exct
end

"""
    Base.infer_effects(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> effects::Effects

Returns the possible computation effects of the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `effects::Effects`: The computed effects of the function call specified by the given call signature.
  See the documentation of [`Effects`](@ref Core.Compiler.Effects) or [`Base.@assume_effects`](@ref)
  for more information on the various effect properties.

!!! note
    Note that, different from [`Base.return_types`](@ref), this doesn't give you the list
    effect analysis results for every possible matching method with the given `f` and `types`.
    It returns a single effect, taking into account all potential outcomes of any function
    call entailed by the given signature type.

# Examples

```julia
julia> f1(x) = x * 2;

julia> Base.infer_effects(f1, (Int,))
(+c,+e,+n,+t,+s,+m,+i)
```

This function will return an `Effects` object with information about the computational
effects of the function `f1` when called with an `Int` argument.

```julia
julia> f2(x::Int) = x * 2;

julia> Base.infer_effects(f2, (Integer,))
(+c,+e,!n,+t,+s,+m,+i)
```

This case is pretty much the same as with `f1`, but there's a key difference to note. For
`f2`, the argument type is limited to `Int`, while the argument type is given as `Tuple{Integer}`.
Because of this, taking into account the chance of the method error entailed by the call
signature, the `:nothrow` bit gets tainted.

!!! warning
    The `Base.infer_effects` function should not be used from generated functions;
    doing so will result in an error.

# See Also
- [`Core.Compiler.Effects`](@ref): A type representing the computational effects of a method call.
- [`Base.@assume_effects`](@ref): A macro for making assumptions about the effects of a method.
"""
function infer_effects(@nospecialize(f), @nospecialize(types=default_tt(f));
                       world::UInt=get_world_counter(),
                       interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    check_generated_context(world)
    if isa(f, Core.Builtin)
        return _builtin_effects(interp, f, types)
    end
    tt = signature_type(f, types)
    matches = Core.Compiler.findall(tt, Core.Compiler.method_table(interp))
    if matches === nothing
        # unanalyzable call, i.e. the interpreter world might be newer than the world where
        # the `f` is defined, return the unknown effects
        return Core.Compiler.Effects()
    end
    effects = Core.Compiler.EFFECTS_TOTAL
    if _may_throw_methoderror(matches)
        # account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
        effects = Core.Compiler.Effects(effects; nothrow=false)
    end
    for match in matches.matches
        match = match::Core.MethodMatch
        frame = Core.Compiler.typeinf_frame(interp, match, #=run_optimizer=#true)
        frame === nothing && return Core.Compiler.Effects()
        effects = Core.Compiler.merge_effects(effects, frame.result.ipo_effects)
    end
    return effects
end

"""
    print_statement_costs(io::IO, f, types)

Print type-inferred and optimized code for `f` given argument types `types`,
prepending each line with its cost as estimated by the compiler's inlining engine.
"""
function print_statement_costs(io::IO, @nospecialize(f), @nospecialize(t); kwargs...)
    tt = signature_type(f, t)
    print_statement_costs(io, tt; kwargs...)
end

function print_statement_costs(io::IO, @nospecialize(tt::Type);
                               world::UInt=get_world_counter(),
                               interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world))
    tt = to_tuple_type(tt)
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    matches = _methods_by_ftype(tt, #=lim=#-1, world)::Vector
    params = Core.Compiler.OptimizationParams(interp)
    cst = Int[]
    for match in matches
        match = match::Core.MethodMatch
        println(io, match.method)
        (code, ty) = Core.Compiler.typeinf_code(interp, match, true)
        if code === nothing
            println(io, "  inference not successful")
        else
            empty!(cst)
            resize!(cst, length(code.code))
            sptypes = Core.Compiler.VarState[Core.Compiler.VarState(sp, false) for sp in match.sparams]
            maxcost = Core.Compiler.statement_costs!(cst, code.code, code, sptypes, params)
            nd = ndigits(maxcost)
            irshow_config = IRShow.IRShowConfig() do io, linestart, idx
                print(io, idx > 0 ? lpad(cst[idx], nd+1) : " "^(nd+1), " ")
                return ""
            end
            IRShow.show_ir(io, code, irshow_config)
        end
        println(io)
    end
end

print_statement_costs(args...; kwargs...) = print_statement_costs(stdout, args...; kwargs...)

function _which(@nospecialize(tt::Type);
    method_table::Union{Nothing,Core.MethodTable,Core.Compiler.MethodTableView}=nothing,
    world::UInt=get_world_counter(),
    raise::Bool=true)
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    if method_table === nothing
        table = Core.Compiler.InternalMethodTable(world)
    elseif method_table isa Core.MethodTable
        table = Core.Compiler.OverlayMethodTable(world, method_table)
    else
        table = method_table
    end
    match, = Core.Compiler.findsup(tt, table)
    if match === nothing
        raise && error("no unique matching method found for the specified argument types")
        return nothing
    end
    return match
end

"""
    which(f, types)

Returns the method of `f` (a `Method` object) that would be called for arguments of the given `types`.

If `types` is an abstract type, then the method that would be called by `invoke` is returned.

See also: [`parentmodule`](@ref), [`@which`](@ref Main.InteractiveUtils.@which), and [`@edit`](@ref Main.InteractiveUtils.@edit).
"""
function which(@nospecialize(f), @nospecialize(t))
    tt = signature_type(f, t)
    world = get_world_counter()
    match, _ = Core.Compiler._findsup(tt, nothing, world)
    if match === nothing
        me = MethodError(f, t, world)
        ee = ErrorException(sprint(io -> begin
            println(io, "Calling invoke(f, t, args...) would throw:");
            Base.showerror(io, me);
        end))
        throw(ee)
    end
    return match.method
end

"""
    which(types::Type{<:Tuple})

Returns the method that would be called by the given type signature (as a tuple type).
"""
function which(@nospecialize(tt#=::Type=#))
    return _which(tt).method
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

Get the name of a generic `Function` as a symbol. For anonymous functions,
this is a compiler-generated name. For explicitly-declared subtypes of
`Function`, it is the name of the function's type.
"""
function nameof(f::Function)
    t = typeof(f)
    mt = t.name.mt
    if mt === Symbol.name.mt
        # uses shared method table, so name is not unique to this function type
        return nameof(t)
    end
    return mt.name
end

function nameof(f::Core.IntrinsicFunction)
    name = ccall(:jl_intrinsic_name, Ptr{UInt8}, (Core.IntrinsicFunction,), f)
    return ccall(:jl_symbol, Ref{Symbol}, (Ptr{UInt8},), name)
end

"""
    parentmodule(f::Function) -> Module

Determine the module containing the (first) definition of a generic
function.
"""
parentmodule(f::Function) = parentmodule(typeof(f))

"""
    parentmodule(f::Function, types) -> Module

Determine the module containing the first method of a generic function `f` matching
the specified `types`.
"""
function parentmodule(@nospecialize(f), @nospecialize(types))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    return parentmodule(first(m))
end

"""
    parentmodule(m::Method) -> Module

Return the module in which the given method `m` is defined.

!!! compat "Julia 1.9"
    Passing a `Method` as an argument requires Julia 1.9 or later.
"""
parentmodule(m::Method) = m.module

"""
    hasmethod(f, t::Type{<:Tuple}[, kwnames]; world=get_world_counter()) -> Bool

Determine whether the given generic function has a method matching the given
`Tuple` of argument types with the upper bound of world age given by `world`.

If a tuple of keyword argument names `kwnames` is provided, this also checks
whether the method of `f` matching `t` has the given keyword argument names.
If the matching method accepts a variable number of keyword arguments, e.g.
with `kwargs...`, any names given in `kwnames` are considered valid. Otherwise
the provided names must be a subset of the method's keyword arguments.

See also [`applicable`](@ref).

!!! compat "Julia 1.2"
    Providing keyword argument names requires Julia 1.2 or later.

# Examples
```jldoctest
julia> hasmethod(length, Tuple{Array})
true

julia> f(; oranges=0) = oranges;

julia> hasmethod(f, Tuple{}, (:oranges,))
true

julia> hasmethod(f, Tuple{}, (:apples, :bananas))
false

julia> g(; xs...) = 4;

julia> hasmethod(g, Tuple{}, (:a, :b, :c, :d))  # g accepts arbitrary kwargs
true
```
"""
function hasmethod(@nospecialize(f), @nospecialize(t))
    return Core._hasmethod(f, t isa Type ? t : to_tuple_type(t))
end

function Core.kwcall(kwargs::NamedTuple, ::typeof(hasmethod), @nospecialize(f), @nospecialize(t))
    world = kwargs.world::UInt # make sure this is the only local, to avoid confusing kwarg_decl()
    return ccall(:jl_gf_invoke_lookup, Any, (Any, Any, UInt), signature_type(f, t), nothing, world) !== nothing
end

function hasmethod(f, t, kwnames::Tuple{Vararg{Symbol}}; world::UInt=get_world_counter())
    @nospecialize
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    isempty(kwnames) && return hasmethod(f, t; world)
    t = to_tuple_type(t)
    ft = Core.Typeof(f)
    u = unwrap_unionall(t)::DataType
    tt = rewrap_unionall(Tuple{typeof(Core.kwcall), NamedTuple, ft, u.parameters...}, t)
    match = ccall(:jl_gf_invoke_lookup, Any, (Any, Any, UInt), tt, nothing, world)
    match === nothing && return false
    kws = ccall(:jl_uncompress_argnames, Array{Symbol,1}, (Any,), (match::Method).slot_syms)
    isempty(kws) && return true # some kwfuncs simply forward everything directly
    for kw in kws
        endswith(String(kw), "...") && return true
    end
    kwnames = Symbol[kwnames[i] for i in 1:length(kwnames)]
    return issubset(kwnames, kws)
end

"""
    fbody = bodyfunction(basemethod::Method)

Find the keyword "body function" (the function that contains the body of the method
as written, called after all missing keyword-arguments have been assigned default values).
`basemethod` is the method you obtain via [`which`](@ref) or [`methods`](@ref).
"""
function bodyfunction(basemethod::Method)
    fmod = parentmodule(basemethod)
    # The lowered code for `basemethod` should look like
    #   %1 = mkw(kwvalues..., #self#, args...)
    #        return %1
    # where `mkw` is the name of the "active" keyword body-function.
    ast = uncompressed_ast(basemethod)
    if isa(ast, Core.CodeInfo) && length(ast.code) >= 2
        callexpr = ast.code[end-1]
        if isa(callexpr, Expr) && callexpr.head === :call
            fsym = callexpr.args[1]
            while true
                if isa(fsym, Symbol)
                    return getfield(fmod, fsym)
                elseif isa(fsym, GlobalRef)
                    if fsym.mod === Core && fsym.name === :_apply
                        fsym = callexpr.args[2]
                    elseif fsym.mod === Core && fsym.name === :_apply_iterate
                        fsym = callexpr.args[3]
                    end
                    if isa(fsym, Symbol)
                        return getfield(fmod, fsym)::Function
                    elseif isa(fsym, GlobalRef)
                        return getfield(fsym.mod, fsym.name)::Function
                    elseif isa(fsym, Core.SSAValue)
                        fsym = ast.code[fsym.id]
                    else
                        return nothing
                    end
                elseif isa(fsym, Core.SSAValue)
                    fsym = ast.code[fsym.id]
                else
                    return nothing
                end
            end
        end
    end
    return nothing
end

"""
    Base.isambiguous(m1, m2; ambiguous_bottom=false) -> Bool

Determine whether two methods `m1` and `m2` may be ambiguous for some call
signature. This test is performed in the context of other methods of the same
function; in isolation, `m1` and `m2` might be ambiguous, but if a third method
resolving the ambiguity has been defined, this returns `false`.
Alternatively, in isolation `m1` and `m2` might be ordered, but if a third
method cannot be sorted with them, they may cause an ambiguity together.

For parametric types, the `ambiguous_bottom` keyword argument controls whether
`Union{}` counts as an ambiguous intersection of type parameters – when `true`,
it is considered ambiguous, when `false` it is not.

# Examples
```jldoctest
julia> foo(x::Complex{<:Integer}) = 1
foo (generic function with 1 method)

julia> foo(x::Complex{<:Rational}) = 2
foo (generic function with 2 methods)

julia> m1, m2 = collect(methods(foo));

julia> typeintersect(m1.sig, m2.sig)
Tuple{typeof(foo), Complex{Union{}}}

julia> Base.isambiguous(m1, m2, ambiguous_bottom=true)
true

julia> Base.isambiguous(m1, m2, ambiguous_bottom=false)
false
```
"""
function isambiguous(m1::Method, m2::Method; ambiguous_bottom::Bool=false)
    m1 === m2 && return false
    ti = typeintersect(m1.sig, m2.sig)
    ti === Bottom && return false
    function inner(ti)
        ti === Bottom && return false
        if !ambiguous_bottom
            has_bottom_parameter(ti) && return false
        end
        world = get_world_counter()
        world == typemax(UInt) && return true # intersecting methods are always ambiguous in the generator world, which is true, albeit maybe confusing for some
        min = Ref{UInt}(typemin(UInt))
        max = Ref{UInt}(typemax(UInt))
        has_ambig = Ref{Int32}(0)
        ms = collect(Core.MethodMatch, _methods_by_ftype(ti, nothing, -1, world, true, min, max, has_ambig)::Vector)
        has_ambig[] == 0 && return false
        if !ambiguous_bottom
            filter!(ms) do m::Core.MethodMatch
                return !has_bottom_parameter(m.spec_types)
            end
        end
        # if ml-matches reported the existence of an ambiguity over their
        # intersection, see if both m1 and m2 seem to be involved in it
        # (if one was fully dominated by a different method, we want to will
        # report the other ambiguous pair)
        have_m1 = have_m2 = false
        for match in ms
            m = match.method
            m === m1 && (have_m1 = true)
            m === m2 && (have_m2 = true)
        end
        if !have_m1 || !have_m2
            # ml-matches did not need both methods to expose the reported ambiguity
            return false
        end
        if !ambiguous_bottom
            # since we're intentionally ignoring certain ambiguities (via the
            # filter call above), see if we can now declare the intersection fully
            # covered even though it is partially ambiguous over Union{} as a type
            # parameter somewhere
            minmax = nothing
            for match in ms
                m = match.method
                match.fully_covers || continue
                if minmax === nothing || morespecific(m, minmax)
                    minmax = m
                end
            end
            if minmax === nothing || minmax == m1 || minmax == m2
                return true
            end
            for match in ms
                m = match.method
                m === minmax && continue
                if !morespecific(minmax, m)
                    if match.fully_covers || !morespecific(m, minmax)
                        return true
                    end
                end
            end
            return false
        end
        return true
    end
    if !(ti <: m1.sig && ti <: m2.sig)
        # When type-intersection fails, it's often also not commutative. Thus
        # checking the reverse may allow detecting ambiguity solutions
        # correctly in more cases (and faster).
        ti2 = typeintersect(m2.sig, m1.sig)
        if ti2 <: m1.sig && ti2 <: m2.sig
            ti = ti2
        elseif ti != ti2
            # TODO: this would be the more correct way to handle this case, but
            #       people complained so we don't do it
            #inner(ti2) || return false # report that the type system failed to decide if it was ambiguous by saying they definitely are
            return false # report that the type system failed to decide if it was ambiguous by saying they definitely are not
        else
            return false # report that the type system failed to decide if it was ambiguous by saying they definitely are not
        end
    end
    inner(ti) || return false
    # otherwise type-intersection reported an ambiguity we couldn't solve
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
    @invoke f(arg::T, ...; kwargs...)

Provides a convenient way to call [`invoke`](@ref) by expanding
`@invoke f(arg1::T1, arg2::T2; kwargs...)` to `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)`.
When an argument's type annotation is omitted, it's replaced with `Core.Typeof` that argument.
To invoke a method where an argument is untyped or explicitly typed as `Any`, annotate the
argument with `::Any`.

It also supports the following syntax:
- `@invoke (x::X).f` expands to `invoke(getproperty, Tuple{X,Symbol}, x, :f)`
- `@invoke (x::X).f = v::V` expands to `invoke(setproperty!, Tuple{X,Symbol,V}, x, :f, v)`
- `@invoke (xs::Xs)[i::I]` expands to `invoke(getindex, Tuple{Xs,I}, xs, i)`
- `@invoke (xs::Xs)[i::I] = v::V` expands to `invoke(setindex!, Tuple{Xs,V,I}, xs, v, i)`

# Examples

```jldoctest
julia> @macroexpand @invoke f(x::T, y)
:(Core.invoke(f, Tuple{T, Core.Typeof(y)}, x, y))

julia> @invoke 420::Integer % Unsigned
0x00000000000001a4

julia> @macroexpand @invoke (x::X).f
:(Core.invoke(Base.getproperty, Tuple{X, Core.Typeof(:f)}, x, :f))

julia> @macroexpand @invoke (x::X).f = v::V
:(Core.invoke(Base.setproperty!, Tuple{X, Core.Typeof(:f), V}, x, :f, v))

julia> @macroexpand @invoke (xs::Xs)[i::I]
:(Core.invoke(Base.getindex, Tuple{Xs, I}, xs, i))

julia> @macroexpand @invoke (xs::Xs)[i::I] = v::V
:(Core.invoke(Base.setindex!, Tuple{Xs, V, I}, xs, v, i))
```

!!! compat "Julia 1.7"
    This macro requires Julia 1.7 or later.

!!! compat "Julia 1.9"
    This macro is exported as of Julia 1.9.

!!! compat "Julia 1.10"
    The additional syntax is supported as of Julia 1.10.
"""
macro invoke(ex)
    topmod = Core.Compiler._topmod(__module__) # well, except, do not get it via CC but define it locally
    f, args, kwargs = destructure_callex(topmod, ex)
    types = Expr(:curly, :Tuple)
    out = Expr(:call, GlobalRef(Core, :invoke))
    isempty(kwargs) || push!(out.args, Expr(:parameters, kwargs...))
    push!(out.args, f)
    push!(out.args, types)
    for arg in args
        if isexpr(arg, :(::))
            push!(out.args, arg.args[1])
            push!(types.args, arg.args[2])
        else
            push!(out.args, arg)
            push!(types.args, Expr(:call, GlobalRef(Core, :Typeof), arg))
        end
    end
    return esc(out)
end

"""
    @invokelatest f(args...; kwargs...)

Provides a convenient way to call [`invokelatest`](@ref).
`@invokelatest f(args...; kwargs...)` will simply be expanded into
`Base.invokelatest(f, args...; kwargs...)`.

It also supports the following syntax:
- `@invokelatest x.f` expands to `Base.invokelatest(getproperty, x, :f)`
- `@invokelatest x.f = v` expands to `Base.invokelatest(setproperty!, x, :f, v)`
- `@invokelatest xs[i]` expands to `Base.invokelatest(getindex, xs, i)`
- `@invokelatest xs[i] = v` expands to `Base.invokelatest(setindex!, xs, v, i)`

```jldoctest
julia> @macroexpand @invokelatest f(x; kw=kwv)
:(Base.invokelatest(f, x; kw = kwv))

julia> @macroexpand @invokelatest x.f
:(Base.invokelatest(Base.getproperty, x, :f))

julia> @macroexpand @invokelatest x.f = v
:(Base.invokelatest(Base.setproperty!, x, :f, v))

julia> @macroexpand @invokelatest xs[i]
:(Base.invokelatest(Base.getindex, xs, i))

julia> @macroexpand @invokelatest xs[i] = v
:(Base.invokelatest(Base.setindex!, xs, v, i))
```

!!! compat "Julia 1.7"
    This macro requires Julia 1.7 or later.

!!! compat "Julia 1.9"
    Prior to Julia 1.9, this macro was not exported, and was called as `Base.@invokelatest`.

!!! compat "Julia 1.10"
    The additional `x.f` and `xs[i]` syntax requires Julia 1.10.
"""
macro invokelatest(ex)
    topmod = Core.Compiler._topmod(__module__) # well, except, do not get it via CC but define it locally
    f, args, kwargs = destructure_callex(topmod, ex)
    out = Expr(:call, GlobalRef(Base, :invokelatest))
    isempty(kwargs) || push!(out.args, Expr(:parameters, kwargs...))
    push!(out.args, f)
    append!(out.args, args)
    return esc(out)
end

function destructure_callex(topmod::Module, @nospecialize(ex))
    function flatten(xs)
        out = Any[]
        for x in xs
            if isexpr(x, :tuple)
                append!(out, x.args)
            else
                push!(out, x)
            end
        end
        return out
    end

    kwargs = Any[]
    if isexpr(ex, :call) # `f(args...)`
        f = first(ex.args)
        args = Any[]
        for x in ex.args[2:end]
            if isexpr(x, :parameters)
                append!(kwargs, x.args)
            elseif isexpr(x, :kw)
                push!(kwargs, x)
            else
                push!(args, x)
            end
        end
    elseif isexpr(ex, :.)   # `x.f`
        f = GlobalRef(topmod, :getproperty)
        args = flatten(ex.args)
    elseif isexpr(ex, :ref) # `x[i]`
        f = GlobalRef(topmod, :getindex)
        args = flatten(ex.args)
    elseif isexpr(ex, :(=)) # `x.f = v` or `x[i] = v`
        lhs, rhs = ex.args
        if isexpr(lhs, :.)
            f = GlobalRef(topmod, :setproperty!)
            args = flatten(Any[lhs.args..., rhs])
        elseif isexpr(lhs, :ref)
            f = GlobalRef(topmod, :setindex!)
            args = flatten(Any[lhs.args[1], rhs, lhs.args[2]])
        else
            throw(ArgumentError("expected a `setproperty!` expression `x.f = v` or `setindex!` expression `x[i] = v`"))
        end
    else
        throw(ArgumentError("expected a `:call` expression `f(args...; kwargs...)`"))
    end
    return f, args, kwargs
end

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
