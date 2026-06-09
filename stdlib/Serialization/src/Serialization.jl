# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Provide serialization of Julia objects via the functions
* [`serialize`](@ref)
* [`deserialize`](@ref)
"""
module Serialization

import Base: Bottom, unsafe_convert
import Base.ScopedValues: ScopedValue, with
import Core: svec, SimpleVector
using Base: @assume_effects, unaliascopy, unwrap_unionall, require_one_based_indexing, ntupleany
using Core.IR

export serialize, deserialize, AbstractSerializer, Serializer

abstract type AbstractSerializer end

# Dict is generally a much better dictionary than IdDict, but we want objectid comparison
struct IdKey val::Any end
Base.hash(k::IdKey, h::UInt) = hash(objectid(k.val), h)
Base.isequal(a::IdKey, b::IdKey) = a.val === b.val

mutable struct Serializer{I<:IO} <: AbstractSerializer
    const io::I
    counter::Int
    const cycle_table::Dict{IdKey,Int}
    const backref_table::Vector{Any}
    const pending_refs::Vector{Int}
    const known_object_data::Dict{UInt64,Any}
    version::Int
    Serializer{I}(io::I) where I<:IO = new(io, 0, Dict{IdKey,Int}(), Any[], Int[], Dict{UInt64,Any}(), ser_version)
end

Serializer(io::IO) = Serializer{typeof(io)}(io)

const current_module = ScopedValue{Union{Nothing,Module}}(nothing)

## serializing values ##

const n_int_literals = 33
const n_reserved_slots = 24
const n_reserved_tags = 8

const TAGS = Any[
    Symbol, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128,
    Float16, Float32, Float64, Char, DataType, Union, UnionAll, Core.TypeName, Tuple,
    Array, Expr, LineNumberNode, :__LabelNode__, GotoNode, QuoteNode, CodeInfo, TypeVar,
    Core.Box, Core.MethodInstance, Module, Task, String, SimpleVector, Method,
    GlobalRef, SlotNumber, Const, NewvarNode, SSAValue,

    # dummy entries for tags that don't correspond directly to types
    Symbol, # UNDEFREF_TAG
    Symbol, # BACKREF_TAG
    Symbol, # LONGBACKREF_TAG
    Symbol, # SHORTBACKREF_TAG
    Symbol, # LONGTUPLE_TAG
    Symbol, # LONGSYMBOL_TAG
    Symbol, # LONGEXPR_TAG
    Symbol, # LONGSTRING_TAG
    Symbol, # SHORTINT64_TAG
    Symbol, # FULL_DATATYPE_TAG
    Symbol, # WRAPPER_DATATYPE_TAG
    Symbol, # OBJECT_TAG
    Symbol, # REF_OBJECT_TAG
    Symbol, # FULL_GLOBALREF_TAG
    Symbol, # HEADER_TAG
    Symbol, # IDDICT_TAG
    Symbol, # SHARED_REF_TAG
    ReturnNode, GotoIfNot,
    fill(Symbol, n_reserved_tags)...,

    (), Bool, Any, Bottom, Core.TypeofBottom, Type, svec(), Tuple{}, false, true, nothing,
    :Any, :Array, :TypeVar, :Box, :Tuple, :Ptr, :return, :call, Symbol("::"), :Function,
    :(=), :(==), :(===), :gotoifnot, :A, :B, :C, :M, :N, :T, :S, :X, :Y, :a, :b, :c, :d, :e, :f,
    :g, :h, :i, :j, :k, :l, :m, :n, :o, :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z, :add_int,
    :sub_int, :mul_int, :add_float, :sub_float, :new, :mul_float, :bitcast, :start, :done, :next,
    :indexed_iterate, :getfield, :meta, :eq_int, :slt_int, :sle_int, :ne_int, :push_loc, :pop_loc,
    :pop, :arrayset, :arrayref, :apply_type, :inbounds, :getindex, :setindex!, :Core, :!, :+,
    :Base, :static_parameter, :convert, :colon, Symbol("#self#"), Symbol("#temp#"), :tuple, Symbol(""),

    fill(:_reserved_, n_reserved_slots)...,

    (Int32(0):Int32(n_int_literals-1))...,
    (Int64(0):Int64(n_int_literals-1))...
]

const NTAGS = length(TAGS)
@assert NTAGS == 255

const ser_version = 30 # do not make changes without bumping the version #!

format_version(::AbstractSerializer) = ser_version
format_version(s::Serializer) = s.version

# static lookup table of serializee value --> TAG index via objectid + linear probe
# omit constant ints & reserved slots from the table as sertag doesn't return these
const NSERTAG_KEYS = NTAGS - n_reserved_slots - 2*n_int_literals

# keeps >50% sparse so linear probes hit in 1-2 steps. also allows :terminates
const SERTAG_TABLE_SIZE = nextpow(2, 2 * NSERTAG_KEYS)

struct SertagEmpty end
const sertag_empty = SertagEmpty()

struct SertagTable
    keys::Memory{Any}
    vals::Memory{Int32}
end

const sertag_table = let
    keys = Memory{Any}(undef, SERTAG_TABLE_SIZE)
    vals = Memory{Int32}(undef, SERTAG_TABLE_SIZE)
    fill!(keys, sertag_empty)
    @assume_effects :terminates_locally :noub @inbounds for i in Iterators.reverse(1:NSERTAG_KEYS)
        key = TAGS[i]
        loc = mod1(objectid(key), SERTAG_TABLE_SIZE)
        while true
            k = keys[loc]
            if k === sertag_empty || k === key
                keys[loc] = key
                vals[loc] = Int32(i)
                break
            end
            loc = mod1(loc + 1, SERTAG_TABLE_SIZE)
        end
    end
    SertagTable(keys, vals)
end

@inline function sertag(@nospecialize(v))
    (; keys, vals) = sertag_table
    loc = mod1(objectid(v), SERTAG_TABLE_SIZE)
    @assume_effects :terminates_locally :noub @inbounds while true
        @inbounds k = keys[loc]
        if k === v
            return vals[loc]
        elseif k === sertag_empty
            return Int32(-1)
        else
            loc = mod1(loc + 1, SERTAG_TABLE_SIZE)
        end
    end
end
desertag(i::Int32) = @inbounds(TAGS[i])

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = sertag(())
const ZERO32_TAG = Int32(NTAGS-(2*n_int_literals-1))
const ZERO64_TAG = Int64(NTAGS-(n_int_literals-1))
const TRUE_TAG = sertag(true)
const FALSE_TAG = sertag(false)
const EMPTYTUPLE_TAG = sertag(())
const TUPLE_TAG = sertag(Tuple)
const SIMPLEVECTOR_TAG = sertag(SimpleVector)
const SYMBOL_TAG = sertag(Symbol)
const INT8_TAG = sertag(Int8)
const ARRAY_TAG = sertag(Array)
const EXPR_TAG = sertag(Expr)
const MODULE_TAG = sertag(Module)
const METHODINSTANCE_TAG = sertag(Core.MethodInstance)
const METHOD_TAG = sertag(Method)
const TASK_TAG = sertag(Task)
const DATATYPE_TAG = sertag(DataType)
const TYPENAME_TAG = sertag(Core.TypeName)
const INT32_TAG = sertag(Int32)
const INT64_TAG = sertag(Int64)
const GLOBALREF_TAG = sertag(GlobalRef)
const BOTTOM_TAG = sertag(Bottom)
const UNIONALL_TAG = sertag(UnionAll)
const STRING_TAG = sertag(String)
const o0 = sertag(SSAValue)
const UNDEFREF_TAG         = Int32(o0+1)
const BACKREF_TAG          = Int32(o0+2)
const LONGBACKREF_TAG      = Int32(o0+3)
const SHORTBACKREF_TAG     = Int32(o0+4)
const LONGTUPLE_TAG        = Int32(o0+5)
const LONGSYMBOL_TAG       = Int32(o0+6)
const LONGEXPR_TAG         = Int32(o0+7)
const LONGSTRING_TAG       = Int32(o0+8)
const SHORTINT64_TAG       = Int32(o0+9)
const FULL_DATATYPE_TAG    = Int32(o0+10)
const WRAPPER_DATATYPE_TAG = Int32(o0+11)
const OBJECT_TAG           = Int32(o0+12)
const REF_OBJECT_TAG       = Int32(o0+13)
const FULL_GLOBALREF_TAG   = Int32(o0+14)
const HEADER_TAG           = Int32(o0+15)
const IDDICT_TAG           = Int32(o0+16)
const SHARED_REF_TAG       = Int32(o0+17)

writetag(s::IO, tag) = (write(s, UInt8(tag)); nothing)

function write_as_tag(s::IO, tag)
    tag < VALUE_TAGS && write(s, UInt8(0))
    write(s, UInt8(tag))
    nothing
end

# cycle handling
_getcycle(s::AbstractSerializer, @nospecialize(x)) = get(s.table, x, -1)::Int
_getcycle(s::Serializer, @nospecialize(x)) = get(s.cycle_table, IdKey(x), -1)
_setcycle!(s::AbstractSerializer, @nospecialize(x), v::Int) = (s.table[x] = v; nothing)
_setcycle!(s::Serializer, @nospecialize(x), v::Int) = (s.cycle_table[IdKey(x)] = v; nothing)

_setbackref!(s::AbstractSerializer, slot::Int, @nospecialize(x)) = (s.table[slot] = x; nothing)
function _setbackref!(s::Serializer, slot::Int, @nospecialize(x))
    bt = s.backref_table
    i = slot + 1
    i > length(bt) && resize!(bt, max(i, 2 * length(bt) + 1))
    @inbounds bt[i] = x
    nothing
end

@noinline function __getbackref_error(id::Int)
    error("""Inconsistent Serializer state when deserializing.
            Attempt to access internal table with key $id failed.

            This might occur if the Serializer contexts when serializing and deserializing are inconsistent.
            In particular, if multiple serialize calls use the same Serializer object then
            the corresponding deserialize calls should also use the same Serializer object.
        """)
end

_getbackref(s::AbstractSerializer, id::Int) = get(() -> __getbackref_error(id), s.table, id)
function _getbackref(s::Serializer, id::Int)
    bt = s.backref_table
    i = id + 1
    (id < 0 || i > length(bt) || !isassigned(bt, i)) && __getbackref_error(id)
    @inbounds return bt[i]
end

_sizehint_cycle!(s::AbstractSerializer, n::Integer) = (sizehint!(s.table, n); nothing)
_sizehint_cycle!(s::Serializer, n::Integer) = (sizehint!(s.cycle_table, n); nothing)

_sizehint_backref!(s::AbstractSerializer, n::Integer) = (sizehint!(s.table, n); nothing)
_sizehint_backref!(s::Serializer, n::Integer) = (sizehint!(s.backref_table, n; shrink=false); nothing)

function _emit_backref(io::IO, offs::Int)
    if offs <= typemax(UInt16)
        writetag(io, SHORTBACKREF_TAG)
        write(io, UInt16(offs))
    elseif offs <= typemax(Int32)
        writetag(io, BACKREF_TAG)
        write(io, Int32(offs))
    else
        writetag(io, LONGBACKREF_TAG)
        write(io, Int64(offs))
    end
    nothing
end

function serialize_cycle(s::AbstractSerializer, @nospecialize(x))
    offs = _getcycle(s, x)
    if offs != -1
        _emit_backref(s.io, offs)
        return true
    end
    _setcycle!(s, x, s.counter)
    s.counter += 1
    return false
end

function serialize_cycle_header(s::AbstractSerializer, @nospecialize(x))
    serialize_cycle(s, x) && return true
    serialize_type(s, typeof(x), true)
    return false
end

function reset_state(s::AbstractSerializer)
    s.counter = 0
    empty!(s.table)
    empty!(s.pending_refs)
    s
end
function reset_state(s::Serializer)
    s.counter = 0
    empty!(s.cycle_table)
    empty!(s.backref_table)
    empty!(s.pending_refs)
    s
end

serialize(s::AbstractSerializer, x::Bool) = x ? writetag(s.io, TRUE_TAG) :
                                                writetag(s.io, FALSE_TAG)

serialize(s::AbstractSerializer, p::Ptr) = serialize_any(s, oftype(p, C_NULL))

serialize(s::AbstractSerializer, ::Tuple{}) = writetag(s.io, EMPTYTUPLE_TAG)

function serialize(s::AbstractSerializer, t::Tuple)
    l = nfields(t)
    short = l <= NTAGS
    if @generated
        exprs = Vector{Expr}(undef, l + 2)
        exprs[1] = :(writetag(s.io, $(short ? TUPLE_TAG : LONGTUPLE_TAG)))
        exprs[2] = :(write(s.io, $(short ? UInt8 : Int32)($l)))
        for i in 1:l
            exprs[2+i] = :(serialize(s, t[$i]))
        end
        return Expr(:block, exprs...)
    else
        writetag(s.io, short ? TUPLE_TAG : LONGTUPLE_TAG)
        write(s.io, (short ? UInt8 : Int32)(l))
        for i in 1:l
            serialize(s, t[i])
        end
    end
end

function serialize(s::AbstractSerializer, v::SimpleVector)
    writetag(s.io, SIMPLEVECTOR_TAG)
    write(s.io, Int32(length(v)))
    for x in v
        serialize(s, x)
    end
end

function serialize(s::AbstractSerializer, x::Symbol)
    tag = sertag(x)
    if tag > 0
        return write_as_tag(s.io, tag)
    end
    pname = unsafe_convert(Ptr{UInt8}, x)
    len = Int(ccall(:strlen, Csize_t, (Cstring,), pname))
    if len > 7
        serialize_cycle(s, x) && return
    end
    if len <= NTAGS
        writetag(s.io, SYMBOL_TAG)
        write(s.io, UInt8(len))
    else
        writetag(s.io, LONGSYMBOL_TAG)
        write(s.io, Int32(len))
    end
    unsafe_write(s.io, pname, len)
    nothing
end

function serialize_array_data(s::IO, a)
    require_one_based_indexing(a)
    isempty(a) && return 0
    if eltype(a) === Bool
        last = a[1]::Bool
        count = 1
        for i = 2:length(a)
            if a[i]::Bool != last || count == 127
                write(s, UInt8((UInt8(last) << 7) | count))
                last = a[i]::Bool
                count = 1
            else
                count += 1
            end
        end
        write(s, UInt8((UInt8(last) << 7) | count))
    else
        write(s, a)
    end
end

function _serialize_non_bits_elements!(s::AbstractSerializer, a)
    _sizehint_cycle!(s, div(length(a), 4))  # prepare for lots of pointers
    @inbounds for i in eachindex(a)
        if isassigned(a, i)
            serialize(s, a[i])
        else
            writetag(s.io, UNDEFREF_TAG)
        end
    end
end

function serialize(s::AbstractSerializer, a::Array)
    serialize_cycle(s, a) && return
    elty = eltype(a)
    writetag(s.io, ARRAY_TAG)
    if elty !== UInt8
        serialize_datatype(s, elty)
    end
    if ndims(a) != 1
        serialize(s, size(a))
    else
        serialize(s, length(a))
    end
    if isbitstype(elty)
        serialize_array_data(s.io, a)
    else
        _serialize_non_bits_elements!(s, a)
    end
end

function serialize(s::AbstractSerializer, a::SubArray{T,N,A}) where {T,N,A<:Array}
    # SubArray's copy only selects the relevant data (and reduces the size) but does not
    # preserve the type of the argument. This internal function does both:
    b = unaliascopy(a)
    serialize_any(s, b)
end

serialize(s::AbstractSerializer, m::GenericMemory) = error("GenericMemory{:atomic} currently cannot be serialized")
function serialize(s::AbstractSerializer, m::Memory)
    serialize_cycle_header(s, m) && return
    serialize(s, length(m))
    elty = eltype(m)
    if isbitstype(elty)
        serialize_array_data(s.io, m)
    else
        _serialize_non_bits_elements!(s, m)
    end
end

function serialize(s::AbstractSerializer, x::GenericMemoryRef)
    serialize_type(s, typeof(x))
    serialize(s, getfield(x, :mem))
    serialize(s, Base.memoryrefoffset(x))
end

function serialize(s::AbstractSerializer, ss::String)
    len = sizeof(ss)
    if len > 7
        serialize_cycle(s, ss) && return
        writetag(s.io, SHARED_REF_TAG)
    end
    if len <= NTAGS
        writetag(s.io, STRING_TAG)
        write(s.io, UInt8(len))
    else
        writetag(s.io, LONGSTRING_TAG)
        write(s.io, Int64(len))
    end
    write(s.io, ss)
    nothing
end

function serialize(s::AbstractSerializer, ss::SubString{String})
    # avoid saving a copy of the parent string, keeping the type of ss
    serialize_any(s, SubString(String(ss)))
end

# Don't serialize the pointers
function serialize(s::AbstractSerializer, r::Regex)
    serialize_type(s, typeof(r))
    serialize(s, r.pattern)
    serialize(s, r.compile_options)
    serialize(s, r.match_options)
end

const BIGINT_BASE = 62  # max base accepted by `string`/`parse` (0-9A-Za-z)

function serialize(s::AbstractSerializer, n::BigInt)
    serialize_type(s, BigInt)
    serialize(s, string(n, base = BIGINT_BASE))
end

function serialize(s::AbstractSerializer, ex::Expr)
    serialize_cycle(s, ex) && return
    l = length(ex.args)
    if l <= NTAGS
        writetag(s.io, EXPR_TAG)
        write(s.io, UInt8(l))
    else
        writetag(s.io, LONGEXPR_TAG)
        write(s.io, Int32(l))
    end
    serialize(s, ex.head)
    for a in ex.args
        serialize(s, a)
    end
end

function serialize_dict_data(s::AbstractSerializer, d::AbstractDict)
    write(s.io, Int32(length(d)))
    for (k,v) in d
        serialize(s, k)
        serialize(s, v)
    end
end

function serialize(s::AbstractSerializer, d::Dict)
    serialize_cycle_header(s, d) && return
    serialize_dict_data(s, d)
end

function serialize(s::AbstractSerializer, d::IdDict)
    serialize_cycle(s, d) && return
    writetag(s.io, IDDICT_TAG)
    serialize_type_data(s, typeof(d))
    serialize_dict_data(s, d)
end

function serialize_mod_names(s::AbstractSerializer, m::Module)
    p = parentmodule(m)
    if p === m || m === Base
        key = Base.root_module_key(m)
        uuid = key.uuid
        serialize(s, uuid === nothing ? nothing : uuid.value)
        serialize(s, Symbol(key.name))
    else
        serialize_mod_names(s, p)
        serialize(s, nameof(m))
    end
end

function serialize(s::AbstractSerializer, m::Module)
    writetag(s.io, MODULE_TAG)
    serialize_mod_names(s, m)
    writetag(s.io, EMPTYTUPLE_TAG)
end

# In principle, use the `AbstractSerializer`'s counter to assign unique object numbers.
# However, as the counter as already be incremented, actually use the copy in `table.l`
# as the result is more intuitive.
object_number(s::AbstractSerializer, l) = s.table[l] % UInt64

lookup_object_number(s::AbstractSerializer, n::UInt64) = nothing

remember_object(s::AbstractSerializer, @nospecialize(o), n::UInt64) = nothing

function lookup_object_number(s::Serializer, n::UInt64)
    return get(s.known_object_data, n, nothing)
end

function remember_object(s::Serializer, @nospecialize(o), n::UInt64)
    s.known_object_data[n] = o
    return nothing
end

function serialize(s::AbstractSerializer, meth::Method)
    serialize_cycle(s, meth) && return
    writetag(s.io, METHOD_TAG)
    write(s.io, object_number(s, meth))
    serialize(s, meth.module)
    serialize(s, meth.name)
    serialize(s, meth.file)
    serialize(s, meth.line)
    serialize(s, meth.sig)
    serialize(s, meth.slot_syms)
    serialize(s, meth.nargs)
    serialize(s, meth.isva)
    serialize(s, meth.is_for_opaque_closure)
    serialize(s, meth.nospecializeinfer)
    serialize(s, meth.constprop)
    serialize(s, meth.purity)
    if isdefined(meth, :source)
        serialize(s, Base._uncompressed_ast(meth))
    else
        serialize(s, nothing)
    end
    if isdefined(meth, :generator)
        serialize(s, meth.generator)
    else
        serialize(s, nothing)
    end
    if isdefined(meth, :recursion_relation)
        serialize(s, meth.recursion_relation)
    else
        serialize(s, nothing)
    end
    if isdefined(meth, :external_mt)
        error("cannot serialize Method objects with external method tables")
    end
    nothing
end

function serialize(s::AbstractSerializer, mt::Core.MethodTable)
    serialize_type(s, typeof(mt))
    serialize(s, mt.name)
    serialize(s, mt.module)
    nothing
end

function serialize(s::AbstractSerializer, mc::Core.MethodCache)
    error("cannot serialize MethodCache objects")
end


function serialize(s::AbstractSerializer, linfo::Core.MethodInstance)
    serialize_cycle(s, linfo) && return
    writetag(s.io, METHODINSTANCE_TAG)
    serialize(s, nothing)  # for backwards compat
    serialize(s, linfo.sparam_vals)
    serialize(s, Any)  # for backwards compat
    serialize(s, linfo.specTypes)
    serialize(s, linfo.def)
    nothing
end

function serialize(s::AbstractSerializer, @nospecialize(u::Union))
    serialize_type(s, Union, false)
    serialize(s, u.a)
    serialize(s, u.b)
end

function serialize(s::AbstractSerializer, t::Task)
    serialize_cycle(s, t) && return
    if istaskstarted(t) && !istaskdone(t)
        error("cannot serialize a running Task")
    end
    writetag(s.io, TASK_TAG)
    serialize(s, t.code)
    serialize(s, t.storage)
    serialize(s, t.state)
    if t._isexception && (stk = Base.current_exceptions(t); !isempty(stk))
        # the exception stack field is hidden inside the task, so if there
        # is any information there make a CapturedException from it instead.
        # TODO: Handle full exception chain, not just the first one.
        serialize(s, CapturedException(stk[1].exception, stk[1].backtrace))
    else
        serialize(s, t.result)
    end
    serialize(s, t._isexception)
end

function serialize(s::AbstractSerializer, g::GlobalRef)
    if (g.mod === __deserialized_types__ ) ||
        (g.mod === Main && isdefined(g.mod, g.name) && isconst(g.mod, g.name))

        v = getglobal(g.mod, g.name)
        unw = unwrap_unionall(v)
        if isa(unw,DataType) && v === unw.name.wrapper && should_send_whole_type(s, unw)
            # handle references to types in Main by sending the whole type.
            # needed to be able to send nested functions (#15451).
            writetag(s.io, FULL_GLOBALREF_TAG)
            serialize(s, v)
            return
        end
    end
    writetag(s.io, GLOBALREF_TAG)
    serialize(s, g.mod)
    serialize(s, g.name)
end

function serialize(s::AbstractSerializer, t::Core.TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)
    write(s.io, object_number(s, t))
    serialize_typename(s, t)
end

function serialize_typename(s::AbstractSerializer, t::Core.TypeName)
    serialize(s, t.name)
    serialize(s, t.names)
    primary = unwrap_unionall(t.wrapper)
    serialize(s, primary.super)
    serialize(s, primary.parameters)
    serialize(s, primary.types)
    serialize(s, Base.issingletontype(primary))
    serialize(s, t.flags & 0x1 == 0x1) # .abstract
    serialize(s, t.flags & 0x2 == 0x2) # .mutable
    serialize(s, Int32(length(primary.types) - t.n_uninitialized))
    serialize(s, t.max_methods)
    ms = Base.matches_to_methods(Base._methods_by_ftype(Tuple{t.wrapper, Vararg}, -1, Base.get_world_counter()), t, nothing).ms
    if t.singletonname !== t.name || !isempty(ms)
        serialize(s, t.singletonname)
        serialize(s, ms)
        serialize(s, t.max_args)
        kws = Base.matches_to_methods(Base._methods_by_ftype(Tuple{typeof(Core.kwcall), Any, t.wrapper, Vararg}, -1, Base.get_world_counter()), t, nothing).ms
        if isempty(kws)
            writetag(s.io, UNDEFREF_TAG)
        else
            serialize(s, kws)
        end
    else
        writetag(s.io, UNDEFREF_TAG)
    end
    nothing
end

# decide whether to send all data for a type (instead of just its name)
function should_send_whole_type(s, t::DataType)
    tn = t.name
    # TODO improve somehow?
    # send whole type for anonymous functions in Main
    name = tn.singletonname
    mod = tn.module
    mod === __deserialized_types__ && return true
    isanonfunction = mod === Main && # only Main
        t.super === Function && # only Functions
        unsafe_load(unsafe_convert(Ptr{UInt8}, tn.name)) == UInt8('#') && # hidden type
        (!isdefined(mod, name) || t != typeof(getglobal(mod, name))) # XXX: 95% accurate test for this being an inner function
        # TODO: more accurate test? (tn.name !== "#" name)
    return isanonfunction
end

function serialize_type_data_dynamic(s, @nospecialize(t::DataType))
    whole = should_send_whole_type(s, t)
    iswrapper = (t === unwrap_unionall(t.name.wrapper))
    if whole && iswrapper
        writetag(s.io, WRAPPER_DATATYPE_TAG)
        serialize(s, t.name)
        return
    end
    serialize_cycle(s, t) && return
    if whole
        writetag(s.io, FULL_DATATYPE_TAG)
        serialize(s, t.name)
    else
        writetag(s.io, DATATYPE_TAG)
        serialize(s, nameof(t))
        serialize(s, parentmodule(t))
    end
    if !isempty(t.parameters)
        if iswrapper
            write(s.io, Int32(0))
        else
            write(s.io, Int32(length(t.parameters)))
            for p in t.parameters
                serialize(s, p)
            end
        end
    end
    nothing
end

# Serialize a type-valued DataType parameter statically, recursing through
# `Union` so that trimming does not need to see the reflective
# `serialize_fields(::Union)` fallback. Any non-`Union` case falls back to the
# ordinary `serialize` dispatch (which for `DataType`/`UnionAll`/`Bottom` is
# already statically resolvable from the compile-time type argument).
function serialize_type_param(s::AbstractSerializer, ::Type{T}) where T
    if @generated
        if T isa Union
            a = T.a
            b = T.b
            return quote
                writetag(s.io, $(sertag(Union)))
                serialize_type_param(s, $a)
                serialize_type_param(s, $b)
            end
        elseif T isa DataType
            return :(serialize_datatype(s, $T))
        else
            return :(serialize(s, T))
        end
    else
        serialize(s, T)
    end
end

function serialize_type_data(s, t::Type{T}) where T
    if @generated
        # When called with a DataType containing free TypeVars (e.g. Foo{T} from
        # a UnionAll body), Julia's type dispatch binds T to the TypeVar itself
        # rather than the DataType. Fall back to the runtime path in that case.
        T isa DataType || return :(serialize_type_data_dynamic(s, t))
        should_send_whole_type(nothing, T) && return :(serialize_type_data_dynamic(s, $T))
        iswrapper = T === unwrap_unionall(T.name.wrapper)
        name_sym = QuoteNode(nameof(T))
        mod = parentmodule(T)
        params = T.parameters
        exprs = Expr[]
        push!(exprs, :(serialize_cycle(s, $T) && return))
        push!(exprs, :(writetag(s.io, DATATYPE_TAG)))
        push!(exprs, :(serialize(s, $name_sym)))
        push!(exprs, :(serialize(s, $mod)))
        if !isempty(params)
            np = iswrapper ? 0 : length(params)
            push!(exprs, :(write(s.io, Int32($np))))
            if !iswrapper
                for p in params
                    if p isa DataType
                        push!(exprs, :(serialize_datatype(s, $p)))
                    elseif p isa Type
                        # Union (and similar) type parameters: recurse
                        # statically so trim does not see the reflective
                        # `serialize_fields`/`getfield(::Type, N)::Any` path.
                        push!(exprs, :(serialize_type_param(s, $p)))
                    else
                        push!(exprs, :(serialize(s, $(QuoteNode(p)))))
                    end
                end
            end
        end
        return quote $(exprs...) end
    else
        serialize_type_data_dynamic(s, t)
    end
end

function serialize(s::AbstractSerializer, t::DataType)
    tag = sertag(t)
    tag > 0 && return write_as_tag(s.io, tag)
    if t === Tuple
        # `sertag` is not able to find types === to `Tuple` because they
        # will not have been hash-consed. Plus `serialize_type_data` does not
        # handle this case correctly, since Tuple{} != Tuple. `Tuple` is the
        # only type with this property. issue #15849
        return write_as_tag(s.io, TUPLE_TAG)
    end
    serialize_type_data(s, t)
end

# Resolves tag checks at compile time when the type is statically known
# (e.g. Array element type serialization).
function serialize_datatype(s::AbstractSerializer, ::Type{T}) where T
    if @generated
        T isa DataType || return :(serialize(s, $T))
        tag = sertag(T)
        tag > 0 && return :(write_as_tag(s.io, $tag))
        T === Tuple && return :(write_as_tag(s.io, TUPLE_TAG))
        return :(serialize_type_data(s, $T))
    else
        serialize(s, T)
    end
end

function serialize_type(s::AbstractSerializer, ::Type{T}, ref::Bool = false) where T
    tag = sertag(T)
    tag > 0 && return writetag(s.io, tag)
    writetag(s.io, ref ? REF_OBJECT_TAG : OBJECT_TAG)
    serialize_type_data(s, T)
end

function serialize(s::AbstractSerializer, n::Int32)
    if 0 <= n <= (n_int_literals-1)
        write(s.io, UInt8(ZERO32_TAG+n))
    else
        writetag(s.io, INT32_TAG)
        write(s.io, n)
    end
    nothing
end

function serialize(s::AbstractSerializer, n::Int64)
    if 0 <= n <= (n_int_literals-1)
        write(s.io, UInt8(ZERO64_TAG+n))
    elseif typemin(Int32) <= n <= typemax(Int32)
        writetag(s.io, SHORTINT64_TAG)
        write(s.io, Int32(n))
    else
        writetag(s.io, INT64_TAG)
        write(s.io, n)
    end
    nothing
end

for i in 0:13
    tag = Int32(INT8_TAG + i)
    ty = TAGS[tag]
    (ty === Int32 || ty === Int64) && continue
    @eval serialize(s::AbstractSerializer, n::$ty) = (writetag(s.io, $tag); write(s.io, n); nothing)
end

serialize(s::AbstractSerializer, ::Type{Bottom}) = write_as_tag(s.io, BOTTOM_TAG)

function serialize(s::AbstractSerializer, u::UnionAll)
    writetag(s.io, UNIONALL_TAG)
    n = 0; t = u
    while isa(t, UnionAll)
        t = t.body
        n += 1
    end
    if isa(t, DataType) && t === unwrap_unionall(t.name.wrapper)
        write(s.io, UInt8(1))
        write(s.io, Int16(n))
        serialize(s, t)
    else
        write(s.io, UInt8(0))
        serialize(s, u.var)
        serialize(s, u.body)
    end
end

serialize(s::AbstractSerializer, x) = serialize_any(s, x)

function serialize(s::AbstractSerializer, x::Core.AddrSpace)
    serialize_type(s, typeof(x))
    write(s.io, Core.bitcast(UInt8, x))
end

function serialize(s::AbstractSerializer, x::Core.IntrinsicFunction)
    serialize_type(s, typeof(x))
    serialize(s, nameof(x))
end

function serialize_any(s::AbstractSerializer, x::T) where T
    if @generated
        exprs = Expr[]
        push!(exprs, :(tag = sertag(x)))
        push!(exprs, :(tag > 0 && return write_as_tag(s.io, tag)))
        if isprimitivetype(T)
            push!(exprs, :(serialize_type(s, $T)))
            push!(exprs, :(write(s.io, x)))
        elseif ismutabletype(T)
            push!(exprs, :(serialize_cycle(s, x) && return))
            push!(exprs, :(serialize_type(s, $T, true)))
            push!(exprs, :(serialize_fields(s, x)))
        else
            push!(exprs, :(serialize_type(s, $T, false)))
            push!(exprs, :(serialize_fields(s, x)))
        end
        push!(exprs, :(return nothing))
        return quote $(exprs...) end
    else
        tag = sertag(x)
        tag > 0 && return write_as_tag(s.io, tag)
        if isprimitivetype(T)
            serialize_type(s, T)
            write(s.io, x)
        elseif ismutabletype(T)
            serialize_cycle(s, x) && return
            serialize_type(s, T, true)
            serialize_fields(s, x)
        else
            serialize_type(s, T, false)
            serialize_fields(s, x)
        end
        return nothing
    end
end

function serialize_fields(s::AbstractSerializer, x::T) where T
    if @generated
        nf = fieldcount(T)
        exprs = Vector{Expr}(undef, nf)
        for i in 1:nf
            exprs[i] = quote
                if isdefined(x, $i)
                    serialize(s, getfield(x, $i))
                else
                    writetag(s.io, UNDEFREF_TAG)
                end
            end
        end
        return Expr(:block, exprs...)
    else
        for i in 1:nfields(x)
            if isdefined(x, i)
                serialize(s, getfield(x, i))
            else
                writetag(s.io, UNDEFREF_TAG)
            end
        end
    end
end

"""
    Serialization.writeheader(s::AbstractSerializer)

Write an identifying header to the specified serializer. The header consists of
8 bytes as follows:

| Offset | Description                                     |
|:-------|:------------------------------------------------|
|   0    | tag byte (0x37)                                 |
|   1-2  | signature bytes "JL"                            |
|   3    | protocol version                                |
|   4    | bits 0-1: endianness: 0 = little, 1 = big       |
|   4    | bits 2-3: platform: 0 = 32-bit, 1 = 64-bit      |
|   5-7  | reserved                                        |
"""
function writeheader(s::AbstractSerializer)
    io = s.io
    writetag(io, HEADER_TAG)
    write(io, "JL")  # magic bytes
    write(io, UInt8(ser_version))
    endianness = (ENDIAN_BOM == 0x04030201 ? 0 :
                  ENDIAN_BOM == 0x01020304 ? 1 :
                  error("unsupported endianness in serializer"))
    machine = (sizeof(Int) == 4 ? 0 :
               sizeof(Int) == 8 ? 1 :
               error("unsupported word size in serializer"))
    write(io, UInt8(endianness) | (UInt8(machine) << 2))
    write(io, [0x00,0x00,0x00]) # 3 reserved bytes
    nothing
end

function readheader(s::AbstractSerializer)
    # Tag already read
    io = s.io
    m1 = read(io, UInt8)
    m2 = read(io, UInt8)
    if m1 != UInt8('J') || m2 != UInt8('L')
        error("Unsupported serialization format (got header magic bytes $m1 $m2)")
    end
    version    = read(io, UInt8)
    flags      = read(io, UInt8)
    reserved1  = read(io, UInt8)
    reserved2  = read(io, UInt8)
    reserved3  = read(io, UInt8)
    endianflag = flags & 0x3
    wordflag   = (flags >> 2) & 0x3
    wordsize = wordflag == 0 ? 4 :
               wordflag == 1 ? 8 :
               error("Unknown word size flag in header")
    endian_bom = endianflag == 0 ? 0x04030201 :
                 endianflag == 1 ? 0x01020304 :
                 error("Unknown endianness flag in header")
    # Check protocol compatibility.
    endian_bom == ENDIAN_BOM  || error("Serialized byte order mismatch ($(repr(endian_bom)))")
    # We don't check wordsize == sizeof(Int) here, as Int is encoded concretely
    # as Int32 or Int64, which should be enough to correctly deserialize a range
    # of data structures between Julia versions.
    if version > ser_version
        error("""Cannot read stream serialized with a newer version of Julia.
                 Got data version $version > current version $ser_version""")
    end
    s.version = version
    return
end

"""
    serialize(stream::IO, value)

Write an arbitrary value to a stream in an opaque format, such that it can be read back by
[`deserialize`](@ref). The read-back value will be as identical as possible to the original,
but note that `Ptr` values are serialized as all-zero bit patterns (`NULL`).

An 8-byte identifying header is written to the stream first. To avoid writing the header,
construct a `Serializer` and use it as the first argument to `serialize` instead.
See also [`Serialization.writeheader`](@ref).

The data format can change in minor (1.x) Julia releases, but files written by prior 1.x
versions will remain readable. The main exception to this is when the definition of a
type in an external package changes. If that occurs, it may be necessary to specify
an explicit compatible version of the affected package in your environment.
Renaming functions, even private functions, inside packages can also put existing files
out of sync. Anonymous functions require special care: because their names are automatically
generated, minor code changes can cause them to be renamed.
Serializing anonymous functions should be avoided in files intended for long-term storage.

In some cases, the word size (32- or 64-bit) of the reading and writing machines must match.
In rarer cases the OS or architecture must also match, for example when using packages
that contain platform-dependent code.
"""
function serialize(s::IO, x)
    ss = Serializer(s)
    writeheader(ss)
    serialize(ss, x)
end

"""
    serialize(filename::AbstractString, value)

Open a file and serialize the given value to it.

!!! compat "Julia 1.1"
    This method is available as of Julia 1.1.
"""
function serialize(filename::AbstractString, x)
    io = open(filename, "w")
    try
        serialize(io, x)
    finally
        close(io)
    end
end

## deserializing values ##

"""
    deserialize(stream)

Read a value written by [`serialize`](@ref). `deserialize` assumes the binary data read from
`stream` is correct and has been serialized by a compatible implementation of [`serialize`](@ref).
`deserialize` is designed for simplicity and performance, and so does not validate
the data read. Malformed data can result in process termination. The caller must ensure
the integrity and correctness of data read from `stream`.
"""
deserialize(s::IO) = deserialize(Serializer(s))

"""
    deserialize(stream, ::Type{T}) where T

Read a value written by [`serialize`](@ref) and assert that the deserialized value is of type `T`.
Throws an `ArgumentError` if the deserialized value is not an instance of `T`.

The type parameter `T` enables the compiler to statically determine the expected return type,
which is useful for trimming (dead-code elimination in compiled binaries). Use `Union` types
to allow multiple possible types, e.g. `deserialize(stream, Union{Int, String})`.

!!! compat "Julia 1.14"
    This method is available as of Julia 1.14.
"""
function deserialize(s::IO, ::Type{T}) where T
    if @generated
        return quote
            ser = Serializer(s)
            b = Int32(read(ser.io, UInt8)::UInt8)
            if b == HEADER_TAG
                readheader(ser)
                b = Int32(read(ser.io, UInt8)::UInt8)
            end
            return deserialize_typed(ser, b, $T)
        end
    else
        ser = Serializer(s)
        b = Int32(read(ser.io, UInt8)::UInt8)
        if b == HEADER_TAG
            readheader(ser)
            b = Int32(read(ser.io, UInt8)::UInt8)
        end
        return deserialize_typed(ser, b, T)
    end
end

"""
    deserialize(filename::AbstractString)

Open a file and deserialize its contents.

!!! compat "Julia 1.1"
    This method is available as of Julia 1.1.
"""
deserialize(filename::AbstractString) = open(deserialize, filename)

"""
    deserialize(filename::AbstractString, ::Type{T}) where T

Open a file and deserialize its contents, asserting that the result is of type `T`.
Throws an `ArgumentError` if the deserialized value is not an instance of `T`.

!!! compat "Julia 1.14"
    This method is available as of Julia 1.14.
"""
function deserialize(filename::AbstractString, ::Type{T}) where T
    io = open(filename)
    try
        deserialize(io, T)
    finally
        close(io)
    end
end

function deserialize(s::AbstractSerializer)
    handle_deserialize(s, Int32(read(s.io, UInt8)::UInt8))
end

function deserialize_cycle(s::AbstractSerializer, @nospecialize(x))
    slot = pop!(s.pending_refs)
    _setbackref!(s, slot, x)
    nothing
end

# optimized version of:
#     slot = s.counter; s.counter += 1
#     push!(s.pending_refs, slot)
#     slot = pop!(s.pending_refs)
#     _setbackref!(s, slot, x)
function resolve_ref_immediately(s::AbstractSerializer, @nospecialize(x))
    _setbackref!(s, s.counter, x)
    s.counter += 1
    nothing
end

# deserialize_ is an internal function to dispatch on the tag
# describing the serialized representation. the number of
# representations is fixed, so deserialize_ does not get extended.
function handle_deserialize(s::AbstractSerializer, b::Int32)
    if b == 0
        return desertag(Int32(read(s.io, UInt8)::UInt8))
    end
    if b >= VALUE_TAGS
        return desertag(b)
    elseif b == TUPLE_TAG
        return deserialize_tuple(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == SHORTBACKREF_TAG
        id = read(s.io, UInt16)::UInt16
        return _getbackref(s, Int(id))
    elseif b == BACKREF_TAG
        id = read(s.io, Int32)::Int32
        return _getbackref(s, Int(id))
    elseif b == ARRAY_TAG
        return deserialize_array(s)
    elseif b == DATATYPE_TAG
        return deserialize_datatype(s, false)
    elseif b == FULL_DATATYPE_TAG
        return deserialize_datatype(s, true)
    elseif b == WRAPPER_DATATYPE_TAG
        tname = deserialize(s)::Core.TypeName
        return unwrap_unionall(tname.wrapper)
    elseif b == OBJECT_TAG
        t = deserialize(s)
        if t === Missing
            return missing
        end
        return deserialize(s, t)
    elseif b == REF_OBJECT_TAG
        slot = s.counter; s.counter += 1
        push!(s.pending_refs, slot)
        t = deserialize(s)
        return deserialize(s, t)
    elseif b == SHARED_REF_TAG
        slot = s.counter; s.counter += 1
        obj = deserialize(s)
        _setbackref!(s, slot, obj)
        return obj
    elseif b == SYMBOL_TAG
        return deserialize_symbol(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == SHORTINT64_TAG
        return Int64(read(s.io, Int32)::Int32)
    elseif b == EXPR_TAG
        return deserialize_expr(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == MODULE_TAG
        return deserialize_module(s)
    elseif b == STRING_TAG
        return deserialize_string(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == LONGSTRING_TAG
        return deserialize_string(s, Int(read(s.io, Int64)::Int64))
    elseif b == SIMPLEVECTOR_TAG
        return deserialize_svec(s)
    elseif b == GLOBALREF_TAG
        return GlobalRef(deserialize(s)::Module, deserialize(s)::Symbol)
    elseif b == FULL_GLOBALREF_TAG
        ty = deserialize(s)
        tn = unwrap_unionall(ty).name
        return GlobalRef(tn.module, tn.name)
    elseif b == LONGTUPLE_TAG
        return deserialize_tuple(s, Int(read(s.io, Int32)::Int32))
    elseif b == LONGEXPR_TAG
        return deserialize_expr(s, Int(read(s.io, Int32)::Int32))
    elseif b == LONGBACKREF_TAG
        id = read(s.io, Int64)::Int64
        return _getbackref(s, Int(id))
    elseif b == LONGSYMBOL_TAG
        return deserialize_symbol(s, Int(read(s.io, Int32)::Int32))
    elseif b == HEADER_TAG
        readheader(s)
        return deserialize(s)
    elseif b == INT8_TAG
        return read(s.io, Int8)
    elseif b == INT8_TAG+1
        return read(s.io, UInt8)
    elseif b == INT8_TAG+2
        return read(s.io, Int16)
    elseif b == INT8_TAG+3
        return read(s.io, UInt16)
    elseif b == INT32_TAG
        return read(s.io, Int32)
    elseif b == INT8_TAG+5
        return read(s.io, UInt32)
    elseif b == INT64_TAG
        return read(s.io, Int64)
    elseif b == INT8_TAG+7
        return read(s.io, UInt64)
    elseif b == INT8_TAG+8
        return read(s.io, Int128)
    elseif b == INT8_TAG+9
        return read(s.io, UInt128)
    elseif b == INT8_TAG+10
        return read(s.io, Float16)
    elseif b == INT8_TAG+11
        return read(s.io, Float32)
    elseif b == INT8_TAG+12
        return read(s.io, Float64)
    elseif b == INT8_TAG+13
        return read(s.io, Char)
    elseif b == IDDICT_TAG
        slot = s.counter; s.counter += 1
        push!(s.pending_refs, slot)
        t = deserialize(s)
        return deserialize_dict(s, t)
    end
    t = desertag(b)::DataType
    if ismutabletype(t) && length(t.types) > 0  # manual specialization of fieldcount
        slot = s.counter; s.counter += 1
        push!(s.pending_refs, slot)
    end
    return deserialize(s, t)
end

function deserialize_symbol(s::AbstractSerializer, len::Int)
    str = Base._string_n(len)
    unsafe_read(s.io, pointer(str), len)
    sym = Symbol(str)
    if len > 7
        resolve_ref_immediately(s, sym)
    end
    return sym
end

function deserialize_tuple(s::AbstractSerializer, len)
    len == 0 && return ()
    Base.Cartesian.@nexprs 10 i -> begin
        len == i && return (Base.Cartesian.@ntuple i _ -> deserialize(s))
    end
    return ntupleany(i -> deserialize(s), len)
end

function deserialize_svec(s::AbstractSerializer)
    n = read(s.io, Int32)
    svec(Any[ deserialize(s) for i=1:n ]...)
end

function deserialize_module(s::AbstractSerializer)
    mkey = deserialize(s)
    if isa(mkey, Tuple)
        # old version, TODO: remove
        if mkey === ()
            return Main
        end
        m = Base.root_module(mkey[1])
        for i = 2:length(mkey)
            m = getglobal(m, mkey[i])::Module
        end
    else
        name = String(deserialize(s)::Symbol)
        pkg = (mkey === nothing) ? Base.PkgId(name) : Base.PkgId(Base.UUID(mkey), name)
        m = Base.root_module(pkg)
        mname = deserialize(s)
        while mname !== ()
            m = getglobal(m, mname)::Module
            mname = deserialize(s)
        end
    end
    return m
end

function deserialize(s::AbstractSerializer, ::Type{Method})
    lnumber = read(s.io, UInt64)
    meth = lookup_object_number(s, lnumber)
    if meth !== nothing
        meth = meth::Method
        makenew = false
    else
        meth = ccall(:jl_new_method_uninit, Ref{Method}, (Any,), Main)
        makenew = true
    end
    deserialize_cycle(s, meth)
    mod = deserialize(s)::Module
    name = deserialize(s)::Symbol
    file = deserialize(s)::Symbol
    line = deserialize(s)::Int32
    sig = deserialize(s)::Type
    syms = deserialize(s)
    if syms isa SimpleVector
        # < v1.2
        _ambig = deserialize(s)
    else
        slot_syms = syms::String
    end
    nargs = deserialize(s)::Int32
    isva = deserialize(s)::Bool
    is_for_opaque_closure = false
    nospecializeinfer = false
    constprop = 0x00
    purity = 0x0000
    template_or_is_opaque = with(current_module => mod) do
        deserialize(s)
    end
    template = if isa(template_or_is_opaque, Bool)
        is_for_opaque_closure = template_or_is_opaque
        if format_version(s) >= 24
            nospecializeinfer = deserialize(s)::Bool
        end
        if format_version(s) >= 14
            constprop = deserialize(s)::UInt8
        end
        if format_version(s) >= 26
            purity = deserialize(s)::UInt16
        elseif format_version(s) >= 17
            purity = UInt16(deserialize(s)::UInt8)
        end
        with(current_module => mod) do
            deserialize(s)
        end
    else
        template_or_is_opaque
    end
    generator = deserialize(s)
    recursion_relation = nothing
    if format_version(s) >= 15
        recursion_relation = deserialize(s)
    end
    if makenew
        meth.module = mod
        meth.debuginfo = NullDebugInfo
        meth.name = name
        meth.file = file
        meth.line = line
        meth.sig = sig
        meth.nargs = nargs
        meth.isva = isva
        meth.is_for_opaque_closure = is_for_opaque_closure
        meth.nospecializeinfer = nospecializeinfer
        meth.constprop = constprop
        meth.purity = purity
        if template !== nothing
            # TODO: compress template
            template = template::CodeInfo
            if format_version(s) < 29
                template.nargs = nargs
                template.isva = isva
            end
            meth.source = template
            meth.debuginfo = template.debuginfo
            if !@isdefined(slot_syms)
                slot_syms = ccall(:jl_compress_argnames, Ref{String}, (Any,), meth.source.slotnames)
            end
        end
        meth.slot_syms = slot_syms
        if generator !== nothing
            meth.generator = generator
        end
        if recursion_relation !== nothing
            meth.recursion_relation = recursion_relation
        end
        if !is_for_opaque_closure
            mt = Core.methodtable
            if nothing === ccall(:jl_methtable_lookup, Any, (Any, UInt), sig, Base.get_world_counter()) # XXX: quite sketchy?
                ccall(:jl_method_table_insert, Cvoid, (Any, Any, Ptr{Cvoid}), mt, meth, C_NULL)
            end
        end
        remember_object(s, meth, lnumber)
    end
    return meth
end

function deserialize(s::AbstractSerializer, ::Type{Core.MethodTable})
    name = deserialize(s)::Symbol
    mod = deserialize(s)::Module
    return getglobal(mod, name)::Core.MethodTable
end

function deserialize(s::AbstractSerializer, ::Type{Core.MethodInstance})
    linfo = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, (Ptr{Cvoid},), C_NULL)
    deserialize_cycle(s, linfo)
    if format_version(s) < 28
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            code = handle_deserialize(s, tag)::CodeInfo
            ci = ccall(:jl_new_codeinst_for_uninferred, Ref{CodeInstance}, (Any, Any), linfo, code)
            @atomic linfo.cache = ci
        end
    end
    tag = Int32(read(s.io, UInt8)::UInt8)
    if tag != UNDEFREF_TAG
        # for reading files prior to v1.2
        handle_deserialize(s, tag)
    end
    linfo.sparam_vals = deserialize(s)::SimpleVector
    _rettype = deserialize(s)  # for backwards compat
    linfo.specTypes = deserialize(s)
    linfo.def = deserialize(s)
    return linfo
end

function deserialize(s::AbstractSerializer, ::Type{Core.LineInfoNode})
    mod = deserialize(s)
    if mod isa Module
        method = deserialize(s)
    else
        # files post v1.2 and pre v1.6 are broken
        method = mod
        mod = Main
    end
    return Core.LineInfoNode(mod, method, deserialize(s)::Symbol, Int32(deserialize(s)::Union{Int32, Int}), Int32(deserialize(s)::Union{Int32, Int}))
end


function deserialize(s::AbstractSerializer, ::Type{PhiNode})
    edges = deserialize(s)
    if edges isa Vector{Any}
        edges = Vector{Int32}(edges)
    end
    values = deserialize(s)::Vector{Any}
    return PhiNode(edges, values)
end

# v1.12 disallows bare symbols in IR, but older CodeInfos might still have them
function symbol_to_globalref(@nospecialize(x), m::Module)
    mapper(@nospecialize(x)) = symbol_to_globalref(x, m)
    if x isa Symbol
        return GlobalRef(m, x)
    elseif x isa Expr
        return Expr(x.head, map(mapper, x.args)...)
    elseif x isa ReturnNode
        return ReturnNode(mapper(x.val))
    elseif x isa GotoIfNot
        return GotoIfNot(mapper(x.cond), x.dest)
    else
        return x
    end
end

function deserialize(s::AbstractSerializer, ::Type{CodeInfo})
    ci = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    deserialize_cycle(s, ci)
    code = deserialize(s)::Vector{Any}
    ci.code = code
    ci.debuginfo = NullDebugInfo
    # allow older-style IR with return and gotoifnot Exprs
    for i in 1:length(code)
        stmt = code[i]
        if isa(stmt, Expr)
            ex = stmt::Expr
            if ex.head === :return
                code[i] = ReturnNode(isempty(ex.args) ? nothing : ex.args[1])
            elseif ex.head === :gotoifnot
                code[i] = GotoIfNot(ex.args[1], ex.args[2])
            end
        end
    end
    if current_module[] !== nothing
        map!(x->symbol_to_globalref(x, current_module[]), code)
    end
    _x = deserialize(s)
    have_debuginfo = _x isa Core.DebugInfo
    if have_debuginfo
        ci.debuginfo = _x
    else
        codelocs = _x::Vector{Int32}
        # TODO: convert codelocs to debuginfo format?
    end
    _x = deserialize(s)
    if _x isa Array || _x isa Int
        pre_12 = false
    else
        pre_12 = true
        # < v1.2
        ci.method_for_inference_limit_heuristics = _x
        _x = deserialize(s)
    end
    ci.ssavaluetypes = _x
    if pre_12
        linetable = deserialize(s)
        # TODO: convert linetable to debuginfo format?
    end
    ssaflags = deserialize(s)
    if length(ssaflags) ≠ length(code)
        # make sure the length of `ssaflags` matches that of `code`
        # so that the latest inference doesn't throw on IRs serialized from old versions
        ssaflags = UInt32[0x00 for _ in 1:length(code)]
    elseif eltype(ssaflags) != UInt32
        ssaflags = map(UInt32, ssaflags)
    end
    ci.ssaflags = ssaflags
    if pre_12
        ci.slotflags = deserialize(s)
    else
        if format_version(s) <= 26
            ci.method_for_inference_limit_heuristics = deserialize(s)
        end
        if !have_debuginfo # pre v1.11 format
            linetable = deserialize(s)
            # TODO: convert linetable to debuginfo format?
        end
    end
    ci.slotnames = deserialize(s)
    if !pre_12
        ci.slotflags = deserialize(s)
        ci.slottypes = deserialize(s)
        ci.rettype = deserialize(s)
        ci.parent = deserialize(s)
        if format_version(s) < 29 && ci.parent isa MethodInstance && ci.parent.def isa Method
            ci.nargs = ci.parent.def.nargs
        end
        world_or_edges = deserialize(s)
        pre_13 = isa(world_or_edges, Union{UInt, Int})
        if pre_13
            ci.min_world = reinterpret(UInt, world_or_edges)
            ci.max_world = reinterpret(UInt, deserialize(s))
        else
            ci.edges = world_or_edges
            ci.min_world = deserialize(s)::UInt
            ci.max_world = deserialize(s)::UInt
        end
        if format_version(s) >= 29
            ci.method_for_inference_limit_heuristics = deserialize(s)
        end
    end
    if format_version(s) <= 26
        deserialize(s)::Bool # inferred
    end
    if format_version(s) < 22
        inlining_cost = deserialize(s)
        if isa(inlining_cost, Bool)
            Core.Compiler.set_inlineable!(ci, inlining_cost)
        else
            ci.inlining_cost = inlining_cost
        end
    end
    if format_version(s) >= 29
        ci.nargs = deserialize(s)
    end
    ci.propagate_inbounds = deserialize(s)
    if format_version(s) < 23
        deserialize(s) # `pure` field has been removed
    end
    if format_version(s) >= 20
        ci.has_fcall = deserialize(s)
    end
    if format_version(s) >= 30
        ci.has_image_globalref = deserialize(s)::Bool
    end
    if format_version(s) >= 24
        ci.nospecializeinfer = deserialize(s)::Bool
    end
    if format_version(s) >= 29
        ci.isva = deserialize(s)::Bool
    end
    if format_version(s) >= 21
        ci.inlining = deserialize(s)::UInt8
    end
    if format_version(s) >= 14
        ci.constprop = deserialize(s)::UInt8
    end
    if format_version(s) >= 26
        ci.purity = deserialize(s)::UInt16
    elseif format_version(s) >= 17
        ci.purity = deserialize(s)::UInt8
    end
    if format_version(s) >= 22
        ci.inlining_cost = deserialize(s)::UInt16
    end
    ci.debuginfo = NullDebugInfo
    return ci
end

import Core: NullDebugInfo

if Int === Int64
const OtherInt = Int32
else
const OtherInt = Int64
end

function deserialize_array(s::AbstractSerializer)
    slot = s.counter; s.counter += 1
    d1 = deserialize(s)
    if isa(d1, Type)
        elty = d1
        d1 = deserialize(s)
    else
        elty = UInt8
    end
    if isa(d1, Int32) || isa(d1, Int64)
        if elty !== Bool && isbitstype(elty)
            a = Vector{elty}(undef, d1)
            _setbackref!(s, slot, a)
            return read!(s.io, a)
        end
        dims = (Int(d1),)
    elseif d1 isa Dims
        dims = d1::Dims
    else
        dims = convert(Dims, d1::Tuple{Vararg{OtherInt}})::Dims
    end
    if isbitstype(elty)
        n = prod(dims)::Int
        if elty === Bool && n > 0
            A = Array{Bool, length(dims)}(undef, dims)
            i = 1
            while i <= n
                b = read(s.io, UInt8)::UInt8
                v = (b >> 7) != 0
                count = b & 0x7f
                nxt = i + count
                while i < nxt
                    A[i] = v
                    i += 1
                end
            end
        else
            A = read!(s.io, Array{elty}(undef, dims))
        end
        _setbackref!(s, slot, A)
        return A
    end
    A = Array{elty, length(dims)}(undef, dims)
    _setbackref!(s, slot, A)
    _sizehint_backref!(s, s.counter + div(length(A)::Int,4))
    deserialize_fillarray!(A, s)
    return A
end

function deserialize_fillarray!(A::Union{Array{T},Memory{T}}, s::AbstractSerializer) where {T}
    for i = eachindex(A)
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            @inbounds A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

function deserialize(s::AbstractSerializer, X::Type{Memory{T}} where T)
    slot = pop!(s.pending_refs) # e.g. deserialize_cycle
    n = deserialize(s)::Int
    elty = eltype(X)
    if isbitstype(elty)
        A = X(undef, n)
        if X === Memory{Bool}
            i = 1
            while i <= n
                b = read(s.io, UInt8)::UInt8
                v = (b >> 7) != 0
                count = b & 0x7f
                nxt = i + count
                while i < nxt
                    A[i] = v
                    i += 1
                end
            end
        else
            A = read!(s.io, A)::X
        end
        _setbackref!(s, slot, A)
        return A
    end
    A = X(undef, n)
    _setbackref!(s, slot, A)
    _sizehint_backref!(s, s.counter + div(n, 4))
    deserialize_fillarray!(A, s)
    return A
end

function deserialize(s::AbstractSerializer, X::Type{MemoryRef{T}} where T)
    x = Core.memoryref(deserialize(s))::X
    i = deserialize(s)::Int
    i == 1 || (x = Core.memoryrefnew(x, i, true))
    return x::X
end

function deserialize(s::AbstractSerializer, X::Type{Core.AddrSpace{M}} where M)
    Core.bitcast(X, read(s.io, UInt8))
end

function deserialize(s::AbstractSerializer, ::Type{Core.IntrinsicFunction})
    name = deserialize(s)::Symbol
    return getfield(Core.Intrinsics, name)::Core.IntrinsicFunction
end

function deserialize_expr(s::AbstractSerializer, len)
    e = Expr(:temp)
    resolve_ref_immediately(s, e)
    e.head = deserialize(s)::Symbol
    e.args = Any[ deserialize(s) for i = 1:len ]
    e
end

module __deserialized_types__ end

function deserialize(s::AbstractSerializer, ::Type{Core.TypeName})
    number = read(s.io, UInt64)
    return deserialize_typename(s, number)
end

function deserialize_typename(s::AbstractSerializer, number)
    name = deserialize(s)::Symbol
    tn = lookup_object_number(s, number)
    if tn !== nothing
        makenew = false
    else
        # reuse the same name for the type, if possible, for nicer debugging
        tn_name = isdefined(__deserialized_types__, name) ? gensym() : name
        tn = ccall(:jl_new_typename_in, Any, (Any, Any, Cint, Cint),
                   tn_name, __deserialized_types__, false, false)
        makenew = true
    end
    tn = tn::Core.TypeName
    remember_object(s, tn, number)
    deserialize_cycle(s, tn)

    names = deserialize(s)::SimpleVector
    super = deserialize(s)::Type
    parameters = deserialize(s)::SimpleVector
    types = deserialize(s)::SimpleVector
    attrs = Core.svec()
    has_instance = deserialize(s)::Bool
    abstr = deserialize(s)::Bool
    mutabl = deserialize(s)::Bool
    ninitialized = deserialize(s)::Int32
    maxm = format_version(s) >= 18 ? deserialize(s)::UInt8 : UInt8(0)

    if makenew
        # TODO: there's an unhanded cycle in the dependency graph at this point:
        # while deserializing super and/or types, we may have encountered
        # tn.wrapper and throw UndefRefException before we get to this point
        ndt = ccall(:jl_new_datatype, Any, (Any, Any, Any, Any, Any, Any, Any, Cint, Cint, Cint),
                    tn, tn.module, super, parameters, names, types, attrs,
                    abstr, mutabl, ninitialized)
        @assert tn == ndt.name
        ccall(:jl_set_const, Cvoid, (Any, Any, Any), tn.module, tn.name, tn.wrapper)
        ty = tn.wrapper
        tn.max_methods = maxm
        if has_instance
            ty = ty::DataType
            if !isdefined(ty, :instance)
                singleton = ccall(:jl_new_struct, Any, (Any, Any...), ty)
                # use setfield! directly to avoid `fieldtype` lowering expecting to see a Singleton object already on ty
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), ty, Base.fieldindex(DataType, :instance)-1, singleton)
            end
        end
    end

    tag = Int32(read(s.io, UInt8)::UInt8)
    if tag != UNDEFREF_TAG
        mtname = handle_deserialize(s, tag)
        defs = deserialize(s)
        maxa = deserialize(s)::Union{Int,Int32}
        if makenew
            tn.singletonname = mtname
            setfield!(tn, :max_args, Int32(maxa), :monotonic)
        end
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            kws = handle_deserialize(s, tag)
            if makenew && !(kws isa Vector{Method})
                # old object format -- try to forward from old to new
                @eval Core.kwcall(kwargs::NamedTuple, f::$ty, args...) = $kws(kwargs, f, args...)
            end
        end
    end
    return tn
end

function deserialize_datatype(s::AbstractSerializer, full::Bool)
    slot = s.counter; s.counter += 1
    if full
        tname = deserialize(s)::Core.TypeName
        ty = tname.wrapper
    else
        name = deserialize(s)::Symbol
        mod = deserialize(s)::Module
        ty = getglobal(mod, name)
    end
    if isa(ty,DataType) && isempty(ty.parameters)
        t = ty
    else
        np = Int(read(s.io, Int32)::Int32)
        if np == 0
            t = unwrap_unionall(ty)
        elseif ty === Tuple
            # note np==0 has its own tag
            if np == 1
                t = Tuple{deserialize(s)}
            elseif np == 2
                t = Tuple{deserialize(s), deserialize(s)}
            elseif np == 3
                t = Tuple{deserialize(s), deserialize(s), deserialize(s)}
            elseif np == 4
                t = Tuple{deserialize(s), deserialize(s), deserialize(s), deserialize(s)}
            else
                t = Tuple{Any[ deserialize(s) for i=1:np ]...}
            end
        else
            t = ty
            for i = 1:np
                t = t{deserialize(s)}
            end
        end
    end
    _setbackref!(s, slot, t)
    return t
end

function deserialize(s::AbstractSerializer, ::Type{UnionAll})
    form = read(s.io, UInt8)
    if form == 0
        var = deserialize(s)
        body = deserialize(s)
        return UnionAll(var, body)
    else
        n = read(s.io, Int16)
        t = deserialize(s)::DataType
        w = t.name.wrapper
        k = 0
        while isa(w, UnionAll)
            w = w.body
            k += 1
        end
        w = t.name.wrapper
        k -= n
        while k > 0
            w = w.body
            k -= 1
        end
        return w
    end
end

function deserialize(s::AbstractSerializer, ::Type{Task})
    t = Task(()->nothing)
    deserialize_cycle(s, t)
    t.code = deserialize(s)
    t.storage = deserialize(s)
    state = deserialize(s)
    if state === :runnable
        @atomic :release t._state = Base.task_state_runnable
    elseif state === :done
        @atomic :release t._state = Base.task_state_done
    elseif state === :failed
        @atomic :release t._state = Base.task_state_failed
    else
        @assert false
    end
    t.result = deserialize(s)
    exc = deserialize(s)
    if exc === nothing
        t._isexception = false
    elseif exc isa Bool
        t._isexception = exc
    else
        t._isexception = true
        t.result = exc
    end
    t
end

function deserialize_string(s::AbstractSerializer, len::Int)
    out = ccall(:jl_alloc_string, Ref{String}, (Csize_t,), len)
    unsafe_read(s.io, pointer(out), len)
    return out
end

# Typed deserialization for trimming support
# ==========================================
#
# These functions provide statically-typed deserialization paths that avoid the
# fully-dynamic `handle_deserialize` dispatch. When the expected type `T` is known
# at compile time (via `deserialize(s, T)`), the `if @generated` mechanism emits
# specialized code that only handles the relevant serialization tags for that type,
# never calling into the generic `handle_deserialize` (which would produce
# unresolvable dynamic dispatch that breaks trimming).

"""
    deserialize_typed(s::AbstractSerializer, b::Int32, ::Type{T}) -> T

Deserialize a value of known type `T` from the stream, given that the first tag byte `b`
has already been read. Uses `if @generated` to emit specialized code for concrete types.
"""
function deserialize_typed(s::AbstractSerializer, b::Int32, ::Type{T}) where T
    if @generated
        if T isa Union
            members = Base.uniontypes(T)
            if all(t -> isconcretetype(t) || t === Union{} || can_deserialize_typed(t), members)
                return emit_deserialize_union(T, members)
            end
        end

        if !isconcretetype(T)
            # Non-concrete T: fall back to the fully-dynamic dispatch. Such
            # calls are not trim-safe, but the trim verifier already rejects
            # abstract `T` at the entrypoint (`deserialize(s, ::Type{T})`).
            return quote
                result = handle_deserialize(s, b)
                result isa $T || throw(ArgumentError(
                    string("expected value of type ", $(string(T)), " during deserialization, got ", typeof(result))))
                return result::$T
            end
        end

        parts = Expr[]
        emit_deserialize_refs!(parts, T)
        emit_deserialize_body!(parts, T)
        push!(parts, quote
            if b >= VALUE_TAGS
                result = desertag(b)
                result isa $T && return result::$T
            end
            throw(ArgumentError(
                string("unexpected serialization tag ", b, " when deserializing ", $(string(T)))))
        end)

        return Expr(:block, parts...)
    else
        # Non-generated path (interpreter / introspection only — not trim-compiled):
        # defer to the untyped dispatch with a runtime type assertion.
        handle_deserialize(s, b)::T
    end
end

# Check whether a non-concrete type can be handled by deserialize_typed without
# falling back to handle_deserialize. Currently only `Union` types whose members
# are all themselves handleable qualify; everything else must be concrete.
function can_deserialize_typed(@nospecialize(T))
    T isa Union && return all(can_deserialize_typed, Base.uniontypes(T))
    return isconcretetype(T)
end

# Emit the common backref / shared-ref / header handling block.
# Predicate and reader for the three backref tag variants. Widely reused in
# both generated and interpreted deserialization paths. Assumes `b` is one of
# `SHORTBACKREF_TAG` / `BACKREF_TAG` / `LONGBACKREF_TAG`.
is_backref_tag(b::Int32) = b == SHORTBACKREF_TAG || b == BACKREF_TAG || b == LONGBACKREF_TAG
@inline function read_backref_id(s::AbstractSerializer, b::Int32)
    b == SHORTBACKREF_TAG && return Int(read(s.io, UInt16)::UInt16)
    b == BACKREF_TAG      && return Int(read(s.io, Int32)::Int32)
    return Int(read(s.io, Int64)::Int64)
end

function emit_deserialize_refs!(parts::Vector{Expr}, @nospecialize(T))
    push!(parts, quote
        if is_backref_tag(b)
            return gettable(s, read_backref_id(s, b))::$T
        elseif b == SHARED_REF_TAG
            slot = s.counter; s.counter += 1
            nb = Int32(read(s.io, UInt8)::UInt8)
            obj = deserialize_typed(s, nb, $T)
            s.table[slot] = obj
            return obj::$T
        elseif b == HEADER_TAG
            readheader(s)
            nb = Int32(read(s.io, UInt8)::UInt8)
            return deserialize_typed(s, nb, $T)
        end
    end)
end

# Generate code for deserializing a Union type by combining handlers for all members.
function emit_deserialize_union(@nospecialize(T), members)
    parts = Expr[]

    # Backrefs / shared refs
    emit_deserialize_refs!(parts, T)

    # Emit type-specific handlers for each member type
    for M in members
        emit_deserialize_body!(parts, M)
    end

    # VALUE_TAGS
    push!(parts, quote
        if b >= VALUE_TAGS
            result = desertag(b)
            result isa $T && return result::$T
        end
        throw(ArgumentError(
            string("unexpected serialization tag ", b, " when deserializing ", $(string(T)))))
    end)

    return Expr(:block, parts...)
end

# Code generation helpers for deserialize_typed
function emit_deserialize_body!(parts::Vector{Expr}, @nospecialize(T))
    if T === Bool || T === Nothing
        # Handled entirely by the VALUE_TAGS fallback at the end of the generated function.
    elseif T === Missing
        # missing does not have a sertag; it's serialized as OBJECT_TAG
        push!(parts, quote
            if b == OBJECT_TAG
                deserialize_read_type!(s, Missing)
                return missing
            end
        end)
    elseif T === Int64
        push!(parts, quote
            if b == INT64_TAG
                return read(s.io, Int64)
            elseif b == SHORTINT64_TAG
                return Int64(read(s.io, Int32)::Int32)
            elseif b >= VALUE_TAGS
                return Int64(desertag(b)::Int64)
            end
        end)
    elseif T === Int32
        push!(parts, quote
            if b == INT32_TAG
                return read(s.io, Int32)
            elseif b >= VALUE_TAGS
                return Int32(desertag(b)::Int32)
            end
        end)
    elseif T <: Union{Int8, UInt8, Int16, UInt16, UInt32, UInt64,
                      Int128, UInt128, Float16, Float32, Float64, Char}
        tag = sertag(T)
        push!(parts, quote
            if b == $(Int32(tag))
                return read(s.io, $T)
            end
        end)
    elseif T === String
        push!(parts, quote
            if b == STRING_TAG
                return deserialize_string(s, Int(read(s.io, UInt8)::UInt8))
            elseif b == LONGSTRING_TAG
                return deserialize_string(s, Int(read(s.io, Int64)::Int64))
            end
        end)
    elseif T === Symbol
        push!(parts, quote
            if b == SYMBOL_TAG
                return deserialize_symbol(s, Int(read(s.io, UInt8)::UInt8))
            elseif b == LONGSYMBOL_TAG
                return deserialize_symbol(s, Int(read(s.io, Int32)::Int32))
            end
        end)
    elseif T <: Tuple
        emit_deserialize_tuple!(parts, T)
    elseif T <: Array
        emit_deserialize_array!(parts, T)
    elseif T <: Memory
        emit_deserialize_memory!(parts, T)
    elseif T <: AbstractDict
        emit_deserialize_dict!(parts, T)
    elseif T isa DataType
        emit_deserialize_struct!(parts, T)
    end
end

function emit_deserialize_tuple!(parts::Vector{Expr}, @nospecialize(T))
    # NamedTuples serialize like immutable structs (OBJECT_TAG + type + fields)
    # and are routed through `emit_deserialize_struct!` via the fall-through in
    # `emit_deserialize_body!`; here we only handle plain `Tuple{...}`.
    n = fieldcount(T)
    if n == 0
        push!(parts, quote
            if b == EMPTYTUPLE_TAG
                return ()::$T
            end
        end)
        return
    end
    ancestors = Set{DataType}([T])
    field_reads = Expr[]
    for i in 1:n
        FT = fieldtype(T, i)
        ft_sym = gensym(:tf)
        val = emit_field_read_expr(FT, ft_sym, ancestors)
        push!(field_reads, :(
            let $(ft_sym) = Int32(read(s.io, UInt8)::UInt8)
                ($(val))::$FT
            end))
    end
    lenmsg = :(throw(ArgumentError(
        string("Tuple ", $(string(T)), " has ", $n, " fields but stream has ", len))))
    push!(parts, quote
        if b == TUPLE_TAG
            len = Int(read(s.io, UInt8)::UInt8)
            len == $n || $lenmsg
            return ($(field_reads...),)::$T
        elseif b == LONGTUPLE_TAG
            len = Int(read(s.io, Int32)::Int32)
            len == $n || $lenmsg
            return ($(field_reads...),)::$T
        end
    end)
end

function emit_deserialize_array!(parts::Vector{Expr}, @nospecialize(T))
    # Array{ET, N} — we know the element type and ndims at compile time
    ET = eltype(T)
    N = ndims(T)
    push!(parts, quote
        if b == ARRAY_TAG
            return deserialize_array_typed(s, $ET, Val($N))::$T
        end
    end)
end

# Consume the serialized element type tag for a typed array, if one was written.
# `serialize(::AbstractSerializer, ::Array)` skips the element type tag iff
# `elty === UInt8`; for everything else the type was emitted via `serialize_datatype`.
# Because `ET` is known at compile time, the `@generated` body reduces to either a
# no-op or a direct call to `deserialize_read_type!(s, ET)`, which itself is `@generated`
# to emit only the tag branches valid for `ET`.
function deserialize_array_read_eltype!(s::AbstractSerializer, ::Type{ET}) where {ET}
    if @generated
        ET === UInt8 ? :nothing : :(deserialize_read_type!(s, $ET); nothing)
    else
        ET === UInt8 || deserialize_read_type!(s, ET)
        nothing
    end
end

"""
    deserialize_array_typed(s, ::Type{ET}, ::Val{N}) -> Array{ET, N}

Typed array deserialization. Since `ET` is known at compile time, we know whether
the element type was serialized (everything except `UInt8`) and delegate tag
consumption to `deserialize_read_type!(s, ET)`, which emits only the tag branches
valid for `ET`. This avoids pulling in the fully-dynamic `deserialize_typename`
path.
"""
function deserialize_array_typed(s::AbstractSerializer, ::Type{ET}, ::Val{N}) where {ET, N}
    slot = s.counter; s.counter += 1
    deserialize_array_read_eltype!(s, ET)
    db = Int32(read(s.io, UInt8)::UInt8)
    d1 = deserialize_read_dims(s, db)

    if isa(d1, Int64)
        if ET !== Bool && isbitstype(ET)
            a = Vector{ET}(undef, d1)
            s.table[slot] = a
            return read!(s.io, a)::Array{ET, N}
        end
        dims = (d1,)::NTuple{1, Int64}
    else
        dims = d1::NTuple{N, Int64}
    end

    if isbitstype(ET)
        n = prod(dims)::Int
        if ET === Bool && n > 0
            A = Array{Bool, N}(undef, dims)
            i = 1
            while i <= n
                b = read(s.io, UInt8)::UInt8
                v = (b >> 7) != 0
                count = b & 0x7f
                nxt = i + count
                while i < nxt
                    A[i] = v
                    i += 1
                end
            end
        else
            A = read!(s.io, Array{ET, N}(undef, dims))
        end
        s.table[slot] = A
        return A::Array{ET, N}
    end
    A = Array{ET, N}(undef, dims)
    s.table[slot] = A
    sizehint!(s.table, s.counter + div(length(A)::Int, 4))
    deserialize_fillarray_typed!(A, s)
    return A::Array{ET, N}
end

# Read dimensions from the stream for typed array deserialization.
# Returns either an Int64 (for 1-d) or an NTuple of Int64.
function deserialize_read_dims(s::AbstractSerializer, b::Int32)
    read_one(tb::Int32) =
        tb == INT64_TAG     ? read(s.io, Int64) :
        tb == INT32_TAG     ? Int64(read(s.io, Int32)::Int32) :
        tb == SHORTINT64_TAG ? Int64(read(s.io, Int32)::Int32) :
        tb >= VALUE_TAGS    ? Int64(desertag(tb)::Union{Int32, Int64}) :
        throw(ArgumentError("unexpected tag in array dimensions"))
    if b == TUPLE_TAG
        n = Int(read(s.io, UInt8)::UInt8)
        return ntuple(_ -> read_one(Int32(read(s.io, UInt8)::UInt8)), n)::NTuple{n, Int64}
    elseif b == LONGTUPLE_TAG
        n = Int(read(s.io, Int32)::Int32)
        return ntuple(_ -> read_one(Int32(read(s.io, UInt8)::UInt8)), n)::NTuple{n, Int64}
    end
    return read_one(b)
end

# Typed version of deserialize_fillarray! that uses deserialize_typed instead
# of handle_deserialize. Uses `if @generated` on the element type so that, when
# the element is itself a user-defined struct, the per-element body is inlined
# via `emit_struct_value_expr` — otherwise the inner `deserialize_typed` call
# would be a cross-specialization invoke the trim verifier cannot resolve.
function deserialize_fillarray_typed!(A::Union{Array{T},Memory{T}}, s::AbstractSerializer) where {T}
    if @generated
        tag = gensym(:tag)
        elem_expr = emit_field_read_expr(T, tag, Set{DataType}())
        return quote
            for i = eachindex(A)
                $tag = Int32(read(s.io, UInt8)::UInt8)
                if $tag != UNDEFREF_TAG
                    @inbounds A[i] = ($(elem_expr))::$T
                end
            end
            return A
        end
    else
        for i = eachindex(A)
            tag = Int32(read(s.io, UInt8)::UInt8)
            if tag != UNDEFREF_TAG
                @inbounds A[i] = deserialize_typed(s, tag, T)
            end
        end
        return A
    end
end

# Typed Memory deserialization — avoids untyped deserialize(s, T) which uses handle_deserialize.
function deserialize_memory_typed(s::AbstractSerializer, ::Type{X}) where {X <: Memory}
    slot = pop!(s.pending_refs)
    # Read length — typed read of Int64
    nb = Int32(read(s.io, UInt8)::UInt8)
    n = deserialize_typed(s, nb, Int)::Int
    ET = eltype(X)
    if isbitstype(ET)
        A = X(undef, n)
        if X === Memory{Bool}
            i = 1
            while i <= n
                b = read(s.io, UInt8)::UInt8
                v = (b >> 7) != 0
                count = b & 0x7f
                nxt = i + count
                while i < nxt
                    A[i] = v
                    i += 1
                end
            end
        else
            A = read!(s.io, A)::X
        end
        s.table[slot] = A
        return A
    end
    A = X(undef, n)
    s.table[slot] = A
    sizehint!(s.table, s.counter + div(n, 4))
    deserialize_fillarray_typed!(A, s)
    return A
end

function emit_deserialize_memory!(parts::Vector{Expr}, @nospecialize(T))
    # Memory{ET} is serialized via serialize_cycle_header → REF_OBJECT_TAG + type + length + data
    ET = eltype(T)
    push!(parts, quote
        if b == REF_OBJECT_TAG
            slot = s.counter; s.counter += 1
            push!(s.pending_refs, slot)
            deserialize_read_type!(s, $T)
            return deserialize_memory_typed(s, $T)::$T
        end
    end)
end

function emit_deserialize_dict!(parts::Vector{Expr}, @nospecialize(T))
    # Dict is serialized via serialize_cycle_header → REF_OBJECT_TAG + type + data;
    # IDDICT_TAG (IdDict) uses the same framing but is only valid for an IdDict T.
    if T <: Dict && isconcretetype(T)
        push!(parts, quote
            if b == IDDICT_TAG || b == REF_OBJECT_TAG
                slot = s.counter; s.counter += 1
                push!(s.pending_refs, slot)
                deserialize_read_type!(s, $T)
                return deserialize_dict_typed(s, $T)::$T
            end
        end)
    else
        push!(parts, quote
            if b == IDDICT_TAG
                slot = s.counter; s.counter += 1
                push!(s.pending_refs, slot)
                t = deserialize(s)
                return deserialize_dict(s, t)::$T
            elseif b == REF_OBJECT_TAG
                slot = s.counter; s.counter += 1
                push!(s.pending_refs, slot)
                deserialize_read_type!(s, $T)
                return deserialize_dict(s, $T)::$T
            end
        end)
    end
end


# Types with their own top-level tag byte (e.g. `STRING_TAG`, `ARRAY_TAG`,
# tuple tags, `DICT_TAG`), dispatched in `emit_deserialize_body!` ahead of
# the generic struct path. They do not use the `OBJECT_TAG + type + fields`
# envelope. Their typed deserialization goes through dedicated helpers
# (`deserialize_array_typed`, `deserialize_memory_typed`,
# `deserialize_dict_typed`, ...) whose bodies the trim verifier can resolve,
# so they are never inlined as struct bodies into a parent's generated code.
uses_own_tag(@nospecialize(T)) = T === String || T === Symbol ||
    T <: Tuple || T <: Array || T <: Memory || T <: AbstractDict

# Struct-shaped types that share the `OBJECT_TAG + type + payload` envelope
# but whose payload does not match the default "one field per declaration"
# layout (they skip internal pointers or replace fields with surrogates).
# `emit_deserialize_struct!` delegates to their hand-written
# `deserialize(s, ::Type{T})` method instead of emitting field reads.
uses_custom_payload(@nospecialize(T)) =
    T === Regex || T === BigInt || T === Base.StackTraces.StackFrame

# Whether a concrete type is a struct whose body we should inline into a
# parent's generated body rather than route through a recursive
# `deserialize_typed` call. Inlining is required because the trim verifier
# cannot resolve cross-specialization `invoke`s between two
# `@generated deserialize_typed` bodies for different struct types.
is_inlineable_struct(@nospecialize(T)) =
    T isa DataType && isconcretetype(T) && !isprimitivetype(T) &&
    fieldcount(T) > 0 && !uses_own_tag(T) && !uses_custom_payload(T)

# Emit an inline value-expression that produces a field of concrete type `FT`,
# given that its 1-byte tag has already been read into the variable named
# `tag_sym` (and is known to differ from `UNDEFREF_TAG`). For inlineable
# struct fields whose type is not currently being inlined (i.e. not in
# `ancestors`) the struct's body is inlined to avoid cross-type
# `deserialize_typed` invokes the trim verifier cannot resolve. All other
# field types delegate to `deserialize_typed`, whose own specialization the
# trimmer can resolve (see e.g. `deserialize_fillarray_typed!`).
emit_field_read_expr(@nospecialize(FT), tag_sym::Symbol, ancestors::Set{DataType}) =
    if is_inlineable_struct(FT) && FT ∉ ancestors
        new_ancestors = copy(ancestors)
        push!(new_ancestors, FT)
        emit_struct_value_expr(FT, tag_sym, new_ancestors)
    else
        :(deserialize_typed(s, $tag_sym, $FT))
    end

# Build the two value-expression bodies for a user-struct type `T`:
# one for OBJECT_TAG (no backref registration) and one for REF_OBJECT_TAG.
# `ancestors` is the set of struct types currently being inlined on the
# enclosing path; it must already contain `T` and is used directly (the
# caller is responsible for copying if needed).
# Used both by `emit_struct_value_expr` (inlined as a field) and
# `emit_deserialize_struct!` (top-level dispatch).
# Build a list of per-field read expressions for `T`. Each element reads the
# 1-byte field tag into a fresh local and dispatches to `set_field(i, FT,
# ft_sym, val)` for the actual store/assignment (which also handles the
# `UNDEFREF_TAG` case).
function emit_field_reads(@nospecialize(T), ancestors::Set{DataType}, set_field)
    field_reads = Expr[]
    for i in 1:fieldcount(T)
        FT = fieldtype(T, i)
        ft_sym = gensym(:ft)
        val = emit_field_read_expr(FT, ft_sym, ancestors)
        push!(field_reads, :(
            let $(ft_sym) = Int32(read(s.io, UInt8)::UInt8)
                $(set_field(i, FT, ft_sym, val))
            end))
    end
    return field_reads
end

function emit_struct_bodies(@nospecialize(T), ancestors::Set{DataType})
    nf = fieldcount(T)
    # Use gensym'd locals so nested inlined struct bodies do not clobber each
    # other's `x` / `vflds` / `na` bindings when one struct type is inlined
    # as a field of another.
    if ismutabletype(T)
        x = gensym(:x)
        field_reads = emit_field_reads(T, ancestors, (i, FT, ft_sym, val) -> :(
            if $ft_sym != UNDEFREF_TAG
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), $x, $(i-1),
                      ($val)::$FT)
            end))
        object_body = quote
            deserialize_read_type!(s, $T)
            $x = ccall(:jl_new_struct_uninit, Any, (Any,), $T)
            $(field_reads...)
            $x
        end
        ref_body = quote
            _rslot = s.counter; s.counter += 1
            push!(s.pending_refs, _rslot)
            deserialize_read_type!(s, $T)
            $x = ccall(:jl_new_struct_uninit, Any, (Any,), $T)
            deserialize_cycle(s, $x)
            $(field_reads...)
            $x
        end
    else
        vflds = gensym(:vflds)
        na = gensym(:na)
        field_reads = emit_field_reads(T, ancestors, (i, FT, ft_sym, val) -> :(
            if $ft_sym != UNDEFREF_TAG
                $vflds[$i] = ($val)::$FT
            else
                $na >= $i && ($na = $(i - 1))
            end))
        object_body = quote
            deserialize_read_type!(s, $T)
            $na = $nf
            $vflds = Vector{Any}(undef, $nf)
            $(field_reads...)
            ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), $T, $vflds, $na)
        end
        ref_body = object_body
    end
    return object_body, ref_body
end

# Emit an inline value-expression producing a value of user-struct type `T`,
# given its 1-byte tag in `tag_sym`. `ancestors` must already contain `T`
# (the caller is responsible for copying and pushing).
function emit_struct_value_expr(@nospecialize(T), tag_sym::Symbol, ancestors::Set{DataType})
    object_body, ref_body = emit_struct_bodies(T, ancestors)
    return quote
        if $tag_sym == OBJECT_TAG
            $(object_body)::$T
        elseif $tag_sym == REF_OBJECT_TAG
            $(ref_body)::$T
        elseif is_backref_tag($tag_sym)
            gettable(s, read_backref_id(s, $tag_sym))::$T
        else
            # Unreachable for normal struct fields; avoid a cross-specialization
            # `deserialize_typed` call so the trim verifier can resolve the
            # enclosing specialization.
            throw(ArgumentError(string("unexpected tag ", $tag_sym,
                                       " while deserializing field of type ", $(string(T)))))
        end
    end
end

function emit_deserialize_struct!(parts::Vector{Expr}, @nospecialize(T))
    if uses_custom_payload(T)
        # Delegate the payload after `OBJECT_TAG + type` to the hand-written
        # `deserialize(s, ::Type{T})` method. These types' hand-written
        # `serialize` methods only ever emit `OBJECT_TAG` (no cycle header).
        push!(parts, quote
            if b == OBJECT_TAG
                deserialize_read_type!(s, $T)
                return deserialize(s, $T)::$T
            end
        end)
    elseif isprimitivetype(T)
        push!(parts, quote
            if b == OBJECT_TAG
                deserialize_read_type!(s, $T)
                return read(s.io, $T)
            end
        end)
    else
        object_body, ref_body = emit_struct_bodies(T, Set{DataType}([T]))
        if ismutabletype(T) && fieldcount(T) > 0
            push!(parts, quote
                if b == REF_OBJECT_TAG
                    return ($(ref_body))::$T
                elseif b == OBJECT_TAG
                    return ($(object_body))::$T
                end
            end)
        else
            # Immutable struct (or mutable with 0 fields) — only OBJECT_TAG.
            push!(parts, quote
                if b == OBJECT_TAG
                    return ($(object_body))::$T
                end
            end)
        end
    end
end

# Consume a serialized value from the stream without interpreting it.
# Only handles tags that appear in type-data contexts (Symbol, Module, sertag values,
# backrefs, numeric types, tuples). Does NOT call handle_deserialize, making it trimming-safe.
function deserialize_consume_value!(s::AbstractSerializer, b::Int32)
    if b >= VALUE_TAGS
        # sertag value — no extra bytes
    elseif b == SYMBOL_TAG
        deserialize_symbol(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == LONGSYMBOL_TAG
        deserialize_symbol(s, Int(read(s.io, Int32)::Int32))
    elseif b == MODULE_TAG
        # Module data: sequence of serialized values terminated by EMPTYTUPLE_TAG.
        # Format: [uuid_or_nothing, root_name, submod1, submod2, ...] EMPTYTUPLE_TAG
        while true
            mb = Int32(read(s.io, UInt8)::UInt8)
            mb == EMPTYTUPLE_TAG && break
            deserialize_consume_value!(s, mb)
        end
    elseif is_backref_tag(b)
        read_backref_id(s, b)
    elseif b == 0
        read(s.io, UInt8)  # two-byte tag
    elseif b == INT8_TAG;       read(s.io, Int8)
    elseif b == INT8_TAG+1;     read(s.io, UInt8)
    elseif b == INT8_TAG+2;     read(s.io, Int16)
    elseif b == INT8_TAG+3;     read(s.io, UInt16)
    elseif b == INT32_TAG;      read(s.io, Int32)
    elseif b == INT8_TAG+5;     read(s.io, UInt32)
    elseif b == INT64_TAG;      read(s.io, Int64)
    elseif b == INT8_TAG+7;     read(s.io, UInt64)
    elseif b == INT8_TAG+8;     read(s.io, Int128)
    elseif b == INT8_TAG+9;     read(s.io, UInt128)
    elseif b == INT8_TAG+10;    read(s.io, Float16)
    elseif b == INT8_TAG+11;    read(s.io, Float32)
    elseif b == INT8_TAG+12;    read(s.io, Float64)
    elseif b == INT8_TAG+13;    read(s.io, Char)
    elseif b == SHORTINT64_TAG; read(s.io, Int32)
    elseif b == STRING_TAG
        deserialize_string(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == LONGSTRING_TAG
        deserialize_string(s, Int(read(s.io, Int64)::Int64))
    elseif b == EMPTYTUPLE_TAG
        # nothing
    elseif b == TUPLE_TAG
        n = Int(read(s.io, UInt8)::UInt8)
        for _ in 1:n
            deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))
        end
    elseif b == LONGTUPLE_TAG
        n = Int(read(s.io, Int32)::Int32)
        for _ in 1:n
            deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))
        end
    elseif b == DATATYPE_TAG
        # Nested DataType in type parameters — consume without getglobal.
        # Allocate a table slot and consume name + module.
        slot = s.counter; s.counter += 1
        deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # name
        deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # module
        s.table[slot] = nothing  # placeholder
    else
        throw(ArgumentError("unsupported tag in type data context"))
    end
    return nothing
end

# Consume a serialized TypeName from the stream (for FULL_DATATYPE_TAG and WRAPPER_DATATYPE_TAG).
function deserialize_consume_typename!(s::AbstractSerializer)
    tb = Int32(read(s.io, UInt8)::UInt8)
    if tb == TYPENAME_TAG
        number = read(s.io, UInt64)
        deserialize_typename(s, number)
    elseif is_backref_tag(tb)
        read_backref_id(s, tb)
    else
        throw(ArgumentError("expected TypeName tag in type data"))
    end
    return nothing
end

"""
    deserialize_read_type!(s::AbstractSerializer, ::Type{T}) -> nothing

Read the serialized DataType from the stream and validate it matches `T`.
Instead of calling `deserialize_datatype` (which uses `getglobal` to look up
the type by name — failing in trimmed executables where bindings are removed),
this reads the type data from the stream, discards it, and registers `T`
in the backref table (so later backrefs resolve correctly).

This function avoids calling `handle_deserialize` to remain trimming-compatible.
"""
@generated function deserialize_read_type!(s::AbstractSerializer, ::Type{T}) where T
    # Determine at generation time whether this type uses whole-type serialization.
    # Union{...} is serialized as a 1-byte sertag(Union) followed by
    # `serialize_fields` of its two members (`a` and `b`); because `T` is a
    # concrete Union at generation time, we recurse statically.
    whole = isa(T, DataType) && should_send_whole_type(nothing, T)
    # Common tail: backref variants, sertag-encoded types, and the 2-byte tag
    # escape. Union-valued `T` does not use DATATYPE_TAG, so WRAPPER/FULL
    # branches are omitted for it.
    tail = quote
        if is_backref_tag(tb)
            read_backref_id(s, tb)
        elseif tb >= VALUE_TAGS
            # sertag-encoded type
        elseif tb == 0
            read(s.io, UInt8)
        else
            throw(ArgumentError("unexpected tag when reading type"))
        end
    end
    if T isa Union
        head = quote
            if tb == $(Int32(sertag(Union)))
                deserialize_read_type!(s, $(T.a))
                deserialize_read_type!(s, $(T.b))
                return nothing
            end
        end
    elseif whole
        head = quote
            if tb == DATATYPE_TAG || tb == FULL_DATATYPE_TAG
                deserialize_skip_datatype!(s, $T, tb == FULL_DATATYPE_TAG)
                return nothing
            elseif tb == WRAPPER_DATATYPE_TAG
                deserialize_consume_typename!(s)
                return nothing
            end
        end
    else
        head = quote
            if tb == DATATYPE_TAG
                deserialize_skip_datatype!(s, $T, false)
                return nothing
            end
        end
    end
    return quote
        tb = Int32(read(s.io, UInt8)::UInt8)
        $head
        $tail
        return nothing
    end
end

"""
    deserialize_skip_datatype!(s, ::Type{T}, full::Bool) -> nothing

Read the serialized DataType data from the stream (matching the format
written by `serialize_type_data`), registering the known type `T` in the
backref table. The type data bytes are consumed from the stream but the
result is discarded — we already know `T`.

Uses `@generated` to emit specialized parameter-consumption code that
avoids `getglobal` for primitive type instance parameters (which would
fail in trimmed executables).
"""
@generated function deserialize_skip_datatype!(s::AbstractSerializer, ::Type{T}, full::Bool) where T
    whole = isa(T, DataType) && should_send_whole_type(nothing, T)

    param_reads = Expr[]
    if isa(T, DataType) && !isempty(T.parameters)
        iswrapper = T === unwrap_unionall(T.name.wrapper)
        if !iswrapper
            for i in 1:length(T.parameters)
                P = T.parameters[i]
                PT = typeof(P)
                if isprimitivetype(PT)
                    nbytes = sizeof(PT)
                    push!(param_reads, quote
                        let pb = Int32(read(s.io, UInt8)::UInt8)
                            if pb == OBJECT_TAG
                                deserialize_read_type!(s, $PT)
                                skip(s.io, $nbytes)
                            else
                                deserialize_consume_value!(s, pb)
                            end
                        end
                    end)
                elseif P isa Type
                    # Type-valued parameter — recurse via `deserialize_read_type!`,
                    # which knows `P`'s static shape and emits exactly the
                    # right reads (in particular, it knows whether `P` itself
                    # is parametric, so no wire-format ambiguity arises).
                    push!(param_reads, :(deserialize_read_type!(s, $P)))
                else
                    push!(param_reads, :(deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))))
                end
            end
        end
    end

    has_params = !isempty(param_reads)
    param_block = has_params ? Expr(:block,
        :(np = Int(read(s.io, Int32)::Int32)),
        param_reads...
    ) : :nothing

    if whole
        return quote
            slot = s.counter; s.counter += 1
            if full
                deserialize_consume_typename!(s)
            else
                deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # name
                deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # module
            end
            $param_block
            s.table[slot] = $T
            return nothing
        end
    else
        return quote
            slot = s.counter; s.counter += 1
            deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # name
            deserialize_consume_value!(s, Int32(read(s.io, UInt8)::UInt8))  # module
            $param_block
            s.table[slot] = $T
            return nothing
        end
    end
end

# default DataType deserializer
function deserialize(s::AbstractSerializer, t::DataType)
    nf = length(t.types)
    if isprimitivetype(t)
        return read(s.io, t)
    elseif ismutabletype(t)
        x = ccall(:jl_new_struct_uninit, Any, (Any,), t)
        deserialize_cycle(s, x)
        for i in 1:nf
            tag = Int32(read(s.io, UInt8)::UInt8)
            if tag != UNDEFREF_TAG
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), x, i-1, handle_deserialize(s, tag))
            end
        end
        return x
    elseif nf == 0
        return ccall(:jl_new_struct_uninit, Any, (Any,), t)
    else
        na = nf
        vflds = Vector{Any}(undef, nf)
        for i in 1:nf
            tag = Int32(read(s.io, UInt8)::UInt8)
            if tag != UNDEFREF_TAG
                f = handle_deserialize(s, tag)
                na >= i && (vflds[i] = f)
            else
                na >= i && (na = i - 1) # rest of tail must be undefined values
            end
        end
        return ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), t, vflds, na)
    end
end

function deserialize_dict(s::AbstractSerializer, T::Type{<:AbstractDict})
    n = read(s.io, Int32)
    t = T(); sizehint!(t, n)
    deserialize_cycle(s, t)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

deserialize(s::AbstractSerializer, T::Type{Dict{K,V}}) where {K,V} = deserialize_dict(s, T)

# Typed dict deserialization — uses deserialize_typed for keys and values to avoid
# unresolvable dynamic dispatch in trimmed executables. Uses `if @generated` so
# that, when `K` or `V` is a user-defined struct, its body is inlined via
# `emit_struct_value_expr` rather than emitting a cross-specialization invoke.
function deserialize_dict_typed(s::AbstractSerializer, ::Type{T}) where {K, V, T <: Dict{K, V}}
    if @generated
        kb = gensym(:kb); vb = gensym(:vb)
        key_expr = emit_field_read_expr(K, kb, Set{DataType}())
        val_expr = emit_field_read_expr(V, vb, Set{DataType}())
        return quote
            n = read(s.io, Int32)
            t = sizehint!($T(), n)
            deserialize_cycle(s, t)
            for i = 1:n
                $kb = Int32(read(s.io, UInt8)::UInt8)
                k = ($(key_expr))::$K
                $vb = Int32(read(s.io, UInt8)::UInt8)
                v = ($(val_expr))::$V
                t[k] = v
            end
            return t
        end
    else
        n = read(s.io, Int32)
        t = sizehint!(T(), n)
        deserialize_cycle(s, t)
        for i = 1:n
            kb = Int32(read(s.io, UInt8)::UInt8)
            k = deserialize_typed(s, kb, K)
            vb = Int32(read(s.io, UInt8)::UInt8)
            v = deserialize_typed(s, vb, V)
            t[k] = v
        end
        return t
    end
end

deserialize(s::AbstractSerializer, ::Type{BigInt}) = parse(BigInt, deserialize(s), base = BIGINT_BASE)

function deserialize(s::AbstractSerializer, t::Type{Regex})
    pattern = deserialize(s)
    compile_options = deserialize(s)
    match_options = deserialize(s)
    return Regex(pattern, compile_options, match_options)
end

## StackTraces

# provide a custom serializer that skips attempting to serialize the `outer_linfo`
# which is likely to contain complex references, types, and module references
# that may not exist on the receiver end
function serialize(s::AbstractSerializer, frame::Base.StackTraces.StackFrame)
    serialize_type(s, typeof(frame))
    serialize(s, frame.func)
    serialize(s, frame.file)
    write(s.io, frame.line)
    write(s.io, frame.from_c)
    write(s.io, frame.inlined)
    write(s.io, frame.pointer)
    nothing
end

function deserialize(s::AbstractSerializer, ::Type{Base.StackTraces.StackFrame})
    func = deserialize(s)
    file = deserialize(s)
    line = read(s.io, Int)
    from_c = read(s.io, Bool)
    inlined = read(s.io, Bool)
    pointer = read(s.io, UInt64)
    return Base.StackTraces.StackFrame(func, file, line, nothing, from_c, inlined, pointer)
end

function serialize(s::AbstractSerializer, lock::Base.AbstractLock)
    # assert_havelock(lock)
    serialize_cycle_header(s, lock)
    nothing
end

function deserialize(s::AbstractSerializer, ::Type{T}) where T<:Base.AbstractLock
    lock = T()
    deserialize_cycle(s, lock)
    return lock
end

function serialize(s::AbstractSerializer, cond::Base.GenericCondition)
    serialize_cycle_header(s, cond) && return
    serialize(s, cond.lock)
    nothing
end

function deserialize(s::AbstractSerializer, ::Type{T}) where T<:Base.GenericCondition
    lock = deserialize(s)
    cond = T(lock)
    deserialize_cycle(s, cond)
    return cond
end

serialize(s::AbstractSerializer, l::LazyString) =
    invoke(serialize, Tuple{AbstractSerializer,Any}, s, Base._LazyString((), string(l)))

end
