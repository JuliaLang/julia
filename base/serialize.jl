# This file is a part of Julia. License is MIT: https://julialang.org/license

module Serializer

import Base: GMP, Bottom, unsafe_convert, uncompressed_ast
import Core: svec
using Base: ViewIndex, Slice, index_lengths, unwrap_unionall

export serialize, deserialize, SerializationState

mutable struct SerializationState{I<:IO} <: AbstractSerializer
    io::I
    counter::Int
    table::ObjectIdDict
    pending_refs::Vector{Int}
    known_object_data::Dict{UInt64,Any}
    SerializationState{I}(io::I) where I<:IO = new(io, 0, ObjectIdDict(), Int[], Dict{UInt64,Any}())
end

SerializationState(io::IO) = SerializationState{typeof(io)}(io)

## serializing values ##

# types AbstractSerializer and Serializer  # defined in dict.jl

const n_int_literals = 33
const n_reserved_slots = 12

const TAGS = Any[
    Symbol, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128,
    Float16, Float32, Float64, Char, DataType, Union, UnionAll, TypeName, Tuple,
    Array, Expr, LineNumberNode, LabelNode, GotoNode, QuoteNode, CodeInfo, TypeVar,
    Core.Box, Core.MethodInstance, Module, Task, String, SimpleVector, Method,
    GlobalRef, SlotNumber, TypedSlot, NewvarNode, SSAValue,

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

    (), Bool, Any, Bottom, Core.TypeofBottom, Type, svec(), Tuple{}, false, true, nothing,
    :Any, :Array, :TypeVar, :Box, :Tuple, :Ptr, :return, :call, Symbol("::"), :Function,
    :(=), :(==), :(===), :gotoifnot, :A, :B, :C, :M, :N, :T, :S, :X, :Y, :a, :b, :c, :d, :e, :f,
    :g, :h, :i, :j, :k, :l, :m, :n, :o, :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z, :add_int,
    :sub_int, :mul_int, :add_float, :sub_float, :new, :mul_float, :bitcast, :start, :done, :next,
    :indexed_next, :getfield, :meta, :eq_int, :slt_int, :sle_int, :ne_int, :push_loc, :pop_loc,
    :pop, :arrayset, :arrayref, :apply_type, :inbounds, :getindex, :setindex!, :Core, :!, :+,
    :Base, :static_parameter, :convert, :colon, Symbol("#self#"), Symbol("#temp#"), :tuple,

    fill(:_reserved_, n_reserved_slots)...,

    (Int32(0):Int32(n_int_literals-1))...,
    (Int64(0):Int64(n_int_literals-1))...
]

@assert length(TAGS) <= 255

const ser_version = 6 # do not make changes without bumping the version #!

const NTAGS = length(TAGS)

function sertag(v::ANY)
    ptr = pointer_from_objref(v)
    ptags = convert(Ptr{Ptr{Void}}, pointer(TAGS))
    # note: constant ints & reserved slots never returned here
    @inbounds for i in 1:(NTAGS-(n_reserved_slots+2*n_int_literals))
        ptr == unsafe_load(ptags,i) && return i%Int32
    end
    return Int32(-1)
end
desertag(i::Int32) = TAGS[i]

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
const ARRAY_TAG = sertag(Array)
const EXPR_TAG = sertag(Expr)
const MODULE_TAG = sertag(Module)
const METHODINSTANCE_TAG = sertag(Core.MethodInstance)
const METHOD_TAG = sertag(Method)
const TASK_TAG = sertag(Task)
const DATATYPE_TAG = sertag(DataType)
const TYPENAME_TAG = sertag(TypeName)
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

writetag(s::IO, tag) = write(s, UInt8(tag))

function write_as_tag(s::IO, tag)
    tag < VALUE_TAGS && write(s, UInt8(0))
    write(s, UInt8(tag))
end

# cycle handling
function serialize_cycle(s::AbstractSerializer, x)
    offs = get(s.table, x, -1)
    if offs != -1
        if offs <= typemax(UInt16)
            writetag(s.io, SHORTBACKREF_TAG)
            write(s.io, UInt16(offs))
        elseif offs <= typemax(Int32)
            writetag(s.io, BACKREF_TAG)
            write(s.io, Int32(offs))
        else
            writetag(s.io, LONGBACKREF_TAG)
            write(s.io, Int64(offs))
        end
        return true
    end
    s.table[x] = s.counter
    s.counter += 1
    return false
end

function serialize_cycle_header(s::AbstractSerializer, x::ANY)
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

serialize(s::AbstractSerializer, x::Bool) = x ? writetag(s.io, TRUE_TAG) :
                                                writetag(s.io, FALSE_TAG)

serialize(s::AbstractSerializer, p::Ptr) = serialize_any(s, oftype(p, C_NULL))

serialize(s::AbstractSerializer, ::Tuple{}) = writetag(s.io, EMPTYTUPLE_TAG)

function serialize(s::AbstractSerializer, t::Tuple)
    l = length(t)
    if l <= 255
        writetag(s.io, TUPLE_TAG)
        write(s.io, UInt8(l))
    else
        writetag(s.io, LONGTUPLE_TAG)
        write(s.io, Int32(l))
    end
    for x in t
        serialize(s, x)
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
    if len <= 255
        writetag(s.io, SYMBOL_TAG)
        write(s.io, UInt8(len))
    else
        writetag(s.io, LONGSYMBOL_TAG)
        write(s.io, Int32(len))
    end
    unsafe_write(s.io, pname, len)
end

function serialize_array_data(s::IO, a)
    elty = eltype(a)
    if elty === Bool && !isempty(a)
        last = a[1]
        count = 1
        for i = 2:length(a)
            if a[i] != last || count == 127
                write(s, UInt8((UInt8(last) << 7) | count))
                last = a[i]
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

function serialize(s::AbstractSerializer, a::Array)
    serialize_cycle(s, a) && return
    elty = eltype(a)
    writetag(s.io, ARRAY_TAG)
    if elty !== UInt8
        serialize(s, elty)
    end
    if ndims(a) != 1
        serialize(s, size(a))
    else
        serialize(s, length(a))
    end
    if isbits(elty)
        serialize_array_data(s.io, a)
    else
        for i in eachindex(a)
            if isassigned(a, i)
                serialize(s, a[i])
            else
                writetag(s.io, UNDEFREF_TAG)
            end
        end
    end
end

function serialize(s::AbstractSerializer, a::SubArray{T,N,A}) where {T,N,A<:Array}
    b = trimmedsubarray(a)
    serialize_any(s, b)
end

function trimmedsubarray(V::SubArray{T,N,A}) where {T,N,A<:Array}
    dest = Array{eltype(V)}(trimmedsize(V))
    copy!(dest, V)
    _trimmedsubarray(dest, V, (), V.indexes...)
end

trimmedsize(V) = index_lengths(V.indexes...)

function _trimmedsubarray(A, V::SubArray{T,N,P,I,LD}, newindexes) where {T,N,P,I,LD}
    LD && return SubArray{T,N,P,I,LD}(A, newindexes, Base.compute_offset1(A, 1, newindexes), 1)
    SubArray{T,N,P,I,LD}(A, newindexes, 0, 0)
end
_trimmedsubarray(A, V, newindexes, index::ViewIndex, indexes...) = _trimmedsubarray(A, V, (newindexes..., trimmedindex(V.parent, length(newindexes)+1, index)), indexes...)

trimmedindex(P, d, i::Real) = oftype(i, 1)
trimmedindex(P, d, i::Colon) = i
trimmedindex(P, d, i::Slice) = i
trimmedindex(P, d, i::AbstractArray) = oftype(i, reshape(linearindices(i), indices(i)))

function serialize(s::AbstractSerializer, ss::String)
    len = sizeof(ss)
    if len <= 255
        writetag(s.io, STRING_TAG)
        write(s.io, UInt8(len))
    else
        writetag(s.io, LONGSTRING_TAG)
        write(s.io, Int64(len))
    end
    write(s.io, ss)
end

function serialize(s::AbstractSerializer, ss::SubString{T}) where T<:AbstractString
    # avoid saving a copy of the parent string, keeping the type of ss
    serialize_any(s, convert(SubString{T}, convert(T,ss)))
end

# Don't serialize the pointers
function serialize(s::AbstractSerializer, r::Regex)
    serialize_type(s, typeof(r))
    serialize(s, r.pattern)
    serialize(s, r.compile_options)
    serialize(s, r.match_options)
end

function serialize(s::AbstractSerializer, n::BigInt)
    serialize_type(s, BigInt)
    serialize(s, base(62,n))
end

function serialize(s::AbstractSerializer, n::BigFloat)
    serialize_type(s, BigFloat)
    serialize(s, string(n))
end

function serialize(s::AbstractSerializer, ex::Expr)
    serialize_cycle(s, ex) && return
    l = length(ex.args)
    if l <= 255
        writetag(s.io, EXPR_TAG)
        write(s.io, UInt8(l))
    else
        writetag(s.io, LONGEXPR_TAG)
        write(s.io, Int32(l))
    end
    serialize(s, ex.head)
    serialize(s, ex.typ)
    for a in ex.args
        serialize(s, a)
    end
end

function serialize(s::AbstractSerializer, d::Dict)
    serialize_cycle_header(s, d) && return
    write(s.io, Int32(length(d)))
    for (k,v) in d
        serialize(s, k)
        serialize(s, v)
    end
end

function serialize_mod_names(s::AbstractSerializer, m::Module)
    p = module_parent(m)
    if m !== p
        serialize_mod_names(s, p)
        serialize(s, module_name(m))
    end
end

function serialize(s::AbstractSerializer, m::Module)
    writetag(s.io, MODULE_TAG)
    serialize_mod_names(s, m)
    writetag(s.io, EMPTYTUPLE_TAG)
end

# TODO: make this bidirectional, so objects can be sent back via the same key
const object_numbers = WeakKeyDict()
const obj_number_salt = Ref(0)
function object_number(l::ANY)
    global obj_number_salt, object_numbers
    if haskey(object_numbers, l)
        return object_numbers[l]
    end
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    ln = obj_number_salt[]+(UInt64(myid())<<44)
    obj_number_salt[] += 1
    object_numbers[l] = ln
    return ln::UInt64
end

lookup_object_number(s::AbstractSerializer, n::UInt64) = nothing

remember_object(s::AbstractSerializer, o::ANY, n::UInt64) = nothing

function lookup_object_number(s::SerializationState, n::UInt64)
    return get(s.known_object_data, n, nothing)
end

function remember_object(s::SerializationState, o::ANY, n::UInt64)
    s.known_object_data[n] = o
    return nothing
end

function serialize(s::AbstractSerializer, meth::Method)
    serialize_cycle(s, meth) && return
    writetag(s.io, METHOD_TAG)
    write(s.io, object_number(meth))
    serialize(s, meth.module)
    serialize(s, meth.name)
    serialize(s, meth.file)
    serialize(s, meth.line)
    serialize(s, meth.sig)
    serialize(s, meth.sparam_syms)
    serialize(s, meth.ambig)
    serialize(s, meth.nargs)
    serialize(s, meth.isva)
    serialize(s, meth.isstaged)
    serialize(s, uncompressed_ast(meth, meth.source))
    nothing
end

function serialize(s::AbstractSerializer, linfo::Core.MethodInstance)
    serialize_cycle(s, linfo) && return
    isdefined(linfo, :def) && error("can only serialize toplevel MethodInstance objects")
    writetag(s.io, METHODINSTANCE_TAG)
    serialize(s, linfo.inferred)
    if isdefined(linfo, :inferred_const)
        serialize(s, linfo.inferred_const)
    else
        writetag(s.io, UNDEFREF_TAG)
    end
    serialize(s, linfo.sparam_vals)
    serialize(s, linfo.rettype)
    serialize(s, linfo.specTypes)
end

function serialize(s::AbstractSerializer, t::Task)
    serialize_cycle(s, t) && return
    if istaskstarted(t) && !istaskdone(t)
        error("cannot serialize a running Task")
    end
    state = [t.code,
        t.storage,
        t.state == :queued || t.state == :runnable ? (:runnable) : t.state,
        t.result,
        t.exception]
    writetag(s.io, TASK_TAG)
    for fld in state
        serialize(s, fld)
    end
end

function serialize(s::AbstractSerializer, g::GlobalRef)
    if g.mod === Main && isdefined(g.mod, g.name) && isconst(g.mod, g.name)
        v = getfield(g.mod, g.name)
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

function serialize(s::AbstractSerializer, t::TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)
    write(s.io, object_number(t))
    serialize_typename(s, t)
end

function serialize_typename(s::AbstractSerializer, t::TypeName)
    serialize(s, t.name)
    serialize(s, t.names)
    primary = unwrap_unionall(t.wrapper)
    serialize(s, primary.super)
    serialize(s, primary.parameters)
    serialize(s, primary.types)
    serialize(s, isdefined(primary, :instance))
    serialize(s, primary.abstract)
    serialize(s, primary.mutable)
    serialize(s, primary.ninitialized)
    if isdefined(t, :mt)
        serialize(s, t.mt.name)
        serialize(s, collect(Base.MethodList(t.mt)))
        serialize(s, t.mt.max_args)
        if isdefined(t.mt, :kwsorter)
            serialize(s, t.mt.kwsorter)
        else
            writetag(s.io, UNDEFREF_TAG)
        end
    else
        writetag(s.io, UNDEFREF_TAG)
    end
    nothing
end

# decide whether to send all data for a type (instead of just its name)
function should_send_whole_type(s, t::DataType)
    tn = t.name
    if isdefined(tn, :mt)
        # TODO improve somehow
        # send whole type for anonymous functions in Main
        name = tn.mt.name
        mod = tn.module
        isanonfunction = mod === Main && # only Main
            t.super === Function && # only Functions
            unsafe_load(unsafe_convert(Ptr{UInt8}, tn.name)) == UInt8('#') && # hidden type
            (!isdefined(mod, name) || t != typeof(getfield(mod, name))) # XXX: 95% accurate test for this being an inner function
            # TODO: more accurate test? (tn.name !== "#" name)
        #TODO: iskw = startswith(tn.name, "#kw#") && ???
        #TODO: iskw && return send-as-kwftype
        return mod === __deserialized_types__ || isanonfunction
    end
    return false
end

function serialize_type_data(s, t::DataType)
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
        tname = t.name.name
        serialize(s, tname)
        mod = t.name.module
        serialize(s, mod)
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

function serialize_type(s::AbstractSerializer, t::DataType, ref::Bool = false)
    tag = sertag(t)
    tag > 0 && return writetag(s.io, tag)
    writetag(s.io, ref ? REF_OBJECT_TAG : OBJECT_TAG)
    serialize_type_data(s, t)
end

function serialize(s::AbstractSerializer, n::Int32)
    if 0 <= n <= (n_int_literals-1)
        write(s.io, UInt8(ZERO32_TAG+n))
    else
        writetag(s.io, INT32_TAG)
        write(s.io, n)
    end
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

serialize(s::AbstractSerializer, x::ANY) = serialize_any(s, x)

function serialize_any(s::AbstractSerializer, x::ANY)
    tag = sertag(x)
    if tag > 0
        return write_as_tag(s.io, tag)
    end
    t = typeof(x)::DataType
    nf = nfields(t)
    if nf == 0 && t.size > 0
        serialize_type(s, t)
        write(s.io, x)
    else
        if t.mutable && nf > 0
            serialize_cycle(s, x) && return
            serialize_type(s, t, true)
        else
            serialize_type(s, t, false)
        end
        for i in 1:nf
            if isdefined(x, i)
                serialize(s, getfield(x, i))
            else
                writetag(s.io, UNDEFREF_TAG)
            end
        end
    end
end

serialize(s::IO, x) = serialize(SerializationState(s), x)

## deserializing values ##

deserialize(s::IO) = deserialize(SerializationState(s))

function deserialize(s::AbstractSerializer)
    handle_deserialize(s, Int32(read(s.io, UInt8)::UInt8))
end

function deserialize_cycle(s::AbstractSerializer, x::ANY)
    slot = pop!(s.pending_refs)
    s.table[slot] = x
    nothing
end

# optimized version of:
#     slot = s.counter; s.counter += 1
#     push!(s.pending_refs, slot)
#     slot = pop!(s.pending_refs)
#     s.table[slot] = x
function resolve_ref_immediately(s::AbstractSerializer, x::ANY)
    s.table[s.counter] = x
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
        return s.table[Int(id)]
    elseif b == BACKREF_TAG
        id = read(s.io, Int32)::Int32
        return s.table[Int(id)]
    elseif b == ARRAY_TAG
        return deserialize_array(s)
    elseif b == DATATYPE_TAG
        return deserialize_datatype(s, false)
    elseif b == FULL_DATATYPE_TAG
        return deserialize_datatype(s, true)
    elseif b == WRAPPER_DATATYPE_TAG
        tname = deserialize(s)::TypeName
        return unwrap_unionall(tname.wrapper)
    elseif b == OBJECT_TAG
        t = deserialize(s)
        return deserialize(s, t)
    elseif b == REF_OBJECT_TAG
        slot = s.counter; s.counter += 1
        push!(s.pending_refs, slot)
        t = deserialize(s)
        return deserialize(s, t)
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
        return s.table[Int(id)]
    elseif b == LONGSYMBOL_TAG
        return deserialize_symbol(s, Int(read(s.io, Int32)::Int32))
    end
    t = desertag(b)
    if t.mutable && nfields(t) > 0
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

deserialize_tuple(s::AbstractSerializer, len) = ntuple(i->deserialize(s), len)

function deserialize_svec(s::AbstractSerializer)
    n = read(s.io, Int32)
    svec(Any[ deserialize(s) for i=1:n ]...)
end

function deserialize_module(s::AbstractSerializer)
    path = deserialize(s)
    m = Main
    if isa(path,Tuple) && path !== ()
        # old version
        for mname in path
            m = getfield(m,mname)::Module
        end
    else
        mname = path
        while mname !== ()
            m = getfield(m,mname)::Module
            mname = deserialize(s)
        end
    end
    m
end

function deserialize(s::AbstractSerializer, ::Type{Method})
    lnumber = read(s.io, UInt64)
    meth = lookup_object_number(s, lnumber)
    if meth !== nothing
        meth = meth::Method
        makenew = false
    else
        meth = ccall(:jl_new_method_uninit, Ref{Method}, ())
        makenew = true
    end
    deserialize_cycle(s, meth)
    mod = deserialize(s)::Module
    name = deserialize(s)::Symbol
    file = deserialize(s)::Symbol
    line = deserialize(s)::Int32
    sig = deserialize(s)::DataType
    sparam_syms = deserialize(s)::SimpleVector
    ambig = deserialize(s)::Union{Array{Any,1}, Void}
    nargs = deserialize(s)::Int32
    isva = deserialize(s)::Bool
    isstaged = deserialize(s)::Bool
    template = deserialize(s)::CodeInfo
    if makenew
        meth.module = mod
        meth.name = name
        meth.file = file
        meth.line = line
        meth.sig = sig
        meth.sparam_syms = sparam_syms
        meth.ambig = ambig
        meth.isstaged = isstaged
        meth.nargs = nargs
        meth.isva = isva
        # TODO: compress template
        meth.source = template
        meth.pure = template.pure
        if isstaged
            linfo = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ())
            linfo.specTypes = Tuple
            linfo.inferred = template
            meth.generator = linfo
        end
        ftype = ccall(:jl_first_argument_datatype, Any, (Any,), sig)::DataType
        if isdefined(ftype.name, :mt) && nothing === ccall(:jl_methtable_lookup, Any, (Any, Any, UInt), ftype.name.mt, sig, typemax(UInt))
            ccall(:jl_method_table_insert, Void, (Any, Any, Ptr{Void}), ftype.name.mt, meth, C_NULL)
        end
        remember_object(s, meth, lnumber)
    end
    return meth
end

function deserialize(s::AbstractSerializer, ::Type{Core.MethodInstance})
    linfo = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, (Ptr{Void},), C_NULL)
    deserialize_cycle(s, linfo)
    linfo.inferred = deserialize(s)::CodeInfo
    tag = Int32(read(s.io, UInt8)::UInt8)
    if tag != UNDEFREF_TAG
        linfo.inferred_const = handle_deserialize(s, tag)
    end
    linfo.sparam_vals = deserialize(s)::SimpleVector
    linfo.rettype = deserialize(s)
    linfo.specTypes = deserialize(s)
    return linfo
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
    if isa(d1, Integer)
        if elty !== Bool && isbits(elty)
            a = Array{elty, 1}(d1)
            s.table[slot] = a
            return read!(s.io, a)
        end
        dims = (Int(d1),)
    else
        dims = convert(Dims, d1)::Dims
    end
    if isbits(elty)
        n = prod(dims)::Int
        if elty === Bool && n > 0
            A = Array{Bool, length(dims)}(dims)
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
            A = read(s.io, elty, dims)
        end
        s.table[slot] = A
        return A
    end
    A = Array{elty, length(dims)}(dims)
    s.table[slot] = A
    for i = eachindex(A)
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

function deserialize_expr(s::AbstractSerializer, len)
    e = Expr(:temp)
    resolve_ref_immediately(s, e)
    e.head = deserialize(s)::Symbol
    ty = deserialize(s)
    e.args = Any[ deserialize(s) for i = 1:len ]
    e.typ = ty
    e
end

module __deserialized_types__ end

function deserialize(s::AbstractSerializer, ::Type{TypeName})
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
        tn = ccall(:jl_new_typename_in, Ref{TypeName}, (Any, Any),
                   tn_name, __deserialized_types__)
        makenew = true
    end
    remember_object(s, tn, number)
    deserialize_cycle(s, tn)

    names = deserialize(s)::SimpleVector
    super = deserialize(s)::Type
    parameters = deserialize(s)::SimpleVector
    types = deserialize(s)::SimpleVector
    has_instance = deserialize(s)::Bool
    abstr = deserialize(s)::Bool
    mutabl = deserialize(s)::Bool
    ninitialized = deserialize(s)::Int32

    if makenew
        tn.names = names
        # TODO: there's an unhanded cycle in the dependency graph at this point:
        # while deserializing super and/or types, we may have encountered
        # tn.wrapper and throw UndefRefException before we get to this point
        ndt = ccall(:jl_new_datatype, Any, (Any, Any, Any, Any, Any, Cint, Cint, Cint),
                    tn, super, parameters, names, types,
                    abstr, mutabl, ninitialized)
        tn.wrapper = ndt.name.wrapper
        ccall(:jl_set_const, Void, (Any, Any, Any), tn.module, tn.name, tn.wrapper)
        ty = tn.wrapper
        if has_instance && !isdefined(ty, :instance)
            # use setfield! directly to avoid `fieldtype` lowering expecting to see a Singleton object already on ty
            Core.setfield!(ty, :instance, ccall(:jl_new_struct, Any, (Any, Any...), ty))
        end
    end

    tag = Int32(read(s.io, UInt8)::UInt8)
    if tag != UNDEFREF_TAG
        mtname = handle_deserialize(s, tag)
        defs = deserialize(s)
        maxa = deserialize(s)::Int
        if makenew
            tn.mt = ccall(:jl_new_method_table, Any, (Any, Any), name, tn.module)
            tn.mt.name = mtname
            tn.mt.max_args = maxa
            for def in defs
                if isdefined(def, :sig)
                    ccall(:jl_method_table_insert, Void, (Any, Any, Ptr{Void}), tn.mt, def, C_NULL)
                end
            end
        end
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            kws = handle_deserialize(s, tag)
            if makenew
                tn.mt.kwsorter = kws
            end
        end
    end
    return tn::TypeName
end

function deserialize_datatype(s::AbstractSerializer, full::Bool)
    slot = s.counter; s.counter += 1
    if full
        tname = deserialize(s)::TypeName
        ty = tname.wrapper
    else
        name = deserialize(s)::Symbol
        mod = deserialize(s)::Module
        ty = getfield(mod,name)
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
    s.table[slot] = t
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
    t.state = deserialize(s)
    t.result = deserialize(s)
    t.exception = deserialize(s)
    t
end

function deserialize_string(s::AbstractSerializer, len::Int)
    out = ccall(:jl_alloc_string, Ref{String}, (Csize_t,), len)
    unsafe_read(s.io, pointer(out), len)
    return out
end

# default DataType deserializer
function deserialize(s::AbstractSerializer, t::DataType)
    nf = nfields(t)
    if nf == 0 && t.size > 0
        # bits type
        return read(s.io, t)
    end
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    elseif isbits(t)
        if nf == 1
            f1 = deserialize(s)
            return ccall(:jl_new_struct, Any, (Any,Any...), t, f1)
        elseif nf == 2
            f1 = deserialize(s)
            f2 = deserialize(s)
            return ccall(:jl_new_struct, Any, (Any,Any...), t, f1, f2)
        elseif nf == 3
            f1 = deserialize(s)
            f2 = deserialize(s)
            f3 = deserialize(s)
            return ccall(:jl_new_struct, Any, (Any,Any...), t, f1, f2, f3)
        else
            flds = Any[ deserialize(s) for i = 1:nf ]
            return ccall(:jl_new_structv, Any, (Any,Ptr{Void},UInt32), t, flds, nf)
        end
    else
        x = ccall(:jl_new_struct_uninit, Any, (Any,), t)
        t.mutable && deserialize_cycle(s, x)
        for i in 1:nf
            tag = Int32(read(s.io, UInt8)::UInt8)
            if tag != UNDEFREF_TAG
                ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), x, i-1, handle_deserialize(s, tag))
            end
        end
        return x
    end
end

function deserialize(s::AbstractSerializer, T::Type{Dict{K,V}}) where {K,V}
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

deserialize(s::AbstractSerializer, ::Type{BigFloat}) = parse(BigFloat, deserialize(s))

deserialize(s::AbstractSerializer, ::Type{BigInt}) = parse(BigInt, deserialize(s), 62)

function deserialize(s::AbstractSerializer, t::Type{Regex})
    pattern = deserialize(s)
    compile_options = deserialize(s)
    match_options = deserialize(s)
    Regex(pattern, compile_options, match_options)
end

if !is_windows()
    function serialize(s::AbstractSerializer, rd::RandomDevice)
        serialize_type(s, typeof(rd))
        serialize(s, rd.unlimited)
    end
    function deserialize(s::AbstractSerializer, t::Type{RandomDevice})
        unlimited = deserialize(s)
        return RandomDevice(unlimited)
    end
end
end
