# This file is a part of Julia. License is MIT: http://julialang.org/license

module Serializer

import Base: GMP, Bottom, svec, unsafe_convert, uncompressed_ast
using Base: ViewIndex, index_lengths

export serialize, deserialize

## serializing values ##

# type SerializationState  # defined in dict.jl

const TAGS = Any[
    Symbol, Int8, UInt8, Int16, UInt16, Int32, UInt32,
    Int64, UInt64, Int128, UInt128, Float32, Float64, Char, Ptr,
    DataType, Union, TypeName,
    Tuple, Array, Expr,
    #LongSymbol, LongTuple, LongExpr,
    Symbol, Tuple, Expr,  # dummy entries, intentionally shadowed by earlier ones
    LineNumberNode, Slot, LabelNode, GotoNode,
    QuoteNode, TopNode, TypeVar, Core.Box, LambdaInfo,
    Module, #=UndefRefTag=#Symbol, Task, ASCIIString, UTF8String,
    UTF16String, UTF32String, Float16,
    SimpleVector, #=BackrefTag=#Symbol, :reserved11, :reserved12,

    (), Bool, Any, :Any, Bottom, :reserved21, :reserved22, Type,
    :Array, :TypeVar, :Box,
    :lambda, :body, :return, :call, symbol("::"),
    :(=), :null, :gotoifnot, :A, :B, :C, :M, :N, :T, :S, :X, :Y,
    :a, :b, :c, :d, :e, :f, :g, :h, :i, :j, :k, :l, :m, :n, :o,
    :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z,
    :add_int, :sub_int, :mul_int, :add_float, :sub_float,
    :mul_float, :unbox, :box,
    :eq_int, :slt_int, :sle_int, :ne_int,
    :arrayset, :arrayref,
    :Core, :Base, svec(), Tuple{},
    :reserved17, :reserved18, :reserved19, :reserved20,
    false, true, nothing, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
    28, 29, 30, 31, 32
]

const ser_version = 3 # do not make changes without bumping the version #!

const NTAGS = length(TAGS)

function sertag(v::ANY)
    ptr = pointer_from_objref(v)
    ptags = convert(Ptr{Ptr{Void}}, pointer(TAGS))
    @inbounds for i in 1:NTAGS
        ptr == unsafe_load(ptags,i) && return (i+1)%Int32
    end
    return Int32(-1)
end
desertag(i::Int32) = TAGS[i-1]

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = sertag(())
const ZERO_TAG = sertag(0)
const TRUE_TAG = sertag(true)
const FALSE_TAG = sertag(false)
const EMPTYTUPLE_TAG = sertag(())
const TUPLE_TAG = sertag(Tuple)
const LONGTUPLE_TAG = Int32(sertag(Expr)+2)
const SIMPLEVECTOR_TAG = sertag(SimpleVector)
const SYMBOL_TAG = sertag(Symbol)
const LONGSYMBOL_TAG = Int32(sertag(Expr)+1)
const ARRAY_TAG = sertag(Array)
const UNDEFREF_TAG = Int32(sertag(Module)+1)
const BACKREF_TAG = Int32(sertag(SimpleVector)+1)
const EXPR_TAG = sertag(Expr)
const LONGEXPR_TAG = Int32(sertag(Expr)+3)
const MODULE_TAG = sertag(Module)
const LAMBDASTATICDATA_TAG = sertag(LambdaInfo)
const TASK_TAG = sertag(Task)
const DATATYPE_TAG = sertag(DataType)
const TYPENAME_TAG = sertag(TypeName)
const INT_TAG = sertag(Int)

writetag(s::IO, tag) = write(s, UInt8(tag))

function write_as_tag(s::IO, tag)
    tag < VALUE_TAGS && write(s, UInt8(0))
    write(s, UInt8(tag))
end

# cycle handling
function serialize_cycle(s::SerializationState, x)
    if !isimmutable(x) && !typeof(x).pointerfree
        offs = get(s.table, x, -1)
        if offs != -1
            writetag(s.io, BACKREF_TAG)
            write(s.io, Int(offs))
            return true
        end
        s.table[x] = s.counter
        s.counter += 1
    end
    return false
end

serialize(s::SerializationState, x::Bool) = x ? writetag(s.io, TRUE_TAG) :
                                                writetag(s.io, FALSE_TAG)

serialize(s::SerializationState, p::Ptr) = serialize_any(s, oftype(p, C_NULL))

serialize(s::SerializationState, ::Tuple{}) = writetag(s.io, EMPTYTUPLE_TAG)

function serialize(s::SerializationState, t::Tuple)
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

function serialize(s::SerializationState, v::SimpleVector)
    writetag(s.io, SIMPLEVECTOR_TAG)
    write(s.io, Int32(length(v)))
    for x in v
        serialize(s, x)
    end
end

function serialize(s::SerializationState, x::Symbol)
    tag = sertag(x)
    if tag > 0
        return write_as_tag(s.io, tag)
    end
    pname = unsafe_convert(Ptr{UInt8}, x)
    ln = Int(ccall(:strlen, Csize_t, (Cstring,), pname))
    if ln <= 255
        writetag(s.io, SYMBOL_TAG)
        write(s.io, UInt8(ln))
    else
        writetag(s.io, LONGSYMBOL_TAG)
        write(s.io, Int32(ln))
    end
    unsafe_write(s.io, pname, ln)
end

function serialize_array_data(s::IO, a)
    elty = eltype(a)
    if elty === Bool && !isempty(a)
        last = a[1]
        count = 1
        for i = 2:length(a)
            if a[i] != last || count == 127
                write(s, UInt8((UInt8(last)<<7) | count))
                last = a[i]
                count = 1
            else
                count += 1
            end
        end
        write(s, UInt8((UInt8(last)<<7) | count))
    else
        write(s, a)
    end
end

function serialize(s::SerializationState, a::Array)
    elty = eltype(a)
    if !isbits(elty)
        # This is subtle: whether Arrays are put in the table depends on
        # the eltype, so we need to be able to deserialize the eltype first.
        # However deserializing the eltype might also use the table.
        offs = get(s.table, a, -1)
        if offs != -1
            writetag(s.io, BACKREF_TAG)
            write(s.io, Int(offs))
            return
        end
    end
    writetag(s.io, ARRAY_TAG)
    if elty !== UInt8
        serialize(s, elty)
    end
    if !isbits(elty)
        s.table[a] = s.counter
        s.counter += 1
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
            if isdefined(a, i)
                serialize(s, a[i])
            else
                writetag(s.io, UNDEFREF_TAG)
            end
        end
    end
end

function serialize{T,N,A<:Array}(s::SerializationState, a::SubArray{T,N,A})
    b = trimmedsubarray(a)
    serialize_any(s, b)
end

function trimmedsubarray{T,N,A<:Array}(V::SubArray{T,N,A})
    dest = Array(eltype(V), trimmedsize(V))
    copy!(dest, V)
    _trimmedsubarray(dest, V, (), V.indexes...)
end

trimmedsize(V) = index_lengths(V.parent, V.indexes...)

_trimmedsubarray{T,N,P,I,LD}(A, V::SubArray{T,N,P,I,LD}, newindexes) = SubArray{T,N,P,I,LD}(A, newindexes, size(V), 1, 1)
_trimmedsubarray(A, V, newindexes, index::ViewIndex, indexes...) = _trimmedsubarray(A, V, (newindexes..., trimmedindex(V.parent, length(newindexes)+1, index)), indexes...)

trimmedindex(P, d, i::Real) = oftype(i, 1)
trimmedindex(P, d, i::Colon) = i
trimmedindex(P, d, i::AbstractVector) = oftype(i, 1:length(i))

function serialize{T<:AbstractString}(s::SerializationState, ss::SubString{T})
    # avoid saving a copy of the parent string, keeping the type of ss
    serialize_any(s, convert(SubString{T}, convert(T,ss)))
end

# Don't serialize the pointers
function serialize(s::SerializationState, r::Regex)
    serialize_type(s, typeof(r))
    serialize(s, r.pattern)
    serialize(s, r.compile_options)
    serialize(s, r.match_options)
end

function serialize(s::SerializationState, n::BigInt)
    serialize_type(s, BigInt)
    serialize(s, base(62,n))
end

function serialize(s::SerializationState, n::BigFloat)
    serialize_type(s, BigFloat)
    serialize(s, string(n))
end

function serialize(s::SerializationState, ex::Expr)
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

function serialize(s::SerializationState, t::Dict)
    serialize_cycle(s, t) && return
    serialize_type(s, typeof(t))
    write(s.io, Int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function serialize_mod_names(s::SerializationState, m::Module)
    p = module_parent(m)
    if m !== p
        serialize_mod_names(s, p)
        serialize(s, module_name(m))
    end
end

function serialize(s::SerializationState, m::Module)
    writetag(s.io, MODULE_TAG)
    serialize_mod_names(s, m)
    writetag(s.io, EMPTYTUPLE_TAG)
end

# TODO: make this bidirectional, so objects can be sent back via the same key
const object_numbers = WeakKeyDict()
obj_number_salt = 0
function object_number(l::ANY)
    global obj_number_salt, object_numbers
    if haskey(object_numbers, l)
        return object_numbers[l]
    end
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    ln = obj_number_salt+(UInt64(myid())<<44)
    obj_number_salt += 1
    object_numbers[l] = ln
    return ln
end

function serialize(s::SerializationState, linfo::LambdaInfo)
    serialize_cycle(s, linfo) && return
    writetag(s.io, LAMBDASTATICDATA_TAG)
    serialize(s, object_number(linfo))
    serialize(s, uncompressed_ast(linfo))
    if isdefined(linfo.def, :roots)
        serialize(s, linfo.def.roots::Vector{Any})
    else
        serialize(s, Any[])
    end
    serialize(s, linfo.sparam_syms)
    serialize(s, linfo.sparam_vals)
    serialize(s, linfo.inferred)
    serialize(s, linfo.module)
    serialize(s, linfo.name)
    serialize(s, linfo.file)
    serialize(s, linfo.line)
    serialize(s, linfo.pure)
end

function serialize(s::SerializationState, t::Task)
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

function serialize(s::SerializationState, t::TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)
    serialize(s, object_number(t))
    serialize(s, t.name)
    serialize(s, t.module)
    serialize(s, t.names)
    serialize(s, t.primary.super)
    serialize(s, t.primary.parameters)
    serialize(s, t.primary.types)
    serialize(s, t.primary.size)
    serialize(s, t.primary.abstract)
    serialize(s, t.primary.mutable)
    serialize(s, t.primary.ninitialized)
    if isdefined(t, :mt)
        serialize(s, t.mt.name)
        serialize(s, t.mt.defs)
        serialize(s, t.mt.max_args)
        if isdefined(t.mt, :kwsorter)
            serialize(s, t.mt.kwsorter)
        else
            writetag(s.io, UNDEFREF_TAG)
        end
    else
        writetag(s.io, UNDEFREF_TAG)
    end
end

# decide whether to send all data for a type (instead of just its name)
function should_send_whole_type(s, t::ANY)
    tn = t.name
    if isdefined(tn, :mt)
        # TODO improve somehow
        # send whole type for anonymous functions in Main
        fname = tn.mt.name
        mod = tn.module
        toplevel = isdefined(mod, fname) && isdefined(t, :instance) &&
            getfield(mod, fname) === t.instance
        ishidden = unsafe_load(unsafe_convert(Ptr{UInt8}, fname))==UInt8('#')
        return mod === __deserialized_types__ || (mod === Main && (ishidden || !toplevel))
    end
    return false
end

# `type_itself` means we are serializing a type object. when it's false, we are
# sending the type tag part of some other object's representation.
function serialize_type_data(s, t::ANY, type_itself::Bool)
    whole = should_send_whole_type(s, t)
    form = type_itself ? UInt8(0) : UInt8(1)
    if whole
        form |= UInt8(2)
    end
    writetag(s.io, DATATYPE_TAG)
    write(s.io, form)
    if whole
        serialize(s, t.name)
    else
        tname = t.name.name
        serialize(s, tname)
        mod = t.name.module
        serialize(s, mod)
    end
    if !isempty(t.parameters)
        if (whole ? (t === t.name.primary) : (isdefined(mod,tname) && t === getfield(mod,tname)))
            serialize(s, svec())
        else
            serialize(s, t.parameters)
        end
    end
end

function serialize(s::SerializationState, t::DataType)
    tag = sertag(t)
    tag > 0 && return write_as_tag(s.io, tag)
    serialize_type_data(s, t, true)
end

function serialize_type(s::SerializationState, t::DataType)
    tag = sertag(t)
    tag > 0 && return writetag(s.io, tag)
    serialize_type_data(s, t, false)
end

function serialize(s::SerializationState, n::Int)
    if 0 <= n <= 32
        write(s.io, UInt8(ZERO_TAG+n))
        return
    end
    writetag(s.io, INT_TAG)
    write(s.io, n)
end

serialize(s::SerializationState, x::ANY) = serialize_any(s, x)

function serialize_any(s::SerializationState, x::ANY)
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
        t.mutable && haskey(s.table, x) && serialize_cycle(s, x) && return
        serialize_type(s, t)
        t.mutable && serialize_cycle(s, x)
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

function deserialize(s::SerializationState)
    handle_deserialize(s, Int32(read(s.io, UInt8)::UInt8))
end

function deserialize_cycle(s::SerializationState, x::ANY)
    if !isimmutable(x) && !typeof(x).pointerfree
        s.table[s.counter] = x
        s.counter += 1
    end
    nothing
end

# deserialize_ is an internal function to dispatch on the tag
# describing the serialized representation. the number of
# representations is fixed, so deserialize_ does not get extended.
function handle_deserialize(s::SerializationState, b::Int32)
    if b == 0
        return desertag(Int32(read(s.io, UInt8)::UInt8))
    end
    if b >= VALUE_TAGS
        return desertag(b)
    elseif b == TUPLE_TAG
        return deserialize_tuple(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == LONGTUPLE_TAG
        return deserialize_tuple(s, Int(read(s.io, Int32)::Int32))
    elseif b == BACKREF_TAG
        id = read(s.io, Int)::Int
        return s.table[id]
    elseif b == ARRAY_TAG
        return deserialize_array(s)
    elseif b == DATATYPE_TAG
        return deserialize_datatype(s)
    elseif b == SYMBOL_TAG
        return symbol(read(s.io, UInt8, Int(read(s.io, UInt8)::UInt8)))
    elseif b == LONGSYMBOL_TAG
        return symbol(read(s.io, UInt8, Int(read(s.io, Int32)::Int32)))
    elseif b == EXPR_TAG
        return deserialize_expr(s, Int(read(s.io, UInt8)::UInt8))
    elseif b == LONGEXPR_TAG
        return deserialize_expr(s, Int(read(s.io, Int32)::Int32))
    end
    return deserialize(s, desertag(b))
end

deserialize_tuple(s::SerializationState, len) = ntuple(i->deserialize(s), len)

function deserialize(s::SerializationState, ::Type{SimpleVector})
    n = read(s.io, Int32)
    svec([ deserialize(s) for i=1:n ]...)
end

function deserialize(s::SerializationState, ::Type{Module})
    path = deserialize(s)
    m = Main
    if isa(path,Tuple) && path !== ()
        # old version
        for mname in path
            if !isdefined(m,mname)
                warn("Module $mname not defined on process $(myid())")  # an error seemingly fails
            end
            m = getfield(m,mname)::Module
        end
    else
        mname = path
        while mname !== ()
            if !isdefined(m,mname)
                warn("Module $mname not defined on process $(myid())")  # an error seemingly fails
            end
            m = getfield(m,mname)::Module
            mname = deserialize(s)
        end
    end
    m
end

const known_object_data = Dict()

function deserialize(s::SerializationState, ::Type{LambdaInfo})
    lnumber = deserialize(s)
    if haskey(known_object_data, lnumber)
        linfo = known_object_data[lnumber]::LambdaInfo
        makenew = false
    else
        linfo = ccall(:jl_new_lambda_info, Any, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}), C_NULL, C_NULL, C_NULL, C_NULL)::LambdaInfo
        makenew = true
    end
    deserialize_cycle(s, linfo)
    ast = deserialize(s)::Expr
    roots = deserialize(s)::Vector{Any}
    sparam_syms = deserialize(s)::SimpleVector
    sparam_vals = deserialize(s)::SimpleVector
    infr = deserialize(s)::Bool
    mod = deserialize(s)::Module
    name = deserialize(s)
    file = deserialize(s)
    line = deserialize(s)
    pure = deserialize(s)
    if makenew
        linfo.module = mod
        linfo.sparam_syms = sparam_syms
        linfo.sparam_vals = sparam_vals
        ccall(:jl_lambda_info_set_ast, Void, (Any, Any), linfo, ast)
        linfo.inferred = infr
        linfo.roots = roots
        linfo.name = name
        linfo.file = file
        linfo.line = line
        linfo.pure = pure
        known_object_data[lnumber] = linfo
    end
    return linfo
end

function deserialize_array(s::SerializationState)
    d1 = deserialize(s)
    if isa(d1,Type)
        elty = d1
        d1 = deserialize(s)
    else
        elty = UInt8
    end
    if isa(d1,Integer)
        if elty !== Bool && isbits(elty)
            return read!(s.io, Array(elty, d1))
        end
        dims = (Int(d1),)
    else
        dims = convert(Dims, d1)::Dims
    end
    if isbits(elty)
        n = prod(dims)::Int
        if elty === Bool && n>0
            A = Array(Bool, dims)
            i = 1
            while i <= n
                b = read(s.io, UInt8)::UInt8
                v = (b>>7) != 0
                count = b&0x7f
                nxt = i+count
                while i < nxt
                    A[i] = v; i+=1
                end
            end
        else
            A = read(s.io, elty, dims)
        end
        return A
    end
    A = Array(elty, dims)
    deserialize_cycle(s, A)
    for i = eachindex(A)
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

function deserialize_expr(s::SerializationState, len)
    hd = deserialize(s)::Symbol
    e = Expr(hd)
    deserialize_cycle(s, e)
    ty = deserialize(s)
    e.args = Any[ deserialize(s) for i=1:len ]
    e.typ = ty
    e
end

function deserialize(s::SerializationState, ::Type{Union})
    types = deserialize(s)
    Union{types...}
end

module __deserialized_types__
end

function deserialize(s::SerializationState, ::Type{TypeName})
    number = deserialize(s)
    name = deserialize(s)
    mod = deserialize(s)
    if haskey(known_object_data, number)
        tn = known_object_data[number]::TypeName
        name = tn.name
        mod = tn.module
        makenew = false
    elseif isdefined(mod, name)
        tn = getfield(mod, name).name
        # TODO: confirm somehow that the types match
        name = tn.name
        mod = tn.module
        makenew = false
    else
        name = gensym()
        mod = __deserialized_types__
        tn = ccall(:jl_new_typename_in, Any, (Any, Any), name, mod)
        makenew = true
    end
    deserialize_cycle(s, tn)

    names = deserialize(s)
    super = deserialize(s)
    parameters = deserialize(s)
    types = deserialize(s)
    size = deserialize(s)
    abstr = deserialize(s)
    mutable = deserialize(s)
    ninitialized = deserialize(s)

    if makenew
        tn.names = names
        tn.primary = ccall(:jl_new_datatype, Any, (Any, Any, Any, Any, Any, Cint, Cint, Cint),
                           tn, super, parameters, names, types,
                           abstr, mutable, ninitialized)
        known_object_data[number] = tn
        ty = tn.primary
        ccall(:jl_set_const, Void, (Any, Any, Any), mod, name, ty)
        if !isdefined(ty,:instance)
            if isempty(parameters) && !abstr && size == 0 && (!mutable || isempty(names))
                setfield!(ty, :instance, ccall(:jl_new_struct, Any, (Any,Any...), ty))
            end
        end
    end
    tag = Int32(read(s.io, UInt8)::UInt8)
    if tag != UNDEFREF_TAG
        mtname = handle_deserialize(s, tag)
        defs = deserialize(s)
        maxa = deserialize(s)
        if makenew
            tn.mt = ccall(:jl_new_method_table, Any, (Any, Any), name, mod)
            tn.mt.name = mtname
            tn.mt.defs = defs
            tn.mt.max_args = maxa
        end
        tag = Int32(read(s.io, UInt8)::UInt8)
        if tag != UNDEFREF_TAG
            kws = handle_deserialize(s, tag)
            if makenew
                tn.mt.kwsorter = kws
            end
        end
    end

    return tn
end

function deserialize_datatype(s::SerializationState)
    form = read(s.io, UInt8)::UInt8
    if (form&2) != 0
        tname = deserialize(s)::TypeName
        ty = tname.primary
    else
        name = deserialize(s)::Symbol
        mod = deserialize(s)::Module
        ty = getfield(mod,name)
    end
    assert(isa(ty,DataType))
    if isempty(ty.parameters)
        t = ty
    else
        params = deserialize(s)
        t = ty{params...}
    end
    if (form&1) == 0
        return t
    end
    deserialize(s, t)
end

function deserialize(s::SerializationState, ::Type{Task})
    t = Task(()->nothing)
    deserialize_cycle(s, t)
    t.code = deserialize(s)
    t.storage = deserialize(s)
    t.state = deserialize(s)
    t.result = deserialize(s)
    t.exception = deserialize(s)
    t
end

# default DataType deserializer
function deserialize(s::SerializationState, t::DataType)
    nf = nfields(t)
    if nf == 0 && t.size > 0
        # bits type
        return read(s.io, t)
    end
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    elseif isbits(t)
        if nf == 1
            return ccall(:jl_new_struct, Any, (Any,Any...), t, deserialize(s))
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

function deserialize{K,V}(s::SerializationState, T::Type{Dict{K,V}})
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

deserialize(s::SerializationState, ::Type{BigFloat}) = parse(BigFloat, deserialize(s))

deserialize(s::SerializationState, ::Type{BigInt}) = get(GMP.tryparse_internal(BigInt, deserialize(s), 62, true))

function deserialize(s::SerializationState, t::Type{Regex})
    pattern = deserialize(s)
    compile_options = deserialize(s)
    match_options = deserialize(s)
    Regex(pattern, compile_options, match_options)
end

end
