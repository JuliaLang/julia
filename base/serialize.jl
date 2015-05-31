# This file is a part of Julia. License is MIT: http://julialang.org/license

module Serializer

import Base: GMP, Bottom, svec, unsafe_convert, uncompressed_ast

export serialize, deserialize

## serializing values ##

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr
abstract UndefRefTag

const TAGS = Any[
    Symbol, Int8, UInt8, Int16, UInt16, Int32, UInt32,
    Int64, UInt64, Int128, UInt128, Float32, Float64, Char, Ptr,
    DataType, UnionType, Function,
    Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,
    LineNumberNode, SymbolNode, LabelNode, GotoNode,
    QuoteNode, TopNode, TypeVar, Box, LambdaStaticData,
    Module, UndefRefTag, Task, ASCIIString, UTF8String,
    UTF16String, UTF32String, Float16,
    SimpleVector, :reserved10, :reserved11, :reserved12,

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

const ser_version = 2 # do not make changes without bumping the version #!
const SER_TAG = ObjectIdDict()
let i = 2
    for t = TAGS
        SER_TAG[t] = Int32(i)
        i += 1
    end
end

const NTAGS = length(TAGS)

function sertag(v::ANY)
    ptr = pointer_from_objref(v)
    ptags = convert(Ptr{Ptr{Void}}, pointer(TAGS))
    @inbounds @simd for i = 1:NTAGS
        ptr == unsafe_load(ptags,i) && return (i+1)%Int32
    end
    return Int32(-1)
end
desertag(i::Int32) = TAGS[i-1]

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = SER_TAG[()]
const ZERO_TAG = SER_TAG[0]
const TRUE_TAG = SER_TAG[true]
const FALSE_TAG = SER_TAG[false]
const EMPTYTUPLE_TAG = SER_TAG[()]
const TUPLE_TAG = SER_TAG[Tuple]
const LONGTUPLE_TAG = SER_TAG[LongTuple]
const SIMPLEVECTOR_TAG = SER_TAG[SimpleVector]
const SYMBOL_TAG = SER_TAG[Symbol]
const LONGSYMBOL_TAG = SER_TAG[LongSymbol]
const ARRAY_TAG = SER_TAG[Array]
const UNDEFREF_TAG = SER_TAG[UndefRefTag]
const EXPR_TAG = SER_TAG[Expr]
const LONGEXPR_TAG = SER_TAG[LongExpr]
const MODULE_TAG = SER_TAG[Module]
const FUNCTION_TAG = SER_TAG[Function]
const LAMBDASTATICDATA_TAG = SER_TAG[LambdaStaticData]
const TASK_TAG = SER_TAG[Task]
const DATATYPE_TAG = SER_TAG[DataType]
const INT_TAG = SER_TAG[Int]

writetag(s, tag) = write(s, UInt8(tag))

function write_as_tag(s, tag)
    tag < VALUE_TAGS && write(s, UInt8(0))
    write(s, UInt8(tag))
end

serialize(s, x::Bool) = x ? writetag(s, TRUE_TAG) :
                            writetag(s, FALSE_TAG)

serialize(s, ::Ptr) = error("cannot serialize a pointer")

serialize(s, ::Tuple{}) = writetag(s, EMPTYTUPLE_TAG)

function serialize(s, t::Tuple)
    l = length(t)
    if l <= 255
        writetag(s, TUPLE_TAG)
        write(s, UInt8(l))
    else
        writetag(s, LONGTUPLE_TAG)
        write(s, Int32(l))
    end
    for i = 1:l
        serialize(s, t[i])
    end
end

function serialize(s, v::SimpleVector)
    writetag(s, SIMPLEVECTOR_TAG)
    write(s, Int32(length(v)))
    for i = 1:length(v)
        serialize(s, v[i])
    end
end

function serialize(s, x::Symbol)
    tag = sertag(x)
    if tag > 0
        return write_as_tag(s, tag)
    end
    pname = unsafe_convert(Ptr{UInt8}, x)
    ln = Int(ccall(:strlen, Csize_t, (Ptr{UInt8},), pname))
    if ln <= 255
        writetag(s, SYMBOL_TAG)
        write(s, UInt8(ln))
    else
        writetag(s, LONGSYMBOL_TAG)
        write(s, Int32(ln))
    end
    write(s, pname, ln)
end

function serialize_array_data(s, a)
    elty = eltype(a)
    if elty === Bool && length(a)>0
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

function serialize(s, a::Array)
    writetag(s, ARRAY_TAG)
    elty = eltype(a)
    if elty !== UInt8
        serialize(s, elty)
    end
    if ndims(a) != 1
        serialize(s, size(a))
    else
        serialize(s, length(a))
    end
    if isbits(elty)
        serialize_array_data(s, a)
    else
        for i = 1:length(a)
            if isdefined(a, i)
                serialize(s, a[i])
            else
                writetag(s, UNDEFREF_TAG)
            end
        end
    end
end

function serialize{T,N,A<:Array}(s, a::SubArray{T,N,A})
    if !isbits(T) || stride(a,1)!=1
        return serialize(s, copy(a))
    end
    writetag(s, ARRAY_TAG)
    serialize(s, T)
    serialize(s, size(a))
    serialize_array_data(s, a)
end

function serialize{T<:AbstractString}(s, ss::SubString{T})
    # avoid saving a copy of the parent string, keeping the type of ss
    invoke(serialize, Tuple{Any, Any}, s, convert(SubString{T}, convert(T,ss)))
end

# Don't serialize the pointers
function serialize(s, r::Regex)
    serialize_type(s, typeof(r))
    serialize(s, r.pattern)
    serialize(s, r.options)
end

function serialize(s, n::BigInt)
    serialize_type(s, BigInt)
    serialize(s, base(62,n))
end

function serialize(s, n::BigFloat)
    serialize_type(s, BigFloat)
    serialize(s, string(n))
end

function serialize(s, ex::Expr)
    l = length(ex.args)
    if l <= 255
        writetag(s, EXPR_TAG)
        write(s, UInt8(l))
    else
        writetag(s, LONGEXPR_TAG)
        write(s, Int32(l))
    end
    serialize(s, ex.head)
    serialize(s, ex.typ)
    for a = ex.args
        serialize(s, a)
    end
end

function serialize(s, t::Dict)
    serialize_type(s, typeof(t))
    write(s, Int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function serialize_mod_names(s, m::Module)
    p = module_parent(m)
    if m !== p
        serialize_mod_names(s, p)
        serialize(s, module_name(m))
    end
end

function serialize(s, m::Module)
    writetag(s, MODULE_TAG)
    serialize_mod_names(s, m)
    writetag(s, EMPTYTUPLE_TAG)
end

function serialize(s, f::Function)
    writetag(s, FUNCTION_TAG)
    name = false
    if isgeneric(f)
        name = f.env.name
    elseif isa(f.env,Symbol)
        name = f.env
    end
    if isa(name,Symbol)
        if isdefined(Base,name) && is(f,getfield(Base,name))
            write(s, UInt8(0))
            serialize(s, name)
            return
        end
        mod = ()
        if isa(f.env,Symbol)
            mod = Core
        elseif !is(f.env.defs, ())
            mod = f.env.defs.func.code.module
        end
        if mod !== ()
            if isdefined(mod,name) && is(f,getfield(mod,name))
                # toplevel named func
                write(s, UInt8(2))
                serialize(s, mod)
                serialize(s, name)
                return
            end
        end
        write(s, UInt8(3))
        serialize(s, f.env)
    else
        linfo = f.code
        @assert isa(linfo,LambdaStaticData)
        write(s, UInt8(1))
        serialize(s, f.env)
        serialize(s, linfo)
    end
end

const lambda_numbers = WeakKeyDict()
lnumber_salt = 0
function lambda_number(l::LambdaStaticData)
    global lnumber_salt, lambda_numbers
    if haskey(lambda_numbers, l)
        return lambda_numbers[l]
    end
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    ln = lnumber_salt+(UInt64(myid())<<44)
    lnumber_salt += 1
    lambda_numbers[l] = ln
    return ln
end

function serialize(s, linfo::LambdaStaticData)
    writetag(s, LAMBDASTATICDATA_TAG)
    serialize(s, lambda_number(linfo))
    serialize(s, uncompressed_ast(linfo))
    if isdefined(linfo.def, :roots)
        serialize(s, linfo.def.roots)
    else
        serialize(s, [])
    end
    serialize(s, linfo.sparams)
    serialize(s, linfo.inferred)
    serialize(s, linfo.module)
    if isdefined(linfo, :capt)
        serialize(s, linfo.capt)
    else
        serialize(s, nothing)
    end
end

function serialize(s, t::Task)
    if istaskstarted(t) && !istaskdone(t)
        error("cannot serialize a running Task")
    end
    writetag(s, TASK_TAG)
    serialize(s, t.code)
    serialize(s, t.storage)
    serialize(s, t.state == :queued || t.state == :waiting ? (:runnable) : t.state)
    serialize(s, t.result)
    serialize(s, t.exception)
end

function serialize_type_data(s, t)
    tname = t.name.name
    serialize(s, tname)
    mod = t.name.module
    serialize(s, mod)
    if length(t.parameters) > 0
        if isdefined(mod,tname) && is(t,getfield(mod,tname))
            serialize(s, svec())
        else
            serialize(s, t.parameters)
        end
    end
end

function serialize(s, t::DataType)
    tag = sertag(t)
    if tag > 0
        return write_as_tag(s, tag)
    end
    writetag(s, DATATYPE_TAG)
    write(s, UInt8(0))
    serialize_type_data(s, t)
end

function serialize_type(s, t::DataType)
    tag = sertag(t)
    if tag > 0
        return writetag(s, tag)
    end
    writetag(s, DATATYPE_TAG)
    write(s, UInt8(1))
    serialize_type_data(s, t)
end

function serialize(s, n::Int)
    if 0 <= n <= 32
        write(s, UInt8(ZERO_TAG+n))
        return
    end
    writetag(s, INT_TAG)
    write(s, n)
end

function serialize(s, x)
    tag = sertag(x)
    if tag > 0
        return write_as_tag(s, tag)
    end
    t = typeof(x)
    nf = nfields(t)
    serialize_type(s, t)
    if nf == 0 && t.size > 0
        write(s, x)
    else
        for i in 1:nf
            if isdefined(x, i)
                serialize(s, getfield(x, i))
            else
                writetag(s, UNDEFREF_TAG)
            end
        end
    end
end

## deserializing values ##

deserialize(s) =
    handle_deserialize(s, Int32(read(s, UInt8)))

function handle_deserialize(s, b)
    if b == 0
        return desertag(Int32(read(s, UInt8)))
    end
    tag = desertag(b)
    if b >= VALUE_TAGS
        return tag
    elseif is(tag,Tuple)
        len = Int32(read(s, UInt8))
        return deserialize_tuple(s, len)
    elseif is(tag,LongTuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    end
    return deserialize(s, tag)
end

deserialize_tuple(s, len) = ntuple(i->deserialize(s), len)

deserialize(s, ::Type{Symbol}) = symbol(read(s, UInt8, Int32(read(s, UInt8))))
deserialize(s, ::Type{LongSymbol}) = symbol(read(s, UInt8, read(s, Int32)))

function deserialize(s, ::Type{SimpleVector})
    n = read(s, Int32)
    svec([ deserialize(s) for i=1:n ]...)
end

function deserialize(s, ::Type{Module})
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

const known_lambda_data = Dict()

function deserialize(s, ::Type{Function})
    b = read(s, UInt8)
    if b==0
        name = deserialize(s)::Symbol
        if !isdefined(Base,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return getfield(Base,name)::Function
    elseif b==2
        mod = deserialize(s)::Module
        name = deserialize(s)::Symbol
        if !isdefined(mod,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return getfield(mod,name)::Function
    elseif b==3
        env = deserialize(s)
        return ccall(:jl_new_gf_internal, Any, (Any,), env)::Function
    end
    env = deserialize(s)
    linfo = deserialize(s)
    ccall(:jl_new_closure, Any, (Ptr{Void}, Any, Any),
          C_NULL, env, linfo)::Function
end

function deserialize(s, ::Type{LambdaStaticData})
    lnumber = deserialize(s)
    ast = deserialize(s)
    roots = deserialize(s)
    sparams = deserialize(s)
    infr = deserialize(s)
    mod = deserialize(s)
    capt = deserialize(s)
    if haskey(known_lambda_data, lnumber)
        return known_lambda_data[lnumber]
    else
        linfo = ccall(:jl_new_lambda_info, Any, (Any, Any), ast, sparams)
        linfo.inferred = infr
        linfo.module = mod
        linfo.roots = roots
        if !is(capt,nothing)
            linfo.capt = capt
        end
        known_lambda_data[lnumber] = linfo
        return linfo
    end
end

function deserialize(s, ::Type{Array})
    d1 = deserialize(s)
    if isa(d1,Type)
        elty = d1
        d1 = deserialize(s)
    else
        elty = UInt8
    end
    if isa(d1,Integer)
        if elty !== Bool && isbits(elty)
            return read!(s, Array(elty, d1))
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
                b = read(s, UInt8)
                v = b>>7 != 0
                count = b&0x7f
                nxt = i+count
                while i < nxt
                    A[i] = v; i+=1
                end
            end
            return A
        else
            return read(s, elty, dims)
        end
    end
    A = Array(elty, dims)
    for i = 1:length(A)
        tag = Int32(read(s, UInt8))
        if tag==0 || !is(desertag(tag), UndefRefTag)
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

deserialize(s, ::Type{Expr})     = deserialize_expr(s, Int32(read(s, UInt8)))
deserialize(s, ::Type{LongExpr}) = deserialize_expr(s, read(s, Int32))

function deserialize_expr(s, len)
    hd = deserialize(s)::Symbol
    ty = deserialize(s)
    e = Expr(hd)
    e.args = Any[ deserialize(s) for i=1:len ]
    e.typ = ty
    e
end

function deserialize(s, ::Type{UnionType})
    types = deserialize(s)
    Union(types...)
end

function deserialize(s, ::Type{DataType})
    form = read(s, UInt8)
    name = deserialize(s)::Symbol
    mod = deserialize(s)::Module
    ty = getfield(mod,name)
    if length(ty.parameters) == 0
        t = ty
    else
        params = deserialize(s)
        t = ty{params...}
    end
    if form == 0
        return t
    end
    deserialize(s, t)
end

deserialize{T}(s, ::Type{Ptr{T}}) = convert(Ptr{T}, 0)

function deserialize(s, ::Type{Task})
    t = Task(deserialize(s))
    t.storage = deserialize(s)
    t.state = deserialize(s)
    t.result = deserialize(s)
    t.exception = deserialize(s)
    t
end

# default DataType deserializer
function deserialize(s, t::DataType)
    nf = nfields(t)
    if nf == 0 && t.size > 0
        # bits type
        return read(s, t)
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
        for i in 1:nf
            tag = Int32(read(s, UInt8))
            if tag==0 || !is(desertag(tag), UndefRefTag)
                ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), x, i-1, handle_deserialize(s, tag))
            end
        end
        return x
    end
end

function deserialize{K,V}(s, T::Type{Dict{K,V}})
    n = read(s, Int32)
    t = T(); sizehint!(t, n)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

deserialize(s, ::Type{BigFloat}) = BigFloat(deserialize(s))

deserialize(s, ::Type{BigInt}) = get(GMP.tryparse_internal(BigInt, deserialize(s), 62, true))

deserialize(s, ::Type{BigInt}) = get(GMP.tryparse_internal(BigInt, deserialize(s), 62, true))

function deserialize(s, t::Type{Regex})
    pattern = deserialize(s)
    options = deserialize(s)
    Regex(pattern, options)
end

end
