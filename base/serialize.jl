## serializing values ##

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr
abstract UndefRefTag
abstract BackrefTag

const ser_version = 1 # do not make changes without bumping the version #!
const ser_tag = ObjectIdDict()
const deser_tag = ObjectIdDict()
let i = 2
    global ser_tag, deser_tag
    for t = {Symbol, Int8, Uint8, Int16, Uint16, Int32, Uint32,
             Int64, Uint64, Int128, Uint128, Float32, Float64, Char, Ptr,
             DataType, UnionType, Function,
             Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,
             LineNumberNode, SymbolNode, LabelNode, GotoNode,
             QuoteNode, TopNode, TypeVar, Box, LambdaStaticData,
             Module, UndefRefTag, Task, BackrefTag,
             :reserved5, :reserved6, :reserved7, :reserved8,
             :reserved9, :reserved10, :reserved11, :reserved12,
             
             (), Bool, Any, :Any, None, Top, Undef, Type,
             :Array, :TypeVar, :Box,
             :lambda, :body, :return, :call, symbol("::"),
             :(=), :null, :gotoifnot, :A, :B, :C, :M, :N, :T, :S, :X, :Y,
             :a, :b, :c, :d, :e, :f, :g, :h, :i, :j, :k, :l, :m, :n, :o,
             :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z,
             :add_int, :sub_int, :mul_int, :add_float, :sub_float,
             :mul_float, :unbox, :box,
             :eq_int, :slt_int, :sle_int, :ne_int,
             :arrayset, :arrayref,
             :reserved13, :reserved14, :reserved15, :reserved16,
             :reserved17, :reserved18, :reserved19, :reserved20,
             false, true, nothing, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
             12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
             28, 29, 30, 31, 32}
        ser_tag[t] = int32(i)
        deser_tag[int32(i)] = t
        i += 1
    end
end

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = ser_tag[()]

writetag(s, x) = write(s, uint8(ser_tag[x]))

function write_as_tag(s, x)
    t = ser_tag[x]
    if t < VALUE_TAGS
        write(s, uint8(0))
    end
    write(s, uint8(t))
end

# cycle handling
serialize_cycle(io, x) = false

function serialize_cycle(io::IOBuffer, x)
    if !isdefined(io,:serialization_table)
        io.serialization_table = ObjectIdDict()
    end
    offs = get(io.serialization_table, x, -1)
    if offs != -1
        writetag(io, BackrefTag)
        write(io, int(offs))
        return true
    end
    io.serialization_table[x] = position(io)
    return false
end

serialize(s, x::Bool) = write_as_tag(s, x)

serialize(s, ::Ptr) = error("cannot serialize a pointer")

serialize(s, ::()) = write_as_tag(s, ())

function serialize(s, t::Tuple)
    l = length(t)
    if l <= 255
        writetag(s, Tuple)
        write(s, uint8(l))
    else
        writetag(s, LongTuple)
        write(s, int32(l))
    end
    for i = 1:l
        serialize(s, t[i])
    end
end

function serialize(s, x::Symbol)
    if haskey(ser_tag, x)
        return write_as_tag(s, x)
    end
    name = string(x)
    ln = sizeof(name)
    if ln <= 255
        writetag(s, Symbol)
        write(s, uint8(ln))
    else
        writetag(s, LongSymbol)
        write(s, int32(ln))
    end
    write(s, name)
end

function serialize_array_data(s, a)
    elty = eltype(a)
    if elty === Bool && length(a)>0
        last = a[1]
        count = 1
        for i = 2:length(a)
            if a[i] != last || count == 127
                write(s, uint8((uint8(last)<<7) | count))
                last = a[i]
                count = 1
            else
                count += 1
            end
        end
        write(s, uint8((uint8(last)<<7) | count))
    else
        write(s, a)
    end
end

function serialize(s, a::Array)
    serialize_cycle(s, a) && return
    writetag(s, Array)
    elty = eltype(a)
    serialize(s, elty)
    serialize(s, size(a))
    if isbits(elty)
        serialize_array_data(s, a)
    else
        for i = 1:length(a)
            if isdefined(a, i)
                serialize(s, a[i])
            else
                writetag(s, UndefRefTag)
            end
        end
    end
end

function serialize{T,N,A<:Array}(s, a::SubArray{T,N,A})
    serialize_cycle(s, a) && return
    if !isbits(T) || stride(a,1)!=1
        return serialize(s, copy(a))
    end
    writetag(s, Array)
    serialize(s, T)
    serialize(s, size(a))
    serialize_array_data(s, a)
end

function serialize(s, e::Expr)
    serialize_cycle(s, e) && return
    l = length(e.args)
    if l <= 255
        writetag(s, Expr)
        write(s, uint8(l))
    else
        writetag(s, LongExpr)
        write(s, int32(l))
    end
    serialize(s, e.head)
    serialize(s, e.typ)
    for a = e.args
        serialize(s, a)
    end
end

function serialize(s, m::Module)
    writetag(s, Module)
    serialize(s, fullname(m))
end

function serialize(s, f::Function)
    serialize_cycle(s, f) && return
    writetag(s, Function)
    name = false
    if isgeneric(f)
        name = f.env.name
    elseif isa(f.env,Symbol)
        name = f.env
    end
    if isa(name,Symbol)
        if isdefined(Base,name) && is(f,eval(Base,name))
            write(s, uint8(0))
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
            if isdefined(mod,name) && is(f,eval(mod,name))
                # toplevel named func
                write(s, uint8(2))
                serialize(s, mod)
                serialize(s, name)
                return
            end
        end
        write(s, uint8(3))
        serialize(s, f.env)
    else
        linfo = f.code
        @assert isa(linfo,LambdaStaticData)
        write(s, uint8(1))
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
    ln = hash(lnumber_salt+(uint64(myid())<<44))
    lnumber_salt += 1
    lambda_numbers[l] = ln
    return ln
end

function serialize(s, linfo::LambdaStaticData)
    serialize_cycle(s, linfo) && return
    writetag(s, LambdaStaticData)
    serialize(s, lambda_number(linfo))
    serialize(s, uncompressed_ast(linfo))
    if isdefined(linfo.def, :roots)
        serialize(s, linfo.def.roots)
    else
        serialize(s, {})
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
    serialize_cycle(s, t) && return
    if istaskstarted(t) && !t.done
        error("cannot serialize a running Task")
    end
    writetag(s, Task)
    serialize(s, t.code)
    serialize(s, t.storage)
    serialize(s, t.done)
    serialize(s, t.runnable)
    serialize(s, t.result)
    serialize(s, t.exception)
end

function serialize_type_data(s, t)
    tname = t.name.name
    serialize(s, tname)
    mod = t.name.module
    serialize(s, mod)
    if isdefined(mod,tname) && is(t,eval(mod,tname))
        serialize(s, ())
    else
        serialize(s, t.parameters)
    end
end

function serialize(s, t::DataType)
    if haskey(ser_tag,t)
        write_as_tag(s, t)
    else
        writetag(s, DataType)
        write(s, uint8(0))
        serialize_type_data(s, t)
    end
end

function serialize_type(s, t::DataType)
    if haskey(ser_tag,t)
        writetag(s, t)
    else
        writetag(s, DataType)
        write(s, uint8(1))
        serialize_type_data(s, t)
    end
end

function serialize(s, x)
    if haskey(ser_tag,x)
        return write_as_tag(s, x)
    end
    t = typeof(x)
    if length(t.names)==0 && t.size>0
        serialize_type(s, t)
        write(s, x)
    else
        if !isimmutable(x)
            serialize_cycle(s, x) && return
        end
        serialize_type(s, t)
        serialize(s, length(t.names))
        for n in t.names
            if isdefined(x, n)
                serialize(s, getfield(x, n))
            else
                writetag(s, UndefRefTag)
            end
        end
    end
end

## deserializing values ##

deserialization_table = nothing

function deserialize(s)
    global deserialization_table
    top = (deserialization_table === nothing)

    obj = handle_deserialize(s, int32(read(s, Uint8)))

    if top
        deserialization_table = nothing
    end
    obj
end

function deserialize_cycle(pos, x)
    if pos < 0
        return
    end
    global deserialization_table
    if deserialization_table === nothing
        deserialization_table = ObjectIdDict()
    end
    deserialization_table[pos] = x
end

# deserialize_ is an internal function to dispatch on the tag
# describing the serialized representation. the number of
# representations is fixed, so deserialize_ does not get extended.

function handle_deserialize(s, b)
    last_obj_pos = position(s)-1
    if b == 0
        return deser_tag[int32(read(s, Uint8))]
    end
    tag = deser_tag[b]
    if b >= VALUE_TAGS
        return tag
    elseif is(tag,Tuple)
        len = int32(read(s, Uint8))
        return deserialize_tuple(s, len)
    elseif is(tag,LongTuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    elseif is(tag,BackrefTag)
        pos = read(s, Int)
        return deserialization_table[pos]
    end
    return deserialize_(s, tag, last_obj_pos)
end

deserialize_tuple(s, len) = ntuple(len, i->deserialize(s))

deserialize_(s, ::Type{Symbol}, p) = symbol(read(s, Uint8, int32(read(s, Uint8))))
deserialize_(s, ::Type{LongSymbol}, p) = symbol(read(s, Uint8, read(s, Int32)))

function deserialize_(s, ::Type{Module}, p)
    path = deserialize(s)
    m = Main
    for mname in path
        m = eval(m,mname)::Module
    end
    m
end

const known_lambda_data = Dict()

function deserialize_(s, ::Type{Function}, p)
    b = read(s, Uint8)
    if b==0
        name = deserialize(s)::Symbol
        if !isdefined(Base,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return eval(Base,name)::Function
    elseif b==2
        mod = deserialize(s)::Module
        name = deserialize(s)::Symbol
        if !isdefined(mod,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return eval(mod,name)::Function
    elseif b==3
        env = deserialize(s)
        return ccall(:jl_new_gf_internal, Any, (Any,), env)::Function
    end
    env = deserialize(s)
    linfo = deserialize(s)
    ccall(:jl_new_closure, Any, (Ptr{Void}, Any, Any),
          C_NULL, env, linfo)::Function
end

function deserialize_(s, ::Type{LambdaStaticData}, p)
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

function deserialize_(s, ::Type{Array}, pos)
    elty = deserialize(s)
    dims = deserialize(s)::Dims
    if isbits(elty)
        n = prod(dims)::Int
        if elty === Bool && n>0
            A = Array(Bool, dims)
            i = 1
            while i <= n
                b = read(s, Uint8)
                v = bool(b>>7)
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
    deserialize_cycle(pos, A)
    for i = 1:length(A)
        tag = int32(read(s, Uint8))
        if tag==0 || !is(deser_tag[tag], UndefRefTag)
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

deserialize_(s, ::Type{Expr}, p)     = deserialize_expr(s, int32(read(s, Uint8)))
deserialize_(s, ::Type{LongExpr}, p) = deserialize_expr(s, read(s, Int32))

function deserialize_expr(s, len)
    hd = deserialize(s)::Symbol
    ty = deserialize(s)
    e = Expr(hd)
    e.args = { deserialize(s) for i=1:len }
    e.typ = ty
    e
end

function deserialize_(s, ::Type{TypeVar}, p)
    nf_expected = deserialize(s)
    name = deserialize(s)
    lb = deserialize(s)
    ub = deserialize(s)
    TypeVar(name, lb, ub)
end

function deserialize_(s, ::Type{UnionType}, p)
    nf_expected = deserialize(s)
    types = deserialize(s)
    Union(types...)
end

function deserialize_(s, ::Type{DataType}, pos)
    form = read(s, Uint8)
    name = deserialize(s)::Symbol
    mod = deserialize(s)::Module
    params = deserialize(s)
    ty = eval(mod,name)
    if is(params,())
        t = ty
    else
        t = apply_type(ty, params...)
    end
    if form == 0
        return t
    end
    if applicable(deserialize, s, t)
        return deserialize(s, t)
    end
    deserialize(s, t, pos)
end

deserialize_{T}(s, ::Type{Ptr{T}}, p) = pointer(T, 0)

function deserialize_(s, ::Type{Task}, p)
    t = Task(deserialize(s))
    t.storage = deserialize(s)
    t.done = deserialize(s)
    t.runnable = deserialize(s)
    t.result = deserialize(s)
    t.exception = deserialize(s)
    t
end

# all other formats are handled by the default DataType deserializer
deserialize_(s, t::DataType, pos) = deserialize(s, t, pos)

# default DataType deserializer
function deserialize(s, t::DataType, pos)
    if length(t.names)==0 && t.size>0
        # bits type
        return read(s, t)
    end
    nf_expected = deserialize(s)
    nf = length(t.names)
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    elseif !t.mutable
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
            flds = { deserialize(s) for i = 1:nf }
            return ccall(:jl_new_structv, Any, (Any,Ptr{Void},Uint32),
                         t, flds, nf)
        end
    else
        x = ccall(:jl_new_struct_uninit, Any, (Any,), t)
        deserialize_cycle(pos, x)
        for n in t.names
            tag = int32(read(s, Uint8))
            if tag==0 || !is(deser_tag[tag], UndefRefTag)
                setfield(x, n, handle_deserialize(s, tag))
            end
        end
        return x
    end
end
