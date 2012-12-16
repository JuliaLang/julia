## serializing values ##

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr
abstract UndefRefTag

const _jl_ser_version = 1 # do not make changes without bumping the version #!
const _jl_ser_tag = ObjectIdDict()
const _jl_deser_tag = ObjectIdDict()
let i = 2
    global _jl_ser_tag, _jl_deser_tag
    for t = {Symbol, Int8, Uint8, Int16, Uint16, Int32, Uint32,
             Int64, Uint64, Int128, Uint128, Float32, Float64, Char, Ptr,
             AbstractKind, UnionKind, BitsKind, CompositeKind, Function,
             Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,
             LineNumberNode, SymbolNode, LabelNode, GotoNode,
             QuoteNode, TopNode, TypeVar, Box, LambdaStaticData,
             Module, UndefRefTag, :reserved3, :reserved4,
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
        _jl_ser_tag[t] = int32(i)
        _jl_deser_tag[int32(i)] = t
        i += 1
    end
end

# tags >= this just represent themselves, their whole representation is 1 byte
const _jl_VALUE_TAGS = _jl_ser_tag[()]

writetag(s, x) = write(s, uint8(_jl_ser_tag[x]))

function write_as_tag(s, x)
    t = _jl_ser_tag[x]
    if t < _jl_VALUE_TAGS
        write(s, uint8(0))
    end
    write(s, uint8(t))
end

serialize(s, x::Bool) = write_as_tag(s, x)

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
    if has(_jl_ser_tag, x)
        return write_as_tag(s, x)
    end
    name = string(x)
    ln = length(name)
    if ln <= 255
        writetag(s, Symbol)
        write(s, uint8(ln))
    else
        writetag(s, LongSymbol)
        write(s, int32(length(name)))
    end
    write(s, name)
end

function serialize_array_data(s, a)
    elty = eltype(a)
    if elty === Bool && numel(a)>0
        last = a[1]
        count = 1
        for i = 2:numel(a)
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
    writetag(s, Array)
    elty = eltype(a)
    serialize(s, elty)
    serialize(s, size(a))
    if isa(elty,BitsKind)
        serialize_array_data(s, a)
    else
        for i = 1:numel(a)
            if isdefined(a, i)
                serialize(s, a[i])
            else
                writetag(s, UndefRefTag)
            end
        end
    end
end

function serialize{T,N,A<:Array}(s, a::SubArray{T,N,A})
    if !isa(T,BitsKind) || stride(a,1)!=1
        return serialize(s, copy(a))
    end
    writetag(s, Array)
    serialize(s, T)
    serialize(s, size(a))
    serialize_array_data(s, a)
end

function serialize(s, e::Expr)
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
    serialize(s, full_name(m))
end

function _jl_lambda_number(l::LambdaStaticData)
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    hash(uint64(object_id(l))+(uint64(myid())<<44))
end

function serialize(s, f::Function)
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
        if !is(f.env.defs, ())
            mod = f.env.defs.func.code.module
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

function serialize(s, linfo::LambdaStaticData)
    writetag(s, LambdaStaticData)
    serialize(s, _jl_lambda_number(linfo))
    serialize(s, linfo.ast)
    serialize(s, linfo.sparams)
    serialize(s, linfo.inferred)
    serialize(s, linfo.module)
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

function serialize(s, t::Union(AbstractKind,BitsKind,CompositeKind))
    if has(_jl_ser_tag,t)
        write_as_tag(s, t)
    else
        writetag(s, AbstractKind)
        serialize_type_data(s, t)
    end
end

function serialize_type(s, t::Union(CompositeKind,BitsKind))
    if has(_jl_ser_tag,t)
        writetag(s, t)
    else
        writetag(s, typeof(t))
        serialize_type_data(s, t)
    end
end

function serialize(s, x)
    if has(_jl_ser_tag,x)
        return write_as_tag(s, x)
    end
    t = typeof(x)
    if isa(t,BitsKind)
        serialize_type(s, t)
        write(s, x)
    elseif isa(t,CompositeKind)
        serialize_type(s, t)
        serialize(s, length(t.names))
        for n in t.names
            if isdefined(x, n)
                serialize(s, getfield(x, n))
            else
                writetag(s, UndefRefTag)
            end
        end
    else
        error(x," is not serializable")
    end
end

## deserializing values ##

function deserialize(s)
    handle_deserialize(s, int32(read(s, Uint8)))
end

function handle_deserialize(s, b)
    if b == 0
        return _jl_deser_tag[int32(read(s, Uint8))]
    end
    tag = _jl_deser_tag[b]
    if b >= _jl_VALUE_TAGS
        return tag
    elseif is(tag,Tuple)
        len = int32(read(s, Uint8))
        return deserialize_tuple(s, len)
    elseif is(tag,LongTuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    end
    return deserialize(s, tag)
end

deserialize_tuple(s, len) = ntuple(len, i->deserialize(s))

deserialize(s, ::Type{Symbol}) = symbol(read(s, Uint8, int32(read(s, Uint8))))
deserialize(s, ::Type{LongSymbol}) = symbol(read(s, Uint8, read(s, Int32)))

function deserialize(s, ::Type{Module})
    path = deserialize(s)
    m = Main
    for mname in path
        m = eval(m,mname)
    end
    m
end

const _jl_known_lambda_data = Dict()

function deserialize(s, ::Type{Function})
    b = read(s, Uint8)
    if b==0
        name = deserialize(s)::Symbol
        return eval(Base,name)
    elseif b==2
        mod = deserialize(s)::Module
        name = deserialize(s)::Symbol
        return eval(mod,name)
    elseif b==3
        env = deserialize(s)
        return ccall(:jl_new_gf_internal, Any, (Any,), env)
    end
    env = deserialize(s)
    linfo = deserialize(s)
    ccall(:jl_new_closure, Any, (Ptr{Void}, Any, Any),
          C_NULL, env, linfo)::Function
end

function deserialize(s, ::Type{LambdaStaticData})
    lnumber = deserialize(s)
    ast = deserialize(s)
    sparams = deserialize(s)
    infr = deserialize(s)
    mod = deserialize(s)
    if has(_jl_known_lambda_data, lnumber)
        return _jl_known_lambda_data[lnumber]
    else
        linfo = ccall(:jl_new_lambda_info, Any, (Any, Any), ast, sparams)
        linfo.inferred = infr
        linfo.module = mod
        _jl_known_lambda_data[lnumber] = linfo
        return linfo
    end
end

function deserialize(s, ::Type{Array})
    elty = deserialize(s)
    dims = deserialize(s)::Dims
    if isa(elty,BitsKind)
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
    for i = 1:numel(A)
        tag = int32(read(s, Uint8))
        if tag==0 || !is(_jl_deser_tag[tag], UndefRefTag)
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

deserialize(s, ::Type{Expr})     = deserialize_expr(s, int32(read(s, Uint8)))
deserialize(s, ::Type{LongExpr}) = deserialize_expr(s, read(s, Int32))

function deserialize_expr(s, len)
    hd = deserialize(s)::Symbol
    ty = deserialize(s)
    e = expr(hd, { deserialize(s) for i=1:len })
    e.typ = ty
    e
end

function deserialize(s, ::Type{TypeVar})
    nf_expected = deserialize(s)
    name = deserialize(s)
    lb = deserialize(s)
    ub = deserialize(s)
    TypeVar(name, lb, ub)
end

function deserialize(s, ::Type{UnionKind})
    nf_expected = deserialize(s)
    types = deserialize(s)
    Union(types...)
end

function deserialize(s, ::Type{AbstractKind})
    name = deserialize(s)::Symbol
    mod = deserialize(s)::Module
    params = deserialize(s)
    ty = eval(mod,name)
    if is(params,())
        return ty
    end
    apply_type(ty, params...)
end

function deserialize(s, ::Union(Type{CompositeKind}, Type{BitsKind}))
    t = deserialize(s, AbstractKind)
    # allow delegation to more specialized method
    return deserialize(s, t)
end

# default bits deserializer
deserialize(s, t::BitsKind) = read(s, t)

# default structure deserializer
function deserialize(s, t::CompositeKind)
    nf_expected = deserialize(s)
    nf = length(t.names)
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    else
        x = ccall(:jl_new_struct_uninit, Any, (Any,), t)
        for n in t.names
            tag = int32(read(s, Uint8))
            if tag==0 || !is(_jl_deser_tag[tag], UndefRefTag)
                setfield(x, n, handle_deserialize(s, tag))
            end
        end
        return x
    end
end
