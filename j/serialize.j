## serializing values ##

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr

const _jl_ser_tag = idtable()
const _jl_deser_tag = idtable()
let i = 2
    global _jl_ser_tag, _jl_deser_tag
    for t = {Symbol, Int8, Uint8, Int16, Uint16, Int32, Uint32,
             Int64, Uint64, Float32, Float64, Char, Ptr,
             AbstractKind, UnionKind, BitsKind, CompositeKind, FuncKind,
             Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,
             LineNumberNode, SymbolNode, LabelNode,

             (), Bool, Any, :Any, :Array, :TypeVar, :FuncKind, :Box,
             :lambda, :vinf, :locals, :body, :return, :call, symbol("::"),
             :null, :goto, :gotoifnot, :string, :T, :S,
             :a, :b, :c, :d, :e, :f, :g, :h, :i, :j, :k, :l, :m, :n, :o,
             :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z,
             false, true, nothing, 0, 1, 2, 3, 4}
        _jl_ser_tag[t] = int32(i)
        _jl_deser_tag[int32(i)] = t
        i += 1
    end
end

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = _jl_ser_tag[()]

writetag(s, x) = write(s, uint8(_jl_ser_tag[x]))

function write_as_tag(s, x)
    t = _jl_ser_tag[x]
    if t < VALUE_TAGS
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

function serialize(s, a::Array)
    writetag(s, Array)
    elty = typeof(a).parameters[1]
    serialize(s, elty)
    serialize(s, size(a))
    if isa(elty,BitsKind)
        write(s, a)
    else
        # TODO: handle uninitialized elements
        for i = 1:numel(a)
            serialize(s, a[i])
        end
    end
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

function serialize(s, t::Union(AbstractKind,BitsKind,CompositeKind))
    if has(_jl_ser_tag,t)
        write_as_tag(s, t)
    else
        writetag(s, AbstractKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
    end
end

function serialize(s, u::UnionKind)
    writetag(s, UnionKind)
    serialize(s, u.types)
end

function _jl_lambda_number(l::LambdaStaticData)
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    hash(uint64(uid(l))+(uint64(myid())<<44))
end

function serialize(s, f::Function)
    writetag(s, FuncKind)
    env = ccall(:jl_closure_env, Any, (Any,), f)
    linfo = ccall(:jl_closure_linfo, Any, (Any,), f)
    if isa(linfo,Symbol)
        if isbound(linfo) && is(f,eval(linfo))
            # toplevel named func
            write(s, uint8(0))
            serialize(s, linfo)
        else
            error(f," is not serializable")
        end
    elseif is(linfo,())
        error(f," is not serializable")
    else
        @assert (isa(linfo,LambdaStaticData))
        write(s, uint8(1))
        serialize(s, _jl_lambda_number(linfo))
        serialize(s, linfo.ast)
        serialize(s, linfo.sparams)
        serialize(s, linfo.inferred)
        serialize(s, env)
    end
end

function serialize(s, x)
    if has(_jl_ser_tag,x)
        return write_as_tag(s, x)
    end
    t = typeof(x)
    if isa(t,BitsKind)
        if has(_jl_ser_tag,t)
            writetag(s, t)
        else
            writetag(s, BitsKind)
            serialize(s, t.name.name)
            serialize(s, t.parameters)
        end
        write(s, x)
    elseif isa(t,CompositeKind)
        writetag(s, CompositeKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
        for n = t.names
            serialize(s, getfield(x, n))
        end
    else
        error(x," is not serializable")
    end
end

## deserializing values ##

force(x) = x
force(x::Function) = x()

# deserialize(s) returns either a simple value or a thunk. calling the
# thunk gives the true value. this allows deserialization to happen in two
# phases: first we do all the I/O and figure out its structure, and second
# we actually make objects. this allows for constructing objects that might
# interfere with I/O by reading, writing, blocking, etc.

function deserialize(s)
    b = int32(read(s, Uint8))
    if b == 0
        return _jl_deser_tag[int32(read(s, Uint8))]
    end
    tag = _jl_deser_tag[b]
    if b >= VALUE_TAGS
        return tag
    elseif is(tag,Tuple)
        len = int32(read(s, Uint8))
        return deserialize_tuple(s, len)
    elseif is(tag,LongTuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    elseif is(tag,FuncKind)
        return deserialize_function(s)
    end
    return deserialize(s, tag)
end

deserialize(s, t::BitsKind) = read(s, t)

function deserialize(s, ::Type{BitsKind})
    name = deserialize(s)::Symbol
    params = force(deserialize(s))
    t = apply_type(eval(name), params...)
    return read(s, t)
end

function deserialize(s, ::Type{UnionKind})
    args = deserialize(s)
    ()->Union(force(args)...)
end

deserialize_tuple(s, len) = (a = ntuple(len, i->deserialize(s));
                             ()->map(force, a))

deserialize(s, ::Type{Symbol}) = symbol(read(s, Uint8, int32(read(s, Uint8))))
deserialize(s, ::Type{LongSymbol}) = symbol(read(s, Uint8, read(s, Int32)))

const _jl_known_lambda_data = HashTable()

function deserialize_function(s)
    b = read(s, Uint8)
    if b==0
        name = deserialize(s)::Symbol
        return ()->eval(name)
    end
    lnumber = force(deserialize(s))
    ast = deserialize(s)
    sparams = deserialize(s)
    infr = force(deserialize(s))
    env = deserialize(s)
    if has(_jl_known_lambda_data, lnumber)
        linfo = _jl_known_lambda_data[lnumber]
        function ()
            ccall(:jl_new_closure_internal, Any, (Any, Any),
                  linfo, force(env))::Function
        end
    else
        function ()
            linfo = ccall(:jl_new_lambda_info, Any, (Any, Any),
                          force(ast), force(sparams))
            linfo.inferred = infr
            _jl_known_lambda_data[lnumber] = linfo
            ccall(:jl_new_closure_internal, Any, (Any, Any),
                  linfo, force(env))::Function
        end
    end
end

function deserialize(s, ::Type{Array})
    elty = force(deserialize(s))
    dims = force(deserialize(s))
    if isa(elty,BitsKind)
        return read(s, elty, dims)
    end
    temp = Array(Any, dims)
    for i = 1:numel(temp)
        temp[i] = deserialize(s)
    end
    function ()
        A = Array(elty, dims)
        for i = 1:numel(A)
            A[i] = force(temp[i])
        end
        return A
    end
end

deserialize(s, ::Type{Expr})     = deserialize_expr(s, int32(read(s, Uint8)))
deserialize(s, ::Type{LongExpr}) = deserialize_expr(s, read(s, Int32))

function deserialize_expr(s, len)
    hd = deserialize(s)::Symbol
    ty = force(deserialize(s))
    args = { deserialize(s) | i=1:len }
    function ()
        e = expr(hd, map(force, args))
        e.typ = ty
        e
    end
end

function deserialize(s, ::Type{AbstractKind})
    name = deserialize(s)::Symbol
    params = force(deserialize(s))
    apply_type(eval(name), params...)
end

function deserialize(s, ::Type{CompositeKind})
    name = deserialize(s)::Symbol
    params = force(deserialize(s))
    t = apply_type(eval(name), params...)
    # allow delegation to more specialized method
    return deserialize(s, t)
end

function deserialize(s, ::Type{TypeVar})
    name = force(deserialize(s))
    lb = force(deserialize(s))
    ub = force(deserialize(s))
    typevar(name, lb, ub)
end

# default structure deserializer
function deserialize(s, t::Type)
    @assert (isa(t,CompositeKind))
    nf = length(t.names)
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    elseif nf == 1
        f1 = deserialize(s)
        ()->ccall(:jl_new_struct, Any, (Any,Any...), t, force(f1))
    elseif nf == 2
        f1 = deserialize(s)
        f2 = deserialize(s)
        ()->ccall(:jl_new_struct, Any, (Any,Any...), t, force(f1), force(f2))
    elseif nf == 3
        f1 = deserialize(s)
        f2 = deserialize(s)
        f3 = deserialize(s)
        ()->ccall(:jl_new_struct, Any, (Any,Any...),
                  t, force(f1), force(f2), force(f3))
    elseif nf == 4
        f1 = deserialize(s)
        f2 = deserialize(s)
        f3 = deserialize(s)
        f4 = deserialize(s)
        ()->ccall(:jl_new_struct, Any, (Any,Any...),
                  t, force(f1), force(f2), force(f3), force(f4))
    else
        f = ntuple(nf, i->deserialize(s))
        ()->ccall(:jl_new_structt, Any, (Any,Any), t, map(force, f))
    end
end
