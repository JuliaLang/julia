## serializing values ##

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr

ser_tag = idtable()
deser_tag = idtable()
let i = 2
    global ser_tag, deser_tag
    for t = {Symbol, Int8, Uint8, Int16, Uint16, Int32, Uint32,
             Int64, Uint64, Float32, Float64, Char, Ptr,
             TagKind, UnionKind, BitsKind, StructKind, FuncKind,
             Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,

             (), Bool, Any, :Any, :Array, :TypeVar, :FuncKind, :Box,
             :lambda, :vinf, :locals, :body, :return, :call, symbol("::"),
             :null, :goto, :gotoifnot, :label, :symbol, :string, :T, :S,
             :a, :b, :c, :d, :e, :f, :g, :h, :i, :j, :k, :l, :m, :n, :o,
             :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z,
             false, true, 0, 1, 2, 3, 4}
        ser_tag[t] = i
        deser_tag[i] = t
        i += 1
    end
end

# tags >= this just represent themselves, their whole representation is 1 byte
VALUE_TAGS = ser_tag[()]

writetag(s, x) = write(s, uint8(ser_tag[x]))

function write_as_tag(s, x)
    t = ser_tag[x]
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
    if has(ser_tag, x)
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
    serialize(s, e.type)
    for a = e.args
        serialize(s, a)
    end
end

function serialize(s, t::TagKind)
    if has(ser_tag,t)
        write_as_tag(s, t)
    else
        writetag(s, TagKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
    end
end

function serialize(s, u::UnionKind)
    writetag(s, UnionKind)
    serialize(s, u.types)
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
        serialize(s, linfo.ast)
        serialize(s, linfo.sparams)
        serialize(s, env)
    end
end

function serialize(s, x)
    if has(ser_tag,x)
        return write_as_tag(s, x)
    end
    t = typeof(x)
    if isa(t,BitsKind)
        if has(ser_tag,t)
            writetag(s, t)
        else
            writetag(s, BitsKind)
            serialize(s, t.name.name)
            serialize(s, t.parameters)
        end
        write(s, x)
    elseif isa(t,StructKind)
        writetag(s, StructKind)
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

function deserialize_function(s)
    b = read(s, Uint8)
    if b==0
        name = deserialize(s)::Symbol
        return ()->eval(name)
    end
    ast = deserialize(s)
    sparams = deserialize(s)
    env = deserialize(s)
    function ()
        linfo = ccall(:jl_new_lambda_info, Any, (Any, Any),
                      force(ast), force(sparams))
        ccall(:jl_new_closure_internal, Any, (Any, Any),
              linfo, force(env))::Function
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
        e.type = ty
        e
    end
end

function deserialize(s, ::Type{TagKind})
    name = deserialize(s)::Symbol
    params = force(deserialize(s))
    apply_type(eval(name), params...)
end

function deserialize(s, ::Type{StructKind})
    name = deserialize(s)::Symbol
    params = force(deserialize(s))
    t = apply_type(eval(name), params...)
    # allow delegation to more specialized method
    return deserialize(s, t)
end

# default structure deserializer
function deserialize(s, t::Type)
    @assert (isa(t,StructKind))
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
