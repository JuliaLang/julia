## serializing values ##

ser_tag = idtable()
let i = 1
    global ser_tag
    for t = {Any, Symbol, Bool, Int8, Uint8, Int16, Uint16, Int32, Uint32,
             Int64, Uint64, Float32, Float64,
             TagKind, UnionKind, BitsKind, StructKind, FuncKind,
             Tuple, Array}
        ser_tag[t] = i
        i += 1
    end
end

deser_tag = idtable()
for (k, v) = ser_tag
    deser_tag[v] = k
end


writetag(s, x) = write(s, uint8(ser_tag[x]))

serialize(s, x::Bool) = (writetag(s,Bool); write(s, uint8(x)))

function serialize(s, t::Tuple)
    writetag(s, Tuple)
    write(s, int32(length(t)))
    for i = 1:length(t)
        serialize(s, t[i])
    end
end

function serialize(s, x::Symbol)
    writetag(s, Symbol)
    name = string(x)
    write(s, int32(length(name)))
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

function serialize(s, t::TagKind)
    if has(ser_tag,t)
        write(s, uint8(0))
        writetag(s, t)
    else
        writetag(s, TagKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
    end
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
        assert(isa(linfo,LambdaStaticData))
        write(s, uint8(1))
        serialize(s, linfo.ast)
        serialize(s, linfo.sparams)
        serialize(s, env)
    end
end

function serialize(s, x)
    if has(ser_tag,x)
        write(s, uint8(0)) # tag 0 indicates just a tag
        writetag(s, x)
        return ()
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

function deserialize(s)
    b = int32(read(s, Uint8))
    if b == 0
        return deser_tag[int32(read(s, Uint8))]
    end
    tag = deser_tag[b]
    if is(tag,Tuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    end
    return deserialize(s, tag)
end

deserialize(s, t::BitsKind) = read(s, t)

function deserialize(s, ::Type{BitsKind})
    name = deserialize(s)::Symbol
    params = deserialize(s)
    t = apply_type(eval(name), params...)
    return read(s, t)
end

deserialize_tuple(s, len) = ntuple(len, i->deserialize(s))

deserialize(s, ::Type{Symbol}) = symbol(read(s, Uint8, read(s, Int32)))

function deserialize(s, ::Type{Array})
    elty = deserialize(s)
    dims = deserialize(s)
    if isa(elty,BitsKind)
        return read(s, elty, dims...)
    end
    A = Array(elty, dims...)
    for i = 1:numel(A)
        A[i] = deserialize(s)
    end
    return A
end

function deserialize(s, ::Type{TagKind})
    name = deserialize(s)::Symbol
    params = deserialize(s)
    apply_type(eval(name), params...)
end

function deserialize(s, ::Type{StructKind})
    name = deserialize(s)::Symbol
    params = deserialize(s)
    t = apply_type(eval(name), params...)
    # allow delegation to more specialized method
    return deserialize(s, t)
end

function deserialize(s, ::Type{FuncKind})
    b = read(s, Uint8)
    if b==0
        name = deserialize(s)::Symbol
        return eval(name)
    end
    ast = deserialize(s)
    sparams = deserialize(s)
    env = deserialize(s)
    linfo = ccall(:jl_new_lambda_info, Any, (Any, Any), ast, sparams)
    ccall(:jl_new_closure_internal, Any, (Any, Any), linfo, env)::Function
end

# default structure deserializer
function deserialize(s, t::Type)
    assert(isa(t,StructKind))
    nf = length(t.names)
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,), t)
    elseif nf == 1
        return ccall(:jl_new_struct, Any, (Any,Any), t, deserialize(s))
    elseif nf == 2
        return ccall(:jl_new_struct, Any,
                     (Any,Any,Any), t, deserialize(s), deserialize(s))
    elseif nf == 3
        return ccall(:jl_new_struct, Any,
                     (Any,Any,Any,Any), t, deserialize(s), deserialize(s),
                     deserialize(s))
    elseif nf == 4
        return ccall(:jl_new_struct, Any,
                     (Any,Any,Any,Any,Any), t, deserialize(s), deserialize(s),
                     deserialize(s), deserialize(s))
    else
        return ccall(:jl_new_structt, Any,
                     (Any,Any), t, ntuple(nf, i->deserialize(s)))
    end
end
