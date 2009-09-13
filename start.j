type Array(a)
    dims: buffer(size)
    strides: buffer(size)
    data: buffer(a)
end

function print(x:any)
    # default print function, call builtin
    _print(x)
    return ()
end

function ref(t:tuple, i:int32)
    return tupleref(t, unbox(i))
end

function ref(b:buffer(a), i:int32)
    return box(typeof(b).parameters[0], bufferref(b, unbox(i)))
end

function set(b:buffer(a), i:int32, x)
    bufferset(b, unbox(i), x)
    return x
end

function `+`(x:int32, y:int32)
    return box(typename(int32), add_int32(unbox(x), unbox(y)))
end
