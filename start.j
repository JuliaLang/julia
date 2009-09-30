function ref(t:Type, params...)
    return instantiate_type(t, params)
end

function print(x:Any)
    # default print function, call builtin
    _print(x)
    return ()
end

function ref(t:Tuple, i:Int32)
    return tupleref(t, unbox(i))
end

function ref(b:Buffer, i:Int32)
    return box(typeof(b).parameters[1], bufferref(b, unbox(i)))
end

function set(b:Buffer, i:Int32, x)
    bufferset(b, unbox(i), x)
    return x
end

function length(t:Tuple)
    return box(Int32, tuplelen(t))
end

function length(b:Buffer)
    return b.length
end

function `+`(x:Int32, y:Int32)
    return box(Int32, add_int32(unbox(x), unbox(y)))
end

function `+`(x:Double, y:Double)
    return box(Double, add_double(unbox(x), unbox(y)))
end

function `-`(x:Int32, y:Int32)
    return box(Int32, sub_int32(unbox(x), unbox(y)))
end

function `-`(x:Int32)
    return box(Int32, neg_int32(unbox(x)))
end

function `*`(x:Int32, y:Int32)
    return box(Int32, mul_int32(unbox(x), unbox(y)))
end

function `/`(x:Int32, y:Int32)
    return box(Int32, div_int32(unbox(x), unbox(y)))
end

function `<=`(x:Int32, y:Int32)
    return lt_int32(unbox(x),unbox(y)) || eq_int32(unbox(x),unbox(y))
end

function `<`(x:Int32, y:Int32)
    return lt_int32(unbox(x),unbox(y))
end

function `==`(x:Int32, y:Int32)
    return eq_int32(unbox(x),unbox(y))
end
